//ZZRELINK  JOB (TSO),
//             'Install ZZRELINK',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//********************************************************************  00020000
//* INSTALL ZZRELINK (CBT FILE #361)                                    00030000
//********************************************************************  00040000
//ASMSUBRS PROC MEMBER=TEMPNAME                                         00050000
//ASM      EXEC PGM=IFOX00,PARM='OBJECT,NODECK',REGION=1000K            00060000
//SYSPRINT DD  SYSOUT=*                                                 00070000
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB                                 00080000
//         DD  DISP=SHR,DSN=SYS1.AMODGEN                                00090000
//SYSGO    DD  DSN=&&OBJECT(&MEMBER),                                   00100000
//             SPACE=(3040,(200,40,2)),                                 00110000
//             UNIT=SYSDA,DISP=(MOD,PASS),                              00120000
//             DCB=(BLKSIZE=3040,LRECL=80,RECFM=FB)                     00130000
//SYSUT1   DD  SPACE=(TRK,(30,30)),UNIT=SYSDA                           00140000
//SYSUT2   DD  SPACE=(TRK,(30,30)),UNIT=SYSDA                           00150000
//SYSUT3   DD  SPACE=(TRK,(30,30)),UNIT=SYSDA                           00160000
//SYSPUNCH DD  DUMMY                                                    00170000
//LKED     EXEC PGM=IEWL,                                               00180000
//             PARM=(XREF,LIST,LET)                                     00190000
//SYSPRINT DD  SYSOUT=*                                                 00200000
//SYSLIN   DD  DSN=&&OBJECT(&MEMBER),DISP=(OLD,PASS)                    00210000
//SYSUT1   DD  UNIT=(SYSDA,SEP=(SYSLIN,SYSLMOD)),SPACE=(1024,(200,20))  00220000
//SYSLMOD  DD  DSN=&&SUBRS(&MEMBER),DISP=(MOD,PASS),                    00230000
//             DCB=(RECFM=U,BLKSIZE=23200),                             00240000
//             SPACE=(CYL,(2,,2)),UNIT=SYSDA                            00250000
//         PEND                                                         00260000
//STEP1    EXEC ASMSUBRS,MEMBER=ZZMESAGE                                00270000
//ASM.SYSIN DD *                                                        00280000
         TITLE  'Z Z M E S A G E  -  ZZRELINK MESSAGE HANDLING ROUTINE' 00010000
ZZMESAGE CSECT                                                          00020000
GR0      EQU   0                                                        00030000
GR1      EQU   1                                                        00040000
GR2      EQU   2                                                        00050000
GR3      EQU   3                                                        00060000
GR4      EQU   4                                                        00070000
GR5      EQU   5                                                        00080000
GR6      EQU   6                                                        00090000
GR7      EQU   7                                                        00100000
GR8      EQU   8                                                        00110000
GR9      EQU   9                                                        00120000
GR10     EQU   10                 BASE REGISTER 1                       00130000
GR11     EQU   11                 BASE REGISTER 2                       00140000
GR12     EQU   12                                                       00150000
GR13     EQU   13                                                       00160000
GR14     EQU   14                                                       00170000
GR15     EQU   15                                                       00180000
TRIPLESP EQU   C'-'                                                     00190000
SKPSPACE EQU   C' '                                                     00200000
C0       EQU   C'0'               EQUATES                               00210000
C1       EQU   C'1'                 TO SET UP                           00220000
C2       EQU   C'2'                   RETURN CODE                       00230000
C9       EQU   C'9'                     IN EOJ MSG                      00240000
FF       EQU   X'FF'                                                    00250000
FIFTYSIX EQU   56                                                       00260000
BLNK     EQU   C' '               BLANK CHARACTER                       00270000
BUFL     EQU   120                LENGTH (MINUS 1) OF MSG BUFFER        00280000
CCLEN    EQU   80                 LENGTH OF CONTROL CARD                00290000
CHAR0    EQU   C'0'               FOR SETTING PAGE NBR ZONE BITS        00300000
COFF     EQU   20                 OFFSET IN BFR FOR CTL CD IMAGE        00310000
ZZRMNO   EQU   4                  OFFSET INTO 'ZZR1  I' FOR MSG NBR     00320000
LFT2     EQU   2                  FOR USE WITH 'SLL' INSTRUCTION        00330000
LINE1    EQU   1                  FOR ADDING 1 TO LINE COUNT            00340000
LINES3   EQU   3                  FOR ADDING 3 TO LINE COUNT            00350000
LNKBK    EQU   4                  FOR STD LINKAGE USE                   00360000
LNKTHS   EQU   8                  FOR STD LINKAGE USE                   00370000
MSGK     EQU   3                  CONSTANT LENGTH OF 3 BYTES WHICH BE-  00380000
*                                 GIN EACH MESSAGE 'DC'                 00390000
M2       EQU   2                  LENGTH OF EACH MSG LIST ENTRY         00400000
NAMLEN   EQU   8                  LENGTH OF NAME                        00410000
NBRLEN   EQU   7                  LENGTH OF NUMBER AND OF 'ZZR1  I' CON 00420000
DDNLTH   EQU   8                  LENGTH OF DDNAME                      00430000
NLEN     EQU   2                  LENGTH OF MSG NBR PUT INTO 'ZZR1  I'  00440000
OFF172I  EQU   12                 OFFSET INTO MSG ZZR172I FOR NAME      00450000
PGOFF    EQU   116                OFFSET INTO PAGE HEADER FOR PAGE NBR  00460000
PNLEN    EQU   4                  LENGTH OF PAGE NUMBER                 00470000
RCOFF    EQU   25                 OFFSET IN EOJ MSG (WTO) FOR RET CODE  00480000
RCOFFP   EQU   16                 OFFSET IN EOJ MSG (NOT WTO) FOR R.C.  00490000
SYN1     EQU   62                 LENGTH OF 1ST PART OF SYNADAF MSG     00500000
SYN2     EQU   58                 LENGTH OF 2ND PART OF SYNADAF MSG     00510000
TXTOFF   EQU   9                  OFFSET IN BFR FOR MSG TEXT            00520000
VALOFF   EQU   3                  OFFSET INTO VALIDATION ERR MSG        00530000
ZRO      EQU   0                  CONSTANT OF ZERO                      00540000
L1       EQU   1                  LENGTH OF 1                           00550000
L3       EQU   3                  LENGTH OF 3                           00560000
L8       EQU   8                  LENGTH OF 8                           00570000
P1       EQU   1                  TO BE ADDED TO SOMETHING              00580000
P4       EQU   4                  TO ADD 4 TO SOMETHING                 00590000
X0       EQU   0                  OFFSET OF ZERO                        00600000
X1       EQU   1                  OFFSET OF ONE                         00610000
LSYN1    EQU   SYN1+P1                                                  00620000
         EJECT                                                          00630000
         SAVE  (14,12),,*                                               00640000
         LR    GR10,GR15          ADDRESSIBILITY                        00650000
         USING ZZMESAGE,GR10                                            00660000
         USING ZZRELCOM,4                                               00670000
         LA    GR12,SAVE                                                00680000
         ST    GR12,LNKTHS(GR13)  STD LINKAGE                           00690000
         ST    GR13,LNKBK(GR12)   ADDR OF CALLER'S SAVE AREA            00700000
         LR    GR13,GR12                                                00710000
         LA    GR2,MSGLIST                                              00720000
         TM    IOEF2,SPRNOPN      I/O ERROR ON SYSPRINT                 00730000
         BZ    PRNTMSG            NO - PROCEED TO WRITE MSG             00740000
         TM    X0(GR2),IOERF      IS I/O ERR MSG IN BUFFER ALREADY      00750000
         BO    WTOS               ISSUE SYNADAF MSG AS WTO              00760000
         CLI   X1(GR2),BADPRINT   IS SYSPRINT BAD - WTO TO BE ISSUED    00770000
         BNE   TESTM53            NO - SEE IF 'END OF JOB' WTO TO       00780000
*                                 BE WRITTEN                            00790000
         L     GR9,ATXT           ADDR OF MESSAGE TEXT CSECT            00800000
         USING TXTCSECT,GR9                                             00810000
         MVC   ZZR172I+OFF172I(NAMLEN),NAMEFLD PUT NAME INTO WTO TEXT   00820000
*                                 MOVE NAME OF SYSPRINT DD WHICH        00830000
*                                 COULD NOT BE OPENED INTO              00840000
*                                 WTOLFBAD WTO                          00850000
         LA    GR1,ZZR172I        POINT TO WTO MESSAGE                  00860000
         WTO   MF=(E,(1))                                               00870000
TESTM53  CLI   X1(GR2),M53        IS 'END OF JOB' WTO TO BE ISSUED NOW  00880000
         BNE   RETURN             IF NOT, RETURN TO CALLER NOW          00890000
         LA    GR1,WTOLF5         ADDRESS OF LIST FORM                  00900000
*                                 'END OF JOB' WTO                      00910000
         CLI   RCBUF,C9           IS RETURN CODE 10 OR HIGHER           00920000
         BH    WTORC12            YES, MOVE IN 12                       00930000
         MVI   RCOFF-P1(GR1),C0   NO, MOVE IN 0X                        00940000
         MVC   RCOFF(L1,GR1),RCBUF  MOVE IN RETURN CODE                 00950000
         B     GOWTO              AND WRITE EOJ WTO MSG                 00960000
WTORC12  EQU   *                                                        00970000
         MVI   RCOFF-P1(GR1),C1   MOVE IN 10                            00980000
         MVI   RCOFF(GR1),C2      MOVE IN 2                             00990000
GOWTO    EQU   *                                                        01000000
         WTO   MF=(E,(1))         EXECUTE 'END OF JOB' WTO              01010000
         B     RETURN             NOW EXIT TO CALLER                    01020000
PRNTMSG  EQU   *                                                        01030000
         SR    GR3,GR3                                                  01040000
         IC    GR3,LINECT                                               01050000
         LTR   GR3,GR3            DOES LINE-COUNT NOW = 0               01060000
         BZ    PRNTHEAD           IF SO, GO PRINT PAGE HEADER           01070000
RDYWRT1  EQU   *                                                        01080000
         MVI   MSGBUF,SKPSPACE                                          01090000
RDYWRTHD TM    X0(GR2),IOERF      IS THERE A MSG IN THE BUFFER          01100000
         BO    WRTMSG             IF SO, GO WRITE IT                    01110000
RDYWRT2  EQU   *                                                        01120000
         LA    GR6,MSGBUF+P1                                            01130000
         TM    X0(GR2),CTLCD      IS A CONTROL STMT TO BE PRINTED       01140000
         BO    ADCC               IF ON, GO PRINT THE CONTROL STMT      01150000
         SR    GR11,GR11                                                01160000
         IC    GR11,X1(GR2)       GET MESSAGE CODE                      01170000
         SLL   GR11,LFT2          MULTIPLY MSG CODE NUMBER BY 4 TO      01180000
*                                 USE IT AS AN INDEX TO THE TABLE OF    01190000
*                                 POINTERS TO MESSAGE TEXT              01200000
         L     GR5,MSGTAB(GR11)   OBTAIN THE POINTER TO THE MESSAGE TXT 01210000
         TM    X0(GR2),RCODE      IS A RETURN CODE TO BE PUT INTO MSG   01220000
         BZ    ANYPARM            NO- SEE IF ANY PARAMETERS             01230000
         CLI   RCBUF,C9           IS RETURN CODE 10 OR HIGHER           01240000
         BH    WRTRC12            YES, MOVE IN 12                       01250000
         MVI   RCOFFP-P1(GR5),C0  NO, MOVE IN 0X                        01260000
         MVC   RCOFFP(L1,GR5),RCBUF MOVE IN RETURN CODE                 01270000
         B     ANYPARM            AND WRITE EOJ MSG                     01280000
WRTRC12  EQU   *                                                        01290000
         MVI   RCOFFP-P1(GR5),C1  MOVE IN 10                            01300000
         MVI   RCOFFP(GR5),C2     MOVE IN 2                             01310000
ANYPARM  EQU   *                                                        01320000
         TM    X0(GR2),PBIT       ARE THERE ANY PARAMETERS              01330000
         BZ    NOPARAM            NO - CONTINUE PROCESSING              01340000
         TM    PARAMS,NAME        IS THERE A NAME PARAMETER             01350000
         BZ    NBRPARM            NO - IS THERE A NBR PARAMETER         01360000
         SR    GR8,GR8                                                  01370000
         IC    GR8,NAMEDISP       DISPLACEMENT OF NAME FROM BEGINING    01380000
*                                 OF MSG IS IN GR8                      01390000
         AR    GR8,GR5            ADD DISPLACEMENT TO MESSAGE ADDRESS   01400000
         LA    GR8,MSGK(GR8)      POINT TO START OF MSG TEXT            01410000
         MVC   X0(NAMLEN,GR8),NAMEFLD  PUT NAME INTO MSG TEXT           01420000
NBRPARM  TM    PARAMS,NBR         IS THERE A NUMBER PARAMETER           01430000
         BZ    DDTEST1            NO - CONTINUE PROCESSING              01440000
         SR    GR8,GR8                                                  01450000
         IC    GR8,NODISP         DISPLACEMENT OF NAME FROM             01460000
*                                 BEGINNING OF MSG IS IN GR8            01470000
         AR    GR8,GR5            ADD DISPLACEMENT TO MESSAGE ADDRESS   01480000
         LA    GR8,MSGK(GR8)      POINT TO START OF MSG TEXT            01490000
         MVC   X0(NBRLEN,GR8),NOFLD    PUT NUMBER INTO MSG TEXT         01500000
DDTEST1  TM    PARAMS,DDNM        IS DDNAME PRESENT                     01510000
         BNO   NOPARAM            NO _ CONTINUE PROCESSING              01520000
         SR    GR8,GR8            ZERO R8                               01530000
         IC    GR8,DDNMDISP       DISPLACEMENT OF DDNAME                01540000
         AR    GR8,GR5            POINT AT DDNAME SLOT                  01550000
         LA    GR8,MSGK(GR8)      POINT AT TRUE SLOT                    01560000
         MVC   X0(DDNLTH,GR8),DDNMFLD INSERT DDNAME IN MESSAGE          01570000
NOPARAM  EQU   *                                                        01580000
         CLI   X1(GR2),GENERUAL   IS THIS A 'VALIDATION ERROR' MSG      01590000
         BNE   NOTVALID           IF NOT, NO SPECIAL SETUP REQUIRED     01600000
         MVC   VALOFF(NAMLEN,GR5),DDVALNM   PUT NAME BEING VALIDATED IN 01610000
*                                 GENERVAL MSG                          01620000
NOTVALID EQU   *                                                        01630000
         MVC   X0(NBRLEN,GR6),ZZRCON   SET UP 'ZZR1XXI' IN BUFFER       01640000
*                                 BUFFER                                01650000
         MVC   ZZRMNO(NLEN,GR6),X1(GR5) PUT MSG NBR INTO 'ZZR1  I'      01660000
         SR    GR7,GR7                                                  01670000
         IC    GR7,X0(GR5)        GET LENGTH OF MSG TEXT (IN BYTES)     01680000
         BCTR  GR7,GR0            SUBTR 1 FOR EX                        01690000
         EX    GR7,MOVEMSG        LENGTH OF MSG IS IN GR7               01700000
         B     WRTMSG             GO WRITE THE MSG                      01710000
ADCC     LA    GR5,CCIMAGE                                              01720000
         MVC   COFF(CCLEN,GR6),X0(GR5) PUT CONTROL CARD IMAGE           01730000
*                                 INTO THE BUFFER                       01740000
WRTMSG   EQU   *                                                        01750000
         LA    GR6,MSGBUF                                               01760000
WRTHEAD  EQU   *                                                        01770000
         LA    GR7,PRTDCB                                               01780000
         LA    GR8,WTOS           ADDRESS TO BE USED BY SYNAD ROUTINE   01790000
*                                 IF I/O ERROR OCCURRS WHILE 'PRINTING' 01800000
         PUT   (GR7),(GR6)        PRINT MESSAGE                         01810000
         TM    IOEF2,SPRNOPN      I/O ERROR ON SYSPRINT                 01820000
         BZ    NOPUTERR           NO - CONTINUE PROCESSING              01830000
WTOS     MVC   WTOLF1+P4(SYN1),MSGBUF+P1 PUT 1ST PART OF SYNADAF        01840000
*                                 MESSAGE INTO WTO BUFFER               01850000
         MVC   WTOLF2+P4(SYN2),MSGBUF+LSYN1 PUT SECOND PART OF          01860000
*                                 SYNADAF MESSAGE INTO WTO BUFFER       01870000
         LA    GR1,WTOLF1         ADDRESS OF LIST FORM WTO IN GR1       01880000
         WTO   MF=(E,(1))         EXECUTE WTO                           01890000
         LA    GR1,WTOLF2         ADDRESS OF LIST FORM WTO IN GR1       01900000
         WTO   MF=(E,(1))         EXECUTE WTO                           01910000
*                                 TWO WTO'S MUST BE ISSUED BECAUSE      01920000
*                                 IMPLEMENTATION OF MCS DOES NOT        01930000
*                                 ALLOW MORE THAN 72 BYTES PER LINE     01940000
*                                 ON THE OPERATORS CONSOLE              01950000
         B     TESTM53            SEE IF END OF JOB MSG TO BE WRITTEN   01960000
NOPUTERR EQU   *                                                        01970000
         TM    SWITCH,WRTHDMSG    WAS A HEADER JUST PRINTED             01980000
         BO    NISWOFF            YES - DONT CLEAR BUFFER               01990000
         MVI   MSGBUF,BLNK        SET MSG BUFFER TO BLANKS AFTER WRITE  02000000
         MVC   MSGBUF+P1(BUFL),MSGBUF                                   02010000
         B     NOTHEAD            SINCE MSG ISNT A HEADER, GO TO        02020000
*                                 GET THE NEXT MESSAGE                  02030000
NISWOFF  EQU   *                                                        02040000
         NI    SWITCH,FF-WRTHDMSG TURN OFF HEADER SWITCH                02050000
         MVI   MSGBUF,TRIPLESP    IF A HEADER WAS PRINTED,              02060000
*                                 SET CARRIAGE CONTROL CHARACTER TO     02070000
*                                 TRIPLE SPACE                          02080000
         LA    GR3,LINES3(GR3)    ADD 3 TO LINE COUNT                   02090000
         B     RDYWRTHD           BRANCH TO SET UP FOR PRINTING THE     02100000
*                                 MESSAGE WHICH FOLLOWS THE HEADER      02110000
WTOLF1   WTO   '                                                       X02120000
                      ',MF=L      FOR 1ST PART OF SYNADAF MSG           02130000
WTOLF2   WTO   '                                                       X02140000
                  ',MF=L          FOR 2ND PART OF SYNADAF MSG           02150000
WTOLF5   WTO   'ZZR147I END OF JOB - X WAS HIGHEST SEVERITY CODE',MF=L  02160000
         SPACE 1                                                        02170000
NOTHEAD  EQU   *                                                        02180000
         LA    GR3,LINE1(GR3)     ADD 1 TO LINE COUNT                   02190000
         STC   GR3,LINECT                                               02200000
         CLI   LINECT,FIFTYSIX    WAS THE 56'TH LINE JUST PRINTED       02210000
         BL    RETCAL             IF NOT, SEE IF NECESSARY TO RETURN    02220000
*                                 TO CALLER                             02230000
         MVI   LINECT,ZRO         RESET LINE COUNT                      02240000
RETCAL   TM    X0(GR2),LASTMSG    IS THIS THE LAST MSG TO HANDLE NOW    02250000
         BO    RETURN             IF SO, EXIT TO CALLER                 02260000
         LA    GR2,M2(GR2)        ELSE POINT TO NXT MSG LIST ENTRY      02270000
         CLI   LINECT,ZRO         DOES PAGE HEADER NEED TO BE PRINTED   02280000
         BNE   RDYWRT1            IF NOT, GO TO SET UP NEXT MESSAGE     02290000
         SR    GR3,GR3                                                  02300000
PRNTHEAD OI    SWITCH,WRTHDMSG                                          02310000
         LA    GR6,HEADBUF                                              02320000
         MVC   PGOFF(PNLEN,GR6),PGNO   PUT PAGE NBR INTO HEADER MSG     02330000
         PACK  PGCONVRT(L8),PGNO+P1(L3) NOW INCREMENT IT                02340000
         CVB   GR8,PGCONVRT       CONVERT PACKED PAGE NUMBER TO         02350000
*                                 BINARY                                02360000
         LA    GR8,X1(GR8)        ADD 1 TO PAGE NUMBER                  02370000
         CVD   GR8,PGCONVRT       CONVERT BACK TO DECIMAL               02380000
         UNPK  PGNO+P1(L3),PGCONVRT                                     02390000
         OI    PGNO+L3,CHAR0      CHANGE SIGN OF LOW ORDER BYTE TO ZONE 02400000
         B     WRTHEAD            GO WRITE THE HEADER                   02410000
RETURN   L     GR13,SAVE+LNKBK                                          02420000
         RETURN  (14,12)                                                02430000
PGCONVRT DC    D'0'               AREA FOR PAGE NBR CONVERSION          02440000
ATXT     DC    A(TXTCSECT)        ADDR OF MESSAGE TEXT CSECT            02450000
SAVE     DC    18F'0'             REGISTER SAVE AREA                    02460000
MOVEMSG  MVC   TXTOFF(X0,GR6),MSGK(GR5) EXECUTED TO PUT MSG INTO BUFFER 02470000
SWITCH   DC    X'00'              INTERNAL SWITCH                       02480000
WRTHDMSG EQU   X'80'         ON = A PAGE HEADER IS BEING WRITTEN        02490000
*              LOW ORDER 7 BITS NOT USED                                02500000
ZZRCON   DC    C'ZZR1  I'         MESSAGE NUMBER CONSTANT               02510000
* THE FOLLOWING 121 BYTES ARE THE PAGE HEADER TEXT                      02520000
HEADBUF  DC    C'1'            CARRIAGE CONTROL=EJECT                   02530000
         DC    40C' '                                                   02540000
         DC    C'ZZRELINK MESSAGES AND CONTROL STATEMENTS '             02550000
         DC    29C' '                                                   02560000
         DC    C'PAGE '                                                 02570000
         DC    5C' '                                                    02580000
         DS    0F                                                       02590000
PATCHLEN EQU   (*-ZZMESAGE)/20      LENGTH OF 5 PER CENT PATCH AREA     02600000
PATCH    DC    XL(PATCHLEN)'00'   5 PER CENT PATCH AREA          A41780 02610000
         DS    0F                                                       02620000
MSGTAB   EQU   *-4                MSGTAB IS A TABLE OF POINTERS TO      02630000
*                                 THE MESSAGES. IT MUST START AT THE    02640000
*                                 FIRST ENTRY-4 BECAUSE THERE IS NO     02650000
*                                 MESSAGE WITH THE NUMBER 0             02660000
* NOTE - EACH ADCON IS ACCESSED AS NEEDED BY USING THE MESSAGE CODE AND 02670000
*        MULTIPLYING IT BY 4.  IT IS THEREFORE IMPORTANT TO RETAIN THE  02680000
*        SAME SEQUENCE OF ADCONS AND MESSAGE CODES.                     02690000
*        DC    AL4(ZZR100I)       MESSAGE TEXT NOT IN THIS MODULE       02700000
*        DC    AL4(ZZR101I)       MESSAGE TEXT NOT IN THIS MODULE       02710000
*        DC    AL4(ZZR102I)       MESSAGE TEXT NOT IN THIS MODULE       02720000
*        DC    AL4(ZZR103I)       MESSAGE TEXT NOT IN THIS MODULE       02730000
         DC    AL4(ZZR104I)                                             02740000
         DC    AL4(ZZR105I)                                             02750000
         DC    AL4(ZZR106I)                                             02760000
         DC    AL4(ZZR107I)                                             02770000
         DC    AL4(ZZR108I)                                             02780000
         DC    AL4(ZZR109I)                                             02790000
         DC    AL4(ZZR110I)                                             02800000
         DC    AL4(ZZR111I)                                             02810000
         DC    AL4(ZZR112I)                                             02820000
         DC    AL4(ZZR113I)                                             02830000
         DC    AL4(ZZR114I)                                             02840000
         DC    AL4(ZZR115I)                                             02850000
         DC    AL4(ZZR116I)                                             02860000
         DC    AL4(ZZR117I)                                             02870000
         DC    AL4(ZZR118I)                                             02880000
         DC    AL4(ZZR119I)                                             02890000
         DC    AL4(ZZR120I)                                             02900000
         DC    AL4(ZZR121I)                                             02910000
         DC    AL4(ZZR122I)                                             02920000
         DC    AL4(ZZR123I)                                             02930000
         DC    AL4(ZZR124I)                                             02940000
         DC    AL4(ZZR125I)                                             02950000
         DC    AL4(ZZR126I)                                             02960000
         DC    AL4(ZZR127I)                                             02970000
         DC    AL4(ZZR128I)                                             02980000
         DC    AL4(ZZR129I)                                             02990000
         DC    AL4(ZZR130I)                                             03000000
         DC    AL4(ZZR131I)                                             03010000
         DC    AL4(ZZR132I)                                             03020000
         DC    AL4(ZZR133I)                                             03030000
         DC    AL4(ZZR134I)                                             03040000
         DC    AL4(ZZR135I)                                             03050000
         DC    AL4(ZZR136I)                                             03060000
         DC    AL4(ZZR137I)                                             03070000
         DC    AL4(ZZR138I)                                             03080000
*        DC    AL4(ZZR139I)       SYNADAF MSG - TEXT NOT IN THIS MODULE 03090000
         DC    AL4(ZZR140I)                                             03100000
         DC    AL4(ZZR141I)                                             03110000
         DC    AL4(ZZR142I)                                             03120000
         DC    AL4(ZZR143I)                                             03130000
         DC    AL4(ZZR144I)                                             03140000
         DC    AL4(ZZR145I)                                             03150000
         DC    AL4(ZZR146I)                                             03160000
         DC    AL4(ZZR147I)                                             03170000
         DC    AL4(ZZR148I)                                             03180000
         DC    AL4(ZZR149I)                                             03190000
         DC    AL4(ZZR150I)                                             03200000
         DC    AL4(ZZR151I)                                             03210000
         DC    AL4(ZZR152I)                                             03220000
         DC    AL4(ZZR153I)                                             03230000
         DC    AL4(ZZR154I)                                             03240000
         DC    AL4(ZZR155I)                                             03250000
         DC    AL4(ZZR156I)                                             03260000
         DC    AL4(ZZR157I)                                             03270000
         DC    AL4(ZZR158I)                                             03280000
         DC    AL4(ZZR159I)                                             03290000
         DC    AL4(ZZR160I)                                             03300000
         DC    AL4(ZZR161I)                                             03310000
         DC    AL4(ZZR162I)                                             03320000
         DC    AL4(ZZR163I)                                             03330000
         DC    AL4(ZZR164I)                                             03340000
         DC    AL4(ZZR165I)                                             03350000
         DC    AL4(ZZR166I)                                             03360000
         DC    AL4(ZZR167I)                                             03370000
         DC    AL4(ZZR168I)                                             03380000
         DC    AL4(ZZR169I)                                             03390000
         DC    AL4(ZZR170I)                                             03400000
*        DC    AL4(ZZR171I)       I/O ERROR MSG - TEXT IN ZZRIOE MODULE 03410000
         DC    AL4(ZZR172I)                                             03420000
*        DC    AL4(ZZR173I)       MESSAGE TEXT NOT IN THIS MODULE       03430000
*        DC    AL4(ZZR174I)       MESSAGE TEXT NOT IN THIS MODULE       03440000
*        DC    AL4(ZZR175I)       MESSAGE TEXT NOT IN THIS MODULE       03450000
*        DC    AL4(ZZR176I)       MESSAGE TEXT NOT IN THIS MODULE       03460000
         DC    AL4(ZZR177I)                                             03470000
         DC    AL4(ZZR178I)                                           $ 03480000
*        DC    AL4(ZZR188I)       MSG TEXT NOT IN THIS MODULE    A38720 03490000
         EJECT                                                          03500000
ZZRVMTXT CSECT                                                          03510000
TXTCSECT EQU   ZZRVMTXT                                                 03520000
*********************************************************************** 03530000
* MESSAGE TEXT FORMAT -                                                 03540000
* A 1 BYTE HEXADECIMAL CONSTANT , WHICH IS THE HEXADECIMAL            * 03550000
* EQUIVALENT OF THE NUMBER OF BYTES  IN THE MESSAGE TEXT , IS         * 03560000
* DEFINED                                                             * 03570000
* FOLLOWING THE 1 BYTE HEXADECIMAL CONSTANT IS THE MESSAGE TEXT.      * 03580000
* THE MESSAGE TEXT IS A VARIABLE LENGTH CHARACTER DEFINED FIELD       * 03590000
* OF WHICH THE FIRST TWO BYTES ARE THE VARIABLE TO BE USED AS THE     * 03600000
* MESSAGE NUMBER IN ZZR1  I CONSTANT FIELD                              03610000
*********************************************************************** 03620000
*                                                                     * 03630000
* NOTE - NOT ALL MESSAGES ISSUED BY THIS UTILITY PROGRAM HAVE THEIR     03640000
*        MESSAGE TEXT CONTAINED WITHIN THIS MODULE.  THOSE MESSAGES NOT 03650000
* CONTAINED HEREIN ARE NOTED IN THEIR APPROPRIATE LOCATIONS WITHIN THE  03660000
*        MESSAGES HERE.                                                 03670000
* MESSAGE NUMBERS CURRENTLY IN USE ARE -                                03680000
*ZZR100I THROUGH ZZR177I AND ZZR188I                             A38720 03690000
*                                                                     * 03700000
*********************************************************************** 03710000
* THE FOLLOWING 4 MESSAGES ARE CONTAINED WITHIN ZZRIOE MODULE -         03720000
*ZZR100I - I/O ERROR READING MEMBER (MBRNAME)                           03730000
*ZZR101I - I/O ERROR WRITING MEMBER DATA AT TTR = (TTR) /READBACK CHECK 03740000
*ZZR102I MEMBER (NAME) NOT COPIED DUE TO I/O ERROR                      03750000
*ZZR103I MEMBERS (MBRNAME) THROUGH END OF DATA SET ARE NOT ACCESSIBLE   03760000
*        DUE TO I/O ERROR                                               03770000
ZZR104I  DC    X'1A'              MSG LENGTH                            03780000
         DC    C'04INVALID COMMAND OR KEYWORD'                          03790000
ZZR105I  DC    X'11'              MSG LENGTH                            03800000
         DC    C'05PARAMETER INVALID'                                   03810000
ZZR106I  DC    X'13'              MSG LENGTH                            03820000
         DC    C'06UNEQUAL PARENTHESIS'                                 03830000
ZZR107I  DC    X'14'              MSG LENGTH                            03840000
         DC    C'07INVALID CONTINUATION'                                03850000
ZZR108I  DC    X'20'              MSG LENGTH                            03860000
         DC    C'08MEMBER WITHOUT SELECT OR EXCLUDE'                    03870000
ZZR109I  DC    X'37'              MSG LENGTH                            03880000
         DC    C'09NO MIXING OF SELECT AND EXCLUDE MODES IN SAME'       03890000
         DC    C' LINK STEP'                                            03900000
ZZR110I  DC    X'19'              MSG LENGTH                            03910000
         DC    C'10INVALID REPLACE SPECIFIED'                           03920000
ZZR111I  DC    X'0F'              MSG LENGTH                            03930000
         DC    C'11NULL PARAMETERS'                                     03940000
ZZR112I  DC    X'20'              MSG LENGTH                            03950000
         DC    C'12CANNOT RENAME/REPLACE ON EXCLUDE'                    03960000
ZZR113I  DC    X'1B'              MSG LENGTH                            03970000
         DC    C'13OUTDD OR INDD NOT SPECIFIED'                         03980000
ZZR114I  DC    X'1B'              MSG LENGTH                            03990000
         DC    C'14OUTDD/LIST NOT ON LINK CARD'                         04000000
ZZR115I  DC    X'14'              MSG LENGTH                            04010000
         DC    C'15END OF FILE ON SYSIN'                                04020000
ZZR116I  DC    X'3E'              MSG LENGTH                            04030000
         DC    C'16MIXING CONTROL STATEMENTS FROM OLD AND NEW '         04040000
         DC    C'VERSION OF ZZRELINK'                                   04050000
ZZR117I  DC    AL1(46)            MSG LENGTH                            04060000
         DC    C'17OVERLAYS NOT SUPPORTED. XXXXXXXX NOT RE-LINKED'      04070000
ZZR118I  DC    X'17'              MSG LENGTH                            04080000
         DC    C'18CONTROL STATEMENT ERROR'                             04090000
ZZR119I  DC    X'18'              MSG LENGTH                            04100000
         DC    C'19STATEMENT SEQUENCE ERROR'                            04110000
ZZR120I  DC    X'19'              MSG LENGTH                            04120000
         DC    C'20         VALIDATION ERROR'                           04130000
ZZR121I  DC    AL1(33)            MSG LENGTH                            04140000
         DC    C'21ERROR ATTEMPTING TO OPEN XXXXXXXX'                   04150000
ZZR122I  DC    X'1A'              MSG LENGTH                            04160000
         DC    C'22DSCB COULD NOT BE OBTAINED'                          04170000
ZZR123I  DC    X'18'              MSG LENGTH                            04180000
         DC    C'23DATA SET NOT PARTITIONED'                            04190000
ZZR124I  DC    X'0D'              MSG LENGTH                            04200000
         DC    C'24INVALID LRECL'                                       04210000
ZZR125I  DC    X'11'              MSG LENGTH                            04220000
         DC    C'25INVALID BLOCKSIZE'                                   04230000
ZZR126I  DC    X'29'              MSG LENGTH                            04240000
         DC    C'26         REFERENCES AN UNMOVABLE DATA SET'           04250000
ZZR127I  DC    X'12'              MSG LENGTH                            04260000
         DC    C'27RECFM INCOMPATIBLE'                                  04270000
ZZR128I  DC    AL1(47)            MSG LENGTH                            04280000
         DC    C'28UNABLE TO GAIN ACCESS TO DIRECTORY FOR XXXXXXXX'     04290000
ZZR129I  DC    AL1(64)            MSG LENGTH                            04300000
         DC    C'29XXXXXXXX DIRECTORY ERROR. ALIAS XXXXXXXX OCCURS '    04310000
         DC    C'MORE THAN ONCE.'                                       04320000
ZZR130I  DC    AL1(46)            MESSAGE LENGTH                        04330000
         DC    C'30IMPLEMENTATION RESTRICTION - ALIAS TABLE FULL.'      04340000
ZZR131I  DC    AL1(51)            MSG LENGTH                            04350000
         DC   C'31UNABLE TO DETERMINE EPA FOR XXXXXXXX. NOT RE-LINKED.' 04360000
ZZR132I  DC    AL1(40)            MSG LENGTH                            04370000
         DC    C'32I/O ERROR READING DIRECTORY FOR XXXXXXXX'            04380000
ZZR133I  DC    AL1(83)            MSG LENGTH                            04390000
         DC    C'33XXXXXXXX NOT RE-LINKED. EXISTS ON OUTPUT DATA SET '  04400000
         DC    C'BUT REPLACE OPTION NOT SPECIFIED.'                     04410000
ZZR134I  DC    X'26'              MSG LENGTH                            04420000
         DC    C'34CANNOT COMPRESS WITH SELECT OR EXCLUDE'              04430000
ZZR135I  DC    AL1(53)            MSG LENGTH                            04440000
         DC    C'35I/O ERROR ON XXXXXXXX WHILE READING CESD FOR '       04450000
         DC    C'XXXXXXXX'                                              04460000
ZZR136I  DC    AL1(77)            MSG LENGTH                            04470000
         DC    C'36LOAD MODULE XXXXXXXX MARKED ''NOT EDITABLE''. '      04480000
         DC    C'IT WAS COPIED TO OUTPUT DATA SET.'                     04490000
ZZR137I  DC    X'4E'                                                    04500000
         DC    C'37CANNOT SPECIFY DUPLICATE MEMBERNAMES FOR SELECT/EXCLX04510000
               UDE/RENAME - NAME=        '                              04520000
ZZR138I  DC    X'2A'              MSG LENGTH                            04530000
         DC    C'38CANNOT PROCESS ALL OLD/NEW-NAMES SPECIFIED'          04540000
*ZZR139I - SYNADAF MESSAGE TEXT, NOT CONTAINED WITHIN THIS MODULE       04550000
ZZR140I  DC    X'29'              MSG LENGTH                            04560000
         DC    C'40         REFERENCES A NULL INPUT DATA SET'           04570000
ZZR141I  DC    X'3E'              MSG LENGTH                            04580000
         DC    C'41CANNOT RE/DE-BLOCK WITH NOTE-LIST/USER TTRN IN '     04590000
         DC    C'MEMBER         '                                       04600000
ZZR142I  DC    X'1F'              MSG LENGTH                            04610000
         DC    C'42CANNOT CONTINUE TO BUILD CTLTAB'                     04620000
ZZR143I  DC    X'3E'              MSG LENGTH                            04630000
         DC    C'43ALL SELECTED MEMBERS LINKED - DID NOT USE ALL '      04640000
         DC    C'SPECIFIED INDD''S'                                     04650000
ZZR144I  DC    X'49'              MSG LENGTH                            04660000
         DC    C'44THERE ARE         UNUSED TRACKS IN OUTPUT DATA SET ' 04670000
         DC    C'REFERENCED BY         '                                04680000
ZZR145I  DC    X'27'              MSG LENGTH                            04690000
         DC    C'45CANNOT COMPRESS TRACK OVERFLOW DATA SET'             04700000
ZZR146I  DC    X'23'              MSG LENGTH                            04710000
         DC    C'46CANNOT COMPRESS WITH RE/DE-BLOCKING'                 04720000
ZZR147I  DC    X'28'              MSG LENGTH                            04730000
         DC    C'47END OF JOB - X WAS HIGHEST SEVERITY CODE'            04740000
ZZR148I  DC    X'4F'              MSG LENGTH                            04750000
         DC    C'48NO SPACE IN OUTPUT DIRECTORY FOR DIRECTORY ENTRIES'  04760000
         DC    C' FROM INPUT DATA SET         '                         04770000
ZZR149I  DC    X'3E'              MSG LENGTH                            04780000
         DC    C'49THERE ARE          UNUSED DIRECTORY BLOCKS IN '      04790000
         DC    C'OUTPUT DIRECTORY'                                      04800000
ZZR150I  DC    X'56'              MSG LENGTH                     A36049 04810000
         DC    C'50**WARNING** THE OUTPUT DS REF BY XXXXXXXX CONTAINS T*04820000
               OO MANY DIRECTORY BLOCKS PER TRACK'               A36049 04830000
ZZR151I  DC    X'20'              MSG LENGTH                     A36047 04840000
         DC    C'51JOB HAS TERMINATED WITH ERROR(S)'             A36047 04850000
ZZR152I  DC    X'38'              MSG LENGTH                            04860000
         DC    C'52         COMPRESSED - WAS ALREADY IN PLACE AND NOT ' 04870000
         DC    C'MOVED'                                                 04880000
ZZR153I  DC    X'37'              MSG LENGTH                            04890000
         DC    C'53ALL MEMBERS COMPRESSED - ALL WERE ORIGINALLY '       04900000
         DC    C'COMPRESSED'                                            04910000
ZZR154I  DC    X'25'              MSG LENGTH                            04920000
         DC    C'54         HAS BEEN SUCCESSFULLY COPIED'               04930000
ZZR155I  DC    X'38'              MSG LENGTH                            04940000
         DC    C'55         HAS BEEN SUCCESSFULLY COPIED AND IS A '     04950000
         DC    C'''NEWNAME'''                                           04960000
ZZR156I  DC    X'1C'              MSG LENGTH                            04970000
         DC    C'56NOT A DIRECT ACCESS DATA SET'                        04980000
ZZR157I  DC    X'1F'              MSG LENGTH                            04990000
         DC    C'57XXXXXXXX DD STATEMENT NOT FOUND'                     05000000
ZZR158I  DC    AL1(41)            MSG LENGTH                            05010000
         DC    C'58UNIT ALLOCATED TO XXXXXXXX NOT SUPPORTED.'           05020000
ZZR159I  DC    X'3C'              MSG LENGTH                            05030000
         DC    C'59NO MEMBERS LINKED FROM INPUT DATA SET '              05040000
         DC    C'REFERENCED BY         '                                05050000
ZZR160I  DC    X'16'              MSG LENGTH                            05060000
         DC    C'60CONCATENATED DATA SETS'                              05070000
ZZR161I  DC    X'2D'              MSG LENGTH                            05080000
         DC    C'61COMPRESS TO BE DONE USING INDD NAMED         '       05090000
ZZR162I  DC    X'4A'              MSG LENGTH                            05100000
         DC    C'62INPUT DATASET FROM INDD NAMED          NOT SAME AS'  05110000
         DC    C' OUTDD - CANNOT COMPRESS'                              05120000
ZZR163I  DC    X'2F'              MSG LENGTH                            05130000
         DC    C'63NO MEMBER NAMES FOR PARTIAL LINK, '                  05140000
         DC    C'WILL NOT LINK'                                         05150000
ZZR164I  DC    X'12'              MSG LENGTH                            05160000
         DC    C'64TOTAL LINK ASSUMED'                                  05170000
ZZR165I  DC    X'49'              MSG LENGTH                            05180000
         DC    C'65         ''FOUND'' BUT NOT LINKED, DUE TO I/O ERROR X05190000
               READING INPUT DIRECTORY'                                 05200000
ZZR166I  DC    X'34'              MSG LENGTH                            05210000
         DC    C'66NO MEMBERS LINKED TO DATA SET REFERENCED BY '        05220000
         DC    C'        '                                              05230000
ZZR167I  DC    X'47'              MSG LENGTH                            05240000
         DC    C'67FOLLOWING MEMBER(S) LINKED FROM INPUT DATA SET REFERX05250000
               ENCED BY          -'                                     05260000
ZZR168I  DC    X'5C'              MSG LENGTH                            05270000
         DC    C'68**WARNING** DUE TO ERROR, POSSIBLE LOSS OF '         05280000
         DC    C'ACCESS TO MEMBER DATA AND/OR INCOMPLETE DIRECTORY'     05290000
ZZR169I  DC    X'4A'              MSG LENGTH                            05300000
         DC    C'69**WARNING** DUE TO I/O ERROR ON SYSUT4, OUTPUT '     05310000
         DC    C'DIRECTORY MAY BE INCOMPLETE'                           05320000
ZZR170I  DC    X'5D'              MSG LENGTH                            05330000
         DC    C'70**WARNING** DUE TO SYSUT3 I/O ERROR, COMPRESS-IN-PLAX05340000
               CE NOT DONE AND LINK OPERATION TERMINATED'               05350000
* THE FOLLOWING MESSAGE IS CONTAINED WITHIN ZZRIOE MODULE -             05360000
*ZZR171I **WARNING** DIRECTORY MAY NOT REFLECT VALID LOCATION OF MEM-   05370000
*        BER DATA                                                       05380000
ZZR172I  WTO   'ZZR172I          COULD NOT BE OPENED',MF=L              05390000
*ZZR173I - MESSAGE TEXT FOR THIS MSG IS IN ZZRDV1 MODULE                05400000
* THE FOLLOWING 3 MESSAGES ARE CONTAINED WITHIN THE ZZRWSU MODULE -     05410000
*ZZR174I ** WARNING ** INPUT RECORD IS A SHORT LENGTH RECORD     A38720 05420000
*        -DDNAME=          -OUTPUT TTRN=                         A38720 05430000
*ZZR175I ** WARNING ** INPUT RECORD IS GREATER THAN OUTPUT       A38720 05440000
*        BLKSIZE  -DDNAME=          -OUTPUT TTRN=                A38720 05450000
*ZZR176I MEMBER        IN DATASET REFERENCED BY        HAS MORE  A38720 05460000
*        THAN ONE NOTELIST POINTER                               A38720 05470000
ZZR177I  DC    X'39'              MSG LENGTH                            05480000
         DC    C'77         WAS SELECTED BUT NOT FOUND IN ANY INPUT DATX05490000
               A SET'                                                   05500000
ZZR178I  DC    AL1(77)            MSG LENGTH                          $ 05510000
         DC    C'78LOAD MODULE XXXXXXXX MARKED ''PAGE ALIGNED''. '    $ 05520000
         DC    C'IT WAS COPIED TO OUTPUT DATA SET.'                   $ 05530000
* THE FOLLOWING MESSAGE IS CONTAINED WITHIN MODULE ZZRWSU -      A38720 05540000
*ZZR188I MEMBER        IN DATASET REFERENCED BY        HAS       A38720 05550000
*        RECORDS GREATER THAN BLKSIZE                            A38720 05560000
         DS    0D                                                       05570000
         EJECT                                                          05580000
ZZRELCOM   DSECT                                                        05590000
         TITLE 'Z Z R E L C O M  -  ZZRELINK COMMUNICATIONS AREA'       00010000
**********       C O M M U N I C A T I O N    A R E A        ********** 00020000
**********               Z Z R E L I N K                     ********** 00030000
*                                                                       00040000
* THE FOLLOWING DESCRIPTION IS FOR ZZRELINK.                            00050000
* IT IS BASICALLY THE SAME FOR THE IEBCOPY PROCEDURE EXCEPT THAT        00060000
* IT DOES NOT GO THROUGH ALL THE SORTING STEPS. IT ALSO DOES NOT BUILD  00070000
* MULTIPLE DD TABLES AND IT DOES NOT ALLOW FOR RENAMES.                 00080000
*                                                                       00090000
         SPACE 2                                                        00100000
         SPACE 2                                                        00110000
* ABBREVIATIONS USED FREQUENTLY -                                       00120000
*        ODE = OUTPUT DIRECTORY ENTRY                                   00130000
*        IDE = INPUT DIRECTORY ENTRY                                    00140000
         SPACE 2                                                        00150000
*                                                                       00160000
* SETAB = SELECT/EXCLUDE TABLE                                          00170000
*        THE SETAB CONSISTS OF 10-BYTE ENTRIES.  ENTRIES ARE MADE TO    00180000
*        THIS TABLE WHEN -                                              00190000
*              1) A SELECTIVE LINK HAS BEEN SPECIFIED.  EACH ENTRY IS   00200000
*              FOR THE NAME OF A MEMBER TO BE SELECTED.  IF A MEMBER IS 00210000
*              TO BE RENAMED, TWO 10-BYTE ENTRIES ARE MADE IN THE SETAB 00220000
*              2) AN EXCLUSIVE LINK HAS BEEN SPECIFIED.  EACH ENTRY IS  00230000
*              FOR THE NAME OF A MEMBER TO BE EXCLUDED.                 00240000
*        NOTE- IN THE CASE OF A SELECTIVE LINK (AND ONLY IN THIS CASE), 00250000
*              THE SETAB WILL ALSO BE USED AS THE CTLTAB.  IN ALL OTHER 00260000
*              CASES, A SEPARATE CTLTAB WILL BE CONSTRUCTED.            00270000
*        BYTE 0 OF AN ENTRY IS DESIGNATED AS SEFLAG1.  WITHIN THIS BYTE 00280000
*        THE FOLLOWING BITS HAVE MEANING -                              00290000
SEBIT1   EQU   X'80' ON=THIS IS A NEWNAME ENTRY                         00300000
SEBIT2   EQU   X'40' ON=THIS IS A RENAMED ENTRY                         00310000
SEBIT3   EQU   X'20' ON=REPLACE OPTION WAS SPECIFIED FOR THIS MEMBER    00320000
SEBIT4   EQU   X'10' ON=DONTLINK FLAG...DO NOT PROCESS THIS ENTRY       00330000
SEBIT5   EQU   X'08' ON= THIS MEMBER HAS BEEN ''FOUND'' ON INPUT D.S.   00340000
SEBIT6   EQU   X'04' ON= THIS IS LAST ENTRY IN SETAB/CTLTAB             00350000
*        LO ORDER 2 BITS NOT USED                                       00360000
*        NOTE THE DEFINITION OF SEBIT3.  IF, IN THE INDD TABLE, BYTE 0  00370000
*              BIT 2 IS ON, THIS MEANS THAT THE REPLACE OPTION WAS SPE- 00380000
*              CIFIED FOR ALL MEMBERS COPIED FROM THIS INDD.  IT IS     00390000
*              VALID FOR BOTH OF THESE BITS TO BE ON AT THE SAME TIME,  00400000
*              ALTHOUGH IF THE BIT IS ON IN THE INDD TABLE, IT IS UN-   00410000
*              NECESSARY FOR SEBIT3 TO ALSO BE ON IN THE SETAB.         00420000
         EJECT                                                          00430000
* FOLLOWING IS A DESCRIPTION OF THE CONTROL TABLE -CTLTAB-              00440000
* (REMEMBER THAT THIS TABLE IS PHYSICALLY THE SAME TABLE AS SETAB WHEN  00450000
* A SELECTIVE LINK OPERATION IS SPECIFIED, BUT IS PHYSICALLY INDEPEN-   00460000
* DENT AND DISTINCT FROM THE SETAB FOR AN EXCLUSIVE LINK OPERATION.     00470000
* THERE IS NO SETAB FOR A FULL LINK OPERATION, AND THE CTLTAB IN THIS   00480000
* CASE IS CONSTRUCTED SIMILARLY TO WHEN AN EXCLUSIVE LINK IS SPECIFIED. 00490000
* SINCE THE SETAB AND CTLTAB ARE ONE AND THE SAME FOR A SEL. LINK, THIS 00500000
*  DESCRIPTION WILL ASSUME THAT A SELECTIVE LINK IS BEING DONE, FOR THE 00510000
*  PURPOSE OF SETTING UP THE TABLE INITIALLY.)....                      00520000
         SPACE 2                                                        00530000
*        INITIAL TABLE, FOLLOWING CCSCAN PROCESSING OF ''SELECT'' -     00540000
*********************************************************************** 00550000
*SEFLAG1 *SEFLAG2 * NAME OF MEMBER TO BE SELECTED, OR, IF SPECIFIED,  * 00560000
*(DESCR. *(UNUSED)* NEWNAME.  IF NEWNAME WAS SPECIFIED, THE TABLE WILL* 00570000
* ABOVE) *        * CONTAIN 1 ENTRY FOR OLDNAME AND ANOTHER FOR NEW.  * 00580000
*********************************************************************** 00590000
* 1 BYTE * 1 BYTE *----------------------8 BYTES----------------------* 00600000
         SPACE 2                                                        00610000
* OLDNAME/NEWNAME PAIRS ARE NOW EXTRACTED FROM THE SETAB.  THEN THE     00620000
* CTLTAB IS SORTED ALPH. BY MBRNAME, AND A NEWNAME PTRTABLE SET UP.     00630000
* WHEN THIS HAS BEEN DONE, THE INPUT DATA SET'S DIRECTORY IS SEARCHED   00640000
* FOR MATCHING NAMES (THE NEWNAME ENTRIES IN THE TABLE ARE NOT USED FOR 00650000
* THIS COMPARISON).  WHEN A MATCHING MEMBERNAME IS FOUND, THE DIRECTORY 00660000
* ENTRY IS RETAINED IN CORE (IF SPACE PERMITS), OR IT IS SPILLED ONTO   00670000
* SYSUT3.  IN EITHER CASE, THE CORE ADDRESS OR THE TTR+INDICATOR ARE    00680000
* PUT INTO THE CTLTAB, OVERLAYING THE HIGH-ORDER 4 BYTES OF 'OLDNAME'.  00690000
* THE MEMBER-TTR IS EXTRACTED FROM THE DIRECTORY ENTRY, AND OVERLAYS    00700000
* THE LOW-ORDER 4 BYTES OF 'OLDNAME' IN THE CTLTAB.  SEBIT5 IS TURNED   00710000
* ON.  IF THIS IS AN ALIAS ENTRY, SEFLAG2 IS SET AS FOLLOWS -           00720000
ALIAS    EQU   X'80'         TO TEST FOR AND SET ALIAS DIRECTORY ENTRY  00730000
         SPACE 2                                                        00740000
*        CTLTAB ENTRY FOR A ''FOUND'' MEMBER -                          00750000
         SPACE 2                                                        00760000
*********************************************************************** 00770000
*SEFLAG1 *SEFLAG2 * INDIC. * ADDR OF THE IN-* ZEROES *   MEMBER TTR   * 00780000
*        *        *  BYTE  * PUT DIR. ENTRY *        *                * 00790000
*********************************************************************** 00800000
*-1 BYTE-*-1 BYTE-*-1 BYTE-*-----3 BYTES----*-1 BYTE-*-----3 BYTES----* 00810000
         SPACE 2                                                        00820000
* THE INDICATOR BYTE IS ZEROES IF THE DIRECTORY ENTRY IS IN CORE, OR IT 00830000
* IS HEX '01' IF THE DIRECTORY ENTRY WAS SPILLED TO SYSUT3.             00840000
* NOTE THAT CTLTAB ENTRIES FOR A ''NEWNAME'' ARE NOT OVERLAYED OR AL-   00850000
* TERED AT ANY TIME.  ONCE THE ENTIRE INPUT DIRECTORY HAS BEEN SCANNED, 00860000
* (OR AT LEAST ALL ENTRIES FOR MEMBERS TO BE COPIED FROM THIS INPUT     00870000
* DATA SET HAVE BEEN BUILT), THE OUTPUT DATA SET DIRECTORY IS READ.     00880000
* MEMBERNAMES OF MEMBERS CURRENTLY IN THE OUTPUT DATA SET ARE COMPARED  00890000
* AGAINST MEMBERNAMES OF MEMBERS REFERENCED IN THE CTLTAB FOR THE CUR-  00900000
* RENT INPUT DATA SET, UNLESS THE LATTER WERE RENAMED.  IF THE INPUT    00910000
* MEMBER IS RENAMED, THEN THE NEWNAME IS COMPARED AGAINST THE OUTPUT.   00920000
* IF DUPLICATE NAMES ARE ENCOUNTERED, AND IF THE REPLACE OPTION WAS NOT 00930000
* SPECIFIED ON EITHER THE INDD LEVEL OR THE MEMBERNAME LEVEL, THEN THE  00940000
* DONT-LINK BIT (SEBIT4) IS SET IN THE FLAG BYTE OF THE APPROPRIATE     00950000
* CTLTAB ENTRY, AND THE INDIC. BYTE (BYTE 3) OF THIS ENTRY IS SET TO    00960000
* HEX 'FF'.                                                             00970000
* THINK OF THE LOW-ORDER 8-BYTES OF EACH ''FOUND'' CTLTAB ENTRY AS BE-  00980000
* ING DIVIDED INTO A LEFT HALF (INDIC. + DIR. ENTRY ADDR.) AND A RIGHT  00990000
* HALF (ZEROES + MBR. TTR).  THE LEFT HALF NOW REPRESENTS DIRECTORY     01000000
* ENTRIES FOUND IN THE CURRENT INPUT DATA SET, AND IS IN ALPHAMERIC     01010000
* SEQUENCE.                                                             01020000
         EJECT                                                          01030000
* THE NEXT STEP IN CTLTAB PROCESSING CAUSES THE ''FOUND'' ENTRIES TO BE 01040000
* MANIPULATED, WITH THE RESULT BEING THAT THE LEFT HALF CONTAINS (IN-   01050000
* DIC + ADDR OF DIR. ENTRY) IN SEQUENCE BY MEMBER TTR, AND THE RIGHT    01060000
* HALF CONTAINS THIS SAME INFORMATION (INDIC. + ADDR OF DIR. ENTRY) IN  01070000
* ALPHAMERIC SEQUENCE, OVERLAYING THE ACTUAL MEMBER TTR.  ANOTHER RE-   01080000
* SULT OF THIS MANIPULATION OF THE CTLTAB IS THAT MAIN-MEMBER ENTRIES   01090000
* PRECEDE THE CORRESPONDING ALIAS ENTRIES.  NOTE THAT THE BITS SET IN   01100000
* SEFLAG1 ARE NOW ONLY USEFUL FOR THE ''RIGHT HALF'' OF THE CTLTAB,     01110000
* SINCE THEY ARE NOT MANIPULATED AND THUS REMAIN IN THE ORIGINAL (AL-   01120000
* PHABETIC) SEQUENCE.                                                   01130000
         SPACE 2                                                        01140000
*        CTLTAB ENTRY FOR A FOUND MEMBER FOLLOWING TTR SORT -           01150000
         SPACE 2                                                        01160000
*********************************************************************** 01170000
*SEFLAG1 *SEFLAG2 * INDIC. * ADDR OF IN. DE * INDIC. * ADDR OF IN. DE * 01180000
*        *        *  BYTE  *                *  BYTE  *                * 01190000
*        *        *IN SEQ BY MEMBER TTR     *IN SEQ ALPHAMERICALLY    * 01200000
*********************************************************************** 01210000
* 1 BYTE-*-1 BYTE-*-1 BYTE-*-----3 BYTES----*-1 BYTE-*-----3 BYTES----* 01220000
         SPACE 2                                                        01230000
* AT THIS POINT, THERE IS NO LOGICAL RELATIONSHIP BETWEEN THE LEFT AND  01240000
* RIGHT HALVES OF THE RELEVANT CTLTAB ENTRIES.  BOTH HALVES CONTAIN THE 01250000
* SAME INFORMATION, BUT IT IS IN TWO DIFFERENT SEQUENCES.  NOTE THAT,   01260000
* IF A MEMBER BEING LOOKED FOR IS NOT ''FOUND'', ITS CTLTAB ENTRY RE-   01270000
* MAINS UNALTERED - THE NAME IS STILL UN-OVERLAYED.                     01280000
* FROM THIS POINT ON, THOSE MEMBERS OF THE INPUT DATA SET WHOSE DIREC-  01290000
* TORY ENTRIES ARE REFERENCED IN THE CTLTAB WILL BE COPIED, PROVIDED    01300000
* THAT THEY ARE ''FOUND'' AND NOT FLAGGED AS ''DONT-LINK''.  THEN THE   01310000
* DIRECTORY ENTRIES WILL BE MERGED.  AS THE MERGE IS PERFORMED, WHEN AN 01320000
* INPUT DE IS MERGED, IF THIS IS A SELECTIVE LINK, THE DONT-LINK BIT    01330000
* (SEBIT4) IS TURNED ON, THUS ALLOWING FOR THESE ENTRIES IN THE CTLTAB  01340000
* TO BE IGNORED IN SUBSEQUENT PASSES THROUGH THE SAME TABLE FOR THE EN- 01350000
* SUING INPUT DATA SETS.                                                01360000
JSTCPD   EQU   X'10'              IF ON IN THE 2ND BYTE OF A CTLTAB EN- 01370000
*                                 TRY (SEFLAG2), THERE IS A NAME (CON-  01380000
*                                 TAINED IN THE 3RD THROUGH 10TH BYTES  01390000
*                                 OF THIS ENTRY) OF AN INPUT MEMBER     01400000
*                                 WHICH HAS BEEN SUCCESSFULLY COPIED.   01410000
*                                 THIS MEMBERNAME WILL BE PRINTED BY    01420000
*                                 THE TERMINATION MODULE (ZZRVTM) UNDER 01430000
*                                 APPROPRIATE CONDITIONS, AT WHICH TIME 01440000
*                                 THIS BIT WILL BE TURNED OFF.          01450000
*********************************************************************** 01460000
         TITLE 'Z Z R E L I N K  -  CONTROL, DDNAME, AND SELECT TABLES' 01470000
*XXXXXXXXXX    EVERY CONSTANT BETW. HERE AND NXT X'S MUST BE CONTIGUOUS 01480000
* ALL CONSTANTS BETWEEN X'S ARE SET TO ZERO FOR EVERY INPUT DATA SET.   01490000
* THE NAMES OF THE CONSTANTS WHICH ARE INCLUDED IN THIS CONTIG. AREA -  01500000
* CDCT, FCT, NNTCT, OLDTTR, FLG5                                        01510000
* ICPT,SVLSTO,SVFSTO,FLG2,FLG6,SWITCH1,UTTRFLAG,OBCT                    01520000
         SPACE 2                                                        01530000
*********************************************************************** 01540000
INBEGIN  DC    F'0'          ADDRESS OF START OF INDD TABLE             01550000
REPLACOP EQU   SEBIT3        IF ON IN HI-ORDER BYTE OF AN INDD-TABLE    01560000
*                            ENTRY, THE REPLACE OPTION WAS SPECIFIED    01570000
*                            FOR THIS INPUT DATA SET.                   01580000
SEBEGIN  DC    AL4(0)        ADDR OF BEGINNING OF SETAB                 01590000
SESTOP   DC    F'0'          IF SELECTIVE OR EXCLUSIVE LINK, THIS IS    01600000
*                            THE ADDRESS (+1) OF THE END OF THE SETAB.  01610000
*                            IF FULL LINK, ADDR (+1) OF END OF INDDTAB. 01620000
ADNNPTRT DS    AL4           ADDR OF NEWNAME POINTER TABLE              01630000
CTAD     DS    AL4           ADDR OF CONTROL TABLE - IF SELECTIVE LINK, 01640000
*                            THIS WILL BE SAME AS SEBEGIN.              01650000
ENCT     DC    HL2'0'        COUNT OF NBR OF ENTRIES IN CTLTAB (TOTAL)  01660000
*              IF THIS IS A SELECTIVE LINK, THIS COUNT WILL BE INI-     01670000
*              TIALIZED TO THE NUMBER OF ENTRIES IN SETAB.              01680000
COUNT    DC    HL2'0'        COUNT OF NBR OF ENTRIES IN SETAB IF SEL OR 01690000
*                            EXCL LINK, OR ZERO IF FULL LINK.           01700000
INDDCT   DC    AL2(0)        COUNT OF NO. INDD'S IN CURRENT STEP        01710000
NNCT1    DC    AL2(0)        NBR OF NEWNAMES SPECIFIED IN CURRENT SE-   01720000
*                            LECTIVE LINK STEP (IF NOT SEL CPY, = 0)    01730000
         DS    0D                                                       01740000
OUTNAME  DC    CL8' '        NAME OF CURRENT OUTPUT DD                  01750000
         SPACE 2                                                        01760000
CCIMAGE  DS    10D                     SCAN CONTROL CARD BUFFER         01770000
         SPACE 2                                                        01780000
WKA1     DC    10F'0'        VOLATILE WORK AREA FOR GENERAL USE         01790000
*                                                                       01800000
*****   PLEASE NOTE---    ZZRESCAN IS NOT THE ONLY MODULE WHICH USES    01810000
**                WKA1 FOR TEMPORARY WORK SPACE.                        01820000
*                 HOWEVER, THE FOLLOWING 4 EQUATES ARE USED ONLY BY     01830000
*                 ZZRESCAN -                                            01840000
*                                                                       01850000
SARG     EQU   WKA1          TEMPORARY WORK AREA USED BY ZZRESCAN       01860000
*                            TO HOLD SEARCH ARGUMENT 8 BYTES LONG       01870000
SAVEPAPR EQU   WKA1+8        TEMPORARY WORK AREA USED BY ZZRESCAN       01880000
*                            TO HOLD PARTIAL PARAMETERS IF CONTROL CARD 01890000
*                            IS CONTINUED, 8 BYTES LONG.                01900000
LEFTPCNT EQU   WKA1+16       TEMPORARY WORK AREA USED BY ZZRESCAN       01910000
*                            TO HOLD COUNT OF LEFT PARENTHESIS SCANNED. 01920000
RGHTPCNT EQU   WKA1+18       TEMPORARY WORK AREA USED BY ZZRESCAN       01930000
*                            TO HOLD COUNT OF RIGHT PARENTHESIS SCANNED 01940000
*                            BOTH LEFTPCNT AND RGHTPCNT 2 BYTES EACH    01950000
*                            AND MUST BE CONTIGUOUS STORAGE.            01960000
CSTOREG  DC    3F'0'         SAVE AREA USED BY ZZRESCAN                 01970000
         SPACE 2                                                        01980000
*        REGISTER SAVE AREAS                                            01990000
SV1      DC    18F'0'        REGISTER SAVE AREA FOR MAINFLOW            02000000
SV2      DC    18F'0'        REGISTER SAVE AREA FOR NON-RESIDENT RTNES  02010000
MCAMOD   DC    10F'0'             ZZRELCOM CHANGE AREA                  02020000
* MCAMOD IS A PATCH-AREA FOR USE IN MAINTENANCE OF THIS PROGRAM         02030000
         TITLE 'POINTERS FOR INTER-MODULE COMMUNICATION'                02040000
VZZRSCN  DC    V(ZZRESCAN)         ADDR OF CONTROL CARD SCAN ROUTINE    02050000
VZZRLEOF DC    V(ZZRLEOF) EP ADDRESS OF SYSIN EODAD EXIT IN ZZRESCAN    02060000
VZZRLMES DC    V(ZZMESAGE)         ADDRESS OF MESSAGE WRITER ROUTINE    02070000
VZZRTERM DC    V(ZZRTERM)         ADDRESS OF TERMINATION ROUTINE        02080000
         ORG   VZZRTERM                                                 02090000
AZZRTERM DS    F                  ADDRESS OF TERMINATION ROUTINE        02100000
         TITLE 'S Y S I N    D A T A    C O N T R O L    B L O C K'     02110000
*CARDCB  DCB   DDNAME=SYSIN,RECFM=FB,LRECL=80,EODAD=VZZRLEOF,           02120000
*              MACRF=(GM),DSORG=PS                                      02130000
CARDCB   DCB   DDNAME=SYSIN,RECFM=FB,LRECL=80,EODAD=VZZRLEOF,          X02140000
               MACRF=(GM),DSORG=PS                                      02150000
         TITLE 'S Y S P R I N T    D A T A    C O N T R O L    B L O C X02160000
               K'                                                       02170000
* PRTDCB DCB   DDNAME=SYSPRINT,DSORG=PS,MACRF=(PM),RECFM=FBA,           02180000
*              LRECL=121,BLKSIZE=121                                    02190000
PRTDCB   DCB   DDNAME=SYSPRINT,DSORG=PS,MACRF=(PM),RECFM=FBA,          X02200000
               LRECL=121,BLKSIZE=121                                    02210000
         TITLE 'SWITCH AND WORK AREA DEFINITIONS FOR ZZRESCAN'          02220000
* THE FOLLOWING SWITCHES (PARMSWCH, CCSWITCH, COMDCDSW, CPARAMSW, CCDE- 02230000
* LIM, CCDELIM2) ARE PRIMARILY BUT NOT SOLELY USED BY ZZRESCAN -        02240000
*                                                                       02250000
         SPACE 1                                                        02260000
PARMSWCH DC    XL1'0'    SCAN INTERNAL INDD AND MEMBER SWITCHES         02270000
SCANNAME EQU   X'80'         SCANNING NAME                              02280000
SET4REPL EQU   X'40'         MULTIPLE ( EXPECT REPLACE                  02290000
ONELEFT  EQU   X'20'         FIRST LEFT PARENTHESIS                     02300000
LINKNOW  EQU   X'10'         NOW SCANNING LINK CARD                     02310000
STOPSCAN EQU   X'08'         BLANK ENCOUNTERED                          02320000
FLUSHSW  EQU   X'04'                   FLUSH TO NEXT LINK RE/SET ALONE  02330000
HASNEWNM EQU   X'02'         HAVE A NEW NAME WITH MEMBER                02340000
COMDPART EQU   X'01'         PARTIAL COMMAND- CONTINUED ON NEXT CARD    02350000
*                                                                       02360000
         SPACE 1                                                        02370000
CCSWITCH DC    XL1'0'                  CONTROL CARD SWITCHES- EXTERNAL  02380000
CARDPRTD EQU   X'80'         ON = CONTROL STATEMENT HAS BEEN PRINTED    02390000
SYSINEOF EQU   X'40'         END OF FILE ON SYSIN                       02400000
UNECPARN EQU   X'20'         INDD/MEMBER NAMES IMBEDDED IN PARENTHESIS  02410000
ZZRLINKC EQU   X'10'         ZZRLINK CONTROL CARDS                      02420000
COMDNOW  EQU   X'08'         COMMAND WORD                               02430000
LASTPARM EQU   X'04'         LAST PARAMETER- BYPASS SWITCH              02440000
MULTSE   EQU   X'02' MULTIPLE SELECT/EXCLUDE STATEMENTS                 02450000
FIRSTSCN EQU   X'01' ON=ZZRESCAN HAS BEEN CALLED FOR THE FIRST TIME     02460000
*                                                                       02470000
         SPACE 2                                                        02480000
COMDCDSW DC    XL1'0'           SOME EXTERNAL SWITCHES                  02490000
LINKDONE EQU   X'80'         LINK COMMAND SCANNED ALL OK                02500000
SELECTSC EQU   X'40'         SELECT COMMAND SCANNED                     02510000
EXCLUDES EQU   X'20'         EXCLUDE COMMAND SCANNED                    02520000
NEWOUT   EQU   X'10'         OUTDD KEYWORD PRESENT                      02530000
NEWINDD  EQU   X'08'         INDD KEYWORD PRESENT                       02540000
LISTSW   EQU   X'04'         DO NOT LIST MEMBERS COPIED (LIST=NO)       02550000
COMPRESS EQU   X'02'                   COMPRESS LINK DATA SET           02560000
MEMBRCD1 EQU   X'01'         MEMBER STATEMENT                           02570000
*                                                                       02580000
         SPACE 2                                                        02590000
CPARAMSW DC    XL1'0'       INTERNAL SCAN SWITCHES                      02600000
DELIMEND EQU   X'80'         DELIMITER IN COLUMN 71                     02610000
CONTINY  EQU   X'40'         CONTINUATION                               02620000
PARMCOME EQU   X'20'         PARAMETER FOLLOWS                          02630000
PARTPARM EQU   X'10'         PARTIAL PARAMETER                          02640000
READ1    EQU   X'08'         READ ANOTHER CONTROL STATEMENT             02650000
COMDPARM EQU   X'04'         COMMAND FOLLOWED BY PARAMETER              02660000
COL72BLK EQU   X'02'         COLUMN 72 NOT BLANK                        02670000
PARMZERO EQU   X'01'         PARAMETER LENGTH ZERO                      02680000
*                                                                       02690000
         SPACE 2                                                 A48742 02700000
SCANSWCH DC    XL1'0'        INTERNAL SCAN SWITCHES              A48742 02710000
NOCMMEXP EQU   X'80'         DON'T SCAN COMMAND ON CONTIN CARDS  A48742 02720000
*              LOW ORDER 7 BITS NOT USED - RESERVED              A48742 02730000
*                                                                       02740000
         SPACE 2                                                        02750000
CCDELIM  DC    XL1'0'        INTERNAL SCAN SWITCHES                     02760000
EQUALSGN EQU   X'80'         EQUAL SIGN                                 02770000
COMMASGN EQU   X'40'         COMMA                                      02780000
LEFTPRSG EQU   X'20'         LEFT PARENTHESIS                           02790000
RIGHTPRS EQU   X'10'         RIGHT PARENTHESIS                          02800000
BLANKSGN EQU   X'08'         BLANK                                      02810000
LASTCOMA EQU   X'04'         LAST DELIMITER A COMMA- READ A CARD        02820000
BADBLOCK EQU   X'02' VALIDATE-ZZRDV0 SETS IF SYSIN/SYSPRINT BLOCKSIZE   02830000
*          IS BAD.                                                      02840000
*              LO ORDER BIT NOT USED                                    02850000
*                                                                       02860000
         SPACE 2                                                        02870000
CCDELIM2 DC    XL1'0'  USED TO SAVE SETTINGS OF CCDELIM ON CONTINUATION 02880000
*                                                                       02890000
         TITLE 'ERROR FLAG DEFINITIONS USED BY OPEN FAILURE ROUTINE'    02900000
IOEF2    DC    X'00'              FLAGS DESCRIBING NATURE/TYPE OF I/O   02910000
*                                 ERROR                                 02920000
ERF9     EQU   X'80'         ON = 'HARD' ERROR WRITING MERGED OUTPUT    02930000
*                                 DIRECTORY TO SYSUT4                   02940000
ERF10    EQU   X'40'         ON = ERROR READING FROM SYSUT4.  IF 'ERF4' 02950000
*                                 OFF, ERROR OCCURRED DURING MERGE      02960000
*                                 PHASE OF PROGRAM - IF 'ERF4' ON, SEE  02970000
*                                 DESCRIPTION OF 'ERF4'.                02980000
NOSYSIN  EQU   X'20'         ON = SYSIN COULD NOT BE OPENED OR WAS IN-  02990000
*                                 VALID, OR BECAME UNUSABLE DUE TO AN   03000000
*                                 I/O ERROR                             03010000
SPRNOPN  EQU   X'10'         SYSPRINT COULD NOT BE OPENED, WAS INVALID- 03020000
*                                 LY SPECIFIED, OR AN I/O ERROR OCCUR-  03030000
*                                 RED MAKING SYSPRINT UNAVAILABLE       03040000
*              LOW ORDER 4 BITS NOT USED - RESERVED                     03050000
*                                                                       03060000
         TITLE 'SWITCH AND WORK AREA DEFINITIONS FOR ZZMESAGE'          03070000
         DS    0H                                                       03080000
MSGLIST  DC    4H'0'              AREA FOR PARAMETRIC INPUT TO ZZMESAGE 03090000
MSG1     EQU   MSGLIST                                                  03100000
MSG2     EQU   MSG1+2                                                   03110000
MSG3     EQU   MSG2+2                                                   03120000
MSG4     EQU   MSG3+2                                                   03130000
* THE FOLLOWING BITS WILL BE SET ON BY THE CALLER OF ZZMESAGE (MESSAGE  03140000
* WRITING ROUTINE) IN THE HIGH ORDER BYTE OF EACH APPROPRIATE HALFWORD  03150000
* IN THE MSGLIST PARAMETER(S) BEING USED -                              03160000
LASTMSG  EQU   X'80'         ON = LAST PARAMETER IN MSGLIST             03170000
CTLCD    EQU   X'40'         ON = A CONTROL CARD IS TO BE PRINTED       03180000
IOERF    EQU   X'20'         ON = A MESSAGE IS IN THE MESSAGE BUFFER -  03190000
*                                 AND IS TO BE PRINTED.  NO MSG CODE IS 03200000
*                                 ASSOCIATED WITH THIS MESSAGE, AND IT  03210000
*                            USUALLY WILL BE A SYNADAF MESSAGE.         03220000
RCODE    EQU   X'10'         ON = PUT RETURN CODE INTO THIS MSG TEXT    03230000
PBIT     EQU   X'08'         ON = USE PARAM LIST WITH THIS MSG TEXT     03240000
*              LO ORDER 3 BITS NOT USED - RESERVED                      03250000
         SPACE 2                                                        03260000
MSGPARAM DS    9H                 THIS FIELD IS TO CONTAIN PARAMETERS   03270000
*                                 TO BE PLACED IN MESSAGES              03280000
         ORG   MSGPARAM                                                 03290000
NAMEDISP DC    X'00'              THE DISPLACEMENT OF A NAME            03300000
*                                 PARAMETER FROM THE BEGINNING OF       03310000
*                                 THE MESSAGE IT IS TO BE PLACED IN     03320000
*                                 THIS BYTE                             03330000
NODISP   DC    X'00'              THE DISPLACEMENT OF A NUMBER          03340000
*                                 PARAMETER FROM THE BEGINNING OF       03350000
*                                 THE MESSAGE IT IS TO BE PLACED IN     03360000
*                                 THIS BYTE                             03370000
DDNMDISP DC    X'00'              DISPLACEMENT OF DDNAME FROM MSG START 03380000
         DS    0D                                                       03390000
NAMEFLD  DC    CL8' '        AREA CONTAINING NAME TO BE PUT INTO MSG    03400000
DDNMFLD  DC    CL8' '        AREA CONTAINING DDNAME TO INSERT IN MSG    03410000
DDVALNM  EQU   NAMEFLD       USED BY VALIDATE TO SAVE DD NAME           03420000
PARAMS   DC    X'00'                                                    03430000
NAME     EQU   X'80'         ON = THERE IS A NAME PARAMETER             03440000
NBR      EQU   X'40'         ON = THERE IS A NUMBER PARAMETER           03450000
DDNM     EQU   X'20'         ON = THERE IS A DDNAME PARAMETER           03460000
NOFLD    DC    CL7'0'        AREA CONTAINING NUMBER TO BE PUT INTO MSG  03470000
         TITLE 'MESSAGE CODE DEFINITIONS USED BY ALL ZZRELINK MODULES'  03480000
*********************************************************************** 03490000
*        THE FOLLOWING MESSAGE CODES ARE USED BY ZZRELINK, ZZRESCAN,  * 03500000
*    AND ZZMESAGE TO IDENTIFY AND PRODUCE ALL ZZRELINK MESSAGES.      * 03510000
*********************************************************************** 03520000
INALCNTR EQU     01                INVALID COMMAND OR KEYWORD           03530000
INVALSPR EQU     02                INVALID PARAMETER                    03540000
ONEQPARN EQU     03                UNEQUAL PARENTHESIS                  03550000
INVALCON EQU     04                INVALID CONTINUATION                 03560000
MEMNOSE  EQU     05                MEMBER WITHOUT SELECT/EXCLUDE        03570000
MULTSSEE EQU     06                ONLY ONE SELECT/EXCLUDE PER INDD     03580000
INVALREP EQU     07                INVALID REPLACE SPECIFIED            03590000
NULLPARM EQU     08                NULL PARAMETERS                      03600000
NORREN   EQU     09                CANNOT RENAME/REPLACE ON EXCLUDE     03610000
NOINDD   EQU     10                OUTDD OR INDD NOT SPECIFIED          03620000
INVALIST EQU   11                  OUTDD/LIST NOT ON LINK STATEMENT     03630000
ENDMESS  EQU   12                  END OF CONTROL CARDS                 03640000
MODEERR  EQU   13                  MIXING ZZRLINKAND ZZRDSCPY MODE      03650000
NOOVLYS  EQU   14                  WARNING - OVERLAYS NOT SUPPORTED     03660000
SCANMSG  EQU   15                  CONTROL STATEMENT ERROR              03670000
SEQERROR EQU   16                  STATEMENT SEQUENCE ERROR             03680000
GENERUAL EQU   17                  GENERAL VALIDATION MESSAGE           03690000
OPENERRX EQU   18                  OPEN ERROR MESSAGE                   03700000
OBTAINER EQU   19                  OBTAIN ERROR                         03710000
NOTPDSER EQU   20                  OBTAIN NOT PDS                       03720000
INVALREC EQU   21                  INVALID LRECL                        03730000
INVALBLK EQU   22                  INVALID BLOCKSIZE                    03740000
UNMOVEDS EQU   23                  DATA SET UNMOVABLE                   03750000
RECFMINC EQU   24                  RECFM INCOMPATIBLE                   03760000
NODIR    EQU   25                  UNABLE TO OPEN DIRECTORY DCB         03770000
DIRERR01 EQU   26                  ALIAS NAME X OCCURS MORE THAN ONCE   03780000
ATABFULL EQU   27                  ALIAS TABLE FULL                     03790000
EPAERR01 EQU   28                  UNABLE TO LOCATE EPA OF MODULE X     03800000
DIRERR02 EQU   29                  I/O ERROR READING DIRECTORY FOR X    03810000
REPERR01 EQU   30                  MEMBER X NOT RELINKED - NO REP OPT   03820000
M39      EQU   31                  CANNOT COMPRESS WITH SELECT OR       03830000
*                                  EXLCUDE                              03840000
CESDIOER EQU   32                  I/O ERROR READING CESD RECORDS       03850000
NE       EQU   33                  LOAD MODULE IS 'NOT EDITABLE'        03860000
*                                  BUFFERS FOR COMPRESS                 03870000
M42      EQU   34 CANNOT SPECIFY DUPLICATE NAME FOR SEL/EXCL/RENAME     03880000
M43      EQU   35                  CANNOT PROCESS ALL OLD/NEW NAMES     03890000
*                                  SPECIFIED                            03900000
M45      EQU   36                  (DATA SET NAME) REFERENCES A NULL    03910000
*                                  INPUT DATA SET                       03920000
M46      EQU   37                  CANNOT RE/DE BLOCK WITH              03930000
*                                  NOTE-LIST/USER TTRN IN MEMBER        03940000
*                                  (MEMBER NAME)                        03950000
M47      EQU   38                  CANNOT CONTINUE TO BUILD CTLTAB      03960000
M48      EQU   39                 ALL SELECTED MEMBERS COPIED - DID NOT 03970000
*                                 USE ALL SPECIFIED INDD'S              03980000
M49      EQU   40                  (NUMBER) UNUSED TRKS IN OUTPUT DATA  03990000
*                                  SET REFERENCED BY (DDNAME)           04000000
M50      EQU   41                  CANNOT COMPRESS TRACK OVERFLOW DATA  04010000
*                                  SET                                  04020000
M51      EQU   42                  CANNOT COMPRESS WITH RE/DE BLOCKING  04030000
M53      EQU   43                  END OF JOB (0,4,8) WAS HIGHEST       04040000
*                                  SEVERITY CODE                        04050000
NORMOD   EQU   44                  NO SPACE IN OUTPUT DIRECTORY FOR     04060000
*                                  DIRECTORY ENTRIES FROM DATA SET      04070000
*                                  (DATA SET NAME)                      04080000
UNUSDDB  EQU   45                  THERE ARE (NUMBER) UNUSED DIRECTORY  04090000
*                                  BLOCKS IN THE OUTPUT DIRECTORY       04100000
TMDBTR   EQU   46     **WARNING** THE OUTPUT DS REF BY XXXXXXXX  A36049 04110000
*                     CONTAINS TOO MANY DIRECTORY BLOCKS PER     A36049 04120000
*                     TRACK                                      A36049 04130000
M58      EQU   47                  ERROR FORCES JOB TO TERMINATE        04140000
M59      EQU   48                  (MEMBER NAME) COMPRESSED- WAS        04150000
*                                  ALREADY IN PLACE                     04160000
M60      EQU   49                  ALL MEMBERS COMPRESSED-              04170000
*                                  ALL WERE ORIGINALLY COMPRESSED       04180000
MEMCOP   EQU   50                  (MEMBERNAME) HAS BEEN SUCCESSFULLY   04190000
*                                  COPIED                               04200000
RNMEMCOP EQU   51                  (MEMBER NAME) HAS BEEN RENAMED AND   04210000
*                                  SUCCESSFULLY COPIED                  04220000
NOTDA    EQU   52                 DATA SET NOT DIRECT ACCESS            04230000
NODDCARD EQU   53                 DD CARD NOT FOUND                     04240000
UNITER01 EQU   54                 UNIT TYPE DEFINED BY X NOT SUPPORTED  04250000
NOMBCPDM EQU   55                 NO MBRS COPIED FROM INPUT DATASET RE- 04260000
*                                 FERENCED BY (XXXXXXXX)                04270000
CONCATBD EQU   56                 CONCATENATED DATA SETS                04280000
IMPCOMPR EQU   57                 IMPLIED COMPRESS                      04290000
NOCMPOSS EQU   58                 CANNOT COMPRESS                       04300000
NOLINK   EQU   59                 NO MEMBERS FOR PARTIAL LINK,          04310000
*                                 WILL NOT LINK                         04320000
DOFULLCP  EQU  60                 TOTAL LINK ASSUMED                    04330000
MFBNC    EQU   61 MEMBER FOUND BUT NOT COPIED - I/O ERROR READING       04340000
*                 INPUT DIRECTORY                                       04350000
NONELINK EQU   62 NO MEMBERS COPIED                                     04360000
FOLLMCPD EQU   63 FOLLOWING MBRS COPIED FROM INPUT DS REF BY XXXXXXXX   04370000
PLAMPID  EQU   64 POSSIBLE LOSS OF ACCESS TO MEMBER AND/OR INCOMPLETE   04380000
*                 DIRECTORY                                             04390000
WODINC   EQU   65 SYSUT4 I/O ERROR - OUTPUT DIRECTORY MAY BE INCOMPLETE 04400000
WONTCOM  EQU   66 I/O ERROR ON SYSUT3 - COMPRESS IN PLACE NOT DONE      04410000
BADPRINT EQU   67 SYSPRINT COULD NOT BE OPENED                          04420000
SMNF    EQU   68                  (MBRNAME) WAS SELECTED BUT NOT FOUND  04430000
PA       EQU   69                  LOAD MODULE IS PAGE ALIGNED        $ 04440000
         SPACE 2                                                        04450000
RCBUF    DC    C'0'               COMPLETION-CODE AREA...CONTAINS CHAR- 04460000
*                                 ACTER REPRESENTATION OF HIGHEST       04470000
*                                 COMPLETION CODE SET BY UTILITY PGM    04480000
LINECT   DC   X'0'                COUNT OF NBR LINES WRITTEN ON ONE PG  04490000
PGLIMIT  EQU   56                 MAX NBR OF LINES TO BE PUT ON ONE PG  04500000
         SPACE 2                                                        04510000
MSGBUF   DC    121C' '            MESSAGE BUFFER                        04520000
PGNO     DC    C'0001'            STARTING PAGE NUMBER FOR MSG OUTPUT   04530000
*************              END OF COMMUNICATION AREA   **************** 04540000
***   KEEP CARD  'MCAEND' LAST IN COMMUNICATION AREA JUST BEFORE THE ** 04550000
****  EQUATE THAT DETERMINES THE COMMUNICATIONS AREA SIZE 'MCASIZE'  ** 04560000
         SPACE 1                                                        04570000
MCAEND   DS    0D        END OF COMMUNICATION AREA                      04580000
MCASIZE  EQU   MCAEND-ZZRELCOM    SIZE OF COMMUNICATIONS AREA FOR SNAPS 04590000
         END                                                            05610000
//STEP2    EXEC ASMSUBRS,MEMBER=ZZRESCAN                                00300000
//ASM.SYSIN DD *                                                        00310000
SCAN     TITLE 'Z Z R E S C A N  -  ZZRELINK CONTROL CARD SCAN'         00010000
ZZRESCAN CSECT                                                          00020000
         ENTRY ZZRLEOF                                                  00030000
         SPACE 1                                                        00040000
*                                                                     * 00050000
*TITLE- ZZRELINK CONTROL CARD SCAN AND ANALYSIS CSECT- ZZRESCAN       * 00060000
*                                                                     * 00070000
*                                                                     * 00080000
*FUNCTION/OPERATION-  THIS MODULE SCANS CONTROL CARDS AND GIVES       * 00090000
*       MESSAGES FOR SYNTAX OR SEQUENCE ERRORS. IT BUILDS AN INDD     * 00100000
*       TABLE OF DD NAMES OF INPUT DATA SETS WHICH CONTAIN MEMBERS    * 00110000
*       TO BE COPIED. IT BUILDS AN SE TABLE OF MEMBER NAMES TO BE     * 00120000
*       SELECTED OR EXCLUDED IN THE LINK. IT STORES THE NAME OF       * 00130000
*       THE OUTPUT DD STATEMENT IN BUFFER 'OUTNAME'.                  * 00140000
*                                                                     * 00150000
*ENTRY POINTS- ENTERED AT ZZRESCAN.                                   * 00160000
*        ENTERED AT ZZRSEF ON EOF ON SYSIN                            * 00170000
*                                                                     * 00180000
*INPUT- SYSIN WHICH CONSISTS OF CONTROL STATEMENTS.                   * 00190000
*                                                                     * 00200000
*OUTPUT- SYSPRINT WHICH CONTAINS THE DIAGNOSTIC MESSAGES AND THE      * 00210000
*        CONTENTS OF SYSIN                                            * 00220000
*                                                                     * 00230000
*EXITS-  ERROR- IF AN ERROR OCCURS, A CODE IS STORED, A MESSAGE IS    * 00240000
*        GIVEN TO THE USER EXPLAINING THE ERROR CONDITION AND A       * 00250000
*        RETURN TO THE CALLING PROGRAM IS GIVEN.                      * 00260000
*                                                                     * 00270000
*EXTERNAL ROUTINES- READ CARD (GET), PRINT CARD OR ERROR MESSAGE (PUT)* 00280000
*                                                                     * 00290000
*TABLES/WORK AREAS-                                                   * 00300000
*      -KEYTAB-    LIST OF VALID KEYWORDS                             * 00310000
*      -COMDTABL-  LIST OF VALID COMMAND WORDS                        * 00320000
*      -CCIMAGE-   BUFFER CONTAINING CONTROL CARD IMAGE               * 00330000
*      -INDD TABLE-POINTED TO BY INBEGIN                              * 00340000
*      -SE TABLE-  POINTED TO BY SEBEGIN                              * 00350000
*      -CTAD-      POINTER TO CONTROL TABLE BUILT WHEN SELECTIVE LINK * 00360000
*      -PARMSWCH-  INTERNAL SCAN SWITCHES                             * 00370000
*      -CCSWITCH-  EXTERNAL SCAN SWITCHES                             * 00380000
*      -COMDCDSW-  EXTERNAL SCAN SWITCHES                             * 00390000
*      -CPARAMSW-  INTERNAL SCAN SWITCHES                             * 00400000
*      -SV2-       REGISTER SAVE AREA                                 * 00410000
*                                                                     * 00420000
*ATTRIBUTES- SERIAL REUSABLE                                          * 00430000
*                                                                     * 00440000
         EJECT                                                          00450000
         SPACE 1                                                        00460000
*                                                                     * 00470000
*                    SCAN ROUTINE CONSTANTS                           * 00480000
*                                                                     * 00490000
ANAT     EQU   C'@'                    NATIONAL SYMBOL- OKAY IN MEMBER  00500000
APOUND   EQU   C'#'                    NATIONAL SYMBOL- OKAY IN MEMBER  00510000
ADOLLAR  EQU   C'$'                    NATIONAL SYMBOL- OKAY IN MEMBER  00520000
AC       EQU   C'L'                                                     00530000
ANA      EQU   X'C0'                   START OF VALID CHARACTER         00540000
EQUAL    EQU   C'='                    HEX 'EQUAL'                      00550000
COMMA    EQU   C','                    HEX 'COMMA'                      00560000
BLANKCOL EQU   C' '                    HEX 'BLANK'                      00570000
PARENLFT EQU   C'('                    LEFT PARENTHESIS                 00580000
PARENRGT EQU   C')'                    RIGHT PARENTHESIS                00590000
CRESET0  EQU   X'00'                   RESETS SWITCHES                  00600000
         SPACE 1                                                        00610000
*                                                                     * 00620000
*                      SYMBOLIC REGISTER FOR SCAN                     * 00630000
         SPACE 1                                                        00640000
LENGTH   EQU   9                       LENGTH OF PARAMETER REGISTER 9   00650000
SCANADR  EQU   1                       ADDRESS OF PARAMETER REGISTER 1  00660000
GR0      EQU   0                                                        00670000
GR1      EQU   1                                                        00680000
GR2      EQU   2                                                        00690000
GR3      EQU   3                                                        00700000
GR4      EQU   4                       COMMUNICATION AREA POINTER       00710000
GR5      EQU   5                                                        00720000
GR6      EQU   6                                                        00730000
GR7      EQU   7                                                        00740000
GR8      EQU   8                                                        00750000
GR9      EQU   9                                                        00760000
GR10     EQU   10                                                       00770000
GR11     EQU   11                                                       00780000
GR12     EQU   12                      REGISTER USED FOR BASE ADDRESS   00790000
GR13     EQU   13                                                       00800000
GR14     EQU   14                                                       00810000
GR15     EQU   15                                                       00820000
FF       EQU   X'FF'              ALL BITS ON                           00830000
LEN8     EQU   8                  LENGTH OF COMMAND OR KEYWORD          00840000
LEN12    EQU   12                 LENGTH OF ENTRY                       00850000
SAV4     EQU   4                  OLD SAVE AREA                         00860000
SAV8     EQU   8                  SAVE                                  00870000
TABLE0   EQU   0                  ZERO TABLE DISPLACEMENT               00880000
INDD1    EQU   1                  ENTRIES IN INDD TABLE                 00890000
LEN10    EQU   10                 LENGTH OF  10                         00900000
LEN2     EQU   2                  LENGTH OF  2                          00910000
LEN4     EQU   4                  LENGTH OF  4                          00920000
COL71    EQU   71                 COLUMN 72                             00930000
COL70    EQU   70                 COLUMN 71                             00940000
SCAN0    EQU   0                  COLUMN BEING SCANNED                  00950000
UP1      EQU   1                  COLUMN UPDATE                         00960000
PARCNT   EQU   X'01'              PARENTHESIS COUNT                     00970000
MS7      EQU   7                  MASK OF 7                             00980000
MS5      EQU   5                  MASK OF 5                             00990000
EX       EQU   C'E'               E FOR EXCLUDE                         01000000
IN       EQU   C'I'               I FOR INCLUDE                         01010000
COMPCD   EQU   C'4'               COMPLETION CODE                       01020000
COMPCDE  EQU   C'8'               COMPLETION CODE                A44144 01030000
MS8      EQU   8                  MASK OF 8                             01040000
LEN3     EQU   3                  LENGTH OF 3                           01050000
MS2      EQU   2                  MASK OF 2                             01060000
         EJECT                                                          01070000
         SAVE  (14,12),,*                                               01080000
*                                                                       01090000
*                    BASE ADDRESS REGISTER GR12-- COMMUNICATION AREA 4  01100000
*                                                                       01110000
         BALR  GR12,GR0           ESTABLISH ADDRESSABILITY              01120000
         USING NEXT,GR12                                                01130000
         USING ZZRELCOM,4                                               01140000
         SPACE 1                                                        01150000
NEXT     EQU   *                                                        01160000
         ST    GR13,SV2+SAV4      SAVE POINTER TO OLD SAVE AREA         01170000
         LA    GR15,SV2                NEW SAVE AREA THIS CSECT         01180000
         ST    GR15,SAV8(GR13)    POINTER TO NEW SAVE AREA IN OLD       01190000
         LR    GR13,GR15               POINTER TO THIS CSECT SAVE AREA  01200000
         SPACE 1                                                        01210000
         TM    CCSWITCH,SYSINEOF       END OF FILE ON LAST READ         01220000
         BO    TERMS              YES- TERMINATE JOB-- SYSIN DONE       01230000
         TM    PARMSWCH,FLUSHSW        FLUSHING                         01240000
         BO    IGNORE             YES- GET NEXT CARD                    01250000
         TM    CCSWITCH,ZZRLINKC       HAVE ZZRLINK CONTROL CARDS       01260000
         BO    SCANERRM           YES- MIXED CODES                      01270000
         NI    COMDCDSW,FF-SELECTSC-EXCLUDES-MEMBRCD1 RESET SWITCHES    01280000
         B     TOSCAN                  CONTINUE                         01290000
         SPACE 1                                                        01300000
IGNORE   EQU   *                                                        01310000
         TM    CPARAMSW,COL72BLK       IS THIS CARD CONTINUED    A48742 01320000
         BO    IGNRENXT                NO                        A48742 01330000
         NI    SCANSWCH,FF-NOCMMEXP    RESET BYPASS SCAN SWITCH  A48742 01340000
         B     IGNOREON                AND CONTINUE              A48742 01350000
IGNRENXT EQU   *                                                 A48742 01360000
         OI    SCANSWCH,NOCMMEXP       SET BYPASS SCAN SWITCH    A48742 01370000
IGNOREON EQU   *                                                 A48742 01380000
         MVI   COMDCDSW,CRESET0                                         01390000
         MVI   CCDELIM2,CRESET0                                         01400000
         TM    CCSWITCH,CARDPRTD  WAS THE CARD PRINTED           A48800 01410000
         BO    GOONSC             YES                            A48800 01420000
         BAL   GR9,PRNTCRD        NO, PRINT THE CARD             A48800 01430000
         SPACE 1                                                        01440000
GOONSC   EQU   *                                                        01450000
         MVI   CCSWITCH,CRESET0        CLEAR SWITCHES FIRST TIME THRU   01460000
GOONS    EQU   *                                                        01470000
         MVI   CPARAMSW,READ1          FORCE READ FIRST CONTROL CARD    01480000
         SPACE 1                                                        01490000
TOSCAN   EQU   *                                                        01500000
         BAL   GR14,RDCARD             GO TO CONTROL CARD SCAN          01510000
RETRTOSC EQU   *                                                        01520000
         L     LENGTH,CSTOREG          LENGTH OF PARAMETER              01530000
         L     SCANADR,CSTOREG+SAV4 START OF PARAMETER                  01540000
         LTR   LENGTH,LENGTH           IS SCAN LENGTH ZERO              01550000
         BZ    BADPARM                 BAD PARAMETER                    01560000
         TM    CCSWITCH,COMDNOW        IS COMMAND SWITCH ON             01570000
         BO    OPRLUP             YES- CHECK COMMAND WORDS              01580000
*                                                                     * 01590000
*   KEYWORD LOOKUP ROUTINE                                              01600000
         LA    GR5,KEYTAB              START OF KEY WORD TABLE          01610000
         LA    GR7,KEYEND              END OF KEY WORD TABLE            01620000
         B     SCANTBL                 SEARCH FOR KEY WORD              01630000
         SPACE 1                                                        01640000
*   COMMAND WORD LOOKUP TABLE                                           01650000
OPRLUP   LA    GR5,COMDTABL            START OF COMMAND TABLE           01660000
         LA    GR7,COMDEND             END OF COMMAND TABLE             01670000
SCANTBL  EQU   *                                                        01680000
         LA    GR6,LEN8           MAXIMUM LENGTH OF KEYWORD OR COMMAND  01690000
         CR    LENGTH,GR6              TEST IF LENGTH EXCEEDS 8         01700000
         BH    PRTBAD                  PRINT BAD CARD                   01710000
         LA    GR6,LEN12          INCREMENT VALUE- 12 BYTE ENTRIES      01720000
         MVC   SARG(LEN8),BLANKS8 MOVE BLANKS TO SEARCH ARGUMENT        01730000
         BCTR  LENGTH,GR0         DECREMENT FOR EXECUTE COUNT           01740000
         EX    LENGTH,OPRLUP3          MOVE PARAMETER TO 'SARG'         01750000
OPRCMP   CLC   SARG(LEN8),TABLE0(GR5) TEST IF SEARCH ARGUMENT IN TABLE  01760000
         BNE   OPRLUP2                 NO- CONTINUE SCAN OF TABLE       01770000
         LA    GR5,LEN8(GR5)      ENTRY ADDRESS OF ROUTINE              01780000
         BR    GR5                     ENTER PROPER ROUTINE             01790000
OPRLUP2  BXLE  GR5,GR6,OPRCMP          LOOP TO CONTINUE SCAN            01800000
PRTBAD   EQU   *                                                        01810000
         LA    GR2,INALCNTR            INVALID COMMAND OR KEYWORD       01820000
         B     GIVESCNE           PRT CARD, SET RC TO 4, WRT MSG        01830000
         EJECT                                                          01840000
*     COMMAND LOOKUP TABLE                                              01850000
* FORMAT OF TABLE -                                                     01860000
*     1) KEYWORD OR ABBREVIATION (ALWAYS AN 8-BYTE 'DC')                01870000
*     2) BRANCH TO ANALYZER SUBROUTINE                                  01880000
COMDTABL DS    0F                      START OF VALID COMMAND TABLE     01890000
         DC    C'LINK    '        KEYWORD                               01900000
         B     CHKLINK            BRANCH TO ANALYZER                    01910000
         DC    C'L       '        ABBREVIATION                          01920000
         B     CHKLINK            BRANCH TO ANALYZER                    01930000
* SUBSEQUENT TABLE ELEMENTS ARE IN SAME FORMAT AS THE ABOVE             01940000
         DC    C'EXCLUDE '                                              01950000
         B     CHKEXCLD           CHECK EXCLUDE                         01960000
         DC    C'E       '                                              01970000
         B     CHKEXCLD           CHECK EXCLUDE                         01980000
         DC    C'SELECT  '                                              01990000
         B     CHKSELCT           CHECK SELECT                          02000000
COMDEND  DC    C'S       '             END OF COMMAND WORD TABLE        02010000
         B     CHKSELCT           CHECK SELECT                          02020000
         EJECT                                                          02030000
*     KEYWORD LOOKUP TABLE                                              02040000
* FORMAT OF TABLE -                                                     02050000
*     1) KEYWORD OR ABBREVIATION (ALWAYS AN 8-BYTE 'DC')                02060000
*     2) BRANCH TO ANALYZER SUBROUTINE                                  02070000
KEYTAB   DS    0F                 START OF KEY WORD TABLE               02080000
         DC    C'MEMBER  '        KEYWORD                               02090000
         B     MEMBRANL           BRANCH TO ANALYZER                    02100000
         DC    C'M       '        ABBREVIATION                          02110000
         B     MEMBRANL           BRANCH TO ANALYZER                    02120000
* SUBSEQUENT TABLE ELEMENTS ARE IN SAME FORMAT AS THE ABOVE             02130000
         DC    C'OUTDD   '                                              02140000
         B     OUTDDANL           CHECK OUTDD                           02150000
         DC    C'O       '                                              02160000
         B     OUTDDANL           CHECK OUTDD                           02170000
         DC    C'INDD    '                                              02180000
         B     INDDANL            CHECK INDD                            02190000
KEYEND   DC    C'I       '                                              02200000
         B     INDDANL            CHECK INDD                            02210000
         EJECT                                                          02220000
         SPACE 1                                                        02230000
         SPACE 1                                                        02240000
OUTDDANL EQU   *                                                        02250000
         TM    PARMSWCH,FLUSHSW        DOING A FLUSH                    02260000
         BO    IGNORE             YES- SKIP THIS CARD                   02270000
         TM    PARMSWCH,LINKNOW        ON LINK STATEMENT                02280000
         BZ    INVALODL           ERROR- OUTDD NOT ON LINK CARD         02290000
         SPACE 1                                                        02300000
COMPRETN EQU   *                       RETURN POINT AFTER MESSAGE       02310000
         OI    COMDCDSW,NEWOUT         SET OUTDD SCANNED SWITCH         02320000
*                                        LINK OPERATION HAS BEEN FOUND  02330000
         TM    CPARAMSW,DELIMEND       NAME ON NEXT CARD (= IN COL 71)  02340000
         BZ    PICKNAME           NO- NORMAL PROCESSING                 02350000
         BAL   GR10,GETPARM            GET NEXT CARD- POSITION TO PARM  02360000
         SPACE 1                                                        02370000
PICKNAME EQU   *                                                        02380000
         BAL   GR14,RDCARD             GET PARAMETER                    02390000
         L     SCANADR,CSTOREG+SAV4 START OF PARAMETER                  02400000
         L     LENGTH,CSTOREG          SIZE OF PARAMETER                02410000
         LA    GR6,LEN8           MAXIMUM LENGTH OF NAME                02420000
         CR    GR6,LENGTH                                               02430000
         BL    BADPARM                 NAME GREATER THAN 8 BYTES        02440000
         MVC   OUTNAME(LEN8),BLANKS8                                    02450000
         MVC   OUTNAME(LENGTH),SCAN0(SCANADR)                           02460000
         TM    CCSWITCH,LASTPARM       LAST PARAMETER ON CARD           02470000
         BZ    TOSCAN             NO- CHECK FOR LIST OPTION             02480000
         B     CHKCOMSC                CHECK FOR CONTINUED COMMENTS     02490000
         SPACE 1                                                        02500000
CHKCOMSC EQU   *                                                        02510000
         TM    CPARAMSW,COL72BLK       CONTINUED CARD                   02520000
         BZ    LINKALLD           FINISH UP LINK CARD SCAN              02530000
         BAL   GR9,CHK4COMT            CHECK FOR CONTINUED COMMENTS     02540000
LINKALLD EQU   *                                                        02550000
         OI    COMDCDSW,LINKDONE       SET DONE WITH LINK CARD SWITCH   02560000
         MVI   PARMSWCH,CRESET0        RESET ALL, ESPECIALLY 'LINKNOW'  02570000
         B     GOONSC                  GET NEXT CARD                    02580000
         SPACE 1                                                        02590000
CHKEXCLD EQU   *                                                        02600000
         TM    PARMSWCH,FLUSHSW        FLUSHING                         02610000
         BO    IGNORE             YES- IGNORE THIS CARD                 02620000
         TM    COMDCDSW,SELECTSC       SELECT CARD AHEAD                02630000
         BO    MULTS              YES- ERROR TO HAVE BOTH               02640000
         LA    GR7,RETRNEV             RETURN ADDRESS                   02650000
         TM    COMDCDSW,EXCLUDES       MULTIPLE EXCLUDES                02660000
         BZ    TESTCSE            NO                                    02670000
         B     MULTSET                 SET MULTIPLE S/E SWITCH          02680000
         SPACE 1                                                        02690000
RETRNEV  EQU   *                                                        02700000
         OI    COMDCDSW,EXCLUDES       SET EXCLUDE SWITCH               02710000
         NI    COMDCDSW,FF-MEMBRCD1    INDIC MEMBER KEYWORD FOR  A38724 02720000
*           THIS EXCLUDE COMMAND NOT YET FOUND                   A38724 02730000
         B     TOSCAN                  CONTINUE                         02740000
         SPACE 1                                                        02750000
CHKSELCT EQU   *                                                        02760000
         LA    GR7,SELCTOK             RETURN FROM FOLLOWING ROUTINE    02770000
         TM    PARMSWCH,FLUSHSW        DOING A FLUSH                    02780000
         BO    IGNORE             YES- SKIP THIS CARD                   02790000
         TM    COMDCDSW,EXCLUDES       EXCLUDE CARD AHEAD OF SELECT     02800000
         BO    MULTS              YES- ERROR TO MIX THEM                02810000
         TM    COMDCDSW,SELECTSC       PREVIOUS SELECT                  02820000
         BZ    TESTCSE            ONLY ONE SELECT                       02830000
MULTSET  EQU   *                                                        02840000
         OI    CCSWITCH,MULTSE         MULTIPLE SELECT/EXCLUDE          02850000
TESTCSE  EQU   *                                                        02860000
         TM    PARMSWCH,COMDPART       PART OF COMMAND ON NEXT CARD     02870000
         BZ    NOGOON             NO- GO ON AND PRINT CARD              02880000
         MVI   PARMSWCH,CRESET0        RESET ENTIRE SWITCH              02890000
         B     AFTRPRT                 DO NOT PRINT CARD                02900000
         SPACE 1                                                        02910000
NOGOON   EQU   *                                                        02920000
         BAL   GR9,PRNTCRD             PRINT THE CARD                   02930000
AFTRPRT  EQU   *                                                        02940000
         TM    COMDCDSW,NEWINDD        HAVE AN INDD                     02950000
         BZ    NOINDDSP           NO- SEQUENCE ERROR                    02960000
         TM    CCSWITCH,LASTPARM       MORE PARAMETERS ON CARD          02970000
         BO    NULLP              NO- MUST HAVE MEMBER KEYWORD          02980000
         NI    CCSWITCH,FF-COMDNOW RESET COMMAND WORD SWITCH            02990000
         BR    GR7                     RETURN                           03000000
         SPACE 1                                                        03010000
SELCTOK  EQU   *                                                        03020000
         OI    COMDCDSW,SELECTSC       SET SELECT SWITCH                03030000
         NI    COMDCDSW,FF-MEMBRCD1    INDIC MEMBER KEYWORD FOR  A38724 03040000
*           THIS EXCLUDE COMMAND NOT YET FOUND                   A38724 03050000
         B     TOSCAN                  CONTINUE                         03060000
         SPACE 1                                                        03070000
INDDANL  EQU   *                                                        03080000
         TM    PARMSWCH,FLUSHSW        DOING A FLUSH                    03090000
         BO    IGNORE             YES- SKIP THIS CARD                   03100000
         TM    CCSWITCH,ZZRLINKC       ZZRLINK STATEMENTS               03110000
         BO    SCANERRM           YES- MIXED MODES                      03120000
         TM    PARMSWCH,LINKNOW        INDD ON LINK CARD                03130000
         BO    NEXTI              YES                                   03140000
         TM    COMDCDSW,SELECTSC+EXCLUDES  JUST GOT SELECT OR EXCLUDE   03150000
         BM    RESETIN            YES- DO A LINK                        03160000
         BAL   GR9,PRNTCRD             PRINT INDD CARD                  03170000
         TM    COMDCDSW,NEWINDD        JUST HAD AN INDD                 03180000
         BO    STRMEMBS           ADD THESE NAMES TO TABLE              03190000
         NI    CCSWITCH,FF-MULTSE RESET MULTIPLE S/E SWITCH             03200000
         B     GOGOGO                  GO BUILD THE INDD TABLE          03210000
         SPACE 1                                                        03220000
NEXTI    EQU   *                                                        03230000
         TM    COMDCDSW,NEWINDD        INDD ON LINK CARD ALREADY        03240000
         BO    SCANERR            YES- ONLY ONE INDD PER LINK CARD      03250000
GOGOGO   EQU   *                                                        03260000
         OI    COMDCDSW,NEWINDD        FLAG AS HAVE AN INDD             03270000
         NI    COMDCDSW,FF-SELECTSC-EXCLUDES-MEMBRCD1 RESET SWITCHES    03280000
         SPACE 1                                                        03290000
**********                                                   ********** 03300000
**********         BUILD TABLE OF INDD NAMES                 ********** 03310000
**********                                                   ********** 03320000
         SPACE 1                                                        03330000
         XC    COUNT(LEN2),COUNT  CLEAR COUNT                           03340000
         XC    ENCT(LEN2),ENCT    CLEAR SE TABLE ENTRY COUNT            03350000
         XC    NNCT1(LEN2),NNCT1  CLEAR NEW NAME COUNT                  03360000
         XC    INDDCT(LEN2),INDDCT CLEAR INDD COUNT                     03370000
         MVC   SESTOP(LEN4),INBEGIN NO SE OR INDD ENTRIES YET           03380000
STRMEMBS EQU   *                                                        03390000
         TM    CPARAMSW,DELIMEND       NAME ON NEXT CARD (= IN 71)      03400000
         BZ    CONTSC             NO- CONTINUE NORMAL PROCESSING        03410000
GETNEXTC EQU   *                                                        03420000
         LA    GR10,SCANSOME           RETURN FROM FOLLOWING ROUTINE    03430000
         SPACE 1                                                        03440000
*  ROUTINE 'GETPARM' READS NEXT CARD AND POSITIONS TO CONTINUATION      03450000
*   PARAMETER. GR10 SET ON ENTRY TO ROUTINE WITH RETURN ADDRESS.        03460000
         SPACE 1                                                        03470000
GETPARM  EQU   *                                                        03480000
         BAL   GR9,READCC              GET NEXT CARD                    03490000
         BAL   GR9,PRNTCRD             PRINT THE CARD                   03500000
         MVI   CPARAMSW,CRESET0        CLEAR PARAMETER SWITCH           03510000
         CLI   CCIMAGE+COL71,BLANKCOL COL 72 BLANK                      03520000
         BE    ARNDNXT                 YES- NOT CONTINUED               03530000
         OI    CPARAMSW,COL72BLK       SET COLUMN 72 NOT BLANK SWITCH   03540000
ARNDNXT  EQU   *                                                        03550000
         LA    GR6,CCIMAGE+COL70  END OF CARD PARAMETER COLUMN          03560000
         BAL   GR9,NAMESCAN            SCAN OVER NAME IF ANY            03570000
         SPACE 1                                                        03580000
LOOP2    EQU   *                                                        03590000
         CLI   SCAN0(GR3),BLANKCOL COLUMN BLANK                         03600000
         BNE   STRTPARM                NO- START OF PARAMETER           03610000
LOOPPARM EQU   *                                                        03620000
         LA    GR3,UP1(GR3)       UPDATE TO NEXT COLUMN                 03630000
         CR    GR3,GR6                                                  03640000
         BNH   LOOP2                   SCAN TO COLUMN 71                03650000
         B     GETPARM                 GET NEXT CARD                    03660000
         SPACE 1                                                        03670000
STRTPARM EQU   *                                                        03680000
         ST    GR3,CSTOREG+SAV8   STORE START OF PARAMETER              03690000
         NI    PARMSWCH,FF-COMDPART RESET PARAMETER CONTINUED SWITCH    03700000
         OI    CPARAMSW,PARMCOME       SET TO EXPECT PARAMETER          03710000
         BR    GR10                    RETURN                           03720000
         SPACE 1                                                        03730000
CONTSC   EQU   *                                                        03740000
         L     GR3,CSTOREG+SAV8   COLUMN TO START SCAN                  03750000
         NI    CCSWITCH,FF-UNECPARN RESET PARENTHESIS SWITCH            03760000
         XC    LEFTPCNT(LEN4),LEFTPCNT CLEAR PARENTHESIS COUNT BUFFER   03770000
         CLI   SCAN0(GR3),PARENLFT  START WITH LEFT PAREN               03780000
         BNE   SCANSOME                GET NEXT PARAMETER               03790000
         OI    CCSWITCH,UNECPARN       SET PARENTHESIS SWITCH           03800000
         LA    GR3,UP1(GR3)       UP TO ACTUAL NAME- SET 4 BCTR         03810000
         MVI   LEFTPCNT+UP1,PARCNT MAKE LEFT PAREN COUNT 1              03820000
SCANSOME EQU   *                                                        03830000
         NI    PARMSWCH,LINKNOW        RESET ALL BUT LINK NOW SWITCH    03840000
         SR    GR7,GR7                 CLEAR PARAMETER SIZE             03850000
         BCTR  GR3,GR0            BACK UP POINTER                       03860000
         SPACE 1                                                        03870000
*********************************************************************** 03880000
*** START OF GENERAL INDD OR MEMBER NAME SCAN ROUTINE AT INDDLOOP.  *** 03890000
*********************************************************************** 03900000
         SPACE 1                                                        03910000
INDDLOOP EQU   *                                                        03920000
         LA    GR3,UP1(GR3)       UP TO NEXT COLUMN                     03930000
         TM    CPARAMSW,DELIMEND       UP TO 71                         03940000
         BZ    NOGET              NO                                    03950000
         BAL   GR10,GETPARM            GET PARAMETER FROM NEXT CARD     03960000
NOGET    EQU   *                                                        03970000
         BAL   GR9,TESTLAST            CHECK IF UP TO COLUMN 71         03980000
         CLI   SCAN0(GR3),COMMA   COMMA                                 03990000
         BH    TRYNAME                 NO- HIGHER THAN COMMA            04000000
         BE    CHKCOMMA                YES- DELIMITER                   04010000
         CLI   SCAN0(GR3),PARENLFT     LEFT PARENTHESIS                 04020000
         BE    CHKPARNL                YES                              04030000
         CLI   SCAN0(GR3),PARENRGT     RIGHT PARENTHESIS                04040000
         BE    CHKPARNR                YES                              04050000
         CLI   SCAN0(GR3),ADOLLAR      IS IT A NATIONAL SYMBOL '$'      04060000
         BE    STRNAME                 YES- OKAY                        04070000
         CLI   SCAN0(GR3),BLANKCOL     BLANK                            04080000
         BNE   BADPARM                 BAD PARAMETER                    04090000
         TM    PARMSWCH,SCANNAME       BEEN SCANNING NAME               04100000
         BO    BLNKSTP                 YES- STOP NAME SCAN              04110000
         SPACE 1                                                        04120000
SET4READ EQU   *                                                        04130000
         CLC   LEFTPCNT(LEN2),RGHTPCNT EQUAL NUMBER LEFT AND RIGHT PARE 04140000
         BNE   MISSPARN                ERROR- MISSING PARENTHESIS       04150000
         TM    CPARAMSW,COL72BLK       CARD CONTINUED                   04160000
         BZ    GOONS                   NO GET NEXT CARD                 04170000
         BAL   GR9,CHK4COMT            CHECK FOR CONTINUED COMMENTS     04180000
         B     GOONS                   GET NEXT CARD                    04190000
         SPACE 1                                                        04200000
CHK4COMT EQU   *                                                        04210000
         ST    GR9,SARG                SAVE RETURN ADDRESS              04220000
         LA    GR6,CCIMAGE+COL71       COLUMN 72                        04230000
LOOPCOM  EQU   *                                                        04240000
         CLI   SCAN0(GR3),BLANKCOL     NEXT COLUMN BLANK                04250000
         BNE   ACOMMENT                NO- START OF COMMENT             04260000
         LA    GR3,UP1(GR3)                                             04270000
         CR    GR3,GR6                 END OF CARD NO COMMENT FOUND     04280000
         BNE   LOOPCOM                 CONTINUE COMMENT SCAN            04290000
         B     CONTINV                 INVALID CONTINUATION      A48799 04300000
         SPACE 1                                                        04310000
ACOMMENT EQU   *                                                        04320000
         BAL   GR9,READCC              READ NEXT CARD                   04330000
         BAL   GR9,PRNTCRD             PRINT THE COMMENT CARD           04340000
         CLI   CCIMAGE+COL71,BLANKCOL  COMMENT CONTINUED                04350000
         BNE   ACOMMENT                YES- GET NEXT COMMENT CARD       04360000
         NI    CPARAMSW,FF-COL72BLK    RESET CONTINUED SWITCH           04370000
         L     GR9,SARG                LOAD RETURN ADDRESS              04380000
         BR    GR9                     RETURN TO CALLER                 04390000
         SPACE 1                                                        04400000
CHKIFSTP EQU   *                                                        04410000
         LA    GR3,UP1(GR3)                                             04420000
         TM    PARMSWCH,COMDPART       UP TO COL 71 PARM CONTINUED      04430000
         BO    GETNEXTC                YES- GET NEXT CARD               04440000
         CLI   SCAN0(GR3),BLANKCOL     NEXT COLUMN BLANK                04450000
         BNE   SCANSOME                NO- SCAN SOME MORE               04460000
         B     CHKCONTY                CHECK IF CARD CONTINUED          04470000
         SPACE 1                                                        04480000
TRYNAME  EQU   *                                                        04490000
         CLI   SCAN0(GR3),ANA                                           04500000
         BNL   STRNAME                 OKAY- ALPHABETIC OR NUMERIC      04510000
         SPACE 1                                                        04520000
*  THREE NATIONAL SYMBOLS ALLOWED IN MEMBER NAMES- 'AT' SIGN, POUND     04530000
*    SIGN, AND DOLLAR SIGN. CHECK BEFORE FOR DOLLAR SIGN MUST CHECK     04540000
*    HERE FOR 'AT' AND POUND SYMBOL.                                    04550000
         SPACE 1                                                        04560000
         CLI   SCAN0(GR3),ANAT         IS IT AN 'AT' SIGN               04570000
         BE    STRNAME                 YES- OKAY                        04580000
         CLI   SCAN0(GR3),APOUND       IS IT A 'POUND' SIGN             04590000
         BE    STRNAME            YES - OKAY                            04600000
         CLI   SCAN0(GR3),EQUAL        COULD IT BE AN EQUAL SIGN        04610000
         BNE   BADPARM            IF ITS NOT, ITS INVALID               04620000
         TM    COMDCDSW,MEMBRCD1       EQUAL SIGN IN MEMBER NAME SCAN   04630000
         BO    BADPARM                 YES- NOT ALLOWED                 04640000
         TM    PARMSWCH,LINKNOW        EQUAL SIGN WITH INDD ON LINK     04650000
         BZ    BADPARM                 NO- NOT ALLOWED IF NOT ON LINK   04660000
         TM    PARMSWCH,COMDPART       DELIMITER IN COLUMN 71           04670000
         BZ    TSTDLM                  NOT IN 71                        04680000
         OI    CPARAMSW,DELIMEND       SET IN 71 SWITCH                 04690000
TSTDLM   EQU   *                                                        04700000
         TM    CPARAMSW,CONTINY        PARAMETER WAS CONTINUED          04710000
         BZ    SETRETOK                NO                               04720000
         NI    CCSWITCH,FF-COMDNOW     RESET COMMAND SWITCH             04730000
         BAL   GR9,KCREATE             RECREATE KEYWORD                 04740000
         OI    CPARAMSW,PARMCOME  SET BIT                               04750000
         SPACE 1                                                        04760000
*          NAME NOT SCANNED, IF AN EQUAL SIGN IS A KEYWORD, POINTERS    04770000
*   ALL SET,   GO TO KPASS AND WILL RETURN TO CHECK KEYWORD JUST AS IF  04780000
*   RDCARD ROUTINE HAD SCANNED THE KEYWORD.                             04790000
         SPACE 1                                                        04800000
SETRETOK EQU   *                                                        04810000
         LA    GR14,RETRTOSC           RETURN FROM INITIALIZED 'KPASS'  04820000
         ST    GR14,SARG               STORE RETURN SO RDCARD CAN FIND  04830000
         B     KPASS                   SET UP AS IF 'RDCARD' SCANNED    04840000
         SPACE 1                                                        04850000
STRNAME  EQU   *                                                        04860000
         LA    GR7,UP1(GR7)            UP COUNT OF CHARACTERS IN NAME   04870000
         TM    PARMSWCH,SCANNAME       ALREADY SCANNING NAME            04880000
         BO    ALLSCAN                 YES                              04890000
         OI    PARMSWCH,SCANNAME       SET NAME SCANNING SWITCH         04900000
         LR    GR1,GR3                 LOAD POINTER TO START OF NAME    04910000
ALLSCAN  EQU   *                                                        04920000
         TM    PARMSWCH,COMDPART       CONTINUED NAME                   04930000
         BZ    SAMECARD                NO- CARD COMPLETE                04940000
         OI    CPARAMSW,CONTINY        SET TO CONTINUE ON NEXT CARD     04950000
         LA    GR14,INDDLOOP           RETURN ADDRESS                   04960000
         ST    GR14,SARG               STORE RETURN ADDRESS FOR SCAN    04970000
         B     KPART                   SET UP FIRST PART OF PARAMETER   04980000
         SPACE 1                                                        04990000
SAMECARD EQU   *                                                        05000000
         TM    CPARAMSW,DELIMEND       CHARACTER IN 71, NOT CONTINUED   05010000
         BZ    INDDLOOP                NO- CONTINUE SCAN                05020000
         B     ENDNAME                 STORE NAME                       05030000
         SPACE 1                                                        05040000
CHKPARNL EQU   *                                                        05050000
*                                                                       05060000
*    NAMES ON MEMBER STATEMENT ONLY IN () IF RENAME OR REPLACE OPTION   05070000
*   WAS SPECIFIED.  HOWEVER, FOR COMPATIBILITY, ALSO ALLOW LIST OF      05080000
*   MBR NAMES WITHIN PARENS.                                            05090000
*                                                                       05100000
         TM    CCSWITCH,ZZRLINKC       ZZRLINK CONTROL CARDS            05110000
         BZ    TSTRGHT                 NO                               05120000
         NI    PARMSWCH,FF-ONELEFT     RESET LEFT PARENTHESIS SWITCH    05130000
         B     CONTCPPY           ADD 1 TO LEFT-PAREN COUNT             05140000
         SPACE 1                                                        05150000
TSTRGHT  EQU   *                                                        05160000
         TM    COMDCDSW,EXCLUDES       REPLACE/RENAME ON EXCLUDE        05170000
         BO    NORENAME           IF SO, ITS INVALID                    05180000
         TM    PARMSWCH,ONELEFT   WAS A LEFT PAREN ALREADY SCANNED      05190000
         BO    BADREPLC                YES- INVALID REPLACE SPECIFIED   05200000
         OI    PARMSWCH,SET4REPL+ONELEFT  SET SWITCHES FOR REPLACE      05210000
         SPACE 1                                                        05220000
CONTCPPY EQU   *                                                        05230000
         LH    GR6,LEFTPCNT                                             05240000
         LA    GR6,UP1(GR6)            UP COUNT BY 1                    05250000
         STH   GR6,LEFTPCNT            STORE NEW COUNT                  05260000
         TM    PARMSWCH,SCANNAME       LEFT PAREN WHILE SCAN OF NAME    05270000
         BO    BADREPLC                YES- INVALID REPLACE SPECIFIED   05280000
         B     INDDLOOP                CONTINUE SCAN                    05290000
         SPACE 1                                                        05300000
CHKDUPC  EQU   *                                                        05310000
         TM    CPARAMSW,DELIMEND       COMMA IN COLUMN 71               05320000
         BO    ENDNAME                 YES- END SCAN OF NAME            05330000
         CLI   UP1(GR3),COMMA          NEXT CHARACTER ALSO A COMMA      05340000
         BE    NULLP                   YES- NULL PARAMETER ERROR        05350000
         CLI   UP1(GR3),PARENRGT       IS COMMA FOLLOWED BY RIGHT PAREN 05360000
         BE    NULLP              YES - NULL PARAMETER                  05370000
         B     ENDNAME                 CONTINUE                         05380000
         SPACE 1                                                        05390000
BLNKSTP  EQU   *                                                        05400000
         MVI   CCDELIM,BLANKSGN        SET AS DELIMITER A BLANK         05410000
ENDNAME  EQU   *                                                        05420000
         TM    CPARAMSW,CONTINY        PARM CONTINUED                   05430000
         BZ    NOCONT                  NO                               05440000
         NI    CCSWITCH,FF-COMDNOW     RESET COMMAND SWITCH             05450000
         BAL   GR9,KCREATE             YES- RECREATE PARAMETER          05460000
NOCONT   EQU   *                                                        05470000
         LA    GR9,LEN8                                                 05480000
         CR    GR7,GR9                                                  05490000
         BH    BADPARM                 BAD PARAMETER                    05500000
         L     GR8,SESTOP              NEXT SLOT IN INDD TABLE          05510000
         MVC   TABLE0(LEN10,GR8),FLAGBYTS  CLEAR FLAGS, BLANK NAME      05520000
         BCTR  GR7,GR0                 DECREMENT COUNT FOR EXECUTE      05530000
         EX    GR7,MOVEDDNM            MOVE DD NAME TO TABLE            05540000
         NI    PARMSWCH,FF-SCANNAME    RESET SCANNING NAME SWITCH       05550000
         TM    COMDCDSW,MEMBRCD1       MEMBER STATEMENT                 05560000
         BO    MEMBRCD                 YES- HANDLE DATA SET NAMES       05570000
         TM    PARMSWCH,SET4REPL       THIS NAME A REPLACE              05580000
         BZ    REGNAME                 NO- CONTINUE                     05590000
         OI    TABLE0(GR8),REPLACOP    SET REPLACE OPTION BIT           05600000
         NI    PARMSWCH,FF-SET4REPL-HASNEWNM  RESET SWITCHES            05610000
         SPACE 1                                                        05620000
REGNAME  EQU   *                                                        05630000
         LH    GR6,INDDCT                                               05640000
         LA    GR6,UP1(GR6)            UP DD COUNT                      05650000
         STH   GR6,INDDCT              STORE NEW COUNT                  05660000
         LA    GR8,LEN10(GR8)          NEXT SLOT IN INDD BUFFER         05670000
         ST    GR8,SESTOP              END OF SE TABLE                  05680000
         SR    GR7,GR7                 CLEAR COUNT REGISTR              05690000
         TM    CCDELIM,BLANKSGN        END OF NAMES REACHED             05700000
         BZ    INDDLOOP                NO- SCAN FOR MORE NAMES          05710000
         CLI   SCAN0(GR3),COMMA        REALLY STOPPED AT A COMMA        05720000
         BE    CHKCONTY                YES                              05730000
ISDONE   EQU   *                                                        05740000
         NI    PARMSWCH,FF-LINKNOW     RESET LINK CARD SCAN SWITCH      05750000
         TM    COMDCDSW,NEWOUT         HAD AN OUTDD ON LINK CARD        05760000
         BZ    NOINDDSP                NO OUTDD SPECIFIED               05770000
         OI    COMDCDSW,LINKDONE+NEWOUT+NEWINDD BE SURE ALL ARE SET     05780000
         B     SET4READ                GET NEXT CARD                    05790000
         SPACE 1                                                        05800000
RESETIN  EQU   *                                                        05810000
         NI    COMDCDSW,FF-NEWINDD     RESET HAVE AN INDD SWITCH        05820000
         LA    GR3,CCIMAGE             START OF CARD                    05830000
         NI    CPARAMSW,COL72BLK       RESET ALL BUT CONTINUATION PUNCH 05840000
         ST    GR3,CSTOREG+SAV8        STORE COLUMN TO SCAN NEXT        05850000
         NI    CCSWITCH,CRESET0        CLEAR SWITCH                     05860000
         B     GETOUT                  RETURN TO MAINSTREAM             05870000
         SPACE 1                                                        05880000
TESTLAST EQU   *                                                        05890000
         LA    GR5,CCIMAGE+COL70       LAST COLUMN (71)                 05900000
         CR    GR3,GR5                 UP TO COLUMN 71                  05910000
         BCR   MS7,GR9                 NO- CONTINUE                     05920000
         OI    CPARAMSW,DELIMEND       CHARACTER IN COLUMN 71           05930000
         TM    CPARAMSW,COL72BLK       CONTINUATION PUNCH IN COLUMN 72  05940000
         BO    SETCONT                 SET FOR CONTINUED PARAMETER      05950000
         MVI   CCDELIM,BLANKSGN        SET TO STOP SCAN                 05960000
         BR    GR9                     RETURN                           05970000
SETCONT  EQU   *                                                        05980000
         OI    PARMSWCH,COMDPART       SET AS CONTINUED PARAMETER       05990000
         BR    GR9                     RETURN                           06000000
         SPACE 1                                                        06010000
CHKCOMMA EQU   *                                                        06020000
         TM    PARMSWCH,SCANNAME       SCANNING NAME                    06030000
         BZ    CHKIFSTP                NO- CHECK IF CAN STOP (N,R),NAME 06040000
         TM    CPARAMSW,DELIMEND       COMMA IN COLUMN 71               06050000
         BO    CHKCONT                 YES                       A45150 06060000
         CLI   UP1(GR3),BLANKCOL       NEXT COLUMN BLANK         A45150 06070000
         BNE   NOCMBLNK                NO                        A45150 06080000
CHKCONT  EQU   *                                                 A45150 06090000
         TM    PARMSWCH,SET4REPL+HASNEWNM REPLACE/RENAME SPECIFIED      06100000
         BC    MS5,CONTINV             YES ILLEGAL TO CONTINUE          06110000
         TM    CPARAMSW,COL72BLK       PUNCH IN COLUMN 72               06120000
         BZ    CONTINV                 NO- CONTINUATION ERROR           06130000
         CLI   UP1(GR3),BLANKCOL       NEXT COLUMN BLANK                06140000
         BE    BLNKSTP                 YES                              06150000
NOCMBLNK EQU   *                                                 A45150 06160000
         TM    PARMSWCH,SET4REPL       REPLACE EXPECTED                 06170000
         BZ    CHKDUPC            NO - CHECK NEXT COLUMN FIRST          06180000
         NI    PARMSWCH,FF-ONELEFT     RESET LEFT PARENTHESIS SWITCH    06190000
         TM    COMDCDSW,MEMBRCD1       DOING MEMBER SCAN                06200000
         BO    REPMEMB                 YES CHECK RENAME REPLACE         06210000
         SPACE 1                                                        06220000
ON2TST   EQU   *                                                        06230000
         TM    CPARAMSW,DELIMEND                                        06240000
         BO    CONTINV            INVALID TO CONTINUE REPLACE    A48799 06250000
         LA    GR3,UP1(GR3)       UP SCAN POINTER                A48799 06260000
         BAL   GR9,TESTLAST       CHECK IF UP TO COLUMN 71       A48799 06270000
         TM    CPARAMSW,DELIMEND  UP TO COLUMN 71                A48799 06280000
         BO    CONTINV            INVALID TO CONTINUE REPLACE    A48799 06290000
         CLC   SCAN0(LEN2,GR3),RPARN  DOES 'R)' FOLLOW           A48799 06300000
         BNE   BADREPLC           IF NOT, ITS INVALID                   06310000
         LA    GR3,UP1(GR3)       UP SCAN POINTER                A48799 06320000
         BAL   GR9,TESTLAST            CHECK IF UP TO COLUMN 71  A45185 06330000
         SPACE 1                                                        06340000
CHKPARNR EQU   *                                                        06350000
         TM    PARMSWCH,ONELEFT        WAS SWITCH RESET                 06360000
         BO    BADREPLC           IF NOT, INVALID CTL CARD              06370000
         LH    GR6,RGHTPCNT                                             06380000
         LA    GR6,UP1(GR6)            UP COUNT RIGHT PARENS            06390000
         STH   GR6,RGHTPCNT            STORE NEW COUNT                  06400000
         CLC   LEFTPCNT(LEN2),RGHTPCNT COMPARE PARENTHESIS COUNT        06410000
         BNE   CHKIFUNC                CHECK IF STARTED WITH LEFT PAREN 06420000
         TM    CCSWITCH,UNECPARN       STARTED WITH PARENTHESIS         06430000
         BZ    ALLOK                   NO                               06440000
         CLI   UP1(GR3),BLANKCOL       NEXT COLUMN BLANK                06450000
         BNE   MORETEST           KEEP ON                               06460000
         TM    COMDCDSW,MEMBRCD1  MEMBER NAME                           06470000
         BO    ALLOK                   YES,STORE NAME                   06480000
         TM    PARMSWCH,SCANNAME  WAS A NAME BEING SCANNED              06490000
         BO    BLNKSTP                 IF SO, GO TO STORE THE NAME      06500000
         B     ISDONE             DD NAME STORED                        06510000
MORETEST EQU   *                                                        06520000
         CLI   UP1(GR3),PARENLFT       NEXT CHARACTER LEFT PAREN        06530000
         BE    MISSPARN                UNEQUAL PARENTHESIS              06540000
         CLI   UP1(GR3),PARENRGT       NEXT CHARACTER RIGHT PAREN       06550000
         BE    MISSPARN                UNEQUAL PARENTHESIS              06560000
         TM    PARMSWCH,LINKNOW        LINK CARD                 A41802 06570000
         BO    ALLOK                   YES                       A41802 06580000
         B     BADREPLC                INVALID CTL CARD          A41802 06590000
ALLOK    EQU   *                                                        06600000
         TM    PARMSWCH,SCANNAME       SCANNING NAME                    06610000
         BO    NAMEFND                 YES- NAME COMPLETE        A45174 06620000
         TM    CPARAMSW,DELIMEND       UP TO 71                  A45174 06630000
         BO    GOONS                   YES- GET NEXT CARD        A45174 06640000
         B     INDDLOOP                NO- CONTINUE SCAN         A45174 06650000
NAMEFND  EQU   *                                                 A45174 06660000
         TM    CPARAMSW,DELIMEND       UP TO 71                         06670000
         BO    BLNKSTP                 YES- SET AS IF BLANK STOPPED     06680000
         CLI   UP1(GR3),BLANKCOL       IS NEXT COLUMN BLANK             06690000
         BNE   ENDNAME                 NO- END SCAN OF THIS NAME        06700000
         B     BLNKSTP                 SET STOPPED ON BLANK SWITCH      06710000
         SPACE 1                                                        06720000
CHKIFUNC EQU   *                                                        06730000
         TM    CCSWITCH,UNECPARN       STARTED WITH PARENTHESIS         06740000
         BZ    MISSPARN                NO- UNEQUAL PAREN COUNT          06750000
         B     ENDNAME                 STORE NAME                       06760000
         SPACE 1                                                        06770000
MEMBRANL EQU   *                                                        06780000
         TM    COMDCDSW,SELECTSC+EXCLUDES   SELECT OR EXCLUDE           06790000
         BZ    BADMEMBS           IF NEITHER, CANT SAY 'MEMBER'         06800000
         TM    CCSWITCH,ZZRLINKC  ZZRLINK CONTROL STATEMENTS            06810000
         BO    SCANERRM           YES MIXED CODES                       06820000
         TM    COMDCDSW,MEMBRCD1  WAS MEMBER KEYWRD FOUND ALRDY  A38724 06830000
         BO    BADMEMBS           YES-CANT SAY 'MEMBER'          A38724 06840000
         OI    COMDCDSW,MEMBRCD1       SET MEMBER KEYWORD SWITCH        06850000
         TM    CCSWITCH,MULTSE         CONSECUTIVE S/E                  06860000
         BO    STRMEMBS                ADD NAMES TO TABLE               06870000
         MVC   SEBEGIN(LEN4),SESTOP    START OF SE TABLE                06880000
         B     STRMEMBS                START INTERNAL MEMBER NAME SCAN  06890000
         SPACE 1                                                        06900000
REPMEMB  EQU   *                                                        06910000
         LA    GR3,UP1(GR3)            UP TO NEXT NAME                  06920000
         BAL   GR9,TESTLAST            CHECK IF UP TO COLUMN 71         06930000
         CLI   SCAN0(GR3),ADOLLAR      VALID MEMBER NAME CHARACTER      06940000
         BE    OK4NAMEM                YES                              06950000
         CLI   SCAN0(GR3),COMMA        NEXT CHARACTER A COMMA           06960000
         BE    CHK4R                   YES- MUST BE R)                  06970000
         BL    BADPARM            IF LT COMMA, ITS AN INVALID CHARACTER 06980000
OK4NAMEM EQU   *                                                        06990000
         ST    GR3,CSTOREG+SAV4        SAVE POINTER START OF NEW NAME   07000000
         SR    GR10,GR10               SECOND PARAMETER COUNT REGISTER  07010000
         SPACE 1                                                        07020000
NAME2LOP EQU   *                                                        07030000
         LA    GR10,UP1(GR10)          UP NAME CHARACTER COUNT          07040000
         TM    CPARAMSW,DELIMEND                                        07050000
         BO    CONTINV               INVALID TO CONTINUE REPLACE A48799 07060000
         CLI   SCAN0(GR3),ANA          USUAL CHARACTERS IN NAME         07070000
         BNL   OKCHAR                  YES- VALID CHARACTER             07080000
         CLI   SCAN0(GR3),ADOLLAR      A DOLLAR SIGN                    07090000
         BE    OKCHAR                  YES, OK                          07100000
         CLI   SCAN0(GR3),ANAT         AN 'AT' SIGN                     07110000
         BE    OKCHAR                  YES- VALID CHARACTER             07120000
         CLI   SCAN0(GR3),APOUND       A 'POUND' SIGN                   07130000
         BNE   BADPARM                 WILL NOT BE , OR ) HERE          07140000
OKCHAR   EQU   *                                                        07150000
         LA    GR3,UP1(GR3)            POINT TO NEXT CHARACTER          07160000
         BAL   GR9,TESTLAST            TEST IF IN COLUMN 71             07170000
         CLI   SCAN0(GR3),COMMA        COMMA                            07180000
         BL    CHKRTPAR                CHECK IF RIGHT PARENTHESIS       07190000
         BH    NAME2LOP                SCAN TO END OF NAME              07200000
         OI    PARMSWCH,SET4REPL+HASNEWNM  SET REPLACE-NEW NAME SWITCH  07210000
         B     ON2TST                  UP POINTER AND RIGHT PAREN COUNT 07220000
         SPACE 1                                                        07230000
CHKRTPAR EQU   *                                                        07240000
         CLI   SCAN0(GR3),ADOLLAR      VALID MEMBER NAME CHARACTER      07250000
         BE    NAME2LOP                YES                              07260000
         CLI   SCAN0(GR3),PARENRGT     RIGHT PARENTHESIS                07270000
         BNE   BADPARM            IF NOT, ITS AN INVALID CHARACTER      07280000
         NI    PARMSWCH,FF-SET4REPL    RESET REPLACE SWITCH             07290000
         OI    PARMSWCH,HASNEWNM       SET NEW NAME- NO REPLACE         07300000
         B     CHKPARNR                UP RIGHT PAREN COUNT             07310000
         SPACE 1                                                        07320000
CHK4R    EQU   *                                                        07330000
         OI    PARMSWCH,SET4REPL       SET REPLACE- NO NEW NAME SW      07340000
         B     ON2TST                  UP POINTERS                      07350000
         SPACE 1                                                        07360000
MEMBRCD  EQU   *                                                        07370000
         TM    PARMSWCH,HASNEWNM       HAVE A NEW NAME                  07380000
         BZ    ONLYOLD                 NO                               07390000
         OI    TABLE0(GR8),SEBIT2      FLAG AS HAS A NEW NAME           07400000
ONLYOLD  TM    PARMSWCH,SET4REPL       REPLACE OPTION                   07410000
         BZ    NOREPL                  NO REPLACE                       07420000
         OI    TABLE0(GR8),REPLACOP    SET REPLACE OPTION               07430000
NOREPL   EQU   *                                                        07440000
         TM    PARMSWCH,HASNEWNM       NEW NAME                         07450000
         BZ    NAMEDONE           NO                                    07460000
         LA    GR9,LEN8                                                 07470000
         LA    GR8,LEN10(GR8)          UP SE TABLE POINTER              07480000
         CR    GR10,GR9                NEW NAME GREATER THAN 8 BYTES    07490000
         BH    BADPARM            IF SO, ITS TOO LONG                   07500000
         MVC   TABLE0(LEN10,GR8),FLAGBYTS CLEAR FLAGS, BLK NAME SE TAB  07510000
         L     GR1,CSTOREG+SAV4        GET START OF NEW NAME            07520000
         BCTR  GR10,GR0                DECREMENT LENGTH FOR EXECUTE     07530000
         EX    GR10,MOVEDDNM           MOVE MEMBER NAME TO SE TABLE     07540000
         OI    TABLE0(GR8),SEBIT1      FLAG AS THIS IS NEW NAME         07550000
         LH    GR6,ENCT                                                 07560000
         LA    GR6,UP1(GR6)                                             07570000
         STH   GR6,ENCT                UP SE ENTRY COUNT                07580000
         LH    GR6,NNCT1                                                07590000
         LA    GR6,UP1(GR6)            INCREMENT NEW NAME COUNT         07600000
         STH   GR6,NNCT1               STORE UPPED NEW NAME COUNT       07610000
         TM    PARMSWCH,SET4REPL       REPLACE OPTION                   07620000
         BZ    NAMEDONE                NO REPLACE                       07630000
         OI    TABLE0(GR8),SEBIT3      FLAG AS MEMBER REPLACE OPTION    07640000
         SPACE 1                                                        07650000
NAMEDONE EQU   *                                                        07660000
         LA    GR8,LEN10(GR8)          UP TO NEXT SLOT SE TABLE         07670000
         LH    GR6,ENCT                                                 07680000
         LA    GR6,UP1(GR6)                                             07690000
         STH   GR6,ENCT                UP SE ENTRY COUNT                07700000
         NI    PARMSWCH,FF-SET4REPL-HASNEWNM  RESET SWITCHES            07710000
         ST    GR8,SESTOP              END OF SE TABLE                  07720000
         SR    GR7,GR7                                                  07730000
         TM    CCDELIM,BLANKSGN        JUST REACHED A BLANK             07740000
         BZ    INDDLOOP                NO- SCAN TILL BLANK              07750000
         CLI   SCAN0(GR3),COMMA        REALLY STOPPED AT COMMA          07760000
         BNE   SET4READ                NO- ALL DONE                     07770000
         SPACE 1                                                        07780000
CHKCONTY EQU   *                                                        07790000
         TM    CPARAMSW,COL72BLK       PUNCH IN CONTINUE COLUMN 72      07800000
         BZ    CONTINV            IF NOT, SET UP INVALID CONTINUE MSG   07810000
         MVI   CCDELIM,CRESET0         RESET STOPPED ON BLANK SWITCH    07820000
         LA    GR10,SCANSOME           RETURN FROM READING NEXTCARD     07830000
         B     GETPARM                 GET NEXT CARD                    07840000
         SPACE 1                                                        07850000
         SPACE 1                                                        07860000
BADPARM  EQU   *                                                        07870000
         LA    GR2,INVALSPR                                             07880000
         B     GIVESCNE           PRT CARD, SET RC TO 4, WRT MSG        07890000
         SPACE 1                                                        07900000
INVALODL EQU   *                                                        07910000
         LA    GR2,INVALIST            OUTDD NOT ON LINK CARD           07920000
         B     GIVESCNE           PRT CARD, SET RC TO 4, WRT MSG        07930000
         SPACE 1                                                        07940000
BADMEMBS EQU   *                                                        07950000
         LA    GR2,MEMNOSE                                              07960000
         B     GIVESCNE           PRT CARD, SET RC TO 4, WRT MSG        07970000
         SPACE 1                                                        07980000
CONTINV  EQU   *                                                 A48799 07990000
         LA    GR2,INVALCON            INVALID CONTINUATION      A48799 08000000
         B     GIVESCNE                GIVE MESSAGE              A48799 08010000
         SPACE 1                                                        08020000
BADREPLC EQU   *                                                        08030000
         LA    GR2,INVALREP                                             08040000
         B     GIVESCNE           PRT CARD, SET RC TO 4, WRT MSG        08050000
         SPACE 1                                                        08060000
NORENAME EQU   *                                                        08070000
         LA    GR2,NORREN                                               08080000
         B     GIVESCNE           PRT CARD, SET RC TO 4, WRT MSG        08090000
         SPACE 1                                                        08100000
MISSPARN EQU   *                                                        08110000
         LA    GR2,ONEQPARN       MESSAGE CODE                          08120000
         B     GIVESCNE           PRT CARD, SET RC TO 4, WRT MSG        08130000
         SPACE 1                                                        08140000
PRTNULL1 EQU   *                                                        08150000
         TM    PARMSWCH,FLUSHSW        FLUSHING                         08160000
         BO    IGNORE                  YES- GET NEXT CARD               08170000
NULLP    EQU   *                                                        08180000
         LA    GR2,NULLPARM            NULL PARAMETER MESSAGE           08190000
         B     GIVESCNE           PRT CARD, SET RC TO 4, WRT MSG        08200000
         SPACE 1                                                        08210000
NOINDDSP EQU   *                                                        08220000
         LA    GR2,NOINDD                                               08230000
         B     GIVESEQS                GIVE MESSAGES                    08240000
         SPACE 1                                                        08250000
         SPACE 1                                                        08260000
SCANERRM EQU   *                                                        08270000
         LA    GR2,MODEERR                                              08280000
         B     ERRSTOP                 GIVE MESSAGE                     08290000
         SPACE 1                                                        08300000
MULTS    EQU   *                                                        08310000
         LA    GR2,MULTSSEE                                             08320000
         SPACE 1                                                        08330000
GIVESEQS EQU   *                                                        08340000
         BAL   GR11,WRITEOUT      PRT CARD, SET RC TO 4, WRT MSG        08350000
SCANSEQR EQU   *                                                        08360000
         LA    GR2,SEQERROR            STATEMENT SEQUENCE ERROR         08370000
         B     ERRSTOP                 GIVE MESSAGE                     08380000
         SPACE 1                                                        08390000
MSSEQER  EQU   *                                                        08400000
         LA    GR2,SEQERROR                                             08410000
         BAL   GR11,WRITEOUT      PRT CARD, SET RC TO 4, WRT MSG        08420000
         B     NEWSCAN                 SCAN THIS LINK- DON'T FLUSH IT   08430000
         SPACE 1                                                        08440000
GIVESCNE EQU   *                                                        08450000
         BAL   GR11,WRITEOUT      PRT CARD, SET RC TO 4, WRT MSG        08460000
SCANERR  EQU   *                                                        08470000
         LA    GR2,SCANMSG                                              08480000
ERRSTOP  EQU   *                                                        08490000
         TM    PARMSWCH,FLUSHSW        STATEMENT ERROR WHILE FLUSHING   08500000
         BO    IGNORE             YES                                   08510000
         MVI   PARMSWCH,FLUSHSW        SET FLUSH SWITCH                 08520000
         TM    CPARAMSW,COL72BLK       IS THIS CARD CONTINUED    A48742 08530000
         BZ    WRITEOUT                NO                        A48742 08540000
         OI    SCANSWCH,NOCMMEXP       SET BYPASS SCAN SWITCH    A48742 08550000
         SPACE 1                                                        08560000
WRITEOUT EQU   *                                                        08570000
         TM    CCSWITCH,CARDPRTD       WAS CARD PRINTED                 08580000
         BO    SETC8                   YES                       A44144 08590000
         BAL   GR9,PRNTCRD             PRINT THE CARD                   08600000
SETC8    EQU   *                                                 A44144 08610000
         CLI   RCBUF,COMPCDE           IS RETURN CODE 8 OR MORE  A44144 08620000
         BNL   STRMESS                 YES- DO NOT CHANGE IT     A44144 08630000
         MVI   RCBUF,COMPCDE           SET CODE TO 8             A44144 08640000
         B     STRMESS                 AND STORE MSG NUMBER      A44144 08650000
SETC4    EQU   *                                                        08660000
         CLI   RCBUF,COMPCD            IS COMPLETION CODE 4 OR MORE     08670000
         BNL   STRMESS                 YES- DO NOT CHANGE IT            08680000
         MVI   RCBUF,COMPCD            SET CODE TO 4                    08690000
STRMESS  EQU   *                                                        08700000
         STH   GR2,MSGLIST             STORE MESSAGE NUMBER             08710000
         MVI   MSGLIST,LASTMSG         FLAG AS LAST MESSAGE             08720000
KEEPCODE EQU   *                                                        08730000
         L     GR15,VZZRLMES           MESSAGE WRITER                   08740000
         BALR  GR14,GR15               GIVE THE MESSAGE                 08750000
         TM    PARMSWCH,FLUSHSW        ERROR                            08760000
         BCR   MS8,GR11                RETURN CARD OR MESSAGE PRINTED   08770000
         L     13,4(13)                                                 08780000
         L     14,12(13)                                                08790000
         L     15,VZZRTERM                                              08800000
         LM    0,12,20(13)                                              08810000
         BR    15                                                       08820000
         SPACE 1                                                        08830000
ZZRSEF   EQU   *                       SYSIN EODAD ROUTINE              08840000
ZZRLEOF  DS    0H                                                       08850000
         OI    CCSWITCH,SYSINEOF       FLAG AS END OF FILE SYSIN        08860000
         TM    CPARAMSW,COL72BLK  EOF AFTER CONTINUED CARD              08870000
         BO    CONTINV            YES- INVALID CONTINUATION             08880000
         TM    PARMSWCH,FLUSHSW        EOF WHILE FLUSHING CARDS         08890000
         BO    TERMS                   GIVE EOF MESSAGE AND QUIT        08900000
         TM    COMDCDSW,LINKDONE       HAD A LINK STATEMENT             08910000
         BZ    NOINDDSP                                                 08920000
         TM    CCSWITCH,ZZRLINKC       ZZRLINK CONTROL CARDS            08930000
         BO    CHKSMEM                 YES- CHECK IF HAVE ALL CARDS     08940000
         TM    COMDCDSW,NEWOUT+NEWINDD HAD AN OUTDD AND AN INDD         08950000
         BO    GETOUT                  BEGIN LINK                       08960000
         B     NOINDDSP                NO- SET TO ISSUE ERROR MESSAGE   08970000
         SPACE 1                                                        08980000
         SPACE 1                                                        08990000
GETOUT   EQU   *                                                        09000000
         MVC   COUNT(LEN2),ENCT        NEED SE COUNT EXCLUDE/SELECT     09010000
         TM    COMDCDSW,SELECTSC       SELECT OPERATION                 09020000
         BZ    TESTEX                  NO- EXCLUDE OR FULL LINK         09030000
         MVC   CTAD(LEN4),SEBEGIN      STORE CTAD FOR SELECT LINK       09040000
         B     DOLINK                  CONTINUE                         09050000
         SPACE 1                                                        09060000
TESTEX   EQU   *                                                        09070000
         XC    NNCT1(LEN2),NNCT1       CLEAR NEW NAME COUNT             09080000
         XC    ENCT(LEN2),ENCT         CLEAR FOR EXCLUDE/FULL           09090000
         MVC   CTAD(LEN4),SESTOP       SET TO SESTOP ON FULL/EXCLUDE    09100000
         TM    COMDCDSW,EXCLUDES       EXCLUSIVE LINK                   09110000
         BO    DOLINK                  YES- FINISH UP                   09120000
         XC    COUNT(LEN2),COUNT       CLEAR SE COUNT ON FULL LINK      09130000
         MVC   SEBEGIN(LEN4),SESTOP    FULL LINK NO ENTRIES IN SE TABLE 09140000
         SPACE 1                                                        09150000
DOLINK   EQU   *                                                        09160000
         NI    CCSWITCH,FF-FIRSTSCN    TURN OFF FIRST ENTRY SWITCH      09170000
         L     GR13,LEN4(GR13)         SAVE AREA POINTER                09180000
         RETURN (14,12)                RETURN TO CALLER                 09190000
         SPACE 1                                                        09200000
TERMS    EQU   *                                                        09210000
         LA    GR2,ENDMESS             SYSIN EOF MESSAGE                09220000
         OI    PARMSWCH,FLUSHSW        SET TO RETURN TO TERMINATE       09230000
         OI    IOEF2,NOSYSIN           SET SWITCH FOR VTM               09240000
         B     STRMESS                 GIVE MESSAGE- NO RETURN          09250000
         SPACE 1                                                        09260000
READCC   EQU   *                                                        09270000
         LA    GR0,CCIMAGE             CONTROL CARD BUFFER              09280000
         LA    GR1,CARDCB              SYSIN DCB                        09290000
         LA    GR8,SYSINERX            SYSIN ERROR RETURN ADDRESS       09300000
         GET   (GR1),(GR0)             GET NEXT STATEMENT               09310000
         NI    CCSWITCH,FF-CARDPRTD    RESET CARD PRINTED SWITCH        09320000
         BR    GR9                     RETURN                           09330000
         SPACE 1                                                        09340000
SYSINERX EQU   *                                                        09350000
         OI    IOEF2,NOSYSIN                                            09360000
         OI    CCSWITCH,SYSINEOF       FAKE AN EOF READ                 09370000
         OI    PARMSWCH,FLUSHSW        SET FLUSH SWITCH                 09380000
         B     KEEPCODE                GIVE SYNAD MESSAGE               09390000
         SPACE 1                                                        09400000
PRNTCRD  EQU   *                                                        09410000
         MVI   MSGLIST,LASTMSG+CTLCD   SET TO PRINT CONTROL CARD        09420000
         MVI   MSGLIST+UP1,CRESET0     CLEAR MSG NUMBER                 09430000
         L     GR15,VZZRLMES           MESSAGE WRITER                   09440000
         BALR  GR14,GR15               PRINT THE CARD                   09450000
         OI    CCSWITCH,CARDPRTD       SET CARD PRINTED SWITCH          09460000
         BR    GR9                     RETURN- CARD IS PRINTED          09470000
         SPACE 1                                                        09480000
CHKLINK  EQU   *                       ANALYSE LINK COMMAND             09490000
         TM    PARMSWCH,FLUSHSW        DOING A FLUSH                    09500000
         BO    NEWSCAN                 YES- RESET COMMAND SWITCHES      09510000
         NI    CCSWITCH,FF-COMDNOW-MULTSE  RESET SWITCHES               09520000
         TM    COMDCDSW,LINKDONE       PREVIOUS LINK DONE               09530000
         BZ    NEWSCAN                 NO- SCAN REST OF CARD            09540000
         SPACE 1                                                        09550000
*  NOW MUST ENTER MAINFLOW TO FINISH UP PREVIOUS LINK OPERATION         09560000
         SPACE 1                                                        09570000
KEEPLINK EQU   *                                                        09580000
         TM    CCSWITCH,ZZRLINKC       HAVE ZZRLINK CONTROL CARDS       09590000
         BO    CHKSMEM                 YES- CHECK IF HAVE ALL CARDS     09600000
         TM    COMDCDSW,NEWINDD        INDD KEYWORD BEFORE LINK         09610000
         BZ    MSSEQER                 NO- SEQUENCE ERROR LINK FOLLOWS  09620000
         NI    COMDCDSW,SELECTSC+EXCLUDES+COMPRESS+LISTSW RESET REST    09630000
         NI    CPARAMSW,COL72BLK       KEEP FLAG COLUMN 72 NOT BLANK ON 09640000
         MVI   CCSWITCH,CRESET0        RESET SWITCH                     09650000
         OI    PARMSWCH,LINKNOW        SET SWITCH TO SHOW LINK PENDING  09660000
         B     GETOUT                  FINISH THIS LINK STEP            09670000
         SPACE 1                                                        09680000
NEWSCAN  EQU   *                                                        09690000
         MVI   PARMSWCH,LINKNOW        SET FOR SCAN, RESET FLUSH        09700000
         NI    SCANSWCH,FF-NOCMMEXP    RESET BYPASS SCAN SWITCH  A48742 09710000
         TM    CCSWITCH,CARDPRTD       IF CONTINUED WAS PRINTED         09720000
         BO    PRNTDONE                ALREADY PRINTED                  09730000
         MVI   LINECT,CRESET0          RESET LINE COUNT TO FORCE HEADER 09740000
         BAL   GR9,PRNTCRD             PRINT HEADER AND LINK CARD       09750000
PRNTDONE EQU   *                                                        09760000
         NI    COMDCDSW,COMPRESS       SAVE COMPRESS FLAG               09770000
         NI    CPARAMSW,FF-PARTPARM-CONTINY  RESET SOME SWITCHES        09780000
         TM    CCSWITCH,LASTPARM       LAST PARAMETER SCANNED           09790000
         BZ    TOSCAN                  NO- GET PARAMETERS               09800000
         TM    CCSWITCH,FIRSTSCN       FIRST ENTRY                      09810000
         BZ    SCANERRM                NO, MIXED CODE                   09820000
         B     NOINDDSP                                                 09830000
         SPACE 1                                                        09840000
CHKSMEM  EQU   *                                                        09850000
         TM    COMDCDSW,EXCLUDES+SELECTSC  SELECT OR EXCLUDE            09860000
         BZ    GETOUT                  NO                               09870000
         TM    COMDCDSW,MEMBRCD1       HAVE MEMBER CARD                 09880000
         BO    GETOUT                  YES                              09890000
         LA    GR2,NOLINK              WILL NOT LINK                    09900000
         B     ERRSTOP                 GIVE MESSAGE AND QUIT            09910000
         SPACE 1                                                        09920000
         EJECT                                                          09930000
*********************************************************************** 09940000
*                      CONTROL STATEMENT ANALYSIS ROUTINES            * 09950000
*********************************************************************** 09960000
         SPACE 1                                                        09970000
RDCARD   EQU   *                                                        09980000
         MVI   CCDELIM,CRESET0         RESET DELIMITER SWITCH           09990000
         ST    GR14,SARG               SAVE RETURN REGISTER             10000000
         TM    CPARAMSW,READ1          IS READ CARD SWITCH ON           10010000
         BZ    GOTCARD                 DON'T READ ANOTHER CARD YET      10020000
         SPACE 1                                                        10030000
KGTCD    EQU   *                                                        10040000
         NI    CPARAMSW,PARTPARM+CONTINY+COL72BLK SAVE ALL CONT SWITS.  10050000
         BAL   GR9,READCC              GET NEXT CARD FROM SYSIN         10060000
         NI    CPARAMSW,FF-COL72BLK    RESET CONTINUED CARD SWITCH      10070000
         CLI   CCIMAGE+COL71,BLANKCOL  COLUMN 72 BLANK                  10080000
         BE    SCANCHK                 YES                       A48742 10090000
         OI    CPARAMSW,COL72BLK       COLUMN 72 NOT BLANK              10100000
SCANCHK  EQU   *                                                 A48742 10110000
         TM    SCANSWCH,NOCMMEXP       SCAN NEEDED               A48742 10120000
         BO    IGNORE                  NO                        A48742 10130000
         B     KPFOL                   START SCAN                A48742 10140000
         SPACE 1                                                        10150000
GOTCARD  EQU   *                                                        10160000
         L     GR3,CSTOREG+SAV8        LOAD ADDRESS OF LAST PASS        10170000
         TM    CPARAMSW,PARMCOME       DOES A PARAMETER FOLLOW          10180000
         BO    SCANCARD                YES- SCAN THE CARD               10190000
         SPACE 1                                                        10200000
KPFOL    EQU   *                                                        10210000
         LA    GR9,KOMMD               RETURN ADDRESS                   10220000
NAMESCAN EQU   *                                                        10230000
         LA    GR3,CCIMAGE             START OF CONTROL STATEMENT       10240000
         CLC   SCAN0(LEN3,GR3),SLASHEOF  /* CARD                        10250000
         BE    ZZRSEF                  YES- ENTER EODAD ROUTINE         10260000
         SR    GR7,GR7                 CLEAR COUNT BUFFER               10270000
         LA    GR1,LEN8                                                 10280000
KNAME    EQU   *                                                        10290000
         CLI   SCAN0(GR3),BLANKCOL     IS COLUMN BLANK                  10300000
         BCR   MS8,GR9                 FIRST COLUMN BLANK OR NAME ENDS  10310000
         LA    GR7,UP1(GR7)            UP CHARACTER COUNT               10320000
         CR    GR7,GR1                 NAME GREATER THAN 8 CHARACTERS   10330000
         BH    BADPARM                 YES- INVALID NAME                10340000
         LA    GR3,UP1(GR3)            GO TO NEXT COLUMN                10350000
         B     KNAME                   CHECK FOR BLANK                  10360000
         SPACE 1                                                        10370000
KOMMD    LA    GR3,UP1(GR3)            SPACE TO NEXT COLUMN AFTER NAME  10380000
         LA    GR5,CCIMAGE+COL70       LAST COLUMN (71)                 10390000
         OI    CCSWITCH,COMDNOW        SET COMMAND SWITCH               10400000
KABC     CLI   SCAN0(GR3),BLANKCOL     IS COLUMN BLANK                  10410000
         BNE   SCANCARD                NO- CHECK PARAMETERS             10420000
         CR    GR3,GR5                 IS THIS THE END COLUMN           10430000
         BE    PRTNULL1                YES- END OF CARD REACHED         10440000
         LA    GR3,UP1(GR3)            ADJUST POINTER TO NEXT COLUMN    10450000
         B     KABC                    CONTINUE SCAN                    10460000
         SPACE 1                                                        10470000
SCANCARD EQU   *                                                        10480000
         MVC   CCDELIM2(UP1),CCDELIM   SAVE DELIMETER SWITCH SETTING    10490000
         MVI   CCDELIM,CRESET0         RESET DELIMITER SWITCH           10500000
         LR    GR1,GR3                 SAVE POINTER TO PRESENT COLUMN   10510000
         ST    GR3,CSTOREG+SAV4        SAVE START OF PARAMETER          10520000
         SR    GR7,GR7                 CLEAR LENGTH REGISTER            10530000
         LA    GR5,CCIMAGE+COL70       LAST COLUMN (71)                 10540000
         TM    CPARAMSW,PARTPARM       PARTIAL PARAMETER                10550000
         BZ    KCONT                   NO- CONTINUE                     10560000
         TM    PARMSWCH,COMDPART       COMMAND WORD                     10570000
         BO    DONTRSTC                YES- KEEP COMDNOW SWITCH ON      10580000
         NI    CCSWITCH,FF-COMDNOW     RESET ANALYSING COMMAND SWITCH   10590000
DONTRSTC EQU   *                                                        10600000
         TM    CCSWITCH,CARDPRTD  WAS CARD PRINTED                      10610000
         BO    NOPR               IF SO, DONT PRINT IT AGAIN            10620000
         BAL   GR9,PRNTCRD        ELSE PRINT CONTINUED CARD             10630000
NOPR     EQU   *                                                        10640000
         L     GR1,CSTOREG+SAV4        RESTORE POINTER                  10650000
         TM    CPARAMSW,CONTINY        FROM MEMBER/INDD SCAN            10660000
         BZ    KOMPAR                  NO- CONTINUE SCAN                10670000
         BCTR  GR3,GR0                 BACK UP POINTER BY ONE           10680000
         NI    PARMSWCH,FF-COMDPART    RESET COMDPART SWITCH            10690000
         B     TESTRETN                STORE AND RETURN                 10700000
         SPACE 1                                                        10710000
KFORZRO  EQU   *                                                        10720000
         TM    CCDELIM,BLANKSGN        IS IT A BLANK                    10730000
         BZ    BADPARM                 INVALID NAME/PARAMETER           10740000
         B     NOTFIRST                NO FIRST PARAMETER PART FAKE IT  10750000
         SPACE 1                                                        10760000
KCONT    EQU   *                                                        10770000
         NI    CCSWITCH,FF-LASTPARM    RESET SWITCH                     10780000
         CR    GR3,GR5                 SCANNED BEYOND COLUMN 71         10790000
         BNH   KOMPAR                  NO- CHECK PARAMETERS             10800000
         SPACE 1                                                        10810000
NOTFIRST EQU   *                                                        10820000
         TM    CPARAMSW,COL72BLK       CONTINUATION PUNCH IN 72         10830000
         BZ    CONTINV            IF NOT, CONTINUATION IS INVALID       10840000
         SR    GR7,GR7                 NO COUNT FOR FIRST PARAMETER     10850000
         B     APARM0                  SET UP FOR PARTIAL PARAMETER     10860000
         SPACE 1                                                        10870000
KOMPAR   CLI   SCAN0(GR3),EQUAL        IS CHARACTER A DELIMITER         10880000
         BNH   DELIMIT                 YES-'EQUAL' CHARACTER OR LOWER   10890000
         LA    GR7,UP1(GR7)            ADD ONE TO LENGTH                10900000
         CR    GR3,GR5                 IS THIS THE END COLUMN           10910000
         BE    KPTERR                  YES- SAVE PARTIAL PARAMETER      10920000
         LA    GR3,UP1(GR3)            UP POINTER TO NEXT COLUMN        10930000
         B     KOMPAR                  CONTINUE SCANNING FOR DELIMITER  10940000
         SPACE 1                                                        10950000
KPTERR   EQU   *                                                        10960000
         TM    CCSWITCH,COMDNOW   SCANNING COMMAND WORD                 10970000
         BZ    NOTCOMD                 NO                               10980000
         CLI   SCAN0(GR1),AC           WORD STARTS WITH C               10990000
         BNE   PRTANDGO                NO                               11000000
         TM    COMDCDSW,LINKDONE       MIGHT BE LINK                    11010000
         BO    KEEPLINK                NOT FIRST LINK                   11020000
         MVI   LINECT,CRESET0          RESET LINE COUNT TO FORCE HEADER 11030000
PRTANDGO EQU   *                                                        11040000
         BAL   GR9,PRNTCRD             PRINT LINK CARD                  11050000
         L     GR1,CSTOREG+SAV4        RESTORE POINTER TO START OF PARM 11060000
         OI    PARMSWCH,COMDPART       SET TO KEEP 'COMDNOW' SET        11070000
NOTCOMD  EQU   *                                                        11080000
         TM    CPARAMSW,COL72BLK       IS COLUMN 72 BLANK               11090000
         BZ    KETBYP                  YES- SET LAST PARM GET NEXT CARD 11100000
KPART    EQU   *                                                        11110000
         LR    GR2,GR7                                                  11120000
         BCTR  GR2,GR0                 DECREMENT FOR EXECUTE            11130000
         EX    GR2,MOVEP               SAVE PARTIAL PARAMETER           11140000
APARM0   EQU   *                                                        11150000
         OI    CPARAMSW,PARTPARM       SET PARTIAL PARAMETER SWITCH     11160000
         ST    GR7,CSTOREG             SAVE PARTIAL PARAMETER COUNT     11170000
         B     KGTCD                   GET NEXT CARD                    11180000
         SPACE 1                                                        11190000
KETBYP   EQU   *                                                        11200000
         OI    CCSWITCH,LASTPARM       NO MORE PARAMETERS               11210000
KPASS    LA    GR3,UP1(GR3)            UP POINTER TO NEXT COLUMN        11220000
KWENT    EQU   *                                                        11230000
         NI    CPARAMSW,FF-READ1-PARMZERO  RESET SWITCHES               11240000
         ST    GR7,CSTOREG             STORE LENGTH REGISTER            11250000
TESTRETN EQU   *                                                        11260000
         L     GR14,SARG               RESTORE RETURN REGISTER          11270000
         ST    GR1,CSTOREG+SAV4        STORE SCAN ADDRESS REGISTER      11280000
         ST    GR3,CSTOREG+SAV8        STORE ADDRESS REGISTER           11290000
         BR    GR14                    RETURN TO MAINSTREAM             11300000
         SPACE 1                                                        11310000
DELIMIT  CR    GR3,GR5                 IS THIS END COLUMN               11320000
         BE    SETCC2                  YES- SET INDICATOR               11330000
         TM    CPARAMSW,PARTPARM       IS PARTIAL PARAMETER SWITCH ON   11340000
         BO    KPPAR                   YES-COMPLETE PARAMETER           11350000
         B     TESTPAR                 CHECK PARAMETER                  11360000
         SPACE 1                                                        11370000
SETCC2   EQU   *                                                        11380000
         OI    CPARAMSW,DELIMEND       SET DELIMITER IN END COLUMN SW   11390000
         CLI   SCAN0(GR3),BLANKCOL     BLANK                            11400000
         BE    ABLNKCOL                YES                              11410000
         TM    CPARAMSW,COL72BLK       PUNCH IN COLUMN 72               11420000
         BZ    CONTINV            IF NOT, CTL STMT IS INVALID           11430000
         TM    CPARAMSW,PARTPARM       PARTIAL PARAMETER                11440000
         BO    CONTINV                 SECOND LEVEL CONTINUATION        11450000
         SPACE 1                                                        11460000
TESTPAR  LTR   GR7,GR7                 TEST IF LENGTH ZERO              11470000
         BC    MS2,DELIM               LENGTH NOT ZERO                  11480000
         OI    CPARAMSW,PARMZERO       SET LENGTH ZERO SWITCH           11490000
         SPACE 1                                                        11500000
DELIM    CLI   SCAN0(GR3),EQUAL        TEST IF DELIMITER IS AN EQUAL    11510000
         BE    KEY                     YES- GO TO KEY WORD ROUTINE      11520000
         CLI   SCAN0(GR3),COMMA        TEST FOR COMMA                   11530000
         BE    PARAMC                  YES- GO TO PARAMETER ROUTINE     11540000
         CLI   SCAN0(GR3),BLANKCOL     TEST FOR BLANK                   11550000
         BNE   BADPARM                 BAD PARAMETER                    11560000
ABLNKCOL EQU   *                                                        11570000
         TM    CCSWITCH,COMDNOW        IS COMMAND SWITCH ON             11580000
         BO    KPCMD                   YES-GO TO ADJUST POINTER         11590000
         MVI   CCDELIM,BLANKSGN        SET BLANK DELIMITER              11600000
DECID    TM    CPARAMSW,DELIMEND       IS DELIMITER IN END COLUMN       11610000
         BO    KWENT                   YES- SET UP COUNT OF FIRST PART  11620000
         TM    CPARAMSW,PARMZERO       LENGTH EQUAL ZERO                11630000
         BO    KFORZRO                 YES- SET UP ZERO LENGTH          11640000
         OI    CPARAMSW,PARMCOME       SET PARAMETER FOLLOWS SWITCH     11650000
         TM    CCDELIM,BLANKSGN        IS IT A BLANK                    11660000
         BZ    KPASS              NO- RETURN WITH POINTERS SET          11670000
         B     KETBYP                  CONTINUE                         11680000
         SPACE 1                                                        11690000
KEY      EQU   *                                                        11700000
         NI    CCSWITCH,FF-COMDNOW     RESET ANALYSING COMMAND SWITCH   11710000
         MVI   CCDELIM,EQUALSGN        SET DELIMITER AN EQUAL           11720000
         CLI   UP1(GR3),BLANKCOL       COLUMN AFTER = BLANK             11730000
         BNE   DECID                   GO TO CHECK DELIMITER            11740000
         TM    COMDCDSW,NEWOUT         HAD AN OUTDD                     11750000
         BZ    NULLP              IF NOT, SET UP NULL PARAMS MSG        11760000
         B     PRTNULL1                PRINT THE CARD                   11770000
         SPACE 1                                                        11780000
PARAMC   EQU   *                                                        11790000
         MVI   CCDELIM,COMMASGN        SET DELIMITER COMMA              11800000
         CLI   UP1(GR3),BLANKCOL       COLUMN AFTER COMMA BLANK         11810000
         BNE   DECID                   NO- CONTINUE                     11820000
         TM    CPARAMSW,COL72BLK       COLUMN 72 BLANK                  11830000
         BZ    CONTINV            IF NOT, CTL STMT IS INVALID           11840000
         B     DECID                   GO TO CHECK DELIMITER            11850000
         SPACE 1                                                        11860000
KPPAR    EQU   *                                                        11870000
        LA    GR9,DELIM               RETURN FROM PARAMETER CREATE      11880000
KCREATE  EQU   *                                                        11890000
         L     GR8,CSTOREG             SIZE OF FIRST PART OF PARAMETER  11900000
         LTR   GR7,GR7            IS LENGTH OF SECOND PART ZERO         11910000
         BC    MS8,KOMPL               YES- THE PARAMETER IS COMPLETE   11920000
         LA    GR6,SAVEPAPR            NO- ADDRESS OF FIRST PART        11930000
         AR    GR6,GR8                 ADDRESS TO MOVE IN SECOND PART   11940000
         LR    GR10,GR7                                                 11950000
         BCTR  GR10,GR0                DECREMENT COUNT FOR EXECUTE      11960000
         EX    GR10,MOVEPP             MOVE SECOND PART TO BUFFER       11970000
KOMPL    AR    GR7,GR8                 TOTAL LENGTH OF PARAMETER        11980000
         LA    GR1,SAVEPAPR            LOAD PARAMETER BUFFER ADDRESS    11990000
         MVC   CCDELIM2(UP1),CCDELIM   SAVE DELIMITER                   12000000
         MVI   CCDELIM,CRESET0         CLEAR DELIMITER SETTINGS         12010000
         NI    CPARAMSW,FF-CONTINY-PARTPARM  RESET CONTINUED SWITCHES   12020000
         NI    PARMSWCH,FF-COMDPART    RESET COMDPART SWITCH            12030000
         BR    GR9                     RETURN                           12040000
         SPACE 1                                                        12050000
KPCMD    LA    GR3,UP1(GR3)            ADJUST POINTER TO NEXT COLUMN    12060000
         CLI   SCAN0(GR3),BLANKCOL     IS COLUMN BLANK                  12070000
         BNE   KPCMA                   NO- CHECK FOR COMMA              12080000
         CR    GR5,GR3                 IS THIS THE LAST COLUMN          12090000
         BE    KETBYP                  YES- NO PARAMETER FOLLOWS        12100000
         B     KPCMD                   CONTINUE SCAN                    12110000
         SPACE 1                                                        12120000
KPCMA    CLI   SCAN0(GR3),COMMA        IS COMMA THE FIRST CHARACTER     12130000
         BE    KETBYP                  YES- NO PARAMETER FOLLOWS        12140000
         BCTR  GR3,GR0                 BACK UP REGISTER 3 TO PARAMETER  12150000
         OI    CPARAMSW,PARMCOME       SET PARAMETER FOLLOWS SWITCH     12160000
         B     KPASS                   GO TO RETURN                     12170000
         SPACE 1                                                        12180000
*                                                                       12190000
*                 EXECUTE STATEMENTS USED IN SCAN ROUTINE               12200000
*                                                                       12210000
         SPACE 1                                                        12220000
MOVEP    MVC   SAVEPAPR(UP1),SCAN0(GR1) STORE PARTIAL PARAMETER         12230000
MOVEPP   MVC   SCAN0(UP1,GR6),SCAN0(GR1) RECREATES PARAMETER            12240000
OPRLUP3  MVC   SARG(UP1),SCAN0(SCANADR)  MOVE TO SEARCH ARGUMENT        12250000
MOVEDDNM MVC   LEN2(UP1,GR8),SCAN0(GR1)  MOVE DD NAME TO INDD TABLE     12260000
         SPACE 1                                                        12270000
LISTALLC DC    C'NO'                   DO NOT LIST MEMBERS COPIED       12280000
RPARN    DC    C'R)'                   FLAGS RENAME OR REPLACE          12290000
SLASHEOF DC    C'/* '                  SIGNALS EOF                      12300000
         SPACE 1                                                        12310000
FLAGBYTS DC    XL10'00004040404040404040'  INDD SE TABLE ENTRY FORM     12320000
BLANKS8  EQU   FLAGBYTS+2              DOUBLE WORD OF BLANKS            12330000
         SPACE 1                                                        12340000
         SPACE 1                                                        12350000
         SPACE 1                                                        12360000
PATCHLEN EQU   (*-ZZRESCAN)/20    LENGTH OF 5 PER CENT PATCH AREA       12370000
PATCH    DC    XL(PATCHLEN)'00'   5 PER CENT PATCH AREA          A41780 12380000
         EJECT                                                          12390000
         SPACE 1                                                        12400000
         SPACE 1                                                        12410000
         DCBD  DSORG=PS                                                 12420000
BOFLGS   EQU   DCBOFLGS-IHADCB         OPEN FLAGS IN DCB                12430000
DDNAME1  EQU   DCBDDNAM-IHADCB         DD NAME FIELD IN DCB             12440000
SYNADADR EQU   DCBSYNAD-IHADCB         SYNAD ADDRESS IN DCB             12450000
EOFADDRS EQU   DCBEODAD-IHADCB+1       EODAD ADDRESS IN DCB             12460000
         SPACE 1                                                        12470000
         EJECT                                                          12480000
ZZRELCOM DSECT                                                          12490000
         TITLE 'Z Z R E L C O M  -  ZZRELINK COMMUNICATIONS AREA'       00010000
**********       C O M M U N I C A T I O N    A R E A        ********** 00020000
**********               Z Z R E L I N K                     ********** 00030000
*                                                                       00040000
* THE FOLLOWING DESCRIPTION IS FOR ZZRELINK.                            00050000
* IT IS BASICALLY THE SAME FOR THE IEBCOPY PROCEDURE EXCEPT THAT        00060000
* IT DOES NOT GO THROUGH ALL THE SORTING STEPS. IT ALSO DOES NOT BUILD  00070000
* MULTIPLE DD TABLES AND IT DOES NOT ALLOW FOR RENAMES.                 00080000
*                                                                       00090000
         SPACE 2                                                        00100000
         SPACE 2                                                        00110000
* ABBREVIATIONS USED FREQUENTLY -                                       00120000
*        ODE = OUTPUT DIRECTORY ENTRY                                   00130000
*        IDE = INPUT DIRECTORY ENTRY                                    00140000
         SPACE 2                                                        00150000
*                                                                       00160000
* SETAB = SELECT/EXCLUDE TABLE                                          00170000
*        THE SETAB CONSISTS OF 10-BYTE ENTRIES.  ENTRIES ARE MADE TO    00180000
*        THIS TABLE WHEN -                                              00190000
*              1) A SELECTIVE LINK HAS BEEN SPECIFIED.  EACH ENTRY IS   00200000
*              FOR THE NAME OF A MEMBER TO BE SELECTED.  IF A MEMBER IS 00210000
*              TO BE RENAMED, TWO 10-BYTE ENTRIES ARE MADE IN THE SETAB 00220000
*              2) AN EXCLUSIVE LINK HAS BEEN SPECIFIED.  EACH ENTRY IS  00230000
*              FOR THE NAME OF A MEMBER TO BE EXCLUDED.                 00240000
*        NOTE- IN THE CASE OF A SELECTIVE LINK (AND ONLY IN THIS CASE), 00250000
*              THE SETAB WILL ALSO BE USED AS THE CTLTAB.  IN ALL OTHER 00260000
*              CASES, A SEPARATE CTLTAB WILL BE CONSTRUCTED.            00270000
*        BYTE 0 OF AN ENTRY IS DESIGNATED AS SEFLAG1.  WITHIN THIS BYTE 00280000
*        THE FOLLOWING BITS HAVE MEANING -                              00290000
SEBIT1   EQU   X'80' ON=THIS IS A NEWNAME ENTRY                         00300000
SEBIT2   EQU   X'40' ON=THIS IS A RENAMED ENTRY                         00310000
SEBIT3   EQU   X'20' ON=REPLACE OPTION WAS SPECIFIED FOR THIS MEMBER    00320000
SEBIT4   EQU   X'10' ON=DONTLINK FLAG...DO NOT PROCESS THIS ENTRY       00330000
SEBIT5   EQU   X'08' ON= THIS MEMBER HAS BEEN ''FOUND'' ON INPUT D.S.   00340000
SEBIT6   EQU   X'04' ON= THIS IS LAST ENTRY IN SETAB/CTLTAB             00350000
*        LO ORDER 2 BITS NOT USED                                       00360000
*        NOTE THE DEFINITION OF SEBIT3.  IF, IN THE INDD TABLE, BYTE 0  00370000
*              BIT 2 IS ON, THIS MEANS THAT THE REPLACE OPTION WAS SPE- 00380000
*              CIFIED FOR ALL MEMBERS COPIED FROM THIS INDD.  IT IS     00390000
*              VALID FOR BOTH OF THESE BITS TO BE ON AT THE SAME TIME,  00400000
*              ALTHOUGH IF THE BIT IS ON IN THE INDD TABLE, IT IS UN-   00410000
*              NECESSARY FOR SEBIT3 TO ALSO BE ON IN THE SETAB.         00420000
         EJECT                                                          00430000
* FOLLOWING IS A DESCRIPTION OF THE CONTROL TABLE -CTLTAB-              00440000
* (REMEMBER THAT THIS TABLE IS PHYSICALLY THE SAME TABLE AS SETAB WHEN  00450000
* A SELECTIVE LINK OPERATION IS SPECIFIED, BUT IS PHYSICALLY INDEPEN-   00460000
* DENT AND DISTINCT FROM THE SETAB FOR AN EXCLUSIVE LINK OPERATION.     00470000
* THERE IS NO SETAB FOR A FULL LINK OPERATION, AND THE CTLTAB IN THIS   00480000
* CASE IS CONSTRUCTED SIMILARLY TO WHEN AN EXCLUSIVE LINK IS SPECIFIED. 00490000
* SINCE THE SETAB AND CTLTAB ARE ONE AND THE SAME FOR A SEL. LINK, THIS 00500000
*  DESCRIPTION WILL ASSUME THAT A SELECTIVE LINK IS BEING DONE, FOR THE 00510000
*  PURPOSE OF SETTING UP THE TABLE INITIALLY.)....                      00520000
         SPACE 2                                                        00530000
*        INITIAL TABLE, FOLLOWING CCSCAN PROCESSING OF ''SELECT'' -     00540000
*********************************************************************** 00550000
*SEFLAG1 *SEFLAG2 * NAME OF MEMBER TO BE SELECTED, OR, IF SPECIFIED,  * 00560000
*(DESCR. *(UNUSED)* NEWNAME.  IF NEWNAME WAS SPECIFIED, THE TABLE WILL* 00570000
* ABOVE) *        * CONTAIN 1 ENTRY FOR OLDNAME AND ANOTHER FOR NEW.  * 00580000
*********************************************************************** 00590000
* 1 BYTE * 1 BYTE *----------------------8 BYTES----------------------* 00600000
         SPACE 2                                                        00610000
* OLDNAME/NEWNAME PAIRS ARE NOW EXTRACTED FROM THE SETAB.  THEN THE     00620000
* CTLTAB IS SORTED ALPH. BY MBRNAME, AND A NEWNAME PTRTABLE SET UP.     00630000
* WHEN THIS HAS BEEN DONE, THE INPUT DATA SET'S DIRECTORY IS SEARCHED   00640000
* FOR MATCHING NAMES (THE NEWNAME ENTRIES IN THE TABLE ARE NOT USED FOR 00650000
* THIS COMPARISON).  WHEN A MATCHING MEMBERNAME IS FOUND, THE DIRECTORY 00660000
* ENTRY IS RETAINED IN CORE (IF SPACE PERMITS), OR IT IS SPILLED ONTO   00670000
* SYSUT3.  IN EITHER CASE, THE CORE ADDRESS OR THE TTR+INDICATOR ARE    00680000
* PUT INTO THE CTLTAB, OVERLAYING THE HIGH-ORDER 4 BYTES OF 'OLDNAME'.  00690000
* THE MEMBER-TTR IS EXTRACTED FROM THE DIRECTORY ENTRY, AND OVERLAYS    00700000
* THE LOW-ORDER 4 BYTES OF 'OLDNAME' IN THE CTLTAB.  SEBIT5 IS TURNED   00710000
* ON.  IF THIS IS AN ALIAS ENTRY, SEFLAG2 IS SET AS FOLLOWS -           00720000
ALIAS    EQU   X'80'         TO TEST FOR AND SET ALIAS DIRECTORY ENTRY  00730000
         SPACE 2                                                        00740000
*        CTLTAB ENTRY FOR A ''FOUND'' MEMBER -                          00750000
         SPACE 2                                                        00760000
*********************************************************************** 00770000
*SEFLAG1 *SEFLAG2 * INDIC. * ADDR OF THE IN-* ZEROES *   MEMBER TTR   * 00780000
*        *        *  BYTE  * PUT DIR. ENTRY *        *                * 00790000
*********************************************************************** 00800000
*-1 BYTE-*-1 BYTE-*-1 BYTE-*-----3 BYTES----*-1 BYTE-*-----3 BYTES----* 00810000
         SPACE 2                                                        00820000
* THE INDICATOR BYTE IS ZEROES IF THE DIRECTORY ENTRY IS IN CORE, OR IT 00830000
* IS HEX '01' IF THE DIRECTORY ENTRY WAS SPILLED TO SYSUT3.             00840000
* NOTE THAT CTLTAB ENTRIES FOR A ''NEWNAME'' ARE NOT OVERLAYED OR AL-   00850000
* TERED AT ANY TIME.  ONCE THE ENTIRE INPUT DIRECTORY HAS BEEN SCANNED, 00860000
* (OR AT LEAST ALL ENTRIES FOR MEMBERS TO BE COPIED FROM THIS INPUT     00870000
* DATA SET HAVE BEEN BUILT), THE OUTPUT DATA SET DIRECTORY IS READ.     00880000
* MEMBERNAMES OF MEMBERS CURRENTLY IN THE OUTPUT DATA SET ARE COMPARED  00890000
* AGAINST MEMBERNAMES OF MEMBERS REFERENCED IN THE CTLTAB FOR THE CUR-  00900000
* RENT INPUT DATA SET, UNLESS THE LATTER WERE RENAMED.  IF THE INPUT    00910000
* MEMBER IS RENAMED, THEN THE NEWNAME IS COMPARED AGAINST THE OUTPUT.   00920000
* IF DUPLICATE NAMES ARE ENCOUNTERED, AND IF THE REPLACE OPTION WAS NOT 00930000
* SPECIFIED ON EITHER THE INDD LEVEL OR THE MEMBERNAME LEVEL, THEN THE  00940000
* DONT-LINK BIT (SEBIT4) IS SET IN THE FLAG BYTE OF THE APPROPRIATE     00950000
* CTLTAB ENTRY, AND THE INDIC. BYTE (BYTE 3) OF THIS ENTRY IS SET TO    00960000
* HEX 'FF'.                                                             00970000
* THINK OF THE LOW-ORDER 8-BYTES OF EACH ''FOUND'' CTLTAB ENTRY AS BE-  00980000
* ING DIVIDED INTO A LEFT HALF (INDIC. + DIR. ENTRY ADDR.) AND A RIGHT  00990000
* HALF (ZEROES + MBR. TTR).  THE LEFT HALF NOW REPRESENTS DIRECTORY     01000000
* ENTRIES FOUND IN THE CURRENT INPUT DATA SET, AND IS IN ALPHAMERIC     01010000
* SEQUENCE.                                                             01020000
         EJECT                                                          01030000
* THE NEXT STEP IN CTLTAB PROCESSING CAUSES THE ''FOUND'' ENTRIES TO BE 01040000
* MANIPULATED, WITH THE RESULT BEING THAT THE LEFT HALF CONTAINS (IN-   01050000
* DIC + ADDR OF DIR. ENTRY) IN SEQUENCE BY MEMBER TTR, AND THE RIGHT    01060000
* HALF CONTAINS THIS SAME INFORMATION (INDIC. + ADDR OF DIR. ENTRY) IN  01070000
* ALPHAMERIC SEQUENCE, OVERLAYING THE ACTUAL MEMBER TTR.  ANOTHER RE-   01080000
* SULT OF THIS MANIPULATION OF THE CTLTAB IS THAT MAIN-MEMBER ENTRIES   01090000
* PRECEDE THE CORRESPONDING ALIAS ENTRIES.  NOTE THAT THE BITS SET IN   01100000
* SEFLAG1 ARE NOW ONLY USEFUL FOR THE ''RIGHT HALF'' OF THE CTLTAB,     01110000
* SINCE THEY ARE NOT MANIPULATED AND THUS REMAIN IN THE ORIGINAL (AL-   01120000
* PHABETIC) SEQUENCE.                                                   01130000
         SPACE 2                                                        01140000
*        CTLTAB ENTRY FOR A FOUND MEMBER FOLLOWING TTR SORT -           01150000
         SPACE 2                                                        01160000
*********************************************************************** 01170000
*SEFLAG1 *SEFLAG2 * INDIC. * ADDR OF IN. DE * INDIC. * ADDR OF IN. DE * 01180000
*        *        *  BYTE  *                *  BYTE  *                * 01190000
*        *        *IN SEQ BY MEMBER TTR     *IN SEQ ALPHAMERICALLY    * 01200000
*********************************************************************** 01210000
* 1 BYTE-*-1 BYTE-*-1 BYTE-*-----3 BYTES----*-1 BYTE-*-----3 BYTES----* 01220000
         SPACE 2                                                        01230000
* AT THIS POINT, THERE IS NO LOGICAL RELATIONSHIP BETWEEN THE LEFT AND  01240000
* RIGHT HALVES OF THE RELEVANT CTLTAB ENTRIES.  BOTH HALVES CONTAIN THE 01250000
* SAME INFORMATION, BUT IT IS IN TWO DIFFERENT SEQUENCES.  NOTE THAT,   01260000
* IF A MEMBER BEING LOOKED FOR IS NOT ''FOUND'', ITS CTLTAB ENTRY RE-   01270000
* MAINS UNALTERED - THE NAME IS STILL UN-OVERLAYED.                     01280000
* FROM THIS POINT ON, THOSE MEMBERS OF THE INPUT DATA SET WHOSE DIREC-  01290000
* TORY ENTRIES ARE REFERENCED IN THE CTLTAB WILL BE COPIED, PROVIDED    01300000
* THAT THEY ARE ''FOUND'' AND NOT FLAGGED AS ''DONT-LINK''.  THEN THE   01310000
* DIRECTORY ENTRIES WILL BE MERGED.  AS THE MERGE IS PERFORMED, WHEN AN 01320000
* INPUT DE IS MERGED, IF THIS IS A SELECTIVE LINK, THE DONT-LINK BIT    01330000
* (SEBIT4) IS TURNED ON, THUS ALLOWING FOR THESE ENTRIES IN THE CTLTAB  01340000
* TO BE IGNORED IN SUBSEQUENT PASSES THROUGH THE SAME TABLE FOR THE EN- 01350000
* SUING INPUT DATA SETS.                                                01360000
JSTCPD   EQU   X'10'              IF ON IN THE 2ND BYTE OF A CTLTAB EN- 01370000
*                                 TRY (SEFLAG2), THERE IS A NAME (CON-  01380000
*                                 TAINED IN THE 3RD THROUGH 10TH BYTES  01390000
*                                 OF THIS ENTRY) OF AN INPUT MEMBER     01400000
*                                 WHICH HAS BEEN SUCCESSFULLY COPIED.   01410000
*                                 THIS MEMBERNAME WILL BE PRINTED BY    01420000
*                                 THE TERMINATION MODULE (ZZRVTM) UNDER 01430000
*                                 APPROPRIATE CONDITIONS, AT WHICH TIME 01440000
*                                 THIS BIT WILL BE TURNED OFF.          01450000
*********************************************************************** 01460000
         TITLE 'Z Z R E L I N K  -  CONTROL, DDNAME, AND SELECT TABLES' 01470000
*XXXXXXXXXX    EVERY CONSTANT BETW. HERE AND NXT X'S MUST BE CONTIGUOUS 01480000
* ALL CONSTANTS BETWEEN X'S ARE SET TO ZERO FOR EVERY INPUT DATA SET.   01490000
* THE NAMES OF THE CONSTANTS WHICH ARE INCLUDED IN THIS CONTIG. AREA -  01500000
* CDCT, FCT, NNTCT, OLDTTR, FLG5                                        01510000
* ICPT,SVLSTO,SVFSTO,FLG2,FLG6,SWITCH1,UTTRFLAG,OBCT                    01520000
         SPACE 2                                                        01530000
*********************************************************************** 01540000
INBEGIN  DC    F'0'          ADDRESS OF START OF INDD TABLE             01550000
REPLACOP EQU   SEBIT3        IF ON IN HI-ORDER BYTE OF AN INDD-TABLE    01560000
*                            ENTRY, THE REPLACE OPTION WAS SPECIFIED    01570000
*                            FOR THIS INPUT DATA SET.                   01580000
SEBEGIN  DC    AL4(0)        ADDR OF BEGINNING OF SETAB                 01590000
SESTOP   DC    F'0'          IF SELECTIVE OR EXCLUSIVE LINK, THIS IS    01600000
*                            THE ADDRESS (+1) OF THE END OF THE SETAB.  01610000
*                            IF FULL LINK, ADDR (+1) OF END OF INDDTAB. 01620000
ADNNPTRT DS    AL4           ADDR OF NEWNAME POINTER TABLE              01630000
CTAD     DS    AL4           ADDR OF CONTROL TABLE - IF SELECTIVE LINK, 01640000
*                            THIS WILL BE SAME AS SEBEGIN.              01650000
ENCT     DC    HL2'0'        COUNT OF NBR OF ENTRIES IN CTLTAB (TOTAL)  01660000
*              IF THIS IS A SELECTIVE LINK, THIS COUNT WILL BE INI-     01670000
*              TIALIZED TO THE NUMBER OF ENTRIES IN SETAB.              01680000
COUNT    DC    HL2'0'        COUNT OF NBR OF ENTRIES IN SETAB IF SEL OR 01690000
*                            EXCL LINK, OR ZERO IF FULL LINK.           01700000
INDDCT   DC    AL2(0)        COUNT OF NO. INDD'S IN CURRENT STEP        01710000
NNCT1    DC    AL2(0)        NBR OF NEWNAMES SPECIFIED IN CURRENT SE-   01720000
*                            LECTIVE LINK STEP (IF NOT SEL CPY, = 0)    01730000
         DS    0D                                                       01740000
OUTNAME  DC    CL8' '        NAME OF CURRENT OUTPUT DD                  01750000
         SPACE 2                                                        01760000
CCIMAGE  DS    10D                     SCAN CONTROL CARD BUFFER         01770000
         SPACE 2                                                        01780000
WKA1     DC    10F'0'        VOLATILE WORK AREA FOR GENERAL USE         01790000
*                                                                       01800000
*****   PLEASE NOTE---    ZZRESCAN IS NOT THE ONLY MODULE WHICH USES    01810000
**                WKA1 FOR TEMPORARY WORK SPACE.                        01820000
*                 HOWEVER, THE FOLLOWING 4 EQUATES ARE USED ONLY BY     01830000
*                 ZZRESCAN -                                            01840000
*                                                                       01850000
SARG     EQU   WKA1          TEMPORARY WORK AREA USED BY ZZRESCAN       01860000
*                            TO HOLD SEARCH ARGUMENT 8 BYTES LONG       01870000
SAVEPAPR EQU   WKA1+8        TEMPORARY WORK AREA USED BY ZZRESCAN       01880000
*                            TO HOLD PARTIAL PARAMETERS IF CONTROL CARD 01890000
*                            IS CONTINUED, 8 BYTES LONG.                01900000
LEFTPCNT EQU   WKA1+16       TEMPORARY WORK AREA USED BY ZZRESCAN       01910000
*                            TO HOLD COUNT OF LEFT PARENTHESIS SCANNED. 01920000
RGHTPCNT EQU   WKA1+18       TEMPORARY WORK AREA USED BY ZZRESCAN       01930000
*                            TO HOLD COUNT OF RIGHT PARENTHESIS SCANNED 01940000
*                            BOTH LEFTPCNT AND RGHTPCNT 2 BYTES EACH    01950000
*                            AND MUST BE CONTIGUOUS STORAGE.            01960000
CSTOREG  DC    3F'0'         SAVE AREA USED BY ZZRESCAN                 01970000
         SPACE 2                                                        01980000
*        REGISTER SAVE AREAS                                            01990000
SV1      DC    18F'0'        REGISTER SAVE AREA FOR MAINFLOW            02000000
SV2      DC    18F'0'        REGISTER SAVE AREA FOR NON-RESIDENT RTNES  02010000
MCAMOD   DC    10F'0'             ZZRELCOM CHANGE AREA                  02020000
* MCAMOD IS A PATCH-AREA FOR USE IN MAINTENANCE OF THIS PROGRAM         02030000
         TITLE 'POINTERS FOR INTER-MODULE COMMUNICATION'                02040000
VZZRSCN  DC    V(ZZRESCAN)         ADDR OF CONTROL CARD SCAN ROUTINE    02050000
VZZRLEOF DC    V(ZZRLEOF) EP ADDRESS OF SYSIN EODAD EXIT IN ZZRESCAN    02060000
VZZRLMES DC    V(ZZMESAGE)         ADDRESS OF MESSAGE WRITER ROUTINE    02070000
VZZRTERM DC    V(ZZRTERM)         ADDRESS OF TERMINATION ROUTINE        02080000
         ORG   VZZRTERM                                                 02090000
AZZRTERM DS    F                  ADDRESS OF TERMINATION ROUTINE        02100000
         TITLE 'S Y S I N    D A T A    C O N T R O L    B L O C K'     02110000
*CARDCB  DCB   DDNAME=SYSIN,RECFM=FB,LRECL=80,EODAD=VZZRLEOF,           02120000
*              MACRF=(GM),DSORG=PS                                      02130000
CARDCB   DCB   DDNAME=SYSIN,RECFM=FB,LRECL=80,EODAD=VZZRLEOF,          X02140000
               MACRF=(GM),DSORG=PS                                      02150000
         TITLE 'S Y S P R I N T    D A T A    C O N T R O L    B L O C X02160000
               K'                                                       02170000
* PRTDCB DCB   DDNAME=SYSPRINT,DSORG=PS,MACRF=(PM),RECFM=FBA,           02180000
*              LRECL=121,BLKSIZE=121                                    02190000
PRTDCB   DCB   DDNAME=SYSPRINT,DSORG=PS,MACRF=(PM),RECFM=FBA,          X02200000
               LRECL=121,BLKSIZE=121                                    02210000
         TITLE 'SWITCH AND WORK AREA DEFINITIONS FOR ZZRESCAN'          02220000
* THE FOLLOWING SWITCHES (PARMSWCH, CCSWITCH, COMDCDSW, CPARAMSW, CCDE- 02230000
* LIM, CCDELIM2) ARE PRIMARILY BUT NOT SOLELY USED BY ZZRESCAN -        02240000
*                                                                       02250000
         SPACE 1                                                        02260000
PARMSWCH DC    XL1'0'    SCAN INTERNAL INDD AND MEMBER SWITCHES         02270000
SCANNAME EQU   X'80'         SCANNING NAME                              02280000
SET4REPL EQU   X'40'         MULTIPLE ( EXPECT REPLACE                  02290000
ONELEFT  EQU   X'20'         FIRST LEFT PARENTHESIS                     02300000
LINKNOW  EQU   X'10'         NOW SCANNING LINK CARD                     02310000
STOPSCAN EQU   X'08'         BLANK ENCOUNTERED                          02320000
FLUSHSW  EQU   X'04'                   FLUSH TO NEXT LINK RE/SET ALONE  02330000
HASNEWNM EQU   X'02'         HAVE A NEW NAME WITH MEMBER                02340000
COMDPART EQU   X'01'         PARTIAL COMMAND- CONTINUED ON NEXT CARD    02350000
*                                                                       02360000
         SPACE 1                                                        02370000
CCSWITCH DC    XL1'0'                  CONTROL CARD SWITCHES- EXTERNAL  02380000
CARDPRTD EQU   X'80'         ON = CONTROL STATEMENT HAS BEEN PRINTED    02390000
SYSINEOF EQU   X'40'         END OF FILE ON SYSIN                       02400000
UNECPARN EQU   X'20'         INDD/MEMBER NAMES IMBEDDED IN PARENTHESIS  02410000
ZZRLINKC EQU   X'10'         ZZRLINK CONTROL CARDS                      02420000
COMDNOW  EQU   X'08'         COMMAND WORD                               02430000
LASTPARM EQU   X'04'         LAST PARAMETER- BYPASS SWITCH              02440000
MULTSE   EQU   X'02' MULTIPLE SELECT/EXCLUDE STATEMENTS                 02450000
FIRSTSCN EQU   X'01' ON=ZZRESCAN HAS BEEN CALLED FOR THE FIRST TIME     02460000
*                                                                       02470000
         SPACE 2                                                        02480000
COMDCDSW DC    XL1'0'           SOME EXTERNAL SWITCHES                  02490000
LINKDONE EQU   X'80'         LINK COMMAND SCANNED ALL OK                02500000
SELECTSC EQU   X'40'         SELECT COMMAND SCANNED                     02510000
EXCLUDES EQU   X'20'         EXCLUDE COMMAND SCANNED                    02520000
NEWOUT   EQU   X'10'         OUTDD KEYWORD PRESENT                      02530000
NEWINDD  EQU   X'08'         INDD KEYWORD PRESENT                       02540000
LISTSW   EQU   X'04'         DO NOT LIST MEMBERS COPIED (LIST=NO)       02550000
COMPRESS EQU   X'02'                   COMPRESS LINK DATA SET           02560000
MEMBRCD1 EQU   X'01'         MEMBER STATEMENT                           02570000
*                                                                       02580000
         SPACE 2                                                        02590000
CPARAMSW DC    XL1'0'       INTERNAL SCAN SWITCHES                      02600000
DELIMEND EQU   X'80'         DELIMITER IN COLUMN 71                     02610000
CONTINY  EQU   X'40'         CONTINUATION                               02620000
PARMCOME EQU   X'20'         PARAMETER FOLLOWS                          02630000
PARTPARM EQU   X'10'         PARTIAL PARAMETER                          02640000
READ1    EQU   X'08'         READ ANOTHER CONTROL STATEMENT             02650000
COMDPARM EQU   X'04'         COMMAND FOLLOWED BY PARAMETER              02660000
COL72BLK EQU   X'02'         COLUMN 72 NOT BLANK                        02670000
PARMZERO EQU   X'01'         PARAMETER LENGTH ZERO                      02680000
*                                                                       02690000
         SPACE 2                                                 A48742 02700000
SCANSWCH DC    XL1'0'        INTERNAL SCAN SWITCHES              A48742 02710000
NOCMMEXP EQU   X'80'         DON'T SCAN COMMAND ON CONTIN CARDS  A48742 02720000
*              LOW ORDER 7 BITS NOT USED - RESERVED              A48742 02730000
*                                                                       02740000
         SPACE 2                                                        02750000
CCDELIM  DC    XL1'0'        INTERNAL SCAN SWITCHES                     02760000
EQUALSGN EQU   X'80'         EQUAL SIGN                                 02770000
COMMASGN EQU   X'40'         COMMA                                      02780000
LEFTPRSG EQU   X'20'         LEFT PARENTHESIS                           02790000
RIGHTPRS EQU   X'10'         RIGHT PARENTHESIS                          02800000
BLANKSGN EQU   X'08'         BLANK                                      02810000
LASTCOMA EQU   X'04'         LAST DELIMITER A COMMA- READ A CARD        02820000
BADBLOCK EQU   X'02' VALIDATE-ZZRDV0 SETS IF SYSIN/SYSPRINT BLOCKSIZE   02830000
*          IS BAD.                                                      02840000
*              LO ORDER BIT NOT USED                                    02850000
*                                                                       02860000
         SPACE 2                                                        02870000
CCDELIM2 DC    XL1'0'  USED TO SAVE SETTINGS OF CCDELIM ON CONTINUATION 02880000
*                                                                       02890000
         TITLE 'ERROR FLAG DEFINITIONS USED BY OPEN FAILURE ROUTINE'    02900000
IOEF2    DC    X'00'              FLAGS DESCRIBING NATURE/TYPE OF I/O   02910000
*                                 ERROR                                 02920000
ERF9     EQU   X'80'         ON = 'HARD' ERROR WRITING MERGED OUTPUT    02930000
*                                 DIRECTORY TO SYSUT4                   02940000
ERF10    EQU   X'40'         ON = ERROR READING FROM SYSUT4.  IF 'ERF4' 02950000
*                                 OFF, ERROR OCCURRED DURING MERGE      02960000
*                                 PHASE OF PROGRAM - IF 'ERF4' ON, SEE  02970000
*                                 DESCRIPTION OF 'ERF4'.                02980000
NOSYSIN  EQU   X'20'         ON = SYSIN COULD NOT BE OPENED OR WAS IN-  02990000
*                                 VALID, OR BECAME UNUSABLE DUE TO AN   03000000
*                                 I/O ERROR                             03010000
SPRNOPN  EQU   X'10'         SYSPRINT COULD NOT BE OPENED, WAS INVALID- 03020000
*                                 LY SPECIFIED, OR AN I/O ERROR OCCUR-  03030000
*                                 RED MAKING SYSPRINT UNAVAILABLE       03040000
*              LOW ORDER 4 BITS NOT USED - RESERVED                     03050000
*                                                                       03060000
         TITLE 'SWITCH AND WORK AREA DEFINITIONS FOR ZZMESAGE'          03070000
         DS    0H                                                       03080000
MSGLIST  DC    4H'0'              AREA FOR PARAMETRIC INPUT TO ZZMESAGE 03090000
MSG1     EQU   MSGLIST                                                  03100000
MSG2     EQU   MSG1+2                                                   03110000
MSG3     EQU   MSG2+2                                                   03120000
MSG4     EQU   MSG3+2                                                   03130000
* THE FOLLOWING BITS WILL BE SET ON BY THE CALLER OF ZZMESAGE (MESSAGE  03140000
* WRITING ROUTINE) IN THE HIGH ORDER BYTE OF EACH APPROPRIATE HALFWORD  03150000
* IN THE MSGLIST PARAMETER(S) BEING USED -                              03160000
LASTMSG  EQU   X'80'         ON = LAST PARAMETER IN MSGLIST             03170000
CTLCD    EQU   X'40'         ON = A CONTROL CARD IS TO BE PRINTED       03180000
IOERF    EQU   X'20'         ON = A MESSAGE IS IN THE MESSAGE BUFFER -  03190000
*                                 AND IS TO BE PRINTED.  NO MSG CODE IS 03200000
*                                 ASSOCIATED WITH THIS MESSAGE, AND IT  03210000
*                            USUALLY WILL BE A SYNADAF MESSAGE.         03220000
RCODE    EQU   X'10'         ON = PUT RETURN CODE INTO THIS MSG TEXT    03230000
PBIT     EQU   X'08'         ON = USE PARAM LIST WITH THIS MSG TEXT     03240000
*              LO ORDER 3 BITS NOT USED - RESERVED                      03250000
         SPACE 2                                                        03260000
MSGPARAM DS    9H                 THIS FIELD IS TO CONTAIN PARAMETERS   03270000
*                                 TO BE PLACED IN MESSAGES              03280000
         ORG   MSGPARAM                                                 03290000
NAMEDISP DC    X'00'              THE DISPLACEMENT OF A NAME            03300000
*                                 PARAMETER FROM THE BEGINNING OF       03310000
*                                 THE MESSAGE IT IS TO BE PLACED IN     03320000
*                                 THIS BYTE                             03330000
NODISP   DC    X'00'              THE DISPLACEMENT OF A NUMBER          03340000
*                                 PARAMETER FROM THE BEGINNING OF       03350000
*                                 THE MESSAGE IT IS TO BE PLACED IN     03360000
*                                 THIS BYTE                             03370000
DDNMDISP DC    X'00'              DISPLACEMENT OF DDNAME FROM MSG START 03380000
         DS    0D                                                       03390000
NAMEFLD  DC    CL8' '        AREA CONTAINING NAME TO BE PUT INTO MSG    03400000
DDNMFLD  DC    CL8' '        AREA CONTAINING DDNAME TO INSERT IN MSG    03410000
DDVALNM  EQU   NAMEFLD       USED BY VALIDATE TO SAVE DD NAME           03420000
PARAMS   DC    X'00'                                                    03430000
NAME     EQU   X'80'         ON = THERE IS A NAME PARAMETER             03440000
NBR      EQU   X'40'         ON = THERE IS A NUMBER PARAMETER           03450000
DDNM     EQU   X'20'         ON = THERE IS A DDNAME PARAMETER           03460000
NOFLD    DC    CL7'0'        AREA CONTAINING NUMBER TO BE PUT INTO MSG  03470000
         TITLE 'MESSAGE CODE DEFINITIONS USED BY ALL ZZRELINK MODULES'  03480000
*********************************************************************** 03490000
*        THE FOLLOWING MESSAGE CODES ARE USED BY ZZRELINK, ZZRESCAN,  * 03500000
*    AND ZZMESAGE TO IDENTIFY AND PRODUCE ALL ZZRELINK MESSAGES.      * 03510000
*********************************************************************** 03520000
INALCNTR EQU     01                INVALID COMMAND OR KEYWORD           03530000
INVALSPR EQU     02                INVALID PARAMETER                    03540000
ONEQPARN EQU     03                UNEQUAL PARENTHESIS                  03550000
INVALCON EQU     04                INVALID CONTINUATION                 03560000
MEMNOSE  EQU     05                MEMBER WITHOUT SELECT/EXCLUDE        03570000
MULTSSEE EQU     06                ONLY ONE SELECT/EXCLUDE PER INDD     03580000
INVALREP EQU     07                INVALID REPLACE SPECIFIED            03590000
NULLPARM EQU     08                NULL PARAMETERS                      03600000
NORREN   EQU     09                CANNOT RENAME/REPLACE ON EXCLUDE     03610000
NOINDD   EQU     10                OUTDD OR INDD NOT SPECIFIED          03620000
INVALIST EQU   11                  OUTDD/LIST NOT ON LINK STATEMENT     03630000
ENDMESS  EQU   12                  END OF CONTROL CARDS                 03640000
MODEERR  EQU   13                  MIXING ZZRLINKAND ZZRDSCPY MODE      03650000
NOOVLYS  EQU   14                  WARNING - OVERLAYS NOT SUPPORTED     03660000
SCANMSG  EQU   15                  CONTROL STATEMENT ERROR              03670000
SEQERROR EQU   16                  STATEMENT SEQUENCE ERROR             03680000
GENERUAL EQU   17                  GENERAL VALIDATION MESSAGE           03690000
OPENERRX EQU   18                  OPEN ERROR MESSAGE                   03700000
OBTAINER EQU   19                  OBTAIN ERROR                         03710000
NOTPDSER EQU   20                  OBTAIN NOT PDS                       03720000
INVALREC EQU   21                  INVALID LRECL                        03730000
INVALBLK EQU   22                  INVALID BLOCKSIZE                    03740000
UNMOVEDS EQU   23                  DATA SET UNMOVABLE                   03750000
RECFMINC EQU   24                  RECFM INCOMPATIBLE                   03760000
NODIR    EQU   25                  UNABLE TO OPEN DIRECTORY DCB         03770000
DIRERR01 EQU   26                  ALIAS NAME X OCCURS MORE THAN ONCE   03780000
ATABFULL EQU   27                  ALIAS TABLE FULL                     03790000
EPAERR01 EQU   28                  UNABLE TO LOCATE EPA OF MODULE X     03800000
DIRERR02 EQU   29                  I/O ERROR READING DIRECTORY FOR X    03810000
REPERR01 EQU   30                  MEMBER X NOT RELINKED - NO REP OPT   03820000
M39      EQU   31                  CANNOT COMPRESS WITH SELECT OR       03830000
*                                  EXLCUDE                              03840000
CESDIOER EQU   32                  I/O ERROR READING CESD RECORDS       03850000
NE       EQU   33                  LOAD MODULE IS 'NOT EDITABLE'        03860000
*                                  BUFFERS FOR COMPRESS                 03870000
M42      EQU   34 CANNOT SPECIFY DUPLICATE NAME FOR SEL/EXCL/RENAME     03880000
M43      EQU   35                  CANNOT PROCESS ALL OLD/NEW NAMES     03890000
*                                  SPECIFIED                            03900000
M45      EQU   36                  (DATA SET NAME) REFERENCES A NULL    03910000
*                                  INPUT DATA SET                       03920000
M46      EQU   37                  CANNOT RE/DE BLOCK WITH              03930000
*                                  NOTE-LIST/USER TTRN IN MEMBER        03940000
*                                  (MEMBER NAME)                        03950000
M47      EQU   38                  CANNOT CONTINUE TO BUILD CTLTAB      03960000
M48      EQU   39                 ALL SELECTED MEMBERS COPIED - DID NOT 03970000
*                                 USE ALL SPECIFIED INDD'S              03980000
M49      EQU   40                  (NUMBER) UNUSED TRKS IN OUTPUT DATA  03990000
*                                  SET REFERENCED BY (DDNAME)           04000000
M50      EQU   41                  CANNOT COMPRESS TRACK OVERFLOW DATA  04010000
*                                  SET                                  04020000
M51      EQU   42                  CANNOT COMPRESS WITH RE/DE BLOCKING  04030000
M53      EQU   43                  END OF JOB (0,4,8) WAS HIGHEST       04040000
*                                  SEVERITY CODE                        04050000
NORMOD   EQU   44                  NO SPACE IN OUTPUT DIRECTORY FOR     04060000
*                                  DIRECTORY ENTRIES FROM DATA SET      04070000
*                                  (DATA SET NAME)                      04080000
UNUSDDB  EQU   45                  THERE ARE (NUMBER) UNUSED DIRECTORY  04090000
*                                  BLOCKS IN THE OUTPUT DIRECTORY       04100000
TMDBTR   EQU   46     **WARNING** THE OUTPUT DS REF BY XXXXXXXX  A36049 04110000
*                     CONTAINS TOO MANY DIRECTORY BLOCKS PER     A36049 04120000
*                     TRACK                                      A36049 04130000
M58      EQU   47                  ERROR FORCES JOB TO TERMINATE        04140000
M59      EQU   48                  (MEMBER NAME) COMPRESSED- WAS        04150000
*                                  ALREADY IN PLACE                     04160000
M60      EQU   49                  ALL MEMBERS COMPRESSED-              04170000
*                                  ALL WERE ORIGINALLY COMPRESSED       04180000
MEMCOP   EQU   50                  (MEMBERNAME) HAS BEEN SUCCESSFULLY   04190000
*                                  COPIED                               04200000
RNMEMCOP EQU   51                  (MEMBER NAME) HAS BEEN RENAMED AND   04210000
*                                  SUCCESSFULLY COPIED                  04220000
NOTDA    EQU   52                 DATA SET NOT DIRECT ACCESS            04230000
NODDCARD EQU   53                 DD CARD NOT FOUND                     04240000
UNITER01 EQU   54                 UNIT TYPE DEFINED BY X NOT SUPPORTED  04250000
NOMBCPDM EQU   55                 NO MBRS COPIED FROM INPUT DATASET RE- 04260000
*                                 FERENCED BY (XXXXXXXX)                04270000
CONCATBD EQU   56                 CONCATENATED DATA SETS                04280000
IMPCOMPR EQU   57                 IMPLIED COMPRESS                      04290000
NOCMPOSS EQU   58                 CANNOT COMPRESS                       04300000
NOLINK   EQU   59                 NO MEMBERS FOR PARTIAL LINK,          04310000
*                                 WILL NOT LINK                         04320000
DOFULLCP  EQU  60                 TOTAL LINK ASSUMED                    04330000
MFBNC    EQU   61 MEMBER FOUND BUT NOT COPIED - I/O ERROR READING       04340000
*                 INPUT DIRECTORY                                       04350000
NONELINK EQU   62 NO MEMBERS COPIED                                     04360000
FOLLMCPD EQU   63 FOLLOWING MBRS COPIED FROM INPUT DS REF BY XXXXXXXX   04370000
PLAMPID  EQU   64 POSSIBLE LOSS OF ACCESS TO MEMBER AND/OR INCOMPLETE   04380000
*                 DIRECTORY                                             04390000
WODINC   EQU   65 SYSUT4 I/O ERROR - OUTPUT DIRECTORY MAY BE INCOMPLETE 04400000
WONTCOM  EQU   66 I/O ERROR ON SYSUT3 - COMPRESS IN PLACE NOT DONE      04410000
BADPRINT EQU   67 SYSPRINT COULD NOT BE OPENED                          04420000
SMNF    EQU   68                  (MBRNAME) WAS SELECTED BUT NOT FOUND  04430000
PA       EQU   69                  LOAD MODULE IS PAGE ALIGNED        $ 04440000
         SPACE 2                                                        04450000
RCBUF    DC    C'0'               COMPLETION-CODE AREA...CONTAINS CHAR- 04460000
*                                 ACTER REPRESENTATION OF HIGHEST       04470000
*                                 COMPLETION CODE SET BY UTILITY PGM    04480000
LINECT   DC   X'0'                COUNT OF NBR LINES WRITTEN ON ONE PG  04490000
PGLIMIT  EQU   56                 MAX NBR OF LINES TO BE PUT ON ONE PG  04500000
         SPACE 2                                                        04510000
MSGBUF   DC    121C' '            MESSAGE BUFFER                        04520000
PGNO     DC    C'0001'            STARTING PAGE NUMBER FOR MSG OUTPUT   04530000
*************              END OF COMMUNICATION AREA   **************** 04540000
***   KEEP CARD  'MCAEND' LAST IN COMMUNICATION AREA JUST BEFORE THE ** 04550000
****  EQUATE THAT DETERMINES THE COMMUNICATIONS AREA SIZE 'MCASIZE'  ** 04560000
         SPACE 1                                                        04570000
MCAEND   DS    0D        END OF COMMUNICATION AREA                      04580000
MCASIZE  EQU   MCAEND-ZZRELCOM    SIZE OF COMMUNICATIONS AREA FOR SNAPS 04590000
         SPACE 1                                                        12510000
         END                                                            12520000
//STEP3    EXEC ASMSUBRS,MEMBER=ZZROVBLD                                00330000
//ASM.SYSIN DD *                                                        00340000
         MACRO                                                          00000010
&TAG     EQUREGS                                                        00000020
         GBLB  &SW                                                      00000030
         LCLA  &N                                                       00000040
         LCLC  &R                                                       00000050
         AIF   (&SW).EXIT                                               00000060
&SW      SETB  1                                                        00000070
&R       SETC  'R'                                                      00000080
&N       SETA  0                                                        00000090
.LOOP    ANOP                                                           00000100
&R.&N    EQU   &N                                                       00000110
&N       SETA  &N+1                                                     00000120
         AIF   (&N LT 16).LOOP                                          00000130
.EXIT    MEXIT                                                          00000140
         MEND                                                           00000150
         MACRO                                                          00001000
&NAME    CHAIN &BASE=10,&SAVE=(14,12),&SAVAR=SAVE,&PL1=NO,&SAVEPTR=13, X00002000
               &RENT=,&MACBASE=15,&MODULE=*,&DSALTH=100,&WKREG=14,     X00003000
               &PLICO=NO,&PARM=,&ENTRY=                                 00004000
         LCLC  &CHARFLD,&INTNAME                                        00005000
         LCLA  &COUNTER,&ROUND,&BASENO,&BDISP                           00006000
         LCLB  &SAVE1                                                   00007000
&INTNAME SETC  '&NAME'                                                  00008000
         AIF    ('&MACBASE' NE '15').NOTB15                             00009000
         AIF   ('&MODULE' EQ '').NOTB15                                 00010000
         AIF   ('&PLICO' EQ 'YES' AND '&MODULE' NE '*').NOTB15          00011000
         USING  *,15 .ESTABLISH TEMPORARY BASE REG                      00012000
.NOTB15  ANOP                                                           00013000
         AIF   ('&MODULE' EQ '').SCHECK                                 00014000
         AIF   ('&MODULE' EQ '*').CSNAME                                00015000
         AIF   ('&ENTRY' EQ '').NENT01                                  00016000
&CHARFLD SETC  '&ENTRY'                                                 00017000
         AGO   .ENT01                                                   00018000
.NENT01  ANOP                                                           00019000
&CHARFLD SETC  '&MODULE'                                                00020000
.ENT01   ANOP                                                           00021000
&COUNTER SETA  1                                                        00022000
.CHARCNT AIF   ('&CHARFLD'(1,&COUNTER) EQ '&CHARFLD').CNTEND            00023000
&COUNTER SETA &COUNTER+1                                                00024000
         AGO   .CHARCNT                                                 00025000
.CNTEND  ANOP                                                           00026000
&ROUND   SETA  ((&COUNTER+2)/2)*2+4                                     00027000
         AIF   ('&PLICO' NE 'YES').NPLI01                               00028000
         AIF   ('&MODULE' EQ '*').NPLI01                                00029000
&ROUND   SETA  7-&COUNTER                                               00030000
         AIF   (&ROUND LE 0).PLI01                                      00031000
         DC    CL&ROUND.' ' PAD MODULE NAME ON LEFT WITH BLANKS         00032000
.PLI01   ANOP                                                           00033000
         AIF   ('&ENTRY' EQ '').NENT02                                  00034000
         DC    C'&ENTRY' MODULE NAME FOR CORE DUMP IDENTIFICATION       00035000
         DC    AL1(&COUNTER.) LENGTH OF ENTRY NAME                      00036000
         ENTRY &ENTRY                                                   00037000
         USING *,15                                                     00038000
&ENTRY   DS    0H                                                       00039000
         AGO   .SCHECK                                                  00040000
.NENT02  ANOP                                                           00041000
         DC    C'&MODULE' MODULE NAME FOR CORE DUMP IDENTIFICATION      00042000
         DC    AL1(&COUNTER.) LENGTH OF MODULE NAME                     00043000
         ENTRY &MODULE                                                  00044000
         USING *,15                                                     00045000
&MODULE  DS    0H                                                       00046000
         AGO   .SCHECK                                                  00047000
.NPLI01  ANOP                                                           00048000
&INTNAME B     *+&ROUND .BRANCH AROUND ID                               00049000
         DC    AL1(&COUNTER) .     ID LENGTH                            00050000
         DC    CL&COUNTER'&CHARFLD' .ID                                 00051000
&INTNAME SETC  ''                  YES                                  00052000
         AGO   .SCHECK                                                  00053000
.CSNAME  AIF   ('&SYSECT' EQ '').E1                                     00054000
&CHARFLD SETC  '&SYSECT'                                                00055000
&COUNTER SETA  1                                                        00056000
         AGO   .CHARCNT                                                 00057000
.*       NOW CHECK FOR REG'S TO BE SAVED                                00058000
.*                                                                      00059000
.*       IF CHECKOUT/OPTIMIZER PROLOGUE HAS BEEN REQUESTED THEN A       00060000
.*   STANDARD REGISTER SAVING CONVENTION MUST BE FOLLOWED.              00061000
.*                                                                      00062000
.SCHECK  AIF   ('&PLICO' NE 'YES').SCHECK2                              00063000
         STM   14,11,12(13) SAVE CALLER'S REGISTERS IN CALLER'S DSA     00064000
&SAVE1   SETB  1           SHOW THAT R1 WAS SAVED                       00065000
         AGO   .SCHAIN                                                  00066000
.SCHECK2 AIF   ('&SAVE' EQ '').SCHAIN   REGS TO BE SAVED?               00067000
.SNAMEOK AIF   (T'&SAVE(1) NE 'N').E2   REGS MUST BE SELF-DEFINING      00068000
&COUNTER SETA  &SAVE(1)*4+20                                            00069000
         AIF   (&COUNTER LE 75).SCHK01                                  00070000
&COUNTER SETA  &COUNTER-64                                              00071000
.SCHK01  AIF   (N'&SAVE NE 2).SCHK02                                    00072000
&INTNAME STM   &SAVE(1),&SAVE(2),&COUNTER.(&SAVEPTR) .SAVE REQ'D REG'S  00073000
&SAVE1   SETB  (&SAVE(1) LE 1 AND &SAVE(2) GE 1)                        00074000
&SAVE1   SETB  (&SAVE(1) GT 1 AND &SAVE(2) GE 1)                        00075000
&INTNAME SETC  ''                                                       00076000
         AGO   .SCHAIN                                                  00077000
.SCHK02  AIF   (N'&SAVE NE 1).E3                                        00078000
&INTNAME ST    &SAVE(1),&COUNTER.(&SAVEPTR,0) .SAVE REQ'D REGISTER      00079000
&SAVE1   SETB  (&SAVE(1) EQ 1) SHOW THAT R1 WAS SAVED                   00080000
&INTNAME SETC  ''                                                       00081000
.*                                                                      00082000
.*       NOW WE CHAIN THE SAVE AREAS                                    00083000
.*                                                                      00084000
.SCHAIN  ANOP                                                           00085000
         AIF   ('&ENTRY' EQ '').NENT03                                  00086000
         AIF   ('&MODULE' EQ '').E8                                     00087000
&INTNAME L     &BASE(1),=A(&MODULE.) SET UP BASE ENTRY ADDRESS          00088000
         AGO   .ENT03                                                   00089000
.NENT03  ANOP                                                           00090000
&INTNAME LR    &BASE(1),15 .        ESTABLISH ADDRESSIBILITY            00091000
         AIF   ('&PLICO' NE 'YES').NPLI02                               00092000
         AIF   ('&MODULE' EQ '' OR '&MODULE' EQ '*').NPLI02             00093000
.ENT03   ANOP                                                           00094000
         USING &MODULE,&BASE(1)                                         00095000
         AGO   .PLI02                                                   00096000
.NPLI02  ANOP                                                           00097000
         USING &SYSECT,&BASE(1)                                         00098000
.PLI02   ANOP                                                           00099000
         SPACE 1                                                        00100000
         AIF   (N'&BASE LE 1).BASETST                                   00101000
&BASENO  SETA  2                                                        00102000
&BDISP   SETA  4096                                                     00103000
.BASELP1 ANOP                                                           00104000
         LA    &BASE(&BASENO),4095(&BASE(&BASENO-1))                    00105000
         LA    &BASE(&BASENO),1(&BASE(&BASENO))                         00106000
         AIF   ('&PLICO' NE 'YES').NPLI03                               00107000
         AIF   ('&MODULE' EQ '' OR '&MODULE' EQ '*').NPLI03             00108000
         USING &MODULE+&BDISP,&BASE(&BASENO)                            00109000
         AGO   .PLI03                                                   00110000
.NPLI03  ANOP                                                           00111000
         USING &SYSECT+&BDISP,&BASE(&BASENO)                            00112000
.PLI03   ANOP                                                           00113000
         SPACE 1                                                        00114000
&BASENO  SETA  &BASENO+1                                                00115000
&BDISP   SETA  &BDISP+4096                                              00116000
         AIF   (&BASENO LE N'&BASE).BASELP1                             00117000
.BASETST AIF   ('&MACBASE' NE '15').NOTB15F                             00118000
         AIF   ('&MODULE' EQ '').NOTB15F                                00119000
         DROP  15 .DROP TEMPORARY BASE                                  00120000
.NOTB15F ANOP                                                           00121000
&INTNAME SETC  ''                                                       00122000
         AIF   ('&PL1' EQ 'YES').PL1GEN  PL/1 (F) PROLOGUE REQUIRED     00123000
.*                                                                      00124000
         AIF   ('&PLICO' EQ 'YES').PLIGEN CHECKER/OPTIMIZER PROLOGUE    00125000
.*                                                                      00126000
*        NOW CHAIN THE SAVE AREAS                                       00127000
         SPACE 1                                                        00128000
         AIF   ('&RENT' EQ '').SCHAIN1                                  00129000
         AIF   ('&RENT' NE 'YES').E6    'YES' IS THE ONLY VALID PARM    00130000
         SPACE 1                                                        00131000
*        OBTAIN A DYNAMIC SAVE AREA TO RETAIN RE-ENTRABILITY            00132000
         SPACE 1                                                        00133000
         GETMAIN    R,LV=&DSALTH . GET A DYNAMIC SAVE AREA (DSA)        00134000
         SPACE 1                                                        00135000
         XC    0(&DSALTH,1),0(1) . CLEAR DSA TO ZEROES                  00136000
         SPACE 1                                                        00137000
         ST    &SAVEPTR,4(1) .     SAVE PTR TO OLD SAVE AREA IN NEW     00138000
         ST    1,8(0,&SAVEPTR) .   STORE PTR TO NEW SAVE AREA IN OLD    00139000
         LR    &SAVEPTR,1 .        SET POINTER TO NEW SAVE AREA         00140000
         SPACE 1                                                        00141000
         AGO   .EXIT                                                    00142000
.SCHAIN1 ANOP                                                           00143000
         ST    &SAVEPTR,&SAVAR+4 . SAVE PTR TO SAVE AREA                00144000
         LA    &WKREG,&SAVAR .     PICK UP PTR TO NEW SAVE AREA         00145000
         ST    &WKREG,8(0,&SAVEPTR) .STORE PTR TO NEW AREA IN OLD AREA  00146000
         LR    &SAVEPTR,&WKREG .   SET SAVE PTR TO NEW SAVE AREA        00147000
         SPACE 1                                                        00148000
.*                                                                      00149000
.*       IF PL1=YES HAS BEEN CODED WE MUST GENERATE THE APPROPRIATE     00150000
.*   PL/1 PROLOGUE FOR ERROR HANDLING                                   00151000
.*                                                                      00152000
.PL1GEN  ANOP                                                           00153000
         AIF   ('&PL1' EQ 'NO').EXIT                                    00154000
         AIF   ('&PL1' NE 'YES').E4                                     00155000
         AIF   ('&SYSECT' EQ '').E5                                     00156000
&CHARFLD SETC  '&SYSECT' .    COUNT NUMBER OF CHARACTERS IN CSECT NAME  00157000
&COUNTER SETA  1                                                        00158000
.CSCOUNT AIF   ('&CHARFLD'(1,&COUNTER) EQ '&CHARFLD').CSCHECK           00159000
&COUNTER SETA  &COUNTER+1                                               00160000
         AGO   .CSCOUNT                                                 00161000
.CSCHECK AIF   (&COUNTER GT 7).E7  CSECT NAME > 7 CHARACTERS            00162000
&INTNAME L     15,MLMA&SYSNDX .    SET UP DSA AND CHAIN IT TO OTHERS    00163000
         LA    0,&DSALTH .         LOAD UP LENGTH OF REQ'D DSA          00164000
         BALR  14,15 .             LINK TO IHESADA - PL/1 STORAGE MGT.  00165000
&CHARFLD.B DXD A              SYMBOLIC OFFSET OF PSEUDO REGISTER        00166000
         MVI   0(13),X'80' .       SET DSA FLAG                         00167000
         ST    13,0(12) .          STORE DSA ADDRESS IN PSEUDO REGISTER 00168000
         ORG   *-2                                                      00169000
         DC    QL2(&SYSECT.B) .PLACES PR OFFSET IN ST INSTRUCTION ABOVE 00170000
         CNOP  0,4 .         ALIGN TO FULL WORD BOUNDARY                00171000
         B     *+8 .         BRANCH OVER ADCON                          00172000
MLMA&SYSNDX DC   V(IHESADA) .     ADCON POINTING TO GET DSA ROUTINE     00173000
&INTNAME SETC  ''                                                       00174000
         AGO   .EXIT                                                    00175000
.*                                                                      00176000
.*        THE FOLLOWING CODE WILL GENERATE A PL/1 CHECKOUT/OPTIMIZER    00177000
.*   COMPATABLE PROLOGUE, UTILIZING THE LIFO STACK TO OBTAIN DSA CORE   00178000
.*                                                                      00179000
.PLIGEN  ANOP                                                           00180000
         LA    0,&DSALTH .  R0 = LENGTH OF REQUIRED DSA                 00181000
         L     1,76(13) .   R1 = ADDRESS OF FIRST AVAILABLE BYTE IN     00182000
*                                THE LIFO STACK                         00183000
         ALR   0,1 .        R0 = PTR TO END OF REQUIRED DSA IF IT       00184000
*                                FIT IN LIFO STORAGE                    00185000
         CL    0,12(12) .   COMPARE RESULT WITH ADDRESS OF LAST         00186000
*                           AVAILABLE BYTE IN LIFO STORAGE              00187000
         BNH   *+10 .       BRANCH IF REQUIRED DSA STORAGE IS AVAILABLE 00188000
*                           IN THE LIFO STACK                           00189000
         L     15,116(12) . R15 = PTR TO PL/I STORAGE OVERFLOW ROUTINE  00190000
         BALR  14,15 .      BRANCH OUT TO GETMAIN MORE STORAGE          00191000
         ST    0,76(1) .    UPDATE ADDRESS OF NEXT AVAILABLE BYTE IN    00192000
*                           LIFO STACK                                  00193000
         ST    13,4(1) .    STORE PTR TO CALLER'S DSA IN OUR DSA        00194000
         MVC   72(4,1),72(13) COPY PTR TO LIBRARY WORKSPACE             00195000
         LR    13,1 .       SET UP PTR TO OUR NEW DSA                   00196000
         MVI   0(13),X'80' .SET DSA FLAGS TO PRESERVE PL/I              00197000
         MVI   86(13),X'91' ERROR-HANDLING WHILE EXECUTING              00198000
         MVI   87(13),X'C0' IN THIS ASSEMBLER ROUTINE.                  00199000
*                                                                       00200000
*        E N D    O F    P L / I    P R O L O G U E                     00201000
*                                                                       00202000
.*                                                                      00203000
.EXIT    ANOP                      NORMAL MACRO EXIT                    00204000
         AIF   (NOT &SAVE1).EXIT2    BRANCH IF R1 WAS NOT SAVED         00205000
         AIF   ('&PARM' NE 'YES').EXIT2 BRANCH IF NOT 'PARM=YES'        00206000
         L     1,4(13) .           R1 = PTR TO CALLERS' SAVE AREA       00207000
         L     1,24(1) .           RESTORE R1 TO POINT TO PARMS         00208000
.*                                                                      00209000
.EXIT2   MEXIT                                                          00210000
.*                                                                      00211000
.*       ERROR MESSAGES                                                 00212000
.*                                                                      00213000
.E1      MNOTE 12,'NO CSECT NAME SPECIFIED AND ''*'' CODED'             00214000
         MEXIT                                                          00215000
.E2      MNOTE 12,'INVALID REGISTER SPECIFICATION IN SAVE RANGE'        00216000
         MEXIT                                                          00217000
.E3      MNOTE 12,'INVALID RANGE OF REGISTERS TO BE SAVED'              00218000
         MEXIT                                                          00219000
.E4      MNOTE 12,'INVALID ''PL1='' SPECIFICATION'                      00220000
         MEXIT                                                          00221000
.E5      MNOTE 12,'PL1=YES CODED BUT NO CSECT NAME PRESENT'             00222000
         MEXIT                                                          00223000
.E6      MNOTE 12,'INVALID ''RENT='' SPECIFICATION'                     00224000
         MEXIT                                                          00225000
.E7      MNOTE 12,'PL1=YES CODED BUT CSECT NAME > 7 CHARACTERS'         00226000
         MEXIT                                                          00227000
.E8      MNOTE 12,'''ENTRY='' OPERAND CODED BUT ''MODULE='' NOT SPECIFIX00228000
               ED'                                                      00229000
         MEXIT                                                          00230000
.*                                                                      00231000
         MEND                                                           00232000
         TITLE 'ROUTINE TO BUILD OVERLAY TABLES FOR THE CARD GENERATOR' 00010000
ZZROVBLD CSECT                                                          00020000
         EQUREGS                                                        00030000
         EJECT                                                          00040000
         CHAIN BASE=12                                                  00050000
         USING ZZRELCOM,R4                                              00060000
         EJECT                                                          00070000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00080000
*                                                                     * 00090000
*    FUNCTION TABLE FORMAT    R1 HAS CODE FOR FUNCTION REQUIRED.      * 00100000
*                                                                     * 00110000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00120000
         B     BLDRTNS(R1)                                              00130000
BLDRTNS  DS    0H                                                       00140000
         B     CLSOPN                   CODE = 0                        00150000
         B     BLDTAB                   CODE = 4                        00160000
         EJECT                                                          00170000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00180000
*                                                                     * 00190000
*    FUNCTION - CLOSE AND OPEN THE FILE                               * 00200000
*                                                                     * 00210000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00220000
CLSOPN   DS    0H                                                       00230000
         MVI   RC,0                     SET RETURN CODE TO ZERO         00240000
         CLOSE (ESDCB)                  CLOSE PREVIOUS INPUT D.S.       00250000
         L     R3,INBEGIN               START OF TABLE                  00260000
         MVC   ESDCB+40(8),2(R3)        MAIN DD NAME                    00270000
         MVC   ESDCB+33(3),AEODAE+1     END OF CESD EXIT                00280000
         OPEN  (ESDCB)                  OPEN THE FILE                   00290000
         TM    ESDCB+48,X'10'           OPEN SUCESSFUL?                 00300000
         BO    NORMRET                  OPEN O.K. - RETURN              00310000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00320000
*                                                                     * 00330000
*    OPEN IS UNSUCESSFUL PUT OUT MESSAGE                              * 00340000
*                                                                     * 00350000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00360000
         MVI   RC,12                                                    00370000
         MVC   DDNMFLD(8),2(R3)         MOVE IN DDNAME FOR MESSAGE      00380000
         MVI   DDNMDISP,0               DDNAME GOES AT +0               00390000
         MVI   MSGLIST+1,NODDCARD       SET UP DD CARD MISSING MESSAGE  00400000
         MVI   MSGLIST,PBIT+LASTMSG     SHOW PARM PRES & LAST MSG       00410000
         MVI   PARAMS,DDNM              SHOW DDNAME PRESENT             00420000
         LA    R2,NORMRET               R2 HAS SUPPLIED RETURN POINT    00430000
MSGWRT   DS    0H                                                       00440000
         L     R15,VZZRLMES             PTR TO MESSAGE WRITER           00450000
         BALR  R14,R15                  GO WRITE MESSAGE                00460000
         BR    R2                       EXIT TO SUPPLIED RETURN POINT   00470000
MSGWRTN  DS    0H                                                       00480000
         MVI   MSGLIST,PBIT+LASTMSG     SHOW LAST MSG AND PARMS         00490000
         MVI   PARAMS,NAME              SHOW NAME PARM PRESENT          00500000
         B     MSGWRT                   WRITE MESSAGE                   00510000
         EJECT                                                          00520000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00530000
*                                                                     * 00540000
*    FUNCTION - BUILD NECESSARY OVERLAY TABLES                        * 00550000
*                                                                     * 00560000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00570000
BLDTAB   DS    0H                                                       00580000
         MVI   RC,0                     SET RETURN CODE TO ZERO         00590000
         MVI   CNMEFND,X'00'            SET FOUND SWITCH OFF            00600000
         MVI   CESDSW,X'00'             SET CESD FOUND SWITCH OFF     $ 00610000
         ZAP   CSECTCTR,=P'0'           CLEAR CSECT COUNTER TO 0      $ 00620000
         MVI   ENDTABSW,X'00'           TURN ENDTAB SW OFF              00630000
         MVI   CSCTNAME,C' '            INIT FIRST CHAR TO BLANK        00640000
         MVC   MEMNAME(8),WKA1          GET MEMBER NAME                 00650000
         MVC   OVLYSW(1),WKA1+8         GET OVERLAY SWITCH              00660000
         MVC   EPADDR(3),WKA1+9         GET EPA ADDRESS                 00670000
         MVC   ADDRMAIN(4),WKA1+12      GET MAIN MEMBER ADDRESS         00680000
         LA    R6,CESDADR               R6 -> START OF CESD TABLE       00690000
         FIND  ESDCB,ADDRMAIN,C         ESTABLISH BEGINNING OF MEMBER   00700000
         EJECT                                                          00710000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00720000
*                                                                     * 00730000
*    SET UP TO GET NEXT RECORD                                        * 00740000
*                                                                     * 00750000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00760000
CESDGET  DS    0H                                                       00770000
         LA    R2,CESDBFFR              PTR TO READ BUFFER              00780000
         LA    R3,256                   LENGTH OF READ                  00790000
         BAL   R9,CESDREAD              READ A RECORD                   00800000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00810000
*                                                                     * 00820000
*    DO WE WANT TO PROCESS THIS RECORD                                * 00830000
*                                                                     * 00840000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00850000
         CLI   0(R2),X'80'              IDR?                            00860000
         BE    CESDGET                  YES - GET NEXT BLOCK            00870000
         CLI   0(R2),X'40'              SYM?                            00880000
         BE    CESDGET                  YES - GET NEXT BLOCK            00890000
         CLI   0(R2),X'20'              CESD?                           00900000
         BNE   *+12                     NO                            $ 00910000
         MVI   CESDSW,X'FF'             INDICATE AT LEAST ONE         $ 00920000
         B     CESDFND                  CESD FOUND AND PROCESS        $ 00930000
         TM    OVLYSW,X'FF'             OVERLAY PROCESSING?             00940000
         BZ    CESDER1                  NO - CSECT NOT FOUND - ERROR    00950000
         CLI   0(R2),X'01'              CONTROL RECORD?                 00960000
         BE    CESDCMPL                 YES - SEGTAB FOLLOWS            00970000
         B     CESDER1                  CSECT ERROR                     00980000
         EJECT                                                          00990000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01000000
*                                                                     * 01010000
*    SET UP TO PROCESS BLOCK                                          * 01020000
*                                                                     * 01030000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01040000
CESDFND  DS    0H                                                       01050000
         LH    R7,6(R2)                 LENGTH OF DATA IN BLOCK         01060000
         SRL   R7,4                     / 16 = # OF ENTRIES             01070000
         LA    R5,8(R2)                 BUMP PAST BLOCK HEADER          01080000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01090000
*                                                                     * 01100000
*    CHECK CESD DATA FOR WHAT WE WANT                                 * 01110000
*                                                                     * 01120000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01130000
CESDCHCK DS    0H                                                       01140000
         CLI   8(R5),X'07'              IS THIS A NULL RECORD         $ 01150000
         BE    CESDINCR                 YES - SKIP                    $ 01160000
         CLI   12(R5),X'00'             SEG # = ZERO?                   01170000
         BNE   *+14                     NO - CONTINUE                 $ 01180000
         CLC   13(3,R5),=X'000000'      YES - TREAT AS WR, NULL,      $ 01190000
         BE    CESDINCR                   OR ER TYPE IF LEN. = ZERO   $ 01200000
         CLI   8(R5),X'14'              SEGTAB OR ENDTAB?               01210000
         BNE   CHCKPVT                  NO - CONTINUE CHECKS            01220000
         TM    ENDTABSW,X'FF'           FIRST PC ITEM?                  01230000
         BO    ENDTAB                   MUST BE ENDTAB                  01240000
         MVI   ENDTABSW,X'FF'           INDICATE SEGTAB FOUND           01250000
         MVC   0(8,R5),=C'$SEGTAB '     INDICATE SEGTAB                 01260000
         B     UPDTCTAB                 MOVE TO BUFFER                  01270000
ENDTAB   DS    0H                                                       01280000
         MVC   0(8,R5),=C'$ENDTAB '     INDICATE ENDTAB                 01290000
         B     UPDTCTAB                 MOVE TO BUFFER                  01300000
CHCKPVT  DS    0H                                                       01310000
         CLI   8(R5),X'04'              PRIVATE CODE TYPE?              01320000
         BNE   CONTCHK1                 CONTINUE CHECKS                 01330000
         MVC   0(8,R5),=C'$PRIVATE'     INDICATE PRIVATE CODE           01340000
         B     UPDTCTAB                 MOVE TO BUFFER                  01350000
CONTCHK1 DS    0H                                                       01360000
         NI    8(R5),X'0F'              TURN OFF UNWANTED BITS          01370000
         CLI   8(R5),X'00'              SD TYPE?                        01380000
         BE    CNMEPRCS                 YES - CHECK FOR CSECT NAME      01390000
         CLI   8(R5),X'03'              LR TYPE?                        01400000
         BE    CNMEPRCS                 YES - CHECK FOR CSECT NAME      01410000
         CLI   8(R5),X'05'              COMMON TYPE?                    01420000
         BNE   CESDINCR                 NO - UNACCEPTABLE TYPE          01430000
         CLI   0(R5),X'40'              BLANK COMMON?                   01440000
         BNE   UPDTCTAB                 NO - MOVE TO BUFFER             01450000
         MVC   0(8,R5),=C'$BLKCOM '     INDICATE BLANK COMMON           01460000
         B     UPDTCTAB                 MOVE TO BUFFER                  01470000
         EJECT                                                          01480000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01490000
*                                                                     * 01500000
*    SET BUFFER TO LOOK AT NEXT ENTRY                                 * 01510000
*                                                                     * 01520000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01530000
CESDINCR DS    0H                                                       01540000
         LA    R5,16(R5)                INCR TO NEXT ENTRY              01550000
         BCT   R7,CESDCHCK              CHECK NEXT ENTRY                01560000
         B     CESDGET                  GET NEXT RECORD WHEN NO ENTRY   01570000
         EJECT                                                          01580000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01590000
*                                                                     * 01600000
*    THIS ROUTINE ACTUALLY DOES THE READ                              * 01610000
*                                                                     * 01620000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01630000
CESDREAD DS    0H                                                       01640000
         READ  CESDECB,SF,ESDCB,(R2),(R3)    DO THE READ                01650000
         CHECK CESDECB                  WAIT FOR COMPLETION OF READ     01660000
         BR    R9                       RETURN                          01670000
         EJECT                                                          01680000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01690000
*                                                                     * 01700000
*    THIS PHASE READS IN THE SEGTAB AND PLACES IT IN SEGTAB BUFFER    * 01710000
*                                                                     * 01720000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01730000
CESDCMPL DS    0H                                                       01740000
         ST    R6,CESDUSED              PTR TO DYNAMIC END OF TABLE     01750000
         LH    R3,14(R2)                GET LENGTH FROM CCW             01760000
         LA    R2,SEGBFBEG              PTR TO START OF SEGTAB BUFFER   01770000
         LR    R8,R2                    R8 -> SEGTAB                    01780000
         AR    R8,R3                    LENGTH OF SEGTAB                01790000
         BAL   R9,CESDREAD              READ THE SEGTAB                 01800000
         ST    R8,SEGUSED               DYNAMIC END OF SEGTAB           01810000
         B     OVLYRET                  RETURN FROM OVERLAY ROUTINE     01820000
         EJECT                                                          01830000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01840000
*                                                                     * 01850000
*    MOVE CESD ENTRY TO TABLE                                         * 01860000
*                                                                     * 01870000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01880000
UPDTCTAB DS    0H                                                       01890000
         TM    OVLYSW,X'FF'             OVERLAY MODULE?                 01900000
         BZ    CESDINCR                 NO - DO NOT SET UP TABLE        01910000
         MVC   0(16,R6),0(R5)           MOVE ENTRY TO BUFFER            01920000
         MVC   10(2,R6),=H'0'           ZERO OUT ADDRESS                01930000
         AH    R6,=H'16'                UPDATE BUFFER POINTER           01940000
         B     CESDINCR                                                 01950000
         EJECT                                                          01960000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01970000
*                                                                     * 01980000
*    RETRIEVE CSECT NAME OF MAIN ENTRY POINT                          * 01990000
*                                                                     * 02000000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 02010000
CNMEPRCS DS    0H                                                       02020000
         AP    CSECTCTR,=P'1'           ADD 1 TO THE COUNT OF CEST    $ 02030000
*                                       NAMES FOUND IN CESD FOR MOD   $ 02040000
         TM    CNMEFND,X'FF'            NAME FOUND ALREADY?             02050000
         BO    CNMERET                  YES - RETURN                    02060000
         CLC   9(3,R5),EPADDR           ADDRESSES MATCH?                02070000
         BNE   CNMERET                  NO - NOT THE NAME               02080000
         MVC   CSCTNAME(8),0(R5)        SAVE THE CSECT NAME             02090000
         MVI   CNMEFND,X'FF'            SHOW FOUND                      02100000
         TM    OVLYSW,X'FF'             OVERLAY MODULE?                 02110000
         BZ    CSCTRET                  NO - DO NOT SET UP TABLE        02120000
CNMERET  DS    0H                                                       02130000
         CLI   8(R5),X'03'              LR TYPE?                        02140000
         BE    CESDINCR                 YES - PROCESS NEXT ENTRY        02150000
         B     UPDTCTAB                 SD TYPE - MOVE TO BUFFER        02160000
         EJECT                                                          02170000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 02180000
*                                                                     * 02190000
*    RETURN FROM OVERLAY BUILD ROUTINE. SET UP VALUES IN COMM AREA.   * 02200000
*                                                                     * 02210000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 02220000
OVLYRET  DS    0H                                                       02230000
         LA    R9,SEGBFBEG              R9 -> START OF SEGTAB           02240000
         ST    R9,WKA1+8                STORE THE ADDRESS               02250000
         MVC   WKA1+12(4),SEGUSED       MOVE END OF SEGTAB              02260000
         LA    R9,CESDADR               R9 -> START OF CESD TABLE       02270000
         ST    R9,WKA1+16               STORE THE ADDRESS               02280000
         MVC   WKA1+20(4),CESDUSED      MOVE END OF CESD TABLE          02290000
CSCTRET  DS    0H                                                       02300000
         MVC   WKA1(8),CSCTNAME         MOVE CSECT NAME                 02310000
         EJECT                                                          02320000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 02330000
*                                                                     * 02340000
*    RETURN TO CALLING ROUTINE                                        * 02350000
*                                                                     * 02360000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 02370000
NORMRET  DS    0H                                                       02380000
         L     R13,4(R13)               RESTORE SAVE AREA POINTER       02390000
         XR    R15,R15                  CLEAR R15                       02400000
         IC    R15,RC                   SET RETURN CODE                 02410000
         RETURN (14,12),RC=(15)         RETURN                          02420000
         EJECT                                                          02430000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 02440000
*                                                                     * 02450000
*    EOF EXIT WHEN READING MEMBER LOAD MODULE. NO VALID ENTRY POINT.  * 02460000
*                                                                     * 02470000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 02480000
CESDER1  DS    0H                                                       02490000
         CLI   CESDSW,X'FF'             HAVE AT LEAST 1 CESD ENTRY    $ 02500000
         BNE   NOEPERR                  NO - BYPASS CHECKS FOR MOD    $ 02510000
         CLC   EPADDR,=X'000000'        IF ENTRY POINT ADDRESS IS 0   $ 02520000
         BNE   *+14                     BUT NO VALID EP NAME FOUND    $ 02530000
         MVC   WKA1(8),=C'$PRIVATE'     PASS BACK $PRIVATE AS A SW    $ 02540000
         B     NORMRET                  TO BYPASS USE OF ENTRY CARD   $ 02550000
         CP    CSECTCTR,=P'1'           IF THE CESD HAS ONLY 1 CEST,  $ 02560000
         BNE   NOEPERR                  THERE IS NO EP MATCH, AND     $ 02570000
         MVC   WKA1(8),=C'$PRIVATE'     THERE IS A NON-ZERO EP, WE    $ 02580000
         B     NORMRET                  WILL ASUME AN RPG MODULE AND  $ 02590000
*                                       PASS BACK $PRIVATE AS A SW    $ 02600000
*                                       TO BYPASS USE OF ENTRY CARD   $ 02610000
NOEPERR  EQU   *                                                      $ 02620000
         MVC   NAMEFLD(8),MEMNAME       SET MEMBER NAME FOR MSG         02630000
         MVI   NAMEDISP,28              MEMBER NAME GOES AT +28         02640000
         MVI   MSGLIST+1,EPAERR01       SET FOR EPA ERROR MSG           02650000
         MVI   RC,4                     SET RETURN CODE TO 4            02660000
         LA    R2,NORMRET               SET RETURN ADDRESS              02670000
         B     MSGWRTN                  WRITE MSG WITH NAME             02680000
         EJECT                                                          02690000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 02700000
*                                                                     * 02710000
*    I/O ERROR HANDLING ROUTINE SYNAD EXIT FOR ESDCB                  * 02720000
*                                                                     * 02730000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 02740000
ESDIOER  DS    0H                                                       02750000
         SYNADAF ACSMETH=BPAM           ANALYZE I/O ERRORS              02760000
         WTO   MF=(E,(1)),ROUTCDE=11,DESC=7  PUT OUT SYNAD MESSAGE      02770000
         MVC   NAMEFLD(8),MEMNAME       SET UP MEMBER NAME FOR MSG      02780000
         L     R3,INBEGIN               PTR TO INDD TABLE               02790000
         MVC   DDNMFLD(8),2(R3)         MOVE DDNAME TO MSG              02800000
         MVI   NAMEDISP,45              MEMBER NAME GOES AT +45         02810000
         MVI   DDNMDISP,13              DDNAME GOES AT +13              02820000
         MVI   MSGLIST+1,CESDIOER       I/O ERROR READING MEMBER        02830000
         LA    R2,ESDIOER1              SET RETURN ADDRESS              02840000
         B     MSGWRTN                  WRITE MSG WITH NAME             02850000
ESDIOER1 DS    0H                                                       02860000
         SYNADRLS                       RELEASE SYNAD BUFFERS           02870000
         MVI   RC,12                    SET RETURN CODE TO 12           02880000
         B     NORMRET                  RETURN                          02890000
         EJECT                                                          02900000
ESDCB    DCB   MACRF=(R),RECFM=U,SYNAD=ESDIOER,DSORG=PO                 02910000
         EJECT                                                          02920000
ZZRELCOM DSECT                                                          02930000
         TITLE 'Z Z R E L C O M  -  ZZRELINK COMMUNICATIONS AREA'       00010000
**********       C O M M U N I C A T I O N    A R E A        ********** 00020000
**********               Z Z R E L I N K                     ********** 00030000
*                                                                       00040000
* THE FOLLOWING DESCRIPTION IS FOR ZZRELINK.                            00050000
* IT IS BASICALLY THE SAME FOR THE IEBCOPY PROCEDURE EXCEPT THAT        00060000
* IT DOES NOT GO THROUGH ALL THE SORTING STEPS. IT ALSO DOES NOT BUILD  00070000
* MULTIPLE DD TABLES AND IT DOES NOT ALLOW FOR RENAMES.                 00080000
*                                                                       00090000
         SPACE 2                                                        00100000
         SPACE 2                                                        00110000
* ABBREVIATIONS USED FREQUENTLY -                                       00120000
*        ODE = OUTPUT DIRECTORY ENTRY                                   00130000
*        IDE = INPUT DIRECTORY ENTRY                                    00140000
         SPACE 2                                                        00150000
*                                                                       00160000
* SETAB = SELECT/EXCLUDE TABLE                                          00170000
*        THE SETAB CONSISTS OF 10-BYTE ENTRIES.  ENTRIES ARE MADE TO    00180000
*        THIS TABLE WHEN -                                              00190000
*              1) A SELECTIVE LINK HAS BEEN SPECIFIED.  EACH ENTRY IS   00200000
*              FOR THE NAME OF A MEMBER TO BE SELECTED.  IF A MEMBER IS 00210000
*              TO BE RENAMED, TWO 10-BYTE ENTRIES ARE MADE IN THE SETAB 00220000
*              2) AN EXCLUSIVE LINK HAS BEEN SPECIFIED.  EACH ENTRY IS  00230000
*              FOR THE NAME OF A MEMBER TO BE EXCLUDED.                 00240000
*        NOTE- IN THE CASE OF A SELECTIVE LINK (AND ONLY IN THIS CASE), 00250000
*              THE SETAB WILL ALSO BE USED AS THE CTLTAB.  IN ALL OTHER 00260000
*              CASES, A SEPARATE CTLTAB WILL BE CONSTRUCTED.            00270000
*        BYTE 0 OF AN ENTRY IS DESIGNATED AS SEFLAG1.  WITHIN THIS BYTE 00280000
*        THE FOLLOWING BITS HAVE MEANING -                              00290000
SEBIT1   EQU   X'80' ON=THIS IS A NEWNAME ENTRY                         00300000
SEBIT2   EQU   X'40' ON=THIS IS A RENAMED ENTRY                         00310000
SEBIT3   EQU   X'20' ON=REPLACE OPTION WAS SPECIFIED FOR THIS MEMBER    00320000
SEBIT4   EQU   X'10' ON=DONTLINK FLAG...DO NOT PROCESS THIS ENTRY       00330000
SEBIT5   EQU   X'08' ON= THIS MEMBER HAS BEEN ''FOUND'' ON INPUT D.S.   00340000
SEBIT6   EQU   X'04' ON= THIS IS LAST ENTRY IN SETAB/CTLTAB             00350000
*        LO ORDER 2 BITS NOT USED                                       00360000
*        NOTE THE DEFINITION OF SEBIT3.  IF, IN THE INDD TABLE, BYTE 0  00370000
*              BIT 2 IS ON, THIS MEANS THAT THE REPLACE OPTION WAS SPE- 00380000
*              CIFIED FOR ALL MEMBERS COPIED FROM THIS INDD.  IT IS     00390000
*              VALID FOR BOTH OF THESE BITS TO BE ON AT THE SAME TIME,  00400000
*              ALTHOUGH IF THE BIT IS ON IN THE INDD TABLE, IT IS UN-   00410000
*              NECESSARY FOR SEBIT3 TO ALSO BE ON IN THE SETAB.         00420000
         EJECT                                                          00430000
* FOLLOWING IS A DESCRIPTION OF THE CONTROL TABLE -CTLTAB-              00440000
* (REMEMBER THAT THIS TABLE IS PHYSICALLY THE SAME TABLE AS SETAB WHEN  00450000
* A SELECTIVE LINK OPERATION IS SPECIFIED, BUT IS PHYSICALLY INDEPEN-   00460000
* DENT AND DISTINCT FROM THE SETAB FOR AN EXCLUSIVE LINK OPERATION.     00470000
* THERE IS NO SETAB FOR A FULL LINK OPERATION, AND THE CTLTAB IN THIS   00480000
* CASE IS CONSTRUCTED SIMILARLY TO WHEN AN EXCLUSIVE LINK IS SPECIFIED. 00490000
* SINCE THE SETAB AND CTLTAB ARE ONE AND THE SAME FOR A SEL. LINK, THIS 00500000
*  DESCRIPTION WILL ASSUME THAT A SELECTIVE LINK IS BEING DONE, FOR THE 00510000
*  PURPOSE OF SETTING UP THE TABLE INITIALLY.)....                      00520000
         SPACE 2                                                        00530000
*        INITIAL TABLE, FOLLOWING CCSCAN PROCESSING OF ''SELECT'' -     00540000
*********************************************************************** 00550000
*SEFLAG1 *SEFLAG2 * NAME OF MEMBER TO BE SELECTED, OR, IF SPECIFIED,  * 00560000
*(DESCR. *(UNUSED)* NEWNAME.  IF NEWNAME WAS SPECIFIED, THE TABLE WILL* 00570000
* ABOVE) *        * CONTAIN 1 ENTRY FOR OLDNAME AND ANOTHER FOR NEW.  * 00580000
*********************************************************************** 00590000
* 1 BYTE * 1 BYTE *----------------------8 BYTES----------------------* 00600000
         SPACE 2                                                        00610000
* OLDNAME/NEWNAME PAIRS ARE NOW EXTRACTED FROM THE SETAB.  THEN THE     00620000
* CTLTAB IS SORTED ALPH. BY MBRNAME, AND A NEWNAME PTRTABLE SET UP.     00630000
* WHEN THIS HAS BEEN DONE, THE INPUT DATA SET'S DIRECTORY IS SEARCHED   00640000
* FOR MATCHING NAMES (THE NEWNAME ENTRIES IN THE TABLE ARE NOT USED FOR 00650000
* THIS COMPARISON).  WHEN A MATCHING MEMBERNAME IS FOUND, THE DIRECTORY 00660000
* ENTRY IS RETAINED IN CORE (IF SPACE PERMITS), OR IT IS SPILLED ONTO   00670000
* SYSUT3.  IN EITHER CASE, THE CORE ADDRESS OR THE TTR+INDICATOR ARE    00680000
* PUT INTO THE CTLTAB, OVERLAYING THE HIGH-ORDER 4 BYTES OF 'OLDNAME'.  00690000
* THE MEMBER-TTR IS EXTRACTED FROM THE DIRECTORY ENTRY, AND OVERLAYS    00700000
* THE LOW-ORDER 4 BYTES OF 'OLDNAME' IN THE CTLTAB.  SEBIT5 IS TURNED   00710000
* ON.  IF THIS IS AN ALIAS ENTRY, SEFLAG2 IS SET AS FOLLOWS -           00720000
ALIAS    EQU   X'80'         TO TEST FOR AND SET ALIAS DIRECTORY ENTRY  00730000
         SPACE 2                                                        00740000
*        CTLTAB ENTRY FOR A ''FOUND'' MEMBER -                          00750000
         SPACE 2                                                        00760000
*********************************************************************** 00770000
*SEFLAG1 *SEFLAG2 * INDIC. * ADDR OF THE IN-* ZEROES *   MEMBER TTR   * 00780000
*        *        *  BYTE  * PUT DIR. ENTRY *        *                * 00790000
*********************************************************************** 00800000
*-1 BYTE-*-1 BYTE-*-1 BYTE-*-----3 BYTES----*-1 BYTE-*-----3 BYTES----* 00810000
         SPACE 2                                                        00820000
* THE INDICATOR BYTE IS ZEROES IF THE DIRECTORY ENTRY IS IN CORE, OR IT 00830000
* IS HEX '01' IF THE DIRECTORY ENTRY WAS SPILLED TO SYSUT3.             00840000
* NOTE THAT CTLTAB ENTRIES FOR A ''NEWNAME'' ARE NOT OVERLAYED OR AL-   00850000
* TERED AT ANY TIME.  ONCE THE ENTIRE INPUT DIRECTORY HAS BEEN SCANNED, 00860000
* (OR AT LEAST ALL ENTRIES FOR MEMBERS TO BE COPIED FROM THIS INPUT     00870000
* DATA SET HAVE BEEN BUILT), THE OUTPUT DATA SET DIRECTORY IS READ.     00880000
* MEMBERNAMES OF MEMBERS CURRENTLY IN THE OUTPUT DATA SET ARE COMPARED  00890000
* AGAINST MEMBERNAMES OF MEMBERS REFERENCED IN THE CTLTAB FOR THE CUR-  00900000
* RENT INPUT DATA SET, UNLESS THE LATTER WERE RENAMED.  IF THE INPUT    00910000
* MEMBER IS RENAMED, THEN THE NEWNAME IS COMPARED AGAINST THE OUTPUT.   00920000
* IF DUPLICATE NAMES ARE ENCOUNTERED, AND IF THE REPLACE OPTION WAS NOT 00930000
* SPECIFIED ON EITHER THE INDD LEVEL OR THE MEMBERNAME LEVEL, THEN THE  00940000
* DONT-LINK BIT (SEBIT4) IS SET IN THE FLAG BYTE OF THE APPROPRIATE     00950000
* CTLTAB ENTRY, AND THE INDIC. BYTE (BYTE 3) OF THIS ENTRY IS SET TO    00960000
* HEX 'FF'.                                                             00970000
* THINK OF THE LOW-ORDER 8-BYTES OF EACH ''FOUND'' CTLTAB ENTRY AS BE-  00980000
* ING DIVIDED INTO A LEFT HALF (INDIC. + DIR. ENTRY ADDR.) AND A RIGHT  00990000
* HALF (ZEROES + MBR. TTR).  THE LEFT HALF NOW REPRESENTS DIRECTORY     01000000
* ENTRIES FOUND IN THE CURRENT INPUT DATA SET, AND IS IN ALPHAMERIC     01010000
* SEQUENCE.                                                             01020000
         EJECT                                                          01030000
* THE NEXT STEP IN CTLTAB PROCESSING CAUSES THE ''FOUND'' ENTRIES TO BE 01040000
* MANIPULATED, WITH THE RESULT BEING THAT THE LEFT HALF CONTAINS (IN-   01050000
* DIC + ADDR OF DIR. ENTRY) IN SEQUENCE BY MEMBER TTR, AND THE RIGHT    01060000
* HALF CONTAINS THIS SAME INFORMATION (INDIC. + ADDR OF DIR. ENTRY) IN  01070000
* ALPHAMERIC SEQUENCE, OVERLAYING THE ACTUAL MEMBER TTR.  ANOTHER RE-   01080000
* SULT OF THIS MANIPULATION OF THE CTLTAB IS THAT MAIN-MEMBER ENTRIES   01090000
* PRECEDE THE CORRESPONDING ALIAS ENTRIES.  NOTE THAT THE BITS SET IN   01100000
* SEFLAG1 ARE NOW ONLY USEFUL FOR THE ''RIGHT HALF'' OF THE CTLTAB,     01110000
* SINCE THEY ARE NOT MANIPULATED AND THUS REMAIN IN THE ORIGINAL (AL-   01120000
* PHABETIC) SEQUENCE.                                                   01130000
         SPACE 2                                                        01140000
*        CTLTAB ENTRY FOR A FOUND MEMBER FOLLOWING TTR SORT -           01150000
         SPACE 2                                                        01160000
*********************************************************************** 01170000
*SEFLAG1 *SEFLAG2 * INDIC. * ADDR OF IN. DE * INDIC. * ADDR OF IN. DE * 01180000
*        *        *  BYTE  *                *  BYTE  *                * 01190000
*        *        *IN SEQ BY MEMBER TTR     *IN SEQ ALPHAMERICALLY    * 01200000
*********************************************************************** 01210000
* 1 BYTE-*-1 BYTE-*-1 BYTE-*-----3 BYTES----*-1 BYTE-*-----3 BYTES----* 01220000
         SPACE 2                                                        01230000
* AT THIS POINT, THERE IS NO LOGICAL RELATIONSHIP BETWEEN THE LEFT AND  01240000
* RIGHT HALVES OF THE RELEVANT CTLTAB ENTRIES.  BOTH HALVES CONTAIN THE 01250000
* SAME INFORMATION, BUT IT IS IN TWO DIFFERENT SEQUENCES.  NOTE THAT,   01260000
* IF A MEMBER BEING LOOKED FOR IS NOT ''FOUND'', ITS CTLTAB ENTRY RE-   01270000
* MAINS UNALTERED - THE NAME IS STILL UN-OVERLAYED.                     01280000
* FROM THIS POINT ON, THOSE MEMBERS OF THE INPUT DATA SET WHOSE DIREC-  01290000
* TORY ENTRIES ARE REFERENCED IN THE CTLTAB WILL BE COPIED, PROVIDED    01300000
* THAT THEY ARE ''FOUND'' AND NOT FLAGGED AS ''DONT-LINK''.  THEN THE   01310000
* DIRECTORY ENTRIES WILL BE MERGED.  AS THE MERGE IS PERFORMED, WHEN AN 01320000
* INPUT DE IS MERGED, IF THIS IS A SELECTIVE LINK, THE DONT-LINK BIT    01330000
* (SEBIT4) IS TURNED ON, THUS ALLOWING FOR THESE ENTRIES IN THE CTLTAB  01340000
* TO BE IGNORED IN SUBSEQUENT PASSES THROUGH THE SAME TABLE FOR THE EN- 01350000
* SUING INPUT DATA SETS.                                                01360000
JSTCPD   EQU   X'10'              IF ON IN THE 2ND BYTE OF A CTLTAB EN- 01370000
*                                 TRY (SEFLAG2), THERE IS A NAME (CON-  01380000
*                                 TAINED IN THE 3RD THROUGH 10TH BYTES  01390000
*                                 OF THIS ENTRY) OF AN INPUT MEMBER     01400000
*                                 WHICH HAS BEEN SUCCESSFULLY COPIED.   01410000
*                                 THIS MEMBERNAME WILL BE PRINTED BY    01420000
*                                 THE TERMINATION MODULE (ZZRVTM) UNDER 01430000
*                                 APPROPRIATE CONDITIONS, AT WHICH TIME 01440000
*                                 THIS BIT WILL BE TURNED OFF.          01450000
*********************************************************************** 01460000
         TITLE 'Z Z R E L I N K  -  CONTROL, DDNAME, AND SELECT TABLES' 01470000
*XXXXXXXXXX    EVERY CONSTANT BETW. HERE AND NXT X'S MUST BE CONTIGUOUS 01480000
* ALL CONSTANTS BETWEEN X'S ARE SET TO ZERO FOR EVERY INPUT DATA SET.   01490000
* THE NAMES OF THE CONSTANTS WHICH ARE INCLUDED IN THIS CONTIG. AREA -  01500000
* CDCT, FCT, NNTCT, OLDTTR, FLG5                                        01510000
* ICPT,SVLSTO,SVFSTO,FLG2,FLG6,SWITCH1,UTTRFLAG,OBCT                    01520000
         SPACE 2                                                        01530000
*********************************************************************** 01540000
INBEGIN  DC    F'0'          ADDRESS OF START OF INDD TABLE             01550000
REPLACOP EQU   SEBIT3        IF ON IN HI-ORDER BYTE OF AN INDD-TABLE    01560000
*                            ENTRY, THE REPLACE OPTION WAS SPECIFIED    01570000
*                            FOR THIS INPUT DATA SET.                   01580000
SEBEGIN  DC    AL4(0)        ADDR OF BEGINNING OF SETAB                 01590000
SESTOP   DC    F'0'          IF SELECTIVE OR EXCLUSIVE LINK, THIS IS    01600000
*                            THE ADDRESS (+1) OF THE END OF THE SETAB.  01610000
*                            IF FULL LINK, ADDR (+1) OF END OF INDDTAB. 01620000
ADNNPTRT DS    AL4           ADDR OF NEWNAME POINTER TABLE              01630000
CTAD     DS    AL4           ADDR OF CONTROL TABLE - IF SELECTIVE LINK, 01640000
*                            THIS WILL BE SAME AS SEBEGIN.              01650000
ENCT     DC    HL2'0'        COUNT OF NBR OF ENTRIES IN CTLTAB (TOTAL)  01660000
*              IF THIS IS A SELECTIVE LINK, THIS COUNT WILL BE INI-     01670000
*              TIALIZED TO THE NUMBER OF ENTRIES IN SETAB.              01680000
COUNT    DC    HL2'0'        COUNT OF NBR OF ENTRIES IN SETAB IF SEL OR 01690000
*                            EXCL LINK, OR ZERO IF FULL LINK.           01700000
INDDCT   DC    AL2(0)        COUNT OF NO. INDD'S IN CURRENT STEP        01710000
NNCT1    DC    AL2(0)        NBR OF NEWNAMES SPECIFIED IN CURRENT SE-   01720000
*                            LECTIVE LINK STEP (IF NOT SEL CPY, = 0)    01730000
         DS    0D                                                       01740000
OUTNAME  DC    CL8' '        NAME OF CURRENT OUTPUT DD                  01750000
         SPACE 2                                                        01760000
CCIMAGE  DS    10D                     SCAN CONTROL CARD BUFFER         01770000
         SPACE 2                                                        01780000
WKA1     DC    10F'0'        VOLATILE WORK AREA FOR GENERAL USE         01790000
*                                                                       01800000
*****   PLEASE NOTE---    ZZRESCAN IS NOT THE ONLY MODULE WHICH USES    01810000
**                WKA1 FOR TEMPORARY WORK SPACE.                        01820000
*                 HOWEVER, THE FOLLOWING 4 EQUATES ARE USED ONLY BY     01830000
*                 ZZRESCAN -                                            01840000
*                                                                       01850000
SARG     EQU   WKA1          TEMPORARY WORK AREA USED BY ZZRESCAN       01860000
*                            TO HOLD SEARCH ARGUMENT 8 BYTES LONG       01870000
SAVEPAPR EQU   WKA1+8        TEMPORARY WORK AREA USED BY ZZRESCAN       01880000
*                            TO HOLD PARTIAL PARAMETERS IF CONTROL CARD 01890000
*                            IS CONTINUED, 8 BYTES LONG.                01900000
LEFTPCNT EQU   WKA1+16       TEMPORARY WORK AREA USED BY ZZRESCAN       01910000
*                            TO HOLD COUNT OF LEFT PARENTHESIS SCANNED. 01920000
RGHTPCNT EQU   WKA1+18       TEMPORARY WORK AREA USED BY ZZRESCAN       01930000
*                            TO HOLD COUNT OF RIGHT PARENTHESIS SCANNED 01940000
*                            BOTH LEFTPCNT AND RGHTPCNT 2 BYTES EACH    01950000
*                            AND MUST BE CONTIGUOUS STORAGE.            01960000
CSTOREG  DC    3F'0'         SAVE AREA USED BY ZZRESCAN                 01970000
         SPACE 2                                                        01980000
*        REGISTER SAVE AREAS                                            01990000
SV1      DC    18F'0'        REGISTER SAVE AREA FOR MAINFLOW            02000000
SV2      DC    18F'0'        REGISTER SAVE AREA FOR NON-RESIDENT RTNES  02010000
MCAMOD   DC    10F'0'             ZZRELCOM CHANGE AREA                  02020000
* MCAMOD IS A PATCH-AREA FOR USE IN MAINTENANCE OF THIS PROGRAM         02030000
         TITLE 'POINTERS FOR INTER-MODULE COMMUNICATION'                02040000
VZZRSCN  DC    V(ZZRESCAN)         ADDR OF CONTROL CARD SCAN ROUTINE    02050000
VZZRLEOF DC    V(ZZRLEOF) EP ADDRESS OF SYSIN EODAD EXIT IN ZZRESCAN    02060000
VZZRLMES DC    V(ZZMESAGE)         ADDRESS OF MESSAGE WRITER ROUTINE    02070000
VZZRTERM DC    V(ZZRTERM)         ADDRESS OF TERMINATION ROUTINE        02080000
         ORG   VZZRTERM                                                 02090000
AZZRTERM DS    F                  ADDRESS OF TERMINATION ROUTINE        02100000
         TITLE 'S Y S I N    D A T A    C O N T R O L    B L O C K'     02110000
*CARDCB  DCB   DDNAME=SYSIN,RECFM=FB,LRECL=80,EODAD=VZZRLEOF,           02120000
*              MACRF=(GM),DSORG=PS                                      02130000
CARDCB   DCB   DDNAME=SYSIN,RECFM=FB,LRECL=80,EODAD=VZZRLEOF,          X02140000
               MACRF=(GM),DSORG=PS                                      02150000
         TITLE 'S Y S P R I N T    D A T A    C O N T R O L    B L O C X02160000
               K'                                                       02170000
* PRTDCB DCB   DDNAME=SYSPRINT,DSORG=PS,MACRF=(PM),RECFM=FBA,           02180000
*              LRECL=121,BLKSIZE=121                                    02190000
PRTDCB   DCB   DDNAME=SYSPRINT,DSORG=PS,MACRF=(PM),RECFM=FBA,          X02200000
               LRECL=121,BLKSIZE=121                                    02210000
         TITLE 'SWITCH AND WORK AREA DEFINITIONS FOR ZZRESCAN'          02220000
* THE FOLLOWING SWITCHES (PARMSWCH, CCSWITCH, COMDCDSW, CPARAMSW, CCDE- 02230000
* LIM, CCDELIM2) ARE PRIMARILY BUT NOT SOLELY USED BY ZZRESCAN -        02240000
*                                                                       02250000
         SPACE 1                                                        02260000
PARMSWCH DC    XL1'0'    SCAN INTERNAL INDD AND MEMBER SWITCHES         02270000
SCANNAME EQU   X'80'         SCANNING NAME                              02280000
SET4REPL EQU   X'40'         MULTIPLE ( EXPECT REPLACE                  02290000
ONELEFT  EQU   X'20'         FIRST LEFT PARENTHESIS                     02300000
LINKNOW  EQU   X'10'         NOW SCANNING LINK CARD                     02310000
STOPSCAN EQU   X'08'         BLANK ENCOUNTERED                          02320000
FLUSHSW  EQU   X'04'                   FLUSH TO NEXT LINK RE/SET ALONE  02330000
HASNEWNM EQU   X'02'         HAVE A NEW NAME WITH MEMBER                02340000
COMDPART EQU   X'01'         PARTIAL COMMAND- CONTINUED ON NEXT CARD    02350000
*                                                                       02360000
         SPACE 1                                                        02370000
CCSWITCH DC    XL1'0'                  CONTROL CARD SWITCHES- EXTERNAL  02380000
CARDPRTD EQU   X'80'         ON = CONTROL STATEMENT HAS BEEN PRINTED    02390000
SYSINEOF EQU   X'40'         END OF FILE ON SYSIN                       02400000
UNECPARN EQU   X'20'         INDD/MEMBER NAMES IMBEDDED IN PARENTHESIS  02410000
ZZRLINKC EQU   X'10'         ZZRLINK CONTROL CARDS                      02420000
COMDNOW  EQU   X'08'         COMMAND WORD                               02430000
LASTPARM EQU   X'04'         LAST PARAMETER- BYPASS SWITCH              02440000
MULTSE   EQU   X'02' MULTIPLE SELECT/EXCLUDE STATEMENTS                 02450000
FIRSTSCN EQU   X'01' ON=ZZRESCAN HAS BEEN CALLED FOR THE FIRST TIME     02460000
*                                                                       02470000
         SPACE 2                                                        02480000
COMDCDSW DC    XL1'0'           SOME EXTERNAL SWITCHES                  02490000
LINKDONE EQU   X'80'         LINK COMMAND SCANNED ALL OK                02500000
SELECTSC EQU   X'40'         SELECT COMMAND SCANNED                     02510000
EXCLUDES EQU   X'20'         EXCLUDE COMMAND SCANNED                    02520000
NEWOUT   EQU   X'10'         OUTDD KEYWORD PRESENT                      02530000
NEWINDD  EQU   X'08'         INDD KEYWORD PRESENT                       02540000
LISTSW   EQU   X'04'         DO NOT LIST MEMBERS COPIED (LIST=NO)       02550000
COMPRESS EQU   X'02'                   COMPRESS LINK DATA SET           02560000
MEMBRCD1 EQU   X'01'         MEMBER STATEMENT                           02570000
*                                                                       02580000
         SPACE 2                                                        02590000
CPARAMSW DC    XL1'0'       INTERNAL SCAN SWITCHES                      02600000
DELIMEND EQU   X'80'         DELIMITER IN COLUMN 71                     02610000
CONTINY  EQU   X'40'         CONTINUATION                               02620000
PARMCOME EQU   X'20'         PARAMETER FOLLOWS                          02630000
PARTPARM EQU   X'10'         PARTIAL PARAMETER                          02640000
READ1    EQU   X'08'         READ ANOTHER CONTROL STATEMENT             02650000
COMDPARM EQU   X'04'         COMMAND FOLLOWED BY PARAMETER              02660000
COL72BLK EQU   X'02'         COLUMN 72 NOT BLANK                        02670000
PARMZERO EQU   X'01'         PARAMETER LENGTH ZERO                      02680000
*                                                                       02690000
         SPACE 2                                                 A48742 02700000
SCANSWCH DC    XL1'0'        INTERNAL SCAN SWITCHES              A48742 02710000
NOCMMEXP EQU   X'80'         DON'T SCAN COMMAND ON CONTIN CARDS  A48742 02720000
*              LOW ORDER 7 BITS NOT USED - RESERVED              A48742 02730000
*                                                                       02740000
         SPACE 2                                                        02750000
CCDELIM  DC    XL1'0'        INTERNAL SCAN SWITCHES                     02760000
EQUALSGN EQU   X'80'         EQUAL SIGN                                 02770000
COMMASGN EQU   X'40'         COMMA                                      02780000
LEFTPRSG EQU   X'20'         LEFT PARENTHESIS                           02790000
RIGHTPRS EQU   X'10'         RIGHT PARENTHESIS                          02800000
BLANKSGN EQU   X'08'         BLANK                                      02810000
LASTCOMA EQU   X'04'         LAST DELIMITER A COMMA- READ A CARD        02820000
BADBLOCK EQU   X'02' VALIDATE-ZZRDV0 SETS IF SYSIN/SYSPRINT BLOCKSIZE   02830000
*          IS BAD.                                                      02840000
*              LO ORDER BIT NOT USED                                    02850000
*                                                                       02860000
         SPACE 2                                                        02870000
CCDELIM2 DC    XL1'0'  USED TO SAVE SETTINGS OF CCDELIM ON CONTINUATION 02880000
*                                                                       02890000
         TITLE 'ERROR FLAG DEFINITIONS USED BY OPEN FAILURE ROUTINE'    02900000
IOEF2    DC    X'00'              FLAGS DESCRIBING NATURE/TYPE OF I/O   02910000
*                                 ERROR                                 02920000
ERF9     EQU   X'80'         ON = 'HARD' ERROR WRITING MERGED OUTPUT    02930000
*                                 DIRECTORY TO SYSUT4                   02940000
ERF10    EQU   X'40'         ON = ERROR READING FROM SYSUT4.  IF 'ERF4' 02950000
*                                 OFF, ERROR OCCURRED DURING MERGE      02960000
*                                 PHASE OF PROGRAM - IF 'ERF4' ON, SEE  02970000
*                                 DESCRIPTION OF 'ERF4'.                02980000
NOSYSIN  EQU   X'20'         ON = SYSIN COULD NOT BE OPENED OR WAS IN-  02990000
*                                 VALID, OR BECAME UNUSABLE DUE TO AN   03000000
*                                 I/O ERROR                             03010000
SPRNOPN  EQU   X'10'         SYSPRINT COULD NOT BE OPENED, WAS INVALID- 03020000
*                                 LY SPECIFIED, OR AN I/O ERROR OCCUR-  03030000
*                                 RED MAKING SYSPRINT UNAVAILABLE       03040000
*              LOW ORDER 4 BITS NOT USED - RESERVED                     03050000
*                                                                       03060000
         TITLE 'SWITCH AND WORK AREA DEFINITIONS FOR ZZMESAGE'          03070000
         DS    0H                                                       03080000
MSGLIST  DC    4H'0'              AREA FOR PARAMETRIC INPUT TO ZZMESAGE 03090000
MSG1     EQU   MSGLIST                                                  03100000
MSG2     EQU   MSG1+2                                                   03110000
MSG3     EQU   MSG2+2                                                   03120000
MSG4     EQU   MSG3+2                                                   03130000
* THE FOLLOWING BITS WILL BE SET ON BY THE CALLER OF ZZMESAGE (MESSAGE  03140000
* WRITING ROUTINE) IN THE HIGH ORDER BYTE OF EACH APPROPRIATE HALFWORD  03150000
* IN THE MSGLIST PARAMETER(S) BEING USED -                              03160000
LASTMSG  EQU   X'80'         ON = LAST PARAMETER IN MSGLIST             03170000
CTLCD    EQU   X'40'         ON = A CONTROL CARD IS TO BE PRINTED       03180000
IOERF    EQU   X'20'         ON = A MESSAGE IS IN THE MESSAGE BUFFER -  03190000
*                                 AND IS TO BE PRINTED.  NO MSG CODE IS 03200000
*                                 ASSOCIATED WITH THIS MESSAGE, AND IT  03210000
*                            USUALLY WILL BE A SYNADAF MESSAGE.         03220000
RCODE    EQU   X'10'         ON = PUT RETURN CODE INTO THIS MSG TEXT    03230000
PBIT     EQU   X'08'         ON = USE PARAM LIST WITH THIS MSG TEXT     03240000
*              LO ORDER 3 BITS NOT USED - RESERVED                      03250000
         SPACE 2                                                        03260000
MSGPARAM DS    9H                 THIS FIELD IS TO CONTAIN PARAMETERS   03270000
*                                 TO BE PLACED IN MESSAGES              03280000
         ORG   MSGPARAM                                                 03290000
NAMEDISP DC    X'00'              THE DISPLACEMENT OF A NAME            03300000
*                                 PARAMETER FROM THE BEGINNING OF       03310000
*                                 THE MESSAGE IT IS TO BE PLACED IN     03320000
*                                 THIS BYTE                             03330000
NODISP   DC    X'00'              THE DISPLACEMENT OF A NUMBER          03340000
*                                 PARAMETER FROM THE BEGINNING OF       03350000
*                                 THE MESSAGE IT IS TO BE PLACED IN     03360000
*                                 THIS BYTE                             03370000
DDNMDISP DC    X'00'              DISPLACEMENT OF DDNAME FROM MSG START 03380000
         DS    0D                                                       03390000
NAMEFLD  DC    CL8' '        AREA CONTAINING NAME TO BE PUT INTO MSG    03400000
DDNMFLD  DC    CL8' '        AREA CONTAINING DDNAME TO INSERT IN MSG    03410000
DDVALNM  EQU   NAMEFLD       USED BY VALIDATE TO SAVE DD NAME           03420000
PARAMS   DC    X'00'                                                    03430000
NAME     EQU   X'80'         ON = THERE IS A NAME PARAMETER             03440000
NBR      EQU   X'40'         ON = THERE IS A NUMBER PARAMETER           03450000
DDNM     EQU   X'20'         ON = THERE IS A DDNAME PARAMETER           03460000
NOFLD    DC    CL7'0'        AREA CONTAINING NUMBER TO BE PUT INTO MSG  03470000
         TITLE 'MESSAGE CODE DEFINITIONS USED BY ALL ZZRELINK MODULES'  03480000
*********************************************************************** 03490000
*        THE FOLLOWING MESSAGE CODES ARE USED BY ZZRELINK, ZZRESCAN,  * 03500000
*    AND ZZMESAGE TO IDENTIFY AND PRODUCE ALL ZZRELINK MESSAGES.      * 03510000
*********************************************************************** 03520000
INALCNTR EQU     01                INVALID COMMAND OR KEYWORD           03530000
INVALSPR EQU     02                INVALID PARAMETER                    03540000
ONEQPARN EQU     03                UNEQUAL PARENTHESIS                  03550000
INVALCON EQU     04                INVALID CONTINUATION                 03560000
MEMNOSE  EQU     05                MEMBER WITHOUT SELECT/EXCLUDE        03570000
MULTSSEE EQU     06                ONLY ONE SELECT/EXCLUDE PER INDD     03580000
INVALREP EQU     07                INVALID REPLACE SPECIFIED            03590000
NULLPARM EQU     08                NULL PARAMETERS                      03600000
NORREN   EQU     09                CANNOT RENAME/REPLACE ON EXCLUDE     03610000
NOINDD   EQU     10                OUTDD OR INDD NOT SPECIFIED          03620000
INVALIST EQU   11                  OUTDD/LIST NOT ON LINK STATEMENT     03630000
ENDMESS  EQU   12                  END OF CONTROL CARDS                 03640000
MODEERR  EQU   13                  MIXING ZZRLINKAND ZZRDSCPY MODE      03650000
NOOVLYS  EQU   14                  WARNING - OVERLAYS NOT SUPPORTED     03660000
SCANMSG  EQU   15                  CONTROL STATEMENT ERROR              03670000
SEQERROR EQU   16                  STATEMENT SEQUENCE ERROR             03680000
GENERUAL EQU   17                  GENERAL VALIDATION MESSAGE           03690000
OPENERRX EQU   18                  OPEN ERROR MESSAGE                   03700000
OBTAINER EQU   19                  OBTAIN ERROR                         03710000
NOTPDSER EQU   20                  OBTAIN NOT PDS                       03720000
INVALREC EQU   21                  INVALID LRECL                        03730000
INVALBLK EQU   22                  INVALID BLOCKSIZE                    03740000
UNMOVEDS EQU   23                  DATA SET UNMOVABLE                   03750000
RECFMINC EQU   24                  RECFM INCOMPATIBLE                   03760000
NODIR    EQU   25                  UNABLE TO OPEN DIRECTORY DCB         03770000
DIRERR01 EQU   26                  ALIAS NAME X OCCURS MORE THAN ONCE   03780000
ATABFULL EQU   27                  ALIAS TABLE FULL                     03790000
EPAERR01 EQU   28                  UNABLE TO LOCATE EPA OF MODULE X     03800000
DIRERR02 EQU   29                  I/O ERROR READING DIRECTORY FOR X    03810000
REPERR01 EQU   30                  MEMBER X NOT RELINKED - NO REP OPT   03820000
M39      EQU   31                  CANNOT COMPRESS WITH SELECT OR       03830000
*                                  EXLCUDE                              03840000
CESDIOER EQU   32                  I/O ERROR READING CESD RECORDS       03850000
NE       EQU   33                  LOAD MODULE IS 'NOT EDITABLE'        03860000
*                                  BUFFERS FOR COMPRESS                 03870000
M42      EQU   34 CANNOT SPECIFY DUPLICATE NAME FOR SEL/EXCL/RENAME     03880000
M43      EQU   35                  CANNOT PROCESS ALL OLD/NEW NAMES     03890000
*                                  SPECIFIED                            03900000
M45      EQU   36                  (DATA SET NAME) REFERENCES A NULL    03910000
*                                  INPUT DATA SET                       03920000
M46      EQU   37                  CANNOT RE/DE BLOCK WITH              03930000
*                                  NOTE-LIST/USER TTRN IN MEMBER        03940000
*                                  (MEMBER NAME)                        03950000
M47      EQU   38                  CANNOT CONTINUE TO BUILD CTLTAB      03960000
M48      EQU   39                 ALL SELECTED MEMBERS COPIED - DID NOT 03970000
*                                 USE ALL SPECIFIED INDD'S              03980000
M49      EQU   40                  (NUMBER) UNUSED TRKS IN OUTPUT DATA  03990000
*                                  SET REFERENCED BY (DDNAME)           04000000
M50      EQU   41                  CANNOT COMPRESS TRACK OVERFLOW DATA  04010000
*                                  SET                                  04020000
M51      EQU   42                  CANNOT COMPRESS WITH RE/DE BLOCKING  04030000
M53      EQU   43                  END OF JOB (0,4,8) WAS HIGHEST       04040000
*                                  SEVERITY CODE                        04050000
NORMOD   EQU   44                  NO SPACE IN OUTPUT DIRECTORY FOR     04060000
*                                  DIRECTORY ENTRIES FROM DATA SET      04070000
*                                  (DATA SET NAME)                      04080000
UNUSDDB  EQU   45                  THERE ARE (NUMBER) UNUSED DIRECTORY  04090000
*                                  BLOCKS IN THE OUTPUT DIRECTORY       04100000
TMDBTR   EQU   46     **WARNING** THE OUTPUT DS REF BY XXXXXXXX  A36049 04110000
*                     CONTAINS TOO MANY DIRECTORY BLOCKS PER     A36049 04120000
*                     TRACK                                      A36049 04130000
M58      EQU   47                  ERROR FORCES JOB TO TERMINATE        04140000
M59      EQU   48                  (MEMBER NAME) COMPRESSED- WAS        04150000
*                                  ALREADY IN PLACE                     04160000
M60      EQU   49                  ALL MEMBERS COMPRESSED-              04170000
*                                  ALL WERE ORIGINALLY COMPRESSED       04180000
MEMCOP   EQU   50                  (MEMBERNAME) HAS BEEN SUCCESSFULLY   04190000
*                                  COPIED                               04200000
RNMEMCOP EQU   51                  (MEMBER NAME) HAS BEEN RENAMED AND   04210000
*                                  SUCCESSFULLY COPIED                  04220000
NOTDA    EQU   52                 DATA SET NOT DIRECT ACCESS            04230000
NODDCARD EQU   53                 DD CARD NOT FOUND                     04240000
UNITER01 EQU   54                 UNIT TYPE DEFINED BY X NOT SUPPORTED  04250000
NOMBCPDM EQU   55                 NO MBRS COPIED FROM INPUT DATASET RE- 04260000
*                                 FERENCED BY (XXXXXXXX)                04270000
CONCATBD EQU   56                 CONCATENATED DATA SETS                04280000
IMPCOMPR EQU   57                 IMPLIED COMPRESS                      04290000
NOCMPOSS EQU   58                 CANNOT COMPRESS                       04300000
NOLINK   EQU   59                 NO MEMBERS FOR PARTIAL LINK,          04310000
*                                 WILL NOT LINK                         04320000
DOFULLCP  EQU  60                 TOTAL LINK ASSUMED                    04330000
MFBNC    EQU   61 MEMBER FOUND BUT NOT COPIED - I/O ERROR READING       04340000
*                 INPUT DIRECTORY                                       04350000
NONELINK EQU   62 NO MEMBERS COPIED                                     04360000
FOLLMCPD EQU   63 FOLLOWING MBRS COPIED FROM INPUT DS REF BY XXXXXXXX   04370000
PLAMPID  EQU   64 POSSIBLE LOSS OF ACCESS TO MEMBER AND/OR INCOMPLETE   04380000
*                 DIRECTORY                                             04390000
WODINC   EQU   65 SYSUT4 I/O ERROR - OUTPUT DIRECTORY MAY BE INCOMPLETE 04400000
WONTCOM  EQU   66 I/O ERROR ON SYSUT3 - COMPRESS IN PLACE NOT DONE      04410000
BADPRINT EQU   67 SYSPRINT COULD NOT BE OPENED                          04420000
SMNF    EQU   68                  (MBRNAME) WAS SELECTED BUT NOT FOUND  04430000
PA       EQU   69                  LOAD MODULE IS PAGE ALIGNED        $ 04440000
         SPACE 2                                                        04450000
RCBUF    DC    C'0'               COMPLETION-CODE AREA...CONTAINS CHAR- 04460000
*                                 ACTER REPRESENTATION OF HIGHEST       04470000
*                                 COMPLETION CODE SET BY UTILITY PGM    04480000
LINECT   DC   X'0'                COUNT OF NBR LINES WRITTEN ON ONE PG  04490000
PGLIMIT  EQU   56                 MAX NBR OF LINES TO BE PUT ON ONE PG  04500000
         SPACE 2                                                        04510000
MSGBUF   DC    121C' '            MESSAGE BUFFER                        04520000
PGNO     DC    C'0001'            STARTING PAGE NUMBER FOR MSG OUTPUT   04530000
*************              END OF COMMUNICATION AREA   **************** 04540000
***   KEEP CARD  'MCAEND' LAST IN COMMUNICATION AREA JUST BEFORE THE ** 04550000
****  EQUATE THAT DETERMINES THE COMMUNICATIONS AREA SIZE 'MCASIZE'  ** 04560000
         SPACE 1                                                        04570000
MCAEND   DS    0D        END OF COMMUNICATION AREA                      04580000
MCASIZE  EQU   MCAEND-ZZRELCOM    SIZE OF COMMUNICATIONS AREA FOR SNAPS 04590000
         EJECT                                                          02950000
ZZROVBLD CSECT                                                          02960000
RC       DC    XL1'00'                                                  02970000
*                                                                       02980000
AEODAE   DC    A(CESDER1)                                               02990000
*                                                                       03000000
SAVE     DS    18F                                                      03010000
*                                                                       03020000
CNMEFND  DC    XL1'00'                                                  03030000
*                                                                       03040000
CESDSW   DC    XL1'00'                                                $ 03050000
*                                                                     $ 03060000
ENDTABSW DC    XL1'00'                                                  03070000
*                                                                       03080000
OVLYSW   DC    XL1'00'                                                  03090000
*                                                                       03100000
CSECTCTR DS    PL2                                                    $ 03110000
*                                                                     $ 03120000
EPADDR   DS    CL3                                                      03130000
*                                                                       03140000
MEMNAME  DS    CL8                                                      03150000
*                                                                       03160000
ADDRMAIN DC    F'0'                                                     03170000
*                                                                       03180000
CSCTNAME DS    CL8                                                      03190000
*                                                                       03200000
CESDUSED DC    F'0'                                                     03210000
*                                                                       03220000
SEGUSED  DC    F'0'                                                     03230000
*                                                                       03240000
CESDBFFR DC    XL256'0'                                                 03250000
*                                                                       03260000
SEGBFBEG DS    CL1048                                                   03270000
*                                                                       03280000
         EJECT                                                          03290000
         LTORG                                                          03300000
         EJECT                                                          03310000
CESDADR  DS    CL20480                                                  03320000
         END                                                            03330000
//STEP4    EXEC ASMSUBRS,MEMBER=ZZROVCRD                                00380000
//ASM.SYSIN DD *                                                        00390000
         MACRO                                                          00000010
&TAG     EQUREGS                                                        00000020
         GBLB  &SW                                                      00000030
         LCLA  &N                                                       00000040
         LCLC  &R                                                       00000050
         AIF   (&SW).EXIT                                               00000060
&SW      SETB  1                                                        00000070
&R       SETC  'R'                                                      00000080
&N       SETA  0                                                        00000090
.LOOP    ANOP                                                           00000100
&R.&N    EQU   &N                                                       00000110
&N       SETA  &N+1                                                     00000120
         AIF   (&N LT 16).LOOP                                          00000130
.EXIT    MEXIT                                                          00000140
         MEND                                                           00000150
         MACRO                                                          00001000
&NAME    CHAIN &BASE=10,&SAVE=(14,12),&SAVAR=SAVE,&PL1=NO,&SAVEPTR=13, X00002000
               &RENT=,&MACBASE=15,&MODULE=*,&DSALTH=100,&WKREG=14,     X00003000
               &PLICO=NO,&PARM=,&ENTRY=                                 00004000
         LCLC  &CHARFLD,&INTNAME                                        00005000
         LCLA  &COUNTER,&ROUND,&BASENO,&BDISP                           00006000
         LCLB  &SAVE1                                                   00007000
&INTNAME SETC  '&NAME'                                                  00008000
         AIF    ('&MACBASE' NE '15').NOTB15                             00009000
         AIF   ('&MODULE' EQ '').NOTB15                                 00010000
         AIF   ('&PLICO' EQ 'YES' AND '&MODULE' NE '*').NOTB15          00011000
         USING  *,15 .ESTABLISH TEMPORARY BASE REG                      00012000
.NOTB15  ANOP                                                           00013000
         AIF   ('&MODULE' EQ '').SCHECK                                 00014000
         AIF   ('&MODULE' EQ '*').CSNAME                                00015000
         AIF   ('&ENTRY' EQ '').NENT01                                  00016000
&CHARFLD SETC  '&ENTRY'                                                 00017000
         AGO   .ENT01                                                   00018000
.NENT01  ANOP                                                           00019000
&CHARFLD SETC  '&MODULE'                                                00020000
.ENT01   ANOP                                                           00021000
&COUNTER SETA  1                                                        00022000
.CHARCNT AIF   ('&CHARFLD'(1,&COUNTER) EQ '&CHARFLD').CNTEND            00023000
&COUNTER SETA &COUNTER+1                                                00024000
         AGO   .CHARCNT                                                 00025000
.CNTEND  ANOP                                                           00026000
&ROUND   SETA  ((&COUNTER+2)/2)*2+4                                     00027000
         AIF   ('&PLICO' NE 'YES').NPLI01                               00028000
         AIF   ('&MODULE' EQ '*').NPLI01                                00029000
&ROUND   SETA  7-&COUNTER                                               00030000
         AIF   (&ROUND LE 0).PLI01                                      00031000
         DC    CL&ROUND.' ' PAD MODULE NAME ON LEFT WITH BLANKS         00032000
.PLI01   ANOP                                                           00033000
         AIF   ('&ENTRY' EQ '').NENT02                                  00034000
         DC    C'&ENTRY' MODULE NAME FOR CORE DUMP IDENTIFICATION       00035000
         DC    AL1(&COUNTER.) LENGTH OF ENTRY NAME                      00036000
         ENTRY &ENTRY                                                   00037000
         USING *,15                                                     00038000
&ENTRY   DS    0H                                                       00039000
         AGO   .SCHECK                                                  00040000
.NENT02  ANOP                                                           00041000
         DC    C'&MODULE' MODULE NAME FOR CORE DUMP IDENTIFICATION      00042000
         DC    AL1(&COUNTER.) LENGTH OF MODULE NAME                     00043000
         ENTRY &MODULE                                                  00044000
         USING *,15                                                     00045000
&MODULE  DS    0H                                                       00046000
         AGO   .SCHECK                                                  00047000
.NPLI01  ANOP                                                           00048000
&INTNAME B     *+&ROUND .BRANCH AROUND ID                               00049000
         DC    AL1(&COUNTER) .     ID LENGTH                            00050000
         DC    CL&COUNTER'&CHARFLD' .ID                                 00051000
&INTNAME SETC  ''                  YES                                  00052000
         AGO   .SCHECK                                                  00053000
.CSNAME  AIF   ('&SYSECT' EQ '').E1                                     00054000
&CHARFLD SETC  '&SYSECT'                                                00055000
&COUNTER SETA  1                                                        00056000
         AGO   .CHARCNT                                                 00057000
.*       NOW CHECK FOR REG'S TO BE SAVED                                00058000
.*                                                                      00059000
.*       IF CHECKOUT/OPTIMIZER PROLOGUE HAS BEEN REQUESTED THEN A       00060000
.*   STANDARD REGISTER SAVING CONVENTION MUST BE FOLLOWED.              00061000
.*                                                                      00062000
.SCHECK  AIF   ('&PLICO' NE 'YES').SCHECK2                              00063000
         STM   14,11,12(13) SAVE CALLER'S REGISTERS IN CALLER'S DSA     00064000
&SAVE1   SETB  1           SHOW THAT R1 WAS SAVED                       00065000
         AGO   .SCHAIN                                                  00066000
.SCHECK2 AIF   ('&SAVE' EQ '').SCHAIN   REGS TO BE SAVED?               00067000
.SNAMEOK AIF   (T'&SAVE(1) NE 'N').E2   REGS MUST BE SELF-DEFINING      00068000
&COUNTER SETA  &SAVE(1)*4+20                                            00069000
         AIF   (&COUNTER LE 75).SCHK01                                  00070000
&COUNTER SETA  &COUNTER-64                                              00071000
.SCHK01  AIF   (N'&SAVE NE 2).SCHK02                                    00072000
&INTNAME STM   &SAVE(1),&SAVE(2),&COUNTER.(&SAVEPTR) .SAVE REQ'D REG'S  00073000
&SAVE1   SETB  (&SAVE(1) LE 1 AND &SAVE(2) GE 1)                        00074000
&SAVE1   SETB  (&SAVE(1) GT 1 AND &SAVE(2) GE 1)                        00075000
&INTNAME SETC  ''                                                       00076000
         AGO   .SCHAIN                                                  00077000
.SCHK02  AIF   (N'&SAVE NE 1).E3                                        00078000
&INTNAME ST    &SAVE(1),&COUNTER.(&SAVEPTR,0) .SAVE REQ'D REGISTER      00079000
&SAVE1   SETB  (&SAVE(1) EQ 1) SHOW THAT R1 WAS SAVED                   00080000
&INTNAME SETC  ''                                                       00081000
.*                                                                      00082000
.*       NOW WE CHAIN THE SAVE AREAS                                    00083000
.*                                                                      00084000
.SCHAIN  ANOP                                                           00085000
         AIF   ('&ENTRY' EQ '').NENT03                                  00086000
         AIF   ('&MODULE' EQ '').E8                                     00087000
&INTNAME L     &BASE(1),=A(&MODULE.) SET UP BASE ENTRY ADDRESS          00088000
         AGO   .ENT03                                                   00089000
.NENT03  ANOP                                                           00090000
&INTNAME LR    &BASE(1),15 .        ESTABLISH ADDRESSIBILITY            00091000
         AIF   ('&PLICO' NE 'YES').NPLI02                               00092000
         AIF   ('&MODULE' EQ '' OR '&MODULE' EQ '*').NPLI02             00093000
.ENT03   ANOP                                                           00094000
         USING &MODULE,&BASE(1)                                         00095000
         AGO   .PLI02                                                   00096000
.NPLI02  ANOP                                                           00097000
         USING &SYSECT,&BASE(1)                                         00098000
.PLI02   ANOP                                                           00099000
         SPACE 1                                                        00100000
         AIF   (N'&BASE LE 1).BASETST                                   00101000
&BASENO  SETA  2                                                        00102000
&BDISP   SETA  4096                                                     00103000
.BASELP1 ANOP                                                           00104000
         LA    &BASE(&BASENO),4095(&BASE(&BASENO-1))                    00105000
         LA    &BASE(&BASENO),1(&BASE(&BASENO))                         00106000
         AIF   ('&PLICO' NE 'YES').NPLI03                               00107000
         AIF   ('&MODULE' EQ '' OR '&MODULE' EQ '*').NPLI03             00108000
         USING &MODULE+&BDISP,&BASE(&BASENO)                            00109000
         AGO   .PLI03                                                   00110000
.NPLI03  ANOP                                                           00111000
         USING &SYSECT+&BDISP,&BASE(&BASENO)                            00112000
.PLI03   ANOP                                                           00113000
         SPACE 1                                                        00114000
&BASENO  SETA  &BASENO+1                                                00115000
&BDISP   SETA  &BDISP+4096                                              00116000
         AIF   (&BASENO LE N'&BASE).BASELP1                             00117000
.BASETST AIF   ('&MACBASE' NE '15').NOTB15F                             00118000
         AIF   ('&MODULE' EQ '').NOTB15F                                00119000
         DROP  15 .DROP TEMPORARY BASE                                  00120000
.NOTB15F ANOP                                                           00121000
&INTNAME SETC  ''                                                       00122000
         AIF   ('&PL1' EQ 'YES').PL1GEN  PL/1 (F) PROLOGUE REQUIRED     00123000
.*                                                                      00124000
         AIF   ('&PLICO' EQ 'YES').PLIGEN CHECKER/OPTIMIZER PROLOGUE    00125000
.*                                                                      00126000
*        NOW CHAIN THE SAVE AREAS                                       00127000
         SPACE 1                                                        00128000
         AIF   ('&RENT' EQ '').SCHAIN1                                  00129000
         AIF   ('&RENT' NE 'YES').E6    'YES' IS THE ONLY VALID PARM    00130000
         SPACE 1                                                        00131000
*        OBTAIN A DYNAMIC SAVE AREA TO RETAIN RE-ENTRABILITY            00132000
         SPACE 1                                                        00133000
         GETMAIN    R,LV=&DSALTH . GET A DYNAMIC SAVE AREA (DSA)        00134000
         SPACE 1                                                        00135000
         XC    0(&DSALTH,1),0(1) . CLEAR DSA TO ZEROES                  00136000
         SPACE 1                                                        00137000
         ST    &SAVEPTR,4(1) .     SAVE PTR TO OLD SAVE AREA IN NEW     00138000
         ST    1,8(0,&SAVEPTR) .   STORE PTR TO NEW SAVE AREA IN OLD    00139000
         LR    &SAVEPTR,1 .        SET POINTER TO NEW SAVE AREA         00140000
         SPACE 1                                                        00141000
         AGO   .EXIT                                                    00142000
.SCHAIN1 ANOP                                                           00143000
         ST    &SAVEPTR,&SAVAR+4 . SAVE PTR TO SAVE AREA                00144000
         LA    &WKREG,&SAVAR .     PICK UP PTR TO NEW SAVE AREA         00145000
         ST    &WKREG,8(0,&SAVEPTR) .STORE PTR TO NEW AREA IN OLD AREA  00146000
         LR    &SAVEPTR,&WKREG .   SET SAVE PTR TO NEW SAVE AREA        00147000
         SPACE 1                                                        00148000
.*                                                                      00149000
.*       IF PL1=YES HAS BEEN CODED WE MUST GENERATE THE APPROPRIATE     00150000
.*   PL/1 PROLOGUE FOR ERROR HANDLING                                   00151000
.*                                                                      00152000
.PL1GEN  ANOP                                                           00153000
         AIF   ('&PL1' EQ 'NO').EXIT                                    00154000
         AIF   ('&PL1' NE 'YES').E4                                     00155000
         AIF   ('&SYSECT' EQ '').E5                                     00156000
&CHARFLD SETC  '&SYSECT' .    COUNT NUMBER OF CHARACTERS IN CSECT NAME  00157000
&COUNTER SETA  1                                                        00158000
.CSCOUNT AIF   ('&CHARFLD'(1,&COUNTER) EQ '&CHARFLD').CSCHECK           00159000
&COUNTER SETA  &COUNTER+1                                               00160000
         AGO   .CSCOUNT                                                 00161000
.CSCHECK AIF   (&COUNTER GT 7).E7  CSECT NAME > 7 CHARACTERS            00162000
&INTNAME L     15,MLMA&SYSNDX .    SET UP DSA AND CHAIN IT TO OTHERS    00163000
         LA    0,&DSALTH .         LOAD UP LENGTH OF REQ'D DSA          00164000
         BALR  14,15 .             LINK TO IHESADA - PL/1 STORAGE MGT.  00165000
&CHARFLD.B DXD A              SYMBOLIC OFFSET OF PSEUDO REGISTER        00166000
         MVI   0(13),X'80' .       SET DSA FLAG                         00167000
         ST    13,0(12) .          STORE DSA ADDRESS IN PSEUDO REGISTER 00168000
         ORG   *-2                                                      00169000
         DC    QL2(&SYSECT.B) .PLACES PR OFFSET IN ST INSTRUCTION ABOVE 00170000
         CNOP  0,4 .         ALIGN TO FULL WORD BOUNDARY                00171000
         B     *+8 .         BRANCH OVER ADCON                          00172000
MLMA&SYSNDX DC   V(IHESADA) .     ADCON POINTING TO GET DSA ROUTINE     00173000
&INTNAME SETC  ''                                                       00174000
         AGO   .EXIT                                                    00175000
.*                                                                      00176000
.*        THE FOLLOWING CODE WILL GENERATE A PL/1 CHECKOUT/OPTIMIZER    00177000
.*   COMPATABLE PROLOGUE, UTILIZING THE LIFO STACK TO OBTAIN DSA CORE   00178000
.*                                                                      00179000
.PLIGEN  ANOP                                                           00180000
         LA    0,&DSALTH .  R0 = LENGTH OF REQUIRED DSA                 00181000
         L     1,76(13) .   R1 = ADDRESS OF FIRST AVAILABLE BYTE IN     00182000
*                                THE LIFO STACK                         00183000
         ALR   0,1 .        R0 = PTR TO END OF REQUIRED DSA IF IT       00184000
*                                FIT IN LIFO STORAGE                    00185000
         CL    0,12(12) .   COMPARE RESULT WITH ADDRESS OF LAST         00186000
*                           AVAILABLE BYTE IN LIFO STORAGE              00187000
         BNH   *+10 .       BRANCH IF REQUIRED DSA STORAGE IS AVAILABLE 00188000
*                           IN THE LIFO STACK                           00189000
         L     15,116(12) . R15 = PTR TO PL/I STORAGE OVERFLOW ROUTINE  00190000
         BALR  14,15 .      BRANCH OUT TO GETMAIN MORE STORAGE          00191000
         ST    0,76(1) .    UPDATE ADDRESS OF NEXT AVAILABLE BYTE IN    00192000
*                           LIFO STACK                                  00193000
         ST    13,4(1) .    STORE PTR TO CALLER'S DSA IN OUR DSA        00194000
         MVC   72(4,1),72(13) COPY PTR TO LIBRARY WORKSPACE             00195000
         LR    13,1 .       SET UP PTR TO OUR NEW DSA                   00196000
         MVI   0(13),X'80' .SET DSA FLAGS TO PRESERVE PL/I              00197000
         MVI   86(13),X'91' ERROR-HANDLING WHILE EXECUTING              00198000
         MVI   87(13),X'C0' IN THIS ASSEMBLER ROUTINE.                  00199000
*                                                                       00200000
*        E N D    O F    P L / I    P R O L O G U E                     00201000
*                                                                       00202000
.*                                                                      00203000
.EXIT    ANOP                      NORMAL MACRO EXIT                    00204000
         AIF   (NOT &SAVE1).EXIT2    BRANCH IF R1 WAS NOT SAVED         00205000
         AIF   ('&PARM' NE 'YES').EXIT2 BRANCH IF NOT 'PARM=YES'        00206000
         L     1,4(13) .           R1 = PTR TO CALLERS' SAVE AREA       00207000
         L     1,24(1) .           RESTORE R1 TO POINT TO PARMS         00208000
.*                                                                      00209000
.EXIT2   MEXIT                                                          00210000
.*                                                                      00211000
.*       ERROR MESSAGES                                                 00212000
.*                                                                      00213000
.E1      MNOTE 12,'NO CSECT NAME SPECIFIED AND ''*'' CODED'             00214000
         MEXIT                                                          00215000
.E2      MNOTE 12,'INVALID REGISTER SPECIFICATION IN SAVE RANGE'        00216000
         MEXIT                                                          00217000
.E3      MNOTE 12,'INVALID RANGE OF REGISTERS TO BE SAVED'              00218000
         MEXIT                                                          00219000
.E4      MNOTE 12,'INVALID ''PL1='' SPECIFICATION'                      00220000
         MEXIT                                                          00221000
.E5      MNOTE 12,'PL1=YES CODED BUT NO CSECT NAME PRESENT'             00222000
         MEXIT                                                          00223000
.E6      MNOTE 12,'INVALID ''RENT='' SPECIFICATION'                     00224000
         MEXIT                                                          00225000
.E7      MNOTE 12,'PL1=YES CODED BUT CSECT NAME > 7 CHARACTERS'         00226000
         MEXIT                                                          00227000
.E8      MNOTE 12,'''ENTRY='' OPERAND CODED BUT ''MODULE='' NOT SPECIFIX00228000
               ED'                                                      00229000
         MEXIT                                                          00230000
.*                                                                      00231000
         MEND                                                           00232000
         TITLE 'ROUTINE TO CREATE OVERLAY CARDS FOR THE LINKAGE EDITOR' 00000000
ZZROVCRD CSECT                                                          00000010
         EQUREGS                        REGISTER EQUATES                00000020
         EJECT                                                          00000030
         CHAIN BASE=12                  SET UP ADDRESSABILITY           00000040
         EJECT                                                          00000050
         MVC   SEGBFBEG(4),0(R1)        BEGINNING ADDRESS OF SEGTAB     00000060
         MVC   SEGUSED(4),4(R1)         DYNAMIC END OF SEGTAB           00000070
         MVC   CESDADR(4),8(R1)         BEGINNING OF CESD TABLE         00000080
         MVC   CESDUSED(4),12(R1)       DYNAMIC END OF CESD TABLE       00000090
         MVC   ADCARNUM(4),16(R1)       ADDR OF CARD ACC FIELD          00000100
         MVC   ADCRDOUT(4),20(R1)       ADDR OF CARD OUT DCB            00000110
         EJECT                                                          00000120
         L     R3,SEGBFBEG              R3 -> SEGTAB                    00000130
         LA    R3,24(0,R3)              R3 -> 1ST LINE                  00000140
         LA    R4,4                     INDEX                           00000150
         L     R5,SEGUSED               PTR TO DYNAMIC END OF TABLE     00000160
         SR    R5,R4                    -4 FOR BXLE END ADDRESS         00000170
         SR    R6,R6                    CLEAR COUNTER                   00000180
         EJECT                                                          00000190
STSEGNO  DS    0H                                                       00000200
         LA    R6,1(0,R6)               ADD 1 TO COUNTER                00000210
         STC   R6,3(0,R3)               STORE SEG # IN APPROPRIATE LINE 00000220
         BXLE  R3,R4,STSEGNO            PROCESS ALL SEGTAB LINES        00000230
         MVC   0(2,R3),=X'FFFF'         DESIGNATES END OF TABLE         00000240
         LA    R1,4                     INDEX                           00000250
         MVC   RGCNT(2),=H'1'           INITIALIZE REGION COUNT         00000260
         NI    INSRTSW,X'00'            TURN SWITCH OFF                 00000270
         LA    R8,4                     R8 = 4                          00000280
         L     R11,SEGBFBEG             R11 -> SEGTAB                   00000290
         LR    R10,R11                  R10 -> SEGTAB                   00000300
         LA    R11,8(0,R11)             ADDR OF HGHEST SEG # IN REG     00000310
         LA    R10,24(0,R10)            R10 -> 1ST LINE IN SEGTAB       00000320
         EJECT                                                          00000330
PRCRGNS  DS    0H                                                       00000340
         ST    R1,R1SVE                 SAVE INDEX REGISTER             00000350
         SR    R0,R0                    CLEAR R0                        00000360
         IC    R0,0(0,R11)              GET HIGHEST SEGMENT #           00000370
         LTR   R0,R0                    IS IT ZERO?                     00000380
         BZ    ENDOVLY                  NO MORE CARDS TO CREATE         00000390
         BCTR  R0,0                     -1 TO GET INDEX TO LAST LINE    00000400
         SLL   R0,2                     X 4 BYTES PER ENTRY             00000410
         L     R9,SEGBFBEG              START OF SEGTAB                 00000420
         LA    R9,24(0,R9)              POINT TO FIRST LINE             00000430
         AR    R9,R0                    ADDR OF LAST LINE               00000440
PNCLOOP  DS    0H                                                       00000450
         L     R4,CESDADR               START OF CESD TABLE             00000460
         LA    R6,16                    INDEX                           00000470
         L     R7,CESDUSED              PTR TO DYNAMIC END OF CESD TAB  00000480
         SR    R7,R6                    -16 TO POINT TO END OF TABLE    00000490
NXTSEG   DS    0H                                                       00000500
         TM    9(R4),X'FF'              ENTRY PROCESSED?                00000510
         BO    BXLE1                    YES - GET NEW ENTRY             00000520
         CLC   12(1,R4),0(R11)          IN RANGE OF HIGHEST SEG #       00000530
         BH    BXLE1                    NO - DO NOT PROCESS             00000540
         CLC   12(1,R4),3(R10)          SEG # = # SEARCHING FOR         00000550
         BNE   BXLE1                    NO - DO NOT PROCESS             00000560
         CLC   0(8,R4),=C'$BLKCOM '     BLANK COMMON SYMBOL?            00000570
         BE    SETPRSW                  YES- SHOW PROCESSED             00000580
         CLC   0(8,R4),=C'$ENDTAB '     ENTAB SYMBOL?                   00000590
         BE    SETPRSW                  YES - SHOW PROCESSED            00000600
         CLC   0(8,R4),=C'$PRIVATE'     PRIVATE SYMBOL?                 00000610
         BE    SETPRSW                  YES - SHOW PROCESSED            00000620
         CLC   0(8,R4),=C'$SEGTAB '     SEGTAB SYMBOL?                  00000630
         BE    SETPRSW                  YES - SHOW PROCESSED            00000640
         MVC   OVLYCRD,OVLYCRD-1        BLANK OUT CARD                  00000650
         MVC   OVLYCRD+10(6),=C'INSERT' MOVE IN INSERT MESSAGE          00000660
         MVC   OVLYCRD+17(8),0(R4)       MOVE IN CSECT NAME             00000670
         OI    INSRTSW,X'FF'            SHOW INSERT FOR THIS SEGMENT    00000680
         BAL   R2,PUTCRD                PUT OUT THE CARD                00000690
SETPRSW  DS    0H                                                       00000700
         OI    9(R4),X'FF'              SHOW ITEM PROCESSED.            00000710
BXLE1    DS    0H                                                       00000720
         BXLE  R4,R6,NXTSEG             PROCESS NEXT SEGMENT            00000730
         EJECT                                                          00000740
         CLC   4(2,R10),=X'FFFF'        END OF SEG TABLE?               00000750
         BE    ENDOVLY                  YES - END                       00000760
         TM    INSRTSW,X'FF'            INSERT CARD PUNCHED FOR SEG?    00000770
         BZ    BXLE2                    NO - SKIP OVERLAY CARD          00000780
         LR    R2,R10                   R2 -> CURRENT LINE OF SEGTAB    00000790
         AR    R2,R8                    ADD INCREMENT                   00000800
         CR    R2,R9                    END OF SEBTAB?                  00000810
         BH    BXLE2                    YES - DONT GEN OVERLAY CARD     00000820
         NI    INSRTSW,X'00'            RESET SWITCH                    00000830
         MVC   OVLYCRD,OVLYCRD-1        BLANK OUT CARD                  00000840
         MVC   OVLYCRD+5(7),=C'OVERLAY' MOVE IN OVERLAY MESSAGE         00000850
         SR    R3,R3                    ZERO OUT R3                     00000860
         IC    R3,4(R10)                GET PREV SEGMENT #              00000870
         LTR   R3,R3                    SEG # = 0                       00000880
         BZ    USEREGID                 YES - USE REGION ID FOR OVERLAY 00000890
         CVD   R3,ADAREA                CONVERT TO DECIMAL              00000900
         OI    ADAREA+7,X'03'           MAKE POSITIVE                   00000910
         MVC   OVLYCRD+13(4),=C'OVLY'   MOVE IN OVLY VERBIAGE           00000920
         UNPK  OVLYCRD+17(3),ADAREA+6(2) UNPACK SEG #                   00000930
         BAL   R2,PUTCRD                PUT OUT THE CARD                00000940
         B     BXLE2                    ITERATE LOOP                    00000950
USEREGID DS    0H                                                       00000960
         LH    R3,RGCNT                 PUT REGION # INTO R3            00000970
         CVD   R3,ADAREA                CONVERT IT TO DECIMAL           00000980
         OI    ADAREA+7,X'03'           FORCE IT POSITIVE               00000990
         MVC   OVLYCRD+13(6),=C'REGION' MOVE IN VERBIAGE                00001000
         UNPK  OVLYCRD+19(1),ADAREA+7(1) UNPACK REGION #                00001010
         BAL   R2,PUTCRD                WRITE OUT THE CARD              00001020
BXLE2    DS    0H                                                       00001030
         BXLE  R10,R8,PNCLOOP           ITERATE LOOP                    00001040
         EJECT                                                          00001050
         LA    R11,2(0,R11)             INDEX TO NEXT LINE              00001060
         CLI   0(R11),X'00'             MORE REGIONS?                   00001070
         BE    ENDOVLY                  NO - END TASK                   00001080
         LH    R3,RGCNT                 GET COUNT                       00001090
         LA    R3,1(0,R3)               ADD 1 TO COUNT                  00001100
         CVD   R3,ADAREA                CONVERT TO DECIMAL              00001110
         OI    ADAREA+7,X'03'           FORCE POSITIVE                  00001120
         STH   R3,RGCNT                 SAVE THE COUNT                  00001130
         MVC   OVLYCRD,OVLYCRD-1        BLANK OUT CARD                  00001140
         MVC   OVLYCRD+9(6),=C'REGION'  MOVE IN REGION VERBIAGE         00001150
         UNPK  OVLYCRD+15(1),ADAREA+7(1) MOVE IN REGION #               00001160
         MVC   OVLYCRD+16(8),=C'(REGION)' MOVE IN VERBIAGE              00001170
         MVC   OVLYCRD+1(7),=C'OVERLAY' MOVE IN VERBIAGE                00001180
         BAL   R2,PUTCRD                PUT OUT THE CARD                00001190
         L     R1,R1SVE                 RETRIEVE REGION INDEX           00001200
         BCT   R1,PRCRGNS               PROCESS NEXT REGION             00001210
         B     ENDOVLY                  END TASK                        00001220
         EJECT                                                          00001230
PUTCRD   DS    0H                                                       00001240
         L     R1,ADCARNUM              R1 -> CARD COUNT                00001250
         AP    0(5,R1),=P'10'           UP THE CARD COUNT               00001260
         UNPK  OVLYCRD+72(8),0(5,R1)    NUMBER THE CARD                 00001270
         OI    OVLYCRD+79,X'F0'         ENSURE VALID CHARACTER          00001280
         L     R5,ADCRDOUT              ADDRESS OF DCB                  00001290
         PUT   (R5),OVLYCRD             PUT OUT CARD                    00001300
         BR    R2                       RETURN                          00001310
ENDOVLY  DS    0H                                                       00001320
         L     R13,4(R13)               RESTORE R13                     00001330
         RETURN (14,12),RC=0            RETURN                          00001340
         EJECT                                                          00001350
SAVE     DS    18F                                                      00001360
         DC    CL1' '                   USED TO BLANK OUT CARD          00001370
OVLYCRD  DS    CL80                     OVLY CARD WORK AREA             00001380
SEGBFBEG DS    F                                                        00001390
SEGUSED  DS    F                                                        00001400
CESDADR  DS    F                                                        00001410
CESDUSED DS    F                                                        00001420
ADCARNUM DS    F                                                        00001430
ADCRDOUT DS    F                                                        00001440
R1SVE    DS    F                                                        00001450
ADAREA   DC    1D'0'                                                    00001460
RGCNT    DC    1H'0'                                                    00001470
INSRTSW  DC    XL1'00'                                                  00001480
         LTORG                                                          00001490
         END                                                            00001500
//ASMZZREL EXEC PGM=IFOX00,PARM='OBJECT,NODECK',REGION=1000K            00430000
//SYSPRINT DD  SYSOUT=*                                                 00440000
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB                                 00450000
//         DD  DISP=SHR,DSN=SYS1.AMODGEN                                00460000
//SYSGO    DD  DSN=&&ZZRLOBJ,SPACE=(3040,(40,40),,,ROUND),UNIT=SYSDA,   00470000
//             DISP=(MOD,PASS),                                         00480000
//             DCB=(BLKSIZE=3040,LRECL=80,RECFM=FBS,BUFNO=1)            00490000
//SYSUT1   DD  SPACE=(TRK,(30,30)),UNIT=SYSDA                           00500000
//SYSUT2   DD  SPACE=(TRK,(30,30)),UNIT=SYSDA                           00510000
//SYSUT3   DD  SPACE=(TRK,(30,30)),UNIT=SYSDA                           00520000
//SYSPUNCH DD  DUMMY                                                    00530000
//SYSIN    DD  *                                                        00540000
         MACRO                                                          00000010
&TAG     EQUREGS                                                        00000020
         GBLB  &SW                                                      00000030
         LCLA  &N                                                       00000040
         LCLC  &R                                                       00000050
         AIF   (&SW).EXIT                                               00000060
&SW      SETB  1                                                        00000070
&R       SETC  'R'                                                      00000080
&N       SETA  0                                                        00000090
.LOOP    ANOP                                                           00000100
&R.&N    EQU   &N                                                       00000110
&N       SETA  &N+1                                                     00000120
         AIF   (&N LT 16).LOOP                                          00000130
.EXIT    MEXIT                                                          00000140
         MEND                                                           00000150
         MACRO                                                          00001000
         EQUBITS                                                        00002000
         GBLA  &EQUBITS                                                 00003000
         AIF   (&EQUBITS  EQ 0).EQB004                                  00004000
         MEXIT                                                          00005000
.EQB004  ANOP                                                           00006000
&EQUBITS SETA  1                                                        00007000
         SPACE 2                                                        00008000
*********************************************************************** 00009000
*                                  *                                  * 00010000
*                                  *    SYMBOLIC BIT DEFINITIONS      * 00011000
*                                  *                                  * 00012000
*********************************************************************** 00013000
         SPACE 2                                                        00014000
BIT0     EQU   128                                                      00015000
BIT1     EQU   64                                                       00016000
BIT2     EQU   32                                                       00017000
BIT3     EQU   16                                                       00018000
BIT4     EQU   8                                                        00019000
BIT5     EQU   4                                                        00020000
BIT6     EQU   2                                                        00021000
BIT7     EQU   1                                                        00022000
*********************************************************************** 00023000
*                                                                     * 00024000
*        E N D    O F    B I T    D E F I N I T I O N S               * 00025000
*                                                                     * 00026000
*********************************************************************** 00027000
         MEND                                                           00028000
         MACRO                                                          00001000
&NAME    CHAIN &BASE=10,&SAVE=(14,12),&SAVAR=SAVE,&PL1=NO,&SAVEPTR=13, X00002000
               &RENT=,&MACBASE=15,&MODULE=*,&DSALTH=100,&WKREG=14,     X00003000
               &PLICO=NO,&PARM=,&ENTRY=                                 00004000
         LCLC  &CHARFLD,&INTNAME                                        00005000
         LCLA  &COUNTER,&ROUND,&BASENO,&BDISP                           00006000
         LCLB  &SAVE1                                                   00007000
&INTNAME SETC  '&NAME'                                                  00008000
         AIF    ('&MACBASE' NE '15').NOTB15                             00009000
         AIF   ('&MODULE' EQ '').NOTB15                                 00010000
         AIF   ('&PLICO' EQ 'YES' AND '&MODULE' NE '*').NOTB15          00011000
         USING  *,15 .ESTABLISH TEMPORARY BASE REG                      00012000
.NOTB15  ANOP                                                           00013000
         AIF   ('&MODULE' EQ '').SCHECK                                 00014000
         AIF   ('&MODULE' EQ '*').CSNAME                                00015000
         AIF   ('&ENTRY' EQ '').NENT01                                  00016000
&CHARFLD SETC  '&ENTRY'                                                 00017000
         AGO   .ENT01                                                   00018000
.NENT01  ANOP                                                           00019000
&CHARFLD SETC  '&MODULE'                                                00020000
.ENT01   ANOP                                                           00021000
&COUNTER SETA  1                                                        00022000
.CHARCNT AIF   ('&CHARFLD'(1,&COUNTER) EQ '&CHARFLD').CNTEND            00023000
&COUNTER SETA &COUNTER+1                                                00024000
         AGO   .CHARCNT                                                 00025000
.CNTEND  ANOP                                                           00026000
&ROUND   SETA  ((&COUNTER+2)/2)*2+4                                     00027000
         AIF   ('&PLICO' NE 'YES').NPLI01                               00028000
         AIF   ('&MODULE' EQ '*').NPLI01                                00029000
&ROUND   SETA  7-&COUNTER                                               00030000
         AIF   (&ROUND LE 0).PLI01                                      00031000
         DC    CL&ROUND.' ' PAD MODULE NAME ON LEFT WITH BLANKS         00032000
.PLI01   ANOP                                                           00033000
         AIF   ('&ENTRY' EQ '').NENT02                                  00034000
         DC    C'&ENTRY' MODULE NAME FOR CORE DUMP IDENTIFICATION       00035000
         DC    AL1(&COUNTER.) LENGTH OF ENTRY NAME                      00036000
         ENTRY &ENTRY                                                   00037000
         USING *,15                                                     00038000
&ENTRY   DS    0H                                                       00039000
         AGO   .SCHECK                                                  00040000
.NENT02  ANOP                                                           00041000
         DC    C'&MODULE' MODULE NAME FOR CORE DUMP IDENTIFICATION      00042000
         DC    AL1(&COUNTER.) LENGTH OF MODULE NAME                     00043000
         ENTRY &MODULE                                                  00044000
         USING *,15                                                     00045000
&MODULE  DS    0H                                                       00046000
         AGO   .SCHECK                                                  00047000
.NPLI01  ANOP                                                           00048000
&INTNAME B     *+&ROUND .BRANCH AROUND ID                               00049000
         DC    AL1(&COUNTER) .     ID LENGTH                            00050000
         DC    CL&COUNTER'&CHARFLD' .ID                                 00051000
&INTNAME SETC  ''                  YES                                  00052000
         AGO   .SCHECK                                                  00053000
.CSNAME  AIF   ('&SYSECT' EQ '').E1                                     00054000
&CHARFLD SETC  '&SYSECT'                                                00055000
&COUNTER SETA  1                                                        00056000
         AGO   .CHARCNT                                                 00057000
.*       NOW CHECK FOR REG'S TO BE SAVED                                00058000
.*                                                                      00059000
.*       IF CHECKOUT/OPTIMIZER PROLOGUE HAS BEEN REQUESTED THEN A       00060000
.*   STANDARD REGISTER SAVING CONVENTION MUST BE FOLLOWED.              00061000
.*                                                                      00062000
.SCHECK  AIF   ('&PLICO' NE 'YES').SCHECK2                              00063000
         STM   14,11,12(13) SAVE CALLER'S REGISTERS IN CALLER'S DSA     00064000
&SAVE1   SETB  1           SHOW THAT R1 WAS SAVED                       00065000
         AGO   .SCHAIN                                                  00066000
.SCHECK2 AIF   ('&SAVE' EQ '').SCHAIN   REGS TO BE SAVED?               00067000
.SNAMEOK AIF   (T'&SAVE(1) NE 'N').E2   REGS MUST BE SELF-DEFINING      00068000
&COUNTER SETA  &SAVE(1)*4+20                                            00069000
         AIF   (&COUNTER LE 75).SCHK01                                  00070000
&COUNTER SETA  &COUNTER-64                                              00071000
.SCHK01  AIF   (N'&SAVE NE 2).SCHK02                                    00072000
&INTNAME STM   &SAVE(1),&SAVE(2),&COUNTER.(&SAVEPTR) .SAVE REQ'D REG'S  00073000
&SAVE1   SETB  (&SAVE(1) LE 1 AND &SAVE(2) GE 1)                        00074000
&SAVE1   SETB  (&SAVE(1) GT 1 AND &SAVE(2) GE 1)                        00075000
&INTNAME SETC  ''                                                       00076000
         AGO   .SCHAIN                                                  00077000
.SCHK02  AIF   (N'&SAVE NE 1).E3                                        00078000
&INTNAME ST    &SAVE(1),&COUNTER.(&SAVEPTR,0) .SAVE REQ'D REGISTER      00079000
&SAVE1   SETB  (&SAVE(1) EQ 1) SHOW THAT R1 WAS SAVED                   00080000
&INTNAME SETC  ''                                                       00081000
.*                                                                      00082000
.*       NOW WE CHAIN THE SAVE AREAS                                    00083000
.*                                                                      00084000
.SCHAIN  ANOP                                                           00085000
         AIF   ('&ENTRY' EQ '').NENT03                                  00086000
         AIF   ('&MODULE' EQ '').E8                                     00087000
&INTNAME L     &BASE(1),=A(&MODULE.) SET UP BASE ENTRY ADDRESS          00088000
         AGO   .ENT03                                                   00089000
.NENT03  ANOP                                                           00090000
&INTNAME LR    &BASE(1),15 .        ESTABLISH ADDRESSIBILITY            00091000
         AIF   ('&PLICO' NE 'YES').NPLI02                               00092000
         AIF   ('&MODULE' EQ '' OR '&MODULE' EQ '*').NPLI02             00093000
.ENT03   ANOP                                                           00094000
         USING &MODULE,&BASE(1)                                         00095000
         AGO   .PLI02                                                   00096000
.NPLI02  ANOP                                                           00097000
         USING &SYSECT,&BASE(1)                                         00098000
.PLI02   ANOP                                                           00099000
         SPACE 1                                                        00100000
         AIF   (N'&BASE LE 1).BASETST                                   00101000
&BASENO  SETA  2                                                        00102000
&BDISP   SETA  4096                                                     00103000
.BASELP1 ANOP                                                           00104000
         LA    &BASE(&BASENO),4095(&BASE(&BASENO-1))                    00105000
         LA    &BASE(&BASENO),1(&BASE(&BASENO))                         00106000
         AIF   ('&PLICO' NE 'YES').NPLI03                               00107000
         AIF   ('&MODULE' EQ '' OR '&MODULE' EQ '*').NPLI03             00108000
         USING &MODULE+&BDISP,&BASE(&BASENO)                            00109000
         AGO   .PLI03                                                   00110000
.NPLI03  ANOP                                                           00111000
         USING &SYSECT+&BDISP,&BASE(&BASENO)                            00112000
.PLI03   ANOP                                                           00113000
         SPACE 1                                                        00114000
&BASENO  SETA  &BASENO+1                                                00115000
&BDISP   SETA  &BDISP+4096                                              00116000
         AIF   (&BASENO LE N'&BASE).BASELP1                             00117000
.BASETST AIF   ('&MACBASE' NE '15').NOTB15F                             00118000
         AIF   ('&MODULE' EQ '').NOTB15F                                00119000
         DROP  15 .DROP TEMPORARY BASE                                  00120000
.NOTB15F ANOP                                                           00121000
&INTNAME SETC  ''                                                       00122000
         AIF   ('&PL1' EQ 'YES').PL1GEN  PL/1 (F) PROLOGUE REQUIRED     00123000
.*                                                                      00124000
         AIF   ('&PLICO' EQ 'YES').PLIGEN CHECKER/OPTIMIZER PROLOGUE    00125000
.*                                                                      00126000
*        NOW CHAIN THE SAVE AREAS                                       00127000
         SPACE 1                                                        00128000
         AIF   ('&RENT' EQ '').SCHAIN1                                  00129000
         AIF   ('&RENT' NE 'YES').E6    'YES' IS THE ONLY VALID PARM    00130000
         SPACE 1                                                        00131000
*        OBTAIN A DYNAMIC SAVE AREA TO RETAIN RE-ENTRABILITY            00132000
         SPACE 1                                                        00133000
         GETMAIN    R,LV=&DSALTH . GET A DYNAMIC SAVE AREA (DSA)        00134000
         SPACE 1                                                        00135000
         XC    0(&DSALTH,1),0(1) . CLEAR DSA TO ZEROES                  00136000
         SPACE 1                                                        00137000
         ST    &SAVEPTR,4(1) .     SAVE PTR TO OLD SAVE AREA IN NEW     00138000
         ST    1,8(0,&SAVEPTR) .   STORE PTR TO NEW SAVE AREA IN OLD    00139000
         LR    &SAVEPTR,1 .        SET POINTER TO NEW SAVE AREA         00140000
         SPACE 1                                                        00141000
         AGO   .EXIT                                                    00142000
.SCHAIN1 ANOP                                                           00143000
         ST    &SAVEPTR,&SAVAR+4 . SAVE PTR TO SAVE AREA                00144000
         LA    &WKREG,&SAVAR .     PICK UP PTR TO NEW SAVE AREA         00145000
         ST    &WKREG,8(0,&SAVEPTR) .STORE PTR TO NEW AREA IN OLD AREA  00146000
         LR    &SAVEPTR,&WKREG .   SET SAVE PTR TO NEW SAVE AREA        00147000
         SPACE 1                                                        00148000
.*                                                                      00149000
.*       IF PL1=YES HAS BEEN CODED WE MUST GENERATE THE APPROPRIATE     00150000
.*   PL/1 PROLOGUE FOR ERROR HANDLING                                   00151000
.*                                                                      00152000
.PL1GEN  ANOP                                                           00153000
         AIF   ('&PL1' EQ 'NO').EXIT                                    00154000
         AIF   ('&PL1' NE 'YES').E4                                     00155000
         AIF   ('&SYSECT' EQ '').E5                                     00156000
&CHARFLD SETC  '&SYSECT' .    COUNT NUMBER OF CHARACTERS IN CSECT NAME  00157000
&COUNTER SETA  1                                                        00158000
.CSCOUNT AIF   ('&CHARFLD'(1,&COUNTER) EQ '&CHARFLD').CSCHECK           00159000
&COUNTER SETA  &COUNTER+1                                               00160000
         AGO   .CSCOUNT                                                 00161000
.CSCHECK AIF   (&COUNTER GT 7).E7  CSECT NAME > 7 CHARACTERS            00162000
&INTNAME L     15,MLMA&SYSNDX .    SET UP DSA AND CHAIN IT TO OTHERS    00163000
         LA    0,&DSALTH .         LOAD UP LENGTH OF REQ'D DSA          00164000
         BALR  14,15 .             LINK TO IHESADA - PL/1 STORAGE MGT.  00165000
&CHARFLD.B DXD A              SYMBOLIC OFFSET OF PSEUDO REGISTER        00166000
         MVI   0(13),X'80' .       SET DSA FLAG                         00167000
         ST    13,0(12) .          STORE DSA ADDRESS IN PSEUDO REGISTER 00168000
         ORG   *-2                                                      00169000
         DC    QL2(&SYSECT.B) .PLACES PR OFFSET IN ST INSTRUCTION ABOVE 00170000
         CNOP  0,4 .         ALIGN TO FULL WORD BOUNDARY                00171000
         B     *+8 .         BRANCH OVER ADCON                          00172000
MLMA&SYSNDX DC   V(IHESADA) .     ADCON POINTING TO GET DSA ROUTINE     00173000
&INTNAME SETC  ''                                                       00174000
         AGO   .EXIT                                                    00175000
.*                                                                      00176000
.*        THE FOLLOWING CODE WILL GENERATE A PL/1 CHECKOUT/OPTIMIZER    00177000
.*   COMPATABLE PROLOGUE, UTILIZING THE LIFO STACK TO OBTAIN DSA CORE   00178000
.*                                                                      00179000
.PLIGEN  ANOP                                                           00180000
         LA    0,&DSALTH .  R0 = LENGTH OF REQUIRED DSA                 00181000
         L     1,76(13) .   R1 = ADDRESS OF FIRST AVAILABLE BYTE IN     00182000
*                                THE LIFO STACK                         00183000
         ALR   0,1 .        R0 = PTR TO END OF REQUIRED DSA IF IT       00184000
*                                FIT IN LIFO STORAGE                    00185000
         CL    0,12(12) .   COMPARE RESULT WITH ADDRESS OF LAST         00186000
*                           AVAILABLE BYTE IN LIFO STORAGE              00187000
         BNH   *+10 .       BRANCH IF REQUIRED DSA STORAGE IS AVAILABLE 00188000
*                           IN THE LIFO STACK                           00189000
         L     15,116(12) . R15 = PTR TO PL/I STORAGE OVERFLOW ROUTINE  00190000
         BALR  14,15 .      BRANCH OUT TO GETMAIN MORE STORAGE          00191000
         ST    0,76(1) .    UPDATE ADDRESS OF NEXT AVAILABLE BYTE IN    00192000
*                           LIFO STACK                                  00193000
         ST    13,4(1) .    STORE PTR TO CALLER'S DSA IN OUR DSA        00194000
         MVC   72(4,1),72(13) COPY PTR TO LIBRARY WORKSPACE             00195000
         LR    13,1 .       SET UP PTR TO OUR NEW DSA                   00196000
         MVI   0(13),X'80' .SET DSA FLAGS TO PRESERVE PL/I              00197000
         MVI   86(13),X'91' ERROR-HANDLING WHILE EXECUTING              00198000
         MVI   87(13),X'C0' IN THIS ASSEMBLER ROUTINE.                  00199000
*                                                                       00200000
*        E N D    O F    P L / I    P R O L O G U E                     00201000
*                                                                       00202000
.*                                                                      00203000
.EXIT    ANOP                      NORMAL MACRO EXIT                    00204000
         AIF   (NOT &SAVE1).EXIT2    BRANCH IF R1 WAS NOT SAVED         00205000
         AIF   ('&PARM' NE 'YES').EXIT2 BRANCH IF NOT 'PARM=YES'        00206000
         L     1,4(13) .           R1 = PTR TO CALLERS' SAVE AREA       00207000
         L     1,24(1) .           RESTORE R1 TO POINT TO PARMS         00208000
.*                                                                      00209000
.EXIT2   MEXIT                                                          00210000
.*                                                                      00211000
.*       ERROR MESSAGES                                                 00212000
.*                                                                      00213000
.E1      MNOTE 12,'NO CSECT NAME SPECIFIED AND ''*'' CODED'             00214000
         MEXIT                                                          00215000
.E2      MNOTE 12,'INVALID REGISTER SPECIFICATION IN SAVE RANGE'        00216000
         MEXIT                                                          00217000
.E3      MNOTE 12,'INVALID RANGE OF REGISTERS TO BE SAVED'              00218000
         MEXIT                                                          00219000
.E4      MNOTE 12,'INVALID ''PL1='' SPECIFICATION'                      00220000
         MEXIT                                                          00221000
.E5      MNOTE 12,'PL1=YES CODED BUT NO CSECT NAME PRESENT'             00222000
         MEXIT                                                          00223000
.E6      MNOTE 12,'INVALID ''RENT='' SPECIFICATION'                     00224000
         MEXIT                                                          00225000
.E7      MNOTE 12,'PL1=YES CODED BUT CSECT NAME > 7 CHARACTERS'         00226000
         MEXIT                                                          00227000
.E8      MNOTE 12,'''ENTRY='' OPERAND CODED BUT ''MODULE='' NOT SPECIFIX00228000
               ED'                                                      00229000
         MEXIT                                                          00230000
.*                                                                      00231000
         MEND                                                           00232000
         MACRO                                                          00003290
&LBL     $SETRC &CODE                                                   00003300
&LBL     CLI   RC,&CODE .               IS THIS CODE > CURRENT RC       00003310
         BNL   MLMA&SYSNDX .            BRANCH IF CURRENT RC NOT LOW    00003320
         MVI   RC,&CODE .               UPDATE TO NEW SEVERITY LEVEL    00003330
MLMA&SYSNDX DS    0H .                                                  00003340
         MEND                                                           00003350
         MACRO                                                          00000660
&LABEL   BINSRCH &TBEG=(3),&TEND=(4),&AEND=,&BUILD=NO,&ENTLTH=,        X00000670
               &ARG=(2),&SEQ=A,&COMPARE=L,&ARGOF=0,&FULL=,&TMID=(1),   X00000680
               &ARGLTH=,&FOUND=,&ENTOF=0                                00000690
.********************************************************************** 00000700
.*       MACRO PROTOTYPE SYMBOL MEANINGS AS FOLLOWS                   * 00000710
.********************************************************************** 00000720
.*             AEND = PTR TO ABSOLUTE TABLE END (BUILD FUNCTION ONLY)   00000730
.*                                                                      00000740
.*             ARG  = PTR TO ARGUMENT WHICH IS OBJECT OF SEARCH         00000750
.*                                                                      00000760
.*             ARGLTH = LENGTH OF ARGUMENT ON WHICH COMPARISON IS BASED 00000770
.*                                                                      00000780
.*             ARGOF = OFFSET OF ARGUMENT FROM THE START OF AN ENTRY    00000790
.*                                                                      00000800
.*             BUILD = INDICATION AS TO WHETHER THE SEARCH TABLE IS TO  00000810
.*                     BE EXPANDED IF THE SEARCH ARGUMENT IS NOT FOUND  00000820
.*                     IN THE EXISTING TABLE.                           00000830
.*                                                                      00000840
.*             COMPARE = INDICATION OF WHETHER THE SEARCH TABLE IS KEPT 00000850
.*                       IN ARITHMETIC OR COLLATING SEQUENCE ORDER.     00000860
.*                                                                      00000870
.*             ENTLTH = THE LENGTH OF A SINGLE TABLE ENTRY              00000880
.*                                                                      00000890
.*             TBEG = PTR TO THE START OF THE TABLE (MUST HAVE A DUMMY  00000900
.*                    ENTRY WITH X'0F' FOR 'COMPARE=A' OR X'FF' FOR     00000910
.*                    'COMPARE=L' IN THE ARGUMENT FIELD OF THE FIRST    00000920
.*                    ENTRY FOR THE 'BUILD=YES' FUNCTION).              00000930
.*                                                                      00000940
.*             TEND = PTR TO THE END OF THE TABLE (MUST HAVE THE DUMMY  00000950
.*                    ENTRY DESCRIBED ABOVE IN THE LAST ENTRY OF THE    00000960
.*                    TABLE IF 'BUILD=NO' IS IN EFFECT)                 00000970
.********************************************************************** 00000980
         LCLB  &BS(7)              1 = TBEG IN REG NOTATION             00000990
.*                                 2 = TEND IN REG NOTATION             00001000
.*                                 3 = AEND IN REG NOTATION             00001010
.*                                 4 = ARG IN REG NOTATION              00001020
.*                                 5 = DESCENDING ORDER TABLE           00001030
.*                                 6 = ARITHMETIC TABLE SEARCH          00001040
.*                                 7 = FOUND ROUTINE ADDRESS PRESENT    00001050
.********************************************************************** 00001060
.*       CHECK VALIDIITY OF MACRO OPERANDS                            * 00001070
.********************************************************************** 00001080
         AIF   (T'&TBEG EQ 'O').E1    ERROR IF NOT PRESENT              00001090
&BS(1)   SETB  ('&TBEG'(1,1) EQ '(')  TBEG IN REG NOTATION              00001100
.********************************************************************** 00001110
         AIF   (T'&TEND EQ 'O').E2    ERROR IF NOT PRESENT              00001120
&BS(2)   SETB  ('&TEND'(1,1) EQ '(')  TEND IN REG NOTATION              00001130
.********************************************************************** 00001140
         AIF   ('&BUILD' EQ 'NO').BI004 TABLE BUILD REQUESTED           00001150
         AIF   ('&BUILD' NE 'YES').E3   ERROR IF NOT 'YES' OR 'NO'      00001160
.********************************************************************** 00001170
         AIF   (T'&AEND EQ 'O').E4    ERROR IF ABSOLUTE END NOT         00001180
.*                                    SPECIFIED AND BUILD = 'YES'       00001190
&BS(3)   SETB  ('&AEND'(1,1) EQ '(')  AEND IN REGISTER NOTATION         00001200
.********************************************************************** 00001210
         AIF   (T'&FULL EQ 'O').E5    ERROR IF TABLE FULL ROUTINE PTR   00001220
.*                                    NOT SPECIFIED WITH BUILD = 'YES'  00001230
.BI004   ANOP                                                           00001240
.********************************************************************** 00001250
         AIF   (T'&ENTLTH NE 'N').E6  ERROR IF ENTRY LENGTH NOT SELF    00001260
.*                                    DEFINING                          00001270
.********************************************************************** 00001280
         AIF   (T'&ARG EQ 'O').E7     ERROR IF ARG NOT SPECIFIED        00001290
&BS(4)   SETB  ('&ARG'(1,1) EQ '(')   ARG IN REG NOTATION               00001300
.********************************************************************** 00001310
         AIF   ('&TMID'(1,1) NE '(').E8 ERROR IF TMID NOT A REGISTER    00001320
.********************************************************************** 00001330
         AIF   ('&SEQ' NE 'A' AND '&SEQ' NE 'D').E9 ASCENDING           00001340
.*                                     AND DESCENDING ORDER ONLY        00001350
&BS(5)   SETB  ('&SEQ' EQ 'D')        SET SEQUENCE TO DESCENDING        00001360
.********************************************************************** 00001370
         AIF   ('&COMPARE' NE 'L' AND '&COMPARE' NE 'A').E10 ERROR      00001380
.*                                    IF NOT LOGICAL OR ARITHMETIC      00001390
.*                                    COMPARE                           00001400
&BS(6)   SETB  ('&COMPARE' EQ 'A')    SET COMPARE TYPE TO ARITHMETIC    00001410
.********************************************************************** 00001420
         AIF   (T'&ARGOF NE 'N').E11  ERROR IF NOT SELF-DEFINING        00001430
.********************************************************************** 00001440
         AIF   (T'&ENTOF NE 'N').E12  ERROR IF NOT SELF-DEFINING        00001450
.********************************************************************** 00001460
         AIF   (T'&ARGLTH EQ 'O').E13 ERROR IF NO ARGUMENT LENGTH       00001470
         AIF   (T'&ARGLTH NE 'N').E14  ERROR IF NOT SELF-DEFINING       00001480
.********************************************************************** 00001490
&BS(7)   SETB  (T'&FOUND NE 'O')      FOUND ROUTINE PTR GIVEN           00001500
.********************************************************************** 00001510
*********************************************************************** 00001520
*        THE FOLLOWING CODE IS A BINARY SEARCH ROUTINE. THIS ROUTINE  * 00001530
         AIF   ('&BUILD' EQ 'YES').BI008                                00001540
*    WILL SEARCH THE TABLE POINTED TO BY 'TBEG' FOR THE ARGUMENT AT   * 00001550
*    'ARG'.                                                           * 00001560
         AGO   .BI012                                                   00001570
.BI008   ANOP                                                           00001580
*    WILL SEARCH THE EXISTING TABLE BEGINNING AT 'TBEG' AND ENDING AT * 00001590
*    'TEND' FOR THE ARGUMENT AT 'ARG'. IF THE SPECIFIED ARGUMENT IS   * 00001600
*    NOT FOUND IN THHE TABLE THEN ALL ENTRIES IN THE TABLE WILL BE    * 00001610
*    PROPAGATED UP, SPACE PERMITTING, THE TABLE TO MAKE ROOM FOR A    * 00001620
*    NEW TABLE ENTRY. IN THIS CASE 'TBEG' WILL BE SET TO POINT TO THE * 00001630
*    POSITION IN THE TABLE WHERE THE NEW ENTRY CAN BE PLACED.         * 00001640
.BI012   ANOP                                                           00001650
*********************************************************************** 00001660
         AIF   ('&LABEL' EQ '').BI016 BRANCH IF LABEL NOT CODED         00001670
&LABEL   DS    0H .                                                     00001680
.BI016   ANOP                                                           00001690
.********************************************************************** 00001700
         STM   14,12,12(13) .             SAVE REGISTERS                00001710
         AIF   (NOT &BS(1)).BI020         BRANCH IF SYMBOLIC            00001720
         AIF   ('&TBEG'(1,3) EQ '(3)').BI024 ALREADY IN R1              00001730
         LR    3,&TBEG .         SET PTR TO TABLE START                 00001740
         AGO  .BI024                                                    00001750
.BI020   ANOP                                                           00001760
         L     3,&TBEG .         SET PTR TO TABLE START                 00001770
.BI024   ANOP                                                           00001780
         AIF   (NOT &BS(2)).BI028        BRANCH IF SYMBOLIC             00001790
         AIF   ('&TEND'(1,3) EQ '(4)').BI032 ALREADY IN R4              00001800
         LR    5,&TEND .         SET PTR TO TABLE END                   00001810
         AGO   .BI032                                                   00001820
.BI028   ANOP                                                           00001830
         L     4,&TEND .         SET PTR TO TABLE END                   00001840
.BI032   ANOP                                                           00001850
         AIF   (NOT &BS(4)).BI036 IN SYMBOLIC NOTATION                  00001860
         AIF   ('&ARG'(1,3) EQ '(2)').BI040 ALREADY IN R2               00001870
         LR    2,&ARG .          SET PTR TO SEARCH ARGUMENT             00001880
         AGO   .BI040                                                   00001890
.BI036   ANOP                                                           00001900
         LA    2,&ARG .          SET PTR TO SEARCH ARGUMENT             00001910
.BI040   ANOP                                                           00001920
         AIF   (NOT &BS(2)).BI041          BRANCH IF SYMBOLIC           00001930
         LR    4,5                         R4 = PTR TO TABEND           00001940
.BI041   ANOP                                                           00001950
         AIF   (NOT &BS(3)).BI044 BRANCH IF NOT IN REG NOTATION         00001960
         LR    6,&AEND .         SAVE PTR TO ABSOLUTE TABLE END PTR     00001970
.BI044   ANOP                                                           00001980
         CLC   &ARGOF.(&ARGLTH,2),&ENTOF.(3) . NEW ENTRY FOR BEGINNING  00001990
         AIF   ('&BUILD' EQ 'NO').BI048    BRANCH IF SEARCH ONLY        00002000
         BL    MLMB&SYSNDX .               BRANCH IF YES                00002010
         AGO   .BI052                                                   00002020
.BI048   ANOP                                                           00002030
         BL    MLMG&SYSNDX .               BRANCH IF YES                00002040
.BI052   ANOP                                                           00002050
MLMA&SYSNDX   DS    0H .                                                00002060
         LR    1,4 .             TMID = PTR TO END POLE                 00002070
         SR    1,3 .             TMID = END POLE - BEGIN POLE           00002080
         SRL   1,5 .              TMID = (BEGIN-END)/(2 * ENTLTH)       00002090
         SLL   1,4 .              ROUND MID-PT OFFSET TO A MULTIPLE     00002100
*                                  OF THE LENGTH OF AN ENTRY            00002110
         AR    1,3 .             TMID = PTR TO MID ENTRY                00002120
         CLC   &ARGOF.(&ARGLTH,2),&ARGOF.(1) .CHECK FOR REQUIRED ENTRY  00002130
         BE    MLMF&SYSNDX .            BRANCH IF ENTRY FOUND           00002140
         CR    3,1 .             BEGIN = MID POLE                       00002150
.*                                                                      00002160
         AIF   ('&BUILD' EQ 'NO').BI064 BRANCH IF INSERTION NOT REQ'D   00002170
.*                                                                      00002180
         BE    MLMC&SYSNDX .       BRANCH IF YES -> NEW ENTRY REQUIRED  00002190
.*                                                                      00002200
         AGO   .BI068                                                   00002210
.*                                                                      00002220
.BI064   ANOP                                                           00002230
.*                                                                      00002240
         BE    MLMG&SYSNDX .        BRANCH IF YES -> NEW ENTRY REQUIRED 00002250
.*                                                                      00002260
.BI068  ANOP                                                            00002270
.*                                                                      00002280
         CLC &ARGOF.(&ARGLTH,2),&ENTOF.(1) COMPARE ARGUMENT TO ENTRY    00002290
*                                        MID POINT                      00002300
         BL    MLMD&SYSNDX .         BRANCH IF ARGUMENT IS LOW          00002310
*                                                                       00002320
         LR    3,1 .         NEW BEGIN POLE = CURRENT MID POLE          00002330
         B     MLMA&SYSNDX .        LOOK AT NEXT ENTRY                  00002340
MLMD&SYSNDX      DS    0H .                ARGUMENT COMPARED LOW        00002350
         LR    4,1 .           NEW END POLE = CURRENT MID POLE          00002360
         B     MLMA&SYSNDX .       LOOK AT NEXT ENTRY                   00002370
.*                                                                      00002380
         AIF   ('&BUILD' EQ 'NO').BI418  BRANCH IF NOT BUILD OPERATION  00002390
*                                                                       00002400
*        AT THIS POINT WE KNOW THAT A NEW ENTRY IS TO BE INSERTED INTO  00002410
*    THE TABLE.                                                         00002420
*                                                                       00002430
.*                                                                      00002440
MLMC&SYSNDX     DS    0H .                                              00002450
         LA    3,&ENTLTH.(3) .         ADJUST PTR FOR ENTRY INSERTION   00002460
.*                                                                      00002470
MLMB&SYSNDX     DS    0H .                                              00002480
.*                                                                      00002490
*                                                                       00002500
*     HERE TBEG POINTS TO THE TABLE LOCATION WHERE THE ENTRY IS TO BE   00002510
*    INSERTED. WE MUST, HOWEVER, CHECK TO MAKE SURE THERE IS ROOM FOR   00002520
*    BEFORE ATTEMPTING TO INSERT IT.                                    00002530
*                                                                       00002540
         AIF   (&BS(2)).BI404     TEND IN REG NOTATION                  00002550
         L     4,&TEND .          GET PTR TO CURRENT END OF TABLE       00002560
         AGO   .BI408                                                   00002570
.BI404   ANOP                                                           00002580
         LR    4,5 .              GET PTR TO CURRENT TABLE END          00002590
.BI408   ANOP                                                           00002600
         AIF   (&BS(3)).BI412     AEND IN REG NOTATION                  00002610
         C     4,&AEND .         IS THERE ROOM FOR ANOTHER ENTRY        00002620
         AGO   .BI416                                                   00002630
.BI412   ANOP                                                           00002640
         CR    4,6               IS THERE ROOM FOR ANOTHER ENTRY        00002650
.BI416   ANOP                                                           00002660
         BE    &FULL .        BRANCH IF NOT                             00002670
         LA    1,&ENTLTH.(4) .         UPDATE POINTER TO                00002680
         ST    1,TABEND .            NEW TABLE END                      00002690
         LA    1,&ENTLTH .             TMID = ENTRY LENGTH              00002700
MLME&SYSNDX   DS    0H .                                                00002710
*                                                                       00002720
*        IN ORDER TO MAKE ROOM FOR THE NEW ENTRY WE MUST NOW SHIFT ALL  00002730
*    EXISTING TABLE ENTRIES UP ONE ENTRY IN THE TABLE FROM THE POINT OF 00002740
*    INSERTION TO THE END OF THE TABLE.                                 00002750
*                                                                       00002760
         MVC   &ENTLTH.(&ENTLTH,4),0(4) . MOVE AN ENTRY UP              00002770
*                                                 ONE TABLE SLOT        00002780
         SR    4,1 .             BACK UP ONE ENTRY                      00002790
         CR    4,3 .             HAVE WE MOVED ALL ENTRIES YET          00002800
         BNL   MLME&SYSNDX .       BRANCH IF NOT                        00002810
         LM    5,6,40(13) .        RESTORE REGS                         00002820
         B     MLMG&SYSNDX .       EXIT                                 00002830
.BI418   ANOP                                                           00002840
MLMF&SYSNDX DS    0H .                                                  00002850
         LM    5,6,40(13) .        RESTORE REGISTERS                    00002860
         AIF   (&BS(7)).BI420      BRANCH IF FOUND ROUTINE PRESENT      00002870
         SR    15,15 .             SET ZERO CONDITION CODE              00002880
         LTR   15,15 .             SET HARDWARE CODE                    00002890
         AGO   .BI500                                                   00002900
.BI420   ANOP                                                           00002910
         B     &FOUND .            GO TO FOUND ROUTINE                  00002920
.BI500   ANOP                                                           00002930
MLMG&SYSNDX DS 0H .                                                     00002940
         AIF   ('&BUILD' EQ 'YES').EXIT                                 00002950
         LM    5,6,40(13) .        RESTORE REGS                         00002960
         MEXIT                                                          00002970
.E1      MLMERR 16,1,DESC=TBEG TBEG NOT SPECIFIED                       00002980
         MEXIT                                                          00002990
.E2      MLMERR 16,1,DESC=TEND TEND NOT SPECIFIED                       00003000
         MEXIT                                                          00003010
.E3      MLMERR 16,2,DESC=BUILD INVALID 'BUILD' SPECIFICATION           00003020
         MEXIT                                                          00003030
.E4      MLMERR 16,1,DESC=AEND AEND NOT SPECIFIED BUT REQUIRED          00003040
         MEXIT                                                          00003050
.E5      MLMERR 16,1,DESC=FULL FULL REQUIRED FOR 'BUILD' FUNCTION       00003060
         MEXIT                                                          00003070
.E6      MNOTE  16,'&ENTLTH. IS NOT SELF-DEFINING'                      00003080
         MEXIT                                                          00003090
.E7      MLMERR 16,1,DESC=ARG  ARG OPERAND REQUIRED                     00003100
         MEXIT                                                          00003110
.E8      MNOTE  16,'TMID NOT SPECIFIED IN REGISTER NOTATION'            00003120
         MEXIT                                                          00003130
.E9      MLMERR 16,2,DESC=SEQ SEQUENCE OPERAND INVALID                  00003140
         MEXIT                                                          00003150
.E10     MLMERR 16,2,DESC=COMPARE COMPARE OPERAND INVALID               00003160
         MEXIT                                                          00003170
.E11     MNOTE  16,'ARGOF OPERAND NOT SELF-DEFINING'                    00003180
         MEXIT                                                          00003190
.E12     MNOTE  16,'ENTOF OPERAND NOT SELF-DEFINING'                    00003200
         MEXIT                                                          00003210
.E13     MLMERR 16,1,DESC=ARGLTH ARGLTH REQUIRED BUT NOT SPECIFIED      00003220
         MEXIT                                                          00003230
.E14     MNOTE  16,'ARGLTH OPERAND NOT SELF-DEFINING'                   00003240
         MEXIT                                                          00003250
.EXIT    MEXIT                                                          00003260
         MEND                                                       
RLNK     TITLE 'Z Z R E L I N K  -  ZZRELINK RE-LINK EDIT UTILITY '     00010000
*                                                                       00020000
*    AUTHOR:   J. SCULLION                                              00030000
*                                                                       00040000
*    DATED:    11 FEB 1973                                              00050000
*                                                                       00060000
*    FUNCTION: THIS IS THE MAINLINE CONTROL ROUTINE FOR THE LINK        00070000
*              UTILITY ROUTINE.                                         00080000
*                                                                       00090000
*              THE PURPOSE OF THIS UTILITY IS TO READ CONTROL CARDS     00100000
*              THAT ARE SIMILAR TO IEBCOPY CONTROL CARDS AND DO EITHER  00110000
*              OF:                                                      00120000
*                   1.   GENERATE AND PUNCH A LINKAGE EDITOR JCL STREAM 00130000
*               OR                                                      00140000
*                   2.   GENERATE A JCL STREAM AND INVOKE THE LINKAGE   00150000
*                        EDITOR                                         00160000
*                                                                       00170000
*              THIS MAINLINE MODULE WILL CONTROL THE ROUTINES THAT:     00180000
*             -VALIDATE THE CONTROL CARDS                               00190000
*             -READ PDS DIRECTORIES                                     00200000
*             -GENERATE MESSAGES                                        00210000
*             -GENERATE JCL STREAM                                      00220000
*                                                                       00230000
*              THIS ROUTINE IS SIMILAR TO IEBCOPY IN THAT IT WILL ALLOW 00240000
*                                                                       00250000
*                                                                       00260000
*                   ALL MEMBERS OF A PDS TO BE RELINKED                 00270000
*             OR    SELECTED MEMBERS OF A PDS TO BE RELINKED            00280000
*             OR    EXCLUDING CERTAIN MEMBERS OF A PDS FROM RELINKING   00290000
*                                                                       00300000
*         FORMAT OF CONTROL STATEMENTS ARE:                             00310000
*                                                                       00320000
*              LINK      INDD=          ,OUTDD=                         00330000
*              L         I=             ,O=                             00340000
*                                                                       00350000
*              SELECT    MEMBER=                                        00360000
*              S         M=                                             00370000
*                                                                       00380000
*              EXCLUDE   MEMBER=                                        00390000
*              E         M=                                             00400000
*                                                                       00410000
*         THE REPLACE OPTION CAN ALSO BE SPECIFIED. IT IS CODED IN      00420000
*     THE SAME MANNER AS THE OPTION IS USED IN IEBCOPY.                 00430000
*     IT CAN REPLACE SELECTED MEMBERS OF A DATA SET OR IT CAN REPLACE   00440000
*     THE WHOLE DATA SET. IF THE REPLACE OPTION IS NOT SPECIFIED, THE   00450000
*     LINK IF INVOKED WILL NOT REPLACE THE MEMBER IN THE NEW DATA SET   00460000
*     IF THE MEMBER IS PRESENT IN THE NEW DATA SET.                     00470000
*                                                                       00480000
*     SEE THE WRITE-UP IN THE STANDARDS MANUAL FOR THE JCL REQUIRED.    00490000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00500000
         EJECT                                                          00510000
*                                                                       00520000
*     THIS PROGRAM IS BUILT FROM FIVE MODULES:                          00530000
*                                                                       00540000
*     1.   ZZRELINK      MAIN LINE  AND  MAIN PROCESSOR                 00550000
*     2.   ZZRESCAN      CONTROL CARD SCAN ROUTINE.                     00560000
*     3.   ZZMESAGE      MESSAGE WRITER ROUTINE.                        00570000
*     4.   ZZROVBLD      ENTRY POINT DETERMINATION AND OVERLAY TABLE    00580000
*                        BUILD ROUTINE.                                 00590000
*     5.   ZZROVCRD      OVERLAY CARD GENERATOR ROUTINE.                00600000
*                                                                       00610000
*     ZZRELCOM IS THE COMMUNICATIONS AREA BETWEEN MODULES.              00620000
*                                                                       00630000
*                                                                       00640000
         EJECT                                                          00650000
         EQUREGS                                                        00660000
         EJECT                                                          00670000
         EQUBITS                                                        00680000
         TITLE 'ENVIRONMENT PRESERVATION ROUTINE'                       00690000
ZZRELINK CSECT                                                          00700000
*                                                                       00710000
*         SAVE REGISTERS AND ESTABLISH ADDRESSABILITY                   00720000
*                                                                       00730000
         CHAIN SAVE=(14,12),BASE=(8,10,11,12),SAVAR=SV1               $ 00740000
         TITLE 'ONE TIME INITIALIZATION ROUTINE'                        00750000
*                                                                       00760000
*         INITIAL PROCESSING I.E. OPENING OF FILES ETC.                 00770000
*                                                                       00780000
         MVI   SZPRMSW,X'00'            TURN PARM SW OFF                00790000
         MVI   LEUSED,X'00'             LINK-EDIT WAS USED            $ 00800000
         L     R1,0(R1)                 PTR T0 PARM BUFFER              00810000
         LH    R2,0(R1)                 PARM LENGTH                     00820000
         STH   R2,SZPRMLTH              SAVE PARM LENGTH FOR LATER      00830000
         LTR   R2,R2                    PARM PRESENT?                   00840000
         BNP   PSTPARM                  NO PARM PRESENT                 00850000
         BCTR  R2,0                     DECR FOR EXEC                   00860000
         EX    R2,MOVE3                 SAVE THE SIZE PARAMETER         00870000
         MVI   SZPRMSW,X'FF'            TURN PARM PRES SW ON            00880000
PSTPARM  DS    0H                                                       00890000
         OPEN  (PRTDCB,(OUTPUT))        OPEN SYSPRINT                   00900000
*                                                                       00910000
         TM    PRTDCB+48,X'10'          OPEN OK                         00920000
         BO    NXTOP2                   YES                             00930000
*                                                                       00940000
         MVC   NAMEFLD(8),=C'SYSPRINT'  SYSPRINT DD CARD MISSING        00950000
         OI    IOEF2,SPRNOPN            SHOW SYSPRINT OPEN FAILED       00960000
         MVI   MSGLIST,0                INITIALIZE MESSAGE LIST         00970000
         MVI   MSGLIST+1,BADPRINT       SET UP MESSAGE NUMBER           00980000
         BAL   R2,MSGWRT                GO WRITE MESSAGE                00990000
         MVI   RC,16                    SET RETURN CODE TO 16           01000000
         B     ZZRTERM                  GOTO END OF JOB                 01010000
*                                                                       01020000
NXTOP2   DS    0H                                                       01030000
         OPEN  (CARDCB,(INPUT))         OPEN SYSIN                      01040000
*                                                                       01050000
         TM    CARDCB+48,X'10'          OPEN OK                         01060000
         BO    NXTOP3                   YES                             01070000
*                                                                       01080000
         MVC   DDNMFLD(5),=C'SYSIN'     SYSIN OPEN ERROR                01090000
OPENFAIL DS    0H                                                       01100000
         MVI   DDNMDISP,26              NAME GOES IN +26                01110000
         MVI   MSGLIST,0                INITIALIZE MESSAGE LIST         01120000
         MVI   MSGLIST+1,OPENERRX       SHOW OPEN ERROR                 01130000
         BAL   R2,MSGWRTD               WRITE MESSAGE WITH DDNAME       01140000
         MVI   RC,16                    SET RETURN CODE TO 16           01150000
         B     ZZRTERM                  GOTO END OF JOB                 01160000
*                                                                       01170000
NXTOP3   DS    0H                                                       01180000
         BAL   R7,CHKFUNC               CHECK THE FUNCTION              01190000
         CLI   LESWCH,X'FF'             PUNCH REQUIRED                  01200000
         BE    NXTOP4                   NO                              01210000
         OPEN  (SYSPUNCH,(OUTPUT))      OPEN SYSPUNCH                   01220000
*                                                                       01230000
         TM    SYSPUNCH+48,X'10'        OPEN OK                         01240000
         BO    ENDOPN                   YES                             01250000
*                                                                       01260000
         MVC   DDNMFLD(8),=C'SYSPUNCH'  SHOW SYSPUNCH OPEN FAILED       01270000
         B     OPENFAIL                 PRODUCE MESSAGE AND TERMINATE   01280000
*                                                                       01290000
NXTOP4   DS    0H                                                       01300000
         CLI   UT2,X'FF'                SYSUT2 DD PRESENT               01310000
         BE    NXTOP5                                                   01320000
         MVC   DDNMFLD(6),=C'SYSUT2'    SHOW SYSUT2 OPEN FAILED         01330000
         BAL   R2,NODD                  PRODUCE NO DD CARD MESSAGE      01340000
         MVI   RC,16                    SET RETURN CODE TO 16           01350000
         B     ZZRTERM                  TERMINATE                       01360000
NXTOP5   DS    0H                                                       01370000
         CLI   SYSLOPR,X'FF'            SYSLOUT DD PRESENT?             01380000
         BE    NXTOP6                   YES                             01390000
         MVC   DDNMFLD(7),=C'SYSLOUT'   SHOW DD CARD MISSING            01400000
         BAL   R2,NODD                  PRODUCE NO DD CARD MESSAGE      01410000
         MVI   RC,16                    SET RETURN CODE TO 16           01420000
         B     ZZRTERM                  TERMINATE                       01430000
NXTOP6   DS    0H                                                       01440000
         CLI   UT1,X'FF'                SYSUT1 DD PRESENT?              01450000
         BE    ENDOPN                   YES                             01460000
         MVC   DDNMFLD(6),=C'SYSUT1'    SHOW SYSUT1 OPEN FAILED         01470000
         BAL   R2,NODD                  PRODUCE NO DD CARD MESSAGE      01480000
         MVI   RC,16                    SET RETURN CODE TO 16           01490000
         B     ZZRTERM                  TERMINATE                       01500000
ENDOPN   DS    0H                                                       01510000
         LA    R4,ZZRTERM               R4 = PTR TO EOJ ROUTINE         01520000
         ST    R4,VZZRTERM              SET UP COMM. PTR TO EOJ         01530000
         TITLE 'MAINLINE CONTROL ROUTINE'                               01540000
*                                                                       01550000
*         THE FOLLOWING CONTROLS THE OVERALL PROCESSING. IT CONTROLS    01560000
*    THE CONTROL CARD SCAN ROUTINE, TABLE BUILDING ROUTINE, MEMBER      01570000
*    PROCESSING ROUTINE                                                 01580000
*                                                                       01590000
         BAL   R7,INITLZ                SET UP VALUES FOR SCAN ROUTINE  01600000
BEGINLNK DS    0H                                                       01610000
         LA    R4,ZZRELCOM              ADDR COMM AREA                  01620000
         L     R15,VZZRSCN              GET ADDRESS OF ZZRESCAN         01630000
         BALR  R14,R15                  DO CONTROL CARD SCAN            01640000
         BAL   R7,ALIASSTR              BUILD ALIAS TABLE               01650000
         BAL   R7,MEMREAD               DO PROCESSING FOR DATA SET      01660000
         B     BEGINLNK                 REPEAT FOR NEXT OPERATION       01670000
         TITLE 'ZZRESCAN - WORK AREA INITIALIZATION ROUTINE'            01680000
*                                                                       01690000
*      SCAN ROUTINE INITIALIZATION                                      01700000
*                                                                       01710000
INITLZ   DS    0H                                                       01720000
         MVC   CARDCB+32(4),VZZRLEOF    SET UP EODAD EXIT PTR           01730000
         MVI   CPARAMSW,X'08'           FORCE FIRST READ                01740000
         MVI   CCSWITCH,X'01'           FIRST SCAN SWITCH               01750000
         L     R5,=A(TABCOR)            R5 = PTR TO TABLE WORK AREA     01760000
         ST    R5,SEBEGIN               START OF SETAB                  01770000
         ST    R5,SESTOP                END OF SETAB                    01780000
         ST    R5,INBEGIN               START OF INDD TABLE             01790000
         BR    R7                                                       01800000
         TITLE 'FUNCTION DETERMINATION ROUTINE'                         01810000
*                                                                       01820000
*    THE FOLLOWING DECIDES WHETHER THE FUNCTION WILL BE TO PUNCH        01830000
*    CARDS OR ELSE TO INVOKE THE LINKAGE EDITOR.                        01840000
*                                                                       01850000
*    CARDS WILL BE PUNCHED WHEN A //SYSPUNCH CARD IS PRESENT.           01860000
*                                                                       01870000
*    LINKAGE EDITOR WILL BE INVOKED WHEN THERE IS NO //SYSPUNCH CARD.   01880000
*                                                                       01890000
CHKFUNC  DS    0H                                                       01900000
         L     R1,16(0)                 CVT                             01910000
         L     R1,0(R1)                 TCBTCB                          01920000
         L     R1,4(R1)                 TCB                             01930000
         L     R1,12(R1)                TIOT                            01940000
         LA    R1,24(R1)                LOAD PAST TIOT HEADER           01950000
         ST    R1,ADRTIOT               SAVE TIOT ADDRESS               01960000
*                                                                       01970000
*    LOOP THROUGH TIOT TO SEE WHAT DD CARDS ARE PRESENT                 01980000
*                                                                       01990000
TIOTLP   DS    0H                                                       02000000
         CLI   0(R1),X'00'              LAST ENTRY?                     02010000
         BE    CHKRET                   YES                             02020000
         CLC   4(8,R1),=C'SYSPUNCH'     SYSPUNCH PRESENT?               02030000
         BNE   CONTSR1                  NOT FOUND YET                   02040000
         MVI   LESWCH,X'00'             TURN OFF L E FUNCTION           02050000
         B     TIOTINCR                 BUMP UP TO NEXT ENTRY           02060000
CONTSR1  DS    0H                                                       02070000
         CLC   4(6,R1),=C'SYSUT2'       SYSUT2 PRESENT                  02080000
         BNE   CONTSR2                  NOT FOUND YET                   02090000
         MVI   UT2,X'FF'                SYSUT2 FOUND                    02100000
         B     TIOTINCR                 BUMP UP TO NEXT ENTRY           02110000
CONTSR2  DS    0H                                                       02120000
         CLC   4(7,R1),=C'SYSLOUT'      SYSLOUT PRESENT                 02130000
         BNE   CONTSR3                  NOT FOUND YET                   02140000
         MVI   SYSLOPR,X'FF'            SYSLOUT FOUND                   02150000
         B     TIOTINCR                 BUMP UP TO NEXT ENTRY           02160000
CONTSR3  DS    0H                                                       02170000
         CLC   4(6,R1),=C'SYSUT3'       SYSUT3 PRESENT                  02180000
         BNE   CONTSR4                  BRANCH IF NOT SYSUT3            02190000
         MVI   UT3,X'FF'                SHOW SYSUT3 PRESENT             02200000
         B     TIOTINCR                 BUMP UP TO NEXT ENTRY           02210000
CONTSR4  DS    0H                                                       02220000
         CLC   4(6,R1),=C'SYSUT1'       SYSUT1 PRESENT?                 02230000
         BNE   TIOTINCR                 NO - CHECK NEXT TIOT ENTRY      02240000
         MVI   UT1,X'FF'                SHOW SYSUT1 PRESENT             02250000
TIOTINCR DS    0H                                                       02260000
         SR    R2,R2                    ZERO R2                         02270000
         IC    R2,0(R1)                 LENGTH OF DD ENTRY              02280000
         AR    R1,R2                    BUMPP UP TO NEXT ENTRY          02290000
         B     TIOTLP                   TEST NEXT ENTRY                 02300000
CHKRET   DS    0H                                                       02310000
         BR    R7                       RETURN                          02320000
         TITLE 'MEMBER READ ROUTINE - SELECT MEMBER FROM DIRECTORY'     02330000
*                                                                       02340000
*         THE FOLLOWING GETS A MEMBER AND THEN PASSES CONTROL TO THE    02350000
*    ROUTINE THAT CONTROLS THE PROCESSING OF IT.                        02360000
*                                                                       02370000
MEMREAD  DS    0H                                                       02380000
         BAL   R9,DDSRCH                VERIFY THAT INDD AND OUTDD PRES 02390000
         LR    R6,R5                    R5=R6 WILL FORCE A READ         02400000
         MVC   DDCB+33(3),AEODAM+1      END OF DIRECTORY EXIT           02410000
         XC    PRVATTRB(3),PRVATTRB     CLEAR ATTRIBUTE INDICATORS    $ 02420000
         ZAP   STEPNO,=P'0'             SET STEP NO. TO ZERO            02430000
         MVI   COPYSW,0                 RESET COPY SWITCH               02440000
         LA    R4,ZZRELCOM              PTR TO COMM AREA                02450000
         LA    R1,0                     CODE = 0 - CLOSE OPEN REQ       02460000
         L     R15,=V(ZZROVBLD)         PTR TO ROUTINE                  02470000
         BALR  R14,R15                  DO THE PROCESS                  02480000
         LTR   R15,R15                  TEST RETURN CODE                02490000
         BNZ   BEGINLNK                 NOT ZERO - OPEN FAILED          02500000
         CLOSE (OUTDCB)                                                 02510000
         MVC   OUTDCB+40(8),OUTNAME      DDNAME                         02520000
         OPEN  (OUTDCB)                                                 02530000
         TM    OUTDCB+48,X'10'                                          02540000
         BNO   DDNMDER1                 OPEN NOT SUCCESSFULL          $ 02550000
         RDJFCB (OUTDCB)                READ JFCB FOR ENQUE LOGIC     $ 02560000
         MVC   QENQOUT+2(6),JFCB+118    SAVE VOLSER FOR ENQUE         $ 02570000
         MVC   RENQOUT,JFCB             SAVE DSNAME FOR ENQUE         $ 02580000
         B     GETMEMBR                                               $ 02590000
DDNMDER1 EQU   *                                                      $ 02600000
*    OPEN WAS NOT SUCESSFUL, PUT OUT MESSAGE.                           02610000
         MVC   DDNMFLD(8),OUTNAME        SET UP DDNAME FOR MESSAGE      02620000
DIRERR1  DS    0H                                                       02630000
         MVI   MSGLIST+1,NODIR           SHOW MESSAGE TO WRITE          02640000
         LA    R2,BEGINLNK               SET RETURN ADDRESS             02650000
         MVI   DDNMDISP,39               DDNAME GOES AT +39             02660000
         $SETRC 8                      SET RETURN CODE TO 8             02670000
         B     MSGWRTD                   GO WRITE MESSAGE WITH DDNAME   02680000
GETMEMBR DS    0H                                                       02690000
         BAL   R9,MEMRDR                DO READ ROUTINE                 02700000
         TM    11(R6),X'80'             IS IT AN ALIAS                  02710000
         BO    MEMINCR                  YES - GET NEXT MEMBER           02720000
         MVC   MEMNAME(8),0(R6)         SAVE THE MEMBER NAME            02730000
         MVC   ADDRMAIN(3),8(R6)        SAVE TTR OF MEMBER              02740000
         IC    R4,11(R6)                PICK UP INDICATOR BYTE        $ 02750000
         SLL   R4,27                    SHIFT OFF THE                 $ 02760000
         SRL   R4,27                      UNWANTED BITS               $ 02770000
         CH    R4,=H'11'                IS LENGTH LESS THAN OR = 34   $ 02780000
         BNH   NOTAUTH                  YES - CANT HAVE AUTH CODE     $ 02790000
         MVI   SETSSISW,X'00'           CLEAR SETSSISW                $ 02800000
         CLI   30(R6),X'98'             DOES MEMBER HAVE SET SSI      $ 02810000
         BNE   *+26                     NO                            $ 02820000
         MVI   SETSSISW,X'FF'           SET SETSSI SW                 $ 02830000
         MVC   SETSSIBI,34(R6)          SAVE BINARY SETSSI INFO       $ 02840000
         CLI   39(R6),X'01'             IS PROGRAM AUTHORIZED         $ 02850000
         BE    *+16                     YES                           $ 02860000
         B     NOTAUTH                  NO                            $ 02870000
         CLI   34(R6),X'01'             IS NON SET SSI                $ 02880000
         BNE   NOTAUTH                    MOD AUTHORIZED              $ 02890000
         OI    20(R6),X'01'             YES- USE BIT 7 OF 1ST         $ 02900000
         B     *+8                        ATTRIBUTE BYTE FOR AUTH     $ 02910000
NOTAUTH  EQU   *                                                      $ 02920000
         NI    20(R6),X'FE'             TURN OFF AUTH BIT             $ 02930000
         MVC   ATTRIB(2),20(R6)         SAVE ATTRIBUTES                 02940000
         CH    R4,=H'10'                IS LENGTH LESS THAN OR = 32   $ 02950000
         BNH   *+14                     NO CANT HAVE AMODE/RMODE      $ 02960000
         MVC   ATTRIB+2(1),31(R6)       SAVE AMODE/RMODE INFO         $ 02970000
         B     *+8                                                    $ 02980000
         MVI   ATTRIB+2,X'00'           CLEAR AMODE/RMODE INFO        $ 02990000
         NI    ATTRIB,X'FD'             TURN OFF PAGE ALIGN BIT       $ 03000000
         CH    R4,=H'09'                IS LENGTH LESS THAN OR = 30   $ 03010000
         BNH   *+16                     NO - CANT HAVE PAGE ALIGNMENT $ 03020000
         TM    30(R6),X'20'             IS MODULE PAGE ALIGNED ?      $ 03030000
         BNO   *+8                      NO                            $ 03040000
         OI    ATTRIB,X'02'             YES USE BIT 6 OF FIRST        $ 03050000
*                                         ATTRIBUTE BYTE FOR PG ALIGN $ 03060000
         MVC   EPADDR(3),27(R6)         SAVE ENTRY POINT ADDRESS        03070000
         BAL   R14,MEMPRCSS             DO MEMBER PROCESSING          $ 03080000
         MVI   FIRSTAL+1,0              SET UP FOR FIRST ENTRY          03090000
MEMINCR  DS    0H                                                       03100000
         LA    R6,11(R6)                BUMP PAST CONSTANTS             03110000
         SR    R4,R4                    ZERO R4                         03120000
         NI    0(R6),X'1F'              TURN OFF UNWANTED BITS          03130000
         IC    R4,0(R6)                 GET LENGTH                      03140000
         SLA   R4,1                     *2 = # OF BYTES                 03150000
         LA    R6,1(R4,R6)              BUMP PAST USER DATA             03160000
         B     GETMEMBR                 GET NEXT MEMBER                 03170000
MEMRDR   DS    0H                                                       03180000
         CR    R6,R5                    END OF BLOCK                    03190000
         BNL   MREAD                    YES - GET NEXT BLOCK            03200000
MEMEND   DS    0H                                                       03210000
         CLC   0(8,R6),FFF              END OF DIRECTORY                03220000
         BE    MEMRET                   YES - RETURN TO MAINLINE        03230000
         BR    R9                       NO - RETURN TO PROCESSING       03240000
MREAD    DS    0H                                                       03250000
         READ  DDECB,SF,DDCB,DBLOCK                                     03260000
         CHECK DDECB                                                    03270000
         SR    R5,R5                    ZERO R5                         03280000
         AH    R5,DBLOCK                LENGTH OF BLOCK                 03290000
         LA    R6,DBLOCK                START OF BLOCK                  03300000
         AR    R5,R6                    END OF BLOCK                    03310000
         LA    R6,2(R6)                 START OF FIRST ENTRY            03320000
         B     MEMEND                   CHECK OUT THE ENTRY             03330000
MEMRET   DS    0H                                                       03340000
         CLOSE (SYSUT2)                 CLOSE SYSUT2                    03350000
         LH    R5,COUNT                 NO. OF ENTRIES IN SELECT TABLE  03360000
         LTR   R5,R5                    ANY ENTRIES IN TABLE            03370000
         BZ    NOSELECT                 NO - LINK ALL WAS REQUESTED     03380000
         LH    R5,ENCT                  R5 = NO. OF SELECT ENTRIES      03390000
         LTR   R5,R5                   ENTRIES IN SELECT TABLE          03400000
         BZ    NOSELECT                 NO - MUST HAVE BEEN EXCLUDES    03410000
         L     R5,SEBEGIN               R5 = PTR TO SELECT TABLE        03420000
CHKSEL   DS    0H                                                       03430000
         TM    0(R5),SEBIT5             WAS THIS MEMBER PROCESSED       03440000
         BO    SELNEXT                  YES - NO MESSAGE TO WRITE       03450000
         MVI   MSGLIST+1,SMNF           MEMBER SELECTED BUT NOT FOUND   03460000
         $SETRC 4                      SET RETURN CODE TO 4             03470000
         MVI   NAMEDISP,0               MEMBER NAME GOES AT +0          03480000
         MVC   NAMEFLD(8),2(R5)         MOVE IN MEMBER TO DISPLAY       03490000
         LA    R2,SELNEXT               SET RETURN ADDRESS              03500000
         B     MSGWRTN                  WRITE ERROR MESSAGE             03510000
SELNEXT  DS    0H                                                       03520000
         LA    R5,10(R5)                BUMP TO NEXT ENTRY              03530000
         C     R5,SESTOP                END OF TABLE ?                  03540000
         BL    CHKSEL                   NO - GET NEXT ENTRY             03550000
NOSELECT DS    0H                                                       03560000
         CLI   ALLSET,X'FF'             READY TO DO LINK?               03570000
         MVI   ALLSET,X'00'             RESET SWITCH                    03580000
         BNE   0(0,R7)                  RETURN - NO LINK REQ            03590000
         CLI   LESWCH,X'FF'             APPLICATION FUNCTION REQ        03600000
         BNE   PSTCLSE                  NO - DONT DO LINK               03610000
         BAL   R14,ENQOUT               ENQ THE FILE TO BE LINKED TO  $ 03620000
         LINK  EP=IEWL,PARAM=(OPLISTL,DDNMLST),VL=1                     03630000
         MVI   LEUSED,X'FF'             SHOW THAT LINK-EDIT WAS USED  $ 03640000
         BAL   R14,DEQOUT               DEQ THE FILE LINKED TO        $ 03650000
         BR    R7                       RETURN                          03660000
PSTCLSE  DS    0H                                                       03670000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  03680000
         MVC   CNTCRD(2),=C'/*'         MOVE IN DELIMITER               03690000
         PUT   SYSPUNCH,CNTCRD          PUT IT OUT                      03700000
         BR    R7                       RETURN TO MAINLINE              03710000
         TITLE 'MEMBER PROCESSING ROUTINE'                              03720000
*                                                                       03730000
*         THE FOLLOWING CONTROLS THE FLOW OF PROCESSING FOR EACH        03740000
*    MEMBER TESTING IF LINKS ARE REQUIRED AND WHAT INFORMATION TO GET   03750000
*                                                                       03760000
MEMPRCSS DS    0H                                                       03770000
         ST    R14,PROCRTRN             SAVE RETURN ADDRESS           $ 03780000
         BAL   R9,LNKREQ                TEST IF LINK IS REQUIRED        03790000
         CLI   MEMNAME,C' '             IS NAME BLANK                   03800000
         BE    PRCSSEND                 YES - RETURN LINK NOT REQ       03810000
         BAL   R9,VERPLC                REPLACE MEMBER ON OUTPUT?       03820000
         MVI   OVLYSW,X'FF'             INIT OVERLAY SW TO ON           03830000
         TM    ATTRIB,X'20'             OVERLAY PRESENT?                03840000
         BO    PATEST                   YES - LEAVE SW ON             $ 03850000
         MVI   OVLYSW,X'00'             NO OVERLAY - TURN SW OFF        03860000
PATEST   EQU   *                                                      $ 03870000
         TM    ATTRIB,X'02'             IS MOD MARKED PAGE ALIGN      $ 03880000
         LA    R9,PAMSG                 SET TO PRODUCE MESSAGE        $ 03890000
         MVC   NAMEFLD(8),MEMNAME                                     $ 03900000
         BNO   NETEST                                                 $ 03910000
         B     NOEDIT                                                 $ 03920000
NETEST   DS    0H                                                       03930000
         TM    ATTRIB+1,X'08'           IS MODULE MARKED N.E.           03940000
         LA    R9,NEMSG                 SET TO PRODUCE MESSAGE          03950000
         MVC   NAMEFLD(8),MEMNAME                                       03960000
         BO    NOEDIT                   BRANCH IF YES                   03970000
*                                                                       03980000
         BAL   R9,CESDSTRT              GET CSECT NAME OF ENTRY POINT   03990000
         CLI   CSCTNAME,C' '            IS NAME BLANK                   04000000
         BE    PRCSSEND                 YES - RETURN NAME NOT FOUND     04010000
*                                                                       04020000
         BAL   R9,PARMGEN               SET UP LINK OPTIONS             04030000
*                                                                       04040000
         CLI   LESWCH,X'FF'             INVOKE LINKAGE EDITOR           04050000
         BE    INVOKELE                 YES - SKIP PAST JCL GEN         04060000
         BAL   R9,JCLGENR               GENERATE THE JCL                04070000
         B     PRCSSEND                 RETURN                          04080000
INVOKELE DS    0H                                                       04090000
         CLI   ALLSET,X'FF'             EVERYTHING ALREADY SET UP       04100000
         BE    SYSUT2OP                 YES DONT DO AGAIN               04110000
         BAL   R9,GENDDLST              SET UP DDNAME LIST              04120000
         MVI   ALLSET,X'FF'             SET SET-UP SW = ON              04130000
         OPEN  (SYSUT2,OUTPUT)                                          04140000
         TM    SYSUT2+48,X'10'          OPEN OK                         04150000
         BO    SYSUT2OP                 YES - CONTINUE                  04160000
         MVC   DDNMFLD(6),=C'SYSUT2'    SHOW SYSUT2 OPEN FAILED         04170000
         B     OPENFAIL                 PRODUCE MESSAGE AND TERMINATE   04180000
SYSUT2OP DS    0H                                                       04190000
         BAL   R4,CONTGEN               GENERATE CONTROL CARDS          04200000
         B     PRCSSEND                 RETURN                          04210000
NOEDIT   DS    0H                                                       04220000
         CLI   UT3,X'FF'                SYSUT3 DD CARD PRESENT          04230000
         BNE   0(0,R9)                  NO - CAN'T DO COPY              04240000
         TM    SYSUT3+48,BIT3           IS SYSUT3 OPEN                  04250000
         BO    GENCOPY                  YES - GO GEN COPY CARD          04260000
         OPEN  (SYSUT3,OUTPUT)          OPEN IEBCOPY FILE               04270000
GENCOPY  DS    0H                                                       04280000
         CLI   COPYSW,X'FF'             COPY CARD PUNCHED YET           04290000
         BE    SELGEN                   BRANCH IF YES - NEED SELECT     04300000
         MVI   COPYSW,X'FF'             SHOW COPY CARD GEN'D            04310000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CONTROL CARD          04320000
         MVC   CNTCRD(L'COPYBASE),COPYBASE MOVE BASIC COPY TEXT         04330000
         L     R2,INBEGIN               POINT AT INDD TABLE             04340000
         MVC   CNTCRD+15(8),2(R2)       MOVE INPUT DDNAME TO CARD       04350000
         LA    R2,CNTCRD+16             SET PTR FOR SCAN                04360000
COPYSCAN DS    0H                                                       04370000
         CLI   0(R2),C' '               END OF INPUT DDNAME             04380000
         BE    COPYFND1                 BRANCH IF YES                   04390000
         LA    R2,1(R2)                 BUMP TO NEXT CHARACTER          04400000
         B     COPYSCAN                 CONTINUE SCAN                   04410000
COPYFND1 DS    0H                                                       04420000
         MVC   0(7,R2),=C',OUTDD='      MOVE IN OUTDD TEXT              04430000
         MVC   7(8,R2),OUTNAME          MOVE IN OUTPUT DDNAME           04440000
         PUT   SYSUT3,CNTCRD            WRITE OUT COPY CARD             04450000
SELGEN   DS    0H                                                       04460000
         MVC   CNTCRD,CNTCRD-1          CLEAR CONTROL AREA              04470000
         MVC   CNTCRD(L'SELECT),SELECT  MOVE BASIC SELECT TEXT          04480000
         CLI   REPLACE,X'FF'            REPLACE OPTION SPECIFIED        04490000
         BNE   NRCOPY                   BRANCH IF NOT                   04500000
         MVC   CNTCRD+27(2),=C'(('      SET UP REPLACE FORMAT           04510000
         MVC   CNTCRD+29(8),MEMNAME     MOVE IN MEMBER NAME             04520000
         LA    R2,CNTCRD+30             SET UP FOR SCAN                 04530000
SELSCAN  DS    0H                                                       04540000
         CLI   0(R2),C' '               END OF MEMBER NAME              04550000
         BE    SELFND1                  BRANCH IF YES                   04560000
         LA    R2,1(R2)                 BUMP UPP TO NEXT CHARACTER      04570000
         B     SELSCAN                  LOOK AT NEXT CHARACTER          04580000
SELFND1  DS    0H                                                       04590000
         MVC   0(5,R2),=C',,R))'        SET REPLACE OPTION              04600000
         B     PUTSEL                   PUNCH OUT SELECT CARD           04610000
NRCOPY   DS    0H                                                       04620000
         MVC   CNTCRD+27(8),MEMNAME     PLACE MEMBER NAME ON CARD       04630000
PUTSEL   DS    0H                                                       04640000
         PUT   SYSUT3,CNTCRD            PUNCH OUT  SELECT CARD          04650000
FIRSTAL  NOP   NFIRSTAL                 BRANCH IF NOT FIRST ENTRY       04660000
         LR    R15,R9                   SET FOR NO ALIAS RETURN         04670000
         BAL   R14,ALSCAN               GO SCAN ALIAS TABLE             04680000
         MVI   FIRSTAL+1,X'F0'          SHOW FIRST ALIAS DONE           04690000
         MVC   MEMNAME(8),3(R3)         SET UP ALIAS NAME AS MEMBER     04700000
         B     SELGEN                   GO PRODUCE SELECT CARD          04710000
NFIRSTAL DS    0H                                                       04720000
         SH    R3,=H'16'                DECREMENT TO NEXT ALIAS         04730000
         CLC   0(3,R3),16(R3)           IS THE TTR THE SAME             04740000
         BNE   0(0,R9)                  BRANCH IF NO - ALIAS COMPLETE   04750000
         MVC   MEMNAME(8),3(R3)         SET UP ALIAS NAME AS MEMBER     04760000
         B     SELGEN                   GO GENERATE SELECT CARD         04770000
PAMSG    EQU   *                                                      $ 04780000
NEMSG    EQU   *                                                      $ 04790000
         TM    ATTRIB,X'02'             IS MOD MARKED PAGE ALIGN      $ 04800000
         BNO   *+12                     NO                            $ 04810000
         MVI   MSGLIST+1,PA             SET PA MESSAGE                $ 04820000
         B     *+8                                                    $ 04830000
         MVI   MSGLIST+1,NE             SET NE MESSAGE                $ 04840000
         L     R14,PROCRTRN             RESTORE RETURN ADDRESS        $ 04850000
         LR    R2,R14                   SET RETURN ADDRESS            $ 04860000
         MVI   NAMEDISP,12              MODULE NAME GOES AT +12         04870000
         B     MSGWRTN                  GO WRITE MESSAGE                04880000
PRCSSEND DS    0H                                                       04890000
         L     R14,PROCRTRN             RESTORE RETURN ADDRESS        $ 04900000
         BR    R14                      RETURN                        $ 04910000
PROCRTRN DS    F                                                      $ 04920000
         TITLE 'REPLACE OPTION VERIFICATION ROUTINE'                    04930000
*                                                                       04940000
*    IF REPLACE OPTION WAS NOT SPECIFIED, THE MEMBER CAN NOT            04950000
*    BE PRESENT ON THE OUTPUT FILE. PRODUCE AN ERROR MESSAGE IN         04960000
*    THIS CASE.                                                         04970000
*                                                                       04980000
VERPLC   DS    0H                                                       04990000
         CLI   REPLACE,X'FF'            REPLACE OPTION?                 05000000
         BE    VERFRET                  YES - RETURN                    05010000
         MVC   BLDLIST+4(8),MEMNAME     NAME FOR BLDL                   05020000
         BLDL  OUTDCB,BLDLIST           BUILD LIST                      05030000
         LTR   R15,R15                  RETURN CODE ZERO?               05040000
         BNZ   VERFRET                  BRANCH IF MEMBER NOT FOUND      05050000
*   MEMBER IS PRESENT AND REPLACE OPTION WAS NOT SPECIFIED.             05060000
         MVC   NAMEFLD(8),MEMNAME       SET MEMBER NAME FOR MESSAGE     05070000
         MVI   NAMEDISP,0               MEMBER NAME GOES AT +0          05080000
         MVI   MSGLIST+1,REPERR01       SET UP NO RELACE OPT. MESSAGE   05090000
         LA    R2,PRCSSEND              RETURN ADDRESS                  05100000
         B     MSGWRTN                  WRITE MESSAGE WITH NAME         05110000
VERFRET  DS    0H                                                       05120000
         BR    R9                       RETURN                          05130000
         TITLE 'ALIAS TABLE BUILD ROUTINE'                              05140000
*                                                                       05150000
*         THE FOLLOWING WILL BUILD A TABLE OF TTR'S AND ALIAS NAMES     05160000
*    FOR ALL ALIASES IN THE DIRECTORY. MAX WAS INCREASED FROM ORIG.   $ 05170000
*    500 TO 601 AND LOGIC CHANGED TO USE GETMAIN FOR TABLE IN APR. 86 $ 05180000
*    AND INCREASED TO 2000 FOR MVS/ESA(OS390) MAR 97                  $ 05190000
*                                                                       05200000
*    FORMAT OF TABLE:    BYTES  1 -  3  TTR OF MAIN MEMBER NAME         05210000
*                        BYTES  4 - 11  ALIAS NAME                      05220000
*                                                                       05230000
ALIASSTR DS    0H                                                       05240000
         CLOSE (DDCB)                                                   05250000
         L     R3,INBEGIN                START OF TABLE                 05260000
         MVC   DDCB+40(8),2(R3)         DDNAME OF DATA SET              05270000
         MVC   DDCB+33(3),AEODAD+1      END OF DIRECTORY EXIT           05280000
         LM    R0,R1,TABLEN             PICK UP NO. OF ENTRIES IN     $ 05290000
*                                       THE ALIAS TABLE AND THE ADD.  $ 05300000
*                                       OF WORK AREA TO HOLD THE TAB. $ 05310000
         LTR   R1,R1                    DO WE ALREADY HAVE WORK AREA  $ 05320000
         BC    7,TABPOINT               YES - CONTINUE                $ 05330000
         SLL   R0,4                     CALCULATE THE SIZE OF TABLE   $ 05340000
*                                       BY MULT. NO. OF ENTRIES X 16  $ 05350000
         GETMAIN R,LV=(R0)              OBTAIN A WORK AREA            $ 05360000
         ST    R1,ATABS                 SAVE BEGINNING OF TABLE       $ 05370000
         ST    R1,TABEND                SAVE END OF TABLE             $ 05380000
         ST    R1,TABADD                SAVE ADDRESS OF TABLE         $ 05390000
         AR    R1,R0                    POINT TO END OF TABLE         $ 05400000
         SH    R1,=H'16'                BACK OFF ONE ENTRY            $ 05410000
         ST    R1,ALEND                 SAVE POINTER TO LAST ENTRY    $ 05420000
         SRL   R0,4                     RESTORE NO. OF ENTRIES        $ 05430000
         ST    R0,TABLEN                SAVE NO. OF ENTRIES           $ 05440000
TABPOINT EQU   *                                                      $ 05450000
         L     R4,TABADD                POINT R4 TO TABLE             $ 05460000
         MVI   0(R4),X'FF'              RESET 1ST TABLE ENTRY TO X'FF'  05470000
         MVC   1(15,R4),0(R4)                                           05480000
         ST    R4,TABEND                                                05490000
         MVI   JOBSW+1,X'00'            SET UP TO PUNCH JOB CARD        05500000
*                                       IF GENERATE FUNCTION            05510000
*                                                                       05520000
         OPEN  (DDCB)                                                   05530000
         TM    DDCB+48,X'10'            OPEN SUCESSFUL?                 05540000
         BNO   ALIASER1                 NO - ERROR SITUATION            05550000
         LR    R6,R5                    R5=R6 WILL FORCE A READ         05560000
ALIASBLD DS    0H                                                       05570000
         BAL   R9,DREAD                 DO READ IF REQUIRED.            05580000
         TM    11(R6),X'80'             ALIAS?                          05590000
         BZ    DIRINCR                  NO - GET NEXT ENTRY             05600000
         MVC   ALENTRY(3),8(R6)         TTR                             05610000
         MVC   ALENTRY+3(8),0(R6)       ALIAS NAME                      05620000
         LA    R4,ALENTRY               ADDRESS OF ENTRY FOR TABLE      05630000
         LM    R1,R2,ATABS              SET UP SEARCH TABLE             05640000
         BINSRCH ARG=(R4),FOUND=ALIASER2,BUILD=YES,AEND=ALEND,         X05650000
               ARGLTH=11,ENTLTH=16,FULL=ALIASER3,                      X05660000
               TBEG=(R1),TEND=(R2),TMID=(R3)                            05670000
         MVC   0(11,R3),ALENTRY         PUT ENTRY INTO TABLE            05680000
DIRINCR  DS    0H                                                       05690000
         LA    R6,11(R6)                BUMP PAST CONSTANTS             05700000
         SR    R4,R4                    ZERO R4                         05710000
         NI    0(R6),X'1F'              TURN OFF UNWANTED BITS          05720000
         IC    R4,0(R6)                 GET LENGTH                      05730000
         SLA   R4,1                     * 2 = # OF BYTES                05740000
         LA    R6,1(R4,R6)              BUMP PAST USER DATA             05750000
         B     ALIASBLD                 CHECK NEXT ENTRY                05760000
DREAD    DS    0H                                                       05770000
         CR    R6,R5                    END OF BLOCK                    05780000
         BNL   DIRDR                    YES - GET NEW BLOCK             05790000
DIREND   DS    0H                                                       05800000
         CLC   0(8,R6),FFF              END OF DIRECTORY                05810000
         BE    ALIASRET                 RETURN TO MAINLINE              05820000
         BR    R9                       RETURN TO BUILD                 05830000
DIRDR    DS    0H                                                       05840000
         READ  RDECB,SF,DDCB,DBLOCK                                     05850000
         CHECK RDECB                                                    05860000
         SR    R5,R5                    ZERO R5                         05870000
         AH    R5,DBLOCK                LENGTH OF BLOCK                 05880000
         LA    R6,DBLOCK                START OF BLOCK                  05890000
         AR    R5,R6                    END OF BLOCK                    05900000
         LA    R6,2(R6)                 START OF FIRST ENTRY            05910000
         B     DIREND                   CHECK OUT THE ENTRY             05920000
ALIASER1 DS    0H                                                       05930000
         L     R3,INBEGIN               START OF TABLE                  05940000
         MVC   DDNMFLD(8),2(R3)         PLACE DDNAME IN MESSAGE         05950000
         B     DIRERR1                  GO WRITE MESSAGE                05960000
ALIASER2 DS    0H                                                       05970000
         L     R3,INBEGIN               START OF TABLE                  05980000
         MVC   DDNMFLD(8),2(R3)         SET UP DDNAME FOR MESSAGE       05990000
         MVI   MSGLIST+1,DIRERR01       INDICATE MSG TO WRITE           06000000
         MVI   DDNMDISP,0               DDNAME GOES AT +0               06010000
         $SETRC 8                      SET RETURN CODE TO 8             06020000
         LA    R2,BEGINLNK              SET UP RETURN ADDRESS           06030000
         B     MSGWRTD                  WRITE MESSAGE WITH DDNAME       06040000
ALIASER3 DS    0H                                                       06050000
         MVI   MSGLIST,LASTMSG          SHOW LAST MESSAGE             $ 06060000
         MVI   MSGLIST+1,ATABFULL       SET UP MESSAGE TO WRITE         06070000
         $SETRC 8                      SET RETURN CODE TO 8             06080000
         LA    R2,BEGINLNK              SET RETURN ADDRESS              06090000
         B     MSGWRT                   WRITE MESSAGE - NO PARMS        06100000
ALIASRET DS    0H                                                       06110000
         CLOSE (DDCB,REREAD),TYPE=T                                     06120000
         BR    R7                       RETURN TO MAINLINE              06130000
         TITLE 'MEMBER SELECT VERIFICATION ROUTINE'                     06140000
*                                                                       06150000
*         THE FOLLOWING CHECKS TO SEE IF THE SELECTED MEMBER FROM THE   06160000
*    DIRECTORY MUST BE RELINKED.                                        06170000
*         IF THERE ARE NO SELECTS OR EXCLUDES, ALL MUST BE RELINKED.    06180000
*                                                                       06190000
*         IF THERE ARE SELECTS, ONLY THOSE SELECTED MUST BE RELINKED.   06200000
*                                                                       06210000
*         IF THERE ARE EXCLUDES, ALL BUT THOSE EXCLUDED MUST BE         06220000
*    RELINKED.                                                          06230000
*                                                                       06240000
LNKREQ   DS    0H                                                       06250000
         LH    R4,COUNT                 # ENTRIES IN SETAB              06260000
         CH    R4,=H'0'                 NO ENTRIES = FULL LINK          06270000
         L     R2,INBEGIN               START OF TABLE                  06280000
         BE    TSTRPLC                  BRANCH IF FULL LINK             06290000
         LH    R4,ENCT                  # OF EXCLUDE ENTRIES            06300000
         CH    R4,=H'0'                 IS IT EXCLUSIVE LINK            06310000
         L     R4,SEBEGIN               POINT TO FIRST ENTRY IN TABLE   06320000
         BNE   SELVCPY                  NO - IT IS SELECTIVE LINK       06330000
EXCLCPY  DS    0H                                                       06340000
         CLC   2(8,R4),MEMNAME          IS IT TO BE EXCLUDED            06350000
         BE    BLNKNME                  YES - BLANK IT OUT              06360000
         LA    R4,10(R4)                BUMP UP TO NEXT ENTRY           06370000
         C     R4,SESTOP                END OF TABLE                    06380000
         BNL   TSTRPLC                  YES - RELINK MEMBER             06390000
         B     EXCLCPY                  CHECK NEXT ENTRY                06400000
SELVCPY  DS    0H                                                       06410000
         LR    R2,R4                                                    06420000
         CLC   2(8,R4),MEMNAME          IS IT TO BE SELECTED            06430000
         BE    TSTRPLC                  YES - RETURN                    06440000
         LA    R4,10(R4)                BUMP UP TO NEXT ENTRY           06450000
         C     R4,SESTOP                END OF TABLE                    06460000
         BNL   BLNKNME                  YES - DO NOT LINK MEMBER        06470000
         B     SELVCPY                  CHECK NEXT ENTRY                06480000
TSTRPLC  DS    0H                                                       06490000
         OI    0(R2),SEBIT5             SHOW MEMBER FOUND ON INPUT DS.  06500000
         MVI   REPLACE,X'00'            INIT TO 0                       06510000
         TM    0(R2),REPLACOP           REPLACE?                        06520000
         BO    REPLCON                  YES                             06530000
         L     R2,INBEGIN               START OF INDD TABLE             06540000
         TM    0(R2),REPLACOP           REPLACE?                        06550000
         BNO   LNKREQRT                 NO                              06560000
REPLCON  DS    0H                                                       06570000
         MVI   REPLACE,X'FF'            YES                             06580000
         BR    R9                       RETURN                          06590000
BLNKNME  DS    0H                                                       06600000
         MVC   MEMNAME(8),BLNK8         BLANK OUT NAME                  06610000
LNKREQRT DS    0H                                                       06620000
         BR    R9                       RETURN                          06630000
         TITLE 'ENTRY POINT DETERMINATION ROUTINE'                      06640000
*                                                                       06650000
*         THE FOLLOWING WILL GET THE CSECT NAME OF THE ENTRY POINT FOR  06660000
*    THE MAIN MEMBER AND BUILD OVERLAY TABLES IF REQUIRED.              06670000
*                                                                       06680000
CESDSTRT DS    0H                                                       06690000
         LA    R4,ZZRELCOM              PTR TO COMM AREA                06700000
         MVC   WKA1(8),MEMNAME          MOVE MEMBER NAME TO COMM AREA   06710000
         MVC   WKA1+8(1),OVLYSW         OVERLAY SW                      06720000
         MVC   WKA1+9(3),EPADDR         ENTRY POINT ADDRESS             06730000
         MVC   WKA1+12(4),ADDRMAIN      MAIN MEMBER ADDRESS             06740000
         LA    R1,4                     CODE 4 - REQ TO BUILD TABLES    06750000
         L     R15,=V(ZZROVBLD)         PTR TO ROUTINE                  06760000
         BALR  R14,R15                  DO PROCESS                      06770000
         LTR   R15,R15                  TEST RETURN CODE                06780000
         BNZ   MEMINCR                  PROCESS FAILED                  06790000
         MVC   CSCTNAME(8),WKA1         SAVE CSECT NAME                 06800000
         TM    OVLYSW,X'FF'             OVERLAY MODULE?                 06810000
         BZ    CESDRET                  NO - CSECT NAME ONLY RETRIEVED  06820000
         MVC   SEGBFBEG(4),WKA1+8       START OF SEGTAB                 06830000
         MVC   SEGUSED(4),WKA1+12       END OF SEGTAB                   06840000
         MVC   CESDADR(4),WKA1+16       START OF CESD TABLE             06850000
         MVC   CESDUSED(4),WKA1+20      END OF CESD TABLE               06860000
CESDRET  DS    0H                                                       06870000
         BR    R9                       RETURN TO CALLER                06880000
         TITLE 'LINKAGE EDITOR CONTROL CARD BUILD ROUTINE'              06890000
*                                                                       06900000
*         THE FOLLOWING WRITES TO SYSUT2 THE CONTROL CARDS REQUIRED     06910000
*    FOR LINK EDITING; WHEN THE FUNCTION IS PUNCH, IT WRITES IT OUT TO  06920000
*    SYSPUNCH                                                           06930000
CONTGEN  DS    0H                                                       06940000
         ST    R4,SVREG4                SAVE R4                         06950000
         L     R9,INBEGIN               R9 = PTR TO INPUT DDNAME TBL    06960000
         MVC   INDCB+40(8),OUTNAME      SET UP DDNAME FOR RDJFCB        06970000
         RDJFCB (INDCB)                 READ THE JOB FILE CONTROL  BLK  06980000
*                                                                       06990000
         CLC    JFCB(11),=C'SYS1.SVCLIB' IS THIS SVCLIB                 07000000
         BNE    NOTSVCS                  BRANCH IF NOT SYS1.SVCLIB      07010000
         MVI    SVCLIB,X'FF'             SHOW SVCLIB WAS ALTERED        07020000
         MVC    SVCDDNM(8),OUTNAME       SAVE PTR TO SVCLIB             07030000
NOTSVCS  DS     0H                                                      07040000
         CLI   LESWCH,X'FF'             INVOKE LINKAGE EDITOR?          07050000
         BE    USEUT2                   YES - USE SYSUT2                07060000
         LA    R9,SYSPUNCH              NO - USE SYSPUNCH               07070000
         B     DDSETUP                  DD SET UP                       07080000
USEUT2   DS    0H                                                       07090000
         LA    R9,SYSUT2                USE SYSUT2                      07100000
DDSETUP  DS    0H                                                       07110000
         CLC   JFCB(13),=C'SYS1.NUCLEUS ' IS THIS THE NUCLEUS           07120000
         BNE   NOTNUC                   BRANCH IF NOT NUCLEUS D.S.      07130000
         MVC   CNTCRD,CNTCRD-1          CLEAR WORK AREA                 07140000
         MVC   CNTCRD+20(L'NUCINS01),NUCINS01                           07150000
         BAL   R1,NUMCARD               NUMBER THIS CARD                07160000
         PUT   (R9),CNTCRD              'INSERT IEAANIP0'               07170000
         MVC   CNTCRD+20(L'NUCINS02),NUCINS02                           07180000
         BAL   R1,NUMCARD               NUMBER THIS CARD                07190000
         PUT   (R9),CNTCRD              'INSERT IEAQFX00'               07200000
         MVC   CNTCRD+20(L'NUCINS03),NUCINS03                           07210000
         BAL   R1,NUMCARD               NUMBER THIS CARD                07220000
         MVI   CNTCRD+34,C' '           CLEAR EXTANEOUS BLANK           07230000
         PUT   (R9),CNTCRD              'INSERT DOSBLKS'                07240000
         MVC   CNTCRD+20(L'NUCINS04),NUCINS04                           07250000
         MVI   CNTCRD+33,C' '           CLEAR EXTRANEOUS CHAR           07260000
         PUT   (R9),CNTCRD              'INSERT IGFCCH'                 07270000
         B     ALCHECK                  NO ENTRY CARD FOR NUCLEUS       07280000
NOTNUC   DS    0H                                                       07290000
         CLC   CSCTNAME,=C'$PRIVATE'    DO WE BYPASS ENTRY CARD       $ 07300000
         BE    ALCHECK                  YES                           $ 07310000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  07320000
         MVC   CNTCRD+15(5),=C'ENTRY'   MOVE IN ENTRY                   07330000
         MVC   CNTCRD+25(8),CSCTNAME    MOVE IN ENTRY POINT NAME        07340000
         BAL   R1,NUMCARD               NUMBER THIS CARD                07350000
         PUT   (R9),CNTCRD              WRITE OUT RECORD                07360000
ALCHECK  DS    0H                                                       07370000
         LA    R14,PARMLOOP             SET UP FOUND ADDRESS            07380000
         LA    R15,PARMEND              SET UP NO ALIAS EXIT            07390000
ALSCAN   DS    0H                                                       07400000
         CLC   ATABS,TABEND             ANY ALIASES?                    07410000
         BE    0(0,R15)                 NO                              07420000
         LM    R1,R2,ATABS              ADDR OF START AND END OF TABLE  07430000
         LA    R4,ADDRMAIN              SEARCH ARGUMENT                 07440000
         BINSRCH ARG=(R4),FOUND=PARMFND,BUILD=NO,AEND=ALEND,           X07450000
               ARGLTH=3,ENTLTH=16,TBEG=(R1),TEND=(R2),TMID=(R3)         07460000
         BR    R15                      GO TO NO ALIAS EXIT             07470000
PARMFND  DS    0H                                                       07480000
PARMPOSL DS    0H                                                       07490000
         C     R1,TABEND                LAST ENTRY IN TABLE             07500000
         BE    PARMPOSE                 YES - R1 IS POSITIONED          07510000
         CLC   0(3,R1),16(R1)           CURR ENTRY = NEXT ENTRY         07520000
         BNE   PARMPOSE                 R1 PTS TO LAST ENTRY FOR MEM    07530000
         AH    R1,=H'16'                POS TO NEXT ENTRY               07540000
         B     PARMPOSL                 CHECK THIS ENTRY                07550000
PARMPOSE DS    0H                                                       07560000
         LR    R3,R1                    R3 PTS TO LAST ENTRY FOR MEM    07570000
         BR    R14                      EXIT TO CALLER WITH ALIAS PTR   07580000
PARMLOOP DS    0H                                                       07590000
*    POSITION R3 SO THAT IT POINTS TO THE LAST ALIAS OF THE GROUP FOR   07600000
*    THE MEMBER WE ARE PROCESSING. ALL THE ALIASES FOR A MEMBER ARE     07610000
*    GROUPED TOGETHER, AND AFTER THE BINARY SEARCH, R3 COULD POINT TO   07620000
*    TO THE START, MIDDLE, OR END OF THE GROUP. THE PRECEDING WILL      07630000
*    ENSURE THAT R3 IS ALWAYS POINTING TO THE END OF THE GROUP AND      07640000
*    THEREFORE ALLOWS THE ALIASES TO BE SELECTED FROM BOTTOM OF ARRAY   07650000
*    TO START OF ARRAY.                                                 07660000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  07670000
         MVC   CNTCRD+10(5),=C'ALIAS'   MOVE IN ALIAS                   07680000
         MVC   CNTCRD+20(8),3(R3)       MOVE IN ALIAS NAME              07690000
         BAL   R1,NUMCARD               NUMBER THIS CARD                07700000
         PUT   (R9),CNTCRD              WRITE OUT RECORD                07710000
         C     R3,ATABS                 FIRST ENTRY IN TABLE?           07720000
         BNH   PARMEND                  YES - GET OUT                   07730000
         SH    R3,=H'16'                DECREMENT TO NEXT ENTRY         07740000
         CLC   0(3,R3),16(R3)           IS NEXT ENTRY EQUAL TO CURRENT  07750000
         BE    PARMLOOP                 YES - PUT THIS ALIAS OUT        07760000
PARMEND  DS    0H                                                       07770000
         CLI   SETSSISW,X'FF'           DOES MOD HAVE SETSSI INFO     $ 07780000
         BNE   GENINCCD                 NO                            $ 07790000
         L     R3,SETSSIBI                                            $ 07800000
         LA    R4,SETSSIDP                                            $ 07810000
         LA    R1,8                                                   $ 07820000
CONHEX   EQU   *                                                      $ 07830000
         XR    R2,R2                    LOOP                          $ 07840000
         SLDL  R2,4                       THRU                        $ 07850000
         IC    R0,TRTAB(R2)                 CHANGING                  $ 07860000
         STC   R0,0(R4)                       SSI FROM                $ 07870000
         LA    R4,1(R4)                         HEXADECIMAL           $ 07880000
         BCT   R1,CONHEX                          TO ZONED            $ 07890000
         MVC   CNTCRD,CNTCRD-1          PUT OUT                       $ 07900000
         MVC   CNTCRD+8(6),=C'SETSSI'     SETSSI TO                   $ 07910000
         MVC   CNTCRD+18(8),SETSSIDP        CONTROL CARD              $ 07920000
         BAL   R1,NUMCARD               NUMBER THIS CARD              $ 07930000
         PUT   (R9),CNTCRD              WRITE OUT RECORD              $ 07940000
         MVI   SETSSISW,X'00'           RESET SW                      $ 07950000
GENINCCD EQU   *                                                      $ 07960000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  07970000
         MVC   CNTCRD+5(7),=C'INCLUDE'  MOVE IN INCLUDE                 07980000
         L     R3,INBEGIN               START OF TABLE                  07990000
         MVC   CNTCRD+15(8),2(R3)       MOVE IN DDNAME                  08000000
         LA    R2,CNTCRD+16             OFFSET OF DDNAME                08010000
PRMLP1   DS    0H                                                       08020000
         CLI   0(R2),C' '               IS IT BLANK                     08030000
         BE    FNDBLK1                  YES                             08040000
         LA    R2,1(R2)                 BUMP UP ADDRESS                 08050000
         B     PRMLP1                   REPEAT                          08060000
FNDBLK1  DS    0H                                                       08070000
         MVI   0(R2),C'('               MOVE IN LEFT PAREN              08080000
         LA    R2,1(R2)                 BUMP UP ADDRESS                 08090000
         MVC   0(8,R2),MEMNAME          MOVE IN MEMBER NAME             08100000
PRMLP2   DS    0H                                                       08110000
         CLI   0(R2),C' '               IS IT BLANK                     08120000
         BE    FNDBLK2                  YES                             08130000
         LA    R2,1(R2)                 BUMP UP ADDRESS                 08140000
         B     PRMLP2                   REPEAT                          08150000
FNDBLK2  DS    0H                                                       08160000
         MVI   0(R2),C')'               MOVE IN RIGHT PAREN             08170000
         BAL   R1,NUMCARD               NUMBER THIS CARD                08180000
         PUT   (R9),CNTCRD              WRITE OUT RECORD                08190000
         TM    OVLYSW,X'FF'             OVERLAY PRESENT?                08200000
         BZ    PSTOVLY                  NO - DO NOT GEN OVERLAY CARDS   08210000
         ST    R9,ADCRDOUT              ST ADDR OF CARD OUT DCB         08220000
         LA    R1,CARDNUM               PTR TO CARD # COUNTER           08230000
         ST    R1,ADCARNUM              ST THE ADDRESS                  08240000
         LA    R1,SEGBFBEG              R1 POINTS TO PARMS              08250000
         L     R15,=V(ZZROVCRD)         R15 -> CARD GENERATOR           08260000
         BALR  R14,R15                  DO THE PROCESS                  08270000
PSTOVLY  DS    0H                                                       08280000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  08290000
         MVC   CNTCRD+1(4),=C'NAME'     MOVE IN NAME                    08300000
         MVC   CNTCRD+10(8),MEMNAME     MOVE IN MEMBER NAME             08310000
         CLI   REPLACE,X'FF'            IS REPLACE OPTION REQ           08320000
         BNE   DONTREP                  NO - PUT OUT RECORD             08330000
         LA    R2,CNTCRD+10             OFFSET OF NAME                  08340000
PRMLP3   DS    0H                                                       08350000
         CLI   0(R2),C' '               IS IT BLANK                     08360000
         BE    FNDBLK3                  YES                             08370000
         LA    R2,1(R2)                 BUMP UP ADDRESS                 08380000
         B     PRMLP3                   REPEAT                          08390000
FNDBLK3  DS    0H                                                       08400000
         MVC   0(3,R2),=C'(R)'          MOVE IN REPLACE OPTION          08410000
DONTREP  DS    0H                                                       08420000
         BAL   R1,NUMCARD               NUMBER THIS CARD                08430000
         PUT   (R9),CNTCRD              WRITE OUT RECORD                08440000
         L     R4,SVREG4                RESTORE R4                      08450000
         BR    R4                       RETURN                          08460000
         TITLE 'LINKAGE EDITOR PARAMETER GENERATION ROUTINE'            08470000
*                                                                       08480000
*         THE FOLLOWING GENERATES THE PARAMETERS REQUIRED FOR THIS      08490000
*    LINKAGE EDIT.                                                      08500000
*                                                                       08510000
PARMGEN  DS    0H                                                       08520000
*                                                                       08530000
*   THE FOLLOWING CODE TESTS TO SEE IF THE MEMBER HAS THE SAME          08540000
*   ATTRIBUTES AS THE PRECEEDING ONE, IF IT DOES, A LINK IS NOT         08550000
*   DONE NOW. IF THE ATTRIBUTES ARE DIFFERENT, A LINK IS DONE AND       08560000
*   THE ATTRIBUTES ARE RECALCULATED.                                    08570000
*                                                                       08580000
         NI    ATTRIB,255-BIT6          TURN OFF UNWANTED BITS        $ 08590000
         NI    ATTRIB+1,255-BIT1-BIT2-BIT3-BIT5-BIT6                    08600000
         CLI   ALLSET,X'00'             VALUES SET UP ?                 08610000
         BE    ATTR1                    NO - SET THEM UP                08620000
         CLC   ATTRIB(3),PRVATTRB       IS ATTRB = PREV ATTRB         $ 08630000
         BE    PARMRET                  YES - RETURN                    08640000
         CLI   LESWCH,X'FF'             L E REQUIRED                    08650000
         BNE   PSTCLS2                  NO - PUCH CARDS                 08660000
         CLOSE (SYSUT2)                                                 08670000
         BAL   R14,ENQOUT               ENQ THE FILE TO BE LINKED TO  $ 08680000
         LINK  EP=IEWL,PARAM=(OPLISTL,DDNMLST),VL=1                     08690000
         MVI   LEUSED,X'FF'             SHOW THAT LINK-EDIT WAS USED  $ 08700000
         BAL   R14,DEQOUT               DEQ THE FILE LINKED TO        $ 08710000
         B     RESET                    SET UP NEW PARAMETERS           08720000
PSTCLS2  DS    0H                                                       08730000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  08740000
         MVC   CNTCRD(2),=C'/*'         DELIMITER                       08750000
         BAL   R1,NUMCARD               NUMBER THIS CARD                08760000
         PUT   SYSPUNCH,CNTCRD          PUNCH CARD                      08770000
*                                                                       08780000
RESET    DS    0H                                                       08790000
         MVI   ALLSET,X'00'             TURN SET-UP SW = OFF            08800000
ATTR1    DS    0H                                                       08810000
         LA    R3,OPLISTV+18            ADDR OF STRING                  08820000
         TM    SZPRMSW,X'FF'            SIZE PARM PRES?                 08830000
         BZ    ATTR2                    NO - CHECK NEXT ATTRIBUTE       08840000
         MVC   0(1,R3),=C','            MOVE A COMMA INTO STRING        08850000
         LA    R3,1(R3)                 BUMP UP LENGTH                  08860000
         LH    R4,SZPRMLTH              GET PARM LENGTH                 08870000
         BCTR  R4,0                     DECR FOR EXEC                   08880000
         EX    R4,MOVE2                 INSERT INTO STRING              08890000
         AH    R3,SZPRMLTH              BUMP UP LENGTH                  08900000
ATTR2    DS    0H                                                       08910000
         LA    R4,OPLISTV               SAVE LENGTH                   $ 08920000
         LR    R2,R3                      OF BASIC PARMS              $ 08930000
         SR    R2,R4                        FOR PUNCHING OF           $ 08940000
         STH   R2,BLEPARML                    LE CONTROL CARDS        $ 08950000
         TM    ATTRIB,X'20'             OVERLAY PRESENT?                08960000
         BZ    ATTR3                    NO - CHECK NEXT ATTRIBUTE       08970000
         MVC   0(5,R3),=C',OVLY'        MOVE IN OPTION                  08980000
         LA    R3,5(R3)                 BUMP UP LENGTH                  08990000
ATTR3    DS    0H                                                       09000000
         TM    ATTRIB,X'80'             IS IT REENTERABLE               09010000
         BZ    ATTR4                    NO CHECK NEXT ATTRIBUTE         09020000
         MVC   0(5,R3),=C',RENT'        MOVE IN OPTION                  09030000
         LA    R3,5(R3)                 BUMP UP LENGTH                  09040000
ATTR4    DS    0H                                                       09050000
         TM    ATTRIB,X'40'             IS IT REUSABLE                  09060000
         BZ    ATTR5                    NO CHECK NEXT ATTRIBUTE         09070000
         MVC   0(5,R3),=C',REUS'        MOVE OPTION                     09080000
         LA    R3,5(R3)                 BUMP UP LENGTH                  09090000
ATTR5    DS    0H                                                       09100000
         TM    ATTRIB+1,X'01'           IS IT REFRESHABLE               09110000
         BZ    ATTR6                    NO CHECK NEXT ATTRIBUTE         09120000
         MVC   0(5,R3),=C',REFR'        MOVE IN OPTION                  09130000
         LA    R3,5(R3)                 BUMP UP LENGTH                  09140000
ATTR6    DS    0H                                                       09150000
         TM    ATTRIB,X'08'             IS IT ONLY LOADABLE             09160000
         BZ    ATTR7                    NO CHECK NEXT ATTRIBUTE         09170000
         MVC   0(3,R3),=C',OL'          MOVE IN OPTION                  09180000
         LA    R3,3(R3)                 BUMP UP LENGTH                  09190000
ATTR7    DS    0H                                                       09200000
         TM    ATTRIB,X'04'             IS IT SCATTER FORMAT            09210000
         BZ    ATTR8                    NO CHECK NEXT ATTRIBUTE         09220000
         MVC   0(5,R3),=C',SCTR'        MOVE IN OPTION                  09230000
         LA    R3,5(R3)                 BUMP UP LENGTH                  09240000
ATTR8    DS    0H                                                       09250000
         TM    ATTRIB,X'10'             IS IT TESTRAN                   09260000
         BZ    ATTR9                    NO CHECK NEXT ATTRIBUTE         09270000
         MVC   0(5,R3),=C',TEST'        MOVE IN OPTION                  09280000
         LA    R3,5(R3)                 BUMP UP LENGTH                  09290000
ATTR9    DS    0H                                                       09300000
         TM    ATTRIB+1,X'80'           IS IT DOWNWARD COMPATIBLE       09310000
         BO    ATTR10                   NO CHECK NEXT ATTRIBUTE       $ 09320000
         MVC   0(3,R3),=C',DC'          MOVE IN OPTION                  09330000
         LA    R3,3(R3)                 BUMP UP LENGTH                  09340000
ATTR10   EQU   *                                                      $ 09350000
         TM    ATTRIB,X'01'             IS IT AUTHORIZED              $ 09360000
         BZ    ATTR11                   NO CHECK NEXT ATTRIBUTE       $ 09370000
         MVC   0(5,R3),=C',AC=1'        MOVE IN OPTION                $ 09380000
         LA    R3,5(R3)                 BUMP UP LENGTH                $ 09390000
ATTR11   EQU   *                                                      $ 09400000
         CLI   ATTRIB+2,X'00'           ANY AMODE/RMODE INFO          $ 09410000
         BZ    ENDATTRB                 NO - END OF ATTRIBUTES        $ 09420000
         TM    ATTRIB+2,X'03'           ANY AMODE IN MAIN EP          $ 09430000
         BZ    ATTR11B                  NO - CHECK ALIAS/ALT EP       $ 09440000
ATTR11MP EQU   *                                                      $ 09450000
         BO    *+18                                                   $ 09460000
         MVC   0(9,R3),=C',AMODE=31'    MOVE IN OPTION                $ 09470000
         LA    R3,9(R3)                   PARM AMODE=31               $ 09480000
         B     ATTR12                                                 $ 09490000
         MVC   0(10,R3),=C',AMODE=ANY'  MOVE IN OPTION                $ 09500000
         LA    R3,10(R3)                  PARM AMODE=ANY              $ 09510000
         B     ATTR12                                                 $ 09520000
ATTR11B  EQU   *                                                      $ 09530000
         TM    ATTRIB+2,X'C0'           ANY AMODE IN ALIAS/ALT EP     $ 09540000
         BZ    ATTR12                   NO CHECK NEXT ATTRIBUTE       $ 09550000
         B     ATTR11MP                                               $ 09560000
ATTR12   EQU   *                                                      $ 09570000
         TM    ATTRIB+2,X'10'           ANY RMODE INFO                $ 09580000
         BZ    ENDATTRB                 NO - END OF ATTRIBUTES        $ 09590000
         MVC   0(10,R3),=C',RMODE=ANY'  MOVE IN RMODE OPTION          $ 09600000
         LA    R3,10(R3)                                              $ 09610000
ENDATTRB DS    0H                                                       09620000
         LA    R4,OPLISTV                                               09630000
         SR    R3,R4                                                    09640000
         STH   R3,OPLISTL               LENGTH OF PARAMETER STRING      09650000
         MVC   PRVATTRB(3),ATTRIB       SAVE ATTRIBUTES               $ 09660000
PARMRET  DS    0H                                                       09670000
         BR    R9                       RETURN                          09680000
         TITLE 'LINKAGE EDITOR DDNAME LIST GENERATOR'                   09690000
*                                                                       09700000
*         THE FOLLOWING GENERATES THE DDNAME LIST FOR THE LINKAGE       09710000
*    EDITOR WHEN REQUIRED.                                              09720000
*                                                                       09730000
GENDDLST DS    0H                                                       09740000
         MVC   DDENTR3,OUTNAME          MOVE IN OUTDDNAME               09750000
         BR    R9                       RETURN                          09760000
         TITLE 'JCL GENERATION ROUTINE - GENERATE FUNCTION ONLY'        09770000
*                                                                       09780000
*         THE FOLLOWING GENERATES THE JCL WHEN THE FUNCTION IS TO       09790000
*    PUNCH IT OUT.                                                      09800000
*                                                                       09810000
JCLGENR  DS    0H                                                       09820000
         CLI   ALLSET,X'FF'             JCL SET UP                      09830000
         BE    JCLSET                   YES DONT DO AGAIN               09840000
*                                                                       09850000
JOBSW    NOP   NOJOB                    BRANCH IF NO JOB CARD TO PUNCH  09860000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD AREA             09870000
         MVC   CNTCRD(L'JOBCARD),JOBCARD MOVE CARD TO PUNCH AREA        09880000
         AP    JOBNO,=P'1'              BUMP UP JOB NUMBER              09890000
         UNPK  CNTCRD+7(3),JOBNO        PLACE JOB NUMBER ON CARD        09900000
         OI    CNTCRD+9,X'F0'           ENSURE VALID SIGN               09910000
         BAL   R1,NUMCARD               GO NUMBER JCL CARD              09920000
         PUT   SYSPUNCH,CNTCRD          PUNCH OUT JOB CARD              09930000
         MVI   JOBSW+1,X'F0'            SHOW JOB CARD ALREADY PUNCHED   09940000
NOJOB    DS    0H                                                       09950000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  09960000
         AP    STEPNO,=P'1'             GET CURRENT STEP NUMBER         09970000
         UNPK  LEEXEC+6(3),STEPNO(2)    UNPACK INTO STEPNAME            09980000
         OI    LEEXEC+8,X'F0'           ENSURE VALID SIGN               09990000
         MVC   CNTCRD(L'LEEXEC),LEEXEC  SET UP L.E. EXEC CARD           10000000
         BAL   R1,NUMCARD               NUMBER THIS CARD                10010000
         PUT   SYSPUNCH,CNTCRD          PUNCH IT OUT                    10020000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  10030000
         MVC   CNTCRD(L'BLEPARM),BLEPARM SET UP BASIC L.E. PARMS        10040000
         LH    R4,BLEPARML              LENGTH                        $ 10050000
         BCTR  R4,0                     DECREMENT FOR EXECUTE           10060000
         EX    R4,MOVE1                 UPDATE STRING                   10070000
         LA    R4,CNTCRD+10                                           $ 10080000
         AH    R4,BLEPARML                                            $ 10090000
         MVI   0(R4),C''''                                              10100000
         CLC   OPLISTL,BLEPARML         ADDITIONAL PARMS ?            $ 10110000
         BE    *+8                      NO                            $ 10120000
         MVI   1(R4),C','               YES - SHOW CONTINUATION       $ 10130000
         BAL   R1,NUMCARD               NUMBER THIS CARD                10140000
         PUT   SYSPUNCH,CNTCRD          PUNCH IT OUT                    10150000
         CLC   OPLISTL,BLEPARML         ADDITIONAL PARMS ?            $ 10160000
         BE    BYEXPARM                 NO                            $ 10170000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                $ 10180000
         MVC   CNTCRD(4),=C'// '''      BEGIN CONTINUATION CARD       $ 10190000
         LH    R4,OPLISTL               GET LENGTH OF                 $ 10200000
         SH    R4,BLEPARML                ADDITIONAL PARMS            $ 10210000
         SH    R4,=H'2'                 REDUCE FOR COMMA AND MOVE     $ 10220000
         LA    R3,OPLISTV               LOCATE ADDITIONAL             $ 10230000
         AH    R3,BLEPARML                PARMS IN WHOLE LIST         $ 10240000
         EX    R4,MOVE1B                MOVE ADDITIONAL PARMS         $ 10250000
         LA    R3,CNTCRD+4                TO LINK EDIT PARMS ON       $ 10260000
         AR    R3,R4                        CONTINUATION CARD AND     $ 10270000
         MVC   1(2,R3),=C''')'                AND TERMINATE PARM LIST $ 10280000
         BAL   R1,NUMCARD               NUMBER THIS CARD              $ 10290000
         PUT   SYSPUNCH,CNTCRD          PUNCH IT OUT                  $ 10300000
BYEXPARM EQU   *                                                      $ 10310000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  10320000
         MVC   CNTCRD(L'LESYSOUT),LESYSOUT SET UP SYSPRINT DD CARD      10330000
         BAL   R1,NUMCARD               NUMBER THIS CARD                10340000
         PUT   SYSPUNCH,CNTCRD          PUNCH IT OUT                    10350000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  10360000
         MVC   CNTCRD(L'LESYSUT1),LESYSUT1 SET UP SYSUT1 WORK FILE      10370000
         BAL   R1,NUMCARD               NUMBER THIS CARD                10380000
         PUT   SYSPUNCH,CNTCRD          PUNCH IT OUT                    10390000
* BYPASS UNIT LOOKUP FOR SYSUT1 DD CARD, TO ACTIVATE MAKE BRANCH A NOP  10400000
         B     BYUNIT                                                   10410000
         LD    R2,OUTNAME               FPR2 = DDNAME                   10420000
         BAL   R4,UNITGEN               DO UNIT LOOK-UP                 10430000
         L     R4,UNITAB(R1)            GET PTR TO UNIT TEXT            10440000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CONTROL CARD          10450000
         MVC   CNTCRD(24),0(R4)         MOVE IN UNIT TEXT               10460000
         BAL   R1,NUMCARD               NUMBER THIS CARD                10470000
         PUT   SYSPUNCH,CNTCRD          PUNCH OUT THE CARD              10480000
BYUNIT   MVC   INDCB+40(8),OUTNAME      MOVE DDNAME TO DCB              10490000
         RDJFCB (INDCB)                 READ JFCB                       10500000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  10510000
         MVC   CNTCRD(L'LESYSLMD),LESYSLMD MOVE IN BASIC SYSLMOD DD     10520000
         BAL   R1,NUMCARD               NUMBER THIS CARD                10530000
         PUT   SYSPUNCH,CNTCRD          PUNCH IT OUT                    10540000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  10550000
         LD    R2,OUTNAME               FPR2 = DDNAME                   10560000
         BAL   R4,UNITGEN               DO UNIT LOOK-UP                 10570000
         L    R4,UNITAB(R1)             PTR TO UNIT TEXT                10580000
         MVC  CNTCRD(24),0(R4)          MOVE IN UNIT TEXT               10590000
         MVC  CNTCRD+24(9),=C',VOL=SER=' MOVE IN BASIC VOLUME TEXT      10600000
         LD   R2,OUTNAME                FLPR2 = DDNAME                  10610000
         BAL  R4,VOLGEN                 DO VOLUME LOOK-UP               10620000
         MVC  CNTCRD+33(6),VOLUME       INSERT VOL ID                   10630000
         MVI  CNTCRD+39,C','            CONTINUE INDICATION             10640000
         BAL  R1,NUMCARD                NUMBER THIS CARD                10650000
         PUT  SYSPUNCH,CNTCRD           PUT OUT CARD                    10660000
         MVC  CNTCRD,CNTCRD-1           BLANK OUT CARD                  10670000
         MVC  CNTCRD(L'LEDSNAME),LEDSNAME MOVE IN DSNAME PARAMETER      10680000
         MVC  CNTCRD+22(44),JFCB        INSERT DSN                      10690000
         BAL   R1,NUMCARD               NUMBER THIS CARD                10700000
         PUT   SYSPUNCH,CNTCRD          PUT OUT CARD                    10710000
         L     R3,INBEGIN               START OF TABLE                  10720000
         MVC   INDCB+40(8),2(R3)        MOVE IN DDNAME TO DCB           10730000
         RDJFCB (INDCB)                 READ JFCB                       10740000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  10750000
         L     R3,INBEGIN               START OF TABLE                  10760000
         MVC   CNTCRD(L'LEINPUT),LEINPUT BASIC INPUT TEXT               10770000
         MVC   CNTCRD+2(8),2(R3)                                        10780000
         BAL   R1,NUMCARD               NUMBER THIS CARD                10790000
         PUT   SYSPUNCH,CNTCRD          PUNCH IT OUT                    10800000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  10810000
         MVC   DW,2(R3)                 MOVE TO DW BOUNDRY              10820000
         LD    R2,DW                    FLPR2 = DDNAME                  10830000
         BAL   R4,UNITGEN               DO UNIT LOOK-UP                 10840000
         L     R4,UNITAB(R1)            GET PTR TO UNIT TEXT            10850000
         MVC   CNTCRD(24),0(R4)         MOVE IN UNIT TEXT               10860000
         MVC   CNTCRD+24(9),=C',VOL=SER=' SET UP BASIC VOL TEXT         10870000
         MVC   DW,2(R3)                 MOVE TO DW BOUNDRY              10880000
         LD    R2,DW                    FLPR2 = DDNAME                  10890000
         BAL   R4,VOLGEN                GET VOL ID                      10900000
         MVC   CNTCRD+33(6),VOLUME      VOL ID                          10910000
         MVI   CNTCRD+39,C','           CONTINUE INDICATION             10920000
         BAL   R1,NUMCARD               NUMBER THIS CARD                10930000
         PUT   SYSPUNCH,CNTCRD          PUT OUT CARD                    10940000
         MVC   CNTCRD,CNTCRD-1          BLANK OUT CARD                  10950000
         MVC   CNTCRD(L'LEDSNAME),LEDSNAME BASIC DSNAME TEXT            10960000
         MVC   CNTCRD+22(44),JFCB       DSNAME                          10970000
         BAL   R1,NUMCARD               NUMBER THIS CARD                10980000
         PUT   SYSPUNCH,CNTCRD         PUT OUT CARD                     10990000
         MVC   CNTCRD,CNTCRD-1         BLANK OUT CARD                   11000000
         MVC   CNTCRD(L'LESYSLIN),LESYSLIN MOVE IN SYSLIN DD            11010000
         BAL   R1,NUMCARD               NUMBER THIS CARD                11020000
         PUT   SYSPUNCH,CNTCRD          PUNCH IT OUT                    11030000
JCLSET   DS    0H                                                       11040000
         ST    R9,SVREG9                SAVE R9                         11050000
         BAL   R4,CONTGEN               PUNCH OUT CONTROL CARDS         11060000
         L     R9,SVREG9                RESTORE R9                      11070000
         MVI   ALLSET,X'FF'             TURN SET-UP SW = ON             11080000
         BR    R9                       RETURN                          11090000
*                                                                       11100000
NUMCARD  DS    0H                                                       11110000
         CLI   LESWCH,X'FF'             APPLICATION FUNCTION ?          11120000
         BE    0(0,R1)                  RETURN IF YES                   11130000
         AP    CARDNUM,=P'10'           INCREMENT CARD NUMBER           11140000
         UNPK  CNTCRD+72(8),CARDNUM     NUMBER THE CARD                 11150000
         OI    CNTCRD+79,X'F0'          ENSURE VALID CHARACTER          11160000
         BR    R1                       RETURN                          11170000
         TITLE 'UNIT AND VOLUME DETERMINATION ROUTINE'                  11180000
UNITGEN  DS    0H                                                       11190000
         MVI   DDFND+1,X'F0'            SET BRANCH SWITCH               11200000
         B     TIOTSET                  ENTER TIOT LOOK-UP              11210000
VOLGEN   DS    0H                                                       11220000
         MVI   DDFND+1,X'00'            INDIC VOL LOOK-UP REQ           11230000
TIOTSET  DS    0H                                                       11240000
         L     R1,ADRTIOT               R1 PTS TO TIOT                  11250000
         SR    R14,R14                  CLEAR LENGTH REG                11260000
TIOTLUP  DS    0H                                                       11270000
         IC    R14,0(R1)                LENGTH OF ENTRY                 11280000
         LTR   R14,R14                  END OF TIOT?                    11290000
         BZ    TIOTER1                  NO MATCHING DD ENTRY            11300000
         MVC   DW,4(R1)                 MOVE TO BOUNDRY                 11310000
         CD    R2,DW                    IS THIS THE REQ DD ENTRY        11320000
         BE    DDFND                    BRANCH IF YES                   11330000
         LA    R1,0(R14,R1)             BUMP TO NEXT DD ENTRY           11340000
         B     TIOTLUP                  BRANCH TO TIOT LOOP             11350000
DDFND    DS    0H                                                       11360000
         NOP   UNITLOOP                 BRANCH IF A UNIT GEN            11370000
         L     R1,16(R1)                R1 = PTR TO UCB                 11380000
         TM    18(R1),X'20'             IS THIS A DASD DEVICE           11390000
         BZ    UCBER1                   NO - ERROR                      11400000
         MVC   VOLUME(6),28(R1)         SET UP VOLUME                   11410000
         BR    R4                       RETURN                          11420000
UNITLOOP DS    0H                                                       11430000
         L     R1,16(R1)                R1 = PTR TO UCB                 11440000
         TM    18(R1),X'20'             DASD DEVICE?                    11450000
         BZ    UCBER1                   NO - ERROR                      11460000
         LR    R14,R1                   SAVE UCB PTR                    11470000
         TM    19(R14),X'0F'            IS IT 3390                    $ 11480000
         BNO   *+10                     NO                            $ 11490000
         LA    R1,28                    YES - SET UP 3390             $ 11500000
         BR    R4                       3390                          $ 11510000
         LA    R1,4                     SET FOR 3330 FIRST              11520000
         TM    19(R14),X'09'            3330?                           11530000
         BNO   *+16                     NO                            $ 11540000
         TM    19(R14),X'04'            IS IT DUAL DENSITY            $ 11550000
         BCR   8,R4                     NO - 3330                     $ 11560000
         LA    R1,20                    YES - SET UP 3330-1           $ 11570000
         BR    R4                       3330-1                        $ 11580000
         TM    19(R14),X'0E'            IS IT 3380                    $ 11590000
         BNO   *+10                     NO                            $ 11600000
         LA    R1,24                    YES - SET UP 3380             $ 11610000
         BR    R4                       3380                          $ 11620000
         SR    R1,R1                    TRY FOR 2314                    11630000
         TM    19(R14),X'08'            2314?                           11640000
         BCR   1,R4                     YES - RETURN                    11650000
         LA    R1,8                     TRY FOR 2301                    11660000
         TM    19(R14),X'02'            2301?                           11670000
         BCR   1,R4                     YES - RETURN                    11680000
         LA    R1,12                    TRY FOR 2305                    11690000
         TM    19(R14),X'06'            2305 M1 ?                       11700000
         BCR   1,R4                     YES - RETURN                    11710000
         TM    19(R14),X'07'            2305 M2 ?                       11720000
         BCR   1,R4                     YES - RETURN                    11730000
         LA    R1,16                    TRY FOR 2311                    11740000
         TM    19(R14),X'01'            2311?                           11750000
         BCR   1,R4                     YES - RETURN                    11760000
         STD   R2,DDNMFLD               SET UP DDNAME                   11770000
         MVI   DDNMDISP,18              DDNAME GOES AT +18              11780000
         MVI   MSGLIST+1,UNITER01       SET UP UNIT ERROR MESSAGE       11790000
         LA    R2,BEGINLNK              SET RETURN ADDRESS              11800000
         $SETRC 8                       SET RETURN CODE TO 8            11810000
         B     MSGWRTD                  WRITE MESSAGE WITH DDNAME       11820000
UCBER1   DS    0H                                                       11830000
         MVI   MSGLIST+1,NOTDA         NOT DASD DEVICE                  11840000
         LA    R2,BEGINLNK             SET RETURN ADDRESS               11850000
         $SETRC 8                       SET RETURN CODE TO 8            11860000
         B     MSGWRT                  WRITE ERROR MESSAGE              11870000
TIOTER1  DS    0H                                                       11880000
         STD   R2,DDNMFLD              DD CARD MISSING                  11890000
         $SETRC 8                     SET RETURN CODE TO 8              11900000
         LA    R2,BEGINLNK             SET RETURN ADDRESS               11910000
NODD     DS    0H                                                       11920000
         MVI   DDNMDISP,0              DDNAME GOES AT +0                11930000
         MVI   MSGLIST+1,NODDCARD      SET UP DD CARD MISSING           11940000
         B     MSGWRTD                 WRITE MESSAGE WITH NAME          11950000
         TITLE 'I/O ERROR RECOVERY ROUTINES'                            11960000
*                                                                       11970000
*        CONSTANTS AREA                                                 11980000
*                                                                       11990000
MOVE1    MVC   CNTCRD+10(*-*),OPLISTV   MOVE IN STRING                $ 12000000
*                                                                       12010000
MOVE1B   MVC   CNTCRD+4(*-*),1(R3)      MOVE IN STRING                $ 12020000
*                                                                     $ 12030000
MOVE2    MVC   0(*-*,R3),SZPARM         MOVE IN STRING                  12040000
*                                                                       12050000
MOVE3    MVC   SZPARM(*-*),2(R1)        MOVE IN STRING                  12060000
*                                                                       12070000
DDCBIOER DS    0H                                                       12080000
         L     R3,INBEGIN               START OF TABLE                  12090000
         MVC   DDNMFLD(8),2(R3)         MOVE DDNAME TO MSG AREA         12100000
DIRER2   DS    0H                                                       12110000
         MVI   DDNMDISP,32              DDNAME GOES AT +32              12120000
         MVI   MSGLIST+1,DIRERR02       SHOW I/O ERROR ON DIRECTORY     12130000
         LA    R2,BEGINLNK              RETURN ADDRESS                  12140000
         B     MSGWRTN                  WRITE MESSAGE WITH NAME         12150000
*                                                                       12160000
OUTIOER  DS    0H                                                       12170000
         MVC   DDNMFLD(8),OUTNAME       SET NAME FOR MESSAGE            12180000
         B     DIRER2                   WRITE MESSAGE                   12190000
         TITLE 'TERMINATION ROUTINE - END OF JOB AND CLEANUP'           12200000
*                                                                       12210000
*         THE FOLLOWING CONTROLS THE TERMINATION OF THIS UTILITY.       12220000
*    IT CLOSES FILES, RETURNS CONTROL TO THE OPER SYS ETC.              12230000
*                                                                       12240000
ZZRTERM  DS    0H                                                       12250000
         TM    SYSUT3+48,BIT3           ANY COPIES TO DO                12260000
         BNO   NOCOPIES                 BRANCH IF NOT                   12270000
         CLOSE (SYSUT3)                 CLOSE THE CONTROL DATA SET      12280000
*                                                                       12290000
         CLI   LESWCH,X'FF'             APPLICATION FUNCTION ?          12300000
         BNE   NOCOPIES                 BRANCH IF NOT                   12310000
         LINK  EP=IEBCOPY,PARAM=(COPYOPTN,COPYDDNM),VL=1                12320000
*                                                                       12330000
NOCOPIES DS    0H                                                       12340000
         CLI   SVCLIB,X'FF'             SCVLIB ALTERED                  12350000
         BNE   NOIOSUP                  BRANCH IF NOT                   12360000
*                                                                       12370000
*        ASK FOR OPERATOR VERIFICATION AND INVOKE IEHIOSUP              12380000
*                                                                       12390000
VERIOSUP DS    0H                                                       12400000
         XC    RECB,RECB                CLEAR REPLY ECB                 12410000
         WTOR  'ACTION 0571 - VERIFY REQUEST TO PERFORM IEHIOSUP ON ''SX12420000
               YS1.SVCLIB''',REPLY,1,RECB,ROUTCDE=1,DESC=2              12430000
         WAIT  ECB=RECB                 WAIT FOR REPLY                  12440000
         OI    REPLY,C' '               ENSURE UPPER CASE REPLY         12450000
         CLI   REPLY,C'T'               REPLY = 'TERMINATE'             12460000
         BE    NOIOSUP                  BRANCH IF YES                   12470000
         CLI   REPLY,C'U'               OK TO GO AHEAD                  12480000
         BE    IOSPLINK                 BRANCH IF YES                   12490000
         WTO   'ACTION 0008 - REPLY NOT VERIFIED',ROUTCDE=1,DESC=2      12500000
         B     VERIOSUP                 GO ASK AGAIN                    12510000
IOSPLINK DS    0H                                                       12520000
         TM    OUTDCB+48,BIT3           IS DCB OPEN                     12530000
         BNO   IOSPOPEN                 BRANCH IF NOT                   12540000
         CLOSE (OUTDCB)                                                 12550000
IOSPOPEN DS    0H                                                       12560000
         MVC   OUTDCB+40(8),SVCDDNM     SET UP FOR BLDL                 12570000
         MVC   IOSPDD(8),SVCDDNM        SET UP FOR IOSUP                12580000
         OPEN  (OUTDCB)                 OPEN DCB TO SVCLIB              12590000
         MVC   BLDLIST+4(8),=C'IGC0009I' TEST FOR SVC 99 ON SYSTEM      12600000
         BLDL  OUTDCB,BLDLIST           GO TO BLDL ROUTINE              12610000
         LTR   R15,R15                  TEST TO SEE IF SVC 99 FOUND     12620000
         BNZ   NOTSOSYS                 BRANCH IF NOT FOUND             12630000
         MVI   IOSUPARM+1,3             SHOW PARM=TSO PRESENT           12640000
NOTSOSYS DS    0H                                                       12650000
         LINK  EP=IEHIOSUP,PARAM=(IOSUPARM,IOSPDDNM),VL=1               12660000
*                                                                       12670000
NOIOSUP  DS    0H                                                       12680000
         CLOSE (PRTDCB)                                                 12690000
         CLOSE (CARDCB)                                                 12700000
         CLOSE (SYSPUNCH)                                               12710000
         LM    R0,R1,TABLEN             PICK UP NO. OF ENTRIES IN     $ 12720000
*                                       THE ALIAS TABLE AND THE ADD.  $ 12730000
*                                       OF WORK AREA TO HOLD THE TAB. $ 12740000
         LTR   R1,R1                    DID WE OBTAIN A WORK AREA     $ 12750000
         BC    8,RETURN                 NO - CONTINUE                 $ 12760000
         SLL   R0,4                     CALCULATE THE SIZE OF TABLE   $ 12770000
*                                       BY MULT. NO. OF ENTRIES X 16  $ 12780000
         FREEMAIN R,LV=(R0),A=(R1)      FREE WORK AREA                $ 12790000
         XR    R1,R1                    CLEAR POINTER                 $ 12800000
         ST    R1,TABADD                  TO TABLE AREA               $ 12810000
RETURN   EQU   *                                                      $ 12820000
         L     R13,4(R13)                                               12830000
         CLI   LEUSED,X'FF'             WAS LINK-EDIT USED THIS RUN   $ 12840000
         BE    *+12                     YES                           $ 12850000
         LA    R15,20                   NO - SHOW A 20 RC             $ 12860000
         B     *+10                                                     12870000
         XR    R15,R15                  CLEAR R15                       12880000
         IC    R15,RC                   SET RETURN CODE                 12890000
         RETURN (14,12),RC=(15)                                         12900000
         TITLE 'INTERFACES TO MESSAGE WRITING ROUTINE'                  12910000
MSGWRTD  DS    0H                                                       12920000
         MVI   MSGLIST,PBIT+LASTMSG     SHOW PARM PRESENT & LAST MSG    12930000
         MVI   PARAMS,DDNM              SHOW DDNAME PRESENT             12940000
         B     MSGWRT                   GO WRITE MESSAGE                12950000
*                                                                       12960000
MSGWRTN  DS    0H                                                       12970000
         MVI   MSGLIST,PBIT+LASTMSG     SHOW LAST MSG AND PARMS         12980000
         MVI   PARAMS,NAME              SHOW NAME PARM PRESENT          12990000
*                                                                       13000000
MSGWRT   DS    0H                                                       13010000
         LA    R4,ZZRELCOM             RESET PTR TO COMMUNICATION AREA  13020000
         L     R15,VZZRLMES             GET PTR TO MESSAGE WRITER       13030000
         BALR  R14,R15                  GO WRITE MESSAGE                13040000
         BR    R2                       EXIT TO SUPPLIED RETURN POINT   13050000
         TITLE 'ROUTINE TO VERIFY PRESENCE OF INDD AND OUTDD CARDS'     13060000
*                                                                       13070000
*         THE FOLLOWING LOKS THROUGH THE TIOT TO SEE IF                 13080000
*     DDCARDS ARE PRESENT FOR THE INDD DATASET AND THE OUTDD DATASET.   13090000
*                                                                       13100000
DDSRCH   DS    0H                                                       13110000
         MVI   INDDPR,X'00'             INIT SWITCH TO ZERO             13120000
         MVI   OUTDDPR,X'00'            INIT SWITCH TO ZERO             13130000
         L     R1,ADRTIOT               R1 -> TIOT                      13140000
         L     R3,INBEGIN               R3 -> INDD TABLE                13150000
SRCHLOOP DS    0H                                                       13160000
         SR    R14,R14                  INIT R14 TO ZERO                13170000
         IC    R14,0(R1)                LENGTH OF ENTRY                 13180000
         LTR   R14,R14                  ENTRY PRESENT?                  13190000
         BZ    SRCHEND                  END OF TIOT                     13200000
         CLC   OUTNAME(8),4(R1)         OUTNAME MATCH?                  13210000
         BNE   TESTINDD                 NO - CHECK INDD                 13220000
         MVI   OUTDDPR,X'FF'            SHOW OUT DD PRESENT             13230000
         B     SRCHINCR                 INCREMENT TO NEXT TIOT ENTRY    13240000
TESTINDD DS    0H                                                       13250000
         CLC   2(8,R3),4(R1)            INNAME MATCH?                   13260000
         BNE   SRCHINCR                 NO MATCH                        13270000
         MVI   INDDPR,X'FF'             SHOW IN DD PRESENT              13280000
SRCHINCR DS    0H                                                       13290000
         LA    R1,0(R14,R1)             BUMP UP TO NEXT ENTRY           13300000
         B     SRCHLOOP                 PROCESS NEXT ENTRY              13310000
SRCHEND  DS    0H                                                       13320000
         TM    INDDPR,X'FF'             WAS IN DD PRESENT?              13330000
         BZ    NOINDDPR                 NO - ERROR                      13340000
         TM    OUTDDPR,X'FF'            WAS OUT DD PRESENT?             13350000
         BZ    NOOUTDD                  NO - ERROR                      13360000
         BR    R9                       RETURN - ALL PRESENT            13370000
NOINDDPR DS    0H                                                       13380000
         MVC   DDNMFLD(8),2(R3)         STORE DD NAME FOR MSG           13390000
SETDDRET DS    0H                                                       13400000
         LA    R2,BEGINLNK              SET THE RETURN ADDRESS          13410000
         $SETRC 8                       SET THE RETURN CODE TO 8        13420000
         B     NODD                     WRITE DD MISSING MSG            13430000
NOOUTDD  DS    0H                                                       13440000
         MVC   DDNMFLD(8),OUTNAME       DD NAME FOR MSG                 13450000
         B     SETDDRET                 SET RETURN ADDRESS              13460000
         TITLE 'DATA AREAS AND CONSTANTS'                               13470000
*                                                                     $ 13480000
*         THE FOLLOWING ENQUES AND DEQUEUES THE SYSLMOD TYPE DATA     $ 13490000
*     SET (THE DATA SET TO BE LINKED TO) WITH A QNANE AND RNANE       $ 13500000
*     COMPATABLE WITH SHARDISK AND THE PROGS. OF THE CAP SYSTEM.      $ 13510000
*                                                                     $ 13520000
ENQOUT   EQU   *                                                      $ 13530000
         ST    R14,SVREG14              SAVE RETURN REG               $ 13540000
         ENQ   (QENQOUT,RENQOUT,E,,SYSTEM)   ENQ FILE                 $ 13550000
         B     ENQDEQRT                                               $ 13560000
DEQOUT   EQU   *                                                      $ 13570000
         ST    R14,SVREG14              SAVE RETURN REG               $ 13580000
         DEQ   (QENQOUT,RENQOUT,,SYSTEM)     DEQ FILE                 $ 13590000
ENQDEQRT EQU   *                                                      $ 13600000
         L     R14,SVREG14              RESTORE RETURN REG            $ 13610000
         BR    R14                      RETURN                        $ 13620000
         EJECT                                                        $ 13630000
*                                                                       13640000
*         THE FOLLOWING DEFINES THE AREAS OF STORAGE REQUIRED           13650000
*    IN THIS ROUTINE.                                                   13660000
*                                                                       13670000
FFF      DC    8X'FF'                   8 BYTES OF 'FF'                 13680000
ATTRIB   DS    CL3                      ATTRIBUTES OF MEMBER          $ 13690000
PRVATTRB DC    XL3'00'                  SAVE ATTRB AREA               $ 13700000
*       ATTRIBUTE BIT DEFINITIONS                                     $ 13710000
*   BYTE 1         BYTE 2         BYTE 3                              $ 13720000
*  80  RENT       80  DC         80  UNUSED                           $ 13730000
*  40  REUS       40  UNUSED     40  UNUSED                           $ 13740000
*  20  OVLY       20  UNUSED     20  UNUSED                           $ 13750000
*  10  TEST       10  UNUSED     10  RMODE=ANY                        $ 13760000
*  08  OL         08  NOT LE     08  AMODE=31(TRUE ALIAS OR ALT EP)   $ 13770000
*  04  SCTR       04  UNUSED     0C  AMODE=ANY(TRUE ALIAS OR ALT EP)  $ 13780000
*  02  PAGE ALIGN 02  UNUSED     02  AMODE=31(MAIN EP)                $ 13790000
*  01  AC=1       01  REFR       03  AMODE=ANY(MAIN EP)               $ 13800000
EPADDR   DS    CL3                      ENTRY POINT OF MEMBER           13810000
DW       DC    D'0'                                                     13820000
ADDRMAIN DC    F'0'                                                     13830000
MEMNAME  DS    CL8                      MEMBER NAME                     13840000
SVCDDNM  DS    CL8                      SVCLIB DD NAME                  13850000
CSCTNAME DS    CL8                      MAIN ENTRY CSECT NAME           13860000
SETSSIBI DS    F                        SETSSI BINARY                 $ 13870000
SETSSIDP DS    D                        SETSSI DISPLAY                $ 13880000
TRTAB    DC    CL16'0123456789ABCDEF'                                 $ 13890000
BLNK8    DC    CL8' '                   FIELD OF BLANKS                 13900000
DBLOCK   DC    H'00',256X'00'                                           13910000
         DC    50C' '                                                   13920000
         DC    CL1' '                   USED TO BLANK OUT CARD          13930000
CNTCRD   DS    CL80                     JCL OR CONTROL CARD AREA        13940000
AEODAM   DC    A(MEMRET)                EOF EXIT                        13950000
AEODAD   DC    A(ALIASRET)              EOF EXIT                        13960000
UT1      DC    XL1'00'                  SYSUT1 SWITCH  FF - PRESENT     13970000
*                                                      00 - NOT PRES    13980000
UT2      DC    XL1'00'                  SYSUT2 SWITCH  FF - PRESENT     13990000
*                                                      00 - NOT PRES    14000000
LESWCH   DC    XL1'FF'                  LINK SWITCH    FF - LINK        14010000
*                                                      00 - PUNCH       14020000
LEUSED   DC    XL1'00'                  LINK-EDIT USED FF - USED-RUN  $ 14030000
*                                                      00 - NOT USED  $ 14040000
SETSSISW DC    XL1'00'                  SETSSI SWITCH  FF - SETSSI    $ 14050000
*                                                      00 - NO SETSSI $ 14060000
REPLACE  DC    XL1'00'                  REPLACE SWITCH FF - YES         14070000
*                                                      00 - NO          14080000
ALLSET   DC    XL1'00'                  SET UP SWITCH  00 - DO SET UP   14090000
*                                                      FF - DONT SET UP 14100000
SYSLOPR  DC    XL1'00'                  SYSLOUT SWITCH FF - PRESENT     14110000
*                                                      00 - NOT PRES    14120000
INDDPR   DC    XL1'00'                  INDD SWITCH    FF - PRESENT     14130000
*                                                      00 - NOT PRES    14140000
OUTDDPR  DC    XL1'00'                  OUTDD SWITCH   FF - PRESENT     14150000
*                                                      00 - NOT PRES    14160000
UT3      DC    XL1'00'                  SYSUT3 SWITCH  FF - PRESENT     14170000
*                                                      00 - NOT PRES    14180000
SVCLIB   DC    XL1'00'                                                  14190000
COPYSW   DC    XL1'00'                                                  14200000
RC       DC    XL1'00'                  RETURN CODE HOLD AREA           14210000
REPLY    DC    C' '                                                     14220000
RECB     DC    F'0'                                                     14230000
SVREG14  DS    F                                                      $ 14240000
SVREG9   DS    F                        SAVE AREA FOR R9                14250000
SVREG4   DS    F                        SAVE AREA FOR R4                14260000
SVREG5   DS    F                        SAVE AREA FOR R5                14270000
ADRTIOT  DS    F                        SAVE TIOT ADDRESS               14280000
BLDLIST  DC    H'1'                                                     14290000
         DC    H'58'                                                    14300000
         DS    56X                                                      14310000
JFCB     DS    44F                      JFCB                            14320000
         DS    0H                                                       14330000
QENQOUT  DC    CL8'MM      '                                          $ 14340000
RENQOUT  DC    CL44' '                                                $ 14350000
DDNMLST  DC    H'48'                    LIST LENGTH                     14360000
         DC    C'SYSUT2  '                                              14370000
         DC    XL8'00'                                                  14380000
DDENTR3  DS    CL8                                                      14390000
         DC    C'SYSLIB  '                                              14400000
         DC    XL8'00'                                                  14410000
         DC    C'SYSLOUT '                                              14420000
         DS    0H                                                       14430000
OPLISTL  DS    XL2                                                      14440000
OPLISTV  DC    CL78'LET,LIST,XREF,NCAL'                               $ 14450000
COPYOPTN DC    H'0'                                                     14460000
COPYDDNM DC    H'40'                                                    14470000
         DC    8XL1'00'                UNUSED                           14480000
         DC    8XL1'00'                UNUSED                           14490000
         DC    8XL1'00'                UNUSED                           14500000
         DC    8XL1'00'                UNUSED                           14510000
         DC    CL8'SYSUT3'             ALTERNATE NAME FOR SYSIN         14520000
IOSUPARM DC    H'0'                                                     14530000
         DC    C'TSO'                                                   14540000
IOSPDDNM DC    H'64'                                                    14550000
         DC    56XL1'00'                                                14560000
IOSPDD   DC    CL8' '                                                   14570000
         DS    0F                                                       14580000
TABLEN   DC    F'2000'                                                $ 14590000
TABADD   DC    F'0'                                                   $ 14600000
ALEND    DS    F                                                      $ 14610000
ALENTRY  DS    CL11                                                     14620000
ATABS    DS    F                                                      $ 14630000
TABEND   DS    F                                                      $ 14640000
         DS    0F                                                       14650000
XLIST1   DC    X'87'                                                    14660000
         DC    AL3(JFCB)                                                14670000
UNITAB   DC    A(T2314)                                                 14680000
         DC    A(T3330)                                                 14690000
         DC    A(T2301)                                                 14700000
         DC    A(T2305)                                                 14710000
         DC    A(T2311)                                                 14720000
         DC    A(T3330D)                                              $ 14730000
         DC    A(T3380)                                               $ 14740000
         DC    A(T3390)                                               $ 14750000
T2314    DC    C'//             UNIT=2314'                              14760000
T3330    DC    C'//             UNIT=3330'                              14770000
T2301    DC    C'//             UNIT=2301'                              14780000
T2305    DC    C'//             UNIT=2305'                              14790000
T2311    DC    C'//             UNIT=2311'                              14800000
T3330D   DC    C'//           UNIT=3330-1'                            $ 14810000
T3380    DC    C'//             UNIT=3380'                            $ 14820000
T3390    DC    C'//             UNIT=3390'                            $ 14830000
VOLUME   DS    CL6                                                      14840000
JOBNO    DC    PL2'0'                                                   14850000
STEPNO   DC    PL2'0'                                                   14860000
CARDNUM  DC    PL5'0'                                                   14870000
*                                                                       14880000
* THE FOLLOWING MUST STAY DEFINED IN THE ORDER SHOWN                    14890000
SEGBFBEG DS    F                                                        14900000
SEGUSED  DS    F                                                        14910000
CESDADR  DS    F                                                        14920000
CESDUSED DS    F                                                        14930000
ADCARNUM DS    F                                                        14940000
ADCRDOUT DS    F                                                        14950000
OVLYSW   DC    XL1'0'                                                   14960000
SZPARM   DS    CL24                                                     14970000
SZPRMSW  DC    XL1'00'                                                  14980000
SZPRMLTH DS    H                                                        14990000
*                                                                       15000000
JOBCARD  DC    C'//RELNKXXX JOB 1,''JOE PROGRAMMER'''                   15010000
LEEXEC   DC    C'//STEPXXX  EXEC PGM=IEWL,REGION=512K,'               $ 15020000
BLEPARML DS    H                                                      $ 15030000
BLEPARM  DC    C'// PARM=('''                                         $ 15040000
LESYSOUT DC    C'//SYSPRINT DD  SYSOUT=A'                               15050000
LESYSUT1 DC    C'//SYSUT1   DD  SPACE=(CYL,(5,1)),UNIT=SYSDA  '       $ 15060000
LESYSLMD DC    C'//SYSLMOD  DD  DISP=OLD,'                              15070000
LEDSNAME DC    C'//             DSNAME='                                15080000
LEINPUT  DC    C'//         DD  DISP=SHR,'                              15090000
LESYSLIN DC    C'//SYSLIN   DD  *'                                      15100000
COPYBASE DC    C'     COPY INDD='                                       15110000
SELECT   DC    C'          SELECT    MEMBER='                           15120000
NUCINS01 DC    C'INSERT IEAANIP0'                                       15130000
NUCINS02 DC    C'INSERT IEAQFX00'                                       15140000
NUCINS03 DC    C'INSERT DOSBLKS'                                        15150000
NUCINS04 DC    C'INSERT IGFCCH'                                         15160000
         LTORG                                                          15170000
         EJECT                                                          15180000
SYSUT2   DCB   DDNAME=SYSUT2,MACRF=(PM),LRECL=80,BLKSIZE=80,           X15190000
               DSORG=PS,RECFM=FB                                        15200000
         EJECT                                                          15210000
SYSUT3   DCB   DDNAME=SYSUT3,MACRF=(PM),LRECL=80,BLKSIZE=80,           X15220000
               DSORG=PS,RECFM=FB                                        15230000
         EJECT                                                          15240000
SYSPUNCH DCB   DDNAME=SYSPUNCH,MACRF=(PM),LRECL=80,                   $X15250000
               DSORG=PS,RECFM=FB                                        15260000
         EJECT                                                          15270000
DDCB     DCB    MACRF=(RP),BLKSIZE=256,DSORG=PS,RECFM=F,SYNAD=DDCBIOER  15280000
         EJECT                                                          15290000
OUTDCB   DCB   MACRF=(RP),BLKSIZE=256,DSORG=PS,RECFM=F,SYNAD=OUTIOER, $X15300000
               EXLST=XLIST1                                           $ 15310000
         EJECT                                                          15320000
INDCB    DCB    EXLST=XLIST1,DSORG=PO,MACRF=(R),RECFM=U,BLKSIZE=7294    15330000
         EJECT                                                          15340000
ZZRELCOM DS    0D                                                       15350000
         TITLE 'Z Z R E L C O M  -  ZZRELINK COMMUNICATIONS AREA'       00010000
**********       C O M M U N I C A T I O N    A R E A        ********** 00020000
**********               Z Z R E L I N K                     ********** 00030000
*                                                                       00040000
* THE FOLLOWING DESCRIPTION IS FOR ZZRELINK.                            00050000
* IT IS BASICALLY THE SAME FOR THE IEBCOPY PROCEDURE EXCEPT THAT        00060000
* IT DOES NOT GO THROUGH ALL THE SORTING STEPS. IT ALSO DOES NOT BUILD  00070000
* MULTIPLE DD TABLES AND IT DOES NOT ALLOW FOR RENAMES.                 00080000
*                                                                       00090000
         SPACE 2                                                        00100000
         SPACE 2                                                        00110000
* ABBREVIATIONS USED FREQUENTLY -                                       00120000
*        ODE = OUTPUT DIRECTORY ENTRY                                   00130000
*        IDE = INPUT DIRECTORY ENTRY                                    00140000
         SPACE 2                                                        00150000
*                                                                       00160000
* SETAB = SELECT/EXCLUDE TABLE                                          00170000
*        THE SETAB CONSISTS OF 10-BYTE ENTRIES.  ENTRIES ARE MADE TO    00180000
*        THIS TABLE WHEN -                                              00190000
*              1) A SELECTIVE LINK HAS BEEN SPECIFIED.  EACH ENTRY IS   00200000
*              FOR THE NAME OF A MEMBER TO BE SELECTED.  IF A MEMBER IS 00210000
*              TO BE RENAMED, TWO 10-BYTE ENTRIES ARE MADE IN THE SETAB 00220000
*              2) AN EXCLUSIVE LINK HAS BEEN SPECIFIED.  EACH ENTRY IS  00230000
*              FOR THE NAME OF A MEMBER TO BE EXCLUDED.                 00240000
*        NOTE- IN THE CASE OF A SELECTIVE LINK (AND ONLY IN THIS CASE), 00250000
*              THE SETAB WILL ALSO BE USED AS THE CTLTAB.  IN ALL OTHER 00260000
*              CASES, A SEPARATE CTLTAB WILL BE CONSTRUCTED.            00270000
*        BYTE 0 OF AN ENTRY IS DESIGNATED AS SEFLAG1.  WITHIN THIS BYTE 00280000
*        THE FOLLOWING BITS HAVE MEANING -                              00290000
SEBIT1   EQU   X'80' ON=THIS IS A NEWNAME ENTRY                         00300000
SEBIT2   EQU   X'40' ON=THIS IS A RENAMED ENTRY                         00310000
SEBIT3   EQU   X'20' ON=REPLACE OPTION WAS SPECIFIED FOR THIS MEMBER    00320000
SEBIT4   EQU   X'10' ON=DONTLINK FLAG...DO NOT PROCESS THIS ENTRY       00330000
SEBIT5   EQU   X'08' ON= THIS MEMBER HAS BEEN ''FOUND'' ON INPUT D.S.   00340000
SEBIT6   EQU   X'04' ON= THIS IS LAST ENTRY IN SETAB/CTLTAB             00350000
*        LO ORDER 2 BITS NOT USED                                       00360000
*        NOTE THE DEFINITION OF SEBIT3.  IF, IN THE INDD TABLE, BYTE 0  00370000
*              BIT 2 IS ON, THIS MEANS THAT THE REPLACE OPTION WAS SPE- 00380000
*              CIFIED FOR ALL MEMBERS COPIED FROM THIS INDD.  IT IS     00390000
*              VALID FOR BOTH OF THESE BITS TO BE ON AT THE SAME TIME,  00400000
*              ALTHOUGH IF THE BIT IS ON IN THE INDD TABLE, IT IS UN-   00410000
*              NECESSARY FOR SEBIT3 TO ALSO BE ON IN THE SETAB.         00420000
         EJECT                                                          00430000
* FOLLOWING IS A DESCRIPTION OF THE CONTROL TABLE -CTLTAB-              00440000
* (REMEMBER THAT THIS TABLE IS PHYSICALLY THE SAME TABLE AS SETAB WHEN  00450000
* A SELECTIVE LINK OPERATION IS SPECIFIED, BUT IS PHYSICALLY INDEPEN-   00460000
* DENT AND DISTINCT FROM THE SETAB FOR AN EXCLUSIVE LINK OPERATION.     00470000
* THERE IS NO SETAB FOR A FULL LINK OPERATION, AND THE CTLTAB IN THIS   00480000
* CASE IS CONSTRUCTED SIMILARLY TO WHEN AN EXCLUSIVE LINK IS SPECIFIED. 00490000
* SINCE THE SETAB AND CTLTAB ARE ONE AND THE SAME FOR A SEL. LINK, THIS 00500000
*  DESCRIPTION WILL ASSUME THAT A SELECTIVE LINK IS BEING DONE, FOR THE 00510000
*  PURPOSE OF SETTING UP THE TABLE INITIALLY.)....                      00520000
         SPACE 2                                                        00530000
*        INITIAL TABLE, FOLLOWING CCSCAN PROCESSING OF ''SELECT'' -     00540000
*********************************************************************** 00550000
*SEFLAG1 *SEFLAG2 * NAME OF MEMBER TO BE SELECTED, OR, IF SPECIFIED,  * 00560000
*(DESCR. *(UNUSED)* NEWNAME.  IF NEWNAME WAS SPECIFIED, THE TABLE WILL* 00570000
* ABOVE) *        * CONTAIN 1 ENTRY FOR OLDNAME AND ANOTHER FOR NEW.  * 00580000
*********************************************************************** 00590000
* 1 BYTE * 1 BYTE *----------------------8 BYTES----------------------* 00600000
         SPACE 2                                                        00610000
* OLDNAME/NEWNAME PAIRS ARE NOW EXTRACTED FROM THE SETAB.  THEN THE     00620000
* CTLTAB IS SORTED ALPH. BY MBRNAME, AND A NEWNAME PTRTABLE SET UP.     00630000
* WHEN THIS HAS BEEN DONE, THE INPUT DATA SET'S DIRECTORY IS SEARCHED   00640000
* FOR MATCHING NAMES (THE NEWNAME ENTRIES IN THE TABLE ARE NOT USED FOR 00650000
* THIS COMPARISON).  WHEN A MATCHING MEMBERNAME IS FOUND, THE DIRECTORY 00660000
* ENTRY IS RETAINED IN CORE (IF SPACE PERMITS), OR IT IS SPILLED ONTO   00670000
* SYSUT3.  IN EITHER CASE, THE CORE ADDRESS OR THE TTR+INDICATOR ARE    00680000
* PUT INTO THE CTLTAB, OVERLAYING THE HIGH-ORDER 4 BYTES OF 'OLDNAME'.  00690000
* THE MEMBER-TTR IS EXTRACTED FROM THE DIRECTORY ENTRY, AND OVERLAYS    00700000
* THE LOW-ORDER 4 BYTES OF 'OLDNAME' IN THE CTLTAB.  SEBIT5 IS TURNED   00710000
* ON.  IF THIS IS AN ALIAS ENTRY, SEFLAG2 IS SET AS FOLLOWS -           00720000
ALIAS    EQU   X'80'         TO TEST FOR AND SET ALIAS DIRECTORY ENTRY  00730000
         SPACE 2                                                        00740000
*        CTLTAB ENTRY FOR A ''FOUND'' MEMBER -                          00750000
         SPACE 2                                                        00760000
*********************************************************************** 00770000
*SEFLAG1 *SEFLAG2 * INDIC. * ADDR OF THE IN-* ZEROES *   MEMBER TTR   * 00780000
*        *        *  BYTE  * PUT DIR. ENTRY *        *                * 00790000
*********************************************************************** 00800000
*-1 BYTE-*-1 BYTE-*-1 BYTE-*-----3 BYTES----*-1 BYTE-*-----3 BYTES----* 00810000
         SPACE 2                                                        00820000
* THE INDICATOR BYTE IS ZEROES IF THE DIRECTORY ENTRY IS IN CORE, OR IT 00830000
* IS HEX '01' IF THE DIRECTORY ENTRY WAS SPILLED TO SYSUT3.             00840000
* NOTE THAT CTLTAB ENTRIES FOR A ''NEWNAME'' ARE NOT OVERLAYED OR AL-   00850000
* TERED AT ANY TIME.  ONCE THE ENTIRE INPUT DIRECTORY HAS BEEN SCANNED, 00860000
* (OR AT LEAST ALL ENTRIES FOR MEMBERS TO BE COPIED FROM THIS INPUT     00870000
* DATA SET HAVE BEEN BUILT), THE OUTPUT DATA SET DIRECTORY IS READ.     00880000
* MEMBERNAMES OF MEMBERS CURRENTLY IN THE OUTPUT DATA SET ARE COMPARED  00890000
* AGAINST MEMBERNAMES OF MEMBERS REFERENCED IN THE CTLTAB FOR THE CUR-  00900000
* RENT INPUT DATA SET, UNLESS THE LATTER WERE RENAMED.  IF THE INPUT    00910000
* MEMBER IS RENAMED, THEN THE NEWNAME IS COMPARED AGAINST THE OUTPUT.   00920000
* IF DUPLICATE NAMES ARE ENCOUNTERED, AND IF THE REPLACE OPTION WAS NOT 00930000
* SPECIFIED ON EITHER THE INDD LEVEL OR THE MEMBERNAME LEVEL, THEN THE  00940000
* DONT-LINK BIT (SEBIT4) IS SET IN THE FLAG BYTE OF THE APPROPRIATE     00950000
* CTLTAB ENTRY, AND THE INDIC. BYTE (BYTE 3) OF THIS ENTRY IS SET TO    00960000
* HEX 'FF'.                                                             00970000
* THINK OF THE LOW-ORDER 8-BYTES OF EACH ''FOUND'' CTLTAB ENTRY AS BE-  00980000
* ING DIVIDED INTO A LEFT HALF (INDIC. + DIR. ENTRY ADDR.) AND A RIGHT  00990000
* HALF (ZEROES + MBR. TTR).  THE LEFT HALF NOW REPRESENTS DIRECTORY     01000000
* ENTRIES FOUND IN THE CURRENT INPUT DATA SET, AND IS IN ALPHAMERIC     01010000
* SEQUENCE.                                                             01020000
         EJECT                                                          01030000
* THE NEXT STEP IN CTLTAB PROCESSING CAUSES THE ''FOUND'' ENTRIES TO BE 01040000
* MANIPULATED, WITH THE RESULT BEING THAT THE LEFT HALF CONTAINS (IN-   01050000
* DIC + ADDR OF DIR. ENTRY) IN SEQUENCE BY MEMBER TTR, AND THE RIGHT    01060000
* HALF CONTAINS THIS SAME INFORMATION (INDIC. + ADDR OF DIR. ENTRY) IN  01070000
* ALPHAMERIC SEQUENCE, OVERLAYING THE ACTUAL MEMBER TTR.  ANOTHER RE-   01080000
* SULT OF THIS MANIPULATION OF THE CTLTAB IS THAT MAIN-MEMBER ENTRIES   01090000
* PRECEDE THE CORRESPONDING ALIAS ENTRIES.  NOTE THAT THE BITS SET IN   01100000
* SEFLAG1 ARE NOW ONLY USEFUL FOR THE ''RIGHT HALF'' OF THE CTLTAB,     01110000
* SINCE THEY ARE NOT MANIPULATED AND THUS REMAIN IN THE ORIGINAL (AL-   01120000
* PHABETIC) SEQUENCE.                                                   01130000
         SPACE 2                                                        01140000
*        CTLTAB ENTRY FOR A FOUND MEMBER FOLLOWING TTR SORT -           01150000
         SPACE 2                                                        01160000
*********************************************************************** 01170000
*SEFLAG1 *SEFLAG2 * INDIC. * ADDR OF IN. DE * INDIC. * ADDR OF IN. DE * 01180000
*        *        *  BYTE  *                *  BYTE  *                * 01190000
*        *        *IN SEQ BY MEMBER TTR     *IN SEQ ALPHAMERICALLY    * 01200000
*********************************************************************** 01210000
* 1 BYTE-*-1 BYTE-*-1 BYTE-*-----3 BYTES----*-1 BYTE-*-----3 BYTES----* 01220000
         SPACE 2                                                        01230000
* AT THIS POINT, THERE IS NO LOGICAL RELATIONSHIP BETWEEN THE LEFT AND  01240000
* RIGHT HALVES OF THE RELEVANT CTLTAB ENTRIES.  BOTH HALVES CONTAIN THE 01250000
* SAME INFORMATION, BUT IT IS IN TWO DIFFERENT SEQUENCES.  NOTE THAT,   01260000
* IF A MEMBER BEING LOOKED FOR IS NOT ''FOUND'', ITS CTLTAB ENTRY RE-   01270000
* MAINS UNALTERED - THE NAME IS STILL UN-OVERLAYED.                     01280000
* FROM THIS POINT ON, THOSE MEMBERS OF THE INPUT DATA SET WHOSE DIREC-  01290000
* TORY ENTRIES ARE REFERENCED IN THE CTLTAB WILL BE COPIED, PROVIDED    01300000
* THAT THEY ARE ''FOUND'' AND NOT FLAGGED AS ''DONT-LINK''.  THEN THE   01310000
* DIRECTORY ENTRIES WILL BE MERGED.  AS THE MERGE IS PERFORMED, WHEN AN 01320000
* INPUT DE IS MERGED, IF THIS IS A SELECTIVE LINK, THE DONT-LINK BIT    01330000
* (SEBIT4) IS TURNED ON, THUS ALLOWING FOR THESE ENTRIES IN THE CTLTAB  01340000
* TO BE IGNORED IN SUBSEQUENT PASSES THROUGH THE SAME TABLE FOR THE EN- 01350000
* SUING INPUT DATA SETS.                                                01360000
JSTCPD   EQU   X'10'              IF ON IN THE 2ND BYTE OF A CTLTAB EN- 01370000
*                                 TRY (SEFLAG2), THERE IS A NAME (CON-  01380000
*                                 TAINED IN THE 3RD THROUGH 10TH BYTES  01390000
*                                 OF THIS ENTRY) OF AN INPUT MEMBER     01400000
*                                 WHICH HAS BEEN SUCCESSFULLY COPIED.   01410000
*                                 THIS MEMBERNAME WILL BE PRINTED BY    01420000
*                                 THE TERMINATION MODULE (ZZRVTM) UNDER 01430000
*                                 APPROPRIATE CONDITIONS, AT WHICH TIME 01440000
*                                 THIS BIT WILL BE TURNED OFF.          01450000
*********************************************************************** 01460000
         TITLE 'Z Z R E L I N K  -  CONTROL, DDNAME, AND SELECT TABLES' 01470000
*XXXXXXXXXX    EVERY CONSTANT BETW. HERE AND NXT X'S MUST BE CONTIGUOUS 01480000
* ALL CONSTANTS BETWEEN X'S ARE SET TO ZERO FOR EVERY INPUT DATA SET.   01490000
* THE NAMES OF THE CONSTANTS WHICH ARE INCLUDED IN THIS CONTIG. AREA -  01500000
* CDCT, FCT, NNTCT, OLDTTR, FLG5                                        01510000
* ICPT,SVLSTO,SVFSTO,FLG2,FLG6,SWITCH1,UTTRFLAG,OBCT                    01520000
         SPACE 2                                                        01530000
*********************************************************************** 01540000
INBEGIN  DC    F'0'          ADDRESS OF START OF INDD TABLE             01550000
REPLACOP EQU   SEBIT3        IF ON IN HI-ORDER BYTE OF AN INDD-TABLE    01560000
*                            ENTRY, THE REPLACE OPTION WAS SPECIFIED    01570000
*                            FOR THIS INPUT DATA SET.                   01580000
SEBEGIN  DC    AL4(0)        ADDR OF BEGINNING OF SETAB                 01590000
SESTOP   DC    F'0'          IF SELECTIVE OR EXCLUSIVE LINK, THIS IS    01600000
*                            THE ADDRESS (+1) OF THE END OF THE SETAB.  01610000
*                            IF FULL LINK, ADDR (+1) OF END OF INDDTAB. 01620000
ADNNPTRT DS    AL4           ADDR OF NEWNAME POINTER TABLE              01630000
CTAD     DS    AL4           ADDR OF CONTROL TABLE - IF SELECTIVE LINK, 01640000
*                            THIS WILL BE SAME AS SEBEGIN.              01650000
ENCT     DC    HL2'0'        COUNT OF NBR OF ENTRIES IN CTLTAB (TOTAL)  01660000
*              IF THIS IS A SELECTIVE LINK, THIS COUNT WILL BE INI-     01670000
*              TIALIZED TO THE NUMBER OF ENTRIES IN SETAB.              01680000
COUNT    DC    HL2'0'        COUNT OF NBR OF ENTRIES IN SETAB IF SEL OR 01690000
*                            EXCL LINK, OR ZERO IF FULL LINK.           01700000
INDDCT   DC    AL2(0)        COUNT OF NO. INDD'S IN CURRENT STEP        01710000
NNCT1    DC    AL2(0)        NBR OF NEWNAMES SPECIFIED IN CURRENT SE-   01720000
*                            LECTIVE LINK STEP (IF NOT SEL CPY, = 0)    01730000
         DS    0D                                                       01740000
OUTNAME  DC    CL8' '        NAME OF CURRENT OUTPUT DD                  01750000
         SPACE 2                                                        01760000
CCIMAGE  DS    10D                     SCAN CONTROL CARD BUFFER         01770000
         SPACE 2                                                        01780000
WKA1     DC    10F'0'        VOLATILE WORK AREA FOR GENERAL USE         01790000
*                                                                       01800000
*****   PLEASE NOTE---    ZZRESCAN IS NOT THE ONLY MODULE WHICH USES    01810000
**                WKA1 FOR TEMPORARY WORK SPACE.                        01820000
*                 HOWEVER, THE FOLLOWING 4 EQUATES ARE USED ONLY BY     01830000
*                 ZZRESCAN -                                            01840000
*                                                                       01850000
SARG     EQU   WKA1          TEMPORARY WORK AREA USED BY ZZRESCAN       01860000
*                            TO HOLD SEARCH ARGUMENT 8 BYTES LONG       01870000
SAVEPAPR EQU   WKA1+8        TEMPORARY WORK AREA USED BY ZZRESCAN       01880000
*                            TO HOLD PARTIAL PARAMETERS IF CONTROL CARD 01890000
*                            IS CONTINUED, 8 BYTES LONG.                01900000
LEFTPCNT EQU   WKA1+16       TEMPORARY WORK AREA USED BY ZZRESCAN       01910000
*                            TO HOLD COUNT OF LEFT PARENTHESIS SCANNED. 01920000
RGHTPCNT EQU   WKA1+18       TEMPORARY WORK AREA USED BY ZZRESCAN       01930000
*                            TO HOLD COUNT OF RIGHT PARENTHESIS SCANNED 01940000
*                            BOTH LEFTPCNT AND RGHTPCNT 2 BYTES EACH    01950000
*                            AND MUST BE CONTIGUOUS STORAGE.            01960000
CSTOREG  DC    3F'0'         SAVE AREA USED BY ZZRESCAN                 01970000
         SPACE 2                                                        01980000
*        REGISTER SAVE AREAS                                            01990000
SV1      DC    18F'0'        REGISTER SAVE AREA FOR MAINFLOW            02000000
SV2      DC    18F'0'        REGISTER SAVE AREA FOR NON-RESIDENT RTNES  02010000
MCAMOD   DC    10F'0'             ZZRELCOM CHANGE AREA                  02020000
* MCAMOD IS A PATCH-AREA FOR USE IN MAINTENANCE OF THIS PROGRAM         02030000
         TITLE 'POINTERS FOR INTER-MODULE COMMUNICATION'                02040000
VZZRSCN  DC    V(ZZRESCAN)         ADDR OF CONTROL CARD SCAN ROUTINE    02050000
VZZRLEOF DC    V(ZZRLEOF) EP ADDRESS OF SYSIN EODAD EXIT IN ZZRESCAN    02060000
VZZRLMES DC    V(ZZMESAGE)         ADDRESS OF MESSAGE WRITER ROUTINE    02070000
VZZRTERM DC    V(ZZRTERM)         ADDRESS OF TERMINATION ROUTINE        02080000
         ORG   VZZRTERM                                                 02090000
AZZRTERM DS    F                  ADDRESS OF TERMINATION ROUTINE        02100000
         TITLE 'S Y S I N    D A T A    C O N T R O L    B L O C K'     02110000
*CARDCB  DCB   DDNAME=SYSIN,RECFM=FB,LRECL=80,EODAD=VZZRLEOF,           02120000
*              MACRF=(GM),DSORG=PS                                      02130000
CARDCB   DCB   DDNAME=SYSIN,RECFM=FB,LRECL=80,EODAD=VZZRLEOF,          X02140000
               MACRF=(GM),DSORG=PS                                      02150000
         TITLE 'S Y S P R I N T    D A T A    C O N T R O L    B L O C X02160000
               K'                                                       02170000
* PRTDCB DCB   DDNAME=SYSPRINT,DSORG=PS,MACRF=(PM),RECFM=FBA,           02180000
*              LRECL=121,BLKSIZE=121                                    02190000
PRTDCB   DCB   DDNAME=SYSPRINT,DSORG=PS,MACRF=(PM),RECFM=FBA,          X02200000
               LRECL=121,BLKSIZE=121                                    02210000
         TITLE 'SWITCH AND WORK AREA DEFINITIONS FOR ZZRESCAN'          02220000
* THE FOLLOWING SWITCHES (PARMSWCH, CCSWITCH, COMDCDSW, CPARAMSW, CCDE- 02230000
* LIM, CCDELIM2) ARE PRIMARILY BUT NOT SOLELY USED BY ZZRESCAN -        02240000
*                                                                       02250000
         SPACE 1                                                        02260000
PARMSWCH DC    XL1'0'    SCAN INTERNAL INDD AND MEMBER SWITCHES         02270000
SCANNAME EQU   X'80'         SCANNING NAME                              02280000
SET4REPL EQU   X'40'         MULTIPLE ( EXPECT REPLACE                  02290000
ONELEFT  EQU   X'20'         FIRST LEFT PARENTHESIS                     02300000
LINKNOW  EQU   X'10'         NOW SCANNING LINK CARD                     02310000
STOPSCAN EQU   X'08'         BLANK ENCOUNTERED                          02320000
FLUSHSW  EQU   X'04'                   FLUSH TO NEXT LINK RE/SET ALONE  02330000
HASNEWNM EQU   X'02'         HAVE A NEW NAME WITH MEMBER                02340000
COMDPART EQU   X'01'         PARTIAL COMMAND- CONTINUED ON NEXT CARD    02350000
*                                                                       02360000
         SPACE 1                                                        02370000
CCSWITCH DC    XL1'0'                  CONTROL CARD SWITCHES- EXTERNAL  02380000
CARDPRTD EQU   X'80'         ON = CONTROL STATEMENT HAS BEEN PRINTED    02390000
SYSINEOF EQU   X'40'         END OF FILE ON SYSIN                       02400000
UNECPARN EQU   X'20'         INDD/MEMBER NAMES IMBEDDED IN PARENTHESIS  02410000
ZZRLINKC EQU   X'10'         ZZRLINK CONTROL CARDS                      02420000
COMDNOW  EQU   X'08'         COMMAND WORD                               02430000
LASTPARM EQU   X'04'         LAST PARAMETER- BYPASS SWITCH              02440000
MULTSE   EQU   X'02' MULTIPLE SELECT/EXCLUDE STATEMENTS                 02450000
FIRSTSCN EQU   X'01' ON=ZZRESCAN HAS BEEN CALLED FOR THE FIRST TIME     02460000
*                                                                       02470000
         SPACE 2                                                        02480000
COMDCDSW DC    XL1'0'           SOME EXTERNAL SWITCHES                  02490000
LINKDONE EQU   X'80'         LINK COMMAND SCANNED ALL OK                02500000
SELECTSC EQU   X'40'         SELECT COMMAND SCANNED                     02510000
EXCLUDES EQU   X'20'         EXCLUDE COMMAND SCANNED                    02520000
NEWOUT   EQU   X'10'         OUTDD KEYWORD PRESENT                      02530000
NEWINDD  EQU   X'08'         INDD KEYWORD PRESENT                       02540000
LISTSW   EQU   X'04'         DO NOT LIST MEMBERS COPIED (LIST=NO)       02550000
COMPRESS EQU   X'02'                   COMPRESS LINK DATA SET           02560000
MEMBRCD1 EQU   X'01'         MEMBER STATEMENT                           02570000
*                                                                       02580000
         SPACE 2                                                        02590000
CPARAMSW DC    XL1'0'       INTERNAL SCAN SWITCHES                      02600000
DELIMEND EQU   X'80'         DELIMITER IN COLUMN 71                     02610000
CONTINY  EQU   X'40'         CONTINUATION                               02620000
PARMCOME EQU   X'20'         PARAMETER FOLLOWS                          02630000
PARTPARM EQU   X'10'         PARTIAL PARAMETER                          02640000
READ1    EQU   X'08'         READ ANOTHER CONTROL STATEMENT             02650000
COMDPARM EQU   X'04'         COMMAND FOLLOWED BY PARAMETER              02660000
COL72BLK EQU   X'02'         COLUMN 72 NOT BLANK                        02670000
PARMZERO EQU   X'01'         PARAMETER LENGTH ZERO                      02680000
*                                                                       02690000
         SPACE 2                                                 A48742 02700000
SCANSWCH DC    XL1'0'        INTERNAL SCAN SWITCHES              A48742 02710000
NOCMMEXP EQU   X'80'         DON'T SCAN COMMAND ON CONTIN CARDS  A48742 02720000
*              LOW ORDER 7 BITS NOT USED - RESERVED              A48742 02730000
*                                                                       02740000
         SPACE 2                                                        02750000
CCDELIM  DC    XL1'0'        INTERNAL SCAN SWITCHES                     02760000
EQUALSGN EQU   X'80'         EQUAL SIGN                                 02770000
COMMASGN EQU   X'40'         COMMA                                      02780000
LEFTPRSG EQU   X'20'         LEFT PARENTHESIS                           02790000
RIGHTPRS EQU   X'10'         RIGHT PARENTHESIS                          02800000
BLANKSGN EQU   X'08'         BLANK                                      02810000
LASTCOMA EQU   X'04'         LAST DELIMITER A COMMA- READ A CARD        02820000
BADBLOCK EQU   X'02' VALIDATE-ZZRDV0 SETS IF SYSIN/SYSPRINT BLOCKSIZE   02830000
*          IS BAD.                                                      02840000
*              LO ORDER BIT NOT USED                                    02850000
*                                                                       02860000
         SPACE 2                                                        02870000
CCDELIM2 DC    XL1'0'  USED TO SAVE SETTINGS OF CCDELIM ON CONTINUATION 02880000
*                                                                       02890000
         TITLE 'ERROR FLAG DEFINITIONS USED BY OPEN FAILURE ROUTINE'    02900000
IOEF2    DC    X'00'              FLAGS DESCRIBING NATURE/TYPE OF I/O   02910000
*                                 ERROR                                 02920000
ERF9     EQU   X'80'         ON = 'HARD' ERROR WRITING MERGED OUTPUT    02930000
*                                 DIRECTORY TO SYSUT4                   02940000
ERF10    EQU   X'40'         ON = ERROR READING FROM SYSUT4.  IF 'ERF4' 02950000
*                                 OFF, ERROR OCCURRED DURING MERGE      02960000
*                                 PHASE OF PROGRAM - IF 'ERF4' ON, SEE  02970000
*                                 DESCRIPTION OF 'ERF4'.                02980000
NOSYSIN  EQU   X'20'         ON = SYSIN COULD NOT BE OPENED OR WAS IN-  02990000
*                                 VALID, OR BECAME UNUSABLE DUE TO AN   03000000
*                                 I/O ERROR                             03010000
SPRNOPN  EQU   X'10'         SYSPRINT COULD NOT BE OPENED, WAS INVALID- 03020000
*                                 LY SPECIFIED, OR AN I/O ERROR OCCUR-  03030000
*                                 RED MAKING SYSPRINT UNAVAILABLE       03040000
*              LOW ORDER 4 BITS NOT USED - RESERVED                     03050000
*                                                                       03060000
         TITLE 'SWITCH AND WORK AREA DEFINITIONS FOR ZZMESAGE'          03070000
         DS    0H                                                       03080000
MSGLIST  DC    4H'0'              AREA FOR PARAMETRIC INPUT TO ZZMESAGE 03090000
MSG1     EQU   MSGLIST                                                  03100000
MSG2     EQU   MSG1+2                                                   03110000
MSG3     EQU   MSG2+2                                                   03120000
MSG4     EQU   MSG3+2                                                   03130000
* THE FOLLOWING BITS WILL BE SET ON BY THE CALLER OF ZZMESAGE (MESSAGE  03140000
* WRITING ROUTINE) IN THE HIGH ORDER BYTE OF EACH APPROPRIATE HALFWORD  03150000
* IN THE MSGLIST PARAMETER(S) BEING USED -                              03160000
LASTMSG  EQU   X'80'         ON = LAST PARAMETER IN MSGLIST             03170000
CTLCD    EQU   X'40'         ON = A CONTROL CARD IS TO BE PRINTED       03180000
IOERF    EQU   X'20'         ON = A MESSAGE IS IN THE MESSAGE BUFFER -  03190000
*                                 AND IS TO BE PRINTED.  NO MSG CODE IS 03200000
*                                 ASSOCIATED WITH THIS MESSAGE, AND IT  03210000
*                            USUALLY WILL BE A SYNADAF MESSAGE.         03220000
RCODE    EQU   X'10'         ON = PUT RETURN CODE INTO THIS MSG TEXT    03230000
PBIT     EQU   X'08'         ON = USE PARAM LIST WITH THIS MSG TEXT     03240000
*              LO ORDER 3 BITS NOT USED - RESERVED                      03250000
         SPACE 2                                                        03260000
MSGPARAM DS    9H                 THIS FIELD IS TO CONTAIN PARAMETERS   03270000
*                                 TO BE PLACED IN MESSAGES              03280000
         ORG   MSGPARAM                                                 03290000
NAMEDISP DC    X'00'              THE DISPLACEMENT OF A NAME            03300000
*                                 PARAMETER FROM THE BEGINNING OF       03310000
*                                 THE MESSAGE IT IS TO BE PLACED IN     03320000
*                                 THIS BYTE                             03330000
NODISP   DC    X'00'              THE DISPLACEMENT OF A NUMBER          03340000
*                                 PARAMETER FROM THE BEGINNING OF       03350000
*                                 THE MESSAGE IT IS TO BE PLACED IN     03360000
*                                 THIS BYTE                             03370000
DDNMDISP DC    X'00'              DISPLACEMENT OF DDNAME FROM MSG START 03380000
         DS    0D                                                       03390000
NAMEFLD  DC    CL8' '        AREA CONTAINING NAME TO BE PUT INTO MSG    03400000
DDNMFLD  DC    CL8' '        AREA CONTAINING DDNAME TO INSERT IN MSG    03410000
DDVALNM  EQU   NAMEFLD       USED BY VALIDATE TO SAVE DD NAME           03420000
PARAMS   DC    X'00'                                                    03430000
NAME     EQU   X'80'         ON = THERE IS A NAME PARAMETER             03440000
NBR      EQU   X'40'         ON = THERE IS A NUMBER PARAMETER           03450000
DDNM     EQU   X'20'         ON = THERE IS A DDNAME PARAMETER           03460000
NOFLD    DC    CL7'0'        AREA CONTAINING NUMBER TO BE PUT INTO MSG  03470000
         TITLE 'MESSAGE CODE DEFINITIONS USED BY ALL ZZRELINK MODULES'  03480000
*********************************************************************** 03490000
*        THE FOLLOWING MESSAGE CODES ARE USED BY ZZRELINK, ZZRESCAN,  * 03500000
*    AND ZZMESAGE TO IDENTIFY AND PRODUCE ALL ZZRELINK MESSAGES.      * 03510000
*********************************************************************** 03520000
INALCNTR EQU     01                INVALID COMMAND OR KEYWORD           03530000
INVALSPR EQU     02                INVALID PARAMETER                    03540000
ONEQPARN EQU     03                UNEQUAL PARENTHESIS                  03550000
INVALCON EQU     04                INVALID CONTINUATION                 03560000
MEMNOSE  EQU     05                MEMBER WITHOUT SELECT/EXCLUDE        03570000
MULTSSEE EQU     06                ONLY ONE SELECT/EXCLUDE PER INDD     03580000
INVALREP EQU     07                INVALID REPLACE SPECIFIED            03590000
NULLPARM EQU     08                NULL PARAMETERS                      03600000
NORREN   EQU     09                CANNOT RENAME/REPLACE ON EXCLUDE     03610000
NOINDD   EQU     10                OUTDD OR INDD NOT SPECIFIED          03620000
INVALIST EQU   11                  OUTDD/LIST NOT ON LINK STATEMENT     03630000
ENDMESS  EQU   12                  END OF CONTROL CARDS                 03640000
MODEERR  EQU   13                  MIXING ZZRLINKAND ZZRDSCPY MODE      03650000
NOOVLYS  EQU   14                  WARNING - OVERLAYS NOT SUPPORTED     03660000
SCANMSG  EQU   15                  CONTROL STATEMENT ERROR              03670000
SEQERROR EQU   16                  STATEMENT SEQUENCE ERROR             03680000
GENERUAL EQU   17                  GENERAL VALIDATION MESSAGE           03690000
OPENERRX EQU   18                  OPEN ERROR MESSAGE                   03700000
OBTAINER EQU   19                  OBTAIN ERROR                         03710000
NOTPDSER EQU   20                  OBTAIN NOT PDS                       03720000
INVALREC EQU   21                  INVALID LRECL                        03730000
INVALBLK EQU   22                  INVALID BLOCKSIZE                    03740000
UNMOVEDS EQU   23                  DATA SET UNMOVABLE                   03750000
RECFMINC EQU   24                  RECFM INCOMPATIBLE                   03760000
NODIR    EQU   25                  UNABLE TO OPEN DIRECTORY DCB         03770000
DIRERR01 EQU   26                  ALIAS NAME X OCCURS MORE THAN ONCE   03780000
ATABFULL EQU   27                  ALIAS TABLE FULL                     03790000
EPAERR01 EQU   28                  UNABLE TO LOCATE EPA OF MODULE X     03800000
DIRERR02 EQU   29                  I/O ERROR READING DIRECTORY FOR X    03810000
REPERR01 EQU   30                  MEMBER X NOT RELINKED - NO REP OPT   03820000
M39      EQU   31                  CANNOT COMPRESS WITH SELECT OR       03830000
*                                  EXLCUDE                              03840000
CESDIOER EQU   32                  I/O ERROR READING CESD RECORDS       03850000
NE       EQU   33                  LOAD MODULE IS 'NOT EDITABLE'        03860000
*                                  BUFFERS FOR COMPRESS                 03870000
M42      EQU   34 CANNOT SPECIFY DUPLICATE NAME FOR SEL/EXCL/RENAME     03880000
M43      EQU   35                  CANNOT PROCESS ALL OLD/NEW NAMES     03890000
*                                  SPECIFIED                            03900000
M45      EQU   36                  (DATA SET NAME) REFERENCES A NULL    03910000
*                                  INPUT DATA SET                       03920000
M46      EQU   37                  CANNOT RE/DE BLOCK WITH              03930000
*                                  NOTE-LIST/USER TTRN IN MEMBER        03940000
*                                  (MEMBER NAME)                        03950000
M47      EQU   38                  CANNOT CONTINUE TO BUILD CTLTAB      03960000
M48      EQU   39                 ALL SELECTED MEMBERS COPIED - DID NOT 03970000
*                                 USE ALL SPECIFIED INDD'S              03980000
M49      EQU   40                  (NUMBER) UNUSED TRKS IN OUTPUT DATA  03990000
*                                  SET REFERENCED BY (DDNAME)           04000000
M50      EQU   41                  CANNOT COMPRESS TRACK OVERFLOW DATA  04010000
*                                  SET                                  04020000
M51      EQU   42                  CANNOT COMPRESS WITH RE/DE BLOCKING  04030000
M53      EQU   43                  END OF JOB (0,4,8) WAS HIGHEST       04040000
*                                  SEVERITY CODE                        04050000
NORMOD   EQU   44                  NO SPACE IN OUTPUT DIRECTORY FOR     04060000
*                                  DIRECTORY ENTRIES FROM DATA SET      04070000
*                                  (DATA SET NAME)                      04080000
UNUSDDB  EQU   45                  THERE ARE (NUMBER) UNUSED DIRECTORY  04090000
*                                  BLOCKS IN THE OUTPUT DIRECTORY       04100000
TMDBTR   EQU   46     **WARNING** THE OUTPUT DS REF BY XXXXXXXX  A36049 04110000
*                     CONTAINS TOO MANY DIRECTORY BLOCKS PER     A36049 04120000
*                     TRACK                                      A36049 04130000
M58      EQU   47                  ERROR FORCES JOB TO TERMINATE        04140000
M59      EQU   48                  (MEMBER NAME) COMPRESSED- WAS        04150000
*                                  ALREADY IN PLACE                     04160000
M60      EQU   49                  ALL MEMBERS COMPRESSED-              04170000
*                                  ALL WERE ORIGINALLY COMPRESSED       04180000
MEMCOP   EQU   50                  (MEMBERNAME) HAS BEEN SUCCESSFULLY   04190000
*                                  COPIED                               04200000
RNMEMCOP EQU   51                  (MEMBER NAME) HAS BEEN RENAMED AND   04210000
*                                  SUCCESSFULLY COPIED                  04220000
NOTDA    EQU   52                 DATA SET NOT DIRECT ACCESS            04230000
NODDCARD EQU   53                 DD CARD NOT FOUND                     04240000
UNITER01 EQU   54                 UNIT TYPE DEFINED BY X NOT SUPPORTED  04250000
NOMBCPDM EQU   55                 NO MBRS COPIED FROM INPUT DATASET RE- 04260000
*                                 FERENCED BY (XXXXXXXX)                04270000
CONCATBD EQU   56                 CONCATENATED DATA SETS                04280000
IMPCOMPR EQU   57                 IMPLIED COMPRESS                      04290000
NOCMPOSS EQU   58                 CANNOT COMPRESS                       04300000
NOLINK   EQU   59                 NO MEMBERS FOR PARTIAL LINK,          04310000
*                                 WILL NOT LINK                         04320000
DOFULLCP  EQU  60                 TOTAL LINK ASSUMED                    04330000
MFBNC    EQU   61 MEMBER FOUND BUT NOT COPIED - I/O ERROR READING       04340000
*                 INPUT DIRECTORY                                       04350000
NONELINK EQU   62 NO MEMBERS COPIED                                     04360000
FOLLMCPD EQU   63 FOLLOWING MBRS COPIED FROM INPUT DS REF BY XXXXXXXX   04370000
PLAMPID  EQU   64 POSSIBLE LOSS OF ACCESS TO MEMBER AND/OR INCOMPLETE   04380000
*                 DIRECTORY                                             04390000
WODINC   EQU   65 SYSUT4 I/O ERROR - OUTPUT DIRECTORY MAY BE INCOMPLETE 04400000
WONTCOM  EQU   66 I/O ERROR ON SYSUT3 - COMPRESS IN PLACE NOT DONE      04410000
BADPRINT EQU   67 SYSPRINT COULD NOT BE OPENED                          04420000
SMNF    EQU   68                  (MBRNAME) WAS SELECTED BUT NOT FOUND  04430000
PA       EQU   69                  LOAD MODULE IS PAGE ALIGNED        $ 04440000
         SPACE 2                                                        04450000
RCBUF    DC    C'0'               COMPLETION-CODE AREA...CONTAINS CHAR- 04460000
*                                 ACTER REPRESENTATION OF HIGHEST       04470000
*                                 COMPLETION CODE SET BY UTILITY PGM    04480000
LINECT   DC   X'0'                COUNT OF NBR LINES WRITTEN ON ONE PG  04490000
PGLIMIT  EQU   56                 MAX NBR OF LINES TO BE PUT ON ONE PG  04500000
         SPACE 2                                                        04510000
MSGBUF   DC    121C' '            MESSAGE BUFFER                        04520000
PGNO     DC    C'0001'            STARTING PAGE NUMBER FOR MSG OUTPUT   04530000
*************              END OF COMMUNICATION AREA   **************** 04540000
***   KEEP CARD  'MCAEND' LAST IN COMMUNICATION AREA JUST BEFORE THE ** 04550000
****  EQUATE THAT DETERMINES THE COMMUNICATIONS AREA SIZE 'MCASIZE'  ** 04560000
         SPACE 1                                                        04570000
MCAEND   DS    0D        END OF COMMUNICATION AREA                      04580000
MCASIZE  EQU   MCAEND-ZZRELCOM    SIZE OF COMMUNICATIONS AREA FOR SNAPS 04590000
         EJECT                                                          15370000
         DS    0F                                                       15380000
TABCOR   DC    16X'FF'                                                  15390000
         DS    CL1024                                                   15400000
         END                                                            15410000
//LKED     EXEC PGM=IEWL,                                               00610000
//             PARM=(XREF,LIST,LET,'AC=1')                              00620000
//SYSPRINT DD  SYSOUT=*                                                 00630000
//SYSLIB   DD  DSN=&&SUBRS,DISP=(OLD,DELETE)                            00640000
//SYSLIN   DD  DSN=&&ZZRLOBJ,DISP=(OLD,DELETE)                          00650000
//         DD  *                                                        00660000
  NAME ZZRELINK(R)                                                      00670000
//SYSUT1   DD  UNIT=(SYSDA,SEP=(SYSLIN,SYSLMOD)),SPACE=(1024,(200,20))  00680000
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.LINKLIB                                00690000
