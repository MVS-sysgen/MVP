//SUPERGEN  JOB (TSO),
//             'Install SUPERGEN',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//VDBS$DDF EXEC ASMFCL                                                  00020000
//ASM.SYSIN DD *                                                        00030000
VDBS$DDF CSECT                                                          00010000
         ENTRY DDFIND                                                   00020003
DDFIND   EQU   *                                                        00030003
         USING VDBS$DDF,15                                              00040001
         STM   14,12,12(13)                                             00050000
         ST    13,SAVEAREA+4                                            00060000
         LR    12,13                                                    00070000
         CNOP  0,4                                                      00080000
         BAL   13,DDFINDS                                               00090000
SAVEAREA DC    18A(0)                                                   00100000
         USING SAVEAREA,13                                              00110000
         DROP  15                                                       00120000
DDFINDS  EQU   *                                                        00130000
         ST    13,8(12)                                                 00140000
         ST    14,DDRETURN                                              00150000
         MVC   PARMS,0(1)              DDNAME,TIOTADDR                  00160001
         EXTRACT  TIOT,FIELDS=TIOT                                      00170000
         SR    4,4                                                      00180000
         L     3,TIOT                                                   00190000
         LA    3,24(3)                                                  00200000
         L     5,PARMDD                DDNAME GIVEN                     00210000
DDLOOP   CLC   4(8,3),0(5)             IS THIS DDNAME                   00220000
         BE    DDFOUND                    YES                           00230000
         IC    4,0(3)                                                   00240000
         LTR   4,4                                                      00250000
         BZ    DDERROR                 NOT FOUND--EXIT                  00260000
         LA    3,0(3,4)                                                 00270000
         B     DDLOOP                                                   00280000
DDFOUND  EQU   *                                                        00290000
         L     5,PARMTIOT                                               00300002
         ST    3,0(5)                                                   00310000
         SR    15,15                                                    00320001
DDEXIT   EQU   *                                                        00330000
         L     13,SAVEAREA+4                                            00340000
         RETURN (14,12),T,RC=(15)                                       00350001
*************************************************************           00360000
DDERROR  EQU   *                                                        00370000
         LA    15,4                                                     00380002
         B     DDEXIT                                                   00390000
*************************************************************           00400000
*                                                                       00410000
TIOT     DC    A(0)                                                     00420000
*                                                                       00430000
DDRETURN DC    A(0)                                                     00440000
*                                                                       00450000
PARMS    DS    0CL8                                                     00460001
PARMDD   DC    A(0)                                                     00470000
PARMTIOT DC    A(0)                                                     00480001
         END                                                        
//LKED.SYSLMOD DD DSN=&SUBLOAD,UNIT=SYSDA,DISP=(,PASS),                 00050000
//             SPACE=(CYL,(3,1,3)),DCB=(SYS2.LINKLIB)                   00060000
//SYSIN    DD  *                                                        00070000
 NAME VDBS$DDF(R)                                                       00080000
//VDBS$HEX EXEC ASMFCL                                                  00090000
//ASM.SYSIN DD *                                                        00100000
VDBS$HEX CSECT                                                          00010000
         ENTRY HEXDUMP                                                  00020000
         ENTRY HEXDUMP2                                                 00030000
         ENTRY HEXDCB                                                   00040000
*********************************************************************** 00050000
HEXDUMP  EQU   *                                                        00060000
         USING HEXDUMP,15                                               00070000
         B     DUMP1EP                                                  00080000
         DC    AL1(L'DUMP1LIT)                                          00090000
DUMP1LIT DC    C'HEXDUMP  - DUMP STORAGE IN HEX &SYSDATE &SYSTIME'      00100000
DUMP1EP  EQU   *                                                        00110000
         MVI   BRVECTOR,4                                               00120000
         LA    15,HEXME                                                 00130000
         BR    15                                                       00140000
*                                                                     * 00150000
HEXDUMP2 EQU   *                                                        00160000
         USING HEXDUMP2,15                                              00170000
         B     DUMP2EP                                                  00180000
         DC    AL1(L'DUMP2LIT)                                          00190000
DUMP2LIT DC    C'HEXDUMP2 - DUMP STORAGE IN HEX &SYSDATE &SYSTIME'      00200000
DUMP2EP  EQU   *                                                        00210000
         MVI   BRVECTOR,8                                               00220000
         LA    15,HEXME                                                 00230000
         BR    15                                                       00240000
*********************************************************************** 00250000
*********************************************************************** 00260000
*                                                                       00270000
*        DUMP FORMATTING ROUTINE                                        00280000
*********************************************************************** 00290000
*        R1-> A(TYPE,START,END,HDNG,DETAIL)---HEXDUMP                 * 00300000
*  OR    R1-> A(TYPE,START,LNGTH,HDNG,DETAIL)---HEXDUMP2              * 00310000
*        A(TYPE)->=C'RELO' PRINTS DATA RELATIVE TO 0                  * 00320000
*        A(TYPE)->=C'DATA' WILL PRINT 132 BYTES AS IS FROM START      * 00330000
*        A(TYPE)->=C'PAGE' WILL PAGE EJECT ON NEXT CALL               * 00340000
*        A(TYPE)->=C'CLOS' WILL CLOSE HEXDUMP DCB                     * 00350000
*        TYPE=ANYTHING ELSE WILL PRINT ACTUAL ADDRESS                 * 00360000
*    HDNG AND DETAIL ARE OPTIONAL AND IF SUPPLIED WILL BE USED        * 00370000
*        TO PRINT A HEADING AND/OR DETAIL LINE EACH 132 BYTES         * 00380000
*    ENTRY HEXDUMP2 IS USED FOR SECOND PARM LIST WHERE LNGTH IS       * 00390000
*        ADDRESS OF A FULLWORD CONTAINING BINARY LNGTH OF BYTES TO DUM* 00400000
*********************************************************************** 00410000
R1       EQU   1                                                        00420000
R2       EQU   2                                                        00430000
R3       EQU   3                                                        00440000
R4       EQU   4                                                        00450000
R5       EQU   5                                                        00460000
R6       EQU   6                                                        00470000
R7       EQU   7                                                        00480000
R8       EQU   8                                                        00490000
R9       EQU   9                                                        00500000
R10      EQU   10                                                       00510000
R11      EQU   11                                                       00520000
R12      EQU   12                                                       00530000
R13      EQU   13                                                       00540000
R14      EQU   14                                                       00550000
R15      EQU   15                                                       00560000
*********************************************************************** 00570000
         EJECT                                                          00580000
*********************************************************************** 00590000
         USING HEXME,15                                                 00600000
HEXME    EQU   *                                                        00610000
         SAVE  (14,12)                                                  00620000
         ST    13,SAVEAREA+4                                            00630000
         LR    12,13                                                    00640000
         CNOP  0,4                                                      00650000
         BAL   13,HEXXER                                                00660000
SAVEAREA DC    18A(0)                                                   00670000
         DROP  15                                                       00680000
         USING SAVEAREA,13                                              00690000
HEXXER   EQU   *                                                        00700000
         ST    13,8(12)                                                 00710000
HEXDDNOP NOP   HEXEXIT                 B IF BAD OPEN                    00720000
         XC    PARMS,PARMS             CLEAR                            00730000
         ICM   R3,7,1(R1)              A(TYPE)                          00740000
         STCM  R3,7,PARMTYP+1          POST                             00750000
         TM    0(R1),X'80'             LAST PARM                        00760000
         BNZ   HEXEOPRM                YES                              00770000
         ICM   R3,7,5(R1)              A(START)                         00780000
         STCM  R3,7,PARMSTRT+1         POST                             00790000
         TM    4(R1),X'80'             LAST PARM                        00800000
         BNZ   HEXEOPRM                YES                              00810000
         ICM   R3,7,9(R1)              A(END)                           00820000
         STCM  R3,7,PARMEND+1          POST                             00830000
         TM    8(R1),X'80'             LAST PARM                        00840000
         BNZ   HEXEOPRM                YES                              00850000
         ICM   R3,7,13(R1)             A(HEADING)                       00860000
         STCM  R3,7,PARMHDNG+1         POST                             00870000
         TM    12(R1),X'80'            LAST PARM                        00880000
         BNZ   HEXEOPRM                YES                              00890000
         ICM   R3,7,17(R1)             A(DETAIL)                        00900000
         STCM  R3,7,PARMDTL+1          POST                             00910000
HEXEOPRM EQU   *                                                        00920000
         L     3,PARMTYP               TYPE                             00930000
         CLC   0(4,3),=CL4'CLOS'       CLOSE IT                         00940000
         BE    HEXCLOSE                   YES                           00950000
         CLC   0(4,3),=CL4'PAGE'       PAGE EJECT                       00960000
         BE    HEXPAGE                    YES                           00970000
*                                                                       00980000
*        BRVECTOR = 4 FOR HEXDUMP                                       00990000
*        BRVECTOR = 8 FOR HEXDUMP2                                      01000000
*                                                                       01010000
         LA    1,0                                                      01020000
BRVECTOR EQU   *-1                                                      01030000
         B     *(1)                                                     01040000
         B     HEXCHK1                                                  01050000
         L     5,PARMEND               A(LENGTH)                        01060000
         L     5,0(R5)                 LENGTH                           01070000
         A     5,PARMSTRT              + START = END+1                  01080000
         BCTR  5,0                     NOW END                          01090000
         ST    5,PARMEND               POST                             01100000
HEXCHK1  EQU   *                                                        01110000
         L     R4,PARMSTRT                                              01120000
         L     R5,PARMEND                                               01130000
         CR    4,5                                                      01140000
         BNH   HEXOK                   NO MISTAKE IN START/END          01150000
         LA    5,31(4)                 SET FOR 1 LINE                   01160000
HEXOK    EQU   *                                                        01170000
         CLC   0(4,3),=CL4'ASIS'       ASIS ONLY                        01180000
         BE    HEXFIRST                   YES                           01190000
         OI    HEXRELO+1,X'F0'                                          01200000
         CLC   0(4,3),=CL4'RELO'                                        01210000
         BNE   HEXFIRST                                                 01220001
         NI    HEXRELO+1,X'0F'                                          01230000
*********************************************************************** 01240000
         EJECT                                                          01250000
*********************************************************************** 01260000
HEXFIRST EQU   *                                                        01270000
         NOP   HEXMORE                                                  01280000
         OI    *-3,X'F0'                                                01290000
         LA    1,DDPARM                                                 01300000
         L     15,=V(VDBS$DDF)                                          01310000
         BALR  14,15                                                    01320000
         B     *+4(15)                 CHECK RETURN CODE                01330000
         B     *+8                     0 -- OK                          01340000
         B     NODD                    4 -- NOT FOUND                   01350000
         OPEN  (LISTDCB,(OUTPUT))      OPEN LISTING FILE                01360000
         TIME  DEC                                                      01370000
         ST    1,FULL                                                   01380000
         UNPK  YYDDD,FULL                                               01390000
         OI    YYDDD+4,X'F0'                                            01400000
         MVC   PRNTYY,YEAR                                              01410000
         MVC   PRNTDDD,DAY                                              01420000
         ST    0,FULL                                                   01430000
         UNPK  HHMMSS,FULL                                              01440000
         MVC   PRNTHH,HOURS                                             01450000
         MVC   PRNTMM,MINUTES                                           01460000
         MVC   PRNTSS,SECONDS                                           01470000
         TM    LISTDCB+(DCBOFLGS-IHADCB),X'10'  IS IT OPEN              01480000
         BO    HEXMORE                                                  01490000
NODD     EQU   *                                                        01500000
         OI    HEXDDNOP+1,X'F0'                                         01510000
         B     HEXEXIT                                                  01520000
HEXCLOSE EQU   *                                                        01530000
         TM    LISTDCB+(DCBOFLGS-IHADCB),X'10'  WAS IT OPEN             01540000
         BZ    HEXEXIT                    NO                            01550000
         MVC   LINE+1(L'ENDMSG),ENDMSG                                  01560000
         BAL   14,PRINT                                                 01570000
         CLOSE (LISTDCB)                                                01580000
         NI    HEXFIRST+1,X'0F'                                         01590000
         B     HEXEXIT                                                  01600000
HEXMORE  EQU   *                                                        01610000
         MVI   LINE,C'0'                                                01620000
         L     2,PARMDTL                                                01630000
         LTR   R2,R2                                                    01640000
         BZ    HEXDO                                                    01650000
         MVC   LINE+1(132),0(2)        THE DETAIL LINE                  01660000
         BAL   14,PRINT                                                 01670000
HEXDO    EQU   *                                                        01680000
         L     2,PARMTYP                                                01690000
         CLC   0(4,2),=CL4'ASIS'       IS IT ASIS                       01700000
         BNE   HEXTYPE                                                  01710000
         L     2,PARMSTRT                                               01720000
         MVC   LINE+1(132),0(2)                                         01730000
         BAL   14,PRINT                                                 01740000
         B     HEXEXIT                                                  01750000
HEXTYPE  EQU   *                                                        01760000
         BAL   14,HEXXIT                                                01770000
HEXEXIT  EQU   *                                                        01780000
         L     R13,SAVEAREA+4                                           01790000
         RETURN                (14,12),,RC=0                            01800000
HEXPAGE  EQU   *                                                        01810000
         ZAP   LINECTR,=P'999'                                          01820000
         B     HEXEXIT                                                  01830000
********************************************************************    01840000
         EJECT                                                          01850000
**************************************************************          01860000
HEXXIT   EQU   *                                                        01870000
         ST    14,HEXR14                                                01880000
         SR    3,3                                                      01890000
         LA    R6,3                MAX SIZE INDEX INTO LENGTHS TABLE. L 01900005
         LA    R10,15              MASK FOR SELECTING BLANK POSITION. L 01910005
HEXNEXT  EQU   *                                                        01920000
         ST    R4,DUMPADDR         WHERE DUMP IS AT NOW                 01930000
HEXRELO  B     HEXACT                                                   01940000
         ST    3,DUMPADDR                                               01950000
HEXACT   EQU   *                                                        01960000
         UNPK  LINE+1(7),DUMPADDR+1(4)   DUMP ADDRESS                   01970000
         TR    LINE+1(6),CHAR-240      INTO PRINTABLE                   01980000
         MVI   LINE+7,C' '                                              01990000
         LA    9,LINE+88                                                02000000
         LA    8,LINE+10                                                02010000
HEXLOOP  EQU   *                                                        02020000
         LR    R7,R5              BOTTOM OF DUMP AREA LESS TOP        L 02030005
         SR    R7,R4                GIVES SIZE LEFT TO DUMP.          L 02040005
         CR    R7,R6              LESS THAN OR EQUAL TO THREE?        L 02050005
         BNH   HEXNREND           YES, DUMP ONLY THAT MANY BYTES + 1  L 02060005
         LR    R7,R6                ELSE DUMP NEXT 4 (3 + 1) BYTES.   L 02070005
HEXNREND EQU   *                  R7 * 4 INDEXES INTO PROPER LENGTHS  L 02080005
         SLA   R7,2                 FOR THE # OF BYTES WE'LL DUMP.    L 02090005
         L     R7,LENGTHS(R7)     LOAD THOSE LENGTHS.                 L 02100005
         EX    R7,GETTER1         MOVE THE BYTES INLINE.              L 02110005
         SRL   R7,8                 (NEXT LENGTH).                    L 02120005
         EX    R7,UNPKER          UNPACK THEM.                        L 02130005
         SRL   R7,8                 (NEXT LENGTH).                    L 02140005
         LR    R11,R7             UNPACK HAS CREATED A GARBAGE BYTE   L 02150005
         NR    R11,R10              JUST AFTER FIELD.  EXTRACT ITS    L 02160005
         AR    R11,R8               POSITION AND REPLACE IT WITH A    L 02170005
         MVI   1(R11),C' '          BLANK.                            L 02180005
         EX    R7,TRNSLER1        CONVERT THE BYTES TO HEXADECIMAL.   L 02190005
         SRL   R7,8                 (NEXT LENGTH).                    L 02200005
         EX    R7,GETTER2         NOW COPY THE BYTES TO THE RIGHT     L 02210005
         EX    R7,TRNSLER2          AND CONVERT THEM TO CHARACTER.    L 02220005
         LA    R4,4(4)            ASSUME 4 BYTES JUST PROCESSED;      L 02230005
         CR    4,5                  WHETHER 4 OR LESS, WE'LL END UP   L 02240005
         BNH   HEXINCR              HERE IF WE REACH THE BOTTOM OF    L 02250005
         BAL   14,PRINTHEX          THE DUMP AREA.                    L 02260005
         L     14,HEXR14          SO PRINT THE LAST LINE              L 02270005
         BR    14                   AND TRUNDLE ON BACK.              L 02280005
*                                                                     * 02290000
HEXINCR  EQU   *         MORE TO PRINT (SO WE MUST HAVE PROCESSED 4). L 02300005
         LA    3,4(3)             SO BOOST THE RELO POINTER, THE HEX    02310003
         LA    8,9(8)               POINTER AND THE CHARACTER POINTER L 02320005
         LA    9,4(9)               BY '4 BYTES PROCESSED'.             02330003
*                                                                     * 02340000
         C     8,=A(LINE+46)      MIDDLE OF THE LINE?                   02350003
         BNE   HEXEND                                                   02360000
         LA    8,3(8)             YEP -- SET UP BLANK COLUMNS.          02370003
         B     HEXLOOP                                                L 02380005
HEXEND   C     8,=A(LINE+84)      HOW ABOUT THE END OF THE LINE?        02390003
         BL    HEXLOOP            NAH -- JUST KEEP ON DUMPING.        L 02400005
*                                                                     * 02410000
         BAL   14,PRINTHEX        BUT IF SO, PRINT THE LINE             02420001
         B     HEXNEXT              AND SET UP A NEW ONE.             L 02430005
*********************************************************************** 02440000
         EJECT                                                          02450000
*********************************************************************** 02460000
PRINTR14 DC    A(0)                                                     02470000
*                                                                     * 02480000
PRINTHEX EQU   *                                                        02490000
         MVI   LINE+87,SIDES                                            02500000
         MVI   LINE+120,SIDES                                           02510000
PRINT    EQU   *                                                        02520000
         ST    14,PRINTR14                                              02530000
         CLI   LINE,C'0'                                                02540000
         BNE   PRINT2                                                   02550000
         AP    LINECTR,=P'1'                                            02560000
PRINT2   EQU   *                                                        02570000
         CP    LINECTR,=P'60'                                           02580000
         BL    PRINTIT                                                  02590000
         MVI   LINE,C'0'                                                02600000
         ZAP   LINECTR,=P'2'                                            02610000
         AP    PAGE,=P'1'                                               02620000
         MVC   PRNTPAGE,=XL6'402020202120'                              02630000
         ED    PRNTPAGE,PAGE                                            02640000
         PUT   LISTDCB,PRNTHDNG                                         02650000
         L     2,PARMHDNG                                               02660000
         LTR   2,2                                                      02670000
         BZ    PRINTIT                                                  02680000
         MVC   HEADING+1(132),0(2)                                      02690000
         PUT   LISTDCB,HEADING                                          02700000
         AP    LINECTR,=P'1'                                            02710000
         MVI   LINE,C'0'                                                02720000
PRINTIT  EQU   *                                                        02730000
         PUT   LISTDCB,LINE                                             02740000
         AP    LINECTR,=P'1'                                            02750000
         MVI   LINE,C' '                                                02760000
         MVC   LINE+1(L'LINE-1),LINE                                    02770000
         L     14,PRINTR14                                              02780000
         BR    14                                                       02790000
*********************************************************************** 02800000
         EJECT                                                          02810000
*********************************************************************** 02820000
LINE     DC    CL133' '                                                 02830000
*                                                                       02840000
HEADING  DC    CL133' '                                                 02850000
*                                                                     * 02860000
ENDMSG DC C'     ****************  END OF HEXDUMP  *****************'   02870000
*                                                                       02880000
PRNTHDNG DC    CL133'1'                                                 02890000
         ORG   PRNTHDNG+94                                              02900000
         DC    C'DATE '                                                 02910000
PRNTYY   DC    CL2' '                                                   02920000
         DC    C'.'                                                     02930000
PRNTDDD  DC    CL3' '                                                   02940000
         DC    C' TIME '                                                02950000
PRNTHH   DC    CL2' '                                                   02960000
         DC    C'.'                                                     02970000
PRNTMM   DC    CL2' '                                                   02980000
         DC    C'.'                                                     02990000
PRNTSS   DC    CL2' '                                                   03000000
         DC    CL3' '                                                   03010000
         DC    C' PAGE'                                                 03020000
PRNTPAGE DC    CL6' '                                                   03030000
         ORG                                                            03040000
*                                                                     * 03050000
*                                                                       03060000
CHAR     DC   C'0123456789ABCDEF'                                       03070000
GRAPHIC  DC    256C'.'                                                  03080000
         ORG   GRAPHIC+C' '                                             03090000
         DC    C' '                                                     03100000
         ORG   GRAPHIC+C'<'                                             03110000
         DC    C'<(+'                                                   03120000
         ORG   GRAPHIC+C'$'                                             03130000
         DC    C'$*);'                                                  03140000
         ORG   GRAPHIC+C'-'                                             03150000
         DC    C'-/'                                                    03160000
         ORG   GRAPHIC+C','                                             03170000
         DC    C','                                                     03180000
         ORG   GRAPHIC+C'>'                                             03190000
         DC    C'>'                                                     03200000
         ORG   GRAPHIC+C''''                                            03210000
         DC    C'''="'                                                  03220000
         ORG   GRAPHIC+C'A'                                             03230000
         DC    C'ABCDEFGHI'                                             03240000
         ORG   GRAPHIC+C'J'                                             03250000
         DC    C'JKLMNOPQR'                                             03260000
         ORG   GRAPHIC+C'S'                                             03270000
         DC    C'STUVWXYZ'                                              03280000
         ORG   GRAPHIC+C'0'                                             03290000
         DC    C'0123456789'                                            03300000
         ORG                                                            03310000
DUMPADDR DC    F'0'                                                     03320000
         DS    0F                                                       03330000
PARMS    DS    0CL20                                                    03340000
PARMTYP  DC    A(0)                                                     03350000
PARMSTRT DC    A(0)                                                     03360000
PARMEND  DC    A(0)                                                     03370000
PARMHDNG DC    A(0)                                                     03380000
PARMDTL  DC    A(0)                                                     03390000
*************************************************************           03400000
         PRINT NOGEN                                                    03410000
SIDES    EQU   C'*'                                                     03420000
HEXR14   DC    A(0)                                                     03430000
FULL     DC    A(0)                                                     03440000
PAGE     DC    PL3'0'                                                   03450000
*                                                                     * 03460000
YYDDD    DS    0CL5                                                     03470000
YEAR     DC    CL2' '                                                   03480000
DAY      DC    CL3' '                                                   03490000
*                                                                     * 03500000
HHMMSS   DS    0CL7                                                     03510000
HOURS    DC    CL2' '                                                   03520000
MINUTES  DC    CL2' '                                                   03530000
SECONDS  DC    CL2' '                                                   03540000
         DC    C' '                                                     03550000
LINECTR  DC    PL3'90'                                                  03560000
*                                                                     * 03570000
         DS    0F                                                     L 03580005
LENGTHS  DC    X'00012100'        LENGTH TABLE FOR SNARFING BYTES TO  L 03590005
         DC    X'01034201'          ROCESS AND DUMP. LENGTHS CONCAT-  L 03600005
         DC    X'02056302'          END TO END AND SHIFTED FOR USE IN L 03610005
         DC    X'03078403'          EX'ED INSTRUCTIONS.               L 03620005
         DS    0H                                                     L 03630005
GETTER1  MVC   0(0,8),0(4)        MOVES BYTES INTO DUMP LINE (HEX).   L 03640005
UNPKER   UNPK  0(0,8),0(0,8)      UNPACKS THOSE BYTES INPLACE.        L 03650005
TRNSLER1 TR    0(0,8),CHAR-C'0'   TRANSLATES THEM TO HEX.             L 03660005
GETTER2  MVC   0(0,9),0(4)        MOVES BYTES INTO DUMP LINE (CHAR).  L 03670005
TRNSLER2 TR    0(0,9),GRAPHIC     TRANSLATES THEM TO CHARACTERS.      L 03680005
         DS    0D                                                       03690000
HEXDCB   EQU   *                                                        03700000
LISTDCB  DCB   DDNAME=HEXDUMP,DSORG=PS,MACRF=PM,                       X03710000
               RECFM=FBA,LRECL=133,EXLST=EXLSTPRT                       03720000
DDPARM   DC    A(LISTDCB+40)                                            03730000
         DC    A(WORKTIOT)                                              03740000
         DC    A(NODD)                                                  03750000
WORKTIOT DC    A(0)                                                     03760000
*                                                                       03770000
         LTORG                                                          03780000
*                                                                       03790000
EXLSTPRT DC    0F'0',X'85',AL3(EXITPRT)                                 03800000
EXITPRT  EQU   *                                                        03810000
         USING *,15                                                     03820000
         USING IHADCB,1                                                 03830000
         OC    DCBBLKSI,DCBBLKSI                                        03840000
         BNZR  14                                                       03850000
         MVC   DCBBLKSI,DCBLRECL                                        03860000
         BR    14                                                       03870000
         DROP  1,15                                                     03880000
*                                                                       03890000
         PRINT NOGEN                                                    03900000
IHADCB   DCBD  DSORG=PS,DEVD=DA                                         03910000
         END                                                            03920000
*                                                                       03930000
         LTORG                                                          03940000
*                                                                       03950000
EXLSTPRT DC    0F'0',X'85',AL3(EXITPRT)                                 03960000
EXITPRT  EQU   *                                                        03970000
         USING *,15                                                     03980000
         OC    DCBBLKSI,DCBBLKSI                                        03990000
         BNZR  14                                                       04000000
         MVC   DCBBLKSI,DCBLRECL                                        04010000
         BR    14                                                       04020000
         DROP  1,15                                                     04030000
*                                                                       04040000
         PRINT NOGEN                                                    04050000
IHADCB   DCBD  DSORG=PS,DEVD=DA                                         04060000
         END                                                        
//LKED.SYSLMOD DD DSN=&SUBLOAD,DISP=(MOD,PASS)                          00120000
//SYSIN    DD  *                                                        00130000
 NAME VDBS$HEX(R)                                                       00140000
//DBACONSL EXEC ASMFCL                                                  00150000
//ASM.SYSIN DD *                                                        00160000
         MACRO                                                          00010000
         REGS                                                           00020000
R0       EQU   0                                                        00030000
R1       EQU   1                                                        00040000
R2       EQU   2                                                        00050000
R3       EQU   3                                                        00060000
R4       EQU   4                                                        00070000
R5       EQU   5                                                        00080000
R6       EQU   6                                                        00090000
R7       EQU   7                                                        00100000
R8       EQU   8                                                        00110000
R9       EQU   9                                                        00120000
R10      EQU   10                                                       00130000
R11      EQU   11                                                       00140000
R12      EQU   12                                                       00150000
R13      EQU   13                                                       00160000
R14      EQU   14                                                       00170000
R15      EQU   15                                                       00180000
         MEND                                                           00190000
DBACONSL CSECT                                                          00010000
         ENTRY DBAWTO                                                   00020000
         ENTRY DBAWTOR                                                  00030000
         REGS                                                           00040000
*                                                                       00050000
*        WTO/WTOR PROCESSOR                                             00060000
*                                                                       00070000
         TITLE 'DBAWTO - ENTRY POINT LOGIC'                             00080000
DBAWTO   EQU   *                                                        00090000
         LA    R15,MAINEP-DBAWTO(R15) A(MAINLINE)                       00100011
         USING MAINEP,R15     ADDRESSABILITY                            00110000
         MVI   SWEPA+3,X'00'  INDICATE WTO                              00120000
         BR    R15            CONTINUE                                  00130000
         DROP  R15                                                      00140010
*                                                                       00150000
*                                                                       00160000
*                                                                       00170000
DBAWTOR  EQU   *                                                        00180000
         LA    R15,MAINEP-DBAWTOR(R15) A(MAINLINE)                      00190011
         USING MAINEP,R15     ADDRESSABILITY                            00200000
         MVI   SWEPA+3,X'04'  INDICATE WTOR                             00210000
         BR    R15                                                      00220000
         TITLE 'DBACONSL - MAINLINE'                                    00230000
MAINEP   EQU   *                                                        00240000
         SAVE  (14,12)        SAVE REGS                                 00250000
         ST    R13,SAVEAREA+4 BACK CHAIN                                00260000
         LR    R12,R13        SAVE BACK POINTER                         00270000
         CNOP  0,4            ALIGN                                     00280000
         BAL   R13,SKIPSAVE   A(NEW SAVE AREA)                          00290000
SAVEAREA DC    18F'0'                                                   00300000
SKIPSAVE EQU   *                                                        00310000
         ST    R13,8(R12)     FOREWARD CHAIN                            00320000
         DROP  R15                                                      00330000
         USING SAVEAREA,R13   ADDRESSABILITY                            00340000
         LM    R6,R7,0(R1)    POST PARMS                                00350000
ONETIME  NOP   JNAMEOK                                                  00360000
         OI    ONETIME+1,X'F0' SKIP INIT 2ND TIME THRU                  00370000
         EXTRACT TIOT,FIELDS=TIOT                                       00380000
         L     R2,TIOT        A(TIOT)                                   00390000
         MVC   JOBNAMES,0(R2) POST REAL JOB NAME                        00400000
JNAMEOK  EQU   *                                                        00410000
         TIME  DEC                                                      00420000
         ST    R0,HHMMSSTH    POST HHMMSSTH                             00430000
         OI    HHMMSSTH+2,X'0F' PUT GOOD SIGN IN MIDDLE                 00440000
         MVC   TIME,MASK      POST EDIT MASK                            00450000
         ED    TIME,HHMMSSTH  EDIT                                      00460000
         SR    R5,R5          CLEAR                                     00470000
         IC    R5,1(R6)       PICK UP LENGTH                            00480000
         BCTR  R5,0           LESS 1 FOR MVC                            00490000
         STC   R5,COPY1+1     POST                                      00500001
COPY1    MVC   USERMSG,2(R6)  COPY TEXT                                 00510011
         LA    R4,20(R5)      FULL LENGTH                               00520000
         STH   R4,MSGL        POST                                      00530003
         LA    R4,MSGL(R4)    NOW ADDR PAST END                         00540003
         MVC   0(4,R4),ROUTDESC POST ROUTING AND DESCRIPTOR             00550003
         MVC   0(1,R4),0(R6)  FIX TRAILER                               00560000
         L     R15,SWEPA      WTO/WTOR                                  00570000
         B     *+4(R15)                                                 00580000
         B     DOWTO          00 - WTO                                  00590000
         B     DOWTOR         04 - WTOR                                 00600000
DOWTO    EQU   *                                                        00610000
         WTO   MF=(E,WTOBUF)                                            00620003
         B     RETURN                                                   00630000
DOWTOR   EQU   *                                                        00640000
         NI    ECB,X'00'      CLEAR                                     00650000
         LA    R1,WTORBUF                                               00660006
         MVC   ANS(80),BLANK  CLEAR                                     00670007
         WTOR  MF=(E,(1))                                               00680000
         WAIT  1,ECB=ECB                                                00690003
         LH    R5,0(R7)        L'ANS                                    00700001
         BCTR  R5,0            LESS 1 FOR MVC                           00710001
         STC   R5,COPY2+1      POST                                     00720001
COPY2    MVC   2(0,R7),ANS     COPY                                     00730006
         B     RETURN                                                   00740001
         TITLE 'DBACONSL - RETURN'                                      00750001
RETURN   EQU   *                                                        00760001
         L     R13,4(,R13)                                              00770011
         RETURN (14,12)                                                 00780005
         TITLE  'DBACONSL - DATA AREAS'                                 00790001
TIOT     DC     A(0)                                                    00800001
HHMMSSTH DC     XL4'00'                                                 00810005
SWEPA    DC     A(0)                                                    00820001
BLANK    DC     C' '                                                    00830007
ANS      DC     CL80' '                                                 00840007
ECB      DC     A(0)                                                    00850001
WTORBUF  DS     0F                                                      00860001
REPLYL   DC     AL1(L'ANS)                                              00870007
         DC     AL3(ANS)                                                00880001
         DC     A(ECB)                                                  00890001
WTOBUF   EQU    *                                                       00900001
MSGL     DC     AL2(116)                                                00910010
         DC     X'8000'       MCS FLAGS                                 00920007
MSG      DC     112C' '                                                 00930007
         ORG    MSG                                                     00940007
JOBNAMES DC     C'JOBNAMES'                                             00950002
TIME     DC     C' HH.MM'                                               00960002
         DC     C' '                                                    00970010
USERMSG  DC     CL4' '                                                  00980010
         ORG    ,                                                       00990007
         DC     X'40004000'                                             01000002
         DC     XL4'00'                                                 01010001
ROUTDESC DC     XL4'00004000'                                           01020004
MASK     DC     X'4021204B2020'                                         01030002
         END                                                            01040001
//LKED.SYSLMOD DD DSN=&SUBLOAD,DISP=(MOD,PASS)                          00190000
//SYSIN    DD  *                                                        00200000
 NAME DBACONSL(R)                                                       00210000
//LINKS EXEC LKED                                                       00220000
//LKED.SYSLMOD DD DSN=&SUBLOAD,DISP=(MOD,PASS)                          00230000
//SYSIN    DD  *                                                        00240000
 INCLUDE SYSLMOD(VDBS$DDF)                                              00250000
 ALIAS DDFIND                                                           00260000
 ENTRY VDBS$DDF                                                         00270000
 NAME VDBS$DDF(R)                                                       00280000
 INCLUDE SYSLMOD(VDBS$HEX)                                              00290000
 ALIAS HEXDUMP                                                          00300000
 ALIAS HEXDUMP2                                                         00310000
 ENTRY HEXDUMP                                                          00320000
 NAME VDBS$HEX(R)                                                       00330000
 INCLUDE SYSLMOD(DBACONSL)                                              00340000
 ALIAS DBAWTO                                                           00350000
 ALIAS DBAWTOR                                                          00360000
 ENTRY DBACONSL                                                         00370000
 NAME DBACONSL(R)                                                       00380000
//SUPERGEN EXEC ASMFCL,MAC1='SYS1.AMODGEN',PARM.LKED='XREF,LIST'        00390000
//ASM.SYSIN DD *                                                        00400000
         MACRO                                                          00010000
         REGS                                                           00020000
R0       EQU   0                                                        00030000
R1       EQU   1                                                        00040000
R2       EQU   2                                                        00050000
R3       EQU   3                                                        00060000
R4       EQU   4                                                        00070000
R5       EQU   5                                                        00080000
R6       EQU   6                                                        00090000
R7       EQU   7                                                        00100000
R8       EQU   8                                                        00110000
R9       EQU   9                                                        00120000
R10      EQU   10                                                       00130000
R11      EQU   11                                                       00140000
R12      EQU   12                                                       00150000
R13      EQU   13                                                       00160000
R14      EQU   14                                                       00170000
R15      EQU   15                                                       00180000
         MEND                                                           00190000
         MACRO                                                          00000001
         DBAIOB  &CCW=,&DCB=,&PREFIX=IOB,&IOBFLG1=42                    00000002
****************************************************                    00000003
*         IOB FOR EXCP USE                         *                    00000004
****************************************************                    00000005
&PREFIX.ECB   DC   A(0)                                                 00000006
&PREFIX.IOB   DS   0D                                                   00000007
&PREFIX.FLAG1 DC   X'&IOBFLG1'                                          00000008
&PREFIX.FLAG2 DC   X'00'                                                00000009
&PREFIX.SENS0 DC   X'00'                                                00000010
&PREFIX.SENS1 DC   X'00'                                                00000011
&PREFIX.ECBCC DC   X'00'                                                00000012
&PREFIX.ECBPB DC   AL3(&PREFIX.ECB)                                     00000013
&PREFIX.FLAG3 DC   X'00'                                                00000014
&PREFIX.CSW   DC    XL7'00'                                             00000015
&PREFIX.STAT  EQU   &PREFIX.CSW+3                                       00000016
&PREFIX.BYTES EQU   &PREFIX.CSW+5                                       00000017
&PREFIX.STRTB DC   A(&CCW)                                              00000018
&PREFIX.DCBPB DC   A(&DCB)                                              00000019
              DC    2A(0)                                               00000020
              DS    0XL8                    MBBCCHHR                    00000021
&PREFIX.MBB   DC    XL3'00'                 MBB                         00000022
&PREFIX.CCHHR DC    XL5'00'                 CCHHR                       00000023
**************************************************                      00000024
         MEND                                                           00000025
SUPERGEN CSECT                                                          00010000
         PRINT NOGEN                                                    00020000
         USING SUPERGEN,R15                                             00030000
         USING SEARCH,R5                                                00040000
         STM   R14,R12,12(R13)             SAVE REGS                    00050000
         ST    R13,SAVEAREA+4                                           00060000
         LR    R12,R13                                                  00070000
         CNOP  0,4                                                      00080000
         BAL   R13,SPGENENT                PERFORM                      00090000
SAVEAREA DC    18A(0)                                                   00100000
BASES    DC    A(SAVEAREA+2*4096,SAVEAREA+4096)                         00110000
         DROP  R15                                                      00120000
         USING SAVEAREA,R13,R12,R11                                     00130000
         USING IHADCB,R4                                                00140000
         USING INBUF,R10                                                00150000
SPGENENT EQU   *                                                        00160000
         ST    R13,8(R12)                                               00170000
*                                                                     * 00180000
         LM    R11,R12,BASES               RESTORE REGS                 00190000
         ST    R1,PARMADR                  SAVE PARM ADDR               00200000
         L     R1,PARMADR                                               00210000
         L     R1,0(R1)                    PARM ADDR                    00220000
         LH    R2,0(R1)                    LENGTH                       00230000
         L     R4,=V(HEXDCB)                                            00240000
         MVC   DCBDDNAM,=CL8'SYSPRINT'                                  00250000
         LTR   R2,R2                                                    00260000
         BZ    CONTROL                                                  00270000
         BCTR  R2,0                                                     00280000
         STC   R2,PARMMVC+1                                             00290000
         LA    R2,2(R2)                                                 00300000
         STH   R2,PRMCOUNT                                              00310000
PARMMVC  MVC   CTLCARD,2(1)                                             00320000
         MVC   PRMHIT(4),=A(CTLCARD)                                    00330000
         B     CONTROL2                                                 00340000
*                                                                     * 00350000
*********************************************************************** 00360000
*                                                                     * 00370000
CONTROL  EQU   *                                                        00380000
         BAL   R14,GETSYSIN                PERFORM                      00390000
         B     CTLEOF                      EOF                          00400000
CONTROL2 EQU   *                                                        00410000
         MVC   PRINTREC(80),CTLCARD                                     00420000
         BAL   R14,LISTDATA                PERFORM                      00430000
*                                                                     * 00440000
OPTION   EQU   *                                                        00450000
         BAL   R14,SETTRT                  PERFORM                      00460000
         MVC   PRMSTART(4),PRMHIT                                       00470000
         BAL   R14,PRMSCAN                 PERFORM                      00480000
         B     CONTROL                     DONE                         00490000
         B     OPTION                      ZERO                         00500000
         MVC   CHKADR(4),PRMSTART                                       00510000
         MVC   CHKLNGTH(2),PRMLNGTH                                     00520000
         MVC   CHKTBL(4),=A(KEYWORD1)                                   00530000
*                                                                     * 00540000
         BAL   R14,CHKTYPE                 PERFORM                      00550000
         B     INVALID                                                  00560000
         B     EXIT3                       END                          00570000
         B     CONTROL                     * COMMENT *                  00580000
         B     OPTION2                     DUMP                         00590000
         B     SOR                         OR                           00600000
         B     SAND                        AND                          00610000
         B     OPTION2                     KEEP                         00620000
         B     OPTION2                     PUNCH                        00630000
         B     OPTION2                     MOVE                         00640000
         B     OPTION2                     SKIP                         00650000
         B     OPTION2                     COUNT                        00660000
         B     OPTION2                     VTOC                         00670000
         B     OPTION2                     EXCP                         00680000
         B     OPTION2                     SEARCH                       00690000
         B     SCLEAR                      CLEAR                        00700000
         B     OPTION2                     INDD                         00710000
         B     OPTION2                     OUTDD                        00720000
         B     SGO                         GO                           00730000
         B     OPTION2                     BLKSIZE                      00740000
         B     OPTION2                     CYL                          00750000
         B     OPTION2                     HEAD                         00760000
         B     OPTION2                     REC                          00770000
         B     OPTION2                     CCHHR                        00780000
         B     OPTION2                     TTR                          00790000
         B     OPTION2                     DSN                          00800000
         B     OPTION2                     EOF                          00810000
         B     OPTION2                     DEB                          00820000
         B     SINTRACT                    INTERACT                     00830000
         B     OPTION2                     EROPT                        00840000
OPTION2  EQU   *                                                        00850000
         MVC   PRMSTART(4),PRMHIT                                       00860000
         BAL   R14,PRMSCAN                 PERFORM                      00870000
         B     INVALID                                                  00880000
         B     OPTION2                                                  00890000
         MVC   VALUE(80),VALUECLR                                       00900000
         LH    R4,PRMLNGTH                                              00910000
         BCTR  R4,0                                                     00920000
         STC   R4,VALMVC+1                                              00930000
         L     R3,PRMSTART                                              00940000
VALMVC   MVC   VALUE,0(3)                                               00950000
*                                                                     * 00960000
         BAL   R14,CHKTYPE                 PERFORM                      00970000
         B     INVALID                                                  00980000
         B     EXIT3                       END                          00990000
         B     CONTROL                     * COMMENT *                  01000000
         B     SLIST                       DUMP                         01010000
         B     SOR                         OR                           01020000
         B     SAND                        AND                          01030000
         B     SKEEP                       KEEP                         01040000
         B     SPUNCH                      PUNCH                        01050000
         B     SMOVE                       MOVE                         01060000
         B     SSKIP                       SKIP                         01070000
         B     SCOUNT                      COUNT                        01080000
         B     SVTOC                       VTOC                         01090000
         B     SEXCP                       EXCP                         01100000
         B     SSEARCH                     SEARCH                       01110000
         B     SCLEAR                      CLEAR                        01120000
         B     SINDD                       INDD                         01130000
         B     SOUTDD                      OUTDD                        01140000
         B     SGO                         GO                           01150000
         B     SBLKSIZE                    BLKSIZE                      01160000
         B     SCYCL                       CYL                          01170000
         B     SHEAD                       HEAD                         01180000
         B     SREC                        REC                          01190000
         B     SCCHHR                      CCHHR                        01200000
         B     STTR                        TTR                          01210000
         B     SDSN                        DSN                          01220000
         B     SEOF                        EOF                          01230000
         B     SDEB                        DEB                          01240000
         B     SINTRACT                    INTERACT                     01250000
         B     SEROPT                      EROPT                        01260000
*********************************************************************** 01270000
         EJECT                                                          01280000
*********************************************************************** 01290000
SINTRACT LA    R4,SYSIN                                                 01300000
         TM    48(R4),X'10'                                             01310000
         BO    OPTION                                                   01320000
         MVI   NTRACTON,X'FF'                                           01330000
         B     OPTION                                                   01340000
*********************************************************************** 01350000
SEROPT   MVI   EROPT,X'04'                                              01360000
         CLC   VALUE(4),=C'SKIP'                                        01370000
         BE    OPTION                                                   01380000
         MVI   EROPT,X'08'                                              01390000
         CLC   VALUE(6),=C'ACCEPT'                                      01400000
         BE    OPTION                                                   01410000
         MVI   EROPT,X'00'                                              01420000
         CLC   VALUE(4),=C'STOP'                                        01430000
         BE    OPTION                                                   01440000
         B     INVALID                                                  01450000
*********************************************************************** 01460000
SLIST    EQU   *                                                        01470000
         MVI   DUMPALL,0                                                01480000
         MVI   LISTON,0                                                 01490000
         CLI   VALUE,C'N'                                               01500000
         BE    OPTION                                                   01510000
         MVI   LISTON,X'FF'                                             01520000
         MVC   HEXCHAR,=C'    '                                         01530000
         CLC   VALUE(2),=C'HC'                                          01540000
         BNE   SLIST1                                                   01550000
         MVC   HEXCHAR,=C'HEXC'                                         01560000
SLIST1   CLC   VALUE(3),=CL3'ALL'                                       01570000
         BNE   OPTION                                                   01580000
         OI    DUMPALL,X'FF'                                            01590000
         B     OPTION                                                   01600000
*********************************************************************** 01610000
SOR      EQU   *                                                        01620000
         MVI   ANDOR,0                                                  01630000
         B     OPTION                                                   01640000
*********************************************************************** 01650000
SAND     EQU   *                                                        01660000
         MVI   ANDOR,X'FF'                                              01670000
         B     OPTION                                                   01680000
*********************************************************************** 01690000
SKEEP    EQU   *                                                        01700000
         MVI   KEEPON,0                                                 01710000
         CLI   VALUE,C'N'                                               01720000
         BE    OPTION                                                   01730000
         MVI   KEEPON,X'FF'                                             01740000
         B     OPTION                                                   01750000
*********************************************************************** 01760000
SPUNCH   MVI   DUMPON,X'FF'                                             01770000
         MVI   SRCHSET+1,X'0C'                                          01780000
         B     DOSEARCH                                                 01790000
*********************************************************************** 01800000
SMOVE    MVI   SRCHSET+1,X'10'                                          01810000
         MVI   MOVEON,X'FF'                                             01820000
         B     DOSEARCH                                                 01830000
*********************************************************************** 01840000
SSKIP    EQU   *                                                        01850000
         BAL   R14,PACK                    PERFORM                      01860000
         B     NOTNUM                                                   01870000
         ZAP   SKIPNUM,DOUBLE(8)                                        01880000
         B     OPTNSET                                                  01890000
*********************************************************************** 01900000
SCOUNT   EQU   *                                                        01910000
         BAL   R14,PACK                    PERFORM                      01920000
         B     NOTNUM                                                   01930000
         ZAP   COUNTNUM,DOUBLE(8)                                       01940000
         B     OPTNSET                                                  01950000
*********************************************************************** 01960000
SVTOC    EQU   *                                                        01970000
         BAL   R14,CLOSEUT1                PERFORM                      01980000
         MVI   VTOC,0                                                   01990000
         CLI   VALUE,C'N'                                               02000000
         BE    SEXCP3                                                   02010000
         MVI   VTOC,X'FF'                                               02020000
         LA    R4,INDCB                                                 02030000
         MVC   DCBBLKSI,=AL2(96)                                        02040000
         MVI   DCBKEYLE,44                                              02050000
*                                                                       02060000
         B     SEXCP3                                                   02070000
*********************************************************************** 02080000
SEXCP    EQU   *                                                        02090000
         BAL   R14,CLOSEUT1                PERFORM                      02100000
         MVI   VTOC,0                                                   02110000
SEXCP3   EQU   *                                                        02120000
         XC    NEXTCCHH(5),NEXTCCHH                                     02130000
         XC    EXCPTTR(4),EXCPTTR          RESET TO SCRATCH             02140000
         LA    R4,INDCB                                                 02150000
         IC    R2,DCBKEYLE                 THE KEY                      02160000
         LH    R3,DCBBLKSI                 SAVE ANY PREVIOUS BLKSIZE    02170000
         MVC   INDCB(LMODEL),MODELDCB                                   02180000
         MVI   EXCP,0                                                   02190000
         CLI   VALUE,C'N'                                               02200000
         BE    SEXCP6                                                   02210000
         MVC   INDCB(LEXCPDCB),EXCPDCB                                  02220000
         MVI   EXCP,X'FF'                                               02230000
         MVI   DCBRECFM,X'C0'              PRETENT U                    02240000
SEXCP6   EQU   *                                                        02250000
         STC   R2,DCBKEYLE                                              02260000
         STH   R3,DCBBLKSI                 AND SAVE AGAIN               02270000
         STH   R3,DCBLRECL                                              02280000
         B     OPTION                                                   02290000
*********************************************************************** 02300000
SSEARCH  MVI   SRCHSET+1,X'00'                                          02310000
DOSEARCH BAL   R14,SRCHGETM                GET STORAGE                  02320000
         L     R5,SRCHADR                                               02330000
         XC    SRCHOFF,SRCHOFF                                          02340000
         MVC   SRCHRNGE,=XL2'7FFF'                                      02350000
         MVI   SRCHFUNC,EQ                                              02360000
SRCHSET  MVI   SRCHOPTN,0                                               02370000
         MVI   WORKFUNC,EQ                                              02380000
         BAL   R14,PACK                    PERFORM                      02390000
         B     SRCHOP1B                                                 02400000
         BCTR  R1,0                                                     02410000
         L     R5,SRCHADR                                               02420000
         STH   R1,SRCHOFF                                               02430000
         MVC   SRCHRNGE,=AL2(1)            DEFAULT 1 TIME               02440000
*                                                                     * 02450000
SRCHOP1  EQU   *                                                        02460000
         MVC   PRMSTART(4),PRMHIT                                       02470000
         BAL   R14,PRMSCAN                 PERFORM                      02480000
         B     INVALID                                                  02490000
         B     SRCHOP1                                                  02500000
         L     R3,PRMSTART                                              02510000
         MVC   VALUE(80),0(R3)                                          02520000
         BAL   R14,PACK                    PERFORM                      02530000
         B     SRCHOP1B                                                 02540000
         L     R5,SRCHADR                                               02550000
         SH    R1,SRCHOFF                                               02560000
         BNP   NOTSIZ                                                   02570000
         STH   R1,SRCHRNGE                                              02580000
SRCHOP2  EQU   *                                                        02590000
         MVC   PRMSTART(4),PRMHIT                                       02600000
         BAL   R14,PRMSCAN                 PERFORM                      02610000
         B     INVALID                                                  02620000
         B     SRCHOP2                                                  02630000
SRCHOP1B EQU   *                                                        02640000
         MVC   CHKADR(4),PRMSTART                                       02650000
         MVC   CHKLNGTH(2),PRMLNGTH                                     02660000
         MVC   CHKTBL(4),=A(KEYWORD2)                                   02670000
         BAL   R14,CHKTYPE                 PERFORM                      02680000
         B     SNOHIT                                                   02690000
         B     SEQ                         EQ                           02700000
         B     SNE                         NE                           02710000
         B     SLT                         LT                           02720000
         B     SGT                         GT                           02730000
         B     SLE                         LE                           02740000
         B     SGE                         GE                           02750000
         B     SRCHOP4A                    'X'                          02760000
         B     SRCHOP4A                    'C'                          02770000
         B     SSTOP                       STOP                         02780000
         B     SSKIPF                      SKIP                         02790000
*********************************************************************** 02800000
SNOHIT   CLI   SRCHSET+1,X'10'                                          02810000
         BNE   INVALID                                                  02820000
         L     R3,PRMSTART                                              02830000
         MVC   VALUE(80),0(R3)                                          02840000
         BAL   R14,PACK                    PERFORM                      02850000
         B     SRCHOP4A                                                 02860000
         BCTR  R1,0                                                     02870000
         L     R5,SRCHADR                                               02880000
         STH   R1,SRCHDATA                                              02890000
         MVC   SRCHLNG,=X'0002'                                         02900000
         LA    R5,10(R5)                                                02910000
         B     SRCHEXIT                                                 02920000
SEQ      EQU   *                                                        02930000
         MVI   WORKFUNC,EQ                                              02940000
         B     SRCHOP3A                                                 02950000
SNE      EQU   *                                                        02960000
         MVI   WORKFUNC,NE                                              02970000
         B     SRCHOP3A                                                 02980000
SLT      EQU   *                                                        02990000
         MVI   WORKFUNC,LT                                              03000000
         B     SRCHOP3A                                                 03010000
SGT      EQU   *                                                        03020000
         MVI   WORKFUNC,GT                                              03030000
         B     SRCHOP3A                                                 03040000
SLE      EQU   *                                                        03050000
         MVI   WORKFUNC,LE                                              03060000
         B     SRCHOP3A                                                 03070000
SGE      EQU   *                                                        03080000
         MVI   WORKFUNC,GE                                              03090000
         B     SRCHOP3A                                                 03100000
SRCHOP3A EQU   *                                                        03110000
         L     R5,SRCHADR                                               03120000
         MVC   SRCHFUNC,WORKFUNC                                        03130000
         MVC   PRMSTART(4),PRMHIT                                       03140000
         BAL   R14,PRMSCAN                 PERFORM                      03150000
         B     INVALID                                                  03160000
         B     SRCHOP3A                                                 03170000
         MVC   CHKADR(4),PRMSTART                                       03180000
         MVC   CHKLNGTH(2),PRMLNGTH                                     03190000
         MVC   CHKTBL(4),=A(KEYWORD3)                                   03200000
         BAL   R14,CHKTYPE                 PERFORM                      03210000
         B     INVALID                                                  03220000
         B     SRCHOP4A                    HEX                          03230000
         B     SRCHOP4A                    CHAR                         03240000
         B     SSTOP                                                    03250000
         B     SSKIPF                      SKIP                         03260000
SSTOP    EQU   *                                                        03270000
         L     R5,SRCHADR                                               03280000
         MVI   SRCHOPTN,X'04'                                           03290000
         MVI   STOPON,X'FF'                                             03300000
         B     SRCHOP4                                                  03310000
*********************************************************************** 03320000
SSKIPF   EQU   *                                                        03330000
         L     R5,SRCHADR                                               03340000
         MVI   SRCHOPTN,X'08'                                           03350000
         MVI   SKIPON,X'FF'                                             03360000
         B     SRCHOP4                                                  03370000
*********************************************************************** 03380000
SRCHOP4  EQU   *                                                        03390000
         MVC   PRMSTART(4),PRMHIT          LOOK FOR C,X                 03400000
         BAL   R14,PRMSCAN                 PERFORM                      03410000
         B     INVALID                                                  03420000
         B     SRCHOP4                                                  03430000
*                                                                     * 03440000
SRCHOP4A EQU   *                                                        03450000
         L     R5,SRCHADR                                               03460000
         CLI   SRCHOPTN,X'00'                                           03470000
         BNE   SRCHOP4B                                                 03480000
         MVI   SEARCHON,X'FF'                                           03490000
SRCHOP4B XC    TRTTBL(256),TRTTBL                                       03500000
         SR    R2,R2                                                    03510000
         IC    R2,PRMCHAR+3                THE DELIMITER                03520000
         LA    R3,TRTTBL(2)                                             03530000
         STC   R2,0(R3)                    THIS WILL NOW DELIMIT        03540000
*                                                                     * 03550000
         CLI   SRCHSET+1,X'10'                                          03560000
         BNE   SRCHOP4C                                                 03570000
         MVI   SRCHOPTN,X'14'                                           03580000
SRCHOP4C L     R3,PRMSTART                                              03590000
         CLI   0(R3),C'C'                  CHAR                         03600000
         BE    SRCHOP4D                      YES                        03610000
         CLI   0(R3),C'X'                  HEX                          03620000
         BNE   INVALID                        NO                        03630000
         MVC   PRMSTART(4),PRMHIT          GET SEARCH ARG HEX           03640000
         BAL   R14,PRMSCAN                 PERFORM                      03650000
         B     INVALID                                                  03660000
         B     INVALID                                                  03670000
         L     R3,PRMSTART                                              03680000
         LH    R4,PRMLNGTH                                              03690000
         LR    R5,R4                                                    03700000
         BCTR  R5,0                                                     03710000
         STC   R5,SRCHTR+1                                              03720000
SRCHTR   TR    0(0,3),HEXTBL                                            03730000
         SRL   R4,1                                                     03740000
         LTR   R4,R4                                                    03750000
         BZ    INVALID                                                  03760000
         L     R5,SRCHADR                                               03770000
         STH   R4,SRCHLNG                                               03780000
SRCHHEX  PACK  SRCHDATA(2),0(3,R3)                                      03790000
         LA    R3,2(R3)                                                 03800000
         LA    R5,1(R5)                                                 03810000
         BCT   R4,SRCHHEX                                               03820000
         LA    R5,SRCHCTL(R5)                                           03830000
         B     SRCHEXIT                                                 03840000
*                                                                     * 03850000
SRCHOP4D EQU   *                                                        03860000
         MVC   PRMSTART(4),PRMHIT          GET SEARCH ARG CHAR          03870000
         BAL   R14,PRMSCAN                 PERFORM                      03880000
         B     INVALID                                                  03890000
         B     INVALID                                                  03900000
         L     R3,PRMSTART                                              03910000
         LH    R4,PRMLNGTH                                              03920000
         L     R5,SRCHADR                                               03930000
         STH   R4,SRCHLNG                                               03940000
         BCTR  R4,0                                                     03950000
         STC   R4,SRCHMVC+1                                             03960000
SRCHMVC  MVC   SRCHDATA,0(3)                                            03970000
         LA    R5,SRCHCTL+1(R4,R5)                                      03980000
SRCHEXIT EQU   *                                                        03990000
         ST    R5,SRCHADR                                               04000000
         XC    SRCHOFF(SRCHCTL),SRCHOFF                                 04010000
         NI    SELCTNOP+1,X'0F'                                         04020000
         B     OPTNSET                                                  04030000
*********************************************************************** 04040000
SCLEAR   EQU   *                                                        04050000
         BAL   R14,CLOSEUT1                PERFORM                      04060000
         MVC   INDCB(LMODEL),MODELDCB                                   04070000
         MVI   VTOC,0                                                   04080000
         MVI   EXCP,0                                                   04090000
         MVI   DUMPALL,0                                                04100000
         MVI   SKIPCTL,0                                                04110000
         MVI   OPN1MVI2+1,X'20'                                         04120000
         XC    NEXTCCHH(5),NEXTCCHH        ONLY IF RESET WILL IT CLEAR  04130000
         XC    EXCPTTR(4),EXCPTTR                                       04140000
         MVI   KEEPON,0                                                 04150000
SRESET2  EQU   *                                                        04160000
         ZAP   OUTCOUNT,=P'0'                                           04170000
         ZAP   SKIPNUM,=P'0'                                            04180000
         CLI   KEEPON,X'FF'                DO WE KEEP THE FIRST         04190000
         BE    OPTION                                                   04200000
         MVI   SKIPON,0                                                 04210000
         MVI   STOPON,0                                                 04220000
         MVI   DUMPON,0                                                 04230000
         MVI   MOVEON,0                                                 04240000
         MVI   SEARCHON,0                                               04250000
         MVI   ANDOR,0                                                  04260000
         ZAP   COUNTNUM,=P'99999999'                                    04270000
         BAL   R14,SRCHFREM                PERFORM                      04280000
         OI    SELCTNOP+1,X'F0'                                         04290000
         B     OPTION                                                   04300000
*********************************************************************** 04310000
SINDD    EQU   *                                                        04320000
         BAL   R14,CLOSEUT1                PERFORM                      04330000
         MVC   UT1NAME(8),VALUE                                         04340000
         LA    R1,UT1NAME                                               04350000
         BAL   R14,DDFIND                  PERFORM                      04360000
         B     SINDDER                                                  04370000
         B     OPTION                                                   04380000
SINDDER  EQU   *                                                        04390000
         LA    R1,DDERR                                                 04400000
         BAL   R14,WTORTN                  PERFORM                      04410000
         B     OPTION                                                   04420000
*********************************************************************** 04430000
SOUTDD   EQU   *                                                        04440000
         BAL   R14,CLOSEUT2                PERFORM                      04450000
         CLI   VALUE,C'N'                                               04460000
         BNE   SOUTDD2                                                  04470000
         MVI   OUTON,0                                                  04480000
         B     OPTION                                                   04490000
SOUTDD2  EQU   *                                                        04500000
         MVC   UT2NAME(8),VALUE                                         04510000
         LA    R1,UT2NAME                                               04520000
         BAL   R14,DDFIND                  PERFORM                      04530000
         B     SOUTDDER                                                 04540000
         MVI   OUTON,X'FF'                                              04550000
         B     OPTION                                                   04560000
SOUTDDER EQU   *                                                        04570000
         LA    R1,DDERR                                                 04580000
         BAL   R14,WTORTN                  PERFORM                      04590000
         B     OPTION                                                   04600000
*                                                                     * 04610000
*********************************************************************** 04620000
SBLKSIZE EQU   *                                                        04630000
         BAL   R14,CLOSEUT1                PERFORM                      04640000
         BAL   R14,PACK                    PERFORM                      04650000
         B     INVALID                                                  04660000
         LA    R4,INDCB                                                 04670000
         STH   R1,DCBLRECL                                              04680000
         STH   R1,DCBBLKSI                                              04690000
         MVI   DCBRECFM,X'C0'              SET TO 'U'                   04700000
         B     OPTION                                                   04710000
*********************************************************************** 04720000
SCYCL    EQU   *                                                        04730000
         TR    VALUE(80),HEXTBL                                         04740000
         LH    R3,PRMLNGTH                                              04750000
         EX    R3,SEEKPAK                                               04760000
         MVC   NEXTCCHH(2),WORKCCHH+6                                   04770000
         B     OPTION                                                   04780000
*********************************************************************** 04790000
SHEAD    EQU   *                                                        04800000
         TR    VALUE(80),HEXTBL                                         04810000
         LH    R3,PRMLNGTH                                              04820000
         EX    R3,SEEKPAK                                               04830000
         MVC   NEXTCCHH+2(2),WORKCCHH+6                                 04840000
         B     OPTION                                                   04850000
*********************************************************************** 04860000
SREC     EQU   *                                                        04870000
         TR    VALUE(80),HEXTBL                                         04880000
         LH    R3,PRMLNGTH                                              04890000
         EX    R3,SEEKPAK                                               04900000
         MVC   NEXTCCHH+4(1),WORKCCHH+7                                 04910000
         B     OPTION                                                   04920000
*********************************************************************** 04930000
SCCHHR   EQU   *                                                        04940000
         TR    VALUE(80),HEXTBL                                         04950000
         PACK  WORKCCHH(6),VALUE(11)                                    04960000
         MVC   NEXTCCHH(5),WORKCCHH                                     04970000
         LA    R4,INDCB                                                 04980000
         TM    DCBOFLGS,X'10'              OPEN YET                     04990000
         BZ    OPTION                         NO                        05000000
         BAL   R14,EXTENTS                 CHK IT OUT                   05010000
         B     OPTION                      ITS A BADDY                  05020000
         B     SCCHHTTR                                                 05030000
*********************************************************************** 05040000
STTR     EQU   *                                                        05050000
         XC    NEXTCCHH(5),NEXTCCHH        CANT DO CCHHR NOW            05060000
         TR    VALUE(80),HEXTBL                                         05070000
         LH    R3,PRMLNGTH                                              05080000
         EX    R3,SEEKPAK                                               05090000
         MVC   EXCPTTR(3),WORKCCHH+5                                    05100000
         MVI   EXCPTTR+3,0                                              05110000
         LA    R4,INDCB                                                 05120000
         TM    DCBOFLGS,X'10'              OPEN YET                     05130000
         BZ    OPTION                         NO                        05140000
         BAL   R14,CNVTTTR                 PERFORM                      05150000
         B     OPTION                      ITS A BADDY                  05160000
         B     SCCHHTTR                                                 05170000
SCCHHTTR EQU   *                                                        05180000
         SR    R2,R2                                                    05190000
         IC    R2,NEXTCCHH+4               THE RECORD                   05200000
         LTR   R2,R2                       IS IT ZERO                   05210000
         BZ    OPTION                         YES                       05220000
         BCTR  R2,0                        REVISE                       05230000
         STC   R2,NEXTCCHH+4                                            05240000
         B     OPTION                                                   05250000
*********************************************************************** 05260000
SDSN     EQU   *                                                        05270000
         BAL   R14,CLOSEUT1                PERFORM                      05280000
         XC    NEXTCCHH(5),NEXTCCHH                                     05290000
         XC    EXCPTTR(4),EXCPTTR                                       05300000
         MVC   WORKDSN,VALUE               THE DSN                      05310000
         B     OPTION                                                   05320000
*********************************************************************** 05330000
SEOF     EQU   *                                                        05340000
         MVI   OPN1MVI2+1,X'20'                                         05350000
         CLI   VALUE,C'N'                                               05360000
         BE    OPTION                                                   05370000
         MVI   OPN1MVI2+1,X'60'                                         05380000
         B     OPTION                                                   05390000
*********************************************************************** 05400000
SDEB     EQU   *                                                        05410000
         MVI   DEBON,0                                                  05420000
         CLI   VALUE,C'N'                                               05430000
         BE    OPTION                                                   05440000
         MVI   DEBON,X'FF'                                              05450000
         B     OPTION                                                   05460000
OPTNSET  EQU   *                                                        05470000
         B     OPTION                                                   05480000
*********************************************************************** 05490000
SEEKPAK  PACK  WORKCCHH(9),VALUE(0)                                     05500000
*********************************************************************** 05510000
*********************************************************************** 05520000
         EJECT                                                          05530000
*********************************************************************** 05540000
CTLEOF   EQU   *                                                        05550000
         CLI   GO,0                        DID WE GO YET                05560000
         BNE   EXIT3                          YES WE DID                05570000
         MVI   EOF,X'FF'                                                05580000
         B     DOIT                                                     05590000
*********************************************************************** 05600000
SGO      EQU   *                                                        05610000
         CLI   NTRACTON,X'00'                                           05620000
         BE    DOIT                                                     05630000
         BAL   R14,FRSTPRMP                PERFORM                      05640000
DOIT     MVC   SKIPCTL,SKIPON                                           05650000
         MVC   STOPCTL,STOPON                                           05660000
         MVC   DUMPCTL,DUMPON                                           05670000
         MVC   MOVECTL,MOVEON                                           05680000
         MVI   GO,X'FF'                                                 05690000
         BAL   R14,OPENUT1                 PERFORM                      05700000
         B     EXIT2                                                    05710000
         AP    SKIPNUM,RECOUNT             CATCH UP IF NEEDED           05720000
         CLI   OUTON,0                     ANY OUTPUT                   05730000
         BE    READDSN                         NO                       05740000
*                                                                     * 05750000
         BAL   R14,OPENUT2                 PERFORM                      05760000
         B     OUTOFF                                                   05770000
         BAL   R14,PUNCHER                 PERFORM                      05780000
         B     READDSN                                                  05790000
         BAL   R14,DSNPUT                  PERFORM                      05800000
         B     EOFDATA                                                  05810000
         B     READDSN                                                  05820000
OUTOFF   MVI   OUTON,0                     DISABLE OUTPUT               05830000
         MVI   MOVEON,0                                                 05840000
*********************************************************************** 05850000
*        BEGIN HERE                                                   * 05860000
*********************************************************************** 05870000
READDSN  EQU   *                                                        05880000
         CP    OUTCOUNT,COUNTNUM           ENOUGH OUTPUT                05890000
         BNL   EOFDATA                         YES                      05900000
*                                                                     * 05910000
         BAL   R14,TESTPRMP                PERFORM                      05920000
         B     EOFDATA                                                  05930000
*                                                                     * 05940000
         BAL   R14,DSNGET                  PERFORM                      05950000
         B     EOFDATA                     EOF OR IOERR                 05960000
         CP    RECOUNT,SKIPNUM          ENOUGH SKIPPED                  05970000
         BNH   READDSN                        NO                        05980000
*                                                                     * 05990000
         BAL   R14,SKIPFIND                PERFORM                      06000000
         B     READDSN                     NOT THERE YET                06010000
*                                                                     * 06020000
         BAL   R14,CHKSERCH                CHK CRITERIA                 06030000
         B     READDSN                       NO MATCH                   06040000
         BAL   R14,LISTHEX2                PERFORM                      06050000
         BAL   R14,MOVER                   PERFORM                      06060000
         BAL   R14,DSNPUT                  PERFORM                      06070000
         B     EOFDATA                                                  06080000
*                                                                     * 06090000
         AP    OUTCOUNT,=P'1'              +1 TO OUTPUT REC COUNT       06100000
         BAL   R14,LISTRTN                 LISTING ONLY                 06110000
         B     EOFDATA                                                  06120000
         B     READDSN                                                  06130000
*********************************************************************** 06140000
         EJECT                                                          06150000
*********************************************************************** 06160000
REALEOF  EQU   *                                                        06170000
         MVI   INEOF,X'FF'             THIS IS THE END REALLY           06180000
*********************************************************************** 06190000
*                                                                     * 06200000
*        EOF        EOF      EOF     EOF                              * 06210000
*        GOES TO HERE.                                                * 06220000
*                                                                     * 06230000
*********************************************************************** 06240000
EOFDATA  DS    0H                                                       06250000
         OI    RECOUNT+L'RECOUNT-1,X'0F'                                06260000
         UNPK  CTAPEREC(10),RECOUNT                                     06270000
         LA    R1,CTAPECNT                                              06280000
         BAL   R14,WTORTN                  PERFORM                      06290000
         OI    OUTCOUNT+L'OUTCOUNT-1,X'0F'                              06300000
         UNPK  COUTREC(6),OUTCOUNT                                      06310000
         LA    R1,COUTCNT                                               06320000
         BAL   R14,WTORTN                  PERFORM                      06330000
EXIT     EQU   *                                                        06340000
EXIT2    EQU   *                                                        06350000
         CLI   INEOF,0                     REAL EOF                     06360000
         BE    EXIT2B                         NO                        06370000
         BAL   R14,CLOSEUT1                PERFORM                      06380000
EXIT2B   EQU   *                                                        06390000
         CLI   EOF,0                       IS IT REALLY EOF             06400000
         BE    SRESET2                        NOT YET                   06410000
*********************************************************************** 06420000
EXIT3    EQU   *                                                        06430000
         BAL   R14,CLOSEUT2                PERFORM                      06440000
         MVC   FUNCTION,=CL4'CLOSE'                                     06450000
         BAL   R14,HEXDUMP                 PERFORM                      06460000
         LA    R4,SYSIN                                                 06470000
         TM    DCBOFLGS,X'10'              OPEN OK                      06480000
         BZ    EXIT6                          NO                        06490000
         CLOSE (SYSIN)                                                  06500000
         FREEPOOL SYSIN                                                 06510000
EXIT6    EQU   *                                                        06520000
         BAL   R14,SRCHFREM            FREE THE SRCH BLK                06530000
         L     R13,SAVEAREA+4          BACK CHAIN TO CALLERS SAVE AREA  06540000
         LM    R14,R12,12(R13)         RESTORE CALLERS REGS             06550000
RETURNRC LA    R15,0                   SET RETURN CODE                  06560000
         BR    R14                     RETURN TO CALLER                 06570000
*********************************************************************** 06580000
         EJECT                                                          06590000
*********************************************************************** 06600000
NOTSIZ   LA    R1,SIZERR                                                06610000
         BAL   R14,WTORTN                  PERFORM                      06620000
         B     MOREBAD                                                  06630000
*                                                                     * 06640000
NOTNUM   EQU   *                                                        06650000
         LA    R1,NUMERR                                                06660000
         BAL   R14,WTORTN                  PERFORM                      06670000
         B     MOREBAD                                                  06680000
*                                                                     * 06690000
INVALID  EQU   *                                                        06700000
         LA    R1,OPTNERR                                               06710000
         BAL   R14,WTORTN                  PERFORM                      06720000
*                                                                     * 06730000
MOREBAD  MVI   BADKEY,C' '                                              06740000
         MVC   BADKEY+1(L'BADKEY-1),BADKEY                              06750000
         L     R3,PRMSTART                                              06760000
         LH    R4,PRMLNGTH                                              06770000
         BCTR  R4,0                                                     06780000
         LTR   R4,R4                                                    06790000
         BM    INBAD                                                    06800000
         STC   R4,INMVC+1                                               06810000
INMVC    MVC   BADKEY,0(3)                                              06820000
         LA    R1,BADKEY                                                06830000
         BAL   R14,WTORTN                  PERFORM                      06840000
INBAD    CLI   TRACE,X'00'                                              06850000
         BE    OPTION                                                   06860000
         MVC   FUNCTION,=CL4'RELO'                                      06870000
         MVC   DUMPSTRT(4),=A(SUPERGEN)                                 06880000
         MVC   DUMPEND(4),=A(PATCHERA)                                  06890000
         BAL   R14,HEXDUMP                 PERFORM                      06900000
         B     OPTION                                                   06910000
         B     OPTION                                                   06920000
*********************************************************************** 06930000
WTOR14   DC    A(0)                                                     06940000
*                                                                     * 06950000
WTORTN   ST    R14,WTOR14                                               06960000
         SR    R2,R2                                                    06970000
         SH    R1,=H'2'                                                 06980000
         IC    R2,1(R1)                    LENGTH OF MSG                06990000
         STH   R2,WTOBLKL                                               07000000
         BCTR  R2,0                                                     07010000
         STC   R2,WTOMVC+1                                              07020000
WTOMVC   MVC   PRINTREC,2(1)               THE MESSAGE                  07030000
         CLI   PRINTREC+5,C'E'             IS IT ERROR                  07040000
         BNE   PRINTNOP                       NO                        07050000
         MVI   RETURNRC+3,8                SET RETURN CODE              07060000
PRINTNOP CLI   NTRACTON,X'00'                                           07070000
         BE    PRINTRTN                                                 07080000
         LA    R1,WTOPARM                                               07090000
         CALL  DBAWTO                                                   07100000
*                                                                     * 07110000
PRINTRTN BAL   R14,LISTDATA                PERFORM                      07120000
         L     R14,WTOR14                                               07130000
         BR    R14                         EXIT                         07140000
*                                                                     * 07150000
WTOPARM  DC    A(WTOBLKL)                                               07160000
WTOBLKL  DC    AL2(L'WTOBLK)                                            07170000
WTOBLK   DC    CL40' '                                                  07180000
*                                                                     * 07190000
         ORG   WTOBLK                                                   07200000
PRINTREC DC    CL133' '                                                 07210000
         ORG                                                            07220000
*********************************************************************** 07230000
         EJECT                                                          07240000
*********************************************************************** 07250000
LISTXR14 DC    A(0)                                                     07260000
LISTHEX  EQU   *                                                        07270000
         CLI   LISTON,0                    LISTING                      07280000
         BER   R14                            NO                        07290000
         MVC   FUNCTION,=CL4'ASIS'                                      07300000
         MVC   DUMPSTRT(4),=A(EOFMSG)                                   07310000
         ST    R14,LISTXR14                                             07320000
         B     LISTHEX3                                                 07330000
*                                                                     * 07340000
*********************************************************************** 07350000
*                                                                     * 07360000
LISTHEX2 EQU   *                                                        07370000
         CLI   LISTON,0                    LISTING                      07380000
         BER   R14                            NO                        07390000
         ST    R14,LISTXR14                                             07400000
         LA    R4,INDCB                                                 07410000
         ST    R10,DUMPSTRT                                             07420000
         LR    R5,R10                                                   07430000
         AH    R5,DCBLRECL                 +RECSIZE                     07440000
         BCTR  R5,0                                                     07450000
         ST    R5,DUMPEND                  WHERE IT STOPS               07460000
         MVC   FUNCTION,=CL4'RELO'                                      07470000
LISTHEX3 MVC   DTLCOUNT(10),=XL10'40202020202020202120'                 07480000
         ED    DTLCOUNT(10),RECOUNT                                     07490000
         MVC   DTLHDR,DTLHDR-1             CLEAR HEADR                  07500000
         CLI   EXCP,0                      EXCPPING                     07510000
         BE    LISTHEX4                       NO                        07520000
         TM    UT1DVCLS,X'20'                                           07530000
         BZ    LISTHEX4                                                 07540000
         UNPK  HEXWORK2(11),NEXTCCHH(6)                                 07550000
         TR    HEXWORK2(19),TRTBL-C'0'                                  07560000
         MVC   DTLCCHH,HEXWORK2                                         07570000
         BAL   R14,CNVTCCHH                PERFORM                      07580000
         UNPK  HEXWORK2(7),EXCPTTR(4)                                   07590000
         TR    HEXWORK2(19),TRTBL-C'0'                                  07600000
         MVC   DTLTTR,HEXWORK2                                          07610000
         SR    R1,R1                                                    07620000
         IC    R1,NEXTCCHH+5               THE KEYLEN                   07630000
         BAL   R14,CVD                     PERFORM                      07640000
         UNPK  DTLKEY,DOUBLE(8)                                         07650000
         ICM   R1,B'1100',NEXTCCHH+6       THE LRECL                    07660000
         SRA   R1,16                                                    07670000
         BAL   R14,CVD                     PERFORM                      07680000
         UNPK  DTLDATA,DOUBLE(8)                                        07690000
         MVC   DTLHDR,DTLWORK                                           07700000
LISTHEX4 EQU   *                                                        07710000
         BAL   R14,HEXDUMP                 PERFORM                      07720000
         L     R14,LISTXR14                                             07730000
         BR    R14                         EXIT                         07740000
*********************************************************************** 07750000
         EJECT                                                          07760000
*********************************************************************** 07770000
LISTDATA EQU   *                                                        07780000
         ST    R14,LISTXR14                                             07790000
         MVC   FUNCTION,=CL4'ASIS'                                      07800000
         MVC   DUMPSTRT(4),=A(PRINTREC)                                 07810000
         OI    DUMPHDNG,X'80'                                           07820000
         BAL   R14,HEXDUMP                 PERFORM                      07830000
         NI    DUMPHDNG,X'7F'                                           07840000
         MVI   PRINTREC,C' '                                            07850000
         MVC   PRINTREC+1(L'PRINTREC-1),PRINTREC                        07860000
         L     R14,LISTXR14                                             07870000
         BR    R14                         EXIT                         07880000
*********************************************************************** 07890000
HEXDMP14 DC    A(0)                                                     07900000
HEXDUMP  EQU   *                                                        07910000
         ST    R14,HEXDMP14                                             07920000
         LA    R1,DUMPPARM                                              07930000
         CALL  HEXDUMP                                                  07940000
         L     R14,HEXDMP14                                             07950000
         BR    R14                         EXIT                         07960000
*********************************************************************** 07970000
SKIPR14  DC    A(0)                                                     07980000
SKIPFIND EQU   *                                                        07990000
         CLI   SKIPCTL,0                   ARE WE SKIPPING TO A FIND    08000000
         BE    4(R14)                          NO                       08010000
         ST    R14,SKIPR14                                              08020000
         MVI   SELCHECK+1,X'08'                                         08030000
         BAL   R14,SELECT                  TRY TO FIND                  08040000
         B     SKIPFEXT                    NO HIT                       08050000
         MVI   SKIPCTL,0                                                08060000
         L     R14,SKIPR14                                              08070000
         B     4(R14)                                                   08080000
SKIPFEXT L     R14,SKIPR14                                              08090000
         BR    R14                         EXIT                         08100000
*********************************************************************** 08110000
         EJECT                                                          08120000
*********************************************************************** 08130000
LSTRTN14 DC    A(0)                                                     08140000
LISTRTN  CLI   STOPCTL,X'00'                                            08150000
         BE    4(R14)                                                   08160000
         ST    R14,LSTRTN14                                             08170000
         MVI   SELCHECK+1,X'04'                                         08180000
         BAL   R14,SELECT                  PERFORM                      08190000
         B     LISTRTNA                                                 08200000
         MVI   STOPCTL,X'00'                                            08210000
         L     R14,LSTRTN14                                             08220000
         BR    R14                         EXIT                         08230000
LISTRTNA L     R14,LSTRTN14                                             08240000
         B     4(R14)                                                   08250000
*********************************************************************** 08260000
PUNCHER  CLI   DUMPCTL,X'00'                                            08270000
         BER   R14                                                      08280000
         MVI   PNCHDONE,X'00'                                           08290000
         MVI   DUMPCTL,X'00'                                            08300000
         L     R5,SRCHBUF                                               08310000
         LA    R4,INDCB                                                 08320000
         LH    R6,DCBLRECL                                              08330000
PNCHLOOP LH    R4,SRCHLNG                                               08340000
         LTR   R4,R4                                                    08350000
         BZ    PNCHEXIT                                                 08360000
         BCTR  R4,0                                                     08370000
         CLI   SRCHOPTN,X'0C'                                           08380000
         BNE   PNCHNEXT                                                 08390000
         STC   R4,PNCHMVC+1                                             08400000
         LH    R3,SRCHOFF                                               08410000
         LR    R2,R6                                                    08420000
         SR    R2,R4                                                    08430000
         SR    R2,R3                                                    08440000
         BNP   PNCHNEXT                                                 08450000
         MVI   PNCHDONE,X'FF'                                           08460000
         LA    R3,0(R3,R10)                                             08470000
         CH    R2,SRCHRNGE                                              08480000
         BL    PNCHMVC                                                  08490000
         LH    R2,SRCHRNGE                                              08500000
PNCHMVC  MVC   0(R3),8(R5)                                              08510000
         LA    R3,1(R3)                                                 08520000
         BCT   R2,PNCHMVC                                               08530000
PNCHNEXT LA    R5,9(R4,R5)                                              08540000
         B     PNCHLOOP                                                 08550000
PNCHEXIT CLI   PNCHDONE,X'00'                                           08560000
         BE    14                          PSA REFERENCE                08570000
         B     4(R14)                                                   08580000
*********************************************************************** 08590000
MOVER    CLI   MOVECTL,X'00'                                            08600000
         BER   R14                                                      08610000
         L     R5,SRCHBUF                                               08620000
         LH    R6,UT2BLKSI                                              08630000
MOVELOOP LH    R4,SRCHLNG                                               08640000
         LTR   R4,R4                                                    08650000
         BZ    MOVEEXIT                                                 08660000
         CLI   SRCHOPTN,X'14'                                           08670000
         BE    MOVER2                                                   08680000
         CLI   SRCHOPTN,X'10'                                           08690000
         BNE   MOVENEXT                                                 08700000
         LH    R4,SRCHRNGE                                              08710000
         BCTR  R4,0                                                     08720000
         STC   R4,MOVE1MVC+1                                            08730000
         LH    R3,SRCHOFF                                               08740000
         LR    R2,R6                                                    08750000
         SR    R2,R4                                                    08760000
         LA    R4,1                                                     08770000
         SR    R2,R3                                                    08780000
         BNP   MOVENEXT                                                 08790000
         L     R9,UT2BUF                                                08800000
         LA    R9,0(R3,R9)                                              08810000
         LH    R3,8(,R5)                                                08820000
         LA    R3,0(R3,R10)                                             08830000
MOVE1MVC MVC   0(0,9),0(3)                                              08840000
         LA    R4,1                                                     08850000
         B     MOVENEXT                                                 08860000
MOVER2   BCTR  R4,0                                                     08870000
         STC   R4,MOVE2MVC+1                                            08880000
         LH    R3,SRCHOFF                                               08890000
         LR    R2,R6                                                    08900000
         SR    R2,R4                                                    08910000
         SR    R2,R3                                                    08920000
         BNP   MOVENEXT                                                 08930000
         L     R9,UT2BUF                                                08940000
         LA    R9,0(R3,R9)                                              08950000
         CH    R2,SRCHRNGE                                              08960000
         BL    MOVE2MVC                                                 08970000
         LH    R2,SRCHRNGE                                              08980000
MOVE2MVC MVC   0(0,9),8(5)                                              08990000
         LA    R9,1(R9)                                                 09000000
         BCT   R2,MOVE2MVC                                              09010000
MOVENEXT LA    R5,9(R4,R5)                                              09020000
         B     MOVELOOP                                                 09030000
MOVEEXIT L     R10,UT2BUF                                               09040000
         BR    R14                         EXIT                         09050000
*********************************************************************** 09060000
CHKSERCH CLI   SEARCHON,X'00'                                           09070000
         BE    4(R14)                                                   09080000
         MVI   SELCHECK+1,X'00'                                         09090000
*********************************************************************** 09100000
         EJECT                                                          09110000
*********************************************************************** 09120000
SELECT   EQU   *                                                        09130000
*                                                                     * 09140000
SELCTNOP B     4(14)                       SET TO NOP IF ENTRIES        09150000
         MVI   SLECTHIT,X'00'                                           09160000
         L     R5,SRCHBUF                                               09170000
         LA    R4,INDCB                                                 09180000
         LH    R6,DCBLRECL                 THE REC LNGTH                09190000
SELLOOP  LH    R4,SRCHLNG                                               09200000
         LTR   R4,R4                                                    09210000
         BZ    SELEXIT                     ---END ---EXIT               09220000
         BCTR  R4,0                                                     09230000
SELCHECK CLI   SRCHOPTN,0                                               09240000
         BNE   SELINC2                                                  09250000
         STC   4,SELCLC+1                                               09260000
         LH    R3,SRCHOFF                  OFFSET                       09270000
         LR    R2,R6                       THE LRECL                    09280000
         SR    R2,R4                       MINUS CLC LENGTH             09290000
         SR    R2,R3                       MINUS OFFSET                 09300000
         BNP   SELINC                      TOO BAD                      09310000
         LA    R3,0(R3,R10)                                             09320000
         MVC   SELBRAN+1(1),SRCHFUNC                                    09330000
         CH    R2,SRCHRNGE                                              09340000
         BL    SELLOOP2                                                 09350000
         LH    R2,SRCHRNGE                                              09360000
SELLOOP2 EQU   *                                                        09370000
SELCLC   CLC   0(0,3),SRCHDATA                                          09380000
SELBRAN  B     SELHIT                      ---HIT---EXIT                09390000
         LA    3,1(3)                                                   09400000
         BCT   R2,SELLOOP2                                              09410000
*                                                                     * 09420000
SELINC   EQU   *                                                        09430000
         CLI   ANDOR,X'FF'                 ANDING                       09440000
         BER   R14                         YES                          09450000
SELINC2  LA    R5,SRCHCTL+1(R4,R5)                                      09460000
         B     SELLOOP                                                  09470000
*********************************************************************** 09480000
SELHIT   EQU   *                                                        09490000
         CLI   ANDOR,0                     ANDING                       09500000
         BE    4(R14)                         NO                        09510000
         MVI   SLECTHIT,X'FF'                                           09520000
         B     SELINC2                                                  09530000
*********************************************************************** 09540000
SELEXIT  EQU   *                                                        09550000
         CLI   ANDOR,0                     ANDING                       09560000
         BER   R14                            NO                        09570000
         CLI   SLECTHIT,X'00'              STOPPING                     09580000
         BER   R14                            NO                        09590000
         B     4(R14)                                                   09600000
*********************************************************************** 09610000
         EJECT                                                          09620000
*********************************************************************** 09630000
DSNGR14  DC    A(0)                                                     09640000
DSNGET   EQU   *                                                        09650000
         ST    R14,DSNGR14                                              09660000
         L     R10,EXCPBUF                                              09670000
         CLI   EXCP,X'FF'                                               09680000
         BE    EXCPGET                                                  09690000
DSNLOOP  MVI   SYNADERR,0                                               09700000
         GET   INDCB,INBUF                                              09710000
         CLI   SYNADERR,X'FF'                                           09720000
         BNE   DSNEXIT                                                  09730000
         CLI   EROPT,X'04'                                              09740000
         BL    EXCPERRX                                                 09750000
         BC    2,DSNEXIT                                                09760000
         RELSE INDCB                                                    09770000
         B     DSNLOOP                                                  09780000
DSNEXIT  AP    RECOUNT,=P'1'                                            09790000
         LA    R4,INDCB                                                 09800000
         LH    R3,DCBLRECL                                              09810000
         LA    R3,INBUF(R3)                                             09820000
         ST    R3,RECEND                                                09830000
         L     R14,DSNGR14                                              09840000
         B     4(R14)                                                   09850000
*********************************************************************** 09860000
EXCPGET  EQU   *                                                        09870000
         MVI   IOBECB,0                                                 09880000
*                                                                     * 09890000
         LA    R4,INDCB                                                 09900000
         NI    DCBIFLGS,X'3F'                                           09910000
*                                                                     * 09920000
         TM    UT1DVCLS,X'20'                                           09930000
         BZ    EXCPEXCP                                                 09940000
         BAL   R14,EXTENTS                 FIND THE EXTENT              09950000
         B     EXCPERRX                    ITS NOT RIGHT                09960000
*                                                                     * 09970000
         MVC   IOBCCHHR,NEXTCCHH                                        09980000
EXCPEXCP EXCP  IOBIOB                                                   09990000
         WAIT  ECB=IOBECB                                               10000000
*                                                                     * 10010000
         LH    R2,EXCPREAD+6               NUMBER SUPPOSED TO READ      10020000
         SH    R2,IOBBYTES                                              10030000
         LA    R4,INDCB                                                 10040000
         STH   R2,DCBLRECL                 NUMBER OF BYTES READ         10050000
*                                                                     * 10060000
         CLI   IOBECB,X'7F'                ALL OK                       10070000
         BE    DSNEXIT                        YES                       10080000
*                                                                     * 10090000
         CLI   IOBECB,X'42'                EXTENT VIOLATION             10100000
         BNE   EXCPYNAD                       NO                        10110000
         CLI   SAVEDEB,0                   ANY DEBS LEFT                10120000
         BE    EXCPERRX                      NOPE                       10130000
         L     R2,SAVEDEB                                               10140000
         LA    R2,X'10'(R2)                THE NEXT ONE                 10150000
         MVI   NEXTCCHH+4,0                                             10160000
         MVC   NEXTCCHH(4),6(R2)           START EXTENT                 10170000
         B     EXCPGET                                                  10180000
EXCPYNAD EQU   *                                                        10190000
         MVC   HEXWORK1(1),IOBECB                                       10200000
         MVC   HEXWORK1+1(1),IOBSTAT                                    10210000
         MVC   HEXWORK1+2(2),IOBSENS0                                   10220000
         MVC   HEXWORK1+4(5),IOBCCHHR                                   10230000
*                                                                     * 10240000
         UNPK  HEXWORK2(15),HEXWORK1(8)                                 10250000
         UNPK  HEXWORK2+14(5),HEXWORK1+7(3)                             10260000
         TR    HEXWORK2(19),TRTBL-C'0'                                  10270000
         MVC   MSGECB(2),HEXWORK2          ECB                          10280000
         MVC   MSGCSW(2),HEXWORK2+2        CSW                          10290000
         MVC   MSGSENS(4),HEXWORK2+4       SENSE                        10300000
         MVC   MSGCHR(10),HEXWORK2+8       CHR                          10310000
         CLI   IOBECB,X'41'                                             10320000
         BNE   EXCPIOER                                                 10330000
         TM    IOBSTAT,X'01'               EOF                          10340000
         BZ    EXCPIOER                       NOPE                      10350000
         MVI   INEOF,X'FF'                 TELLUM EOF IS HERE           10360000
         AP    RECOUNT,=P'1'               COUNT THIS ONE TOO           10370000
         BAL   R14,LISTHEX                 PERFORM                      10380000
         B     EXCPALL                                                  10390000
EXCPIOER EQU   *                                                        10400000
         LA    R1,IOERR                                                 10410000
         TM    UT1DVCLS,X'20'                                           10420000
         BO    EXCPWTOR                                                 10430000
         MVC   IOERR2(43),IOERR                                         10440000
         LA    R1,IOERR2                                                10450000
EXCPWTOR BAL   R14,WTORTN                  PERFORM                      10460000
EXCPALL  EQU   *                                                        10470000
         CLI   DUMPALL,0                   KEEP ON GOING                10480000
         BE    EXCPERRX                        NO                       10490000
         TM    UT1DVCLS,X'20'                                           10500000
         BZ    EXCPERRX                                                 10510000
         CLC   NEXTCCHH(5),IOBCCHHR        STUCK ON THE SAME            10520000
         BNE   EXCPGET                       NO                         10530000
EXCPERRX EQU   *                                                        10540000
         L     R14,DSNGR14                                              10550000
         BR    R14                         EXIT ADDR                    10560000
*********************************************************************** 10570000
         EJECT                                                          10580000
*********************************************************************** 10590000
         USING UTSYNAD,R15                                              10600000
UTSYNAD  SYNADAF ACSMETH=QSAM                                           10610000
         STM   R13,R15,SYNADREG            SAVE REGS                    10620000
         L     R13,SAVEADDR                                             10630000
         USING SAVEAREA,R13                                             10640000
         DROP  R15                                                      10650000
         MVC   SYNADSAV(72),SAVEAREA                                    10660000
         MVC   IOERR3A(78),50(R1)                                       10670000
         LA    R1,IOERR3                                                10680000
         BAL   R14,WTORTN                  PERFORM                      10690000
         MVC   SAVEAREA(72),SYNADSAV                                    10700000
         MVI   SYNADERR,X'FF'                                           10710000
         OI    SYNADREG+8,X'FF'                                         10720000
         LM    R13,R15,SYNADREG            RESTORE REGS                 10730000
         SVC   68                          SYNADRLS                     10740000
         BR    R14                         EXIT                         10750000
         B     *                                                        10760000
         B     *                                                        10770000
SYNADREG DC    3A(0)                                                    10780000
SYNADSAV DC    18A(0)                                                   10790000
SAVEADDR DC    A(SAVEAREA)                                              10800000
*********************************************************************** 10810000
DSNPR14  DC    A(0)                                                     10820000
DSNPUT   CLI   OUTON,X'00'                                              10830000
         BE    4(R14)                                                   10840000
         ST    R14,DSNPR14                                              10850000
         LA    R4,OUTDCB                                                10860000
         CLI   DCBRECFM,X'C0'                                           10870000
         BNE   DSNPUT1                                                  10880000
         MVC   DCBLRECL(2),INDCB+(DCBLRECL-IHADCB)                      10890000
DSNPUT1  MVI   SYNADERR,X'00'                                           10900000
         GET   OUTDCB,INBUF                                             10910000
         L     R14,DSNPR14                                              10920000
         CLI   SYNADERR,X'00'                                           10930000
         BE    4(R14)                                                   10940000
         BR    R14                         EXIT                         10950000
*********************************************************************** 10960000
         EJECT                                                          10970000
*********************************************************************** 10980000
CNVTBR14 DC    A(0)                                                     10990000
*                                                                     * 11000000
CNVTTTR  EQU   *                                                        11010000
         ST    R14,CNVTBR14                                             11020000
*                                                                     * 11030000
         USING MAPCVT,R15                                               11040000
         LA    R1,DBAPARM2                                              11050000
         LM    R4,R6,0(R1)                 DCB,TTR,CCHHR                11060000
         L     R1,DCBDEBA-1                ADDRESS DEB                  11070000
         L     R0,0(R5)                    TTR TO  CONVERT              11080000
         LR    R2,R6                       WHERE TO STORE NUMBER        11090000
         L     R15,CVT                                                  11100000
         L     R15,CVTPCNVT                TTR->CCHHR CONVERT ROUTINE   11110000
         STM   R0,R12,20(R13)              SAVE REGS                    11120000
         BALR  R14,R15                     STD LINKAGE                  11130000
         DROP  R13                                                      11140000
         USING RESTOR13,R14                                             11150000
RESTOR13 L     R13,ASAVE                   RELOAD BASE                  11160000
         DROP  R14                                                      11170000
         USING SAVEAREA,R13                                             11180000
         LM    R0,R12,20(R13)              RESTORE REGS                 11190000
         B     *+4(15)                                                  11200000
         B     CNVTRTN                     OK                           11210000
         B     CNVTERR                     BAD                          11220000
CNVTRTN  L     R14,CNVTBR14                                             11230000
         B     4(R14)                                                   11240000
*********************************************************************** 11250000
CNVTERR  EQU   *                                                        11260000
         LA    R1,EXTENTMG                                              11270000
         BAL   R14,WTORTN                  PERFORM                      11280000
         L     R14,CNVTBR14                                             11290000
         BR    R14                         EXIT                         11300000
*********************************************************************** 11310000
CVT      EQU   16                                                       11320000
CNVTCCHH EQU   *                                                        11330000
         ST    R14,CNVTBR14                                             11340000
         LA    R1,DBAPARM1                                              11350000
         LM    R4,R6,0(R1)                 DCB,TTR,CCHHR                11360000
         L     R1,DCBDEBA-1                                             11370000
         LR    R2,R6                       CCHHR ADDRESS                11380000
         L     R15,CVT                     CVT ADDRESS                  11390000
         L     R15,CVTPRLTV                                             11400000
         STM   R0,R12,20(R13)              SAVE REGS                    11410000
         BALR  R14,R15                     CCHHR->TTR CONVERT ROUTINE   11420000
         DROP  R13                                                      11430000
         USING *,R14                                                    11440000
A00106E  EQU   *                                                        11450000
         L     R13,ASAVE                                                11460000
         DROP  R14                                                      11470000
         USING SAVEAREA,R13                                             11480000
         LM    R1,R12,24(R13)              RESTORE REGS                 11490000
         ST    R0,0(R5)                    THE TTR                      11500000
         L     R14,CNVTBR14                                             11510000
         BR    R14                         EXIT                         11520000
ASAVE    DC    A(SAVEAREA)                                              11530000
*********************************************************************** 11540000
EXTENT14 DC    A(0)                                                     11550000
SAVEDEB  DC    A(0)                                                     11560000
EXTENTS  EQU   *                                                        11570000
         ST    R14,EXTENT14                                             11580000
         LA    R4,INDCB                                                 11590000
         L     R2,DCBDEBAD                 THE DEB ADDR                 11600000
         SR    R3,R3                                                    11610000
         SR    R5,R5                                                    11620000
         IC    R3,X'10'(R2)                # OF EXTENTS                 11630000
         LA    R2,X'20'(R2)                POINT TO FIRST EXTENT        11640000
EXTENTLP EQU   *                                                        11650000
         CLC   NEXTCCHH(4),6(R2)           BEGIN OF EXTENT              11660000
         BL    EXTENTUP                       NOT THIS ONE              11670000
         CLC   NEXTCCHH(4),10(R2)          END OF EXTENT                11680000
         BH    EXTENTUP                    NOT THIS ONE                 11690000
         B     EXTENTHT                    THIS IS IT                   11700000
EXTENTUP LA    R5,1(R5)                                                 11710000
         LA    R2,X'10'(R2)                TO THE NEXT ONE              11720000
         BCT   R3,EXTENTLP                                              11730000
*                                                                     * 11740000
         LA    R1,EXTENTMG                                              11750000
         BAL   R14,WTORTN                  PERFORM                      11760000
         L     R14,EXTENT14                                             11770000
         BR    R14                         EXIT                         11780000
EXTENTHT EQU   *                                                        11790000
         STC   R5,IOBMBB                                                11800000
         STC   R5,NEXTMBB                                               11810000
         ST    R2,SAVEDEB                                               11820000
         BCTR  R3,0                                                     11830000
         STC   R3,SAVEDEB                                               11840000
         L     R14,EXTENT14                                             11850000
         B     4(R14)                                                   11860000
*********************************************************************** 11870000
         EJECT                                                          11880000
*********************************************************************** 11890000
GETINR14 DC    A(0)                                                     11900000
*                                                                     * 11910000
GETSYSIN ST    R14,GETINR14                                             11920000
         CLI   NTRACTON,X'FF'                                           11930000
         BE    GETPRMPT                                                 11940000
         NOP   GETMORE                                                  11950000
         OI    *-3,X'F0'                                                11960000
         LA    R4,SYSIN                                                 11970000
         LA    R1,DCBDDNAM                                              11980000
         BAL   R14,DDFIND                  PERFORM                      11990000
         B     SYSINEND                                                 12000000
         OPEN  (SYSIN)                                                  12010000
GETMORE  EQU   *                                                        12020000
         LA    R4,SYSIN                                                 12030000
         TM    DCBOFLGS,X'10'              OPEN OK                      12040000
         BZ    SYSINEND                       NO                        12050000
         GET   SYSIN,CTLCARD                                            12060000
GETJOIN  MVC   PRMCOUNT(2),=AL2(72)                                     12070000
         MVC   PRMSTART(4),=A(CTLCARD)                                  12080000
         MVC   PRMHIT(4),PRMSTART                                       12090000
         BAL   R14,SETTRT                  PERFORM                      12100000
         L     R14,GETINR14                                             12110000
         B     4(R14)                                                   12120000
SYSINEND EQU   *                                                        12130000
         L     R14,GETINR14                                             12140000
         BR    R14                         EXIT                         12150000
GETPRMPT CLI   PROMPTED,X'FF'                                           12160000
         BE    WAITPRMP                                                 12170000
         MVI   PRMPTECB,X'00'                                           12180000
         WTOR  MF=(E,PROMPT)                                            12190000
WAITPRMP WAIT  ECB=PRMPTECB                                             12200000
         MVI   PROMPTED,X'00'                                           12210000
         MVI   PRMPTECB,X'00'                                           12220000
         MVC   CTLCARD,CTLIN                                            12230000
         MVC   CTLIN,CTLINCLR                                           12240000
         B     GETJOIN                                                  12250000
*********************************************************************** 12260000
FRSTPRMP ST    R14,GETINR14                                             12270000
         MVI   PROMPTED,X'FF'                                           12280000
         WTOR  MF=(E,PROMPT)                                            12290000
         L     R14,GETINR14                                             12300000
         BR    R14                         EXIT                         12310000
*********************************************************************** 12320000
TESTPRMP TM    PRMPTECB,X'40'                                           12330000
         BCR   1,R14                                                    12340000
         B     4(R14)                                                   12350000
*********************************************************************** 12360000
SETTRT   EQU   *                                                        12370000
         XC    TRTTBL(256),TRTTBL                                       12380000
         MVI   TRTTBL+C' ',C' '            BLANK                        12390000
         MVI   TRTTBL+C',',C','            COMMA                        12400000
         MVI   TRTTBL+C'=',C'='            EQUAL                        12410000
         MVI   TRTTBL+C'(',C'('            LEFT PAREN                   12420000
         MVI   TRTTBL+C')',C')'            RIGHT PAREN                  12430000
         MVI   TRTTBL+C'''',C''''                                       12440000
         MVI   TRTTBL+C'/',C'/'            SLASH                        12450000
         BR    R14                         EXIT                         12460000
*********************************************************************** 12470000
         EJECT                                                          12480000
*********************************************************************** 12490000
* SCAN FIELD FOR DELIMITER                 *                          * 12500000
*********************************************************************** 12510000
PRMSTART DC    A(CTLCARD)                  WHERE TO START SCAN          12520000
PRMHIT   DC    A(0)                        DELIMITER ADDRESS+1          12530000
PRMCHAR  DC    A(0)                        THE DELIM CHARACTER          12540000
PRMLNGTH DC    AL2(0)                      LENGTH OF HIT                12550000
PRMCOUNT DC    AL2(71)                     # COLS TO CHK-DECREMENTED    12560000
*                                                                     * 12570000
PRMSCAN  EQU   *                                                        12580000
         XC    PRMLNGTH(2),PRMLNGTH        INIT TO 0 LNGTH              12590000
         L     R3,PRMSTART                                              12600000
         LH    R5,PRMCOUNT                 BYTES LEFT                   12610000
         LTR   R5,R5                                                    12620000
         BNP   0(R14)                      ---NONE LEFT---EXIT          12630000
         BCTR  R5,0                                                     12640000
         STC   R5,PRMTRT+1                                              12650000
         LA    R1,1(R5,R3)                 PREDICT END                  12660000
PRMTRT   TRT   0(0,3),TRTTBL                                            12670000
         MVC   PRMCHAR+3(1),0(R1)          DELIMITER                    12680000
         LA    R3,1(R1)                    +1 AFTER DELIM               12690000
         ST    R3,PRMHIT                   SAVE                         12700000
         S     R1,PRMSTART                 END-BEGIN=LENGTH             12710000
         SR    R5,R1                       DECR # LEFT                  12720000
         STH   R5,PRMCOUNT                                              12730000
         STH   R1,PRMLNGTH                 LENGTH FROM START TO END     12740000
         BM    0(R14)                      ---PAST END---EXIT           12750000
         LTR   R1,R1                                                    12760000
         BZ    4(R14)                      ---ZERO LENGTH---EXIT        12770000
         B     8(R14)                      ---OK---EXIT                 12780000
*********************************************************************** 12790000
         EJECT                                                          12800000
*********************************************************************** 12810000
CHKADR   DC    A(0)                        ARG TO COMPARE               12820000
CHKTBL   DC    A(0)                        TABLE TO SEARCH AGAINST      12830000
CHKLNGTH DC    AL2(0)                      LENGTH ARG                   12840000
*                                                                     * 12850000
CHKTYPE  EQU   *                                                        12860000
         SR    R5,R5                                                    12870000
         L     R3,CHKADR                   ARG TO COMPARE               12880000
         L     R4,CHKTBL                   TABLE                        12890000
CHK80    EQU   *                                                        12900000
         LH    R2,CHKLNGTH                 LENGTH                       12910000
         BCTR  R2,0                                                     12920000
         STC   R2,CHKCLC+1                 LENGTH TO CHK                12930000
         TM    0(R4),X'80'                 MUST IT EQUAL ITT            12940000
         BZ    CHKCLC                                                   12950000
         IC    R2,0(R4)                                                 12960000
         N     R2,=A(X'0000007F')          TURN '80' OFF                12970000
         BCTR  R2,0                                                     12980000
         STC   R2,CHKCLC+1                                              12990000
CHKCLC   CLC   0(0,3),2(4)                                              13000000
         BE    CHKHIT                      ---HIT----EXIT               13010000
         LA    R5,4(R5)                    BUMP EXIT OFFSET             13020000
         IC    R2,1(R4)                    +LENGTH                      13030000
         AR    R4,R2                                                    13040000
         LA    R4,2(R4)                    +2 CTL                       13050000
         IC    R2,1(R4)                                                 13060000
         LTR   R2,R2                       IS THIS THE END              13070000
         BZ    0(R14)                      ----END--NOHIT---EXIT        13080000
         B     CHK80                                                    13090000
*                                                                     * 13100000
CHKHIT   EQU   *                                                        13110000
         SR    R1,R1                                                    13120000
         IC    R1,0(R4)                    MIN LENGTH                   13130000
         N     R1,=A(X'0000007F')                                       13140000
         BCTR  R1,0                                                     13150000
         SR    R2,R2                                                    13160000
         IC    R2,CHKCLC+1                                              13170000
         CR    R2,R1                       GT MININUM                   13180000
         BLR   R14                         ---NOPE---NOT LONG ENOUGH    13190000
         B     4(R5,R14)                   ---HIT---EXIT                13200000
*********************************************************************** 13210000
         EJECT                                                          13220000
*********************************************************************** 13230000
EXLSTPRT DC    0F'0',X'85',AL3(DCBEXIT)                                 13240000
*                                                                     * 13250000
DCBEXIT  EQU   *                                                        13260000
         USING *,R15                                                    13270000
         DROP  R4                                                       13280000
         USING IHADCB,R1                                                13290000
         CLI   DCBRECFM,0                  ANY RECFM                    13300000
         BNE   DCBEXIT2                      YES                        13310000
         MVI   DCBRECFM,X'C0'              SET TO U MAYBE CAN USE       13320000
DCBEXIT2 OC    DCBBLKSI(2),DCBBLKSI        IS BLKSIZE ZERO              13330000
         BNZR  R14                            NO                        13340000
         MVC   DCBBLKSI(2),DCBLRECL        MAKE BLK=LRECL               13350000
         BR    R14                         EXIT                         13360000
*                                                                     * 13370000
UT1EXLST DC    0F'0',X'87',AL3(UT1JFCB)                                 13380000
UT2EXLST DC    0F'0',X'05',AL3(UT2EXIT),X'87',AL3(UT2JFCB)              13390000
*                                                                     * 13400000
UT2EXIT  EQU   *                                                        13410000
         USING *,R15                                                    13420000
         CLI   DCBRECFM,0                  ANY RECFM                    13430000
         BNE   UT2EX1                         YES                       13440000
         MVC   DCBRECFM(1),INDCB+(DCBRECFM-IHADCB) USE UT1              13450000
UT2EX1   EQU   *                                                        13460000
         OC    DCBLRECL(2),DCBLRECL        ANY LRECL                    13470000
         BNZ   UT2EX2                         YES                       13480000
         MVC   DCBLRECL(2),INDCB+(DCBLRECL-IHADCB) USE UT1              13490000
UT2EX2   EQU   *                                                        13500000
         OC    DCBBLKSI(2),DCBBLKSI        ANY BLKSIZE                  13510000
         BNZR  R14                            YES                       13520000
         MVC   DCBBLKSI(2),INDCB+(DCBBLKSI-IHADCB) USE UT1              13530000
         BR    R14                         EXIT                         13540000
         DROP  R1,R15                                                   13550000
         USING IHADCB,R4                                                13560000
         DS    0D                                                       13570000
UT1JFCB  EQU   *                                                        13580000
UT2JFCB  EQU   *                                                        13590000
         IEFJFCBN                                                       13600000
*                                                                     * 13610000
*********************************************************************** 13620000
         EJECT                                                          13630000
*********************************************************************** 13640000
OPEN1R14 DC    A(0)                                                     13650000
OPENUT1  EQU   *                                                        13660000
         ST    R14,OPEN1R14                                             13670000
         MVC   ERRDDNAM(8),UT1NAME                                      13680000
         LA    R4,INDCB                                                 13690000
         TM    DCBOFLGS,X'10'              ALREADY OPEN                 13700000
         BO    4(R14)                         YES                       13710000
         ZAP   RECOUNT,=P'0'                                            13720000
         MVC   FUNCTION,=CL4'PAGE'                                      13730000
         BAL   R14,HEXDUMP                 PERFORM                      13740000
         LA    R1,UT1NAME                                               13750000
         BAL   R14,DDFIND                  PERFORM                      13760000
         B     OPN1NODD                                                 13770000
         LA    R4,INDCB                                                 13780000
         MVC   DCBDDNAM,UT1NAME                                         13790000
         RDJFCB (INDCB)                                                 13800000
         CLI   WORKDSN,C' '                ANY DSN                      13810000
         BE    OPENUT1G                       NO                        13820000
         MVC   JFCBDSNM(44),WORKDSN                                     13830000
OPENUT1G EQU   *                                                        13840000
         MVC   UT1DSN,JFCBDSNM                                          13850000
         MVC   UT1DD,UT1NAME                                            13860000
         MVC   UT1VOL,JFCBVOLS                                          13870000
         OI    JFCBTSDM,X'08'              DONT BOTHER TO WRITE BACK    13880000
         L     R1,WORKUCB                                               13890000
         MVC   UT1DVADR,13(R1)                                          13900000
         MVC   UT1DVCLS,18(R1)                                          13910000
         TM    JFCBTSDM,X'20'                                           13920000
         BO    OPENUT1H                                                 13930000
         CLI   JFCBVOLS,C' '                                            13940000
         BE    OPENUT1H                                                 13950000
         TM    18(R1),X'20'                                             13960000
         BZ    OPENUT1H                                                 13970000
         CLI   VTOC,X'FF'                  DOING A VTOC                 13980000
         BE    OPENUT1N                    YEP                          13990000
         OBTAIN                            CAMLST                       14000000
         LTR   R15,R15                                                  14010000
         BZ    OPENUT1H                                                 14020000
         LA    R1,DSNERR                                                14030000
         BAL   R14,WTORTN                  PERFORM                      14040000
         B     OPENUT1X                                                 14050000
OPENUT1N MVI   JFCBDSNM,X'04'                                           14060000
         MVC   JFCBDSNM+1(L'JFCBDSNM-1),JFCBDSNM                        14070000
         MVC   UT1DSN(44),=CL44' ******** VTOC ********'                14080000
OPENUT1H EQU   *                                                        14090000
         OPEN  (INDCB),TYPE=J                                           14100000
         LA    R4,INDCB                                                 14110000
         TM    DCBOFLGS,X'10'              OPEN OK                      14120000
         BZ    OPENUT1X                       NO                        14130000
         LH    R3,DCBLRECL                                              14140000
         CVD   R3,DOUBLE                                                14150000
         OI    DOUBLE+7,X'0F'                                           14160000
         UNPK  UT1RECL,DOUBLE(8)                                        14170000
         LH    R3,DCBBLKSI                                              14180000
         LTR   R3,R3                                                    14190000
         BNZ   OPENUT1O                                                 14200000
         LH    R3,DS1BLKL                                               14210000
         STH   R3,DCBBLKSI                                              14220000
OPENUT1O EQU   *                                                        14230000
         SR    R2,R2                                                    14240000
         IC    R2,DCBKEYLE                                              14250000
         LA    R2,0(R2,R3)                                              14260000
         STH   R2,EXCPREAD+6                                            14270000
         CVD   R3,DOUBLE                                                14280000
         OI    DOUBLE+7,X'0F'                                           14290000
         UNPK  UT1BLOCK,DOUBLE(8)                                       14300000
         CLI   DCBRECFM,X'C0'                                           14310000
         BE    OPENUT1I                       YES ITS 'U'               14320000
         LH    R2,DCBLRECL                 GET ONLY ENOUGH FOR A LRECL  14330000
OPENUT1I EQU   *                                                        14340000
         LR    R0,R2                                                    14350000
         STH   R0,UT1BLKSI                                              14360000
         LTR   R0,R0                                                    14370000
         BNZ   OPENUT1L                                                 14380000
         LA    R1,BLKERR                                                14390000
         BAL   R14,WTORTN                  PERFORM                      14400000
         B     OPENUT1X                                                 14410000
OPENUT1L EQU   *                                                        14420000
         GETMAIN R,LV=(0)                                               14430000
         LR    R10,R1                      WHERE THE BUFFER IS          14440000
         ST    R10,EXCPREAD                                             14450000
         ST    R10,EXCPBUF                                              14460000
         CLI   EXCP,X'00'                  IS IT EXCP                   14470000
         BE    OPEN1EXX                       NO                        14480000
         TM    UT1DVCLS,X'20'                                           14490000
         BO    OPN1MVI1                                                 14500000
         MVI   EXCPREAD,X'02'                                           14510000
         MVI   EXCPREAD+4,X'20'                                         14520000
         MVI   IOBIOB,X'02'                                             14530000
         MVC   IOBSTRTB,=A(EXCPREAD)                                    14540000
         B     OPEN1EXX                                                 14550000
OPN1MVI1 MVI   EXCPREAD,X'0E'                                           14560000
OPN1MVI2 MVI   EXCPREAD+4,X'20'                                         14570000
         MVI   IOBIOB,X'42'                                             14580000
         MVC   IOBSTRTB,=A(EXCPCCW)                                     14590000
         CLI   DEBON,0                     ANY DEB                      14600000
         BE    OPENUT1P                       NO                        14610000
         BAL   R14,DEBMOD                  PERFORM                      14620000
OPENUT1P EQU   *                                                        14630000
         CLC   NEXTCCHH(5),=XL5'00'        ANY CCHHR                    14640000
         BNE   OPEN1EXT                       YES THERE IS              14650000
         CLC   EXCPTTR(4),=A(0)                                         14660000
         BNE   OPENUT1K                       THERE IS A TTR            14670000
         MVC   EXCPTTR(4),=A(X'00000100')                               14680000
OPENUT1K EQU   *                                                        14690000
         BAL   R14,CNVTTTR                 MAKE IT A CCHHR              14700000
         B     OPENUT1X                    SOMETHING IS WRONG           14710000
OPEN1EXT EQU   *                                                        14720000
         SR    R2,R2                                                    14730000
         IC    R2,NEXTCCHH+4               THE RECORD                   14740000
         LTR   R2,R2                       IS IT ZERO                   14750000
         BZ    OPEN1EXX                       YES                       14760000
         BCTR  R2,0                        REVISE                       14770000
         STC   R2,NEXTCCHH+4                                            14780000
OPEN1EXX EQU   *                                                        14790000
         L     R14,OPEN1R14                                             14800000
         B     4(R14)                                                   14810000
OPN1NODD LA    R1,DDERR                                                 14820000
         BAL   R14,WTORTN                  PERFORM                      14830000
OPENUT1X EQU   *                                                        14840000
         LA    R1,OPNERR                                                14850000
         BAL   R14,WTORTN                  PERFORM                      14860000
         L     R14,OPEN1R14                                             14870000
         BR    R14                         EXIT                         14880000
*********************************************************************** 14890000
DEBR14   DC    A(0)                                                     14900000
DEBMOD   EQU   *                                                        14910000
         ST    R14,DEBR14                                               14920000
*                                                                     * 14930000
         SR    R1,R1                       AUTH ON                      14940000
         SVC   247                                                      14950000
*                                                                     * 14960000
         MODESET KEY=ZERO                                               14970000
*                                                                     * 14980000
         LA    R4,INDCB                                                 14990000
         L     R2,DCBDEBAD                                              15000000
         LA    R2,X'20'(R2)                FIRST EXTENT                 15010000
         MVC   4((MYDEBE-MYDEBS),R2),MYDEBS                             15020000
*                                                                     * 15030000
         MODESET KEY=NZERO                                              15040000
*                                                                     * 15050000
         LA    R1,1                        AUTH OFF                     15060000
         SVC   247                                                      15070000
         L     R14,DEBR14                                               15080000
         BR    R14                         EXIT                         15090000
*********************************************************************** 15100000
         EJECT                                                          15110000
*********************************************************************** 15120000
OPEN2R14 DC    A(0)                                                     15130000
OPENUT2  EQU   *                                                        15140000
         ST    R14,OPEN2R14                                             15150000
         MVC   ERRDDNAM(8),UT2NAME                                      15160000
         LA    R4,OUTDCB                                                15170000
         TM    DCBOFLGS,X'10'              OPEN YET                     15180000
         BO    OPENUT2A                       YES                       15190000
         MVC   DCBDDNAM(8),UT2NAME                                      15200000
         RDJFCB OUTDCB                                                  15210000
         MVC   UT2DSN,JFCBDSNM                                          15220000
         OPEN  (OUTDCB,(OUTPUT))                                        15230000
         LA    R4,OUTDCB                                                15240000
         TM    DCBOFLGS,X'10'              OPEN OK                      15250000
         BZ    OPENUT2X                                                 15260000
         MVC   UT2BLKSI(2),82(R4)                                       15270000
         CLI   36(R4),X'C0'                                             15280000
         BNE   OPENUT2A                                                 15290000
         MVC   UT2BLKSI(2),62(R4)                                       15300000
OPENUT2A CLI   MOVEON,X'00'                                             15310000
         BE    OPENUT2B                                                 15320000
         BAL   R14,UT2GETM                 PERFORM                      15330000
OPENUT2B L     R14,OPEN2R14                                             15340000
         B     4(R14)                                                   15350000
OPENUT2X EQU   *                                                        15360000
         LA    R1,OPNERR                                                15370000
         BAL   R14,WTORTN                  PERFORM                      15380000
         L     R14,OPEN2R14                                             15390000
         BR    R14                         EXIT                         15400000
*********************************************************************** 15410000
         EJECT                                                          15420000
*********************************************************************** 15430000
CLOSE114 DC    A(0)                       A(0)                          15440000
CLOSEUT1 EQU   *                                                        15450000
         ST    R14,CLOSE114                                             15460000
         MVI   INEOF,0                     RESET TO NO EOF              15470000
         LA    R4,INDCB                                                 15480000
         TM    DCBOFLGS,X'10'              WAS IT OPEN                  15490000
         BZR   R14                            NO                        15500000
         LH    R0,UT1BLKSI                                              15510000
         LTR   R0,R0                                                    15520000
         BZ    CLOSET1F                    NOTHING THERE                15530000
         FREEMAIN R,LV=(0),A=EXCPBUF                                    15540000
CLOSET1F EQU   *                                                        15550000
         CLOSE (INDCB)                                                  15560000
         CLI   EXCP,X'FF'                  WAS IT EXCP                  15570000
         BE    CLOS1EXT                       YES                       15580000
         FREEPOOL INDCB                                                 15590000
CLOS1EXT EQU   *                                                        15600000
         L     R14,CLOSE114                                             15610000
         BR    R14                         EXIT                         15620000
*********************************************************************** 15630000
         EJECT                                                          15640000
*********************************************************************** 15650000
CLOSE214 DC    A(0)                                                     15660000
CLOSEUT2 EQU   *                                                        15670000
         ST    R14,CLOSE214                                             15680000
         LA    R4,OUTDCB                                                15690000
         TM    DCBOFLGS,X'10'              OPEN EVER                    15700000
         BZR   R14                            NO                        15710000
         BAL   R14,UT2FREM                 PERFORM                      15720000
         CLOSE (OUTDCB)                                                 15730000
         FREEPOOL OUTDCB                                                15740000
         L     R14,CLOSE214                                             15750000
         BR    R14                         EXIT                         15760000
*********************************************************************** 15770000
         EJECT                                                          15780000
*********************************************************************** 15790000
DDFIND14 DC    A(0)                                                     15800000
DDFIND   EQU   *                                                        15810000
         ST    R14,DDFIND14                                             15820000
         ST    R1,DDPARM                                                15830000
         MVC   DDERRNAM(8),0(R1)                                        15840000
         LA    R1,DDPARM                                                15850000
         CALL  VDBS$DDF                                                 15860000
         B     *+4(R15)                                                 15870000
         B     DDFINDOK                                                 15880000
         B     DDFINDER                                                 15890000
DDFINDOK EQU   *                                                        15900000
         L     R15,WORKTIOT                                             15910000
         SR    R14,R14                                                  15920000
         ICM   R14,7,41-24(R15)                                         15930000
         ST    R14,WORKUCB                                              15940000
         L     R14,DDFIND14                                             15950000
         B     4(R14)                                                   15960000
DDFINDER EQU   *                                                        15970000
         SR    R14,R14                                                  15980000
         ST    R14,WORKUCB                                              15990000
         L     R14,DDFIND14                                             16000000
         BR    R14                         EXIT                         16010000
*********************************************************************** 16020000
DDPARM   DC    A(*)                                                     16030000
         DC    X'80',AL3(WORKTIOT)                                      16040000
WORKTIOT DC    A(0)                                                     16050000
WORKUCB  DC    A(0)                                                     16060000
*********************************************************************** 16070000
PACKR14  DC    A(0)                                                     16080000
PACK     EQU   *                                                        16090000
         ST    R14,PACKR14                                              16100000
         LH    R4,PRMLNGTH                                              16110000
         BCTR  R4,0                                                     16120000
         EX    R4,NUMCHK                                                16130000
         BNZ   PACKERR                                                  16140000
         EX    R4,PACKITT                                               16150000
         CVB   R1,DOUBLE                                                16160000
         B     4(R14)                                                   16170000
CVD      EQU   *                                                        16180000
         CVD   R1,DOUBLE                                                16190000
         OI    DOUBLE+7,X'0F'                                           16200000
         BR    R14                         EXIT                         16210000
PACKERR  EQU   *                                                        16220000
         L     R14,PACKR14                                              16230000
         BR    R14                         EXIT                         16240000
*********************************************************************** 16250000
PACKITT  PACK  DOUBLE(8),VALUE(0)                                       16260000
NUMCHK   TRT   VALUE(0),TRTNUM                                          16270000
*********************************************************************** 16280000
         EJECT                                                          16290000
*********************************************************************** 16300000
SRCHGM14 DC    A(0)                                                     16310000
SRCHGETM EQU   *                                                        16320000
         ST    R14,SRCHGM14                                             16330000
         CLC   SRCHBUF(4),=A(0)                                         16340000
         BNER  R14                                                      16350000
         L     R0,SRCHSIZE                AMT STORAGE WE WANT FOR SRCHS 16360000
         GETMAIN R,LV=(0)                                               16370000
         ST    R1,SRCHADR                                               16380000
         ST    R1,SRCHBUF                                               16390000
         LR    R5,R1                                                    16400000
         XC    SRCHOFF(SRCHCTL),SRCHOFF                                 16410000
         L     R14,SRCHGM14                                             16420000
         BR    R14                         EXIT                         16430000
*********************************************************************** 16440000
SRCHFREM EQU   *                                                        16450000
         ST    R14,SRCHGM14                                             16460000
         CLC   SRCHBUF(4),=A(0)                                         16470000
         BER   R14                                                      16480000
         L     R1,SRCHBUF                                               16490000
         L     R0,SRCHSIZE                                              16500000
         FREEMAIN R,LV=(0),A=(1)                                        16510000
         XC    SRCHADR(4),SRCHADR                                       16520000
         XC    SRCHBUF(4),SRCHBUF                                       16530000
         L     R14,SRCHGM14                                             16540000
         BR    R14                         EXIT                         16550000
*********************************************************************** 16560000
UT2GM14  DC    A(0)                                                     16570000
UT2GETM  ST    R14,UT2GM14                                              16580000
         CLC   UT2BUF(4),=A(0)                                          16590000
         BNER  R14                                                      16600000
         LH    R0,UT2BLKSI                                              16610000
         GETMAIN R,LV=(0)                                               16620000
         ST    R1,UT2BUF                                                16630000
         L     R14,UT2GM14                                              16640000
         BR    R14                         EXIT                         16650000
*********************************************************************** 16660000
UT2FREM  ST    R14,UT2GM14                                              16670000
         CLC   UT2BUF(4),=A(0)                                          16680000
         BER   R14                                                      16690000
         LH    R0,UT2BLKSI                                              16700000
         L     R1,UT2BUF                                                16710000
         FREEMAIN R,LV=(0),A=(1)                                        16720000
         XC    UT2BUF(4),UT2BUF                                         16730000
         L     R14,UT2GM14                                              16740000
         BR    R14                         EXIT                         16750000
*********************************************************************** 16760000
         EJECT                                                          16770000
TRTTBL   DC    256X'00'                                                 16780000
HEXTBL   EQU   *-C'A'                                                   16790000
HEX0     DC    (1+C'9'-C'A')X'00'                                       16800000
         ORG   HEX0                                                     16810000
HEX1     DC    6AL1((*-HEX1)+10)                                        16820000
         ORG   HEX0+(C'0'-C'A')                                         16830000
HEX2     DC    10AL1(*-HEX2)                                            16840000
         ORG                                                            16850000
HEXWORK1 DC    XL10'0'                                                  16860000
HEXWORK2 DC    XL19'0'                                                  16870000
*                                                                     * 16880000
TRTBL    DC    C'0123456789ABCDEF'                                      16890000
*                                                                     * 16900000
TRTNUM   DC    256AL1(*-TRTNUM)                                         16910000
         ORG   TRTNUM+C'0'                                              16920000
         DC    10X'00'                                                  16930000
         ORG                                                            16940000
*                                                                     * 16950000
         DC    CL2' '                                                   16960000
CTLCARD  DC    CL80' '                                                  16970000
CTLINCLR DC    CL80' '                                                  16980000
CTLIN    EQU   CTLINCLR+1                                               16990000
         DC    CL(256-2*80)' '                                          17000000
*                                                                     * 17010000
VALUECLR DC    CL80' '                                                  17020000
VALUE    EQU   VALUECLR+1                                               17030000
         DC    CL2' '                                                   17040000
HEADING  DC    CL132' '                                                 17050000
         ORG   HEADING                                                  17060000
         DC    C'SUPERGEN DSN DUMP  '                                   17070000
UT1VOL   DC    CL6' '                                                   17080000
         DC    C'-'                                                     17090000
UT1DVADR DC    CL3' '                                                   17100000
         DC    C'-'                                                     17110000
UT1DD    DC    CL8' '                                                   17120000
         DC    C' '                                                     17130000
UT1RECL  DC    CL5' '                                                   17140000
         DC    C'/'                                                     17150000
UT1BLOCK DC    CL5' '                                                   17160000
         DC    C' '                                                     17170000
UT1DSN   DC    CL44' '                                                  17180000
         ORG                                                            17190000
EOFMSG   DC    CL132'******** EOF RECORD *********'                     17200000
*                                                                     * 17210000
DTLWORKS EQU   *                                                        17220000
DTLWORK  DC    C' CCHHR='                                               17230000
DTLCCHH  DC    CL10' '                                                  17240000
         DC    C' KEYLEN='                                              17250000
DTLKEY   DC    CL3' '                                                   17260000
         DC    C' BLKSIZE='                                             17270000
DTLDATA  DC    CL5' '                                                   17280000
         DC    C' TTR='                                                 17290000
DTLTTR   DC    CL6' '                                                   17300000
DTLWORKE EQU   *                                                        17310000
*                                                                     * 17320000
DETAIL   DC    CL132' '                                                 17330000
         ORG   DETAIL                                                   17340000
         DC    C'RECORD SEQUENCE NUMBER1234567890'                      17350000
DTLCOUNT EQU   *-10                                                     17360000
         DC    C' '                                                     17370000
DTLHDR   DC    CL(DTLWORKE-DTLWORKS)' '                                 17380000
         ORG                                                            17390000
*                                                                     * 17400000
UT2DSN   DC    CL44' '                                                  17410000
WORKDSN  DC    CL44' '                                                  17420000
*                                                                     * 17430000
         DC    AL2(L'CTAPECNT)                                          17440000
CTAPECNT DC    C'SPGENMSG10- # OF RECORDS READ=1234567890'              17450000
CTAPEREC EQU   *-10                                                     17460000
*                                                                     * 17470000
         DC    AL2(L'COUTCNT)                                           17480000
COUTCNT  DC    C'SPGENMSG11- # OF RECORDS SELECTED=123456'              17490000
COUTREC  EQU   *-6                                                      17500000
*                                                                     * 17510000
         DC    AL2(L'DDERR)                                             17520000
DDERR    DC    C'SPGENERR03--- DDNAME MISSING ----12345678'             17530000
DDERRNAM EQU   *-8                                                      17540000
*                                                                     * 17550000
         DC    AL2(L'DSNERR)                                            17560000
DSNERR   DC    C'SPGENERR06---DATASET NOT ON VOLUME ---SEE HDNG'        17570000
*                                                                     * 17580000
         DC    AL2(L'NUMERR)                                            17590000
NUMERR   DC    C'SPGENERR05---THATS NOT A NUMBER---'                    17600000
*                                                                     * 17610000
         DC    AL2(L'SIZERR)                                            17620000
SIZERR   DC    C'SPGENERR04--THIS NUMBER SHOULD BE G.E. TO 1ST NUMBER'  17630000
*                                                                     * 17640000
         DC    AL2(L'OPTNERR)                                           17650000
OPTNERR  DC    C'SPGENERR02--INVALID COMMAND : '                        17660000
*                                                                     * 17670000
         DC    AL2(L'BADKEY)                                            17680000
BADKEY   DC    CL80' '                                                  17690000
*                                                                     * 17700000
         DC    AL2(L'OPNERR)                                            17710000
OPNERR   DC    C'SPGENERR01---OPEN HAS FAILED----12345678'              17720000
ERRDDNAM EQU   *-8                                                      17730000
*                                                                     * 17740000
         DC    AL2(L'BLKERR)                                            17750000
BLKERR   DC    C'SPGENERR77----THE BLKSIZE IS ZERO---PLEASE CORRECT'    17760000
*                                                                     * 17770000
         DC    AL2(L'IOERR)                                             17780000
IOERR DC C'SPGENERR88-IOERROR-ECB=12,CSW=12,SENSE=1234,CHR=123456789A'  17790000
MSGECB   EQU   IOERR+23                                                 17800000
MSGCSW   EQU   IOERR+30                                                 17810000
MSGSENS  EQU   IOERR+39                                                 17820000
MSGCHR   EQU   IOERR+48                                                 17830000
*                                                                     * 17840000
         DC    AL2(L'IOERR2)                                            17850000
IOERR2   DC    C'SPGENERR88-IOERROR-ECB=12,CSW=12,SENSE=1234'           17860000
*                                                                     * 17870000
         DC    AL2(L'IOERR3)                                            17880000
IOERR3   DC    CL97' '                                                  17890000
         ORG   IOERR3                                                   17900000
         DC    C'SPGENERR88-IOERROR-'                                   17910000
IOERR3A  EQU   *                                                        17920000
         ORG                                                            17930000
*                                                                     * 17940000
         DC    AL2(L'EXTENTMG)                                          17950000
EXTENTMG DC    C'SPGENERR04---ADDRESS OUTSIDE EXTENTS---'               17960000
*                                                                     * 17970000
TAPER14  DC    A(0)                                                     17980000
*                                                                     * 17990000
EXCPBUF  DC    A(0)                                                     18000000
UT2BUF   DC    A(0)                                                     18010000
RECEND   DC    A(0)                                                     18020000
SRCHADR  DC    A(0)                                                     18030000
SRCHBUF  DC    A(0)                                                     18040000
SRCHSIZE DC    A(4000)                 SIZE FOR FINDS                   18050000
*                                                                     * 18060000
PARMADR  DC    A(0)                                                     18070000
BLOCKADR DC    A(0)                                                     18080000
BLOCKEND DC    A(0)                                                     18090000
*                                                                     * 18100000
DBAPARM1 DC    A(INDCB)                                                 18110000
         DC    A(EXCPTTR)                                               18120000
         DC    A(NEXTMBB)                                               18130000
*                                                                     * 18140000
DBAPARM2 DC    A(INDCB)                                                 18150000
         DC    A(EXCPTTR)              THE TTR IN                       18160000
         DC    A(NEXTMBB)              THE CCHHR OUT                    18170000
*                                                                     * 18180000
CAMLST   CAMLST                        SEARCH,UT1DSN,UT1VOL,DSCB        18190000
DSCB     DC    CL148' '                                                 18200000
DS1BLKL  EQU   DSCB+86-44                                               18210000
*                                                                     * 18220000
DUMPPARM DS    0F                                                       18230000
DUMPTYP  DC    A(FUNCTION)                                              18240000
DUMPSTRT DC    A(0)                                                     18250000
DUMPEND  DC    A(4)                                                     18260000
DUMPHDNG DC    A(HEADING)                                               18270000
DUMPDTL  DC    AL1(X'80'),AL3(DETAIL)                                   18280000
*********************************************************************** 18290000
*                                                                     * 18300000
FUNCTION DC    CL4'RELO'                                                18310000
HEXCHAR  DC    C'    '                                                  18320000
VTOC     DC    X'00'                                                    18330000
EXCP     DC    X'00'                                                    18340000
ANDOR    DC    X'00'                                                    18350000
DUMPALL  DC    X'00'                                                    18360000
SKIPCTL  DC    X'00'                                                    18370000
SKIPON   DC    X'00'                                                    18380000
DUMPCTL  DC    X'00'                                                    18390000
DUMPON   DC    X'00'                                                    18400000
PNCHDONE DC    X'00'                                                    18410000
MOVECTL  DC    X'00'                                                    18420000
MOVEON   DC    X'00'                                                    18430000
WORKFUNC DC    X'00'                                                    18440000
LISTON   DC    X'00'                                                    18450000
SEARCHON DC    X'00'                                                    18460000
OUTON    DC    X'FF'                                                    18470000
STOPCTL  DC    X'00'                                                    18480000
STOPON   DC    X'00'                                                    18490000
DEBON    DC    X'00'                                                    18500000
KEEPON   DC    X'00'                                                    18510000
SLECTHIT DC    X'00'                                                    18520000
TRACE    DC    X'00'                                                    18530000
NTRACTON DC    X'00'                                                    18540000
PROMPTED DC    X'00'                                                    18550000
SYNADERR DC    X'00'                                                    18560000
EROPT    DC    X'00'                                                    18570000
         EJECT                                                          18580000
*                                                                     * 18590000
KEYWORD1 DS    0F                                                       18600000
         DC    AL1(1,L'KEY19)                                           18610000
KEY19    DC    C'END'                                                   18620000
         DC    AL1(1,L'KEY20)                                           18630000
KEY20    DC    C'*'                                                     18640000
         DC    AL1(1,L'KEY1A)                                           18650000
KEY1A    DC    C'DUMP'                                                  18660000
         DC    AL1(X'82',L'KEY1B)                                       18670000
KEY1B    DC    C'OR'                                                    18680000
         DC    AL1(X'83',L'KEY1C)                                       18690000
KEY1C    DC    C'AND'                                                   18700000
         DC    AL1(1,L'KEY1D)                                           18710000
KEY1D    DC    C'KEEP'                                                  18720000
         DC    AL1(1,L'KEY1E)                                           18730000
KEY1E    DC    C'PUNCH'                                                 18740000
         DC    AL1(1,L'KEY1F)                                           18750000
KEY1F    DC    C'MOVE'                                                  18760000
         DC    AL1(1,L'KEY1I)                                           18770000
KEY1I    DC    C'SKIP'                                                  18780000
         DC    AL1(1,L'KEY1J)                                           18790000
KEY1J    DC    C'COUNT'                                                 18800000
         DC    AL1(X'84',L'KEY1K)                                       18810000
KEY1K    DC    C'VTOC'                                                  18820000
         DC    AL1(X'84',L'KEY1L)                                       18830000
KEY1L    DC    C'EXCP'                                                  18840000
         DC    AL1(1,L'KEY1M)                                           18850000
KEY1M    DC    C'FIND'                                                  18860000
         DC    AL1(1,L'KEY1N)                                           18870000
KEY1N    DC    C'RESET'                                                 18880000
         DC    AL1(1,L'KEY1O)                                           18890000
KEY1O    DC    C'INDD'                                                  18900000
         DC    AL1(1,L'KEY1P)                                           18910000
KEY1P    DC    C'OUTDD'                                                 18920000
         DC    AL1(1,L'KEY1Q)                                           18930000
KEY1Q    DC    C'GO'                                                    18940000
         DC    AL1(1,L'KEY1R)                                           18950000
KEY1R    DC    C'BLKSIZE'                                               18960000
         DC    AL1(X'83',L'KEY1S)                                       18970000
KEY1S    DC    C'CYL'                                                   18980000
         DC    AL1(X'84',L'KEY1T)                                       18990000
KEY1T    DC    C'HEAD'                                                  19000000
         DC    AL1(X'83',L'KEY1U)                                       19010000
KEY1U    DC    C'REC'                                                   19020000
         DC    AL1(X'85',L'KEY1V)                                       19030000
KEY1V    DC    C'CCHHR'                                                 19040000
         DC    AL1(X'83',L'KEY1W)                                       19050000
KEY1W    DC    C'TTR'                                                   19060000
         DC    AL1(X'83',L'KEY1X)                                       19070000
KEY1X    DC    C'DSN'                                                   19080000
         DC    AL1(X'83',L'KEY1Y)                                       19090000
KEY1Y    DC    C'EOF'                                                   19100000
         DC    AL1(X'83',L'KEY1Z)                                       19110000
KEY1Z    DC    C'DEB'                                                   19120000
         DC    AL1(3,L'KEY1G)                                           19130000
KEY1G    DC    C'INTERACT'                                              19140000
         DC    AL1(2,L'KEY1H)                                           19150000
KEY1H    DC    C'EROPT'                                                 19160000
         DC    AL2(0)                                                   19170000
*                                                                     * 19180000
         DS    0F                                                       19190000
         EJECT                                                          19200000
KEYWORD2 DS    0F                                                       19210000
         DC    AL1(X'82',L'KEY2A)                                       19220000
KEY2A    DC    C'EQ'                                                    19230000
         DC    AL1(X'82',L'KEY2B)                                       19240000
KEY2B    DC    C'NE'                                                    19250000
         DC    AL1(X'82',L'KEY2C)                                       19260000
KEY2C    DC    C'LT'                                                    19270000
         DC    AL1(X'82',L'KEY2D)                                       19280000
KEY2D    DC    C'GT'                                                    19290000
         DC    AL1(X'82',L'KEY2E)                                       19300000
KEY2E    DC    C'LE'                                                    19310000
         DC    AL1(X'82',L'KEY2F)                                       19320000
KEY2F    DC    C'GE'                                                    19330000
KEYWORD3 EQU   *                                                        19340000
         DC    AL1(X'81',L'KEY3A)                                       19350000
KEY3A    DC    C'X'                                                     19360000
         DC    AL1(X'81',L'KEY3B)                                       19370000
KEY3B    DC    C'C'                                                     19380000
         DC    AL1(X'84',L'KEY3C)                                       19390000
KEY3C    DC    C'STOP'                                                  19400000
         DC    AL1(X'84',L'KEY3D)                                       19410000
KEY3D    DC    C'SKIP'                                                  19420000
         DC    AL2(0)                                                   19430000
         DC    AL2(0)                                                   19440000
         LTORG                                                          19450000
RECOUNT  DC    PL5'0'                                                   19460000
SKIPNUM  DC    PL6'0'                                                   19470000
COUNTNUM DC    PL6'9999999'                                             19480000
OUTCOUNT DC    PL6'0'                                                   19490000
*                                                                     * 19500000
UT1DVCLS DC    XL1'0'                                                   19510000
UT1BLKSI DC    H'0'                                                     19520000
UT1NAME  DC    CL8'SYSUT1'                                              19530000
UT2BLKSI DC    H'0'                                                     19540000
UT2NAME  DC    CL8'SYSUT2'                                              19550000
*                                                                     * 19560000
*                                                                     * 19570000
EOF      DC    X'00'                                                    19580000
INEOF    DC    X'00'                                                    19590000
ERROR    DC    X'00'                                                    19600000
GO       DC    X'00'                                                    19610000
*                                                                     * 19620000
*                                                                     * 19630000
DOUBLE   DC    D'0'                                                     19640000
         SPACE                                                          19650000
*********************************************************************** 19660000
*                                                                     * 19670000
*        DCBS FOR THE PROGRAM                                         * 19680000
*                                                                     * 19690000
*********************************************************************** 19700000
*                                                                     * 19710000
         PRINT NOGEN                                                    19720000
SYSIN    DCB   DDNAME=SYSIN,MACRF=GM,DSORG=PS,                         X19730000
               RECFM=FB,LRECL=80,EODAD=SYSINEND,EXLST=EXLSTPRT          19740000
*                                                                     * 19750000
*                                                                     * 19760000
PROMPT   WTOR  'SPGENMSG00--ENTER FUNCTION--',CTLIN,80,PRMPTECB,MF=L    19770000
PRMPTECB DC    F'0'                                                     19780000
*                                                                     * 19790000
OUTDCB   DCB   DDNAME=SYSUT2,DSORG=PS,MACRF=PM,EXLST=UT2EXLST,         X19800000
               SYNAD=UTSYNAD,EROPT=SKP                                  19810000
*                                                                     * 19820000
INDCB    DCB   DDNAME=SYSUT1,DSORG=PS,MACRF=GM,EODAD=REALEOF,          X19830000
               EXLST=UT1EXLST,SYNAD=UTSYNAD,EROPT=ACC                   19840000
*                                                                     * 19850000
MODELDCB DCB   DDNAME=SYSUT1,DSORG=PS,MACRF=GM,EODAD=REALEOF,          X19860000
               EXLST=UT1EXLST,SYNAD=UTSYNAD,EROPT=ACC                   19870000
LMODEL   EQU   *-MODELDCB                                               19880000
*                                                                     * 19890000
         PRINT GEN                                                      19900000
         DBAIOB CCW=EXCPCCW,PREFIX=IOB,DCB=INDCB                        19910000
*                                                                     * 19920000
EXCPTTR  DC    A(X'00000100')                                           19930000
NEXTMBB  DC    XL3'00'                                                  19940000
NEXTCCHH DC    XL5'00'                                                  19950000
         DC    XL3'00'                                                  19960000
WORKCCHH DC    XL8'00'                                                  19970000
         DC    X'00'                                                    19980000
*                                                                     * 19990000
EXCPCCW  CCW   X'31',IOBCCHHR,X'40',5                                   20000000
         CCW   X'08',EXCPCCW,X'00',1                                    20010000
EXCPLOOP CCW   X'92',NEXTCCHH,X'60',8  MT COUNT                         20020000
EXCPREAD CCW   X'0E',*,X'20',0        READ KEY,DATA                     20030000
         CCW   X'08',EXCPLOOP,0,1      TRY TO LOOP IT                   20040000
         DS    0D                                                       20050000
MYDEBS   DC    AL2(0)                  BIN                              20060000
         DC    AL4(0)                  CCHH START                       20070000
         DC    H'-1',H'-1'             CCHH END                         20080000
         DC    X'7FFF'                 # TRKS                           20090000
MYDEBE   EQU   *                                                        20100000
*********************************************************************** 20110000
EXCPDCB  DCB   DDNAME=EEEE,MACRF=E,DSORG=PS,EXLST=UT1EXLST              20120000
LEXCPDCB EQU   *-EXCPDCB                                                20130000
*                                                                     * 20140000
PATCHERA DC    400X'00'                                                 20150000
*                                                                     * 20160000
         EJECT                                                          20170000
*********************************************************************** 20180000
*                                                                     * 20190000
*        DSECTS USED BY THE PROGRAM                                   * 20200000
*                                                                     * 20210000
*********************************************************************** 20220000
*                                                                     * 20230000
SEARCH   DSECT                                                          20240000
SRCHOFF  DC    H'0'                                                     20250000
SRCHRNGE DC    H'0'                                                     20260000
SRCHLNG  DC    H'0'                                                     20270000
SRCHOPTN DC    X'00'                                                    20280000
SRCHFUNC DC    X'00'                                                    20290000
SRCHCTL  EQU   *-SRCHOFF                                                20300000
SRCHDATA EQU   *                                                        20310000
*                                                                       20320000
INBUF    DSECT                                                          20330000
         PRINT NOGEN                                                    20340000
IHADCB   DCBD  DSORG=PS,DEVD=DA                                         20350000
MAPCVT   DSECT                                                          20360000
         CVT   SYS=AOS1                                                 20370000
         PRINT GEN                                                      20380000
         REGS                                                           20390000
EQ       EQU   X'80'                                                    20400000
NE       EQU   X'70'                                                    20410000
LT       EQU   X'40'                                                    20420000
GT       EQU   X'20'                                                    20430000
LE       EQU   EQ+LT                                                    20440000
GE       EQU   EQ+GT                                                    20450000
         END                                                            20460000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR                             00440000
//LKED.SYSLIB   DD  DSN=&SUBLOAD,UNIT=SYSDA,DISP=(OLD,DELETE)           00450000
//SYSIN    DD  *                                                        00460000
 NAME SUPERGEN(R)                                                       00470000
//                                                                      00480000
