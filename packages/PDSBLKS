//PDSBLKS  JOB (TSO),
//             'Install PDSBLKS',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//* ********************************************************            00020000
//* *  INSTALL THE 'PDSBLKS' TSO COMMAND                   *            00030000
//* ********************************************************            00040000
//ASSEM    EXEC ASMFCL,COND=(0,NE),MAC1='SYS1.AMODGEN',                 00050000
//         PARM.ASM='OBJECT,NODECK,LIST',                               00060000
//         PARM.LKED='XREF,LIST,LET'                                    00070000
//SYSIN    DD *                                                         00080000
PDSBLKS  TITLE 'LIST NUMBER OF DIRECTORY BLOCKS ALLOCATED AND USED'     00090000
PDSBLKS  CSECT                                                          00100000
**                                                                      00110000
** THIS COMMNAD PROCESSOR WAS DESIGNED AND DEVELOPED BY                 00120000
**   J. SCHINDLER                                                       00130000
**   COMPUTER SCIENCES CORP.                                            00140000
**   NASA, GODDARD SPACE FLIGHT CENTER                                  00150000
**   GREENBELT, MARYLAND                                                00160000
**   301-982-6768                                                       00170000
**                                                                      00180000
** THIS COMMAND PROCESSOR DISPLAYS THE NUMBER OF PDS DIRECTORY          00190000
** BLOCKS ALLOCATED AND USED.                                           00200000
**                                                                      00210000
**      SYNTAX -                                                        00220000
**             PDSBLKS  'DSNAME'                                        00230000
**                                                                      00240000
**                                                                      00250000
** CSECT ATTR - NONE                                                    00260000
**                                                                      00270000
**                                                                      00280000
** EQUATES -                                                            00290000
**                                                                      00300000
R0       EQU   0  WORK                                                  00310000
R1       EQU   1  LINKAGE, WORK                                         00320000
R2       EQU   2  WORK                                                  00330000
R3       EQU   3  WORK                                                  00340000
R4       EQU   4  WORK                                                  00350000
R5       EQU   5  WORK                                                  00360000
R6       EQU   6  WORK                                                  00370000
R7       EQU   7  WORK                                                  00380000
R8       EQU   8  BASE FOR DAPB AND DCB                                 00390000
R9       EQU   9  BASE FOR PPL AND DAPB                                 00400000
R10      EQU   10 BASE FOR PDL                                          00410000
R11      EQU   11 PROGRAM BASE                                          00420000
R12      EQU   12                                                       00430000
R13      EQU   13 SAVE                                                  00440000
R14      EQU   14 STANDARD LINKAGE                                      00450000
R15      EQU   15 STANDARD LINAGE                                       00460000
**                                                                      00470000
**                                                                      00480000
         SAVE  (14,12),,PDSBLKS                                         00490000
         LR    R11,R15                                                  00500000
         USING PDSBLKS,R11                                              00510000
         ST    R13,SAVE+4                                               00520000
         LA    R10,SAVE                                                 00530000
         ST    R10,8(R13)                                               00540000
         LR    R13,R10                                                  00550000
**                                                                      00560000
** INIT PPL AND DAPL                                                    00570000
**                                                                      00580000
         LM    R2,R5,0(R1)                                              00590000
         LA    R9,MYPPL                                                 00600000
         USING PPL,R9                                                   00610000
         LA    R8,DAIRPL                                                00620000
         USING DAPL,R8                                                  00630000
         ST    R2,PPLCBUF                                               00640000
         ST    R3,PPLUPT                                                00650000
         ST    R5,PPLECT                                                00660000
         ST    R3,DAPLUPT                                               00670000
         ST    R4,DAPLPSCB                                              00680000
         ST    R5,DAPLECT                                               00690000
         LA    R2,ECB                                                   00700000
         ST    R2,PPLECB                                                00710000
         ST    R2,DAPLECB                                               00720000
         LA    R2,DAIRPB                                                00730000
         ST    R2,DAPLDAPB                                              00740000
         L     R2,=V(PARSECL)                                           00750000
         ST    R2,PPLPCL                                                00760000
         LA    R2,ANSWER                                                00770000
         ST    R2,PPLANS                                                00780000
         XC    PPLUWA,PPLUWA                                            00790000
         DROP  R8,R9                                                    00800000
         XC    ECB,ECB                                                  00810000
         LA    R1,MYPPL                                                 00820000
         LINK  EP=IKJPARS  GO PARSE                                     00830000
         LTR   R15,R15                                                  00840000
         BZ    PARSEOK                                                  00850000
         TPUT  PARSERR,L'PARSERR  PARSE ERROR                           00860000
         B     RETURN1                                                  00870000
PARSEOK  DC    0H'0'                                                    00880000
         L     R10,ANSWER                                               00890000
         USING PARSEDL,R10                                              00900000
         L     R2,PARSDSN  LOAD ADDR OF INPUT DSNAME                    00910000
         LH    R3,PARSDSN+4  LOAD LENGTH OF DSNAME                      00920000
         STH   R3,DAIRDSNL                                              00930000
         BCTR  R3,R0                                                    00940000
         EX    R3,MVCDSN                                                00950000
*MVCDSN  MVC   DAIRDSN(0),0(R2)                                         00960000
**                                                                      00970000
**  ALLOCATE DATA SET                                                   00980000
**                                                                      00990000
         LA    R9,DAIRPB                                                01000000
         USING DAPB08,R9                                                01010000
         MVC   DA08CD,=X'0008'                                          01020000
         XC    DA08FLG(6),DA08FLG                                       01030000
         LA    R2,DAIRDSNL                                              01040000
         ST    R2,DA08PDSN                                              01050000
         MVI   DA08DDN,C' '                                             01060000
         MVC   DA08DDN+1(23),DA08DDN                                    01070000
         XC    DA08BLK(16),DA08BLK                                      01080000
         MVC   DA08MNM,=CL8' '                                          01090000
         MVC   DA08PSWD,=CL8' '                                         01100000
         MVI   DA08DSP1,X'08'  SHR                                      01110000
         MVI   DA08DPS2,X'08'  KEEP                                     01120000
         MVI   DA08DPS3,X'08'  KEEP                                     01130000
         TM    PARSDSN+6,X'40'  DSNAME INPUT IN QUOTES?                 01140000
         BZ    PREFIXID  NO                                             01150000
         MVI   DA08CTL,X'00'  DONT PREFIX USER ID                       01160000
         B     GOON                                                     01170000
PREFIXID MVI   DA08CTL,X'20'  PREFIX USER ID                            01180000
GOON     XC    DA08CTL+1(4),DA08CTL+1                                   01190000
         XC    ECB,ECB                                                  01200000
         LA    R1,DAIRPL                                                01210000
         LINK  EP=IKJEFD00  LINK TO DAIR                                01220000
         LTR   R15,R15  ANY ERRORS?                                     01230000
         BZ    TESTPO  NO                                               01240000
         CH    R15,=H'8'                                                01250000
         BE    CATERR                                                   01260000
         CH    R15,=H'12'                                               01270000
         BE    CATERR                                                   01280000
         CH    R15,=H'16'                                               01290000
         BE    NOTIOTS                                                  01300000
         CVD   R15,DWORK                                                01310000
         MVC   DWORK(4),=X'40202120'                                    01320000
         ED    DWORK(4),DWORK+6                                         01330000
         MVC   MSG01C,DWORK+2                                           01340000
         TPUT  MSG01,MSG01L  DAIR RETURN CODE --                        01350000
         B     RETURN1                                                  01360000
NOTIOTS  TPUT  MSG02,L'MSG02  NO TIOTS                                  01370000
         B     RETURN1                                                  01380000
CATERR   CLI   DA08DARC,X'17'  LOCATE ERROR?                            01390000
         BNE   CHKERR  NO                                               01400000
         LH    R2,DA08DARC                                              01410000
         SLL   R2,24                                                    01420000
         SRL   R2,24                                                    01430000
         CH    R2,=H'8'                                                 01440000
         BL    LOCERR                                                   01450000
         CH    R2,=H'16'                                                01460000
         BH    LOCERR                                                   01470000
         TPUT  MSG03,L'MSG03  DATA SET NAME NOT FOUND                   01480000
         B     RETURN1                                                  01490000
LOCERR   CVD   R2,DWORK                                                 01500000
         MVC   DWORK(4),=X'40202120'                                    01510000
         ED    DWORK(4),DWORK+6                                         01520000
         MVC   MSG04C,DWORK+2                                           01530000
         TPUT  MSG04,MSG04L  LOCATE ERROR CODE --                       01540000
         B     RETURN1                                                  01550000
CHKERR   DC    0H'0'                                                    01560000
         CLI   DA08DARC,X'67'  OBTAIN ERROR?                            01570000
         BNE   CHKERR1  NO                                              01580000
         LH    R2,DA08DARC                                              01590000
         SLL   R2,24                                                    01600000
         SRL   R2,24                                                    01610000
         CVD   R2,DWORK                                                 01620000
         MVC   DWORK(4),=X'40202120'                                    01630000
         ED    DWORK(4),DWORK+6                                         01640000
         MVC   MSG05C,DWORK+2                                           01650000
         TPUT  MSG05,MSG05L  OBTAIN ERROR CODE --                       01660000
         B     RETURN1                                                  01670000
CHKERR1  DC    0H'0'                                                    01680000
         CLI   DA08DARC,X'47'                                           01690000
         BNE   DAIRERR                                                  01700000
         LH    R2,DA08DARC                                              01710000
         SLL   R2,24                                                    01720000
         SRL   R2,24                                                    01730000
         CVD   R2,DWORK                                                 01740000
         MVC   DWORK(4),=X'40202120'                                    01750000
         ED    DWORK(4),DWORK+6                                         01760000
         MVC   MSG06C,DWORK+2                                           01770000
         TPUT  MSG06,MSG06L  DADSM ERROR CODE --                        01780000
         B     RETURN1                                                  01790000
DAIRERR  DC    0H'0'                                                    01800000
         CLC   DA08DARC,=X'0210'  IS DATA SET IN USE?                   01810000
         BNE   DAIRERR1  NO                                             01820000
         TPUT  MSG07,L'MSG07                                            01830000
         B     RETURN1                                                  01840000
DAIRERR1 LH    R2,DA08DARC                                              01850000
         LA    R5,4                                                     01860000
LOOP1    SRDL  R2,4                                                     01870000
         SRL   R3,28                                                    01880000
         IC    R7,XLATE(R3)                                             01890000
         STC   R7,MSG08C-1(R5)                                          01900000
         BCT   R5,LOOP1                                                 01910000
         TPUT  MSG08,MSG08L  DYNAMIC ERROR CODE ---                     01920000
         B     RETURN                                                   01930000
TESTPO   DC    0H'0'                                                    01940000
         TM    DA08DSO,X'02'  IS DATA SET PDS?                          01950000
         BO    DSISPO   YES                                             01960000
         TPUT  MSG09,L'MSG09                                            01970000
         B     RETURN                                                   01980000
DSISPO   DC    0H'0'                                                    01990000
         LA    R8,DCB                                                   02000000
         USING IHADCB,R8                                                02010000
         MVC   DCBDDNAM,DA08DDN  MOVE IN DDNAME                         02020000
         OPEN  (DCB,(INPUT))                                            02030000
         TM    DCBOFLGS,X'10'  OPEN SUCESSFUL?                          02040000
         BO    OPENOK  YES                                              02050000
         TPUT  MSG10,L'MSG10  OPEN ERROR                                02060000
         B     RETURN                                                   02070000
OPENOK   DC    0H'0'                                                    02080000
         XR    R2,R2  COUNT REG                                         02090000
         XR    R3,R3  TOTAL COUNT REG                                   02100000
GETREC   GET   DCB                                                      02110000
         LA    R3,1(R3)                                                 02120000
         LH    R4,0(R1)  LOAD LENGTH OF BLK                             02130000
         AR    R4,R1  ADDR OF BYTE PAST END OF RECORD                   02140000
         LA    R1,2(R1)  POINT OT MEMBER NAME                           02150000
         CLC   0(8,R1),=X'FFFFFFFFFFFFFFFF'                             02160000
         BE    GETREC1                                                  02170000
         B     NEXTMEM                                                  02180000
LOOP     CR    R4,R1  END OF RECORD?                                    02190000
         BNH   BUMPCNT  YES                                             02200000
         CLC   0(8,R1),=X'FFFFFFFFFFFFFFFF'  END OF MEMBERS?            02210000
         BE    FINISH1  YES                                             02220000
NEXTMEM  XR    R5,R5                                                    02230000
         NI    11(R1),X'1F'  LEAVE NUMBER OF HALF-WORDS                 02240000
         IC    R5,11(R1)  GET NUMBER OF HALF-WORDS                      02250000
         SLA   R5,1  MULTIPLY BY 2 TO GET NO. OF BYTES                  02260000
         LA    R1,12(R1,R5)  NEXT MEMBER                                02270000
         B     LOOP                                                     02280000
BUMPCNT  LA    R2,1(R2)  NO OF DIR BLKS USED                            02290000
         B     GETREC                                                   02300000
FINISH1  LA    R2,1(R2)                                                 02310000
GETREC1  GET   DCB                                                      02320000
         LA    R3,1(R3)                                                 02330000
         B     GETREC1                                                  02340000
EOD      CLOSE (DCB)                                                    02350000
         CVD   R3,DWORK                                                 02360000
         MVC   MSG11C1,=X'40202120'                                     02370000
         ED    MSG11C1,DWORK+6                                          02380000
         CVD   R2,DWORK                                                 02390000
         MVC   MSG11C2,=X'40202120'                                     02400000
         ED    MSG11C2,DWORK+6                                          02410000
         TPUT  MSG11,MSG11L  NO. OF DIR BLKS ALLOC AND USED             02420000
         DROP  R9                                                       02430000
         USING DAPB18,R9                                                02440000
**                                                                      02450000
**  FREE ALLOCATED DATA SET                                             02460000
**                                                                      02470000
         MVC   DA18CD,=X'0018'                                          02480000
         XC    DA18FLG(10),DA18FLG                                      02490000
         MVC   DA18DDN,DCBDDNAM                                         02500000
         MVC   DA18MNM,=CL8' '                                          02510000
         MVC   DA18SCLS,=CL2' '                                         02520000
         MVI   DA18DPS2,X'08'  KEEP                                     02530000
         MVI   DA18CTL,X'10'  UNALLOC                                   02540000
         MVC   DA18JBNM,=CL8' '                                         02550000
         XC    ECB,ECB                                                  02560000
         LA    R1,DAIRPL                                                02570000
         LINK  EP=IKJEFD00  LIANK TO DAIR                               02580000
RETURN   DC    0H'0'                                                    02590000
         IKJRLSA ANSWER                                                 02600000
RETURN1  DC    0H'0'                                                    02610000
         L     R13,SAVE+4                                               02620000
         RETURN (14,12),T,RC=0                                          02630000
**                                                                      02640000
MVCDSN   MVC   DAIRDSN(0),0(R2)                                         02650000
**                                                                      02660000
**                                                                      02670000
** SAVE, CONTROL BLOCKS, ETC.                                           02680000
**                                                                      02690000
DWORK    DC    D'0'                                                     02700000
SAVE     DC    18F'0'                                                   02710000
MYPPL    DC    7F'0'                                                    02720000
DAIRPB   DC    20F'0'                                                   02730000
ECB      DC    F'0'                                                     02740000
ANSWER   DC    F'0'                                                     02750000
DAIRPL   DC    5F'0'                                                    02760000
DAIRDSNL DC    H'0'                                                     02770000
DAIRDSN  DC    CL44' '                                                  02780000
XLATE    DC    C'0123456789ABCDEF'                                      02790000
**                                                                      02800000
**  MESSAGES                                                            02810000
**                                                                      02820000
PARSERR  DC    C'PARSE ERROR'                                           02830000
MSG01    DC    C'DAIR RETURN CODE '                                     02840000
MSG01C   DC    CL2' '                                                   02850000
MSG01L   EQU   *-MSG01                                                  02860000
MSG02    DC    C'UNABLE TO ALLOCATE, MAXIMUM NUMBER OF ALLOCATIONS EXCEX02870000
               EDED'                                                    02880000
MSG03    DC    C'DATA SET NAME NOT FOUND IN CATALOG'                    02890000
MSG04    DC    C'LOCATE ERROR CODE '                                    02900000
MSG04C   DC    CL2' '                                                   02910000
MSG04L   EQU   *-MSG04                                                  02920000
MSG05    DC    C'OBTAIN ERROR CODE '                                    02930000
MSG05C   DC    CL2' '                                                   02940000
MSG05L   EQU   *-MSG05                                                  02950000
MSG06    DC    C'DADSM ERROR CODE '                                     02960000
MSG06C   DC    CL2' '                                                   02970000
MSG06L   EQU   *-MSG06                                                  02980000
MSG07    DC    C'DATA SET IN USE, TRY LATER'                            02990000
MSG08    DC    C'DYNAMIC ALLOCATION ERROR CODE '                        03000000
MSG08C   DC    CL4' '                                                   03010000
MSG08L   EQU   *-MSG08                                                  03020000
MSG09    DC    C'DATA SET IS NOT PARTITIONED'                           03030000
MSG10    DC    C'DATA SET OPEN ERROR'                                   03040000
MSG11    DC    0C' '                                                    03050000
MSG11C1  DC    CL4' '                                                   03060000
         DC    C' DIRECTORY BLOCK(S) ALLOCATED, '                       03070000
MSG11C2  DC    CL4' '                                                   03080000
         DC    C' DIRECTORY BLOCK(S) USED'                              03090000
MSG11L   EQU   *-MSG11                                                  03100000
         PRINT NOGEN                                                    03110000
**                                                                      03120000
**  DCB                                                                 03130000
**                                                                      03140000
DCB      DCB   DSORG=PS,MACRF=GL,DDNAME=WILLFILL,EODAD=EOD,            X03150000
               RECFM=U,LRECL=256,BLKSIZE=256                            03160000
**                                                                      03170000
**  PARSE CONTROL LIST                                                  03180000
**                                                                      03190000
PARSECL  IKJPARM DSECT=PARSEDL                                          03200000
PARSDSN  IKJPOSIT DSNAME,PROMPT='DATA SET NAME'                         03210000
         IKJENDP                                                        03220000
**                                                                      03230000
**  MAPPING DSECTS                                                      03240000
**                                                                      03250000
         DCBD                                                           03260000
         IKJDAP08                                                       03270000
         IKJDAP18                                                       03280000
         IKJCPPL                                                        03290000
         IKJPPL                                                         03300000
         IKJDAPL                                                        03310000
         END                                                            03320000
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR         <= TARGET LIBRARY    03330000
//LKED.SYSIN DD *                                                       03340000
 NAME PDSBLKS(R)                                                        03350000
//HELP    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)                        03360000
//SYSPRINT DD  SYSOUT=*                                                 03370000
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP              <= TARGET LIBRARY    03380000
//SYSIN    DD  *                                                        03390000
./ ADD NAME=PDSBLKS                                                     03400000
)F FUNCTION -                                                           03410000
  THE PDSBLKS COMMAND IS USED TO LIST THE NUMBER OF                     03420000
  PDS DIRECTORY BLOCKS ALLOCATED AND USED.                              03430000
)X SYNTAX -                                                             03440000
          PDSBLKS  'DATA SET NAME'                                      03450000
  REQUIRED - DATA SET NAME                                              03460000
  DEFAULTS - NONE                                                       03470000
  ALIAS    - NONE                                                       03480000
)O OPERANDS -                                                           03490000
  DATA SET NAME -                                                       03500000
              NAME OF THE PDS FOR WHICH THE NUMBER OF DIRECTORY BLOCKS  03510000
              IS TO BE LISTED.                                          03520000
./ ENDUP                                                                03530000
//                                                                      03540000
