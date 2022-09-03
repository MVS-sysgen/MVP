//ALIST  JOB (TSO),
//             'Install ALIST',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00030000
//* ***************************************************************** * 00040000
//* INSTALL ALIST COMMAND IN SYS2.CMDLIB (HELP IN SYS2.HELP)          * 00050000
//* ***************************************************************** * 00060000
//* SOURCE: XEPHON MVS UPDATE September 1994                            00070000
//*                                                                     00080000
//ALIST   EXEC ASMGCL,                                                  00000900
//             MAC1='SYS1.AMODGEN'                                      00001000
//ASM.SYSLIB DD                                                         00001100
//           DD                                                         00001200
//           DD DISP=SHR,DSN=SYS1.APVTMACS                              00001300
//ASM.SYSIN DD *                                                        00001400
         MACRO                                                          00001500
         YREGS                                                          00001600
         GBLA  &REGS                                                    00001700
&REGS    SETA  1                                                        00001800
         SPACE 1                                                        00001900
R0       EQU   0                                                        00002000
R1       EQU   1                                                        00002100
R2       EQU   2                                                        00002200
R3       EQU   3                                                        00002300
R4       EQU   4                                                        00002400
R5       EQU   5                                                        00002500
R6       EQU   6                                                        00002600
R7       EQU   7                                                        00002700
R8       EQU   8                                                        00002800
R9       EQU   9                                                        00002900
R10      EQU   10                                                       00003000
R11      EQU   11                                                       00003100
R12      EQU   12                                                       00003200
R13      EQU   13                                                       00003300
R14      EQU   14                                                       00003400
R15      EQU   15                                                       00003500
         SPACE 1                                                        00003600
         MEND                                                           00003700
ALIST    CSECT                    ESTABLISH CSECT                       00003800
         SAVE  (14,12),,ALIST-&SYSDATE                                  00003900
         YREGS                    PERFORM REGISTER EQUATES              00004000
         LR    R12,R15            LOAD R12 W/EPA ADDRESS                00004100
         USING ALIST,R12          ESTABLISH ADDRESSABLITY TO CSECT      00004200
         LA    R8,SAVEAREA        LOAD ADDR OF MY S/A                   00004300
         ST    R8,8(R13)          ST MY S/A ADDR IN CALLER'S S/A        00004400
         ST    R13,4(R8)          ST CALLER'S S/A ADDR IN MY S/A        00004500
         LR    R13,R8             LOAD ADDR OF MY S/A IN R13            00004600
         L     R1,CVTPTR          LOAD R1 W/A(CVT)                      00004700
         USING CVT,R1                                                   00004800
         L     R1,CVTTCBP         LOAD R1 W/A(TCBWORDS)                 00004900
         L     R1,4(R1)           LOAD R1 W/A(MY TCB)                   00005000
         USING TCB,R1                                                   00005100
         L     R1,TCBJSCB         LOAD R1 W/A(JSCB)                     00005200
         USING IEZJSCB,R1                                               00005300
         L     R1,JSCDSABQ        LOAD R1 W/A(QDB)                      00005400
         USING QDB,R1                                                   00005500
         L     R6,QDBFELMP        LOAD R6 W/A(DSAB Q)                   00005600
         USING DSAB,R6                                                  00005700
         SR    R2,R2              CLEAR R2 FOR IC                       00005800
LOOP     MVC   DSORG(L'DASHES),DASHES  INITIALIZE DSORG+FLAGS OUTPUT    00005900
         IC    R2,DSABORG1        COPY DSABORG1 TO R2 FOR COMPARE       00006000
         LA    R1,DORG1           LOAD A(DSABORG1 FLAG TABLE)           00006100
ORG1LOOP EX    R2,EXTM            COMPARE (TABLE 1 ENTRY <> DSABORG1)   00006200
         BNZ   ORG1HIT            BITS ON, PICK UP VALUE                00006300
         CLI   0(R1),X'FF'        END OF TABLE?                         00006400
         BE    ORG2BEG            YES, GO CHECK TABLE 2                 00006500
         LA    R1,DORG1LEN(,R1)    ELSE, BUMP TO NEXT TABLE ENTRY       00006600
         B     ORG1LOOP           LOOP TO CHECK NEXT ENTRY              00006700
ORG1HIT  MVC   DSORG(3),1(R1)     PUT VALUE IN OUTPUT LINE              00006800
         B     FLG1BEG            GO CHECK DSABFLG1                     00006900
ORG2BEG  IC    R2,DSABORG2        COPY DSABORG2 TO R2 FOR COMPARE       00007000
         LA    R1,DORG2           LOAD A(DSABORG2 FLAG TABLE)           00007100
ORG2LOOP EX    R2,EXTM            COMPARE (TABLE 2 ENTRY <> DSABORG2)   00007200
         BNZ   ORG2HIT            BITS ON, PICK UP VALUE                00007300
         CLI   0(R1),X'FF'        END OF TABLE?                         00007400
         BE    FLG1BEG            YES, GO CHECK DSABFLG1                00007500
         LA    R1,DORG2LEN(,R1)    ELSE, BUMP TO NEXT TABLE ENTRY       00007600
         B     ORG2LOOP           LOOP TO CHECK NEXT ENTRY              00007700
ORG2HIT  MVC   DSORG(3),1(R1)     PUT VALUE IN OUTPUT LINE              00007800
FLG1BEG  IC    R2,DSABFLG1        COPY DSABFLG1 TO R2 FOR COMPARE       00007900
         LA    R3,FLAGS           LOAD A(FLAGS OUTPUT AREA)             00008000
         LA    R1,DFLG1           LOAD A(DSABLFG1 FLAG TABLE)           00008100
FLG1LOOP CLI   0(R1),X'FF'        END OF TABLE?                         00008200
         BE    FLG3BEG            YES, GO CHECK DSABFLG3                00008300
         EX    R2,EXTM            COMPARE (TABLE 3 ENTRY <> DSABFLG1)   00008400
         BNZ   FLG1HIT            BITS ON, PICK UP VALUE                00008500
         LA    R1,DFLG1LEN(,R1)    ELSE, BUMP TO NEXT TABLE ENTRY       00008600
         LA    R3,1(,R3)          BUMP TO NEXT OUTPUT FLAG              00008700
         B     FLG1LOOP           LOOP TO CHECK NEXT ENTRY              00008800
FLG1HIT  MVC   0(1,R3),1(R1)      PUT VALUE IN OUTPUT LINE              00008900
         LA    R1,DFLG1LEN(,R1)   BUMP TO NEXT TABLE ENTRY              00009000
         LA    R3,1(,R3)          BUMP TO NEXT OUTPUT FLAG              00009100
         B     FLG1LOOP           LOOP TO CHECK NEXT ENTRY              00009200
FLG3BEG  IC    R2,DSABFLG3        LOAD A(DSABFLG3 FLAG TABLE)           00009300
         LA    R1,DFLG3           LOAD A(DSABFLG3 FLAG TABLE)           00009400
FLG3LOOP CLI   0(R1),X'FF'        END OF TABLE?                         00009500
         BE    FLAGDONE           YES, GO MOVE DDNAME                   00009600
         EX    R2,EXTM            COMPARE (TABLE 4 ENTRY <> DSABFLG3)   00009700
         BNZ   FLG3HIT            BITS ON, PICK UP VALUE                00009800
         LA    R1,DFLG3LEN(,R1)    ELSE, BUMP TO NEXT TABLE ENTRY       00009900
         LA    R3,1(,R3)          BUMP TO NEXT OUTPUT FLAG              00010000
         B     FLG3LOOP           LOOP TO CHECK NEXT ENTRY              00010100
FLG3HIT  MVC   0(1,R3),1(R1)      PUT VALUE IN OUTPUT LINE              00010200
         LA    R1,DFLG3LEN(,R1)   BUMP TO NEXT TABLE ENTRY              00010300
         LA    R3,1(,R3)          BUMP TO NEXT OUTPUT FLAG              00010400
         B     FLG3LOOP           LOOP TO CHECK NEXT ENTRY              00010500
FLAGDONE L     R7,DSABTIOT        LOAD R7 W/A(TIOT ENTRY)               00010600
         USING TIOENTRY,R7                                              00010700
MOVEDDN  MVC   DDNAME,TIOEDDNM    MOVE DDNAME                           00010800
         ICM   R8,7,TIOEJFCB      LOAD R8 W/A(JFCB)                     00010900
         LA    R8,16(,R8)         BUMP PAST PREFIX                      00011000
         USING JFCB,R8                                                  00011100
MOVEDSN  CLI   JFCBDSNM,X'04'     IS IT FORMAT4.DSCB                    00011200
         BNE   NORMDSN            NO, GO PROCESS NORMAL DSN             00011300
         MVC   DSN,FORMAT4K       ELSE, PUT SPECIAL NAME                00011400
         B     CKDISP             GO PROCESS DISP                       00011500
NORMDSN  MVC   DSN,JFCBDSNM       MOVE NORMAL DSN                       00011600
         TM    TIOELINK,TIOESYOT  IS IT A SYSOUT                        00011700
         BZ    CKTERM             NO, GO CHECK FOR TERMINAL/SYSIN       00011800
         LA    R1,DSN             LOAD A(DSN)                           00011900
         LA    R2,L'DSN           LOAD LENGTH(DSN)                      00012000
ALP      CLI   0(R1),C' '         FIND END OF DSN                       00012100
         BE    MVSYSCL            FOUND, GO MOVE SYSOUT ID              00012200
         LA    R1,1(,R1)          ELSE, BUMP TO NEXT BYTE               00012300
         BCT   R2,ALP             LOOP TO CHECK NEXT BYTE               00012400
MVSYSCL  MVC   0(SYSOUTLN,R1),SYSOUT MOVE SYSOUT ID                     00012500
         ICM   R15,7,DSABSSVA     LOAD A(SIOT PREFIX)                   00012600
         LA    R15,16(,R15)       LOAD A(SIOT)                          00012700
         USING SIOT,R15                                                 00012800
         MVC   8(1,R1),SCTOUTPN   MOVE SYSOUT CLASS                     00012900
         MVC   VOL,SYSOUTK        MOVE SYSOUT ID TO VOL                 00013000
         B     CKDISP             GO CHECK DISP                         00013100
CKTERM   TM    TIOELINK,TIOTTERM  IS IT A TERMINAL                      00013200
         BZ    CKPDS              NO, GO CHECK FOR PDS                  00013300
         MVC   DSN,BLANKS         ELSE, CLEAR OUT RESIDUAL JFCBDSNM     00013400
         MVC   DSN(L'TERMK),TERMK SHOW DSN AS A TERMINAL                00013500
         B     CKDISP             GO CHECK DISP                         00013600
CKPDS    TM    JFCBIND1,JFCPDS    IS IT A PDS                           00013700
         BZ    CKDISP             NO, GO CHECK DISP                     00013800
         LA    R1,DSN             LOAD A(DSN)                           00013900
         LA    R2,L'DSN           LOAD LENGTH(DSN)                      00014000
BLP      CLI   0(R1),C' '         FIND END OF DSN                       00014100
         BE    MVMEM              FOUND, GO MOVE MEMBER NAME            00014200
         LA    R1,1(,R1)          ELSE, BUMP TO NEXT BYTE               00014300
         BCT   R2,BLP             LOOP TO CHECK NEXT BYTE               00014400
MVMEM    MVI   0(R1),C'('         MOVE START PAREN                      00014500
         MVC   1(8,R1),JFCBELNM   MOVE MEMBER NAME                      00014600
         LA    R1,1(,R1)          LOAD A(MEMBER NAME)                   00014700
         LA    R2,L'JFCBELNM      LOAD LENGTH(MEMBER NAME)              00014800
CLP      CLI   0(R1),C' '         FIND END OF MEMBER NAME               00014900
         BE    MVPAREN            FOUND, GO MOVE END PAREN              00015000
         LA    R1,1(,R1)          ELSE, BUMP TO NEXT BYTE               00015100
         BCT   R2,CLP             LOOP TO CHECK NEXT BYTE               00015200
MVPAREN  MVI   0(R1),C')'         MOVE END PAREN                        00015300
CKDISP   LA    R1,JIND2           LOAD A(JFCBIND2 FLAG TABLE)           00015400
         IC    R2,JFCBIND2        GET JFCBIND2 BYTE                     00015500
IND2L1   CLC   JIND2END(JIND2LEN),0(R1) END OF TABLE                    00015600
         BE    CKVOL              YES, GO CHECK VOL                     00015700
         EX    R2,EXTM            ELSE, EXECUTE TM                      00015800
         BNZ   IND2L3             BITS ON, PICK UP VALUE                00015900
IND2L2   LA    R1,JIND2LEN(,R1)   ELSE, BUMP TO NEXT ENTRY              00016000
         B     IND2L1             GO CHECK ENTRY                        00016100
IND2L3   OC    DISP,1(R1)         PUT VALUE IN OUTPUT LINE              00016200
         TM    JFCBIND2,JFCTEMP   IS IT A TEMPORARY DATASET             00016300
         BZ    CKVOL              NO, GO CHECK VOL                      00016400
         MVC   VOL,TMPK           ELSE SHOW TEMP IN VOL                 00016500
CKVOL    CLC   VOL,BLANKS         WAS VOL ALREADY FILLED IN             00016600
         BNE   CKOPEN             YES, SKIP JFCB VOL INFO               00016700
         MVC   VOL,JFCBVOLS       ELSE, MOVE FIRST VOLSER TO VOL        00016800
CKOPEN   TM    DSABFLG2,DSABOPEN  IS DATASET OPEN                       00016900
         BZ    DODISP             NO, GO DISPLAY MSG                    00017000
         MVI   DISP+3,C'o'        ELSE, SHOW OPEN                       00017100
DODISP   TPUT  MSG,MSGLEN         DISPLAY MSG                           00017200
         MVC   MSG(MSGLEN),BLANKS CLEAR MSG                             00017300
         L     R6,DSABFCHN        LOAD A(NEXT DSAB), IF ANY             00017400
         LTR   R6,R6              WAS THERE ANOTHER                     00017500
         BNZ   LOOP               YES, GO PROCESS ENTRY                 00017600
END      L     R13,SAVEAREA+4     RELOAD R13 W/ADDR OF CALLER'S S/A     00017700
         RETURN (14,12),RC=0      RETURN TO OS WITH RETCODE=0           00017800
SAVEAREA DC    18F'0'             O/S SAVE AREA                         00017900
         LTORG                    GENERATE LITERAL POOL                 00018000
EXTM     TM    0(R1),0            EXECUTED INSTRUCTION                  00018100
MSG      DS    0CL(57)                                       IFOX00 JLM 00018200
*MSG      DS    0CL(MSGEND-DDNAME) ----------------------------         00018300
DDNAME   DC    CL8' ',C' '                                              00018400
DSORG    DC    CL3' ',C' '                                              00018500
FLAGS    DC    CL8' ',C' '                                              00018600
DISP     DC    CL3' ',C' '                                              00018700
VOL      DC    CL6' ',C' '                                              00018800
DSN      DC    CL44' ',C' '                                             00018900
MEM      DC    CL8' ',C' '                                              00019000
MSGEND   EQU   *                                                        00019100
MSGLEN   EQU   *-MSG -----------------------------------------          00019200
BLANKS   DC    CL(MSGLEN)' '                                            00019300
DASHES   DC    CL12'--- --------'                                       00019400
FORMAT4K DC    CL44'----------------FORMAT4.DSCB----------------'       00019500
SYSOUT   DC    C'(Sysout='                                              00019600
SYSOUTCL DC    C'*)'                                                    00019700
SYSOUTLN EQU   *-SYSOUT                                                 00019800
TERMK    DC    C'**Terminal**'                                          00019900
SYSOUTK  DC    C'Sysout'                                                00020000
TMPK     DC    C'*Temp*'                                                00020100
******                                                                  00020200
DORG1    DC    AL1(DSABIS),C'IS '       ISAM                            00020300
         DC    AL1(DSABPS),C'PS '       SEQUENTIAL                      00020400
         DC    AL1(DSABDA),C'DA '       DIRECT ACCESS                   00020500
         DC    AL1(DSABCX),C'CX '       COMMUNICATION LINE GROUP        00020600
         DC    AL1(DSABCQ),C'CQ '       DIRECT ACCESS MESSAGE QUEUE     00020700
         DC    AL1(DSABMQ),C'MQ '       PROBLEM PROGRAM MESSAGE QUEUE   00020800
         DC    AL1(DSABPO),C'PO '       PARTITIONED                     00020900
         DC    AL01(DSABU),C'  U'       UNMOVEABLE                      00021000
DORG1END DC    AL0001(255),C'---'       END-OF-TABLE                    00021100
DORG1LEN EQU   *-DORG1END                                               00021200
******                                                                  00021300
DORG2    DC    AL1(DSABGS),C'GS '       GRAPHICS                        00021400
         DC    AL1(DSABTX),C'TX '       TCAM LINE GROUP                 00021500
         DC    AL1(DSABTQ),C'TQ '       TCAM MESSAGE QUEUE              00021600
         DC    AL1(DSABAM),C'AM '       VSAM                            00021700
         DC    AL1(DSABTR),C'TR '       TCAM 3705                       00021800
DORG2END DC    AL0001(255),C'---'       END-OF-TABLE                    00021900
DORG2LEN EQU   *-DORG2END                                               00022000
******                                                                  00022100
DFLG1    DC    AL1(DSABDALC),C'D'        DYNAMICALLY ALLOCATED          00022200
         DC    AL1(DSABPALC),C'P'        PERMANENTLY ALLOCATED          00022300
         DC    AL1(DSABDCNV),C'D'        DYNAMICALLY CONVERTED          00022400
         DC    AL1(DSABCONV),C'C'        CONVERTIBLE                    00022500
         DC    AL1(DSABNUSE),C'I'        IN USE                         00022600
DFLG1END DC    AL000001(255),C'-'        END-OF-TABLE                   00022700
DFLG1LEN EQU   *-DFLG1END                                               00022800
******                                                                  00022900
DFLG3    DC    AL01(DSABVAM),C'V'        VIO DATASET                    00023000
         DC    AL1(DSABCATL),C'C'        DATASET IS A CATALOG           00023100
         DC    AL1(DSABJSCT),C'J'        JOBCAT/STEPCAT DATASET         00023200
DFLG3END DC    AL000001(255),C'-'        END-OF-TABLE                   00023300
DFLG3LEN EQU   *-DFLG3END                                               00023400
******                                                                  00023500
JIND2    DC    AL1(JFCSHARE),C'SHR'          SHARE                      00023600
         DC    AL001(JFCOLD),C'OLD'          OLD                        00023700
         DC    AL001(JFCMOD),C'MOD'          MOD                        00023800
         DC    AL001(JFCNEW),C'NEW'          NEW                        00023900
         DC    AL01(JFCTEMP),C'TMP'          TEMPORARY                  00024000
JIND2END DC    AL000001(255),C'---'          END-OF-TABLE               00024100
JIND2LEN EQU   *-JIND2END                                               00024200
         CVT   DSECT=YES                                                00024300
         IKJTCB                                                         00024400
         IEZJSCB                                                        00024500
         IHADSAB                                                        00024600
         IEFJESCT                                                       00024700
TIOT     DSECT                                                          00024800
         IEFTIOT1                                                       00024900
SIOT     DSECT                                                          00025000
         IEFASIOT                                                       00025100
JFCB     DSECT                                                          00025200
         IEFJFCBN                                                       00025300
         IHAQDB                                                         00025400
         DSECT                                                          00025500
         END                                                            00025600
/*                                                                      00025700
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR                   <== TARGET 02520000
//LKED.SYSIN DD *                                                       02530000
  NAME ALIST(R)                                                         02540000
/*                                                                      02550000
//HELP    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)                        02560000
//SYSPRINT DD  SYSOUT=*                                                 02570000
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP                        <== TARGET 02580000
//SYSIN    DD  *                                                        02590000
./ ADD NAME=ALIST                                                       02600000
./ NUMBER NEW1=10,INCR=10                                               02610000
)F ALIST FUNCTIONS -                                                    02620000
          THE ALIST COMMAND WILL LIST ALL ALLOCATED FILES FOR           02630000
          YOUR TSO SESSION IN THE FOLLOWING FORMAT:                     02640000
                                                                        02650000
          ISPPROF  PO  -------- SHRxVOLSER TSOUSER.ISPFPDF.PROFILE      02660000
                       ||||||||                                         02670000
                       |||||||*-JOBCAT/STEPCAT dataset (J)              02680000
                       ||||||*--dataset is a catalog   (C)              02690000
                       |||||*---VIO dataset            (V)              02700000
                       ||||*----dataset in-use         (I)              02710000
                       |||*-----convertible            (C)              02720000
                       ||*------dynamically converted  (D)              02730000
                       |*-------permanently allocated  (P)              02740000
                       *--------dynamically allocated  (D)              02750000
                                                                        02760000
          FIELDS SHOWN ARE DDNAME, DSORG, DSAB FLAG FIELDS,             02770000
          ALLOCATION TYPE, VOLUME SERIAL NUMBER, AND DATASET NAME.      02780000
                                                                        02790000
)X SYNTAX -                                                             02800000
          ALIST                                                         02810000
)O OPERANDS -                                                           02820000
          ALIST COMMAND TAKES NO OPERANDS                               02830000
./ ENDUP                                                                02840000
//                                                                      02850000
