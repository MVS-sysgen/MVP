//LISTDD  JOB (TSO),
//             'Install LISTDD',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//ASMFCL   EXEC ASMFCL,MAC1='SYS1.AMODGEN'                              00020003
//ASM.SYSIN DD *                                                        00030000
LISTDD   CSECT ,                                                        00040000
         USING LISTDD,R15          TEMPORARY ADDRESSABILITY             00050000
BEGIN    SAVE  (14,12),T,'LISTDD    &SYSDATE &SYSTIME'                  00060000
         LA    R12,SAVEAREA        LOAD ADDRESS OF SAVE AREA            00070000
         ST    R13,4(,R12) .       PROVIDE BACKWARD                     00080000
         ST    R12,8(,R13) ..       & FORWARD CHAINING                  00090000
         LR    R13,R12             COPY NEW SAVE AREA ADDRESS           00100000
         LR    R12,R15             COPY BASE ADDRESS                    00110000
         DROP  R15 .               DROP TEMPORARY ADDRESSABILITY        00120000
         USING LISTDD,R12          PERMANENT ADDRESSABILITY FOR PROGRAM 00130000
         LR    R3,R1               COPY PARM/CPPL ADDRESS               00140000
         TM    0(R3),X'80'         IS IT A PARM OR A CPPL ADDRESS       00150000
         BZ    GOTCPPL             BRANCH IF R3 ---> CPPL               00160000
         L     R4,0(,R3)           GET ADDRESS OF PARM FIELD            00170000
         LH    R6,0(,R4)           GET LENGTH OF PARM                   00180000
         SH    R6,=H'1'            WAS A PARM SUPPLIED                  00190000
         BM    EXIT                NO, EXIT WITH R/C = 8                00200000
         LA    R4,2(,R4)           ELSE POINT TO START OF PARM DATA     00210000
         B     GOTPARM             AND CONTINUE PROCESSING              00220000
GOTCPPL  L     R4,0(,R3)           GET POINTER TO COMMAND BUFFER        00230000
         LH    R5,2(,R4)           GET OFFSET TO COMMAND VARIABLE(S)    00240000
         LTR   R5,R5               ARE THERE ANY VARIABLES              00250000
         BZ    EXIT                NO, EXIT WITH R/C = 8                00260000
         LH    R6,0(,R4)           GET LENGTH OF COMMAND BUFFER         00270000
         SR    R6,R5               SUBTRACT VARIABLE OFFSET FROM LGH.   00280000
         SH    R6,=H'5'            SUBTRACT CONSTANT FROM LENGTH        00290000
         BM    EXIT                EXIT WITH R/C = 8 IF NO VARIABLES    00300000
         LA    R4,4(R4,R5)         POINT TO START OF VARIABLES          00310000
GOTPARM  CH    R6,=AL2(7)          DOES INPUT EXCEED MAXIMUM            00320000
         BNH   *+8                 NO, CONTINUE                         00330000
         LA    R6,7                ELSE SET LENGTH TO MAX               00340000
         MVC   DDNAME(0),0(R4)     *** EXECUTED BY NEXT INSTR ***       00350000
         EX    R6,*-6              COPY THE DDNAME                      00360000
         OC    DDNAME,=CL8' '      MAKE SURE DDNAME IS ALL UPPER-CASE   00370000
*** DETERMINE IF THE DATASET IS ALLOCATED BY SCANNING THE TIOT ***      00380000
         L     R2,16               POINT TO CVT                         00390000
         L     R2,0(,R2)           POINT TO TCB/ASCB WORDS              00400000
         L     R2,4(,R2)           POINT TO TCB                         00410000
         L     R2,12(,R2)          POINT TO TIOT                        00420000
         LA    R1,TIOENTRY-TIOT1   OFFSET TO FIRST TIOT ENTRY           00430000
TIOSCAN  AR    R2,R1               POINT TO FIRST/NEXT ENTRY            00440000
         USING TIOENTRY,R2         PROVIDE TIOT ENTRY ADDRESSABILITY    00450000
         TM    TIOESTTA,TIOSLTYP   IS THIS ENTRY IN USE                 00460000
         BO    TIOBYPAS            NO                                   00470000
         CLI   DDNAME,C'*'         IS REQUEST FOR FIRST ALL DATASETS    00480000
         BE    ALLOCATD            YES, EXIT NOW                        00490000
         CLC   TIOEDDNM,DDNAME     IS THIS ENTRY FOR REQUESTED DDNAME   00500000
         BE    ALLOCATD            YES, EXIT NOW                        00510000
TIOBYPAS ICM   R1,1,TIOELNGH       GET LENGTH OF THIS TIOT ENTRY        00520000
         BNZ   TIOSCAN             CONTINUE IF ANOTHER ENTRY AVAILABLE  00530000
         B     NOTFOUND            BRANCH IF DDNAME NOT FOUND           00540000
ALLOCATD MVI   RETCODE,0           SET FINAL RETURN CODE TO ZERO        00550000
         TPUT  CLRSCRN,CLRSCRNL,FULLSCR CLEAR THE SCREEN FOR STARTERS   00560000
         TPUT  MSGHDR,L'MSGHDR     OUTPUT THE MESSAGE HEADER LINE       00570000
         TPUT  BLANKS,L'BLANKS     OUTPUT A BLANK LINE                  00580000
LOOP1    MVC   MSGDDN,TIOEDDNM     COPY THE DDNAME TO OUTPUT            00590000
         SR    R1,R1               CLEAR A WORK REGISTER                00600000
         ICM   R1,7,TIOEJFCB       GET THE JFCB ADDRESS FOR DATASET     00610000
         LA    R1,16(,R1)          INCREMENT PAST JFCB HEADER           00620000
         USING JFCBDSCT,R1         PROVIDE JFCB ADDRESSABILITY          00630000
         MVC   MSGDSN,JFCBDSNM     COPY THE DSNAME TO OUTPUT            00640000
         MVC   WORKAREA(1),JFCBIND2 AND THE JFCB INDICATOR BYTE         00650000
         NI    WORKAREA,JFCDISP+JFCSHARE TURN OFF UNINTERESTING BITS    00660000
         LA    R14,DISP            POINT TO DISP-1 TABLE                00670000
         LA    R0,DISP#             AND SET NUMBER OF TABLE ENTRIES     00680000
LOOP2    CLC   WORKAREA(1),0(R14)  LOOK FOR DISP-1 MATCH IN TABLE       00690000
         BE    EXIT2               EXIT IF/WHEN FOUND                   00700000
         LA    R14,L'DISP(,R14)    ELSE POINT TO NEXT ENTRY             00710000
         BCT   R0,LOOP2            AND CONTINUE TRYING                  00720000
EXIT2    MVC   MSGDISP1,1(R14)     COPY THE DISPOSITION TO OUTPUT       00730000
         MVC   MSGDISP2,=C'KEEP  ' ASSUME FINAL DISP=KEEP               00740000
         TM    JFCBIND2,JFCTEMP    IS FINAL DISPOSITION KEEP/DELETE     00750000
         BZ    *+10                BRANCH IF 'KEEP'                     00760000
         MVC   MSGDISP2,=C'DELETE' ELSE RESET TO SHOW 'DELETE'          00770000
         MVC   MSGVOL,JFCBVOLS     COPY THE FIRST VOLSER TO OUTPUT      00780000
         DROP  R1                  DROP JFCB ADDRESSABILITY             00790000
         TPUT  MESSAGE,L'MESSAGE   OUTPUT A MESSAGE LINE                00800000
         SR    R1,R1               CLEAR A WORK REGISTER                00810000
LOOP3    IC    R1,TIOELNGH         GET LENGTH OF THIS TIOT ENTRY        00820000
         LA    R2,TIOENTRY(R1)     POINT TO NEXT TIOT ENTRY             00830000
         CLI   TIOELNGH,0          HAVE WE HIT END OF TIOT              00840000
         BE    EXIT                ALL DONE IF LENGTH IS ZERO           00850000
         TM    TIOESTTA,TIOSLTYP   IS THIS ENTRY IN USE                 00860000
         BZ    CHKALL              NO, SEE IF LISTDD ALL OR CONCAT.     00870000
         CLI   DDNAME,C'*'         IS LISTDD ALL IN EFFECT              00880000
         BNE   EXIT                NO, WE'RE ALL DONE                   00890000
         B     LOOP3               ELSE TRY THE NEXT TIOT ENTRY         00900000
CHKALL   CLI   DDNAME,C'*'         IS LISTDD ALL IN EFFECT              00910000
         BE    LOOP1               YES, LIST THIS ENTRY                 00920000
         CLC   =CL8' ',TIOEDDNM    IS THIS ENTRY CONCAT FOR PREVIOUS    00930000
         BNE   EXIT                NO,WE'RE ALL DONE                    00940000
         B     LOOP1               ELSE PRINT THE CONCATENATION INFO    00950000
NOTFOUND TPUT  ERRMSG,L'ERRMSG                                          00960000
         MVI   RETCODE,4           RESET FINAL RETURN CODE TO 4         00970000
EXIT     LA    R15,8               LOAD THE FINAL RETURN CODE           00980000
RETCODE  EQU   EXIT+3                                                   00990000
         L     R13,4(,R13)    .    POINT TO PREVIOUS SAVE AREA          01000000
         RETURN (14,12),T,RC=(15)  TERMINATE THE TASK                   01010000
         LTORG ,                                                        01020000
SAVEAREA DC    9D'0'                                                    01030000
WORKAREA DC    C' '                                                     01040000
DISP     DC    0CL4' '                                                  01050000
         DC    AL1(JFCNEW),C'NEW'                                       01060000
         DC    AL1(JFCMOD),C'MOD'                                       01070000
         DC    AL1(JFCOLD),C'OLD'                                       01080000
         DC    AL1(JFCOLD+JFCSHARE),C'SHR'                              01090000
DISP#    EQU   (*-DISP)/L'DISP                                          01100000
         DC    C'????'                                                  01110000
ERRMSG   DC    C'XXXXXXXX - DATASET NOT ALLOCATED'                      01120000
         ORG   ERRMSG                                                   01130000
DDNAME   DC    CL8' '                                                   01140000
         ORG   ,                                                        01150000
CLRSCRN  DC    X'C31140403C404000'                                      01160000
CLRSCRNL EQU   *-CLRSCRN                                                01170000
BLANKS   DC    C' '                                                     01180000
*                123456789012345678901234567890123456789012345678901234 01190000
MSGHDR   DC    C'_DDNAME_  ___DISP___  VOLSER  ___________________DSNAMX01200000
               E___________________'                                    01210000
*              5678901234567890123456789                                01220000
MESSAGE  DC    CL79' '                                                  01230000
         ORG   MESSAGE                                                  01240000
MSGDDN   DC    C'_DDNAME_',C'  '                                        01250000
MSGDISP1 DC    C'XXX',C','                                              01260000
MSGDISP2 DC    C'YYYYYY',C'  '                                          01270000
MSGVOL   DC    C'VOLSER',C'  '                                          01280000
MSGDSN   DC    CL44' '                                                  01290000
         ORG   ,                                                        01300000
JFCBDSCT DSECT ,                                                        01310000
         IEFJFCBN LIST=NO                                               01320000
TIOTDSCT DSECT ,                                                        01330000
         IEFTIOT1                                                       01340000
         SPACE 1                                                        01350000
R0       EQU   0                                                        01360000
R1       EQU   1                                                        01370000
R2       EQU   2                                                        01380000
R3       EQU   3                                                        01390000
R4       EQU   4                                                        01400000
R5       EQU   5                                                        01410000
R6       EQU   6                                                        01420000
R7       EQU   7                                                        01430000
R8       EQU   8                                                        01440000
R9       EQU   9                                                        01450000
R10      EQU   10                                                       01460000
R11      EQU   11                                                       01470000
R12      EQU   12                                                       01480000
R13      EQU   13                                                       01490000
R14      EQU   14                                                       01500000
R15      EQU   15                                                       01510000
         SPACE 1                                                        01520000
         END   BEGIN                                                    01530000
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR                              01540000
//LKED.SYSIN DD *                                                       01550000
  NAME LISTDD(R)                                                        01560000
//*                                                                     01570003
//HELP    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)                        01580003
//SYSPRINT DD  SYSOUT=*                                                 01590003
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP                        <== TARGET 01600003
//SYSIN    DD  *                                                        01610003
./ ADD NAME=ALIST                                                       01620003
./ NUMBER NEW1=10,INCR=10                                               01630003
)F LISTDD FUNCTIONS -                                                   01640003
          THE LISTDD COMMAND WILL LIST THE ATTRIBUTES FOR THE           01650003
          DATASET WHOSE DDNAME IS SPECIFIED IN THE FORMAT:              01660003
                                                                        01670003
          _DDNAME_  ___DISP___  VOLSER  _______DSNAME_______            01680003
          SYSPROC   SHR,KEEP    PUB000  HMVS01.CLIST                    01690003
                    SHR,KEEP    MVS000  SYS1.CMDPROC                    01700003
                                                                        01710003
          FIELDS SHOWN ARE DDNAME, DISPOSITION, VOLSER, AND DSNAME.     01720003
                                                                        01730003
)X SYNTAX -                                                             01740003
          LISTDD <DDNAME>                                               01750003
)O OPERANDS -                                                           01760003
          <DDNAME>   *                                                  01770003
                                                                        01780003
          <DDNAME> INFORMATION WILL BE DISPLAYED FOR THE SPECIFIC       01790003
                   DDNAME ENTERED                                       01800003
                                                                        01810003
          *        INFORMATION WILL BE DISPLAYED FOR ALL ALLOCATED      01820003
                   DDNAMES                                              01830003
//                                                                      01840003
