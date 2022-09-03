//CONSAMS  JOB (TSO),
//             'Install CONSAMS',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//CONSAMS EXEC ASMFCL,PARM.ASM='LIST,LOAD,NODECK',                      00000200
//             MAC1='SYS1.AMODGEN'                                      00000300
//ASM.SYSIN DD *                                                        00000400
         PRINT NOGEN                                                    00000500
         MACRO                                                          00000600
&LABEL   QEDIT &CIBCTR=,&BLOCK=,&ORIGIN=                                00000700
         LCLC  &OP,&REG,&SIGN      PARAMETERS                           00000800
         LCLA  &LENGTH             K'&OP                                00000900
         LCLC  &STRIP              '&OP'(2,&LENGTH-2)                   00001000
.*                                                                      00001100
.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*. 00001200
.*                                                                      00001300
.*             QEDIT               CIB CHAIN MANIPULATOR                00001400
.*                                                                      00001500
.*       THE QEDIT MACRO GENERATES BOTH THE REQUIRED ENTRY PARAMETERS   00001600
.*       AND THE LINKAGE TO SVC 34 FOR THE FOLLOWING THREE FUNCTIONS    00001700
.*                                                                      00001800
.*       (1)   ADDING A CIB TO THE CHAIN FOR A TASK                     00001900
.*       (2)   DECHAINING AND FREEING A CIB                             00002000
.*       (3)   SETTING A LIMIT ON THE NUMBER OF CIB'S                   00002100
.*             TO BE SIMULTANEOUSLY CHAINED FOR A TASK                  00002200
.*                                                                      00002300
.*             THE MACRO FORMAT IS                                      00002400
.*                                                                      00002500
.*       SYMBOL   QEDIT   ORIGIN=ADDRESS/(0),                           00002600
.*                        BLOCK=ADDRESS/(1),CIBCTR=NUMBER/(1)           00002700
.*             WHERE                                                    00002800
.*       SYMBOL                                                         00002900
.*             IS ANY SYMBOL VALID IN THE ASSEMBLER LANGUAGE            00003000
.*       ADDRESS                                                        00003100
.*             IS ANY ADDRESS VALID IN AN RX INSTRUCTION                00003200
.*             OR ONE OF THE GENERAL REGISTERS 2 THROUGH 12             00003300
.*             PREVIOUSLY LOADED WITH THE INDICATED ADDRESS             00003400
.*             THE REGISTER MUST BE DESIGNATED BY A NUMBER              00003500
.*             OR SYMBOL CODED WITHIN PARENTHESES                       00003600
.*       NUMBER                                                         00003700
.*             IS ANY ADDRESS VALID IN AN RX INSTRUCTION                00003800
.*             OR ONE OF THE GENERAL REGISTERS 2 THROUGH 12             00003900
.*             PREVIOUSLY LOADED WITH THE INDICATED ADDRESS             00004000
.*             THE REGISTER MUST BE DESIGNATED BY A NUMBER              00004100
.*             OR SYMBOL CODED WITHIN PARENTHESES                       00004200
.*             AND MUST NOT CONTAIN ZERO                                00004300
.*       BLOCK                                                          00004400
.*             SPECIFIES THE ADDRESS OF THE CIB                         00004500
.*             WHICH IS TO BE OPERATED UPON                             00004600
.*       ORIGIN                                                         00004700
.*             SPECIFIES THE ADDRESS OF THE POINTER TO THE FIRST CIB    00004800
.*             THIS PARAMETER MUST SPECIFY THE ADDRESS                  00004900
.*             OF THE THIRD WORD OF THE START PARAMETER LIST            00005000
.*             OTHERWISE THE OPERATION WILL BE IGNORED                  00005100
.*             AND A RETURN CODE OF FOUR RETURNED                       00005200
.*       CIBCTR                                                         00005300
.*             SPECIFIES A NONNEGATIVE INTEGER TO BE USED               00005400
.*             AS A LIMIT ON THE NUMBER OF CIB'S                        00005500
.*             TO BE CHAINED AT ANY ONE TIME FOR A TASK                 00005600
.*                                                                      00005700
.*       THE FOLLOWING TABLE INDICATES THE VALID OPERAND COMBINATIONS   00005800
.*       OF THE MACRO ACCORDING TO THE FUNCTION BEING INVOKED           00005900
.*                                                                      00006000
.*             FUNCTION            OPERANDS                             00006100
.*                                                                      00006200
.*             ADD A CIB           ORIGIN BLOCK                         00006300
.*             FREE A CIB          ORIGIN BLOCK                         00006400
.*             FREE ALL CIB'S      ORIGIN                               00006500
.*             SET CIB LIMIT       ORIGIN CIBCTR                        00006600
.*                                                                      00006700
.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*. 00006800
.*                                                                      00006900
.*             THE QEDIT MACRO WILL ISSUE SVC 34                        00007000
.*       TO ADD OR FREE AN ELEMENT OF A CIB QUEUE                       00007100
.*             WITH (0) = +ORIGIN AND (1) = -BLOCK                      00007200
.*       TO FREE ALL THE ELEMENTS OF A CIB QUEUE                        00007300
.*             WITH (0) = +ORIGIN AND (1) = ZERO                        00007400
.*       OR TO SET THE CIB QUEUE LENGTH MAXIMUM IN A CSCB               00007500
.*             WITH (0) = -ORIGIN AND (1) = -CIBCTR                     00007600
.*       BUT (1) = ZERO IF CIBCTR = ZERO                                00007700
.*                                                                      00007800
         AIF   ('&LABEL' EQ '').L                                       00007900
&LABEL   DS    0H                                                       00008000
.L       ANOP                                                           00008100
.*                                 CHECK                                00008200
.*                                     KEYWORDS                         00008300
         AIF   ((T'&CIBCTR EQ 'O' OR T'&BLOCK EQ 'O')                  $00008400
               AND T'&ORIGIN NE 'O').Q00                                00008500
         MNOTE *,'ORIGIN MUST BE PRESENT AND CIBCTR OR BLOCK ABSENT'    00008600
         MEXIT                                                          00008700
.Q00     ANOP                                                           00008800
.*                                     REGISTERS                        00008900
         AIF   ('&ORIGIN' NE '(1)').Q10                                 00009000
         MNOTE *,'ORIGIN=(1) OR EQUIVALENT MUST NOT BE SPECIFIED'       00009100
         MEXIT                                                          00009200
.Q10     ANOP                                                           00009300
.*                                     OPERANDS                         00009400
         AIF   ('&CIBCTR' NE '&ORIGIN' AND '&BLOCK' NE '&ORIGIN').Q20   00009500
         MNOTE *,'OPERAND REFERENTS OBVIOUSLY MUST BE DISTINCT'         00009600
         MEXIT                                                          00009700
.Q20     ANOP                                                           00009800
.*                                 R1                                   00009900
.S       ANOP                                                           00010000
&REG     SETC  '1'                                                      00010100
         AIF   (T'&CIBCTR NE 'O').S10                                   00010200
         AIF   (T'&BLOCK NE 'O').S20                                    00010300
.*                                     ORIGIN                           00010400
&OP      SETC  '0'                                                      00010500
&SIGN    SETC  '+'                                                      00010600
         AGO   .X                                                       00010700
.*                                     CIBCTR                           00010800
.S10     ANOP                                                           00010900
         AIF   ('&CIBCTR' EQ '0').S15                                   00011000
&OP      SETC  '&CIBCTR'                                                00011100
&LENGTH  SETA  K'&CIBCTR                                                00011200
&SIGN    SETC  '-'                                                      00011300
         AGO   .X                                                       00011400
.*                                     DISABLE                          00011500
.S15     ANOP                                                           00011600
         SR    R1,R1               BAR CIB'S                            00011700
         AGO   .R                                                       00011800
.*                                     BLOCK                            00011900
.S20     ANOP                                                           00012000
&OP      SETC  '&BLOCK'                                                 00012100
&LENGTH  SETA  K'&BLOCK                                                 00012200
&SIGN    SETC  '-'                                                      00012300
         AGO   .X                                                       00012400
.*                                 R0                                   00012500
.R       ANOP                                                           00012600
&REG     SETC  '0'                                                      00012700
         AIF   (T'&CIBCTR NE 'O').R10                                   00012800
         AIF   (T'&BLOCK NE 'O').R20                                    00012900
.*                                     ORIGIN                           00013000
&OP      SETC  '&ORIGIN'                                                00013100
&LENGTH  SETA  K'&ORIGIN                                                00013200
&SIGN    SETC  '+'                                                      00013300
         AGO   .X                                                       00013400
.*                                     CIBCTR                           00013500
.R10     ANOP                                                           00013600
&OP      SETC  '&ORIGIN'                                                00013700
&LENGTH  SETA  K'&ORIGIN                                                00013800
&SIGN    SETC  '-'                                                      00013900
         AGO   .X                                                       00014000
.*                                     BLOCK                            00014100
.R20     ANOP                                                           00014200
&OP      SETC  '&ORIGIN'                                                00014300
&LENGTH  SETA  K'&ORIGIN                                                00014400
&SIGN    SETC  '+'                                                      00014500
         AGO   .X                                                       00014600
.*                                 LOAD                                 00014700
.X       ANOP                                                           00014800
         AIF   ('&OP'(1,1) EQ '(').X50                                  00014900
.*                                     SYMBOLIC                         00015000
         LA    &REG,&OP            LOAD PARAMETER                       00015100
         AIF   ('&SIGN' EQ '+').Y                                       00015200
         LNR   &REG,&REG           COMPLEMENT PARAMETER                 00015300
         AGO   .Y                                                       00015400
.*                                     REGISTER                         00015500
.X50     ANOP                                                           00015600
         AIF   ('&OP'(2,1) EQ '&REG').X70                               00015700
.*                                     DISTINCT                         00015800
&STRIP   SETC  '&OP'(2,&LENGTH-2)                                       00015900
         AIF   ('&SIGN' EQ '+').X60                                     00016000
         LNR   &REG,&STRIP         LOAD COMPLEMENTED PARAMETER          00016100
         AGO   .Y                                                       00016200
.X60     ANOP                                                           00016300
         LR    &REG,&STRIP         LOAD PARAMETER                       00016400
         AGO   .Y                                                       00016500
.*                                     IDENTICAL                        00016600
.X70     ANOP                                                           00016700
         AIF   ('&SIGN' EQ '+').Y                                       00016800
         LNR   &REG,&REG           COMPLEMENT PARAMETER                 00016900
         AGO   .Y                                                       00017000
.*                                 END                                  00017100
.Y       ANOP                                                           00017200
         AIF   ('&REG' EQ '1').R                                        00017300
         SVC   34                  CALL SUPERVISOR                      00017400
         MEND                                                           00017500
         MACRO                                                          00017600
         REGEQU                                                         00017700
.*-------------------------------------------------------------------*. 00017800
.*                                                                   *. 00017900
.*  REGEQU -- PROVIDE SYMBOLIC REGISTERS EQUATES                     *. 00018000
.*                                               RDM 10/21/68        *. 00018100
.*                                                                   *. 00018200
.*-------------------------------------------------------------------*. 00018300
R0       EQU   0                                                        00018400
R1       EQU   1                                                        00018500
R2       EQU   2                                                        00018600
R3       EQU   3                                                        00018700
R4       EQU   4                                                        00018800
R5       EQU   5                                                        00018900
R6       EQU   6                                                        00019000
R7       EQU   7                                                        00019100
R8       EQU   8                                                        00019200
R9       EQU   9                                                        00019300
R10      EQU   10                                                       00019400
R11      EQU   11                                                       00019500
R12      EQU   12                                                       00019600
R13      EQU   13                                                       00019700
R14      EQU   14                                                       00019800
R15      EQU   15                                                       00019900
         MEND                                                           00020000
CONSAMS  TITLE 'INVOKE ACCESS METHOD SERVICES FROM CONSOLE'             00020100
*********************************************************************** 00020200
*                                                                       00020300
*        CONSAMS PROVIDES AN OPERATOR CONSOLE INTERFACE TO              00020400
*        ACCESS METHOD SERVICES (IDCAMS).                               00020500
*                                                                       00020600
*        CONSAMS MUST BE AUTHORIZED IN ORDER TO EXECUTE CERTAIN         00020700
*        IDCAMS COMMANDS. THE MAJORITY OF COMMANDS DO NOT REQUIRE       00020800
*        AUTHORIZATION.                                                 00020900
*                                                                       00021000
*        CONSAMS IS INVOKED VIA AN OPERATOR START COMMAND TO A JCL      00021100
*        PROCEDURE IN SYS1.PROCLIB. THE FOLLOWING JCL IS SUGGESTED:     00021200
*              //AMS     PROC                                           00021300
*              //IEFPROC EXEC PGM=CONSAMS,PARM='MARGINS(1,120)'         00021400
*              //AMSDUMP  DD  SYSOUT=A    IDCAMS SNAP DUMP              00021500
*              //SYSOUT   DD  SYSOUT=A    OUTFILE OUTPUT                00021600
*              //SYSPRINT DD  SYSOUT=A    CONSOLE LOG                   00021700
*              //SYSUDUMP DD  SYSOUT=A                                  00021800
*        THE PARM FIELD ON THE EXEC STATEMENT IS PASSED TO IDCAMS,      00021900
*        WHICH EXECUTES IT AS THE FIRST COMMAND.                        00022000
*        ALL DD STATEMENTS ARE OPTIONAL.                                00022100
*        AMSDUMP MAY BE USEFUL FOR DEBUGGING IDCAMS.                    00022200
*        SYSPRINT IS WRITTEN BY CONSAMS, AND CONTAINS A HARD COPY       00022300
*        OF THE OUTPUT WRITTEN TO THE CONSOLE.                          00022400
*        SYSOUT MAY BE USED VIA THE OUTFILE PARAMETER OF SOME IDCAMS    00022500
*        COMMANDS FOR HARD COPY OUTPUT TOO VOLUMINOUS FOR A CONSOLE.    00022600
*                                                                       00022700
*        TO USE THE ABOVE PROCEDURE, ENTER THE OPERATOR COMMAND:        00022800
*              START AMS                                                00022900
*        CONSAMS WILL RESPOND WITH THE MESSAGE: AMS READY.              00023000
*        TO ENTER AN IDCAMS COMMAND, USE THE OPERATOR MODIFY COMMAND:   00023100
*              MODIFY AMS,IDCAMS-COMMAND                                00023200
*        CONSAMS WILL PASS THE IDCAMS-COMMAND TEXT TO IDCAMS AS A SYSIN 00023300
*        RECORD.                                                        00023400
*        IDCAMS WILL THEN RESPOND WITH SYSPRINT RECORDS WHICH CONSAMS   00023500
*        WILL WRITE VIA WTO TO THE CONSOLE WHICH ENTERED THE MODIFY     00023600
*        COMMAND. A ONE SECOND DELAY IS IMPOSED ON CONSECUTIVE WTO'S TO 00023700
*        PREVENT EXCESSIVE QUEUED CONSOLE OUTPUT.                       00023800
*        TO STOP CONSAMS WHEN IT IS WAITING FOR A MODIFY COMAND,        00023900
*        ENTER THE COMMAND:                                             00024000
*              STOP AMS                                                 00024100
*        TO CANCEL CONSAMS AT ANY TIME, ENTER THE COMMAND:              00024200
*              CANCEL AMS                                               00024300
*                                                                       00024400
*********************************************************************** 00024500
         EJECT ,                                                        00024600
CONSAMS  CSECT ,                                                        00024700
         EJECT ,                                                        00024800
         DCBD  ,                                                        00024900
IEZCIB   DSECT ,                                                        00025000
         IEZCIB ,                                                       00025100
         EJECT ,                                                        00025200
IEZCOM   DSECT ,                                                        00025300
         IEZCOM ,                                                       00025400
         EJECT ,                                                        00025500
         IEZWPL ,                                                       00025600
         EJECT ,                                                        00025700
         REGEQU ,                                                       00025800
         EJECT ,                                                        00025900
*********************************************************************** 00026000
*              WORK AREA                                                00026100
*              COMMUNICATION BETWEEN CONSAMS TASK, IDCAMS SUBTASK,      00026200
*              AND SYSIN/SYSPRINT EXIT ROUTINES.                        00026300
*********************************************************************** 00026400
WORKAREA DSECT ,                                                        00026500
SAVE     DS    18F                 MOTHER TASK SAVE AREA                00026600
RTNSAVE  DS    18F                 DAUGHTER TASK USER EXIT SAVE AREA    00026700
FIELDS   DS    0F                                                       00026800
COMM     DS    A                   COMM ECB/CIB ADDRESS                 00026900
ARGLIST  DS    0XL16               IDCAMS ARGUMENT LIST                 00027000
ARGOPT   DS    A                       OPTIONS                          00027100
ARGDDNAM DS    A                       DDNAMES                          00027200
ARGPAGE  DS    A                       PAGE NUMBER                      00027300
ARGIOLST DS    A                       IOLIST                           00027400
IOLIST   DS    0X                  IDCAMS INPUT/OUTPUT LIST             00027500
ION      DS    F                       N - NUMBER OF GROUPS OF FIELDS   00027600
IODDIN   DS    A                       DDNAME - SYSIN                   00027700
IORTNIN  DS    A                       IOROUTINE - SYSIN                00027800
IOUDIN   DS    A                       USER DATA - SYSIN                00027900
IODDPRT  DS    A                       DDNAME - SYSPRINT                00028000
IORTNPRT DS    A                       IOROUTINE - SYSPRINT             00028100
IOUDPRT  DS    A                       USER DATA - SYSPRINT             00028200
IOGROUPS EQU   ((*-ION)/4-1)/3         N - CONTENTS OF ION              00028300
ATTACH   ATTACH EP=IDCAMS,         ATTACH ACCESS METHOD SERVICES       *00028400
               ECB=ATTECB,                                             *00028500
               SZERO=YES,                                              *00028600
               SF=L                                                     00028700
ATTECB   DS    F                   IDCAMS SUBTASK TERMINATION ECB       00028800
         USING STIMEREX,R15                                             00028900
STIMEREX SAVE  (14,12)             ACTUAL STIMER EXIT                   00029000
         POST  TIMECB              POST SYSPRINT EXIT ROUTINE           00029100
         RETURN (14,12)                                                 00029200
         DROP  R15                                                      00029300
STIMERSI DS    0XL(*-STIMEREX)     SIZE OF STIMER EXIT                  00029400
TIMECB   DS    F                   SYSPRINT STIMER EXIT ECB             00029500
TCB      DS    A                   IDCAMS SUBTASK TCB ADDRESS           00029600
EXTRACT  EXTRACT FIELDS,           GET COMM ECB/CIB POINTERS           *00029700
               FIELDS=COMM,                                            *00029800
               MF=L                                                     00029900
WTO      DS    XL(L'WPLLGH+L'WPLMCSF),CL(L'WPLTXT)                      00030000
SYSPRINT DCB   BFALN=F,                                                *00030100
               BLKSIZE=129,                                            *00030200
               BUFNO=2,                                                *00030300
               DDNAME=SYSPRINT,                                        *00030400
               DSORG=PS,                                               *00030500
               LRECL=125,                                              *00030600
               MACRF=PL,                                               *00030700
               RECFM=VA                                                 00030800
PRTSIZE  DS    0XL(*-SYSPRINT)     SIZE OF SYSPRINT DCB                 00030900
DMLIST   OPEN  (SYSPRINT,OUTPUT),  OPEN/CLOSE PARM LIST                *00031000
               MF=L                                                     00031100
CONTROL  DS    B                   USER EXIT FLAGS                      00031200
SUPPRESS EQU   B'10000000'              SUPPRESS WTO                    00031300
WORKSIZE DS    0XL(*-WORKAREA)     SIZE OF WORK AREA                    00031400
         EJECT ,                                                        00031500
RTNARG   DSECT ,                   USER I/O ROUTINE ARGUMENT LIST       00031600
USERDATA DS    A                   USER DATA ADDRESS                    00031700
IOFLAGS  DS    A                   IOFLAGS ADDRESS                      00031800
IOINFO   DS    A                   IOINFO ADDRESS                       00031900
         SPACE 2                                                        00032000
FLAGS    DSECT ,                   IOFLAGS -> FLAGS                     00032100
         DS    BL4                                                      00032200
         SPACE 2                                                        00032300
GETPUT   DSECT ,                   IOINFO -> GET/PUT INFO               00032400
RECORD   DS    A                   RECORD ADDRESS                       00032500
RECORDLN DS    F                   RECORD LENGTH                        00032600
         EJECT ,                                                        00032700
*********************************************************************** 00032800
*              CONSAMS TASK                                             00032900
*              INITIALIZE, ATTACH IDCAMS, TERMINATE                     00033000
*********************************************************************** 00033100
CONSAMS  CSECT ,                                                        00033200
         USING CONSAMS,R15                                              00033300
         SAVE  (14,12)                                                  00033400
         MODID ,                                                        00033500
         LR    R12,R15                                                  00033600
         USING CONSAMS,R12                                              00033700
         DROP  R15                                                      00033800
         LR    R2,R1               SAVE PARM POINTER                    00033900
         GETMAIN RU,               GET WORK AREA                       *00034000
               LV=L'WORKSIZE,                                          *00034100
               BNDRY=PAGE                                               00034200
         LR    R4,R1               CLEAR WORK AREA                      00034300
         LA    R5,L'WORKSIZE                                            00034400
         SR    R6,R6                                                    00034500
         SR    R7,R7                                                    00034600
         MVCL  R4,R6                                                    00034700
         ST    R13,4(R1)           CHAIN SAVE AREAS                     00034800
         ST    R1,8(R13)                                                00034900
         LR    R13,R1                                                   00035000
         USING WORKAREA,R13                                             00035100
         EXTRACT FIELDS,           GET COMM ECB/CIB POINTERS           *00035200
               FIELDS=COMM,                                            *00035300
               MF=(E,EXTRACT)                                           00035400
         L     R3,COMM             GET ECB/CIB POINTERS                 00035500
         USING COMLIST,R3                                               00035600
         ICM   R4,15,COMCIBPT      POINT TO START CIB                   00035700
         BZ    NORMAL              BRANCH IF NO CIB                     00035800
         DROP  R3                                                       00035900
         MVC   ARGOPT+1(3),1(R2)   COPY PARM POINTER                    00036000
         LA    R2,IOLIST           SET INPUT/OUTPUT LIST POINTER        00036100
         ST    R2,ARGIOLST                                              00036200
         MVI   ARGIOLST,X'80'      TERMINATE ARGUMENT LIST              00036300
         LA    R2,IOGROUPS         SET NUMBER OF I/O LIST GROUPS        00036400
         ST    R2,ION                                                   00036500
         LA    R2,DDIN             SET SYSIN DD POINTER                 00036600
         ST    R2,IODDIN                                                00036700
         LA    R2,RTNIN            SET SYSIN I/O ROUTINE POINTER        00036800
         ST    R2,IORTNIN                                               00036900
         LA    R2,RTNSAVE          SET USER DATA POINTER                00037000
         ST    R2,IOUDIN                                                00037100
         LA    R2,DDPRT            SET SYSPRINT DD POINTER              00037200
         ST    R2,IODDPRT                                               00037300
         LA    R2,RTNPRT           SET SYSPRINT I/O ROUTINE POINTER     00037400
         ST    R2,IORTNPRT                                              00037500
         LA    R2,RTNSAVE          SET USER DATA POINTER                00037600
         ST    R2,IOUDPRT                                               00037700
         MVC   SYSPRINT(L'PRTSIZE),DCBPRT  COPY DCB TO WORK AREA        00037800
         OI    DMLIST,X'80'        TERMINATE OPEN LIST                  00037900
         MVC   STIMEREX(L'STIMERSI),STIMER  COPY STIMER EXIT            00038000
         MVI   TIMECB,X'40'        SET STIMER ECB INITIALLY POSTED      00038100
         MVI   WPLMCSF1-WPL+WTO,WPLMCSFB+WPLMCSFC  SET MCS FLAGS        00038200
         OI    CONTROL,SUPPRESS    SUPPRESS WTO OUTPUT INITIALLY        00038300
*---------------------------------------------------------------------- 00038400
         ATTACH EP=IDCAMS,         ATTACH ACCESS METHOD SERVICES       *00038500
               ECB=ATTECB,                                             *00038600
               SZERO=YES,                                              *00038700
               MF=(E,ARGLIST),                                         *00038800
               SF=(E,ATTACH)                                            00038900
         ST    R1,TCB              SAVE IDCAMS TCB POINTER              00039000
         WAIT  ECB=ATTECB,         WAIT FOR IDCAMS TERMINATION         *00039100
               LONG=YES            ALLOW SWAP OUT                       00039200
         DETACH TCB                DETACH IDCAMS TCB                    00039300
*---------------------------------------------------------------------- 00039400
NORMAL   DS    0H                                                       00039500
         L     R2,ATTECB           SAVE IDCAMS RETURN CODE              00039600
         LR    R1,R13              RESTORE SAVE AREA POINTER            00039700
         L     R13,4(R1)                                                00039800
         FREEMAIN RU,              FREE WORK AREA STORAGE              *00039900
               LV=L'WORKSIZE,                                          *00040000
               A=(1)                                                    00040100
         LA    R15,0(R2)           SET RETURN CODE                      00040200
         RETURN (14,12),           RETURN TO SYSTEM                    *00040300
               RC=(15)                                                  00040400
         DROP  R12,R13                                                  00040500
         EJECT ,                                                        00040600
*********************************************************************** 00040700
*              SYSIN EXIT ROUTINE                                       00040800
*              ISSUE AMS READY MESSAGE                                  00040900
*              PASS MODIFY COMMAND INPUT TO IDCAMS AS SYSIN             00041000
*              PASS STOP COMMAND TO IDCAMS AS EOD                       00041100
*********************************************************************** 00041200
RTNIN    SAVE  (14,12)                                                  00041300
         LR    R12,R15                                                  00041400
         USING RTNIN,R12                                                00041500
         LR    R2,R1               SAVE ARGUMENT LIST POINTER           00041600
         USING RTNARG,R2                                                00041700
         L     R1,USERDATA         GET WORK AREA ADDRESSABILITY         00041800
         ST    R13,4(R1)           CHAIN SAVE AREAS                     00041900
         ST    R1,8(R13)                                                00042000
         LR    R13,R1                                                   00042100
         USING RTNSAVE,R13                                              00042200
         L     R3,COMM             GET ECB/CIB POINTERS                 00042300
         USING COMLIST,R3                                               00042400
         L     R4,COMCIBPT         POINT TO CIB                         00042500
         USING CIBNEXT,R4                                               00042600
         L     R5,IOFLAGS          POINT TO IDCAMS FLAGS                00042700
         USING FLAGS,R5                                                 00042800
         CLI   FLAGS,0             IS THIS OPEN REQUEST?                00042900
         BE    RTNINOPN            BRANCH - YES                         00043000
         CLI   FLAGS,4             IS THIS CLOSE REQUEST?               00043100
         BE    RTNINCLS            BRANCH - YES                         00043200
         CLI   FLAGS,8             IS THIS GET REQUEST?                 00043300
         BE    RTNINGET            BRANCH - YES                         00043400
         B     RTNINNO             BRANCH - ERROR                       00043500
         DROP  R5                                                       00043600
*---------------------------------------------------------------------- 00043700
RTNINOPN DS    0H                  OPEN SYSIN                           00043800
         QEDIT ORIGIN=COMCIBPT,    ALLOW MODIFY COMMAND                *00043900
               CIBCTR=1                                                 00044000
         SR    R0,R0                                                    00044100
         IC    R0,CIBCONID         GET START CONSOLE ID                 00044200
         WTO   MF=(E,WTOREADY)     READY FOR INPUT                      00044300
         B     RTNINNRM            BRANCH                               00044400
*---------------------------------------------------------------------- 00044500
RTNINCLS DS    0H                  CLOSE SYSIN                          00044600
         QEDIT ORIGIN=COMCIBPT,    DISALLOW MODIFY COMMANDS            *00044700
               CIBCTR=0                                                 00044800
         QEDIT ORIGIN=COMCIBPT,    FREE STOP CIB                       *00044900
               BLOCK=CIBNEXT                                            00045000
         B     RTNINNRM            BRANCH                               00045100
*---------------------------------------------------------------------- 00045200
RTNINGET DS    0H                  GET SYSIN                            00045300
         QEDIT ORIGIN=COMCIBPT,    FREE START/MODIFY CIB               *00045400
               BLOCK=CIBNEXT                                            00045500
         DROP  R4                                                       00045600
         NI    CONTROL,X'FF'-SUPPRESS  ALLOW WTO OUTPUT                 00045700
         L     R1,COMECBPT         POINT TO COMM ECB                    00045800
*                                                                       00045900
         WAIT  ECB=(1),            WAIT FOR MODIFY/STOP COMMAND        *00046000
               LONG=YES            ALLOW SWAP OUT                       00046100
*                                                                       00046200
         L     R4,COMCIBPT         POINT TO CIB                         00046300
         USING CIBNEXT,R4                                               00046400
         CLI   CIBVERB,CIBSTOP     IS THIS STOP COMMAND?                00046500
         BE    RTNINEOD            BRANCH - YES                         00046600
         CLI   CIBVERB,CIBMODFY    IS THIS MODIFY COMMAND?              00046700
         BNE   RTNINNO             BRANCH - NO                          00046800
         L     R5,IOINFO           POINT TO IDCAMS RECORD INFO          00046900
         USING GETPUT,R5                                                00047000
         LH    R6,CIBDATLN         GET CIB DATA LENGTH                  00047100
         ST    R6,RECORDLN         STORE LENGTH FOR IDCAMS              00047200
         LA    R7,CIBDATA          GET ADDRESS OF RECORD                00047300
         ST    R7,RECORD           STORE ADDRESS FOR IDCAMS             00047400
         B     RTNINNRM            BRANCH                               00047500
RTNINEOD DS    0H                                                       00047600
         LA    R15,4               SET END OF DATA RETURN CODE          00047700
         B     RTNINRET            BRANCH                               00047800
*---------------------------------------------------------------------- 00047900
RTNINNO  DS    0H                  ERROR                                00048000
         LA    R15,12              SET DO NOT CALL RETURN CODE          00048100
         B     RTNINRET            BRANCH                               00048200
*---------------------------------------------------------------------- 00048300
RTNINNRM DS    0H                  PASS SYSIN RECORD                    00048400
         SR    R15,R15             SET NORMAL RETURN CODE               00048500
RTNINRET DS    0H                                                       00048600
         L     R13,4(R13)          RESTORE SAVE AREA POINTER            00048700
         RETURN (14,12),           RETURN TO IDCAMS                    *00048800
               RC=(15)                                                  00048900
         DROP  R12,R2,R13,R3,R4,R5                                      00049000
         EJECT ,                                                        00049100
*********************************************************************** 00049200
*              SYSPRINT EXIT ROUTINE                                    00049300
*              OPEN SYSPRINT                                            00049400
*              WRITE IDCAMS OUTPUT TO CONSOLE AND SYSPRINT              00049500
*              CLOSE SYSPRINT                                           00049600
*********************************************************************** 00049700
RTNPRT   SAVE  (14,12)                                                  00049800
         LR    R12,R15                                                  00049900
         USING RTNPRT,R12                                               00050000
         LR    R2,R1               SAVE ARGUMENT LIST POINTER           00050100
         USING RTNARG,R2                                                00050200
         L     R1,USERDATA         GET WORK AREA ADDRESSABILITY         00050300
         ST    R13,4(R1)           CHAIN SAVE AREAS                     00050400
         ST    R1,8(R13)                                                00050500
         LR    R13,R1                                                   00050600
         USING RTNSAVE,R13                                              00050700
         L     R3,COMM             GET ECB/CIB POINTERS                 00050800
         USING COMLIST,R3                                               00050900
         L     R4,COMCIBPT         POINT TO CIB                         00051000
         USING CIBNEXT,R4                                               00051100
         L     R5,IOFLAGS          POINT TO IDCAMS FLAGS                00051200
         USING FLAGS,R5                                                 00051300
         CLI   FLAGS,0             IS THIS OPEN REQUEST?                00051400
         BE    RTNPRTOP            BRANCH - YES                         00051500
         CLI   FLAGS,4             IS THIS CLOSE REQUEST?               00051600
         BE    RTNPRTCL            BRANCH - YES                         00051700
         CLI   FLAGS,12            IS THIS PUT REQUEST?                 00051800
         BE    RTNPRTPU            BRANCH - YES                         00051900
         B     RTNPRTNO            BRANCH - ERROR                       00052000
         DROP  R5                                                       00052100
*---------------------------------------------------------------------- 00052200
RTNPRTOP DS    0H                  OPEN SYSPRINT                        00052300
         OPEN  (SYSPRINT,OUTPUT),  OPEN DCB                            *00052400
               MF=(E,DMLIST)                                            00052500
         B     RTNPRTNR            BRANCH                               00052600
*---------------------------------------------------------------------- 00052700
RTNPRTCL DS    0H                  CLOSE SYSPRINT                       00052800
         TM    SYSPRINT+DCBOFLGS-IHADCB,DCBOFOPN  DCB OPEN?             00052900
         BZ    RTNPRTNR            NO, BRANCH                           00053000
         CLOSE MF=(E,DMLIST)       CLOSE DCB                            00053100
         FREEPOOL SYSPRINT         FREE BUFFER POOL                     00053200
         B     RTNPRTNR            BRANCH                               00053300
*---------------------------------------------------------------------- 00053400
RTNPRTPU DS    0H                  PUT SYSPRINT                         00053500
         L     R5,IOINFO           POINT TO IDCAMS RECORD INFO          00053600
         USING GETPUT,R5                                                00053700
         L     R6,RECORDLN         GET IDCAMS RECORD LENGTH             00053800
         L     R7,RECORD           POINT TO IDCAMS OUTPUT RECORD        00053900
         DROP  R5                                                       00054000
         LA    R8,WTO              POINT TO WTO PARAMETER LIST          00054100
         USING WPL,R8                                                   00054200
         BCTR  R6,0                DECREMENT LENGTH FOR MVC             00054300
         TM    SYSPRINT+DCBOFLGS-IHADCB,DCBOFOPN  DCB OPEN?             00054400
         BZ    RTNPRTNP            NO, BRANCH                           00054500
         PUT   SYSPRINT            LOCATE SYSPRINT BUFFER               00054600
         EX    R6,RTNPRTMP         MOVE DATA TO BUFFER                  00054700
RTNPRTNP DS    0H                                                       00054800
         BCTR  R6,0                DECREMENT LENGTH FOR MVC             00054900
         EX    R6,RTNPRTMW         MOVE DATA TO WTO TEXT                00055000
         LA    R0,5(R6)            GET LENGTH FOR WTO                   00055100
         STH   R0,WPLLGH           STORE LENGTH FOR WTO                 00055200
         LA    R0,6(R6)            GET LENGTH FOR RECFM=VA              00055300
         SLL   R0,16               SHIFT FOR RDW                        00055400
         ST    R0,0(R1)            STORE LENGTH FOR PUT                 00055500
         CLI   0(R7),C'1'          IS THIS PAGE EJECT?                  00055600
         BE    RTNPRTNR            BRANCH - YES                         00055700
         EX    R6,RTNPRTCW         IS LINE BLANK?                       00055800
         BE    RTNPRTNR            BRANCH - YES                         00055900
         CLC   ENTRIES,WPLTXT      ENTRIES PROCESSED LINE?              00056000
         BE    RTNPRTEN            BRANCH - YES                         00056100
         CLC   PROTECT,WPLTXT      PROTECTED ENTRIES SUPPRESSED LINE?   00056200
         BE    RTNPRTPR            BRANCH - YES                         00056300
         TM    CONTROL,SUPPRESS    SUPPRESSING WTO OUTPUT?              00056400
         BO    RTNPRTNR            BRANCH - YES                         00056500
         WAIT  ECB=TIMECB,         SLOW DOWN CONSOLE OUTPUT            *00056600
               LONG=NO             PREVENT SWAP OUT                     00056700
         SR    R0,R0               CLEAR R0                             00056800
         IC    R0,CIBCONID         INSERT CALLING CONSOLE ID            00056900
         WTO   MF=(E,WPL)          RESPOND WITH IDCAMS OUTPUT           00057000
         XC    TIMECB,TIMECB       CLEAR STIMER ECB                     00057100
         STIMER REAL,              SET WAIT INTERVAL                   *00057200
               STIMEREX,                                               *00057300
               BINTVL=BINTVL                                            00057400
         B     RTNPRTNR            BRANCH                               00057500
RTNPRTEN DS    0H                                                       00057600
         OI    CONTROL,SUPPRESS    SET SUPPRESS WTO OUTPUT FLAG         00057700
         B     RTNPRTNR                                                 00057800
RTNPRTPR DS    0H                                                       00057900
         NI    CONTROL,X'FF'-SUPPRESS  ALLOW WTO OUTPUT                 00058000
         B     RTNPRTNR                                                 00058100
*---------------------------------------------------------------------- 00058200
RTNPRTNO DS    0H                  ERROR                                00058300
         LA    R15,12              SET DO NOT CALL RETURN CODE          00058400
         B     RTNPRTRT            BRANCH                               00058500
*---------------------------------------------------------------------- 00058600
RTNPRTNR DS    0H                  SYSPRINT RECORD PROCESSED            00058700
         SR    R15,R15             SET NORMAL RETURN CODE               00058800
RTNPRTRT DS    0H                                                       00058900
         L     R13,4(R13)          RESTORE SAVE AREA POINTER            00059000
         RETURN (14,12),           RETURN TO IDCAMS                    *00059100
               RC=(15)                                                  00059200
*---------------------------------------------------------------------- 00059300
RTNPRTMP MVC   4(0,R1),0(R7)       EXECUTED - MOVE SYSPRINT FOR PUT     00059400
RTNPRTMW MVC   WPLTXT(0),1(R7)     EXECUTED - MOVE SYSPRINT FOR WTO     00059500
RTNPRTCW CLC   WPLTXT(0),BLANKS    EXECUTED - COMPARE FOR ALL BLANKS    00059600
         DROP  R12,R2,R13,R4,R8                                         00059700
         EJECT ,                                                        00059800
*********************************************************************** 00059900
*              DUMMY CODE/CONTROL BLOCKS                                00060000
*              THESE ARE COPIED TO WORK AREA FOR REENTRANCY             00060100
*********************************************************************** 00060200
         USING STIMEREX,R15                                             00060300
STIMER   SAVE  (14,12)             DUMMY STIMER EXIT                    00060400
         POST  TIMECB              POST SYSPRINT EXIT ROUTINE           00060500
         RETURN (14,12)                                                 00060600
         DROP  R15                                                      00060700
DCBPRT   DCB   BFALN=F,            DUMMY SYSPRINT DCB                  *00060800
               BLKSIZE=129,                                            *00060900
               BUFNO=2,                                                *00061000
               DDNAME=SYSPRINT,                                        *00061100
               DSORG=PS,                                               *00061200
               LRECL=125,                                              *00061300
               MACRF=PL,                                               *00061400
               RECFM=VA                                                 00061500
         EJECT ,                                                        00061600
*********************************************************************** 00061700
*              CONSTANTS                                                00061800
*********************************************************************** 00061900
BINTVL   DC    F'100'              1.00 SECOND STIMER VALUE             00062000
DDIN     DC    CL10'DDSYSIN'       DDNAME - SYSIN                       00062100
DDPRT    DC    CL10'DDSYSPRINT'    DDNAME - SYSPRINT                    00062200
BLANKS   DC    CL(L'WPLTXT)' '     BLANK LINE                           00062300
ENTRIES  DC    C'         THE NUMBER OF ENTRIES PROCESSED WAS'          00062400
PROTECT  DC  C'         THE NUMBER OF PROTECTED ENTRIES SUPPRESSED WAS' 00062500
WTOREADY WTO   'AMS READY',                                            *00062600
               MCSFLAG=(REG0,RESP),                                    *00062700
               MF=L                                                     00062800
         END   ,                                                        00062900
//********************************************************************  00063000
//*                                                                     00063100
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR     <- TARGET LOADLIB       00063200
//LKED.SYSIN DD *                                                       00063300
  SETCODE AC(1)                                                         00063400
  NAME CONSAMS(R)                                                       00063500
//********************************************************************  00063600
//*                                                                     00063700
//IEBUPDTE EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW                      00063800
//*                                                                     00063900
//SYSUT2   DD  DISP=OLD,DSN=SYS2.PROCLIB        <- TARGET PROCLIB       00064000
//SYSPRINT DD  SYSOUT=*                                                 00064100
//SYSIN DD DATA,DLM='$$'                                                00064200
./ ADD NAME=AMS                                                         00064300
//*-------------------------------------------------------------------* 00064400
//*              EXECUTE IDCAMS COMMANDS FROM CONSOLE                 * 00064500
//*-------------------------------------------------------------------* 00064600
//AMS     PROC                                                          00064700
//IEFPROC EXEC PGM=CONSAMS,PARM='MARGINS(1,120)'                        00064800
//AMSDUMP  DD  SYSOUT=Y    IDCAMS SNAP DUMP                             00064900
//SYSOUT   DD  SYSOUT=Y    OUTFILE OUTPUT                               00065000
//SYSPRINT DD  SYSOUT=Y    CONSOLE LOG                                  00065100
//SYSUDUMP DD  SYSOUT=Y                                                 00065200
./ ENDUP                                                                00065300
$$                                                                      00065400
//********************************************************************  00065500
//                                                                      00065600
