//SHOWMVS  JOB (TSO),
//             'Install ShowMVS',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(0,0),
//             REGION=0K,
//             USER=IBMUSER,
//             PASSWORD=SYS1
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *
    DELETE SYS2.LINKLIB(SHOWMVS)
    DELETE SYS2.MACLIB(YREGS)
    DELETE SYS2.MACLIB(IHADQE)
    DELETE SYS2.MACLIB(WK$OUT)
    DELETE SYS2.MACLIB($MEMORY)
    DELETE SYS2.MACLIB(IHAPCCA)
    SET LASTCC = 0
    SET MAXCC = 0
/*
//YREGS    EXEC PGM=IEBGENER
//SYSUT1   DD DATA,DLM=@@
         MACRO
         YREGS
         GBLA  &REGS
&REGS    SETA  1
         SPACE 1
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         SPACE 1
         MEND                                                    MACRO
@@
//SYSUT2   DD DISP=SHR,DSN=SYS2.MACLIB(YREGS)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//IHADQE   EXEC PGM=IEBGENER
//SYSUT1   DD DATA,DLM=@@
         MACRO
         IHADQE   &TYPE
*
*        RELEASE 1 AOS, 11/3/71, LEVEL=1
*
         AIF   ('&TYPE' EQ 'INTERNAL').EQU
DQESECT  DSECT      DESCRIPTOR QUEUE ELEMENT
DQFQEPTR DS    A    PTR TO FIRST FREE AREA
DQEPTR   DS    A    PTR TO NEXT DQE OR ZERO
DQEHRID  DS    0C   HIERARCHY IDENTIFIER
DQEBLKAD DS    A    ADDR FIRST 2K BLOCK DESCRIBED BY THIS DQE
DQELNTH  DS    F    LENGTH OF AREA DESCRIBED BY THIS DQE
         MEXIT
.EQU     ANOP
DQFQEPTR EQU      0    PTR TO FIRST FREE AREA
DQEPTR   EQU      4    PTR TO NEXT DQE OR ZERO
DQEHRID  EQU      4   HIERARCHY IDENTIFIER
DQEBLKAD EQU      8    ADDR FIRST 2K BLOCK DESCRIBED BY THIS DQE
DQELNTH  EQU      12    LENGTH OF AREA DESCRIBED BY THIS DQE
         MEND  ,
@@
//SYSUT2   DD DISP=SHR,DSN=SYS2.MACLIB(IHADQE)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//IHAPQE   EXEC PGM=IEBGENER
//SYSUT1   DD DATA,DLM=@@
         MACRO
         IHAPQE   &TYPE
*
*        RELEASE 1 AOS, 11/3/71, LEVEL=1
*
         AIF   ('&TYPE' EQ 'INTERNAL').EQU
PQESECT  DSECT      PARTITION QUEUE ELEMENT
PQEFFBQE DS    A    PTR TO FIRST FBQE OR IF NONE TO PQE
PQEBFBQE DS    A    PTR TO LAST FBQE OR IF NONE, TO PQE
PQEFPQE  DS    A    ADDR NEXT PQE OR ZERO
PQEBPQE  DS    A    ADDR PREVIOUS PQE OR ZERO
PQETCB   DS    A    ADDR TCB FOR JOB STEP TO WHICH SPACE BELONGS
PQESIZE  DS    F    SIZE OF REGION DESCRIBED BY THIS PQE
PQEREGN  DS    A    ADDR FIRST BYTE OF REGION DESCRIBED BY THIS PQE
PQERFLGS DS    CL1  FLAG BYTE
PQEHRID  DS    CL1  HIERARCHY IDENTIFIER
VMMFLGS  DS    BL1  SEVEN HIGH ORDER BITS ZERO
VVVRFLG  EQU   X'01' REAL OR VIRTUAL REGION FLAG
PQERSVD  DS    CL1  RESERVED
         MEXIT
.EQU     ANOP
PQEFFBQE EQU      0    PTR TO FIRST FBQE OR IF NONE TO PQE
PQEBFBQE EQU      4    PTR TO LAST FBQE OR IF NONE, TO PQE
PQEFPQE  EQU      8    ADDR NEXT PQE OR ZERO
PQEBPQE  EQU      12    ADDR PREVIOUS PQE OR ZERO
PQETCB   EQU      16    ADDR TCB FOR JOB STEP TO WHICH SPACE BELONGS
PQESIZE  EQU      20    SIZE OF REGION DESCRIBED BY THIS PQE
PQEREGN  EQU      24    ADDR FIRST BYTE OF REGION DESCRIBED BY THIS PQE
PQERFLGS EQU      28  FLAG BYTE
PQEHRID  EQU      29  HIERARCHY IDENTIFIER
VMMFLGS  EQU      30  SEVEN HIGH ORDER BITS ZERO
VVVRFLG  EQU   X'01' REAL OR VIRTUAL REGION FLAG
PQERSVD  EQU      31  RESERVED
         MEND  ,
@@
//SYSUT2   DD DISP=SHR,DSN=SYS2.MACLIB(IHAPQE)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY

//WK$OUT   EXEC PGM=IEBGENER
//SYSUT1   DD DATA,DLM=@@
         MACRO
         WK$OUT
DY8OUT   DS    16F                     SAVEAREA: SHOWOUT
DY8DCB   DCB   DDNAME=SYSPRINT,MACRF=PM,DSORG=PS,                      +
               LRECL=100,BLKSIZE=3200,RECFM=FB
DY8OPEN  OPEN  (,),MF=L
DY8CLOSE CLOSE (,),MF=L
DY8PTRS  DS    2F                      BXLE: @REC, L'REC
DY8SA    DS    18F                     DATA MANAGEMENT SAVEAREA
DY8WORD1 DS    F                       WORK
DY8WORD2 DS    F                       WORK
DY8WORD3 DS    F                       WORK
         MEND
@@
//SYSUT2   DD DISP=SHR,DSN=SYS2.MACLIB(WK$OUT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//EMEMORY  EXEC PGM=IEBGENER
//SYSUT1   DD DATA,DLM=@@
         MACRO                                              $MEMORY
         EMEMORY &MSG,&START,&END
         L     R1,&START               START ADDRESS        $MEMORY
         L     R2,&END                 END ADDRESS          $MEMORY
         LA    R0,0001                 ONE BYTE             $MEMORY
         ALR   R0,R2                   END ADDRESS          $MEMORY
         SLR   R0,R1                   SIZE IN BYTES        $MEMORY
         SRL   R0,010                  SIZE  IN "K"         $MEMORY
         SRL   R1,010                  START IN "K"         $MEMORY
         SRL   R2,010                  END   IN "K"         $MEMORY
         STRING 4X,&MSG,                                               X
               2X,(&START,,X),((R1),,R8B),'K',                         X
               5X,(&END,4,X),((R2),,R7B),'K',                          X
               ((R0),,R8B),'K',                                        X
               INTO=LINE
         LA    R10,NEXTLINE                                 $MEMORY
         MEND                                               $MEMORY
@@
//SYSUT2   DD DISP=SHR,DSN=SYS2.MACLIB(EMEMORY)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SMEMORY  EXEC PGM=IEBGENER
//SYSUT1   DD DATA,DLM=@@
         MACRO                                              $MEMORY
         SMEMORY &MSG,&START,&SIZE
         L     R1,&START               START ADDRESS        $MEMORY
         L     R0,&SIZE                SIZE IN BYTES
         LR    R2,R0                   SIZE IN BYTES        $MEMORY
         AR    R2,R1                   END ADDRESS          $MEMORY
         ST    R2,DWD                  END ADDRESS          $MEMORY
         SRL   R0,010                  SIZE  IN "K"         $MEMORY
         SRL   R1,010                  START IN "K"         $MEMORY
         SRL   R2,010                  END   IN "K"         $MEMORY
         STRING 4X,&MSG,                                               X
               2X,(&START,,X),((R1),,R8B),'K',                         X
               5X,(DWD,4,X),((R2),,R7B),'K',                           X
               ((R0),,R8B),'K',                                        X
               INTO=LINE
         LA    R10,NEXTLINE                                 $MEMORY
         MEND                                               $MEMORY
@@
//SYSUT2   DD DISP=SHR,DSN=SYS2.MACLIB(SMEMORY)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//IHAPCCA  EXEC PGM=IEBGENER
//SYSUT1   DD DATA,DLM=@@
         MACRO
         IHAPCCA &DSECT=YES   */
*
*/* **************************************************************** */
*/*                                                                  */
*/*            PHYSICAL CONFIGURATION COMMUNICATION AREA             */
*/*                                                                  */
*/*  OS/VS2 SU64, 06/26/78, LEVEL=3                                  */
*/*                                                                  */
*/*  METHOD OF ACCESS                                                */
*/*      BAL  - DSECT IS PRODUCED UNLESS DSECT=NO IS SPECIFIED.      */
*/*             USING ON PCCA GIVES ADDRESSABILITY FOR ALL SYMBOLS.  */
*/*      PL/S - DCL PCCAPTR PTR                                      */
*/*                                                                  */
*/* **************************************************************** */
         SPACE 1
         AIF   ('&DSECT' EQ 'NO').PCCA010
PCCA     DSECT
         AGO   .PCCA020
.PCCA010 ANOP
         DS    0D
PCCA     EQU   *
.PCCA020 ANOP
PCCAPCCA DC    CL4'PCCA' -    CONTROL BLOCK ACRONYM IN EBCDIC
PCCACPID DC    XL12'00' -     CPU ID (CONTAINS SERIAL NUMBER)
PCCACPUA DC    H'0' -         PHYSICAL CPU ADDRESS
PCCACAFM DC    H'0' -         BIT MASK CORRESPONDING TO PHYSICAL CPU
*                             ADDRESS
PCCATQEP DC    A(0) -         TQE POINTER
PCCAPSAV DC    A(0) -         VIRTUAL ADDRESS OF PSA
PCCAPSAR DC    A(0) -         REAL ADDRESS OF PSA
PCCARV81 DS    A -            RESERVED                           MDC005
PCCARV82 DS    A -            RESERVED                           MDC005
PCCARV83 DS    A -            RESERVED                           MDC005
PCCARV84 DS    A -            RESERVED                           MDC005
PCCARV85 DS    A -            RESERVED                           MDC005
PCCARV86 DS    A -            RESERVED                           MDC005
PCCARV87 DS    A -            RESERVED                           MDC005
PCCARV88 DS    A -            RESERVED                           MDC005
PCCARV89 DS    A -            RESERVED                           MDC005
PCCARV90 DS    A -            RESERVED                           MDC005
PCCARV91 DS    A -            RESERVED                           MDC005
PCCARV92 DS    A -            RESERVED                           MDC005
PCCARV93 DS    A -            RESERVED                           MDC005
PCCARV94 DS    A -            RESERVED                           MDC005
PCCARV95 DS    A -            RESERVED                           MDC005
PCCARV96 DS    A -            RESERVED                           MDC005
PCCARV97 DS    A -            RESERVED                           MDC005
PCCARV98 DS    A -            RESERVED                           MDC005
PCCARV99 DS    A -            RESERVED                           MDC005
PCCARV9A DS    A -            RESERVED                           MDC005
PCCARV9B DS    A -            RESERVED                           MDC005
PCCARV9C DS    A -            RESERVED                           MDC005
PCCARV9D DS    A -            RESERVED                           MDC005
PCCARV9E DS    A -            RESERVED                           MDC005
PCCATMST DS    0BL4 -         TIMER STATUS BYTES
PCCATMFL DC    XL1'00' -      FIRST BYTE OF PCCATMST
PCCAINIT EQU   X'80' -        ENTRY HAS BEEN INITIALIZED
PCCASYNC EQU   X'40' -        CLOCK OUT OF SYNCHRONIZATION
PCCAVKIL EQU   X'20' -        VARY CPU SHOULD BE CANCELLED
PCCAMCC  EQU   X'10' -        PROCESSING FOR PERMANENTLY DAMAGED CLOCK
*                             COMPARATOR MUST BE DONE
PCCAMINT EQU   X'08' -        PROCESSING FOR CPU TIMER MUST BE DONE
PCCARV02 EQU   X'04',,C'X' -  RESERVED
PCCARV03 EQU   X'02',,C'X' -  RESERVED
PCCARV04 EQU   X'01',,C'X' -  RESERVED
PCCATODE DC    XL1'00' -      TOD CLOCK ERROR FLAGS
PCCANUTD EQU   X'80' -        CLOCK CANNOT BE USED
PCCANFTD EQU   X'40' -        CLOCK SHOULD NOT BE RESET
PCCACTTD EQU   X'3F' -        ERROR COUNT (6 BITS)
PCCACCE  DC    XL1'00' -      FLAGS FOR CLOCK COMPARATOR
PCCANUCC EQU   X'80' -        CLOCK COMPARATOR CANNOT BE USED
PCCANFCC EQU   X'40' -        CLOCK COMPARATOR SHOULD NOT BE RESET
PCCACTCC EQU   X'3F' -        ERROR COUNT (6 BITS)
PCCAINTE DC    XL1'00' -      FLAGS FOR CPU TIMER
PCCANUIN EQU   X'80' -        CPU TIMER CANNOT BE USED
PCCANFIN EQU   X'40' -        CPU TIMER SHOULD NOT BE RESET
PCCACTIN EQU   X'3F' -        ERROR COUNT (6 BITS)
PCCARPB  DC    F'0' -         EXTERNAL CALL SIGP BUFFER
PCCAEMSB DS    0CL16 -        EMERGENCY SIGNAL SIGP BUFFER
PCCAEMSI DS    0BL4 -         FIRST WORD OF EMS BUFFER
PCCARISP DC    XL1'00' -      CONTAINS PARALLEL/SERIAL REQUEST
*                             INDICATOR FOR REMOTE IMMEDIATE SIGNAL
PCCAPARL EQU   X'80' -        PARALLEL REQUEST                   MDC002
PCCASERL EQU   X'40' -        SERIAL REQUEST                     MDC003
PCCARV06 EQU   X'20',,C'X' -  RESERVED
PCCARV07 EQU   X'10',,C'X' -  RESERVED
PCCARV08 EQU   X'08',,C'X' -  RESERVED
PCCARV09 EQU   X'04',,C'X' -  RESERVED
PCCARV10 EQU   X'02',,C'X' -  RESERVED
PCCARV11 EQU   X'01',,C'X' -  RESERVED
PCCAEMS2 DC    XL1'00' -      SECOND BYTE OF PCCAEMSI
PCCASERP EQU   X'80' -        SERIAL PENDING INDICATOR
*                             (MDC325)                         @G64UP9A
PCCARV13 EQU   X'40',,C'X' -  RESERVED
PCCARV14 EQU   X'20',,C'X' -  RESERVED
PCCARV15 EQU   X'10',,C'X' -  RESERVED
PCCARV16 EQU   X'08',,C'X' -  RESERVED
PCCARV17 EQU   X'04',,C'X' -  RESERVED
PCCARV18 EQU   X'02',,C'X' -  RESERVED
PCCARV19 EQU   X'01',,C'X' -  RESERVED
PCCAEMS3 DC    XL1'00' -      THIRD BYTE OF PCCAEMSI
PCCARV20 EQU   X'80',,C'X' -  RESERVED
PCCARV21 EQU   X'40',,C'X' -  RESERVED
PCCARV22 EQU   X'20',,C'X' -  RESERVED
PCCARV23 EQU   X'10',,C'X' -  RESERVED
PCCARV24 EQU   X'08',,C'X' -  RESERVED
PCCARV25 EQU   X'04',,C'X' -  RESERVED
PCCARV26 EQU   X'02',,C'X' -  RESERVED
PCCARV27 EQU   X'01',,C'X' -  RESERVED
PCCARMSB DC    XL1'00' -      CONTAINS RMS INDICATOR
PCCARV28 EQU   X'80',,C'X' -  RESERVED
PCCARV29 EQU   X'40',,C'X' -  RESERVED
PCCARV30 EQU   X'20',,C'X' -  RESERVED
PCCARV31 EQU   X'10',,C'X' -  RESERVED
PCCARV32 EQU   X'08',,C'X' -  RESERVED
PCCARV33 EQU   X'04',,C'X' -  RESERVED
PCCARV34 EQU   X'02',,C'X' -  RESERVED
PCCARMS  EQU   X'01' -        SIGP WAS ISSUED VIA RMS
PCCAEMSP DC    A(0) -         REMOTE IMMEDIATE SIGNAL PARAMETER ADDRESS
PCCAEMSE DC    A(0) -         REMOTE IMMEDIATE SIGNAL RECEIVING ROUTINE
*                             ENTRY POINT ADDRESS
PCCAEMSA DC    A(0) -         PCCA ADDRESS OF THE RECEIVING ROUTINE
PCCAPWAV DC    A(0) -         VIRTUAL ADDRESS OF MCH PROCESSOR WORK
*                             AREA
PCCAPWAR DC    A(0) -         REAL ADDRESS OF MCH PROCESSOR WORK AREA
PCCALRBV DC    A(0) -         VIRTUAL ADDRESS OF MCH LOGREC BUFFER
PCCALRBR DC    A(0) -         REAL ADDRESS OF MCH LOGREC BUFFER
PCCAELAD DC    A(0) -         VIRTUAL ADDRESS OF I/O EXTENDED LOGOUT
*                             (IOEL) AREA  (MDC310)            @G51BP9A
PCCAELBA DC    A(0) -         VIRTUAL ADDRESS OF CCH LOGOUT BUFFER
*                             (MDC311)                         @G51BP9A
PCCACCHM DC    A(0) -         VIRTUAL ADDRESS OF CCH MESSAGE BUFFER
*                             (MDC312)                         @G51BP9A
PCCASRB  DC    XL44'00' -     SRB FOR CCH TO SCHEDULE IECVIRST TO
*                             PROCESS CHANNEL ERRORS  (MDC314) @G51BP9A
PCCACHAN DC    XL1'00' -      FLAG BYTE FOR CCH-IOS CHANNEL RECOVERY
*                             COMMUNICATION  (MDC315)          @G51BP9A
PCCAIRST EQU   X'80' -        IECVIRST IS PROCESSING CHANNEL ERRORS
*                             DETECTED DURING AN EXTERNAL MACHINE CHECK
*                             (MDC316)                         @G51BP9A
PCCAEXDM EQU   X'40' -        WHILE PCCAIRST BIT WAS SET, MORE CHANNEL
*                             ERRORS WERE DETECTED WHILE PROCESSING
*                             ANOTHER EXTERNAL DAMAGE MACHINE CHECK
*                             (MDC317)                         @G51BP9A
PCCAR107 EQU   X'20',,C'X' -  RESERVED                         @G51BP9A
PCCAR108 EQU   X'10',,C'X' -  RESERVED                         @G51BP9A
PCCAR109 EQU   X'08',,C'X' -  RESERVED                         @G51BP9A
PCCAR110 EQU   X'04',,C'X' -  RESERVED                         @G51BP9A
PCCAR111 EQU   X'02',,C'X' -  RESERVED                         @G51BP9A
PCCAR112 EQU   X'01',,C'X' -  RESERVED                         @G51BP9A
PCCASRBL DC    XL1'00' -      LOCK BYTE FOR COMMUNICATING CHANNEL
*                             ERRORS BETWEEN CCH AND IOS
*                             (MDC318)                         @G51BP9A
PCCASRBA EQU   X'00' -        SRB IS AVAILABLE FOR SCHEDULING
*                             (MDC319)                         @G51BP9A
PCCASRBN EQU   X'FF' -        SRB IS NOT AVAILABLE FOR SCHEDULING
*                             (MDC320)                         @G51BP9A
PCCACCHI DC    H'0' -         CHANNEL SET ID FOR THE CHANNEL SET IN
*                             ERROR  (MDC324)                  @G64AP9A
PCCAR106 DC    52XL1'00' -    RESERVED  (MDC304)               @G51BP9A
         DS    0D -           ALIGN PCCAWERP TO DOUBLEWORD
PCCAWERP DS    0XL8 -         WORK ERPIB FOR CCH
PCCACHUB DC    A(0) -         UCB ADDRESS OF THE DEVICE IN USE WHEN
*                             THE CHANNEL-DETECTED ERROR OCCURRED.
*                             THIS FIELD IS ZERO IF CCH HAS NOT
*                             CREATED AN ERPIB FOR THE ERP'S.
PCCACHPF DC    XL1'00' -      PROGRAM FLAGS.  INDICATES THE SELECTION
*                             OR INTERRUPTION SEQUENCE WHEN THE CSW WAS
*                             STORED.
PCCACSIO EQU   X'80' -        THE CSW WAS STORED AFTER A START I/O
*                             INSTRUCTION WAS EXECUTED.
PCCACINT EQU   X'40' -        THE CSW WAS STORED AFTER AN I/O
*                             INTERRUPTION
PCCACTIO EQU   X'20' -        THE CSW WAS STORED AFTER A TEST I/O
*                             INSTRUCTION WAS EXECUTED.
PCCACHIO EQU   X'10' -        THE CSW WAS STORED AFTER A HALT I/O
*                             INSTRUCTION WAS EXECUTED
PCCARV37 EQU   X'08',,C'X' -  RESERVED
PCCACSNS EQU   X'04' -        THE SENSE DATA WAS STORED
PCCACCNT EQU   X'02' -        THE CSW COUNT IS VALID
PCCACNOR EQU   X'01' -        NO RETRY IS TO BE ATTEMPTED UNDER ANY
*                             CONDITIONS
PCCACHBL DC    XL1'00' -      PROBABLE SOURCE OF ERROR
PCCACCPU EQU   X'80' -        CPU ERROR
PCCACCHA EQU   X'40' -        CHANNEL ERROR
PCCACSCU EQU   X'20' -        STORAGE CONTROL UNIT ERROR
PCCACSTG EQU   X'10' -        STORAGE ERROR
PCCACCUE EQU   X'08' -        CONTROL UNIT ERROR
PCCARV38 EQU   X'04',,C'X' -  RESERVED
PCCARV39 EQU   X'02',,C'X' -  RESERVED
PCCARV40 EQU   X'01',,C'X' -  RESERVED
PCCACHVA DC    XL1'00' -      VALIDITY INDICATORS.  WHEN THE DESIGNATED
*                             FIELD IS STORED BY THE CHANNEL WITH THE
*                             CORRECT CONTENTS THE VALIDITY BIT IS ONE.
*                             THE VALIDITY BIT FOR NON-STORED FIELDS
*                             IS MEANINGLESS.
PCCACITF EQU   X'80' -        INTERFACE ADDRESS IS VALID
PCCARV41 EQU   X'40',,C'X' -  RESERVED
PCCARV42 EQU   X'20',,C'X' -  RESERVED
PCCACSQV EQU   X'10' -        SEQUENCE CODE IS VALID
PCCACUNS EQU   X'08' -        UNIT STATUS IS VALID
PCCACCMD EQU   X'04' -        COMMAND ADDRESS IS VALID.  THE CSW
*                             CONTAINS A VALID COMMAND ADDRESS.
PCCACCHV EQU   X'02' -        CHANNEL ADDRESS IS VALID
PCCACDAV EQU   X'01' -        DEVICE ADDRESS IS VALID
PCCACHTS DC    XL1'00' -      TERMINATION AND SEQUENCE (RETRY) CODES
PCCACTEC EQU   X'C0' -        TWO-BIT TERMINATION CODE.  THIS CODE
*                             SPECIFIES THE TERMINATION SIGNALS USED ON
*                             THE I/O INTERFACE AFTER THE CHANNEL
*                             DETECTED THE ERROR.  THIS FIELD HAS
*                             MEANING ONLY WHEN ICC OR CCC IS INDICATED
*                             IN THE CSW.  THE FOLLOWING 4 EQU'S ARE
*                             THE VALUES FOR TERMINATION CODE.
PCCACTC0 EQU   X'00' -        INTERFACE DISCONNECT
PCCACTC1 EQU   X'40' -        STOP, STACK OR NORMAL TERMINATION
PCCACTC2 EQU   X'80' -        SELECTIVE RESET
PCCACTC3 EQU   X'C0' -        SYSTEM RESET
PCCARV43 EQU   X'20',,C'X' -  RESERVED
PCCARV44 EQU   X'10',,C'X' -  RESERVED
PCCACDIN EQU   X'08' -        I/O ERROR ALERT
PCCACSEQ EQU   X'07' -        THREE-BIT SEQUENCE CODE.  THESE CODES
*                             HAVE CHANNEL-DEPENDENT MEANINGS.
PCCACHS1 DC    XL1'00' -      CCH INTERNAL SWITCH 1
PCCACCMP EQU   X'80' -        COMMAND REGISTER PARITY IS VALID
PCCACNRE EQU   X'40' -        CCH WILL NOT CREATE A RECORD FOR THIS
*                             ERROR                              MDC006
PCCACFRR EQU   X'20' -        THE CCH FRR IS IN THE STACK        MDC007
PCCACNLS EQU   X'10' -        CCH IS TO PERFORM THE RECORD FUNCTION
*                             ONLY.  AN ERPIB IS NOT TO BE PLACED IN
*                             THE EWA.
PCCACAND EQU   X'08' -        ATTENTION HAS BEEN PRESENTED
PCCACIBC EQU   X'04' -        AN ERPIB FOR THIS ERROR HAS ALREADY BEEN
*                             CREATED
PCCACUCB EQU   X'02' -        UCB INVALID BIT
PCCARV47 EQU   X'01',,C'X' -  RESERVED
PCCACHS2 DC    XL1'00' -      CCH INTERNAL SWITCH 2
PCCACIOR EQU   X'80' -        I/O RESTART FUNCTION REQUIRED
PCCACALT EQU   X'40' -        THE ALTERNATE RETURN TO IOS IS TO BE USED
PCCACMOD EQU   X'20' -        NO MODULE IS AVAILABLE TO ANALYZE THE
*                             CHANNEL LOGOUT
PCCACNLG EQU   X'10' -        CCH DETECTED A FAILURE TO LOG OR FAILURE
*                             TO STORE AN ECSW CONDITION
PCCACURC EQU   X'08' -        THE STIDC FIELD OF THE CAT ENTRY IS
*                             VALID BUT NOT THAT OF A SUPPORTED
*                             CHANNEL                            MDC011
PCCACCRA EQU   X'04' -        CHANNEL RECONFIGURATION HARDWARE ACTIVE
*                             FOR THE CHANNEL  (MDC300)        @Y30CQ9A
PCCARV50 EQU   X'02',,C'X' -  RESERVED
PCCARV51 EQU   X'01',,C'X' -  RESERVED
PCCACHRB DC    XL1'00' -      CCH RECORD BYTE
PCCACSIB EQU   X'80' -        ERROR ON SIO
PCCACINB EQU   X'40' -        ERROR ON INTERRUPT
PCCACTIB EQU   X'20' -        ERROR ON TIO
PCCACHIB EQU   X'10' -        ERROR ON HIO
PCCARV52 EQU   X'08',,C'X' -  RESERVED
PCCACSNB EQU   X'04' -        SENSE DATA STORED
PCCACCVB EQU   X'02' -        COUNT VALID
PCCACNRB EQU   X'01' -        NO RETRY
PCCAIOSI DC    XL1'00' -      IOS INTERCEPT BYTE
PCCACHW1 DC    F'0' -         CCH WORK AREA 1
PCCACHW2 DC    F'0' -         CCH WORK AREA 2
PCCALOGL DC    H'0' -         LENGTH OF CHANNEL LOGOUT FOR CURRENT
*                             ERROR  (MDC305)                  @G51BP9A
PCCACELL DC    H'0' -         MAXIMUM LENGTH OF I/O EXTENDED LOGOUT
*                             (IOEL) AREA  (MDC313)            @G51BP9A
PCCALGP1 DC    XL1'00' -      LOGOUT PARITY AREA 1  (MDC306)   @G51BP9A
PCCALGP2 DC    XL1'00' -      LOGOUT PARITY AREA 2  (MDC307)   @G51BP9A
PCCACHPB DC    FL1'0' -       LOGOUT PARITY BYTE COUNT
*                             (MDC308)                         @G51BP9A
PCCARV05 DC    XL1'00' -      RESERVED FOR CCH  (MDC309)       @G51BP9A
PCCACHF1 DC    XL1'00' -      CCH FOOTPRINT BYTE 1
PCCACF11 EQU   X'80' -        IOS GPR'S SAVED
PCCACF12 EQU   X'40' -        UCB ADDRESS IS ZERO
PCCACF13 EQU   X'20' -        ERPIB EXISTS
PCCACF14 EQU   X'10' -        IGFCCHSI ENTERED
PCCACF15 EQU   X'08' -        IGFCCHII ENTERED
PCCACF16 EQU   X'04' -        IGFCCHFE ENTERED
PCCACF17 EQU   X'02' -        IGFC60 ENTERED
PCCACF18 EQU   X'01' -        IGFC70 ENTERED
PCCACHF2 DC    XL1'00' -      CCH FOOTPRINT BYTE 2
PCCACF21 EQU   X'80' -        IGFC80 ENTERED
PCCACF22 EQU   X'40' -        IGFCIC ENTERED
PCCACF23 EQU   X'20' -        IGFCCHRD ENTERED                   MDC008
PCCACF24 EQU   X'10' -        IGFCCHMP ENTERED
PCCACF25 EQU   X'08' -        IGFCCHUC ENTERED
PCCACF26 EQU   X'04' -        IGFCCHAS ENTERED                   MDC009
PCCACF27 EQU   X'02' -        IGFCCHIO ENTERED
PCCACF28 EQU   X'01' -        EXIT CCH
PCCACHF3 DC    XL1'00' -      CCH FOOTPRINT BYTE 3
PCCAISRB EQU   X'80' -        SRB FOR IECVIRST SCHEDULED
*                             (MDC321)                         @G51BP9A
PCCASLCK EQU   X'40' -        SPACE ALLOCATION LOCK HELD BY CCH
*                             (MDC322)                         @G51BP9A
PCCARV66 EQU   X'20',,C'X' -  RESERVED
PCCARV67 EQU   X'10',,C'X' -  RESERVED
PCCARV68 EQU   X'08',,C'X' -  RESERVED
PCCARV69 EQU   X'04',,C'X' -  RESERVED
PCCARV70 EQU   X'02',,C'X' -  RESERVED
PCCARV71 EQU   X'01',,C'X' -  RESERVED
PCCACHF4 DC    XL1'00' -      CCH FOOTPRINT BYTE 4
PCCARV72 EQU   X'80',,C'X' -  RESERVED
PCCARV73 EQU   X'40',,C'X' -  RESERVED
PCCARV74 EQU   X'20',,C'X' -  RESERVED
PCCARV75 EQU   X'10',,C'X' -  RESERVED
PCCARV76 EQU   X'08',,C'X' -  RESERVED
PCCARV77 EQU   X'04',,C'X' -  RESERVED
PCCARV78 EQU   X'02',,C'X' -  RESERVED
PCCARV79 EQU   X'01',,C'X' -  RESERVED
PCCACHSV DC    3F'0' -        CCH INTERNAL SAVE AREA.  FIRST WORD
*                             CONTAINS THE ADDRESS OF THE CURRENT CCH
*                             RECORD BUFFER                      MDC010
PCCACHID DC    XL8'00' -      STORE CHANNEL ID WORK AREA
PCCALOGA DC    A(0) -         ADDRESS OF CHANNEL LOGOUT          MDC004
PCCARV54 DC    A(0) -         RESERVED
PCCARV55 DC    A(0) -         RESERVED
PCCARV56 DC    A(0) -         RESERVED
PCCARV57 DC    A(0) -         RESERVED
PCCARV58 DC    A(0) -         RESERVED
PCCARV59 DC    A(0) -         RESERVED
PCCARV60 DC    A(0) -         RESERVED
PCCARV61 DC    A(0) -         RESERVED
PCCARV62 DC    A(0) -         RESERVED
PCCARV63 DC    A(0) -         RESERVED
PCCAATTR DC    XL1'00' -      PROCESSOR ATTRIBUTES  (MDC302)   @G47AN9A
PCCACPUM EQU   X'80' -        INDICATOR THAT DEAD CPU HAD A MALFUNCTION
*                             (MDC301)                         @G47AN9A
PCCAIO   EQU   X'40' -        PROCESSOR HAS I/O CAPABILITY
*                             (MDC303)                         @G47AN9A
PCCAR100 EQU   X'20',,C'X' -  RESERVED                         @G47AN9A
PCCAR101 EQU   X'10',,C'X' -  RESERVED                         @G47AN9A
PCCAR102 EQU   X'08',,C'X' -  RESERVED                         @G47AN9A
PCCAR103 EQU   X'04',,C'X' -  RESERVED                         @G47AN9A
PCCAR104 EQU   X'02',,C'X' -  RESERVED                         @G47AN9A
PCCAR105 EQU   X'01',,C'X' -  RESERVED                         @G47AN9A
PCCARV01 DC    XL1'00' -      RESERVED
PCCARV35 DC    H'0' -         RESERVED
PCCARV36 DC    F'0' -         RESERVED
         DC    XL200'00' -    RESERVED  (MDC323)               @G64AP9A
         MEND  , -   */
@@
//SYSUT2   DD DISP=SHR,DSN=SYS2.MACLIB(IHAPCCA)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//ASMLKD EXEC ASMFCL,
//             PARM.ASM='OBJECT,NODECK,TERM,XREF(SHORT)',
//             PARM.LKED='LET,MAP,XREF,LIST,TEST,AC=1'
//ASM.SYSLIB  DD   DSN=SYS1.MACLIB,DISP=SHR
//            DD   DSN=SYS2.MACLIB,DISP=SHR
//            DD   DSN=SYS1.AMODGEN,DISP=SHR
//            DD   DSN=SYS1.AMACLIB,DISP=SHR
//ASM.SYSTERM DD SYSOUT=*
//ASM.SYSTERM DD SYSOUT=*
//ASM.SYSIN   DD *,DLM=@@
***********************************************************************
*                                                                     *
*     THIS PROGRAM RUNS IN BATCH   AND DISPLAYS A LOT OF         <38J>*
*     INFORMATION THAT YOU GENERALLY HAVE TO GET FROM DIFFERENT       *
*     SOURCES.                                                        *
*                                                                     *
*       ON-LINE CPU'S                                                 *
*       AMOUNT OF ON-LINE STORAGE (REAL AND EXTENDED)                 *
*       IPL DATE, TIME, VOLSER, NUC-ID, CLPA                          *
*       VIRTUAL STORAGE MAP (CSA, SQA, LPA, ETC)                      *
*       DSN OF THE MASTER CATALOG                                     *
*       DSN OF THE PAGE DATA SETS                                     *
*       SMFSID, JWT, GRS' SNAME                                       *
*       MVS LEVEL, DFP LEVEL, OSLVL FLAGS                             *
*       PRODUCT LEVELS FOR: TSO/E, ISPF, DFDSS, HSM, RACF, VTAM       *
*       UCB TABLE WITH UNIT NAMES AND DASD VOLUME STATUS              *
*       SUB-SYSTEM VECTOR TABLE WITH FUNCTIONS PROCESSED              *
*       ACTIVE LPA QUEUE                                              *
*       SVC TABLE WITH NAME OF CORRESPONDING LPA MODULE               *
*       LINK-LIST DATA SETS, WITH CREATION DATE                       *
*       LPA-LIST DATA SETS, WITH CREATION DATE                        *
*       LIST OF AUTHORIZED LIBRARIES                                  *
*       YOUR TIOT, WITH EXCP COUNTS FOR EACH DD                       *
*       ENHANCED VIEW OF YOUR JPAQ                                    *
*       ENHANCED VIEW OF THE LOAD-LISTS OF YOUR ADDRESS SPACE         *
***********************************************************************
*
*  REGISTER USAGE
*
*  R0    SCRATCH
*  R1    SCRATCH
*  R2
*  R3    CVT
*  R4    TCB
*  R5
*  R6
*  R7
*  R8
*  R9
*  R10   CURRENT LINE PTR FOR STRING
*  R11   BASE
*  R12   BASE (SHORT-TERM VIA P$BEGIN)
*  R13   DYN
*  R14   SCRATCH, INTERNAL LINKAGE
*  R15   SCRATCH, RETURN CODE
***********************************************************************
         PRINT GEN,DATA                   <38J>
         EJECT ,
SHOWMVS  CSECT
         USING PSA,0                                              <38J>
*8OWMVS  RMODE ANY
BASEADDR DS    0H                                                 <38J>
         LR    R11,R15
         USING BASEADDR,R11                                       <38J>
         GETMAIN RU,LV=DYNL,BNDRY=PAGE                            <38J>
         LR    R13,R1
         USING DYND,R13
         ST    R14,0(R13)
         LA    R10,LINES               START OF TABLES
         USING LINE,R10
         L     R3,CVTPTR               CVT ADDRESS
         USING CVTMAP,R3               PERMANENT ASSIGNMENT
         L     R4,PSATOLD-PSA          MY TCB
         USING TCB,R4
         ST    R4,MYTCB                KEEP ITS ADDRESS FOR DETACH
         MVC   JSTCB,TCBJSTCB          THE JOB STEP TCB
*----------------------------------------------------------------------
*        INFO FUNCTION
*----------------------------------------------------------------------
         BAL   R14,SPACE1              BLANK LINE AT THE TOP
         BAL   R14,HARDWARE            HARDWARE DATA
         BAL   R14,IPLDATA             IPL DATE
         BAL   R14,MEMORY              VIRTUAL MEMORY MAP
         BAL   R14,MASTRCAT            MASTER CATALOG
         BAL   R14,PAGEDS              PAGE DATA SETS
         BAL   R14,PRODUCTS            TSO, SPF, DFDSS, HSM, RACF, VTAM
         BAL   R14,DEVICES             ON-LINE UNITS
         BAL   R14,SUBSYSTM            SUB-SYSTEMS
         BAL   R14,LPACTIV             ACTIVE LPA
         BAL   R14,SVCTABLE            SVC TABLE
         BAL   R14,LINKLIST            LNKLSTXX
         BAL   R14,LPALIST             LPALSTXX
         BAL   R14,APFLIST             APF LIST
         BAL   R14,TCBTREE             TCB TREE
         BAL   R14,JPAQ                JPAQ
         BAL   R14,LOADLIST            LOAD LISTS
*----------------------------------------------------------------------
*        END ROUTINES
*----------------------------------------------------------------------
         BAL   R14,CLOSE               CLOSE SYSPRINT             <38J>
         BAL   R14,GOBACK              CLOSE PROG                 <38J>
*----------------------------------------------------------------------
*        ON-LINE CPU'S AND REAL STORAGE
*----------------------------------------------------------------------
HARDWARE ST    R14,4(R13)
         STRING 'ONLINE CPU(S)',INTO=LINE                         <38J>
         LA    R10,NEXTLINE                                       <38J>
         L     R7,CVTPCCAT             PCCA VECTOR TABLE
         LA    R8,0016                 16 IS THE MAX NUMBER OF CPU'S
*LOOP
HARDW1   ICM   R9,B'1111',0(R7)        PCCA
         BZ    HARDW4                  THIS CPU ACTIVE, JUMP
         USING PCCA,R9
         STRING '  CPU ',(PCCACPUA,H,L),'  SERIAL: ',(PCCACPID+0,8), XXX
               '  MODEL: ',(PCCACPID+8,4),INTO=LINE
         LA    R10,NEXTLINE
HARDW4   LA    R7,4(,R7)               BUMP PCCAT PTR
         BCT   R8,HARDW1
*ENDLOOP
         BAL   R14,SPACE1              BLANK LINE
*
*        REAL STORAGE
*
         STRING 'REAL STORAGE',INTO=LINE                          <38J>
         LA    R10,NEXTLINE                                       <38J>
         LA    R1,0001
         AL    R1,CVTEORM              HI-ADDR
         SRL   R1,0010                 GET IT IN "K"
         STRING '  ON-LINE: ',(CVTRLSTG,F,L),'K',                      X
               '   HIGHEST ADDRESS: ',((R1),,L),'K',                   X
               INTO=LINE
         BAL   R14,SPACE2              BLANK LINE
         L     R14,4(R13)
         BR    R14
         EJECT ,
*----------------------------------------------------------------------
*        IPL DATA
*----------------------------------------------------------------------
IPLDATA  ST    R14,4(R13)
         AGO   .XIPLD1                                            <38J>
         L     R6,CVTSMCA              SMF SMCA
         USING SMCABASE,R6
         L     R1,SMCAIDTE             IPL DATE 00YYDDDF
         L     R1,=X'0000000F'         IPL DATE 00YYDDDF          <38J>
         L     R15,=A(@JDATE)          DATE CONVERT RTNE
         BALR  R14,R15                 CONVERT JULIAN TO YYMMDD
         MVC   DWD+0(2),0(R1)          YY
         MVI   DWD+2,C'/'              YY/
         MVC   DWD+3(2),2(R1)          YY/MM
         MVI   DWD+5,C'/'              YY/MM/
         MVC   DWD+6(2),4(R1)          YY/MM/DD
         SLR   R0,R0
         L     R1,SMCAITME             IPL TIME (BINARY)
         L     R1,=A(0)                IPL TIME (BINARY) <38J>
         D     R0,=X'00057E40'         GET HOURS
         LR    R2,R1                   HH
         LR    R1,R0                   REMAINDER
         SLR   R0,R0
         D     R0,=F'6000'             GET MINUTES IN R1
*
         ZAP   K2,CVTDATE              TODAY'S DATE
         STRING '(TODAY)',INTO=NEXTLINE
         SP    K2,SMCAIDTE             DIFFERENCE IN DAYS
         SP    K2,CVTDATE              DIFFERENCE IN DAYS
         BZ    IPLDATA6                TODAY, JUMP
         STRING '(YESTERDAY)',INTO=NEXTLINE
         CP    K2,=P'1'                WAS IT YESTERDAY?
         BE    IPLDATA6                YES, JUMP
         STRING '(',(K2,P,L0),' DAYS AGO)',INTO=NEXTLINE
IPLDATA6 DS    0H
         STRING 'IPL DATE: ',DWD,2X,(NEXTLINE,,T),                   XXX
               '   TIME: ',((R2),,R2Z),':',((R1),,R2Z),INTO=LINE
         LA    R10,NEXTLINE
.XIPLD1  ANOP                                                     <38J>
*
         L     R5,CVTSYSAD             IPL UCB
         USING UCBOB,R5
         L     R8,CVTEXT2              CVT EXTENSION
         USING CVTXTNT2,R8
         L     R15,CVTASMVT            POINT TO ASM VECTOR TABLE
         LA    R9,=C'NO '              CLPA=NO
         TM    ASMFLAG2-ASMVT(R15),ASMQUICK   QUICK START?
         BO    *+8                     YES, JUMP
         LA    R9,=C'YES'              NO, CLPA=YES
         STRING 'IPL FROM: ',UCBVOLI,'/',UCBNAME,                 <38J>X
               '   NUC ID: ',CVTNUCLS,                                 X
               '   CLPA: ',((R9),3),INTO=LINE
         BAL   R14,SPACE2              BLANK LINE
         L     R14,4(R13)
         BR    R14
         EJECT ,
*----------------------------------------------------------------------
*        DISPLAY VIRTUAL MEMORY MAP
*----------------------------------------------------------------------
MEMORY   ST    R14,4(R13)
         STRING 'VIRTUAL STORAGE MAP',INTO=LINE
         BAL   R14,SPACE2
         STRING '  ----AREA----',                                     XX
               '  ------START------     ------END-------     SIZE',   XX
               INTO=LINE
         BAL   R14,SPACE2
*
         L     R7,CVTGDA               GLOBAL DATA AREA
         USING GDA,R7
*
* COMMON AREA
*
         L     R8,CSAPQEP              GDA -> CSA PQE             <38J>
         USING PQESECT,R8                                         <38J>
         EMEMORY '----------',PQEREGN,CVTMZ00                     <38J>
         S     R10,=A(L$LINE)
         MVC   LINE(8),=C'  COMMON'
         BAL   R14,SPACE2
*
*
         L     R9,SQASPQEP             GDA -> SQA (SP245) SPQE
         USING SPQESECT,R9
         L     R9,SPDQEPTR             SPQE -> DQE
         USING DQESECT,R9
         SMEMORY '       SQA',DQEBLKAD,DQELNTH
*
         L     R14,DQEBLKAD            SQA BEGIN ADDR
         BCTR  R14,0
         ST    R14,DY8WORD2            PLPA/SQA CONTIGUOUS
         SLR   R9,R9
         ICM   R9,7,CVTLPDIR           CVT -> LPA DIRECTORY
         ST    R9,DY8WORD1
         EMEMORY '      PLPA',DY8WORD1,DY8WORD2
*
         L     R8,CSAPQEP              GDA -> CSA PQE             <38J>
         USING PQESECT,R8                                         <38J>
         SMEMORY '       CSA',PQEREGN,PQESIZE
         BAL   R14,SPACE1
*
* PRIVATE USER AREA
*
         SMEMORY '----------',PASTRT,PASIZE
         S     R10,=A(L$LINE)
         MVC   LINE(9),=C'  PRIVATE'
         BAL   R14,SPACE2
*
         L     R9,PSAAOLD              PSA -> CURRENT ASCB
         USING ASCB,R9
         L     R9,ASCBLDA              ASCB -> LDA
         USING LDA,R9
         L     R8,ASDPQE               LDA -> ADDR SPACE RGN PQE  <38J>
         USING PQESECT,R8
         SMEMORY 'V=V REGION',PQEREGN,PQESIZE                     <38J>
*
         L     R8,VRPQEP               GDA -> ALLOWABLE V=R       <38J>
         USING PQESECT,R8                                         <38J>
         SMEMORY 'V=R REGION',PQEREGN,PQESIZE                     <38J>
*
         L     R8,LDASRPQE             LDA -> SYSTEM REGION PQE   <38J>
         USING PQESECT,R8                                         <38J>
         SMEMORY 'SYS REGION',PQEREGN,PQESIZE                     <38J>
         BAL   R14,SPACE1
*
* NUCLEUS/SYSTEM AREA
*
         SLR   R14,R14                 ADDRESS 0
         ST    R14,DY8WORD1
         L     R14,CVTNUCB             LOWEST ADDR NOT IN NUC
         BCTR  R14,0                   LAST ADDR IN NUC
         ST    R14,DY8WORD2
         EMEMORY '----------',DY8WORD1,DY8WORD2                   <38J>
         S     R10,=A(L$LINE)
         MVC   LINE(9),=C'  NUCLEUS'
         BAL   R14,SPACE2
         DROP  R7                      GDA                        <38J>
         DROP  R8                      PQE                        <38J>
         DROP  R9
         L     R14,4(R13)
         BR    R14
ADDR0    DC    A(0)                    VIRTUAL ADDR ZERO          <38J>
         EJECT ,
*----------------------------------------------------------------------
*        DSNAME OF THE MASTER CATALOG
*----------------------------------------------------------------------
CBSCAXCN EQU   X'14'                   AMCBS OFFSET TO CAXWA CHAIN<38J>
CAXUCB   EQU   X'1C'                   CAXWA OFFSET TO UCB        <38J>
CAXCNAM  EQU   X'34'                   CAXWA OFFSET TO CAT NAME   <38J>
MASTRCAT ST    R14,4(R13)
         L     R5,CVTCBSP              @ AMCBS
         L     R6,CBSCAXCN(,R5)        @ CAXWA CHAIN              <38J>
*
CATLOOP  DS    0H
         L     R7,CAXUCB(,R6)          UCB ADDR                   <38J>
         USING UCBOB,R7
*                                      MCAT AT END OF CAXWA CHAIN <38J>
         MVC   UNITNAME+8(4),UCBTYP    DEVICE TYPE
*8<TMP>  BAL   R14,GETUNIT             GET UNITNAME
*                                      KEEP OVERLAYING, MCAT LAST CAXWA
         MVC   LINE,BLANKS                                        <38J>
         STRING 'MASTER CATALOG:',                                     +
               '  DSN=',(X'034'(R6),44,T),                             +
               ' CUU=',UCBNAME,                                        +
               ' VOLSER=',UCBVOLI,INTO=LINE
         ICM   R6,15,4(R6)             @ NEXT CAXWA, IF ANY       <38J>
         BNZ   CATLOOP                                            <38J>
         BAL   R14,SPACE2                                         <38J>
         L     R14,4(R13)
         BR    R14
         DROP  R7                      UCBOB
         EJECT ,
*----------------------------------------------------------------------
*        DISPLAY PAGE DATA SETS
*----------------------------------------------------------------------
PAGEDS   ST    R14,4(R13)
         STRING 'PAGE DATA SETS:',INTO=LINE
         BAL   R14,SPACE2              BLANK LINE
         L     R9,CVTASMVT             POINT TO ASM VECTOR TABLE
         L     R9,ASMPART-ASMVT(,R9)   POINT TO PAGE ACT REF TABLE
         L     R1,PARTSIZE-PART(,R9)   NUMBER OF PART ENTRIES
         L     R2,PARTDSNL-PART(,R9)   POINT TO 1ST PAGE DSN
*LOOP
PAGEDS1  CLI   0(R2),C' '              VALID DSN?
         BNH   PAGEDS2                 NO, SKIP IT
         MVC   LINE,BLANKS             BLANK LINE
         MVC   DSNAME,0(R2)            MOVE DSNAME
*        MVI   VOLSER,C'?'             LOCATE WILL BE DONE LATER  <38J>
         LA    R10,NEXTLINE
PAGEDS2  LA    R2,44(,R2)              NEXT DSN
         BCT   R1,PAGEDS1
*ENDLOOP
         BAL   R14,SPACE1              BLANK LINE
         L     R14,4(R13)
         BR    R14
         EJECT ,
*----------------------------------------------------------------------
*        TSO/E, DFDSS, HSM, RACF, VTAM
*----------------------------------------------------------------------
PRODUCTS ST    R14,4(R13)
         STRING 'PRODUCTS:',INTO=LINE
         LA    R10,NEXTLINE
         AGO   .XPROD    <38J>  SKIP CVTTVT
*
         L     R7,CVTTVT               TSO VECTOR TABLE
         USING TSVT,R7
         STRING '  TSO/E LEVEL:  ',TSVTLVER,'.',TSVTLREL,'.',TSVTLMOD, X
               4X,(ZENVIR,8),INTO=LINE
         BAL   R14,SPACE2              BLANK LINE
*
         MVC   WK256(2+2),=Y(1,80)             BLDL HDR
         MVC   WK256+4(8),=C'ADRRELVL'         EPNAME
         BLDL  0,WK256                 ISSUE BLDL
         LTR   R15,R15
         BNZ   DFDSS8                  NOT FOUND, JUMP
         LOAD  DE=WK256+4              LOAD ADRRELVL, IF PRESENT
         LR    R1,R0                   PASS EP ADDR
         STRING '  DF/DSS LEVEL: ',((R1),H,L),'.',(2(R1),FL1,L),'.', XXX
               (3(R1),FL1,L),INTO=LINE
         DELETE DE=WK256+4             I'M DONE WITH IT
         BAL   R14,SPACE2              BLANK LINE
.XPROD   ANOP              <38J>
*
VTAM00   L     R8,PSAATCVT             ADDR OF VTAM CVT
         LTR   R8,R8
         BZ    VTAM99
*8       STRING '  VTAM LEVEL:   ',((R8),4),8X,((R8),,X),INTO=LINE
         STRING '  VTAM ACTIVE',INTO=LINE                         <38J>
         BAL   R14,SPACE2              BLANK LINE
VTAM99   DS    0H
         L     R14,4(R13)
         BR    R14
         EJECT ,
*----------------------------------------------------------------------
*        UCB TABLE
*----------------------------------------------------------------------
DEVICES  ST    R14,4(R13)
         STRING 'ON-LINE DEVICES:',INTO=LINE
         BAL   R14,SPACE2              BLANK LINE
         STRING '  CUU  UCBTYP    UNITNAME  VOLSER  STATUS',INTO=LINE
         BAL   R14,SPACE2              BLANK LINE
         XC    WK256(100),WK256        CLEAR WORK AREA
         MVC   WK256(4),CVTILK2        @ 1ST UCB HWORD PTR   <38J>
*LOOP
DEVGET   DS    0H                                            <38J>
*                                                            <38J>
*8V_GET  LA    R14,WK256               WORK AREA FOR UCB SCAN RTN.
*8       LA    R15,CVTPTR              POINT TO X'00' IF CLASS=ALL
*8       LA    R0,TENWORDS+12          RETURNED UCB ADDRESS
*8       STM   R14,R0,TENWORDS         BUILD PARM LIST
*8       OI    TENWORDS+8,X'80'        MARK END OF LIST
*8       LA    R1,TENWORDS             PARAMETER LIST
*8       L     R15,CVTUCBSC            UCB SCAN ROUTINE
*8       BALR  R14,R15                 CALL THE UCBSCAN ROUTINE
*8       LTR   R15,R15
*8       BNZ   DEVEOD
*8       L     R5,TENWORDS+12          LOAD R1 WITH ADDRESS OF UCB
*                                                             <38J>
         L     R14,WK256               @ UCB OR END-OF-TABLE  <38J>
         CLC   =X'FFFF',0(R14)         END OF UCB LOOKUP TBL? <38J>
         BE    DEVEOD                  YES, BR                <38J>
         SLR   R5,R5                                          <38J>
         ICM   R5,3,0(R14)             @ UCB                  <38J>
         LA    R14,2(,R14)             @ NEXT UCB PTR         <38J>
         ST    R14,WK256               FOR NEXT TIME          <38J>
*                                                             <38J>
         TM    UCBSTAT,UCBONLI         ONLINE UNITS ONLY
         BNO   DEVGET
*
         MVC   VOLMOUNT,BLANKS
         MVC   UNITNAME+8(4),UCBTYP    DEVICE TYPE
         BAL   R14,GETUNIT             GET UNITNAME
DEVVOL   LA    R1,BLANKS               NO VOLSER UNLESS TAPE/DASD
         CLI   UCBTBYT3,UCB3TAPE       TAPE?
         BE    DEVVOL2                 YES, JUMP
         CLI   UCBTBYT3,UCB3DACC       DASD?
         BNE   DEVPUN                  NO, JUMP
DEVVOL2  CLI   UCBVOLI,C' '            VALID VOLSER?
         BNH   DEVPUN                  NO, JUMP
         LA    R1,UCBVOLI              VOLSER FOR TAPE/DASD
         CLI   UCBTBYT3,UCB3DACC       DASD?
         BNE   DEVPUN                  NO, JUMP
*8       MVC   VOLMOUNT(3),=C'SMS'     YES, SHOW IT
*8       TM    UCBFL5,UCBSMS           SMS VOL?
*8       BO    DEVPUN                  YES, JUMP
         MVC   VOLMOUNT,=C'PRIVATE'    USE=PRIVATE
         TM    UCBSTAB,UCBBPRV         CHECK USE
         BO    DEVPUN
         MVC   VOLMOUNT,=C'PUBLIC '    USE=PUBLIC
         TM    UCBSTAB,UCBBPUB         CHECK USE
         BO    DEVPUN
         MVC   VOLMOUNT,=C'STORAGE'    USE=STORAGE
DEVPUN   DS    0H
         STRING 2X,UCBNAME,2X,(UCBTYP,4,X),2X,UNITNAME,2X,((R1),6),  XXX
               2X,VOLMOUNT,INTO=LINE
         LA    R10,NEXTLINE
         B     DEVGET
*ENDLOOP
DEVEOD   BAL   R14,SPACE1              BLANK LINE
         L     R14,4(R13)
         BR    R14                        UCB
         DROP  R5                      UCB
         EJECT ,
*----------------------------------------------------------------------
*        SUB-SYSTEMS AND FUNCTIONS PROCESSED
*----------------------------------------------------------------------
SUBSYSTM ST    R14,4(R13)
         STRING 'SUB-SYSTEM VECTOR TABLE:',INTO=LINE
         BAL   R14,SPACE2              BLANK LINE
         L     R1,CVTJESCT             JES COMM TABLE
         L     R5,JESSSCT-JESCT(,R1)   FIRST JSCVT
         USING SSCT,R5
*LOOP
SUBSYS22 MVC   NEXTLINE,BLANKS
         ICM   R6,15,SSCTSSVT          SUB-SYSTEM VECTOR TABLE
         BZ    SUBSYS70                INACTIVE SUB-SYSTEM
         USING SSVT,R6
         SLR   R1,R1                   FIRST FUNCTION CODE
         LA    R2,256                  MAX NUMBER OF FUNCTIONS
*--LOOP
SUBSYS30 LA    R14,SSVTFCOD(R1)        FUNCTION BYTE
         LA    R1,1(,R1)               NEXT FUNCTION CODE
         CLI   0(R14),0
         BE    SUBSYS35
         STRING (NEXTLINE,,T),1X,((R1),,L0),INTO=NEXTLINE
SUBSYS35 BCT   R2,SUBSYS30
*--ENDLOOP
SUBSYS70 DS    0H
         STRING 2X,SSCTSNAM,1X,(SSCTSSVT,,X),NEXTLINE,INTO=LINE
         LA    R10,NEXTLINE
         ICM   R5,15,4(R5)             NEXT SSCVT
         BNZ   SUBSYS22
*ENDLOOP
         BAL   R14,SPACE1              BLANK LINE
         L     R14,4(R13)
         BR    R14
         EJECT ,
*----------------------------------------------------------------------
*        ACTIVE LPA Q
*----------------------------------------------------------------------
LPACTIV  ST    R14,4(R13)
         L     R5,CVTQLPAQ             ACTIVE LPA QUEUE
         ICM   R5,B'1111',0(R5)        FIRST LPDE ON QUEUE
         BZ    LPACT99                 EMPTY QUEUE, SKIP SEARCH
*
         USING LPDE,R5
         STRING 'ACTIVE LPA QUEUE:',INTO=LINE
         BAL   R14,SPACE2
         STRING 9X,'@LPDE   ',                                         +
               1X,'LPDECHN ',                                          +
               1X,'LPDERBP ',                                          +
               1X,'LPDENAME',                                          +
               1X,'LPDENTP ',                                          +
               1X,'LPDEXLP ',                                          +
               1X,'USE ',                                              +
               1X,'AT',                                                +
               1X,'A2',                                                +
               1X,'LPDEXTLN',                                          +
               1X,'LPDEXTAD',                                          +
               1X,'MAJORNAM',                                          +
               INTO=LINE
         BAL   R14,SPACE2
*
*
LPACT11  TM    LPDEATTR,LPDEMIN        MINOR LPDE?
         BO    LPACT12                 YES, JUMP
*
         STRING ' MAJLPDE',                                            +
               1X,((R5),,X),                                           +
               1X,(LPDECHN,,X),                                        +
               1X,(LPDERBP,,X),                                        +
               1X,LPDENAME,                                            +
               1X,(LPDENTP,,X),                                        +
               1X,(LPDEXLP,,X),                                        +
               1X,(LPDEUSE,,X),                                        +
               1X,(LPDEATTR,,X),                                       +
               1X,(LPDEATT2,,X),                                       +
               1X,(LPDEXTLN,,X),                                       +
               1X,(LPDEXTAD,,X),                                       +
               1X,(LPDENAME-LPDE(R5),8),                               +
               INTO=LINE
         B     LPACT14
*
LPACT12  DS    0H                      MINOR LPDE
         ICM   R2,15,LPDEXLP           @ ASSOCIATED MAJOR LPDE
         BZ    LPACT14
         STRING ' MINLPDE',                                            +
               1X,((R5),,X),                                           +
               1X,(LPDECHN,,X),                                        +
               1X,(LPDERBP,,X),                                        +
               1X,LPDENAME,                                            +
               1X,(LPDENTP,,X),                                        +
               1X,(LPDEXLP,,X),                                        +
               1X,(LPDEUSE,,X),                                        +
               1X,(LPDEATTR,,X),                                       +
               1X,(LPDEATT2,,X),                                       +
               1X,(LPDEXTLN,,X),                                       +
               1X,(LPDEXTAD,,X),                                       +
               1X,(LPDENAME-LPDE(R2),8),                               +
               INTO=LINE
*
LPACT14  DS    0H
         LA    R10,NEXTLINE
         ICM   R5,B'1111',LPDECHN      NEXT LPDE ADDR
         BNZ   LPACT11                 NO FINISHED YET, LOOP
*
LPACT99  DS    0H
         BAL   R14,SPACE2
         L     R14,4(R13)
         BR    R14
         EJECT ,
*----------------------------------------------------------------------
*        SVC TABLE
*----------------------------------------------------------------------
SVCTABLE ST    R14,4(R13)
         STRING 'SVC TABLE:',INTO=LINE
         BAL   R14,SPACE2              BLANK LINE
         L     R5,CVTABEND             SECONDARY CVT
         USING SCVTSECT,R5
         LA    R1,088*8                MULT BY 8 (LEN OF SVC ENTRY)
         A     R1,SCVTSVCT             CHANGE OFFSET TO ADDRESS
         MVC   SVC88,0(R1)             SAVE FOR LATER
         L     R5,SCVTSVCT             SVC TABLE
         LA    R8,00256/2
         OI    K1+L'K1-1,15            INIT PACKED CTR
*LOOP
SVCTAB3  ZAP   K2,K1
         AP    K2,=P'1'
         L     R1,0(,R5)               SVC EP ADDR
         BAL   R14,CSVQUERY            GET EP NAME
         MVC   EP1,EP2                 PASS EP NAME
         L     R1,8(,R5)               SVC EP ADDR
         BAL   R14,CSVQUERY            GET EP NAME
         STRING (K1,P),2X,(0(R5),4,X),2X,(4(R5),4,X),1X,EP1,       XXXXX
               (K2,P),2X,(8(R5),4,X),2X,(12(R5),4,X),1X,EP2,INTO=LINE
         LA    R10,NEXTLINE
         AP    K1,=P'2'
         LA    R5,8+8(,R5)
         BCT   R8,SVCTAB3
*ENDLOOP
         BAL   R14,SPACE1              BLANK LINE
         L     R14,4(R13)
         BR    R14
         EJECT ,
*----------------------------------------------------------------------
*        LIST LNKLSTXX LIBRARIES
*----------------------------------------------------------------------
LINKLIST ST    R14,4(R13)
         L     R6,CVTLINK              SYS1.LINKLIB DCB
         ICM   R6,B'0111',DCBDEBA-IHADCB(R6) DEB ADDRESS
         LA    R1,=C'LNKAUTH=LNKLST'
         TM    DEBFLGS1-DEBBASIC(R6),DEBAPFIN    AUTH=YES?
         BO    *+8                     YES, JUMP
         LA    R1,=C'LNKAUTH=APFTAB'
         AGO   .XLNKL                                 <38J>
         L     R7,CVTLLTA              LINK LIST TABLE
         USING LLT,R7
         STRING 'LINK-LIST: ',(CVTLLTA,,X),                            X
               (LLTNO,F,R9B),' ENTRIES',6X,((R1),14),INTO=LINE
         BAL   R14,SPACE2              BLANK LINE
         L     R8,LLTNO                # OF ENTRIES
         LA    R9,LLTENTRY             LINK LIST TABLE ENTRY
         USING LLTENTRY,R9
*LOOP
LNKLST41 MVC   LINE,BLANKS
         MVC   DSNAME,LLTDSNAM         MOVE DSNAME TO UNPROTECTED STRGE
         MVI   VOLSER,C'?'             $LOCATE REQUIRED
         MVI   YYMMDD,C'?'             $OBTAIN REQUIRED
         MVI   CATUNCAT,C'-'           APF-LIST SCAN REQUIRED
         LA    R10,NEXTLINE
         LA    R9,LLTNEXT              NEXT ENTRY
         BCT   R8,LNKLST41
         DROP  R7,R9                       <38J>
.XLNKL   ANOP                              <38J>
*ENDLOOP
         BAL   R14,SPACE1              BLANK LINE
         L     R14,4(R13)
         BR    R14
         EJECT ,
*----------------------------------------------------------------------
*        LIST LPALSTXX LIBRARIES
*----------------------------------------------------------------------
LPALIST  ST    R14,4(R13)
         AGO   .XLPAL                            <38J>
         TM    CVTDCB,CVTMVSE          XA/ESA?
         BZ    LPALST99                NO, JUMP
         L     R1,CVTSMEXT             STORAGE MAP EXTENSION
         L     R7,CVTEPLPS-CVTVSTGX(,R1)  LPA TABLE
         USING LLT,R7                                <38J>
         STRING 'LPA-LIST: ',((R7),,X),                                X
               (LLTNO,F,R9B),' ENTRIES.',INTO=LINE
         BAL   R14,SPACE2              BLANK LINE
         L     R8,LLTNO                # OF ENTRIES
         LA    R9,LLTENTRY-LLT(,R7)    FIRST LPA LIST TABLE ENTRY
         USING LLTENTRY,R9                          <38J>
*LOOP
LPALST42 MVC   LINE,BLANKS
         MVC   DSNAME,LLTDSNAM         MOVE DSNAME TO UNPROTECTED STRGE
         MVI   VOLSER,C'?'             $LOCATE REQUIRED
         MVI   YYMMDD,C'?'             $OBTAIN REQUIRED
         LA    R10,NEXTLINE
         LA    R9,LLTNEXT              NEXT ENTRY
         BCT   R8,LPALST42
*ENDLOOP
         BAL   R14,SPACE1              BLANK LINE
         DROP  R7,R9                   LLT,LLTENTRY <38J>
.XLPAL   ANOP                                       <38J>
LPALST99 L     R14,4(R13)
         BR    R14
         EJECT ,
*----------------------------------------------------------------------
*        LIST AUTHORIZED LIBRARIES
*----------------------------------------------------------------------
APFLIST  ST    R14,4(R13)
         L     R7,CVTAUTHL             APF TABLE
         LH    R8,0(,R7)               # OF ENTRIES
         STRING 'APF-LIST: ',(CVTAUTHL,,X),                            X
               ((R8),,R9B),' ENTRIES.',INTO=LINE
         BAL   R14,SPACE2              BLANK LINE
         ST    R10,APFTABLE            SAVE ADDR OF 1ST LINE
*LOOP
APFLIST3 MVC   LINE,BLANKS
         SLR   R1,R1
         IC    R1,2(,R7)
         SH    R1,=H'6'                VOLSER LENGTH
         STRING (9(R7),(R1)),INTO=DSNAME MOVE DSNAME
         MVC   VOLSER,3(R7)            MOVE VOLSER
         MVI   YYMMDD,C'?'             $OBTAIN REQUIRED
         MVI   CATUNCAT,C'?'           $LOCATE REQUIRED
         LA    R10,NEXTLINE
         LA    R7,7(R1,R7)             NEXT ENTRY
         BCT   R8,APFLIST3
*ENDLOOP
         EJECT ,
*----------------------------------------------------------------------
*------- TRI DE LA TABLE APF-LIST -------------------------------------
*----------------------------------------------------------------------
         LA    R0,L'LINE               LONGUEUR D'UN POSTE
         LR    R1,R10                  A(NEXTLINE)
         SLR   R1,R0                   DERNIER POSTE DE LA TABLE
         STM   R0,R1,APFTABLE+4        LONGUEUR, DERNIER POSTE
*LOOP
TRIZO    MVI   0(R13),0                ETAT INITIAL DE L'INDICATEUR
         L     R15,APFTABLE            DEBUT DE LA TABLE  N=1
         USING LINE,R15
         SLR   R1,R0                   LE DERNIER POSTE EST TRIE
*--LOOP
TRIZOC   CLC   DSNAME,DSNAME+L'LINE    (POSTE N) GT (POSTE N+1) ?
         BNH   TRIZOH                  SI NON, BRANCH
         XC    LINE,NEXTLINE           SI (POSTE N+1) LT (POSTE N)
         XC    NEXTLINE,LINE            RENVERSER LES
         XC    LINE,NEXTLINE             DEUX POSTES.
         MVI   0(R13),8                NOTER LE DECLASSEMENT
TRIZOH   BXLE  R15,R0,TRIZOC           FAIRE N=N+1
*--ENDLOOP
TRIZON   CLI   0(R13),0                Y-A-T-IL EU UN DECLASSEMENT ?
         BNE   TRIZO                   SI OUI, REFAIRE UN PASSAGE
*ENDLOOP
         DROP  R15                     LINE
         BAL   R14,SPACE1              BLANK LINE
         L     R14,4(R13)
         BR    R14
         EJECT ,
*----------------------------------------------------------------------
*        DISPLAY TCB TREE AND RB CHAINS
*----------------------------------------------------------------------
TCBTREE  ST    R14,4(R13)
         L     R4,JSTCB                THE JOB STEP TCB
         STRING 'TCB TREE AND RB CHAINS:',INTO=LINE
         BAL   R14,SPACE2              BLANK LINE
         STRING 4X,'TCB ADDRESS',19X,INTO=LINE,                        X
               'PROGRAM     IC    STAB  CDFLGS    DDNAME'
         BAL   R14,SPACE2              BLANK LINE
         SLR   R3,R3                   INDENTATION INDEX
*LOOP
TREE100  STCM  R4,B'0111',DWD          STORE TCB ADDR
         STRING (BLANKS,4(R3)),(DWD,3,X),INTO=LINE
         L     R5,TCBRBP               POINT TO TOP RB
         LA    R9,WK256                START OF RB TABLE
*
*        BUILD RB TABLE
*--LOOP
TREE110  ICM   R5,B'1000',CVTPTR       CLEAN UP HI-ORDER BYTE
         SH    R5,=Y(RBBASIC-RBPREFIX) POINT TO RBPREFIX
         USING RBPREFIX,R5
         ST    R5,0(,R9)               STORE RB ADDRESS
         TM    RBSTAB2,RBTCBNXT        CHECK FOR END OF CHAIN
         L     R5,RBLINK               POINT TO PREVIOUS RB (OR TCB)
         LA    R9,4(,R9)               BUMP UP TO NEXT TABLE ENTRY
         BZ    TREE110                 JUMP IF RB FOR 1ST ATTACHED PGM
*--ENDLOOP
*
*        PROCESS RB TABLE BACKWARDS
*--LOOP
TREE200  SH    R9,=H'4'                PREVIOUS ENTRY IN RB TABLE
         L     R5,0(,R9)               LOAD RB ADDRESS
         CLI   RBSTAB1,RBFTPRB         IS THIS A PRB?
         BNE   TREE280                 NO, IGNORE IT
         TM    RBCDFLGS,RBCDSYNC       CHECK FLAGS
         BO    TREE260                 JUMP IF IT IS A SYNCH PRB
         L     R1,RBCDE                POINT TO CDE/LPDE
         STRING INTO=(LINE+30,L'LINE),4X,                             XX
               (CDNAME-CDENTRY(R1),8),4X,      PGM NAME               XX
               (RBINTCOD,2,X),4X,      INTERRUPT CODE <38J>           XX
               (RBSTAB,2,X),4X,        STATUS BYTE                    XX
               (RBCDFLGS,,X)           FLAGS
*8             (RBWLIC+3,1,X),         IC
         B     TREE270
TREE260  MVC   DWD,RBGRS15             PICK UP ENTRY POINT ADDRESS
         NI    DWD+3,X'FE'             SET BIT 31 TO ZERO
         STRING INTO=(LINE+30,L'LINE),4X,                             XX
               (DWD,4,X),4X,           EP ADDRESS                     XX
               (RBINTCOD,2,X),4X,      INTERRUPT CODE <38J>           XX
               (RBSTAB,2,X),4X,                                       XX
               (RBCDFLGS,,X)
*8             (RBWLIC+3,1,X),
*
TREE270  LA    R10,NEXTLINE            NEXT LINE
TREE280  LA    R0,WK256                CHECK FOR END OF CHAIN
         CR    R9,R0                   CHECK FOR END OF CHAIN
         BH    TREE200                 LOOP THROUGH RB TABLE
*--ENDLOOP
         BAL   R14,SCANTCB             GET NEXT TCB
         BNZ   TREE100                 PROCESS NEXT TCB
*ENDLOOP
         BAL   R14,SPACE1              BLANK LINE
         L     R14,4(R13)
         BR    R14
         EJECT ,
*----------------------------------------------------------------------
*        DISPLAY JPAQ (CDE CHAIN)
*----------------------------------------------------------------------
JPAQ     ST    R14,4(R13)
         L     R4,JSTCB                THE JOB STEP TCB
         STRING 'JPAQ:',INTO=LINE
         BAL   R14,SPACE2              BLANK LINE
         L     R5,TCBJPQ               POINT TO FIRST CDE IN CHAIN
         USING CDENTRY,R5
         STRING '   NAME     ENTPT     USE ',                          X
               ' ATTRB SP ATTR ATTR2',                                 X
               '  MAJ-CDE    LENGTH   LOAD-PNT',INTO=LINE
         BAL   R14,SPACE2              BLANK LINE
*LOOP
JPAQ21   L     R6,CDXLMJP              POINT TO XL (OR MAJOR CDE)
         TM    CDATTR,CDMIN            CHECK ATTRIBUTES
         BO    JPAQ25                  JUMP IF THIS IS A MINOR CDE
         USING XTLST,R6
         STRING 3X,CDNAME,1X,(CDENTPT,,X),2X,(CDUSE,,X),3X,            X
               (CDATTRB,,X),2X,(CDATTR,,X),4X,           <38J>         X
               (CDATTR2,,X),14X,(XTLMSBLN,,X),3X,(XTLMSBAD,,X),        X
               INTO=LINE
*8             (CDSP,,X)
         B     JPAQ29
JPAQ25   DS    0H
         STRING 3X,CDNAME,1X,(CDENTPT,,X),9X,(CDATTRB,,X),6X,          X
               (CDATTR,,X),4X,(CDATTR2,,X),3X,(CDNAME-CDENTRY(R6),8),  X
               INTO=LINE
JPAQ29   LA    R10,NEXTLINE            NEXT LINE
         ICM   R5,B'1111',CDCHAIN      CHECK FOR END OF CHAIN
         BNZ   JPAQ21                  LOOP THROUGH RB TABLE
*ENDLOOP
         DROP  R5,R6                   CDE, XTLST
         BAL   R14,SPACE1              BLANK LINE
         L     R14,4(R13)
         BR    R14
         DROP  R3                      CVT
         EJECT ,
*----------------------------------------------------------------------
*        DISPLAY LOAD-LISTS
*----------------------------------------------------------------------
LOADLIST ST    R14,4(R13)
         L     R4,JSTCB                THE JOB STEP TCB
         STRING 'LOAD-LIST:',INTO=LINE
         LA    R10,NEXTLINE
         SLR   R3,R3                   INDENTATION INDEX
*LOOP
LOADL11  ICM   R9,B'1111',TCBLLS       POINT TO LAST LLE IN CHAIN
         BZ    LOADL80                 SKIP THIS TCB IF NO LLE CHAIN
         USING LLE,R9
*
         BAL   R14,SPACE1              BLANK LINE
         STCM  R4,B'0111',DWD          STORE TCB ADDR
         L     R5,TCBRBP               TOP RB
         USING RBBASIC,R5
         TM    RBCDFLGS,RBCDSYNC       CHECK FLAGS
         BNO   *+8                     JUMP IF IT IS NOT A SYNCH PRB
         ICM   R5,B'0111',RBLINK+1     POINT TO PREVIOUS RB IF SYNCH RB
         L     R5,RBCDE                POINT TO CDE/LPDE
         USING CDENTRY,R5
         STRING '  TCB ',(DWD,3,X),'  PGM ',CDNAME,INTO=LINE
         LA    R10,NEXTLINE
*--LOOP
LOADL70  STCM  R9,B'0111',DWD          STORE LLE ADDR
         L     R5,LLECDPT              CDE PTR
         STRING '    LLE ',(DWD,3,X),2X,CDNAME,2X,(CDENTPT,,X),2X,    XX
               (LLECOUNT,,X),1X,(LLESYSCT,,X),INTO=LINE
         LA    R10,NEXTLINE
         ICM   R9,B'1111',LLECHN       CHECK FOR END OF CHAIN
         BNZ   LOADL70                 LOOP THROUGH LOAD LIST
*--ENDLOOP
LOADL80  BAL   R14,SCANTCB             NEXT TCB IN TREE
         BNZ   LOADL11
*ENDLOOP
         BAL   R14,SPACE1                                         <38J>
         L     R14,4(R13)
         BR    R14
         EJECT ,
*----------------------------------------------------------------------
*        CLOSE SYSPRINT                                           <38J>
*----------------------------------------------------------------------
CLOSE    ST    R14,4(R13)
         STRING 'DONE',INTO=LINE
         BAL   R14,SPACE2
         TM    DY8DCB+(DCBOFLGS-IHADCB),DCBOFOPN OPENED?
         BNO   GOBACK
         MVC   DY8CLOSE(L$CLOSE),MODLCLOS        COPY CLOSE
         CLOSE DY8DCB,MF=(E,DY8CLOSE)            CLOSE SYSPRINT
         L     R14,4(R13)
         BR    R14
*----------------------------------------------------------------------
GOBACK   L     R14,0(R13)
         XR    R15,R15
         BR    R14
*----------------------------------------------------------------------
         EJECT ,
*----------------------------------------------------------------------
*        SCAN LPA QUEUES TO LOCATE EPNAME FOR ADDR IN (R1)
*----------------------------------------------------------------------
CSVQUERY MVC   EP2,BLANKS              CLEAR OUTPUT AREA
         AGO   .XCSVQ             <38J>
         CL    R1,SVC88                THIS SVC USED?
         BE    CSVQRY88                NO, GOBACK
         LA    R1,0(,R1)               CLEAN-UP AMODE BIT
         USING CVTMAP,R3
*
*        SCAN ACTIVE LPA QUEUE (MLPA/FLPA)
*
         L     R2,CVTQLPAQ             ACTIVE LPA QUEUE
         ICM   R2,B'1111',0(R2)        FIRST LPDE ON QUEUE
         BZ    CSVQRY20                EMPTY QUEUE, SKIP SEARCH
         USING LPDE,R2
         MVI   EP2,C'A'                ACTIVE LPA Q
*LOOP
CSVQRY11 L     R15,LPDENTP             ENTRY POINT
         LA    R15,0(,R15)             CLEAN UP AMODE BIT
         CR    R1,R15                  IS THIS MY ENTRY POINT?
         BE    CSVQRY82                MODULE FOUND, JUMP
         ICM   R2,B'1111',LPDECHN      NEXT LPDE ADDR
         BNZ   CSVQRY11                NO FINISHED YET, LOOP FURTHER
*ENDLOOP
*        SCAN PAGEABLE LPA QUEUE (PLPA)
*
CSVQRY20 L     R2,CVTLPDIA             FIRST LPDE
         USING LPDE,R2
         MVI   EP2,C'P'                PAGEABLE LPA Q
*LOOP
CSVQRY21 L     R15,LPDENTP             ENTRY POINT
         LA    R15,0(,R15)             CLEAN UP AMODE BIT
         CR    R1,R15                  IS THIS MY ENTRY POINT?
         BE    CSVQRY82                MODULE FOUND, JUMP
         TM    LPDEATTR,LPDEMIN        MINOR LPDE?
         BO    CSVQRY22                YES, IGNORE
         LM    R15,R0,LPDEXTLN         LENGTH/LOAD ADDR
         CR    R0,R1
         BH    CSVQRY22                OUTSIDE BOUNDARIES, JUMP
         AR    R0,R15
         CR    R0,R1
         BH    CSVQRY82                MODULE FOUND, JUMP
CSVQRY22 LA    R2,LPDEXTAD+4           BUMP LPDE ADDR
         CLI   LPDENAME,X'FF'          END OF LPA DIRECTORY?
         BNE   CSVQRY21                NO, LOOP FURTHER
*ENDLOOP
         MVC   EP2+2(4),=C'*NUC'       EP FOUND IN THE NUCLEUS
         MVI   EP2,C'N'                NUCLEUS
         L     R2,CVTSMEXT             STORAGE MAP EXTENSION
         USING CVTVSTGX,R2
         CL    R1,CVTRWNS              NUC?
         BL    CSVQRY61                NO, JUMP
         CL    R1,CVTERWNE             NUC?
         BLR   R14                     YES, GOBACK
*
CSVQRY61 MVC   EP2+2(5),=C'*FLPA'      EP FOUND IN FIXED LPA
         MVI   EP2,C'F'                FIXED LPAQ
         CL    R1,CVTFLPAS             FLPA (BELOW)
         BL    CSVQRY62                NO, JUMP
         CL    R1,CVTFLPAE             END OF FLPA (BELOW)
         BLR   R14                     YES, GOBACK
CSVQRY62 CL    R1,CVTEFLPS             FLPA (ABOVE)
         BL    CSVQRY63                NO, JUMP
         CL    R1,CVTEFLPE             END OF FLPA (ABOVE)
         BLR   R14                     YES, GOBACK
*
CSVQRY63 MVC   EP2+2(5),=C'*MLPA'      EP FOUND IN FIXED LPA
         MVI   EP2,C'F'                MODIFIED LPAQ
         CL    R1,CVTMLPAS             MLPA (BELOW)
         BL    CSVQRY64                NO, JUMP
         CL    R1,CVTMLPAE             END OF MLPA (BELOW)
         BLR   R14                     YES, GOBACK
CSVQRY64 CL    R1,CVTEMLPS             MLPA (ABOVE)
         BL    CSVQRY71                NO, JUMP
         CL    R1,CVTEMLPE             END OF MLPA (ABOVE)
         BLR   R14                     YES, GOBACK
*
CSVQRY71 L     R2,CVTGDA               POINT TO GDA
         USING GDA,R2
         MVC   EP2+2(5),=C'*CSA '      EP FOUND IN CSA
         MVI   EP2,C'C'                CSA
         L     R0,GDACSA               CSA (BELOW)
         CLR   R1,R0                   WITHIN CSA?
         BL    CSVQRY72                NO, JUMP
         AL    R0,GDACSASZ             END OF CSA (BELOW)
         CLR   R1,R0                   WITHIN CSA?
         BLR   R14                     YES, GOBACK
CSVQRY72 L     R0,GDAECSA              CSA (ABOVE)
         CLR   R1,R0                   WITHIN CSA?
         BL    CSVQRY73                NO, JUMP
         AL    R0,GDAECSAS             END OF CSA (ABOVE)
         CLR   R1,R0                   WITHIN CSA?
         BLR   R14                     YES, GOBACK
CSVQRY73 MVC   EP2,BLANKS              RETURN BLANK NAME
         BR    R14
*
         USING LPDE,R2
CSVQRY82 MVC   EP2+2(8),LPDENAME       PASS EP NAME
         BR    R14
         DROP  R2,R3                   LPDE, CVT
CSVQRY88 MVI   EP2,C'-'                SVC IS NOT IN SERVICE
.XCSVQ   ANOP                  <38J>
         BR    R14
         EJECT ,
*----------------------------------------------------------------------
*        CONVERT UCBTYP TO UNITNAME
*----------------------------------------------------------------------
GETUNIT  ST    R14,8(R13)
         CLC   UNITNAME+12(4),UNITNAME+8   SAME DEVICE TYPE?
         BER   R14                     YES, UNITNAME IS OK ALREADY
         MVC   UNITNAME+12(4),UNITNAME+8   SAVE DEVICE TYPE
         AGO   .XGUNT                  <38J>
         MVI   TENWORDS+20,X'01'       RETURN A LOOK-UP VALUE (BIT7)
         MVI   TENWORDS+21,X'00'       CLEAR UNUSED BYTE
         LA    R14,UNITNAME            UNITNAME+DEVTYPE
         LA    R15,TENWORDS+20         FLAGS
         STM   R14,R15,TENWORDS
         OI    TENWORDS+4,X'80'        END-OF-LIST FLAG
         LA    R1,TENWORDS             PARM LIST ADDRESS
         L     R15,IEFEB4UV            LOAD ROUTINE ADDRESS
         BALR  R14,R15                 GET UNIT NAME
         LTR   R15,R15
         BNZ   GETUNIT6                BAD RETURN CODE, QUIT
         MVI   TENWORDS+20,X'20'       RETURN A UNIT NAME (BIT2)
         LA    R1,TENWORDS             PARM LIST ADDRESS
         L     R15,IEFEB4UV            LOAD ROUTINE ADDRESS
         BALR  R14,R15                 GET UNIT NAME
         LTR   R15,R15
         BZ    GETUNIT9                GOOD RETURN CODE, GOBACK
.XGUNT   ANOP                          <38J>
GETUNIT6 MVC   UNITNAME,BLANKS         CONVERSION DID NOT WORK
GETUNIT9 L     R14,8(R13)
         BR    R14
         EJECT ,
*----------------------------------------------------------------------
*        PRINT A BLANK LINE
*----------------------------------------------------------------------
SPACE2   LA    R10,NEXTLINE
SPACE1   MVC   LINE,BLANKS
         LA    R10,NEXTLINE
*----------------------------------------------------------------------
*  SHOWOUT - OUTPUT LINES THAT "STRING" HAS PRODUCED SO FAR       <38J>
*            USE QSAM "PUT" RATHER THAN ISPF "BRIF"
*----------------------------------------------------------------------
SHOWOUT  DS    0H
         PUSH  USING
         STM   R0,R15,DY8OUT           FREE UP SOME REGS
         BALR  R12,0
         USING *,R12
         LR    R8,R13                  SAVE DYN PTR
         USING DYN,R8
         LA    R9,DY8DCB               @ DCB
         USING IHADCB,R9
         DROP  R13                     DON'T CONFUSE ASSEMBLER
         LA    R13,DY8SA               GIVE PUT A TEMP SAVEAREA
*
         TM    DCBOFLGS,DCBOFOPN       DCB ALREADY OPENED?
         BO    OUTOPEN
*
         MVC   DY8DCB(L$DCB),DCBPRINT            COPY DCB
         MVC   DY8OPEN(L$OPEN),MODLOPEN          COPY OPEN
         OPEN  ((R9),(OUTPUT)),MF=(E,DY8OPEN)    OPEN SYSPRINT OUTPUT
         TM    DCBOFLGS,DCBOFOPN                 DID IT OPEN OK?
         BO    OPENOK
U100     ABEND 100,DUMP                          SYSPRINT OPEN FAILED
*
OPENOK   DS    0H
         LA    R0,LINES
         ST    R0,DY8PTRS              @ 1ST REC
         LA    R0,L$LINE
         ST    R0,DY8PTRS+4            L' REC
*
OUTOPEN  DS    0H
         LM    R5,R6,DY8PTRS           R5-R7: @REC, L'REC, @CURRENT
         LA    R7,LINE                 MOST RECENT LINE
         ST    R7,DY8PTRS              FOR NEXT TIME
         BCTR  R7,0                    DON'T WRITE CURRENT LINE
*
OUTPUT   DS    0H
         PUT   (R9),(R5)               WRITE SYSPRINT LINE
         BXLE  R5,R6,OUTPUT            UNTIL CAUGHT UP WITH STRING
*
         LM    R0,R15,DY8OUT           PUT REGS BACK AS NORMAL
         POP   USING
         BR    R14
*
DCBPRINT DCB   DDNAME=SYSPRINT,MACRF=PM,DSORG=PS,                      +
               RECFM=FB,LRECL=L$LINE,BLKSIZE=3200
L$DCB    EQU   *-DCBPRINT
*
MODLOPEN OPEN  (0,(OUTPUT)),MF=L
L$OPEN   EQU   *-MODLOPEN
*
MODLCLOS CLOSE (,),MF=L
L$CLOSE  EQU   *-MODLCLOS
         EJECT ,
*----------------------------------------------------------------------
*        TCB TREE SCAN SOUTINE
*----------------------------------------------------------------------
SCANTCB  ST    R14,8(R13)
         LR    R1,R4                   SAVE TCB ADDRESS
         L     R4,TCBLTC-TCB(,R4)      DAUGHTER
         LA    R3,1(,R3)               INDENTATION INDEX
*LOOP
SCANTCB2 LTR   R4,R4                   CHECK FOR END OF CHAIN
         BNZR  R14                     PASS VALID TCB ADDRESS
         L     R4,TCBNTC-TCB(,R1)      SISTER
         L     R1,TCBOTC-TCB(,R1)      MOTHER
         BCT   R3,SCANTCB2             INDENTATION INDEX
*ENDLOOP
         SR    R4,R4                   SET CC=0
         L     R14,8(R13)
         BR    R14                     GOBACK
         DROP
         EJECT ,
*----------------------------------------------------------------------
*----------------------------------------------------------------------
         STRING GENERATE               GENERATE LITERALS  <38J>
*----------------------------------------------------------------------
*----------------------------------------------------------------------
         EJECT ,
*----------------------------------------------------------------------
*----------------------------------------------------------------------
*        DYNAMIC STORAGE AREA
*----------------------------------------------------------------------
*----------------------------------------------------------------------
DYND     DSECT                         DYNAMIC STORAGE AREA
DYN      DS    18F                     SAVE AREA FOR MAINLINE
DWD      DS    D                       WORK AREA
TENWORDS DS    10F                     WORK AREA
JSTCB    DS    A(TCB)                  ADDRESS OF THE JOB-STEP TCB
MYTCB    DS    A(TCB)                  ADDRESS OF MY TCB
NUMLINES DS    F                       NUMBER OF LINES
APFTABLE DS    A(LINE,L'LINE,NEXTLINE) APF-LIST
BLANKS   DS    CL100                   A BUNCH OF BLANKS
VOLMOUNT DS    C'STORAGE'              VOLUME MOUNT ATTRIBUTE
IEFEB4UV DS    V(IEFEB4UV)             UNITNAME CONVERSION RTNE
UNITNAME DS    CL8,XL4,XL4             IEFEB4UV
K1       DS    PL4                     COUNTER
K2       DS    PL4                     COUNTER
EP1      DS    CL10                    SVC TABLE
EP2      DS    CL10                    SVC TABLE
SVC88    DS    A                       UNUSED SVC NUMBER
WK256    DS    XL256,2D                265-BYTE WORK AREA
ATTACHL  ATTACH SF=L
ECB1     DS    F,A(TCB)                COMMUNICATION ECB
         IECSDSL1 1                    F1-DSCB
         DS    XL5                     PADDING FOR OBTAIN
         WK$OUT ,                      SHOWOUT WORKAREAS <38J>
         DS    0F                                        <38J>
LINES    DS    5000CL100               LINES FOR BRIF
DYNL     EQU   *-DYN                   LENGTH OF DYNAMIC STORAGE AREA
         SPACE 2
         PRINT NOGEN                   SAVE PAPER
*----------------------------------------------------------------------
*        WORK AREA FOR RDJFCB
*----------------------------------------------------------------------
IHADCB   DCBD  DSORG=XE,DEVD=DA        IHADCB
OPENLIST DS    A(IHADCB,0)             OPEN LIST, DCB EXIT LIST
DYN24L   EQU   *-IHADCB
         SPACE 2
*----------------------------------------------------------------------
         DSECT
LINE     DS    CL100                   CURRENT LINE
L$LINE   EQU   *-LINE                                             <38J>
DSNAME   EQU   LINE+2,44
VOLSER   EQU   LINE+2+44+3,6
YYMMDD   EQU   LINE+2+44+3+6+3,6
CATUNCAT EQU   LINE+2+44+3+6+3+6+3,5   UNCAT
NEXTLINE DS    CL100                   NEXT LINE
         EJECT ,
*----------------------------------------------------------------------
*----------------------------------------------------------------------
*        MVS CONTROL-BLOCKS
*----------------------------------------------------------------------
*----------------------------------------------------------------------
* LOW STORAGE, PHYSICAL CONFIGURATION
*----------------------------------------------------------------------
         IHAPSA DSECT=YES              PREFIXED STORAGE AREA
*        IHAPCCAT DSECT=YES            PCCA VECTOR TABLE
         IHAPCCA DSECT=YES             PHYSICAL CONFIG. COMM. AREA
*----------------------------------------------------------------------
*----------------------------------------------------------------------
* COMMUNICATION VECTORS
*----------------------------------------------------------------------
*----------------------------------------------------------------------
         CVT   PREFIX=YES,DSECT=YES,LIST=NO
         IHASCVT DSECT=YES,LIST=NO     SECONDARY CVT
         IEFJESCT                      JES VECTOR TABLE
         IEFJSCVT                      SUB-SYSTEM COMM. VECTOR TABLE
         IEFJSSVT                      SUB-SYSTEM VECTOR TABLE
*----------------------------------------------------------------------
*----------------------------------------------------------------------
* REAL & VIRTUAL STORAGE MANAGEMENT
*----------------------------------------------------------------------
*----------------------------------------------------------------------
         IHAGDA                        GLOBAL DATA AREA
L$GDA    EQU    *-GDA
         IHALDA                        LOCAL  DATA AREA
         IHAPQE ,                      VSM PARTITION QUEUE ELEMENT<38J>
         IHASPQE ,                     VSM SUBPOOL QUEUE ELEMENT  <38J>
         IHADQE ,                      VSM DESCRIPTOR Q ELEMENT   <38J>
*----------------------------------------------------------------------
*        ILRASMVT DSECT=YES            AUXILIARY STRGE MGR VECTOR TABLE
ASMVT    DSECT                         ILRASMVT
ASMFLAG1 DS    X                       FLAGS 1
ASMFLAG2 DS    X,2X                    FLAGS 2
ASMQUICK EQU   X'08'                   QUICK START IPL
ASMSART  DS    V(SART)                 SWAP ACTIVITY REFERENCE TABLE
ASMPART  DS    V(PART)                 PAGE ACTIVITY REFERENCE TABLE
*----------------------------------------------------------------------
PART     DSECT                         ILRPART
PARTSIZE EQU   PART+04,4,C'X'          NUMBER OF ENTRIES IN THE PART
PARTDSNL EQU   PART+24,4,C'X'          ADDR OF DSN LIST
*----------------------------------------------------------------------
*----------------------------------------------------------------------
* TASK MANAGEMENT
*----------------------------------------------------------------------
*----------------------------------------------------------------------
         IHAASCB                       ADDRESS SPACE CONTROL BLOCK
         IKJTCB DSECT=YES,LIST=NO      TASK CONTROL BLOCK
         IKJRB  DSECT=YES              REQUEST BLOCK              <38J>
*----------------------------------------------------------------------
*----------------------------------------------------------------------
* CONTENTS MANAGEMENT
*----------------------------------------------------------------------
*----------------------------------------------------------------------
         IHALPDE ,                     LPA DIRECTORY ENTRY
         IHACDE ,                      CONTENTS DIRECTORY ENTRY
         IHALLE ,                      LOAD-LIST ELEMENT
         IHAXTLST ,                    EXTENT LIST
*----------------------------------------------------------------------
*----------------------------------------------------------------------
* IOS & DATA MANAGEMENT
*----------------------------------------------------------------------
*----------------------------------------------------------------------
         IEZDEB  LIST=NO               DATA EXTENT BLOCK
TIOT     DSECT
         IEFTIOT1                      TASK INPUT-OUTPUT TABLE
         IEFUCBOB LIST=NO,PREFIX=NO    UNIT CONTROL BLOCK
JFCB     DSECT
         IEFJFCBN LIST=NO              JOB FILE CONTROL BLOCK
*----------------------------------------------------------------------
*----------------------------------------------------------------------
* MISCELLANEOUS
*----------------------------------------------------------------------
         IEZBITS                       BIT0-BIT7
         YREGS                         REGISTER EQUATES
         END
@@
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB(SHOWMVS),DISP=SHR
//LKED.SYSPRINT DD   SYSOUT=A