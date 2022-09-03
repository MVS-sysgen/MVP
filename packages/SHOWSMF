//SHOWSMF  JOB (TSO),
//             'Install SHOWSMF',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*
// EXEC ASMFCL,PARM.ASM='LIST,LOAD,NODECK,STMT,NUM,BUF(MAX)',
//             MAC1='SYS1.AMODGEN'
//*
//ASM.SYSIN DD *
         MACRO
&NAME    PGM   &BASE=R12,&SAVE=$$SAVE,&START=$$START,&EOJ=$$EOJ,&RC=16
&NAME    CSECT
         B     14(0,R15)           BRANCH AROUND PROGRAM ID.
         DC    AL1(8)              CSECT NAME LENGTH.
         DC    CL8'&NAME'          CSECT IDENTIFIER.
         DC    X'FF'               FILLER.
         STM   R14,R12,12(R13)     STORE THE REGISTERS.
         BALR  &BASE,0             ESTABLISH ADDRESSABILITY.
         USING *,&BASE             TELL ASSEMBLER ABOUT BASE REGISTER.
R0       EQU   0                   REGISTER 0.
R1       EQU   1                   REGISTER 1.
R2       EQU   2                   REGISTER 2.
R3       EQU   3                   REGISTER 3.
R4       EQU   4                   REGISTER 4.
R5       EQU   5                   REGISTER 5.
R6       EQU   6                   REGISTER 6.
R7       EQU   7                   REGISTER 7.
R8       EQU   8                   REGISTER 8.
R9       EQU   9                   REGISTER 9.
R10      EQU   10                  REGISTER 10.
R11      EQU   11                  REGISTER 11.
R12      EQU   12                  REGISTER 12.
R13      EQU   13                  REGISTER 13.
R14      EQU   14                  REGISTER 14.
R15      EQU   15                  REGISTER 15.
         LA    R15,&SAVE           ADDRESS OF OUR SAVE AREA.
         ST    R13,4(R15)          BACKWARD SAVE AREA CHAIN.
         ST    R15,8(R13)          FORWARD SAVE AREA CHAIN.
         LR    R13,R15             POINT R13 TO CURRENT SAVE AREA.
         B     &START              BRANCH TO ENTRY CODE.
&EOJ     CH    R15,$$RC            IS RETURN CODE HIGHER THAN &RC?
         BNH   *+6                 YES - LETS ZERO R15.
         SR    R15,R15             ZERO OUT REGISTER 15.
         L     R13,&SAVE+4         POINT R13 TO PREVIOUS SAVE AREA.
         L     R14,12(R13)         RESTORE REGISTER 14.
         LM    R0,R12,20(R13)      RESTORE THE REGISTERS.
         BR    R14                 RETURN TO OS.
&SAVE    DS    18F                 OUR SAVE AREA.
$$RC     DC    H'&RC'              RETURN CODE.
$$START  DS    0H                  DEFAULT ENTRY POINT.
         MEND
*          DATA SET SHOWSMF    AT LEVEL 002 AS OF 10/22/77
*          DATA SET SHOWSMF    AT LEVEL 001 AS OF 04/27/77
*
* THE SHOWSMF PROGRAM WILL LOCATE, AND OBTAIN THE F1 DSCBS FOR
* THE SMF DATASETS AND CALCULATE THE PERCENTAGE USED OF EACH.
* IT MAY BE INVOKED VIA TSO OR BATCH..
*       K TRUE   26APR77
*
SHOWSMF  PGM
         LA     R0,LWORK           GET LENGTH OF WORK AREA
         ICM    R0,B'1000',=FL1'1' GET FROM SUBPOOL 1
         GETMAIN R,LV=(0)
         LR    R11,R1               SAVE ADDR OF WORK AREA
         USING WORKAREA,R11
         TITLE 'SHOWSMF - DATASET NAME ROUTINE'
SMCAMAN  EQU   X'23'
CVTSMCA  EQU   X'C4'
         SPACE 2
         LA    R2,16                (16)=POINTER TO ADDR OF CVT
         L     R2,0(R2)             (R2)=ADDR CVT
         LA    R2,CVTSMCA(R2)       (R2)=ADDR OF ADDR OF SMCA
         L     R2,0(R2)             (R2)=ADDR SMCA
         MVC   CURMAN,SMCAMAN(R2)   MOVE IN CURRENT MAN DATASET NAME
         SPACE
         EXTRACT MF=(E,EXTRACT)
         L     R1,ANSWER            (R1)=ADDR OF TSO FLAG FROM EXTRACT
         MVC   TSOFLAG,0(R1)        MOVE IN THE ANSWER TO OUR FLAG
         SPACE
         LA    R1,MSG1
         BAL   R10,PUTMSG           GO WRITE OUT THE HEADING
         MVC   DSN1,=CL44'SYS1.MANX'
         MVI   MAN,C'X'
         SPACE 2
         BAL   R9,OBTNRTN          LINK TO OBTAIN AND PRINT ROUTINE
         MVC   DSN1,=CL44'SYS1.MANY'
         MVI   MAN,C'Y'
         SPACE 2
         BAL   R9,OBTNRTN          LINK TO OBTAIN AND PRINT ROUTINE
         B     $$EOJ               EXIT STAGE LEFT
         TITLE 'SHOWSMF - OBTAIN AND PRINT ROUTINES'
*   READ DSCB1 AND GET SPACE
OBTNRTN  STM   R2,R9,OBTNSAVE     SAVE CALLER'S REGISTERS
         LOCATE INDS
         LTR   R15,R15              DID IT FIND VOL SER
         BNZ   ERR02                 NO WRITE ERROR MSG
         MVC   VOLS(6),BUF1+6      YES - MOVE IN VOL SER
CLEARIT  MVI   MSG2AREA,C' '
         MVC   MSG2AREA+1(L'MSG2AREA-1),MSG2AREA  CLEAR MSG AREA
OBTAIN1  OBTAIN SERCHCAM
         LTR   R15,R15             DID OBTAIN WORK
         BNZ   ERR03               NO - WRITE ERROR MSG
         CLI   BUF1,C'1'            IS IT F1 DSCB?
         BNE   ERR01                NOT F1
         SPACE
         CLC   LASTVOL,VOLS        HAVE WE SEEN THIS VOL BEFORE?
         BE    OKVOL                YES-> SKIP OBTAIN OF F4 DSCB
         MVC   LASTVOL,VOLS         NO-> REMEMBER HIM
         OBTAIN F4CAM              GO GET F4 DSCB
         LTR   R15,R15             ANY PROBLEMS
         BNZ   FCIERR1             YES-> GO GRIPE
         MVC   VOLTKCYL(2),BUF2+20    MOVE IN # TRACKS/CYL
OKVOL    LA    R9,XTNTS            POINT TO EXTENTS SAVE AREA
         LH    R6,BUF1+54          GET LAST RELATIVE TRK USED
         CLI   BUF1+56,X'00'       IS TRK UNUSED?
         BE    *+8                  YES
         LA    R6,1(R6)             NO, ANOTHER TRACK
         STH   R6,LASTTRK          SAVE LAST REL. TRK
         MVC   DSORG,BUF1+38       SAVE DSORG
         EJECT
GDSO1    SR    R2,R2                ZERO REG 2                    FCI
         IC    R2,BUF1+15           PICK UP NUMBER OF EXTENTS
         LA    R3,1                 SET EXTENT COUNTER
         SR    R4,R4                CLEAR R4 TO TRK ACCUMULATION
         LTR   R2,R2               NO XTNTS? - GDG PATTERN. DSCB
         BZ    VVALID               YES, BYPASS ACCUMULATE
         LA    R5,BUF1+61           POINT TO FIRST EXTENT
VXTLOOP  MVC   0(10,R9),0(R5)      MOVE EXTENT TO SAVE AREA
         LA    R9,10(R9)           BUMP TO NEXT SAVE AREA
         MVC   HWK1(2),6(R5)        MOVE HI-CYL TO HWD
         LH    R0,HWK1              LOAD
         MVC   HWK1(2),8(R5)        MOVE HI-TRK TO HWD
         LH    R1,HWK1              LOAD
         MVC   HWK1(2),2(R5)        MOVE LOW-CYL TO HWD
         SH    R0,HWK1              SUBTRACT
         MVC   HWK1(2),4(R5)        MOVE LOW-TRK TO HWD
         SH    R1,HWK1              SUBTRACT
         MH    R0,VOLTKCYL          CONVERT CYL TO TRK
         AR    R1,R0                GET TOTAL MINUS1
         LA    R4,1(R1,R4)          GET TOTAL AND ACCUMULATE
         CR    R3,R2                DONE LAST EXTENT?
         BE    VVALID               GO TO VVALID IF SO
         LA    R3,1(R3)             BUMP TO EXTENT COUNTER
         CH    R3,=H'4'             FOURTH EXTENT?
         BE    VXT4                   BRANCH IF SO
         CH    R3,=H'8'             EIGHTTH EXTENT?
         BE   VXT8
         LA    R5,10(R5)            ELSE BUMP EXTENT POINTER
         B     VXTLOOP              AND GO TO NEXT EXTENT
VXT4     MVC   VTOCCHHR(5),BUF1+91  POINT NEXT DSCH(F2 OR F3)
VXT4OBT  OBTAIN SEEKCAM
         CLI   BUF1+44,C'3'         IF IT F3 DSCB
         BE    VXT4F3               BRANCH IF SO
         MVC   VTOCCHHR(5),BUF1+135  ELSE ITS F2-POINT TO F3
         B     VXT4OBT
VXT4F3   LA    R5,BUF1+4            POINT TO FIRST EXTENT IN F3
         B     VXTLOOP              CONTINUE LOOP FOR SIZE
VXT8     LA    R5,BUF1+45           SKIP OVER F3 ID IN F3 DSCB
         B     VXTLOOP              CONTINUE LOOP FOR SIZE
VVALID   EQU   *
* CONVERT SPACE ALLOC AND EDIT IN PRINT LINE
         CVD   R4,WKD               CONVERT DEC. R4  #TRKS ALLOC
         MVC   MALLOC,MASKED2        MOVE MASKED FIELD IN
         ED    MALLOC,WKD+5          EDIT  # TRKS MSG2+5
         TM    DSORG,X'42'        IS IT PO OR PS
         BNZ   CVDU                  YES - BRANCH
         LR    R7,R4                INDICATE ALL USED
         B     CVDX                 BRANCH TO EXTENT CONVERSION
CVDU     LH    R7,LASTTRK            GET LAST RELATIVE TRACK
CVDX     LA     R7,1(R7)           FUDGE...MUST USE AT LEAST 1...  FCI
         CVD   R7,WKD               CONVERT DEC R7
         MVC   MUSED,MASKED2          MOVE IN MASKED FIELD
         ED    MUSED,WKD+5            EDIT # TRKD USED
*
         SR     R2,R2              CALCULATE % USED IF WE CAN      FCI
         MVC    MFULL,MASKED3      MOVE IN THE EDIT MASK           FCI
         LR     R3,R7              (R3)=(USED)                     FCI
         M      R2,=F'1000'        (R3)=(USED)*1000                FCI
         DR     R2,R4              (R3)=%USED*10                   FCI
         CVD    R3,WKD             AND CONVERT TO DECIMAL          FCI
         ED     MFULL,WKD+5        EDIT THE PATTERN                FCI
         MVC   MDSNAME,DSN1         MOVE DSN TO PRINT LINE
         MVI   MDASH,C'-'          MOVE IN THE DASH....
         CLC   MAN,CURMAN          IS THIS THE CURRENT MAN DATASET?
         BNE   DOWTO
         MVC   MINUSE,=C'***'
DOWTO    LA    R1,MSG2
         BAL   R10,PUTMSG          WRITE OUT LINE
         EJECT
OBTNRET  LM    R2,R9,OBTNSAVE     RELOAD CALLER'S REGISTERS
         BR    R9                  RETURN TO CALLER
         EJECT
RETC     B     $$EOJ
         SPACE 3
ERR01    WTO   MF=(E,EMSG01)
         B     $$EOJ
         SPACE 1
ERR02    MVC   MSG2AREA(L'ERMSG2),ERMSG2 LOCATE FAILED..NOT CATALOGED
         MVC   MSG2AREA+L'ERMSG2(20),DSN1 MOVE IN DSNAME
         LA    R1,MSG2
         BAL   R10,PUTMSG
         B     $$EOJ
* DATASET NAME NOT FOUND
ERR03    C     R15,=F'4'                OBTAIN ERR=4(VOL NOT MNTED)?
         BNE   ERR03A                   NO-> ASSUME NOT ON VOL
         MVC   MSG2AREA(L'ERMSG4),ERMSG4
         MVC   MSG2AREA+ERR04VOL(6),VOLS MOVE IN VOLSER            FCI
         MVC   MSG2AREA+L'ERMSG4(20),DSN1
         B     ERR03P
ERR03A   MVC   MSG2AREA(L'ERMSG9),ERMSG9
         MVC   MSG2AREA+ERR03VOL(6),VOLS MOVE IN VOLSER            FCI
         MVC   MSG2AREA+L'ERMSG9(20),DSN1
ERR03P   LA     R1,MSG2
         BAL    R10,PUTMSG              GO ISSUE THE MESSAGE
         B     OBTNRET
         SPACE
FCIERR1  MVC   MSG2AREA(L'FERRMSG1),FERRMSG1
         MVC   MSG2AREA+L'FERRMSG1(6),VOLS
         LA    R1,MSG2
         BAL   R10,PUTMSG
         MVI   MSG2AREA,C' '
         MVC   MSG2AREA+1(L'MSG2AREA-1),MSG2AREA
         B     OKVOL
         EJECT
*  I/O ROUTINES OPERATOR & TSO
*
* ENTER WITH WTO,WTOR LIST FORM POINTED TO BY R1
*    BAL  R10,PUTMSG
*
*  USES R14 FOR WORK
*
PUTMSG   DS    0H
         TM    TSOFLAG,X'80'       IS IT TSO?
         BO    TPUTIT              YES-> GO DO TPUT
         SVC   35                  NO-> USE WTO
         BR    R10
TPUTIT   DS    0H
         LH    R0,0(,R1)           GET LENGTH OF WTO MESSAGE
         S     R0,=F'4'            SUBTRACT OFF HEADER
         LA    R1,4(,R1)           BUMP MSG ADDRESS PAST HDR
         TPUT  (1),(0),R           DO THE TPUT
         BR    R10
         SPACE 2
         TITLE 'SHOWSMF - DATA AREAS'
ENDLIST  DS    0F
         DC    XL4'FF000000'
* SETUP CONSTANTS
VOLDSCTK DC    H'0'                NUM DSCH ON A TRK
VOLTKCYL DC    H'19'               19 TRACKS/CYL FOR 3330
VOLF4CHR DC    XL5'00'             CCHHR OF DSCB
VOLNO    DC    H'0'                NUM VOLUMES PROCESSED
PEXCTR   DC    H'0'                CTR FOR TATAL NUM FREE SPACE
LASTTRK  DC    H'0'
DSN1     DC    CL44' '              DATASET NAME
VOLS     DC    CL6' '                 VOLUME SER
*
DSORG    DC    X'00'
MAN      DC    C' '                WHICH MAN DATASET..?
CURMAN   DC    C' '                CURRENT MAN DATASET 'X' OR 'Y'
TSOFLAG  DC    X'00'               ='80' IF TSO. '00' IF NOT TSO
BLANKS   DC    CL6' '
BUF1     DS    0D
         DS    265C
BUF2     DS    0D
         DS    265C
F4NAME   DS    CL44
         ORG   F4NAME
         DC    44X'04'
LASTVOL  DC    CL6' '
MASKED2  DC    XL6'402020202120'   MASK FIELD FOR EDIT (PRNT 0)   FCI
MASKED3  DC    XL7'40202021204B20' MASK FIELD FOR EDIT OF %       FCI
         EJECT
* MESSAGE TO BE PRINTED
MSG1     WTO   'SHOWSMF - ALLOC  USED  %FULL IN-USE DSNAME ',          X
               ROUTCDE=(11),MF=L
*                        XXXXX  XXXXX  XXX.X   ***  SYS1.MANX
*               456789-123456789-123456789-123456789-123456789-12345678
*               9-123456789-123456789-123456789-123456789-0123456789
MSG2     WTO   'SHOWSMF -                                              X
                                               ',                      X
               ROUTCDE=(11),MF=L
         ORG   MSG2+13
MSG2AREA DS    CL66
         ORG   MSG2+12
MDASH    DS    CL1
         ORG   MSG2+12
MALLOC   DS    CL6
         ORG   MSG2+19
MUSED    DS    CL6
         ORG   MSG2+25
MFULL    DS    CL7
         ORG   MSG2+35
MINUSE   DS    CL3
         ORG   MSG2+40
MDSNAME  DS    CL44
         ORG
         SPACE 3
* ERROR MESSAGES
EMSG01   WTO   'SHOWSMF - CANT GET F1 DSCB',ROUTCDE=(11),MF=L
ERMSG2   DC    C'DSN IS NOT CATALOGED     - '
ERMSG4   DC    C'DISK        NOT MOUNTED  - '
ERR04VOL EQU   5
ERMSG8   DC    C'ERROR READING CATALOG    - '
ERMSG9   DC    C'DSN NOT ON DISK          - '
ERR03VOL EQU   16
FERRMSG1 DC    C'SHOWSMF - TRKS MAY BE WRONG-CANT GET F4 DSCB FOR VOL='
         EJECT
* CAMLST FOR DSCB3
SEEKCAM  CAMLST SEEK,VTOCCHHR,VOLS,BUF1
         SPACE 2
* CAMLST LOCATE DSN OV VOL SER
INDS     CAMLST NAME,DSN1,,BUF1
         SPACE 2
* CAMLST FOR DSCB 1
SERCHCAM CAMLST SEARCH,DSN1,VOLS,BUF1
         SPACE 2
* CAMLST FOR FORMAT 4 DSCB OBTAIN
F4CAM    CAMLST SEARCH,F4NAME,LASTVOL,BUF2
         SPACE 2
EXTRACT  EXTRACT ANSWER,'S',FIELDS=(TSO),MF=L
         EJECT
* CONSTANTS AND WORK AREAS
HWK1     DC    H'0'                HALF WORD WORK AREA
VTOCCHHR DC    XL5'0'              TRACK ADDR WORK AREA
WKD      DC    D'0'                DOUBLE WORK WORK AREA
ANSWER   DC    F'0'                ADDRESS POINTER FOR EXTRACT
         SPACE 3
RCC      EQU   R8
RHH      EQU   R7
RR       EQU   R6
         TITLE 'SHOWSMF - WORK AREA'
WORKAREA DSECT
WPPL     DS    7A
CPECB    DS    F
WPDL     DS    F
OBTNSAVE DS    10F
XTNTS    DS    16XL10
         DS    0D
LWORK    EQU   *-WORKAREA
         END
/*
//*
//********************************************************************
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR  <== TARGET
//LKED.SYSIN DD *
  NAME SHOWSMF(R)
/*
//*
//********************************************************************
//IEBUPDTE EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW
//*
//SYSUT2   DD  DISP=OLD,DSN=SYS2.PROCLIB
//SYSPRINT DD  SYSOUT=*
//SYSIN DD DATA,DLM='$$'
./ ADD NAME=SHOWSMF
//*-------------------------------------------------------------------*
//*                  SHOW SMF DATASET INFORMATION                     *
//*-------------------------------------------------------------------*
//SHOWSMF  PROC
//SHOWSMF  EXEC PGM=SHOWSMF
./ ENDUP
$$
//
