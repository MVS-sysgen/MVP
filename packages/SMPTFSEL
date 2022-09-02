//SMPTFSEL JOB (JOB),
//             'INSTALL SMPTFSEL',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//ASM     EXEC PGM=IFOX00,PARM='NOXREF,NOLIST,TERM,DECK,NOOBJECT'
//SYSIN    DD  DATA,DLM=@@
         TITLE 'SMP ELEMENT SELECTION PROGRAM'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        F E D E R A L   E X P R E S S   C O R P O R A T I O N        *
*                                                                     *
*        SMPPTFSEL                                                    *
*                SMP ELEMENT SELECTION PROGRAM                        *
*                                                                     *
*        FUNCTION                                                     *
*                ALLOW SELECTION OF SMP ELEMENTS                      *
*                    FOR GENERATION OF SELECT, EXCLUDE LISTS, ETC.    *
*        PROCESS                                                      *
*                SCANS SMP LIST OUTPUT FOR FIELD NAMES AND VALUES     *
*                SPECIFIED ON INPUT PARAMETERS.                       *
*        INPUT   DDNAME                                               *
*                INPUT    SMP SMPLIST OUTPUT DATASET                  *
*                SYSIN    ELEMENT SELECTION PARAMETERS                *
*        OUTPUT  DDNAME                                               *
*                OUTPUT   CARD IMAGE FILE OF SELECTED ELEMENT I.D.'S  *
*                SYSPRINT SMPLIST PRINT RECORDS THAT CAUSED SELECTION *
*                                                                     *
*        PARAMETER SYNTAX: (FREE FORMAT)                              *
*                FIELDNAME(VALUE) FIELDNAME(VALUE)...                 *
*                WHERE:                                               *
*                      FIELDNAME IS FIELD LABEL IN SMPLIST REPORT     *
*                      VALUE IS GENERIC VALUE TO BE SELECTED          *
*                            OR GENERIC VALUE RANGE                   *
*                      . INDICATES END OF PARAMETERS                  *
*                EXAMPLES:                                            *
*                 A-   FMID(ESP1200) FMID(JSP1210) .                  *
*                 B-   APP(80-80.122) .                               *
*                 C-   REQ(UZ9-UZ95).                                 *
*                 D-   PRE(UZ27866) .                                 *
*                 E-   LMOD(IKJCT469) .                               *
*                 F-   UMID(USER-USER50) UMID(USER900).               *
*                 G-   SZAP(IRARMCNS) .                               *
*                RESULTS: (DEPENDING ON SMP LIST PARAMETERS)          *
*                 A-   ALL ELEMENTS OF THE FMIDS                      *
*                 B-   ALL ELEMENTS APPLIED 80.000 THRU 80.122        *
*                 C-   ALL ELEMENTS THAT REQ ANY ELEMENT IN THE RANGE *
*                 D-   ALL ELEMENTS THAT PRE-REQ UZ27866              *
*                 E-   ALL MODULES OF IKJCT469                        *
*                 F-   ALL ELEMENTS UPDATED BY USER50. THRU USER900   *
*                 G-   ALL SUPERZAPS FOR IRARMCNS                     *
*                                                                     *
*        SAMPLE EXECUTION JCL IS PROVIDED FOLLOWING ALC SOURCE        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         TITLE 'SMP ELEMENT SELECTION PROGRAM'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 1
SMPTFSEL CSECT
         SAVE  (14,12),,SMPTFSEL_&SYSDATE_&SYSTIME
         LR    11,15                   SET BASE REGISTER
         USING SMPTFSEL,11             INDICATE BASE REGISTER(S)
         LA    12,4088(11)
         USING SMPTFSEL+4088,12
         LR    2,1                     SAVE PARAMETER REGISTER
         LA    0,72                    INDICATE SIZE
         BAL   1,G0001                 INDICATE GETMAIN
G0001    SVC   10                      ISSUE GETMAIN SVC
         ST    13,4(1)                 SAVE HIGHER SAVEAREA POINTER
         ST    1,8(13)                 SET POINTER THIS SAVEAREA
         LR    13,1                    SET SAVEAREA REGISTER
         LR    1,2                     RESTORE PARAMETER REGISTER
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         TITLE 'SELECTION PARAMETER PROCESSING'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 1
         L     COLUMN,SCANEOT          SCAN CONTROL
         LA    PARMS,PARMTBL           PARM TABLE
         A     PARMS,PARMEND               + PARM TABLE LENGTH
         ST    PARMS,PARMEND                   = PARM TABLE END
         LA    PARMS,PARMTBL           PARM TABLE
         USING PARMD,PARMS             PARM TABLE ENTRY DSECT
         OPEN  (SYSIN,(INPUT),SYSPRINT,(OUTPUT))
PARMLOOP DS    0H
         MVC   PARMNTRY,LOVALUE        CLEAR PARM
         BAL   RETURN,SCAN             SCAN INPUT PARAMETERS
         CLI   0(COLUMN),C'.'          Q. END OF SPECIFICATIONS
         BE    SYSINEOF                ...YES
         STC   R1,FLDNAMEL             SAVE FEILD NAME LENGTH
         MVC   FLDNAME,WORK            SAVE FIELD NAME, LENGTH
         CLI   0(COLUMN),C'('          Q. SPECIFICATION GROUP FOLLOWING
         BNE   PARMERR                 ...NO
         LA    COLUMN,1(COLUMN)        BUMP PAST PARENTHESIS
         BAL   RETURN,SCAN             SCAN FOR FIELD SPECIFICATION
         STC   R1,VALUE1L              SAVE FEILD VALUE LENGTH
         MVC   VALUE1,WORK             SAVE FIELD VALUE
         CLI   0(COLUMN),C')'          Q. END OF FIELD SPECIFICATION
         BE    PARMBUMP                ...YES
         CLI   0(COLUMN),C'-'          Q. SPECIFICATION RANGE
         BNE   PARMERR                 ...NO, ERROR
         LA    COLUMN,1(COLUMN)        BUMP PAST DASH
         BAL   RETURN,SCAN             SCAN FOR SPECIFICATION RANGE
         STC   R1,VALUE2L              SAVE FEILD VALUE LENGTH
         MVC   VALUE2,WORK             SAVE FIELD VALUE RANGE
         CLI   0(COLUMN),C')'          Q. END OF FIELD SPECIFICATIONS
         BNE   PARMERR                 ...NO, ERROR
PARMBUMP DS    0H
         LA    COLUMN,1(COLUMN)        BUMP PAST CLOSE PARENTHESIS
         LA    PARMS,PARMN(PARMS)      BUMP PARM TABLE ENTRY
         C     PARMS,PARMEND           Q. AT END OF PARM TABLE
         BL    PARMLOOP                ...NO, RETURN FOR MORE
         B     PARMERR
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         TITLE 'SMP LIST PROCESSING INITIALIZATION'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 1
SYSINEOF DS    0H
         CLOSE (SYSIN)
         CLI   0(COLUMN),C'.'          Q. END OF PARAMETERS SPECIFIED
         BNE   HEADER                  ...NO, PARAMETER ALREADY PRINTED
         PUT   SYSPRINT,PARMCARD       LIST PARAMETER & NOTES
HEADER   DS    0H
         PUT   SYSPRINT,RESULTS        SELECTION RESULTS TITLE
         OPEN  (INPUT,(INPUT),OUTPUT,(OUTPUT))
         ST    PARMS,PARMEND           PARM TABLE END
         USING SMPLD,SMPBASE           SMP LIST RECORD DSECT
         MVC   RECORD,SPACES           CLEAR
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         TITLE 'SMP LIST PROCESSING'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 1
FILELOOP DS    0H
         GET   INPUT
         LR    SMPBASE,R1              SET RECORD BASE REG
         CLI   0(R1),C'1'              Q. TITLE LINE
         LA    RETURN,FILELOOP            SKIP ROUTINE RETURN
         BE    SKIPHEAD                ...YES, SKIP
         CLC   =C'TYPE        ',SMPFLD Q. ELEMENT HEADER
         BE    SAVELEM                 ...YES, SAVE
         CLC   =C'LASTUPD     ',SMPFLD Q. ELEMENT HEADER
         BE    SAVELEM                 ...YES, SAVE
         CLI   SELECTED,X'FF'          Q. ELEMENT SELECTED
         BE    FILELOOP                ...YES, SKIP
         CLC   SMPFLD(10),SPACES       Q. OMMITTED DATE/TIME APP/ACC
         BNE   PARMSET                 ...NO
         MVC   SMPFLD(3),SMPFLD+10     SHIFT POSSIBLE APP/ACC
         MVC   SMPFLD+10(3),SPACES     CLEAR REMAINDER
PARMSET  DS    0H
         LA    PARMS,PARMTBL           PARM TABLE
         CLC   SMPFLD,SPACES           Q. CONTINUATION
         BNE   VALLOOP                 ...NO
         MVC   SMPFLD,SAVEFLD          SET CONTINUATION
VALLOOP  DS    0H
         MVC   SAVEFLD,SMPFLD          SAVE FOR CONTINUATION
         IC    R1,FLDNAMEL             FIELD NAME LENGTH
         EX    R1,CLCFLD               Q. FIELD SPECIFIED
         BNE   FLDBUMP                 ...NO, TRY NEXT PARM
         LA    VALBASE,SMPVALUE        SMP LIST FIELD VALUE
         USING SMPVALUE,VALBASE        SMP LIST FIELD VALUE ADDRESS
VALCHK   DS    0H
         IC    R1,VALUE1L              FIELD VALUE LENGTH
         CLI   VALUE2L,X'00'           Q. VALUE RANGE SPECIFIED
         BNE   RANGECHK                ...YES
         EX    R1,CLCVAL1              Q. VALUE SPECIFIED
         BE    WRITE                   ...YES
         B     VALBUMP                 ...NO, LOOK AGAIN
RANGECHK DS    0H
         EX    R1,CLCVAL1              Q. WITHIN VALUE RANGE
         BL    VALBUMP                 ...NO, LOOK AGAIN
         IC    R1,VALUE2L              FIELD VALUE LENGTH
         EX    R1,CLCVAL2              Q. WITHIN VALUE RANGE
         BH    VALBUMP                 ...NO, LOOK AGAIN
WRITE    DS    0H
         MVC   RETURNCD,LOVALUE        SET RETURN CODE - FOUND
         MVI   SELECTED,X'FF'          SET ELEMENT SELECTED
         MVC   ELEMENT,RECORD          ELEMENT I.D.
         PUT   SYSPRINT,SMPLIST
         PUT   OUTPUT,RECORD
         B     FILELOOP
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         TITLE 'SMP LIST PROCESSING'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 1
VALBUMP  DS    0H
         LA    VALBASE,10(VALBASE)     BUMP SMP LIST FIELD VALUE
         LA    R1,SMPLL(SMPBASE)       END OF SMP LIST RECORD
         CR    VALBASE,R1              Q. END OF SMP LIST RECORD
         BNL   VALCONT                 ...YES, CONTINUE
         CLC   SMPVALUE(10),SPACES     Q. END OF SMP LIST VALUES
         BE    FLDBUMP                 ...YES
         B     VALCHK                  ...NO, KEEP TRYING
VALCONT  DS    0H
         GET   INPUT
         LR    SMPBASE,R1              SET RECORD BASE REG
         CLI   0(R1),C'1'              Q. TITLE LINE
         LA    RETURN,VALCONT             SKIP ROUTINE RETURN
         BE    SKIPHEAD                ...YES, SKIP
         CLC   =C'TYPE        ',SMPFLD Q. ELEMENT HEADER
         BE    SAVELEM                 ...YES, SAVE
         CLC   =C'LASTUPD     ',SMPFLD Q. ELEMENT HEADER
         BE    SAVELEM                 ...YES, SAVE
         CLC   SMPFLD,SPACES           Q. CONTINUATION
         BNE   PARMSET                 ...NO, PROCESS PARAMETERS
         IC    R1,FLDNAMEL             FIELD NAME LENGTH
         EX    R1,MVCFLD               FIELD SPECIFIED
         B     VALLOOP                 KEEP TRYING
FLDBUMP  DS    0H
         LA    PARMS,PARMN(PARMS)      BUMP PARM TABLE ENTRY
         C     PARMS,PARMEND           Q. AT END OF PARM TABLE
         BL    VALLOOP                 ...NO, RETURN FOR MORE
         B     FILELOOP
         SPACE 1
SKIPHEAD DS    0H
         LA    ENTRY,6                 NUMBER OF LINES TO SKIP
SKIPPER  DS    0H
         STM   RETURN,ENTRY,@RETURN    SAVE LINKAGE
         GET   INPUT
         LM    RETURN,ENTRY,@RETURN    RESTORE LINKAGE
         BCT   ENTRY,SKIPPER           SKIP IT
         BR    RETURN                  RETURN
         SPACE 1
SAVELEM  MVI   SELECTED,X'00'          SET ELEMENT NOT SELECTED
         MVC   RECORD(8),ELEMENT       ELEMENT I.D.
         B     FILELOOP
         SPACE 1
CLCFLD   CLC   SMPFLD(0),FLDNAME
CLCVAL1  CLC   SMPVALUE(0),VALUE1
CLCVAL2  CLC   SMPVALUE(0),VALUE2
MVCFLD   CLC   SMPFLD(0),FLDNAME
         SPACE 1
         DROP  VALBASE
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         TITLE 'EXIT'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 1
EXIT     DS    0H
         CLOSE (INPUT,,OUTPUT,,SYSPRINT)
EXITIT   LR    1,13                    SET SAVEAREA POINTER
         L     13,4(13)                RESET REGISTER - HIGHER SAVEAREA
         L     0,=F'72'                INDICATE SIZE
         LA    1,0(0,1)                INDICATE FREEMAIN
         SVC   10                      ISSUE FREEMAIN SVC
         XC    16(4,13),16(13)         ZERO SAVEAREA RETURN CODE
         MVC   20-L'RETURNCD(L'RETURNCD,13),RETURNCD        SET RETURN
         LM    14,12,12(13)            RESTORE HIGHER LEVEL REGISTERS
         BR    14                      RETURN TO HIGHER LEVEL
         SPACE 1
PARMERR  DS    0H
         LA    R1,PARMCARD              CARD COLUMN =
         SR    COLUMN,R1                    CURRENT POSITION - START
         LA    COLUMN,1(COLUMN)                 + 1
         CVD   COLUMN,DBLWORD           CONVERT
         ED    PARMCOL,DBLWORD+6        EDIT
         PUT   SYSPRINT,PARMERRM        LIST ERROR MESSAGE
         CLOSE (SYSIN,,SYSPRINT)
         MVC   RETURNCD,=F'8'           SET RETURN CODE
         B     EXITIT                   EXIT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         TITLE 'PARAMETER PROCESSING'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 1
SCAN     DS    0H
         MVC   WORK,SPACES              RESET LINKAGE FIELD
         ST    R2,@2                    SAVE TRT WORK REGISTER
SCANIT   TRT   0(80,COLUMN),SPACE       SKIP SPACES
         BZ    SCANEND                  SCAN HAS ENDED - ALL SPACES
         LR    COLUMN,R1                NEW SCAN START
         C     COLUMN,SCANEOT           Q. ALL DATA BEEN SCANNED
         BNL   SCANEND                  ...YES
         TRT   0(80,COLUMN),DATA        SKIP PAST DATA
         ST    R1,@1                    SAVE NEW SCAN START
         C     R1,SCANEOT               Q. AT END OF CARD
         BL    SCANMOVE                 ...NO
         L     R1,SCANEOT               LIMIT TO END OF CARD
         ST    R1,@1
SCANMOVE SR    R1,COLUMN                MOVE LENGTH = END - START
         BCTR  R1,0                     ADJUST FOR EXECUTE
         EX    R1,MOVEDATA              MOVE DATA STRING USING LENGTH
         L     COLUMN,@1                NEW SCAN START
         L     R2,@2                    RESTORE TRT WORK REGISTER
SCANEXIT BR    RETURN
MOVEDATA MVC   WORK(0),0(COLUMN)
SCANEND  DS    0H
         STM   RETURN,R1,@RETURN       SAVE LINKAGE REGISTERS
         PUT   SYSPRINT,PARMCARD       LIST PARAMETER & NOTES
         GET   SYSIN                   READ NEXT PARAMETER RECORD
         MVI   PARMCARD,C' '           SPACE 1
         MVC   PARMCARD+1(80),0(R1)    SAVE PARAMETER RECORD
         LM    RETURN,R1,@RETURN       RESTORE LINKAGE REGISTERS
         LA    COLUMN,PARMCARD         RESET SCAN POINTER
         B     SCANIT                  CONTINUE SCAN
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         TITLE 'DATA AREAS AND CONSTANTS'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 1
RETURNCD DC    F'4'                    RETURN CODE - NOTHING FOUND
DBLWORD  DC    D'0'
RECORD   DC    CL80' '
PARMCARD DC    CL121'1SMPTFSEL - SMP ELEMENT SELECTION PARAMETERS'
         ORG   PARMCARD+72
CARDEND  DS    CL8
         ORG
SCANEOT  DC    A(CARDEND)              SCAN END ADDRESS
PARMERRM DC    CL121'        >>>> INVALID PARAMETER IN COLUMN     <<<<'
         ORG   PARMERRM+40                                   ....
PARMCOL  DC    X'40202120'             ERROR COLUMN EDIT FIELD...
         ORG
WORK     DS    0CL16                   TEXT WORK AREA
         DC    256C' '
RESULTS  DC    CL121'1SMPTFSEL - SMP ELEMENT SELECTION RESULTS'
SELECTED DC    XL1'00'                 ELEMENT SELECTED TEST
SAVEFLD  DS    CL16
SPACES   DC    256C' '
LOVALUE  DC    256X'00'
HIVALUE  DC    256X'FF'
SPACE    DS    0CL256                  TRANSLATE & TEST SPACE TABLE
         DC    64X'FF',X'00'               SPACE
         DC    10X'FF',C'.'                PERIOD
         DC    X'FF',C'('                  LEFT PARENTHESIS
         DC    15X'FF',C')'                RIGHT PARENTHESIS
         DC    X'00'                       SEMICOLON
         DC    12X'FF',X'00'               COMMA
         DC    148X'FF'
DATA     DS    0CL256                  TRANSLATE & TEST DATA TABLE
         DC    77X'00',C'('                LEFT PARENTHESIS
         DC    15X'00',C')'                RIGHT PARENTHESIS
         DC    2X'00',C'-'                 DASH
         DC    159X'00'
HEXCHAR  DS    0CL256                  TRANSLATE HEX CHARACTER TABLE
         DC    C'0123456789ABCDEF'
@RETURN  DC     F'0'
@ENTRY   DC     F'0'
@0       DC     F'0'
@1       DC     F'0'
@2       DC     F'0'
@3       DC     F'0'
@4       DC     F'0'
@5       DC     F'0'
@6       DC     F'0'
@PARMS   DC     F'0'
@COLUMN  DC     F'0'
@VALBASE DC     F'0'
@SMPBASE DC     F'0'
@BASE1   DC     F'0'
@BASE2   DC     F'0'
@SAVEREG DC     F'0'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         TITLE 'LITERALS AND DCBS'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 1
         LTORG
         PRINT NOGEN
INPUT    DCB   DSORG=PS,MACRF=GL,DDNAME=INPUT,EODAD=EXIT
OUTPUT   DCB   DSORG=PS,MACRF=PM,DDNAME=OUTPUT,                        X
               RECFM=F,LRECL=80,BLKSIZE=80
SYSIN    DCB   DSORG=PS,MACRF=GL,DDNAME=SYSIN,EODAD=SYSINEOF,          X
               LRECL=80
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,                      X
               RECFM=FA,LRECL=121,BLKSIZE=121
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         TITLE 'PARM TABLE AND DSECTS'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 1
PARMEND  DC    A(LPARMTBL)
PARMTBL  DS    1000CL72
LPARMTBL EQU   *-PARMTBL
PARMD    DSECT
PARMNTRY DS    CL72
         ORG   PARMNTRY
FLDNAMEL DS    XL1
FLDNAME  DS    CL23
VALUE1L  DS    XL1
VALUE1   DS    CL23
VALUE2L  DS    XL1
VALUE2   DS    CL23
         ORG
PARMN    EQU   *-PARMNTRY
SMPLD    DSECT
SMPLIST  DS    CL121
         ORG   SMPLIST+1
ELEMENT  DS    CL8
         ORG   SMPLIST+11
SMPFLD   DS    CL16
         ORG   SMPLIST+29
SMPVD    EQU   *-SMPLIST
SMPVALUE DS    CL16
         ORG
SMPLL    EQU   *-SMPLIST
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         TITLE 'REGISTER EQUATES'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 1
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
PARMS    EQU   7
COLUMN   EQU   8
VALBASE  EQU   9
SMPBASE  EQU   10
BASEREG1 EQU   11
BASEREG2 EQU   12
SAVEREG  EQU   13
RETURN   EQU   14
ENTRY    EQU   15
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 1
         END
@@
//SYSLIB   DD  DISP=SHR,DSN=SYS2.MACLIB,DCB=BLKSIZE=32720
//         DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT2   DD  UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT3   DD  UNIT=VIO,SPACE=(CYL,(1,1))
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSPUNCH DD  DISP=(,PASS),UNIT=VIO,SPACE=(CYL,(1,1))
//LINK    EXEC PGM=IEWL,
//             COND=(0,NE),
//             PARM='NORENT,LIST,LET,MAP'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(TRK,(50,20))
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.LINKLIB
//SYSLIN   DD  DISP=(OLD,DELETE),DSN=*.ASM.SYSPUNCH
//          DD *
 NAME SMPTFSEL(R)
//*
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=SMPTFSEL
//SMPTFSEL  JOB  (SETUP),
//             'Run SMPTFSEL',
//             CLASS=A,
//             MSGCLASS=H,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//********************************************************************
//*
//* Name: SMPTFSEL
//*
//* Desc: Run the SMPTFSEL program
//*
//********************************************************************
//LIST    EXEC PGM=HMASMP,REGION=1024K,PARM='DATE=U'
//SMPCDS   DD  DSN=SYS1.SMPCDS,DISP=SHR
//SMPACDS  DD  DSN=SYS1.SMPACDS,DISP=SHR
//SMPPTS   DD  DSN=SYS1.SMPPTS,DISP=SHR
//SMPLIST  DD  UNIT=VIO,SPACE=(CYL,(10,4),RLSE),DISP=(,PASS)
//SMPRPT   DD  SYSOUT=*
//SMPOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SMPLOG   DD  DUMMY
//SMPCNTL  DD  *
           LIST PTS SYSMOD .
           LIST CDS SYSMOD NOACCEPT .
//*
//SELECT  EXEC PGM=SMPTFSEL
//INPUT    DD  DSN=*.LIST.SMPLIST,DISP=(OLD,PASS)
//OUTPUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
 FMID(EJE1103)
 APP(72000-74365)
 REQ(UZ7-UZ9)
 PRE(JVT1102)
 LMOD(IEANUC01)
 RMID(ZUM0001-ZUM0002) UMID(ZUM0004)
 SZAP(IEFJESNM)
 .
