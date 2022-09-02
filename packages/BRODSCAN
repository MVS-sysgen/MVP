//BRODSCAN JOB (JOB),
//             'INSTALL BRODSCAN',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//ASM     EXEC PGM=IFOX00,PARM='DECK,NOOBJECT,TERM,NOXREF'
//SYSIN    DD  DATA,DLM=@@
***********************************************************************
*                                                                     *
* The source was sent to me by                   (somitcw@yahoo.com)  *
*                                                                     *
* If things don't work on the Turnkey system, blame me                *
* If everything works out okay, it is his fault                       *
*                                                                     *
***********************************************************************
*
* The following is from the Share tape, see details below.
*
         MACRO
&SUBR    ENTER &REGS,&BASES,&SAVE
         GBLC  &LV,&SP,&SAVED(2)
         LCLA  &K,&N
         LCLC  &AREA,&B(16),&SUBNAME,&S
.* ENTER IS A MACRO TO DO STANDARD LINKAGE AT THE ENTRY POINT OF A
.* PROGRAM.  THE FORM IS:
.*
.*      label   ENTER (saveregs),(baseregs),(sp,len)
.*
.* label : Represents the name to be given to the csect defined.  if
.*         omitted, various options can be taken depending on the
.*         current state of the control section.
.*
.* saveregs: The registers to be saved, e.g. (14,12).
.*
.* baseregs: The base register(s) to be assigned, e.g 12 or (11,12).
.*
.* sp,len  : Specifies the length and subpool for a getmain of the
.*           SAVEAREA and any other storage. Default subpool is zero.
.*           example: (,72) will get 72 bytes from sp 0.
.*                    (1,worklen) will get worklen bytes from sp 1.
.*                    * - indicates a static area named savearea is to
.*                        be allocated.
.*                    name - indicates that a static area named 'name'
.*                        is to be allocated in line.
.*
&SAVED(1) SETC '&REGS(1)'
&SAVED(2) SETC '&REGS(2)'
&SUBNAME SETC  '&SUBR'
         AIF   ('&SUBNAME' NE '').P1
&SUBNAME SETC  'MAIN'
.P1      ANOP
&SUBNAME CSECT
         AIF   ('&REGS' EQ '').PA
         SAVE  &REGS,T,*
.PA      AIF   ('&BASES(1)' EQ '15' OR '&BASES' EQ '').PC
         AIF   ('&BASES(1)' EQ '13' AND '&SAVE' NE '').PC
         LR    &BASES(1),15
.PC      CNOP  0,4
&S       SETC  '&SUBNAME'
         AIF   (N'&SAVE EQ 2).P4
         AIF   ('&SAVE' EQ '').P3
&AREA    SETC  '&SAVE'
         AIF   ('&SAVE' NE '*').P2
&AREA    SETC  'SAVEAREA'
.P2      AIF   ('&SAVE' NE '+').PB
&AREA    SETC  'SAVE'.'&SYSNDX'
.PB      AIF   ('&BASES(1)' NE '13').P4
&S       SETC  '*'
         USING &SUBNAME,15
         AIF   ('&REGS' EQ '').PD
         ST    14,&AREA+4
         LA    14,&AREA
         ST    14,8(13)
         L     14,&AREA+4
         ST    13,&AREA+4
.PD      BAL   13,*+76
         DROP  15
         AGO   .P4
.P3      AIF   ('&BASES(1)' NE '13').P4
         MNOTE 8,'* * THE CONTENTS OF REG. 13 ARE LOST. NO SAVE AREA WAX
               S ESTABLISHED.'
.P4      AIF   ('&BASES(1)' NE '14' OR '&SAVE' EQ '').P5
         MNOTE 8,'* * MACRO RESTRICTION - REG. 14 MUST NOT BE USED AS TX
               HE FIRST BASE REGISTER IF A SAVE AREA IS USED.'
.P5      AIF   ('&BASES' EQ '').P9
&N       SETA  N'&BASES
.P6      ANOP
&K       SETA  &K+1
&B(&K)   SETC  ','.'&BASES(&K)'
         AIF   (N'&SAVE EQ 1).PE
         AIF   ('&BASES(&K)' NE '13').P7
         MNOTE 8,'* * REG. 13 MAY NOT BE USED AS A BASE REGISTER FOR REX
               ENTRANT CODE.'
         AGO   .P7
.PE      AIF   ('&BASES(&K+1)' NE '13' OR '&SAVE' EQ '').P7
         MNOTE 8,'* * WHEN USING A SAVE AREA, REG. 13 MAY NOT BE USED AX
               S A SECONDARY BASE REGISTER.'
.P7      AIF   ('&BASES(&K+1)' NE '').P6
         USING &S&B(1)&B(2)&B(3)&B(4)&B(5)&B(6)&B(7)&B(8)&B(9)&B(10)&B(X
               11)&B(12)&B(13)&B(14)&B(15)&B(16)
&K       SETA  1
         AIF   ('&BASES(1)' NE '13' OR '&SAVE' EQ '').P8
&AREA    DC    18F'0'
.P8      AIF   (&K GE &N).P10
         LA    &BASES(&K+1),X'FFF'(&BASES(&K))
         LA    &BASES(&K+1),1(&BASES(&K+1))
&K       SETA  &K+1
         AGO   .P8
.P9      USING &SUBNAME,15
.P10     AIF   (N'&SAVE EQ 2).P13
         AIF   ('&SAVE' EQ '' OR '&BASES(1)' EQ '13').P12
         AIF   ('&REGS' EQ '').P11
         ST    14,&AREA+4
         LA    14,&AREA
         ST    14,8(13)
         L     14,&AREA+4
         ST    13,&AREA+4
.P11     BAL   13,*+76
&AREA    DC    18F'0'
.P12     MEXIT
.P13     ANOP
&LV      SETC  '&SAVE(2)'
&SP      SETC  '0'
         AIF   ('&SAVE(1)' EQ '').P14
&SP      SETC  '&SAVE(1)'
.P14     GETMAIN R,LV=&LV,SP=&SP
         ST    13,4(1)
         ST    1,8(13)
         LR    2,13
         LR    13,1
         LM    0,2,20(2)
         MEND
         MACRO
&NAME    LEAVE  &EQ,&CC=
         GBLC  &LV,&SP,&SAVED(2)
.* LEAVE IS A MACRO TO DO STANDARD LINKAGE AT THE EXIT OF A
.* PROGRAM.  THERE MUST BE AN ENTER CORRESPONDING TO EACH LEAVE.
.* THE SAVED REGISTERS WILL BE RETURNED AND THE SAVEAREA MARKED.
.*
.*   THE FORM IS:
.*
.*      label   LEAVE EQ,CC=cond
.*
.* label : Represents a label to be branched to.
.*
.* EQ:     specifies that the SETR macro should be issued to define
.*         the equates for the GPR's.
.*
.* cond:   specifies a return code to be placed in R15 prior to
.*         returning.  If not specified, R15 remains unchanged.
.*
&NAME    LR    2,13               SAVE CURRENT WORK/SAVE AREA
         L     13,4(13)           PICK UP LAST SAVE AREA
         STM   15,1,16(13)        STORE RETURN REGS
         AIF   ('&LV' EQ '').L1
         FREEMAIN R,LV=&LV,SP=&SP,A=(2)  FREE SAVE AREA
.L1      AIF   ('&SAVED(2)' EQ '').L2
         AIF   ('&CC' EQ '').L15       WAS CC SPECIFIED
         RETURN (&SAVED(1),&SAVED(2)),T,RC=&CC RETURN
         AGO   .L3
.L15     RETURN (&SAVED(1),&SAVED(2)),T        RETURN
         AGO   .L3
.L2      RETURN &SAVED(1),T *   RETURN TO CALLER
.L3      AIF   ('&EQ' NE 'EQ').L4
         SETR
.L4      MEND
         MACRO
         SETR
.* SETR WILL DO EQUATES FOR THE GENERAL PURPOSE REGISTERS.
.*
.*   THE FORM IS:
.*
.*           SETR
.*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         MEND
BRODSCAN ENTER (14,12),12,*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*        BRODSCAN -   FROM SHARE FILE 141
*
*        WRITTEN 7/83
*        WASHINGTON STATE DEPARTMENT OF TRANSPORTATION
*        RICHARD G. NIKULA
*
*        THIS PROGRAM ANALYZES THE TSO BROADCAST DATASET.  IT
*        DISPLAYS THE NUMBER OF AVAILABLE BLOCKS AND HOW THEY
*        ARE USED.  IT ALSO DISPLAYS THE TSO USERS WHO HAVE
*        MESSAGES WAITING.  IN ADDITION IT ALSO DOES A VALIDITY
*        CHECK ON RECORDS WHICH ARE IN THE MESSAGE POOL TO ASSURE
*        THEY ARE CHAINED OFF OF A USER RECORD.  ACCORDING TO
*        DOCUMENTATION, INSTANCES OCCUR WHICH CAN RESULT IN THESE
*        GARBAGE RECORDS EXISTING.
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
         OPEN  (BRODCAST,(INPUT),SYSOUT,(OUTPUT))
*
*  GET THE HEADER RECORD
*
         READ  HDRECB,DI,BRODCAST,'S','S','S',BLOCKNO
         CHECK HDRECB
         L     R3,HDRECB+12
         USING HEADER,R3
         MVC   SAVERRN,USERRRN
         MVC   HIGHVAL,TOTALREC         SAVE GET OUT VALUE
         LH    R5,HIGHVAL               GET NUMBER FOR PRINTOUT
         CVD   R5,CVDAREA               CONVERT IT TO PRINTABLE
         MVC   MAXCNT,=X'4020206B202120'
         ED    MAXCNT,CVDAREA+5         MAKE IT PRETTY
         PUT   SYSOUT,MAXIMUM           TELL HOW MANY
         LH    R5,MAXNOT                GET NUMBER OF NOTICES
         XR    R4,R4                    CLEAR R4
         D     R4,=F'25'                NUMBER OF BLOCKS REQUIRED
         AH    R5,MAXNOT                PLUS TOTAL FOR MESSAGES
         LTR   R4,R4                    REMAINDER?
         BZ    CVDNOT
         LA    R5,1(R5)                 ONE MORE REQUIRED
CVDNOT   CVD   R5,CVDAREA               CONVERT IT TO PRINTABLE
         MVC   NOTCNT,=X'40202120'
         ED    NOTCNT,CVDAREA+6         MAKE IT PRETTY
         PUT   SYSOUT,NOTICES           TELL HOW MANY
         FREEDBUF HDRECB,D,BRODCAST
         LA    R7,TYPETABL+1
         MVI   TYPETABL,X'04'
READLOOP ICM   R1,7,BLOCKNO
         LA    R1,1(R1)
         STCM  R1,7,BLOCKNO
         CLC   BLOCKNO+1(2),HIGHVAL
         BE    ANALYZE
         READ  RECECB,DI,BRODCAST,'S','S','S',BLOCKNO
         CHECK RECECB
         L     R2,RECECB+20             KEY ADDRESS
         MVC   0(1,R7),0(R2)            STORE TYPE INTO TABLE
         LA    R7,1(R7)
         CLI   0(R2),X'00'              IS THIS NOTICE RECORD
         BNE   CHK1                     NO.
         AP    NOTDIR,=P'1'             ADD 1 TO TOTAL
         B     FREEB
CHK1     CLI   0(R2),X'01'              IS THIS USERID INDEX RECORD
         BNE   CHK2                     NO.
         AP    USRIND,=P'1'             ADD 1 TO TOTAL
         B     FREEB
CHK2     CLI   0(R2),X'02'              IS THIS NOTICE MSG RECORD
         BNE   CHK3                     NO.
         AP    NOTMSG,=P'1'             ADD 1 TO TOTAL
         B     FREEB
CHK3     CLI   0(R2),X'03'              IS THIS USER MESSAGE RECORD
         BNE   CHKFF                    NO.
         AP    USRMSG,=P'1'             ADD 1 TO TOTAL
         B     FREEB
CHKFF    CLI   0(R2),X'FF'              IS THIS USER MESSAGE RECORD
         BNE   UNKNOWN                  NO.
         AP    FREEREC,=P'1'            ADD 1 TO TOTAL
         B     FREEB
UNKNOWN  PUT   SYSOUT,ODDKEY
FREEB    FREEDBUF RECECB,D,BRODCAST
         B     READLOOP
ANALYZE  EQU   *
         PUT   SYSOUT,BREAKOUT
         PUT   SYSOUT,KEYCOUNT
         MVC   KEYTYPE,=CL50'BROADCAST MSG DIRECTORY RECORDS'
         MVC   KEYCNT,=X'4020206B202120'
         ED    KEYCNT,NOTDIR
         PUT   SYSOUT,KEYCOUNT
         MVC   KEYTYPE,=CL50'BROADCAST MESSAGE RECORDS'
         MVC   KEYCNT,=X'4020206B202120'
         ED    KEYCNT,NOTMSG
         PUT   SYSOUT,KEYCOUNT
         MVC   KEYTYPE,=CL50'USERID INDEX RECORDS'
         MVC   KEYCNT,=X'4020206B202120'
         ED    KEYCNT,USRIND
         PUT   SYSOUT,KEYCOUNT
         MVC   KEYTYPE,=CL50'NON-BROADCAST MESSAGE RECORDS'
         MVC   KEYCNT,=X'4020206B202120'
         ED    KEYCNT,USRMSG
         PUT   SYSOUT,KEYCOUNT
         MVC   KEYTYPE,=CL50'FREE RECORDS'
         MVC   KEYCNT,=X'4020206B202120'
         ED    KEYCNT,FREEREC
         PUT   SYSOUT,KEYCOUNT
         PUT   SYSOUT,USERSOUT
INDEXLP  LA    R6,9                     COUNT OF USERS PER RECORD
         MVC   BLOCKNO,SAVERRN
         READ  SCNECB,DI,BRODCAST,'S','S','S',BLOCKNO
         CHECK SCNECB
         L     R3,SCNECB+12             ADDRESS OF RECORD
         MVC   SAVERRN,126(R3)          COPY NEXT INDEX RECORD NUMBER
USRLOOP  MVC   USERID,0(R3)             COPY WHO THIS IS
         CLC   USERID,=7X'00'           IS THIS A REAL USER?
         BE    NOMSG                    NOPE, MUST HAVE BEEN DELETED
         AP    USERS,=P'1'
         XR    R4,R4                    CLEAR UPPER BYTE
         ICM   R4,7,7(R3)               GET BLOCK POSITION
         BZ    NOMSG                    NO.. JUST BUMP
ALTCODE  LA    R5,TYPETABL(R4)          ADDRESS OF THIS BLOCK IN TABLE
         CLI   0(R5),X'03'              JUST VERIFY WERE WE ARE
         BE    TABLOK
         PUT   SYSOUT,TABLERR           JUST LET THEM KNOW I'M CONFUSED
         B     BUMPUSR
TABLOK   EQU   *
         MVI   0(R5),X'88'              SET THAT IT WAS A VALID MESSAGE
BUMPUSR  AP    MSGCOUNT,=P'1'           INCREMENT HOW MANY MESSAGES
         STCM  R4,7,MSGNUM
         READ  MSGECB,DI,BRODCAST,'S','S','S',MSGNUM
         CHECK MSGECB
         L     R4,MSGECB+12             ADDRESS OF RECORD
         ICM   R4,7,126(R4)             RRN OF NEXT MESSAGE
         FREEDBUF MSGECB,D,BRODCAST
         LTR   R4,R4
         BNZ   ALTCODE
         AP    RECCNT,=P'1'
         CP    RECCNT,=P'2'             TEST MID RANGE
         BL    LINE1                    LOWER
         BH    LINE3                    HIGHER
         BE    LINE2                    .MIDRANGE
LINE1    MVC   MSGCNT1,=X'402020202120'                 99-08-12 W.A.M.
         MVC   USERID1,USERID
         ED    MSGCNT1,MSGCOUNT
         B     ZAPCNT
LINE2    MVC   MSGCNT2,=X'402020202120'                 99-08-12 W.A.M.
         MVC   USERID2,USERID
         ED    MSGCNT2,MSGCOUNT
         B     ZAPCNT
LINE3    MVC   MSGCNT3,=X'402020202120'                 99-08-12 W.A.M.
         MVC   USERID3,USERID
         ED    MSGCNT3,MSGCOUNT
         PUT   SYSOUT,USERLINE
         MVC   MSGCNT2,=CL6' '                          99-08-12 W.A.M.
         MVC   MSGCNT3,=CL6' '                          99-08-12 W.A.M.
         MVC   USERID2,=CL7' '
         MVC   USERID3,=CL7' '
         ZAP   RECCNT,=P'0'
ZAPCNT   ZAP   MSGCOUNT,=P'0'
NOMSG    LA    R3,13(R3)                BUMP TO NEXT ENTRY
         BCT   R6,USRLOOP               REPEAT FOR NINE TIMES
         FREEDBUF SCNECB,D,BRODCAST
         CLC   SAVERRN,=3X'00'          IS THIS LAST
         BNE   INDEXLP                  GO DO NEXT ONE
         CP    RECCNT,=P'0'
         BE    PUTUCNT
         PUT   SYSOUT,USERLINE
PUTUCNT  MVC   USERCNT,=X'4020206B202120'
         ED    USERCNT,USERS            HOW MANY USERS ARE THERE
         PUT   SYSOUT,UTOTLINE
         LA    R7,TYPETABL              ONCE MORE THRU TYPE TABLE
VERFLOOP CLI   0(R7),X'99'              END YET?
         BE    ENDVERF
         CLI   0(R7),X'03'              IS THIS STILL AN 03
         LA    R7,1(R7)                 BUMP EITHER WAY
         BNE   VERFLOOP
         AP    UNUSABL,=P'1'
         B     VERFLOOP
ENDVERF  EQU   *
         MVC   UNUSDCNT,=X'40202120'
         ED    UNUSDCNT,UNUSABL
         PUT   SYSOUT,JUNKLINE
CLOSE    CLOSE (BRODCAST,,SYSOUT)
         LEAVE EQ
         LTORG ,
BLOCKNO  DC    X'000000'                BLOCK NUMBERS
SAVERRN  DS    CL3                      SAVE LOCAL OF USER INDEX
MSGNUM   DS    CL3                      SAVE LOCAL OF USER INDEX
CVDAREA  DS    D
HIGHVAL  DS    H
RECCNT   DC    PL1'0'
MSGCOUNT DC    PL3'0'                                   99-08-12 W.A.M.
NOTDIR   DC    PL3'0'
USRIND   DC    PL3'0'
NOTMSG   DC    PL3'0'
USRMSG   DC    PL3'0'
FREEREC  DC    PL3'0'
UNUSABL  DC    PL3'0'
USERS    DC    PL3'0'
ODDKEY   DC    CL133'  *  UNKNOWN KEY TYPE ENCUNTERED * *'
TABLERR  DC    CL133'  *  TABLE IS OUT OF SYNC WITH MY REALITY * '
BREAKOUT DC    CL133'0< THE FOLLOWING KEY BREAKDOWN WAS FOUND >'
USERSOUT DC    CL133'0< THE FOLLOWING USERS HAVE MESSAGES WAITING >'
USERID   DS    CL7
KEYCOUNT DS    0CL133
         DC    CL10' '
KEYTYPE  DC    CL50'HEADER RECORDS'
KEYCNT   DC    CL7'      1'
         DC    CL66' '
MAXIMUM  DS    0CL133
         DC    CL60'1TOTAL AVAILABLE BLOCKS IN DATASET'
MAXCNT   DS    CL7
         DC    CL66' '
NOTICES  DS    0CL133
         DC    CL63' BLOCKS NECESSARY FOR BROADCST MSGS (DIRECTORY/MESSX
               AGES)'
NOTCNT   DS    CL4
         DC    CL66' '
USERLINE DS    0CL133
         DC    CL10' '
USERID1  DS    CL7
         DC    CL3' '                                   99-08-12 W.A.M.
MSGCNT1  DS    CL6                                      99-08-12 W.A.M.
         DC    CL10' '
USERID2  DS    CL7
         DC    CL3' '                                   99-08-12 W.A.M.
MSGCNT2  DS    CL6                                      99-08-12 W.A.M.
         DC    CL10' '
USERID3  DS    CL7
         DC    CL3' '                                   99-08-12 W.A.M.
MSGCNT3  DS    CL6                                      99-08-12 W.A.M.
         DC    CL55' '
UTOTLINE DS    0CL133
         DC    CL60' NUMBER OF TSO USERS DEFINED IN DATASET'
USERCNT  DS    CL7
         DC    CL66' '
JUNKLINE DS    0CL133
         DC    CL63'0BLOCKS FOUND TO BE GARBAGED'
UNUSDCNT DS    CL4
         DC    CL66' '
BRODCAST DCB   DDNAME=BRODCAST,BLKSIZE=129,DSORG=DA,MACRF=RISC,        X
               OPTCD=R,RECFM=F,BUFNO=2,KEYLEN=1,BUFL=130
SYSOUT   DCB   DDNAME=SYSOUT,DSORG=PS,LRECL=133,MACRF=PM,RECFM=FBA
TYPETABL DC    16318X'99'
HEADER   DSECT
         DS    CL1
NOTRRN   DS    CL3                      RRN OF FIRST NOTICE RECORD
         DS    CL1
USERRRN  DS    CL3                      RRN OF FIRST INDEX RECORD
         DS    CL2
TOTALREC DS    CL2                      NUMBER OF RECORDS IN FILE
MAXNOT   DS    CL2                      MAXIMUM NUMBER OF NOTICES
         DS    CL1                      -- REST OF RECORD ---
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
//             PARM='LIST,LET,MAP'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(TRK,(50,20))
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.LINKLIB
//SYSLIN   DD  DISP=(OLD,DELETE),DSN=*.ASM.SYSPUNCH
//          DD *
 NAME BRODSCAN(R)
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=BRODSCAN
//BRODSCAN  JOB  (SETUP),
//             'Run BRODSCAN',
//             CLASS=A,
//             MSGCLASS=H,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//********************************************************************
//*
//* Name: BRODSCAN
//*
//* Desc: Run the BRODSCAN program
//*
//********************************************************************
//BRODSCN EXEC PGM=BRODSCAN
//BRODCAST DD  DSN=SYS1.BRODCAST,DISP=SHR
//SYSOUT   DD  SYSOUT=*,DCB=BLKSIZE=133
//* SYSUDUMP DD  SYSOUT=*
//
@@