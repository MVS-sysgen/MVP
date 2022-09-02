//TAPEHDR JOB (JOB),
//             'INSTALL TAPEHDR',
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
* When trying to assemble it I found that 20 lines were duplicated    *
* Those lines were removed                                            *
*                                                                     *
* If things don't work on the Turnkey system, blame me                *
* If everything works out okay, it is his fault                       *
*                                                                     *
***********************************************************************
         TITLE 'Analyze and display tape labels - John A. Shell'
         MACRO
&NAME    BEGIN &PFX=R,&SAVE=(GENR),&BASE=13
         GBLC  &REG,&SA
         LCLA  &A,&B,&C
         LCLC  &E1,&H,&I
&E1      SETC  '0'
&H       SETC  '&NAME'
&REG     SETC  '&PFX'
&SA      SETC  '&SAVE'
         AIF   ('&H' EQ '').NOCSECT
.CSECT   ANOP
&H       CSECT
         AGO   .PFXTEST
.NOCSECT ANOP
&H       SETC  '&SYSECT'
         AIF   ('&H' NE '').PFXTEST
&H       SETC  'MAIN&SYSNDX'
         AGO   .CSECT
.PFXTEST AIF   ('&REG' NE 'NO').EQUS
&REG     SETC  ''
         AGO   .USING1
.EQUS    ANOP
&REG.0   EQU   0                   * * * * * * *
&REG.1   EQU   1                   *           *
&REG.2   EQU   2                   * REGISTERS *
&REG.3   EQU   3                   *    ARE    *
&REG.4   EQU   4                   *  EQUATED  *
&REG.5   EQU   5                   *    TO     *
&REG.6   EQU   6                   *  SYMBOLS  *
&REG.7   EQU   7                   *    SO     *
&REG.8   EQU   8                   *   THEY    *
&REG.9   EQU   9                   *   WILL    *
&REG.10  EQU   10                  *   SHOW    *
&REG.11  EQU   11                  *   UP ON   *
&REG.12  EQU   12                  *   CROSS   *
&REG.13  EQU   13                  * REFERENCE *
&REG.14  EQU   14                  *           *
&REG.15  EQU   15                  * * * * * * *
.USING1  USING *,&REG.15           INITIALLY USE ENTRY AS BASE REGISTER
         DS    0H
         B     $BG&SYSNDX          BRANCH AROUND CONSTANTS
&A       SETA  1
.LOOP1   AIF   ('&H'(1,&A) EQ '&H').LP1EXIT
&A       SETA  1+&A
         AGO   .LOOP1
.LP1EXIT DC    AL1(&A)             LENGTH OF CSECT NAME
         DC    CL&A.'&H'           CSECT ID
         DC    C' '
         DC    CL8'&SYSDATE'
         DC    C' '
         DC    CL5'&SYSTIME'
         DC    C' '
         AIF   (K'&SYSPARM NE 2).NOPGMR
         DC    CL7'PGMR=&SYSPARM'
.NOPGMR  ANOP
         DS    0F
$BG&SYSNDX.B EQU   *
         AIF   ('&SAVE' NE '(GENR)').NOTMINE
$BG&SYSNDX.A DS    18F             MY REGISTER SAVE AREA
         AGO   .BRCHK1
.NOTMINE AIF   ('&SAVE' EQ '(RENT)').BRCHK1
$BG&SYSNDX.C DC    A(&SAVE)
.BRCHK1  ANOP
&A       SETA  1
&B       SETA  0
.LOOP2   ANOP
&I       SETC  '&BASE(&A)'
         AIF   ('&I' EQ '').LP2EXIT
         AIF   ('&I' NE '13').LP2A
         AIF   (&A NE 1).FLUSH1
         AIF   ('&SAVE' EQ '(GENR)').LP2E
         AIF   ('&BASE(2)' NE '').FLUSH1
&A       SETA  1+&A
         AGO   .LP2EXIT
.LP2A    AIF   ('&I' EQ '0').LP2B
         AIF   ('&I' EQ '1').LP2B
         AIF   ('&I' EQ '14').LP2B
         AIF   ('&I' EQ '15').LP2B
         AGO   .LP2C
.LP2B    ANOP
&E1      SETC  '1'
.LP2C    AIF   (&A EQ 1).LP2E
         AIF   (&A GT 2).LP2D
$BG&SYSNDX.D DC    A($BG&SYSNDX.B+4096)
         AGO   .LP2E
.LP2D    DC    A($BG&SYSNDX.B+&B)
.LP2E    ANOP
&A       SETA  1+&A
&B       SETA  4096+&B
         AGO   .LOOP2
.LP2EXIT AIF   (&A EQ 1).FLUSH2
$BG&SYSNDX  STM   &REG.14,&REG.12,12(&REG.13)   SAVE CLG PGMS REGS
&A       SETA  3
         AIF   ('&SAVE' EQ '(GENR)').DOLA
         AIF   ('&SAVE' EQ '(RENT)').RENT
         L     &REG.&A,$BG&SYSNDX.C  LOAD ADDR OF MY REG SAVE AREA
         AGO   .STYOURS
.RENT    LR    &REG.3,&REG.1       SAVE PARM POINTER
&A       SETA  1
         LA    &REG.0,72           LOAD LENGTH DESIRED (18F)
         BAL   &REG.1,*+4          INDICATE GETMAIN
         SVC   10                  ISSUE REGMAIN SVC
         AGO   .STYOURS
.DOLA    LA    &REG.&A,$BG&SYSNDX.A  LOAD ADDR OF MY REG SAVE AREA
.STYOURS ST    &REG.13,4(&REG.&A)  STORE ADDR OF CLG SAVE IN MINE
         ST    &REG.&A,8(&REG.13)  STORE ADDR OF MY SAVE IN CLG PGM
         LR    &REG.13,&REG.&A     POINT REG 13 AT MY SAVE AREA
         AIF   ('&I' EQ '').NORMAL
&I       SETC  '12'
         AGO   .RTST
.NORMAL  ANOP
&I       SETC  '&BASE(1)'
.RTST    AIF   ('&SAVE' NE '(RENT)').B1TST
         LR    &REG.1,&REG.3       RESTORE PARM POINTER
.B1TST   AIF   ('&I' EQ '13').DROP
         LA    &REG.&I,$BG&SYSNDX.B  LOAD 1ST BASE REGISTER
.DROP    DROP  &REG.15             DROP INITIAL BASE REGISTER
         USING $BG&SYSNDX.B,&REG.&I
&A       SETA  2
&B       SETA  4096
&C       SETA  0
.LOOP3   ANOP
&I       SETC  '&BASE(&A)'
         AIF   ('&I' EQ '').LP3EXIT
         USING $BG&SYSNDX.B+&B,&REG.&I
         AIF   (&A GT 2).LP3A
         L     &REG.&I,$BG&SYSNDX.D  LOAD 2ND BASE REGISTER
         AGO   .LP3B
.LP3A    L     &REG.&I,$BG&SYSNDX.D+&C  LOAD NTH BASE REGISTER
.LP3B    ANOP
&A       SETA  1+&A
&B       SETA  4096+&B
&C       SETA  4+&C
         AGO   .LOOP3
.LP3EXIT AIF   ('&E1' EQ '0').EOMACRO
         MNOTE 4,'USE OF REGS 0, 1, 14 OR 15 AS BASE REGS IS POTENTIALLX
               Y DANGEROUS'
         AGO   .EOMACRO
.FLUSH1  MNOTE 16,'REG 13 MAY BE USED AS A BASE ONLY IF IT IS THE 1ST BX
               ASE AND THE MACRO-GENERATED SAVE AREA IS USED'
         AGO   .EOMACRO
.FLUSH2  MNOTE 16,'NO BASE REGISTER HAS BEEN SPECIFIED'
.EOMACRO MEXIT
         MEND
         MACRO
&NAME    FINISH &RC=,&PFX=
         GBLC  &REG,&SA
         LCLA  &A
         AIF   ('&PFX' EQ '').CKNAME
&REG     SETC  '&PFX'
.CKNAME  AIF   ('&NAME' EQ '').CKSA1
         DS    0H
&NAME    EQU   *
.CKSA1   AIF   ('&SA' NE '(RENT)').CKRC
         AIF   ('&RC' EQ '').RENT
         AIF   ('&RC'(1,1) EQ '(').RREG
         L     &REG.2,&RC          SAVE RETURN CODE FROM FIELD
         AGO   .SETA2
.RREG    ANOP
&A       SETA  &RC(1)
         AIF   (&A LT 2).RSVRG
         AIF   (&A GT 12).RSVRG
         AGO   .RENT
.RSVRG   LR    &REG.2,&REG.&A      SAVE RETURN CODE FROM REGISTER
.SETA2   ANOP
&A       SETA  2
.RENT    LR    &REG.1,&REG.13      POINT AT AREA TO BE FREED
         LA    &REG.0,72           SET LENGTH TO BE FREED
         AGO   .LOAD13
.CKRC    AIF   ('&RC' EQ '').LOAD13
         AIF   ('&RC'(1,1) EQ '(').SVRG
         L     &REG.15,&RC         LOAD RETURN CODE FROM FIELD
         AGO   .LOAD13
.SVRG    AIF   (&RC(1) EQ 15).LOAD13
         LR    &REG.15,&REG.&RC(1)  LOAD RETURN CODE FROM REGISTER
.LOAD13  L     &REG.13,4(&REG.13)  RESTORE POINTER TO CALLING PGM SAVE
         AIF   ('&SA' NE '(RENT)').LOAD14
         SVC   10                  ISSUE REGMAIN SVC (FREEMAIN)
.LOAD14  L     &REG.14,12(&REG.13)  LOAD RETURN ADDRESS
         AIF   ('&RC' EQ '').ZERO15
         AIF   ('&SA' NE '(RENT)').LMREST
         LR    &REG.15,&REG.&A     LOAD RETURN CODE FROM REGISTER
         AGO   .LMREST
.ZERO15  SR    &REG.15,&REG.15     ZERO RETURN CODE
.LMREST  LM    &REG.0,&REG.12,20(&REG.13)  RESTORE REGS 0 THRU 12
         BR    &REG.14             RETURN TO CALLING PROGRAM
         MEND
TAPEHDR  BEGIN BASE=8
*
START    EQU   *
         LA    R1,TIOB             LOAD PARAMETER REG1     *** OPEN ***
         SVC   19                  ISSUE OPEN SVC                     *
         LA    R10,REMAINDR        POINT AT BEGINNING OF REMAINDR
         LA    R11,1               INITIALIZE LINE COUNTER
         SR    R12,R12             ZERO CONTROL REGISTER
         EJECT
READTI   LA    R1,TIDECB           LOAD PARAMETER REG1     *** READ ***
         L     R15,TIDCB+48        LOAD READ ROUTINE ADDRESS          *
         BALR  R14,R15             LINK TO READ ROUTINE               *
*
         LA    R1,TIDECB           LOAD PARAMETER REG1    *** CHECK ***
         L     R15,TIDCB+52        LOAD CHECK ROUTINE ADDRESS         *
         BALR  R14,R15             LINK TO CHECK ROUTINE              *
*
         LH    R4,TILNGTH          LOAD BLOCK LENGTH REQUESTED (32,760)
         L     R5,TIIOBPT          LOAD ADDRESS OF IOB
         SH    R4,14(R5)           SUBTRACT RESIDUAL COUNT FROM 32,760
         CH    R4,=H'80'           IS BLOCK SIZE EIGHTY BYTES...
         BE    BRLIST(R12)            ..YES, GO TEST FOR LITERALS
         CVD   R4,DBLWRD              ..NO, CONVERT BLKSIZE TO DECIMAL
         UNPK  BLKSIZE2(5),DBLWRD+5(3)      UNPACK INTO MESSAGE
         OI    BLKSIZE2+4,X'F0'             AND CLEAR SIGN
         MVC   NBR(LNOL),NOLABELS  MOVE NO LABELS MSG TO LINE 7
         B     FILL8                    AND GO FILL LINE 8
*
BRLIST   B     VOL1TST             BRANCH TO TEST FOR VOL1
         B     DSL1TST                                DSL1
         B     DSL2TST                                DSL2
*
VOL1TST  CLC   VOL1,TIWORK         IBM STANDARD VOLUME LABEL...
         BNE   DSL1TST                ..NO, GO TEST FOR DSL1
         MVC   VOLSER(6),TIWORK+4     ..YES, MOVE VOLUME SERIAL NUMBER
         MVC   OWNERID(10),TIWORK+41          AND OWNER ID TO LINE3
         MVC   0(L2,R10),LINE2     MOVE LINE2 TO REMAINDR
         LA    R10,L2(R0,R10)      INCREMENT REMAINDR POINTER
         LA    R11,1(R0,R11)             AND LINE COUNTER
         LA    R12,4               RESET CONTROL REGISTER
         B     READTI              GO READ NEXT LABEL RECORD
*
BAD1ST   MVC   NBR(3),=C'1ST'      MOVE "1ST" AND
         MVC   IDS,VOL1                 VOL1/DSL1 ID LITS TO LINE 7
         B     FILL8               GO FILL LINE 8
*
BAD2ND   MVC   NBR(3),=C'2ND'      MOVE "2ND" TO LINE 8
         CH    R11,=H'3'           IS MISSING RECORD DSL2...
         BE    MOVEDSL2               ..YES, GO INSERT DSL2 ID LITS
         MVC   IDS,HDR1            MOVE DSL1 ID LITS TO LINE 7
         B     FILL8               GO FILL LINE 8
*
EOT      MVC   TIWORK(71),TAPEMARK INDICATE THAT END OF FILE IS HERE
         B     BRLIST(R12)              AND GO FINISH PROCESSING
         EJECT
DSL1TST  CLC   HDR1,TIWORK         IBM STANDARD DATA SET LABEL 1...
         BE    DSL1OK                 ..YES, CONTINUE PROCESSING
         CLC   EOV1,TIWORK
         BE    DSL1OK                 ..YES, CONTINUE PROCESSING
         CLC   EOF1,TIWORK
         BE    DSL1OK                 ..YES, CONTINUE PROCESSING
         LTR   R12,R12             CONTROL REGISTER ZERO...
         BZ    BAD1ST                 ..YES, 1ST RECORD BAD
         B     BAD2ND                 ..NO, 2ND RECORD BAD (R11=2)
*
DSL1OK   MVC   DSL1(4),TIWORK      MOVE LABEL IDENTIFIER
         MVC   DSID(17),TIWORK+4        DATA SET IDENTIFIER
         LA    R12,8               RESET CONTROL REGISTER
         B     READTI              GO READ NEXT LABEL RECORD
*
BAD3RD   MVC   NBR(3),=C'3RD'      MOVE "3RD" AND
MOVEDSL2 MVC   IDS,HDR2                 DSL2 ID LITS TO LINE 7
FILL8    MVC   BADRCD,TIWORK       MOVE BAD RECORD TO LINE 8
         MVC   0(L7L8,R10),LINE7   MOVE LINE7/LINE8 TO REMAINDR
         B     RESETLC             GO SET NUMBER OF LINES
         EJECT
DSL2TST  CLC   HDR2,TIWORK         IBM STANDARD DATA SET LABEL 2...
         BE    DSL2OK                 ..YES, CONTINUE PROCESSING
         CLC   EOV2,TIWORK
         BE    DSL2OK                 ..YES, CONTINUE PROCESSING
         CLC   EOF2,TIWORK
         BE    DSL2OK                 ..YES, CONTINUE PROCESSING
         CH    R11,=H'4'           LINE COUNTER FOUR...
         BE    BAD3RD                 ..YES, 3RD RECORD BAD
         B     BAD2ND                 ..NO, 2ND RECORD BAD (R11=3)
*
DSL2OK   MVC   DSL2(4),TIWORK      MOVE LABEL IDENTIFIER TO LINE 5
         MVC   RECFM(4),BLANKS     BLANK RECORD FORMAT
         MVC   RECFM(1),TIWORK+4   MOVE FORMAT TO RECFM (F/V/U)
         LA    R9,RECFM+1          POINT AT 2ND POSITION IN RECFM
         CLI   TIWORK+38,X'40'     BLOCK ATTRIBUTE BLANK...
         BE    MOVECC                 ..YES, GO MOVE CONTROL CHARACTER
         CLI   TIWORK+38,C'R'      RECORDS ARE BLOCKED AND SPANNED...
         BE    INSERTBS               ..YES, GO INSERT LITERAL "BS"
         MVC   0(1,R9),TIWORK+38      ..NO, MOVE "B" OR "S" TO RECFM
         LA    R9,1(R9)                     POINT AT NEXT OPEN POSITION
         B     MOVECC                   AND GO MOVE CONTROL CHARACTER
INSERTBS MVC   0(2,R9),=C'BS'      MOVE "BS" TO RECFM AND
         LA    R9,2(R9)                 POINT AT NEXT OPEN POSITION
MOVECC   MVC   0(1,R9),TIWORK+36   MOVE CONTROL CHARACTER
         MVC   BLKSIZE(5),TIWORK+5      BLOCK LENGTH
         MVC   LRECL(5),TIWORK+10       RECORD LENGTH
         MVC   DSPOS(1),TIWORK+16       DATA SET POSITION
         MVC   JOBSTEP(17),TIWORK+17    JOB/JOB STEP IDENTIFICATION
         MVC   TRTCH(2),TIWORK+34       TAPE RECORDING TECHNIQUE
         MVC   DEN(1),TIWORK+15     AND TAPE DENSITY TO LINES 5 AND 6
         MVC   0(L5L6,R10),LINE5   MOVE LINE5/LINE6 TO REMAINDR
RESETLC  LA    R11,2(R11)          INCREMENT LINE COUNTER AND
         STH   R11,NBRLINES             STORE IN NUMBER OF LINES
         LA    R1,WTOLIST          LOAD PARAMETER REG1      *** WTO ***
         SVC   35                  ISSUE WTO SVC                      *
         LA    R1,TIOB             LOAD PARAMETER REG1    *** CLOSE ***
         SVC   20                  ISSUE CLOSE SVC                    *
         SPACE 2
WTOR     XC    ECBADDR,ECBADDR
         WTOR  'TAPEHDR----ANOTHER TAPE?  REPLY  Y OR N',              *
               REPLY,1,ECBADDR
         WAIT  ECB=ECBADDR
         CLI   REPLY,C'N'
         BE    END
         CLI   REPLY,C'Y'
         BE    START
         B     WTOR
         SPACE 2
END      EQU   *
         FINISH
         EJECT
DBLWRD   DS    D                   BINARY/DECIMAL CONVERSION AREA
*
         DS    0F                  OPEN/CLOSE PARAMETER LIST
TIOB     DC    X'80',AL3(TIDCB)
*
TIDECB   DC    F'0'                EVENT CONTROL BLOCK
         DC    X'00'               S NOT CODED FOR LENGTH
         DC    X'80'               READ TYPE SF
TILNGTH  DC    AL2(32760)          LENGTH
         DC    A(TIDCB)            DCB ADDRESS
         DC    A(TIWORK)           AREA ADDRESS
TIIOBPT  DC    A(0)                RECORD POINTER WORD (IOB ADDRESS)
*
*        VOLUME AND DATA SET LABEL IDENTIFIER LITERALS
VOL1     DC    C'VOL1'
         DC    C'/'
HDR1     DC    C'HDR1'
         DC    C'/'
EOV1     DC    C'EOV1'
         DC    C'/'
EOF1     DC    C'EOF1'
         DC    CL5' '
HDR2     DC    C'HDR2'
         DC    C'/'
EOV2     DC    C'EOV2'
         DC    C'/'
EOF2     DC    C'EOF2'
BLANKS   DC    CL8' '
REPLY    DC    C' '
ECBADDR  DC    F'0'
*
PASSWORD DC    C'PASSWORD'
*
NOPWREAD DC    C'NOPWREAD'
*
TAPEMARK DC    CL71'*** TAPE MARK ***'
         EJECT
WTOLIST  DS    0F
LINE1    DC    AL2(ENDL1-*)        LINE 1 TEXT LENGTH
         DC    XL2'8040'           MCS FLAGS
         DC    CL5' '
         DC    CL45'**** IBM STANDARD TAPE LABEL INFORMATION ****'
ENDL1    EQU   *
*
         DC    XL2'0200'           DESCRIPTOR CODES (7)
         DC    XL2'4000'           ROUTING CODES    (2)
         DC    XL2'2000'           LINE 1 LINE TYPE (D)
NBRLINES DC    AL2(0)              TOTAL NUMBER OF LINES
*
REMAINDR DS CL300                  SPACE FOR REMAINDER OF LINES
*
*
LINE2    DC    AL2(ENDL2-*)        LINE 2 TEXT LENGTH
         DC    XL2'2000'           LINE 2 LINE TYPE (D)
         DC    CL14'VOL1 - VOLSER='
VOLSER   DC    CL16'XXXXXX  OWNERID='
OWNERID  DC    CL10'XXXXXXXXXX'
ENDL2    EQU   *
*
L2       EQU   (ENDL2-LINE2)       LENGTH OF LINE2
*
*
LINE3    DC    AL2(ENDL3-*)        LINE 3 TEXT LENGTH
         DC    XL2'2000'           LINE 3 LINE TYPE (D)
DSL1     DC    CL13'HDR1 - DSID='''
DSID     DC    CL26'XXXXXXXXXXXXXXXXX''  DSSER='
DSSER    DC    CL15'XXXXXX  VOLSEQ='
VOLSEQ   DC    CL12'XXXX  DSSEQ='
DSSEQ    DC    CL4'XXXX'
ENDL3    EQU   *
*
LINE4    DC    AL2(ENDL4-*)        LINE 4 TEXT LENGTH
         DC    XL2'2000'           LINE 4 LINE TYPE (D)
         DC    CL13'*      CREDT='
CREDT    DC    CL13'XXXXX  EXPDT='
EXPDT    DC    CL14'XXXXX  BLKCNT='
BLKCNT   DC    CL8'XXXXXX  '
SECURITY DC    CL8' '
ENDL4    EQU   *
*
L3L4     EQU   (ENDL4-LINE3)       LENGTH OF LINE3/LINE4
         EJECT
LINE5    DC    AL2(ENDL5-*)        LINE 5 TEXT LENGTH
         DC    XL2'2000'           LINE 5 LINE TYPE (D)
DSL2     DC    CL13'HDR2 - RECFM='
RECFM    DC    CL14'XXXX  BLKSIZE='
BLKSIZE  DC    CL13'XXXXX  LRECL='
LRECL    DC    CL13'XXXXX  DSPOS='
DSPOS    DC    CL1'X'
ENDL5    EQU   *
*
LINE6    DC    AL2(ENDL6-*)        LINE 6 TEXT LENGTH
         DC    XL2'3000'           LINE 6 LINE TYPE (DE)
         DC    CL16'*      JOB/STEP='
JOBSTEP  DC    CL23'XXXXXXXX/XXXXXXXX  DEN='
DEN      DC    CL9'X  TRTCH='
TRTCH    DC    CL2'XX'
ENDL6    EQU   *
*
L5L6     EQU   (ENDL6-LINE5)       LENGTH OF LINE5/LINE6
*
*
LINE7    DC    AL2(ENDL7-*)        LINE 7 TEXT LENGTH
         DC    XL2'2000'           LINE 7 LINE TYPE (D)
NBR      DC    CL15'XXX RECORD NOT '
IDS      DC    CL19' '
         DC    CL6' '
ENDL7    EQU   *
*
LINE8    DC    AL2(ENDL8-*)        LINE 8 TEXT LENGTH
         DC    XL2'3000'           LINE 8 LINE TYPE (DE)
BADRCD   DC    CL71' '
ENDL8    EQU   *
*
L7L8     EQU   (ENDL8-LINE7)       LENGTH OF LINE7/LINE8
*
NOLABELS DC    CL33'FILE ON TAPE NOT LABELS (BLKSIZE='
BLKSIZE2 DC    CL6'XXXXX)'
ENDNOL   EQU   *
*
LNOL     EQU   (ENDNOL-NOLABELS)   LENGTH OF NO LABELS MSG
*
         LTORG
         EJECT
TIDCB    DCB   BLKSIZE=32760,DDNAME=TAPEIN,DEVD=TA,DSORG=PS,EODAD=EOT, X
               MACRF=(R),RECFM=U,OPTCD=Z,SYNAD=TAPERROR
*
TAPERROR SYNADAF ACSMETH=BSAM
         MVC   TIWORK(71),50(R1)   MOVE SYNAD MESSAGE TO TAPEIN AREA
         SYNADRLS
         BR    R14                 RETURN TO MAIN LOGIC
         EJECT
TIWORK   DS    CL32760             TAPEIN WORK AREA
         END
         END   ,                  The end of the program
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
 NAME TAPEHDR(R)
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=TAPEHDR
//TAPEHDR  JOB  (SETUP),
//             'Run TAPEHDR',
//             CLASS=A,
//             MSGCLASS=H,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//********************************************************************
//*
//* Name: TAPEHDR
//*
//* Desc: Run the TAPEHDR program
//*
//********************************************************************
//TAPEHDR EXEC PGM=TAPEHDR
//SYSPRINT DD  SYSOUT=*
//TAPEIN   DD  DISP=SHR,DSN=TAPEIN,UNIT=TAPE,VOL=SER=TAPEIN,
//             LABEL=(,BLP)
//
@@