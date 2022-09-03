//PDSGEN  JOB (TSO),
//             'Install PDSGEN',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//* ********************************************************            00020000
//* *  INSTALL THE 'PDSGEN'  PROGRAM                       *            00030000
//* ********************************************************            00040000
//PDSGEN   EXEC ASMFCL                                                  00050000
//SYSIN    DD *                                                         00060000
*                                                                 *JLM*
         TITLE '***  PDSGEN  --  FREEWARE  --  STELI INC. 2000  ***'
         MACRO
&LAB     BAS   &OP1,&OP2
.*******       *
.*******       *BAS     :83-05-30 - BRANCH AND SAVE
.*******       *
&LAB     BAL &OP1,&OP2           +
         ORG *-4                 +
*        DC  X'4D'               +
         ORG ,                   +
         MEND
*                                                                 *JLM*
*  PDSGEN IS FREEWARE AND MAY BE USED AT NO COST. NEITHER A LICENCE
*  NOR REGISTRATION IS REQUIRED.  YOU MAY INDICATE TO STELI THAT YOU'RE
*  A PDSGEN USER VIA WWW.STELI.COM OR BY MAIL. SUGGESTIONS, QUESTIONS
*  OR PROBLEMS ARE WELCOME.
*      STELI INC.
*      PDSGEN TEAM
*      27 HALF HOLLOW TURN
*      MONROE, NY  10950-4118
*                                                                 *JLM*
         PRINT NOGEN
*                                                                 *JLM*
PDSGEN   CSECT
*                                                                 *JLM*
REG1     EQU   1
*                                                                 *JLM*
BALREG   EQU   2
*                                                                 *JLM*
BUFREG1  EQU   3
COL1REG  EQU   3
GMREG    EQU   3
*                                                                 *JLM*
BUFREG   EQU   4
COL2REG  EQU   4
SLOTREG  EQU   4
*                                                                 *JLM*
ENDREG   EQU   5
CATREG   EQU   5
*                                                                 *JLM*
DATAREG  EQU   6
BCTREG   EQU   6
*                                                                 *JLM*
DSNREG   EQU   7
LRECLREG EQU   7
*                                                                 *JLM*
FINDREG  EQU   8
NMEMREG  EQU   8
*                                                                 *JLM*
LMEMREG  EQU   9
*                                                                 *JLM*
BASEREG  EQU   10
*                                                                 *JLM*
BLKSIREG EQU   11
LCREG    EQU   11
COL3REG  EQU   11
*                                                                 *JLM*
IHAREG   EQU   12
COL4REG  EQU   12
*                                                                 *JLM*
WORKREG  EQU   14
*                                                                 *JLM*
WORKREG2 EQU   15
*                                                                 *JLM*
         USING *,15
         USING IHADCB,IHAREG
*                                                                 *JLM*
*  ENTRY POINT
*                                                                 *JLM*
         STM   14,12,12(13)
         LA    WORKREG,SAVEAREA
         ST    WORKREG,8(13)
         ST    13,4(WORKREG)
*                                                                 *JLM*
         CNOP  0,4
         BAS   13,AROUND
         DROP  15
         USING SAVEAREA,13,BASEREG
SAVEAREA DS    18F
AROUND   LA    BASEREG,1
         LA    BASEREG,4095(BASEREG,13)
*                                                                 *JLM*
         OPEN  (DIR,,PDS,,SYSIN,,SYSPRINT,OUTPUT)
         LTR   15,15
         BZ    OPENOK
         STC   15,RC+3
         B     QUIT
OPENOK   GET   SYSIN,CARD
         B     PARMRTN
PARMEXIT TIME  DEC
         ST    REG1,DW
         MVC   HDR+6(6),DATEFMT
         ED    HDR+6(6),DW+1
         LA    LMEMREG,EIGHTXFF
         LA    IHAREG,DIR
         LA    DSNREG,DSNAREA
         LR    NMEMREG,LMEMREG
         LA    GMREG,1
*                                                                 *JLM*
*  DIRECTORY READ
*                                                                 *JLM*
D00      GET   DIR
EOGDSW   B     D10
         CLC   OLDTIOT,DCBTIOT
         BE    D00
         MVI   EOGDSW+1,X'F0'
D10      LH    ENDREG,0(REG1)
         LA    DATAREG,2(REG1)
         LA    ENDREG,0(REG1,ENDREG)
         CLC   OLDTIOT,DCBTIOT
         BE    D20
         RDJFCB  DIR
         MVC   0(44,DSNREG),JFCBAREA
         MVC   44(6,DSNREG),JFCBAREA+118
         LA    DSNREG,50(DSNREG)
         MVC   OLDTIOT,DCBTIOT
D20      CR    DATAREG,ENDREG
         BNL   D00
         CLC   EIGHTXFF,0(DATAREG)
         BNE   D40
         L     WORKREG,X000000
         MVI   EOGDSW+1,X'00'
         LA    WORKREG,1(WORKREG)
         STC   WORKREG,CATCNT
         B     D00
*                                                                 *JLM*
*  ENTER MEMBER INTO LIST STRUCTURE
*                                                                 *JLM*
D40      B     D42
         CLC   0(8,DATAREG),LOMEMBER
         BL    D88
         CLC   0(8,DATAREG),HIMEMBER
         BH    D88
D42      BCT   GMREG,D44
         GETMAIN  R,LV=4096
         LA    GMREG,256
         LR    SLOTREG,REG1
         B     D47
D44      LA    SLOTREG,16(SLOTREG)
         LR    REG1,SLOTREG
D47      MVC   0(11,REG1),0(DATAREG)
         MVC   11(1,REG1),CATCNT
         IC    WORKREG,11(DATAREG)
         SLL   WORKREG,27
         SRL   WORKREG,26
         LA    DATAREG,12(WORKREG,DATAREG)
D50      CLC   0(8,REG1),0(LMEMREG)
         BNL   D70
         L     LMEMREG,FIRSTMEM
         CLC   0(8,REG1),0(LMEMREG)
         BL    D60
         L     NMEMREG,12(LMEMREG)
         B     D50
D60      ST    LMEMREG,12(REG1)
         LR    NMEMREG,LMEMREG
         ST    REG1,FIRSTMEM
D65      B     D20
         B     D20
D70      CLC   0(8,REG1),0(NMEMREG)
         BL    D80
         LR    LMEMREG,NMEMREG
         L     NMEMREG,12(NMEMREG)
         B     D70
D80      ST    REG1,12(LMEMREG)
         LR    LMEMREG,REG1
         ST    NMEMREG,12(REG1)
D84      B     D20
         B     D20
D88      IC    WORKREG,11(DATAREG)
         SLL   WORKREG,27
         SRL   WORKREG,26
         LA    DATAREG,12(WORKREG,DATAREG)
         B     D20
*                                                                 *JLM*
*  ALL DIRECTORY ENTRIES ARE NOW IN THE LIST STRUCTURE
*                                                                 *JLM*
DIREOF   LA    IHAREG,PDS
         TM    DCBRECFM,X'C0'
         BO    S30
         TM    DCBRECFM,X'40'
         BZ    S90
         MVC   P50+2(2),RECFMV
         B     S90
S30      MVC   P50+2(2),RECFMU
S90      LH    BLKSIREG,DCBBLKSI
         STH   BLKSIREG,BLKSI
         LA    BLKSIREG,7(BLKSIREG)
         N     BLKSIREG,FFFFFFF8
         LA    0,0(BLKSIREG,BLKSIREG)
         GETMAIN  R,LV=(0)
         LR    BUFREG,REG1
         LA    DSNREG,DSNAREA
         LA    BUFREG1,0(BUFREG,BLKSIREG)
         L     LMEMREG,FIRSTMEM
         MVC   PRTLINE,BLANKS
*                                                                 *JLM*
*  DETERMINE WHICH ARE DUPS (FOR DUPS(ONLY) OPTION)
*                                                                 *JLM*
X00      B     P00
         L     WORKREG,FIRSTMEM
         CLC   EIGHTXFF,0(WORKREG)
X05      BE    P00
         L     WORKREG2,12(WORKREG)
X10      CLC   0(8,WORKREG),0(WORKREG2)
         BE    X20
         MVI   8(WORKREG),X'FF'
         LR    WORKREG,WORKREG2
         CLC   EIGHTXFF,0(WORKREG)
X15      BE    P00
         L     WORKREG2,12(WORKREG)
         B     X10
X20      LR    WORKREG,WORKREG2
         CLC   EIGHTXFF,0(WORKREG)
X25      BE    P00
         L     WORKREG2,12(WORKREG)
         CLC   0(8,WORKREG),0(WORKREG2)
         BE    X20
         LR    WORKREG,WORKREG2
         CLC   EIGHTXFF,0(WORKREG)
X35      BE    P00
         L     WORKREG2,12(WORKREG)
         B     X10
*                                                                 *JLM*
*  GEN'ERATE THE SYSOUT FILE
*                                                                 *JLM*
G00      OPEN  (SYSGEN,,SYSOUT,OUTPUT)
         LTR   15,15
         BZ    G10
         STC   15,RC+3
         B     QUIT
G10      L     0,GENSZREQ
         GETMAIN  R,LV=(0)
         ST    REG1,GENCDADR
         L     BCTREG,GENNOREQ
         LR    SLOTREG,REG1
G20      GET   SYSGEN,(SLOTREG)
         LA    SLOTREG,80(SLOTREG)
         BCT   BCTREG,G20
G40      S     BCTREG,GENNOREQ
         LCR   BCTREG,BCTREG
         ST    BCTREG,GENNORED
         L     LMEMREG,FIRSTMEM
         LTR   BCTREG,BCTREG
         BZ    G90
G50      CLC   EIGHTXFF,0(LMEMREG)
         BE    G90
         CLI   8(LMEMREG),X'FF'
         BE    G67
         L     BCTREG,GENNORED
         L     SLOTREG,GENCDADR
G60      MVC   CARD,0(SLOTREG)
         LA    WORKREG,CARD
         L     ENDREG,CARDSIZE
         S     ENDREG,FW8
         LA    ENDREG,1(ENDREG,WORKREG)
G61      CLC   MEMPOUND,0(WORKREG)
         BE    G910
         CLC   MEMPERC,0(WORKREG)
         BE    G930
         CLC   DSNPERC,0(WORKREG)
         BE    G950
         CLC   DSNPOUND,0(WORKREG)
         BE    G970
         LA    WORKREG,1(WORKREG)
G65      CR    WORKREG,ENDREG
         BL    G61
         PUT   SYSOUT,CARD
         LA    SLOTREG,80(SLOTREG)
         BCT   BCTREG,G60
G67      L     NMEMREG,12(LMEMREG)
G68      CLC   0(8,LMEMREG),0(NMEMREG)
G70      BE    G80
         LR    LMEMREG,NMEMREG
         CLC   EIGHTXFF,0(LMEMREG)
         BE    G90
         L     NMEMREG,12(LMEMREG)
         B     G50
G80      LR    LMEMREG,NMEMREG
         CLC   EIGHTXFF,0(LMEMREG)
         BE    G90
         L     NMEMREG,12(LMEMREG)
         B     G68
G90      B     QUIT
*                                                                 *JLM*
*  FIXED MEMBER NAME INSERTION
*                                                                 *JLM*
G910     MVC   0(8,WORKREG),0(LMEMREG)
         LA    WORKREG,8(WORKREG)
         B     G61
*                                                                 *JLM*
*  VARIABLE MEMBER NAME INSERTION
*                                                                 *JLM*
G930     MVC   0(8,WORKREG),0(LMEMREG)
         LA    WORKREG2,1(WORKREG)
         LA    WORKREG,8(WORKREG)
G935     CLI   0(WORKREG2),C' '
         BE    G940
         LA    WORKREG2,1(WORKREG2)
         CR    WORKREG2,WORKREG
         BL    G935
         B     G65
G940     LA    REG1,CARD
         A     REG1,CARDSIZE
         SR    REG1,WORKREG
         S     REG1,FW1
         EX    REG1,G930MVC1
         SR    WORKREG,WORKREG2
         LA    REG1,1(REG1,WORKREG2)
         S     WORKREG,FW1
         EX    WORKREG,G930MVC2
         LR    WORKREG,WORKREG2
         B     G61
G930MVC1 MVC   0(0,WORKREG2),0(WORKREG)
G930MVC2 MVC   0(0,REG1),BLANKS
*                                                                 *JLM*
*  VARIABLE DATA SET NAME INSERTION
*                                                                 *JLM*
G950     MVC   CARDSAVE,CARD
         SR    WORKREG2,WORKREG2
         IC    WORKREG2,11(LMEMREG)
         MH    WORKREG2,HW50
         LA    WORKREG2,DSNAREA(WORKREG2)
         LA    GMREG,1(WORKREG2)
         LA    REG1,43
G955     CLI   0(GMREG),C' '
         BE    G960
         LA    GMREG,1(GMREG)
         BCT   REG1,G955
         LA    GMREG,43
         B     G965
G960     SR    GMREG,WORKREG2
         S     GMREG,FW1
G965     LA    REG1,1(GMREG,WORKREG)
         LA    BALREG,CARD
         A     BALREG,CARDSIZE
         CR    BALREG,REG1
         BL    G969
         EX    GMREG,G965MVC1
         LA    WORKREG2,88(WORKREG)
         SR    BALREG,REG1
         S     BALREG,FW1
         EX    BALREG,G965MVC2
         LR    WORKREG,REG1
         S     GMREG,FW8
         LTR   GMREG,GMREG
         BC    10,G61
         LCR   GMREG,GMREG
         LA    REG1,CARD
         A     REG1,CARDSIZE
         SR    REG1,GMREG
         S     GMREG,FW1
         EX    GMREG,G965MVC3
         B     G61
G969     LA    WORKREG,8(WORKREG)
         B     G61
G965MVC1 MVC   0(0,WORKREG),0(WORKREG2)
G965MVC2 MVC   0(0,REG1),0(WORKREG2)
G965MVC3 MVC   0(0,REG1),BLANKS
*                                                                 *JLM*
*  FIXED DATA SET NAME INSERTION
*                                                                 *JLM*
G970     SR    WORKREG2,WORKREG2
         IC    WORKREG2,11(LMEMREG)
         MH    WORKREG2,HW50
         LA    WORKREG2,DSNAREA(WORKREG2)
         MVC   0(44,WORKREG),0(WORKREG2)
         LA    WORKREG,44(WORKREG)
         B     G61
*                                                                 *JLM*
*                                                                 *JLM*
*  PRINT THE MEMBER DATA
*                                                                 *JLM*
P00      CLC   EIGHTXFF,0(LMEMREG)
P05      BE    QUIT
P07      B     P08
         CLI   8(LMEMREG),X'FF'
         BE    PDSEOF05
P08      LA    FINDREG,8(LMEMREG)
         FIND  PDS,(FINDREG),C
         LA    LCREG,1
         MVI   READONE,C'N'
         XC    PRINTED,PRINTED
         MVC   8(3,LMEMREG),PAGECNT+1
         BAS   BALREG,RDPDS
P10      CHECK PDSDECB
         MVI   READONE,C'Y'
         XR    BUFREG,BUFREG1
         L     WORKREG,PDSDECB+16
         XR    BUFREG1,BUFREG
         LH    ENDREG,BLKSI
         XR    BUFREG,BUFREG1
         SH    ENDREG,14(WORKREG)
         BAS   BALREG,RDPDS
P50      B     F00
*                                                                 *JLM*
*                                                                 *JLM*
*  PRINT FIXED DATA
*                                                                 *JLM*
F00      LR    DATAREG,BUFREG
         LA    ENDREG,0(DATAREG,ENDREG)
F10      LH    LRECLREG,DCBLRECL
         MVI   F17+1,X'00'
F15      C     LRECLREG,FW100
         BNH   F20
         MVI   TRANSLEN,X'63'
         MVC   PRTLINE+10(100),0(DATAREG)
F17      B     F18
         CVD   LRECLREG,DW
         UNPK  PRTLINE+1(5),DW+5(3)
         OI    PRTLINE+5,X'F0'
         MVI   F17+1,X'F0'
F18      BAS   BALREG,PRINT
         S     LRECLREG,FW100
         LA    DATAREG,100(DATAREG)
         B     F15
F20      BCTR  LRECLREG,0
         EX    LRECLREG,MVC
         STC   LRECLREG,TRANSLEN
         BAS   BALREG,PRINT
         LA    DATAREG,1(LRECLREG,DATAREG)
         CR    DATAREG,ENDREG
         BL    F10
         B     P10
*                                                                 *JLM*
*  PRINT UNDEFINED FORMAT DATA
*                                                                 *JLM*
U00      LR    DATAREG,BUFREG
         LR    LRECLREG,ENDREG
         CVD   LRECLREG,DW
         UNPK  PRTLINE+1(5),DW+5(3)
         OI    PRTLINE+5,X'F0'
U10      C     LRECLREG,FW100
         BNH   U20
         MVI   TRANSLEN,X'63'
         MVC   PRTLINE+10(100),0(DATAREG)
         BAS   BALREG,PRINT
         S     LRECLREG,FW100
         LA    DATAREG,100(DATAREG)
         B     U10
U20      BCTR  LRECLREG,0
         EX    LRECLREG,MVC
         STC   LRECLREG,TRANSLEN
         BAS   BALREG,PRINT
         LA    DATAREG,1(DATAREG,LRECLREG)
         B     P10
*                                                                 *JLM*
*  PRINT VARIABLE DATA
*                                                                 *JLM*
V00      LA    DATAREG,4(BUFREG)
         LH    ENDREG,0(BUFREG)
         LA    ENDREG,0(ENDREG,BUFREG)
V10      LH    LRECLREG,0(DATAREG)
V12      TRT   4(8,DATAREG),NUMTEST
V13      BNZ   V15
         MVC   PRTLINE+113(8),4(DATAREG)
         LA    DATAREG,8(DATAREG)
         S     LRECLREG,FW8
V15      S     LRECLREG,FW4
         LA    DATAREG,4(DATAREG)
         CVD   LRECLREG,DW
         UNPK  PRTLINE+1(5),DW+5(3)
         OI    PRTLINE+5,X'F0'
V20      C     LRECLREG,FW100
         BNH   V30
         MVI   TRANSLEN,X'63'
         MVC   PRTLINE+10(100),0(DATAREG)
         BAS   BALREG,PRINT
         S     LRECLREG,FW100
         LA    DATAREG,100(DATAREG)
         B     V20
V30      BCTR  LRECLREG,0
         EX    LRECLREG,MVC
         STC   LRECLREG,TRANSLEN
         BAS   BALREG,PRINT
         LA    DATAREG,1(DATAREG,LRECLREG)
         CR    DATAREG,ENDREG
         BL    V10
         B     P10
*                                                                 *JLM*
*  PRINT SUBROUTINE
*                                                                 *JLM*
PRINT    BCT   LCREG,PRINT70
         L     LCREG,PAGESIZE
PRINT05  B     PRINT50
         MVI   PRINT05+1,X'F0'
         B     PRINT50
PRINT30  PUT   SYSPRINT,FOOTLINE
PRINT50  SR    WORKREG,WORKREG
         MVC   HDR+94(8),0(LMEMREG)
         IC    WORKREG,11(LMEMREG)
         MH    WORKREG,HW50
         LA    WORKREG,DSNAREA(WORKREG)
         MVC   HDR+37(44),0(WORKREG)
         MVC   HDR+21(6),44(WORKREG)
         L     WORKREG,PAGECNT
         MVC   HDR+109(12),PAGEFMT
         CVD   WORKREG,DW
         ED    HDR+109(12),DW+3
         LA    WORKREG,1(WORKREG)
         ST    WORKREG,PAGECNT
         PUT   SYSPRINT,HDR
         PUT   SYSPRINT,UNDER
PRINT70  MVI   PRTLINE,C' '
PRINT71  B     PRINT73
         BCR   0,0
PRINT72  TR    PRTLINE+10(100),PRTABLE
PRINT73  PUT   SYSPRINT,PRTLINE
PRINT74  B     PRINT80
         BCR   0,0
         MVC   PRTLINE(6),BLANKS
PRINT75  TR    PRTLINE+10(100),TR1TABLE
         PUT   SYSPRINT,PRTLINE
         MVC   PRINT78+1(1),TRANSLEN
         IC    WORKREG,TRANSLEN
         EX    WORKREG,MVC
PRINT78  TR    PRTLINE+10(100),TR2TABLE
         PUT   SYSPRINT,PRTLINE
PRINT80  L     WORKREG,PRINTED
         A     WORKREG,FW1
         C     WORKREG,PRINT1ST
         BNL   PDSEOF80
         ST    WORKREG,PRINTED
PRINT90  MVC   PRTLINE,BLANKS
         BR    BALREG
*                                                                 *JLM*
*  THE CURRENT MEMBER IS COMPLETE
*                                                                 *JLM*
PDSEOF   CLI   READONE,C'Y'
         BE    PDSEOF05
PDSEOF02 B     PDSEOF03
         PUT   SYSPRINT,FOOTLINE
PDSEOF03 SR    WORKREG,WORKREG
         MVC   HDR+94(8),0(LMEMREG)
         IC    WORKREG,11(LMEMREG)
         MH    WORKREG,HW50
         LA    WORKREG,DSNAREA(WORKREG)
         MVC   HDR+37(44),0(WORKREG)
         MVC   HDR+21(6),44(WORKREG)
         L     WORKREG,PAGECNT
         MVC   HDR+109(12),PAGEFMT
         CVD   WORKREG,DW
         ED    HDR+109(12),DW+3
         LA    WORKREG,1(WORKREG)
         ST    WORKREG,PAGECNT
         PUT   SYSPRINT,HDR
         PUT   SYSPRINT,UNDER
PDSEOF05 L     NMEMREG,12(LMEMREG)
PDSEOF10 CLC   0(8,LMEMREG),0(NMEMREG)
         BNE   PDSEOF20
         MVI   8(NMEMREG),X'FF'
         L     NMEMREG,12(NMEMREG)
         B     PDSEOF10
PDSEOF20 LR    LMEMREG,NMEMREG
         B     P00
*                                                                 *JLM*
PDSEOF80 CHECK PDSDECB
         B     PDSEOF
*                                                                 *JLM*
*  ALL THE MEMBER DATA HAS BEEN PRINTED
*                                                                 *JLM*
ENDITALL B     ENDITALK
         PUT   SYSPRINT,FOOTLINE
ENDITALK L     CATREG,X000000
         PUT   SYSPRINT,HDR1
         PUT   SYSPRINT,UNDER
         LA    DSNREG,DSNAREA
*                                                                 *JLM*
*  PRINT THE DATA SET CONCATENATION LIST
*                                                                 *JLM*
         MVC   PRTLINE,SKIPPER
DSNRTN50 MVC   PRTLINE+10(44),0(DSNREG)
         MVC   PRTLINE+61(6),44(DSNREG)
         MVC   PRTLINE+1(4),CATFMT
         ED    PRTLINE+1(4),PCATCNT
         PUT   SYSPRINT,PRTLINE
         LA    DSNREG,50(DSNREG)
         AP    PCATCNT,P1
         BCT   CATREG,DSNRTN50
*                                                                 *JLM*
*  PRINT THE MEMBER INDEX
*                                                                 *JLM*
         L     LMEMREG,FIRSTMEM
INDEX    CLC   EIGHTXFF,0(LMEMREG)
         BE    QUIT
         LA    WORKREG,50
         LR    COL1REG,LMEMREG
GET50A   CLC   EIGHTXFF,0(LMEMREG)
         BE    GET50B
         L     LMEMREG,12(LMEMREG)
         BCT   WORKREG,GET50A
GET50B   LA    WORKREG,50
         LR    COL2REG,LMEMREG
GET50C   CLC   EIGHTXFF,0(LMEMREG)
         BE    GET50D
         L     LMEMREG,12(LMEMREG)
         BCT   WORKREG,GET50C
GET50D   LA    WORKREG,50
         LR    COL3REG,LMEMREG
GET50E   CLC   EIGHTXFF,0(LMEMREG)
         BE    GET50F
         L     LMEMREG,12(LMEMREG)
         BCT   WORKREG,GET50E
GET50F   LA    WORKREG,50
         LR    COL4REG,LMEMREG
GET50G   CLC   EIGHTXFF,0(LMEMREG)
         BE    GET50H
         L     LMEMREG,12(LMEMREG)
         BCT   WORKREG,GET50G
GET50H   LA    BCTREG,50
         PUT   SYSPRINT,HDR2
         PUT   SYSPRINT,UNDER
         MVC   PRTLINE,SKIPPER
INDEX10  CLC   EIGHTXFF,0(COL1REG)
         BE    QUIT
         SR    WORKREG,WORKREG
         MVC   PRTLINE+1(8),0(COL1REG)
         IC    WORKREG,11(COL1REG)
         LA    WORKREG,1(WORKREG)
         CVD   WORKREG,DW
         MVC   PRTLINE+21(4),CATFMT
         ED    PRTLINE+21(4),DW+6
         CLI   8(COL1REG),X'FF'
INDEX11  BE    INDEX12
         MVC   PAGECNT+1(3),8(COL1REG)
         L     WORKREG,PAGECNT
         MVC   PRTLINE+09(12),PAGEFMT
         CVD   WORKREG,DW
         ED    PRTLINE+09(12),DW+3
INDEX12  L     COL1REG,12(COL1REG)
         CLC   EIGHTXFF,0(COL2REG)
         BE    INDEX20
         SR    WORKREG,WORKREG
         MVC   PRTLINE+33(8),0(COL2REG)
         IC    WORKREG,11(COL2REG)
         LA    WORKREG,1(WORKREG)
         CVD   WORKREG,DW
         MVC   PRTLINE+53(4),CATFMT
         ED    PRTLINE+53(4),DW+6
         CLI   8(COL2REG),X'FF'
INDEX13  BE    INDEX14
         MVC   PAGECNT+1(3),8(COL2REG)
         L     WORKREG,PAGECNT
         MVC   PRTLINE+41(12),PAGEFMT
         CVD   WORKREG,DW
         ED    PRTLINE+41(12),DW+3
INDEX14  L     COL2REG,12(COL2REG)
         CLC   EIGHTXFF,0(COL3REG)
         BE    INDEX20
         SR    WORKREG,WORKREG
         MVC   PRTLINE+65(8),0(COL3REG)
         IC    WORKREG,11(COL3REG)
         LA    WORKREG,1(WORKREG)
         CVD   WORKREG,DW
         MVC   PRTLINE+85(4),CATFMT
         ED    PRTLINE+85(4),DW+6
         CLI   8(COL3REG),X'FF'
INDEX15  BE    INDEX16
         MVC   PAGECNT+1(3),8(COL3REG)
         L     WORKREG,PAGECNT
         MVC   PRTLINE+73(12),PAGEFMT
         CVD   WORKREG,DW
         ED    PRTLINE+73(12),DW+3
INDEX16  L     COL3REG,12(COL3REG)
         CLC   EIGHTXFF,0(COL4REG)
         BE    INDEX20
         SR    WORKREG,WORKREG
         MVC   PRTLINE+97(8),0(COL4REG)
         IC    WORKREG,11(COL4REG)
         LA    WORKREG,1(WORKREG)
         CVD   WORKREG,DW
         MVC   PRTLINE+117(4),CATFMT
         ED    PRTLINE+117(4),DW+6
         CLI   8(COL4REG),X'FF'
INDEX17  BE    INDEX18
         MVC   PAGECNT+1(3),8(COL4REG)
         L     WORKREG,PAGECNT
         MVC   PRTLINE+105(12),PAGEFMT
         CVD   WORKREG,DW
         ED    PRTLINE+105(12),DW+3
INDEX18  L     COL4REG,12(COL4REG)
INDEX20  PUT   SYSPRINT,PRTLINE
         MVC   PRTLINE,BLANKS
         BCT   BCTREG,INDEX10
         B     INDEX
*                                                                 *JLM*
*  TERMINATE SETTING RETURN CODE
*                                                                 *JLM*
QUIT     B     QUIT50
         PUT   SYSPRINT,FOOTLINE
QUIT50   CLOSE (DIR,DISP,PDS,DISP,SYSIN,DISP,SYSPRINT,DISP)
         L     13,4(13)
         LM    14,12,12(13)
RC       LA    15,0
         BR    14
*                                                                 *JLM*
*  START A READ INTO THE SECONDARY BUFFER
*                                                                 *JLM*
RDPDS    READ  PDSDECB,SF,PDS,(BUFREG1),'S'
         BR    BALREG
*                                                                 *JLM*
*  THIS IS THE DATA SECTION (WHICH ALSO CONTAINS EXECUTABLE CODE
*                            TO KEEP PDSGEN WITHIN ONE PAGE
*                            AS FAR AS POSSIBLE)
*                                                                 *JLM*
DW       DS    D
*                                                                 *JLM*
JFCBAREA DS    0CL176
PRTLINE  DS    0CL121
*                                                                 *JLM*
PARMRTN  LA    DATAREG,CARD
         LA    ENDREG,CARD+71
         LA    IHAREG,SYSIN
         MVC   DCBEODAD,APARMEOF
PARM00   CR    DATAREG,ENDREG
         BNL   PARMQEND
         CLC   PARMNO,0(DATAREG)
         BE    PARM50
         CLC   PARMBLKS,0(DATAREG)
         BE    PARM18
         CLC   PARMDUPS,0(DATAREG)
         BE    PARM20
*        CLC   PARM1ST,0(DATAREG)
*        BE    PARM21
*        CLC   PARMEJEC,0(DATAREG)
*        BE    PARM22
*        CLC   PARMFOOT,0(DATAREG)
*        BE    PARM23
         CLC   PARMINDE,0(DATAREG)
         BE    PARM24
*        CLC   PARMHEX,0(DATAREG)
*        BE    PARM25
*        CLC   PARMLINE,0(DATAREG)
*        BE    PARM26
*        CLC   PARMVARS,0(DATAREG)
*        BE    PARM28
*        CLC   PARMTRAN,0(DATAREG)
*        BE    PARM29
         CLC   PARMLTNA,0(DATAREG)
         BE    PARM30
         CLC   PARMGTNA,0(DATAREG)
         BE    PARM32
         CLC   PARMGFIX,0(DATAREG)
         BE    PARM36
         CLC   PARMGVAR,0(DATAREG)
         BE    PARM38
         CLC   PARMGCNT,0(DATAREG)
         BE    PARM40
         CLC   PARMGCRD,0(DATAREG)
         BE    PARM42
         CLC   PARMGEN,0(DATAREG)
         BE    PARM34
         LA    DATAREG,1(DATAREG)
         B     PARM00
*                                                                 *JLM*
*  BLKSIZE KEYWORD
*                                                                 *JLM*
PARM18   LA    DATAREG,L'PARMBLKS(DATAREG)
         LR    WORKREG,DATAREG
PARM18A  CLI   0(WORKREG),C')'
         BE    PARM18B
         LA    WORKREG,1(WORKREG)
         CR    WORKREG,ENDREG
         BNL   PARMQEND
         B     PARM18A
PARM18B  CR    DATAREG,WORKREG
         BE    PARM18C
         SR    WORKREG,DATAREG
         BCTR  WORKREG,0
         EX    WORKREG,PARMTRT
         BNZ   PARM18C
         EX    WORKREG,PARMPACK
         CVB   WORKREG2,DW
         LA    IHAREG,SYSPRINT
         STH   WORKREG2,DCBBLKSI
         OI    DCBRECFM,X'10'
         LA    DATAREG,0(WORKREG,DATAREG)
PARM18C  LA    DATAREG,1(DATAREG)
         B     PARM00
*                                                                 *JLM*
*  DUPS KEYWORD
*                                                                 *JLM*
PARM20   CLC   PARMDUPO,0(DATAREG)
         BE    PARM20A
         MVI   PARMD,C'Y'
         LA    DATAREG,L'PARMINDE(DATAREG)
         B     PARM00
PARM20A  MVI   PARMDO,C'Y'
         LA    DATAREG,L'PARMDUPO(DATAREG)
         B     PARM00
*                                                                 *JLM*
*  FIRST KEYWORD
*ARM21   LA    DATAREG,L'PARM1ST(DATAREG)
*        LR    WORKREG,DATAREG
*ARM21A  CLI   0(WORKREG),C')'
*        BE    PARM21B
*        LA    WORKREG,1(WORKREG)
*        CR    WORKREG,ENDREG
*        BNL   PARMQEND
*        B     PARM21A
*ARM21B  CR    DATAREG,WORKREG
*        BE    PARM21D
*        SR    WORKREG,DATAREG
*        BCTR  WORKREG,0
*        EX    WORKREG,PARMTRT
*        BNZ   PARM21D
*        EX    WORKREG,PARMPACK
*        CVB   WORKREG2,DW
*        C     WORKREG2,FW1
*        BH    PARM21C
*        L     WORKREG2,FW1
*ARM21C  ST    WORKREG2,PRINT1ST
*        LA    DATAREG,0(WORKREG,DATAREG)
*ARM21D  LA    DATAREG,1(DATAREG)
*        B     PARM00
*                                                                 *JLM*
*  EJECT KEYWORD
*ARM22   MVI   PARME,C'Y'
*        LA    DATAREG,L'PARMEJEC(DATAREG)
*        B     PARM00
*                                                                 *JLM*
*  FOOTING KEYWORD
*ARM23   LA    DATAREG,L'PARMFOOT(DATAREG)
*        LR    WORKREG,DATAREG
*ARM23A  CLI   0(WORKREG),C')'
*        BE    PARM23B
*        LA    WORKREG,1(WORKREG)
*        CR    WORKREG,ENDREG
*        BNL   PARMQEND
*        B     PARM23A
*ARM23B  CR    DATAREG,WORKREG
*        BE    PARM23C
*        MVC   PRINT05(4),FOOTINIT
*        MVC   ENDITALL(4),NOP4
*        MVC   PDSEOF02(4),NOP4
*        SR    WORKREG,DATAREG
*        MVC   FOOTLINE+1(120),BLANKS
*        LA    WORKREG2,120
*        SR    WORKREG2,WORKREG
*        SRL   WORKREG2,1
*        LA    WORKREG2,FOOTLINE+1(WORKREG2)
*        BCTR  WORKREG,0
*        EX    WORKREG,FOOTMVC
*        LA    DATAREG,0(WORKREG,DATAREG)
*ARM23C  LA    DATAREG,1(DATAREG)
*        B     PARM00
*                                                                 *JLM*
*  INDEX KEYWORD
*                                                                 *JLM*
PARM24   CLC   PARMINDO,0(DATAREG)
         BE    PARM24A
         MVI   PARMI,C'Y'
         LA    DATAREG,L'PARMINDE(DATAREG)
         B     PARM00
PARM24A  MVI   PARMA,C'N'
         LA    DATAREG,L'PARMINDO(DATAREG)
         B     PARM00
*                                                                 *JLM*
*  HEX KEYWORD
*ARM25   MVI   PARMH,C'Y'
*        LA    DATAREG,L'PARMHEX(DATAREG)
*        B     PARM00
*                                                                 *JLM*
*  LINECNT KEYWORD
*ARM26   LA    DATAREG,L'PARMLINE(DATAREG)
*        LR    WORKREG,DATAREG
*ARM26A  CLI   0(WORKREG),C')'
*        BE    PARM27
*        LA    WORKREG,1(WORKREG)
*        CR    WORKREG,ENDREG
*        BNH   PARM26A
*        B     PARM00
*ARM27   CR    DATAREG,WORKREG
*        BE    PARM27A
*        SR    WORKREG,DATAREG
*        BCTR  WORKREG,0
*        EX    WORKREG,PARMTRT
*        BNZ   PARM27A
*        EX    WORKREG,PARMPACK
*        CVB   WORKREG2,DW
*        C     WORKREG2,FW4
*        BH    PARM27X
*        L     WORKREG2,FW4
*ARM27X  ST    WORKREG2,PAGESIZE
*        LA    DATAREG,0(WORKREG,DATAREG)
*ARM27A  LA    DATAREG,1(DATAREG)
*        B     PARM00
*                                                                 *JLM*
*  TRANSLATE KEYWORD
*ARM29   MVI   PARMT,C'Y'
*        LA    DATAREG,L'PARMTRAN(DATAREG)
*        B     PARM00
*                                                                 *JLM*
*  VARSEQ KEYWORD
*ARM28   MVI   PARMV,C'Y'
*        LA    DATAREG,L'PARMVARS(DATAREG)
*        B     PARM00
*                                                                 *JLM*
*  <MEMBER KEYWORD
*                                                                 *JLM*
PARM30   MVI   D40+1,X'00'
         LA    DATAREG,L'PARMLTNA(DATAREG)
         LR    WORKREG,DATAREG
PARM30A  CLI   0(WORKREG),C')'
         BE    PARM31
         LA    WORKREG,1(WORKREG)
         CR    WORKREG,ENDREG
         BNL   PARMQEND
         B     PARM30A
PARM31   CR    DATAREG,WORKREG
         BE    PARM31A
         SR    WORKREG,DATAREG
         BCTR  WORKREG,0
         MVC   HIMEMBER,BLANKS
         EX    WORKREG,HINAMOVE
         LA    DATAREG,0(WORKREG,DATAREG)
PARM31A  LA    DATAREG,1(DATAREG)
         B     PARM00
*                                                                 *JLM*
*  >MEMBER KEYWORD
*                                                                 *JLM*
PARM32   LA    DATAREG,L'PARMGTNA(DATAREG)
         LR    WORKREG,DATAREG
PARM32A  CLI   0(WORKREG),C')'
         BE    PARM33
         LA    WORKREG,1(WORKREG)
         CR    WORKREG,ENDREG
         BNL   PARMQEND
         B     PARM32A
PARM33   CR    DATAREG,WORKREG
         BE    PARM33A
         SR    WORKREG,DATAREG
         MVI   D40+1,X'00'
         BCTR  WORKREG,0
         MVC   LOMEMBER,BLANKS
         EX    WORKREG,LONAMOVE
         LA    DATAREG,0(WORKREG,DATAREG)
PARM33A  LA    DATAREG,1(DATAREG)
         B     PARM00
*                                                                 *JLM*
*  GEN'ERATE KEYWORD
*                                                                 *JLM*
PARM34   MVI   PARMG,C'Y'
         LA    DATAREG,L'PARMGEN(DATAREG)
         B     PARM00
*                                                                 *JLM*
*  GEN'ERATE FIXED INDICATOR
*                                                                 *JLM*
PARM36   LA    DATAREG,L'PARMGFIX(DATAREG)
         CLI   1(DATAREG),C')'
         BNE   PARM00
         MVC   MEMPOUND(1),0(DATAREG)
         MVC   DSNPOUND(1),0(DATAREG)
         MVC   MEMPOUND+7(1),0(DATAREG)
         MVC   DSNPOUND+7(1),0(DATAREG)
         LA    DATAREG,2(DATAREG)
         B     PARM00
*                                                                 *JLM*
*  GEN'ERATE VARIABLE INDICATOR
*                                                                 *JLM*
PARM38   LA    DATAREG,L'PARMGVAR(DATAREG)
         CLI   1(DATAREG),C')'
         BNE   PARM00
         MVC   MEMPERC(1),0(DATAREG)
         MVC   DSNPERC(1),0(DATAREG)
         MVC   MEMPERC+7(1),0(DATAREG)
         MVC   DSNPERC+7(1),0(DATAREG)
         LA    DATAREG,2(DATAREG)
         B     PARM00
*                                                                 *JLM*
*  GEN'ERATE MAXIMUM NUMBER OF CARDS
*                                                                 *JLM*
PARM40   LA    DATAREG,L'PARMGCNT(DATAREG)
         LR    WORKREG,DATAREG
PARM40A  CLI   0(WORKREG),C')'
         BE    PARM41
         LA    WORKREG,1(WORKREG)
         CR    WORKREG,ENDREG
         BNH   PARM40A
         B     PARM00
PARM41   CR    DATAREG,WORKREG
         BE    PARM41A
         SR    WORKREG,DATAREG
         BCTR  WORKREG,0
         EX    WORKREG,PARMTRT
         BNZ   PARM41A
         EX    WORKREG,PARMPACK
         CVB   WORKREG2,DW
         C     WORKREG2,FW10
         BH    PARM41X
         L     WORKREG2,FW10
PARM41X  ST    WORKREG2,GENNOREQ
         MH    WORKREG2,HW80
         ST    WORKREG2,GENSZREQ
         LA    DATAREG,0(WORKREG,DATAREG)
PARM41A  LA    DATAREG,1(DATAREG)
         B     PARM00
*                                                                 *JLM*
*  GEN'ERATE SIZE OF CARD TO USE
*                                                                 *JLM*
PARM42   LA    DATAREG,L'PARMGCRD(DATAREG)
         LR    WORKREG,DATAREG
PARM42A  CLI   0(WORKREG),C')'
         BE    PARM43
         LA    WORKREG,1(WORKREG)
         CR    WORKREG,ENDREG
         BNH   PARM42A
         B     PARM00
PARM43   CR    DATAREG,WORKREG
         BE    PARM43A
         SR    WORKREG,DATAREG
         BCTR  WORKREG,0
         EX    WORKREG,PARMTRT
         BNZ   PARM43A
         EX    WORKREG,PARMPACK
         CVB   WORKREG2,DW
         CH    WORKREG2,HW80
         BE    PARM43Y
         CH    WORKREG2,HW72
         BNE   PARM00
PARM43Y  ST    WORKREG2,CARDSIZE
         LA    DATAREG,0(WORKREG,DATAREG)
PARM43A  LA    DATAREG,1(DATAREG)
         B     PARM00
*                                                                 *JLM*
*  'NO' PARM OPTION
*                                                                 *JLM*
PARM50   LA    DATAREG,2(DATAREG)
         CLC   PARMDUPS,0(DATAREG)
         BE    PARM70
*        CLC   PARMEJEC,0(DATAREG)
*        BE    PARM72
*        CLC   PARMHEX,0(DATAREG)
*        BE    PARM74
         CLC   PARMINDE,0(DATAREG)
         BE    PARM76
*        CLC   PARMGEN,0(DATAREG)
*        BE    PARM77
*        CLC   PARMTRAN,0(DATAREG)
*        BE    PARM78
*        CLC   PARMVARS,0(DATAREG)
*        BE    PARM79
         LA    DATAREG,1(DATAREG)
         B     PARM00
PARM70   MVI   PARMD,C'N'
         LA    DATAREG,L'PARMDUPS(DATAREG)
         B     PARM00
*ARM72   MVI   PARME,C'N'
*        LA    DATAREG,L'PARMEJEC(DATAREG)
*        B     PARM00
*ARM74   MVI   PARMH,C'N'
*        LA    DATAREG,L'PARMHEX(DATAREG)
*        B     PARM00
PARM76   MVI   PARMI,C'N'
         LA    DATAREG,L'PARMINDE(DATAREG)
         B     PARM00
*ARM77   MVI   PARMG,C'N'
*        LA    DATAREG,L'PARMGEN(DATAREG)
*        B     PARM00
*ARM78   MVI   PARMT,C'N'
*        LA    DATAREG,L'PARMTRAN(DATAREG)
*        B     PARM00
*ARM79   MVI   PARMV,C'N'
*        LA    DATAREG,L'PARMVARS(DATAREG)
*        B     PARM00
*                                                                 *JLM*
*  PARM CARD COMPLETE - SEE IF ANOTHER
*                                                                 *JLM*
PARMQEND GET   SYSIN,CARD
         B     PARMRTN
*                                                                 *JLM*
*  PARM ANALYSIS COMPLETE - IMPLEMENT OPTIONS CHOSEN
*                                                                 *JLM*
PARMDEFN CLI   PARMA,C'Y'
         BE    PARM91
         MVC   D65(4),MVIXFF
         MVC   D84(4),MVIXFF
         MVC   DIREOF+4(4),BINDXONL
         MVC   QUIT(4),NOP4
PARM91   CLI   PARMDO,C'N'
         BE    PARM91A
         MVC   X00(4),NOP4
         MVC   P07(4),NOP4
         MVI   PARMD,C'Y'
PARM91A  CLI   PARMD,C'N'
         BE    PARM92
         NI    G70+1,X'0F'
         MVC   PDSEOF10(6),PDSEOF20
PARM92   CLI   PARME,C'N'
         BE    PARM93
         MVI   HDR,C'1'
PARM93   CLI   PARMH,C'N'
         BE    PARM94
         MVC   PRINT74(6),HEXON
         MVI   PRINT70+1,C'0'
PARM94   CLI   PARMI,C'N'
         BE    PARM95
         MVC   P05+2(2),PARMAEND
         MVC   G90+2(2),PARMAEND
PARM95   CLC   PRINT1ST,FWNEG1
         BNE   PARM96
         MVC   PRINT80(4),BPRINT90
PARM96   CLI   PARMV,C'Y'
         BE    PARM97
         MVC   V12(4),V13
         MVI   V12+1,X'F0'
PARM97   CLI   PARMT,C'N'
         BE    PARM98
         MVC   PRINT71(6),TRANSON
PARM98   CLI   PARMG,C'N'
         BE    PARM99
         MVC   DIREOF(4),BX00
         MVC   X05(4),BEG00
         MVC   X15(4),BEG00
         MVC   X25(4),BEG00
         MVC   X35(4),BEG00
         OI    INDEX11+1,X'F0'
         OI    INDEX13+1,X'F0'
         OI    INDEX15+1,X'F0'
         OI    INDEX17+1,X'F0'
         CLI   PARMDO,C'Y'
         BE    PARM99
         MVC   X00(4),BG00
PARM99   B     PARMEXIT
*                                                                 *JLM*
*  "EX"ECUTABLE  OR MOVABLE INSTRUCTIONS
*                                                                 *JLM*
BINDXONL B     ENDITALL
BPRINT90 B     PRINT90
BEG00    BE    G00
BG00     B     G00
BX00     B     X00
FOOTINIT BC    0,PRINT30
FOOTMVC  MVC   0(1,WORKREG2),0(DATAREG)
HEXON    MVC   PRINT75+1(1),TRANSLEN
HINAMOVE MVC   HIMEMBER(1),0(DATAREG)
LONAMOVE MVC   LOMEMBER(1),0(DATAREG)
MVC      MVC   PRTLINE+10(1),0(DATAREG)
MVIXFF   MVI   8(REG1),X'FF'
NOP4     BCR   0,0
         BCR   0,0
         BCR   0,0
         BCR   0,0
PARMPACK PACK  DW,0(1,DATAREG)
PARMTRT  TRT   0(1,DATAREG),NUMTEST
TRANSON  MVC   PRINT72+1(1),TRANSLEN
*                                                                 *JLM*
*  PARM OPTION DEFAULTS
*                                                                 *JLM*
PAGESIZE DC    F'0058'        LINECNT DEFAULT
PARMA    DC    CL1'Y'         INDEX ONLY DEFAULT
PARMD    DC    CL1'N'         DUPS DEFAULT
PARMDO   DC    CL1'N'         DUPS ONLY DEFAULT
PARME    DC    CL1'N'         EJECT DEFAULT
PARMG    DC    CL1'Y'         GEN'ERATE DEFAULT
PARMH    DC    CL1'N'         HEX DEFAULT
PARMI    DC    CL1'N'         INDEX DEFAULT
PARMT    DC    CL1'N'         TRANSLATE DEFAULT
PARMV    DC    CL1'Y'         VARSEQ DEFAULT
*                                                                 *JLM*
*  PARM CONSTANT STRINGS
*                                                                 *JLM*
PARMNO   DC    CL2'NO'
*                                                                 *JLM*
PARMBLKS DC    CL8'BLKSIZE('
PARMDUPO DS    0CL10
PARMDUPS  DC    CL4'DUPS'
          DC    CL6'(ONLY)'
PARMEJEC DC    CL5'EJECT'
PARMFOOT DC    CL8'FOOTING('
PARMGCNT DC    CL9'GENCOUNT('
PARMGCRD DC    CL12'GENCARDSIZE('
PARMGEN  DC    CL3'GEN'
PARMGFIX DC    CL15'FIXEDINDICATOR('
PARMGTNA DC    CL8'>MEMBER('
PARMGVAR DC    CL18'VARIABLEINDICATOR('
PARMHEX  DC    CL3'HEX'
PARMINDO DS    0CL11
PARMINDE  DC    CL5'INDEX'
          DC    CL6'(ONLY)'
PARMLINE DC    CL8'LINECNT('
PARMLTNA DC    CL8'<MEMBER('
PARMTRAN DC    CL9'TRANSLATE'
PARMVARS DC    CL6'VARSEQ'
PARM1ST  DC    CL6'FIRST('
*                                                                 *JLM*
EIGHTXFF DS    0D     * LEAVE BEFORE NUMTEST
NUMTEST  DC    240XL1'FF',10XL1'00',6XL1'FF'
PRTABLE  DC    XL16'40404040404040404040404040404040'  00 - 0F
         DC    XL16'40404040404040404040404040404040'  10 - 1F
         DC    XL16'40404040404040404040404040404040'  20 - 2F
         DC    XL16'40404040404040404040404040404040'  30 - 3F
         DC    XL16'40404040404040404040404B4C4D4E4F'  40 - 4F
         DC    XL16'504040404040404040405A5B5C5D5E5F'  50 - 5F
         DC    XL16'606140404040404040406A6B6C6D6E6F'  60 - 6F
         DC    XL16'404040404040404040407A7B7C7D7E7F'  70 - 7F
         DC    XL16'40818283848586878889404040404040'  80 - 8F
         DC    XL16'40919293949596979899404040404040'  90 - 9F
         DC    XL16'4040A2A3A4A5A6A7A8A9404040404040'  A0 - AF
         DC    XL16'B0404040404040404040BABB40404040'  B0 - BF
         DC    XL16'C0C1C2C3C4C5C6C7C8C9404040404040'  C0 - CF
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040'  D0 - DF
         DC    XL16'E040E2E3E4E5E6E7E8E9404040404040'  E0 - EF
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040'  F0 - FF
TR1TABLE DC    XL16'F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0'  00 - 0F
         DC    XL16'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'  10 - 1F
         DC    XL16'F2F2F2F2F2F2F2F2F2F2F2F2F2F2F2F2'  20 - 2F
         DC    XL16'F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3'  30 - 3F
         DC    XL16'F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4F4'  40 - 4F
         DC    XL16'F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5'  50 - 5F
         DC    XL16'F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6'  60 - 6F
         DC    XL16'F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7'  70 - 7F
         DC    XL16'F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8'  80 - 8F
         DC    XL16'F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9'  90 - 9F
         DC    XL16'C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1'  A0 - AF
         DC    XL16'C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2C2'  B0 - BF
         DC    XL16'C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3'  C0 - CF
         DC    XL16'C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4C4'  D0 - DF
         DC    XL16'C5C5C5C5C5C5C5C5C5C5C5C5C5C5C5C5'  E0 - EF
         DC    XL16'C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6'  F0 - FF
TR2TABLE DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  00 - 0F
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  10 - 1F
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  20 - 2F
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  30 - 3F
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  40 - 4F
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  50 - 5F
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  60 - 6F
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  70 - 7F
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  80 - 8F
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  90 - 9F
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  A0 - AF
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  B0 - BF
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  C0 - CF
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  D0 - DF
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  E0 - EF
         DC    XL16'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'  F0 - FF
LOMEMBER DC    XL8'0000000000000000'
HIMEMBER DC    XL8'FFFFFFFFFFFFFFFF'
*                                                                 *JLM*
CARDSIZE DC    F'72'
EXLST    DC    AL1(135),AL3(JFCBAREA)
FFFFFFF8 DC    XL4'FFFFFFF8'
FIRSTMEM DC    A(EIGHTXFF)
FWNEG1   DC    F'-1'
FW1      DC    F'1'
FW10     DC    F'10'
FW100    DC    F'100'
FW4      DC    F'4'
FW8      DC    F'8'
GENCDADR DS    F
GENNORED DC    F'0'
GENNOREQ DC    F'1000'
GENSZREQ DC    F'80000'
PAGECNT  DC    F'1'
APARMEOF DC    A(PARMDEFN)
PRINT1ST DC    F'-1'
PRINTED  DS    F
X000000  DC    XL3'000000'
CATCNT   DC    XL1'00'              * MUST BE AFTER X000000
*                                                                 *JLM*
BLKSI    DC    H'0'
HW50     DC    H'50'
HW72     DC    H'72'
HW80     DC    H'80'
PARMAEND DC    S(ENDITALL)
RECFMU   DC    S(U00)
RECFMV   DC    S(V00)
*                                                                 *JLM*
SKIPPER  DC    CL1'-'               * LEAVE BEFORE BLANKS
BLANKS   DC    CL121' '
CARD     DS    CL80                 * LEAVE IN FRONT OF CARDSAVE
CARDSAVE DS    CL80                 * LEAVE BEHIND CARD
CATFMT   DC    XL4'40202021'
DATEFMT  DC    XL6'212061202020'
DSNPERC  DC    CL8'%DSNAME%'
DSNPOUND DC    CL8'#DSNAME#'
FOOTLINE DC    CL121'0'
HDR      DC    CL37'-DATE=YYDDD      VOL=VVVVVV      DSN='
         DC    CL50' ',CL34'MEMBER=MMMMMMMM   PAGE ###,###,###'
HDR1     DC    CL61'1PDS#     DATA SET NAME'
         DC    CL60'VOLUME'
HDR2     DC    CL1'1',3CL32' MEMBER      PAGE#   PDS        '
         DC    CL24' MEMBER      PAGE#   PDS'
MEMPERC  DC    CL8'%MEMBER%'
MEMPOUND DC    CL8'#MEMBER#'
OLDTIOT  DC    XL2'FFFF'
PAGEFMT  DC    XL12'402020206B2020206B202021'
PCATCNT  DC    PL2'1'
TRANSLEN DC    X'64'
P1       DC    PL1'1'
READONE  DC    CL1'N'
UNDER    DC    CL1'+',120C'_'
*                                                                 *JLM*
DIR      DCB   DSORG=PS,MACRF=GL,DDNAME=SYSUT1,                        *
               RECFM=F,LRECL=256,BLKSIZE=256,                          *
               EXLST=EXLST,EODAD=DIREOF
*                                                                 *JLM*
PDS      DCB   DSORG=PO,MACRF=R,DDNAME=SYSUT1,                         *
               EODAD=PDSEOF
*                                                                 *JLM*
SYSIN    DCB   DSORG=PS,MACRF=GM,DDNAME=SYSIN,                         *
               RECFM=FB,LRECL=80,EODAD=PARMDEFN
*                                                                 *JLM*
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,                      *
               RECFM=FA,LRECL=121,BLKSIZE=121
*                                                                 *JLM*
SYSGEN   DCB   DSORG=PS,MACRF=GM,DDNAME=SYSGEN,                        *
               RECFM=FB,LRECL=80,EODAD=G40
*                                                                 *JLM*
SYSOUT   DCB   DSORG=PS,MACRF=PM,DDNAME=SYSOUT,                        *
               RECFM=FB,LRECL=80
*                                                                 *JLM*
SECURITY DC    CL38'COPYRIGHT(R) STELI INC. 1998 COPYRIGHT'
DSNAREA  DS    CL12800
*                                                                 *JLM*
         DCBD  DSORG=(PS,PO),DEVD=DA
*                                                                 *JLM*
         END   PDSGEN
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR                             00080000
//LKED.SYSIN DD *                                                       00090000
  SETSSI 98111400                                                       00100000
  ENTRY PDSGEN                                                          00110000
  NAME PDSGEN(R)                                                        00120000
//*
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=PDSGEN
//***                                                                           
//***  CHANGE ALL -PDS-TO-BE-PROCESSED- TO THE PDS TO BE USED AS INPUT          
//***  CHANGE ALL -SEQUENTIAL-OUTPUT-   TO YOUR OUTPUT FILE                     
//***  CHANGE ALL -YOUR-STEPLIB-        TO YOUR LOAD MODULE LIBRARY             
//***                                                                           
//***  GENERATE A SIMPLE LIST OF MEMBERS AND PDS NAMES                          
//***                                                                           
//CLEANUP    EXEC     PGM=IEFBR14                                               
//DEL01        DD       DSN=-SEQUENTIAL-OUTPUT-,                                
//             DISP=(MOD,DELETE,DELETE),UNIT=SYSDA,SPACE=(TRK,0)                
//***                                                                           
//***                                                                           
//PDSGEN     EXEC     PGM=PDSGEN,REGION=4M                                      
//SYSIN        DD       *                                                       
     DUPS                                                                       
     INDEX                                                                      
//STEPLIB      DD       DISP=SHR,DSN=-YOUR-STEPLIB-                             
//SYSPRINT     DD       SYSOUT=*                                                
//SYSUT1       DD       DISP=SHR,DSN=-PDS-TO-BE-PROCESSED-                      
//SYSGEN       DD       *                                                       
#MEMBER# IS IN %DSNAME%                                                         
//SYSOUT       DD       DSN=-SEQUENTIAL-OUTPUT-,DISP=(,CATLG),                  
//             UNIT=TEST,SPACE=(CYL,(1,1)),                                     
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920)                            
//***                                             