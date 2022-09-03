//FILEATTR  JOB (TSO),
//             'Install FILEATTR',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*
//*********************************************************************
//* This jobstream assembles FILEATTR, assembles the FINDMEM subrou-  *
//* tine, and compiles the BREAKDSN subroutine into a single load     *
//* module that may be called by a COBOL main program to retrieve     *
//* dataset information at run time.                                  *
//*********************************************************************
//*
//FILEATTR EXEC ASMFC,PARM.ASM='LOAD,NODECK,LIST,NOXREF'
//ASM.SYSGO DD DSN=&&LOADSET,UNIT=SYSDA,SPACE=(80,(200,50)),
//             DISP=(MOD,PASS)
//ASM.SYSIN DD *
*          DATA SET 764FILATTR AT LEVEL 008 AS OF 11/24/82
         SPACE 3
********************************************************************
*        SAVE REGISTERS AND ESTABLISH REGISTER 12 AS BASE REGISTER *
********************************************************************
         SPACE 3
FILEATTR START
         SAVE  (14,12),,*
         LR    12,15
         USING FILEATTR,12
         ST    13,SAVEAREA+4
         LA    13,SAVEAREA
         B     STARTIT
         SPACE
SAVEAREA DC    18F'0'
         SPACE 3
********************************************************************
*        EQUATE RESISTERS 0 THRU 15 TO MORE MEANINGFUL NAMES       *
********************************************************************
         SPACE 3
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
         EJECT
         SPACE 3
********************************************************************
*        INITIALIZATION AND ESTABLISHING DATA AREAS                *
*        CONTAINING OUR PARAMETERS                                 *
********************************************************************
         SPACE 3
STARTIT  DS    0D
         LR    R11,R1         SAVE REG 1 CONTENTS IN REG 11
         L     R9,0(R11)
         MVC   INDSN,0(R9)    PUT PARAMETER #1 INTO INDSN
         TM    32(R11),X'80'  IS DEVICE THE LAST PARAMETER PROVIDED?
         BO    NOINVOL        IF SO, BRANCH TO NOINVOL
         L     R9,36(R11)     PUT PARAMETER #10 INTO INVOL
         MVC   INVOL,0(R9)
         B     MAINLINE       BRANCH TO MAINLINE
NOINVOL  MVC   INVOL,=CL6'ABSENT'
         EJECT
         SPACE 3
********************************************************************
*        MAIN LINE LOGIC                                           *
********************************************************************
         SPACE 3
MAINLINE MVI   VOLSER,C' '
         MVC   VOLSER+1(5),VOLSER     MOVE SPACES TO VOLSER
         MVI   LRECL,C' '
         MVC   LRECL+1(4),LRECL       MOVE SPACES TO LRECL
         MVI   BLKSIZE,C' '
         MVC   BLKSIZE+1(4),BLKSIZE   MOVE SPACES TO BLKSIZE
         MVI   DSORG,C' '
         MVC   DSORG+1(1),DSORG       MOVE SPACES TO DSORG
         MVI   RECFM,C' '             MOVE SPACE TO RECFM
         BAL   R14,BREAKDSN           PERFORM ROUTINE TO BREAKDOWN
*                                       DSNAME FIELD INTO AN 8 BYTE
*                                       MEMBER NAME AND A 44 BYTE DSN
         SPACE 1
         CLC   VALIDSW(3),YESS        IS VALIDSW = 'YES'?
         BNE   SHUTDOWN               IF NOT, BRANCH TO SHUTDOWN
         SPACE 2
         CLC   INVOL,=C'ABSENT'       IS INVOL = 'ABSENT' ?
         BE    LOKAYT                 IF YES, BRANCH TO LOKAYT
         BAL   R14,EDITVOL            EDIT INVOL
         CLC   GOODVOL,=CL3'YES'      WAS INVOL VALID ?
         BE    MOVEVOL                IF SO, BRANCH TO MOVEVOL
         MVC   ERRMSG(30),MESS16      MOVE 'INVALID VOL SER GIVEN'
*                                       TO ERRMSG
         MVC   VALIDSW(3),NOPE        MOVE 'NO ' TO VALIDSW
         B     SHUTDOWN               BRANCH TO SHUTDOWN
MOVEVOL  MVC   OBTVOL,INVOL           MOVE INVOL TO OBTVOL
         B     OBTANE                 BRANCH TO OBTANE
LOKAYT   LOCATE CAMLIST1
         SPACE 2
         C     R15,=F'0'              COMPARE REG 15 TO BE = 0
         BE    RC0                    IF REG 15=0, BRANCH TO RC0
         MVC   ERRMSG(30),MESS1       MOVE 'DATASET NOT CATALOGED'
*                                       TO ERRMSG
         MVC   VALIDSW(3),NOPE        MOVE 'NO ' TO VALIDSW
         B     SHUTDOWN               BRANCH TO SHUTDOWN
RC0      CLC   BRKGDG,YESS            IS IT IN GDG EXPRESSED FORMAT ?
         BNE   CHKVOLCT               IF NOT, BRANCH TO CHKVOLCT
         BAL   R14,DSNSUFFX           PERFORM A ROUTINE TO DETERMINE
*                                        IF THIS DATASET IS A GDG ENTRY
         CLC   ISITAGDG,YESS          IS THE DATASET A GDG ?
         BE    CHKVOLSR               IF SO, BRANCH TO CHKVOLSR
         MVC   ERRMSG(30),MESS18      MOVE 'DATASET IS NOT A GDG'
*                                       TO ERRMSG
         MVC   VALIDSW(3),NOPE        MOVE 'NO ' TO VALIDSW
         B     SHUTDOWN               BRANCH TO SHUTDOWN
CHKVOLSR CLC   LOCAREA+6(6),=XL6'000000000000'
         BNE   CHKVOLCT
         MVC   ERRMSG(30),MESS1       MOVE 'DATASET NOT CATALOGUED'
*                                       TO ERRMSG
         MVC   VALIDSW(3),NOPE        MOVE 'NO ' TO VALIDSW
         B     SHUTDOWN               BRANCH TO SHUTDOWN
CHKVOLCT CLC   LOCAREA(2),HEX0001     VERIFY THAT THE NUMBER OF
*                                        VOLUMES COUNT IS 1
         BE    MATCH                  IF YES, BRANCH TO MATCH
         MVC   VALIDSW(3),NOPE        MOVE 'NO ' TO VALIDSW
         MVC   ERRMSG(30),MESS2       MOVE 'FILE IS ON MORE THAN
*                                       1 VOLUME' TO ERRMSG
         MVC   VOLSER(6),LOCAREA+6    LOAD 1ST VOL SER HOWEVER
         B     SHUTDOWN               BRANCH TO SHUTDOWN
MATCH    MVC   VOLSER(6),LOCAREA+6    LOAD VOLSER FROM CAMLST WORKAREA
         MVC   OBTVOL(6),VOLSER
         CLI   LOCAREA+4,X'80'        COMPARE LOCAREA+4 TO BE EQUAL
*                                       TO HEXADECIMAL 80  (THIS
*                                       INDICATES THAT THE DATASET
*                                       IS ON TAPE)
         BE    ONTAPE                 IF YES; BRANCH TO ONTAPE
         MVI   DEVICE,C'D'            MOVE 'D' (FOR DISK) TO DEVICE
         B     OBTANE                 BRANCH TO OBTANE
ONTAPE   MVI   DEVICE,C'T'            MOVE 'T' (FOR TAPE) TO DEVICE
         MVC   ERRMSG(30),MESS15      MOVE 'DATASET IS ON TAPE'
*                                       TO ERRMSG
         MVC   VALIDSW(3),NOPE        MOVE 'NO ' TO VALIDSW
         B     SHUTDOWN               BRANCH TO SHUTDOWN
         SPACE 2
OBTANE   OBTAIN CAMLIST2
         SPACE 2
         C     R15,=F'0'              COMPARE REG 15 TO BE = 0
         BE    RCODE0                 IF REG 15=0, BRANCH TO RCODE0
         C     R15,=F'4'              COMPARE REG 15 TO BE = 4
         BE    RCODE4                 IF REG 15=4, BRANCH TO RCODE4
         C     R15,=F'8'              COMPARE REG 15 TO BE = 8
         BE    RCODE8                 IF REG 15=8, BRANCH TO RCODE8
         MVC   ERRMSG(30),MESS5       MOVE 'OBTAIN MACRO RETURNED
*                                       RC 12/16' TO ERRMSG
         B     MOVENOPE
RCODE4   MVC   ERRMSG(30),MESS3       MOVE 'REQUIRED VOLUME NOT
*                                       MOUNTED' TO ERRMSG
         B     MOVENOPE
RCODE8   MVC   ERRMSG(30),MESS4       MOVE 'FORMAT-1 DSCB NOT FOUND
*                                       ON VOL' TO ERRMSG
MOVENOPE MVC   VALIDSW(3),NOPE        MOVE 'NO ' TO VALIDSW
         B     SHUTDOWN               BRANCH TO SHUTDOWN
RCODE0   MVC   VALIDSW(3),YESS        MOVE 'YES' TO VALIDSW
         MVI   ERRMSG,C' '
         MVC   ERRMSG+1(29),ERRMSG    MOVE SPACES TO ERRMSG
         MVI   DEVICE,C'D'            MOVE 'D' (FOR DISK) TO DEVICE
         MVC   VOLSER,OBTVOL
         SPACE 1
         BAL   R14,LRECLRT            PERFORM LRECL ROUTINE
         SPACE 1
         BAL   R14,BLKSIZRT           PERFORM BLKSIZE ROUTINE
         SPACE 1
         BAL   R14,DSORGRT            PERFORM DSORG ROUTINE
         SPACE 1
         BAL   R14,RECFMRT            PERFORM RECFM ROUTINE
         SPACE 1
         CLC   MEMCNTL(3),YESS        WAS MEMBER NAME PROVIDED
         BNE   SHUTDOWN               IF NOT, BRANCH TO SHUTDOWN
         SPACE 1
         CLC   DSORG,=CL2'PO'         DSORG = 'PO' ?
         BE    DOSRCH                 IF SO, BRANCH TO DOSRCH
         MVC   ERRMSG(30),MESS17      MOVE 'DATASET NOT PARTITIONED'
*                                        TO ERRMSG
         MVC   VALIDSW(3),NOPE        MOVE 'NO ' TO VALIDSW
         B     SHUTDOWN               BRANCH TO SHUTDOWN
DOSRCH   BAL   R14,SRCHDIR            PERFORM ROUTINE TO CALL 'FINDMEM'
*                                       WHICH SEARCHES THE DIRECTORY
*                                       FOR MEMBER NAME = FNDMEMBR
         B     SHUTDOWN               BRANCH TO SHUTDOWN
         EJECT
*
*
***********************************************************
*                                                         *
*  THIS ROUTINE IS RESPONSIBLE FOR CREATING THE PARAMETER *
*  LRECL THAT IS PASSED BACK TO THE CALLING PROGRAM       *
*                                                         *
***********************************************************
*
*
LRECLRT  ST    R14,SAVEIT2
         MVI   FULLWURD,X'00'              MOVE ALL BINARY ZEROS
         MVC   FULLWURD+1(3),FULLWURD        TO FULLWURD
         MVC   FULLWURD+2(2),OBTAREA+44    MOVE LRECL WHICH IS 2 BYTES
*                                            IN LENGTH BINARY TO LOW
*                                            ORDER 2 BYTES OF FULLWURD
         L     R9,FULLWURD                 LOAD REG 9 WITH FULLWURD
         CVD   R9,DUBLWURD                 CONVERT REG 9 CONTENTS TO
*                                            PACKED DECIMAL AND
*                                            PLACE INTO DUBLWURD
         UNPK  LRECL(5),DUBLWURD+5(3)      CONVERT PACKED DECIMAL IN
*                                            DUBLWURD+5 (FOR A LENGTH
*                                            OF 3 BYTES) AND PLACE
*                                            INTO LRECL (FOR A LENGTH
*                                            OF 5 BYTES)
         OI    LRECL+4,X'F0'               GET RID OF THE SIGN OF
*                                            LRECL (I.E. CHANGE HIGH
*                                            ORDER 4 BITS OF LOW ORDER
*                                            BYTE OF LRECL TO ALL ONES
         L     R14,SAVEIT2
         BR    14
         EJECT
*
*
***********************************************************
*                                                         *
*  THIS ROUTINE IS RESPONSIBLE FOR CREATING THE PARAMETER *
*  BLKSIZE THAT IS PASSED BACK TO THE CALLING PROGRAM     *
*                                                         *
***********************************************************
*
*
BLKSIZRT ST    R14,SAVEIT3
         MVI   FULLWURD,X'00'              MOVE ALL BINARY ZEROS
         MVC   FULLWURD+1(3),FULLWURD        TO FULLWURD
         MVC   FULLWURD+2(2),OBTAREA+42    MOVE BLKSIZE WHICH IS
*                                            2 BYTES BINARY TO LOW
*                                            ORDER 2 BYTES OF FULLWURD
         L     R9,FULLWURD                 LOAD REG 9 WITH FULLWURD
         CVD   R9,DUBLWURD                 CONVERT REG 9 CONTENTS TO
*                                            PACKED DECIMAL AND
*                                            PLACE INTO DUBLWURD
         UNPK  BLKSIZE(5),DUBLWURD+5(3)    CONVERT PACKED DECIMAL IN
*                                            DUBLWURD+5 (FOR A LENGTH
*                                            OF 3 BYTES) AND PLACE
*                                            INTO BLKSIZE (FOR A
*                                            LENGTH OF 5 BYTES)
         OI    BLKSIZE+4,X'F0'             GET RID OF THE SIGN OF
*                                            BLKSIZE (I.E. CHANGE HIGH
*                                            ORDER 4 BITS OF LOW ORDER
*                                            BYTE OF BLKSIZE TO ALL
*                                            ONES)
         L     R14,SAVEIT3
         BR    14
         EJECT
*
*
***********************************************************
*                                                         *
*  THIS ROUTINE IS RESPONSIBLE FOR CREATING THE PARAMETER *
*  DSORG THAT IS PASSED BACK TO THE CALLING PROGRAM       *
*                                                         *
***********************************************************
*
*
DSORGRT  ST    R14,SAVEIT4
         TM    OBTAREA+38,B'10000000'
         BO    MOVEIT1
         TM    OBTAREA+38,B'01000000'
         BO    MOVEIT2
         TM    OBTAREA+38,B'00100000'
         BO    MOVEIT3
         TM    OBTAREA+38,B'00010000'
         BO    MOVEIT4
         TM    OBTAREA+38,B'00001000'
         BO    MOVEIT5
         TM    OBTAREA+38,B'00000100'
         BO    MOVEIT6
         TM    OBTAREA+38,B'00000010'
         BO    MOVEIT7
         B     MOVEIT8
MOVEIT1  MVC   DSORG(2),ORGTABLE        MOVE 'IS' TO DSORG
         B     EXITDSRG
MOVEIT2  MVC   DSORG(2),ORGTABLE+2      MOVE 'PS' TO DSORG
         B     EXITDSRG
MOVEIT3  MVC   DSORG(2),ORGTABLE+4      MOVE 'DA' TO DSORG
         B     EXITDSRG
MOVEIT4  MVC   DSORG(2),ORGTABLE+6      MOVE 'CX' TO DSORG
         B     EXITDSRG
MOVEIT5  MVC   DSORG(2),ORGTABLE+8      MOVE 'CQ' TO DSORG
         B     EXITDSRG
MOVEIT6  MVC   DSORG(2),ORGTABLE+10     MOVE 'MQ' TO DSORG
         B     EXITDSRG
MOVEIT7  MVC   DSORG(2),ORGTABLE+12     MOVE 'PO' TO DSORG
         B     EXITDSRG
MOVEIT8  CLI   OBTAREA+38,X'00'
         BNE   DSORGU
         CLC   OBTAREA+96(5),=XL5'0000000000'
         BNE   DSORGU
         MVC   DSORG(2),ORGTABLE+16     MOVE 'VS' TO DSORG
         B     EXITDSRG
DSORGU   MVC   DSORG(2),ORGTABLE+14     MOVE 'U ' TO DSORG
EXITDSRG L     R14,SAVEIT4
         BR    14
         EJECT
*
*
***********************************************************
*                                                         *
*  THIS ROUTINE IS RESPONSIBLE FOR CREATING THE PARAMETER *
*  RECFM THAT IS PASSED BACK TO THE CALLING PROGRAM       *
*                                                         *
***********************************************************
*
*
RECFMRT  ST    R14,SAVEIT5
         TM    OBTAREA+40,B'11000000'
         BO    RECFMU
         TM    OBTAREA+40,B'01000000'
         BO    RECFMV
         MVI   RECFM,C'F'
         B     EXITRCFM
RECFMU   MVI   RECFM,C'U'
         B     EXITRCFM
RECFMV   MVI   RECFM,C'V'
EXITRCFM L     R14,SAVEIT5
         BR    14
         EJECT
*
******************************************************************
*                                                                *
*        THIS ROUTINE BREAKS DOWN A DSN OF THE FORM              *
*        DSNAME OF THE PDS(MEMBER NAME) INTO TWO SEPARATE        *
*        DATA FIELDS FNDDSN AND FNDMEMBR WHERE FNDDSN IS         *
*        44 BYTES IN LENGTH (PADDED WITH SPACES) AND FNDMEMBR    *
*        IS 8 BYTES IN LENGTH (ALSO PADDED WITH SPACES)          *
*                                                                *
******************************************************************
*
BREAKDSN ST    R14,SAVEIT1            SAVE REG 14 ADDRESS IN SAVEIT1
         MVC   BRKINDSN,INDSN
         CALL  BREAKDSN,(BRKDSNIO),VL
         MVC   FNDDSN,BRKOTDSN
         MVC   FNDMEMBR,BRKMEMBR
         MVC   MEMCNTL,BRKMEMPR
         MVC   VALIDSW,BRKVALID
         MVC   ERRMSG,BRKERROR
         L     R14,SAVEIT1
         BR    R14
         EJECT
*
******************************************************************
*                                                                *
*        THIS ROUTINE IS RESPONSIBLE FOR VALIDATING THAT A       *
*        GIVEN BYTE OF A MEMBER NAME IS VALID.  THAT IS;         *
*        IS IT EQUAL TO A-Z, 0-9 OR ONE OF THE THREE NATIONAL    *
*        CHARACTERS @, $, OR #                                   *
*                                                                *
******************************************************************
*
CHEKBYTE ST    R14,SAVEIT6            SAVE REG 14 ADDRESS IN SAVEIT6
         MVC   OKBYTE,YESS            MOVE 'YES' TO OKBYTE
         L     R9,=A(OKCHARS)         LOAD ADDR OF OKCHARS INTO REG 9
CHEK1    CLI   0(R9),X'FF'            END OF TABLE OKCHARS REACHED?
         BE    NOGOOD                 IF YES, BRANCH TO NOGOOD
         CLC   0(1,R6),0(R9)          MEMBER NAME BYTE = TABLE ENTRY?
         BE    EXITCHEK               IF YES, BRANCH TO EXITCHEK
         A     R9,=F'1'               ADD 1 TO INDEX REGISTER 9
         B     CHEK1                  BRANCH TO CHEK1
NOGOOD   MVC   OKBYTE,NOPE            MOVE 'NO ' TO OKBYTE
EXITCHEK L     R14,SAVEIT6
         BR    R14
         EJECT
*
*
***********************************************************
*                                                         *
*  THIS ROUTINE IS RESPONSIBLE FOR CALLING THE SUBROUTINE *
*  FINDMEM WHICH DYNAMICALLY FREES AND ALLOCATES THE      *
*  FILE WITH DSN = FNDDSN TO DDNAME FINDMEDD AND THEN     *
*  SEARCHES THE DIRECTORY FOR MEMBER NAME = FNDMEMBR      *
*                                                         *
*  THE TWO BYTE FIELD INDICATR RETURNS VALUES WHICH       *
*  RELATE TO THE SUCCESS OR NON SUCCESS OF THAT SEARCH    *
*                                                         *
***********************************************************
*
*
SRCHDIR  ST    R14,SAVEIT7            SAVE REG 14 ADDRESS IN SAVEIT7
         CALL  FINDMEM,(IOAREA,VOLSER),VL
         CLC   INDICATR,C00           INDICATR = '00'?
         BE    EXITFIND               IF YES, BRANCH TO EXITFIND
         MVC   VALIDSW(3),NOPE        MOVE 'NO ' TO VALIDSW
         CLC   INDICATR,C04           INDICATR = '04'?
         BE    INDIC04                IF YES, BRANCH TO INDIC04
         CLC   INDICATR,C08           INDICATR = '08'?
         BE    INDIC08                IF YES, BRANCH TO INDIC08
         CLC   INDICATR,C20           INDICATR = '20'?
         BE    INDIC20                IF YES, BRANCH TO INDIC20
         CLC   INDICATR,C24           INDICATR = '24'?
         BE    INDIC24                IF YES, BRANCH TO INDIC24
         MVC   ERRMSG(30),MESS7       MOVE 'PROBLEM IN BAL PGM
*                                       FILEATTR' TO ERRMSG
         B     EXITFIND               BRANCH TO EXITFIND
INDIC04  MVC   ERRMSG(30),MESS8       MOVE 'MEMBER NAME DOES NOT EXIST
*                                       TO ERRMSG
         B     EXITFIND               BRANCH TO EXITFIND
INDIC08  MVC   ERRMSG(30),MESS9       MOVE 'UNSUCCESSFUL ALLOCATION'
*                                       TO ERRMSG
         B     EXITFIND               BRANCH TO EXITFIND
INDIC20  MVC   ERRMSG(30),MESS10      MOVE 'I/O ERROR READING DIRECTRY
*                                       TO ERRMSG
         B     EXITFIND               BRANCH TO EXITFIND
INDIC24  MVC   ERRMSG(30),MESS11      MOVE 'UNSUCESSFUL FREE OF DDNAME
*                                       TO ERRMSG
EXITFIND L     R14,SAVEIT7
         BR    R14
         EJECT
*
*
***********************************************************
*                                                         *
*        THIS ROUTINE IS RESPONSIBLE FOR EDITING INVOL    *
*                                                         *
*        IF INVOL MEETS THE CRITERIA FOR BEING A VALID    *
*        VOLUMER SERIAL NUMBER (6 BYTES LONG, ALL         *
*        NUMERICS AND ALPHABETICS), THEN THIS ROUTINE     *
*        WILL MOVE 'YES' TO GOODVOL ELSE MOVE 'NO '       *
*        TO GOODVOL                                       *
*                                                         *
***********************************************************
*
*
EDITVOL  ST    R14,SAVEIT8            SAVE REG 14 ADDRESS IN SAVEIT8
         MVC   GOODVOL,=CL3'YES'
         MVC   EDITBYTE(1),INVOL
         BAL   R14,EDIT1BYT
         CLC   GOODVOL,=CL3'YES'
         BNE   EXITEVOL
         MVC   EDITBYTE(1),INVOL+1
         BAL   R14,EDIT1BYT
         CLC   GOODVOL,=CL3'YES'
         BNE   EXITEVOL
         MVC   EDITBYTE(1),INVOL+2
         BAL   R14,EDIT1BYT
         CLC   GOODVOL,=CL3'YES'
         BNE   EXITEVOL
         MVC   EDITBYTE(1),INVOL+3
         BAL   R14,EDIT1BYT
         CLC   GOODVOL,=CL3'YES'
         BNE   EXITEVOL
         MVC   EDITBYTE(1),INVOL+4
         BAL   R14,EDIT1BYT
         CLC   GOODVOL,=CL3'YES'
         BNE   EXITEVOL
         MVC   EDITBYTE(1),INVOL+5
         BAL   R14,EDIT1BYT
         B     EXITEVOL
*
*
*
EDIT1BYT ST    R14,SAVEIT9            SAVE REG 14 ADDRESS IN SAVEIT9
         CLI   EDITBYTE,C'0'
         BL    NEXTEST
         CLI   EDITBYTE,C'9'
         BH    BADINVOL
         B     EXITEDT1
NEXTEST  CLI   EDITBYTE,C'A'
         BL    BADINVOL
         CLI   EDITBYTE,C'Z'
         BH    BADINVOL
         B     EXITEDT1
BADINVOL MVC   GOODVOL,=CL3'NO '
EXITEDT1 L     R14,SAVEIT9
         BR    R14
*
*
*
EXITEVOL L     R14,SAVEIT8
         BR    R14
         EJECT
*
*
***********************************************************
*                                                         *
*        THIS ROUTINE IS RESPONSIBLE FOR DETERMINING      *
*        WHETHER THE DATASET NAME CONTAINED IN FNDDSN     *
*        IS A GDG (DETERMINED BY ITS LAST QUALIFIER       *
*        BEING .GXXXXVXX)                                 *
*                                                         *
*        IF IT IS, THIS ROUTINE WILL MOVE 'YES' TO        *
*        ISITAGDG ELSE IT WILL MOVE 'NO ' TO ISITAGDG     *
*                                                         *
***********************************************************
*
*
DSNSUFFX ST    R14,SAVEIT10           SAVE REG 14 ADDRESS IN SAVEIT10
         LA    R3,FNDDSN              R3==> ADDRESS OF FNDDSN
         A     R3,=F'43'              ADD 43 TO REGISTER 3
         LA    R4,0                   R4==> 0
SUFFIX1  C     R4,=F'43'              HAVE WE INDEXED THRU FNDDSN?
         BH    NOTAGDG                IF SO, BRANCH TO NOTAGDG
         CLI   0(R3),C'.'             HAVE WE FOUND THE LAST PERIOD ?
         BE    SUFFIX2                IF SO, BRANCH TO SUFFIX2
         A     R4,=F'1'               ADD 1 TO REG 4
         S     R3,=F'1'               SUBTRACT 1 FROM REG 3
         B     SUFFIX1                BRANCH TO SUFFIX1
SUFFIX2  A     R3,=F'1'               ADD 1 TO REG 3
         CLI   0(R3),C'G'
         BNE   NOTAGDG
         A     R3,=F'1'
         CLI   0(R3),C'0'
         BL    NOTAGDG
         CLI   0(R3),C'9'
         BH    NOTAGDG
         A     R3,=F'1'
         CLI   0(R3),C'0'
         BL    NOTAGDG
         CLI   0(R3),C'9'
         BH    NOTAGDG
         A     R3,=F'1'
         CLI   0(R3),C'0'
         BL    NOTAGDG
         CLI   0(R3),C'9'
         BH    NOTAGDG
         A     R3,=F'1'
         CLI   0(R3),C'0'
         BL    NOTAGDG
         CLI   0(R3),C'9'
         BH    NOTAGDG
         A     R3,=F'1'
         CLI   0(R3),C'V'
         BNE   NOTAGDG
         A     R3,=F'1'
         CLI   0(R3),C'0'
         BL    NOTAGDG
         CLI   0(R3),C'9'
         BH    NOTAGDG
         A     R3,=F'1'
         CLI   0(R3),C'0'
         BL    NOTAGDG
         CLI   0(R3),C'9'
         BH    NOTAGDG
         MVC   ISITAGDG,=CL3'YES'     MOVE 'YES' TO ISITAGDG
         B     EXITSUFF               BRANCH TO EXITSUFF
NOTAGDG  MVC   ISITAGDG,=CL3'NO '     MOVE 'NO ' TO ISITAGDG
EXITSUFF L     R14,SAVEIT10
         BR    R14
         EJECT
         SPACE 3
********************************************************************
*        LOAD PARAMETERS 2 - 8 AND BRANCH BACK TO CALLING PROGRAM  *
********************************************************************
         SPACE 3
SHUTDOWN LA    R15,0                  SET RC = 0
         L     R9,4(R11)
         MVC   0(3,R9),VALIDSW        MOVE VALIDSW TO PARAMETER #2
         L     R9,8(R11)
         MVC   0(30,R9),ERRMSG        MOVE ERRMSG  TO PARAMETER #3
         L     R9,12(R11)
         MVC   0(6,R9),VOLSER         MOVE VOLSER  TO PARAMETER #4
         L     R9,16(R11)
         MVC   0(5,R9),LRECL          MOVE LRECL   TO PARAMETER #5
         L     R9,20(R11)
         MVC   0(5,R9),BLKSIZE        MOVE BLKSIZE TO PARAMETER #6
         L     R9,24(R11)
         MVC   0(2,R9),DSORG          MOVE DSORG   TO PARAMETER #7
         L     R9,28(R11)
         MVC   0(1,R9),RECFM          MOVE RECFM   TO PARAMETER #8
         L     R9,32(R11)
         MVC   0(1,R9),DEVICE         MOVE DEVICE  TO PARAMETER #9
         L     R13,4(R13)
         RETURN (14,12),RC=(15)
         EJECT
         SPACE 3
********************************************************************
*        DATA AREAS                                                *
********************************************************************
         SPACE 3
         DS    0F
INDSN    DS    CL44
CAMLIST1 CAMLST NAME,FNDDSN,,LOCAREA
LOCAREA  DS    0D
         DS    265C
         DS    0D
CAMLIST2 CAMLST SEARCH,FNDDSN,OBTVOL,OBTAREA
OBTAREA  DS    0D
         DS    140C
         DS    0D
OBTVOL   DS    CL6
VALIDSW  DS    CL3
ISITAGDG DS    CL3
ERRMSG   DS    CL30
VOLSER   DS    CL6
LRECL    DS    CL5
BLKSIZE  DS    CL5
DSORG    DS    CL2
RECFM    DS    CL1
DEVICE   DS    CL1
INVOL    DS    CL6
HEX0001  DC    X'0001'
YESS     DC    C'YES'
NOPE     DC    C'NO '
*
*   B R E A K D S N   P A R A M E T E R S
*
BRKDSNIO DS    0CL135
BRKINDSN DS    CL44
BRKOTDSN DS    CL44
BRKMEMBR DS    CL8
BRKMEMPR DS    CL3
BRKGDG   DS    CL3
BRKVALID DS    CL3
BRKERROR DS    CL30
*
*
*
MESS1    DC    C'DATASET NOT CATALOGED         '
MESS2    DC    C'FILE IS ON MORE THAN 1 VOLUME '
MESS3    DC    C'REQUIRED VOLUME NOT MOUNTED   '
MESS4    DC    C'FORMAT-1 DSCB NOT FOUND ON VOL'
MESS5    DC    C'OBTAIN MACRO RETURNED RC 12/16'
MESS6    DC    C'INVALID SYNTAX FOR DSNAME     '
MESS7    DC    C'PROBLEM IN BAL PGM FILEATTR   '
MESS8    DC    C'MEMBER NAME NOT FOUND         '
MESS9    DC    C'UNSUCCESFUL DYNAMIC ALLOCATION'
MESS10   DC    C'I/O ERROR READING DIRECTORY   '
MESS11   DC    C'UNSUCCESSFUL FREE OF DDNAME   '
MESS13   DC    C'MEMBER NAME TOO LONG          '
MESS14   DC    C'INVALID MEMBER NAME           '
MESS15   DC    C'DATASET IS ON TAPE            '
MESS16   DC    C'INVALID VOL SER GIVEN         '
MESS17   DC    C'DATASET NOT PARTITIONED       '
MESS18   DC    C'DATASET IS NOT A GDG          '
ORGTABLE DC    C'ISPSDACXCQMQPOU VS'
         DS    0D
DUBLWURD DS    D
FULLWURD DS    F
SAVEIT1  DS    F
SAVEIT2  DS    F
SAVEIT3  DS    F
SAVEIT4  DS    F
SAVEIT5  DS    F
SAVEIT6  DS    F
SAVEIT7  DS    F
SAVEIT8  DS    F
SAVEIT9  DS    F
SAVEIT10 DS    F
GOODVOL  DS    CL3
OKBYTE   DS    CL3
MEMCNTL  DS    CL3
EDITBYTE DS    CL1
IOAREA   DS    0CL56
         DS    CL2
FNDMEMBR DS    CL8
FNDDSN   DS    CL44
INDICATR DS    CL2
CONSTPO  DC    CL2'PO'
C00      DC    CL2'00'
C04      DC    CL2'04'
C08      DC    CL2'08'
C20      DC    CL2'20'
C24      DC    CL2'24'
OKCHARS  DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
         DC    C'0123456789'
         DC    C'#@$'
         DC    X'FF'
         END
//FINDMEM EXEC ASMFC,PARM.ASM='LOAD,NODECK,LIST,NOXREF'
//ASM.SYSGO DD DSN=&&LOADSET,UNIT=SYSDA,SPACE=(80,(200,50)),
//             DISP=(MOD,PASS)
//ASM.SYSIN DD *
         TITLE 'FINDMEM -- SEARCH PDS FOR A PARTICULAR MEMBER'
**********************************************************************
*        THIS PROGRAM IS A SUBROUTINE WHICH DYNAMICALLY ALLOCATES
*        THE FILE WITH DSN PASSED TO THIS PROGRAM AND THEN SEARCHES
*        THE DIRECTORY TO DETERMINE WHETHER THE MEMBER NAME PASSED
*        TO THIS PROGRAM EXISTS.
*
*        TO CALL THIS PROGRAM FROM A COBOL PROGRAM, DO THE FOLLOWING:
*
*           IF YOUR PDS IS CATALOGUED,
*
*               MOVE YOUR DSN TO DSNAME OF FINDMEM-I-O-AREA.
*               MOVE YOUR MEMBER NAME
*                   TO MEMBER-NAME OF FINDMEM-I-O-AREA.
*               CALL 'FINDMEM' USING FINDMEM-I-O-AREA.
*
*                    WHERE:
*
*                          01  FINDMEM-I-O-AREA.
*                              05  OP-SYS VALUE 'VS'  PIC X(2).
*                              05  MEMBER-NAME        PIC X(8).
*                              05  DSNAME             PIC X(44).
*                              05  SUCCESS-INDICATOR  PIC 99.
*
*
*           IF YOUR PDS IS NOT CATALOGUED,
*
*               MOVE YOUR DSN TO DSNAME OF FINDMEM-I-O-AREA.
*               MOVE YOUR MEMBER NAME
*                   TO MEMBER-NAME OF FINDMEM-I-O-AREA.
*               MOVE THE VOLUME SERIAL NUMBER TO VOL-SER-NO.
*               CALL 'FINDMEM' USING FINDMEM-I-O-AREA, VOL-SER-NO.
*
*                    WHERE:
*
*                          01  FINDMEM-I-O-AREA (SEE ABOVE)
*                          01  VOL-SER-NO             PIC X(6).
*
*
*
*             SUCCESS-INDICATOR   MEANING
*             -----------------   -----------------------------------
*
*                    00           SUCCESSFUL ALLOCATION AND FIND
*                    04           SUCCESSFULLY ALLOCATED FILE BUT
*                                   COULDN'T FIND MEMBER IN DIRECTORY
*                    08           UNSUCCESSFUL ALLOCATION
*                    12           DSN PASSED TO THIS PGM IS SPACES
*                    16           MEMBER NAME PASSED IS SPACES
*                    20           I/O ERROR READING DIRECTORY
*                    24           UNSUCCESSFUL FREE OF DDNAME FINDMEDD
*                    28           INVALID VOL SER PASSED TO THIS PGM
*
*
*        ASSIGNMENT OF REGISTERS
*        -----------------------
*
*        REGISTER      USAGE
*        --------      -------------------------------------------
*
*           1          ADDRESS OF PARAMETER LIST
*           2          NOT USED
*           3          NOT USED
*           4          NOT USED
*           5          RETURN REG USED IN ALL BR AND LINK INSTRUCTIONS
*           6          NOT USED
*           7          WORK REGISTER
*           8          WORK REGISTER
*           9          WORK REGISTER
*           10         SAVE RETURN CODE VALUE
*           11         SAVE REGISTER 1
*           12         BASE REGISTER
*           13         ADDRESS OF SAVE AREA
*           14         RETURN ADDRESS
*           15         ENTRY POINT ADDRESS
*
*---------------------------------------------------------------------
*
*  ACTIVITY LOG:
*
*    V2.0  -   GORDON J. SCHILLINGER    DIS/CSD                09/7/88
*              MODIFIED TO MAKE FINDMEM RE-ENTERANT
*
**********************************************************************
         EJECT
         MACRO
&LAB1    ENTERR  &SA=SAVEAREA,&WA=WORKAREA,&WL=WORKLEN,&LEVEL=,&R=,   XX
               &CLEAR=YES
         MNOTE ' CLEAR=&CLEAR,SA=&SA,WA=&WA,WL=&WL,LEVEL=&LEVEL'
&LAB1    CSECT
         SAVE  (14,12),,&LAB1-&LEVEL
         LR    R12,R15            HOPE HE KNOWS WHAT HE'S DOING
         USING &LAB1,R12
         LR    R10,R1             SAVE PARM PTR R10->PARM PTR
         L     R0,=A(&WL)         R0=GET LENGTH
         GETMAIN R,LV=(0)         R1->WORKAREA
         LR    R11,R13            R11->CALLERS SAVEAREA
         LR    R13,R1             R13->WORKAREA
         USING &WA.,R13
         AIF   ('&CLEAR' NE 'YES').NCLEAR
         L     R15,=A(&WL)        R0=GET LENGTH
         S     R15,=F'72'         SKIP REGS
         MOVE  72(13),(15),0,0
.NCLEAR  ANOP
         ST    R11,&SA.+4         SAVE HIS SAVEAREA PTR
         LA    R13,&SA            R13->SAVEAREA (MINE)
         ST    R13,8(,R11)        MINE IN HIS
         LR    R11,R1             R11->WORKAREA IN CASE NOT SAME AS R13
*                       WORKAREA ADDR IS STILL R13 FOR
*                       THE ASSEMBLER - IF DIFFERENT FROM R13
*                       THEN USE: DROP R13 AND USING &WA.,R11
         LR    R1,R10             RESTORE PARM PTR PTR
         AIF   ('&R' EQ 'NO').NRE
         REGEQU
.NRE     ANOP
         MEND
         MACRO
         REGEQU
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
         MEND
         MACRO
&L1      MOVE  &TO,&TL,&FROM,&FL,&PAD=
         LCLA  &NL,&UL,&VL,&RP,&RP2,&RC
         LCLC  &TO$,&TL$,&FROM$,&FL$
         LCLC  &R1(2),&R2(2),&RN1,&RN2
&RC      SETA  1
&RP      SETA  2
&RP2     SETA  &RP+1
.RPLOOP  ANOP
&RN1     SETC  '(R'.'&RP'.')'
&RN2     SETC  '(R'.'&RP2'.')'
         AIF   ('&TO' EQ '&RN1').CKP2
         AIF   ('&TL' EQ '&RN1').CKP2
         AIF   ('&FROM' EQ '&RN1').CKP2
         AIF   ('&FL' EQ '&RN1').CKP2
         AIF   ('&TO' EQ '&RN2').CKP2
         AIF   ('&TL' EQ '&RN2').CKP2
         AIF   ('&FROM' EQ '&RN2').CKP2
         AIF   ('&FL' EQ '&RN2').CKP2
&R1(&RC) SETC  'R'.'&RP'
&R2(&RC) SETC  'R'.'&RP2'
&RC      SETA  &RC+1
         AIF   (&RC EQ 3).GP
.CKP2    ANOP
&RP      SETA  &RP+2
&RP2     SETA  &RP+1
         AIF   (&RP LT 10).RPLOOP
         MNOTE 8,'NO REGISTER PAIRS FREE FOR MVCL INSTRUCTION'
         MEXIT
.GP      ANOP
&TO$     SETC  '&TO'
&TL$     SETC  '&TL'
&FROM$   SETC  '&FROM'
&FL$     SETC  '&FL'
         AIF   ('&TO'(1,1) NE '(').TLC
&TO$     SETC  '0&TO'
.TLC     AIF   ('&TL'(1,1) NE '(').FC
&TL$     SETC  '0&TL'
.FC      AIF   ('&FROM'(1,1) NE '(').FLC
&FROM$   SETC  '0&FROM'
.FLC     AIF   (T'&FL EQ 'O').OO
         AIF   ('&FL'(1,1) NE '(').OO
&FL$     SETC  '0&FL'
.OO      ANOP
&L1      STM   14,12,12(13)
         LA    &R1(1),&TO$
         LA    &R2(1),&TL$
         LA    &R1(2),&FROM$
         AIF   (T'&FL EQ 'O').UTL
         LA    &R2(2),&FL$
         AGO   .PC
.UTL     LA    &R2(2),&TL$
.PC      AIF   ('&PAD' EQ '').NPC
         ICM   &R2(2),8,=&PAD
.NPC     MVCL  &R1(1),&R1(2)
         LM    14,12,12(13)
         MEND
FINDMEM  ENTERR LEVEL=V_2.0
         LR    R11,R1
********************************************************************
*        INITIALIZE DYNAMIC AREAS
********************************************************************
         OI    ROPPL,X'80'
         OI    RCPPL,X'80'
         MVC   CBLOK(CINITL),CINIT
         MVC   PDSFILE(PDSFLEN),PDSFINIT
         MVC   FREEBLOK(FREELEN),FREEINIT
         MVC   ALOCBLOK(ALOCLEN),ALOCINIT
         LA    R1,FREERB
         STCM  R1,7,FREEPARM+1
         LA    R1,FREETUPL
         ST    R1,ERCODE+4
         LA    R1,FREEUNT1
         ST    R1,FREETUPL
         LA    R1,FREEUNT2
         STCM  R1,7,FREETUPL+5
         LA    R1,DD1RB
         STCM  R1,7,DD1PARM+1
         LA    R1,DD1TUPL
         ST    R1,DD1RB+8
         LA    R1,DD1UNIT1
         LA    R2,DD1UNIT2
         STM   R1,R2,DD1TUPL
         LA    R1,DD1UNIT3
         STCM  R1,7,DD1HIGH3+1
         LA    R1,DD1UNIT4
         STCM  R1,7,DD1HIGH4+1
         LA    R1,DD1UNIT5
         STCM  R1,7,DD1HIGH5+1
*
         L     R9,0(R11)
         MVC   IOAREA,0(R9)
         TM    0(R11),X'80'   CALLED W/ SINGLE PARM?
         BO    NOINVOL        IF SO, BRANCH TO NOINVOL
         L     R9,4(R11)      R9==> ADDRESS OF VOL SER NO
         MVC   INVOL,0(R9)    PUT VOL SER NO INTO INVOL
         B     MAINLINE       BRANCH TO MAINLINE
NOINVOL  MVC   INVOL,=CL6'ABSENT'
         EJECT
         SPACE 3
*
********************************************************************
*                                                                  *
*        M A I N   L I N E   L O G I C                             *
*                                                                  *
********************************************************************
*
MAINLINE BAL   5,LOADSN            LOAD DATA AREAS DSNLEN AND DSNAME
         CLC   PROCESSW,CYES       WERE DATA AREAS SUCCESSFULLY LOADED?
         BNE   SHUTDOWN            IF NOT, BRANCH TO SHUTDOWN
         CLC   INVOL,=CL6'ABSENT'  WAS VOL SER PROVIDED ?
         BE    ALLOKATE            IF NOT, BRANCH TO ALLOKATE
         BAL   5,EDITVOL           EDIT THE VOL SER PROVIDED
         CLC   GOODVOL,CYES        WAS THE VOL SER VALID ?
         BNE   SHUTDOWN            IF NOT, BRANCH TO SHUTDOWN
ALLOKATE BAL   5,DYNAMALC          PERFORM DYNAMIC ALLOCATION
         CLC   PROCESSW,CYES       WAS DYNAMIC ALLOCATION SUCCESSFUL?
         BE    OPENFIL             IF YES, GO TO OPENFIL
         B     SHUTDOWN            BRANCH TO SHUTDOWN
OPENFIL  OPEN  (PDSFILE,(INPUT)),MF=(E,ROPPL)  OPEN PDSFILE
         MVC   FILOPEN,CYES        MOVE 'YES' TO FILOPEN
         BAL   5,SERCHPDS          PERFORM SEARCH DIRECTORY FOR MEMBER
         CLC   PROCESSW,CYES       WAS MEMBER FOUND?
         BE    FOUND               IF YES, GO TO FOUND
         B     SHUTDOWN            BRANCH TO SHUTDOWN
FOUND    MVC   INDICATR,C00        MOVE 00 TO SUCCESS-INDICATOR
         LA    R10,0               MOVE 0 TO REGISTER 10
         B     SHUTDOWN            BRANCH TO SHUTDOWN
         EJECT
         SPACE 3
*
********************************************************************
*                                                                  *
*        THIS ROUTINE IS RESPONSIBLE FOR LOADING THE 2 DATA AREAS  *
*        DSNLEN AND DSNAME.  THE DSN THAT IS PASSED TO THIS PGM    *
*        IS MOVED TO DSNAME.  THIS ROUTINE THEN CALCULATES THE     *
*        SIGNIFICANT LENGTH OF DSNAME AND MOVE THAT VALUE TO       *
*        DSNLEN (EXPRESSED IN BINARY)                              *
*                                                                  *
********************************************************************
*
LOADSN   ST    R5,SAVEIT1            SAVE REG 5 IN SAVEIT1
         MVC   DSNAME,DSN            MOVE DSN THAT IS PASSED TO THIS
*                                      PROGRAM TO DSNAME
         LA    R8,44                 LOAD REG 8 WITH 44
         LA    R7,DSNAME+43
COMPR1   C     R8,=F'1'              COMPARE REG 8 TO 1
         BNL   COMPR2                IF NOT LOW, GO TO COMPR2
         MVC   INDICATR,C12          MOVE 12 TO SUCCESS-INDICATOR
         LA    R10,12                MOVE 12 TO REGISTER 10
         MVC   PROCESSW,CNO          MOVE 'NO ' TO PROCESSW
         B     EXITLOAD              BRANCH TO EXITLOAD
COMPR2   CLI   0(R7),C' '            IS DSNAME AS INDEXED BY REG 7
*                                      EQUAL TO A SPACE?
         BNE   LOADLEN               IF NOT, GO TO LOADLEN
         S     R7,=F'1'              SUBTRACT 1 FROM REGISTER 7
         S     R8,=F'1'              SUBTRACT 1 FROM REGISTER 8
         B     COMPR1                BRANCH TO COMPR1
LOADLEN  ST    R8,R8LENGTH           STORE REG 8 IN R8LENGTH
         MVC   DSNLEN(2),R8LENGTH+2  MOVE LOW ORDER 2 BYTES
*                                      OF R8LENGTH TO DSNLEN
         MVC   PROCESSW,CYES         MOVE 'YES' TO PROCESSW
EXITLOAD L     R5,SAVEIT1            RESTORE REGISTER 5
         BR    R5                    RETURN
         EJECT
         SPACE 3
*
********************************************************************
*                                                                  *
*               D Y N A M I C   A L L O C A T I O N                *
*               -----------------------------------                *
*                                                                  *
*        THIS ROUTINE FIRST ATTEMPTS TO FREE DDNAME FINDMEDD.      *
*        IF DDNAME FINDMEDD WAS SUCCESSFULLY FREED, THEN THIS      *
*        ROUTINE BRANCHES TO THE DYNAMIC ALLOCATION MACRO          *
*        DYNALLOC.  IF DDNAME FINDMEDD WAS NOT SUCCESSFULLY        *
*        FREED, THEN IT CHECKS FOR ERROR REASON CODE 0438          *
*        (DDNAME NOT FOUND).  IF THE ERROR REASON CODE IS 0438,    *
*        THEN THIS ROUTINE BRANCHES TO THE DYNAMIC ALLOCATION      *
*        MACRO DYNALLOC, ELSE IT PRODUCES AN ERROR CONDITION       *
*        (SUCCESS-INDICATOR = '24')                                *
*                                                                  *
********************************************************************
*
         SPACE 3
DYNAMALC ST    R5,SAVEIT2
         MVC   PROCESSW,CYES
         LA    R1,FREEPARM
         DYNALLOC                      FREE F(FINDMEDD)
         LTR   R15,R15                 TEST REGISTER 15 FOR ZERO
         BZ    DODYNAM                 IF ZERO, BRANCH TO DODYNAM
         CLC   ERCODE,HEX0438          COMPARE ERROR REASON CODE
*                                         AGAINST HEXADECIMAL 0438
         BE    DODYNAM                 IF EQUAL, BRANCH TO DODYNAM
         MVC   PROCESSW,CNO            MOVE 'NO ' TO PROCESSW
         MVC   INDICATR,C24            MOVE '24' TO SUCCESS-INDICATOR
         LA    R10,24                  MOVE 24 TO REGISTER 10
         BR    R5                      RETURN
DODYNAM  CLC   INVOL,=CL6'ABSENT'      WAS VOL SER PROVIDED ?
         BE    DODYNAM1                IF NOT, BRANCH TO DODYNAM1
         MVI   DD1HIGH3,X'00'
         MVI   DD1HIGH4,X'00'
         MVI   DD1HIGH5,X'80'
         MVC   DD1VOLSR,INVOL
         B     DODYNAM2
DODYNAM1 MVI   DD1HIGH3,X'80'
         MVI   DD1HIGH4,X'00'
         MVI   DD1HIGH5,X'00'
DODYNAM2 LA    R1,DD1PARM
         DYNALLOC                      ALLOC F(FINDMEDD) DA(DSN) SHR
         L     R5,SAVEIT2
         LTR   R15,R15                 TEST REGISTER 15 FOR ZERO
         BZR   R5                      IF ZERO, RETURN
         MVC   PROCESSW,CNO            MOVE 'NO ' TO PROCESSW
         MVC   INDICATR,C08            MOVE '08' TO SUCCESS-INDICATOR
         LA    R10,8                   MOVE 8 TO REGISTER 10
         BR    R5                      RETURN
         EJECT
*
********************************************************************
*                                                                  *
*        THIS ROUTINE IS RESPONSIBLE FOR SEARCHING THE             *
*        DYNAMICALLY ALLOCATED PDS FOR THE GIVEN MEMBER            *
*                                                                  *
********************************************************************
*
SERCHPDS ST    R5,SAVEIT3           SAVE REG 5 IN SAVEIT3
         CLC   MEMBER,EIGHTSP       IS MEMBER EQUAL TO 8 SPACES?
         BNE   SEARCHIT             IF NOT, GO TO SEARCHIT
         MVC   INDICATR,C16         MOVE 16 TO SUCCESS-INDICATOR
         LA    R10,16               MOVE 16 TO REGISTER 10
         MVC   PROCESSW,CNO         MOVE 'NO ' TO PROCESSW
         B     EXITSRCH             BRANCH TO EXITSRCH
SEARCHIT FIND  PDSFILE,MEMBER,D     SEARCH DIRECTORY FOR MEMBER
         ST    R15,SAVER15          PUT REG 5 CONTENTS INTO SAVER15
         CLI   SAVER15+3,X'00'      CONDITION CODE 00?
         BE    RC00                 IF YES, GO TO RC00
         CLI   SAVER15+3,X'04'      CONDITION CODE 04?
         BE    RC04                 IF YES, GO TO RC04
         MVC   INDICATR,C20         MOVE 20 TO SUCCESS-INDICATOR
         LA    R10,20               MOVE 20 TO REGISTER 10
         MVC   PROCESSW,CNO         MOVE 'NO ' TO PROCESSW
         B     EXITSRCH             BRANCH TO EXITSRCH
RC04     MVC   INDICATR,C04         MOVE 04 TO SUCCESS-INDICATOR
         LA    R10,4                MOVE 4 TO REGISTER 10
         MVC   PROCESSW,CNO         MOVE 'NO ' TO PROCESSW
         B     EXITSRCH             BRANCH TO EXITSRCH
RC00     MVC   PROCESSW,CYES        MOVE 'YES' TO PROCESSW
EXITSRCH L     R5,SAVEIT3           RESTORE REGISTER 5
         BR    R5                   RETURN
         EJECT
*
*
***********************************************************
*                                                         *
*        THIS ROUTINE IS RESPONSIBLE FOR EDITING INVOL    *
*                                                         *
*        IF INVOL MEETS THE CRITERIA FOR BEING A VALID    *
*        VOLUMER SERIAL NUMBER (6 BYTES LONG, ALL         *
*        NUMERICS AND ALPHABETICS), THEN THIS ROUTINE     *
*        WILL MOVE 'YES' TO GOODVOL ELSE MOVE 'NO '       *
*        TO GOODVOL                                       *
*                                                         *
***********************************************************
*
*
EDITVOL  ST    R5,SAVEIT4
         MVC   GOODVOL,=CL3'YES'
         MVC   EDITBYTE(1),INVOL
         BAL   R5,EDIT1BYT
         CLC   GOODVOL,=CL3'YES'
         BNE   EXITEVOL
         MVC   EDITBYTE(1),INVOL+1
         BAL   R5,EDIT1BYT
         CLC   GOODVOL,=CL3'YES'
         BNE   EXITEVOL
         MVC   EDITBYTE(1),INVOL+2
         BAL   R5,EDIT1BYT
         CLC   GOODVOL,=CL3'YES'
         BNE   EXITEVOL
         MVC   EDITBYTE(1),INVOL+3
         BAL   R5,EDIT1BYT
         CLC   GOODVOL,=CL3'YES'
         BNE   EXITEVOL
         MVC   EDITBYTE(1),INVOL+4
         BAL   R5,EDIT1BYT
         CLC   GOODVOL,=CL3'YES'
         BNE   EXITEVOL
         MVC   EDITBYTE(1),INVOL+5
         BAL   R5,EDIT1BYT
         B     EXITEVOL
*
*
*
EDIT1BYT ST    R5,SAVEIT5            SAVE REG 14 ADDRESS IN SAVEIT5
         CLI   EDITBYTE,C'0'
         BL    NEXTEST
         CLI   EDITBYTE,C'9'
         BH    BADINVOL
         B     EXITEDT1
NEXTEST  CLI   EDITBYTE,C'A'
         BL    BADINVOL
         CLI   EDITBYTE,C'Z'
         BH    BADINVOL
         B     EXITEDT1
BADINVOL MVC   GOODVOL,=CL3'NO '
         MVC   INDICATR,C28         MOVE 28 TO SUCCESS-INDICATOR
         LA    R10,28               MOVE 28 TO REGISTER 10
EXITEDT1 L     R5,SAVEIT5
         BR    R5
*
*
*
EXITEVOL L     R5,SAVEIT4
         BR    R5
         EJECT
*
********************************************************************
*                                                                  *
*        E N D  O F   J O B   P R O C E S S I N G                  *
*                                                                  *
********************************************************************
*
SHUTDOWN L     R9,0(R11)
         MVC   0(56,R9),IOAREA
         CLC   FILOPEN,CYES        WAS FILE OPENED?
         BNE   LOADR15             IF NOT, GO TO LOADR15
         CLOSE (PDSFILE),MF=(E,RCPPL)  CLOSE FILE
LOADR15  LR    R15,R10
         L     R13,4(R13)
         RETURN (14,12),RC=(15)
         EJECT
********************************************************************
*                                                                  *
*        S T A T I C   W O R K   A R E A                           *
*                                                                  *
********************************************************************
CINIT    DS    0F
         DC    CL3'YES'
         DC    CL3'NO '
         DC    CL2'00'
         DC    CL2'04'
         DC    CL2'08'
         DC    CL2'12'
         DC    CL2'16'
         DC    CL2'20'
         DC    CL2'24'
         DC    CL2'28'
         DC    CL3'NO '
         DC    CL8' '
         DS    CL3
         DC    X'0438'
CINITL   EQU   *-CINIT
PDSFINIT DCB   DDNAME=FINDMEDD,DSORG=PO,MACRF=(R)
PDSFLEN  EQU   *-PDSFINIT
*
*        DATA STRUCTURE FOR FREEING OF DDNAME FINDMEDD
FREEINIT DS    0F
         DC    X'80'         TURN ON HIGH ORDER BIT
         DC    AL3(0)        ADDRESS OF REQUEST BLOCK
         DC    X'14'         S99RBLN (LENGTH OF REQUEST BLOCK)
         DC    X'02'         S99VERB (REQUEST FOR UNALLOCATION)
         DC    X'2000'       S99FLAG1 (DO NOT MOUNT VOLUMES)
         DC    X'0000'       S99ERROR (ERROR REASON CODE)
         DC    X'0000'       S99INFO (INFORMATION REASON CODE)
         DC    A(0)          S99TXTPP (ADDR OF POINTER TO TEXT UNIT)
         DC    X'00000000'   RESERVED
         DC    X'00000000'   S99FLAG2
         DC    A(0)          ADDRESS OF TEXT UNIT #1
         DC    X'80'         TURN ON HIGH ORDER BIT
         DC    AL3(0)        ADDRESS OF TEXT UNIT #2
         DC    X'0001',X'0001',X'0008',C'FINDMEDD'
         DC    X'0007'
         DC    X'0000'
FREELEN  EQU   *-FREEINIT
*
*        DATA STRUCTURE FOR DYNAMIC ALLOCATION
ALOCINIT DS    0F
         DC    X'80'         TURN ON HIGH ORDER BIT
         DC    AL3(0)        ADDRESS OF REQUEST BLOCK
         DC    X'14'         S99RBLN (LENGTH OF REQUEST BLOCK)
         DC    X'01'         S99VERB (REQUEST FOR DSNAME ALLOC)
         DC    X'2000'       S99FLAG1 (DO NOT MOUNT VOLUMES)
         DC    X'0000'       S99ERROR (ERROR REASON CODE)
         DC    X'0000'       S99INFO (INFORMATION REASON CODE)
         DC    A(0)          S99TXTPP (ADDR OF POINTERS TO TEXT UNITS
         DC    X'00000000'   RESERVED
         DC    X'00000000'   S99FLAG2
         DC    A(0)          ADDRESS OF TEXT UNIT #1
         DC    A(0)          ADDRESS OF TEXT UNIT #2
         DS    XL1
         DC    AL3(0)        ADDRESS OF TEXT UNIT #3
         DS    XL1
         DC    AL3(0)        ADDRESS OF TEXT UNIT #4
         DS    XL1
         DC    AL3(0)        ADDRESS OF TEXT UNIT #5
         DC    X'0001',X'0001',X'0008',C'FINDMEDD'
         DC    X'0002'
         DC    X'0001'
         DS    BL2
         DS    CL44
         DC    X'0004',X'0001',X'0001',X'08'
         DC    X'0015',X'0001',X'0008',C'SYSALLDA'
         DC    X'0010'
         DC    X'0001'
         DC    X'0006'
ALOCLEN  EQU   *-ALOCINIT
*
*        LITERAL POOL
         LTORG
         EJECT
********************************************************************
*                                                                  *
*        D Y N A M I C   W O R K   A R E A                         *
*                                                                  *
********************************************************************
WORKAREA DSECT
SAVEAREA DS    18F
SAVEIT1  DS    F
SAVEIT2  DS    F
SAVEIT3  DS    F
SAVEIT4  DS    F
SAVEIT5  DS    F
SAVER15  DS    F
R8LENGTH DS    F
INVOL    DS    CL6
GOODVOL  DS    CL3
EDITBYTE DS    CL1
*
CBLOK    DS    0F
CYES     DC    CL3'YES'
CNO      DC    CL3'NO '
C00      DC    CL2'00'
C04      DC    CL2'04'
C08      DC    CL2'08'
C12      DC    CL2'12'
C16      DC    CL2'16'
C20      DC    CL2'20'
C24      DC    CL2'24'
C28      DC    CL2'28'
FILOPEN  DC    CL3'NO '
EIGHTSP  DC    CL8' '
PROCESSW DS    CL3
HEX0438  DC    X'0438'
*
IOAREA   DS    0CL56
         DS    CL2
MEMBER   DS    CL8
DSN      DS    CL44
INDICATR DS    CL2
PDSFILE  DCB   DDNAME=FINDMEDD,DSORG=PO,MACRF=(R)
ROPPL    DC    F'0'          OPEN PARM LIST
RCPPL    DC    F'0'          CLOSE PARM LIST
*
FREEBLOK DS    0F
FREEPARM DC    X'80'         TURN ON HIGH ORDER BIT
         DC    AL3(FREERB)   ADDRESS OF REQUEST BLOCK
FREERB   DC    X'14'         S99RBLN (LENGTH OF REQUEST BLOCK)
         DC    X'02'         S99VERB (REQUEST FOR UNALLOCATION)
         DC    X'2000'       S99FLAG1 (DO NOT MOUNT VOLUMES)
ERCODE   DC    X'0000'       S99ERROR (ERROR REASON CODE)
         DC    X'0000'       S99INFO (INFORMATION REASON CODE)
         DC    A(FREETUPL)   S99TXTPP (ADDR OF POINTER TO TEXT UNIT)
         DC    X'00000000'   RESERVED
         DC    X'00000000'   S99FLAG2
FREETUPL DC    A(FREEUNT1)   ADDRESS OF TEXT UNIT #1
         DC    X'80'         TURN ON HIGH ORDER BIT
         DC    AL3(FREEUNT2) ADDRESS OF TEXT UNIT #2
FREEUNT1 DC    X'0001',X'0001',X'0008',C'FINDMEDD'
FREEUNT2 DC    X'0007'
         DC    X'0000'
*
*        DATA STRUCTURE FOR DYNAMIC ALLOCATION
ALOCBLOK DS    0F
DD1PARM  DC    X'80'         TURN ON HIGH ORDER BIT
         DC    AL3(DD1RB)    ADDRESS OF REQUEST BLOCK
DD1RB    DC    X'14'         S99RBLN (LENGTH OF REQUEST BLOCK)
         DC    X'01'         S99VERB (REQUEST FOR DSNAME ALLOC)
         DC    X'2000'       S99FLAG1 (DO NOT MOUNT VOLUMES)
         DC    X'0000'       S99ERROR (ERROR REASON CODE)
         DC    X'0000'       S99INFO (INFORMATION REASON CODE)
         DC    A(DD1TUPL)    S99TXTPP (ADDR OF POINTERS TO TEXT UNITS
         DC    X'00000000'   RESERVED
         DC    X'00000000'   S99FLAG2
DD1TUPL  DC    A(DD1UNIT1)   ADDRESS OF TEXT UNIT #1
         DC    A(DD1UNIT2)   ADDRESS OF TEXT UNIT #2
DD1HIGH3 DS    XL1
         DC    AL3(DD1UNIT3) ADDRESS OF TEXT UNIT #3
DD1HIGH4 DS    XL1
         DC    AL3(DD1UNIT4) ADDRESS OF TEXT UNIT #4
DD1HIGH5 DS    XL1
         DC    AL3(DD1UNIT5) ADDRESS OF TEXT UNIT #5
DD1UNIT1 DC    X'0001',X'0001',X'0008',C'FINDMEDD'
DD1UNIT2 DC    X'0002'
         DC    X'0001'
DSNLEN   DS    BL2
DSNAME   DS    CL44
DD1UNIT3 DC    X'0004',X'0001',X'0001',X'08'
DD1UNIT4 DC    X'0015',X'0001',X'0008',C'SYSALLDA'
DD1UNIT5 DC    X'0010'
         DC    X'0001'
         DC    X'0006'
DD1VOLSR DS    CL6
WORKLEN  EQU   *-WORKAREA
         END
//BREAKDSN EXEC COBUCL
//COB.SYSPRINT DD SYSOUT=*
//COB.SYSIN DD *
000010*******************************
000100 ID DIVISION.
000110*******************************
000200 PROGRAM-ID.     BREAKDSN.
000300 AUTHOR.         GARY DUFFIELD.
000310 REMARKS.
000320                 THIS PROGRAM EDITS ANY DATASET NAME PASSED TO IT
000330                 AND SPLITS OUT THE PDS MEMBER NAME IF IT EXISTS.
000340
000350*******************************
000400 ENVIRONMENT DIVISION.
000410*******************************
000500 CONFIGURATION SECTION.
000600 SOURCE-COMPUTER. IBM-370.
000700 OBJECT-COMPUTER. IBM-370.
000800
000810*******************************
000900 DATA DIVISION.
000910*******************************
001100 WORKING-STORAGE SECTION.
001300 01  TABLES.
001400     05  WORK-DSN-INPUT                  PIC X(44).
001500     05  DSN-CHARACTER REDEFINES WORK-DSN-INPUT
001600         OCCURS 44 TIMES INDEXED BY INX  PIC X.
001700
001800     05  WORK-DSN-OUTPUT                 PIC X(44).
001900     05  DSN-CHAR-OUT REDEFINES WORK-DSN-OUTPUT
002000         OCCURS 44 TIMES INDEXED BY INY  PIC X.
002100
002200     05  WORK-MEMBER                     PIC X(8).
002300     05  DSN-CHAR-MEMBER REDEFINES WORK-MEMBER
002400         OCCURS 8 TIMES INDEXED BY INZ   PIC X.
002500
002600 01  SWITCHES.
002700     05  BAD-CHAR                        PIC X(3).
002800         88  BAD-CHARACTER                           VALUE 'YES'.
002850     05  QUOTE-CHAR                      PIC 9(5) COMP-3.
002860         88  TOO-MANY-QUOTES                    VALUE 3 THRU 44.
002870         88  NOT-ENOUGH-QUOTES                  VALUE 1.
002900
003000 01  ACCUMULATORS.
003100     05  PERFORM-COUNT                   PIC 9(5) COMP-3.
003110     05  CHAR-COUNT                      PIC 9(5) COMP-3.
003200
003300     EJECT
003310*******************************
003400 LINKAGE SECTION.
003410*******************************
003500
003600 01  BREAKDSN-IO-AREA.
003700     05  INPUT-DSN                       PIC X(44).
003800     05  OUTPUT-DSN                      PIC X(44).
003900     05  OUTPUT-MEMBER-NAME              PIC X(8).
004000     05  MEMBER-NAME-PROVIDED            PIC X(3).
004010     05  GDG-TYPE-DATASET                PIC X(3).
004100     05  VALID-DSN-PASSED                PIC X(3).
004200     05  ERROR-MESSAGE                   PIC X(30).
004300     EJECT
004400********************************************
004500 PROCEDURE DIVISION USING BREAKDSN-IO-AREA.
004600********************************************
004700 0100-MAIN-MODULE.
004800********************************************
004900
004910     PERFORM 0110-INITIALIZE-MODULE.
004911     IF DSN-CHARACTER (INX) = SPACE OR '('
004912        MOVE 'YES' TO BAD-CHAR.
004920     IF DSN-CHARACTER (INX) = QUOTE
004930        COMPUTE QUOTE-CHAR = QUOTE-CHAR + 1
004940        MOVE DSN-CHARACTER (INX) TO DSN-CHAR-OUT (INY)
004950        SET INX UP BY 1
004960        SET INY UP BY 1.
005400     PERFORM 0200-EXAMINE-DSN-MODULE
005500        VARYING INX FROM INX BY 1
005600        UNTIL (DSN-CHARACTER (INX) = ' ' OR '(' OR QUOTE)
005610           OR (INX > 44)
005700           OR (BAD-CHARACTER).
005710     IF DSN-CHARACTER (INX) = QUOTE
005720        COMPUTE QUOTE-CHAR = QUOTE-CHAR + 1.
005730     SET INX DOWN BY 1.
005740     IF DSN-CHARACTER (INX) = '.'
005750        MOVE 'YES' TO BAD-CHAR.
005760     SET INX UP BY 1.
005800     IF BAD-CHARACTER
005900        MOVE 'NO ' TO VALID-DSN-PASSED
006000        MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
006100     ELSE
006200        IF INX > 44
006210           IF NOT-ENOUGH-QUOTES
006220              MOVE 'NO ' TO VALID-DSN-PASSED
006230              MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
006240           ELSE
006300              MOVE WORK-DSN-OUTPUT TO OUTPUT-DSN
006400              MOVE 'NO ' TO MEMBER-NAME-PROVIDED
006410              MOVE 'NO ' TO GDG-TYPE-DATASET
006500              MOVE 'YES' TO VALID-DSN-PASSED
006700        ELSE
006800           IF DSN-CHARACTER (INX) = ' ' OR QUOTE
006900              PERFORM 0300-BASIC-DSN-MODULE
007000           ELSE
007100              PERFORM 0310-OTHER-DSN-MODULE.
007110     GOBACK.
007111
007112********************************
007113 0110-INITIALIZE-MODULE.
007114********************************
007115
007120     SET INX TO 1.
007130     SET INY TO 1.
007140     SET INZ TO 1.
007150     MOVE INPUT-DSN TO WORK-DSN-INPUT.
007160     MOVE SPACE TO WORK-DSN-OUTPUT WORK-MEMBER OUTPUT-DSN
007161                   OUTPUT-MEMBER-NAME MEMBER-NAME-PROVIDED
007162                   GDG-TYPE-DATASET VALID-DSN-PASSED
007163                   ERROR-MESSAGE.
007170     MOVE 'NO ' TO BAD-CHAR.
007180     MOVE ZERO TO QUOTE-CHAR PERFORM-COUNT CHAR-COUNT.
007200     EJECT
007300
007400********************************
007500 0200-EXAMINE-DSN-MODULE.
007600********************************
007700
007800     COMPUTE PERFORM-COUNT = PERFORM-COUNT + 1.
007900     IF (DSN-CHARACTER (INX) NOT NUMERIC)
008000        AND (DSN-CHARACTER (INX) NOT ALPHABETIC)
008100        AND (DSN-CHARACTER (INX) NOT = '@' AND '#' AND '$'
008110           AND QUOTE AND '.' AND SPACE)
008200        MOVE 'YES' TO BAD-CHAR.
008210     IF DSN-CHARACTER (INX) = '.'
008211        IF CHAR-COUNT = 0
008212           MOVE 'YES' TO BAD-CHAR
008213        ELSE
008220           COMPUTE CHAR-COUNT = 0
008230     ELSE
008231        IF DSN-CHARACTER (INX) NOT = SPACE
008240           COMPUTE CHAR-COUNT = CHAR-COUNT + 1.
008250     IF CHAR-COUNT > 8
008260        MOVE 'YES' TO BAD-CHAR.
008261     IF CHAR-COUNT = 1
008262        IF DSN-CHARACTER (INX) NUMERIC
008263           MOVE 'YES' TO BAD-CHAR.
008264     IF DSN-CHARACTER (INX) = QUOTE
008265        COMPUTE QUOTE-CHAR = QUOTE-CHAR + 1.
008266     IF TOO-MANY-QUOTES
008267        MOVE 'YES' TO BAD-CHAR.
008300     MOVE DSN-CHARACTER (INX) TO DSN-CHAR-OUT (INY).
008400     SET INY UP BY 1.
008500
008600********************************
008700 0205-MOVE-MEMBER-MODULE.
008800********************************
008900
009000     COMPUTE PERFORM-COUNT = PERFORM-COUNT + 1.
009100     IF (DSN-CHARACTER (INX) NOT NUMERIC)
009200        AND (DSN-CHARACTER (INX) NOT ALPHABETIC)
009300        AND (DSN-CHARACTER (INX) NOT = '@' AND '#' AND '$')
009400        MOVE 'YES' TO BAD-CHAR.
009500     MOVE DSN-CHARACTER (INX) TO DSN-CHAR-MEMBER (INZ).
009600     SET INZ UP BY 1.
009700     EJECT
009800********************************
009900 0300-BASIC-DSN-MODULE.
010000********************************
010100
010110     MOVE DSN-CHARACTER (INX) TO DSN-CHAR-OUT (INY)
010120     SET INX UP BY 1
010200     SET INY UP BY 1
010300     PERFORM 0200-EXAMINE-DSN-MODULE
010400        VARYING INX FROM INX BY 1
010500        UNTIL (DSN-CHARACTER (INX) NOT = ' ' AND QUOTE)
010600           OR (INX > 44).
010610     IF BAD-CHARACTER
010620        MOVE 'NO ' TO VALID-DSN-PASSED
010630        MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
010640     ELSE
010700        IF INX > 44
010710           IF NOT-ENOUGH-QUOTES
010720              MOVE 'NO ' TO VALID-DSN-PASSED
010730              MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
010740           ELSE
010800              MOVE WORK-DSN-OUTPUT TO OUTPUT-DSN
010900              MOVE 'NO ' TO MEMBER-NAME-PROVIDED
010910              MOVE 'NO ' TO GDG-TYPE-DATASET
011000              MOVE 'YES' TO VALID-DSN-PASSED
011100        ELSE
011200           MOVE 'NO ' TO VALID-DSN-PASSED
011300           MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE.
011500
011600********************************
011700 0310-OTHER-DSN-MODULE.
011800********************************
011900
012000     SET INX UP BY 1
012100     IF INX > 44
012200        MOVE 'NO ' TO VALID-DSN-PASSED
012300        MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
012400     ELSE
012500       IF (DSN-CHARACTER (INX) NUMERIC)
012600          OR (DSN-CHARACTER (INX) = '-' OR '+')
012700          PERFORM 0400-GDG-DSN-MODULE
012800       ELSE
012900          IF DSN-CHARACTER (INX) ALPHABETIC
012910             OR DSN-CHARACTER (INX) = '@' OR '#' OR '$'
013000             PERFORM 0410-PDS-DSN-MODULE
013100          ELSE
013200             MOVE 'NO ' TO VALID-DSN-PASSED
013300             MOVE 'INVALID MEMBER NAME' TO ERROR-MESSAGE.
013400     EJECT
013500********************************
013600 0400-GDG-DSN-MODULE.
013700********************************
013710
013800     SET INX DOWN BY 1.
013810     MOVE DSN-CHARACTER (INX) TO DSN-CHAR-OUT (INY).
013820     SET INX UP BY 1.
013821     SET INY UP BY 1.
013830     IF DSN-CHARACTER (INX) = '+' OR '-'
013840        MOVE DSN-CHARACTER (INX) TO DSN-CHAR-OUT (INY)
013900        SET INX UP BY 1
013910        SET INY UP BY 1.
014000     COMPUTE PERFORM-COUNT = 0
014010     COMPUTE CHAR-COUNT = 1
014100     PERFORM 0200-EXAMINE-DSN-MODULE
014200        VARYING INX FROM INX BY 1
014300        UNTIL (DSN-CHARACTER (INX) NOT NUMERIC)
014400           OR (INX > 44)
014500           OR (PERFORM-COUNT > 3).
014600     IF (INX > 44) OR (PERFORM-COUNT > 3)
014700        MOVE 'NO ' TO VALID-DSN-PASSED
014800        MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
014900     ELSE
015000        IF  DSN-CHARACTER (INX) = ')'
015010           IF PERFORM-COUNT = 0
015011              MOVE 'NO ' TO VALID-DSN-PASSED
015012              MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
015020           ELSE
015100              PERFORM 0500-GDG-CONT-MODULE
015200        ELSE
015300           MOVE 'NO ' TO VALID-DSN-PASSED
015400           MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE.
015600
015700********************************
015800 0410-PDS-DSN-MODULE.
015900********************************
016000
016200     COMPUTE PERFORM-COUNT = 0.
016210     COMPUTE CHAR-COUNT = 0.
016300     PERFORM 0205-MOVE-MEMBER-MODULE
016400        VARYING INX FROM INX BY 1
016500        UNTIL (DSN-CHARACTER (INX) = ')' OR ' ')
016600           OR (INX > 44)
016700           OR (PERFORM-COUNT > 8)
016800           OR (BAD-CHARACTER).
016900     IF BAD-CHARACTER
017000        MOVE 'NO ' TO VALID-DSN-PASSED
017100        MOVE 'INVALID MEMBER NAME' TO ERROR-MESSAGE
017200     ELSE
017300        IF INX > 44
017400           MOVE 'NO ' TO VALID-DSN-PASSED
017500           MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
017600        ELSE
017700           IF PERFORM-COUNT > 8
017800              MOVE 'NO ' TO VALID-DSN-PASSED
017900              MOVE 'MEMBER NAME TOO LONG' TO ERROR-MESSAGE
018000           ELSE
018100              IF  DSN-CHARACTER (INX) = ')'
018200                 PERFORM 0510-PDS-CONT-MODULE
018300              ELSE
018400                 MOVE 'NO ' TO VALID-DSN-PASSED
018500                 MOVE 'INVALID MEMBER NAME' TO ERROR-MESSAGE.
018600     EJECT
018700********************************
018800 0500-GDG-CONT-MODULE.
018900********************************
019000
019010     MOVE DSN-CHARACTER (INX) TO DSN-CHAR-OUT (INY)
019100     SET INX UP BY 1
019110     SET INY UP BY 1
019200     PERFORM 0200-EXAMINE-DSN-MODULE
019300        VARYING INX FROM INX BY 1
019400        UNTIL (DSN-CHARACTER (INX) NOT = ' ' AND QUOTE)
019500           OR (INX > 44).
019510     IF BAD-CHARACTER
019520        MOVE 'NO ' TO VALID-DSN-PASSED
019530        MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
019540     ELSE
019600        IF INX > 44
019610           IF NOT-ENOUGH-QUOTES
019611              MOVE 'NO ' TO VALID-DSN-PASSED
019612              MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
019620           ELSE
019700              MOVE WORK-DSN-OUTPUT TO OUTPUT-DSN
019800              MOVE 'NO ' TO MEMBER-NAME-PROVIDED
019900              MOVE 'YES' TO VALID-DSN-PASSED
019910              MOVE 'YES' TO GDG-TYPE-DATASET
020000        ELSE
020100           MOVE 'NO ' TO VALID-DSN-PASSED
020200           MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE.
020400
020500********************************
020600 0510-PDS-CONT-MODULE.
020700********************************
020800
020900     SET INX UP BY 1
021000     PERFORM 0200-EXAMINE-DSN-MODULE
021100        VARYING INX FROM INX BY 1
021200        UNTIL (DSN-CHARACTER (INX) NOT = ' ' AND QUOTE)
021300           OR (INX > 44).
021310     IF BAD-CHARACTER
021320        MOVE 'NO ' TO VALID-DSN-PASSED
021330        MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
021340     ELSE
021400        IF INX > 44
021410           IF NOT-ENOUGH-QUOTES
021420              MOVE 'NO ' TO VALID-DSN-PASSED
021430              MOVE 'INVALID MEMBER NAME' TO ERROR-MESSAGE
021440           ELSE
021500              MOVE WORK-DSN-OUTPUT TO OUTPUT-DSN
021600              MOVE WORK-MEMBER TO OUTPUT-MEMBER-NAME
021700              MOVE 'YES' TO MEMBER-NAME-PROVIDED
021710              MOVE 'NO ' TO GDG-TYPE-DATASET
021800              MOVE 'YES' TO VALID-DSN-PASSED
021900        ELSE
022000           MOVE 'NO ' TO VALID-DSN-PASSED
022100           MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE.
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=MOD    <- TARGET LOAD LIBRARY
//LKED.SYSIN DD *
  NAME FILEATTR(R)
//
