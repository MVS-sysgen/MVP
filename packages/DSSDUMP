//DSSDUMP JOB (JOB),
//             'INSTALL DSSDUMP', 
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//* THIS JCL WAS GENERATED AUTOMATICALLY BY make_release.sh
//*
//SUBCPOOL EXEC ASMFC,PARM='DECK,LIST,OBJECT'
//SYSLIB DD DSN=SYS1.MACLIB,DISP=SHR
//       DD DSN=SYS2.PVTMACS,DISP=SHR
//       DD DSN=SYS2.ESPMAC,DISP=SHR
//       DD DSN=SYS1.AMACLIB,DISP=SHR
//       DD DSN=SYS1.AMODGEN,DISP=SHR
//SYSIN DD DATA,DLM=@@
SUBCPOOL TITLE 'S U B C P O O L  ***  MVS 3.8J CPOOL SUPPORT'
         MACRO ,
&NM      SHEAD &ID=,&CELL=
         PUSH  USING
         DROP  ,
&NM      STM   R14,12,12(R13)  SAVE A BIT
         LR    R12,R15
         USING SUBCPOOL,R12  DECLARE COMMON BASE
         AIF   ('&ID' EQ '').NOID
         LR    R6,R0         PRESERVE POOL ID
.NOID    AIF   ('&CELL' EQ '').NOCELL
         LA    R7,0(,R1)     PRESERVE AND CLEAN CELL ADDRESS
.NOCELL  LA    R0,SAVEEND-SAVE
         GETMAIN R,LV=(0)
         XC    0(SAVEEND-SAVE,R1),0(R1)
         ST    R13,4(,R1)
         ST    R1,8(,R13)
         LR    R13,R1
         USING SAVE,R13
         MVI   SAVE+3,SAVEEND-SAVE  SET LENGTH FOR PGMEXIT
         ST    R6,RSNCODE    RETURN CELL POOL ID
.MEND    MEND  ,
         SPACE 1
         COPY  OPTIONGB
         SPACE 1
         SYSPARM LIST=YES
         SPACE 2
***********************************************************************
**                                                                   **
**  SUBCPOOL IS INVOKED WITH THE LOCAL VERSION OF THE CPOOL MACRO    **
**    LIST FUNCTION AND WORKAREA NOT SUPPORTED                       **
**    SP>127, KEY, TCB, AND LOC ARE NOT SUPPORTED                    **
**  PARAMETER LIST SAME AS IBM'S (?)                                 **
**                                                                   **
**   COPYRIGHT 2005 EXPERT SYSTEM PROGRAMMING                        **
***********************************************************************
**                                                                   **
**  FOR GENERAL USE, THE CONTROL BLOCKS FOR CELL POOL DATA ARE       **
**  BASED IN TCB FSA REGISTER 12, AND R11 HAS 'POOL' TO CONFIRM.     **
**                                                                   **
***********************************************************************
*TEST*   PRINT &PRTSOR
SUBCPOOL START 0                                      ADDED ON 2005012
         USING SUBCPOOL,R15  SET BASE
         B     GETCELL       BRANCH AROUND FIXED AREA
         DC    AL1(19),CL19'SUBCPOOL - &SYSDATE'
         B     FREECELL   24 FIXED OFFSET FOR CPOOL MACRO
         B     BUILD      28 FIXED OFFSET FOR CPOOL MACRO
         B     DELETE     32 FIXED OFFSET FOR CPOOL MACRO
         USING SUBCPOOL,R12  (DONE IN SHEAD)
         USING SAVE,R13          DITTO
         SPACE 1
***********************************************************************
*   GET - ACQUIRE A FREE CELL FROM EXISTING POOL                      *
***********************************************************************
GETCELL  LTR   R0,R0         ANY ID ?                           GP12131
         BNZ   GETCALL         YES; NOT TERMINATION             GP12131
         LTR   R1,R1         ANYTHING ?                         GP12131
         BZ    DELETE          YES; TERMINATION                 GP12131
         DROP  R15
         SPACE 1
GETCALL  SHEAD ID=LOOK       COMMON ENTRY CODE
         L     R1,4(,R13)    CALLER'S R13
         TM    16(R1),X'80'   WAS REQUEST CONDITIONAL?
         BZ    *+8           NO
         OI    PROFLAGS,PFCOND  SET CONDITIONAL MODE
         BAL   R9,FINDPOOL
           B   BADPID        POOL ID NOT FOUND
         USING CPRSECT,R8    DECLARE CONTROL ENTRY
         LA    R5,CPRCHAIN   POINT TO STORAGE BLOCK CHAIN
         USING BLKLINK,R5    DECLARE PREFIX MAPPING
GETBLOOP ICM   R4,15,BLKLINK  IS THERE ANOTHER BLOCK?
         BZ    GETNONE       NO; SEE WHETHER CONDITIONAL
         BAL   R9,FINDFREE   FIND A FREE CELL
         LR    R5,R4
         B     GETBLOOP      TRY AGAIN
         SPACE 1
GETNONE  TM    PROFLAGS,PFCOND  CONDITIONAL REQUEST ?
         BNZ   PGMEXIT       YES; RETURN R1 = 0
         ICM   R0,15,CPRCOUNT   SECONDARY CELL COUNT
         BZ    BADSEC        NO EXTENSION WANTED ?
         BAL   R9,BUILDBLK   BUILD A CELL BLOCK
         B     GETBLOOP      TRY AGAIN
         POP   USING
         SPACE 1
BADSEC   WTO   'SUBCPOOL: PRIMARY CELLS EXHAUSTED'
         ABEND 667,DUMP
         SPACE 1
BADPID   WTO   'SUBCPOOL: INVALID POOL ID'
         ABEND 666,DUMP
         SPACE 2
***********************************************************************
*   FREE - RELEASE A CELL FROM EXISTING POOL                          *
***********************************************************************
FREECELL SHEAD ID=LOOK,CELL=YES    COMMON ENTRY CODE
         BAL   R9,FINDPOOL
           B   BADPID        POOL ID NOT FOUND
         USING CPRSECT,R8    DECLARE CONTROL ENTRY
         LA    R5,CPRCHAIN   POINT TO STORAGE BLOCK CHAIN
         USING BLKLINK,R5    DECLARE PREFIX MAPPING
FREECOOP ICM   R4,15,BLKLINK  IS THERE ANOTHER BLOCK?
         BZ    PGMEXIT       NO; IGNORE ERROR
         DROP  R5
         USING BLKLINK,R4
         C     R7,BLKBXLE    IN THIS BLOCK ?
         BL    FREECEUP      NO
         C     R7,BLKBXLE+8  IN RANGE ?
         BNH   FREECEIT      YES; FREE IT UP
FREECEUP LR    R5,R4
         B     FREECOOP      TRY AGAIN
FREECEIT S     R7,BLKBXLE    OFFSET FROM START OF BLOCK
         SR    R6,R6         CLEAR FOR DIVIDE
         D     R6,BLKBXLE+4  RELATIVE CELL NUMBER
         D     R6,=F'8'      GET BIT COUNT; BYTE OFFSET
         LA    R7,BLKMASK(R7)  POINT TO BYTE
         LA    R5,=X'7F,BF,DF,EF,F7,FB,FD,FE'
         AR    R5,R6         POINT TO APPROPRIATE MASK
         NC    0(1,R7),0(R5)  UNALLOCATE THE CELL
         B     PGMEXIT       AND RETURN
         POP   USING
         SPACE 2
***********************************************************************
*   BUILD - CONSTRUCT A CELL POOL MATCHING PARAMETER LIST             *
***********************************************************************
BUILD    SHEAD CELL=YES      LOAD PARM INTO R7
         USING PARMLIST,R7   DECLARE PARAMETER LIST
         MVC   WTO(PATWTOL),PATWTO  MAKE ERROR MESSAGE
         LA    R2,=CL8'PCELLCT'
         ICM   R0,15,PRMPCT  VALID COUNT ?
         BNP   BADPRM        NO
         LA    R2,=CL8'SCELLCT'
         ICM   R0,15,PRMSCT  VALID COUNT ?
         BM    BADPRM        NO
         LA    R2,=CL8'CSIZE'
         ICM   R0,15,PRMSIZ  VALID COUNT ?
         BNP   BADPRM        NO
         LA    R2,=CL8'SP'
         SR    R0,R0                                            GP12154
         IC    R0,PRMSP                                         GP12154
         CLI   PRMSP,128
         BNL   BADPRM
         TM    PRMFLGS,PFHKEY  ?
         BZ    BUILDNKY
         LA    R2,=CL8'KEY'
         IC    R0,PRMKEY                                        GP12154
         CLI   PRMKEY,8      NORMAL KEY ?
         BNE   BADPRM
BUILDNKY LTCB  R6,USE=YES    GET MY TCB (BASE OF CELL POOL IDS)
         TM    PRMFLGS,PFHTCB  ?
         BZ    BUILDNTC
         LA    R2,=CL8'TCB'
         CLM   R6,7,PRMTCB+1
         BNE   BADPRM
BUILDNTC L     R10,TCBFSA    GET SAVE AREA POINTER
         N     R10,=X'00FFFFFF'  CLEAN
         CLC   =C'POOL',20+11*4(R10)  INITIALIZED ?
         BE    BUILDHSV      YES; RUN CHAIN
         XC    20+12*4(4,R10),20+12*4(R10)  CLEAR ANCHOR
         MVC   20+11*4(4,R10),=C'POOL'  SET ID
BUILDHSV LA    R8,20+12*4(,R10)  POINT TO CHAIN HEAD
         DROP  R6
         USING CPRSECT,R8    DECLARE ROOTING TOOTING POOL CONTROL
BUILDHLP ICM   R9,15,CPRLINK  ANY MORE ?
         BZ    BUILDHGT      NO; APPEND TO THIS ENTRY
         DROP  R8
         USING CPRSECT,R9
         MAX   R6,CPRID      REQUESTED ID ?
         LR    R8,R9         TRY AGAIN
         B     BUILDHLP      RETURN FOUND
         DROP  R9
         SPACE 1
         USING CPRSECT,R8    DECLARE ROOTING TOOTING POOL CONTROL
BUILDHGT SR    R15,R15
         IC    R15,PRMSP     GET SUBPOOL
         GETMAIN RC,LV=CPRLEN,SP=(15)  GET STORAGE
         BXH   R15,R15,STORFAIL  OOPS
         XC    0(CPRLEN,R1),0(R1)
         ST    R1,CPRLINK    LINK IT IN
         LR    R8,R1         SET CORRECT ADDRESS FOR BUILDBLK
         AL    R6,=F'1'      MAKE POOL ID
         ST    R6,CPRID
         ST    R6,RSNCODE    RETURN TO CALLER IN R0
         MVC   CPRCOUNT,PRMSCT  SECONDARY CELL COUNT
         MVC   CPRSIZE,PRMSIZ   CELL SIZE
         MVC   CPRHEAD,PRMHDR   HEADER
         MVC   CPRSP(4),PRMSP   COPY SP, KEY, FLAGS
         L     R0,PRMPCT
         LA    R5,CPRCHAIN
         BAL   R9,BUILDBLK   BUILD FIRST STORAGE BLOCK
         B     PGMEXIT
         SPACE 1
BADPRM   MVC   WTOPARM,0(R2)  SHOW BAD PARAMETER
         LA    R14,WTOPEND-WTO    NEW LENGTH                    GP12154
         STH   R14,WTO       SET NEW LENGTH                     GP12154
         MVI   WTOPVAL-1,C' '     BLANK SPACER                  GP12154
         ST    R0,DB              SAVE BAD VALUE                GP12154
         UNPK  WTOPVAL(9),DB(5)   UNPACK                        GP12154
         TR    WTOPVAL,HEXTRTAB   MAKE PRINTABLE                GP12154
         WTO   MF=(E,WTO)
         ABEND 661,DUMP
PATWTO   WTO   'SUBCPOOL: BAD VALUE FOR ',MF=L                  GP12152
PATWTOL  EQU   *-PATWTO
         POP   USING
         SPACE 2
***********************************************************************
*   DELETE - RELEASE A CELL POOL                                      *
***********************************************************************
DELETE   SHEAD ID=LOOK       COMMON ENTRY CODE
         LTR   R7,R7         COMPLETE TERMINATION ?             GP12131
         BNZ   *+8             NO                               GP12131
         OI    PROFLAGS,PFTERM   YES; FORCE MATCH               GP12131
         ST    R7,RR1CODE    RETURN R1 UNCHANGED
DELTALL  BAL   R9,FINDPOOL                                      GP12131
           B   PGMEXIT       IGNORE IF POOL ID NOT FOUND        GP12131
         USING CPRSECT,R8    DECLARE CONTROL ENTRY
         L     R4,CPRCHAIN   POINT TO STORAGE BLOCK CHAIN
         SR    R7,R7
         IC    R7,CPRSP      GET SUBPOOL
DELBLOOP LTR   R5,R4         ANOTHER CELL BLOCK ?
         BZ    DELCFREE      NO; FREE CONTROL BLOCK
         USING BLKLINK,R5    DECLARE PREFIX MAPPING
         L     R4,BLKLINK    SAVE FOR NEXT TIME
         L     R3,BLKBXLE+8  GET LAST CELL IN BLOCK
         A     R3,BLKBXLE+4  PLUS SIZE OF LAST CELL
         TM    CPRFLAGS,PFHHDR  HEADER ?
         BZ    DELBFREE      NO
         SH    R5,=AL2(L'BLKHEAD)
DELBFREE SR    R3,R5         SIZE TO FREE
         FREEMAIN RC,A=(R5),LV=(R3),SP=(R7)  FREE CELL BLOCK
         B     DELBLOOP      DO NEXT
         SPACE 1
DELCFREE MVC   CPRLINK-CPRSECT(L'CPRLINK,R10),CPRLINK  UNCHAIN THIS ONE
         FREEMAIN R,LV=CPRLEN,SP=(R7),A=(R8)
         TM    PROFLAGS,PFTERM    COMPLETE TERMINATION ?        GP12131
         BNZ   DELTALL              YES; RINSE AND REPEAT       GP12131
         B     PGMEXIT
         POP   USING
         SPACE 2
***********************************************************************
*   PGMEXIT - COMMON RETURN                                           *
***********************************************************************
PGMEXIT  DS    0H
         PGMEXIT COPYRET=(RETCODE,12)  RETURN R15-R1
         SPACE 2
***********************************************************************
*   FINDPOOL - LOCATE POOL BASED ON ID IN R6                          *
***********************************************************************
         PUSH  USING
FINDPOOL LTCB  R4,USE=YES    GET MY TCB
         L     R10,TCBFSA    GET SAVE AREA POINTER
         N     R10,=X'00FFFFFF'  CLEAN
         CLC   =C'POOL',20+11*4(R10)  INITIALIZED ?
         BNER  R9            NO; BAD CALL
         LA    R10,20+12*4(,R10)  POINT TO CHAIN HEAD
         USING CPRSECT,R10   DECLARE ROOTING TOOTING POOL CONTROL
FINDLOOP ICM   R8,15,CPRLINK  ANY MORE ?
         DROP  R10
         USING CPRSECT,R8    DECLARE ROOTING TOOTING POOL CONTROL
         BZR   R9            NO; TOO BAD
         TM    PROFLAGS,PFTERM    COMPLETE TERMINATION ?        GP12131
         BNZ   4(,R9)          YES; TREAT AS MATCH              GP12131
         CL    R6,CPRID      REQUESTED ID ?
         BE    4(,R9)        RETURN FOUND
         LR    R10,R8        UPDATE
         B     FINDLOOP      NO; TRY AGAIN
         POP   USING
         SPACE 2
***********************************************************************
*   FINDFREE - LOCATE A FREE CELL IN THE BLOCK POINTED TO BY R4       *
***********************************************************************
         PUSH  USING
         USING BLKLINK,R4    PASSED BY USER
FINDFREE STM   R14,R12,12(R13)  SAVE A LITTLE
         LA    R14,BLKMASK     GET ALLOCATION BITS
         LM    R3,R5,BLKBXLE   GET POINTERS TO ALL CELLS
         DROP  R4
FINDFREL LA    R0,8          BITS PER BYTE
         LA    R1,X'80'      FIRST MASK BIT
FINDFRET EX    R1,EXTMBIT    IS MASK BIT ON ?
         BZ    FINDFALL      NO; ALLOCATE IT
         SRL   R1,1          NEXT BIT
         BXH   R3,R4,FINDFEXT  NONE FOUND
         BCT   R0,FINDFRET
         LA    R14,1(,R14)   NEXT MASK BYTE
         B     FINDFREL      DO ALL
FINDFEXT LM    R14,R12,12(R13)  RELOAD ALL
         BR    R9            RETURN TO CALLER
FINDFALL STC   R1,DB         STASH BIT TO STORAGE
         OC    0(1,R14),DB   ALLOCATE THE CELL
         ST    R3,RR1CODE    SET THE CELL ADDRESS IN RETURN
         B     PGMEXIT       RETURN THE CELL
EXTMBIT  TM    0(R14),*-*    MASK BIT ON ?
         POP   USING
         SPACE 2
***********************************************************************
*   BUILDBLK - BUILD A CELL BLOCK; CELL COUNT IN R0; LINK IN R5       *
***********************************************************************
         PUSH  USING
         USING BLKLINK,R5    PASSED BY CALLER
         USING CPRSECT,R8    PASSED BY CALLER
BUILDBLK STM   R14,R12,12(R13)  SAVE A LITTLE
         LA    R9,7          FOR ROUNDING
         AR    R9,R0         NUMBER OF BITS REQUIRED
         SRA   R9,3          NUMBER OF MASK BYTES
         LA    R2,BLKMASK-BLKLINK(,R9)  GET CELL OVERHEAD
         TM    CPRFLAGS,PFHHDR      HEADER REQUESTED ?
         BZ    *+8           NO
         LA    R2,L'BLKHEAD(,R2)     ADD HEADER
         L     R15,CPRSIZE   GET CELL SIZE
         MR    R14,R0        TIMES CELL COUNT
         LR    R6,R15        SAVE SIZE
         AR    R2,R15        TOTAL SIZE
         SR    R15,R15
         IC    R15,CPRSP     GET SUBPOOL
         GETMAIN RC,LV=(R2),SP=(15)  GET STORAGE
         BXH   R15,R15,STORFAIL
         LR    R14,R1        BLOCK ADDRESS
         LR    R15,R2        GETMAIN SIZE
         SR    R3,R3         CLEAR LENGTH AND INSERT BYTE
         MVCL  R14,R2        CLEAR BLOCK
         TM    CPRFLAGS,PFHHDR  HEADER ?
         BZ    BUILDBNH      NO
         DROP  R5
         USING BLKSECT,R1
         MVC   BLKHEAD,CPRHEAD  PROPAGATE HEADER
         LA    R1,L'BLKHEAD(,R1)
         DROP  R1
         USING BLKLINK,R5
BUILDBNH ST    R1,BLKLINK    APPEND TO CHAIN
         DROP  R5
         USING BLKLINK,R1
         LA    R2,BLKMASK(R9)   GET FIRST AVAILABLE CELL
         L     R3,CPRSIZE    GET CELL SIZE
         LA    R4,0(R6,R2)   LAST CELL +
         SR    R4,R3         LAST CELL ADDRESS
         STM   R2,R4,BLKBXLE  SAVE POINTERS
         LM    R14,R12,12(R13)
         BR    R9            RETURN TO CALLER
         POP   USING
STORFAIL ABEND 804,DUMP      *****NOT ENOUGH STORAGE*****
         SPACE 2
HEXTAB   DC    C'0123456789ABDEF' HEX PRINTABLES                GP12154
HEXTRTAB EQU   HEXTAB-C'0',256,C'C'                             GP12154
         SPACE 2
SAVE     DSECT ,             WORK AREA
SAVESPLN DS    F
SAVE13   DS    F
SAVEFWD  DS    A
SAVE14   DS    A
SAVE15   DS    A
SAVE0    DS    A
SAVE1    DS    A
SAVE2    DS    A
SAVE3    DS    A
SAVE4    DS    A
SAVE5    DS    A
SAVE6    DS    A
SAVE7    DS    A
SAVE8    DS    A
SAVE9    DS    A
SAVE10   DS    A
SAVE11   DS    A
SAVE12   DS    A
         SPACE 1
DB       DS    D             WORK AREA
         SERVDEFS ,          DEFINE BASICS
PROFLAGS DS    X
PFCOND   EQU   X'80'           REQUEST WAS CONDITIONAL
PFTERM   EQU   X'40'           REQUEST WAS @SERVICE TERMINATION GP12131
WTO      DS    0A,XL(PATWTOL)  WTO
WTOPARM  DS    CL8,C         INSERTION - NAME OF BAD PARM
WTOPVAL  DS    CL8,C         INSERTION - VALUE OF BAD PARM      GP12154
WTOPEND  EQU   *                                                GP12154
SAVEEND  EQU   *
         SPACE 2
CPRSECT  DSECT ,             POOL CONTROL BLOCK MAPPING
CPRLINK  DS    A             CONTROL BLOCK CHAIN
CPRHEAD  DS    CL24          (OPTIONAL) HEADER
CPRID    DS    F             POOL ID
CPRCHAIN DS    A             CHAIN OF STORAGE BLOCKS
CPRSP    DS    X             SUBPOOL
CPRKEY   DS    X             STORAGE KEY (NOT USED)
CPRFLAGS DS    X             PROCESSING FLAGS (SAME AS PARM)
         DS    X               RESERVED
CPRCOUNT DS    F             SECONDARY COUNT
CPRSIZE  DS    F             CELL SIZE
CPRLEN   EQU   *-CPRSECT       GETMAIN SIZE
         SPACE 1
BLKSECT  DSECT ,             STORAGE BLOCK MAPPING
BLKHEAD  DS    CL24          OPTIONAL HEADER (PRECEDES BLOCK)
BLKLINK  DS    A             NEXT BLOCK
BLKBXLE  DS    A,F,A         FIRST CELL/SIZE/LAST CELL
BLKMASK  DS    0X            USE MASK (CELL ASSIGNED IF BIT ON)
         SPACE 2
PARMLIST DSECT ,             CALLER'S PARAMETER LIST
PRMPCT   DS    F             PRIMARY COUNT
PRMSCT   DS    F             SECONDARY COUNT
PRMSIZ   DS    F             CELL SIZE
PRMSP    DS    AL1           SUBPOOL
PRMKEY   DS    AL1           STORAGE PROTECTION KEY
PRMFLGS  DS    X             FLAGS
PFLREAL  EQU   X'80'           LOC IS REAL
PFLANY   EQU   X'60'           LOC = ANY
PFLLOW   EQU   X'20'           LOC = BELOW
PFHTCB   EQU   X'10'           HAVE TCB
PFHKEY   EQU   X'08'           HAVE KEY
PFHHDR   EQU   X'04'           HAVE HEADER
PFOSYS   EQU   X'03'           OWNER IS SYSTEM
PFOPRI   EQU   X'01'           OWNER IS PRIMARY
         DS    X             UNUSED
PRMTCB   DS    A             TCB - IGNORED
PRMHDR   DS    CL24          POOL HEADER
         SPACE 1
         PRINT &PRTSYS
         YREGS ,
         SPACE 1
         IHAPSA ,
         CVT   ,
         IKJTCB ,
         END   ,
@@
//SYSGO DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(CYL,1),DISP=(MOD,PASS)
//SUBLPALK EXEC ASMFC,PARM='DECK,LIST,OBJECT'
//SYSLIB DD DSN=SYS1.MACLIB,DISP=SHR
//       DD DSN=SYS2.PVTMACS,DISP=SHR
//       DD DSN=SYS2.ESPMAC,DISP=SHR
//       DD DSN=SYS1.AMACLIB,DISP=SHR
//       DD DSN=SYS1.AMODGEN,DISP=SHR
//SYSIN DD DATA,DLM=@@
SUBLPALK TITLE 'S U B L P A L K  ***  LOCATE LPA RESIDENT CODE'
         COPY  OPTIONGB
         SPACE 1
         SYSPARM LIST=YES
         SPACE 1
***********************************************************************
*                                                                     *
*                                                                     *
*        COPYRIGHT 2003  EXPERT SYSTEM PROGRAMMING                    *
*                        176 OLD STAGE COACH ROAD                     *
*                        BRADFORD, VT 05033-8844                      *
*                                                                     *
*                    ALL RIGHTS RESERVED                              *
*                                                                     *
***********************************************************************
         EJECT ,
***********************************************************************
**                                                                   **
**  SUBLPALK  LOCATES A MODULE RESIDING IN AN LPA AREA               **
**                                                                   **
**  PARAMETER:  R1 POINTING TO A CL8 AREA CONTAINING THE DESIRED     **
**    MODULE NAME.                                                   **
**                                                                   **
**  WHEN USED WITH THE LOADLPA MACRO, R0 IS EITHER 0 OR A DCB        **
**    ADDRESS. WHEN THE DCB ADDRESS IS SUPPLIED, AND THE MODULE IS   **
**    NOT IN LPA, A LOAD WILL BE ISSUED.                             **
**                                                                   **
***********************************************************************
**                                                                   **
**  NORMAL RETURN:                                                   **
**    R0 : MODULE ENTRY POINT (WITH X'80' ON IF AM31)                **
**    R1 : LENGTH                                                    **
**    R15: 0 (MAIN MODULE)   4 (ALIAS)                               **
**   AR0 : MODULE LOAD POINT (IF AVAILABLE), ELSE SAME AS R0         **
**                                                                   **
**  ERROR RETURNS IN R15:                                            **
**                                                                   **
**  16 - ERROR PROCESSING PARAMETER LIST, OR OTHER SEVERE PROBLEM    **
**   8 - ERROR CSVQUERY OR LOAD                                      **
**                                                                   **
***********************************************************************
         SPACE 1
         PRINT &PRTSOR
SUBLPALK PGMHEAD ZERO31,BASE=R12,PARM=R1,AM=ANY,RM=24,LOC=BELOW
         STM   R0,R1,CALLR0  SAVE CALLER'S PARMS
         SPACE 1
         OICC  16,RESULT=RETCODE  PROVISIONAL RETURN CODE
         LA    R5,0(,R1)     DID USER PASS A PARM AREA ?
         LTR   R5,R5
         BZ    PGMEXIT       NO; FATAL BOO-BOO
         SPACE 1
         L     R3,CVTPTR     GET CVT IN R3 FOR IEAVVMSR
         USING CVTMAP,R3
         SPACE 1
         AIF   (NOT &MVSESA).NOCSVQ                             GP04234
         CSVQUERY INEPNAME=(R5),OUTLENGTH=@LENGTH,OUTEPA=@ENTRY,       *
               OUTLOADPT=@LOAD,OUTATTR1=DB,OUTATTR2=DB+1,OUTATTR3=DB+2,*
               RETCODE=RETCODE,MF=(E,MYQUERY)
         CH    R15,=H'8'     SUCCESSFUL ?
         BNL   TESTOLD       NO
         TM    DB,X'02'      ALIAS ENTRY ?
         BZ    NOTALIAS      NO
         MVICC 4             SET WARNING FLAG
NOTALIAS TM    DB+1,X'30'    ATTR2 AM31 OR AMANY ?
         BZ    LOADTWO       NO; RETURN                         GP03330
         OI    @ENTRY,X'80'  SET FOR AM31 PREFERRED
*---------------------------------------------------------------------*
*  NOTE THAT CSVQUERY (AS ISSUED) WILL FIND A MODULE IN LPA, ETC. AS  *
*  WELL AS IN JPA.                                                    *
*  WHEN THE USER PASSES A NON-ZERO DCB PARAMETER AND THE MODULE IS IN *
*  JPA, WE MUST ISSUE A LOAD FOR IT BECAUSE CALLER WILL EVENTUALLY    *
*  ISSUE A DELETE FOR IT.                                             *
*---------------------------------------------------------------------*
LOADTWO  TM    DB+2,X'40'    FOUND IN JPA ?                     GP03330
         BZ    SETLOAD       NO; RETURN LPA, ETC. ADDRESS       GP03330
         ICM   R4,15,CALLR0  DID USER SUPPLY A DCB PARAMETER ?  GP03330
         BZ    SETLOAD       NO; JUST RETURN                    GP03330
         B     TESTLOAD      YES; DO EXTRA LOAD                 GP03330
         SPACE 1
TESTOLD  CH    R15,=H'20'    UNAVAILABLE ON THIS SYSTEM ?
         BNE   TESTLOAD      NO; SEE WHETHER USER WANTS LOAD
.NOCSVQ  SPACE 1                                                GP04234
*---------------------------------------------------------------------*
*   SCAN THE MLPA FOR THE REQUESTED MODULE                            *
*---------------------------------------------------------------------*
         PUSH  USING
MLPALOOK ICM   R0,15,0(R5)
         ICM   R1,15,4(R5)
         L     R15,CVTQLPAQ  GET MLPA CDE CHAIN
         USING CDENTRY,R15
MLPALOOP ICM   R15,15,CDCHAIN   GET NEXT CDE; ELSE TRY PLPA
         BZ    PLPALOOK
         C     R1,CDNAME+4   MATCH ?
         BNE   MLPALOOP      NO; TRY NEXT
         C     R0,CDNAME     FULL MATCH ?
         BNE   MLPALOOP      NO; TRY NEXT
         B     FOUNDLP2      JOIN COMMON
         POP   USING
         SPACE 1
*---------------------------------------------------------------------*
*   SCAN THE PLPA FOR THE REQUESTED MODULE                            *
*---------------------------------------------------------------------*
         PUSH  USING
PLPALOOK L     R7,CVTLPDSR   IEAVVMSR
         ICM   R0,15,0(R5)
         ICM   R1,15,4(R5)
         BASR  R14,R7    NOTE THAT R7-R9 ARE CLOBBERED
           B   FOUNDLPA      MODULE FOUND
         XC    @ENTRY(8),@ENTRY    SHOW NOT FOUND
         MVICC 8,RESULT=RETCODE  SET LEVEL 8 ERROR
         B     TESTLOAD
         SPACE 1
*---------------------------------------------------------------------*
*   HAVE A CDE OR LPDE - EXTRACT LOAD ADDRESS AND SIZE                *
*---------------------------------------------------------------------*
         USING CDENTRY,R15
FOUNDLPA LR    R15,R0        COPY CDE ADDRESS
FOUNDLP2 MVICC 0             RESET THE RETURN CODE              GP04234
         ICM   R0,15,CDENTPT  LOAD ENTRY ADDRESS
         ST    R0,@ENTRY     RETURN ENTRY ADDRESS
         ST    R0,@LOAD      SET AS LOAD ADDRESS, ALSO
         AIF   (NOT &MVSESA).NOAMF                              GP04234
         TM    CDATTR2,CDEANYM  AM ANY ?
         BZ    LOOKMIN       NO
         OI    @ENTRY,X'80'  SET AM31 PREFERRED
.NOAMF   ANOP  ,                                                GP04234
LOOKMIN  TM    CDATTR,CDMIN  MINOR ?
         BZ    GETXTLST      NO; GET EXTENT LIST
         OICC  4             INDICATE ALIAS ENTRY
         TM    CDATTR2,CDXLE  EXTENT LIST PRESENT ?             GP05303
         BNZ   GETXTLST      YES; DON'T NEED MAJOR              GP05303
         ICM   R15,15,CDXLMJP  GET POINTER TO MAJOR
         BNZ   LOOKMIN
         B     SETLOAD       RESTORE REGS
GETXTLST L     R14,CDXLMJP   GET EXTENT LIST ADDRESS
         USING XTLST,R14
         MVC   @LOAD,XTLMSBAD LOAD ADDRESS
         MVC   @LENGTH,XTLMSBLA  LENGTH
         NI    @LENGTH,255-X'80'  RESET END OF LIST BIT
         B     SETLOAD       JOIN COMMON
         POP   USING         RESTORE ASSIGNMENTS
         SPACE 1
*---------------------------------------------------------------------*
*   USER WANTS A LOAD ISSUED WHEN DCB IS NON-ZERO.                    *
*     WHEN DCB (R0)<256, THEN USE DCB=0 ON LOAD                       *
*---------------------------------------------------------------------*
TESTLOAD ICM   R4,15,CALLR0  DID USER SUPPLY A DCB PARAMETER ?
         BZ    SETLOAD       NO; JUST RETURN ERROR CODE
         CH    R4,=H'256'    VALID DCB ?
         BNL   *+4+2         PERHAPS
         SR    R4,R4         ELSE FLAG TO REQUEST LOAD
         LOAD  DCB=(R4),EPLOC=(R5),ERRET=SETLOAD
         STM   R0,R1,@ENTRY   RETURN ENTRY / LENGTH
         XC    RETCODE,RETCODE    CLEAR RETURN
         ICM   R14,15,@LOAD  IS LOAD ADDRESS SET ?              GP03330
         BNZ   SETLOAD       YES; RETURN IT                     GP03330
         LR    R14,R0        COPY ENTRY ADDRESS                 GP03330
         LA    R14,0(,R14)   CLEAN AM BIT                       GP03330
         ST    R14,@LOAD     SET TO RETURN ENTRY AS LOAD        GP03330
         SPACE 1
*---------------------------------------------------------------------*
*   RETURN LOAD ADDRESS IN AR0                                        *
*---------------------------------------------------------------------*
SETLOAD  DS    0H                                               GP04234
         AIF   (NOT &MVSESA).NOLAM                              GP04234
         LAM   R0,R0,@LOAD   GET LOAD POINT
.NOLAM   SPACE 1                                                GP04234
*---------------------------------------------------------------------*
*   EXIT: WHEN R15=0, ENTRY ADD IN R0, LENGTH IN R1, LOAD ADDR IN AR0 *
*     R15=8 MODULE NOT FOUND; R15=16 INVALID NAME ADDRESS             *
*---------------------------------------------------------------------*
PGMEXIT  PGMEXIT COPYRET=(RETCODE,12)   RETURN R15, R0, R1
         SPACE 1
         LTORG ,
         SPACE 2
SAVE     DSECT ,             SAVE & WORK AREA
DB       DS    D
CALLR0   DS    A     1/2
CALLR1   DS    A     2/2
         SERVDEFS ,          EXPAND COMMON STUFF
         ORG   RETCODE+4     RETURN CODE
@ENTRY   DS    A     2/3     RETURN ENTRY POINT ADDRESS
@LENGTH  DS    F     3/3     RETURN LENGTH
@LOAD    DS    A             RETURN LOAD POINT ADDRESS
         SPACE 1
         AIF   (NOT &MVSESA).SKPCVSQ                            GP04234
         CSVQUERY MF=(L,MYQUERY)
.SKPCVSQ SPACE 1                                                GP04234
SAVEEND  EQU   *             END OF DYNAMIC AREA
         SPACE 2
         PRINT &PRTSYS
         CVT   DSECT=YES
         IHACDE ,
         IHAXTLST ,
         END   ,
@@
//SYSGO DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(CYL,1),DISP=(MOD,PASS)
//SUBCAT EXEC ASMFC,PARM='DECK,LIST,OBJECT'
//SYSLIB DD DSN=SYS1.MACLIB,DISP=SHR
//       DD DSN=SYS2.PVTMACS,DISP=SHR
//       DD DSN=SYS2.ESPMAC,DISP=SHR
//       DD DSN=SYS1.AMACLIB,DISP=SHR
//       DD DSN=SYS1.AMODGEN,DISP=SHR
//SYSIN DD DATA,DLM=@@
SUBCAT   TITLE 'S U B C A T  ***  ICF CATALOG RETRIEVAL SUBROUTINE'
         MACRO ,
&NM      CSIFD &AD,&LEN,&FILL=0
         GBLC  &MACPLAB
&MACPLAB SETC  '&NM'
         LCLA  &K
         LCLC  &L
&K       SETA  K'&AD
&L       SETC  'L'''
         MACPARM R2,&AD .    GET OUTPUT ADDRESS
         AIF   ('&LEN' NE '').LEN
         AIF   ('&AD'(1,1) NE '(').LATTR
         AIF   (&K LT 4).TLEN
         AIF   ('&AD'(2,1) EQ '(').LATTR
         AIF   ('&AD'(&K,1) EQ ')').TLEN
.LATTR   MACPARM R3,&L&AD    USE IMPLICIT OUTPUT LENGTH
         AGO   .BAL
.TLEN    AIF   ('&LEN' NE '').LEN
         MNOTE 4,'LENGTH REQUIRED'
.LEN     MACPARM R3,&LEN     GET OUTPUT LENGTH
.BAL     MACPARM CSWDB,&FILL,OP=MVI,OPR=MVI,MODE=EVEN,NULL=SKIP
         MACPARM R14,GETVAL,OP=BAL
         MEND  ,
         SPACE 2
         MACRO ,
&NM      CETY  &ETYPE,&DESC,&OPRTNE,&OPFD
         LCLC  &LOP,&LFD
         GBLB  &ZZCETYF
&LOP     SETC  '&OPRTNE'
&LFD     SETC  '&OPFD'
         AIF   ('&LOP' NE '').GOTOP
&LOP     SETC  'FMTDS'
.GOTOP   AIF   ('&LFD' NE '').GOTFD
&LFD     SETC  '0'
.GOTFD   AIF   ('&DESC'(1,1) EQ '''').GOTQUOT
&NM      DC    CL1'&ETYPE ',CL7'&DESC ',SL2(&LOP,&LFD)
         AGO   .COMMON
.GOTQUOT ANOP  ,
&NM      DC    CL1'&ETYPE ',CL7&DESC,SL2(&LOP,&LFD)
.COMMON  AIF   (&ZZCETYF).MEND
&ZZCETYF SETB  1
CETOETYP EQU   0,1,C'C'
CETODESC EQU   1,7,C'C'
CETOFORM EQU   8,2,C'S'
CETOFILD EQU   10,2,C'S'
.MEND    MEND  ,
         SPACE 2
         COPY  OPTIONGB
         SPACE 1
         SYSPARM LIST=YES
         SPACE 2
***********************************************************************
*                                                                     *
*                                                                     *
*        COPYRIGHT 2002,2009  EXPERT SYSTEM PROGRAMMING               *
*                        176 OLD STAGE COACH ROAD                     *
*                        BRADFORD, VT 05033-8844                      *
*                                                                     *
*                    ALL RIGHTS RESERVED                              *
*                                                                     *
***********************************************************************
         EJECT ,
***********************************************************************
**                                                                   **
**  SUBCAT - PERFORM CATALOG LOOKUP FUNCTIONS                        **
**                                                                   **
**    THIS SUBROUTINE IS INVOKED USING THE CATLOOK MACRO, AND THE    **
**    CATSPARM WORK AREA MAPPING. SEE EXHCCDMP OR PUNK FOR USE.      **
**                                                                   **
**    FOR AN ASSEMBLY WITH MVS/XA OR LATER, IGGCSI00 IS CALLED;      **
**    FOR EARLIER SYSTEMS, SUPER-LOCATE IS USED. THERE IS NO SUPPORT **
**      FOR MFT OR MVT.                                              **
**                                                                   **
***********************************************************************
         GBLB  &BAKR         SET NEWFANGLED WHEN ASMTRACE READY
&BAKR    SETB  (&MVSXA)      READY TO TRY IT
         PRINT NOGEN
         AIF   (NOT &BAKR).NOTBAKR
SUBCAT   START 0
SUBCAT   AMODE 31            I GET TO BE ON TOP
SUBCAT   RMODE ANY
         BAKR  R14,0         STACK CALLER'S DATA
         LA    R14,SUBCATSL-SUBCAT  OFFSET
         BASR  R12,0         MAKE A NEW BASE
SUBCATSL SLR   R12,R14       MAKE IT MATCH ASSEMBLY OFFSETS
         USING SUBCAT,R12    DECALRE IT
         AGO   .COMBAKR
.NOTBAKR ANOP  ,
SUBCAT   PGMHEAD ZERO12,BASE=R12,PARM=R1,AM=31,RM=ANY
.COMBAKR AIF   (NOT &MVSXA).USEVSAM                             GP09157
         LA    R1,0(,R1)     CLEAN HIGH BIT                     GP03261
         LTR   R8,R1         GET CALLER'S PARM POINTER          GP03261
         BZ    MISSPARM                                         GP03261
         USING CSPDSECT,R8   DECLARE IT
         ICM   R10,15,CSP@WORK  ANYTHING IN WORK ANCHOR ?
         BNZ   REQCHECK      YES; ASSUME INITIALIZED
         SPACE 1
***********************************************************************
**                                                                   **
**  INITIALIZE:  OBTAIN WORK AREA AND SAVE ADDRESS IN USER'S PARM    **
**    USER MUST MAINTAIN IT FOR LATER MODULE AND STORAGE CLEANUP.    **
**    INITIALIZE THE AREA.                                           **
**                                                                   **
***********************************************************************
REQFIRST ICM   R0,1,CSP$REQ    GET REQUEST TYPE                 GP05277
         BZ    GOODEXIT      CLOSE; NO PRIOR OPEN               GP05277
         SR    R2,R2
         IC    R2,CSP#SP     LOAD SUBPOOL (GENERALLY = 0)
         LA    R3,CSWSIZE    SET STORAGE SIZE
         STORAGE OBTAIN,SP=(R2),LENGTH=(R3),LOC=BELOW  GET IT OR ABEND
         STCM  R3,7,CSP#SPL  SAVE SIZE OBTAINED
         ST    R1,CSP@WORK   AND SAVE ADDRESS
         SR    R15,R15       JUST IN CASE
         LR    R2,R1         COPY ADDRESS
         MVCL  R2,R14        CLEAR IT TO ZEROES
         LR    R10,R1        MAKE IT OFFICIAL
         USING CSWDSECT,R10  DECLARE IT
         LOAD  EPLOC==CL8'IGGCSI00',ERRET=BADENTRY  TRY TO LOAD IT
         ST    R0,CSW@CSI    SAVE IT
         ICM   R0,15,CSP@SCMP  DID CALLER LOAD SUBCOMP ?        GP03046
         BNZ   REQFIRCM      YES; USE IT                        GP03046
         LOAD  EPLOC==CL8'SUBCOMP'  LOAD COMPARE ROUTINE        GP03046
         ST    R0,CSP@SCMP   REMEMBER THE ADDRESS               GP03046
         OI    CSWPFLGS,CSWPFCMP  REMEMBER WE LOADED SUBCOMP    GP03046
REQFIRCM LA    R15,CSW@CRTN  GET RETURN CODE ADDRESS
         LA    R0,CSW@CFLD   SELECTION FIELDS
         STM   R15,R1,CSW@CPRM   MAKE PARM ADDRESSES
         MVI   CSW@CFLD,C' '   INITIALIZE REQUEST FIELDS
         MVC   CSW@CFLD+1(CSW@C#EN-CSW@CFLD-1),CSW@CFLD  CLEAR ALL
         MVC   CSW@C#EN(PATFIENM*8+2),PATFIELD  PATTERN FIELD NAMES
         MVI   CSW@CO1C,C'Y'  SEARCH MORE THAN ONE CATALOG
         LA    R0,CSWCM@WK   GET POINTER TO SUBCOMP ROOT
         ST    R0,CSWCMPRM+12  INITIALIZE IT
         L     R3,=A(256*1024)  SET SIZE
         LR    R4,R3         SAVE OVER MVCL
         STORAGE OBTAIN,LENGTH=(R3)         GET SOME STORAGE
         LA    R15,CSW@CRTN  GET RETURN CODE ADDRESS
         LA    R0,CSW@CFLD   SELECTION FIELDS
         STM   R15,R1,CSW@CPRM   MAKE PARM ADDRESSES
         LR    R2,R1
         SR    R15,R15       CLEAR LENGTH AND FILL BYTE
         MVCL  R2,R14        CLEAR IT
         ST    R4,0(,R1)     INITIALIZE WORK AREA LENGTH
         PUSH  USING
         L     R5,CVTPTR
         NUSE  CVT,R5
         ICM   R5,15,CVTCBSP    -> AMCBS
         BZ    DONECAX       HUH?
         USING AMCBS,R5
         LA    R5,CBSCAXCN-(CAXCHN-IGGCAXWA)
         USING IGGCAXWA,R5
NEXTCAX  ICM   R5,15,CAXCHN
         BZ    DONECAX
         LA    R1,CSW@CAHD   ELSE SEE IF ALREADY ON CHAIN
         USING CSCDSECT,R1
LOOPCAX  ICM   R1,15,CSCLINK    ANY MORE ?
         BZ    UCAXADD
         CLC   CSCNAME,CAXCNAM  QUEUED BEFORE ?
         BNE   LOOPCAX       NO; TRY AGAIN
         B     SKIPCAX       YES; DON'T ADD TO CHAIN
UCAXADD  STORAGE OBTAIN,LENGTH=CSCSIZE
         L     R15,CSW@CAHD  GET OLD POINTER
         ST    R15,CSCLINK   CHAIN IN
         ST    R1,CSW@CAHD
         ICM   R14,15,CSW@CATL  IS TAIL SET?
         BNZ   TAILCAX       YES; LET IT BE
         ST    R1,CSW@CATL   SET END POINTER
TAILCAX  MVC   CSCNAME,CAXCNAM
         ST    R15,CSCCAX    REMEMBER THE CAX ADDRESS
         XC    CSCFLGS(4),CSCFLGS  CLEAR REST
         TM    CAXFLGS,CAXMCT  MASTER CATALOG ENTRY ?
         BZ    NEXTCAX       NO
         MVC   CSWMSCAT,CAXCNAM
         ST    R1,CSW@CAMS   REMEMBER THE MASTER
SKIPCAX  B     NEXTCAX       NO
DONECAX  MVC   CSWCUCAT,CSWMSCAT SET DEFAULT
         B     REQCHECK      SEE WHY WE WERE CALLED
         POP   USING
         SPACE 1
***********************************************************************
**                                                                   **
**  CALLER PASSES A REQUEST NUMBER (IN R0) AND A DATA AREA           **
**    VALID REQUESTS ARE:                                            **
**                                                                   **
**    0 - TERMINATE CATALOG PROCESSING                               **
**    1 - GET A SINGLE ENTRY                                         **
**    2 - START PROCESSING A GENERIC REQUEST                         **
**    3 - CONTINUE WITH NEXT GENERIC                                 **
**                                                                   **
***********************************************************************
REQCHECK SR    R2,R2
         IC    R0,CSP$REQ    GET REQUEST TYPE
         BIX   ERR=BADENTRY,PFX=REQ,   BRANCH BY REQUEST TYPE          *
               LOC=(CLOSE,LOOK,INIT,LOOP)
         SPACE 1
***********************************************************************
**                                                                   **
**  ALL FUNCTIONS RETURN HERE                                        **
**                                                                   **
**    GOODEXIT - SETS RC=0                                           **
**    BADRET4  - SETS RC=4                                           **
**    BADRET8  - SETS RC=8                                           **
**    BADENTRY - SETS RC=12                                          **
**                                                                   **
***********************************************************************
BADENTRY LA    R15,12        SET DISASTER
         B     COMMEXIT      GO TO COMMON EXIT
         SPACE 1
BADRET8  LA    R15,8         SET ERROR
         B     COMMEXIT      GO TO COMMON EXIT
         SPACE 1
BADRET4  LA    R15,4         SET WARNING
         B     COMMEXIT      GO TO COMMON EXIT
         SPACE 1
MISSPARM LA    R15,16        PARM MISSING - NO WORK AREA        GP03261
         B     PGMEXIT       TAKE IMMEDIATE ERROR RETURN        GP03261
         SPACE 1                                                GP03261
GOODEXIT SR    R15,R15       SET GOOD COMPLETION
         SPACE 1
COMMEXIT ST    R15,CSPRCOD   ALSO PASS RETURN CODE IN PARM LIST
PGMEXIT  DS    0H                                               GP03261
         AIF   (&BAKR).DOPR
         LR    R9,R15
         PGMEXIT RC=(R9)
         AGO   .COMPR
.DOPR    ANOP  ,
         PR    ,             RETURN TO CALLER
.COMPR   SPACE 2
***********************************************************************
**                                                                   **
**  CLOSE ENTRY - DELETE ALL LOADED MODULES                          **
**                FREE GOTTEN STORAGE                                **
**                RESET CALLER'S POINTERS                            **
**                                                                   **
***********************************************************************
         PUSH  USING
REQCLOSE ICM   R15,15,CSW@CSI  DID WE LOAD IGGCSI00 ?
         BZ    CLONCSI       NO?
         DELETE EPLOC==CL8'IGGCSI00'  FREE IT
         XC    CSW@CSI,CSW@CSI  RESET IT
CLONCSI  ICM   R1,15,CSW@CPRM+8  STORAGE GOTTEN ?
         BZ    CLONDS
         ICM   R2,15,0(R1)   AND GET LENGTH
         BZ    CLONDS        OOPS?
         STORAGE RELEASE,ADDR=(1),LENGTH=(R2)
         XC    CSW@CPRM+8(4),CSW@CPRM+8  RESET IT
CLONDS   ICM   R0,15,CSWCM@WK  COMPARE WORK AREA?
         BZ    CLONCMP
         L     R15,CSP@SCMP  LOAD SUBCOMP ADDRESS               GP03046
         SUBCALL (R15),('END'),MF=(E,CSWCMPRM) FREE THE WORK AREA
         XC    CSWCM@WK,CSWCM@WK  JUST IN CASE
CLONCMP  L     R2,CSW@CAHD   POINT TO CATALOG NAME CHAIN
         LA    R4,CSCSIZE    LENGTH OF ONE CATALOG ENTRY
CLONLCSC LTR   R3,R2         ANY MORE ?
         BZ    CLONNCSC      DONE
         L     R2,CSCLINK-CSCLINK(,R3)   GET NEXT ELEMENT, IF ANY
         STORAGE RELEASE,ADDR=(R3),LENGTH=R4
         B     CLONLCSC      TRY AGAIN
CLONNCSC XC    CSW@CAHD(L'CSW@CAHD+L'CSW@CATL),CSW@CAHD  ZERO POINTERS
         SR    R2,R2
         SR    R3,R3
         IC    R2,CSP#SP     GET WORK AREA SUBPOOL
         ICM   R3,7,CSP#SPL    AND WORK AREA LENGTH
         STORAGE RELEASE,SP=(R2),LENGTH=(R3),ADDR=(R10)
         SR    R10,R10       RESET
         ST    R10,CSP@WORK  CLEAR IN USER'S PARM
         TM    CSWPFLGS,CSWPFCMP  DID WE LOAD SUBCOMP ?         GP03046
         BZ    GOODEXIT      NO; RETURN TO CALLER               GP03046
         DELETE EPLOC==CL8'SUBCOMP'  FREE IT                    GP03046
         ST    R10,CSP@SCMP  AND CLEAR IT                       GP03046
         B     GOODEXIT      RETURN TO CALLER                   GP03046
         POP   USING
         SPACE 2
***********************************************************************
**                                                                   **
**  LOOK ENTRY - GET A SINGLE NAME FROM THE CATALOG                  **
**               ADDRESS OF THE NAME IS PASSED IN CSP@REQ            **
**                                                                   **
***********************************************************************
         PUSH  USING
REQLOOK  L     R2,CSP@REQ   GET REQUEST POINTER
         N     R2,=X'7FFFFFFF'  KILL HIGH BIT AND SET CC
         BZ    BADRET8       OOPS
         OI    CSWPFLGS,CSWPFLUK  SHOW LOOK INVOKED (FOR LOOP MIDDLE)
         OI    CSWPFLGS,CSWPF1CT  ONE CATALOG ONLY
         MVI   CSW@CCAT,C' '
         MVC   CSW@CCAT+1(CSW@C#EN-1-CSW@CCAT),CSW@CCAT  INITIALIZE
         TM    CSP$FLGS,CSP$FGUC  USER PROVIDED CATALOG NAME ?
         BZ    REQLOOK2      NO                                 GP03058
         MVC   CSW@CCAT,CSPRCAT   YES; USE IT
         OI    CSWPFLGS,CSWPF1CT  ONE CATALOG ONLY              GP03058
REQLOOK2 MVC   CSWCIVMS(L'CSPMVOL),CSPMVOL  VOL-SER MASK        GP03058
         TM    CSP$FLGS,CSP$FG1C  USER REQUESTED ONE CATALOG ?  GP03058
         BZ    *+8           NO                                 GP03058
         OI    CSWPFLGS,CSWPF1CT  PROPAGATE IT                  GP03058
         SR    R3,R3         CLEAR FOR IC
         ST    R3,CSW@CBXL   FORCE CATALOG READ
         ICM   R3,3,CSP#LEN  GET NAME LENGTH
         BP    *+8             OK
         LA    R3,L'CSW@CFLT  ELSE SET MAX LENGTH
         LA    R0,CSW@CFLT   MOVE TO CSI PARM LIST
         LA    R1,L'CSW@CFLT  GET LENGTH
         ICM   R3,8,BLANKS   BLANK FILL
         MVCL  R0,R2         MOVE AND FILL
         MVI   CSW@CO1C,C'Y'  SEARCH MORE THAN ONE CATALOG
         ICM   R1,15,CSW@CAMS  DO WE HAVE A MASTER CATALOG POINTER?
         BZ    REQLOOKN      NO
         OI    CSCFLGS-CSCDSECT(R1),CSCFGUS
REQLOOKN BAS   R14,CALLCSI   GO TO COMMON CALL ROUTINE
         MVC   CSPRCAT,CSWCUCAT  RETURN CATALOG
         LTR   R15,R15       GOOD ?
         BNZ   COMMEXIT      NO; RETURN
         L     R14,CSP@REQ   GET USER'S REQUEST
         CLC   CSPRDSN,0(R14)  MATCHES REQUEST?
         BE    COMMEXIT      YES; GOOD
         CLI   CSPRTYP,C'0'  DID WE GET THE (FIRST) CATALOG NAME ?
         BE    REQLOOKN      YES; GET THE NEXT ENTRY
         B     COMMEXIT      ELSE RETURN WHAT WE HAVE
         POP   USING
         SPACE 2
***********************************************************************
**                                                                   **
**  INIT ENTRY - INTERPRET PASSED PARAMETER AS A MASK                **
**                                                                   **
**    AFTER FIRST RETURN, IF GOOD, USE CATCALL LOOP FOR ADDITIONAL   **
**    DATA                                                           **
**                                                                   **
***********************************************************************
REQINIT  L     R2,CSP@REQ   GET REQUEST POINTER
         N     R2,=X'7FFFFFFF'  KILL HIGH BIT AND SET CC
         BZ    BADRET8       OOPS
         SPACE 1
***********************************************************************
**                                                                   **
**  FOR OTHER THE FIRST CALL, RESET THE CATALOG 'USED' BIT           **
**  IN ALL CATALOG ENTRIES                                           **
**                                                                   **
***********************************************************************
         PUSH  USING
         LA    R1,CSW@CAHD-(CSCLINK-CSCDSECT)  POINT TO HEAD
         USING CSCDSECT,R1
REQINIT0 ICM   R1,15,CSCLINK  GET NEXT CATALOG ENTRY OR OUT
         BZ    REQINITS      LEAVE IF DONE
         XC    CSCFLGS(4),CSCFLGS  CLEAR REST
         B     REQINIT0      REPEAT UNTIL DONE
         POP   USING
REQINITS NI    CSWPFLGS,255-CSWPFLUK-CSWPFBUF-CSWPFONE-CSWPF1CT
         ICM   R1,15,CSW@CAMS  DO WE HAVE A MASTER CATALOG POINTER?
         BZ    REQINITT      NO
         OI    CSCFLGS-CSCDSECT(R1),CSCFGUS
REQINITT MVI   CSW@CCAT,C' '
         MVC   CSW@CCAT+1(CSW@C#EN-1-CSW@CCAT),CSW@CCAT  INITIALIZE
         TM    CSP$FLGS,CSP$FGUC  USER PROVIDED CATALOG NAME ?
         BZ    REQINIT2      NO                                 GP03058
         MVC   CSW@CCAT,CSPRCAT   YES; USE IT
         OI    CSWPFLGS,CSWPF1CT  ONE CATALOG ONLY              GP03058
REQINIT2 MVC   CSWCIVMS(L'CSPMVOL),CSPMVOL  VOL-SER MASK        GP03058
         TM    CSP$FLGS,CSP$FG1C  USER REQUESTED ONE CATALOG ?  GP03058
         BZ    *+8           NO                                 GP03058
         OI    CSWPFLGS,CSWPF1CT  PROPAGATE IT                  GP03058
         SR    R3,R3         CLEAR FOR IC
         ST    R3,CSW@CBXL   DON'T USE CURRENT BUFFER CONTENTS  GP03041
         ICM   R3,3,CSP#LEN  GET NAME LENGTH
         BP    *+8             OK
         LA    R3,L'CSW@CFLT  ELSE SET MAX LENGTH
         LA    R0,CSPMASK    MOVE TO CSI PARM LIST
         LA    R1,L'CSPMASK   GET LENGTH
         ICM   R3,8,BLANKS   BLANK FILL
         MVCL  R0,R2         MOVE AND FILL
         BAS   R9,MAKEMASK
         CLI   CSW@CFLT,C'*'  GLOBAL SEARCH ?
         BNE   REQINITC      NO; ONE CATALOG IS PLENTY
         MVI   CSW@CO1C,C'Y'  SEARCH MORE THAN ONE CATALOG
REQINITC BAS   R14,CALLCSI   GO TO COMMON CALL ROUTINE
         MVC   CSPRCAT,CSWCUCAT  RETURN CATALOG
         LA    R1,CSW@CAHD   POINT TO MY CATALOG CHAIN
         PUSH  USING
         USING CSCDSECT,R1   MAP MY CATALOG CHAIN
REQINITL ICM   R1,15,CSCLINK  ANOTHER ENTRY ?
         BZ    REQINITQ      NO; LET IT BE
         CLC   CSPRCAT,CSCNAME  IS THIS IT ?
         BNE   REQINITL      NO; TRY ANOTHER
         OI    CSCFLGS,CSCFGUS  SHOW CATALOG USED
         POP   USING
REQINITQ LTR   R15,R15       GOOD ?
         BNZ   COMMEXIT      NO; RETURN
         OI    CSWPFLGS,CSWPFBUF  SHOW LOOP DATA AVAILABLE
         CLI   CSPRTYP,C'0'  DID WE GET THE (FIRST) CATALOG NAME ?
         BNE   COMMEXIT
         SPACE 2
***********************************************************************
**                                                                   **
**  LOOP ENTRY - EXTRACT PRIOR CATALOG AND RESTART DSN FROM          **
**    CALLER'S PARM, AND GET THE NEXT DSN, IF ANY                    **
**                                                                   **
***********************************************************************
REQLOOP  TM    CSWPFLGS,CSWPFBUF  BEEN HERE BEFORE ?
         BZ    REQINIT       NO; NEED TO INITIALIZE FIRST
         TM    CSP$FLGS,CSP$FG1C  USER REQUESTED ONE CATALOG ?  GP03058
         BZ    *+8           NO                                 GP03058
         OI    CSWPFLGS,CSWPF1CT  PROPAGATE IT                  GP03058
         TM    CSP$FLGS,CSP$FGMC  MASK CHANGED ?
         BNZ   REQLOOPM
         TM    CSWPFLGS,CSWPFLUK  REQUEST INTERRUPTED ?
         BZ    REQLOOPC      NO; JUST CONTINUE
REQLOOPM NI    CSP$FLGS,255-CSP$FGMC  RESET CHANGE FLAG
         MVC   CSW@CFLT,CSPMASK  SET MASK
         MVC   CSW@CCAT,CSPRCAT  CATALOG NAME
         MVC   CSW@CRES,CSPRDSN  PRIOR DSN
         XC    CSW@CBXL,CSW@CBXL  REQUEST CSI CALL
         MVI   CSW@CORS,C'Y' FAKE THE RESTART FLAG
         MVI   CSW@CO1C,C' '  SEARCH ONLY THIS CATALOG
         MVC   CSWCIVMS(L'CSPMVOL),CSPMVOL  VOL-SER MASK
REQLOOPC BAS   R14,CALLCSI   GO TO COMMON CALL ROUTINE
         MVC   CSPRCAT,CSWCUCAT  RETURN CATALOG
         B     COMMEXIT      ELSE RETURN WHAT WE HAVE
         SPACE 1
MAKEMASK TM    CSPMASK,255-C' '  NULL OPERAND?
         BNZ   MAKEDCLI      NO; USE IT
         MVC   CSPMASK(2),=C'**' SET DEFAULT - EVERYTHING
MAKEDCLI L     R15,CSP@SCMP  LOAD SUBCOMP ADDRESS               GP03046
         SUBCALL (R15),(PATCMMSK,CSWCIMSK,CSPMASK,CSWCM@WK),           *
               MF=(E,CSWCMPRM)                                  GP03093
         MVC   CSW@CFLT,CSWCIMSK
         CH    R15,=H'8'     DID IT WORK ?
         BR    R9            RETURN TO CALLER
         SPACE 2
***********************************************************************
**                                                                   **
**  CALLCSI - SUBROUTINE TO INVOKE IGGCSI00 AND DO MINOR RECOVERY    **
**                                                                   **
***********************************************************************
CALLCSI  SR    R15,R15       PRESET FOR ZERO RETURN CODE
         STM   R0,R15,CSWSAV1   SAVE ALL REGISTERS
         LM    R5,R7,CSW@CBXL  TEST RESTART POINTER
         LTR   R5,R5         ANY RESTART ?
         BNZ   BXLECSI       YES; RESTART IN THIS BUFFER
         SPACE 2
*---------------------------------------------------------------------*
*                                                                     *
*  BUFFCSI:  PRIMARY CATALOG LOOKUP LOOP                              *
*    CALL IGGCSI00, PROCESS RETURN, GO TO NEXT CATALOG AS APPROPRIATE *
*                                                                     *
*---------------------------------------------------------------------*
BUFFCSI  XC    CSW@CBXL,CSW@CBXL   CLEAR POINTER
         L     R15,CSW@CSI   GET CATALOG LOOKUP ADDRESS
         LA    R1,CSW@CPRM   GET PARM
         BASSM R14,R15       CALL IT
         MVC   CSPREAS,CSW@CRTN   ALSO SAVE FOR CALLER          GP03044
         CLC   =X'C1C87804',CSW@CRTN  BAD CATALOG RETURN ?
         BE    CATCURSE      YES; TRY ANOTHER
         CH    R15,=H'4'     CHECK RETURN
         BL    PASSCSI       GOOD; USE DATA
         BH    ERRCSI        TOO BAD
*---------------------------------------------------------------------*
*                                                                     *
*  FOR RETURN CODE 4, WE HAVE TWO POSSIBILITIES:                      *
*    REASON CODE 100 - RETURNED BUFFER CONTAINS A BAD ENTRY           *
*    OTHER - CATALOG ACCESS FAILED; TRY ANOTHER CATALOG               *
*                                                                     *
*---------------------------------------------------------------------*
         CLC   =X'6404',CSW@CRTN+2  CODE 4 / REASON 100 (ONE/MORE BAD?)
         BE    PASSCSI       YES; ACCEPT WITH ERROR/MISSING DATA
         CLC   =X'6C04',CSW@CRTN+2  CODE 4 / REASON 108 (CAT NOT OPEN?)
         BNE   ERRCSI        NO
         TM    CSP$FLGS,CSP$FULL  DOES USER WANT NOTIFICATION ? GP03044
         BNZ   ERRCSI        YES; RETURN ERROR                  GP03044
         B     CATCURSE      NO; SEE WHETHER ANOTHER CATALOG WANTED
         SPACE 1
*---------------------------------------------------------------------*
*                                                                     *
*  CHECK RETURNED BUFFER - FIRST RESPONSE ALWAYS HAS THE CATALOG      *
*    RECORD FOR THE RETURNED DATA                                     *
*                                                                     *
*---------------------------------------------------------------------*
PASSCSI  L     R5,CSW@CPRM+8   GET THE RESULT AREA
         USING CSIRWORK,R5   DECLARE IT
         LH    R2,CSINUMFD   GET NUMBER OF FIELDS
         SLA   R2,1          CONVERT TO LENGTH OF LENGTHS
         ST    R2,CSWFDLEN   SAVE
         L     R7,CSIUSDLN   LENGTH USED
         AR    R7,R5         LAST BYTE + 1
         BCTR  R7,0          LAST BYTE
         LA    R5,CSIRWENT   NOW POINT TO ENTRIES
         DROP  R5
*---------------------------------------------------------------------*
*                                                                     *
*  PROCESS NEXT ENTRY IN THE CURRENT BUFFER                           *
*                                                                     *
*---------------------------------------------------------------------*
         USING CSIRWENT,R5   UPDATE MAPPING
TESTCSI  LA    R6,CSICLENG   PROVISIONALLY SET FOR CATALOG ENTRY
         STM   R5,R7,CSW@CBXL   SAVE FOR PROCESSING
         CLI   CSICTYPE,CSIETYP0  CATALOG ENTRY ?
         BE    CATLCSI       YES; UP BY FIXED LENGTH
         TM    CSIEFLG,CSIENTER  ERROR ?
         BNZ   FXERRCSI      OOPS; TOO BAD
         ICM   R6,3,CSITOTLN  GET TOTAL LENGTH
         LA    R6,CSITOTLN-CSIRWENT(,R6)    SIZE OF THIS ENTRY
         ST    R6,CSW@CBXL+4  UPDATE TRUE LENGTH
         TM    CSIEFLG,CSIEDATA  HAVE DATA ?
         BNZ   DATACSI       YES; GET THEM
         B     BXLECSI       BUMP
         SPACE 1
*---------------------------------------------------------------------*
*                                                                     *
*  SINGLE ENTRY RETURNED WITH ERROR FLAG ON - SHOW INDICATIVE DATA    *
*                                                                     *
*---------------------------------------------------------------------*
FXERRCSI BAS   R14,GETTYPE   DO MINIMAL PROCESSING
         B     BXLECSI       INCREMENT TO NEXT ENTRY
         SPACE 1
*---------------------------------------------------------------------*
*                                                                     *
*  FOUND A CATALOG ENTRY - MATCH AGAINST REQUEST, AND DISPLAY.        *
*                                                                     *
*---------------------------------------------------------------------*
CATLCSI  TM    CSWPFLGS,CSWPFCT1 FIRST CATALOG EVER?
         BNZ   CATLCSI2      NO; TREAT AS DATA
         MVC   CSWCUCAT,CSIENAME PRESERVE CURRENT CATALOG NAME
         OI    CSWPFLGS,CSWPFCT1 FIRST CATALOG SET
         PUSH  USING
         LA    R1,CSW@CAHD   POINT TO CATALOG CHAIN
         USING CSCDSECT,R1
CATLUKLP ICM   R1,15,CSCLINK  GET NEXT CATALOG
         BZ    CATLCSI2      TREAT AS DATA
         CLC   CSWCUCAT,CSCNAME  MATCHING NAME?
         BNE   CATLUKLP      NO
         POP   USING
CATLCSI2 TM    CSP$FLGS,CSP$FULL LOTS OF STUFF ?
         BNZ   DATACSI       YES; TREAT AS DATA
         BAS   R14,GETTYPE   DO MINIMAL PROCESSING
         TM    CSP$FLGS,CSP$FGCP  TEST FOR MASK MATCH ?
         BZ    GOODRET       NO; RETURN IT
         L     R15,CSP@SCMP  LOAD SUBCOMP ADDRESS               GP03046
         SUBCALL (R15),(PATCMDSN,CSPRDSN,CSPMASK),MF=(E,CSWCMPRM)
         BXH   R15,R15,BXLECSI  NO MATCH; TRY SOMETHING ELSE
*---------------------------------------------------------------------*
*                                                                     *
*  RETURN TO CALLER WITH CC IN R15, AND APPROPRIATE DATA IN CSP/CSW   *
*                                                                     *
*---------------------------------------------------------------------*
GOODRET  SR    R15,R15       SET GOOD RETURN
         ST    R15,CSWSAV1+4*R15  FOR RELOAD
         B     CALLCSIX
DODONE   LA    R15,4         SHOW LOGICAL END OF SEARCH
         B     COMCLEAR
ERRCSI   LA    R15,8         SET AN ERROR
COMCLEAR ST    R15,CSWSAV1+4*R15  FOR RELOAD
         XC    CSW@CBXL,CSW@CBXL  CLEAR RESTART JUST IN CASE
CALLCSIX LM    R0,R15,CSWSAV1   RESTORE ALL REGISTERS
         BR    R14           RETURN TO CALLER
         SPACE 1
*---------------------------------------------------------------------*
*                                                                     *
*  BUMP TO NEXT ENTRY IN BUFFER. IF NONE, CHECK FOR MORE DATA, OR     *
*    READ ANOTHER CATALOG.                                            *
*                                                                     *
*---------------------------------------------------------------------*
LOOPCSI  LM    R5,R7,CSW@CBXL   GET POINTERS
BXLECSI  BXLE  R5,R6,TESTCSI    LOOK FOR ANOTHER ENTRY
         CLI   CSW@CORS,C'Y' ARE THERE MORE DATA ?
         BE    BUFFCSI       YES; GET ANOTHER BUFFER
         SPACE 1
*---------------------------------------------------------------------*
*                                                                     *
*  CURRENT CATALOG EXHAUSTED, OR NO MATCHES FOUND                     *
*  LOOK FOR ANOTHER CATALOG, AND REPEAT REQUEST                       *
*                                                                     *
*---------------------------------------------------------------------*
         PUSH  USING
CATCURSE TM    CSWPFLGS,CSWPF1CT  ONE CATALOG ONLY ?
         BNZ   DODONE        YES; END OF REQUEST
         LA    R1,CSW@CAHD-(CSCLINK-CSCDSECT)  POINT TO HEAD
         USING CSCDSECT,R1
CATCURSL ICM   R1,15,CSCLINK   ANY MORE ?
         BZ    DODONE        NO; FINISHED
         TM    CSCFLGS,CSCFGUS  ALREADY USED ?
         BNZ   CATCURSL      YES; LOOK AGAIN
         OI    CSCFLGS,CSCFGUS  SET IT USED
         MVI   CSW@CFLD,C' '   INITIALIZE REQUEST FIELDS
         MVC   CSW@CFLD+1(CSW@C#EN-CSW@CFLD-1),CSW@CFLD  CLEAR ALL
         MVC   CSW@C#EN(PATFIENM*8+2),PATFIELD  PATTERN FIELD NAMES
         MVC   CSW@CCAT,CSCNAME  SET NEW CATALOG
         MVC   CSWCUCAT,CSCNAME  SET NEW CATALOG
         MVC   CSW@CFLT,CSWCIMSK RESET THE SEARCH MASK
         LA    R2,CSW@CCAT
         B     BUFFCSI       PROCESS NEW CATALOG
         POP   USING
         SPACE 2
*---------------------------------------------------------------------*
*                                                                     *
*  EXTRACT DATA FROM RETURN BUFFER                                    *
*   NOTE THAT CSIFD FIELDS MATCH IN ORDER THOSE ON THE IGGCSI00 CALL  *
*                                                                     *
*---------------------------------------------------------------------*
DATACSI  BAS   R14,GETTYPE   GET ENTRY TYPE
*  NOTE REGISTER USE: R2 - R5 (R6, R7 AVAILABLE IF RETURN TO LOOPCSI)
         LA    R4,CSILENF1   R4-LENGTHS R5-DATA
         A     R5,CSWFDLEN   R5-POINT TO FIRST DATA ITEM
         LA    R5,CSILENF1-CSIRWENT-2(,R5)
         DROP  R5
         CSIFD CSWDACLS,FILL=C' '  PARM 1 - DATA CLASS
         CSIFD CSWMGCLS,FILL=C' '  PARM 2 - MANAGEMENT CLASS
         CSIFD CSWSTCLS,FILL=C' '  PARM 3 - STORAGE CLASS
         CSIFD CSPRDTYS,FILL=0     PARM 4 - DEVICE TYPE
         SR    R3,R3
         ICM   R3,3,0(R4)    LOAD VOLUME SIZE
         BNM   *+6
         SR    R3,R3         MISSING FIELD IS NONE
         SR    R2,R2
         D     R2,=F'6'      CONVERT FROM LENGTH TO COUNT
         STC   R3,CSP#VOL    SAVE QUOTIENT
         CSIFD CSPRVOLS,FILL=C' '  PARM 5 - VOLUME SERIAL(S)
         SPACE 1                                                GP03044
*---------------------------------------------------------------------*
*  CHECK RETURNED SERIALS TO ELIMINATE DUPLICATES                     *
*---------------------------------------------------------------------*
DUPCHKER LA    R15,CSPRVOLS  POINT TO RETURN LIST               GP03044
         LA    R0,L'CSPRVOL  LENGTH OF ONE ENTRY                GP03044
         SR    R1,R1                                            GP03044
         IC    R1,CSP#VOL    GET NUMBER OF ENTRIES              GP03044
         CH    R1,=H'2'      ANYTHING TO CHECK ?                GP03044
         BL    DUPCHKNO      NO                                 GP03044
         BCTR  R1,0                                             GP03044
         BCTR  R1,0                                             GP03044
         MH    R1,=Y(L'CSPRVOL)  CONVERT TO LENGTH              GP03044
         AR    R1,R15        LAST ONE TO CHECK                  GP03044
DUPCHKOL LR    R2,R15        COMPARE NEXT TO PRIOR              GP03044
         CLC   0(L'CSPRVOL,R15),BLANKS  END OF LIST ?           GP03044
         BE    DUPCHKNO      DONE                               GP03044
DUPCHKIL CLC   CSPRVOL-CSPRVOL(L'CSPRVOL,R15),L'CSPRVOL(R2) DUPLICATE ?
         BNE   DUPCHKIB      NO; BUMP                           GP03044
         CLC   L'CSPRVOL(L'CSPRVOL,R15),BLANKS  END OF LIST ?   GP03044
         BE    DUPCHKNO      DONE                               GP03044
         AR    R1,R0         UP END ADDRESS BY ONE              GP03044
DUPCHKMV MVC   CSPRVOL-CSPRVOL(L'CSPRVOL,R2),L'CSPRVOL(R2) COVER DUPE
         BXLE  R2,R0,DUPCHKMV   SLIDE OVER                      GP03044
         MVC   L'CSPRVOL(L'CSPRVOL,R2),BLANKS  NEW END OF LIST  GP03044
         IC    R0,CSP#VOL    GET OLD NUMBER OF VOLUMES          GP03044
         BCTR  R0,0          REDUCE BY ONE                      GP03044
         STC   R0,CSP#VOL    UPDATE IT                          GP03044
         B     DUPCHKER      AND TRY AGAIN                      GP03044
DUPCHKIB BXLE  R2,R0,DUPCHKIL  TRY ANOTHER                      GP03044
DUPCHKOB BXLE  R15,R0,DUPCHKOL  TRY MORE                        GP03044
         SPACE 1                                                GP03044
DUPCHKNO CSIFD CSPASTYP,FILL=C' '  PARM 6 - ASSOCIATED TYPES    GP03044
         CSIFD CSPASSOC,FILL=C' '  PARM 7 - ASSOCIATED NAMES
         TR    CSPASSOC,TRNULLS  CHANGE MAST.CAT.ASSOC 00->BLANK
         L     R15,CSW@FORM  GET FORMATTING ROUTINE ADDRESS
         BR    R15           GO TO FORMAT AND DISPLAY ENTRY
         SPACE 2
*---------------------------------------------------------------------*
*                                                                     *
*  WE HAVE A CATALOG NAME.                                            *
*  BECAUSE IGGCSI00 DOESN'T DO IT FOR US, WE NEED TO STACK            *
*    THE NAMES OF THE CATALOGS, AND THEN RECURSE THROUGH THEM         *
*    TO FIND MASKED ENTRIES.                                          *
*  EXCEPTIONS:  DO NOT STACK INVALID NAME (ZEROES) OF MASTER          *
*               DO NOT STACK IF SAME AS CURRENT CATALOG               *
*               DO NOT STACK FOR SINGLE CATALOG REQUEST               *
*                                                                     *
*---------------------------------------------------------------------*
UCATFORM CLC   CSPRDSN,CSW@CCAT  SAME AS CURRENT CATALOG NAME ?
         BE    UCATS         IGNORE IF SAME
         CLC   CSPRDSN,CSWMSCAT  SAME AS MASTER CATALOG ?
         BE    UCATS
         CLI   CSPRDSN,X'00'  SAME AS MASTER CATALOG ?
         BE    UCATS
***********************************************************************
**  IGNORE - EITHER IT'S ON THE CAX CHAIN, OR IT CAN'T BE OPENED     **
***********************************************************************
         B     UCATS ********* AVOID RC=4/RSN C1C86C04 ****************
         LA    R1,CSW@CAHD   ELSE SEE IF ALREADY ON CHAIN
         USING CSCDSECT,R1
UCATLKOL ICM   R1,15,CSCLINK    ANY MORE ?
         BZ    UCATADD
         CLC   CSCNAME,CSPRDSN  QUEUED BEFORE ?
         BNE   UCATLKOL      NO; TRY AGAIN
         B     UCATS         YES; DON'T ADD TO CHAIN
UCATADD  STORAGE OBTAIN,LENGTH=CSCSIZE
         ST    R15,CSCLINK   CLEAR LINK POINTER
         OC    CSW@CAHD,CSW@CAHD
         BNZ   UCATC         CHAIN HEAD SET
         ST    R1,CSW@CAHD
UCATC    ICM   R14,15,CSW@CATL  IS TAIL SET?
         BNZ   UCATD         YES; ADD TO END
         LA    R14,CSW@CATL  SET NEW END
UCATD    ST    R1,0(,R14)    CHAIN TO OLD ENTRY
         ST    R1,CSW@CATL   SET END POINTER
         MVC   CSCNAME,CSPRDSN
         SR    R0,R0
         ST    R0,CSCCAX     NO CAX
         ST    R0,CSCFLGS    AND CLEAR FLAGS
UCATS    DS    0H            CONTINUE WITH FMTDS
         DROP  R1
         SPACE 2
*---------------------------------------------------------------------*
*                                                                     *
*  COMPARE DATA SET NAME TO MASK. SKIP ENTRY UNLESS MATCHED.          *
*  WHEN ENTRY CONTAINS VOLUME SERIALS, EXTRACT AND COMPARE TO REQUEST *
*  SKIP UNLESS MATCHED.                                               *
*  ALSO SKIP ENTRIES WITH NO SERIALS WHEN VOLMASK IS REQUESTED        *
*                                                                     *
*---------------------------------------------------------------------*
         SPACE 1
FMTAIX   DS    0H            PROVISIONAL FORMATTING
FMTALIAS DS    0H            PROVISIONAL FORMATTING
FMTCAT   DS    0H            PROVISIONAL FORMATTING
FMTCLUST DS    0H            PROVISIONAL FORMATTING
FMTGDG   DS    0H            PROVISIONAL FORMATTING
FMTINX   DS    0H            PROVISIONAL FORMATTING
FMTLIB   DS    0H            PROVISIONAL FORMATTING
FMTPATH  DS    0H            PROVISIONAL FORMATTING
FMTTAP   DS    0H            PROVISIONAL FORMATTING
FMTDS    TM    CSP$FLGS,CSP$FGCP  TEST FOR MASK MATCH ?
         BZ    DSMATCHD      NO; EXTRACT FIELD DATA
         L     R15,CSP@SCMP  LOAD SUBCOMP ADDRESS               GP03046
         SUBCALL (R15),(PATCMDSN,CSPRDSN,CSPMASK),MF=(E,CSWCMPRM)
         BXLE  R15,R15,DSMATCHD  WE HAVE A WINNER
         LA    R2,CSPASSOC   POINT TO ASSOCIATED NAMES
         LA    R3,L'CSPASSOC/L'CSPASSO1  NUMBER OF ITEMS
FMTDSALP CLI   0(R2),C' '    ENTRY PRESENT ?
         BNH   FMTDSBMP      NO; TRY ANOTHER
         L     R15,CSP@SCMP  LOAD SUBCOMP ADDRESS               GP03046
         SUBCALL (R15),(PATCMDSN,(R2),CSPMASK),MF=(E,CSWCMPRM)  GP03046
         BXLE  R15,R15,DSMATCHD  WE HAVE A WINNER
FMTDSBMP LA    R2,L'CSPASSO1(,R2)  TRY NEXT
         BCT   R3,FMTDSALP   UNTIL DONE
         B     LOOPCSI       NO MATCH - TRY ANOTHER
         SPACE 1
DSMATCHD CLI   CSWCIVMS,C' ' ANY VOLUME REQUEST?
         BNH   ACCEPTVL      NO; USE IT
         LA    R4,CSPRVOLS   POINT TO FIRST SERIAL              GP03115
         SR    R5,R5
         ICM   R5,1,CSP#VOL
         BNP   LOOPCSI       IGNORE ENTRY IF NONE
VOLCOMP  MVC   CSWCIVTS,0(R4) MOVE TO CL8 FIELD FOR COMPARE
         L     R15,CSP@SCMP  LOAD SUBCOMP ADDRESS               GP03046
         SUBCALL (R15),(PATCMVOL,CSWCIVTS,CSWCIVMS),MF=(E,CSWCMPRM)
         BXLE  R15,R15,ACCEPTVL  MATCH
         LA    R4,6(,R4)
         BCT   R5,VOLCOMP
         B     LOOPCSI       NO MATCH
ACCEPTVL OI    CSWPFLGS,CSWPFONE SHOW AT LEAST ONE MATCH FOUND
         B     CALLCSIX      SUBROUTINE EXIT
         EJECT ,
*---------------------------------------------------------------------*
*                                                                     *
*  GETVAL - SEQUENTIAL EXTRACTION OF CSI DATA FIELDS                  *
*           INVOKED MY CSIFD MACRO                                    *
*                                                                     *
*  R4  - POINTER TO HALFWORD FIELD LENGTH(S)                          *
*  R5  - POINTER TO DATA                                              *
*  R2  - POINTER TO OUTPUT FIELD                                      *
*  R3  - MAX OUTPUT LENGTH                                            *
*  CSWDB(1) - FILL CHARACTER FOR MVC; DEFAULT X'00'                   *
*---------------------------------------------------------------------*
*  NOTE THAT UPDATED R4 AND R5 VALUES ARE RETURNED TO CALLER          *
*---------------------------------------------------------------------*
GETVAL   STM   R6,R3,CSWSAV2 SAVE MOST REGISTERS
         SR    R15,R15
         ICM   R15,3,CSILENF1-CSILENF1(R4)  GET LENGTH
         LA    R4,2(,R4)     ADVANCE FIELD LENGTH POINTER
         BNP   GETVALEX      NO DATA AVAILABLE
         LR    R14,R5        SAVE FIELD START
         AR    R5,R15        NEXT PARAMETER'S DATA
         CH    R15,=H'2'     VALID ENTRY ?
         BNH   GETVALEX      NO; SKIP IT TOO
         CLI   0(R14),X'FF'  NON-EXISTING FIXED LENGTH DATA ?
         BNE   GETVALEF      NO
         LR    R6,R14        START
         LR    R7,R15        LENGTH
         L     R9,=X'FF000000'  COMPARE TO X'FF'
         CLCL  R6,R8         REALLY MISSING ?
         BNE   GETVALEF      NO
         SR    R15,R15       NO DATA AVAILABLE
GETVALEF ICM   R15,8,CSWDB   GET FILL CHARACTER
         MVCL  R2,R14        ELSE COPY IT TO USER
GETVALEX LM    R6,R3,CSWSAV2   RETURN NEW R4 & R5
         BR    R14           RETURN TO CALLER
         SPACE 2
*---------------------------------------------------------------------*
*                                                                     *
*  GETTYPE - GET INFORMATION BY ENTRY TYPE FROM CETY TABLE            *
*                                                                     *
*---------------------------------------------------------------------*
         PUSH  USING
         AIF   (&BAKR).SUBBAKR
GETTYPE  STM   R0,R15,SUBSAVE1
         AGO   .SUBACOM
.SUBBAKR ANOP  ,
GETTYPE  BAKR  R14,0         SAVE REGISTERS
.SUBACOM ST    R5,CSP@RAW    RETURN RAW ADDRESS TO USER
         MVC   CSP#RAW,CSW@CBXL+4  AND LENGTH
         LA    R0,CSPRDSN    ADDRESS TO BLANK
         LA    R1,CSPSIZE-(CSPRDSN-CSPDSECT)   LENGTH TO BLANK
         L     R15,=X'40000000'  BLANK FILL
         MVCL  R0,R14        CLEAR STORAGE TO BLANKS
         XC    CSP#VOL(L'CSP#VOL+CSPRVOL-CSP#VOL),CSP#VOL
         USING CSIRWENT,R5   UPDATE MAPPING
         MVC   CSPRDSN,CSIENAME  PROPAGATE NAME
         IC    R0,CSIETYPE   GET ENTRY TYPE
         LM    R1,R3,=A(PROLOGIC,PROLOG2-PROLOGIC,PROLOGND)
         SR    R3,R2         LAST ENTRY IS DEFAULT
GETTYPEL CLM   R0,1,CETOETYP(R1)    MATCH ?
         BE    GETTYPEM      YES
         BXLE  R1,R2,GETTYPEL  CONTINUE
* NO MATCH - R1 IS SET FOR DEFAULT, ERROR ENTRY
GETTYPEM MVC   CSPRTYP(L'CSPRTYP+L'CSPRTYN),CETOETYP(R1) COPY NAME
         MVC   CSWDB(2),=X'41F0' 'LA R15,XXX'
         MVC   CSWDB+2(2),CETOFORM(R1) GET FORMATTING ROUTINE
         EX    0,CSWDB       LOAD ADDRESS
         ST    R15,CSW@FORM  SAVE IT
         MVC   CSWDB+2(2),CETOFILD(R1) GET FORMATTING ROUTINE
         EX    0,CSWDB       LOAD ADDRESS
         ST    R15,CSW@FLD   SAVE IT
         AIF   (&BAKR).SUBPR
GETTYPEX LM    R1,R14,SUBSAVE1+4
         BR    R14
         AGO   .SUBPCOM
.SUBPR   ANOP  ,
GETTYPEX PR    ,             RESTORE AND RETURN
.SUBPCOM POP   USING
         SPACE 2
         LTORG ,
         SPACE 1
BLANKS   DC    CL44' '       LONG ENOUGH FOR DS NAME
HEXTAB   DC    C'0123456789ABCDEF'     1/2
HEXTRTAB EQU   HEXTAB-C'0'             2/2
PATCMMSK DC    C'MSK'        EXAMINE MASK
PATCMDSN DC    C'DSN'        COMPARE DSN TO MASK
PATCMVOL DC    C'VOL'        COMPARE VOLSER MASK
         SPACE 2
PROLOGIC CETY  A,'NONVSAM',FMTDS
PROLOG2  CETY  B,'GDG    ',FMTGDG
         CETY  C,'CLUSTER',FMTCLUST
         CETY  D,'DATA   ',FMTDS
         CETY  G,'ALTINDX',FMTAIX
         CETY  H,'GDS    ',FMTDS
         CETY  I,'INDEX  ',FMTINX
         CETY  L,'TAPELIB',FMTLIB
         CETY  M,'TAPEVOL',FMTTAP
         CETY  R,'PATH   ',FMTPATH
         CETY  U,'UCATLNK',UCATFORM
         CETY  X,'ALIAS  ',FMTALIAS
         CETY  0,'CATALOG',FMTCAT
PROLOGND CETY  0,'UNKNOWN',FMTDS
         SPACE 1
MSKTAB   DC    256AL1(0)     TABLE OF MASK & END CHARACTER IN DSN
         ORG   MSKTAB+C'.'   INDEX POINT
         DC    C'.'
         ORG   MSKTAB+C'?'   CHARACTER MATCH
         DC    C'?'
         ORG   MSKTAB+C'%'   CHARACTER MATCH
         DC    C'%'
         ORG   MSKTAB+C'*'   LEVEL OR NAME MATCH
         DC    C'*'
         ORG   ,
         SPACE 1
         LTORG ,
         SPACE 1
PATFIELD DC    Y(PATFIENM)   NUMBER OF FIELD NAMES
PATFIEL1 DC    CL8'DATACLAS'
         DC    CL8'STORCLAS'
         DC    CL8'MGMTCLAS'
         DC    CL8'DEVTYP  '
         DC    CL8'VOLSER  '
         DC    CL8'TYPE    ' TYPE(S) OF ASSOCIATED ENTRIES
         DC    CL8'NAME    ' NAME(S) OF ASSOCIATED ENTRIES
PATFIENM EQU   (*-PATFIEL1)/8
         SPACE 1
TRNULLS  DC    256AL1(*-TRNULLS)  NO TRANSLATE EXCEPT:          GP03044
         TRENT TRNULLS,C' ',0  HEX ZERO TO BLANK (MAST.CAT.ASSOC)
         ORG   ,                                                GP03044
         SPACE 1                                                GP03044
         PRINT GEN           I GOTTA SEE THIS
         CATSPARM ,          MAP CALLERS PARAMETER AREA
         SPACE 1
         CATSWORK ,          MAP OUR WORK AREA
         SPACE 1
         CATSCAT  ,          MAP OUR CHAIN OF PROCESSED CATALOGS
         SPACE 1
         PRINT GEN
         CSIRWORK ,          MAPPING OF CSI RETURN WORK AREA
         SPACE 1
*        NO USABLE IBM AMCBS MACRO
*
AMCBS    DSECT ,
CBSID    DS    CL2           ID
CBSSIZ   DS    AL2           LENGTH
CBSMCSTA DS    A             CCHH OF MASTER CATLG
CBSACB   DS    A             MASTER CAT ACB
CBSCBP   DS    A             C B MANIP
CBSCMP   DS    0A            CAT RTNE
CBSMCUCB DS    A             MASTER CAT UCB
CBSCAXCN DS    A             CAXWA CHAIN
CBSCRACA DS    A             CRA CAXWA CHAIN
CBSCRTCB DS    A             CRA TASK TCB
CBSVSRT  DS    A
CBSVUSE  DS    A             VSRT USE COUNT
CBSVPTR  DS    A             VSRT
CBSFLAGS DS    X             FLAGS
CBSMICF  EQU   X'80'           MAST IS ICF CATALOG
         DS    XL3           SPARE
CBSVVDSA DS    A             VVDS MANAGER
CBSDEVNT DS    A             DEVICE NAME TABLE
CBSVSICN DS    A             IDAVSI CHAIN
CBSFLG1  DS    X             AMCBS FLAGS
CBSCUVSI EQU   X'80'           VSI CHAIN CLEAN-UP REQUIRED
         DS    XL3           SPARE
         SPACE 2
         PRINT GEN                                               90359
         IGGCAXWA ,          EXPAND LOCAL CAX MAPPING            90359
         AGO   .COMCAT                                          GP09157
.USEVSAM TITLE 'S U B C A T  ***  VSAM CATALOG RETRIEVAL SUBROUTINE'
         SPACE 1             NO ICF CATALOG SERVICES USED       GP09157
         LA    R1,0(,R1)     CLEAN HIGH BIT                     GP03261
         LTR   R8,R1         GET CALLER'S PARM POINTER          GP03261
         BZ    MISSPARM                                         GP03261
         USING CSPDSECT,R8   DECLARE IT
         ICM   R10,15,CSP@WORK  ANYTHING IN WORK ANCHOR ?
         BNZ   REQCHECK      YES; ASSUME INITIALIZED
         SPACE 1
***********************************************************************
**                                                                   **
**  INITIALIZE:  OBTAIN WORK AREA AND SAVE ADDRESS IN USER'S PARM    **
**    USER MUST MAINTAIN IT FOR LATER MODULE AND STORAGE CLEANUP.    **
**    INITIALIZE THE AREA.                                           **
**                                                                   **
***********************************************************************
REQFIRST ICM   R0,1,CSP$REQ    GET REQUEST TYPE                 GP05277
         BZ    GOODEXIT      CLOSE; NO PRIOR OPEN               GP05277
         SR    R2,R2
         IC    R2,CSP#SP     LOAD SUBPOOL (GENERALLY = 0)
         LA    R3,MVSWKLEN   SET STORAGE SIZE                   GP09182
         STORAGE OBTAIN,SP=(R2),LENGTH=(R3),LOC=BELOW  GET IT OR ABEND
         STCM  R3,7,CSP#SPL  SAVE SIZE OBTAINED
         ST    R1,CSP@WORK   AND SAVE ADDRESS
         SR    R15,R15       JUST IN CASE
         LR    R2,R1         COPY ADDRESS
         MVCL  R2,R14        CLEAR IT TO ZEROES
         LR    R10,R1        MAKE IT OFFICIAL
         USING CSWDSECT,R10  DECLARE IT
         ICM   R0,15,CSP@SCMP  DID CALLER LOAD SUBCOMP ?        GP03046
         BNZ   REQFIRCM      YES; USE IT                        GP03046
         LOAD  EPLOC==CL8'SUBCOMP'  LOAD COMPARE ROUTINE        GP03046
         ST    R0,CSP@SCMP   REMEMBER THE ADDRESS               GP03046
         OI    CSWPFLGS,CSWPFCMP  REMEMBER WE LOADED SUBCOMP    GP03046
REQFIRCM L     R3,=A(32*1024)  SET SIZE
         LR    R4,R3         SAVE OVER MVCL
         BCTR  R4,0          SET MAX SUPPORTED
         STORAGE OBTAIN,LENGTH=(R3)         GET SOME STORAGE
         LR    R2,R1
         SR    R15,R15       CLEAR LENGTH AND FILL BYTE
         MVCL  R2,R14        CLEAR IT
         STH   R4,0(,R1)     INITIALIZE WORK AREA LENGTH
         XC    CSW@CRES,CSW@CRES   REUSE FOR CTGPL
         MVI   CTGOPTN1,CTGNAME+CTGGENLD    GENERIC ENTRY
         MVI   CTGOPTN2,CTGRCATN            RETURN CATALOG NAME
         MVI   CTGOPTN3,CTGSUPLT+CTGAM0     SUPERLOCATE
         MVI   CTGOPTN4,0
         LA    R15,CSWCIMSK  GET MASK ADDRESS
         ST    R15,CTGENT    INTO REQUEST LIST
         ST    R1,CTGWKA     STASH WORK AREA ADDRESS
         LA    R0,CSWCM@WK   GET POINTER TO SUBCOMP ROOT
         ST    R0,CSWCMPRM+12  INITIALIZE IT
         SPACE 1
***********************************************************************
**                                                                   **
**  CALLER PASSES A REQUEST NUMBER (IN R0) AND A DATA AREA           **
**    VALID REQUESTS ARE:                                            **
**                                                                   **
**    0 - TERMINATE CATALOG PROCESSING                               **
**    1 - GET A SINGLE ENTRY                                         **
**    2 - START PROCESSING A GENERIC REQUEST                         **
**    3 - CONTINUE WITH NEXT GENERIC                                 **
**                                                                   **
***********************************************************************
REQCHECK SR    R2,R2
         IC    R0,CSP$REQ    GET REQUEST TYPE
         BIX   ERR=BADENTRY,PFX=REQ,   BRANCH BY REQUEST TYPE          *
               LOC=(CLOSE,LOOK,INIT,LOOP)
         SPACE 1
***********************************************************************
**                                                                   **
**  ALL FUNCTIONS RETURN HERE                                        **
**                                                                   **
**    GOODEXIT - SETS RC=0                                           **
**    BADRET4  - SETS RC=4                                           **
**    BADRET8  - SETS RC=8                                           **
**    BADENTRY - SETS RC=12                                          **
**                                                                   **
***********************************************************************
BADENTRY LA    R15,12        SET DISASTER
         B     COMMEXIT      GO TO COMMON EXIT
         SPACE 1
BADRET8  LA    R15,8         SET ERROR
         B     COMMEXIT      GO TO COMMON EXIT
         SPACE 1
BADRET4  LA    R15,4         SET WARNING
         B     COMMEXIT      GO TO COMMON EXIT
         SPACE 1
MISSPARM LA    R15,16        PARM MISSING - NO WORK AREA        GP03261
         B     PGMEXIT       TAKE IMMEDIATE ERROR RETURN        GP03261
         SPACE 1                                                GP03261
GOODEXIT SR    R15,R15       SET GOOD COMPLETION
         SPACE 1
COMMEXIT ST    R15,CSPRCOD   ALSO PASS RETURN CODE IN PARM LIST
PGMEXIT  DS    0H                                               GP03261
         LR    R9,R15
         PGMEXIT RC=(R9)
         SPACE 2
***********************************************************************
**                                                                   **
**  CLOSE ENTRY - DELETE ALL LOADED MODULES                          **
**                FREE GOTTEN STORAGE                                **
**                RESET CALLER'S POINTERS                            **
**                                                                   **
***********************************************************************
         PUSH  USING
REQCLOSE ICM   R1,15,CTGWKA  STORAGE GOTTEN ?
         BZ    CLONDS
         SR    R2,R2
         ICM   R2,3,0(R1)    AND GET LENGTH
         BZ    CLONDS        OOPS?
         STORAGE RELEASE,ADDR=(1),LENGTH=(R2)
         XC    CTGWKA,CTGWKA  RESET
CLONDS   ICM   R0,15,CSWCM@WK  COMPARE WORK AREA?
         BZ    CLONCMP
         L     R15,CSP@SCMP  LOAD SUBCOMP ADDRESS               GP03046
         SUBCALL (R15),('END'),MF=(E,CSWCMPRM) FREE THE WORK AREA
         XC    CSWCM@WK,CSWCM@WK  JUST IN CASE
CLONCMP  SR    R2,R2
         SR    R3,R3
         IC    R2,CSP#SP     GET WORK AREA SUBPOOL
         ICM   R3,7,CSP#SPL    AND WORK AREA LENGTH
         STORAGE RELEASE,SP=(R2),LENGTH=(R3),ADDR=(R10)
         SR    R10,R10       RESET
         ST    R10,CSP@WORK  CLEAR IN USER'S PARM
         TM    CSWPFLGS,CSWPFCMP  DID WE LOAD SUBCOMP ?         GP03046
         BZ    GOODEXIT      NO; RETURN TO CALLER               GP03046
         DELETE EPLOC==CL8'SUBCOMP'  FREE IT                    GP03046
         ST    R10,CSP@SCMP  AND CLEAR IT                       GP03046
         B     GOODEXIT      RETURN TO CALLER                   GP03046
         POP   USING
         SPACE 2
***********************************************************************
**                                                                   **
**  LOOK ENTRY - GET A SINGLE NAME FROM THE CATALOG                  **
**               ADDRESS OF THE NAME IS PASSED IN CSP@REQ            **
**                                                                   **
***********************************************************************
         PUSH  USING
REQLOOK  L     R2,CSP@REQ   GET REQUEST POINTER
         N     R2,=X'00FFFFFF'  KILL HIGH BYTE AND SET CC       GP09157
         BZ    BADRET8       OOPS
         OI    CSWPFLGS,CSWPFLUK  SHOW LOOK INVOKED (FOR LOOP MIDDLE)
         MVI   CSW@CCAT,C' '
         MVC   CSW@CCAT+1(L'CSW@CCAT-1),CSW@CCAT  INITIALIZE
         TM    CSP$FLGS,CSP$FGUC  USER PROVIDED CATALOG NAME ?
         BZ    REQLOOK2      NO                                 GP03058
         MVC   CSW@CCAT,CSPRCAT   YES; USE IT
REQLOOK2 MVC   CSWCIVMS(L'CSPMVOL),CSPMVOL  VOL-SER MASK        GP03058
         SR    R3,R3         CLEAR FOR IC
         ST    R3,CSW@CBXL   FORCE CATALOG READ
         ICM   R3,3,CSP#LEN  GET NAME LENGTH
         BP    *+8             OK
         LA    R3,L'CSW@CFLT  ELSE SET MAX LENGTH
         LA    R0,CSW@CFLT   DESTINATIION
         LA    R1,L'CSW@CFLT  GET LENGTH
         ICM   R3,8,BLANKS   BLANK FILL
         MVCL  R0,R2         MOVE AND FILL
         MVC   CSWCIMSK,CSW@CFLT   DOUBLES AS MASK
         MVI   CTGOPTN1,CTGNAME  NON-GENERIC ENTRY
REQLOOKN BAS   R14,CALLCSI   GO TO COMMON CALL ROUTINE
         MVC   CSPRCAT,CSWCUCAT  RETURN CATALOG
         LTR   R15,R15       GOOD ?
         BNZ   COMMEXIT      NO; RETURN
         L     R14,CSP@REQ   GET USER'S REQUEST
         CLC   CSPRDSN,0(R14)  MATCHES REQUEST?
         BE    COMMEXIT      YES; GOOD
         CLI   CSPRTYP,C'0'  DID WE GET THE (FIRST) CATALOG NAME ?
         BE    REQLOOKN      YES; GET THE NEXT ENTRY
         B     COMMEXIT      ELSE RETURN WHAT WE HAVE
         POP   USING
         SPACE 2
***********************************************************************
**                                                                   **
**  INIT ENTRY - INTERPRET PASSED PARAMETER AS A MASK                **
**                                                                   **
**    AFTER FIRST RETURN, IF GOOD, USE CATCALL LOOP FOR ADDITIONAL   **
**    DATA                                                           **
**                                                                   **
***********************************************************************
REQINIT  L     R2,CSP@REQ   GET REQUEST POINTER
         N     R2,=X'00FFFFFF'  KILL HIGH BYTE AND SET CC       GP09157
         BZ    BADRET8       OOPS
         NI    CSWPFLGS,255-CSWPFLUK-CSWPFBUF-CSWPFONE-CSWPF1CT
         MVI   CTGOPTN1,CTGNAME+CTGGENLD    GENERIC ENTRY
         MVI   CSW@CCAT,C' '
         MVC   CSW@CCAT+1(L'CSW@CCAT-1),CSW@CCAT  INITIALIZE
         MVC   CSWCIVMS(L'CSPMVOL),CSPMVOL  VOL-SER MASK        GP03058
         TM    CSP$FLGS,CSP$FG1C  USER REQUESTED ONE CATALOG ?  GP03058
         BZ    *+8           NO                                 GP03058
         OI    CSWPFLGS,CSWPF1CT  PROPAGATE IT                  GP03058
         SR    R3,R3         CLEAR FOR IC
         ST    R3,CSW@CBXL   DON'T USE CURRENT BUFFER CONTENTS  GP03041
         ICM   R3,3,CSP#LEN  GET NAME LENGTH
         BP    *+8             OK
         LA    R3,L'CSPRDSN  ELSE SET MAX LENGTH
         LA    R0,CSPRDSN    MOVE TO TEMP AREA
         LA    R1,L'CSPRDSN  GET LENGTH
         ICM   R3,8,BLANKS   BLANK FILL
         MVCL  R0,R2         MOVE AND FILL
         BAS   R9,MAKEMASK
         CLI   CSWCIMSK+1,C'*'  GLOBAL SEARCH ?                 GP09157
         BE    BADRET8       NOT SUPPORTED PRIOR TO ICF         GP09157
REQINITC BAS   R14,CALLCSI   GO TO COMMON CALL ROUTINE
         MVC   CSPRCAT,CSWCUCAT  RETURN CATALOG
         LTR   R15,R15       GOOD ?
         BNZ   COMMEXIT      NO; RETURN
         OI    CSWPFLGS,CSWPFBUF  SHOW LOOP DATA AVAILABLE
         CLI   CSPRTYP,C'0'  DID WE GET THE (FIRST) CATALOG NAME ?
         BNE   COMMEXIT
         SPACE 2
***********************************************************************
**                                                                   **
**  LOOP ENTRY - EXTRACT PRIOR CATALOG AND RESTART DSN FROM          **
**    CALLER'S PARM, AND GET THE NEXT DSN, IF ANY                    **
**                                                                   **
***********************************************************************
REQLOOP  TM    CSWPFLGS,CSWPFBUF  BEEN HERE BEFORE ?
         BZ    REQINIT       NO; NEED TO INITIALIZE FIRST
         TM    CSP$FLGS,CSP$FGMC  MASK CHANGED ?
         BNZ   REQLOOPM
         MVI   CTGOPTN1,CTGNAME+CTGGENLD    GENERIC ENTRY
         TM    CSWPFLGS,CSWPFLUK  REQUEST INTERRUPTED ?
         BZ    REQLOOPC      NO; JUST CONTINUE
REQLOOPM NI    CSP$FLGS,255-CSP$FGMC  RESET CHANGE FLAG
         XC    CSW@CBXL,CSW@CBXL  REQUEST CAT CALL
         MVC   CSWCIVMS(L'CSPMVOL),CSPMVOL  VOL-SER MASK
REQLOOPC BAS   R14,CALLCSI   GO TO COMMON CALL ROUTINE
         MVC   CSPRCAT,CSWCUCAT  RETURN CATALOG
         B     COMMEXIT      ELSE RETURN WHAT WE HAVE
         SPACE 1
MAKEMASK TM    CSPMASK,255-C' '  NULL OPERAND?                  GP09157
         BNZ   MAKEDCLI      NO; USE IT                         GP09157
         MVC   CSPMASK(2),=C'**'  SET DEFAULT - EVERYTHING      GP09157
MAKEDCLI L     R15,CSP@SCMP  LOAD SUBCOMP ADDRESS               GP09157
         SUBCALL (R15),(PATCMMSK,CSW@CFLT,CSPRDSN,CSWCM@WK),           *
               MF=(E,CSWCMPRM)                                  GP09157
         MVC   CSWCIMSK+1(43),CSW@CFLT                          GP09157
         LOCBYTE CSWCIMSK+1,LEN=L'CSWCIMSK-1,W2=R2,WK=R1        GP09157
         SR    R2,R1         LENGTH                             GP09157
         CH    R2,=H'4'      AT LEAST A.** ?                    GP09157
         BL    MAKEMSTC      NO                                 GP09157
         LA    R1,CSWCIMSK-1(R2)  POINT AT LAST TWO             GP09157
         CLC   =C'**',0(R1)  ICF WILD CARD?                     GP09157
         BNE   MAKEMSTC                                         GP09157
         SH    R2,=H'2'      NO WILD CARDS FOR SUPERLOCATE      GP09157
MAKEMSTC STC   R2,CSWCIMSK   STASH LENGTH                       GP09157
         CH    R15,=H'8'     DID IT WORK ?
         BR    R9            RETURN TO CALLER
         SPACE 2
***********************************************************************
**                                                                   **
**  CALLCSI - SUBROUTINE TO INVOKE IGGCSI00 AND DO MINOR RECOVERY    **
**                                                                   **
***********************************************************************
CALLCSI  SR    R15,R15       PRESET FOR ZERO RETURN CODE
         STM   R0,R15,CSWSAV1   SAVE ALL REGISTERS
         LM    R5,R7,CSW@CBXL  TEST RESTART POINTER
         LM    R1,R2,GDSCURR      DOING GDG ENTRES ?            GP09182
         LTR   R2,R2         GDS?                               GP09182
         BP    LOOPGDS       RESTART GDS SCAN                   GP09182
         LTR   R5,R5         ANY RESTART ?
         BNZ   BXLECSI       YES; RESTART IN THIS BUFFER
         SPACE 2
*---------------------------------------------------------------------*
*                                                                     *
*  BUFFCSI:  PRIMARY CATALOG LOOKUP LOOP                              *
*    CALL SVC 26 WITH SUPER LOCATE                                    *
*                                                                     *
*---------------------------------------------------------------------*
BUFFCSI  XC    CSW@CBXL,CSW@CBXL   CLEAR POINTER
         XC    GDSCURR(8),GDSCURR  JUST IN CASE                 GP09182
         LOCATE CTGPL        CATALOG LOOKUP                     GP09157
         MVC   CSPREAS,CSW@CRTN   ALSO SAVE FOR CALLER          GP03044
         CH    R15,=H'4'     CHECK RETURN
         BL    PASSCSI       GOOD; USE DATA
         BH    ERRCSI        TOO BAD
         B     BADRET4
         SPACE 1
*---------------------------------------------------------------------*
*                                                                     *
*  CHECK RETURNED BUFFER - FIRST RESPONSE ALWAYS HAS THE CATALOG      *
*    RECORD FOR THE RETURNED DATA                                     *
*                                                                     *
*---------------------------------------------------------------------*
PASSCSI  L     R5,CTGWKA     GET THE RESULT AREA
         XC    CSWFDLEN,CSWFDLEN  INDICATE NO DATA              GP09157
         LH    R7,2(,R5)     LOAD USED LENGTH                   GP09157
         AR    R7,R5         LAST BYTE + 1                      GP09157
         BCTR  R7,0          LAST BYTE                          GP09157
         LA    R5,4(,R5)     NOW POINT TO ENTRIES               GP09157
*---------------------------------------------------------------------*
*                                                                     *
*  PROCESS NEXT ENTRY IN THE CURRENT BUFFER                           *
*                                                                     *
*---------------------------------------------------------------------*
         USING CSIRWENT,R5   UPDATE MAPPING
TESTCSI  LA    R6,CSICLENG   PROVISIONALLY SET FOR CATALOG ENTRY
         STM   R5,R7,CSW@CBXL   SAVE FOR PROCESSING
         CLI   CSICTYPE,CSIETYP0  CATALOG ENTRY ?
         BE    CATLCSI       YES; UP BY FIXED LENGTH
         CLI   CSICTYPE,CSIETYPB  GDG BASE RECORD?              GP09182
         BNE   FXERRCSI      NO; SEE WHAT IT IS                 GP09182
         SPACE 1
*---------------------------------------------------------------------*
*                                                                     *
*  GDG BASE - FIND AND LOOP THROUGH DATA SETS                         *
*                                                                     *
*---------------------------------------------------------------------*
GETGDG   MVC   DSNAME,CSIENAME         MOVE GDG BASE NAME       GP09182
         LA    R2,SHOWSPAC             RETURN AREA              GP09182
         MVC   0(4,R2),=AL2(256*8+12,0)   RESET WORK COUNTER    GP09182
         SHOWCAT NAME=DSNAME,AREA=(R2),MF=(B,SHOWCATL)          GP09182
         SHOWCAT NAME=DSNAME,AREA=(R2),MF=(E,SHOWCATL)          GP09182
         N     R15,=X'FFFFFFDF' ^X'20' UNEXPECTED RETURN CODE?  GP09182
         BNZ   QUITGDG                 YES; JUST SHOW BASE      GP09182
         LH    R2,SHOWSPAC+2           GET LENGTH USED          GP09182
         SH    R2,=H'12'               LESS FIXED OVERHEAD      GP09182
         BNP   QUITGDG                 NO DATA RETURNED         GP09182
         LA    R1,SHOWSPAC+12-8        PRESET ONE HIGHER        GP09182
         AH    R2,=H'8'                DITTO                    GP09182
         SRL   R2,3                    DIVIDE TO GET NUMBER     GP09182
         STM   R1,R2,GDSCURR           SET FOR NEXT INCREMENT   GP09182
         LA    R15,44-9                MAX BASE LENGTH          GP09182
         LA    R14,DSNAME+44-9         BASE NAME                GP09182
GDSGTLEN CLI   0(R14),C' '             TRAILING NON-BLANK YET?  GP09182
         BNE   GDSSTLEN                YES; STASH LENGTH        GP09182
         BCTR  R14,0                                            GP09182
         BCT   R15,GDSGTLEN            TRY AGAIN                GP09182
GDSSTLEN LA    R15,1(,R15)             TRUE BASE LENGTH         GP09182
         ST    R15,GDSBLEN             SAVE BASE LENGTH         GP09182
         SPACE 1
LOOPGDS  LM    R1,R2,GDSCURR           GET PRIOR GENERATION     GP09182
         LA    R1,8(,R1)               SKIP TO NEXT             GP09182
         SH    R2,=H'1'                ACCOUNT FOR IT           GP09182
         BNP   QUITGDG                 DONE HERE                GP09182
         STM   R1,R2,GDSCURR           SET NEXT GENERATION      GP09182
         CLI   0(R1),C'A'              NON-VSAM ENTRY ?         GP09182
         BNE   LOOPGDS                 NO; SKIP                 GP09182
         MVI   CSPRTYP,C'A'            PROPAGATE BACK           GP09182
         MVC   OBTNAME,DSNAME          GET BASE NAME            GP09182
         L     R15,GDSBLEN             GET BASE LENGTH          GP09182
         LA    R15,OBTNAME(R15)        POINT TO INSERTION       GP09182
         MVC   0(9,R15),=C'.GnnnnV00'  MOVE PATTERN             GP09182
         MVC   2(4,R15),4(R1)          COMPLETE NAME            GP09182
         MVC   CSPRDSN,OBTNAME         COMPLETE NAME            GP09182
         B     FMTDS                   GO TO PROCESS IT         GP09182
         SPACE 1
*---------------------------------------------------------------------*
*                                                                     *
*  SINGLE ENTRY RETURNED                                              *
*                                                                     *
*---------------------------------------------------------------------*
FXERRCSI BAS   R14,GETTYPE   DO MINIMAL PROCESSING
         L     R15,CSW@FORM
         BASR  R14,R15       GET SERIALS
         B     GOODEXIT      TAKE GOOD EXIT
         SPACE 1
*---------------------------------------------------------------------*
*                                                                     *
*  FOUND A CATALOG ENTRY - MATCH AGAINST REQUEST, AND DISPLAY.        *
*                                                                     *
*---------------------------------------------------------------------*
CATLCSI  MVC   CSWCUCAT,1(R5)    PRESERVE CURRENT CATALOG NAME
         BAS   R14,GETTYPE   DO MINIMAL PROCESSING
         TM    CSP$FLGS,CSP$FGCP  TEST FOR MASK MATCH ?
         BZ    GOODRET       NO; RETURN IT
         L     R15,CSP@SCMP  LOAD SUBCOMP ADDRESS               GP03046
         SUBCALL (R15),(PATCMDSN,CSPRDSN,CSPMASK),MF=(E,CSWCMPRM)
         BXH   R15,R15,BXLECSI  NO MATCH; TRY SOMETHING ELSE
*---------------------------------------------------------------------*
*                                                                     *
*  RETURN TO CALLER WITH CC IN R15, AND APPROPRIATE DATA IN CSP/CSW   *
*                                                                     *
*---------------------------------------------------------------------*
GOODRET  SR    R15,R15       SET GOOD RETURN
         ST    R15,CSWSAV1+4*R15  FOR RELOAD
         B     CALLCSIX
DODONE   LA    R15,4         SHOW LOGICAL END OF SEARCH
         B     COMCLEAR
ERRCSI   LA    R15,8         SET AN ERROR
COMCLEAR ST    R15,CSWSAV1+4*R15  FOR RELOAD
         XC    CSW@CBXL,CSW@CBXL  CLEAR RESTART JUST IN CASE
CALLCSIX LM    R0,R15,CSWSAV1   RESTORE ALL REGISTERS
         BR    R14           RETURN TO CALLER
         SPACE 1
*---------------------------------------------------------------------*
*                                                                     *
*  BUMP TO NEXT ENTRY IN BUFFER. IF NONE, CHECK FOR MORE DATA, OR     *
*    READ ANOTHER CATALOG.                                            *
*                                                                     *
*---------------------------------------------------------------------*
QUITGDG  XC    GDSCURR(8),GDSCURR      NO MORE GDS              GP09182
LOOPCSI  LM    R5,R7,CSW@CBXL   GET POINTERS
BXLECSI  LM    R1,R2,GDSCURR    DOING GDS ?                     GP09182
         LTR   R2,R2                                            GP09182
         BP    LOOPGDS       YES; RESTART GDS SCAN              GP09182
         BXLE  R5,R6,TESTCSI    LOOK FOR ANOTHER ENTRY
         B     DODONE        ALL DONE IN CATALOG
         SPACE 2
*---------------------------------------------------------------------*
*                                                                     *
*  COMPARE DATA SET NAME TO MASK. SKIP ENTRY UNLESS MATCHED.          *
*  WHEN ENTRY CONTAINS VOLUME SERIALS, EXTRACT AND COMPARE TO REQUEST *
*  SKIP UNLESS MATCHED.                                               *
*  ALSO SKIP ENTRIES WITH NO SERIALS WHEN VOLMASK IS REQUESTED        *
*                                                                     *
*---------------------------------------------------------------------*
         SPACE 1
FMTAIX   DS    0H            PROVISIONAL FORMATTING
FMTALIAS DS    0H            PROVISIONAL FORMATTING
FMTCAT   DS    0H            PROVISIONAL FORMATTING
FMTCLUST DS    0H            PROVISIONAL FORMATTING
FMTGDG   DS    0H            PROVISIONAL FORMATTING
FMTINX   DS    0H            PROVISIONAL FORMATTING
FMTLIB   DS    0H            PROVISIONAL FORMATTING
FMTPATH  DS    0H            PROVISIONAL FORMATTING
FMTTAP   DS    0H            PROVISIONAL FORMATTING
FMTDS    TM    CSP$FLGS,CSP$FGCP  TEST FOR MASK MATCH ?
*DEFER*  BZ    DSMATCHD      NO; EXTRACT FIELD DATA
*DEFER*  L     R15,CSP@SCMP  LOAD SUBCOMP ADDRESS               GP03046
*DEFER*  SUBCALL (R15),(PATCMDSN,CSPRDSN,CSPMASK),MF=(E,CSWCMPRM)
*DEFER*  BXH   R15,R15,LOOPCSI   NOT WANTED                     GP09182
DSMATCHD L     R14,LOCPATT   PATTERN LOCATE
         LA    R15,CSPRDSN   RETURNED DSN
         SR    R0,R0           NO CVOL SERIAL
         LA    R1,LOCWORK    CATALOG BLOCK RETURN
         STM   R14,R1,LOCPARM
         LOCATE LOCPARM      CATALOG LOOKUP
         BXH   R15,R15,ACCEPTVL   TOO BAD
         SR    R5,R5
         ICM   R5,3,LOCWORK       GET NUMBER RETURNED
         BNP   ACCEPTVL           NONE OR BAD - IGNORE
         CH    R5,=H'5'           MORE THAN FIT ?
         BNH   *+8
         LA    R5,5
         STC   R5,CSP#VOL         RETURN VOLUME COUNT
         LA    R1,CSPRVOLS        POINT TO OUTPUT SERIAL
         LA    R2,CSPRDTYS        POINT TO OUTPUT TYPE
         LA    R4,LOCWORK+2       FIRST ENTRY
DSVOLOOP MVC   0(4,R2),0(R4)      MOVE DEVICE TYPE
         MVC   0(6,R1),4(R4)      MOVE SERIAL
         LA    R4,12(,R4)         ADVANCE ONE ENTRY
         LA    R1,6(,R1)
         LA    R2,4(,R2)
         BCT   R5,DSVOLOOP
DSVOLSET CLI   CSWCIVMS,C' ' ANY VOLUME MASK REQUEST?
         BNH   ACCEPTVL      NO; USE IT
         LA    R4,CSPRVOLS   POINT TO FIRST SERIAL              GP03115
         SR    R5,R5
         ICM   R5,1,CSP#VOL
         BNP   LOOPCSI       IGNORE ENTRY IF NONE
VOLCOMP  MVC   CSWCIVTS,0(R4) MOVE TO CL8 FIELD FOR COMPARE
         L     R15,CSP@SCMP  LOAD SUBCOMP ADDRESS               GP03046
         SUBCALL (R15),(PATCMVOL,CSWCIVTS,CSWCIVMS),MF=(E,CSWCMPRM)
         BXLE  R15,R15,ACCEPTVL  MATCH
         LA    R4,6(,R4)
         BCT   R5,VOLCOMP
         B     LOOPCSI       NO MATCH
ACCEPTVL OI    CSWPFLGS,CSWPFONE SHOW AT LEAST ONE MATCH FOUND
         B     CALLCSIX      SUBROUTINE EXIT
         SPACE 2
*---------------------------------------------------------------------*
*                                                                     *
*  GETTYPE - GET INFORMATION BY ENTRY TYPE FROM CETY TABLE            *
*                                                                     *
*---------------------------------------------------------------------*
         PUSH  USING
GETTYPE  STM   R0,R15,SUBSAVE1
         ST    R5,CSP@RAW    RETURN RAW ADDRESS TO USER
         MVC   CSP#RAW,CSW@CBXL+4  AND LENGTH
         LA    R0,CSPRDSN    ADDRESS TO BLANK
         LA    R1,CSPSIZE-(CSPRDSN-CSPDSECT)   LENGTH TO BLANK
         L     R15,=X'40000000'  BLANK FILL
         MVCL  R0,R14        CLEAR STORAGE TO BLANKS
         XC    CSP#VOL(L'CSP#VOL+CSPRVOL-CSP#VOL),CSP#VOL
         USING CSIRWENT,R5   UPDATE MAPPING
         MVC   CSPRDSN,CSIENAME  PROPAGATE NAME
         IC    R0,CSIETYPE   GET ENTRY TYPE
         LM    R1,R3,=A(PROLOGIC,PROLOG2-PROLOGIC,PROLOGND)
         SR    R3,R2         LAST ENTRY IS DEFAULT
GETTYPEL CLM   R0,1,CETOETYP(R1)    MATCH ?
         BE    GETTYPEM      YES
         BXLE  R1,R2,GETTYPEL  CONTINUE
* NO MATCH - R1 IS SET FOR DEFAULT, ERROR ENTRY
GETTYPEM MVC   CSPRTYP(L'CSPRTYP+L'CSPRTYN),CETOETYP(R1) COPY NAME
         MVC   CSWDB(2),=X'41F0' 'LA R15,XXX'
         MVC   CSWDB+2(2),CETOFORM(R1) GET FORMATTING ROUTINE
         EX    0,CSWDB       LOAD ADDRESS
         ST    R15,CSW@FORM  SAVE IT
         MVC   CSWDB+2(2),CETOFILD(R1) GET FORMATTING ROUTINE
         EX    0,CSWDB       LOAD ADDRESS
         ST    R15,CSW@FLD   SAVE IT
GETTYPEX LM    R1,R14,SUBSAVE1+4
         BR    R14
         POP   USING
         SPACE 2                                                GP09157
         LTORG ,
         SPACE 1
LOCPATT  CAMLST NAME,1-1,,3-3     LOCATE DS IN CATALOG
         ORG   LOCPATT+4     SAVE SPACE
BLANKS   DC    CL44' '       LONG ENOUGH FOR DS NAME
HEXTAB   DC    C'0123456789ABCDEF'     1/2
HEXTRTAB EQU   HEXTAB-C'0'             2/2
PATCMMSK DC    C'MSK'        EXAMINE MASK
PATCMDSN DC    C'DSN'        COMPARE DSN TO MASK
PATCMVOL DC    C'VOL'        COMPARE VOLSER MASK
         SPACE 2
PROLOGIC CETY  A,'NONVSAM',FMTDS
PROLOG2  CETY  B,'GDG    ',FMTGDG
         CETY  C,'CLUSTER',FMTCLUST
         CETY  D,'DATA   ',FMTDS
         CETY  G,'ALTINDX',FMTAIX
         CETY  H,'GDS    ',FMTDS
         CETY  I,'INDEX  ',FMTINX
         CETY  L,'TAPELIB',FMTLIB
         CETY  M,'TAPEVOL',FMTTAP
         CETY  R,'PATH   ',FMTPATH
*MVS*    CETY  U,'UCATLNK',UCATFORM
         CETY  U,'UCATLNK',FMTCAT
         CETY  X,'ALIAS  ',FMTALIAS
         CETY  0,'CATALOG',FMTCAT
PROLOGND CETY  0,'UNKNOWN',FMTDS
         SPACE 1
MSKTAB   DC    256AL1(0)     TABLE OF MASK & END CHARACTER IN DSN
         ORG   MSKTAB+C'.'   INDEX POINT
         DC    C'.'
         ORG   MSKTAB+C'?'   CHARACTER MATCH
         DC    C'?'
         ORG   MSKTAB+C'%'   CHARACTER MATCH
         DC    C'%'
         ORG   MSKTAB+C'*'   LEVEL OR NAME MATCH
         DC    C'*'
         ORG   ,
         SPACE 1
         LTORG ,
         SPACE 1
TRNULLS  DC    256AL1(*-TRNULLS)  NO TRANSLATE EXCEPT:          GP03044
         TRENT TRNULLS,C' ',0  HEX ZERO TO BLANK (MAST.CAT.ASSOC)
         ORG   ,                                                GP03044
         SPACE 1                                                GP03044
         PRINT GEN           I GOTTA SEE THIS
         CATSPARM ,          MAP CALLER'S PARAMETER AREA
         SPACE 1
PATFIENM EQU   4             (JUST IN CASE)
         CATSWORK ,          MAP OUR WORK AREA
         SPACE 1
         ORG   CSW@CRES      REUSE FOR CTGPL
CTGPL    DS    0D
CTGOPTN1 DS    X              FIRST OPTION BYTE:
CTGBYPSS EQU   X'80' 1... ....  BYPASS CATALOG MANAGMENT SECURITY
CTGMAST  EQU   X'40' .1.. ....  CHECK THE MASTER PASSWORD
CTGCI    EQU   X'20' ..1. ....  CHECK CONTROL INTERVAL PASSWORD
CTGUPD   EQU   X'10' ...1 ....  CHECK UPDATE PASSWORD
CTGREAD  EQU   X'08' .... 1...  CHECK READ PASSWORD
CTGNAME  EQU   X'04' .... .1..  CTGENT CONTAINS DSNAME OR SERIAL ADDR
*                    .... .0..  CTGENT CONTAINS CONTROL INTERVAL NUMBE
CTGCNAME EQU   X'02' .... ..1.  CTGCAT CONTAINS CATALOG DSNAME ADDRESS
*                    .... ..0.  CTGCAT CONTAINS CATALOG ACB ADDRESS
CTGGENLD EQU   X'01' .... ...1  GENERIC LOCATE REQUEST
*
CTGOPTN2 DS    X              SECOND OPTION BYTE:
CTGEXT   EQU   X'80' 1... ....  EXTEND OPTION (WITH UPDATE)
CTGNSVS  EQU   X'80'            NO SCRATCH VSAM SPACE (WITH DELETE)
CTGERASE EQU   X'40' .1.. ....  ERASE OPTION (WITH DELETE)
CTGSMF   EQU   X'40'            WRITE SMF RECORD OPTION (WITH LSPACE)
CTGGALL  EQU   X'40'            SEARCH ALL CATALOGS (WITH LISTCAT)
CTGPURG  EQU   X'20' ..1. ....  PURGE OPTION (WITH DELETE)
CTGVMNT  EQU   X'20'            CALLER IS VSAM OPEN/CLOSE/EOV
CTGRCATN EQU   X'20'            RETURN CATALOG NAME (WITH LISTCAT)
CTGGTNXT EQU   X'10' ...1 ....  GET-NEXT OPTION (WITH LISTCAT)
CTGDISC  EQU   X'08' .... 1...  DISCONNECT OPTION (WITH EXPORT)
CTGOVRID EQU   X'04' .... .1..  ERASE OVERRIDE OPTION (WITH DELETE)
CTGSCR   EQU   X'02' .... ..1.  SCRATCH SPACE (WITH DELETE NON-VSAM)
*                    .... ...X  RESERVED
*
CTGOPTN3 DS    0X             THIRD OPTION BYTE
CTGFUNC  DS    X     XXX. ....  SPECIFIES THE CALLER-REQUESTED FUNCTION
CTGLOC   EQU   X'20' 001. ....    LOCATE
CTGLSP   EQU   X'40' 010. ....    LSPACE
CTGUPDAT EQU   X'60' 011. ....    UPDATE
CTGCMS   EQU   X'80' 100. ....    CATALOG MGMT SERV. FUNCT. SEE CTGOPNS
CTGSUPLT EQU   X'10' ...1 ....  SUPERLOCATE FUNCTION
CTGGDGL  EQU   X'08' .... 1...  GDG LOCATE - CALLER SUPPLIED BASE LEVEL
CTGSRH   EQU   X'04' .... .1..  SEARCH MASTER CATALOG ONLY
*                    .... .0..  SEARCH USER CATALOGS FIRST
*                    .... ..X.  RESERVED
CTGAM0   EQU   X'01' .... ...1  OS/VS2 CATALOG MANAGMENT REQUEST
*                    .... ...0  OS CATALOG REQUEST USER SUPPLIED CAMLST
*
CTGOPTN4 DS    X              FOURTH OPTION BYTE
CTGLBASE EQU   X'80' 1... ....  LOCATE BASE LEVEL (SUPERLOCATE GDG)
CTGDOCAT EQU   X'40' .0.. ....  DYNAMICALLY LOCATE AND OPEN CAT IF REQ.
*                    .1.. ....  DO NOT DYNAMICALLY OPEN NEEDED CATALOG
*                    ..XX XXXX  RESERVED
*
CTGENT   DS    0A             ADDRESS OF CATALOG RECORD IDENTIFIER
CTGFVT   DS    A              ADDRESS OF CALLER'S CTGFV
*
CTGCAT   DS    0A             ADDRESS OF CATALOG DSNAME OR ACB
CTGCVOL  DS    A              ADDRESS OF OS SYSCTLG DSNAME
*
CTGWKA   DS    A              ADDRESS OF CALLER'S WORK AREA
*
CTGDSORG DS    XL2            DATA SET ORGANIZATION IF SUPERLOCATE
*
CTGOPTNS DS    X              CATALOG MANAGMENT SERVICES REQUEST OPTION
CTGDEFIN EQU   X'08' 0000 1...  DEFINE
CTGALTER EQU   X'10' 0001 0...  ALTER
CTGDELET EQU   X'18' 0001 1...  DELETE
CTGLTCAT EQU   X'20' 0010 0...  LISTCAT
*                    .... .XXX    RESERVED
         DS    X              RESERVED
*
CTGTYPE  DS    X              TYPE OF CATALOG RECORD
CTGTALIN EQU   C'A'             NON-VSAM DATA SET
CTGTGBS  EQU   C'B'             GDG BASE RECORD
CTGTCL   EQU   C'C'             CLUSTER
CTGTDATA EQU   C'D'             DATA SET
CTGTINDX EQU   C'I'             INDEX
CTGTMCAT EQU   C'M'             MASTER CATALOG
CTGTPGS  EQU   C'P'             PAGE DATA SET
CTGTUCAT EQU   C'U'             USER CATALOG
CTGTVOL  EQU   C'V'             VOLUME
CTGTANM  EQU   C'X'             ALIAS NAME
*
CTGNOFLD DS    X              NUMBER OF ENTRIES IN CTGFIELD
*
CTGDDNM  DS    0A             ADDRESS OF DD STATEMENT (OPTIONAL)
CTGNEWNM DS    0A             ADDRESS OF NEW DSNAME (ALTER)
CTGFDBK  DS    XL2            FEEDBACK AREA (SUPERLOCATE)
CTGFBFLG DS    X              FLAGS (SUPERLOCATE)
CTGPAR   EQU   X'80' 1... ....  PARALLEL MOUNT
CTGKEEP  EQU   X'40' .1.. ....  FORCED KEEP
CTGGDGB  EQU   X'20' ..1. ....  GDG BASE LOCATED
CTGNGDSN EQU   X'10' ...1 ....  GDG DSNAME GENERATED (DSNAME.GXXXXVYY)
*                    .... XXXX  RESERVED
         DS    X              RESERVED
*
CTGJSCB  DS    0A             ADDRESS OF JSCB
CTGPSWD  DS    A              ADDRESS OF CALLER-SUPPLIED PASSWORD
*
CTGFIELD EQU   *              VARIABLE LENGTH FIELD
         ORG   ,
***********************************************************************
**  ADD-ONS FOR VSAM CATALOG                                         **
***********************************************************************
DSNAME   DS    CL44                    RETURNED GDG BASE NAME   GP09182
OBTNAME  DS    CL44                    NAME FOR FORMINFO OBTAIN GP09182
GDSBLEN  DS    F                       LENGTH OF BASE NAME      GP09182
GDSCURR  DS    A       1/2             GENERATION ENTRY IN LIST GP09182
GDSCNTR  DS    F       2/2             NO. OF SHOWCAT ENTRIES   GP09182
SHOWCATL SHOWCAT MF=L                                           GP09182
SHOWSPAC DS    0F'0',(256*12+12+8)X'0'                          GP09182
SHOWSPLN EQU   *-SHOWSPAC    LENGTH OF RETURN AREA              GP09182
MVSWKLEN EQU   *-CSWDSECT    SIZE WITH MVS STUFF                GP09182
         SPACE 1
CSIRWENT DSECT ,             LOCATE RETURN
CSIETYPE DS    0C            TYPE INDICATOR
CSICTYPE DS    C             TYPE INDICATOR
CSIETYP0 EQU   C'0'            CATALOG ENTRY
CSIETYPB EQU   C'B'            GDG BASE                         GP09182
CSIENAME DS    CL44          ENTRY NAME
CSICLENG EQU   *-CSIRWENT    BASIC MVS LENGTH
         SPACE 1
.COMCAT  PRINT NOGEN
         CVT   DSECT=YES
         IHAPSA ,
         IHACDE ,
         AIF   (&BAKR).DOREGS
SAVE     DSECT ,             SAVE AREA (NO WORK)
SUBSAVE1 DS    16A
         AIF   (&MVSXA).NOLOC4                                  GP09157
LOCPARM  DS    4A            LOCATE REQUEST                     GP09157
LOCWORK  DS    265X          RETURN AREA                        GP09157
.NOLOC4  ANOP  ,                                                GP09157
SAVEEND  EQU   *
         SPACE 1
MYJFCB   DSECT ,
         SPACE 1
         AGO   .COMREGS
.DOREGS  YREGS ,
.COMREGS END   ,                                                 82228
@@
//SYSGO DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(CYL,1),DISP=(MOD,PASS)
//SUBCOMP EXEC ASMFC,PARM='DECK,LIST,OBJECT'
//SYSLIB DD DSN=SYS1.MACLIB,DISP=SHR
//       DD DSN=SYS2.PVTMACS,DISP=SHR
//       DD DSN=SYS2.ESPMAC,DISP=SHR
//       DD DSN=SYS1.AMACLIB,DISP=SHR
//       DD DSN=SYS1.AMODGEN,DISP=SHR
//SYSIN DD DATA,DLM=@@
COMP     TITLE 'S U B C O M P  ***  MULTI-LEVEL COMPARE SUBROUTINE'
*                                                                     *
* MODULE NAME:  SUBCOMP                                               *
*                                                                     *
* THIS MODULE:                                                        *
*     DETERMINES IF A STRING MATCHES A MASK CRITERION                 *
*                                                                     *
*     THE CALLER PROVIDES:                                            *
*         R1 - > LIST                                                 *
*            +0  ADDRESS OF LENGTH/DATA OF FUNCTION                   *
*            +4  ADDRESS OF LENGTH/DATA OF STRING                     *
*            +8  ADDRESS OF LENGTH/DATA OF MASK                       *
*           EITHER THE ABOVE HAS THE VL BIT ON, OR:                   *
*           +12  ADDRESS OF A WORK AREA (CMPR WILL ALLOCATE IN SP 88) *
*                                                                     *
*     FUNCTION MAY BE:                                                *
*         END    SERVICE CALL TO RELEASE OBTAINED STORAGE             *
*         DSN    TREAT STRING AS DATA SET NAME                        *
*         ACT    TREAT STRING AND MASK AS COMMA SEPARATED FIELDS      *
*         POS    TREAT STRING AS CL44 STRING - NO INDEX LEVEL LOGIC   *
*         MEM    TREAT STRING AS CL8 MEMBER NAME                      *
*         VOL    TREAT STRING AS CL6 VOLUME SERIAL                    *
*         MSK    CONVERT MASK STRING TO ONE SUITABLE FOR IGGCSI00     *
*                  MASK VALUE (NOT USEFUL FOR OTHER MASKING)          *
*         AST    COMPLETE MASK BY APPENDING ** OR .**                 *
*                  USABLE FOR LATER MASK TESTING                      *
*         XXX    ANYTHING ELSE - TREAT AS SINGLE STRING OF LENGTH 44  *
*                (UNLESS LENGTHS ARE SUPPLIED; THEN UP TO MAXSTLEN)   *
*                                                                     *
* A GENERAL MATCH (**) WILL RETURN 0 FOR A NULL INPUT STRING; AND     *
* THAT IT WILL SUPPORT COMMA SEPARATED ACCOUNT FIELDS.                *
*                                                                     *
* UPON RETURN R15 WILL CONTAIN ONE OF THE FOLLOWING CODES:            *
*                                                                     *
* R15 =  0    STRING MATCHES MASK CRITERION                           *
*        4    STRING DOES NOT MATCH MASK                              *
*        8    ---RESERVED---                                          *
*       12    MASK IS INVALID                                         *
*                                                                     *
*---------------------------------------------------------------------*
*                      <<SPECIAL INSTRUCTIONS>>                       *
*                                                                     *
* INSTRUCTIONS FOR USE:                                               *
*                                                                     *
* IF CALLED FROM A REXX MODULE:                                       *
*  /*----------------------------------------------------------------*/
*  /* INVOKE SUBCOMP USING LINKMVS PASSING FUNCTION, STRING, MASK.   */
*  /* THE RESULT IS RETURNED IN REXX VARIABLE "RC".                  */
*  /*----------------------------------------------------------------*/
*                                                                     *
*  CMP_FUN = "DSN"    /* USE DATA SET METHOD                         */
*  CMP_STR = MY_DSNAME   /* SET NAME OF DATA SET                     */
*  CMP_MSK = "**.EXHLOG"    /* SAMPLE MASK                           */
*                                                                    */
*  ADDRESS LINKMVS "CMP_FUN CMP_STR CMP_MSK"  /* INVOKE              */
*                                                                    */
*  IF RC < 0  THEN SIGNAL BAD_CALL  /* MODULE NOT FOUND OR BAD PARM  */
*  IF RC = 0  THEN .....            /* STRING MATCHED                */
*  IF RC = 4  THEN .....            /* STRING DID NOT MATCH MASK     */
*  IF RC = 8  THEN .....            /* DSN FUNCTION - DSN NOT VALID  */
*  IF RC =12  THEN .....            /* MASK IS NOT VALID             */
*  IF RC >12  THEN SIGNAL CMPR_ERROR  /*  OOPS ?                     */
*                                                                    */
*  NOTE:  LINKPGM MAY BE USED INSTEAD, BUT ONLY IF ALL VARIABLES ARE  *
*  PRESET TO THEIR FULL LENGTH (3/44/44)                              *
*                                                                     *
*  /*----------------------------------------------------------------*/
*  /* END OF REXX EXAMPLE                                            */
*  /*----------------------------------------------------------------*/
*                                                                     *
*                                                                     *
* IF CALLED FROM AN ASSEMBLER MODULE:                                 *
*   *-----------------------------------------------------------------*
*   * CALL  SUBCOMP,(FUN,STRING,MASK) OPTIONAL VL AND MF=(E,CALLPL)   *
*   * FUN  IS A THREE BYTE STRING                                     *
*   * STRING  AND MASK ARE EACH 44 BYTES                              *
*   *                                                                 *
*   * OPTIONALLY ALL PARAMETERS MAY BE PRECEDED BY A HALFWORD LENGTH  *
*   * FIELD, IN WHICH CASE THE STRING AND MASK MAY BE UP TO MAXSTLEN  *
*   * IN LENGTH, BUT MAY NOT BE USED WITH FUNCTION DSN OR ACT         *
*   *                                                                 *
*   *-----------------------------------------------------------------*
*---------------------------------------------------------------------*
         EJECT
SUBCOMP  START 0
SUBCOMP  AMODE ANY
SUBCOMP  RMODE ANY
         B     BEGIN-*(,R15)
         DC    AL1(BEGIN-*),C'SUBCOMP &SYSDATE '
BEGIN    BSM   R14,0         SAVE USER'S AMODE                  GP03047
         STM   R14,R12,12(R13) SAVE ALMOST EVERYTHING           GP03047
         LR    R12,R15       GET BASE
         USING SUBCOMP,R12   DECLARE IT
         LR    R5,R1         SAVE ENTRY PARM POINTER
         TM    8(R5),X'80'   SHORT LIST WITH VL ?
         BNZ   TEMPWORK      YES; GET SAVE AREA
         L     R6,12(,R5)    LOAD ROOT POINTER
         LA    R6,0(,R6)                                        GP03116
         LTR   R6,R6                                            GP03116
         BZ    EXITKEEP      FAIL (R15=ENTRY)                   GP03116
         ICM   R9,15,0(R6)   LOAD AND TEST WORK ADDRESS
         BNZ   HAVEWORK      PREVIOUSLY GOTTEN; USE IT
         BAS   R8,GETMEAN    GET AND INIT STORAGE
         ST    R9,0(,R6)     SAVE FOR NEXT TIME
         B     HAVEWORK      JOIN COMMON
TEMPWORK BAS   R8,GETMEAN    GET STORAGE
HAVEWORK ST    R13,4(,R9)    MAKE BACK CHAIN
         ST    R9,8(,R13)    MAKE FORWARD CHAIN
         LR    R13,R9        MAKE NEW SAVE AREA (NEED FOR DEBUGGING)
         USING DYNAREA,R13   DECLARE IT
         LA    R0,LOCWORK
         L     R1,=A(LOCWKLEN)
         SR    R15,R15
         MVCL  R0,R14        CLEAR WORK SPACE
         B     INIT
DOGPATCH DC    128S(*)            MODULE PATCH AREA
         SPACE 2
*---------------------------------------------------------------------*
* GENERAL PURPOSE REGISTERS:                                          *
*                                                                     *
* GPR         USE         AS                                          *
* ---         ---         --                                          *
* R1          INT         PARM REG/WORK REG                           *
* R8          INT         BAL REGISTER                                *
* R11         INT         PROGRAM BASE REGISTER                       *
* R12         INT         PROGRAM BASE REGISTER                       *
* R13         INT         SAVE AREA                                   *
* R15         INT/OUT     WORK REG, RETURN CODE                       *
*                                                                     *
*---------------------------------------------------------------------*
         SPACE 2
***********************************************************************
*                                                                     *
NUMSTACK EQU   22            NUMBER OF NODES IN STACK
MAXSTLEN EQU   255           MAXIMUM STRING LENGTH < 256
*                                                                     *
***********************************************************************
         TITLE 'S U B C O M P  ***  INITIALIZATION'
*---------------------------------------------------------------------*
*                                                                     *
* INITIALIZE FOR EXECUTION                                            *
*                                                                     *
*                          <<PSEUDOCODE>>                             *
*                                                                     *
*  GET INPUT PARMS                                                    *
*  INITIALIZE WORK AREA AND VARIABLE FIELDS                           *
*                                                                     *
* GET CALLER'S "STRING" AND "GENERIC" PARAMETERS                      *
*                                                                     *
* CASE 1: ADDRESS POINTERS POINT TO CHARACTER TEXT (FIXED LENGTH MODE)*
* CASE 2: ADDRESS POINTERS POINT TO LENGTH HALFWORD, FOLLOWED BY TEXT *
*                                                                     *
*---------------------------------------------------------------------*
INIT     LM    R2,R4,0(R5)   GET POINTERS TO ALL THREE PARMS
         TM    8(R5),X'80'   VL BIT ON PARM THREE ?
         BZ    INITHAVE      YES; WORK AREA IS TEMPORARY ONLY
         OI    CSWPFLGS,PFTEMP  REMEMBER TO FREE WORK AREA
INITHAVE CLI   0(R2),C' '    IS THE FUNCTION CODE WITHOUT A LENGTH?
         BNL   UNLENGTH      YES; DO FIXED LENGTH MOVES
         LA    R0,FUNPARM
         LA    R1,L'FUNPARM  GET DESTINATION ADDRESS/LENGTH
         LH    R15,0(,R2)    GET LENGTH OF FUNCTION CODE
         ICM   R15,8,BLANKS  REQUEST BLANK PADDING
         LA    R14,2(,R2)    SOURCE ADDRESS
         MVCL  R0,R14        WOW; THIS IS REALLY SLOW
         LA    R0,DSNPARM
         LA    R1,L'DSNPARM  GET DESTINATION ADDRESS/LENGTH
         LH    R15,0(,R3)    GET LENGTH OF STRING
         LA    R14,2(,R3)    SOURCE ADDRESS
         STM   R14,R15,DB    SAVE FOR FEEDBACK
         ICM   R15,8,BLANKS  REQUEST BLANK PADDING
         MVCL  R0,R14        WOW; THIS IS REALLY SLOW
         LA    R0,GENPARM
         LA    R1,L'GENPARM  GET DESTINATION ADDRESS/LENGTH
         LH    R15,0(,R4)    GET LENGTH OF STRING
         ICM   R15,8,BLANKS  REQUEST BLANK PADDING
         LA    R14,2(,R4)    SOURCE ADDRESS
         MVCL  R0,R14        WOW; THIS IS REALLY SLOW
         B     COMMPARM
         SPACE 1
UNLENGTH MVC   FUNPARM,BLANKS
         MVC   DSNPARM,DSNPARM-1  BLANK FILL
         MVC   GENPARM,GENPARM-1  BLANK FILL
         OC    FUNPARM,0(R2)                                    GP02289
         LA    R15,6-1       VOL-SER LENGTH                     GP02289
         CLC   =C'VOL',FUNPARM    VOL-SER COMPARE ?             GP02289
         BE    UNLENCOM                                         GP05168
         LA    R15,8-1       MEMBER LENGTH                      GP05168
         CLC   =C'MEM',FUNPARM    MEMBER COMPARE ?              GP05168
         BE    UNLENCOM                                         GP02289
UNLEN44  LA    R15,44-1      DEFAULT LENGTH                     GP02289
UNLENCOM EX    R15,EXMVCR3   MOVE VALUE TO BE TESTED            GP02289
         EX    R15,EXMVCR4   MOVE MASK                          GP02289
         LA    R4,1(,R15)    TRUE LENGTH                        GP02289
         STM   R3,R4,DB      SAVE FOR FEEDBACK
COMMPARM OC    FUNPARM,BLANKS     JUST IN CASE
         MVI   GTAB+C'*',C'*'     ASTERISK TRIGGER - TRANSLATE TABLE G
         CLC   =C'DSN',FUNPARM    DATA SET COMPARISON?
         BE    COMMDSN            YES; SET IT
         CLC   =C'POS',FUNPARM    DATA SET COMPARISON WITH POSITIONAL?
         BE    POSCOMP            YES; GET IT                   GP09184
         CLC   =C'ACT',FUNPARM    ACCOUNT COMPARISON?
         BE    COMMACT            YES; SET IT
         CLC   =C'MSK',FUNPARM    BUILD CSI MASK FROM USER'S
         BE    COMMMASK           YES; BUILD IT
         CLC   =C'AST',FUNPARM    BUILD COMP MASK FROM USER'S   GP09177
         BE    COMMASTM           YES; BUILD IT                 GP09177
         CLC   =C'MEM',FUNPARM    MEMBER NAME COMPARISON?       GP05168
         BE    COMMMEM            YES; BUILD IT                 GP08328
         CLC   =C'VOL',FUNPARM    VOLUME SERIAL COMPARISON ?    GP05168
         BE    COMMMEM            YES; BUILD IT                 GP08328
         CLC   =C'END',FUNPARM    SPECIAL - RELEASE STORAGE ?
         BNE   COMMOTH       NO; TREAT AS SIMPLE COMPARISON
         OI    CSWPFLGS,PFTEMP  FORCE FREEING THE WORK AREA
         B     EXIT               AND RETURN                    GP02290
COMMOTH  CLC   GENPARM(44),DCAST1     COMPLETE GENERAL MATCH ?
         BE    EXIT               YES; SKIP REMAINING GARBAGE
         OI    CSWPFLGS,PFOTH   SET OTHER FLAG
         MVC   LVLMAX,=Y(MAXSTLEN)  NO LEVEL RESTRICTION
         MVI   LVLSEP,X'FE'     ONLY ONE NODE
         MVI   GTAB+X'FE',C'.'    NO TRIGGER - TRANSLATE TABLE G
         MVI   GTAB+C' ',C' '     BLANK TRIGGER - TRANSLATE TABLE G
         MVI   DTAB+X'FE',C'.'    NO TRIGGER - TRANSLATE TABLE D
         MVI   DTAB+C' ',C' '     BLANK TRIGGER - TRANSLATE TABLE D
         B     INITDONE      MORE LATER?
COMMACT  CLC   GENPARM(44),DCAST2     COMPLETE GENERAL MATCH ?
         BE    EXIT               YES; SKIP REMAINING GARBAGE
         MVI   LVLMAX+L'LVLMAX-1,44   VERY LONG LEVELS
         MVI   LVLSEP,C','      COMMA SEPARATES NODES
         OI    CSWPFLGS,PFACT   SET ACCT FLAG
         MVI   GTAB+C',',C'.'     COMMA TRIGGER - TRANSLATE TABLE G
         MVI   GTAB+C' ',C' '     BLANK TRIGGER - TRANSLATE TABLE G
         MVI   DTAB+C',',C'.'     COMMA TRIGGER - TRANSLATE TABLE D
         MVI   DTAB+C' ',C' '     BLANK TRIGGER - TRANSLATE TABLE D
         B     INITDONE           RETURN TO CALLER
COMMMEM  CLC   GENPARM(44),DCAST1     COMPLETE GENERAL MATCH ?  GP08328
         BE    EXIT               YES; SKIP REMAINING GARBAGE   GP08328
         OI    CSWPFLGS,PFMEM   SET MEMBER/VOLUME FLAG          GP08328
         B     COMMDMC            JOIN COMMON                   GP08328
COMMDSN  CLC   GENPARM(44),DCAST2     COMPLETE GENERAL MATCH ?
         BE    EXIT               YES; SKIP REMAINING GARBAGE
         OI    CSWPFLGS,PFDSN   SET DSN FLAG                    GP08328
COMMDMC  MVI   LVLMAX+L'LVLMAX-1,8   DSN LEVEL MAXIMUM LENGTH
         MVI   LVLSEP,C'.'      PERIOD SEPARATES NODES
         MVI   LVLMIN+L'LVLMIN-1,1   MINIMUM LEVEL LENGTH
         MVI   GTAB+C' ',C' '     BLANK TRIGGER - TRANSLATE TABLE G
         MVI   DTAB+C' ',C' '     BLANK TRIGGER - TRANSLATE TABLE D
         MVI   GTAB+C'.',C'.'     PERIOD TRIGGER - TRANSLATE TABLE G
         MVI   DTAB+C'.',C'.'     PERIOD TRIGGER - TRANSLATE TABLE D
         B     INITDONE           RETURN TO CALLER
EXMVCR3  MVC   DSNPARM(44),0(R3)                                GP02289
EXMVCR4  MVC   GENPARM(44),0(R4) MOVE USER'S FIXED LENGTH MASK  GP02289
         SPACE 2
***********************************************************************
*                                                                     *
*   ENTRY FUNCTION MSK ANALYZES THE INPUT MASK FIELD, AND BUILDS      *
*     AN OUTPUT MASK IN THE 'DSN' FIELD SUITABLE FOR PROCESSING       *
*     BY IGGCSI00. NOTE THAT SUPERLOCATE WILL NOT ACCEPT A WILDCARD   *
*     CHARACTER IN THE FIRST INDEX LEVEL; GENERAL SEARCHES NEED TO BE *
*     DONE BY THE CATALOG LOOKUP ROUTINE (SEE SUBCAT IN ICF MODE).    *
*                                                                     *
*   BASICALLY, IT EXTRACTS THE HIGHEST NUMBER OF LEVELS NOT USING     *
*     MASKING CHARACTERS, AND APPENDS ** (GENERAL MATCH) TO THEM.     *
*                                                                     *
***********************************************************************
COMMMASK LA    R5,GENPARM    SET START OF SCAN
         LA    R4,L'GENPARM-1  SET LENGTH TO CHECK LESS ONE FOR EXEC
         LA    R3,GENPARM-1  SET ADDRESS OF LAST PERIOD
         SR    R2,R2         FOR BRANCH
         LR    R15,R5        SET CURSOR
COMMMMEX EX    R4,EXTRTUMS
         B     COMMMBRT(R2)  BRANCH ACCORDING TO RESULT
COMMMBRT B     COMMMCOP      PROBABLY BAD - 44 ALPHA
         B     COMMMIND      INDEX
         B     COMMMMSK      MASK CHARACTER
         B     COMMMBLK      BLANK - END OF SCAN (NO MASKS)
         SPACE 1
COMMMIND LR    R3,R1         SAVE LOCATION OF INDEX END
         LA    R15,1(,R1)    SET NEXT SCAN LOCATION
         LA    R4,GENPARM+L'GENPARM-1  SET LAST SCANNED POSITION
         SR    R4,R15        CHARS LEFT - 1
         BM    COMMMMSL      ENDS WITH INDEX                    GP09157
         CLI   0(R15),C' '   ENDS WITH INDEX?                   GP09157
*NEXT*   BE    COMMMMSK      YES; STOP NOW                      GP09157
         BNE   COMMMMEX      DO ANOTHER                         GP09157
COMMMMSK LR    R4,R3         GET LAST INDEX ADDRESS
         SR    R4,R5         LESS START
         BNP   COMMMMSL      NOTHING
         EX    R4,EXMVCUMS   MOVE INDEX (WITH PERIOD)
COMMMMSL LA    R1,DSNPARM(R4)  LOCATION OF MASK
         MVC   1(2,R1),DCAST2  MAKE GENERAL MATCH AFTER INDEX
         LA    R0,DSNPARM
         LA    R1,3(,R1)     NEW OFFSET
         SR    R1,R0         NEW LENGTH
         B     COMMMEND
COMMMBLK DS    0H            NAME CONTAINS NOMASKS
COMMMCOP MVC   DSNPARM,GENPARM  RETURN TO USER
         LA    R0,DSNPARM
         LA    R1,L'DSNPARM
COMMMEND LM    R14,R15,DB    USER'S DSN
         ICM   R1,8,BLANKS
         MVCL  R14,R0        COPY CSI MASK
         B     EXIT
EXTRTUMS TRT   0(0,R15),TRTMASK  GET FIRST SPECIAL CHARACTER
EXMVCUMS MVC   DSNPARM(0),GENPARM  MOVE LONGEST INDEX
         SPACE 2
***********************************************************************
*                                                                     *
*   ENTRY FUNCTION AST ANALYZES THE INPUT MASK FIELD, AND COPIES      *
*     AS-IS IF THERE ARE WILD-CARD CHARACTERS IN THE MASK.            *
*     IF LAST NON-BLANK IS . IT APPENDS ** TO OUTPUT MASK             *
*                                                                     *
***********************************************************************
COMMASTM MVC   DSNPARM,GENPARM    COPY AS IS                    GP09177
         LA    R5,DSNPARM    SET START OF SCAN                  GP09177
         LA    R4,L'DSNPARM-1  SET LENGTH TO CHECK LESS ONE FOR EXEC
         LA    R3,DSNPARM-1  SET ADDRESS OF LAST PERIOD         GP09177
         SR    R2,R2         FOR BRANCH                         GP09177
         LR    R15,R5        SET CURSOR                         GP09177
COMMAMEX EX    R4,EXTRTUMS                                      GP09177
         B     COMMABRT(R2)  BRANCH ACCORDING TO RESULT         GP09177
COMMABRT B     COMMABLK      PROBABLY BAD - 44 ALPHA - ACCEPT   GP09177
         B     COMMAIND      INDEX                              GP09177
         B     COMMABLK      MASK CHARACTER - ACCEPT AS IS      GP09177
         B     COMMABLK      BLANK - END OF SCAN (NO MASKS)     GP09177
         SPACE 1                                                GP09177
COMMAIND LR    R3,R1         SAVE LOCATION OF INDEX END         GP09177
         LA    R15,1(,R1)    SET NEXT SCAN LOCATION             GP09177
         LA    R4,DSNPARM+L'DSNPARM-1  LAST SCANNED POSITION    GP09177
         SR    R4,R15        CHARS LEFT - 1                     GP09177
         BM    COMMABLK      ENDS WITH INDEX                    GP09177
         CLI   0(R15),C' '   ENDS WITH INDEX?                   GP09177
         BNE   COMMAMEX      DO ANOTHER                         GP09177
         MVI   0(R15),C'*'     INSERT TRAILING MASK             GP09177
         MVI   1(R15),C'*'     INSERT TRAILING MASK             GP09177
COMMABLK DS    0H            NAME CONTAINS NOMASKS              GP09177
         LA    R0,DSNPARM                                       GP09177
         LA    R1,L'DSNPARM                                     GP09177
         LM    R14,R15,DB    USER'S DSN                         GP09177
         ICM   R1,8,BLANKS                                      GP09177
         MVCL  R14,R0        COPY CSI MASK                      GP09177
         B     EXIT                                             GP09177
         EJECT
*--------------------------------------------------------------------*
*                                                                    *
*  SPLIT STRING INTO SEPARATE QUALIFIERS, COUNT THE NUMBER OF        *
*  QUALIFIERS AND SET APPROPRIATE FLAG(S) FOR EACH ONE.              *
*                                                                    *
*--------------------------------------------------------------------*
         SPACE 1
INITDONE SLR   R5,R5                   CLEAR NODE COUNTER
         LA    R4,DSNTABS-LDSNTAB      DSN TABLE POINTER LESS 1 ENTRY
         USING STRQTAB,R4
         LA    R1,DSNPARM-1            DSN POINTER LESS 1 BYTE
         LH    R15,LVLMAX    GET MAXIMUM LENGTH OF ONE LEVEL
         MIN   R15,DB+4      LESS THAN 8 IF VOL REQUEST         GP08296
SPLOOP   LA    R1,1(,R1)               POINT TO 1ST CHAR IN NODE
         LR    R3,R1                   SAVE POINTER TO NODE
         LA    R4,LDSNTAB(,R4)         INCREMENT NODE TABLE POINTER
         LA    R5,1(,R5)               BUMP NODE COUNTER
         LA    R1,1(R15,R3)  END IF VOL OR MEM                  GP08296
         EX    R15,EXTRTDAT  FIND STOPPER USING DATA TABLE
         BNZ   SPFINDX       FOUND INDEX                        GP08296
         TM    CSWPFLGS,PFDSN        MEM OR VOL REQUEST ?       GP08328
         BNZ   INVDSN        NO; INVALID                        GP08296
         MVI   0(R1),C' '    FORCE STOPPER                      GP08296
SPFINDX  MVC   STRQUAL,BLANKS  CLEAR IT
         LR    R6,R1                   CALC NODE LENGTH
         SR    R6,R3
         STH   R6,STRQLEN
         CH    R6,LVLMIN     NOT TOO SHORT?
         BL    INVDSN        NODE TOO SHORT; IS GARBAGE
         BCTR  R6,0                    DECREMENT FOR EXECUTE
         EX    R6,EXMVCDAT             CAPTURE DSN NODE
         CLI   0(R1),C' '              ?-END OF DSN
         BNE   SPLOOP                   N-ITERATE FOR NEXT NODE
         OI    STRQFLAG,FDQ             Y-NODE IS FINAL DSN QUALIFIER
         ST    R5,NUMNODES             SAVE NODE COUNTER
         B     SPLITDON                RETURN TO CALLER
EXMVCDAT MVC   STRQUAL(*-*),0(R3)      CAPTURE DSN NODE
EXTRTDAT TRT   0(*-*,R3),DTAB     FIND NEXT SPACE OR PERIOD
         DROP  R4
         EJECT ,
*---------------------------------------------------------------------*
*                                                                     *
*  WHEN THE MASK CONTAINS A POSITIONAL (DISREGARD INDEX POINT) CHAR   *
*  (%), THEN WE DO NOT SPLIT INDEX LEVELS, BUT COMPARE BYTE BY BYTE   *
*                                                                     *
*   DATASET MASKS ARE PROCESSED AS FOLLOWS:                           *
*                                                                     *
*   1) A NAME WITHOUT WILDCARDS IS PROCESSED AS IS                    *
*   2) A ? MATCHES ANY CHARACTER OTHER THAN BLANK AND PERIOD          *
*   3) A % MATCHES ANY CHARACTER                                      *
*   4) A * MATCHES THE REMAINDER OF THE INDEX LEVEL                   *
*   5) A TRAILING ** MATCHES ANY NON-BLANKS                           *
*                                                                     *
*      ABC.DEF                      EXACT NAME; PROCESSED AS IS       *
*      ABC.DE*     MATCHES ABC.DEF, BUT NOT ABC.D OR ABC.DEF.G        *
*      ABC.???     MATCHES ABC.DEF, BUT NOT ABC.DE OR ABC.DEF.G       *
*      ABC.*.*     MATCHES ABC.DEF.G, BUT NOT ABC.DEF                 *
*      ABC.D**     MATCHES ABC.DE, ABC.DEF, ABC.DEF.G                 *
*      ABC.%%%%    MATCHES ABC.DEFG, ABC.DE.F, BUT NOT ABC.DEF.G      *
*      ABC.%%%%*   MATCHES ABC.DEF.G, ABC.DEF.GH, BUT NOT ABC.DEF.G.H *
*                                                                     *
*                                                                     *
*---------------------------------------------------------------------*
         SPACE 1
POSCOMP  LA    R0,1          SET INCREMENT
         LA    R1,L'DSNPARM-1+DSNPARM  SET END OF DSN
         LR    R2,R0         DITTO FOR MASK
         LA    R3,L'GENPARM  GET MASK LENGTH
         LA    R15,DSNPARM   POINT TO NAME TO BE CHECKED
         LA    R14,GENPARM   POINT TO MASK
         LA    R3,GENPARM-1(R3) LAST BYTE OF MASK
COMPMASK CLI   0(R14),C'%'   ACCEPT ANYTHING IN THIS POSITION?
         BE    BOTHUP        YES; INCREMENT MASK AND NAME
         CLC   0(1,R14),0(R15)  MASK AND DSN MATCH?
         BE    BOTHUP        YES; INCREMENT
         CLI   0(R14),C'?'   WITHIN LEVEL MATCH?
         BNE   COMPAST       NO; CHECK FOR ASTERISKS
         CLI   0(R15),C' '   IS THE DSNMASK FINISHED?
         BE    NOMATCH       YES; NOT MATCHED
         CLI   0(R15),C'.'   IS THE LEVEL FINISHED?
         BE    NOMATCH       YES; ALSO NO MATCH
         B     BOTHUP        ELSE ACCEPT AND INCREMENT
COMPAST  CLI   0(R14),C'*'   ASTERISK REQUEST?
         BNE   NOMATCH       NO; BYTE IS UNMATCHED
         CLI   1(R14),C'*'   GENERAL MATCH FROM HERE ON?
         BE    EXIT          YES; PROCESS
         CLI   1(R14),C' '   LIMIT TO THIS LEVEL?
         BNE   COMPASTL      NO
COMPASTB CLI   0(R15),C'.'   REACHED NEXT INDEX LEVEL?
         BE    NOMATCH       YES; NOT A MATCH
         CLI   0(R15),C' '   REACHED END OF NAME ?
         BE    EXIT          YES; FULL MATCH
         BXLE  R15,R0,COMPASTB  SKIP TO NEXT LEVEL
         B     EXIT          DSN MISSING LEVEL; ACCEPT
COMPASTL CLI   0(R15),C'.'   REACHED NEXT INDEX LEVEL?
         BE    BACKUP        YES; INCREMENT BOTH
         CLI   0(R15),C' '   REACHED END OF NAME ?
         BE    NOMATCH       YES; NOT A MATCH
         BXLE  R15,R0,COMPASTL  SKIP TO NEXT LEVEL
         B     NOMATCH       DSN MISSING LEVEL; NO ACCEPT
NOMATCH  OI    RETC+L'RETC-1,4  SHOW NO MATCH
         B     EXIT          EXIT UNMATCHED
         SPACE 1
BACKUP   BCTR  R15,0         NEED TO MATCH PERIODS
BOTHUP   BXH   R14,R2,EXIT    ACCEPT IF MASK EXHAUSTED
         BXLE  R15,R0,COMPMASK  ELSE TRY AGAIN
         B     EXIT          FULL MATCH
         EJECT
*--------------------------------------------------------------------*
*                                                                    *
*  SPLIT MASK INTO SEPARATE QUALIFIERS, COUNT NUMBER OF QUALIFIERS   *
*  AND SET APPROPRIATE FLAG(S)/VALUES FOR EACH ONE.                  *
*                                                                    *
*--------------------------------------------------------------------*
         SPACE 1
SPLITDON SR    R5,R5                   CLEAR NODE COUNT
         LA    R4,DSNTABS-LGENTAB
         AH    R4,=Y(GENTABS-DSNTABS)  GEN TABLE POINTER LESS 1 ENTRY
         USING GENQTAB,R4
         LA    R1,GENPARM-1            GENPARM POINTER LESS 1 BYTE
SPLOOP2  LA    R4,LGENTAB(,R4)         INCREMENT GEN TABLE POINTER
         LA    R5,1(,R5)               BUMP NODE COUNTER
SPLOOP3  LH    R15,LVLMAX    SET LENGTH FOR TRT
         LA    R7,SPLOOP2              SETUP NORMAL LOOP ADDRESS
         LA    R1,1(,R1)               POINT TO 1ST CHAR IN NODE
         LR    R3,R1                   SAVE POINTER TO NODE
         EX    R15,EXTRTMSK
         BZ    INVGEN                  NONE, GENERIC IS GARBAGE
         LR    R6,R1                   CALC NODE LENGTH
         SR    R6,R3
         ST    R6,GENNLEN
         BZ    INVGEN                  NULL NODE, GENERIC IS GARBAGE
         EX    R6,EXMVCMSK             CAPTURE NODE + PERIOD/BLANK
         LA    R8,GENNODE(R6)  GET LAST BYTE PASSED
         MVC   0(1,R8),LVLSEP  CHANGE POSS. BLANK TO REAL SEPARATOR
         BAS   R8,GENCALC              GO BUILD GENERIC TABLE ENTRY
         CLC   0(2,R3),DCAST2          ?-PSEUDO QUALIFIER
         BNE   REALQUAL                 N-CONTINUE, REAL QUALIFIER
         OI    TEMPFLAG,NOGENNUM        Y-DON'T COUNT GENERIC NODES
         OI    GENQFLAG,QSEARCH          -(RE)START POINT FOR SEARCH
         LA    R7,SPLOOP3                -DISCARD THE "**" QUALIFIER
REALQUAL CLI   0(R1),C' '              ?-END OF DSN
         BNER  R7                       N-ITERATE FOR NEXT NODE
         OI    GENQFLAG,FGQ             Y-NODE IS FINAL GEN QUALIFIER
         TM    TEMPFLAG,NOGENNUM       ?-DON'T COUNT GENERIC NODES
         BO    *+8                      Y-DON'T COUNT THEM
         ST    R5,NUMGENS               N-SAVE GENERIC NODE COUNTER
         CLC   0(2,R3),DCAST2          ?-PSEUDO QUALIFIER
         BNE   *+8                      N-CONTINUE, REAL QUALIFIER
         OI    GENQFLAG,PSEQ            Y-NODE IS PSEUDO QUALIFIER
SPGEXIT  B     PARSEDON                RETURN TO CALLER
EXMVCMSK MVC   GENNODE(*-*),0(R3)      CAPTURE NODE + PERIOD/BLANK
EXTRTMSK TRT   0(*-*,R1),DTAB   FIND NEXT SPACE OR PERIOD
         EJECT
*--------------------------------------------------------------------*
*                                                                    *
*  ANALYZE GENERIC QUALIFIER FOR ONE OF SEVEN DIFFERENT TYPES AND    *
*  BUILD AN ENTRY REPRESENTING THE QUALIFIER.                        *
*  THE DIFFERENT TYPES ARE:                                          *
*  1. **                                                             *
*  2. *                                                              *
*  3. AB*DE                                                          *
*  4. *BCDE                                                          *
*  5. ABCD*                                                          *
*  6. *BCD*                                                          *
*  7. ABCDE                                                          *
*  TYPE 1    = VARIABLE NUMBER OF QUALIFIERS MAY EXIST               *
*  TYPE 2    = MATCH EQUAL ON QUALIFIER OF ANY LENGTH OR CONTENT     *
*  TYPES 3-6 = VARIABLE LENGTH QUALIFIERS                            *
*  TYPE 7    = FIXED LENGTH QUALIFIER                                *
*  TYPES 3-7 MAY ALSO CONTAIN 0-8 "DON'T CARE" CHARACTERS (% ?)      *
*                                                                    *
*--------------------------------------------------------------------*
         SPACE 1
GENCALC  STM   R0,R15,GCSAVE           SAVE REGS
         CLI   GENNODE,C'*'            LEADING ASTERISK?
         BNE   CHKLCHAR                NO, GO GET LEADING CHARACTERS
         CLC   GENNODE(2),DCAST2       PSEUDO QUALIFIER?
         BNE   NOTPSE                  NO, TRY NEXT TEST
         CLC   GENNODE+2(1),LVLSEP     END OF QUALIFIER FOLLOWS?
         BNE   INVGEN                  NO, GENERIC IS INVALID
         B     GENRET                  DON'T HANDLE PSEUDOS HERE
         SPACE 1
NOTPSE   LA    R1,GENNODE              PREP PTR FOR TRAILING CHARS
         CLC   1(1,R1),LVLSEP          END OF QUALIFIER FOLLOWS?
         BNE   CHKTCHA2                NO, GO GET TRAILING CHARACTERS
         B     GENRET                  YES, WE'RE DONE WITH THIS ONE
CHKLCHAR LH    R15,LVLMAX
         LA    R1,GENNODE
         EX    R15,EXTRTAST
         LR    R8,R1                   CALC LENGTH OF LEADING CHARS
         LA    R0,GENNODE
         SLR   R8,R0                   . . .
         STH   R8,GENQLEN1             . . .
         OI    GENQFLAG,LCHAR          SPECIFIC LEADING CHARS EXIST
         BCTR  R8,0                    DECREMENT FOR EXECUTE
         EX    R8,GETQ1                GET THE LEADING COMPARE CHARS
         EX    R8,TRANQ1               CHANGE ALL % TO X'00'
         EX    R8,INITM1               COPY COMPARE CHARS TO MASK
         EX    R8,TRANM1               CHANGE ALL NON X'00' TO X'FF'
         CLI   0(R1),C'*'              QUAL TERMINATED BY ASTERISK?
         BE    CHKTCHAR                YES, GO CHECK TRAILING CHARS
         OI    GENQFLAG,ACHAR          NO, ALL CHARS MUST MATCH
         B     GENRET                  FINISHED WITH THIS QUALIFIER
         SPACE 1
CHKTCHAR CLC   1(1,R1),LVLSEP          END OF QUALIFIER FOLLOWS?
         BE    GENRET                  YES, DONE WITH THIS QUALIFIER
CHKTCHA2 LH    R15,LVLMAX
         LA    R3,1(,R1)               FIRST POSITION FOLLOWING ASTER
         EX    R15,EXTRTAST
         LR    R8,R1                   CALC LENGTH OF TRAILING CHARS
         SR    R8,R3                   . . .
         BZ    INVGEN                  ABCDEF** = INVALID
         STH   R8,GENQLEN2             . . .
         BCTR  R8,0                    DECREMENT FOR EXECUTE
         EX    R8,GETQ2                GET THE TRAILING COMPARE CHARS
         EX    R8,TRANQ2               CHANGE ALL % TO X'00'
         EX    R8,INITM2               COPY COMPARE CHARS TO MASK
         EX    R8,TRANM2               CHANGE ALL NON X'00' TO X'FF'
         CLI   0(R1),C'*'              QUAL TERMINATED BY ASTERISK?
         BE    CHKTVAL                 YES, GO CHECK VALIDITY
         OI    GENQFLAG,TCHAR          SPECIFIC TRAILING CHARS EXIST
         B     GENRET                  FINISHED WITH THIS QUALIFIER
         SPACE 1
CHKTVAL  CLC   1(1,R1),LVLSEP          END OF QUALIFIER FOLLOWS?
         BNE   INVGEN                  NO, *AB*CD = INVALID
         TM    GENQFLAG,LCHAR          LEADING CHARS ALSO?
         BO    INVGEN                  YES, AB*CD* = INVALID
GENRET   LH    R7,GENQLEN1             LENGTH 1ST STRING
         AH    R7,GENQLEN2             LENGTH 2ND STRING
         CH    R7,LVLMAX               TOTAL MORE THAN 8?
         BH    INVGEN                  YES, GENERIC IS INVALID
         LM    R0,R15,GCSAVE           RESTORE REGS
         BR    R8                      RETURN TO CALLER
GETQ1    MVC   GENQUAL1(*-*),GENNODE   GET CHARS PRIOR TO ASTERISK
TRANQ1   TR    GENQUAL1(*-*),TRANTAB1  CHANGE ALL % TO X'00'
INITM1   MVC   GENMASK1(*-*),GENQUAL1  INIT MASK W/TRANSLATED CHARS
TRANM1   TR    GENMASK1(*-*),TRANTAB2  CHANGE ALL BUT X'00' TO X'FF'
GETQ2    MVC   GENQUAL2(*-*),0(R3)     GET CHARS FOLLOWING ASTERISK
TRANQ2   TR    GENQUAL2(*-*),TRANTAB1  CHANGE ALL % TO X'00'
INITM2   MVC   GENMASK2(*-*),GENQUAL2  INIT MASK W/TRANSLATED CHARS
TRANM2   TR    GENMASK2(*-*),TRANTAB2  CHANGE ALL BUT X'00' TO X'FF'
EXTRTAST TRT   1(*-*,R1),GTAB     FIND ASTERISK OR END QUALIFIER
         DROP  R4
         EJECT
*--------------------------------------------------------------------*
*                                                                    *
*  COMPARE DSN QUALIFIERS AGAINST THE GENERIC QUALIFIERS             *
*                                                                    *
*--------------------------------------------------------------------*
         SPACE 1
PARSEDON ICM   R15,15,NUMGENS          SPECIFIC NUMBER NODES REQUIRED?
         BZ    CDSETUP                 NO, OH WELL, CONTINUE
         C     R15,NUMNODES            YES, DOES DSN HAVE THAT NUMBER?
         BNE   NOSELECT                NO, THIS WAS SURE AN EASY OUT
CDSETUP  LA    R15,DSNTABS
         LR    R14,R15
         AH    R14,=Y(GENTABS-DSNTABS) GEN TABLE POINTER
         USING GENQTAB,R14
         USING STRQTAB,R15
CDLOOP   TM    GENQFLAG,QSEARCH        THIS NODE A RESTART POINT?
         BNO   NORST                   NO, LEAVE RESTART ADDR AS IS
         ST    R14,GENSTART            YES, SET GENERIC RESTART POINT
         LA    R2,LDSNTAB(,R15)             SET DSN RESTART POINT
         TM    STRQFLAG,FDQ            FINAL DSN QUALIFIER DONE?
         BNO   *+6                     NO, CONTINUE
         SLR   R2,R2                   YES, DO NOT ALLOW DSN RESTART
         ST    R2,DSNSTART                  SAVE DSN RESTART POINT
NORST    BAS   R8,COMPQUAL             COMPARE A DSN/GENERIC QUALIFIER
         BNE   QFAIL                   DSN/GENERIC QUALIFIER MISMATCH
         TM    GENQFLAG,FGQ            FINAL GENERIC QUALIFIER DONE?
         BO    CHKEND1                 YES, GO CHECK ENDING STATUS
         LA    R14,LGENTAB(,R14)       BUMP TO NEXT GEN TABLE ENTRY
         TM    STRQFLAG,FDQ            FINAL DSN QUALIFIER DONE?
         BO    CHKEND2                 YES, GO CHECK ENDING STATUS
         LA    R15,LDSNTAB(,R15)       BUMP TO NEXT DSN TABLE ENTRY
         B     CDLOOP                  ITERATE COMPARE LOOP
*-----------------------------------------------------------*
* A DSN QUALIFIER DOES NOT MATCH ITS GENERIC QUALIFIER.              *
* IF WE ARE SEARCHING MULTIPLE DSN QUALIFIERS, RESTART THE           *
* SEARCH AT THE LAST KNOWN RESTART POINT.                            *
* IF NOT SEARCHING MULTIPLES, FAIL THIS DSN AND GET OUT.             *
*-----------------------------------------------------------*
         SPACE 1
QFAIL    ICM   R14,15,GENSTART         NEED TO RESTART SCAN?
         BZ    NOSELECT                NO, THIS DSN DONE FAILED
         ICM   R15,15,DSNSTART         DSN RESTART POINT OK?
         BZ    NOSELECT                NO, THIS DSN DONE FAILED
         B     CDLOOP
CDEXIT   B     EXIT
*-----------------------------------------------------------*
* FINAL GENERIC QUALIFIER SATISFIED. CHECK FOR END OF DSN            *
*-----------------------------------------------------------*
CHKEND1  TM    STRQFLAG,FDQ            FINAL DSN QUALIFIER DONE?
         BO    CDEXIT                  YES, DSN PASSED ALL THE TESTS
         TM    GENQFLAG,PSEQ           FINAL GEN QUALIFIER = PSEUDO?
         BO    CDEXIT                  YES, DSN PASSED ALL THE TESTS
         B     NOSELECT                NO, THIS DSN DONE FAILED
*-----------------------------------------------------------*
* END OF DSN REACHED. CHECK FOR PSUEDO GENERIC QUALIFIER.            *
*-----------------------------------------------------------*
CHKEND2  TM    GENQFLAG,PSEQ           FINAL GEN QUALIFIER = PSEUDO?
         BO    CDEXIT                  YES, DSN PASSED ALL THE TESTS
*NEXT*   B     NOSELECT                NO, THIS DSN DONE FAILED
         SPACE 1
NOSELECT MVI   RETC+3,4                DSN DID NOT PASS GENERIC TESTS
         B     CDEXIT                  GET OUT
         EJECT
*--------------------------------------------------------------------*
*                                                                    *
*  COMPARE A DSN QUALIFIER AGAINST ITS GENERIC COUNTERPART AND       *
*  RETURN CONDITION CODE ZERO IF THE DSN QUALIFIER MATCHES           *
*                                                                    *
*  SEE IF QUALIFIER LENGTHS ALONE DETERMINE MATCH/MISMATCH           *
*--------------------------------------------------------------------*
COMPQUAL LH    R7,GENQLEN1             LENGTH 1ST STRING
         LH    R2,GENQLEN2             LENGTH 2ND STRING
         ALR   R2,R7                   TOTAL LENGTH REQUIRED CHARS
         BZ    MATCH                   ZERO LENGTH=AUTOMATIC MATCH
         CH    R2,STRQLEN              DSN QUALIFIER LONG ENOUGH?
         BH    MISMATCH                NO, QUALIFIER CAN'T MATCH
         TM    GENQFLAG,ACHAR          ALL CHAR MATCH REQUIRED?
         BNO   ENDAC                   NO, SKIP LENGTH CHECK
         CH    R7,STRQLEN              YES, ARE LENGTHS EQUAL?
         BNE   MISMATCH                NO, QUALIFIER CAN'T MATCH
*-----------------------------------------------------------*
* SEE IF "LEADING" DSN CHARACTERS MATCH GENERIC SPEC                 *
*-----------------------------------------------------------*
ENDAC    TM    GENQFLAG,LCHAR          LEADING CHAR MATCH REQUIRED?
         BNO   ENDLC                   NO, SKIP LEADING COMPARE
         BCTR  R7,0                    DECREMENT FOR EXECUTE
         EX    R7,GETMASK1             SETUP "DON'T CARE" MASK
         EX    R7,GETQUAL1             MERGE DSN QUALIFIER W/MASK
         EX    R7,COMPARE1             DSN CHARS MATCH GENERIC?
         BNE   MISMATCH                NO, QUALIFIER DOESN'T MATCH
         TM    GENQFLAG,ACHAR          ALL CHAR MATCH REQUIRED?
         BO    MATCH                   YES, QUALIFIER MATCHES
*-----------------------------------------------------------*
* SEE IF "TRAILING" DSN CHARACTERS MATCH GENERIC SPEC                *
*-----------------------------------------------------------*
ENDLC    TM    GENQFLAG,TCHAR          TRAILING CHAR MATCH REQUIRED?
         BNO   ENDTC                   NO, SKIP TRAILING COMPARE
         LH    R7,GENQLEN2             LENGTH 2ND STRING
         LA    R1,STRQUAL              CALC PTR TO TRAILING DSN CHARS
         AH    R1,STRQLEN              . . .
         SLR   R1,R7                   . . .
         BCTR  R7,0                    DECREMENT FOR EXECUTE
         EX    R7,GETMASK2             SETUP "DON'T CARE" MASK
         EX    R7,GETQUAL2             MERGE DSN QUALIFIER W/MASK
         EX    R7,COMPARE2             DSN CHARS MATCH GENERIC?
         BE    MATCH                   YES, QUALIFIER MATCHES
         B     MISMATCH                NO, QUALIFIER DOESN'T MATCH
*-----------------------------------------------------------*
* SEE IF "MIDDLE" DSN CHARACTERS MATCH GENERIC SPEC                  *
*-----------------------------------------------------------*
ENDTC    LH    R2,STRQLEN              CALC LOOP COUNT FOR SCAN
         SH    R2,GENQLEN1             . . .
         LA    R2,1(R2)                . . .
         ST    R2,SCANCNT              . . .
         SLR   R2,R2                   CLEAR LOOP COUNTER
         LA    R1,STRQUAL              1ST CHAR OF DSN QUALIFIER
SCANLOOP EX    R7,GETMASK2             SETUP "DON'T CARE" MASK
         EX    R7,GETQUAL2             MERGE DSN QUALIFIER W/MASK
         EX    R7,COMPARE2             DSN CHARS MATCH GENERIC?
         BE    MATCH                   YES, QUALIFIER MATCHES
         C     R2,SCANCNT              NO, IS SCAN DONE YET?
         BNL   MISMATCH                YES, QUALIFIER DOESN'T MATCH
         LA    R1,1(R1)                BUMP DSN QUALIFIER POINTER
         LA    R2,1(R2)                BUMP LOOP COUNTER
         B     SCANLOOP                ITERATE SCAN LOOP
MATCH    CLR   R0,R0                   SET CONDITION CODE ZERO
         BR    R8                      RETURN TO CALLER
MISMATCH CLR   R14,R15                 SET CONDITION CODE NON-ZERO
         BR    R8                      RETURN TO CALLER
GETMASK1 MVC   COMPAREA(*-*),GENMASK1  GET THE "DON'T CARE" MASK
GETMASK2 MVC   COMPAREA(*-*),GENMASK2  GET THE "DON'T CARE" MASK
GETQUAL1 NC    COMPAREA(*-*),STRQUAL   MERGE DSN CHARACTERS W/MASK
GETQUAL2 NC    COMPAREA(*-*),0(R1)     MERGE DSN CHARACTERS W/MASK
COMPARE1 CLC   COMPAREA(*-*),GENQUAL1  DSN MATCHES GENERIC SPEC?
COMPARE2 CLC   COMPAREA(*-*),GENQUAL2  DSN MATCHES GENERIC SPEC?
         DROP  R14,R15
         SPACE 1
EXIT     L     R15,RETC      SET RETURN CODE
         LR    R1,R13        SAVE WORK AREA ADDRESS
         L     R13,4(,R13)   RESTORE USER'S SAVE AREA
         ST    R15,16(,R13)  RETURN RETURN CODE
         TM    CSWPFLGS-DYNAREA(R1),PFTEMP  TEMPORARY GETMAIN?
         BZ    EXITKEEP      NO; KEEP MY WORK AREA
         L     R0,=A(GWORKLEN)
         STORAGE RELEASE,ADDR=(1),LENGTH=(0)
EXITKEEP LM    R14,R12,12(R13) RESTORE ALMOST EVERYTHING ELSE
         BSM   0,R14         RETURN TO CALLER                   GP03047
         EJECT
*--------------------------------------------------------------------*
*                                                                    *
*  PROCESS ERROR CONDITIONS.                                         *
*                                                                    *
*--------------------------------------------------------------------*
INVDSN   MVI   RETC+3,8           RETURN CODE
         B     EXIT               RETURN TO CALLER
         SPACE 1
INVGEN   XC    GENPARM,GENPARM    ERASE BAD DSN FOR NEXT CALLER
         MVI   RETC+3,12          RETURN CODE
         B     EXIT               RETURN TO CALLER
         SPACE 2
**********************************************************************
**                                                                  **
**   STORAGE INITIALIZATION                                         **
**                                                                  **
**********************************************************************
         PUSH  USING
         DROP  R13
GETMEAN  L     R3,=A(GWORKLEN)   LOAD WORK AREA LENGTH
         STORAGE OBTAIN,LENGTH=(R3),BNDRY=PAGE,LOC=BELOW (S.A.)
         LR    R9,R1         PRESERVE IT
         SR    R15,R15       CLEAR LENGTH AND INSERTION
         LR    R2,R1         COPY ADDRESS
         MVCL  R2,R14        CLEAR IT
         USING DYNAREA,R9
         MVC   DCAST2(3),=C'** '
         MVC   BLANKS+1(L'BLANKS-1),BLANKS
         BR    R8            RETURN TO CALLER
         POP   USING
*--------------------------------------------------------------------*
*                                                                    *
*    REFERENCE DATA                                                  *
*                                                                    *
*--------------------------------------------------------------------*
         SPACE 1
TRANTAB1 DC    256AL1(*-TRANTAB1)  NON-TRANSLATION TABLE
         ORG   TRANTAB1+C'%'   POINT TO PERCENT SIGN
         DC    X'00'           CHANGE TO HEX ZERO
         ORG   TRANTAB1+C'?'   POINT TO QUESTION MARK
         DC    X'00'           CHANGE TO HEX ZERO
         ORG   ,
         SPACE 1
TRANTAB2 DC    X'00',255X'FF'
         SPACE 1
TRTMASK  DC    256AL1(0)     EVERYTHING IS VALID, EXCEPT:
         TRENT TRTMASK,12,C' '  BLANK ENDS THE MASK
         TRENT ,8,C'?',C'%',C'*'  MASK VALUES
         TRENT ,4,C'.'       INDEX LEVEL
         ORG   ,
         EJECT
*--------------------------------------------------------------------*
*                                                                    *
*              DSECTS                                                *
*                                                                    *
*--------------------------------------------------------------------*
*------------------------------------------------------------*
*           DEFINE A SINGLE GENERIC QUALIFIER                         *
*------------------------------------------------------------*
GENQTAB  DSECT
GENQLEN1 DS    H             LENGTH OF GENERIC QUALIFIER PART 1
GENQLEN2 DS    H             LENGTH OF GENERIC QUALIFIER PART 2
GENQFLAG DS    X             GENERIC QUALIFIER FLAG BITS
FGQ      EQU   X'80'         THIS IS THE FINAL GENERIC QUALIFIER
PSEQ     EQU   X'40'         THIS IS A PSEUDO GENERIC QUALIFIER
LCHAR    EQU   X'20'         LEADING CHARS MUST MATCH
TCHAR    EQU   X'10'         TRAILING CHARS MUST MATCH
ACHAR    EQU   X'08'         ALL CHARS MUST MATCH
QSEARCH  EQU   X'01'         THIS IS A RESTART POINT FOR SEARCH
         DS    3X            UNUSED
GENQUAL1 DS    CL(MAXSTLEN)  LEADING, OR ALL CHARACTERS
GENMASK1 DS    CL(MAXSTLEN)  "DON'T CARE" MASK FOR ABOVE CHARS
GENQUAL2 DS    CL(MAXSTLEN)  MIDDLE OR TRAILING CHARACTERS
GENMASK2 DS    CL(MAXSTLEN)  "DON'T CARE" MASK FOR ABOVE CHARS
LGENTAB  EQU   *-GENQTAB     LENGTH OF ONE ENTRY
*
*  FOR THE GENERIC QUALIFIER: % ARE CHANGED TO X'00'
*  FOR THE GENERIC MASK: % ARE X'00' AND ALL OTHER ARE X'FF'
*
*  EXAMPLE: ("-" REPRESENTS X'00' AND "+" REPRESENTS X'FF')
*
*  INPUT DSN QUAL WAS       "ABCXEYZH"
*  GENERIC WAS SPECIFIED AS "ABC%E%%H"
*  MASK WAS CREATED AS      "+++-+--+"
*  GENERIC WAS SAVED AS     "ABC-E--H"
*  THE "AND" CHANGES DSN TO "ABC-E--H"
*  WHEN THE ABOVE TWO ARE COMPARED, THEY ARE EQUAL.
         SPACE 2
*------------------------------------------------------------*
*           DEFINE A SINGLE DSN QUALIFIER                             *
*------------------------------------------------------------*
STRQTAB  DSECT
STRQLEN  DS    H             LENGTH OF DSN QUALIFIER
STRQFLAG DS    X             DSN QUALIFIER FLAG BITS
FDQ      EQU   X'80'         THIS IS THE LAST DSN QUALIFIER
         DS    5X            UNUSED
DSNQUAL  DS    0CL8          SPACE FOR DSN QUALIFIER
STRQUAL  DS    CL(MAXSTLEN)  SPACE FOR ACCOUNT QUALIFIER
LDSNTAB  EQU   *-STRQTAB     LENGTH OF ONE ENTRY
SUBCOMP  CSECT ,   DEFINE LDSNTAB BEFORE FIRST DC/DS REFERENCE
         SPACE 1
*--------------------------------------------------------------------*
*                                                                    *
*    DYNAMIC WORK AREA                                               *
*                                                                    *
*--------------------------------------------------------------------*
DYNAREA  DSECT ,
         DS    18A           LOCAL SAVE AREA (DEBUGGING, ETC.)
         SPACE 1
DCAST2   DC    0CL44' ',C'*'  1/3
DCAST1   DC    0CL44' ',C'*'  2/3
BLANKS   DC    CL44' '        3/3
         SPACE 1
LOCWORK  DS    0D
DB       DS    D             WORK AREA
RETC     DS    F
SCANCNT  DS    F
NUMNODES DC    F'0'              NUMBER OF DSN QUALIFIERS
NUMGENS  DC    F'0'              NUMBER OF GENERIC QUALIFIERS
GENSTART DS    A                 RESTART POINT IN GENERIC TABLES
DSNSTART DS    A                 RESTART POINT IN DSN TABLES
CSWPFLGS DS    X             PROCESSING FLAGS
PFACT    EQU   X'80'           PROCESS ACCOUNT MASK
PFDSN    EQU   X'40'           PROCESS DATA SET MASK
PFMEM    EQU   X'20'           PROCESS VOLUME OR MEMBER MASK    GP08328
PFOTH    EQU   X'02'           PROCESS SOMETHING ELSE
PFTEMP   EQU   X'01'           AREA WAS GETMAINED; FREE ON EXIT
TEMPFLAG DS    X
NOGENNUM EQU   X'80'             VARIABLE NUMBER OF NODES
SPGSAVE  DC    F'0'                    RETURN ADDRESS
CDSAVE   DC    F'0'                    RETURN ADDRESS
GCSAVE   DC    16F'0'                  REGS SAVED HERE
LVLMAX   DS    H             8 FOR DSN; 44 FOR ACT; 256 OTH
LVLMIN   DS    H             1 FOR DSN; 0 FOR REST
LVLSEP   DS    C             . OR , FOR LEVEL SEPARATOR
GENNODE  DC    XL(MAXSTLEN)'00' SPACE FOR GENERIC QUALIFIER/WORK AREA
GENNLEN  DC    F'0'              LENGTH OF GENERIC QUALIFIER ABOVE
COMPAREA DS    CL(MAXSTLEN)
FUNPARM  DS    CL3
DSNPARM  DS    CL(MAXSTLEN)  LOGICALLY 44 FOR DSN AND ACT REQUESTS
GENPARM  DS    CL(MAXSTLEN)
DTAB     DC    XL256'00'
GTAB     DC    XL256'00'
         SPACE 1
DSNTABS  DS    (NUMSTACK)XL(LDSNTAB)  SPACE FOR DSN/ACT QUALIFIERS
GENTABS  DS    (NUMSTACK)XL(LGENTAB)  SPACE FOR MASK QUALIFIERS
WORKEND  DS    0D        END OF DYNAMIC WORK AREA
LOCWKLEN EQU   *-LOCWORK  SIZE TO CLEAR EVERY ENTRY
GWORKLEN EQU   *-DYNAREA  SIZE TO GET/RELEASE
         SPACE 1
         PRINT NOGEN
         YREGS ,
         END ,
@@
//SYSGO DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(CYL,1),DISP=(MOD,PASS)
//SUBTREE EXEC ASMFC,PARM='DECK,LIST,OBJECT'
//SYSLIB DD DSN=SYS1.MACLIB,DISP=SHR
//       DD DSN=SYS2.PVTMACS,DISP=SHR
//       DD DSN=SYS2.ESPMAC,DISP=SHR
//       DD DSN=SYS1.AMACLIB,DISP=SHR
//       DD DSN=SYS1.AMODGEN,DISP=SHR
//SYSIN DD DATA,DLM=@@
SUBTREE  TITLE 'S U B T R E E  ***  SORTED TREE FUNCTIONS'
         MACRO ,
         NODEF &PFX=NODE
&PFX.SECT DSECT ,            DEFINE ONE NODE
&PFX.BASE DS    0A   1/3     BASE OF LESS/MORE ARRAY
&PFX.LESS DS    A    2/3     ADDRESS OF LOWER NODE OR 0
&PFX.MORE DS    A    3/3     ADDRESS OF HIGHER NODE OR 0
&PFX.DELT DS    X            SUBTREE DELTA (RIGHT-LEFT COUNT)
&PFX.FLAG DS    X            STATUS FLAG
         AIF   ('&PFX' NE 'NODE').NOFLGS
NFDELETE EQU   X'80'           ENTRY LOGICALLY DELETED
NFFIND   EQU   X'40'           FIND ONLY
.NOFLGS  ANOP  ,
&PFX.REC  DS    0C           RECORD
&PFX.SIZE EQU   *-&PFX.SECT  FIXED NODE OVERHEAD (+ RECORD LENGTH)
         MEND  ,
         SPACE 1
         COPY  OPTIONGB
         SPACE 2
         SYSPARM LIST=YES
         SPACE 1
***********************************************************************
**                                                                   **
**  SUBTREE:  SUBROUTINES FOR BUILDING A SORTED, BALANCED TREE,      **
**    UPDATING INFORMATION, AND SEQUENTIAL LISTING                   **
**                                                                   **
**  R1  - POINTS TO REQUEST LIST:                                    **
**    +0  ADDRESS OF CHARACTER REQUEST:GET, FIND, UPDATE, LIST, END  **
**    +4  ADDRESS OF SERVTREE PARAMETER AREA                         **
**    +8  ADDRESS OF INCOMING KEY (FIND), RECORD (GET,UPD)           **
**                                                                   **
**  R14 - RETURN ADDRESS                                             **
**                                                                   **
***********************************************************************
**                                                                   **
**    ON RETURN:                                                     **
**                                                                   **
**  R15 - CONDITION CODE  0 FOUND; GOOD COMPLETION                   **
**                        4 RECORD BUILT BY GET                      **
**                        8 NOT FOUND; END OF LIST; ETC.             **
**                       12 ERROR COMPLETING THE REQUEST             **
**                       16 BAD PARAMETER LIST; INVALID SERVTREE     **
**                                                                   **
**  R1  - ADDRESS OF CURRENT RECORD, OR 0                            **
**                                                                   **
***********************************************************************
         PRINT &PRTSOR
SUBTREE  START 0
SUBTREE  AMODE ANY
SUBTREE  RMODE ANY
         B     BEGIN-*(,R15)
         YREGS ,
BEGID    BCON  'SUBTREE &SYSDATE '
BEGIN    STM   R14,R12,12(R13) SAVE ALMOST EVERYTHING
         LR    R12,R15       GET BASE
         USING SUBTREE,R12   DECLARE IT
         LA    R1,0(,R1)     CLEAR POSSIBLE HIGH BIT
         LTR   R5,R1         SAVE ENTRY PARM POINTER
         BZ    ERROR16       INVALID CALL
         L     R11,4(,R1)    GET PRESUMED SERVTREE AREA
         LA    R11,0(,R11)   CLEAR HIGH BIT                     GP03149
         LTR   R11,R11         AND SET CC                       GP03149
         BZ    ERROR16       INVALID CALL
         USING ROOTSECT,R11  DECLARE USER'S PARAMETER AREA
         CLC   ROTID,BEGID+4  DOES IT MATCH 'TREE' ?
         BNE   ERROR16       NO; INVALID CALL
         L     R4,0(,R5)     LOAD REQUEST ADDRESS
         LA    R4,0(,R4)     CLEAR HIGH BIT                     GP03149
         LTR   R4,R4           AND SET CC                       GP03149
         BZ    ERROR16       INVALID CALL
         SR    R6,R6         SIGNAL NO RECORD POINTER PASSED
         TM    4(R5),X'80'   SHORT LIST WITH VL ?
         BNZ   TESTWORK      YES; GET SAVE AREA
         L     R6,8(,R5)     LOAD RECORD/KEY POINTER
TESTWORK ICM   R10,15,ROTWORK  LOAD AND TEST MY WORK AREA
         BNZ   HAVEWORK      OK
         LA    R3,SAVEEND-SAVE  SIZE OF LOCAL WORK AREA
         STORAGE OBTAIN,LENGTH=(R3)  GET A WORK AREA
         ST    R1,ROTWORK    STASH IT
         LR    R10,R1        COPY TO WORKING REGISTER
         LR    R2,R1         COPY FOR CLEARING
         SR    R15,R15       ZERO FILL AND LENGTH
         MVCL  R2,R14        CLEAR GOTTEN AREA
HAVEWORK ST    R13,4(,R10)   BACK LINK
         ST    R10,8(,R13)   FORWARD LINK
         LR    R13,R10       MAKE NEW SAVE AREA POINTER
         USING SAVE,R13      DECLARE IT
         B     WHAT
DOGPATCH DC    128S(*)            MODULE PATCH AREA
         SPACE 1
*---------------------------------------------------------------------*
*  FATAL CALLING ERROR - NO WORK AREA; NO NEW R13; JUST SET & RETURN  *
*---------------------------------------------------------------------*
ERROR16  LM    R14,R12,12(R13)  RELOAD CALLER'S REGISTERS
         LA    R15,16        SET FATAL ERROR
         SR    R1,R1         NO RECORD
         BR    R14           RETURN TO CALLER
         SPACE 2
**********************************************************************
**                                                                  **
**   FUNCTION INITIALIZATION                                        **
**                                                                  **
**********************************************************************
         PUSH  USING
WHAT     NI    PROFLAGS,255-PFFIND-PFNEW   RESET FIND-ONLY/NEW-CELL
         TM    PROFLAGS,PFINIT  INITIALIZATION DONE ?
         BNZ   WHAT2DO
         L     R0,=C'KL=0'   ANY KEY ?
         SR    R14,R14       CLEAR FOR IC
         ICM   R14,1,ROTKEYL   KEY LENGTH
         BZ    RETEIGHT      NO; SIGNAL KEY LENGTH REQUIRED
         STH   R14,KEYLEN    LOCAL COPY
         LH    R15,ROTRECL   RECORD LENGTH
         STH   R15,RECLEN    LOCAL COPY
         L     R0,=C'KEY>'   KEY LARGER THAN RECORD
         CR    R14,R15       KEY NO LONGER THAN RECORD?
         BH    RETEIGHT      NO; SIGNAL KEY TOO LONG
         AH    R14,ROTKEYO   KEYLENGTH+OFFSET
         L     R0,=C'KYO>'   KEY+OFFSET LARGER THAN RECORD
         CR    R14,R15       RECORD LONG ENOUGH ?
         BH    RETEIGHT      NO; ERROR                          GP03149
         LH    R14,ROTKEYO   KEY OFFSET
         STH   R14,KEYOFF    LOCAL COPY
         LA    R15,NODESIZE(,R15)  RECLEN PLUS NODE OVERHEAD
         ST    R15,CELLSIZE  SAVE IT
         MVC   EXNPCLC(6),PATPCLC  MOVE PRIMARY COMPARE
         MVC   EXNSCLC(6),PATSCLC  MOVE SECONDARY COMPARE
         ICM   R0,3,EXNPCLC+4     GET (BASE)OFFSET
         ALR   R0,R14             MAKE NEW OFFSET
         STCM  R0,3,EXNPCLC+4     AND STASH IT BACK
         ICM   R0,3,EXNSCLC+4     GET (BASE)OFFSET
         ALR   R0,R14             MAKE NEW OFFSET
         STCM  R0,3,EXNSCLC+4     AND STASH IT BACK
         LH    R14,KEYLEN
         BCTR  R14,0         CHANGE KEY LENGTH FOR EXECUTE
         STC   R14,EXNPCLC+1 AND PUT INTO CLC
         STC   R14,EXNSCLC+1 AND PUT INTO CLC
         OI    PROFLAGS,PFINIT  INITIALIZATION DONE ?
WHAT2DO  CLC   =C'GET',0(R4)      REQUEST TO LOCATE/CREATE A RECORD?
         BE    DOGET              YES
         CLC   =C'FIN',0(R4)      REQUEST TO LOCATE RECORD
         BE    DOFIND             YES
         CLC   =C'LOC',0(R4)      REQUEST TO LOCATE BY KEY?
         BE    DOLOC              YES
         CLC   =C'UPD',0(R4)      LOCATE AND UPDATE FROM R1?
         BE    DOUPDATE           YES
         CLC   =C'ADD',0(R4)      REQUEST TO ADD A RECORD?      GP05105
         BE    DOUPDATE           YES                           GP05105
         CLC   =C'LIS',0(R4)      LIST A RECORD?
         BE    DOLIST             YES
         CLC   =C'NEX',0(R4)      RETRIEVE NEXT SEQUENTIAL RECORD
         BE    DONEXT             YES
         CLC   =C'END',0(R4)      TERMINATE PROCESSING?
         BE    DOEND              YES
         CLC   =C'INI',0(R4)      REQUEST TO INITIALIZE?
         BE    RETZERO            YES; RETURN 0 IN R15/R1
         L     R0,=C'^REQ'        REASON - REQUEST
         B     RETEIGHT           RETURN REASON CODE
         SPACE 1
RETZERO  SR    R1,R1         NO RECORD ADDRESS RETURNED
RETINR1  SR    R0,R0         CLEAR REASON CODE
         SR    R15,R15       CLEAR RETURN CODE
         TM    PROFLAGS,PFNEW     NEW CELL ASSIGNED ?           GP09157
         BNZ   RETFOUR       YES; INDICATE NEED FOR DATA        GP09157
         B     RETREASN      RETURN TO CALLER WITH R15, R0, R1
         SPACE 1
RETEIGHT LA    R15,8         SET PROBABLE ERROR
         B     RETREASN      RETURN WITH R15
         SPACE 1
RETFOUR  LA    R15,4         SET WARNING
*NEXT*   B     RETREASN      RETURN WITH R15
         SPACE 1
RETREASN L     R13,4(,R13)        GET CALLER'S SAVE AREA
         L     R14,12(,R13)       LOAD RETURN
         LM    R2,R12,28(R13)     RETURN R15, R0, AND R1
         BR    R14                RETURN TO CALLER
         SPACE 2
*---------------------------------------------------------------------*
*                                                                     *
*  END REQUESTED :                                                    *
*    1) FREE CELL POOL, IF ANY GOTTEN                                 *
*    2) FREE GOTTEN WORK AREA                                         *
*    3) RETURN                                                        *
*                                                                     *
*---------------------------------------------------------------------*
DOEND    TM    PROFLAGS,PFPOOL    WAS THERE A CELL POOL ?
         BZ    DOENDPL       NO
         L     R0,MYPOOLID   GET CELL POOL ID
         CPOOL DELETE,CPID=(0)   FREE ALL OF IT
         NI    PROFLAGS,255-PFPOOL  SHOW POOL RESET
         SPACE 1
DOENDPL  LA    R3,SAVEEND-SAVE
         LR    R1,R13        SAVE CURRENT WORK AREA
         L     R13,4(,R13)   GET CALLER'S SAVE AREA
         STORAGE RELEASE,LENGTH=(R3),ADDR=(1)  RELEASE WORK AREA
         XC    ROTHEAD(ROTWORK-ROTHEAD+L'ROTWORK),ROTHEAD       GP05135
         LM    R14,R12,12(R13)  RELOAD EVERYTHING
         SR    R15,R15       SET GOOD RETURN
         SR    R0,R0         NO REASON
         SR    R1,R1         NO RECORD
         BR    R14           AND RETURN
         POP   USING
         SPACE 2
*---------------------------------------------------------------------*
*                                                                     *
*  MOSTLY COMMON CODE FOR:                                            *
*    1) LOC  - LOCATE A KEY; RETURN R1=0 IF NOT FOUND                 *
*    2) FIND - LOCATE AN ENTRY (CONTAINING KEY); R1=0 IF NOT FOUND    *
*    3) GET  - LOCATE AN ENTRY; BUILD AN ENTRY IF NOT FOUND           *
*              RETURNS RECORD ADDRESS IN R1; CALLER MUST COMPLETE     *
*    4) UPD  - LOCATE AN ENTRY; REPLACE FROM USER'S RECORD            *
*                                                                     *
*  THE USER MAY PASS THE KEY OR RECORD ADDRESS AS THE 3RD CALL PARM.  *
*  WHEN OMITTED OR ZERO, THE SERVTREE EXPANDED KEY AREA IS USED       *
*                                                                     *
*---------------------------------------------------------------------*
DOLOC    LTR   R6,R6         DID THE USER PASS THE KEY ADDRESS ?
         BZ    DOFIND        NO; TREAT AS FIND FROM ROOT/REC AREA
         SH    R6,KEYOFF     FAKE IT TO LOOK LIKE RECORD
DOFIND   MVI   REQUEST,RQFIND  SHOW FIND ONLY
         OI    PROFLAGS,PFFIND  ALSO SET FLAG
         B     GOLOOK        LOOK FOR RECORD
         SPACE 1
DOGET    MVI   REQUEST,RQGET SHOW GET/BUILD A RECORD
         B     GOLOOK        LOOK FOR RECORD
         SPACE 1
DOUPDATE MVI   REQUEST,RQUPD  SHOW UPDATE ONLY
*NEXT*   B     GOLOOK        LOOK FOR RECORD
         SPACE 1
GOLOOK   LTR   R10,R6        DID THE USER PASS A RECORD ADDRESS?
         BNZ   GOLOOKDF      YES
         LA    R6,ROTREC     ELSE USE THE ONE IN THE PARAMETER AREA
         LR    R10,R6        COPY FOR KEY ADDRESS               GP05196
GOLOOKDF AH    R10,KEYOFF    POINT TO THE KEY
         BAS   R14,KEYLOOK   LOCATE THE RECORD
         LTR   R2,R1         WAS A RECORD FOUND ?
         BZ    RETFOUR       NO; RETURN NOT FOUND
         LA    R1,NODEREC-NODESECT(,R1)  SKIP NODE HEADER
         IC    R0,REQUEST    GET THE REQUEST
         BIX   VAL=(R0),     BRANCH BY TYPE                            *
               LOC=(RETINR1,     FIND ONLY                             *
               GTGET,            GET OR BUILD A RECORD                 *
               GTUPD)            UPDATE THE RECORD
         SPACE 1
GTGET    OI    PROFLAGS,PFLTREE  SHOW GET LAST
         NI    PROFLAGS,255-PFLLIST-PFLSTND  RESET LIST MODE
         B     RETINR1       RETURN RECORD ADDRESS
         SPACE 1
GTUPD    OI    PROFLAGS,PFLTREE  SHOW GET LAST
         NI    PROFLAGS,255-PFLLIST-PFLSTND  RESET LIST MODE
         LH    R3,RECLEN     GET RECORD
         LR    R14,R6        USER'S RECORD AREA
         LR    R15,R3        AND LENGTH
         LR    R2,R1         DEST. IS RECORD ADDRESS            GP03264
         MVCL  R2,R14        UPDATE MY RECORD FROM USER'S AREA
         B     RETINR1       RETURN RECORD ADDRESS
         SPACE 2
*---------------------------------------------------------------------*
*                                                                     *
*  CONTROL CODE FOR SEQUENTIAL LOOKUP:                                *
*    1) LIST - STARTS A NEW LIST REQUEST                              *
*    2) NEXT - WHEN PFLSTND SET, RETURNS CODE 8 AND 0 ADDRESS         *
*              WHEN PFLLIST NOT SET, STARTS A NEW LIST REQUEST        *
*              ELSE GETS ANOTHER ENTRY, OR SETS R1 TO 0               *
*                                                                     *
*---------------------------------------------------------------------*
         SPACE 1
DONEXT   TM    PROFLAGS,PFLSTND   PRIOR END OF LIST ?
         BNZ   RETEIGHT           YES; TOO BAD
         L     R4,ROT@REC         GET ADDRESS OF LAST RECORD
         TM    PROFLAGS,PFLLIST   DID WE START SEQUENTIAL RETRIEVAL ?
         BNZ   GOLIST             YES
         SPACE 1
DOLIST   NI    PROFLAGS,255-PFLTREE-PFLSTND  NOT IN TREE MODE
         OI    PROFLAGS,PFLLIST   SHOW LIST MODE
         SR    R4,R4              SIGNAL FIRST ENTRY
*NEXT*   B     GOLIST
         SPACE 1
GOLIST   SUBCALL SEQLOOK          GET FIRST/NEXT SEQUENTIAL RECORD
         ST    R1,ROT@REC         RETURN AND SAVE RECORD ADDRESS
         LTR   R2,R1              ANY ?
         BZ    GOLISTND           NO MORE
         LH    R3,RECLEN     GET RECORD
         LA    R14,ROTREC    USER'S RECORD AREA
         LR    R15,R3        AND LENGTH
         MVCL  R14,R2        MAKE A COPY IN USER'S AREA
         B     RETINR1       AND RETURN
GOLISTND OI    PROFLAGS,PFLSTND   SHOW END OF LIST
         B     RETFOUR       AND SET WARNING
         SPACE 2
***********************************************************************
**                                                                   **
**  SUBROUTINE TO SEARCH AND UPDATE A BALANCED TREE                  **
**                                                                   **
**  INPUT:  R9 - ROOT OF TREE (MAPPED BY SERVTREE MACRO)             **
**          XXXKEY - SEARCH KEY                                      **
**          XXXFLAG - 0 TO INSERT KEY IF IT DOESN'T EXIST            **
**          XXXFLAG - XXXFGRD TO RETURN 0 OR LEAF ADDRESS            **
**  OUTPUT: R1 - 0 OR NEW/REQUESTED LEAF ADDRESS.                    **
**          FOR NEW LEAF, DATA AFTER KEY ARE ZEROED.                 **
**                                                                   **
**  ADAPTED FROM D.E. KNUTH 'THE ART OF PROGRAMMING' VOL 3, SECT 6.2 **
**                                                                   **
***********************************************************************
         PUSH  USING
KEYLOOK  STM   R0,R15,SUBSAVE1  RETURN MATCHED NODE IN R1       GP05013
PTRP     EQU   R4
PTRQ     EQU   R5
PTRR     EQU   R6
PTRS     EQU   R7
PTRT     EQU   R8
         LA    PTRT,ROTHEAD  POINT TO (PHONY) TOP
*OLD*NP  USING NODESECT,PTRP
*OLD*NQ  USING NODESECT,PTRQ
*OLD*NR  USING NODESECT,PTRR
*OLD*NS  USING NODESECT,PTRS
*OLD*NT  USING NODESECT,PTRT
         USING PODESECT,PTRP
         USING QODESECT,PTRQ
         USING RODESECT,PTRR
         USING SODESECT,PTRS
         USING TODESECT,PTRT
         ICM   PTRP,15,TODEMORE  GET RIGHT LINK
         LR    PTRS,PTRP     ALSO SET
         BNZ   STEP2         NOT FIRST TIME - PROCESS NORMALLY
         TM    PROFLAGS,PFFIND     READ-ONLY MODE?
         BNZ   KEYLESS       YES; RETURN NO MATCH
         SUBCALL GETNODE     GET A NEW NODE; RETURN ADDRESS IN R1
         LR    PTRP,R1       USE AS P
         ST    PTRP,TODEMORE  SET ROOT POINTER
         B     KEYLOOX       AND EXIT
         SPACE 1
STEP2    EX    0,EXNPCLC     TEST KEY
         BH    STEP4         HIGH - DO ON RIGHT
         BL    STEP3         LOW - DO ON LEFT
         LR    R1,PTRP       RETURN THIS NODE TO USER
         B     KEYLOOX       MATCH - RETURN FOR ANALYSIS
         SPACE 1
STEP3    ICM   PTRQ,15,PODELESS  GET LEFT POINTER
         BNZ   STEP3A        NON-ZERO
         TM    PROFLAGS,PFFIND     READ-ONLY MODE?
         BNZ   KEYLESS       YES; RETURN NO MATCH
         SUBCALL GETNODE     GET A NEW NODE
         LR    PTRQ,R1       SET AS Q
         ST    PTRQ,PODELESS ADD NEW BRANCH
         B     STEP5         JOIN COMMON
STEP3A   CLI   QODEDELT,0  BALANCED?
         BE    STEP3B        YES
         LR    PTRT,PTRP     SWAP AROUND
         LR    PTRS,PTRQ
STEP3B   LR    PTRP,PTRQ
         B     STEP2         TRY ON LOWER LEVEL
         SPACE 1
STEP4    ICM   PTRQ,15,PODEMORE  GET LEFT POINTER
         BNZ   STEP3A        NON-ZERO; GO DOWN A LEVEL
         TM    PROFLAGS,PFFIND     READ-ONLY MODE?
         BNZ   KEYLESS       YES; RETURN NO MATCH
         SUBCALL GETNODE     GET A NEW NODE
         LR    PTRQ,R1       SET AS Q
         ST    PTRQ,PODEMORE ADD NEW BRANCH
         B     STEP5         JOIN COMMON
         SPACE 1
*  NOTE: R1 HAS NODE ADDRESS FOR USER - LEAVE INTACT UNTIL KEYLOOX
STEP5    DS    0H            DONE BY GETNODE SUBROUTINE
         SPACE 1
*  NEW NODE CREATED - NOW SEE ABOUT BALANCING THIS THING
*
STEP6    LA    R15,SODELESS   POINT LEFT
         EX    0,EXNSCLC     TEST KEY
         BL    STEP6C
         LA    R15,SODEMORE
STEP6C   L     PTRP,0(R15)   LOAD LEFT OR RIGHT
         LTR   PTRR,PTRP                                        GP03149
         BZ    STEP7         ***FIX FOR 0C4?***                 GP03149
STEP6LUP EX    0,EXNPCLC     TEST KEY
         BE    STEP7
         BH    STEP6LH
         MVI   PODEDELT,X'FF'  (-1)
         L     PTRP,PODELESS
         B     STEP6LUP
STEP6LH  MVI   PODEDELT,1
         L     PTRP,PODEMORE
         B     STEP6LUP
         SPACE 1
STEP7    LA    R0,1
         LA    R15,NODEMORE-NODEBASE  INDEX TO NODEMORE
         EX    0,EXNSCLC     TEST KEY
         BNL   STEP7ST
         LNR   R0,R0
         LA    R15,NODELESS-NODEBASE  INDEX TO NODELESS
STEP7ST  STH   R0,DELTA      SAVE DELTA
         STH   R15,DELT2     SAVE OFFSET
         LCR   R0,R0         COMPLIMENT IT
         STH   R0,CURLA      SAVE THE COMPLEMENT
         LA    R0,NODEMORE-NODEBASE+NODELESS-NODEBASE  SUM
         SR    R0,R15        COMPLEMENTARY OFFSET
         STH   R0,COMP2      SAVE COMPLEMENTED DELTA
         SPACE 1
         CLI   SODEDELT,0  B(S)=0 ?
         BE    STEP7I        YES
         CLC   SODEDELT,CURLA+L'CURLA-L'NODEDELT  =-1
         BE    STEP7II       NEGATIVE
         B     STEP7III      POSITIVE
STEP7I   MVC   SODEDELT,DELTA+L'DELTA-L'NODEDELT  SET A
         LA    R15,1
         A     R15,ROTHEAD+NODELESS-NODESECT   UP COUNT
         ST    R15,ROTHEAD+NODELESS-NODESECT   UP COUNT
         B     KEYLOOX       AND EXIT
         SPACE 1
STEP7II  MVI   SODEDELT,0  BALANCED
         B     KEYLOOX       AND EXIT
         SPACE 1
STEP7III CLC   RODEDELT,DELTA+L'DELTA-L'NODEDELT  TEST A
         BE    STEP8
         CLC   RODEDELT,CURLA+L'CURLA-L'NODEDELT  TEST -A
         BE    STEP9
         EX    0,*           SHOULD NEVER GET HERE?             GP03149
         SPACE 1
STEP8    LH    R14,COMP2     GET A OFFSET COMPLEMENTED
         LH    R15,DELT2     GET A OFFSET
         LR    PTRP,PTRR
         L     R0,RODEBASE(R14)    LEFT OR RIGHT
         ST    R0,SODEBASE(R15)    UPDATE
         ST    PTRS,RODEBASE(R14)
         MVI   RODEDELT,0
         MVI   SODEDELT,0
         B     STEP10
         SPACE 1
STEP9    LH    R14,COMP2     GET A OFFSET COMPLEMENTED
         LH    R15,DELT2     GET A OFFSET
         L     PTRP,RODEBASE(R14)
         L     R0,PODEBASE(R15)
         ST    R0,RODEBASE(R14)
         ST    PTRR,PODEBASE(R15)
         L     R0,PODEBASE(R14)
         ST    R0,SODEBASE(R15)
         ST    PTRS,PODEBASE(R14)
         SPACE 1
         MVI   RODEDELT,0
         MVI   SODEDELT,0
         SPACE 1
         CLC   PODEDELT,DELTA+L'DELTA-L'NODEDELT  =A ?
         BE    STEP9A
         CLC   PODEDELT,CURLA+L'CURLA-L'NODEDELT  =A ?
         BNE   STEP9X
STEP9C   MVC   RODEDELT,DELTA+L'DELTA-L'NODEDELT  =A ?
         B     STEP9X
STEP9A   MVC   SODEDELT,CURLA+L'CURLA-L'NODEDELT  =A ?
STEP9X   MVI   PODEDELT,0
         SPACE 1
STEP10   C     PTRS,TODEMORE
         BNE   STEP10B
STEP10A  ST    PTRP,TODEMORE
         B     KEYLOOX
STEP10B  ST    PTRP,TODELESS
         B     KEYLOOX
         SPACE 1
PATPCLC  CLC   ROTREC-ROTREC(0,R10),PODEREC
PATSCLC  CLC   ROTREC-ROTREC(0,R10),SODEREC
         SPACE 1
KEYLESS  SR    R1,R1         RETURN LEAF NOT FOUND
         SPACE 1
KEYLOOX  LM   R2,R14,SUBSAVE1+2*4  RESTORE MOST                 GP05013
         BR   R14            RETURN                             GP05013
         POP   USING
         SPACE 2
***********************************************************************
**                                                                   **
**  SEQLOOK : SUBROUTINE TO RETURN NODES IN SEQUENCE                 **
**     USES TREE BUILT BY KEYLOOK                                    **
**                                                                   **
**  INPUT:  R4 FROM PREVIOUS RETURN, OR 0 TO BEGIN LIST              **
**  OUTPUT: R1 POINTS TO CURRENT NODE, OR 0 WHEN DONE                **
**                                                                   **
**  REQUIRES 4 WORD SAVE AREA                                        **
**                                                                   **
***********************************************************************
         PUSH  USING
SEQLOOK  STM   R0,R15,SUBSAVE2  RETURN MATCHED NODE IN R1       GP05013
*OLD*NP  USING NODESECT,PTRP
*OLD*NQ  USING NODESECT,PTRQ
         USING PODESECT,PTRP
         USING QODESECT,PTRQ
         LTR   R4,R4         START LIST FROM TOP?
         BNZ   SEQCONT       NO; CONTINUE
         LA    PTRS,NODESTAK  POINT TO TOP OF STACK
         XC    0(8,PTRS),0(PTRS)  CLEAR FIRST NODE
         LA    PTRS,4(,PTRS) STACK EMPTY
         LA    PTRP,ROTHEAD   POINT TO (PHONY) TOP
         ICM   PTRP,15,PODEMORE  GET RIGHT LINK
         BZ    SEQLOOX       TREE IS EMPTY
         SPACE 1
SEQLDOWN ICM   PTRQ,15,PODELESS  IS THERE A LEFT BRANCH ?
         BZ    SEQLNLFT      NO; PRESENT THIS ONE TO USER
         ST    PTRP,0(,PTRS) STACK THIS NODE
         XC    4(4,PTRS),4(PTRS)  CLEAR FOR DEBUGGING
         LA    PTRS,4(,PTRS)  PUSH THE STACK
SEQRDOWN LR    PTRP,PTRQ     SWAP NODES
         B     SEQLDOWN
         SPACE 1
SEQCONT  LM    R4,R8,LISTSAVE RESTORE OLD REGISTERS
         ICM   PTRQ,15,PODEMORE  IS THERE A RIGHT BRANCH?
         BNZ   SEQRDOWN      YES; DOWN IT
SEQPOP   SH    PTRS,=H'4'    POP THE STACK
         LR    PTRQ,PTRP     SAVE OLD ADDRESS
         ICM   PTRP,15,0(PTRS)  GET HIGHER NODE
         BZ    SEQLOOX       QUIT IF ON TOP
         C     PTRQ,PODEMORE  DID WE DESCEND BY RIGHT NODE?
         BE    SEQPOP        YES; POP IT AGAIN
         B     SEQLOOX       ELSE PROCESS KEY FOR THIS NODE
         SPACE 1
SEQLNLFT NOP   0             RETURN THIS NODE
         SPACE 1
SEQLOOX  STM   R4,R8,LISTSAVE SAVE FOR RESTART
         LTR   R1,R4         ALL DONE ?
         BZ    *+8           YES; RETURN ZERO
         LA    R1,NODEREC-NODESECT(,R4)  RETURN RECORD ADDRESS
         LM   R2,R14,SUBSAVE2+2*4  RESTORE MOST                 GP05013
         BR   R14            RETURN                             GP05013
         POP   USING
         SPACE 2
***********************************************************************
**                                                                   **
**  GETNODE : SUBROUTINE TO OBTAIN STORAGE FOR ONE NODE.             **
**     SPACE IS CLEARED TO ZERO, AND KEY IS INSERTED.                **
**                                                                   **
**  LOCATION IS RETURNED IN R1                                       **
**                                                                   **
***********************************************************************
         PUSH  USING
GETNODE  STM   R0,R15,SUBSAVE3  R1 UPDATED WITH NEW INFORMATION GP05013
         TM    PROFLAGS,PFPOOL  FIRST TIME HERE ?
         BNZ   GETNFILL      NO
         SPACE 1
         L     R4,CELLSIZE   GET CELL SIZE
         ICM   R5,15,ROTCOUNT  ESTIMATED CELL NUMBER
         BNZ   GETNBILD      PROCEED
         L     R5,=F'1000'   DEFAULT TO ONE THOUSAND (?)
GETNBILD CPOOL BUILD,PCELLCT=(R5),SCELLCT=(R5),CSIZE=(R4),             *
               CPID=MYPOOLID,MF=(E,MYCELL)
         OI    PROFLAGS,PFPOOL  FIRST TIME DONE                 GP03150
         SPACE 1
GETNFILL CPOOL GET,CPID=MYPOOLID,REGS=SAVE   GET A CELL
         L     R3,CELLSIZE   GET CELL SIZE AGAIN
         LR    R2,R1         COPY CELL ADDRESS
         SR    R15,R15       CLEAR FOR MVCL
         MVCL  R2,R14        CLEAR IT
         USING NODESECT,R1
         LH    R15,KEYLEN    GET KEY LENGTH
         LH    R2,KEYOFF     GET KEY OFFSET
         LA    R14,NODEREC(R2)  POINT TO KEY IN RECORD
         BCTR  R15,0         LENGTH FOR EXECUTE
         EX    R15,EXKEYMVC  MOVE KEY TO RECORD
         OI    PROFLAGS,PFNEW     SHOW NEW CELL ASSIGNED        GP09157
         LM    R2,R14,SUBSAVE3+2*4  RESTORE MOST                GP05013
         BR    R14            RETURN                            GP05013
         SPACE 1
EXKEYMVC MVC   0(0,R14),0(R10)  MOVE REQUESTED KEY +
         POP   USING
         SPACE 1
         LTORG ,                                                GP15145
         SPACE 1
**** ARBITRARY DEFINITION SUPPLIED FOR CLEAN ASSEMBLY
ROOTSECT DSECT ,
         SERVTREE PFX=ROT,RECLEN=32,KEYLEN=4,KEYOFF=0 (PATTERN ONLY)
         SPACE 1
         NODEF PFX=NODE      MAIN DEFINITIONS
         NODEF PFX=PODE      SECONDARIES
         NODEF PFX=QODE      SECONDARIES
         NODEF PFX=RODE      SECONDARIES
         NODEF PFX=SODE      SECONDARIES
         NODEF PFX=TODE      SECONDARIES
         SPACE 1
SAVE     DSECT ,
         DS    18A           GENERAL SAVE AREA
LISTSAVE DS    8A            SAVE AREA FOR LIST BUILDING
SUBSAVE1 DS    16A           IN LIEU OF BAKR/PR                 GP05013
SUBSAVE2 DS    16A           IN LIEU OF BAKR/PR                 GP05013
SUBSAVE3 DS    16A           IN LIEU OF BAKR/PR                 GP05013
DB       DS    D             CONVERSION WORK AREA
DB2      DS    D             CONVERSION WORK AREA
BINDCLC  EQU   DB2,6,C'I'    DEFINE DYNAMIC INSTRUCTION
EXNPCLC  CLC   ROTREC(0),NODEREC-NODEREC
EXNSCLC  CLC   ROTREC(0),NODEREC-NODEREC
CELLSIZE DS    F             CELL SIZE
MYPOOLID DS    F             CPOOL TOKEN
KEYOFF   DS    H             KEY OFFSET
KEYLEN   DS    H             KEY LENGTH
RECLEN   DS    H             RECORD LENGTH
DELTA    DS    H             WORKING DELTA
DELT2    DS    H               AS OFFSET
CURLA    DS    H             COMPLEMENTARY DELTA
COMP2    DS    H               AS OFFSET
         SPACE 1
PROFLAGS DC    X'00'         PROCESSING FLAGS
PFLTREE  EQU   X'80'           LAST REQUEST WAS TREE (GET/FIND/UPD)
PFLLIST  EQU   X'40'           LAST REQUEST WAS LIST (LIST/NEXT)
PFLSTND  EQU   X'20'           LIST EXHAUSTED
PFFIND   EQU   X'10'           KEY LOOK - FIND ONLY
PFNEW    EQU   X'04'           KEY LOOK - NEW CELL BUILT        GP09157
PFPOOL   EQU   X'02'           CELL POOL BUILT
PFINIT   EQU   X'01'           ENTRY CHECKING DONE
         SPACE 1
REQUEST  DC    X'00'         CURRENT REQUEST
RQFIND   EQU   0               FIND A RECORD (RETURNS 0 OR ADDRESS)
RQGET    EQU   1               LOCATE OR BUILD A RECORD
RQUPD    EQU   2               UPDATE SPECIFIED RECORD
RQLIST   EQU   3               LIST FIRST/NEXT RECORD
         SPACE 1
MYCELL   CPOOL BUILD,MF=L    SPACE FOR CELL POOL BUILD REQUEST
         SPACE 1
NODESTAK DS    25A           NODE PROCESSING STACK
SAVEEND  EQU   *
         SPACE 1
         END   ,
@@
//SYSGO DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(CYL,1),DISP=(MOD,PASS)
//SUBVERB EXEC ASMFC,PARM='DECK,LIST,OBJECT'
//SYSLIB DD DSN=SYS1.MACLIB,DISP=SHR
//       DD DSN=SYS2.PVTMACS,DISP=SHR
//       DD DSN=SYS2.ESPMAC,DISP=SHR
//       DD DSN=SYS1.AMACLIB,DISP=SHR
//       DD DSN=SYS1.AMODGEN,DISP=SHR
//SYSIN DD DATA,DLM=@@
SUBVERB  TITLE 'S U B V E R B  ***  VERB TABLE LOOKUP'
         COPY  OPTIONGB
         SPACE 1
         SYSPARM LIST=YES
         SPACE 2
***********************************************************************
*                                                                     *
*   SUBVERB COMPARES A VERB ENTRY (ADDRESS IN R0, LENGTH=8)           *
*     TO FIND A MATCH IN A TABLE (ADDRESS OF BXLE POINTERS IN R1:     *
*     A(FIRST ENTRY,ENTRY LENGTH,LAST ENTRY)                          *
*                                                                     *
*   THE VERB IS COMPARED TO EACH TABLE ENTRY, AND MAY BE ABBREVIATED  *
*     TO THE MINIMUM LENGTH SPECIFIED ON THE TABLE ENTRY              *
*   THE ROUTINE RETURNS 0 WHEN THERE IS NO MATCH, OTHERWISE THE       *
*     MATCHED TABLE ENTRY ADDRESS IS PASSED IN R15 (IND.AD. IN R1)    *
*     (NOTE THAT THE ADDRESS IN R1 IS MEANINGFUL PROVIDED THE OFFSETS *
*      0-12 MATCH IN PARKEYAD AND PARKEYFG EXPANSIONS)                *
*                                                                     *
*   THE TABLE BXLE POINTER MAY BE CREATED USING THE PARKEYBX MACRO.   *
*   TABLES MAY BE CREATED WITH PARKEYAD AND PARKEYFG MACROS,          *
*   PROVIDING ALL ENTRIES IN A TABLE ARE THE SAME LENGTH              *
*                                                                     *
***********************************************************************
*  MAINTENANCE:                                                       *
*                                                                     *
*                                                                     *
*                                                                     *
***********************************************************************
*                                                                     *
*        COPYRIGHT 2003  EXPERT SYSTEM PROGRAMMING                    *
*                        176 OLD STAGE COACH ROAD                     *
*                        BRADFORD, VT 05033-8844                      *
*                                                                     *
*                    ALL RIGHTS RESERVED                              *
*                                                                     *
***********************************************************************
         SPACE 1
SUBVERB  START 0
SUBVERB  AMODE ANY           USE CALLER'S AMODE
SUBVERB  RMODE ANY           USE CALLER'S RMODE
         SPACE 1
         B     BEGIN-SUBVERB(,R15)
         BCON  'SUBVERB &SYSDATE'
BEGIN    STM   R14,R12,12(R13)  SAVE A BIT
         LR    R12,R15       COPY BASE ADDRESS
         USING SUBVERB,R12   DECLARE IT
         SPACE 1
*---------------------------------------------------------------------*
*  CHECK INPUT POINTERS; ON ERROR R15=0, R0=100+ERROR CODE            *
*---------------------------------------------------------------------*
         LR    R6,R0         COPY KEYWORD ADDRESS
         LA    R6,0(,R6)     CLEAN IT
         LA    R0,100        SET REASON
         LTR   R6,R6
         BZ    BADRET15      NO KEYWORD ADDRESS
         LA    R0,101        SET REASON
         LA    R1,0(,R1)     CLEAN BXLE ADDRESS
         LTR   R1,R1
         BZ    BADRET15      NO TABLE ADDRESS
         LM    R3,R5,0(R1)   GET TABLE POINTERS
         LA    R0,103        SET REASON
         LA    R3,0(,R3)     CLEAN BXLE ADDRESS
         LTR   R3,R3
         BZ    BADRET15      NO FIRST ENTRY ADDRESS
         LA    R0,104        SET REASON
         LA    R4,0(,R4)     CLEAN BXLE ADDRESS
         LTR   R4,R4
         BZ    BADRET15      NO ENTRY LENGTH
         LA    R0,105        SET REASON
         LA    R5,0(,R5)     CLEAN BXLE ADDRESS
         LTR   R5,R5
         BZ    BADRET15      NO END ADDRESS
         LOCBYTE 0(R6),LEN=8,W2=(R7)  LOOK FOR BLANK IN KEYWORD
         SR    R7,R14        GET KEYWORD LENGTH
         SPACE 1
*---------------------------------------------------------------------*
*  LOOK FOR A MATCH                                                   *
*---------------------------------------------------------------------*
VERBLOON LOCBYTE OFO@TXT(R3),LEN=8,WK=R15,W2=R14  GET FIELD LENGTH
         SR    R14,R15       GET LENGTH
         BNP   VERBLOOX      SKIP BAD ENTRY
         CR    R14,R7        COMPARE TO ACTUAL
         BL    VERBLOOX        TOO LONG FOR MATCH
         LR    R14,R7        START WITH GOTTEN LENGTH
         BCTR  R14,0         CHANGE TO EXECUTE LENGTH
         CLM   R14,1,OFO@MIN(R3)  CHECK AGAINST DESIGN MINIMUM
         BL    VERBLOOX      TOO SHORT; DO ANOTHER WORD
VERBLOOP EX    R14,VERBLCLC  COMPARE TO REQUEST
         BE    VERBLOAD      FOUND
         SH    R14,=H'1'     DECREASE LENGTH
         BM    VERBLOOX        BUT NOT TOO MUCH
         CLM   R14,1,OFO@MIN(R3)  CHECK AGAINST DESIGN MINIMUM
         BNL   VERBLOOP      TRY WITH SHORTER VERSION >= MIN
VERBLOOX BXLE  R3,R4,VERBLOON  TRY ANOTHER WORD
         LA    R0,804        SET NOT FOUND
         B     BADRET15      RETURN UNMATCHED
VERBLCLC CLC   OFO@TXT(0,R3),0(R6)  CHECK FOR VERB MATCH
         SPACE 1
*---------------------------------------------------------------------*
*  MATCHED; PUT ENTRY ADDRESS IN R15; AND SET FLAG/BRANCH ADDR IN R1  *
*---------------------------------------------------------------------*
VERBLOAD SR    R1,R1         JUST IN CASE
         ICM   R1,3,OFO@ADD(R3)  LOAD FIELD OFFSET
         LR    R2,R1         MAKE A COPY
         N     R1,=X'00000FFF'  ISOLATE OFFSET
         SRL   R2,12-2       ISOLATE REGISTER OFFSET
         N     R2,=X'0000003C'  MAKE REGISTER OFFSET MASK
         BZ    VERBABSO      ABSOLUTE OFFSET (?)
         C     R2,=A(13*4)   IS IT REGISTER 13 ?
         BE    VERBAB13      YES; SPECIAL
         BH    VERBABSO      DON'T DO FOR R14-R15
         AL    R1,20(R2,R13)  RELOCATE R1-R12
         B     VERBABSO
VERBAB13 ALR   R1,R13        (IN DYNAMIC SAVE/WORK AREA?)
VERBABSO LR    R15,R3        RETURN MATCHED ENTRY
         B     COMMRET       VECTORED RETURN - SUCCESS
         SPACE 1
BADRET15 SR    R15,R15
COMMRET  L     R14,12(,R13)  LOAD RETURN ADDRESS
         LM    R2,R12,28(R13)  LOAD REMAINING REGISTERS
         BR    R14           RETURN TO CALLER
         SPACE 1
UNWANTED DSECT ,
         PARKEYAD 'KEYWORD',COMMRET       DEFINE OFFSETS
         PARKEYFG 'KEYWORD',COMMRET,1,2   DEFINE OFFSETS
         SPACE 1
         YREGS ,             REGISTER MNEMONICS
         END   ,
@@
//SYSGO DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(CYL,1),DISP=(MOD,PASS)
//SUBVTVAL EXEC ASMFC,PARM='DECK,LIST,OBJECT'
//SYSLIB DD DSN=SYS1.MACLIB,DISP=SHR
//       DD DSN=SYS2.PVTMACS,DISP=SHR
//       DD DSN=SYS2.ESPMAC,DISP=SHR
//       DD DSN=SYS1.AMACLIB,DISP=SHR
//       DD DSN=SYS1.AMODGEN,DISP=SHR
//SYSIN DD DATA,DLM=@@
VTVAL    TITLE 'S U B V T V A L  ***  GET FMT4 VALUES FOR UCB'
         COPY  OPTIONGB
         SPACE 1
         SYSPARM LIST=YES
         SPACE 1
         PRINT &PRTSOR
SUBVTVAL PGMHEAD ZERO15,BASE=R12,AM=ANY,RM=ANY,PARM=R3
         USING UCBOB,R3                                         GP13007
         TM    UCBID,X'CC'   REAL OR COPIED UCB ?
         BNO   PGMEXIT       NEITHER; FAIL
         LA    R6,DVCTYPMK   GET TYPE MASK
         N     R6,UCBTBYT1   RETAIN ONLY TBYT4
         BAL   R9,DVTBLOC    GET DEVICE TABLE ENTRY
         CH    R15,=H'8'     VALID ?
         BNL   PGMEXIT       NO, RETURN ALL ZEROES
         USING DVCT,R6       DECLARE DEVICE CHARACTERISTICS
         SPACE 1
         XC    STARLING,STARLING  CLEAR LIST, JUST IN CASE
         TRKCALC FUNCTN=TRKCAP,DEVTAB=(R6),REGSAVE=YES,R=1,K=44,DD=96, *
               MF=(E,STARLING)
         BXH   R15,R15,PGMEXIT
         STC   R0,RSNCODE+2
         TRKCALC FUNCTN=TRKCAP,DEVTAB=(R6),REGSAVE=YES,R=1,K=8,DD=256, *
               MF=(E,STARLING)
         BXH   R15,R15,PGMEXIT
         STC   R0,RSNCODE+3
         SPACE 1
         MVC   RETCODE+2(2),DVCTRKLN   RAW TRACK SIZE
         ICM   R0,3,DVCTRK             TRACKS PER CYLINDER
         ICM   R14,12,UCBVTOC          GET VTOC F4 ADDRESS
         SRDL  R14,16+32               PREPARE FOR DIVIDE
         DR    R14,R0                  TRACKS / CYLINDER
         STCM  R15,12,RR1CODE          SET CYLINDER ADDRESS
         STH   R14,RR1CODE+2             AND TRACK
PGMEXIT  PGMEXIT COPYRET=(RETCODE,12)  RETURN R15, R0, R1
         SPACE 2
*---------------------------------------------------------------------*
*   SUBROUTINE TO LOCATE DEVICE TABLE ENTRY                           *
*---------------------------------------------------------------------*
         SPACE 1
DVTBLOC  SR    R4,R4         CLEAR FOR TEST OFFSET
DVTBLOCN LTR   R5,R6         COPY AND TEST DEVICE INDEX
         BNP   DVTBLOCM      MAJOR ERROR
         CH    R5,=Y(DVCTYPMK)  IN RANGE OF VALID DEVICES ?
         BH    DVTBLOCM      NO; FAIL IT
*        THIS CRUTTY CODE IS REQUIRED TO TEST IF THE SUB-TYPE IS
*        PRESENT IN THE SYSTEM (ALTERNATIVE IS A LOOP THROUGH THE
*        UCBS AND A DEVICE COMPARE)
*
         L     R6,CVTPTR     GET THE CVT
         L     R6,CVTZDTAB-CVTMAP(,R6)  GET THE DASD DEVICE TABLE
         USING DVCTI,R6      DECLARE THE DVCT INDEX MAPPING
         LA    R14,DVCTIOFF  POINT TO FIRST OFFSET FIELD
         LR    R15,R5        NUMBER OF DESIRED ENTRY
         B     DVTBLOTU      SKIP FIRST BYTE (GARBAGE ?)
DVTBLOTL CLI   0(R14),0      FILLER BYTE ?
         BE    DVTBLOTU      YES; SKIP IT
         CLM   R5,1,DVCTIOFF-DVCTI(R14)  OFFSET LOWER THAN TYPE ?
         BNL   DVTBLOTN      YES; DEVICE NOT IN SYSTEM
DVTBLOTU LA    R14,1(,R14)
         BCT   R15,DVTBLOTL  TRY NEXT ENTRY
         IC    R4,DVCTIOFF(R5)  GET CORRESPONDING OFFSET FOR THIS
         LTR   R4,R4         FILLER ENTRY ?
         BNZ   DVTBLOTS      NO; USE IT
DVTBLOTN LA    R6,LOCZDTAB   POINT TO LOCAL TABLE
         SLL   R5,1          DOUBLE (LOCAL IS AL2)
         LA    R5,DVCTIOFF(R5)
         ICM   R4,3,0(R5)    LOAD OFFSET
         BNP   DVTBLOCM      NO; ERROR
         LA    R15,4         RETURN UNSUPPORTED, BUT VALID DEVICE
DVTBLOTS AR    R6,R4         POINT TO DEVICE ENTRY
         BR    R9            RETURN TO CALLER
DVTBLOCM LA    R15,12        SET MAJOR ERROR
         BR    R9            RETURN TO CALLER
         SPACE 1
         LTORG ,
         SPACE 1
         IHADVCT ,           DEVICE TABLE
DVZNOCYL EQU   X'80'         DVCFLAGS: CYL # INVALID (3340)
         AIF   (&MVSSP).HAVMODU
DVCMODU  EQU   X'10'         DVCFLAGS - MODULO DEVICE
DVCMOD1  EQU   DVCBPSEC+2,2,C'A'  MODULUS
DVCOVH1  EQU   DVCBPSEC+4,2,C'A'  OVERHEAD VALUE 1
DVCOVH2  EQU   DVCBPSEC+6,2,C'A'  OVERHEAD VALUE 2
.HAVMODU SPACE 1
SAVE     DSECT ,
         SERVDEFS ,
         SPACE 1
STARLING SERVCALC MF=L       MAP THE TRKCALC PARM LIST
SAVEEND  EQU   *
SUBVTVAL CSECT ,             RESTORE MAIN CONTROL SECTION
         PRINT NOGEN
         SPACE 2
*   COPY OF DEVICE CHARACTERISTICS IN CASE THEY AREN'T IN THE
*   RUNNING SYSTEM.
*
LOCZDTAB DC    AL2(0,0,0,0)       UNSUPPORTED TYPES 0-3
         AIF   (&MVS).NEWDEV                                    GP09116
         DC    AL2(0)                  2302                04
         AGO   .COMDEV
.NEWDEV  DC    AL2(DVZ9345-LOCZDTAB)   9345                04
.COMDEV  DC    AL2(0)             UNSUPPORTED TYPES 5
         DC    AL2(DVZ2305A-LOCZDTAB)  2305-1              06
         DC    AL2(DVZ2305B-LOCZDTAB)  2305-2              07
         DC    AL2(DVZ2314-LOCZDTAB)   2314                08
         DC    AL2(DVZ3330-LOCZDTAB)   3330 SINGLE         09
         DC    AL2(DVZ3340-LOCZDTAB)   3340                0A
         DC    AL2(DVZ3350-LOCZDTAB)   3350                0B
         DC    AL2(DVZ3375-LOCZDTAB)   3375                0C
         DC    AL2(DVZ333D-LOCZDTAB)   3330 DOUBLE         0D
         DC    AL2(DVZ3380-LOCZDTAB)   3380                0E
         DC    AL2(DVZ3390-LOCZDTAB)   3390                0F
*        DC    (DVCTYPMK-*-LOCZDTAB+1)AL2(0)      ---SPARES---   10-1F
         DC    (31-15+1)AL2(0)
         SPACE 1
*                 CYL TRK LEN OHDNKFG TOL ALT  R0TSDS B/S MOD
*        DC    CL6'2311'
*VZ2311  DC    X'00CB000A0E29511414010219001E'
*        DC    CL6'2301'
*VZ2301  DC    X'000100C85003BA35350002000000'
*        DC    CL6'2303'
*VZ2303  DC    X'0050000A131C9226260002000000'
*        DC    CL6'2302'
*VZ2302  DC    X'00FA002E137851141401021900C8'
*        DC    CL6'2321'
*VZ2321  DC    X'140A051407D06410100302190190'
         DC    CL6'9345'
DVZ9345  DC    X'05A0000FBC98000000100230000F04A0D50000EE0023'
         DC    CL6'2305A'
DVZ2305A DC    X'0030000838E8027ACA080200000100EA5A5700A8'
         DC    CL6'2305B'
DVZ2305B DC    X'006000083A0A01215B08020000010076B4B10054'
         DC    CL6'2314'
DVZ2314  DC    X'00CB00141C7E922D2D010216003C00000000'  0-RPS
         DC    CL6'3330'
DVZ3330  DC    X'019B0013336DBFBF38000200008500ED807C0069'
*        DC    CL6'3340'
*VZ3340  DC    X'015D000C2157F2F24B800200000C0125403D008C'
         DC    CL6'3340'
DVZ3340  DC    X'02BA000C2157F2F24B80020000180125403D008C'
         DC    CL6'3350'
DVZ3350  DC    X'0230001E4B36010B5208020000960185807B009C'
         DC    CL6'3375'
DVZ3375  DC    X'03C0000C8CA000E0001000BF000C0340C4BB00C00020'
         DC    CL6'3330-1'
DVZ333D  DC    X'032F0013336DBFBF38000200008500ED807C0069'
         DC    CL6'3380'
DVZ3380  DC    X'0376000FBB6001000010010B000F04E0DDD600E00020'
         DC    CL6'3390'
DVZ3390  DC    X'0459000FE5A2000000100220000F0594E00001100022'
*                 0 1 2 3 4 5 6 7 8 9 A B C D E F 0 1 2 3 4 5
         SPACE 2
         PRINT NOGEN
         IEFUCBOB ,
         CVT ,
         END
@@
//SYSGO DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(CYL,1),DISP=(MOD,PASS)
//SUBXTSUM EXEC ASMFC,PARM='DECK,LIST,OBJECT'
//SYSLIB DD DSN=SYS1.MACLIB,DISP=SHR
//       DD DSN=SYS2.PVTMACS,DISP=SHR
//       DD DSN=SYS2.ESPMAC,DISP=SHR
//       DD DSN=SYS1.AMACLIB,DISP=SHR
//       DD DSN=SYS1.AMODGEN,DISP=SHR
//SYSIN DD DATA,DLM=@@
SUBXTSUM TITLE 'S U B X T S U M  ***  ADD UP DSCB EXTENT SIZES'
         COPY  OPTIONGB
         SPACE 1
         SYSPARM LIST=YES
         SPACE 2
***********************************************************************
**                                                                   **
**  SUBXTSUM - ACCUMULATE CONSECUTIVE EXTENT SIZES                   **
**                                                                   **
**  R0 - NUMBER OF EXTENTS IN GROUP                                  **
**  R1 - ADDRESS OF FIRST EXTENT                                     **
**  R2 - NUMBER OF TRACKS PER CYLINDER FOR THIS DEVICE               **
**                                                                   **
**  R15 - 0 ON ERROR, ELSE NUMBER OF TRACKS                          **
**                                                                   **
***********************************************************************
SUBXTSUM START 0
         USING SUBXTSUM,R15
         B     BEGIN
         DC    AL1(17),CL17'SUBXTSUM &SYSDATE'
BEGIN    STM   R14,R12,12(R13)   SAVE CALLER'S REGISTERS
         DROP  R15
         LR    R12,R15
         USING SUBXTSUM,R12
         SPACE 1
         SR    R15,R15       ZERO SIZE TO DATE
         SR    R9,R9         CLEAR FOR ICM
         USING MAPEXTNT,R1   DECLARE EXTENT MAPPING
DVEXTSUB CLI   XTWHOLE,0     UNUSED EXTENT ?
         BE    PGMEXIT       YES; TOO BAD
         SR    R11,R11       CLEAR FOR ICM
         ICM   R11,3,XTHICYL   GET END CYLINDER
         ICM   R9,3,XTLOCYL    AND START CYLINDER
         SR    R11,R9        LESS START CYLINDER
         BM    PGMEXIT       FAIL BAD RANGE
         LH    R5,XTHITRK    GET HIGH TRACK
         SH    R5,XTLOTRK    LESS START TRACK
         AH    R5,=Y(1)      PLUS ONE FOR RELATIVITY
         CLI   XTTYPE,XTTSPLIT  SPLIT EXTENT ?
         BNE   DVEXTSEQ      NO
         AH    R11,=Y(1)     GET CYLINDER COUNT
         MR    R4,R11        GET TOTAL TRACKS
         AR    R15,R5        ADD TO RUNNING TOTAL
         B     DVEXTSUP      GET NEXT EXTENT
DVEXTSEQ MR    R10,R2        CONVERT TO TRACKS
         AR    R11,R5        ADD ODD TRACKS
         AR    R15,R11       ACCUMULATE
DVEXTSUP LA    R1,XTLEN(,R1)   GET NEXT EXTENT
         BCT   R0,DVEXTSUB   DO NEXT EXTENT
         SPACE 1
PGMEXIT  L     R14,12(,R13)  GET RETURN ADDRESS
         LM    R0,R12,20(R13)  RESTORE OTHER REGISTERS
         BR    R14           RETURN TO CALLER
         SPACE 2
         PRINT &PRTMAC
         MAPEXTNT ,          MAP AN EXTENT ENTRY
         SPACE 2
         PRINT &PRTSYS
         YREGS ,
         END   ,
@@
//SYSGO DD DSN=&&OBJSET,UNIT=SYSDA,SPACE=(CYL,1),DISP=(MOD,PASS)
//DSSDUMP EXEC ASMFCL,PARM='NODECK,NOLIST,OBJECT'
//SYSLIB DD DSN=SYS1.MACLIB,DISP=SHR
//       DD DSN=SYS2.PVTMACS,DISP=SHR
//       DD DSN=SYS2.ESPMAC,DISP=SHR
//       DD DSN=SYS1.AMACLIB,DISP=SHR
//       DD DSN=SYS1.AMODGEN,DISP=SHR
//SYSIN DD DATA,DLM=@@
DSSDUMP  TITLE 'D S S D U M P  ***  DATA SET DUMP IN ADRDSSU FORMAT'
         PUNCH '   SETCODE AC(1) '
         PUNCH ' ORDER DSSDUMP(P) '   ***** MAKE DUMPS EASIER *****
         SPACE 1
         COPY  OPTIONGB
         SPACE 1
         SYSPARM LIST=YES    SET LOCAL OPTIONS
         SPACE 1
***********************************************************************
*                                                                     *
*    DSSDUMP - QUICK AND DIRTY SUBSTITUTE FOR MVS 3.8J USERS          *
*                                                                     *
*    PROGRAM IS AUTHORIZED, AND REENTRANT (but not all subroutines)   *
*    REQUIRED DD CARDS:                                               *
*      SYSPRINT - PROCESSING LOG                                      *
*      SYSIN    - REQUESTS:                                           *
*             Note that all reserved words may be abbreviated to the  *
*             first three characters (e.g., OPT for OPTIONS)          *
*        OPTIONS {ENQ,NOENQ,TEST,DEBUG,NODEBUG}   <anwhere in SYSIN>  *
*                {ALLDATA,EXPORT,LIST,SHOW}       <anwhere in SYSIN>  *
*             ENQ       issues an exclusive ENQ TEST for each data    *
*                       set. Dump continues if DS not available,      *
*                       and issues RC=4. Applies from prior DUMP on.  *
*             NOENQ     (default). Dumps DS as is.                    *
*             ALLDATA × ALLEXCP   Causes all tracks, including unused *
*                       ones, to be written out. May be needed to     *
*                       recover from invalid, non-zero LSTAR.         *
*                       This option also preserves LABEL=SUL data.    *
*             EXPORT    modifies the output DSCB1 by removing any     *
*                       expiration date and password flags.           *
*             TEST      bypasses all TAPE output. Note that file has  *
*                       been opened already, and will be empty.       *
*             DEBUG     -not implemented-                             *
*             NODEBUG   -not implemented-                             *
*             LIST × SHOW   - display current OPTION settings.        *
*        INCLUDE mask × DUMP mask - one or more dump requests per run *
*             mask      specifies a data set name (unquoted). If it   *
*                       contains an asterisk, question mark, or       *
*                       percent sign, it is treated as a mask.        *
*                       A name ending in a period is treated as a     *
*                       mask followed by an implied **                *
*                       In MVS (VSAM catalog), the first index level  *
*                       may not contain a mask character. Also note   *
*                       that VSAM catalog processing does not support *
*                       detection or processing of alias entries.     *
*                       Note that a percent sign is treated as a      *
*                       positional mask (one to one correspondence    *
*                       of characters/mask to dsname).                *
*                       Any number of DUMP cards may be used in a run *
*                       but only about 700 data sets will fit in the  *
*                       name table (assembly option)                  *
*        DUMP mask VOLUME(serial)   Processes matching data sets on   *
*                       that volume serial only. If this results in   *
*                       duplicate data set names, a .D#nnnnnn is      *
*                       appended to duplicates on higher volume       *
*                       serials (i.e., the cataloged entry may be the *
*                       one that gets renamed). Masking bytes are     *
*                       valid in any position in the mask.            *
*        EXCLUDE mask        (optional) follows relevant DUMP card    *
*             mask      as above. Excludes matching data sets chosen  *
*                       by the previous DUMP/INCLUDE/SELECT card.     *
*        PREFIX name    causes all data set names to be prefixed by   *
*                       the specified text string. It is not required *
*                       to be an index level (e.g., SYS9.), but if    *
*                       not, generated names may be syntactically     *
*                       invalid. Result names are truncated to 44     *
*                       characters, and a trailing period is blanked. *
*                       Only one PREFIX card may be used per run,     *
*                       and it is mutually exclusive of RENAME and    *
*                       STRIP options.                                *
*        STRIP name     The specified string is removed from any DSN  *
*                       where it is found. Multiple STRIP and RENAME  *
*                       requests are supported (up to 16; ASM option) *
*        RENAME oldname newname   The specified string is replaced in *
*                       any DSN found and replaced by prefix newname. *
*                       Up to 16 RENAME and STRIP requests are legal. *
*                       All strings in PREFIX/RENAME/STRIP are        *
*                       limited to 23 characters (assembly option).   *
*                                                                     *
*      TAPE     - UNIT,DSN,VOLSER - dump tape(s) - in DSS format,     *
*                            uncompressed; TEST assumed if missing.   *
*        RECFM=U, BLKSIZE=65520 is the default for tape output.       *
*        RECFM=U  7892<block size<65520 is supported.                 *
*        RECFM=V  is supported, with blocks 7900<block size<32760     *
*                                                                     *
*        For DASD output, the range is 7892<data size<32760, with     *
*        the default being either the track size (if >=7892) or the   *
*        half-track size for modulo devices.                          *
*                                                                     *
*      PARM=EXPORT is the only supported PARM OPTION at present.      *
*                                                                     *
*                                                                     *
*    DSSDUMP MAY BE INVOKED AUTHORIZED FROM ANOTHER AUTHORIZED        *
*    PROGRAM, USING A PARM, AND A DD NAME LIST OVERRIDE:              *
*                                                                     *
*    LA   R1,PARMDD          COMBINED LIST                            *
*    LINK/ATTACH/SYNCH/L =V,BALR  etc. to invoke                      *
*                                                                     *
*    PARMDD  DC  A(PARMFLD),X'80',AL3(DDNMLST)                        *
*    PARMFLD DC  AL2(L'TEXT),C'parm text'                             *
*    DDNMLST DC  AL2(L'DDN),CL8'SYSIN',CL8'SYSPRINT',CL8'TAPE'        *
*        unwanted override positions may be blank or zero             *
*                                                                     *
***********************************************************************
*                                                                     *
*   The mapping in the DSSBLOCK DSECT adapted from material supplied  *
*   by Charlie Brint, the DSSREST author, at OS_390@hotmail.com       *
*                                                                     *
***********************************************************************
*                                                                     *
*        COPYRIGHT 2009  EXPERT SYSTEM PROGRAMMING                    *
*                        176 OLD STAGE COACH ROAD                     *
*                        BRADFORD, VT 05033-8844                      *
*                        gerhard (at) postpischil (dot) com          *
*                    ALL RIGHTS RESERVED                              *
*                                                                     *
*   Not-for-profit use permitted, and distribution rights granted to  *
*   Hercules and CBT projects.                                        *
***********************************************************************
         PRINT &PRTSOR
DSSDUMP  PGMHEAD ZERO31,BASE=(R11,R12),PARM=R9,BNDRY=PAGE,AM=24,RM=24
         L     R10,=A(DSSDUMPD)   GET SUBROUTINES AND STATIC DATA
         USING DSSDUMPD,R10  DECLARE SUBROUTINE AREA
         MVC   DYNIOWK(DYNIOLEN),PATIOWK    INIT I/O WORK AREAS GP09194
         TM    0(R9),X'80'   NORMAL OS PARM ?                   GP09194
         BNZ   SKIPORID      YES; NO DD NAME LIST               GP09194
         L     R14,0(,R1)    LOAD PARM POINTER                  GP09247
         CLI   2(R14),0      POSSIBLE TSO CP PARM ?             GP09247
         BE    SKIPORID      YES; NO DD NAME LIST               GP09247
         L     R1,4(,R9)     GET DD NAME LIST                   GP09194
         LA    R1,0(,R1)     CLEAN                              GP09194
         LTR   R1,R1         TEST                               GP09194
         BZ    SKIPORID      NULL LIST                          GP09194
         LH    R15,0(,R1)    GET PURPORTED LENGTH               GP09194
         LTR   R15,R15       NULL OR INVALID ?                  GP09194
         BM    BADPARM       INVALID                            GP09194
         BZ    SKIPORID      NULL LIST                          GP09194
         CH    R15,=H'24'    ROOM FOR THREE NAMES?              GP09194
         BL    TWOORID       NO                                 GP09194
         CLI   18(R1),C' '   ANYTHING THERE ?                   GP09194
         BNH   TWOORID       NO; IGNORE                         GP09194
         MVC   SYSDDNM,18(R1)   MOVE TAPE DD OVERRIDE           GP09194
TWOORID  CH    R15,=H'16'    ROOM FOR TWO NAMES?                GP09194
         BL    ONEORID       NO                                 GP09194
         CLC   10(8,R1),ZEROES  ANYTHING THERE ?                GP09194
         BE    ONEORID       NO; IGNORE                         GP09194
         CLC   10(8,R1),BLANKS  ANYTHING THERE ?                GP09194
         BE    ONEORID       NO; IGNORE                         GP09194
         MVC   SYSPRINT(8),10(R1)   MOVE SYSPRINT DD OVERRIDE   GP09194
ONEORID  CH    R15,=H'8'     ROOM FOR ONE NAME ?                GP09194
         BL    SKIPORID      NO                                 GP09194
         CLC   10(8,R1),ZEROES  ANYTHING THERE ?                GP09194
         BE    SKIPORID      NO; IGNORE                         GP09194
         CLC   10(8,R1),BLANKS  ANYTHING THERE ?                GP09194
         BE    SKIPORID      NO; IGNORE                         GP09194
         MVC   SYSIN(8),2(R1)   MOVE SYSIN DD OVERRIDE          GP09194
         B     SKIPORID      RESUME NORMAL CODE                 GP09194
BADPARM  WTO   'DSSDUMP: INVALID PARM OR DD NAME LIST'          GP09194
         ABEND 1234,DUMP                                        GP09194
         SPACE 1
SKIPORID PARMLOAD R8         LOOK AT PARM FIELD                 GP09247
         LTR   R9,R9         ANY TEXT?                          GP09247
         BNP   DONEPARM      USE IT                             GP09197
SKIPPRLP CLI   0(R8),C' '    CP SPACER ?                        GP09247
         BNE   SKIPPRCM      NO                                 GP09197
         LA    R8,1(,R8)     SKIP ONE SPACER                    GP09197
         BCT   R9,SKIPPRLP   TRY AGAIN                          GP09247
SKIPPRCM CH    R9,=H'6'      PARM=EXPORT ?                      GP09197
         BL    FAILPARM      NO; IGNORE                         GP09197
         OI    OPTFLAGS,FGEXP     (PRE)SET FOR EXPORT           GP09197
         CLC   =C'EXPORT',0(R8)  REALLY EXPECTED PARM?          GP09197
         BE    DONEPARM      USE IT                             GP09197
FAILPARM WTO   'DSSDUMP: INVALID PARM IGNORED'                  GP09197
         SPACE 1
DONEPARM LA    R8,RECREC     USE RECORD AREA IN MACRO EXPANSION
         USING CELLSECT,R8   DECLARE IT
         SERVINIT ,          INITIALIZE SUPPORT SERVICES
         SERVLOAD @INPREAD,@PARSER,@PRINTER              DYNAMIC
         SERVLOAD SUBCAT,SUBCOMP,SUBTREE,SUBVERB,SUBVTVAL,LFETCH=LINK
         MVC   #SUBCAT,@UBCAT    COPY                           GP13007
         MVC   #SUBCOMP,@UBCOMP  COPY                           GP13007
         PRTOPEN SYSPRINT,OPT=(ABEND,WTO)   MUST HAVE
         PRTL  '#  Version 0.97       DSS Format Data Set Dump',TITLE=1
*DEFER*  BANDAID INIT        *****DEBUG*****
*DEFER*  BANDAID SPIE        *****DEBUG*****
*---------------------------------------------------------------------*
*   WE NEED TO RUN AUTHORIZED FOR @VOLREAD'S DEB USE                  *
*---------------------------------------------------------------------*
         TESTAUTH FCTN=1     ARE WE AUTHORIZED ?
         BXLE  R15,R15,HAVEAUTH   YES; WHOOPPEE
         PRTL  '*** DSSDUMP must run authorized ***'
         PRTCLOSE ,
         ABEND 047,DUMP
         SPACE 1
*---------------------------------------------------------------------*
*   @VOLREAD USES A //VOLMOUNT DD DUMMY.  IF USER DIDN'T SUPPLY,      *
*     WE BUILD ONE HERE, BUT NEVER UNALLOCATE IT.                     *
*---------------------------------------------------------------------*
HAVEAUTH SERVCALL TIODD,DCVOLMNT  DID USER SUPPLY A VOLMOUNT DD ?
         LTR   R0,R0         ANY ADDRESS RETURNED ?
         BNZ   HAVEVMNT
         MVC   BUFTAPE(PATALLEN),PATALLOC   COPY ALLOCATION REQUEST
         LA    R1,BUFTAPE    POINT TO IT
         ST    R1,DB
         OI    DB,X'80'
         LA    R1,DB         PASS PRB
         DYNALLOC ,          ALLOCATE IT
*   NOTE THAT I NEVER UNALLOCATE - TOO BAD
         LTR   R15,R15
         BZ    HAVEVMNT      SUCCESSFUL
         ST    R15,DB
         PRTL  '*** No VOLMOUNT DD DUMMY found ***',NL,CC=NO
 PRTDATA '*** Dynamic Allocation failed: R15=',(DB,I,PADR),' Reason=',(*
               BUFTAPE+4,2,HEX,PADR),' Info=',(BUFTAPE+6,2,HEX)
         SPACE 1
*---------------------------------------------------------------------*
*   WE NEED TO KEEP THE FILE INFORMATION IN ORDER BY VOLUME SERIAL    *
*     AND DSNAME (PROCESSING ALL FILES PER VOLUME IS FASTER)          *
*   RATHER THAN SORTING STUFF, WE USE TREE ALGORITHM, BASED ON ONE    *
*   IN DONALD KNUTH'S ART OF COMPUTER PROGRAMMING, VOLUME 3.          *
*---------------------------------------------------------------------*
HAVEVMNT MVC   ROOTBALL(RECSIZE),ROOTPATT   INIT TREE STUMP
         SUBCALL INITREE     BUILD THE DUMP REQUEST TREE
         LTR   R15,R15       GOOD ?
         BZ    HAVETREE      OK
         PRTDATA '*** Fatal error - SUBTREE routine failed ***'
         B     PGMEXIT8      NO; QUIT
         SPACE 2
*---------------------------------------------------------------------*
*   INITIALIZE THE @VOLREAD ROUTINE. WE DO THIS NOW TO AVOID          *
*     STORAGE FRAGMENTATION PROBLEMS LATER.                           *
*---------------------------------------------------------------------*
HAVETREE VOLREAD LOAD        LOAD THE VOLUME READ ROUTINE
         SPACE 1
*---------------------------------------------------------------------*
*   OUTPUT GOES TO DDNAME TAPE. IF MISSING, WE RUN IN TEST MODE ONLY. *
*   1) SET DEFAULT BUFFER OF 65520 {<=32760 FOR DASD & RECFM=V(B)}    *
*   2) GET THE TIOT ENTRY AND CHECK THE UNIT TYPE                     *
*   3) GET THE JFCB - FAIL INVALID RECFM; ACCEPT BLKSIZE= OVERRIDE    *
*   4) USE EXCP FOR TAPE, AND BSAM FOR DASD                           *
*   5) BUILD APPROPRIATE CONTROL BLOCKS                               *
*   6) FOR BSAM OPEN, ALLOW JFCB/DSCB MERGE BLKSIZE OVERRIDE.         *
*   7) SET BLKSIZE DEPENDENT VALUES                                   *
*   N.B. ADRDSSU REQUIRES 7892 AS A MINIMUM SIZE                      *
MINBLOCK EQU   7892          SO WE SET IT HERE                  GP09207
*---------------------------------------------------------------------*
         LA    R3,BUFTAPE    GET TAPE OUTPUT BUFFER
         ST    R3,@BUF       SAVE BUFFER START
         ST    R3,@BUFCUR    SAVE AVAILABLE ADDRESS
         L     R4,=A(65520)  GET MAXIMUM BUFFER SIZE
         ST    R4,#BUFMAX    PROVISIONALLY SAVE MAXIMUM BUFFER SIZE
         LA    R0,BUFTAPE(R4)     PHYSICAL MEMORY LIMIT
         ST    R0,@MEMEND    SAVE FOR DSN TABLE
         SERVCALL TIODD,SYSDDNM   FIND TAPE DD                  GP09194
         LTR   R2,R0         FOUND IT ?
         BZ    TAPEDDNO      NO
         USING TIOENTRY,R2   DECLARE IT                         GP09207
         SERVCALL RJFCB,SYSDDNM   LOOK FOR JFCB                 GP09194
         BXH   R15,R15,TAPEDDNO
         USING INFMJFCB,R1                                      GP09207
         SR    R14,R14
         ICM   R14,3,JFCLRECL     CHECK LRECL                   GP09207
         ICM   R15,3,JFCBLKSI     CHECK BLOCK SIZE              GP09207
         MVC   DUMPDSN,JFCBDSNM   SAVE FOR VALIDITY CHECK       GP09317
         MVC   DUMPVOL,JFCBVOLS     (CAN'T DUMP OPEN DS)        GP09317
         CLI   JFCRECFM,0         UNSPECIFIED? -> U             GP09207
         BE    TAPERFUU
         CLI   JFCRECFM,X'C0'     RECFM=U ?                     GP09207
         BE    TAPERFUU
         TM    JFCRECFM,255-(DCBRECV+DCBRECBR+DCBRECSB) V(B)(S) GP09207
         BZ    TAPERFVV      NO
         DROP  R1                                               GP09207
 PRTDATA '*** Output DD',(SYSDDNM,DEB,PAD),'RECFM= not supported ***'
         B     PGMEXIT8
TAPERFVV MINH  R4,=H'32760'  SET RECFM=V DATA MAXIMUM
         OI    OPTFLAGS,FGVAR     SET VARIABLE MODE
         LTR   R14,R14       ANY RECORD LENGTH?
         BZ    TAPERVNL      NO
         LA    R14,4(,R14)   ALLOWANCE FOR RDW
TAPERVNL LTR   R15,R15       ANY BLOCK SIZE ?
         BZ    TAPELROK      NO; USE DEFAULT
         LTR   R14,R14       ANY RECORD LENGTH?
         BZ    TAPELROK      NO
         MIN   R15,(R14)     USE SMALLER VALUE
         B     TAPELROK
TAPERFUU LTR   R14,R14       ANY LRECL ?
         BZ    TAPELROK
         CR    R14,R15       SAME AS BLOCK SIZE?
         BE    TAPELROK
 PRTDATA '*** Output DD',(SYSDDNM,DEB,PAD),'LRECL ignored ***'
TAPELROK LTR   R15,R15       ANY BLOCK SIZE ?
         BZ    TAPEDFLT
         MIN   R4,(R15)      USE SMALLER VALUE
TAPEDFLT ICM   R6,7,TIOEFSRT      GET UCB                       GP09207
         BZ    TAPEDDNO      NONE
         DROP  R2            DONE WITH TIOT                     GP09207
         USING UCBOB,R6      HAVE UCB                           GP09207
         CLI   UCBTBYT3,UCB3TAPE                                GP09207
         BE    TAPEDDEX      REALLY TAPE
         MINH  R4,=H'32760'  SET DASD MAXIMUM
         CLI   UCBTBYT3,UCB3DACC  DASD ?                        GP09207
         BNE   TAPENU        ELSE DEVICE TYPE UNSUPPORTED
         IC    R15,UCBTBYT4  GET DEVICE TYPE                    GP09207
         N     R15,=X'0000000F'             MASK WHAT I KNOW
         BZ    TAPENU        ?
         SLL   R15,1         CONVERT TO HALFWORD OFFSET
         LH    R1,DASDSIZE-2(R15)           GET PREFERRED VALUE
         CH    R1,=AL2(MINBLOCK)  TOO SMALL FOR MINIMUM?        GP09207
         BL    DASD2WEE      YES; REGRETS                       GP09207
         MIN   (R4),(R1)     USE SMALLER OF DEFAULT OR JFCB     GP09201
         MVC   TAPEDCB(TAPELEN),PATTAPE   INIT TAPE DCB
         MVC   DCBDDNAM-IHADCB+TAPEDCB(8),SYSDDNM  UPDATE DDN   GP09194
 PRTDATA '   === Using BSAM mode for output DD',(SYSDDNM,DEB,PAD),'==='
         B     TAPEDDCM      YES; PROCEES
         SPACE 1
DASD2WEE PRTDATA '*** Output DD',(SYSDDNM,DEB,PAD),'not supported due t*
               o small track size ***'                          GP09207
         B     PGMEXIT8                                         GP09207
         SPACE 1
DASDSIZE DC    H'3625,20483,4892,27998,6144,14136,14660,7294' 2311-2314
         DC    H'13030,8368,19069,17600,13030,23476,27998' 3330-3390
         SPACE 1
TAPENU PRTDATA '*** Output DD',(SYSDDNM,DEB,PAD),'neither TAPE nor DASD*
                - TEST mode forced ***'
         OI    OPTFLAGS,FGTEST    SET TEST MODE
         B     TAPEDDCM
         DROP  R6                                               GP09207
TAPEDDEX MVC   TAPEDCB(TAPEXLEN),PATEXCP   INIT FOR EXCP MODE
         MVC   DCBDDNAM-IHADCB+TAPEDCB(8),SYSDDNM  UPDATE DDN   GP09194
         XC    DUMPVOL,DUMPVOL    INVALIDATE DUMP DSN CHECK     GP09317
         LA    R2,TAPEIOB    FOR EASIER SETTINGS
         USING IOBSTDRD,R2
         MVI   IOBFLAG1,IOBDATCH+IOBCMDCH   COMMAND CHAINING IN USE
         MVI   IOBFLAG2,IOBRRT2
         LA    R1,TAPEECB
         ST    R1,IOBECBPT
         LA    R1,TAPECCW
         ST    R1,IOBSTART   CCW ADDRESS
         ST    R1,IOBRESTR   CCW ADDRESS
         LA    R1,TAPEDCB
         ST    R1,IOBDCBPT   DCB
         LA    R0,1          SET BLOCK COUNT INCREMENT
         STH   R0,IOBINCAM
         DROP  R2
         MVC   TAPECCW+1(3),@BUF+1  WRITE FROM BUFFER
         TM    OPTFLAGS,FGTEST    SET TEST MODE
         BNZ   TAPEDDCM
 PRTDATA '   === Using EXCP mode for output DD',(SYSDDNM,DEB,PAD),'==='
         B     TAPEDDCM
TAPEDDNO PRTDATA '*** Output DD',(SYSDDNM,DEB,PAD),'not found - running*
                in TEST mode ***'
         OI    OPTFLAGS,FGTEST    SET TEST MODE
TAPEDDCM ST    R4,#BUFMAX    SAVE MAXIMUM BUFFER SIZE
         CH    R4,=AL2(MINBLOCK)   IS ADRDSSU MINIMUM?          GP09207
         BNL   TAPEGBLK      YES
 PRTDATA '*** Output block size',(#BUFMAX,I,PAD),'too small ***'
         B     PGMEXIT8
TAPEGBLK TM    OPTFLAGS,FGTEST    SET TEST MODE
         BNZ   TAPESETZ      DONE WITH TAPE; BUILD SIZES
         MVI   OCLIST,X'8F'  OPEN OUTPUT
         LA    R2,TAPEDCB    POINT TO DCB
         OPEN  ((R2),OUTPUT),MF=(E,OCLIST)  OPEN TAPE
         TM    DCBOFLGS-IHADCB(R2),DCBOFOPN
         BNZ   TAPEIMRG      CHECK FOR OPEN EXIT UPDATE
     PRTL  '*** Output DD won''t open - running in TEST mode ***',CC=NO
         OI    OPTFLAGS,FGTEST    SET TEST MODE
         B     TAPESETZ
TAPEIMRG TM    DCBMACR1-IHADCB+TAPEDCB,DCBMRECP  EXCP MODE?
         BNZ   TAPESETZ      USING EXCP
         TM    DCBRECFM-IHADCB+TAPEDCB,X'80'     RECFM=U ?
         BNZ   TAPESETZ
         OI    OPTFLAGS,FGVAR     RECFM=V
         SPACE 1
TAPESETZ L     R4,#BUFMAX    GET MERGED SIZE
         TM    OPTFLAGS,FGVAR     USING RECFM=V?
         BZ    TAPESETX      NO                                 GP09201
         SH    R4,=H'8'      ALLOW FOR BDW AND RDW
TAPESETX L     R3,@BUF
         AR    R3,R4         GET END ADDRESS
         ST    R3,@BUFEND    AND SAVE IT
         LR    R15,R4        COPY BUFMAX
         SH    R15,=AL2(DTPSIZE)  CONTROL BLOCK OVERHEAD
         ST    R15,#DATMAX   MAX DATA PER BLOCK
    PRTDATA '   === Data block size adjusted to',(#BUFMAX,I,PAD),'==='
         SPACE 2
*---------------------------------------------------------------------*
*   READ A RECORD FROM SYSIN. ABEND 2540 IF NOT SUPPLIED.             *
*   READ CARDS UNTIL A DUMP TYPE IS FOUND (DUMP/INCLUDE/SELECT)       *
*     IF SO, PUT THE CURRENT CARD BACK ON THE STACK, AND PROCESS      *
*     PREVIOUS REQUEST.                                               *
*   ECHO CARD AS APPROPRIATE                                          *
*---------------------------------------------------------------------*
         INPOPEN SYSIN,OPT=(ABEND,FOLD)     UPPER-CASE INPUT
         SPACE 2
CARDNEXT INPGET ,            CONTROL CARD LOOP
         BXH   R15,R15,CARDEOD   TREAT ERROR AS EOF
         LR    R7,R0         COPY INPUT LENGTH
         LR    R6,R1         COPY INPUT ADDRESS
         LR    R3,R7         PRESERVE CARD LENGTH
         MINH  R3,=H'72'     IGNORE SEQUENCE NUMBERS
         MVI   PAR$RQFG,PAR$PARK+PAR$COUQ+PAR$COUP
         PARSE (R6),(R3),OPT=KEYWORD
         LTR   R15,R15
         BNZ   PARSFAIL
         TM    PROFLAGS,PFGDUMP   DUMP PENDING ?
         BZ    CARDPRNT           NO; OK TO PRINT
         ICM   R3,15,PAR@TABL
         BZ    CARDPRNT      ALL BLANK
         USING PRSDSECT,R3   DECLARE RESULT LIST
         LA    R0,PRS$TEXT   VERB TO BE LOCATED
         LA    R1,WAITBXLE   DEFER PRINT ?
         SUBCALL SUBVERB
         LTR   R15,R15       DID IT WORK ?
         BNZ   CARDNPRT      YES; SKIP PRINT
CARDPRNT PRTDATA (' --> ''',NL),(0(R6),(R7)),''''
CARDNPRT ICM   R3,15,PAR@TABL
         BZ    CARDNEXT      ALL BLANK
         CLI   PRS$TEXT,C'*'  COMMENT CARD ?
         BE    CARDNEXT      YES; JUST GET ANOTHER
         LA    R0,PRS$TEXT   VERB TO BE LOCATED
         LA    R1,VERBBXLE   VERB TABLE
         SUBCALL SUBVERB
         LTR   R15,R15       DID IT WORK ?
         BNZR  R1            YES; INVOKE MATCHING ADDRESS
         SPACE 1
PARSBCD  PRTL  ' *** UNRECOGNIZED CONTROL CARD ***'
         OICC  4,4,RESULT=RETCODE
         B     CARDNEXT      GET ANOTHER CARD
         SPACE 1
PARSFAIL OICC  12
         PRTL  '0*** Unexpected PARSER failure ***'
         PRTCLOSE ,          WRITE MESSAGE BEFORE DUMP
         ABEND 666,DUMP
         SPACE 1
VERBBXLE PARKEYBX VERBTAB    DEFINE PARM VALUES
         SPACE 1
WAITBXLE PARKEYBX WAITTAB    DEFINE PARM VALUES
WAITTAB  PARKEYAD 'INCLUDE ',SETDUMP   SPECIFY DS MASK
WAITTAB2 PARKEYAD 'SELECT  ',SETDUMP   SPECIFY DS MASK
         PARKEYAD 'DUMP    ',SETDUMP   BEGIN DS SELECTION
WAITTABN PARKEYAD 'COPY    ',SETDUMP   BEGIN DS SELECTION
         SPACE 1
SETALLTK OI    OPTFLAGS,FGADAT   DUMP ALL ALLOCATED TRACKS
    PRTL  ' ***** All allocated tracks will be dumped *****',MODE=DEBUG
         B     CARDNEXT      NEXT CARD
         SPACE 1
SETBUGON OI    OPTFLAGS,FGBUG  SET DEBUG MODE ON
         PRTL  ' ***** Debug mode now on *****',MODE=DEBUG
         B     CARDNEXT      NEXT CARD
         SPACE 1
SETBUGOF NI    OPTFLAGS,255-FGBUG  SET DEBUG MODE OFF
         PRTL  ' ***** Debug mode now off *****',MODE=DEBUG
         B     CARDNEXT      NEXT CARD
         SPACE 1
SETTSTON OI    OPTFLAGS,FGTEST   SET TAPE OUTPUT INHIBITED
         PRTL  ' ***** Output inhibited *****',MODE=DEBUG
         B     CARDNEXT      NEXT CARD
         SPACE 1
*---------------------------------------------------------------------*
*   SAVE RENAME/PREFIX DATA.                                          *
*     PREFIX index.          ALL DATA SET NAMES PREFIXED WITH .index  *
*     RENAME oldpfx. newpfx.   OLD PREFIX REPLACED BY NEW ONE.        *
*   NOTE THAT RENAME MAY NOT BE USED WHEN PREFIX IS SPECIFIED         *
*   IF newpfx. IS OMITTED, oldpfx. IS STRIPPED AND NOT REPLACED       *
*   A MAXIMUM OF MAXPFX RENAME CARDS MAY BE USED IN ONE RUN           *
*---------------------------------------------------------------------*
         SPACE 1
SETPFX   ICM   R3,15,PRSLINK      GET SECOND OPERAND            GP09197
         BZ    OPMISS                                           GP09197
         L     R0,PRS#TEXT        GET LENGTH                    GP09197
         CL    R0,=A(L'PFXNEW)    VALID ?                       GP09197
         BH    OP2LONG            NO                            GP09197
         OC    PRS$TEXT,BLANKS    UPPER-CASE                    GP09197
         SPACE 1                                                GP09197
         XC    PFXOLDL(PFXLEN),PFXOLDL   CLEAR RENAME STUFF     GP09197
         STC   R0,PFXNEWL         SAVE TEXT LENGTH              GP09197
         MVC   PFXNEW,PRS$TEXT    SAVE PREFIX TO BE DELETED     GP09197
         OI    PROFLAGS,PFPREF    SHOW PREFIX FOUND             GP09197
         ICM   R15,15,NUMPFX      OTHER PREFIX REQUESTS?        GP09197
         BZ    SETPFXCT           NO; SET ONE                   GP09197
         PRTV  MSGEXPFX           SHOW ERROR                    GP09197
         OICC  4             MINOR ERROR                        GP09197
SETPFXCT LA    R0,1                                             GP09197
         ST    R0,NUMPFX     FORCE ONE AND ONLY                 GP09197
         B     CARDNEXT                                         GP09197
MSGEXPFX VCON  ' *** RENAME/REPLACE IGNORED; NOT VALID WITH PREFIX ***'
         SPACE 1
SETREN   ICM   R3,15,PRSLINK      GET SECOND OPERAND            GP09197
         BZ    OPMISS                                           GP09197
         TM    PROFLAGS,PFPREF    WAS THERE A PREFIX REQUEST?   GP09197
         BZ    SETRENEW           NO; ADD THIS ONE              GP09197
         PRTV  MSGEXPFX           SHOW ERROR                    GP09197
         OICC  4             MINOR ERROR                        GP09197
         B     CARDNEXT                                         GP09197
         SPACE 1
SETRENEW L     R4,NUMPFX     GET PRIOR RENAME COUNT             GP09197
         CH    R4,=AL2(MAXPFX)  ROOM FOR MORE ?                 GP09197
         BL    SETRENAD      YES; GET ADDRESS                   GP09197
         PRTL  ' *** TOO MANY RENAME REQUESTS - IGNORED'        GP09197
         OICC  8                                                GP09197
         B     CARDNEXT                                         GP09197
         SPACE 1
SETRENAD LA    R4,1(,R4)     SET NEW COUNT                      GP09197
         ST    R4,NUMPFX     UPDATE                             GP09197
         MH    R4,=AL2(PFXLEN)    TIMES ENTRY SIZE              GP09197
         LA    R4,PFXOLDL-PFXLEN(R4)   POINT TO NEW ENTRY       GP09197
         L     R0,PRS#TEXT        GET LENGTH                    GP09197
         CL    R0,=A(L'PFXOLD)    VALID ?                       GP09197
         BH    OP2LONG            NO                            GP09197
         OC    PRS$TEXT,BLANKS    UPPER-CASE                    GP09197
         SPACE 1                                                GP09197
*TEST*   XC    PFXOLDL(PFXLEN),PFXOLDL   CLEAR RENAME STUFF     GP09197
         STC   R0,PFXOLDL         SAVE TEXT LENGTH              GP09197
         MVC   PFXOLD,PRS$TEXT    SAVE PREFIX TO BE DELETED     GP09197
         ICM   R3,15,PRSLINK      GET THIRD OPERAND             GP09197
         BZ    CARDNEXT                                         GP09197
         L     R0,PRS#TEXT        GET LENGTH                    GP09197
         CL    R0,=A(L'PFXNEW)    VALID ?                       GP09197
         BH    OP2LONG            NO                            GP09197
         OC    PRS$TEXT,BLANKS    UPPER-CASE                    GP09197
         SPACE 1                                                GP09197
         STC   R0,PFXNEWL         SAVE TEXT LENGTH              GP09197
         MVC   PFXNEW,PRS$TEXT    SAVE PREFIX TO BE PREPENDED   GP09197
         B     CARDNEXT                                         GP09197
         SPACE 1
*---------------------------------------------------------------------*
*   ACCEPT EXCLUSION MASKS FOR THE PREVIOUS DUMP REQUEST              *
*       EXCLUDE name×mask                                             *
*     MASKS END WITH A PERIOD, OR CONTAIN *, ?, OR %                  *
*     NONE OF THE ABOVE DEFINES A SIMPLE DATA SET NAME                *
*                                                                     *
*   THE ALLOWED NUMBER OF EXCLUDES IS DEFINED BY EXCMASKS REPEAT CNT  *
*---------------------------------------------------------------------*
         SPACE 1
GETXMASK TM    PROFLAGS,PFGDUMP   FOLLOWS A DUMP CARD ?
         BNZ   GETXMAS2           YEAH
 PRTL '*** Exclusion mask valid after a DUMP/SELect only ***',NL,CC=NO
         OICC  4                  MINOR ERROR?
         B     CARDNEXT
GETXMAS2 ICM   R3,15,PRSLINK      GET SECOND OPERAND
         BZ    OPMISS
         L     R0,PRS#TEXT        GET LENGTH
         CL    R0,=A(L'COMPMASK)  VALID ?
         BH    OP2LONG            NO
         ST    R0,DB              SAVE TEXT LENGTH
         OC    PRS$TEXT,BLANKS    UPPER-CASE
         SPACE 1
         INC   EXC#MASK,WORK=R15
         CH    R15,=AL2((EXC#MASK-EXCMASKS)/L'EXCMASKS)
         BNH   GETXMAS3           YES; CONTINUE
         PRTL  '*** Too many EXClude statements ***',NL,CC=NO
         OICC  4                  TOO BAD
         B     CARDNEXT
         SPACE 1
GETXMAS3 BCTR  R15,0              RELATIVE TO ZERO
         M     R14,=A(L'EXCMASKS)  (RE-USE LITERAL)
         LA    R4,EXCMASKS(R15)   FIND CURRENT ENTRY
         MVC   0(L'COMPMASK,R4),PRS$TEXT    USE IT
         LR    R1,R4         POINT TO MASK NAME
         BAL   R9,WHATMASK   ANALYZE WHAT WE ARE LOOKING AT
         CH    R15,=H'4'
         BNL   GETXSHOM      MASK
         PRTDATA '       Excluding data set',(0(R4),L'COMPMASK,PADL)
         B     CARDNEXT
         SPACE 1
GETXSHOM PRTDATA '       Excluding by mask:',(0(R4),L'COMPMASK,PADL)
         B     CARDNEXT
         SPACE 1
GETXMBAD PRTDATA '*** Mask',(0(R4),44,DEB,PAD),'invalid ***'
         B     PGMEXIT8
         SPACE 1
*---------------------------------------------------------------------*
*   SET ONE OR MORE OPTIONS BITS, ACCORDING TO SELF-DEFINING KEYWORDS *
*   ON OPTION CARDS.                                                  *
*---------------------------------------------------------------------*
         SPACE 1
SETOPT   ICM   R3,15,PRSLINK      GET SECOND OPERAND            GP09197
         BZ    OPMISS                                           GP09197
SETOFLAG OC    PRS$TEXT,BLANKS    UPPER CASE                    GP09197
         PARFGSET OPTSETP,ERR=SETOBAD,DONE=SETONEXT             GP09197
SETONEXT ICM   R3,15,PRSLINK      ANY MORE ?                    GP09197
         BZ    SETOSHOW           NO; READ ANOTHER CARD         GP09197
         CLI   PRS$TEXT,C'*'      COMMENTS?                     GP09197
         BNE   SETOFLAG           NO; CHECK IT                  GP09197
SETOSHOW PRTLIST MSGOPTS                                        GP09197
         B     CARDNEXT           READ NEXT CARD                GP09197
         SPACE 1
SETOBAD  PRTDATA '*** UNRECOGNIZED OPTION',(PRS$TEXT,DEB,PAD),'***'
         OICC  8                                                GP09197
         B     CARDNEXT                                         GP09197
         SPACE 1
MSGOPTS  FDPRT '      Options now on are : ',NL                 GP09197
         FDTM   OPTFLAGS,255-FGPSPO-FGWRITE,BZ=MSGOPTS0         GP09197
         FDFLAG OPTFLAGS,TABLE=OPTSHO,SPACE=1,LEN=80            GP09197
         FD    *END                                             GP09197
MSGOPTS0 FDPRT 'all off'                                        GP09197
         FD    *END                                             GP09197
         SPACE 1
OPTSHO   FLGTAB FGENQ,'ENQUEUE',MLEN=1                          GP09197
         FLGTAB FGVAR,'RECFM=V'                                 GP09197
         FLGTAB FGADAT,'ALLDATA'                                GP09197
         FLGTAB FGEXP,'EXPORT'                                  GP09197
         FLGTAB FGTEST,'TEST-MODE'                              GP09197
         FLGTAB FGBUG,'DEBUG-MODE'                              GP09197
         FLGTAB *END                                            GP09197
         SPACE 1
         PARKEYBX OPTSET     BXLE                               GP09197
OPTSET   PARKEYFG 'ENQUEUE',OPTFLAGS,0,FGENQ     ENQTEST        GP09197
OPTSET2  PARKEYFG 'NOENQUE',OPTFLAGS,FGENQ,0     SKIPENQ        GP09197
         PARKEYFG 'SKIPENQ',OPTFLAGS,FGENQ,0     SKIPENQ        GP09197
         PARKEYFG 'ALLDATA',OPTFLAGS,0,FGADAT    WRITE ALL      GP09197
         PARKEYFG 'ALLEXCP',OPTFLAGS,0,FGADAT    WRITE ALL      GP09197
         PARKEYFG 'TEST',OPTFLAGS,0,FGTEST       NO WRITE       GP09197
         PARKEYFG 'EXPORT',OPTFLAGS,0,FGEXP      RESET PROTECT  GP09197
         PARKEYFG 'LIST',OPTFLAGS,0,0            SHOW SETTINGS  GP09212
         PARKEYFG 'SHOW',OPTFLAGS,0,0            SHOW SETTINGS  GP09212
OPTSETN  PARKEYFG 'DEBUG',OPTFLAGS,0,FGBUG       TEST MODE      GP09197
         SPACE 2
***********************************************************************
*                                                                     *
*   Examine contents of COPY/DUMP CARD.                               *
*     COPY level.            Uses catalog lookup to find candidates.  *
*     COPY mask VOLUME(serial)   Uses VTOC entries                    *
*     COPY VOLUME(serial) mask   Uses VTOC entries                    *
*                                                                     *
***********************************************************************
SETDUMP  TM    PROFLAGS,PFGDUMP   DUMP REQUEST PENDING ?
         BZ    SETDUMP1           NO; EXAMINE THIS ONE
         INPKEEP ,                PUT THE INPUT CARD BACK
         B     ACTDUMP
SETDUMP1 ICM   R3,15,PRSLINK      GET SECOND OPERAND
         BZ    OPMISS
         XC    DB,DB              CLEAR LENGTHS
         MVC   CELLKEY,BLANKS     CLEAR VOL/DSN×MASK
SETDUMPC L     R0,PRS#TEXT        GET LENGTH
         CLC   =C'VOL',PRSKEYWD   IS IT VOL() OR VOL= ?
         BE    SETDUMPV           YES; DIFFERENT PROCESSING
         CLC   =C'SER',PRSKEYWD   IS IT SER() OR SER= ?
         BE    SETDUMPV           YES; DIFFERENT PROCESSING
         CLI   PRS$TEXT,C'*'      COMMENT OR MASK ?             GP09207
         BNE   SETDUMPM           NO                            GP09207
         CH    R0,=H'1'           COMMENTS FIELD ?              GP09207
         BE    SETDUMPX           YES; SEE WHAT WE HAVE         GP09207
SETDUMPM ICM   R14,15,DB          DONE DSN×MASK BEFORE?         GP09207
         BNZ   OP2MANY            YES; FAIL
         CL    R0,=A(L'CELLDSN)   VALID ?
         BH    OP2LONG            NO
         ST    R0,DB              SAVE TEXT LENGTH
         OC    CELLDSN,PRS$TEXT   COPY TEXT
         TRT   CELLDSN,TRTDSNM    DSN, POSSIBLE MASK?
         CLM   R2,1,=X'4'         FULL 44, OR BLANK STOP?
         BNH   SETDUMPN           ACCEPT IT
OPCHAR   PRTDATA '***',(PRS$TEXT,PAD,DEB),'contains illegal characters *
               ***'
         OICC  8
         B     ACTRESET           QUIT THIS ONE
         SPACE 1
SETDUMPN ICM   R3,15,PRSLINK      ANY MORE ?
         BNZ   SETDUMPC           YES; EXAMINE
SETDUMPX OI    PROFLAGS,PFGDUMP+PFGONCE   SHOW DUMP REQUESTED
         B     CARDNEXT
         SPACE 1
SETDUMPV ICM   R14,15,DB+4        DID WE GET A VOLUME SERIAL?   GP09207
         BNZ   OP2MANY            YES; DUPLICATE
         LTR   R0,R0              TEST LENGTH                   GP09207
         BZ    OPMISS             NEED A SERIAL
         CL    R0,=A(L'CELLVOL)   VALID ?
         BH    OP2LONG            NO
         ST    R0,DB+4            SAVE TEXT LENGTH
         OC    CELLVOL,PRS$TEXT
         SERVCALL UCBVS,CELLVOL   VALID SERIAL ?
         BXH   R15,R15,VOLNTMNT   NO
         LR    R15,R0
         CLI   UCBTBYT3-UCBOB(R15),UCB3DACC  DASD ?
         BE    SETDUMPN      LOOK FOR ANOTHER OPERAND
         PRTDATA '***',(CELLVOL,PAD),'is not DASD ***'
         OICC  8
         B     ACTRESET
VOLNTMNT PRTDATA '***',(CELLVOL,PAD),'is not available ***'
         OICC  8
         B     ACTRESET
         SPACE 1
ACTDUMP  CLI   CELLVOL,C' '       DID WE GET A VOLUME SERIAL?
         BH    SEARCHVL           YES; RUN VTOC
         CLI   CELLDSN,C' '       DID WE GET A MASK OR DSN
         BNE   SEARCHCT           YES; SEARCH CATALOG
OPMISS   PRTL  '*** DSname/Mask operand required; VOL(serial) optional *
               ***',CC=NO
         OICC  8
         B     ACTRESET
         SPACE 1
OP2MANY  PRTL  '*** Too many operands ***',CC=NO
         OICC  4
         B     ACTRESET
         SPACE 1
OP2LONG  PRTDATA '*** Operand',(PRS$TEXT,PAD,DEB),'too long ***'
         OICC  8                                                GP09197
         B     ACTRESET
         DROP  R3
         EJECT ,
***********************************************************************
*                                                                     *
*   PROCESS DATA SET SELECTION FOR 'DUMP level '                      *
*     Use IGGCSI00 OR SuperLocate to get non-VSAM data sets,          *
*     screen for DSORG, etc. and compare to mask.                     *
*                                                                     *
***********************************************************************
SEARCHCT MVC   COMPMASK,CELLDSN   COPY TO PERMANENT SPACE
         LA    R1,COMPMASK
         BAL   R9,WHATMASK   EXAMINE IT
         CH    R15,=H'4'     IS IT NAME OR MASK?
CATMASK  CATCALL INIT,COMPMASK,MODE=(@,BALR)
         BXH   R15,R15,CATNOLOC  FAILED
         PRTDATA '   === Processing catalog',(CSPRCAT,PAD,DEB)
         B     CATNEXT2
         SPACE 1
CATNOLOC PRTDATA '*** Catalog lookup failed',(CELLDSN,DEB,PAD),'***'
         OICC  8
         B     ACTRESET
         SPACE 1
*---------------------------------------------------------------------*
*  GET NEXT CATALOG ENTRY                                             *
*---------------------------------------------------------------------*
CATNEXT  CATCALL LOOP,MODE=(@,BALR)   GET NEXT ENTRY
         BXH   R15,R15,CATDONE   DONE WITH THIS MASK
CATNEXT2 CLI   CSPRTYP,C'A'  NON-VSAM ENTRY?
         BNE   CATNEXT       NO; SKIP TO NEXT ENTRY
         MVC   CMPREQ,=C'DSN'    'NORMAL' MASKING
         TM    COMPMASK+L'COMPMASK,X'10'    POSITIONAL ?
         BZ    *+10
         MVC   CMPREQ,=C'POS'    POSITIONAL MASKING
         SUBCALL SUBCOMP,(CMPREQ,CSPRDSN,COMPMASK,CMP@STOR),VL,        *
               MF=(E,CALLPARM)
         BXH   R15,R15,CATNEXT  SKIP IF NO MATCH
         SPACE 1
         ICM   R2,15,EXC#MASK     ANY EXCLUSION MASKS ?
         BNP   CATXDONE           NO; ACCEPT
         LA    R3,EXCMASKS        POINT TO FIRST
CATXLOOP L     R15,CSP@SCMP  GET COMPARE ROUTINE ADDRESS
         TM    L'COMPMASK(R3),X'80'    MASK PROCESING ?
         BNZ   CATXMASK      YES
         CLC   CSPRDSN,0(R3)      DSN MATCHES EXCLUDE ?
         BNE   CATXBUMP      NO; TRY AGAIN
         B     CATNEXT       REJECT THIS ONE
CATXMASK MVC   CMPREQ,=C'DSN'    'NORMAL' MASKING
         TM    L'COMPMASK(R3),X'10'    POSITIONAL ?
         BZ    *+10
         MVC   CMPREQ,=C'POS'    POSITIONAL MASKING
         SUBCALL SUBCOMP,(CMPREQ,CSPRDSN,0(R3),CMP@STOR),VL,           *
               MF=(E,CALLPARM)
         BXLE  R15,R15,CATNEXT    SKIP IF MATCH ON EXCLUDE
CATXBUMP LA    R3,L'EXCMASKS(,R3)
         BCT   R2,CATXLOOP        TRY NEXT
         SPACE 1
CATXDONE SLR   R2,R2
         ICM   R2,1,CSP#VOL LOAD VOLUME COUNT
         BNP   CAT0VOL
         BCT   R2,CAT2VOL    DON'T DO MULTI-VOLUME (YET?)
*LATER*  LA    R6,CSPRVOL    POINT TO FIRST VOLUME SERIAL
*LATER*  LA    R5,CSPRDTY    AND MATCHING DEVICE TYPE
         MVC   CELLVOL,CSPRVOL    MOVE VOLUME
         MVC   CELLDSN,CSPRDSN    AND SERIAL
         SERVICE DSDS1,CELLVOL    GET DSCB 1
         BXH   R15,R15,CATNEXT
         LR    R3,R1         COPY DSCB ADDRESS
         USING DS1FMTID,R3   DECLARE RETURN
         CLI   DS1FMTID,C'1'   REALLY A FORMAT 1 ?
         BNE   CATNEXT       IF NOT, SKIP
         BAL   R9,TESTDS     EXAMINE
         B     CATNEXT       AND TRY NEXT
         SPACE 1
CAT2VOL  PRTDATA '*** Multi-volume DS',(CSPRDSN,PAD,DEB),'skipped ***'
CATSKIP  INC   NUMSKIP       COUNT SKIPPED DATA SETS
         B     CATNEXT       TRY NEXT ENTRY
         SPACE 1
CAT0VOL PRTDATA '*** No VOLSER information for',(CSPRDSN,PAD,DEB),'***'
         B     CATNEXT       SKIP THIS
         SPACE 1
CATDONE  CATCALL CLOSE,CSPMASK,MODE=(@,BALR)  FREE IT UP
         PRTL  '   === End of requested catalog search ===',NL,CC=NO
         B     ACTRESET
         EJECT ,
***********************************************************************
*                                                                     *
*   PROCESS DATA SET SELECTION FOR 'DUMP mask VOLUME(xyz)'            *
*     Read the VTOC, screen DSORG, etc., and compare mask.            *
*                                                                     *
***********************************************************************
SEARCHVL VOLREAD OPEN,CELLVOL   INITIALIZE VOLUME PROCESSING
         MVC   VOLVOL,CELLVOL   REMEMBER WHAT'S OPEN
         BXH   R15,R15,SKIPVOL    IGNORE IF BAD
         MVC   COMPMASK,CELLDSN   PROPAGATE MASK
         CLI   COMPMASK,C' '      ANY MASK ?                    GP09194
         BH    DSCBTMSK           YES; TEST IT                  GP09194
         MVC   COMPMASK(2),=C'**'   MAKE UNIVERSAL MATCH        GP09194
DSCBTMSK LA    R1,COMPMASK
         BAL   R9,WHATMASK   EXAMINE MASK
DSCBFMT4 VOLREAD DSCB        READ THE FORMAT 4
         BXH   R15,R15,SKIPVOL   ERROR
         B     DSCBNEX4      JOIN NORMAL PROCESSING              82137
         SPACE 1
SKIPVOL  PRTDATA '*** Error reading VTOC',(VOLVOL,PAD),'***'
         OICC  8
         B     ACTRESET
         SPACE 1
DSCBNEXT VOLREAD DSCB        GET ANOTHER DSCB
         CH    R15,=H'4'     DID WE GET ONE ?
         BE    DSCBNDVL      NO; END OF VOLUME
         BH    SKIPVOL       NO; I/O ERROR
DSCBNEX4 LR    R3,R0         COPY DSCB ADDRESS
         USING DS1DSNAM,R3   DECLARE RETURN
         MVC   CELLDSN,DS1DSNAM   PROPAGATE DS NAME
         LA    R3,DS1FMTID   FOR TESTDS COMMON MAPPING
         USING DS1FMTID,R3
         CLI   DS1FMTID,C'1'   REALLY A FORMAT 1 ?
         BNE   DSCBNEXT      IF NOT, SKIP
*   SUBMIT TO COMPARE TESTS
         MVC   CMPREQ,=C'DSN'    'NORMAL' MASKING
         TM    COMPMASK+L'COMPMASK,X'10'    POSITIONAL ?
         BZ    *+10
         MVC   CMPREQ,=C'POS'    POSITIONAL MASKING
         SUBCALL SUBCOMP,(CMPREQ,CELLDSN,COMPMASK,CMP@STOR),VL,        *
               MF=(E,COMPPARM)
         CH    R15,=H'4'
         BNL   DSCBNEXT      SKIP IT
         SPACE 1
         ICM   R2,15,EXC#MASK     ANY EXCLUSION MASKS ?
         BNP   DSCXDONE           NO; ACCEPT
         LA    R4,EXCMASKS        POINT TO FIRST
DSCXLOOP L     R15,CSP@SCMP  GET COMPARE ROUTINE ADDRESS
         TM    L'COMPMASK(R4),X'80'    MASK PROCESING ?
         BNZ   DSCXMASK      YES
         CLC   CELLDSN,0(R4)      DSN MATCHES EXCLUDE ?         GP10163
         BNE   DSCXBUMP      NO; TRY AGAIN
         B     DSCBNEXT      REJECT THIS ONE
DSCXMASK MVC   CMPREQ,=C'DSN'    'NORMAL' MASKING
         TM    L'COMPMASK(R4),X'10'    POSITIONAL ?
         BZ    *+10
         MVC   CMPREQ,=C'POS'    POSITIONAL MASKING
         SUBCALL SUBCOMP,(CMPREQ,CELLDSN,0(R4),CMP@STOR),VL,           *
               MF=(E,CALLPARM)
         BXLE  R15,R15,DSCBNEXT   SKIP IF MATCH ON EXCLUDE
DSCXBUMP LA    R4,L'EXCMASKS(,R4)
         BCT   R2,DSCXLOOP        TRY NEXT
         SPACE 1
DSCXDONE BAL   R9,TESTDS     EXAMINE
         B     DSCBNEXT      AND TRY NEXT
         SPACE 1
DSCBNDVL PRTL  '   === End of requested volume search ===',NL,CC=NO
ACTRESET XC    EXC#MASK,EXC#MASK  RESET EXCLUSION MASKS
         ZI    PROFLAGS,PFGDUMP   NO LONGER DUMP MODE
         B     CARDNEXT      AND TRY NEXT
         SPACE 1
***********************************************************************
**  End of input on SYSIN - see what to do                           **
***********************************************************************
CARDEOD  TM    PROFLAGS,PFGDUMP   DUMP REQUEST PENDING ?
         BNZ   ACTDUMP            COLLECT DS INFORMATION
         SPACE 1
CARDEODX PRTL  '   === End file on SYSIN ===',NL,CC=NO
         L     R15,=A(DSSDUMP2)   GO TO DUMP PHASE
         L     R0,RETCODE    LOOK AT ERRORS SO FAR
         CH    R0,=H'4'      ANY ERRORS?
         BNHR  R15           NO; CONTINUE
         PRTL  '0*** Dump not attempted due to errors ***',CC=ASA
         B     PGMEXIT1      AND QUIT
         SPACE 2
***********************************************************************
**  TESTDS - CHECK DSCB1 TO SEE WHETHER THIS IS A FILE TYPE WE       **
**    SUPPORT. 1) DSORG = PS, PO, DA                                 **
**             2) LSTAR > 0                                          **
***********************************************************************
         USING DS1FMTID,R3
TESTDS   TM    DS1DSORG,DS1DSGPS+DS1DSGDA+DS1DSGPO  xSAM/BDAM ?
         BNM   TESTSKIP      NO; SKIP
         CLI   DS1DSORG+1,0  VSAM, ETC. ?                       GP09186
         BNE   TESTSKIP      YES; SKIP                          GP09186
         CLC   CELLDSN,DUMPDSN    ACTIVE DUMP DS ?              GP09317
         BNE   TESTNSLF      NO                                 GP09317
         CLC   CELLVOL,DUMPVOL    SAME SERIAL ?                 GP09317
         BNE   TESTNSLF      NO
         PRTDATA '*** Data set',(CELLDSN,PAD,DEB),' bypassed - is activ*
               e DSSDUMP output ***'                            GP09317
         MVICC 4                                                GP09317
         B     TESTSKNO                                         GP09317
TESTNSLF CLI   DS1NOEPV,0    ANY DATA ?                         GP09193
         BE    TESTSKEM      NO; NOTHING TO DUMP                GP09193
         ICM   R0,7,DS1LSTAR  EVER USED ?
         BZ    TESTSKIP      SKIP EMPTY AND PDS/E, HFS, ETC.
         SPACE 1
         TM    OPTFLAGS,FGENQ     ENQUEUE TEST REQUESTED        GP09212
         BZ    TESTNENQ           NO                            GP09212
         LOCBYTE CELLDSN          GET DSN LENGTH                GP09212
         SR    R15,R14            LENGTH OF DSN                 GP09212
         LR    R6,R15             USE BETTER REGISTER           GP09212
         MVC   ENQLIST(PATENQL),PATENQ      REFRESH             GP09212
         ENQ   (,CELLDSN,,(R6),),RET=TEST,MF=(E,ENQLIST)        GP09212
         BXH   R15,R15,TESTNBAD                                 GP09212
         ENQ   (QJAM,CELLDSN,,(R6),),RET=TEST,MF=(E,ENQLIST)    GP09212
         BXLE  R15,R15,TESTNENQ   PASSED                        GP09212
TESTNBAD PRTDATA '*** Data set',(CELLDSN,PAD,DEB),' bypassed - ENQ test*
                unsuccessful ***'                               GP09212
         MVICC 4                                                GP09212
         B     TESTSKNO                                         GP09212
         SPACE 1
TESTNENQ MVI   CELLFLAG,CFPICK    FLAG DS AS SELECTED
         MVC   CELLALI,CELLDSN    SET ALIAS SAME AS TRUE NAME
         ICM   R6,15,NUMPFX  DOING PREFIXING ?                  GP09197
         BZ    TESTUPD       NO, LEAVE DSN ALONE                GP09197
         LA    R5,PFXOLDL    POINT TO RENAME/PREFIX LIST        GP09197
TESTPFX  MVI   LOCFMT1,C' '  PREPARE A WORK AREA                GP09197
         MVC   LOCFMT1+1(43+44),LOCFMT1    MORE THAN ENOUGH     GP09197
         MVC   LOCFMT1(L'PFXNEW),PFXNEW-PFXOLDL(R5) NEW PREFIX  GP09197
         SR    R14,R14                                          GP09197
         SR    R1,R1                                            GP09197
         IC    R1,PFXNEWL-PFXOLDL(,R5)                          GP09197
         LA    R1,LOCFMT1(R1)   TO ADDRESS                      GP09197
         IC    R14,PFXOLDL-PFXOLDL(R5)                          GP09197
         LA    R15,CELLALI(R14)  SET SOURCE ADDRESS             GP09197
         LTR   R14,R14       ANY COMPARE ?                      GP09197
         BNP   TESTNOLD      NO                                 GP09197
         BCTR  R14,0         EX LENGTH                          GP09197
         EX    R14,EXCLCPFX  MATCHING PREFIX ?                  GP09197
         BNE   TESTBUMP      NO; TRY ANOTHER                    GP09197
         LA    R14,1(,R14)   FIX UP                             GP09317
TESTNOLD LA    R0,44-1       GET LENGTH - 1 TO MOVE             GP09197
         SR    R0,R14        GET EX LENGTH                      GP09197
         BM    TESTNNEW                                         GP09197
         LR    R14,R0        SWAP                               GP09197
         EX    R14,EXMVCPFX  MOVE                               GP09197
TESTNNEW MVC   CELLALI,LOCFMT1    MOVE NEW NAME                 GP09197
         CLI   CELLALI+L'CELLALI-1,C'.'   TRAILING INDEX POINT? GP09197
         BNE   TESTUPD       NO                                 GP09197
         MVI   CELLALI+L'CELLALI-1,C' '   YES; FIX IT           GP09197
         B     TESTUPD            AND USE IT                    GP09197
EXCLCPFX CLC   CELLALI(0),PFXOLD-PFXOLDL(R5)   MATCHING PREFIX? GP09197
EXMVCPFX MVC   0(0,R1),0(R15)   MOVE TRAILER                    GP09197
         SPACE 1
TESTBUMP LA    R5,PFXLEN(,R5)
         BCT   R6,TESTPFX    CHECK FOR PREFIX MATCH             GP09197
TESTUPD  LOCBYTE CELLALI     GET NAME LENGTH                    GP09197
         SR    R15,R14       GET LENGTH                         GP09197
         STC   R15,CELLALIL  AND SAVE FOR PASS 2                GP09197
         SUBCALL SUBTREE,('UPD',ROOTBALL,RECREC),VL,MF=(E,CALLPARM)
         CH    R15,=H'8'     ACCEPTABLE RESULT ?
         BL    TESTGOOD
         PRTL  '0*** List update failed ***',CC=ASA
         B     PGMEXIT8
         SPACE 1
TESTSKEM DS    0H            NO EXTENTS                         GP09193
 PRTDATA ' ? ',CELLVOL,(CELLDSN,PAD),'skipped (no extents).        DSOR*
               G=',(DS1DSORG,2,HEX,PAD),'LSTAR=',(DS1LSTAR,3,HEX)
         B     TESTSKNO
         SPACE 1
TESTSKIP DS    0H            BAD DSORG OR LSTAR                 GP09193
 PRTDATA ' ? ',CELLVOL,(CELLDSN,PAD),'skipped (unsupported/empty). DSOR*
               G=',(DS1DSORG,2,HEX,PAD),'LSTAR=',(DS1LSTAR,3,HEX)
TESTSKNO INC   NUMSKIP       COUNT DATA SETS SKIPPED
         BR    R9            RETURN TO CALLER
         SPACE 1
TESTGOOD BXH   R15,R15,TESTADDD
 PRTDATA '   ',CELLVOL,(CELLDSN,PAD),'already queued'
         BR    R9            RETURN TO CALLER
         SPACE 1
TESTADDD INC   NUMPICK
         SERVCALL DSFMT,DS1DSORG  MAKE ATTRIBUTES PRINTABLE
         LR    R3,R1         SAVE RETURN
   PRTDATA '   ',CELLVOL,(CELLDSN,PAD),'queued:    ',(0(R3),3,PAD),(3(R*
               3),6,PAD),(22(R3),5,PAD),(17(R3),5)
         BR    R9            RETURN TO CALLER
         DROP  R3
         SPACE 1
         LTORG ,
         SPACE 1
VERBTAB  PARKEYAD 'EXCLUDE ',GETXMASK  SPECIFY DS MASK
VERBTAB2 PARKEYAD 'INCLUDE ',SETDUMP   SPECIFY DS MASK
         PARKEYAD 'SELECT  ',SETDUMP   SPECIFY DS MASK
         PARKEYAD 'DUMP    ',SETDUMP   BEGIN DS SELECTION
         PARKEYAD 'COPY    ',SETDUMP   BEGIN DS SELECTION
         PARKEYAD 'RENAME  ',SETREN    SET PFX RENAME           GP09197
         PARKEYAD 'REPLACE ',SETREN    SET PFX RENAME           GP09197
         PARKEYAD 'STRIP   ',SETREN    DELETE LEADING DSN CHARS GP09197
         PARKEYAD 'PREFIX  ',SETPFX    SET PREFIX (NO RENAME)   GP09197
         PARKEYAD 'OPTIONS ',SETOPT    SET OPTION BITS ON/OFF
         PARKEYAD 'ALLDATA ',SETALLTK  DUMP ALL TRACKS
         PARKEYAD 'ALLEXCP ',SETALLTK  DUMP ALL TRACKS
         PARKEYAD 'NODEBUG ',SETBUGOF
         PARKEYAD 'TEST    ',SETTSTON
VERBTABN PARKEYAD 'DEBUG   ',SETBUGON
         TITLE 'D S S D U M P  ***  MAKE DUMP TAPE'
DSSDUMP2 CSECT ,             SUBROUTINE AND DATA AREA
         LA    R11,0(,R15)   COPY NEW BASE
         LA    R12,2048(,R11)
         LA    R12,2048(,R12)
         USING DSSDUMP2,R11,R12
         PRTL  ' ',NL,CC=NO       PRETTIFY OUTPUT
         SPACE 1
*---------------------------------------------------------------------*
*   Note that the catalog name isn't available for VOL() lookup,      *
*   and I didn't keep it from the catalog retrieval.                  *
*   So we use the master catalog name for laughs.                     *
*---------------------------------------------------------------------*
         PUSH  USING
         L     R5,CVTPTR
         USING CVTMAP,R5                                        GP13007
         ICM   R5,15,CVTCBSP    -> AMCBS
         BZ    NONECAX       HUH?
         USING AMCBS,R5
         LA    R5,CBSCAXCN-(CAXCHN-IGGCAXWA)
         USING IGGCAXWA,R5
CAXNEXT  ICM   R5,15,CAXCHN
         BZ    NONECAX
         TM    CAXFLGS,CAXMCT     MASTER CATALOG ?
         BZ    CAXNEXT            NO; IGNORE
         MVC   CSPRCAT,CAXCNAM    REMEMBER IT
         LOCBYTE CSPRCAT                                        GP09192
         SR    R15,R14       NON-BLANK LENGTH                   GP09192
         STC   R15,CSPRTYP   SAVE IN STOLEN FIELD               GP09192
         POP   USING
         SPACE 1
*---------------------------------------------------------------------*
*   Check for at least one queued entry. Quit if none.                *
*   Else build tape header record.                                    *
*---------------------------------------------------------------------*
NONECAX  SUBCALL LISTREE     INITIALIZE SEQUENTIAL READING
         LTR   R15,R15       ANY PROBLEM ?
         BZ    DSNLINIT
         PRTDATA '*** No data sets to dump ***'
         B     PGMEXIT8
         SPACE 1
DSNLINIT L     R8,REC@REC    UPDATE THE CELL ADDRESS
         L     R2,@BUF       POINT TO START OF BUFFER
         BAL   R9,BUILDHED   BUILD A HEADER RECORD
         USING DSSBLOCK,R2   DECLARE IT
         XC    DTHTIMD(DTHSIZE),DTHTIMD  CLEAR UNUSED
         MVI   DTPRCID1,DTPTHDR   IDENTIFY HEADER
         TIME  DEC
         ST    R1,DTHTIMD    SET DAY
         ST    R0,DTHTIMD+4  AND TIME
         MVI   DTHIND2,DTHGVI     NON-VSAM ONLY
         LA    R1,DTHSIZE    SIZE OF HEADER - HEADER
         STCM  R1,3,DTHLEN
         LA    R1,DTPSIZE(,R1)
         STCM  R1,3,DTPSEGLN      SEGMENT SIZE
         MVI   DTHVERNO,X'1F'
         MVI   DTHLVLNO,X'50'
         L     R0,#BUFMAX    MAXIMUM BLOCK
         STCM  R0,3,DTHBLKSZ
         L     R0,NUMPICK
         STCM  R0,3,DTHNDS   NUMBER OF DATA SETS
         LA    R2,DTHSIZE+DTPSIZE(,R2)
         ST    R2,@BUFCUR    SET SIZE USED
         BAL   R9,TAPEOUT    WRITE A BLOCK
         SPACE 1
*---------------------------------------------------------------------*
*   Now run through the queued data sets just to build the data set   *
*   name block. Check for duplicate names (on separate volumes) and   *
*   assign them a fake name.                                          *
*   The table is built in the output buffer, even though larger than  *
*   the block size, and is then written as multiple blocks.           *
*---------------------------------------------------------------------*
         SPACE 1
         L     R2,@BUF
         BAL   R9,BUILDHED   BUILD COMMON HEADER
         MVI   DTPRCID1,DTPDSNL   DS NAME LIST
         LA    R2,DSSHEDND   POINT TO START OF LIST
         ST    R2,@MEMCUR    SAVE FOR REFERENCE
         B     DSNLCOMM
         SPACE 1
DSNLNEXT SUBCALL NEXTREE     GET NEXT ENTRY
         BXH   R15,R15,DSNLTEND   DONE
         L     R8,REC@REC    UPDATE THE CELL ADDRESS
         SPACE 1
         SR    R15,R15                                          GP09197
         IC    R15,CELLALIL  GET LENGTH BACK                    GP09197
         L     R2,@BUF
         SR    R14,R14       CLEAR FOR IC
DSNLDLUP CLM   R15,1,DTLLEN  SAME LENGTH ?
         BNE   DSNLDBMP      NO; IGNORE
         LR    R14,R15
         BCTR  R14,0         EXECUTE LENGTH FOR COMPARE
         EX    R14,EXCLCDSN  IS IT A DUPLICATE ?
         BE    DSNLDDUP      YES; RENAME
DSNLDBMP IC    R14,DTLLEN         GET ENTRY LENGTH
         LA    R2,DTLSIZE(R14,R2)   NEXT ENTRY
         C     R2,@MEMCUR    IS THAT IT ?
         BL    DSNLDLUP      NO; CHECK AGAIN
         B     DSNLCOM2      RE-USE R15
EXCLCDSN CLC   CELLALI(0),DTLDSN  DUPE CHECK
         SPACE 1
DSNLDDUP MINH  R15,=AL2(L'CELLALI-9) IF TOO LONG, TRUNCATE
         LA    R1,CELLALI-1(R15)
         CLI   0(R1),C'.'    HIT ON A PERIOD ?
         BNE   *+4+2
         BCTR  R1,0          REUSE IT
         MVC   1(9,R1),=C'.D#nnnnnn'   MAKE FAKE NAME
         INC   NUMDUPE,WORK=R0
         CVD   R0,DB
         OI    DB+7,X'0F'    FORCE SIGN
         MVC   DB2(8),=X'F021202020202020'
         ED    DB2(8),DB+4
         MVC   4(6,R1),DB2+2   MAKE UNIQUE (?) NAME
         SPACE 1
DSNLCOMM SR    R15,R15                                          GP09197
         IC    R15,CELLALIL  LENGTH TO FIRST BLANK              GP09197
DSNLCOM2 L     R2,@MEMCUR    GET CURRENT USE
         USING DTLLEN,R2     SKIP PAST HEADER
         LA    R3,DTLSIZE(R15,R2)  SIZE AFTER MOVE
         C     R3,@MEMEND    WILL IT FIT
         BL    DSNLMOVE      YES (NOTE IT TAKES ONE MORE)
      PRTL '*** Data set name table overflow - split job? ***',NL,CC=NO
         PRTDATA '    at data set',(NUMPICK,I,PAD)
         B     PGMEXIT8
         SPACE 1
DSNLMOVE STC   R15,DTLLEN    SET DSN LENGTH
         MVC   DTLCAT,CSPRCAT  CATALOG NAME (IRRELEVANT?)
         EX    R15,EXMVCDSN  MOVE NAME + ONE CRUD BYTE
         ST    R3,@MEMCUR    UPDATE BUFFER
         B     DSNLNEXT      PROCESS NEXT DATA SET
EXMVCDSN MVC   DTLDSN(0),CELLALI       MOVE NAME
         SPACE 1
DSNLTEND L     R3,@MEMCUR    POINT TO END OF BUFFER
         MVI   0(R3),X'0'    END FLAG - NO DSN
         LA    R3,1(,R3)     ADJUST LENGTH
         ST    R3,@MEMCUR
         ST    R3,@BUFCUR    FOR SINGLE BLOCK
         L     R2,@BUF       BUFFER START
         USING DSSBLOCK,R2        HEADER
         C     R3,@BUFEND         SINGLE BLOCK ?
         BNH   DSNL1BLK           YES; WRITE DSN TABLE AS SINGLE BLOCK
         SPACE 1
*---------------------------------------------------------------------*
*    The Data Set Name Table is larger than the block size, but less  *
*    than 65KB. Break it into block-sized chunks.                     *
*    To make life more interesting, a simple break was too easy for   *
*    IBM - each chunk must end with a DSN length field, artificially  *
*    set to zero. And we need to examine data to get segment count!   *
*---------------------------------------------------------------------*
         LA    R15,DTLLEN    GET FIRST DATUM
         SR    R1,R1         CLEAR IC REGISTER
         LA    R5,1          AT LEAST ONE SEGMENT
         LA    R4,1          ALLOW FOR ZERO END BYTE
DSNLSLUP LR    R14,R15       CURRENT DSN ENTRY
         ICM   R1,1,0(R14)   DSN LENGTH
         BZ    DSNLSDON      DONE
         LA    R15,DTLSIZE(R1,R14)  NEXT ENTRY
         LA    R4,DTLSIZE(R1,R4)    PURPORTED DATA SIZE IN BLOCK
         C     R4,#DATMAX    WILL IT FIT ?
         BL    DSNLSLUP      YES; TRY AGAIN
         LA    R4,1+DTLSIZE(R1)     SIZE IN NEXT BLOCK
         LA    R5,1(,R5)     NEW SEGMENT COUNT
         B     DSNLSLUP        TRY NEXT
DSNLSDON STC   R5,DTPNOSEG   SAVE SEGMENT COUNT
         LR    R5,R3         COPY END ADDRESS
         SR    R5,R2         LESS START
         SH    R5,=AL2(DTPSIZE)   LESS HEADER
         LA    R4,DTLLEN          POINT TO START OF DATA
DSNLBLKN L     R7,#DATMAX         SIZE WE'RE WRITING
         AR    R7,R2              LAST BYTE+1
         LR    R15,R4             POINT TO DATA START
         SR    R1,R1              CLEAR FOR IC
DSNLHLUP LR    R14,R15            COPY TO WORKING REGISTER
         ICM   R1,1,0(14)         GET NEXT DSN LENGTH
         BZ    DSNLHDON              BUT SHOULD NOT HAPPEN?
         LA    R15,DTLSIZE(R1,R14)   NEXT LENGTH BYTE
         CR    R15,R7             AT END OF CHUNK?
         BNH   DSNLHLUP           AND AGAIN
DSNLHDON ICM   R6,8,0(R14)        SAVE OLD LENGTH
         MVI   0(R14),0           SET END FLAG
         LR    R7,R14             END ADDRESS
         SR    R7,R4              GET DATA LENGTH USED
         BNP   COPYINIT      SHOULD NOT HAPPEN?
         LA    R7,1(,R7)         CURRENT BYTE OF ZERO (NOT DATA)
         LA    R3,DTPSIZE(,R7)    PLUS HEADER
         STCM  R3,3,DTPSEGLN      SET LENGTH INTO RECORD
         MVC   DB3,DTPNOSEG  SAVE
         BAL   R9,TAPEOUTR   WRITE ONE BLOCK
         CR    R5,R7         FINISHED ?
         BNH   COPYINIT      YES
         BCTR  R7,0          ACCOUNT FOR EXTRA LENGTH FIELD
         SR    R5,R7         ACCOUNT FOR PORTION WRITTEN
         AR    R4,R7         NEXT BUFFER START
         STCM  R6,8,0(R4)    RESTORE DSN LENGTH
         LR    R2,R4         GET DATA START
         SH    R2,=AL2(DTPSIZE)  ROOM FOR NEXT HEADER
         BAL   R9,BUILDHED   BUILD BASICS
         MVC   DTPNOSEG(8),DB3   RESTORE
         SR    R15,R15
         IC    R15,DTPSEGNO  PREVIOUS
         LA    R15,1(,R15)
         STC   R15,DTPSEGNO  NEXT SEGMENT
         B     DSNLBLKN      WRITE THIS SEGMENT
         SPACE 1
DSNL1BLK SR    R3,R2              LENGTH
         STCM  R3,3,DTPSEGLN      SET LENGTH INTO RECORD
         BAL   R9,TAPEOUT
         SPACE 1
*---------------------------------------------------------------------*
*   Copy Data Set information - fake as SMS.                          *
*---------------------------------------------------------------------*
         SPACE 1
COPYINIT SUBCALL LISTREE     INITIALIZE SEQUENTIAL READ BACK
         LTR   R15,R15       ANY PROBLEM ?
         BNZ   COPYIBAD
         L     R8,REC@REC         GET RECORD ADDRESS
         CLC   CELLVOL,VOLVOL     NEW SERIAL ?
         BE    COPYTYPE           NO; JUST GET DEVICE TYPE
         B     COPYFILE           YES; SWITCH PACKS
COPYIBAD PRTDATA '*** Program error in SUBTREE ***'
         PRTCLOSE ,          WRITE MESSAGE BEFORE DUMP
         ABEND 1001,DUMP
         SPACE 1
COPYNEXT SUBCALL NEXTREE     GET NEXT ENTRY
         BXH   R15,R15,COPYTEND   DONE
         L     R8,REC@REC         GET RECORD ADDRESS
COPYFILE CLC   CELLVOL,VOLVOL     NEW SERIAL ?
         BE    COPYSAME           NO
         VOLREAD OPEN,CELLVOL
         BXH   R15,R15,COPYBADV
         MVC   VOLVOL,CELLVOL
COPYTYPE MVC   GENERIC,BLANKS
         SERVCALL UCBVS,VOLVOL    GET UCB BACK
         LTR   R1,R0              TEST
         BZ    COPYSAME           WILL FAIL ?
         LR    R2,R1         SAVE OVER SUBROUTINE CALL          GP09210
         SUBCALL SUBVTVAL    GET TRACK SIZE, ETC FOR UCB IN R1  GP09210
         LTR   R15,R15       SUCCESSFUL ?                       GP09210
         BZ    COPYN4VL      NO; SKIP                           GP09210
         STM   R15,R0,DB     TEMP STASH                         GP09210
         MVI   LOCFMT1,X'04'                                    GP09210
         MVC   LOCFMT1+1(L'LOCFMT1-1),LOCFMT1                   GP09210
         VOLREAD DSCB        READ THE FORMAT 4                  GP09210
         BXH   R15,R15,COPYN4VL   ERROR                         GP09210
         LR    R1,R0              COPY ADDRESS                  GP09210
         LA    R6,44(,R1)         SKIP NAME                     GP09210
         USING DS4IDFMT,R6                                      GP09210
         CLI   DS4IDFMT,C'4'      REALLY?                       GP09210
         BNE   COPYN4VL           NO; SKIP                      GP09210
         LM    R15,R0,DB          RESTORE TRK.CAP, DSCB, DE     GP09210
         CLM   R15,3,DS4DEVTK     MATCHING TRACK CAPACITY?      GP09210
         BNE   COPY4IFF           NO; MAY NOT WORK              GP09210
         CLM   R0,3,DS4DEVDT      TEST DSCB,DE COUNT            GP09210
         BE    COPYN4VL           GOOD                          GP09210
COPY4IFF PRTDATA '***',(VOLVOL,PAD),'device constants do not match IBM'*
               's; like device restore with ADRDSSU questionable ***'
         PRTDATA 'VTOC track size',(DS4DEVTK,IA,PAD),' DSCBs/tk',(DS4DE*
               VDT,IA,PAD),' DirBlk/tk',(DS4DEVDB,IA,PAD)       GP09210
         PRTDATA 'DVCT track size',(DB+2,2,IA,PAD),' DSCBs/tk',(DB+6,1,*
               IA,PAD),' DirBlk/tk',(DB+7,1,IA,PAD)             GP09210
         OICC  4                  SET WARNING                   GP09210
         DROP  R6                                               GP09210
COPYN4VL SERVCALL UCBGN,(R2)      GET GENERIC                   GP09210
         LM    R14,R15,0(R1)      LOAD RESULT
         LA    R0,7               SHOW SOME RESTRAINT
COPYSGLP CLM   R15,1,BLANKS       TRAILING BLANK ?
         BNE   COPYSGEN
         SRDL  R14,8
         ICM   R14,8,BLANKS       LEFT FILL
         BCT   R0,COPYSGLP
COPYSGEN STM   R14,R15,GENERIC    COPY RESULT OR BLANK
         SPACE 1
*---------------------------------------------------------------------*
*   Finally we can actually write a data set:                         *
*   1) Get the format 1 DSCB, and a format 3 if present               *
*   2) Write the data set header record                               *
*   3) Write the volume header                                        *
*   4) Write the track data  (up to LSTAR+1 tracks; all if ALLDATA)   *
*---------------------------------------------------------------------*
COPYSAME XC    DSN#TRAK(NUMPCYL-DSN#TRAK),DSN#TRAK  RESET COUNTERS
         ZI    OPTFLAGS,FGPSPO    RESET DSORG FLAG
         ZI    PROFLAGS,PFSOME    RESET NOT ALLDATA FLAG        GP09202
         MVC   DSN#TMAX,=X'7FFFFFFF'    DUMP ALL TRACKS
         L     R2,@BUF
         BAL   R9,BUILDHED        BUILD HEADER
         XC    DTDLEN(DTDSIZE),DTDLEN  CLEAR UNUSED
         MVI   DTPRCID1,DTPDSHDR  DATA SET HEADER
         LA    R3,DTPSIZE+DTDSIZE  SEGMENT SIZE
         STCM  R3,3,DTPSEGLN      SET
         AR    R3,R2
         ST    R3,@BUFCUR
         MVC   DTDLEN,CELLALIL    GET DATA SET NAME LENGTH      GP09197
         MVC   DTDCATLN,CSPRTYP   COPY CATALOG NAME LENGTH      GP09192
         MVI   DTDNVOL,1          ONLY VOLUME SUPPORTED AT PRESENT
         MVI   DTDIND,DTDSMS           FAKE SMS AND RACF        GP09193
*DOC*    XC    DTDPSWD,DTDPSWD    NO PASSWORD                   GP09193
         MVC   DTDDSN-L'CSPRCAT(L'CSPRCAT),CSPRCAT
         MVC   DTDDSN,CELLALI
         MVI   DTDVCTD,1          VOLUME COUNT
*DOC*    MVI   DTDVCTD+1,0        VSAM COMPONENT COUNT
         VOLREAD SEARCH,CELLDSN     GET FORMAT 1 DSCB
         BXH   R15,R15,COPYBAD1   NONE ?
         LR    R6,R0
         USING IECSDSL1,R6
         MVC   LOCFMT1(DS1END-IECSDSL1),IECSDSL1   SAVE FOR LATER
         LA    R6,LOCFMT1         USE PERMANENT COPY
         MVC   DS1DSNAM,CELLALI   FAKE USING ALIAS NAME
         MVC   DTDDSORG,DS1DSORG
         MVC   DTDOPTCD,DS1OPTCD
         LA    R1,DS1CREDT        POINT TO DATE                 GP09194
         BAL   R9,FIXDATE         FIX IT UP                     GP09194
         LA    R1,DS1EXPDT        POINT TO DATE                 GP09194
         BAL   R9,FIXDATE         FIX IT UP                     GP09194
         LA    R1,DS1REFD         POINT TO DATE                 GP09194
         BAL   R9,FIXDATE         FIX IT UP                     GP09194
         TM    OPTFLAGS,FGEXP     EXPORTING THIS ?              GP09197
         BZ    COPYNSET           NO                            GP09197
         NI    DS1DSIND,255-(DS1IND40+DS1IND10+DS1IND04)        GP09197
         XC    DS1EXPDT,DS1EXPDT  KILL EXPIRATION               GP09197
         NI    DS1DSORG,255-DS1DSGU   RESET UNMOVEABLE          GP09197
COPYNSET TM    OPTFLAGS,FGADAT    ALLDATA/ALLEXCP ?
         BNZ   COPYALLT      YES; COPY ALL ALLOCATED TRACKS
         TM    DS1DSORG,DS1DSGPS+DS1DSGPO   SEQUENTIAL OR PARTITIONED?
         BNM   COPYALLT      NEITHER - COPY ALL
         TM    DS1DSORG+1,0  VSAM, ETC.?                        GP09186
         BNE   COPYALLT      FUNNY - COPY ALL                   GP09186
         OI    OPTFLAGS,FGPSPO    INDICATE LSTAR VALID
         SR    R1,R1
         ICM   R1,3,DS1LSTAR
         LA    R1,1(,R1)     NUMBER TO BE DONE
         ST    R1,DSN#TMAX   SAVE
         OI    DTDIND2,DTDNTALL   SHOW SKIPPING UNUSED TRACKS
         OI    PROFLAGS,PFSOME    SHOW SKIPPING TRACKS          GP09202
COPYALLT BAL   R9,TAPEOUT         WRITE THE DATA SET HEADER
         SPACE 1
*---------------------------------------------------------------------*
*   Write volume information. Create fake VVRS; fake indexed VTOC     *
*---------------------------------------------------------------------*
COPYVTOC L     R2,@BUF
         BAL   R9,BUILDHED        BUILD HEADER
         MVI   DTPRCID1,DTPVOLD   VOLUME HEADER
         XC    DTMVOL(256),DTMVOL  CLEAR UNUSED ITEMS           GP09193
         XC    DTMVOL+256(256),DTMVOL+256  CLEAR MORE           GP09193
         MVC   DTMVSERL,CELLVOL   SERIAL
         SERVCALL UCBDK,CELLVOL   GET UCB
         LTR   R1,R0         DID WE FIND IT ?
         BZ    COPYNUCB        BUT HOW/
         MVC   DTMDEVTY,UCBTBYT1-UCBOB(R1)
         SR    R14,R14
         IC    R14,UCBTBYT4-UCBOB(R1)  GET DEVICE TYPE
         N     R14,=A(DVCTYPMK)    REMOVE EXTRANEOUS BITS
         L     R15,CVTPTR    GET CVT
         L     R15,CVTZDTAB-CVTMAP(,R15)
         IC    R14,DVCTIOFF-DVCTI(R14,R15)   GET DEVICE OFFSET
         AR    R14,R15       GET DEVICE ENTRY
         MVC   DTMTRKCP+2(2),DVCTRKLN-DVCT(R14)  RAW TRACK SIZE
         MVC   DTMLOGCY(L'DTMLOGCY+L'DTMTRKCY),DVCCYL-DVCT(R14)
         MVC   NUMPCYL+2(2),DVCCYL+2-DVCT(R14)
COPYNUCB MVI   DTM#DSCB,1    FOR NOW
         MVI   DTM#VVRS,1         FAKE VVRS                     GP09193
         MVI   DTMFLAGS,DTMCVAF   INDEXED VTOC                  GP09193
         MVC   DTMDSCB(DS1END-IECSDSL1),LOCFMT1
         XC    LOCFMT3,LOCFMT3    CLEAR PROPOSED FMT3 INPUT
         OC    DS1PTRDS,DS1PTRDS  IS THERE A FORMAT 3?
         BZ    COPY3NOT
         VOLREAD DSC3 DS1PTRDS    GET FMT3 FOR PREVIOUS FMT 1
         LR    R1,R0
         BXH   R15,R15,COPY3NOT   OOPS?
         LR    R1,R0
         CLI   DS3FMTID-IECSDSL3(R1),C'3'
         BNE   COPY3NOT
         MVC   LOCFMT3(DS3END-IECSDSL3),0(R1)    SAVE FOR LATER
COPY3NOT SR    R3,R3
         IC    R3,DS1NOEPV   GET EXTENTS                        GP09193
         TM    OPTFLAGS,FGADAT    ALLDATA/ALLEXCP ?             GP09314
         BZ    COPYNSUL      NO                                 GP09314
         CLI   DS1EXT1,X'40' LABEL TRACK PRESENT?               GP09314
         BNE   COPYNSUL      NO                                 GP09314
         LA    R3,1(,R3)     SET PHYSICAL EXTENTS               GP09314
COPYNSUL STC   R3,DTM#EXT    AND UPDATE                         GP09314
         LR    R5,R3         EXTENTS ON THIS VOLUME             GP09238
         MH    R3,=AL2(DS1EXT2-DS1EXT1)     EXTENT DATA         GP09193
         LA    R3,DTPSIZE+DTMSIZE-(DS1EXT2-DS1EXT1)(,R3)        GP09193
         SR    R15,R15                                          GP09193
         IC    R15,CSPRTYP        LENGTH OF CATALOG NAME        GP09193
         AR    R3,R15                                           GP09193
         IC    R15,CELLALIL       LENGTH OF DATA SET NAME       GP09197
         AR    R3,R15                                           GP09193
         STCM  R3,3,DTPSEGLN
         AR    R3,R2
         ST    R3,@BUFCUR
*   MAKE ALL (16) EXTENTS CONTIGUOUS FOR EASIER USE
         MVC   DS1EXT1+3*10(4*10),DS3EXTNT-IECSDSL3+LOCFMT3     GP09193
         MVC   DS1EXT1+7*10(9*10),DS3ADEXT-IECSDSL3+LOCFMT3     GP09193
         LR    R15,R5                                           GP09193
         MH    R15,=AL2(DS1EXT2-DS1EXT1)
         BCTR  R15,0         TOTAL EXTENT SIZE -1
         LA    R4,DS1EXT1    POINT TO FIRST EXTENT IN LIST      GP09193
         CLI   DS1EXT1,X'40'      LABEL TRACK ?
         BNE   COPYNLBL      NO
         TM    OPTFLAGS,FGADAT    ALLDATA/ALLEXCP ?             GP09314
         BNZ   COPYNLBL      YES; COPY LABEL TRACK ALSO         GP09314
         LA    R4,DS1EXT2    SKIP LABEL TRACK
COPYNLBL MVC   CURCCHH,2(R4)   COPY STARTING CC HH
EXMVCEXT MVC   DTMEXTS(0),0(R4)   COPY EXTENTS                  GP09193
         STM   R4,R5,CUREXT       SAVE FOR COPY                 GP09238
         TM    PROFLAGS,PFSOME    PARTIAL DUMP ?                GP09202
         BZ    COPYXALL           NO; DUMP ALL                  GP09202
*---------------------------------------------------------------------*
*   WE ARE DUMPING ONLY SOME OF THE TRACKS IN THE DATA SET.           *
*   WE NEED TO MOVE ALL EXTENTS NECESSARY TO SATISFY LSTAR(+1), AND   *
*   CLIP THE LAST EXTENT TO MATCH THE LSTAR TRACK                     *
*---------------------------------------------------------------------*
         LR    R9,R2         TEMP RE-ASSIGNEMNT                 GP09202
         DROP  R2            GONE, BUT NOT FORGOTTEN            GP09202
         USING DSSBLOCK,R9                                      GP09202
         ICM   R6,3,DS1LSTAR   GET LSTAR BACK                   GP09202
         DROP  R6                                               GP09202
         N     R6,=X'0000FFFF'    KILL SIGN EXTENSION           GP09202
         LA    R6,1(,R6)     NUMBER OF USED TRACKS              GP09202
         LA    R3,DTMEXTS    FIRST EXTENT DESTINATION           GP09202
         L     R2,NUMPCYL    TRACKS PER CYLINDER                GP09202
         MVI   DTM#EXT,0     RESET EXTENT COUNT                 GP09202
         SPACE 1
COPYXSLP LR    R1,R4         CURRENT EXTENT ADDRESS             GP09202
         LA    R0,1          ONE AT A TIME                      GP09202
         L     R15,=V(SUBXTSUM)    EXTENT SIZER                 GP09202
         BALR  R14,R15       GET EXTENT SIZE                    GP09202
         LTR   R15,R15       ANY ?                              GP09202
         BNP   COPYXSUP      SKIP BAD EXTENT                    GP09202
         SR    R14,R14                                          GP09202
         IC    R14,DTM#EXT   GET CURRENT COUNT                  GP09202
         LA    R14,1(,R14)   BUMP                               GP09202
         STC   R14,DTM#EXT   STASH BACK                         GP09202
         MVC   0(10,R3),0(R4)   COPY EXTENT                     GP09202
         CR    R15,R6        SATISFIED LSTAR YET ?              GP09202
         BH    COPYXSTK      YES; NEED TO ADJUST END CCHH       GP09202
         BE    COPYXSTX      YES, EXACTLY                       GP09202
         SR    R6,R15        NEW RESIDUAL                       GP09202
         LA    R3,10(,R3)    NEXT OUTPUT                        GP09202
COPYXSUP LA    R4,10(,R4)    NEXT INPUT                         GP09202
         BCT   R5,COPYXSLP   TRY NEXT                           GP09202
         B     COPYXSTY      ?  SHOULD GET HERE                 GP09202
COPYXSTK SR    R15,R15       COPY RESIDUAL LSTAR COUNT          GP09202
         SR    R14,R14                                          GP09202
         ICM   R15,3,2(R3)   START CC                           GP09202
         MR    R14,R2        CONVERT TO TRACKS                  GP09202
         ICM   R14,3,4(R3)   GET TRACKS                         GP09202
         AR    R15,R14       ADD RESIDUAL TRACKS                GP09202
         AR    R15,R6        ADD LSTAR TRACKS                   GP09202
         BCTR  R15,0         ALLOW FOR LSTAR BUMP               GP09202
         SR    R14,R14       CLEAR FOR DIVIDE                   GP09202
         DR    R14,R2        GET TRACKS / CYLINDERS OF END      GP09202
         STCM  R15,3,6(R3)   STASH END CYLINDER                 GP09202
         STCM  R14,3,8(R3)     AND END TRACK                    GP09202
COPYXSTX LA    R3,10(,R3)    SET FOR VVRS ADDRESS               GP09202
COPYXSTY LR    R2,R9         RESTORE                            GP09202
         SR    R14,R14                                          GP09202
         IC    R14,DTM#EXT   GET CURRENT COUNT                  GP09202
         ST    R14,CUREXT+4  SAVE EXTENT COUNT                  GP09238
         DROP  R9                                               GP09202
         USING DSSBLOCK,R2                                      GP09202
         B     COPYXCOM      AND BUILD IT
         SPACE 1
*---------------------------------------------------------------------*
*   ALLDATA/ALLEXCP MODE, OR NOT A PS/PO DATA SET - USE ALL EXTENTS   *
*---------------------------------------------------------------------*
COPYXALL EX    R15,EXMVCEXT    COPY EXTENTS                     GP09193
         LA    R3,DTMEXTS+1(R15)  VVRS LOCATION                 GP09193
*PRTDATA '>>> Extent',(0(R4),2,HEX,PAD),(2(R4),4,HEX),(6(R4),4,HEX,PAD)
         PUSH  USING                                            GP09193
         USING DTMVVRS,R3    DECLARE FAKE VVRS RECORD           GP09193
COPYXCOM SR    R15,R15       DATA SET LENGTH                    GP09193
         SR    R14,R14       CATALOG LENGTH                     GP09193
         IC    R15,CELLALIL  GET LENGTH OF DSN                  GP09193
         LA    R15,1(,R15)     FINAGLE ??                       GP09193
         IC    R14,CSPRTYP   GET LENGTH OF CDSN                 GP09193
         LA    R0,12(R14,R15)   LENGTH OF NAME SEGMENT          GP09193
         STCM  R0,3,DTMVL2   SET LENGTH OF N SEGMENT            GP09193
         LA    R0,L'DTMREST+14(R14,R15)  LENGTHS + OHD          GP09193
         STCM  R0,3,DTMVL1   SET TOTAL LENGTH OF RECORD         GP09193
         MVC   DTMVT1,=XL6'D50000000000'                        GP09193
         STC   R15,DTMVDSL   DSN LENGTH                         GP09193
         BCTR  R15,0           FINAGLE ??                       GP09193
EXMVCCDS MVC   DTMVDSN(0),CELLALI   MOVE DSN                    GP09247
         EX    R15,EXMVCCDS  MOVE DSN (+1 GARBAGE)              GP09193
         LA    R15,DTMVDSN(R15)  POINT PAST DSN                 GP09193
         XC    0(2,R15),0(R15)   MAKE SURE IT'S CLEAR           GP09193
         STC   R14,2(,R15)       SET CATALOG DSN LENGTH         GP09193
         EX    R14,EXMVCCAT      MOVE CATALOG NAME              GP09193
         LA    R15,3(R14,R15)    PAST CATALOG NAME              GP09193
         MVI   0(R15),0          CLEAR                          GP09193
         MVC   1(L'VVRSREST,R15),VVRSREST  MOVE REST            GP09193
         POP   USING                                            GP09193
         BAL   R9,TAPEOUT         WRITE THE DATA SET HEADER
         B     COPYTGET                                         GP09193
         SPACE 1
EXMVCCAT MVC   3(0,R15),CSPRCAT  MOVE CATALOG NAME              GP09193
         SPACE 1
*---------------------------------------------------------------------*
*   Loop through extents and write track images.                      *
*---------------------------------------------------------------------*
         SPACE 1
COPYTGET LM    R4,R5,CUREXT  GET CURRENT EXTENT AND COUNT       GP09247
COPYTRAK STM   R4,R5,CUREXT  SAVE CURRENT EXTENT AND COUNT LEFT GP09247
         CLI   0(R4),0       NULL EXTENT?
         BE    COPYDONE      YES; DONE WITH THIS DATA SET
COPYTTWO L     R2,@BUFCUR    GET CURRENT SLOT
         L     R0,@BUFEND
         SR    R0,R2         ROOM LEFT IN CURRENT BUFFER
         CH    R0,=H'40'     ARBITRARY MINIMUM
         BNL   COPYSOME
         BAL   R9,TAPEOUT    WRITE OUT THE CURRENT BUFFER
         L     R2,@BUFCUR    LOAD NEW BUFFER START
COPYSOME BAL   R9,BUILDHED   MAKE TRACK HEADER
         XC    DTTTRK(DTTSIZE),DTTTRK   CLEAR UNUSED
         MVI   DTPRCID1,DTPDATA   TRACK RECORD
         MVC   DTTCCHH,CURCCHH    COPY ADDRESS
         VOLREAD TRACK,CURCCHH    READ NEXT TRACK
         LR    R6,R1
         USING MAPVOLRD,R6
         BXLE  R15,R15,COPYTROK   HAVE IT
         OI    DTTTRKID,DTTIOER   SET FOR ERROR
         OICC  4,8           SET BAD TRACK
         PRTDATA '*** Track',(CURCCHH,HEX,PAD),'failed ***'
COPYTROK SR    R5,R5         SIZE FOR I/O ERROR
         TM    DTTTRKID,DTTIOER   TEST FOR ERROR
         BNZ   *+8           YES; NO DATA
         A     R5,TRKCURSZ
         LA    R4,DB         ANY VALID ADDRESS (FOR I/O ERROR)
         LA    R1,8(,R5)     ALLOW FOR R0 DATA
         STH   R1,DTTTRKLN   DATA LENGTH
         LA    R1,DTPSIZE+DTTSIZE(,R5)      FULL TRACK SIZE
         STH   R1,DTPSEGLN   STASH FOR FIT
         INC   DSN#TRAK      COUNT TRACKS WRITTEN
         TM    DTTTRKID,DTTIOER   TEST FOR ERROR
         BNZ   COPYNOST      YES; NO DATA
         L     R4,TRK@DATA   FIRST COUNT FIELD
         L     R14,TRK#BLOK  RECORDS ON TRACK
         A     R14,DSN#REC   COUNT
         ST    R14,DSN#REC   BLOCKS WRITTEN
         TM    OPTFLAGS,FGPSPO    SEQUENTIAL ?
         BNZ   COPYNOST      YES; DON'T NEED R0
         MVC   DTTR0DAT,TRKR0DAT   R0 DATA
COPYNOST LA    R14,DTTCOUNT  TRACK START
         L     R15,@BUFEND   END OF BUFFER
         SR    R15,R14       SIZE LEFT IN BUFFER
         CR    R5,R15        DOES TRACK FIT ?
         BNH   COPYMOVE
         L     R0,@BUFEND
         SR    R0,R2         HEADER+DATA LENGTH
         STH   R0,DTPSEGLN   SET PARTIAL SIZE
         LR    R1,R5         RESIDUAL SIZE
         SR    R1,R15        LESS CURRENT BUFFER
         SR    R0,R0
         D     R0,#DATMAX    DATA SIZE OF ONE BUFFER
         LTR   R0,R0         ANY REMAINDER
         BZ    *+8           NO
         LA    R1,1(,R1)     PARTIAL
         LA    R1,1(,R1)     PLUS CURRENT = NUMBER OF SEGMENTS
         STC   R1,DTPNOSEG   FIRST SEGMENT OF #
COPYSPLT MVC   DB3,DTPNOSEG  SAVE IT
         MVCL  R14,R4        COPY PARTIAL TRACK DATA
         ST    R14,@BUFCUR   UPDATE FOR NEXT TIME
         LTR   R5,R5         ANYTHING LEFT ?
         BNP   COPYBUMP      NO; DO NEXT TRACK
         BAL   R9,TAPEOUT    WRITE FIRST/NEXT BUFFER
         L     R2,@BUFCUR    NEXT AVAILABLE ADDRESS
         BAL   R9,BUILDHED   MAKE NEW HEADER
         MVC   DTPNOSEG(8),DB3   SET OLD HEADER INFO
         SR    R1,R1
         IC    R1,DTPSEGNO   PREVIOUS SEGMENT
         LA    R1,1(,R1)     INCREASE
         STC   R1,DTPSEGNO   AND WRITE BACK
         LA    R14,DSSHEDND  DATA START
         L     R15,#DATMAX   SIZE IN THIS BUFFER
         LR    R1,R15
         MIN   R1,(R5)       MAX MOVEABLE
         LR    R15,R1        COPY
         LA    R1,DTPSIZE(R1)  SIZE OF THIS PORTION
         STH   R1,DTPSEGLN   SET SIZE
         B     COPYSPLT      WRITE NEXT SEGMENT
COPYMOVE LR    R15,R5        NOT MORE THAN WHAT'S AVAILABLE
         MVCL  R14,R4        COPY TRACK DATA
         ST    R14,@BUFCUR   FOR NEXT TIME
         SPACE 1
COPYBUMP BAL   R9,TAPEOUT         WRITE BLOCK, IF USED          GP09210
         LM    R4,R5,CUREXT  GET CURRENT EXTENT ADDRESS / COUNT
         CLI   0(R4),X'40'   LABEL TRACK ?
         BE    COPYBLAB      YES; IGNORE IT
         INC   DSN#DATA,WORK=R0   COUNT DATA TRACKS
         C     R0,DSN#TMAX   PAST END?
         BNL   COPYDONE      DONE WITH LOGICAL DATA
COPYBLAB SR    R14,R14
         SR    R15,R15
         ICM   R15,3,CURCCHH+2    CURRENT HEAD ADDRESS
         LA    R15,1(,R15)   UP
         STH   R15,CURCCHH+2      SET IT BACK
         CLC   CURCCHH,6(R4)      IN CURRENT EXTENT ?
         BH    COPYBUMR             NO; GET NEXT EXTENT
         C     R15,NUMPCYL        IN CURRENT CYLINDER ?
         BL    COPYTRAK             YES; GET IT                 GP09238
         STH   R14,CURCCHH+2      ELSE SET TO ZERO
         ICM   R14,3,CURCCHH      GET CYLINDER
         LA    R14,1(,R14)        BUMP IT
         STH   R14,CURCCHH        STASH BACK
         CLC   CURCCHH,6(R4)      IN CURRENT EXTENT ?
         BNH   COPYTRAK             YES; GET THIS TRACK         GP09238
COPYBUMR LA    R4,10(,R4)         NEXT EXTENT
         MVC   CURCCHH,2(R4)      SET NEXT ADDRESS
*PRTDATA '>>> Extent',(0(R4),2,HEX,PAD),(2(R4),4,HEX),(6(R4),4,HEX,PAD)
         SH    R5,=H'1'           IS THERE ONE ?
         BP    COPYTRAK           YES; DO IT                    GP09238
COPYDONE BAL   R9,TAPEOUT         WRITE BLOCK, IF USED
         SPACE 1
         PRTLIST FDDSDONE
         INC   NUMCOPY
         LA    R5,2          TRAILER WRITTEN TWICE
COPYTRAL L     R2,@BUF       GET BUFFER START
         BAL   R9,BUILDHED   BUILD A HEADER RECORD
         LA    R4,DTRSIZE+DTPSIZE  SIZE OF HEADER + TRAILER
         STCM  R4,3,DTPSEGLN UPDATE LENGTH
         AR    R4,R2         BUFFER USED
         ST    R4,@BUFCUR    SET IT
         MVI   DTPRCID1,DTPDTRLR  IDENTIFY TRAILER
         XC    DTRDLR,DTRDLR      CLEAR DATA
         BAL   R9,TAPEOUT    WRITE FINAL RECORD
         BCT   R5,COPYTRAL
         B     COPYNEXT           TRY ANOTHER
         SPACE 1
FDDSDONE FDPRT '     ',NL
         FDPRT GENERIC,PAD
         FDPRT CELLVOL
         FDPRT CELLDSN,PAD
         FDPRT 'dumped'
         FDPRT DSN#TRAK,I,PAD,RADJ,LEN=10
         FDPRT 'Tracks,'
         FDPRT DSN#REC,I,PAD,RADJ,LEN=10
         FDPRT 'Blocks'
         FDCLC CELLDSN,CELLALI,BE=FDDSDONX
         FDPRT 'as',NL,RADJ,LEN=14                              GP09197
         FDPRT CELLALI,PAD
FDDSDONX FDPRT *END
         DROP  R6
         SPACE 1
COPYTEND B     PGMEXIT
         SPACE 1
COPYBAD1 PRTDATA '*** FMT1 read on',(CELLVOL,PAD),'failed'
         B     COPYBADC
         SPACE 1
COPYBADV PRTDATA '*** VTOC read for',(CELLVOL,PAD),'failed'
COPYBADC PRTDATA '    ',(CELLDSN,PAD),'not dumped'
         INC   NUMFAIL
         B     COPYNEXT
         SPACE 1
*---------------------------------------------------------------------*
*   BUILD A SEGMENT HEADER AT (R2)                                    *
*     NOTE THAT HEADER TYPE AND SEGMENT LENGTH WILL BE COMPLETED      *
*     BY THE CALLER                                                   *
*---------------------------------------------------------------------*
BUILDHED INC   OUT#SEQ,WORK=R0    INCREASE SEGMENT COUNT
         ST    R0,DTPSEQNO   SET SEGMENT
         MVI   DTPNOSEG,1    SEGMENTS/RECORD
         MVI   DTPSEGNO,1    SEGMENT IN RECORD
         LA    R0,DTPSIZE    PROVISIONAL SIZE
         STC   R0,DTPPFXLN   PREFIX LENGTH
         STCM  R0,3,DTPSEGLN   SEGMENT LENGTH
         MVI   DTPDMPID,DTPLOGCL  LOGICAL DUMP
         MVI   DTPRCID1,DTPDATA   SET MOST FREQUENT
         MVI   DTPRCFL1,0    NO DISPLACEMENT
         XC    DTPRESVD,DTPRESVD  RESERVED WORD
         BR    R9
         SPACE 1
*---------------------------------------------------------------------*
*   FIX DATES APPEARING TO BE PRIOR TO 1964                           *
*---------------------------------------------------------------------*
FIXDATE  ICM   R14,7,0(R1)   IS THE DATE ALL ZERO ?             GP09194
         BZR   R9            YES; LEAVE IT ALONE                GP09194
         SRL   R14,16        ISOLATE YEAR                       GP09194
         CH    R14,=H'64'    IS IT 1964 OR LATER ?              GP09194
         BNLR  R9            YES; KEEP IT                       GP09194
         LA    R14,100(,R14)    CHANGE 1900 -> 2000             GP09194
         STC   R14,0(,R1)    UPDATE IT                          GP09194
         BR    R9            RETURN TO CALLER                   GP09194
         SPACE 1
*---------------------------------------------------------------------*
*   TAPE OUTPUT ROUTINE                                               *
*   FOR BSAM USE PLAIN WRITE WITH NO FRILLS                           *
*                                                                     *
*   FOR EXCP, USE WRITE/NOP CCW. NOTE THAT I CAN'T GET UNIT EXCEPTION *
*   WHEN END OF TAPE MARKER IS PASSED. INSTEAD THE TAPE SWITCH IS     *
*   TRIGGERED BY AN EQUIPMENT CHECK (DDR IS INHIBITED).               *
*---------------------------------------------------------------------*
TAPEOUT  L     R2,@BUF
         L     R3,@BUFCUR
         SR    R3,R2
TAPEOUTR STM   R2,R3,SUBSAVE
         LTR   R3,R3         VALID LENGTH ?                     GP09207
         BNP   TAPEOUTY      PROGRAM ERROR
         TM    OPTFLAGS,FGVAR     RECFM=V OR U ?
         BZ    TAPEOUTL      U
         SH    R2,=H'8'      BACK UP TO BDW
         MVC   HOLDBDW,0(R2)     PRESERVE OLD DATA, IF ANY      GP09207
         LA    R15,4(,R3)    LENGTH FOR RDW
         LA    R14,4(,R15)   LENGTH FOR BDW
         LR    R3,R14        NEW WRITE LENGTH
         SLDL  R14,16        ALIGN
         STM   R14,R15,0(R2)  BUILD BDW/RDW
TAPEOUTL TM    OPTFLAGS,FGTEST    RUNNING IN TEST MODE ?
         BNZ   TAPEOUTX      YES; BYPASS I/O
         TM    DCBMACRF-IHADCB+TAPEDCB,DCBMRECP  EXCP MODE?
         BNZ   XDAPST        USING EXCP
         WRITE TAPEDECB,SF,TAPEDCB,(R2),(R3),MF=E   WRITE A BIG BLOCK
         CHECK TAPEDECB          WAIT FOR COMPLETION
         B     TAPEOUTX
         SPACE 1
XDAPST   STH   R3,TAPECCW+6
         STCM  R2,7,TAPECCW+1     WRITE FROM TEXT
         ZI    DCBIFLGS-IHADCB+TAPEDCB,DCBIFEC   ENABLE ERP
         OI    DCBIFLGS-IHADCB+TAPEDCB,X'40'     SUPPRESS DDR
         STCM  R3,12,IOBSENS0-IOBSTDRD+TAPEIOB   CLEAR SENSE
         OI    DCBOFLGS-IHADCB+TAPEDCB,DCBOFLWR  SHOW WRITE DONE
         XC    TAPEECB,TAPEECB
         EXCP  TAPEIOB
         WAIT  ECB=TAPEECB
         TM    TAPEECB,X'7F'      GOOD COMPLETION?
         BNO   TAPEN7F            NO
         B     TAPEOUTX
         SPACE 1
TAPEN7F  TM    IOBUSTAT-IOBSTDRD+TAPEIOB,IOBUSB7  END OF TAPE MARKER?
         BNZ   TAPEEND       YES; SWITCH TAPES
         CLC   =X'1020',IOBSENS0-IOBSTDRD+TAPEIOB  EXCEEDED AWS/HET ?
         BNE   TAPEB001
         INC   DCBBLKCT-IHADCB+TAPEDCB,INC=-1    FIX BLOCK COUNT
         EOV   TAPEDCB       TRY TO RECOVER
 PRTDATA '--- New tape at block',(OUT#BLOK,I,PAD),'***'
         B     XDAPST
         SPACE 1
TAPEB001 LA    R9,TAPEIOB    GET IOB FOR QUICK REFERENCE
         PRTLIST FDTAPIOB    PRINT INFO
         PRTCLOSE
         ABEND 001,DUMP
         SPACE 1
         USING IOBSTDRD,R9   DECLARE IT
FDTAPIOB FDOPT NL,CC=C'0'
         FDPRT '*** TAPE WRITE ERROR; ECB='
         FDPRT TAPEECB,HEX,PADR
         FDPRT '***'
         FDFD  IOBFLAG1,OPTL=NL
         FDFD  IOBFLAG2
         FDFD  IOBSENS0
         FDFD  IOBSENS1
         FDFD  IOBCSW
         FDFD  IOBSTART
         FDFD  IOBINCAM
         FDFD  IOBERRCT
         FDPRT *END
         DROP  R9
         SPACE 1
TAPEEND  INC   DCBBLKCT-IHADCB+TAPEDCB   UPDATE BLOCKCOUNT
         OI    DCBOFLGS-IHADCB+TAPEDCB,DCBOFLWR  SHOW WRITE DONE
         EOV   TAPEDCB            GET ANOTHER TAPE
         B     TAPEOUTX      CONTINUE ON NEW TAPE
         SPACE 1
TAPEOUTX TM    OPTFLAGS,FGVAR     RECFM=V OR U ?                GP09207
         BZ    TAPEOUT2      U                                  GP09207
         MVC   0(8,R2),HOLDBDW    RESTORE OLD DATA              GP09207
TAPEOUT2 LM    R2,R3,SUBSAVE
         INC   OUT#BLOK      INCREMENT BLOCK COUNT
         STMAX R3,OUT#MAX    SAVE LARGEST BLOCK SIZE
         A64F  OUT#BYTE,(R3)   COUNT OUTPUT BYTES
TAPEOUTY L     R2,@BUF
         ST    R2,@BUFCUR    RESET BUFFER USE
         BR    R9
         SPACE 1
         LTORG ,
         TITLE 'D S S D U M P  ***  SUBROUTINES AND DATA'
DSSDUMPD CSECT ,             SUBROUTINE AND DATA AREA
         DROP  R11,R12       USING R10 FOR THIS
PGMEXIT8 PRTL  '0*** Unable to continue due to errors',CC=ASA
         OICC  8
         SPACE 1
PGMEXIT  ICM   R0,15,NUMCOPY   CHECK DATA SETS COPIED
         BZ    PGMEXWRN      THAT'S NOT GOOD ?
         S     R0,NUMPICK     COMPARE TO COUNT SELECTED
         BZ    PGMEXIT1      OK
PGMEXWRN LA    R0,4
         STMAX R0,RETCODE    SET WARNING CODE
PGMEXIT1 TM    TAPEDCB+DCBOFLGS-IHADCB,DCBOFOPN
         BZ    PGMEXIT2      TAPE OPEN ?
         TM    OPTFLAGS,FGWRITE   OUTSTANDING WRITE ?
         BZ    PGMEXCLS
         CHECK TAPEDCB       CHECK LAST BLOCK
         ZI    OPTFLAGS,FGWRITE   OUTSTANDING WRITE
PGMEXCLS CLOSE MF=(E,OCLIST)    YES; CLOSE IT
         L     R15,OUT#MAX        IN CASE CAN'T DIVIDE
         ICM   R0,15,OUT#BLOK     GET BLOCK COUNT
         BZ    PGMEXWAT           USE MAX AS AVERAGES
         LM    R14,R15,OUT#BYTE   GET OUTPUT BYTE COUNT
         DR    R14,R0             GET AVERAGE BLOCK SIZE
PGMEXWAT ST    R15,OUT#AVG          AND SAVE IT
         PRTROOM 6
         PRTLIST FDSTAT      PRINT TAPE STATISTICS
PGMEXIT2 PRTROOM 6
         PRTLIST FDFINAL
         BAL   R14,ENDTREE   TERMINATE TREE IF NOT DONE EARLIER
         SUBCALL SUBCOMP,('END',,,CMP@STOR),VL,MF=(E,COMPPARM)
         PRTCLOSE DEV=255    LFETCH=LINK DOESN'T CLOSE
         SERVTERM ,          CLOSE AND FREE EVERYTHING
         PGMEXIT COPYRET=(RETCODE,8)   RETURN R15 & R0
         SPACE 1
INITREE  SUBCALL SUBTREE,('INI',ROOTBALL),VL,MF=(E,CALLPARM)
         ORG   *-2           OVERLAY BALR
         BR    R15           RETURN TO CALLER
         SPACE 1
ENDTREE  SUBCALL SUBTREE,('END',ROOTBALL),VL,MF=(E,CALLPARM)
         ORG   *-2           OVERLAY BALR
         BR    R15           RETURN TO CALLER
         SPACE 1
LISTREE  SUBCALL SUBTREE,('LIST',ROOTBALL),VL,MF=(E,CALLPARM)
         ORG   *-2           OVERLAY BALR
         BR    R15           RETURN TO CALLER
         SPACE 1
NEXTREE  SUBCALL SUBTREE,('NEXT',ROOTBALL),VL,MF=(E,CALLPARM)
         ORG   *-2           OVERLAY BALR
         BR    R15           RETURN TO CALLER
         SPACE 2
***********************************************************************
**  WHATMASK - LOOK AT WHATEVER USER IS PASSING OFF AS A MASK, AND   **
**    CLASSIFY IT AS NAME, MASK, or POSITIONAL MASK                  **
**                rc  0      4          8                            **
**              flag  00    80         90                            **
***********************************************************************
WHATMASK STM   R1,R5,SUBSAVE SAVE A LITTLE
         LR    R5,R1         PRESERVE MASK ADDRESS
         MVI   L'COMPMASK(R5),X'00'    SET FOR NAME
         SUBCALL SUBCOMP,('AST',0(R5),0(R5),CMP@STOR),VL,              *
               MF=(E,CALLPARM)
*DEFER*  CH    R15,=H'8'     IS MASK VALID ?
*DEFER*  BNL   WHATMBAD
         SR    R15,R15       SET RETURN CODE
         TRT   0(L'COMPMASK,R5),TRTMASK   ANY MASK CHARACTERS?
         BZ    WHATMEXT      NO; RETURN NAME
         MVI   L'COMPMASK(R5),X'80'    SHOW MASK
         SR    R2,R2         CLEAR FOR TRT IC
         TRT   0(L'COMPMASK,R5),TRTMASKP  ANY POSITIONALS ?
         LA    R15,4(,R2)    SET MASK=4, POSITIONAL=8
         BZ    WHATMEXT      NO
         OI    L'COMPMASK(R5),X'10'    SHOW POSITIONAL
WHATMEXT LM    R1,R5,SUBSAVE
         BR    R9            RETURN TO CALLER
         SPACE 1
*---------------------------------------------------------------------*
*   PHASE I DATA                                                      *
*---------------------------------------------------------------------*
         DS    0D            FORCE ALIGNMENT                    GP09207
PATTAPE  DCB   DDNAME=TAPE,MACRF=W,DSORG=PS,EXLST=TAPEXLST  WAS RECFM=U
         WRITE PATDECB,SF,TAPEDCB-TAPEDCB,2-2,3-3,MF=L   WRITE A BLOCK
PATTAPEL EQU   *-PATTAPE
TAPEXLST DC    0A(0),X'85',AL3(TAPEMERG)
         DS    0D            ALIGN CCW(S)
PATEXCP  DCB   DDNAME=TAPE,MACRF=E,DSORG=PS,REPOS=Y
PATCCW   CCW   1,2-2,X'40',3-3
         CCW   3,2-2,X'20',1
PATXLEN  EQU   *-PATEXCP     PATTERN TO MOVE
         SPACE 1
PATALLOC DS    0F            ALLOCATION REQUEST BLOCK
         DC    AL1(PATTUPTR-PATALLOC)   RB LENGTH
         DC    AL1(S99VRBAL)   ALLOCATE
         DC    AL1(0,0)      (NO) FLAGS
         DC    AL4(0)        ERROR/INFO CODE RETURN
         DC    AL4(PATTUPTR) TEXT UNIT POINTER LIST ADDRESS
         DC    AL4(0)        RB EXTENSION
         DC    4AL1(0)       MORE FLAGS
PATALLEN EQU   *-PATALLOC    LENGTH TO MOVE
PATTUPTR DC    A(TUPDDN)     SPECIFY DDN
         DC    AL1(X'80'),AL3(TUPDUM) AND REQUEST DUMMY DD
TUPDDN   DC    AL2(DALDDNAM,1,8)    DDN 1 ENTRY, LEN 8
DCVOLMNT DC    CL8'VOLMOUNT'
TUPDUM   DC    AL2(DALDUMMY)
         SPACE 1
TRTMASK  DC    256AL1(0)     ALLOW ALL CHARACTERS
         TRENT TRTMASK,4,C'*',C'?',  NORMAL WILD CARD
         TRENT ,8,C'%'        POSITIONAL WILD CARD
         SPACE 1
TRTMASKP DC    256AL1(0)     ALLOW ALL CHARACTERS
         TRENT TRTMASKP,4,C'%'   POSITIONAL WILD CARD ?
         SPACE 1
TRTDSNM  DC    256AL1(8)     FAIL ALL
         TRENT TRTDSNM,0,(C'A',9),(C'J',9),(C'S',8),  ALPHA
         TRENT ,0,(C'0',10),C'@',C'#',C'$', -MERIC + INTEGER
         TRENT ,0,C'?',C'*',C'.',C'%',        INDEX + MASKING
         TRENT ,4,C' '       SCAN STOPPER
         SPACE 1
PATIOWK  DS    0F            START OF WORK PATTERN              GP09194
PATPRINT PRTWORK SYSPRINT,SYSOUT,TITLE=3                        GP09194
PATIN    INPWORK SYSIN,EODAD=CARDEOD,EDIT=128                   GP09194
PATDDNM  DC    CL8'TAPE'                                        GP09194
PATIOLEN EQU   *-PATIOWK     LENGTH TO MOVE                     GP09194
VVRSREST DC    X'0054220000000000000000000048240002000000000000000008E2*
               E3C1D5C4C1D9C400000008E2E3C1D5C4C1D9C4'          GP09193
         SPACE 1
QMAJ     DC    CL8'SYSDSN '                                     GP09212
QJAM     DC    CL8'SPFEDIT'                                     GP09212
PATENQ   ENQ   (QMAJ,1-1,E,44,SYSTEM),RET=TEST,MF=L             GP09212
PATENQL  EQU   *-PATENQ                                         GP09212
         SPACE 1
FDFINAL  FDOPT NL,CC=C'0'    DOUBLE SPACE
         FDPRT 'Completion code',NL
         FDPRT RETCODE,I,PAD
         FDPRT NUMPICK,I,RADJ,NL,LEN=12
         FDPRT 'Data Sets selected',PAD
         FDPRT NUMSKIP,I,RADJ,NL,LEN=12
         FDPRT 'Data Sets skipped',PAD
         FDPRT NUMFAIL,I,RADJ,NL,LEN=12
         FDPRT 'Data Sets failed',PAD
         FDPRT NUMCOPY,I,RADJ,NL,LEN=12
         FDPRT 'Data Sets copied',PAD
         FDCLC NUMDUPE,ZEROES,BE=FDFINALX
         FDPRT NUMDUPE,I,RADJ,NL,LEN=12
         FDPRT 'Data Sets renamed',PAD
FDFINALX FDPRT *END
         SPACE 1
FDSTAT   FDOPT NL,CC=C'0'    DOUBLE-SPACE
         FDPRT OUT#BLOK,I,RADJ,NL,LEN=12
         FDPRT 'Blocks written',PAD
         FDPRT OUT#SEQ,I,RADJ,NL,LEN=12
         FDPRT 'Segments written',PAD
         FDPRT OUT#MAX,I,RADJ,NL,LEN=12
         FDPRT 'Maximum block size',PAD
         FDPRT OUT#AVG,I,RADJ,NL,LEN=12
         FDPRT 'Average block size',PAD
         FDPRT *END
         SPACE 1
*---------------------------------------------------------------------*
*   BSAM DCB OPEN EXIT                                                *
*   USE BLKSIZE FROM JFCB OR DSCB, BUT NO LESS THAN MINBLOCK          *
*   AND NO MORE THAN TRACK SIZE (OR HALF-TRACK FOR MODULO DEVICES)    *
*---------------------------------------------------------------------*
         PUSH  USING
         DROP  ,
         USING TAPEMERG,R15
         USING IHADCB,R1
TAPEMERG L     R3,#BUFMAX-TAPEDCB(,R1)    CURRENT BUFMAX        GP09207
         SR    R2,R2
         ICM   R2,3,DCBBLKSI   GET JFCB OR DSCB SIZE
         BNZ   *+6
         LR    R2,R3         USE DEFAULT
         MIN   R2,(R3)       USE SMALLER VALUE
         MAXH  R2,=AL2(MINBLOCK)   BUT AT LEAST 1K              GP09207
         LR    R3,R2         ALSO UPDATE BUFMAX
         TM    DCBRECFM,X'C0'     RECFM=U?
         BO    TAPEMERX      YES; JUST SET BLOCK SIZE
         BM    TAPEMERV      F OR V (F WILL FAIL)
         OI    DCBRECFM,X'C0'     SET =U
         B     TAPEMERX          AND EXIT
TAPEMERV SH    R3,=H'4'      LESS FOUR FOR RDW
         STH   R3,DCBLRECL
         SH    R3,=H'4'      LESS FOUR FOR BDW
TAPEMERX STH   R2,DCBBLKSI
         ST    R3,#BUFMAX-TAPEDCB(,R1)    UPDATE BUFMAX         GP09207
         BR    R14           RETURN TO OPEN
         POP   USING
         SPACE 1
         LTORG ,
         TITLE 'D S S D U M P  ***  DYNAMIC DATA AND MAPPINGS'
CELLSECT DSECT ,             TREE DEFINITION
CELLVOL  DS    CL6           VOLUME SERIAL
CELLDSN  DS    CL44          DATA SET NAME
CELLKEY  EQU   CELLVOL,L'CELLVOL+L'CELLDSN,C'C'  KEY
CELLALIL DS    X             LENGTH OF ALIAS NAME               GP09197
CELLALI  DS    CL44          ALIAS SDATA SET NAME
CELLFLAG DS    X             FLAGS
CFPICK   EQU   X'01'           TO BE DUMPED
CFFAIL   EQU   X'02'           FAILED
CFSKIP   EQU   X'04'           NOT SELECTED
CFENQ    EQU   X'08'           ENQ FAILED
CFDUMP   EQU   X'80'           DUMPED SUCCESSFULLY
CELLLEN  EQU   *-CELLSECT    LENGTH OF ONE ENTRY
         SPACE 1
DSSDUMPD CSECT ,             DEFINE AFTER CELL
ROOTPATT SERVTREE PFX=PAT,KEYLEN=L'CELLKEY,KEYOFF=CELLKEY-CELLSECT,    *
               RECLEN=CELLLEN,RECNUM=(64*1024-48)/CELLLEN  USE 64K
BLANKS   EQU   PATREC,CELLLEN,C'C'     REDEFINE
         SPACE 1
SAVE     DSECT ,
ZEROES   DS    D             CONSTANT
DB       DS    D
DB2      DS    D
DB3      DS    D
         SERVDEFS ,          DEFINE NEEDED CONSTANTS
DYNIOWK  DS    0F            START OF WORK PATTERN              GP09194
SYSPRINT PRTWORK SYSPRINT,SYSOUT,TITLE=3                        GP09194
SYSIN    INPWORK SYSIN,EODAD=CARDEOD,EDIT=128                   GP09194
SYSDDNM  DC    CL8'TAPE'                                        GP09194
DYNIOLEN EQU   *-DYNIOWK     LENGTH TO MOVE                     GP09194
@VOLREAD DS    A             VOLUME READER
NUMCOPY  DS    F             NUMBER OF DATA SETS COPIED
NUMPICK  DS    F             NUMBER OF DATA SETS SELECTED
NUMSKIP  DS    F             NUMBER OF DATA SETS SKIPPED
NUMFAIL  DS    F             NUMBER OF DATA SETS FAILED
NUMDUPE  DS    F             NUMBER OF DUPLICATE NAMES REPLACED
NUMPFX   DS    F             NUMBER OF PREFIX/RENAME ENTRIES    GP09197
         SPACE 1
SUBSAVE  DS    5A
         SPACE 1
COMPPARM DS    4A            SUBCOMP SUBROUTINE PARAMETERS
CMP@STOR DC    A(0)          MORE STORAGE
COMPMASK DS    CL44,X        (CURRENT) COMPARE MASK
         SPACE 1
ROOTBALL SERVTREE PFX=REC,KEYLEN=L'CELLKEY,KEYOFF=CELLKEY-CELLSECT,    *
               RECLEN=CELLLEN,RECNUM=(64*1024-48)/CELLLEN  USE 64K
         SPACE 1
VOLVOL   DS    CL6           SERIAL LAST USED IN VOLREAD OPEN
         SPACE 2
*     PARAMETER LIST AND WORK AREA
*
***********************************************************************
**   THESE FIELDS MUST STAY CONTIGUOUS FOR CLEARING                  **
**                                                                   **
         CATSPARM PFX=CSP,DSECT=   SUBCAT INTERFACE PARAMETER AREA
#SUBCAT  EQU   CSP@SCAT,4,C'V'
#SUBCOMP EQU   CSP@SCMP,4,C'V'
CATCALL  SUBCALL (1,2,3),MF=L  SUBCAT REMOTE PARAMETER LIST
CMP@WORK DC    A(0)          ADDRESS OF GETMAINED WORK AREA
CALLLIST DC    A(CMPREQ,CSPMASK,DSNMASK,CMP@WORK)
GENERIC  DS    CL8           GENERIC UNIT NAME FOR CURRENT VOLUME
DSNMASK  DC    CL44' '       USER'S REQUESTED DS NAME OR MASK
VOLMASK  DC    CL6' ',CL2' '    REFERENCED VOLUME (COMPARE PAD)
CMPREQ   DC    C'DSN'        COMPARE DSN TO MASK (DSN OR POS)
         SPACE 1
DUMPDSN  DC    CL44' '       CURRENT NON-TAPE DUMP DSN          GP09317
DUMPVOL  DC    CL6'------'     AND VOLUME SERIAL                GP09317
         SPACE 1
OUT#BYTE DS    D             OUTPUT BYTES
OUT#BLOK DS    F             OUTPUT BLOCKS
OUT#SEQ  DS    F             OUTPUT SEGMENTS
OUT#MAX  DS    F             LARGEST BLOCK WRITTEN
OUT#AVG  DS    F             AVERAGE BLOCK WRITTEN
         SPACE 1
DSN#TRAK DS    F       1/5   DATA SET TRACKS (PHYSICAL)
DSN#TMAX DS    F       2/5   MAX TO PROCESS (DS1LSTAR+1)
DSN#DATA DS    F       3/5   DATA SET TRACKS (LOGICAL)
DSN#REC  DS    F       4/5   DATA SET RECORDS
NUMPCYL  DS    F       5/5   TRACKS PER CYLINDER
CURCCHH  DS    0XL4,2XL2     CURRENT TRACK ADDRESS
CUREXT   DS    AL4           ADDRESS OF CURRENT EXTENT
CUR#EXT  DS    FL4           EXTENTS LEFT TO DO
         SPACE 1
         MAPPARSE DSECT=NO   @PARSER INVOCATION ADDRESS
         SPACE 1
OPTFLAGS DS    X             PROCESSING OPTIONS
FGENQ    EQU   X'80'           GET ENQUEUE WHILE DUMPING DS
FGVAR    EQU   X'40'           USE RECFM=V FOR OUTPUT
FGPSPO   EQU   X'20'           CURRENT DATA SET IS PS OR PO
FGADAT   EQU   X'10'           DUMP ALL ALLOCATED TRACKS
FGTEST   EQU   X'08'           TEST MODE - NO OUTPUT WRITTEN
FGBUG    EQU   X'04'           DEBUG MODE - LOTS OF OUTPUT
FGEXP    EQU   X'02'           EXPORTING DATA - RESET LOCALS    GP09197
FGWRITE  EQU   X'01'           UNCHECKED WRITE OUTSTANDING
         SPACE 1
PROFLAGS DS    X             PROCESSING OPTIONS
PFGDUMP  EQU   X'80'           DUMP REQUEST PENDING
PFPREF   EQU   X'20'           PREFIX (ONLY) FORM OF RENAME     GP09197
PFSOME   EQU   X'02'           NOT USING ALLDATA FOR THIS DS    GP09202
PFGONCE  EQU   X'01'           AT LEAST ONE DUMP REQUEST
         SPACE 1
OCLIST   OPEN  (TAPEDCB,OUTPUT),MF=L
         SPACE 1
         DS    0D            ALIGN CCW(S)
TAPEDCB  DCB   DDNAME=TAPE,MACRF=W,DSORG=PS
         WRITE TAPEDECB,SF,TAPEDCB,1-1,2-2,MF=L  WRITE A BLOCK
TAPELEN  EQU   *-TAPEDCB
         SPACE 1
         ORG   TAPEDCB
TAPEEXCP DCB   DDNAME=TAPE,MACRF=E,DSORG=PS,REPOS=Y
TAPECCW  CCW   1,3-3,X'40',4-4
         CCW   3,3-3,X'20',1
TAPEXLEN EQU   *-TAPEEXCP    PATTERN TO MOVE
TAPEECB  DC    A(0)
TAPEIOB  DC    X'42,00,00,00'
         DC    A(TAPEECB)
         DC    2A(0)
         DC    A(TAPECCW)
         DC    A(TAPEDCB)
         DC    2A(0)
         SPACE 1
         ORG   ,             LONGER OF BSAM AND EXCP DATA
         SPACE 1
@BUF     DS    A             ADDRESS OF TAPE BUFFER
#BUFMAX  DS    F             (LOGICAL) SIZE OF BUFFER
#DATMAX  DS    F             MAXIMUM TRACK DATA SIZE IN BUFFER
@BUFCUR  DS    A             NEXT AVAILABLE SPACE
@BUFEND  DS    A             (LOGICAL) BUFFER END
@MEMCUR  DS    A             DITTO FOR DS NAME TABLE
@MEMEND  DS    A             (PHYSICAL) BUFFER END
         SPACE 1
ENQLIST  ENQ   (QMAJ,1-1,E,44,SYSTEM),RET=TEST,MF=L             GP09212
         ORG   ENQLIST                                          GP09212
LOCFMT1  DS    (44+96)X      FORMAT 1 DSCB
LOCFMT2  DS    (140)X        FORMAT 2 DSCB
LOCFMT3  DS    (140)X        FORMAT 3 DSCB
         SPACE 1
EXCMASKS DS    16CL45  1/2   EXCLUSION MASKS FOR ONE SELECT
EXC#MASK DS    F       2/2   CURRENT EXCLUSION MASK REQUESTS
         SPACE 1
PFXOLDL  DS    X             LENGTH OF OLD PREFIX (0<= <23)     GP09197
PFXOLD   DS    CL23          OLD PREFIX (0<= <23)               GP09197
PFXNEWL  DS    X             LENGTH OF NEW PREFIX (0<= <23)     GP09197
PFXNEW   DS    CL23          NEW PREFIX (0<= <23)               GP09197
PFXLEN   EQU   *-PFXOLDL     ONE RENAME ENTRY                   GP09197
         DS    (15)XL(PFXLEN)     16 ENTRIES MAX PER RUN        GP09197
MAXPFX   EQU   (*-PFXOLDL)/PFXLEN   ENTRIES ASSEMBLED           GP09197
         SPACE 1
HOLDBDW  DS    CL8           TEMP STORAGE FOR HELD DATA
BUFBDW   DS    2AL4          RECFM=V BDW/RDW PREFIX
BUFTAPE  DS    256XL256      65K TAPE BUFFER
         SPACE 1
SAVEEND  EQU   *
         SPACE 1
         PRINT &PRTSYS
CRUDFMT1 DSECT ,
         IECSDSL1 1
CRUDFMT3 DSECT ,
         IECSDSL1 3
CRUDFMT4 DSECT ,
         IECSDSL1 4
         IEFUCBOB ,
         IEZIOB ,
         IEFTIOT1 ,
         DCBD  DSORG=PS,DEVD=(TA,DA)
         IHADVCT ,
         CVT   DSECT=YES
         IEFZB4D0 ,          SVC 99 RB DEFINITION
         IEFZB4D2 ,          SVC 99 TEXT UNIT DEFS
         SPACE 1
*        NO USABLE IBM AMCBS MACRO
*
AMCBS    DSECT ,
CBSID    DS    CL2           ID
CBSSIZ   DS    AL2           LENGTH
CBSMCSTA DS    A             CCHH OF MASTER CATLG
CBSACB   DS    A             MASTER CAT ACB
CBSCBP   DS    A             C B MANIP
CBSCMP   DS    0A            CAT RTNE
CBSMCUCB DS    A             MASTER CAT UCB
CBSCAXCN DS    A             CAXWA CHAIN
CBSCRACA DS    A             CRA CAXWA CHAIN
CBSCRTCB DS    A             CRA TASK TCB
CBSVSRT  DS    A
CBSVUSE  DS    A             VSRT USE COUNT
CBSVPTR  DS    A             VSRT
CBSFLAGS DS    X             FLAGS
CBSMICF  EQU   X'80'           MAST IS ICF CATALOG
         DS    XL3           SPARE
CBSVVDSA DS    A             VVDS MANAGER
CBSDEVNT DS    A             DEVICE NAME TABLE
CBSVSICN DS    A             IDAVSI CHAIN
CBSFLG1  DS    X             AMCBS FLAGS
CBSCUVSI EQU   X'80'           VSI CHAIN CLEAN-UP REQUIRED
         DS    XL3           SPARE
         SPACE 1
         IGGCAXWA ,          (PVTMACS)
MYJFCB   DSECT ,
         IEFJFCBN ,
         MAPPARST ,
         SPACE 1
         PRINT ON,GEN
         MAPVOLRD ,          RETURN VALUES FROM VOLREAD TRACK
         TITLE 'D S S D U M P  ***  TAPE RECORD DESCRIPTION'
* MAP OF ADRDSSU BLOCKS - ORG TO COMMON HEADER
*
DSSBLOCK DSECT ,
*** Common Header
*
DTPSEQNO DS    F           Segment sequence number
DTPNOSEG DS    XL1         Number of segments per record
DTPSEGNO DS    XL1         Segment number of record
DTPSEGLN DS    XL2         Segment length including prefix
DTPPFXLN DS    XL1         Length of prefix (constant 16)
DTPDMPID DS    XL1         Type of dump
DTPLOGCL EQU   X'10'         Logical dump
DTPRCID1 DS    XL1         Record Identifier 1
DTPTHDR  EQU   X'80'         Tape Header (see DTHDR)
DTPDSNL  EQU   X'40'         DSName/Catalog Lst (see DTLDSN)
DTPDSHDR EQU   X'20'         Dataset Header (see DTDSHDR)
DTPVOLD  EQU   X'10'         Volume Definition (see DTMVOL)
DTPDATA  EQU   X'08'         Data Track (see  DTTTRK)
DTPDTRLR EQU   X'04'         Dataset Trailer (see DTRTLR)
DTPRCFL1 DS    XL1         Flag Byte
DTPDDISP EQU   X'80'         If on, 16 bytes of track data has been
*                            displaced from this segment to the end of
*                            the last segment for this track.
DTPRESVD DS    F                  RESERVED
DTPSIZE  EQU   *-DSSBLOCK      SIZE OF COMMON HEADER
DSSHEDND DS    0F            END OF COMMON HEADER
*
         ORG   DSSHEDND      X'80'
DTHTIMD  DS    CL8         Date & time of dump
DTHIND2  DS    XL1         Dataset type indicators
DTHGNVI  EQU   X'80'         no non-VSAM datasets
DTHGVI   EQU   X'40'         no VSAM datasets
DTHGT64K EQU   X'20'         more than 65535 datasets on volume
DTHLEN   DS    XL2         Header length
DTHVERNO DS    XL1         DSS Version number
DTHLVLNO DS    XL1         DSS modification number (level)
DTHBLKSZ DS    XL2         Maximum blksize
DTHNDS   DS    XL2         Number of datasets in list
DTHIND1  DS    XL1         Indicators
DTHFCMP  EQU   X'80'         file compressed
DTHUNLCD EQU   X'40'         unallocated space dumped
DTHSFER  EQU   X'20'         sphere option
DTHSIZE  EQU   *-DSSHEDND      SIZE OF TAPE HEADER
*
*** Dataset Contents List    X'40'
*
         ORG   DSSHEDND
DTLLEN   DS    XL1         Length of dataset name
DTLCAT   DS    CL44        Catalog DSN
DTLDSN   DS    0CL44       Data Set name 1-44
* ENDED BY X'00' = DTP SIZE
DTLSIZE  EQU   *-DSSHEDND      SIZE OF ONE ENTRY + DSN
*
*** Dataset Header record    X'20'
*
         ORG   DSSHEDND
DTDLEN   DS    XL1         Length of dataset name
DTDCATLN DS    XL1         Length of catalog name
DTDDSORG DS    XL2         Dataset org (from FMT 1)
DTDOPTCD DS    XL1         Dataset option code (from FMT 1)
DTDNVOL  DS    XL1         Number of volumes for dataset
DTDIND   DS    XL1         Dataset indicator
DTDRACFG EQU   X'08'         Generic RACF profile               GP09193
DTDALIAS EQU   X'04'         User catalog alias
DTDSPER  EQU   X'02'         Sphere record follows
DTDSMS   EQU   X'01'         SMS Managed dataset
DTDPSWD  DS    XL8         pswd
         DS    CL44        catlg name
DTDDSN   DS    CL44        dataset name
DTDVCTD  DS    XL1         Volume count for non-vsam
         DS    XL1         VSAM index component vol cnt or zero
DTDIND2  DS    XL1
DTDAIXSP EQU   X'80'       AIX and part of a sphere
DTDCDF   EQU   X'40'       Common format dataset
DTDPDSE  EQU   X'20'       PDSE dataset
DTDNTALL EQU   X'10'       Dumped without ALLD or ALLX
DTDSAI   EQU   X'08'       DS additional information
DTDNOIDX EQU   X'04'       VSAM indexed DS dumped using VALIDATE
DTDPDSET EQU   X'02'       PDSE dumped as track images
DTDSDM   EQU   X'01'       Use system data mover
DTDSIZE  EQU   *-DSSHEDND
*
*** Volume Definition Record (1 per volume)
*
         ORG   DSSHEDND    X'10'
DTMVOL   EQU   *
DTMVSERL DS    CL6         Volume serial number
DTMDEVTY DS    XL4         DEVTYPE (UCBTBYT4)
         DS    XL2
DTMTRKCP DS    XL4         Bytes per track
DTMLOGCY DS    XL2         Cylinders per volume
DTMTRKCY DS    XL2         Tracks per cylinder
         DS    XL2         max compress buffer in words
DTMFLAGS DS    XL2         flags
DTMCVAF  EQU   X'20'         Indexed VTOC                       GP09193
DTM#VVRS DS    XL1         Number of VVRS/NVRS dumped
DTM#DSCB DS    XL1         Number of DSCBs dumped
DTM#EXT  DS    XL1         Number of extents dumped
DTMMODNO DS    XL1         Model number (looks like zero for logical)
DTMDSCB  DS    XL(DS1END-IECSDSL1)  DSCB 1
DTMEXTS  DS    1XL10          1-16 EXTENTS (?)
DTMVVRS  DS    XL126       Not sure what goes in here           GP09193
         ORG   DTMVVRS     REDEFINE                             GP09193
DTMVL1   DS    XL2         Total VVRS LENGTH (x'7E')            GP09193
DTMVL2   DS    XL2         Length of N segment                  GP09193
DTMVT1   DS    XL6'D50000000000'                                GP09193
DTMVDSL  DS    X           Data set name length                 GP09193
DTMVDSN  DS    0CL44       Data set name                        GP09193
DTMVT2   DS    XL2'0000'                                        GP09193
DTMVCSL  DS    X           Catalog  name length                 GP09193
DTMVCSN  DS    0CL44       Catalog  name                        GP09193
DTMVT3   DS    X'00'                                            GP09193
DTMREST  DS    XL84'005422'   Length, x'22' record, zeros       GP09193
DTMSIZE  EQU   *-DSSHEDND
         ORG   ,           RESTORE MAX SIZE                     GP09193
*
*** Data Track Record
*
         ORG   DSSHEDND    X'08'
DTTTRK   EQU   *
DTTTRKLN DS    XL2         Length of data on track
DTTTRKID DS    XL1         Track indicators
DTTIOER  EQU   X'80'         I/O Error
DTTTROVF EQU   X'40'         Last rec on trk is overflow record
DTTTCMP  EQU   X'20'         Track compressed
DTTVFRST EQU   X'10'         First VVDS record
DTTINVT  EQU   X'08'         Invalid track format
DTTSTAT  EQU   X'04'         User statisical record
DTTCCHH  DS    XL4         CCHH of track
         DS    XL4         VSAM stuff
         DS    XL5         reserved
DTTR0DAT DS    XL8         Record 0 data
DTTCOUNT DS    0X            FIRST COUNT FIELD
DTTSIZE  EQU   *-DSSHEDND
*
*** Dataset Trailer record
*
         ORG   DSSHEDND      X'04'
DTRDLR   DS    XL6           ZEROES ?
DTRSIZE  EQU   *-DSSHEDND
*
         END
@@
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB(DSSDUMP),DISP=SHR
