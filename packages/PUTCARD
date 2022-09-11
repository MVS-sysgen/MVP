//PUTCARD JOB (JOB),
//             'INSTALL PUTCARD',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//* -------------------------------------------------------*
//* *  ISPOPT5 for MVS3.8J TSO / Hercules                  *
//* *                                                      *
//* *  JOB: $INST40  Install Other Components              *
//* *                                                      *
//* *       Install PUTCARD  Programs                      *
//* *                                                      *
//* *  - PUTCARD  programs installs to ISPLLIB             *
//* *  - Install libraries marked...                       *
//* *    - Search for '<--TARGET'                          *
//* *    - Update install libraries per your               *
//* *      installation standard                           *
//* *                                                      *
//* -------------------------------------------------------*
//*
//* -------------------------------------------------------*
//* *                                                      *
//* *  PROC: ASMLKED                                       *
//* *       Assembler Link-Edit                            *
//* *                                                      *
//* -------------------------------------------------------*
//ASML     PROC
//*
//ASM      EXEC PGM=IFOX00,
//             PARM='NODECK,LOAD,RENT,TERM,XREF'
//SYSGO    DD  DSN=&&LOADSET,DISP=(MOD,PASS),SPACE=(CYL,(1,1)),
//             UNIT=VIO,DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//         DD  DSN=SYS2.MACLIB,DISP=SHR          ** YREG  **
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSPUNCH DD  DSN=NULLFILE
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(6,1))
//SYSUT2   DD  UNIT=VIO,SPACE=(CYL,(6,1))
//SYSUT3   DD  UNIT=VIO,SPACE=(CYL,(6,1))
//SYSIN    DD  DUMMY
//*
//LKED     EXEC PGM=IEWL,PARM='MAP,LIST,LET,RENT,XREF',
//             COND=(0,NE,ASM)
//SYSLIN   DD  DSN=&&LOADSET,DISP=(OLD,DELETE)
//         DD  DDNAME=SYSIN
//SYSLMOD  DD  DUMMY
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(5,2))
//SYSIN    DD  DUMMY
//*
//         PEND
//*
//* -------------------------------------------------------*
//* *  Assemble Link-Edit PUTCARD to ISPLLIB               *
//* -------------------------------------------------------*
//PUTCARD  EXEC  ASML,
//         PARM.ASM='OBJECT,NODECK,XREF,TERM',
//         PARM.LKED='LET,LIST,MAP,XREF,SIZE=(900K,124K)'
         TITLE 'CNTLCARD-COMMAND TO LOAD SYSIN FROM COMMAND PARM.'
* From mvs9809.pdf on https://www.cbttape.org/xephon
*
* CONTROL CARD IMAGE QUICK BUILDER
*
* Frequently within CLISTS and REXX EXECs, utilities are invoked
* that require input via control cards. While the control cards can be
* built within CLISTS and EXECs, it is not a user friendly process.
* For that reason, I came up with the CNTLCARD program, which makes
* it much easier to build the control card input required by such
* utilities.
*
* The CNTLCARD program assumes (and requires) that a pre-allocated
* virtual I/O (VIO) dataset be used to hold the control card that is
* to be generated. This was done as a security feature, since sensitive
* information could be put into the control card (like passwords) and
* only the job or user allocating the VIO dataset could access it.
*
* It is also the reason that only one control card can be written to
* the VIO dataset using the CNTLCARD program (you cannot specify a
* a disposition of MOD for a VIO dataset). If you do not require the
* security, or if you desire to be able to create multiple control
* cards, you can modify the source code at label DDOK to skip the VIO
* checking. If you do not perform this modification, CNTLCARD will
* set a return code of 4 if the SYSIN DD does not refer to a VIO
* dataset.
*
* CNTLCARD will return two other return codes. A code of 8 will be
* set if the SYSIN DD is not allocated, and a return code of 12 will be
* set if no parameter is passed to it. If you attempt to pass a control
* card whose length is greater than 80 bytes for CNTLCARD to write, it
* will truncate the length to 80 bytes without issuing any
* notification.
*
* In order to handle special character situations, such as leading
* blanks, the string of data to be used as a control card can be
* enclosed in single quotes, which will be stripped off if found by the
* CNTLCARD program. Otherwise the string can be coded without the
* quotes even if it has embedded blanks or special characters.
*
* As an example of its use, to generate a SORT control card, you would
* code:
*
*   SYSLOAD ' SORT FIELDS=(3,9,CH,A),FILSZ=E100'
*
* Below is an example CLIST which uses an unmodified CNTLCARD
* program to write a single control card to a VIO dataset:
*
*   FREE DD(SYSIN) DELETE
*   ALLOC DD(SYSIN) UNIT(VIO) SP(1) NEW REUSE +
*   RECFM(F) LRECL(80) BLKSIZE(80) DSORG(PS)
*   SYSLOAD ' SORT FIELDS=(3,9,CH,A),FILSZ=E100'
*
* Below is an example CLIST which to use a modified version of the
* CNTLCARD program (as described earlier) to write multiple control
* cards to a non-VIO dataset:
*
*   FREE DD(SYSIN) DELETE
*   ALLOC DD(SYSIN) UNIT(SYSDA) SP(1) MOD REUSE DA(TEMP) +
*   RECFM(F) LRECL(80) BLKSIZE(80) DSORG(PS)
*   SYSLOAD ' SORT FIELDS=(3,9,CH,A),FILSZ=E100'
*   SYSLOAD ' RECORD TYPE=F'
*
*
*                                                   c Xephon 1998
         EJECT
* From https://www.cbttape.org/xephon/XEPHON.intro
*
*********************************************************************   DOC FILE
*** --                                                           -- *   DOC FILE
*** --                     XEPHON Materials                      -- *   DOC FILE
*** --                                                           -- *   DOC FILE
*** --  Thomas Publications, which acquired all rights for the   -- *   DOC FILE
*** --  Xephon materials from www.xephon.com, has given support  -- *   DOC FILE
*** --  rights for all the materials, up thru the end of         -- *   DOC FILE
*** --  the year 2005, to www.cbttape.org.                       -- *   DOC FILE
*** --                                                           -- *   DOC FILE
*** --  Therefore, XEPHON materials republished here, certainly  -- *   DOC FILE
*** --  have a right to be here, so you don't have to worry      -- *   DOC FILE
*** --  about using them.                                        -- *   DOC FILE
*** --                                                           -- *   DOC FILE
*** --  The Xephon magazines have a rather scary copyright       -- *   DOC FILE
*** --  notice printed on each page.  This copyright notice no   -- *   DOC FILE
*** --  longer applies, since Xephon gave the rights to Thomas,  -- *   DOC FILE
*** --  and Xephon is defunct.  But I have found it impossible   -- *   DOC FILE
*** --  to edit these out of the magazines, so they simply       -- *   DOC FILE
*** --  need to be ignored.  Please use these materials as you   -- *   DOC FILE
*** --  see fit.  If you use any code, though, you should give   -- *   DOC FILE
*** --  attribution to the author.  It is only right to do that. -- *   DOC FILE
*** --                                                           -- *   DOC FILE
*** --  Sam Golob (12-2009, 07-2016)                             -- *   DOC FILE
*** --                                                           -- *   DOC FILE
*** --  This applies to all of the Xephon magazines except for   -- *   DOC FILE
*** --  VM.  The VM magazine rights were bought by SDS-USA, and  -- *   DOC FILE
*** --  I got permission from Jim Lampi of SDS to post them on   -- *   DOC FILE
*** --  our website, or put them on the CBT Tape, provided that  -- *   DOC FILE
*** --  attribution is given to SDS (Software Diversified        -- *   DOC FILE
*** --  Services).                                               -- *   DOC FILE
*** --                                                           -- *   DOC FILE
*** --  - - - - - - - - - - - - - - - - - - - - - - - - - - - -  -- *   DOC FILE
*** --                                                           -- *   DOC FILE
*** --       Also, go to www.cbttape.org/xephon                  -- *   DOC FILE
*** --  where we have magazines from 1998 thru 2005 in PDF       -- *   DOC FILE
*** --  format, with copyright notices changed, as far as        -- *   DOC FILE
*** --  possible, and protections removed where they previously  -- *   DOC FILE
*** --  existed.  Use of these materials is now according to     -- *   DOC FILE
*** --  the disclaimers on the CBT Tape.                         -- *   DOC FILE
*** --                                                           -- *   DOC FILE
*** --  File 830 also contains Xephon MVS materials from before  -- *   DOC FILE
*** --  1998, so it is worth while to search the contents of     -- *   DOC FILE
*** --  that file.  The "raw" version of File 830 is File 814.   -- *   DOC FILE
*** --  File 830 is a "fixed up" version of File 814, with much  -- *   DOC FILE
*** --  effort put in, to correct errors.  Not perfect yet, but  -- *   DOC FILE
*** --  much better.                                             -- *   DOC FILE
*** --                                                           -- *   DOC FILE
*********************************************************************   DOC FILE
         EJECT
         PRINT NOGEN
*
* This program was pubished in MVS Update Sept 1988, author UNKNOWN.
*
* Modified by Larry Belmontes
* Modified to COMMENT label DDOK and three following statements
* Modified DCB to use DDNAME=CARDS
* Load Module assigned PUTCARD, see assembler-link JCL
*
CNTLCARD CSECT
         YREGS
         STM   R14,R12,12(R13)
         LR    R12,R15
         USING CNTLCARD,R12
         LA    R2,SAVE
         ST    R2,8(,R13)
         ST    R13,4(,R2)
         LR    R13,R2
         LR    R2,R1               SAVE CPPL POINTER.
         RDJFCB SYSIN              LOAD JFCB.
         LTR   R15,R15             DDNAME EXIST?
         BZ    DDOK
         LA    R15,8               LOAD ERROR CODE OF 8.
         B     EXIT                EXIT.
DDOK     EQU   *
*DDOK    TM    JFCBAREA+JFCFLGS1-JFCB,JFCVRDS VIO DATASET?
*        BO    VIOOK
*        LA    R15,4               LOAD ERROR CODE OF 4.
*        B     EXIT                EXIT.
VIOOK    OPEN  (SYSIN,OUTPUT),TYPE=J    OPEN THE VIO DATASET.
         L     R3,0(,R2)           POINT TO PARM.
         LH    R5,0(,R3)           LOAD LENGTH.
         LH    R4,2(,R3)           LOAD OFFSET.
         LA    R4,4(,R4)           CREATE ...
         SR    R5,R4               ... LENGTH.
         LTR   R5,R5               ANYTHING THERE?
         BP    PARMOK              B IF YES.
         LA    R15,12              LOAD ERROR CODE.
         B     EXIT                EXIT.
PARMOK   CH    R5,=H'80'           GREATER THAN 80?
         BNH   PARMOK0             B IF NOT.
         LA    R5,80               TRUNCATE.
PARMOK0  LA    R3,0(R3,R4)         POINT TO PARM.
         CLI   0(R3),C''''         FIRST CHAR APOST?
         BNE   MPARM               B IF NOT.
         LA    R3,1(,R3)           BUMP POINTER
         SH    R5,=H'2'            ASSUME TRAILING APOST.
         BP    MPARM               B IF NOT MERELY ONE APOST.
         LA    R15,12              SET RC OF 12.
         B     EXIT                EXIT.
MPARM    BCTR  R5,*-*              GEN SS LEN.
         EX    R5,MOVEPARM         MOVE PARM.
         PUT   SYSIN,OUT           OUTPUT PARM.
         CLOSE (SYSIN)             CLOSE DATASET.
         SR    R15,R15             SET RC OF 0.
EXIT     L     R13,SAVE+4
         L     R14,12(,R13)
         LM    R0,R12,20(R13)
         BR    R14
MOVEPARM MVC   OUT(*-*),0(R3)      EXECUTED MOVE.
SAVE     DS    18F
EXLST    DC    X'87',AL3(JFCBAREA)
JFCBAREA DS    XL176
OUT      DC    CL80' '
         LTORG
         PRINT NOGEN
*SYSIN   DCB   DDNAME=SYSIN,DSORG=PS,LRECL=80,BLKSIZE=(80),
SYSIN    DCB   DDNAME=CARDS,DSORG=PS,LRECL=80,BLKSIZE=(80),            *
               RECFM=F,BUFNO=1,NCP=1,MACRF=(PM),EXLST=EXLST
JFCB     DSECT
         IEFJFCBN LIST=NO
         END
@@
//LKED.SYSLMOD DD  DISP=SHR,DSN=SYSGEN.ISPF.LLIB
//*
//********************************************************************/
//* 04/10/2020 1.0.20   Larry Belmontes Jr.                          */
//*                     - Added ALIAS LISTDSI to LKED step           */
//********************************************************************/
//LKED.SYSIN DD *
 ALIAS CNTLCARD
 NAME PUTCARD(R)
/*
//CLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.HELP,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=PUTCARD
)F FUNCTION -
  CONTROL CARD IMAGE QUICK BUILDER - 
  Frequently within CLISTS and REXX EXECs, utilities are invoked
  that require input via control cards. While the control cards can be
  built within CLISTS and EXECs, it is not a user friendly process.
  For that reason, I came up with the CNTLCARD program, which makes
  it much easier to build the control card input required by such
  utilities.
 
  The CNTLCARD program assumes (and requires) that a pre-allocated
  virtual I/O (VIO) dataset be used to hold the control card that is
  to be generated. This was done as a security feature, since sensitive
  information could be put into the control card (like passwords) and
  only the job or user allocating the VIO dataset could access it.
  
  It is also the reason that only one control card can be written to
  the VIO dataset using the CNTLCARD program (you cannot specify a
  a disposition of MOD for a VIO dataset). If you do not require the
  security, or if you desire to be able to create multiple control
  cards, you can modify the source code at label DDOK to skip the VIO
  checking. If you do not perform this modification, CNTLCARD will
  set a return code of 4 if the SYSIN DD does not refer to a VIO
  dataset.

  CNTLCARD will return two other return codes. A code of 8 will be
  set if the SYSIN DD is not allocated, and a return code of 12 will be
  set if no parameter is passed to it. If you attempt to pass a control
  card whose length is greater than 80 bytes for CNTLCARD to write, it
  will truncate the length to 80 bytes without issuing any
  notification.

  In order to handle special character situations, such as leading
  blanks, the string of data to be used as a control card can be
  enclosed in single quotes, which will be stripped off if found by the
  CNTLCARD program. Otherwise the string can be coded without the
  quotes even if it has embedded blanks or special characters.
  As an example of its use, to generate a SORT control card, you would
  code:

    SYSLOAD ' SORT FIELDS=(3,9,CH,A),FILSZ=E100'
    
  Below is an example CLIST which uses an unmodified CNTLCARD
  program to write a single control card to a VIO dataset:

    FREE DD(SYSIN) DELETE
    ALLOC DD(SYSIN) UNIT(VIO) SP(1) NEW REUSE +
    RECFM(F) LRECL(80) BLKSIZE(80) DSORG(PS)
    SYSLOAD ' SORT FIELDS=(3,9,CH,A),FILSZ=E100'

  Below is an example CLIST which to use a modified version of the
  CNTLCARD program (as described earlier) to write multiple control
  cards to a non-VIO dataset:

    FREE DD(SYSIN) DELETE
    ALLOC DD(SYSIN) UNIT(SYSDA) SP(1) MOD REUSE DA(TEMP) +
    RECFM(F) LRECL(80) BLKSIZE(80) DSORG(PS)
    SYSLOAD ' SORT FIELDS=(3,9,CH,A),FILSZ=E100'
    SYSLOAD ' RECORD TYPE=F'
                                                    c Xephon 1998
