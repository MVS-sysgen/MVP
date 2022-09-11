//ISPOPT5 JOB (JOB),
//             'INSTALL ISPOPT5',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.SAMPLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=HELLOFOR
C
C SIMPLE HELLO WORLD FORTRAN PROGRAM FOR VALIDATION
C OF ISPOPT5 WHICH PRINTS 'HELLO WORLD' ONTO PRINTER.
C
      WRITE(6,10)
   10 FORMAT(12H HELLO WORLD)
      STOP
      END
@@
//CMDPROC  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.CMDPROC,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=C$ISPOP5
PROC 0

/********************************************************************/
/*                                                                  */
/*    CLIST: C$ISPOP5                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* IVP CLIST to validate Background Selection Menu functions        */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY VERSION  NAME / DESCRIPTION                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/


/* ------------------------------------------------------ */
/* ISPEXEC CONTROL, use Diagnose Panel on ERROR           */
/* ------------------------------------------------------ */
ISPEXEC CONTROL ERRORS CANCEL

/* ------------------------------------------------------ */
/* Display File Menu                                      */
/* ------------------------------------------------------ */
ISPEXEC SELECT CMD(%CBGP5) NEWAPPL(CBGP)


END

./ ADD NAME=CLDSI
PROC 0 DSN()

/*********************************************************************/
/* CLIST:  CLDSI                                                     */
/*                                                                   */
/* Description                                                       */
/* -----------                                                       */
/* List dataset attributes using LISTDSI                             */
/*                                                                   */
/* More information at:                                              */
/*  https://ShareABitOfIT.net/LISTDSJ-for-mvs-3-8j/                  */
/*                                                                   */
/* Syntax                                                            */
/* -------                                                           */
/* CLDSI DSN('''xxxxxxxx.xxxxxxxx.xxxxxxxx''')                       */
/*    DSN - Dataset Name, fully qualified                            */
/*         o use three single quotes in DSN                          */
/*                                                                   */
/* CLDSI DSN(TEST.CNTL)                                              */
/*    DSN - Dataset Name, prepend prefix                             */
/*         o prefix preceeds DSN if no quotes used in DSN            */
/*                                                                   */
/* Output:                                                           */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*DSN:LARRY01.TEST.CNTL                                              */
/*MSG:                                                               */
/*RETURN CD: 0   SUCCESSFUL REQUEST                                  */
/*SYSREASON: 00   NORMAL COMPLETION                                  */
/*                                                                   */
/*DSORG RECFM LRECL BLKSZ KEYL  RKP   PASSWORD RACF CHGD             */
/*PO    FB    00080 19040 000   00000 NONE     NONE    Y             */
/*CREDT 2013-03-09  EXPDT 0000-00-00  REFDT 2019-01-30     CATL: Y   */
/*  13068 2013-068    00000 0000-000    19030 2019-030     CVOL: PUB000
/*PUB000                                                   VOLS: 001 */
/*ALLOCATION:     TYPE     PRI     USED     SEC        ALLOC         */
/*        PUB000  CYLINDER 00003   00002    0000001    00003         */
/*TRACKS: TOT     USED     UNUSED  EXTENTS                           */
/*        00090   00087    00003   003                               */
/*DEVICE: CYLS    TRKSCYL  TRKLEN  BLKSTRK  CAPACITY                 */
/*3350    00555   00030    19254   00001    000000320579100          */
/*PO DIR: BLKS    USED     UNUSED  MEMBERS  ALIAS                    */
/*        ?????   ?????    ?????   ?????    ?????                    */
/*-------------------------------------------------------------------*/
/*                                                                   */
/*                                                                   */
/*  Disclaimer: <DSCLAIMR>                                           */
/*  ================================================================ */
/*                                                                   */
/*  No guarantee; No warranty; Install / Use at your own risk.       */
/*                                                                   */
/*  This software is provided "AS IS" and without any expressed      */
/*  or implied warranties, including, without limitation, the        */
/*  implied warranties of merchantability and fitness for a          */
/*  particular purpose.                                              */
/*                                                                   */
/*                                                                   */
/*********************************************************************/

/*********************************************************************/
/* CLIST CONTROL statement                                           */
/*********************************************************************/
CONTROL MSG


/*********************************************************************/
/* EXECUTE LISDSI                                                    */
/*********************************************************************/
LISTDSI &DSN
SET RC = &LASTCC

/*********************************************************************/
/* Print results on terminal                                         */
/*********************************************************************/
SET WLINE = &STR(DSN: &SYSDSNAME)
WRITE &WLINE

SET WLINE = &STR(MSG: &SUBSTR(1:71,&SYSLISTDSJMSG)...)
WRITE &WLINE

SET WLINE = &STR(RETURN CD: &RC   &STR(&SYSMSGLVL1))
WRITE &WLINE

SET WLINE = &STR(SYSREASON: &SYSREASON   )
SET WLINE = &STR(&WLINE&STR(&SYSMSGLVL2))
WRITE &WLINE

WRITE

SET WLINE = &STR(DSORG RECFM LRECL BLKSZ KEYL  RKP)
SET WLINE = &STR(&WLINE   PASSWORD RACF CHGD)
WRITE &WLINE

SET WLINE = &STR(&SYSDSORG)
SET WLINE = &STR(&WLINE   &SYSRECFM)
SET WLINE = &STR(&WLINE &SYSLRECL)
SET WLINE = &STR(&WLINE &SYSBLKSIZE)
SET WLINE = &STR(&WLINE &SYSKEYLEN)
SET WLINE = &STR(&WLINE   &SYSKEYPOS)
SET WLINE = &STR(&WLINE &SYSPASSWORD)
SET WLINE = &STR(&WLINE    &SYSRACFA)
SET WLINE = &STR(&WLINE    &SYSUPDATED)
WRITE &WLINE

SET WLINE = &STR(CREDT &SYSCCREATE)
SET WLINE = &STR(&WLINE  EXPDT &SYSCEXDATE)
SET WLINE = &STR(&WLINE  REFDT &SYSCREFDATE)
SET WLINE = &STR(&WLINE     CATL: &SYSDSCAT)
WRITE &WLINE

SET WLINE = &STR(  &SYSJCREATE &SYSCREATE)
SET WLINE = &STR(&WLINE    &SYSJEXDATE &SYSEXDATE)
SET WLINE = &STR(&WLINE    &SYSJREFDATE &SYSREFDATE)
SET WLINE = &STR(&WLINE     CVOL: &SYSDSCATV)
WRITE &WLINE

SET WLINE = &STR(&SYSVOLUMES         )
SET WLINE = &STR(&WLINE             VOLS: &SYSNUMVOLS)
WRITE &WLINE

SET WLINE = &STR(ALLOCATION:     TYPE     PRI     USED)
SET WLINE = &STR(&WLINE     SEC        ALLOC)
WRITE &WLINE

SET WLINE = &STR(        &SYSVOLUME)
SET WLINE = &STR(&WLINE  &SYSUNITS)
SET WLINE = &STR(&WLINE &SYSPRIMARY)
SET WLINE = &STR(&WLINE   &SYSUSED)
SET WLINE = &STR(&WLINE    &SYSSECONDS)
SET WLINE = &STR(&WLINE    &SYSALLOC)
WRITE &WLINE

SET WLINE = &STR(TRACKS: TOT     USED     UNUSED  EXTENTS)
SET WLINE = &STR(&WLINE )
WRITE &WLINE

SET WLINE = &STR(        &SYSTRKSALLOC)
SET WLINE = &STR(&WLINE   &SYSTRKSUSED)
SET WLINE = &STR(&WLINE    &SYSTRKSUNUSED)
SET WLINE = &STR(&WLINE   &SYSEXTENTS)
WRITE &WLINE

SET WLINE = &STR(DEVICE: CYLS    TRKSCYL  TRKLEN  BLKSTRK)
SET WLINE = &STR(&WLINE  CAPACITY)
WRITE &WLINE

SET WLINE = &STR(&SYSUNIT)
SET WLINE = &STR(&WLINE &SYSCYLVOL)
SET WLINE = &STR(&WLINE   &SYSTRKSCYL)
SET WLINE = &STR(&WLINE    &SYSTRKLEN)
SET WLINE = &STR(&WLINE   &SYSBLKSTRK)
SET WLINE = &STR(&WLINE    &SYSUNITCAP)
WRITE &WLINE

IF &SYSADIRBLK NE &STR(?????) THEN -
  DO
    SET WLINE = &STR(PO DIR: BLKS    USED     UNUSED  MEMBERS)
    SET WLINE = &STR(&WLINE  ALIAS)
    WRITE &WLINE

    SET WLINE = &STR(        &SYSADIRBLK)
    SET WLINE = &STR(&WLINE   &SYSUDIRBLK)
    SET WLINE = &STR(&WLINE    &SYSNUDIRBLK)
    SET WLINE = &STR(&WLINE   &SYSMEMBERS)
    SET WLINE = &STR(&WLINE    &SYSMEMBERSALIAS)
    WRITE &WLINE
  END

/*********************************************************************/
/* Done!                                                             */
/*********************************************************************/
EXIT CODE(0)


@@
//PROCLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.PROCLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=GCCISP5

//********************************************************************/
//*                                                                  */
//*    PROC: GCCISP5                                                 */
//*                                                                  */
//* Author: Larry Belmontes Jr.                                      */
//*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
//*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
//*                                                                  */
//* Description:                                                     */
//* ---------------------------------------------------------------  */
//*                                                                  */
//* PROC to invoke GCC.  It only includes the EXEC card with         */
//* a cond code and PARM in lower-case.                              */
//*                                                                  */
//*                                                                  */
//*                                                                  */
//* Disclaimer:                                                      */
//* ================================================================ */
//*                                                                  */
//*    No guarantee; No warranty; Install / Use at your own risk.    */
//*                                                                  */
//*    This software is provided "AS IS" and without any expressed   */
//* or implied warranties, including, without limitation, the        */
//* implied warranties of merchantability and fitness for a          */
//* particular purpose.                                              */
//*                                                                  */
//*    The author requests keeping authors name intact in any        */
//* modified versions.                                               */
//*                                                                  */
//*    In addition, the author requests readers to submit any        */
//* code modifications / enhancements and associated comments        */
//* for consideration into a subsequent release (giving credit       */
//* to contributor(s)) thus, improving overall functionality         */
//* and further benefiting the MVS 3.8J hobbyist public domain       */
//* community.                                                       */
//*                                                                  */
//*                                                                  */
//*                                                                  */
//*                                                                  */
//* Change History:                                                  */
//* ================================================================ */
//* MM/DD/CCYY VERSION  NAME / DESCRIPTION                           */
//* ---------- -------  -------------------------------------------- */
//* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
//*                     - Initial version released to MVS 3.8J       */
//*                       hobbyist public domain                     */
//*                                                                  */
//********************************************************************/
//GCCISP5  PROC
//GCC      EXEC  PGM=GCC,
//  COND=(4,LT),
// PARM='-ansi -pedantic-errors -S -v -o dd:out -'
//* remaining GCC JCL follows...
@@
//CLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYSGEN.ISPF.CLIB
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CBGIJCL0
PROC 0 PANID(NOPANIDX) STEPL(Y) TERM(Y) UTX(4) SLIB(Y) SLIN(LIN) -
       SPRT(Y) SPUN(Y) SIN(Y) SGOSUF() SDMP(N)

/********************************************************************/
/*                                                                  */
/* CLIST: CBGIJCL0                                                  */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* This CLIST adds DD statements to compiler step for background    */
/* processing option 5.                                             */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGIJCL0 PANID(PANID) STEPL(Y) TERM(Y) UTX(4) SLIB(Y) SLIN(LIN)  */
/*          SPRT(Y) SPUN(Y) SIN(Y) SGOSUF()                         */
/*                                                                  */
/* Parameters:                                                      */
/* PANID    panel name for selected background process              */
/* STEPL    yes/no indicator to include STEPLIB DD                  */
/* TERM     yes/no indicator to include SYSTERM DD                  */
/* UTX      number of SYSUTx DDs to include (1-6)                   */
/* SLIB     yes/no indicator to include SYSLIB  DD                  */
/* SLIN     LIN/GO value used to include SYSLIN or SYSGO DD         */
/* SPRT     yes/no indicator to include SYSPRINT DD                 */
/* SPUN     yes/no indicator to include SYSPUNCH DD                 */
/* SIN      yes/no/instream indicator to include SYSIN DD           */
/* SGOSUF   DSN suffix for listing files                            */
/* SDMP     yes/no indicator to include SYSUDUMP DD                 */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET EXITCD = 0

  /****************************************************************
  /* STEPLIB
  /****************************************************************
  /* STEPLIB from panel
  /*
  /*  //STEPLIB  DD DISP=SHR,
  /*  //         DSN=steplib.dsn.from.panel
  /*
  /****************************************************************
  IF &STEPL = Y AND +
     &LENGTH(&LBXSTEP1) > 0 THEN -
    DO
      PUTCARD '//STEPLIB  DD DISP=SHR,'
      PUTCARD '//         DSN=&LBXSTEP1'
    END


  /****************************************************************
  /* SYSTERM
  /****************************************************************
  /* TERM   = IF PARTITIONED, DSN=PREFIX.MEMBER.TERM
  /*          IF SEQUENTIAL,  DSN=PREFIX.TEMPNAME.TERM
  /* NOTERM = NO TERM DD
  /*REQUIRED FIELD
  /*     DISP=MOD
  /*
  /*  //SYSTERM  DD DISP=(MOD,CATLG)
  /*  //         UNIT=SYSDA,
  /*  //         SPACE=(CYL,(2,1),RLSE),
  /*  //         DSN=PREFIX.xxxxxx.TERM
  /*
  /****************************************************************
  IF &TERM = Y AND +
     &LBXTST = TERM THEN +
    DO
      PUTCARD '//SYSTERM  DD DISP=(MOD,CATLG),'
      PUTCARD '//         UNIT=SYSDA,'
      PUTCARD '//         SPACE=(CYL,(2,1),RLSE),'
      IF &STR(&SYSPREF) > &STR( ) THEN +
        SET XX     = &STR(//         DSN=&SYSPREF)
      ELSE -
        SET XX     = &STR(//         DSN=&SYSUID)
      IF &ODSNMEM > &STR() THEN +
        SET XX     = &STR(&XX).&ODSNMEM
      ELSE +
        SET XX     = &STR(&XX).&LBXMBR
      PUTCARD '&XX..TERM'
    END
  ELSE +
    DO
    END


  /****************************************************************
  /* SYSPRINT
  /****************************************************************
  /* SYSPRINT can be a SEQ DSN or SYSOUT Class
  /*
  /*  //SYSPRINT DD DISP=(MOD,CATLG)
  /*  //         UNIT=SYSDA,
  /*  //         SPACE=(CYL,(2,1),RLSE),
  /*  //         DSN=PREFIX.LISTID.LIST .suffix
  /*
  /*  - or -
  /*
  /*  //SYSPRINT DD SYSOUT=(SYSOUT class)
  /*
  /****************************************************************
  IF &SPRT = Y THEN -
    DO
      IF &STR(&LBXLSTID) > &STR( ) THEN +
        DO
          PUTCARD '//SYSPRINT DD DISP=(MOD,CATLG),'
          PUTCARD '//         UNIT=SYSDA,'
          PUTCARD '//         SPACE=(CYL,(2,1),RLSE),'
          IF &STR(&SYSPREF) > &STR( ) THEN +
            SET XX = &STR(//         DSN=&SYSPREF)
          ELSE +
            SET XX = &STR(//         DSN=&SYSUID)
          SET XX     = &STR(&XX).&LBXLSTID..LIST
          IF &STR(&SGOSUF) > &STR( ) THEN -
            SET XX   = &STR(&XX).&LBXLSTID..LIST.&SGOSUF
          PUTCARD '&XX'
        END
      ELSE +
        DO
          PUTCARD '//SYSPRINT DD SYSOUT=(&LBXSOUT)'
        END
    END


  /****************************************************************
  /* SYSUDUMP
  /****************************************************************
  /* SYSUDUMP
  /*
  /*  //SYSUDUMP DD SYSOUT=*
  /*
  /****************************************************************
  IF &SDMP = Y THEN -
    DO
      PUTCARD '//SYSUDUMP DD SYSOUT=*'
    END


  /****************************************************************
  /* SYSPUNCH
  /****************************************************************
  /* SYSPUNCH can be a SYSOUT=B or SEQ DSN
  /*
  /*  //SYSPUNCH DD SYSOUT=B,
  /*  //         DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)'
  /*
  /*  - or -
  /*
  /* PUNCH  = IF PARTITIONED, DSN=PREFIX.MEMBER.PUNCH
  /*          IF SEQUENTIAL,  DSN=PREFIX.TEMPNAME.PUNCH
  /*
  /*  //SYSPUNCH DD DISP=(MOD,CATLG)
  /*  //         UNIT=SYSDA,
  /*  //         SPACE=(CYL,(2,1),RLSE),
  /*  //         DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120),
  /*  //         DSN=PREFIX.xxxxxx.PUNCH
  /*
  /****************************************************************
  IF &SPUN = F THEN -
    DO
      PUTCARD '//SYSPUNCH DD DISP=(MOD,CATLG),'
      PUTCARD '//         UNIT=SYSDA,'
      PUTCARD '//         SPACE=(CYL,(2,1),RLSE),'
      PUTCARD '//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120),'
      IF &STR(&SYSPREF) > &STR( ) THEN +
        SET XX     = &STR(//         DSN=&SYSPREF)
      ELSE -
        SET XX     = &STR(//         DSN=&SYSUID)
      IF &ODSNMEM > &STR() THEN +
        SET XX     = &STR(&XX).&ODSNMEM
      ELSE +
        SET XX     = &STR(&XX).&LBXMBR
      PUTCARD '&XX..PUNCH'
    END
  ELSE -
  IF &SPUN = Y THEN -
    DO
      PUTCARD '//SYSPUNCH DD SYSOUT=B,'
      PUTCARD '//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)'
    END


  /****************************************************************
  /* SYSIN
  /****************************************************************
  /* SYSIN Language processor source statements
  /*
  /*  //SYSIN    DD *
  /*
  /*  - or -
  /*
  /*  //SYSIN    DD DISP=SHR,
  /*  //         DSN=dataset name with member (if PDS)
  /*
  /****************************************************************
  IF &SIN = Y THEN -
    DO
      PUTCARD '//SYSIN    DD DISP=SHR,'
      PUTCARD '//         DSN=&LIBMEM'
    END
  ELSE -
  IF &SIN = I THEN -
    DO
      PUTCARD '//SYSIN    DD *'
    END


  /****************************************************************
  /* SYSUTx WORK FILES
  /****************************************************************
  /* SYSUTx work files 1-6
  /*
  /*  //SYSUTx   DD UNIT=SYSDA,SPACE(CYL,(2,2))
  /*
  /****************************************************************
  IF &DATATYPE(&UTX) = NUM THEN +
    IF &UTX GE 1  AND  &UTX LE 6 THEN -
      DO
        SET X = 1
        DO WHILE &X LE &UTX
          PUTCARD '//SYSUT&X   DD UNIT=SYSDA,SPACE=(CYL,(2,2))'
          SET X = &X + 1
        END
      END


  /****************************************************************
  /* SYSLIB Language processor libraries to resolve macros,
  /*        copybooks, etc.
  /****************************************************************
  /* //SYSLIB  CONCATENATE ISPF LIBS 1-4, IF NOT BLANK
  /*           CONCATENATE SYSLIBS   1-4, IF NOT BLANK
  /*
  /****************************************************************
  IF &SLIB = Y THEN +
    DO
      /*********************************
      /* SYSLIB  CONCATENATE ISPF LIBS
      /*********************************
      SET &DDNM = &STR(SYSLIB)
      IF &STR(&LBXPROJ) > &STR( ) THEN -
        DO
          SET I = 1
          SET &VARNAM = &STR(&&LBXLIB)
          DO WHILE &I <= 4
            SET DSNSL = &STR(&VARNAM&I)
            IF &STR(&DSNSL) > &STR( ) THEN +
              DO
        SET DSNSL = &STR(&LBXPROJ).&STR(&VARNAM&I).&STR(&LBXTYPE)
            PUTCARD '//&STR(&DDNM)   DD DISP=SHR,        <--ISPF&I'
                PUTCARD '//         DSN=&DSNSL'
                IF &DDNM > &STR( ) THEN SET &DDNM = &STR(      )
              END
            SET I = &I + 1
          END
        END
      /*********************************
      /* SYSLIB  Other Partitioned
      /*********************************
      /*********************************
      /* SYSLIB  CONCATENATE USER LIBS
      /*********************************
      SET I = 1
      SET &VARNAM = &STR(&&LBXSL)
   /* SET &DDNM   = &STR(SYSLIB)
      DO WHILE &I <= 4
        SET DSNSL  = &STR(&VARNAM&I)
        IF &STR(&DSNSL) > &STR( ) THEN +
          DO
            PUTCARD '//&STR(&DDNM)   DD DISP=SHR,        <--USER&I'
            PUTCARD '//         DSN=&DSNSL'
            IF &DDNM > &STR( ) THEN SET &DDNM = &STR(      )
          END
        SET I = &I + 1
      END
    END


  /****************************************************************
  /* SYSLIN or SYSGO
  /****************************************************************
  /*
  /*  //SYSLIN   DD DISP=SHR,               <-- for PDS object file
  /*  //         DSN=&LINMEM
  /*
  /*  - or -
  /*
  /*  //SYSLIN   DD DISP=(MOD,CATLG),       <-- for SEQ object file
  /*  //         UNIT=SYSDA,
  /*  //         SPACE=((CYL,2,1),RLSE),
  /*  //         DSN=&LINMEM
  /*
  /****************************************************************
  IF &SLIN = NO  THEN
  ELSE -
    DO
      IF &SLIN = LIN THEN +
        SET &DDNM   = &STR(SYSLIN)
      ELSE -
        SET &DDNM   = &STR(SYSGO )
      SET L = &LENGTH(&LINMEM)
      /************************************/
      /* LINMEM include member name?      */
      /************************************/
      IF &SUBSTR(&L:&L,&STR(&LINMEM)) = ) THEN -
        DO
          PUTCARD '//&DDNM   DD DISP=SHR,'
          PUTCARD '//         DSN=&LINMEM'
        END
      ELSE -
        DO
          PUTCARD '//&DDNM   DD DISP=(MOD,CATLG),'
          PUTCARD '//         UNIT=SYSDA,'
          PUTCARD '//         SPACE=(CYL,(2,1),RLSE),'
          PUTCARD '//         DSN=&LINMEM'
        END
    END


DONE: +
EXIT CODE(&EXITCD)
./ ADD NAME=CBGIQGO
PROC 0

/********************************************************************/
/*                                                                  */
/* CLIST: CBGIQGO                                                   */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* This CLIST adds a LOADER-GO step after a background compiler     */
/* step.  This is for quick testing purposes and limited in         */
/* functionality.  For example, no SYSIN is available!              */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGIQGO                                                          */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET EXITCD = 0

  /****************************************************************
  /* QGO STEP
  /****************************************************************
  IF &LBXGO = Y THEN
  ELSE -
    GOTO DONE

  /****************************************************************
  /* LOADER EXEC
  /****************************************************************
  PUTCARD '&CMNT Quick GO -----------------------------'
  PUTCARD '//QGO      EXEC PGM=LOADER,'

  /****************************************************************
  /* LOADER PARM
  /****************************************************************
  IF &P = B THEN -
    DO
      /* B=SIMULA                       */
PUTCARD '// PARM='EP=ZYQENT,MAP,PRINT/DUMP=5,SYMBDUMP=6,TERM,TRACE=1','
    END
  ELSE -
    DO
      /* Default PARM                   */
      PUTCARD '//   PARM='MAP,PRINT,LET','
    END

  /****************************************************************
  /* LOADER COND, SYSLIN, SYSLOUT, SYSPRINT, SYSUDUMP, SYSPUNCH
  /****************************************************************
  /*PUTCARD '//   COND=(8,LT,&LP)'
  PUTCARD '//   COND=(8,LT)'
  PUTCARD '//SYSLIN   DD  DISP=SHR,        <--OBJECT'
  PUTCARD '//             DSN=&LINMEM'
  PUTCARD '//SYSLOUT  DD  SYSOUT=*'
  PUTCARD '//SYSPRINT DD  SYSOUT=*'
  PUTCARD '//SYSUDUMP DD  SYSOUT=*'
  PUTCARD '//SYSPUNCH DD  SYSOUT=*         <--PUNCH'

  /****************************************************************
  /* LOADER Specific DDs for Language Processor and Process Type
  /*
  /* Note: For MVS 3.8J Compilers, SYSC SYSLIBs will be used
  /*       if exists, otherwise, SYS1 SYSLIB will be used.
  /*       (COBOL, FORTRAN, PL/1, ALGOL)
  /****************************************************************
  IF &P = 1 THEN
  ELSE -
  IF &P = 2 THEN -
    DO
      /* 2=COBOL MVT                    */
      CHKDSN 'SYSC.COBLIB' QUIET
      IF &LASTCC > 0 THEN -
        DO
          PUTCARD '//SYSLIB   DD  DISP=SHR,DSN=SYS1.COBLIB'
        END
      ELSE -
        DO
          PUTCARD '//SYSLIB   DD  DISP=SHR,DSN=SYSC.COBLIB'
        END
      PUTCARD '//SYSOUT   DD  SYSOUT=*'
    END
  ELSE -
  IF &P = 3 OR &P = 4 THEN -
    DO
      /* 3=FORTRAN                      */
      /* 4=MORTRAN                      */
      CHKDSN 'SYSC.FORTLIB' QUIET
      IF &LASTCC > 0 THEN -
        DO
          PUTCARD '//SYSLIB   DD  DISP=SHR,DSN=SYS1.FORTLIB'
        END
      ELSE -
        DO
          PUTCARD '//SYSLIB   DD  DISP=SHR,DSN=SYSC.FORTLIB'
        END
      PUTCARD '//FT05F001 DD  DDNAME=SYSIN'
      PUTCARD '//FT06F001 DD  SYSOUT=*'
      PUTCARD '//FT07F001 DD  SYSOUT=B'
      IF &P = 4 THEN -
        DO
          PUTCARD '//SYSTERM  DD  SYSOUT=*'
        END
    END
  ELSE -
  IF &P = 5 THEN -
    DO
      /* 5=PL/I                         */
      CHKDSN 'SYSC.LINKLIB' QUIET
      IF &LASTCC > 0 THEN -
        DO
          PUTCARD '//STEPLIB  DD  DISP=SHR,DSN=SYS1.LINKLIB'
          PUTCARD '//         DD  DISP=SHR,DSN=SYS1.PL1LIB'
          PUTCARD '//SYSLIB   DD  DISP=SHR,DSN=SYS1.PL1LIB'
        END
      ELSE -
        DO
          PUTCARD '//STEPLIB  DD  DISP=SHR,DSN=SYSC.LINKLIB'
          PUTCARD '//         DD  DISP=SHR,DSN=SYSC.PL1LIB'
          PUTCARD '//SYSLIB   DD  DISP=SHR,DSN=SYSC.PL1LIB'
        END
    END
  ELSE -
  IF &P = 6 THEN
  ELSE -
  IF &P = 7 THEN
  ELSE -
  IF &P = 8 THEN -
    DO
      /* 8=ALGOL                        */
      IF &LBXCMPL = 1 THEN -
        DO
          /* 1=ALGOL F                  */
          CHKDSN 'SYSC.ALGLIB' QUIET
          IF &LASTCC > 0 THEN -
            DO
              PUTCARD '//SYSLIB   DD  DISP=SHR,DSN=SYSC.ALGLIB'
            END
          ELSE -
            DO
              PUTCARD '//SYSLIB   DD  DISP=SHR,DSN=SYSC.ALGLIB'
            END
          PUTCARD '//ALGLDD01 DD  SYSOUT=*'
          PUTCARD '//SYSUT1   DD  UNIT=VIO,SPACE=(1024,(20,10))'
        END
      ELSE -
      IF &LBXCMPL = 2 THEN -
        DO
          /* 2=ALGOL 68C                */
          PUTCARD '//SYSLIB   DD  DISP=SHR,DSN=SYSC.A68CLIB'
        END
    END
  ELSE -
  IF &P = 9 THEN
  ELSE -
  IF &P = A THEN
  ELSE -
  IF &P = B THEN -
    DO
      /* B=Simula                       */
      PUTCARD '//SYSLIB   DD  DISP=SHR,DSN=SYSC.LINKLIB'
      PUTCARD '//SYSOUT   DD  SYSOUT=*'
      PUTCARD '//SYSIN    DD  DUMMY'
    END
  ELSE -
  IF &P = C THEN
  ELSE -
  IF &P = D THEN
  ELSE -
  IF &P = E THEN
  ELSE -
  IF &P = F THEN
  ELSE -
  IF &P = G THEN
  ELSE -
  IF &P = H THEN
  ELSE -
  IF &P = I THEN
  ELSE -
    DO
      /* Default SYSLIB                 */
      PUTCARD '//SYSLIB   DD  DISP=SHR,        <--Default SYSLIB'
      CHKDSN 'SYSC.LINKLIB' QUIET
      IF &LASTCC > 0 THEN -
        DO
          PUTCARD '//             DSN=SYS1.LINKLIB'
        END
      ELSE -
        DO
          PUTCARD '//             DSN=SYSC.LINKLIB'
        END
    END

  PUTCARD '&CMNT'




DONE: +
EXIT CODE(&EXITCD)
./ ADD NAME=CBGLLIBS
PROC 0 PANID(NOPANIDX)

/********************************************************************/
/*                                                                  */
/* CLIST: CBGLLIBS                                                  */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* This CLIST will list DSNs searched for SOURCE as JCL comment     */
/* statements for background processing option 5.                   */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGLLIBS PANID(PANID)                                            */
/*                                                                  */
/* Parameters:                                                      */
/* PANID    panel name for selected background process              */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET EXITCD = 0

  /****************************************************************
  /* COMMENT: LIST LIBRARIES ELIGIBLE FOR SOURCE CONTENT SEARCH
  /*          WHEN SOURCE ONLINE SET TO YES
  /****************************************************************
  IF &LBBGSO = Y THEN +
    DO
      PUTCARD '//* SOURCE LIBRARIES SEARCHED:-----------'
      IF &LBXODSN > &STR() THEN +
        DO
          /********************************************************
          /* LIST OTHER DSN
          /********************************************************
          PUTCARD '&CMNT   OTHER: &ODSNTMP'
          PUTCARD '//*   --- SEARCH STOPPED AT OTHER DSN ---'
        END
      ELSE +
        DO
          /********************************************************
          /* LIST LIBRARIES LBXLIB1-4 , IF NOT BLANK
          /********************************************************
          SET I = 1
          SET VARNAM = &STR(&&LBXLIB)
          DO WHILE &I <= 4
            SET VARVAL = &STR(&VARNAM&I)
            IF &LENGTH(&VARVAL) > 0 THEN +
              DO
                SET XX = &STR(//*      )&I:  &LBXPROJ
                SET XX = &STR(&XX).&VARVAL
                SET XX = &STR(&XX).&LBXTYPE
                SET XX = &STR(&XX)(&LBXMBR)
                PUTCARD '&XX.'
              END
            SET I = &I + 1
          END
        END
    END


  /****************************************************************
  /* COMMENT: LIST SOURCE FOUND LIBRARY TEXT, IF PRESENT
  /****************************************************************
  IF &STR(&SRCFND) > &STR() THEN +
    DO
      PUTCARD '&SRCFND'
    END


  /****************************************************************
  /* COMMENT: LIST SOURCE FILE AND OBJ FILE, UNCONDITIONALLY
  /****************************************************************
  PUTCARD '&CMNT'
  PUTCARD '&CMNT   FROM --- &LIBMEM'
  IF &P = 6 THEN -
    PUTCARD '&CMNT     TO --- &LMDMEM'
  ELSE -
    PUTCARD '&CMNT     TO --- &LINMEM'
  PUTCARD '//*---------------------------------------'




DONE: +
EXIT CODE(&EXITCD)
./ ADD NAME=CBGP51
PROC 0                                /* BG ASSEMBLER               */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP51                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG Assembler JCL.                             */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP51                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT51)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LB1CMPL = 1 THEN -
    PUTCARD '//* A S S E M B L E R   -  XF'
  ELSE -
  IF &LB1CMPL = 2 THEN -
    PUTCARD '//* A S S E M B L E R   -  XF MACRO XREF'
  ELSE -
  IF &LB1CMPL = 3 THEN -
    PUTCARD '//* A S S E M B L E R   -  G  WATERLOO'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LB1CMPL = 1 THEN -
    PUTCARD '//ASM      EXEC PGM=IFOX00,REGION=128K,'
  ELSE -
  IF &LB1CMPL = 2 THEN -
    PUTCARD '//ASM      EXEC PGM=AFOX00,REGION=512K,'
  ELSE -
  IF &LB1CMPL = 3 THEN -
    PUTCARD '//ASM      EXEC PGM=ASMGASM,REGION=1024K,'
  PUTCARD '//         COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/


  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  IF &LB1CMPL = 3 THEN +
    SET LDD = LIN
  ELSE +
    SET LDD = GO

  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(Y) UTX(4) SLIB(Y) SLIN(&LDD) -
      SPRT(Y) SPUN(Y) SIN(Y)
  SET RC=&LASTCC

  /****************************************************************
  /* MACREF FOR IFOX00 MACRO XREF
  /****************************************************************
  IF &LB1CMPL = 2 THEN -
    DO
      PUTCARD '//MACREF   DD DUMMY'
    END

  /****************************************************************
  /* Quick GO Step, if selected
  /****************************************************************
  IF &LB1GO = Y THEN +
    DO
      %CBGIQGO
    END

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP52
PROC 0                                /* BG OS/VS COBOL             */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP52                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG COBOL JCL.                                 */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP52                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT52)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LB2CMPL = 1 THEN +
    PUTCARD '//* O S / V S   C O B O L  C O M P I L E'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LB2CMPL = 1 THEN +
    PUTCARD '//COBOL    EXEC PGM=IKFCBL00,REGION=192K,'
  PUTCARD '//         COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(Y) UTX(6) SLIB(Y) SLIN(LIN)  -
      SPRT(Y) SPUN(Y) SIN(Y)
  SET RC=&LASTCC

  /****************************************************************
  /* Quick GO Step, if selected
  /****************************************************************
  IF &LB2GO = Y THEN +
    DO
      %CBGIQGO
    END

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP53
PROC 0                                /* BG FORTRAN IV              */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP53                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG FORTRAN JCL.                               */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP53                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT53)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LB3CMPL = 1 THEN +
    PUTCARD '//* F O R T R A N  IV  G  C O M P I L E'
  ELSE +
    PUTCARD '//* F O R T R A N  IV  H  C O M P I L E'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LB3CMPL = 1 THEN +
    PUTCARD '//FORTG   EXEC PGM=IEYFORT,REGION=100K,'
  ELSE +
    PUTCARD '//FORTH   EXEC PGM=IEKAA00,REGION=228K,'
  PUTCARD '//         COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(4) SLIB(N) SLIN(LIN)  -
      SPRT(Y) SPUN(Y) SIN(Y)
  SET RC=&LASTCC

  /****************************************************************
  /* Quick GO Step, if selected
  /****************************************************************
  IF &LB3GO = Y THEN +
    DO
      %CBGIQGO
    END

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP54
PROC 0                                /* BG MORTRAN W FORTRAN       */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP54                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG MORTRAN JCL.                               */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP54                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT54)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  PUTCARD '//* M O R T R A N   PRE-PROCESSOR'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  PUTCARD '//MORT     EXEC  PGM=MORTRAN,REGION=192K,'
  PUTCARD '//         COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(0) SLIB(N) SLIN(NO)   -
      SPRT(N) SPUN(N) SIN(N)
  SET RC=&LASTCC

  /****************************************************************
  /* MORTRAN DD STATEMENTS
  /****************************************************************
  PUTCARD '//FT01F001 DD DISP=SHR,'
  PUTCARD '//         DSN=SYSC.MACLIB(MORTMAC1)'
  PUTCARD '//FT05F001 DD DISP=SHR,'
  PUTCARD '//         DSN=&LIBMEM'
  IF &STR(&LB4SOUT) > &STR( ) THEN -
    PUTCARD '//FT06F001 DD SYSOUT=(&LB4SOUT),'
  ELSE -
    PUTCARD '//FT06F001 DD SYSOUT=*,'
  PUTCARD '// DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1596,BUFNO=2)'
  PUTCARD '//FT07F001 DD DSN=&&FORTIN,'
  PUTCARD '// DISP=(,PASS),UNIT=SYSDA,SPACE=(CYL,(2,1)),'
  PUTCARD '// DCB=(RECFM=FB,LRECL=080,BLKSIZE=1680)'

  /****************************************************************
  /* FORTRAN STEP
  /****************************************************************
  PUTCARD '//*----------------------------------------'
  IF &LB4CMPL = 1 THEN +
    PUTCARD '//* USING  F O R T R A N  IV  G'
  ELSE +
    PUTCARD '//* USING  F O R T R A N  IV  H'
  PUTCARD '//*---------------------------------------'
  IF &LB4CMPL = 1 THEN +
    PUTCARD '//FORTG    EXEC PGM=IEYFORT,REGION=100K,'
  ELSE +
    PUTCARD '//FORTH    EXEC PGM=IEKAA00,REGION=228K,'
  PUTCARD '// COND=(05,LE),'
/*---START------------------- $$cbg-compileparm2 --------------------*/
  /****************************************************************
  /* Construct PARM2 string for Compiler/Assembler Options
  /****************************************************************
  CUTIL00 TRIM LB&P.OPT2
  SET VAROPT = &STR(&&LB&P.OPT2)
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&VAROPT) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&VAROPT''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&VAROPT) > &STR( ) THEN -
      PUTCARD '//   PARM='&VAROPT''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm2 --------------------*/

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(4) SLIB(N) SLIN(LIN)  -
      SPRT(Y) SPUN(Y) SIN(N)
  SET RC=&LASTCC
  PUTCARD '//SYSIN     DD DSN=&&FORTIN,DISP=(OLD,DELETE,DELETE)'

  /****************************************************************
  /* Quick GO Step, if selected
  /****************************************************************
  IF &LB4GO = Y THEN +
    DO
      %CBGIQGO
    END

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP55
PROC 0                                /* BG PL/I                    */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP55                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG PL/I F JCL.                                */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP55                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT55)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LB5CMPL = 1 THEN +
    PUTCARD '//* P L / I   F   M V T   C O M P I L E'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LB5CMPL = 1 THEN +
    PUTCARD '//PLIF     EXEC PGM=IEMAA,REGION=52K,'
  PUTCARD '//         COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(3) SLIB(Y) SLIN(LIN)  -
      SPRT(Y) SPUN(Y) SIN(Y)
  SET RC=&LASTCC

  /****************************************************************
  /* Quick GO Step, if selected
  /****************************************************************
  IF &LB5GO = Y THEN +
    DO
      %CBGIQGO
    END

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP56
PROC 0                                /* BG LINK-EDIT               */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP56                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 6                                  '*/
/*                                                                  */
/* This CLIST creates BG Linkage Edit  JCL.                         */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP56                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT56)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LB6CMPL = 1 THEN -
    PUTCARD '//* L I N K A G E   E D I T O R'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  PUTCARD '//LINK   EXEC  PGM=IEWL,REGION=192K,'
  PUTCARD '// COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(Y) UTX(1) SLIB(N) SLIN(LIN)  -
      SPRT(Y) SPUN(N) SIN(N)
  SET RC=&LASTCC

  /*********************************
  /* SYSLMOD
  /*********************************
  PUTCARD '//SYSLMOD  DD DISP=SHR,'
  PUTCARD '//         DSN=&LMDMEM'

  /*********************************
  /* SYSLIB  CONCATENATE USER LIBS
  /*********************************
  SET I = 1
  SET &VARNAM = &STR(&&LB6SL)
  SET &DDNM   = &STR(SYSLIB)
  DO WHILE &I <= 4
    SET DSNSL  = &STR(&VARNAM&I)
    IF &STR(&DSNSL) > &STR( ) THEN +
      DO
        PUTCARD '//&STR(&DDNM)   DD DISP=SHR,        <--LKED&I'
        PUTCARD '//         DSN=&DSNSL'
        IF &DDNM > &STR( ) THEN SET &DDNM = &STR(      )
      END
    SET I = &I + 1
  END

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
END
./ ADD NAME=CBGP57
PROC 0                                /* BG ASSIST Interpreter      */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP57                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG ASSIST JCL.                                */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP57                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT57)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LB7CMPL = 1 THEN +
    PUTCARD '//* A S S I S T   Interpreter'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LB7CMPL = 1 THEN +
    PUTCARD '//DATA     EXEC PGM=ASSIST,REGION=4096K,'
  PUTCARD '//         COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(0) SLIB(Y) SLIN(NO)   -
      SPRT(Y) SPUN(Y) SIN(Y)
  SET RC=&LASTCC

  /****************************************************************
  /* Processor Specific JCL
  /****************************************************************
  PUTCARD '//SYSIN2   DD  DDNAME=SYSIN2'
  PUTCARD '//SYSUT1   DD  UNIT=SYSDA,DISP=(,DELETE),'
  PUTCARD '//         SPACE=(3520,(100,10)),'
  PUTCARD '//         DCB=(RECFM=F,BLKSIZE=3520)'

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP58
PROC 0                                /* BG ALGOL                   */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP58                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG ALGOL JCL.                                 */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP58                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT58)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LB8CMPL = 1 THEN +
    PUTCARD '//* A L G O L   F   M V T   C O M P I L E'
  ELSE +
    PUTCARD '//* A L G O L   6 8 C   C O M P I L E'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LB8CMPL = 1 THEN +
    PUTCARD '//ALGOL    EXEC PGM=ALGOL,REGION=1024K,'
  ELSE +
    PUTCARD '//A68      EXEC PGM=A68C,REGION=1024K,'
  PUTCARD '// COND=(12,LE),'


  IF &LB8CMPL = 1 THEN +
    DO
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/
    END
  ELSE +
    DO
      SET XX     = &STR(// PARM='F40K/
      SET L = &LENGTH(&LB8OPT1)
      IF &L > 0 THEN -
        PUTCARD 'STR(&XX)&SUBSTR(1:&L,&LB8OPT1)''
      ELSE -
        PUTCARD '&STR(&XX)''
    END

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  IF &LB8CMPL = 1 THEN +
    DO
      %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(3) SLIB(N) -
          SLIN(LIN) SPRT(Y) SPUN(Y) SIN(Y)
      SET RC=&LASTCC
    END
  ELSE -
    DO
      SET A68CLIB = &STR(SYSC.A68CLIB)
      PUTCARD '//STEPLIB  DD DISP=SHR,'
      PUTCARD '//         DSN=&A68CLIB..MOD'
      PUTCARD '//INIT     DD DISP=SHR,DSN=&A68CLIB..SYS(INIT)'
      PUTCARD '//SYSENV   DD DISP=SHR,DSN=&A68CLIB..SYS'
      PUTCARD '//CODE     DD DISP=(NEW,PASS,DELETE),'
      PUTCARD '//         UNIT=SYSDA,'
      PUTCARD '//         SPACE=(CYL,(3,1),RLSE)'
      PUTCARD '//PROGRAM  DD DISP=SHR,'
      PUTCARD '//         DSN=&LIBMEM'
      %CBGIJCL0 PANID(&PANL0) STEPL(N) TERM(N) UTX(0) SLIB(N) -
          SLIN(NO)  SPRT(Y) SPUN(N) SIN(N)
      SET RC=&LASTCC
    END

  /****************************************************************
  /* Quick GO Step, if selected
  /****************************************************************
  IF &LB8GO = Y THEN +
    IF &LB8CMPL = 1 THEN +
      DO
        %CBGIQGO PROCS(&LB8CMPL)
      END
    ELSE -
      DO
        PUTCARD '&CMNT'
        PUTCARD '//Z370     EXEC PGM=Z370,REGION=0200K,'
        PUTCARD '//         COND=(12,LE),'
        PUTCARD '//    PARM='F40K/''
        PUTCARD '//STEPLIB  DD DISP=SHR,'
        PUTCARD '//         DSN=&A68CLIB..MOD'
        PUTCARD '//ZCODE    DD DISP=(OLD,DELETE,DELETE),'
        PUTCARD '//         DSN=*.A68.CODE'
        PUTCARD '//SYSPRINT DD SYSOUT=(&LB8SOUT)'
        PUTCARD '//SYSGO    DD DISP=(NEW,PASS,DELETE),'
        PUTCARD '//    SPACE=(CYL,(3,1),RLSE),UNIT=SYSDA,'
        PUTCARD '//    DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)'
        PUTCARD '&CMNT'
        PUTCARD '//GO       EXEC PGM=LOADER,'
        PUTCARD '//         COND=(12,LE),'
        PUTCARD '//    PARM='MAP,PRINT,LET''
        PUTCARD '//STEPLIB  DD DISP=SHR,'
        PUTCARD '//         DSN=&A68CLIB..MOD'
        PUTCARD '//SYSLIB   DD DISP=SHR,'
        PUTCARD '//         DSN=&A68CLIB..MOD'
        PUTCARD '//SYSLIN   DD DISP=(OLD,DELETE,DELETE),'
        PUTCARD '//            DSN=*.Z370.SYSGO'
        PUTCARD '//SYSLOUT  DD SYSOUT=(&LB8SOUT)'
        PUTCARD '//SYSPRINT DD SYSOUT=(&LB8SOUT)'
      END

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP59
PROC 0                                /* BG PASCAL                  */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP59                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG PASCAL JCL.                                */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP59                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT59)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LB9CMPL = 1 THEN +
    PUTCARD '//* P A S C A L   8 0 0 0   COMPILE'
  IF &LB9CMPL = 2 THEN +
    PUTCARD '//* P A S C A L   STONYBROOK COMPILE'
  IF &LB9CMPL = 3 THEN +
    PUTCARD '//* P A S C A L   STANFORD COMPILE'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LB9CMPL = 1 THEN +
    DO
      PUTCARD '//PASC     EXEC PGM=PASCALC,REGION=1024K,'
    END
  ELSE +
  IF &LB9CMPL = 2 THEN +
    DO
      PUTCARD '//SBPASC   EXEC PGM=SBPASCAL,REGION=1024K,'
    END
  ELSE +
  IF &LB9CMPL = 3 THEN +
    DO
      PUTCARD '//COMPILE  EXEC PGM=PASCAL,'
    END

  IF &LB9CMPL = 3 THEN +
    PUTCARD '// COND=(12,LE)'
  ELSE -
    PUTCARD '// COND=(12,LE),'

  IF &LB9CMPL = 1 THEN +
    DO
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/
    END
  ELSE +
  IF &LB9CMPL = 2 THEN +
    DO
      SET XX    = &STR(// PARM='TIME=10,LINES=1000,DEBUG=2)
      SET L = &LENGTH(&LB9OPT1)
      IF &L > 0 THEN +
        SET XX    = &STR(&XX)&SUBSTR(1:&L,&LB9OPT1)'
      ELSE +
        SET XX    = &STR(&XX)&STR(')
      PUTCARD '&XX'
    END

  /****************************************************************
  /* STEPLIB
  /****************************************************************
  IF &STR(&SUBSTR(1:1,&LB9STEP1)) > &STR( ) THEN -
    DO
      IF &LB9CMPL = 1 THEN +
        DO
          PUTCARD '//STEPLIB  DD DISP=SHR,'
          PUTCARD '//   DSN=SYSC.PASCAL.PASLIB'
        END
      ELSE +
        DO
          PUTCARD '//STEPLIB  DD DISP=SHR,'
          PUTCARD '//   DSN=SYSC.LINKLIB'
        END
    END

  /****************************************************************
  /* Processor Specific JCL
  /****************************************************************
  IF &LB9CMPL = 1 THEN +
    DO
      PUTCARD '//$PASMSGS DD DISP=SHR,'
      PUTCARD '//         DSN=SYSC.PASCAL.PASMSGS'
    END
  IF &LB9CMPL = 2 THEN +
    DO
      PUTCARD '//PASS1    DD DISP=SHR,'
      PUTCARD '//   DSN=SYSC.STONYBRK.PASCAL.OBJECT(PASS1)'
      PUTCARD '//PASS2    DD DISP=SHR,'
      PUTCARD '//   DSN=SYSC.STONYBRK.PASCAL.OBJECT(PASS2)'
      PUTCARD '//PASS3    DD DISP=SHR,'
      PUTCARD '//   DSN=SYSC.STONYBRK.PASCAL.OBJECT(PASS3)'
      PUTCARD '//PMD      DD DISP=SHR,'
      PUTCARD '//   DSN=SYSC.STONYBRK.PASCAL.OBJECT(PMD)'
      PUTCARD '//FILE3    DD UNIT=SYSDA,SPACE=(CYL,3)'
    END
  IF &LB9CMPL = 3 THEN +
    DO
      PUTCARD '//OUTPUT   DD SYSOUT=(&LB9SOUT)'
      PUTCARD '//PRD      DD DISP=SHR,'
      PUTCARD '//   DSN=SYSC.STANFORD.PASCAL.LIB(PASMSG)'
      PUTCARD '//PRR      DD DISP=(,PASS),'
      PUTCARD '//   DSN=&NRSTR(&&PCODE),UNIT=SYSDA,'
      PUTCARD '//   DCB=RECFM=VB,SPACE=(CYL,(3,1),RLSE)'
      PUTCARD '//QRR      DD DISP=(,PASS),'
      PUTCARD '//   DSN=&NRSTR(&&TABLES),UNIT=SYSDA,'
      PUTCARD '//   DCB=RECFM=VB,SPACE=(CYL,(3,1),RLSE)'
    END

  /****************************************************************
  /* SYSPRINT, SYSPUNCH, SYSIN
  /****************************************************************
  IF &LB9CMPL = 3 THEN +
    DO
      PUTCARD '//INPUT    DD DISP=SHR,'
      PUTCARD '//         DSN=&LIBMEM'
    END
  ELSE +
    DO
      PUTCARD '//SYSPRINT DD SYSOUT=(&LB9SOUT)'
      PUTCARD '//SYSIN    DD DISP=SHR,'
      PUTCARD '//         DSN=&LIBMEM'
    END

  /****************************************************************
  /* SYSUT  WORK FILES
  /****************************************************************
  IF &LB9CMPL = 2 THEN +
    DO
      PUTCARD '//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(2,2))'
      PUTCARD '//SYSUT2   DD UNIT=SYSDA,SPACE=(CYL,(2,2))'
      PUTCARD '//SYSUT3   DD UNIT=SYSDA,SPACE=(CYL,(2,2))'
      PUTCARD '//SYSUT4   DD UNIT=SYSDA,SPACE=(CYL,(2,2))'
      PUTCARD '//SYSUT5   DD UNIT=SYSDA,SPACE=(CYL,(2,2))'
    END

  /****************************************************************
  /* GO JCL
  /****************************************************************
  IF &LB9CMPL = 1 THEN +
    DO
      PUTCARD '//SYSGO    DD DISP=OLD,DSN=&LINMEM'
      PUTCARD '//*  GO ***'
      PUTCARD '//LKED   EXEC  PGM=IEWL,PARM='LIST,MAP','
      PUTCARD '//             COND=(0,NE,PASC)'
      PUTCARD '//SYSLIB   DD  DISP=SHR,DSN=SYSC.PASCAL.PASLIB'
      PUTCARD '//         DD  DISP=SHR,DSN=SYSC.FORTLIB'
      PUTCARD '//SYSLMOD  DD  DISP=(,PASS),DSN=&&LOAD(USERSPGM),'
      PUTCARD '//             SPACE=(TRK,(2,10,2)),UNIT=SYSDA'
      PUTCARD '//SYSLIN   DD  DISP=SHR,DSN=SYSC.PROCLIB(P8PASINC),'
      PUTCARD '//             DCB=BLKSIZE=400'
      PUTCARD '//         DD  DISP=SHR,DSN=*.PASC.SYSGO'
      PUTCARD '//SYSPRINT DD  SYSOUT=(&LB9SOUT)'
      PUTCARD '//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(5,5))'
      PUTCARD '//GO     EXEC  PGM=*.LKED.SYSLMOD,COND=(0,NE,LKED)'
      PUTCARD '//$LOCAL   DD  UNIT=SYSDA,SPACE=(TRK,0)'
      PUTCARD '//LOCAL    DD  VOL=REF=*.$LOCAL,DISP=SHR'
      PUTCARD '//SYSPRINT DD  SYSOUT=(&LB9SOUT)'
      PUTCARD '//*        FORTRAN ROUTINES WITH'
      PUTCARD '//*        STANDARD OUTPUT ARE CALLED'
      PUTCARD '//FT03F001 DD  SYSOUT=(&LB9SOUT)'
    END
  ELSE +
  IF &LB9CMPL = 3 THEN +
    DO
      PUTCARD '&CMNT'
      PUTCARD '//POSTPROC EXEC PGM=ASMPCODE,COND=(0,LT)'
      PUTCARD '//STEPLIB  DD DSN=SYSC.LINKLIB,DISP=SHR'
      PUTCARD '//INPUT    DD DSN=*.COMPILE.PRR,DISP=(OLD,DELETE)'
      PUTCARD '//PRD      DD DSN=*.COMPILE.QRR,DISP=(OLD,PASS)'
      PUTCARD '//OUTPUT   DD SYSOUT=(&LB9SOUT)'
      PUTCARD '//PRR      DD DSN=&NRSTR(&&OBJECT),'
      PUTCARD '//            UNIT=SYSDA,DCB=RECFM=FB,'
      PUTCARD '//            SPACE=(TRK,(10,5),RLSE),DISP=(,PASS)'
      PUTCARD '&CMNT'
      PUTCARD '//GO       EXEC PGM=LOADER,COND=(0,LT),'
      PUTCARD '//            PARM='//TIME=10''
      PUTCARD '//SYSLIN   DD DSN=*.POSTPROC.PRR,DISP=(OLD,DELETE)'
      PUTCARD '//SYSLOUT  DD SYSOUT=(&LB9SOUT)'
      PUTCARD '//SYSLIB   DD DSN=SYSC.FORTLIB,DISP=SHR'
      PUTCARD '//         DD DSN=SYSC.LINKLIB,DISP=SHR'
      PUTCARD '//PRD      DD DUMMY'
      PUTCARD '//QRD      DD DSN=*.COMPILE.QRR,DISP=(OLD,DELETE)'
      PUTCARD '//QRR      DD UNIT=SYSDA,SPACE=(TRK,(2,2))'
      PUTCARD '//FT06F001 DD SYSOUT=(&LB9SOUT)'
      PUTCARD '//OUTPUT   DD SYSOUT=(&LB9SOUT)'
    END

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP5A
PROC 0                                /* BG RPG MVT                 */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP5A                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG RPG MVT JCL.                               */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP5A                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT5A)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LBACMPL = 1 THEN +
    PUTCARD '//* R P G   E   M V T   C O M P I L E'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LBACMPL = 1 THEN +
    PUTCARD '//RPG      EXEC PGM=IESRPG,REGION=052K,'
  PUTCARD '// COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(3) SLIB(N) SLIN(GO)     -
      SPRT(Y) SPUN(Y) SIN(Y)
  SET RC=&LASTCC

  /****************************************************************
  /* Quick GO Step, if selected
  /****************************************************************
  IF &LBAGO = Y THEN +
    DO
      %CBGIQGO
    END

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP5B
PROC 0                                /* BG SIMULA                  */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP5B                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG SIMULA JCL.                                */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP5B                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT5B)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LBBCMPL = 1 THEN +
    PUTCARD '//* S I M U L A   C O M P I L E'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LBBCMPL = 1 THEN +
    PUTCARD '//SIM      EXEC PGM=SIMULA,REGION=1024K,'
  PUTCARD '// COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(4) SLIB(N) SLIN(GO)   -
      SPRT(Y) SPUN(N) SIN(Y)
  SET RC=&LASTCC

  /****************************************************************
  /* Quick GO Step, if selected
  /****************************************************************
  IF &LBBGO = Y THEN +
    DO
      %CBGIQGO
    END

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP5C
PROC 0                                /* BG SNOBOL                  */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP5C                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG SNOBOL JCL.                                */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP5C                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT5C)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LBCCMPL = 1 THEN +
    PUTCARD '//* S N O B O L   Interpreter'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LBCCMPL = 1 THEN +
    PUTCARD '//SNOBOL4  EXEC PGM=SNOBOL4,'
  PUTCARD '// COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(0) SLIB(N) SLIN(NO)     -
      SPRT(N) SPUN(N) SIN(N)
  SET RC=&LASTCC

  /****************************************************************
  /* Processor Specific JCL
  /****************************************************************
  PUTCARD '//FT06F001 DD SYSOUT=(&LBCSOUT)'
  PUTCARD '//FT07F001 DD SYSOUT=(&LBCSOUT)'
  PUTCARD '//FT05F001 DD DISP=SHR,'
  PUTCARD '//            DSN=&LIBMEM'

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP5D
PROC 0                                /* BG WATFIV                  */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP5D                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG WATFIV JCL.                                */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP5D                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT5D)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LBDCMPL = 1 THEN +
    PUTCARD '//* W A T F I V   Interpreter'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LBDCMPL = 1 THEN +
    PUTCARD '//WATFIVE EXEC PGM=WATFIV,REGION=512K,'
  PUTCARD '// COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(0) SLIB(N) SLIN(NO)     -
      SPRT(N) SPUN(N) SIN(N)
  SET RC=&LASTCC

  /****************************************************************
  /* Processor Specific JCL
  /****************************************************************
  PUTCARD '//WATLIB   DD DISP=SHR,DSN=SYSC.WATFIV.FUNLIB'
  PUTCARD '//         DD DISP=SHR,DSN=SYSC.WATFIV.WATLIB'
  PUTCARD '//FT01F001 DD SPACE=(CYL,(2,1)),UNIT=SYSDA,'
  PUTCARD '//         DCB=(RECFM=VS,BLKSIZE=256)'
  PUTCARD '//FT02F001 DD SPACE=(CYL,(2,1)),UNIT=SYSDA,'
  PUTCARD '//         DCB=(RECFM=VS,BLKSIZE=256)'
  PUTCARD '//FT03F001 DD SPACE=(CYL,(2,1)),UNIT=SYSDA,'
  PUTCARD '//         DCB=(RECFM=VS,BLKSIZE=256)'
  PUTCARD '//FT04F001 DD SPACE=(CYL,(2,1)),UNIT=SYSDA,'
  PUTCARD '//         DCB=(RECFM=VS,BLKSIZE=256)'
  PUTCARD '//FT05F001 DD DISP=SHR,'
  PUTCARD '//         DSN=&LIBMEM'
  PUTCARD '//FT06F001 DD SYSOUT=(&LBDSOUT)'
  PUTCARD '//FT07F001 DD SYSOUT=(&LBDSOUT)'
  PUTCARD '//FT08F001 DD SYSOUT=(&LBDSOUT)'

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP5E
PROC 0                                /* BG XPL                     */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP5E                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG XPL JCL.                                   */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP5E                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT5E)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LBECMPL = 1 THEN +
    PUTCARD '//* X P L   A n a l y z e r'
  IF &LBECMPL = 2 THEN +
    PUTCARD '//* X P L   a n d   G O'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LBECMPL = 1 THEN +
    PUTCARD '//XPLA    EXEC  PGM=XPLSM,'
  ELSE -
    PUTCARD '//XPL     EXEC  PGM=XPLSM,REGION=1024K,'
  PUTCARD '// COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(0) SLIB(N) SLIN(NO)   -
      SPRT(N) SPUN(N) SIN(N)
  SET RC=&LASTCC

  IF &LBECMPL = 1 OR &LBECMPL = 2 OR &LBECMPL = 3 THEN +
    DO
      PUTCARD '//PROGRAM  DD DISP=SHR,'
      IF &LBECMPL = 1 THEN +
        PUTCARD '//   DSN=SYSC.XPL.OBJECT(ANALYZER)'
      ELSE -
        DO
          PUTCARD '//   DSN=SYSC.XPL.OBJECT(XCOM)'
          PUTCARD '//FILE1    DD UNIT=SYSDA,SPACE=(TRK,(15,15)),'
          PUTCARD '//       DISP=(,PASS)'
          PUTCARD '//FILE2    DD UNIT=SYSDA,SPACE=(TRK,(15,15))'
          PUTCARD '//FILE3    DD UNIT=SYSDA,SPACE=(TRK,(15,15))'
          PUTCARD '//INPUT2   DD DISP=SHR,'
          PUTCARD '//         DSN=SYSC.XPL.SOURCE(XPLLIBR)'
        END
  %CBGIJCL0 PANID(&PANL0) STEPL(N) TERM(N) UTX(0) SLIB(N) SLIN(NO)   -
      SPRT(Y) SPUN(Y) SIN(Y) SDMP(Y)
    END

  IF &LBECMPL = 3 THEN +
    DO
      PUTCARD '&CMNT'
      PUTCARD '//GO       EXEC  PGM=XPLSM,REGION=1024K,'
      PUTCARD '//   COND=(0,NE)'
      PUTCARD '//PROGRAM  DD DISP=OLD,'
      PUTCARD '//         DSN=*.XPL.FILE1'
      PUTCARD '//FILE1    DD UNIT=SYSDA,SPACE=(CYL,(3,1))'
      PUTCARD '//FILE2    DD UNIT=SYSDA,SPACE=(CYL,(3,1))'
      PUTCARD '//FILE3    DD UNIT=SYSDA,SPACE=(CYL,(3,1))'
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(0) SLIB(N) SLIN(NO)   -
      SPRT(Y) SPUN(Y) SIN(I) SDMP(Y)
      PUTCARD '//*'
    END

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP5F
PROC 0                                /* BG SPITBOL 370             */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP5F                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG SPITBOL 370 JCL.                           */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP5F                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT5F)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LBFCMPL = 1 THEN +
    PUTCARD '//* S P I T B O L  /  3 7 0   Interpreter'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LBFCMPL = 1 THEN +
    PUTCARD '//SPIT370  EXEC PGM=OSINT,'
  PUTCARD '// COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(0) SLIB(N) SLIN(NO)     -
      SPRT(Y) SPUN(Y) SIN(Y)
  SET RC=&LASTCC

  /****************************************************************
  /* Processor Specific JCL
  /****************************************************************
    PUTCARD '//SYSUDUMP DD  SYSOUT=*'

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP5G
PROC 0                                /* BG GCC                     */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP5G                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates GCC JCL.                                      */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP5G                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT5G)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LBGCMPL = 1 THEN +
    PUTCARD '//* G C C   M V S   C O M P I L E'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /*
  /* NOTE: Due to MVS 3.8J TSO, all characters are upper-case
  /* ----- translated.  Since GCC using lower-case for compiler
  /*       parameters, a 'band-aid' solution is being applied
  /*       to render the EXEC card from a PROCLIB which forfeits
  /*       the use of compiler options via the ISPF panel.
  /*       Compiler options can be changed by editing the
  /*       PROC in SYS2.PROCLIB.
  /****************************************************************
  IF &LBGCMPL = 1 THEN +
    PUTCARD '//GCCP    EXEC  GCCISP5'

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(0) SLIB(N) SLIN(NO)     -
      SPRT(N) SPUN(N) SIN(N)
  SET RC=&LASTCC

  /****************************************************************
  /* Processor Specific JCL
  /****************************************************************
  IF &LBGCMPL = 1 THEN +
    DO
      PUTCARD '//INCLUDE  DD DISP=SHR,'
      PUTCARD '//   DSN=PDPCLIB.INCLUDE,DCB=BLKSIZE=32720'
      PUTCARD '//SYSINCL  DD DISP=SHR,'
      PUTCARD '//   DSN=PDPCLIB.INCLUDE,DCB=BLKSIZE=32720'
      PUTCARD '//OUT      DD DISP=(,PASS),UNIT=SYSDA,'
      PUTCARD '//   DCB=(LRECL=80,BLKSIZE=6160,RECFM=FB),'
      PUTCARD '//   DSN=&&TEMP,'
      PUTCARD '//   SPACE=(CYL,(5,2),RLSE)'
      PUTCARD '//SYSIN    DD DISP=SHR,'
      PUTCARD '//         DSN=&LIBMEM'
      PUTCARD '//SYSPRINT DD SYSOUT=(&LBGSOUT)'
      PUTCARD '//SYSTERM  DD SYSOUT=(&LBGSOUT)'
      PUTCARD '//*'
      PUTCARD '//ASM      EXEC PGM=IFOX00,'
      PUTCARD '//   PARM='DECK,LIST,XREF',COND=(4,LT,GCCP.GCC)'
      PUTCARD '//SYSLIB   DD DISP=SHR,DCB=BLKSIZE=32720,'
      PUTCARD '//            DSN=SYS1.MACLIB'
      PUTCARD '//         DD DISP=SHR,DSN=PDPCLIB.MACLIB'
      PUTCARD '//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(3,1))'
      PUTCARD '//SYSUT2   DD UNIT=SYSDA,SPACE=(CYL,(3,1))'
      PUTCARD '//SYSUT3   DD UNIT=SYSDA,SPACE=(CYL,(3,1))'
      PUTCARD '//SYSPRINT DD SYSOUT=(&LBGSOUT)'
      PUTCARD '//SYSLIN   DD DUMMY'
      PUTCARD '//SYSGO    DD DUMMY'
      PUTCARD '//SYSPUNCH DD DISP=OLD,DSN=&LINMEM'
      PUTCARD '//SYSIN    DD DSN=&&TEMP,DISP=(OLD,DELETE)'
    END

  /****************************************************************
  /* Quick GO Step, if selected
  /****************************************************************
  IF &LBGGO = Y THEN +
    DO
      PUTCARD '//*'
      PUTCARD '//LKED     EXEC PGM=IEWL,'
      PUTCARD '//   PARM='MAP','
      PUTCARD '//   COND=((4,LT,GCCP.GCC),(4,LT,ASM))'
      PUTCARD '//SYSLIN   DD DISP=SHR,DSN=&LINMEM'
      PUTCARD '//         DD DDNAME=SYSIN'
      PUTCARD '//SYSIN    DD DUMMY'
      PUTCARD '//SYSLIB   DD DISP=SHR,DSN=PDPCLIB.NCALIB'
      PUTCARD '//SYSLMOD  DD DSN=&&GOSET(GO),UNIT=SYSALLDA,'
      PUTCARD '//            SPACE=(1024,(50,20,1)),'
      PUTCARD '//            DISP=(MOD,PASS)'
      PUTCARD '//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(3,1))'
      PUTCARD '//SYSPRINT DD SYSOUT=(&LBGSOUT)'
      PUTCARD '//*'
      PUTCARD '//GO       EXEC PGM=*.LKED.SYSLMOD,'
      PUTCARD '//   COND=((4,LT,GCCP.GCC),(4,LT,ASM),(4,LT,LKED))'
      PUTCARD '//SYSPRINT DD SYSOUT=(&LBGSOUT)'
      PUTCARD '//SYSTERM  DD SYSOUT=(&LBGSOUT)'
      PUTCARD '//SYSIN    DD DUMMY'
    END

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP5H
PROC 0                                /* BG PL360                   */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP5H                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG PL360 JCL.                                 */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP5H                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT5H)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LBHCMPL = 1 THEN +
    PUTCARD '//* P L 3 6 0   C O M P I L E'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LBHCMPL = 1 THEN +
    PUTCARD '//PL360    EXEC PGM=PL360,'
  PUTCARD '// COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(Y) TERM(N) UTX(0) SLIB(N) SLIN(GO)   -
      SPRT(Y) SPUN(Y) SIN(Y)
  SET RC=&LASTCC

  /****************************************************************
  /* Quick GO Step, if selected
  /****************************************************************
  IF &LBHGO = Y THEN +
    DO
      %CBGIQGO
    END

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP5I
PROC 0                                /* BG BASIC 360               */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP5I                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                  '*/
/*                                                                  */
/* This CLIST creates BG BASIC 360 JCL.                             */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGP5I                                                           */
/*                                                                  */
/* Parameters:                                                      */
/* None                                                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET PANL0 = &STR(ISPOPT5I)           /* Process Panel             */

/*---START------------------- $$cbg-pnl-val -------------------------*/
  /****************************************************************
  /* Set Panel Number, initial CURSOR field, and JCL Message
  /****************************************************************
  SET P = &SUBSTR(8:8,&PANL0)          /* Position 8 of panel name  */
  SET CSRF  = LB&P.MBR                 /* Panel Cursor Field        */
  SET EMSG  = BGOPT55&P                /* JCL Generated message     */

  /****************************************************************
  /* Display Panel and handle response
  /****************************************************************
  GETDATA:  +
  IF &CSRF > &STR() THEN +
    ISPEXEC DISPLAY PANEL(&PANL0) CURSOR(&CSRF)
  ELSE +
    ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY DEPRESSED
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          /*ERROR: END KEY PRESSED, PROCESS TERMINATED! NO JCL GENED
          ISPEXEC SETMSG MSG(BGOPT09)
          GOTO DONE
        END
      /************************************************************
      /* Check for JUMP (e.g. =3.4)
      /************************************************************
      IF &STR(&BGPO) > &STR( ) THEN +
        IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
          DO
          /*SET LBBGPO = &STR(=)   /* Indicate JUMP */
            SET LBBGPO = &STR(&BGPO)
          /*ISPEXEC SETMSG MSG(BGOPT08) */
            GOTO DONE
          END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY DEPRESSED, PROCESS.
      ELSE +
        DO
          /*ERROR: SEVERE ERROR WITH DISPLAY SERVICE
          WRITE SEVERE ERROR IN BG PROCESS
          WRITE DISPLAY PANEL=&PANL0  RC=&RC CSRF=&CSRF
          WRITE PROCESS TERMINATED...
          GOTO DONE
        END
    END

  /****************************************************************
  /* Timing
  /****************************************************************
  SET CPU = &SYSCPU
  SET SRV = &SYSSRV

  /****************************************************************
  /* Validate SOURCE INPUT, Set SYSIN DSN, OBJ DSN
  /****************************************************************
  %CBGVALIN PANID(&PANL0)

  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    IF &RC = 4 THEN +
      GOTO GETDATA
    ELSE +
      DO
        /*ERROR FROM CLIST
        WRITE *** ERROR IN CBGVALIN RC=&RC ***
        GOTO GETDATA
      END

  /****************************************************************
  /* COMMENT:
  /****************************************************************
  SET VARNAM = &STR(&&LB&P.CMPL)
  SET VARVAL = &STR(&VARNAM)
  PUTCARD '&CMNT.----------------------------------------'
  PUTCARD '&CMNT &P..&VARVAL'
/*---END--------------------- $$cbg-pnl-val -------------------------*/

  /****************************************************************
  /* COMMENT: SELECTED LANGUAGE PROCESSOR
  /****************************************************************
  IF &LBICMPL = 1 THEN +
    PUTCARD '//* B A S I C   3 6 0   Interpreter'
  PUTCARD '&CMNT'

/*---START------------------- $$cbg-lib-info ------------------------*/
  /****************************************************************
  /* COMMENT: LIBRARY INFORMATION
  /****************************************************************
  %CBGLLIBS PANID(&PANL0)
  SET RC=&LASTCC
  IF &RC = 0 THEN
  ELSE +
    DO
      /*ERROR FROM CLIST
      WRITE *** ERROR IN CBGLLIBS RC=&RC ***
      WRITE *** Should not display this message !!
    END
/*---END--------------------- $$cbg-lib-info ------------------------*/

  /****************************************************************
  /* EXEC CARD
  /****************************************************************
  IF &LBICMPL = 1 THEN +
      PUTCARD '//BASIC360 EXEC PGM=BASIC360,'
  PUTCARD '// COND=(12,LE),'
/*---START------------------- $$cbg-compileparm ---------------------*/
  /****************************************************************
  /* Construct PARM1 string for Compiler/Assembler Options
  /****************************************************************
  IF &STR(&LBXTST) > &STR( ) THEN -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXTST,&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='&LBXTST''
  ELSE -
    IF &STR(&LBXOPT1) > &STR( ) THEN -
      PUTCARD '//   PARM='&LBXOPT1''
    ELSE -
      PUTCARD '//   PARM='''
/*---END--------------------- $$cbg-compileparm ---------------------*/

  /****************************************************************
  /* Processor Specific STEPLIB
  /****************************************************************
  IF &STR(&SUBSTR(1:1,&LBISTEP1)) > &STR( ) THEN -
    DO
      PUTCARD '//STEPLIB  DD DISP=SHR,'
      PUTCARD '//   DSN=&LBISTEP1'
      PUTCARD '//         DD DISP=SHR,'
      PUTCARD '//   DSN=SYSC.PL1LIB'
    END
  ELSE +
    DO
      PUTCARD '//STEPLIB  DD DISP=SHR,'
      PUTCARD '//   DSN=SYSC.PL1LIB'
    END

  /****************************************************************
  /* INSERT STEPLIB, SYSTERM
  /*        SYSPRINT, SYSPUNCH, SYSIN
  /*        SYSUT  WORK FILES
  /*        SYSLIB
  /*        SYSLIN, SYSGO
  /****************************************************************
  %CBGIJCL0 PANID(&PANL0) STEPL(N) TERM(N) UTX(0) SLIB(N) SLIN(NO)   -
      SPRT(Y) SPUN(N) SIN(Y)
  SET RC=&LASTCC

  /****************************************************************
  /* Processor Specific JCL
  /****************************************************************
  PUTCARD '//* RENUMFL USED IF SYSIN HAS ++RENUM OR *RENUM___ OPTION'
  PUTCARD '//RENUMFL  DD DUMMY,'
  PUTCARD '//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)'

/*---START------------------- $$cbg-morejcl -------------------------*/
  /****************************************************************
  /* Append JCL statement from panel to last step generated
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXJCL)
  DO WHILE &I <= 4
    SET OTHJCL  = &STR(&VARNAM&I)
    IF &STR(&OTHJCL) > &STR( ) THEN +
      DO
        PUTCARD '&OTHJCL'
      END
    SET I = &I + 1
  END
/*---END--------------------- $$cbg-morejcl -------------------------*/

/*---START------------------- $$cbg-endstep -------------------------*/
  /****************************************************************
  /* End of Step
  /****************************************************************
  PUTCARD '//*--END----------------------------------'
  PUTCARD '&CMNT'

  SET JCLGEN = Y

  /****************************************************************
  /* JCL GENERATED Message from CBGP5x panel processor
  /****************************************************************
  ISPEXEC SETMSG MSG(&EMSG)

  /****************************************************************
  /* Performance Marks
  /****************************************************************
  SET CPU = &STR(&SYSCPU-&CPU)
  SET SRV = &STR(&SYSSRV-&SRV)
/*---END--------------------- $$cbg-endstep -------------------------*/

  /****************************************************************
  /* RETURN TO CALLING CLIST
  /****************************************************************

DONE: +
EXIT CODE(0)
./ ADD NAME=CBGP5
PROC 0 DEBUG ICMD()                   /* BG PROCESSING MENU         */

/********************************************************************/
/*                                                                  */
/* CLIST: CBGP5 ICMD()                                              */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Background Processing Option 5                                   */
/*                                                                  */
/* This CLIST is the process driver for background selections for   */
/* use with ISPF 2.1 or greater from Wally Mclaughlin.              */
/*                                                                  */
/* Assign temp DSN and allocate JCL file.                           */
/*                                                                  */
/* Create JOB and other initial JCL statements, first time.         */
/* JCL is written to temp file using PUTCARD.                       */
/*                                                                  */
/* Process selection from menu panel until PF3/PF4                  */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* as issued from menu panel ISP@PRIM -                             */
/*  'CMD(%CBGP5) ICMD(&ZMD) NEWAPPL(CBGP)'                          */
/*                                                                  */
/* Parameters:                                                      */
/* DEBUG    optional, used to display debug information during      */
/*          CLIST execution.  The value for this positional         */
/*          parameter is DEBUG.                                     */
/*                                                                  */
/*          i.e. CBGP5 DEBUG ICMD(&ZCMD)                            */
/*                                                                  */
/* ICMD     ISPF Command, &ZCMD                                     */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

  CONTROL MSG NOFLUSH

  ISPEXEC CONTROL ERRORS RETURN

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  SET JCLGEN = N                       /* BG JCL GENERATED FLAG     */
  SET TDD  = CARDS                     /* BG JCL DD NAME            */
  SET TDSN =                           /* BG JCL TEMP DSN           */
  SET TDSA = CARDSA                    /* BG JCL TEMP DS ATTIB NAME */
  SET LIBMEM=                          /* LIB(MEM) OF SOURCE        */
  SET LINMEM=                          /* LIB(MEM) OF OBJ           */
  SET LMDMEM=                          /* LIB(MEM) OF LOAD          */
  SET ODSNTMP=                         /* ODSN TMP                  */
  SET ODSNMEM=                         /* LIB(MEM) OF ODSN          */
  SET SRCFND=                          /* SOURCE FOUND TEXT         */
  SET CSRF =                           /* CURSOR FIELD FOR REINIT   */
  SET CMNT = &STR(//*)                 /* Comment JCL               */

  /****************************************************************
  /* SET VARIABLES
  /****************************************************************
  SET PANL0 = &STR(ISPOPT5)            /* BG SELECTION PANEL INITIAL*/
  SET JOBC  = N                        /* INITIAL JCL CREATE FLAG   */
  SET SELBGO = N                       /* INIT BATCH SELECTION FLAG */
  SET TS =                             /* Time-Stamp for DSN        */
  CUTIL00 TDSN TS

  /****************************************************************
  /* ALLOCATE TEMP DSN TO HOLD JCL
  /****************************************************************
  SET TDSN = &STR(&SYSUID..CARDS.&TS)
  CONTROL NOMSG
  FREE FI(&TDD)
  FREE ATTR(&TDSA)
  CONTROL MSG
  ATTRIB &TDSA LRECL(80)
  ALLOC FI(&TDD) DA('&TDSN') MOD   REUSE  USING(&TDSA)

  /****************************************************************
  /* DISPLAY PANEL - BACKGROUND SELECTION MENU
  /****************************************************************
  GETDATA:  +
  ISPEXEC DISPLAY PANEL(&PANL0)
  SET RC = &LASTCC
  /* END/RETURN KEY PRESSED, CHECK FOR JUMP */
  /* END/RETURN KEY PRESSED, SUBMIT ANY JCL */
  IF &RC = 8 THEN +
    DO
      IF &KEYPRESS = PF03 THEN +
        DO
          IF (&JCLGEN = Y) THEN +
            DO
              ISPEXEC SETMSG MSG(BGOPT05)     /* JCL submitted    */
              ISPEXEC CONTROL DISPLAY LINE START(22)
              SUBMIT  '&TDSN'
            END
          ELSE +
            DO
              ISPEXEC SETMSG MSG(BGOPT06)     /* No JCL to submit */
            END
          GOTO CLOSME
        END
    END
  ELSE +
    DO
      IF &RC = 0 THEN
      /* ENTER KEY PRESSED, PROCESS SELECTION */
      ELSE +
        DO
          WRITE ** SEVERE ERROR IN BG PROCESS
          WRITE ** DISPLAY PANEL=&PANL0  RC=&RC
          GOTO CLOSME
        END
    END

  /****************************************************************
  /* Check for JUMP (e.g. =3.4)
  /****************************************************************
  IF &STR(&BGPO) > &STR( ) THEN +
    DO
      IF (&SUBSTR(1:1,&STR(&BGPO)) = &STR(=) THEN +
        DO
          GOTO CLOSME
        END
    END
  ELSE +
    /**************************************************************
    /* Check for CANCEL option       From ISPOPT5X only
    /**************************************************************
    IF (&ZCMD = CANCEL  AND  &PANL0 = ISPOPT5X) THEN +
      DO
        ISPEXEC SETMSG MSG(BGOPT07)    /* Process cancelled, no sub */
        GOTO CLOSME
      END
    ELSE +
      /************************************************************
      /* Process BG Selection Option
      /*   - Do not use option X, reserved for MENU confirmation!
      /************************************************************
      IF (&BGPO = 1) OR (&BGPO = 2) OR (&BGPO = 3) OR (&BGPO = 4) OR +
         (&BGPO = 5) OR (&BGPO = 6) OR (&BGPO = 7) OR (&BGPO = 8) OR +
         (&BGPO = 9) OR (&BGPO = A) OR (&BGPO = B) OR (&BGPO = C) OR +
         (&BGPO = D) OR (&BGPO = E) OR (&BGPO = F) OR (&BGPO = G) OR +
         (&BGPO = H) OR (&BGPO = I)                                  +
         THEN +
        DO
          SET SELBGO = Y            /* Made a selection!!  */
          /********************************************************
          /* WRITE INITIAL JOB AND COMMENT CARDS TO JOB STREAM FILE
          /********************************************************
          IF (&JOBC = N) THEN +
            DO
              /****************************************************
              /* Write JCL statments 1-4 if position 1 is not BLANK
              /****************************************************
              SET I = 1
              SET &JCL = &STR(&&JCL)
              DO WHILE &I <= 4
                IF &STR(&SUBSTR(1:1,&JCL&I)) > &STR( ) THEN +
                  DO
                    PUTCARD '&STR(&JCL&I)'
                  END
                SET I = &I + 1
              END
              /****************************************************
              /* Write Temp DSN and Panel Flags
              /****************************************************
              PUTCARD '&CMNT.**BEGIN TDD****************************'
              PUTCARD '&CMNT'
              PUTCARD '&CMNT JCL TEMPORARY DATASET INFORMATION:'
              PUTCARD '&CMNT ----------------------------------'
              PUTCARD '&CMNT      DD : &TDD'
              PUTCARD '&CMNT      DSN: &TDSN'
              PUTCARD '&CMNT      DSA: &TDSA'
              PUTCARD '&CMNT   SOURCE DATA ONLINE : &BGSO'
              PUTCARD '&CMNT   DELETE JCL TEMPFILE: &BGDJ'
              PUTCARD '&CMNT'
              PUTCARD '&CMNT    TSO USER INFORMATION:'
              PUTCARD '&CMNT ----------------------------------'
              PUTCARD '&CMNT   PREFIX: &SYSPREF'
              PUTCARD '&CMNT   SYSUID: &SYSUID'
              PUTCARD '&CMNT'
              PUTCARD '&CMNT.**END  TDD****************************'
              PUTCARD '&CMNT'
              SET JOBC = Y
            END
          /********************************************************
          /* INVOKE BG CLIST as CBGP5x where x is selection
          /********************************************************
          %CBGP5&BGPO
          /********************************************************
          /* Check for JUMP command
          /********************************************************
          IF &STR(&LBBGPO) > &STR( ) THEN +
            IF (&SUBSTR(1:1,&STR(&LBBGPO)) = &STR(=) THEN +
              GOTO CLOSME
          /********************************************************
          /* SET to BG MENU CONFIRMATION PANEL
          /********************************************************
          SET PANL0 = &STR(ISPOPT5X)
        END
      ELSE +
        DO
          ISPEXEC SETMSG MSG(BGOPT04)           /* Invalid selection*/
        END

  /****************************************************************
  /* GET ANOTHER REQUEST FROM SELECTION PANEL
  /****************************************************************
  GOTO GETDATA


/******************************************************************
/* FREE DATASETS
/******************************************************************
CLOSME: +
IF &SELBGO = Y THEN +
  DO
    IF &BGDJ = Y THEN +
      FREE FI(&TDD) DELETE
    ELSE +
      FREE FI(&TDD)
  END
ELSE -
  DO
    FREE FI(&TDD) DELETE
  END

FREE ATTR(&TDSA)


DONE: +
END
./ ADD NAME=CBGVALIN
PROC 0 PANID(NOPANIDX)

/********************************************************************/
/*                                                                  */
/* CLIST: CBGVALIN                                                  */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* This CLIST checks if datasets exists for background processing   */
/* option 5.  They include - source, object, steplib, syslib.       */
/*                                                                  */
/*                                                                  */
/* Command:                                                         */
/* CBGVALIN PANID(PANID)                                            */
/*                                                                  */
/* Parameters:                                                      */
/* PANID    panel name for selected background process              */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/

/*---START------------------- $$cbg-globals -------------------------*/
  /****************************************************************
  /* Global Variables to SHARE with subordinate CLISTS
  /****************************************************************
  GLOBAL JCLGEN TDD TDSN TDSA LIBMEM LINMEM +
         ODSNTMP ODSNMEM SRCFND CSRF CMNT P +
         LMDMEM                             +
         LBBGSO  LBBGPO                     +
         LBXODSN LBXPROJ LBXTYPE LBXMBR     +
         LBXLIB1 LBXLIB2 LBXLIB3 LBXLIB4    +
         LBXSTEP1                           +
         LBXTST  LBXOPT1                    +
         LBXGO   LBXCMPL                    +
         LBXSOUT LBXLSTID                   +
         LBXSL1  LBXSL2  LBXSL3  LBXSL4     +
         LBXJCL1 LBXJCL2 LBXJCL3 LBXJCL4
/*---END--------------------- $$cbg-globals -------------------------*/

  /****************************************************************
  /* GET ISPF PANEL PROFILE VARIABLES
  /* - USING LAST BYTE OF PANELID
  /*   GET ASSOCIATED ISPF VARS
  /*   SET X VARS FROM ASSOCIATED ISPF VARS
  /****************************************************************

  ISPEXEC VGET (BGSO BGPO)
  ISPEXEC VGET (LB&P.ODSN)
  ISPEXEC VGET (LB&P.PROJ LB&P.TYPE LB&P.LIB1 LB&P.MBR)
  ISPEXEC VGET (LB&P.LIB2 LB&P.LIB3 LB&P.LIB4)
  ISPEXEC VGET (LB&P.STEP1)
  ISPEXEC VGET (LB&P.SL1 LB&P.SL2 LB&P.SL3 LB&P.SL4)
  ISPEXEC VGET (LB&P.JCL1 LB&P.JCL2 LB&P.JCL3 LB&P.JCL4) PROFILE
  ISPEXEC VGET (LB&P.TST LB&P.OPT1)
  ISPEXEC VGET (LB&P.GO  LB&P.CMPL)
  ISPEXEC VGET (LB&P.SOUT LB&P.LSTID)

  /****************************************************************
  /* TRIM all PANEL VARIABLES
  /****************************************************************
  CUTIL00 TRIM BGSO       LBBGSO
  CUTIL00 TRIM BGPO       LBBGPO
  CUTIL00 TRIM LB&P.ODSN  LBXODSN
  CUTIL00 TRIM LB&P.PROJ  LBXPROJ
  CUTIL00 TRIM LB&P.TYPE  LBXTYPE
  CUTIL00 TRIM LB&P.MBR   LBXMBR
  CUTIL00 TRIM LB&P.LIB1  LBXLIB1
  CUTIL00 TRIM LB&P.LIB2  LBXLIB2
  CUTIL00 TRIM LB&P.LIB3  LBXLIB3
  CUTIL00 TRIM LB&P.LIB4  LBXLIB4
  CUTIL00 TRIM LB&P.STEP1 LBXSTEP1
  CUTIL00 TRIM LB&P.SL1   LBXSL1
  CUTIL00 TRIM LB&P.SL2   LBXSL2
  CUTIL00 TRIM LB&P.SL3   LBXSL3
  CUTIL00 TRIM LB&P.SL4   LBXSL4
  CUTIL00 TRIM LB&P.JCL1  LBXJCL1
  CUTIL00 TRIM LB&P.JCL2  LBXJCL2
  CUTIL00 TRIM LB&P.JCL3  LBXJCL3
  CUTIL00 TRIM LB&P.JCL4  LBXJCL4
  CUTIL00 TRIM LB&P.TST   LBXTST
  CUTIL00 TRIM LB&P.OPT1  LBXOPT1
  CUTIL00 TRIM LB&P.GO    LBXGO
  CUTIL00 TRIM LB&P.CMPL  LBXCMPL
  CUTIL00 TRIM LB&P.SOUT  LBXSOUT
  CUTIL00 TRIM LB&P.LSTID LBXLSTID

  SET &EXITCD = 12

  SET CSRF =


  /****************************************************************
  /* Panel Edits
  /****************************************************************

  /*****************************/
  /* ODSN or LIB1 required     */
  /*****************************/
  IF &STR(&LBXODSN) > &STR( ) THEN
  ELSE -
    DO
      IF &STR(&LBXPROJ) = &STR( )   AND -
      IF &STR(&LBXLIB1) = &STR( )   AND -
      IF &STR(&LBXTYPE) = &STR( )   AND -
      IF &STR(&LBXMBR)  = &STR( )   THEN -
        DO
          SET CSRF = LBXPROJ           /* No ISPF LIB1 or ODSN found*/
          ISPEXEC SETMSG MSG(BGOPT12)
          GOTO VALINXT
        END
      /*****************************/
      /* Edit ISPF PROJ            */
      /*****************************/
      IF &CSRF = &STR() THEN -
        IF &STR(&LBXPROJ) > &STR( ) THEN -
          DO
            CUTIL00 ISDSN LBXPROJ
            IF &ERRMSG = FALSE THEN -
              SET CSRF = LBXPROJ       /* PROJ not DSN              */
          END
        ELSE -
          SET CSRF = LBXPROJ           /* PROJ is BLANK             */
      /*****************************/
      /* Edit ISPF LIB1            */
      /*****************************/
      IF &CSRF = &STR() THEN -
        IF &STR(&LBXLIB1) > &STR( ) THEN -
          DO
            CUTIL00 ISDSN LBXLIB1
            IF &ERRMSG = FALSE THEN -
              SET CSRF = LBXLIB1       /* LIB1 not DSN              */
          END
        ELSE -
          SET CSRF = LBXLIB1           /* LIB1 is BLANK             */
      /*****************************/
      /* Edit ISPF TYPE            */
      /*****************************/
      IF &CSRF = &STR() THEN -
        IF &STR(&LBXTYPE) > &STR( ) THEN -
          DO
            CUTIL00 ISDSN LBXTYPE
            IF &ERRMSG = FALSE THEN -
              SET CSRF = LBXTYPE       /* TYPE not DSN              */
          END
        ELSE -
          SET CSRF = LBXTYPE           /* TYPE is BLANK             */
      /*****************************/
      /* Edit ISPF MEMBER          */
      /*****************************/
      IF &CSRF = &STR() THEN -
        IF &STR(&LBXMBR) > &STR( ) THEN -
          DO
            CUTIL00 ISDSN LBXMBR
            IF &ERRMSG = FALSE THEN -
              SET CSRF = LBXMBR        /* MBR  not DSN              */
          END
        ELSE -
          SET CSRF = LBXMBR            /* MBR  is BLANK             */
    END
    /*****************************/
    /* Edit STEPLIB              */
    /*****************************/
    IF &CSRF = &STR() THEN -
      IF &STR(&LBXSTEP1) > &STR( ) THEN -
        DO
          CUTIL00 ISDSN LBXSTEP1
          IF &ERRMSG = FALSE THEN -
            SET CSRF = LBXSTEP1        /* STEPLIB not DSN           */
        END
    /*****************************/
    /* Edit SYSLIB1              */
    /*****************************/
    IF &CSRF = &STR() THEN -
      IF &STR(&LBXLIB1) > &STR( ) THEN -
        DO
          CUTIL00 ISDSN LBXLIB1
          IF &ERRMSG = FALSE THEN -
            SET CSRF = LBXLIB1         /* LIB1 not DSN              */
        END
    /*****************************/
    /* Edit SYSLIB2              */
    /*****************************/
    IF &CSRF = &STR() THEN -
      IF &STR(&LBXLIB2) > &STR( ) THEN -
        DO
          CUTIL00 ISDSN LBXLIB2
          IF &ERRMSG = FALSE THEN -
            SET CSRF = LBXLIB2         /* LIB2 not DSN              */
        END
    /*****************************/
    /* Edit SYSLIB3              */
    /*****************************/
    IF &CSRF = &STR() THEN -
      IF &STR(&LBXLIB3) > &STR( ) THEN -
        DO
          CUTIL00 ISDSN LBXLIB3
          IF &ERRMSG = FALSE THEN -
            SET CSRF = LBXLIB3         /* LIB2 not DSN              */
        END
    /*****************************/
    /* Edit SYSLIB4              */
    /*****************************/
    IF &CSRF = &STR() THEN -
      IF &STR(&LBXLIB4) > &STR( ) THEN -
        DO
          CUTIL00 ISDSN LBXLIB4
          IF &ERRMSG = FALSE THEN -
            SET CSRF = LBXLIB4         /* LIB2 not DSN              */
        END
  /*****************************/
  /* LISTID or SYSOUT CLASS required
  /*****************************/
  IF &CSRF = &STR() THEN -
    IF &STR(&LBXLSTID) > &STR( ) THEN -
      DO
        CUTIL00 ISDSN LBXLSTID
        IF &ERRMSG = FALSE THEN -
          SET CSRF = LBXLSTID        /* LISTID not DSN            */
      END
    ELSE -
      IF &STR(&LBXSOUT) > &STR( ) THEN
      ELSE -
        SET CSRF = LBXSOUT           /* SYSOUT is BLANK           */
  /*****************************/
  /* Post error if cursor field set
  /*****************************/
  IF &CSRF > &STR() THEN -
    DO
      IF &CSRF = LBXSOUT THEN -
        ISPEXEC SETMSG MSG(BGOPT01)
      ELSE -
        ISPEXEC SETMSG MSG(BGOPT11)
      GOTO VALINXT
    END


  /****************************************************************
  /* Validate ISPF Library Existence   (ISPF LIB 1-4)
  /****************************************************************
  IF &LBBGSO = Y THEN +
    DO
      SET I = 1
      SET &VARNAM = &STR(&&LBXLIB)
      /********************************/
      /* CHECK LBXTYPE 1-4 FOR MEMBER EXISTANCE, IF NOT BLANK
      /********************************/
      DO WHILE &I <= 4
        SET VARVAL = &STR(&VARNAM&I)
        IF &STR(&VARVAL) > &STR( ) THEN +
          DO
            IF &SYSDSN('&LBXPROJ..&VARVAL..&LBXTYPE') = OK THEN
            ELSE +
              DO
                SET CSRF = LBXLIB&I
                ISPEXEC SETMSG MSG(BGOPT10)
                GOTO VALINXT
              END
          END
        SET I = &I + 1
      END
    END


  /****************************************************************
  /* SOURCE DATA SET AND LIBRARIES VALIDATION
  /****************************************************************
  /*  At this point, LIBRARIES are validated, if SOURCE ONLINE = Y
  /*
  SET ODSNTMP =
  SET ODSNMEM =
  IF &LBBGSO = Y THEN +
    SET SRCFND =
  ELSE +
    SET SRCFND = &STR(//*   )NO SOURCE SEARCH PERFORMED
  /************************************/
  /* Check ODSN, if specified         */
  /************************************/
  IF &STR(&LBXODSN) > &STR() THEN +
    DO
      SET CSRF = LBXODSN
      SET ODSNTMP = &STR(&LBXODSN)
      SET L = &LENGTH(&ODSNTMP)
      SET M = 1
      /********************************/
      /*ODSN FULLY QUALIFIED?         */
      /********************************/
      IF &SUBSTR(1:1,&ODSNTMP) = &STR(') THEN +
        DO
          IF &SUBSTR(&L:&L,&ODSNTMP) = &STR(') THEN +
            SET &ODSNTMP = &SUBSTR(2:&L-1,&ODSNTMP)
          ELSE +
            SET &ODSNTMP = &SUBSTR(2:&L,&ODSNTMP)
        END
      /********************************/
      /*ODSN NOT FULLY QUALIFIED,     */
      /*  INSERT &SYSPREF             */
      /********************************/
      ELSE +
        DO
          IF &STR(&SYSPREF) > &STR( ) THEN -
            SET &ODSNTMP = &SYSPREF..&STR(&ODSNTMP)
          ELSE -
            SET &ODSNTMP = &SYSUID..&STR(&ODSNTMP)
        END
      SET L = &LENGTH(&ODSNTMP)
      SET LIBDSN =
      SET LIBMEM =
      /********************************/
      /* FIND MEMBER NAME IN ODSN     */
      /********************************/
      DO WHILE &M <= &L
        IF .&SUBSTR(&M:&M,&ODSNTMP) = .( THEN +
          DO
            SET LIBMEM = &LIBDSN&SUBSTR(&M:&L,&ODSNTMP)
            SET ODSNMEM = &SUBSTR(&M+1:&L-1,&ODSNTMP)
            SET M = &L
          END
        ELSE +
          DO
            SET LIBDSN = &LIBDSN&SUBSTR(&M:&M,&ODSNTMP)
            SET ODSNMEM = &STR(TEMPNAME)
          END
        SET M = &M + 1
      END
      /********************************/
      /* ASSIGN LIBDSN TO LIBMEM IF MEMBER NOT FOUND, MUST BE SEQ FILE
      /********************************/
      IF &LIBMEM = &STR() THEN +
        SET LIBMEM = &LIBDSN
      /********************************/
      /* CHECK FOR SOURCE IF DATA ONLINE = Y
      /********************************/
      IF &LBBGSO = Y THEN +
        DO
          /****************************/
          /* ODSN EXIST?              */
          /****************************/
          IF &SYSDSN('&LIBMEM') = OK THEN +
            DO
              /************************/
              /* Check DSORG          */
              /************************/
              LISTDSJ '&LIBDSN'      /*MVS38J LISTDSI */
              IF &SYSDSORG = PO THEN +
                DO
                  /********************/
                  /* PDS, IF ')' AT END ASSUME MEMBER NAME CAPTURED
                  /********************/
                  IF &SUBSTR(&L:&L,&ODSNTMP) = &STR()) THEN
                  ELSE +
                    DO
                      /*ERROR: MISSING PDS MEMBER NAME
                      ISPEXEC SETMSG MSG(BGOPT16)
                      GOTO VALINXT
                    END
                END
              ELSE +
              IF &SYSDSORG = PS THEN +
                DO
                  /* SEQ, ASSIGN TEMPNAME AS MEMBER NAME FOR OBJ
                  SET ODSNMEM = &STR(TEMPNAME)
                END
              ELSE +
                DO
                  /*ERROR: INVALID DSORG FOR SOURCE
                  ISPEXEC SETMSG MSG(BGOPT15)
                  GOTO VALINXT
                END
              GOTO INDSNOK
            END
          ELSE +
            DO
              /*ERROR: ODSN SPECIFIED, BUT NOT FOUND
              ISPEXEC SETMSG MSG(BGOPT13)
              GOTO VALINXT
            END
        END
      ELSE +
        DO
          SET SRCFND = &STR(&SRCFND), USE ODSN AS IS
        END
    END
  /************************************/
  /* OTHERWISE, CHECK EACH SOURCE DSN BY LIBRARY UNTIL SOURCE FOUND.
  /************************************/
  ELSE +
    DO
      SET CSRF = LBXLIB1
      SET I = 1
      SET &VARNAM = &STR(&&LBXLIB)
      /********************************/
      /* CHECK LBXTYPE 1-4 FOR MEMBER EXISTANCE, IF NOT BLANK
      /********************************/
      DO WHILE &I <= 4
        SET VARVAL = &STR(&VARNAM&I)
        IF &STR(&VARVAL) > &STR( ) THEN +
          DO
            SET LIBDSN = &LBXPROJ             /*  BUILD        */
            SET LIBDSN = &LIBDSN..&VARVAL     /*  THE          */
            SET LIBDSN = &LIBDSN..&LBXTYPE    /*  LIB DSN      */
            SET LIBMEM = &LIBDSN(&LBXMBR      /*  WITH         */
            SET LIBMEM = &LIBMEM)             /*  MEMBER NAME  */
            /**************************/
            /* CHECK FOR SOURCE IF DATA ONLINE = Y
            /**************************/
            IF &LBBGSO = Y THEN +
              DO
                IF &SYSDSN('&LIBMEM') = OK THEN +
                  DO
                    SET SRCFND = &STR(//*      )SOURCE FOUND
                    SET SRCFND = &STR(&SRCFND) IN LIBRARY &I
                    GOTO INDSNOK
                  END
              END
            /**************************/
            /* USE FIRST LIB IF DATA ONLINE = N
            /**************************/
            ELSE +
              DO
                SET SRCFND = &STR(&SRCFND), USE LIB 1 AS IS
                GOTO INDSNOK
              END
          END
        SET I = &I + 1
      END
    END

  /*ERROR: DSN NOT FOUND IN LIBS CONCATENATION
  IF &LBBGSO = Y THEN +
    DO
      ISPEXEC SETMSG MSG(BGOPT14)
      GOTO VALINXT
    END


  /****************************************************************
  /* OBJ DSN: ALWAYS FIRST LIBRARY IN CONCATENATION
  /****************************************************************
  /* if seq, DSN and sub last node with OBJ
  /* if pds, DSN with last node as OBJ with SOURCE member name
  /* if ispf lib, 1stLIB  w last node as OBJ with SOURCE member name
  /*
/*INDSNOK: -
/*WRITE INDSNOK
/*WRITE LBXODSN='&LBXODSN'
/*WRITE LIBMEM ='&LIBMEM'
/*WRITE LIBDSN ='&LIBDSN'
/*WRITE ODSNMEM='&ODSNMEM'

  INDSNOK: -
  IF &STR(&ODSNMEM) = &STR(TEMPNAME) THEN -
    DO
      /********************************/
      /* ODSN w/o MEMBER              */
      /********************************/
      /* ODSN dsn=replace last node w "OBJ"
      /********************************/
      SET DLMTR = &STR(".")
      CUTIL00 INDEXB LIBDSN  DLMTR
      SET INX = &LASTCC
      IF &INX > 0 THEN -
        DO
          SET LINMEM = &SUBSTR(1:&INX,&STR(&LIBDSN))
          SET LINMEM = &STR(&LINMEM)OBJ
          SET LMDMEM = &STR(LOAD_IS_SEQ_INVALID)
        END
    END
  ELSE -
    DO
      /********************************/
      /* ODSN or LIB w/ MEMBER        */
      /********************************/
      IF &STR(&LBXLIB1) > &STR( ) THEN -
        DO
          SET LINDSN = &LBXPROJ..&LBXLIB1..OBJ
          SET LINMEM = &STR(&LINDSN)(&LBXMBR)
          SET LMDMEM = &LBXPROJ..&LBXLIB1..LOAD
          SET LMDMEM = &STR(&LMDMEM)(&LBXMBR)
        END
      ELSE -
        DO
          /****************************/
          /* ODSN dsn=replace last node w "OBJ"
          /****************************/
          SET DLMTR = &STR(".")
          CUTIL00 INDEXB LIBDSN  DLMTR
          SET INX = &LASTCC
          IF &INX > 0 THEN -
            DO
              SET LINMEM = &SUBSTR(1:&INX,&STR(&LIBDSN))
              SET LINMEM = &STR(&LINMEM)OBJ
              SET LMDMEM = &STR(&LINMEM)LOAD
            END
          SET LINMEM = &STR(&LINMEM)(&ODSNMEM)
          SET LMDMEM = &STR(&LMDMEM)(&ODSNMEM)
        END
    END
/*WRITE LINMEM ='&LINMEM'
/*WRITE LMDMEM ='&LMDMEM'


  /****************************************************************
  /* STEPLIB VALIDATION
  /****************************************************************
  IF &LENGTH(&LBXSTEP1) > 0 THEN -
    DO
      SET CSRF = LBXSTEP1
      /********************************/
      /* CHECK DSN EXISTANCE
      /********************************/
      IF &SYSDSN('&LBXSTEP1') = OK THEN
      ELSE +
        DO
          /*ERROR: STEPLIB NOT FOUND
          ISPEXEC SETMSG MSG(BGOPT17)
          GOTO VALINXT
        END
    END


  /****************************************************************
  /* SYSLIB  VALIDATION  SYSLIBS 1-4
  /****************************************************************
  SET I = 1
  SET &VARNAM = &STR(&&LBXSL)
  DO WHILE &I <= 4
    SET DSNSL  = &VARNAM&I
    IF &LENGTH(&DSNSL) > 0 THEN +
      DO
        /******************************/
        /* CHECK DSN EXISTANCE
        /******************************/
        IF &SYSDSN('&DSNSL') = OK THEN
        ELSE +
          DO
            /*ERROR: SYSLIB  NOT FOUND
            ISPEXEC SETMSG MSG(BGOPT18)
            SET CSRF = LBXSL&I
            GOTO VALINXT
          END
      END
    SET I = &I + 1
  END



  SET &EXITCD = 0
  GOTO DONE

VALINXT: +
  SET &EXITCD = 4
  GOTO DONE



DONE: +
SET L = &LENGTH(&CSRF)
/**************************************/
/* Convert cursor field to actual      /
/* panel variable name using &P        /
/**************************************/
IF &L > 0 THEN +
  SET &CSRF = &SUBSTR(1:2,&CSRF)&P&SUBSTR(4:&L,&CSRF)

EXIT CODE(&EXITCD)
@@
//MLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYSGEN.ISPF.MLIB
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=BGOPT0
/********************************************************************/
/*                                                                  */
/* MESSAGES: BGOPT0                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 ISPOPT5 messages                                        */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
BGOPT00  '&ZERRSM                 ' .ALARM=NO
'&ZERRMSG &ZERRLM                                                              '
BGOPT01  '                        ' .ALARM=NO
'BGOPT01  Cannot be blank                                                      '
BGOPT02  '                        ' .ALARM=NO
'BGOPT02  Invalid value                                                        '
BGOPT03  '                        ' .ALARM=NO
'BGOPT03  Invalid dataset name                                                 '
BGOPT04  '                        ' .ALARM=NO
'BGOPT04  Invalid Process Selection                                            '
BGOPT05  '                        ' .ALARM=NO
'BGOPT05  JCL submitted ... END key pressed                                    '
BGOPT06  '                        ' .ALARM=NO
'BGOPT06  No JCL to submit, END key pressed                                    '
BGOPT07  '                        ' .ALARM=NO
'BGOPT07  CANCEL requested, no JCL submitted                                   '
BGOPT08  'BG process interrupted  ' .ALARM=NO
'BGOPT08  Process interrupted, no JCL submitted                                '
BGOPT09  '                        ' .ALARM=NO
'BGOPT09  NO ACTION TAKEN on previous panel.  END key pressed.                 '
./ ADD NAME=BGOPT1
/********************************************************************/
/*                                                                  */
/* MESSAGES: BGOPT1                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 ISPOPT5 messages                                        */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
BGOPT10  '                        ' .ALARM=NO
'BGOPT10  ISPF Library not found - &LBXPROJ..&VARVAL..&LBXTYPE                 '
BGOPT11  '                        ' .ALARM=NO
'BGOPT11  Invalid DSN node or blank                                            '
BGOPT12  '                        ' .ALARM=NO
'BGOPT12  ISPF Library 1 or Other Partitioned or Sequential Data Set required! '
BGOPT13  '                        ' .ALARM=NO
'BGOPT13  ODSN not found  &LIBMEM                                              '
BGOPT14  '                        ' .ALARM=NO
'BGOPT14  Source not found in ODSN or LIBs                                     '
BGOPT15  '                        ' .ALARM=NO
'BGOPT15  ODSN not PDS or SEQ file &LIBMEM                                     '
BGOPT16  '                        ' .ALARM=NO
'BGOPT16  Missing member name for &LIBMEM                                      '
BGOPT17  '                        ' .ALARM=NO
'BGOPT17  STEPLIB not found  &LBXSTEP1                                         '
BGOPT18  '                        ' .ALARM=NO
'BGOPT18  SYSLIB&I not found  &DSNSL                                           '
BGOPT19  '                        ' .ALARM=NO
'BGOPT19  Must be blank                                                        '
./ ADD NAME=BGOPT55
/********************************************************************/
/*                                                                  */
/* MESSAGES: BGOPT55                                                */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 ISPOPT5 messages                                        */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
BGOPT551 '                        ' .ALARM=NO
'BGOPT551 Assembler JCL generated for &LIBMEM                                  '
BGOPT552 '                        ' .ALARM=NO
'BGOPT552 OS/VS COBOL JCL generated for &LIBMEM                                '
BGOPT553 '                        ' .ALARM=NO
'BGOPT553 Fortran IV JCL generated for &LIBMEM                                 '
BGOPT554 '                        ' .ALARM=NO
'BGOPT554 Mortran JCL generated for &LIBMEM                                    '
BGOPT555 '                        ' .ALARM=NO
'BGOPT555 PL/1 JCL generated for &LIBMEM                                       '
BGOPT556 '                        ' .ALARM=NO
'BGOPT556 LINK-EDIT JCL generated for &LIBMEM                                  '
BGOPT557 '                        ' .ALARM=NO
'BGOPT557 ASSIST JCL generated for &LIBMEM                                     '
BGOPT558 '                        ' .ALARM=NO
'BGOPT558 ALGOL JCL generated for &LIBMEM                                      '
BGOPT559 '                        ' .ALARM=NO
'BGOPT559 PASCAL JCL generated for &LIBMEM                                     '
./ ADD NAME=BGOPT5
/********************************************************************/
/*                                                                  */
/* MESSAGES: BGOPT5                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 ISPOPT5 messages                                        */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
BGOPT55A '                        ' .ALARM=NO
'BGOPT55A RPG MVT JCL generated for &LIBMEM                                    '
BGOPT55B '                        ' .ALARM=NO
'BGOPT55B Simula JCL generated for &LIBMEM                                     '
BGOPT55C '                        ' .ALARM=NO
'BGOPT55C SNOBOL JCL generated for &LIBMEM                                     '
BGOPT55D '                        ' .ALARM=NO
'BGOPT55D Watfiv JCL generated for &LIBMEM                                     '
BGOPT55E '                        ' .ALARM=NO
'BGOPT55E XPL JCL generated for &LIBMEM                                        '
BGOPT55F '                        ' .ALARM=NO
'BGOPT55F SPITBOL JCL generated for &LIBMEM                                    '
BGOPT55G '                        ' .ALARM=NO
'BGOPT55G GCC JCL generated for &LIBMEM                                        '
BGOPT55H '                        ' .ALARM=NO
'BGOPT55H PL360 JCL generated for &LIBMEM                                      '
BGOPT55I '                        ' .ALARM=NO
'BGOPT55I BASIC 360 JCL generated for &LIBMEM                                  '
BGOPT56A '                        ' .ALARM=NO
'BGOPT56A OS/VS LINK-EDIT JCL GENERATED FOR &LIBMEM                            '
@@
//PLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYSGEN.ISPF.PLIB
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=HSPOPT51
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT51                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background Assembler help panel                         */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial------------  BACKGROUND Assembler  -------------------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT52
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT52                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background COBOL help panel                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial---------------  BACKGROUND COBOL Compiler  -----------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT53
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT53                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background FORTRAN help panel                           */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial---------------  BACKGROUND Fortran Compiler  ---------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT54
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT54                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background Mortran Translator help panel                */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial-------------  BACKGROUND Mortran Translator  ---------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+ Mortran/Fortran    %MORTRAN and FORTRAN options (parms)
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT55
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT55                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background PL/I help panel                              */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial---------------  BACKGROUND PL/I F Compiler  ----------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT56
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT56                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background Linkage Edit help panel                      */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial---------------  BACKGROUND Linkage Editor  -----------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ LIST   %prefix.listid.LIST     # Optional +
+ LOAD   %project.lib1.LOAD(member) or ODSN-1node.LOAD(member)             +
+
+$Processor Field   +$Description                                              +
+ Term               %TERM- TERM dataset  NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statemente appended to Link Edit step
+
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT57
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT57                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background ASSIST help panel                            */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial---------------  BACKGROUND ASSIST Interpreter --------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT58
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT58                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background ALGOL help panel                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial---------------  BACKGROUND ALGOL Compiler  -----------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT59
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT59                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background PASCAL help panel                            */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial---------------  BACKGROUND PASCAL Compiler  ----------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT5A
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT5A                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background RPG help panel                               */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial---------------  BACKGROUND RPG Compiler  -------------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT5B
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT5B                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background SIMULA help panel                            */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial---------------  BACKGROUND SIMULA Compiler  ----------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT5C
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT5C                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background SNOBOL help panel                            */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial---------------  BACKGROUND SNOBOL Interpreter --------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT5D
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT5D                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background WATFIV help panel                            */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial---------------  BACKGROUND WATFIV Interpreter  -------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT5E
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT5E                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background XPL help panel                               */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial---------------  BACKGROUND XPL Compiler  -------------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT5F
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT5F                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background SPITBOL help panel                           */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial------------  BACKGROUND SPITBOL 370 Interpreter  -----------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT5G
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT5G                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background GCC help panel                               */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial---------------  BACKGROUND GCC Compiler  -------------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT5H
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT5H                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background PL360 help panel                             */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial---------------  BACKGROUND PL360 Compiler  -----------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT5I
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT5I                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background BASIC 360 help panel                         */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial-------------  BACKGROUND BASIC 360 Interpreter  ------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
+ This function creates batch processor JCL for submission via TSO.
+
+$TYPE  +$Data Set Naming Convention                                           +
+ INPUT  %ODSN+or%project.lib1-4+with%project.lib1+at minimum                  +
+ OBJ    %project.lib1.OBJ(member), ODSN-1node.OBJ(member) or SEQdsn-1node.OBJ
+ TERM   %prefix.member.TERM     # Optional, member=TEMPNAME for SEQ Source  +
+ PUNCH  %prefix.member.PUNCH    # SYSOUT=B unless noted in processor +
+ LIST   %prefix.listid.LIST     # Optional +
+
+$Processor Field   +$Description (field ignored if noted in processor panel)  +
+ Term               %TERM- TERM dataset  blank,NOTERM- no TERM dataset
+ Quik GO            %Add simple GO step  (Y/N)
+ STEPLIB            %STEPLIB DD, validated if specified
+ Process            %Processor type from list
+ List ID            %LIST dataset, if specified. Otherwise, hardcopy.
+ SYSOUT class       %Sysout class for hardcopy
+ Parms              %Processor specific options (parms)
+ SYSLIBs            %Up to 4 SYSLIBs, validated if specified
+ MoreJCL            %Up to 4 JCL statements appended to processor or GO step
+
+ See appropriate language reference manuals for syntax and/or options.
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=HSPOPT5
/********************************************************************/
/*                                                                  */
/*    PANEL: HSPOPT5                                                */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background Selection menu help panel                    */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 [ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(TURQ)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial--------------  BACKGROUND SELECTION MENU  ------------------Tutorial-
%SELECT OPTION ===>_ZCMD                                              +&ZPANELID
%
[ This menu facilitates selection of various language processor panels.  Each
[ panel/CLIST creates JCL for submission via TSO.
[
[ While using the background panels, you can continue using ISPF interactively
[ with no session waits.
[
[ When+Source DATA online[=%Y[source existence is verified real-time by
[ searching%library1-4.[Otherwise,^ODSN[is validated. An error message is
[ displayed if source is not found.
+
[ When+Source DATA online[=%N[source is not validated.  Source location is from
[^ODSN[if specified. Otherwise, source location is from%library1.[
+
+$Field Name        +$Description
+ Delete SUBMIT JCL :%N+delete temp file      %Y+keep temp file
+ Source DATA online:%N+use^ODSN+or^library1  %Y+confirm existence real-time
+ USING             :%Batch session JCL Data Set Name
+ JCL               :%Up to 4 lines for JOB statement
+
+^ ODSN[Other Partitioned or Sequential Data Set Name
+
)INIT
  .CURSOR = ZCMD
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)END
./ ADD NAME=ISPOPT51
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT51                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background Assembler panel                              */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%---------------------  BACKGROUND Assembler  ----------------------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LB1PROJ +                                             {Z
+   LIBRARY ===>_LB1LIB1 + ===>_LB1LIB2 + ===>_LB1LIB3 + ===>_LB1LIB4 +
+   TYPE    ===>_LB1TYPE +
+   MEMBER  ===>_LB1MBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LB1ODSN                                         +
+
+Processor options:      +Term ==>_LB1TST+ QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LB1STEP1                                    _Z+ 1. Assembler XF
+ List ID ==>_LB1LSTID%or+SYSOUT class ==>_Z+                 2. XF Macro XREF
+ Parms   ==>_LB1OPT1                                 +       3. G  Waterloo
+ SYSLIB1 ==>_LB1SL1                                          +
+ SYSLIB2 ==>_LB1SL2                                          +
+ SYSLIB3 ==>_LB1SL3                                          +
+ SYSLIB4 ==>_LB1SL4                                          +
+ MoreJCL ==>_LB1JCL1                                                          +
+         ==>_LB1JCL2                                                          +
+         ==>_LB1JCL3                                                          +
+         ==>_LB1JCL4                                                          +
)INIT
  .HELP = HSPOPT51
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT51   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT51 /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LB1GO,LB1CMPL,LB1SOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LB1PROJ LB1LIB1 LB1LIB2 LB1LIB3 LB1LIB4 LB1TYPE LB1MBR) PROFILE
  VGET (LB1ODSN) PROFILE
  VGET (LB1LSTID LB1SOUT LB1GO LB1CMPL) PROFILE
  VGET (LB1TST LB1OPT1) PROFILE
  VGET (LB1SL1 LB1SL2 LB1SL3 LB1SL4) PROFILE
  VGET (LB1JCL1 LB1JCL2 LB1JCL3 LB1JCL4) PROFILE
  VGET (LB1STEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LB1TST,NB,MSG=BGOPT01)
    VER (&LB1TST,LIST,TERM,NOTERM,MSG=BGOPT02)
 /* VER (&LB1GO,NB,MSG=BGOPT01)
    VER (&LB1GO,LIST,Y,N,MSG=BGOPT02)
    VER (&LB1CMPL,NB,MSG=BGOPT01)
    VER (&LB1CMPL,LIST,1,2,3,MSG=BGOPT02)
    VPUT (LB1PROJ LB1LIB1 LB1LIB2 LB1LIB3 LB1LIB4 LB1TYPE LB1MBR) PROFILE
    VPUT (LB1ODSN) PROFILE
    VPUT (LB1LSTID LB1SOUT LB1GO LB1CMPL) PROFILE
    VPUT (LB1TST LB1OPT1) PROFILE
    VPUT (LB1SL1 LB1SL2 LB1SL3 LB1SL4) PROFILE
    VPUT (LB1JCL1 LB1JCL2 LB1JCL3 LB1JCL4) PROFILE
    VPUT (LB1STEP1) PROFILE

)END
./ ADD NAME=ISPOPT52
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT52                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background COBOL panel                                  */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%------------------------  BACKGROUND COBOL Compiler  --------------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LB2PROJ +                                             {Z
+   LIBRARY ===>_LB2LIB1 + ===>_LB2LIB2 + ===>_LB2LIB3 + ===>_LB2LIB4 +
+   TYPE    ===>_LB2TYPE +
+   MEMBER  ===>_LB2MBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LB2ODSN                                         +
+
+Processor options:      +Term ==>_LB2TST+ QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LB2STEP1                                    _Z+ 1. MVT COBOL
+ List ID ==>_LB2LSTID%or+SYSOUT class ==>_Z+                 2. n/a
+ Parms   ==>_LB2OPT1                                 +       3. n/a
+ SYSLIB1 ==>_LB2SL1                                          +
+ SYSLIB2 ==>_LB2SL2                                          +
+ SYSLIB3 ==>_LB2SL3                                          +
+ SYSLIB4 ==>_LB2SL4                                          +
+ MoreJCL ==>_LB2JCL1                                                          +
+         ==>_LB2JCL2                                                          +
+         ==>_LB2JCL3                                                          +
+         ==>_LB2JCL4                                                          +
)INIT
  .HELP = HSPOPT52
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT52   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT52 /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LB2GO,LB2CMPL,LB2SOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LB2PROJ LB2LIB1 LB2LIB2 LB2LIB3 LB2LIB4 LB2TYPE LB2MBR) PROFILE
  VGET (LB2ODSN) PROFILE
  VGET (LB2LSTID LB2SOUT LB2GO LB2CMPL) PROFILE
  VGET (LB2TST LB2OPT1) PROFILE
  VGET (LB2SL1 LB2SL2 LB2SL3 LB2SL4) PROFILE
  VGET (LB2JCL1 LB2JCL2 LB2JCL3 LB2JCL4) PROFILE
  VGET (LB2STEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LB2TST,NB,MSG=BGOPT01)
    VER (&LB2TST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LB2GO,NB,MSG=BGOPT01)
    VER (&LB2GO,LIST,Y,N,MSG=BGOPT02)
    VER (&LB2CMPL,NB,MSG=BGOPT01)
    VER (&LB2CMPL,LIST,1,MSG=BGOPT02)
    VPUT (LB2PROJ LB2LIB1 LB2LIB2 LB2LIB3 LB2LIB4 LB2TYPE LB2MBR) PROFILE
    VPUT (LB2ODSN) PROFILE
    VPUT (LB2LSTID LB2SOUT LB2GO LB2CMPL) PROFILE
    VPUT (LB2TST LB2OPT1) PROFILE
    VPUT (LB2SL1 LB2SL2 LB2SL3 LB2SL4) PROFILE
    VPUT (LB2JCL1 LB2JCL2 LB2JCL3 LB2JCL4) PROFILE
    VPUT (LB2STEP1) PROFILE

)END
./ ADD NAME=ISPOPT53
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT53                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background FORTRAN panel                                */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%------------------------  BACKGROUND Fortran Compiler  ------------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LB3PROJ +                                             {Z
+   LIBRARY ===>_LB3LIB1 + ===>_LB3LIB2 + ===>_LB3LIB3 + ===>_LB3LIB4 +
+   TYPE    ===>_LB3TYPE +
+   MEMBER  ===>_LB3MBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LB3ODSN                                         +
^*SYSLIBs are ignored...*
+Processor options:      +Term ==>_LB3TST+ QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LB3STEP1                                    _Z+ 1. FORTRAN IV G
+ List ID ==>_LB3LSTID%or+SYSOUT class ==>_Z+                 2. FORTRAN IV H
+ Parms   ==>_LB3OPT1                                 +       3. n/a
+^SYSLIB1 ==>_LB3SL1                                          +
+^SYSLIB2 ==>_LB3SL2                                          +
+^SYSLIB3 ==>_LB3SL3                                          +
+^SYSLIB4 ==>_LB3SL4                                          +
+ MoreJCL ==>_LB3JCL1                                                          +
+         ==>_LB3JCL2                                                          +
+         ==>_LB3JCL3                                                          +
+         ==>_LB3JCL4                                                          +
)INIT
  .HELP = HSPOPT53
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT53   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT53 /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LB3GO,LB3CMPL,LB3SOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LB3PROJ LB3LIB1 LB3LIB2 LB3LIB3 LB3LIB4 LB3TYPE LB3MBR) PROFILE
  VGET (LB3ODSN) PROFILE
  VGET (LB3LSTID LB3SOUT LB3GO LB3CMPL) PROFILE
  VGET (LB3TST LB3OPT1) PROFILE
  VGET (LB3SL1 LB3SL2 LB3SL3 LB3SL4) PROFILE
  VGET (LB3JCL1 LB3JCL2 LB3JCL3 LB3JCL4) PROFILE
  VGET (LB3STEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LB3TST,NB,MSG=BGOPT01)
    VER (&LB3TST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LB3GO,NB,MSG=BGOPT01)
    VER (&LB3GO,LIST,Y,N,MSG=BGOPT02)
    VER (&LB3CMPL,NB,MSG=BGOPT01)
    VER (&LB3CMPL,LIST,1,2,MSG=BGOPT02)
    VPUT (LB3PROJ LB3LIB1 LB3LIB2 LB3LIB3 LB3LIB4 LB3TYPE LB3MBR) PROFILE
    VPUT (LB3ODSN) PROFILE
    VPUT (LB3LSTID LB3SOUT LB3GO LB3CMPL) PROFILE
    VPUT (LB3TST LB3OPT1) PROFILE
    VPUT (LB3SL1 LB3SL2 LB3SL3 LB3SL4) PROFILE
    VPUT (LB3JCL1 LB3JCL2 LB3JCL3 LB3JCL4) PROFILE
    VPUT (LB3STEP1) PROFILE

)END
./ ADD NAME=ISPOPT54
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT54                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background MORTRAN Translate panel                      */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%----------------------  BACKGROUND Mortran Translator  ------------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LB4PROJ +                                             {Z
+   LIBRARY ===>_LB4LIB1 + ===>_LB4LIB2 + ===>_LB4LIB3 + ===>_LB4LIB4 +
+   TYPE    ===>_LB4TYPE +
+   MEMBER  ===>_LB4MBR  +
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LB4ODSN                                         +
^*SYSLIBs are ignored...*
+Processor options:      +Term ==>_LB4TST+ QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LB4STEP1                                    _Z+ 1. w FORTRAN IV G
+ List ID ==>_LB4LSTID%or+SYSOUT class ==>_Z+                 2. w FORTRAN IV H
+ Mortran ==>_LB4OPT1                                 +       3. n/a
+ Fortran ==>_LB4OPT2                                 +
+^SYSLIB1 ==>_LB4SL1                                          +
+^SYSLIB2 ==>_LB4SL2                                          +
+^SYSLIB3 ==>_LB4SL3                                          +
+^SYSLIB4 ==>_LB4SL4                                          +
+ MoreJCL ==>_LB4JCL1                                                          +
+         ==>_LB4JCL2                                                          +
+         ==>_LB4JCL3                                                          +
+         ==>_LB4JCL4                                                          +
)INIT
  .HELP = HSPOPT54
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT54   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT54 /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LB4GO,LB4CMPL,LB4SOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LB4PROJ LB4LIB1 LB4LIB2 LB4LIB3 LB4LIB4 LB4TYPE LB4MBR) PROFILE
  VGET (LB4ODSN) PROFILE
  VGET (LB4LSTID LB4SOUT LB4GO LB4CMPL) PROFILE
  VGET (LB4TST LB4OPT1) PROFILE
  VGET (LB4OPT2) PROFILE
  VGET (LB4SL1 LB4SL2 LB4SL3 LB4SL4) PROFILE
  VGET (LB4JCL1 LB4JCL2 LB4JCL3 LB4JCL4) PROFILE
  VGET (LB4STEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LB4TST,NB,MSG=BGOPT01)
    VER (&LB4TST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LB4GO,NB,MSG=BGOPT01)
    VER (&LB4GO,LIST,Y,N,MSG=BGOPT02)
    VER (&LB4CMPL,NB,MSG=BGOPT01)
    VER (&LB4CMPL,LIST,1,2,MSG=BGOPT02)
    VPUT (LB4PROJ LB4LIB1 LB4LIB2 LB4LIB3 LB4LIB4 LB4TYPE LB4MBR) PROFILE
    VPUT (LB4ODSN) PROFILE
    VPUT (LB4LSTID LB4SOUT LB4GO LB4CMPL) PROFILE
    VPUT (LB4TST LB4OPT1) PROFILE
    VPUT (LB4OPT2) PROFILE
    VPUT (LB4SL1 LB4SL2 LB4SL3 LB4SL4) PROFILE
    VPUT (LB4JCL1 LB4JCL2 LB4JCL3 LB4JCL4) PROFILE
    VPUT (LB4STEP1) PROFILE

)END
./ ADD NAME=ISPOPT55
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT55                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background PL/I panel                                   */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%------------------------  BACKGROUND PL/I F Compiler  -------------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LB5PROJ +                                             {Z
+   LIBRARY ===>_LB5LIB1 + ===>_LB5LIB2 + ===>_LB5LIB3 + ===>_LB5LIB4 +
+   TYPE    ===>_LB5TYPE +
+   MEMBER  ===>_LB5MBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LB5ODSN                                         +
+
+Processor options:      +Term ==>_LB1TST+ QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LB5STEP1                                    _Z+ 1. PL/1 F
+ List ID ==>_LB5LSTID%or+SYSOUT class ==>_Z+                 2. n/a
+ Parms   ==>_LB5OPT1                                 +       3. n/a
+ SYSLIB1 ==>_LB5SL1                                          +
+ SYSLIB2 ==>_LB5SL2                                          +
+ SYSLIB3 ==>_LB5SL3                                          +
+ SYSLIB4 ==>_LB5SL4                                          +
+ MoreJCL ==>_LB5JCL1                                                          +
+         ==>_LB5JCL2                                                          +
+         ==>_LB5JCL3                                                          +
+         ==>_LB5JCL4                                                          +
)INIT
  .HELP = HSPOPT55
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT55   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT55 /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LB5GO,LB5CMPL,LB5SOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LB5PROJ LB5LIB1 LB5LIB2 LB5LIB3 LB5LIB4 LB5TYPE LB5MBR) PROFILE
  VGET (LB5ODSN) PROFILE
  VGET (LB5LSTID LB5SOUT LB5GO LB5CMPL) PROFILE
  VGET (LB5TST LB5OPT1) PROFILE
  VGET (LB5SL1 LB5SL2 LB5SL3 LB5SL4) PROFILE
  VGET (LB5JCL1 LB5JCL2 LB5JCL3 LB5JCL4) PROFILE
  VGET (LB5STEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LB5TST,NB,MSG=BGOPT01)
    VER (&LB5TST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LB5GO,NB,MSG=BGOPT01)
    VER (&LB5GO,LIST,Y,N,MSG=BGOPT02)
    VER (&LB5CMPL,NB,MSG=BGOPT01)
    VER (&LB5CMPL,LIST,1,1,MSG=BGOPT02)
    VPUT (LB5PROJ LB5LIB1 LB5LIB2 LB5LIB3 LB5LIB4 LB5TYPE LB5MBR) PROFILE
    VPUT (LB5ODSN) PROFILE
    VPUT (LB5LSTID LB5SOUT LB5GO LB5CMPL) PROFILE
    VPUT (LB5TST LB5OPT1) PROFILE
    VPUT (LB5SL1 LB5SL2 LB5SL3 LB5SL4) PROFILE
    VPUT (LB5JCL1 LB5JCL2 LB5JCL3 LB5JCL4) PROFILE
    VPUT (LB5STEP1) PROFILE

)END
./ ADD NAME=ISPOPT56
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT56                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background Linkage Editor panel                         */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-------------------------  BACKGROUND Linkage Editor  -------------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LB6PROJ +                                             {Z
+   LIBRARY ===>_LB6LIB1 + ===>_LB6LIB2 + ===>_LB6LIB3 + ===>_LB6LIB4 +
+   TYPE    ===>_LB6TYPE +
+   MEMBER  ===>_LB6MBR  +
+
+Other Partitioned Data Set:
+   Data Set Name  . ._LB6ODSN                                         +
+
+Processor options:      +Term ==>_LB6TST+ QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LB6STEP1                                    _Z+ 1. Link-Edit
+ List ID ==>_LB6LSTID%or+SYSOUT class ==>_Z+                 2. n/a
+ Parms   ==>_LB6OPT1                                 +       3. n/a
+ SYSLIB1 ==>_LB6SL1                                          +
+ SYSLIB2 ==>_LB6SL2                                          +
+ SYSLIB3 ==>_LB6SL3                                          +
+ SYSLIB4 ==>_LB6SL4                                          +
+ MoreJCL ==>_LB6JCL1                                                          +
+         ==>_LB6JCL2                                                          +
+         ==>_LB6JCL3                                                          +
+         ==>_LB6JCL4                                                          +
)INIT
  .HELP = HSPOPT56
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT56   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT56 /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LB6GO,LB6CMPL,LB6SOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LB6PROJ LB6LIB1 LB6LIB2 LB6LIB3 LB6LIB4 LB6TYPE LB6MBR) PROFILE
  VGET (LB6ODSN) PROFILE
  VGET (LB6LSTID LB6SOUT LB6CMPL) PROFILE
  VGET (LB6TST LB6OPT1) PROFILE
  VGET (LB6SL1 LB6SL2 LB6SL3 LB6SL4) PROFILE
  VGET (LB6JCL1 LB6JCL2 LB6JCL3 LB6JCL4) PROFILE
  VGET (LB6STEP1) PROFILE
)REINIT
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
 &LBMEMBR = ''
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
    VER (&LB6TST,LIST,TERM,NOTERM,MSG=BGOPT02)
    VER (&LB6GO,LIST,N,N,MSG=BGOPT02)
    VER (&LB6CMPL,NB,MSG=BGOPT01)
    VER (&LB6CMPL,LIST,1,1,MSG=BGOPT02)
    VPUT (LB6PROJ LB6LIB1 LB6LIB2 LB6LIB3 LB6LIB4 LB6TYPE LB6MBR) PROFILE
    VPUT (LB6ODSN) PROFILE
    VPUT (LB6LSTID LB6SOUT LB6CMPL) PROFILE
    VPUT (LB6TST LB6OPT1) PROFILE
    VPUT (LB6SL1 LB6SL2 LB6SL3 LB6SL4) PROFILE
    VPUT (LB6JCL1 LB6JCL2 LB6JCL3 LB6JCL4) PROFILE
    VPUT (LB6STEP1) PROFILE

)END
./ ADD NAME=ISPOPT57
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT57                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background ASSIST panel                                 */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%---------------------  BACKGROUND ASSIST Interpreter  -------------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LB7PROJ +                                             {Z
+   LIBRARY ===>_LB7LIB1 + ===>_LB7LIB2 + ===>_LB7LIB3 + ===>_LB7LIB4 +
+   TYPE    ===>_LB7TYPE +
+   MEMBER  ===>_LB7MBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LB7ODSN                                         +
+
+Processor options:      +Term ==>_LB7TST+ QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LB7STEP1                                    _Z+ 1. ASSIST
+ List ID ==>_LB7lSTID%or+SYSOUT class ==>_Z+                 2. n/a
+ Parms   ==>_LB7OPT1                                 +       3. n/a
+ SYSLIB1 ==>_LB7SL1                                          +
+ SYSLIB2 ==>_LB7SL2                                          +
+ SYSLIB3 ==>_LB7SL3                                          +
+ SYSLIB4 ==>_LB7SL4                                          +
+ MoreJCL ==>_LB7JCL1                                                          +
+         ==>_LB7JCL2                                                          +
+         ==>_LB7JCL3                                                          +
+         ==>_LB7JCL4                                                          +
)INIT
  .HELP = HSPOPT57
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT57   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT57 /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LB7GO,LB7CMPL,LB7SOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LB7PROJ LB7LIB1 LB7LIB2 LB7LIB3 LB7LIB4 LB7TYPE LB7MBR) PROFILE
  VGET (LB7ODSN) PROFILE
  VGET (LB7LSTID LB7SOUT LB7GO LB7CMPL) PROFILE
  VGET (LB7TST LB7OPT1) PROFILE
  VGET (LB7SL1 LB7SL2 LB7SL3 LB7SL4) PROFILE
  VGET (LB7JCL1 LB7JCL2 LB7JCL3 LB7JCL4) PROFILE
  VGET (LB7STEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LB7TST,NB,MSG=BGOPT01)
    VER (&LB7TST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LB7GO,NB,MSG=BGOPT01)
    VER (&LB7GO,LIST,Y,Y,MSG=BGOPT02)
    VER (&LB7CMPL,NB,MSG=BGOPT01)
    VER (&LB7CMPL,LIST,1,1,MSG=BGOPT02)
    VPUT (LB7PROJ LB7LIB1 LB7LIB2 LB7LIB3 LB7LIB4 LB7TYPE LB7MBR) PROFILE
    VPUT (LB7ODSN) PROFILE
    VPUT (LB7LSTID LB7SOUT LB7GO LB7CMPL) PROFILE
    VPUT (LB7TST LB7OPT1) PROFILE
    VPUT (LB7SL1 LB7SL2 LB7SL3 LB7SL4) PROFILE
    VPUT (LB7JCL1 LB7JCL2 LB7JCL3 LB7JCL4) PROFILE
    VPUT (LB7STEP1) PROFILE

)END
./ ADD NAME=ISPOPT58
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT58                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background ALGOL panel                                  */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%------------------------  BACKGROUND ALGOL Compiler  --------------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LB8PROJ +                                             {Z
+   LIBRARY ===>_LB8LIB1 + ===>_LB8LIB2 + ===>_LB8LIB3 + ===>_LB8LIB4 +
+   TYPE    ===>_LB8TYPE +
+   MEMBER  ===>_LB8MBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LB8ODSN                                         +
^SYSLIBs are ignored...*
+Processor options:      +Term ==>_LB8TST+ QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LB8STEP1                                    _Z+ 1. ALGOL F
+ List ID ==>_LB8LSTID%or+SYSOUT class ==>_Z+                 2. ALGOL 68c
+ Parms   ==>_LB8OPT1                                 +       3. n/a
+^SYSLIB1 ==>_LB8SL1                                          +
+^SYSLIB2 ==>_LB8SL2                                          +
+^SYSLIB3 ==>_LB8SL3                                          +
+^SYSLIB4 ==>_LB8SL4                                          +
+ MoreJCL ==>_LB8JCL1                                                          +
+         ==>_LB8JCL2                                                          +
+         ==>_LB8JCL3                                                          +
+         ==>_LB8JCL4                                                          +
)INIT
  .HELP = HSPOPT58
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT58   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT58 /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LB8GO,LB8CMPL,LB8SOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LB8PROJ LB8LIB1 LB8LIB2 LB8LIB3 LB8LIB4 LB8TYPE LB8MBR) PROFILE
  VGET (LB8ODSN) PROFILE
  VGET (LB8LSTID LB8SOUT LB8GO LB8CMPL) PROFILE
  VGET (LB8TST LB8OPT1) PROFILE
  VGET (LB8SL1 LB8SL2 LB8SL3 LB8SL4) PROFILE
  VGET (LB8JCL1 LB8JCL2 LB8JCL3 LB8JCL4) PROFILE
  VGET (LB8STEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LB8TST,NB,MSG=BGOPT01)
    VER (&LB8TST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LB8GO,NB,MSG=BGOPT01)
    VER (&LB8GO,LIST,Y,N,MSG=BGOPT02)
    VER (&LB8CMPL,NB,MSG=BGOPT01)
    VER (&LB8CMPL,LIST,1,2,MSG=BGOPT02)
    VPUT (LB8PROJ LB8LIB1 LB8LIB2 LB8LIB3 LB8LIB4 LB8TYPE LB8MBR) PROFILE
    VPUT (LB8ODSN) PROFILE
    VPUT (LB8LSTID LB8SOUT LB8GO LB8CMPL) PROFILE
    VPUT (LB8TST LB8OPT1) PROFILE
    VPUT (LB8SL1 LB8SL2 LB8SL3 LB8SL4) PROFILE
    VPUT (LB8JCL1 LB8JCL2 LB8JCL3 LB8JCL4) PROFILE
    VPUT (LB8STEP1) PROFILE

)END
./ ADD NAME=ISPOPT59
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT59                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background PASCAL panel                                 */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%------------------------  BACKGROUND PASCAL Compiler  -------------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LB9PROJ +                                             {Z
+   LIBRARY ===>_LB9LIB1 + ===>_LB9LIB2 + ===>_LB9LIB3 + ===>_LB9LIB4 +
+   TYPE    ===>_LB9TYPE +
+   MEMBER  ===>_LB9MBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LB9ODSN                                         +
^*Quik GO, STEPLIB, List ID and SYSLIBs are ignored...*
+Processor options:      +Term ==>_LB9TST+^QuikGO ==>_Z+  +Process-------------
+^STEPLIB ==>_LB9STEP1                                    _Z+ 1. PASCAL 8000-go
+^List ID ==>_LB9LSTID%or+SYSOUT class ==>_Z+                 2. Stonybrook-go
+ Parms   ==>_LB9OPT1                                 +       3. Stanford-go
+^SYSLIB1 ==>_LB9SL1                                          +
+^SYSLIB2 ==>_LB9SL2                                          +
+^SYSLIB3 ==>_LB9SL3                                          +
+^SYSLIB4 ==>_LB9SL4                                          +
+ MoreJCL ==>_LB9JCL1                                                          +
+         ==>_LB9JCL2                                                          +
+         ==>_LB9JCL3                                                          +
+         ==>_LB9JCL4                                                          +
)INIT
  .HELP = HSPOPT59
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT59   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT59 /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LB9GO,LB9CMPL,LB9SOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LB9PROJ LB9LIB1 LB9LIB2 LB9LIB3 LB9LIB4 LB9TYPE LB9MBR) PROFILE
  VGET (LB9ODSN) PROFILE
  VGET (LB9LSTID LB9SOUT LB9GO LB9CMPL) PROFILE
  VGET (LB9TST LB9OPT1) PROFILE
  VGET (LB9SL1 LB9SL2 LB9SL3 LB9SL4) PROFILE
  VGET (LB9JCL1 LB9JCL2 LB9JCL3 LB9JCL4) PROFILE
  VGET (LB9STEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LB9TST,NB,MSG=BGOPT01)
    VER (&LB9TST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LB9GO,NB,MSG=BGOPT01)
    VER (&LB9GO,LIST,Y,N,MSG=BGOPT02)
    VER (&LB9CMPL,NB,MSG=BGOPT01)
    VER (&LB9CMPL,LIST,1,2,3,MSG=BGOPT02)
    VPUT (LB9PROJ LB9LIB1 LB9LIB2 LB9LIB3 LB9LIB4 LB9TYPE LB9MBR) PROFILE
    VPUT (LB9ODSN) PROFILE
    VPUT (LB9LSTID LB9SOUT LB9GO LB9CMPL) PROFILE
    VPUT (LB9TST LB9OPT1) PROFILE
    VPUT (LB9SL1 LB9SL2 LB9SL3 LB9SL4) PROFILE
    VPUT (LB9JCL1 LB9JCL2 LB9JCL3 LB9JCL4) PROFILE
    VPUT (LB9STEP1) PROFILE

)END
./ ADD NAME=ISPOPT5A
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT5A                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background RPG panel                                    */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%------------------------  BACKGROUND RPG Compiler  ----------------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LBAPROJ +                                             {Z
+   LIBRARY ===>_LBALIB1 + ===>_LBALIB2 + ===>_LBALIB3 + ===>_LBALIB4 +
+   TYPE    ===>_LBATYPE +
+   MEMBER  ===>_LBAMBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LBAODSN                                         +
^*SYSLIBs are ignored...*
+Processor options:      +Term ==>_LBATST+ QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LBASTEP1                                    _Z+ 1. RPG E MVT
+ List ID ==>_LBALSTID%or+SYSOUT class ==>_Z+                 2. n/a
+ Parms   ==>_LBAOPT1                                 +       3. n/a
+^SYSLIB1 ==>_LBASL1                                          +
+^SYSLIB2 ==>_LBASL2                                          +
+^SYSLIB3 ==>_LBASL3                                          +
+^SYSLIB4 ==>_LBASL4                                          +
+ MoreJCL ==>_LBAJCL1                                                          +
+         ==>_LBAJCL2                                                          +
+         ==>_LBAJCL3                                                          +
+         ==>_LBAJCL4                                                          +
)INIT
  .HELP = HSPOPT59
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT59   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT59 /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LBAGO,LBACMPL,LBASOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LBAPROJ LBALIB1 LBALIB2 LBALIB3 LBALIB4 LBATYPE LBAMBR) PROFILE
  VGET (LBAODSN) PROFILE
  VGET (LBALSTID LBASOUT LBAGO LBACMPL) PROFILE
  VGET (LBATST LBAOPT1) PROFILE
  VGET (LBASL1 LBASL2 LBASL3 LBASL4) PROFILE
  VGET (LBAJCL1 LBAJCL2 LBAJCL3 LBAJCL4) PROFILE
  VGET (LBASTEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LBATST,NB,MSG=BGOPT01)
    VER (&LBATST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LBAGO,NB,MSG=BGOPT01)
    VER (&LBAGO,LIST,Y,N,MSG=BGOPT02)
    VER (&LBACMPL,NB,MSG=BGOPT01)
    VER (&LBACMPL,LIST,1,1,MSG=BGOPT02)
    VPUT (LBAPROJ LBALIB1 LBALIB2 LBALIB3 LBALIB4 LBATYPE LBAMBR) PROFILE
    VPUT (LBAODSN) PROFILE
    VPUT (LBALSTID LBASOUT LBAGO LBACMPL) PROFILE
    VPUT (LBATST LBAOPT1) PROFILE
    VPUT (LBASL1 LBASL2 LBASL3 LBASL4) PROFILE
    VPUT (LBAJCL1 LBAJCL2 LBAJCL3 LBAJCL4) PROFILE
    VPUT (LBASTEP1) PROFILE

)END
./ ADD NAME=ISPOPT5B
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT5B                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background SIMULA panel                                 */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%------------------------  BACKGROUND SIMULA Compiler  -------------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LBBPROJ +                                             {Z
+   LIBRARY ===>_LBBLIB1 + ===>_LBBLIB2 + ===>_LBBLIB3 + ===>_LBBLIB4 +
+   TYPE    ===>_LBBTYPE +
+   MEMBER  ===>_LBBMBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LBBODSN                                         +
^*Term and SYSLIBs are ignored...*
+Processor options:      ^Term ==>_LBBTST+ QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LBBSTEP1                                    _Z+ 1. SIMULA
+ List ID ==>_LBBLSTID%or+SYSOUT class ==>_Z+                 2. n/a
+ Parms   ==>_LBBOPT1                                 +       3. n/a
+^SYSLIB1 ==>_LBBSL1                                          +
+^SYSLIB2 ==>_LBBSL2                                          +
+^SYSLIB3 ==>_LBBSL3                                          +
+^SYSLIB4 ==>_LBBSL4                                          +
+ MoreJCL ==>_LBBJCL1                                                          +
+         ==>_LBBJCL2                                                          +
+         ==>_LBBJCL3                                                          +
+         ==>_LBBJCL4                                                          +
)INIT
  .HELP = HSPOPT5B
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT5B   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT5B /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LBBGO,LBBCMPL,LBBSOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LBBPROJ LBBLIB1 LBBLIB2 LBBLIB3 LBBLIB4 LBBTYPE LBBMBR) PROFILE
  VGET (LBBODSN) PROFILE
  VGET (LBBLSTID LBBSOUT LBBGO LBBCMPL) PROFILE
  VGET (LBBTST LBBOPT1) PROFILE
  VGET (LBBSL1 LBBSL2 LBBSL3 LBBSL4) PROFILE
  VGET (LBBJCL1 LBBJCL2 LBBJCL3 LBBJCL4) PROFILE
  VGET (LBBSTEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LBBTST,NB,MSG=BGOPT01)
    VER (&LBBTST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LBBGO,NB,MSG=BGOPT01)
    VER (&LBBGO,LIST,Y,N,MSG=BGOPT02)
    VER (&LBBCMPL,NB,MSG=BGOPT01)
    VER (&LBBCMPL,LIST,1,1,MSG=BGOPT02)
    VPUT (LBBPROJ LBBLIB1 LBBLIB2 LBBLIB3 LBBLIB4 LBBTYPE LBBMBR) PROFILE
    VPUT (LBBODSN) PROFILE
    VPUT (LBBLSTID LBBSOUT LBBGO LBBCMPL) PROFILE
    VPUT (LBBTST LBBOPT1) PROFILE
    VPUT (LBBSL1 LBBSL2 LBBSL3 LBBSL4) PROFILE
    VPUT (LBBJCL1 LBBJCL2 LBBJCL3 LBBJCL4) PROFILE
    VPUT (LBBSTEP1) PROFILE

)END
./ ADD NAME=ISPOPT5C
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT5C                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background SNOBOL panel                                 */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%------------------------  BACKGROUND SNOBOL Interpreter -----------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LBCPROJ +                                             {Z
+   LIBRARY ===>_LBCLIB1 + ===>_LBCLIB2 + ===>_LBCLIB3 + ===>_LBCLIB4 +
+   TYPE    ===>_LBCTYPE +
+   MEMBER  ===>_LBCMBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LBCODSN                                         +
^*Term, Quik GO, List ID and SYSLIBs are ignored...*
+Processor options:      ^Term ==>_LBCTST+^QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LBCSTEP1                                    _Z+ 1. SNOBOL
+^List ID ==>_LBCLSTID%or+SYSOUT class ==>_Z+                 2. n/a
+ Parms   ==>_LBCOPT1                                 +       3. n/a
+^SYSLIB1 ==>_LBCSL1                                          +
+^SYSLIB2 ==>_LBCSL2                                          +
+^SYSLIB3 ==>_LBCSL3                                          +
+^SYSLIB4 ==>_LBCSL4                                          +
+ MoreJCL ==>_LBCJCL1                                                          +
+         ==>_LBCJCL2                                                          +
+         ==>_LBCJCL3                                                          +
+         ==>_LBCJCL4                                                          +
)INIT
  .HELP = HSPOPT5C
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT5C   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT5C /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LBCGO,LBCCMPL,LBCSOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LBCPROJ LBCLIB1 LBCLIB2 LBCLIB3 LBCLIB4 LBCTYPE LBCMBR) PROFILE
  VGET (LBCODSN) PROFILE
  VGET (LBCLSTID LBCSOUT LBCGO LBCCMPL) PROFILE
  VGET (LBCTST LBCOPT1) PROFILE
  VGET (LBCSL1 LBCSL2 LBCSL3 LBCSL4) PROFILE
  VGET (LBCJCL1 LBCJCL2 LBCJCL3 LBCJCL4) PROFILE
  VGET (LBCSTEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LBCTST,NB,MSG=BGOPT01)
    VER (&LBCTST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LBCGO,NB,MSG=BGOPT01)
    VER (&LBCGO,LIST,Y,Y,MSG=BGOPT02)
    VER (&LBCCMPL,NB,MSG=BGOPT01)
    VER (&LBCCMPL,LIST,1,1,MSG=BGOPT02)
    VPUT (LBCPROJ LBCLIB1 LBCLIB2 LBCLIB3 LBCLIB4 LBCTYPE LBCMBR) PROFILE
    VPUT (LBCODSN) PROFILE
    VPUT (LBCLSTID LBCSOUT LBCGO LBCCMPL) PROFILE
    VPUT (LBCTST LBCOPT1) PROFILE
    VPUT (LBCSL1 LBCSL2 LBCSL3 LBCSL4) PROFILE
    VPUT (LBCJCL1 LBCJCL2 LBCJCL3 LBCJCL4) PROFILE
    VPUT (LBCSTEP1) PROFILE

)END
./ ADD NAME=ISPOPT5D
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT5D                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background WATFIV panel                                 */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%------------------------  BACKGROUND WATFIV Interpreter -----------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LBDPROJ +                                             {Z
+   LIBRARY ===>_LBDLIB1 + ===>_LBDLIB2 + ===>_LBDLIB3 + ===>_LBDLIB4 +
+   TYPE    ===>_LBDTYPE +
+   MEMBER  ===>_LBDMBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LBDODSN                                         +
^*Term, Quik Go, List ID and SYSLIBs are ignored...*
+Processor options:      ^Term ==>_LBDTST+^QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LBDSTEP1                                    _Z+ 1. WATFIV
+^List ID ==>_LBDLSTID%or+SYSOUT class ==>_Z+                 2. n/a
+ Parms   ==>_LBDOPT1                                 +       3. n/a
+^SYSLIB1 ==>_LBDSL1                                          +
+^SYSLIB2 ==>_LBDSL2                                          +
+^SYSLIB3 ==>_LBDSL3                                          +
+^SYSLIB4 ==>_LBDSL4                                          +
+ MoreJCL ==>_LBDJCL1                                                          +
+         ==>_LBDJCL2                                                          +
+         ==>_LBDJCL3                                                          +
+         ==>_LBDJCL4                                                          +
)INIT
  .HELP = HSPOPT5D
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT5D   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT5D /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LBDGO,LBDCMPL,LBDSOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LBDPROJ LBDLIB1 LBDLIB2 LBDLIB3 LBDLIB4 LBDTYPE LBDMBR) PROFILE
  VGET (LBDODSN) PROFILE
  VGET (LBDLSTID LBDSOUT LBDGO LBDCMPL) PROFILE
  VGET (LBDTST LBDOPT1) PROFILE
  VGET (LBDSL1 LBDSL2 LBDSL3 LBDSL4) PROFILE
  VGET (LBDJCL1 LBDJCL2 LBDJCL3 LBDJCL4) PROFILE
  VGET (LBDSTEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LBDTST,NB,MSG=BGOPT01)
    VER (&LBDTST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LBDGO,NB,MSG=BGOPT01)
    VER (&LBDGO,LIST,Y,Y,MSG=BGOPT02)
    VER (&LBDCMPL,NB,MSG=BGOPT01)
    VER (&LBDCMPL,LIST,1,1,MSG=BGOPT02)
    VPUT (LBDPROJ LBDLIB1 LBDLIB2 LBDLIB3 LBDLIB4 LBDTYPE LBDMBR) PROFILE
    VPUT (LBDODSN) PROFILE
    VPUT (LBDLSTID LBDSOUT LBDGO LBDCMPL) PROFILE
    VPUT (LBDTST LBDOPT1) PROFILE
    VPUT (LBDSL1 LBDSL2 LBDSL3 LBDSL4) PROFILE
    VPUT (LBDJCL1 LBDJCL2 LBDJCL3 LBDJCL4) PROFILE
    VPUT (LBDSTEP1) PROFILE

)END
./ ADD NAME=ISPOPT5E
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT5E                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background XPL panel                                    */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%------------------------  BACKGROUND XPL Compiler -----------------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LBEPROJ +                                             {Z
+   LIBRARY ===>_LBELIB1 + ===>_LBELIB2 + ===>_LBELIB3 + ===>_LBELIB4 +
+   TYPE    ===>_LBETYPE +
+   MEMBER  ===>_LBEMBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LBEODSN                                         +
^*Term, Quik GO and SYSLIBs are ignored...*
+Processor options:      ^Term ==>_LBETST+^QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LBESTEP1                                    _Z+ 1. XPL Analyzer
+ List ID ==>_LBELSTID%or+SYSOUT class ==>_Z+                 2. XPL Compile
+ Parms   ==>_LBEOPT1                                 +       3. XPL Compile-GO
+^SYSLIB1 ==>_LBESL1                                          +
+^SYSLIB2 ==>_LBESL2                                          +
+^SYSLIB3 ==>_LBESL3                                          +
+^SYSLIB4 ==>_LBESL4                                          +
+ MoreJCL ==>_LBEJCL1                                                          +
+         ==>_LBEJCL2                                                          +
+         ==>_LBEJCL3                                                          +
+         ==>_LBEJCL4                                                          +
)INIT
  .HELP = HSPOPT5E
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT5E   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT5E /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LBEGO,LBECMPL,LBESOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LBEPROJ LBELIB1 LBELIB2 LBELIB3 LBELIB4 LBETYPE LBEMBR) PROFILE
  VGET (LBEODSN) PROFILE
  VGET (LBELSTID LBESOUT LBEGO LBECMPL) PROFILE
  VGET (LBETST LBEOPT1) PROFILE
  VGET (LBESL1 LBESL2 LBESL3 LBESL4) PROFILE
  VGET (LBEJCL1 LBEJCL2 LBEJCL3 LBEJCL4) PROFILE
  VGET (LBESTEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LBETST,NB,MSG=BGOPT01)
    VER (&LBETST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LBEGO,NB,MSG=BGOPT01)
    VER (&LBEGO,LIST,Y,N,MSG=BGOPT02)
    VER (&LBECMPL,NB,MSG=BGOPT01)
    VER (&LBECMPL,LIST,1,2,3,MSG=BGOPT02)
    VPUT (LBEPROJ LBELIB1 LBELIB2 LBELIB3 LBELIB4 LBETYPE LBEMBR) PROFILE
    VPUT (LBEODSN) PROFILE
    VPUT (LBELSTID LBESOUT LBEGO LBECMPL) PROFILE
    VPUT (LBETST LBEOPT1) PROFILE
    VPUT (LBESL1 LBESL2 LBESL3 LBESL4) PROFILE
    VPUT (LBEJCL1 LBEJCL2 LBEJCL3 LBEJCL4) PROFILE
    VPUT (LBESTEP1) PROFILE

)END
./ ADD NAME=ISPOPT5F
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT5F                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background SPITBOL panel                                */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%---------------------  BACKGROUND SPITBOL 370 Interpreter  --------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LBFPROJ +                                             {Z
+   LIBRARY ===>_LBFLIB1 + ===>_LBFLIB2 + ===>_LBFLIB3 + ===>_LBFLIB4 +
+   TYPE    ===>_LBFTYPE +
+   MEMBER  ===>_LBFMBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LBFODSN                                         +
^*Term, Quik GO and SYSLIBs are ignored...*
+Processor options:      ^Term ==>_LBFTST+^QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LBFSTEP1                                    _Z+ 1. SPITBOL 370
+ List ID ==>_LBFLSTID%or+SYSOUT class ==>_Z+                 2. n/a
+ Parms   ==>_LBFOPT1                                 +       3. n/a
+^SYSLIB1 ==>_LBFSL1                                          +
+^SYSLIB2 ==>_LBFSL2                                          +
+^SYSLIB3 ==>_LBFSL3                                          +
+^SYSLIB4 ==>_LBFSL4                                          +
+ MoreJCL ==>_LBFJCL1                                                          +
+         ==>_LBFJCL2                                                          +
+         ==>_LBFJCL3                                                          +
+         ==>_LBFJCL4                                                          +
)INIT
  .HELP = HSPOPT5F
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT5F   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT5F /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LBFGO,LBFCMPL,LBFSOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LBFPROJ LBFLIB1 LBFLIB2 LBFLIB3 LBFLIB4 LBFTYPE LBFMBR) PROFILE
  VGET (LBFODSN) PROFILE
  VGET (LBFLSTID LBFSOUT LBFGO LBFCMPL) PROFILE
  VGET (LBFTST LBFOPT1) PROFILE
  VGET (LBFSL1 LBFSL2 LBFSL3 LBFSL4) PROFILE
  VGET (LBFJCL1 LBFJCL2 LBFJCL3 LBFJCL4) PROFILE
  VGET (LBFSTEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LBFTST,NB,MSG=BGOPT01)
    VER (&LBFTST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LBFGO,NB,MSG=BGOPT01)
    VER (&LBFGO,LIST,Y,N,MSG=BGOPT02)
    VER (&LBFCMPL,NB,MSG=BGOPT01)
    VER (&LBFCMPL,LIST,1,1,MSG=BGOPT02)
    VPUT (LBFPROJ LBFLIB1 LBFLIB2 LBFLIB3 LBFLIB4 LBFTYPE LBFMBR) PROFILE
    VPUT (LBFODSN) PROFILE
    VPUT (LBFLSTID LBFSOUT LBFGO LBFCMPL) PROFILE
    VPUT (LBFTST LBFOPT1) PROFILE
    VPUT (LBFSL1 LBFSL2 LBFSL3 LBFSL4) PROFILE
    VPUT (LBFJCL1 LBFJCL2 LBFJCL3 LBFJCL4) PROFILE
    VPUT (LBFSTEP1) PROFILE

)END
./ ADD NAME=ISPOPT5G
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT5G                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background GCC panel                                    */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%------------------------  BACKGROUND GCC Compiler -----------------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LBGPROJ +                                             {Z
+   LIBRARY ===>_LBGLIB1 + ===>_LBGLIB2 + ===>_LBGLIB3 + ===>_LBGLIB4 +
+   TYPE    ===>_LBGTYPE +
+   MEMBER  ===>_LBGMBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LBGODSN                                         +
^*Term, List ID, Parms and SYSLIBs are ignored...*
+Processor options:      ^Term ==>_LBGTST+ QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LBGSTEP1                                    _Z+ 1. GCC
+^List ID ==>_LBGLSTID%or+SYSOUT class ==>_Z+                 2. n/a
+^Parms   ==>_LBGOPT1                                 +       3. n/a
+^SYSLIB1 ==>_LBGSL1                                          +
+^SYSLIB2 ==>_LBGSL2                                          +
+^SYSLIB3 ==>_LBGSL3                                          +
+^SYSLIB4 ==>_LBGSL4                                          +
+ MoreJCL ==>_LBGJCL1                                                          +
+         ==>_LBGJCL2                                                          +
+         ==>_LBGJCL3                                                          +
+         ==>_LBGJCL4                                                          +
)INIT
  .HELP = HSPOPT5G
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT5G   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT5G /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LBGGO,LBGCMPL,LBGSOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LBGPROJ LBGLIB1 LBGLIB2 LBGLIB3 LBGLIB4 LBGTYPE LBGMBR) PROFILE
  VGET (LBGODSN) PROFILE
  VGET (LBGLSTID LBGSOUT LBGGO LBGCMPL) PROFILE
  VGET (LBGTST LBGOPT1) PROFILE
  VGET (LBGSL1 LBGSL2 LBGSL3 LBGSL4) PROFILE
  VGET (LBGJCL1 LBGJCL2 LBGJCL3 LBGJCL4) PROFILE
  VGET (LBGSTEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LBGTST,NB,MSG=BGOPT01)
    VER (&LBGTST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LBGGO,NB,MSG=BGOPT01)
    VER (&LBGGO,LIST,Y,N,MSG=BGOPT02)
    VER (&LBGCMPL,NB,MSG=BGOPT01)
    VER (&LBGCMPL,LIST,1,1,MSG=BGOPT02)
    VPUT (LBGPROJ LBGLIB1 LBGLIB2 LBGLIB3 LBGLIB4 LBGTYPE LBGMBR) PROFILE
    VPUT (LBGODSN) PROFILE
    VPUT (LBGLSTID LBGSOUT LBGGO LBGCMPL) PROFILE
    VPUT (LBGTST LBGOPT1) PROFILE
    VPUT (LBGSL1 LBGSL2 LBGSL3 LBGSL4) PROFILE
    VPUT (LBGJCL1 LBGJCL2 LBGJCL3 LBGJCL4) PROFILE
    VPUT (LBGSTEP1) PROFILE

)END
./ ADD NAME=ISPOPT5H
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT5H                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background PL360 panel                                  */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%------------------------  BACKGROUND PL360 Compiler  --------------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LBHPROJ +                                             {Z
+   LIBRARY ===>_LBHLIB1 + ===>_LBHLIB2 + ===>_LBHLIB3 + ===>_LBHLIB4 +
+   TYPE    ===>_LBHTYPE +
+   MEMBER  ===>_LBHMBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LBHODSN                                         +
^*Term and SYSLIBs are ignored...*
+Processor options:      ^Term ==>_LBHTST+ QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LBHSTEP1                                    _Z+ 1. PL360
+ List ID ==>_LBHLSTID%or+SYSOUT class ==>_Z+                 2. n/a
+ Parms   ==>_LBHOPT1                                 +       3. n/a
+^SYSLIB1 ==>_LBHSL1                                          +
+^SYSLIB2 ==>_LBHSL2                                          +
+^SYSLIB3 ==>_LBHSL3                                          +
+^SYSLIB4 ==>_LBHSL4                                          +
+ MoreJCL ==>_LBHJCL1                                                          +
+         ==>_LBHJCL2                                                          +
+         ==>_LBHJCL3                                                          +
+         ==>_LBHJCL4                                                          +
)INIT
  .HELP = HSPOPT5H
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT5H   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT5H /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LBHGO,LBHCMPL,LBHSOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LBHPROJ LBHLIB1 LBHLIB2 LBHLIB3 LBHLIB4 LBHTYPE LBHMBR) PROFILE
  VGET (LBHODSN) PROFILE
  VGET (LBHLSTID LBHSOUT LBHGO LBHCMPL) PROFILE
  VGET (LBHTST LBHOPT1) PROFILE
  VGET (LBHSL1 LBHSL2 LBHSL3 LBHSL4) PROFILE
  VGET (LBHJCL1 LBHJCL2 LBHJCL3 LBHJCL4) PROFILE
  VGET (LBHSTEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LBHTST,NB,MSG=BGOPT01)
    VER (&LBHTST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LBHGO,NB,MSG=BGOPT01)
    VER (&LBHGO,LIST,Y,N,MSG=BGOPT02)
    VER (&LBHCMPL,NB,MSG=BGOPT01)
    VER (&LBHCMPL,LIST,1,1,MSG=BGOPT02)
    VPUT (LBHPROJ LBHLIB1 LBHLIB2 LBHLIB3 LBHLIB4 LBHTYPE LBHMBR) PROFILE
    VPUT (LBHODSN) PROFILE
    VPUT (LBHLSTID LBHSOUT LBHGO LBHCMPL) PROFILE
    VPUT (LBHTST LBHOPT1) PROFILE
    VPUT (LBHSL1 LBHSL2 LBHSL3 LBHSL4) PROFILE
    VPUT (LBHJCL1 LBHJCL2 LBHJCL3 LBHJCL4) PROFILE
    VPUT (LBHSTEP1) PROFILE

)END
./ ADD NAME=ISPOPT5I
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT5I                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background BASIC 360 Panel                              */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%---------------------  BACKGROUND BASIC 360 Interpreter  ----------------------
%COMMAND ===>_ZCMD                                             +
%
+ISPF Library:                                                         {Z
+   PROJECT ===>_LBIPROJ +                                             {Z
+   LIBRARY ===>_LBILIB1 + ===>_LBILIB2 + ===>_LBILIB3 + ===>_LBILIB4 +
+   TYPE    ===>_LBITYPE +
+   MEMBER  ===>_LBIMBR  +
+
+Other Partitioned or Sequential Data Set:
+   Data Set Name  . ._LBIODSN                                         +
^*Term, Quik GO and SYSLIBs are ignored...*
+Processor options:      ^Term ==>_LBITST+^QuikGO ==>_Z+  +Process-------------
+ STEPLIB ==>_LBISTEP1                                    _Z+ 1. BASIC 360
+ List ID ==>_LBILSTID%or+SYSOUT class ==>_Z+                 2. n/a
+ Parms   ==>_LBIOPT1                                 +       3. n/a
+^SYSLIB1 ==>_LBISL1                                          +
+^SYSLIB2 ==>_LBISL2                                          +
+^SYSLIB3 ==>_LBISL3                                          +
+^SYSLIB4 ==>_LBISL4                                          +
+ MoreJCL ==>_LBIJCL1                                                          +
+         ==>_LBIJCL2                                                          +
+         ==>_LBIJCL3                                                          +
+         ==>_LBIJCL4                                                          +
)INIT
  .HELP = HSPOPT5I
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT5I   /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT5I /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  .ZVARS = (ZUSER,ZPANELID,LBIGO,LBICMPL,LBISOUT)
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LBIPROJ LBILIB1 LBILIB2 LBILIB3 LBILIB4 LBITYPE LBIMBR) PROFILE
  VGET (LBIODSN) PROFILE
  VGET (LBILSTID LBISOUT LBIGO LBICMPL) PROFILE
  VGET (LBITST LBIOPT1) PROFILE
  VGET (LBISL1 LBISL2 LBISL3 LBISL4) PROFILE
  VGET (LBIJCL1 LBIJCL2 LBIJCL3 LBIJCL4) PROFILE
  VGET (LBISTEP1) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
 /* VER (&LBITST,NB,MSG=BGOPT01)
    VER (&LBITST,LIST,NOTERM,MSG=BGOPT02)
 /* VER (&LBIGO,NB,MSG=BGOPT01)
    VER (&LBIGO,LIST,Y,N,MSG=BGOPT02)
    VER (&LBICMPL,NB,MSG=BGOPT01)
    VER (&LBICMPL,LIST,1,2,3,MSG=BGOPT02)
    VPUT (LBIPROJ LBILIB1 LBILIB2 LBILIB3 LBILIB4 LBITYPE LBIMBR) PROFILE
    VPUT (LBIODSN) PROFILE
    VPUT (LBILSTID LBISOUT LBIGO LBICMPL) PROFILE
    VPUT (LBITST LBIOPT1) PROFILE
    VPUT (LBISL1 LBISL2 LBISL3 LBISL4) PROFILE
    VPUT (LBIJCL1 LBIJCL2 LBIJCL3 LBIJCL4) PROFILE
    VPUT (LBISTEP1) PROFILE

)END
./ ADD NAME=ISPOPT5
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT5                                                */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background Selection panel (initial JCL entry)          */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-----------------------  Background Selection Menu  ---------------------------
%SELECT OPTION ===>_ZCMD
+
+   1 -^Assembler         +A -^RPG               +J -^TBD              {Z
+   2 -^OS/VS Cobol       +B -^Simula            +K -^TBD              {Z
+   3 -^Fortran IV        +C -^SNOBOL            +L -^TBD
+   4 -^Mortran           +D -^WATFIV            +   ^
+   5 -^PL/I              +E -^XPL               +   ^
+   6 -^Linkage Editor    +F -^SPITBOL           +   ^
+   7 -^ASSIST            +G -^GCC               +   ^
+   8 -^ALGOL             +H -^PL360             +   ^
+   9 -^Pascal            +I -^BASIC 360         +   ^
+
+
+
+Source DATA online ===>_Z+  (Y/N)     Delete SUBMIT JCL ===>_Z+  (Y/N)
+USING ===>%&TDSN
+JOB STATEMENT INFORMATION:  (Verify before proceeding)
+  ===>_JCL1                                                                   +
+  ===>_JCL2                                                                   +
+  ===>_JCL3                                                                   +
+  ===>_JCL4                                                                   +
+
+
)INIT
  .CURSOR = ZCMD
  &Z = ' '
  .HELP = HSPOPT5
  .ZVARS = (ZUSER,ZPANELID,BGSO,BGDJ)
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT5    /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT5  /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (JCL1 JCL2 JCL3 JCL4) PROFILE
  VGET (BGSO BGDJ) PROFILE
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  IF (&KEYPRESS NE PF03)
    VER (&BGPO,NB,MSG=BGOPT01)
    VER (&BGSO,NB,MSG=BGOPT01)
    VER (&BGSO,LIST,Y,N,MSG=BGOPT02)
    VER (&BGDJ,NB,MSG=BGOPT01)
    VER (&BGDJ,LIST,Y,N,MSG=BGOPT02)
    VER (&JCL1,NB,MSG=BGOPT01)
    VPUT (JCL1 JCL2 JCL3 JCL4) PROFILE
    VPUT (BGSO BGDJ) PROFILE

)END
./ ADD NAME=ISPOPT5X
/********************************************************************/
/*                                                                  */
/*    PANEL: ISPOPT5X                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/ISPOPT5-in-MVS38J              */
/*         Copyright (C) 2021  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Background Selection panel                              */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/31/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/* 05/13/2018 0.1.00   Larry Belmontes Jr.                          */
/*                     - Initial protyping with initial version     */
/*                       of ISPF from Wally Mclaughlin              */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ^ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ~ TYPE(INPUT)  INTENS(HIGH) JUST(LEFT)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 ] TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-----------------------  BACKGROUND SELECTION MENU  ---------------------------
%SELECT OPTION ===>_ZCMD
%
+   1 -^Assembler         +A -^RPG               +   ^                 {Z
+   2 -^OS/VS Cobol       +B -^Simula            +   ^                 {Z
+   3 -^Fortran IV        +C -^SNOBOL            +   ^
+   4 -^Mortran           +D -^WATFIV            +   ^
+   5 -^PL/I              +E -^XPL               +   ^
+   6 -^Linkage Editor    +F -^SPITBOL           +   ^
+   7 -^ASSIST            +G -^GCC               +   ^
+   8 -^ALGOL             +H -^PL360             +   ^
+   9 -^Pascal            +I -^BASIC 360
+
+Select OPTION to continue generating JCL
+Enter CANCEL on OPTION LINE to EXIT without submitting JOB
+Press END KEY to submit JOB
+Source DATA online ===>[Z  +(Y/N)     Delete SUBMIT JCL ===>[Z  +(Y/N)
+USING ===>%&TDSN
+JOB STATEMENT INFORMATION:
%      &JCL1                                                                   +
%      &JCL2                                                                   +
%      &JCL3                                                                   +
%      &JCL4                                                                   +
+
)INIT
  .HELP = HSPOPT5
  .ZVARS = '(ZUSER,ZPANELID,BGSO,BGDJ)'
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HSPOPT5    /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HSPOPT5  /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &ZSEL = TRUNC (&ZCMD,'.')
  &ZTRAIL = .TRAIL
  &BGPO = &ZCMD
  VER (&BGPO,NB,MSG=BGOPT01)
)END
@@
//* -------------------------------------------------------*
//* *  ISPOPT5 for MVS3.8J TSO / Hercules                  *
//* *                                                      *
//* *  JOB: $INST99  Install Other Files                   *
//* *                                                      *
//* *       Install OBJ Validation File                    *
//* *                                                      *
//* -------------------------------------------------------*
//*
//OBJFILE  EXEC PGM=IEFBR14
//OBJ      DD DISP=(MOD,CATLG),
//         DSN=ISPOPT5.V0R9M00.OBJ,
//         UNIT=SYSDA,                       <-- Review and Modify
//         SPACE=(CYL,(1,1,10),),
//         DCB=(DSORG=PO,RECFM=FB,LRECL=80,BLKSIZE=3120)
//
