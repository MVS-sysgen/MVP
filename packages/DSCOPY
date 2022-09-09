//DSCOPY JOB (JOB),
//             'INSTALL DSCOPY',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//PROC   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.CMDPROC,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CDSCPY0
PROC 0 DEBUG DSN1() VOL1() DSO1() PRT1(N) PFLG(YYNNNNN9)

/********************************************************************/
/*                                                                  */
/* CLIST: CDSCPY0    (DASD Dataset Copy - PS/PO only)               */
/*                                                                  */
/* Command:                                                         */
/* CDSCPY0 DEBUG DSN1() VOL1() DSO1()                               */
/*                                                                  */
/* Parameters:                                                      */
/* DEBUG    Flag used to display DEBUG information during execution */
/* DSN1     Source dataset name (required)                          */
/* VOL1     Source dataset volume serial (optional)                 */
/* DSO1     Source dataset organization (optional)                  */
/* PRT1     Copy utility log  (Y/N)     (default=N)                 */
/* PFLG     Processing Flags            (default=YYNNNNN9)          */
/*          1:  Y    - Use DDN for allocation of files              */
/*             other - Use ALLOC for allocation of files            */
/*          2:  Y    - Use $ CP to call IEBGENER                    */
/*             other - Use TSO CALL to call IEBGENER                */
/*          3:  TBD  - TBD                                          */
/*             other - TBD                                          */
/*          4:  TBD  - TBD                                          */
/*             other - TBD                                          */
/*          5:  TBD  - TBD                                          */
/*             other - TBD                                          */
/*          6:  TBD  - TBD                                          */
/*             other - TBD                                          */
/*          7:  TBD  - TBD                                          */
/*             other - TBD                                          */
/*          8:       - Level of CONTROL information for DEBUG       */
/*              0    - NOLIST NOCONLIST NOSYMLIST NOFLUSH PROMPT MSG*/
/*             other - LIST   CONLIST   SYMLIST NOFLUSH PROMPT MSG  */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/DSCOPY-in-MVS38J               */
/*         Copyright (C) 2021 Larry Belmontes, Jr.                  */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* CDSCPY0 can run under native TSO or TSO/ISPF.                    */
/*                                                                  */
/* Under TSO, user presentation is in the form of line displays.    */
/*                                                                  */
/* Under ISPF, user presentation is in the form of panels.          */
/*                                                                  */
/* CDSCPY0 receives multiple parameters.  Data set name is required */
/* and other parameters are optional.  See above documentation.     */
/*                                                                  */
/* The source data set is validated for existence and data set      */
/* attributes are fetched via LISTSDJ.  The source data set must    */
/* reside on DASD as a sequential or partitioned organization.      */
/*                                                                  */
/* The destination data set name is defaulted with source data set  */
/* name appended with a time-stamp in attempt to prevent duplicate  */
/* data set names.                                                  */
/*                                                                  */
/* Under ISPF, the Data Set Copy panel (PDCPY0 or PDCPY1) is        */
/* pre-filled with source and destination data set names and        */
/* attributes.                                                      */
/*                                                                  */
/* Upon review and/or modification of new data set attributes,      */
/* the ENTER key is used as confirmation of the copy operation.     */
/*                                                                  */
/* Under TSO, data set names and attributes cannot be modified.     */
/* Copy operation uses  source data set attributes with no          */
/* user intervention.                                               */
/*                                                                  */
/* The destination data set name is validated for non-existence.    */
/*                                                                  */
/* The copy operation occurs in real-time via IEBGENER (PS dataset) */
/* or COPYPDS via IEBCOPY (PO dataset).                             */
/*                                                                  */
/* Control is returned to the invoking entity.                      */
/*                                                                  */
/*                                                                  */
/* Messages:                                                        */
/* ---------------------------------------------------------------  */
/* DCPY00         Messages DCPY000 - DCPY009                        */
/* DCPY01         Messages DCPY010 - DCPY019                        */
/* DCPY02         Messages DCPY020 - DCPY029                        */
/*                                                                  */
/*                                                                  */
/* Panels:                                                          */
/* ---------------------------------------------------------------  */
/* PDCPY0         Data Set Copy Action  panel  PO                   */
/* PDCPY1         Data Set Copy Action  panel  PS                   */
/* HDCPY0         Data Set Copy Action  Help panel                  */
/*                                                                  */
/*                                                                  */
/* Utilities:                                                       */
/* ---------------------------------------------------------------  */
/* SYSDSN         Check for data set presence                       */
/* LISTDSJ        Data set attributes                               */
/* CUTIL00        CLIST variable functions utility                  */
/* COPYPDS        Copy PO data sets TSO CP (CBT168)                 */
/* IFALC          Check if FILE allocated to TSO session (CBT270)   */
/* IEBGENER       Copy PS data sets utility                         */
/* $              CALL command replacement TSO CP (CBT077)          */
/*                     for pgms in LINKLIST                         */
/*                                                                  */
/*                                                                  */
/********************************************************************/
/*                                                                  */
/* CLIST Return                                                     */
/*    Code        Description                                       */
/* ------------   ------------------------------------------------  */
/*     00         Normal Completion                                 */
/*     04         See IEBGENER utility                              */
/*     04         See IEBCOPY  utility                              */
/*     08         See IEBGENER utility                              */
/*     08         See IEBCOPY  utility                              */
/*     12         See IEBGENER utility                              */
/*     16         See IEBGENER utility                              */
/* -   40         Invalid DSORG, must be PO or PS d                 */
/* -   41         Invalid RECFM, not specified                      */
/* -   51         DSN1 failure (i.e. invalid DSN, DSN not found)    */
/* -   52         DSN2 failure (i.e. duplicate DSN)                 */
/* -   60         ATTR failure DSN2                                 */
/* -   61         ALLOC failure SYSUT2 for DSN2                     */
/* -   62         ALLOC failure SYSUT1 for DSN1                     */
/* -   63         ALLOC failure SYSIN                               */
/* -   64         ALLOC failure SYSPRINT                            */
/* -   71         DSN1 not provided                                 */
/* -   72         DSO1 provided, but invalid (must be PO or PS)     */
/* -   80         COPY request cancelled by User                    */
/*                                                                  */
/********************************************************************/
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
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 04/20/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/


/********************************************************************/
/* Initialization                                                   */
/********************************************************************/
SET MYNAME = &STR(CDSCPY0 )
SET RC = 00                            /* RC initialized to 00      */
SET EMSG =                             /* ISPF Message              */


/********************************************************************/
/* CONTROL Options                                                  */
/*                                                                  */
/* SYMLIST Show each stmt before symbolic sub                       */
/*         includes CMDs, SUBCMDs and CLIST stmts                   */
/*    LIST Show CMDs and SUBCMDs after symbolic sub before execution*/
/* CONLIST Show CLIST stmt after symbolic sub before execution      */
/********************************************************************/
IF &DEBUG = DEBUG THEN DO
  IF &SUBSTR(8,&PFLG) = 0 THEN DO      /* Set DEBUG CONTROL         */
    CONTROL NOLIST NOCONLIST NOSYMLIST NOFLUSH PROMPT MSG
    END
  ELSE DO
    CONTROL   LIST   CONLIST   SYMLIST NOFLUSH PROMPT MSG
    END
  END
ELSE DO                                /* Set Normal CONTROL        */
  CONTROL NOLIST NOCONLIST NOSYMLIST FLUSH NOPROMPT MSG
  END


IF &DEBUG = DEBUG THEN DO
  WRITE ** START DEBUG INFO           <-------***
  WRITE ** &MYNAME **
  WRITE ** ISPF Status: &SYSISPF *************
  IF &STR(&DSN1) > &STR( ) THEN WRITE ** PARM DSN1 PRESENT
  IF &STR(&VOL1) > &STR( ) THEN WRITE ** PARM VOL1 PRESENT
  IF &STR(&DSO1) > &STR( ) THEN WRITE ** PARM DSO1 PRESENT
  IF &STR(&PRT1) > &STR( ) THEN WRITE ** PARM PRT1 PRESENT
  IF &STR(&PFLG) > &STR( ) THEN WRITE ** PARM PFLG PRESENT
  END


/********************************************************************/
/* Check for DSN parm                            DCPY019    RC=71   */
/********************************************************************/
IF &DEBUG = DEBUG THEN WRITE ** 01:CHECK FOR DSN1          <-------***

IF &STR(&DSN1) > &STR( ) THEN
ELSE DO
  SET EMSG = DCPY019                   /* NO DSN provided           */
  SET RC = 71
  GOTO MYEXIT
  END


/********************************************************************/
/* Check for Valid DSO Parm                                         */
/********************************************************************/
/* Only process PS (Seq) or PO (PDS) files       DCPY018    RC=72   */
/********************************************************************/
IF &DEBUG = DEBUG THEN WRITE ** 02:CHECK PS/PO DSO1        <-------***

IF &STR(&DSO1) > &STR( ) THEN DO
  SET SYSDSORG = &STR(&DSO1)
  IF &SYSDSORG = PO OR &SYSDSORG = PS THEN
  ELSE DO
    SET EMSG = DCPY018                 /* Invalid DSO provided      */
    SET RC = 72
    GOTO MYEXIT
    END
  END


/********************************************************************/
/* Fetch DSN info via LISTSDJ                    DCPY001    RC=51   */
/********************************************************************/
IF &DEBUG = DEBUG THEN WRITE ** 03:FETCH DSN1 ATTRIBUTES   <-------***

IF &STR(&VOL1) > &STR( ) THEN DO
  SET CMDLDSJ = LISTDSJ &STR('&DSN1') VOL(&VOL1) DIR
  END
ELSE DO
  SET CMDLDSJ = LISTDSJ &STR('&DSN1') DIR
  END

&CMDLDSJ
SET RC = &LASTCC
SET RSN = &SYSREASON
SET MLVL1 = &SYSMSGLVL1
SET MLVL2 = &SYSMSGLVL2

IF &DEBUG = DEBUG THEN DO
  WRITE ** &CMDLDSJ
  WRITE ** RC=&RC
  WRITE ** REASON CODE=&SYSREASON
  WRITE ** MSGLVL1=&SYSMSGLVL1
  WRITE ** MSGLVL2=&SYSMSGLVL2
  END

IF &RC > 0 THEN DO
  SET EMSG = DCPY001                   /* DSN1 Failed               */
  SET RC = 51
  GOTO MYEXIT
  END

/********************************************************************/
/* DSORG PS (Seq) or PO (PDS) files,             DCPY000    RC=40   */
/* set panel to use                                                 */
/********************************************************************/
IF &DEBUG = DEBUG THEN WRITE ** 04:CHECK PS/PO, SET PANEL  <-------***

IF &SYSDSORG = PO THEN DO
  SET CPYPNL = &STR(PDCPY0)            /* PDS Panel                 */
  END
ELSE DO
  IF &SYSDSORG = PS THEN DO            /* SEQ Panel                 */
    SET CPYPNL = &STR(PDCPY1)
    END
  ELSE DO
    SET EMSG = DCPY000                 /* Invalid DSO               */
    SET RC = 40
    GOTO MYEXIT
    END
  END


/********************************************************************/
/* SET Copy panel variables from LISTDSJ                            */
/********************************************************************/
IF &DEBUG = DEBUG THEN WRITE ** 05:SET SYS LISTDSJ VALUES  <-------***

SET DSN      = &SYSDSNAME
SET VOLUME   = &SYSVOLUME
SET UNIT     = &SYSUNIT
SET DSORG    = &SYSDSORG
SET RECFM    = &SYSRECFM
SET LRECL    = &SYSLRECL
SET BLKSIZE  = &SYSBLKSIZE
SET ALLOC    = &SYSALLOC
SET PRIMARY  = &SYSPRIMARY
SET SECOND   = &SYSSECONDS
SET UNITS    = &SYSUNITS
SET ADIRBLK  = &SYSADIRBLK
SET UDIRBLK  = &SYSUDIRBLK
SET RACFPROT = &SYSRACFA
SET MBRS     = &SYSMEMBERS

SET OVOLUME  = &SYSVOLUME
SET OUNIT    = &SYSUNIT


/********************************************************************/
/* Add date-time to DSN1 for DSN2 using CUTIL00                     */
/* Check for DSN2 truncation                     DCPY012            */
/********************************************************************/
IF &DEBUG = DEBUG THEN WRITE ** 06:SET DSN2                <-------***

SET DSN2 = &DSN1&STR(.)
CUTIL00 TDSN DSN2

IF &DSN2L > 44 THEN DO
  SET DSN2 = &SUBSTR(1:44,&STR(&DSN2))
  SET EMSG = DCPY012                 /* Dest DSN truncated          */
  IF &STR(&SYSISPF) = &STR(ACTIVE) THEN DO
    END
  ELSE DO
    WRITE ** DESTINATION DSN TRUNCATED.....
    WRITE ** ATTEMPTING TO COPY DATA SET...
    END
  END


/********************************************************************/
/* If ISPF-Active, display Copy Action Panel     DCPY002    RC=80   */
/*                                               DCPY003    RC=80   */
/********************************************************************/
/* Set PRTACTN for COPY operations                                  */
/********************************************************************/
IF &DEBUG = DEBUG THEN WRITE ** 07:DISPLAY COPY PANEL      <-------***

IF &STR(&SYSISPF) = &STR(ACTIVE) THEN DO
  IF &EMSG > &STR( ) THEN DO
    ISPEXEC DISPLAY PANEL(&CPYPNL) MSG(&EMSG)
    END
  ELSE DO
    ISPEXEC DISPLAY PANEL(&CPYPNL)
    END
  SET RC = &LASTCC

  IF &RC > 0 THEN DO
    SET EMSG = DCPY002                 /* Copy canceled             */
    SET RC = 80
    GOTO MYEXIT
    END

  IF &KEYPRESS NE PF00 THEN DO
    SET EMSG = DCPY003                 /* Copy canceled, not ENTER  */
    SET RC = 80
    GOTO MYEXIT
    END

  IF &LISTCPY = N THEN -
    SET PRTACTN = NOPRINT              /* Set from scrn   NOPRINT   */
  ELSE -
    SET PRTACTN = &STR(PRINT LIST)     /* Set from scrn   PRINT     */

  END
ELSE DO
  IF &STR(&PRT1) = N THEN -
    SET PRTACTN = NOPRINT              /* Set from PROC   NOPRINT   */
  ELSE -
    SET PRTACTN = &STR(PRINT LIST)     /* Set from PROC   PRINT     */

  END

/********************************************************************/
/* Ensure DSN2 not exist before COPY             DCPY004    RC=52   */
/********************************************************************/
IF &DEBUG = DEBUG THEN WRITE ** 08:CHECK FOR DSN2 EXIST    <-------***

SET DSN2 = &DSN2
IF &SYSDSN('&DSN2') EQ OK THEN DO
  SET EMSG = DCPY004                   /* Dup copy-to DSN           */
  SET RC = 52
  GOTO MYEXIT
  END


/********************************************************************/
/* Check RECFM                                   DCPY017    RC=41   */
/********************************************************************/
IF &DEBUG = DEBUG THEN WRITE ** 09:CHECK RECFM             <-------***

IF &LENGTH(&RECFM) = 0 THEN DO
  SET EMSG = DCPY017                   /* No RECFM  specified       */
  SET RC = 41
  GOTO MYEXIT
  END


/********************************************************************/
/* Allocate new DSN Attributes  MYATTR           DCPY005    RC=60   */
/********************************************************************/
IF &DEBUG = DEBUG THEN WRITE ** 10:ALLOC MYATTR            <-------***

IF &LENGTH(&RECFM) GE 1 THEN -
  SET THERECFM = &STR(&SUBSTR(1:1,&RECFM))
IF &LENGTH(&RECFM) GE 2 THEN -
  SET THERECFM = &STR(&THERECFM &SUBSTR(2:2,&RECFM))
IF &LENGTH(&RECFM) GE 3 THEN -
  SET THERECFM = &STR(&THERECFM &SUBSTR(3:3,&RECFM))

IF &THERECFM = U THEN -
  SET THELRECL =
ELSE -
  SET THELRECL = &STR(LRECL(&LRECL))

IF &SUBSTR(1,&PFLG) = Y THEN DO        /* Use DDN for allocation    */
  END
ELSE DO                                /* Use ATTR for allocation   */
  IFALC F(MYATTR)                      /* Free if allocated         */
  IF &LASTCC = 0 THEN DO
    FREE ATTR(MYATTR)
    END

  ATTR MYATTR &THELRECL BLKSIZE(&BLKSIZE) +
    RECFM(&THERECFM)  DSORG(&DSORG)

  SET RC = &LASTCC
  IF &RC NE 0 THEN -
    DO
      SET EMSG = DCPY005                 /* ATTR Alloc TO-dsn error   */
      SET RC = 60
      GOTO MYEXIT
    END
  END

/********************************************************************/
/* Allocate new DSN (&DSN2 on &VOLUME)  SYSUT2   DCPY006    RC=61   */
/********************************************************************/
IF &DEBUG = DEBUG THEN WRITE ** 11:ALLOC SYSUT2            <-------***

IF &DSORG = PO THEN -
  SET THEDIRBLKS = &STR(DIR(&ADIRBLK))
ELSE -
  SET THEDIRBLKS =

IF &UNITS = BLOCK THEN -
  SET THEUNITS = &STR(BLOCK(&ALLOC))
ELSE -
  SET THEUNITS = &UNITS

IF &LENGTH(&UNIT) > 0 THEN -
  SET THEUNIT = &STR(UNIT(&UNIT))
ELSE -
  SET THEUNIT = &STR()

IF &SUBSTR(1,&PFLG) = Y THEN DO        /* Use DDN for allocation    */

  DDN SYSUT2 '&DSN2' NEW VOLUME(&VOLUME) +
      &THEUNIT SPACE(&PRIMARY,&SECOND) &THEUNITS +
      &THEDIRBLKS  +
      &THELRECL BLKSIZE(&BLKSIZE) +
      RECFM(&THERECFM)  DSORG(&DSORG)

  END
ELSE DO                                /* Use ATTR/ALLOC for alloc  */
  IFALC F(SYSUT2)                      /* Free if allocated         */
  IF &LASTCC = 0 THEN DO
    FREE FILE(SYSUT2)
    END

  ALLOC FI(SYSUT2) DATASET('&DSN2') NEW VOLUME(&VOLUME) +
   &THEUNIT SPACE(&PRIMARY,&SECOND) &THEUNITS RELEASE +
   USING(MYATTR) &THEDIRBLKS

  END

SET RC = &LASTCC
IF &RC NE 0 THEN -
  DO
    SET EMSG = DCPY006                 /* SYSUT2 Alloc err          */
    SET RC = 61
    GOTO MYEXIT
  END

IF &DSORG = PO THEN DO                 /* PDS, branch to COPYPDS    */
  IFALC F(MYATTR)                      /* Free if allocated         */
  IF &LASTCC = 0 THEN DO
    FREE ATTR(MYATTR)
    END
  IFALC F(SYSUT2)                      /* Free if allocated         */
  IF &LASTCC = 0 THEN DO
    FREE FILE(SYSUT2)
    END
  GOTO POCOPY                          /*  to perform copy          */
  END

/********************************************************************/
/* Allocate SYSUT1 for IEBGENER                  DCPY007    RC=62   */
/********************************************************************/
PSCOPY: +
IF &DEBUG = DEBUG THEN WRITE ** 12:ALLOC SYSUT1            <-------***

IF &LENGTH(&OVOLUME) > 0 THEN -
  SET THEOVOL = &STR(VOLUME(&OVOLUME))
ELSE -
  SET THEOVOL =&STR()

IF &SUBSTR(1,&PFLG) = Y THEN DO        /* Use DDN for allocation    */

  DDN SYSUT1 '&DSN' &THEOVOL +
      UNIT(&OUNIT) SHR

  END
ELSE DO                                /* Use ATTR/ALLOC for alloc  */
  IFALC F(SYSUT1)                      /* Free if allocated         */
  IF &LASTCC = 0 THEN DO
    FREE FILE(SYSUT1)
    END

  ALLOC FI(SYSUT1) DATASET('&DSN') &THEOVOL +
  UNIT(&OUNIT) SHR

  END

SET RC = &LASTCC
IF &RC NE 0 THEN DO
  SET EMSG = DCPY007                   /* SYSUT1 Alloc err          */
  SET RC = 62
  GOTO MYEXIT
  END

/********************************************************************/
/* Allocate SYSIN for IEBGENER                   DCPY008    RC=63   */
/********************************************************************/
IF &DEBUG = DEBUG THEN WRITE ** 13:ALLOC SYSIN             <-------***

ALLOC FI(SYSIN) DUMMY

IF &SUBSTR(1,&PFLG) = Y THEN DO        /* Use DDN for allocation    */

  DDN SYSIN 'DUMMY'

  END
ELSE DO                                /* Use ATTR/ALLOC for alloc  */
  IFALC F(SYSIN)                       /* Free if allocated         */
  IF &LASTCC = 0 THEN DO
    FREE FILE(SYSIN)
    END

  ALLOC FI(SYSIN) DUMMY

  END

SET RC = &LASTCC
IF &RC > 0 THEN DO
  SET EMSG = DCPY008                 /* SYSIN  Alloc err            */
  SET RC = 63
  GOTO MYEXIT
  END

/********************************************************************/
/* Allocate SYSPRINT for IEBGENER                DCPY009    RC=64   */
/********************************************************************/
IF &DEBUG = DEBUG THEN WRITE ** 14:ALLOC SYSPRINT          <-------***

IF &SUBSTR(1,&PFLG) = Y THEN DO        /* Use DDN for allocation    */

  IF &PRTACTN = NOPRINT THEN DO
    DDN SYSPRINT 'DUMMY'
    END
  ELSE DO
    DDN SYSPRINT '*'
    END

  END
ELSE DO                                /* Use ATTR/ALLOC for alloc  */
  IFALC F(SYSPRINT)                    /* Free if allocated         */
  IF &LASTCC = 0 THEN DO
    FREE FILE(SYSPRINT)
    END

  IF &PRTACTN = NOPRINT THEN DO
    ALLOC FI(SYSPRINT) DUMMY
    END
  ELSE DO
    ALLOC FI(SYSPRINT) DA(*)
    END

  END
SET RC = &LASTCC
IF &RC > 0 THEN DO
  SET EMSG = DCPY009                 /* SYSPRINT Alloc err          */
  SET RC = 64
  GOTO MYEXIT
  END


/********************************************************************/
/* IEBGENER called via $ command (CBT077)        DCPY011            */
/********************************************************************/
IF &DEBUG = DEBUG THEN WRITE ** 15:PS COPY                 <-------***

IF &STR(&SYSISPF) = &STR(ACTIVE) THEN
ELSE DO
  WRITE COPY SEQUENTIAL
  WRITE '&DSN' --> '&DSN2'
  END

IF &SUBSTR(2,&PFLG) = Y THEN DO        /* Use $ CP for CALL         */
  IF &DEBUG = DEBUG THEN DO
    WRITE ** 13:USING $ CALL            <-------***
    END
  $ IEBGENER
  SET RC = &LASTCC
  END
ELSE DO
  IF &DEBUG = DEBUG THEN DO
    WRITE ** 13:USING TSO CALL          <-------***
    END
  CALL 'SYS1.LINKLIB(IEBGENER)'
  SET RC = &LASTCC
  END

SET EMSG = DCPY011                 /* IEBGENER done...              */

GOTO FREEDSNS


/********************************************************************/
/* IEBCOPY via COPYPDS  (CBT168)                 DCPY010            */
/********************************************************************/
POCOPY: +
IF &DEBUG = DEBUG THEN WRITE ** 16:PO COPY                 <-------***

IF &STR(&SYSISPF) = &STR(ACTIVE) THEN
ELSE DO
  WRITE COPY PDS
  WRITE '&DSN' --> '&DSN2'
  END
COPYPDS '&DSN' '&DSN2' IVOL(&OVOLUME) &STR(&PRTACTN)
SET RC = &LASTCC

SET EMSG = DCPY010                 /* COPYPDS done...               */

GOTO MYEXIT


/********************************************************************/
/* FREE all datasets                                                */
/********************************************************************/
FREEDSNS: -
IF &DEBUG = DEBUG THEN WRITE ** 17:FREE DSNs               <-------***

IFALC F(MYATTR)                        /* Free if allocated         */
IF &LASTCC = 0 THEN DO
  FREE ATTR(MYATTR)
  END

FREE FI(SYSUT2)
FREE FI(SYSUT1)
FREE FI(SYSIN)
FREE FI(SYSPRINT)


/********************************************************************/
/* EXIT                                                             */
/********************************************************************/
MYEXIT: -
IF &DEBUG = DEBUG THEN WRITE ** 99:MYEXIT                  <-------***

IF &STR(&SYSISPF) = &STR(ACTIVE) THEN DO
  IF &EMSG > &STR( ) THEN DO
    ISPEXEC SETMSG MSG(&EMSG)          /* Set MSG for next panel    */
    END
  END
ELSE DO
  WRITE ** &MYNAME: RC=&RC
  END

IF &DEBUG = DEBUG THEN DO
  WRITE ** END   DEBUG INFO           <-------***
  WRITE ** &MYNAME: RC=&RC
  END

EXIT CODE(&RC)

END
@@
//HELP   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.HELP,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CDSCPY0
)F FUNCTION -
  The CDSCPY0 CLIST copies a sequential or partitioned dats set
  to another assigning a destination data set name comprised
  of the source data set name appended with a date-time stamp.
  Date-time stamp is in the format  Dyyjjj.Thhmmss

)X SYNTAX  -
         CDSCPY0 DSN1() VOL1() DSO1()

  REQUIRED - DSN1
  DEFAULTS - NONE
  ALIAS    - NONE

))DSN1     - Source data set name of sequential or partitioned file.
             DSN1 must be fully qualifed, no quotes are necessary.

))VOL1     - optional, volume of DSN1 if not catalogued

))DSO1     - optional, data set organization of DSN1



@@
//PLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.PLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=HDCPY0
/********************************************************************/
/*                                                                  */
/*    PANEL: HDCPY0                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/DSCOPY-in-MVS38J               */
/*         Copyright (C) 2021 Larry Belmontes, Jr.                  */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x DSCOPY   DSN Copy Action Help Panel                     */
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
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 04/20/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ~ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 { TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(TURQ)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)  COLOR(GREEN)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY EXPAND(\\)
%-Tutorial-------------- Data Set Copy Action -------------------------Tutorial-
%Command ===>_ZCMD                                                            +
%
+                                                                      [Z
{ This function simplifies copying a sequential or partitioned         [Z
{ data set by pre-filling destination data set attributes that may
{ be modified before confirming the copy action similar to modifying
{ JCL DD statement SPACE or DCB parameters.
+
+ The%From DSN+exhibits some attributes (display only).
+
+ The%To DSN+exhibits attributes that can be over-typed including the
+ destination data set name.
+
+ Source and Destination data sets must reside on DASD.
)Init
  .CURSOR = ZCMD
  .ZVARS = '(ZUSER,ZPANELID)'
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)Proc
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)End
./ ADD NAME=HDCPY0R
/********************************************************************/
/*                                                                  */
/*    PANEL: HDCPY0R                                                */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/DSCOPY-in-MVS38J               */
/*         Copyright (C) 2021 Larry Belmontes, Jr.                  */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x DSCOPY   DSN Request Help Panel                         */
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
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 04/20/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 $ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 # TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(BLUE)   HILITE(REVERSE)
 ~ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)  COLOR(BLUE)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY EXPAND(\\)
%-Tutorial-------------- Data Set Copy Request ------------------------Tutorial-
%Command ===>_ZCMD                                                            +
%
+                                                                      {Z
+ This panel is used to request a DASD dataset to be copied.           {Z
+
+ The data set organization must be sequential or partitioned.
+
+
+
+
+
)Init
  .CURSOR = ZCMD
  .ZVARS = '(ZUSER,ZPANELID)'
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)Proc
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
)End
./ ADD NAME=PDCPY0
/********************************************************************/
/*                                                                  */
/*    PANEL: PDCPY0                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/DSCOPY-in-MVS38J               */
/*         Copyright (C) 2021 Larry Belmontes, Jr.                  */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x DSCOPY     DSN Request Panel (PDS)                      */
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
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 04/20/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 @ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 ~ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(RED)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)  COLOR(TURQ)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY EXPAND(\\)
%----------------------- Data Set Copy Action ----------------------------------
%Command ===>_ZCMD                                                            +
%
+                                                                      [Z
% From DSN  . :[dsn                                         +          [Z
+  Volume . . :[ovolume%
+  DSORG  . . :[dsorg+
+  RACF . . . :[racfprot+
+  CurAlloc . :[ounits  [alloc+
+  DirBlksUsed:[udirblk+ Members:[mbrs   +
+
~ Copy%From DSN~-->%To DSN~using below attributes.+
~ Overtype details and press ENTER to perform copy operation.+
+
% To DSN  :_dsn2                                        +
+  Volume :_volume+  Unit     :_unit+
+  RECFM  :[recfm+   BLKSIZE  :_blksize+  LRECL    :[lrecl+
+  Alloc  :_units   +Primary  :_primary+  Secondary:_second+
+  DirBlks:_adirblk+
+
% List:_Z+ (Y/N) List copy member log on screen
)Init
  &ounits = &units
  .CURSOR = 'dsn2'
  .HELP = HDCPY0
  .ZVARS = '(ZUSER,ZPANELID,LISTCPY)'
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HDCPY0     /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HDCPY0   /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LISTCPY) PROFILE
)Proc
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  IF (&KEYPRESS NE PF03)
    VER (&DSN2,NONBLANK,DSNAME,MSG=DCPY020)
    VER (&VOLUME,NONBLANK,MSG=DCPY021)
    VER (&UNIT,NONBLANK,MSG=DCPY021)
    VER (&UNITS,NB,LIST,BLOCK,TRACK,CYLINDER,MSG=DCPY023)
    VER (&BLKSIZE,NUM,MSG=DCPY022)
    VER (&LRECL,NUM,MSG=DCPY022)
    VER (&PRIMARY,NUM,MSG=DCPY022)
    VER (&SECOND,NUM,MSG=DCPY022)
    IF (&DSORG = PO)
      VER (&ADIRBLK,NUM,MSG=DCPY022)
    VER (&LISTCPY,NB,LIST,Y,N,MSG=DCPY024)
    VPUT (LISTCPY) PROFILE
)End
./ ADD NAME=PDCPY0R
/********************************************************************/
/*                                                                  */
/*    PANEL: PDCPY0R                                                */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/DSCOPY-in-MVS38J               */
/*         Copyright (C) 2021 Larry Belmontes, Jr.                  */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x DSCOPY   DSN Request Panel                              */
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
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 04/20/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ~ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY EXPAND(\\)
%----------------------- Data Set Copy Request ---------------------------------
%Command ===>_ZCMD                                                            +
%
+                                                                      {Z
+ Source  :_dsn                                         +              {Z
+ Volume  :_volume%    (If not catalogued)
+
+
% Enter DASD source name of sequential or partitioned data set.
+
+
+
)Init
  .CURSOR = 'dsn'
  .HELP = HDCPY0R
  .ZVARS = '(ZUSER,ZPANELID)'
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HDCPY0R    /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HDCPY0R  /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (DSN VOLUME) PROFILE
)Proc
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  IF (&KEYPRESS NE PF03)
    VER (&DSN,NONBLANK,DSNAME,MSG=DCPY020)
    VPUT (DSN VOLUME) PROFILE
    &DCPYPRM = 'DSN1(&DSN)'
    IF (&VOLUME > ' ')
      &DCPYPRM = '&DCPYPRM VOL1(&VOLUME)'
    &ZSEL = 'CMD(%CDSCPY0 &ZCMD &DCPYPRM) NEWAPPL(DCPY)'
)End
./ ADD NAME=PDCPY1
/********************************************************************/
/*                                                                  */
/*    PANEL: PDCPY1                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/DSCOPY-in-MVS38J               */
/*         Copyright (C) 2021 Larry Belmontes, Jr.                  */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x DSCOPY     DSN Request Panel (SEQ)                      */
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
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 04/20/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 @ TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)  HILITE(USCORE)
 ~ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(RED)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)  COLOR(TURQ)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY EXPAND(\\)
%----------------------- Data Set Copy Action ----------------------------------
%Command ===>_ZCMD                                                            +
%
+                                                                      [Z
% From DSN  . :[dsn                                         +          [Z
+  Volume . . :[ovolume%
+  DSORG  . . :[dsorg+
+  RACF . . . :[racfprot+
+  CurAlloc . :[ounits  [alloc+
+
+
~ Copy%From DSN~-->%To DSN~using below attributes.+
~ Overtype details and press ENTER to perform copy operation.+
+
% To DSN  :_dsn2                                        +
+  Volume :_volume+  Unit     :_unit+
+  RECFM  :[recfm+   BLKSIZE  :_blksize+  LRECL    :[lrecl+
+  Alloc  :_units   +Primary  :_primary+  Secondary:_second+
+
% List:_Z+ (Y/N) List copy operation log on screen
)Init
  &ounits = &units
  .CURSOR = 'dsn2'
  .HELP = HDCPY0
  .ZVARS = '(ZUSER,ZPANELID,LISTCPY)'
  &ZPRIM = NO         /* NOT A PRIMARY OPTION MENU         */
  &ZHTOP = HDCPY0     /* TUTORIAL TABLE OF CONTENTS        */
  &ZHINDEX = HDCPY0   /* TUTORIAL INDEX - 1ST PAGE isptutor*/
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  VGET (LISTCPY) PROFILE
)Proc
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  IF (&KEYPRESS NE PF03)
    VER (&DSN2,NONBLANK,DSNAME,MSG=DCPY020)
    VER (&VOLUME,NONBLANK,MSG=DCPY021)
    VER (&UNIT,NONBLANK,MSG=DCPY021)
    VER (&UNITS,NB,LIST,BLOCK,TRACK,CYLINDER,MSG=DCPY023)
    VER (&BLKSIZE,NUM,MSG=DCPY022)
    VER (&LRECL,NUM,MSG=DCPY022)
    VER (&PRIMARY,NUM,MSG=DCPY022)
    VER (&SECOND,NUM,MSG=DCPY022)
    IF (&DSORG = PO)
      VER (&ADIRBLK,NUM,MSG=DCPY022)
    VER (&LISTCPY,NB,LIST,Y,N,MSG=DCPY024)
    VPUT (LISTCPY) PROFILE
)End
@@
//MLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.MLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=DCPY00
/********************************************************************/
/*                                                                  */
/* MESSAGES: DCPY00                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/DSCOPY-in-MVS38J               */
/*         Copyright (C) 2021 Larry Belmontes, Jr.                  */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 DCPY messages                                           */
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
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 04/20/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
DCPY000  'Invalid DSORG           ' .ALARM=NO
'DCPY000  COPY &DSN, invalid DSORG=&SYSDSORG                                   '
DCPY001  'Invalid Input Dataset   ' .ALARM=NO
'DCPY001  &DSN1 failed. LISTDSJ RC=&RC Reason=&RSN &MLVL2                      '
DCPY002  'Copy canceled per User  ' .ALARM=NO
'DCPY002  COPY cancelled by user. RC=&RC                                       '
DCPY003  'Copy canceled, not ENTER' .ALARM=NO
'DCPY003  COPY cancelled, not ENTER key. PF=&KEYPRESS  RC=&RC                  '
DCPY004  'Duplicate TO-dsn        ' .ALARM=NO
'DCPY004  &DSN2 already exists! Cannot proceed!                                '
DCPY005  'Alloc error TO-dsn ATTR ' .ALARM=NO
'DCPY005  &DSN2 ATTR alloc failed! RC=&RC                                      '
DCPY006  'Alloc error SYSUT2      ' .ALARM=NO
'DCPY006  SYSUT2 ALLOC for &DSN2 failed. RC=&RC                                '
DCPY007  'Alloc error SYSUT1      ' .ALARM=NO
'DCPY007  SYSUT1 ALLOC for &DSN failed. RC=&RC                                 '
DCPY008  'Alloc error SYSIN       ' .ALARM=NO
'DCPY008  SYSIN ALLOC for DUMMY failed.  RC=&RC                                '
DCPY009  'Alloc error SYSPRINT    ' .ALARM=NO
'DCPY009  SYSPRINT ALLOC for DUMMY failed.  RC=&RC                             '
./ ADD NAME=DCPY01
/********************************************************************/
/*                                                                  */
/* MESSAGES: DCPY01                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/DSCOPY-in-MVS38J               */
/*         Copyright (C) 2021 Larry Belmontes, Jr.                  */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 DCPY messages                                           */
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
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 04/20/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
DCPY010  'COPYPDS Done RC=&RC     ' .ALARM=NO
'DCPY010  COPYPDS &DSN &OVOLUME &DSN2 &VOLUME                                  '
DCPY011  'IEBGENER Done RC=&RC    ' .ALARM=NO
'DCPY011  IEBGENER &DSN &OVOLUME &DSN2 &VOLUME                                 '
DCPY012  'Destination DSN Trunc   ' .ALARM=NO
'DCPY012  Destination DSN truncated, review and modify destination DSN         '
DCPY017  'RECFM Invalid           ' .ALARM=NO
'DCPY017  RECFM not specified                                                  '
DCPY018  'DSO Invalid             ' .ALARM=NO
'DCPY018  DSO must be PO or PS                                                 '
DCPY019  'DSN Missing             ' .ALARM=NO
'DCPY019  DSN must be provided                                                 '
./ ADD NAME=DCPY02
/********************************************************************/
/*                                                                  */
/* MESSAGES: DCPY02                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/DSCOPY-in-MVS38J               */
/*         Copyright (C) 2021 Larry Belmontes, Jr.                  */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 DCPY messages                                           */
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
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 04/20/2021 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
DCPY020  'DSN Invalid             ' .ALARM=NO
'DCPY020  DSN cannot be blank, must be a data set name                         '
DCPY021  'Cannot be blank         ' .ALARM=NO
'DCPY021  Cannot be blank                                                      '
DCPY022  'Must be numeric         ' .ALARM=NO
'DCPY022  Must be numeric                                                      '
DCPY023  'Invalid value           ' .ALARM=NO
'DCPY023  Must be CYLINDER, BLOCK, or TRACK                                    '
DCPY024  'Invalid value           ' .ALARM=NO
'DCPY024                                                                       '
@@
