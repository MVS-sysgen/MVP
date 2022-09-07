//LISTDSJ JOB (JOB),
//             'INSTALL LISTDSJ',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*  Installs CLISTs
//*
//CLISTS   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.CMDPROC,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CFLDSI
PROC 0 DD()

/***************************************************************/
/* CLIST:  CFLDSI                                              */
/*                                                             */
/* Description                                                 */
/* -----------                                                 */
/* List session allocated datasets using LISTA and LISTDSI     */
/*                                                             */
/* More information at:                                        */
/*  https://ShareABitOfIT.net/LISTDSJ-for-mvs-3-8j/            */
/*                                                             */
/*                                                             */
/* Syntax                                                      */
/* -------                                                     */
/* CFLDSI DD(xxxxxxxx)                                         */
/*    DD - optional, DD name                                   */
/*         o when NOT specificed, all TSO session allocated    */
/*           DD names and associated DSNs are listed           */
/*         o when specified, TSO session allocated DSNs        */
/*           and dataset attributes are listed for the         */
/*           specified DD                                      */
/*                                                             */
/*                                                             */
/* Example to list session allocated files                     */
/* ---------------------------------------                     */
/* CFLDSI                                                      */
/*                                                             */
/* Sample Output: (partial listing)                            */
/*                                                             */
/* ***** STEPLIB  ISP.V2R1M0.LLIB                              */
/* *****          LARRY01.REVIEW.R49M8.LOAD                    */
/* ***** SYS00004 SYS1.UCAT.TSO                                */
/* ***** SYS00002 SYS1.UCAT.MVS                                */
/* ***** SYSUDUMP JES2.TSU00038.SO0101                         */
/* ***** SYSABEND JES2.TSU00038.SO0102                         */
/* ***** SYSDBOUT JES2.TSU00038.SO0103                         */
/* ***** SYSHELP  SYS1.HELP                                    */
/* *****          SYS2.HELP                                    */
/* *****          LARRY01.REVIEW.R49M7.HELP                    */
/* ***** SYSPROC  LARRY01.CMDPROC                              */
/* *****          SYS1.CMDPROC                                 */
/* *****          SYS2.CMDPROC                                 */
/* *****          LARRY01.REVIEW.R45M0.CLIST                   */
/* *>>** SYSOUT   TERMFILE                                     */
/* *>>** DISPLAY  TERMFILE                                     */
/* .                                                           */
/* .                                                           */
/* ***** LOGIT    LARRY01.LOGIT                                */
/* LINES PROCESSED:163                                         */
/* ***                                                         */
/*                                                             */
/*                                                             */
/*                                                             */
/*                                                             */
/* Example to list session allocated files information         */
/* for DD SYSHELP                                              */
/* ---------------------------------------------------         */
/*                                                             */
/* CFLDSI DD(SYSHELP)                                          */
/*                                                             */
/* Sample Output:                                              */
/*                                                             */
/* *$$** DDNAME   VOLUME LRECL BLKSZ DATASETNAME               */
/* *$$** SYSHELP  MVSRES 00080 19040 SYS1.HELP                 */
/* *$$**          PUB000 00080 19040 SYS2.HELP                 */
/* *$$**          PUB005 00080 06480 LARRY01.REVIEW.R49M7.HELP */
/* LINES PROCESSED:163                                         */
/* ***                                                         */
/*                                                             */
/*                                                             */
/*                                                             */
/*  Disclaimer: <DSCLAIMR>                                     */
/*  ========================================================== */
/*                                                             */
/*  No guarantee; No warranty; Install / Use at your own risk. */
/*                                                             */
/*  This software is provided "AS IS" and without any expressed*/
/*  or implied warranties, including, without limitation, the  */
/*  implied warranties of merchantability and fitness for a    */
/*  particular purpose.                                        */
/*                                                             */
/*                                                             */
/***************************************************************/

/***************************************************************/
/* CLIST CONTROL statement                                     */
/***************************************************************/
CONTROL MSG

/***************************************************************/
/* Execute LISTALC STATUS and trap OUTPUT                      */
/***************************************************************/
SET LLIMIT     = 1000                           /* Line Limit  */
SET SYSOUTTRAP = &LLIMIT                        /* TRAP Limit  */

LISTALC STATUS                                  /* LISTALC     */

SET CRC = &LASTCC                               /* CP RC       */
SET SYSOUTTRAP = 0                              /* Reset TRAP  */
SET MAXLINE = &SYSOUTLINE                       /* Total Lines */
SET NUM = 1                                     /*             */
SET XX = 0                                      /* Heading Flag*/

/***************************************************************/
/* Check for no SYSOUT or CP does not use TPUT                 */
/***************************************************************/
IF &MAXLINE = 0 THEN -
  DO
    WRITE NO OUTPUT DETECTED FROM COMMAND
    EXIT
  END

/***************************************************************/
/* Check for excess SYSOUT from CP                             */
/***************************************************************/
IF &MAXLINE > &LLIMIT THEN -
  DO
    WRITE EXCEEDED CP SYSOUT OF &MAXLINE BY &MAXLINE-&LLIMIT LINES
    EXIT
  END

/***************************************************************/
/* Process captured output                                     */
/***************************************************************/
DO WHILE &NUM ¬> &MAXLINE
  SET SYSSCAN = 0
  SET HVAL1 = &SYSOUTLINE
  SET SYSSCAN = 1
  SET HVAL2 = &STR(&HVAL1&NUM)
  SET SYSSCAN = 16

  SET COL01 = &STR(&HVAL2)                      /* Output line */

  /*************************************************************/
  /* SKIP Null lines                                           */
  /*************************************************************/
  IF &LENGTH(&STR(&COL01)) = 0 THEN GOTO SKIPME
  /*************************************************************/
  /* SKIP Heading lines fro LISTA                              */
  /*************************************************************/
  IF &SUBSTR(1:2,&STR(&COL01)) = &STR(--) THEN +
    GOTO SKIPME

  /*************************************************************/
  /* DSN line if BYTE 1 > ' '                                  */
  /*************************************************************/
  IF &SUBSTR(1:1,&STR(&COL01)) > &STR( ) THEN +
    DO
      SET DSNL = &LENGTH(&STR(&COL01))
      SET DSN  = &SUBSTR(1:&DSNL,&COL01)
    END

  /*************************************************************/
  /* Assume DD  line if BTYE 1 = ' '                           */
  /*  Otherwise, must be TERMFILE or similar                   */
  /*************************************************************/
  IF &SUBSTR(1:1,&STR(&COL01)) = &STR( ) THEN +
    DO
      SET DDN = &SUBSTR(3:10,&COL01)
      IF &DDN > &STR( ) THEN +
        SET DDH = &DDN
      IF &DD = &STR() THEN +
        WRITE ***** &DDN &DSN
      ELSE +
        IF &DD = &DDH THEN +
          DO
            IF &XX = 0 THEN +
              DO
                WRITE *$$** DDNAME   VOLUME LRECL BLKSZ DatasetName
                SET XX = &XX +1
              END
            LISTDSI '&DSN'
            SET RC=&LASTCC
            WRITE *$$** &DDN &SYSVOLUME &SYSLRECL &SYSBLKSIZE &DSN
          END
    END
  ELSE +
    IF &LENGTH(&STR(&COL01)) > 10 THEN +
      IF &SUBSTR(9:10,&STR(&COL01)) = &STR(  ) THEN +
        DO
          SET DSN = &SUBSTR(1:08,&COL01)
          SET DDN = &SUBSTR(11:18,&COL01)
          IF &DD = &STR() THEN +
            WRITE *>>** &DDN &DSN
          ELSE +
            IF &DD = &DDN THEN +
              WRITE *>>** &DDN &DSN
        END

  SKIPME: +
  SET NUM = &NUM + 1

END

/***************************************************************/
/* Done!                                                       */
/***************************************************************/
WRITE LINES PROCESSED:&NUM
EXIT CODE(0)


./ ADD NAME=CLDSI2
PROC 0 DSN()

/*********************************************************************/
/* CLIST:  CLDSI2                                                    */
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
/*  DATA SET NAME: LARRY01.TEST.CNTL                                 */
/*      ON VOLUME: PUB000  (3350)                                    */
/*      DSORG: PO     RECFM: FB                                      */
/*      LRECL: 80    BLKSIZE: 19040    KEYL: 0                       */
/*      3 CYLINDER(S) ALLOCATED, 2 CYLINDER(S) USED                  */
/*      ALLOCATION, PRIMARY: 3    SECONDARY: 1                       */
/*        IN 3 EXTENTS                                               */
/*      CREATED: 2013-068    REFERENCED: 2020-110                    */
/*      EXPIRATION DATE: ***NONE***                                  */
/*      PASSWORD: NONE , RACF PROTECTION: NONE                       */
/*      DATA SET CHANGED: Y                                          */
/*      THIS UNIT HAS 30 TRACKS PER CYLNDER                          */
/*               AND 1 BLOCKS PER TRACK                              */
/*                                                                   */
/*                                                                   */
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
IF &RC NE 0 THEN -
  DO
    SET WLINE = &STR(MSG: &SUBSTR(1:71,&SYSLISTDSJMSG)...)
    WRITE &WLINE
    SET WLINE = &STR(RETURN CD: &RC   &STR(&SYSMSGLVL1))
    WRITE &WLINE
    SET WLINE = &STR(SYSREASON: &SYSREASON   )
    SET WLINE = &STR(&WLINE&STR(&SYSMSGLVL2))
    WRITE &WLINE
  END

SET SYSUNIT  = &SYSUNIT
SET SYSLRECL = &SYSLRECL
SET &SYSBLKSIZE = &SYSBLKSIZE
SET &SYSKEYLEN = &SYSKEYLEN
SET &SYSALLOC = &SYSALLOC
SET &SYSUSED = &SYSUSED
SET &SYSPRIMARY = &SYSPRIMARY
SET &SYSSECONDS = &SYSSECONDS
SET &SYSEXTENTS = &SYSEXTENTS
SET &SYSTRKSCYL = &SYSTRKSCYL
SET &SYSBLKSTRK = &SYSBLKSTRK

IF &STR(&SYSEXDATE) = &STR(0000-000) THEN -
  SET &SYSEXDATE = &STR(***NONE***)

 WRITE Data Set Name: &SYSDSNAME
 WRITE     on Volume: &SYSVOLUME  (&SYSUNIT)
 WRITE     DSORG: &SYSDSORG    RECFM: &SYSRECFM
 WRITE     LRECL: &SYSLRECL    BLKSIZE: &SYSBLKSIZE    KEYL: &SYSKEYLEN
 WRITE     &SYSALLOC &SYSUNITS(S) Allocated, &SYSUSED &SYSUNITS(S) Used
 WRITE     Allocation, primary: &SYSPRIMARY    secondary: &SYSSECONDS
 WRITE       In &SYSEXTENTS Extents
 WRITE     Created: &SYSCREATE    Referenced: &SYSREFDATE
 WRITE     Expiration Date: &SYSEXDATE
 WRITE     Password: &SYSPASSWORD, RACF Protection: &SYSRACFA
 WRITE     Data Set changed: &SYSUPDATED
 WRITE     This Unit has &SYSTRKSCYL Tracks per Cylnder
 WRITE              and &SYSBLKSTRK Blocks per Track

 IF &SYSADIRBLK NE &STR(?????)  THEN -
   DO
     WRITE     &SYSADIRBLK Directory Blocks allocated
     WRITE     &SYSUDIRBLK Directory Blocks used
     WRITE     &SYSMEMBERS Members
   END

/*********************************************************************/
/* Done!                                                             */
/*********************************************************************/
EXIT CODE(0)


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


./ ADD NAME=CLISTDSJ
PROC 1 LTYP PRMS()

/********************************************************************/
/*                                                                  */
/*    CLIST: CLISTDSJ                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitOfIT.net/LISTDSJ-for-mvs-3-8j/          */
/*         Copyright (C) 2019-2021  Larry Belmontes, Jr.            */
/*                                                                  */
/*                                                                  */
/* DESCRIPTION:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* Test harness to display symbolic variable names and values       */
/* returned from LISTDSJ in one of three formats:                   */
/*   1 - TWO COLUMN LIST                                            */
/*   2 - CONDENSED FORM                                             */
/*   3 - GROUPED FORM  (3270-LIKE)                                  */
/*                                                                  */
/* NOTE: Do not use PNL option.  CLIST will terminate abnormally.   */
/*       This CLIST can only display &SYS* variable names.          */
/*                                                                  */
/*                                                                  */
/* COMMAND SYNTAX:                                                  */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* CLISTDSJ TYPE PRMS('xxxx...xx')                                  */
/*                                                                  */
/*   TYPE (REQUIRED) DISPLAY LAYOUT TYPE                            */
/*         1 - TWO COLUMN LIST                                      */
/*         2 - CONDENSED FORM                                       */
/*         3 - GROUPED FORM                                         */
/*   PRMS (REQUIRED) FULLY QUALIFIED DATASET NAME followed          */
/*                   by LISTDSJ valid parameters.  PRMS content     */
/*                   is enclosed with quotes.                       */
/*   Example:                                                       */
/*   CLISTDSJ 1 PRMS('''herc01.test.cntl'' dir')                    */  LB1010
/*                                                                  */
/*                                                                  */
/* Disclaimer: <DSCLAIMR>                                           */
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
/* CHANGE HISTORY:                                                  */
/* ---------------------------------------------------------------  */
/* MM/DD/CCYY VERSION  NAME / DESCRIPTION                           */
/* 02/27/2021 1.0.40   LARRY BELMONTES JR.                          */  LB1040
/*                     ADD SYSUPDATED to CLIST                      */  LB1040
/*                                                                  */  LB1040
/* 09/30/2019 1.0.10   LARRY BELMONTES JR.                          */  LB1010
/*                     ADD SYSALLOC to CLIST                        */  LB1010
/*                                                                  */  LB1010
/* 04/29/2019 1.0.00   LARRY BELMONTES JR.                          */
/*                     INITIAL VERSION RELEASED TO MVS 3.8JHOBBYIST */
/*                     PUBLIC DOMAIN                                */
/*                                                                  */
/********************************************************************/

SET L00 =&STR(--- CLISTDSJ LIST TYPE '&LTYP')

/*********************************************/
/* PRINT SOME LINES AND PRMS CONTENT         */
/*********************************************/
IF &LTYP EQ X THEN -
  DO
    WRITE
    WRITE *********************************************************
    IF &LENGTH(&PRMS) > 0 THEN -
      DO
       WRITE * * * &PRMS
       WRITE *********************************************************
      END
    WRITE
    GOTO ENDIT
  END

/*********************************************/
/* CHECK FOR VALID LIST TYPES                */
/*********************************************/
IF &LTYP GE 1 AND &LTYP LE 3 THEN -
  WRITE &L00
ELSE -
  DO
    WRITE &L00
    WRITE LIST TYPE '&LTYP' IS INVALID
    WRITE 1 - LIST OF SYMBOLICS AND VALUES
    WRITE 2 - LIST OF ARRANGED SYMBOLICS AND VALUES
    WRITE 3 - LIST OF TITLES AND VALUES (3270-LIKE)
    GOTO ENDIT
  END

/*********************************************/
/* INVOKE LISTDSJ                            */
/*********************************************/
LISTDSJ &PRMS
SET RC=&LASTCC
WRITE --- LISTDSJ RC=&RC

/*********************************************/
/* DISPLAY RESULTS BASED ON LIST TYPE        */
/*********************************************/
IF &RC = 0 OR &RC = 4 OR &RC = 8 OR &RC = 16 THEN -
  DO
    IF &LTYP EQ 1 THEN -
      DO
/*   1 - TWO COLUMN LIST                                            */
        WRITE 00. SYSDSNAME      ='&SYSDSNAME'
        WRITE 01. SYSLISTDSJMSG  ='&SYSLISTDSJMSG'
        WRITE 02. SYSVOLUME      ='&SYSVOLUME'
        WRITE 03. SYSUNIT        ='&SYSUNIT'
        WRITE 04. SYSDSORG       ='&SYSDSORG'
        WRITE 05. SYSRECFM       ='&SYSRECFM'
        WRITE 06. SYSLRECL       ='&SYSLRECL'
        WRITE 07. SYSBLKSIZE     ='&SYSBLKSIZE'
        WRITE 08. SYSKEYLEN      ='&SYSKEYLEN'
        WRITE 09. SYSKEYPOS      ='&SYSKEYPOS'
        WRITE 10. SYSSECONDS     ='&SYSSECONDS'
        WRITE 11. SYSUNITS       ='&SYSUNITS'
        WRITE 12. SYSEXTENTS     ='&SYSEXTENTS'
        WRITE 13. SYSCREATE      ='&SYSCREATE'
        WRITE 14. SYSJCREATE     ='&SYSJCREATE'
        WRITE 15. SYSCCREATE     ='&SYSCCREATE'
        WRITE 16. SYSREFDATE     ='&SYSREFDATE'
        WRITE 17. SYSJREFDATE    ='&SYSJREFDATE'
        WRITE 18. SYSCREFDATE    ='&SYSCREFDATE'
        WRITE 19. SYSEXDATE      ='&SYSEXDATE'
        WRITE 20. SYSJEXDATE     ='&SYSJEXDATE'
        WRITE 21. SYSCEXDATE     ='&SYSCEXDATE'
        WRITE 22. SYSPASSWORD    ='&SYSPASSWORD'
        WRITE 23. SYSRACFA       ='&SYSRACFA'
        WRITE 24. SYSTRKSCYL     ='&SYSTRKSCYL'
        WRITE 25. SYSTRKLEN      ='&SYSTRKLEN'
        WRITE 26. SYSBLKSTRK     ='&SYSBLKSTRK'
        WRITE 27. SYSADIRBLK     ='&SYSADIRBLK'
        WRITE 28. SYSUDIRBLK     ='&SYSUDIRBLK'
        WRITE 29. SYSNUDIRBLK    ='&SYSNUDIRBLK'
        WRITE 30. SYSMEMBERS     ='&SYSMEMBERS'
        WRITE 31. SYSMEMBERSALIAS='&SYSMEMBERSALIAS'
        WRITE 32. SYSREASON      ='&SYSREASON'
        WRITE 33. SYSMSGLVL1     ='&SYSMSGLVL1'
        WRITE 34. SYSMSGLVL2     ='&SYSMSGLVL2'
        WRITE 35. SYSCYLVOL      ='&SYSCYLVOL'
        WRITE 36. SYSTRKSALLOC   ='&SYSTRKSALLOC'
        WRITE 37. SYSTRKSUSED    ='&SYSTRKSUSED'
        WRITE 38. SYSTRKSUNUSED  ='&SYSTRKSUNUSED'
        WRITE 39. SYSUNITCAP     ='&SYSUNITCAP'
        WRITE 40. SYSNUMVOLS     ='&SYSNUMVOLS'
        WRITE 41. SYSDSCAT       ='&SYSDSCAT'
        WRITE 42. SYSDSCATV      ='&SYSDSCATV'
        WRITE 43. SYSPRIMARY     ='&SYSPRIMARY'
        WRITE 44. SYSUSED        ='&SYSUSED'
        WRITE 45. SYSVOLUMES     ='&SYSVOLUMES'
        /* V1.0.10 */
        WRITE 46. SYSALLOC       ='&SYSALLOC'
        /* V1.0.40 */
        WRITE 47. SYSUPDATED     ='&SYSUPDATED'
      END
    ELSE -
    IF &LTYP EQ 2 THEN -
      DO
/*   2 - CONDENSED FORM                                             */
        WRITE SYSLISTDSJMSG='&SYSLISTDSJMSG'
        SET WLINE = &STR(RC='&RC')
        SET WLINE = &STR(&WLINE SYSMSGLVL1='&SYSMSGLVL1'
        WRITE &WLINE
        SET WLINE = &STR(SYSREASON='&SYSREASON')
        SET WLINE = &STR(&WLINE SYSMSGLVL2='&SYSMSGLVL2'
        WRITE &WLINE
        WRITE SYSDSNAME='&SYSDSNAME'

        SET WLINE = &STR(SYSDSORG='&SYSDSORG')
        SET WLINE = &STR(&WLINE SYSRECFM='&SYSRECFM')
        SET WLINE = &STR(&WLINE SYSLRECL='&SYSLRECL')
        SET WLINE = &STR(&WLINE SYSBLKSIZE='&SYSBLKSIZE')
        WRITE &WLINE

        SET WLINE = &STR(SYSKEYLEN='&SYSKEYLEN')
        SET WLINE = &STR(&WLINE SYSKEYPOS='&SYSKEYPOS')
        SET WLINE = &STR(&WLINE SYSPASSWORD='&SYSPASSWORD')
        SET WLINE = &STR(&WLINE SYSRACFA='&SYSRACFA')
        WRITE &WLINE

        SET WLINE = &STR(SYSJCREATE='&SYSJCREATE')
        SET WLINE = &STR(&WLINE SYSJEXDATE='&SYSJEXDATE')
        SET WLINE = &STR(&WLINE SYSJREFDATE='&SYSJREFDATE')
        WRITE &WLINE

        SET WLINE = &STR(SYSCREATE='&SYSCREATE')
        SET WLINE = &STR(&WLINE SYSEXDATE='&SYSEXDATE')
        SET WLINE = &STR(&WLINE SYSREFDATE='&SYSREFDATE')
        WRITE &WLINE

        SET WLINE = &STR(SYSCCREATE='&SYSCCREATE')
        SET WLINE = &STR(&WLINE SYSCEXDATE='&SYSCEXDATE')
        SET WLINE = &STR(&WLINE SYSCREFDATE='&SYSCREFDATE')
        WRITE &WLINE

        SET WLINE = &STR(SYSTRKLEN='&SYSTRKLEN')
        SET WLINE = &STR(&WLINE SYSBLKSTRK='&SYSBLKSTRK')
        WRITE &WLINE

        SET WLINE = &STR(SYSVOLUME='&SYSVOLUME')
        SET WLINE = &STR(&WLINE SYSUNIT='&SYSUNIT')
        SET WLINE = &STR(&WLINE SYSCYLVOL='&SYSCYLVOL')
        SET WLINE = &STR(&WLINE SYSTRKSCYL='&SYSTRKSCYL')
        WRITE &WLINE

        SET WLINE = &STR(SYSUNITS='&SYSUNITS')
        SET WLINE = &STR(&WLINE SYSSECONDS='&SYSSECONDS')
        SET WLINE = &STR(&WLINE SYSEXTENTS='&SYSEXTENTS')
        WRITE &WLINE

        SET WLINE = &STR(SYSTRKSALLOC='&SYSTRKSALLOC')
        SET WLINE = &STR(&WLINE SYSTRKSUSED='&SYSTRKSUSED')
        SET WLINE = &STR(&WLINE SYSTRKSUNUSED='&SYSTRKSUNUSED')
        WRITE &WLINE

        SET WLINE = &STR(SYSADIRBLK='&SYSADIRBLK')
        SET WLINE = &STR(&WLINE SYSUDIRBLK='&SYSUDIRBLK')
        SET WLINE = &STR(&WLINE SYSNUDIRBLK='&SYSNUDIRBLK')
        WRITE &WLINE

        SET WLINE = &STR(SYSMEMBERS='&SYSMEMBERS')
        SET WLINE = &STR(&WLINE SYSMEMBERSALIAS='&SYSMEMBERSALIAS')
        WRITE &WLINE

        SET WLINE = &STR(SYSUNITCAP='&SYSUNITCAP')
        SET WLINE = &STR(&WLINE SYSNUMVOLS='&SYSNUMVOLS')
        SET WLINE = &STR(&WLINE SYSDSCAT='&SYSDSCAT')
        SET WLINE = &STR(&WLINE SYSDSCATV='&SYSDSCATV')
        WRITE &WLINE

        SET WLINE = &STR(SYSPRIMARY='&SYSPRIMARY')
        SET WLINE = &STR(&WLINE SYSUSED='&SYSUSED')
        WRITE &WLINE

        SET WLINE = &STR(SYSVOLUMES='&SYSVOLUMES')
        SET WLINE = &STR(&WLINE SYSALLOC='&SYSALLOC') /* V1.0.10 */
        SET WLINE = &STR(&WLINE SYSUPDATED='&SYSUPDATED') /* V1.0.40 */
        WRITE &WLINE
      END
    ELSE -
    IF &LTYP EQ 3 THEN -
      DO
/*   3 - GROUPED FORM  (3270-LIKE)                                  */
        SET WLINE = &STR(MSG: &SUBSTR(1:71,&SYSLISTDSJMSG)...)
        WRITE &WLINE

        SET WLINE = &STR(RETURN CD: &RC   &STR(&SYSMSGLVL1))
        WRITE &WLINE

        SET WLINE = &STR(SYSREASON: &SYSREASON   )
        SET WLINE = &STR(&WLINE&STR(&SYSMSGLVL2))
        WRITE &WLINE

        WRITE

        SET WLINE = &STR(&SYSDSNAME)
        WRITE &WLINE

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
        SET WLINE = &STR(&WLINE    &SYSUPDATED)   /* 1.0.40 */
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
        SET WLINE = &STR(&WLINE     SEC        ALLOC) /* V1.0.10 */
        WRITE &WLINE

        SET WLINE = &STR(        &SYSVOLUME)
        SET WLINE = &STR(&WLINE  &SYSUNITS)
        SET WLINE = &STR(&WLINE &SYSPRIMARY)     /* V1.0.10 */
        SET WLINE = &STR(&WLINE   &SYSUSED)
        SET WLINE = &STR(&WLINE    &SYSSECONDS)
        SET WLINE = &STR(&WLINE    &SYSALLOC)    /* V1.0.10 */
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

  END
ELSE -
  WRITE --- LISTDSJ BYPASSED VALUE LISTING DUE TO RETURN CODE (&RC).

ENDIT: -
END

EXIT CODE(0)
@@
//*
//*  Installs HELP
//*
//HELP     EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.HELP,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CFLDSI
)F FUNCTION -
  The CFLDSI CLIST displays session allocations using LISTALC and
  LISTDSI.

)X SYNTAX  -
         CFLDSI DD(xxxx)

  REQUIRED - NONE
  DEFAULTS - NONE
  ALIAS    -
)O OPERANDS -

))DD       - optional, DD (data definition name) to be listed


  Sample Commands

  1) CFLDSI
     Displays all session allocated DDs w/ associated DSNs

  2) CFLDSI DD(ISPCLIB)
     Displays allocated datasets ONLY for DD ISPCLIB including some
     dataset attributes

./ ADD NAME=CLDSI2
)F FUNCTION -
  The CLDSI2 CLIST displays dataset attributes using LISTDSI.

)X SYNTAX  -
         CLDSI2 DSN(xxxx)

  REQUIRED - DSN
  DEFAULTS - NONE
  ALIAS    -
)O OPERANDS -

))DSN      - DSN (dataset name)
             DSN in quotes is fully qualified, otherwise, prefix DSN
             with USERID


  Sample Commands

  1) CLDSI2 DSN('''HERC01.TEST.CNTL''')
     Displays dataset attributes for 'HERC01.TEST.CNTL'

  2) CLDSI2 DSN(TEST.CNTL)
     Displays dataset attributes for 'userid.TEST.CNTL'

./ ADD NAME=CLDSI
)F FUNCTION -
  The CLDSI CLIST displays dataset attributes using LISTDSI.

)X SYNTAX  -
         CLDSI DSN(xxxx)

  REQUIRED - DSN
  DEFAULTS - NONE
  ALIAS    -
)O OPERANDS -

))DSN      - DSN (dataset name)
             DSN in quotes is fully qualified, otherwise, prefix DSN
             with USERID


  Sample Commands

  1) CLDSI DSN('''HERC01.TEST.CNTL''')
     Displays dataset attributes for 'HERC01.TEST.CNTL'

  2) CLDSI DSN(TEST.CNTL)
     Displays dataset attributes for 'userid.TEST.CNTL'

./ ADD NAME=LISTDSJ
)F FUNCTION -
  The LISTDSJ command sets CLIST variables for dataset
  attributes and other information.
)X SYNTAX  -
         LISTDSJ NAME VOL(vvvvvv) DIR PNL ABOUT DS(x) DF(n)             LB1010
                 FILE DLn                                               LB1010
  REQUIRED - NAME
  DEFAULTS - NONE
  ALIAS    - LISTDSI                                                    LB1020
)O OPERANDS -
  Parameters can appear in any order except NAME which must be the
  first parameter
))NAME     - DSN (dataset name) or DDNAME (see FILE keyword)            LB1010
             DSN in quotes is fully qualified, otherwise, prefix DSN    LB1010
             with USERID                                                LB1010
))VOLUME   - optional, target volume for data set name
  or VOL     bypassing catalog search.                                  LB1010
))DIRECTORY- optional, retrieve PDS directory information.              LB1010
  or DIR     Ignored when file is not Partitioned.                      LB1010
             is not Partitioned.
             NOTE: Last Referenced Date is updated when file open.
))PNL      - CLIST symbolic names use short format, &Snn.
             See Table 3, column 'S#' in program documentation
             for name specifics.
))ABOUT    - optional, display LISTDSJ date time stamp on TSO
             session screen.
             NOTE: A data set name is required as the first parameter   LB1040
             followed by the ABOUT keyword.  No dataset information     LB1040
             is retrieved.                                              LB1040
             All CLIST variables will contain default values of
             question marks (?).
))DS       - optional, date separator to be used for dates              LB1001a
             Note: default value is DS(S) slash                         LB1001a
             DS(D) use dash   (-)                                       LB1001a
             DS(P) use period (.)                                       LB1001a
             DS(S) use slash  (/)  DEFAULT <--                          LB1001a
))DF       - optional, date format layout for MDY type dates            LB1001b
             Note: default value is DF(3)    YYYY_MM_DD                 LB1001b
             DF(1) use date format MM_DD_CCYY                           LB1001b
             DF(2) use date format DD_MM_CCYY                           LB1001b
             DF(3) use date format YYYY_MM_DD  DEFAULT <--              LB1001b
             DF(4) use date format YYYY_DD_MM                           LB1001b
))FILE     - optional, use NAME as a DDNAME for an allocated dataset    LB1010
))DLn      - optional, create PDS directory list with ISPF statistics   LB1001h
             Note: ISPLINK must be available to utilize ISPF table      LB1001h
                   services.  Otherwise, this parameter is ignored.     LB1001h
             DL0   ISPF table name DIRLST0 is created                   LB1001h
             DL1   ISPF table name DIRLST1 is created                   LB1001h
))TSOB      - optional, bypass test validation of executing under TSO.  LB1040c
             Used when executing under batch TSO.                       LB1040c


  Sample Commands

  1) LISTDSJ 'HERC01.TEST.CNTL' ABOUT                                   LB1010
     Displays LISTDSJ date time stamp on TSO session screen and
     sets all LISTDSJ SYS* CLIST variables to question marks (?).

  2) LISTDSJ 'HERC01.TEST.CNTL'                                         LB1010
     Dataset attributes and other information for HERC01.TEST.CNTL
     are set for each LISTDSJ SYS* CLIST variable excluding
     DIRECTORY information.

  3) LISTDSJ 'HERC01.TEST.CNTL' DIR                                     LB1010
     Dataset attributes and other information for HERC01.TEST.CNTL
     are set for each LISTDSJ SYS* CLIST variable including
     DIRECTORY information.
     Note: Last Referenced Date updated when file opened.

  4) LISTDSJ 'HERC01.TEST.CNTL' PNL                                     LB1010
     Dataset attributes and other information for HERC01.TEST.CNTL
     are set for each LISTDSJ Snn CLIST variable excluding
     DIRECTORY information.
     Note: Short name notation is used for this request (&Snn).

  5) LISTDSJ SYSPROC FILE                                               LB1010
     Dataset attributes and other information for dataset name          LB1010
     associated with DDNAME SYSPROC are set for each LISTDSJ SYS*       LB1010
     CLIST variables excluding DIRECTORY information.                   LB1010
                                                                        LB1010


CLIST Variables <CLSTVARS>
 +--+------------------+-------------------------------------------+
 |S#| CLIST VARIABLE   | CONTENT                                   |
 +--+------------------+-------------------------------------------+
 |  | LASTCC           | Return code from LISTDSJ available        |
 |  |                  | immediately after execution of LISTDSJ    |
 |  |                  | command                                   |
 +--+------------------+-------------------------------------------+
 |00| SYSDSNAME        | Dataset name                              |
 +--+------------------+-------------------------------------------+
 |01| SYSLISTDSJMSG    | LISTDSJ message variable                  |
 |  |                  | Miscellaneous processing message          |
 +--+------------------+-------------------------------------------+
 |02| SYSREASON        | LISTDSJ reason code                       |
 |  |                  | See REASON CODE table, Table 4            |
 |  |                  |     column REASON CODE                    |
 +--+------------------+-------------------------------------------+
 |03| SYSMSGLVL1       | First-level message                       |
 |  |                  | See RETURN CODE table, Table 2            |
 |  |                  |     column DESCRIPTION                    |
 +--+------------------+-------------------------------------------+
 |04| SYSMSGLVL2       | Second-level message                      |
 |  |                  | See REASON CODE table, Table 4            |
 |  |                  |     column DESCRIPTION                    |
 +--+------------------+-------------------------------------------+
 |05| SYSCREATE        | Creation Date  format CCYY/JJJ            |
 |  |                  | CCYY-century year, JJJ-julian date        |
 |  |                  | Note: Century windowed at 70.             |
 +--+------------------+-------------------------------------------+
 |06| SYSEXDATE        | Expiration Date  format CCYY/JJJ          |
 |  |                  | CCYY-century year, JJJ-julian date        |
 |  |                  | Note: Century windowed at 70.             |
 +--+------------------+-------------------------------------------+
 |07| SYSREFDATE       | Last Referenced Date  format CCYY/JJJ     |
 |  |                  | CCYY-century year, JJJ-julian date        |
 |  |                  | Note: Century windowed at 70.             |
 |  |                  | Referenced date will be updated with DIR  |
 |  |                  | option is used on a PDS request.          |
 +--+------------------+-------------------------------------------+
 |08| SYSDSORG         | Dataset Organization                      |
 |  |                  | IS  -ISAM                                 |
 |  |                  | PS  -Physical Sequential                  |
 |  |                  | DA  -Direct Access                        |
 |  |                  | PO  -Partition Organization               |
 |  |                  | U   -Undefined                            |
 |  |                  | VS  -VSAM                                 |
 |  |                  |                                           |
 |  |                  | xxU -Unmovable                            |
 +--+------------------+-------------------------------------------+
 |09| SYSRECFM         | Record format for non-VSAM datasets       |
 |  |                  | Character combination        e.g.  FBA    |
 |  |                  | U -Undefined                              |
 |  |                  | F -Fixed                                  |
 |  |                  | V -Variable                               |
 |  |                  | B -Blocked                                |
 |  |                  | T -Track Overflow                         |
 |  |                  | S -Spanned or Standard                    |
 |  |                  | A -ANSI                                   |
 |  |                  | M -Machine                                |
 +--+------------------+-------------------------------------------+
 |10| SYSLRECL         | Logical record length                     |
 +--+------------------+-------------------------------------------+
 |11| SYSBLKSIZE       | Block size                                |
 +--+------------------+-------------------------------------------+
 |12| SYSKEYLEN        | Key Length                                |
 +--+------------------+-------------------------------------------+
 |13| SYSKEYPOS        | Relative Key Position                     |
 +--+------------------+-------------------------------------------+
 |14| SYSPASSWORD      | Password Indication                       |
 |  |                  | NONE   No password required               |
 |  |                  | READ   Password required to READ          |
 |  |                  | R_W    Password required to READ and WRITE|
 +--+------------------+-------------------------------------------+
 |15| SYSRACFA         | RACF Indication                           |
 |  |                  | NONE                                      |
 |  |                  | RACF                                      |
 +--+------------------+-------------------------------------------+
 |16| SYSADIRBLK       | Directory Blocks Allocated                |
 |  |                  | Only for PDS when DIR option is specified |
 +--+------------------+-------------------------------------------+
 |17| SYSUDIRBLK       | Directory Blocks Used                     |
 |  |                  | Only for PDS when DIR option is specified |
 +--+------------------+-------------------------------------------+
 |18| SYSNUDIRBLK      | Directory Blocks Not Used                 |
 |  |                  | Only for PDS when DIR option is specified |
 +--+------------------+-------------------------------------------+
 |19| SYSMEMBERS       | Number of Members (total members)         |
 |  |                  | Only for PDS when DIR option is specified |
 +--+------------------+-------------------------------------------+
 |20| SYSMEMBERSALIAS  | Number of Alias Members (total within     |
 |  |                  |   SYSMEMBERS)                             |
 |  |                  | Only for PDS when DIR option is specified |
 +--+------------------+-------------------------------------------+
 |21| SYSVOLUME        | Volume serial ID                          |
 +--+------------------+-------------------------------------------+
 |22| SYSUNIT          | Device Unit on which volume resides       |
 |  |                  | 2305, 2305-2, 2314,                       |
 |  |                  | 3330, 3340, 3350, 3375, 3380, 3390        |
 +--+------------------+-------------------------------------------+
 |23| SYSUNITS         | Space Units                               |
 |  |                  | CYLINDER                                  |    LB1010c
 |  |                  | TRACK                                     |    LB1010c
 |  |                  | BLOCK                                     |    LB1010c
 |  |                  | ABSOLUTE                                  |    LB1010c
 +--+------------------+-------------------------------------------+
 |24| SYSTRKSUSED      | Total Tracks Used                         |
 +--+------------------+-------------------------------------------+
 |25| SYSEXTENTS       | Number of Extents allocated               |
 +--+------------------+-------------------------------------------+
 |26| SYSTRKSALLOC     | Total Tracks Allocated                    |
 +--+------------------+-------------------------------------------+
 |27| SYSSECONDS       | Secondary Allocation in Space Units       |
 +--+------------------+-------------------------------------------+
 |28| SYSTRKSUNUSED    | Total Tracks Unused                       |
 +--+------------------+-------------------------------------------+
 |29| SYSCYLVOL        | Cylinders for Volume                      |
 |  |                  | As stored in volume F4-DSCB               |
 +--+------------------+-------------------------------------------+
 |30| SYSTRKSCYL       | Tracks per Cylinder for SYSUNIT           |
 |  |                  | As stored in volume F4-DSCB               |
 +--+------------------+-------------------------------------------+
 |31| SYSJCREATE       | Creation Date  format YYJJJ               |
 |  |                  | YY-year, JJJ-julian date                  |
 |  |                  | as stored by MVS3.8J                      |
 +--+------------------+-------------------------------------------+
 |32| SYSJEXDATE       | Expiration Date  format YYJJJ             |
 |  |                  | YY-year, JJJ-julian date                  |
 |  |                  | as stored by MVS3.8J                      |
 +--+------------------+-------------------------------------------+
 |33| SYSJREFDATE      | Last Referenced Date  format YYJJJ        |
 |  |                  | YY-year, JJJ-julian date                  |
 |  |                  | as stored by MVS3.8J                      |
 |  |                  | Referenced date will be updated when DIR  |
 |  |                  | option is used on a PDS request.          |
 +--+------------------+-------------------------------------------+
 |S#| CLIST VARIABLE   | CONTENT                                   |
 +--+------------------+-------------------------------------------+
 |34| SYSTRKLEN        | Track Length        for SYSUNIT           |
 |  |                  | As stored in volume F4-DSCB               |
 +--+------------------+-------------------------------------------+
 |35| SYSUNITCAP       | Capacity for SYSUNIT                      |
 |  |                  | Computed as -                             |
 |  |                  | SYSCYLVOL * SYSTRKSCYL * SYSTRKLEN        |
 +--+------------------+-------------------------------------------+
 |36| SYSBLKSTRK       | Blocks per Track for SYSUNIT              |
 |  |                  | Computed as -                             |
 |  |                  | SYSTRKLEN / SYSBLKSIZE                    |
 +--+------------------+-------------------------------------------+
 |37| SYSCCREATE       | Creation Date  format CCYY/MM/DD          |    
 |  |                  | MM-month, DD-day, CCYY-century year       |
 |  |                  | Note: See DF parm to vary date layout     |  
 +--+------------------+-------------------------------------------+
 |38| SYSCEXDATE       | Expiration Date  format CCYY/MM/DD        |  
 |  |                  | MM-month, DD-day, CCYY-century year       |
 |  |                  | Note: See DF parm to vary date layout     |  
 +--+------------------+-------------------------------------------+
 |39| SYSCREFDATE      | Last Reference Date   format CCYY/MM/DD   |  
 |  |                  | MM-month, DD-day, CCYY-century year       |
 |  |                  | Note: See DF parm to vary date layout     |  
 |  |                  | Referenced date will be updated with DIR  |
 |  |                  | option is used on a PDS request.          |
 +--+------------------+-------------------------------------------+
 |40| SYSNUMVOLS       | Number of volumes for dataset             |
 |  |                  | As stored in catalog                      |
 +--+------------------+-------------------------------------------+
 |41| SYSDSCAT         | Dataset catalog indication (Y/N)          |
 |  |                  | Y      Dataset is cataloged               |
 |  |                  | N      Dataset is not cataloged           |
 +--+------------------+-------------------------------------------+
 |42| SYSDSCATV        | Dataset cataloged volume serial ID        |
 |  |                  | Volume serial of dataset name found       |
 |  |                  | on catalog                                |
 +--+------------------+-------------------------------------------+
 |43| SYSPRIMARY       | Primary Allocation in Space units.        |
 +--+------------------+-------------------------------------------+
 |44| SYSUSED          | Allocation used, in Space units.          |
 +--+------------------+-------------------------------------------+
 |45| SYSVOLUMES       | List of volume serial numbers for         |
 |  |                  | single and multiple volume datasets.      |
 |  |                  | Limited to 5 volumes.                     |
 +--+------------------+-------------------------------------------+
 |46| SYSALLOC         | Allocation in Space units.                |    LB1010d
 +--+------------------+-------------------------------------------+    LB1010d
 |47| SYSUPDATED       | Dataset Backup Change Indication (Y/N)    |    LB1040a
 |  |                  | Y      Dataset updated since last backup  |    LB1040a
 |  |                  | N      Dataset not updated since last     |    LB1040a
 |  |                  |        backup                             |    LB1040a
 +--+------------------+-------------------------------------------+    LB1040a


@@
//MACLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=&&MACLIBS,UNIT=SYSDA,
//             DISP=(,PASS,DELETE),
//             SPACE=(TRK,(04,02,02)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=#DATEFMT
         MACRO                                                          LB2000a
&NAME    #DATEFMT  &DF                                                  LB1100b
.*****************************************************************      LB1100b
.*       Date Format Character Configuration                     *      LB1100b
.*****************************************************************      LB1100b
         MNOTE 'DATEFMT:  Valid values are:           '                 LB1100b
         MNOTE '          1=MM_DD_CCYY                '                 LB1100b
         MNOTE '          2=DD_MM_CCYY                '                 LB1100b
         MNOTE '          3=CCYY_MM_DD     default ** '                 LB1100b
         MNOTE '          4=CCYY_DD_MM                '                 LB1100b
         AIF   ('&DF' EQ '1').DF1                                       LB1100b
         AIF   ('&DF' EQ '2').DF2                                       LB1100b
         AIF   ('&DF' EQ '3').DF3                                       LB1100b
         AIF   ('&DF' EQ '4').DF4                                       LB1100b
.DFDFLT  MVI   DATEFMT,C'3'        Date Format    default value <---    LB1001f
         MNOTE '  Applied DEFAULT date format of CCYY_MM_DD'            LB1100b
         MEXIT                                                          LB1100b
.DF1     MVI   DATEFMT,C'1'        Date Format    1 MM_DD_CCYY          LB1010a
         MNOTE '  Applied date format MM_DD_CCYY'                       LB1100b
         MEXIT                                                          LB1100b
.DF2     MVI   DATEFMT,C'2'        Date Format    2 DD_MM_CCYY          LB1010a
         MNOTE '  Applied date format DD_MM_CCYY'                       LB1100b
         MEXIT                                                          LB1100b
.DF3     MVI   DATEFMT,C'3'        Date Format    3 CCYY_MM_DD          LB1010a
         MNOTE '  Applied date format CCYY_MM_DD'                       LB1100b
         MEXIT                                                          LB1100b
.DF4     MVI   DATEFMT,C'4'        Date Format    4 CCYY_DD_MM          LB1010a
         MNOTE '  Applied date format CCYY_DD_MM'                       LB1100b
         MEXIT                                                          LB1100b
         MEND                                                           LB2000a
./ ADD NAME=#DATESEP
         MACRO                                                          LB2000a
&NAME    #DATESEP  &DS                                                  LB1100b
.*****************************************************************      LB1100b
.*       Date Separator Character Configuration                  *      LB1100b
.*****************************************************************      LB1100b
         MNOTE 'DATESEP:  Valid values are:           '                 LB1100b
         MNOTE '          D=dash    (-)    default ** '                 LB1100b
         MNOTE '          P=period  (.)               '                 LB1100b
         MNOTE '          S=slash   (/)               '                 LB1100b
         AIF   ('&DS' EQ 'D').DSD                                       LB1100b
         AIF   ('&DS' EQ 'P').DSP                                       LB1100b
         AIF   ('&DS' EQ 'S').DSS                                       LB1100b
.DSDFLT  ANOP                                                           LB1100b
         MNOTE '  Applied DEFAULT date separator as DASH'               LB1100b
         MVI   DATESEP,C'-'        Date Separator default value <---    LB1001f
         MEXIT                                                          LB1100b
.DSD     ANOP                                                           LB1100b
         MNOTE '  Applied date separator as DASH'                       LB1100b
         MVI   DATESEP,C'-'        Date Separator - Dash                LB1010a
         MEXIT                                                          LB1100b
.DSP     ANOP                                                           LB1100b
         MNOTE '  Applied date separator as PERIOD'                     LB1100b
         MVI   DATESEP,C'.'        Date Separator . Period              LB1010a
         MEXIT                                                          LB1100b
.DSS     ANOP                                                           LB1100b
         MNOTE '  Applied date separator as SLASH'                      LB1100b
         MVI   DATESEP,C'/'        Date Separator / Slash               LB1010a
         MEXIT                                                          LB1100b
         MEND                                                           LB2000a
./ ADD NAME=#IPAL
         MACRO                                                          LB2000a
&NAME    #IPAL                                                          LB2000e
*     * /********************************************************/      LB2000e
*     * /* ISP Srv Parameter Address List            LISTDSJ    */      LB2000e
*     * /********************************************************/      LB2000e
PISPPALS DS    0F                  PAL start address                    LB2000e
PISPREQ  DS    CL4                 ISP Srv Request Type                 LB2000e
PISPEP   DS    F                   ISPLINK Entry Point                  LB2000e
PISPDCOL DS    F                   ISP Table Columns                    LB2000e
PISPTBN  DS    F                   ISP Table Name                       LB2000e
PISPDMSG DS    F                   ISP DJMSG                            LB2000e
PISPPALL EQU   *-PISPPALS          Length of Parameter Address List     LB2000e
         MEND                                                           LB2000a
./ ADD NAME=ISPFPL
         MACRO
         ISPFPL
         PUSH  PRINT
         PRINT GEN
**********************************************************************
*   Macro:  ISPFPL    V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
**********************************************************************
*        ISPF LINK Parameter Address List                            *
**********************************************************************
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
ISPLINK  DS    F                   Entry point for ISPLINK
ISPFP1   DS    A                   ISPLINK Parm 1
ISPFP2   DS    A                   ISPLINK Parm 2
ISPFP3   DS    A                   ISPLINK Parm 3
ISPFP4   DS    A                   ISPLINK Parm 4
ISPFP5   DS    A                   ISPLINK Parm 5
ISPFP6   DS    A                   ISPLINK Parm 6
ISPFP7   DS    A                   ISPLINK Parm 7
ISPFP8   DS    A                   ISPLINK Parm 8
ISPFP9   DS    A                   ISPLINK Parm 9
ISPFP10  DS    A                   ISPLINK Parm 10
*
*              END OF ISPFPL
         POP   PRINT
         MEND
./ ADD NAME=ISPFSRV
         MACRO
         ISPFSRV
         PUSH  PRINT
         PRINT GEN
**********************************************************************
*   Macro:  ISPFSRV   V1R0M00    07/30/2020
*   Add TBSAVE,ASIS   V1R0M01    11/30/2021   Larry Belmontes           LB1001
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
**********************************************************************
*        ISPF Services and keywords (kw)                             *
**********************************************************************
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
CONTROL  DC    CL8'CONTROL '               Service Control
VDEFINE  DC    CL8'VDEFINE '               Service VDEFINE
VDELETE  DC    CL8'VDELETE '               Service VDELETE
DISPLAY  DC    CL8'DISPLAY '               Service Display
SELECT   DC    CL8'SELECT  '               Service Select
BROWSE   DC    CL8'BROWSE  '               Service BROWSE
EDIT     DC    CL8'EDIT    '               Service EDIT
TBDISPL  DC    CL8'TBDISPL '               Service TBDISPL
TBCREATE DC    CL8'TBCREATE'               Service TBCREATE
TBERASE  DC    CL8'TBERASE '               Service TBERASE
TBADD    DC    CL8'TBADD   '               Service TBADD
TBMOD    DC    CL8'TBMOD   '               Service TBMOD
TBPUT    DC    CL8'TBPUT   '               Service TBPUT
TBGET    DC    CL8'TBGET   '               Service TBGET
TBSKIP   DC    CL8'TBSKIP  '               Service TBSKIP
TBDELETE DC    CL8'TBDELETE'               Service TBDELETE
TBTOP    DC    CL8'TBTOP   '               Service TBTOP
TBBOTTOM DC    CL8'TBBOTTOM'               Service TBBOTTOM
TBSORT   DC    CL8'TBSORT  '               Service TBSORT
TBEND    DC    CL8'TBEND   '               Service TBEND
TBCLOSE  DC    CL8'TBCLOSE '               Service TBCLOSE
TBEXIST  DC    CL8'TBEXIST '               Service TBEXIST
TBQUERY  DC    CL8'TBQUERY '               Service TBQUERY
TBSTATS  DC    CL8'TBSTATS '               Service TBSTATS
TBSCAN   DC    CL8'TBSCAN  '               Service TBSCAN
TBSARG   DC    CL8'TBSARG  '               Service TBSARG
TBSAVE   DC    CL8'TBSAVE  '               Service TBSAVE               LB1001
VGET     DC    CL8'VGET    '               Service VGET
VPUT     DC    CL8'VPUT    '               Service VPUT
GETMSG   DC    CL8'GETMSG  '               Service GETMSG
SETMSG   DC    CL8'SETMSG  '               Service SETMSG
ERRORS   DC    CL8'ERRORS  '               kw ERRORS
RETURN   DC    CL8'RETURN  '               kw RETURN
LOCK     DC    CL8'LOCK    '               kw LOCK
SAVE     DC    CL8'SAVE    '               kw SAVE
RESTORE  DC    CL8'RESTORE '               kw RESTORE
LINE     DC    CL8'LINE    '               kw LINE
PROFILE  DC    CL8'PROFILE '               kw PROFILE
SHARED   DC    CL8'SHARED  '               kw SHARED
ASIS     DC    CL8'ASIS    '               kw ASIS                      LB1001
CHAR     DC    CL8'CHAR    '               kw CHAR
ALL      DC    CL8'*       '               kw ALL (*)
NOWRITE  DC    CL8'NOWRITE '               kw NOWRITE
REPLACE  DC    CL8'REPLACE '               kw REPLACE
YES      DC    CL8'YES     '               kw YES
NO       DC    CL8'NO      '               kw NO
B        DC    CL8'        '               kw B as blanks
*
*              END OF ISPFSRV
         POP   PRINT
         MEND
./ ADD NAME=#ISTATS
         MACRO                                                          LB2000a
&NAME    #ISTATS                                                        LB2000a
*     * /********************************************************/      LB1010h
*     * /* ISPF Table column fields   DIRLSTn        LISTDSJ    */      LB1010h
*     * /********************************************************/      LB1010h
ISTATS   EQU   *                                                        LB1010h
IMEMBR   DS    CL8                 PDS Member Name                      LB1010h
ITTRP    DS    CL6                 PDS TTR                              LB1010h
IVVMM    DS    CL5                 VV.MM format                         LB1010h
ICDTE    DS    CL8                 Create Date mm-dd-yy                 LB1010h
IMDTE    DS    CL8                 Modify Date mm-dd-yy                 LB1010h
IMTIM    DS    CL8                 Modify Time hh:mm:ss                 LB1010h
IRCUR    DS    CL5                 ISPF Currrent Rcd Count              LB1010h
IRINT    DS    CL5                 ISPF Initial  Rcd Count              LB1010h
IRMOD    DS    CL5                 ISPF Records Modified                LB1010h
IUSRID   DS    CL7                 ISPF USERID                          LB1010h
ISTATSL  EQU   *-ISTATS            Length of STATS                      LB1010h
         MEND                                                           LB2000a
./ ADD NAME=LBISPL
         MACRO
&NAME    LBISPL &ENTRY,&PARMS,&VL
**********************************************************************
*   Macro:  LBISPL    V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
.*********************************************************************
.*       LBISPL ISPLINK,(PARM1,PARM2,PARM3,PARM4,PARM5),VL           *
.*********************************************************************
.*       This macro loads a parm address list using LA instruction   *
.*       and BALR to R15                                             *
.*       - Uses R1,R14,R15                                           *
.*       - Uses variables ISPFP1-ISPFP5                              *
.*********************************************************************
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
         LCLA  &A,&NPARMS
         LCLC  &B
&NPARMS  SETA  N'&PARMS
         AIF   (&A GT 5).TOOMANY
&A       SETA  1
.LP      ANOP
         AIF   (&A GT &NPARMS).BP1
         LA    1,&PARMS(&A)        Parm Addr List  &A
         ST    1,ISPFP&A
&A       SETA  &A+1
         AGO   .LP
.BP1     ANOP
         AIF   ('&VL' NE 'VL').BP2
         OI    ISPFP&NPARMS,X'80'  VL Mark
.BP2     ANOP
         LA    1,ISPFP1            R1 - Addr of PARMS
         L     15,&ENTRY           R15- Entry Point
         BALR  14,15               Branch to Entry Point
         AGO   .ENDME
.TOOMANY ANOP
         MNOTE 'More than 5 parms, max is 5!'
         AGO    .ENDME
.ENDME   ANOP
         MEND
./ ADD NAME=#MPAL
         MACRO                                                          LB2000a
&NAME    #MPAL                                                          LB2000d
*     * /********************************************************/      LB2000d
*     * /* Message Parameter Address List            LISTDSJ    */      LB2000d
*     * /********************************************************/      LB2000d
PMSGPALS DS    0F                  PAL start address                    LB2000d
PMSGREQ  DS    CL4                 Message Request Type                 LB2000d
PMSGNBR  DS    F                   Message Number                       LB2000d
PMSGT    DS    F                   Starting address to receive text     LB2000d
PMSGL    DS    F                   Length of area to receive text       LB2000d
PMSGTXTL DS    F                   Length of message text moved         LB2000d
PMSGPALL EQU   *-PMSGPALS          Length of Parameter Address List     LB2000d
         MEND                                                           LB2000a
@@
//* -------------------------------------------------------*
//* *  LISTDSJ for MVS3.8J TSO / Hercules                  *
//* *                                                      *
//* *  JOB: $INST04                                        *
//* *       Install LISTDSJ Programs                       *
//* *                                                      *
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
//         DD  DSN=&&MACLIBS,DISP=OLD 
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
//* *  Assemble Link-Edit LISTDSJ to SYS2.CMDLIB           *
//* -------------------------------------------------------*
//LISTDSJ EXEC   ASML,
//         PARM.LKED='MAP,LIST,LET,RENT,XREF,REUS,REFR'
//ASM.SYSIN DD DATA,DLM=@@
         TITLE 'LISTDSJ - Dataset Information for MVS3.8J / Hercules'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  ================================================================
*
*  LL        II   SSSSS   TTTTTTTT  DDDDDD     SSSSS   JJJJJJJJ
*  LL        II  SS   SS     TT     DD   DD   SS   SS     JJ
*  LL        II  SS          TT     DD    DD  SS          JJ
*  LL        II   SSSSS      TT     DD    DD   SSSSS      JJ
*  LL        II       SS     TT     DD    DD       SS     JJ
*  LL        II  SS   SS     TT     DD    DD  SS   SS  JJ JJ
*  LLLLLLLL  II   SSSSS      TT     DDDDDD     SSSSS    JJJ
*
*  ================================================================
*
*  Program: LISTDSJ
*
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/LISTDSJ-for-MVS-3-8J
*           Copyright (C) 2019-2021  Larry Belmontes, Jr.
*
*  Disclaimer: <DSCLAIMR>
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
         EJECT
*  Overview:
*  ================================================================
*
*     Inspired by LISTDS and other similar TSO CPs that display
*  dataset information and, in particular, the absence of a
*  LISTDSI-type TSO CP in MVS 3.8J, I started to focus on
*  creating one to fill this void!
*
*     Some weeks later, I finalized my functional and design
*  thoughts to embark in writing LISTDSJ for use under TSO and
*  ISPF 2.0 (Wally Mclaughlin's product) as my contribution to
*  the MVS 3.8J hobbyist community.
*
*
*     This program, LISTDSJ, accepts a data set name and provides
*  dataset information from various VTOC DSCBs including directory
*  information for PDS files via CLIST variables.  A full list of
*  CLIST (symbolic) variables is provided below in Table 3.
*
*     LISTDSJ is similar to LISTDSI introduced and enhanced after
*  MVS3.8J.  Some of the symbolic names used by LISTDSJ may not
*  use the same values of a current LISTDSI version or may not be
*  defined as in the current version of LISTDSI.
*
*     This program is written in IFOX00 Assembler (24-bit addressing)
*  using the MVS 3.8J Tur(n)key 3 Operating System.  Therefore, the
*  LISTDSJ functions are limited to the features (support) offered by
*  the above OS and/or documentation within this program.  For example,
*  PDS/E is not supported by the public domain MVS 3.8J Tur(n)key 3 OS.
*  Thus, not supported by LISTDSJ.
*
*     All CLIST variables are initialized to question marks (?)
*  and accordingly set to dataset attribute values as processed by
*  LISTDSJ.
*
*     Since MVS3.8J is not Y2K compliant, the dates stored in DSCBs
*  will be windowed at 70 by LISTDSJ.  For example, a date stored as
*  format YYJJJ (YY-year, JJJ-Julian date) of value 70002 and
*  converting to format of MM/DD/CCYY will be displayed as 01/02/1970.
*  Another example where the year is less than 70, say 04060 (format
*  YYJJJ) will be converted to 02/01/2004.  Leap year detection
*  is part of the date conversion process.
*
*
         EJECT
*  Overview:  (continued)
*  ================================================================
*
*     Dataset information provided are for datasets residing on a
*  single volume.
*
*     However, if a dataset residing on multiple volumes is requested,
*  a volume list (currently limited to 5 by LISTDSJ) will be provided
*  with reason code 19 and allocations for the FIRST volume only.       LB1040x
*  This is intentional.                                                 LB1040x
*
*     LISTDSJ was developed and tested using Volker Bandke's MVS CD -
*  MVS38J TK3 with Hercules 3.07 hosted on a Windows 10 Pro machine.
*
*     MVS38J TK4- update 8 (maintained by Juergen Winkelmann) includes
*  user-mod ZP60014.  After applying user-mod ZP60038, LISTDSJ was
*  installed and successfully validated with a CLIST harness under
*  TSO.
*
*     LISTDSJ only processes data set types (DSORG) of IS (ISAM),       LB1040x
*  PS (Sequential), PO (Partitioned), DA (Direct).                      LB1040x
*                                                                       LB1040x
*     If LISTDSJ retrieves information for a VSAM dataset, only limited LB1040c
*  variables are returned including SYSDSNAME, SYSVOLUME, SYSUNIT, and  LB1040c
*  SYSDSORG.  Reason code 12 will be raised with an RC of 4.            LB1040c
*                                                                       LB1040c
*     LISTDSJ does not support Data Facility Hierarchical Storage       LB1040x
*  Manager (DFHSM) and System Managed Storage (SMS) information as      LB1040x
*  not available on the public domain version of MVS3.8J.               LB1040x
*                                                                       LB1040x
*     LISTDSJ does not support RACF authority checking when opening     LB1040x
*  a dataset for attribute information.                                 LB1040x
*                                                                       LB1040x
*     LISTDSJ allocates, opens, reads directory, closes and frees a     LB1040x
*  partitioned data set (PO) only if the DIR parameter is specified.    LB1040x
*  The data set can be specified as a DSN or FILENAME.  Therefore,      LB1040x
*  a pre-allocated dataset may be prematurely (unintentionally) freed.  LB1040x
*                                                                       LB1040x
*                                                                       LB1040x
*  Enjoy LISTDSJ!
*  Larry Belmontes Jr.
*
*
         EJECT
*  Prerequisite: User Modifications <PREREQS>
*  ===================================================================
*
*     Two user-mods, ZP60014 and ZP60038, are REQUIRED to process
*  CLIST symbolic variables via the IKJCT441 API on MVS 3.8J before
*  using LISTDSJ.  Otherwise, LISTDSJ is rendered useless!
*
*     More information on the above user-mods can be obtained from
*  the following website:
*       http://www.prycroft6.com.au/vs2mods/
*
*     If the above user-mods are not installed, a system abend 806-4
*  will occur when LISTDSJ is executed.  Additionally, an error
*  message will be displayed on your TSO session screen:
*      'LISTDSJ 4098 -Cannot link to IKJCT441'
*
*     Check your system to determine if one or both user-mods are
*  required.  ZP60038 requires ZP60014.
*
*
*     Although not a prerequisite, ISPLINK is called for table services
*  to create PDS member lists via the LISTDSJ DLn option if ISPF is
*  installed on MVS38J such as ISPF 2.0 (Wally Mclaughlin's product).
*  If a member directory list option (DL0 or DL1) is requested
*  and MVS38J does not have ISPF installed, the option is ignored.
*  However, MVS38J will report a system abend 806-4 for program
*  ISPLINK when the LISTDSJ request is processed.  This is by design.
*
*
         EJECT
*  Command Syntax for LISTDSJ: <CMDSYNTX>
*  ================================================================
*
*  LISTDSJ NAME VOL(volser) DIR PNL ABOUT DS(x) DF(n)                   LB1001b
*          FILE DLn                                                     LB1010gh
*    where:
*
*    1) NAME, required, dataset or DD name as the FIRST parameter       LB1010e
*            in the command line.  Dataset name is fully qualified      LB1010e
*            when enclosed in single quotes.  Dataset name without      LB1010e
*            quotes will be prefixed with USERID.                       LB1010e
*            Use FILE or NAME to specify dataset name.                  LB1010h
*
*    2) VOLUME or VOL, optional, target VOLUME for data set name        LB1010b
*            bypassing catalog search.
*
*    3) DIRECTORY or DIR, optional, retrieve directory information.     LB1010b
*            This option will be ignored when requested file
*            is not Partitioned.
*            Additionally, a message will be set in DJMSG as-
*            'DIR option not valid, ignored'
*            NOTE: Last Referenced Date updated when file open.
*
*    4) PNL, optional, CLIST symbolic names use short format,
*            &Snn.
*            See Table 3, column 'S#' for name specifics.
*
*    5) ABOUT, optional, display LISTDSJ date time stamp on TSO
*            session screen.
*            NOTE: Although a dataset name must be specified as
*                  the first parameter before the ABOUT keyword,
*                  no dataset information is retrieved.  All CLIST
*                  variables will contain default values of question
*                  marks (?).                            ,
*                                                                       LB1040h
*    5a) ?ABOUT, optional, display LISTDSJ date time stamp on TSO       LB1040h
*            session screen.                                            LB1040h
*            NOTE: This option is specified in place of DSN as the      LB1040h
*                  parameter to LISTDSJ.                                LB1040h
*                                                                       LB1001a
*    6) DS,  optional, date separator to be used for dates              LB1001a
*            DS(D) use dash   (-) **Default                             LB1001f
*            DS(P) use period (.)                                       LB1001a
*            DS(S) use slash  (/)                                       LB1001f
*                                                                       LB1001b
*    7) DF,  optional, date format layout for MDY type dates            LB1001b
*            DF(1) use date format MM_DD_CCYY                           LB1001b
*            DF(2) use date format DD_MM_CCYY                           LB1001b
*            DF(3) use date format CCYY_MM_DD **Default                 LB1001b
*            DF(4) use date format CCYY_DD_MM                           LB1001b
*                                                                       LB1010g
*    8) FILE, optional, DDName to be used to resolve dataset name       LB1010g
*            from session allocated files.                              LB1010g
*            Use FILE or NAME to specify dataset name.                  LB1010h
*                                                                       LB1010h
*    9) DL0 or DL1, optional, implies DIRECTORY option.                 LB1010h
*            If ISPF is available, create PDS directory list in         LB1010h
*            a ISPF table.                                              LB1010h
*            DL0 creates ISPF table named DIRLST0.                      LB1010h
*            DL1 creates ISPF table named DIRLST1.                      LB1010h
*            PDS directory list includes ISPF statistics, if available. LB1010h
*                                                                       LB1040c
*   10) TSOB, optional, bypass test validation of executing under TSO.  LB1040c
*            Used when executing under batch TSO.                       LB1040c
*
         EJECT
*  LISTDSJ Command Examples:
*  ================================================================
*
*    1) LISTDSJ 'HERC01.TEST.CNTL'                                      LB1010e
*       Dataset attributes and other information for HERC01.ETST.CNTL
*       are set for each LISTDSJ SYS* CLIST variable excluding
*       DIRECTORY information.
*
*    2) LISTDSJ 'HERC01.TEST.CNTL' DIR                                  LB1010e
*       Dataset attributes and other information for HERC01.ETST.CNTL
*       are set for each LISTDSJ SYS* CLIST variable including
*       DIRECTORY information.
*       NOTE: Last Referenced Date updated when file opened.
*
*    3) LISTDSJ TEST.CNTL PNL                                           LB1010e
*       Dataset attributes and other information for HERC01.TEST.CNTL   LB1010e
*       are set for each LISTDSJ Snn CLIST variable excluding
*       DIRECTORY information.
*       NOTE: Short variable name notation used (&Snn)                  LB1010
*       NOTE: DSN prefixed with USERID of HERC01 due to no quotes       LB1010e
*
*    4) LISTDSJ 'HERC01.TEST.CNTL' ABOUT                                LB1010e
*       Display LISTDSJ date time stamp on TSO session screen and set
*       all LISTDSJ SYS* CLIST variables to question marks (?).
*
*    5) LISTDSJ SYSPROC FILE                                            LB1010g
*       Dataset attributes and other information for dataset name       LB1010g
*       associated with DDNAME SYSPROC are set for each LISTDSJ SYS*    LB1010g
*       CLIST variable excluding DIRECTORY information.                 LB1010g
*                                                                       LB1040h
*    6) LISTDSJ ?ABOUT                                                  LB1040h
*       Display LISTDSJ date time stamp on TSO session screen and set   LB1040h
*       all LISTDSJ SYS* CLIST variables to question marks (?).         LB1040h
*
         EJECT                                                          LB2000
*  Programs Called:                                                     LB2000
*  ==================================================================   LB2000
*                                                                       LB2000
*    o  GETMAIN/FREEMAIN       Working Storage                          LB2000
*    o  LDSJMSG                LISTDSJ Message Processing               LB2000d
*    o  LDSJISP                LISTDSJ ISPF Service Processing          LB2000e
*    o  IKJCT441               TSO CLIST Variables API                  LB2000
*                                                                       LB2000
*                                                                       LB2000
         EJECT
*  Table 1 - Program Register Usage
*
*  +------+----------------------------------------------------------+
*  | REG  |                                                          |
*  |USAGE |  DESCRIPTION                                             |
*  +------+----------------------------------------------------------+
*  |  R13 |  Calling program registers (R14-R12) upon entry          |
*  |  R14 |  Calling program return address upon entry               |
*  |  R15 |  Called  program entry address  upon entry               |
*  |  R1  |  Called  program parms starting address                  |
*  +------+----------------------------------------------------------+
*  |  R0  |  Working Register                                        |
*  |  R1  |  Working Register                                        |
*  |  R2  |  Working Register                                        |
*  |  R3  |  Working Register                                        |
*  |  R4  |  Working Register                                        |
*  |  R5  |  Working Register                                        |
*  |  R6  |  Working Register                                        |
*  |  R7  |  Working Register                                        |
*  |  R8  |  Working Register                                        |
*  |  R9  |  Address of WORKAREA via GETMAIN/FREEMAIN                |
*  |  R10 |  Base Register 1                                         |
*  |  R11 |  Base Register 2                                         |
*  |  R12 |  Base Register 3                                         |
*  |  R13 |  Address of SAVEAREA                                     |
*  +------+----------------------------------------------------------+
*  |  R15 |  Return Code upon exit                                   |
*  +------+----------------------------------------------------------+
*
         EJECT
*  Table 2 - Return Codes
*
*  +------+----------------------------------------------------------+
*  |RETURN|                                                          |
*  | CODE |  DESCRIPTION                                             |
*  +------+----------------------------------------------------------+
*  |  00  |  Successful Request                                      |
*  +------+----------------------------------------------------------+
*  |  04  |  Some dataset information is unavailable. All variable   |
*  |      |  values are valid except for those with question marks   |
*  |      |  (?) which could not resolved or not valid for request.  |
*  +------+----------------------------------------------------------+
*  |  16  |  Severe error occurred.                                  |
*  |      |  None of the variables values are valid.                 |
*  +------+----------------------------------------------------------+
*  | 4004-|  Parm not supplied to LISTDSJ                            |
*  | 4005 |  None of the variables values are valid.                 |
*  |      |  Program terminated.                                     |
*  +------+----------------------------------------------------------+
*  | 4008-|  CPPL error no variables found                           |
*  | 4010 |  None of the variables values are valid.                 |
*  |      |  Program terminated.                                     |
*  +------+----------------------------------------------------------+
*  | 4095 |  Program cannot link to LDSJISP                          |  LB2000e
*  |      |  Program terminated.                                     |  LB2000e
*  +------+----------------------------------------------------------+  LB2000e
*  | 4096 |  Program cannot link to LDSJMSG                          |  LB2000d
*  |      |  Program terminated.                                     |  LB2000d
*  +------+----------------------------------------------------------+  LB2000d
*  | 4097 |  Program cannot link to TSO dynamic allocation interface |
*  |      |  Program terminated.                                     |
*  +------+----------------------------------------------------------+
*  | 4098 |  Program cannot link to CLIST variable processing API    |
*  |      |  Program terminated.                                     |
*  +------+----------------------------------------------------------+
*  | 4099 |  Program not executing under TSO                         |
*  |      |  Program terminated.                                     |
*  +------+----------------------------------------------------------+
*  Note: -Symbolic Variable &LASTCC contains LISTDSJ return code
*        -Messages in the 4xxx range issue a TSO session message
         EJECT
*  Table 3 - CLIST Variables <CLSTVARS>
*
*  +--+------------------+-------------------------------------------+
*  |S#| CLIST VARIABLE   | CONTENT                                   |
*  +--+------------------+-------------------------------------------+
*  |  | LASTCC           | Return code from LISTDSJ available        |
*  |  |                  | immediately after execution of LISTDSJ    |
*  |  |                  | command                                   |
*  +--+------------------+-------------------------------------------+
*  |00| SYSDSNAME        | Dataset name                              |
*  +--+------------------+-------------------------------------------+
*  |01| SYSLISTDSJMSG    | LISTDSJ message variable                  |
*  |  |                  | Miscellaneous processing message          |
*  +--+------------------+-------------------------------------------+
*  |02| SYSREASON        | LISTDSJ reason code                       |
*  |  |                  | See REASON CODE table, Table 4            |
*  |  |                  |     column REASON CODE                    |
*  +--+------------------+-------------------------------------------+
*  |03| SYSMSGLVL1       | First-level message                       |
*  |  |                  | See RETURN CODE table, Table 2            |
*  |  |                  |     column DESCRIPTION                    |
*  +--+------------------+-------------------------------------------+
*  |04| SYSMSGLVL2       | Second-level message                      |
*  |  |                  | See REASON CODE table, Table 4            |
*  |  |                  |     column DESCRIPTION                    |
*  +--+------------------+-------------------------------------------+
*  |05| SYSCREATE        | Creation Date  format CCYY/JJJ            |
*  |  |                  | CCYY-century year, JJJ-julian date        |
*  |  |                  | Note: Century windowed at 70.             |
*  +--+------------------+-------------------------------------------+
*  |06| SYSEXDATE        | Expiration Date  format CCYY/JJJ          |
*  |  |                  | CCYY-century year, JJJ-julian date        |
*  |  |                  | Note: Century windowed at 70.             |
*  +--+------------------+-------------------------------------------+
*  |07| SYSREFDATE       | Last Referenced Date  format CCYY/JJJ     |
*  |  |                  | CCYY-century year, JJJ-julian date        |
*  |  |                  | Note: Century windowed at 70.             |
*  |  |                  | Referenced date will be updated with DIR  |
*  |  |                  | option is used on a PDS request.          |
*  +--+------------------+-------------------------------------------+
*  |08| SYSDSORG         | Dataset Organization                      |
*  |  |                  | IS  -ISAM                                 |
*  |  |                  | PS  -Physical Sequential                  |
*  |  |                  | DA  -Direct Access                        |
*  |  |                  | PO  -Partition Organization               |
*  |  |                  | U   -Undefined                            |
*  |  |                  | VS  -VSAM                                 |
*  |  |                  |                                           |
*  |  |                  | xxU -Unmovable                            |
*  +--+------------------+-------------------------------------------+
         EJECT
*  Table 3 - CLIST Variables  (continued)
*
*  +--+------------------+-------------------------------------------+
*  |S#| CLIST VARIABLE   | CONTENT                                   |
*  +--+------------------+-------------------------------------------+
*  |09| SYSRECFM         | Record format for non-VSAM datasets       |
*  |  |                  | Character combination        e.g.  FBA    |
*  |  |                  | U -Undefined                              |
*  |  |                  | F -Fixed                                  |
*  |  |                  | V -Variable                               |
*  |  |                  | B -Blocked                                |
*  |  |                  | T -Track Overflow                         |
*  |  |                  | S -Spanned or Standard                    |
*  |  |                  | A -ANSI                                   |
*  |  |                  | M -Machine                                |
*  +--+------------------+-------------------------------------------+
*  |10| SYSLRECL         | Logical record length                     |
*  +--+------------------+-------------------------------------------+
*  |11| SYSBLKSIZE       | Block size                                |
*  +--+------------------+-------------------------------------------+
*  |12| SYSKEYLEN        | Key Length                                |
*  +--+------------------+-------------------------------------------+
*  |13| SYSKEYPOS        | Relative Key Position                     |
*  +--+------------------+-------------------------------------------+
*  |14| SYSPASSWORD      | Password Indication                       |
*  |  |                  | NONE   No password required               |
*  |  |                  | READ   Password required to READ          |
*  |  |                  | R_W    Password required to READ and WRITE|
*  +--+------------------+-------------------------------------------+
*  |15| SYSRACFA         | RACF Indication                           |
*  |  |                  | NONE                                      |
*  |  |                  | RACF                                      |
*  +--+------------------+-------------------------------------------+
*  |16| SYSADIRBLK       | Directory Blocks Allocated                |
*  |  |                  | Only for PDS when DIR option is specified |
*  +--+------------------+-------------------------------------------+
*  |17| SYSUDIRBLK       | Directory Blocks Used                     |
*  |  |                  | Only for PDS when DIR option is specified |
*  +--+------------------+-------------------------------------------+
*  |18| SYSNUDIRBLK      | Directory Blocks Not Used                 |
*  |  |                  | Only for PDS when DIR option is specified |
*  +--+------------------+-------------------------------------------+
*  |19| SYSMEMBERS       | Number of Members (total members)         |
*  |  |                  | Only for PDS when DIR option is specified |
*  +--+------------------+-------------------------------------------+
*  |20| SYSMEMBERSALIAS  | Number of Alias Members (total within     |
*  |  |                  |   SYSMEMBERS)                             |
*  |  |                  | Only for PDS when DIR option is specified |
*  +--+------------------+-------------------------------------------+
         EJECT
*  Table 3 - CLIST Variables  (continued)
*
*  +--+------------------+-------------------------------------------+
*  |S#| CLIST VARIABLE   | CONTENT                                   |
*  +--+------------------+-------------------------------------------+
*  |21| SYSVOLUME        | Volume serial ID                          |
*  +--+------------------+-------------------------------------------+
*  |22| SYSUNIT          | Device Unit on which volume resides       |
*  |  |                  | 2305, 2305-2, 2314,                       |
*  |  |                  | 3330, 3340, 3350, 3375, 3380, 3390        |
*  +--+------------------+-------------------------------------------+
*  |23| SYSUNITS         | Space Units                               |
*  |  |                  | CYLINDER                                  |  LB1010c
*  |  |                  | TRACK                                     |  LB1010c
*  |  |                  | BLOCK                                     |  LB1010c
*  |  |                  | ABSOLUTE                                  |  LB1010c
*  +--+------------------+-------------------------------------------+
*  |24| SYSTRKSUSED      | Total Tracks Used                         |
*  +--+------------------+-------------------------------------------+
*  |25| SYSEXTENTS       | Number of Extents allocated               |
*  +--+------------------+-------------------------------------------+
*  |26| SYSTRKSALLOC     | Total Tracks Allocated                    |
*  +--+------------------+-------------------------------------------+
*  |27| SYSSECONDS       | Secondary Allocation in Space Units       |
*  +--+------------------+-------------------------------------------+
*  |28| SYSTRKSUNUSED    | Total Tracks Unused                       |
*  +--+------------------+-------------------------------------------+
*  |29| SYSCYLVOL        | Cylinders for Volume                      |
*  |  |                  | As stored in volume F4-DSCB               |
*  +--+------------------+-------------------------------------------+
*  |30| SYSTRKSCYL       | Tracks per Cylinder for SYSUNIT           |
*  |  |                  | As stored in volume F4-DSCB               |
*  +--+------------------+-------------------------------------------+
*  |31| SYSJCREATE       | Creation Date  format YYJJJ               |
*  |  |                  | YY-year, JJJ-julian date                  |
*  |  |                  | as stored by MVS3.8J                      |
*  +--+------------------+-------------------------------------------+
*  |32| SYSJEXDATE       | Expiration Date  format YYJJJ             |
*  |  |                  | YY-year, JJJ-julian date                  |
*  |  |                  | as stored by MVS3.8J                      |
*  +--+------------------+-------------------------------------------+
*  |33| SYSJREFDATE      | Last Referenced Date  format YYJJJ        |
*  |  |                  | YY-year, JJJ-julian date                  |
*  |  |                  | as stored by MVS3.8J                      |
*  |  |                  | Referenced date will be updated when DIR  |
*  |  |                  | option is used on a PDS request.          |
*  +--+------------------+-------------------------------------------+
         EJECT
*  Table 3 - CLIST Variables  (continued)
*
*  +--+------------------+-------------------------------------------+
*  |S#| CLIST VARIABLE   | CONTENT                                   |
*  +--+------------------+-------------------------------------------+
*  |34| SYSTRKLEN        | Track Length        for SYSUNIT           |
*  |  |                  | As stored in volume F4-DSCB               |
*  +--+------------------+-------------------------------------------+
*  |35| SYSUNITCAP       | Capacity for SYSUNIT                      |
*  |  |                  | Computed as -                             |
*  |  |                  | SYSCYLVOL * SYSTRKSCYL * SYSTRKLEN        |
*  +--+------------------+-------------------------------------------+
*  |36| SYSBLKSTRK       | Blocks per Track for SYSUNIT              |
*  |  |                  | Computed as -                             |
*  |  |                  | SYSTRKLEN / SYSBLKSIZE                    |
*  +--+------------------+-------------------------------------------+
*  |37| SYSCCREATE       | Creation Date  format CCYY/MM/DD          |  LB1001b
*  |  |                  | MM-month, DD-day, CCYY-century year       |
*  |  |                  | Note: See DF parm to vary date layout     |  LB1001b
*  +--+------------------+-------------------------------------------+
*  |38| SYSCEXDATE       | Expiration Date  format CCYY/MM/DD        |  LB1001b
*  |  |                  | MM-month, DD-day, CCYY-century year       |
*  |  |                  | Note: See DF parm to vary date layout     |  LB1001b
*  +--+------------------+-------------------------------------------+
*  |39| SYSCREFDATE      | Last Reference Date   format CCYY/MM/DD   |  LB1001b
*  |  |                  | MM-month, DD-day, CCYY-century year       |
*  |  |                  | Note: See DF parm to vary date layout     |  LB1001b
*  |  |                  | Referenced date will be updated with DIR  |
*  |  |                  | option is used on a PDS request.          |
*  +--+------------------+-------------------------------------------+
*  |40| SYSNUMVOLS       | Number of volumes for dataset             |
*  |  |                  | As stored in catalog                      |
*  +--+------------------+-------------------------------------------+
*  |41| SYSDSCAT         | Dataset catalog indication (Y/N)          |
*  |  |                  | Y      Dataset is cataloged               |
*  |  |                  | N      Dataset is not cataloged           |
*  +--+------------------+-------------------------------------------+
         EJECT
*  Table 3 - CLIST Variables  (continued)
*
*  +--+------------------+-------------------------------------------+
*  |S#| CLIST VARIABLE   | CONTENT                                   |
*  +--+------------------+-------------------------------------------+
*  |42| SYSDSCATV        | Dataset cataloged volume serial ID        |
*  |  |                  | Volume serial of dataset name found       |
*  |  |                  | on catalog                                |
*  +--+------------------+-------------------------------------------+
*  |43| SYSPRIMARY       | Primary Allocation in Space units.        |
*  +--+------------------+-------------------------------------------+
*  |44| SYSUSED          | Allocation used, in Space units.          |
*  +--+------------------+-------------------------------------------+
*  |45| SYSVOLUMES       | List of volume serial numbers for         |
*  |  |                  | single and multiple volume datasets.      |
*  |  |                  | Limited to 5 volumes.                     |
*  +--+------------------+-------------------------------------------+
*  |46| SYSALLOC         | Allocation in Space units.                |  LB1010d
*  +--+------------------+-------------------------------------------+  LB1010d
*  |47| SYSUPDATED       | Dataset Backup Change Indication (Y/N)    |  LB1040a
*  |  |                  | Y      Dataset updated since last backup  |  LB1040a
*  |  |                  | N      Dataset not updated since last     |  LB1040a
*  |  |                  |        backup                             |  LB1040a
*  +--+------------------+-------------------------------------------+  LB1040a
*
         EJECT
*  Table 4 - REASON CODES Table
*
*  +-------+---------------------------------------------------------+
*  |REASON |                                                         |
*  | CODE  | DESCRIPTION                                             |
*  +-------+---------------------------------------------------------+
*  |  00   | Normal completion                                       |
*  +-------+---------------------------------------------------------+
*  |  02   | Dynamic allocation processing error                     |
*  |       | 'RC=xxx Allocate PDS error'                             |
*  |       | 'RC=xxx Free PDS error'                                 |
*  +-------+---------------------------------------------------------+
*  |  03   | Data set type cannot be processed                       |  LB1040g
*  +-------+---------------------------------------------------------+  LB1040g
*  |  05   | Dataset not cataloged                                   |
*  |       | 'RC=xxx DSCB-Fx CAMLST NAME error'                      |
*  +-------+---------------------------------------------------------+
*  |  06   | Error obtaining dataset attributes                      |  LB1040x
*  +-----------------------------------------------------------------+  LB1040e
*  |  07   | Device type not found (DVCLST internal table)           |  LB1040e
*  +-----------------------------------------------------------------+  LB1040c
*  |  12   | VSAM datasets not supported                             |  LB1040c
*  +-------+---------------------------------------------------------+
*  |  13   | Dataset could not be opened or closed                   |
*  |       | 'RC=xxx Open PDS error'                                 |
*  |       | 'RC=xxx Close PDS error'                                |
*  +-----------------------------------------------------------------+  LB1040e
*  |  14   | Device type not found in UCB                            |  LB1040e
*  +-------+---------------------------------------------------------+  LB1040x
*  |  17   | System or User Abend error                              |  LB1040x
*  +-------+---------------------------------------------------------+
*  |  19   | Dataset resides on multiple volumes                     |
*  +-------+---------------------------------------------------------+
*  |  21   | Catalogue error when locating dataset                   |
*  +-------+---------------------------------------------------------+
*  |  22   | Volume not mounted                                      |
*  |       | 'RC=xxx DSCB-Fx CAMLST NAME error'                      |
*  |       | 'RC=xxx DSCB-Fx CAMLST SEARCH error'                    |
*  |       | 'RC=xxx DSCB-Fx CAMLST SEEK error'                      |
*  +-------+---------------------------------------------------------+
*  |  23   | Permanent I/O error on volume                           |
*  |       | 'RC=xxx DSCB-Fx CAMLST NAME error'                      |
*  |       | 'RC=xxx DSCB-Fx CAMLST SEARCH error'                    |
*  |       | 'RC=xxx DSCB-Fx CAMLST SEEK error'                      |
*  +-------+---------------------------------------------------------+
*  |  24   | Dataset not found                                       |
*  |       | 'RC=xxx DSCB-Fx CAMLST NAME error'                      |
*  |       | 'RC=xxx DSCB-Fx CAMLST SEARCH error'                    |
*  |       | 'RC=xxx DSCB-Fx CAMLST SEEK error'                      |
*  +-------+---------------------------------------------------------+
*  |  26   | Dataset on MSS (Mass Storage) device                    |
*  +-------+---------------------------------------------------------+
*  |  27   | No VOLSER allocated to dataset (volumes=0)              |  LB1040f
*  +-------+---------------------------------------------------------+  LB1040f
         EJECT                                                          LB1040f
*  Table 4 - REASON CODES Table  (continued)                            LB1040f
*                                                                       LB1040f
*  +--+------------------+-------------------------------------------+  LB1040f
*  |  28   | DDNAME must be 1-8 characters                           |  LB1010g
*  +-------+---------------------------------------------------------+  LB1010g
*  |  29   | Dataset or DD name must be specified                    |  LB1010g
*  +-------+---------------------------------------------------------+
*  |  90   | DDN not allocated (not found)                           |  LB1010g
*  +-------+---------------------------------------------------------+  LB1010g
*  |  99   | ABOUT request, no DSN info retrieved                    |
*  +-------+---------------------------------------------------------+
*  Note: Additional information may be included for selected Reason
*        Codes in variable SYSLISTDSJMSG show in quotes above
*
*
         EJECT
*  Macros and SYSLIB Location:
*  ================================================================
*
*  Macro     Description                         Library
*  --------  ----------------------------------  ------------------
*  YREGS     Register Equates                    SYS2.MACLIB
*  IHAPSA    Prefixed Save Area                  SYS1.AMODGEN
*  CVT       Communication Vector Table          SYS1.AMODGEN
*  IKJTCB    Task Control Block                  SYS1.AMODGEN           LB1010g
*  IEFTIOT1  Task IO Table                       SYS1.AMODGEN           LB1010g
*  IEFJFCBN  Job File Control Block              SYS1.AMODGEN           LB1010g
*  IHAASCB   Address Space Control Block         SYS1.AMODGEN
*  IEFUCBOB  Unit Control Block                  SYS1.AMODGEN
*  IKJCPPL   CP Parm List                        SYS1.MACLIB
*  IKJUPT    User Profile Table                  SYS1.MACLIB            LB1010g
*  IKJPPL    Parse Parm List                     SYS1.MACLIB
*  IKJIOPL   I/O Service Routine Parm List       SYS1.MACLIB
*  IKJDAPL   Dynamic Allocation Parm LIst        SYS1.MACLIB
*  IKJDAP08  Operation block ALLOC Dataset       SYS1.MACLIB
*  IKJDAP18  Operation block FREE  Dataset       SYS1.MACLIB
*  IECSDSL1  DSCB Dataset Mapping 1              SYS1.AMODGEN           LB2000z
*  IECSDSL2  DSCB Dataset Mapping 2              SYS1.AMODGEN           LB2000z
*  IECSDSL3  DSCB Dataset Mapping 3              SYS1.AMODGEN           LB2000z
*  IECSDSL4  DSCB Dataset Mapping 4              SYS1.AMODGEN           LB2000z
*  #ISTATS   ISPF Stats Table Columns            LISTDSJ maclib         LB2000a
*  #MPAL     Message Parameter Address List      LISTDSJ maclib         LB2000a
*  #IPAL     ISP Srv Parameter Address List      LISTDSJ maclib         LB2000a
*  #DATESEP  Date Separator Char Config          LISTDSJ maclib         LB2000a
*  #DATEFMT  Date Format    Char Config          LISTDSJ maclib         LB2000a
*
*
*  References:
*  ================================================================
*
*  - GC28-6628-9 OS System Control Blocks (R21.7)
*  - Advanced Assembler Language and MVS Interfaces (C. Cannatello)
*  - SC34-2036-1 SPF Dialogue Management Services (March 1981)          LB1010h
*
*
         EJECT
*  Change History: <CHGHIST>
*  ================================================================
*
*  MM/DD/CCYY Version  Name / Description
*  ---------- -------  --------------------------------------------
*  04/02/2021 2.0.00   Larry Belmontes Jr.                              LB2000a
*                      - LISTDSJ Shared macros                          LB2000a
*                      - Add data and date format macros globals        LB2000b
*                      - Correct register usage for testing TSO         LB2000c
*                      - Externalize message processing                 LB2000d
*                      - Externalize ISPF Service processing            LB2000e
*                      - Subroutine R15 rc on DJMSG                     LB2000f
*                      - Misc documentation updates                     LB2000z
*                                                                       LB2000a
*  02/27/2021 1.0.40   Larry Belmontes Jr.                              LB1040a
*                      - Support new variable, &SYSUPDATED,             LB1040a
*                        reporting status of DS1DSIND                   LB1040a
*                        as DS1IND02 mask true                          LB1040a
*                      - Correct REG use from R0 to R4 in TSO test      LB1040b
*                      - Add TSOB  parm keyword and relocate            LB1040c
*                        under TSO test                                 LB1040c
*                      - Curtail VSAM data returned to DSORG, VOLUME,   LB1040d
*                        and UNIT.  Activate reason code 12 (VSAM       LB1040d
*                        not supported).                                LB1040d
*                      - Unit Type not found error handling,            LB1040e
*                        activate reason code 14 (not found UCB entry)  LB1040e
*                      - Unit Type not found error handling,            LB1040e
*                        activate reason code 7  (not found in internal LB1040e
*                        table DVCLST)                                  LB1040e
*                      - Activate reason code 27 (no volser for DSN)    LB1040f
*                      - Activate reason code 3 (cannot process DSORG)  LB1040g
*                      - Add support of '?ABOUT' as first paramter      LB1040h
*                        for ABOUT information                          LB1040h
*                      - Various program documentation updates          LB1040x
*                                                                       LB1040x
*  04/10/2020 1.0.20   Larry Belmontes Jr.                              LB1020a
*                      - Correct TSB addr for TSO ENV testing           LB1020a
*                                                                       LB1020a
*  09/30/2019 1.0.10   Larry Belmontes Jr.                              LB1010a
*                      - Add comments for DF and DS keywords            LB1010a
*                        regarding customizing default values at        LB1010a
*                        time of installation                           LB1001a
*                      - Add keywords DIRECTORY and VOLUME.             LB1010b
*                        Associated abbreviated keywords remain         LB1010b
*                        unchanged, DIR and VOL.                        LB1010b
*                      - Changed list of values for &SYSUNITS           LB1010c
*                        instead of abbreviations                       LB1010c
*                        CYLINDER, TRACK, BLOCK, ABSOLUTE               LB1010c
*                      - Support new variable, &SYSALLOC,               LB1010d
*                        allocation in space units                      LB1010d
*                        - VALLOC   DC    C'SYSALLOC'                   LB1010d
*                      - Support USERID prefix for DSN when             LB1010e
*                        unquoted dataset name is provided              LB1010e
*                      - Changed date seperator and format defaults     LB1010f
*                        - Date Separator to dash    (-)                LB1010f
*                        - Date Format to CCYY_MM_DD (3)                LB1010f
*                      - Support FILE keyword to provide DDNAME         LB1010g
*                        in place of dataset name                       LB1010g
*                      - Support DL0 and DL1 keywords to provide a      LB1010h
*                        PDS directory list including ISPF stats (if    LB1010h
*                        available) in a ISPF table.  ISPF must be      LB1010h
*                        available to MVS38J to use this feature.       LB1010h
*                        DL0 creates ISPF table named DIRLST0           LB1010h
*                        DL1 creates ISPF table named DIRLST1           LB1010h
*                      - Enhance calling of IKJCT441 and ISPLINK        LB1010i
*                                                                       LB1010a
*  05/17/2019 1.0.01   Larry Belmontes Jr.                              LB1001a
*                      - Add DS (Date Separator) keyword to support     LB1001a
*                        dash, period, and slash date separators        LB1001a
*                        - Default can be customized before installing  LB1010a
*                          see variable DATESEP                         LB1010a
*                      - Add DF (Date Format layout) keyword to support LB1001b
*                        the following layouts:                         LB1001b
*                        MM_DD_CCYY                                     LB1001b
*                        DD_MM_CCYY                                     LB1001b
*                        CCYY_MM_DD                                     LB1001b
*                        CCYY_DD_MM                                     LB1001b
*                        - Default can be customized before installing  LB1010a
*                          see variable DATEFMT                         LB1010a
*                                                                       LB1001a
*  04/29/2019 1.0.00   Larry Belmontes Jr.
*                      - Initial version released to MVS 3.8J
*                        hobbyist public domain
*
*  04/28/2019 0.5.10   Larry Belmontes Jr.
*                      - Enhance keyword detection by checking for
*                        a delimiter (blank) after keyword
*                        - KDIR     EQU  *
*                        - KPNL     EQU  *
*                        - KABOUT   EQU  *
*
*  04/26/2019 0.5.00   Larry Belmontes Jr.
*                      - Support parms: DSN VOL() DIR PNL ABOUT
*                        - Add ABOUT parm processing
*                      - Enhance space used routine
*                        - SPCUSED  EQU   *
*                      - Support volumes list for multi-volume DSN
*                        (limited to 5 intentionally)
*                        - VVOLLST  DC    C'SYSVOLUMES'
*
*  02/21/2019 0.4.00   Larry Belmontes Jr.
*                      - Support parms: DSN VOL() DIR PNL
*                        - Add PNL parm processing
*                      - Enhance DSORG table processing
*                      - Support primary allocations
*                        - VPSPCAL  DC    C'SYSPRIMARY'
*                        - VPSPCUS  DC    C'SYSUSED'
*                      - Add TSO session messages for certain errors
*                        using a message prefix
*                        - 'LISTDSJ   nnnn - error...'
*
*  01/08/2019 0.3.00   Larry Belmontes Jr.
*                      - Enhance device table search
*                      - Enhance symbolic variable processing
*
*  01/26/2019 0.3.10   Larry Belmontes Jr.
*                      - Add error processing
*                        - ERR97
*                        - ERR99
*                      - Add Y2k Window value as a variable
*                        - Y2KWIN   DC    F'70'
*
*  12/22/2018 0.2.00   Larry Belmontes Jr.
*                      - Support parms: DSN VOL() DIR
*                        - Add DIR parm processing
*                      - Support catalog variables
*                        - VDSCAT   DC    C'SYSDSCAT'
*                        - VDSCATV  DC    C'SYSDSCATV'
*                        - VNUMVOL  DC    C'SYSNUMVOLS'
*                        - catalog / not cataloged
*                        - single volume / multi volume
*                      - Support reason code processing
*                        - VRSCODE  DC    C'SYSREASON'
*                        - VMSGL1   DC    C'SYSMSGLVL1'
*                        - VMSGL2   DC    C'SYSMSGLVL2'
*                      - Support DSCB F-1 dataset attributes
*                        - VKEYP    DC    C'SYSKEYPOS'
*                      - Support additional file date format CLIST
*                        variables
*                        - VCREDTJ  DC    C'SYSJCREATE'
*                        - VEXPDTJ  DC    C'SYSJEXDATE'
*                        - VREFDTJ  DC    C'SYSJREFDATE'
*                        - VCREDTC  DC    C'SYSCCREATE'
*                        - VEXPDTC  DC    C'SYSCEXDATE'
*                        - VREFDTC  DC    C'SYSCREFDATE'
*                      - Check for presence of DSCB F-2 ISAM
*                        vtoc record
*                      - Support DSCB F-3 dataset extents
*                      - Support DSCB F-4 volume device
*                        characteristics
*                        - VCCVOL   DC    C'SYSCYLVOL'
*                        - VTTCC    DC    C'SYSTRKSCYL'
*                        - VTRKLEN  DC    C'SYSTRKLEN'
*                        - VBLKTRK  DC    C'SYSBLKSTRK'
*                        - VUNITCAP DC    C'SYSUNITCAP'
*                      - Compute total space allocation in tracks
*                        and cylinders by summarizing extent info
*                        in F-1 and F-3 DSCBs
*                        - VTKAL    DC    C'SYSTRKSALLOC'
*                        - VTKUS    DC    C'SYSTRKSUSED'
*                        - VTKUUS   DC    C'SYSTRKSUNUSED'
*                      - Enhance message communication via new CLIST
*                        variable
*                        - VJMSG    DC    C'SYSLISTDSJMSG'
*                      - Add DAIR processing for PDS Directory
*                      - Support PDS directory information
*                        - VMEMS    DC    C'SYSMEMBERS'
*                        - VALIAS   DC    C'SYSMEMBERSALIAS'
*                        - VDIRBKSU DC    C'SYSUDIRBLK'
*                        - VDIRBKSN DC    C'SYSNUDIRBLK'
*                        - VDIRBKSA DC    C'SYSADIRBLK'
*
*  11/25/2018 0.1.00   Larry Belmontes Jr.
*                      - Support parms: DSN
*                        - Add DSN parm processing
*                      - Support basic catalog checks for DSN:
*                        - catalog / not cataloged
*                        - single volume / multi volume
*                      - Support reason code processing
*                        - VRSCODE  DC    C'SYSREASON'
*                        - VMSGL1   DC    C'SYSMSGLVL1'
*                        - VMSGL2   DC    C'SYSMSGLVL2'
*                      - Support DSCB F-1 dataset attributes
*                        - VVOL     DC    C'SYSVOLUME'
*                        - VLRECL   DC    C'SYSLRECL'
*                        - VBLKSZ   DC    C'SYSBLKSIZE'
*                        - VKEYL    DC    C'SYSKEYLEN'
*                        - VDSORG   DC    C'SYSDSORG'
*                        - VRECFM   DC    C'SYSRECFM'
*                        - VCREDT   DC    C'SYSCREATE'
*                        - VEXPDT   DC    C'SYSEXDATE'
*                        - VREFDT   DC    C'SYSREFDATE'
*                        - VPASSWD  DC    C'SYSPASSWORD'
*                        - VRACF    DC    C'SYSRACFA'
*                        - VSSPC    DC    C'SYSUNITS'
*                        - VSSPCNM  DC    C'SYSSECONDS'
*                        - VUNIT    DC    C'SYSUNIT'
*                        - VEXTS    DC    C'SYSEXTENTS'
*
*  11/18/2018 0.0.00   Larry Belmontes Jr.
*                      - Simple prototype
*                      - Return code processing via R15 (&LASTCC TSO)
*                      - Support parms: DSN
*                      - Echo parm to clist variable
*                        - VDSN     DC    C'SYSDSNAME'
*
*
         EJECT
*  Potential Enhancements: <ENHMNTS>
*  ================================================================
*
*  01) Add ability to detect 31-bit addressing and execute ATL
*      for MVS 380
*
*
*
*
*
*
*
*
*
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
LISTDSJ  CSECT
         USING LISTDSJ,R10,R11,R12 my BASE REGISTER(S)
         PRINT NOGEN
*     *
*     * /********************************************************/
*     * /* Save regs and declare base registers R10,R11,R12     */
*     * /* including saving R1 (PARMS) in R2                    */
*     * /* - R1  myPARMS address on entry                       */
*     * /* - R15 myENTRY address on entry                       */
*     * /********************************************************/
         STM   R14,R12,12(R13)     Save calling program registers
         LR    R10,R15             Load base1 w R15
         LA    R11,4095(R10)       Load base2 (R11) w/ offset
         LA    R11,1(R11)          Adjust base2
         LA    R12,4095(R11)       Load base3 (R12) w/ offset
         LA    R12,1(R12)          Adjust base3
         LR    R15,R13             Save callers registers
         LR    R2,R1               Save PARM addr
*     *
*     * /********************************************************/
*     * /* Program Eyecatcher                                   */
*     * /********************************************************/
         B     OVRSTAMP            Branch past eye-catcher
MYSTAMP  EQU   *
PGMID    DC    CL8'LISTDSJ '       My Program STAMP
         DC    CL8'MVS3.8J '       OS
         DC    CL8'V2.0.00 '       .Version                             LB2000
         DC    CL8'04022021'       ..date  MMDDCCYY                     LB2000
         DC    CL1' '
PGMDS    DC    CL8'&SYSDATE'       Assemble Date
         DC    CL3' - '            .and
PGMTS    DC    CL8'&SYSTIME'       ..Time
         DC    C'Copyright (C) 2019-2021 '                              LB1040
         DC    C'Larry Belmontes, Jr. '                                 LB1020
         DC    C'https://ShareABitofIT.net/LISTDSJ-for-MVS-3-8J'        LB1020
MYSTAMPL EQU   *-MYSTAMP
OVRSTAMP DS    0H
*
         EJECT
*     * /********************************************************/
*     * /* GETMAIN, working storage, using R9                   */
*     * /* - R0  Working register                               */
*     * /* - R1  Working register (GETMAIN address)             */
*     * /* - R9  Working Storage DSECT address                  */
*     * /********************************************************/
         LA    R0,WSAREAL          R0 = size of Workarea DSECT
         GETMAIN   R,LV=(R0)       Get Storage, R1 = addr of storage
         LTR   R15,R15             Did we get the storage?              LB1020
         BNZ   NOMAINS             NO, return with rc in R15            LB1020
         LR    R9,R1               R9 = Workarea DSECT Register
         USING WSDSECT,R9          Tell Assembler
*     *
*     * /********************************************************/
*     * /* Update Save Areas for caller and calling programs    */
*     * /* - R1  myPARMS address on entry  (restored)           */
*     * /********************************************************/
         LR    R15,R13             Save callers savearea addr
         LA    R13,SAVEAREA        Load Addr of my savearea
         ST    R15,4(,R13)         Chain callers to my savearea
         ST    R13,8(,R15)         Chain my savearea to callers
         LR    R1,R2               Restore R1 as PARM addr
*                                                                       LB2000e
*     * /********************************************************/      LB2000e
*     * /* Load LDSJISP  Entry Point  (ISPF Processor)          */      LB2000e
*     * /* - R0, R1 Working Register                            */      LB2000e
*     * /********************************************************/      LB2000e
         LOAD  EP=LDSJISP,ERRET=ERR4095                                 LB2000e
         ST    R0,LDSJISP          Load and Save LDSJMSG  Entry Point   LB2000e
         LR    R1,R2               Restore R1 as PARM addr              LB2000e
*                                                                       LB2000d
*     * /********************************************************/      LB2000d
*     * /* Load LDSJMSG  Entry Point  (Message Processor)       */      LB2000d
*     * /* - R0, R1 Working Register                            */      LB2000d
*     * /********************************************************/      LB2000d
         LOAD  EP=LDSJMSG,ERRET=ERR4096                                 LB2000d
         ST    R0,LDSJMSG          Load and Save LDSJMSG  Entry Point   LB2000d
         LR    R1,R2               Restore R1 as PARM addr              LB2000d
*     *                                                                 LB1010i
         EJECT                                                          LB2000d
*     * /********************************************************/      LB1010i
*     * /* Load IKJCT441 Entry Point                            */      LB1010i
*     * /* - R0, R1 Working Register                            */      LB1010i
*     * /********************************************************/      LB1010i
         LOAD  EP=IKJCT441,ERRET=ERR4098                                LB1010i
         ST    R0,IKJCT441         Load and Save IKJCT441 Entry Point   LB1010i
         LR    R1,R2               Restore R1 as PARM addr              LB1010i
*     *
*     * /********************************************************/
*     * /* Initialize IKJCT441 Parm List area                   */
*     * /* - R0 Working Register                                */
*     * /********************************************************/
         LA    R0,ECUPDT           Entry Code
         ST    R0,CT441P1
         LA    R0,NAMP1            Name Pointer
         ST    R0,CT441P2
         LA    R0,NAML1            Name Length
         ST    R0,CT441P3
         LA    R0,VALP1            Var  Pointer
         ST    R0,CT441P4
         LA    R0,VALL1            Var  Length
         ST    R0,CT441P5
         LA    R0,TOKEN            Token
         ST    R0,CT441P6
         MVI   CT441P6,B'10000000' Last Parm designation
*
         EJECT
*     * /********************************************************/
*     * /* Initialize CLIST Symbol data area                    */
*     * /* - R3, R4 Working Register                            */
*     * /********************************************************/
         LA    R3,DSYMS            Addr of symbolic data area
         LA    R4,DSYMSL           Len  of symbolic data area
INITSYMA EQU   *
         MVI   0(R3),C'?'          Populate with '?'
         LA    R3,1(R3)            Bump to next byte
         BCT   R4,INITSYMA
*     *
*     * /********************************************************/
*     * /* Initialize RC and Reason Code                        */
*     * /********************************************************/
         MVC   RC,=C'00'           Zero Return Code
         MVC   RCEXIT,=F'0'        Zero Return Code R15
*     * /* #Data source: 02 SYSREASON   set throughout program  */
         MVC   DRSNCODE,=C'00'     Zero Reason Code
*     *
*     * /********************************************************/
*     * /* Initialize - OTHER                                   */
*     * /********************************************************/
         XC    MYECB,MYECB
*     * /* #Data source: 01 SYSLISTDSJMSG set throughout program*/
         MVI   DJMSG,C' '
         MVC   DJMSG+1(L'DJMSG-1),DJMSG
*
*     * /********************************************************/       LB2000e
*     * /* Initialize LDSJISP Parm Address List                 */       LB2000e
*     * /* - R3     Working Register                            */       LB2000e
*     * /********************************************************/       LB2000e
*              PISPREQ             Set at place of request               LB2000e
*              PISPEP              ISPLINK Entry Point set in PODDIR     LB2000e
         LA    R3,ISTATS           Address of                            LB2000e
         ST    R3,PISPDCOL         ** ISPF Stats                         LB2000e
         LA    R3,TBN              Address of                            LB2000e
         ST    R3,PISPTBN          ** TBN                                LB2000e
         LA    R3,DJMSG+16         Address of                            LB2000e
         ST    R3,PISPDMSG         ** DJMSG                              LB2000e
*                                                                        LB2000e
         EJECT
*     * /********************************************************/
*     * /* Get PARAMETER information passed to program          */
*     * /* - R1 myPARMS address on entry                        */
*     * /* - R4 Starting parms address                          */
*     * /* - R6 Parm Length                                     */
*     * /* - R3, R4, R5 Working Register                        */
*     * /* -ReasonCode 29 Dataset name must be specified        */
*     * /* -JMSG'LISTDSJ  4004 -PARM Not supplied'              */
*     * /* -JMSG'LISTDSJ  4005 -PARM Not supplied'              */
*     * /* -JMSG'LISTDSJ  4008 -CPPL No variables'              */
*     * /* -JMSG'LISTDSJ  4009 -CPPL No variables'              */
*     * /* -JMSG'LISTDSJ  4010 -CPPL No variables'              */
*     * /********************************************************/
PARMS    EQU   *                   We have a parm...
         LTR   R1,R1               Do I have a PARM?
         BZ    ERR4005             NO, EXIT with RC = 4005
         LR    R3,R1               YES, R3=PARM/CPPL addr
         TM    0(R3),X'80'         Is it PARM or CPPL addr?
         BZ    CPPLIN              YES, CPPL addr
PARMIN   EQU   *                   NO,  must be PARM addr
         L     R4,0(,R3)           R4=Addr of PARM
         LH    R6,0(,R4)           R6=Length of PARM
         LTR   R6,R6               PARM > 0 length?
         BZ    ERR4004             NO, EXIT with RC = 4004
         LA    R4,2(,R4)           YES, R4=point to start of PARM data
         B     PARMSXT              and continue...
CPPLIN   EQU   *
         ST    R3,MYCPPLP          Store CPPL Address
         L     R4,0(,R3)           R4=Command Buffer addr
         LH    R5,2(,R4)           R5=Offset to Command Variables
         LTR   R5,R5               Any Variables?
         BZ    ERR4008             NO, EXIT with RC = 4008
         LH    R6,0(,R4)           YES, R6=Length of Command Buffer
         SR    R6,R5               Subtract variable offset
         SH    R6,=H'4'            Subtract prefix
         BE    ERR4009             EXIT with RC = 4009 IF NO VARIABLES
         BM    ERR4010             EXIT with RC = 4010 IF NO VARIABLES
         LA    R4,4(R4,R5)         R4=Point to variables start addr
*
PARMSXT  EQU   *                   We have a parm...
*     *
*     * /********************************************************/
*     * /* Uppercase translation of PARMIN                      */
*     * /* - R4 Starting parms address                          */
*     * /* - R6 Parm Length                                     */
*     * /* - R8, R4, R5 Working Register                        */
*     * /********************************************************/
CAP#EM   EQU   *
         LR    R8,R6               R8=Parm Length
         BCTR  R8,0                Adjust length for OC
         EX    R8,UPPERC           Execute Uppercase translation
         B     UPPERCX
UPPERC   OC    0(0,R4),=256X'40'   EBCDIC Lower-Upper Case Translate
UPPERCX  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Scan parm list                                       */
*     * /********************************************************/
PARMSCAN EQU   *
*     *
*     * /*------------------------------------------------------*/
*     * /* Initialize parm switches / variables                 */
*     * /* - R7, R8 Working Register                            */
*     * /*------------------------------------------------------*/
         MVI   PDSN,C' '           Clear DSN
         MVC   PDSN+1(L'PDSN-1),PDSN
         MVI   PDIR,C'N'           DIR    request   - init to NO
         MVI   PPNL,C'N'           PNL    request   - init to NO
         MVI   PABOUT,C'N'         ABOUT  request   - init to NO
         MVI   PVOL,C'N'           VOLUME specified - init to NO
         MVI   PFILE,C'N'          FILE   request   - init to NO        LB1010g
         MVI   ALLOCPO,C'N'        PDS Allocated Flag init to NO        LB1010h
         MVI   PDL,C'N'            DLn    request     init to NO        LB1010h
         MVI   PTSOB,C'N'          TSOB   request     init to NO        LB1040c
         MVC   TBN,=C'DIRLST  '    Table Name - Init                    LB1010h
         MVI   DATESEP,C'-'        Date Separator default value <---    LB1001f
         MVI   DATEFMT,C'3'        Date Format    default value <---    LB1001f
*                                                                       LB1010a
         PRINT GEN                                                      LB2000b
*                                                                       LB2000b
*   Date Separator configuration                                        LB2000b
*   D-dash, P-period, S-slash   (default=D)                             LB2000b
         #DATESEP 'D'              Default - DASH                       LB2000b
*   Date Format Layout configuration                                    LB2000b
*   1=MM_DD_CCYY 2=DD_MM_CCYYYY 3=CCYY_MM_DD 4=CCYY_DD_MM (default=3)   LB2000b
         #DATEFMT '3'              Default - 3=CCYY_MM_DD               LB2000b
*                                                                       LB2000b
         PRINT NOGEN                                                    LB2000b
*                                                                       LB1001a
*                                                                       LB1001a
         LA    R7,PDSN             R7=Start of PDSN area
         LA    R8,0                R8=PDSN actual length
         STH   R8,PDSNL            Store length of PDSN
*                                                                       LB1001e
         EJECT                                                          LB1010e
*     * /*------------------------------------------------------*/
*     * /* Get DATASET NAME as 1st required parm                */
*     * /* - R4 Starting parms address                          */
*     * /* - R6 Parm Length                                     */
*     * /*------------------------------------------------------*/
         MVI   IDPREFIX,C'Y'       Assume USERID Prefix appending       LB1010e
PRMDSN   EQU   *
         CLC   =C'?ABOUT',0(R4)    ?ABOUT keyword ?                     LB1040h
         BE    ABOUT#GO            Branch to ABOUT                      LB1040h
         CLI   0(R4),C' '          Delimiter?
         BE    NEXTPRM             YES, next parm
         CLI   0(R4),C''''         Apostrophe in DSN?                   LB1010e
         BE    BUMPPRM             YES, bypass and try again!           LB1010e
         MVC   0(1,R7),0(R4)       NO, continue with dataset name
         LA    R7,1(R7)            Bump up position PDSN
         LA    R8,1(R8)            Bump up length of PDSN
         STH   R8,PDSNL            Store length of PDSN
         B     BUMPCNT             Jump to PARMIN position bump!        LB1010e
BUMPPRM  EQU   *                                                        LB1010e
         CLI   IDPREFIX,C'N'       USERID Prefix Y?                     LB1010e
         BE    BUMPCNT             YES, continue...                     LB1010e
         MVI   IDPREFIX,C'N'       NO, set USERID PREFIX to Y           LB1010e
BUMPCNT  EQU   *                                                        LB1010e
         LA    R4,1(R4)            Bump up position PARMIN              LB1010e
         BCT   R6,PRMDSN           Check again...
         B     PARMDONE            Done...
*                                                                       LB1001a
         EJECT                                                          LB1001a
*     * /*------------------------------------------------------*/
*     * /* Get other optional parms                             */
*     * /* - R4 Starting parms address                          */
*     * /*------------------------------------------------------*/
NEXTPRM  EQU   *
         LA    R4,1(R4)            Bump up position PARMIN
NEXT01   EQU   *
         CLC   KWDIR9,0(R4)        Keyword 'DIRECTORY'?                 LB1010b
         BE    KDIR#9              YES, process                         LB1010b
         CLC   KWDIR,0(R4)         Keyword 'DIR'?
         BE    KDIR                YES, process
         CLC   KWPNL,0(R4)         Keyword 'PNL'?
         BE    KPNL                YES, process
         CLC   KWABOUT,0(R4)       Keyword 'ABOUT'?
         BE    KABOUT              YES, process
         CLC   KWVOLUME,0(R4)      Keyword 'VOLUME('?                   LB1010b
         BE    PARMVOL6            YES, process                         LB1010b
         CLC   KWVOL,0(R4)         Keyword 'VOL('?
         BE    PARMVOL             YES, process
         CLC   KWDS,0(R4)          Keyword 'DS('?                       LB1001a
         BE    PARMDS              YES, process                         LB1001a
         CLC   KWDF,0(R4)          Keyword 'DF('?                       LB1001b
         BE    PARMDF              YES, process                         LB1001b
         CLC   KWFILE,0(R4)        Keyword 'FILE'?                      LB1001g
         BE    KFILE               YES, process                         LB1001g
         CLC   KWDL0,0(R4)         Keyword 'DL0'?                       LB1001h
         BE    KDL0                YES, process                         LB1001h
         CLC   KWDL1,0(R4)         Keyword 'DL1'?                       LB1001h
         BE    KDL0                YES, process                         LB1001h
         CLC   KWTSOB,0(R4)        Keyword 'TSOB'?                      LB1040c
         BE    KTSOB               YES, process                         LB1040c
         BCT   R6,NEXTPRM          Check again...
         B     PARMDONE            Done
*
         EJECT
*     * /*------------------------------------------------------*/
*     * /* Keyword Delimiter Check                              */      LB1010g
*     * /* - R0 Working register                                */
*     * /* - R6 Parm Length                                     */
*     * /*------------------------------------------------------*/
KDIR#9   EQU  *                                                         LB1010b
         LA    R0,(L'KWDIR9)                                            LB1010b
         CR    R6,R0               End of PARMS?                        LB1010b
         BE    PARMDIR             YES, flag it DIR                     LB1010b
         CLI   L'KWDIR9(R4),C' '   Blank follow keyword?                LB1010b
         BE    PARMDIR             YES, flag it DIR                     LB1010b
         B     NEXTPRM             NO, continue checking...             LB1010b
KDIR     EQU  *
         LA    R0,(L'KWDIR)
         CR    R6,R0               End of PARMS?
         BE    PARMDIR             YES, flag it DIR
         CLI   L'KWDIR(R4),C' '    Blank follow keyword?
         BE    PARMDIR             YES, flag it DIR
         B     NEXTPRM             NO, continue checking...
KPNL     EQU  *
         LA    R0,(L'KWPNL)
         CR    R6,R0               End of PARMS?
         BE    PARMPNL             YES, flag it PNL
         CLI   L'KWPNL(R4),C' '    Blank follow keyword?
         BE    PARMPNL             YES, flag it PNL
         B     NEXTPRM             NO, continue checking...
KABOUT   EQU  *
         LA    R0,(L'KWABOUT)
         CR    R6,R0               End of PARMS?
         BE    PARMABOT            YES, flag it ABOUT
         CLI   L'KWABOUT(R4),C' '  Blank follow keyword?
         BE    PARMABOT            YES, flag it ABOUT
         B     NEXTPRM             NO, continue checking...
KFILE    EQU  *                                                         LB1010g
         LA    R0,(L'KWFILE)                                            LB1010g
         CR    R6,R0               End of PARMS?                        LB1010g
         BE    PARMFILE            YES, flag it FILE                    LB1010g
         CLI   L'KWFILE(R4),C' '   Blank follow keyword?                LB1010g
         BE    PARMFILE            YES, flag it FILE                    LB1010g
         B     NEXTPRM             NO, continue checking...             LB1010g
KDL0     EQU  *                                                         LB1010h
         LA    R0,(L'KWDL0)                                             LB1010h
         CR    R6,R0               End of PARMS?                        LB1010h
         BE    PARMDL0             YES, flag it DLn                     LB1010h
         CLI   L'KWDL0(R4),C' '    Blank follow keyword?                LB1010h
         BE    PARMDL0             YES, flag it DLn                     LB1010h
         B     NEXTPRM             NO, continue checking...             LB1010h
KTSOB    EQU  *                                                         LB1040c
         LA    R0,(L'KWTSOB)                                            LB1040c
         CR    R6,R0               End of PARMS?                        LB1040c
         BE    PARMTSOB            YES, flag it TSOB                    LB1040c
         CLI   L'KWTSOB(R4),C' '   Blank follow keyword?                LB1040c
         BE    PARMTSOB            YES, flag it TSOB                    LB1040c
         B     NEXTPRM             NO, continue checking...             LB1040c
*
         EJECT
*     * /*------------------------------------------------------*/
*     * /* Keyword Set                                          */      LB1010g
*     * /* - R4 Starting parms address                          */
*     * /* - R0 Keyword Length                                  */
*     * /*------------------------------------------------------*/
PARMDIR  EQU   *
         MVI   PDIR,C'Y'           Mark DIR action ON
         LA    R4,L'KWDIR(R4)      Bump up position PARMIN
         LA    R0,(L'KWDIR)
         B     PARM@DEC            Decrement...
PARMPNL  EQU   *
         MVI   PPNL,C'Y'           Mark PNL action ON
         LA    R4,L'KWPNL(R4)      Bump up position PARMIN
         LA    R0,(L'KWPNL)
         B     PARM@DEC            Decrement...
PARMABOT EQU   *
         MVI   PABOUT,C'Y'         Mark ABOUT action ON
         LA    R4,L'KWABOUT(R4)    Bump up position PARMIN
         LA    R0,(L'KWABOUT)
         B     PARM@DEC            Decrement...
PARMFILE EQU   *                                                        LB1010g
         MVI   PFILE,C'Y'          Mark FILE action ON                  LB1010g
         LA    R4,L'KWFILE(R4)     Bump up position PARMIN              LB1010g
         LA    R0,(L'KWFILE)                                            LB1010g
         B     PARM@DEC            Decrement...                         LB1010g
PARMDL0  EQU   *                                                        LB1010h
**       MVI   PDL,C'0'            Mark DL0  action                     LB1010h
         MVI   PDIR,C'Y'           Mark DIR action ON  IMPLY            LB1010h
         MVC   PDL,L'KWDL0-1(R4)   Mark DL0  action                     LB1010h
         LA    R4,L'KWDL0(R4)      Bump up position PARMIN              LB1010h
         LA    R0,(L'KWDL0)                                             LB1010h
         B     PARM@DEC            Decrement...                         LB1010h
PARMTSOB EQU   *                                                        LB1040c
         MVI   PTSOB,C'Y'          Mark TSOB action ON                  LB1040c
         LA    R4,L'KWTSOB(R4)     Bump up position PARMIN              LB1040c
         LA    R0,(L'KWTSOB)                                            LB1040c
         B     PARM@DEC            Decrement...                         LB1040c
*     *
*     * /*------------------------------------------------------*/
*     * /* Decrement command buffer scanning position           */
*     * /* - R4 Starting parms address                          */
*     * /* - R0 Keyword Length                                  */
*     * /* - R6 Parm Length                                     */
*     * /*------------------------------------------------------*/
PARM@DEC EQU   *
         SR    R6,R0               Decrement R6 accordingly
         BZ    PARMDONE            Done when R6 is ZERO
         BM    PARMDONE             ... or NEGATIVE
         BCT   R6,NEXTPRM          Check again
         B     PARMDONE            Done
*
         EJECT
*     * /*------------------------------------------------------*/
*     * /* VOL keyword and subfield process                     */
*     * /* - R4 Starting parms address                          */
*     * /* - R0 Keyword Length                                  */
*     * /* - R6 Parm Length                                     */
*     * /* - R7, R8 Working register                            */
*     * /*------------------------------------------------------*/
PARMVOL6 EQU   *                   YES, process                         LB1010b
         MVI   PVOL,C'Y'           Mark VOL specified action ON         LB1010b
         LA    R4,L'KWVOLUME(R4)   Bump up position PARMIN              LB1010b
         LA    R0,(L'KWVOLUME) 1                                        LB1010b
         B     PARMVGO                                                  LB1010b
PARMVOL  EQU   *
         MVI   PVOL,C'Y'           Mark VOL specified action ON
         LA    R4,L'KWVOL(R4)      Bump up position PARMIN
         LA    R0,(L'KWVOL) 1
PARMVGO  EQU   *                                                        LB1010b
         SR    R6,R0               Decrement R6 accordingly
         BZ    PARMDONE            Done when R6 is ZERO
         BM    PARMDONE             ... or NEGATIVE
LOADVOL  EQU   *
         MVI   OBTVOL,C' '         Clear VOL
         MVC   OBTVOL+1(L'OBTVOL-1),OBTVOL
         LA    R7,OBTVOL           Start of OBTVOL area
         LA    R8,L'OBTVOL         Max limit for VOL
VOL00    EQU   *
         CLI   0(R4),C' '          Delimiter?
         BE    NEXTPRM             YES, branch to NEXTPRM
         CLI   0(R4),C')'          Delimiter?
         BE    NEXTPRM             YES, branch to NEXTPRM
         MVC   0(1,R7),0(R4)       NO, continue with volume
         LA    R4,1(R4)            Bump up position PARMIN
         LA    R7,1(R7)            Bump up position PDSN
         BCTR  R8,0                Decrement R8
         LTR   R8,R8               0?  VOL max length used?
         BZ    VOL00A              YES, branch to NEXTPRM via BCT
         BCT   R6,VOL00            Check again... next VOL character
         B     PARMDONE            Done...
VOL00A   BCT   R6,NEXTPRM          Check again... next parm
         B     PARMDONE            Done...
*                                                                       LB1001a
         EJECT                                                          LB1001a
*     * /*------------------------------------------------------*/      LB1001a
*     * /* DS    keyword delimiter check                        */      LB1001a
*     * /* - R4 Starting parms address                          */      LB1001a
*     * /*------------------------------------------------------*/      LB1001a
PARMDS   EQU   *                                                        LB1001a
         LA    R4,L'KWDS(R4)       Bump up position PARMIN              LB1001a
LOADDS   EQU   *                                                        LB1001a
         CLI   0(R4),C'D'          Date Separator DASH?                 LB1001a
         BE    LOADDSD             YES, load DASH                       LB1001a
         CLI   0(R4),C'S'          Date Separator SLASH?                LB1001a
         BE    LOADDSS             YES, load SLASH                      LB1001a
         CLI   0(R4),C'P'          Date Separator PERIOD?               LB1001a
         BE    LOADDSP             YES, load PERIOD                     LB1001a
         B     LOADDSX             Use Date Separator Default value     LB1001a
LOADDSS  EQU   *                                                        LB1001a
         MVI   DATESEP,C'/'        SLASH...                             LB1001a
         B     LOADDSX                                                  LB1001a
LOADDSD  EQU   *                                                        LB1001a
         MVI   DATESEP,C'-'        DASH...                              LB1001a
         B     LOADDSX                                                  LB1001a
LOADDSP  EQU   *                                                        LB1001a
         MVI   DATESEP,C'.'        PERIOD...                            LB1001a
         B     LOADDSX                                                  LB1001a
LOADDSX  EQU   *                   Done....                             LB1001a
         LA    R4,1(R4)            Bump up position PARMIN              LB1001a
         B     NEXTPRM             Go get NEXTPRM                       LB1001a
*                                                                       LB1001b
         EJECT                                                          LB1001b
*     * /*------------------------------------------------------*/      LB1001b
*     * /* DF    keyword delimiter check                        */      LB1001b
*     * /* - R4 Starting parms address                          */      LB1001b
*     * /*------------------------------------------------------*/      LB1001b
PARMDF   EQU   *                                                        LB1001b
         LA    R4,L'KWDF(R4)       Bump up position PARMIN              LB1001b
LOADDF   EQU   *                                                        LB1001b
         CLI   0(R4),C'1'          Date Format < 1?                     LB1001b
         BL    LOADDFX             YES, use default                     LB1001b
         CLI   0(R4),C'4'          Date Format > 4?                     LB1001b
         BH    LOADDFX             YES, use default                     LB1001b
*        B     LOADDSX             Use Date Separator Default value     LB1001b
LOADDFT  EQU   *                                                        LB1001b
         MVC   DATEFMT,0(R4)       Store date format from keyword       LB1001b
         B     LOADDFX                                                  LB1001b
LOADDFX  EQU   *                   Done....                             LB1001b
         LA    R4,1(R4)            Bump up position PARMIN              LB1001b
         B     NEXTPRM             Go get NEXTPRM                       LB1001b
*                                                                       LB1001b
         EJECT                                                          LB1001b
*     * /*------------------------------------------------------*/
*     * /* Parm processing complete                             */
*     * /*   Check for FILE DDN to DSN processing               */      LB1010g
*     * /*   Check for appending USERID as prefix               */      LB1010e
*     * /* -ReasonCode 29 Dataset name must be specified        */
*     * /* - R4, R6, R7, R8 Working register                    */      LB1010e
*     * /*------------------------------------------------------*/
PARMDONE EQU   *                   Done with SCAN
         MVC   DDSNL,PDSNL         Dataset name length from PARMIN      LB1010e
         MVC   DDSN,PDSN           Dataset name from PARMIN             LB1010e
         CLI   PFILE,C'Y'          FILE requested, using DDN?           LB1010g
         BE    CHKDDNL             YES, go check valid DDN length?      LB1010g
*                                  NO, must be DDSN request             LB1010g
         CLI   IDPREFIX,C'Y'       USERID Prefix needed for DSN?        LB1010e
         BNE   PARMDCNT            NO, continue with DSN as-is          LB1010e
*                                                                       LB1010e
*     * /********************************************************/      LB1040c
*     * /* Check if we are running under TSO                    */      LB1040c
*     * /* - R4 Working Register                                */      LB1040b
*     * /* -JMSG'LISTDSJ  4099 -Must run under TSO'             */      LB1040c
*     * /********************************************************/      LB1040c
         CLI   PTSOB,C'Y'          Bypass TSO test?                     LB1040c
         BE    BYPASTSO            Yes.                                 LB1040c
*                                  No. PSA is at address 0              LB1040c
         L     R4,PSAAOLD-PSA(0)   R4=ASCB Address                      LB1040b
         L     R4,ASCBTSB-ASCB(R4) R4=ASCBTSB Address                   LB1040b
         LTR   R4,R4               If 0, program not under TSO          LB1040b
         BZ    ERR4099             ..error, not under TSO!              LB1040c
BYPASTSO EQU   *                                                        LB1040c
*     *                                                                 LB1040c
         EJECT                                                          LB1010e
*     * /*------------------------------------------------------*/      LB1010e
*     * /* DSN, apply USERID prefix                             */      LB1010e
*     * /*------------------------------------------------------*/      LB1010e
APPENDID EQU   *                   YES, add prefix to DSN               LB1010e
         L     R4,MYCPPLP          Point to CPPL                        LB1010e
         USING CPPL,R4             Tell Assembler                       LB1010e
         L     R6,CPPLUPT          Point to UPT                         LB1010e
         USING UPT,R6              Tell Assembler                       LB1010e
         SR    R7,R7               Clear R7                             LB1010e
         IC    R7,UPTPREFL         Prefix ID Length                     LB1010e
         STH   R7,MYIDL            Store Prefix ID Length               LB1010e
         MVC   MYID,UPTPREFX       Store Prefix ID                      LB1010e
         DROP  R4                                                       LB1010e
         DROP  R6                                                       LB1010e
*                                                                       LB1010e
         MVI   DDSN,C' '           Clear DDSN                           LB1010e
         MVC   DDSN+1(L'DDSN-1),DDSN                                    LB1010e
*                                                                       LB1010e
         LA    R4,DDSN             Address of DDSN                      LB1010e
         SR    R6,R6               Clear R6                             LB1010e
         LH    R6,MYIDL            Length  of DDSN, set to MYIDL        LB1010e
         LA    R7,MYID             Address of MYID                      LB1010e
*                                                                       LB1010e
         EJECT                                                          LB1010e
DOUSERID EQU   *                   Move USERID prefix to DDSN           LB1010e
         BCTR  R6,0                Adjust length for EX                 LB1010e
         EX    R6,MVCMYID          EX move of PDSN                      LB1010e
         LA    R6,1(R6)            Reset   DDSN length                  LB1010e
         AR    R4,R6               Bump up DDSN position                LB1010e
         B     DOPERIOD            Add period after USERID              LB1010e
MVCMYID  MVC   0(0,R4),MYID        Move MYID for EX                     LB1010e
DOPERIOD EQU   *                                                        LB1010e
         MVI   0(R4),C'.'          Move period after USERID             LB1010e
         LA    R4,1(R4)            Bump up DDSN position                LB1010e
         LA    R6,1(R6)            Bump up DDSN length                  LB1010e
DOPDSN0  EQU   *                                                        LB1010e
         SR    R7,R7               Clear R7                             LB1010e
         LH    R7,PDSNL            Length of PDSN                       LB1010e
         AR    R7,R6               Add current DDSN length              LB1010e
         C     R7,=F'44'           DDSN within 44 byte limit?           LB1010e
         BNH   DOPDSN              YES, move PDSN portion               LB1010e
DOPDSN44 EQU   *                   NO, move PDSN portion truncated!     LB1010e
         LA    R7,44               Initialize DSSNL                     LB1010e
         STH   R7,DDSNL              to 44                              LB1010e
         SR    R7,R6               44 - current DDSNL                   LB1010e
         EX    R7,MVCPDSN          EX move of PDSN                      LB1010e
         B     PARMDCNT                                                 LB1010e
DOPDSN   EQU   *                                                        LB1010e
         SR    R7,R7               Clear R7                             LB1010e
         STH   R7,DDSNL            Length of DDSN                       LB1010e
         LH    R7,PDSNL            Length of PDSN                       LB1010e
         BCTR  R7,0                Adjust length for EX                 LB1010e
         EX    R7,MVCPDSN          EX move of PDSN                      LB1010e
         B     PARMDCNT                                                 LB1010e
MVCPDSN  MVC   0(0,R4),PDSN        Move PDSN for EX                     LB1010e
*     * /*------------------------------------------------------*/      LB1010e
*     * /* DSN, use as-is                                       */      LB1010e
*     * /*------------------------------------------------------*/      LB1010e
PARMDCNT EQU   *                                                        LB1010e
**       STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT   LB1010e
**       TPUT  DDSN,44                                                  LB1010e
**       LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT   LB1010e
         CLI   PDSN,C' '           Have a DSN?
         BE    RSN29               NO, errror Reasoncode 29
         B     PARMCNT2            NO, continue with DDSN request       LB1010g
*                                                                       LB1010g
         EJECT                                                          LB1010g
*     * /*------------------------------------------------------*/      LB1010e
*     * /* DDN, check valid length and locate DSN from DDN      */      LB1010e
*     * /* -ReasonCode 28 DDNAME must be 1-8 characters         */      LB1010e
*     * /* -ReasonCode 90 DDN not allocated (not found)         */      LB1010e
*     * /*------------------------------------------------------*/      LB1010e
CHKDDNL  EQU   *                                                        LB1010g
         CLC   DDSNL,=H'8'         DDN length valid?                    LB1010g
         BH    RSN28               NO, error  Reasoncode 28             LB1010g
FINDDDN  EQU   *                   YES, find DDN in TIOT                LB1010g
         L     R4,CVTPTR           Address of CVT                       LB1010g
         USING CVT,R4              Tell Assembler, CVT                  LB1010g
         L     R7,CVTTCBP          Addrs (2 FW) pointing to TCB         LB1010g
         DROP  R4                                                       LB1010g
         MVC   DW,0(R7)            Save to DW                           LB1010g
         L     R7,DW+4             Take 2nd FW as TCB Addr              LB1010g
         USING TCB,R7              Tell Assembler, TCB                  LB1010g
         L     R4,TCBTIO           Load TIOT Address                    LB1010g
         DROP  R7                                                       LB1010g
         USING TIOT,R4             Tell Assembler, TIOT                 LB1010g
         LA    R7,TIOENTRY         Load TIOENTRY address                LB1010g
         USING TIOENTRY,R7         Tell Assembler, TIOENTRY             LB1010g
LOOPDDN  EQU   *                   YES, find DDN in TIOT                LB1010g
         CLI   TIOELNGH,0          End of TIOT?                         LB1010g
         BE    RSN90               Yes, error RSN90,DDN not found       LB1010g
*                                  No, keep looking for DDN...          LB1010g
         CLC   DDSN(8),TIOEDDNM    DDNAME = TIOT entry?                 LB1010g
         BE    GOODDDN             Yes, found it!                       LB1010g
         SR    R8,R8               Clear R8                             LB1010g
         IC    R8,TIOELNGH         TIOT Entry Length                    LB1010g
         AR    R7,R8               Bump to next TIOT entry              LB1010g
         B     LOOPDDN                                                  LB1010g
GOODDDN  EQU   *                   Found DDN                            LB1010g
         XC    FW,FW               Clear FW (fullword)                  LB1010g
         MVC   FW+1(3),TIOEJFCB    FW contains addr of JFCB for DDN     LB1010g
         L     R8,FW               R8 = JFCB addrs                      LB1010g
         A     R8,=F'16'           R8 = JFCB+16 (for offset)            LB1010g
         USING JFCB,R8             Tell Assembler, JFCB                 LB1010g
         MVC   DDSN,JFCBDSNM       Move associated dataset name         LB1010g
         DROP  R4                  Tell Assembler to give back          LB1010g
         DROP  R7                    ... R4, R7                         LB1010g
         DROP  R8                    ... and R8                         LB1010g
         LA    R4,0                Length of dataset name               LB1010g
         LA    R7,44               MAX length of dataset name           LB1010g
         LA    R8,DDSN             Address of dataset name variable     LB1010g
CALCDSNL EQU   *                   Calculate DDSN length                LB1010g
         CLI   0(R8),C' '          Are we done?                         LB1010g
         BE    CALCEXIT            YES, move dataset name               LB1010g
         LA    R4,1(R4)            Bump up dataset name length          LB1010g
         LA    R8,1(R8)            Bump up to next dataset name char    LB1010g
         BCT   R7,CALCDSNL         Do again...                          LB1010g
CALCEXIT EQU   *                                                        LB1010g
         STH   R4,DDSNL            Store new length                     LB1010g
*                                                                       LB1010g
PARMCNT2 EQU   *                                                        LB1010g
         MVC   CAMDSN,DDSN         YES, Load DSN from DDSN data
         MVC   PVOLSER,OBTVOL      Save VOLSER from PARM data
*
         EJECT
*     * /********************************************************/
*     * /* Display ABOUT information on terminal                */
*     * /* and exit.  No dataset processing done...             */
*     * /* -ReasonCode 99 ABOUT request, no DSN info retrieved  */
*     * /********************************************************/
ABOUT#   EQU   *
         CLI   PABOUT,C'Y'         Display ABOUT Info?
         BNE   ABOUT#XT            NO.
ABOUT#GO EQU   *                                                        LB1040h
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
         TPUT  MYSTAMP,MYSTAMPL
         LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
         MVC   DJMSG(MYSTAMPL),MYSTAMP     Move to &SYSLISTDSJMSG
         MVC   DRSNCODE,=C'99'     ABOUT, no DSN info msg
         MVC   RCEXIT,=F'00'
         MVC   RC,=C'00'
         B     ENDME
ABOUT#XT EQU   *
         EJECT
*     * /********************************************************/
*     * /* Locate dataset name via catalog                      */
*     * /* #Data source: 00 SYSDSNAME  (1 of 02)                */
*     * /* #Data source: 41 SYSDSCAT                            */
*     * /* #Data source: 42 SYSDSCATV                           */
*     * /* -ReasonCode 05 DSN not catalogued                    */
*     * /* -JMSG'RC=xxx DSCB-Fx CAMLST NAME error'              */
*     * /* - R3 Working Register                                */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
LOC8DSN  EQU   *
         LA    R3,0                ZERO R3
         ST    R3,CAMLIST           ... FW1 of CAMLIST
         MVI   CAMLIST,X'44'       Indicate NAME option
         LA    R3,CAMDSN           Addr of DSN
         ST    R3,CAMLIST+4         ... FW2 of CAMLIST
         LA    R3,0                ZERO R3
         ST    R3,CAMLIST+8         ... FW3 of CAMLIST
         LA    R3,LOCATEA          Addr of LOCATEA
         ST    R3,CAMLIST+12        ... FW4 of CAMLIST
*
         LOCATE  CAMLIST           Search for DSN via catalog
         LTR   R15,R15             Found DSN?
         BZ    DSNFOUND            YES, branch to DSNFOUND
*                                  NO, dataset not on catalog
         MVI   DDSCAT,C'N'         Dataset catalogued NO
         CLI   PVOL,C'Y'           Volume parm specified?
         BE    DSCBF1              YES, attempt fetch per given VOL
*                                  NO. Error DSN not found in catlg
         ST    R15,SAVER15                                              LB2000d
         MVC   RCEXIT,=F'0020'     Dataset not on catalog               LB2000d
         BAL   R7,MSGPRCR2                                              LB2000d
         LTR   R15,R15                                                  LB2000d
         BNZ   LOC8DSNB                                                 LB2000d
         L     R15,SAVER15                                              LB2000d
         BAL   R7,R15DJMSG         R15 rc on DJMSG                      LB2000f
*        CVD   R15,DW              Convert to decimal
*        UNPK  DJMSG+3(3),DW+6(2)  Unpack and
*        OI    DJMSG+5,X'F0'       ... force and F zone
         MVC   DJMSG+07(8),=C'LOCATE  '
LOC8DSNB EQU   *                                                        LB2000d
         MVC   DRSNCODE,=C'05'     Dataset not catalogued
         MVC   RCEXIT,=F'16'
         MVC   RC,=C'16'
         B     ENDME               Done, exit.
*
DSNFOUND EQU   *
         MVI   DDSCAT,C'Y'         Dataset catalogued YES
         MVC   DDSCATV,LOCAVSER    Dataset catalogued VOLUME
         MVC   DDSN,CAMDSN         Dataset name from catalog
*
         EJECT
*     * /********************************************************/
*     * /* Number of Volumes for this dataset name              */
*     * /* #Data source: 40 SYSNUMVOLS                          */
*     * /* - R5 Working Register                                */
*     * /********************************************************/
NUMVOL   EQU   *
         XC    FW,FW               Clear contents
         MVC   FW+2(2),LOCA#VOL    Get Number of Volumes
         L     R5,FW                ... and convert
         CVD   R5,DW                ... to decimal
         UNPK  DNUMVOL,DW+6(2)      Unpack and
         OI    DNUMVOL+2,X'F0'      ... force and F zone
NUMVOLXT EQU   *
*     *
*     * /********************************************************/
*     * /* Volume list for multivolume datasets                 */
*     * /* #Data source: 45 SYSVOLUMES                          */
*     * /* Limited by DVOLLST# and DVOLLST                      */
*     * /* - R5 contains number of volumes from NUMVOL above    */
*     * /* - R3, R4, R5, R6 Working Register                    */
*     * /********************************************************/
         MVI   DVOLLST,C' '        Clear DVOLLST
         MVC   DVOLLST+1(L'DVOLLST-1),DVOLLST
         LA    R3,DVOLLST          R3=addrs of DVOLLST
         LA    R4,LOCAVSER         R4=addrs of LOCATEA VOLSER
         LA    R6,DVOLLST#         R6=max DVOLLST volumes
         CR    R5,R6               R5>=R6?
         BE    VOLLST              NO,  branch to VOLLST
         BL    VOLLST              NO,  branch to VOLLST
         LR    R5,R6               YES. Init R5 to R6
VOLLST   EQU   *
         MVC   0(6,R3),0(R4)       Move VOLUME to list
         LA    R4,LOCAVLLN(R4)     Point to next VOLUME
         LA    R3,7(R3)            Point to next Vol List
         BCT   R5,VOLLST           Do R5 times
VOLLSTXT EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Check for multiple volumes and DASD device           */
*     * /* #Data source: 21 SYSVOLUME   (1 of 02)               */
*     * /* -ReasonCode 19 Data set on multiple volumes          */
*     * /* -ReasonCode 26 Data set on MSS (Mass Storage) device */
*     * /********************************************************/
CHKVOLS  EQU   *                   Check for MULTI volumes
         CLC   LOCA#VOL,=X'0001'   Volumes = 1?
         BE    SINGLVOL
         CLC   LOCA#VOL,=X'0000'   Volumes = 0?
         BE    DSNOVOL                                                  LB1040f
MULTIVOL EQU   *
         MVC   DRSNCODE,=C'19'     Dataset is on mutilple volumes
         MVC   RCEXIT,=F'16'
         MVC   RC,=C'16'
         B     ENDME               Done, exit.
SINGLVOL EQU   *
         MVC   DVOLSER,LOCAVSER    LOAD VOLSER FROM CAMLST WORKAREA
         MVC   OBTVOL(6),DVOLSER   MOVE VOLSER TO OBTVOL
         CLI   LOCADEVC,X'80'      Dataset on Tape?
         BE    ONTAPE              YES.
ONDASD   EQU   *                   NO, DASD entry
         B     CHKVOLSX
ONTAPE   EQU   *
         MVC   DRSNCODE,=C'26'     Dataset is on MSS device
         MVC   RCEXIT,=F'16'
         MVC   RC,=C'16'
         B     ENDME               Done, exit.
DSERR    EQU   *
         MVC   DRSNCODE,=C'06'     Error obtaining dataset
         MVC   RCEXIT,=F'16'
         MVC   RC,=C'16'
         B     ENDME               Done, exit.
DSNOVOL  EQU   *                                                        LB1040f
         MVC   DRSNCODE,=C'27'     No VOLSER allocated to dataset       LB1040f
         MVC   RCEXIT,=F'16'                                            LB1040f
         MVC   RC,=C'16'                                                LB1040f
         B     ENDME               Done, exit.                          LB1040f
CHKVOLSX EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Get Format-1 IDENTIFIER DATA SET CONTROL BLOCK       */
*     * /* #Data source: 00 SYSDSNAME  (1 of 02)                */
*     * /* #Data source: 21 SYSVOLUME   (2 of 02)               */
*     * /* -ReasonCode 21 Catalog error locating dataset        */
*     * /* -ReasonCode 22 Volume not mounted                    */
*     * /* -JMSG'RC=xxx DSCB-Fx CAMLST SEARCH error'            */
*     * /* - R3 Working Register                                */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
DSCBF1   EQU   *
         CLI   PVOL,C'Y'           Volume specified?
         BNE   DSCBF1A             NO, branch to fetch format-1 DSCB
*                                  YES.
         MVC   DNUMVOL,=C'001'      Number of volumes = 1
         MVC   OBTVOL,PVOLSER       Override OBTVOL per given VOL
         MVC   DVOLSER,PVOLSER      LOAD VOLSER FROM given VOL
DSCBF1A  EQU   *
         LA    R3,0                ZERO R3
         ST    R3,CAMLIST           ... FW1 of CAMLIST
         MVI   CAMLIST,X'C1'       Request SEARCH
         LA    R3,CAMDSN           Addr of DSN
         ST    R3,CAMLIST+4         ... FW2 of CAMLIST
         LA    R3,OBTVOL           Addr of OBTVOL
         ST    R3,CAMLIST+8         ... FW3 of CAMLIST
         LA    R3,OBTAINA          Addr of OBTAINA
         ST    R3,CAMLIST+12        ... FW4 of CAMPLIST
*
         OBTAIN  CAMLIST           Get F1 DSCB
         LTR   R15,R15             Successful?
         BZ    OBTR00              YES.
*                                  NO.
         ST    R15,SAVER15                                              LB2000d
         MVC   RCEXIT,=F'0021'     Free allocation error                LB2000d
         BAL   R7,MSGPRCR2                                              LB2000d
         L     R15,SAVER15                                              LB2000d
         BAL   R7,R15DJMSG         R15 rc on DJMSG                      LB2000f
*        CVD   R15,DW              Convert to decimal
*        UNPK  DJMSG+3(3),DW+6(2)  Unpack and
*        OI    DJMSG+5,X'F0'       ... force and F zone
         MVC   DJMSG+13(1),=C'1'   Mark as F1 request on err msg
*
         B     CAMRCERR
OBTR00   EQU   *
         MVC   DDSN,CAMDSN         Dataset name from F1
         MVC   DVOLSER,OBTVOL      Save VOLUME
*
         EJECT
*     * /********************************************************/
*     * /* Get Format-2 ISAM DATA SET CONTROL BLOCK             */
*     * /* -JMSG'RC=xxx DSCB-Fx CAMLST SEEK error'              */
*     * /* - R3 Working Register                                */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
DSCBF2   EQU   *
         TM    DS1DSORG+1,X'08'    VSAM ?                               LB1040d
         BO    DSORG               Yes, branch to DSORG                 LB1040d
         TM    DS1DSORG,X'80'      ISAM ?
         BNO   DSCBF3              NO, bypass DSCB-F2 and get DSCB-F3
         LA    R3,0                ZERO R3
         ST    R3,CAMLIST           ... FW1 of CAMLIST
         MVI   CAMLIST,X'C0'       Request SEEK
         MVI   CAMLIST+1,X'80'
         LA    R3,DS1PTRDS         CCHHR OF FORMAT 2 DSCB
         ST    R3,CAMLIST+4         ... FW2 of CAMLIST
         LA    R3,OBTVOL           Addr of OBTVOL
         ST    R3,CAMLIST+8         ... FW3 of CAMLIST
         LA    R3,IECSDSF2         Addr of FORMAT 2 DSCB
         ST    R3,CAMLIST+12        ... FW4 of CAMPLIST
*
         OBTAIN  CAMLIST           Get F2 DSCB (Extents)
         LTR   R15,R15             Successful?
         BZ    DSCBF2X             YES.
*                                  NO.
         ST    R15,SAVER15                                              LB2000d
         MVC   RCEXIT,=F'0022'     CAMLST SEEK error                    LB2000d
         BAL   R7,MSGPRCR2                                              LB2000d
         L     R15,SAVER15                                              LB2000d
         BAL   R7,R15DJMSG         R15 rc on DJMSG                      LB2000f
*        CVD   R15,DW              Convert to decimal
*        UNPK  DJMSG+3(3),DW+6(2)  Unpack and
*        OI    DJMSG+5,X'F0'       ... force and F zone
         MVC   DJMSG+13(1),=C'2'   Mark as F2 request on err msg
*
         B     CAMRCERR
DSCBF2X  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Get Format-3 EXTENSION DATA SET CONTROL BLOCK        */
*     * /* -JMSG'RC=xxx DSCB-Fx CAMLST SEEK error'              */
*     * /* - R3 Working Register                                */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
DSCBF3   EQU   *
         CLI   DS1NOEPV,3          Dataset > 3 Extents?
         BNH   DSCBF3X             NO.  F3 DSCB not needed!
         LA    R3,0                ZERO R3
         ST    R3,CAMLIST           ... FW1 of CAMLIST
         MVI   CAMLIST,X'C0'       Request SEEK
         MVI   CAMLIST+1,X'80'
         LA    R3,DS1PTRDS         CCHHR OF FORMAT 3 DSCB
         TM    DS1DSORG,X'80'      ISAM ?
         BNO   *+8                 NO.
         LA    R3,DS2PTRDS         CCHHR OF FORMAT 3 DSCB
         ST    R3,CAMLIST+4         ... FW2 of CAMLIST
         LA    R3,OBTVOL           Addr of OBTVOL
         ST    R3,CAMLIST+8         ... FW3 of CAMLIST
         LA    R3,IECSDSF3         Addr of FORMAT 3 DSCB
         ST    R3,CAMLIST+12        ... FW4 of CAMPLIST
*
         OBTAIN  CAMLIST           Get F3 DSCB (Extents)
         LTR   R15,R15             Successful?
         BZ    DSCBF3X             YES.
*                                  NO.
         ST    R15,SAVER15                                              LB2000d
         MVC   RCEXIT,=F'0022'     CAMLST SEEK error                    LB2000d
         BAL   R7,MSGPRCR2                                              LB2000d
         L     R15,SAVER15                                              LB2000d
         BAL   R7,R15DJMSG         R15 rc on DJMSG                      LB2000f
*        CVD   R15,DW              Convert to decimal
*        UNPK  DJMSG+3(3),DW+6(2)  Unpack and
*        OI    DJMSG+5,X'F0'       ... force and F zone
         MVC   DJMSG+13(1),=C'3'   Mark as F3 request on err msg
*
         B     CAMRCERR
DSCBF3X  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Get Format-4 DSCB for VOLUME device characterics     */
*     * /* -JMSG'RC=xxx DSCB-Fx CAMLST NAME error'              */
*     * /* - R3 Working Register                                */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
DSCBF4   EQU   *
         MVI   CAMDSN,X'04'        Set to F4 DSCB
         MVC   CAMDSN+1(L'CAMDSN-1),CAMDSN
*
         LA    R3,0                ZERO R3
         ST    R3,CAMLIST           ... FW1 of CAMLIST
         MVI   CAMLIST,X'C1'       INDICATE NAME   OPTION
         LA    R3,CAMDSN           Addr of DSN
         ST    R3,CAMLIST+4         ... FW2 of CAMLIST
         LA    R3,OBTVOL           Addr of OBTVOL
         ST    R3,CAMLIST+8         ... FW3 of CAMLIST
         LA    R3,IECSDSF4         Addr of FORMAT 4 DSCB
         ST    R3,CAMLIST+12        ... FW4 of CAMPLIST
*
         OBTAIN  CAMLIST           Get F4 DSCB (Volume Info)
         LTR   R15,R15             Successful?
         BZ    DSCBF4X             YES.
*                                  NO.
         ST    R15,SAVER15                                              LB2000d
         MVC   RCEXIT,=F'0020'     CAMLST NAME error                    LB2000d
         BAL   R7,MSGPRCR2                                              LB2000d
         L     R15,SAVER15                                              LB2000d
         BAL   R7,R15DJMSG         R15 rc on DJMSG                      LB2000f
*        CVD   R15,DW              Convert to decimal
*        UNPK  DJMSG+3(3),DW+6(2)  Unpack and
*        OI    DJMSG+5,X'F0'       ... force and F zone
         MVC   DJMSG+13(1),=C'4'   Mark as F4 request on err msg
*
         B     CAMRCERR
DSCBF4X  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Cylinders for Volume              from format-4 DSCB */
*     * /* #Data source: 29 SYSCYLVOL                           */
*     * /* - R3 Working Register                                */
*     * /********************************************************/
         LH    R3,DS4DEVSZ         Cylinders for volume
         ST    R3,CYL4VOL
         CVD   R3,DW               Convert to decimal
         UNPK  DCYL4VOL(5),DW+5(3) Unpack and
         OI    DCYL4VOL+4,X'F0'     ... force and F zone
*     *
*     * /********************************************************/
*     * /* Tracks per Cylinder for Volume    from format-4 DSCB */
*     * /* #Data source: 30 SYSTRKSCYL                          */
*     * /* - R3 Working Register                                */
*     * /********************************************************/
         LH    R3,DS4DEVSZ+2       Tracks/Cylinder
         ST    R3,TRKSCYL
         CVD   R3,DW               Convert to decimal
         UNPK  DTRKSCYL(5),DW+5(3) Unpack and
         OI    DTRKSCYL+4,X'F0'     ... force and F zone
*     *
*     * /********************************************************/
*     * /* Track Length for volume           from format-4 DSCB */
*     * /* #Data source: 34 SYSTRKLEN                           */
*     * /* - R3 Working Register                                */
*     * /********************************************************/
         LH    R3,DS4DEVTK         Track Length
         ST    R3,TRKLEN
         CVD   R3,DW               Convert to decimal
         UNPK  DTRKLEN(5),DW+5(3)  Unpack and
         OI    DTRKLEN+4,X'F0'      ... force and F zone
*
         EJECT
*     * /********************************************************/
*     * /* Capacity for volume  =                               */
*     * /*   Cylinders for Volume * Tracks per Cylinder *       */
*     * /*   Track Length                                       */
*     * /* #Data source: 35 SYSUNITCAP                          */
*     * /* - R2, R3 Working Registers                           */
*     * /********************************************************/
         L     R3,CYL4VOL          R3=CYLS for volume
         M     R2,TRKSCYL          (R2,R3)=CYL4VOL*TRKSCYL
*              R3                  tracks for volume
         M     R2,TRKLEN           (R2,R3)=R3*TRKLEN
         CVD   R3,DW               Convert to decimal
         UNPK  DUNITCAP,DW         Unpack and
         OI    DUNITCAP+14,X'F0'    ... force and F zone
*     *
*     * /********************************************************/
*     * /* Blocks per Track =                                   */
*     * /*   Track Length / Blocksize                           */
*     * /* #Data source: 36 SYSBLKSTRK                          */
*     * /* - R2, R3 Working Registers                           */
*     * /********************************************************/
         SR    R3,R3               Zero R3
         CLC   DS1BLKL,=H'0'       Is BLOCKSIZE 0?
         BE    BLKTRKC             YES, BLKTRKC is 0
         L     R3,TRKLEN           NO, compute BLKTRKC
         M     R2,=F'1'
         XC    FW,FW               Clear FW
         MVC   FW+2(2),DS1BLKL     Move BLOCKSIZE
         D     R2,FW               TrackLength/Blocksize
BLKTRKC  EQU   *
         CVD   R3,DW               Convert to decimal
         UNPK  DBLKTRK(5),DW+5(3)  Unpack and
         OI    DBLKTRK+4,X'F0'      ... force and F zone
         ST    R3,BLKSPTRK         Store blocks per track
*
         EJECT
*     * /********************************************************/
*     * /* Total Space Allocation                               */
*     * /* - Sum extents; 3 F1, 4 F3, 9 F3                      */
*     * /* - R1, R2, R3, R4, R5, R7 Working Registers           */
*     * /********************************************************/
SUMEXT   EQU   *
         CLI   DS1NOEPV,0          Extents to process?
         BE    SUMEXTXT            NO. Zero.
         SR    R2,R2               Zero, R2 = TRK Counter (HH)
         SR    R3,R3               Zero, R3 = CYL Counter (CC)
*     * /*------------------------------------------------------*/
*     * /* Sum-  1st 3 extents in F1 DSCB                       */
*     * /*------------------------------------------------------*/
         LA    R4,DS1EXT1          Addr of 1st extent (F1)
         LA    R5,3                3 times...
EXTF1    EQU   *
         CLI   0(R4),X'00'         Extent slot null?
         BE    SUMEXTXT            YES, Done.
         BAL   R7,TOTEXTS          Sum extents
         LA    R4,10(R4)           Point to next extent
         BCT   R5,EXTF1
*     * /*------------------------------------------------------*/
*     * /* Sum-  Count F3 DSCB extents?                         */
*     * /*------------------------------------------------------*/
         CLI   DS1NOEPV,3          Dataset > 3 Extents?
         BNH   SUMEXTXT            NO. Done...
*     * /*------------------------------------------------------*/
*     * /* Sum-  4 extents in F3 DSCB                           */
*     * /*------------------------------------------------------*/
         LA    R4,DS3EXTNT         Addr of 1st extent (F3)
         LA    R5,4                4 times...
EXTF3    EQU   *
         CLI   0(R4),X'00'         Extent slot null?
         BE    SUMEXTXT            YES, Done.
         BAL   R7,TOTEXTS          Sum extents
         LA    R4,10(R4)           Point to next extent
         BCT   R5,EXTF3
*     * /*------------------------------------------------------*/
*     * /* Sum-  last 9 extents in F3 DSCB                      */
*     * /*------------------------------------------------------*/
         LA    R4,DS3ADEXT         Addr of 1st extent (F3)
         LA    R5,9                9 times...
EXTF3B   EQU   *
         CLI   0(R4),X'00'         Extent slot null?
         BE    SUMEXTXT            YES, Done.
         BAL   R7,TOTEXTS          Sum extents
         LA    R4,10(R4)           Point to next extent
         BCT   R5,EXTF3B
*
SUMEXTXT EQU   *
*
         EJECT
*     * /*------------------------------------------------------*/
*     * /* TOTALS  R2=TRKS, R3=CYL                              */
*     * /*------------------------------------------------------*/
         LR    R1,R2               R1=TRKS accumulated
         M     R2,TRKSCYL          CYL to TRKS (R2,R3)
         AR    R3,R1               R3=Total TRKS allocated
*     *
*     * /********************************************************/
*     * /* Total Allocated Tracks                               */
*     * /* #Data source: 26 SYSTRKSALLOC                        */
*     * /********************************************************/
         CVD   R3,DW               Convert to decimal
         UNPK  DTRKSAL(5),DW+5(3)  Unpack and
         OI    DTRKSAL+4,X'F0'      ... force and F zone
         ST    R3,TRKSTOTA         Store tot alloc tracks
*     *
*     * /********************************************************/
*     * /* Total Allocated Cylinders                            */
*     * /********************************************************/
         LR    R1,R3               R1=Total Allocated Tracks
         D     R2,TRKSCYL          Revise CYL and TRK (R2,R3)
         ST    R3,CYLSTOTA         Store tot alloc cyls
*     *
*     * /*------------------------------------------------------*/
*     * /* TOTALS: R2=TRKS, R3=CYL
*     * /*------------------------------------------------------*/
*
         EJECT
*     * /********************************************************/
*     * /* Space Used in Tracks for PS and PO Organization      */
*     * /* - R1, R2 Working Registers                           */
*     * /********************************************************/
SPCUSED  EQU   *
         TM    DS1DSORG,X'40'     PS SEQ Dataset ?
         BO    SPCUSEDC           YES, continue.
         TM    DS1DSORG,X'02'     PO PDS Dataset ?
         BO    SPCUSEDC           YES, continue.
         XC    TTRKUSED,TTRKUSED  Clear Total Used Tracks
         B     SPCUSEDX           NO, Exit.
SPCUSEDC EQU   *
         SR    R2,R2              R2=Total Tracks Used
         ICM   R2,B'0011',DS1LSTAR  Last Block Written   TT of TTR
         CLI   DS1LSTAR+2,0       Portion of track used?  R of TTR
         BE    TOTUSED            NO.
         LA    R2,1(R2)           YES, add 1 to tracks
*     *
*     * /********************************************************/
*     * /* Total Used      Tracks                               */
*     * /* #Data source: 24 SYSTRKSUSED                         */
*     * /********************************************************/
TOTUSED  EQU   *
         ST    R2,TTRKUSED         Store total tracks used
         CVD   R2,DW               Convert to decmial
         UNPK  DTRKSUS(5),DW+5(3)  Unpack and
         OI    DTRKSUS+4,X'F0'      ... force and F zone
*     *
*     * /********************************************************/
*     * /* Total Unused    Tracks                               */
*     * /* #Data source: 28 SYSTRKSUNUSED                       */
*     * /********************************************************/
TOTUUSED EQU   *
         SR    R1,R2               R1=Unused tracks
         CVD   R1,DW               Convert to decmial
         UNPK  DTRKSUUS(5),DW+5(3) Unpack and
         OI    DTRKSUUS+4,X'F0'     ... force and F zone
SPCUSEDX EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Logical Record Length                                */
*     * /* #Data source: 10 SYSLRECL                            */
*     * /* - R3 Working Register                                */
*     * /********************************************************/
LRECL    EQU   *
         XC    FW,FW               Clear contents
         MVC   FW+2(2),DS1LRECL    Get LRECL
         L     R3,FW                ... and convert
         CVD   R3,DW                ... to decimal
         UNPK  DLRECL(5),DW+5(3)   Unpack and
         OI    DLRECL+4,X'F0'       ... force and F zone
LRECLXT  EQU   *
*     *
*     * /********************************************************/
*     * /* Blocksize                                            */
*     * /* #Data source: 11 SYSBLKSIZE                          */
*     * /* - R3 Working Register                                */
*     * /********************************************************/
BLKSIZ   EQU   *
         XC    FW,FW               Clear contents
         MVC   FW+2(2),DS1BLKL     Get BLKSIZE
         L     R3,FW                ... and convert
         CVD   R3,DW                ... to decimal
         UNPK  DBLKSZ(5),DW+5(3)   Unpack and
         OI    DBLKSZ+4,X'F0'       ... force and F zone
BLKSIZXT EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Keylength                                            */
*     * /* #Data source: 12 SYSKEYLEN                           */
*     * /* - R3 Working Register                                */
*     * /********************************************************/
KEYL     EQU   *
         XC    FW,FW               Clear contents
         MVC   FW+3(1),DS1KEYL     Get KEY LENGTH
         L     R3,FW                ... and convert
         CVD   R3,DW                ... to decimal
         UNPK  DKEYL(3),DW+6(2)    Unpack and
         OI    DKEYL+2,X'F0'        ... force and F zone
KEYLXT   EQU   *
*     *
*     * /********************************************************/
*     * /* Relative Key Position                                */
*     * /* #Data source: 13 SYSKEYPOS                           */
*     * /* - R3 Working Register                                */
*     * /********************************************************/
RKP      EQU   *
         XC    FW,FW               Clear contents
         MVC   FW+2(2),DS1RKP      Get RELATIVE KEY POSITION
         L     R3,FW                ... and convert
         CVD   R3,DW                ... to decimal
         UNPK  DRKP(5),DW+5(3)     Unpack and
         OI    DRKP+4,X'F0'         ... force and F zone
RKPXT    EQU   *
*     *
*     * /********************************************************/
*     * /* Extents Allocated                                    */
*     * /* #Data source: 25 SYSEXTENTS                          */
*     * /* - R3 Working Register                                */
*     * /********************************************************/
EXTS     EQU   *
         XC    FW,FW               Clear contents
         MVC   FW+3(1),DS1NOEPV    Get ALLOCATED EXTENTS
         L     R3,FW                ... and convert
         CVD   R3,DW                ... to decimal
         UNPK  DEXTS(3),DW+6(2)    Unpack and
         OI    DEXTS+2,X'F0'        ... force and F zone
EXTSXT   EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Dataset Organization                                 */
*     * /* #Data source: 08 SYSDSORG                            */
*     * /* -JMSG'DSORG Type xxx not defined in table'           */
*     * /* - R2, R3, R4, R5 Working Registers                   */
*     * /********************************************************/
DSORG    EQU   *
         LA    R3,TBLDSOB          Address of TBL
         LA    R4,TBLDSOE          Number of tbl entries
         SR    R5,R5               Clear R5
DSORGLP  EQU   *
         LA    R2,DS1DSORG         Address of DSORG
         AH    R2,0(R3)            Adj Addr of DSORG
         ICM   R5,B'0001',2(R3)    Load DSORG bit pattern
         EX    R5,TMDSORG          Execute TM
         BO    DSO#FND             Found DSORG Type
         LA    R3,TBLDSOL(R3)      Otherwise, check next
         BCT   R4,DSORGLP            ... TBLDSO entry
*                                  Exhausted TBLDS entries, not found
         ST    R15,SAVER15                                              LB2000d
         MVC   RCEXIT,=F'0025'     DSORG not defined                    LB2000d
         BAL   R7,MSGPRCR2                                              LB2000d
         L     R15,SAVER15                                              LB2000d
         MVC   DJMSG(10),=C'DSORG Type'
         XC    FW,FW
         MVC   FW+3(1),DS1DSORG
         L     R15,FW
         CVD   R15,DW              Convert to decimal
         UNPK  DJMSG+11(3),DW+6(2) Unpack and
         OI    DJMSG+13,X'F0'       ... force and F zone
INVLDSO  EQU   *                                                        LB1040g
         MVC   DRSNCODE,=C'03'     Data set type cannot be processed    LB1040g
         MVC   RCEXIT,=F'16'                                            LB1040g
         MVC   RC,=C'16'                                                LB1040g
         B     ENDME               Done, exit.                          LB1040g
*
DSO#FND  EQU   *
         MVC   DDSORG,3(R3)        Move DSORG type
DSOUNMOV EQU   *
         TM    DS1DSORG,B'00000001'    Unmovable?
         BZ    DSCHKVSM                YES, movable (default)           LB1040d
         MVI   DDSORG+2,C'U'           NO, unmovable
         B     DSCHKVSM                                                 LB1040d
DSCHKVSM EQU   *                                                        LB1040d
         TM    DS1DSORG+1,X'08'    VSAM?                                LB1040d
         BO    DEVTP               Yes, branch to device type (DEVTP)   LB1040d
         B     DSORGXT             No, continue...                      LB1040d
*                                                                       LB1040d
         EJECT                                                          LB1040d
*
TMDSORG  TM    0(R2),B'00000000'   DSORG TM instruction (EX)
*                         - TBLDSO Table---------------------------
*              6 bytes   /  halfword, offset relative to DS1DSORG
*              per entry+   01 bytes, DS1DSORG bit pattern
*                        \  03 bytes, file organization type
*                         -
TBLDSOB  DS    0H
         DC    H'0',B'10000000',C'IS '       ISAM
TBLDSOL  EQU   *-TBLDSOB                           ** Entry Length
         DC    H'0',B'01000000',C'PS '       Sequential
         DC    H'0',B'00100000',C'DA '       Direct
         DC    H'0',B'00000010',C'PO '       Partitioned
         DC    H'1',B'00001000',C'VS '       VSAM
TBLDSOE  EQU   (*-TBLDSOB)/TBLDSOL                 ** Num of entries

DSORGXT EQU    *
*
         EJECT
*     * /********************************************************/
*     * /* Length Attribute (F,V,U)                             */
*     * /* - R2, R3, R4, R5 Working Registers                   */
*     * /********************************************************/
LENTY    EQU   *
         MVI   DRECFM,C' '         Clear DRECFM
         MVC   DRECFM+1(L'DRECFM-1),DRECFM
         LA    R3,TBLRFM1B         Address of TBL
         LA    R4,TBLRFM1E         Number of tbl entries
         LA    R2,DS1RECFM         Address of DS1RECFM
         SR    R5,R5               Clear R5
LENTYLP  EQU   *
         ICM   R5,B'0001',0(R3)    Load DS1RECFM bit pattern
         EX    R5,TMDSRFM1         Execute TM
         BO    LEN#FND             Found RECFM Type
         LA    R3,TBLRFM1L(R3)     Otherwise, check next
         BCT   R4,LENTYLP            ... TBL entry
*                                  Exhausted TBL entries
         B     LENTYXT
LEN#FND  EQU   *
         MVC   DRECFM+0(1),TBLRFM1L-1(R3)
         B     LENTYXT
*
TMDSRFM1 TM    0(R2),B'00000000'   DSRFM TM instruction
*
*              2 bytes   /  TBLRFM1 Table--------------------------
*              per entry+   01 bytes, DS1RECFM bit pattern
*                        \  01 bytes, Length type
TBLRFM1B DS    0H
         DC    B'11000000',C'U'    Undefined Length
TBLRFM1L EQU   *-TBLRFM1B                          ** Entry Length
         DC    B'01000000',C'V'    Variable Length
         DC    B'10000000',C'F'    Fixed Length
TBLRFM1E EQU   (*-TBLRFM1B)/TBLRFM1L               ** Num of entries
*
LENTYXT  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Block attribute (B)                                  */
*     * /********************************************************/
BLK      EQU   *
         TM    DS1RECFM,B'00010000'    Blocked
         BO    BLK#B
         B     BLKXT
BLK#B    EQU   *
         MVI   DRECFM+1,C'B'           Blocked
         B     BLKXT
BLKXT    EQU   *
*     *
*     * /********************************************************/
*     * /* Span - Standard Attribute (S)                        */
*     * /********************************************************/
SPNCHR   EQU   *
         TM    DS1RECFM,B'00001000'   Span - Standard
         BO    SPN#S
         B     SPNCHRXT
SPN#S    EQU   *
         MVI   DRECFM+2,C'S'          Span - Standard
SPNCHRXT EQU   *
*     *
*     * /********************************************************/
*     * /* Track Overflow  Attribute (T)                        */
*     * /********************************************************/
OVFCHR   EQU   *
         TM    DS1RECFM,B'00100000'   Track Overflow
         BO    OVF#S
         B     OVFCHRXT
OVF#S    EQU   *
         MVI   DRECFM+3,C'T'          Track Overflow
OVFCHRXT EQU   *
*     *
*     * /********************************************************/
*     * /* ANSI Control Character Attribute (A,M)               */
*     * /********************************************************/
CTLCHR   EQU   *
         TM    DS1RECFM,B'00000100'  ANSI
         BO    CTL#A
         TM    DS1RECFM,B'00000010'  Machine Code
         BO    CTL#M
         B     CTLCHRXT
CTL#A    EQU   *                   ANSI Control Character
         MVI   DRECFM+4,C'A'
         B     CTLCHRXT
CTL#M    EQU   *                   Machine Code Control Character
         MVI   DRECFM+4,C'M'
         B     CTLCHRXT
CTLCHRXT EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Record Format - Concatenate RECFM characters         */
*     * /* #Data source: 09 SYSRECFM                            */
*     * /* - R2, R3, R4 Working Registers                       */
*     * /********************************************************/
RECFM    EQU   *
         MVI   DW,C' '             Clear DW Temp Workarea 8-bytes
         MVC   DW+1(L'DW-1),DW
         LA    R2,DRECFM           Load Addr of DRECFM
         LA    R3,L'DRECFM         Load Length of DRECFM
         LA    R4,DW               Load Addr of DW
RECFML   EQU   *
         CLI   0(R2),C' '          Blank?
         BE    RECFMLA             YES, skip, next DRECFM position
         MVC   0(1,R4),0(R2)       NO, move DRECFM pos to DW
         LA    R4,1(R4)            Point to next DW position
RECFMLA  EQU   *
         LA    R2,1(R2)            Point to next DRECFM position
         BCT   R3,RECFML           Do again.
         MVC   DRECFM,DW           Move temp work to DRECFM
RECFMXT  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Allocation Parameters   CYL,TRK,BLK,ABS              */
*     * /* Calculate SYSPRIMARY and SYSUSED                     */
*     * /* #Data source: 23 SYSUNITS                            */
*     * /* #Data source: 43 SYSPRIMARY                          */
*     * /* #Data source: 44 SYSUSED                             */
*     * /* - R2, R3, R5 Working Registers                       */
*     * /********************************************************/
SSPC     EQU   *
**       DISPLAY  DATA=(DS1SCALO,B,1)
         TM    DS1SCALO,X'C0'      Allocation- CYL
*        TM    DS1SCALO,B'11000000'  Allocation- CYL
         BO    SSPC#CYL
         TM    DS1SCALO,X'80'      Allocation- TRK
*        TM    DS1SCALO,B'10000000'  Allocation- TRK
         BO    SSPC#TRK
         TM    DS1SCALO,X'40'      Allocation- BLK
*        TM    DS1SCALO,B'01000000'  Allocation- BLK
         BO    SSPC#BLK
         B     SSPC#ABS            Must be Allocation - ABS
SSPC#CYL EQU   *
         MVC   DSSPC,=C'CYLINDER'  CYLINDER Allocation...               LB1010c
*     * /* Calculate SYSPRIMARY=CYLSTOTA                        */
         L     R5,CYLSTOTA         ... and convert
         CVD   R5,DW               ... to decimal
         UNPK  DPSPCAL,DW+5(3)     Unpack and
         OI    DPSPCAL+4,X'F0'     ... force and F zone
*     * /* Calculate SYSUSED=TTRKUSED/TRKSCYL                   */
         L     R3,TTRKUSED         TTRKSUSED
         M     R2,=F'1'
         D     R2,TRKSCYL          TTRKSUSED/TRKSCYL
         CVD   R3,DW
         UNPK  DPSPCUS,DW+5(3)     Unpack and
         OI    DPSPCUS+4,X'F0'     ... force and F zone
         B     SSPCXT
SSPC#TRK EQU   *
         MVC   DSSPC,=C'TRACK   '  TRACK Allocation...                  LB1010c
*     * /* Calculate SYSPRIMARY=TRKSTOTA                        */
         L     R5,TRKSTOTA         ... and convert
         CVD   R5,DW               ... to decimal
         UNPK  DPSPCAL,DW+5(3)     Unpack and
         OI    DPSPCAL+4,X'F0'     ... force and F zone
*     * /* Calculate SYSUSED=TTRKUSED                           */
         L     R5,TTRKUSED         ... and convert
         CVD   R5,DW               ... to decimal
         UNPK  DPSPCUS,DW+5(3)     Unpack and
         OI    DPSPCUS+4,X'F0'     ... force and F zone
         B     SSPCXT
*
         EJECT
SSPC#ABS EQU   *
         MVC   DSSPC,=C'ABSOLUTE'  ABSOLUTE Allocation...               LB1010c
*     * /* Calculate SYSPRIMARY=TRKSTOTA                        */
         L     R5,TRKSTOTA         ... and convert
         CVD   R5,DW               ... to decimal
         UNPK  DPSPCAL,DW+5(3)     Unpack and
         OI    DPSPCAL+4,X'F0'     ... force and F zone
*     * /* Calculate SYSUSED= not applicable                    */
         B     SSPCXT
SSPC#BLK EQU   *
         MVC   DSSPC,=C'BLOCK   '  BLOCK Allocation...                  LB1010c
*     * /* Calculate SYSPRIMARY=TRKSTOTA*BLKSPTRK               */
         L     R3,TRKSTOTA         TRKSTOTA
         M     R2,BLKSPTRK         BLKSPTRK
         CVD   R3,DW               ... to decimal
         UNPK  DPSPCAL,DW+5(3)     Unpack and
         OI    DPSPCAL+4,X'F0'     ... force and F zone
*     * /* Calculate SYSUSED=TTRKUSED*BLKSPTRK                  */
         L     R3,TTRKUSED         TRKSUSED
         M     R2,BLKSPTRK         BLKSPTRK
         CVD   R3,DW               ... to decimal
         UNPK  DPSPCUS,DW+5(3)     Unpack and
         OI    DPSPCUS+4,X'F0'     ... force and F zone
         B     SSPCXT
SSPCXT   EQU   *
         MVC   DALLOC,DPSPCAL      Allocation in space units            LB1010d
*
         EJECT
*     * /********************************************************/
*     * /* Secondary Allocation Quantity                        */
*     * /* #Data source: 27 SYSSECONDS                          */
*     * /* - R3 Working Register                                */
*     * /********************************************************/
SSPCNM   EQU   *
         XC    FW,FW               Clear contents
         MVC   FW+1(3),DS1SCALO+1  Get Secondary Allocation quantity
         L     R3,FW                ... and convert
         CVD   R3,DW                ... to decimal
         UNPK  DSSPCNM,DW+4(4)     Unpack and
         OI    DSSPCNM+6,X'F0'      ... force and F zone
SSPCNMXT EQU    *
*     *
*     * /********************************************************/
*     * /* Password Protection                                  */
*     * /* #Data source: 14 SYSPASSWORD                         */
*     * /********************************************************/
PASSWD   EQU   *
**       DISPLAY  DATA=(DS1DSIND+49,B,1)
         TM    DS1DSIND,X'14'      Write Password required?
         BO    WRPW                YES.
         TM    DS1DSIND,X'10'      Read-Write PW required?
         BO    RWPW                YES.
PWDNONE  EQU   *
         MVC   DPASSWD,=C'NONE '   No password needed
         B     PASSWDXT
WRPW     EQU   *
         MVC   DPASSWD,=C'WRITE'   Password needed for READ
         B     PASSWDXT
RWPW     EQU   *
         MVC   DPASSWD,=C'R_W  '   Password needed for READ/WRITE
         B     PASSWDXT
PASSWDXT EQU   *
*     *
*     * /********************************************************/
*     * /* RACF Secured                                         */
*     * /* #Data source: 15 SYSRACF                             */
*     * /********************************************************/
RACF     EQU   *
         TM    DS1DSIND,X'40'      RACF defined?
         BO    RACFRQD             YES.
RACFNONE EQU   *
         MVC   DRACF,=C'NONE'      No RACF
         B     RACFXT
RACFRQD  EQU   *
         MVC   DRACF,=C'RACF'      We have RACF...
         B     RACFXT
RACFXT   EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* File Create Date  X'yy0jjj'                          */
*     * /* #Data source: 05 SYSCREATE                           */
*     * /* #Data source: 31 SYSJCREATE                          */
*     * /* #Data source: 37 SYSCCREATE                          */
*     * /* - R6, R7 Working Register                            */
*     * /********************************************************/
CREDT    EQU   *
         MVC   DCREDT,ZEROCL11     Default date CCYY/JJJ                LB1001b
         MVC   DCREDT+4(1),DATESEP Date Separator value                 LB1001a
         MVC   DCREDJ,ZEROCL11     Default date YYJJJ                   LB1001b
         MVC   DC#MDCY,ZEROCL11    Default date MM/DD/CCYY              LB1001b
         BAL   R7,DFMT             Format Date CCYY MM DD               LB1001b
         MVC   DCREDC,DATEWA                                            LB1001b
         CLC   DS1CREDT,=X'000000' Zero date?
         BE    CREDTXT             YES, done.
         LA    R6,DS1CREDT         Create Date
         BAL   R7,DATECONV         Convert date to CCYY/JJJJ
         MVC   DCREDT(4),DC#CCYY+1 CCYY  -\      JulianCCYY
         MVC   DCREDT+5(3),DC#JJJ  JJJ     +---- CCYY/JJJ
         MVC   DCREDJ(2),DC#CCYY+3 YY    -\      MVS38J
         MVC   DCREDJ+2(3),DC#JJJ  JJJ     +---- YYJJJ
         BAL   R7,DFMT             Format Date CCYY MM DD               LB1001b
         MVC   DCREDC,DATEWA                                            LB1001b
CREDTXT  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* File Expire Date  X'yy0jjj'                          */
*     * /* #Data source: 06 SYSEXDATE                           */
*     * /* #Data source: 32 SYSJEXDATE                          */
*     * /* #Data source: 38 SYSCEXDATE                          */
*     * /* - R6, R7 Working Register                            */
*     * /********************************************************/
EXPDT    EQU   *
         MVC   DEXPDT,ZEROCL11     Default date CCYY/JJJ                LB1001b
         MVC   DEXPDT+4(1),DATESEP Date Separator value                 LB1001a
         MVC   DEXPDJ,ZEROCL11     Default date YYJJJ                   LB1001b
         MVC   DC#MDCY,ZEROCL11    Default date MM/DD/CCYY              LB1001b
         BAL   R7,DFMT             Format Date CCYY MM DD               LB1001b
         MVC   DEXPDC,DATEWA                                            LB1001b
         CLC   DS1EXPDT,=X'000000' Zero date?
         BE    EXPDTXT             YES, done.
         LA    R6,DS1EXPDT         Expire Date
         BAL   R7,DATECONV         Convert date to CCYY/JJJJ
         MVC   DEXPDT(4),DC#CCYY+1 CCYY  -\      JulianCCYY
         MVC   DEXPDT+5(3),DC#JJJ  JJJ     +---- CCYY/JJJ
         MVC   DEXPDJ(2),DC#CCYY+3 YY    -\      MVS38J
         MVC   DEXPDJ+2(3),DC#JJJ  JJJ     +---- YYJJJ
         BAL   R7,DFMT             Format Date CCYY MM DD               LB1001b
         MVC   DEXPDC,DATEWA                                            LB1001b
EXPDTXT  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* File Reference Date  X'yy0jjj'                       */
*     * /* #Data source: 07 SYSREFDATE                          */
*     * /* #Data source: 33 SYSJREFDATE                         */
*     * /* #Data source: 39 SYSCREFDATE                         */
*     * /* - R6, R7 Working Register                            */
*     * /********************************************************/
REFDT    EQU   *
         MVC   DREFDT,ZEROCL11     Default date CCYY/JJJ                LB1001b
         MVC   DREFDT+4(1),DATESEP Date Separator value                 LB1001a
         MVC   DREFDJ,ZEROCL11     Default date YYJJJ                   LB1001b
         MVC   DC#MDCY,ZEROCL11    Default date MM/DD/CCYY              LB1001b
         BAL   R7,DFMT             Format Date CCYY MM DD               LB1001b
         MVC   DREFDC,DATEWA                                            LB1001b
         CLC   DS1REFD,=X'000000'  Zero date?
         BE    REFDTXT             YES, done.
         LA    R6,DS1REFD          Referenced Date
         BAL   R7,DATECONV         Convert date to CCYY/JJJJ
         MVC   DREFDT(4),DC#CCYY+1 CCYY  -\      JulianCCYY
         MVC   DREFDT+5(3),DC#JJJ  JJJ     +---- CCYY/JJJ
         MVC   DREFDJ(2),DC#CCYY+3 YY    -\      MVS38J
         MVC   DREFDJ+2(3),DC#JJJ  JJJ     +---- YYJJJ
         BAL   R7,DFMT             Format Date CCYY MM DD               LB1001b
         MVC   DREFDC,DATEWA                                            LB1001b
REFDTXT  EQU   *
*
         EJECT                                                          LB1040a
*     * /********************************************************/      LB1040a
*     * /* File Backup Indication                               */      LB1040a
*     * /* #Data source: 47 SYSUPDATED                          */      LB1040a
*     * /********************************************************/      LB1040a
BUIND    EQU   *                                                        LB1040a
         MVI   DUPDTD,C'Y'         Assume, mark as Y                    LB1040a
         TM    DS1DSIND,DS1IND02   Is Dataset altered?                  LB1040a
         BO    BUINDXT             Yes, done.....                       LB1040a
BUINDN   EQU   *                                                        LB1040a
         MVI   DUPDTD,C'N'         No, mark as N                        LB1040a
BUINDXT  EQU   *                                                        LB1040a
*                                                                       LB1040a
         EJECT
*     * /********************************************************/
*     * /* Device Type                                          */
*     * /* For catalogued Datasets, use catalog unit type       */
*     * /*   to determine device type                           */
*     * /* For Uncatalogued Datasets, match VOLSER in UCB list  */
*     * /*   to obtain unit type to determine device type       */
*     * /* #Data source: 22 SYSUNIT                             */
*     * /* - R4, R2, R3, R5, R6 Working Registers               */
*     * /********************************************************/
DEVTP    EQU   *
         CLI   DDSCAT,C'Y'         Dataset catalogued?
         BE    GETDEVTY            YES, branch to GETDEVTY
*                                  NO, find unit type from VOLSER
*                                      using UCB
         L     R4,CVTPTR           Address of CVT
         USING CVT,R4
         L     R2,CVTILK2          Address of UCB Lookup table
         USING UCB,R3
UCBSCAN  EQU   *
         SR    R3,R3               Clear R3
         LH    R3,0(R2)            Point to UCB
         LA    R2,2(R2)            Bump to next UCB entry
         LTR   R3,R3               NULL?
         BZ    UCBSCAN             YES, get another
         CL    R3,=X'0000FFFF'     End of UCB Table?
         BE    UCBNOFND            YES, report UCB error RSN=14         LB1040e
         TM    UCBJBNR,UCBVRDEV    VIO Device?
         BO    UCBSCAN             NO, re-scan
         TM    UCBSTAT,UCBONLI     Device ONLINE?
         BNO   UCBSCAN             NO, re-scan
         CLC   DVOLSER,UCBVOLI     Is this the VOLUME?
         BNE   UCBSCAN             NO, re-scan
*
         EJECT
*     * /********************************************************/
*     * /* Find device type (e.g. 3350) given unit type         */
*     * /* using below table (DVCLST)                           */
*     * /* -JMSG'DeviceType xxx not defined in table'           */
*     * /* - R4, R2, R3, R5, R6 Working Registers               */
*     * /********************************************************/
GETDEVTY EQU   *                   YES, determine type!
         LA    R5,DVCLST$E         Load # of DVCLST entries
         LA    R6,DVCLST$          Address of DVCLST entries
LOOK4DT  EQU    *
         CLI   DDSCAT,C'Y'         Dataset catalogued?
         BE    LOOK4DT2            YES, check catlg device type
LOOK4DT1 EQU   *
**       CLC   19(1,R3),0(R6)      NO. UCB device type in table?
         CLC   UCBUNTYP,0(R6)      NO. UCB device type in table?
         BE    DEVFND              YES, found device in table
         B     LOOK4DT3            NO, check next one...
LOOK4DT2 EQU   *
         CLC   LOCAUNTY,0(R6)      Catlg device type in table?
         BE    DEVFND              YES, found device in table
LOOK4DT3 EQU   *
         LA    R6,DVCLST$L(R6)     NO, point to next device entry
         BCT   R5,LOOK4DT          Look at next entry
NODEVFND EQU   *
*                                  Not defined, Error.
         ST    R15,SAVER15                                              LB2000d
         MVC   RCEXIT,=F'0025'     DeviceType not defined               LB2000d
         BAL   R7,MSGPRCR2                                              LB2000d
         L     R15,SAVER15                                              LB2000d
         MVC   DJMSG(10),=C'DeviceType'
         XC    FW,FW
         CLI   DDSCAT,C'Y'         Dataset catalogued?                  LB1040e
         BE    MVCDT2              YES, check catlg device type         LB1040e
         MVC   FW+3(1),UCBUNTYP                                         LB1040e
         B     MVCDTX                                                   LB1040e
MVCDT2   EQU   *                                                        LB1040e
         MVC   FW+3(1),LOCAUNTY                                         LB1040e
MVCDTX   EQU   *                                                        LB1040e
         L     R15,FW
         CVD   R15,DW              Convert to decmial
         UNPK  DJMSG+11(3),DW+6(2) Unpack and
         OI    DJMSG+13,X'F0'       ... force and F zone
         B     DEVNOFND                                                 LB1040e
DEVFND   EQU   *
         MVC   DUNIT,1(R6)         MOVE DEVTTYPE
         TM    DS1DSORG+1,X'08'    VSAM?                                LB1040d
         BO    VSMNOSUP            Yes, error...                        LB1040d
         B     DEVTPXT
VSMNOSUP EQU   *                                                        LB1040d
         MVC   DRSNCODE,=C'12'     VSAM datasets not supported          LB1040d
         MVC   RCEXIT,=F'04'                                            LB1040d
         MVC   RC,=C'04'                                                LB1040d
         B     ENDME               Done, exit.                          LB1040d
UCBNOFND EQU   *                                                        LB1040e
         MVC   DRSNCODE,=C'14'     Device not found in UCB table        LB1040e
         MVC   RCEXIT,=F'04'                                            LB1040e
         MVC   RC,=C'04'                                                LB1040e
         B     DEVTPXT                                                  LB1040e
DEVNOFND EQU   *                                                        LB1040e
         MVC   DRSNCODE,=C'07'     Device not found in DEVLST int tbl   LB1040e
         MVC   RCEXIT,=F'04'                                            LB1040e
         MVC   RC,=C'04'                                                LB1040e
         B     DEVTPXT                                                  LB1040e
*
         EJECT
*              8 bytes   /  DVCLST Table---------------------------
*              per entry+   01 bytes, device type code
*                        \  07 bytes, Device Type
*
DVCLST$  DC    X'00',CL7'??***??'
DVCLST$L EQU   *-DVCLST$           Table Entry length
         DC    X'01',CL7'2311   '
         DC    X'02',CL7'2301   '
         DC    X'03',CL7'2303   '
         DC    X'04',CL7'9345   '
         DC    X'05',CL7'2321   '
         DC    X'06',CL7'2305-1 '
         DC    X'07',CL7'2305-2 '
         DC    X'08',CL7'2314   '
         DC    X'09',CL7'3330   '
         DC    X'0A',CL7'3340   '
         DC    X'0B',CL7'3350   '
         DC    X'0C',CL7'3375   '
         DC    X'0D',CL7'3330-11'
         DC    X'0E',CL7'3380   '
         DC    X'0F',CL7'3390   '
DVCLST$E EQU   (*-DVCLST$)/DVCLST$L  Table Entries
*
DEVTPXT  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* PO Directory Information                             */
*     * /* Allocate PDS                                         */
*     * /* -JMSG'RC=xxx Allocate PDS error'                     */
*     * /* - R7, R2, R1 Working Register                        */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
PODDIR   EQU   *
PODALLOC EQU   *
         CLI   PDIR,C'Y'           PDS DIR Info requested?
         BNE   PODDIRXT            NO, bypass PDS Info
         CLC   DDSORG(2),=C'PO'    YES. Is file a PDS?
         BNE   PODDIRNN            NO, bypass PDS Info w/ MSG
                                                                        LB1010h
         CLI   PDL,C'N'            Create Directory List?               LB1010h
         BE    PODALCGO            No, continue to DIR stats            LB1010h
*                                  Yes, add ISPF stats                  LB1010h
         MVC   TBN+6(1),PDL        Table Name Number (0 or 1)           LB1010h
*     * /********************************************************/      LB1010h
*     * /* ISPLINK Entry Point                                  */      LB1010h
*     * /********************************************************/      LB1010h
         XC    ISPLINK,ISPLINK     Clear ISPLINK Address                LB1010h
         LOAD  EP=ISPLINK,ERRET=NOISPLNK                                LB1010h
         ST    R0,ISPLINK          Save ISPLINK Entry Point             LB1010h
         MVC   PISPEP,ISPLINK      ISPLINK Entry Point                  LB2000e
         B     OKISPLNK            Continue...                          LB1010h
NOISPLNK EQU   *                                                        LB1010h
         MVI   PDL,C'N'            Force Dir List OFF!                  LB1010h
         MVC   DJMSG+7(18),=C'NO ISPLINK SUPPORT'                       LB1010h
         B     PODALCGO            No, continue to DIR stats            LB1010h
OKISPLNK EQU   *                                                        LB1010h
         BAL   R7,VARDEFS          Define ISPF Functional variables     LB1010h
         BAL   R7,ERASETB          Erase existing ISPF table            LB1010h
         BAL   R7,CREATETB         Create ISPF table                    LB1010h
PODALCGO EQU  *                                                         LB1010h
         BAL   R7,PDSALC           YES, allocate dataset
         LTR   R15,R15             Successful?
         BZ    PODINFO             YES, get DIR information
*                                  NO, ERROR, cannot alloc PDS
         ST    R15,SAVER15                                              LB2000d
         MVC   RCEXIT,=F'0017'     File Allocation error                LB2000d
         BAL   R7,MSGPRCR2                                              LB2000d
         L     R15,SAVER15                                              LB2000d
         BAL   R7,R15DJMSG         R15 rc on DJMSG                      LB2000f
*        CVD   R15,DW              Convert to decimal
*        UNPK  DJMSG+3(3),DW+6(2)  Unpack and
*        OI    DJMSG+5,X'F0'       ... force and F zone
         MVC   DRSNCODE,=C'02'     Dynam Alloc error
         MVC   RCEXIT,=F'04'
         MVC   RC,=C'04'
         B     ENDME               Done, exit.
*
         EJECT
*     *  PDS DIR BLOCK STRUCTURE
*     *  +-----+ +---+ +---------------------------------------+
*     *  |Count| |Key| |Data ..................................|
*     *  | ( ) | |(8)| |(256)                                  |
*     *  +-----+ +---+ +---------------------------------------+
*     *
*     *  The KEY is an external key to the data block.  It contains
*     *  the last member name in the block for search efficiency.
*     *
*     *  +-----------------------------------------------------+
*     *  |Data(256) ...........................................|
*     *  +-----------------------------------------------------+
*     *  |DIR Bytes|Member |Member |Member |Member |Unused     |
*     *  |Used (2) |Ent #1 |Ent #2 |Ent #3 |Ent #n |      .....|
*     *  +-----------------------------------------------------+
*     *
*     *  +-----------------------------------------------------+
*     *  |Member Entry ........................................|
*     *  +-----------------------------------------------------+
*     *  |Member   |TTR    |C      |Optional User Data         |
*     *  |Name (8) | (3)   | (1)   | (2-62)                    |
*     *  +-----------------------------------------------------+
*     *
*     *  PDS dir block is 256 bytes in length.  First 2 bytes contains
*     *  a count field representing the number of bytes used INCLUDING
*     *  count field (2 bytes).  The remaining 254 bytes contain
*     *  member entries.
*     *
*     *  Last member entry contains binary ones.  All the directory
*     *  blocks that follow are unused.  This is key when counting
*     *  used and unused blocks.
*     *
*     *  C in Member Entry:
*     *  Bit 0-0  0=Primary Name   1=Alias Name
*     *  Bit 1-2  Number of TTRN fields in the user data
*     *  Bit 3-7  Length of user data in HALFWORDS
*     *
*
         EJECT
*     * /********************************************************/
*     * /* PO Directory Information                             */
*     * /* Get DIR block and member count information           */
*     * /* -JMSG'RC=xxx Open PDS error'                         */
*     * /* - R7, R0, R1, R2 ,R3 Working Register                */      LB1010h
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
PODINFO  EQU   *                   Initialize FW counters
         XC    PODBLKS,PODBLKS     Zero PDS DIR Blocks Count
         XC    PODBLKSU,PODBLKSU   Zero PDS Used DIR Blocks Count
         XC    POMEMS,POMEMS       Zero PDS Member Count
         XC    POALIS,POALIS       Zero PDS Alias  Count
*
*                                  Init DIRDCBW from DIRDCB
         LA    R2,DIRDCBW          Addr of myDIRDCB
         MVC   0(DIRDCBL,R2),DIRDCB  Init myDIRDCB with DIRDCB
         MVC   DDNAM(8,R2),DDNAME  Apply DDN from DAIR
         MVI   PARMVL1,X'80'       Mark as last parm FW (VL=1)
         PRINT GEN
PODOPEN  EQU   *
         OPEN  ((R2),INPUT),MF=(E,PARMVL1) Open DIRDCBW execute form
         LTR   R15,R15             Successful?
         BZ    PODCONT0            YES, continue...
*                                  NO, ERROR, cannot open  PDS
         ST    R15,SAVER15                                              LB2000d
         MVC   RCEXIT,=F'0030'     Open PDS error                       LB2000d
         BAL   R7,MSGPRCR2                                              LB2000d
         L     R15,SAVER15                                              LB2000d
         BAL   R7,R15DJMSG         R15 rc on DJMSG                      LB2000f
*        CVD   R15,DW              Convert to decimal
*        UNPK  DJMSG+3(3),DW+6(2)  Unpack and
*        OI    DJMSG+5,X'F0'       ... force and F zone
         MVC   DRSNCODE,=C'13'     Cannot open file error
         MVC   RCEXIT,=F'04'
         MVC   RC,=C'04'
         B     ENDME               Done, exit.
*
PODCONT0 EQU   *
*                                  Init DIRDDECBW from DIRDECB
         MVC   DIRDECBW(DIRDECBL),DIRDECB
         MVI   DIRSW,0             Set to Initial value
*              DIRSW=0 Initial, process used blocks
*              DIRSW=1 Used Blocks elapsed, process unused blocks
*              DIRSW=2 IO Error
PODREAD  EQU   *                   Read DIR Block
         READ  DIRDECBW,SF,(R2),BLOCK,256,MF=E    READ Execute form
         CHECK DIRDECBW
         TM    DIRSW,2             I/O Error?
         BO    PODERR              YES, report and exit
*
PODBLKSC EQU   *                   Count DIR Blocks
         L     R1,PODBLKS          R1=PODBLKS
         LA    R1,1(R1)            Add 1 to PODBLKS
         ST    R1,PODBLKS          Save PODBLKS count
         TM    DIRSW,1             Past USED blocks?
         BO    PODREAD             YES, read another block
         LA    R1,BLOCK            NO, R1=Addr of BLOCK
         LH    R0,0(,R1)           R0=Bytes in use
         AR    R0,R1               R0=Addr past last entry
         LA    R1,2(,R1)           R1=Addr of first member in block
*
         EJECT
PODENTRY EQU   *                   USED and UNUSED DIR BLK Test
         CLC   0(8,R1),=8X'FF'     Last member in DIR BLK?
         BE    PODUSED             YES, mark End of USED BLKS and read
*                                  NO, test for ALIAS or MEMBER entry
         TM    11(R1),X'80'        ALIAS?
         BO    PODALIAS            YES, branch to PODALIAS
*                                  NO, count MEMBER type
PODMEMBR EQU   *                   Member Count
         L     R3,POMEMS           R3=POMEMS
         LA    R3,1(R3)            Add 1 to POMEMS
         ST    R3,POMEMS           Save member count
         B     PODNEXT             Branch to next DIR entry
*
PODALIAS EQU   *                   Alias Member Count
         L     R3,POALIS           R3=POALIS
         LA    R3,1(R3)            Add 1 to POALIS
         ST    R3,POALIS           Save POALIS count
*
PODNEXT  EQU   *                   Get next DIR ENTRY
         SLR   R3,R3               Clear R3
         IC    R3,11(,R1)          Load C (Control Info)
         N     R3,=F'31'           All BITS OFF except bits 3-7
         SLL   R3,1                Multiply by 2; HALFWORDS to bytes
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /*------------------------------------------------------*/      LB1010h
*     * /* Create ISPF table with member list including ISPF    */      LB1010h
*     * /* statistics, if available.                            */      LB1010h
*     * /*------------------------------------------------------*/      LB1010h
         CLI   PDL,C'N'            Create Directory List?               LB1010h
         BE    PODCONT5            No, continue to DIR stats            LB1010h
*                                                                       LB1010h
         LR    R4,R1               Address Dir Entry and User Area      LB1010h
         USING DIRENTUA,R4         Tell Assembler                       LB1010h
*                                                                       LB1010h
         MVI   IMEMBR,C' '         Blank out ISPF Table fields          LB1010h
         MVC   IMEMBR+1(L'ISTATSL-1),IMEMBR                             LB1010h
*                                                                       LB1010h
         MVC   IMEMBR,UAMMEM       ** Member Name                       LB1010h
*                                                                       LB1010h
         LA    R15,ITTRP           ** Member TTR  CHARACTER Field       LB1010h
         LA    R5,L'UAMTTR         Length of BINARY field               LB1010h
         LA    R7,UAMTTR           Addr   of BINARY field               LB1010h
CH2C     EQU   *                   Convert Hex byte to 2 characters     LB1010h
         IC    R14,0(R7)           Process LEFT  4 bits                 LB1010h
         N     R14,=F'15'                                               LB1010h
         IC    R14,HEXTAB(R14)     Translate based on offset            LB1010h
         STC   R14,1(R15)          Store character                      LB1010h
*                                                                       LB1010h
         IC    R14,0(R7)           Process RIGHT 4 bits                 LB1010h
         SRL   R14,4               Shift left                           LB1010h
         IC    R14,HEXTAB(R14)     Translate based on offset            LB1010h
         STC   R14,0(R15)          Store character                      LB1010h
         LA    R7,1(R7)            Bump to next binary byte             LB1010h
         LA    R15,2(R15)          Bump to next character pair          LB1010h
         BCT   R5,CH2C                                                  LB1010h
*                                                                       LB1010h
         C     R3,=F'30'           ISPF stats present?                  LB1010h
         BE    ISPFSTS             Yes, convert values and save row     LB1010h
         B     ADDTBZZ             No, default values and save row      LB1010h
*                                                                       LB1010h
         EJECT                                                          LB1010h
ISPFSTS  EQU   *                                                        LB1010h
         XR    R5,R5               Clear R5                             LB1010h
         IC    R5,UAMVLVL          ** Version Level                     LB1010h
         CVD   R5,DW                                                    LB1010h
         UNPK  IVLVL,DW+6(2)       Unpack and                           LB1010h
         OI    IVLVL+2,X'F0'         ... force and F zone               LB1010h
*                                                                       LB1010h
         XR    R5,R5               Clear R5                             LB1010h
         IC    R5,UAMMLVL          ** Modification Level                LB1010h
         CVD   R5,DW                                                    LB1010h
         UNPK  IMLVL,DW+6(2)       Unpack and                           LB1010h
         OI    IMLVL+2,X'F0'         ... force and F zone               LB1010h
*                                                                       LB1010h
         MVC   IVVMM(2),IVLVL+1    Concatenate                          LB1010h
         MVI   IVVMM+2,C'.'        ..version and modification           LB1010h
         MVC   IVVMM+3(2),IMLVL+1  ...as VV.MM                          LB1010h
*                                                                       LB1010h
*                                  ** Member Create Date                LB1010h
         XC    DW,DW               Clear DW                             LB1010h
         MVC   DW+7(1),UAMCREDT+1  Move YY portion (decimal)            LB1010h
         L     R6,DW+4             Load R6 with YY portion              LB1010h
         SLL   R6,4                Shift left 4 bits to insert F zone   LB1010h
         ST    R6,DW+4             Store                                LB1010h
         OI    DW+7,X'0F'          Insert F zone                        LB1010h
         CVB   R6,DW               Convert decimal to binary            LB1010h
         STC   R6,DC#YYJJJ         Store right-most character (1-byte)  LB1010h
*                                                                       LB1010h
         XC    DW,DW               Clear DW                             LB1010h
         MVC   DW+6(2),UAMCREDT+2  Move 0JJJ portion (packed)           LB1010h
         CVB   R6,DW               Convert packed to binary             LB1010h
         STH   R6,DC#HJJJ          Store binary result                  LB1010h
         MVC   DC#YYJJJ+1(2),DC#HJJJ                                    LB1010h
         LA    R6,DC#YYJJJ                                              LB1010h
         ST    R3,SAVER3           Save R3, used by DATECONV            LB1010h
         BAL   R7,DATECONV                                              LB1010h
         BAL   R7,DFMT                                                  LB1010h
         MVC   ICDTE,DATEWA+2                                           LB1010h
         L     R3,SAVER3           Restore R3                           LB1010h
*                                                                       LB1010h
*                                  ** Member Change Date                LB1010h
         XC    DW,DW               Clear DW                             LB1010h
         MVC   DW+7(1),UAMCHGDT+1  Move YY portion (decimal)            LB1010h
         L     R6,DW+4             Load R6 with YY portion              LB1010h
         SLL   R6,4                Shift left 4 bits to insert F zone   LB1010h
         ST    R6,DW+4             Store                                LB1010h
         OI    DW+7,X'0F'          Insert F zone                        LB1010h
         CVB   R6,DW               Convert decimal to binary            LB1010h
         STC   R6,DC#YYJJJ         Store right-most character (1-byte)  LB1010h
*                                                                       LB1010h
         XC    DW,DW               Clear DW                             LB1010h
         MVC   DW+6(2),UAMCHGDT+2  Move 0JJJ portion (packed)           LB1010h
         CVB   R6,DW               Convert packed to binary             LB1010h
         STH   R6,DC#HJJJ          Store binary result                  LB1010h
         MVC   DC#YYJJJ+1(2),DC#HJJJ                                    LB1010h
         LA    R6,DC#YYJJJ                                              LB1010h
         ST    R3,SAVER3           Save R3, used by DATECONV            LB1010h
         BAL   R7,DATECONV                                              LB1010h
         BAL   R7,DFMT                                                  LB1010h
         MVC   IMDTE,DATEWA+2                                           LB1010h
         L     R3,SAVER3           Restore R3                           LB1010h
*                                                                       LB1010h
         MVO   DW(3),UAMCHGTM      ** Modification Time HHMM            LB1010h
         OI    DW+2,X'0F'          Set F zone                           LB1010h
         UNPK  ICHGTM(5),DW(3)     Unpack                               LB1010h
         MVC   IMTIM+0(2),ICHGTM+1    data                              LB1010h
         MVI   IMTIM+2,C':'            as                               LB1010h
         MVC   IMTIM+3(2),ICHGTM+3      HH:MM                           LB1010h
         MVI   IMTIM+5,C':'                                             LB1010h
         MVO   DW(2),UAMCHGSS      ** Modification Time SS              LB1010h
         OI    DW+1,X'0F'          Set F zone                           LB1010h
         UNPK  ICHGTM(3),DW(2)     Unpack                               LB1010h
         MVC   IMTIM+6(2),ICHGTM+1    to HH:MM:SS                       LB1010h
*                                                                       LB1010h
         XR    R5,R5               Clear R5                             LB1010h
         LH    R5,UAMCURR          ** Current Record Count NNNNN        LB1010h
         CVD   R5,DW                                                    LB1010h
         UNPK  IRCUR,DW+5(3)       Unpack and                           LB1010h
         OI    IRCUR+4,X'F0'         ... force and F zone               LB1010h
*                                                                       LB1010h
         XR    R5,R5               Clear R5                             LB1010h
         LH    R5,UAMINIT          ** Initial Record Count NNNNN        LB1010h
         CVD   R5,DW                                                    LB1010h
         UNPK  IRINT,DW+5(3)       Unpack and                           LB1010h
         OI    IRINT+4,X'F0'         ... force and F zone               LB1010h
*                                                                       LB1010h
         XR    R5,R5               Clear R5                             LB1010h
         LH    R5,UAMMOD           ** Records Modified Count NNNNN      LB1010h
         CVD   R5,DW                                                    LB1010h
         UNPK  IRMOD,DW+5(3)       Unpack and                           LB1010h
         OI    IRMOD+4,X'F0'         ... force and F zone               LB1010h
*                                                                       LB1010h
         MVC   IUSRID,UAMUID       ** User ID                           LB1010h
*                                                                       LB1010h
ADDTBZZ  EQU   *                                                        LB1010h
         BAL   R7,ADDTB            Add ISPF Table row                   LB1010h
*                                                                       LB1010h
         LR    R1,R4               Restore R1                           LB1010h
*                                                                       LB1010h
         EJECT                                                          LB1010h
PODCONT5 EQU   *                                                        LB1010h
         LA    R1,12(R3,R1)        Point past user data...
         CR    R1,R0               End of DIR block?
         BL    PODENTRY            NO, process next entry
         B     PODREAD             YES, read next block
*
PODUSED  OI    DIRSW,1             Passed Used blocks...
         MVC   PODBLKSU,PODBLKS    Store Used Blocks count
         B     PODREAD             Read next block
*
         EJECT
PODSYN   EQU   *                   IO ERROR from reading PDS DIR
         SYNADAF ACSMETH=BSAM      Obtain msg/err code for I/O error
         MVC   DJMSG(78),44(R1)            Move to &SYSLISTDSJMSG
         OI    DIRSW,2             I/O error setting
         SYNADRLS                  Release msg buff/saveareas -SYNADAF
         B     PODCNTS             Go Unpack DIR INFO and FREE PDS

PODERR   EQU   *                   SYNADAF Error
*                                  Message already in DJMSG
         B     PODCNTS             Go Unpack DIR INFO and FREE PDS

PODEOF   EQU   *                   End of File Detected (DCB)
         CLOSE ((R2)),MF=(E,PARMVL1)  Close PDS execute form
         LTR   R15,R15             Successful?
         BZ    PODCONT9            YES, continue...
*                                  NO, ERROR, cannot close PDS
         ST    R15,SAVER15                                              LB2000d
         MVC   RCEXIT,=F'0031'     IO Error                             LB2000d
         BAL   R7,MSGPRCR2                                              LB2000d
         L     R15,SAVER15                                              LB2000d
         BAL   R7,R15DJMSG         R15 rc on DJMSG                      LB2000f
*        CVD   R15,DW              Convert to decimal
*        UNPK  DJMSG+3(3),DW+6(2)  Unpack and
*        OI    DJMSG+5,X'F0'       ... force and F zone
         MVC   DRSNCODE,=C'13'     Cannot close file error
         MVC   RCEXIT,=F'04'
         MVC   RC,=C'04'
         B     ENDME               Done, exit.
         PRINT NOGEN
*
PODCONT9 EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* PO Directory Information                             */
*     * /* Save block and member information                    */
*     * /* - R3, R7 Working Register                            */
*     * /********************************************************/
PODCNTS  EQU   *
*     * /* #Data source: 16 SYSADIRBLK                          */
         L     R3,PODBLKS          DIR Blocks
         CVD   R3,DW               convert to decimal
         UNPK  DDBLKS,DW+5(3)      Unpack and
         OI    DDBLKS+4,X'F0'       ... force F zone
*
*     * /* #Data source: 17 SYSUDIRBLK                          */
         L     R3,PODBLKSU         DIR Blocks Used
         CVD   R3,DW               convert to decimal
         UNPK  DDIRBKSU,DW+5(3)    Unpack and
         OI    DDIRBKSU+4,X'F0'     ... force F zone
*
*     * /* #Data source: 18 SYSNUDIRBLK                         */
         L     R3,PODBLKS          DIR Blocks Unused (calculate)
         S     R3,PODBLKSU
         CVD   R3,DW               convert to decimal
         UNPK  DDIRBKSN,DW+5(3)    Unpack and
         OI    DDIRBKSN+4,X'F0'     ... force F zone
*
*     * /* #Data source: 19 SYSMEMBERS                          */
         L     R3,POMEMS           PDS POMEMS
         A     R3,POALIS            plus PDS Aliases
         CVD   R3,DW               convert to decimal
         UNPK  DMEMBERS,DW+5(3)    Unpack and
         OI    DMEMBERS+4,X'F0'     ... force F zone
*
*     * /* #Data source: 20 SYSMEMBERSALIAS                     */
         L     R3,POALIS           PDS Aliases
         CVD   R3,DW               convert to decimal
         UNPK  DALIASES,DW+5(3)    Unpack and
         OI    DALIASES+4,X'F0'     ... force F zone
*
PODX     EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* PDS Directory Information                            */
*     * /* Free PDS                                             */
*     * /* -JMSG'RC=xxx Free PDS error'                         */
*     * /* - R7 Working Register                                */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
PDSUNALC EQU   *
         CLI   PDL,C'N'            Create Directory List?               LB1010h
         BE    PODFREE             No, continue to free PDS             LB1010h
*                                  Yes...ISPF clean up                  LB1010h
         BAL   R7,SAVETB           Save ISPF table                      LB1010h
         BAL   R7,CLOSETB          Close ISPF table                     LB1010h
         BAL   R7,VARDELS          Delete ISPF functional variables     LB1010h
         B     PODFREE             Branch to free PDS                   LB1010h
*                                                                       LB1010h
ISPFERR  EQU   *                                                        LB1010h
         BAL   R7,R15DJMSG         R15 rc on DJMSG                      LB2000f
*        CVD   R15,DW              Convert to decimal                   LB1010h
*        UNPK  DJMSG+3(3),DW+6(2)  Unpack and                           LB1010h
*        OI    DJMSG+5,X'F0'       ... force and F zone                 LB1010h
         MVC   DRSNCODE,=C'17'     System User Abend error              LB1010h
         MVC   RCEXIT,=F'16'                                            LB1010h
         MVC   RC,=C'16'                                                LB1010h
         CLI   ALLOCPO,C'Y'        PDS Allocated Flag init to YES       LB1010h
         BE    PODFREE             Yes, free PDS and exit               LB1010h
         B     ENDME               No, exit.                            LB1010h
*                                                                       LB1010h
PODFREE  EQU   *                                                        LB1010h
*                                                                       LB1010h
         BAL   R7,PDSFRE           FREE dataset
         LTR   R15,R15             Successful?
         BZ    PODDIRXT            YES, Done.
*                                  NO, cannot FREE PDS
         ST    R15,SAVER15                                              LB2000d
         MVC   RCEXIT,=F'0018'     FREE PDS error                       LB2000d
         BAL   R7,MSGPRCR2                                              LB2000d
         L     R15,SAVER15                                              LB2000d
         BAL   R7,R15DJMSG         R15 rc on DJMSG                      LB2000f
*        CVD   R15,DW              Convert to decimal
*        UNPK  DJMSG+3(3),DW+6(2)  Unpack and
*        OI    DJMSG+5,X'F0'       ... force and F zone
         MVC   DRSNCODE,=C'02'     Dynam Alloc error
         MVC   RCEXIT,=F'04'
         MVC   RC,=C'04'
         B     ENDME               Done, exit.
PODDIRNN EQU   *                   DIR option not valid
         ST    R15,SAVER15                                              LB2000d
         MVC   RCEXIT,=F'0015'     DIR option error                     LB2000d
         BAL   R7,MSGPRCR2                                              LB2000d
         L     R15,SAVER15                                              LB2000d

PODDIRXT EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Pre-exit Processing                                  */
*     * /* - Create symbolic variables, FREEMAIN, and           */
*     * /*   return to caller                                   */
*     * /* - R5, R7, R3, R4, R9 Working Register                */
*     * /********************************************************/
ENDME    DS    0H                  Pre-exit processing
*     *
*     * /--------------------------------------------------------/
*     * /* Create Symbolic Variables                            */
*     * /--------------------------------------------------------/
CR8SYMS  EQU   *
         BAL   R7,SETML2           Get DMSGL2 text and length
*     * /* #Data source: 03 SYSMSGLVL1                          */
*     * /* #Data source: 04 SYSMSGLVL2     via SETML1 subrtn    */
         BAL   R7,SETML1           Get DMSGL1 text and length
         ZAP   SNN,=P'+0'          Initialize Snn to 000
         LA    R3,S#@NUM           Number of Symbolic Variables
         LA    R4,S#@              Start addr of Sym Var meta table
*     * /--------------------------------------------------------/
*     * /* Loop through symbol parm table                       */
*     * /--------------------------------------------------------/
S#@VAR   EQU   *
         CLI   PPNL,C'Y'           PNL action for Snn vars?
         BNE   S#@VARLN            NO, use Long Symbolic Names
S#@VARSN EQU   *                   YES, use Short Symbolic Names
         UNPK  SNNUPK,SNN          Unpack seq number 3 digits
         OI    SNNUPK+2,X'F0'      ... force an F zone
         MVI   SNNC,C'S'           Panel Symbols are format 'Snn'
         MVC   SNNC+1(2),SNNUPK+1  Move right-most 2 digits
         LA    R5,SNNC             Set Var Name Address
         ST    R5,NAMP1
         LA    R5,3                Set Var Name Length
         ST    R5,NAML1
         B     S#@VARC             Branch to ...
S#@VARLN EQU   *                   Long  Symbolic Names
         MVC   NAMP1,0(R4)         Set Var Name Address
         MVC   NAML1,4(R4)         Set Var Name Length
*
S#@VARC  EQU   *                   Continue...
         LH    R5,8(R4)            Load Base-Displacement HW
         SH    R5,=X'9000'         Subract Base  from S-adcon
         AR    R5,R9               Add Base R9 to displacement R5
         ST    R5,VALP1            Set Var Value Address
*
         CP    SNN,=P'+3'          Custom MSGL1?
         BE    MSGL1               YES.
         CP    SNN,=P'+4'          Custom MSGL2?
         BE    MSGL2               YES.
         B     OTHR                Normal
*
         EJECT
MSGL1    EQU   *                   Custom MSGL1
         L     R5,DMSGL1L
         B     MSGLX
MSGL2    EQU   *                   Custom MSGL2
         L     R5,DMSGL2L
MSGLX    EQU   *                   Custom common
         ST    R5,VALL1
         B     CONTZ
OTHR     EQU   *                   Normal
         MVC   VALL1,12(R4)        Set Var Value Length
*
CONTZ    EQU   *                   Continue...
         BAL   R5,SETVAR           Set Variable via IKJCT441
         LA    R4,16(R4)           Bump to next Sym Var meta entry
         AP    SNN,=P'+1'          Increment seq no
         BCT   R3,S#@VAR
S#@VARXT EQU   *
*
         EJECT
MYEXIT   EQU   *
*     * /********************************************************/
*     * /* - Restore savearea, FREEMAIN, and                    */
*     * /*   return to caller                                   */
*     * /********************************************************/
*     *
*     * /--------------------------------------------------------/
*     * /* R1 = my Working Storage Address from GETMAIN         */
*     * /********************************************************/
         LR    R1,R13              Working Storage Addr
*     *
*     * /--------------------------------------------------------/
*     * /* Get callers savearea into R13 and overlay my R15     */
*     * /* to be returned to caller                             */
*     * /--------------------------------------------------------/
         L     R13,SAVEAREA+4      Restore R13
         L     R5,RCEXIT           Set R15 to my exit value             LB1020
*     *
*     * /--------------------------------------------------------/
*     * /* Working Storage FREEMAIN                             */
*     * /--------------------------------------------------------/
         LA    R0,WSAREAL          Size of WSAREA DSECT
         FREEMAIN  R,LV=(R0),A=(R1)    Free Storage
*     *
*     * /--------------------------------------------------------/
*     * /* Restore caller registers and return to caller        */
*     * /--------------------------------------------------------/
         LR    R15,R5              R15 = RC for exit                    LB1020
NOMAINS  EQU   *                                                        LB1020
         L     R14,12(R13)         Restore R14
         LM    R0,R12,20(R13)      Restore R0 thru R12
         BR    R14                 Return to caller
*
         TITLE 'LISTDSJ - Error Entry Points                          '
*     * /********************************************************/
*     * /* Error Setting                                        */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
CAMRCERR EQU   *
         C     R15,=F'4'           RC = 4?
         BE    CAMR04              YES.
         C     R15,=F'8'           RC = 8?
         BE    CAMR08              YES.
         C     R15,=F'12'          RC = 12?
         BE    CAMR12              YES.
*                                  NO, must be catalog error
         MVC   DRSNCODE,=C'21'     Catalog error, obtain err rc=8,12,16
         MVC   RCEXIT,=F'16'
         MVC   RC,=C'16'
         B     ENDME               Done, exit.
CAMR04   EQU   *
         MVC   DRSNCODE,=C'22'     Volume not mounted
         MVC   RCEXIT,=F'16'
         MVC   RC,=C'16'
         B     ENDME               Done, exit.
CAMR08   EQU   *
         MVC   DRSNCODE,=C'24'     Dataset not found
         MVC   RCEXIT,=F'16'
         MVC   RC,=C'16'
         B     ENDME               Done, exit.
CAMR12   EQU   *
         MVC   DRSNCODE,=C'23'     Permanent I/O error
         MVC   RCEXIT,=F'16'
         MVC   RC,=C'16'
         B     ENDME               Done, exit.
*
         EJECT
ERR4004  EQU   *
         MVC   RCEXIT,=F'4004'     PARM not supplied
         BAL   R7,PREFXMSG         Prefix start of message
         B     ERR#GO1                                                  LB2000d
ERR4005  EQU   *
         MVC   RCEXIT,=F'4005'     PARM not supplied  R1
         BAL   R7,PREFXMSG         Prefix start of message
         B     ERR#GO1                                                  LB2000d
ERR4008  EQU   *
         MVC   RCEXIT,=F'4008'     CPPL error no variables
         BAL   R7,PREFXMSG         Prefix start of message
         B     ERR#GO1                                                  LB2000d
ERR4009  EQU   *
         MVC   RCEXIT,=F'4009'     CPPL error no variables ZERO
         BAL   R7,PREFXMSG         Prefix start of message
         B     ERR#GO1                                                  LB2000d
ERR4010  EQU   *
         MVC   RCEXIT,=F'4010'     CPPL error no variables NEGATIVE
         BAL   R7,PREFXMSG         Prefix start of message
         B     ERR#GO1                                                  LB2000d
ERR4095  EQU   *                                                        LB2000e
         MVC   RCEXIT,=F'4095'     LDSJISP  not found, link error       LB2000e
         BAL   R7,PREFXMSG         Prefix start of message              LB2000e
         B     ERR#GO2                                                  LB2000e
ERR4096  EQU   *                                                        LB2000d
         MVC   RCEXIT,=F'4096'     LDSJMSG  not found, link error       LB2000d
         BAL   R7,PREFXMSG         Prefix start of message              LB2000d
         MVC   DJMSG+15(MSG4096L),MSG4096  Move to &SYSLISTDSJMSG       LB2000d
         TPUT  DJMSG,MSG4096L+15                                        LB2000d
         B     MYEXIT                                                   LB2000d
ERR4097  EQU   *
         MVC   RCEXIT,=F'4097'     IKJDAIR  not found, link error
         BAL   R7,PREFXMSG         Prefix start of message
         B     ERR#GO2                                                  LB2000d
ERR4098  EQU   *
         MVC   RCEXIT,=F'4098'     IKJCT441 not found, link error
         BAL   R7,PREFXMSG         Prefix start of message
         B     ERR#GO2                                                  LB2000d
ERR4099  EQU   *
         MVC   RCEXIT,=F'4099'     Program requires TSO
         BAL   R7,PREFXMSG         Prefix start of message
         B     ERR#GO2                                                  LB2000d
ERR#GO1  EQU   *                                                        LB2000d
         BAL   R7,MSGPRCR                                               LB2000d
         L     R0,PMSGTXTL                                              LB2000d
         A     R0,=F'15'                                                LB2000d
         LA    R1,DJMSG                                                 LB2000d
         TPUT  (R1),(R0)                                                LB2000d
         B     RSN29                                                    LB2000d
ERR#GO2  EQU   *                                                        LB2000d
         BAL   R7,MSGPRCR                                               LB2000d
         L     R0,PMSGTXTL                                              LB2000d
         A     R0,=F'15'                                                LB2000d
         LA    R1,DJMSG                                                 LB2000d
         TPUT  (R1),(R0)                                                LB2000d
         B     MYEXIT                                                   LB2000d
RSN28    EQU   *                                                        LB1010g
         MVC   DRSNCODE,=C'28'     DDN must be 1-8 characters           LB1010g
         MVC   RCEXIT,=F'16'                                            LB1010g
         MVC   RC,=C'16'                                                LB1010g
         B     ENDME               Done, exit.                          LB1010g
RSN29    EQU   *
         MVC   DRSNCODE,=C'29'     DSN/DD must be specified
         MVC   RCEXIT,=F'16'
         MVC   RC,=C'16'
         B     ENDME               Done, exit.
RSN90    EQU   *                                                        LB1010g
         MVC   DRSNCODE,=C'90'     DDN not found in TIOT                LB1010g
         MVC   RCEXIT,=F'16'                                            LB1010g
         MVC   RC,=C'16'                                                LB1010g
         B     ENDME               Done, exit.                          LB1010g
*
         TITLE 'LISTDSJ - Subroutines                                 '
*     * /********************************************************/      LB1001b
*     * /* Subroutine - Format Date Layout               (R7)   */      LB1001b
*     * /*   DF(1) use date format MM_DD_CCYY                   */      LB1001b
*     * /*   DF(2) use date format DD_MM_CCYY                   */      LB1001b
*     * /*   DF(3) use date format YYYY_MM_DD  DEFAULT          */      LB1001b
*     * /*   DF(4) use date format YYYY_DD_MM                   */      LB1001b
*     * /*                                                      */      LB1001b
*     * /********************************************************/      LB1001b
DFMT     EQU   *                                                        LB1001b
         CLI   DATEFMT,C'2'        DF(2)?                               LB1001b
         BH    DFMT3               DF(3) or DF(4)                       LB1001b
DFMT1    EQU   *                   DF(1) or DF(2)                       LB1001b
         MVC   DATEWA+2(1),DATESEP Date Separator value                 LB1001b
         MVC   DATEWA+5(1),DATESEP Date Separator value                 LB1001b
         MVC   DATEWA+6(4),DC#CCYY+1     CCYY                           LB1001b
         CLI   DATEFMT,C'2'        DF(2)?                               LB1001b
         BE    DFMT2               YES.                                 LB1001b
XFMT1    EQU   *                   DF(1) MM_DD_CCYY                     LB1001b
         MVC   DATEWA+0(2),DC#MM+1       MM                             LB1001b
         MVC   DATEWA+3(2),DC#DD+1       DD                             LB1001b
         B     DFMTXT                                                   LB1001b
DFMT2    EQU   *                   DF(2) DD_MM_CCYY                     LB1001b
         MVC   DATEWA+0(2),DC#DD+1       DD                             LB1001b
         MVC   DATEWA+3(2),DC#MM+1       MM                             LB1001b
         B     DFMTXT                                                   LB1001b
DFMT3    EQU   *                   DF(3) or DF(4)                       LB1001b
         MVC   DATEWA+4(1),DATESEP Date Separator value                 LB1001b
         MVC   DATEWA+7(1),DATESEP Date Separator value                 LB1001b
         MVC   DATEWA+0(4),DC#CCYY+1     CCYY                           LB1001b
         CLI   DATEFMT,C'4'        DF(4)?                               LB1001b
         BE    DFMT4               YES.                                 LB1001b
XFMT3    EQU   *                   DF(3) CCYY_MM_DD                     LB1001b
         MVC   DATEWA+5(2),DC#MM+1       MM                             LB1001b
         MVC   DATEWA+8(2),DC#DD+1       DD                             LB1001b
         B     DFMTXT                                                   LB1001b
DFMT4    EQU   *                   DF(4) CCYY_DD_MM                     LB1001b
         MVC   DATEWA+5(2),DC#DD+1       DD                             LB1001b
         MVC   DATEWA+8(2),DC#MM+1       MM                             LB1001b
         B     DFMTXT                                                   LB1001b
DFMTXT   EQU   *                                                        LB1001b
         BR    R7                  Return to caller                     LB1001b
*                                                                       LB1001b
         EJECT                                                          LB1001b
*     * /********************************************************/
*     * /* Subroutine - Prefix message                   (R7)   */
*     * /* w/ 14-byte header 'LISTSDJ  nnnn -'                  */
*     * /* - R3, R7 Working Register                            */
*     * /********************************************************/
PREFXMSG EQU   *
         MVC   DJMSG+00(8),PGMID
         L     R3,RCEXIT           Load RCEXIT code value
         CVD   R3,DW               Convert to decimal
         UNPK  DJMSG+08(5),DW+5(3) Unpack
         OI    DJMSG+12,X'F0'      Force an F Zone
         MVI   DJMSG+08,C' '       Blank out digit
         MVI   DJMSG+14,C'-'       Blank out digit
*
         BR    R7                  Return to caller
*
*     * /********************************************************/      LB2000f
*     * /* Subroutine - R15 rc on DJMSG                  (R7)   */      LB2000f
*     * /* - R15    Working Register                            */      LB2000f
*     * /********************************************************/      LB2000f
R15DJMSG EQU   *                                                        LB2000f
         CVD   R15,DW              Convert to decimal                   LB2000f
         UNPK  DJMSG+3(3),DW+6(2)  Unpack and                           LB2000f
         OI    DJMSG+5,X'F0'       ... force and F zone                 LB2000f
*                                                                       LB2000f
         BR    R7                  Return to caller                     LB2000f
         EJECT                                                          LB2000d
*     * /********************************************************/      LB2000d
*     * /* Subroutine - Message Processor                (R7)   */      LB2000d
*     * /* Call LDSJMSG  for message text                       */      LB2000d
*     * /* - R0, R1 Working Register (saved and restored)       */      LB2000d
*     * /* - R14 Working Register                               */      LB2000d
*     * /* - R15 Working Register and RC exit code register     */      LB2000d
*     * /********************************************************/      LB2000d
MSGPRCR  EQU   *                   Entry point 1                        LB2000d
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before        LB2000d
         MVC   PMSGREQ,=C'MSG '    Load MESSAGE request type            LB2000d
         MVC   PMSGNBR,RCEXIT      Load message number                  LB2000d
         LA    R1,DJMSG+15         Load DJMSG starting address          LB2000d
         ST    R1,PMSGT                                                 LB2000d
         LA    R1,L'DJMSG-15       Load DJMSG length                    LB2000d
         ST    R1,PMSGL                                                 LB2000d
         MVC   PMSGTXTL,=F'0'      Init msg text length                 LB2000d
         LA    R1,PMSGPALS         R1 = Parm List                       LB2000d
         L     R15,LDSJMSG         Call LDSJMSG                         LB2000d
         BALR  R14,R15                                                  LB2000d
         ST    R15,SAVER15M        SAVE R15                             LB2000d
         LTR   R15,R15             R15=0?  Message Found                LB2000d
         BZ    MSGPRCRX            YES, return                          LB2000d
         C     R15,=F'02'          R15=2?  Message Not Found            LB2000d
         BE    MSGPRCRX            YES, return                          LB2000d
         MVC   DJMSG+15(L'MSGABNRL),MSGABNR                             LB2000d
         CVD   R15,DW              Convert to decimal                   LB2000d
         UNPK  DJMSG+15+(L'MSGABNRL-1)-2(03),DW+6(02) Unpack and        LB2000d
         OI    DJMSG+15+(L'MSGABNRL-1)-0,X'F0'  ... force and F zone    LB2000d
         L     R15,SAVER15M        Restore R15                          LB2000d
*                                                                       LB2000d
MSGPRCRX EQU   *                                                        LB2000d
         LM    R0,R1,DW            RESTORE R0,R1  from DW after         LB2000d
         BR    R7                  Return to caller                     LB2000d
*                                                                       LB2000d
         EJECT                                                          LB2000d
*     * /********************************************************/      LB2000d
*     * /* Subroutine - Message Processor 2              (R7)   */      LB2000d
*     * /* Call LDSJMSG  for message text                       */      LB2000d
*     * /* - R0, R1 Working Register (saved and restored)       */      LB2000d
*     * /* - R14 Working Register                               */      LB2000d
*     * /* - R15 Working Register and RC exit code register     */      LB2000d
*     * /********************************************************/      LB2000d
MSGPRCR2 EQU   *                   Entry point 1                        LB2000d
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before        LB2000d
         MVC   PMSGREQ,=C'MSG '    Load MESSAGE request type            LB2000d
         MVC   PMSGNBR,RCEXIT      Load message number                  LB2000d
         LA    R1,DJMSG+00         Load DJMSG starting address          LB2000d
         ST    R1,PMSGT                                                 LB2000d
         LA    R1,L'DJMSG-00       Load DJMSG length                    LB2000d
         ST    R1,PMSGL                                                 LB2000d
         MVC   PMSGTXTL,=F'0'      Init msg text length                 LB2000d
         LA    R1,PMSGPALS         R1 = Parm List                       LB2000d
         L     R15,LDSJMSG         Call LDSJMSG                         LB2000d
         BALR  R14,R15                                                  LB2000d
         ST    R15,SAVER15M        SAVE R15                             LB2000d
         LTR   R15,R15             R15=0?  Message Found                LB2000d
         BZ    MSGPRC2X            YES, return                          LB2000d
         C     R15,=F'02'          R15=2?  Message Not Found            LB2000d
         BE    MSGPRC2X            YES, return                          LB2000d
         MVC   DJMSG+00(L'MSGABNRL),MSGABNR                             LB2000d
         CVD   R15,DW              Convert to decimal                   LB2000d
         UNPK  DJMSG+00+(L'MSGABNRL-1)-2(03),DW+6(02) Unpack and        LB2000d
         OI    DJMSG+00+(L'MSGABNRL-1)-0,X'F0'  ... force and F zone    LB2000d
         L     R15,SAVER15M        Restore R15                          LB2000d
*                                                                       LB2000d
MSGPRC2X EQU   *                                                        LB2000d
         LM    R0,R1,DW            RESTORE R0,R1  from DW after         LB2000d
         BR    R7                  Return to caller                     LB2000d
*                                                                       LB2000d
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Allocate Dataset for DIR Info    (R7)   */
*     * /* - R0 Working Register                                */
*     * /* - R1 Working Register                                */
*     * /* - R2 Working Register                                */
*     * /* - R3 Working Register                                */
*     * /* - R5 Working Register                                */
*     * /********************************************************/
PDSALC   EQU   *
*
*     * /* Build MYDAPL area comprised of 5 FW                  */
*     * /* using DAPL DSECT with data from CPPL                 */
*
         LA    R1,MYDAPL           MYDAPL area, 5 FW
         USING DAPL,R1             Use DAPL DSECT
         L     R2,MYCPPLP          MYCPPL address
         USING CPPL,R2             Use CPPL DSECT
         MVC   DAPLUPT(4),CPPLUPT  Init DAPLUPT FW1
         MVC   DAPLECT(4),CPPLECT  Init DAPLECT FW2
         LA    R0,MYECB
         ST    R0,DAPLECB
         MVC   DAPLPSCB(4),CPPLPSCB Init DAPLPSCB FW4
         LA    R3,MYDAPB           MYDAPB area, dynalloc block
         ST    R3,DAPLDAPB         Init DAPLDAPB FW5
*     * /*------------------------------------------------------*/
*     * /* Build DAPB08 Allocate Block for Dynamic             */
*     * /* Allocation Interface Routine (DAIR)                  */
*     * /*------------------------------------------------------*/
         USING DAPB08,R3           Use DAPB08 DSECT
         MVC   0(INPB08L,R3),INPB08   Init MYDAPB w X'08' entry code
         LA    R0,DDSNL            Name length and name of Dataset      LB1010e
         ST    R0,DA08PDSN
         MVC   DA08UNIT,=CL8'SYSALLDA'     UNIT=SYSALLDA
         MVC   DA08SER(L'DVOLSER),DVOLSER  VOL=SER=
         MVI   DA08DSP1,DA08SHR            DISP=SHR
         BAL   R5,DAIR             Call DAIR
         LTR   R15,R15             Allocate successful?
         BNZ   PDSALCXT            NO, exit
         LA    R3,MYDAPB           YES, get DDN
         MVC   DDNAME,DA08DDN
         MVI   ALLOCPO,C'Y'        PDS Allocated Flag init to YES       LB1010h
PDSALCXT EQU   *
*
         BR    R7                  Return to caller
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Free     Dataset                 (R7)   */
*     * /********************************************************/
PDSFRE   EQU   *
*     * /*------------------------------------------------------*/
*     * /* Build DAPB18 Free     Block for Dyanamic             */
*     * /* Allocation Interface Routine (DAIR)                  */
*     * /*------------------------------------------------------*/
         LA    R3,MYDAPB           Addr of MYDAPB for DAIR
         USING DAPB18,R3           Use DAPB18 DSECT
         MVC   0(INPB18L,R3),INPB18   Init MYDAPB w X'18' entry code
         MVC   DA18DDN,DDNAME
         LA    R1,MYDAPL           Load R1 with block addr
         BAL   R5,DAIR             Call DAIR
*
PDSFREXT EQU   *
*
         BR    R7                  Return to caller
*     *
*     * /********************************************************/
*     * /* Subroutine - DAIR Link                        (R5)   */
*     * /********************************************************/
DAIR     EQU   *
         PRINT GEN
         LINK  EP=IKJDAIR,MF=(E,(R1)),ERRET=ERR4097       Link
         PRINT NOGEN
*
         BR    R5
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Sum Extents                      (R7)   */
*     * /* - R0, R1 Working Registers                           */
*     * /* - R2 Total CYL                                       */
*     * /* - R3 Total TRKS                                      */
*     * /********************************************************/
TOTEXTS  EQU   *
         SR    R1,R1               Use R1 and R0 for
         SR    R0,R0                ...work CC and HH
         ICM   R1,B'0011',6(R4)    CC High value
         ICM   R0,B'0011',2(R4)    CC Low  value
         SR    R1,R0               Extent total
         AR    R3,R1               R3 = Total CYL (CC)

         ICM   R1,B'0011',8(R4)    HH High value
         LA    R1,1(R1)            HH, up by 1 to account for 0
         ICM   R0,B'0011',4(R4)    HH Low  value
         SR    R1,R0               Extent total
         AR    R2,R1               R2 = Total TRKS (HH)
*
         BR    R7                  Return to caller
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Set MSGL1 Text and Length        (R7)   */
*     * /* Subroutine - Set MSGL2 Text and Length        (R7)   */
*     * /* Set R5     to length of DMSGL1 / DMSGL2 text         */
*     * /* - R3, R4, R6 Working Registers                       */
*     * /* - R5 Length of DMSGL1 text                           */
*     * /********************************************************/
SETML1   EQU   *
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before        LB2000d
         PACK  PK3,RC              Convert message number               LB2000d
         ZAP   DW,PK3                                                   LB2000d
         OI    DW+7,X'0F'          Insert F zone                        LB2000d
         CVB   R1,DW                                                    LB2000d
         ST    R1,PMSGNBR                                               LB2000d
         MVC   PMSGREQ,=C'CD  '    Load MESSAGE request type            LB2000d
         LA    R1,DMSGL1           Load DMSGL1 start address            LB2000d
         ST    R1,PMSGT                                                 LB2000d
         LA    R1,L'DMSGL1         Load DMSGL1 length                   LB2000d
         ST    R1,PMSGL                                                 LB2000d
         MVC   PMSGTXTL,=F'0'      Init msg text length                 LB2000d
         LA    R1,PMSGPALS         R1 = Parm List                       LB2000d
         L     R15,LDSJMSG         Call LDSJMSG                         LB2000d
         BALR  R14,R15                                                  LB2000d
         ST    R15,SAVER15M        SAVE R15                             LB2000d
         MVC   DMSGL1L,PMSGTXTL         R15                             LB2000d
         L     R5,PMSGTXTL         R5=msg text length                   LB2000d
         LTR   R15,R15             R15=0?  Message Found                LB2000d
         BZ    SETML1XT            YES, return                          LB2000d
         MVC   DMSGL1(2),=C'??'    Exhausted table lookup loop          LB2000d
         LA    R5,2                .. msg '??', len=2                   LB2000d
         ST    R5,DMSGL1L          Store Length                         LB2000d
SETML1XT EQU   *                                                        LB2000d
         BR    R7                  Return to caller                     LB2000d
*                                                                       LB2000d
*                                                                       LB2000d
SETML2   EQU   *
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before        LB2000d
         PACK  PK3,DRSNCODE        Convert message number               LB2000d
         ZAP   DW,PK3                                                   LB2000d
         OI    DW+7,X'0F'          Insert F zone                        LB2000d
         CVB   R1,DW                                                    LB2000d
         ST    R1,PMSGNBR                                               LB2000d
         MVC   PMSGREQ,=C'RSN '    Load MESSAGE request type            LB2000d
         LA    R1,DMSGL2           Load DMSGL1 start address            LB2000d
         ST    R1,PMSGT                                                 LB2000d
         LA    R1,L'DMSGL2         Load DMSGL1 length                   LB2000d
         ST    R1,PMSGL                                                 LB2000d
         MVC   PMSGTXTL,=F'0'      Init msg text length                 LB2000d
         LA    R1,PMSGPALS         R1 = Parm List                       LB2000d
         L     R15,LDSJMSG         Call LDSJMSG                         LB2000d
         BALR  R14,R15                                                  LB2000d
         ST    R15,SAVER15M        SAVE R15                             LB2000d
         MVC   DMSGL2L,PMSGTXTL         R15                             LB2000d
         L     R5,PMSGTXTL         R5=msg text length                   LB2000d
         LTR   R15,R15             R15=0?  Message Found                LB2000d
         BZ    SETML2XT            YES, return                          LB2000d
         MVC   DMSGL2(2),=C'??'    Exhausted table lookup loop          LB2000d
         LA    R5,2                .. msg '??', len=2                   LB2000d
         ST    R5,DMSGL2L          Store Length                         LB2000d
SETML2XT EQU   *                                                        LB2000d
         BR    R7                  Return to caller                     LB2000d
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Date Convert CCYY/JJJ            (R7)   */
*     * /* Date windowing of 70.                                */
*     * /* - R5 Working Register                                */
*     * /********************************************************/
DATECONV EQU   *
         XC    FW,FW               Clear contents
         MVC   FW+3(1),0(R6)       Get YY portion
         L     R5,FW               Load FW to R5
         C     R5,Y2KWIN           Before Y2k Window?
         BL    DATECY2             YES, assume CC 20
DATECY1  EQU   *
         LA    R5,1900(R5)         NO,  assume CC 19
         B     DATECCNT            Continue...
DATECY2  EQU   *
         LA    R5,2000(R5)         CC 20...
DATECCNT EQU   *
*     * /*------------------------------------------------------*/
*     * /* CCYY, JJJ                                            */
*     * /*------------------------------------------------------*/
         CVD   R5,DW               R5   to decimal
         UNPK  DC#CCYY,DW+5(3)     Unpack 0CCYY and
         OI    DC#CCYY+4,X'F0'      ... force and F zone
         XC    FW,FW               Clear contents
         MVC   FW+2(2),1(R6)       Get 0JJJ portion
         L     R5,FW                ... and convert
         CVD   R5,DW                ... to decimal
         UNPK  DC#JJJ,DW+6(2)      Unpack JJJ and
         OI    DC#JJJ+2,X'F0'       ... force and F zone
*
         EJECT
DATELEAP EQU   *
         LA    R3,0                Leap Count (0 or 1)
         STH   R5,DC#HJJJ          R5 contains JJJ
         CH    R5,=H'59'           R5 = JJJ, > 2/28?
         BNH   NOLEAP              NO, not leap year eligible
*                                  YES, check for leap year
         PACK  DC#YR,DC#CCYY       Pack CCYY
         DP    DC#YR,=P'100'       CCYY / 100
         CP    DC#YR+2(2),=P'0'    Remainder 0?
         BH    NOLEAP              NO, not leap year
         DP    DC#YR,=P'400'       YES, check CCYY / 400
         CP    DC#YR+2(2),=P'0'    Remainder 0?
         BH    NOLEAP              NO, not leap year
YESLEAP  EQU   *                   YES, leap year
         LA    R3,1
NOLEAP   EQU   *
         LA    R5,12               Number of Months, 12!
         LA    R6,DAYSTBLJ         Addr of Julian Days Table,
CHKME    EQU   *
         LH    R8,0(R6)            Load R8 with month julian start days
         CH    R5,=H'2'            JAN or FEB?
         BE    NOADD               YES,
         BL    NOADD               YES,
         AR    R8,R3               NO, Account for leap year
NOADD    EQU   *
         CH    R8,DC#HJJJ          Found MONTH based on number of days?
***      BE    DONELOOP            YES, done. 334  350        334  334
         BL    DONELOOP            YES, done.
         LA    R6,2(R6)            NO, bump up to next month
         BCT   R5,CHKME            Look again.

DONELOOP EQU   *
*     * /*------------------------------------------------------*/
*     * /* MM, DD                                               */
*     * /*------------------------------------------------------*/
*        R5 month
         CVD   R5,DW               R5   to decimal
         UNPK  DC#MM,DW+6(2)       Unpack 0MM   and
         OI    DC#MM+2,X'F0'        ... force and F zone
         LH    R5,DC#HJJJ          Load R5 with JJJ
         SR    R5,R8               JJJ - Julian Days in Month
         CVD   R5,DW               R5   to decimal
         UNPK  DC#DD,DW+6(2)       Unpack 0DD   and
         OI    DC#DD+2,X'F0'        ... force and F zone
*
         BR    R7                  Return to caller
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Store Symbolic Variable        (R5)     */
*     * /* -JMSG'RC=xxx IKJCT441 error w zzzzzzzzzzzzzzzzzz'    */
*     * /********************************************************/
SETVAR   EQU   *
         LA    R1,CT441LST         R1 = Parm List
*        PRINT GEN
**       LINK  EP=IKJCT441,ERRET=ERR4098   Link                         LB1010i
         L     R15,IKJCT441                                             LB1010i
         BALR  R14,R15                                                  LB1010i
*        PRINT NOGEN
         ST    R15,SAVER15         SAVE R15
*        DISPLAY DATA=((R15),R,)
         LTR   R15,R15             R15=0?
         BZ    SETVARXT            YES.
*                                  NO.
         ST    R15,SAVER15                                              LB2000d
         MVC   RCEXIT,=F'0027'     IKJCT441 error                       LB2000d
         BAL   R7,MSGPRCR2                                              LB2000d
         L     R15,SAVER15                                              LB2000d
         BAL   R7,R15DJMSG         R15 rc on DJMSG                      LB2000f
*        CVD   R15,DW              Convert to decmial
*        UNPK  DJMSG+3(3),DW+6(2)  Unpack and
*        OI    DJMSG+5,X'F0'       ... force and F zone
         L     R0,NAMP1
         L     R1,NAML1     18
         C     R1,=F'18'
         BH    MVCMAX
         B     MVCOK
MVCMAX   EQU   *
         LA    R1,18
MVCOK    EQU   *
         EX    R1,MVCSYS
         B     SETVARXT
MVCSYS   MVC   DJMSG+24(00),0(R0)
*
SETVARXT EQU   *
         BR    R5                  Return to caller
*
*     * /********************************************************/
*     * /* Return codes for IKJCT441                            */
*     * /*                                                      */
*     * /*  0 - SUCCESS                                         */
*     * /*  4 - VARIABLE RETURNED SHOULDN'T BE RE-SCANNED       */
*     * /*  8 - VARIABLE RETURNED REQUIRES EVALUATION           */
*     * /* 12 - VARIABLE RETURNED IS A LABEL (CANNOT UPDATE)    */
*     * /* 16 - SYSTEM VARIABLE - CAN'T BE UPDATED BY THE USER  */
*     * /* 20 - FOR LOCATE - NO MORE VARIABLES TO RETURN        */
*     * /* 32 - GETMAIN/FREEMAIN FAILURE                        */
*     * /* 36 - NAME LENGTH < 1 OR > 252                        */
*     * /*      -OR- VALUE LENGTH < 0 OR > 32767                */
*     * /* 40 - INCORRECT ENVIRONMENT -                         */
*     * /*      PLIST ERROR OR NO CLIST ACTIVE                  */
*     * /* 44 - INVALID ENTRY CODE                              */
*     * /* 52 - UNDEFINED VARIABLE                              */
*     * /*                                                      */
*     * /********************************************************/
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* Subroutine - ISPF VDEFINE                   (R7)     */      LB1010h
*     * /* - R1, R14, R15 Working Registers                     */      LB2000e
*     * /********************************************************/      LB1010h
VARDEFS  EQU   *                                                        LB1010h
*     /* CALL  ISPLINK,(VDEFINE,IMEMBRN,IMEMBR,CHAR,IMEMBRL),VL */      LB1010h
*     /*        (for each var)                                  */      LB2000e
         MVC   PISPREQ,=C'1   '                                         LB2000e
         LA    R1,PISPPALS         R1 = ISPF Parm List                  LB2000e
         L     R15,LDSJISP                                              LB2000e
         BALR  R14,R15                                                  LB2000e
         LTR   R15,R15             R15=0?                               LB2000e
         BNZ   ISPFERR             No, error - terminate                LB2000e
         BR    R7                  Yes, return to caller                LB2000e
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* Subroutine - ISPF TBERASE                   (R7)     */      LB1010h
*     * /* - R1, R14, R15 Working Registers                     */      LB2000e
*     * /********************************************************/      LB1010h
ERASETB  EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(TBERASE,TBN),VL                         */      LB1010h
         MVC   PISPREQ,=C'2   '                                         LB2000e
         LA    R1,PISPPALS         R1 = ISPF Parm List                  LB2000e
         L     R15,LDSJISP                                              LB2000e
         BALR  R14,R15                                                  LB2000e
         LTR   R15,R15             R15=0?                               LB2000e
         BNZ   ISPFERR             No, error - terminate                LB2000e
         BR    R7                  Yes, return to caller                LB2000e
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* Subroutine - ISPF TBCREATE                  (R7)     */      LB1010h
*     * /* - R1, R14, R15 Working Registers                     */      LB2000e
*     * /********************************************************/      LB1010h
CREATETB EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(TBCREATE,TBN,KLIST,NLIST),VL            */      LB1010h
         MVC   PISPREQ,=C'3   '                                         LB2000e
         LA    R1,PISPPALS         R1 = ISPF Parm List                  LB2000e
         L     R15,LDSJISP                                              LB2000e
         BALR  R14,R15                                                  LB2000e
         LTR   R15,R15             R15=0?                               LB2000e
         BNZ   ISPFERR             No, error - terminate                LB2000e
         BR    R7                  Yes, return to caller                LB2000e
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* Subroutine - ISPF TBADD                     (R7)     */      LB1010h
*     * /* - R1, R14, R15 Working Registers                     */      LB2000e
*     * /********************************************************/      LB1010h
ADDTB    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(TBADD,TBN),VL                           */      LB1010h
         MVC   PISPREQ,=C'4   '                                         LB2000e
         LA    R1,PISPPALS         R1 = ISPF Parm List                  LB2000e
         L     R15,LDSJISP                                              LB2000e
         BALR  R14,R15                                                  LB2000e
         LTR   R15,R15             R15=0?                               LB2000e
         BNZ   ISPFERR             No, error - terminate                LB2000e
         BR    R7                  Yes, return to caller                LB2000e
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* Subroutine - ISPF TBSAVE                    (R7)     */      LB1010h
*     * /* - R1, R14, R15 Working Registers                     */      LB2000e
*     * /********************************************************/      LB1010h
SAVETB   EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(TBSAVE,TBN),VL                          */      LB1010h
         MVC   PISPREQ,=C'5   '                                         LB2000e
         LA    R1,PISPPALS         R1 = ISPF Parm List                  LB2000e
         L     R15,LDSJISP                                              LB2000e
         BALR  R14,R15                                                  LB2000e
         LTR   R15,R15             R15=0?                               LB2000e
         BNZ   ISPFERR             No, error - terminate                LB2000e
         BR    R7                  Yes, return to caller                LB2000e
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* Subroutine - ISPF TBCLOSE                   (R7)     */      LB1010h
*     * /* - R1, R14, R15 Working Registers                     */      LB2000e
*     * /********************************************************/      LB1010h
CLOSETB  EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(TBCLOSE,TBN),VL                         */      LB1010h
         MVC   PISPREQ,=C'6   '                                         LB2000e
         LA    R1,PISPPALS         R1 = ISPF Parm List                  LB2000e
         L     R15,LDSJISP                                              LB2000e
         BALR  R14,R15                                                  LB2000e
         LTR   R15,R15             R15=0?                               LB2000e
         BNZ   ISPFERR             No, error - terminate                LB2000e
         BR    R7                  Yes, return to caller                LB2000e
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* Subroutine - ISPF VDELETE                   (R7)     */      LB1010h
*     * /* - R1, R14, R15 Working Registers                     */      LB2000e
*     * /********************************************************/      LB1010h
VARDELS  EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDELETE,IMEMBRN),VL                     */      LB1010h
*     /*        (for each var)                                  */      LB2000e
         MVC   PISPREQ,=C'7   '                                         LB2000e
         LA    R1,PISPPALS         R1 = ISPF Parm List                  LB2000e
         L     R15,LDSJISP                                              LB2000e
         BALR  R14,R15                                                  LB2000e
         LTR   R15,R15             R15=0?                               LB2000e
         BNZ   ISPFERR             No, error - terminate                LB2000e
         BR    R7                  Yes, return to caller                LB2000e
*
         TITLE 'LISTDSJ - Literal Pool                                '
         LTORG
*
         TITLE 'LISTDSJ - Constants                                   '
*     * /********************************************************/
*     * /* Parm Input Keywords                                  */
*     * /********************************************************/
KWDIR9   DC    C'DIRECTORY'        Keyword DIRECTORY                    LB1010b
KWDIR    DC    C'DIR'              Keyword DIR
KWPNL    DC    C'PNL'              Keyword PNL
KWABOUT  DC    C'ABOUT'            Keyword ABOUT
KWVOLUME DC    C'VOLUME('          Keyword VOLUME                       LB1010b
KWVOL    DC    C'VOL('             Keyword VOL
KWDS     DC    C'DS('              Keyword DS                           LB1001a
KWDF     DC    C'DF('              Keyword DF                           LB1001b
ZEROCL11 DC    C'00000000000'      11 C'0'                              LB1001b
KWFILE   DC    C'FILE'             Keyword FILE                         LB1001g
KWDL0    DC    C'DL0'              Keyword DL0                          LB1001h
KWDL1    DC    C'DL1'              Keyword DL1                          LB1001h
KWTSOB   DC    C'TSOB'             Keyword TSOB                         LB1040c
*     *
*     * /********************************************************/
*     * /* Entry codes for IKJCT441                             */
*     * /********************************************************/
ECUPDT   DC    A(TSVEUPDT)         ENTRY CODE FOR UPDATE/CREATE
TOKEN    DC    F'0'                TOKEN
*     *
*     * /********************************************************/
*     * /* Julian start day by month in reverse order           */
*     * /********************************************************/
DAYSTBLJ DC    H'334',H'304',H'273',H'243',H'212',H'181'   Dec-Jul
         DC    H'151',H'120',H'090',H'059',H'031',H'000'   Jun-Jan
Y2KWIN   DC    F'70'               Y2k Window value
*
         EJECT
*     * /********************************************************/
*     * /* Symbolic Variable Names for CLIST use                */
*     * /********************************************************/
V#@      EQU   *
VDSN     DC    C'SYSDSNAME'
VRSCODE  DC    C'SYSREASON'
VMSGL1   DC    C'SYSMSGLVL1'
VMSGL2   DC    C'SYSMSGLVL2'
VVOL     DC    C'SYSVOLUME'
VLRECL   DC    C'SYSLRECL'
VBLKSZ   DC    C'SYSBLKSIZE'
VKEYL    DC    C'SYSKEYLEN'
VKEYP    DC    C'SYSKEYPOS'
VDSORG   DC    C'SYSDSORG'
VRECFM   DC    C'SYSRECFM'
VCREDT   DC    C'SYSCREATE'
VEXPDT   DC    C'SYSEXDATE'
VREFDT   DC    C'SYSREFDATE'
VCREDTJ  DC    C'SYSJCREATE'
VEXPDTJ  DC    C'SYSJEXDATE'
VREFDTJ  DC    C'SYSJREFDATE'
VCREDTC  DC    C'SYSCCREATE'
VEXPDTC  DC    C'SYSCEXDATE'
VREFDTC  DC    C'SYSCREFDATE'
VPASSWD  DC    C'SYSPASSWORD'
VRACF    DC    C'SYSRACFA'
VSSPC    DC    C'SYSUNITS'
VSSPCNM  DC    C'SYSSECONDS'
VUNIT    DC    C'SYSUNIT'
VEXTS    DC    C'SYSEXTENTS'
VNUMVOL  DC    C'SYSNUMVOLS'
VCCVOL   DC    C'SYSCYLVOL'
VTTCC    DC    C'SYSTRKSCYL'
VTRKLEN  DC    C'SYSTRKLEN'
VBLKTRK  DC    C'SYSBLKSTRK'
VUNITCAP DC    C'SYSUNITCAP'
VTKAL    DC    C'SYSTRKSALLOC'
VTKUS    DC    C'SYSTRKSUSED'
VTKUUS   DC    C'SYSTRKSUNUSED'
VJMSG    DC    C'SYSLISTDSJMSG'
VMEMS    DC    C'SYSMEMBERS'
VALIAS   DC    C'SYSMEMBERSALIAS'
VDIRBKSU DC    C'SYSUDIRBLK'
VDIRBKSN DC    C'SYSNUDIRBLK'
VDIRBKSA DC    C'SYSADIRBLK'
VDSCAT   DC    C'SYSDSCAT'
VDSCATV  DC    C'SYSDSCATV'
VPSPCAL  DC    C'SYSPRIMARY'
VPSPCUS  DC    C'SYSUSED'
VVOLLST  DC    C'SYSVOLUMES'
VALLOC   DC    C'SYSALLOC'                                              LB1010d
VUPDTD   DC    C'SYSUPDATED'                                            LB1040a
V#@LEN   EQU   *-V#@               Length of area
*
         EJECT
*     * /********************************************************/
*     * /* Internal table of symbolic variables and data values */
*     * /********************************************************/
S#@      DS    0F
*     * /*     VARNAME,VARLEN
*     * /*     VALNAME,VALLEN
A#DSN    DC    A(VDSN),A(L'VDSN)         00 SYSDSNAME
         DC    S(DDSN),A(L'DDSN)
S#@ENTL  EQU   *-S#@
A#JMSG   DC    A(VJMSG),A(L'VJMSG)       01 SYSLISTDSJMSG
         DC    S(DJMSG),A(L'DJMSG)
A#RSC    DC    A(VRSCODE),A(L'VRSCODE)   02 SYSREASON
         DC    S(DRSNCODE),A(L'DRSNCODE)
A#MSG1   DC    A(VMSGL1),A(L'VMSGL1)     03 SYSMSGLVL1
         DC    S(DMSGL1),A(L'DMSGL1)
A#MSG2   DC    A(VMSGL2),A(L'VMSGL2)     04 SYSMSGLVL2
         DC    S(DMSGL2),A(L'DMSGL2)
A#CREDT  DC    A(VCREDT),A(L'VCREDT)     05 SYSCREATE
         DC    S(DCREDT),A(L'DCREDT)
A#EXPDT  DC    A(VEXPDT),A(L'VEXPDT)     06 SYSEXDATE
         DC    S(DEXPDT),A(L'DEXPDT)
A#REFDT  DC    A(VREFDT),A(L'VREFDT)     07 SYSREFDATE
         DC    S(DREFDT),A(L'DREFDT)
A#DSORG  DC    A(VDSORG),A(L'VDSORG)     08 SYSDSORG
         DC    S(DDSORG),A(L'DDSORG)
A#RECFM  DC    A(VRECFM),A(L'VRECFM)     09 SYSRECFM
         DC    S(DRECFM),A(L'DRECFM)
A#LRECL  DC    A(VLRECL),A(L'VLRECL)     10 SYSLRECL
         DC    S(DLRECL),A(L'DLRECL)
A#BLKSZ  DC    A(VBLKSZ),A(L'VBLKSZ)     11 SYSBLKSIZE
         DC    S(DBLKSZ),A(L'DBLKSZ)
A#KEYL   DC    A(VKEYL),A(L'VKEYL)       12 SYSKEYLEN
         DC    S(DKEYL),A(L'DKEYL)
A#KEYP   DC    A(VKEYP),A(L'VKEYP)       13 SYSKEYPOS
         DC    S(DRKP),A(L'DRKP)
A#PASSWD DC    A(VPASSWD),A(L'VPASSWD)   14 SYSPASSWORD
         DC    S(DPASSWD),A(L'DPASSWD)
A#RACF   DC    A(VRACF),A(L'VRACF)       15 SYSRACF
         DC    S(DRACF),A(L'DRACF)
A#DBKSA  DC    A(VDIRBKSA),A(L'VDIRBKSA) 16 SYSADIRBLK
         DC    S(DDBLKS),A(L'DDBLKS)
A#DBKSU  DC    A(VDIRBKSU),A(L'VDIRBKSU) 17 SYSUDIRBLK
         DC    S(DDIRBKSU),A(L'DDIRBKSU)
A#DBKSN  DC    A(VDIRBKSN),A(L'VDIRBKSN) 18 SYSNUDIRBLK
         DC    S(DDIRBKSN),A(L'DDIRBKSN)
A#MEMS   DC    A(VMEMS),A(L'VMEMS)       19 SYSMEMBERS
         DC    S(DMEMBERS),A(L'DMEMBERS)
A#ALIAS  DC    A(VALIAS),A(L'VALIAS)     20 SYSMEMBERSALIAS
         DC    S(DALIASES),A(L'DALIASES)
A#VOL    DC    A(VVOL),A(L'VVOL)         21 SYSVOLUME
         DC    S(DVOLSER),A(L'DVOLSER)
A#UNIT   DC    A(VUNIT),A(L'VUNIT)       22 SYSUNIT
         DC    S(DUNIT),A(L'DUNIT)
A#UNITS  DC    A(VSSPC),A(L'VSSPC)       23 SYSUNITS
         DC    S(DSSPC),A(L'DSSPC)
A#VTKUS  DC    A(VTKUS),A(L'VTKUS)       24 SYSTRKSUSED
         DC    S(DTRKSUS),A(L'DTRKSUS)
A#EXTS   DC    A(VEXTS),A(L'VEXTS)       25 SYSEXTENTS
         DC    S(DEXTS),A(L'DEXTS)
A#VTKAL  DC    A(VTKAL),A(L'VTKAL)       26 SYSTRKSALLOC
         DC    S(DTRKSAL),A(L'DTRKSAL)
A#SCONDS DC    A(VSSPCNM),A(L'VSSPCNM)   27 SYSSECONDS
         DC    S(DSSPCNM),A(L'DSSPCNM)
A#TKUUS  DC    A(VTKUUS),A(L'VTKUUS)     28 SYSTRKSUNUSED
         DC    S(DTRKSUUS),A(L'DTRKSUUS)
A#VCCVOL DC    A(VCCVOL),A(L'VCCVOL)     29 SYSCYLVOL
         DC    S(DCYL4VOL),A(L'DCYL4VOL)
A#VTTCC  DC    A(VTTCC),A(L'VTTCC)       30 SYSTRKSCYL
         DC    S(DTRKSCYL),A(L'DTRKSCYL)
A#CREDTJ DC    A(VCREDTJ),A(L'VCREDTJ)   31 SYSJCREATE
         DC    S(DCREDJ),A(L'DCREDJ)
A#EXPDTJ DC    A(VEXPDTJ),A(L'VEXPDTJ)   32 SYSJEXDATE
         DC    S(DEXPDJ),A(L'DEXPDJ)
A#REFDTJ DC    A(VREFDTJ),A(L'VREFDTJ)   33 SYSJREFDATE
         DC    S(DREFDJ),A(L'DREFDJ)
A#VTRKLN DC    A(VTRKLEN),A(L'VTRKLEN)   34 SYSTRKLEN
         DC    S(DTRKLEN),A(L'DTRKLEN)
A#VUCAP  DC    A(VUNITCAP),A(L'VUNITCAP) 35 SYSUNITCAP
         DC    S(DUNITCAP),A(L'DUNITCAP)
A#VBLKTK DC    A(VBLKTRK),A(L'VBLKTRK)   36 SYSBLKSTRK
         DC    S(DBLKTRK),A(L'DBLKTRK)
A#CREDTC DC    A(VCREDTC),A(L'VCREDTC)   37 SYSCCREATE
         DC    S(DCREDC),A(L'DCREDC)
A#EXPDTC DC    A(VEXPDTC),A(L'VEXPDTC)   38 SYSCEXDATE
         DC    S(DEXPDC),A(L'DEXPDC)
A#REFDTC DC    A(VREFDTC),A(L'VREFDTC)   39 SYSCREFDATE
         DC    S(DREFDC),A(L'DREFDC)
A#NUMVOL DC    A(VNUMVOL),A(L'VNUMVOL)   40 SYSNUMVOLS
         DC    S(DNUMVOL),A(L'DNUMVOL)
A#DSCAT  DC    A(VDSCAT),A(L'VDSCAT)     41 SYSDSCAT
         DC    S(DDSCAT),A(L'DDSCAT)
A#DSCATV DC    A(VDSCATV),A(L'VDSCATV)   42 SYSDSCATV
         DC    S(DDSCATV),A(L'DDSCATV)
A#PSPCAL DC    A(VPSPCAL),A(L'VPSPCAL)   43 SYSPRIMARY
         DC    S(DPSPCAL),A(L'DPSPCAL)
A#PSPCUS DC    A(VPSPCUS),A(L'VPSPCUS)   44 SYSUSED
         DC    S(DPSPCUS),A(L'DPSPCUS)
A#DVOLS  DC    A(VVOLLST),A(L'VVOLLST)   45 SYSVOLUMES
         DC    S(DVOLLST),A(L'DVOLLST)
A#DALLOC DC    A(VALLOC),A(L'VALLOC)     46 SYSALLOC                    LB1010d
         DC    S(DALLOC),A(L'DALLOC)                                    LB1010d
A#DUPDTD DC    A(VUPDTD),A(L'VUPDTD)     47 SYSUPDATED                  LB1040a
         DC    S(DUPDTD),A(L'DUPDTD)                                    LB1040a
S#@LEN   EQU   *-S#@               Length of area
S#@NUM   EQU   S#@LEN/S#@ENTL      Number of entries
*
         EJECT
*     * /********************************************************/
*     * /* Messages - Internal                                  */
*     * /********************************************************/
MSG4096  DC    C'Cannot link to LDSJMSG'                                LB2000d
MSG4096L EQU   *-MSG4096                                                LB2000d
MSGABNR  DC    C'* * *Abnormal error detected; LDSJMSG  RC=xxx'         LB2000d
MSGABNRL EQU   *-MSGABNR                                                LB2000d
*
         EJECT
*     * /********************************************************/
*     * /* Initialized DAPB08 Allocate Block                    */
*     * /********************************************************/
INPB08   DS    0D            Initialized DAPB08 Block
IN08CD   DC    X'0008'       DAIR ENTRY CODE
IN08FLG  DC    X'00'         FUNCTIONS TO BE PERFORMED WHEN RC=0
         DC    X'00'
IN08DARC DC    H'0'          DYN ALLOC RETURN CODE
IN08CTRC DC    H'0'          CATALOG RETURN CODE
IN08PDSN DC    A(0)          POINTER TO DSNAME TO BE SEARCHED IN DSE
IN08DDN  DC    CL8' '        DDNAME TO BE SEARCHED IN DSE
IN08UNIT DC    CL8' '        UNITNAME REQUESTED
IN08SER  DC    CL8' '        VOLUME SERIAL NUMBER-PADDED W/BLANKS
IN08BLK  DC    X'00000000'   DATA SET  AVERAGE RECORD LENGTH
IN08PQTY DC    X'00000000'   PRIMARY SPACE QUANTITY
IN08SQTY DC    X'00000000'   SECONDARY SPACE QUANTITY
IN08DQTY DC    X'00000000'   DIRECTORY BLOCK QUANTITY
IN08MNM  DC    CL8' '        MEMBER NAME
IN08PSWD DC    CL8' '        PASSWORD
IN08DSP1 DC    X'00'         DATA SET STATUS FLGS(IF=0 OLD ASSUMED)
IN08DPS2 DC    X'00'         DATA SET DISPOSITION(IF=0 KEEP ASSUMED)
IN08DPS3 DC    X'00'         DATA SET CONDITIONAL DISPOSITION
IN08CTL  DC    X'00'         FLAGS TO CONTROL ACTIONS TAKEN BY DAIR
         DC    X'000000'     RESERVED
IN08DSO  DC    X'00'         DSORG
IN08ALN  DC    CL8' '        ATTR-LIST-NAME
INPB08L  EQU   *-INPB08      Length of Initialized DAPB08 Block
*
         EJECT
*     * /********************************************************/
*     * /* Initialized DAPB18 Free     Block                    */
*     * /********************************************************/
INPB18   DS    0D            Initialized DAPB18 Block
IN18CD   DC    X'0018'       DAIR ENTRY CODE
IN18FLG  DC    X'00'         FUNCTIONS TO BE PERFORMED WHEN RC=0
         DC    X'00'
IN18DARC DC    H'0'          DYNAMIC ALLOCATION RETURN CODE
IN18CTRC DC    H'0'          CATALOG RETURN CODE AREA
IN18PDSN DC    A(0)          POINTER TO DSNAME TO BE SEARCHED IN DSE
IN18DDN  DC    CL8' '        DDNAME TO BE SEARCHED IN DSE
IN18MNM  DC    CL8' '        MEMBER NAME
IN18SCLS DC    CL2' '        SYSOUT CLASS DESIRED WHEN UNALLOCATING A
*                            SYSOUT DATA SET
IN18DPS2 DC    X'00'         DATA SET DISPOSITION
IN18CTL  DC    X'00'         FLAGS FOR SPECIAL DAIR PROCESSING
IN18JBNM DC    CL8' '        IGNORED AS OF OS VS/2 RELEASE 2
INPB18L  EQU   *-INPB18      Length of Initialized DAPB18 Block
*
         EJECT
*     * /********************************************************/      LB2000z
*     * /* Hex Table contants                                   */      LB2000z
*     * /********************************************************/      LB2000z
HEXTAB   DC    C'01234567'                                              LB1010h
         DC    C'89ABCDEF'                                              LB1010h
         EJECT
*     * /********************************************************/
*     * /* PDS Directory DCB and READ as model                  */
*     * /********************************************************/
         PRINT GEN
DIRDCB   DCB   DDNAME=DYNAM,DSORG=PO,MACRF=R,EODAD=PODEOF,SYNAD=PODSYN,X
               RECFM=U,BLKSIZE=256
DIRDCBL  EQU   *-DIRDCB
*
DIRREADM READ  DIRDECB,SF,MF=L
DIRDECBL EQU   *-DIRDECB
*
         TITLE 'LISTDSJ - Equates                                     '
*     * /********************************************************/
*     * /* E Q U A T E S                                        */
*     * /********************************************************/
*     *
*     * /********************************************************/
*     * /* IKJCT441 Entry Code Values                           */
*     * /********************************************************/
TSVEUPDT EQU   2                   Update/Create Request
*     *
*     * /********************************************************/
*     * /* Register Equates                                     */
*     * /********************************************************/
         YREGS                     Register EQU as Rn or Rnn
*
*R0       EQU   0
*R1       EQU   0
*R2       EQU   0
*R3       EQU   0
*R4       EQU   0
*R5       EQU   0
*R6       EQU   0
*R7       EQU   0
*R8       EQU   0
*R9       EQU   0
*R10      EQU   0
*R11      EQU   0
*R12      EQU   0
*R13      EQU   0
*R14      EQU   0
*R15      EQU   0
*
*     * /********************************************************/
*     * /* Other    Equates                                     */
*     * /********************************************************/
DDNAM    EQU   40                  DDNAME DCB offset
*
         TITLE 'LISTDSJ - System DSECTs                               '
*     *
*     * /********************************************************/
*     * /* System DSECTs                                        */
*     * /********************************************************/
         IHAPSA                    Prefixed Save Area
PSALEN   EQU   *-PSA               Length of PSA
         EJECT
         CVT   DSECT=YES,LIST=YES  Communication Vector Table
         EJECT                                                          LB1010g
         IKJTCB DSECT=YES,LIST=YES Task Control Block                   LB1010g
         EJECT                                                          LB1010g
TIOT     DSECT                                                          LB1010g
         IEFTIOT1                  Task IO Table                        LB1010g
         EJECT                                                          LB1010g
JFCB     DSECT                                                          LB1010g
         IEFJFCBN  LIST=YES        Job File Control BLock               LB1010g
         EJECT
         IHAASCB                   Address Space Control Block
ASCBLEN  EQU   *-ASCB              Length of ASCB
         EJECT
UCB      DSECT
         IEFUCBOB  LIST=YES,PREFIX=NO   Unit Control Block
         EJECT
         IKJCPPL                   Command Processor Parm List
CPPLLEN  EQU   *-CPPL              Length of CPPL
         EJECT                                                          LB1010e
         IKJUPT                    User Profile Table                   LB1010e
UPTLEN   EQU   *-UPT               Length of UPT                        LB1010e
         EJECT
         IKJPPL                    Parse Parm List
PPLLEN   EQU   *-PPL               Length of PPL
         EJECT
         IKJIOPL                   I/O Service Routine Parm List
IOPLLEN  EQU   *-IOPL              Length of IOPL
         EJECT
         IKJDAPL                   Dynamic Allocation Parm LIst
DAPLLEN  EQU   *-DAPL              Length of DAPL
         EJECT
         IKJDAP08                  Operation block ALLOC Dataset
DAP08LEN EQU   *-DAPB08            Length of DAP08  LEN=54
         EJECT
         IKJDAP18                  Operation block FREE  Dataset
DAP18LEN EQU   *-DAPB18            Length of DAP18  LEN=28
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* ISPF Statistics - directory user area                */      LB1010h
*     * /********************************************************/      LB1010h
DIRENTUA DSECT                     Map of directory entry (ISPF stats)  LB1010h
UAMMEM   DS    CL8                 CHAR      Member Name                LB1010h
UAMTTR   DS    XL3                 .. See SYS1.MODGEN(IHAPDS)           LB1010h
UAMC     DS    X                       Last 5 bits have number of       LB1010h
*                                      halfwords of user data which     LB1010h
*                                                                       LB1010h
UAMSPF   DS   0XL30                .. SPF statistics (30 bytes)         LB1010h
UAMVLVL  DS    X                   Byte 01 Version Number x'01'-'99'    LB1010h
UAMMLVL  DS    X                   Byte 02 Modification   x'00'-'99'    LB1010h
UAMRES1  DS    X                   Byte 03 Reserved                     LB1010h
*                                   Bit 1 ON  member last edited by     LB1010h
*                                             SCLM                      LB1010h
*                                   Bit 1 OFF member processed outside  LB1010h
*                                             of SCLM                   LB1010h
*                                   Bit 2 Reserved                      LB1010h
*                                   Bit 3 Indicates ISPF extended       LB1010h
*                                         statistics exist              LB1010h
*                                   Bit 4-7 reserved for future ISPF    LB1010h
*                                   Bit 8 Reserved                      LB1010h
UAMCHGSS DS    X                   BYTE 04 Seconds portion of time last LB1010h
*                                          modified, packed deciaml SS  LB1010h
UAMCREDT DS    PL4                 BYTE 5-8 Creation Date               LB1010h
*                                   byte 5   Century  x'00'=1900,       LB1010h
*                                                     x'01'=2000, etc   LB1010h
*                                   byte 6-8 Julian Date,               LB1010h
*                                            packed decimal             LB1010h
UAMCHGDT DS    PL4                 BYTE 9-12 Date Last Modified         LB1010h
*                                   byte 9   Century  x'00'=1900,       LB1010h
*                                                     x'01'=2000, etc   LB1010h
*                                   byte 10-12 Julian Date,             LB1010h
*                                            packed decimal             LB1010h
UAMCHGTM DS    XL2                 BYTE 13-14 Time Last Modified        LB1010h
*                                   byte 13 Hours, packed decimal       LB1010h
*                                   byte 14 Minutes, packed decimal     LB1010h
UAMCURR  DS    H                   BYTE 15-16 Current Number of Lines   LB1010h
*                                   hexadecimal format                  LB1010h
UAMINIT  DS    H                   BYTE 17-18 Initial Number of Lines   LB1010h
*                                   hexadecimal format                  LB1010h
UAMMOD   DS    H                   BYTE 19-20 Number of Modified Lines  LB1010h
*                                   hexadecimal format                  LB1010h
UAMUID   DS    CL7                 BYTE 21-27 UserID, character format  LB1010h
*                                                                       LB1010h
         DS    CL3                 BYTE 28-30 Blanks                    LB1010h
*
         PRINT NOGEN
*
         TITLE 'LISTDSJ - Working Storage Variables                   '
*     * /********************************************************/
*     * /* Working Storage  DSECT                               */
*     * /********************************************************/
WSDSECT  DSECT
WSAREA   EQU   *
*     *
*     * /********************************************************/
*     * /* mySAVEARA                                            */
*     * /********************************************************/
SAVEAREA DS    18F                 my Savearea
*                      +00  A(savearea of current CSECT)
*                      +04  A(savearea of calling CSECT)
*                      +08  A(savearea of called  CSECT)
*                      +12  R14
*                      +16  R15
*                      +20  R0
*                      +24  R1
*                      +28  R2
*                      +32  R3
*                      +36  R4
*                      +40  R5
*                      +44  R6
*                      +48  R7
*                      +52  R8
*                      +56  R9
*                      +60  R10
*                      +64  R11
*                      +68  R12
*     *
*     * /********************************************************/
*     * /* IKJCT441 Parm Address List                           */
*     * /********************************************************/
IKJCT441 DS    F                   Entry point for IKJCT441             LB1010i
CT441LST DS    0F
CT441P1  DS    A                   Entry Code
CT441P2  DS    A                   VAR NAME POINTER
CT441P3  DS    A                   VAR NAME LENGTH
CT441P4  DS    A                   VAR VALUE POINTER
CT441P5  DS    A                   VAR VALUE LENGTH
CT441P6  DS    A                   Token
*     *
*     * /********************************************************/
*     * /* IKJCT441 Parm Variables                              */
*     * /********************************************************/
NAMP1    DS    A                   VAR NAME POINTER
NAML1    DS    A                   VAR NAME LENGTH
VALP1    DS    A                   VAR VALUE POINTER
VALL1    DS    A                   VAR VALUE LENGTH
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* ISPF     Parm Variables                              */      LB1010h
*     * /********************************************************/      LB1010h
ISPLINK  DS    F                   Entry point for ISPLINK              LB1010i
ISPFP1   DS    A                   ISPLINK Parm 1                       LB1010h
ISPFP2   DS    A                   ISPLINK Parm 2                       LB1010h
ISPFP3   DS    A                   ISPLINK Parm 3                       LB1010h
ISPFP4   DS    A                   ISPLINK Parm 4                       LB1010h
ISPFP5   DS    A                   ISPLINK Parm 5                       LB1010h
*
         EJECT
*     * /********************************************************/
*     * /* Symbolic Variable Data Area                          */
*     * /********************************************************/
DDSNL    DS    0F,H                Dataset Name Length (must be here    LB1010e
*                                  before Dataset Name DDSN)            LB1010e
DSYMS    EQU   *
*
DDSN     DS    CL44                Dataset Name
DJMSG    DS    CL100               MY MESSAGE VARIABLE
DRSNCODE DS    CL2                 Reason Code
DMSGL1   DS    CL40                Message Level 1
DMSGL2   DS    CL40                Message Level 2
DVOLSER  DS    CL6                 Volume Serial Number
DLRECL   DS    CL5                 LRECL
DBLKSZ   DS    CL5                 BLKSIZE
DDSORG   DS    CL3                 Dataset Organization
DRECFM   DS    CL5                 Record Format  e.g. FBA
DCREDT   DS    CL8                 CCYY/JJJ   Create Date
DCREDJ   DS    CL5                 YYJJJ      Create Date
DCREDC   DS    CL10                MM/DD/CCYY Create Date
DEXPDT   DS    CL8                 CCYY/JJJ   Expire Date
DEXPDJ   DS    CL5                 YYJJJ      Exprire Date
DEXPDC   DS    CL10                MM/DD/CCYY Exprire Date
DREFDT   DS    CL8                 CCYY/JJJ   Last Reference Date
DREFDJ   DS    CL5                 YYJJJ      Last Reference Date
DREFDC   DS    CL10                MM/DD/CCYY Last Reference Date
DKEYL    DS    CL3                 Keylength
DRKP     DS    CL5                 Relative Key Position
DPASSWD  DS    CL5                 Password Protection
DRACF    DS    CL4                 RACF
DDEVICE  DS    CL15                Device
DSSPC    DS    CL8                 Secondary Space Request Type         LB1010c
DSSPCNM  DS    CL7                 Secondary Space Amount
DUNIT    DS    CL7                 Device Unit
DEXTS    DS    CL3                 Number of Extents
DNUMVOL  DS    CL3                 Number of Volumes
DTRKSAL  DS    CL5                 Total Tracks Allocated
DTRKSUS  DS    CL5                 Total Tracks Used
DTRKSUUS DS    CL5                 Total Tracks Unused
DTRKSCYL DS    CL5                 Tracks/Cylinder
DTRKLEN  DS    CL5                 Track Length
DBLKTRK  DS    CL5                 Blocks per Track
DCYL4VOL DS    CL5                 Cylinders for Volume
DUNITCAP DS    CL15                Capacity  for Volume
DDBLKS   DS    CL5                 PDS Directory Blocks
DDIRBKSU DS    CL5                 PDS Directory Blocks Used
DDIRBKSN DS    CL5                 PDS Directory Blocks Unused
DMEMBERS DS    CL5                 PDS Total Members
DALIASES DS    CL5                 PDS Alias Members
DDSCAT   DS    CL1                 Dataset catalog indication Y/N
DDSCATV  DS    CL6                 Dataset catalog volume
DPSPCAL  DS    CL5                 Primary Space allocation
DPSPCUS  DS    CL5                 Primary Space allocation used
DVOLLST  DS    CL35                Up to 5 volumes for multivol DSN
DVOLLST# EQU   L'DVOLLST/7         7 bytes for each volume
DALLOC   DS    CL5                 Total Allocation in space units      LB1010d
DUPDTD   DS    CL1                 Backup Indicator Y/N                 LB1040a
*
DSYMSL   EQU   *-DSYMS             Length of DSYMS area
*
         EJECT
*     * /********************************************************/
*     * /* Date Conversion Variables                            */
*     * /********************************************************/
DC#YR    DS    PL4                 CCYY Packed workarea
DC#HJJJ  DS    H                   JJJ  halfword workarea
DC#JJJ   DS    CL3                 JJJ   Julian Date 001-366
DC#YYJJJ DS    XL3                 YY0JJJ workarea binary               LB1010h
*                                                                       LB1001b
DC#MDCY  DS    CL11                                                     LB1001b
         ORG   DC#MDCY             Redefine 11 byte area                LB1001b
DC#MM    DS    CL3                 0MM   Month        01-12
DC#DD    DS    CL3                 0DD   Date         01-31
DC#CCYY  DS    CL5                 0CCYY Century and Year
*                                                                       LB1001b
DATESEP  DS    CL1                 Date Separator value                 LB1001a
DATEFMT  DS    CL1                 Date Format    value                 LB1001b
DATEWA   DS    CL15                Date Workarea                        LB1001b
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* ISPF Statistics Workareas                            */      LB1010h
*     * /********************************************************/      LB1010h
TBN      DS    CL8                 TBL Name (DIRLSTxx)                  LB1010h
IVLVL    DS    CL3                 ISPF Version      Level NN           LB1010h
IMLVL    DS    CL3                 ISPF Modification Level NN           LB1010h
ICREDT   DS    CL7                 ISPF Create Date YYYYJJJ             LB1010h
ICHGDT   DS    CL7                 ISPF Change Date YYYYJJJ             LB1010h
ICHGTM   DS    CL5                 ISPF Change Time HHMM                LB1010h
*                                                                       LB1010h
         #ISTATS                   ISPF Stat Table column fields        LB2000a
*     *
*     * /********************************************************/
*     * /* Misc Variables                                       */
*     * /********************************************************/
SAVER15  DS    F                   R15 Hold
SAVER5   DS    F                   R5  Hold                             LB1010h
SAVER3   DS    F                   R3  Hold                             LB1010h
SAVER6   DS    F                   R6  Hold                             LB1010h
SAVER8   DS    F                   R8  Hold                             LB1010h
SAVER15M DS    F                   R15 Hold for Message Processing      LB2000d
DW       DS    D                   Double Word area
FW       DS    F                   Full Word   area
RCEXIT   DS    F                   Exit RC
TRKSCYL  DS    F                   Tracks/Cylinder
TRKLEN   DS    F                   Track Length
TRKSTOTA DS    F                   Track Allocated TOTAL
TTRKUSED DS    F                   Track Used      TOTAL
CYL4VOL  DS    F                   Cylinders for Volume
CYLSTOTA DS    F                   CYLs  Allocated TOTAL
BLKSTOTA DS    F                   BLKs  Allocated TOTAL
BLKSPTRK DS    F                   BLKs per track
DMSGL1L  DS    F                   Length or MSGL1
DMSGL2L  DS    F                   Length or MSGL2
CD       DS    CL2                 Code for searching tables
RC       DS    CL2                 Return Code
PK3      DS    PL2                 Pack work area                       LB2000d
SNN      DS    PL2                 Short symbolic seq no
SNNC     DS    CL3                 Short symbolic name  Snn
SNNUPK   DS    CL3                 Short symbolic unpacked seq no
*
         EJECT
*     * /********************************************************/
*     * /* PARM input scan Variables                            */
*     * /********************************************************/
PDSNL    DS    0F,H                Dataset Name Length
PDSN     DS    CL44                Dataset Name from PARM input
PVOLSER  DS    CL6                 VOLSER from PARM input
PVOL     DS    C                   VOL   keyword switch
PDIR     DS    C                   DIR   keyword switch
PPNL     DS    C                   PNL   keyword switch
PABOUT   DS    C                   ABOUT keyword switch
PFILE    DS    C                   FILE  keyword switch                 LB1010g
PDL      DS    C                   DLx   keyword number                 LB1010h
PTSOB    DS    C                   TSOB  keyword switch                 LB1040c
*                                                                       LB1010e
IDPREFIX DS    C                   Prefix UserID Y-yes, N-no            LB1010e
MYIDL    DS    H                   Prefix UserID Length                 LB1010e
MYID     DS    CL8                 Prefix UserID                        LB1010e
*
         EJECT
*     * /********************************************************/
*     * /* CAMLIST Workareas                                    */
*     * /********************************************************/
OBTVOL   DS    CL6                 VOLSER

CAMDSN   DS    CL44                Dataset name

CAMLIST  DS    4F                  CAM Access List

LOCATEA  DS    0D                  Locate Area
         DS    265C                Declare space for catalog rcd
         DS    0D

         ORG   LOCATEA             REDEFINE catalog rcd
LOCA#VOL DS    H                   Number of volumes
LOCAVENT EQU   *                   Volume List Entry
LOCADEVT DS    0CL4                 Device type see UCBTYP field
         DS    CL1                   I/O supervisor flags
         DS    CL1                   Optional features
LOCADEVC DS    CL1                   Device class
LOCAUNTY DS    CL1                   Unit Type
LOCAVSER DS    CL6                  Device serial number
LOCAVS#  DS    CL2                  Device sequence number
LOCAVLLN EQU   *-LOCAVENT          Length of Volume List Entry

         ORG   LOCAVENT
LOCAVLST DS    20CL12              Volume list (20 max) 240 bytes
         DS    CL23
         DS    0D

OBTAINA  DS    0D
         DS    4CL140              Declare space for 4 DSCBs
         DS    0D

         ORG   OBTAINA-44          Start DSCBs after 44 byte DSN
         EJECT
         IECSDSL1 1                DSCB F1
         EJECT
         IECSDSL1 2                DSCB F2
         EJECT
         IECSDSL1 3                DSCB F3
         EJECT
         IECSDSL1 4                DSCB F4
         EJECT
*     * /********************************************************/
*     * /* Workareas for DAPL Build                             */
*     * /********************************************************/
MYCPPLP  DS    F                   CPPL Address
MYECB    DS    F                   Env Control Block
MYDAPL   DS    0F,(DAPLLEN)C       Dynamic Alloc Parm List
MYDAPB   DS    0D,(DAP08LEN)C      Dynamic Alloc Parm Block LEN=X'54'
*
*        MYDAPB is a variable length area used by DAIR
*  NOTE=>  ENSURE THE LENGTH is the largest of the DAnn DSECTS used!
*
         EJECT
*     * /********************************************************/
*     * /* PDS DIR Workareas                                    */
*     * /********************************************************/
BLOCK    DS    0D,256C             PDS Block area
DDNAME   DS    CL8                 DDN assigned by Dynamic Alloc

DIRSW    DS    C                   Read PDS Block or process switch
PODBLKS  DS    F                   # of dir blocks used
PODBLKSU DS    F                   # of dir blocks unused
POMEMS   DS    F                   PDS member count
POALIS   DS    F                   PDS alias  count
ALLOCPO  DS    CL1                 PDS Allocated Flag                   LB1010h

DIRDCBW  DS    0F,(DIRDCBL)X       PDS DIR DCB work area
DIRDECBW DS    0F,(DIRDECBL)X      PDS

PARMVL1  DS    F                   Null Parm
*                                                                       LB2000e
*     * /********************************************************/      LB2000e
*     * /* Parameter Address List for LDSJISP                   */      LB2000e
*     * /********************************************************/      LB2000e
LDSJISP  DS    F                   LDSJISP  Entry Address               LB2000e
         #IPAL                     ISPF Service PAL                     LB2000a
*                                                                       LB2000d
*     * /********************************************************/      LB2000d
*     * /* Parameter Address List for LDSJMSG                   */      LB2000d
*     * /********************************************************/      LB2000d
LDSJMSG  DS    F                   LDSJMSG  Entry Address               LB2000d
         #MPAL                     Message PAL                          LB2000a
*
         DS    0F,(40)X
WSAREAE  EQU   *                   Working Storage END
WSAREAL  EQU   WSAREAE-WSAREA      Working Storage Area Length
         EJECT
         END   LISTDSJ
@@
//LKED.SYSLMOD DD  DISP=SHR,
//         DSN=SYS2.CMDLIB                        <--TARGET
//*
//********************************************************************/
//* 04/10/2020 1.0.20   Larry Belmontes Jr.                          */
//*                     - Added ALIAS LISTDSI to LKED step           */
//********************************************************************/
//LKED.SYSIN DD *
 ALIAS LISTDSI
 NAME LISTDSJ(R)
/*
//*
//* -------------------------------------------------------*
//* *  Assemble Link-Edit LDSJMSG to SYS2.CMDLIB           *
//* -------------------------------------------------------*
//LDSJMSG EXEC   ASML,
//         PARM.LKED='MAP,LIST,LET,RENT,XREF,REUS,REFR'
//ASM.SYSIN DD DATA,DLM=@@
         TITLE 'LDSJMSG - Message Processor for LISTDSJ               '
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  ==================================================================
*
* LL       DDDDD     SSSS   TTTTTTTT  MM   MM   SSSS     GGG
* LL       DD  DD   SS  SS     TT     MMM MMM  SS  SS   GG  GG
* LL       DD   DD  SS         TT     MM M MM  SS      GG
* LL       DD   DD   SSSS      TT     MM M MM   SSSS   GG  GGG
* LL       DD   DD      SS     TT     MM   MM      SS  GG    G
* LL       DD  DD   SS  SS     TT     MM   MM  SS  SS   GG  GG
* LLLLLLL  DDDDD     SSSS      TT     MM   MM   SSSS     GGG
*
*  ==================================================================
*
*  Program: LDSJMSG
*
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/LISTDSJ-for-MVS-3-8J
*           Copyright (C) 2019-2021  Larry Belmontes, Jr.
*
*  Disclaimer: <DSCLAIMR>
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
         EJECT
*  Overview:
*  ================================================================
*
*     This program performs all message requests for LISTDSJ using
*  a parameter address list macro (#IMAL) which must be set before
*  invoking LDSJMSG.
*
*     #MPAL (portion of macro)
*     --------------------------------------------------------
*     PMSGPALS DS    0F
*     PMSGREQ  DS    CL4     Message Request Type
*     PMSGNBR  DS    F       Message Number
*     PMSGT    DS    F       Starting address to receive text
*     PMSGL    DS    F       Length of area to receive text
*     PMSGTXTL DS    F       Length of message text moved (OUTPUT)
*
*
*
*     Sample priming code
*     --------------------------------------------------------
*     MVC   PMSGREQ,=C'MSG '    Set MESSAGE request type
*     MVC   PMSGNBR,RCEXIT      Set message number
*     LA    R1,DJMSG+15         Set DJMSG starting address
*     ST    R1,PMSGT
*     LA    R1,L'DJMSG-15       Set DJMSG length
*     ST    R1,PMSGL
*     MVC   PMSGTXTL,=F'0'      Init final msg text length
*     LA    R1,PMSGPALS         R1 = Parm List
*     L     R15,LDSJMSG         Call LDSJMSG
*     BALR  R14,R15
*     ST    R15,SAVER15M        SAVE R15
*     LTR   R15,R15             R15=0?  Message Found
*     BZ    MSGPRCRX            YES, return
*     C     R15,=F'02'          R15=2?  Message Not Found
*     BE    MSGPRCRX            YES, return
*     .   *******               Unexpected return from LDSJMSG
*     .
*     .
*
*
*     Request Types (PMSGREQ)
*     --------------------------------------------------------
*      'MSG '    Message Code Request
*      'RSN '    Reason Code Request
*      'CD  '    Return Code Request
*
*
*
         EJECT
*  Programs Called:
*  ==================================================================
*
*    o  GETMAIN/FREEMAIN       Working Storage
*
*
         EJECT
*  Table 1 - Program Register Usage
*
*  +------+----------------------------------------------------------+
*  | REG  |                                                          |
*  |USAGE |  DESCRIPTION                                             |
*  +------+----------------------------------------------------------+
*  |  R13 |  Calling program registers (R14-R12) upon entry          |
*  |  R14 |  Calling program return address upon entry               |
*  |  R15 |  Called  program entry address  upon entry               |
*  |  R1  |  Called  program parms starting address                  |
*  +------+----------------------------------------------------------+
*  |  R0  |  Working Register                                        |
*  |  R1  |  Working Register                                        |
*  |  R2  |  Working Register                                        |
*  |  R3  |  Working Register                                        |
*  |  R4  |  Working Register                                        |
*  |  R5  |  Working Register                                        |
*  |  R6  |  Working Register                                        |
*  |  R7  |  Working Register                                        |
*  |  R8  |  Working Register                                        |
*  |  R9  |  Address of WORKAREA via GETMAIN/FREEMAIN                |
*  |  R10 |  Base Register 1                                         |
*  |  R11 |  Working Register                                        |
*  |  R12 |  Working Register                                        |
*  |  R13 |  Address of SAVEAREA                                     |
*  +------+----------------------------------------------------------+
*  |  R15 |  Return Code upon exit                                   |
*  +------+----------------------------------------------------------+
*
         EJECT
*  Table 2 - Return Codes
*
*  +------+----------------------------------------------------------+
*  |RETURN|                                                          |
*  | CODE |  DESCRIPTION                                             |
*  +------+----------------------------------------------------------+
*  |  00  |  Message found                                           |
*  +------+----------------------------------------------------------+
*  |  02  |  Message not found, but message text returned            |
*  +------+----------------------------------------------------------+
*  |  12  |  No PARMS                                                |
*  +------+----------------------------------------------------------+
*  |  16  |  Invalid Request Type                                    |
*  +------+----------------------------------------------------------+
*
         EJECT
*  Macros and SYSLIB Location:
*  ==================================================================
*
*  Macro     Description                         Library
*  --------  ----------------------------------  --------------------
*  YREGS     Register Equates                    SYS2.MACLIB
*  #MPAL     Message Parameter Address List      LISTDSJ maclib
*
*
*  References:
*  ==================================================================
*
*  - None
*
*
         EJECT
*  Change History: <CHGHIST>
*  ==================================================================
*
*  MM/DD/CCYY Version  Name / Description
*  ---------- -------  --------------------------------------------
*
*  04/02/2021 2.0.00   Larry Belmontes Jr.
*                      - Initial version released to MVS 3.8J
*                        hobbyist public domain
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
LDSJMSG  CSECT
         USING LDSJMSG,R10         my BASE REGISTER(S)
         PRINT NOGEN
*
*     * /********************************************************/
*     * /* Save regs and declare base registers R10             */
*     * /* including saving R1 (PARMS) in R2                    */
*     * /* - R1  myPARMS address on entry                       */
*     * /* - R15 myENTRY address on entry                       */
*     * /********************************************************/
         STM   R14,R12,12(R13)     Save calling program registers
         LR    R10,R15             Load base1 w R15
         LR    R15,R13             Save callers registers
         LR    R2,R1               Save PARM addr
*
*     * /********************************************************/
*     * /* Program Eyecatcher                                   */
*     * /********************************************************/
         B     OVRSTAMP            Branch past eye-catcher
MYSTAMP  EQU   *
PGMID    DC    CL8'LDSJMSG'        My Program STAMP
         DC    CL8'MVS3.8J '       OS
         DC    CL8'V2.0.00 '       .Version
         DC    CL8'04022021'       ..date  MMDDCCYY
         DC    CL1' '
PGMDS    DC    CL8'&SYSDATE'       Assemble Date
         DC    CL3' - '            .and
PGMTS    DC    CL8'&SYSTIME'       ..Time
         DC    C'Copyright (C) 2021'
         DC    C'Larry Belmontes, Jr. '
         DC    C'https://ShareABitofIT.net/LISTDSJ-in-MVS38J'
MYSTAMPL EQU   *-MYSTAMP
OVRSTAMP DS    0H
*
         EJECT
*     * /********************************************************/
*     * /* GETMAIN, working storage, using R9                   */
*     * /* - R0  Working register (Size of Workarea)            */
*     * /* - R1  Working register (GETMAIN address)             */
*     * /* - R9  Working Storage DSECT address  **              */
*     * /********************************************************/
         LA    R0,WSAREAL          R0 = size of Workarea DSECT
         GETMAIN   R,LV=(R0)       Get Storage, R1 = addr of storage
         LTR   R15,R15             Did we get the storage?
         BNZ   NOMAINS             NO, return with rc in R15
         LR    R9,R1               YES. R9 = Workarea DSECT Register
         USING WSDSECT,R9          Tell Assembler
*
*     * /********************************************************/
*     * /* Update Save Areas for caller and calling programs    */
*     * /* - R1  myPARMS address on entry  (restored)           */
*     * /********************************************************/
         LR    R15,R13             Save callers savearea addr
         LA    R13,SAVEAREA        Load Addr of my savearea
         ST    R15,4(,R13)         Chain callers to my savearea
         ST    R13,8(,R15)         Chain my savearea to callers
         LR    R1,R2               Restore R1 as PARM addr
*
         EJECT
*     * /********************************************************/
*     * /* Get PARAMETER information passed to program          */
*     * /* - R1 myPARMS address on entry                        */
*     * /********************************************************/
PARMS    EQU   *                   We have a parm...
         LTR   R1,R1               Do I have a PARM?
         BNZ   PARMIN              YES, check parms
         MVC   RCEXIT,=F'12'       NO, return with RC=12 (no parm)
         B     MYEXIT              Branch to MYEXIT
*
*     * /********************************************************/
*     * /* Process Message Request Type                         */
*     * /* - R1 Starting parms address                          */
*     * /* - R3 Message Table DSECT                             */
*     * /* - R6 Address of message text receiving area          */
*     * /* - R7 Length  of message text receiving area          */
*     * /********************************************************/
PARMIN   EQU   *                   NO,  must be PARM addr
         USING PMSGPAL,R1          Tell Assembler, R1=PARMS pointer
         USING TBLMSG,R3           Tell Assembler, R3=Msg tbl pointer
         MVC   RCEXIT,=F'0'        Initialize RCEXIT
         L     R6,PMSGT            R6=Address of PMSGT
         L     R7,PMSGL            R7=Length  of PMSGL
         CLC   =C'MSG ',PMSGREQ    Message Request?
         BE    REQMSG
         CLC   =C'RSN ',PMSGREQ    Reason Code Request?
         BE    REQRSN
         CLC   =C'CD  ',PMSGREQ    Return Code Request?
         BE    REQCD
         MVC   RCEXIT,=F'16'       Invalid Request Type
         B     MYEXIT              Branch to MYEXIT
*
         EJECT
*     * /********************************************************/
*     * /* Set message table parameters based on request type   */
*     * /* - R2 Number of message table entries                 */
*     * /* - R3 Address of message table                        */
*     * /********************************************************/
REQMSG   EQU   *
         LA    R2,MSGTENT#         R2=Number of MSGTBL entries
         LA    R3,MSGTBL           R3=Starting address of MSGTBL
         B     LOOK4MSG            Search for message
*
REQRSN   EQU   *
         LA    R2,RSNTENT#         R2=Number of RSNTBL entries
         LA    R3,RSNTBL           R3=Starting address of RSNTBL
         B     LOOK4MSG            Search for message
*
REQCD    EQU   *
         LA    R2,RCTENT#          R2=Number of RCTBL entries
         LA    R3,RCTBL            R3=Starting address of RCTBL
         B     LOOK4MSG            Search for message
*
         EJECT
*     * /********************************************************/
*     * /* Search for message, return message text              */
*     * /* - R5, R7, R8      Working Register                   */
*     * /********************************************************/
LOOK4MSG EQU   *
         CLC   TMSGNUM,PMSGNBR     Requested msg number?
         BE    MSG$TXT             Yes, move message text
         CLC   TMSGNUM,MSGTEOT     End of msg table?
         BE    MSGNOTFN            Yes, requested msg not found!
         LA    R3,TBLMSGL(R3)      Bump up to next entry
         BCT   R2,LOOK4MSG         Look again...
MSG$EOT  EQU   *                   Exhausted msg table! Msg not found
         LA    R3,MANUALTE         Reset R3- addrs of manual msg entry
         L     R2,MSGTEOT          End of Msg Table value (x'FFFFFFFF')
         ST    R2,0(R3)
         LA    R2,MSGNFND          Address of MSGNFND message
         ST    R2,4(R3)
         LA    R2,L'MSGNFND        Length  of MSGNFND message
         ST    R2,8(R3)
MSGNOTFN EQU   *                   Requested message not found
         MVC   RCEXIT,=F'2'
MSG$TXT  EQU   *                   Requested message found
         L     R8,TMSGTXTL         Length of TMSGTXT
         CR    R7,R8               Sufficient length to recv TMSGTXT?
         BL    MSGSMALR            No, use smaller length
         B     MSGOKLEN            YES, use msg txt length
MSGSMALR EQU   *
         LR    R8,R7               Reset TMSGTXTL via R8
MSGOKLEN EQU   *
         BCTR  R8,0                Adjust for EX MVC
         L     R5,TMSGTXT
         EX    R8,MVCMSG           EX MVC of MSG text
         LA    R8,1(R8)            Restore length of TMSGTXT
         ST    R8,PMSGTXTL         Set message length moved
         CLC   RCEXIT,=F'2'        Msg not found condition?
         BE    MSGXR02             Yes, add msg number to text
         B     MYEXIT              No, Done...
*
*     * /********************************************************/
*     * /* - Place msg number in msg not found text             */
*     * /********************************************************/
MSGXR02  EQU   *
         L     R8,PMSGNBR          Yes, indicate message number
         CVD   R8,DW               Convert to decimal
         UNPK  08(05,R6),DW+5(03)  Unpack and
         OI    08+05-01(R6),X'F0'  ... force and F zone
         B     MYEXIT              Done...
*
MVCMSG   MVC   0(0,R6),0(R5)       Move message to requestor area
*
         EJECT
*     * /********************************************************/
*     * /* - Restore savearea, FREEMAIN, and                    */
*     * /*   return to caller                                   */
*     * /********************************************************/
MYEXIT   EQU   *
*
*        STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
*        TPUT  DJMSG,L'DJMSG
*        LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
*
*     * /--------------------------------------------------------/
*     * /* R1 = my Working Storage Address from GETMAIN         */
*     * /********************************************************/
         LR    R1,R13              Working Storage Addr
*
*     * /--------------------------------------------------------/
*     * /* Get callers savearea into R13 and overlay my R15     */
*     * /* to be returned to caller                             */
*     * /--------------------------------------------------------/
         L     R13,SAVEAREA+4      Restore R13
         L     R5,RCEXIT           Set R15 to my exit value
*
*     * /--------------------------------------------------------/
*     * /* Working Storage FREEMAIN                             */
*     * /--------------------------------------------------------/
         LA    R0,WSAREAL          Size of WSAREA DSECT
         FREEMAIN  R,LV=(R0),A=(R1)    Free Storage
*
*     * /--------------------------------------------------------/
*     * /* Restore caller registers and return to caller        */
*     * /--------------------------------------------------------/
         LR    R15,R5              R15 = RC for exit
NOMAINS  EQU   *
         L     R14,12(R13)         Restore R14
         LM    R0,R12,20(R13)      Restore R0 thru R12
         BR    R14                 Return to caller
*
         TITLE 'LDSJMSG - Literal Pool                                '
         LTORG
*
         TITLE 'LDSJMSG - Message Address Table / Message Text        '
*     * /********************************************************/
*     * /* Message Address Table                                */
*     * /********************************************************/
MSGTBL   DS    0F
*              MSGNUM ,MSGTXT    ,MSGTXTL
*              -------,----------,-----------
         DC    A(4004),A(MSG4004),A(L'MSG4004)
         DC    A(4005),A(MSG4004),A(L'MSG4004)         Same as 4004
         DC    A(4008),A(MSG4009),A(L'MSG4009)         Same as 4009
         DC    A(4009),A(MSG4009),A(L'MSG4009)
         DC    A(4010),A(MSG4009),A(L'MSG4009)         Same as 4009
         DC    A(4095),A(MSG4095),A(L'MSG4095)
         DC    A(4097),A(MSG4097),A(L'MSG4097)
         DC    A(4098),A(MSG4098),A(L'MSG4098)
         DC    A(4099),A(MSG4099),A(L'MSG4099)
         DC    A(0015),A(MSGJ015),A(L'MSGJ015)
         DC    A(0017),A(MSGJ017),A(L'MSGJ017)
         DC    A(0018),A(MSGJ018),A(L'MSGJ018)
         DC    A(0020),A(MSGJ020),A(L'MSGJ020)
         DC    A(0021),A(MSGJ021),A(L'MSGJ021)
         DC    A(0022),A(MSGJ022),A(L'MSGJ022)
         DC    A(0025),A(MSGJ025),A(L'MSGJ025)
         DC    A(0027),A(MSGJ027),A(L'MSGJ027)
         DC    A(0030),A(MSGJ030),A(L'MSGJ030)
         DC    A(0031),A(MSGJ031),A(L'MSGJ031)
         DC    A(0032),A(MSGJ032),A(L'MSGJ032)
MSGTLAST DC    X'FFFFFFFF',A(MSGNFND),A(L'MSGNFND)     Last Entry
MSGTENTL EQU   *-MSGTLAST          Length of each entry
MSGTBLL  EQU   *-MSGTBL            Length of table
MSGTENT# EQU   MSGTBLL/MSGTENTL    Number of table entries
*
MSGTEOT  DC    X'FFFFFFFF'
         EJECT
*     * /********************************************************/
*     * /* Message Text                                         */
*     * /********************************************************/
MSGS     EQU   *
MSGNFND  DC    C'Message nnnnn not found'
MSG4004  DC    C'PARM Not supplied'
MSG4009  DC    C'CPPL No variables' ZERO
MSG4095  DC    C'Cannot link to LDSJISP'
MSG4097  DC    C'Cannot link to IKJDAIR'
MSG4098  DC    C'Cannot link to IKJCT441'
MSG4099  DC    C'Must run under TSO'
*
MSGJ015  DC    C'DIR option not valid, ignored'
MSGJ017  DC    C'RC=xxx Allocate PDS error'
MSGJ018  DC    C'RC=xxx Free PDS error'
MSGJ020  DC    C'RC=xxx DSCB-Fx CAMLST NAME error'
MSGJ021  DC    C'RC=xxx DSCB-Fx CAMLST SEARCH error'
MSGJ022  DC    C'RC=xxx DSCB-Fx CAMLST SEEK error'
MSGJ025  DC    C'1234567890 xxx not defined in table'
MSGJ027  DC    C'RC=xxx IKJCT441 error w zzzzzzzzzzzzzzzzzz'
MSGJ030  DC    C'RC=xxx Open PDS error'
MSGJ031  DC    C'RC=xxx Close PDS error'
MSGJ032  DC    C'RC=xxx ISPF 12345678 .                   .'            LB1010h
*
MSGSL    EQU   *-MSGS
*
         TITLE 'LDSJMSG - Reason Code Address Table / Reason Code Text'
*     * /********************************************************/
*     * /* Reason Code Address Table                            */
*     * /********************************************************/
RSNTBL   DS    0F
         DC    A(0000),A(RSMSG00),A(L'RSMSG00)
         DC    A(0002),A(RSMSG02),A(L'RSMSG02)
         DC    A(0003),A(RSMSG03),A(L'RSMSG03)
         DC    A(0005),A(RSMSG05),A(L'RSMSG05)
         DC    A(0006),A(RSMSG06),A(L'RSMSG06)
         DC    A(0007),A(RSMSG07),A(L'RSMSG07)
         DC    A(0012),A(RSMSG12),A(L'RSMSG12)
         DC    A(0013),A(RSMSG13),A(L'RSMSG13)
         DC    A(0014),A(RSMSG14),A(L'RSMSG14)
         DC    A(0017),A(RSMSG17),A(L'RSMSG17)
         DC    A(0019),A(RSMSG19),A(L'RSMSG19)
         DC    A(0021),A(RSMSG21),A(L'RSMSG21)
         DC    A(0022),A(RSMSG22),A(L'RSMSG22)
         DC    A(0023),A(RSMSG23),A(L'RSMSG23)
         DC    A(0024),A(RSMSG24),A(L'RSMSG24)
         DC    A(0026),A(RSMSG26),A(L'RSMSG26)
         DC    A(0028),A(RSMSG28),A(L'RSMSG28)
         DC    A(0029),A(RSMSG29),A(L'RSMSG29)
         DC    A(0090),A(RSMSG90),A(L'RSMSG90)
         DC    A(0099),A(RSMSG99),A(L'RSMSG99)
RSNTLAST DC    X'FFFFFFFF',A(MSGNFND),A(L'MSGNFND)  Must be last entry
RSNTENTL EQU   *-RSNTLAST          Length of each entry
RSNTBLL  EQU   *-RSNTBL            Length of table
RSNTENT# EQU   (RSNTBLL/RSNTENTL)  Number of table entries
         EJECT
*     * /********************************************************/
*     * /* Reason Code Text                                     */
*     * /********************************************************/
RSMSG00  DC    C'Normal completion'
RSMSG02  DC    C'Dynamic allocation error'
RSMSG03  DC    C'Data set type cannot be processed'                     LB1040g
RSMSG05  DC    C'Dataset not catalogued'
RSMSG06  DC    C'Error obtaining DSN'
RSMSG07  DC    C'Device type not found- DVCLST table'                   LB1040e
RSMSG12  DC    C'VSAM datasets not supported'                           LB1040d
RSMSG13  DC    C'Dataset could not open or close'
RSMSG14  DC    C'Device type not found in UCB table'                    LB1040e
RSMSG17  DC    C'System or User Abend error'                            LB1010h
RSMSG19  DC    C'Dataset resides on multiple volumes'
RSMSG21  DC    C'Catalog error trying to locate DSN'
RSMSG22  DC    C'Volume not mounted'
RSMSG23  DC    C'Permanent I/O error on volume'
RSMSG24  DC    C'Dataset name not found'
RSMSG26  DC    C'Dataset on Mass Storage device'
RSMSG28  DC    C'DD name must be 1-8 characters'                        LB1010g
RSMSG29  DC    C'Dataset or DD name required'                           LB1010g
RSMSG90  DC    C'DDN not allocated (not found)'                         LB1010g
RSMSG99  DC    C'ABOUT request, no DSN info retrieved'
*
         TITLE 'LDSJMSG - Return Code Address Table / Return Code Text'
*     * /********************************************************/
*     * /* Return Code Address Table                            */
*     * /********************************************************/
RCTBL    DS    0F
         DC    A(0000),A(RCMSG00),A(L'RCMSG00)
         DC    A(0004),A(RCMSG04),A(L'RCMSG04)
         DC    A(0016),A(RCMSG16),A(L'RCMSG16)
RCTLAST  DC    X'FFFFFFFF',A(MSGNFND),A(L'MSGNFND)  Must be last entry
RCTENTL  EQU   *-RCTLAST           Length of each entry
RCTBLL   EQU   *-RCTBL             Length of table
RCTENT#  EQU   RCTBLL/RCTENTL      Number of table entries
         EJECT
*     * /********************************************************/
*     * /* Reason Code Text                                     */
*     * /********************************************************/
RCMSG00  DC    C'Successful request'
RCMSG04  DC    C'Some dataset information is unavailable'
RCMSG16  DC    C'Error occured, variable data invalid'
*
         TITLE 'LDSJMSG - Equates                                     '
*     * /********************************************************/
*     * /* Register Equates                                     */
*     * /********************************************************/
         YREGS                     Register EQU as Rn or Rnn
*
*R0       EQU   0
*R1       EQU   0
*R2       EQU   0
*R3       EQU   0
*R4       EQU   0
*R5       EQU   0
*R6       EQU   0
*R7       EQU   0
*R8       EQU   0
*R9       EQU   0
*R10      EQU   0
*R11      EQU   0
*R12      EQU   0
*R13      EQU   0
*R14      EQU   0
*R15      EQU   0
*
         TITLE 'LDSJMSG - DSECTS                                      '
PMSGPAL  DSECT
         #MPAL                     Message PAL
*
         EJECT
*     * /********************************************************/
*     * /* Message Address Table                                */
*     * /********************************************************/
TBLMSG   DSECT
TMSGNUM  DS    A                   Message Number
TMSGTXT  DS    A                   Message Text Starting address
TMSGTXTL DS    A                   Message Text Length
TBLMSGL  EQU   *-TBLMSG            Length of Message Address Table
*
         TITLE 'LDSJMSG - Working Storage Variables                   '
*     * /********************************************************/
*     * /* Working Storage  DSECT                               */
*     * /********************************************************/
WSDSECT  DSECT
WSAREA   EQU   *
*
*     * /********************************************************/
*     * /* mySAVEARA                                            */
*     * /********************************************************/
SAVEAREA DS    18F                 my Savearea
*                      +00  A(savearea of current CSECT)
*                      +04  A(savearea of calling CSECT)
*                      +08  A(savearea of called  CSECT)
*                      +12  R14
*                      +16  R15
*                      +20  R0
*                      +24  R1
*                      +28  R2
*                      +32  R3
*                      +36  R4
*                      +40  R5
*                      +44  R6
*                      +48  R7
*                      +52  R8
*                      +56  R9
*                      +60  R10
*                      +64  R11
*                      +68  R12
*
*     * /********************************************************/
*     * /* Misc Variables                                       */
*     * /********************************************************/
SAVER15  DS    F                   R15 Hold
DW       DS    D                   Double Word area
FW       DS    F                   Full Word   area
RCEXIT   DS    F                   Exit RC
RC       DS    CL2                 Return Code
*
MANUALTE DS    3F                  Manual entry of MSGTBL row
*
WSAREAE  EQU   *                   Working Storage END
WSAREAL  EQU   WSAREAE-WSAREA      Working Storage Area Length
*
         END   LDSJMSG
@@
//LKED.SYSLMOD DD  DISP=SHR,DSN=SYS2.CMDLIB(LDSJMSG)
//* -------------------------------------------------------*
//* *  Assemble Link-Edit LDSJISP to SYS2.CMDLIB           *
//* -------------------------------------------------------*
//LDSJISP EXEC   ASML,
//         PARM.LKED='MAP,LIST,LET,RENT,XREF,REUS,REFR'
//ASM.SYSIN DD DATA,DLM=@@
         TITLE 'LDSJISP - ISPF    Processor for LISTDSJ               '
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  ==================================================================
*
* LL       DDDDD     SSSS   JJJJJJJJ  IIIIIIII   SSSS   PPPPP
* LL       DD  DD   SS  SS     JJ        II     SS  SS  PP   PP
* LL       DD   DD  SS         JJ        II     SS      PP   PP
* LL       DD   DD   SSSS      JJ        II      SSSS   PPPPP
* LL       DD   DD      SS     JJ        II         SS  PP
* LL       DD  DD   SS  SS  JJ JJ        II     SS  SS  PP
* LLLLLLL  DDDDD     SSSS    JJJ      IIIIIIII   SSSS   PP
*
*  ==================================================================
*
*  Program: LDSJISP
*
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/LISTDSJ-for-MVS-3-8J
*           Copyright (C) 2019-2021  Larry Belmontes, Jr.
*
*  Disclaimer: <DSCLAIMR>
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
         EJECT
*  Overview:
*  ================================================================
*
*     This program performs all ISPF requests for LISTDSJ using a
*  parameter address list macro (#IPAL) which must be set before
*  invoking LDSJISP.
*
*     #IPAL (portion of macro)
*     --------------------------------------------------------
*     PISPPALS DS    0F      PAL start address
*     PISPREQ  DS    CL4     ISP Srv Request Type
*     PISPEP   DS    F       ISPLINK Entry Point
*     PISPDCOL DS    F       ISP Table Columns
*     PISPTBN  DS    F       ISP Table Name
*     PISPDMSG DS    F       ISP DJMSG
*
*
*
*     Sample priming code
*     --------------------------------------------------------
*     MVC   PISPREQ,=C'1   '    Set ISP request type
*     LA    R1,PISPPALS         R1 = ISPF Parm List
*     L     R15,LDSJISP         R15= LSDJISP entry address
*     BALR  R14,R15             Call LSDJISP
*     LTR   R15,R15             R15=0?
*     BNZ   xxxxxxx             No, error process
*     .                         Yes, continue processing
*     .
*     .
*     .
*
*     Request Types (PISPREQ)
*     --------------------------------------------------------
*      '1   '  -  LISTDSJ ISPF VDEFINE
*      '2   '  -  LISTDSJ ISPF TBERASE
*      '3   '  -  LISTDSJ ISPF TBCREATE
*      '4   '  -  LISTDSJ ISPF TBADD
*      '5   '  -  LISTDSJ ISPF TBSAVE
*      '6   '  -  LISTDSJ ISPF TBCLOSE
*      '7   '  -  LISTDSJ ISPF VDELETE
*
*
*
         EJECT
*  Programs Called:
*  ==================================================================
*
*    o  GETMAIN/FREEMAIN       Working Storage
*    o  ISPLINK                ISP Services
*
*
         EJECT
*  Table 1 - Program Register Usage
*
*  +------+----------------------------------------------------------+
*  | REG  |                                                          |
*  |USAGE |  DESCRIPTION                                             |
*  +------+----------------------------------------------------------+
*  |  R13 |  Calling program registers (R14-R12) upon entry          |
*  |  R14 |  Calling program return address upon entry               |
*  |  R15 |  Called  program entry address  upon entry               |
*  |  R1  |  Called  program parms starting address                  |
*  +------+----------------------------------------------------------+
*  |  R0  |  Working Register                                        |
*  |  R1  |  Working Register                                        |
*  |  R2  |  Parm Pointer (save)                                     |
*  |  R3  |  Table name pointer                                      |
*  |  R4  |  ISPF Member stats pointer                               |
*  |  R5  |  Working Register                                        |
*  |  R6  |  Working Register                                        |
*  |  R7  |  Working Register                                        |
*  |  R8  |  DJMSG Pointer                                           |
*  |  R9  |  Address of WORKAREA via GETMAIN/FREEMAIN                |
*  |  R10 |  Base Register 1                                         |
*  |  R11 |  Working Register                                        |
*  |  R12 |  Working Register                                        |
*  |  R13 |  Address of SAVEAREA                                     |
*  +------+----------------------------------------------------------+
*  |  R15 |  Return Code upon exit                                   |
*  +------+----------------------------------------------------------+
*
         EJECT
*  Table 2 - Return Codes
*
*  +------+----------------------------------------------------------+
*  |RETURN|                                                          |
*  | CODE |  DESCRIPTION                                             |
*  +------+----------------------------------------------------------+
*  |  00  |  Successful Request                                      |
*  +------+----------------------------------------------------------+
*  | 1012 |  No Parm Received                                        |
*  +------+----------------------------------------------------------+
*  | 1016 |  Invalid Request Type                                    |
*  +------+----------------------------------------------------------+
*  | Other|  ISPLINK return code value                               |
*  +------+----------------------------------------------------------+
*
         EJECT
*  Macros and SYSLIB Location:
*  ==================================================================
*
*  Macro     Description                         Library
*  --------  ----------------------------------  --------------------
*  YREGS     Register Equates                    SYS2.MACLIB
*  ISPFSRV   ISPF Services and Keywords          LISTDSJ maclib
*  ISPFPL    ISPF Parm List for LBISPL           LISTDSJ maclib
*  LBISPL    CALL equivalent for ISPLINK         LISTSDJ maclib
*  #IPAL     ISP Srv Parameter Address List      LISTDSJ maclib
*  #ISTATS   ISPF Stats Table Columns            LISTDSJ maclib
*
*
*  References:
*  ==================================================================
*
*  - None
*
*
         EJECT
*  Change History: <CHGHIST>
*  ==================================================================
*
*  MM/DD/CCYY Version  Name / Description
*  ---------- -------  --------------------------------------------
*
*  04/02/2021 2.0.00   Larry Belmontes Jr.
*                      - Initial version released to MVS 3.8J
*                        hobbyist public domain
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
LDSJISP  CSECT
         USING LDSJISP,R10         my BASE REGISTER(S)
         PRINT NOGEN
*
*     * /********************************************************/
*     * /* Save regs and declare base registers R10             */
*     * /* including saving R1 (PARMS) in R2                    */
*     * /* - R1  myPARMS address on entry                       */
*     * /* - R15 myENTRY address on entry                       */
*     * /********************************************************/
         STM   R14,R12,12(R13)     Save calling program registers
         LR    R10,R15             Load base1 w R15
         LR    R15,R13             Save callers registers
         LR    R2,R1               Save PARM addr
*
*     * /********************************************************/
*     * /* Program Eyecatcher                                   */
*     * /********************************************************/
         B     OVRSTAMP            Branch past eye-catcher
MYSTAMP  EQU   *
PGMID    DC    CL8'LDSJISP'        My Program STAMP
         DC    CL8'MVS3.8J '       OS
         DC    CL8'V2.0.00 '       .Version
         DC    CL8'04022021'       ..date  MMDDCCYY
         DC    CL1' '
PGMDS    DC    CL8'&SYSDATE'       Assemble Date
         DC    CL3' - '            .and
PGMTS    DC    CL8'&SYSTIME'       ..Time
         DC    C'Copyright (C) 2021'
         DC    C'Larry Belmontes, Jr. '
         DC    C'https://ShareABitofIT.net/LISTDSJ-in-MVS38J'
MYSTAMPL EQU   *-MYSTAMP
OVRSTAMP DS    0H
*
         EJECT
*     * /********************************************************/
*     * /* GETMAIN, working storage, using R9                   */
*     * /* - R0  Working register                               */
*     * /* - R1  Working register (GETMAIN address)             */
*     * /* - R9  Working Storage DSECT address                  */
*     * /********************************************************/
         LA    R0,WSAREAL          R0 = size of Workarea DSECT
         GETMAIN   R,LV=(R0)       Get Storage, R1 = addr of storage
         LTR   R15,R15             Did we get the storage?
         BNZ   NOMAINS             NO, return with rc in R15
         LR    R9,R1               YES. R9 = Workarea DSECT Register
         USING WSDSECT,R9          Tell Assembler
*
*     * /********************************************************/
*     * /* Update Save Areas for caller and calling programs    */
*     * /* - R1  myPARMS address on entry  (restored)           */
*     * /********************************************************/
         LR    R15,R13             Save callers savearea addr
         LA    R13,SAVEAREA        Load Addr of my savearea
         ST    R15,4(,R13)         Chain callers to my savearea
         ST    R13,8(,R15)         Chain my savearea to callers
         LR    R1,R2               Restore R1 as PARM addr
*
         EJECT
*     * /********************************************************/
*     * /* Get PARAMETER information passed to program          */
*     * /* - R1 myPARMS address on entry                        */
*     * /* - R2 myPARMS address save                            */
*     * /* - R4 ISPF Member stats pointer                       */
*     * /* -                                                    */
*     * /* - R5         Working Register                        */
*     * /********************************************************/
PARMS    EQU   *                   We have a parm...
*        STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
*        TPUT  PGMID,8
*        LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
         LTR   R1,R1               Do I have a PARM?
         BNZ   PARMIN              YES, check parm length ????
         MVC   RCEXIT,=F'1012'     NO, return with RC=1012 (no parm)
         B     MYEXIT              Branch to MYEXIT
*
*     * /********************************************************/
*     * /* Initial Registers from Parm Address List             */
*     * /* - R2 Starting parms address                          */
*     * /* - R3 Table name pointer                              */
*     * /* - R4 ISPF Member stats pointer                       */
*     * /* - R8 DJMSG pointer                                   */
*     * /********************************************************/
PARMIN   EQU   *                   NO,  must be PARM addr
         MVC   RCEXIT,=F'0'        Initialize RCEXIT
         USING PISPPALS,R2         Tell Assembler, R2=PARMS pointer
         L     R3,PISPTBN          R3=Table name pointer
         USING TBNAL,R3            Tell Assembler
         L     R8,PISPDMSG         R8=DJMSG pointer
         L     R4,PISPDCOL         R4=ISPF Member stats pointer
         USING DIRAL,R4            Tell Assembler
*
         EJECT
*     * /********************************************************/
*     * /* Process ISPF Service Request Type from LISTDSJ       */
*     * /* - R7 Working Register                                */
*     * /********************************************************/
REQCHK1  EQU   *
         CLI   PISPREQ,C'1'        VARDEFS Request?
         BNE   REQCHK2             No, check 2
         BAL   R7,VARDEFS          Yes, VDEFINE variables
         B     REQCHKXT
REQCHK2  EQU   *
         CLI   PISPREQ,C'2'        ERASETB Request?
         BNE   REQCHK3             No, check 3
         BAL   R7,ERASETB          Yes, Erase existing ISPF table
         B     REQCHKXT
REQCHK3  EQU   *
         CLI   PISPREQ,C'3'        CREATETB Request?
         BNE   REQCHK4             No, check 4
         BAL   R7,CREATETB         Yes, Create ISPF table
         B     REQCHKXT
REQCHK4  EQU   *
         CLI   PISPREQ,C'4'        ADDTB Request?
         BNE   REQCHK5             No, check 5
         BAL   R7,ADDTB            Yes, Add ISPF Table row
         B     REQCHKXT
REQCHK5  EQU   *
         CLI   PISPREQ,C'5'        SAVETB Request?
         BNE   REQCHK6             No, check 6
         BAL   R7,SAVETB           Yes, Save ISPF table
         B     REQCHKXT
REQCHK6  EQU   *
         CLI   PISPREQ,C'6'        CLOSETB Request?
         BNE   REQCHK7             No, check 7
         BAL   R7,CLOSETB          Yes, Close ISPF table
         B     REQCHKXT
REQCHK7  EQU   *
         CLI   PISPREQ,C'7'        VARDELS Request?
         BNE   REQCHKNF            No, invalid request
         BAL   R7,VARDELS          Yes, Delete ISPF functional vars
         B     REQCHKXT
REQCHKNF EQU   *
         MVC   RCEXIT,=F'1016'     Invalid Request Type
         B     MYEXIT              Branch to MYEXIT
REQCHKXT EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* - Restore savearea, FREEMAIN, and                    */
*     * /*   return to caller                                   */
*     * /********************************************************/
MYEXIT   EQU   *
*     * /--------------------------------------------------------/
*     * /* R1 = my Working Storage Address from GETMAIN         */
*     * /********************************************************/
         LR    R1,R13              Working Storage Addr
*
*     * /--------------------------------------------------------/
*     * /* Get callers savearea into R13 and overlay my R15     */
*     * /* to be returned to caller                             */
*     * /--------------------------------------------------------/
         L     R13,SAVEAREA+4      Restore R13
         L     R5,RCEXIT           Set R15 to my exit value
*
*     * /--------------------------------------------------------/
*     * /* Working Storage FREEMAIN                             */
*     * /--------------------------------------------------------/
         LA    R0,WSAREAL          Size of WSAREA DSECT
         FREEMAIN  R,LV=(R0),A=(R1)    Free Storage
*
*     * /--------------------------------------------------------/
*     * /* Restore caller registers and return to caller        */
*     * /--------------------------------------------------------/
         LR    R15,R5              R15 = RC for exit
NOMAINS  EQU   *
         L     R14,12(R13)         Restore R14
         LM    R0,R12,20(R13)      Restore R0 thru R12
         BR    R14                 Return to caller
*
         TITLE 'LDSJISP - Subroutines                                 '
*     * /********************************************************/      LB1010h
*     * /* Subroutine - ISPF VDEFINE                   (R7)     */      LB1010h
*     * /* - R1, R14, R15 Working Registers                     */
*     * /********************************************************/      LB1010h
VARDEFS  EQU   *                                                        LB1010h
*     /* CALL  ISPLINK,(VDEFINE,IMEMBRN,IMEMBR,CHAR,IMEMBRL),VL */      LB1010h
*        STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
*        TPUT  VDEFINE,8
*        LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
VDEF1    EQU   *                                                        LB1010h
         MVC   7(L'VDEFINE,R8),VDEFINE
         PRINT GEN
         LBISPL PISPEP,(VDEFINE,IMEMBRN,IMEMBR,CHAR,IMEMBRL),VL
         PRINT NOGEN
         LTR   R15,R15                                                  LB1010h
         BZ    VDEF2                                                    LB1010h
         MVC   16(L'IMEMBRN,R8),IMEMBRN
         B     VARDEFSZ
VDEF2    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDEFINE,ITTRPN,ITTRP,CHAR,ITTRPL),VL    */      LB1010h
         LBISPL PISPEP,(VDEFINE,ITTRPN,ITTRP,CHAR,ITTRPL),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEF3                                                    LB1010h
         MVC   16(L'ITTRPN,R8),ITTRPN                                   LB1010h
         B     VARDEFSZ
VDEF3    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDEFINE,IVVMMN,IVVMM,CHAR,IVVMML),VL    */      LB1010h
         LBISPL PISPEP,(VDEFINE,IVVMMN,IVVMM,CHAR,IVVMML),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEF4                                                    LB1010h
         MVC   16(L'IVVMMN,R8),IVVMMN
         B     VARDEFSZ
VDEF4    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDEFINE,ICDTEN,ICDTE,CHAR,ICDTEL),VL    */      LB1010h
         LBISPL PISPEP,(VDEFINE,ICDTEN,ICDTE,CHAR,ICDTEL),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEF5                                                    LB1010h
         MVC   16(L'ICDTEN,R8),ICDTEN
         B     VARDEFSZ
VDEF5    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDEFINE,IMDTEN,IMDTE,CHAR,IMDTEL),VL    */      LB1010h
         LBISPL PISPEP,(VDEFINE,IMDTEN,IMDTE,CHAR,IMDTEL),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEF6                                                    LB1010h
         MVC   16(L'IMDTEN,R8),IMDTEN
         B     VARDEFSZ
VDEF6    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDEFINE,IRINTN,IRINT,CHAR,IRINTL),VL    */      LB1010h
         LBISPL PISPEP,(VDEFINE,IRINTN,IRINT,CHAR,IRINTL),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEF7                                                    LB1010h
         MVC   16(L'IRINTN,R8),IRINTN
         B     VARDEFSZ
VDEF7    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDEFINE,IRCURN,IRCUR,CHAR,IRCURL),VL    */      LB1010h
         LBISPL PISPEP,(VDEFINE,IRCURN,IRCUR,CHAR,IRCURL),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEF8                                                    LB1010h
         MVC   16(L'IRCURN,R8),IRCURN
         B     VARDEFSZ
VDEF8    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDEFINE,IRMODN,IRMOD,CHAR,IRMODL),VL    */      LB1010h
         LBISPL PISPEP,(VDEFINE,IRMODN,IRMOD,CHAR,IRMODL),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEF9                                                    LB1010h
         MVC   16(L'IRMODN,R8),IRMODN
         B     VARDEFSZ
VDEF9    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDEFINE,IUSRIDN,IUSRID,CHAR,IUSRIDL),VL */      LB1010h
         LBISPL PISPEP,(VDEFINE,IUSRIDN,IUSRID,CHAR,IUSRIDL),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEF10                                                   LB1010h
         MVC   16(L'IUSRIDN,R8),IUSRIDN
         B     VARDEFSZ
VDEF10   EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDEFINE,IMTIMN,IMTIM,CHAR,IMTIML),VL    */      LB1010h
         LBISPL PISPEP,(VDEFINE,IMTIMN,IMTIM,CHAR,IMTIML),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEF11                                                   LB1010h
         MVC   16(L'IMTIMN,R8),IMTIMN
         B     VARDEFSZ
VDEF11   EQU   *                                                        LB1010h
         B     VARDEFSX
VARDEFSZ EQU   *
         ST    R15,RCEXIT          R15 to caller
VARDEFSX EQU   *                                                        LB1010h
         BR    R7                  Return to caller                     LB1010h
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* Subroutine - ISPF TBERASE                   (R7)     */      LB1010h
*     * /* - R1, R14, R15 Working Registers                     */
*     * /********************************************************/      LB1010h
ERASETB  EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(TBERASE,TBN),VL                         */      LB1010h
ERASE01  EQU   *                                                        LB1010h
         MVC   7(L'TBERASE,R8),TBERASE
*        CALL  ISPLINK,(TBERASE,TBN),VL                                 LB1010h
         LBISPL PISPEP,(TBERASE,TBN),VL
         LTR   R15,R15                                                  LB1010h
         BZ    ERASETBX            Table erased...                      LB1010h
         C     R15,=F'8'                                                LB1010h
         BE    ERASETBX            Table not found...                   LB1010h
         MVC   16(L'TBN,R8),TBN
ERASETBZ EQU   *
         ST    R15,RCEXIT          R15 to caller
ERASETBX EQU   *                                                        LB1010h
         BR    R7                  Return to caller                     LB1010h
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* Subroutine - ISPF TBCREATE                  (R7)     */      LB1010h
*     * /* - R1, R14, R15 Working Registers                     */
*     * /********************************************************/      LB1010h
CREATETB EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(TBCREATE,TBN,KLIST,NLIST),VL            */      LB1010h
CRE8TB1  EQU   *                                                        LB1010h
         MVC   7(L'TBCREATE,R8),TBCREATE
*        CALL  ISPLINK,(TBCREATE,TBN,KLIST,NLIST,NOWRITE,SHARE),VL      LB1010h
*        CALL  ISPLINK,(TBCREATE,TBN,KLIST,NLIST),VL                    LB1010h
         LBISPL PISPEP,(TBCREATE,TBN,KLIST,NLIST),VL
         LTR   R15,R15                                                  LB1010h
         BZ    CREATETX                                                 LB1010h
         MVC   16(L'TBN,R8),TBN
CREATETZ EQU   *
         ST    R15,RCEXIT          R15 to caller
CREATETX EQU   *                                                        LB1010h
         BR    R7                  Return to caller                     LB1010h
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* Subroutine - ISPF TBADD                     (R7)     */      LB1010h
*     * /* - R1, R14, R15 Working Registers                     */
*     * /********************************************************/      LB1010h
ADDTB    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(TBADD,TBN),VL                           */      LB1010h
ADDTB1   EQU   *                                                        LB1010h
         MVC   7(L'TBADD,R8),TBADD
*        CALL  ISPLINK,(TBADD,TBN),VL                                   LB1010h
         LBISPL PISPEP,(TBADD,TBN),VL
         LTR   R15,R15                                                  LB1010h
         BZ    ADDTBX                                                   LB1010h
         MVC   16(L'TBN,R8),TBN
ADDTBZ   EQU   *
         ST    R15,RCEXIT          R15 to caller
ADDTBX   EQU   *                                                        LB1010h
         BR    R7                  Return to caller                     LB1010h
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* Subroutine - ISPF TBSAVE                    (R7)     */      LB1010h
*     * /* - R1, R14, R15 Working Registers                     */
*     * /********************************************************/      LB1010h
SAVETB   EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(TBSAVE,TBN),VL                          */      LB1010h
SAVETB1  EQU   *                                                        LB1010h
         MVC   7(L'TBSAVE,R8),TBSAVE
*        CALL  ISPLINK,(TBSAVE,TBN),VL                                  LB1010h
         LBISPL PISPEP,(TBSAVE,TBN),VL
         LTR   R15,R15                                                  LB1010h
         BZ    SAVETBX                                                  LB1010h
         MVC   16(L'TBN,R8),TBN
SAVETBZ  EQU   *
         ST    R15,RCEXIT          R15 to caller
SAVETBX  EQU   *                                                        LB1010h
         BR    R7                  Return to caller                     LB1010h
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* Subroutine - ISPF TBCLOSE                   (R7)     */      LB1010h
*     * /* - R1, R14, R15 Working Registers                     */
*     * /********************************************************/      LB1010h
CLOSETB  EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(TBCLOSE,TBN),VL                         */      LB1010h
CLOSETB1 EQU   *                                                        LB1010h
         MVC   7(L'TBCLOSE,R8),TBCLOSE
*        CALL  ISPLINK,(TBCLOSE,TBN),VL                                 LB1010h
         LBISPL PISPEP,(TBCLOSE,TBN),VL
         LTR   R15,R15                                                  LB1010h
         BZ    CLOSETBX                                                 LB1010h
         MVC   16(L'TBN,R8),TBN
CLOSETBZ EQU   *
         ST    R15,RCEXIT          R15 to caller
CLOSETBX EQU   *                                                        LB1010h
         BR    R7                  Return to caller                     LB1010h
*                                                                       LB1010h
         EJECT                                                          LB1010h
*     * /********************************************************/      LB1010h
*     * /* Subroutine - ISPF VDELETE                   (R7)     */      LB1010h
*     * /* - R1, R14, R15 Working Registers                     */
*     * /********************************************************/      LB1010h
VARDELS  EQU   *                                                        LB1010h
VDEL1    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDELETE,IMEMBRN),VL                     */      LB1010h
         MVC   7(L'VDELETE,R8),VDELETE
         LBISPL PISPEP,(VDELETE,IMEMBRN),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEL2                                                    LB1010h
         MVC   16(L'IMEMBRN,R8),IMEMBRN
         B     VARSDELZ
VDEL2    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDELETE,ITTRPN),VL                      */      LB1010h
         LBISPL PISPEP,(VDELETE,ITTRPN),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEL3                                                    LB1010h
         MVC   16(L'ITTRPN,R8),ITTRPN
         B     VARSDELZ
VDEL3    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDELETE,IVVMMN),VL                      */      LB1010h
         LBISPL PISPEP,(VDELETE,IVVMMN),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEL4                                                    LB1010h
         MVC   16(L'IVVMMN,R8),IVVMMN
         B     VARSDELZ
VDEL4    EQU   *                                                */      LB1010h
* * * /* CALL  ISPLINK,(VDELETE,ICDTEN),VL                              LB1010h
         LBISPL PISPEP,(VDELETE,ICDTEN),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEL5                                                    LB1010h
         MVC   16(L'ICDTEN,R8),ICDTEN
         B     VARSDELZ
VDEL5    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDELETE,IMDTEN),VL                      */      LB1010h
         LBISPL PISPEP,(VDELETE,IMDTEN),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEL6                                                    LB1010h
         MVC   16(L'IMDTEN,R8),IMDTEN
         B     VARSDELZ
VDEL6    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDELETE,IRINTN),VL                      */      LB1010h
         LBISPL PISPEP,(VDELETE,IRINTN),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEL7                                                    LB1010h
         MVC   16(L'IRINTN,R8),IRINTN
         B     VARSDELZ
VDEL7    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDELETE,IRCURN),VL                      */      LB1010h
         LBISPL PISPEP,(VDELETE,IRCURN),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEL8                                                    LB1010h
         MVC   16(L'IRCURN,R8),IRCURN
         B     VARSDELZ
VDEL8    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDELETE,IRMODN),VL                      */      LB1010h
         LBISPL PISPEP,(VDELETE,IRMODN),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEL9                                                    LB1010h
         MVC   16(L'IRMODN,R8),IRMODN
         B     VARSDELZ
VDEL9    EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDELETE,IUSRIDN),VL                     */      LB1010h
         LBISPL PISPEP,(VDELETE,IUSRIDN),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEL10                                                   LB1010h
         MVC   16(L'IUSRIDN,R8),IUSRIDN
         B     VARSDELZ
VDEL10   EQU   *                                                        LB1010h
* * * /* CALL  ISPLINK,(VDELETE,IMTIMN),VL                      */      LB1010h
         LBISPL PISPEP,(VDELETE,IMTIMN),VL
         LTR   R15,R15                                                  LB1010h
         BZ    VDEL11                                                   LB1010h
         MVC   16(L'IMTIMN,R8),IMTIMN
         B     VARSDELZ
VDEL11   EQU   *                                                        LB1010h
         B     VARSDELX
VARSDELZ EQU   *
         ST    R15,RCEXIT          R15 to caller
         BR    R7                  Return to caller
VARSDELX EQU   *                                                        LB1010h
         MVC   7(8,R8),=C'        '
         BR    R7                  Return to caller                     LB1010h
*
         TITLE 'LDSJISP - Literal Pool                                '
         LTORG
*
         TITLE 'LDSJISP - ISPF Constants and Variables                '
         ISPFSRV                   ISPF Service Constants
         EJECT
*     * /********************************************************/      LB1010h
*     * /* ISPF Table lists                                     */      LB1010h
*     * /********************************************************/      LB1010h
KLIST    DC    C'(IMEMBR)'                                              LB1010h
NLIST DC C'(ITTRP IVVMM ICDTE IMDTE IMTIM IRCUR IRINT IRMOD IUSRID)'    LB1010h
*
         EJECT
*     * /********************************************************/      LB1010h
*     * /* ISPF Table column names    DIRLSTn    8-bytes        */      LB1010h
*     * /********************************************************/      LB1010h
IMEMBRN  DC    C'IMEMBR  '                                              LB1010h
ITTRPN   DC    C'ITTRP   '                                              LB1010h
IVVMMN   DC    C'IVVMM   '                                              LB1010h
ICDTEN   DC    C'ICDTE   '                                              LB1010h
IMDTEN   DC    C'IMDTE   '                                              LB1010h
IMTIMN   DC    C'IMTIM   '                                              LB1010h
IRCURN   DC    C'IRCUR   '                                              LB1010h
IRINTN   DC    C'IRINT   '                                              LB1010h
IRMODN   DC    C'IRMOD   '                                              LB1010h
IUSRIDN  DC    C'IUSRID  '                                              LB1010h
*
*     * /********************************************************/      LB1010h
*     * /* ISPF Table column lengths  DIRLSTn                   */      LB1010h
*     * /********************************************************/      LB1010h
IMEMBRL  DC    A(L'IMEMBR)                                              LB1010h
ITTRPL   DC    A(L'ITTRP)                                               LB1010h
IVVMML   DC    A(L'IVVMM)                                               LB1010h
ICDTEL   DC    A(L'ICDTE)                                               LB1010h
IMDTEL   DC    A(L'IMDTE)                                               LB1010h
IMTIML   DC    A(L'IMTIM)                                               LB1010h
IRCURL   DC    A(L'IRCUR)                                               LB1010h
IRINTL   DC    A(L'IRINT)                                               LB1010h
IRMODL   DC    A(L'IRMOD)                                               LB1010h
IUSRIDL  DC    A(L'IUSRID)                                              LB1010h
*
         TITLE 'LDSJISP - Equates                                     '
*     * /********************************************************/
*     * /* Register Equates                                     */
*     * /********************************************************/
         YREGS                     Register EQU as Rn or Rnn
*
*R0       EQU   0
*R1       EQU   0
*R2       EQU   0
*R3       EQU   0
*R4       EQU   0
*R5       EQU   0
*R6       EQU   0
*R7       EQU   0
*R8       EQU   0
*R9       EQU   0
*R10      EQU   0
*R11      EQU   0
*R12      EQU   0
*R13      EQU   0
*R14      EQU   0
*R15      EQU   0
*
         TITLE 'LDSJISP - DSECTS                                      '
PARMAL   DSECT
         #IPAL                     ISPF Services Parm Addr List
         EJECT
*     * /********************************************************/
*     * /* ISPF Directory Table Name                            */
*     * /********************************************************/
TBNAL    DSECT
TBN      DS    CL8                 ISPF Directory Table Name
         EJECT
*     * /********************************************************/
*     * /* ISPF Statistics Column Data Areas                    */
*     * /********************************************************/
DIRAL    DSECT
         #ISTATS                   ISPF Directory List Table
*
         TITLE 'LDSJISP - Working Storage Variables                   '
*     * /********************************************************/
*     * /* Working Storage  DSECT                               */
*     * /********************************************************/
WSDSECT  DSECT
WSAREA   EQU   *
*
*     * /********************************************************/
*     * /* mySAVEARA                                            */
*     * /********************************************************/
SAVEAREA DS    18F                 my Savearea
*                      +00  A(savearea of current CSECT)
*                      +04  A(savearea of calling CSECT)
*                      +08  A(savearea of called  CSECT)
*                      +12  R14
*                      +16  R15
*                      +20  R0
*                      +24  R1
*                      +28  R2
*                      +32  R3
*                      +36  R4
*                      +40  R5
*                      +44  R6
*                      +48  R7
*                      +52  R8
*                      +56  R9
*                      +60  R10
*                      +64  R11
*                      +68  R12
*
*     * /********************************************************/
*     * /* Misc Variables                                       */
*     * /********************************************************/
SAVER15  DS    F                   R15 Hold
DW       DS    D                   Double Word area
FW       DS    F                   Full Word   area
RCEXIT   DS    F                   Exit RC
RC       DS    CL2                 Return Code
*
         EJECT
         ISPFPL                    ISPF Parm List for LBISPL macro
         EJECT
*
WSAREAE  EQU   *                   Working Storage END
WSAREAL  EQU   WSAREAE-WSAREA      Working Storage Area Length
*
         END   LDSJISP
@@
//LKED.SYSLMOD DD  DISP=SHR,DSN=SYS2.CMDLIB(LDSJISP)
//CLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.CLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CLISTDIJ
PROC 0                                /* DATASET INFORMATION        */

/********************************************************************/
/*                                                                  */
/*    CLIST: CLISTDIJ                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitOfIT.net/LISTDSJ-for-mvs-3-8j/          */
/*         Copyright (C) 2019-2021  Larry Belmontes, Jr.            */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 Panel driver to invoke LISTDSJ and display variable     */
/* names and values.                                                */
/*                                                                  */
/*                                                                  */
/* Disclaimer: <DSCLAIMR>                                           */
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
/* Change History: <CHGHIST>                                        */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 02/27/2021 1.0.40   Larry Belmontes Jr.                          */
/*                     - Add SYSUPDATED variable for display        */
/*                                                                  */
/* 04/10/2020 1.0.20   Larry Belmontes Jr.                          */
/*                     - Minor updates                              */
/*                                                                  */
/* 09/30/2019 1.0.10   Larry Belmontes Jr.                          */
/*                     - Add DISPLAY PANEL error condition          */
/*                     - Add SYSALLOC variable for display          */
/*                                                                  */
/* 04/29/2019 1.0.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/


  /******************************************************************/
  /* TSO CLIST Control Statement                                    */
  /******************************************************************/
  CONTROL MAIN

  /******************************************************************/
  /* SET VARIABLES - CLIST                                          */
  /******************************************************************/
  SET TPMNM = LISTDSJ                  /* TPM name                  */
  SET PANELID = PLISTDSJ               /* Panel name                */
  SET &PARMDSN  = &STR(                                            )
  SET &PARMIN   = &STR(                                            )

  /******************************************************************/
  /* PROCESSING SWITCHES                                            */
  /******************************************************************/
  /* Debug switch                     */
  /* D=debugON,                       */
  /* Other value = debugOFF           */
  SET &DEBUG    = 1                    /* Debug ON                  */
  SET &DEBUG    = 0                    /* Debug OFF                 */
  /* Panel Message switch             */
  /* requires message lib entry       */
  /* for LDSJ000Z                     */
  /* 1=YES                            */
  /* Other value = OFF                */
  SET &PMSG     = 1                    /* Panel Messages ON         */
  SET &PMSG     = 0                    /* Panel Messages OFF        */

  /******************************************************************/
  /* ISPF Control statement                                         */
  /******************************************************************/
  ISPEXEC CONTROL ERRORS RETURN

  /******************************************************************/
  /* First-time Enter DSN message                                   */
  /******************************************************************/
  IF &PMSG EQ 1 THEN -
    DO
      SET &ZERRSM  = &STR(Enter DSN)
      SET &ZERRLM  = &STR(Enter DSN and PARMS for &TPMNM CP)
      SET &ZERRMSG = &STR(LDSJ000Z)
    /*ISPEXEC SETMSG (&ZERRMSG)
    END

  /******************************************************************/
  /* DISPLAY PANEL                                                  */
  /******************************************************************/
  GETDATA:  +
  IF &PMSG EQ 1 THEN -
    ISPEXEC DISPLAY PANEL(&PANELID) MSG(&ZERRMSG)
  ELSE -
    ISPEXEC DISPLAY PANEL(&PANELID)
  SET RC = &LASTCC

  /******************************************************************/
  /* EXIT on PF3 or PF4                                             */
  /******************************************************************/
  IF &RC = 8 THEN GOTO DONE

  /******************************************************************/
  /* EXIT on DISPLAY PANEL ERROR                                    */
  /******************************************************************/
  IF &RC = 0 THEN
  ELSE -
   DO
     WRITE DISPLAY PANEL ERROR, RC=&RC
     GOTO DONE
   END

  /******************************************************************/
  /* GETDATA on PF1 HELP, Help screen displayed by ISPF 2.0         */
  /******************************************************************/
  IF &KEYPRESS EQ PF01 THEN -
      DO
        IF &PMSG EQ 1 THEN -
          DO
            SET ZERRSM = &STR(HELP PANEL PROCESSED)
            SET ZERRLM = &STR(PF1 Help panel displayed, enter new NAME.)
          /*ISPEXEC SETMSG MSG(&ZERRMSG)
          END
        GOTO GETDATA
      END

  /******************************************************************/
  /* Process request on ENTER, otherwise leave display content      */
  /* as-is and GETDATA                                              */
  /******************************************************************/
  IF &KEYPRESS EQ PF00 THEN
  ELSE -
    DO
      IF &PMSG EQ 1 THEN -
        DO
          SET ZERRSM = &STR(INVALID PF KEY)
          SET ZERRLM = &STR(Invalid PF key detected, no action taken.)
        /*ISPEXEC SETMSG MSG(&ZERRMSG)
        END
      GOTO GETDATA
    END

  /******************************************************************/
  /* Clear Panel values                                             */
  /******************************************************************/
  CONTINU: +
  SET S00=
  SET S01=&S00
  SET S02=&S00
  SET S03=&S00
  SET S04=&S00
  SET S05=&S00
  SET S06=&S00
  SET S07=&S00
  SET S08=&S00
  SET S09=&S00
  SET S10=&S00
  SET S11=&S00
  SET S12=&S00
  SET S13=&S00
  SET S14=&S00
  SET S15=&S00
  SET S16=&S00
  SET S17=&S00
  SET S18=&S00
  SET S19=&S00
  SET S20=&S00
  SET S21=&S00
  SET S22=&S00
  SET S23=&S00
  SET S24=&S00
  SET S25=&S00
  SET S26=&S00
  SET S27=&S00
  SET S28=&S00
  SET S29=&S00
  SET S30=&S00
  SET S31=&S00
  SET S32=&S00
  SET S33=&S00
  SET S34=&S00
  SET S35=&S00
  SET S36=&S00
  SET S37=&S00
  SET S38=&S00
  SET S39=&S00
  SET S40=&S00
  SET S41=&S00
  SET S42=&S00
  SET S43=&S00
  SET S44=&S00
  SET S45=&S00
  SET S46=&S00          /* V1.0.10 */
  SET S47=&S00          /* V1.0.40 */

  /******************************************************************/
  /* Build Command Line                                             */
  /******************************************************************/
  SET CMDLINE = &TPMNM
  IF &LENGTH(&PARMDSN) > 0 THEN -
    DO
      SET PARMS   = &STR(&SUBSTR(1:&LENGTH(&PARMDSN),&PARMDSN))
      SET CMDLINE = &STR(&CMDLINE &PARMS)
    END
  ELSE -
    SET PARMS   = &STR()

/*SET CMDLINE = &STR(&CMDLINE &PARMS)
  IF &LENGTH(&PARMIN) > 0 THEN -
    DO
      SET PARMS   = &STR(&SUBSTR(1:&LENGTH(&PARMIN),&PARMIN))
      SET CMDLINE = &STR(&CMDLINE &PARMS)
    END

  /******************************************************************/
  /* Set PNLSW if PNL parm is specified                             */
  /******************************************************************/
  SET &SYSDVAL = &STR(&PARMS)
  READDVAL PRM1 PRM2 PRM3 PRM4 PRM5 PRM6 PRM7 PRM8 PRM9
  IF &STR(&PRM1) EQ PNL OR  &STR(&PRM2) EQ PNL OR -
     &STR(&PRM3) EQ PNL OR  &STR(&PRM4) EQ PNL OR -
     &STR(&PRM5) EQ PNL OR  &STR(&PRM6) EQ PNL OR -
     &STR(&PRM7) EQ PNL OR  &STR(&PRM8) EQ PNL OR -
     &STR(&PRM9) EQ PNL THEN -
    SET PNLSW = 1
  ELSE -
    SET PNLSW = 0

  /******************************************************************/
  /* EXECUTE TPM                                                    */
  /******************************************************************/
  &CMDLINE
  SET RC = &LASTCC

  IF &DEBUG = 1 THEN +
    DO
      CLS
      WRITE CMDLINE='&CMDLINE'   RC=&RC
    END

  IF &RC = 0 OR &RC = 4 OR &RC = 8 OR &RC = 16 THEN -
    DO
      IF &PNLSW EQ 0 THEN -
        DO
          SET S00=&STR(&SYSDSNAME)
          SET S01=&STR(&SYSLISTDSJMSG)   /* V1.0.20 */
          SET S02=&STR(&SYSREASON)
          SET S03=&STR(&SYSMSGLVL1)
          SET S04=&STR(&SYSMSGLVL2)
          SET S05=&STR(&SYSCREATE)
          SET S06=&STR(&SYSEXDATE)
          SET S07=&STR(&SYSREFDATE)
          SET S08=&SYSDSORG
          SET S09=&SYSRECFM
          SET S10=&SYSLRECL
          SET S11=&SYSBLKSIZE
          SET S12=&SYSKEYLEN
          SET S13=&SYSKEYPOS
          SET S14=&SYSPASSWORD
          SET S15=&SYSRACFA
          SET S16=&SYSADIRBLK
          SET S17=&SYSUDIRBLK
          SET S18=&SYSNUDIRBLK
          SET S19=&SYSMEMBERS
          SET S20=&SYSMEMBERSALIAS
          SET S21=&STR(&SYSVOLUME)
          SET S22=&SYSUNIT
          SET S23=&SYSUNITS
          SET S24=&SYSTRKSUSED
          SET S25=&SYSEXTENTS
          SET S26=&SYSTRKSALLOC
          SET S27=&SYSSECONDS
          SET S28=&SYSTRKSUNUSED
          SET S29=&SYSCYLVOL
          SET S30=&SYSTRKSCYL
          SET S31=&STR(&SYSJCREATE)
          SET S32=&STR(&SYSJEXDATE)
          SET S33=&STR(&SYSJREFDATE)
          SET S34=&SYSTRKLEN
          SET S35=&STR(&SYSUNITCAP)
          SET S36=&SYSBLKSTRK
          SET S37=&STR(&SYSCCREATE)
          SET S38=&STR(&SYSCEXDATE)
          SET S39=&STR(&SYSCREFDATE)
          SET S40=&SYSNUMVOLS
          SET S41=&SYSDSCAT
          SET S42=&STR(&SYSDSCATV)
          SET S43=&SYSPRIMARY
          SET S44=&SYSUSED
          SET S45=&STR(&SYSVOLUMES)
          SET S46=&SYSALLOC       /* V1.0.10 */
          SET S47=&SYSUPDATED     /* V1.0.40 */
        END
      ELSE -
        SET S01=&STR(&S01)       /* V1.0.20 */
      IF &DEBUG EQ 1 THEN DO
        WRITE S00='&S00'
        WRITE S01='&S01'
        WRITE S02='&S02'
        WRITE S03='&S03'
        WRITE S04='&S04'
        WRITE S05='&S05'
        WRITE S06='&S06'
        WRITE S07='&S07'
        WRITE S08='&S08'
        WRITE S09='&S09'
        WRITE S10='&S10'
        WRITE S11='&S11'
        WRITE S12='&S12'
        WRITE S13='&S13'
        WRITE S14='&S14'
        WRITE S15='&S15'
        WRITE S16='&S16'
        WRITE S17='&S17'
        WRITE S18='&S18'
        WRITE S19='&S19'
        WRITE S20='&S20'
        WRITE S21='&S21'
        WRITE S22='&S22'
        WRITE S23='&S23'
        WRITE S24='&S24'
        WRITE S25='&S25'
        WRITE S26='&S26'
        WRITE S27='&S27'
        WRITE S28='&S28'
        WRITE S29='&S29'
        WRITE S30='&S30'
        WRITE S31='&S31'
        WRITE S32='&S32'
        WRITE S33='&S33'
        WRITE S34='&S34'
        WRITE S35='&S35'
        WRITE S36='&S36'
        WRITE S37='&S37'
        WRITE S38='&S38'
        WRITE S39='&S39'
        WRITE S40='&S40'
        WRITE S41='&S41'
        WRITE S42='&S42'
        WRITE S43='&S43'
        WRITE S44='&S44'
        WRITE S45='&S45'
        WRITE S46='&S46'          /* V1.0.10 */
        WRITE S47='&S47'          /* V1.0.40 */
      END
    END
  ELSE -
    DO
      WRITE LISTDSJ RC=&RC
      SET S00=
      SET S01=&S00
      SET S02=&S00
      SET S03=&S00
      SET S04=&S00
      SET S05=&S00
      SET S06=&S00
      SET S07=&S00
      SET S08=&S00
      SET S09=&S00
      SET S10=&S00
      SET S11=&S00
      SET S12=&S00
      SET S13=&S00
      SET S14=&S00
      SET S15=&S00
      SET S16=&S00
      SET S17=&S00
      SET S18=&S00
      SET S19=&S00
      SET S20=&S00
      SET S21=&S00
      SET S22=&S00
      SET S23=&S00
      SET S24=&S00
      SET S25=&S00
      SET S26=&S00
      SET S27=&S00
      SET S28=&S00
      SET S29=&S00
      SET S30=&S00
      SET S31=&S00
      SET S32=&S00
      SET S33=&S00
      SET S34=&S00
      SET S35=&S00
      SET S36=&S00
      SET S37=&S00
      SET S38=&S00
      SET S39=&S00
      SET S40=&S00
      SET S41=&S00
      SET S42=&S00
      SET S43=&S00
      SET S44=&S00
      SET S45=&S00
      SET S46=&S00           /* V1.0.10 */
      SET S47=&S00           /* V1.0.40 */
    END
    SET CD = &RC

  /******************************************************************/
  /* PANEL PROCESSING COMPLETE                                      */
  /******************************************************************/

  IF &PMSG EQ 1 THEN -
    DO
      SET ZERRSM = &STR(&TPMNM RC=&RC)
      SET ZERRLM = &STR(RC=&RC &CMDLINE)
    /*ISPEXEC SETMSG MSG(&ZERRMSG)
    END

  /******************************************************************/
  /* GET ANOTHER REQUEST FROM PANEL                                 */
  /******************************************************************/
  GOTO GETDATA

  /******************************************************************/
  /* DONE...                                                        */
  /******************************************************************/
  DONE: +
  IF &DEBUG = 1 THEN +
    WRITE BYE...
  EXIT CODE(0)


END   /* END PROC                     */

@@
//MLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.MLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=LDSJ00
/********************************************************************/
/*                                                                  */
/* MESSAGES: LDSJ00                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitOfIT.net/LISTDSJ-for-mvs-3-8j/          */
/*         Copyright (C) 2019-2021  Larry Belmontes, Jr.            */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 Generic message entry for PANEL PLISTDSJ.               */
/*                                                                  */
/*                                                                  */
/* Disclaimer: <DSCLAIMR>                                           */
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
/* Change History: <CHGHIST>                                        */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 04/10/2020 1.0.20   Larry Belmontes Jr.                          */
/*                     - Corrected long message end quote           */
/*                                                                  */
/* 04/29/2019 1.0.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
LDSJ000Z '&ZERRSM                 ' .ALARM=NO
'&ZERRMSG &ZERRLM                                                              '
@@
//PLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.PLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=PLISTDSJ
/********************************************************************/
/*                                                                  */
/*    PANEL: PLISTDSJ                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitOfIT.net/LISTDSJ-for-mvs-3-8j/          */
/*         Copyright (C) 2019-2021  Larry Belmontes, Jr.            */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x panel to invoke LISTSDJ and display results             */
/*                                                                  */
/*                                                                  */
/* Disclaimer: <DSCLAIMR>                                           */
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
/* Change History: <CHGHIST>                                        */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 04/02/2021 2.0.00   Larry Belmontes Jr.                          */
/*                     - Updated panel version number               */
/*                                                                  */
/* 02/27/2021 1.0.40   Larry Belmontes Jr.                          */
/*                     - Updated panel version number               */
/*                     - Added field, CHGD  &S47(&SYSUPDATED)       */
/*                     - Moved ending attribute byte to maximize    */
/*                       display of S01, S03, S04                   */
/*                                                                  */
/* 04/10/2020 1.0.20   Larry Belmontes Jr.                          */
/*                     - Updated panel version number               */
/*                                                                  */
/* 09/30/2019 1.0.10   Larry Belmontes Jr.                          */
/*                     - Modified version number on panel           */
/*                     - Deleted EXPAND symbols except for line 01  */
/*                     - TermSize using Z variables                 */
/*                     - Increase display size, TYPE &S23(&SYSUNITS)*/
/*                     - Added field, ALLOC &S46(&SYSALLOC)         */
/*                                                                  */
/* 05/18/2019 1.0.01   Larry Belmontes Jr.                          */
/*                     - Added version number to panel              */
/*                                                                  */
/* 04/29/2019 1.0.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
% TYPE(TEXT) INTENS(HIGH)
+ TYPE(TEXT) INTENS(LOW)
_ TYPE(INPUT) INTENS(HIGH) CAPS(ON) JUST(LEFT)
[   TYPE(TEXT)   INTENS(HIGH)                      COLOR(TURQ)
]   TYPE(TEXT)   INTENS(LOW)                       COLOR(PINK)
{   TYPE(TEXT)   INTENS(HIGH)                      COLOR(GREEN)
}   TYPE(TEXT)   INTENS(HIGH)                      COLOR(YELLOW)
$   TYPE(TEXT)   INTENS(LOW)       HILITE(REVERSE) COLOR(BLUE)
? TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%--------------------------}LISTDSJ for MVS38J%---------/-/-----------}v2.0.00%-
%COMMAND ===>_ZCMD                                   +                         +
+                                                       +  %PANEL    - &ZPANELID
%NAME%==>_PARMDSN                                       +  %USERID   - &ZUSER  +
%PARMS%=>_PARMIN                                        +  %TIME     - &ZTIME  +
+                                                          %DATE     - &ZDATE  +
%MSG:]&S01                                                +         %- &ZJDATE +
%Return CD:+&CD  +&S03                                    +%SYSTEM   -}&ZSYSID +
%SYSREASON:+&S02 +&S04                                    +%TERMINAL - &ZTERM  +
%                                                     +    %TermSize?Z   ?Z   ++
{&S00                                        +        +    %APPLID   -[&ZAPPLID+
%DSORG RECFM LRECL BLKSZ KEYL  RKP   PASSWORD RACF CHGD                        +
{&S08 +&S09 +&S10 +&S11 +&S12 +&S13 +&S14    +&S15   +&S47                     +
%CREDT+&S37      % EXPDT+&S38      % REFDT+&S39      +   $CATL:]&S41   +       +
% +&S31 +&S05    +   &S32 +&S06    +  +&S33 +&S07    +   $Cvol:]&S42   +       +
]&S45                                                 ++ $VOLS:]&S40   +       +
%Allocation:     TYPE     PRI     USED     SEC     ALLOC                       +
+       {&S21   +&S23    +&S43 + +&S44 +  +&S27   +&S46   +                    +
%Tracks: TOT     USED     UNUSED  EXTENTS                                      +
+       +&S26 + +&S24 +  +&S28 + +&S25 +                                       +
%Device: CYLS    TRKSCYL  TRKLEN  BLKSTRK  CAPACITY                            +
{&S22   +&S29 + +&S30 +  +&S34 + +&S36  + +&S35           +                    +
%PO Dir: BLKS    USED     UNUSED  MEMBERS  ALIAS                               +
+       +&S16 + +&S17 +  +&S18 + +&S19 +  +&S20 +                              +
)INIT
.ZVARS = '(ZSCREEND ZSCREENW)'
.CURSOR = PARMDSN
.HELP = HLISTDSJ
)REINIT
REFRESH(*)          /* refresh all fields */
)PROC

&RESP0 = .RESP
&KEYPRESS = .PFKEY

/*IF (&KEYPRESS NE PF03)
/*  VER(&PARMDSN,NB)
/*  VER(&PARMDSN,DSNAME)


)END

./ ADD NAME=HLISTDSJ
/********************************************************************/
/*                                                                  */
/*    PANEL: HLISTDSJ                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitOfIT.net/LISTDSJ-for-mvs-3-8j/          */
/*         Copyright (C) 2019-2021  Larry Belmontes, Jr.            */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x HELP for panel PLISTDSJ                                 */
/*                                                                  */
/*                                                                  */
/* Disclaimer: <DSCLAIMR>                                           */
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
/* Change History: <CHGHIST>                                        */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 04/02/2021 2.0.00   Larry Belmontes Jr.                          */
/*                     - Updated panel version number               */
/*                                                                  */
/* 02/27/2021 1.0.40   Larry Belmontes Jr.                          */
/*                     - Updated panel version number               */
/*                                                                  */
/* 04/10/2020 1.0.20   Larry Belmontes Jr.                          */
/*                     - Updated panel version number               */
/*                                                                  */
/* 09/30/2019 1.0.10   Larry Belmontes Jr.                          */
/*                     - Added help panel info for NAME, DIRECTORY, */
/*                       VOLUME, DL0 and DL1 keywords               */
/*                     - Changed panel version number               */
/*                     - Reformatted help screen layout             */
/*                                                                  */
/* 05/18/2019 1.0.01   Larry Belmontes Jr.                          */
/*                     - Added help panel info for date seperator,  */
/*                       date formate layout, and version number    */
/*                                                                  */
/* 04/29/2019 1.0.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
!   TYPE(OUTPUT) INTENS(LOW) CAPS(OFF) JUST(ASIS)
$   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(RED)
[   TYPE(TEXT)   INTENS(HIGH)                      COLOR(TURQ)
]   TYPE(TEXT)   INTENS(LOW)                       COLOR(PINK)
{   TYPE(TEXT)   INTENS(HIGH)                      COLOR(GREEN)
}   TYPE(TEXT)   INTENS(HIGH)                      COLOR(YELLOW)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%--HELP--------------------}LISTDSJ for MVS38J%---------/-/-----------}v2.0.00%-
%COMMAND ===>_ZCMD                                     +   %PANEL    - &ZPANELID
+                                                          %USERID   - &ZUSER  +
+                                                                              +
+{NAME ==>}xxxxxxxx.xxxxxxxx.xxxxxxxx.xxxxxxxx.xxxxxxxx                        +
+ o+ required, DSN (dataset name) or DDNAME (see FILE keyword)                 +
+    DSN in quotes is fully qualified, otherwise, prefix DSN with%USERID       +
+{PARMS =>}VOLUME(nnnnnn) DIRECTORY PNL ABOUT DS(x) DF(n) FILE DLn             +
+ o%VOLUME+   optional, target VOLUME for dataset name.     [(abbreviated VOL) +
+ o%DIRECTORY+optional, retrieve PDS directory information. [(abbreviated DIR) +
+             Ignored when file is not Partitioned.                            +
+            %NOTE:+Last Referenced Date updated when file open.               +
+ o%PNL+      optional, CLIST symbolic names use short name format, Snn.       +
+ o%ABOUT+    optional, display LISTDSJ date time stamp on TSO session screen. +
+            %NOTE:+No dataset information is retrieved.                       +
+ o%DS+  +    optional, date seperator to be used for dates                    +
+            %D-Dash+P-Period S-Slash          +                               +
+ o%DF+  +    optional, date format layout for MDY type dates                  +
+             1-MM/DD/CCYY 2-DD/MM/CCYY%3-CCYY/MM/DD+4-CCYY/DD/MM              +
+ o%FILE +    optional, use{NAME+as a DDNAME for an allocated dataset          +
+ o%DL0 DL1 + optional, create PDS member list in ISPF table                   +
+            %DL0+table named DIRLST0,%DL1+table named DIRLST1                 +
+                                                                              +
+                                                                              +
)INIT
.CURSOR = ZCMD
)PROC

&RESP0 = .RESP
&KEYPRESS = .PFKEY

)END
@@
