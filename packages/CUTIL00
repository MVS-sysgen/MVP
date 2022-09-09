//CUTIL00 JOB (JOB),
//             'INSTALL CUTIL00',
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
./ ADD NAME=CCUTIL00
PROC 1 FUNCT VAR01() VAR02($N$) UQ1() CLS(Y) VERBOSE(Y)

/********************************************************************/
/*                                                                  */
/* CLIST: CCUTIL00                                                  */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitOfIT.net/CUTIL00-for-mvs-3-8j/          */
/*         Copyright (C) 2020-2021  Larry Belmontes, Jr.            */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ================================================================ */
/*                                                                  */
/* Test harness to invoke CUTIL00 from TSO.                         */
/*                                                                  */
/*                                                                  */
/* Command Syntax:                                                  */
/* ================================================================ */
/*                                                                  */
/* CCUTIL00 FUNCT VAR01('xx.') VAR02('yyy yy')                      */
/*                                                                  */
/*   FUNCT (Required) CUTIL00 Function                              */
/*   VAR01 (Required) Content for VAR01 in quotes                   */
/*   VAR02 (Optional) Content for VAR02 in quotes                   */
/*                    dependent on function                         */
/*   CLS   (Y/N)      Clear screen displaying results               */
/*                    default=Y                                     */
/*   VERBOSE (Y/N)    Display header and command line               */
/*                    default=Y                                     */
/*                                                                  */
/*   Example:                                                       */
/*   CCUTIL00 TRIM VAR01(' TRIM  MY SPACE   ') VAR02(X)             */
/*                                                                  */
/*                                                                  */
/*   Sample Output:                                                 */
/*   --- CCUTIL00: CUTIL00 TEST TOOL                                */
/*                                                                  */
/*   CUTIL00 TRIM  VAR01                                            */
/*                                                                  */
/*   --- CUTIL00 RESPONSE ..........................................*/
/*    RC    =0 SUCCESSFUL REQUEST                                   */
/*    VAR01 =' TRIM  MY SPACE   '                                   */
/*    VAR01L=00018                                                  */
/*    VAR02 ='TRIM  MY SPACE'                                       */
/*    VAR02L=00014                                                  */
/*                                                                  */
/*   --- CCUTIL00: CUTIL00 TEST TOOL   ...DONE!  TRIM               */
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
/*    In addition, the author requests submissions regarding any    */
/* code modifications / enhancements and/or associated comments     */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* benefiting the MVS 3.8J hobbyist public domain community.        */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 03/20/2020 1.0.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/

/*********************************************/
/* Clear Screen                              */
/*********************************************/
IF &CLS = Y THEN -
  CLS

SET TPM = &STR(CUTIL00)
SET MYPROC = &STR(CCUTIL00)
IF &VERBOSE = Y THEN DO
  WRITE --- &MYPROC: &TPM Test Tool
  WRITE
  END

/*********************************************/
/* Default CUTIL00 return values             */
/*********************************************/
SET ERRMSG =
SET VAR01L =
SET VAR02L =

/*********************************************/
/* Default CUTIL00 command string            */
/* -- ABOUT does not need var parms          */
/* -- VEXIST uses VAR01 content for VAR1     */
/*********************************************/
IF &STR(&FUNCT) = VEXIST THEN -
  SET CMDLINE= &STR(&TPM &FUNCT &VAR01)
ELSE -
  IF &STR(&FUNCT) = ABOUT THEN -
    SET CMDLINE= &STR(&TPM &FUNCT)
  ELSE -
    SET CMDLINE= &STR(&TPM &FUNCT VAR01)

/*********************************************/
/* Build CUTIL00 command string              */
/* based on VAR02 options                    */
/*********************************************/
IF &STR(&VAR02) = $N$ THEN DO
  SET VAR02  =
  END
ELSE -
  IF &STR(&VAR02) = $NOVAR2 THEN DO
    SET VAR02  =
    SET CMDLINE= &STR(&CMDLINE $NOVAR2)
    END
  ELSE DO
    SET CMDLINE= &STR(&CMDLINE VAR02)
    END

/*********************************************/
/* Execute CUTIL00 and display results       */
/*********************************************/
&CMDLINE
SET RC = &LASTCC
IF &VERBOSE = Y THEN DO
  WRITE &CMDLINE
  WRITE
  END
WRITE --- &TPM Response ..........................................
WRITE  RC    =&RC &ERRMSG
WRITE  VAR01 ='&VAR01'
WRITE  VAR01L=&VAR01L
WRITE  VAR02 ='&VAR02'
WRITE  VAR02L=&VAR02L
IF &STR(&FUNCT) = UNSTR THEN DO
  WRITE  - &FUNCT VARIABLE(S) CREATED: &VAR010
  SET CTR = 0
  DO WHILE &CTR <= &VAR010
    SET EVALLN = &STR(&&VAR01&CTR)
    WRITE  VAR01&CTR = '&EVALLN'
    SET CTR = &CTR + 1
  END
END

WRITE --- &MYPROC: &TPM Test Tool  ... Done!  &FUNCT
WRITE


/*********************************************/
/* Exit point                                */
/*********************************************/
ENDIT: -
END

EXIT CODE(0)
./ ADD NAME=CCUTIL0V
PROC 0

CONTROL MAIN

/********************************************************************/
/*                                                                  */
/* CLIST: CCUTIL0V                                                  */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitOfIT.net/CUTIL00-for-mvs-3-8j/          */
/*         Copyright (C) 2020-2021  Larry Belmontes, Jr.            */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ================================================================ */
/*                                                                  */
/* Validation CLIST to execute each CUTIL00 function and print      */
/* results.  This CLIST should be submitted under TSO Batch.        */
/*                                                                  */
/* Contains samples from program documentation.                     */
/*                                                                  */
/* Command Syntax:                                                  */
/* ================================================================ */
/*                                                                  */
/* CCUTIL00                                                         */
/*                                                                  */
/*   This is a simple CLIST that invokes CUTIL00 multiple times     */
/*   using the following approach for each test:                    */
/*         1 - SET variables to be used by CUTIL00 function         */
/*         2 - Invoke CUTIL00                                       */
/*         3 - Print results from CUTIL00                           */
/*                                                                  */
/*   Example:                                                       */
/*   CCUTIL00                                                       */
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
/*    In addition, the author requests submissions regarding any    */
/* code modifications / enhancements and/or associated comments     */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* benefiting the MVS 3.8J hobbyist public domain community.        */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/05/2021 1.1.05   Larry Belmontes Jr.                          */  LB1105
/*                     - Add tests for LEN, SLEN, OVERLAY,          */  LB1105
/*                       UTDSN, TRUNC                               */  LB1105
/*                                                                  */  LB1105
/* 04/10/2021 1.1.00   Larry Belmontes Jr.                          */  LB1100
/*                     - Add tests for PUT1V, GET1V, MCAL           */  LB1100
/*                                                                  */  LB1100
/* 03/20/2020 1.0.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/

SET TSTHDR = &STR(====> TEST )
SET I = 0

WRITE
WRITE ***** Testing CUTIL00 after installation   *** &SYSDATE &SYSTIME
WRITE *****        program documention samples   ***
WRITE

WRITE /\/\/\*00  A B O U T                         /\/\/\-----------
/********************************************************************/
/* *00 ABOUT from program documentation                             */
/********************************************************************/
SET MYCMD = &STR(CUTIL00 ABOUT  MYDATA  MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  =
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*01  L T R I M                         /\/\/\-----------
/********************************************************************/
/* *01 LTRIM from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 LTRIM  MYDATA  MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(   leading spaces)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*02  R T R I M                         /\/\/\-----------
/********************************************************************/
/* RTRIM  from program documentation                                */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 RTRIM  MYDATA  MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(trailing spaces  )
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*03  T R I M                           /\/\/\-----------
/********************************************************************/
/* *03 TRIM from program documentation                              */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 TRIM  MYDATA  MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR( trim   my  data )
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*04  T R I M #                         /\/\/\-----------
/********************************************************************/
/* *04 TRIM$ from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 TRIM$ MYDATA  MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR( trim   my  data )
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*05  I N D E X                         /\/\/\-----------
/********************************************************************/
/* *05 INDEX from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 INDEX MYDATA  DLMTR)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(my, string, ok?   )
SET MYDATAL =
SET DLMTR = &STR(",")
SET DLMTRL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  DLMTR    ='&DLMTR'
WRITE  DLMTRL   =&DLMTRL
IF &RC EQ 3 THEN WRITE  ** PASSED PER RC **
IF &RC NE 3 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*06  I N D E X B                       /\/\/\-----------
/********************************************************************/
/* *06 INDEXB from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 INDEXB MYDATA  DLMTR)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(my, string, ok?   )
SET MYDATAL =
SET DLMTR = &STR(",")
SET DLMTRL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  DLMTR    ='&DLMTR'
WRITE  DLMTRL   =&DLMTRL
IF &RC EQ 11 THEN WRITE  ** PASSED PER RC **
IF &RC NE 11 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*07  I S N U M                         /\/\/\-----------
/********************************************************************/
/* *07 ISNUM from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 ISNUM  MYDATA)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(0123456789)
SET MYDATAL =
SET MYRESULT =             /* not used for this function */
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 4001 THEN WRITE  ** PASSED PER RC **
IF &RC NE 4001 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*08  I S A L P H A                     /\/\/\-----------
/********************************************************************/
/* *08 ISALPHA from program documentation                           */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 ISALPHA MYDATA)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(abCDEfghiJ)
SET MYDATAL =
SET MYRESULT =             /* not used for this function */
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 4001 THEN WRITE  ** PASSED PER RC **
IF &RC NE 4001 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*09  I S L O W E R                     /\/\/\-----------
/********************************************************************/
/* *09 ISLOWER from program documentation                           */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 ISLOWER MYDATA          )
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(abcdefghij)
SET MYDATAL =
SET MYRESULT =             /* not used for this function */
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 4000 THEN WRITE  ** PASSED PER RC **
IF &RC NE 4000 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*10  I S U P P E R                     /\/\/\-----------
/********************************************************************/
/* *10 ISUPPER from program documentation                           */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 ISUPPER MYDATA          )
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(ABCDEFGHIJ)
SET MYDATAL =
SET MYRESULT =             /* not used for this function */
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 4001 THEN WRITE  ** PASSED PER RC **
IF &RC NE 4001 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*11  I S B L A N K                     /\/\/\-----------
/********************************************************************/
/* *11 ISBLANK from program documentation                           */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 ISBLANK MYDATA)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(         )
SET MYDATAL =
SET MYRESULT =             /* not used for this function */
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 4001 THEN WRITE  ** PASSED PER RC **
IF &RC NE 4001 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*12  I S D S N                         /\/\/\-----------
/********************************************************************/
/* *12 ISDSN from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 ISDSN   MYDATA )
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(A.A1234567)
SET MYDATAL =
SET MYRESULT =             /* not used for this function */
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 4001 THEN WRITE  ** PASSED PER RC **
IF &RC NE 4001 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*13  E C H O                           /\/\/\-----------
/********************************************************************/
/* *13 ECHO from program documentation                              */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 ECHO    MYDATA)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(my data to display.)
SET MYDATAL =
SET MYRESULT =             /* not used for this function */
SET MYRESULTL =
SET ERRMSG =
WRITE  CUTIL00 ECHO of MYDATA follows...
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*14  E C H O Q                         /\/\/\-----------
/********************************************************************/
/* *14 ECHOQ from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 ECHOQ   MYDATA)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(my data to display.)
SET MYDATAL =
SET MYRESULT =             /* not used for this function */
SET MYRESULTL =
SET ERRMSG =
WRITE  CUTIL00 ECHOQ of MYDATA follows...
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*15  R E V R S                         /\/\/\-----------
/********************************************************************/
/* *15 REVRS from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 REVRS   MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(ABCDEFGHIJKLM96%QRS)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*16  U P P E R C                       /\/\/\-----------
/********************************************************************/
/* *16 UPPERC from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 UPPERC  MYDATA)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(abCdeFgHIJklmnopqrS)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE  CUTIL00 ECHOQ of MYDATA follows...
CUTIL00 ECHOQ  MYDATA
WRITE
WRITE /\/\/\*17  L O W E R C                       /\/\/\-----------
/********************************************************************/
/* *17 LOWERC from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 LOWERC  MYDATA  MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(abCdeFgHIJklmnopqrS)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
WRITE  CUTIL00 ECHOQ of MYDATA follows...
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
CUTIL00 ECHOQ   MYDATA
WRITE
WRITE /\/\/\*18  C O U N T                         /\/\/\-----------
/********************************************************************/
/* *18 COUNT from program documentati                               */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 COUNT   MYDATA  SRCHPRMS)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(My name is Dino, big Dino.)
SET MYDATAL =
SET SRCHPRMS = &STR("Dino")    /* Search Parms           */
SET SRCHPRMSL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  SRCHPRMS ='&SRCHPRMS'
WRITE  SRCHPRMSL=&SRCHPRMSL
IF &RC EQ 2 THEN WRITE  ** PASSED PER RC **
IF &RC NE 2 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*19  F I N D                           /\/\/\-----------
/********************************************************************/
/* *19 FIND from program documentation                              */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 FIND    MYDATA  SRCHPRMS)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(My name is Dino, big Dino.)
SET MYDATAL =
SET SRCHPRMS = &STR("Dino")    /* Search Parms           */
SET SRCHPRMSL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  SRCHPRMS ='&SRCHPRMS'
WRITE  SRCHPRMSL=&SRCHPRMSL
IF &RC EQ 12 THEN WRITE  ** PASSED PER RC **
IF &RC NE 12 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*20  F I N D L                         /\/\/\-----------
/********************************************************************/
/* *20 FINDL from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 FINDL   MYDATA  SRCHPRMS)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(My name is Dino, big Dino.)
SET MYDATAL =
SET SRCHPRMS = &STR("Dino")    /* Search Parms           */
SET SRCHPRMSL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  SRCHPRMS ='&SRCHPRMS'
WRITE  SRCHPRMSL=&SRCHPRMSL
IF &RC EQ 22 THEN WRITE  ** PASSED PER RC **
IF &RC NE 22 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*21  C E N T E R                       /\/\/\-----------
/********************************************************************/
/* *21 CENTER from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 CENTER  MYDATA  MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(Center this T E X T ...       )
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*22  L J U S T                         /\/\/\-----------
/********************************************************************/
/* *22 LJUST from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 LJUST   MYDATA  MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR( JUSTIFY this text    )
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*23  R J U S T                         /\/\/\-----------
/********************************************************************/
/* *23 RJUST from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 RJUST   MYDATA  MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR( JUSTIFY this text    )
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*24  Z F I L L                         /\/\/\-----------
/********************************************************************/
/* *24 ZFILL from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 ZFILL   MYDATA  MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(12   )
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*25  W O R D S                         /\/\/\-----------
/********************************************************************/
/* *25 WORDS from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 WORDS   MYDATA)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(  one two three33 four., five )
SET MYDATAL =
SET MYRESULT =             /* not used for this function */
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 5 THEN WRITE  ** PASSED PER RC **
IF &RC NE 5 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*26  G E N #                           /\/\/\-----------
/********************************************************************/
/* *26 GEN# from program documentation                              */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 GEN#    MYDATA)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = 0
SET MYDATAL =
SET MYRESULT =             /* not used for this function */
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*27  D D 2 D S N                       /\/\/\-----------
/********************************************************************/
/* *27 DD2DSN from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 DD2DSN  MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(SYSPROC)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*28  J O B I N F O                     /\/\/\-----------
/********************************************************************/
/* *28 JOBINFO from program documentation                           */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 JOBINFO MYDATA)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(123456789012345678901234)
SET MYDATAL =
SET MYRESULT =             /* not used for this function */
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*29  D A Y S M M                       /\/\/\-----------
/********************************************************************/
/* *29 DAYSMM from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 DAYSMM   MYDATA)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(022016)
SET MYDATAL =
SET MYRESULT =             /* not used for this function */
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 29 THEN WRITE  ** PASSED PER RC **
IF &RC NE 29 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*30  D A Y S Y Y                       /\/\/\-----------
/********************************************************************/
/* *30 DAYSYY from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 DAYSYY   MYDATA)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(2020)
SET MYDATAL =
SET MYRESULT =             /* not used for this function */
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 366 THEN WRITE  ** PASSED PER RC **
IF &RC NE 366 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*31  I S L E A P                       /\/\/\-----------
/********************************************************************/
/* *31 ISLEAP from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 ISLEAP   MYDATA)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(2020)
SET MYDATAL =
SET MYRESULT =             /* not used for this function */
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 4001 THEN WRITE  ** PASSED PER RC **
IF &RC NE 4001 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*32  C Y J - D 8                       /\/\/\-----------
/********************************************************************/
/* *32 CYJ-D8 from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 CYJ-D8   MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(2020061)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*33  C Y J - D A Y                     /\/\/\-----------
/********************************************************************/
/* *33 CYJ-DAY from program documentation                           */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 CYJ-DAY  MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(1981061)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
CUTIL00 ECHOQ   MYDATA
CUTIL00 ECHOQ   MYRESULT
WRITE
WRITE /\/\/\*34  C Y J - D O W                     /\/\/\-----------
/********************************************************************/
/* *34 CYJ-DOW from program documentation                           */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 CYJ-DOW  MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(1980061)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
CUTIL00 ECHOQ   MYDATA
CUTIL00 ECHOQ   MYRESULT
WRITE
WRITE /\/\/\*35  C Y J - M D C Y                   /\/\/\-----------
/********************************************************************/
/* *35 CYJ-MDCY from program documentation                          */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 CYJ-MDCY MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(2020061)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*36  J C Y - D 8                       /\/\/\-----------
/********************************************************************/
/* *36 JCY-D8 from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 JCY-D8   MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(1612017)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*37  J C Y - D A Y                     /\/\/\-----------
/********************************************************************/
/* *37 JCY-DAY from program documentation                           */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 JCY-DAY  MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(0611981)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
CUTIL00 ECHOQ   MYDATA
CUTIL00 ECHOQ   MYRESULT
WRITE
WRITE /\/\/\*38  J C Y - D O W                     /\/\/\-----------
/********************************************************************/
/* *38 JCY-DOW from program documentation                           */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 JCY-DOW  MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(0611981)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
CUTIL00 ECHOQ   MYDATA
CUTIL00 ECHOQ   MYRESULT
WRITE
WRITE /\/\/\*39  J C Y - M D C Y                   /\/\/\-----------
/********************************************************************/
/* *39 JCY-MDCY from program documentation                          */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 JCY-MDCY MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(1612017)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*40  M D C Y - D 8                     /\/\/\-----------
/********************************************************************/
/* *40 MDCY-D8 from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 MDCY-D8   MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(12101990)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*41  M D C Y - D A Y                   /\/\/\-----------
/********************************************************************/
/* *41 MDCY-DAY from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 MDCY-DAY MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(12101990)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
CUTIL00 ECHOQ   MYDATA
CUTIL00 ECHOQ   MYRESULT
WRITE
WRITE /\/\/\*42     M D C Y - D O W                   /\/\/\-----------
/********************************************************************/
/* *42 MDCY-DOW from program documentation                          */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 MDCY-DOW MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(12101990)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
CUTIL00 ECHOQ   MYDATA
CUTIL00 ECHOQ   MYRESULT
WRITE
WRITE /\/\/\*43  M D C Y - C Y J                   /\/\/\-----------
/********************************************************************/
/* *43 MDCY-CYJ from program documentation                          */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 MDCY-CYJ  MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(12101990)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*44  D M C Y - D 8                     /\/\/\-----------
/********************************************************************/
/* *44 DMCY-D8 from program documentation                           */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 DMCY-D8   MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(15071987)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*45  D M C Y - D A Y                   /\/\/\-----------
/********************************************************************/
/* *45 DMCY-DAY from program documentation                          */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 DMCY-DAY MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(15071987)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
CUTIL00 ECHOQ   MYDATA
CUTIL00 ECHOQ   MYRESULT
WRITE
WRITE /\/\/\*46  D M C Y - D O W                   /\/\/\-----------
/********************************************************************/
/* *46 DMCY-DOW from program documentation                          */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 DMCY-DOW MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(15071987)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
CUTIL00 ECHOQ   MYDATA
CUTIL00 ECHOQ   MYRESULT
WRITE
WRITE /\/\/\*47  D M C Y - C Y J                   /\/\/\-----------
/********************************************************************/
/* *47 DMCY-CYJ from program documentation                          */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 DMCY-CYJ  MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(15071987)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*48  C Y M D - D 8                     /\/\/\-----------
/********************************************************************/
/* *48 CYMD-D8 from program documentation                           */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 CYMD-D8   MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(19951231)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*49  C Y M D - D A Y                   /\/\/\-----------
/********************************************************************/
/* *49 CYMD-DAY from program documentation                          */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 CYMD-DAY MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(19951231)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
CUTIL00 ECHOQ   MYDATA
CUTIL00 ECHOQ   MYRESULT
WRITE
WRITE /\/\/\*50  C Y M D - D O W                   /\/\/\-----------
/********************************************************************/
/* *50 CYMD-DOW from program documentation                          */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 CYMD-DOW MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(19951231)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
CUTIL00 ECHOQ   MYDATA
CUTIL00 ECHOQ   MYRESULT
WRITE
WRITE /\/\/\*51  C Y M D - C Y J                   /\/\/\-----------
/********************************************************************/
/* *51 CYMD-CYJ from program documentation                          */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 CYMD-CYJ  MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(19951231)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*52  C Y D M - D 8                     /\/\/\-----------
/********************************************************************/
/* *52 CYDM-D8 from program documentation                           */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 CYDM-D8   MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(19951605)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*53  C Y D M - D A Y                   /\/\/\-----------
/********************************************************************/
/* *53 CYDM-DAY from program documentation                          */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 CYDM-DAY MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(19951605)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
CUTIL00 ECHOQ   MYDATA
CUTIL00 ECHOQ   MYRESULT
WRITE
WRITE /\/\/\*54  C Y D M - D O W                   /\/\/\-----------
/********************************************************************/
/* *54 CYDM-DOW from program documentation                          */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 CYDM-DOW MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(19951605)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
CUTIL00 ECHOQ   MYDATA
CUTIL00 ECHOQ   MYRESULT
WRITE
WRITE /\/\/\*55  C Y D M - C Y J                   /\/\/\-----------
/********************************************************************/
/* *55 CYDM-CYJ from program documentation                          */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 CYDM-CYJ  MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(19951605)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*56  F I L L                           /\/\/\-----------
/********************************************************************/
/* *56 FILL from program documentation                              */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 FILL     MYDATA MYPARMS )
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(1)
SET MYDATAL =
SET MYPARMS  = &STR(*20)   /* 20 *'s in VAR1             */
SET MYPARMSL  =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYPARMS  ='&MYPARMS'
WRITE  MYPARMSL =&MYPARMSL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*57  L S T R I P                       /\/\/\-----------
/********************************************************************/
/* *57 LSTRIP from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 LSTRIP   MYDATA MYPARMS )
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(,,28,BC,)
SET MYDATAL =
SET MYPARMS  = &STR(,)
SET MYPARMSL  =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYPARMS  ='&MYPARMS'
WRITE  MYPARMSL =&MYPARMSL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*58  R S T R I P                       /\/\/\-----------
/********************************************************************/
/* *58 RSTRIP from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 RSTRIP   MYDATA MYPARMS )
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(,,28,BC,)
SET MYDATAL =
SET MYPARMS  = &STR(,)
SET MYPARMSL  =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYPARMS  ='&MYPARMS'
WRITE  MYPARMSL =&MYPARMSL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*59  S T R I P                         /\/\/\-----------
/********************************************************************/
/* *59 STRIP from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 STRIP    MYDATA MYPARMS )
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(,,28,BC,)
SET MYDATAL =
SET MYPARMS  = &STR(,)
SET MYPARMSL  =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYPARMS  ='&MYPARMS'
WRITE  MYPARMSL =&MYPARMSL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*60  S T R I P $                       /\/\/\-----------
/********************************************************************/
/* *60 STRIP$ from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 STRIP$   MYDATA MYPARMS )
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(,,28,BC,)
SET MYDATAL =
SET MYPARMS  = &STR(,)
SET MYPARMSL  =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYPARMS  ='&MYPARMS'
WRITE  MYPARMSL =&MYPARMSL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*61  C O N C A T                       /\/\/\-----------
/********************************************************************/
/* *61 CONCAT from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 CONCAT   MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(String data 1.)
SET MYDATAL =
SET MYRESULT = &STR(A result string 2.)
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULT'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*62  U N S T R                         /\/\/\-----------
/********************************************************************/
/* *62 UNSTR from program documentation                             */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 UNSTR    MYDATA MYPARMS )
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(1234567 and so on . I am 78 long. )
SET MYDATAL =
SET MYPARMS  = &STR(7)
SET MYPARMSL  =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYPARMS  ='&MYPARMS'
WRITE  MYPARMSL =&MYPARMSL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
SET CTR = 0
DO WHILE &CTR <= &MYDATA0
  SET EVALLN = &STR(&&MYDATA&CTR)
  WRITE  MYDATA&CTR = '&EVALLN'
  SET CTR = &CTR + 1
END
WRITE
WRITE /\/\/\*63  R E P L A C E                     /\/\/\-----------
/********************************************************************/
/* *63 REPLACE from program documentation                           */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 REPLACE  MYDATA MYPARMS )
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(1234567 AND SO ON . I AM 7879.)
SET MYDATAL =
SET MYPARMS  = &STR("SO ON ","890123456--")
SET MYPARMSL  =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYPARMS  ='&MYPARMS'
WRITE  MYPARMSL =&MYPARMSL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*64  V E X I S T                       /\/\/\-----------
/********************************************************************/
/* *64 VEXIST from program documentation                            */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 VEXIST   MYDATA )
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  = &STR(TEST)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULTL'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 4 THEN WRITE  ** PASSED PER RC **
IF &RC NE 4 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*65  T D S N                           /\/\/\-----------
/********************************************************************/
/* *65 TDSN from program documentation                              */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 TDSN     MYDATA )
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  =&STR(HERC01.CARDS)
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULTL'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*66  N O W                             /\/\/\-----------
/********************************************************************/
/* *66 NOW from program documentation                               */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 NOW      MYDATA )
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  =
SET MYDATAL =
SET MYRESULT =
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULTL'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*67  P A D                             /\/\/\-----------
/********************************************************************/
/* *67 PAD from program documentation                               */
/********************************************************************/
SET I = &I + 1
SET MYCMD = &STR(CUTIL00 PAD      MYDATA MYRESULT)
WRITE &STR(&TSTHDR)&I: &MYCMD
SET MYDATA  =&STR(PAD ME TO 30 LONG)
SET MYDATAL =
SET MYRESULT =&STR( 30)
SET MYRESULTL =
SET ERRMSG =
&MYCMD
SET RC = &LASTCC
WRITE  RC       =&RC
WRITE  ERRMSG   =&ERRMSG
WRITE  MYDATA   ='&MYDATA'
WRITE  MYDATAL  =&MYDATAL
WRITE  MYRESULT ='&MYRESULTL'
WRITE  MYRESULTL=&MYRESULTL
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX
WRITE
WRITE /\/\/\*68  G E T 1 V                         /\/\/\-----------    LB1100
/********************************************************************/  LB1100
/* *68 GET1V from program documentation                             */  LB1100
/********************************************************************/  LB1100
SET I = &I + 1                                                          LB1100
SET MYCMD = &STR(CUTIL00 GET1V    MYDATA MYPEEK)                        LB1100
WRITE &STR(&TSTHDR)&I: &MYCMD                                           LB1100
SET MYDATA  =&STR(THE TABLE TOP IS CLEAR)                               LB1100
SET MYDATAL =                                                           LB1100
SET MYPEEK =&STR(x11)                                                   LB1100
SET MYPEEKL =                                                           LB1100
SET ERRMSG =                                                            LB1100
&MYCMD                                                                  LB1100
SET RC = &LASTCC                                                        LB1100
WRITE  RC       =&RC                                                    LB1100
WRITE  ERRMSG   =&ERRMSG                                                LB1100
WRITE  MYDATA   ='&MYDATA'                                              LB1100
WRITE  MYDATAL  =&MYDATAL                                               LB1100
WRITE  MYPEEK   ='&MYPEEK'                                              LB1100
WRITE  MYPEEKL  =&MYPEEKL                                               LB1100
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **                             LB1100
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX                   LB1100
WRITE                                                                   LB1100
WRITE /\/\/\*69  P U T 1 V                         /\/\/\-----------    LB1100
/********************************************************************/  LB1100
/* *69 PUT1V from program documentation                             */  LB1100
/********************************************************************/  LB1100
SET I = &I + 1                                                          LB1100
SET MYCMD = &STR(CUTIL00 PUT1V    MYDATA MYPOKE)                        LB1100
WRITE &STR(&TSTHDR)&I: &MYCMD                                           LB1100
SET MYDATA  =&STR(THE TABLE TOP IS CLEAR)                               LB1100
SET MYDATAL =                                                           LB1100
SET MYPOKE =&STR(N22)                                                   LB1100
SET MYPOKEL =                                                           LB1100
SET ERRMSG =                                                            LB1100
&MYCMD                                                                  LB1100
SET RC = &LASTCC                                                        LB1100
WRITE  RC       =&RC                                                    LB1100
WRITE  ERRMSG   =&ERRMSG                                                LB1100
WRITE  MYDATA   ='&MYDATA'                                              LB1100
WRITE  MYDATAL  =&MYDATAL                                               LB1100
WRITE  MYPOKE   ='&MYPOKE'                                              LB1100
WRITE  MYPOKEL  =&MYPOKEL                                               LB1100
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **                             LB1100
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX                   LB1100
WRITE                                                                   LB1100
WRITE /\/\/\*70  M C A L                           /\/\/\-----------    LB1100
/********************************************************************/  LB1100
/* *70 MCAL from program documentation                              */  LB1100
/********************************************************************/  LB1100
SET I = &I + 1                                                          LB1100
SET MYCMD = &STR(CUTIL00 MCAL     MYDATA MYCALNDR)                      LB1100
WRITE &STR(&TSTHDR)&I: &MYCMD                                           LB1100
SET MYDATA  =                                                           LB1100
SET MYDATAL =                                                           LB1100
SET MYCALNDR =                                                          LB1100
SET MYCALNDRL =                                                         LB1100
SET ERRMSG =                                                            LB1100
&MYCMD                                                                  LB1100
SET RC = &LASTCC                                                        LB1100
WRITE  RC       =&RC                                                    LB1100
WRITE  ERRMSG   =&ERRMSG                                                LB1100
WRITE  MYDATA   ='&MYDATA'                                              LB1100
WRITE  MYDATAL  =&MYDATAL                                               LB1100
WRITE  MYCALNDR ='&MYCALNDR'                                            LB1100
WRITE  MYCALNDRL =&MYCALNDRL                                            LB1100
IF &RC EQ 0 THEN WRITE  ** PASSED PER RC **                             LB1100
IF &RC NE 0 THEN WRITE  ** FAILED PER RC **   <<===XX                   LB1100
SET SCOL = 1                                                            LB1100
SET LOOPN = 7                                                           LB1100
DO WHILE &LOOPN > 0                                                     LB1100
 WRITE     &SUBSTR(&SCOL:&SCOL+20,&MYCALNDR)                            LB1100
 SET LOOPN = &LOOPN - 1                                                 LB1100
 SET SCOL  = &SCOL  + 21                                                LB1100
END                                                                     LB1100
WRITE                                                                   LB1100
WRITE /\/\/\*71  L E N                             /\/\/\-----------    LB1105
/********************************************************************/  LB1105
/* *72 LEN from program documentation                               */  LB1105
/********************************************************************/  LB1105
SET I = &I + 1                                                          LB1105
SET MYCMD = &STR(CUTIL00 MCAL     MYDATA         )                      LB1105
WRITE &STR(&TSTHDR)&I: &MYCMD                                           LB1105
SET MYDATA  = &STR( MY WORD IN THE STRING    )                          LB1105
SET MYDATAL =                                                           LB1105
SET MYRESULT =                                                          LB1105
SET MYRESULTL =                                                         LB1105
SET ERRMSG =                                                            LB1105
&MYCMD                                                                  LB1105
SET RC = &LASTCC                                                        LB1105
WRITE  RC       =&RC                                                    LB1105
WRITE  ERRMSG   =&ERRMSG                                                LB1105
WRITE  MYDATA   ='&MYDATA'                                              LB1105
WRITE  MYDATAL  =&MYDATAL                                               LB1105
WRITE  MYRESULT ='&MYRESULT'                                            LB1105
WRITE  MYRESULTL =&MYRESULTL                                            LB1105
IF &RC EQ 22 THEN WRITE  ** PASSED PER RC **                            LB1105
IF &RC NE 22 THEN WRITE  ** FAILED PER RC **   <<===XX                  LB1105
WRITE                                                                   LB1105
WRITE /\/\/\*73  S L E N                           /\/\/\-----------    LB1105
/********************************************************************/  LB1105
/* *73 SLEN from program documentation                              */  LB1105
/********************************************************************/  LB1105
SET I = &I + 1                                                          LB1105
SET MYCMD = &STR(CUTIL00 SLEN     MYDATA         )                      LB1105
WRITE &STR(&TSTHDR)&I: &MYCMD                                           LB1105
SET MYDATA  = &STR( MY WORD IN THE STRING    )                          LB1105
SET MYDATAL =                                                           LB1105
SET MYRESULT =                                                          LB1105
SET MYRESULTL =                                                         LB1105
SET ERRMSG =                                                            LB1105
&MYCMD                                                                  LB1105
SET RC = &LASTCC                                                        LB1105
WRITE  RC       =&RC                                                    LB1105
WRITE  ERRMSG   =&ERRMSG                                                LB1105
WRITE  MYDATA   ='&MYDATA'                                              LB1105
WRITE  MYDATAL  =&MYDATAL                                               LB1105
WRITE  MYRESULT ='&MYRESULT'                                            LB1105
WRITE  MYRESULTL =&MYRESULTL                                            LB1105
IF &RC EQ 26 THEN WRITE  ** PASSED PER RC **                            LB1105
IF &RC NE 26 THEN WRITE  ** FAILED PER RC **   <<===XX                  LB1105
WRITE                                                                   LB1105
WRITE /\/\/\*74  O V E R L A Y                     /\/\/\-----------    LB1105
/********************************************************************/  LB1105
/* *74 OVERLAY from program documentation                           */  LB1105
/********************************************************************/  LB1105
SET I = &I + 1                                                          LB1105
SET MYCMD = &STR(CUTIL00 OVERLAY  MYDATA MYOPARM)                       LB1105
WRITE &STR(&TSTHDR)&I: &MYCMD                                           LB1105
SET MYDATA  = &STR( MY TEXT TO BE OVERLAYED OK!! )                      LB1105
SET MYDATAL =                                                           LB1105
SET MYOPARM = &STR("----- ",7)                                          LB1105
SET MYOPARML =                                                          LB1105
SET ERRMSG =                                                            LB1105
&MYCMD                                                                  LB1105
SET RC = &LASTCC                                                        LB1105
WRITE  RC       =&RC                                                    LB1105
WRITE  ERRMSG   =&ERRMSG                                                LB1105
WRITE  MYDATA   ='&MYDATA'                                              LB1105
WRITE  MYDATAL  =&MYDATAL                                               LB1105
WRITE  MYOPARM  ='&MYOPARM'                                             LB1105
WRITE  MYOPARML =&MYOPARML                                              LB1105
IF &RC EQ 00 THEN WRITE  ** PASSED PER RC **                            LB1105
IF &RC NE 00 THEN WRITE  ** FAILED PER RC **   <<===XX                  LB1105
WRITE                                                                   LB1105
WRITE /\/\/\*75  U T D S N                         /\/\/\-----------    LB1105
/********************************************************************/  LB1105
/* *75 UTDSN   from program documentation                           */  LB1105
/********************************************************************/  LB1105
SET I = &I + 1                                                          LB1105
SET MYCMD = &STR(CUTIL00 UTDSN    MYDATA)                               LB1105
WRITE &STR(&TSTHDR)&I: &MYCMD                                           LB1105
SET MYDATA  = &STR(CARDS)                                               LB1105
SET MYDATAL =                                                           LB1105
SET MYRESULT =                                                          LB1105
SET MYRESULTL =                                                         LB1105
SET ERRMSG =                                                            LB1105
&MYCMD                                                                  LB1105
SET RC = &LASTCC                                                        LB1105
WRITE  RC       =&RC                                                    LB1105
WRITE  ERRMSG   =&ERRMSG                                                LB1105
WRITE  MYDATA   ='&MYDATA'                                              LB1105
WRITE  MYDATAL  =&MYDATAL                                               LB1105
WRITE  MYRESULT ='&MYRESULT'                                            LB1105
WRITE  MYRESULTL =&MYRESULTL                                            LB1105
IF &RC EQ 00 THEN WRITE  ** PASSED PER RC **                            LB1105
IF &RC NE 00 THEN WRITE  ** FAILED PER RC **   <<===XX                  LB1105
WRITE                                                                   LB1105
WRITE /\/\/\*76  T R U N C                         /\/\/\-----------    LB1105
/********************************************************************/  LB1105
/* *76 TRUNC   from program documentation                           */  LB1105
/********************************************************************/  LB1105
SET I = &I + 1                                                          LB1105
SET MYCMD = &STR(CUTIL00 TRUNC MYDATA MYTRUNC)                          LB1105
WRITE &STR(&TSTHDR)&I: &MYCMD                                           LB1105
SET MYDATA  = &STR(THE MAN WALKED HERE)                                 LB1105
SET MYDATAL =                                                           LB1105
SET MYTRUNC = &STR(x07)                                                 LB1105
SET MYTRUNCL =                                                          LB1105
SET ERRMSG =                                                            LB1105
&MYCMD                                                                  LB1105
SET RC = &LASTCC                                                        LB1105
WRITE  RC       =&RC                                                    LB1105
WRITE  ERRMSG   =&ERRMSG                                                LB1105
WRITE  MYDATA   ='&MYDATA'                                              LB1105
WRITE  MYDATAL  =&MYDATAL                                               LB1105
WRITE  MYTRUNC  ='&MYTRUNC'                                             LB1105
WRITE  MYTRUNCL =&MYTRUNCL                                              LB1105
IF &RC EQ 00 THEN WRITE  ** PASSED PER RC **                            LB1105
IF &RC NE 00 THEN WRITE  ** FAILED PER RC **   <<===XX                  LB1105
WRITE                                                                   LB1105


WRITE ***** Testing CUTIL00 concluded. . . . . .  *** &SYSDATE &SYSTIME


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
./ ADD NAME=CCUTIL00
)F FUNCTION -
  The CCUTIL00 CLIST invokes the TPM CUTIL00 from TSO.

)X Statement SYNTAX  -

   CCUTIL00 FUNCT VAR01('xx.') VAR02('yyy yy') CLS(Y) VERBOSE(N)

  ALIAS    - NONE
)O OPERANDS -
  Parameters must appear in order as listed

))FUNCT    - required, CUTIL00 function to be performed

))VAR01    - required, content for VAR01 in quotes
             o  use quotes to include leading, trailing or
                embedded spaces (blanks) in content

))VAR02    - optional, content for VAR02 in quotes
             o  use quotes to include leading, trailing or
                embedded spaces (blanks) in content

))CLS      - optional, clear display screen
             o  Y - clear screen   Other - no clear screen
             o  default=Y

))VERBOSE  - optional, display header and command line
             o  Y - display header and command line
             o  Other - no display of header and command line
             o  default=Y


     Sample Command:

     ==> CCUTIL00 TRIM VAR01(' TRIM  MY SPACE   ') VAR02(X)

     Sample Response:

     --- CCUTIL00: CUTIL00 TEST TOOL

     CUTIL00 TRIM  VAR01

     --- CUTIL00 RESPONSE ..........................................
      RC    =0 SUCCESSFUL REQUEST
      VAR01 =' TRIM  MY SPACE   '
      VAR01L=00018
      VAR02 ='TRIM  MY SPACE'
      VAR02L=00014

     --- CCUTIL00: CUTIL00 TEST TOOL   ...DONE!  TRIM

./ ADD NAME=CUTIL00
)F FUNCTION -
  The CUTIL00 command performs functions on CLIST variables
  such as CENTER, TRIM, and RJUST string functions.  All
  processing occurs internally on CLIST variables.  Return
  code is returned via &LASTCC.  See program documentation
  for more functional details.

)X Statement SYNTAX  -
         CUTIL00 FUNCT VAR1 VAR2 QUOTE
  REQUIRED - FUNCT, VAR1
  DEFAULTS - NONE
  ALIAS    - NONE
)O OPERANDS -
  Parameters must appear in order as listed

))FUNCT    - required, function to be performed

))VAR1     - required, name of CLIST variable
             o  is not prefixed with an '&'
             o  variable must exist before invoking CUTIL00
             o  1-8 bytes in length, must follow CLIST variable
                naming conventions

))VAR2     - optional, name of CLIST variable to store results
             or contain function parameters
             o  is not prefixed with an '&'
             o  1-8 bytes in length, must follow CLIST variable
                naming conventions
             o  Storing results
                -  If variable does not exist, it will be created
                -  If variable does exist, it will be updated
                -  If no variable is specifed, use VAR1 variable
                -  If '$NOVAR2' is specifed, use VAR1 variable
                   - '$NOVAR2' used as a placeholder if VAR2
                     is not specified with QUOTE option
             o  Function Parameters
                -  variable must exist before invoking CUTIL00

))QUOTE    - optional, quote delimited string values
             o  Used by CUTIL00 3270 ISPF test tool
             o  If 'QUOTE' is specified , string content between
                first and last quotes comprise VAR1 value.
             o  Use this option to include leading or trailing
                spaces in a string.

     The following variable names provide additional information
     after successful execution of CUTIL00:

      o  &LASTCC contains return code from CUTIL00 after execution
      o  &ERRMSG contains error message text for function executed
      o  &var1L  contains length of var1 after execution
      o  &var2L  contains length of var2 after execution

      NOTE:  The above variables may not be declared by CUTIL00 if
             CUTIL00 abends or does not execute. Variables include
             &ERRMSG, &var1L and &var2L.  Suggested to initialize
             to NULLS before invoking CUTIL00. See LTRIM CLIST
             example below.

      NOTE:  - CLIST variable name length is 8 bytes max
             - CLIST variable data length is 256 bytes max
             - Result data length is 256 bytes max


  Function Summary: (see program for more details on each function)

     ABOUT    - Display mySTAMP information

     LTRIM    - Remove leading spaces
     RTRIM    - Remove trailing spaces
     TRIM     - Remove leading and trailing spaces
     TRIM$    - Remove leading, trailing, and duplicate spaces

     INDEX    - Alias for FIND
     INDEXB   - Alias for FINDL

     ISNUM    - Test for all numeric (0-9)
     ISALPHA  - Test for all alphabetic (A-Z,a-z)
     ISLOWER  - Test for all alphabetic (a-z)
     ISUPPER  - Test for all alphabetic (A-Z)
     ISBLANK  - Test for all blank (whitespace)
     ISDSN    - Test for valid MVS Dataset Name

     ECHO     - Display string value
     ECHOQ    - Display string value surrounded by single quotes
                on terminal screen

     REVRS    - Translate to reverse  A-Z > Z-A,  0-9 > 9-0

     UPPERC   - Translate to uppercase
     LOWERC   - Translate to lowercase

     COUNT    - Count occurances of a search string
     FINDALL  - Alias for COUNT
     FIND     - Find starting position of FIRST search string
     FINDL    - Find starting position of LAST search string

     CENTER   - Center content
     LJUST    - Left justify content
     RJUST    - Right justify content

     ZFILL    - Zero fill after content is right justified
     WORDS    - Number of words (contiguous non-blank groups)
     GEN#     - Generate a 3 digit number (000-999)

     DD2DSN   - Dataset name associated with DD name
     JOBINFO  - Job Information

     DAYSMM   - Number of days in MM
     DAYSYY   - Number of days in CCYY (365 or 366)
     ISLEAP   - Test CCYY for leap year

     CYJ-D8   - Date Conversion  CCYYJJJ to monthname DD, CCYY
     CYJ-DAY  - Date Conversion  CCYYJJJ to day name
     CYJ-DOW  - Date Conversion  CCYYJJJ to day of week number
     CYJ-mdcy - Date Conversion  CCYYJJJ to MMDDCCYY
                -mdcy can be in any order

     JCY-D8   - Date Conversion  JJJCCYY to monthname DD, CCYY
     JCY-DAY  - Date Conversion  JJJCCYY to day name
     JCY-DOW  - Date Conversion  JJJCCYY to day of week number
     JCY-mdcy - Date Conversion  JJJCCYY to MMDDCCYY
                -mdcy can be in any order

     MDCY-D8  - Date Conversion  MMDDCCYY to monthname DD, CCYY
     MDCY-DAY - Date Conversion  MMDDCCYY to day name
     MDCY-DOW - Date Conversion  MMDDCCYY to day of week number
     MDCY-cyj - Date Conversion  MMDDCCYY to CCYYJJJ
                -cyj  can be in any order

     DMCY-D8  - Date Conversion  DDMMCCYY to monthname DD, CCYY
     DMCY-DAY - Date Conversion  DDMMCCYY to day name
     DMCY-DOW - Date Conversion  DDMMCCYY to day of week number
     DMCY-cyj - Date Conversion  DDMMCCYY to CCYYJJJ
                -cyj  can be in any order

     CYMD-D8  - Date Conversion  CCYYMMDD to monthname DD, CCYY
     CYMD-DAY - Date Conversion  CCYYMMDD to day name
     CYMD-DOW - Date Conversion  CCYYMMDD to day of week number
     CYMD-cyj - Date Conversion  CCYYMMDD to CCYYJJJ
                -cyj  can be in any order

     CYDM-D8  - Date Conversion  CCYYDDMM to monthname DD, CCYY
     CYDM-DAY - Date Conversion  CCYYDDMM to day name
     CYDM-DOW - Date Conversion  CCYYDDMM to day of week number
     CYDM-cyj - Date Conversion  CCYYDDMM to CCYYJJJ
                -cyj  can be in any order

     FILL     - Fill VAR1 nnn times with delimiter

     LSTRIP   - Remove leading delimiter
     RSTRIP   - Remove trailing delimiter
     STRIP    - Remove leading and trailing delimiter
     STRIP$   - Remove all delimiter

     CONCAT   - Concatenate two strings
     UNSTR    - Unstring into separate variables

     REPLACE  - Replace substring in string
     VEXIST   - Test if CLIST Variable exists
     TDSN     - Generate temporary DSN nodes
     NOW      - Current system date and time
     PAD      - PAD string with delimiter to given length

     PUT1V    - Overlay 1-byte at position n in VAR1
     GET1V    - Get     1-byte at position n from VAR1
     MCAL     - Create monthly calendar string

     LEN      - Length of data in string
     SLEN     - String defined length
     OVERLAY  - Overlay substring in string
     UTDSN    - Generate user temporary DSN
     TRUNC    - Truncate String

@@
//* -------------------------------------------------------*
//* *  CUTIL00 for MVS3.8J TSO / Hercules                  *
//* *                                                      *
//* *  JOB: $INST04  Install CUTIL00 Software              *
//* *                                                      *
//* *  - Install libraries marked...                       *
//* *    - Search for '<--TARGET'                          *
//* *    - Update install libraries per your               *
//* *      installation standard                           *
//* *                                                      *
//* *  NOTE:                                               *
//* *  -----                                               *
//* *  Two user-mods, ZP60014 and ZP60038, are REQUIRED.   *
//* *                                                      *
//* *  For more information, refer to program CUTIL00.     *
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
//* *  Assemble Link-Edit CUTIL00 to SYS2.CMDLIB           *
//* -------------------------------------------------------*
//CUTIL00  EXEC  ASML,
//         PARM.LKED='MAP,LIST,LET,RENT,XREF,REUS,REFR'
//ASM.SYSIN DD DATA,DLM=@@
         TITLE 'CUTIL00:  CLIST Utility 00 for MVS3.8J / Hercules     '
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  ====================================================================
*
*    CCCC    UU    UU  TTTTTTTT  IIIIIIII  LL          0000      0000
*   CC  CC   UU    UU     TT        II     LL         00  00    00  00
*  CC        UU    UU     TT        II     LL        00    00  00    00
*  CC        UU    UU     TT        II     LL        00    00  00    00
*  CC        UU    UU     TT        II     LL        00    00  00    00
*   CC  CC   UUU  UUU     TT        II     LL         00  00    00  00
*    CCCC      UUUU       TT     IIIIIIII  LLLLLLLL    0000      0000
*
*  ====================================================================
*  12345678--12345678--12345678--12345678--12345678--12345678--12345678
*
*  Program: CUTIL00
*
*  Author: Larry Belmontes Jr.
*          https://ShareABitOfIT.net/CUTIL00-for-mvs-3-8j/
*          Copyright (C) 2020-2021 Larry Belmontes, Jr.
*
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
*     In addition, the author requests submissions regarding any
*  code modifications / enhancements and/or associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  benefiting the MVS 3.8J hobbyist public domain community.
*
*
         EJECT
*  Overview:
*  ================================================================
*
*     This program, CUTIL00, performs various functions on symbolic
*  variables given a function code and variable names containing
*  such data in a TSO CLIST.
*
*     Given the limited CLIST functions available under MVS 3.8J,
*  simple functions available in other languges and current OS
*  versions are enhanced for your CLIST programming convenience.
*
*     For example, centering content or trimming of strings or date
*  conversion:
*
*    String (before)                     String (after)
*  -------------------------------  -------------------------------
*  'center this TEXT             '  '      center this TEXT       '
*  ' trim  this TEXT   area      '  'trim this TEXT area'
*  '12311987'                       'December 31, 1987'
*
*     Invoking this utility is kept to a simple line command using
*  two to four parameters:
*
*  CUTIL00 FUNCT VAR1 VAR2
*      where; FUNCT - function being requested
*             VAR1  - variable name containing source string
*             VAR2  - variable name for results or parameters,
*                     which may be optional depending of function
*      Note:  Other parameters are availble.  See Command Syntax.
*
*     This program is written in IFOX00 Assembler (24-bit addressing)
*  using the MVS 3.8J Tur(n)key 3 Operating System.
*
*     CUTIL00 was developed and tested using Volker Bandke's MVS CD -
*  MVS38J TK3 with Hercules 3.13 hosted on a Windows 10 Pro machine.
*
*     MVS38J TK4- update 8 (maintained by Juergen Winkelmann) includes
*  user-mod ZP60014.  After applying user-mod ZP60038, CUTIL00 was
*  installed and successfully validated with a CLIST harness under
*  TSO.
*
*  Enjoy the added value to CLIST development with CUTIL00!
*  Larry Belmontes Jr.
*
*
         EJECT
*  Prerequisite: User Modifications
*  ===================================================================
*
*     Two user-mods, ZP60014 and ZP60038, are REQUIRED to process
*  CLIST symbolic variables via the IKJCT441 API on MVS 3.8J before
*  using CUTIL00.  Otherwise, CUTIL00 is rendered useless!
*
*     More information on the above user-mods can be obtained from
*  the following website:
*       http://www.prycroft6.com.au/vs2mods/
*
*     If the above user-mods are not installed, a system abend
*  806-4 will occur when CUTIL00 is executed.  Additionally, an
*  error message will be displayed on your TSO session screen:
*      'CUTIL00 4098 -Cannot link to IKJCT441'
*
*     Check your system to determine if one or both user-mods are
*  required.  ZP60038 requires ZP60014.
*
*
         EJECT
*  Command Syntax for CUTIL00:                                          LB1100z
*  ================================================================
*
*  TSO CP:                                                              LB1105z
*  CUTIL00 FUNCT VAR1 VAR2 QUOTE
*
*
*  TSO Called:
*  CALL 'pds(CUTIL00)' 'FUNCT VAR1 VAR2 QUOTE'
*
*    where:
*
*    1) FUNCT    required, function to be performed
*
*    2) VAR1     required, name of CLIST variable
*                o  is not prefixed with an '&'
*                o  variable must exist before invoking CUTIL00
*                o  1-8 bytes in length, must follow CLIST variable
*                   naming conventions
*
*    3) VAR2     optional, name of CLIST variable to store results
*                or contain function parameters
*                o  is not prefixed with an '&'
*                o  1-8 bytes in length, must follow CLIST variable
*                   naming conventions
*                o  Storing results
*                   -  If variable does not exist, it will be created
*                   -  If variable does exist, it will be updated
*                   -  If no variable is specifed, use VAR1 variable
*                   -  If '$NOVAR2' is specifed, use VAR1 variable
*                      - '$NOVAR2' used as a placeholder if VAR2
*                        is not specified with QUOTE option
*                o  Function Parameters
*                   -  variable must exist before invoking CUTIL00
*
*    4) QUOTE    optional, quote delimited string values
*                o  If 'QUOTE' is specified , string content between
*                   first and last quotes comprise VAR1 value.
*                   Use this option to include leading or trailing
*                   spaces in a string.
*
         EJECT
*  Command Syntax for CUTIL00:              (continued)                 LB1100z
*  ================================================================
*    The following variable names provide additional information
*    after successful execution of CUTIL00:
*
*     o  &LASTCC contains return code from CUTIL00 after execution
*     o  &ERRMSG contains error message text for function executed
*     o  &var1L  contains length of var1 after execution
*     o  &var2L  contains length of var2 after execution
*
*     NOTE:  The above variables may not be declared by CUTIL00 if
*            CUTIL00 abends or does not execute. Variables include
*            &ERRMSG, &var1L and &var2L.  Suggested to initialize
*            to NULLS before invoking CUTIL00. See LTRIM CLIST
*            example below.
*
*     NOTE:  - CLIST variable name length is 8 bytes max
*            - CLIST variable data length is 256 bytes max
*            - Result data length is 256 bytes max
*
*
         EJECT
*    Function List:
*  --------------------------------------------------------------------
*00  ABOUT    - Display mySTAMP information
*
*01  LTRIM    - remove leading spaces
*02  RTRIM    - remove trailing spaces
*03  TRIM     - remove leading and trailing
*04  TRIM$    - remove leading, trailing, and duplicate spaces
*
*05  INDEX    - Alias to FIND
*06  INDEXB   - Alias to FINDL
*
*07  ISNUM    - Test for all numeric (0-9)
*08  ISALPHA  - Test for all alphabetic (A-Z,a-z)
*09  ISLOWER  - Test for all alphabetic Lower (a-z)
*10  ISUPPER  - Test for all alphabetic Upper (A-Z)
*11  ISBLANK  - Test for all blank (whitespace)
*12  ISDSN    - Test for valid MVS Dataset Name
*
*13  ECHO     - Display string value
*14  ECHOQ    - Display string value surrounded by single quotes
*
*15  REVRS    - Translate to reverse  A-Z > Z-A,  0-9 > 9-0
*
*16  UPPERC   - Translate to uppercase
*17  LOWERC   - Translate to lowercase
*
*18  COUNT    - Count occurrences of a search string                    LB1105z
*18  FINDALL  - Alias COUNT
*19  FIND     - Find starting position of FIRST search string
*20  FINDL    - Find starting position of LAST search string
*
*21  CENTER   - Center content
*22  LJUST    - Left justify content
*23  RJUST    - Right justify content
*
*24  ZFILL    - Zero fill after content is right justified
*25  WORDS    - Number of words (contiguous non-blank groups)
*26  GEN#     - Generate a 3 digit number (000-999)
*
*27  DD2DSN   - Dataset name associated with DD name
*28  JOBINFO  - Job Information
*
*29  DAYSMM   - Number of days in MM
*30  DAYSYY   - Number of days in CCYY (365 or 366)
*31  ISLEAP   - Test CCYY for leap year
*
         EJECT
*    Function List:  (continued)
*  --------------------------------------------------------------------
*32  CYJ-D8   - Date Conversion  CCYYJJJ to monthname DD, CCYY
*33  CYJ-DAY  - Date Conversion  CCYYJJJ to day name
*34  CYJ-DOW  - Date Conversion  CCYYJJJ to day of week number
*35  CYJ-mdcy - Date Conversion  CCYYJJJ to MMDDCCYY (any order)
*
*36  JCY-D8   - Date Conversion  JJJCCYY to monthname DD, CCYY
*37  JCY-DAY  - Date Conversion  JJJCCYY to day name
*38  JCY-DOW  - Date Conversion  JJJCCYY to day of week number
*39  JCY-mdcy - Date Conversion  JJJCCYY to MMDDCCYY (any order)
*
*40  MDCY-D8  - Date Conversion  MMDDCCYY to monthname DD, CCYY
*41  MDCY-DAY - Date Conversion  MMDDCCYY to day name
*42  MDCY-DOW - Date Conversion  MMDDCCYY to day of week number
*43  MDCY-cyj - Date Conversion  MMDDCCYY to CCYYJJJ (any order)
*
*44  DMCY-D8  - Date Conversion  DDMMCCYY to monthname DD, CCYY
*45  DMCY-DAY - Date Conversion  DDMMCCYY to day name
*46  DMCY-DOW - Date Conversion  DDMMCCYY to day of week number
*47  DMCY-cyj - Date Conversion  DDMMCCYY to CCYYJJJ (any order)
*
*48  CYMD-D8  - Date Conversion  CCYYMMDD to monthname DD, CCYY
*49  CYMD-DAY - Date Conversion  CCYYMMDD to day name
*50  CYMD-DOW - Date Conversion  CCYYMMDD to day of week number
*51  CYMD-cyj - Date Conversion  CCYYMMDD to CCYYJJJ (any order)
*
*52  CYDM-D8  - Date Conversion  CCYYDDMM to monthname DD, CCYY
*53  CYDM-DAY - Date Conversion  CCYYDDMM to day name
*54  CYDM-DOW - Date Conversion  CCYYDDMM to day of week number
*55  CYDM-cyj - Date Conversion  CCYYDDMM to CCYYJJJ (any order)
*
*56  FILL     - Fill VAR1 nnn times with delimiter
*
*57  LSTRIP   - Remove leading delimiter
*58  RSTRIP   - Remove trailing delimiter
*59  STRIP    - Remove leading and trailing delimiter
*60  STRIP$   - Remove all delimiter
*
*61  CONCAT   - Concatenate two strings
*62  UNSTR    - Unstring into separate variables
*
*63  REPLACE  - Replace substring in string
*64  VEXIST   - Test if CLIST Variable exists
*65  TDSN     - Generate temporary DSN nodes
*66  NOW      - Current system date and time
*67  PAD      - PAD string with delimiter to given length
*
         EJECT                                                          LB1105z
*    Function List:  (continued)                                        LB1105z
*  -------------------------------------------------------------------- LB1105z
*68  GET1V    - Get single byte from position VAR1(nnn)                 LB1100c
*69  PUT1V    - Put single byte to   position VAR1(nnn)                 LB1100d
*70  MCAL     - Monthly calendar string for MMDDCCYY                    LB1100e
*71  MCALA    - Monthly calendar string for MMDDCCYY w attributes       LB1100e
*72  LEN      - Length of data in string                                LB1105a
*73  SLEN     - String defined length                                   LB1105a
*74  OVERLAY  - Overlay string into VAR1                                LB1105c
*75  UTDSN    - Generate user temporary DSN                             LB1105g
*76  TRUNC    - Truncate String                                         LB1105h
*
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*00| ABOUT    |  -   |  -   | Display program stamp information.      |
*  |          |      |      |                                         |
*  --------------------------------------------------------------------
*  CUTIL00 ABOUT                                   /* ABOUT         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    CUTIL00 MVS3.8J Vv.r.mm 02202020 mm/dd/yy @ hh.mm
*       |       |       |       |        |        |
*       |       |       |       |        |        |
*       +---+---+       +---+---+        +----+---+
*           |               |                 |
*     Program Name          |              Current
*     and Basis OS          |             Date/Time
*                           |
*                      Version/Date
*
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*
*
         EJECT
*    Functions:
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*01| LTRIM    | req  | opt  | Remove leading spaces from variable     |
*  |          |      |      | VAR1.                                   |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  Statement Line:
*
*  CUTIL00 LTRIM var1 var2                         /* LTRIM         */
*
*     o  var1 must be declared before invoking utility
*     o  var2 contains results, if specified.
*        Otherwise, var1 contains results.
*     o  &LASTCC contains return code from CUTIL00
*     o  &ERRMSG contains error message information from CUTIL00
*     o  &var1L  contains length of var1
*     o  &var2L  contains length of var2, if var2 was specified
*
*
*  Example CLIST:
*  0...0....1....1....2....2....3....3....4....4....5....5....6....6...
*  1...5....0....5....0....5....0....5....0....5....0....5....0....5...
*  SET MYDATA=&STR(   leading spaces)              /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 LTRIM MYDATA MYRESULT                   /* LTRIM         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*  WRITE CUTIL00 RC=&RC - &ERRMSG
*  WRITE MYDATAL   =&MYDATAL   MYDATA='&MYDATA'
*  WRITE MYRESULTL =&MYRESULTL MYRESULT='&MYRESULT'
*  CUTIL00 ECHOQ  MYRESULT
*    /*                                                             */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='   LEADING SPACES'
*              MYDATAL  =17
*              MYRESULT ='LEADING SPACES'
*              MYRESULTL=14
*
*    NOTE:  MVS3.8J CLIST processor converts all characters to
*           UPPERCASE!  While CUTIL00 can convert a string to
*           lowercase, the string will remain lowercase until
*           referenced in a CLIST.
*
*    req - VAR name is required
*    opt - VAR name is optional
*    no  - VAR name is not required
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*02| RTRIM    | req  | opt  | Remove trailing spaces from variable    |
*  |          |      |      | VAR1.                                   |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(trailing spaces  )              /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 RTRIM MYDATA MYRESULT                   /* RTRIM         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='TRAILING SPACES  '
*              MYDATAL  =17
*              MYRESULT ='TRAILING SPACES'
*              MYRESULTL=15
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*03| TRIM     | req  | opt  | Remove leading and trailing spaces from |
*  |          |      |      | variable VAR1.                          |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR( trim   my  data )              /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 TRIM MYDATA MYRESULT                    /* TRIM          */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   =' TRIM   MY  DATA '
*              MYDATAL  =17
*              MYRESULT ='TRIM   MY  DATA'
*              MYRESULTL=15
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*04| TRIM$    | req  | opt  | Remove leading, trailing and duplicate  |
*  |          |      |      | spaces from variable VAR1.              |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR( trim   my  data )              /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 TRIM$ MYDATA MYRESULT                   /* TRIM$         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   =' TRIM   MY  DATA '
*              MYDATAL  =17
*              MYRESULT ='TRIM MY DATA'
*              MYRESULTL=12
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*05| INDEX    | req  | opt  | ALIAS to FIND, see FIND function.       |
*  | (alias)  |      |      |                                         |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(my, string, ok?   )             /* INIT MYDATA   */
*  SET DLMTR=&STR(",")                             /* INIT DLMTR    */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET DLMTRL   =                                  /* INIT DLMTRL   */
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 INDEX MYDATA DLMTR                      /* INDEX         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =3
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='MY, STRING, OK?   '
*              MYDATAL  =18
*              DLMTR    ='","'
*              DLMTRL   =3
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*06| INDEXB   | req  | opt  | ALIAS to FINDL, see FINDL function.     |
*  | (alias)  |      |      |                                         |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(my, string, ok?   )             /* INIT MYDATA   */
*  SET DLMTR=&STR(",")                             /* INIT DLMTR    */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET DLMTRL   =                                  /* INIT DLMTRL   */
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 INDEXB MYDATA DLMTR                     /* INDEXB        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =11
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='MY, STRING, OK?   '
*              MYDATAL  =18
*              DLMTR    ='","'
*              DLMTRL   =3
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*07| ISNUM    | req  | no   | Test variable VAR1 for all numeric      |
*  |          |      |      | values (0-9) returning a true-false     |
*  |          |      |      | indication.                             |
*  |          |      |      |                                         |
*  |          |      |      | RC=4000  &ERRMSG='FALSE'                |
*  |          |      |      | RC=4001  &ERRMSG='TRUE'                 |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(0123456789)                     /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 ISNUM MYDATA                            /* ISNUM         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =4001
*              ERRMSG   ='TRUE                          '
*              MYDATA   ='0123456789'
*              MYDATAL  =10
*              MYRESULT =''
*              MYRESULTL=0
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*08| ISALPHA  | req  | no   | Test variable VAR1 for all alphabetic   |
*  |          |      |      | values (A-Z,a-z) returning a true-false |
*  |          |      |      | indication.                             |
*  |          |      |      |                                         |
*  |          |      |      | RC=4000  &ERRMSG='FALSE'                |
*  |          |      |      | RC=4001  &ERRMSG='TRUE'                 |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(abCDEfghiJ)                     /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 ISALPHA MYDATA                          /* ISALPHA       */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =4001
*              ERRMSG   ='TRUE                          '
*              MYDATA   ='ABCDEFGHIJ'
*              MYDATAL  =10
*              MYRESULT =''
*              MYRESULTL=0
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*09| ISLOWER  | req  | no   | Test variable VAR1 for all alphabetic   |
*  |          |      |      | lowercase (A-Z,a-z) returning a         |
*  |          |      |      | true-false indication.                  |
*  |          |      |      |                                         |
*  |          |      |      | RC=4000  &ERRMSG='FALSE'                |
*  |          |      |      | RC=4001  &ERRMSG='TRUE'                 |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(abcdefghij)                     /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 ISLOWER MYDATA                          /* ISLOWER       */  LB1100z
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =4000
*              ERRMSG   ='FALSE                         '
*              MYDATA   ='ABCDEFGHIJ'
*              MYDATAL  =10
*              MYRESULT =''
*              MYRESULTL=0
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*10| ISUPPER  | req  | no   | Test variable VAR1 for all alphabetic   |
*  |          |      |      | uppercase (A-Z) returning a true-false  |
*  |          |      |      | indication.                             |
*  |          |      |      |                                         |
*  |          |      |      | RC=4000  &ERRMSG='FALSE'                |
*  |          |      |      | RC=4001  &ERRMSG='TRUE'                 |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(ABCDEFGHIJ)                     /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 ISUPPER MYDATA                          /* ISUPPER       */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =4001
*              ERRMSG   ='TRUE                          '
*              MYDATA   ='ABCDEFGHIJ'
*              MYDATAL  =10
*              MYRESULT =''
*              MYRESULTL=0
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*11| ISBLANK  | req  | no   | Test variable VAR1 for all blank        |
*  |          |      |      | (spaces) returning a true-false         |
*  |          |      |      | indication.                             |
*  |          |      |      |                                         |
*  |          |      |      | RC=4000  &ERRMSG='FALSE'                |
*  |          |      |      | RC=4001  &ERRMSG='TRUE'                 |
*  |          |      |      |                                         |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(         )                      /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 ISBLANK MYDATA                          /* ISBLANK       */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =4001
*              ERRMSG   ='TRUE                          '
*              MYDATA   ='          '
*              MYDATAL  =9
*              MYRESULT =''
*              MYRESULTL=0
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*12| ISDSN    | req  | no   | Test variable VAR1 for valid MVS        |
*  |          |      |      | dataset name returning a true-false     |
*  |          |      |      | indication.                             |
*  |          |      |      |                                         |
*  |          |      |------------------------------------------------|
*  |          |      |  0   5    1    1    2    2    3    3    4   4  |
*  |          |      |  1...5....0....5....0....5....0....5....0...4  |
*  |          |      |  xxxxxxxx.xxxxxxxx.xxxxxxxx.xxxxxxxx.xxxxxxxx  |
*  |          |      |                                                |
*  |          |      |  MVS Dataset Naming standard:                  |
*  |          |      |        - DSN is 1 to 44 bytes                  |
*  |          |      |        - DSN can contain up to 22 segments     |
*  |          |      |        - DSN segment delimiter is a period (.) |
*  |          |      |        - Each segment is 1-8 bytes             |
*  |          |      |        - Byte 1-1 in segment is A-Z #@$        |
*  |          |      |        - Byte 2-8 in segment is A-Z #@$ 0-9    |
*  |          |      |        - Case-sensitive, upper case            |
*  |          |      |------------------------------------------------|
*  |          |      |      |                                         |
*  |          |      |      | RC=4000  &ERRMSG='FALSE'                |
*  |          |      |      | RC=4001  &ERRMSG='TRUE'                 |
*  |          |      |      |                                         |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(A.A1234567)                     /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 ISDSN MYDATA                            /* ISDSN         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =4001
*              ERRMSG   ='TRUE                          '
*              MYDATA   ='A.A1234567'
*              MYDATAL  =10
*              MYRESULT =''
*              MYRESULTL=0
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*13| ECHO     | req  | no   | Display variable VAR1 value on screen.  |
*  |          |      |      |                                         |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(my data to display.)            /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 ECHO MYDATA                             /* ECHO          */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='MY DATA TO DISPLAY.'
*              MYDATAL  =19
*              MYRESULT =''
*              MYRESULTL=0
*              CUTIL00 ECHO of MYRESULT =MY DATA TO DISPLAY.
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*14| ECHOQ    | req  | no   | Display variable VAR1 value on screen   |
*  |          |      |      | with content in quotes.                 |
*  |          |      |      |                                         |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(my data to display.)            /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 ECHOQ MYDATA                            /* ECHOQ         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='MY DATA TO DISPLAY.'
*              MYDATAL  =19
*              MYRESULT =''
*              MYRESULTL=0
*              CUTIL00 ECHOQ of MYRESULT ='MY DATA TO DISPLAY.'
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*15| REVRS    | req  | opt  | Reverse letters and numbers in variable |
*  |          |      |      | VAR1.                                   |
*  |          |      |      |                                         |
*  |          |      |      | A-Z translate to Z-A                    |
*  |          |      |      | a-z translate to z-a                    |
*  |          |      |      | 0-9 translate to 9-0                    |
*  |          |      |      | Others translate to blank (space)       |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(ABCDEFGHIJKLM96%QRS)            /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 REVRS  MYDATA MYRESULT                  /* REVRS         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='ABCDEFGHIJKLM96%QRS'
*              MYDATAL  =19
*              MYRESULT ='ZYXWVUTSRQPON03 JIH'
*              MYRESULTL=19
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*16| UPPERC   | req  | opt  | Translate content to uppercase in       |
*  |          |      |      | variable VAR1.                          |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(abCdeFgHIJklmnopqrS)            /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 UPPERC MYDATA                           /* UPPERC        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*  CUTIL00 ECHOQ  MYRESULT                         /* ECHOQ         */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='ABCDEFGHIJKLMNOPQRS'
*              MYDATAL  =19
*              MYRESULT =''
*              MYRESULTL=0
*              CUTIL00 ECHOQ of MYRESULT ='ABCDEFGHIJKLMNOPQRS'
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*17| LOWERC   | req  | opt  | Translate content to lowercase in       |
*  |          |      |      | variable VAR1.                          |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(abCdeFgHIJklmnopqrS)           /* INIT MYDATA    */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 LOWERC MYDATA MYRESULT                  /* LOWERC        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*  CUTIL00 ECHOQ  MYRESULT                         /* ECHOQ         */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='ABCDEFGHIJKLMNOPQRS'
*              MYDATAL  =19
*              MYRESULT ='ABCDEFGHIJKLMNOPQRS'
*              MYRESULTL=19
*              CUTIL00 ECHOQ of MYRESULT ='ABCDEFGHIJKLMNOPQRS'
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*18| COUNT    | req  | req  | Count occurrences of a search string    | LB1105z
*  |          |      |      | in variable VAR1.                       |
*18| FINDALL  |      |      |                                         |
*  | (alias)  |      |      | RC=occurrence count                     | LB1105z
*  |          |      |      |    occurrence count of 0 = NOT FOUND    | LB1105z
*  |          |      |      |                                         |
*  |          |      |      | VAR2 contains Search Parms -------------|
*  |          |      |      | Format: "sssssss",bbb,eee               |
*  |          |      |      |                                         |
*  |          |      |      |  s - search string in double quotes     |
*  |          |      |      |  b - optional, beginning search position| LB1100z
*  |          |      |      |      1-3 numeric digits, default=1      |
*  |          |      |      |      If b > VAR1 len, b = 1.            | LB1105z
*  |          |      |      |  e - optional, ending   search position |
*  |          |      |      |      1-3 numeric digits, default=end    |
*  |          |      |      |      If e > VAR1 len, e = VAR1 len.     | LB1105z
*  |          |      |      |  Parms separated by commas (,)          |
*  |          |      |      |                                         |
*  |          |      |      |  Note: If VAR2 not specified, defaults  |
*  |          |      |      |        are applied including search     |
*  |          |      |      |        string of one BLANK (" ")        |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(My name is Dino, big Dino.)     /* INIT MYDATA   */
*  SET SRCHPRMS = &STR("Dino")                     /* INIT SRCHPRMS */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET SRCHPRMSL=                                  /* INIT SRCHPRMSL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 COUNT MYDATA SRCHPRMS                   /* COUNT         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =2
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='MY NAME IS DINO, BIG DINO.'
*              MYDATAL  =26
*              SRCHPRMS ='"DINO"'
*              SRCHPRMSL=6
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*19| FIND     | req  | req  | Find starting position of FIRST search  |
*  |          |      |      | string occurrence in variable VAR1.     | LB1105z
*05| INDEX    |      |      |                                         |
*  | (alias)  |      |      | RC=position, relative to 1              |
*  |          |      |      |    position of 0 = NOT FOUND            |
*  |          |      |      |                                         |
*  |          |      |      | VAR2 contains Search Parms -------------|
*  |          |      |      | Format: "sssssss",bbb,eee               |
*  |          |      |      |                                         |
*  |          |      |      |  s - search string in double quotes     |
*  |          |      |      |  b - optional, beginning search position| LB1100z
*  |          |      |      |      1-3 numeric digits, default=1      |
*  |          |      |      |      If b > VAR1 len, b = 1.            | LB1105z
*  |          |      |      |  e - optional, ending   search position |
*  |          |      |      |      1-3 numeric digits, default=end    |
*  |          |      |      |      If e > VAR1 len, e = VAR1 len.     | LB1105z
*  |          |      |      |  Parms separated by commas (,):         |
*  |          |      |      |                                         | LB1100z
*  |          |      |      |  Note: If VAR2 not specified, defaults  | LB1100z
*  |          |      |      |        are applied including search     | LB1100z
*  |          |      |      |        string of one BLANK (" ")        | LB1100z
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(My name is Dino, big Dino.)     /* INIT MYDATA   */
*  SET SRCHPRMS = &STR("Dino")                     /* INIT SRCHPRMS */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET SRCHPRMSL=                                  /* INIT SRCHPRMSL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 FIND  MYDATA SRCHPRMS                   /* FIND          */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =12
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='MY NAME IS DINO, BIG DINO.'
*              MYDATAL  =26
*              SRCHPRMS ='"DINO"'
*              SRCHPRMSL=6
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*20| FINDL    | req  | req  | Find starting position of LAST search   |
*  |          |      |      | string occurrence in variable VAR1.     | LB1105z
*06| INDEXB   |      |      |                                         |
*  | (alias)  |      |      | RC=position, relative to 1              |
*  |          |      |      |    position of 0 = NOT FOUND            |
*  |          |      |      |                                         |
*  |          |      |      | VAR2 contains Search Parms -------------|
*  |          |      |      | Format: "sssssss",bbb,eee               |
*  |          |      |      |                                         |
*  |          |      |      |  s - search string in double quotes     |
*  |          |      |      |  b - optional, beginning search position| LB1100z
*  |          |      |      |      1-3 numeric digits, default=1      |
*  |          |      |      |      If b > VAR1 len, b = 1.            | LB1105z
*  |          |      |      |  e - optional, ending   search position |
*  |          |      |      |      1-3 numeric digits, default=end    |
*  |          |      |      |      If e > VAR1 len, e = VAR1 len.     | LB1105z
*  |          |      |      |  Parms separated by commas (,):         |
*  |          |      |      |                                         | LB1100z
*  |          |      |      |  Note: If VAR2 not specified, defaults  | LB1100z
*  |          |      |      |        are applied including search     | LB1100z
*  |          |      |      |        string of one BLANK (" ")        | LB1100z
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(My name is Dino, big Dino.)     /* INIT MYDATA   */
*  SET SRCHPRMS = &STR("Dino")                     /* INIT SRCHPRMS */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET SRCHPRMSL=                                  /* INIT SRCHPRMSL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 FINDL MYDATA SRCHPRMS                   /* FINDL         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =22
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='MY NAME IS DINO, BIG DINO.'
*              MYDATAL  =26
*              SRCHPRMS ='"DINO"'
*              SRCHPRMSL=6
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*21| CENTER   | req  | opt  | Center content in variable VAR1.        |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(Center this T E X T ...      )  /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 CENTER MYDATA MYRESULT                  /* CENTER        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='CENTER THIS T E X T ...       '
*              MYDATAL  =30
*              MYRESULT ='   CENTER THIS T E X T ...    '
*              MYRESULTL=30
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*22| LJUST    | req  | opt  | Left justify content in variable VAR1.  |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR( JUSTIFY this text    )         /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 LJUST MYDATA MYRESULT                   /* LJUST         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   =' JUSTIFY THIS TEXT    '
*              MYDATAL  =22
*              MYRESULT ='JUSTIFY THIS TEXT     '
*              MYRESULTL=22
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*23| RJUST    | req  | opt  | Right justify content in variable VAR1. |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR( JUSTIFY this text    )         /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 RJUST MYDATA MYRESULT                   /* RJUST         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   =' JUSTIFY THIS TEXT    '
*              MYDATAL  =22
*              MYRESULT ='     JUSTIFY THIS TEXT'
*              MYRESULTL=22
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*24| ZFILL    | req  | opt  | Zero fill after content is right        |
*  |          |      |      | justified in variable VAR1.             |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(12   )                          /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 ZFILL MYDATA MYRESULT                   /* ZFILL         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='12   '
*              MYDATAL  =5
*              MYRESULT ='00012'
*              MYRESULTL=5
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*25| WORDS    | req  | no   | Number of words (contiguous non-blank   |
*  |          |      |      | groups) in variable VAR1.               |
*  |          |      |      |                                         |
*  |          |      |      | RC=count                                |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(  one two three33 four., five ) /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 WORDS MYDATA                            /* WORDS         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =5
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='  ONE TWO THREE33 FOUR., FIVE '
*              MYDATAL  =30
*              MYRESULT =''
*              MYRESULTL=0
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*26| GEN#     | req  | no   | Generate a 3-digit number and return    |
*  |          |      |      | in variable VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=0                                    /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 GEN# MYDATA                             /* GEN#          */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='515'
*              MYDATAL  =3
*              MYRESULT =''
*              MYRESULTL=0
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*27| DD2DSN   | req  | req  | Dataset name associated with DD name.   |
*  |          |      |      |                                         |
*  |          |      |      | RC=8 DD not found                       |
*  |          |      |      |                                         |
*  |          |      |      | VAR1 contains DD name                   |
*  |          |      |      |      note: Case-sensitive, upper case   |
*  |          |      |      | VAR2 contains Dataset name as result    |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(TEMPJCL)                        /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 DD2DSN MYDATA MYRESULT                  /* DD2DSN        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='SYSPROC'
*              MYDATAL  =7
*              MYRESULT ='SYS2.CMDPROC'
*              MYRESULTL=12
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*28| JOBINFO  | req  | no   | Return JOB INFORMATION in variable      |
*  |          |      |      | VAR1 as follows:                        |
*  |          |      |      |                                         |
*  |          |      |      | VAR1 Pos    Batch TSO    Online TSO     |
*  |          |      |      | ----------  ---------    ----------     |
*  |          |      |      | byte 01-08  JOB NAME     TSO Userid     |
*  |          |      |      | byte 09-16  JOB PROC     TSO PROC       |
*  |          |      |      | byte 17-24  JOB STEP     TSO LOGON PROC |
*  --------------------------------------------------------------------
*  SET MYDATA=                                     /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 JOBINFO MYDATA                          /* JOBINFO       */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='HERC01  IKJACCNTTSOLOGON'
*              MYDATAL  =24
*              MYRESULT =''
*              MYRESULTL=0
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*29| DAYSMM   | req  | no   | Number of days in month (MMCCYY).       |
*  |          |      |      |                                         |
*  |          |      |      | RC=Number of days (1-31)                |
*  |          |      |      |                                         |
*  |          |      |      | VAR1 contains month in the              |
*  |          |      |      | format of MMCCYY.                       |
*  |          |      |      |                                         |
*  |          |      |      | Leap year is considered.                |
*  |          |      |      |                                         |
*  |          |      |      | For example, if MM is FEB and CCYY is   |
*  |          |      |      | a leap year, number of days are 29,     |
*  |          |      |      | else, 28.                               |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(022016)                         /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 DAYSMM MYDATA                           /* DAYSMM        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =29
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='022016'
*              MYDATAL  =6
*              MYRESULT =''
*              MYRESULTL=
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*30| DAYSYY   | req  | no   | Number of days in year (CCYY).          |
*  |          |      |      |                                         |
*  |          |      |      | RC=Number of days (365 or 366)          |
*  |          |      |      |                                         |
*  |          |      |      | VAR1 contains century and year in the   | LB1100z
*  |          |      |      | format of CCYY.                         |
*  |          |      |      |                                         |
*  |          |      |      | Leap year is considered.                |
*  |          |      |      |                                         |
*  |          |      |      | For example, if CCYY is a leap year,    |
*  |          |      |      | number of days is 366, else 365.        |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(2020)                           /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 DAYSYY MYDATA                           /* DAYSYY        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =366
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='2020'
*              MYDATAL  =4
*              MYRESULT =''
*              MYRESULTL=
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*31| ISLEAP   | req  | no   | Test for leap year of variable VAR1     |
*  |          |      |      | in the format (CCYY) returning a        |
*  |          |      |      | true-false indication.                  |
*  |          |      |      |                                         |
*  |          |      |      | RC=4000  &ERRMSG='FALSE'                |
*  |          |      |      | RC=4001  &ERRMSG='TRUE'                 |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(2020)                           /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 ISLEAP MYDATA                           /* ISLEAP        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =4001
*              ERRMSG   ='TRUE                          '
*              MYDATA   ='2020'
*              MYDATAL  =4
*              MYRESULT =''
*              MYRESULTL=
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*32| CYJ-D8   | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format CCYYJJJ to monthname DD, CCYY    |
*  |          |      |      | where:                                  |
*  |          |      |      | monthname - Full month name (see below) |
*  |          |      |      | DD        - 2 digit date (01-31)        |
*  |          |      |      | CCYY      - 4 digit century and year    |
*  |          |      |      |                                         |
*  |          |      |      | Monthnames:                             |
*  |          |      |      | 01-January   02-February  03-March      |
*  |          |      |      | 04-April     05-May       06-June       |
*  |          |      |      | 07-July      08-August    09-September  |
*  |          |      |      | 10-October   11-November  12-December   |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(2020061)                        /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 CYJ-D8   MYDATA MYRESULT                /* CYJ-D8        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='2020061'
*              MYDATAL  =7
*              MYRESULT ='MARCH 01, 2020'
*              MYRESULTL=14
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*33| CYJ-DAY  | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format CCYYJJJ to weekday name.         |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Name:                           |
*  |          |      |      | Sunday, Monday, Tuesday, Wednesday      |
*  |          |      |      | Thursday, Friday, Saturday              |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(1981061)                        /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 CYJ-DAY  MYDATA MYRESULT                /* CYJ-DAY       */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='1981061'
*              MYDATAL  =7
*              MYRESULT ='MONDAY'
*              MYRESULTL=6
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*34| CYJ-DOW  | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format CCYYJJJ to weekday number.       |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Number:                         |
*  |          |      |      | 0-6                                     |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Number to Weekday Name:         |
*  |          |      |      | 0-Sunday     1-Monday    2-Tuesday      |
*  |          |      |      | 3-Wednesday  4-Thursday  5-Friday       |
*  |          |      |      | 6-Saturday                              |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(1980061)                        /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 CYJ-DOW  MYDATA MYRESULT                /* CYJ-DOW       */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='1980061'
*              MYDATAL  =7
*              MYRESULT ='6'
*              MYRESULTL=1
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*35| CYJ-MDCY*| req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format CCYYJJJ to MMDDCCYY.             |
*  |          |      |      |                                         |
*  |*MDCY can |      |      | For example-                            |
*  | be in any|      |      | CYJ-MDY  convert CCYYJJJ to MMDDYY      |
*  | order    |      |      | CYJ-CYDM convert CCYYJJJ to CCYYDDMM    |
*  |          |      |      |                                         |
*  |          |      |      | MM - 2 digit month (01-12)              |
*  |          |      |      | DD - 2 digit date  (01-31)              |
*  |          |      |      | CC - 2 digit century                    |
*  |          |      |      | YY - 2 digit year                       |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(2020061)                        /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 CYJ-MDCY MYDATA MYRESULT                /* CYJ-MDCY      */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='2020061'
*              MYDATAL  =7
*              MYRESULT ='03012020'
*              MYRESULTL=8
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*36| JCY-D8   | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format JJJCCYY to monthname DD, CCYY    |
*  |          |      |      | where:                                  |
*  |          |      |      | monthname - Full month name (see below) |
*  |          |      |      | DD        - 2 digit date (01-31)        |
*  |          |      |      | CCYY      - 4 digit century and year    |
*  |          |      |      |                                         |
*  |          |      |      | Monthnames:                             |
*  |          |      |      | 01-January   02-February  03-March      |
*  |          |      |      | 04-April     05-May       06-June       |
*  |          |      |      | 07-July      08-August    09-September  |
*  |          |      |      | 10-October   11-November  12-December   |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(1612017)                        /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 JCY-D8   MYDATA MYRESULT                /* JCY-D8        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='1612017'
*              MYDATAL  =7
*              MYRESULT ='JUNE 10, 2017'
*              MYRESULTL=13
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*37| JCY-DAY  | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format JJJCCYY to weekday name.         |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Name:                           |
*  |          |      |      | Sunday, Monday, Tuesday, Wednesday      |
*  |          |      |      | Thursday, Friday, Saturday              |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(0611981)                        /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 JCY-DAY  MYDATA MYRESULT                /* JCY-DAY       */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='0611981'
*              MYDATAL  =7
*              MYRESULT ='MONDAY'
*              MYRESULTL=6
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*38| JCY-DOW  | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format JJJCCYY to weekday number.       |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Number:                         |
*  |          |      |      | 0-6                                     |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Number to Weekday Name:         |
*  |          |      |      | 0-Sunday     1-Monday    2-Tuesday      |
*  |          |      |      | 3-Wednesday  4-Thursday  5-Friday       |
*  |          |      |      | 6-Saturday                              |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(0611981)                        /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 JCY-DOW  MYDATA MYRESULT                /* JCY-DOW       */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='0611981'
*              MYDATAL  =7
*              MYRESULT ='1'
*              MYRESULTL=1
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*39| JCY-MDCY*| req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format JJJCCYY to MMDDCCYY.             |
*  |          |      |      |                                         |
*  |*MDCY can |      |      | For example-                            |
*  | be in any|      |      | JCY-MDY  convert JJJCCYY to MMDDYY      |
*  | order    |      |      | JCY-CYDM convert JJJCCYY to CCYYDDMM    |
*  |          |      |      |                                         |
*  |          |      |      | MM - 2 digit month (01-12)              |
*  |          |      |      | DD - 2 digit date  (01-31)              |
*  |          |      |      | CC - 2 digit century                    |
*  |          |      |      | YY - 2 digit year                       |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(1612017)                        /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 JCY-MDCY MYDATA MYRESULT                /* JCY-MDCY      */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='1612017'
*              MYDATAL  =7
*              MYRESULT ='06102017'
*              MYRESULTL=8
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*40| MDCY-D8  | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format MMDDCCYY to monthname DD, CCYY   |
*  |          |      |      | where:                                  |
*  |          |      |      | monthname - Full month name (see below) |
*  |          |      |      | DD        - 2 digit date (01-31)        |
*  |          |      |      | CCYY      - 4 digit century and year    |
*  |          |      |      |                                         |
*  |          |      |      | Monthnames:                             |
*  |          |      |      | 01-January   02-February  03-March      |
*  |          |      |      | 04-April     05-May       06-June       |
*  |          |      |      | 07-July      08-August    09-September  |
*  |          |      |      | 10-October   11-November  12-December   |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(12101990)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 MDCY-D8  MYDATA MYRESULT                /* MDCY-D8       */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='12101990'
*              MYDATAL  =8
*              MYRESULT ='DECEMBER 10, 1990'
*              MYRESULTL=17
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*41| MDCY-DAY | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format MMDDCCYY to weekday name.        |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Name:                           |
*  |          |      |      | Sunday, Monday, Tuesday, Wednesday      |
*  |          |      |      | Thursday, Friday, Saturday              |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(12101990)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 MDCY-DAY MYDATA MYRESULT                /* MDCY-DAY      */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='12101990'
*              MYDATAL  =8
*              MYRESULT ='MONDAY'
*              MYRESULTL=6
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*42| MDCY-DOW | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format MMDDCCYY to weekday number.      |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Number:                         |
*  |          |      |      | 0-6                                     |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Number to Weekday Name:         |
*  |          |      |      | 0-Sunday     1-Monday    2-Tuesday      |
*  |          |      |      | 3-Wednesday  4-Thursday  5-Friday       |
*  |          |      |      | 6-Saturday                              |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(12101990)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 MDCY-DOW MYDATA MYRESULT                /* MDCY-DOW      */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='12101990'
*              MYDATAL  =8
*              MYRESULT ='1'
*              MYRESULTL=1
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*43| MDCY-CYJ*| req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format MMDDCCYY to CCYYJJJ.             |
*  |          |      |      |                                         |
*  |*CYJ  can |      |      | For example-                            |
*  | be in any|      |      | MDCY-YJ  convert MMDDCCYY to YYJJJ      |
*  | order    |      |      | MDCY-J   convert MMDDCCYY to JJJ        |
*  |          |      |      |                                         |
*  |          |      |      | JJJ- 3 digit julian day (1-366)         |
*  |          |      |      | CC - 2 digit century                    |
*  |          |      |      | YY - 2 digit year                       |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(12101990)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 MDCY-CYJ MYDATA MYRESULT                /* MDCY-CYJ      */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='12101990'
*              MYDATAL  =8
*              MYRESULT ='1990344'
*              MYRESULTL=7
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*44| DMCY-D8  | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format DDMMCCYY to monthname DD, CCYY   |
*  |          |      |      | where:                                  |
*  |          |      |      | monthname - Full month name (see below) |
*  |          |      |      | DD        - 2 digit date (01-31)        |
*  |          |      |      | CCYY      - 4 digit century and year    |
*  |          |      |      |                                         |
*  |          |      |      | Monthnames:                             |
*  |          |      |      | 01-January   02-February  03-March      |
*  |          |      |      | 04-April     05-May       06-June       |
*  |          |      |      | 07-July      08-August    09-September  |
*  |          |      |      | 10-October   11-November  12-December   |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(15071987)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 DMCY-D8  MYDATA MYRESULT                /* DMCY-D8       */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='15071987'
*              MYDATAL  =8
*              MYRESULT ='JULY 15, 1987'
*              MYRESULTL=13
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*45| DMCY-DAY | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format DDMMCCYY to weekday name.        |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Name:                           |
*  |          |      |      | Sunday, Monday, Tuesday, Wednesday      |
*  |          |      |      | Thursday, Friday, Saturday              |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(15071987)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 DMCY-DAY MYDATA MYRESULT                /* DMCY-DAY      */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='15071987'
*              MYDATAL  =8
*              MYRESULT ='WEDNESDAY'
*              MYRESULTL=9
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*46| DMCY-DOW | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format DDMMCCYY to weekday number.      |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Number:                         |
*  |          |      |      | 0-6                                     |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Number to Weekday Name:         |
*  |          |      |      | 0-Sunday     1-Monday    2-Tuesday      |
*  |          |      |      | 3-Wednesday  4-Thursday  5-Friday       |
*  |          |      |      | 6-Saturday                              |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(15071987)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 DMCY-DOW MYDATA MYRESULT                /* DMCY-DOW      */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='15071987'
*              MYDATAL  =8
*              MYRESULT ='3'
*              MYRESULTL=1
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*47| DMCY-CYJ*| req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format DDMMCCYY to CCYYJJJ.             |
*  |          |      |      |                                         |
*  |*CYJ  can |      |      | For example-                            |
*  | be in any|      |      | DMCY-YJ  convert DDMMCCYY to YYJJJ      |
*  | order    |      |      | DMCY-J   convert DDMMCCYY to JJJ        |
*  |          |      |      |                                         |
*  |          |      |      | JJJ- 3 digit julian day (1-366)         |
*  |          |      |      | CC - 2 digit century                    |
*  |          |      |      | YY - 2 digit year                       |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(15071987)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 DMCY-CYJ MYDATA MYRESULT                /* DMCY-CYJ      */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='15071987'
*              MYDATAL  =8
*              MYRESULT ='1987196'
*              MYRESULTL=7
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*48| CYMD-D8  | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format CCYYMMDD to monthname DD, CCYY   |
*  |          |      |      | where:                                  |
*  |          |      |      | monthname - Full month name (see below) |
*  |          |      |      | DD        - 2 digit date (01-31)        |
*  |          |      |      | CCYY      - 4 digit century and year    |
*  |          |      |      |                                         |
*  |          |      |      | Monthnames:                             |
*  |          |      |      | 01-January   02-February  03-March      |
*  |          |      |      | 04-April     05-May       06-June       |
*  |          |      |      | 07-July      08-August    09-September  |
*  |          |      |      | 10-October   11-November  12-December   |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(19951231)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 CYMD-D8  MYDATA MYRESULT                /* CYMD-D8       */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='19951231'
*              MYDATAL  =8
*              MYRESULT ='DECEMBER 31, 1995'
*              MYRESULTL=17
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*49| CYMD-DAY | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format CCYYMMDD to weekday name.        |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Name:                           |
*  |          |      |      | Sunday, Monday, Tuesday, Wednesday      |
*  |          |      |      | Thursday, Friday, Saturday              |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(19951231)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 CYMD-DAY MYDATA MYRESULT                /* CYMD-DAY      */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='19951231'
*              MYDATAL  =8
*              MYRESULT ='SUNDAY'
*              MYRESULTL=6
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*50| CYMD-DOW | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format CCYYMMDD to weekday number.      |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Number:                         |
*  |          |      |      | 0-6                                     |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Number to Weekday Name:         |
*  |          |      |      | 0-Sunday     1-Monday    2-Tuesday      |
*  |          |      |      | 3-Wednesday  4-Thursday  5-Friday       |
*  |          |      |      | 6-Saturday                              |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(19951231)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 CYMD-DOW MYDATA MYRESULT                /* CYMD-DOW      */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='19951231'
*              MYDATAL  =8
*              MYRESULT ='0'
*              MYRESULTL=1
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*51| CYMD-CYJ*| req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format CCYYMMDD to CCYYJJJ.             |
*  |          |      |      |                                         |
*  |*CYJ  can |      |      | For example-                            |
*  | be in any|      |      | CYMD-YJ  convert CCYYMMDD to YYJJJ      |
*  | order    |      |      | CYMD-J   convert CCYYMMDD to JJJ        |
*  |          |      |      |                                         |
*  |          |      |      | JJJ- 3 digit julian day (1-366)         |
*  |          |      |      | CC - 2 digit century                    |
*  |          |      |      | YY - 2 digit year                       |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(19951231)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 CYMD-CYJ MYDATA MYRESULT                /* CYMD-CYJ      */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='19951231'
*              MYDATAL  =8
*              MYRESULT ='1995365'
*              MYRESULTL=7
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*52| CYDM-D8  | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format CCYYDDMM to monthname DD, CCYY   |
*  |          |      |      | where:                                  |
*  |          |      |      | monthname - Full month name (see below) |
*  |          |      |      | DD        - 2 digit date (01-31)        |
*  |          |      |      | CCYY      - 4 digit century and year    |
*  |          |      |      |                                         |
*  |          |      |      | Monthnames:                             |
*  |          |      |      | 01-January   02-February  03-March      |
*  |          |      |      | 04-April     05-May       06-June       |
*  |          |      |      | 07-July      08-August    09-September  |
*  |          |      |      | 10-October   11-November  12-December   |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(19951605)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 CYDM-D8  MYDATA MYRESULT                /* CYDM-D8       */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='19951605'
*              MYDATAL  =8
*              MYRESULT ='MAY 16, 1995'
*              MYRESULTL=12
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*53| CYDM-DAY | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format CCYYDDMM to weekday name.        |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Name:                           |
*  |          |      |      | Sunday, Monday, Tuesday, Wednesday      |
*  |          |      |      | Thursday, Friday, Saturday              |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(19951605)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 CYDM-DAY MYDATA MYRESULT                /* CYDM-DAY      */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='19951605'
*              MYDATAL  =8
*              MYRESULT ='TUESDAY'
*              MYRESULTL=7
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*54| CYDM-DOW | req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format CCYYDDMM to weekday number.      |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Number:                         |
*  |          |      |      | 0-6                                     |
*  |          |      |      |                                         |
*  |          |      |      | Weekday Number to Weekday Name:         |
*  |          |      |      | 0-Sunday     1-Monday    2-Tuesday      |
*  |          |      |      | 3-Wednesday  4-Thursday  5-Friday       |
*  |          |      |      | 6-Saturday                              |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(19951605)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 CYDM-DOW MYDATA MYRESULT                /* CYDM-DOW      */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='19951605'
*              MYDATAL  =8
*              MYRESULT ='2'
*              MYRESULTL=1
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*55| CYMD-CYJ*| req  | opt  | Convert date in variable VAR1 in        |
*  |          |      |      | format CCYYDDMM to CCYYJJJ.             |
*  |          |      |      |                                         |
*  |*CYJ  can |      |      | For example-                            |
*  | be in any|      |      | CYDM-YJ  convert CCYYDDMM to YYJJJ      |
*  | order    |      |      | CYDM-J   convert CCYYDDMM to JJJ        |
*  |          |      |      |                                         |
*  |          |      |      | JJJ- 3 digit julian day (1-366)         |
*  |          |      |      | CC - 2 digit century                    |
*  |          |      |      | YY - 2 digit year                       |
*  |          |      |      |                                         |
*  |          |      |      | If VAR2 is omitted, results will be     |
*  |          |      |      | placed into VAR1.                       |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(19951605)                       /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 CYMD-CYJ MYDATA MYRESULT                /* CYMD-CYJ      */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='19951605'
*              MYDATAL  =8
*              MYRESULT ='1995136'
*              MYRESULTL=7
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*56| FILL     | req  | req  | Fill variable VAR1 with a delimiter     |
*  |          |      |      | value n times.                          |
*  |          |      |      |                                         |
*  |          |      |      | VAR1 - variable for result              |
*  |          |      |      |                                         |
*  |          |      |      | VAR2 contains Fill Parms ---------------|
*  |          |      |      | Format: cnnn                            |
*  |          |      |      |                                         |
*  |          |      |      |  c - single byte character              |
*  |          |      |      |      default=blank                      | LB1100z
*  |          |      |      |  n - length of fill content, 1-3 digits |
*  |          |      |      |      valid length values, 1-256         |
*  |          |      |      |      default=1                          |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(02281980)                       /* INIT MYDATA   */
*  SET MYPARMS =&STR(*20)                          /* INIT MYPARMS  */  LB1100z
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYPARMSL =                                  /* INIT MYPARMSL */  LB1100z
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 FILL MYDATA MYPARMS                     /* FILL          */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='********************'
*              MYDATAL  =20
*              MYPARMS  ='*20'
*              MYPARMSL =3
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*57| LSTRIP   | req  | opt  | Remove leading delimiter from variable  |
*  |          |      |      | VAR1.                                   |
*  |          |      |      |                                         |
*  |          |      |      | VAR1 will contain result                |
*  |          |      |      |                                         |
*  |          |      |      | VAR2 contains delimiter ----------------|
*  |          |      |      | Format: c                               |
*  |          |      |      |                                         |
*  |          |      |      |  c - single byte character delimiter    |
*  |          |      |      |      default=BLANK, if not specified    |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(,,28,BC,)                       /* INIT MYDATA   */
*  SET MYDELIM=&STR(,)                             /* INIT MYDELIM  */  LB1100z
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYDELIML=                                   /* INIT MYDELIML */  LB1100z
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 LSTRIP MYDATA MYDELIM                   /* LSTRIP        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='28,BC,'
*              MYDATAL  =6
*              MYDELIM  =','
*              MYDELIML =1
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*58| RSTRIP   | req  | opt  | Remove trailing delimiter from variable |
*  |          |      |      | VAR1.                                   |
*  |          |      |      |                                         |
*  |          |      |      | VAR1 will contain result                |
*  |          |      |      |                                         |
*  |          |      |      | VAR2 contains delimiter ----------------|
*  |          |      |      | Format: c                               |
*  |          |      |      |                                         |
*  |          |      |      |  c - single byte character delimiter    |
*  |          |      |      |      default=BLANK, if not specified    |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(,,28,BC,)                       /* INIT MYDATA   */
*  SET MYDELIM =&STR(,)                            /* INIT MYDELIM  */  LB1100z
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYDELIML =                                  /* INIT MYDELIML */  LB1100z
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 RSTRIP MYDATA MYDELIM                   /* RSTRIP        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   =',,28,BC'
*              MYDATAL  =7
*              MYDELIM  =','
*              MYDELIML =1
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*59| STRIP    | req  | opt  | Remove leading and trailing delimiter   |
*  |          |      |      | from variable VAR1.                     |
*  |          |      |      |                                         |
*  |          |      |      | VAR1 will contain result                |
*  |          |      |      |                                         |
*  |          |      |      | VAR2 contains delimiter ----------------|
*  |          |      |      | Format: c                               |
*  |          |      |      |                                         |
*  |          |      |      |  c - single byte character delimiter    |
*  |          |      |      |      default=BLANK, if not specified    |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(,,28,BC,)                       /* INIT MYDATA   */
*  SET MYDELIM =&STR(,)                            /* INIT MYDELIM  */  LB1100z
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYDELIML =                                  /* INIT MYDELIML */  LB1100z
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 STRIP MYDATA MYDELIM                    /* STRIP         */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='28,BC'
*              MYDATAL  =5
*              MYDELIM  =','
*              MYDELIML =1
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*60| STRIP$   | req  | opt  | Remove all delimiter values from        |
*  |          |      |      | variable VAR1.                          |
*  |          |      |      |                                         |
*  |          |      |      | VAR1 will contain result                |
*  |          |      |      |                                         |
*  |          |      |      | VAR2 contains delimiter ----------------|
*  |          |      |      | Format: c                               |
*  |          |      |      |                                         |
*  |          |      |      |  c - single byte character delimiter    |
*  |          |      |      |      default=BLANK, if not specified    |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(,,28,BC,)                       /* INIT MYDATA   */
*  SET MYDELIM =&STR(,)                            /* INIT MYDELIM  */  LB1100z
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYDELIML =                                  /* INIT MYDELIML */  LB1100z
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 STRIP$ MYDATA MYDELIM                   /* STRIP$        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='28BC'
*              MYDATAL  =4
*              MYDELIM  =','
*              MYDELIML =1
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*61| CONCAT   | req  | req  | Concatenate VAR1 and VAR2 into variable |
*  |          |      |      | VAR1.                                   |
*  |          |      |      |                                         |
*  |          |      |      |                                         |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(String data 1.)                 /* INIT MYDATA   */
*  SET MYRESULT=&STR(A result string 2.)           /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 CONCAT MYDATA MYRESULT                  /* CONCAT        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='String data 1.A result string 2.'
*              MYDATAL  =32
*              MYRESULT ='A result string 2.'
*              MYRESULTL=18
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*62| UNSTR    | req  | opt  | Unstring content in variable VAR1 into  |
*  |          |      |      | separate variables (VAR1n) based on a   |
*  |          |      |      | 1-byte delimiter.                       |
*  |          |      |      |                                         |
*  |          |      |      | VAR1 contains content for unstring.     |
*  |          |      |      |                                         |
*  |          |      |      | VAR2 contains delimiter ----------------|
*  |          |      |      | Format: c                               |
*  |          |      |      |                                         |
*  |          |      |      |  c - single byte character delimiter    |
*  |          |      |      |      default=BLANK, if not specified    |
*  |          |      |      |                                         |
*  |          |      |      | VAR10 contains number of variables      |
*  |          |      |      |                                         |
*  |          |      |      | VAR10,1,2,3,4,5,6,7,8,9,10,11 to 99     |
*  |          |      |      | are possible variable names with        |
*  |          |      |      | representing unstringed values.         |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(1234567 AND SO ON . I AM 78 LONG. )  /* MYDATA   */
*  SET MYRESULT=&STR(7)                            /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 UNSTR  MYDATA MYRESULT                  /* UNSTR         */  LB1100z
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='1234567 AND SO ON . I AM 78 LONG. '
*              MYDATAL  =34
*              MYRESULT ='7'
*              MYRESULTL=1
*              MYDATA0 = '003'
*              MYDATA1 = '123456'
*              MYDATA2 = ' AND SO ON . I AM '
*              MYDATA3 = '8 LONG. '
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*63| REPLACE  | req  | req  | Replace substring in string             | LB1100z
*  |          |      |      |                                         |
*  |          |      |      | RC=occurrence count                     | LB1105z
*  |          |      |      |    occurrence count of 0 = NOT FOUND    | LB1105z
*  |          |      |      |                                         |
*  |          |      |      | VAR1 contains content for replace.      |
*  |          |      |      |                                         |
*  |          |      |      | VAR2 contains Search Parms -------------|
*  |          |      |      | Format: "ssss","rrrrr",bbb,eee          |
*  |          |      |      |                                         |
*  |          |      |      |  s - search string in double quotes     |
*  |          |      |      |  r - replace string in double quotes    |
*  |          |      |      |  b - optional, beginning search position| LB1100z
*  |          |      |      |      1-3 numeric digits, default=1      |
*  |          |      |      |  e - optional, ending   search position |
*  |          |      |      |      1-3 numeric digits, default=end    |
*  |          |      |      |  Parms separated by commas (,):         |
*  |          |      |      |                                         |
*  |          |      |      | RC=4032  Start > End                    | LB1105f
*  |          |      |      |          one of the following errors:   | LB1105f
*  |          |      |      |          - start pos > end pos          | LB1105f
*  |          |      |      |          - start pos > VAR1 len         | LB1105f
*  |          |      |      |          - end   pos > VAR1 len         | LB1105f
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(1234567 AND SO ON . I AM 7879.) /* INIT MYDATA   */
*  SET MYRLPARM=&STR("SO ON ","890123456--")       /* INIT MYRLPARM */  LB1105z
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRLPARML=                                  /* INIT MYRLPARML*/  LB1105z
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 REPLACE MYDATA MYRLPARM                 /* REPLACE       */  LB1105z
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='1234567 AND 890123456--. I AM 7879.'
*              MYDATAL  =35
*              MYRLPARM ='"SO ON ","890123456--"'                       LB1105z
*              MYRLPARML=22                                             LB1105z
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*64| VEXIST   | req  | no   | Test for existence of VAR1 variable.    |
*  |          |      |      | Results in return code (RC).            |
*  |          |      |      |                                         |
*  |          |      |      | RC=nnn  Length of variable              |
*  |          |      |      | RC=4000 Variable NOT found              |
*  |          |      |      |                                         |
*  |          |      |      | No content of variable is returned.     |
*  |          |      |      |                                         |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(TEST)                           /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 VEXIST  MYDATA                          /* VEXIST        */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =4
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='TEST'
*              MYDATAL  =4
*              MYRESULT =''
*              MYRESULTL=
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*65| TDSN     | req  | no   | Generate temporary DSN nodes formatted  |
*  |          |      |      | as 'Dyyddd.Thhmmss' concatenated to     |
*  |          |      |      | end of VAR1 variable.                   |
*  |          |      |      |                                         |
*  |          |      |      | Note: A period '.' may be placed before |
*  |          |      |      | the date and time nodes should one not  |
*  |          |      |      | exist in the provided variable VAR1.    |
*  |          |      |      |                                         |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(HERC01.CARDS)                   /* INIT MYDATA   */  LB1100z
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 TDSN    MYDATA                          /* TDSN          */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='HERC01.CARDS.D19231.T183011'
*              MYDATAL  =27
*              MYRESULT =''
*              MYRESULTL=
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*66| NOW      | req  | no   | Current system date and time returned   |
*  |          |      |      | in variable VAR01.                      |
*  |          |      |      |                                         |
*  |          |      |      | Format:                                 |
*  |          |      |      | 'MM/DD/CCYY.JJJ HH:MM:SS.TT N nnnnnnnnn'|
*  |          |      |      |   where -                               | LB1100z
*  |          |      |      |    MM/DD/CCYY    Date                   | LB1100z
*  |          |      |      |    HH:MM:SS.TT   Day of Week number     | LB1100z
*  |          |      |      |    N             Day of Week number     | LB1100z
*  |          |      |      |    nnnnnnnnn     Day Name               | LB1100z
*  --------------------------------------------------------------------
*  SET MYDATA=                                     /* INIT MYDATA   */
*  SET MYRESULT=                                   /* INIT MYRESULT */
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 NOW     MYDATA                          /* NOW           */
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='03/29/2020.089 21:07:26.42 0 SUNDAY   '
*              MYDATAL  =38
*              MYRESULT =''
*              MYRESULTL=
*
         EJECT
*  --------------------------------------------------------------------
*  | Function | VAR1 | VAR2 | Description                             |
*  +----------+------+------+-----------------------------------------+
*67| PAD      | req  | req  | Pad current string with delimiter       |
*  |          |      |      | for a given length.                     |
*  |          |      |      |                                         |
*  |          |      |      | VAR1 - variable for result              |
*  |          |      |      |                                         |
*  |          |      |      | VAR2 contains PAD Parms ----------------| LB1100z
*  |          |      |      | Format: cnnn                            |
*  |          |      |      |                                         |
*  |          |      |      |  c - single byte character              |
*  |          |      |      |      default=blank                      | LB1100z
*  |          |      |      |  n - resulting length, 1-3 digits       |
*  |          |      |      |      valid length values, 1-256         |
*  |          |      |      |      default=1                          |
*  |          |      |      |  Note: No padding will occur when       |
*  |          |      |      |        resulting length less than or    |
*  |          |      |      |        equal to VAR1 length             |
*  |          |      |      |                                         |
*  |          |      |      |                                         |
*  |          |      |      | RC=4040  Invalid Length                 |
*  |          |      |      |          one of the following errors:   |
*  |          |      |      |          - VAR2 contains more than 4    |
*  |          |      |      |            characters per above format  |
*  |          |      |      |          - nnn is zero                  |
*  |          |      |      |          - VAR2 is null                 |
*  |          |      |      |          - resulting string > 256 bytes |
*  --------------------------------------------------------------------
*  SET MYDATA=&STR(PAD ME TO 30 LONG)              /* INIT MYDATA   */  LB1100z
*  SET MYRESULT=&STR(.30)                          /* INIT MYRESULT */  LB1100z
*  SET MYDATAL =                                   /* INIT MYDATAL  */
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/
*  SET ERRMSG  =                                   /* INIT ERRMSG   */
*  CUTIL00 PAD     MYDATA MYRESULT                 /* PAD           */  LB1100z
*  SET RC=&LASTCC                                  /* RETURN CODE   */
*
*    Results:  RC       =0
*              ERRMSG   ='SUCCESSFUL REQUEST            '
*              MYDATA   ='PAD ME TO 30 LONG.............'               LB1100z
*              MYDATAL  =30
*              MYRESULT ='.30'                                          LB1100z
*              MYRESULTL=3
*
         EJECT                                                          LB1100c
*  -------------------------------------------------------------------- LB1100c
*  | Function | VAR1 | VAR2 | Description                             | LB1100c
*  +----------+------+------+-----------------------------------------+ LB1100c
*68| GET1V    | req  | req  | Get single byte from position VAR1(nnn) | LB1100c
*  |          |      |      |                                         | LB1100c
*  |          |      |      | VAR1 - variable                         | LB1100c
*  |          |      |      |                                         | LB1100c
*  |          |      |      | VAR2 contains GET1V Parms --------------| LB1100c
*  |          |      |      | Format: cnnn                            | LB1100c
*  |          |      |      |                                         | LB1100c
*  |          |      |      |  c - single character RETURNED from     | LB1100c
*  |          |      |      |      VAR1 at position n                 | LB1100c
*  |          |      |      |      default=blank                      | LB1100c
*  |          |      |      |  n - position within VAR1, , 1-3 digits | LB1100c
*  |          |      |      |      valid length values, 1-256         | LB1100c
*  |          |      |      |      default=1                          | LB1100c
*  |          |      |      |      max=length of VAR1                 | LB1100c
*  |          |      |      |                                         | LB1100c
*  |          |      |      | RC=4040  Invalid Length                 | LB1100c
*  |          |      |      |          one of the following errors:   | LB1100c
*  |          |      |      |          - nnn more than 3 digits per   | LB1100c
*  |          |      |      |            above format                 | LB1100c
*  |          |      |      |          - nnn is zero                  | LB1100c
*  |          |      |      |          - nnn > VAR1 length            | LB1100c
*  |          |      |      |          - resulting string > 256 bytes | LB1100c
*  -------------------------------------------------------------------- LB1100c
*  SET MYDATA=&STR(THE TABLE TOP IS CLEAR)         /* INIT MYDATA   */  LB1100c
*  SET MYDATAL =                                   /* INIT MYDATAL  */  LB1100c
*  SET MYPEEK  =&STR(x11)                          /* INIT MYPEEK   */  LB1100c
*  SET MYPEEKL=                                    /* INIT MYPEEKL  */  LB1100c
*  SET ERRMSG  =                                   /* INIT ERRMSG   */  LB1100c
*  CUTIL00 GET1V MYDATA MYPEEK                     /* GET1V         */  LB1100c
*  SET RC=&LASTCC                                  /* RETURN CODE   */  LB1100c
*                                                                       LB1100c
*    Results:  RC       =0                                              LB1100c
*              ERRMSG   ='SUCCESSFUL REQUEST            '               LB1100c
*              MYDATA   ='THE TABLE TOP IS CLEAR'                       LB1100c
*              MYDATAL  =22                                             LB1100c
*              MYPEEK   ='T11'                                          LB1100c
*              MYPEEKL  =3                                              LB1100c
*                                                                       LB1100c
         EJECT                                                          LB1100d
*  -------------------------------------------------------------------- LB1100d
*  | Function | VAR1 | VAR2 | Description                             | LB1100d
*  +----------+------+------+-----------------------------------------+ LB1100d
*69| PUT1V    | req  | req  | Put single byte to   position VAR1(nnn) | LB1100d
*  |          |      |      |                                         | LB1100d
*  |          |      |      | VAR1 - variable                         | LB1100d
*  |          |      |      |                                         | LB1100d
*  |          |      |      | VAR2 contains PUT1V Parms --------------| LB1100d
*  |          |      |      | Format: cnnn                            | LB1100d
*  |          |      |      |                                         | LB1100d
*  |          |      |      |  c - single character OVERLAID into     | LB1100d
*  |          |      |      |      VAR1 at position n                 | LB1100d
*  |          |      |      |      default=blank                      | LB1100d
*  |          |      |      |  n - position within VAR1, , 1-3 digits | LB1100d
*  |          |      |      |      valid length values, 1-256         | LB1100d
*  |          |      |      |      default=1                          | LB1100d
*  |          |      |      |      max=length of VAR1                 | LB1100d
*  |          |      |      |                                         | LB1100d
*  |          |      |      | RC=4040  Invalid Length                 | LB1100d
*  |          |      |      |          one of the following errors:   | LB1100d
*  |          |      |      |          - nnn more than 3 digits per   | LB1100d
*  |          |      |      |            above format                 | LB1100d
*  |          |      |      |          - nnn is zero                  | LB1100d
*  |          |      |      |          - nnn > VAR1 length            | LB1100d
*  |          |      |      |          - resulting string > 256 bytes | LB1100d
*  -------------------------------------------------------------------- LB1100d
*  SET MYDATA=&STR(THE TABLE TOP IS CLEAR)         /* INIT MYDATA   */  LB1100d
*  SET MYDATAL =                                   /* INIT MYDATAL  */  LB1100d
*  SET MYPOKE  =&STR(N22)                          /* INIT MYPOKE   */  LB1100d
*  SET MYPOKEL=                                    /* INIT MYPOKEL  */  LB1100d
*  SET ERRMSG  =                                   /* INIT ERRMSG   */  LB1100d
*  CUTIL00 PUT1V MYDATA MYPOKE                     /* PUT1V         */  LB1100d
*  SET RC=&LASTCC                                  /* RETURN CODE   */  LB1100d
*                                                                       LB1100d
*    Results:  RC       =0                                              LB1100d
*              ERRMSG   ='SUCCESSFUL REQUEST            '               LB1100d
*              MYDATA   ='THE TABLE TOP IS CLEAN'                       LB1100d
*              MYDATAL  =22                                             LB1100d
*              MYPEEK   ='N22'                                          LB1100d
*              MYPEEKL  =3                                              LB1100d
*                                                                       LB1100d
         EJECT                                                          LB1100e
*  -------------------------------------------------------------------- LB1100e
*  | Function | VAR1 | VAR2 | Description                             | LB1100e
*  +----------+------+------+-----------------------------------------+ LB1100e
*70| MCAL     | req  | opt  | Monthly calendar given MMDDCCYY.        | LB1100e
*  |          |      |      |                                         | LB1100e
*  |          |      |      | Result is 168 bytes in length and       | LB1100e
*  |          |      |      | may be displayed in 21-byte segments    | LB1100e
*  |          |      |      | to give calendar appearance (8-rows):   | LB1100e
*  |          |      |      |                                         | LB1100e
*  |          |      |      |            0        1         2         | LB1100e
*  |          |      |      |            1...5....0....5....0.        | LB1100e
*  |          |      |      |                                         | LB1100e
*  |          |      |      |          /  DECEMBER 2018____349        | LB1100e
*  |          |      |      |         /    S  M  T  W  T  F  S        | LB1100e
*  |          |      |      |        /     .  .  .  .  .  .  1        | LB1100e
*  |          |      |      |Result {      2  3  4  5  6  7  8        | LB1100e
*  |          |      |      |        \     9 10 11 12 13 14@15        | LB1100e
*  |          |      |      |         \   16 17 18 19 20 21 22        | LB1100e
*  |          |      |      |          \  23 24 25 26 27 28 29        | LB1100e
*  |          |      |      |           \ 30 31                       | LB1100e
*  |          |      |      |                                         | LB1100e
*  |          |      |      | Todays julian date and marking of       | LB1100e
*  |          |      |      | date is part of the result when         | LB1100e
*  |          |      |      | request month and year is current       | LB1100e
*  |          |      |      | month and year.  Otherwise, it is       | LB1100e
*  |          |      |      | not contained in the result.            | LB1100e
*  |          |      |      |                                         | LB1100e
*  |          |      |      | If VAR2 is omitted, results will be     | LB1100e
*  |          |      |      | placed into VAR1.                       | LB1100e
*  |          |      |      |                                         | LB1100e
*  |          |      |      | If requested date is null, todays       | LB1100e
*  |          |      |      | date is used.                           | LB1100e
*  |          |      |      |                                         | LB1100e
*  -------------------------------------------------------------------- LB1100e
*  SET MYDATA=                                     /* INIT MYDATA   */  LB1100e
*  SET MYCALNDR=                                   /* INIT MYCALNDR */  LB1100e
*  SET MYDATAL =                                   /* INIT MYDATAL  */  LB1100e
*  SET MYCALNDRL=                                  /* INIT MYCALNDRL*/  LB1100e
*  SET ERRMSG  =                                   /* INIT ERRMSG   */  LB1100e
*  CUTIL00 MCAL    MYDATA                          /* MCAL          */  LB1100e
*  SET RC=&LASTCC                                  /* RETURN CODE   */  LB1100e
*                                                                       LB1100e
*    Results:  RC       =0                                              LB1100e
*              ERRMSG   ='SUCCESSFUL REQUEST            '               LB1100e
*              MYDATA   ='12152018'                                     LB1100e
*              MYDATAL  =8                                              LB1100e
*              MYCALNDR =calendar as shown above                        LB1100e
*              MYCALNDRL=168                                            LB1100e
*                                                                       LB1100e
*                                                                       LB1100e
         EJECT                                                          LB1100e
*  -------------------------------------------------------------------- LB1105a
*  | Function | VAR1 | VAR2 | Description                             | LB1105a
*  +----------+------+------+-----------------------------------------+ LB1105a
*72| LEN      | req  | no   | Length of data in string                | LB1105a
*  |          |      |      |                                         | LB1105a
*  |          |      |      | VAR1 - variable                         | LB1105a
*  |          |      |      |                                         | LB1105a
*  |          |      |      | VAR2 - ignored                          | LB1105a
*  |          |      |      |                                         | LB1105a
*  |          |      |      | RC=length of data in string             | LB1105a
*  |          |      |      |                                         | LB1105a
*  -------------------------------------------------------------------- LB1105a
*  SET MYDATA=&STR( MY WORD IN THE STRING    )     /* INIT MYDATA   */  LB1105a
*  SET MYDATAL =                                   /* INIT MYDATAL  */  LB1105a
*  SET ERRMSG  =                                   /* INIT ERRMSG   */  LB1105a
*  CUTIL00 LEN   MYDATA                            /* LEN           */  LB1105a
*  SET RC=&LASTCC                                  /* RETURN CODE   */  LB1105a
*                                                                       LB1105a
*    Results:  RC       =22                                             LB1105a
*              ERRMSG   ='SUCCESSFUL REQUEST - LEN      '               LB1105a
*              MYDATA   =' MY WORD IN THE STRING    '                   LB1105a
*              MYDATAL  =26                                             LB1105a
*                                                                       LB1105a
*                                                                       LB1105a
         EJECT                                                          LB1105a
*  -------------------------------------------------------------------- LB1105a
*  | Function | VAR1 | VAR2 | Description                             | LB1105a
*  +----------+------+------+-----------------------------------------+ LB1105a
*73| SLEN     | req  | no   | Length of string                        | LB1105a
*  |          |      |      |                                         | LB1105a
*  |          |      |      | VAR1 - variable                         | LB1105a
*  |          |      |      |                                         | LB1105a
*  |          |      |      | VAR2 - ignored                          | LB1105a
*  |          |      |      |                                         | LB1105a
*  |          |      |      | RC=length of string                     | LB1105a
*  |          |      |      |                                         | LB1105a
*  -------------------------------------------------------------------- LB1105a
*  SET MYDATA=&STR( MY WORD IN THE STRING    )     /* INIT MYDATA   */  LB1105a
*  SET MYDATAL =                                   /* INIT MYDATAL  */  LB1105a
*  SET ERRMSG  =                                   /* INIT ERRMSG   */  LB1105a
*  CUTIL00 LEN   MYDATA                            /* LEN           */  LB1105a
*  SET RC=&LASTCC                                  /* RETURN CODE   */  LB1105a
*                                                                       LB1105a
*    Results:  RC       =26                                             LB1105a
*              ERRMSG   ='SUCCESSFUL REQUEST - LEN      '               LB1105a
*              MYDATA   =' MY WORD IN THE STRING    '                   LB1105a
*              MYDATAL  =26                                             LB1105a
*                                                                       LB1105a
*                                                                       LB1105a
         EJECT                                                          LB1105c
*  -------------------------------------------------------------------- LB1105c
*  | Function | VAR1 | VAR2 | Description                             | LB1105c
*  +----------+------+------+-----------------------------------------+ LB1105c
*74| OVERLAY  | req  | req  | Overlay substring within VAR1.          | LB1105c
*  |          |      |      |                                         | LB1105c
*  |          |      |      | VAR1 contains content for overlay.      | LB1105c
*  |          |      |      |                                         | LB1105c
*  |          |      |      | VAR2 contains Overlay Parms ------------| LB1105c
*  |          |      |      | Format: "oooo",bbb                      | LB1105c
*  |          |      |      |                                         | LB1105c
*  |          |      |      |  o - overlay string in double quotes    | LB1105c
*  |          |      |      |  b - optional, begin overlay position   | LB1105c
*  |          |      |      |      1-3 numeric digits, default=1      | LB1105c
*  |          |      |      |  Parms separated by commas (,):         | LB1105c
*  |          |      |      |                                         | LB1105c
*  |          |      |      | RC=4032  Start > End                    | LB1105c
*  |          |      |      |          one of the following errors:   | LB1105c
*  |          |      |      |          - str start pos > VAR1 len     | LB1105c
*  |          |      |      |          - str len > VAR1 len           | LB1105c
*  |          |      |      |          - str content overflows VAR1   | LB1105c
*  -------------------------------------------------------------------- LB1105c
*  SET MYDATA=&STR( MY TEXT TO BE OVERLAYED OK!! ) /* INIT MYDATA   */  LB1105c
*  SET MYOPARM =&STR("----- ",7)                   /* INIT MYOPARM  */  LB1105c
*  SET MYDATAL =                                   /* INIT MYDATAL  */  LB1105c
*  SET MYOPARML =                                  /* INIT MYOPARML */  LB1105c
*  SET ERRMSG  =                                   /* INIT ERRMSG   */  LB1105c
*  CUTIL00 OVERLAY MYDATA MYOPARM                  /* OVERLAY       */  LB1105c
*  SET RC=&LASTCC                                  /* RETURN CODE   */  LB1105c
*                                                                       LB1105c
*    Results:  RC       =0                                              LB1105c
*              ERRMSG   ='SUCCESSFUL REQUEST            '               LB1105c
*              MYDATA   =' MY TE----- BE OVERLAYED OK!! '               LB1105c
*              MYDATAL  =30                                             LB1105c
*              MYOPARM  ='"----- ",7'                                   LB1105c
*              MYOPARML =10                                             LB1105c
*                                                                       LB1105c
         EJECT                                                          LB1105g
*  -------------------------------------------------------------------- LB1105g
*  | Function | VAR1 | VAR2 | Description                             | LB1105g
*  +----------+------+------+-----------------------------------------+ LB1105g
*75| UTDSN    | req  | no   | Generate temporary user DSN formatted   | LB1105g
*  |          |      |      | as 'USERID.VAR1.Dyyddd.Thhmmss'         | LB1105g
*  |          |      |      |                                         | LB1105g
*  |          |      |      | VAR1 contains content of appending      | LB1105g
*  |          |      |      |      DSN node(s).                       | LB1105g
*  |          |      |      |                                         | LB1105g
*  |          |      |      | USERID is assigned TSO Prefix.          | LB1105g
*  |          |      |      |                                         | LB1105g
*  |          |      |      | If TSO Prefix is blank, USERID is       | LB1105g
*  |          |      |      | assigned TSO Userid.                    | LB1105g
*  |          |      |      |                                         | LB1105g
*  |          |      |      | No MVS DSN validation performed.        | LB1105g
*  |          |      |      |                                         | LB1105g
*  |          |      |      | Note: A period '.' may be placed before | LB1105g
*  |          |      |      | the date and time nodes should one not  | LB1105g
*  |          |      |      | exist in the provided variable VAR1.    | LB1105g
*  |          |      |      |                                         | LB1105g
*  -------------------------------------------------------------------- LB1105g
*  SET MYDATA=&STR(CARDS)                          /* INIT MYDATA   */  LB1105g
*  SET MYRESULT=                                   /* INIT MYRESULT */  LB1105g
*  SET MYDATAL =                                   /* INIT MYDATAL  */  LB1105g
*  SET MYRESULTL=                                  /* INIT MYRESULTL*/  LB1105g
*  SET ERRMSG  =                                   /* INIT ERRMSG   */  LB1105g
*  CUTIL00 UTDSN   MYDATA                          /* TDSN          */  LB1105g
*  SET RC=&LASTCC                                  /* RETURN CODE   */  LB1105g
*                                                                       LB1105g
*    Results:  RC       =0                                              LB1105g
*              ERRMSG   ='SUCCESSFUL REQUEST            '               LB1105g
*              MYDATA   ='HERC01.CARDS.D19231.T183011'                  LB1105g
*              MYDATAL  =27                                             LB1105g
*              MYRESULT =''                                             LB1105g
*              MYRESULTL=                                               LB1105g
*       --  Assuming TSO user is HERC01.                                LB1105g
*                                                                       LB1105g
         EJECT                                                          LB1105h
*  -------------------------------------------------------------------- LB1105h
*  | Function | VAR1 | VAR2 | Description                             | LB1105h
*  +----------+------+------+-----------------------------------------+ LB1105h
*76| TRUNC    | req  | req  | Truncate VAR1 at n length               | LB1105h
*  |          |      |      |                                         | LB1105h
*  |          |      |      | VAR1 - variable                         | LB1105h
*  |          |      |      |                                         | LB1105h
*  |          |      |      | VAR2 contains TRUNC Parms --------------| LB1105h
*  |          |      |      | Format: cnnn                            | LB1105h
*  |          |      |      |                                         | LB1105h
*  |          |      |      |  c - placeholder, any character         | LB1105h
*  |          |      |      |  n - new length of VAR1, 1-3 digits     | LB1105h
*  |          |      |      |      valid length values, 1-256         | LB1105h
*  |          |      |      |      default=1                          | LB1105h
*  |          |      |      |      max=length of VAR1                 | LB1105h
*  |          |      |      |                                         | LB1105h
*  |          |      |      | RC=4040  Invalid Length                 | LB1105h
*  |          |      |      |          one of the following errors:   | LB1105h
*  |          |      |      |          - nnn more than 3 digits per   | LB1105h
*  |          |      |      |            above format                 | LB1105h
*  |          |      |      |          - nnn is zero                  | LB1105h
*  |          |      |      |          - nnn > VAR1 length            | LB1105h
*  |          |      |      |          - resulting string > 256 bytes | LB1105h
*  -------------------------------------------------------------------- LB1105h
*  SET MYDATA=&STR(THE MAN WALKED HERE)            /* INIT MYDATA   */  LB1105h
*  SET MYDATAL =                                   /* INIT MYDATAL  */  LB1105h
*  SET MYTRUNC =&STR(x07)                          /* INIT MYTRUNC  */  LB1105h
*  SET MYTRUNCL =                                  /* INIT MYTRUNCL */  LB1105h
*  SET ERRMSG  =                                   /* INIT ERRMSG   */  LB1105h
*  CUTIL00 TRUNC MYDATA MYTRUNC                    /* TRUNC         */  LB1105h
*  SET RC=&LASTCC                                  /* RETURN CODE   */  LB1105h
*                                                                       LB1105h
*    Results:  RC       =0                                              LB1105h
*              ERRMSG   ='SUCCESSFUL REQUEST            '               LB1105h
*              MYDATA   ='THE MAN'                                      LB1105h
*              MYDATAL  =7                                              LB1105h
*              MYTRUNC  ='x07'                                          LB1105h
*              MYTRUNCL =3                                              LB1105h
*                                                                       LB1105h
         EJECT
*  CUTIL00 Command Examples:
*  ================================================================
*
*    1) CUTIL00 TRIM VAR01 VAR02
*       Trim contents of &VAR01 variable and place results in &VARO2
*       variable.
*
*    2) CUTIL00 TRIM VAR01
*       Trim contents of &VAR01 variable and place results in &VARO1
*       variable.  Note: Original content is replaced.
*
*    3) CUTIL00 TRIM VAR01 VAR02 QUOTE
*       Trim contents of &VAR01 variable and place results in &VARO2
*       variable.  Note: VAR01 content will be delimited by single
*                        quotes (').
*
*
         EJECT                                                          LB1100z
*  Programs Called:                                                     LB1100z
*  ==================================================================   LB1100z
*                                                                       LB1100z
*    o  GETMAIN/FREEMAIN       Working Storage                          LB1100z
*    o  IKJCT441               CLIST variable API                       LB1100z
*    o  CUTILTBL               Tables and Message Processor             LB1100z
*                                                                       LB1100z
*                                                                       LB1100z
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
*  |  R9  |  Working Register                                        |
*  |  R10 |  Base Register 1                                         |
*  |  R11 |  Base Register 2                                         |
*  |  R12 |  Base Register 3                                         |
*  |  R13 |  Address of WORKAREA including SAVEAREA                  |
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
*  | 4000 |  FALSE                                                   |
*  +------+----------------------------------------------------------+
*  | 4001 |  TRUE                                                    |
*  +------+----------------------------------------------------------+
*  | 4002 |  VAR1 not found                                          |
*  +------+----------------------------------------------------------+
*  | 4004 |  No PARM                                                 |
*  +------+----------------------------------------------------------+
*  | 4005 |  WORD GT 8 bytes                                         |
*  +------+----------------------------------------------------------+
*  | 4008 |  CPPL no Parm                                            |
*  +------+----------------------------------------------------------+
*  | 4009 |  Too many WORDS                                          |
*  +------+----------------------------------------------------------+
*  | 4010 |  SETVAR no content                                       |
*  +------+----------------------------------------------------------+
*  | 4011 |  Invalid function                                        |
*  +------+----------------------------------------------------------+
*  | 4012 |  VAR2 not updated                                        |
*  +------+----------------------------------------------------------+
*  | 4014 |  VAR1 GT 256                                             |
*  +------+----------------------------------------------------------+
*  | 4015 |  VAR2 required                                           |
*  +------+----------------------------------------------------------+
*  | 4016 |  VAR2 not found                                          |
*  +------+----------------------------------------------------------+
*  | 4017 |  VAR2 GT 256                                             |
*  +------+----------------------------------------------------------+
*  | 4018 |  VAR2 null                                               |
*  +------+----------------------------------------------------------+
*  | 4019 |  VAR1 null                                               |
*  +------+----------------------------------------------------------+
*  | 4020 |  ERRMSG cannot create updt                               |
*  +------+----------------------------------------------------------+
*  | 4021 |  VAR1 required                                           |
*  +------+----------------------------------------------------------+
*  | 4022 |  VAR2 not required                                       |
*  +------+----------------------------------------------------------+
*  | 4023 |  Result GT 256                                           |
*  +------+----------------------------------------------------------+
*  | 4030 |  No begin "                                              |
*  +------+----------------------------------------------------------+
*  | 4031 |  No end "                                                |
*  +------+----------------------------------------------------------+
*  | 4032 |  Start GT End                                            |
*  +------+----------------------------------------------------------+
*  | 4033 |  Invalid Char                                            |
*  +------+----------------------------------------------------------+
         EJECT
*  Table 2 - Return Codes (continued)
*
*  +------+----------------------------------------------------------+
*  | 4034 |  Not numeric                                             |
*  +------+----------------------------------------------------------+
*  | 4035 |  Too many digits                                         |
*  +------+----------------------------------------------------------+
*  | 4036 |  Null value not allowed                                  |
*  +------+----------------------------------------------------------+
*  | 4037 |  No begin QUOTE                                          |
*  +------+----------------------------------------------------------+
*  | 4038 |  No end QUOTE                                            |
*  +------+----------------------------------------------------------+
*  | 4039 |  No end QUOTE found                                      |
*  +------+----------------------------------------------------------+
*  | 4040 |  Invalid length                                          |
*  +------+----------------------------------------------------------+
*  | 4047 |  Invalid DD                                              |
*  +------+----------------------------------------------------------+
*  | 4048 |  Invalid DOW                                             |  LB1100f
*  +------+----------------------------------------------------------+  LB1100f
*  | 4049 |  Invalid MM                                              |
*  +------+----------------------------------------------------------+
*  | 4050 |  Invalid DD for MM                                       |
*  +------+----------------------------------------------------------+
*  | 4051 |  Invalid JJJ                                             |
*  +------+----------------------------------------------------------+
*  | 4060 |  Call error in CUTILTBL                                  |  LB1100g
*  +------+----------------------------------------------------------+  LB1100g
*  | 4071 |  Invalid function                                        |  LB1100e
*  +------+----------------------------------------------------------+  LB1100e
*  | 4072 |  Invalid function                                        |  LB1100e
*  +------+----------------------------------------------------------+  LB1100e
*  | 4073 |  Invalid function                                        |  LB1100e
*  +------+----------------------------------------------------------+  LB1100e
*  | 4074 |  Invalid function                                        |  LB1100e
*  +------+----------------------------------------------------------+  LB1100e
*  | 4075 |  Invalid function                                        |  LB1100e
*  +------+----------------------------------------------------------+  LB1100e
*  | 4097 |  Cannot load CUTILTBL                                    |  LB1105b
*  +------+----------------------------------------------------------+  LB1105b
*  | 4098 |  Cannot link to IKJCT441                                 |
*  +------+----------------------------------------------------------+
*  Note: -Symbolic Variable &LASTCC contains CUTIL00 return code
         EJECT
*  Macros and SYSLIB Location:
*  ================================================================
*
*  Macro     Description                         Library
*  --------  ----------------------------------  ------------------
*  YREGS     Register Equates                    SYS2.MACLIB
*  IHAPSA    Prefixed Save Area                  SYS1.AMODGEN
*  CVT       Communication Vector Table          SYS1.AMODGEN
*  IKJCPPL   Command Program Parameter Table     SYS1.MACLIB            LB1105z
*  IKJUPT    User Profile Table                  SYS1.MACLIB            LB1105g
*  IKJPSCB   Protected Step Control Block        SYS1.MACLIB            LB1105g
*  IKJTCB    Task Control Block                  SYS1.AMODGEN
*  IEFTIOT1  Task Input/Output Table             SYS1.AMODGEN
*  IEFJFCBN  Job File Control Block              SYS1.AMODGEN
*  IHAASCB   Address Space Control Block         SYS1.AMODGEN
*
*
*  References:
*  ================================================================
*
*  - SC34-2088-2 ISPF Dialog Management MVS (March 1985)
*  - GC28-0646-4 OS/VS2 TSO Command Language Reference (VS2 Rel 3.8)
*
*
         EJECT
*  Change History:                                                      LB1100z
*  ================================================================
*
*  MM/DD/CCYY Version  Name / Description
*  ---------- -------  --------------------------------------------
*  10/05/2021 1.1.05   Larry Belmontes Jr.                              LB1105
*                      - Add function LEN and SLEN                      LB1105a
*                      - Load CUTILTBL and call via BALR                LB1105b
*                      - Add function OVERLAY                           LB1105c
*                      - Correction to REPLACE function                 LB1105f
*                      - Add function UTDSN                             LB1105g
*                      - Add function TRUNC                             LB1105g
*                      - Misc updates to program documentation          LB1105z
*                                                                       LB1105
*  04/10/2021 1.1.00   Larry Belmontes Jr.                              LB1100
*                      - Misc updates to program documentation          LB1100z
*                      - Minimize Literal Pool by changing CLC          LB1100y
*                        instructions to use relavent string length     LB1100y
*                      - Update subroutine S#BD8 to conditionaliy       LB1100a
*                        bypass date formatting for monthname dd, ccyy  LB1100a
*                      - Update subroutine S#MD2J to save               LB1100b
*                        DC#MDAYS (days in month for requested date)    LB1100b
*                      - Add function GET1V                             LB1100c
*                      - Add function PUT1V                             LB1100d
*                      - Add function MCAL and MCALA                    LB1100e
*                      - Consolidate tables into a common module        LB1100
*                        to increase coding base of CUTIL00             LB1100
*                      - DOWTBL moved into CUTILTBL (1DOW)              LB1100f
*                      - MMTBL  moved into CUTILTBL (2MM , 2J2M)        LB1100g
*                      - TRTBL  moved into CUTILTBL (3ULC)              LB1100h
*                      - TREVRS moved into CUTILTBL (3TRV)              LB1100i
*                      - MSGTBL moved into CUTILTBL (9ERR)              LB1100m
*                                                                       LB1100
*  03/20/2020 1.0.00   Larry Belmontes Jr.
*                      - Initial version released to MVS 3.8J
*                        hobbyist public domain
*
*
*
         EJECT
*  Potential Enhancements:                                              LB1100z
*  ================================================================
*
*  01) Hmmm... thinking...
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
CUTIL00  CSECT
         USING CUTIL00,R10,R11,R12 my BASE REGISTER(S)
         PRINT NOGEN
*     * /********************************************************/
*     * /* Save regs and declare base registers R10,R11,R12     */
*     * /* including saving R1 (PARMS) in R2                    */
*     * /* - R1  myPARMS address on entry                       */
*     * /* - R15 myENTRY address on entry                       */
*     * /********************************************************/
         STM   R14,R12,12(R13)     Save registers in HSA
         LR    R10,R15             Load base1 (R10) w R15
         LA    R11,4095(R10)       Load base2 (R11) w/ offset
         LA    R11,1(R11)          Adjust base2
         LA    R12,4095(R11)       Load base3 (R12) w/ offset
         LA    R12,1(R12)          Adjust base3
         LR    R2,R1               Save PARM addr in R2
*     * /********************************************************/
*     * /* Program Eyecatcher                                   */
*     * /********************************************************/
         B     OVRSTAMP            Branch past eye-catcher
MYSTAMP  EQU   *
PGMID    DC    CL8'CUTIL00 '       My Program STAMP
         DC    CL8'MVS3.8J '       OS
PGMVRM   DC    CL8'V1.1.05 '       .Version                             LB1105
         DC    CL8'10052021'       ..date  MMDDCCYY                     LB1105
         DC    CL1' '
PGMDS    DC    CL8'&SYSDATE'       Assemble Date
         DC    CL3' @ '            .and
PGMTS    DC    CL8'&SYSTIME'       ..Time
MYSTAMPL EQU   *-MYSTAMP
         DC    C'Copyright (C) 2020-2021'                               LB1100
         DC    C'Larry Belmontes, Jr. '
         DC    C'https://ShareABitofIT.net/CUTIL00-for-MVS-3-8J'
OVRSTAMP DS    0H
*
         EJECT
*     * /********************************************************/
*     * /* GETMAIN, working storage, using R9                   */
*     * /* - R1  Working register (GETMAIN address)             */
*     * /* - R13 Working Storage DSECT address                  */
*     * /********************************************************/
         PRINT GEN
         GETMAIN   R,LV=WSAREAL    Get Storage, R1 = addr of storage
         LTR   R15,R15             Did we get the storage?
         BNZ   NOMAINS             NO, return with rc in R15
         PRINT NOGEN
*     * /********************************************************/
*     * /* Update Save Areas for caller and calling programs    */
*     * /* - R1  myPARMS address on entry  (restored)           */
*     * /********************************************************/
         ST    R13,4(R1)           Store HSA addr, chain caller > mySA
         ST    R1,8(R13)           Store LSA addr, chain mySA > caller
         LR    R13,R1              R13 = Workarea DSECT Register
         USING WSDSECT,R13         Tell Assembler
         LR    R1,R2               Restore R1 as PARM addr
*     * /********************************************************/
*     * /* Load IKJCT441 Entry Point                            */
*     * /* - R0, R1                   Working Register          */
*     * /********************************************************/
         LOAD  EP=IKJCT441,ERRET=ERR4098
         ST    R0,IKJCT441         Load and Save IKJCT441 Entry Point
         LR    R1,R2               Restore R1 as PARM addr
*     * /********************************************************/      LB1105b
*     * /* Load CUTILTBL Entry Point  (Message Processor)       */      LB1105b
*     * /* - R0, R1 Working Register                            */      LB1105b
*     * /********************************************************/      LB1105b
         LOAD  EP=CUTILTBL,ERRET=ERR4097                                LB1105b
         ST    R0,CUTILTBL         Load and Save CUTILTBL Entry Point   LB1105b
         LR    R1,R2               Restore R1 as PARM addr              LB1105b
*
         EJECT
*     * /********************************************************/
*     * /* Initialize Working Storage                           */
*     * /* - R2, R3                   Working Register          */
*     * /********************************************************/
         MVC   TOKEN,=F'0'         TOKEN
*     * /********************************************************/
*     * /* Initialize VAR pointers                              */
*     * /********************************************************/
         LA    R2,L'NAM1           VAR1 name length
         ST    R2,NAML1
         LA    R2,NAM1             VAR1 name pointer
         ST    R2,NAMP1
         LA    R2,L'VAL1           VAR1 value length
         ST    R2,VALL1
         LA    R2,VAL1             VAR1 value pointer
         ST    R2,VALP1
*
         LA    R2,L'NAM2           VAR2 name length
         ST    R2,NAML2
         LA    R2,NAM2             VAR2 name pointer
         ST    R2,NAMP2
         LA    R2,L'VAL2           VAR2 value length
         ST    R2,VALL2
         LA    R2,VAL2             VAR2 value pointer
         ST    R2,VALP2
*
         LA    R2,L'NAM3           VAR3 name length
         ST    R2,NAML3
         LA    R2,NAM3             VAR3 name pointer
         ST    R2,NAMP3
         LA    R2,L'VAL3           VAR3 value length
         ST    R2,VALL3
         LA    R2,VAL3             VAR3 value pointer
         ST    R2,VALP3
*
         LA    R2,L'NAM4           VAR4 name length
         ST    R2,NAML4
         LA    R2,NAM4             VAR4 name pointer
         ST    R2,NAMP4
         LA    R2,L'VAL4           VAR4 value length
         ST    R2,VALL4
         LA    R2,VAL4             VAR4 value pointer
         ST    R2,VALP4
*
         EJECT
*     * /********************************************************/
*     * /* Initialize VARBLK                                    */
*     * /********************************************************/
         LA    R2,VARBLK           Blank out VARBLK
         LA    R3,VARBLKL
SPCVARB  EQU   *
         MVI   0(R2),C' '
         LA    R2,1(R2)
         BCT   R3,SPCVARB
*     * /********************************************************/
*     * /* Initialize WORKBLK                                   */
*     * /********************************************************/
         LA    R2,WORDBLK          Blank out WORDBLK
         LA    R3,WORDBLKK
SPCWORDB EQU   *
         MVI   0(R2),C' '
         LA    R2,1(R2)
         BCT   R3,SPCWORDB
*     * /********************************************************/
*     * /* Initialize BLANKS                                    */
*     * /********************************************************/
         MVI   BLANKS,C' '         Blank out for Case Translation
         MVC   BLANKS+1(L'BLANKS-1),BLANKS
*     * /********************************************************/
*     * /* Initialize EX Instructions                           */
*     * /********************************************************/
         MVC   MVCVARWK(MVCVARWL),MVCVARW#
         MVC   MVCVAR1(MVCVAR1L),MVCVAR1#
         MVC   UPPERCS(UPPERCSL),UPPERCS#
         MVC   MVCR8$6(MVCR8$6L),MVCR8$6#
         MVC   EXTRLC(EXTRLCL),EXTRLC#
         MVC   SRCHCLC(SRCHCLCL),SRCHCLC#
         MVC   REVRS(REVRSL),REVRS#
*
         XC    VARSET1L,VARSET1L
*
         MVI   MCALA1$,C'['        Attribute Month Name and Year INIT   LB1100e
         MVI   MCALA2$,C'?'        Attribute JJJ                 INIT   LB1100e
         MVI   MCALA3$,C'['        Attribute Weekend Day Names   INIT   LB1100e
         MVI   MCALA4$,C'$'        Attribute Calendar Dates      INIT   LB1100e
         MVI   MCALA5$,C'?'        Attribute Calendar Cur Date   INIT   LB1100e
         EJECT
*     * /********************************************************/
*     * /* Get PARAMTER information passed to program           */
*     * /* - R1 myPARMS address on entry                        */
*     * /* - R4 Starting parms address                          */
*     * /* - R6 Starting parms length                           */
*     * /* - R3, R5                   Working Register          */
*     * /********************************************************/
         LTR   R1,R1               Do I have a PARM?
         BZ    ERR4004             NO, EXIT with ERR4004
         LR    R3,R1               PARM/CPPL addr
         TM    0(R3),X'80'         Is it PARM or CPPL addr?
         BZ    CPPL$IN             YES, CPPL addr
PARM$IN  EQU   *                   NO,  PARM addr
         L     R4,0(,R3)           Addr of PARM
         LH    R6,0(,R4)           Length of PARM
         LTR   R6,R6               PARM > 0 length?
         BZ    ERR4004             NO, EXIT with ERR4004
         LA    R4,2(,R4)           YES, point to start of PARM data
         B     DOPARMS              and continue...
CPPL$IN  EQU   *
         USING CPPL,R1             CPPL, tell assembler
         MVC   MYUPT,CPPLUPT       Save UPT
         MVC   MYECT,CPPLECT       Save ECT
         MVC   MYPSCB,CPPLPSCB     Save PSCB                            LB1105g
         DROP  R1                  Drop R1
         L     R4,0(,R3)           Command Buffer addr
         LH    R5,2(,R4)           Offset to Command Variables
         LTR   R5,R5               Any Variables?
         BZ    ERR4008             NO, EXIT with RC = 8
         LH    R6,0(,R4)           Length of Command Buffer
         SR    R6,R5               Subtract variable offset
         SH    R6,=H'4'            Subtract prefix
         BM    ERR4008             EXIT with RC = 8 IF NO VARIABLES
         LA    R4,4(R4,R5)         Point to variables start addr
DOPARMS  EQU   *
         ST    R1,MYR1             My PARM address on entry
         ST    R4,MYR4             My Starting Parms address
         ST    R6,MYR6             My Starting Parms length
*     * /********************************************************/
*     * /* Uppercase translation of PARMIN                      */
*     * /* - R4 Starting parms address                          */
*     * /* - R6 Parm Length                                     */
*     * /* - R2, R7                   Working Register          */
*     * /********************************************************/
UP#EM    EQU   *
         LR    R2,R4               Start addr - PARMS
         LR    R7,R6               Length of  - PARMS
         BCTR  R7,0                Adjust for EXecute
         EX    R7,UPPERCS          Execute Uppercase translate   FUNCT
*UPPERCS  OC    0(0,R2),BLANKS      EBCDIC Lower-Upper Case Translate
*
         EJECT
*     * /********************************************************/
*     * /* Get WORDS from PARM data                             */
*     * /* Each WORD is a MAX of 8 bytes                        */
*     * /* Process SETVAR keyword                               */
*     * /* - R4 Starting parms address                          */
*     * /* - R6 Starting parms length                           */
*     * /* - R7, R8, R5, R3, R2, R0      Working Registers      */
*     * /********************************************************/
         LA    R7,MAXWD
         ST    R7,MAXWORDS         Initialize MAXWORDS
         LA    R7,WORDLEN
         ST    R7,WORDL            Initialize WORKLENGTH
         LA    R7,WORDBLK          Addr of WORDBLK                      r10->r0
         LR    R8,R4               Copy addr of PARM data
         SR    R2,R2       r9      Count WORD length
         LA    R5,1         r11    WORD Count
         LA    R3,WORDBLKL         Addr of WORDBLKL Lengths
GETWORDS EQU   *
         LTR   R6,R6
         BZ    ERR4004
         CLI   0(R8),C' '          WORD delimiter in PARM?
         BE    NEXTWORD            YES, Bump to next word in WORDBLK
         LA    R2,1(R2)      r9    NO,  Bump up WORD length
         C     R2,WORDL      r9    Word LENGTH > 8?
         BH    ERR4005             Yes, EXIT with RC = 5
         MVC   0(1,R7),0(R8)       Move PARM data to WORDBLK
         LA    R7,1(R7)            Bump to next WORD char
         B     NEXTCHAR            Bump to next PARM char
NEXTWORD EQU   *
         LTR   R2,R2               Started moving new WORD?
         BE    NEXTCHAR            No, look at next PARM char
         LA    R5,1(R5)      r11   Add 1 to WORD count
         C     R5,MAXWORDS   r11   More than MAX words?
         BH    ERR4009             YES, Too Many WORDS
         C     R5,=F'3'            3rd WORD?
         BNE   NXTCONT             NO, continue
         CLC   =C'SETVAR ',FUNCT   YES. Is it SETVAR function?          LB1100y
         BNE   NXTCONT             NO, continue
         EJECT
DOSETVAR EQU   *
**       STM   R1,R2,DW            SAVE    R1,R2  to   DW before TPUT
**       LA    R1,=C'-- SETVAR word 3    '
**       LA    R2,20
**       TPUT  (R1),(R2)
**       LM    R1,R2,DW            RESTORE R1,R2  from DW after  TPUT
         LA    R7,VARSET1          R7=VAL1
         ST    R6,VARSET1L         Store LEN of VARSET
         XC    VARSET1L,VARSET1L   Set VARSET1L to ZERO
FNXTBLNK EQU   *                   Find next non-blank after WORD
         CLI   0(R8),C' '          Blank ?
         BNE   SV1CHKQB            No, check for QUOTE
         LA    R8,1(R8)            Yes, increment and
         BCT   R6,FNXTBLNK         ... keep checking
         B     ERR4010             Error - No SETVAR content found
SV1CHKQB EQU   *                   Check for QUOTE at beginning         LB1100z
         CLI   0(R8),C''''         Is it QUOTE?
         BNE   SV1NOQ              No, save LEN and move data
         LA    R8,1(R8)            Yes, increment to next location
         BCTR  R6,0                Adjust LEN
SV1NOQ   EQU   *
         ST    R6,VARSET1L         Store LEN of VARSET
MOVVAR1L EQU   *
         C     R6,=F'1'            Last position in PARM data?
         BNE   FFF1                NO, keep moving data
         CLI   0(R8),C''''         Is is a QUOTE?
         BE    FFF2                Yes
FFF1     EQU   *                   No
         MVC   0(1,R7),0(R8)
         LA    R7,1(R7)            Bump to next PARM char
         LA    R8,1(R8)            Bump to next PARM char
         BCT   R6,MOVVAR1L         Do PARM IN Length
         B     FFF3
FFF2     EQU   *
         L     R6,VARSET1L         Adjust length of VARSET
         BCTR  R6,0                ...variable
         ST    R6,VARSET1L         Store LEN of VARSET
FFF3     EQU   *
         STM   R1,R2,DW            SAVE    R1,R2  to   DW before TPUT
         LA    R1,VARSET1
         L     R2,VARSET1L
         TPUT  (R1),(R2)
         LM    R1,R2,DW            RESTORE R1,R2  from DW after  TPUT
         B     CONT00L
         EJECT
NXTCONT  EQU   *
         ST    R2,0(R3)  r9        Store WORD length
         LA    R0,8      r10       NO,  position to next WORD
         SR    R0,R2     r10
         AR    R7,R0        r10    Initialize to start of next WORD
         SR    R2,R2     r9 r9     Reset WORD length count
         LA    R3,4(R3)            Bump to next WORD length store
NEXTCHAR EQU   *
         LA    R8,1(R8)            Bump to next PARM char
         BCT   R6,GETWORDS       r5Do PARM IN Lenght
CONT00L  EQU   *
         ST    R2,0(R3)  r9        Store WORD length  - last one
         L     R6,MYR6             Restore Starting Parms Length
*
         EJECT
*     * /********************************************************/
*     * /* Process function                                     */
*     * /********************************************************/
PRCFUNCT EQU   *
         CLC   =C'ABOUT ',FUNCT    *00 About, myStamp                   LB1100y
         BE    PABOUT
         CLC   =C'LTRIM ',FUNCT    *01 Trim Leading  spaces             LB1100y
         BE    PLSTRIP
         CLC   =C'RTRIM ',FUNCT    *02 Trim Trailing spaces             LB1100y
         BE    PRSTRIP
         CLC   =C'TRIM ',FUNCT     *03 Trim Leading/Trailing spaces     LB1100y
         BE    PSTRIP
         CLC   =C'TRIM$ ',FUNCT    *04 Trim Leading/Trailing/Dup spaces LB1100y
         BE    PTRIM$
         CLC   =C'INDEX ',FUNCT    *05 Alias FIND                       LB1100y
         BE    PCOUNT
         CLC   =C'INDEXB ',FUNCT   *06 Alias FINDL                      LB1100y
         BE    PCOUNT
         CLC   =C'ISNUM ',FUNCT    *07 ISnumeric                        LB1100y
         BE    PISNUM
         CLC   =C'ISALPHA ',FUNCT  *08 ISalpha                          LB1100y
         BE    PISALPHA
         CLC   =C'ISLOWER ',FUNCT  *09 ISlower                          LB1100y
         BE    PISLOWER
         CLC   =C'ISUPPER ',FUNCT  *10 ISupper                          LB1100y
         BE    PISUPPER
         CLC   =C'ISBLANK ',FUNCT  *11 ISblank                          LB1100y
         BE    PISBLANK
         CLC   =C'ISDSN ',FUNCT    *12 ISdsn                            LB1100y
         BE    PISDSN
         CLC   =C'ECHO ',FUNCT     *13 ECHO to terminal                 LB1100y
         BE    PECHO
         CLC   =C'ECHOQ ',FUNCT    *14 ECHO to terminal w quotes        LB1100y
         BE    PECHO
         CLC   =C'REVRS ',FUNCT    *15 Translate to Reverse             LB1100y
         BE    PREVRS
         CLC   =C'UPPERC ',FUNCT   *16 Upper Case Translate             LB1100y
         BE    PUPPERC
         CLC   =C'LOWERC ',FUNCT   *17 Lower Case Translate             LB1100y
         BE    PLOWERC
         CLC   =C'COUNT ',FUNCT    *18 COUNT occurrence of substr       LB1105z
         BE    PCOUNT
         CLC   =C'FINDALL ',FUNCT  *18 Alias COUNT                      LB1100y
         BE    PCOUNT
         CLC   =C'FINDL ',FUNCT    *19 Find last occurrence of substr   LB1105z
         BE    PCOUNT
         CLC   =C'FIND ',FUNCT     *20 Find first occurrence of substr  LB1105z
         BE    PCOUNT
         EJECT
         CLC   =C'CENTER ',FUNCT   *21 Center string content            LB1100y
         BE    PCENTER
         CLC   =C'LJUST ',FUNCT    *22 Left  Justify content            LB1100y
         BE    PLJUST
         CLC   =C'RJUST ',FUNCT    *23 Right Justify content            LB1100y
         BE    PRJUST
         CLC   =C'ZFILL ',FUNCT    *24 Zero fill and right justify      LB1100y
         BE    PRJUST
         CLC   =C'WORDS ',FUNCT    *25 Count of WORDS in string         LB1100y
         BE    PWORDS
         CLC   =C'GEN# ',FUNCT     *26 Generate 3 digit number          LB1100y
         BE    PGEN#
         CLC   =C'DD2DSN ',FUNCT   *27 DD to DSN                        LB1100y
         BE    PDD2DSN
         CLC   =C'JOBINFO ',FUNCT  *28 JOB INFORMATION                  LB1100y
         BE    PJOBINFO
***      CLC   =C'SETVAR ',FUNCT   SET VAR1                             LB1100y
***      BE    PSETVAR
         CLC   =C'DAYSMM ',FUNCT   *29 Days in Month                    LB1100y
         BE    PDAYSMM
         CLC   =C'DAYSYY ',FUNCT   *30 Days in Year                     LB1100y
         BE    PDAYSYY
         CLC   =C'ISLEAP ',FUNCT   *31 ISleap year                      LB1100y
         BE    PDAYSYY
         EJECT
         CLC   =C'CYJ-D8 ',FUNCT   *32 Julian to Monthname-Day-Year     LB1100y
         BE    PJDATE
         CLC   =C'CYJ-DAY ',FUNCT  *33 Julian to Day Name               LB1100y
         BE    PJDAY
         CLC   =C'CYJ-DOW ',FUNCT  *34 Julian to Day of Week Number     LB1100y
         BE    PJDOW
         CLC   =C'CYJ-',FUNCT      *35 Julian to MMDDCCYY (any order)   LB1100y
         BE    PJMDCY
         CLC   =C'JCY-D8 ',FUNCT   *36 Julian to Monthname-Day-Year     LB1100y
         BE    PJDATE
         CLC   =C'JCY-DAY ',FUNCT  *37 Julian to Day Name               LB1100y
         BE    PJDAY
         CLC   =C'JCY-DOW ',FUNCT  *38 Julian to Day of Week Number     LB1100y
         BE    PJDOW
         CLC   =C'JCY-',FUNCT      *39 Julian to MMDDCCYY (any order)   LB1100y
         BE    PJMDCY
         CLC   =C'MDCY-D8 ',FUNCT  *40 MDCY to Monthname-Day-Year       LB1100y
         BE    PMDATE
         CLC   =C'MDCY-DAY',FUNCT  *41 MDCY to Day Name                 LB1100y
         BE    PMDAY
         CLC   =C'MDCY-DOW',FUNCT  *42 MDCY to Day of Week Number       LB1100y
         BE    PMDOW
         CLC   =C'MDCY-',FUNCT     *43 MDCY to CCYYJJJ (any order)      LB1100y
         BE    PMDCYJ
         CLC   =C'DMCY-D8 ',FUNCT  *44 DMCY to Monthname-Day-Year       LB1100y
         BE    PMDATE
         CLC   =C'DMCY-DAY',FUNCT  *45 DMCY to Day Name                 LB1100y
         BE    PMDAY
         CLC   =C'DMCY-DOW',FUNCT  *46 DMCY to Day of Week Number       LB1100y
         BE    PMDOW
         CLC   =C'DMCY-',FUNCT     *47 DMCY to CCYYJJJ (any order)      LB1100y
         BE    PMDCYJ
         CLC   =C'CYMD-D8 ',FUNCT  *48 CYMD to Monthname-Day-Year       LB1100y
         BE    PMDATE
         CLC   =C'CYMD-DAY',FUNCT  *49 CYMD to Day Name                 LB1100y
         BE    PMDAY
         CLC   =C'CYMD-DOW',FUNCT  *50 CYMD to Day of Week Number       LB1100y
         BE    PMDOW
         CLC   =C'CYMD-',FUNCT     *51 CYMD to CCYYJJJ (any order)      LB1100y
         BE    PMDCYJ
         CLC   =C'CYDM-D8 ',FUNCT  *52 CYDM to Monthname-Day-Year       LB1100y
         BE    PMDATE
         CLC   =C'CYDM-DAY',FUNCT  *53 CYDM to Day Name                 LB1100y
         BE    PMDAY
         CLC   =C'CYDM-DOW',FUNCT  *54 CYDM to Day of Week Number       LB1100y
         BE    PMDOW
         CLC   =C'CYDM-',FUNCT     *55 CYDM to CCYYJJJ (any order)      LB1100y
         BE    PMDCYJ
         EJECT
         CLC   =C'FILL ',FUNCT     *56 FILL with delimiter              LB1100y
         BE    PFILL
         CLC   =C'LSTRIP ',FUNCT   *57 LSTRIP delimiter (leading)       LB1100y
         BE    PLSTRIP
         CLC   =C'RSTRIP ',FUNCT   *58 RSTRIP delimiter (trailing)      LB1100y
         BE    PRSTRIP
         CLC   =C'STRIP ',FUNCT    *59 STRIP delimiter (lead/trail)     LB1100y
         BE    PSTRIP
         CLC   =C'STRIP$ ',FUNCT   *60 STRIP delimiter (all)            LB1100y
         BE    PSTRIP$
         CLC   =C'CONCAT ',FUNCT   *61 Concatenate two variables        LB1100y
         BE    PCONCAT
         CLC   =C'UNSTR ',FUNCT    *62 UNSTRing into variables          LB1100y
         BE    PUNSTR
         CLC   =C'REPLACE ',FUNCT  *63 REPLACE str with st2             LB1100y
         BE    PREPL
         CLC   =C'VEXIST ',FUNCT   *64 VEXIST variable exist            LB1100y
         BE    PVEXIST
         CLC   =C'TDSN ',FUNCT     *65 TDSN temporary DSN nodes         LB1100y
         BE    PTDSN
         CLC   =C'NOW ',FUNCT      *66 NOW  current date time info      LB1100y
         BE    PNOW
         CLC   =C'PAD ',FUNCT      *67 PAD  Pad current string          LB1100y
         BE    PPAD
         CLC   =C'PUT1V ',FUNCT    *68 PUT1V Overlay 1-byte on VAR1(n)  LB1100c
         BE    PPUT1V                                                   LB1100c
         CLC   =C'GET1V ',FUNCT    *69 GET1V Get 1-byte from VAR1(n)    LB1100d
         BE    PGET1V                                                   LB1100d
         CLC   =C'MCAL ',FUNCT     *70 MCAL  Create Month Calendar      LB1100e
         BE    PMCAL                                                    LB1100e
         CLC   =C'MCALA ',FUNCT    *71 MCALA Create Month Calendar      LB1100e
         BE    PMCAL                                                    LB1100e
         CLC   =C'LEN ',FUNCT      *72 LEN Length of data in string     LB1105a
         BE    PLEN                                                     LB1105a
         CLC   =C'SLEN ',FUNCT     *73 LEN Length of string             LB1105a
         BE    PSLEN                                                    LB1105a
         CLC   =C'OVERLAY ',FUNCT  *74 OVERLAY string                   LB1105c
         BE    POVRL                                                    LB1105c
         CLC   =C'UTDSN ',FUNCT    *75 UTDSN user temporary DSN         LB1105g
         BE    PUTDSN                                                   LB1105g
         CLC   =C'TRUNC ',FUNCT    *76 Truncate string                  LB1105h
         BE    PTRUNC                                                   LB1105h
PRCFUNCX EQU   *
         B     ERR4011             Invalid function ************
*
         EJECT
*     * /********************************************************/
*00   * /* Function: ABOUT                   -   -              */
*     * /********************************************************/
*     * /* Display mystamp                                      */
*     * /* - R1, R2, R5               Working Register          */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /********************************************************/
PABOUT   EQU   *                   Process ABOUT
*     * /--------------------------------------------------------/
*     * /* Display mySTAMP info on terminal                     */
*     * /--------------------------------------------------------/
         LA    R5,MYSTAMPL         Load length of mySTAMP
         LA    R5,4(R5)            Adj length for header (+4)
         STH   R5,OUTTXT
         MVC   OUTTXT+2(2),=X'0000'  Init reserved fld to x'0'
         MVC   INFOLINE(MYSTAMPL),MYSTAMP
         BAL   R5,S#PUTL           Do PUTLINE
PABOUTX  EQU   *
         SR    R15,R15
         B     ERRDONE             Successful Request
*
         EJECT
*     * /********************************************************/
*04   * /* Function: TRIM$                  REQ OPT             */
*     * /********************************************************/
*     * /* Strip leading and trailing spaces including remaining*/
*     * /* duplicate spaces in string                           */
*     * /* - R2, R5                   Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /********************************************************/
PTRIM$   EQU   *                   Process TRIM$$
         BAL   R5,S#GVAR1          Get VAR1
         BAL   R5,S#CHKV2          Reset VAR2 when $NOVAR2
         LA    R2,VARWORK
         SR    R5,R5               VARWORK Length
*     * /--------------------------------------------------------/
*     * /* Eliminate repeating blanks in string                 */
*     * /--------------------------------------------------------/
TRIML$   EQU   *
         MVC   0(1,R2),0(R8)
         LA    R5,1(R5)            Add 1 to length - VARWORK
         CLI   0(R8),C' '          Blank character moved?
         BE    SKIPBLK$            YES, skip subsequent blanks
         B     BUMPV$              NO, move more data
SKIPBLK$ EQU   *
         LA    R8,1(R8)            Bump to next position - VAR1
         CLI   0(R8),C' '          Blank character?
         BNE   BUMPVW$             NO,  go move more data
         BCT   R7,SKIPBLK$         YES, skip it and check next pos
         B     BUMPEND$
BUMPV$   EQU   *
         LA    R8,1(R8)            Bump to next position - VAR1
BUMPVW$  EQU   *
         LA    R2,1(R2)            Bump to next position - VARWORK
         BCT   R7,TRIML$           Decrement length while BLANK byte
BUMPEND$ EQU   *
         LR    R7,R5               Load revised length for VAR1
*     * /--------------------------------------------------------/
*     * /* At this point, VARWORK contains text with only one   */
*     * /* blank between content i.e. ' this is my content '    */
*     * /--------------------------------------------------------/
         EJECT
*     * /--------------------------------------------------------/
*     * /* If last byte is blank, decrement length              */
*     * /--------------------------------------------------------/
CHKLA$T  EQU   *
         LA    R2,VARWORK          Start addr - VARWORK
         AR    R2,R7               Add length to start addr - VARWORK
         BCTR  R2,0                .. make adjustment, end addr
         CLI   0(R2),C' '          Last position blank?
         BNE   CHKFIR$T            NO, check starting position
         BCTR  R7,0                YES, adjust length
*     * /--------------------------------------------------------/
*     * /* If first byte is blank, move string left by 1        */
*     * /--------------------------------------------------------/
CHKFIR$T EQU   *
         LA    R2,VARWORK          Start addr - VARWORK
         CLI   0(R2),C' '          First position blank?
         BNE   PTRIM$X             NO, done...
*                                  YES, adjust and reseed data
         LA    R2,1(R2)            Move start addr - VARWORK
         LR    R8,R2               Load start addr for EX
         BCTR  R7,0                Adjust for EXecute
         EX    R7,MVCVARWK         Execute VARWORK value move
*MVCVARWK MVC   VARWORK(0),0(R8)    Save VARWORK  value
****     LA    R7,1(R7)            Reset after EXecute
****                               No need to adjust R7, as data
****                               is 1 byte less!!
PTRIM$X  EQU   *
         BAL   R5,S#UVAR2          Update VAR2
         B     ERRDONE             Successful Request
*
         EJECT
*     * /********************************************************/
*07   * /* Function: ISNUM                  REQ NO              */
*     * /********************************************************/
*     * /* Check string for all numeric (0-9)                   */
*     * /* - R2, R5                   Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=4000  FALSE                                     */
*     * /* - RC=4001  TRUE                                      */
*     * /* - RC=4022  VAR2 not required                         */
*     * /********************************************************/
PISNUM   EQU   *                   Process ISNUM
*     * /--------------------------------------------------------/
*     * /* No   VAR2 content in position 1 needed               */
*     * /--------------------------------------------------------/
         CLC   VAR2,=C'$NOVAR2 '   VAR2 none keyword?
         BE    PISNUM#             YES, continue...
         CLI   VAR2,C' '           VAR2 name captured?
         BNE   ERR4022             YES, VAR2 not needed
PISNUM#  EQU   *
         BAL   R5,S#GVAR1          Get VAR1
         BAL   R5,S#ISNUM          Numeric Test
         C     R15,=F'4000'        Numeric?
         BE    ERR4000             NO, FALSE
         B     ERR4001             YES, TRUE
*
         EJECT
*     * /********************************************************/
*08   * /* Function: ISALPHA                REQ NO              */
*09   * /* Function: ISLOWER                REQ NO              */
*10   * /* Function: ISUPPER                REQ NO              */
*     * /********************************************************/
*     * /* ISALPHA Check string for all alphabetic (A-Z,a-z)    */
*     * /* ISLOWER Check string for all UPPER alphabetic (A-Z)  */
*     * /* ISUPPER Check string for all LOWER alphabetic (a-z)  */
*     * /* - R2, R5                   Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=4000  FALSE                                     */
*     * /* - RC=4001  TRUE                                      */
*     * /* - RC=4022  VAR2 not required                         */
*     * /********************************************************/
PISALPHA EQU   *                   Process ISALPHA
PISLOWER EQU   *                   Process ISLOWER
PISUPPER EQU   *                   Process ISUPPER
*     * /--------------------------------------------------------/
*     * /* No   VAR2 content in position 1 needed               */
*     * /--------------------------------------------------------/
         CLC   VAR2,=C'$NOVAR2 '   VAR2 none keyword?
         BE    PISALPH#            YES, continue...
         CLI   VAR2,C' '           VAR2 name captured?
         BNE   ERR4022             YES, VAR2 not needed
PISALPH# EQU   *
         BAL   R5,S#GVAR1          Get VAR1
         CLC   =C'ISLOWER ',FUNCT  ISLOWER request?                     LB1100y
         BE    PISLOWRL            Yes.
         CLC   =C'ISUPPER ',FUNCT  ISUPPER request?                     LB1100y
         BE    PISUPPRL            Yes.
*                                  Must be ISALPHA request
*
         EJECT
*     * /********************************************************/
*     * /* Check string for all alphabetic (A-Z,a-z)            */
*     * /********************************************************/
         BCTR  R7,0                Adjust for EXecute
         EX    R7,MVCVARWK         Execute VARWORK value move
*MVCVARWK MVC   VARWORK(0),0(R8)    Save VARWORK  value
         LA    R2,VARWORK          Start addr - VARWORK
         EX    R7,UPPERCS          Execute Uppercase translation
*UPPERCS  OC    0(0,R2),BLANKS      EBCDIC Lower-Upper Case Translate
         LA    R7,1(R7)            Reset R7 to original value
PISALPHL EQU   *                   Process ISALPHA
         CLI   0(R2),C'Z'          Byte > Z?
         BH    ERR4000             YES, not alphabetic, FALSE
         CLI   0(R2),C'A'          Byte < A?
         BL    ERR4000             YES, not alphabetic, FALSE
         CLI   0(R2),C'I'          Byte <=I?
         BNH   PISLAPNX            Alphabetic
         CLI   0(R2),C'J'          Byte < J?
         BL    ERR4000             YES, not alphabetic, FALSE
         CLI   0(R2),C'R'          Byte <=R?
         BNH   PISLAPNX            Alphabetic
         CLI   0(R2),C'S'          Byte < S?
         BL    ERR4000             YES, not alphabetic, FALSE
         CLI   0(R2),C'Z'          Byte <=Z?
         BNH   PISLAPNX            Alphabetic
         B     ERR4000             Not alphabetic, FALSE
PISLAPNX EQU   *
         LA    R2,1(R2)            Bump up to next byte of VARWORK
         BCT   R7,PISALPHL         LOOP for alphabetic
         B     ERR4001             All pass alphabetic, TRUE
*
         EJECT
*     * /********************************************************/
*     * /* Check string for all LOWER alphabetic (a-z)          */
*     * /********************************************************/
PISLOWRL EQU   *                   Process ISLOWER
         CLI   0(R8),C'z'          Byte > z?
         BH    ERR4000             YES, not low alpha,  FALSE
         CLI   0(R8),C'a'          Byte < a?
         BL    ERR4000             YES, not low alpha,  FALSE
         CLI   0(R8),C'i'          Byte <=i?
         BNH   PISLOWRX            Low alpha
         CLI   0(R8),C'j'          Byte < j?
         BL    ERR4000             YES, not low alpha,  FALSE
         CLI   0(R8),C'r'          Byte <=r?
         BNH   PISLOWRX            Low alpha
         CLI   0(R8),C's'          Byte < s?
         BL    ERR4000             YES, not low alpha,  FALSE
         CLI   0(R8),C'z'          Byte <=z?
         BNH   PISLOWRX            Low alpha
         B     ERR4000             Not low alpha,  FALSE
PISLOWRX EQU   *
         LA    R8,1(R8)            Bump up to next byte of VAR1
         BCT   R7,PISLOWRL         LOOP for lowercase alphabetic
         B     ERR4001             All pass lower alphabetic, TRUE
*
         EJECT
*     * /********************************************************/
*     * /* Check string for all UPPER alphabetic (A-Z)          */
*     * /********************************************************/
PISUPPRL EQU   *                   Process ISUPPER
         CLI   0(R8),C'Z'          Byte > Z?
         BH    ERR4000             YES, not upperalpha, FALSE
         CLI   0(R8),C'A'          Byte < A?
         BL    ERR4000             YES, not upperalpha, FALSE
         CLI   0(R8),C'I'          Byte <=I?
         BNH   PISUPPRX            upperalpha
         CLI   0(R8),C'J'          Byte < J?
         BL    ERR4000             YES, not upperalpha, FALSE
         CLI   0(R8),C'R'          Byte <=R?
         BNH   PISUPPRX            upperalpha
         CLI   0(R8),C'S'          Byte < S?
         BL    ERR4000             YES, not upperalpha, FALSE
         CLI   0(R8),C'Z'          Byte <=Z?
         BNH   PISUPPRX            upperalpha
         B     ERR4000             Not upperalpha, FALSE
PISUPPRX EQU   *
         LA    R8,1(R8)            Bump up to next byte of VAR1
         BCT   R7,PISUPPRL         LOOP for uppercase alphabetic
         B     ERR4001             All pass upper alphabetic, TRUE
*
         EJECT
*     * /********************************************************/
*11   * /* Function: ISBLANK                REQ NO              */
*     * /********************************************************/
*     * /* Check string for all whitespace (space)              */
*     * /* - R2, R5                   Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=4000  FALSE                                     */
*     * /* - RC=4001  TRUE                                      */
*     * /* - RC=4022  VAR2 not required                         */
*     * /********************************************************/
PISBLANK EQU   *                   Process ISBLANK
*     * /--------------------------------------------------------/
*     * /* No   VAR2 content in position 1 needed               */
*     * /--------------------------------------------------------/
         CLC   VAR2,=C'$NOVAR2 '   VAR2 none keyword?
         BE    PISBLNK#            YES, continue...
         CLI   VAR2,C' '           VAR2 name captured?
         BNE   ERR4022             YES, VAR2 not needed
PISBLNK# EQU   *
         BAL   R5,S#GVAR1          Get VAR1
PISWHTL  EQU   *
         CLI   0(R8),C' '          Byte = SPACE?
         BNE   ERR4000             NO, not blank, FALSE
         LA    R8,1(R8)            Bump up to next byte of VAR1
         BCT   R7,PISWHTL          LOOP for blanks
PISBLANX EQU   *
         B     ERR4001             All pass spaces, TRUE
*     *
         EJECT
*     * /********************************************************/
*12   * /* Function: ISDSN                  REQ NO              */
*     * /********************************************************/
*     * /* Check string for valid dataset name                  */
*     * /*   - string 1 to 44 bytes                             */
*     * /*   - contain up to 22 segments                        */
*     * /*   - segment delimiter is period (.)                  */
*     * /*   - each segment 1-8 bytes                           */
*     * /*   - char 1-1 in segment A-Z #@$                      */
*     * /*   - char 2-8 in segment A-Z #@$ 0-9                  */
*     * /* - R2, R5                   Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=4000  FALSE                                     */
*     * /* - RC=4001  TRUE                                      */
*     * /* - RC=4022  VAR2 not required                         */
*     * /********************************************************/
PISDSN   EQU   *                   Process ISDSN
*     * /--------------------------------------------------------/
*     * /* No   VAR2 content in position 1 needed               */
*     * /--------------------------------------------------------/
         CLC   VAR2,=C'$NOVAR2 '   VAR2 none keyword?
         BE    PISDSN#             YES, continue...
         CLI   VAR2,C' '           VAR2 name captured?
         BNE   ERR4022             YES, VAR2 not needed
PISDSN#  EQU   *
         BAL   R5,S#GVAR1          Get VAR1
         BCTR  R7,0                Adjust for EXecute
         EX    R7,MVCVARWK         Execute VARWORK value move
*MVCVARWK MVC   VARWORK(0),0(R8)    Save VARWORK  value
         LA    R7,1(R7)            Reset R7 to original value
         LA    R2,VARWORK          Start addr - VARWORK
*     * /--------------------------------------------------------/
*     * /* Check DSN max length 44 bytes                        */
*     * /--------------------------------------------------------/
         C     R7,=F'44'           Var > 44 bytes?
         BH    ERR4000             Failed valid length, FALSE
*     * /--------------------------------------------------------/
*     * /* Start DSN name check                                 */
*     * /--------------------------------------------------------/
         ZAP   KSEGCHAR,=P'+0'     Segment character count
         ZAP   KSEGCNT,=P'+0'      Segment count
PISDSNC  EQU   *
         CLI   0(R2),C'.'          Segment delimiter? 4B
         BNE   DODSNSEG            NO. Test for valid chars
*                                  YES. Process segment mark
         AP    KSEGCNT,=P'+1'      Bump up segment count
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Check valid segment length (1-8)                     */
*     * /--------------------------------------------------------/
         CP    KSEGCHAR,=P'+1'     Seg length < 1?
         BL    ERR4000             YES. Invalid seg length, FALSE
         CP    KSEGCHAR,=P'+8'     Seg length > 8?
         BH    ERR4000             YES. Invalid seg length, FALSE
         ZAP   KSEGCHAR,=P'+0'     Reset segment character count
         B     PIDSNCX             Bump to next char
DODSNSEG EQU   *
*     * /--------------------------------------------------------/
*     * /* Test notional characters                             */
*     * /--------------------------------------------------------/
         CLI   0(R2),C'$'          Byte = #?    5B
         BE    CHKSEG              Valid char, check segment start
         CLI   0(R2),C'-'          Byte = #?    60
         BE    CHKSEG              Valid char, check segment start
         CLI   0(R2),C'#'          Byte = #?    7B
         BE    CHKSEG              Valid char, check segment start
         CLI   0(R2),C'@'          Byte = #?    7C
         BE    CHKSEG              Valid char, check segment start
*     * /--------------------------------------------------------/
*     * /* Test alphabetic characters                           */
*     * /--------------------------------------------------------/
*        STM   R1,R2,DW            SAVE    R1,R2  to   DW before TPUT
*        LA    R1,=C'-- Test alpha       '
*        LA    R2,20
*        TPUT  (R1),(R2)
*        LM    R1,R2,DW            RESTORE R1,R2  from DW after  TPUT
         CLI   0(R2),C'A'          Byte < A?    C1
         BL    ERR4000             Not alphabetic, return FALSE
         CLI   0(R2),C'I'          Byte <=I?    C9
         BNH   CHKSEG              Valid char, check segment start
         CLI   0(R2),C'J'          Byte < J?    D1
         BL    ERR4000             Not alphabetic, return FALSE
         CLI   0(R2),C'R'          Byte <=R?    D9
         BNH   CHKSEG              Valid char, check segment start
         CLI   0(R2),C'S'          Byte < S?    E2
         BL    ERR4000             Not alphabetic, return FALSE
         CLI   0(R2),C'Z'          Byte <=Z?    E9
         BNH   CHKSEG              Valid char, check segment start
*     * /--------------------------------------------------------/
*     * /* Byte 1 of segment is invalid at this point           */
*     * /--------------------------------------------------------/
         CP    KSEGCHAR,=P'+0'     Seg length = 0?
         BE    ERR4000             YES, invalid char, return FALSE
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Test numerics                                        */
*     * /--------------------------------------------------------/
         CLI   0(R2),C'0'          Byte < 0?    F0
         BL    ERR4000             Not numeric, return FALSE
         CLI   0(R2),C'9'          Byte <= 9?    F9
         BNH   CHKSEG              Valid char, check segment start
CHKSEG   EQU   *
         AP    KSEGCHAR,=P'+1'     Bump up segment char count
         B     PIDSNCX             Valid char, bump to next char
PIDSNCX  EQU   *
         LA    R2,1(R2)            Bump up to next byte of VAR1
         BCT   R7,PISDSNC          LOOP for alphabetic
*                                  End of Loop...
         CP    KSEGCHAR,=P'+0'     Seg length = 0?
         BE    ERR4000             YES, invalid char, return FALSE
PISDSNX  EQU   *
         B     ERR4001             All pass, TRUE
*
         EJECT
*     * /********************************************************/
*13   * /* Function: ECHO                   REQ NO              */
*14   * /*           ECHOQ                  REQ NO              */
*     * /********************************************************/
*     * /* Display var1 without or with single quotes           */
*     * /* - R1, R2, R5               Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /* - RC=4022  VAR2 not required                         */
*     * /********************************************************/
PECHO    EQU   *                   Process ECHO
*     * /--------------------------------------------------------/
*     * /* No   VAR2 content in position 1 needed               */
*     * /--------------------------------------------------------/
         CLC   VAR2,=C'$NOVAR2 '   VAR2 none keyword?
         BE    PECHO#              YES, continue...
         CLI   VAR2,C' '           VAR2 name captured?
         BNE   ERR4022             YES, VAR2 not needed
PECHO#   EQU   *
         BAL   R5,S#GVAR1          Get VAR1
         STM   R1,R2,DW            SAVE    R1,R2  to   DW before TPUT
         CLI   FUNCT+4,C'Q'        Is it ECHOQ?
         BNE   DOECHO              NO, DOECHO
DOECHOQ  EQU   *                   YES... DOECHOQ
         MVI   VAL1Q,C''''         Put start quote around string
         LA    R1,VAL1Q
         A     R1,VALL1
         LA    R1,1(R1)
         MVI   0(R1),C''''         Put end quote around string
         LA    R1,VAL1Q
         L     R2,VALL1
         LA    R2,2(R2)
         B     DODISPLY
*
         EJECT
DOECHO   EQU   *
         LA    R1,VAL1
         L     R2,VALL1
DODISPLY EQU   *                   Echo VAR01 content
         STH   R2,OUTTXT           Set length of VAL1
         CH    R2,=X'00FF'         GT 256?
         BNH   DOPLADJ             No, jump to adjust
         MVC   OUTTXT(2),=X'00FF'  Yes, set length of VAL1 to 256
DOPLADJ  LA    R2,4(R2)            Adj length for header (+4)
         STH   R2,OUTTXT
         MVC   OUTTXT+2(2),=X'0000'  Init reserved fld to x'0'
         EX    R5,MVCPUTL          EX move VAL1 to INFOLINE
         BAL   R5,S#PUTL           Do PUTLINE
*        TPUT  (R1),(R2)
         LM    R1,R2,DW            RESTORE R1,R2  from DW after  TPUT
PECHOX   EQU   *
         B     ERRDONE             Successful Request
*
MVCPUTL  MVC   INFOLINE(0),0(R1)   Move VAL1 to INFOLINE
*MVCPUTL  MVC   INFOLINE(0),0(R1)   Move VAL1 to INFOLINE
         EJECT
*     * /********************************************************/
*15   * /* Function: REVRS                  REQ OPT             */
*     * /********************************************************/
*     * /* Reverse letters and numbers.                         */
*     * /* A-Z > Z-A,  0-9 > 9-0, others to blanks.             */
*     * /* - R2, R3, R5               Working Register          */      LB1100i
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /********************************************************/
PREVRS   EQU   *                   Process REVRS
         MVC   PALP1,=C'3RVR'      Parm 1: Function Request             LB1100i
         LA    R1,PALLST           R1=Parm Addr List                    LB1100i
         L     R15,CUTILTBL        Call CUTILTBL                        LB1105b
         BALR  R14,R15                                                  LB1105b
         LTR   R15,R15             RC = 0 ?                             LB1100i
         BNZ   ERR4060             No, Error in CUTILTBL                LB1100i
         L     R3,PALP2            Yes, TRTBL start addr in Parm 2      LB1100i
         BAL   R5,S#GVAR1          Get VAR1
         BAL   R5,S#CHKV2          Reset VAR2 when $NOVAR2
         BCTR  R7,0                Adjust for EXecute
         EX    R7,MVCVARWK         Execute VARWORK value move
*MVCVARWK MVC   VARWORK(0),0(R8)    Save VARWORK  value
         EX    R7,REVRS            Execute XOR
*REVRS    TR    VARWORK(0),TREVRS   Reverse letters and numbers
*REVRS    TR    VARWORK(0),0(R3)    Reverse letters and numbers         LB1100i
         LA    R7,1(R7)            Adjust length to original
         BAL   R5,S#UVAR2          Update VAR2
PREVRSX  EQU   *
         B     ERRDONE             Successful Request
*
         EJECT
*     * /********************************************************/
*16   * /* Function: UPPERC                 REQ OPT             */
*17   * /* Function: LOWERC                 REQ OPT             */
*     * /********************************************************/
*     * /* UPPERC Convert string to uppercase                   */
*     * /* LOWERC Convert string to lowercase                   */
*     * /* - R2, R3, R5               Working Register          */      LB1100h
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /********************************************************/
PUPPERC  EQU   *                   Process UPPERC
PLOWERC  EQU   *                   Process LOWERC
         MVC   PALP1,=C'3ULC'      Parm 1: Function Request             LB1100h
         LA    R1,PALLST           R1=Parm Addr List                    LB1100h
         L     R15,CUTILTBL        Call CUTILTBL                        LB1105b
         BALR  R14,R15                                                  LB1105b
         LTR   R15,R15             RC = 0 ?                             LB1100h
         BNZ   ERR4060             No, Error in CUTILTBL                LB1100h
         L     R3,PALP2            Yes, TRTBL start addr in Parm 2      LB1100h
         BAL   R5,S#GVAR1          Get VAR1
         BAL   R5,S#CHKV2          Reset VAR2 when $NOVAR2
         BCTR  R7,0                Adjust for EXecute
         EX    R7,MVCVARWK         Execute VARWORK value move
*MVCVARWK MVC   VARWORK(0),0(R8)    Save VARWORK  value
         LA    R2,VARWORK
         CLC   =C'UPPERC ',FUNCT                                        LB1100y
         BE    DOUPPER
DOLOWER  EQU   *
         EX    R7,EXTRLC           Execute Lowercase translation
*EXTRLC   TR    0(0,R2),TRTBL       EBCDIC Upper-Lower Case Translate
*EXTRLC   TR    0(0,R2),0(R3)       EBCDIC Upper-Lower Case Translate   LB1100h
         B     PLOWERCX            Adjust and return
DOUPPER  EQU   *
         EX    R7,UPPERCS          Execute Uppercase translation
*UPPERCS  OC    0(0,R2),BLANKS      EBCDIC Lower-Upper Case Translate
PUPPERCX EQU   *
PLOWERCX EQU   *
         LA    R7,1(R7)            Adjust length to original
         BAL   R5,S#UVAR2          Update VAR2
         B     ERRDONE             Successful Request
*
         EJECT
*     * /********************************************************/
*18   * /* Function: COUNT                  REQ REQ             */
*18   * /* Function: FINDALL (Alias COUNT)  REQ REQ             */
*19   * /* Function: FIND                   REQ REQ             */
*05   * /* Function: INDEX   (Alias FIND)   REQ REQ             */
*20   * /* Function: FINDL                  REQ REQ             */
*06   * /* Function: INDEXB  (Alias FINDL)  REQ REQ             */
*     * /********************************************************/
*     * /* COUNT Number of search string occurrences            */      LB1105z
*     * /* FIND  Position of first search string occurrence     */      LB1105z
*     * /* FINDL Position of last search string occurrence      */      LB1105z
*     * /*                                                      */
*     * /* VAR2 contains Search Parms -------------             */
*     * /* Format: "sssssss",bbb,eee                            */
*     * /*    where;                                            */
*     * /*        s - search string in double quotes            */
*     * /*        b - optional, beginning search position       */      LB1100z
*     * /*              1-3 numeric digits, default=1           */
*     * /*              If b > VAR1 len, b = 1.                 */      LB1105z
*     * /*        e - optional, ending   search position        */
*     * /*              1-3 numeric digits, default=end         */
*     * /*              If e > VAR1 len, e = VAR1 len.          */      LB1105z
*     * /*        Parms separated by commas (,):                */
*     * /*      **If no VAR2 specified, BLANK is search string  */
*     * /*        including default start and end positions     */
*     * /*                                                      */
*     * /* - R0, R2, R3, R4, R5, R6   Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Search string NOT found                   */
*     * /* - RC>0     COUNT: number of occurrences              */      LB1105z
*     * /* -          FUNDF: pos of occurrence, relative to 1   */      LB1105z
*     * /* -          FUNDL: pos of occurrence, relative to 1   */      LB1105z
*     * /* - RC=4016  VAR2 not found                            */
*     * /* - RC=4032  Start > End Location                      */
*     * /********************************************************/
PCOUNT   EQU   *                   Process COUNT
         BAL   R5,S#GVAR1          Get VAR1
*     * /--------------------------------------------------------/
*     * /* Initialize Search Lengths                            */
*     * /--------------------------------------------------------/
         LA    R2,0                Initialize Search Lengths
         ST    R2,SRCHFND
*     *
         EJECT
*     * /--------------------------------------------------------/
*     * /* Copy VAR1 to VARWORK                                 */
*     * /--------------------------------------------------------/
         BCTR  R7,0                Adjust for EXecute
         EX    R7,MVCVARWK         Execute VARWORK value move
*MVCVARWK MVC   VARWORK(0),0(R8)    Save VARWORK  value
         LA    R7,1(R7)            Reset R7 to original value
         LA    R2,VARWORK          Start addr - VARWORK
*     * /--------------------------------------------------------/
*     * /* Check for absence of VAR2 (set default search)       */
*     * /--------------------------------------------------------/
         CLI   VAR2,C' '           VAR2 name captured?
         BE    LOADDFLT            No, use search default
         CLC   VAR2,=C'$NOVAR2 '   VAR2 none keyword?
         BE    LOADDFLT            Yes, use search default
         B     LOADVAR2            No, get VAR2
*     * /--------------------------------------------------------/
*     * /* Default search values                                */
*     * /--------------------------------------------------------/
LOADDFLT EQU   *
         MVC   VAL2(3),=C'" "'     Search default, BLANK
         MVC   VALL2,=F'3'         Search default length, 3
         B     LOADINSS            Start process
*     * /--------------------------------------------------------/
*     * /* Get VAR2                                             */
*     * /--------------------------------------------------------/
LOADVAR2 EQU   *
         BAL   R5,S#GVAR2          Get VAR2
         L     R15,RC#441          Restore R15
         LTR   R15,R15             Successful IKJCT441?
         BNZ   ERR4016             No, VAR2  not found
*     * /--------------------------------------------------------/
*     * /* Load Registers with VAR2                             */
*     * /--------------------------------------------------------/
LOADINSS EQU   *                   Start loading search string
         L     R5,VALP2            VAR2 address
         L     R6,VALL2            VAR2 length
*     * /--------------------------------------------------------/
*     * /* Get search parms                                     */
*     * /--------------------------------------------------------/
         BAL   R1,S#SRCHP          Get Search Parms
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Initialize and check Search Start and End Locations  */
*     * /--------------------------------------------------------/
*              SRCHSTR             Search String
         L     R3,SRCHS            Search Start Location
         L     R4,SRCHE            Search End   Location
         L     R5,SRCHSTRL         Search String Length
         BCTR  R5,0                Adjust for EXecute  CLC
         LA    R1,0                Find COUNTer
         LA    R2,VARWORK          Address of VAR1 copy
PCNTC0   EQU   *                   Check Start LOC value
         CR    R3,R4               Start > End location?
         BH    ERR4032             YES, start end loc error
PCNTC1   EQU   *                   Check Start LOC value
         LTR   R3,R3               Start LOC specified?
         BZ    PCNTC2              NO, check End LOC
*                                  YES.
         CR    R3,R7               StartLoc > Length?
         BH    PCNTC2              Yes, Ignore, check End LOC,
*                                  NO.
         AR    R2,R3               R2 = VAR1 addr + Start LOC
         BCTR  R2,0                R2 = R2 - 1
PCNTC2   EQU   *                   Check End   LOC value
         LTR   R4,R4               END   LOC specified?
         BZ    PCOUNTL             NO, process...
*                                  YES.
         CR    R4,R7               StartLoc > Length?
         BH    PCOUNTL             Yes, Ignore, process...
*                                  NO.
         LR    R0,R4               R0 = End Loc
         SR    R0,R3               R0 = R0 - Start LOC
         A     R0,=F'1'            R0 = R0 + 1
         LR    R7,R0               R7 = VAR1 LEN  adjusted
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Count of substring                                   */
*     * /--------------------------------------------------------/
         L     R0,SRCHSTRL         Search String Length
PCOUNTL  EQU   *                   Process COUNT
         EX    R5,SRCHCLC          Substring found?
*SRCHCLC  CLC   0(0,R2),SRCHSTR
         BE    PCOUNTF             YES,
         LA    R2,1(R2)            NO, bump to next byte of VAR1
         BCT   R7,PCOUNTL          Check again...
         B     PCOUNTX             Done
PCOUNTF  EQU   *
         ST    R7,SRCHFND
         CLC   =C'FIND ',FUNCT     FIND First function?                 LB1100y
         BE    PCOUNTX             YES. Return Position.
         CLC   =C'INDEX ',FUNCT    Alias FIND First function?           LB1100y
         BE    PCOUNTX             YES. Return Position.
         CR    R7,R0               Enough for another search?
         BL    PCOUNTX             NO, Done...
*                                  YES, count FIND.
         LA    R1,1(R1)            Add 1 to Find COUNT
         AR    R2,R5               Bump up VAR1 position by SRCHSTRL
         LA    R2,1(R2)             ...adjust to next byte of VAR1
         SR    R7,R5               Adjust loop count by SRCHSTRL
         BCT   R7,PCOUNTL          Adjust loop count
         B     PCOUNTX             Done
PCOUNTX  EQU   *
         CLC   =C'COUNT ',FUNCT    COUNT function?                      LB1100y
         BE    PCOUNTXZ            Yes, return count (R1)
         CLC   =C'FINDALL ',FUNCT  Alias COUNT function?                LB1100y
         BE    PCOUNTXZ            Yes, return count (R1)
*        must be FIND  FINDL       No, return found position (R1)
*                INDEX INDEXB
         CLC   SRCHFND,=F'0'
         BE    PCOUNTXY
         L     R1,VALL1
         S     R1,SRCHFND
         A     R1,=F'1'
         B     PCOUNTXZ
PCOUNTXY EQU   *
         L     R1,SRCHFND
PCOUNTXZ EQU   *
         LR    R15,R1              Return POSITION or COUNT
         B     ERRDONEF            Successful Request - function
*
         EJECT
*     * /********************************************************/
*21   * /* Function: CENTER                 REQ OPT             */
*     * /********************************************************/
*     * /* Center string content                                */
*     * /* - R3, R4, R5, R6           Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /********************************************************/
PCENTER  EQU   *                   Process CENTER
         BAL   R5,S#GVAR1          Get VAR1
         BAL   R5,S#CHKV2          Reset VAR2 when $NOVAR2
*     * /--------------------------------------------------------/
*     * /* Determine content start                              */
*     * /--------------------------------------------------------/
PCTRL    EQU   *
         CLI   0(R8),C' '          Byte = SPACE?
         BNE   CTRBEG              NO, found begin position
         LA    R8,1(R8)            Bump up to next byte of VAR1
         BCT   R7,PCTRL            LOOP for blanks
*                                  all blank, nothing to center
         B     PCENTERX            Done...
CTRBEG   EQU   *
         L     R4,VALL1            VAR1 Length
         SR    R4,R7
         A     R4,=F'1'            Starting position
*     * /--------------------------------------------------------/
*     * /* Determine content end                                */
*     * /--------------------------------------------------------/
         L     R7,VALL1            VAR1 Length
         L     R8,VALP1            VAR1 Address
         AR    R8,R7               Add VAR1 length
         S     R8,=F'1'            VAR1 end address
PCTRE    EQU   *
         CLI   0(R8),C' '          Byte = SPACE?
         BNE   CTREND              NO, found end   position
         BCTR  R8,0                Bump down byte position of VAR1
         BCT   R7,PCTRE            LOOP for blanks
*                                  all blank, nothing to center
         B     PCENTERX            Done...
*
         EJECT
CTREND   EQU   *
         LR    R5,R7               Ending   position
*     * /--------------------------------------------------------/
*     * /* Determine content length                             */
*     * /--------------------------------------------------------/
         SR    R5,R4               End - Start Pos
         A     R5,=F'1'            adj for content length
*     * /--------------------------------------------------------/
*     * /* Determine content starting address                   */
*     * /--------------------------------------------------------/
         L     R7,VALL1            VAR1 Length
         L     R8,VALP1            VAR1 Address
         AR    R8,R4
         BCTR  R8,0                content starting address
*     * /--------------------------------------------------------/
*     * /* Determine center starting position                   */
*     * /--------------------------------------------------------/
         L     R3,VALL1            VAR1 Length
         SR    R3,R5                - content length
         SRL   R3,1                Divide by 2 for centering
*     * /--------------------------------------------------------/
*     * /* Determine center starting address                    */
*     * /--------------------------------------------------------/
         LA    R6,VARWORK          Address of VARWORK
         AR    R6,R3               adjust address
*     * /--------------------------------------------------------/
*     * /* Move content to center of VAR2                       */
*     * /--------------------------------------------------------/
         BCTR  R5,0                Adjust for EXecute
         EX    R5,MVCR8$6          Execute R8-R6 MVC
*MVCR8$6  MVC   0(0,R6),0(R8)
PCENTERX EQU   *
         BAL   R5,S#UVAR2          Update VAR2
         B     ERRDONE             Successful Request
*
         EJECT
*     * /********************************************************/
*22   * /* Function: LJUST                  REQ OPT             */
*     * /********************************************************/
*     * /* Left justify string content                          */
*     * /* - R5, R6                   Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /********************************************************/
PLJUST   EQU   *                   Process LJUST
         BAL   R5,S#GVAR1          Get VAR1
         BAL   R5,S#CHKV2          Reset VAR2 when $NOVAR2
*     * /--------------------------------------------------------/
*     * /* Determine content begin                              */
*     * /--------------------------------------------------------/
PLJSTL   EQU   *
         CLI   0(R8),C' '          Byte = SPACE?
         BNE   PLJBEG              NO, found begin position
         LA    R8,1(R8)            Bump up   byte position of VAR1
         BCT   R7,PLJSTL           LOOP for blanks
         L     R7,VALL1            Reset VAR1 length
*                                  all blank, nothing to justify
         BAL   R5,S#UVAR2          Update VAR2
         B     ERRDONE             Successful Request
PLJBEG   EQU   *
*                                  R8=address of content start
*                                  R5=start of content position
*                                  R7=length of remaining content
         L     R5,VALL1            VAR1 Length
         SR    R5,R7                - current position
         A     R5,=F'1'            Adjust
*     * /--------------------------------------------------------/
*     * /* Determine left  justify starting address             */
*     * /--------------------------------------------------------/
         LA    R6,VARWORK          Address of VARWORK
*     * /--------------------------------------------------------/
*     * /* Right justify content into VAR2                      */
*     * /--------------------------------------------------------/
*        L     R8,VALP1            Address of VAR1 content
         BCTR  R7,0                Adjust for EXecute
         EX    R7,MVCR8$6          Execute R8-R6 MVC
*MVCR8$6  MVC   0(0,R6),0(R8)
         L     R7,VALL1            Reset VAR1 length
PLJUSTX  EQU   *
         BAL   R5,S#UVAR2          Update VAR2
         B     ERRDONE             Successful Request
*
         EJECT
*     * /********************************************************/
*23   * /* Function: RJUST                  REQ OPT             */
*24   * /* Function: ZFILL                  REQ OPT             */
*     * /********************************************************/
*     * /* RJUST Right justify string content                   */
*     * /* ZFILL Zero Fill string content after right justify   */
*     * /* - R5, R6                   Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /********************************************************/
PRJUST   EQU   *                   Process RJUST
         BAL   R5,S#GVAR1          Get VAR1
         BAL   R5,S#CHKV2          Reset VAR2 when $NOVAR2
*     * /--------------------------------------------------------/
*     * /* Determine content end                                */
*     * /--------------------------------------------------------/
         AR    R8,R7               Add VAR1 length
         BCTR  R8,0                VAR1 end address
PRJSTL   EQU   *
         CLI   0(R8),C' '          Byte = SPACE?
         BNE   PRJEND              NO, found end   position
         BCTR  R8,0                Bump down byte position of VAR1
         BCT   R7,PRJSTL           LOOP for blanks
         L     R7,VALL1            Reset VAR1 length
*                                  all blank, nothing to move
         BAL   R5,S#UVAR2          Update VAR2
         B     ERRDONE             Successful Request
PRJEND   EQU   *
*                                  R7=length of content
*     * /--------------------------------------------------------/
*     * /* Determine right justify starting address             */
*     * /--------------------------------------------------------/
         LA    R6,VARWORK          Address of VARWORK
         A     R6,VALL1             + Length of VAR1
         SR    R6,R7                - RJUST content
*     * /--------------------------------------------------------/
*     * /* Right justify content into VAR2                      */
*     * /--------------------------------------------------------/
         L     R8,VALP1            Address of VAR1 content
         BCTR  R7,0                Adjust for EXecute
         EX    R7,MVCR8$6          Execute R8-R6 MVC
*MVCR8$6  MVC   0(0,R6),0(R8)
         EJECT
*     * /--------------------------------------------------------/
*     * /* Zero Fill Request?                                   */
*     * /--------------------------------------------------------/
         CLC   =C'ZFILL ',FUNCT    ZFILL?                               LB1100y
         BNE   PRJX                NO, done.
         L     R7,VALL1            YES. Reset VAR1 length
         LA    R6,VARWORK
PRJZF    EQU   *
         CLI   0(R6),C' '          Byte = SPACE?
         BNE   PRJX                NO, Done.
         MVI   0(R6),C'0'          YES, Zero Fill
         LA    R6,1(R6)            Bump up
         BCT   R7,PRJZF            LOOP for blanks
PRJX     EQU   *
         L     R7,VALL1            Reset VAR1 length
PRJUSTX  EQU   *
         BAL   R5,S#UVAR2          Update VAR2
         B     ERRDONE             Successful Request
*
         EJECT
*     * /********************************************************/
*25   * /* Function: WORDS                  REQ NO              */
*     * /********************************************************/
*     * /* Count number of contigous content separated by       */
*     * /* blanks in a string.  Count is returned in return     */
*     * /* code.                                                */
*     * /* - R5                       Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=#     Number of WORDS                           */
*     * /* - RC=4022  VAR2 not required                         */
*     * /********************************************************/
PWORDS   EQU   *                   Process WORDS
*     * /--------------------------------------------------------/
*     * /* No   VAR2 content in position 1 needed               */
*     * /--------------------------------------------------------/
         CLC   VAR2,=C'$NOVAR2 '   VAR2 none keyword?
         BE    PWORDS#             YES, continue...
         CLI   VAR2,C' '           VAR2 name captured?
         BNE   ERR4022             YES, VAR2 not needed
PWORDS#  EQU   *
         BAL   R5,S#GVAR1          Get VAR1
         LA    R5,0                Word Count
*     * /--------------------------------------------------------/
*     * /* Count WORDS, non-blank contig blocks                 */
*     * /--------------------------------------------------------/
PWORDBLK EQU   *
         CLI   0(R8),C' '          BLANK Character ?
         BNE   PWORDFND            NO, found begin of WORD
PWORD000 EQU   *
         LA    R8,1(R8)            YES, Bump up position of VAR1
         BCT   R7,PWORDBLK         Keep looking until no BLANK
         B     PWORDSX             Done... Return
PWORDFND EQU   *                   WORD found
         LA    R5,1(R5)            Add 1 to count
         B     PWORD002            Go find next BLANK
PWORDNXT EQU   *
         CLI   0(R8),C' '          BLANK Character ?
         BE    PWORD000            YES, look for next word
PWORD002 EQU   *
         LA    R8,1(R8)            NO, bump up position of VAR1
         BCT   R7,PWORDNXT         Keep looking until BLANK
         B     PWORDSX             Done... Return
*     * /--------------------------------------------------------/
*     * /* Return WORD count                                    */
*     * /--------------------------------------------------------/
PWORDSX  EQU   *
         LR    R15,R5              Return WORD count
         B     ERRDONEF            Successful Request - function
*
         EJECT
*     * /********************************************************/
*26   * /* Function: GEN#                   REQ NO              */
*65   * /* Function: TDSN                   REQ NO              */
*66   * /* Function: NOW                    REQ NO              */      LB1100z
*75   * /* Function: UTDSN                  REQ NO              */      LB1105g
*     * /********************************************************/
*     * /* GEN# Generate a number (000-999)                     */
*     * /* TDSN Generate temp DSN nodes                         */
*     * /* NOW  Current system date and time                    */      LB1100z
*     * /* UTDSN Generate userid.var1.Dyyjjj.Thhmmss DSN        */      LB1105g
*     * /* - R0, R1, R5               Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /* - RC=4022  VAR2 not required                         */
*     * /********************************************************/
PGEN#    EQU   *                   Process GEN#
PTDSN    EQU   *                   Process TSDN
PNOW     EQU   *                   Process NOW                          LB1105z
PUTDSN   EQU   *                   Process UTDSN                        LB1105g
*     * /--------------------------------------------------------/
*     * /* No   VAR2 content in position 1 needed               */
*     * /--------------------------------------------------------/
         CLC   VAR2,=C'$NOVAR2 '   VAR2 none keyword?
         BE    PGEN##              YES, continue...
         CLI   VAR2,C' '           VAR2 name captured?
         BNE   ERR4022             YES, VAR2 not needed
PGEN##   EQU   *
         BAL   R5,S#GVAR1N         Get VAR1, do not check for NULL
         TIME  DEC                 Time in R0  HH_MM_SS_TH
*                                  Date in R1  00_YY_DD_DC
         ST    R0,WORKAREA         Store R0 to fullword
         MVO   DW(5),WORKAREA      Pack via MVO into 5 bytes
         OI    DW+4,X'0F'          ... with an F zone
         UNPK  UNPKAREA(9),DW(5)   Unpack 9 bytes 0HHMMSSTH
*
         CLC   =C'TDSN ',FUNCT     TDSN function ?                      LB1100y
         BE    PTDSNC              Yes, go to TDSNC
         CLC   =C'NOW ',FUNCT      NOW  function ?                      LB1100y
         BE    PNOWC               Yes, go to PNOWC
         CLC   =C'UTDSN ',FUNCT    UTDSN function ?                     LB1105g
         BE    PUTDSNC             Yes, go to UTDSNC                    LB1105g
*                                  No, must be GEN#
         MVC   VARWORK(3),UNPKAREA+6  Use STH portion for gen number
         MVC   VALL1,=F'3'         VAR LENGTH = 3
         LA    R7,3                VAR1 value len = 3
         B     PGEN#X              Done...
*
         EJECT                                                          LB1105g
*     * /--------------------------------------------------------/      LB1105g
*     * /* VARWORK  -->   USERID + VAR1                         */      LB1105g
*     * /--------------------------------------------------------/      LB1105g
PUTDSNC  EQU   *                                                        LB1105g
         BAL   R14,GETUSRID        Get Prefix and Length                LB1105g
         SR    R5,R5               Clear R5 for IC                      LB1105g
         IC    R5,DPFXL            R5=Prefix Length                     LB1105g
         LTR   R5,R5               Prefix Length = 0?                   LB1105g
         BZ    PTDSNC              Yes, ignore prefix and go to PTDSNC  LB1105g
*                                  No, move prefix to VARWORK           LB1105g
         MVC   VARWORK(8),DPFX     Move prefix to VARWORK               LB1105g
         LA    R6,VARWORK          R5=Start addr of VARWORK             LB1105g
*                                  Append '.'      to VARWORK           LB1105g
         AR    R6,R5               Adjust VARWORK position by DPFXL     LB1105g
         MVI   0(R6),C'.'          Place '.' into VARWORK               LB1105g
         LA    R6,1(R6)            Bump up VARWORK position             LB1105g
         LA    R5,1(R5)            R5=temp VAR1 length                  LB1105g
*                                  Append VAR1     to VARWORK           LB1105g
         BCTR  R7,0                Adjust for EXecute                   LB1105g
         EX    R7,MVCR8$6#         Append VAR1 into VARWORK             LB1105g
*MVCR8$6# MVC   0(0,R6),0(R8)       MVC R8 to R6                        LB1105g
         LA    R7,1(R7)            Reset R7 to original value           LB1105g
         AR    R7,R5               R7=Adjusted VAR1 length              LB1105g
         B     PDTSNTS             Add time-stamp                       LB1105g
*                                                                       LB1105g
         EJECT
*     * /--------------------------------------------------------/
*     * /* VARWORK  -->   VAR1 + Dyyjjj.Thhmmss                 */
*     * /--------------------------------------------------------/
PTDSNC   EQU   *
         BCTR  R7,0                Adjust for EXecute
         EX    R7,MVCVARWK         Execute VARWORK value move
*MVCVARWK MVC   VARWORK(0),0(R8)    Save VARWORK  value
         LA    R7,1(R7)            Reset R7 to original value
PDTSNTS  EQU   *                   Append time-stamp to VARWORK         LB1105g
         LA    R5,VARWORK          Start addr - VARWORK
         AR    R5,R7               Point to end of VAR1 in VARWORK
         LTR   R7,R7               VAR1 data?
         BZ    PTDSN1              No, go move time date to DSN
         BCTR  R5,0                Yes, back up and check for '.'
         CLI   0(R5),C'.'          Node at end of VAR1?
         BE    PTDSN0              Yes, go move time date to DSN
         LA    R5,1(R5)            No, add node to DSN
         LA    R7,1(R7)                and adjust VAR1L
         MVI   0(R5),C'.'
PTDSN0   EQU   *
         LA    R5,1(R5)            Point to end of VAR1 in VARWORK
PTDSN1   EQU   *
         MVI   7(R5),C'T'          'Thhmmss'
         MVC   8(6,R5),UNPKAREA+1
         ST    R1,WORKAREA         Date in R1  00_YY_DD_DC
         UNPK  UNPKAREA(5),WORKAREA+1(3)  Unpack YY_DD_DC
         OI    UNPKAREA+4,X'F0'    F-zone
         MVI   0(R5),C'D'          'Dyyjjj.'
         MVC   1(5,R5),UNPKAREA
         MVI   6(R5),C'.'
         LA    R7,14(R7)           Adjust to new length
         ST    R7,VALL1
         B     PTDSNX              Done...
*
         EJECT
*     * /--------------------------------------------------------/
*     * /*                0....5....0....5....0....5....0....5..*/
*     * /* VARWORK  -->   MM/DD/CCYY.JJJ HH:MM:SS.TT 6 DAYOFWEEK*/
*     * /--------------------------------------------------------/
PNOWC    EQU   *
         MVC   VARWORK+15(02),UNPKAREA+1     'hh:mm:ss.tt'
         MVI   VARWORK+17,C':'
         MVC   VARWORK+18(02),UNPKAREA+3
         MVI   VARWORK+20,C':'
         MVC   VARWORK+21(02),UNPKAREA+5
         MVI   VARWORK+23,C'.'
         MVC   VARWORK+24(02),UNPKAREA+7
         ST    R1,WORKAREA         Date in R1  II_YY_DD_DC
         UNPK  UNPKAREA(7),WORKAREA+0(4)     Unpack IIYYDDD
         OI    UNPKAREA+6,X'F0'            F-zone   IIYYDDD
         MVC   VARWORK+00(07),UNPKAREA       IIYYDDD
         PACK  FW(2),UNPKAREA(2)             II
         AP    FW(2),=P'19'                  CC = II + 19
         UNPK  UNPKAREA(2),FW(2)             CC
         OI    UNPKAREA+1,X'F0'    F-zone    CC
         MVI   VARWORK+10,C'.'
         MVC   VARWORK+11(03),UNPKAREA+4    'JJJ'
         MVI   VARWORK+05,C'/'
         MVC   VARWORK+06(04),UNPKAREA      'CCYY'
         MVI   DCCCCYY,C'0'                 0CCYY
         MVC   DCCCCYY+1(4),UNPKAREA
         MVC   DCJJJ,UNPKAREA+4             DCJJJ
*     * /--------------------------------------------------------/
*     * /* Determine if CCYY is a leap year                     */
*     * /--------------------------------------------------------/
*        input  DCCCCYY  0CCYY
*        output DC#LEAP  0=common year     1=leap year
*               DC#YDAYS 365=common year   366=leap year
         BAL   R5,S#ISLPY          Determine Leap Year
*     * /--------------------------------------------------------/
*     * /* Determine MM and DD from JJJ                         */
*     * /--------------------------------------------------------/
*        input  DC#LEAP PKJJJ
*        Work   FW, R3, R4,
*        output DC#TRM  DCMMM DCDDD
*               R4=addr of MONTHNAME  R6=Length of MONTHNAME
         PACK  PKJJJ,DCJJJ
         BAL   R5,S#J2MD                     Determine MM DD from JJJ
         MVC   VARWORK+00(02),DCMMM+1        'MM'
         MVI   VARWORK+02,C'/'
         MVC   VARWORK+03(02),DCDDD+1        'DD'
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Call S#CDOW to calculate DOW                         */
*     * /--------------------------------------------------------/
*        input  DCDDD   DCCCCCYY  DCMMM  DC#TRM
*        output DCCDOW  DCCWDAY
*        Working fields   DC#YR4 DC#DD DC#YR100 DC#YR400 DC#DOW
         BAL   R5,S#CDOW
         MVC   VARWORK+27(01),DCCDOW         '3'
         MVC   VARWORK+29(09),DCCWDAY        'Wednesday'
         LA    R7,38               VAR1 value len = 38
         ST    R7,VALL1
         B     PNOWX               Done...
*
PGEN#X   EQU   *
PTDSNX   EQU   *
PNOWX    EQU   *
         MVI   VAR2,C' '           Force use of VAR1
         BAL   R5,S#UVAR2          Update VAR2
         B     ERRDONE             Successful Request
*
         EJECT
*     * /********************************************************/
*27   * /* Function: DD2DSN                 REQ REQ             */
*     * /********************************************************/
*     * /* Obtain 1st DSN from allocated DD                     */
*     * /* - R3, R4, R5, R6           Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /* - RC=8     DD not found                              */
*     * /* - RC=4015  VAR2 is required                          */
*     * /********************************************************/
PDD2DSN  EQU   *                   Process DD2DSN
         BAL   R5,S#GVAR1          Get VAR1
PDDFIND  EQU   *
         CLC   VALL1,=F'8'         DDNAME > 8 charaters?
         BH    PDDF8               Yes, override to 8 charaters
         B     PDDCHKV2            No, check for var2
PDDF8    EQU   *
         MVC   VALL1,=F'8'
PDDCHKV2 EQU   *
         CLI   VAR2,C' '           Do I have a VAR2?
         BE    ERR4015             No, VAR2 required error
FINDDDN  EQU   *                   YES, find DDN in TIOT
         L     R4,CVTPTR          xAddress of CVT
         USING CVT,R4             xTell Assembler, CVT
         L     R3,CVTTCBP         7Addrs (2 FW) pointing to TCB
         DROP  R4                 x
         MVC   DW,0(R3)           7Save to DW
         L     R3,DW+4            7Take 2nd FW as TCB Addr
         USING TCB,R3             7Tell Assembler, TCB
         L     R4,TCBTIO          xLoad TIOT Address
         DROP  R3                 7
         USING TIOT,R4            xTell Assembler, TIOT
         LA    R3,TIOENTRY        7Load TIOENTRY address
         USING TIOENTRY,R3        7Tell Assembler, TIOENTRY
         L     R6,VALL1
         BCTR  R6,0                Adjust for EXecute CLC
LOOPDDN  EQU   *                   YES, find DDN in TIOT
         CLI   TIOELNGH,0          End of TIOT?
         BE    DSNNOFND            Yes, DDN not found, DSN=blanks
*                                  No, keep looking for DDN...
         EX    R6,CHKDDN           DDNAME = TIOT entry?
*CHKDDN   CLC   0(0,R8),TIOEDDNM    DDNAME = TIOT entry?
         BE    GOODDDN             Yes, found it!
         SR    R5,R5               Clear R5
         IC    R5,TIOELNGH         TIOT Entry Length
         AR    R3,R5               Bump to next TIOT entry
         B     LOOPDDN
*HKDDN   CLC   0(0,R8),TIOEDDNM    DDNAME = TIOT entry?
CHKDDN   CLC   0(0,R8),4(R3)       DDNAME = TIOT entry?
         EJECT
GOODDDN  EQU   *                   Found DDN
         XC    FW,FW               Clear FW (fullword)
         MVC   FW+1(3),TIOEJFCB    FW contains addr of JFCB for DDN
         L     R5,FW               R8 = JFCB addrs
         A     R5,=F'16'           R8 = JFCB+16 (for offset)
         USING JFCB,R5             Tell Assembler, JFCB
         MVC   VARWORK(44),JFCBDSNM   Move associated dataset name
         DROP  R4
         DROP  R3
         DROP  R5
CALCINIT EQU   *                   Calculate DDSN Init
         LA    R4,0                DSN Length
         LA    R3,44               Max Length, loop control
         LA    R5,VARWORK          DSN in VARWORK
CALCDSNL EQU   *                   Calculate DDSN length
         CLI   0(R5),C' '          Is it a BLANK?
         BE    CALCEXIT            Yes, we have a length
         LA    R4,1(R4)            No, look at next byte
         LA    R5,1(R5)
         BCT   R3,CALCDSNL         Look again...
CALCEXIT EQU   *
         LR    R7,R4               Store new length
         BAL   R5,S#UVAR2          Update VAR2
         B     PDD2DSNX            Return
DSNNOFND EQU   *
         MVC   VARWORK(12),=C'DD.NOT.FOUND'
         L     R7,=F'12'           VAR2 Length
         BAL   R5,S#UVAR2          Update VAR2
         LA    R15,8               SET RC to 8, DD not found
PDD2DSNX EQU   *
         B     ERRDONE             Successful Request
         EJECT
*     * /********************************************************/
*28   * /* Function: JOBINFO                REQ NO              */
*     * /********************************************************/
*     * /* JOB Information from TIOT in string                  */
*     * /*   - JOB NAME   8-bytes                               */
*     * /*   - PROC NAME  8-bytes                               */
*     * /*   - STEP NAME  8-bytes                               */
*     * /* - R3, R4, R5               Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /* - RC=8     DD not found                              */
*     * /* - RC=4022  VAR2 not required                         */
*     * /********************************************************/
PJOBINFO EQU   *                   Process JOBINFO
*     * /--------------------------------------------------------/
*     * /* No   VAR2 content in position 1 needed               */
*     * /--------------------------------------------------------/
         CLC   VAR2,=C'$NOVAR2 '   VAR2 none keyword?
         BE    PJOBINF#            YES, continue...
         CLI   VAR2,C' '           VAR2 name captured?
         BNE   ERR4022             YES, VAR2 not needed
PJOBINF# EQU   *
         BAL   R5,S#GVAR1N         Get VAR1, do not check for NULL
         L     R4,CVTPTR           Address of CVT
         USING CVT,R4              Tell Assembler, CVT
         L     R3,CVTTCBP          Addrs (2 FW) pointing to TCB
         DROP  R4
         MVC   DW,0(R3)            Save to DW
         L     R3,DW+4             Take 2nd FW as TCB Addr
         USING TCB,R3              Tell Assembler, TCB
         L     R4,TCBTIO           Load TIOT Address
         DROP  R3
         USING TIOT,R4             Tell Assembler, TIOT
         LA    R3,TIOENTRY         Load TIOENTRY address
         USING TIOENTRY,R3         Tell Assembler, TIOENTRY
*
         MVC   VARWORK(24),TIOCNJOB JOB NUMBER and other info
         MVC   VALL1,=F'24'
         L     R7,=F'24'
         MVI   VAR2,C' '
PJOBINFX EQU   *
         BAL   R5,S#UVAR2          Update VAR2
         B     ERRDONE             Successful Request
*
         EJECT
*     * /********************************************************/
*     * /* Function: SETVAR                                     */
*     * /********************************************************/
*     * /* SET VAR1 via PARM data                               */
*     * /* - R5                       Working register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
PSETVAR  EQU   *                   Process SETVAR
         BAL   R5,S#GVAR1          Get VAR1
*
         MVC   VARWORK,VARSET1     SETVAR into VARWORK
*        MVC   VAL1,VARSET1        SETVAR into VAR1
         MVC   VALL1,VARSET1L      SETVAR length
*        L     R7,VARSET1L
         L     R7,VARSET1L
         LTR   R7,R7
         BZ    ERR4010             No SETVAR content
         MVI   VAR2,C' '
         BAL   R5,S#UVAR2          Update VAR2
         B     ERRDONE             Successful Request
*
         EJECT
*     * /********************************************************/
*29   * /* Function: DAYSMM                 REQ NO              */
*     * /********************************************************/
*     * /* Number of DAYS in specified MMCCYY                   */
*     * /* - R3, R4, R5               Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /* - RC=4022  VAR2 not required                         */
*     * /* - RC=4040  Invalid length (VAR1)                     */
*     * /* - RC=4034  Not numeric (VAR1)                        */
*     * /* - RC=4049  Invalid MM (VAR1)                         */
*     * /********************************************************/
PDAYSMM  EQU   *                   Process DAYSMM
*     * /--------------------------------------------------------/
*     * /* No   VAR2 content in position 1 needed               */
*     * /--------------------------------------------------------/
         CLC   VAR2,=C'$NOVAR2 '   VAR2 none keyword?
         BE    PDAYSMM#            YES, continue...
         CLI   VAR2,C' '           VAR2 name captured?
         BNE   ERR4022             YES, VAR2 not needed
PDAYSMM# EQU   *
         BAL   R5,S#GVAR1          Get VAR1
         ST    R7,R7HOLD           Save VAR1 value Length
         ST    R8,R8HOLD           Save VAR1 value Address
         LA    R5,0
PDMML    EQU   *
         CLI   0(R8),C' '          Blank delimiter?
         BE    PDMMLX              Yes, go process
         LA    R8,1(R8)            Bump up VAR1
         LA    R5,1(R5)            Bump up VAR1 length
         BCT   R7,PDMML            ...do it again
PDMMLX   EQU   *
         LR    R7,R5               Implied VAR1 length
*     * /--------------------------------------------------------/
*     * /* Check VAR1 for proper length                         */
*     * /--------------------------------------------------------/
         C     R7,=F'6'            VAR1 length = 6 ?
         BNE   ERR4040             No, Error- VAR1 wrong length
         L     R8,R8HOLD           Restore VAR1 value Address
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Check VAR1 for numerics based on LENGTH (R7)         */
*     * /--------------------------------------------------------/
         BAL   R5,S#ISNUM          Numeric Test
         C     R15,=F'4000'        Numeric?
         BE    ERR4034             NO, error
*                                  YES, Continue...
         L     R7,R7HOLD           Restore VAR1 value Length
         L     R8,R8HOLD           Restore VAR1 value Address
*     * /--------------------------------------------------------/
*     * /* Store VAR1 date   (R8=MMCCYY)                        */
*     * /--------------------------------------------------------/
         MVI   DCMMM,C'0'          Save MM from VAR1
         MVC   DCMMM+1(2),0(R8)    ... to 0MM field
         MVI   DCCCCYY,C'0'        Save CCYY from VAR1
         MVC   DCCCCYY+1(4),2(R8)  ... to 0CCYY field
*     * /--------------------------------------------------------/
*     * /* Find Days in MM                                      */
*     * /--------------------------------------------------------/
         MVC   PALP1,=C'2MM '      Parm 1: Function Request             LB1100g
         LA    R1,DCMMM+1          Parm 2: MM (CL2 month)               LB1100g
         ST    R1,PALP2                                                 LB1100g
         LA    R1,PALLST           R1=Parm Addr List                    LB1100g
         L     R15,CUTILTBL        Call CUTILTBL                        LB1105b
         BALR  R14,R15                                                  LB1105b
         LTR   R15,R15             RC = 0 ?                             LB1100g
         BNZ   ERR4049             No, Error - Invalid MM               LB1100g
         L     R4,PALP3            Yes, MM entry start addr in Parm 3   LB1100g
         USING MPT,R4              Tell Assembler
         PACK  DW,MPTMMAXD         Pack number of days for month
         CLC   DCMMM+1(2),=C'02'   MM = FEB ?
         BNE   PDAYSMMX            NO, go to PDAYSMMX
*                                  YES, check for leap year
*     * /--------------------------------------------------------/
*     * /* Determine if CCYY is a leap year                     */
*     * /--------------------------------------------------------/
*        input  DCCCCYY  0CCYY
*        output DC#LEAP  0=common year     1=leap year
*               DC#YDAYS 365=common year   366=leap year
         BAL   R5,S#ISLPY          Determine Leap Year
         AP    DW,DC#LEAP          Adjust for leap year
PDAYSMMX EQU   *
         CVB   R15,DW              Convert to binary (R15)
         B     ERRDONEF            Successful Request - function
*
         EJECT
*     * /********************************************************/
*30   * /* Function: DAYSYY                 REQ NO              */
*31   * /* Function: ISLEAP                 REQ NO              */
*     * /********************************************************/
*     * /* DAYSYY  Number of days for year in CCYY              */
*     * /* ISLEAP  Determine if CCYY is a leap year             */
*     * /* - R3, R4, R5               Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /* - RC=4000  FALSE                                     */
*     * /* - RC=4001  TRUE                                      */
*     * /* - RC=4022  VAR2 not required                         */
*     * /* - RC=4040  Invalid length (VAR1)                     */
*     * /* - RC=4034  Not numeric (VAR1)                        */
*     * /********************************************************/
PDAYSYY  EQU   *                   Process DAYSYY
*     * /--------------------------------------------------------/
*     * /* No   VAR2 content in position 1 needed               */
*     * /--------------------------------------------------------/
         CLC   VAR2,=C'$NOVAR2 '   VAR2 none keyword?
         BE    PDAYSYY#            YES, continue...
         CLI   VAR2,C' '           VAR2 name captured?
         BNE   ERR4022             YES, VAR2 not needed
PDAYSYY# EQU   *
         BAL   R5,S#GVAR1          Get VAR1
         ST    R7,R7HOLD           Save VAR1 value Length
         ST    R8,R8HOLD           Save VAR1 value Address
         LA    R5,0
PDYYL    EQU   *
         CLI   0(R8),C' '          Blank delimiter?
         BE    PDYYLX              Yes, go process
         LA    R8,1(R8)            Bump up VAR1
         LA    R5,1(R5)            Bump up VAR1 length
         BCT   R7,PDYYL            ...do it again
PDYYLX   EQU   *
         LR    R7,R5               Implied VAR1 length
*     * /--------------------------------------------------------/
*     * /* Check VAR1 for proper length                         */
*     * /--------------------------------------------------------/
         C     R7,=F'4'            VAR1 length = 2 ?
         BNE   ERR4040             No, Error- VAR1 wrong length
         L     R8,R8HOLD           Restore VAR1 value Address
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Check VAR1 for numerics based on LENGTH (R7)         */
*     * /--------------------------------------------------------/
         BAL   R5,S#ISNUM          Numeric Test
         C     R15,=F'4000'        Numeric?
         BE    ERR4034             NO, Error
*                                  YES, Continue...
         L     R7,R7HOLD           Restore VAR1 value Length
         L     R8,R8HOLD           Restore VAR1 value Address
*     * /--------------------------------------------------------/
*     * /* Store VAR1 date   (R8=CCYY)                          */
*     * /--------------------------------------------------------/
         MVI   DCCCCYY,C'0'        Save CCYY from VAR1
         MVC   DCCCCYY+1(4),0(R8)  ... to 0CCYY field
*     * /--------------------------------------------------------/
*     * /* Determine if CCYY is a leap year                     */
*     * /--------------------------------------------------------/
*        input  DCCCCYY  0CCYY
*        output DC#LEAP  0=not leap year   1=leap year
*               DC#YDAYS 365=common year   366=leap year
         BAL   R5,S#ISLPY          Determine Leap Year
         CLC   =C'ISLEAP ',FUNCT   ISLEAP?                              LB1100y
         BE    LEAPTF              Yes, return TRUE/FALSE
         ZAP   DW,DC#YDAYS         No, return number of days in year
         CVB   R15,DW              Convert to binary (R15)
         B     ERRDONEF            Successful Request - function
LEAPTF   EQU   *
         CP    DC#LEAP,=P'1'       Leap Year?
         BE    ERR4001             Yes, TRUE
         B     ERR4000             NO, FALSE
*
         EJECT
*     * /********************************************************/
*32   * /* Function: CYJ-D8                 REQ OPT             */
*33   * /* Function: CYJ-DAY                REQ OPT             */
*34   * /* Function: CYJ-DOW                REQ OPT             */
*35   * /* Function: CYJ-mdcy               REQ OPT             */
*36   * /* Function: JCY-D8                 REQ OPT             */
*37   * /* Function: JCY-DAY                REQ OPT             */
*38   * /* Function: JCY-DOW                REQ OPT             */
*39   * /* Function: JCY-mdcy               REQ OPT             */
*     * /*                                                      */
*     * /*           Date Conversion                            */
*     * /********************************************************/
*     * /*    -D8    converts to monthname DD, CCYY             */
*     * /*    -DAY   converts to day name                       */
*     * /*    -DOW   converts to day of week number             */
*     * /* CYJ-*     converts to MMDDCCYY (any order)           */
*     * /* JCY-*     converts to MMDDCCYY (any order)           */
*     * /*                                                      */
*     * /*  Date input form specified by function bytes 1-3:    */
*     * /*  CYJ  --> CCYYJJJ                                    */
*     * /*  JCY  --> JJJCCYY                                    */
*     * /********************************************************/
*     * /* - R2, R3, R4, R5, R6       Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /* - RC=4071  Invalid FUNCTION                          */
*     * /* - RC=4072  Invalid FUNCTION                          */
*     * /* - RC=4040  Invalid length (VAR1)                     */
*     * /* - RC=4034  Not numeric (VAR1)                        */
*     * /* - RC=4051  Invalid JJJ (VAR1)                        */
*     * /********************************************************/
         EJECT
PJMDCY   EQU   *                   Process CYJ-*
*                                  Process JCJ-*
*     * /--------------------------------------------------------/
*     * /* Check for valid FUNCT in last 4 bytes and including  */
*     * /* valid letters in MDCY result build                   */
*     * /--------------------------------------------------------/
         LA    R2,FUNCT+4          Starting address
         LA    R3,4                Number of times
         CLC   FUNCT+4(4),BLANKS   BLANK ?
         BE    ERR4071             Yes, invalid function
PJCHKF   EQU   *
         CLI   0(R2),C'M'          Valid character?
         BE    PJCHKFNX            Yes, check next
         CLI   0(R2),C'D'          Valid character?
         BE    PJCHKFNX            Yes, check next
         CLI   0(R2),C'C'          Valid character?
         BE    PJCHKFNX            Yes, check next
         CLI   0(R2),C'Y'          Valid character?
         BE    PJCHKFNX            Yes, check next
         CLI   0(R2),C' '          Blank delimiter?
         BE    PJDATE              Yes, done, go process
         B     ERR4071             no, must be invalid function
PJCHKFNX EQU   *
         LA    R2,1(R2)
         BCT   R3,PJCHKF           Check next character...
*
         EJECT
PJDATE   EQU   *                   Process CYJ-D8,  JCY-D8
PJDAY    EQU   *                   Process CYJ-DAY, JCY-DAY
PJDOW    EQU   *                   Process CYJ-DOW, JCY-DOW
         BAL   R5,S#GVAR1          Get VAR1
         BAL   R5,S#CHKV2          Reset VAR2 when $NOVAR2
         ST    R7,R7HOLD           Save VAR1 value Length
         ST    R8,R8HOLD           Save VAR1 value Address
         LA    R5,0
PJDLL    EQU   *
         CLI   0(R8),C' '          Blank delimiter?
         BE    PJDLLX              Yes, go process
         LA    R8,1(R8)            Bump up VAR1
         LA    R5,1(R5)            Bump up VAR1 length
         BCT   R7,PJDLL            ...do it again
PJDLLX   EQU   *
         LR    R7,R5               Implied VAR1 length
*     * /--------------------------------------------------------/
*     * /* Check VAR1 for proper length                         */
*     * /--------------------------------------------------------/
         C     R7,=F'7'            VAR1 length = 7 ?
         BNE   ERR4040             No, Error- VAR1 wrong length
         L     R8,R8HOLD           Restore VAR1 value Address
*     * /--------------------------------------------------------/
*     * /* Check VAR1 for numerics based on LENGTH (R7)         */
*     * /--------------------------------------------------------/
         BAL   R5,S#ISNUM          Numeric Test
         C     R15,=F'4000'        Numeric?
         BE    ERR4034             NO, Error
*                                  YES, Continue...
         L     R7,R7HOLD           Restore VAR1 value Length
         L     R8,R8HOLD           Restore VAR1 value Address
*     * /--------------------------------------------------------/
*     * /* Store VAR1 date   (R8=CCYYJJJ or JJJCCYY)            */
*     * /--------------------------------------------------------/
*        input  VAR1
*        output DCCCCYY  DCJJJ
         BAL   R5,S#ICYJ           Store VAR1 date
         PACK  PKJJJ,DCJJJ         PACK JJJ of VAR1
*     * /--------------------------------------------------------/
*     * /* Determine if CCYY is leap year                       */
*     * /--------------------------------------------------------/
*        input  DCCCCYY  0CCYY
*        output DC#LEAP  0=not leap year   1=leap year
*               DC#YDAYS 365=common year   366=leap year
         BAL   R5,S#ISLPY          Determine Leap Year
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Check VAR1 for valid JJJ                             */
*     * /--------------------------------------------------------/
         CP    PKJJJ,DC#YDAYS      Valid MAX JJJ for Year
         BH    ERR4051             No, VAR1 invalid JJJ
         CP    PKJJJ,=P'1'         Valid MIN JJJ for Year
         BL    ERR4051             No, VAR1 invalid JJJ
*     * /--------------------------------------------------------/
*     * /* Determine MM and DD from JJJ                         */
*     * /--------------------------------------------------------/
*        input  DC#LEAP PKJJJ
*        Work   FW, R3, R4, R5, R6
*        output DC#TRM  DCMMM DCDDD
*               R4=addr of MONTHNAME  R6=Length of MONTHNAME
         BAL   R5,S#J2MD           Determine MM and DD from JJJ
*     * /--------------------------------------------------------/
*     * /* Start of result content build     ??LB               */
*     * /--------------------------------------------------------/
         LA    R2,VARWORK          Addr of VARWORK
         LA    R3,0                Length of Result
*     * /--------------------------------------------------------/
*     * /* Process D8,   DAY, DOW or MDCY result                */
*     * /--------------------------------------------------------/
         CLC   =C'D8 ',FUNCT+4     Date ? DECEMBER 31, 2020             LB1100y
         BE    DO#DATE             Yes, do DATE
         CLC   =C'DAY',FUNCT+4     Day ? MONDAY                         LB1100y
         BE    DO#DOWDY            Yes, do Weekday Name
         CLC   =C'DOW',FUNCT+4     DOW ? 3                              LB1100y
         BE    DO#DOW              Yes, do DOW
*                                  No, must be MDCY
         EJECT
*     * /--------------------------------------------------------/
*     * /* Build M D C Y J content based on FUNCT               */
*     * /--------------------------------------------------------/
DO#MDCY  EQU   *                   Build MDCY
         CLC   =C'MDCY-',FUNCT     MDCY- Function ?                     LB1100y
         BE    DO#MDCY3            Yes, apply MDCY- controls
         CLC   =C'DMCY-',FUNCT     DMCY- Function ?                     LB1100y
         BE    DO#MDCY3            Yes, apply MDCY- controls
         CLC   =C'CYMD-',FUNCT     CYMD- Function ?                     LB1100y
         BE    DO#MDCY3            Yes, apply MDCY- controls
         CLC   =C'CYDM-',FUNCT     CYDM- Function ?                     LB1100y
         BE    DO#MDCY3            Yes, apply MDCY- controls
         CLC   =C'CYJ-',FUNCT      CYJ-  Function?                      LB1100y
         BE    DO#MDCY4            Yes, apply CYJ-  controls
         CLC   =C'JCY-',FUNCT      JCY-  Function?                      LB1100y
         BE    DO#MDCY4            Yes, apply CYJ-  controls
         B     ERR4072             no, must be invalid function
DO#MDCY3 EQU   *
         LA    R4,FUNCT+5          MDCY- Function start addr
         LA    R6,3                Limit to 3
         B     DO#MDCY9
DO#MDCY4 EQU   *
         LA    R4,FUNCT+4          CYJ- Function start addr
         LA    R6,4                Limit to 4
         B     DO#MDCY9
*     * /--------------------------------------------------------/
*     * /* Call S#BMDCY to build M D C Y J content              */
*     * /--------------------------------------------------------/
*     * /* R2 - addr VARWORK                                    */
*     * /* R3 - Length of content in VARWORK                    */
*     * /* R4 - Addr of FUNCT+4                                 */
*     * /* R6 - Loop control for monthname                      */
*     * /--------------------------------------------------------/
DO#MDCY9 EQU   *
         BAL   R5,S#BMDCY
         B     PJMDCYX             Done
*
         EJECT
DO#DATE  EQU   *                   Build MONTHNAME
*     * /--------------------------------------------------------/
*     * /* Call S#BD8 to build DATE (monthname dd, ccyy)        */
*     * /--------------------------------------------------------/
*     * /* R2 - addr VARWORK                                    */
*     * /* R3 - Length of content in VARWORK                    */
*     * /* R4 - Addr of MONTHNAME                               */
*     * /* R6 - Loop control for monthname                      */
*     * /--------------------------------------------------------/
         BAL   R5,S#BD8
         B     PJMDCYX             Done
DO#DOW   EQU   *                   Build DOW
*     * /--------------------------------------------------------/
*     * /* Call S#CDOW to build DOW     3                       */
*     * /--------------------------------------------------------/
*        input  DCDDD   DCCCCCYY  DCMMM  DC#TRM
*        output DCCDOW  DCCWDAY
*        Working fields   DC#YR4 DC#DD DC#YR100 DC#YR400 DC#DOW
         BAL   R5,S#CDOW
         MVC   0(1,R2),DCCDOW      Move N
         LA    R2,1(R2)            Bump up VARWORK by 1 for 'n'
         LA    R3,1(R3)            Bump up VARWORKL by 1 for 'n'
         B     PJMDCYX             Done
DO#DOWDY EQU   *                   Build DOW Day Name
*     * /--------------------------------------------------------/
*     * /* Call S#CDOW to build weekday    Wednesday            */
*     * /--------------------------------------------------------/
*        input  DCDDD   DCCCCCYY  DCMMM  DC#TRM
*        output DCCDOW  DCCWDAY
*        Working fields   DC#YR4 DC#DD DC#YR100 DC#YR400 DC#DOW
         BAL   R5,S#CDOW
         LA    R4,DCCWDAY          R4=Day of Week name begin
         LA    R6,L'DCCWDAY        R6=Length of Week Day Name
DO#DOWNM EQU   *
         CLI   0(R4),C' '          Blank character?
         BE    PJMDCYX             Yes, done...
         MVC   0(1,R2),0(R4)       Move day of week name character
         LA    R2,1(R2)            Bump up VARWORK
         LA    R4,1(R4)            Bump up day of week name
         LA    R3,1(R3)            Bump up VARWORKL
         BCT   R6,DO#DOWNM         Do next DOW name character
         B     PJMDCYX             Done
PJMDCYX  EQU   *
         LR    R7,R3               Result length in R3
         BAL   R5,S#UVAR2          Update VAR2
         L     R7,R7HOLD           Restore VAR1 value Length
         B     ERRDONE             Successful Request
*
         EJECT
*     * /********************************************************/
*40   * /* Function: MDCY-D8                REQ OPT             */
*41   * /* Function: MDCY-DAY               REQ OPT             */
*42   * /* Function: MDCY-DOW               REQ OPT             */
*43   * /* Function: MDCY-cyj               REQ OPT             */
*44   * /* Function: DMCY-D8                REQ OPT             */
*45   * /* Function: DMCY-DAY               REQ OPT             */
*46   * /* Function: DMCY-DOW               REQ OPT             */
*47   * /* Function: DMCY-cyj               REQ OPT             */
*48   * /* Function: CYMD-D8                REQ OPT             */
*49   * /* Function: CYMD-DAY               REQ OPT             */
*50   * /* Function: CYMD-DOW               REQ OPT             */
*51   * /* Function: CYMD-cyj               REQ OPT             */
*52   * /* Function: CYDM-D8                REQ OPT             */
*53   * /* Function: CYDM-DAY               REQ OPT             */
*54   * /* Function: CYDM-DOW               REQ OPT             */
*55   * /* Function: CYDM-cyj               REQ OPT             */
*     * /*                                                      */
*     * /*           Date Conversion                            */
*     * /********************************************************/
*     * /*      -D8  converts   to monthname DD, CCYY           */
*     * /*      -DAY converts   to day name                     */
*     * /*      -DOW converts   to day of week number           */
*     * /*  MDCY-*   converts   to YYCCJJJ  (any order)         */
*     * /*  DMCY-*   converts   to YYCCJJJ  (any order)         */
*     * /*  CYMD-*   converts   to YYCCJJJ  (any order)         */
*     * /*  CYDM-*   converts   to YYCCJJJ  (any order)         */
*     * /*                                                      */
*     * /*  Date input form specified by function bytes 1-4:    */
*     * /*  MDCY --> MMDDCCYY                                   */
*     * /*  DMCY --> DDMMCCYY                                   */
*     * /*  CYMD --> CCYYMMDD                                   */
*     * /*  CYDM --> CCYYDDMM                                   */
*     * /********************************************************/
*     * /* - R3, R4, R5, R6           Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /* - RC=4073  Invalid FUNCTION                          */
*     * /* - RC=4040  Invalid length (VAR1)                     */
*     * /* - RC=4047  Invalid DD (VAR1)                         */
*     * /* - RC=4034  Not numeric (VAR1)                        */
*     * /* - RC=4049  Invalid MM (VAR1)                         */
*     * /* - RC=4050  Invalid DD for MM (VAR1)                  */
*     * /* - RC=4051  Invalid JJJ (VAR1)                        */
*     * /********************************************************/
*
         EJECT
PMDCYJ   EQU   *                   Process MDCY-*
*                                  Process DMCY-*
*                                  Process CYMD-*
*                                  Process CYDM-*
*     * /--------------------------------------------------------/
*     * /* Check for valid FUNCT in last 3 bytes and including  */
*     * /* valid letters in MDCY result build                   */
*     * /--------------------------------------------------------/
         LA    R2,FUNCT+5          Starting address
         LA    R3,3                Number of times
         CLC   FUNCT+5(3),BLANKS   BLANK ?
         BE    ERR4073             Yes, invalid function
PMCHKF   EQU   *
         CLI   0(R2),C'J'          Valid character?
         BE    PMCHKFNX            Yes, check next
         CLI   0(R2),C'C'          Valid character?
         BE    PMCHKFNX            Yes, check next
         CLI   0(R2),C'Y'          Valid character?
         BE    PMCHKFNX            Yes, check next
         CLI   0(R2),C' '          Blank delimiter?
         BE    PMDATE              Yes, done, go process
         B     ERR4073             no, must be invalid function
PMCHKFNX EQU   *
         LA    R2,1(R2)
         BCT   R3,PMCHKF           Check next character...
*
         EJECT
PMDATE   EQU   *                   Process MDCY-D8  DMCY-D8
*                                          CYMD-D8  CYDM-D8
PMDAY    EQU   *                   Process MDCY-DAY DMCY-DAY
*                                          CYMD-DAY CYDM-DAY
PMDOW    EQU   *                   Process MDCY-DOW DMCY-DOW
*                                          CYMD-DOW CYDM-DOW
         BAL   R5,S#GVAR1          Get VAR1
         BAL   R5,S#CHKV2          Reset VAR2 when $NOVAR2
         ST    R7,R7HOLD           Save VAR1 value Length
         ST    R8,R8HOLD           Save VAR1 value Address
         LA    R5,0
PMDDL    EQU   *
         CLI   0(R8),C' '          Blank delimiter?
         BE    PMDDLX              Yes, go process
         LA    R8,1(R8)            Bump up VAR1
         LA    R5,1(R5)            Bump up VAR1 length
         BCT   R7,PMDDL            ...do it again
PMDDLX   EQU   *
         LR    R7,R5               Implied VAR1 length
*     * /--------------------------------------------------------/
*     * /* Check VAR1 for proper length                         */
*     * /--------------------------------------------------------/
         C     R7,=F'8'            VAR1 length = 8 ?
         BNE   ERR4040             No, Error- VAR1 wrong length
         L     R8,R8HOLD           Restore VAR1 value Address
*     * /--------------------------------------------------------/
*     * /* Check VAR1 for numerics based on LENGTH (R7)         */
*     * /--------------------------------------------------------/
         BAL   R5,S#ISNUM          Numeric Test
         C     R15,=F'4000'        Numeric?
         BE    ERR4034             NO, Error
*                                  YES, Continue...
         L     R7,R7HOLD           Restore VAR1 value Length
         L     R8,R8HOLD           Restore VAR1 value Address
*     * /--------------------------------------------------------/
*     * /* Store VAR1 date   (R8=MMDDCCYY or variation of MDCY) */
*     * /--------------------------------------------------------/
*        input  VAR1
*        output DCCCCYY  DCJJJ
         BAL   R5,S#IMDCY          Store VAR1 date
*     * /--------------------------------------------------------/
*     * /* Determine if CCYY is leap year                       */
*     * /--------------------------------------------------------/
*        input  DCCCCYY  0CCYY
*        output DC#LEAP  0=not leap year   1=leap year
*               DC#YDAYS 365=common year   366=leap year
         BAL   R5,S#ISLPY          Determine Leap Year
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Determine JJJ from MM DD                             */
*     * /--------------------------------------------------------/
*        input  DC#LEAP  n packed
*               DCDDD    0DD                                            LB1100z
*               DCMMM    0MM                                            LB1100z
*        output DC#JJJ   JJJ packed
*               DC#TRM   n   packed
*               DC#DD    DD  packed
*               DCJJJ    JJJ
*               DC#MDAYS 0nn packed                                     LB1100b
*               R4=addr of MONTHNAME
*               R6=Length of MONTHNAME
         BAL   R5,S#MD2J           MMDD to JJJ
*     * /--------------------------------------------------------/
*     * /* Start of result content build                        */
*     * /--------------------------------------------------------/
         LA    R2,VARWORK          Addr of VARWORK
         LA    R3,0                Length of Result
*     * /--------------------------------------------------------/
*     * /* Process D8,   DAY, DOW or CYJ  result                */
*     * /--------------------------------------------------------/
         CLC   =C'D8 ',FUNCT+5     ? DECEMBER 31, 2020    MDCY-d        LB1100y
         BE    DO#DATE             Yes, do DATE
         CLC   =C'DAY',FUNCT+5     ? MONDAY                             LB1100y
         BE    DO#DOWDY            Yes, do Weekday Name
         CLC   =C'DOW',FUNCT+5     ? 3                                  LB1100y
         BE    DO#DOW              Yes, do DOW
         B     DO#MDCY             No, Must be CYJ  request
*
         EJECT
*     * /********************************************************/
*56   * /* Function: FILL                   REQ REQ             */
*67   * /* Function: PAD                    REQ REQ             */
*68   * /* Function: GET1V                  REQ REQ             */      LB1100c
*69   * /* Function: PUT1V                  REQ REQ             */      LB1100c
*     * /********************************************************/
*     * /* Fill VAR1 with a 1-byte character.                   */
*     * /* VAR2 contains 1-byte FILLing character and up to 3   */
*     * /* digits representing VAR1 length.                     */
*     * /*   e.g. "*33" FILL VAR1 with * for a length of 33     */
*     * /*                                                      */
*     * /* Pad  VAR1 with a 1-byte character.                   */
*     * /* VAR2 contains 1-byte PADding character and up to 3   */
*     * /* digits representing VAR1 length.                     */
*     * /*   e.g. "*33" PAD  VAR1 with * for a length of 33     */
*     * /*                                                      */
*     * /* Get  1-byte character from VAR1 position n.          */      LB1100c
*     * /* VAR2 contains 1-byte placeholder and up to 3 digits  */      LB1100c
*     * /* represent VAR1 position to be fetched.               */      LB1100c
*     * /*   e.g. "x33" After execution, x represents 1-byte    */      LB1100c
*     * /*              feteched from VAR1 position 33.         */      LB1100c
*     * /*                                                      */      LB1100c
*     * /* Put  VAR1 1-byte character into position n.          */      LB1100d
*     * /* VAR2 contains 1-byte DELIMIT character and up to 3   */      LB1100d
*     * /* digits representing VAR1 position.                   */      LB1100d
*     * /*   e.g. "x33" PUT  into VAR1 position 33              */      LB1100d
*     * /*                                                      */      LB1100d
*     * /* VAR2 contains Parms --------------------             */
*     * /* Format: cnnn                                         */
*     * /*    where;                                            */
*     * /*        c - single byte character                     */
*     * /*        n - 1-3 digit length (1-256)                  */
*     * /*      **If no VAR2 specified, BLANK is search string  */      LB1100z
*     * /*        including default nnn of 1                    */      LB1100z
*     * /*                                                      */
*     * /* - R2, R3, R4, R5           Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /* - RC=4015  VAR2 is required                          */
*     * /* - RC=4016  VAR2 not found                            */
*     * /* - RC=4040  Invalid length (VAR2 content or Length)   */
*     * /* - RC=4034  Not numeric (Length)                      */
*     * /********************************************************/
PFILL    EQU   *                   Process FILL
PPAD     EQU   *                   Process PAD
PGET1V   EQU   *                   Process GET1V                        LB1100c
PPUT1V   EQU   *                   Process PUT1V                        LB1100d
PTRUNC   EQU   *                   Process TRUNC                        LB1105h
         BAL   R5,S#GVAR1N         Get VAR1, do not check for NULL
         ST    R7,R7HOLD           Save VAR1 value Length
         ST    R8,R8HOLD           Save VAR1 value Address
         BCTR  R7,0                Adjust for EXecute
         EX    R7,MVCVARWK         Execute VARWORK value move
*MVCVARWK MVC   VARWORK(0),0(R8)    Save VARWORK value
         LA    R7,1(R7)            Reset after EXecute
*     * /--------------------------------------------------------/
*     * /* Need VAR2 content in position 1                      */
*     * /--------------------------------------------------------/
         CLI   VAR2,C' '           VAR2 name captured?
         BE    ERR4015             NO,  VAR2 required
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441 to obtain VAR2                      */
*     * /--------------------------------------------------------/
         BAL   R5,S#GVAR2          Get VAR2
         L     R15,RC#441          Restore R15
         LTR   R15,R15             Successful IKJCT441?
         BNZ   ERR4016             No, VAR2  not found
*     * /--------------------------------------------------------/
*     * /* Update VAR2 Length                                   */
*     * /--------------------------------------------------------/
         BAL   R5,S#UV2L           Update VAR2L  variable
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Check VAR2 length limits           VAR2 L = 1-4      */
*     * /--------------------------------------------------------/
         L     R2,VALP2            R2  Value Ptr retreived/created
         L     R4,VALL2            R4  Value Len retreived/created
         LA    R3,0                R3  Length of VAR2
FILLLEN  EQU   *
         LA    R2,1(R2)            Bump up VAR2
         LA    R3,1(R3)            Bump up VAR2 Length
         CLI   0(R2),C' '          Blank delimiter?
         BE    GOTFLEN             Yes, go process
         BCT   R4,FILLLEN
GOTFLEN  EQU   *
         C     R3,=F'4'            Variable length > 4?
         BH    ERR4040             No, Error- VAR2 wrong length
         C     R3,=F'1'            Variable length = 0?
         BL    ERR4040             No, Error- VAR2 wrong length
*                                    must be 1 or ggreater
*                                  R3=Length of VAR2
         ST    R3,VALL2            Store length of VAR2
*     * /--------------------------------------------------------/
*     * /* Get 1-byte delimiter from VAR2                       */
*     * /--------------------------------------------------------/
         L     R2,VALP2            R2  Value Ptr retreived/created
         MVC   DELIM(1),0(R2)      Save 1 byte delimiter
*     * /--------------------------------------------------------/
*     * /* Check VAR2 FILL Len for numerics based on LENGTH (R7)*/
*     * /--------------------------------------------------------/
         C     R3,=F'1'            Only delimiter?
         BE    GETLFILL            Yes, done...
*                                  No, continue to edit
*     * /* - R7  Length                                         */
*     * /* - R8  Address of Content                             */
*     * /* - R15 Return Code                                    */
*     * /*       4001-TRUE                                      */
*     * /*       4000-FALSE                                     */
         LR    R7,R3               R3=Length of FILL
         BCTR  R7,0                Adjust for delimiter
         L     R8,VALP2            Restore VAR2 value Address
         LA    R8,1(R8)            Adjust for delimiter
         BAL   R5,S#ISNUM          Numeric Test
         C     R15,=F'4000'        Numeric?
         BE    ERR4034             NO, Error
*                                  We are numeric...
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Get FILL length                                      */
*     * /--------------------------------------------------------/
GETLFILL EQU   *
*        STM   R1,R2,DW            SAVE    R1,R2  to   DW before TPUT
*        LA    R1,=C' getlfill           '
*        LA    R2,20
*        TPUT  (R1),(R2)
*        LM    R1,R2,DW            RESTORE R1,R2  from DW after  TPUT
         L     R2,VALP2            R2  Value Ptr retreived/created
         C     R3,=F'2'            VAR2 Length = 2?
         BE    LFILL1              Yes, len = 1
         C     R3,=F'3'            VAR2 Length = 3?
         BE    LFILL2              Yes, len = 2
         C     R3,=F'4'            VAR2 Length = 4?
         BE    LFILL3              Yes, len = 3
         B     DFLTFILL            No, default to 1
LFILL1   EQU   *                   FILL len = 1 byte
         PACK  DW,1(1,R2)          Pack 1 digit after delim
         B     LFILLX
LFILL2   EQU   *                   FILL len = 2 bytes
         PACK  DW,1(2,R2)          Pack 2 digits after delim
         B     LFILLX
LFILL3   EQU   *                   FILL len = 3 bytes
         PACK  DW,1(3,R2)          Pack 3 digits after delim
         B     LFILLX
DFLTFILL EQU   *
         PACK  DW,=C'1'            Default to len = 1
LFILLX   EQU   *
         CVB   R3,DW               Convert to binary
*     * /--------------------------------------------------------/
*     * /* Check FILL length for variable limits                */
*     * /--------------------------------------------------------/
         C     R3,=F'256'          Variable length > 256?
         BH    ERR4040             Cannot process. 256 limit.
         C     R3,=F'0'            Variable length = 256?
         BE    ERR4040             Yes, null value. Exit.
*                                  R3= Length of FILL
         CLC   =C'PAD ',FUNCT      PAD function ?                       LB1100c
         BE    FILLPAD             Yes, continue to PAD                 LB1100c
         CLC   =C'FILL ',FUNCT     FILL function ?                      LB1100d
         BE    FILLPAD             Yes, continue to FILL                LB1100d
         CLC   =C'TRUNC ',FUNCT    TRUNC function ?                     LB1105h
         BE    TRUNCIT             Yes, continue to TRUNC               LB1105h
*                                  Must be GET1V/PUT1V function         LB1100c
         C     R3,VALL1            Position > VALL1 ?                   LB1100c
         BH    ERR4040             Yes, error                           LB1100c
         LA    R2,VARWORK          Addrs of VARWORK                     LB1100c
         AR    R2,R3               Move to requested position           LB1100c
         BCTR  R2,0                Adjust R2 relative to 0              LB1100c
         CLC   =C'GET1V ',FUNCT    GET1V function ?                     LB1100c
         BE    DOGET1V             Yes, Do GET1V                        LB1100d
DOPUT1V  EQU   *                   No,  Do PUT1V                        LB1100d
         MVC   0(1,R2),DELIM       PUT1V                                LB1100d
         L     R7,R7HOLD           Restore VAR1 value Length            LB1100d
         B     PPADX               Done                                 LB1100d
DOGET1V  EQU   *                                                        LB1100c
         MVC   DELIM,0(R2)         Get VAR1(n)                          LB1100c
         L     R2,VALP2            Load R2 w VAR2 data pointer          LB1100c
         MVC   0(1,R2),DELIM       Move VAR1(n) to VAR2(1)              LB1100c
         L     R7,VALL2            Restore VAR2 value Length            LB1100c
         B     PPADXV2             Done                                 LB1100c
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Fill VARWORK w delimiter value                       */
*     * /--------------------------------------------------------/
FILLPAD  EQU   *                                                        LB1100c
         LA    R2,VARWORK          Addr of VARWORK
         LR    R4,R3               Loop control - R3 times
         CLC   =C'FILL ',FUNCT     FILL  Function ?                     LB1100y
         BE    FILLEM              Yes, go to FILLEM
*                                  No, must be PAD function
         L     R7,R7HOLD           Restore VAR1 value Length
         CR    R7,R4               VAR1 Length > Pad Length?
         BNL   PPADX               No, no need to pad VAR1
*                                  Yes, pad VAR1
         AR    R2,R7               Position to end of VAR1
         SR    R4,R7               Adjust loop control
FILLEM   EQU   *
         MVC   0(1,R2),DELIM       Move DELIM to VARWORK
         LA    R2,1(R2)            Bump up VAR1
         BCT   R4,FILLEM           Again...
*     * /--------------------------------------------------------/
*     * /* Update VAR1 w VARWORK value                          */
*     * /--------------------------------------------------------/
TRUNCIT  EQU   *                                                        LB1105h
PFILLX   EQU   *
         LR    R7,R3               Final Length as VAR1L
PPADX    EQU   *
         LA    R2,VARWORK          VARWORK area as VAR1
         ST    R2,VALP2
         MVI   VAR2,C' '           Force VAR1 to be updated
PPADXV2  EQU   *                                                        LB1100c
         BAL   R5,S#UVAR2          Update VAR1, no update to VAR2
         B     ERRDONE
*
         EJECT
*     * /********************************************************/
*01   * /* Function: LTRIM                  REQ OPT             */
*57   * /* Function: LSTRIP                 REQ OPT             */
*     * /********************************************************/
*     * /* LTRIM : Strip leading spaces                         */
*     * /* LSTRIP: Strip leading delimiter, default=BLANK       */
*     * /* - R2, R5                   Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /********************************************************/
PLSTRIP  EQU   *                   Process LSTRIP
         BAL   R5,S#GVAR1          Get VAR1
         BAL   R5,S#CHKV2          Reset VAR2 when $NOVAR2
*     * /--------------------------------------------------------/
*     * /* VAR2 content area initial value is blanks            */
*     * /* regardless of S#GVAR2 return                         */
*     * /--------------------------------------------------------/
         CLC   =C'LTRIM ',FUNCT    LTRIM Function ?                     LB1100y
         BE    STRIPL              Yes, DELIM = BLANK at init.
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441 to obtain VAR2                      */
*     * /--------------------------------------------------------/
         BAL   R5,S#GVAR2          Get VAR2
*     * /--------------------------------------------------------/
*     * /* Update VAR2 Length                                   */
*     * /--------------------------------------------------------/
         BAL   R5,S#UV2L           Update VAR2L  variable
*     * /--------------------------------------------------------/
*     * /* Point to VAR2 value and length address               */
*     * /--------------------------------------------------------/
         L     R2,VALP2            R2  Value Ptr retreived/created
         L     R4,VALL2            R4  Value Len retreived/created
*     * /--------------------------------------------------------/
*     * /* Get 1-byte delimiter from VAR2                       */
*     * /* BLANK assumed if not VAR2 specified                  */
*     * /--------------------------------------------------------/
         MVC   DELIM(1),0(R2)      Save 1 byte delimiter
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Skip LEADING delimiter character                     */
*     * /--------------------------------------------------------/
STRIPL   EQU   *
         CLC   0(1,R8),DELIM       Strip character ?
         BNE   STRIPLM             NO, move remaining data
         LA    R8,1(R8)            YES, bump to next position
         BCT   R7,STRIPL           Decrement length while DELIM
*     * /--------------------------------------------------------/
*     * /* Move remaining data                                  */
*     * /--------------------------------------------------------/
STRIPLM  EQU   *
         BCTR  R7,0                Adjust for EXecute
         EX    R7,MVCVARWK         Execute VARWORK value move
*MVCVARWK MVC   VARWORK(0),0(R8)    Save VARWORK value
         LA    R7,1(R7)            Reset after EXecute
         CLC   =C'LTRIM ',FUNCT    LTRIM Function ?                     LB1100y
         BE    PLSTRIPX            Yes, update VAR2 and exit
*                                  No,  update VAR1 and exit
*     * /--------------------------------------------------------/
*     * /* Force VAR1 update with VARWORK content               */
*     * /--------------------------------------------------------/
         LA    R2,VARWORK          VARWORK area as VAR1
         ST    R2,VALP2
         MVI   VAR2,C' '           Force VAR1 to be updated
PLSTRIPX EQU   *
         BAL   R5,S#UVAR2          Update VAR2
         B     ERRDONE
*
         EJECT
*     * /********************************************************/
*02   * /* Function: RTRIM                  REQ OPT             */
*58   * /* Function: RSTRIP                 REQ OPT             */
*     * /********************************************************/
*     * /* RTRIM : Strip trailing spaces                        */
*     * /* RSTRIP: Strip trailing delimiter, default=BLANK      */
*     * /* - R2, R5                   Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /********************************************************/
PRSTRIP  EQU   *                   Process RSTRIP
         BAL   R5,S#GVAR1          Get VAR1
         BAL   R5,S#CHKV2          Reset VAR2 when $NOVAR2
         AR    R8,R7               Add VAR1 length to start addr
         BCTR  R8,0                ..to determine VAR1 end addr
*     * /--------------------------------------------------------/
*     * /* VAR2 content area initial value is blanks            */
*     * /* regardless of S#GVAR2 return                         */
*     * /--------------------------------------------------------/
         CLC   =C'RTRIM ',FUNCT    RTRIM Function ?                     LB1100y
         BE    STRIPR              Yes, DELIM = BLANK at init.
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441 to obtain VAR2                      */
*     * /--------------------------------------------------------/
         BAL   R5,S#GVAR2          Get VAR2
*     * /--------------------------------------------------------/
*     * /* Update VAR2 Length                                   */
*     * /--------------------------------------------------------/
         BAL   R5,S#UV2L           Update VAR2L  variable
*     * /--------------------------------------------------------/
*     * /* Point to VAR2 value and length address               */
*     * /--------------------------------------------------------/
         L     R2,VALP2            R2  Value Ptr retreived/created
         L     R4,VALL2            R4  Value Len retreived/created
*     * /--------------------------------------------------------/
*     * /* Get 1-byte delimiter from VAR2                       */
*     * /* BLANK assumed if not VAR2 specified                  */
*     * /--------------------------------------------------------/
         MVC   DELIM(1),0(R2)      Save 1 byte delimiter
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Skip TRAILING delimiter character                    */
*     * /--------------------------------------------------------/
STRIPR   EQU   *
         CLC   0(1,R8),DELIM       Strip character ?
         BNE   STRIPRM             NO, move data
         BCTR  R8,0                YES, bump to prev position
         BCT   R7,STRIPR           Decrement length while DELIM
*     * /--------------------------------------------------------/
*     * /* Move remaining data                                  */
*     * /--------------------------------------------------------/
STRIPRM  EQU   *
*                                  R7=VAR1 revised length
         L     R8,VALP1            Reset to VAR1 value address
         BCTR  R7,0                Adjust for EXecute
         EX    R7,MVCVARWK         Execute VARWORK value move
*MVCVARWK MVC   VARWORK(0),0(R8)    Save VARWORK value
         LA    R7,1(R7)            Reset after EXecute
         CLC   =C'RTRIM ',FUNCT    RTRIM Function ?                     LB1100y
         BE    PRSTRIPX            Yes, update VAR2 and exit
*                                  No,  update VAR1 and exit
*     * /--------------------------------------------------------/
*     * /* Force VAR1 update with VARWORK content               */
*     * /--------------------------------------------------------/
         LA    R2,VARWORK          VARWORK area as VAR1
         ST    R2,VALP2
         MVI   VAR2,C' '           Force VAR1 to be updated
PRSTRIPX EQU   *
         BAL   R5,S#UVAR2          Update VAR2
         B     ERRDONE
*
         EJECT
*     * /********************************************************/
*03   * /* Function: TRIM                   REQ OPT             */
*59   * /* Function: STRIP                  REQ OPT             */
*     * /********************************************************/
*     * /* TRIM : Strip leading and trailing spaces             */
*     * /* STRIP: Strip leading and trailing delimiter,         */
*     * /*        default=BLANK                                 */
*     * /* - R2, R5                   Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /********************************************************/
PSTRIP   EQU   *                   Process STRIP
         BAL   R5,S#GVAR1          Get VAR1
*        ST    R7,R7HOLD           Save VAR1 value Length
*        ST    R8,R8HOLD           Save VAR1 value Address
         BAL   R5,S#CHKV2          Reset VAR2 when $NOVAR2
*     * /--------------------------------------------------------/
*     * /* VAR2 content area initial value is blanks            */
*     * /* regardless of S#GVAR2 return                         */
*     * /--------------------------------------------------------/
         CLC   =C'TRIM ',FUNCT     TRIM Function ?                      LB1100y
         BE    STRIPLL             Yes, DELIM = BLANK at init.
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441 to obtain VAR2                      */
*     * /--------------------------------------------------------/
         BAL   R5,S#GVAR2          Get VAR2
*     * /--------------------------------------------------------/
*     * /* Update VAR2 Length                                   */
*     * /--------------------------------------------------------/
         BAL   R5,S#UV2L           Update VAR2L  variable
*     * /--------------------------------------------------------/
*     * /* Check VAR2 length limits           VAR2 L = 1        */
*     * /--------------------------------------------------------/
         L     R2,VALP2            R2  Value Ptr retreived/created
         L     R4,VALL2            R4  Value Len retreived/created
*     * /--------------------------------------------------------/
*     * /* Get 1-byte delimiter from VAR2                       */
*     * /--------------------------------------------------------/
         MVC   DELIM(1),0(R2)      Save 1 byte delimiter
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Skip LEADING delimiter character                     */
*     * /--------------------------------------------------------/
STRIPLL  EQU   *
         CLC   0(1,R8),DELIM       Strip character ?
         BNE   STRIPLMM            NO, move remaining data
         LA    R8,1(R8)            YES, Bump to next position
         BCT   R7,STRIPLL          Decrement length while DELIM
         B     STRIPRMM            Done...
*     * /--------------------------------------------------------/
*     * /* Move remainint data                                  */
*     * /--------------------------------------------------------/
STRIPLMM EQU   *
         BCTR  R7,0                Adjust for EXecute
         EX    R7,MVCVARWK         Execute VARWORK value move
*MVCVARWK MVC   VARWORK(0),0(R8)    Save VARWORK value
         LA    R7,1(R7)            Reset after EXecute
*     * /--------------------------------------------------------/
*     * /* Move VARWORK to VAR1                                 */
*     * /--------------------------------------------------------/
         MVC   VAL1,VARWORK        VAR1 w/o leading delimiter
         AR    R8,R7               Add VAR1 length to start addr
         BCTR  R8,0                ..to determine VAR1 end addr
*     * /--------------------------------------------------------/
*     * /* Skip TRAILING delimiter character                    */
*     * /--------------------------------------------------------/
STRIPRR  EQU   *
         CLC   0(1,R8),DELIM       Strip character ?
         BNE   STRIPRMM            No, update data
         BCTR  R8,0                Yes, bump to prev position
         BCT   R7,STRIPRR          Decrement length while DELIM
STRIPRMM EQU   *
*                                  R7=VAR1 revised length
         L     R8,VALP1            Reset to VAR1 value address
         CLC   =C'TRIM ',FUNCT     TRIM Function ?                      LB1100y
         BE    PSTRIPX             Yes, update VAR2 and exit
*                                  No,  update VAR1 and exit
*     * /--------------------------------------------------------/
*     * /* Force VAR1 update with VARWORK content               */
*     * /--------------------------------------------------------/
         LA    R2,VARWORK          VARWORK area as VAR1
         ST    R2,VALP2
         MVI   VAR2,C' '           Force VAR1 to be updated
PSTRIPX  EQU   *
         BAL   R5,S#UVAR2          Update VAR1, no update to VAR2
         B     ERRDONE
*
         EJECT
*     * /********************************************************/
*60   * /* Function: STRIP$                 REQ OPT             */
*     * /********************************************************/
*     * /* Strip all delimiter.                                 */
*     * /* Default delimiter is BLANK.                          */
*     * /* - R2, R5                   Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /********************************************************/
PSTRIP$  EQU   *                   Process STRIP$
         BAL   R5,S#GVAR1          Get VAR1
         ST    R7,R7HOLD           Save VAR1 value Length
         ST    R8,R8HOLD           Save VAR1 value Address
         BAL   R5,S#CHKV2          Reset VAR2 when $NOVAR2
*     * /--------------------------------------------------------/
*     * /* VAR2 content area initial value is blanks            */
*     * /* regardless of S#GVAR2 return                         */
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441 to obtain VAR2                      */
*     * /--------------------------------------------------------/
         BAL   R5,S#GVAR2          Get VAR2
*     * /--------------------------------------------------------/
*     * /* Update VAR2 Length                                   */
*     * /--------------------------------------------------------/
         BAL   R5,S#UV2L           Update VAR2L  variable
*     * /--------------------------------------------------------/
*     * /* Point to VAR2 value and length address               */
*     * /--------------------------------------------------------/
         L     R2,VALP2            R2  Value Ptr retreived/created
         L     R4,VALL2            R4  Value Len retreived/created
*     * /--------------------------------------------------------/
*     * /* Get 1-byte delimiter from VAR2                       */
*     * /* BLANK assumed if not VAR2 specified                  */
*     * /--------------------------------------------------------/
         MVC   DELIM(1),0(R2)      Save 1 byte delimiter
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441 to obtain VAR2                      */
*     * /--------------------------------------------------------/
         BAL   R5,S#GVAR2          Get VAR2
*     * /--------------------------------------------------------/
*     * /* Skip all delimiter in string (VAR1)                  */
*     * /--------------------------------------------------------/
         LA    R2,VARWORK
         SR    R5,R5               VARWORK Length
STRIPALL EQU   *
         CLC   0(1,R8),DELIM       Strip character ?
         BE    SKIPDLM             Yes, bypass
         MVC   0(1,R2),0(R8)       No, keep it...
         LA    R5,1(R5)            Add 1 to length - VARWORK
         LA    R2,1(R2)            Bump to next position - VARWORK
SKIPDLM  EQU   *
         LA    R8,1(R8)            Bump to next position - VAR1
         BCT   R7,STRIPALL         YES, skip it and check next pos
*     * /--------------------------------------------------------/
*     * /* Update VAR1 w VARWORK value                          */
*     * /--------------------------------------------------------/
PSTRIP$X EQU   *
         LR    R7,R5               Fill Length as VAR1L
         LA    R2,VARWORK          VARWORK area as VAR1
         ST    R2,VALP2
         MVI   VAR2,C' '           Force VAR1 to be updated
         BAL   R5,S#UVAR2          Update VAR1, no update to VAR2
         B     ERRDONE
*
         EJECT
*     * /********************************************************/
*61   * /* Function: CONCAT                 REQ REQ             */
*     * /********************************************************/
*     * /* Concatenate VAR01 and VAR02 into VAR01               */
*     * /* VAR01 max length is 256 bytes                        */
*     * /* - R2, R4, R5, R6           Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /* - RC=4015  VAR2 is required                          */
*     * /* - RC=4016  VAR2 not found                            */
*     * /********************************************************/
PCONCAT  EQU   *                   Process CONCATenate
         BAL   R5,S#GVAR1          Get VAR1
         ST    R7,R7HOLD           Save VAR1 value Length
         ST    R8,R8HOLD           Save VAR1 value Address
*     * /--------------------------------------------------------/
*     * /* Need VAR2 content in position 1                      */
*     * /--------------------------------------------------------/
         CLI   VAR2,C' '           VAR2 name captured?
         BE    ERR4015             NO,  VAR2 required
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441 to obtain VAR2                      */
*     * /--------------------------------------------------------/
         BAL   R5,S#GVAR2          Get VAR2
         L     R15,RC#441          Restore R15
         LTR   R15,R15             Successful IKJCT441?
         BNZ   ERR4016             No, VAR2  not found
*     * /--------------------------------------------------------/
*     * /* Update VAR2 Length                                   */
*     * /--------------------------------------------------------/
         BAL   R5,S#UV2L           Update VAR2L  variable
*     * /--------------------------------------------------------/
*     * /* Check VAR2 length limits           VAR2 L = 1        */
*     * /--------------------------------------------------------/
         L     R2,VALP2            R2  Value Ptr retreived/created
         L     R4,VALL2            R4  Value Len retreived/created
*     * /--------------------------------------------------------/
*     * /* Move VAR1 to VARWORK                                 */
*     * /--------------------------------------------------------/
         BCTR  R7,0                Adjust for EXecute
         EX    R7,MVCVARWK         Execute VARWORK value move
*MVCVARWK MVC   VARWORK(0),0(R8)    Save VARWORK value
         LA    R7,1(R7)            Adjust length to original
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* MOVE VAR2 to VARWORK (concatenate)                   */
*     * /--------------------------------------------------------/
         LA    R6,VARWORK          Point to VARWORK
         AR    R6,R7               Point to end of VAR1 in VARWORK
         LR    R8,R2               R8=Start addr of VAL2
         LR    R5,R7               Init R5 with VAR1 Length
         AR    R5,R4               Add VAR2 Length
*     * /--------------------------------------------------------/
*     * /* Force total length to  256 if > 256                  */
*     * /--------------------------------------------------------/
         C     R5,=F'256'          Exceed 256 limit?
         BNH   PCONCATC            No, continue
         S     R5,=F'256'          Yes, calc remaining length
         LR    R4,R5
         LA    R5,256              Yes, force total length to 256
PCONCATC EQU   *
         BCTR  R4,0                Ajust for EXecute    R4=VALL2 - 1
         EX    R4,MVCR8$6          Move remainder of VAR1
*MVCR8$6  MVC   0(0,R6),0(R8)       MVC R8 to R6
         LA    R4,1(R4)            Adjust length to original R4=VALL2
*     * /--------------------------------------------------------/
*     * /* Update VAR1 w VARWORK value                          */
*     * /--------------------------------------------------------/
PCONCATX EQU   *
         LR    R7,R5               Fill Length as VAR1L
         LA    R2,VARWORK          VARWORK area as VAR1
         ST    R2,VALP2
         MVI   VAR2,C' '           Force VAR1 to be updated
         BAL   R5,S#UVAR2          Update VAR1, no update to VAR2
         B     ERRDONE
*
         EJECT
*     * /********************************************************/
*62   * /* Function: UNSTR                  REQ OPT             */
*     * /********************************************************/
*     * /* Unstring VAR1 into separate VAR1nnn variables        */
*     * /* based on 1-byte delimiter, if specified in VAR2.     */
*     * /* - If no VAR2 is specified, blank delimiter assumed.  */
*     * /* - VAR10  contains number of variables created.       */
*     * /* - VAR10,1,2,3,4,5,6,7,8,9,10,11 to 99 are possible   */
*     * /*   variable names.                                    */
*     * /*                                                      */
*     * /* VAR2 contains Search Parms -------------             */
*     * /* Format: c                                            */
*     * /*    where;                                            */
*     * /*        c - single byte delimiter (default=BLANK)     */
*     * /*      **If no VAR2 specified, BLANK is default for    */      LB1100z
*     * /*        search string                                 */      LB1100z
*     * /*                                                      */
*     * /* - R2, R4, R5               Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /********************************************************/
PUNSTR   EQU   *                   Process UNSTRing
         BAL   R5,S#GVAR1          Get VAR1
         ST    R7,R7HOLD           Save VAR1 value Length
         ST    R8,R8HOLD           Save VAR1 value Address
         BAL   R5,S#CHKV2          Reset VAR2 when $NOVAR2
*     * /--------------------------------------------------------/
*     * /* VAR2 content area initial value is blanks            */
*     * /* regardless of S#GVAR2 return                         */
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441 to obtain VAR2                      */
*     * /--------------------------------------------------------/
         BAL   R5,S#GVAR2          Get VAR2
*     * /--------------------------------------------------------/
*     * /* Update VAR2 Length                                   */
*     * /--------------------------------------------------------/
         BAL   R5,S#UV2L           Update VAR2L  variable
*     * /--------------------------------------------------------/
*     * /* Point to VAR2 value and length address               */
*     * /--------------------------------------------------------/
         L     R2,VALP2            R2  Value Ptr retreived/created
         L     R4,VALL2            R4  Value Len retreived/created
*     * /--------------------------------------------------------/
*     * /* Get 1-byte delimiter from VAR2                       */
*     * /* BLANK assumed if not VAR2 specified                  */
*     * /--------------------------------------------------------/
         MVC   DELIM(1),0(R2)      Save 1 byte delimiter
*     * /--------------------------------------------------------/
*     * /* Variable Counter to append VAR1 variable name        */
*     * /--------------------------------------------------------/
         ZAP   NNN,=P'0'
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Copy string until first delimiter is found           */
*     * /--------------------------------------------------------/
CR8VARS  EQU   *
         LA    R2,VARWORK          Variable content from VAR1
         SR    R5,R5               VARWORK Length
CHK4VAR  EQU   *
         CLC   0(1,R8),DELIM       Delim character ?
         BE    CR8VCHK             Yes, check for create of VAR1nnn
         MVC   0(1,R2),0(R8)       No, keep it...
         LA    R5,1(R5)            Add 1 to length - VARWORK
         LA    R2,1(R2)            Bump to next position - VARWORK
         LA    R8,1(R8)            Bump to next position - VAR1
         BCT   R7,CHK4VAR          YES, skip it and check next pos
*     * /--------------------------------------------------------/
*     * /* Primary loop complete.  Check if last var needs      */
*     * /* processing, then quit.                               */
*     * /--------------------------------------------------------/
         LTR   R5,R5               Length = 0?
         BZ    PUNSTRX             Yes, done... no process necessary
         LR    R3,R5               No, process last one
         BAL   R5,S#CVARN          Create/Update VAR1nnn and VAR10
         B     PUNSTRX             done...
*     * /--------------------------------------------------------/
*     * /* Delimiter found, create varwork length for           */
*     * /* processing and continue to scan for delimiter        */
*     * /--------------------------------------------------------/
CR8VCHK  EQU   *                   Delimiter found, check length
*        R5 is length of varwork
         LTR   R5,R5               Length > 0?
         BNZ   CR8VAR              Yes, create VAR1nnn
         LA    R8,1(R8)            No, Bump to next position - VAR1
         BCT   R7,CHK4VAR          check delim again...
         B     PUNSTRX             End of Loop. Done..
CR8VAR   EQU   *
         LR    R3,R5               Length of VARWORK in RC for sub
         BAL   R5,S#CVARN          Create/Update VAR1nnn and VAR10
         LA    R8,1(R8)            Bump to next position - VAR1
         BCT   R7,CR8VARS          Reset and check next VAR01 character
*     * /--------------------------------------------------------/
*     * /* No more VAR1 content                                 */
*     * /--------------------------------------------------------/
PUNSTRX  EQU   *
         B     ERRDONE
*
         EJECT
*     * /********************************************************/
*63   * /* Function: REPLACE                REQ REQ             */
*     * /********************************************************/
*     * /* Replace substring in VAR1.                           */
*     * /* VAR2 has from, to, start, end                        */
*     * /* - result in VAR1                                     */
*     * /*                                                      */
*     * /* VAR2 contains Search Parms -------------             */
*     * /* Format: "sssssss","rrrrrrr",bbb,eee                  */
*     * /*    where;                                            */
*     * /*        s - search string in double quotes            */
*     * /*        r - replace string in double quotes           */
*     * /*        b - optional, beginning search position       */      LB1100z
*     * /*              1-3 numeric digits, default=1           */
*     * /*        e - optional, ending   search position        */
*     * /*              1-3 numeric digits, default=end         */
*     * /*        Parms separated by commas (,):                */
*     * /*                                                      */
*     * /* - R1, R2, R4, R5, R6       Working Register          */
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */
*     * /********************************************************/
*     * /* - RC=0     Successful Request                        */
*     * /* - RC=4015  VAR2 is required                          */
*     * /* - RC=4023  Result > 256 bytes                        */
*     * /* - RC=4030  No begin "  (search / replace string)     */
*     * /* - RC=4031  No end "    (search / replace string)     */
*     * /* - RC=4032  Start > End position                      */
*     * /* - RC=4033  Invalid character (delimiter)             */
*     * /* - RC=4034  Not numeric (start / end position)        */
*     * /* - RC=4035  Too many digits (start / end position)    */
*     * /* - RC=4036  Search or replace string is null          */
*     * /********************************************************/
PREPL    EQU   *                   Process REPLace
         BAL   R5,S#GVAR1          Get VAR1
         ST    R7,R7HOLD           Save VAR1 value Length
         ST    R8,R8HOLD           Save VAR1 value Address
*     * /--------------------------------------------------------/
*     * /* Need VAR2 content in position 1                      */
*     * /--------------------------------------------------------/
         CLI   VAR2,C' '           VAR2 name captured?
         BE    ERR4015             NO,  VAR2 required
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441 to obtain VAR2                      */
*     * /--------------------------------------------------------/
         BAL   R5,S#GVAR2          Get VAR2
         L     R15,RC#441          Restore R15
         LTR   R15,R15             Successful IKJCT441?
         BNZ   ERR4015             No, VAR2 required
*
         BAL   R5,S#UV2L           Update VAR2L  variable
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Check VAR2 length limits           VAR2 L = 1        */
*     * /--------------------------------------------------------/
         L     R5,VALP2            R2  Value Ptr retreived/created
         L     R6,VALL2            R4  Value Len retreived/created
         C     R6,=F'0'            Variable length = 0?
         BE    ERR4015             Yes, VAR2 required
*     * /--------------------------------------------------------/
*     * /* Get Replace Parameters                               */
*     * /--------------------------------------------------------/
         BAL   R1,S#SRCHP          Get Search Parms
         L     R2,VALP2            VAR2 address
         L     R4,VALL2            VAR2 length
*     * /--------------------------------------------------------/
*     * /* Check Search Start and End Values                    */
*     * /--------------------------------------------------------/
         CLC   SRCHS,SRCHE         Start > End location?
         BH    ERR4032             Yes, Error Start > End
         C     R7,SRCHS            VAR1 Length < Start ?                LB1105f
         BL    ERR4032             Yes, Error Start beyond VAR1 Length  LB1105f
         C     R7,SRCHE            VAR1 Length < End   ?                LB1105f
         BL    ERR4032             Yes, Error End   beyond VAR1 Length  LB1105f

*     * /--------------------------------------------------------/
*     * /* Initialize for Search and Replace                    */
*     * /--------------------------------------------------------/
*     * /* R8- VAR1         R7- VAR1 Length                     */
*     * /* R3- VARWORK      R5- VARWORK Length                  */
         L     R1,SRCHS            R1=Search Start
         BCTR  R1,0                ...Adj for 0-base
         EX    R1,MVCVARW#         Move portion of VAR1 to VARWORK
*MVCVARW# MVC   VARWORK(0),0(R8)    Save VARWORK value
         SR    R7,R1               Adj VAR1 Length by SRCHS
         AR    R8,R1               Adj start by SRCHS
         LA    R3,VARWORK          R3=Addr VARWORK
         AR    R3,R1               Adj VARWORK addr by SRCHS
         LA    R5,0                R5=VARWORK Length
         AR    R5,R1               Adj VARWORK Length by SRCHS
         LR    R2,R8               R2=VAR1
         L     R1,SRCHSTRL         R1=Length of Search String
         BCTR  R1,0                ...Adj for CLC
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Search VARWORK for 'str'                             */
*       r8 var1   r7 varlL
*       r3 varwork r5 varworkL
*     * /--------------------------------------------------------/
REPL     EQU   *                   Look for 'str' in VAR1
         EX    R1,SRCHCLC          Substring found?
*SRCHCLC  CLC   0(0,R2),SRCHSTR
         BE    REPLF               YES, let us replace!
         MVC   0(1,R3),0(R2)       Move VAR1 to VARWORK
         LA    R3,1(R3)            Bump up VARWORK
         LA    R5,1(R5)            Bump up VARWORK length
         C     R5,=F'256'          > 256 Limit?
         BH    ERR4023             Yes, ERROR.
         LA    R2,1(R2)            Bump up to next byte of VAR1
         BCT   R7,REPL             Check again...
         B     PREPLX              Done...
*     * /--------------------------------------------------------/
*     * /* Replace 'str' with 'st2'                             */
*     * /--------------------------------------------------------/
REPLF    EQU   *
         LA    R1,SRCHST2          R1=Addr   of Search String 2
         L     R6,SRCHST2L         R6=Length of Search String 2
REPLFL   EQU   *
         MVC   0(1,R3),0(R1)       MOVE REPL String to VARWORK
         LA    R3,1(R3)            Bump up VARWORK
         LA    R5,1(R5)            Bump up VARWORK length
         C     R5,=F'256'          > 256 Limit?
         BH    ERR4023             Yes, ERROR.
         LA    R1,1(R1)            Bump to next REPL String char
         BCT   R6,REPLFL           Move again...
*                                  End of loop
         L     R1,SRCHSTRL         R1=Length of Search String
         BCTR  R1,0                ...Adj for CLC
         A     R2,SRCHSTRL         Bump up SRCHSTRL bytes of VAR1
         S     R7,SRCHSTRL         Reset length of VAR1
         LA    R7,1(R7)            ...Adj R7 Length
         BCT   R7,REPL             Check again...
*                                  Done...
*     * /--------------------------------------------------------/
*     * /* Update VAR1 w VARWORK value                          */
*     * /--------------------------------------------------------/
PREPLX   EQU   *
         LR    R7,R5               Fill Length as VAR1L
         LA    R2,VARWORK          VARWORK area as VAR1
         ST    R2,VALP2
         MVI   VAR2,C' '           Force VAR1 to be updated
         BAL   R5,S#UVAR2          Update VAR1, no update to VAR2
         B     ERRDONE
*
         EJECT
*     * /********************************************************/
*64   * /* Function: VEXIST                 REQ NO              */
*     * /********************************************************/
*     * /* Check if VAR1 variable exists.                       */
*     * /* Result in RC; RC=4000 FALSE or content length, nnn   */
*     * /* No content returned in VAR1                          */
*     * /* - R5                       Working Register          */
*     * /* - R15 for RC                                         */
*     * /********************************************************/
*     * /* - RC=nnn   VAR1 content length                       */
*     * /* - RC=4000  FALSE, VAR1 NOT found                     */
*     * /* - RC=4022  VAR2 not needed                           */
*     * /********************************************************/
PVEXIST  EQU   *                   Process VEXIST
*     * /--------------------------------------------------------/
*     * /* VAR2 position 1 must be blank                        */
*     * /--------------------------------------------------------/
         CLC   VAR2,=C'$NOVAR2 '   VAR2 none keyword?
         BE    PVEXIST#            YES, continue...
         CLI   VAR2,C' '           VAR2 name captured?
         BNE   ERR4022             NO,  VAR2 NOT required
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441 to obtain VAR2 (via VAR1)           */
*     * /--------------------------------------------------------/
PVEXIST# EQU   *
         MVC   VAR2,VAR1           Overlay VAR2 with VAR1
         MVC   VAR2L,VAR1L         Overlay VAR2 with VAR1
         BAL   R5,S#GVAR2N         Get VAR2 as VAR1, no NULL check
         L     R15,RC#441          Restore R15
         LTR   R15,R15             Successful IKJCT441?
         BNZ   VFINDNO             No, VAR1 not found
VFIND    EQU   *
         L     R15,VALL2           Length of VAR1
         B     PVEXISTX            Done...
VFINDNO  EQU   *
         B     ERR4000             FALSE, NOT FOUND
*     * /--------------------------------------------------------/
*     * /* Done...                                              */
*     * /--------------------------------------------------------/
PVEXISTX EQU   *
         B     ERRDONE
*
         EJECT                                                          LB1100e
*     * /********************************************************/      LB1100e
*70   * /* Function: MCAL                   REQ OPT             */      LB1100e
*71   * /* Function: MCALA                  REQ OPT             */      LB1100e
*     * /********************************************************/      LB1100e
*     * /* Month Calendar String given MMDDCCYY                 */      LB1100e
*     * /********************************************************/      LB1100e
*     * /*  Date input form:                                    */      LB1100e
*     * /*  MMDDCCYY                                            */      LB1100e
*     * /*    DD will be overriden to 01                        */      LB1100e
*     * /********************************************************/      LB1100e
*     * /* - R3, R4, R5, R6           Working Register          */      LB1100e
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */      LB1100e
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */      LB1100e
*     * /********************************************************/      LB1100e
*     * /* - RC=0     Successful Request                        */      LB1100e
*     * /* - RC=4011  Invalid FUNCTION                          */      LB1100e
*     * /* - RC=4040  Invalid length (VAR1)                     */      LB1100e
*     * /* - RC=4047  Invalid DD (VAR1)                         */      LB1100e
*     * /* - RC=4034  Not numeric (VAR1)                        */      LB1100e
*     * /* - RC=4049  Invalid MM (VAR1)                         */      LB1100e
*     * /* - RC=4050  Invalid DD for MM (VAR1)                  */      LB1100e
*     * /* - RC=4051  Invalid JJJ (VAR1)                        */      LB1100e
*     * /********************************************************/      LB1100e
*                                                                       LB1100e
         EJECT                                                          LB1100e
PMCAL    EQU   *                   Process MCAL                         LB1100e
         BAL   R5,S#GVAR1N         Get VAR1 w/ no NULL check            LB1100e
         BAL   R5,S#CHKV2          Reset VAR2 when $NOVAR2              LB1100e
         ST    R7,R7HOLD           Save VAR1 value Length               LB1100e
         ST    R8,R8HOLD           Save VAR1 value Address              LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Get todays JJJ and CCYY                              */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
         TIME  DEC                 Time in R0  HH_MM_SS_TH              LB1100e
*                                  Date in R1  00_YY_DD_DC              LB1100e
         ST    R1,WORKAREA         Date in R1  II_YY_DD_DC              LB1100e
         UNPK  UNPKAREA(7),WORKAREA+0(4)     Unpack IIYYDDD             LB1100e
         OI    UNPKAREA+6,X'F0'            F-zone   IIYYDDD             LB1100e
         PACK  FW(2),UNPKAREA(2)             II                         LB1100e
         AP    FW(2),=P'19'                  CC = II + 19               LB1100e
         UNPK  UNPKAREA(2),FW(2)             CC                         LB1100e
         OI    UNPKAREA+1,X'F0'    F-zone    CC                         LB1100e
         MVC   MCAL2JJJ,UNPKAREA+4          Today JJJ                   LB1100e
         MVC   MCAL2MCY+2(04),UNPKAREA      Today CCYY                  LB1100e
         MVI   DCCCCYY,C'0'                 0CCYY                       LB1100e
         MVC   DCCCCYY+1(4),UNPKAREA                                    LB1100e
         MVC   DCJJJ,UNPKAREA+4             DCJJJ                       LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Determine if CCYY is a leap year                     */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
*        input  DCCCCYY  0CCYY                                          LB1100e
*        output DC#LEAP  0=common year     1=leap year                  LB1100e
*               DC#YDAYS 365=common year   366=leap year                LB1100e
         BAL   R5,S#ISLPY          Determine Leap Year                  LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Determine MM and DD from JJJ                         */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
*        input  DC#LEAP PKJJJ                                           LB1100e
*        Work   FW, R3, R4,                                             LB1100e
*        output DC#TRM  DCMMM DCDDD                                     LB1100e
*               R4=addr of MONTHNAME  R6=Length of MONTHNAME            LB1100e
         PACK  PKJJJ,DCJJJ                                              LB1100e
         BAL   R5,S#J2MD                     Determine MM DD from JJJ   LB1100e
         MVC   MCAL2MCY(02),DCMMM+1          Today MM                   LB1100e
         MVC   MCAL2DD,DCDDD+1               Today DD                   LB1100e
*                                                                       LB1100e
         EJECT                                                          LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Determine whether to default input date              */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
         LTR   R7,R7               Zero length on input ?               LB1100e
         BZ    DFLT2DAY            Yes, default date                    LB1100e
         CLI   0(R8),C' '          Blank in position 1 of input ?       LB1100e
         BNE   CHKVAR1L            No, continue                         LB1100e
DFLT2DAY EQU   *                                                        LB1100e
         MVC   0(2,R8),MCAL2MCY    Default MM   today                   LB1100e
         MVC   4(4,R8),MCAL2MCY+2  Default CCYY today                   LB1100e
         CLC   SQUOTE,=C'QUOTE   ' QUOTE Processing?                    LB1100e
         BNE   NOQOTE              No, continue                         LB1100e
         MVI   8(R8),C''''         Yes, place end QUOTE in VAR1         LB1100e
NOQOTE   EQU   *                                                        LB1100e
         LA    R7,8                                                     LB1100e
         ST    R7,R7HOLD           Save VAR1 value Length               LB1100e
         ST    R7,VALL1            Save VAR1 value Length               LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Check VAR1 for proper length                         */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
CHKVAR1L EQU   *                                                        LB1100e
         C     R7,=F'8'            VAR1 length = 8 ?                    LB1100e
         BNE   ERR4040             No, Error- VAR1 wrong length         LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Force DD to 1st of month                             */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
         MVC   2(2,R8),=C'01'      Force DD to 01                       LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Check VAR1 for numerics based on LENGTH (R7)         */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
         BAL   R5,S#ISNUM          Numeric Test                         LB1100e
         C     R15,=F'4000'        Numeric?                             LB1100e
         BE    ERR4034             NO, Error                            LB1100e
*                                  YES, Continue...                     LB1100e
         L     R7,R7HOLD           Restore VAR1 value Length            LB1100e
         L     R8,R8HOLD           Restore VAR1 value Address           LB1100e
*                                                                       LB1100e
         EJECT                                                          LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Save Request MMCCYY                                  */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
         MVC   MCALRMCY(02),0(R8)      Request MM                       LB1100e
         MVC   MCALRMCY+02(04),4(R8)   Request CCYY                     LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Store VAR1 date   (R8=MMDDCCYY or variation of MDCY) */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
*        input  VAR1                                                    LB1100e
*        output DCCCCYY  DCJJJ                                          LB1100e
         BAL   R5,S#IMDCY          Store VAR1 date                      LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Determine if CCYY is leap year                       */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
*        input  DCCCCYY  0CCYY                                          LB1100e
*        output DC#LEAP  0=not leap year   1=leap year                  LB1100e
*               DC#YDAYS 365=common year   366=leap year                LB1100e
         BAL   R5,S#ISLPY          Determine Leap Year                  LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Determine JJJ from MM DD                             */      LB1100e
*     * /---------f----------------------------------------------/      LB1100e
*        input  DC#LEAP  n packed                                       LB1100e
*               DCDDD    0DD                                            LB1100e
*               DCMMM    0MM                                            LB1100e
*        output DC#JJJ   JJJ packed                                     LB1100e
*               DC#TRM   n   packed                                     LB1100e
*               DC#DD    DD  packed                                     LB1100e
*               DCJJJ    JJJ                                            LB1100e
*               DC#MDAYS 0nn packed                                     LB1100e
*               R4=addr of MONTHNAME                                    LB1100e
*               R6=Length of MONTHNAME                                  LB1100e
         BAL   R5,S#MD2J           MMDD to JJJ                          LB1100e
*                                                                       LB1100e
         EJECT                                                          LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Start of result content build                        */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
         LA    R2,VARWORK          Addr of VARWORK                      LB1100e
         LA    R3,0                Length of Result                     LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Row 1     ' December 2018    JJJ'   21-bytes         */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
         MVI   0(R2),C' '          Row1, start with blank               LB1100e
         LA    R2,1(R2)            Bump up VARWORK addr                 LB1100e
         LA    R3,1(R3)            Bump up result length                LB1100e
         BAL   R5,S#BD8            Monthname CCYY                       LB1100e
         CLC   MCALRMCY,MCAL2MCY   Request MMCCYY = current MMCCYY ?    LB1100e
         BE    MCALF1A             Yes, pad to position 17              LB1100e
         LA    R5,21               No,  pad to position 21              LB1100e
         B     MCALF1B                                                  LB1100e
MCALF1A  EQU   *                                                        LB1100e
         LA    R5,17               Pad to position 17                   LB1100e
MCALF1B  EQU   *                                                        LB1100e
         SR    R5,R3               Adjust R5 for loop                   LB1100e
MCALF1C  EQU   *                                                        LB1100e
         MVI   0(R2),C' '          Pad character                        LB1100e
         LA    R2,1(R2)            Bump up VARWORK addr                 LB1100e
         LA    R3,1(R3)            Bump up result length                LB1100e
         BCT   R5,MCALF1C          Loop...                              LB1100e
         CLC   =C'MCALA ',FUNCT    MCALA Function ?                     LB1100e
         BE    MCALF1D             Yes.                                 LB1100e
         MVI   0(R2),C' '          No, place pad character              LB1100e
         B     MCALF1E                                                  LB1100e
MCALF1D  EQU   *                                                        LB1100e
         MVC   0(1,R2),MCALA2$     Attribute2                           LB1100e
MCALF1E  EQU   *                                                        LB1100e
         LA    R2,1(R2)            Bump up VARWORK addr                 LB1100e
         LA    R3,1(R3)            Bump up result length                LB1100e
         CLC   MCALRMCY,MCAL2MCY   Request MMCCYY = current MMCCYY ?    LB1100e
         BE    MCALF1F             Yes, exhibit JJJ                     LB1100e
         B     MCALF1X             No, continue                         LB1100e
MCALF1F  EQU   *                                                        LB1100e
         MVC   0(3,R2),MCAL2JJJ    JJJ                                  LB1100e
MCALF1X  EQU   *                                                        LB1100e
         LA    R2,VARWORK+20       End   of Row1                        LB1100e
*                                                                       LB1100e
         EJECT                                                          LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Row 2     '  S  M  T  W  T  F  S'   21-bytes         */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
         LA    R6,=C'SMTWTFS'      Weekday String                       LB1100e
         LA    R5,7                                                     LB1100e
MCALF2A  EQU   *                                                        LB1100e
         LA    R2,1(R2)            Bump up VARWORK addr (start)         LB1100e
         CLC   =C'MCALA ',FUNCT    MCALA Function ?                     LB1100e
         BNE   MCALF2C             No.                                  LB1100e
*                                  Yes.                                 LB1100e
         C     R5,=F'7'            Weekend Date?                        LB1100e
         BE    MCALF2B             Yes.                                 LB1100e
         C     R5,=F'1'            Weekend Date?                        LB1100e
         BE    MCALF2B             Yes.                                 LB1100e
         MVC   0(1,R2),MCALA4$     No, Weekday Date Attribute4          LB1100e
         B     MCALF2D                                                  LB1100e
MCALF2B  EQU   *                                                        LB1100e
         MVC   0(1,R2),MCALA3$     Attribute3                           LB1100e
         B     MCALF2D                                                  LB1100e
MCALF2C  EQU   *                                                        LB1100e
         MVI   0(R2),C' '                                               LB1100e
MCALF2D  EQU   *                                                        LB1100e
         LA    R2,2(R2)            Bump up VARWORK addr                 LB1100e
         LA    R3,3(R3)            Bump up result length                LB1100e
         MVC   0(1,R2),0(R6)       Move weekday name                    LB1100e
         LA    R6,1(R6)            Bump up Weekday string               LB1100e
         BCT   R5,MCALF2A          Loop...                              LB1100e
MCALF2X  EQU   *                                                        LB1100e
         LA    R2,VARWORK+41       End of   Row2                        LB1100e
*                                                                       LB1100e
         EJECT                                                          LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Row 3     '  .  .  .  .  .  .  1'   21-bytes         */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
         BAL   R5,S#CDOW           Day of week number                   LB1100e
         PACK  DW,DCCDOW           Pack it                              LB1100e
         CVB   R5,DW               Convert to binary                    LB1100e
         LTR   R5,R5               Is it zero?                          LB1100e
         BZ    MCALF3X             Yes, no dots needed!                 LB1100e
MCALF3A  EQU   *                                                        LB1100e
         LA    R2,1(R2)            Bump up VARWORK addr                 LB1100e
         CLC   =C'MCALA ',FUNCT    MCALA Function ?                     LB1100e
         BE    MCALF3B             Yes.                                 LB1100e
         MVI   0(R2),C' '          No.                                  LB1100e
         B     MCALF3C             Continue...                          LB1100e
MCALF3B  EQU   *                                                        LB1100e
         MVC   0(1,R2),MCALA4$     Attribute4                           LB1100e
MCALF3C  EQU   *                                                        LB1100e
         LA    R2,2(R2)            Bump up VARWORK addr                 LB1100e
         LA    R3,3(R3)            Bump up result length                LB1100e
         MVI   0(R2),C'.'          Move a dot!                          LB1100e
         BCT   R5,MCALF3A          Loop...                              LB1100e
MCALF3X  EQU   *                                                        LB1100e
*                                                                       LB1100e
         EJECT                                                          LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Calendar dates                                       */      LB1100e
*     * /* Row 4     '  2  3  4  5  6  7  8'   21-bytes         */      LB1100e
*     * /* Row 5     '  9 10 11 12 13 14 15'   21-bytes         */      LB1100e
*     * /* Row 6     ' 16 17 18 19 20 21 22'   21-bytes         */      LB1100e
*     * /* Row 7     ' 23 24 25 26 27 28 29'   21-bytes         */      LB1100e
*     * /* Row 8     ' 30 31               '   21-bytes         */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
         ZAP   DW,DC#MDAYS         Number of days in Month, packed      LB1100e
         CVB   R5,DW               Convert to binary                    LB1100e
         ZAP   DC#DD,=P'1'         Date counter                         LB1100e
MCALF4A  EQU   *                                                        LB1100e
         LA    R2,2(R2)            Bump up VARWORK addr                 LB1100e
         LA    R3,3(R3)            Bump up result length                LB1100e
         CLC   =C'MCALA ',FUNCT    MCALA Function ?                     LB1100e
         BE    MCALF4B             Yes.                                 LB1100e
         B     MCALF4C             No, continue                         LB1100e
MCALF4B  EQU   *                                                        LB1100e
         BCTR  R2,0                Move back 1 position                 LB1100e
         MVC   0(1,R2),MCALA4$     Attribute4                           LB1100e
         LA    R2,1(R2)            Restore position                     LB1100e
MCALF4C  EQU   *                                                        LB1100e
         UNPK  0(2,R2),DC#DD(2)    Unpack date                          LB1100e
         OI    1(R2),X'F0'         F-zone                               LB1100e
         CLC   MCALRMCY,MCAL2MCY   Request MMCCYY = current MMCCYY ?    LB1100e
         BNE   MCALF4H             No, continue                         LB1100e
         CLC   0(2,R2),MCAL2DD     Yes, mark todays date ?              LB1100e
         BNE   MCALF4H             No, continue                         LB1100e
         CLI   0(R2),C'0'          Leading zero on date?                LB1100e
         BE    MCALF4F             Yes, mark todays date                LB1100e
         BCTR  R2,0                No.                                  LB1100e
         CLC   =C'MCALA ',FUNCT    MCALA Function ?                     LB1100e
         BE    MCALF4D             Yes.                                 LB1100e
         MVI   0(R2),C'@'          No.                                  LB1100e
         B     MCALF4E                                                  LB1100e
MCALF4D  EQU   *                                                        LB1100e
         MVC   0(1,R2),MCALA5$     Attribute5                           LB1100e
MCALF4E  EQU   *                                                        LB1100e
         LA    R2,1(R2)                                                 LB1100e
         B     MCALF4I                                                  LB1100e
MCALF4F  EQU    *                                                       LB1100e
         CLC   =C'MCALA ',FUNCT    MCALA Function ?                     LB1100e
         BE    MCALF4G             Yes.                                 LB1100e
         MVI   0(R2),C'@'                                               LB1100e
         B     MCALF4H             Continue...                          LB1100e
MCALF4G  EQU   *                                                        LB1100e
         MVC   0(1,R2),MCALA5$     Attribute5                           LB1100e
MCALF4H  EQU   *                                                        LB1100e
         CLI   0(R2),C'0'          Leading zero ?                       LB1100e
         BNE   MCALF4I             No, leave unchanged                  LB1100e
         MVI   0(R2),C' '          Yes, blank it out                    LB1100e
MCALF4I  EQU   *                                                        LB1100e
         LA    R2,1(R2)            Bump up VARWORK addr                 LB1100e
         AP    DC#DD,=P'1'         Get next date                        LB1100e
         BCT   R5,MCALF4A                                               LB1100e
*     * /--------------------------------------------------------/      LB1100e
*     * /* Done                                                 */      LB1100e
*     * /--------------------------------------------------------/      LB1100e
MCALFXX  EQU   *                                                        LB1100e
         CLC   =C'MCALA ',FUNCT    MCALA Function ?                     LB1100e
         BNE   MCALFXXA            No.                                  LB1100e
         MVC   VARWORK(1),MCALA1$  Yes, Row1, start with attribute1     LB1100e
MCALFXXA EQU   *                                                        LB1100e
         LA    R7,168              Calendar text string length          LB1100e
         BAL   R5,S#UVAR2          Update VAR2                          LB1100e
         B     ERRDONE             Successful Request                   LB1100e
*                                                                       LB1100e
         EJECT                                                          LB1105a
*     * /********************************************************/      LB1105a
*72   * /* Function: LEN                    REQ NO              */      LB1105a
*73   * /* Function: SLEN                   REQ NO              */      LB1105a
*     * /********************************************************/      LB1105a
*     * /* LEN   : Length of data in string                     */      LB1105a
*     * /* SLEN  : Length of string                             */      LB1105a
*     * /* - R2, R5                   Working Register          */      LB1105a
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */      LB1105a
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */      LB1105a
*     * /********************************************************/      LB1105a
*     * /* - RC=n     Length                                    */      LB1105a
*     * /********************************************************/      LB1105a
PLEN     EQU   *                   Process LEN                          LB1105a
PSLEN    EQU   *                   Process SLEN                         LB1105a
         BAL   R5,S#GVAR1          Get VAR1                             LB1105a
         CLC   =C'SLEN ',FUNCT     SLEN Function ?                      LB1105a
         BE    PLENX               Yes, return var1 length              LB1105a
*                                  No, continue...                      LB1105a
         AR    R8,R7               Add VAR1 length to start addr        LB1105a
         BCTR  R8,0                ..to determine VAR1 end addr         LB1105a
*     * /--------------------------------------------------------/      LB1105a
*     * /* Look for end of data in string VAR1                  */      LB1105a
*     * /--------------------------------------------------------/      LB1105a
LOOK4EOD EQU   *                                                        LB1105a
         CLC   0(1,R8),DELIM       Blank position?                      LB1105a
         BNE   FNDEOD              NO. Found end of data                LB1105a
         BCTR  R8,0                YES. check again.....                LB1105a
         BCT   R7,LOOK4EOD         Decrement length while DELIM         LB1105a
FNDEOD   EQU   *                                                        LB1105a
*                                  R7=VAR1 revised length               LB1105a
PLENX    EQU   *                                                        LB1105a
         LR    R15,R7              Return LENGTH                        LB1105a
         B     ERRDONEF            Successful Request - function        LB1105a
*                                                                       LB1105a
         EJECT                                                          LB1105c
                                                                        LB1105c
*     * /********************************************************/      LB1105c
*74   * /* Function: OVERLY                 REQ REQ             */      LB1105c
*     * /********************************************************/      LB1105c
*     * /* Overlay substring in VAR1.                           */      LB1105c
*     * /* VAR2 has overlay-str, start                          */      LB1105c
*     * /* - result in VAR1                                     */      LB1105c
*     * /*                                                      */      LB1105c
*     * /* VAR2 contains Search Parms -------------             */      LB1105c
*     * /* Format: "ooooooo",bbb                                */      LB1105c
*     * /*    where;                                            */      LB1105c
*     * /*        o - overlay string in double quotes           */      LB1105c
*     * /*        b - optional, beginning overlay position      */      LB1105c
*     * /*              1-3 numeric digits, default=1           */      LB1105c
*     * /*              If b > VAR1 len, b = 1. /////????????????????   LB1105z
*     * /*        Parms separated by commas (,):                */      LB1105c
*     * /*                                                      */      LB1105c
*     * /* - R1, R2, R4, R5, R6       Working Register          */      LB1105c
*     * /* - R7 Length  of VAR1 value (after BAL S#GVAR1)       */      LB1105c
*     * /* - R8 Address of VAR1 value (after BAL S#GVAR1)       */      LB1105c
*     * /********************************************************/      LB1105c
*     * /* - RC=0     Successful Request                        */      LB1105c
*     * /* - RC=4015  VAR2 is required                          */      LB1105c
*     * /* - RC=4030  No begin "  (overlay string)              */      LB1105c
*     * /* - RC=4031  No end "    (overlay string)              */      LB1105c
*     * /* - RC=4032  Start > End position                      */      LB1105c
*     * /* - RC=4033  Invalid character (delimiter)             */      LB1105c
*     * /* - RC=4034  Not numeric (start / end position)        */      LB1105c
*     * /* - RC=4035  Too many digits (start / end position)    */      LB1105c
*     * /* - RC=4036  Overlay string is null                    */      LB1105c
*     * /********************************************************/      LB1105c
POVRL    EQU   *                   Process OVERLay  ???                 LB1105c
         BAL   R5,S#GVAR1          Get VAR1                             LB1105c
         ST    R7,R7HOLD           Save VAR1 value Length               LB1105c
         ST    R8,R8HOLD           Save VAR1 value Address              LB1105c
*     * /--------------------------------------------------------/      LB1105c
*     * /* Need VAR2 content in position 1                      */      LB1105c
*     * /--------------------------------------------------------/      LB1105c
         CLI   VAR2,C' '           VAR2 name captured?                  LB1105c
         BE    ERR4015             NO,  VAR2 required                   LB1105c
*     * /--------------------------------------------------------/      LB1105c
*     * /* Link to IKJCT441 to obtain VAR2                      */      LB1105c
*     * /--------------------------------------------------------/      LB1105c
         BAL   R5,S#GVAR2          Get VAR2                             LB1105c
         L     R15,RC#441          Restore R15                          LB1105c
         LTR   R15,R15             Successful IKJCT441?                 LB1105c
         BNZ   ERR4015             No, VAR2 required                    LB1105c
*                                                                       LB1105c
         BAL   R5,S#UV2L           Update VAR2L  variable               LB1105c
*                                                                       LB1105c
         EJECT                                                          LB1105c
*     * /--------------------------------------------------------/      LB1105c
*     * /* Check VAR2 length limits           VAR2 L = 1        */      LB1105c
*     * /--------------------------------------------------------/      LB1105c
         L     R5,VALP2            R2  Value Ptr retreived/created      LB1105c
         L     R6,VALL2            R4  Value Len retreived/created      LB1105c
         C     R6,=F'0'            Variable length = 0?                 LB1105c
         BE    ERR4015             Yes, VAR2 required                   LB1105c
*     * /--------------------------------------------------------/      LB1105c
*     * /* Get Overlay Parameters                               */      LB1105c
*     * /--------------------------------------------------------/      LB1105c
         BAL   R1,S#SRCHP          Get Search Parms                     LB1105c
         L     R2,VALP2            VAR2 address                         LB1105c
         L     R4,VALL2            VAR2 length                          LB1105c
*     * /--------------------------------------------------------/      LB1105c
*     * /* Overlay str start  within VAR1 bounds?               */      LB1105c
*     * /--------------------------------------------------------/      LB1105c
         C     R7,SRCHS            VAR1 Length < Str Start              LB1105c
         BL    ERR4032             Yes, Error                           LB1105c
*     * /--------------------------------------------------------/      LB1105c
*     * /* Overlay str length within VAR1 bounds?               */      LB1105c
*     * /--------------------------------------------------------/      LB1105c
         C     R7,SRCHSTRL         VAR1 Length < Str Length             LB1105c
         BL    ERR4032             Yes, Error                           LB1105c
*     * /--------------------------------------------------------/      LB1105c
*     * /* Overlay str overflows VAR1?                          */      LB1105c
*     * /--------------------------------------------------------/      LB1105c
         L     R3,SRCHS            Overlay start pos                    LB1105c
         A     R3,SRCHSTRL          + str Length                        LB1105c
         BCTR  R3,0                ...Adj for compare                   LB1105c
         CR    R3,R7               Beyond VAR1 Length?                  LB1105c
         BH    ERR4032             Yes, Error str overflow              LB1105c
         EJECT                                                          LB1105c
*     * /--------------------------------------------------------/      LB1105c
*     * /* Copy VAR1 to VARWORK                                 */      LB1105c
*     * /--------------------------------------------------------/      LB1105c
         MVC   VARWORK,0(R8)       Move VAR1 to VARWORK                 LB1105c
*     * /--------------------------------------------------------/      LB1105c
*     * /* Overlay into VARWORK                                 */      LB1105c
*     * /--------------------------------------------------------/      LB1105c
OVRLF    EQU   *                                                        LB1105c
         LA    R3,VARWORK          R3=Addr VARWORK                      LB1105c
         A     R3,SRCHS            ..Add overlay start                  LB1105c
         BCTR  R3,0                ...Adj for address start             LB1105c
         LA    R1,SRCHSTR          R1=Addr   of Search String 2         LB1105c
         L     R6,SRCHSTRL         R6=Length of Search String 2         LB1105c
OVRLFL   EQU   *                                                        LB1105c
         MVC   0(1,R3),0(R1)       MOVE REPL String to VARWORK          LB1105c
         LA    R3,1(R3)            Bump up VARWORK                      LB1105c
         LA    R1,1(R1)            Bump to next OVRL String char        LB1105c
         BCT   R6,OVRLFL           Move again...                        LB1105c
*                                  Done...                              LB1105c
*     * /--------------------------------------------------------/      LB1105c
*     * /* Update VAR1 w VARWORK value                          */      LB1105c
*     * /--------------------------------------------------------/      LB1105c
POVRLX   EQU   *                                                        LB1105c
         L     R7,VALL1            Fill Length as VAR1L                 LB1105c
         LA    R2,VARWORK          VARWORK area as VAR1                 LB1105c
         ST    R2,VALP2                                                 LB1105c
         MVI   VAR2,C' '           Force VAR1 to be updated             LB1105c
         BAL   R5,S#UVAR2          Update VAR1, no update to VAR2       LB1105c
         B     ERRDONE                                                  LB1105c
*                                                                       LB1105c
         EJECT
*     * /********************************************************/
*     * /* Error Setting                                        */
*     * /********************************************************/
ERRDONE  EQU   *
         MVC   ERRMSG(MSGDONEL),MSGDONE
         B     ENDME
ERRDONEF EQU   *
         MVC   ERRMSG(MSGDONEL),MSGDONE
         MVC   ERRMSG+11(8),FUNCT
         B     ENDME
ERR4000  EQU   *
         LA    R15,4000            FALSE
         B     ERRCALL                                                  LB1100m
ERR4001  EQU   *
         LA    R15,4001            TRUE
         B     ERRCALL                                                  LB1100m
ERR4002  EQU   *
         LA    R15,4002            VAR1 not found
         B     ERRCALL                                                  LB1100m
ERR4004  EQU   *
         LA    R15,4004            No PARM
         B     ERRCALL                                                  LB1100m
ERR4005  EQU   *
         LA    R15,4005            WORD GT 8 bytes
         B     ERRCALL                                                  LB1100m
ERR4008  EQU   *
         LA    R15,4008            CPPL no Parm
         B     ERRCALL                                                  LB1100m
ERR4009  EQU   *
         LA    R15,4009            Too many WORDS
         B     ERRCALL                                                  LB1100m
         EJECT
ERR4010  EQU   *
         LA    R15,4010            SETVAR no content
         B     ERRCALL                                                  LB1100m
ERR4011  EQU   *
         LA    R15,4011            Invalid Function
         B     ERRCALL                                                  LB1100m
ERR4012  EQU   *
         LA    R15,4012            VAR2 not updated
         B     ERRCALL                                                  LB1100m
ERR4014  EQU   *
         LA    R15,4014            VAR1 GT 256
         B     ERRCALL                                                  LB1100m
ERR4015  EQU   *
         LA    R15,4015            VAR2 required
         B     ERRCALL                                                  LB1100m
ERR4016  EQU   *
         LA    R15,4016            VAR2 not found
         B     ERRCALL                                                  LB1100m
ERR4017  EQU   *
         LA    R15,4017            VAR2 GT 256
         B     ERRCALL                                                  LB1100m
ERR4018  EQU   *
         LA    R15,4018            VAR2 is null
         B     ERRCALL                                                  LB1100m
ERR4019  EQU   *
         LA    R15,4019            VAR1 is null
         B     ERRCALL                                                  LB1100m
         EJECT
ERR4020  EQU   *
         LA    R15,4020            ERRMSG cannot create updt
         B     ERRCALL                                                  LB1100m
ERR4021  EQU   *
         LA    R15,4021            VAR1 required
         B     ERRCALL                                                  LB1100m
ERR4022  EQU   *
         LA    R15,4022            VAR2 not required
         B     ERRCALL                                                  LB1100m
ERR4023  EQU   *
         LA    R15,4023            Result GT 256
         B     ERRCALL                                                  LB1100m
         EJECT
ERR4030  EQU   *
         LA    R15,4030            No begin "
         B     ERRCALL                                                  LB1100m
ERR4031  EQU   *
         LA    R15,4031            No end "
         B     ERRCALL                                                  LB1100m
ERR4032  EQU   *
         LA    R15,4032            Start GT End
         B     ERRCALL                                                  LB1100m
ERR4033  EQU   *
         LA    R15,4033            Invalid char
         B     ERRCALL                                                  LB1100m
ERR4034  EQU   *
         LA    R15,4034            Not numeric
         B     ERRCALL                                                  LB1100m
ERR4035  EQU   *
         LA    R15,4035            Too many digits
         B     ERRCALL                                                  LB1100m
ERR4036  EQU   *
         LA    R15,4036            Null value not allowed
         B     ERRCALL                                                  LB1100m
         EJECT
ERR4037  EQU   *
         LA    R15,4037            No begin QUOTE
         B     ERRCALL                                                  LB1100m
ERR4038  EQU   *
         LA    R15,4038            No end QUOTE
         B     ERRCALL                                                  LB1100m
ERR4039  EQU   *
         LA    R15,4039            No end QUOTE found
         B     ERRCALL                                                  LB1100m
         EJECT
ERR4040  EQU   *
         LA    R15,4040            Invalid length
         B     ERRCALL                                                  LB1100m
ERR4047  EQU   *
         LA    R15,4047            Invalid DD
         B     ERRCALL                                                  LB1100m
ERR4048  EQU   *                                                        LB1100f
         LA    R15,4048            Invalid DOW                          LB1100f
         B     ERRCALL                                                  LB1100m
ERR4049  EQU   *
         LA    R15,4049            Invalid MM
         B     ERRCALL                                                  LB1100m
         EJECT
ERR4050  EQU   *
         LA    R15,4050            Invalid DD for MM
         B     ERRCALL                                                  LB1100m
ERR4051  EQU   *
         LA    R15,4051            Invalid JJJ
         B     ERRCALL                                                  LB1100m
ERR4060  EQU   *                                                        LB1100g
         LA    R15,4060            Call error in CUTILTBL               LB1100g
         MVC   ERRMSG(MSG4060L),MSG4060                                 LB1100g
         B     ENDME                                                    LB1100g
ERR4071  EQU   *                                                        LB1100e
         LA    R15,4071            Invalid function                     LB1100e
         B     ERRCALL                                                  LB1100e
ERR4072  EQU   *                                                        LB1100e
         LA    R15,4072            Invalid function                     LB1100e
         B     ERRCALL                                                  LB1100e
ERR4073  EQU   *                                                        LB1100e
         LA    R15,4073            Invalid function                     LB1100e
         B     ERRCALL                                                  LB1100e
ERR4074  EQU   *                                                        LB1100e
         LA    R15,4074            Invalid function                     LB1100e
         B     ERRCALL                                                  LB1100e
ERR4075  EQU   *                                                        LB1100e
         LA    R15,4075            Invalid function                     LB1100e
         B     ERRCALL                                                  LB1100e
ERR4097  EQU   *                                                        LB1105b
         L     R15,=F'4097'        Cannot load CUTILTBL                 LB1105b
         MVC   ERRMSG(MSG4097L),MSG4097                                 LB1105b
         B     ENDME                                                    LB1105b
ERR4098  EQU   *
         L     R15,=F'4098'        Cannot link to IKJCT441
         B     ERRCALL                                                  LB1100m
ERRCALL  EQU   *                                                        LB1100m
         MVC   PALP1,=C'9ERR'      Parm 1: Function Request             LB1100m
         ST    R15,PALP2           Parm 2: Error number (F  )           LB1100m
         LA    R1,ERRMSG           Parm 3: Error msg (ERRMSG)           LB1100m
         ST    R1,PALP3                                                 LB1100m
         LA    R1,PALLST           R1=Parm Addr List                    LB1100m
         L     R15,CUTILTBL        Call CUTILTBL                        LB1105b
         BALR  R14,R15                                                  LB1105b
*                                  RC = error number from R15           LB1100m
*                                       above in PALP2                  LB1100m
         B     ENDME                                                    LB1100m
*     *
         EJECT
*     * /********************************************************/
*     * /* Exit, return to caller                               */
*     * /* Update ERRMSG, VAR1L, VAR2L, FREEMAIN, BR14          */
*     * /********************************************************/
ENDME    DS    0H                  Return....
         ST    R15,SAVER15         SAVE R15, RC for exit
         BAL   R5,S#UERRM          Update ERRMSG variable
         BAL   R5,S#UV1L           Update VAR1L  variable
         BAL   R5,S#UV2L           Update VAR2L  variable
MYEXIT   EQU   *
*     * /********************************************************/
*     * /* - Restore savearea, FREEMAIN, and                    */
*     * /*   return to caller                                   */
*     * /********************************************************/
         LR    R1,R13              Working Storage Addr w Savearea
         L     R5,SAVER15          Load R5 with R15 return code
         L     R13,4(R13)          Restore R13 w callers savearea
*     * /--------------------------------------------------------/
*     * /* Working Storage FREEMAIN                             */
*     * /--------------------------------------------------------/
         PRINT GEN
         FREEMAIN  R,LV=WSAREAL,A=(R1) Free Storage
         PRINT NOGEN
*     * /--------------------------------------------------------/
*     * /* Restore caller registers and return to caller        */
*     * /--------------------------------------------------------/
         LR    R15,R5              R15 = RC for exit
NOMAINS  EQU   *
RETURN   EQU   *
         L     R14,12(R13)         Restore R14
         LM    R0,R12,20(R13)      Restore R0 thru R12
         BR    R14                 Return to caller
*     *
         TITLE 'CUTIL00:  - - - -   S U B - R O U T I N E S'
*     * /********************************************************/
*     * /* Subroutine - Get Variable 1                   (R5)   */
*     * /* Get Variable 1 (Source)                              */
*     * /* - R7  Length  of VAR1 symbolic (after return)        */
*     * /* - R8  Address of VAR1 symbolic (after return)        */
*     * /* - R0, R1, R2, R6, R15      Working Register          */
*     * /********************************************************/
*     * /* -,VALL1      VALUE Len retreived/created             */
*     * /* - VALP1      VALUE Ptr retreived/created             */
*     * /* - VAL1       VALUE content retreived/created         */
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
         EJECT
S#GVAR1N EQU   *
         MVI   CHKV1NL,C'N'        Bypass VAR1 NULL test
S#GVAR1  EQU   *
         ST    R5,SAVER5
*     * /--------------------------------------------------------/
*     * /* Bypass VAR1 fetch                                    */
*     * /--------------------------------------------------------/
         CLI   VAR1,C' '           Blank VAR1 name in position 1?
         BE    ERR4021             No, VAR1 missing
*     * /--------------------------------------------------------/
*     * /* Get provided VAR1 name                               */
*     * /--------------------------------------------------------/
         MVC   NAM1,VAR1           Init VARIABLE NAME
         MVC   NAML1,VAR1L         Init VARIABLE NAME LENGTH
*     * /--------------------------------------------------------/
*     * /* Initialize IKJCT441 Parm List area                   */
*     * /--------------------------------------------------------/
         LA    R0,ECNOIMP          Entry Code
         ST    R0,CT441P1
         LA    R0,NAMP1            Name Pointer
         ST    R0,CT441P2          NAMP1
         LA    R0,NAML1            Name Length
         ST    R0,CT441P3          NAML1
         LA    R0,VALP1            Var  Pointer
         ST    R0,CT441P4          VALP1
         LA    R0,VALL1            Var  Length
         ST    R0,CT441P5          VALL1
         LA    R0,TOKEN            Token
         ST    R0,CT441P6
         MVI   CT441P6,B'10000000' Last Parm designation
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441                                     */
*     * /--------------------------------------------------------/
         LA    R1,CT441LST         R1 = Parm List
         L     R15,IKJCT441
         BALR  R14,R15
*
         ST    R15,RC#441          Save R15 from IKJCT441
         LTR   R15,R15             Successful invokation?
         BNZ   ERR4002             No, VAR1  not found
*                                  Yes.
         L     R7,VALL1            R7  Value Len retreived/created
         L     R8,VALP1            R8  Value Ptr retreived/created
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Check length of variable limits                      */
*     * /--------------------------------------------------------/
         C     R7,=F'256'          Variable length > 256?
         BH    ERR4014             Cannot process. 256 limit.
         CLI   CHKV1NL,C'N'        Check VAR1 for NULL?
         BE    BYPSV1NL            No, bypass check
*                                  Yes, check for NULL
         C     R7,=F'0'            Variable length = 0?
         BE    ERR4019             Yes, null value. Exit.
BYPSV1NL EQU   *
*     * /--------------------------------------------------------/
*     * /* Copy value of CLIST value                            */
*     * /--------------------------------------------------------/
         EX    R7,MVCVAR1          Execute VAR1 value move
*MVCVAR1  MVC   VAL1(0),0(R8)       Save VAR1  value
*
         EJECT
*     * /********************************************************/
*     * /* QUOTE, rebuild VAR1 based on QUOTE content           */
*     * /* - R6                       Working Register          */
*     * /********************************************************/
         CLC   SQUOTE,=C'QUOTE   ' QUOTE Processing?
         BNE   S#GVAR1X            No, branch to continue
         ST    R8,R8HOLD           Yes, take content between quotes
         ST    R7,R7HOLD
*              VAR1 = 'I am in set of quotes  '
*                     ~----------------------------~
*                     0--------1---------2---------3---------4
*                     1---+----0----+----0----+----0----+----0
*              R7   =  30   (length)  FW   R8   =  5003 (address) FW
         CLI   0(R8),C''''         VAR1 1st byte a QUOTE?
         BNE   ERR4037             NO,  Error need quote at start!
         AR    R8,R7               YES, Point to end of
         BCTR  R8,0                ... VAR1 and check/find end quote
         BCTR  R7,0                Adjust length of VAR1
LOOKBBB  EQU   *                   Look for end quote ...
         CLI   0(R8),C' '          VAR1 LAST byte a BLANK?
         BNE   ENDVAR1             NO, Check for end QUOTE in VAR1
         BCTR  R8,0                YES, Move back one byte in VAR1
         BCT   R7,LOOKBBB          ... and Look again.
         B     ERR4039             Did not find ending quote
ENDVAR1  EQU   *
         CLI   0(R8),C''''         Is LAST byte a quote?
         BNE   ERR4038             NO,  Error need quote at end!
         BCTR  R7,0                R7=Revised VAR1 Length minus QUOTE
MOVQTVAR EQU   *
         LA    R6,VARWORK          R6=Addr of VARWORK
         L     R8,R8HOLD           Restore R8, start of VAR1 content
         LA    R8,1(R8)            Adjust R8 for beginning QUOTE        LB1100z
         EX    R7,MVCR8$6          Move VAR1 content to VARWORK
*MVCR8$6  MVC   0(0,R6),0(R8)
         LA    R6,VAL1             R6=Addr of VAL1 (VAR1 content)
         LA    R8,VARWORK          R8=Addr of VARWORK
         EX    R7,MVCR8$6          Move VARWORD to VAR1 (overlay)
*MVCR8$6  MVC   0(0,R6),0(R8)
         L     R8,R8HOLD           Restore start of VAR1
         LA    R8,1(R8)            Adjust R8 for beginning QUOTE        LB1100z
*
*                                  New VAR1 length and start addr
         ST    R7,VALL1            Store adjusted length for VAR1
         ST    R8,VALP1            Store revised start addr for VAR1
         MVI   VARWORK,C' '        Clear VARWORK
         MVC   VARWORK+1(L'VARWORK),VARWORK
         CLC   VAR2,=C'$NOVAR2 '   NOVAR2 keyword?
         BNE   S#GVAR1X            No, use VAR2 for result
         MVI   VAR2,C' '           Yes, use VAR1 for result
         B     S#GVAR1X
S#GVAR1X EQU   *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Get Variable 2                   (R5)   */
*     * /* Get Variable 2 (Source)                              */
*     * /* - R0, R1                   Working Register          */
*     * /********************************************************/
*     * /* - VALL2      VALUE Len retreived/created             */
*     * /* - VALP2      VALUE Ptr retreived/created             */
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
         EJECT
S#GVAR2N EQU   *
         MVI   CHKV2NL,C'N'        Bypass VAR2 NULL test
S#GVAR2  EQU   *
         ST    R5,SAVER5
         MVC   NAM2,VAR2           Init VARIABLE NAME
         MVC   NAML2,VAR2L         Init VARIABLE NAME LENGTH
*     * /********************************************************/
*     * /* Initialize IKJCT441 Parm List area                   */
*     * /* - R0                       Working Register          */
*     * /* - R15 RC from IKJCT441                               */
*     * /********************************************************/
         LA    R0,ECNOIMP          Entry Code
         ST    R0,CT441P1
         LA    R0,NAMP2            Name Pointer
         ST    R0,CT441P2
         LA    R0,NAML2            Name Length
         ST    R0,CT441P3
         LA    R0,VALP2            Var  Pointer
         ST    R0,CT441P4
         LA    R0,VALL2            Var  Length
         ST    R0,CT441P5
         LA    R0,TOKEN            Token
         ST    R0,CT441P6
         MVI   CT441P6,B'10000000' Last Parm designation
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441                                     */
*     * /--------------------------------------------------------/
         LA    R1,CT441LST         R1 = Parm List
         L     R15,IKJCT441
         BALR  R14,R15
*
         ST    R15,RC#441          Save R15 from IKJCT441
*     * /--------------------------------------------------------/
*     * /* Check length of variable limits                      */
*     * /--------------------------------------------------------/
         CLC   VALL2,=F'256'       Variable length > 256?
         BH    ERR4017             Cannot process. 256 limit.
         CLI   CHKV2NL,C'N'        Check VAR1 for NULL?
         BE    BYPSV2NL            No, bypass check
*                                  Yes, check for NULL
         CLC   VALL2,=F'0'         Variable length = 0?
         BE    ERR4018             Yes, null value. Exit.
BYPSV2NL EQU   *
S#GVAR2X EQU   *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Update/Create Variable 2 (Result)(R5)   */
*     * /*                                                      */
*     * /* If no VAR2 specified, overlay VAR1 with result       */
*     * /* If    VAR2 specified, create/update VAR2 with result */
*     * /* - R7 Length  of VAR1 symbolic                        */
*     * /* - R0, R1                   Working Register          */
*     * /********************************************************/
S#UVAR2  EQU    *
         ST    R5,SAVER5
         MVC   VAL2,VARWORK        Copy VARWORK to VAL2
         ST    R7,VALL2            Init VARIABLE DATA LENGTH
         CLI   VAR2,C' '           VAR2 symbolic name blank?
         BE    USEVAR1             YES, Update VAR1 symbolic
USEVAR2  EQU   *                   NO, Update VAR2 symbolic
         MVC   NAM2,VAR2           Init VARIABLE NAME
         MVC   NAML2,VAR2L         Init VARIABLE NAME LENGTH
         B     USECONT
USEVAR1  EQU   *                   Update VAR2 symbolic
         MVC   NAM2,VAR1           Init VARIABLE NAME
         MVC   NAML2,VAR1L         Init VARIABLE NAME LENGTH
         MVC   VALL1,VALL2         Update VAR01 data length
USECONT  EQU   *
*     * /--------------------------------------------------------/
*     * /* Initialize IKJCT441 Parm List area                   */
*     * /--------------------------------------------------------/
         LA    R0,ECUPDT           Entry Code
         ST    R0,CT441P1
         LA    R0,NAMP2            Name Pointer
         ST    R0,CT441P2
         LA    R0,NAML2            Name Length
         ST    R0,CT441P3
         LA    R0,VALP2            Var  Pointer
         ST    R0,CT441P4
         LA    R0,VALL2            Var  Length
         ST    R0,CT441P5
         LA    R0,TOKEN            Token
         ST    R0,CT441P6
         MVI   CT441P6,B'10000000' Last Parm designation
         EJECT
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441                                     */
*     * /--------------------------------------------------------/
         LA    R1,CT441LST         R1 = Parm List
         L     R15,IKJCT441
         BALR  R14,R15
*
         ST    R15,RC#441          Save R15 from IKJCT441
         LTR   R15,R15             Successful invokation?
         BNZ   ERR4012             No, VAR2 not updated
S#UVAR2X EQU    *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Check for $NOVAR2 in VAR2 Var    (R5)   */
*     * /*                                                      */
*     * /* Set VAR2 value to SPACE if $NOVAR2                   */
*     * /********************************************************/
S#CHKV2  EQU    *
         ST    R5,SAVER5
         CLC   VAR2,=C'$NOVAR2 '   $NOVAR2 ?
         BNE   S#CHKV2X            NO, return.
         MVI   VAR2,C' '           YES, set VAR2 name to BLANK
S#CHKV2X EQU    *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Update/Create ERRMSG Variable    (R5)   */
*     * /*                                                      */
*     * /* Update/Create ERRMSG Variable                        */
*     * /* - Attempt to update ERRMSG, do not check R15         */
*     * /* - R0, R1, R7               Working Register          */
*     * /********************************************************/
S#UERRM  EQU   *
         ST    R5,SAVER5
         MVC   VAL3(L'ERRMSG),ERRMSG   Copy VARWORK to VAL3
         LA    R7,L'ERRMSG             Length of ERRMSG
         ST    R7,VALL3            Init VARIABLE DATA LENGTH
         MVC   NAM3,=C'ERRMSG  '   Init VARIABLE NAME
         MVC   NAML3,=F'6'         Init VARIABLE NAME LENGTH
*     * /--------------------------------------------------------/
*     * /* Initialize IKJCT441 Parm List area                   */
*     * /--------------------------------------------------------/
         LA    R0,ECUPDT           Entry Code
         ST    R0,CT441P1
         LA    R0,NAMP3            Name Pointer
         ST    R0,CT441P2
         LA    R0,NAML3            Name Length
         ST    R0,CT441P3
         LA    R0,VALP3            Var  Pointer
         ST    R0,CT441P4
         LA    R0,VALL3            Var  Length
         ST    R0,CT441P5
         LA    R0,TOKEN            Token
         ST    R0,CT441P6
         MVI   CT441P6,B'10000000' Last Parm designation
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441                                     */
*     * /--------------------------------------------------------/
         LA    R1,CT441LST         R1 = Parm List
         L     R15,IKJCT441
         BALR  R14,R15
         ST    R15,RC#441          Save R15 from IKJCT441
S#UERRMX EQU   *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Update/Create VAR1 Length Var    (R5)   */
*     * /*                                                      */
*     * /* - Attempt to update var1L,  do not check R15         */
*     * /* - R0, R1, R2               Working Register          */
*     * /********************************************************/
S#UV1L   EQU   *
         ST    R5,SAVER5
         CLI   NAM1,C' '           Bypass if no NAM1 exists
         BE    S#UV1LX
         MVC   NAM3(8),NAM1        Copy NAM1 variable name
         MVI   NAM3+8,C' '         Blank position 9
         LA    R2,NAM3             Starting address of NAM3
         A     R2,NAML1            Adjust address to past of NAM3
         MVI   0(R2),C'L'          Append 'L' to variable name
         L     R2,NAML1            Get NAM1 variable length
         LA    R2,1(R2)            Adjust by 1
         ST    R2,NAML3            Store Length
         LA    R2,5                Length of value (5)
         ST    R2,VALL3            Store Length
         L     R2,VALL1            Length of NAM1 Value
         CVD   R2,DW               Convert to decimal
         UNPK  VAL3(5),DW+5(3)     Unpack and
         OI    VAL3+4,X'F0'        ... force an F zone
*     * /--------------------------------------------------------/
*     * /* Initialize IKJCT441 Parm List area                   */
*     * /--------------------------------------------------------/
         LA    R0,ECUPDT           Entry Code
         ST    R0,CT441P1
         LA    R0,NAMP3            Name Pointer
         ST    R0,CT441P2
         LA    R0,NAML3            Name Length
         ST    R0,CT441P3
         LA    R0,VALP3            Var  Pointer
         ST    R0,CT441P4
         LA    R0,VALL3            Var  Length
         ST    R0,CT441P5
         LA    R0,TOKEN            Token
         ST    R0,CT441P6
         MVI   CT441P6,B'10000000' Last Parm designation
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441                                     */
*     * /--------------------------------------------------------/
         LA    R1,CT441LST         R1 = Parm List
         L     R15,IKJCT441
         BALR  R14,R15
         ST    R15,RC#441          Save R15 from IKJCT441
S#UV1LX  EQU   *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Update/Create VAR2 Length Var    (R5)   */
*     * /*                                                      */
*     * /* - Attempt to update var2L,  do not check R15         */
*     * /* - R0, R1, R2               Working Register          */
*     * /********************************************************/
S#UV2L   EQU   *
         ST    R5,SAVER5
         CLI   NAM2,C' '           Bypass if no NAM2 exists
         BE    S#UV2LX
         CLI   VAR2,C' '           VAR2 symbolic name blank?
         BE    S#UV2LX             YES, go to var2l exit
         MVC   NAM3(8),NAM2        Copy NAM1 variable name
         MVI   NAM3+8,C' '         Blank position 9
         LA    R2,NAM3             Starting address of NAM3
         A     R2,NAML2            Adjust address to past of NAM3
         MVI   0(R2),C'L'          Append 'L' to variable name
         L     R2,NAML2            Get NAM1 variable length
         LA    R2,1(R2)            Adjust by 1
         ST    R2,NAML3            Store Length
         LA    R2,5                Length of value (5)
         ST    R2,VALL3            Store Length
         L     R2,VALL2            Length of NAM2 Value
         CVD   R2,DW               Convert to decimal
         UNPK  VAL3(5),DW+5(3)     Unpack and
         OI    VAL3+4,X'F0'        ... force an F zone
*     * /--------------------------------------------------------/
*     * /* Initialize IKJCT441 Parm List area                   */
*     * /--------------------------------------------------------/
         LA    R0,ECUPDT           Entry Code
         ST    R0,CT441P1
         LA    R0,NAMP3            Name Pointer
         ST    R0,CT441P2
         LA    R0,NAML3            Name Length
         ST    R0,CT441P3
         LA    R0,VALP3            Var  Pointer
         ST    R0,CT441P4
         LA    R0,VALL3            Var  Length
         ST    R0,CT441P5
         LA    R0,TOKEN            Token
         ST    R0,CT441P6
         MVI   CT441P6,B'10000000' Last Parm designation
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441                                     */
*     * /--------------------------------------------------------/
         LA    R1,CT441LST         R1 = Parm List
         L     R15,IKJCT441
         BALR  R14,R15
         ST    R15,RC#441          Save R15 from IKJCT441
S#UV2LX  EQU   *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Write text to screen PUTLINE     (R5)   */
*     * /* save/restore R7,R8                                   */
*     * /* - R7, R8                   Working Register          */
*     * /********************************************************/
S#PUTL   EQU   *
         ST    R5,SAVER5
         STM   R7,R8,SAVER7R8      Save R7,R8
         LM    R7,R8,MYUPT         Load UPT and ECT
         PUTLINE PARM=PUTBLK,UPT=(R7),ECT=(R8),ECB=MYECB,              X
               OUTPUT=(OUTTXT,TERM,SINGLE,DATA),MF=(E,IOPLADS)
         LM    R7,R8,SAVER7R8      Restore R7,R8
S#PUTLX  EQU    *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Test Numeric (TRUE/FALSE)        (R5)   */
*     * /* - R7  Length                                         */
*     * /* - R8  Address of Content                             */
*     * /* - R15 Return Code                                    */
*     * /*       4001-TRUE                                      */
*     * /*       4000-FALSE                                     */
*     * /********************************************************/
S#ISNUM  EQU   *
         ST    R5,SAVER5
         CLI   0(R8),C'0'          Byte < 0?
         BL    S#ISNUMF            YES, not numeric, FALSE
         CLI   0(R8),C'9'          Byte > 9?
         BH    S#ISNUMF            YES, not numeric, FALSE
         LA    R8,1(R8)            Bump up to next byte of VAR1
         BCT   R7,S#ISNUM          LOOP for numeric
*                                  At this point, all numeric!
S#ISNUMT EQU   *
         LA    R15,4001            TRUE, numeric
         B     S#ISNUMX
S#ISNUMF EQU   *
         LA    R15,4000            FALSE, not numeric
S#ISNUMX EQU   *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Determine Leap Year              (R5)   */
*     * /*                                                      */
*     * /* IF CCYY MOD 4   = 0                                  */
*     * /*   IF CCYY MOD 100 = 0                                */
*     * /*     IF CCYY MOD 400 = 0                              */
*     * /*         LEAP =  1                                    */
*     * /*     ELSE                                             */
*     * /*         LEAP =  0                                    */
*     * /*   ELSE                                               */
*     * /*       LEAP =  1                                      */
*     * /* ELSE                                                 */
*     * /*     LEAP =  0                                        */
*     * /*                                                      */
*     * /* - Input : DCCCCYY   initialized to format 0CCYY      */
*     * /* - Output: DC#LEAP   1-LeapYear  0-CommonYear         */
*     * /* -         DC#YDAYS  366-LeapYear  365-CommonYear     */
*     * /********************************************************/
S#ISLPY  EQU   *
         ST    R5,SAVER5
         ZAP   DC#LEAP,=P'0'       Assume, not leap year
         ZAP   DC#YDAYS,=P'365'    Assume, 365 days in year
         PACK  DC#YR4,DCCCCYY      Seed YR4 with 0CCYY
*
         ZAP   DC#YR100,DC#YR4     Seed YR100
         ZAP   DC#YR400,DC#YR4     Seed YR400
         DP    DC#YR4,=P'4'        CCYY / 4
         DP    DC#YR100,=P'100'    CCYY / 100
         DP    DC#YR400,=P'400'    CCYY / 400
         CP    DC#YR4+3(1),=P'0'   Remainder 0?
         BH    S#ISLPYX            No, not a leap year
*                                  Yes, check further
         CP    DC#YR100+2(2),=P'0' Remainder 0?
         BH    S#ISLPYT            Yes, it's leap year
*                                  No,  check further
         CP    DC#YR400+2(2),=P'0' Remainder 0?
         BH    S#ISLPYX            No, not leap year
S#ISLPYT EQU   *
         AP    DC#LEAP,=P'1'       Yes, leap year
         AP    DC#YDAYS,=P'1'      ...  366 days in year
S#ISLPYX EQU   *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Build DATE (monthname dd, ccyy)  (R5)   */
*     * /--------------------------------------------------------/
*     * /* R2 - Addr to receive content                         */
*     * /* R3 - Length of content                               */
*     * /* R4 - Addr of MONTHNAME                               */
*     * /* R6 - Loop control for monthname                      */
*     * /* Bypass date build when byte 1 of VARWORK is blank    */      LB1100a
*     * /********************************************************/
S#BD8    EQU   *
         ST    R5,SAVER5
*     * /--------------------------------------------------------/
*     * /* Build MONTHNAME   JANUARY                            */
*     * /--------------------------------------------------------/
S#BMN    EQU   *
         CLI   0(R4),C' '          BLANK in MONTHNAME
         BE    S#BDD               Yes, continue...
         MVC   0(1,R2),0(R4)       Move MONTHNAME
         LA    R2,1(R2)            Bump up VARWORK
         LA    R4,1(R4)            Bump up MONTHNAME
         LA    R3,1(R3)            Bump up VARWWORKL
         BCT   R6,S#BMN            Again...
*     * /--------------------------------------------------------/
*     * /* Build DD          JANUARY 10,                        */
*     * /--------------------------------------------------------/
S#BDD    EQU   *                   Build DD
         MVI   0(R2),C' '          Leave a blank after MONTHNAME
         LA    R2,1(R2)            Bump up VARWORK
         LA    R3,1(R3)            Bump up VARWORKL
         CLI   VARWORK,C' '        BLANK in byte 1 of VARWORK           LB1100a
         BE    S#BCCYY             Yes, bypass date BDD                 LB1100a
         MVC   0(2,R2),DCDDD+1     MOVE DD
         LA    R2,2(R2)            Bump up VARWORK by 2 for DD
         LA    R3,2(R3)            Bump up VARWORKL by 2 for DD
         MVC   0(2,R2),=C', '      Place ', ' after DD
         LA    R2,2(R2)            Bump up VARWORK by 2 for ', '
         LA    R3,2(R3)            Bump up VARWORKL by 2 for ', '
*     * /--------------------------------------------------------/
*     * /* Build CCYY        JANUARY 10, 2000                   */
*     * /--------------------------------------------------------/
S#BCCYY  EQU   *                   Build DD
         MVC   0(4,R2),DCCCCYY+1   Move CCYY
         LA    R2,4(R2)            Bump up VARWORK by 4 for CCYY
         LA    R3,4(R3)            Bump up VARWORKL by 4 for CCYY
S#BD8X   EQU   *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Calculate Day of Week (0-6)      (R5)   */
*     * /*              and Weekday (Sunday-Saturday)           */
*     * /--------------------------------------------------------/
*     * /* - Input : DCCCCYY   initialized to format 0CCYY      */
*     * /* -         DCDDD     initialized to format 0DD        */
*     * /* -         DCMMM     initialized to format 0MM        */
*     * /* -         DC#TRM    initialized to month term (packed*/
*     * /* - Output: DCCDOW    0-6                              */
*     * /* -         DCCWDAY   Sunday-Saturday                  */
*     * /--------------------------------------------------------/
*     * /* - R4, R6                   Working Register          */
*     * /********************************************************/
S#CDOW   EQU   *
         ST    R5,SAVER5
*     * /--------------------------------------------------------/
*     * /* Calculate DOW (Day of Week Number)                        */
*     * /*                                                           */
*     * /* Using Sakamoto's method:                                  */
*     * /*                                                           */
*     * /* DOW(y, m, d) /* 1 <= m <= 12,  y > 1752 (in the U.K.)     */
*     * /* {                                                         */
*     * /*   static int t[[ = {0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4};  */
*     * /*   y -= m < 3;                                  */         */
*     * /*   return (y + y/4 - y/100 + y/400 + t[m-1[ + d) % 7;      */
*     * /* }                                                         */
*     * /*                                                           */
*     * /* Posted by Tomohiko Sakamoto on the comp.lang.c            */
*     * /* Usenet newsgroup in 1992, it is accurate for any          */
*     * /* Gregorian date.                                           */
*     * /*                                                           */
*     * /* See below https:// link                                   */
*     * /* en.wikipedia.org/wiki/Determination_of_the_day_of_the_week*/
*     * /--------------------------------------------------------/
         PACK  DC#DD,DCDDD         Pack 0DD
         PACK  DC#YR4,DCCCCYY      Pack 0CCYY
         CLC   DCMMM+1(2),=C'02'   > FEB?
         BH    S#CDWNA             Yes, do not adjust Year
         SP    DC#YR4,=P'1'        OCCYY - 1
S#CDWNA  EQU   *
         ZAP   DC#DOW,DC#YR4       Seed adjusted CCYY
         ZAP   DC#YR100,DC#YR4     Seed adjusted CCYY
         ZAP   DC#YR400,DC#YR4     Seed adjusted CCYY
         DP    DC#YR4,=P'4'        CCYY / 4
         DP    DC#YR100,=P'100'    CCYY / 100
         DP    DC#YR400,=P'400'    CCYY / 400

         AP    DC#DOW,DC#YR4(3)         + INT(y/4)
         SP    DC#DOW,DC#YR100(2)       - INT(y/100)
         AP    DC#DOW,DC#YR400(2)       + INT(y/400)
         AP    DC#DOW,DC#TRM            + T
         AP    DC#DOW,DC#DD             + DD
         DP    DC#DOW,=P'7'         DOW = INT(DOW/7)
         UNPK  DCCDOW,DC#DOW+3(1)   Remainder of DOW
         OI    DCCDOW,X'F0'
         EJECT
*     * /--------------------------------------------------------/
*     * /*     Determine Weekday                                */
*     * /--------------------------------------------------------/
         MVC   PALP1,=C'1DOW'      Parm 1: Function Request             LB1100f
         LA    R1,DCCDOW           Parm 2: DOW (CL1 DOW)                LB1100f
         ST    R1,PALP2                                                 LB1100f
         LA    R1,PALLST           R1=Parm Addr List                    LB1100f
         L     R15,CUTILTBL        Call CUTILTBL                        LB1105b
         BALR  R14,R15                                                  LB1105b
         LTR   R15,R15             RC = 0 ?                             LB1100f
         BNZ   ERR4048             No, Error - Invalid DOW              LB1100f
         L     R4,PALP3            Yes, DOW entry start addr in Parm 3  LB1100f
         MVC   DCCWDAY,1(R4)       Get Weekday name                     LB1100f
S#CDOWX  EQU   *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Build CYJ input content          (R5)   */
*     * /--------------------------------------------------------/
*     * /* Build C Y J input content based on FUNCT(4)          */
*     * /* - Input : VAR1      via R8                           */
*     * /* -         FUNCT     Function request                 */
*     * /* - Output: DCCCCYY   0CCYY                            */
*     * /* -         DCJJJ     JJJ                              */
*     * /********************************************************/
S#ICYJ   EQU   *
         ST    R5,SAVER5
         MVI   DCCCCYY,C'0'        Move 0 to first byte DCCCYY
         CLC   =C'CYJ-',FUNCT      CYJ- Function                        LB1100y
         BE    INCYJ               Yes, CCYYJJJ input
         CLC   =C'JCY-',FUNCT      JCY- Function                        LB1100y
         BE    INJCY               Yes, JJJCCYY input
         B     ERR4074             No, invalid function

INCYJ    EQU   *                   R8 CCYYJJJ
         MVC   DCCCCYY+1(4),0(R8)  Save CCYY from VAR1
         MVC   DCJJJ,4(R8)         Save JJJ from VAR1
         B     S#ICYJX
INJCY    EQU   *                   R8 JJJCCYY
         MVC   DCCCCYY+1(4),3(R8)  Save CCYY from VAR1
         MVC   DCJJJ,0(R8)         Save JJJ from VAR1
         B     S#ICYJX
S#ICYJX  EQU   *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Build MDCY input content         (R5)   */
*     * /--------------------------------------------------------/
*     * /* Build M D C Y input content based on FUNCT(5)        */
*     * /* - Input : VAR1      via R8                           */
*     * /* -         FUNCT     Function request                 */
*     * /* - Output: DCCCCYY   0CCYY                            */
*     * /* -         DCDDD     0DD                              */
*     * /* -         DCMMM     0MM                              */
*     * /********************************************************/
S#IMDCY  EQU   *
         ST    R5,SAVER5
         MVI   DCCCCYY,C'0'        Move 0 to first byte DCCCYY
         MVI   DCDDD,C'0'          Move 0 to first byte DCDDD
         MVI   DCMMM,C'0'          Move 0 to first byte DCMMM
         CLC   =C'MCAL ',FUNCT     MCAL  Function                       LB1100e
         BE    INMDCY              Yes, MMDDCCYY input                  LB1100e
         CLC   =C'MCALA ',FUNCT    MCALA Function                       LB1100e
         BE    INMDCY              Yes, MMDDCCYY input                  LB1100e
         CLC   =C'MDCY-',FUNCT     MDCY- Function                       LB1100y
         BE    INMDCY              Yes, MMDDCCYY input
         CLC   =C'DMCY-',FUNCT     DMCY- Function                       LB1100y
         BE    INDMCY              Yes, DDMMCCYY input
         CLC   =C'CYMD-',FUNCT     CYMD- Function                       LB1100y
         BE    INCYMD              Yes, CCYYMMDD input
         CLC   =C'CYDM-',FUNCT     CYDM- Function                       LB1100y
         BE    INCYDM              Yes, CCYYDDMM input
         B     ERR4075             No, invalid function
INMDCY   EQU   *                   R8 MMDDCCYY
         MVC   DCCCCYY+1(4),4(R8)  Save CCYY from VAR1
         MVC   DCMMM+1(2),0(R8)         Save MM  from VAR1
         MVC   DCDDD+1(2),2(R8)         Save DD  from VAR1
         B     S#IMDCYX
INDMCY   EQU   *                   R8 DDMMCCYY
         MVC   DCCCCYY+1(4),4(R8)  Save CCYY from VAR1
         MVC   DCMMM+1(2),2(R8)         Save MM  from VAR1
         MVC   DCDDD+1(2),0(R8)         Save DD  from VAR1
         B     S#IMDCYX
INCYMD   EQU   *                   R8 CCYYMMDD
         MVC   DCCCCYY+1(4),0(R8)  Save CCYY from VAR1
         MVC   DCMMM+1(2),4(R8)         Save MM  from VAR1
         MVC   DCDDD+1(2),6(R8)         Save DD  from VAR1
         B     S#IMDCYX
INCYDM   EQU   *                   R8 CCYYDDMM
         MVC   DCCCCYY+1(4),0(R8)  Save CCYY from VAR1
         MVC   DCMMM+1(2),6(R8)         Save MM  from VAR1
         MVC   DCDDD+1(2),4(R8)         Save DD  from VAR1
         B     S#IMDCYX
S#IMDCYX EQU   *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Build M D C Y content            (R5)   */
*     * /--------------------------------------------------------/
*     * /* R2 - addr VARWORK                                    */
*     * /* R3 - Length of content in VARWORK                    */
*     * /* R4 - Addr of FUNCT                                   */
*     * /* R6 - Loop control for FUNCT test                     */
*     * /********************************************************/
S#BMDCY  EQU   *
         ST    R5,SAVER5
         CLI   0(R4),C'M'          Format MM?
         BE    MOVMM               Yes, move MM
         CLI   0(R4),C'D'          Format DD?
         BE    MOVDD               Yes, move DD
         CLI   0(R4),C'C'          Format CC?
         BE    MOVCC               Yes, move CC
         CLI   0(R4),C'Y'          Format YY?
         BE    MOVYY               Yes, move YY
         CLI   0(R4),C'J'          Format JJJ?
         BE    MOVJJJ              Yes, move JJJ
         B     NXTC                No, check next...
MOVMM    EQU   *
         MVC   0(2,R2),DCMMM+1     MM content
         B     MOVXX               Next one...
MOVDD    EQU   *
         MVC   0(2,R2),DCDDD+1     DD content
         B     MOVXX               Next one...
MOVCC    EQU   *
         MVC   0(2,R2),DCCCCYY+1   CC content
         B     MOVXX               Next one...
MOVYY    EQU   *
         MVC   0(2,R2),DCCCCYY+3   YY content
         B     MOVXX               Next one...
MOVJJJ   EQU   *
         MVC   0(3,R2),DCJJJ       JJJ content
         LA    R2,1(R2)            Bump up VARWORK
         LA    R3,1(R3)            Bump up VARWORKL
         B     MOVXX               Next one...
MOVXX    EQU   *
         LA    R2,2(R2)            Bump up VARWORK
         LA    R3,2(R3)            Bump up VARWORKL
         LA    R4,1(R4)            Bump up FUNCT
NXTC     EQU   *
         BCT   R6,S#BMDCY          Check next value
S#BMDCYX EQU    *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
        EJECT
*     * /********************************************************/
*     * /* Subroutine - Determine MM DD from JJJ         (R5)   */
*     * /--------------------------------------------------------/
*     * /* Determine MM and DD from JJJ                         */
*     * /* - Input : DC#LEAP   L   packed                       */
*     * /* -         PKJJJ     JJJ packed                       */
*     * /* - Work  : FW                                         */
*     * /* - Output: DC#TRM    T   packed                       */
*     * /* -         DCDDD     0DD                              */
*     * /* -         DCMMM     0MM                              */
*     * /* -         R4=addr of MONTHNAME                       */
*     * /* -         R6=Length of MONTHNAME                     */
*     * /--------------------------------------------------------/
*     * /* - R3, R4, R5, R6           Working Register          */
*     * /********************************************************/
S#J2MD   EQU    *
         ST    R5,SAVER5
*     * /--------------------------------------------------------/
*     * /* Search for MM using JJJ                              */
*     * /--------------------------------------------------------/
         MVC   PALP1,=C'2J2M'      Parm 1: Function Request             LB1100g
         LA    R1,PKJJJ            Parm 2: JJJ (PL3 Julian)             LB1100g
         ST    R1,PALP2                                                 LB1100g
         LA    R1,DC#LEAP          Parm 3: L   (PL1 Leap Year)          LB1100g
         ST    R1,PALP3                                                 LB1100g
         LA    R1,FW               Parm 4: FW  (F   work pack)          LB1100g
         ST    R1,PALP4                                                 LB1100g
         LA    R1,PALLST           R1=Parm Addr List                    LB1100g
         L     R15,CUTILTBL        Call CUTILTBL                        LB1105b
         BALR  R14,R15                                                  LB1105b
         LTR   R15,R15             RC = 0 ?                             LB1100g
         BNZ   ERR4051             No, Error - Invalid JJJ              LB1100g
         L     R4,PALP5            Yes, MM entry start addr in Parm 5   LB1100g
         USING MPT,R4              Tell Assembler
*     * /--------------------------------------------------------/
*     * /* Found MM                                             */
*     * /--------------------------------------------------------/
**       DC    C'02280313February '
*        R4 points to MM table entry
*        DD=FW(2)-JJJ
         MVI   DCMMM,C'0'          Save MM from
         MVC   DCMMM+1(2),MPTMM    ... table lookup
         PACK  DC#TRM,MPTDWTRM     Pack TERM for DOW calculation
         SP    PKJJJ,FW(2)         Compute DD for MM from JJJ
         UNPK  DCDDD,PKJJJ         Save DD
         OI    DCDDD+2,X'F0'       F-Zone
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Load R4 and R6 with MONTHNAME and Length             */
*     * /--------------------------------------------------------/
         LA    R4,MPTMNAME         Point to MONTHNAME
         LA    R6,MPTMNL           R6=Length of MONTHNAME
         DROP  R4                  Tell Assembler
*
S#J2MDX  EQU    *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Determine JJ from MMDD           (R5)   */
*     * /--------------------------------------------------------/
*     * /* Determine JJJ from MM DD                             */
*     * /* - Input : DC#LEAP   L   packed                       */
*     * /* -         DCDDD     0DD                              */
*     * /* -         DCMMM     0MM                              */
*     * /* - Work  : FW                                         */
*     * /* - Output: DC#JJJ    JJJ packed                       */
*     * /* -         DC#TRM    T   packed                       */
*     * /* -         DC#DD     0DD packed                       */
*     * /* -         DCJJJ     JJJ                              */
*     * /* -         DC#MDAYS  0nn packed                       */      LB1100b
*     * /* -         R4=addr of MONTHNAME                       */
*     * /* -         R6=Length of MONTHNAME                     */
*     * /--------------------------------------------------------/
*     * /* - R3, R4, R6               Working Register          */
*     * /********************************************************/
S#MD2J   EQU    *
         ST    R5,SAVER5
*     * /--------------------------------------------------------/
*     * /* Find MMTABLE entry for MM                            */
*     * /--------------------------------------------------------/
         MVC   PALP1,=C'2MM '      Parm 1: Function Request             LB1100g
         LA    R1,DCMMM+1          Parm 2: MM (CL2 month)               LB1100g
         ST    R1,PALP2                                                 LB1100g
         LA    R1,PALLST           R1=Parm Addr List                    LB1100g
         L     R15,CUTILTBL        Call CUTILTBL                        LB1105b
         BALR  R14,R15                                                  LB1105b
         LTR   R15,R15             RC = 0 ?                             LB1100g
         BNZ   ERR4049             No, Error - Invalid MM               LB1100g
         L     R4,PALP3            Yes, MM entry start addr in Parm 3   LB1100g
         USING MPT,R4              Tell Assembler
**       DC    C'02280313February '
         PACK  DC#JJJ,MPTDAT@1     Pack JJJ b4 start of month
         PACK  DC#TRM,MPTDWTRM     Pack T (term for weekday)
         PACK  DC#MDAYS,MPTMMAXD   Pack Days for requested month        LB1100b
*
         CLC   DCDDD+1(2),=C'01'   DD < 01 ?
         BL    ERR4047             Yes, Error, invalid DD
         CLC   DCMMM+1(2),=C'02'   MM = Feb?
         BNE   CHKDDMAX            No, use DD in MM for max days
         CP    DC#LEAP,=P'1'       Yes, Leap Year?
         BNE   CHKDDMAX            No, use DD in MM for max days
         AP    DC#MDAYS,=P'1'      Yes, Adjust FEB days to 29           LB1100b
         CLC   DCDDD+1(2),=C'29'   Yes, DD > 29 ?
         BH    ERR4047             Yes, Error, invalid DD
         B     GOTMMX              No, done.
CHKDDMAX EQU   *
         CLC   DCDDD+1(2),MPTMMAXD DD > MPT MAX DD for MM
         BH    ERR4050             Yes, Error invalid DD for MM
GOTMMX   EQU   *
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Determine JJJ                                        */
*     * /--------------------------------------------------------/
         PACK  DC#DD,DCDDD         Pack DD
         AP    DC#JJJ,DC#DD        JJJ = JJJ + DD
         CLC   DCMMM+1(2),=C'02'   MM > 02 (FEB) ?
         BNH   GOTJJJX             No.  Do not adjust
         AP    DC#JJJ,DC#LEAP      Yes. Adjust JJJ for leap year
GOTJJJX  EQU   *
         UNPK  DCJJJ,DC#JJJ        DCJJJ and fix F-zone
         OI    DCJJJ+2,X'F0'       ...
*     * /--------------------------------------------------------/
*     * /* Load R4 and R6 with MONTHNAME and Length             */
*     * /--------------------------------------------------------/
         LA    R4,MPTMNAME         Point to MONTHNAME
         LA    R6,MPTMNL           R6=Length of MONTHNAME
         DROP  R4                  Tell Assembler
*
S#MD2JX  EQU    *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
        EJECT
*     * /********************************************************/
*     * /* Subroutine - Updt/Create VAR1nnn Variable     (R5)   */
*     * /--------------------------------------------------------/
*     * /* Input:                                               */
*     * /* R3 - Length of content in VARWORK                    */
*     * /* VARWORK                                              */
*     * /* NAM1                                                 */
*     * /********************************************************/
*     * /* - R0, R1, R2               Working Register          */
*     * /********************************************************/
S#CVARN  EQU    *
         ST    R5,SAVER5
         CLI   NAM1,C' '           Bypass if no NAM1 exists
         BE    S#CVARNX
*     * /********************************************************/
*     * /* Updt/Create VAR1nnn Variable                         */
*     * /********************************************************/
*     * /--------------------------------------------------------/
*     * /* VAR1nnn | Variable Value and Length                  */
*     * /--------------------------------------------------------/
         MVC   VAL4,VARWORK        Move VARWORK content
         ST    R3,VALL4            Store Length of VARWORK content
         AP    NNN,=P'1'           Add 1 to nnn
*     * /--------------------------------------------------------/
*     * /* VAR1nnn | Variable Name w/ appended NNN              */
*     * /--------------------------------------------------------/
         LA    R2,NAM4             Address of NAM4 (copy of NAM1)
         A     R2,NAML1            Point to past end of var name
         MVC   NAM4(8),NAM1        Copy NAM1 variable name
         CP    NNN,=P'9'           0-9
         BNH   S#CVAR1             Yes, 1 digit
         CP    NNN,=P'99'          10-99
         BNH   S#CVAR2             Yes, 2 digits
*                                  No, Must be...
S#CVAR3  EQU   *                   3 digits, 100-999
         UNPK  0(3,R2),NNN         Append to VAR name
         OI    2(R2),X'F0'         F-Zone
         LA    R1,3                Number of digits - 3
         B     S#CVARC
S#CVAR2  EQU   *                   2 Digits, 10-99
         UNPK  0(2,R2),NNN         Append to VAR name
         OI    1(R2),X'F0'         F-Zone
         LA    R1,2                Number of digits - 2
         B     S#CVARC
S#CVAR1  EQU   *                   1 Digit, 0-9
         UNPK  0(1,R2),NNN+1(1)    Append to VAR name
         OI    0(R2),X'F0'         F-Zone
         LA    R1,1                Number of digits - 2
         B     S#CVARC
S#CVARC  EQU   *                   Common Exit
*
        EJECT
*     * /--------------------------------------------------------/
*     * /* VAR1nnn | Variable Name Length                       */
*     * /--------------------------------------------------------/
         A     R1,NAML1            Digits + NAM1 Variable length
         ST    R1,NAML4            Store Length
*     * /--------------------------------------------------------/
*     * /* Initialize IKJCT441 Parm List area                   */
*     * /--------------------------------------------------------/
         LA    R0,ECUPDT           Entry Code
         ST    R0,CT441P1
         LA    R0,NAMP4            Name Pointer
         ST    R0,CT441P2
         LA    R0,NAML4            Name Length
         ST    R0,CT441P3
         LA    R0,VALP4            Var  Pointer
         ST    R0,CT441P4
         LA    R0,VALL4            Var  Length
         ST    R0,CT441P5
         LA    R0,TOKEN            Token
         ST    R0,CT441P6
         MVI   CT441P6,B'10000000' Last Parm designation
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441                                     */
*     * /--------------------------------------------------------/
         LA    R1,CT441LST         R1 = Parm List
         L     R15,IKJCT441
         BALR  R14,R15
*
         ST    R15,RC#441          Save R15 from IKJCT441
*
         EJECT
*     * /********************************************************/
*     * /* Updt/Create VAR10   Variable                         */
*     * /********************************************************/
*     * /--------------------------------------------------------/
*     * /* VAR10   | Variable Value from NNN                    */
*     * /--------------------------------------------------------/
         UNPK  VAL4(3),NNN(2)      Unpack and
         OI    VAL4+2,X'F0'        ... force an F zone
*     * /--------------------------------------------------------/
*     * /* VAR10   | Variable Value Length set to 3             */
*     * /--------------------------------------------------------/
         LA    R0,3
         ST    R0,VALL4            Store Length of VARWORK content
*     * /--------------------------------------------------------/
*     * /* VAR10   | Variable Name                              */
*     * /--------------------------------------------------------/
         MVC   NAM4(8),NAM1     Copy NAM1 variable name
         MVC   NAM4+8(3),BLANKS     and blank remainder
         LA    R2,NAM4             Address of NAM4 (copy of NAM1)
         A     R2,NAML1            Point to past end of var name
         MVC   0(1,R2),=C'0'          VAR10 Name
*     * /--------------------------------------------------------/
*     * /* VAR10   | Variable Name Length                       */
*     * /--------------------------------------------------------/
         L     R0,NAML1            Store Length for Var Name
         A     R0,=F'1'            Adjust by 1
         ST    R0,NAML4            Store Length
*     * /--------------------------------------------------------/
*     * /* Initialize IKJCT441 Parm List area                   */
*     * /--------------------------------------------------------/
         LA    R0,ECUPDT           Entry Code
         ST    R0,CT441P1
         LA    R0,NAMP4            Name Pointer
         ST    R0,CT441P2
         LA    R0,NAML4            Name Length
         ST    R0,CT441P3
         LA    R0,VALP4            Var  Pointer
         ST    R0,CT441P4
         LA    R0,VALL4            Var  Length
         ST    R0,CT441P5
         LA    R0,TOKEN            Token
         ST    R0,CT441P6
         MVI   CT441P6,B'10000000' Last Parm designation
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441                                     */
*     * /--------------------------------------------------------/
         LA    R1,CT441LST         R1 = Parm List
         L     R15,IKJCT441
         BALR  R14,R15
*
         ST    R15,RC#441          Save R15 from IKJCT441
S#CVARNX EQU    *
         L     R5,SAVER5
         BR    R5                  Return to caller (R5)
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Get Search Parms                 (R1)   */
*     * /--------------------------------------------------------/
*     * /* Input:                                               */
*     * /* R3 - Length of content in VARWORK                    */
*     * /* VARWORK                                              */
*     * /* NAM1                                                 */
*     * /********************************************************/
*     * /* Input:                                               */
*     * /* -R5 VAR2 addr                                        */
*     * /* -R6 VAR2 length                                      */
*     * /* - R0, R2, R3, R4           Working Registers         */
*     * /********************************************************/
S#SRCHP  EQU   *
*     * /--------------------------------------------------------/      LB1105f
*     * /* Initialize Search Lengths                            */      LB1105f
*     * /--------------------------------------------------------/      LB1105f
         LA    R3,0                Initialize Search Lengths            LB1105f
         ST    R3,SRCHSTRL                                              LB1105f
         ST    R3,SRCHST2L                                              LB1105f
         ST    R3,SRCHBEGL                                              LB1105f
         ST    R3,SRCHENDL                                              LB1105f
         ST    R3,SRCHS                                                 LB1105f
         ST    R3,SRCHE                                                 LB1105f
*     * /--------------------------------------------------------/
*     * /* Load Search String 1                                 */
*     * /--------------------------------------------------------/
         LA    R3,SRCHSTR          Address of SRCHSTR
*     * /* "ssss ssss","rrrrrrr",bbb,eee     <-- VAR2 template  */
*     * /* ~~~~~~~~~~~                                          */
*     * /--------------------------------------------------------/
*     * /* Need " in position 1                                 */
*     * /--------------------------------------------------------/
         CLI   0(R5),C'"'          Double Quote (DQ) ?
         BNE   ERR4030             NO, error, need DQ
         B     LOADSSL             YES, load search substring
LOADSS   EQU   *                   Load substring until DQ
         LA    R5,1(R5)            Bump to next VAR2 byte
         CLI   0(R5),C'"'          Double Quote?
         BE    LOADSSX             YES, done with substring
         MVC   0(1,R3),0(R5)       NO, load substring
         LA    R3,1(R3)            Bump to next substring byte
LOADSSL  EQU   *
         BCT   R6,LOADSS           Keep loading substring...
         B     ERR4031             No ending DQ found
*     * /--------------------------------------------------------/
*     * /* Search string ended with "                           */
*     * /--------------------------------------------------------/
LOADSSX  EQU   *                   Done with SS after DQ found
         LA    R4,SRCHSTR          Determine length
         SR    R3,R4                 of search string
         ST    R3,SRCHSTRL         Save search string length
         C     R3,=F'0'            Length = 0?
         BE    ERR4036             YES, null string not allowed
*
         EJECT
         CLC   =C'REPLACE ',FUNCT  Replace needs string 2               LB1100y
         BNE   LOADBEGA            Not replace, go to start position
LOADST2  EQU   *
*     * /--------------------------------------------------------/
*     * /* Load Search String  2, optional                      */
*     * /--------------------------------------------------------/
*     * /* "ssss ssss","rrrrrrr",bbb,eee     <-- VAR2 template  */
*     * /*           ~                                          */
*     * /--------------------------------------------------------/
*     * /* Decrement R6 for " on exit from prior parm           */
*     * /--------------------------------------------------------/
         BCT   R6,LOADST20         xx Decrement for "
         B     POSDONE             Loop expired, done w parm scan
LOADST20 EQU   *
         LA    R3,SRCHST2          Addr of String2
         LA    R2,0                Length  String2
*     * /* "ssss ssss","rrrrrrr",bbb,eee     <-- VAR2 memplate  */
*     * /*            ~                                         */
*     * /--------------------------------------------------------/
*     * /* Decrement R6 for delimit check                       */
*     * /--------------------------------------------------------/
         BCT   R6,LOADST21         xx Decrement for delim chk
         B     POSDONE             Loop expired, done w parm scan
LOADST21 EQU   *
         LA    R5,1(R5)            Bump to next VAR2 byte
         CLI   0(R5),C' '          Blank Delimiter?
         BE    POSDONE             Yes, Done. go process...
         CLI   0(R5),C','          Comma?
         BE    LOADS2B             Yes, do string2
         B     ERR4033             Error, inv char after search string
*
         EJECT
*     * /* "ssss ssss","rrrrrrr",bbb,eee     <-- VAR2 template  */
*     * /*             ~                                        */
*     * /--------------------------------------------------------/
*     * /* Need " in next position                              */
*     * /--------------------------------------------------------/
LOADS2B  EQU   *
         LA    R5,1(R5)            Bump to next VAR2 byte
         CLI   0(R5),C'"'          Double Quote (DQ) ?
         BNE   ERR4030             NO, error, need DQ
         B     LOADS2L             YES, load search substring2
*     * /* "ssss ssss","rrrrrrr",bbb,eee     <-- VAR2 template  */
*     * /*              ~~~~~~~~                                */
LOADS2   EQU   *                   Load substring2 until DQ
         LA    R5,1(R5)            Bump to next VAR2 byte
         CLI   0(R5),C'"'          Double Quote?
         BE    LOADS2X             YES, done with substring
         MVC   0(1,R3),0(R5)       NO, load substring
         LA    R3,1(R3)            Bump to next substring byte
LOADS2L  EQU   *
         BCT   R6,LOADS2           Keep loading substring...
         B     ERR4031             No ending DQ found
*     * /* Search string ended with "                           */
LOADS2X  EQU   *                   Done with SS after DQ found
         LA    R4,SRCHST2          Determine length
         SR    R3,R4                 of search string
         ST    R3,SRCHST2L         Save search string length
         C     R3,=F'0'            Length = 0?
         BE    ERR4036             YES, null string not allowed
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Load Search Start position 3-digits                  */
*     * /--------------------------------------------------------/
LOADBEGA EQU   *
*     * /* "ssss ssss","rrrrrrr",bbb,eee     <-- VAR2 template  */
*     * /*                     ~                                */
*     * /--------------------------------------------------------/
*     * /* Decrement R6 for " on exit from prior parm           */
*     * /--------------------------------------------------------/
         BCT   R6,LOADBEG0         xx Decrement for "
         B     POSDONE             Loop expired, done w parm scan
LOADBEG0 EQU   *
*     * /--------------------------------------------------------/
*     * /* Setup for search begin position                      */
*     * /--------------------------------------------------------/
         LA    R3,SRCHBEG          Addr of POS BEG
         LA    R2,0                Length  POS BEG
*     * /* "ssss ssss","rrrrrrr",bbb,eee     <-- VAR2 template  */
*     * /*                      ~                               */
*     * /--------------------------------------------------------/
*     * /* Decrement R6 for delimit check                       */
*     * /--------------------------------------------------------/
         BCT   R6,LOADBEG1         xx Decrement for delim chk
         B     POSDONE             Loop expired, done w parm scan
LOADBEG1 EQU   *
*     * /* "ssss ssss","rrrrrrr",bbb,eee     <-- VAR2 template  */
*     * /*                       ~~~~                           */
         LA    R5,1(R5)            Bump to next VAR2 byte
         CLI   0(R5),C' '          Blank Delimiter?
         BE    POSDONE             Yes, Done.  go process...
         CLI   0(R5),C','          Comma?
         BE    LOADBEG             Yes, up to 3 digits start
         B     ERR4033             Error, inv char after search string
LOADBEG  EQU   *
         LA    R5,1(R5)
         CLI   0(R5),C' '          Blank Delimiter?
         BE    POSDONE             Yes, Done.  go process...
         CLI   0(R5),C','          Comma?
         BE    LOADENDA            Yes, up to 3 digits end
         CLI   0(R5),C'0'          < 0 ?
         BL    ERR4034             YES. must be numeric
         CLI   0(R5),C'9'          > 9 ?
         BH    ERR4034             YES. must be numeric
         MVC   0(1,R3),0(R5)       load start position byte
         LA    R3,1(R3)            Bump to next substring byte
         LA    R2,1(R2)            Count digits
         ST    R2,SRCHBEGL         Store Length
         C     R2,=F'3'            More than 3 digits?
         BH    ERR4035             YES, error.  too many digits
         BCT   R6,LOADBEG          Keep loading substring...
         B     POSDONE             Loop expired, done w parm scan
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Load Search End   position 3-digits                  */
*     * /--------------------------------------------------------/
LOADENDA EQU   *
*     * /* "ssss ssss","rrrrrrr",bbb,eee     <-- VAR2 template  */
*     * /*                          ~                           */
*     * /* Decrement R6 for , exit above                        */
         BCT   R6,LOADEND0         xx Decrement for ,
         B     POSDONE             Loop expired, done w parm scan
LOADEND0 EQU   *
*     * /--------------------------------------------------------/
*     * /* Setup for search end   position                      */
*     * /--------------------------------------------------------/
         CLC   =C'OVERLAY ',FUNCT  Bypass  End for OVERLAY              LB1105c
         BE    POSDONE                                                  LB1105c
         LA    R3,SRCHEND          Addr of POS END
         LA    R2,0                Addr of POS END
LOADEND  EQU   *
*     * /* "ssss ssss","rrrrrrr",bbb,eee     <-- VAR2 template  */
*     * /*                           ~~~~~..                    */
         LA    R5,1(R5)
         CLI   0(R5),C' '          Blank Delimiter?
         BE    POSDONE             Yes, Done.  go process...
         CLI   0(R5),C'0'          < 0 ?
         BL    ERR4034             YES. must be numeric
         CLI   0(R5),C'9'          > 9 ?
         BH    ERR4034             YES. must be numeric
         MVC   0(1,R3),0(R5)       load start position byte
         LA    R3,1(R3)            Bump to next substring byte
         LA    R2,1(R2)            Count digits
         ST    R2,SRCHENDL         Store Length
         C     R2,=F'3'            More than 3 digits?
         BH    ERR4035             YES, error.  too many digits
         BCT   R6,LOADEND          Keep loading substring...
         B     POSDONE             Loop expired, done w parm scan
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Set Search Begin position                            */
*     * /* - R0                       Working Register          */
*     * /--------------------------------------------------------/
POSDONE  EQU   *
         L     R0,SRCHBEGL
         C     R0,=F'0'            BEG position specified?
         BE    DFLTBEG             NO, use default 1
         C     R0,=F'1'            BEG pos len = 1?
         BE    SRCHBPK1            YES.
         C     R0,=F'2'            BEG pos len = 2?
         BE    SRCHBPK2            YES.
SRCHBPK3 EQU   *                   Must be pos len = 3...
         PACK  DW,SRCHBEG(3)
         B     SRCHBPKX
SRCHBPK2 EQU   *
         PACK  DW,SRCHBEG(2)
         B     SRCHBPKX
SRCHBPK1 EQU   *
         PACK  DW,SRCHBEG(1)
         B     SRCHBPKX
DFLTBEG  EQU   *
         PACK  DW,=C'1'
         B     SRCHBPKX
SRCHBPKX EQU   *
         CVB   R0,DW               Convert to binary
         ST    R0,SRCHS            Store
         CLC   =C'OVERLAY ',FUNCT  Do not override Start for OVERLAY    LB1105c
         BE    SRCHBXT                                                  LB1105c
         CLC   =C'REPLACE ',FUNCT  Do not override Start for REPLACE    LB1105d
         BE    SRCHBXT                                                  LB1105d
*     * /--------------------------------------------------------/      LB1105z
*     * /* Override start pos to 1 ?                            */      LB1105z
*     * /--------------------------------------------------------/      LB1105z
SRCHBOK  EQU   *
         CR    R0,R7               End   POS > VAR1 Len?                LB1105f
*        C     R0,VALL1            Start POS > VAR1 Len?                LB1105f
         BNH   SRCHBXT             NO, within reason...
         LA    R0,1                YES, override start POS
         ST    R0,SRCHS            Store
SRCHBXT  EQU   *
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Set Search End   position                            */
*     * /* - R0                       Working Register          */
*     * /--------------------------------------------------------/
         L     R0,SRCHENDL
         C     R0,=F'0'            END position specified?              LB1105z
         BE    DFLTEND             NO, use default 999
         C     R0,=F'1'            END pos len = 1?                     LB1105z
         BE    SRCHEPK1            YES.
         C     R0,=F'2'            END pos len = 2?                     LB1105z
         BE    SRCHEPK2            YES.
SRCHEPK3 EQU   *                   NO, Must be pos len = 3...
         PACK  DW,SRCHEND(3)
         B     SRCHEPKX
SRCHEPK2 EQU   *
         PACK  DW,SRCHEND(2)
         B     SRCHEPKX
SRCHEPK1 EQU   *
         PACK  DW,SRCHEND(1)
         B     SRCHEPKX
DFLTEND  EQU   *
**       PACK  DW,=C'999'
**       B     SRCHEPKX
         ST    R7,SRCHE            End   POS = VAR1 Len                 LB1105f
         B     SRCHEXT                                                  LB1105f
SRCHEPKX EQU   *
         CVB   R0,DW               Convert to binary
         ST    R0,SRCHE            Store
         CLC   =C'OVERLAY ',FUNCT  Do not override End   for OVERLAY    LB1105c
         BE    SRCHEXT                                                  LB1105c
         CLC   =C'REPLACE ',FUNCT  Do not override End   for REPLACE    LB1105d
         BE    SRCHEXT                                                  LB1105d
*     * /--------------------------------------------------------/      LB1105z
*     * /* Override end   pos to VAR1 Len ?                     */      LB1105z
*     * /--------------------------------------------------------/      LB1105z
SRCHEOK  EQU   *
         CR    R0,R7               End   POS > VAR1 Len?                LB1105f
*        C     R0,VALL1            End   POS > VAR1 Len?                LB1105f
         BNH   SRCHEXT             NO, within reason...
         MVC   SRCHE,VALL1         YES, override end   POS
SRCHEXT  EQU   *
S#SRCHPX EQU   *
         BR    R1                  Return to caller (R1)
*
                                                                        LB1105g
         EJECT                                                          LB1105g
*     * /********************************************************/      LB1105g
*     * /* Subroutine - Get USERID                       (R14)  */      LB1105g
*     * /--------------------------------------------------------/      LB1105g
*     * /********************************************************/      LB1105g
*     * /* Input:                                               */      LB1105g
*     * /* DPFX and DPFXL are returned                          */      LB1105g
*     * /* - R2, R3                   Working Registers         */      LB1105g
*     * /********************************************************/      LB1105g
GETUSRID EQU   *                                                        LB1105g
*     * /*------------------------------------------------------*/      LB1105g
*     * /* Establish USERID from TSO PREFIX or TSO USERID       */      LB1105g
*     * /*------------------------------------------------------*/      LB1105g
         MVI   DPFXL,X'00'         Initialize Dataset Name Prefix       LB1105g
         MVI   DPFX,C' '               length and                       LB1105g
         MVC   DPFX+1(L'DPFX),DPFX       variable                       LB1105g
GDSNPRF  EQU   *                   Get USERID from UPT                  LB1105g
         L     R3,MYUPT            Point to UPT                         LB1105g
         USING UPT,R3              Tell Assebler, UPT                   LB1105g
         SR    R2,R2               Clear R2 for IC                      LB1105g
         IC    R2,UPTPREFL         PREFIX Length                        LB1105g
         LTR   R2,R2               Length = 0?                          LB1105g
         BZ    GDSNUSR             Yes, try USERID                      LB1105g
         MVC   DPFXL,UPTPREFL      PREFIX Length                        LB1105g
         MVC   DPFX(L'UPTPREFX),UPTPREFX  PREFIX                        LB1105g
         B     GDSNPRFX            Done, continue...                    LB1105g
GDSNUSR  EQU   *                   Get USERID from PSCB                 LB1105g
         L     R3,MYPSCB           Point to PSCB                        LB1105g
         USING PSCB,R3             Tell Assebler, PSCB                  LB1105g
         SR    R2,R2               Clear R2 for IC                      LB1105g
         IC    R2,PSCBUSRL         USERID Length                        LB1105g
         LTR   R2,R2               Length = 0?                          LB1105g
         BZ    NOUSRIDP            Yes, no DSN Prefix                   LB1105g
         MVC   DPFXL,PSCBUSRL      USERID Length                        LB1105g
         MVC   DPFX(L'PSCBUSER),PSCBUSER  USERID                        LB1105g
NOUSRIDP EQU   *                   UserID returned as blank!            LB1105g
GDSNPRFX EQU   *                   Done...                              LB1105g
         DROP  R3                                                       LB1105g
         BR    R14                 Return to caller (R14)               LB1105g
*                                                                       LB1105g
         TITLE 'CUTIL00:  - - - -   Literal Pool                      '
*     * /********************************************************/
*     * /* Literal Pool                                         */
*     * /********************************************************/
         LTORG
*     *
         TITLE 'CUTIL00:  - - - -   Constants                         '
*
*     * /********************************************************/
*     * /* Entry codes for IKJCT441                             */
*     * /********************************************************/
ECNOIMP  DC    A(TSVNOIMP)         ENTRY CODE FOR RETRIEVE/NOCREATE
ECUPDT   DC    A(TSVEUPDT)         ENTRY CODE FOR UPDATE/CREATE
*     *
         EJECT
*     * /********************************************************/
*     * /* Messages - Internal                                  */
*     * /********************************************************/
MSGDONE  DC    C'Successful Request '
MSGDONEL EQU   *-MSGDONE
MSG4060  DC    C'Call error w CUTILTBL'                                 LB1100g
MSG4060L EQU   *-MSG4060                                                LB1100g
MSG4097  DC    C'Cannot load CUTILTBL'                                  LB1105b
MSG4097L EQU   *-MSG4097                                                LB1105b
*
         TITLE 'CUTIL00:  - - - -   EX Instructions                   '
*     * /********************************************************/
*     * /* Instructions used for EXecutes                       */
*     * /********************************************************/
MVCVAR1# MVC   VAL1(0),0(R8)       Save VAR1  value
MVCVAR1L EQU   *-MVCVAR1#
*
MVCVARW# MVC   VARWORK(0),0(R8)    Save VARWORK  value
MVCVARWL EQU   *-MVCVARW#
*
UPPERCS# OC    0(0,R2),BLANKS      EBCDIC Lower-Upper Case Translate
UPPERCSL EQU   *-UPPERCS#
*
MVCR8$6# MVC   0(0,R6),0(R8)       MVC R8 to R6
MVCR8$6L EQU   *-MVCR8$6#
*
*EXTRLC#  TR    0(0,R2),TRTBL       EBCDIC Upper-Lower Case Translate   LB1100h
EXTRLC#  TR    0(0,R2),0(R3)       EBCDIC Upper-Lower Case Translate    LB1100h
EXTRLCL  EQU   *-EXTRLC#
*
SRCHCLC# CLC   0(0,R2),SRCHSTR
SRCHCLCL EQU   *-SRCHCLC#
*
*CHKDDN#  CLC   0(0,R8),TIOEDDNM    DDNAME = TIOT entry?
*CHKDDNL  EQU   *-CHKDDN#
*
*REVRS#   TR    VARWORK(0),TREVRS   Reverse letters and numbers         LB1100i
REVRS#   TR    VARWORK(0),0(R3)     Reverse letters and numbers         LB1100i
REVRSL   EQU   *-REVRS#
*
         TITLE 'CUTIL00:  - - - -   EQUATES                           '
*     * /********************************************************/
*     * /* IKJCT441 Entry Code Values                           */
*     * /********************************************************/
TSVERETR EQU   1                   Retrieve/Create Request
TSVEUPDT EQU   2                   Update/Create Request
TSVELOC  EQU   3                   Locate ALL Request (uses TOKEN)
TSVNOIMP EQU   18                  Retrieve/Nocreate Request
TSVRNOM  EQU   20                  No more variables to return (locate)
*     * /********************************************************/
*     * /* Register Equates                                     */
*     * /********************************************************/
         PRINT GEN
         YREGS
         PRINT NOGEN
*     *
         TITLE 'CUTIL00:  - - - -   System DSECTs                     '
WDT      DSECT                     Weekday Parameter Table              LB1100f
WDTNUM   DS    CL1                 Day Number                           LB1100f
WDTNAME  DS    CL9                 Day Name                             LB1100f
WDTNAMEL EQU   *-WDTNAME           Length of Day Name                   LB1100f
WDTLE    EQU   *-WDT               Length of Entry                      LB1100f
*                                                                       LB1100f
MPT      DSECT                     Month Parameter Table
MPTMM    DS    CL2                 Month
MPTMMAXD DS    CL2                 Month Max Days
MPTDAT@1 DS    CL3                 Days before 1st of Month
MPTDWTRM DS    CL1                 DOW Term (0,3,2,5,0,3,5,1,4,6,2,4)
MPTMNB   EQU   *                   Start of Month Name
MPTMNAME DS    CL9                 Month Name
MPTMNL   EQU   *-MPTMNAME          Length of Month Name
MPTLE    EQU   *-MPT               Length of Entry
         EJECT
         IHAPSA                    Prefixed Save Area
         EJECT
         CVT   DSECT=YES,LIST=YES  Communication Vector Table
         EJECT                                                          LB1105g
         IKJUPT                    User Profile Table                   LB1105g
         EJECT                                                          LB1105g
         IKJPSCB                   Protected Step Control Block         LB1105g
         EJECT
         IKJCPPL                   Command Processor Parameter List
         EJECT
         IKJTCB DSECT=YES,LIST=YES Task Control Block
         EJECT
TIOT     DSECT
         IEFTIOT1                  Task IO Table
         EJECT
JFCB     DSECT
         IEFJFCBN  LIST=YES        Job File Control BLock
         EJECT
         IHAASCB                   Address Space Control Block
         TITLE 'CUTIL00:  - - - -   Working Storage                   '
*     * /********************************************************/
*     * /* Working Storage                                      */
*     * /********************************************************/
WSDSECT  DSECT
WSAREA   EQU   *                   Working Storage START
*     *
SAVEAREA DS    18F                 Savearea
**       DS    F              +00  Unused
**       DS    F              +04  Backward Savearea pointer
**       DS    F              +08  Forward  Savearea pointer
**       DS    F              +12  R14 contents
**       DS    F              +16  R15 contents
**       DS    F              +20  R0  contents
**       DS    F              +24  R1  contents
**       DS    F              +28  R2  contents
**       DS    F              +32  R3  contents
**       DS    F              +36  R4  contents
**       DS    F              +40  R5  contents
**       DS    F              +44  R6  contents
**       DS    F              +48  R7  contents
**       DS    F              +52  R8  contents
**       DS    F              +56  R9  contents
**       DS    F              +60  R10 contents
**       DS    F              +64  R11 contents
**       DS    F              +68  R12 contents
SAVER15  DS    F                   Save R15 for exit
RC#441   DS    F                   Save R15 for link IKJCT441
MYR1     DS    F                   My PARM address on entry
MYR4     DS    F                   My Starting Parms address
MYR6     DS    F                   My Starting Parms length
R7HOLD   DS    F                   R7 Hold area
R8HOLD   DS    F                   R8 Hold area
CUTILTBL DS    F                   CUTILTBL Entry Point                 LB1105b
*
NEWSEG   DS    CL1                 New dataset segment switch
KSEGCHAR DS    PL2                 Segment character count
KSEGCNT  DS    PL2                 Segment count
*
SAVER1   DS    F                   Save R1
SAVER5   DS    F                   Save R5
SAVER7R8 DS    2F                  Save R7,R8 for PUTLINE Subroutine
MYUPT    DS    F                   User Profile Table Address
MYECT    DS    F                   Env Control Table Address
MYECB    DS    F                   Env Control Block for PUTLINE
MYPSCB   DS    F                   Protected Step Control Block         LB1105g
IOPLADS  DS    4F                  I/O parameter block
PUTBLK   PUTLINE MF=L              List Form of PUTLINE
OUTTXT   DS    H                   Len of line data (incl header)
         DS    H                   reserved
INFOLINE DS    CL300               Output data area
OUTTXTL  EQU   *-OUTTXT
NNN      DS    PL2                 Variable counter
TSO      EQU   X'80'
*
         EJECT
WORKAREA DS    F                   Workarea
UNPKAREA DS    CL16                Workarea for unpacking data
TOKEN    DS    F                   TOKEN (UNUSED HERE) Init to ZERO
*   ----                            AND NEVER CHANGED BY THE CALLER
*        DS    0D
SRCHFND  DS    F                   Search String Found Position
SRCHBEGL DS    F                   Search String BEG Pos Length
SRCHENDL DS    F                   Search String END Pos Length
SRCHSTRL DS    F                   Search String Length
SRCHST2L DS    F                   Search String Length 2
SRCHS    DS    F                   Search Starting Position
SRCHE    DS    F                   Search Ending   Position
WORDL    DS    F                   Number of WORDS
MAXWORDS DS    F                   Max Words
DW       DS    D                   Workarea DW
FW       DS    F                   Workarea FW
DC#TRM   DS    PL1                 DOW Term
DC#JJJ   DS    PL2                 Julian Day
DC#YR4   DS    PL4                 CCYY / 4
DC#YR100 DS    PL4                 CCYY / 100
DC#YR400 DS    PL4                 CCYY / 400
DC#DOW   DS    PL4                 Day of Week
DC#DD    DS    PL2                 DD
DC#LEAP  DS    PL1                 Leap Year (0-No, 1-Yes)
DC#YDAYS DS    PL2                 Days in Year (365 or 366)
DC#MDAYS DS    PL2                 Days in Month (1-31)                 LB1100b
DCCCCYY  DS    CL5                 0CCYY character area
DCDDD    DS    CL3                 0DD   character area
DCMMM    DS    CL3                 0MM   character area
DCJJJ    DS    CL3                 JJJ   character area
DCCDOW   DS    CL1                 Day of Week character area
DCCWDAY  DS    CL9                 Weekday character area
PKJJJ    DS    PL3   060           Workarea pack JJJ
PKACCUM  DS    PL3                 Workarea
MCALRMCY DS    CL6                 MCAL request date MMCCYY             LB1100e
MCAL2MCY DS    CL6                 MCAL Today MMCCYY                    LB1100e
MCAL2JJJ DS    CL3                 MCAL Today JJJ                       LB1100e
MCAL2DD  DS    CL2                 MCAL Today DD                        LB1100e
DPFXL    DS    CL1                 DSN PREFIX   Length                  LB1105g
DPFX     DS    CL08                DSN PREFIX                           LB1105g
*     *
         EJECT                                                          LB1100g
*     * /********************************************************/      LB1100g
*     * /* Generic  Parm Address List                           */      LB1100g
*     * /********************************************************/      LB1100g
PALLST   DS    0F                                                       LB1100g
PALP1    DS    A                   Parm 1                               LB1100g
PALP2    DS    A                   Parm 2                               LB1100g
PALP3    DS    A                   Parm 3                               LB1100g
PALP4    DS    A                   Parm 4                               LB1100g
PALP5    DS    A                   Parm 5                               LB1100g
PALP6    DS    A                   Parm 6                               LB1100g
         EJECT
*     * /********************************************************/
*     * /* IKJCT441 Parm Address List                           */
*     * /********************************************************/
IKJCT441 DS    F                   Entry point for IKJCT441
CT441LST DS    0F
CT441P1  DS    A                   Entry Code
CT441P2  DS    A                   VAR NAME POINTER
CT441P3  DS    A                   VAR NAME LENGTH
CT441P4  DS    A                   VAR VALUE POINTER
CT441P5  DS    A                   VAR VALUE LENGTH
CT441P6  DS    A                   Token
*     *
*     * /********************************************************/
*     * /* IKJCT441 Parm List variable - VAR1                   */
*     * /********************************************************/
NAML1    DC    A(L'NAM1)           VAR NAME LENGTH
NAMP1    DC    A(NAM1)             VAR NAME POINTER
VALL1    DC    A(L'VAL1)           VAR VALUE LENGTH
VALP1    DC    A(VAL1)             VAR VALUE POINTER
*     *
*     * /********************************************************/
*     * /* IKJCT441 Parm List variable - VAR2                   */
*     * /********************************************************/
NAML2    DC    A(L'NAM2)           VAR NAME LENGTH
NAMP2    DC    A(NAM2)             VAR NAME POINTER
VALL2    DC    A(L'VAL2)           VAR VALUE LENGTH
VALP2    DC    A(VAL2)             VAR VALUE POINTER
*     *
*     * /********************************************************/
*     * /* IKJCT441 Parm List variable - VAR3                   */
*     * /********************************************************/
NAM3     DS    CL9                 VAR NAME
NAML3    DS    F                   VAR NAME LENGTH
NAMP3    DS    F                   VAR NAME POINTER
VAL3     DS    CL256               VAR VALUE
VALL3    DS    F                   VAR VALUE LENGTH
VALP3    DS    F                   VAR VALUE POINTER
*     *
*     * /********************************************************/
*     * /* IKJCT441 Parm List variable - VAR4                   */
*     * /********************************************************/
NAM4     DS    CL11                VAR NAME
NAML4    DS    F                   VAR NAME LENGTH
NAMP4    DS    F                   VAR NAME POINTER
VAL4     DS    CL256               VAR VALUE
VALL4    DS    F                   VAR VALUE LENGTH
VALP4    DS    F                   VAR VALUE POINTER
*     *
         EJECT
*     * /********************************************************/
*     * /* Variable names and values area                       */
*     * /********************************************************/
VARBLK   EQU   *
NAM1     DS    CL8                 VAR NAME 1
NAM2     DS    CL8                 VAR NAME 2
VAL1Q    DS    CL1                 VAR VALUE 1 QUOTE
VAL1     DS    CL256               VAR VALUE 1
         DS    CL1                 VAR VALUE 1 QUOTE
VAL2Q    DS    CL1                 VAR VALUE 2 QUOTE
VAL2     DS    CL256               VAR VALUE 2
         DS    CL1                 VAR VALUE 2 QUOTE
VARWORK  DS    CL256               VAR WORK
SRCHSTR  DS    CL256               Search String
SRCHST2  DS    CL256               Search String 2
SRCHBEG  DS    CL3                 Search String BEG Pos
SRCHEND  DS    CL3                 Search String END Pos
DELIM    DS    CL1                 Delimiter 1-byte
ERRMSG   DS    CL30                Error Message Text
VARSET1  DS    CL256
VARBLKL  EQU   *-VARBLK
*     *
*     * /********************************************************/
*     * /* Block to store each parsed WORD                      */
*     * /* - All words are of equal length                      */
*     * /********************************************************/
WORDBLK  EQU   *
FUNCT    DS    CL8                 Function
WORDLEN  EQU   (*-FUNCT)           WORD Length, words are equal len
VAR1     DS    CL8                 Symbolic Variable name 1
VAR2     DS    CL8                 Symbolic Variable name 2
SQUOTE   DS    CL8                 Quotes surrounding variable value
WORDBLKK EQU   *-WORDBLK
MAXWD    EQU   WORDBLKK/WORDLEN
         EJECT
*     *
*     * /********************************************************/
*     * /* Block to store each parsed WORD length               */
*     * /* - Must be in same order as WORDBLK                   */
*     * /********************************************************/
WORDBLKL DS    0F
FUNCTL   DS    F                   Length of FUNCT
VAR1L    DS    F                   Length of VAR1
VAR2L    DS    F                   Length of VAR2
SQOUTEL  DS    F                   Length of QOUTE keyword
VARSET1L DS    F                   Length of VARSET
*
CHKV1NL  DS    C                   Test VAR1 for NULL flag
CHKV2NL  DS    C                   Test VAR2 for NULL flag
DDD      DS    CL120
BLANKS   DS    CL256               Blanks used for Case Traslation
*     *                                                                 LB1100e
*     * /********************************************************/      LB1100e
*     * /* MCALA ISPF Attributes                                */      LB1100e
*     * /********************************************************/      LB1100e
MCALA1$  DS    C                   Attribute Month Name and Year        LB1100e
MCALA2$  DS    C                   Attribute JJJ                        LB1100e
MCALA3$  DS    C                   Attribute Weekend Day Names          LB1100e
MCALA4$  DS    C                   Attribute Calendar Dates             LB1100e
MCALA5$  DS    C                   Attribute Calendar Current Date      LB1100e
         EJECT                                                          LB1100e
*     *
*     * /********************************************************/
*     * /* EX instructions                                      */
*     * /********************************************************/
MVCVARWK MVC   VARWORK(0),0(R8)    Save VARWORK  value
*
MVCVAR1  MVC   VAL1(0),0(R8)       Save VAR1  value
*
UPPERCS  OC    0(0,R2),BLANKS      EBCDIC Lower-Upper Case Translate
*UPPERCS  OC    0(0,R2),=256X'40'   EBCDIC Lower-Upper Case Translate
*
MVCR8$6  MVC   0(0,R6),0(R8)       MVC R8 to R6
*
*EXTRLC   TR    0(0,R2),TRTBL       EBCDIC Upper-Lower Case Translate   LB1100h
EXTRLC   TR    0(0,R2),0(R3)       EBCDIC Upper-Lower Case Translate    LB1100h
*
SRCHCLC  CLC   0(0,R2),SRCHSTR
*
*CHKDDN   CLC   0(0,R8),TIOEDDNM    DDNAME = TIOT entry?
*
*REVRS    TR    VARWORK(0),TREVRS   Reverse letters and numbers         LB1100i
REVRS    TR    VARWORK(0),0(R3)    Reverse letters and numbers          LB1100i
*     *
*     * /********************************************************/
*     * /* End of Working Storage                               */
*     * /********************************************************/
         DS    0D
WSAREAE  EQU   *                   Working Storage END
WSAREAL  EQU   WSAREAE-WSAREA      Working Storage Area Length
         EJECT
*
         END
@@
//LKED.SYSLMOD DD  DISP=SHR,
//         DSN=SYS2.CMDLIB(CUTIL00)                       <--TARGET
//*
//* -------------------------------------------------------*
//* *  Assemble Link-Edit CUTILTBL to SYS2.CMDLIB          *
//* -------------------------------------------------------*
//CUTILTBL EXEC  ASML,
//         PARM.LKED='MAP,LIST,LET,RENT,XREF,REUS,REFR'
//ASM.SYSIN DD DATA,DLM=@@
         TITLE 'CUTILTBL - Tables Processor for CUTIL00               ' LB1105z
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  ==================================================================
*
*    CCCC  UU   UU TTTTTTT IIIIIII LL      TTTTTTT BBBBBB  LL
*   CC  CC UU   UU   TT      II    LL        TT    BB   BB LL
*  CC      UU   UU   TT      II    LL        TT    BB   B  LL
*  CC      UU   UU   TT      II    LL        TT    BBBBB   LL
*  CC      UU   UU   TT      II    LL        TT    BB   B  LL
*   CC  CC UU   UU   TT      II    LL        TT    BB   BB LL
*    CCCC    UUU     TT    IIIIIII LLLLLLL   TT    BBBBBB  LLLLLLL
*
*  ==================================================================
*  1234567-1234567-1234567-1234567-1234567-1234567-1234567-1234567
*
*  Program: CUTILTBL
*
*  Author: Larry Belmontes Jr.
*          https://ShareABitOfIT.net/CUTIL00-for-mvs-3-8j/
*          Copyright (C) 2020-2021 Larry Belmontes, Jr.
*
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
*     In addition, the author requests submissions regarding any
*  code modifications / enhancements and/or associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  benefiting the MVS 3.8J hobbyist public domain community.
*
*
         EJECT
*  Overview:
*  ================================================================
*
*     This program is the central table processor for CUTIL00 using     LB1105z
*  the following parameter address list:                                LB1105z
*
*       o   Input     CL4    Request Type
*       o   I/O       F      Parm 1 based on request type
*       o   I/O       F      Parm 2 based on request type
*       o   I/O       F      Parm 3 based on request type
*       o   I/O       F      Parm 4 based on request type
*       o   I/O       F      Parm 5 based on request type
*       o   I/O       F      Parm 6 based on request type
*
*     L    R1,parm addr list
*     CALL CUTILTBL
*     LTR  R15,R15
*     .
*     .
*     .
*
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
*  |  00  |  Successful Request                                      |
*  +------+----------------------------------------------------------+
*  |  04  |  Table entry not found (for table search requests only)  |
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
*  10/05/2021 1.1.05   Larry Belmontes Jr.                              LB1105
*                      - Added messages 4071-4075                       LB1105a
*                      - Misc updates to program documentation          LB1105z
*                                                                       LB1105
*  04/10/2021 1.1.00   Larry Belmontes Jr.
*                      - Initial version released to MVS 3.8J
*                        hobbyist public domain
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
CUTILTBL CSECT
         USING CUTILTBL,R10        my BASE REGISTER(S)
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
PGMID    DC    CL8'CUTILTBL'       My Program STAMP
         DC    CL8'MVS3.8J '       OS
         DC    CL8'V1.1.00 '       .Version
         DC    CL8'04102021'       ..date  MMDDCCYY
         DC    CL1' '
PGMDS    DC    CL8'&SYSDATE'       Assemble Date
         DC    CL3' - '            .and
PGMTS    DC    CL8'&SYSTIME'       ..Time
         DC    C'Copyright (C) 2021'
         DC    C'Larry Belmontes, Jr. '
         DC    C'https://ShareABitofIT.net/CUTIL00-for-MVS-3-8J'        LB1105z
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
PARMS    EQU   *
         LTR   R1,R1               Do I have a PARM?
         BNZ   PARMIN              YES, check parms
         MVC   RCEXIT,=F'12'       NO, return with RC=12 (no parm)
         B     MYEXIT              Branch to MYEXIT
*
*     * /********************************************************/
*     * /* Process Request Type                                 */
*     * /* - R1 Starting parms address                          */
*     * /********************************************************/
PARMIN   EQU   *
         MVC   MVCR8$6(MVCR8$6L),MVCR8$6#     Init MVC for EX
         MVC   RCEXIT,=F'0'        Initialize RCEXIT
*
         CLC   =C'1DOW',0(R1)      DOW Table Request
         BE    REQDOW
         CLC   =C'2MM ',0(R1)      MM Table Reuqest
         BE    REQMM
         CLC   =C'2J2M',0(R1)      JJJ to MM Table Request
         BE    REQJ2M
         CLC   =C'3ULC',0(R1)      Upper-Lower Case Table Reqest
         BE    REQULC
         CLC   =C'3RVR',0(R1)      Reverse Table Request
         BE    REQRVR
         CLC   =C'9ERR',0(R1)      Error Message Request
         BE    REQERR
*
         MVC   RCEXIT,=F'16'       Invalid Request Type
         B     MYEXIT              Branch to MYEXIT
*
PARMSXT  EQU   *                   We have a parm...
*
         EJECT
*     * /********************************************************/
*     * /* Search for Day of Week                               */
*     * /* - R2 Number of message table entries                 */
*     * /* - R3 Address of message table                        */
*     * /* - R5 Address of Parm 2                               */
*     * /*                                                      */
*     * /* - Parm 1: CL4 Request '1DOW'                         */
*     * /* - Parm 2: A   Addr of DOW  e.g DCCDOW CL1            */
*     * /* - Parm 3: A   Addr of DOWTBL entry (on return)       */
*     * /********************************************************/
REQDOW   EQU   *
         LA    R2,DOWTBL#          R2=Number of DOWTBL entries
         LA    R3,DOWTBL           R3=Starting address of DOWTBL
         L     R5,4(R1)            R5=Starting address of DCCDOW
CHKDOW   EQU   *
         CLC   0(1,R3),0(R5)       Match DOW ?
         BE    FNDDOW              Yes, found it...
         LA    R3,DOWTBLL(R3)      No, look at next entry
         BCT   R2,CHKDOW           Check again
         MVC   RCEXIT,=F'04'       DOW not found, invalid DOW
         B     MYEXIT              Branch to MYEXIT
FNDDOW   EQU   *
         ST    R3,8(R1)            DOW Row starting addr
         B     MYEXIT              Done...
*
         EJECT
*     * /********************************************************/
*     * /* Search for Month Table Entry using MM                */
*     * /* - R2 Number of message table entries                 */
*     * /* - R3 Address of message table                        */
*     * /* - R5 Address of Parm 2                               */
*     * /*                                                      */
*     * /* - Parm 1: CL4 Request '2MM '                         */
*     * /* - Parm 2: A   Addr of MM   e.g DCMMM+1 CL2           */
*     * /* - Parm 3: A   Addr of MMTBL  entry (on return)       */
*     * /********************************************************/
REQMM    EQU   *
         LA    R2,MMTBL#           R2=Number of MMTBL entries
         LA    R3,MMTBL            R3=Starting address of MMTBL
         L     R5,4(R1)            R5=Starting address of MM
CHKMM    EQU   *
         CLC   0(2,R3),0(R5)       Match MM ?
         BE    FNDMM               Yes, found it..
         LA    R3,MMTBLL(R3)       No, look at next entry
         BCT   R2,CHKMM            Check again
         MVC   RCEXIT,=F'04'       MM not found, invalid MM
         B     MYEXIT              Branch to MYEXIT
FNDMM    EQU   *
         ST    R3,8(R1)            MMTBL Row starting addr
         B     MYEXIT              Done...
*
         EJECT
*     * /********************************************************/
*     * /* Search for Month Table Entry using JJJ               */
*     * /* - R4 Address of message table                        */
*     * /* - R3 Number of message table entries                 */
*     * /* - R5 Length of message table entry                   */
*     * /* - R6 Address of Parm 2                               */
*     * /* - R7 Address of Parm 3                               */
*     * /* - R8 Address of Parm 4                               */
*     * /*                                                      */
*     * /* - Parm 1: CL4 Request '2J2M'                         */
*     * /* - Parm 2: A   Addr of JJJ  e.g PKJJJ   PL3           */
*     * /* - Parm 3: A   Addr of packed leap year indicator     */
*     * /*                            e.g DC#LEAP PL1           */
*     * /* - Parm 4: A   Addr of full-work field                */
*     * /*                            e.g FW      F             */
*     * /* - Parm 5: A   Addr of MMTBL  entry (on return)       */
*     * /********************************************************/
REQJ2M   EQU   *

         LA    R4,MMTBLE           R4=End Address of MonthTable
         LA    R5,MMTBLL           R3=Length of table entry
         SR    R4,R5               R4=Address of Last MonthTable Entry
         LA    R3,MMTBL#           R3=12 Months
         L     R6,4(R1)            R6=Starting address of PKJJJ
         L     R7,8(R1)            R7=Starting address of DC#LEAP
         L     R8,12(R1)           R8=Starting address of FW
CHKMMJJJ EQU   *
*        PACK  FW(2),4(3,R4)       Pack start month days from table
         PACK  0(2,R8),4(3,R4)     Pack start month days from table
         CLC   0(2,R4),=C'01'      JAN?
         BE    NOADJLY             Yes, bypass adjust for leap year
*        AP    FW(2),0(1,R7)       No, adjust for leap year when MM >1
         AP    0(2,R8),0(1,R7)     No, adjust for leap year when MM >1
NOADJLY  EQU   *
*        CP    FW(2),0(3,R6)       MM JJ < JJJ
         CP    0(2,R8),0(3,R6)     MM JJ < JJJ
         BL    FNDMMJJJ            Yes, found month table entry MM
         SR    R4,R5               No, Point to prev MonthTable Entry
         BCT   R3,CHKMMJJJ         Check again...
         MVC   RCEXIT,=F'04'       JJJ not found, invalid JJJ
         B     MYEXIT              Branch to MYEXIT
FNDMMJJJ EQU   *
         ST    R4,16(R1)           MMTBL Row starting addr
         B     MYEXIT              Done...
*
         EJECT
*     * /********************************************************/
*     * /* Return TRTBL starting address                        */
*     * /* - R3 Address of TRTBL table                          */
*     * /*                                                      */
*     * /* - Parm 1: CL4 Request '2ULC'                         */
*     * /* - Parm 2: A   Addr of TRTBL table  (on return)       */
*     * /********************************************************/
REQULC   EQU   *
         LA    R3,TRTBL            R3=Starting address of TRTBL
         ST    R3,4(R1)            TRTBL Row starting addr
         B     MYEXIT              Done...
*
         EJECT
*     * /********************************************************/
*     * /* Return TREVRS starting address                       */
*     * /* - R3 Address of TREVRS table                         */
*     * /*                                                      */
*     * /* - Parm 1: CL4 Request '3RVR'                         */
*     * /* - Parm 2: A   Addr of TREVRS table (on return)       */
*     * /********************************************************/
REQRVR   EQU   *
         LA    R3,TREVRS           R3=Starting address of TREVRS
         ST    R3,4(R1)            TREVRS Row starting addr
         B     MYEXIT              Done...
*
         EJECT
*     * /********************************************************/
*     * /* Process ERRMSG                                       */
*     * /* - R2 Number of message table entries                 */
*     * /* - R3 Address of message table                        */
*     * /* - R8 Address of Parm 2                               */
*     * /*                                                      */
*     * /* - Parm 1: CL4 Request '9ERR'                         */
*     * /* - Parm 2: A   Addr of full-word return code          */
*     * /*                       e.g RC       A                 */
*     * /* - Parm 3: A   Addr of error message text area        */
*     * /*                       e.g ERRMSG   CL30              */
*     * /********************************************************/
REQERR   EQU   *
         LA    R2,MSGTBL#          R2=Number of MSGTBL entries
         LA    R3,MSGTBL           R3=Starting address of MSGTBL
         L     R6,4(R1)            R5=Error Number
         ST    R6,FW
         L     R6,8(R1)            R5=Starting address of ERRMSG
CHKERR   EQU   *
         CLC   0(4,R3),FW          Match ERRNO ?
         BE    FNDERR              Yes, found it...
         LA    R3,MSGTBLL(R3)      No, look at next entry
         BCT   R2,CHKERR           Check again
NFNDERR  EQU   *
         LA    R8,MSGNFND          R6=Starting address of MSGxxxx
         LA    R7,L'MSGNFND        R7=Length of MSGxxxx
         B     MOVERR
FNDERR   EQU   *
         L     R8,4(R3)            R6=Starting address of MSGxxxx
         L     R7,8(R3)            R7=Length of MSGxxxx
MOVERR   EQU   *
         BCTR  R7,0                Adjust for EX MVC
         EX    R7,MVCR8$6          Move R8 to R6 for len of R7
         MVC   RCEXIT,FW           Return Err no in R15
         B     MYEXIT              Done...
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
         TITLE 'CUTILTBL - Constants                                  '
*
MVCR8$6# MVC   0(0,R6),0(R8)       MVC R8 to R6
MVCR8$6L EQU   *-MVCR8$6#
*
         TITLE 'CUTILTBL - Literal Pool                               '
         LTORG
*
         TITLE 'CUTILTBL - Day of Week Table                          '
*     * /********************************************************/
*     * /* Day of Week Table                                    */
*     * /********************************************************/
DOWTBL   EQU   *
*                +----------------- 1 C    Weekday Number
*                |+---------------- 9 C    Weekday name
*                ||
         DC    C'0Sunday   '
DOWTBLL  EQU   *-DOWTBL                    Length of table entry
         DC    C'1Monday   '
         DC    C'2Tuesday  '
         DC    C'3Wednesday'
         DC    C'4Thursday '
         DC    C'5Friday   '
         DC    C'6Saturday '
DOWTBL#  EQU   (*-DOWTBL)/DOWTBLL          Number of table entries
*
         TITLE 'CUTILTBL - Month Parameter Table                      '
*     * /********************************************************/
*     * /* Month Parameter Table                                */
*     * /********************************************************/
MMTBL    EQU   *
*                +----------------- 2 C    Month Number
*                | +--------------- 2 C    Days in month
*                | | +------------- 3 C    Days at start of month
*                | | |  +---------- 1 C    Term 0,3,2,5,0,3,5,1,4,6,2,4
*                | | |  |+--------- 9 C    Monthname
*                | | |  ||
         DC    C'01310000January  '
MMTBLL   EQU   *-MMTBL                     Length of table entry
         DC    C'02280313February '
         DC    C'03310592March    '
         DC    C'04300905April    '
         DC    C'05311200May      '
         DC    C'06301513June     '
         DC    C'07311815July     '
         DC    C'08312121August   '
         DC    C'09302434September'
         DC    C'10312736October  '
         DC    C'11303042November '
         DC    C'12313344December '
MMTBLE   EQU   *                           End of table
MMTBL#   EQU   (*-MMTBL)/MMTBLL            Number of table entries
         TITLE 'CUTILTBL - Upper-Lower Case Table                     '
*     * /********************************************************/
*     * /* Upper-Lower Case Table                               */
*     * /********************************************************/
*TRTAB    DC    256AL1(*-TRTAB)
*         ORG   TRTAB+C'A'
*         DC    C'abcdefghi'
*         ORG   TRTAB+C'J'
*         DC    C'jklmnopqr'
*         ORG   TRTAB+C'S'
*         DC    C'stuvwxyz'
*         ORG   ,
*     * /********************************************************/
*     * /* Upper-Lower Case Table                               */
*     * /********************************************************/
TRTBL    DC    0XL256'0'
         DC    (C'A')AL1(*-TRTBL),9AL1(*-TRTBL-X'40')
         DC    (C'J'-(*-TRTBL))AL1(*-TRTBL),9AL1(*-TRTBL-X'40')
         DC    (C'S'-(*-TRTBL))AL1(*-TRTBL),8AL1(*-TRTBL-X'40')
         DC    (256-(*-TRTBL))AL1(*-TRTBL)
         TITLE 'CUTILTBL - Translate Reverse Table                    '
*     * /********************************************************/
*     * /* Translate table reverse it!                          */
*     * /********************************************************/
*     * /* abcdefghi  to  zyxwvutsr                             */
*     * /* jklmnopqr  to  qponmlkji                             */
*     * /*  stuvwxyz  to   hgfedcba                             */
*     * /* ABCDEFGHI  to  ZYXWVUTSR                             */
*     * /* jklmnopqr  to  qponmlkji                             */
*     * /*  stuvwxyz  to   hgfedcba                             */
*     * /* 0123456789 to  9876543210                            */
*     * /********************************************************/
TREVRS   DC    256C' '             256 blanks
         ORG   TREVRS+C'a'
         DC    C'zyxwvutsr'
         ORG   TREVRS+C'j'
         DC    C'qponmlkji'
         ORG   TREVRS+C's'
         DC    C'hgfedcba'
         ORG   TREVRS+C'A'
         DC    C'ZYXWVUTSR'              tr string(5),TREVRS
         ORG   TREVRS+C'J'
         DC    C'QPONMLKJI'
         ORG   TREVRS+C'S'
         DC    C'HGFEDCBA'
         ORG   TREVRS+C'0'
         DC    C'9876543210'
         TITLE 'CUTILTBL - Message Address Table / Message Text       '
*     * /********************************************************/
*     * /* Message Address Table                                */
*     * /********************************************************/
MSGTBL   DS    0F
*              MSGNUM ,MSGTXT    ,MSGTXTL
*              -------,----------,-----------
         DC    A(4000),A(MSG4000),A(L'MSG4000)
         DC    A(4001),A(MSG4001),A(L'MSG4001)
         DC    A(4002),A(MSG4002),A(L'MSG4002)
         DC    A(4004),A(MSG4004),A(L'MSG4004)
         DC    A(4005),A(MSG4005),A(L'MSG4005)
         DC    A(4008),A(MSG4008),A(L'MSG4008)
         DC    A(4009),A(MSG4009),A(L'MSG4009)
         DC    A(4010),A(MSG4010),A(L'MSG4010)
         DC    A(4011),A(MSG4011),A(L'MSG4011)
         DC    A(4012),A(MSG4012),A(L'MSG4012)
         DC    A(4014),A(MSG4014),A(L'MSG4014)
         DC    A(4015),A(MSG4015),A(L'MSG4015)
         DC    A(4016),A(MSG4016),A(L'MSG4016)
         DC    A(4017),A(MSG4017),A(L'MSG4017)
         DC    A(4018),A(MSG4018),A(L'MSG4018)
         DC    A(4019),A(MSG4019),A(L'MSG4019)
         DC    A(4020),A(MSG4020),A(L'MSG4020)
         DC    A(4021),A(MSG4021),A(L'MSG4021)
         DC    A(4022),A(MSG4022),A(L'MSG4022)
         DC    A(4023),A(MSG4023),A(L'MSG4023)
         DC    A(4030),A(MSG4030),A(L'MSG4030)
         DC    A(4031),A(MSG4031),A(L'MSG4031)
         DC    A(4032),A(MSG4032),A(L'MSG4032)
         DC    A(4033),A(MSG4033),A(L'MSG4033)
         DC    A(4034),A(MSG4034),A(L'MSG4034)
         DC    A(4035),A(MSG4035),A(L'MSG4035)
         DC    A(4036),A(MSG4036),A(L'MSG4036)
         DC    A(4037),A(MSG4037),A(L'MSG4037)
         DC    A(4038),A(MSG4038),A(L'MSG4038)
         DC    A(4039),A(MSG4039),A(L'MSG4039)
         DC    A(4040),A(MSG4040),A(L'MSG4040)
         DC    A(4047),A(MSG4047),A(L'MSG4047)
         DC    A(4048),A(MSG4048),A(L'MSG4048)
         DC    A(4049),A(MSG4049),A(L'MSG4049)
         DC    A(4050),A(MSG4050),A(L'MSG4050)
         DC    A(4051),A(MSG4051),A(L'MSG4051)
         DC    A(4060),A(MSG4060),A(L'MSG4060)
         DC    A(4071),A(MSG4071),A(L'MSG4071)                          LB1105a
         DC    A(4072),A(MSG4072),A(L'MSG4072)                          LB1105a
         DC    A(4073),A(MSG4073),A(L'MSG4073)                          LB1105a
         DC    A(4074),A(MSG4074),A(L'MSG4074)                          LB1105a
         DC    A(4075),A(MSG4075),A(L'MSG4075)                          LB1105a
         DC    A(4098),A(MSG4098),A(L'MSG4098)
MSGTLAST DC    X'FFFFFFFF',A(MSGNFND),A(L'MSGNFND)  Must be Last Entry
MSGTBLL  EQU   *-MSGTLAST                  Length of table entry
MSGTBLE  EQU   *                           End of table
MSGTBL#  EQU   (*-MSGTBL)/MSGTBLL          Number of table entries
*
         EJECT
*     * /********************************************************/
*     * /* Message Text                                         */
*     * /********************************************************/
MSGS     EQU   *
MSGNFND  DC    C'Message not found'
MSGDONE  DC    C'Successful Request '
MSG4000  DC    C'FALSE'
MSG4001  DC    C'TRUE'
MSG4002  DC    C'VAR1 not found'
MSG4004  DC    C'No PARM'
MSG4005  DC    C'WORD GT 8 bytes'
MSG4008  DC    C'CPPL no PARM'
MSG4009  DC    C'Too many words'
MSG4010  DC    C'SETVAR no content'
MSG4011  DC    C'Invalid Function'
MSG4012  DC    C'VAR2 not updated'
MSG4014  DC    C'VAR1 GT 256 bytes'
MSG4015  DC    C'VAR2 required'
MSG4016  DC    C'VAR2 not found'
MSG4017  DC    C'VAR2 GT 256 bytes'
MSG4018  DC    C'VAR2 null'
MSG4019  DC    C'VAR1 null'
MSG4020  DC    C'ERRMSG cannot create updt'
MSG4021  DC    C'VAR1 required'
MSG4022  DC    C'VAR2 not required'
MSG4023  DC    C'Result GT 256'
MSG4030  DC    C'No begin "'
MSG4031  DC    C'No end "'
MSG4032  DC    C'Start GT End'
MSG4033  DC    C'Invalid char'
MSG4034  DC    C'Not numeric'
MSG4035  DC    C'Too many digits'
MSG4036  DC    C'Null value not allowed'
MSG4037  DC    C'No begin QUOTE'
MSG4038  DC    C'No end QUOTE'
MSG4039  DC    C'No end QUOTE found'
MSG4040  DC    C'Invalid length'
MSG4047  DC    C'Invalid DD'
MSG4048  DC    C'Invalid DOW'
MSG4049  DC    C'Invalid MM'
MSG4050  DC    C'Invalid DD for MM'
MSG4051  DC    C'Invalid JJJ'
MSG4060  DC    C'Call error w CUTILTBL'
MSG4071  DC    C'Invalid Function:date-to-MDCY'                         LB1105a
MSG4072  DC    C'Invalid Function:date-from-MDCYJ'                      LB1105a
MSG4073  DC    C'Invalid Function:date-to-JCY'                          LB1105a
MSG4074  DC    C'Invalid Function:date CYJ-'                            LB1105a
MSG4075  DC    C'Invalid Function:date MDCY-'                           LB1105a
MSG4098  DC    C'Cannot link to IKJCT441'
*
MSGSL    EQU   *-MSGS
*
         TITLE 'CUTILTBL - Equates                                    '
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
         TITLE 'CUTILTBL - Working Storage Variables                  '
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
*RC       DS    CL2                 Return Code
*
MVCR8$6  MVC   0(0,R6),0(R8)       MVC R8 to R6 (ERRMSG)
*
WSAREAE  EQU   *                   Working Storage END
WSAREAL  EQU   WSAREAE-WSAREA      Working Storage Area Length
*
         END   CUTILTBL
@@
//LKED.SYSLMOD DD  DISP=SHR,
//         DSN=SYS2.CMDLIB(CUTILTBL) 
//CLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.CLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CCUTIL0I
PROC 0 DEBUG                          /* CUTIL00 Utility Driver     */ -LB1100
      TPMNM(CUTIL00)                   /* TPM name                  */ -LB1100
      PANELID(PCUTIL00)                /* Panel name                */ -LB1100
      PMSG(Y)                          /* Panel Messages Y=yes N-no */  LB1100

/********************************************************************/
/*                                                                  */
/* CLIST: CCUTIL0I                                                  */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitOfIT.net/CUTIL00-for-mvs-3-8j/          */
/*         Copyright (C) 2020-2021  Larry Belmontes, Jr.            */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 Panel driver to invoke CUTIL00.                         */
/*                                                                  */
/* Note: Program CLS is used to clear display screen.               */
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
/*    In addition, the author requests submissions regarding any    */
/* code modifications / enhancements and/or associated comments     */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* benefiting the MVS 3.8J hobbyist public domain community.        */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History: <CHGHIST>                                        */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 04/10/2021 1.1.00   Larry Belmontes Jr.                          */  LB1100
/*                     - Replace CLIST variable with PROC variables */  LB1100
/*                       to allow overrides when PROC is started    */  LB1100
/*                       (DEBUG, TPMNM, PANELID, PMSG)              */  LB1100
/*                     - On DEBUG, display VAR01 and VAR02          */  LB1100
/*                       including ISPF DISPLAY PANEL RC            */  LB1100
/*                     - Detect and display truncation message      */  LB1100
/*                       for VAR01 and VAR02                        */  LB1100
/*                                                                  */  LB1100
/* 03/20/2020 1.0.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/


  /******************************************************************/
  /* TSO CLIST Control Statement                                    */
  /******************************************************************/
  CONTROL MAIN

  /******************************************************************/
  /* ISPF Control statement                                         */
  /******************************************************************/
  ISPEXEC CONTROL ERRORS RETURN

  /******************************************************************/
  /* First-time Enter Data message                                  */
  /******************************************************************/
  IF &PMSG EQ Y THEN -                                                  LB1100
    DO
      SET &ZERRSM  = &STR(Enter Data)
      SET &ZERRLM  = &STR(Enter function and variables for &TPMNM CP)
      SET &ZERRMSG = &STR(CUTIL00Z)
    END

  /******************************************************************/
  /* DISPLAY PANEL                                                  */
  /******************************************************************/
  GETDATA:  +
  IF &PMSG EQ Y THEN -                                                  LB1100
    ISPEXEC DISPLAY PANEL(&PANELID) MSG(&ZERRMSG)
  ELSE -
    ISPEXEC DISPLAY PANEL(&PANELID)
  SET RC = &LASTCC

  IF &DEBUG = DEBUG THEN -                                              LB1100
    DO                                                                  LB1100
      WRITE DISPLAY PANEL   RC=&RC                                      LB1100
      WRITE VAR01='&VAR01'                                              LB1100
      WRITE VAR02='&VAR02'                                              LB1100
    END                                                                 LB1100
                                                                        LB1100
  /******************************************************************/
  /* EXIT on PF3 or PF4                                             */
  /******************************************************************/
  IF &RC = 8 THEN GOTO DONE

  SET VAR01L =      /* reset length of VAR01  */
  SET VAR02L =      /* reset length of VAR02  */
  SET ERRMSG =      /* reset ERRMSG           */
  SET CD =          /* reset RC               */

  /******************************************************************/
  /* GETDATA on PF1 HELP, Help screen displayed by ISPF 2.0         */
  /******************************************************************/
  IF &KEYPRESS EQ PF01 THEN -
      DO
        IF &PMSG EQ Y THEN -                                            LB1100
          DO
            SET ZERRSM = &STR(HELP PANEL PROCESSED)
            SET ZERRLM = &STR(PF1 Help panel displayed)
            SET &ZERRMSG = &STR(CUTIL00Z)
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
      IF &PMSG EQ Y THEN -                                              LB1100
        DO
          SET ZERRSM = &STR(INVALID PF KEY)
          SET ZERRLM = &STR(Invalid PF key detected, no action taken.)
          SET &ZERRMSG = &STR(CUTIL00Z)
        END
      GOTO GETDATA
    END

  /******************************************************************/
  /* Build Command Line                                             */
  /******************************************************************/
  SET CMDLINE = &TPMNM
  IF &STR(&FUNCT) > &STR( ) THEN -
    DO
      SET PARMS   = &STR(&FUNCT VAR01)
      /* using VAR02?  */
      IF &UV2 = Y THEN -
        SET PARMS = &STR(&PARMS)&STR( VAR02)
      ELSE -
      IF &UV2 = $ THEN -
        SET PARMS = &STR(&PARMS)&STR( $NOVAR2)
      ELSE -
        SET PARMS = &STR(&PARMS)&STR( )
      SET CMDLINE = &STR(&CMDLINE &PARMS)
      /* insert QUOTE keyword?  */
      IF &UQ1 = &STR(Y) THEN -
        SET CMDLINE = &STR(&CMDLINE)&STR( QUOTE)
    /* set vars to NULL             */
      IF &STR(&VAR01) = NULL  THEN -                                    LB1100
        SET VAR01 =
      IF &STR(&VAR02) = NULL  THEN -                                    LB1100
        SET VAR02 =
    END
  ELSE -
    /* no function, pass null parms */
    SET PARMS   = &STR()


  /******************************************************************/
  /* QUOTE parm needs $NOVAR2 when VAR2 is no.                      */
  /******************************************************************/
  IF &UV2 = N AND &UQ1 = Y THEN +
    DO
      SET UV2=&STR($)
      SET CMDLINE = &STR(&TPMNM &STR(&FUNCT) VAR01 $NOVAR2 QUOTE)
    END

  /******************************************************************/
  /* EXECUTE TPM                                                    */
  /******************************************************************/
  &CMDLINE
  SET RC = &LASTCC

  /******************************************************************/  LB1100
  /* CHECK FOR TRUNCATED RESULTS (VAR01 and VAR02)                  */  LB1100
  /******************************************************************/  LB1100
  SET VAR01TR =                                                         LB1100
  SET VAR02TR =                                                         LB1100
  IF &LENGTH(&STR(&VAR01)) > 78 THEN -                                  LB1100
    DO                                                                  LB1100
      SET VAR01 = &SUBSTR(1:78,&STR(&VAR01)                             LB1100
      SET VAR01TR = &STR(** value truncated **)                         LB1100
    END                                                                 LB1100
  IF &LENGTH(&STR(&VAR02)) > 78 THEN -                                  LB1100
    DO                                                                  LB1100
      SET VAR02 = &SUBSTR(1:78,&STR(&VAR02)                             LB1100
      SET VAR02TR = &STR(** value truncated **)                         LB1100
    END                                                                 LB1100
                                                                        LB1100
  IF &DEBUG = DEBUG THEN +                                              LB1100
    DO
      CLS
      WRITE CMDLINE='&CMDLINE'   RC=&RC
    END

  /******************************************************************/  LB1100
  /* LIST VARIABLES FOR UNSTR FUNCTION                              */  LB1100
  /******************************************************************/  LB1100
  IF &STR(&FUNCT) = UNSTR THEN DO
  /*WRITE VAR010 = '&VAR010' */                                         LB1100
    SET CTR = 0
    DO WHILE &CTR <= &VAR010
      SET EVALLN = &STR(&&VAR01&CTR)
      WRITE VAR01&CTR = '&EVALLN'
      SET CTR = &CTR + 1
    END
  END

  SET CD = &RC

  /******************************************************************/
  /* PANEL PROCESSING COMPLETE                                      */
  /******************************************************************/

  IF &PMSG EQ Y THEN -                                                  LB1100
    DO
      SET ZERRSM = &STR(&TPMNM RC=&RC)
      SET ZERRLM = &STR(RC=&RC &CMDLINE)
      SET &ZERRMSG = &STR(CUTIL00Z)
    END

  /******************************************************************/
  /* GET ANOTHER REQUEST FROM PANEL                                 */
  /******************************************************************/
  GOTO GETDATA

  /******************************************************************/
  /* DONE...                                                        */
  /******************************************************************/
  DONE: +
  IF &DEBUG = DEBUG THEN +                                              LB1100
    WRITE BYE...

  EXIT CODE(0)


END   /* END PROC                     */

@@
//MLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.MLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CUTIL0
/********************************************************************/
/*                                                                  */
/* MESSAGES: CUTIL0                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitOfIT.net/CUTIL00-for-mvs-3-8j/          */
/*         Copyright (C) 2020-2021  Larry Belmontes, Jr.            */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 Generic message entry for PANEL PCUTIL00.               */
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
/*    In addition, the author requests submissions regarding any    */
/* code modifications / enhancements and/or associated comments     */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* benefiting the MVS 3.8J hobbyist public domain community.        */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History: <CHGHIST>                                        */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 03/20/2020 1.0.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
CUTIL00Z '&ZERRSM                 ' .ALARM=NO
'&ZERRMSG &ZERRLM                                                              '
CUTIL01  'FUNCTION is blank       ' .ALARM=NO
'CUTIL01  FUNCTION cannot be blank                                             '
CUTIL02  'Invalid VAR1 Quote      ' .ALARM=NO
'CUTIL02  Invalid value in VAR1 Quote                                          '
CUTIL03  'Invalid VAR2 Use        ' .ALARM=NO
'CUTIL03  Invalid value in VAR2 Use                                            '
@@
//PLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.PLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=HCUTIL00
/********************************************************************/
/*                                                                  */
/* PANEL: HCUTIL00                                                  */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitOfIT.net/CUTIL00-for-mvs-3-8j/          */
/*         Copyright (C) 2020-2021  Larry Belmontes, Jr.            */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 HELP for panel PCUTIL00                                 */
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
/*    In addition, the author requests submissions regarding any    */
/* code modifications / enhancements and/or associated comments     */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* benefiting the MVS 3.8J hobbyist public domain community.        */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History: <CHGHIST>                                        */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/05/2021 1.1.05   Larry Belmontes Jr.                          */  LB1105
/*                     - Removed hard-coded version on panel        */  LB1105
/*                     - Removed EXPAND characters on panel         */  LB1105
/*                                                                  */  LB1105
/* 04/10/2021 1.1.00   Larry Belmontes Jr.                          */  LB1100
/*                     - Changed version and panel description.     */  LB1100
/*                                                                  */  LB1100
/* 03/20/2020 1.0.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
!   TYPE(OUTPUT) INTENS(LOW) CAPS(OFF) JUST(ASIS)
[   TYPE(TEXT)   INTENS(HIGH)                      COLOR(TURQ)
[   TYPE(TEXT)   INTENS(LOW)                       COLOR(PINK)
{   TYPE(TEXT)   INTENS(HIGH)                      COLOR(GREEN)
}   TYPE(TEXT)   INTENS(HIGH)                      COLOR(YELLOW)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%--HELP--------------------}CUTIL00 for MVS38J%---------------------------------
%COMMAND ===>_ZCMD                                     +   %PANEL    - &ZPANELID
+                                                          %USERID   - &ZUSER  +
+                                                                              +
+ This panel is used to invoke CUTIL00 as a driver of a single function        +
+ as an interactive unit test facility.                                        +
+                                                                              +
+ o%FUNCTION  +Function to be performed                                        +
+ o%VAR1 Quote}Y+VAR1 content and length is determined by quote}(')+delimiters +
+             }N+VAR1 content is 78 bytes in length                            +
+ o%VAR2 Use  }Y+VAR2 specified in command line                                +
+             }N+VAR2 is%NOT+specified in command line                         +
+             }$%$NOVAR2+placeholder is used as VAR2 in command line           +
+               +(same as%NOT+specifying a VAR2 variable name)                 +
+                                                                              +
+ o{command line displayed here for invoked function                           +
+ o%Return CD +Return code and error message from operation                    +
+ o%VAR1      +VAR1 input of 78 bytes in length or delimited by single quotes  +
+             }Len=+represents length of a VAR1 result.                        +
+ o%VAR2      +VAR2 input is always 78 bytes in length.                        +
+             }Len=+represents length of a VAR2 result.                        +
+                                                                              +
+                                                                              +
+%Note:[VAR1 and VAR2 limited to 78 characters due to panel presentation.      +
)INIT
.CURSOR = ZCMD
)PROC

&RESP0 = .RESP
&KEYPRESS = .PFKEY

)END

./ ADD NAME=PCUTIL00
/********************************************************************/
/*                                                                  */
/* PANEL: PCUTIL00                                                  */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitOfIT.net/CUTIL00-for-mvs-3-8j/          */
/*         Copyright (C) 2020-2021  Larry Belmontes, Jr.            */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 panel to invoke CUTIL00 and display results             */
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
/*    In addition, the author requests submissions regarding any    */
/* code modifications / enhancements and/or associated comments     */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* benefiting the MVS 3.8J hobbyist public domain community.        */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History: <CHGHIST>                                        */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/05/2021 1.1.05   Larry Belmontes Jr.                          */  LB1105
/*                     - Removed hard-coded version on panel        */  LB1105
/*                     - Removed EXPAND characters on panel         */  LB1105
/*                                                                  */  LB1105
/* 04/10/2021 1.1.00   Larry Belmontes Jr.                          */  LB1100
/*                     - Changed version                            */  LB1100
/*                     - Added VAR01TR and VAR02TR variables        */  LB1100
/*                                                                  */  LB1100
/* 03/20/2020 1.0.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
% TYPE(TEXT) INTENS(HIGH)
+ TYPE(TEXT) INTENS(LOW)
_ TYPE(INPUT) INTENS(HIGH) CAPS(ON)  JUST(LEFT)
~ TYPE(INPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS)
[   TYPE(TEXT)   INTENS(HIGH)                      COLOR(TURQ)
[   TYPE(TEXT)   INTENS(LOW)                       COLOR(PINK)
{   TYPE(TEXT)   INTENS(HIGH)                      COLOR(GREEN)
}   TYPE(TEXT)   INTENS(HIGH)                      COLOR(YELLOW)
!   TYPE(TEXT)   INTENS(LOW)       HILITE(REVERSE) COLOR(BLUE)
?   TYPE(TEXT)   INTENS(LOW)       SKIP(ON)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%--------------------------}CCUTIL0 for MVS38J%---------------------------------
%COMMAND ===>_ZCMD                                                             +
+                                                          %PANEL    - &ZPANELID
%FUNCTION  %=>_FUNCT   +                             +     %USERID   - &ZUSER  +
%VAR1 Quote%=>_Z?}Y,N+                               +     %TIME     - &ZTIME  +
%VAR2 Use  %=>_Z?}Y,N,$NOVAR2+                             %DATE     - &ZDATE  +
{&CMDLINE                                              +            %- &ZJDATE +
%Return CD:+&CD                                       +    %SYSTEM   -}&ZSYSID +
%          [&ERRMSG                                   +    %TERMINAL - &ZTERM  +
%                                                     +    %TermSize[&SD  &SW  +
{                                            +        +    %APPLID   -[&ZAPPLID+
%VAR1 }Len=&VAR01L % [&VAR01TR                                                 +
+0        1         2         3         4         5         6         7        +
+1...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...+
~VAR01                                                                         +
+                                                                              +
%VAR2 }Len=&VAR02L % [&VAR02TR                                                 +
+0        1         2         3         4         5         6         7        +
+1...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...+
~VAR02                                                                         +
+                                                                              +
+                                                                              +
+                                                                              +
%  PF1[Help   %PF3[End   %ENTER[process %                                      +
)INIT
&SD = &ZSCREEND
&SW = &ZSCREENW
.CURSOR = FUNCT
.HELP = HCUTIL00
.ZVARS = '(UQ1,UV2)'
IF (&UQ1 = ' ')
  &UQ1 = Y
IF (&UV2 = ' ')
  &UV2 = Y
&RESP0 = .RESP
&KEYPRESS = .PFKEY

)REINIT
REFRESH(*)          /* refresh all fields */
&RESP0 = .RESP
&KEYPRESS = .PFKEY

)PROC
&RESP0 = .RESP
&KEYPRESS = .PFKEY

/*IF (&KEYPRESS NE PF03)
    VER(&FUNCT,NB,MSG=CUTIL01)
    VER(&UQ1,LIST,Y,N,MSG=CUTIL02)
    VER(&UV2,LIST,Y,N,$,MSG=CUTIL03)


)END

@@
