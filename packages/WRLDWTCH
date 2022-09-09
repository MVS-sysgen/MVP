//WRLDWTCH JOB (JOB),
//             'INSTALL WRLDWTCH',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//* -------------------------------------------------------*
//* *  WRLDWTCH for MVS3.8J TSO / Hercules                 *
//* *                                                      *
//* *       Install WRLDWTCH Program                       *
//* *                                                      *
//* -------------------------------------------------------*
//ASML     PROC MBR=WHOWHAT
//ASM      EXEC PGM=IFOX00,
//             PARM='NODECK,LOAD,RENT,TERM,XREF'
//SYSGO    DD  DSN=&&LOADSET,DISP=(MOD,PASS),SPACE=(CYL,(1,1)),
//             UNIT=VIO,DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS2.MACLIB,DISP=SHR          ** YREG  **
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSPUNCH DD  DSN=NULLFILE
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(6,1))
//SYSUT2   DD  UNIT=VIO,SPACE=(CYL,(6,1))
//SYSUT3   DD  UNIT=VIO,SPACE=(CYL,(6,1))
//SYSIN    DD  DUMMY
//*
//LKED     EXEC PGM=IEWL,PARM='MAP,LIST,LET,RENT,XREF,RENT,REFR',
//             COND=(0,NE,ASM)
//SYSLIN   DD  DSN=&&LOADSET,DISP=(OLD,DELETE)
//         DD  DDNAME=SYSIN
//SYSLMOD  DD  DSN=SYSGEN.ISPF.LLIB(&MBR),DISP=SHR     <--TARGET
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(5,2))
//SYSIN    DD  DUMMY
//         PEND
//WRLDWTCH EXEC  ASML,MBR=WRLDWTCH
//ASM.SYSIN DD DATA,DLM=@@
         MACRO
&NAME    LA#ST &A,&B
**********************************************************************
*   Macro:  LA#ST
**********************************************************************
*        Load Address and Store (uses R0)                            *
**********************************************************************
&NAME    LA    0,&A
         ST    0,&B
*              END OF LA#ST
         MEND
         TITLE 'WRLDWTCH: World Watch CBT366 in IFOX00                '
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  ====================================================================
*
* WW   WW  RRRRRR   LL       DDDDD    WW   WW  TTTTTTT   CCCC   HH   HH
* WW   WW  RR   RR  LL       DD  DD   WW   WW    TT     CC  CC  HH   HH
* WW   WW  RR  RR   LL       DD   DD  WW   WW    TT    CC       HH   HH
* WW W WW  RRRRR    LL       DD   DD  WW   WW    TT    CC       HHHHHHH
* WWWWWWW  RR RR    LL       DD   DD  WWWWWWW    TT    CC       HH   HH
* WWW WWW  RR  RR   LL       DD  DD   WWW WWW    TT     CC  CC  HH   HH
* WW   WW  RR   RR  LLLLLLL  DDDDD    WW   WW    TT      CCCC   HH   HH
*
*  ====================================================================
* 1234567--1234567--1234567--1234567--1234567--1234567-1234567--1234567
*
*  Program: WRLDWTCH
*
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/WRLDWTCH-in-MVS38J
*           Copyright (C) 2018  Larry Belmontes, Jr.
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
*     The World Clock Program in CBT File 366 is from Marvin Shaw.
*  This software was submitted in 1999 as a REXX / ISPF application.
*  The panel uses a dynamic area to display a world map with time
*  zones on a 3270 terminal.
*
*     Thanks to Marvin for this contribution to CBT.
*
*     As a learning experience and conversion exercise, I embarked in
*  the task of rewritting the original World Clock suite (CBT File 366)
*  using IFOX00 and ISPF 2.0 (from Wally Mclaughlin).
*
*     WRLDWTCH, simulates the REXX logic and displays the World Clock
*  via ISPF.
*
*     The limitations desiring the conversion at this time:
*  1. REXX (proper) not available in MVS 3.8J public domain OS.
*  2. ISPF 2.0 supports variable sharing for TSO CLISTs only.
*
*     The modified software components include:
*
*       - WRLDWTCH  TSO CP to build and display World Clock
*                   using ISPF services
*       - PWW       ISPF 2.0 Modified Panel
*
*     WRLDWTCH is invoked from the ISPF/TSO COMMAND panel or from a
*  ISPF menu.
*
*     TSO Command:            TSO WRLDWTCH
*
         EJECT
*
*     If invoking from an ISPF menu, use the suggested menu entry:
*
*    %  WW +WRLDWTCH    - World WATCH Timezones
*
*     Assume an ISPF menu panel has the following sections, the
*  'NEW ENTRY' line can be added to invoke World WATCH when menu
*  option 'WW is typed followed by the <ENTER> key:
*
*    )PROC
*      &ZSEL = TRANS( TRUNC (&ZCMD,'.')
*                    1,'CMD(xxxxx) NEWAPPL(ISR)'
*                    6,'PGM(xxxxxx)'
*                    .
*                    .
*                    C,'CMD(WRLDWTCH)'               <-- NEW ENTRY
*                    .
*                    .
*                  ' ',' '
*                    *,'?' )
*    )END
*
*     Hope this program benefits your toolset and/or learning knowledge
*  in regards to MVS 3.8J and ISPF 2.x.
*
*     As a challenge to the reader, improve this program by adding a
*  a parm to the WRLDWTCH command as 'WRLDWTCH -3' to set the default
*  time zone at execution time.  Remember, editing the parm will be
*  required before using the value in Assembler instructions!
*
*  Have fun!
*
*  Larry Belmontes Jr.
*
*
         EJECT
*  Prerequisite: User Modifications
*  ===================================================================
*
*     No user-mods are needed for this software distribution.
*
         EJECT
*  WRLDWTCH Programs / Services:
*  ==================================================================
*
*    1) GETMAIN/FREEMAIN          Working storage
*    2) ISPLINK                   ISPLINK for ISPF Services
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
*  |  00  |  Successful Completion                                   |
*  +------+----------------------------------------------------------+
*  |  20  |  ISPF VDEFINE service error                              |
*  +------+----------------------------------------------------------+
*  |  21  |  ISPF DISPLAY service error                              |
*  +------+----------------------------------------------------------+
*  | 4097 |  ISPLINK not found                                       |
*  +------+----------------------------------------------------------+
*
         EJECT
*  Macros and SYSLIB Location:
*  ==================================================================
*
*  Macro     Description                         Library
*  --------  ----------------------------------  ------------------
*  LA#ST     LA and ST macro                     in-line, supplied
*  YREGS     Register Equates                    SYS2.MACLIB
*
*  References:
*  ==================================================================
*
*  - CBT File#366   FILE366 World Clock Program from Marvin Shaw
*
*
         EJECT
*  Messages:
*  ==================================================================
*
*  +--------+--------------------------------------------------------+
*  |Msg ID  |Description                                             |
*  +--------+--------------------------------------------------------+
*  |        |No ISPF Messages...                                     |
*  +--------+--------------------------------------------------------+
*
*
*
         EJECT
*  Change History:
*  ==================================================================
*
*  MM/DD/CCYY Version  Name / Description
*  ---------- -------  --------------------------------------------
*
*  09/22/2018 0.9.00   Larry Belmontes Jr.
*                      - Initial version released to MVS 3.8J
*                        hobbyist public domain
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
WRLDWTCH CSECT
         USING WRLDWTCH,R10,R11,R12 my BASE REGISTER(S)
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
*
*     * /********************************************************/
*     * /* Program Eyecatcher                                   */
*     * /********************************************************/
         B     OVRSTAMP            Branch past eye-catcher
MYSTAMP  EQU   *
PGMID    DC    CL8'WRLDWTCH'       My Program STAMP
         DC    CL8'MVS3.8J '       OS
PGMVRM   DC    CL8'V0.9.00 '       .Version
         DC    CL8'09222018'       ..date  MMDDCCYY
         DC    CL1' '
PGMDS    DC    CL8'&SYSDATE'       Assemble Date
         DC    CL3' - '            .and
PGMTS    DC    CL8'&SYSTIME'       ..Time
         DC    C'Copyright (C) 2018'
         DC    C'Larry Belmontes, Jr. '
         DC    C'https://ShareABitofIT.net/WRLDWTCH-in-MVS38J'
MYSTAMPL EQU   *-MYSTAMP
OVRSTAMP DS    0H
*
         EJECT
*     * /********************************************************/
*     * /* GETMAIN, working storage, using R9                   */
*     * /* - R1  Working register (GETMAIN address)             */
*     * /* - R13 Working Storage DSECT address                  */
*     * /********************************************************/
         GETMAIN   R,LV=WSAREAL    Get Storage, R1 = addr of storage
         LTR   R15,R15             Did we get the storage?
         BNZ   NOMAINS             NO, return with rc in R15
*
*     * /********************************************************/
*     * /* Update Save Areas for caller and calling programs    */
*     * /* - R1  myPARMS address on entry  (restored)           */
*     * /********************************************************/
         ST    R13,4(R1)           Store HSA addr, chain caller > mySA
         ST    R1,8(R13)           Store LSA addr, chain mySA > caller
         LR    R13,R1              R13 = Workarea DSECT Register
         USING WSDSECT,R13         Tell Assembler
         LR    R1,R2               Restore R1 as PARM addr
*
*     * /********************************************************/
*     * /* ISPLINK Entry Point                                  */
*     * /* - R0, R1                   Working Register          */
*     * /********************************************************/
         XC    ISPLINK,ISPLINK     Clear ISPLINK Address
         LOAD  EP=ISPLINK,ERRET=ERR4097
         ST    R0,ISPLINK          Save ISPLINK Entry Point
         LR    R1,R2               Restore R1 as PARM addr
*
*     * /********************************************************/
*     * /* Initialize Working Storage                           */
*     * /********************************************************/
*
*     * /********************************************************/
*     * /* Get PARAMETER information passed to program          */
*     * /********************************************************/
*
         TITLE 'WRLDWTCH: - - REXX Equivalent from CBT366             '
*     * /********************************************************/
*     * /* Set GMT for current time zone                        */
*     * /* REXX Statements:                                     */
*     * /* GMT = -6                                             */
*     * /********************************************************/
         ZAP   GMT,=P'-6'         Dallas  GMT - 6
*
*     * /********************************************************/
*     * /* Set M stem variables for World map image on 3270     */
*     * /* REXX Statements:                                     */
*     * /* M.0 =20                                              */
*     * /* M.1 ='...   .......                                  */
*     * /* .                                                    */
*     * /* M.20='...   .......                                  */
*     * /********************************************************/
*
*     *  One dynamic area is defined on ISPF Panel.
*     *
*     *  WWDATA is the data component.
*     *  WWATTR is the shadow component.
*     *  Dynamic area used to render 21 lines on 3270 screen.
*     *
*     *  WWDATA is build from the Mn data in the declarative section
*     *  representing the World WATCH!
*     *
*     *  M, WWDATA and WWATTR strings are and MUST be the same length.
*     *
*
         EJECT
*     * /********************************************************/
*     * /* Move Mn to WWDATA string translating WWDATA          */
*     * /* into WWATTR string                                   */
*     * /* - R5, R6, R7, R8  Working Registers                  */
*     * /* REXX Statements:                                     */
*     * /* DO I = 1 TO M.0                                      */
*     * /*   INTERPRET "WWDATA"RIGHT(I,2,'0') "= '02'X"         */
*     * /*   INTERPRET "WWATTR"RIGHT(I,2,'0') "=                */
*     * /*           ' "TRANSLATE(M.I,'03040506'X,' H.#')"'"    */
*     * /* END                                                  */
*     * /********************************************************/
         LA    R5,M                Load start addr of M (M.n)
         LA    R6,WWDATA           Load start addr of WWDATA
         LA    R7,WWATTRL          Load length of WWATTR
         LA    R8,WWATTR           Load start addr of WWATTR
MVCDATA  EQU   *
         MVC   0(1,R6),0(R5)       Move M.n(R5) to WWDATA(R6)
ATTRCHK  EQU   *
         CLI   0(R6),C' '          Is DATA a SPACE?
         BE    MVCATTR1
         CLI   0(R6),C'H'          Is DATA an H?
         BE    MVCATTR2
         CLI   0(R6),C'.'          Is DATA an .?
         BE    MVCATTR3
         CLI   0(R6),C'#'          Is DATA an #?
         BE    MVCATTR4
         MVI   0(R8),X'07'         Default translation...
         B     MVCATTRX
MVCATTR1 EQU   *
         MVI   0(R8),X'03'         XLATE SP to 03 TYPE(CHAR)
         B     MVCATTRX
MVCATTR2 EQU   *
         MVI   0(R8),X'04'         XLATE H  to 04 TYPE(CHAR)
         B     MVCATTRX
MVCATTR3 EQU   *
         MVI   0(R8),X'05'         XLATE .  to 05 TYPE(CHAR)
         B     MVCATTRX
MVCATTR4 EQU   *
         MVI   0(R8),X'06'         XLATE *  to 06 TYPE(CHAR)
         B     MVCATTRX
MVCATTRX EQU   *
         LA    R5,1(R5)            Bump M     to next byte
         LA    R6,1(R6)            Bump WWDATA to next byte
         LA    R8,1(R8)            Bump WWATTR to next byte
         BCT   R7,MVCDATA          Do entire length of WWATTR
*
         EJECT
*     * /********************************************************/
*     * /* Determine HH for each time zone for M.21 (M21)       */
*     * /* via local subroutine                                 */
*     * /* - R14 used for BAL return......                      */
*     * /* REXX Statements:                                     */
*     * /* TIME0 = TIME(M) % 60 - GMT                           */
*     * /* DO I = 1 TO 24                                       */
*     * /*   TIME = TIME0 + I                                   */
*     * /*   IF TIME <   0 THEN TIME = TIME + 24                */
*     * /*   IF TIME >= 24 THEN TIME = TIME - 24                */
*     * /*   WWDATA20 = WWDATA20 LEFT(TIME,2,' ')               */
*     * /* END                                                  */
*     * /********************************************************/
         BAL   R14,TZLINE          Get time and update timezone line
*
*     * /********************************************************/
*     * /* Overlay of WWDATA09                                  */
*     * /* REXX Statements:                                     */
*     * /* WWDATA09 = OVERLAY('Dallas--------*',WWDATA09,03)    */
*     * /********************************************************/
         LA    R4,WWDATA           Addr of WWDATA
         LA    R4,WWDATA09(R4)     Addr of WWDATA09
         MVC   2(15,R4),=C'Dallas--------*'     Move to WWDATA09+2
*
         EJECT
*     * /********************************************************/
*     * /* Store &WWDATA Variable for ISPF                      */
*     * /* - R0, R1, R14, R15     Working Registers             */
*     * /********************************************************/
*        CALL  ISPLINK,(VDEFINE,DWWDATA,WWDATA,CHAR,WWDATAL),VL
         LA#ST VDEFINE,PALP1       Service   pointer
         LA#ST DWWDATA,PALP2       Var Name  pointer
         LA#ST WWDATA,PALP3        Var Data  pointer
         LA#ST CHAR,PALP4          Var Type  pointer
         LA#ST WWDATAL,VARPTR      Init VARPTR with Var Value Len
         LA#ST VARPTR,PALP5        VARPTR    pointer
         MVI   PALP5,B'10000000' Last Parm designation
         LA    R1,PALIST           R1 = Parm List
         L     R15,ISPLINK         Link to ISPLINK
         BALR  R14,R15               via BALR
         ST    R15,SAVER15         SAVE R15
         LTR   R15,R15             Successful invokation?
         BNZ   ERR0020             No, ISPLINK VDEFINE error
*
         EJECT
*     * /********************************************************/
*     * /* Store &WWATTR Variable for ISPF                      */
*     * /* - R0, R1, R14, R15     Working Registers             */
*     * /********************************************************/
VWWATTR  EQU   *
*        CALL  ISPLINK,(VDEFINE,DWWATTR,WWATTR,CHAR,WWATTRL),VL
         LA#ST VDEFINE,PALP1       Service   pointer
         LA#ST DWWATTR,PALP2       Var Name  pointer
         LA#ST WWATTR,PALP3        Var Value pointer
         LA#ST CHAR,PALP4          Var Type  pointer
         LA#ST WWATTRL,VARPTR      Init VARPTR with Var Value Len
         LA#ST VARPTR,PALP5        VARPTR    pointer
         MVI   PALP5,B'10000000' Last Parm designation
         LA    R1,PALIST           R1 = Parm List
         L     R15,ISPLINK         Link to ISPLINK
         BALR  R14,R15               via BALR
         ST    R15,SAVER15         SAVE R15
         LTR   R15,R15             Successful invokation?
         BNZ   ERR0020             No, ISPLINK VDEFINE error
*
         EJECT
*     * /********************************************************/
*     * /* Store &HHMMSS Variable for ISPF                      */
*     * /* - R0, R1, R14, R15     Working Registers             */
*     * /********************************************************/
VHHMMSS  EQU   *
*        CALL  ISPLINK,(VDEFINE,DHHMMSS,WWHMS,CHAR,HHMMSSL),VL
         LA#ST VDEFINE,PALP1       Service   pointer
         LA#ST DHHMMSS,PALP2       Var Name  pointer
         LA#ST WWHMS,PALP3         Var Value pointer
         LA#ST CHAR,PALP4          Var Type  pointer
         LA#ST WWHMSL,VARPTR       Init VARPTR with Var Value Len
         LA#ST VARPTR,PALP5        VARPTR    pointer
         MVI   PALP5,B'10000000' Last Parm designation
         LA    R1,PALIST           R1 = Parm List
         L     R15,ISPLINK         Link to ISPLINK
         BALR  R14,R15               via BALR
         ST    R15,SAVER15         SAVE R15
         LTR   R15,R15             Successful invokation?
         BNZ   ERR0020             No, ISPLINK VDEFINE error
*
         EJECT
*     * /********************************************************/
*     * /* DISPLAY World WATCH panel using ISPF                 */
*     * /* - R0, R1, R14, R15     Working Registers             */
*     * /********************************************************/
*     * /* On ENTER (RC=0), update and refresh display          */
*     * /* PF3      (RC=8), exit and return to TSO/ISPF         */
*     * /* PF4      (RC=8), exit and return to TSO/ISPF         */
*     * /********************************************************/
DISPLY   EQU   *
*        CALL  ISPLINK,(DISPLAY,PWW),VL
         LA#ST DISPLAY,PALP1
         LA#ST PWW,PALP2
         MVI   PALP2,B'10000000' Last Parm designation
         LA    R1,PALIST           R1 = Parm List
         L     R15,ISPLINK         Link to ISPLINK
         BALR  R14,R15               via BALR
         ST    R15,SAVER15         SAVE R15
         LTR   R15,R15             Successful invokation?
         BZ    UPDTME              Yes, update display
         C     R15,=F'8'           Is it END/RETURN?
         BE    ERR0000             Yes, EXIT
         B     ERR0021             No, must be an error!
UPDTME   EQU   *
         BAL   R14,TZLINE          Get time and update timezone line
         B     DISPLY              Refresh display
*                                  No, must be an error!
         EJECT
*     * /********************************************************/
*     * /* VDEFINE Error                                        */
*     * /* - R0, R1, R15     Working Registers                  */
*     * /********************************************************/
ERR0020  EQU   *
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
         TPUT  ERR0020M,L'ERR0020M DEFINE Error msg
         LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
         L     R15,=F'20'          RC = 20
         B     ERRXIT
*
*     * /********************************************************/
*     * /* DISPLAY Error                                        */
*     * /* - R0, R1, R15     Working Registers                  */
*     * /********************************************************/
ERR0021  EQU   *
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
         TPUT  ERR0021M,L'ERR0021M DISPLAY Error msg
         LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
         L     R15,=F'21'          RC = 20
         B     ERRXIT
*
*     * /********************************************************/
*     * /* ISPLINK  not found error                             */
*     * /* - R0, R1, R15     Working Registers                  */
*     * /********************************************************/
ERR4097  EQU   *
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
         TPUT  ERR4097M,L'ERR4097M ISPLINK not found msg
         LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
         L     R15,=F'4097'        RC = 4097
         B     ERRXIT
*
*     * /********************************************************/
*     * /* RC=0 Exit Point                                      */
*     * /********************************************************/
ERR0000  EQU   *
         LA    R15,0               RC = 0 return to caller
ERRXIT   EQU   *
         ST    R15,SAVER15         SAVE R15
*
         TITLE 'WRLDWTCH: - - Exit and Return to caller               '
*     * /********************************************************/
*     * /* Exit, return to caller                               */
*     * /********************************************************/
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
         FREEMAIN  R,LV=WSAREAL,A=(R1) Free Storage
*     * /--------------------------------------------------------/
*     * /* Restore caller registers and return to caller        */
*     * /--------------------------------------------------------/
         LR    R15,R5              R15 = RC for exit
NOMAINS  EQU   *
RETURN   EQU   *
         L     R14,12(R13)         Restore R14
         LM    R0,R12,20(R13)      Restore R0 thru R12
         BR    R14                 Return to caller
*
         TITLE 'WRLDWTCH: - - - -   S U B - R O U T I N E S'
*     * /********************************************************/
*     * /* Determine HH for each time zone for M.21 (WWDATA21)  */
*     * /* - R0, R1, R2, R5  Working Registers                  */
*     * /* - R6, R7, R8      Working Registers                  */
*     * /* - R14 used for BAL return......                      */
*     * /* REXX Statements:                                     */
*     * /* TIME0 = TIME(M) % 60 - GMT                           */
*     * /* DO I = 1 TO 24                                       */
*     * /*   TIME = TIME0 + I                                   */
*     * /*   IF TIME <   0 THEN TIME = TIME + 24                */
*     * /*   IF TIME >= 24 THEN TIME = TIME - 24                */
*     * /*   WWDATA20 = WWDATA20 LEFT(TIME,2,' ')               */
*     * /* END                                                  */
*     * /********************************************************/
*     * /* TIME(M) % 60            Minutes from midnight % 60   */
*     * /*   representative as HH below                         */
TZLINE   EQU   *
         TIME  DEC                 Time in R0  HH_MM_SS_TH
*                                  Date in R1  00_YY_DD_DC
         ST    R0,FW               SAVE TIME
         UNPK  TIMEOUT(7),FW(4)    0HHMMSS   24-hr clock format
         MVC   WWHMSHH,TIMEHH      HH
         MVI   WWHMSC1,C':'        :
         MVC   WWHMSMM,TIMEMM      MM
         MVI   WWHMSC2,C':'        :
         MVC   WWHMSSS,TIMESS      SS
         OI    WWHMS+7,X'F0'
*     * /* TIME0 = HH + GMT  (GMT can be + or - value)          */
         PACK  HH,TIMEHH           Pack HH
         ZAP   TIME0,HH            TIME0 = HH
         AP    TIME0,GMT                 + GMT
         ZAP   DW,TIME0            DW<-TIME0     PACKED
         CVB   R6,DW               R6<-DW(TIME0) BINARY
         LR    R2,R6               R2<-R6(TIME0) Saved TIME0
*
         EJECT
         LA    R5,24               R5 is loop control
         LA    R7,1                R7 is I
         LA    R8,WWDATA           Address of WWDATA
         LA    R8,WWDATA21(R8)     Address of WWDATA21
*     * /* DO I = 1 TO 24                                       */
ITER8A   EQU   *
*     * /*   TIME = TIME0 + I                                   */
         LR    R6,R2               Restore R6   TIME = TIME0
         AR    R6,R7                                 + I
IF#1     EQU   *
*     * /*   IF TIME <   0 THEN TIME = TIME + 24                */
         C     R6,=F'0'
         BNL   IF#2
         A     R6,=F'24'           TIME + 24
IF#2     EQU   *
*     * /*   IF TIME >= 24 THEN TIME = TIME - 24                */
         C     R6,=F'24'
         BL    CONT1
         S     R6,=F'24'           TIME - 24
*     * /*   WWDATA20 = WWDATA20 LEFT(TIME,2,' ')               */
CONT1    EQU   *
         CVD   R6,DW               TIME to DW
         OI    DW+7,X'0F'          Restore sign
         UNPK  TZONEHHH,DW+6(2)    Unpack TIME
         MVC   0(2,R8),TZONEHHH+1  Display as timezone HH
         MVI   2(R8),C' '          Blank pos 3 of timezone
         LA    R7,1(R7)            Bump up I
         LA    R8,3(R8)            Bump up WWDATA21 position
         BCT   R5,ITER8A           DO R5 times
*
         BR    R14                 Return to caller
*
         TITLE 'WRLDWTCH: - - Literal Pool                            '
         LTORG
*
         TITLE 'WRLDWTCH: - - Constants                               '
*
*     * /********************************************************/
*     * /* Error Messages                                       */
*     * /********************************************************/
ERR0020M DC    C'WRLDWTCH 0020 -VDEFINE error'
ERR0021M DC    C'WRLDWTCH 0021 -DISPLAY error'
ERR4097M DC    C'WRLDWTCH 4097 -Cannot link to ISPLINK'
*
         TITLE 'WRLDWTCH: - - World Watch MAP                         '
*     * /********************************************************/
*     * /* World Watch MAP Data for dynamic area                */
*     * /* REXX Statements:                                     */
*     * /* M.1 ='...   .......                                  */
*     * /* .                                                    */
*     * /* M.20='...   .......                                  */
*     * /********************************************************/
M        EQU   *
M01      DC C'...   ...   ...   ... HH###HHH#..   ...   ...   ...   '
         DC C'...   ...   ...   '
M02      DC C'...   ...   ...H H#..  H###HHH#..   ...   ...   ... HH'
         DC C'##.   ...   ...   '
M03      DC C'...   ... HH### HH###   ###HHH...   ...HH ...   ###HHH'
         DC C'###HHH###H  ...   '
M04      DC C'..#HHH###HHH###HHH#.#H  ###H  .#.   .##HHH###HHH###HHH'
         DC C'###HHH###HHH###HH '
M05      DC C'...HHH.##HHH###HH .##   .#.   ...   ##.HHH###HHH###HHH'
         DC C'###HHH###HH ##.   '
M06      DC C'...   ...HHH###HHH###HH ...   ...HH ###HHH###HHH###HHH'
         DC C'###HHH##.  H...   '
M07      DC C'...   ... HH###HHH###H H...   ... HH###HHH###HHH###HHH'
         DC C'###HHH##.   ...   '
M08      DC C'...   ...  H###HHH##.   ...   ...HH .##HHH##.HHH###HHH'
         DC C'###HHH..#   ...   '
M09      DC C'...   ...   ###HHH#..   ...   ...HHH#..  H###HHH###HHH'
         DC C'###H  .#.   ...   '
M10      DC C'...   ...   .##H  ...   ...   ..#HHH###HHH###H H###HHH'
         DC C'###H  ...   ...   '
M11      DC C'...   ...   ...HHH...   ...   .##HHH###HHH###   .##  H'
         DC C'##.   ...   ...   '
M12      DC C'...   ...   ...   .##HH ...   ..#HHH###HHH##.   .#.   '
         DC C'...   ...   ...   '
M13      DC C'...   ...   ...   .##HHH#..   ...   ###HHH#..   ...   '
         DC C'#.#  H.##   ...   '
M14      DC C'...   ...   ...   .##HHH####  ...   .##HHH...   ...   '
         DC C'...   ...   ...   '
M15      DC C'...   ...   ...   ...HHH###   ...   .##HHH.#.   ...   '
         DC C'... HH###   ...   '
M16      DC C'...   ...   ...   ...HHH#..   ...   .##HH ...   ...   '
         DC C'...HHH###H  ...   '
M17      DC C'...   ...   ...   ..#HHH...   ...   ..#H  ...   ...   '
         DC C'...HH .##H  ...   '
M18      DC C'...   ...   ...   ..#H  ...   ...   ...   ...   ...   '
         DC C'...   ..#   ..#   '
M19      DC C'...   ...   ...   ..#H  ...   ...   ...   ...   ...   '
         DC C'...   ...   ...   '
M20      DC C'...   ...   ...   ...H  ...   ...   ...   ...   ...   '
         DC C'...   ...   ...   '
M21      DC C'...   ...   ...   ...   ...   ...   ...   ...   ...   '
         DC C'...   ...   ...   '
ML       EQU   *-M                 World Map area length
MLL      EQU   *-M21               Line length
ML#      EQU   ML/MLL              Number of lines
WWDATA09 EQU   M09-M               Displacement -M9
WWDATA21 EQU   M21-M               Displacement -M21
*
         EJECT
*     * /********************************************************/
*     * /* ISPF Service Constants                               */
*     * /********************************************************/
VDEFINE  DC    CL8'VDEFINE '       ISPF Service Keyword
DISPLAY  DC    CL8'DISPLAY '       ISPF Service Keyword
CHAR     DC    CL8'CHAR    '       ISPF Service Keyword
*
*     * /********************************************************/
*     * /* Misc Constants                                       */
*     * /********************************************************/
DWWDATA  DC    CL8'WWDATA  '       VarName for WWDATA
DWWATTR  DC    CL8'WWATTR  '       VarName for WWATTR
DHHMMSS  DC    CL8'HHMMSS  '       VarName for HHMMSS
PWW      DC    CL8'PWW     '       ISPF Panel World WATCH
*
         TITLE 'WRLDWTCH: - - Equates                                 '
*     * /********************************************************/
*     * /* E Q U A T E S                                        */
*     * /********************************************************/
*
*
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
         TITLE 'WRLDWTCH: - - Working Storage                         '
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
FW       DS    F                   Full Word   area
DW       DS    D                   Double Word area
CURTIME  DS    F                   Current Time as Packed
TIMEOUT  DS    0CL7                Current Time as Character
TIMEHH   DS    CL2                 Current Time HH
TIMEMM   DS    CL2                 Current Time MM
TIMESS   DS    CL2                 Current Time SS
         DS    CL1
HH       DS    PL2                 HH in 24hr format
GMT      DS    PL2                 GMT value for location
TIME0    DS    PL2                 TIME0  00-24 (HH 24-hour)
TZONEHHH DS    CL3                 TIME as 0HH
VARPTR   DS    A(0)                Var Pointer work
*
         EJECT
*     * /********************************************************/
*     * /* World Watch Time                                     */
*     * /********************************************************/
WWHMS    DS    0CL8                TIME as HH:MM:SS
WWHMSHH  DS    CL2                    HH
WWHMSC1  DS    CL1                    :
WWHMSMM  DS    CL2                    MM
WWHMSC2  DS    CL1                    :
WWHMSSS  DS    CL2                    SS
WWHMSL   EQU   *-WWHMS             Length of WWHMS
*
*     * /********************************************************/
*     * /* World Watch Data Area                                */
*     * /********************************************************/
WWDATA   DS 21CL72
WWDATAL  EQU   *-WWDATA
*
*     * /********************************************************/
*     * /* World Watch Shadow Area                              */
*     * /********************************************************/
WWATTR   DS 21CL72
WWATTRL  EQU   *-WWATTR
*
*     * /********************************************************/
*     * /* Storage for Entry Points                             */
*     * /********************************************************/
ISPLINK  DS    F                   Entry point for ISPLINK
*
*     * /********************************************************/
*     * /* Parm Address List                                    */
*     * /********************************************************/
PALIST   DS    0F
PALP1    DS    A                   Parm Addr 1
PALP2    DS    A                   Parm Addr 2
PALP3    DS    A                   Parm Addr 3
PALP4    DS    A                   Parm Addr 4
PALP5    DS    A                   Parm Addr 5
PALP6    DS    A                   Parm Addr 6
PALP7    DS    A                   Parm Addr 7
*     *
*     * /********************************************************/
*     * /* End of Working Storage                               */
*     * /********************************************************/
         DS    0D
WSAREAE  EQU   *                   Working Storage END
WSAREAL  EQU   WSAREAE-WSAREA      Working Storage Area Length
*
         END
@@
//CLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.CLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CWW
/********************************************************************/
/*                                                                  */
/* CLIST: CWW                                                       */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/WRLDWTCH-in-MVS38J             */
/*         Copyright (C) 2018 Larry Belmontes, Jr.                  */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 CLIST to invoke WRLDWTCH and display panel PWW.         */
/*          This CLIST simulates the REXX / ISPF World Clock        */
/*          application from Marvin Shaw (CBT Tape file 366).       */
/*                                                                  */
/* PANELS:   PWW                                                    */
/*                                                                  */
/* CPs:      WRLDWTCH                                               */
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
/* 09/22/2018 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/

PROC 0                                /* CWW                        */

CONTROL MSG NOFLUSH

/* ---------------------------------------------------------------- */
/* ISPEXEC CONTROL, use Diagnose Panel on ERROR                     */
/* ---------------------------------------------------------------- */
ISPEXEC CONTROL ERRORS CANCEL

/* ---------------------------------------------------------------- */
/* Call WRLDWTCH                                                    */
/* ---------------------------------------------------------------- */
WRLDWTCH

/* ---------------------------------------------------------------- */
/* End of PROC                                                      */
/* ---------------------------------------------------------------- */
DONE: +
END
@@
//PLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.PLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=PWW
/********************************************************************/
/*                                                                  */
/*    PANEL: PWW                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/WRLDWTCH-in-MVS38J             */
/*         Copyright (C) 2018 Larry Belmontes, Jr.                  */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 World WATCH panel for WRLDWTCH                          */
/*                                                                  */
/*                                                                  */
/* Modifications in reference to original panel on CBT File #366    */
/*                                                                  */
/* - Removed DEFAULT(%+_) from )ATTR section header                 */
/* - Removed CMD(ZCMD) SMSG(MSG) LMSG(MSG) from )BODY section header*/
/* - Modified panel TITLE line                                      */
/* - Removed "MSG from panel line 3                                 */
/* - Removed all @WWDATAnn,WWATTRnn   ...  @ area definitions and   */
/*   replaced with a first line   @WWDATA,WWATTR   ... @            */
/*   and remaining lines as       @                    @            */
/*   for a total of 21 lines for AREA                               */
/* - Replace HELP screen w/ HWW on .HELP statement in INIT section  */
/* - Removed last line, before )INIT section to use World Map       */
/*   line 21                                                        */
/* - Removed extra blank lines in )INIT and )REINIT sections        */
/* - Added 4 CHAR attributes using visible characters               */
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
/* 09/22/2018 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR

  ' TYPE(OUTPUT) COLOR(BLUE)  INTENS(LOW)  CAPS(OFF)
  " TYPE(OUTPUT) COLOR(WHITE) INTENS(HIGH) CAPS(OFF)

  @ AREA(DYNAMIC)
 01 TYPE(DATAIN)
 02 TYPE(DATAOUT)
 03 TYPE(CHAR)   COLOR(GREEN)
 04 TYPE(CHAR)   COLOR(GREEN) HILITE(REVERSE)
 05 TYPE(CHAR)   COLOR(BLUE)
 06 TYPE(CHAR)   COLOR(BLUE)  HILITE(REVERSE)

 [  TYPE(CHAR)   COLOR(GREEN)
 [  TYPE(CHAR)   COLOR(GREEN) HILITE(REVERSE)
 {  TYPE(CHAR)   COLOR(BLUE)
 }  TYPE(CHAR)   COLOR(BLUE)  HILITE(REVERSE)

)BODY WIDTH(&ZSCREENW) EXPAND(;;)
%                           + World WATCH     'DATE     'HHMMSS                +
%===>_ZCMD               +                                                     +
+                                                                              +
+@WWDATA,WWATTR                                                         @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
+@                                                                      @
)INIT

  .CURSOR = ZCMD
  .HELP   = HWW

  &DATE   = '&ZDAY..&ZMONTH..&ZYEAR'

)REINIT

  REFRESH(*)

)END
./ ADD NAME=HWW
/********************************************************************/
/*                                                                  */
/*    PANEL: HWW                                                    */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/WRLDWTCH-in-MVS38J             */
/*         Copyright (C) 2018 Larry Belmontes, Jr.                  */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 Help panel for PWW                                      */
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
/* 09/22/2018 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ~ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 ? TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(BLUE)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(LEFT)
 [ TYPE(OUTPUT) INTENS(HIGH) JUST(RIGHT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-Tutorial-                 ? World WATCH      &ZDATE    &ZTIME              %--
%OPTION  ===>_ZCMD                                                             +
%                                                                              +
% The World WATCH panel displays a world map with an hourly time zone line at
% the bottom of the map relative to current time zone.
%                                                                              +
% For example, if the local time is 17:14 in Dallas,TX, each time zone in the
%              World Map is listed from GMT-11 to GMT+4 as shown below:
%                                                                              +
%?12+13?14+15?16+17?18+19?20+21?22+23?00+01?02+03?04+05?06+07?08+09?10+11
%                 *
%                /
%   Dallas-------
%                                                                              +
% This application was converted to IFOX00 Assembler and ISPF 2.0 (ISPF-like
% product from Wally Mclaughlin).  The original World Clock program was
% contributed by Marvin Shaw as a REXX/ISPF application on CBT File 366.
?
%                                                                              +
)INIT
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY

)REINIT

)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY

)END
@@
