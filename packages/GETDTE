//GETDTE JOB (JOB),
//             'INSTALL GETDTE',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//MACLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=&&MACLIBS,UNIT=SYSDA,
//             DISP=(,PASS,DELETE),
//             SPACE=(TRK,(04,02,02)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=GETDTE
         TITLE 'GETDTE - Get Date-Time-Env Utility                    '
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  ==================================================================
*
*   GGGGG  EEEEEEE TTTTTTT DDDDD   TTTTTTT EEEEEEE
*  GG   GG EE        TT    DD   D    TT    EE
*  GG      EE        TT    DD   DD   TT    EE
*  GGGGGGG EEEEEE    TT    DD   DD   TT    EEEEEE
*  GG   GG EE        TT    DD   DD   TT    EE
*  GG   GG EE        TT    DD   D    TT    EE
*   GGGGG  EEEEEEE   TT    DDDDD     TT    EEEEEEE
*
*  ==================================================================
*
*
*  Program: GETDTE
*
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/GETDTE-in-MVS38J
*           Copyright (C) 2013-2020  Larry Belmontes, Jr.
*
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
*
         EJECT
*  Overview:
*  ================================================================
*
*     GETDTE is a utility used to obtain current date-time and
*  environment information.  This subroutine requires a work area
*  passed from the calling program.
*
*     GETDTE is written in IFOX00 and uses the TIME macro to format
*  date-time results.  Various MVS control blocks are referenced
*  for environment results based on the executing address space.
*
*     Results are returned to the caller via the provided commarea
*  and/or CLIST variable, RDTEANSR.
*
*     Based on how GETDTE is called, results are returned as listed
*  below:
*
*     1) If GETDTE is called from higher-level lanaguage (i.e. COBOL)
*        with a commarea, results are returned in the commarea.
*
*     2) If GETDTE is called from assembler (i.e. IFOX00) with a
*        commarea, results are returned in the commarea.
*
*     3) If GETDTE is called from TSO (foreground or background)
*        with a commarea, results are returned in commarea and CLIST
*        variable, RDTEANSR.
*        Note:  If IKJCT441 is not located, no CLIST variable is
*               created and return code set to 8.  Commarea contains
*               valid data.
*
*     4) If GETDTE is called from TSO (foreground or background)
*        with NO commarea, results are returned ONLY in CLIST
*        variable, RDTEANSR.
*        Note:  If IKJCT441 is not located, no CLIST variable is
*               created and return code set to 8.  No data is
*               returned in the commarea.
*
*     5) If GETDTE is called from BATCH (EXEC PGM=GETDTE),
*        job step will most likely abend with S0C4!
*        -- NOTE: INVALID use scenerio
*
*     6) If GETDTE is called from BATCH (EXEC PGM=GETDTE,PARM='xyz..'),
*        job step will most likely abend with S0C4!
*        -- NOTE: INVALID use scenerio
*
         EJECT
*  Overview: (continued)
*  ================================================================
*
*     Representation of date-time-environment result data (commarea):
*
*     0        1         2         3         4
*     123456789012345678901234567890123456789012345
*     +---+----+----+----+----+----+----+----+----+
*     12/31/2001.365 10:11:12.45December 1Monday
*     MM/DD/CCYY.JJJ HH:MM:SS:THLLLLLLLLLNWWWWWWWWW
*
*     4   5         6         7         8
*     67890123456789012345678901234567890
*     +---+----+----+----+----+----+----+
*     LARRY01 IKJACCNTTSOLOGONTSOFGISPARM
*     Jobname StepnameProcstepEnvirParmty
*
*     Result Data DSECT (80 bytes):
*
*     $DTECOMA EQU   *              GETDTE Results
*     RDATE    DS    0CL14          MM/DD/CCYY.JJJ
*     RDATEMM  DS    CL2            MM   Month
*     RDATES1  DS    CL1            Separator
*     RDATEDD  DS    CL2            DD   Day
*     RDATES2  DS    CL1            Separator
*     RDATECC  DS    CL2            CC   Century
*     RDATEYY  DS    CL2            YY   Year
*     RDATES3  DS    CL1            Separator
*     RDATEJJJ DS    CL3            JJJ  Julian
*     RDATES4  DS    CL1            Separator
*     RTIME    DS    0CL11          HH:MM:SS:TT
*     RTIMEHH  DS    CL2            HH   Hour
*     RTIMES1  DS    CL1            Separator
*     RTIMEMM  DS    CL2            MM   Minutes
*     RTIMES2  DS    CL1            Separator
*     RTIMESS  DS    CL2            SS   Seconds
*     RTIMES3  DS    CL1            Separator
*     RTIMETT  DS    CL2            TT   Hundredths
*     RMONTHN  DS    CL9            Month Name
*     RDAY#    DS    CL1            Weekday Number 0-Sun to 6-Sat
*     RDAYN    DS    CL9            Weekday
*     RJOBNAM  DS    CL8            Job Name   / TSO UserID
*     RSTEPNAM DS    CL8            Step Name  / Logon Step name
*     RPROCSTP DS    CL8            Proc Step Name  / Logon PROC
*     $DTECOME EQU   *
*     $DTECOML EQU   $DETCOME-$DTECOMA  Length of Result Area
*
         EJECT
*  Overview: (continued)
*  ================================================================
*
*     GETDTE uses a variable-length (VL) address parameter list as
*  the data interface (as depicted below) and is passed via R1 before
*  calling GETDTE.
*
*     DTPARML  DS    0F             GETDTE Parameter List
*     DTPARM1  DS    A              Address of Results Area (commarea)
*
*     After storing the address of Results Area into DTPARM1 (see
*  above), store an X'80' in the high-order bit of DTPARM1 using
*  this instruction  OI  DTPARM1,X'80'  or equivalent.
*
*     The CALL macro can also be used to invoke GETDTE as follows:
*     ST    Rx,DTPARM1              Store addrs of commarea
*     CALL  GETDTE,(DTPARML),VL=1   Invoke GETDTE
*
*     GETDTE can be called from other languages such as COBOL as
*  shown below:
*
*     CALL 'GETDTE' USING GETDTE-RESULTS.
*
*     Note: COBOL will automatically add the VL marker during the
*  compilation process of the CALL statement.
*
*     GETDTE-RESULTS is a working-storage area in the COBOL calling
*  program.  A copybook, RDTECOMC, is included in the ASM PDS for
*  use by COBOL programs.  Below is a sample COBOL working-storage
*  definition:
*
*      01  GETDTE-RESULTS.
*          05  RDATE.
*              10  RDATEMM       PIC X(02).
*              10  RDATES1       PIC X(01).
*              10  RDATEDD       PIC X(02).
*              10  RDATES2       PIC X(01).
*              10  RDATECC       PIC X(02).
*              10  RDATEYY       PIC X(02).
*              10  RDATES3       PIC X(01).
*              10  RDATEJJJ      PIC X(03).
*          05  RDATES4           PIC X(01).
*          05  RTIME.
*              10  RTIMEHH       PIC X(02).
*              10  RTIMES1       PIC X(01).
*              10  RTIMEMM       PIC X(02).
*              10  RTIMES2       PIC X(01).
*              10  RTIMESS       PIC X(02).
*              10  RTIMES3       PIC X(01).
*              10  RTIMETT       PIC X(02).
*          05  RMONTH-NAME       PIC X(09).
*          05  RDAY-NUM          PIC X(01).
*          05  RDAY-NAME         PIC X(09).
*          05  RJOB-NAME         PIC X(08).
*          05  RSTEP-NAME        PIC X(08).
*          05  RPROC-STEP-NAME   PIC X(08).
*
*     GETDTE can be invoked as a command in a CLIST and results are
*  returned in a CLIST variable, RDTEANSR.
*
*
*
*
*
*
*  Enjoy GETDTE!
*  Larry Belmontes Jr.
*
*
         EJECT
*  Prerequisite: User Modifications
*  ===================================================================
*
*     Two user-mods, ZP60014 and ZP60038, are REQUIRED to process
*  CLIST symbolic variables via the IKJCT441 API on MVS 3.8J before
*  using GETDTE.  Otherwise, GETDTE will function but will NOT
*  create CLIST variables.
*
*     More information on the above user-mods can be obtained from
*  the following website:
*       http://www.prycroft6.com.au/vs2mods/
*
*     Check your system to determine if one or both user-mods are
*  required.  ZP60038 requires ZP60014.
*
*
         EJECT
*  GETDTE Programs / Services:
*  ==================================================================
*
*    1) GETMAIN/FREEMAIN          Working storage
*    2) TIME                      Macro
*    3) IKJCT441                  User Modification (see prereqs)
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
*  |  00  |  Successful                                              |
*  |  01  |  No PARM provided                                        |
*  |  02  |  Error in IKJCT441, no CLIST var created                 |
*  |  08  |  IKJCT441 not found, no CLIST var created                |
*  +------+----------------------------------------------------------+
*
         EJECT
*  Macros and SYSLIB Location:
*  ==================================================================
*
*  Macro     Description                         Library
*  --------  ----------------------------------  ------------------
*  YREGS     Register Equates                    SYS2.MACLIB
*  RDTECOMA  GETDTE Commarea                     GETDTE.V1R0M00.ASM
*  RDTECOMC  GETDTE Commarea (COBOL copybook)    GETDTE.V1R0M00.ASM
*
*
*  References:
*  ==================================================================
*
*  GC28-0683-3 OS/VS2 MVS Supervisor Services and Macros Rel 3.7
*
*
         EJECT
*  Change History: <CHGHIST>
*  ==================================================================
*
*  MM/DD/CCYY Version  Name / Description
*  ---------- -------  --------------------------------------------
*
*  09/26/2013 0.0.00   Larry Belmontes Jr.
*                      - Initial version written to obtain date-time
*                        using TIME macro as a general utility
*
*                      Larry Belmontes Jr.
*                      - Enhanced to include Gregorian date
*                        including Day of Week, Day Name and
*                        Month Name.
*
*                      Larry Belmontes Jr.
*                      - Enhanced to include environment information
*                        such as TSO or BATCH, and type of PARM.
*
*                      Larry Belmontes Jr.
*                      - Enhanced to include job name, proc name and
*                        proc step name to environment information.
*
*
*  07/30/2020 1.0.00   Larry Belmontes Jr.
*                      - Initial version released to MVS 3.8J
*                        hobbyist public domain
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
GETDTE   CSECT
         USING GETDTE,R10,R11,R12 my BASE REGISTER(S)
         PRINT NOGEN
*
*     * /********************************************************/
*     * /* Save regs and declare base registers R10,R11,R12     */
*     * /* including saving R1 (PARMS) in R2                    */
*     * /********************************************************/
         STM   R14,R12,12(R13)     Save calling program registers
         LR    R10,R15             Load base1 w R15
         LA    R11,4095(R10)       Load base2 (R11) w/ offset
         LA    R11,1(R11)          Adjust base2
         LA    R12,4095(R11)       Load base3 (R12) w/ offset
         LA    R12,1(R12)          Adjust base3
         LR    R15,R13             Save callers registers
         LR    R2,R1               Save PARM addr
*
*     * /********************************************************/
*     * /* Program Eyecatcher                                   */
*     * /********************************************************/
         B     OVRSTAMP            Branch past eye-catcher
MYSTAMP  EQU   *
PGMID    DC    CL8'GETDTE  '       My Program STAMP
         DC    CL8'MVS3.8J '       OS
PGMVRM   DC    CL8'V1.0.00 '       .Version
         DC    CL8'07302020'       ..date  MMDDCCYY
         DC    CL1' '
PGMDS    DC    CL8'&SYSDATE'       Assemble Date
         DC    CL3' - '            .and
PGMTS    DC    CL8'&SYSTIME'       ..Time
         DC    C'Copyright (C) 2013-2020'
         DC    C'Larry Belmontes, Jr. '
         DC    C'https://ShareABitofIT.net/GETDTE-in-MVS38J'
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
         ST    R2,MYCOMMA          Store Addr of Parm Address List
*
*     * /********************************************************/
*     * /* Initialization of variables                          */
*     * /********************************************************/
         MVC   RCEXIT,=F'0'        Initialize RCEXIT to zero
         MVI   $DTECOMS,C'?'       Initialize Answer Area
         MVC   $DTECOMS+1($DTECOML-1),$DTECOMS
*
         EJECT
*     * /********************************************************/
*     * /* Determine PARM type based on high-order bit          */
*     * /* - R3, R4, R5   Working Register                      */
*     * /********************************************************/
CHKPARM  EQU   *
         TM    0(R2),X'80'         PARM or CPPL?
         BZ    ISCPPL              CPPL...
ISPARM   EQU   *                   PARM...
*        WTO   'ISPARM'
         MVC   RPARMTY,=C'ISPARM'
         LA    R2,0(,R2)           Clear high order bit
         L     R3,0(,R2)           YES. Load addr of commarea
         LTR   R3,R3               Do we have a commarea?
         BZ    NOPARM              NO.
         B     CHKPARMX            YES, continue
ISCPPL   EQU   *                   CPPL...
*        WTO   'ISCPPL'
         MVC   RPARMTY,=C'ISCPPL'
         B     NOPARM              Do not use commarea
NOPARM   EQU   *
*        WTO   'NOPARM'
         XC    MYCOMMA,MYCOMMA     Clear
         B     CHKPARMX
CHKPARMX EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Set Environment Information                          */
*     * /* - R4, R5       Working Register                      */
*     * /********************************************************/
SETENV   EQU   *
*
*     * /--------------------------------------------------------/
*     * /* Get JOB information by following CVT-TCB-TIOT        */
*     * /--------------------------------------------------------/
         L     R5,16               Point to CVT
         L     R5,0(,R5)           Point to TCBs
         L     R5,4(,R5)           Point to Current TCB
         L     R5,12(,R5)          Point to TIOT  TCBTIO
         MVC   RJOBNAM,0(R5)       ...from TIOCNJOB
         MVC   RSTEPNAM,8(R5)      ...from TIOCSTEP
         MVC   RPROCSTP,16(R5)     ...from TIOCSTEP+8
*
*     * /--------------------------------------------------------/
*     * /* Get ENV information by following CVT-TCB-JSCP        */
*     * /--------------------------------------------------------/
         L     R5,CVTPTR           Point to CVT
         USING CVTMAP,R5           Addressability CVT
         L     R5,CVTTCBP          Point to TCBs
         L     R5,4(,R5)           Point to current TCB
         DROP  R5                  Drop Addressibility
         USING TCB,R5              Addressability TCB
         L     R5,TCBJSCB          JSCB Addr
         DROP  R5                  Drop Addressibility
         USING IEZJSCB,R5          Addressability JSCB
         L     R5,JSCBPSCB         PSCB Addr
         LTR   R5,R5               TSO?
         BZ    NOTTSO              NO, BATCH...
*                                  YES...
         USING PSA,0
         L     R4,PSAAOLD          R4=Current ASCB Address
         ICM   R4,15,ASCBTSB-ASCB(R4)
         BNZ   TSOFG
         B     TSOBG
TSOFG    EQU   *                   Running as TSO FOREGROUND
*        WTO   'TSOFG'
         MVC   RENV,=C'TSOFG'
         B     SETENVX
TSOBG    EQU   *                   Running as TSO BACKGROUND
*        WTO   'TSOBG'
         MVC   RENV,=C'TSOBG'
         B     SETENVX
NOTTSO   EQU   *                   Running as BATCH (NOT TSO)
*        WTO   'BATCH'
         MVC   RENV,=C'BATCH'
         CLC   MYCOMMA,=X'00000000'  If set to no commarea,
         BNE   NOTTSO1               ...set RC = 1
         MVC   RCEXIT,=F'1'
NOTTSO1  EQU   *
         XC    IKJCT441,IKJCT441   Clear, no CLIST VAR to create
         B     CHK441X
SETENVX  EQU   *
*
*     * /********************************************************/
*     * /* Load IKJCT441 Entry Point                            */
*     * /********************************************************/
CHK441   EQU   *
         XC    IKJCT441,IKJCT441   Clear
         LOAD  EP=IKJCT441,ERRET=NOTF441
         ST    R0,IKJCT441         Load and Save IKJCT441 Entry Point
         B     CHK441X
NOTF441  EQU   *
         MVC   RCEXIT,=F'8'        RC=8, IKJCT441 not found
CHK441X  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Initialize seperators   'MM/DD/CCYY.JJJ HH:MM:SS.TT' */
*     * /********************************************************/
*        WTO   'INIT'
         MVC   RDATES1,DATESEP
         MVC   RDATES2,DATESEP
         MVC   RDATES3,PERIOD
         MVC   RDATES4,BLANK
         MVC   RTIMES1,TIMESEP
         MVC   RTIMES2,TIMESEP
         MVC   RTIMES3,PERIOD
*
*     * /********************************************************/
*     * /* Issue TIME Macro        Time in R0  HH_MM_SS_TH      */
*     * /*                         Date in R1  00_YY_DD_DC      */
*     * /********************************************************/
         TIME  DEC                 Get Time/Date
*
*     * /********************************************************/
*     * /* Unpack TIME from R0                                  */
*     * /********************************************************/
         ST    R0,WORKAREA         Time in R0  HH_MM_SS_TH
         MVO   DW(5),WORKAREA(4)   Pack via MVO into 5 bytes
         OI    DW+4,X'0F'          ... with an F zone
         UNPK  UNPKAREA(9),DW(5)   Unpack 9 bytes 0HHMMSSTH
         MVC   RTIMEHH,UNPKAREA+01      HH
         MVC   RTIMEMM,UNPKAREA+03      MM
         MVC   RTIMESS,UNPKAREA+05      SS
         MVC   RTIMETT,UNPKAREA+07      TT
*
*     * /********************************************************/
*     * /* Unpack DATE from R1                                  */
*     * /********************************************************/
         ST    R1,WORKAREA         Date in R1  00_YY_DD_DC
         AP    WORKAREA(4),=P'1900000'  Adj for Y2K
         UNPK  UNPKAREA(7),WORKAREA(4)  Unpack CC_YY_DD_DC
         OI    UNPKAREA+6,X'F0'    F-zone
         MVC   RDATECC(04),UNPKAREA      CCYY
         MVC   RDATEJJJ,UNPKAREA+4       JJJ
*
         EJECT
*     * /********************************************************/
*     * /* Determine if CCYY is a leap year                     */
*     * /********************************************************/
*        WTO   'ISLPY'
         BAL   R5,S#ISLPY          Determine Leap Year
*
*     * /********************************************************/
*     * /* Determine MM and DD from JJJ                         */
*     * /********************************************************/
         PACK  PKJJJ,RDATEJJJ
         BAL   R5,S#J2MD           Determine MM DD from JJJ
         MVC   RDATEMM,DCMMM+1     Set MM
         MVC   RDATEDD,DCDDD+1     Set DD
         MVC   RMONTHN,0(R4)       Set Month Name
*
*     * /********************************************************/
*     * /* Call S#CDOW to calculate DOW                         */
*     * /********************************************************/
         BAL   R5,S#CDOW           Calc DOW
         MVC   RDAY#,DCCDOW        Set DOW
         MVC   RDAYN,DCCWDAY       Set DOW Name
*
*        TPUT  RDATE,$DTECOML
*        TPUT  RENV,5
*
*     * /********************************************************/
*     * /* Return data to caller                                */
*     * /********************************************************/
*        WTO   'GET MYCOMMA'
         L     R2,MYCOMMA          Get PARM Address List
         LA    R2,0(,R2)           Clear high order bit      ****
         LTR   R2,R2               Is it set?
         BZ    CONTZ               No, do not pass back answer
         L     R2,0(,R2)           Get PARM1 (callers results area)
         MVC   0($DTECOML,R2),$DTECOMS       Move results
*        WTO   'RETURNED RDTE DATA'
CONTZ    EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Create RDTEANSR variable                             */
*     * /********************************************************/
         L     R1,IKJCT441         Do we have a LOAD address
         LTR   R1,R1                ...for IKJCT441?
         BZ    MYCVARX             NO, bypass CLIST var creation
MYCVAR   EQU   *                   YES, create CLIST variable
*        WTO   'CR8 VAR RDTEANSR'
         LA    R0,ECUPDT           Entry Code
         ST    R0,CT441P1
         LA    R0,PDTEANSR         Name Pointer
         ST    R0,CT441P2
         LA    R0,LDTEANSR         Name Length
         ST    R0,CT441P3
         LA    R0,$DTECOMS         Var  Pointer
         ST    R0,FW
         LA    R0,FW
         ST    R0,CT441P4
*        ST    R0,VALP1
*        LA    R0,VALP1
*        ST    R0,CT441P4
         LA    R0,LDTRSLT          Var  Length
         ST    R0,CT441P5
         LA    R0,TOKEN            Token
         ST    R0,CT441P6
         MVI   CT441P6,B'10000000' Last Parm designation
*
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441                                     */
*     * /--------------------------------------------------------/
         LA    R1,CT441LST         R1 = Parm List
         L     R15,IKJCT441
         BALR  R14,R15
*        ST    R15,RC#441          Save R15 from IKJCT441
         ST    R15,RCEXIT          Save R15 from IKJCT441
*        LTR   R15,R15             Successful invokation?
*        BZ    MYCVARX             YES, branch to MYCVARX
*        MVC   RCEXIT,=F'2'        NO, Set RC=2, IKJCT441 Error
*        B     MYCVARX
MYCVARX  EQU   *
*        WTO   'END VAR RDTEANSR'
*
         EJECT
*     * /********************************************************/
*     * /* Program Exit                                         */
*     * /* - Restore savearea, FREEMAIN, and                    */
*     * /*   return to caller                                   */
*     * /********************************************************/
MYEXIT   EQU   *
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
         TITLE 'GETDTE - Subroutines                                  '
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
         ST    R5,SAVER5S
         ZAP   DC#LEAP,=P'0'       Assume, not leap year
         ZAP   DC#YDAYS,=P'365'    Assume, 365 days in year
         PACK  DC#YR4,RDATECC(4)   Seed YR4 with  CCYY
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
         L     R5,SAVER5S
         BR    R5                  Return to caller (R5)
*
        EJECT
*     * /********************************************************/
*     * /* Subroutine - Determine MM DD from JJJ         (R5)   */
*     * /--------------------------------------------------------/
*     * /* Determine MM and DD from JJJ                         */
*     * /* -         R4=addr of MONTHNAME                       */
*     * /* -         R6=Length of MONTHNAME                     */
*     * /--------------------------------------------------------/
*     * /* - R3, R4, R5, R6           Working Register          */
*     * /********************************************************/
S#J2MD   EQU    *
         ST    R5,SAVER5S
*     * /--------------------------------------------------------/
*     * /* Search for MM using JJJ                              */
*     * /--------------------------------------------------------/
         LA    R4,MMTABLEE         R4=End Address of MonthTable
         USING MPT,R4              Use MPT DSECT
         LA    R5,MMTBLEN          R3=Length of table entry
         SR    R4,R5               R4=Address of Last MonthTable Entry
         LA    R3,MMTBLN           R3=12 Months
GETMMJJJ EQU   *
         PACK  FW(2),MPTDAT@1      Pack start month days from table
         CLC   MPTMM,=C'02'        JAN or FEB?
         BNH   NOADJLY             Yes, bypass adjust for leap year
         AP    FW(2),DC#LEAP       No, adjust for leap year when MM >1
NOADJLY  EQU   *
         CP    FW(2),PKJJJ         MM JJ < JJJ
         BL    FNDMM               Yes, found month table entry MM
         SR    R4,R5               No, Point to prev MonthTable Entry
         BCT   R3,GETMMJJJ         Check again...
****     something wrong if this executes
         DC    X'0000'
*     * /--------------------------------------------------------/
*     * /* Found MM                                             */
*     * /--------------------------------------------------------/
FNDMM    EQU   *
         MVI   DCMMM,C'0'          Save MM from
         MVC   DCMMM+1(2),MPTMM    ... table lookup
         PACK  DC#TRM,MPTDWTRM     Pack TERM for DOW calculation
         SP    PKJJJ,FW(2)         Compute DD for MM from JJJ
         UNPK  DCDDD,PKJJJ         Save DD
         OI    DCDDD+2,X'F0'       F-Zone
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Load R4 and R6 with MONTHNAME and Length of MONTHNAME*/
*     * /--------------------------------------------------------/
         LA    R4,MPTMNAME         Point to MONTHNAME
         LA    R6,MPTMNL           R6=Length of MONTHNAME
         DROP  R4                  Tell Assembler
*
S#J2MDX  EQU    *
         L     R5,SAVER5S
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
         ST    R5,SAVER5S
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
         PACK  DC#YR4,RDATECC(4)   Pack 0CCYY
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
         LA    R4,DOWTABLE         R4=Address of DOWTABLE
         LA    R6,DOWTBLN          R6=Number of days
DO#DOWSR EQU   *
         CLC   0(1,R4),DCCDOW      Match DOW ?
         BE    DO#DOWFD            Yes, use Week Day Name
         LA    R4,DOWTBLL(R4)      No, look at next DOWTABLE entry
         BCT   R6,DO#DOWSR         Check again
DO#DOWFD EQU   *
         MVC   DCCWDAY(DOWTBLL-1),1(R4)  Get Weekday name
S#CDOWX  EQU   *
         L     R5,SAVER5S
         BR    R5                  Return to caller (R5)
*
         TITLE 'GETDTE - Month and Day Reference Tables               '
*     * /********************************************************/
*     * /* Month Parameter Table                                */
*     * /********************************************************/
MMTABLE  EQU   *
*                +----------------- 2 C   Month Number
*                | +--------------- 2 C   Days in month
*                | | +------------- 3 C   Days at start of month
*                | | |  +---------- 1 C   Term 0,3,2,5,0,3,5,1,4,6,2,4
*                | | |  |+--------- 9 C   Monthname
*                | | |  ||
         DC    C'01310000January  '
MMTBLEN  EQU   *-MMTABLE                  Length of table entry
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
MMTABLEE EQU   *
MMTBLN   EQU   (*-MMTABLE)/MMTBLEN        Number of table entries
*
*     * /********************************************************/
*     * /* Day of Week Table                                    */
*     * /********************************************************/
DOWTABLE EQU   *
*                +----------------- 1 C   Weekday Number
*                |+---------------- 9 C   Weekday name
*                ||
         DC    C'0Sunday   '
DOWTBLL  EQU   *-DOWTABLE                 Length of table entry
         DC    C'1Monday   '
         DC    C'2Tuesday  '
         DC    C'3Wednesday'
         DC    C'4Thursday '
         DC    C'5Friday   '
         DC    C'6Saturday '
DOWTBLN  EQU   (*-DOWTABLE)/DOWTBLL       Number of table entries
*
         TITLE 'GETDTE - Constants                                    '
*     * /********************************************************/
*     * /* Misc Constants                                       */
*     * /********************************************************/
BLANK    DC    C' '                Blank (space)
PERIOD   DC    C'.'                Period
DATESEP  DC    C'/'                Date Seperator
TIMESEP  DC    C':'                Time Seperator
*
RDTEANSR DC    C'RDTEANSR'         Date-Time-Env CLIST VarName
PDTEANSR DC    A(RDTEANSR)         Addr VAR NAME
LDTEANSR DC    A(L'RDTEANSR)       Addr VAR NAME LENGTH
LDTRSLT  DC    A($DTECOML)         Addr VAR DATA LENGTH
ECUPDT   DC    A(TSVEUPDT)         Addr ENTRY CODE FOR UPDATE/CREATE
TSVEUPDT EQU   2                   Update/Create Request
TOKEN    DC    F'0'                Token used by IKJCT441
*
*
         TITLE 'GETDTE - Equates                                      '
*     * /********************************************************/
*     * /* E Q U A T E S                                        */
*     * /********************************************************/
*     *
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
         TITLE 'GETDTE - System DSECTs                                '
*     *
*     * /********************************************************/
*     * /* System DSECTs                                        */
*     * /********************************************************/
         IHAPSA                    Prefixed Save Area
PSALEN   EQU   *-PSA               Length of PSA
         EJECT
         IHAASCB                   Address Space Control Block
ASCBLEN  EQU   *-ASCB              Length of ASCB
         EJECT
         CVT   DSECT=YES,LIST=NO   Communication Vector Table
         EJECT
         IKJTCB DSECT=YES,LIST=NO  Task Control Block
         EJECT
TIOT     DSECT
         IEFTIOT1                  Task Input/Output Table
TIOTLEN  EQU   *-TIOT              Length of TIOT
         EJECT
JSCB     DSECT
         IEZJSCB                   Job/Step Control Block
         EJECT
*     *
*     * /********************************************************/
*     * /* Month Parameter Table DSECT                          */
*     * /********************************************************/
MPT      DSECT                     Month Parameter Table
MPTMM    DS    CL2                 Month
MPTMMAXD DS    CL2                 Month Max Days
MPTDAT@1 DS    CL3                 Days before 1st of Month
MPTDWTRM DS    CL1                 DOW Term (0,3,2,5,0,3,5,1,4,6,2,4)
MPTMNB   EQU   *                   Start of Month Name
MPTMNAME DS    CL9                 Month Name
MPTMNL   EQU   *-MPTMNAME          Length of Month Name
MPTLE    EQU   *-MPT               Length of Entry
*
         TITLE 'GETDTE - Working Storage Variables                    '
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
*
         EJECT
*     * /********************************************************/
*     * /* Program Variables                                    */
*     * /********************************************************/
SAVER5S  DS    F                   R5  Save in Subroutines
DW       DS    D                   Double Word area
FW       DS    F                   Full Word   area
RCEXIT   DS    F                   Exit RC
MYCOMMA  DS    F                   Addr of Parm Address List
WORKAREA DS    CL8                 Workarea
UNPKAREA DS    CL16                Workarea for unpacking data
*
DC#LEAP  DS    PL1                 Leap Year (0-No, 1-Yes)
DC#YDAYS DS    PL2                 Days in Year (365 or 366)
DC#DD    DS    PL2                 DD
DC#YR4   DS    PL4                 CCYY / 4
DC#YR100 DS    PL4                 CCYY / 100
DC#YR400 DS    PL4                 CCYY / 400
DC#DOW   DS    PL4                 Day of Week
PKJJJ    DS    PL3                 Workarea pack JJJ
DCMMM    DS    CL3                 0MM   character area
DCDDD    DS    CL3                 0DD   character area
DCCDOW   DS    CL1                 Day of Week character area
DCCWDAY  DS    CL9                 Weekday character area
DC#TRM   DS    PL1                 DOW Term
*
         EJECT
         RDTECOMA                  GETDTE Commarea
*
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
*
VALP1    DC    A(0)                VAR VALUE POINTER
         EJECT
*
WSAREAE  EQU   *                   Working Storage END
WSAREAL  EQU   WSAREAE-WSAREA      Working Storage Area Length
         EJECT
         END   GETDTE
./ ADD NAME=RDTECOMA
         MACRO
&NAME    RDTECOMA
         PUSH  PRINT
         PRINT GEN
**********************************************************************
*   Macro:  RDETCOMA  V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
**********************************************************************
*     Date-Time-Environment Communications Area                      *
**********************************************************************
*                                                                    *
*     Representation of date-time-environment result data:           *
*                                                                    *
*     0        1         2         3         4                       *
*     123456789012345678901234567890123456789012345                  *
*     +---+----+----+----+----+----+----+----+----+                  *
*     12/31/2001.365 10:11:12:45December 1Monday                     *
*     MM/DD/CCYY.JJJ HH:MM:SS:THLLLLLLLLLNWWWWWWWWW                  *
*                                                                    *
*     4   5         6         7         8                            *
*     67890123456789012345678901234567890                            *
*     +---+----+----+----+----+----+----+                            *
*     LARRY01 ILKJACCTTSOLOGONTSOFGISPARM                            *
*     Jobname StepnameProcstepEnvirParmty                            *
*                                                                    *
*     NOTE:  This macro should be in sync with                       *
*     -----  COBOL copybook, RDTECOMC.                               *
*                                                                    *
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
$DTECOMS EQU   *
RDATE    DS    0CL14           'MM/DD/CCYY.JJJ'
RDATEMM  DS    CL2             MM
RDATES1  DS    CL1             Separator
RDATEDD  DS    CL2             DD
RDATES2  DS    CL1             Separator
RDATECC  DS    CL2             CC
RDATEYY  DS    CL2             YY
RDATES3  DS    CL1             Separator
RDATEJJJ DS    CL3             JJJ
RDATES4  DS    CL1             Blank
RTIME    DS    0CL11           'HH:MM:SS:TT'
RTIMEHH  DS    CL2             HH
RTIMES1  DS    CL1             Separator
RTIMEMM  DS    CL2             MM
RTIMES2  DS    CL1             Separator
RTIMESS  DS    CL2             SS
RTIMES3  DS    CL1             Separator
RTIMETT  DS    CL2             TT
RMONTHN  DS    CL9             Month Name     i.e. January
RDAY#    DS    CL1             Weekday Number 0-Sunday to 6-Saturday
RDAYN    DS    CL9             Weekday        i.e  Tuesday
RJOBNAM  DS    CL8             JOB Name / UserID
RSTEPNAM DS    CL8             Step Name / Logon PROC
RPROCSTP DS    CL8             Proc Step Name / TSO Logon PROC
RENV     DS    CL5             Environment Type
*                              TSOFG - TSO Foreground
*                              TSOBG - TSO Background (run in batch)
*                              BATCH - Not TSO, must be Batch
RPARMTY  DS    CL6             PARM Type
*                              ISPARM - Parm
*                              ISCPPL - Parm
*                              ISPARM  Parm
$DTECOME EQU   *
$DTECOML EQU   $DTECOME-$DTECOMS     Length of Result Area
*
*              END OF RDTECOMA
         POP   PRINT
         MEND
./ ADD NAME=RDTECOMC
       01  RDTECOM.
      * /************************************************************/
      * /*Copybook: RDTECOMC  (COBOL)     V1R0M00    07/30/2020     */
      * /*  Author: Larry Belmontes, Jr.                            */
      * /*  https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j    */
      * /*  Copyright (C) 2020-2021  Larry Belmontes, Jr.           */
      * /************************************************************/
      * /************************************************************/
      * /* Date-Time-Environment Communications Area                */
      * /************************************************************/
      * /* Representation of date-time-environment result data:     */
      * /*                                                          */
      * /* 0        1         2         3         4                 */
      * /* 123456789012345678901234567890123456789012345            */
      * /* +---+----+----+----+----+----+----+----+----+            */
      * /* 12/31/2001.365 10:11:12:45December 1Monday               */
      * /* MM/DD/CCYY.JJJ HH:MM:SS:THLLLLLLLLLNWWWWWWWWW            */
      * /*                                                          */
      * /* 4   5         6         7         8                      */
      * /* 67890123456789012345678901234567890                      */
      * /* +---+----+----+----+----+----+----+                      */
      * /* LARRY01 ILKJACCTTSOLOGONTSOFGISPARM                      */
      * /* Jobname StepnameProcstepEnvirParmty                      */
      * /*                                                          */
      * /*                                                          */
      * /* NOTE:  This copybook should be in sync with              */
      * /* -----  assembler version, RDTECOMA.                      */
      * /*                                                          */
      * /************************************************************/
      * /* Disclaimer:                                              */
      * /* ======================================================== */
      * /*                                                          */
      * /*    No guarantee; No warranty; Install / Use at your own  */
      * /* risk.                                                    */
      * /*                                                          */
      * /*    This software is provided "AS IS" and without any     */
      * /* expressed  or implied warranties, including, without     */
      * /* limitation, the implied warranties of merchantability    */
      * /* and fitness for a particular purpose.                    */
      * /*                                                          */
      * /*    The author requests keeping authors name intact in    */
      * /* any modified versions.                                   */
      * /*                                                          */
      * /*    In addition, the author requests readers to submit    */
      * /* any code modifications / enhancements and associated     */
      * /* comments for consideration into a subsequent release     */
      * /* (giving credit to contributor(s)) thus, improving        */
      * /* overall functionality and further benefiting the         */
      * /* MVS 3.8J hobbyist public domain community.               */
      * /*                                                          */
      * /*                                                          */
           05  RDATE.
               10  RDATEMM       PIC X(02).
               10  RDATES1       PIC X(01).
               10  RDATEDD       PIC X(02).
               10  RDATES2       PIC X(01).
               10  RDATECC       PIC X(02).
               10  RDATEYY       PIC X(02).
               10  RDATES3       PIC X(01).
               10  RDATEJJJ      PIC X(03).
           05  RDATES4           PIC X(01).
           05  RTIME.
               10  RTIMEHH       PIC X(02).
               10  RTIMES1       PIC X(01).
               10  RTIMEMM       PIC X(02).
               10  RTIMES2       PIC X(01).
               10  RTIMESS       PIC X(02).
               10  RTIMES3       PIC X(01).
               10  RTIMETT       PIC X(02).
           05  RMONTHN           PIC X(09).
           05  RDAYNUM           PIC X(01).
           05  RDAYN             PIC X(09).
           05  RJOBNAM           PIC X(08).
           05  RSTEPNAM          PIC X(08).
           05  RPROCSTP          PIC X(08).
           05  RENV              PIC X(05).
      *                        TSOFG - TSO Foreground
      *                        TSOBG - TSO Background (run in batch)
      *                        BATCH - Not TSO, must be Batch
           05  RPARMTY           PIC X(06).
      *                        ISPARM - Parm
      *                        ISCPPL - Parm
      *                        ISPARM  Parm
@@
//* -------------------------------------------------------*
//* *  GETDTE for MVS3.8J TSO / Hercules                   *
//* *                                                      *
//* *  JOB: $INST04                                        *
//* *       Install GETDTE Program                         *
//* *                                                      *
//* *  - Programs install          to SYS2.LINKLIB         *
//* *  - Install libraries marked...                       *
//* *    - Search for '<--TARGET'                          *
//* *    - Update install libraries per your               *
//* *      installation standard                           *
//* *                                                      *
//* -------------------------------------------------------*
//ASML     PROC MBR=WHOWHAT
//ASM      EXEC PGM=IFOX00,
//             PARM='NODECK,LOAD,RENT,TERM,XREF'
//SYSGO    DD  DSN=&&LOADSET,DISP=(MOD,PASS),SPACE=(CYL,(1,1)),
//             UNIT=VIO,DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//         DD  DSN=SYS2.MACLIB,DISP=SHR          ** YREG  **
//         DD  DSN=&&MACLIBS,DISP=OLD   ** RDTECOMA **
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
//SYSLMOD  DD  DSN=SYS2.LINKLIB(&MBR),DISP=SHR     <--TARGET
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(5,2))
//SYSIN    DD  DUMMY
//         PEND
//*
//* -------------------------------------------------------*
//* *                                                      *
//* *  Assemble GETDTE                                     *
//* *                                                      *
//* -------------------------------------------------------*
//GETDTE   EXEC  ASML,MBR=GETDTE
//ASM.SYSIN DD DATA,DLM=@@
         TITLE 'GETDTE - Get Date-Time-Env Utility                    '
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  ==================================================================
*
*   GGGGG  EEEEEEE TTTTTTT DDDDD   TTTTTTT EEEEEEE
*  GG   GG EE        TT    DD   D    TT    EE
*  GG      EE        TT    DD   DD   TT    EE
*  GGGGGGG EEEEEE    TT    DD   DD   TT    EEEEEE
*  GG   GG EE        TT    DD   DD   TT    EE
*  GG   GG EE        TT    DD   D    TT    EE
*   GGGGG  EEEEEEE   TT    DDDDD     TT    EEEEEEE
*
*  ==================================================================
*
*
*  Program: GETDTE
*
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/GETDTE-in-MVS38J
*           Copyright (C) 2013-2020  Larry Belmontes, Jr.
*
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
*
         EJECT
*  Overview:
*  ================================================================
*
*     GETDTE is a utility used to obtain current date-time and
*  environment information.  This subroutine requires a work area
*  passed from the calling program.
*
*     GETDTE is written in IFOX00 and uses the TIME macro to format
*  date-time results.  Various MVS control blocks are referenced
*  for environment results based on the executing address space.
*
*     Results are returned to the caller via the provided commarea
*  and/or CLIST variable, RDTEANSR.
*
*     Based on how GETDTE is called, results are returned as listed
*  below:
*
*     1) If GETDTE is called from higher-level lanaguage (i.e. COBOL)
*        with a commarea, results are returned in the commarea.
*
*     2) If GETDTE is called from assembler (i.e. IFOX00) with a
*        commarea, results are returned in the commarea.
*
*     3) If GETDTE is called from TSO (foreground or background)
*        with a commarea, results are returned in commarea and CLIST
*        variable, RDTEANSR.
*        Note:  If IKJCT441 is not located, no CLIST variable is
*               created and return code set to 8.  Commarea contains
*               valid data.
*
*     4) If GETDTE is called from TSO (foreground or background)
*        with NO commarea, results are returned ONLY in CLIST
*        variable, RDTEANSR.
*        Note:  If IKJCT441 is not located, no CLIST variable is
*               created and return code set to 8.  No data is
*               returned in the commarea.
*
*     5) If GETDTE is called from BATCH (EXEC PGM=GETDTE),
*        job step will most likely abend with S0C4!
*        -- NOTE: INVALID use scenerio
*
*     6) If GETDTE is called from BATCH (EXEC PGM=GETDTE,PARM='xyz..'),
*        job step will most likely abend with S0C4!
*        -- NOTE: INVALID use scenerio
*
         EJECT
*  Overview: (continued)
*  ================================================================
*
*     Representation of date-time-environment result data (commarea):
*
*     0        1         2         3         4
*     123456789012345678901234567890123456789012345
*     +---+----+----+----+----+----+----+----+----+
*     12/31/2001.365 10:11:12.45December 1Monday
*     MM/DD/CCYY.JJJ HH:MM:SS:THLLLLLLLLLNWWWWWWWWW
*
*     4   5         6         7         8
*     67890123456789012345678901234567890
*     +---+----+----+----+----+----+----+
*     LARRY01 IKJACCNTTSOLOGONTSOFGISPARM
*     Jobname StepnameProcstepEnvirParmty
*
*     Result Data DSECT (80 bytes):
*
*     $DTECOMA EQU   *              GETDTE Results
*     RDATE    DS    0CL14          MM/DD/CCYY.JJJ
*     RDATEMM  DS    CL2            MM   Month
*     RDATES1  DS    CL1            Separator
*     RDATEDD  DS    CL2            DD   Day
*     RDATES2  DS    CL1            Separator
*     RDATECC  DS    CL2            CC   Century
*     RDATEYY  DS    CL2            YY   Year
*     RDATES3  DS    CL1            Separator
*     RDATEJJJ DS    CL3            JJJ  Julian
*     RDATES4  DS    CL1            Separator
*     RTIME    DS    0CL11          HH:MM:SS:TT
*     RTIMEHH  DS    CL2            HH   Hour
*     RTIMES1  DS    CL1            Separator
*     RTIMEMM  DS    CL2            MM   Minutes
*     RTIMES2  DS    CL1            Separator
*     RTIMESS  DS    CL2            SS   Seconds
*     RTIMES3  DS    CL1            Separator
*     RTIMETT  DS    CL2            TT   Hundredths
*     RMONTHN  DS    CL9            Month Name
*     RDAY#    DS    CL1            Weekday Number 0-Sun to 6-Sat
*     RDAYN    DS    CL9            Weekday
*     RJOBNAM  DS    CL8            Job Name   / TSO UserID
*     RSTEPNAM DS    CL8            Step Name  / Logon Step name
*     RPROCSTP DS    CL8            Proc Step Name  / Logon PROC
*     $DTECOME EQU   *
*     $DTECOML EQU   $DETCOME-$DTECOMA  Length of Result Area
*
         EJECT
*  Overview: (continued)
*  ================================================================
*
*     GETDTE uses a variable-length (VL) address parameter list as
*  the data interface (as depicted below) and is passed via R1 before
*  calling GETDTE.
*
*     DTPARML  DS    0F             GETDTE Parameter List
*     DTPARM1  DS    A              Address of Results Area (commarea)
*
*     After storing the address of Results Area into DTPARM1 (see
*  above), store an X'80' in the high-order bit of DTPARM1 using
*  this instruction  OI  DTPARM1,X'80'  or equivalent.
*
*     The CALL macro can also be used to invoke GETDTE as follows:
*     ST    Rx,DTPARM1              Store addrs of commarea
*     CALL  GETDTE,(DTPARML),VL=1   Invoke GETDTE
*
*     GETDTE can be called from other languages such as COBOL as
*  shown below:
*
*     CALL 'GETDTE' USING GETDTE-RESULTS.
*
*     Note: COBOL will automatically add the VL marker during the
*  compilation process of the CALL statement.
*
*     GETDTE-RESULTS is a working-storage area in the COBOL calling
*  program.  A copybook, RDTECOMC, is included in the ASM PDS for
*  use by COBOL programs.  Below is a sample COBOL working-storage
*  definition:
*
*      01  GETDTE-RESULTS.
*          05  RDATE.
*              10  RDATEMM       PIC X(02).
*              10  RDATES1       PIC X(01).
*              10  RDATEDD       PIC X(02).
*              10  RDATES2       PIC X(01).
*              10  RDATECC       PIC X(02).
*              10  RDATEYY       PIC X(02).
*              10  RDATES3       PIC X(01).
*              10  RDATEJJJ      PIC X(03).
*          05  RDATES4           PIC X(01).
*          05  RTIME.
*              10  RTIMEHH       PIC X(02).
*              10  RTIMES1       PIC X(01).
*              10  RTIMEMM       PIC X(02).
*              10  RTIMES2       PIC X(01).
*              10  RTIMESS       PIC X(02).
*              10  RTIMES3       PIC X(01).
*              10  RTIMETT       PIC X(02).
*          05  RMONTH-NAME       PIC X(09).
*          05  RDAY-NUM          PIC X(01).
*          05  RDAY-NAME         PIC X(09).
*          05  RJOB-NAME         PIC X(08).
*          05  RSTEP-NAME        PIC X(08).
*          05  RPROC-STEP-NAME   PIC X(08).
*
*     GETDTE can be invoked as a command in a CLIST and results are
*  returned in a CLIST variable, RDTEANSR.
*
*
*
*
*
*
*  Enjoy GETDTE!
*  Larry Belmontes Jr.
*
*
         EJECT
*  Prerequisite: User Modifications
*  ===================================================================
*
*     Two user-mods, ZP60014 and ZP60038, are REQUIRED to process
*  CLIST symbolic variables via the IKJCT441 API on MVS 3.8J before
*  using GETDTE.  Otherwise, GETDTE will function but will NOT
*  create CLIST variables.
*
*     More information on the above user-mods can be obtained from
*  the following website:
*       http://www.prycroft6.com.au/vs2mods/
*
*     Check your system to determine if one or both user-mods are
*  required.  ZP60038 requires ZP60014.
*
*
         EJECT
*  GETDTE Programs / Services:
*  ==================================================================
*
*    1) GETMAIN/FREEMAIN          Working storage
*    2) TIME                      Macro
*    3) IKJCT441                  User Modification (see prereqs)
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
*  |  00  |  Successful                                              |
*  |  01  |  No PARM provided                                        |
*  |  02  |  Error in IKJCT441, no CLIST var created                 |
*  |  08  |  IKJCT441 not found, no CLIST var created                |
*  +------+----------------------------------------------------------+
*
         EJECT
*  Macros and SYSLIB Location:
*  ==================================================================
*
*  Macro     Description                         Library
*  --------  ----------------------------------  ------------------
*  YREGS     Register Equates                    SYS2.MACLIB
*  RDTECOMA  GETDTE Commarea                     GETDTE.V1R0M00.ASM
*  RDTECOMC  GETDTE Commarea (COBOL copybook)    GETDTE.V1R0M00.ASM
*
*
*  References:
*  ==================================================================
*
*  GC28-0683-3 OS/VS2 MVS Supervisor Services and Macros Rel 3.7
*
*
         EJECT
*  Change History: <CHGHIST>
*  ==================================================================
*
*  MM/DD/CCYY Version  Name / Description
*  ---------- -------  --------------------------------------------
*
*  09/26/2013 0.0.00   Larry Belmontes Jr.
*                      - Initial version written to obtain date-time
*                        using TIME macro as a general utility
*
*                      Larry Belmontes Jr.
*                      - Enhanced to include Gregorian date
*                        including Day of Week, Day Name and
*                        Month Name.
*
*                      Larry Belmontes Jr.
*                      - Enhanced to include environment information
*                        such as TSO or BATCH, and type of PARM.
*
*                      Larry Belmontes Jr.
*                      - Enhanced to include job name, proc name and
*                        proc step name to environment information.
*
*
*  07/30/2020 1.0.00   Larry Belmontes Jr.
*                      - Initial version released to MVS 3.8J
*                        hobbyist public domain
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
GETDTE   CSECT
         USING GETDTE,R10,R11,R12 my BASE REGISTER(S)
         PRINT NOGEN
*
*     * /********************************************************/
*     * /* Save regs and declare base registers R10,R11,R12     */
*     * /* including saving R1 (PARMS) in R2                    */
*     * /********************************************************/
         STM   R14,R12,12(R13)     Save calling program registers
         LR    R10,R15             Load base1 w R15
         LA    R11,4095(R10)       Load base2 (R11) w/ offset
         LA    R11,1(R11)          Adjust base2
         LA    R12,4095(R11)       Load base3 (R12) w/ offset
         LA    R12,1(R12)          Adjust base3
         LR    R15,R13             Save callers registers
         LR    R2,R1               Save PARM addr
*
*     * /********************************************************/
*     * /* Program Eyecatcher                                   */
*     * /********************************************************/
         B     OVRSTAMP            Branch past eye-catcher
MYSTAMP  EQU   *
PGMID    DC    CL8'GETDTE  '       My Program STAMP
         DC    CL8'MVS3.8J '       OS
PGMVRM   DC    CL8'V1.0.00 '       .Version
         DC    CL8'07302020'       ..date  MMDDCCYY
         DC    CL1' '
PGMDS    DC    CL8'&SYSDATE'       Assemble Date
         DC    CL3' - '            .and
PGMTS    DC    CL8'&SYSTIME'       ..Time
         DC    C'Copyright (C) 2013-2020'
         DC    C'Larry Belmontes, Jr. '
         DC    C'https://ShareABitofIT.net/GETDTE-in-MVS38J'
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
         ST    R2,MYCOMMA          Store Addr of Parm Address List
*
*     * /********************************************************/
*     * /* Initialization of variables                          */
*     * /********************************************************/
         MVC   RCEXIT,=F'0'        Initialize RCEXIT to zero
         MVI   $DTECOMS,C'?'       Initialize Answer Area
         MVC   $DTECOMS+1($DTECOML-1),$DTECOMS
*
         EJECT
*     * /********************************************************/
*     * /* Determine PARM type based on high-order bit          */
*     * /* - R3, R4, R5   Working Register                      */
*     * /********************************************************/
CHKPARM  EQU   *
         TM    0(R2),X'80'         PARM or CPPL?
         BZ    ISCPPL              CPPL...
ISPARM   EQU   *                   PARM...
*        WTO   'ISPARM'
         MVC   RPARMTY,=C'ISPARM'
         LA    R2,0(,R2)           Clear high order bit
         L     R3,0(,R2)           YES. Load addr of commarea
         LTR   R3,R3               Do we have a commarea?
         BZ    NOPARM              NO.
         B     CHKPARMX            YES, continue
ISCPPL   EQU   *                   CPPL...
*        WTO   'ISCPPL'
         MVC   RPARMTY,=C'ISCPPL'
         B     NOPARM              Do not use commarea
NOPARM   EQU   *
*        WTO   'NOPARM'
         XC    MYCOMMA,MYCOMMA     Clear
         B     CHKPARMX
CHKPARMX EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Set Environment Information                          */
*     * /* - R4, R5       Working Register                      */
*     * /********************************************************/
SETENV   EQU   *
*
*     * /--------------------------------------------------------/
*     * /* Get JOB information by following CVT-TCB-TIOT        */
*     * /--------------------------------------------------------/
         L     R5,16               Point to CVT
         L     R5,0(,R5)           Point to TCBs
         L     R5,4(,R5)           Point to Current TCB
         L     R5,12(,R5)          Point to TIOT  TCBTIO
         MVC   RJOBNAM,0(R5)       ...from TIOCNJOB
         MVC   RSTEPNAM,8(R5)      ...from TIOCSTEP
         MVC   RPROCSTP,16(R5)     ...from TIOCSTEP+8
*
*     * /--------------------------------------------------------/
*     * /* Get ENV information by following CVT-TCB-JSCP        */
*     * /--------------------------------------------------------/
         L     R5,CVTPTR           Point to CVT
         USING CVTMAP,R5           Addressability CVT
         L     R5,CVTTCBP          Point to TCBs
         L     R5,4(,R5)           Point to current TCB
         DROP  R5                  Drop Addressibility
         USING TCB,R5              Addressability TCB
         L     R5,TCBJSCB          JSCB Addr
         DROP  R5                  Drop Addressibility
         USING IEZJSCB,R5          Addressability JSCB
         L     R5,JSCBPSCB         PSCB Addr
         LTR   R5,R5               TSO?
         BZ    NOTTSO              NO, BATCH...
*                                  YES...
         USING PSA,0
         L     R4,PSAAOLD          R4=Current ASCB Address
         ICM   R4,15,ASCBTSB-ASCB(R4)
         BNZ   TSOFG
         B     TSOBG
TSOFG    EQU   *                   Running as TSO FOREGROUND
*        WTO   'TSOFG'
         MVC   RENV,=C'TSOFG'
         B     SETENVX
TSOBG    EQU   *                   Running as TSO BACKGROUND
*        WTO   'TSOBG'
         MVC   RENV,=C'TSOBG'
         B     SETENVX
NOTTSO   EQU   *                   Running as BATCH (NOT TSO)
*        WTO   'BATCH'
         MVC   RENV,=C'BATCH'
         CLC   MYCOMMA,=X'00000000'  If set to no commarea,
         BNE   NOTTSO1               ...set RC = 1
         MVC   RCEXIT,=F'1'
NOTTSO1  EQU   *
         XC    IKJCT441,IKJCT441   Clear, no CLIST VAR to create
         B     CHK441X
SETENVX  EQU   *
*
*     * /********************************************************/
*     * /* Load IKJCT441 Entry Point                            */
*     * /********************************************************/
CHK441   EQU   *
         XC    IKJCT441,IKJCT441   Clear
         LOAD  EP=IKJCT441,ERRET=NOTF441
         ST    R0,IKJCT441         Load and Save IKJCT441 Entry Point
         B     CHK441X
NOTF441  EQU   *
         MVC   RCEXIT,=F'8'        RC=8, IKJCT441 not found
CHK441X  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Initialize seperators   'MM/DD/CCYY.JJJ HH:MM:SS.TT' */
*     * /********************************************************/
*        WTO   'INIT'
         MVC   RDATES1,DATESEP
         MVC   RDATES2,DATESEP
         MVC   RDATES3,PERIOD
         MVC   RDATES4,BLANK
         MVC   RTIMES1,TIMESEP
         MVC   RTIMES2,TIMESEP
         MVC   RTIMES3,PERIOD
*
*     * /********************************************************/
*     * /* Issue TIME Macro        Time in R0  HH_MM_SS_TH      */
*     * /*                         Date in R1  00_YY_DD_DC      */
*     * /********************************************************/
         TIME  DEC                 Get Time/Date
*
*     * /********************************************************/
*     * /* Unpack TIME from R0                                  */
*     * /********************************************************/
         ST    R0,WORKAREA         Time in R0  HH_MM_SS_TH
         MVO   DW(5),WORKAREA(4)   Pack via MVO into 5 bytes
         OI    DW+4,X'0F'          ... with an F zone
         UNPK  UNPKAREA(9),DW(5)   Unpack 9 bytes 0HHMMSSTH
         MVC   RTIMEHH,UNPKAREA+01      HH
         MVC   RTIMEMM,UNPKAREA+03      MM
         MVC   RTIMESS,UNPKAREA+05      SS
         MVC   RTIMETT,UNPKAREA+07      TT
*
*     * /********************************************************/
*     * /* Unpack DATE from R1                                  */
*     * /********************************************************/
         ST    R1,WORKAREA         Date in R1  00_YY_DD_DC
         AP    WORKAREA(4),=P'1900000'  Adj for Y2K
         UNPK  UNPKAREA(7),WORKAREA(4)  Unpack CC_YY_DD_DC
         OI    UNPKAREA+6,X'F0'    F-zone
         MVC   RDATECC(04),UNPKAREA      CCYY
         MVC   RDATEJJJ,UNPKAREA+4       JJJ
*
         EJECT
*     * /********************************************************/
*     * /* Determine if CCYY is a leap year                     */
*     * /********************************************************/
*        WTO   'ISLPY'
         BAL   R5,S#ISLPY          Determine Leap Year
*
*     * /********************************************************/
*     * /* Determine MM and DD from JJJ                         */
*     * /********************************************************/
         PACK  PKJJJ,RDATEJJJ
         BAL   R5,S#J2MD           Determine MM DD from JJJ
         MVC   RDATEMM,DCMMM+1     Set MM
         MVC   RDATEDD,DCDDD+1     Set DD
         MVC   RMONTHN,0(R4)       Set Month Name
*
*     * /********************************************************/
*     * /* Call S#CDOW to calculate DOW                         */
*     * /********************************************************/
         BAL   R5,S#CDOW           Calc DOW
         MVC   RDAY#,DCCDOW        Set DOW
         MVC   RDAYN,DCCWDAY       Set DOW Name
*
*        TPUT  RDATE,$DTECOML
*        TPUT  RENV,5
*
*     * /********************************************************/
*     * /* Return data to caller                                */
*     * /********************************************************/
*        WTO   'GET MYCOMMA'
         L     R2,MYCOMMA          Get PARM Address List
         LA    R2,0(,R2)           Clear high order bit      ****
         LTR   R2,R2               Is it set?
         BZ    CONTZ               No, do not pass back answer
         L     R2,0(,R2)           Get PARM1 (callers results area)
         MVC   0($DTECOML,R2),$DTECOMS       Move results
*        WTO   'RETURNED RDTE DATA'
CONTZ    EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Create RDTEANSR variable                             */
*     * /********************************************************/
         L     R1,IKJCT441         Do we have a LOAD address
         LTR   R1,R1                ...for IKJCT441?
         BZ    MYCVARX             NO, bypass CLIST var creation
MYCVAR   EQU   *                   YES, create CLIST variable
*        WTO   'CR8 VAR RDTEANSR'
         LA    R0,ECUPDT           Entry Code
         ST    R0,CT441P1
         LA    R0,PDTEANSR         Name Pointer
         ST    R0,CT441P2
         LA    R0,LDTEANSR         Name Length
         ST    R0,CT441P3
         LA    R0,$DTECOMS         Var  Pointer
         ST    R0,FW
         LA    R0,FW
         ST    R0,CT441P4
*        ST    R0,VALP1
*        LA    R0,VALP1
*        ST    R0,CT441P4
         LA    R0,LDTRSLT          Var  Length
         ST    R0,CT441P5
         LA    R0,TOKEN            Token
         ST    R0,CT441P6
         MVI   CT441P6,B'10000000' Last Parm designation
*
*     * /--------------------------------------------------------/
*     * /* Link to IKJCT441                                     */
*     * /--------------------------------------------------------/
         LA    R1,CT441LST         R1 = Parm List
         L     R15,IKJCT441
         BALR  R14,R15
*        ST    R15,RC#441          Save R15 from IKJCT441
         ST    R15,RCEXIT          Save R15 from IKJCT441
*        LTR   R15,R15             Successful invokation?
*        BZ    MYCVARX             YES, branch to MYCVARX
*        MVC   RCEXIT,=F'2'        NO, Set RC=2, IKJCT441 Error
*        B     MYCVARX
MYCVARX  EQU   *
*        WTO   'END VAR RDTEANSR'
*
         EJECT
*     * /********************************************************/
*     * /* Program Exit                                         */
*     * /* - Restore savearea, FREEMAIN, and                    */
*     * /*   return to caller                                   */
*     * /********************************************************/
MYEXIT   EQU   *
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
         TITLE 'GETDTE - Subroutines                                  '
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
         ST    R5,SAVER5S
         ZAP   DC#LEAP,=P'0'       Assume, not leap year
         ZAP   DC#YDAYS,=P'365'    Assume, 365 days in year
         PACK  DC#YR4,RDATECC(4)   Seed YR4 with  CCYY
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
         L     R5,SAVER5S
         BR    R5                  Return to caller (R5)
*
        EJECT
*     * /********************************************************/
*     * /* Subroutine - Determine MM DD from JJJ         (R5)   */
*     * /--------------------------------------------------------/
*     * /* Determine MM and DD from JJJ                         */
*     * /* -         R4=addr of MONTHNAME                       */
*     * /* -         R6=Length of MONTHNAME                     */
*     * /--------------------------------------------------------/
*     * /* - R3, R4, R5, R6           Working Register          */
*     * /********************************************************/
S#J2MD   EQU    *
         ST    R5,SAVER5S
*     * /--------------------------------------------------------/
*     * /* Search for MM using JJJ                              */
*     * /--------------------------------------------------------/
         LA    R4,MMTABLEE         R4=End Address of MonthTable
         USING MPT,R4              Use MPT DSECT
         LA    R5,MMTBLEN          R3=Length of table entry
         SR    R4,R5               R4=Address of Last MonthTable Entry
         LA    R3,MMTBLN           R3=12 Months
GETMMJJJ EQU   *
         PACK  FW(2),MPTDAT@1      Pack start month days from table
         CLC   MPTMM,=C'02'        JAN or FEB?
         BNH   NOADJLY             Yes, bypass adjust for leap year
         AP    FW(2),DC#LEAP       No, adjust for leap year when MM >1
NOADJLY  EQU   *
         CP    FW(2),PKJJJ         MM JJ < JJJ
         BL    FNDMM               Yes, found month table entry MM
         SR    R4,R5               No, Point to prev MonthTable Entry
         BCT   R3,GETMMJJJ         Check again...
****     something wrong if this executes
         DC    X'0000'
*     * /--------------------------------------------------------/
*     * /* Found MM                                             */
*     * /--------------------------------------------------------/
FNDMM    EQU   *
         MVI   DCMMM,C'0'          Save MM from
         MVC   DCMMM+1(2),MPTMM    ... table lookup
         PACK  DC#TRM,MPTDWTRM     Pack TERM for DOW calculation
         SP    PKJJJ,FW(2)         Compute DD for MM from JJJ
         UNPK  DCDDD,PKJJJ         Save DD
         OI    DCDDD+2,X'F0'       F-Zone
*
         EJECT
*     * /--------------------------------------------------------/
*     * /* Load R4 and R6 with MONTHNAME and Length of MONTHNAME*/
*     * /--------------------------------------------------------/
         LA    R4,MPTMNAME         Point to MONTHNAME
         LA    R6,MPTMNL           R6=Length of MONTHNAME
         DROP  R4                  Tell Assembler
*
S#J2MDX  EQU    *
         L     R5,SAVER5S
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
         ST    R5,SAVER5S
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
         PACK  DC#YR4,RDATECC(4)   Pack 0CCYY
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
         LA    R4,DOWTABLE         R4=Address of DOWTABLE
         LA    R6,DOWTBLN          R6=Number of days
DO#DOWSR EQU   *
         CLC   0(1,R4),DCCDOW      Match DOW ?
         BE    DO#DOWFD            Yes, use Week Day Name
         LA    R4,DOWTBLL(R4)      No, look at next DOWTABLE entry
         BCT   R6,DO#DOWSR         Check again
DO#DOWFD EQU   *
         MVC   DCCWDAY(DOWTBLL-1),1(R4)  Get Weekday name
S#CDOWX  EQU   *
         L     R5,SAVER5S
         BR    R5                  Return to caller (R5)
*
         TITLE 'GETDTE - Month and Day Reference Tables               '
*     * /********************************************************/
*     * /* Month Parameter Table                                */
*     * /********************************************************/
MMTABLE  EQU   *
*                +----------------- 2 C   Month Number
*                | +--------------- 2 C   Days in month
*                | | +------------- 3 C   Days at start of month
*                | | |  +---------- 1 C   Term 0,3,2,5,0,3,5,1,4,6,2,4
*                | | |  |+--------- 9 C   Monthname
*                | | |  ||
         DC    C'01310000January  '
MMTBLEN  EQU   *-MMTABLE                  Length of table entry
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
MMTABLEE EQU   *
MMTBLN   EQU   (*-MMTABLE)/MMTBLEN        Number of table entries
*
*     * /********************************************************/
*     * /* Day of Week Table                                    */
*     * /********************************************************/
DOWTABLE EQU   *
*                +----------------- 1 C   Weekday Number
*                |+---------------- 9 C   Weekday name
*                ||
         DC    C'0Sunday   '
DOWTBLL  EQU   *-DOWTABLE                 Length of table entry
         DC    C'1Monday   '
         DC    C'2Tuesday  '
         DC    C'3Wednesday'
         DC    C'4Thursday '
         DC    C'5Friday   '
         DC    C'6Saturday '
DOWTBLN  EQU   (*-DOWTABLE)/DOWTBLL       Number of table entries
*
         TITLE 'GETDTE - Constants                                    '
*     * /********************************************************/
*     * /* Misc Constants                                       */
*     * /********************************************************/
BLANK    DC    C' '                Blank (space)
PERIOD   DC    C'.'                Period
DATESEP  DC    C'/'                Date Seperator
TIMESEP  DC    C':'                Time Seperator
*
RDTEANSR DC    C'RDTEANSR'         Date-Time-Env CLIST VarName
PDTEANSR DC    A(RDTEANSR)         Addr VAR NAME
LDTEANSR DC    A(L'RDTEANSR)       Addr VAR NAME LENGTH
LDTRSLT  DC    A($DTECOML)         Addr VAR DATA LENGTH
ECUPDT   DC    A(TSVEUPDT)         Addr ENTRY CODE FOR UPDATE/CREATE
TSVEUPDT EQU   2                   Update/Create Request
TOKEN    DC    F'0'                Token used by IKJCT441
*
*
         TITLE 'GETDTE - Equates                                      '
*     * /********************************************************/
*     * /* E Q U A T E S                                        */
*     * /********************************************************/
*     *
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
         TITLE 'GETDTE - System DSECTs                                '
*     *
*     * /********************************************************/
*     * /* System DSECTs                                        */
*     * /********************************************************/
         IHAPSA                    Prefixed Save Area
PSALEN   EQU   *-PSA               Length of PSA
         EJECT
         IHAASCB                   Address Space Control Block
ASCBLEN  EQU   *-ASCB              Length of ASCB
         EJECT
         CVT   DSECT=YES,LIST=NO   Communication Vector Table
         EJECT
         IKJTCB DSECT=YES,LIST=NO  Task Control Block
         EJECT
TIOT     DSECT
         IEFTIOT1                  Task Input/Output Table
TIOTLEN  EQU   *-TIOT              Length of TIOT
         EJECT
JSCB     DSECT
         IEZJSCB                   Job/Step Control Block
         EJECT
*     *
*     * /********************************************************/
*     * /* Month Parameter Table DSECT                          */
*     * /********************************************************/
MPT      DSECT                     Month Parameter Table
MPTMM    DS    CL2                 Month
MPTMMAXD DS    CL2                 Month Max Days
MPTDAT@1 DS    CL3                 Days before 1st of Month
MPTDWTRM DS    CL1                 DOW Term (0,3,2,5,0,3,5,1,4,6,2,4)
MPTMNB   EQU   *                   Start of Month Name
MPTMNAME DS    CL9                 Month Name
MPTMNL   EQU   *-MPTMNAME          Length of Month Name
MPTLE    EQU   *-MPT               Length of Entry
*
         TITLE 'GETDTE - Working Storage Variables                    '
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
*
         EJECT
*     * /********************************************************/
*     * /* Program Variables                                    */
*     * /********************************************************/
SAVER5S  DS    F                   R5  Save in Subroutines
DW       DS    D                   Double Word area
FW       DS    F                   Full Word   area
RCEXIT   DS    F                   Exit RC
MYCOMMA  DS    F                   Addr of Parm Address List
WORKAREA DS    CL8                 Workarea
UNPKAREA DS    CL16                Workarea for unpacking data
*
DC#LEAP  DS    PL1                 Leap Year (0-No, 1-Yes)
DC#YDAYS DS    PL2                 Days in Year (365 or 366)
DC#DD    DS    PL2                 DD
DC#YR4   DS    PL4                 CCYY / 4
DC#YR100 DS    PL4                 CCYY / 100
DC#YR400 DS    PL4                 CCYY / 400
DC#DOW   DS    PL4                 Day of Week
PKJJJ    DS    PL3                 Workarea pack JJJ
DCMMM    DS    CL3                 0MM   character area
DCDDD    DS    CL3                 0DD   character area
DCCDOW   DS    CL1                 Day of Week character area
DCCWDAY  DS    CL9                 Weekday character area
DC#TRM   DS    PL1                 DOW Term
*
         EJECT
         RDTECOMA                  GETDTE Commarea
*
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
*
VALP1    DC    A(0)                VAR VALUE POINTER
         EJECT
*
WSAREAE  EQU   *                   Working Storage END
WSAREAL  EQU   WSAREAE-WSAREA      Working Storage Area Length
         EJECT
         END   GETDTE
@@
//CLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.SAMPLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=GETDTECB
//COB2DTEC JOB (001),'TEST GETDTE',              <-- Review and Modify
//             CLASS=A,MSGCLASS=X,               <-- Review and Modify
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID     <-- Review and Modify
//* -------------------------------------------------------*
//* *  Test Driver to sample COBOL call to GETDTE          *
//* -------------------------------------------------------*
//STEP01  EXEC COBUCLG,CPARM1='LIST,LOAD,NODECK,PMAP,DMAP,LIB',
//        COND.LKED=(0,NE,COB),
//        COND.GO=(0,NE,LKED)
//COB.SYSIN DD *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.             COB2DTE.
       AUTHOR.                 Larry Belmontes.
       REMARKS.
                               This program calls GETDTE and
                               displays results via DISPLAY and
                               PRTOUT DD.

                               This program calls GETDTE with and
                               with no parms for testing purposes.

                               This program serves as a sample of
                               COBOL calling GETDTE.

           https://www.shareabitofit.net/GETDTE-in-MVS38J            */
                                                                     */
                                                                     */
           Disclaimer:                                               */
           -----------                                               */
           No guarantee; No warranty; Install / Use at your own risk.*/
                                                                     */
           This software is provided ôAS ISö and without any         */
           expressed or implied warranties, including, without       */
           limitation, the implied warranties of merchantability     */
           and fitness for a particular purpose.                     */
                                                                     */
           The author requests keeping authors name intact in any    */
           modified versions.                                        */
                                                                     */
           In addition, the author requests submissions regarding    */
           any code modifications / enhancements and/or associated   */
           comments for consideration into a subsequent release      */
           (giving credit to contributor(s)) thus, improving overall */
           functionality benefiting the MVS 3.8J hobbyist public     */
           domain community.                                         */
                                                                     */
       EJECT
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        IBM-4341.
       OBJECT-COMPUTER.        IBM-4341.
       SPECIAL-NAMES.          C01 IS TO-TOP-OF-PAGE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
           SELECT REPORT-FILE ASSIGN TO UR-1403-S-PRTOUT.
      *
       EJECT
       DATA DIVISION.

       FILE SECTION.

       FD  REPORT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS REPORT-LINE.
       01  REPORT-LINE.
           05  CC                  PIC X(01).
           05  REPORT-L132         PIC X(132).

       EJECT
       WORKING-STORAGE SECTION.

       01  RDTECOM  COPY RDTECOMC.

       EJECT
       01  RC-X     PIC 9999.

       EJECT
       PROCEDURE DIVISION.

       0000-MAINLINE SECTION.

      ******************************************************
      * Open Files                                         *
      ******************************************************
           OPEN OUTPUT REPORT-FILE.
           MOVE SPACES   TO CC.
           MOVE 'OPEN REPORT FILE' TO REPORT-L132.
           WRITE REPORT-LINE
                     AFTER 1.

      ******************************************************
      * Call GETDTE with NO commarea and disply results    *
      ******************************************************
           MOVE ALL '*'          TO RDTECOM.
           MOVE SPACES           TO CC.
           MOVE '1. DATA:'       TO REPORT-L132.
           WRITE REPORT-LINE
                     AFTER 1.
           DISPLAY '1. B4 CALL USING DISPLAY OUTPUT'.
           CALL 'GETDTE'.
           MOVE RETURN-CODE      TO RC-X.
           DISPLAY '1. BACK FROM CALL  RC=' RC-X.
           DISPLAY 'DATA=' RDTECOM.
           MOVE SPACES           TO CC.
           MOVE RDTECOM          TO REPORT-L132.
           WRITE REPORT-LINE
                       AFTER 1.

      ******************************************************
      * Call GETDTE with commarea and disply results       *
      ******************************************************
           MOVE SPACES           TO REPORT-L132.
           WRITE REPORT-LINE
                     AFTER 1.
           MOVE ALL '*'          TO RDTECOM.
           MOVE SPACES           TO CC.
           MOVE '2. DATA:'       TO REPORT-L132.
           WRITE REPORT-LINE
                     AFTER 1.
           DISPLAY ' '.
           DISPLAY '2. B4 CALL USING DISPLAY OUTPUT'.
           CALL 'GETDTE' USING RDTECOM.
           MOVE RETURN-CODE      TO RC-X.
           DISPLAY '2. BACK FROM CALL  RC=' RC-X.
           DISPLAY 'DATA=' RDTECOM.
           MOVE SPACES           TO CC.
           MOVE RDTECOM          TO REPORT-L132.
           WRITE REPORT-LINE
                       AFTER 1.

      ******************************************************
      * Close files                                        *
      ******************************************************
           MOVE SPACES   TO CC.
           MOVE 'CLOSE REPORT FILE' TO REPORT-L132.
           WRITE REPORT-LINE
                     AFTER 1.
           CLOSE REPORT-FILE.

      ******************************************************
      * Return to OS                                       *
      ******************************************************
           GOBACK.

/*
//COB.SYSLIB  DD
//            DD DSN=GETDTE.V1R0M00.ASM,DISP=SHR  -- RDTECOMC
//LKED.SYSLIB DD
//            DD DSN=SYS2.LINKLIB,DISP=SHR        -- GETDTE
//GO.SYSPRINT DD SYSOUT=*
//GO.SYSOUT   DD SYSOUT=*                         -- DISPLAY output
//GO.PRTOUT   DD SYSOUT=*                         -- REPORT  output
//GO.SYSDUMP  DD SYSOUT=*
//GO.SYSABEND DD SYSOUT=*
//
./ ADD NAME=GETDTEPL
//PLI2DTEC JOB (001),'TEST GETDTE',              <-- Review and Modify
//             CLASS=A,MSGCLASS=A,REGION=0M,     <-- Review and Modify
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID     <-- Review and Modify
//* -------------------------------------------------------*
//* *  Test Driver to sample PL/I call to GETDTE           *
//* -------------------------------------------------------*
//STEP01   EXEC PL1LFCLG,
//        REGION.PL1L=256K,
//        PARM.PL1L='L,E,A,X,M,S2,NT,SORMGIN=(2,72,1),LINECNT=55',
//        PARM.LKED='XREF,LIST',
//        COND.LKED=(0,NE,PL1L),
//        COND.GO=(0,NE,LKED)
//PL1L.SYSIN DD *
1  PL12DTE: PROC OPTIONS (MAIN);
   /*************************************************************/
   /*                                                           */
   /* Program: PLI2DTE                                          */
   /*                                                           */
   /* Author:  Larry Belmontes                                  */
   /* https://www.shareabitofit.net/GETDTE-in-MVS38J            */
   /*                                                           */
   /* Purpose: This program calls GETDTE and displays results   */
   /*          via DISPLAY and SYSOUT DD.                       */
   /*                                                           */
   /*          This program call GETDTE with and with no parms  */
   /*          for testing purposes.                            */
   /*                                                           */
   /*          This program serves as a sample of PL/I calling  */
   /*          GETDTE.                                          */
   /*                                                           */
   /* Disclaimer:                                               */
   /* -----------                                               */
   /* No guarantee; No warranty; Install / Use at your own risk.*/
   /*                                                           */
   /* This software is provided ôAS ISö and without any         */
   /* expressed or implied warranties, including, without       */
   /* limitation, the implied warranties of merchantability     */
   /* and fitness for a particular purpose.                     */
   /*                                                           */
   /* The author requests keeping authors name intact in any    */
   /* modified versions.                                        */
   /*                                                           */
   /* In addition, the author requests submissions regarding    */
   /* any code modifications / enhancements and/or associated   */
   /* comments for consideration into a subsequent release      */
   /* (giving credit to contributor(s)) thus, improving overall */
   /* functionality benefiting the MVS 3.8J hobbyist public     */
   /* domain community.                                         */
   /*                                                           */
   /*************************************************************/


   /*************************************************************/
   /* Declare GETDTE  Entry Point                               */
   /*************************************************************/
      DCL GETDTE  ENTRY  ;
   /*************************************************************/
   /* Declare GETDTE  Parm List per PLI                         */
   /*************************************************************/
   /* IBM MANUAL: GC28-6594-7 PLI/F Programmers Guide Jan 1971  */
   /* Refer to the above manual for explanation related to      */
   /* declaring the GETDTE parm list as described in section    */
   /* Communications with Other Languages, Chapter 15, p.212    */
   /*************************************************************/
      DCL DTECOM  CHAR(80),
          DTPARML FIXED DEC(1,0) BASED(DTPARM1);
   /*************************************************************/
   /* Declare GETDTE  Detailed Parm List                        */
   /*************************************************************/
      DCL 1 DTECOM_A BASED(DTPARM1),
           2 RDATE,
            3 RDATEMM         CHAR(02),
            3 RDATES1         CHAR(01),
            3 RDATEDD         CHAR(02),
            3 RDATES2         CHAR(01),
            3 RDATECC         CHAR(02),
            3 RDATEYY         CHAR(02),
            3 RDATES3         CHAR(01),
            3 RDATEJJJ        CHAR(03),
           2 RDATES4          CHAR(01),
           2 RTIME,
            3 RTIMEHH         CHAR(02),
            3 RTIMES1         CHAR(01),
            3 RTIMEMM         CHAR(02),
            3 RTIMES2         CHAR(01),
            3 RTIMESS         CHAR(02),
            3 RTIMES3         CHAR(01),
            3 RTIMETT         CHAR(02),
           2 RMONTH_NAME      CHAR(09),
           2 RDAY_NUM         CHAR(01),
           2 RDAY_NAME        CHAR(09),
           2 RJOB_NAME        CHAR(08),
           2 RSTEP_NAME       CHAR(08),
           2 RPROC_STEP_NAME  CHAR(08),
           2 RENV             CHAR(05),
           2 RPARMTY          CHAR(06);
1  /*************************************************************/
   /* Declare Others...                                         */
   /*************************************************************/
      DCL PRTOUT FILE RECORD OUTPUT
          ENV(CTLASA F(101));
      DCL   PRT_REC_AREA   CHAR(101);
      DCL 1 PRT_REC DEFINED PRT_REC_AREA,
            2 PRT_CC       CHAR(1),
            2 PRT_REC_DATA,
              3 FILLER       CHAR(80);

1  /*************************************************************/
   /* Open DD PRTOUT for printing                               */
   /*************************************************************/
      DISPLAY ('Start PL1 to DTE');
      PUT FILE(SYSPRINT)
          SKIP EDIT ('Start PL1 to GETDTE')(A);
      OPEN FILE(PRTOUT);
   /*************************************************************/
   /* Setup Parm and invoke GETDTE                              */
   /*************************************************************/
      DTPARM1 = ADDR (DTECOM);
      CALL GETDTE (DTPARML);
   /* DISPLAY (DTECOM);    */
   /*************************************************************/
   /* Print DTECOM string on PRTOUT                             */
   /*************************************************************/
      PRT_CC       = ' ';
      PRT_REC_DATA = DTECOM;
      WRITE FILE(PRTOUT) FROM(PRT_REC_AREA);
   /*************************************************************/
   /* Close DD PRTOUT                                           */
   /*************************************************************/
      CLOSE FILE(PRTOUT);

   /*************************************************************/
   /* Done: Return to OS                                        */
   /*************************************************************/
      END PL12DTE;
/*
//LKED.SYSLIB DD
//            DD DSN=SYS2.LINKLIB,DISP=SHR        -- GETDTE
//LKED.SYSLMOD DD DSNAME=&&GOSET(GO),DISP=(MOD,PASS),
//        UNIT=SYSALLDA,SPACE=(1024,(70,30,5),RLSE)
//GO.SYSPRINT DD SYSOUT=*                         -- PUT     output
//GO.SYSOUT   DD SYSOUT=*
//GO.PRTOUT   DD SYSOUT=*                         -- REPORT  output
//GO.SYSDUMP  DD SYSOUT=*
//GO.SYSABEND DD SYSOUT=*
//
./ ADD NAME=GETDTE
//IVP1     JOB (SYS),'GETDTE IPV1',          <-- Review and Modify
//             CLASS=A,MSGCLASS=X,           <-- Review and Modify
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID <-- Review and Modify
//* -------------------------------------------------------*
//* *  GETDTE for MVS3.8J TSO / Hercules                   *
//* *                                                      *
//* *  JOB: $IVP1                                          *
//* *       Run GETDTE Validation from TSO Batch           *
//* *       to execulte clist CGETDTE                      *
//* *                                                      *
//* *  Expected Result:                                    *
//* *  RC=0                                                *
//* *                                                      *
//* -------------------------------------------------------*
//BATCHTSO PROC
//STEP01   EXEC PGM=IKJEFT01
//SYSPROC  DD  DISP=SHR,DSN=SYS1.CMDPROC           <--TARGET
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSTSIN  DD  DUMMY       Command Line Input
//         PEND
//*
//IVP1     EXEC BATCHTSO
//STEP01.SYSTSIN DD *
CGETDTE
/*
//
@@
//*
//*  Installs CLISTs
//*
//CLISTS   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.CMDPROC,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CGETDTE
PROC 0

/********************************************************************/
/*                                                                  */
/*    CLIST: $GETDTE                                                */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/GETDTE-in-MVS38J               */
/*         Copyright (C) 2013-2020  Larry Belmontes, Jr.            */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* IVP CLIST to validate GETDTE                                     */
/* and demonstrate various call types under TSO                     */
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
/* 07/30/2020 1.0.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/

/*                                                                  */
/* EXAMPLE 1: TSO CALL FOR GETDTE WITH NO PARM                      */
/* UNDER TSO, PARMS ARE ALWAYS IGNORED.                             */
/* EXPECTED RESULT:                                                 */
/*    RETURN RESULTS IN CLIST VARIABLE RDTEANSR                     */
/*                                                                  */
WRITE
SET RDTEANSR =
CALL 'SYS2.LINKLIB(GETDTE)'
SET RC=&LASTCC
WRITE 1 GETDTE RC=&RC (TSO CALL W NO PARM)
IF &RC=0 THEN -
 WRITE 1 RDTEANSR='&RDTEANSR'
/*                                                                  */
/* EXAMPLE 2: TSO CALL FOR GETDTE WITH PARM                         */
/* UNDER TSO, PARMS ARE ALWAYS IGNORED.                             */
/* EXPECTED RESULT:                                                 */
/*    RETURN RESULTS IN CLIST VARIABLE RDTEANSR                     */
/*                                                                  */
WRITE
SET RDTEANSR =
CALL 'SYS2.LINKLIB(GETDTE)' 'TSO'
SET RC=&LASTCC
WRITE 2 GETDTE RC=&RC (TSO CALL W PARM)
IF &RC=0 THEN -
 WRITE 2 RDTEANSR='&RDTEANSR'
/*                                                                  */
/* EXAMPLE 3: TSO CMD  FOR GETDTE WITH NO PARMS IN CMD LINE         */
/* UNDER TSO, PARMS ARE ALWAYS IGNORED.                             */
/* EXPECTED RESULT:                                                 */
/*    RETURN RESULTS IN CLIST VARIABLE RDTEANSR                     */
/*                                                                  */
WRITE
SET RDTEANSR =
GETDTE
SET RC=&LASTCC
WRITE 3 GETDTE RC=&RC (TSO CP W NO PARM)
IF &RC=0 THEN -
 WRITE 3 RDTEANSR='&RDTEANSR'
/*                                                                  */
/* EXAMPLE 4: TSO CMD  FOR GETDTE WITH PARM  IN CMD LINE            */
/* UNDER TSO, PARMS ARE ALWAYS IGNORED.                             */
/* EXPECTED RESULT:                                                 */
/*    RETURN RESULTS IN CLIST VARIABLE RDTEANSR                     */
/*                                                                  */
WRITE
SET RDTEANSR =
GETDTE TSO
SET RC=&LASTCC
WRITE 4 GETDTE RC=&RC (TSO CP W PARM IN CMD LINE)
IF &RC=0 THEN -
 WRITE 4 RDTEANSR='&RDTEANSR'
/*                                                                  */
/* EXAMPLE 5: TSO CMD  FOR GETDTE WITH PARMS IN CMD LINE            */
/* UNDER TSO, PARMS ARE ALWAYS IGNORED.                             */
/* EXPECTED RESULT:                                                 */
/*    RETURN RESULTS IN CLIST VARIABLE RDTEANSR                     */
/*                                                                  */
WRITE
SET RDTEANSR =
GETDTE XXO XY2
SET RC=&LASTCC
WRITE 5 GETDTE RC=&RC (TSO CP W PARM IN CMD LINE)
IF &RC=0 THEN -
  DO
    WRITE 5 RDTEANSR='&RDTEANSR'
    WRITE
    WRITE SPLIT TO VARIOUS COMPONENTS using RDTECOMA NAMES...
    WRITE RDATE='&SUBSTR(01:14,&RDTEANSR)'
    WRITE RDATEMM='&SUBSTR(01:02,&RDTEANSR)'
    WRITE RDATEDD='&SUBSTR(04:05,&RDTEANSR)'
    WRITE RDATECC='&SUBSTR(07:08,&RDTEANSR)'
    WRITE RDATEYY='&SUBSTR(09:10,&RDTEANSR)'
    WRITE RDATEJJJ='&SUBSTR(12:14,&RDTEANSR)'
    WRITE RTIME='&SUBSTR(16:26,&RDTEANSR)'
    WRITE RTIMEHH='&SUBSTR(16:17,&RDTEANSR)'
    WRITE RTIMEMM='&SUBSTR(19:20,&RDTEANSR)'
    WRITE RTIMESS='&SUBSTR(22:23,&RDTEANSR)'
    WRITE RTIMETT='&SUBSTR(25:26,&RDTEANSR)'
    WRITE RMONTHN='&SUBSTR(27:35,&RDTEANSR)'
    WRITE RDAY#='&SUBSTR(36:36,&RDTEANSR)'
    WRITE RDAYN='&SUBSTR(37:45,&RDTEANSR)'
    WRITE RJOBNAM='&SUBSTR(46:53,&RDTEANSR)'
    WRITE RSTEPNAM='&SUBSTR(54:61,&RDTEANSR)'
    WRITE RPROCSTP='&SUBSTR(62:69,&RDTEANSR)'
    WRITE RENV='&SUBSTR(70:74,&RDTEANSR)'
    WRITE RPARMTY='&SUBSTR(75:80,&RDTEANSR)'
  END

END

EXIT CODE(0)

