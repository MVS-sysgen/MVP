//CHKDSN JOB (JOB),
//             'INSTALL CHKDSN',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//* -------------------------------------------------------*
//* *  CHKDSN for MVS3.8J TSO / Hercules                   *
//* *                                                      *
//* *  JOB: $INST02                                        *
//* *       Install CHKDSN Programs                        *
//* *       Install CHKDSN HELP file                       *
//* *                                                      *
//* *  - CHKDSN programs  installs to SYS2.CMDLIB          *
//* *  - CHKDSN HELP file installs to SYS2.HELP            *
//* *  - Install libraries marked...                       *
//* *    - Search for '<--TARGET'                          *
//* *    - Update install libraries per your               *
//* *      installation standard                           *
//* *                                                      *
//* -------------------------------------------------------*
//ASML     PROC MBR=WHOWHAT
//ASM      EXEC PGM=IFOX00,
//             PARM='NODECK,LOAD,RENT,TERM'
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
//SYSLMOD  DD  DSN=SYS2.CMDLIB(&MBR),DISP=SHR      <--TARGET
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(5,2))
//SYSIN    DD  DUMMY
//         PEND
//*
//* -------------------------------------------------------*
//* *                                                      *
//* *  Assemble CHKDSN programs                            *
//* *                                                      *
//* -------------------------------------------------------*
//CHKDSN   EXEC  ASML,MBR=CHKDSN
//ASM.SYSIN DD DATA,DLM=@@
         TITLE 'CHKDSN - Check for DSN existence                      '
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  ==================================================================
*
*   CCC    HH   HH  KK   KK  DDDDD     SSSS   NN   NN
*  CC  CC  HH   HH  KK  KK   DD  DD   SS  SS  NN   NN
* CC       HH   HH  KK K     DD   DD  SS      NNN  NN
* CC       HHHHHHH  KKK      DD   DD   SSSS   NN N NN
* CC       HH   HH  KK KK    DD   DD      SS  NN  NNN
*  CC  CC  HH   HH  KK  KK   DD  DD   SS  SS  NN   NN
*   CCC    HH   HH  KK   KK  DDDDD     SSSS   NN   NN
*
*  ==================================================================
*
*  Program: CHKDSN
*
*     This program uses a simple command line.  Below is a sample:
*
*     CHKDSN DSN VOL(volser)
*
*
*     CHKDSN checks for existence of a provided Data Set Name (DSN).
*  A dataset name can be a fully-qualified DSN or DSN(MBR) (surrounded
*  by single quotes) or non-qualified DSN or DSN(MBR) (without single
*  quotes) where the USERID is prepended to the DSN or DSN(MBR).
*
*  The DSN validation order is system catalog, VTOC, and PDS member
*  (if specified).
*
*     System catalog search is performed when VOL parm is absent.
*  If catalog entry is found, the VTOC on the cataloged volume is
*  searched for the DSN.  If VTOC entry is found, the PDS MBR is
*  searched for in the DSN, if MBR is specified.  If any of the
*  searches fail, the DSN or DSN(MBR) is considered not found
*  (RC=08 or 12 or 16).  If all searched succeed, the DSN or DSN(MBR)
*  is considered found (RC=00).
*
*     System catalog search is bypassed when VOL parm is specified.
*  The VTOC is searched for the DSN on the specified VOL.  If the VTOC
*  entry is found, the PDS MBR is searched for in the DSN, if MBR is
*  specified.  If any of the searches fail, the DSN or DSN(MBR) is
*  considered not found (RC=12 or 16).  If all searches succeed, the
*  DSN or DSN(MBR) is considered found (RC=00).
*
*     Other processing parameters are available.  See Command Syntax.
*
*
*
*
         EJECT
*  Command Syntax for CHKDSN: <CMDSYNTX>
*  ==================================================================
*
*  +-----------------------------------------------------------------+
*  |CHKDSN DSN VOL(volser) MBR(member) QUIET CATLG                   |
*  +-----------------------------------------------------------------+
*
*
*    where:
*
*    1) DSN   required, dataset as the FIRST positional parameter
*             in the command line.  Dataset name is fully-qualified
*             when enclosed in single quotes.  Dataset name without
*             quotes will be prefixed with USERID.
*
*             NOTE: Dataset name can contain a PDS member surrounded
*                   within '(' and ')' instead of specifying MBR
*                   keyword.
*
*    2) VOL   optional, target volume for uncataloged dataset name.
*             System catalog search is bypassed.
*
*    3) MBR   optional, PDS member name.
*
*    4) QUIET optional, suppress display of messages.
*
*    5) CATLG optional, force catalog search.
*             Validate cataloged and requested VOL match (equal).
*             If not equal, the DSN or DSN(MBR) is considered not
*             found.
*
*
         EJECT
*  CHKDSN Command Examples:
*  ==================================================================
*
*    1) CHKDSN 'HERC01.TEST.CNTL'
*       Check for dataset existence in catalog and VTOC.
*
*    2) CHKDSN 'HERC01.TEST.CNTL' MBR(MYMBR)
*       Check for dataset existence in catalog, VTOC and PDS
*       membership.
*
*    3) CHKDSN TEST.CNTL VOL(LKJ00)
*       Check for dataset existence in VTOC on volume LKJ00.
*       System catalog search is bypassed.
*       NOTE: DSN prefixed with USERID PREFIX due to no quotes
*
*    4) CHKDSN TEST.CNTL(MEMBR0) VOL(LKJ00)
*       Check for dataset existence in VTOC on volume LKJ00 and PDS
*       membership.
*       System catalog search is bypassed.
*       NOTE: DSN prefixed with USERID PREFIX due to no quotes
*
*    5) CHKDSN TEST.CNTL(MEMBR0) VOL(LKJ00) CATLG
*       Check for dataset existence in VTOC on volume LKJ00 and PDS
*       membership and cataloged volume matches VOL parameter
*       System catalog search is forced.
*       NOTE: DSN prefixed with USERID PREFIX due to no quotes
*
*
         EJECT
*  CHKDSN Batch Examples:
*  ==================================================================
*
*     CHKDSN can be executed in batch jobs using qualified dataset
*  names only as user prefixes are not available in batch.  The return
*  code from CHKDSN can be used to control subsequent JOB step(s)
*  processing based on dataset existence.
*
*    1) //STEP0001 EXEC  PGM=CHKDSN,
*       //  PARM='HERC01.TEST.CNTL'
*       //STEP0002 EXEC  PGM=IEBCOPY,COND=(0,NE,STEP001),
*       //INPUT   DD  DSN=HERC01.TEST.CNTL,DISP=SHR
*       .
*       .
*       .
*
*       Check for dataset existence in catalog and VTOC.
*       If DSN exists, execute STEP0002.
*
*
*    2) //STEP0001 EXEC  PGM=CHKDSN,
*       //  PARM='HERC01.TEST.CNTL MBR(MYMBR)'
*
*       Check for dataset existence in catalog, VTOC and PDS
*       membership.
*
*
*    3) //STEP0001 EXEC  PGM=CHKDSN,
*       //  PARM='HERC01.TEST.CNTL VOL(LKJ00)'
*
*       Check for dataset existence in VTOC on volume LKJ00.
*       System catalog search is bypassed.
*
*
         EJECT
*  CHKDSN Programs / Services:
*  ==================================================================
*
*    1) GETMAIN/FREEMAIN          Working storage
*    1) LOCATE CAMLIST            Search Catalog
*    1) OBTAIN CAMLIST            Fetch DSN from VTOC
*    1) Dynamic Allocation        PDS processing
*    1) OPEN/FIND/CLOSE           PDS member fetch
*    1) CHKDSNMS                  Message Process Module
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
*  |  00  |  DSN found                                               |
*  |      |  Message text: DSN on vvvvvv found   OR                  |
*  |      |                DSN(MBR) on vvvvvv found                  |
*  +------+----------------------------------------------------------+
*  |  04  |  DSN not found                                           |
*  |      |  Message text: Cataloged and Request volume not equal    |
*  +------+----------------------------------------------------------+
*  |  08  |  DSN not found in catalog                                |
*  |      |  Message text: RC=xxx LOCATE  CAMLST NAME error          |
*  +------+----------------------------------------------------------+
*  |  12  |  DSN not found in VTOC                                   |
*  |      |  Message text: RC=xxx OBTAIN  CAMLST SEARCH error        |
*  +------+----------------------------------------------------------+
*  |  16  |  PDS member not found (membernm)                         |
*  +------+----------------------------------------------------------+
*  | 4004-|  Parm not supplied                                       |
*  | 4005 |                                                          |
*  +------+----------------------------------------------------------+
*  | 4008-|  No variables found                                      |
*  | 4010 |                                                          |
*  +------+----------------------------------------------------------+
*  | 4041 |  RC=xxx PDS Allocate Error                               |
*  +------+----------------------------------------------------------+
*  | 4042 |  RC=xxx PDS Open Error                                   |
*  +------+----------------------------------------------------------+
*  | 4044 |  RC=xxx PDS Close Error                                  |
*  +------+----------------------------------------------------------+
*  | 4045 |  RC=xxx PDS Free Error                                   |
*  +------+----------------------------------------------------------+
*  | 4046 |  DSN not PDS (DSORG=PO) for MBR request                  |
*  +------+----------------------------------------------------------+
*  | 4097 |  Cannot link to IKJDAIR                                  |
*  +------+----------------------------------------------------------+
*
         EJECT
*  Macros and SYSLIB Location:
*  ==================================================================
*
*  Macro     Description                         Library
*  --------  ----------------------------------  ------------------
*  YREGS     Register Equates                    SYS2.MACLIB
*  IHAPSA    Prefixed Save Area                  SYS1.AMODGEN
*  CVT       Communication Vector Table          SYS1.AMODGEN
*  IKJTCB    Task Control Block                  SYS1.AMODGEN
*  IEFTIOT1  Task IO Table                       SYS1.AMODGEN
*  IEFJFCBN  Job File Control Block              SYS1.AMODGEN
*  IHAASCB   Address Space Control Block         SYS1.AMODGEN
*  IEFUCBOB  Unit Control Block                  SYS1.AMODGEN
*  IKJCPPL   CP Parm List                        SYS1.MACLIB
*  IKJUPT    User Profile Table                  SYS1.MACLIB
*  IKJPPL    Parse Parm List                     SYS1.MACLIB
*  IKJIOPL   I/O Service Routine Parm List       SYS1.MACLIB
*  IKJDAPL   Dynamic Allocation Parm LIst        SYS1.MACLIB
*  IKJDAP08  Operation block ALLOC Dataset       SYS1.MACLIB
*  IKJDAP18  Operation block FREE  Dataset       SYS1.MACLIB
*  IECSDSL1  DSCB Dataset Mapping 1-4            SYS1.AMODGEN
*
*
*  References:
*  ==================================================================
*
*  - Advanced Assembler Language and MVS Interfaces (C. Cannatello)
*
*
         EJECT
*  Change History: <CHGHIST>
*  ==================================================================
*
*  MM/DD/CCYY Version  Name / Description
*  ---------- -------  --------------------------------------------
*
*  07/30/2020 0.9.00   Larry Belmontes Jr.
*                      - Initial version released to MVS 3.8J
*                        hobbyist public domain
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
CHKDSN   CSECT
         USING CHKDSN,R10,R11,R12  my BASE REGISTER(S)
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
PGMID    DC    CL8'CHKDSN '        My Program STAMP
         DC    CL8'MVS3.8J '       OS
         DC    CL8'V1.0.00 '       .Version
         DC    CL8'07302020'       ..date  MMDDCCYY
         DC    CL1' '
PGMDS    DC    CL8'&SYSDATE'       Assemble Date
         DC    CL3' - '            .and
PGMTS    DC    CL8'&SYSTIME'       ..Time
         DC    C'Copyright (C) 2020'
         DC    C'Larry Belmontes, Jr. '
         DC    C'https://ShareABitofIT.net/CHKDSN-in-MVS38J'
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
*
*     * /********************************************************/
*     * /* Load CHKDSNMS Entry Point  (Message Processor)       */
*     * /* - R0, R1 Working Register                            */
*     * /********************************************************/
         LOAD  EP=CHKDSNMS,ERRET=ERR4098
         ST    R0,CHKDSNMS         Load and Save CHKDSNMS Entry Point
         LR    R1,R2               Restore R1 as PARM addr
         EJECT
*     * /********************************************************/
*     * /* Check if we are running under TSO                    */
*     * /* - R3 Working Register                                */
*     * /********************************************************/
*                                  PSA is at address 0
         MVI   AMITSO,C'Y'         Assume TSO
         L     R3,PSAAOLD-PSA(0)   R3=ASCB Address
         L     R3,ASCBTSB-ASCB(R3) R3=ASCBTSB Address
         LTR   R3,R3               If 0, program not under TSO
         BNZ   TSO#YES             ..continue, under TSO
TSO#NO   EQU   *
         MVI   AMITSO,C'N'         Not under TSO
TSO#YES  EQU   *
*     *
*     * /********************************************************/
*     * /* Initialize RC                                        */
*     * /********************************************************/
         MVC   RCEXIT,=F'0'        Zero Return Code R15
*     *
*     * /********************************************************/
*     * /* Initialize - OTHER                                   */
*     * /********************************************************/
         MVI   PQUIET,C'N'         QUIET  request   - init to NO
         MVI   DJMSG,C' '          Clear DJMSG
         MVC   DJMSG+1(L'DJMSG-1),DJMSG
*
         EJECT
*     * /********************************************************/
*     * /* Get PARAMETER information passed to program          */
*     * /* - R1 myPARMS address on entry                        */
*     * /* - R4 Starting parms address                          */
*     * /* - R6 Parm Length                                     */
*     * /* - R3, R4, R5 Working Register                        */
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
*
         EJECT
*     *
*     * /********************************************************/
*     * /* Uppercase translation of PARMIN                      */
*     * /* - R4 Starting parms address                          */
*     * /* - R6 Parm Length                                     */
*     * /* - R8 Working Register                                */
*     * /********************************************************/
CAP#EM   EQU   *
         LR    R8,R6               R8=Parm Length
         BCTR  R8,0                Adjust length for OC
         EX    R8,UPPERC           Execute Uppercase translation
         B     UPPERCX
UPPERC   OC    0(0,R4),=256X'40'   EBCDIC Lower-Upper Case Translate
UPPERCX  EQU   *
*
*     * /********************************************************/
*     * /* Scan parm list                                       */
*     * /********************************************************/
PARMSCAN EQU   *
*     *
*     * /*------------------------------------------------------*/
*     * /* Initialize parm switches / variables                 */
*     * /* - R7 PDSN address                                    */
*     * /* - R8 PDSN length                                     */
*     * /*------------------------------------------------------*/
         MVI   PDSN,C' '           Clear DSN
         MVC   PDSN+1(L'PDSN-1),PDSN
         MVI   PMBR,C'N'           MBR    specified - init to NO
         MVI   PVOL,C'N'           VOLUME specified - init to NO
         MVI   PDASD,C'N'          DASD   request   - init to NO
         MVI   PMULTI,C'N'         MULTI  request   - init to NO
         MVI   PCATLG,C'N'         CATLG  request   - init to NO
         MVI   PQUIET,C'N'         QUIET  request   - init to NO
         LA    R7,PDSN             R7=Start of PDSN area
         LA    R8,0                R8=PDSN actual length
         STH   R8,PDSNL            Store length of PDSN
*
         EJECT
*     * /*------------------------------------------------------*/
*     * /* Get DATASET NAME as 1st required parm                */
*     * /* - R4 Parm address                                    */
*     * /* - R6 Parm Length                                     */
*     * /* - R7 PDSN address                                    */
*     * /* - R8 PDSN length                                     */
*     * /*------------------------------------------------------*/
         MVI   IDPREFIX,C'Y'       Assume USERID Prefix appending
PRMDSN   EQU   *
         CLI   0(R4),C' '          Delimiter?
         BE    NEXTPRM             YES, next parm
         CLI   0(R4),C'('          Delimiter for PDS member?
         BE    DSNMBR              YES, process
         CLI   0(R4),C''''         Apostrophe in DSN?
         BE    BUMPPRM             YES, bypass and try again!
         MVC   0(1,R7),0(R4)       NO, continue with dataset name
         LA    R7,1(R7)            Bump up position PDSN
         LA    R8,1(R8)            Bump up length of PDSN
         STH   R8,PDSNL            Store length of PDSN
         B     BUMPCNT             Jump to PARMIN position bump!
BUMPPRM  EQU   *
         CLI   IDPREFIX,C'N'       USERID Prefix Y?
         BE    BUMPCNT             YES, continue...
         MVI   IDPREFIX,C'N'       NO, set USERID PREFIX to Y
BUMPCNT  EQU   *
         LA    R4,1(R4)            Bump up position PARMIN
         BCT   R6,PRMDSN           Check again...
         B     PARMDONE            Done...
*
         EJECT
*     * /*------------------------------------------------------*/
*     * /* Get other optional parms                             */
*     * /* - R4 Parm address                                    */
*     * /* - R6 Parm Length                                     */
*     * /*------------------------------------------------------*/
NEXTPRM  EQU   *
         LA    R4,1(R4)            Bump up position PARMIN
NEXT01   EQU   *
         CLC   KWMBR,0(R4)         Keyword 'MBR('?
         BE    PARMMBR             YES, process
         CLC   KWVOL,0(R4)         Keyword 'VOL('?
         BE    PARMVOL             YES, process
         CLC   KWQUIET,0(R4)       Keyword 'QUIET'?
         BE    KQUIET              YES, process
         CLC   KWCATLG,0(R4)       Keyword 'CATLG'?
         BE    KCATLG              YES, process
         BCT   R6,NEXTPRM          Check again...
         B     PARMDONE            Done
*
         EJECT
*     * /*------------------------------------------------------*/
*     * /* Keyword Delimiter Check                              */
*     * /* - R0 Keyword Length                                  */
*     * /* - R4 Parm address                                    */
*     * /* - R6 Parm Length                                     */
*     * /* - R7 Branch address for keyword set                  */
*     * /*------------------------------------------------------*/
KQUIET   EQU  *
         LA    R7,PARMQIET         Keyword Set
         LA    R0,(L'KWQUIET)
         B     K@SET               Continue on Keyword Set
KCATLG   EQU  *
         LA    R7,PARMCTLG         Keyword Set
         LA    R0,(L'KWCATLG)
         B     K@SET               Continue on Keyword Set
K@SET    EQU   *
         CR    R6,R0               End of PARMS?
         BER   R7                  YES, flag keyword
         AR    R4,R0               Bump up position PARMIN
         CLI   0(R4),C' '           Blank follow keyword?
         BER   R7                  YES, flag keyword
         SR    R4,R0               NO, restore position PARMIN
         B     NEXTPRM             continue checking...
*
         EJECT
*     * /*------------------------------------------------------*/
*     * /* Keyword Set                                          */
*     * /*------------------------------------------------------*/
PARMDASD EQU   *
         MVI   PDASD,C'Y'          Mark DASD action ON   (future)
         B     PARM@DEC            Decrement...
PARMMLTI EQU   *
         MVI   PMULTI,C'Y'         Mark MULTI action ON  (future)
         B     PARM@DEC            Decrement...
PARMCTLG EQU   *
         MVI   PCATLG,C'Y'         Mark CATLG action ON
         B     PARM@DEC            Decrement...
PARMQIET EQU   *
         MVI   PQUIET,C'Y'         Mark QUIET action ON
         B     PARM@DEC            Decrement...
*     *
*     * /*------------------------------------------------------*/
*     * /* Decrement command buffer scanning position           */
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
*     * /* - R4 Parm address                                    */
*     * /* - R0 Keyword Length                                  */
*     * /* - R6 Parm Length                                     */
*     * /* - R7, R8 Working register                            */
*     * /*------------------------------------------------------*/
PARMVOL  EQU   *
         MVI   PVOL,C'Y'           Mark VOL specified action ON
         LA    R4,L'KWVOL(R4)      Bump up position PARMIN
         LA    R0,(L'KWVOL) 1
*        EQU   *
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
*
         EJECT
*     * /*------------------------------------------------------*/
*     * /* MBR keyword and subfield process                     */
*     * /* - R4 Parm address                                    */
*     * /* - R0 Keyword Length                                  */
*     * /* - R6 Parm Length                                     */
*     * /* - R7, R8 Working register                            */
*     * /*------------------------------------------------------*/
DSNMBR   EQU   *
         MVI   PMBR,C'Y'           Mark MBR specified action ON
         LA    R4,1(R4)            Bump up position PARMIN
         LA    R0,1
         B     PARMMGO
PARMMBR  EQU   *
         MVI   PMBR,C'Y'           Mark MBR specified action ON
         LA    R4,L'KWMBR(R4)      Bump up position PARMIN
         LA    R0,(L'KWMBR) 1
PARMMGO  EQU   *
         SR    R6,R0               Decrement R6 accordingly
         BZ    PARMDONE            Done when R6 is ZERO
         BM    PARMDONE             ... or NEGATIVE
LOADMBR  EQU   *
         MVI   PMEMBER,C' '        Clear MBR
         MVC   PMEMBER+1(L'PMEMBER-1),PMEMBER
         LA    R7,PMEMBER          Start of PMEMBER area
         LA    R8,L'PMEMBER        Max limit for VOL
MBR00    EQU   *
         CLI   0(R4),C' '          Delimiter?
         BE    NEXTPRM             YES, branch to NEXTPRM
         CLI   0(R4),C')'          Delimiter?
         BE    NEXTPRM             YES, branch to NEXTPRM
         MVC   0(1,R7),0(R4)       NO, continue with volume
         LA    R4,1(R4)            Bump up position PARMIN
         LA    R7,1(R7)            Bump up position PMEMBER
         BCTR  R8,0                Decrement R8
         LTR   R8,R8               0?  MBR max length used?
         BZ    MBR00A              YES, branch to NEXTPRM via BCT
         BCT   R6,MBR00            Check again... next MBR character
         B     PARMDONE            Done...
MBR00A   BCT   R6,NEXTPRM          Check again... next parm
         B     PARMDONE            Done...
*
         EJECT
*     * /*------------------------------------------------------*/
*     * /* Parm processing complete                             */
*     * /*   Check for appending USERID as prefix under TSO     */
*     * /*------------------------------------------------------*/
PARMDONE EQU   *                   Done with SCAN
         MVC   DDSNL,PDSNL         Dataset name length from PARMIN
         MVC   DDSN,PDSN           Dataset name from PARMIN
         CLI   AMITSO,C'Y'         TSO?
         BNE   PARMDCNT            NO, continue with DSN as-is
         CLI   IDPREFIX,C'Y'       USERID Prefix needed for DSN?
         BNE   PARMDCNT            NO, continue with DSN as-is
*
         EJECT
*     * /*------------------------------------------------------*/
*     * /* DSN, apply USERID prefix                             */
*     * /* - R4, R6, R7     Working register                    */
*     * /*------------------------------------------------------*/
APPENDID EQU   *                   YES, add prefix to DSN
         L     R4,MYCPPLP          Point to CPPL
         USING CPPL,R4             Tell Assembler
         L     R6,CPPLUPT          Point to UPT
         USING UPT,R6              Tell Assembler
         SR    R7,R7               Clear R7
         IC    R7,UPTPREFL         Prefix ID Length
         STH   R7,MYIDL            Store Prefix ID Length
         MVC   MYID,UPTPREFX       Store Prefix ID
         DROP  R4
         DROP  R6
*
         MVI   DDSN,C' '           Clear DDSN
         MVC   DDSN+1(L'DDSN-1),DDSN
*
         LA    R4,DDSN             Address of DDSN
         SR    R6,R6               Clear R6
         LH    R6,MYIDL            Length  of DDSN, set to MYIDL
         LA    R7,MYID             Address of MYID
*
         EJECT
DOUSERID EQU   *                   Move USERID prefix to DDSN
         BCTR  R6,0                Adjust length for EX
         EX    R6,MVCMYID          EX move of PDSN
         LA    R6,1(R6)            Reset   DDSN length
         AR    R4,R6               Bump up DDSN position
         B     DOPERIOD            Add period after USERID
MVCMYID  MVC   0(0,R4),MYID        Move MYID for EX
DOPERIOD EQU   *
         MVI   0(R4),C'.'          Move period after USERID
         LA    R4,1(R4)            Bump up DDSN position
         LA    R6,1(R6)            Bump up DDSN length
DOPDSN0  EQU   *
         SR    R7,R7               Clear R7
         LH    R7,PDSNL            Length of PDSN
         AR    R7,R6               Add current DDSN length
         C     R7,=F'44'           DDSN within 44 byte limit?
         BNH   DOPDSN              YES, move PDSN portion
DOPDSN44 EQU   *                   NO, move PDSN portion truncated!
         LA    R7,44               Initialize DDSNL
         STH   R7,DDSNL              to 44
         SR    R7,R6               44 - current DDSNL
         EX    R7,MVCPDSN          EX move of PDSN
         B     PARMDCNT
DOPDSN   EQU   *
         SR    R7,R7               Clear R7
         STH   R7,DDSNL            Length of DDSN
         LH    R7,PDSNL            Length of PDSN
         BCTR  R7,0                Adjust length for EX
         EX    R7,MVCPDSN          EX move of PDSN
         B     PARMDCNT
MVCPDSN  MVC   0(0,R4),PDSN        Move PDSN for EX
*     * /*------------------------------------------------------*/
*     * /* DSN, use as-is                                       */
*     * /*------------------------------------------------------*/
PARMDCNT EQU   *
         MVC   CAMDSN,DDSN         YES, Load DSN from DDSN data
         MVC   PVOLSER,OBTVOL      Save VOLSER from PARM data
*
         EJECT
*     * /********************************************************/
*     * /* Locate dataset name via catalog when                 */
*     * /*   VOL or CATLG keywords specified                    */
*     * /* - R3, R7 Working Register                            */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
LOC8DSN  EQU   *
         MVI   DDSCAT,C'N'         Assume Dataset cataloged NO
         CLI   PCATLG,C'Y'         CATLG specified?
         BE    CAMNAME             Yes, force cat srch
         CLI   PVOL,C'Y'           VOL specified?
         BE    DSCBF1A             Yes, bypass cat srch, chk F1 DSCB
CAMNAME  EQU   *
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
*                                  NO. Error DSN not found in catlg
         ST    R15,SAVER15         Save LOCATE R15
         MVC   RCEXIT,=F'4020'     Locate error on DSN
         BAL   R7,MSGPRCR
         LTR   R15,R15             R15 = 0?
         BNZ   LOC8C01             NO, error msg provided, done
*                                  YES, augment provide message
         L     R7,PDMSG            DJMSG starting address
         A     R7,PMSGTXTL         Adjust by length of provided msg txt
         LA    R7,1(R7)            Next position

         L     R15,SAVER15         Restore LOCATE R15
         C     R15,=F'4'           Catlg not avail?
         BE    CATNOTAV
         C     R15,=F'8'           DSN not in catlg?
         BE    DSNNOCAT
         B     CATNOTAV            Other catlg error
*
         EJECT
CATNOTAV EQU   *
         MVC   0(22,R7),=C':Catalog not available'
         B     LOC8C00
DSNNOCAT EQU   *
         MVC   0(23,R7),=C':Dataset not in catalog'
         B     LOC8C00
LOC8C00  EQU   *
         CVD   R15,DW              Convert to decimal
         UNPK  DJMSG+18(3),DW+6(2) Unpack and
         OI    DJMSG+20,X'F0'       ... force and F zone
         MVC   DJMSG+22(8),=C'LOCATE  '                              :
         MVI   DDSCAT,C'N'         Dataset cataloged NO
         CLI   PCATLG,C'Y'         CATLG specified?
         BE    LOC8C01             Yes, error..
         CLI   PVOL,C'Y'           Volume specified?
         BE    DSCBF1              YES, attempt fetch per given VOL
*                                  NO, Error DSN not found in catlg
LOC8C01  EQU   *
         MVC   RCEXIT,=F'08'       Dataset not cataloged
         B     MYEXIT              Done, exit.
*
DSNFOUND EQU   *
         MVI   DDSCAT,C'Y'         Dataset cataloged YES
         MVC   DDSCATV,LOCAVSER    Dataset cataloged VOLUME
         MVC   DDSN,CAMDSN         Dataset name from catalog
         MVC   DVOLSER,LOCAVSER    LOAD VOLSER FROM CAMLST WORKAREA
         MVC   OBTVOL(6),DVOLSER   MOVE VOLSER TO OBTVOL
*
         EJECT
*     * /********************************************************/
*     * /* Get Format-1 IDENTIFIER DATA SET CONTROL BLOCK       */
*     * /* - R3, R7 Working Register                            */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
DSCBF1   EQU   *
         CLI   PVOL,C'Y'           Volume specified?
         BNE   DSCBF1A             NO, branch to fetch format-1 DSCB
*                                  YES.
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
*                                  NO. Error DSN not found in F1 DSCB
         ST    R15,SAVER15         Save LOCATE R15
         MVC   RCEXIT,=F'4021'     Obtain error on DSN
         BAL   R7,MSGPRCR
         LTR   R15,R15             R15 = 0?
         BNZ   OBT8C01             NO, error msg provided, done
*                                  YES, augment provide message
         L     R7,PDMSG            DJMSG starting address
         A     R7,PMSGTXTL         Adjust by length of provided msg txt
         LA    R7,1(R7)            Next position

         L     R15,SAVER15         Restore LOCATE R15
         C     R15,=F'4'
         BE    VOLNMNTD
         C     R15,=F'8'
         BE    DSNNOFND
         B     DSNCATER
*
         EJECT
VOLNMNTD EQU   *
         MVC   0(20,R7),=C':Volume not mounted'
         B     OBT8C00
DSNNOFND EQU   *
         CLI   PVOL,C'Y'           Volume specified?
         BE    DSNOTVOL
         MVC   0(26,R7),=C':Dataset not found in VTOC'
         B     OBT8C00
DSNOTVOL EQU   *
         MVC   0(16,R7),=C':Dataset not on '
         MVC   16(06,R7),PVOLSER
         B     OBT8C00
DSNCATER EQU   *
         MVC   0(14,R7),=C':Catalog error'
         B     OBT8C00
OBT8C00  EQU   *
         CVD   R15,DW              Convert to decimal
         UNPK  DJMSG+18(3),DW+6(2) Unpack and
         OI    DJMSG+20,X'F0'       ... force and F zone
         MVC   DJMSG+22(8),=C'OBTAIN  '
OBT8C01  EQU   *
         MVC   RCEXIT,=F'12'       Dataset not on DSCB-1
         B     MYEXIT              Done, exit.
OBTR00   EQU   *
         CLI   PCATLG,C'Y'         CATLG specified?
         BNE   OBTR01              No, continue...
*                                  Yes, check VOLUMES
         CLC   DDSCATV,PVOLSER     Volumes =?
         BE    OBTR01              Yes, continue
*                                  No, error, CATVOL not = VTOCVOL
         MVC   RCEXIT,=F'4025'     VOLs not equal error
         BAL   R7,MSGPRCR
         MVC   RCEXIT,=F'04'
         B     MYEXIT              Done, exit.
OBTR01   EQU   *
         MVC   DDSN,CAMDSN         Dataset name from F1
         MVC   DVOLSER,OBTVOL      Save VOLUME
*
         EJECT
*     * /********************************************************/
*     * /* Allocate PDS                                         */
*     * /* - R7         Working Register                        */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
POMBRF   EQU   *
         CLI   PMBR,C'Y'           PDS MBR keyword?
         BNE   POMBRFXT            NO, bypass PDS Info
         TM    DS1DSORG,X'02'      PO PDS Dataset ?
         BO    PDSALLOC            YES, Allocate PDS
*                                  NO, error DSORG not PO
         MVC   RCEXIT,=F'4046'     DSN not a PDS
         BAL   R7,MSGPRCR
         B     MYEXIT              Done, exit.
PDSALLOC EQU   *
         BAL   R7,PDSALC           YES, allocate dataset
         LTR   R15,R15             Successful?
         BZ    PDSFETCH            YES, check for PDS member
*                                  NO, ERROR, cannot alloc PDS
         ST    R15,SAVER15
         MVC   RCEXIT,=F'4041'     Dynam Alloc error
         BAL   R7,MSGPRCR
*                                  Augment msg with RC regardless
         L     R15,SAVER15
         CVD   R15,DW              Convert to decimal
         UNPK  DJMSG+18(3),DW+6(2) Unpack and
         OI    DJMSG+20,X'F0'       ... force and F zone
         B     MYEXIT              Done, exit.
*
         EJECT
*     * /********************************************************/
*     * /* Open, Find, Close PDS                                */
*     * /* - R2, R7             Working Register                */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
PDSFETCH EQU   *
*                                  Init PDSDCBW from PDSDCB
         LA    R2,PDSDCBW          Addr of myPDSDCB
         MVC   0(PDSDCBL,R2),PDSDCB  Init myPDSDCB with PDSDCB
         MVC   DDNAM(8,R2),DDNAME  Apply DDN from DAIR
         MVI   PARMVL1,X'80'       Mark as last parm FW (VL=1)
PDSOPEN  EQU   *
         OPEN  ((R2),INPUT),MF=(E,PARMVL1) Open PDSDCBW execute form
         LTR   R15,R15             Successful?
         BZ    PDSFIND             YES, continue...
*                                  NO, ERROR, cannot open  PDS
         ST    R15,SAVER15
         MVC   RCEXIT,=F'4042'     PDS open error
         BAL   R7,MSGPRCR
*                                  Augment msg with RC regardless
         L     R15,SAVER15
         CVD   R15,DW              Convert to decimal
         UNPK  DJMSG+18(3),DW+6(2) Unpack and
         OI    DJMSG+20,X'F0'       ... force and F zone
         B     PDSFREE             GO free PDS and exit
*
PDSFIND  EQU   *
         FIND  (R2),PMEMBER,D      FIND member
         LTR   R15,R15             Found member?
         BZ    PDSCLSE             Yes.
*                                  No, PDS member not found
         MVC   RCEXIT,=F'4043'     PDS Member not Found
         BAL   R7,MSGPRCR
         LTR   R15,R15             R15 = 0?
         BNZ   PDSCLSE             NO, error msg provided
*                                  YES, augment provided msg
         L     R7,PDMSG            DJMSG starting address
         A     R7,PMSGTXTL         Adjust by length of provided msg txt
         LA    R7,1(R7)            Next position
         MVI   0(R7),C'('
         LA    R7,1(R7)            Next position
         MVC   0(8,R7),PMEMBER
         LA    R7,8(R7)            Next position
         MVI   0(R7),C')'
         MVC   RCEXIT,=F'16'       RC=16, DSN(MBR) not found
         B     PDSCLSE             Done, close, free and exit
*
         EJECT
PDSSYN   EQU   *                   IO ERROR from reading PDS DIR
         SYNADAF ACSMETH=BSAM      Obtain msg/err code for I/O error
         MVC   DJMSG+15(78),44(R1)
         SYNADRLS                  Release msg buff/saveareas -SYNADAF
         B     PDSFREE             Go FREE PDS

PDSERR   EQU   *                   SYNADAF Error
*                                  Message already in DJMSG
         B     PDSFREE             Go FREE PDS

PDSCLSE  EQU   *                   CLOSE file           (DCB)
         CLOSE ((R2)),MF=(E,PARMVL1)  Close PDS execute form
         LTR   R15,R15             Successful?
         BZ    PDSFREE             YES, Free allocation
*                                  NO, ERROR, cannot close PDS
         ST    R15,SAVER15
         MVC   RCEXIT,=F'4044'     PDS close error
         BAL   R7,MSGPRCR
*                                  Augment msg with RC regardless
         L     R15,SAVER15
         CVD   R15,DW              Convert to decimal
         UNPK  DJMSG+18(3),DW+6(2) Unpack and
         OI    DJMSG+20,X'F0'       ... force and F zone
         B     PDSFREE             GO free PDS and exit
*
         EJECT
*     * /********************************************************/
*     * /* Free PDS                                             */
*     * /* - R7 Working Register                                */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
PDSFREE  EQU   *
*
         BAL   R7,PDSFRE           FREE dataset
         LTR   R15,R15             Successful?
         BZ    POMBRFXT            YES, Done.
*                                  NO, cannot FREE PDS
         ST    R15,SAVER15
         MVC   RCEXIT,=F'4045'     Free allocation error
         BAL   R7,MSGPRCR
*                                  Augment msg with RC regardless
         L     R15,SAVER15
         CVD   R15,DW              Convert to decimal
         UNPK  DJMSG+18(3),DW+6(2) Unpack and
         OI    DJMSG+20,X'F0'       ... force and F zone
         B     MYEXIT              Done, exit.
*
POMBRFXT EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Check other options                                  */
*     * /* - Create DJMSG for options requested                 */
*     * /********************************************************/
CHKOPTS  EQU   *
CHKXT    EQU   *                   Check Exit
         EJECT
*     * /********************************************************/
*     * /* Exit, return to caller                               */
*     * /* - Display message, FREEMAIN and return to caller     */
*     * /********************************************************/
MYEXIT   EQU   *
         CLC   AMITSO,C'N'         TSO?
         BE    NOMSG0              No, bypass messaging...
         CLI   PQUIET,C'Y'         QUIET?
         BE    NOMSG0              Yes, bypass messaging...
         CLC   RCEXIT,=F'0'        RC=0?
         BNE   YESMSG0             No, post message
*                                  Yes, construction DSN on vvvvvv msg
MSGRC00  EQU   *
         LA    R4,DJMSG+15         R4=Start addr of DJMSG
         SR    R2,R2               Clear R2
         LH    R2,DDSNL            R2=Length of DSN
         LA    R3,DDSN             R3=Start addr of DSN
         BCTR  R2,0                Adjust length for EX
         EX    R2,RC0MSG           EX DSN MVC
         AR    R4,R2               R4=Last DJMSG position
         LA    R4,1(R4)            R4=Bump up by 1
*
         CLI   PMBR,C'Y'           MBR to display?
         BNE   MSGRC00C            No, branch to MSGRC00C
         MVI   0(R4),C'('          Yes, move literal '('
         LA    R4,1(R4)            R4=Bump up by 1
         LA    R2,L'PMEMBER        R2=Length member name
         LA    R3,PMEMBER          R3=Start addr of member name
DOMBRL   EQU   *
         CLI   0(R3),C' '          Any more characters?
         BE    DOMBRX              No, done
         MVC   0(1,R4),0(R3)       Yes, move it
         LA    R3,1(R3)            Bump up member name addr
         LA    R4,1(R4)            Bump up DJMSG addr
         BCT   R2,DOMBRL           Do again...
         LA    R4,1(R4)            Loop exhasted, bump up DJMSG addr
DOMBRX   EQU   *
         MVI   0(R4),C')'          Move literal ')'
         LA    R4,1(R4)            Bump up DJMSG addr
MSGRC00C EQU   *
         MVC   0(4,R4),=C' on '    Move literal ' on '
         LA    R4,4(R4)            Next DJMSG position
         LA    R2,6                R2=Length of VOL
         LA    R3,DVOLSER          R3=Start addr of VOL
         BCTR  R2,0                Adjust length for EX
         EX    R2,RC0MSG           Move VOL
         B     YESMSG0             Branch to TPUT
RC0MSG   MVC   0(0,R4),0(R3)       Move for EX
*
YESMSG0  EQU   *
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
         BAL   R7,PREFXMSG         Prefix start of message
         TPUT  DJMSG,L'DJMSG       Display message
         LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
NOMSG0   EQU   *
*
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
         L     R5,RCEXIT           Set R15 to my exit value
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
         LR    R15,R5              R15 = RC for exit
NOMAINS  EQU   *
         L     R14,12(R13)         Restore R14
         LM    R0,R12,20(R13)      Restore R0 thru R12
         BR    R14                 Return to caller
*
         TITLE 'CHKDSN - Error Entry Points                           '
*     * /********************************************************/
*     * /* Error Setting                                        */
*     * /* - R7  Working Register                               */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
ERR4004  EQU   *
         MVC   RCEXIT,=F'4004'     PARM not supplied
         B     ERR#GO
ERR4005  EQU   *
         MVC   RCEXIT,=F'4005'     PARM not supplied  R1
         B     ERR#GO
ERR4008  EQU   *
         MVC   RCEXIT,=F'4008'     CPPL error no variables
         B     ERR#GO
ERR4009  EQU   *
         MVC   RCEXIT,=F'4009'     CPPL error no variables ZERO
         B     ERR#GO
ERR4010  EQU   *
         MVC   RCEXIT,=F'4010'     CPPL error no variables NEGATIVE
         B     ERR#GO
ERR4097  EQU   *
         MVC   RCEXIT,=F'4097'     IKJDAIR  not found, link error
         B     ERR#GO
ERR4098  EQU   *
         MVC   RCEXIT,=F'4098'     CHKDSNMS not found, link error
         MVC   DJMSG+15(MSG4098L),MSG4098
         B     MYEXIT
ERR#GO   EQU   *
         BAL   R7,MSGPRCR
         B     MYEXIT
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Prefix message                   (R7)   */
*     * /* w/ 15-byte header 'pgmid    nnnn -'                  */
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
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Message Processor                (R7)   */
*     * /* Call CHKMSGDS for message text                       */
*     * /* - R0, R1 Working Register (saved and restored)       */
*     * /* - R14 Working Register                               */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
MSGPRCR  EQU   *                   Entry point 1
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before
         MVC   PMSGNBR,RCEXIT      Load message number
         LA    R1,DJMSG+15         Load DJMSG starting address
         ST    R1,PDMSG
         LA    R1,L'DJMSG-15       Load DJMSG length
         ST    R1,PDMSGL
         MVC   PMSGTXTL,=F'0'      Init msg text length
         B     MSGCONT0            continue....
MSGPRCR2 EQU   *                   Entry point 2
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before
MSGCONT0 EQU   *
         LA    R1,PARMAL           R1 = Parm List
         L     R15,CHKDSNMS        Call CHKDSNMS
         BALR  R14,R15
         ST    R15,SAVER15M        SAVE R15
         LTR   R15,R15             R15=0?  Message Found
         BZ    MSGPRCRX            YES, return
         C     R15,=F'02'          R15=2?  Message Not Found
         BE    MSGPRCRX            YES, return
MSGERR   EQU   *                   NO, abnormal error
         MVC   DJMSG+15(23),=C'* * *Abnormal error det'
         MVC   DJMSG+15+23(05),=C'ected'
         B     MSGCONT1
MSGCONT1 EQU   *
         MVC   DJMSG+15+23+05(14),=C'; CHKDSNMS RC='
         MVC   DJMSG+15+23+05+14(03),=C'xxx'
         CVD   R15,DW              Convert to decimal
         UNPK  DJMSG+15+23+05+14(03),DW+6(02) Unpack and
         OI    DJMSG+15+23+05+14+03-01,X'F0'  ... force and F zone
         L     R15,RCEXIT
         CVD   R15,DW              Convert to decimal
         UNPK  DJMSG+15+23(05),DW+5(03) Unpack and
         OI    DJMSG+15+23+05-01,X'F0'  ... force and F zone
         LM    R0,R1,DW            RESTORE R0,R1  from DW after
         L     R15,SAVER15M        Restore R15
*
MSGPRCRX EQU   *
         BR    R7                  Return to caller
*
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
         LA    R0,DDSNL            Name length and name of Dataset
         ST    R0,DA08PDSN
         MVC   DA08UNIT,=CL8'SYSALLDA'     UNIT=SYSALLDA
         MVC   DA08SER(L'DVOLSER),DVOLSER  VOL=SER=
         MVI   DA08DSP1,DA08SHR            DISP=SHR
         BAL   R5,DAIR             Call DAIR
         LTR   R15,R15             Allocate successful?
         BNZ   PDSALCXT            NO, exit
         LA    R3,MYDAPB           YES, get DDN
         MVC   DDNAME,DA08DDN
         MVI   ALLOCPO,C'Y'        PDS Allocated Flag init to YES
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
         TITLE 'CHKDSN - Literal Pool                                 '
         LTORG
*
         TITLE 'CHKDSN - Constants                                    '
*     * /********************************************************/
*     * /* Parm Input Keywords                                  */
*     * /********************************************************/
KWMBR    DC    C'MBR('             Keyword MBR
KWVOL    DC    C'VOL('             Keyword VOL
KWQUIET  DC    C'QUIET'            Keyword QUIET
KWCATLG  DC    C'CATLG'            Keyword CATLG
*
*     * /********************************************************/
*     * /* Local Messages                                       */
*     * /********************************************************/
MSG4098  DC    C'Cannot load to CHKDSNMS'
MSG4098L EQU   *-MSG4098
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
*     * /********************************************************/
*     * /* PDS DCB as model                                     */
*     * /********************************************************/
***      PRINT GEN
PDSDCB   DCB   DDNAME=DYNAM,DSORG=PO,MACRF=R,EODAD=PDSCLSE,            X
               SYNAD=PDSSYN,RECFM=F,BLKSIZE=256
PDSDCBL  EQU   *-PDSDCB
*
         TITLE 'CHKDSN - Equates                                      '
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
*     * /********************************************************/
*     * /* Other    Equates                                     */
*     * /********************************************************/
DDNAM    EQU   40                  DDNAME DCB offset
*
         TITLE 'CHKDSN - System DSECTs                                '
*     *
*     * /********************************************************/
*     * /* System DSECTs                                        */
*     * /********************************************************/
         IHAPSA                    Prefixed Save Area
PSALEN   EQU   *-PSA               Length of PSA
         EJECT
         CVT   DSECT=YES,LIST=YES  Communication Vector Table
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
ASCBLEN  EQU   *-ASCB              Length of ASCB
         EJECT
UCB      DSECT
         IEFUCBOB  LIST=YES,PREFIX=NO   Unit Control Block
         EJECT
         IKJCPPL                   Command Processor Parm List
CPPLLEN  EQU   *-CPPL              Length of CPPL
         EJECT
         IKJUPT                    User Profile Table
UPTLEN   EQU   *-UPT               Length of UPT
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
         EJECT
*
         PRINT NOGEN
*
         TITLE 'CHKDSN - Working Storage Variables                    '
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
         EJECT
*     * /********************************************************/
*     * /* Dataset  Variable Data Area                          */
*     * /********************************************************/
DDSNL    DS    0F,H                Dataset Name Length (must be here
*                                  before Dataset Name DDSN)
DDSN     DS    CL44                Dataset Name
DJMSG    DS    CL100               MY MESSAGE VARIABLE
DVOLSER  DS    CL6                 Volume Serial Number
DDSCAT   DS    CL1                 Dataset catalog indication Y/N
DDSCATV  DS    CL6                 Dataset catalog volume
*
*     *
*     * /********************************************************/
*     * /* Misc Variables                                       */
*     * /********************************************************/
SAVER15  DS    F                   R15 Hold
SAVER15M DS    F                   R15 Hold for Message Processing
DW       DS    D                   Double Word area
FW       DS    F                   Full Word   area
RCEXIT   DS    F                   Exit RC
CHKDSNMS DS    F                   CHKDSNMS Entry Address
*RC       DS    CL2                 Return Code
AMITSO    DS    C                  TSO Flag
*
         EJECT
*     * /********************************************************/
*     * /* PARM input scan Variables                            */
*     * /********************************************************/
PDSNL    DS    0F,H                Dataset Name Length
PDSN     DS    CL44                Dataset Name from PARM input
PVOLSER  DS    CL6                 VOLSER from PARM input
PMEMBER  DS    CL8                 MEMBER from PARM input
PVOL     DS    C                   VOL   keyword switch
PMBR     DS    C                   MBR   keyword switch
PDASD    DS    C                   DASD  keyword switch
PCATLG   DS    C                   CATLG keyword switch
PMULTI   DS    C                   MULTI keyword switch
PQUIET   DS    C                   QUIET keyword switch
*
IDPREFIX DS    C                   Prefix UserID Y-yes, N-no
MYIDL    DS    H                   Prefix UserID Length
MYID     DS    CL8                 Prefix UserID
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
*     * /* PDS Workareas                                        */
*     * /********************************************************/
DDNAME   DS    CL8                 DDN assigned by Dynamic Alloc

ALLOCPO  DS    CL1                 PDS Allocated Flag

PDSDCBW  DS    0F,(PDSDCBL)X       PDS DCB work area

PARMVL1  DS    F                   Null Parm
*
         EJECT
*     * /********************************************************/
*     * /* Parameter Address List for CHKDSNMS                  */
*     * /********************************************************/
PARMAL   DS    0F
PMSGNBR  DS    F                   Message Number
PDMSG    DS    F                   Starting address to receive text
PDMSGL   DS    F                   Length of area to receive text
PMSGTXTL DS    F                   Length of message text moved
PARMALL  EQU   *-PARMAL            Length of Parameter Address List


         DS    0F,(40)X
WSAREAE  EQU   *                   Working Storage END
WSAREAL  EQU   WSAREAE-WSAREA      Working Storage Area Length
         EJECT
         END   CHKDSN
@@
//CHKDSNSM EXEC  ASML,MBR=CHKDSNMS
//ASM.SYSIN DD DATA,DLM=@@
         TITLE 'CHKDSNMS - Message Processor for CHKDSN               '
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  ==================================================================
*
*   CCC    HH   HH  KK   KK  DDDDD     SSSS   NN   NN  MM   MM   SSSS
*  CC  CC  HH   HH  KK  KK   DD  DD   SS  SS  NN   NN  MMM MMM  SS  SS
* CC       HH   HH  KK K     DD   DD  SS      NNN  NN  MM M MM  SS
* CC       HHHHHHH  KKK      DD   DD   SSSS   NN N NN  MM M MM   SSSS
* CC       HH   HH  KK KK    DD   DD      SS  NN  NNN  MM   MM      SS
*  CC  CC  HH   HH  KK  KK   DD  DD   SS  SS  NN   NN  MM   MM  SS  SS
*   CCC    HH   HH  KK   KK  DDDDD     SSSS   NN   NN  MM   MM   SSSS
*
*  ==================================================================
*
*  Program: CHKDSNMS
*
*     This program stores all messages for CHKDSN to provide a central
*  and common processor to return message text using a parameter
*  address list as follows:
*
*     FW#1  Input     F      Message Number requested
*     FW#2  Input     F      Starting address to received message text
*     FW#3  Input     F      Length of above area to received content
*     FW#4  Output    F      Length of message text moved after call
*
*     L    R1,parm addr list
*     call CHKDSNMS
*     LTR  R15,R15
*     .
*     .
*     .
*
*
*
*
         EJECT
*  CHKDSN Programs Called:
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
*  07/30/2020 1.0.00   Larry Belmontes Jr.
*                      - Initial version released to MVS 3.8J
*                        hobbyist public domain
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
CHKDSNMS CSECT
         USING CHKDSNMS,R10        my BASE REGISTER(S)
         PRINT NOGEN
*
*     * /********************************************************/
*     * /* Save regs and declare base registers R10,R11,R12     */
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
PGMID    DC    CL8'CHKDSNMS'       My Program STAMP
         DC    CL8'MVS3.8J '       OS
         DC    CL8'V1.0.00 '       .Version
         DC    CL8'07302020'       ..date  MMDDCCYY
         DC    CL1' '
PGMDS    DC    CL8'&SYSDATE'       Assemble Date
         DC    CL3' - '            .and
PGMTS    DC    CL8'&SYSTIME'       ..Time
         DC    C'Copyright (C) 2020'
         DC    C'Larry Belmontes, Jr. '
         DC    C'https://ShareABitofIT.net/CHKDSN-in-MVS38J'
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
*     * /* - R4 Starting parms address                          */
*     * /* - R6 Parm Length                                     */
*     * /* - R3, R4, R5 Working Register                        */
*     * /********************************************************/
PARMS    EQU   *                   We have a parm...
         LTR   R1,R1               Do I have a PARM?
         BNZ   PARMIN              YES, check parm length
         MVC   RCEXIT,=F'12'       NO, return with RC=12 (no parm)
         B     MYEXIT              Branch to MYEXIT
PARMIN   EQU   *                   NO,  must be PARM addr
PARMSXT  EQU   *                   We have a parm...
*
         EJECT
*     * /********************************************************/
*     * /* Find and return message text                         */
*     * /* - R1 Starting parms address                          */
*     * /* - R2, R3, R4, R5, R6, R7, R8 Working Register        */
*     * /********************************************************/
         MVC   RCEXIT,=F'0'        Initialize RCEXIT
         USING PARMAL,R1           Tell Assembler
         LA    R2,MSGTENT#         Number of MSGTBL entries
         LA    R3,MSGTBL           Starting address of MSGTBL
         USING TBLMSG,R3           Tell Assembler
         L     R6,PDMSG            Address of PDMSG
         L     R7,PDMSGL           Length of PDMSG
LOOK4MSG EQU   *
         CLC   TMSGNUM,MSGTEOT     End of message table?
         BE    MSGNOTFN            Yes, message not found!
         CLC   TMSGNUM,PMSGNBR     Message number?
         BE    MSGFOUND            Yes, message found
         LA    R3,TBLMSGL(R3)      Bump up to next entry
         BCT   R2,LOOK4MSG         Look again...
*                                  Exhausted msg table, not found!
*                                  Construct not found msg by
*                                  repurposing R3 to use DSECT
         LA    R3,MANUALTE         Reset R3- addrs of manual msg entry
         L     R2,MSGTEOT          End of Msg Table value (x'ffffffff')
         ST    R2,0(R3)
         LA    R2,MSGNFND          Address of MSGNFND message
         ST    R2,4(R3)
         LA    R2,L'MSGNFND        Length of MSGNFND message
         ST    R2,8(R3)
MSGNOTFN EQU   *                   Requested message not found
         MVC   RCEXIT,=F'2'
MSGFOUND EQU   *                   Requested message found
         L     R8,TMSGTXTL         Length of TMSGTXT
         CR    R7,R8               Sufficient length to recv TMSGTXT?
         BL    MSGSMALR            No, use smaller length
         B     MSGOKLEN            YES, use msg txt length
MSGSMALR EQU   *
         LR    R8,R7
MSGOKLEN EQU   *
         BCTR  R8,0                Adjust for EX MVC
         L     R5,TMSGTXT
         EX    R8,MVCMSG           EX MVC of MSG
         LA    R8,1(R8)            Restore length of TMSGTXT
         ST    R8,PMSGTXTL         Set message length moved
         CLC   TMSGNUM,MSGTEOT     End of message table?
         BNE   MYEXIT              No, done...
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
         TITLE 'CHKDSNMS - Literal Pool                               '
         LTORG
*
         TITLE 'CHKDSNMS - Message Address Table / Message Text       '
*     * /********************************************************/
*     * /* Message Address Table                                */
*     * /********************************************************/
MSGTBL   DS    0F
*              MSGNUM ,MSGTXT    ,MSGTXTL
*              -------,----------,-----------
         DC    A(4004),A(MSG4004),A(L'MSG4004)
         DC    A(4009),A(MSG4009),A(L'MSG4009)
         DC    A(4020),A(MSG4020),A(L'MSG4020)
         DC    A(4021),A(MSG4021),A(L'MSG4021)
         DC    A(4025),A(MSG4025),A(L'MSG4025)
         DC    A(4041),A(MSG4041),A(L'MSG4041)
         DC    A(4042),A(MSG4042),A(L'MSG4042)
         DC    A(4043),A(MSG4043),A(L'MSG4043)
         DC    A(4044),A(MSG4044),A(L'MSG4044)
         DC    A(4045),A(MSG4045),A(L'MSG4045)
         DC    A(4046),A(MSG4046),A(L'MSG4046)
         DC    A(4097),A(MSG4097),A(L'MSG4097)
         DC    X'FFFFFFFF',A(MSGNFND),A(L'MSGNFND)     Last Entry
MSGTBLL  EQU   *-MSGTBL            Length of MSGTBL
MSGTENTL EQU   12                  Length of each MSGTBL entry
MSGTENT# EQU   (*-MSGTBL)/MSGTENTL Number of MSGTBL entries
*
MSGTEOT  DC    X'FFFFFFFF'
         EJECT
*     * /********************************************************/
*     * /* Message Text                                         */
*     * /********************************************************/
MSGS     EQU   *
MSGNFND  DC    C'Message nnnnn not found'
MSG4004  DC    C'Parm not supplied'
MSG4009  DC    C'No variables found'
MSG4020  DC    C'RC=xxx LOCATE  CAMLST NAME error'
MSG4021  DC    C'RC=xxx OBTAIN  CAMLST SEARCH error'
MSG4025  DC    C'Cataloged and Request volume not equal'
MSG4041  DC    C'RC=xxx PDS Allocate Error'
MSG4042  DC    C'RC=xxx PDS Open Error'
MSG4043  DC    C'PDS member not found'
MSG4044  DC    C'RC=xxx PDS Close Error'
MSG4045  DC    C'RC=xxx PDS Free Error'
MSG4046  DC    C'DSN not PDS (DSORG=PO) for MBR request'
MSG4097  DC    C'Cannot link to IKJDAIR'
MSGSL    EQU   *-MSGS
*
         TITLE 'CHKDSNMS - Equates                                    '
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
         TITLE 'CHKDSNMS - DSECTS                                     '
*     * /********************************************************/
*     * /* Parameter Address List                               */
*     * /********************************************************/
PARMAL   DSECT
PMSGNBR  DS    F                   Message Number
PDMSG    DS    F                   Starting address to receive text
PDMSGL   DS    F                   Length of area to receive text
PMSGTXTL DS    F                   Length of message text moved
PARMALL  EQU   *-PARMAL            Length of Parameter Address List
*
*     * /********************************************************/
*     * /* Message Address Table                                */
*     * /********************************************************/
TBLMSG   DSECT
TMSGNUM  DS    A                   Message Number
TMSGTXT  DS    A                   Message Text Starting address
TMSGTXTL DS    A                   Message Text Length
TBLMSGL  EQU   *-TBLMSG            Length of Message Address Table
*
         TITLE 'CHKDSNMS - Working Storage Variables                  '
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
         END   CHKDSNMS
@@
//CLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.HELP,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CHKDSN
)F FUNCTION -
  The CHKDSN command checks for existence of a DSN or DSN(member)
  in the system catalog and cataloged volume.  Additionally, CHKDSN
  will search the DSN for existence of a PDS member, if member name is
  provided.

)X Statement SYNTAX  -

         CHKDSN DSN VOL(volser) MBR(member) QUIET CATLG

  REQUIRED - NAME
  DEFAULTS - NONE
  ALIAS    - NONE

)O OPERANDS -
  DSN  is positional and must appear as the first parameter

))NAME     - required, dataset name is fully-qualified when enclosed
             in single quotes.  Dataset name without single quotes
             will be prefixed with USERID.

))VOL      - optional, target volume for uncataloged dataset name.
             Note: System Catalog search is bypassed.

))MBR      - optional, PDS member name
             PDS member name can be part of the DSN (dataset name) or
             provided using the MBR keyword

))QUIET    - optional, suppress display of messages.

))CATLG    - optional, force catalog search.
             Validate cataloged and requested VOL match (equal).
             If not equal, the DSN is considered not found.

             Note: DSN must be catalogued and VOL keyword must be
             provided.

