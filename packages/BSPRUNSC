//BSPRUNSC JOB (JOB),
//             'INSTALL BSPRUNSC',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//ASM     EXEC PGM=IFOX00,PARM='DECK,NOOBJECT,TERM,NOXREF'
//SYSIN    DD  DATA,DLM=@@
BSPRUNSC TITLE 'MVS Auto-Pilot Runscript module'
*********************************************************************** 00213
*                                                                     * 00290
*  This program will read input data from a member of a PDS with      * 00251
*  DDNAME SCRIPTS.  If no SCRIPTS DDNAME is found, SYS1.PARMLIB       * 00253
*  will be used                                                       * 00253
*                                                                     * 00253
*  The member to be used is specified via the PARM passed to the      * 00253
*  program.  If none if specified, a default of BSPRUNSC is used      * 00254
*                                                                     * 00254
*  Syntax of script files:                                            * 00258
*  Leading spaces will be removed from the script line                * 00254
*                                                                     * 00254
*  An asteriks (*) in col 1 means a comment, this line gets ignored   * 00258
*  PARM TEST       - don't exec, just simulate (default),             *
*  PARM CHECK      - same as PARM TEST                                *
*  PARM ECHO       - show commands on console (default)               *
*  PARM NOECHO     - no longer echo commands to console               *
*  PARM EXEC       - Execute the script, ask operator first           *
*  PARM NOREPLYU   - Execute the script, don't ask operator permission*
*  PARM REPLYU     - Next time, ask operator permission again         *
*  WAIT xxx        - Wait specified number of seconds before going on * 00258
*                    Default is 10 seconds                            * 00258
*  COM  ccc        - Execute the command ccc                          * 00258
*  CMD  ccc        - Execute the command ccc, equivalent to COM       * 00258
*  MSG  ttt        - show the message with the text ttttt             * 00258
*  WTO  ttt        - equivalent to MSG                                * 00258
*  IF xxxxxxxx     - execute following command if xxxxxxxx is active  * 00290
*  ELSE            - otherwise execute the second branch              * 00290
*  ENDIF           - end of IF constructs.  IF may not be nested      * 00290
*                                                                     * 00266
*  JCL Execution Parameters are specified via the                     * 00266
*  PARM statement on the EXEC card:                                   * 00266
*                                                                     * 00266
*       PARM=xxxxxxxx - Member name of script to process              * 00266
*                                                                     * 00266
*  Required DD statement: none                                        * 00266
*                                                                     * 00266
*  Optional DD statements:                                            * 00266
*           SCRIPTS  -  Input dataset for control statements          * 00266
*                       Default: SYS1.PARMLIB                         * 00266
*                                                                     * 00290
*           SYSPRINT -  (When running as a batch job)                 * 00266
*                       Default: SYSOUT=A                             * 00266
*                                                                     * 00290
*           SYSUDUMP -  Default: SYSOUT=A                             * 00266
*                                                                     * 00290
*           SNAPDUMP -  (When compiled with &DEBUG=YES)               * 00266
*                       Default: SYSOUT=A                             * 00266
*                                                                     * 00290
*           JCLPDS   -  (When compiled with &DEBUG=YES)               * 00266
*                       Default: SYS1.PARMLIB                         * 00266
*                                                                     * 00290
***********************************************************************
         PRINT OFF,NOGEN
         COPY  BSPGLBLS
         GBLC  &DEBUG
         COPY  BSPSGLBL
&DEBUG   SETC  'NO'
         DCBD  DSORG=(PS),DEVD=DA     , DCB layout
         IEFZB4D0                     , dynalloc dsects
         IEFZB4D2                     , and equates
         PRINT ON,NOGEN               , Macro expansion not needed
BSPRUNSC BSPENTER BASE=(R11,R12),RENT=YES
BLOCKPTR EQU   R6                     , pointer to current block
RECOFFST EQU   R7                     , record pointer within block
RECPTR   EQU   R8                     , address of current record
         BAL   R14,SETINIT            , initialize some variables
         BAL   R14,GETPARAM           , get scriptname from parm
         BAL   R14,ALCPRINT           , allocate SYSPRINT if needed
         BAL   R14,OPNPRINT           , open SYSPRINT if needed
         BAL   R14,ALCUDUMP           , allocate SYSUDUMP if needed
         AIF   ('&DEBUG' EQ 'NO').NOSNAP1
         BAL   R14,ALCPDUMP           , go allocate SNAPDUMP
         BAL   R14,OPNPDUMP           , go open SNAPDUMP
.NOSNAP1 ANOP
         BAL   R14,ALCRIPTS           , allocate SCRIPTS if needed
         BAL   R14,OPNRIPTS           , OPEN the scripts file
         DBGMSG =CL8'SCRIPTS',=CL8'OPENOKAY'
         BAL   R14,TESTMEMB           , Test if req. script in dataset
         BAL   R14,READJFCB           , Read JFCB and get IO buffer
         LR    BLOCKPTR,R1            , R6 ---> IO Buffer(block)
         TITLE 'Main processing loop'
         DO FOREVER                   , Loop through all blocks
          BAL  R14,READBLCK           , read one block
          PRINT GEN
          DO WHILE=(CH,RECOFFST,LT,BLOCKLEN) Loop through recs in block
           DBGMSG EXECMBR,=CL8'READREC' write a debug message
           LA    RECPTR,0(RECOFFST,BLOCKPTR)  RECPTR ---> Record
           BAL   R14,GETLINE          , Remove leading blanks
           BAL   R14,ECHOLINE         , echo the script line if wanted
           BAL   R14,PROCLINE         , process script line
           LA    RECOFFST,80(RECOFFST)  next byte in buffer
          ENDDO                       , end of loop through buffer
         ENDDO                        , end of loop through member
EXIT     DS    0H                     , end of data
         MSGPUT MSG99I                , insert message
         L     R15,MAXCC              , get return code
         CVD   R15,DBL                , make a number
         UNPK  MSG99I1,DBL+5(3)       , make printable
         OI    MSG99I1+L'MSG99I1-1,C'0'      last digit printable
         BAL   R14,PUTMSG             , write last message
         IF    (TM,PROCFLAG,HAVEBUFF,O)
          L    R2,JFCBSAVE+12         , address of buffer
          L    R0,JFCBSAVE+8          , length of buffer
          FREEMAIN R,LV=(0),A=(2)
         ENDIF
         IF    (TM,PROCFLAG,SCRIOPEN,O) close SCRIPTS if open
          DBGMSG =CL8'SCRIPTS',=CL8'CLOSE'
          LA   R3,SCRIPTS             , point to DCB
          MVC   OCLIST,THELIST        , init parmaeter areas
          CLOSE ((R3),FREE),MF=(E,OCLIST)  , and close it
          NI   PROCFLAG,255-SCRIOPEN
         ENDIF (TM,PROCFLAG,SCRIOPEN,O) close SCRIPTS if open
         IF    (TM,PROCFLAG,SNAPOPEN,O) close SNAPDUMP if open
          DBGMSG =CL8'SNAPDUMP',=CL8'CLOSE'
          LA   R3,SNAPDUMP            , point to DCB
          MVC   OCLIST,THELIST
          CLOSE ((R3)),MF=(E,OCLIST)  , and close it
          NI   PROCFLAG,255-SNAPOPEN
         ENDIF (TM,PROCFLAG,SNAPOPEN,O) close SNAPDUMP if open
         IF    (TM,PROCFLAG,PRNTOPEN,O) close PARMDIR if open
          DBGMSG =CL8'SYSPRINT',=CL8'CLOSE'
          LA   R3,SYSPRINT            , point to DCB
          MVC   OCLIST,THELIST
          CLOSE ((R3)),MF=(E,OCLIST)  , and close it
          NI   PROCFLAG,255-PRNTOPEN
         ENDIF (TM,PROCFLAG,PRNTOPEN,O) close PARMDIR if open
         L     R15,MAXCC              , get return code
         BSPRET RC=(15)               , and return with rc in r15
         TITLE 'Subroutines: GETLINE - Read a line from the SCRIPT'
***********************************************************************
*  This routine will take the last input record that was read , will  *
*  remove leading blanks and place the result into the area SCRIPTLIN *
*                                                                     *
*  Registers on ENTRY :  RECPTR = Address of record in buffer         *
*                     :  R14 = return address                         *
*                                                                     *
*  registers on exit  :  unchanged                                    *
***********************************************************************
         SPACE 1
GETLINE  DS    0H                     , Build SCRPTLIN
         STM   R14,R1,GETLSAVE        , save the registers
         DBGMSG EXECMBR,=CL8'SKIPBLNK'  debug message
         MVC   TEMPLINE,0(RECPTR)     , get record into work area
         BLANK SCRPTLIN,SCRPTLIN      , init SCRIPT line
         LA    R1,TEMPLINE            , point to beginning of record
         LA    R0,72                  , no more than 72 bytes are valid
         BAL   R14,NEXTWORD           , position to next word
         IF    (LTR,R15,R15,Z)        , some nonblank character found?
          LA   R15,TEMPLINE           , original buffer
          LA   R14,R1                 , R14 ---> Nonblank part
          SR   R14,R15                , R14 =  offset to nonblank
          LA   R15,72                 , max length
          SR   R15,R14                , - offset = length for move
          BCTR R15,0                  , - 1 for EX
          MVC  SCRPTLIN(*-*),0(R1)    , insert the trimmed text
          EX   R15,*-6                , via EX
         ENDIF                        , exit the routine
         LM    R14,R1,GETLSAVE        , restore the registers
         BR    R14                    , back to caller
         TITLE 'Subroutines: ECHOLINE - Echo the script line'
***********************************************************************
*  This routine will echo the script line to the console unless the   *
*  NOECHO parameter is set or the line is in a non-executed           *
*  conditional block                                                  *
*                                                                     *
*  Registers on ENTRY :  R14 = return address                         *
*                                                                     *
*  Registers on exit  :  unchanged                                    *
***********************************************************************
         SPACE 1
ECHOLINE DS    0H                     , process the PARM statement
         STM   R14,R1,ECHOSAVE        , save registers
         DBGMSG EXECMBR,=CL8'ECHOLINE'
         IF    (CLC,=C'MSG ',NE,SCRPTLIN),AND,                         +
               (CLC,=C'IF ',NE,SCRPTLIN),AND,                          +
               (CLC,=C'ELSE ',NE,SCRPTLIN),AND,                        +
               (CLC,=C'ENDIF ',NE,SCRPTLIN),AND,                       +
               (CLI,SCRPTLIN,NE,C'*'),AND,                             +
               (TM,PARMFLAG,PARMNOEC,Z),AND,                           +
               (TM,PROCFLAG,SKIPLINE,Z),
          MSGPUT MSG08I               , insert message body
          MVC  MSG08I1,SCRPTLIN       , insert script line into msg
          BAL  R14,PUTMSG             , echo the script line
         ENDIF
         LM    R14,R1,ECHOSAVE        , restore rgisters
         BR    R14                    , return to caller
         TITLE 'Subroutines: DOREM - Handle comments'
***********************************************************************
*  This routine handles the comments.  There might be processing flags*
*  in the comments later, but for now they are ignored                *
*                                                                     *
*  Registers on ENTRY :  R14 = return address                         *
*                                                                     *
*  Registers on EXIT  :  unchanged                                    *
***********************************************************************
         SPACE 1
DOREM    DS    0H                     , process the comment
         BR    R14                    , return to caller
         TITLE 'Subroutines: INVCMD - Handle invalid commands'
***********************************************************************
*  This routine handles the invalid commands                          *
*  currently, invalid commands are ignored                            *
*                                                                     *
*  Registers on ENTRY :  R14 = return address                         *
*                                                                     *
*  Registers on EXIT  :  unchanged                                    *
***********************************************************************
         SPACE 1
INVCMD   DS    0H                     , process an invalid statement
         BR    R14                    , return to caller
         TITLE 'Subroutines: SETPARMS - Set processing parameters'
***********************************************************************
*  This routine handles the PARM command and modifies the PARMFLAG    *
*  accordingly                                                        *
*                                                                     *
*  Registers on ENTRY :  R14 = return address                         *
*                                                                     *
*  registers on exit  :  R15 = Return code                            *
*                          0 : PARM processed successfully            *
*                          8 : Invalid PARM statement, ignored        *
***********************************************************************
         SPACE 1
SETPARMS DS    0H                     , process the PARM statement
         SR    R15,R15
         STM   R14,R15,SETPSAVE       , save the registers
         IF    (TM,PROCFLAG,SKIPLINE,Z) if line should not be skipped
          IF   (CLC,=C'NOECHO ',EQ,PARMTEXT)
           OI  PARMFLAG,PARMNOEC
          ELSEIF (CLC,=C'ECHO ',EQ,PARMTEXT)
           NI  PARMFLAG,255-PARMNOEC
          ELSEIF (CLC,=C'TEST ',EQ,PARMTEXT),OR,                       *
               (CLC,=C'CHECK ',EQ,PARMTEXT)
           NI  PARMFLAG,255-PARMNORU-PARMEXEC
          ELSEIF (CLC,=C'EXEC ',EQ,PARMTEXT)
           OI  PARMFLAG,PARMEXEC
          ELSEIF (CLC,=C'NOREPLYU ',EQ,PARMTEXT)
           OI  PARMFLAG,PARMEXEC+PARMNORU
          ELSEIF (CLC,=C'REPLYU ',EQ,PARMTEXT)
           OI  PARMFLAG,PARMEXEC
           NI  PARMFLAG,255-PARMNORU
          ELSE
           LA  R15,8                  , indicate error
           ST  R15,SETPSAVE+4         , set callers R15
          ENDIF (CLC,=C'NOECHO ',EQ,PARMTEXT)
         ENDIF (TM,PROCFLAG,SKIPLINE,Z)
         LM    R14,R3,SETPSAVE        , restore registers
         BR    R14                    , and return to caller
         TITLE 'Subroutines: DOIF - Handle IF statements'
***********************************************************************
*                                                                     *
*  This routine handles the IF statement.  The IF statement has as an *
*  argument a jobname or STC name (up to 8 character).  If this       *
*  Job/STC is active, the commands following the IF line (until ELSE  *
*  line is encountered) will be executed                              *
*                                                                     *
***********************************************************************
         SPACE 1
DOIF     DS    0H                     , process an IF statement
         STM   R14,R3,DOIFSAVE        , save all registers
         DBGMSG EXECMBR,=CL8'DOIF'    , issue debug message
         MVI   WORKFLAG,X'0'          , initialize work field
         USING PSA,0                  , tell assembler
         L     R2,CVTPTR              , get address of CVT pointer
         USING CVT,R2                 , tell assembler
         L     R2,CVTASVT             , get address of ASVT
         USING ASVT,R2                , tell assembler
         LA    R14,ASVTENTY-4         , point to first entry - 1 entry
         L     R15,ASVTMAXU           , number of ASIDs
SCANASVT DS    0H                     , search for AUTOPILOT
         BCTR  R15,R0                 , decrement number of ASISs
         LTR   R15,R15                , Last one?
         BZ    SETELSE                , yes, get out take ELSE branch
         LA    R14,4(R14)             , next ASVT entry
         USING ASVTENTY,R14           , tell assembler
         ICM   R1,B'1111',ASVTENTY    , Get address of ASCB
         BM    SCANASVT               , try next if not active
         USING ASCB,R1                , R3 now points to ASCB
         ICM   R2,B'1111',ASCBJBNS    , get address of STC name
         BZ    CHKJOB1                , if none, must be jobname
         CLC   =CL8'INIT',0(R2)       , is it 'INIT'
         BNE   CHKIT1                 , bif not
CHKJOB1  DS    0H                     , check for jobname
         ICM   R2,B'1111',ASCBJBNI    , address of JOB name
         BZ    SCANASVT               , if not, go around again
CHKIT1   DS    0H                     , is task/job our AUTOPILOT?
         CLC   0(8,R2),SCRPTLIN+3     , test for argument
         BNE   SCANASVT               , if not ours, try next
         DROP  R1,R14                 , not needed any more
SETTRUE  DS    0H                     , The ASCB is active
         NI    WORKFLAG,255-SKIPLINE  , do not skip line for now
         B     DOIFX                  , leave
SETELSE  DS    0H                     , The ASCB is active
         OI    WORKFLAG,SKIPLINE      , Skip lines until matching else
         B     DOIFX                  , leave
DOIFX    DS    0H                     , exit
         OC    PROCFLAG,WORKFLAG      , set skip flag
         LM    R14,R3,DOIFSAVE        , restore registers
         BR    R14                    , return to caller
         TITLE 'Subroutines: DOELSE - Handle ELSE statement'
***********************************************************************
*  The skipline flag needs to be toggled                              *
***********************************************************************
         SPACE 1
DOELSE   DS    0H                     , process an ELSE statement
         XI    PROCFLAG,SKIPLINE      , toggle the flag
         BR    R14                    , and return
         TITLE 'Subroutines: DOENDIF - Handle ENDIF statement'
***********************************************************************
*  The skipline flag needs to be turned off                           *
***********************************************************************
         SPACE 1
DOENDIF  DS    0H                     , process an ENDIF statement
         NI    PROCFLAG,255-SKIPLINE  , turn SKIPLINE flag off
         BR    R14                    , and return
         TITLE 'Subroutines: DOMSG - Handle MSG statement'
***********************************************************************
*  Insert the message text into a WTO line and send it                *
***********************************************************************
         SPACE 1
DOMSG    DS    0H                     , process an ENDIF statement
         IF    (TM,PROCFLAG,SKIPLINE,Z), if line should not be skipped +
               AND,                     and                            +
               (TM,PARMFLAG,PARMEXEC,O) EXECUTION parm is set
          STM  R14,R1,DOMSSAVE        , save register
          MSGPUT MSG09I               , insert message body
          MVC  MSG09I1,SCRPTLIN+4     , insert MSG text
          WTO  MF=(E,THEWTO)          , show the message
          LM   R14,R1,DOMSSAVE        , restore registers
         ENDIF (TM,PROCFLAG,SKIPLINE,Z) if line should not be skipped
         BR    R14                    , and return to caller
         TITLE 'Issue a command to the console'
***********************************************************************
* This is a line with the COM or CMD statement                        *
***********************************************************************
         SPACE 1
DOCMD    DS    0H                     , issue an operator command
         IF    (TM,PROCFLAG,SKIPLINE,Z), if line should not be skipped +
               AND,                     and                            +
               (TM,PARMFLAG,PARMEXEC,O) EXECUTION parm is set
          STM  R14,R1,DOCMSAVE        , save registers
          IF   (TM,PARMFLAG,PARMNORU,Z)
           BLANK WTORTEXT             , clear message area
           MVC WTORTEXT(L'MSG16D),MSG16D insert message text
           LA  R1,L'MSG16D+4          , length of message
           STH R1,THEWTOR+8           , put into WTOR parmlist
           XC  WTOECB,WTOECB          , clear the ECB
           WTOR ,WTOREPLY,L'WTOREPLY,WTOECB,MF=(E,THEWTOR)
           WAIT ECB=WTOECB            , wait until operator re
           OC  WTOREPLY,=C' '         , convert to uppercase
           IF  (CLI,WTOREPLY,EQ,C'U')
            OI PARMFLAG,PARMNORU
           ELSE
            NI PARMFLAG,255-PARMNORU  , disallow
            NI PARMFLAG,255-PARMEXEC  , command execution
            MSGPUT MSG12E
            BAL R14,PUTMSG
            SETMAXCC 8
           ENDIF (CLI,WTOREPLY,EQ,C'U')
          ENDIF (TM,PARMFLAG,PARMNORU,Z)
          IF   (TM,PARMFLAG,PARMEXEC,O)
           MSGPUT SCRPTLIN+4          , insert command text
           BSPAUTH ON
           MODESET KEY=ZERO           , become big-boss
           SR  R0,R0                  , clear flags
           LA  R1,THEWTO              , point to command text
           SVC 34                     , issue OS command
           MODESET KEY=NZERO          , back to normal again
           BSPAUTH OFF
          ENDIF (TM,PARMFLAG,PARMEXEC)
          LM   R14,R1,DOCMSAVE        , get return address
          ENDIF (TM,PROCFLAG,SKIPLINE,Z) if line should not be skipped
         BR    R14                    , return to caller
         TITLE 'Wait for some time'
***********************************************************************
* This routine takes the parameter following the WAIT statement       *
* converts it to a number (if it is valid, that is), and issues       *
* an STIMER WAIT for delaying a bit                                   *
***********************************************************************
DOWAIT   DS    0H
         STM   R14,R5,DOWASAVE        , save registers
         IF    (TM,PROCFLAG,SKIPLINE,Z),  if line should not be skipped+
               AND,                     and                            +
               (TM,PARMFLAG,PARMEXEC,O) EXECUTION parm is set
          LA   R0,66                  , maximum number of bytes to test
          LA   R1,SCRPTLIN+5          , first data byte
          BAL  R14,NEXTWORD           , find next word in scriptline
          IF   (LTR,R15,R15,Z)        , found it?
*                                     , R1 = address of string
*                                     , R0 = length of string
           BAL R14,NUMTEST            , test if numeric
           IF  (LTR,R15,R15,Z)        , if yes
            LR   R2,R0                , R2 = length
            BCTR R2,0                 , minus one for EX
            PACK DBL,0(*-*,R1)        , make packed
            EX   R2,*-6               , via execute
            CVB  R15,DBL              , make binary
            MH   R15,=H'100'          , time 100
           ELSE                       , if not numeric, use default
            L    R15,DFLTTIME         , load default wait time
           ENDIF                      , test if numeric
          ELSE
           L   R15,DFLTTIME           , load default wait time
          ENDIF
          ST   R15,WAITTIME           , set the wait time
          STIMER WAIT,BINTVL=WAITTIME  , wait a moment
         ENDIF (TM,PROCFLAG,SKIPLINE,Z)   if line should not be skipped
         LM    R14,R5,DOWASAVE        , restore registers
         BR    R14                    , return to caller
         TITLE 'Subroutine NUMTEST - Test if valid zoned numeric field'
***********************************************************************
*                                                                     *
* Registers on Entry:      R1  =   address of field to test           *
*                          R0  =   Length to test                     *
*                          R14 =   Return address                     *
*                                                                     *
* Registers on Exit:      R15 = 0 ===> Field is numeric               *
*                         R15 = 4 ===> Field is NOT numeric           *
*                                                                     *
***********************************************************************
         SPACE 1
NUMTEST  DS    0H                     , test vor valis zoned numeric
         STM   R14,R3,NUMSAVE         , save register
         LR    R3,R0                  , R0 = length, provided by caller
         BCTR  R3,0                   , Decrement for excecute
         BLANK TRTTABLE,CHAR=X'01'    , Init table for TRT
         XC    TRTTABLE+C'0'(10),TRTTABLE+C'0'
         TRT   0(*-*,R1),TRTTABLE     , Do the translate
         IF    (EX,R3,*-6,Z)          , if all position valid
          LA   R15,0                  , set RC=0
         ELSE                         , set error RC
          LA   R15,4                  , otherwise set RC=4
         ENDIF                        , test if all positions valid
         ST    R15,NUMSAVE+4          , set caller's R15
         LM    R14,R3,NUMSAVE         , restore register
         BR    R14
         TITLE 'Subroutines: NEXTWORD - Find the next delimited word'
***********************************************************************
* This routine finds tries to locate the next word in a string.       *
* The delimiter can be ' ' or ', or the word can be right at the      *
* beginning, or right at the end, of the main string                  *
*                                                                     *
* Registers on entry:  R0  = length of string to scan                 *
*                      R1  = address of string to scan                *
*                                                                     *
* Registers on exit:   R15 = Returncode                               *
*                        0 ==> Search was successfull                 *
*                        8 ==> Search failed                          *
*                      R0  = length of word                           *
*                      R1  = address of word                          *
***********************************************************************
NEXTWORD DS    0H
         STM   R14,R4,NEXTSAVE        , save registers
         SR    R15,R15                , init return code
         LR    R2,R0                  , R2 = string length
         LR    R3,R1                  , R2 = string length
NEXTA    DS    0H                     , loop till first nonblank
         CLI   0(R3),C' '             , is this a space
         BNE   NEXTB                  , okay, found beginning of word
         LA    R3,1(R3)               , else bump to next byte
         BCT   R2,NEXTA               , and go around again
NEXTB    DS    0H                     , R1 ---> Nonblank character
         ST    R3,NEXTSAVE+12         , place into caller's regs
         SR    R4,R4                  , counter for nonblanks
NEXTC    DS    0H                     , Loop until blank
         CLI   0(R3),C' '             , Found a blank?
         BE    NEXTD                  , yep, we are nearly done
         LA    R3,1(R3)               , next byte in buffer
         LA    R4,1(R4)               , add one to counter
         BCT   R2,NEXTC               , go around again
NEXTD    DS    0H                     , either end of buffer or blank
         ST    R4,NEXTSAVE+8          , place counter into caller's reg
         LA    R15,0                  , load return code
         B     NEXTX                  , and exit
NEXT08   DS    0H                     , no nonblank character found
         LA    R15,8                  , load return code
         SR    R0,R0                  , load return value 1
         SR    R1,R1                  , load return value 2
         STM   R0,R1,NEXTSAVE+8       , place into caller's regs
         B     NEXTX                  , and exit
NEXTX    DS    0H                     , leave
         ST    R15,NEXTSAVE+4         , set callers return code
         LM    R14,R4,NEXTSAVE        , restore registers
         BR    R14                    , return to caller
         TITLE 'Initialize some variables'
***********************************************************************
* Initialze some variables, set processing options according to       *
* runtime environmaent (STC or BATCH)                                 *
***********************************************************************
         SPACE 1
SETINIT  DS    0H                     , Initialize some variables
         STM   R14,R1,SETISAVE        , save registers
         DBGMSG =CL8'        ',=CL8'SETINIT'
         XC    MEMCC,MEMCC            , return code field
         XC    MAXCC,MAXCC            , return code field
         XC    LASTCC,LASTCC          , return code field
         MVI   PARMFLAG,X'0'          , clear parm flag
         MVI   PROCFLAG,X'0'          , clear parm flag
         ZAP   LINENUM,=P'100'        , force page break
         ZAP   PAGENUM,=P'0'          , init page number
         XC    PARMFLAG,PARMFLAG      , clear parm flag
         XC    PROCFLAG,PROCFLAG      , clear parm flag
         XC    JFCBAREA,JFCBAREA      , clear JFCB
         TESTENV                      , STC? BATCH? TSO?
         IF    (CH,R1,EQ,=H'0')       , is this a batch job
          OI   PROCFLAG,ISJOB         , remember in process option flag
         ELSEIF (CH,R1,EQ,=H'4')      , is this an STC
          OI   PROCFLAG,ISSTC         , put indicator to flag
         ELSE                         , it is not supported
          BLANK MSGTEXT
          MSGPUT MSG14E               ,  insert message text
          BAL  R14,PUTMSG             , issue message
          SETMAXCC 12                 , get out with RC=12
          B    EXIT
         ENDIF
         LM    R14,R1,SETISAVE        , restore all register
         BR    R14                    , and return to caller
         TITLE 'Get JCL Parameter'
***********************************************************************
* Registers on entry:                                                 *
*       R1  = address of parameter list                               *
*       R14 = Return address                                          *
* Registers on exit:                                                  *
*       All registers are restored                                    *
*       EXECMBR : Name of script to run                               *
*       EXECMBL : Length of membername                                *
*       MAXCC   : might be set to 4 if Parm was omitted or too long   *
***********************************************************************
         SPACE 1
GETPARAM DS    0H                     , Get JCL Parameter
         STM   R14,R1,GETPSAVE        , save all registers
         DBGMSG =CL8'        ',=CL8'GETPARAM'
         BLANK EXECMBR                , clear parm text area
         L     R1,0(0,R1)             , Address of passed parm
         LH    R15,0(0,R1)            , R15 = Length of parameter
         LA    R1,2(0,R1)             , R1  = Address of Parameter
         IF    (LTR,R15,R15,NZ)       , any parameter provided?
          BCTR R15,0                  , decrement for EX
          IF   (CH,R15,H,=H'7')       , test for maximum
           LA  R15,7                  , load maximum
           MSGPUT MSG03W              , insert message body
           BAL R14,PUTMSG             , and show the message
           SETMAXCC 4                 , set warning RC
          ENDIF (CH,R15,H,=H'7')      , test for maximum
          MVC  EXECMBR(*-*),0(R1)     , move in parameters
          EX   R15,*-6                , move in the parm text
          OC   EXECMBR,=CL8' '        , upper case
          LA   R15,1(R15)             , load real length
         ELSE                         , no parm given
          MSGPUT MSG04W               , build message
          MVC  EXECMBR,=C'BSPRUNSC'   , default script name
          LA   R15,8                  , load length
          SETMAXCC 4                  , set warning RC
         ENDIF                        , test if parm given
         STCM  R15,B'0011',EXECMBL    , set length
         LM    R14,R1,GETPSAVE        , restore all registers
         BR    R14                    , and return to caller
         TITLE 'Subroutines - Allocate SYSUDUMP if needed'
***********************************************************************
* Of course this program never has problems, but we allocated a       *
* SYSUDUMP DD for SYSOUT=A anyways                                    *
***********************************************************************
ALCUDUMP DS    0H                     , allocate SYSUDUMP
         STM   R14,R1,ALCUSAVE        , save registers
         DBGMSG =CL8'SYSUDUMP',=CL8'ALLOC'
         MVC   TEMPDDN,TUSDDDNM       , insert DDNAME into msg text
         MVC   SVC99WA(TUSDLEN),TUSDPTR move text units to WS
         LA    R1,SVC99WA+TUSDDDN-TUSDPTR  point to DDNAME
         ST    R1,SVC99P1             , put into TU list
         LA    R1,SVC99WA+TUSDCLS-TUSDPTR  , point to CLASS parm
         ICM   R1,B'1000',=XL1'80'    , indicate last parm
         ST    R1,SVC99P2             , put into TU list
         LA    R1,SVC99WA             , point to work area
         BAL   R14,DOSVC99            , go and do it
         IF    (LTR,R15,R15,NZ)       , Error on DYNALLOC?
          WTO  MF=(E,THEWTO)          , tell the user
          SETMAXCC 12                 , and end with RC=12
          B    EXIT
         ENDIF (LTR,R15,R15,NZ)       , Error on DYNALLOC
         LM    R14,R1,ALCUSAVE        , restore registers
         BR    R14                    , return to caller
***********************************************************************
* SYSUDUMP DYNALLOC parameters                                        *
***********************************************************************
         SPACE 1
TUSDPTR  DS    0F                     , text unit pointers
         DC    A(TUSDDDN)             , address of DDNAME
         DC    X'80'                  , end of list indicator
         DC    AL3(TUSDCLS)           , address of SYSOUT CLASS info
TUSDDDN  DC    AL2(DALDDNAM)          , key for DDNAME
         DC    AL2(1)                 , number of entries
         DC    AL2(8)                 , length od 1 entry
TUSDDDNM DC    CL8'SYSUDUMP'          , contens of entry
TUSDCLS  DC    AL2(DALSYSOU)          , key for SYSOUT
         DC    AL2(1)                 , number of entries
         DC    AL2(1)                 , length of 1 entry
         DC    C'A'                   , sysout class
TUSDLEN  EQU   *-TUSDPTR
         AIF   ('&DEBUG' EQ 'NO').NOALCP
         TITLE 'Subroutines - ALCPDUMP: Allocate SNAPDUMP'
***********************************************************************
* Allocate SNAPDUMP DD if not already present                         *
***********************************************************************
         SPACE
ALCPDUMP DS    0H                     , allocate SNAPDUMP
         STM   R14,R1,ALCSSAVE        , save registers
         DBGMSG =CL8'SNAPDUMP',=CL8'ALLOC'
         MVC   TEMPDDN,TUSNDDNM       , insert DDNAME into msg text
         MVC   SVC99WA(TUSNLEN),TUSNPTR move text units to WS
         LA    R1,SVC99WA+TUSNDDN-TUSNPTR  point to DDNAME
         ST    R1,SVC99P1             , put into TU list
         LA    R1,SVC99WA+TUSNCLS-TUSNPTR  , point to CLASS parm
         ICM   R1,B'1000',=XL1'80'    , indicate last parm
         ST    R1,SVC99P2             , put into TU list
         LA    R1,SVC99WA             , point to work area
         BAL   R14,DOSVC99            , go and do it
         IF    (LTR,R15,R15,NZ)       , Error on DYNALLOC?
          BAL  R14,PUTMSG             , tell the user
          SETMAXCC 12                 , and end with RC=12
          B    EXIT
         ENDIF (LTR,R15,R15,NZ)       , Error on DYNALLOC
         LM    R14,R1,ALCSSAVE        , restore registers
         BR    R14                    , return to caller
         SPACE 2
***********************************************************************
* SNAPDUMP DYNALLOC parameters                                        *
***********************************************************************
         SPACE
TUSNPTR  DS    0F                     , text unit pointers
         DC    A(TUSNDDN)             , address of DDNAME
         DC    X'80'                  , end of list indicator
         DC    AL3(TUSNCLS)           , address of SYSOUT CLASS info
TUSNDDN  DC    AL2(DALDDNAM)          , key for DDNAME
         DC    AL2(1)                 , number of entries
         DC    AL2(8)                 , length od 1 entry
TUSNDDNM DC    CL8'SNAPDUMP'          , contens of entry
TUSNCLS  DC    AL2(DALSYSOU)          , key for SYSOUT
         DC    AL2(1)                 , number of entries
         DC    AL2(1)                 , length of 1 entry
         DC    C'A'                   , sysout class
TUSNLEN  EQU   *-TUSNPTR
.NOALCP  ANOP
         TITLE 'Allocate SYSPRINT for JOB if needed'
***********************************************************************
* Allocate SYSPRINT DD if not already present                         *
***********************************************************************
         SPACE
ALCPRINT DS    0H                     , allocate SYSPRINT DD
         STM   R14,R1,ALCPSAVE        , save the registers
         DBGMSG =CL8'SYSPRINT',=CL8'ALLOC'
         IF    (TM,PROCFLAG,ISJOB,O)  , Is this a batch job?
          MVC  TEMPDDN,TUSPDDNM       , insert DDNAME into msg text
          MVC  SVC99WA(TUSPLEN),TUSPPTR move text units to WS
          LA   R1,SVC99WA+TUSPDDN-TUSPPTR  point to DDNAME
          ST   R1,SVC99P1             , put into TU list
          LA   R1,SVC99WA+TUSPCLS-TUSPPTR  , point to CLASS parm
          ICM  R1,B'1000',=XL1'80'    , indicate last parm
          ST   R1,SVC99P2             , put into TU list
          LA   R1,SVC99WA             , point to work area
          BAL  R14,DOSVC99            , go and do it
          IF   (LTR,R15,R15,NZ)       , Error on DYNALLOC?
           BAL R14,PUTMSG             , tell the user
           SETMAXCC 12                , and end with RC=12
           B   EXIT
          ENDIF (LTR,R15,R15,NZ)      , Error on DYNALLOC?
         ENDIF (TM,PROCFLAG,ISJOB)    , Is this a batch job?
         LM    R14,R1,ALCPSAVE        , restore the registers
         BR    R14                    , and return to caller
         SPACE 2
***********************************************************************
* SYSPRINT DYNALLOC parameters                                        *
***********************************************************************
         SPACE
TUSPPTR  DS    0F                     , text unit pointers
         DC    A(TUSPDDN)             , address of DDNAME
         DC    X'80'                  , end of list indicator
         DC    AL3(TUSPCLS)           , address of SYSOUT CLASS info
TUSPDDN  DC    AL2(DALDDNAM)          , key for DDNAME
         DC    AL2(1)                 , number of entries
         DC    AL2(8)                 , length od 1 entry
TUSPDDNM DC    CL8'SYSPRINT'          , contens of entry
TUSPCLS  DC    AL2(DALSYSOU)          , key for SYSOUT
         DC    AL2(1)                 , number of entries
         DC    AL2(1)                 , length of 1 entry
         DC    C'X'                   , sysout class
TUSPLEN  EQU   *-TUSPPTR
         TITLE 'Subroutines - ALCRIPTS: Allcate SCRIPTSD DD'
***********************************************************************
* Allocate SCRIPTS DD if not already present                          *
***********************************************************************
         SPACE
ALCRIPTS DS    0H                     , allocate PARMLIB DD
         STM   R14,R1,ALCRSAVE        , save registers
         DBGMSG =CL8'SCRIPTS',=CL8'ALLOC'
         MVC   TEMPDDN,TUSCDDNM       , insert DDNAME into msg text
         MVC   SVC99WA(TUSCLEN),TUSCPTR move text units to WS
         LA    R1,SVC99WA+TUSCDDN-TUSCPTR  point to DDNAME
         ST    R1,SVC99P1             , put into TU list
         LA    R1,SVC99WA+TUSCDSN-TUSCPTR  , point to DSN parm
         ST    R1,SVC99P2             , put into TU list
         LA    R1,SVC99WA+TUSCSTAT-TUSCPTR  , point to DISP parm
         ICM   R1,B'1000',=XL1'80'    , indicate last parm
         ST    R1,SVC99P3             , put into TU list
         LA    R1,SVC99WA             , point to work area
         BAL   R14,DOSVC99            , go and do it
         IF    (LTR,R15,R15,NZ)       , Error on DYNALLOC?
          BAL  R14,PUTMSG             , tell the user
          SETMAXCC 12                 , set RC12 or higher
          B    EXIT                   , and terminate the program
         ENDIF (LTR,R15,R15,NZ)       , Error on DYNALLOC
         LM    R14,R1,ALCRSAVE        , restore registers
         BR    R14                    , return to caller
         SPACE 2
***********************************************************************
* PARMLIB DYNALLOC parameters                                         *
***********************************************************************
         SPACE
TUSCPTR  DS    0F                     , text unit pointers
         DC    A(TUSCDDN)             , address of DDNAME
         DC    A(TUSCDSN)             , address of DSNAME
         DC    X'80'                  , end of list indicator
         DC    AL3(TUSCSTAT)          , address of STATUS
TUSCDDN  DC    AL2(DALDDNAM)          , key for DDNAME
         DC    AL2(1)                 , number of entries
         DC    AL2(8)                 , length od 1 entry
TUSCDDNM DC    CL8'SCRIPTS '          , contents of entry
TUSCDSN  DC    AL2(DALDSNAM)          , key for DDNAME
         DC    AL2(1)                 , number of entries
         DC    AL2(44)                , length od 1 entry
         DC    CL44'SYS1.PARMLIB'     , contens of entry
TUSCSTAT DC    AL2(DALSTATS)          , key for STATUS (initial DISP)
         DC    AL2(1)                 , number of entries
         DC    AL2(1)                 , length od 1 entry
         DC    X'08'                  , X'08' = OLD
*                                     , X'04' = NEW
*                                     , X'02' = MOD
*                                     , X'01' = OLD
TUSCLEN  EQU   *-TUSCPTR
         TITLE 'Subroutines: OPNPRINT - Open SYSPRINT'
***********************************************************************
* Open SYSPRINT DD for output processing (for a job)                  *
***********************************************************************
         SPACE
OPNPRINT DS    0H                     , open SYSPRINT
         STM   R14,R1,OPNPSAVE        , save registers
         DBGMSG =CL8'SYSPRINT',=CL8'OPNPRINT'
         IF    (TM,PROCFLAG,ISJOB,O)  , Is this a batch job?
          MVC  SYSPRINT,SYSPRDCB      , move DCB to reentrant storage
          LA   R1,SYSPRINT            , point to SYSPRINT DCB
          BAL  R14,OPENFILE           , go open the file
          IF   (LTR,R15,R15,Z)        , if successful
           OI  PROCFLAG,PRNTOPEN      , indicate that SYSPRINT is open
          ENDIF (LTR,R15,R15,Z)       , if successful
         ENDIF (TM,PROCFLAG,ISJOB)    , Is this a batch job?
         LM    R14,R1,OPNPSAVE        , restore registers
         BR    R14                    , return to caller
         PUSH  PRINT
         PRINT NOGEN
SYSPRDCB DCB   DDNAME=SYSPRINT,       , ddname for this file           -
               DSORG=PS,              , file is sequential             -
               LRECL=133,             , record length                  -
               BLKSIZE=1330,          , block size                     -
               MACRF=(PM),            , will be opened for output      -
               RECFM=FBA              , fixed block, ansi cntlchars
SYSPRLEN EQU   *-SYSPRDCB             , length of DCB
         TITLE 'Subroutines: OPNPDUMP - Open SNAPDUMP'
***********************************************************************
* Open SNAPDUMP for SNAP SVC processing                               *
***********************************************************************
         SPACE
OPNPDUMP DS    0H                     , open SNAPDUMP
         STM   R14,R1,OPNSSAVE        , save registers
         DBGMSG =CL8'SNAPDUMP',=CL8'OPEN'
         MVC   SNAPDUMP,SNAPDDCB      , move DCB to reentrant storage
         LA    R1,SNAPDUMP            , point to SNAPDUMP DCB
         BAL   R14,OPENFILE           , go open the file
         IF    (LTR,R15,R15,Z)        , if successful
          OI   PROCFLAG,SNAPOPEN      , indicate that SNAPDUMP is open
         ENDIF  (LTR,R15,R15,Z)       , if successful
         LM    R14,R1,OPNSSAVE        , restore registers
         BR    R14                    , return to caller
         PUSH  PRINT
         PRINT NOGEN
SNAPDDCB DCB   DDNAME=SNAPDUMP,       , ddname for this file           -
               DSORG=PS,              , file is sequential             -
               LRECL=125,             , record length                  -
               BLKSIZE=1632,          , and blocksize                  -
               MACRF=W,               , will be opened for output      -
               RECFM=VBA              , fixed block, ansi cntlchars
SNAPDLEN EQU   *-SNAPDDCB             , length of DCB
         POP   PRINT
         TITLE 'Subroutines: OPNRIPTS - Open SCRIPTS PDS'
***********************************************************************
* Open SCRIPTS PDS                                                    *
***********************************************************************
         SPACE 1
OPNRIPTS DS    0H                     , open SCRIPTS PDS
         STM   R14,R1,OPNCSAVE        , save registers
         DBGMSG =CL8'SCRIPTS',=CL8'OPEN'
         MVC   SCRIPTS,SCRIPDCB       , move DCB to reentrant storage
         LA    R1,SCRIPTS             , point to DCB
         USING IHADCB,R1
         LA    R15,JFCBAREA           , point to JFCB input area
         ICM   R15,B'1000',=X'87'     , indicate JFCB exit
         ST    R15,JFCBXLST           , put into exit list
         LA    R15,JFCBXLST           , get address of list
         STCM  R15,B'0111',DCBEXLSA   , and put into DCB
         BAL   R14,OPENFILE           , go open the file
         IF    (LTR,R15,R15,Z)        , if successful
           OI  PROCFLAG,SCRIOPEN      , indicate that PARMLIB is open
         ELSE                         , when open failed
           SETMAXCC 12                , exit with RC=12
           B   EXIT
         ENDIF (LTR,R15,R15,Z)        , if successful
         LM    R14,R1,OPNCSAVE        , restore registers
         BR    R14                    , return to caller
         PUSH  PRINT
         PRINT NOGEN
SCRIPDCB DCB   DDNAME=SCRIPTS,                                         -
               DSORG=PO,                                               -
               MACRF=R,                                                -
               EODAD=EXIT
SCRIPLEN EQU   *-SCRIPDCB             , length of this DCB
         DROP  R1
         TITLE 'Subroutines: DOSVC99 - Perform DYNALLOC functions'
***********************************************************************
* Allocate a file to the current JOB/STC using SVC99.                 *
*                                                                     *
* Registers on entry:  R1 --->  Text Unit parameter list              *
*      field TEMPDDN:  DDNAME to be allocated                         *
*                                                                     *
* Registers on exit:   R15 = Retrun code                              *
*                        0 : Allocation successful                    *
*                  nonzero : RC from DYNALLOC call                    *
*      field MSGTEXT: error message text                              *
***********************************************************************
DOSVC99  DS    0H                     , perform DYNALLOC functions
         STM   R14,R3,DOSVSAVE        , save registers
         DBGMSG TEMPDDN,=CL8'*SVC 99*'
         LR    R3,R1                  , point to Text Unit Pointerlist
         DEVTYPE TEMPDDN,DEVTYPE      , Test for DD card
         IF    (LTR,R15,R15,NZ)       , if no DD card
          LA   R2,REQBLK              , point to request block
          USING S99RB,R2              , tell assembler
          XC   REQBLK,REQBLK          , clear RB
          MVI  S99RBLN,REQBLKLN       , set up length
          MVI  S99VERB,S99VRBAL       , indicate ALLOC function
          ST   R3,S99TXTPP            , put into Request block
          LA   R3,RBPTR               , Point to RB pointer
          USING S99RBP,R3             , tell assembler
          ST   R2,S99RBPTR            , st RB address into RB PTR
          OI   S99RBPTR,S99RBPND      , turn on high order bit
          LA   R1,RBPTR               , get addres of RB pointer
          DYNALLOC                    , issue SVC 99
          ST   R15,DOSVSAVE+4         , set caller's R15
          XR   R15,R15                , clear R15
          ICM  R15,B'0011',S99ERROR   , get error code
          ST   R15,DOSVSAVE+8         , place into caller's R0
          MSGPUT MSG01E               , insert message text
          MVC  MSG01E1,TEMPDDN        , insert DDNAME into message
          X2CHRTRN MSG01E2,DOSVSAVE+06,LEN=2
          X2CHRTRN MSG01E3,DOSVSAVE+10,LEN=2
         ELSE
          ST   R15,DOSVSAVE+4         , set caller's R15
          MSGPUT MSG20I               , set up message text
         ENDIF
         LM    R14,R3,DOSVSAVE        , restore return address
         BR    R14                    , and retrun to caller
         DROP  R2,R3                  , not needed any more
         TITLE 'Subroutine OPENFILE - Open files as needed'
***********************************************************************
* Open a file, report any errors if open fails                        *
*                                                                     *
* Registers on entry:  R1  = address of DCB to be opened              *
*                      R14 = Return address                           *
*                                                                     *
* Registers on exit:   R15 = Returncode                               *
*                        0 ==> OPEN successful                        *
*                        8 ==> OPEN failed                            *
***********************************************************************
         SPACE 1
OPENFILE DS    0H                     , Open routine
         STM   14,3,OPENSAVE          , save registers
         LR    R3,R1                  , Point to DCB
         USING IHADCB,R3              , tell assembler
         DBGMSG DCBDDNAM,=CL8'OPENFILE'
         MVC   TEMPDDN,DCBDDNAM       , insert DD name into msg
         MVC   OCLIST,THELIST         , Set up open/close list
         AIF   ('&DEBUG' EQ 'YES').OPNSNAP
         IF    (CLC,DCBDDNAM,EQ,TUSPDDNM)         SYSPRINT?
         AGO   .OPNCONT
.OPNSNAP ANOP
         IF    (CLC,DCBDDNAM,EQ,TUSPDDNM),OR,     SYSPRINT?            +
               (CLC,DCBDDNAM,EQ,TUSNDDNM)      or SNAPDUMP?
         AGO   .OPNCONT
.OPNCONT ANOP
          OPEN ((R3),OUTPUT),MF=(E,OCLIST)
         ELSE
          OPEN ((R3),INPUT),MF=(E,OCLIST)
         ENDIF
         IF    (TM,DCBOFLGS,DCBOFOPN,O) if open was successfull
          LA   R15,0                  , clear return code
         ELSE                         , when open failed
         MSGPUT MSG02E                , insert message body
         MVC MSG02E1,TEMPDDN
          BAL R14,PUTMSG              , issue the message
          LA  R15,8                   , load error RC
         ENDIF (TM,DCBOFLGS,DCBOFOPN,O) if open was okay
         ST    R15,OPENSAVE+4         , set caller's R15
         LM    14,3,OPENSAVE          , restore registers
         BR    R14                    , and return to caller
         DROP  R3                 , not needed outside this module
         TITLE 'Subroutines: READJFCB: Get Job File Control Block'
***********************************************************************
* Read JFCB of PARMLIB to get block size, then acquire a virtual      *
* storage area in that size.                                          *
*                                                                     *
* Registers on Entry: R14 = Return address                            *
*                                                                     *
* Registers on Exit   R1  = Address of SCRIPTS IO buffer              *
*                                                                     *
***********************************************************************
         SPACE 1
READJFCB DS    0H                     , Read JFCB into memory
         STM   R14,R1,JFCBSAVE        , save registers
         DBGMSG TUSCDDNM,=CL8'RDJFCB' , issue debug message
         MVC   OCLIST,THELIST         , init open/close list
         RDJFCB (SCRIPTS),MF=(E,OCLIST) now get the JFCB
         IF    (LTR,R15,R15,NZ)       , RDJFCB failed
          MSGPUT MSG06E               , point to error message
          BAL  R14,PUTMSG             , show the message
          SETMAXCC 12                 , set RC = 12 or higher
          B    EXIT                   , and leave the program
         ELSE
          LH   R0,JFCBLKSI            , block length from JFCB
          ST   R0,JFCBSAVE+8          , save in caller's R0
          GETMAIN R,LV=(0)            , get area from MVS
          ST   R1,JFCBSAVE+12         , put into caller's R1
          OI   PROCFLAG,HAVEBUFF      , indicate we have a buffer
          MSGPUT MSG22I               , insert body part of message
          MVC  MSG22I1,JFCBDSNM       , insert dsname
          BAL  R14,PUTMSG             , show message
          MSGPUT MSG23I               , insert body part of message
          MVC  MSG23I1,JFCBVOLS       , insert dsname
          BAL  R14,PUTMSG             , show message
         ENDIF
         LM    R14,R1,JFCBSAVE        , rstore registers
         BR    R14                    , and back to caller
         TITLE 'Subroutines - TESTMEMB: Test if member exists'
***********************************************************************
* This routine uses the FIND macro instruction to determine if a      *
* requested member is in the directory of the SCRIPTS file            *
*                                                                     *
* Registers on Entry: R14 = Return address                            *
*                                                                     *
* On exit the control blocks are set in such a way that the next      *
* READ macro instruction will read the first block of the named       *
* member.  If the member was not available, we exit from the program  *
* with RC=8                                                           *
*                                                                     *
***********************************************************************
TESTMEMB DS    0H                     , test if member exists
         STM   R14,R1,TESTSAVE        , save registers
         DBGMSG EXECMBR,=CL8'TESTMEMB'
         FIND  SCRIPTS,EXECMBR,D      , search directory
         ST    R15,TESTSAVE+4         , set caller's R15
         IF    (LTR,R15,R15,NZ)       , any error
          MSGPUT MSG07E               , point to error message
          MVC   MSG07E1,EXECMBR       , insert member name
          BAL   R14,PUTMSG            , Display error message
          SETMAXCC 8                  , SET RC to 8 or higher
          B     EXIT                  , and leave
         ENDIF                        , of error processing
         LM    R14,R1,TESTSAVE        , restore registers
         BR    R14                    , retrun to caller
         TITLE 'Subroutines - READBLCK: Read a block from SCRIPTS'
***********************************************************************
* Read a block from SCRUPTS member, and reset the RECOFFST reg        *
***********************************************************************
         SPACE 1
READBLCK DS    0H                     , read a block from SCRIPTS
         STM   R14,R1,BLCKSAVE        , save registers
         DBGMSG EXECMBR,=CL8'READBLCK' write a debug message
         READ  READDECB,SF,SCRIPTS,(R6),MF=E     Read one block
         CHECK READDECB               , and wait for IO completion
         LH    R0,JFCBLKSI            , Load blocksize
         L     R1,READDECB+16         , R1 ---> IOB
         SH    R0,14(R1)              , subtract residual length
         STH   R0,BLOCKLEN            , length of actual block read
         XR    RECOFFST,RECOFFST      , Record offset = 0
         LM    R14,R1,BLCKSAVE        , restore the registers
         BR    R14                    , return to caller
         TITLE 'Subroutines - PROCLINE: Process a Script line'
***********************************************************************
* Process a script line.  The variable SCRPTLIN contains the script   *
* commands, which will be acted upon                                  *
***********************************************************************
         SPACE 1
PROCLINE DS    0H
         STM   R14,R1,PROCSAVE        , save the registers
         DBGMSG EXECMBR,=CL8'PROCLINE'
         IF    (CLC,=C'*',EQ,SCRPTLIN) is this a comment
          BAL  R14,DOREM              , handle comments
         ELSEIF (CLC,=C'PARM ',EQ,SCRPTLIN)
          BAL  R14,SETPARMS           , process PARM command
         ELSEIF (CLC,=C'IF ',EQ,SCRPTLIN)
          BAL  R14,DOIF               , process IF command
         ELSEIF (CLC,=C'ELSE ',EQ,SCRPTLIN)
          BAL  R14,DOELSE             , process ELSE
         ELSEIF (CLC,=C'ENDIF ',EQ,SCRPTLIN)
          BAL  R14,DOENDIF            , process ENDIF
         ELSEIF (CLC,=C'MSG ',EQ,SCRPTLIN)
          BAL  R14,DOMSG              , handle MSG requests
         ELSEIF (CLC,=C'CMD ',EQ,SCRPTLIN),OR,                         +
               (CLC,=C'COM ',EQ,SCRPTLIN)
          BAL  R14,DOCMD              , process CMD and COM
         ELSEIF (CLC,=C'WAIT ',EQ,SCRPTLIN)
          BAL  R14,DOWAIT             , handle WAIT requests
         ELSE
          BAL  R14,INVCMD             , invalid requests
         ENDIF                        , of of case selection
         LM    R14,R1,PROCSAVE        , restore the registers
         BR    R14                    , retrun to caller
         TITLE 'Subroutines: PUTMSG - Display an error message'
***********************************************************************
* Routine to display an error message. The error message is assumed   *
* to be stored in the MSGTEXT area (and is at most 124 bytes long)    *
* The messages will be written to SYSPRINT DD if available and open.  *
* If not, the messages will be isssued via WTO                        *
*                                                                     *
* Registers on Entry: R14 = Return address                            *
***********************************************************************
         SPACE 1
PUTMSG   DS    0H                     , output message on sysprint
         STM   R14,R3,PUTMSAVE        , save registers
         IF    (TM,PROCFLAG,PRNTOPEN,O) SYSPRINT available?
          IF   (CP,LINENUM,GT,=PL2'55') end of page reached?
           MVC PRNTLINE,HEAD001       , Put in page header
           ZAP LINENUM,=P'12'         , reinit line number
           AP  PAGENUM,=P'1'          , increment page number
           MVC HEAD0011,=X'40202120'  , insert edit mask
           ED  HEAD0011-1(4),PAGENUM  , beautify page number
           PUT SYSPRINT,PRNTLINE      , write page header
           PUT SYSPRINT,HEAD0C1       , Insert LOGO1
           PUT SYSPRINT,HEAD0C2       , Insert LOGO2
           PUT SYSPRINT,HEAD0C3       , Insert LOGO3
           PUT SYSPRINT,HEAD0C4       , Insert LOGO4
           PUT SYSPRINT,HEAD0C5       , Insert LOGO5
           PUT SYSPRINT,HEAD0C6       , Insert LOGO6
          ENDIF
          BLANK PRNTTEXT              , erase any garbage
          MVC   PRNTTEXT(L'MSGTEXT),MSGTEXT
          PUT   SYSPRINT,PRNTLINE
         ELSE                         , no SYSPRINT, use WTO instead
          WTO  MF=(E,THEWTO)
         ENDIF
         XR    R15,R15                , clear RC
         ST    R15,PUTMSAVE+4         , set caller's RC
         LM    R14,R3,PUTMSAVE        , restore return address
         BR    R14                    , and return
         TITLE 'Constant - L-Form macro instructions skeletons'
THELIST  OPEN  (,),MF=L               , Open close RDJFCB list
AWTO     WTO   ' ',MF=L
AWTOL    EQU   *-AWTO
         TITLE 'Literal Pool'
         LTORG
         TITLE 'Reentrant Storage - Message Display variables'
WORKAREA DSECT
***********************************************************************
* The description of a few variables has been placed here because     *
* IFOX00 doesn't easily allow certain forward references in the EQU   *
* statement.                                                          *
***********************************************************************
         READ  DIRDECB,SF,,,'S',MF=L
         READ  READDECB,SF,,,'S',MF=L
READDCBE EQU   *
         PRINT ON,GEN
THEWTO   WTO   '----+----1----+----2----+----3----+----4----+----5----+-
               ----6----+----7----+----8----+----9----+----0----+----1--
               ---+----2----',MF=L
MSGTEXT  EQU   THEWTO+4,124           , area for message texts
THEWTOR  WTOR  '---------1---------2---------3---------4---------5------
               ----6---------7---------8---------9---------0---------1--
               --------2-',,,,MF=L
WTORTEXT EQU   THEWTOR+12,121
PRNTLINE DS    0CL133                 , line to SYSPRINT
PRNTCC   DS    CL1                    , control character
PRNTTEXT DS    CL132                  , text to be printed
BSPRUNSC CSECT
         TITLE 'Constants - Report Header Lines'
**********************************************************************
*  BSPRUNSC Version 1.0
*
*          |l      _,,,---,,_
*    ZZZzz /,:.-':''  . -.  ;-;;,
*         |,4-  ) )-,_. ,( (  :'-
*        '---''(_/--'  :-')_)
*
*  Placed into the Hercules Domain
*  by Volker Bandke, BSP GmbH'
*
*  Volume: MVSRES   Dataset: SYS1.PARMLIB
**********************************************************************
         SPACE 1
HEAD001  DS    0CL(133)
         DC    C'1BSPRUNSC Version &BSPVER..&BSPMOD'
         FILL  HEAD001                ,
         ORG   HEAD001+133-8
         DC    C'PAGE '
HEAD0011 EQU   PRNTLINE+133-5,3
         ORG   ,                      ,
HEAD0C1  DS    0CL(133)
         DC    CL60'0'
         DC    C'         |l      _,,,---,,_'
         FILL  HEAD0C1
HEAD0C2  DS    0CL(133)
         DC    CL60' '
         DC    C'   ZZZzz /,:.-'':''    -.  ;-;;,'
         FILL  HEAD0C2
HEAD0C3  DS    0CL(133)
         DC    CL60' '
         DC    C'        |,4-  ) )-,_. ,( (  :''-'''
         FILL  HEAD0C3
HEAD0C4  DS    0CL(133)
         DC    CL60' '
         DC    C'       ''---''''(_/--''  :-'')_)'
         FILL  HEAD0C4
HEAD0C5  DS    0CL(133)
         DC    CL60'0'
         DC    C' Placed into the Hercules Domain'
         FILL  HEAD0C5
HEAD0C6  DS    0CL(133)
         DC    CL60' '
         DC    C' by Volker Bandke, BSP GmbH'
         FILL  HEAD0C6
         DS    0CL(133)
         TITLE 'Constants - Error and status messages'
**********************************************************************
*   BSPRS01E - xxxxxxxx DD statement not allocated
*   BSPRS02E - Open failed for DD xxxxxxxx
*   BSPRS03W - Script name too long, truncated
*   BSPRS04W - script name not provided, default used
*   BSPRS05W -
*   BSPRS06E - RDJFCB failed for DDNAME xxxxxxxx
*   BSPRS07E - Find failed for member mmmmmmm
*   BSPRS08I - control statement image
*   BSPRS09W -
*   BSPRS10E -
*   BSPRS11I - PARM=NOREPLYU specified.  PFK tables will be updated
*   BSPRS12E - Operator rejectected update request.  PARM=NOUPDATE set
*   BSPRS13E - Error found processing above statement
*   BSPRS14E - Environment not BATCH or STC
*   BSPRS16D - Reply 'U' to run script, 'C' to cancel
*   BSPRS17W -
*   BSPRS18E -
*   BSPRS19E -
*   BSPRS91I - Parm passed: xxxx'
*   BSPRS92I -
*   BSPRS99I - Function terminated, highest RC=xxxx
***********************************************************************
         TITLE 'Constants - Message Texts'
*        DC    C'-----+----1----+----2----+----3----+----4----+----5---
MSG01E   DC    C'BSPRS01E - Allocation failed for XXXXXXXX, RC=XXXX, S9+
               9ERROR=XXXX'
MSG01E1  EQU   MSGTEXT+33,8
MSG01E2  EQU   MSGTEXT+46,4
MSG01E3  EQU   MSGTEXT+61,4
MSG02E   DC    C'BSPRS02E - Open failed for DD statement XXXXXXXX'
MSG02E1  EQU   MSGTEXT+40,8
MSG03W   DC    C'BSPRS03W - Script name too long, truncated'
MSG04W   DC    C'BSPRS03W - No script name provided, default taken'
MSG06E   DC    C'BSPRS06E - RDJFCB failed for PARMLIB, function termina-
               ted'
MSG06E1  EQU   MSGTEXT+29,8
MSG07E   DC    C'BSPRS07E - Find failed for member xxxxxxxx. Ignored'
MSG07E1  EQU   MSGTEXT+34,8
MSG08I   DC    C'BSPRS08I -                                            +
                                            '
MSG08I1  EQU   MSGTEXT+11,72
         ORG   MSG08I
MSG09I   DS    CL(L'MSG08I-4)
MSG09I1  EQU   MSGTEXT+11,L'MSG08I1-4
         ORG
MSG12E   DC    C'BSPRS12E - Operator reject request, commands not execut
               ed'
MSG14E   DC    C'BSPRS14E - Environment not BATCH or STC'
MSG15I   DC    C'BSPRS15I - Debug: File XXXXXXXX, Function ffffffff'
MSG15I1  EQU   MSGTEXT+23,8
MSG15I2  EQU   MSGTEXT+42,8
MSG16D   DC    C'BSPRS16D - Reply ''U'' to run the requested script, or+
                ''C'' to cancel request'
MSG20I   DC    C'BSPRS20I - DD already allocated, no allocation done'
*        DC    C'-----+----1----+----2----+----3----+----4----+----5---
MSG22I   DC    C'BSPRS22I - Dataset processed: ----+----1----+----2----+
               +----3----+----4----'
MSG22I1  EQU   MSGTEXT+30,44
*        DC    C'-----+----1----+----2----+----3----+----4----+----5---
MSG23I   DC    C'BSPRS23I - on volume vvvvvv'
MSG23I1  EQU   MSGTEXT+21,06
MSG99I   DC    C'BSPRS99I - End of processing, MAXRC=xxxx'
MSG99I1  EQU   MSGTEXT+36,4
         TITLE 'Constants - Various values'
DFLTTIME DC    F'1000'
         TITLE 'Miscellaneous Variables'
WORKAREA DSECT                        , reentrant storage
ALCPSAVE DS    4F                     , ALCPDUMP Save area R14 - R1
ALCSSAVE DS    4F                     , ALCPDUMP Save area R14 - R1
ALCRSAVE DS    4F                     , ALCRIPTS Save area R14 - R1
ALCUSAVE DS    4F                     , ALCUDUMP Save area R14 - R1
BLCKSAVE DS    4F                     , READBLCK Save area R14 - R1
DOCMSAVE DS    4F                     , DOCMD    Save area R14 - R1
DOIFSAVE DS    6F                     , DOIF     Save area R14 - R3
DOMSSAVE DS    4F                     , DOMSG    Save area R14 - R1
DOSVSAVE DS    6F                     , DOSVC99  Save area R14 - R3
DOWASAVE DS    8F                     , DOWAIT   Save area R14 - R5
ECHOSAVE DS    4F                     , ECHOLINE Save area R14 - R1
GETPSAVE DS    4F                     , GETPARAM Save area R14 - R1
GETLSAVE DS    4F                     , GETLINE  Save area R14 - R1
JFCBSAVE DS    4F                     , READJFCB Save area R14 - R1
NEXTSAVE DS    7F                     , NEXTWORD Save area R14 - R4
NUMSAVE  DS    6F                     , NUMTEST  Save area R14 - R3
OPENSAVE DS    6F                     , OPENFILE Save area R14 - R3
OPNCSAVE DS    4F                     , OPNRIPTS Save area R14 - R1
OPNSSAVE DS    4F                     , OPNSNAP  Save area R14 - R1
OPNPSAVE DS    4F                     , OPNPRINT Save area R14 - R1
PUTMSAVE DS    6F                     , PUTMSG   Save area R14 - R3
PROCSAVE DS    4F                     , PROCLINE Save area R14 - R1
READSAVE DS    4F                     , READLINE Save area R14 - R1
SETISAVE DS    4F                     , SETINIT  Save area R14 - R1
SETPSAVE DS    6F                     , SETPARMS Save area R14 - R3
TESTSAVE DS    4F                     , TESTMBR  Save area R14 - R1
DBL      DS    D                      , Double word for CVB
TEMPDDN  DS    CL8                    , for DDNAME
MAXCC    DS    F                      , returncode given to caller
MEMCC    DS    F                      , current RC
LASTCC   DS    F                      , current RC
WTOECB   DS    F                      , ECB we are waiting for
JFCBXLST DS    F                      , Exitlist in DCB
WAITTIME DS    F                      , time to wait until redispatch
DEVTYPE  DS    6F                     , for devtype macro
SYSPRINT DS    CL(SYSPRLEN)           , reentrant DCB for SYSPRINT
SNAPDUMP DS    CL(SNAPDLEN)           , reentrant DCB for SNAPDUMP
SCRIPTS  DS    CL(SCRIPLEN)           , reentrant DCB for SCRIPTS
TEMPLINE DS    CL80                   , temporary buffer
SCRPTLIN DS    CL72                   , script line, no leading spaces
PARMLINE ORG   SCRPTLIN
         DS    C'PARM '
PARMTEXT DS    CL67
         ORG
RBPTR    DS    F                      , request block pointer
REQBLK   DS    CL(S99RBEND-S99RB)     , Request block
REQBLKLN EQU   L'REQBLK               , length of request block
SVC99WA  DS    CL100                  , parameter area for SVC99
SVC99P1  EQU   SVC99WA+0,4            , SVC 99 parameter 1
SVC99P2  EQU   SVC99WA+4,4            , SVC 99 parameter 2
SVC99P3  EQU   SVC99WA+8,4            , SVC 99 parameter 3
         SPACE
OCLIST   OPEN  (,),MF=L
WTOREPLY DC    C' '                   , reply we are waiting for
PROCFLAG DS    XL1                    , Processing control flag
ISTSO    EQU   B'10000000'            , running as a TSO user
ISJOB    EQU   B'01000000'            , running as a batch job
ISSTC    EQU   B'00100000'            , running as a started task
PRNTOPEN EQU   B'00010000'            , SYSPRINT is open
SCRIOPEN EQU   B'00001000'            , PARMDIR is open
SNAPOPEN EQU   B'00000100'            , We have an IO buffer
HAVEBUFF EQU   B'00000010'            , We have an IO buffer
SKIPLINE EQU   B'00000001'            , X'01' - skip the next lines
WORKFLAG DC    XL1'0'
PARMFLAG DC    XL1'0'                 , Flag byte for PARM date
PARMEXEC EQU   B'10000000'            , X'80' - PARM=EXEC specified
PARMNORU EQU   B'01000000'            , X'40' - PARM=NOREPLYU SPECIFIED
PARMNOEC EQU   B'00100000'            , X'20' - PARM=NOECHO
PARMMEMB EQU   B'00010000'            , X'10' - member in PARM
EXECMBR  DS    CL8'BSPRUNSC'          , default meber name
EXECMBL  DS    H'0'                   , length of member name
BLOCKLEN DS    H                      , length of block read
BLOCK$   DS    CL8
TRTTABLE DS    CL256                  , area for general purpose TRT
LINENUM  DS    PL2
PAGENUM  DS    PL3
         DS    0D
JFCBAREA DS    0CL176
         PRINT ON,NOGEN
         IEFJFCBN
         TITLE 'Constants - Literal pool'                               00454
         LTORG                                                          00455
         IHAPSA
         CVT   DSECT=YES
ASCB     DSECT
         IHAASCB
ASVT     DSECT
         IHAASVT
         BSPEND                       , of module
@@
//SYSLIB   DD  DISP=SHR,DSN=SYS2.MACLIB,DCB=BLKSIZE=32720
//         DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT2   DD  UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT3   DD  UNIT=VIO,SPACE=(CYL,(1,1))
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSPUNCH DD  DISP=(,PASS),UNIT=VIO,SPACE=(CYL,(1,1))
//LINK    EXEC PGM=IEWL,
//             COND=(0,NE),
//             PARM='LIST,LET,MAP,RENT,REUS,REFR'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(TRK,(50,20))
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.LINKLIB
//SYSLIN   DD  DISP=(OLD,DELETE),DSN=*.ASM.SYSPUNCH
//          DD *
 NAME BSPRUNSC(R)
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=BSPRUNSC
//BSPRUNSC  JOB  (SETUP),
//             'Run BSPRUNSC',
//             CLASS=A,
//             MSGCLASS=H,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//********************************************************************
//*
//* Name: BSPRUNSC
//*
//* Desc: Run the BSPRUNSC program
//*
//********************************************************************
//RUNSC   EXEC PGM=BSPRUNSC,PARM=RSCRIPT    <=== name of script
//*SHUTDOWN   EXEC PGM=BSPRUNSC,PARM=SHUTDOWN
//*SHUTFAST   EXEC PGM=BSPRUNSC,PARM=SHUTFAST
//SCRIPTS DD DISP=SHR,DSN=SYS2.SAMPLIB
//SYSPRINT DD SYSOUT=*
./ ADD NAME=RSCRIPT
PARM NOREPLYU  
PARM ECHO    
MSG TESTING    
CMD $BP   
./ ADD NAME=SHUTDOWN     
***********************************************************************
*                                                                     *
* Name: SYS2.SAMPLIB(SHUTDOWN)                                        *
*                                                                     *
* Desc: Sample Shutdown member for use with BSPRUNSC                  *
*                                                                     *
***********************************************************************
PARM EXEC
PARM ECHO
COM SEND 'Please logoff, the system will terminate in 5 minutes'
WAIT 180
COM SE 'You better finish now, the system will shutdown in 2 minutes'
WAIT 60
COM SE 'We mean it.  The system will terminate in 1 minute!!'
WAIT 60
COM P TSO
WAIT 30
COM HALT net,quick
COM STOP MF1
COM STOP CMD1
COM $P PRT1
COM $P PRT2
COM $P PRT3
COM $P PUNCH1
COM $P RDR1
COM $P
./ ADD NAME=SHUTFAST
***********************************************************************
*                                                                     *
* Name: SYS2.SAMPLIB(SHUTFAST)                                        *
*                                                                     *
* Desc: Sample Shutdown member for use with BSPRUNSC                  *
*                                                                     *
***********************************************************************
PARM NOREPLYU
PARM NOECHO
MSG Sending first warning message
COM SE ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
COM SE '; Please logoff, the system will terminate in 2 minutes ;'
COM SE '"""""""""""""""""""""""""""""""""""""""""""""""""""""""""'
WAIT 60
MSG Sending second warning message
COM SE '***************************************************************'
COM SE '* You better finish now, the system will shutdown in 1 minute *'
COM SE '***************************************************************'
WAIT 60
MSG Shutdown beginning
COM P TSO
WAIT 10
COM z net,quick
COM P MF1
COM P CMD1
COM I SMF
COM $P PRT1
COM $P PRT2
COM $P PRT3
COM $P PUNCH1
COM $P RDR1
WAIT 5
COM $P
./ ADD NAME=SCRIPT00
***********************************************************************
*                                                                     *
* Name: SYS2.SAMPLIB(SCRIPT00)                                        *
*                                                                     *
* Desc: Sample Script member for use with BSPRUNSC                    *
*                                                                     *
***********************************************************************
PARM ECHO
PARM REPLYU
IF MF1
   CONTINUE
ELSE
   COM S MF1
ENDIF
PARM NOECHO
WAIT 5
IF MF1
   CONTINUE
ELSE
   MSG MF/1 could not be started, check system log for errors
ENDIF
./ ADD NAME=SCRIPT01
***********************************************************************
*                                                                     *
* Name: SYS2.SAMPLIB(SCRIPT01)                                        *
*                                                                     *
* Desc: Sample Script member for use with BSPRUNSC                    *
*                                                                     *
***********************************************************************
PARM ECHO
PARM REPLYU
IF MF1
   COM P MF1
ENDIF
WAIT 5
CMD I SMF
WAIT 60
CMD I SMF
./ ADD NAME=SCRIPT02
***********************************************************************
*                                                                     *
* Name: SYS2.SAMPLIB(SCRIPT02)                                        *
*                                                                     *
* Desc: Sample Script member for use with BSPRUNSC                    *
*                                                                     *
***********************************************************************
PARM ECHO
PARM REPLYU
CMD S CLEARDMP,DD=00
WAIT 5
CMD S CLEARDMP,DD=01
WAIT 5
CMD S CLEARDMP,DD=02
WAIT 5
@@