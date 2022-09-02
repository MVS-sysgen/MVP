//BSPAPFLS JOB (JOB),
//             'INSTALL BSPAPFLS',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//ASM     EXEC PGM=IFOX00,PARM='DECK,NOOBJECT,TERM,NOXREF'
//SYSIN    DD  DATA,DLM=@@
BSPAPFLS TITLE 'List APF priviledged data sets'
***********************************************************************
*                                                                     *
*  This program will display the currently acive APF priviledged      *
*  datasets.  It will run in a batch job, as an STC, and as a TSO     *
*  commands                                                           *
*                                                                     *
*                                                                     *
*  Required DD statement: none                                        *
*                                                                     *
*  Optional DD statements:                                            *
*                                                                     *
*           SYSPRINT -  (When running as a batch job)                 *
*                       Default: SYSOUT=A                             *
*                                                                     *
*           SYSUDUMP -  Default: SYSOUT=A                             *
*                                                                     *
*           SNAPDUMP -  (When compiled with &DEBUG=YES)               *
*                       Default: SYSOUT=A                             *
*                                                                     *
* Layout of authorization table                                       *
*                                                                     *
*                  CVT                                                *
* X'10' ----> +----------+    +--->+-------------------+              *
*             |          |    |    | number of entries |              *
*             |----------|    |  2 |-------------------|              *
*             | CVTAUTHL |----+    | Entry # 1         |              *
*             |----------|         |-------------------|              *
*             |          |         | Entry # 2         |              *
*             +----------+         |-------------------|              *
*                                  |   ...             |              *
*                                  |-------------------|              *
*                                  | Entry # n         |              *
*                                  +-------------------+              *
* Layout of authorization table entry                                 *
*                                                                     *
*           0 +--------------------+                                  *
*             | Length VOLSER+DSN  |                                  *
*           1 |--------------------|                                  *
*             | Volume Serial num) |                                  *
*           7 |--------------------|                                  *
*             | Dataset name       |                                  *
*             +--------------------+                                  *
*                                                                     *
***********************************************************************
         PRINT OFF,NOGEN
         COPY  BSPGLBLS
         GBLC  &DEBUG
         COPY  BSPSGLBL
&DEBUG   SETC  'NO'
         DCBD  DSORG=(PS),DEVD=DA     , DCB layout
         IEFZB4D0                     , dynalloc dsects
         IEFZB4D2                     , and equates
         PRINT ON,GEN                 , Macro expansion not needed
BSPAPFLS BSPENTER BASE=(R11,R12),RENT=YES
         BAL   R14,SETINIT            , initialize some variables
         BAL   R14,ALCPRINT           , allocate SYSPRINT if needed
         BAL   R14,OPNPRINT           , open SYSPRINT if needed
         BAL   R14,ALCUDUMP           , allocate SYSUDUMP if needed
         AIF   ('&DEBUG' EQ 'NO').NOSNAP1
         BAL   R14,ALCPDUMP           , go allocate SNAPDUMP
         BAL   R14,OPNPDUMP           , go open SNAPDUMP
.NOSNAP1 ANOP
         TITLE 'Main processing loop'
         L     R4,CVTPTR              , R4 points to CVT
         USING CVT,R4                 , tell assembler
         L     R4,CVTAUTHL            , get address of auth table
         DROP  R4                     , don't need CVT any more
         SR    R5,R5
         LH    R5,0(R4)               , number of entries in table
         LA    R4,2(R4)               , point to first entry
         USING AUTHLDS,R4             , Tell assembler
         MSGPUT MSG03I                , place message into buffer
         BAL   R14,PUTMSG             , and display it
         DO FROM=(R5)                 , Loop through all entries
          MSGPUT MSG04I               , place msg into buffer
          MVC  MSG04I2,AUTHLVOL       , insert VOLSER
          BLANK MSG04I1               , init to spaces
          XR   R6,R6                  , clear length register
          IC   R6,AUTHLLEN            , get length of current entry
          SH   R6,=H'7'               , minus length of VOLSER (6)
*                                             1 for EX         (1)
          MVC  MSG04I1(*-*),AUTHLDSN  , insert DSN
          EX   R6,*-6                 , via EX instruction
          BAL  R14,PUTMSG             , write last message
          LA   R4,8(R6,R4)            , next entry (current+length+1)
         ENDDO                        , end of loop through member
EXIT     DS    0H                     , end of data
         MSGPUT MSG99I                , insert message
         L     R15,MAXCC              , get return code
         CVD   R15,DBL                , make a number
         UNPK  MSG99I1,DBL+5(3)       , make printable
         OI    MSG99I1+L'MSG99I1-1,C'0'      last digit printable
         BAL   R14,PUTMSG             , write last message
         AIF   ('&DEBUG' EQ 'NO').NOPCLOS
         IF    (TM,PROCFLAG,SNAPOPEN,O) close SNAPDUMP if open
          DBGMSG =CL8'SNAPDUMP',=CL8'CLOSE'
          LA   R3,SNAPDUMP            , point to DCB
          MVC   OCLIST,THELIST
          CLOSE ((R3)),MF=(E,OCLIST)  , and close it
          NI   PROCFLAG,255-SNAPOPEN
         ENDIF (TM,PROCFLAG,SNAPOPEN,O) close SNAPDUMP if open
.NOPCLOS ANOP
         IF    (TM,PROCFLAG,PRNTOPEN,O) close SYSPRINT if open
          DBGMSG =CL8'SYSPRINT',=CL8'CLOSE'
          LA   R3,SYSPRINT            , point to DCB
          MVC   OCLIST,THELIST
          CLOSE ((R3)),MF=(E,OCLIST)  , and close it
          NI   PROCFLAG,255-PRNTOPEN
         ENDIF (TM,PROCFLAG,PRNTOPEN,O) close SYSPRINT if open
         L     R15,MAXCC              , get return code
         BSPRET RC=(15)               , and return with rc in r15
         TITLE 'Initialize some variables'
***********************************************************************
* Initialze some variables, set processing options according to       *
* runtime environmaent (STC or BATCH)                                 *
***********************************************************************
         SPACE 1
SETINIT  DS    0H                     , Initialize some variables
         STM   R14,R1,SETISAVE        , save registers
         DBGMSG =CL8'        ',=CL8'SETINIT'
         XC    MAXCC,MAXCC            , return code field
         XC    LASTCC,LASTCC          , return code field
         MVI   PROCFLAG,X'0'          , clear parm flag
         ZAP   LINENUM,=P'100'        , force page break
         ZAP   PAGENUM,=P'0'          , init page number
         XC    PROCFLAG,PROCFLAG      , clear parm flag
         TESTENV                      , STC? BATCH? TSO?
         IF    (CH,R1,EQ,=H'0')       , is this a batch job
          OI   PROCFLAG,ISJOB         , remember in process option flag
         ELSEIF (CH,R1,EQ,=H'4')      , is this an STC
          OI   PROCFLAG,ISSTC         , put indicator to flag
         ELSE                         , must be a batch job
          OI   PROCFLAG,ISJOB         , remeber for later
         ENDIF
         LM    R14,R1,SETISAVE        , restore all register
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
         AIF   ('&DEBUG' EQ 'NO').NOOPNP
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
SNAPDDCB DCB   DDNAME=SNAPDUMP,       , ddname for this file           -
               DSORG=PS,              , file is sequential             -
               LRECL=125,             , record length                  -
               BLKSIZE=1632,          , and blocksize                  -
               MACRF=W,               , will be opened for output      -
               RECFM=VBA              , fixed block, ansi cntlchars
SNAPDLEN EQU   *-SNAPDDCB             , length of DCB
.NOOPNP  ANOP
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
         TITLE 'Subroutines: PUTMSG - Display an error message'
***********************************************************************
* Routine to display an error message. The error message is assumed   *
* to be stored in the MSGTEXT area (and is at most 124 bytes long)    *
* If SYSPRINT is available and open, send msgs there                  *
* Otherwise, use WTOes will be isssued via WTO                        *
*                                                                     *
* Registers on Entry: R14 = Return address                            *
***********************************************************************
         SPACE 1
PUTMSG   DS    0H                     , output message on sysprint
         STM   R14,R3,PUTMSAVE        , save registers
         IF    (TM,PROCFLAG,PRNTOPEN,O) SYSPRINT available?
*         WTO  MF=(E,THEWTO)
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
         ELSEIF (TM,PROCFLAG,ISTSO,O) , no SYSPRINT, use TPUT in TSO
*         WTO  MF=(E,THEWTO)
          LA   R1,MSGTEXT
          LA   R0,79
          TPUT (1),(0)
         ELSE                         , No TSO, use WTO
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
BSPAPFLS CSECT
         TITLE 'Constants - Report Header Lines'
**********************************************************************
*  BSPAPFLS Version 1.0 - List APF datasets
*
*          |l      _,,,---,,_
*    ZZZzz /,:.-':''  . -.  ;-;;,
*         |,4-  ) )-,_. ,( (  :'-
*        '---''(_/--'  :-')_)
*
*  Placed into the Hercules Domain
*  by Volker Bandke, BSP GmbH'
*
**********************************************************************
         SPACE 1
HEAD001  DS    0CL(133)
         DC    C'1BSPAPFLS Version &BSPVER..&BSPMOD - List APF Datsets'
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
*   BSPAL01E - xxxxxxxx DD statement not allocated
*   BSPAL02E - Open failed for DD xxxxxxxx
*   BSPAL99I - Function terminated, highest RC=xxxx
***********************************************************************
         TITLE 'Constants - Message Texts'
*        DC    C'-----+----1----+----2----+----3----+----4----+----5---
MSG01E   DC    C'BSPAL01E - Allocation failed for XXXXXXXX, RC=XXXX, S9+
               9ERROR=XXXX'
MSG01E1  EQU   MSGTEXT+33,8
MSG01E2  EQU   MSGTEXT+46,4
MSG01E3  EQU   MSGTEXT+61,4
MSG02E   DC    C'BSPAL02E - Open failed for DD statement XXXXXXXX'
MSG02E1  EQU   MSGTEXT+40,8
MSG03I   DC    C'BSPAL03I - Volume APF Dataset name'
MSG04I   DC    C'           vvvvvv ----+----1----+----2----+----3----+-x
               ---4----'
MSG04I1  EQU   MSGTEXT+18,44
MSG04I2  EQU   MSGTEXT+11,06
MSG15I   DC    C'BSPAL15I - Debug: File XXXXXXXX, Function ffffffff'
MSG15I1  EQU   MSGTEXT+23,8
MSG15I2  EQU   MSGTEXT+42,8
MSG20I   DC    C'BSPAL20I - DD already allocated, no allocation done'
MSG99I   DC    C'BSPAL99I - End of processing, MAXRC=xxxx'
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
MEMCC    DS    F                      , returncode given to caller
LASTCC   DS    F                      , current RC
WAITTIME DS    F                      , time to wait until redispatch
DEVTYPE  DS    6F                     , for devtype macro
SYSPRINT DS    CL(SYSPRLEN)           , reentrant DCB for SYSPRINT
         AIF   ('&DEBUG' EQ 'NO').NOPDCB
SNAPDUMP DS    CL(SNAPDLEN)           , reentrant DCB for SNAPDUMP
.NOPDCB  ANOP
TEMPLINE DS    CL80                   , temporary buffer
RBPTR    DS    F                      , request block pointer
REQBLK   DS    CL(S99RBEND-S99RB)     , Request block
REQBLKLN EQU   L'REQBLK               , length of request block
SVC99WA  DS    CL100                  , parameter area for SVC99
SVC99P1  EQU   SVC99WA+0,4            , SVC 99 parameter 1
SVC99P2  EQU   SVC99WA+4,4            , SVC 99 parameter 2
SVC99P3  EQU   SVC99WA+8,4            , SVC 99 parameter 3
         SPACE
OCLIST   OPEN  (,),MF=L
PROCFLAG DS    XL1                    , Processing control flag
ISTSO    EQU   B'10000000'            , running as a TSO user
ISJOB    EQU   B'01000000'            , running as a batch job
ISSTC    EQU   B'00100000'            , running as a started task
PRNTOPEN EQU   B'00010000'            , SYSPRINT is open
SNAPOPEN EQU   B'00001000'            , We have an IO buffer
SKIPLINE EQU   B'00000001'            , X'01' - skip the next lines
WORKFLAG DC    XL1'0'
TRTTABLE DS    CL256                  , area for general purpose TRT
LINENUM  DS    PL2
PAGENUM  DS    PL3
         TITLE 'Constants - Literal pool'
         LTORG
AUTHLDS  DSECT                        , authorization table layout
AUTHLLEN DS    CL1                    , length of this entry
AUTHLVOL DS    CL6                    , volume serial
AUTHLDSN DS    CL44                   , dataset name
         IHAPSA
         CVT   DSECT=YES,LIST=YES
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
//             PARM='LIST,LET,MAP'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(TRK,(50,20))
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.CMDLIB
//SYSLIN   DD  DISP=(OLD,DELETE),DSN=*.ASM.SYSPUNCH
//          DD *
 NAME BSPAPFLS(R)
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=BSPAPFLS
//BSPAPFLS  JOB  (SETUP),
//             'Run BSPAPFLS',
//             CLASS=A,
//             MSGCLASS=H,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//********************************************************************
//*
//* Name: BSPAPFLS
//*
//* Desc: Run the BSPAPFLS program
//*
//********************************************************************
//LISTAPF EXEC PGM=BSPAPFLS
//
@@