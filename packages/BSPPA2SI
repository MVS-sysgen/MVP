//BSPPA2SI JOB (JOB),
//             'INSTALL BSPPA2SI',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//ASM     EXEC PGM=IFOX00,PARM='DECK,NOOBJECT,TERM,NOXREF'
//SYSIN    DD  DATA,DLM=@@
BSPPA2SI BSPENTER BASE=(R11),RENT=YES
         COPY  BSPGLBLS
         COPY  BSPSGLBL
         TITLE 'Main Processing'
         BAL   R14,SETINIT            , go for setup and init calls
         BAL   R14,ALCUDUMP           , go allocate SYSUDUMP
         BAL   R14,ALCPRINT           , go allocate SYSPRINT
         IF    (LTR,R15,R15,Z)        , if Allocate worked
          BAL  R14,OPNPRINT           , go open SYSPRINT
         ENDIF                        , if not, use WTO instead of put
         BAL   R14,PROCPARM           , go analyse PARM statement
         IF    (LTR,R15,15,NZ)        , Was there a parm statement
          MSGPUT MSG03E               , load message text
          BAL  R14,PUTMSG             , send message to user
          SETMAXCC 12                 , set return code
          B    EXIT
         ENDIF
         BAL   R14,ALSYSUT1           , allocate SYSUT1
         IF    (LTR,R15,R15,NZ)       , if SYSUT1 is not allocated
          MSGPUT MSG04E               , load message text
          BAL  R14,PUTMSG             , send message
          SETMAXCC 12                 , set RC
          B    EXIT                   , and leave
         ENDIF (LTR,R15,R15,NZ)
         BAL   R14,OPSYSUT1           , open SYSUT1
         IF    (LTR,R15,R15,NZ)       , open was okay on SYSUT1
          SETMAXCC 12                 , set a return code of 12
          B    EXIT                   , and leave
         ENDIF
         BAL   R14,WRITPARM           , then write PARM to SYSUT1
EXIT     DS    0H                     , Get out
         MSGPUT MSG99I                , insert message
         L     R15,MAXCC              , get return code
         CVD   R15,DBL                , make a number
         UNPK  MSG99I1,DBL+5(3)       , make printable
         OI    MSG99I1+L'MSG99I1-1,C'0' last digit printable
         BAL   R14,PUTMSG             , write last message
         IF    (TM,PROCFLAG,SUT1OPEN,O) close SYSUT1 if open
          LA   R3,SYSUT1              , point to DCB
          MVC   OCLIST,THELIST        , initialize OC list
          CLOSE ((R3)),MF=(E,OCLIST)  , and close it
          NI   PROCFLAG,255-SUT1OPEN  , indicate file is closed
         ENDIF
         IF    (TM,PROCFLAG,PRNTOPEN,O) close PARMDIR if open
          LA   R3,SYSPRINT            , point to DCB
          MVC   OCLIST,THELIST        , initialize OC list
          CLOSE ((R3)),MF=(E,OCLIST)  , and close it
          NI   PROCFLAG,255-PRNTOPEN  , indicate file is closed
         ENDIF
         L     R15,MAXCC              , get return code
         BSPRET RC=(15)               , and return with rc in r15
         TITLE 'Subroutines: SETINIT - Initialize some variables'
***********************************************************************
* Setup and Initialization                                            *
*                                                                     *
* Registers on Entry:  R14 = Return address                           *
*                                                                     *
* Registers on Exit:   unchanged                                      *
***********************************************************************
SETINIT  DS    0H
         STM   R14,R1,SETISAVE        , save registers
         BLANK PARMAREA               , clear parm text area
         ZAP   LINENUM,=P'100'        , force page break
         ZAP   PAGENUM,=P'0'          , Init page number
         MVC   THEWTO(AWTOL),AWTO     , init reentrant WTO area
         LA    R1,128                 , maximum length
         STH   R1,THEWTO+4            , put into reentrant WTO
         XC    MAXCC,MAXCC            , clear RC area
         XC    LASTCC,LASTCC          , clear RC area
         XC    MEMCC,MEMCC            , clear RC area
         XC    PARMFLAG,PARMFLAG      , clear parm flag
         XC    PROCFLAG,PROCFLAG      , clear parm flag
         TESTENV                      , STC? BATCH? TSO?
         IF    (CH,R1,EQ,=H'0')       , is this a batch job
          OI   PROCFLAG,ISJOB         , remember in process option flag
         ELSEIF (CH,R1,EQ,=H'4')      , is this an STC
          OI   PROCFLAG,ISSTC         , put indicator to flag
         ELSE                         , it is not supported
          BLANK MSGTEXT
          MSGPUT MSG14E               , insert message text
          BAL  R14,PUTMSG             , issue message
          SETMAXCC 12                 , indicate error RC
          B    EXIT                   , get out with RC=12
         ENDIF
         LM    R14,R1,SETISAVE        , restore registers
         BR    R14                    , and return to caller
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
         TITLE 'Allocate SYSUDUMP for JOB if needed'
***********************************************************************
* Allocate SYSUDUMP DD if not already present                         *
***********************************************************************
         SPACE
ALCUDUMP DS    0H                     , allocate SYSUDUMP
         STM   R14,R1,ALCUSAVE        , save registers
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
          SETMAXCC 4                  , and end with RC=12
         ENDIF (LTR,R15,R15,NZ)       , Error on DYNALLOC
         LM    R14,R1,ALCUSAVE        , restore registers
         BR    R14                    , return to caller
***********************************************************************
* SYSUDUMP DYNALLOC parameters                                        *
***********************************************************************
         SPACE
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
         ENDIF
         LM    R14,R3,DOSVSAVE        , restore return address
         BR    R14                    , and retrun to caller
         DROP  R2,R3                  , not needed any more
         TITLE 'Allocate SYSPRINT for JOB if needed'
***********************************************************************
* Allocate SYSPRINT DD if not already present                         *
***********************************************************************
         SPACE
ALCPRINT DS    0H                     , allocate SYSPRINT DD
         STM   R14,R1,ALCPSAVE        , save the registers
         MVC   TEMPDDN,TUSPDDNM       , insert DDNAME into msg text
         MVC   SVC99WA(TUSPLEN),TUSPPTR move text units to WS
         LA    R1,SVC99WA+TUSPDDN-TUSPPTR  point to DDNAME
         ST    R1,SVC99P1             , put into TU list
         LA    R1,SVC99WA+TUSPCLS-TUSPPTR  , point to CLASS parm
         ICM   R1,B'1000',=XL1'80'    , indicate last parm
         ST    R1,SVC99P2             , put into TU list
         LA    R1,SVC99WA             , point to work area
         BAL   R14,DOSVC99            , go and do it
         ST    R15,ALCPSAVE+4         , set caller's retrun code
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
         DC    C'A'                   , sysout class
TUSPLEN  EQU   *-TUSPPTR
         TITLE 'Allocate SYSUT1 for JOB if needed'
***********************************************************************
* Allocate SYSUT1 if not already present                              *
***********************************************************************
         SPACE
ALSYSUT1 DS    0H                     , allocate SYSPRINT DD
         STM   R14,R1,ALSYSAVE        , save the registers
         DEVTYPE =CL8'SYSUT1',DEVTYPE , just test if it is allocated
         ST    R15,ALSYSAVE+4         , set classer's retrun code
         LM    R14,R1,ALSYSAVE        , restore the registers
         BR    R14                    , and return to caller
         TITLE 'Subroutines: OPNPRINT - Open SYSPRINT'
***********************************************************************
* Open SYSPRINT DD for output processing (for a job)                  *
***********************************************************************
         SPACE
OPNPRINT DS    0H                     , open SYSPRINT
         STM   R14,R1,OPNPSAVE        , save registers
         MVC   SYSPRINT,SYSPRDCB      , move DCB to reentrant storage
         LA    R1,SYSPRINT            , point to SYSPRINT DCB
         BAL   R14,OPENFILE           , go open the file
         IF    (LTR,R15,R15,Z)        , if successful
          OI   PROCFLAG,PRNTOPEN      , indicate that SYSPRINT is open
         ENDIF (LTR,R15,R15,Z)       , if successful
         LM    R14,R1,OPNPSAVE        , restore registers
         BR    R14                    , return to caller
         PUSH  PRINT
         PRINT NOGEN
SYSPRDCB DCB   DDNAME=SYSPRINT,       , ddname for this file           +
               DSORG=PS,              , file is sequential             +
               LRECL=133,             , record length                  +
               BLKSIZE=1330,          , and blocksize                  +
               MACRF=(PM),            , will be opened for output      +
               RECFM=FBA              , fixed block, ansi cntlchars
SYSPRLEN EQU   *-SYSPRDCB             , length of DCB
         TITLE 'Subroutines: OPSYSUT1 - Open SYSUT1'
***********************************************************************
* Open SYSUT1 DD                                                      *
***********************************************************************
         SPACE
OPSYSUT1 DS    0H                     , open SYSPRINT
         STM   R14,R1,OPSYSAVE        , save registers
         MVC   SYSUT1,SYSUTDCB        , move DCB to reentrant storage
         LA    R1,SYSUT1              , point to SYSPRINT DCB
         BAL   R14,OPENFILE           , go open the file
         IF    (LTR,R15,R15,Z)        , if successful
          OI   PROCFLAG,SUT1OPEN      , indicate that SYSPRINT is open
         ENDIF (LTR,R15,R15,Z)        , if successful
         LM    R14,R1,OPSYSAVE        , restore registers
         BR    R14                    , return to caller
         PUSH  PRINT
         PRINT NOGEN
SYSUTDCB DCB   DDNAME=SYSUT1,         , ddname for this file           +
               DSORG=PS,              , file is sequential             +
               LRECL=80,              , record length                  +
               RECFM=FB,              , record length                  +
               BLKSIZE=80,            , record length                  +
               MACRF=(PM)             , will be opened for output
SYSUTLEN EQU   *-SYSUTDCB             , length of DCB
         TITLE 'Subroutine OPENFILE - Open files as needed'
***********************************************************************
* Open a file, report any errors ifopen fails                         *
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
         MSGPUT MSG02E                , insert message body
         MVC   MSG02E1,DCBDDNAM       , insert DD name into msg
         MVC   OCLIST,THELIST         , Set up open/close list
         OPEN ((R3),OUTPUT),MF=(E,OCLIST)
         IF    (TM,DCBOFLGS,DCBOFOPN,O) if open was successfull
          XR   R15,R15                , clear return code
         ELSE                         , when open failed
          BAL R14,PUTMSG              , issue the message
          LA  R15,8                   , load error RC
         ENDIF (TM,DCBOFLGS,DCBOFOPN,O) if open was okay
         ST    R15,OPENSAVE+4         , set caller's R15
         LM    14,3,OPENSAVE          , restore registers
         BR    R14                    , and return to caller
         DROP  R3                 , not needed outside this module
         TITLE 'Get PARM from JCL'
***********************************************************************
* Get the EXEC PARM value from the system                             *
***********************************************************************
PROCPARM DS    0H                     , analyse PARM statement
         STM   R14,R1,PROCSAVE        , Save registers
         L     R1,0(0,R1)             , Address of passed parm
         LH    R15,0(0,R1)            , R15 = Length of parameters
         LA    R1,2(0,R1)             , R1  = Address of parameter
         IF    (LTR,R15,R15,Z)        , if no parameter was given
          LA   R15,8                  , Indicate no parameters sent
         ELSE
          IF   (CH,R15,GT,=Y(L'PARMAREA))  PARM too long
           MSGPUT MSG05W              , load message text
           BAL  R14,PUTMSG            , show message
           LA   R15,80                , load maximum length
           SETMAXCC 4                 , indicate a warning
          ENDIF
          BCTR  R15,0                 , minus one for EX
          BLANK PARMAREA
          MVC   PARMAREA(*-*),0(R1)   , move in parameters
          EX    R15,*-6               , via EX
          MSGPUT MSG91I               , load information msg
          MVC   MSG91I1,PARMAREA      , insert PARM text
          BAL   R14,PUTMSG            , send the message
          LA    R15,0                 , indicate success
         ENDIF (LTR,R15,R15,Z)        , if no parameter was given
         ST    R15,PROCSAVE+4         , set caller's R15
         LM    R14,R1,PROCSAVE        , restore registers
         BR    R14                    , return to caller
         TITLE 'Write PARMTEXT to SYSUT1'
***********************************************************************
* Write the PARMTEXT to SYSUT1                                        *
***********************************************************************
WRITPARM DS    0H                     , analyse PARM statement
         STM   R14,R1,WRITSAVE        , Save registers
         PUT   SYSUT1,PARMAREA        , write text to SYSUT1
         LM    R14,R1,WRITSAVE        , restore registers
         BR    R14                    , return to caller
         TITLE 'Constant - L-Form macro instructions skeletons'
         PUSH  PRINT
         PRINT ON,GEN
THELIST  OPEN  (,),MF=L               , Open close RDJFCB list
AWTO     WTO   ' ',MF=L
AWTOL    EQU   *-AWTO
         TITLE 'Literal Pool'
         LTORG
         TITLE 'Reentrant Storage - MF=L type macro storage'
WORKAREA DSECT
***********************************************************************
* The description of a few variables has been placed here because     *
* IFOX00 doesn't easily allow certain forward references in the EQU   *
* statement.  't easily allow certain forward references in the EQU   *
***********************************************************************
THEWTO   WTO   '----+----1----+----2----+----3----+----4----+----5----++
               ----6----+----7----+----8----+----9----+----0----+----1-+
               ---+----2----',MF=L
MSGTEXT  EQU   THEWTO+4,124           , area for message texts
         POP   PRINT
PRNTLINE DS    0CL133                 , line to SYSPRINT
PRNTCC   DS    CL1                    , control character
PRNTTEXT DS    CL132                  , text to be printed
BSPPA2SI CSECT
         TITLE 'Constants - Report Header Lines'
**********************************************************************
*  BSPPA2SI Version 1.0
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
         DC    C'1BSPPA2SI Version &BSPVER..&BSPMOD'
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
*   BSPSI01E - xxxxxxxx DD statement not allocated
*   BSPSI02E - Open failed for DD xxxxxxxx
*   BSPSI03E - PARM statement missing
*   BSPSI04E - Required DD statement SYSUT1 missing
*   BSPSI05W - PARM too long, truncated
*   BSPSI14E - Environment neither JOB nor STC.  Function terminated'
*   BSPSP91I - Parm passed: xxxx'
*   BSPSP99I - End of processing
**********************************************************************
*        DC    C'-----+----1----+----2----+----3----+----4----+----5---
MSG01E   DC    C'BSPSI01E - Allocation failed for XXXXXXXX, RC=XXXX, S9+
               9ERROR=XXXX'
MSG01E1  EQU   MSGTEXT+33,8
MSG01E2  EQU   MSGTEXT+46,4
MSG01E3  EQU   MSGTEXT+61,4
MSG02E   DC    C'BSPSI02E - Open failed for DD statement XXXXXXXX'
MSG02E1  EQU   MSGTEXT+40,8
MSG03E   DC    C'BSPSI03E - EXEC PARM statement missing'
MSG04E   DC    C'BSPSI04E - Required DD statement SYSUT1 missing'
MSG05W   DC    C'BSPSI0WE - Parameter too long, truncated'
MSG14E   DC    C'BSPSI14E - Environment neither JOB nor STC.'
MSG91I   DC    C'BSPSP91I - Parms passed: ----+----1----+----2----+----+
               3----+----4----+----5----+----6----+----7----+----8'
MSG91I1  EQU   MSGTEXT+25,80
MSG99I   DC    C'BSPSP99I - End of processing, MAXRC=xxxx'
MSG99I1  EQU   MSGTEXT+36,4
         TITLE 'Constants - Literal pool'                               00454
         LTORG                                                          00455
         TITLE 'Variables - Reentrant working storage'
WORKAREA DSECT
         SPACE 2                                                        00468
         TITLE 'Variables - Dynalloc request block'
***********************************************************************
* SVC 99 Request Block                                                *
***********************************************************************
         SPACE
         PUSH  PRINT
         PRINT NOGEN
         IEFZB4D0                     , dynalloc dsects
         IEFZB4D2                     , and equates
WORKAREA DSECT
DBL      DS    D                      , general purpose double word
         POP   PRINT
***********************************************************************
* SVC99 Request Block                                                 *
***********************************************************************
         SPACE 1
RBPTR    DS    F                      , request block pointer
REQBLK   DS    CL(S99RBEND-S99RB)     , Request block
REQBLKLN EQU   L'REQBLK               , length of request block
***********************************************************************
* Reentrant area for SVC 99 Text unit blocks                          *
***********************************************************************
         SPACE 1
SVC99WA  DS    CL128                  , parameter area for SVC99
SVC99P1  EQU   SVC99WA+0,4            , SVC 9 parmater 1
SVC99P2  EQU   SVC99WA+4,4            , SVC 9 parmater 2
SVC99P3  EQU   SVC99WA+8,4            , SVC 9 parmater 3
***********************************************************************
* Subroutine save areas                                               *
***********************************************************************
         SPACE 1
ALCPSAVE DS    4F                     , ALCPRINT save area R14 - R1
ALCUSAVE DS    4F                     , ALCUDUMP save area R14 - R1
ALSYSAVE DS    4F                     , ALSYSUT1 save area R14 - R1
DOSVSAVE DS    6F                     , DOSVC99  save area R14 - R3
GETSAVE  DS    9F                     , GETPARAM save area R14 - R6
WRITSAVE DS    4F                     , WRITPARM save area R14 - R1
OPNPSAVE DS    4F                     , OPNPRINT save area R14 - R1
OPSYSAVE DS    4F                     , OPSYSUT1 save area R14 - R1
SETISAVE DS    4F                     , SETINIT  save area R14 - R1
OPENSAVE DS    6F                     , OPENFILE save area R14 - R3
PROCSAVE DS    4F                     , PROCPARM save area R14 - R1
PUTMSAVE DS    6F                     , PUTMSG   save area R14 - R3
         TITLE 'Miscellaneous Variables'
LASTCC   DS    F'0'                   , returncode given to caller
MAXCC    DS    F'0'                   , returncode given to caller
MEMCC    DS    F'0'                   , returncode for this member
DEVTYPE  DC    6F'0'                  , for devtype macro
PAGENUM  DC    PL2'0'                 , Page number in report
LINENUM  DC    PL2'100'               , Line number on page
FLAG     DC    XL1'00'                , status flag
INVPFNUM EQU   B'10000000'            , X'80' - Invalid PFK number
INVCONV  EQU   B'01000000'            , X'40' - Invalid CONV setting
INVCONT  EQU   B'00100000'            , X'20' - Invalid continuation
INVRANG  EQU   B'00010000'            , X'10' - Invalid pfknum range
NEEDCONT EQU   B'00001000'            , X'08' - Continuation requested
NOPFSLOT EQU   B'00000100'            , X'04' - No PFK slot found
NOREPLYU EQU   B'00000010'            , X'02' - OPerator rejected req
PARMFLAG DC    XL1'0'                 , Flag byte for PARM date
PARMSET  EQU   B'10000000'            , X'80' - set keys
PARMNORU EQU   B'01000000'            , X'40' - don't issue WTOR
PARMCHCK EQU   B'00100000'            , X'20' - Check SETPFKxx
PARMLONG EQU   B'00000100'            , X'04' - Invalid parms given
PARMINV  EQU   B'00000010'            , X'02' - Invalid parms given
PARMNONE EQU   B'00000001'            , X'01' - No parms given
PROCFLAG DS    XL1                    , Processing control flag
ISTSO    EQU   B'10000000'            , running as a TSO user
ISJOB    EQU   B'01000000'            , running as a batch job
ISSTC    EQU   B'00100000'            , running as a started task
PRNTOPEN EQU   B'00010000'            , SYSPRINT is open
SUT1OPEN EQU   B'00001000'            , PARMLIB is open
OCLIST   OPEN  (,),MF=L               , OPEN/CLOSE parameter list
SNAPLIST DS    20F
HDRLIST  DS    10F
SAVMEMBR DS    CL8
PARMAREA DS    CL80                   , length of PARM to be passed
TEMPDDN  DS    CL8                    , slot for saving DDNAMES
         DS    0D
SYSPRINT DS    CL(SYSPRLEN)           , area for SYSPRINT DCB
         DS    0D
SYSUT1   DS    CL(SYSUTLEN)           , area for PARMDIR  DCB
         PRINT ON,NOGEN
         DCBD  DSORG=(PO,PS),DEVD=DA  , DCB layout
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
//             PARM='NOLIST,LET,MAP'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(TRK,(50,20))
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.LINKLIB
//SYSLIN   DD  DISP=(OLD,DELETE),DSN=*.ASM.SYSPUNCH
//          DD *
 NAME BSPPA2SI(R)
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=BSPPA2SI
//BSPPA2SI  JOB  (SETUP),
//             'Run BSPPA2SI',
//             CLASS=A,
//             MSGCLASS=H,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//********************************************************************
//*
//* Name: BSPPA2SI
//*
//* Desc: Run the BSPPA2SI program
//*
//********************************************************************
//LISTC   PROC L=
//PA2SI   EXEC PGM=BSPPA2SI,
//             PARM=' LISTCAT LEVEL(&L)'
//SYSUT1   DD  DISP=(,PASS),UNIT=VIO,SPACE=(80,(1,1))
//IDCAMS  EXEC PGM=IDCAMS,COND=(4,LT,PA2SI)
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DISP=(OLD,DELETE),DSN=*.PA2SI.SYSUT1
//        PEND
//IBMUSER  EXEC LISTC,L=IBMUSER
//MVSCE01  EXEC LISTC,L=MVSCE01
@@