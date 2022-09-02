//BSPPILOT JOB (JOB),
//             'INSTALL BSPPILOT',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=8192K,
//             USER=IBMUSER,
//             PASSWORD=SYS1
//* +----------------------------------------------------------------+
//* |                                                                |
//* | Name: CBT.MVS38J.CNTL(CVXIT$)                                  |
//* |                                                                |
//* | Type: JCL to install usermod ZUM0003                           |
//* |                                                                |
//* | Desc: Build the automatic operator made from the WTO           |
//* |       exit IEECVXIT and the R2D2 routine                       |
//* |                                                                |
//* |       The code in IEECVXIT will only be active when and if     |
//* |       the module BSPPILOT is running in a started task or a job|
//* |       with the name BSPPILOT.                                  |
//* |                                                                |
//* |       A Procedure called BSPPILOT will be automatically placed |
//* |       into SYS1.PROCLIB.  This usermod will also create a      |
//* |       member COMMND01 in SYS1.PARMLIB.  You should copy        |
//* |       this member to the beginning of your existing COMMND00   |
//* |       member. If you do not already have a COMMND00 member     |
//* |       then just rename COMMND01 to COMMND00                    |
//* |                                                                |
//* |                                                                |
//* +----------------------------------------------------------------+
/*MESSAGE *****************************
/*MESSAGE *                           *
/*MESSAGE * To activate this usermod  *
/*MESSAGE * an IPL with the CLPA      *
/*MESSAGE * option is required        *
/*MESSAGE *                           *
/*MESSAGE *****************************
//*
//*
//ASM     EXEC PGM=IFOX00,
//             PARM='DECK,NOOBJECT,NOXREF,TERM,SYSPARM(BSPPILOT)'
//*********************************************************************
//* You might need to change the following two DD statements to reflect
//* The correct dataset names for MACLIB and SYSIN
//*********************************************************************
//SYSIN    DD  DATA,DLM=@@
IEECVXIT TITLE 'WTO Exit to allow automatic operations'
         COPY  BSPGLBLS
         GBLC  &PGMSP                 , subpool for program storage
         GBLC  &SRBSP                 , subpool for SRB storage
         GBLC  &ORESCAN               , number or ORE chain scans
         GBLC  &MAXORE                , max number of OREs / Scan
         GBLC  &WAITIME               , wait time in seconds
         COPY  BSPSGLBL               , set globals
         GBLC  &C3PO
         AIF   ('&SYSPARM' EQ '').NOSYSP
&C3PO    SETC  '&SYSPARM'
         AGO   .CONT01
.NOSYSP  ANOP
&C3PO    SETC  'BSPPILOT'
.CONT01  ANOP
&WAITIME SETC  '3'                    , Waittime before reacting
&ORESCAN SETC  '5'                    , max num of ORE chain scans
&MAXORE  SETC  '20'                   , max num of ORE per scan
&PGMSP   SETC  '250'                  , SQA, fixed
&SRBSP   SETC  '245'                  , SQA, fixed
         TITLE 'User-defined DSECTS'
**********************************************************************
* Table of messages with reply texts
**********************************************************************
         SPACE
REPLNTRY DSECT
REPLMSG  DS    CL8                    , ID of msg to suppress
REPLNUM  DS    AL4                    , ID of reply text
REPLNTRL EQU   *-REPLNTRY             , length of entry
         SPACE 2
**********************************************************************
* Table of messages with changes to routing codes
**********************************************************************
ROUTNTRY DSECT
ROUTMSG  DS    CL8                    , ID of msg to suppress
ROUTCA   DS    A                      , Address of descriptor code
ROUTNTRL EQU   *-ROUTNTRY             , length of entry
**********************************************************************
* Table of messages with changes to descriptor codes
**********************************************************************
         SPACE
DESCNTRY DSECT
DESCMSG  DS    CL8                    , ID of msg to suppress
DESCDA   DS    A                      , Address of descriptor code
DESCNTRL EQU   *-DESCNTRY             , length of entry
**********************************************************************
* Table of messages that should be suppressed
**********************************************************************
         SPACE
SUPPNTRY DSECT
SUPPMSG  DS    CL8                    , ID of msg to suppress
SUPPNTRL EQU   *-SUPPNTRY             , length of entry
**********************************************************************
* Table of messages that have specific actions associated
**********************************************************************
         SPACE
ACTNNTRY DSECT
ACTNMSG  DS    CL8                    , message ID to act on
ACTNRTNA DS    A                      , address of action routine
ACTNNTRL EQU   *-ACTNNTRY             , length of entry
IEECVXIT BSPENTER BASE=(R12),RENT=YES,SP=&PGMSP
         PRINT ON,NOGEN
         TITLE 'Assemblers Symbols and Equates'
PARMREG  EQU   R1                     , parameter pointer
CUCMCBAR EQU   R2                     , pointer to IEECUCM
         TITLE 'Initialization and setup'
         PRINT ON,NOGEN
         L     CUCMCBAR,0(PARMREG)    , tell assembler
         USING UCMEXIT,CUCMCBAR       , tell assembler
         L     R8,X'74'(R5)           , get VPARMAD
         MVC   $ECB,4(R8)             , save the ECB address
         LR    R10,R13                , common area
         DROP  R13                    , not needed any more
         USING WORKAREA,R10           , tell assembler
         TITLE 'Search Autopilot'
***********************************************************************
* This routine is supposed to work if and only if the autopilot task  *
* is active.  We scan the ASCB chain, and if the autopilot task is not*
* found then we just finish                                           *
***********************************************************************
         USING PSA,0                  , tell assembler
         L     R3,CVTPTR              , get address of CVT pointer
         USING CVT,R3                 , tell assembler
         L     R1,CVTTCBP             , get address of TCB list
         L     R1,12(R1)              , get our ASCB
         USING ASCB,R1                , tell assembler
         DROP  R1                     , no longer needed
         L     R3,CVTASVT             , get address of ASVT
         DROP  R3                     , not needed any more
         USING ASVT,R3                , tell assembler
         LA    R14,ASVTENTY-4         , point to first entry - 1 entry
         L     R15,ASVTMAXU           , number of ASIDs
SCANASVT DS    0H                     , search for AUTOPILOT
         BCTR  R15,R0                 , decrement number of ASISs
         LTR   R15,R15                , Last one?
         BZ    RETURN                 , yes, get out, no AUTOPILOT
         LA    R14,4(R14)             , next ASVT entry
         USING ASVTENTY,R14           , tell assembler
         ICM   R3,B'1111',ASVTENTY    , Get address of ASCB
         BM    SCANASVT               , try next if not active
         USING ASCB,R3                , R3 now points to ASCB
         ICM   R4,B'1111',ASCBJBNS    , address of STC name
         BZ    CHKJOB1                , if none, must be jobname
         CLC   =CL8'INIT',0(R4)       , is it 'INIT'
         BNE   CHKIT1                 , bif not
CHKJOB1  DS    0H                     , check for jobname
         ICM   R4,B'1111',ASCBJBNI    , address of JOB name
         BZ    SCANASVT               , if not, go araound again
CHKIT1   DS    0H                     , is task/job our AUTOPILOT?
         CLC   =CL8'&C3PO',0(R4)      , test for jobname
         BNE   SCANASVT               , if not ours, try next
         DROP  R3,R14                 , not needed any more
*        B     PROCATBL               , and continue
         TITLE 'Do defined action on certain messages'
***********************************************************************
* Autopilot is active.  We now test for any action rules defined      *
***********************************************************************
PROCATBL DS    0H                     , process the action table
         LA    R1,ACTNTABL            , point to action table
         USING ACTNNTRY,R1            , tell assembler
         LA    R3,(ACTNTABE-ACTNTABL)/ACTNNTRL  , R3 = Num of entries
SCANATBL DS    0H                     , scan the action table
         CLC   ACTNMSG,UCMMSTXT       , is message in table?
         BE    FNDATBL                , yes, leave loop
         LA    R1,ACTNNTRL(R1)        , next entry address
         BCT   R3,SCANATBL            , and go around again
         B     PROCSTBL               , not found, process suppress tbl
FNDATBL  DS    0H                     , found entry in table
         L     R15,ACTNRTNA           , get address of routine
         BR    R15                    , and branch to that routine
         DROP  R1                     , not needed any more
         TITLE 'Action Routines'
SHUTDOWN DS    0H                     , S TSO action routine
         SVC34 'P MF1'                , just issue start command
         SVC34 'P CMD1'               , just issue start command
         SVC34 'P BSPPILOT'           , just issue start command
         SVC34 '$P'                   , just issue start command
         SVC34 '$PJES2'               , just issue start command
         B     RETURN                 , and finish
STSO     DS    0H                     , S TSO action routine
         SVC34 'S TSO'                , just issue start command
         B     RETURN                 , and finish
EREPFULL DS    0H                     , S DUMPEREP routine
         SVC34 'S DUMPEREP'           , just issue start command
         B     RETURN                 , and finish
DUMPFULL DS    0H                     , S DUMPFULL routine
         SVC34 'S DUMPFULL'           , just issue start command
         B     RETURN                 , and finish
SMFFULL  DS    0H
         CLC   =C'SYS1.MANX',UCMMSTXT+27
         BE    SMFFULLX
SMFFULLY DS    0H
         SVC34 'S SMFDAILY,,,MAN=Y'
         B     RETURN
SMFFULLX DS    0H
         SVC34 'S SMFDAILY,,,MAN=X'
         B     RETURN
SETUPPRT DS    0H                     , JES asks for SETUP of printer
*              C'$HASP190 BSPWAIT  SETUP -- PRINTER2 -- F = 00
*              C'-----+----1----+----2----+-xxxxxxxx
         CLC   =C'PRINTER1',UCMMSTXT+27 Is this PRINTER1?
         BE    SETPRT1
         CLC   =C'PRINTER2',UCMMSTXT+27 or Printer 2
         BE    SETPRT2
         B     RETURN                      ???
SETPRT1  DS    0H                     , setup printer 1
         SVC34 '$SPRT1'
         B     RETURN
SETPRT2  DS    0H                     , setup printer 2
         SVC34 '$SPRT2'
         B     RETURN                 , ignore for now
         TITLE 'Process table of messages to be suppressed'
PROCSTBL DS    0H                     , process msgs to be suppressed
         LA    R1,SUPPTABL            , point to action table
         USING SUPPNTRY,R1            , tell assembler
         LA    R3,(SUPPTABE-SUPPTABL)/SUPPNTRL  , R3 = Num of entries
SCANSTBL DS    0H                     , scan the action table
         CLC   SUPPMSG,UCMMSTXT       , is message in table?
         BE    FNDSTBL                , yes, leave loop
         LA    R1,SUPPNTRL(R1)        , next entry address
         BCT   R3,SCANSTBL            , and go around again
         B     PROCDTBL               , go process descriptor codes
FNDSTBL  DS    0H                     , we found a message to suppress
         MVC   UCMROUTC(2),SUPROUTC   , ROUTCD=14
         MVC   UCMDESCD(2),SUPDESCD   , DESC=4
         B     RETURN                 , and exit
         DROP  R1                     , not needed any more
         TITLE 'Process table of Descriptor code changes'
PROCDTBL DS    0H                     , process msgs list
         LA    R1,DESCTABL            , point to action table
         USING DESCNTRY,R1            , tell assembler
         LA    R3,(DESCTABE-DESCTABL)/DESCNTRL  , R3 = Num of entries
SCANDTBL DS    0H                     , scan the action table
         CLC   DESCMSG,UCMMSTXT       , is message in table?
         BE    FNDDTBL                , yes, leave loop
         LA    R1,DESCNTRL(R1)        , next entry address
         BCT   R3,SCANDTBL            , and go around again
         B     PROCRTBL               , go process routing codes
FNDDTBL  DS    0H                     , we found a message to suppress
         L     R1,DESCDA              , get address of DESCD
         DROP  R1                     , not needed any more
         MVC   UCMDESCD(2),0(R1)      , insert descriptor code
*        B     PROCRTBL               , go process routing codes
         TITLE 'Process table of Routing code changes'
PROCRTBL DS    0H                     , process msgs list
         LA    R1,ROUTTABL            , point to routcode table
         USING ROUTNTRY,R1            , tell assembler
         LA    R3,(ROUTTABE-ROUTTABL)/ROUTNTRL  , R3 = Num of entries
SCANRTBL DS    0H                     , scan the action table
         CLC   ROUTMSG,UCMMSTXT       , is message in table?
         BE    FNDRTBL                , yes, leave loop
         LA    R1,ROUTNTRL(R1)        , next entry address
         BCT   R3,SCANRTBL            , and go around again
         B     PROCPTBL               , go process rePly tables
FNDRTBL  DS    0H                     , we found a message to suppress
         L     R1,ROUTCA              , get address of Routcode bytes
         DROP  R1                     , not needed any more
         MVC   UCMROUTC(2),0(R1)      , insert descriptor code
*        B     PROCPTBL               , go process rePly table
         TITLE 'Process msgs that need replies'
PROCPTBL DS    0H                     , process automatic replies
         LA    R1,REPLTABL            , point to routcode table
         USING REPLNTRY,R1            , tell assembler
         LA    R3,(REPLTABE-REPLTABL)/REPLNTRL  , R3 = Num of entries
SCANPTBL DS    0H                     , scan the action table
         CLC   REPLMSG,UCMMSTXT       , is message in table?
         BE    FNDPTBL                , yes, leave loop
         LA    R1,REPLNTRL(R1)        , next entry address
         BCT   R3,SCANPTBL            , and go around again
         B     RETURN                 , go process rePly tables
FNDPTBL  DS    0H                     , we found an entry
         L     R1,REPLNUM             , get reply identifier
         CLC   =C'IEF238D',UCMMSTXT   , special treatment for this
         BNE   DOSRB                  , bif not
         LA    R3,UCMMSTXT+19         , search for WAIT
         LA    R4,1                   , search increment
         LA    R5,UCMMSTXT+39         , upper limit of scan
SCAN238  DS    0H
         CLI   0(R3),C'W'             , search for "WAIT" in msg
         BE    DOSRB                  , we found it, give a reply
         BXLE  R3,R4,SCAN238          , go around again
*        B     RETURN                 , no "WAIT", then exit
***********************************************************************
* return to caller after freemaining the acquired storage             *
***********************************************************************
RETURN   EQU   *
         BSPRET RC=0
         TITLE 'Schedule R2D2'
***********************************************************************
*  We will schedule a subroutine to process the WTOR requests.  The   *
*  SRB will run in the address space of the BSPPILOT address space    *
*  If this address space is not active, no WTOR processing will take  *
*  place                                                              *
***********************************************************************
         SPACE
DOSRB    DS    0H                     , setup for scheduling SRB
         STH   R1,$REASON             , save reply text ID code
         L     R1,CVTPTR              , point to CVT
         L     R1,0(R1)               , get ASCB list address
         L     R1,12(R1)              , get our ASCB address
         MVC   $ASID,36(R1)           , save ASID
         GETMAIN R,LV=WORKAL,SP=&SRBSP  fixed storage for SRB etc
         LR    R3,R1                  , R3 ---> acquired storage
         MVC   0(WORKAL-L'WTOAREA,R3),WORKAREA  Copy out data to SQA
         PUSH  USING                  , save assembler info
         DROP  R10                    , not used for the moment
         USING WORKAREA,R3            , use R3 for addressing instead
         XC    SRB(SRBSIZE),SRB       , set SRB low low-values
         MVC   SRBID,=CL4'SRB '       , put in eyecatcher
         SPACE 2
***********************************************************************
* Check again if BSPPILOT is running.  If not, we cannot reply to the *
* WTOR.  We do this by scanning the ASCB chain (again) for BSPPILOT   *
***********************************************************************
         SPACE 1
         L     R2,CVTPTR              , R2 ---> CVT
         USING CVT,R2                 , tell aseembler
         L     R2,CVTASVT             , get ASVT
         USING ASVT,R2                , and tell Assembler
         LA    R14,ASVTENTY-4         , backup one entry
         L     R15,ASVTMAXU           , get number of ASCBs
NEXTASCB DS    0H                     , entry to loop
         BCTR  R15,R0                 , Minus 1
         LTR   R15,R15                , Last ASCB tested
         BZ    RETURN                 , then exit
         LA    R14,4(R14)             , bump to next ASVTENTY
         USING ASVTENTY,R14           , tell assembler
         ICM   R2,15,ASVTENTY         , get ASCB address
         BM    NEXTASCB               , if not active, go around again
         USING ASCB,R2                , R2 points to an ASCB
         ICM   R7,15,ASCBJBNS         , is this started task?
         BZ    CHKJOB                 , no.  Check for Jobname
         CLC   =CL8'INIT',0(R7)       , is this an initiator?
         BNE   CHKIT                  , bif not to test STC name
CHKJOB   ICM   R7,15,ASCBJBNI         , get address of jobname
         BZ    NEXTASCB               , if no Jobname, go araound
CHKIT    CLC   0(8,R7),=CL8'BSPPILOT' , Autopilot task?
         BNE   NEXTASCB               , if not, try next ASCB
         ST    R2,SRBASCB             , Place ASCB address into SRB
         L     R1,ASCBASXB            , get ASXB address
         L     R1,ASXBFTCB-ASXB(R1)   , get TCB address of BSPPILOT
         ST    R1,SRBPTCB             , and place it into SRB
         MVC   SRBPASID,ASCBASID      , put BSPPILOT ASID into SRB
         DROP  R2                     , we are done with R2
         LA    R1,IEECR2D2            , point to SRB routine address
         ST    R1,SRBEP               , and place address into SRB
         LA    R1,SRBCLEAN            , get address of cleanup routine
         ST    R1,SRBRMTR             , put address into SRB
         ST    R3,SRBPARM             , Work area address into SRB too
         SCHEDULE SRB=SRB,SCOPE=LOCAL , schedule the SRB
         POP   USING                  , restore assembler info
         B     RETURN                 , everything is done, exit
         SPACE 2
**********************************************************************
* The SRB Cleanup routine is rather simple.  Just take a look        *
**********************************************************************
         SPACE 1
SRBCLEAN BR    R14                    , just exit
         TITLE 'Constants - Literal Pool'
         LTORG
         TITLE 'Action Table'
***********************************************************************
* List of specific messages and the address of routines to invoke     *
* if this message is issued                                           *
***********************************************************************
ACTNTABL DS    0D
         DC    CL8'IST020I ',A(STSO)         start TSO
         DC    CL8'IEE362A ',A(SMFFULL)      start SMFDAILY
         DC    CL8'IEEXXXA ',A(EREPFULL)     start EREP processor
         DC    CL8'IEA994A ',A(DUMPFULL)     save all dump datasets
         DC    CL8'$HASP190',A(SETUPPRT)     setup printers
         DC    CL8'BSPSD999',A(SHUTDOWN)     initiate shutdown
ACTNTABE EQU   *
         TITLE 'Suppress Table'
***********************************************************************
* List of messages that get suppressed (I.E, routing code 00000000)   *
***********************************************************************
SUPPTABL DS    0D
         DC    CL8'$HASP000'          , HASP Ok message
SUPPTABE EQU   *
         TITLE 'Messages where descriptor code should be changed'
***********************************************************************
* List of messages that have their descriptor codes changes.          *
* The descriptor codes are generated via the IEECODES macro           *
* and can be found at label XXXDESCD, where XXX is the value on the   *
* ID keyword of the IEECODES macro                                    *
***********************************************************************
DESCTABL DS    0D
         DC    CL8'IEA911E ',A(ROLDESCD)  , dump on XXXX for asid NNNN
         DC    CL8'IEA994E ',A(ROLDESCD)  , dump on XXXX for asid NNNN
         DC    CL8'IEA994A ',A(ROLDESCD)  , all dump datsets are full
         DC    CL8'IGF995I ',A(ROLDESCD)  , I/O Restart scheduled
         DC    CL8'IGF991E ',A(ROLDESCD)  , IGF msg for mount, swap
DESCTABE EQU   *
         TITLE 'Messages where routecode code should be changed'
***********************************************************************
* List of messages that have their routing codes changed.             *
* The routing codes are generated via the IEECODES macro and can be   *
* found at label XXXROUTC, where XXX is the value on the ID keyword   *
* of the IEECODES macro                                               *
***********************************************************************
ROUTTABL DS    0D
         DC    CL8'IEA911E ',A(ROLROUTC)  , dump on XXXX for asid NNNN
         DC    CL8'IEA994E ',A(ROLROUTC)  , dump on XXXX for asid NNNN
         DC    CL8'IEA994A ',A(ROLROUTC)  , all dump datsets are full
         DC    CL8'IGF995I ',A(ROLROUTC)  , I/O Restart scheduled
         DC    CL8'IGF991E ',A(ROLROUTC)  , IGF msg for mount, swap
ROUTTABE EQU   *
         TITLE 'Message Reply Table'
***********************************************************************
* List of messages and their canned reply codes                       *
***********************************************************************
REPLWAIT EQU   0                      , R XX,WAIT
REPLNHLD EQU   4                      , R XX,NOHOLD
REPLU    EQU   8                      , R XX,U
REPLGO   EQU   12                     , R XX,GO
REPLPOST EQU   16                     , R XX,POST
REPLSIC  EQU   20                     , R XX,SIC
REPLCANC EQU   24                     , R XX,CANCEL
REPLTABL DS    0D
         DC    CL8'IEF238D ',A(REPLWAIT)
         DC    CL8'IEF433D ',A(REPLNHLD)
         DC    CL8'IEF434D ',A(REPLNHLD)
         DC    CL8'IKT010D ',A(REPLSIC)
         DC    CL8'IKT012D ',A(REPLU)
         DC    CL8'IEC301A ',A(REPLU)
         DC    CL8'IEC804A ',A(REPLPOST)
         DC    CL8'IFA006A ',A(REPLCANC)
         DC    CL8'BSPTEST0',A(REPLU)
REPLTABE EQU   *
         TITLE 'Routing- and Descriptor codes definitions'
         IEECODES ID=ROL,ROUTCDE=2,DESC=4
         IEECODES ID=SUP,ROUTCDE=14,DESC=4
         DROP
         TITLE 'IEECR2D2 - SRB Routine for processing WTORs'
***********************************************************************
* Actually, the SRB routine does not process the WTOR requests at     *
* all.  What it does is to schedule an Interrupt Request Routine      *
* which in turn will do what we need to do, namely isue the WTOR      *
* reply via SVC34                                                     *
***********************************************************************
IEECR2D2 DS    0H                     , entry point for our Robot
         BALR  R10,R0                 , set up base address
         USING *,R10                  , and tell assembler
         LR    R7,R14                 , save retrun address
         LR    R2,R1                  , get workarea address
         USING WORKAREA,R2            , and tell assembler
***********************************************************************
* We want to issue SVC 34, which means that we need an IRB.  To get   *
* the IRB via the CIRB macro, we need the local lock                  *
***********************************************************************
GETLOCK SETLOCK OBTAIN,               , ask for a lock                 +
               TYPE=LOCAL,            , we want the local lock         +
               REGS=USE,                                               +
               MODE=UNCOND,           , wait until we get the lock     +
               RELATED=FREELOCK       , here we will free the lock
***********************************************************************
* Create an IRB and an IQE.  For branch entry calls to CIRB we need   *
* R4 to point to the TCB                                              *
***********************************************************************
         L     R4,SRBPTCB             , get address of TCB from SRB
         CIRB  EP=IRBROUT,            , address of IRB routine         +
               KEY=SUPR,              , run in key 0                   +
               MODE=PP,               , run in problem mode            +
               BRANCH=YES,            , use branch entry, R4->TCB      +
               SVAREA=YES,            , get a save area                +
               STAB=(DYN),            , IRB is freed at termination    +
               RETIQE=NO,             , do not return IQE to queue     +
               WKAREA=30              , 30 doubleword workarea
         LR    R3,R1                  , R3 ---> IRB3
         USING RBBASIC,R3             , tell assembler
         L     R1,RBNEXAV             , R1 ---> IQE
         USING IQESECT,R1             , tell assembler
         ST    R3,IQEIRB              , put IRB address into IQE
         ST    R2,IQEPARM             , put worlarea address into IQE
         ST    R4,IQETCB              , put TCB address in IQE
         LCR   R1,R1                  , complement IQE address
         L     R12,CVTPTR             , get address of CVT
         USING CVT,R12                , tell assembler
         L     R14,CVT0EF00           , branch entry for SCHEDXIT
         BALR  R14,R14                , schedule the IQE
         DROP  R12                    , CVT bas enolonger needed
FREELOCK SETLOCK RELEASE,             , release                        +
               TYPE=LOCAL,            , the local lock                 +
               REGS=USE,                                               +
               RELATED=GETLOCK        , that we obtained above
         LR    R14,R7                 , restore return address
         BR    R14                    , back to dispatcher
         DROP                         , all USINGs
         TITLE 'IRBROUT - IRB Routine that REALLY processes the WTOR'
IRBROUT  SAVE  (14,12)                , save callers register
         LR    R12,R15                , R12 is out new base register
         USING IRBROUT,R12            , tell assembler
         LR    R10,R1                 , R10 points to WORKAREA
         USING WORKAREA,R10           , tell assembler
         ST    R13,WORKAREA+4         , higher SA into our SA
         ST    R10,8(R13)             , or SA into higher SA
         LR    R13,R10                , R13 ---> our save area
         STIMER WAIT,BINTVL=WAITTIME  , wait two seconds
         TITLE 'Scan the ORE chain'
*--------------------------------------------------------------------*
* Some WTOR is active that needs a reply.  We scan the ORE chain for *
* the request, determine the reply number, and issue the reply       *
* via SVC 34.  We might have to scan the chain more than once, though*
*--------------------------------------------------------------------*
         LA    R7,&ORESCAN            , load maximum number of scans
FNDORE   DS    0H                     , locate operator request element
         LA    R2,&MAXORE             , load max length of search
         L     R1,CVTPTR              , R1 ---> CVT
         USING CVT,R1                 , tell assembler
         L     R1,CVTCUCB             , R1 ---> Table with console UCBs
         DROP  R1                     , CVT not needed any longer
         USING UCM,R1                 , tell assembler
         L     R1,UCMRPYQ             , UCMRPYQ = address of first ORE
         DROP  R1                     , no longer needed
GETORE   DS    0H
         LA    R1,0(R1)               , clear high order byte
         USING OREF,R1                , tell assembler
         CLC   $ASID,OREASID          , is this our ASID?      ID ?
         BNE   NEXTORE                , no, get next ORE
         ICM   R6,B'0111',OREECBA     , get user's ECB address
         LA    R6,0(R6)               , clear higher byte
         C     R6,$ECB                , is this the one we want?
         BE    FOUND                  , yes, go process it
NEXTORE  DS    0H                     , otherwise
         L     R1,ORELKP              , address of next ORE
         DROP  R1                     , no longer needed
         LTR   R1,R1                  , last ORE?
         BZ    ENDLOOK                , test if we want another round
         BCT   R2,GETORE              , else try next ORE
ENDLOOK  DS    0H                     , ORE wasn't found, therefore
         STIMER WAIT,BINTVL=WAITTIME  , wait for 2 seconds
         BCT   R7,FNDORE              , and try again
         B     $EXIT                  , so many tries - but no success
         SPACE                        , just leave
FOUND    DS    0H                     , We found the RQE we needed
         LR    R7,R1                  , R7 ---> RQE
         SR    R11,R11                , clear branch register
         LH    R11,$REASON            , get message ID code
         CH    R11,=Y((REPLYE-REPLY)) , within bounds?
         BNL   $EXIT                  , get out if code too high
         B     REPLY(R11)             , and branch to routine needed
REPLY    B     RWAIT                  , 00: R XX,WAIT
         B     RNOHOLD                , 04: R XX,NOHOLD
         B     RU                     , 08: R XX,U
         B     RGO                    , 0C: R XX,GO
         B     RPOST                  , 10: R XX,POST
         B     RSIC                   , 14: R XX,SIC
         B     RCANCEL                , 18: R XX,CANCEL
REPLYE   EQU   *                      , end of branch table
RNOHOLD  EQU   *
         MVC   WTOAREA(REPLY1L),REPLY1
         B     ISSUE
RU       EQU   *
         MVC   WTOAREA(REPLY2L),REPLY2
         B     ISSUE
RGO      EQU   *
         MVC   WTOAREA(REPLY3L),REPLY3
         B     ISSUE
RPOST    EQU   *
         MVC   WTOAREA(REPLY4L),REPLY4
         B     ISSUE
RWAIT    EQU   *
         MVC   WTOAREA(REPLY5L),REPLY5
         B     ISSUE
RSIC     EQU   *
         MVC   WTOAREA(REPLY6L),REPLY6
         B     ISSUE
         SPACE
RCANCEL  EQU   *
         MVC   WTOAREA(REPLY7L),REPLY7
         B     ISSUE
         SPACE
ISSUE    EQU   *
         MVC   WTOAREA+6(2),4(R7)     , insert reply number from ORE
         LA    R1,WTOAREA             , point to command buffer
         SR    R0,R0                  , clear R0 for SVC 34
         SVC   34                     , send command
*        B     $EXIT                  , and exit
$EXIT    DS    0H                     , we are done
         LR    R1,R13                 , unchain workarea
         L     R13,WORKAREA+4         , address of higher SA
         FREEMAIN R,A=(1),LV=WORKAL,SP=245   free SQA storage
         LM    R14,R12,12(R13)        , restore resgisters
         LA    R15,0(0,0)             , RC = 0
         BR    R14                    , and exit
         TITLE 'Constants'
*------------------------------------------------------------------*
REPLY1   WTO   'R XX,''NOHOLD'' <<<<<< BY BSPPILOT',DESC=(5),          +
               ROUTCDE=(1,2,11),MF=L
REPLY1L  EQU   *-REPLY1
*------------------------------------------------------------------*
REPLY2   WTO   'R XX,''U''      <<<<<< BY BSPPILOT',DESC=(5),          +
               ROUTCDE=(1,2,11),MF=L
REPLY2L  EQU   *-REPLY2
*------------------------------------------------------------------*
REPLY3   WTO   'R XX,''GO''     <<<<<< BY BSPPILOT',DESC=(5),          +
               ROUTCDE=(1,2,11),MF=L
REPLY3L  EQU   *-REPLY3
*------------------------------------------------------------------*
REPLY4   WTO   'R XX,''POST''   <<<<<< BY BSPPILOT',DESC=(5),          +
               ROUTCDE=(1,2,11),MF=L
REPLY4L  EQU   *-REPLY4
*------------------------------------------------------------------*
REPLY5   WTO   'R XX,''WAIT''   <<<<<< BY BSPPILOT',DESC=(5),          +
               ROUTCDE=(1,2,11),MF=L
REPLY5L  EQU   *-REPLY5
*------------------------------------------------------------------*
REPLY6   WTO   'R XX,''SIC''    <<<<<< BY BSPPILOT',DESC=(5),          +
               ROUTCDE=(1,2,11),MF=L
REPLY6L  EQU   *-REPLY6
*------------------------------------------------------------------*
REPLY7   WTO   'R XX,''CANCEL'' <<<<<< BY BSPPILOT',DESC=(5),          +
               ROUTCDE=(1,2,11),MF=L
REPLY7L  EQU   *-REPLY7
*------------------------------------------------------------------*
WAITTIME DC    A(&WAITIME*100)        , Wait some time
         LTORG
         TITLE 'DSECTS USED BY IEECVXIT'
WORKAREA DSECT
$ECB     DS   A                       , requestor's ECB address
$ASID    DS   H                       , requestor's ASID
$REASON  DS   H                       , reply code
*                  0 = WAIT
*                  4 = NOHOLD
*                  8 = U
*                 12 = GO
*                 16 = POST
*                 20 = SIC
*                 24 = CANCEL
SCRATCH  DS    0F'0',CL130
         ORG   SCRATCH
SRB      DS    0A
SRBSECT  EQU   *
SRBID    DS    CL4                      EBCDIC ACRONYM   FOR SRB
SRBFLNK  DS    A                        FORWARD CHAIN FIELD
SRBASCB  DS    A                        PTR TO ASCB OF ADDRESS SPACE
*                                       SRB IS TO BE DISPATCHED TO
SRBFLC   DS    0CL8                     SRB AREA MOVED TO LOW CORE
SRBCPAFF DS    BL2                      CPU AFFINITY MASK
SRBPASID DS    H                        PURGEDQ ASID IDENTIFIER
SRBPTCB  DS    A                        PURGEDQ TCB IDENTIFIER
SRBEP    DS    A                        ENTRY POINT OF ROUTINE
SRBRMTR  DS    A                        ADDRESS OF RESOURCE MGR RTN
SRBPARM  DS    A                        USER PARAMETER
SRBSAVE  DS    A                        SAVE AREA POINTER
SRBPKF   DS    B                        PROTECT KEY INDICATION
SRBPRIOR DS    0B                       PRIORITY LEVEL INDIC
SRBFLGS  DS    B                        SRB OPTION FLAGS
SRBLLREQ EQU   X'80'                    LOCAL LOCK REQUIRED
SRBLLHLD EQU   X'40'                    LOCAL LOCK HELD
SRBFRREQ EQU   X'20'                    FRR REQUESTED
SRBFRRCL EQU   X'10'                    CLEAR FRR PARM AREA
SRBSUSP  EQU   X'08'                    SUSPENDED SRB ONLY ON FOR
*                                       SSRB
SRBPNONQ EQU   X'04'                    NON QUIESCABLE SRB
SRBRESV3 EQU   X'02'                    RESERVED FLAG
SRBRESV4 EQU   X'01'                    RESERVED FLAG
SRBPSYS  EQU   X'00'                    SYSTEM PRIORITY LEVEL
SRBHLHI  DS    BL1                      INDICATION OF SUSPEND LOCKS
*                                       HELD AT SRB SUSPENSION
         DS    BL1                      RESERVED
SRBFRRA  DS    A                        FRR ROUTINE ADDRESS
SRBEND   EQU   *                        END OF SRB
SRBSIZE  EQU   SRBEND-SRB               SIZE OF SRB
WTOAREA  EQU   SRB                      WORK AREA FOR WTO -MUST BE LAST
         ORG
WORKEND  EQU  *
WORKAL   EQU  WORKEND-WORKAREA
         PRINT OFF,NOGEN
         SPACE 4
         IEECUCM DSECT=YES,FORMAT=NEW
CVT      DSECT
         CVT   LIST=YES
         PRINT OFF
ASCB     DSECT
         IHAASCB
ASXB     DSECT
         IHAASXB
ASVT     DSECT
         IHAASVT
IHAPSA   DSECT
         IHAPSA
IHARB    DSECT
         IHARB  SYS=AOS2
         IHAORE                       , operator request element
         IHAIQE                       , interrupt queue element
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
//SYSPUNCH DD  DISP=SHR,DSN=SYS1.UMODOBJ(IEECVXIT)
//SMPREJ  EXEC SMPREC,COND=(0,NE)
//SMPCNTL  DD  *
 REJECT S(ZUM0003).
//SMPREC  EXEC SMPREC,COND=(0,NE,ASM)
//SMPPTFIN DD DATA,DLM=$$
++USERMOD(ZUM0003).
++VER(Z038) FMID(EBB1102).
++MOD(IEECVXIT) TXLIB(UMODOBJ).
$$
//SMPCNTL DD *
 RECEIVE SELECT(ZUM0003).
//SMPAPP  EXEC SMPAPP,
//       COND=((0,NE,ASM),
//             (0,NE,SMPREC.HMASMP))
//SMPCNTL DD  *
 APPLY G(ZUM0003) DIS(WRITE).
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=BSPPPROC
//* BSPPILOT PROC 
//IEFPROC EXEC PGM=BSPPILOT            <<<< added by Autopilot
//SYSUDUMP DD  SYSOUT=A
./ ADD NAME=BSPPILOT
//BSPPILOT  JOB  (SETUP),
//             'Run BSPPILOT',
//             CLASS=A,
//             MSGCLASS=H,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//********************************************************************
//*                                                                  *
//*  Name: SYS2.SAMPLIB(BSPPILOT)                                    *
//*                                                                  *
//*  Type: JCL to run program BSPPILOT                               *
//*                                                                  *
//*  Desc: Wait indefinitely until stop or modify command.  Used     *
//*        by Autooperator                                           *
//*                                                                  *
//********************************************************************
//OPRWT   EXEC PGM=BSPPILOT                  <=== Show all WTOs
//OPRWT   EXEC PGM=BSPPILOT,PARM=NOWTO       <=== Show no WTOs
//OPRWT   EXEC PGM=BSPPILOT,PARM=CATWTO      <=== Show the cat only
@@
//ASM     EXEC PGM=IFOX00,PARM='DECK,NOOBJECT,TERM,NOXREF'
//********************************************************************
//* You might have to change the DSNAMES in the next 2 DD statements *
//********************************************************************
//SYSIN    DD  DATA,DLM=@@
BSPPILOT TITLE 'Task for MVS Autopilot'
*********************************************************************** 00213
*                                                                     * 00290
*  Most of the time this program will do nothing. It will sit and wait* 00251
*  until it gets woken up via MODIFY or STOP command.  It will also   * 00252
*  wake up regularly via an alarm clock to avoid S522 abends          * 00290
*                                                                     * 00290
***********************************************************************
         PRINT OFF,NOGEN
CSCLDS   DSECT
         IEZCOM                       , CSCL layout
CIBDS    DSECT                        , CIB layout
         IEZCIB
         PRINT ON,NOGEN
BSPPILOT BSPENTER BASE=(R11,R12),RENT=NO
         TITLE 'Register Equates'
CIBCBAR  EQU   R9                     , Command Schedule Comm List
         USING CIBDS,CIBCBAR          , tell assembler
CSCLCBAR EQU   R10                    , Command Schedule Comm List
         USING CSCLDS,CSCLCBAR        , tell assembler
         MVC   TIMRWAIT,INITWAIT      , set wait time
         BAL   R14,GETPARMS           , read JCL PARM
         BAL   R14,INITWTO            , Say hello
         BAL   R14,XTRACTIT           , get CIB info from MVS
         BAL   R14,CHECKSTC           , Test if this is STC
         DO    WHILE=(TM,PROCFLAG,GETOUT,Z) loop till GETOUT flag set
          IF   (TM,PROCFLAG,RESETCIB,O) need to reset CIB?
           BAL R14,SETMAXRQ           , set maximum number of MODIFYs
          ENDIF                       , loop until stopped
          BAL  R14,WAITECBS           , wait for the ECB list
          BAL  R14,TESTPOST           , Test who/what woken us up
          BAL  R14,PROCECB            , process the ECB that was posted
         ENDDO                        , end of loop
         BAL   R14,SHUTCANC           , cancel shutdown sequence
         IF    (TM,PARMFLAG,PARMNWTO,Z) should WTO be issued
           WTO 'BSPPILOT - Program ended due to operator request'
         ENDIF
         BSPRET RC=0                  , return to caller
         TITLE 'Subroutines - DOMODIFY: Process the MODIFY commands'
***********************************************************************
* Process the commands the user has entered via the MODIFY OS cmd
***********************************************************************
         SPACE 1
DOMODIFY DS    0H                     , a MODIFY command was issued
         STM   R14,R1,DOMOSAVE        , save the registers
         LH    R15,CIBDATLN           , length of data from operator
         BCTR  R15,0                  , minus 1 for EX
         LA    R1,CIBDATA             , point to operator data
         BLANK PARMAREA               , init receiving area
         MVC   PARMAREA(*-*),0(R1)    , copy data to out storage
         EX    R15,*-6                , do the actual copy
         IF    (CLC,=C'STOP ',EQ,PARMAREA),OR,                         +
               (CLC,=C'END ',EQ,PARMAREA)
          OI   PROCFLAG,GETOUT        , indicate end of loop
         ELSEIF (CLC,=C'CAT ',EQ,PARMAREA)
          BAL   R14,INITWTO           , show splash screen
         ELSEIF (CLC,=C'COOKIE ',EQ,PARMAREA)
          ATTACH  EP=BSPFCOOK         , show fortune cookie
         ELSEIF (CLC,=C'APFLIST ',EQ,PARMAREA)
          ATTACH  EP=BSPAPFLS         , show APF datasets
         ELSEIF (CLC,=C'TEST ',EQ,PARMAREA)
          BAL  R14,TESTMSGS           , show WTO/WTOR for R2D2
         ELSEIF (CLC,=C'SCRIPT=',EQ,PARMAREA)
          LA   R0,7                   , Length of keyword SCRIPT=
          BAL  R14,RUNSCRPT       , run a script
         ELSEIF (CLC,=C'RUN=',EQ,PARMAREA)
          LA   R0,4                   , Length of Keyword RUN=
          BAL  R14,RUNSCRPT           , run a script
         ELSEIF (CLC,=C'SHUTDOWN ',EQ,PARMAREA)
          IF    (TM,PROCFLAG,SHUTDOWN,Z)    SHUTDOWN not yet active
           WTO  'BSPPILOT - Shutdown was requested',DESC=11
           ST   R1,SHTWTOID           , save WTOID
           OI   PROCFLAG,SHUTDOWN     , indicate SHUTDOWN in progress
           LA   R0,0                  , no keyword
           LA   R15,7                 , length of command (-1)
           BAL  R14,RUNSCRPT          , run the script
          ENDIF
         ELSEIF (CLC,=C'SHUTFAST ',EQ,PARMAREA)
          IF    (TM,PROCFLAG,SHUTDOWN,Z)    SHUTDOWN not yet active
           WTO  'BSPPILOT - Fast Shutdown was requested',DESC=11
           ST   R1,SHTWTOID           , save ID of WTO
           OI   PROCFLAG,SHUTDOWN     , indicate shutdown in progress
           LA   R0,0                  , no keyword
           LA   R15,7                 , length of command (-1)
           BAL  R14,RUNSCRPT          , run the script
          ENDIF
         ELSEIF (CLC,=C'SHUTCANC ',EQ,PARMAREA)
          BAL   R14,SHUTCANC
         ELSEIF (CLC,=C'SETPF ',EQ,PARMAREA)
          LINK  EP=BSPSETPF,PARAM=NOREPLYU
         ELSE
           WTO   'BSPPILOT - Modify command not recognized'
         ENDIF
         BAL   R14,DELETCIB           , remove processed CIB
         OI    PROCFLAG,RESETCIB      , indicate CIB needs reset
         LM    R14,R1,DOMOSAVE        , restore the registers
         BR    R14                    , return to caller
DOMOSAVE DS    4F                     , DOMODIFY Save area: R14 - R1
         TITLE 'Subroutines - TESTMSGS: Send WTOS for testing R2D2'
***********************************************************************
* Send standardized messages to allow testing of the R2D2 routine     *
***********************************************************************
         SPACE 1
TESTMSGS DS    0H                     , send messages to R2D2
         STM   R14,R1,TSTMSAVE        , save return address
          IF   (TM,PROCFLAG,WTORACTV,Z) If no WTOR currently active
          OI   PROCFLAG,WTORACTV      , indicate WTOR is active
          WTOR 'BSPTEST0 - Reply C to continue. U to ignore',          +
               WTOREPLY,L'WTOREPLY,WTORECB
          ST   R1,WTOID               , save message ID
          MVC  TIMRWAIT,WTORWAIT      , wait at most 30 seconds
          WTO  'BSPTEST1 - The message above is for testing the auto-pil
               lot, and will be cancelled automatically'
          WTO  'BSPTEST2 - The next message is $HASP000, which should be
               e suppressed'
          WTO  '$HASP000 Message generated by Autopilot'
          WTO  'BSPTEST3 - Last generated test message'
         ELSE
          WTO  'BSPPILOT - Test messages not generated, WTOR already act
               ive'
         ENDIF
         LM    R14,R1,TSTMSAVE        , restore registers
         BR    R14                    , return to caller
TSTMSAVE DS    4F
         TITLE 'Subroutines - RUNSCRPT: Run a script'
***********************************************************************
* Runs a named script.                                                *
* Registers on entry: R0 ---> Length of keyword passed                *
*                     R14 =   Return address                          *
*                     R15 =   Length modify parm - 1                  *
***********************************************************************
         SPACE 1
RUNSCRPT DS    0H                     , run a script
         STM   R14,R1,RUNSSAVE        , save return address
         IF    (TM,PROCFLAG,RUNSCACT,Z) BSPRUNSC not yet active
          BLANK RUNSCPR2              , set to blanks
          IF   (SR,R15,R0,NM)         , If length of Key value is plus
           IF  (CH,R15,GT,=H'7')
            LA R15,7                  , set maximum length
           ENDIF
           LA  R1,PARMAREA            , point to MODIFY parm
           AR  R1,R0                  , add length of keyword
           MVC RUNSCPR2(*-*),0(R1)    , R1 ---> Scriptname
           EX  R15,*-6                , insert script parmlist
           MVC RUN0011,RUNSCPR2       , and into msg
           LA  R15,1(R15)             , get real length
           STH R15,RUNSCPR1           , set length of script name
           CNOP 0,4
RUN0010    WTO  'BSPPILOT - Running script yyyyyyyy'
RUN0011    EQU  RUN0010+8+26,8
           OI   PROCFLAG,RUNSCACT     , disallow BSPRINSC processing
           ATTACH EP=BSPRUNSC,PARAM=RUNSCPRM,ECB=RUNSCECB
           ST  R1,TCBADDR             , save TCB address
           STC  R15,ONEBYTE           , save RC
           IF  (CLI,ONEBYTE,NE,0)     , error happened?
            XR R15,R15                , Clear workreg
            IC R15,ONEBYTE            , Load return code
            CVD R15,DOUBLE            , make a number
            UNPK RUN0021,DOUBLE+6(2)  , make printable
            OI  RUN0021+2,C'0'        , ditto
            MVI RUN0021,C' '          , ditto
            CNOP 0,4
RUN0020     WTO 'BSPPILOT - Script finished with RC=XXX. '
RUN0021     EQU RUN0020+8+35,3
           ENDIF
          ELSE
           WTO  'BSPPILOT - No script name given, request ignored'
          ENDIF
         ELSE
          WTO  'BSPPILOT - Script processor already active',           +
               '           This request ignored'
         ENDIF
         LM    R14,R1,RUNSSAVE        , restore registers
         BR    R14
RUNSSAVE DS    4F
         TITLE 'Subroutines: TIMERXIT - STIMER Exit Routine'
***********************************************************************
* This exit routine is invoked whenever the STIMER pops.  It will then*
* post the TIMER ECB, that way waking up the Autopilot proper         *
***********************************************************************
         SPACE 1
TIMERXIT DS    0H                     , STIMER Exit Routine
XOFFSET  EQU   TIMERXIT-BSPPILOT      , Used for calculating base
         STM   R14,R12,12(R13)        , save caller's registers
         LR    R11,R15                , save entry point address
         LA    R15,XOFFSET            , get offset from first base
         SR    R11,R15                , calculate 1 base of main pgm
         LR    R3,R13                 , Higher SA
         LA    R13,TIMERSA            , out SA
         ST    R3,4(0,R13)            , Higher SA into lower SA
         ST    R13,8(0,R3)            , lower SA into Higher SA
         LA    R3,TIMERECB            , address of ECB
         POST  (R3)                   , post it
         L     R13,4(0,R13)           , restore higher SA
         LM    R14,R12,12(R13)        , restore registers
         BR    R14                    , return to caller
TIMERSA  DC    18F'0'
         TITLE 'Subroutines: GETPARMS - Get JCL Paramters'
***********************************************************************
* This routines reads the JCL parameters and sets PARMFLAg accordingly*
***********************************************************************
GETPARMS DS    0H
         STM   R14,R1,GETPSAVE        , save return address
         BLANK PARMAREA               , clear parm text area
         XC    PARMFLAG,PARMFLAG      , clear parm flag
         XC    PROCFLAG,PROCFLAG      , clear processing flag
         L     R1,0(0,R1)             , Address of passed parm
         LH    R15,0(0,R1)            , Length of Parameter
         LA    R1,2(0,R1)             , address of parameter
         IF    (LTR,R15,R15,Z)        , No parm present?
          OI   PARMFLAG,PARMCWTO      , then at least allow the cat
         ELSE                         , some parm was provided
          BCTR R15,0                  , decrement for EXecute
          MVC  PARMAREA(*-*),0(R1)    , move in parameters
          EX   R15,*-6                , move in the parm text
          OC   PARMAREA,=CL100' '     , Convert to upper case
          IF   (CLC,=C'NOWTO ',EQ,PARMAREA) Silent mode, indicate
           OI  PARMFLAG,PARMNWTO      , by setting bit flag
           OI  PROCFLAG,MSGDOMED      , indicate no DOM needed
          ELSEIF (CLC,=C'CATWTO ',EQ,PARMAREA) Splash screen?
           OI  PARMFLAG,PARMCWTO      , Allow the little cat
           OI  PARMFLAG,PARMNWTO      , Disallow everything else
          ELSE
           OI  PARMFLAG,PARMCWTO      , Allow splash
          ENDIF
         ENDIF
         LM    R14,R1,GETPSAVE        , restore return address
         BR    R14                    , and return to caller
GETPSAVE DS    4F
         TITLE 'Subroutine: INITWTO - Display splash message'
***********************************************************************
* This routine shows the splash screen,  When the timer pops, the     *
* splash screen will be domed                                         *
***********************************************************************
INITWTO  DS    0H                     , Display splash screen
         STM   R14,R1,INITSAVE        , save registers
         MVC   TIMRWAIT,INITWAIT      , set initial wait time
         IF    (TM,PARMFLAG,PARMCWTO,O) Should the cat be shown?
          WTO  '                                 ',                    +
               '         |l      _,,,---,,_      ',                    +
               '   ZZZzz /,:.-'':''    -.  ;-;;,   ',                  +
               '        |,4-  ) )-,_. ,( (  :''-'' ',                  +
               '       ''---''''(_/--''  :-'')_)      ',               +
               '                                 ',                    +
               ' Don''t disturb me, I am snooz ... errr working',      +
               '                                 ',                    +
               DESC=11
          ST   R1,MSGID               , save message ID
          NI   PROCFLAG,255-MSGDOMED  , Message is visible
         ENDIF                        ,
         LM    R14,R1,INITSAVE        , restore return address
         BR    R14                    , return to caller
INITSAVE DS    4F                     , save area
         TITLE 'Subroutines: XTRACTIT - Get CIB Info from MVS'
***********************************************************************
* Use EXTRACT to get CIB information from MVS                         *
***********************************************************************
         SPACE 1
XTRACTIT DS    0H                     , get CIB info from MVS
         STM   R14,R1,XTRASAVE        , save return address
         EXTRACT CSCLADDR,FIELDS=COMM , get the CSCLADDR from MVS
         LM    R14,R1,XTRASAVE        , restore return address
         BR    R14                    , return to caller
XTRASAVE DS    4F                     , save area
         TITLE 'Subroutines: CHECKSTC - Check if STC, delete CIB'
***********************************************************************
* Check if a started task, and if so, delete initial CIB              *
***********************************************************************
         SPACE 1
CHECKSTC DS    0H                     , Set max number of MOFIFY cmds
         STM   R14,R1,CHECSAVE        , save return address
         L     CSCLCBAR,CSCLADDR      , get address of CSCL
         L     CIBCBAR,COMCIBPT       , get address of CIB
         IF    (LTR,CIBCBAR,CIBCBAR,NZ) Is there a CIB?
          BAL  R14,DELETCIB           , delete START CIB of STC
          OI   PROCFLAG,RESETCIB      , indicate reset required
         ENDIF
         LM    R14,R1,CHECSAVE        , restore return address
         BR    R14                    , return to caller
CHECSAVE DS    4F
         TITLE 'Subroutines: DELETCIB - Delete first CIB on chain'
***********************************************************************
* Delete the first CIB from the chain                                 *
***********************************************************************
         SPACE 1
DELETCIB DS    0H                     , Delete first CIB on chain
         STM   R14,R1,DELESAVE        , save registers
         L     CSCLCBAR,CSCLADDR      , get address of CSCL
         L     CIBCBAR,COMCIBPT       , get address of CIB
         QEDIT ORIGIN=COMCIBPT,BLOCK=(CIBCBAR) free CIB
         LM    R14,R1,DELESAVE        , restore return address
         BR    R14                    , return to caller
DELESAVE DS    4F                     , save area
         TITLE 'Subroutines: SETMAXRQ - Set max number of MODIFY cmds'
***********************************************************************
* Set maximum number of concurrent MODIFY requests                    *
***********************************************************************
         SPACE 1
SETMAXRQ DS    0H                     , Set max number of MOFIFY cmds
         STM   R14,R1,SETMSAVE        , save return address
         L     CSCLCBAR,CSCLADDR      , get address of CSCL
         QEDIT ORIGIN=COMCIBPT,CIBCTR=1 only 1 MODIFY at a time
         LM    R14,R1,SETMSAVE        , restore return address
         BR    R14                    , return to caller
SETMSAVE DS    4F                     , save area
         TITLE 'Subroutines: TESTPOST - Has CIB been posted'
***********************************************************************
* This routines tests which of the ECBs in the ECBlist has been       *
* posted, and why                                                     *
* Registers on Entry:  R14 = Return address                           *
* Registers on Exit :  R15 = Return code                              *
*                        0 : MODIFY command                           *
*                        4 : STOP command                             *
*                        8 : TIMER ECB posted or WTOR ECB posted      *
*                       12 : BSPRUNSC ECB posted                      *
***********************************************************************
TESTPOST DS    0H                     , Test ECBLISt and posted ECB
         STM   R14,R1,TESTSAVE        , save return address
         L     CSCLCBAR,CSCLADDR      , get address of CSCL
         L     R1,COMECBPT            , address of ECB
         NI    PROCFLAG,255-RESETCIB  , turn off reset flag
         IF    (TM,0(R1),ECBPOST,O)   , if CIB ECB was posted
          BAL  R14,CMDCHECK           , Check if MODIFY or POST
          OI   PROCFLAG,RESETCIB      , inicate CIB needs a reset
         ELSEIF (TM,TIMERECB,ECBPOST,O)
          IF   (TM,PROCFLAG,WTORACTV,O) Is a WTOR active?
           L   R1,WTOID               , get ID of WTOR
           DOM MSG=(1),REPLY=YES      , and kill it
           NI  PROCFLAG,255-WTORACTV  , turn off flag
           XC  WTOID,WTOID            , clear ID
           WTO 'BSPPILOT - Message cancelled due to Timeout'
          ENDIF
          LA   R15,8                  , return with RC=8,
         ELSEIF (TM,WTORECB,ECBPOST,O) Reply given to WTOR?
          NI   PROCFLAG,255-WTORACTV  , turn off flag
          XC   WTOID,WTOID            , clear ID
          LA   R15,8                  , return with RC=8,
         ELSEIF (TM,RUNSCECB,ECBPOST,O) if TIMER ECB was posted
          LA   R15,12                 , indicate RUNSC ECB posted
         ELSE
          WTO  'BSPPILOT - ECB posted but not part of ECBLIST'
          ABEND 3,DUMP
         ENDIF
         ST    R15,TESTSAVE+4         , set caller's R15
         LM    R14,R1,TESTSAVE        , restore return address
         BR    R14                    , return to caller
TESTSAVE DS    4F                     , save area
         TITLE 'Subroutines: CMDCHECK - Test type of command'
***********************************************************************
* Test type of command,  could be STOP command (p) or MODIFY (f)      *
* Registers on Entry: R14 = Return address                            *
* Registers on Exit : R15 = Return code                               *
*                       0 = MODIFY command                            *
*                       4 = STOP   command                            *
***********************************************************************
         SPACE 1
CMDCHECK DS    0H                     , Test type of command
         STM   R14,R1,CMDCSAVE        , save return address
         L     CSCLCBAR,CSCLADDR      , get address of CSCL
         L     CIBCBAR,COMCIBPT       , get address of CIB
         IF    (CLI,CIBVERB,EQ,CIBMODFY) MODIFY Command?
          IF   (TM,PARMFLAG,PARMNWTO,Z)  Do we want WTOs?
           WTO 'BSPPILOT - Modify command accepted'
          ENDIF
          LA   R15,0                  , indicate MODIFY
         ELSEIF (CLI,CIBVERB,EQ,CIBSTOP)
          IF   (TM,PARMFLAG,PARMNWTO,Z)  Do we want WTOs?
           WTO 'BSPPILOT - Stop command accepted'
          ENDIF
          LA   R15,4                  , indicate STOP
         ELSE
          WTO   'BSPPILOT - Neither STOP nor MODIFY command'
          ABEND 2,DUMP                , abend, with dump
         ENDIF
         ST    R15,CMDCSAVE+4         , set caller's R15
         LM    R14,R1,CMDCSAVE        , restore register
         BR    R14                    , return to caller
CMDCSAVE DS    4F                     , save area
         TITLE 'Subroutines - WAITECBS: Wait for for list of ECBS'
***********************************************************************
* Setup the list of ECBs, and wait.  Regardless which ECB got posted  *
* remove the spalsh screen (if any)                                   *
***********************************************************************
         SPACE 1
WAITECBS DS    0H                     , Wait for list of ECBs
         STM   R14,R1,WAITSAVE        , Save Registers
         XC    TIMERECB,TIMERECB      , clear ECB
         XC    RUNSCECB,RUNSCECB      , clear ECB
         XC    WTORECB,WTORECB        , clear ECB
         LA    R1,TIMERECB            , get address of timer ECB
         ST    R1,TIMECBA             , and save it
         LA    R1,WTORECB             , address if WTOR ECB
         ST    R1,WTOECBA             , put into ECB List
         L     CSCLCBAR,CSCLADDR      , get address of CSCL
         L     R1,COMECBPT            , address of ECB
         ST    R1,CIBECBA             , into wait list
         LA    R1,RUNSCECB            , address of BSPRUNSC ECB
         ICM   R1,B'1000',=X'80'      , insicate last ECB
         ST    R1,RSECBA              , Put into ECB List
         STIMER REAL,TIMERXIT,DINTVL=TIMRWAIT
         WAIT  1,ECBLIST=ECBLIST      , wait on ECBLIST
         MVC   TIMRWAIT,WAITTIME      , set normal wait time
         IF    (TM,PROCFLAG,MSGDOMED,Z) message still visible
          OI   PROCFLAG,MSGDOMED      , indicate it has been domed
          L    R1,MSGID               , get message ID
          DOM  MSG=(1)                , and delete it
         ENDIF
         LM    R14,R1,WAITSAVE        , restore registers
         BR    R14                    , return to caller
WAITSAVE DS    4F                     , WAITSAVE Save area R14 - R1
         TITLE 'Subroutines - PROCECB : Process the posted ECB'
***********************************************************************
* An ECB has been posted.  Do the appropriate action                  *
***********************************************************************
PROCECB  DS    0H                     , Process the posted ECB
         STM   R14,R1,PROCSAVE        , Save Registers
         IF    (CH,R15,EQ,=H'0')      , Was it a MODIFY command
          BAL  R14,DOMODIFY           , process inout command
         ELSEIF (CH,R15,EQ,=H'4')     , STOP command
          OI   PROCFLAG,GETOUT        , Leave the loop
         ELSEIF (CH,R15,EQ,=H'8')     , TIMER ECB posted
          NI   PROCFLAG,255-RESETCIB  , No reset of CIB needed
         ELSEIF (CH,R15,EQ,=H'12')    , BSPRUNSC ECB posted?
          BAL  R14,DORUNSC            , show BSPRUNSC completion code
         ELSE                         , this must be invalid
          CVD  R15,DOUBLE             , Make a number
          BLANK PROC011               , initialize
          UNPK PROC011,DOUBLE+6(2)    , make zoned
          OI   PROC011+L'PROC011-1,C'0' and printable
PROC010   WTO 'BSPPILOT - Invalid return code (XXXX) from TESTPOST'
PROC011   EQU PROC010+8+32,4
          ABEND 4,DUMP
         ENDIF
         LM    R14,R1,PROCSAVE        , restore the registers
         BR    R14                    , return to caller
PROCSAVE DS    4F                     , PROCECB  Save area: R14 - R1
         TITLE 'Subroutines - DORUNSC : Process RUNSC ECB'
***********************************************************************
* The RUNSC ECB has been posted.  Detach the task, and report CC      *
***********************************************************************
DORUNSC  DS    0H                     , Process the RUNSC ECB
         STM   R14,R1,DORUSAVE        , save registers
         DETACH TCBADDR               , and detach task
         XR    R1,R1                  , Clear workreg
         IF    (ICM,R1,B'0111',RUNSCECB+1,NZ) Any error?
          L    R1,RUNSCECB            , get contents of ECB
          N    R1,=X'00FFF000'        , mask out user code
          SRL  R1,12                  , R1 = '00000FFF'
          ST   R1,FULLW               , put into work area
          X2CHRTRN DORU011,FULLW+2,LEN=2 Make 4 bytes printable
          MVI  DORU011,C'S'           , first byte overlayed by 'S'
          L    R1,RUNSCECB            , get contents of ECB
          N    R1,=X'00000FFF'        , mask out system code
          CVD  R1,DOUBLE              , make a decimal number
          UNPK DORU012,DOUBLE+5(3)    , place into message
          OI   DORU012+L'DORU012-1,C'0' make last character printable
          CNOP  0,4
DORU010   WTO  'BSPPILOT - Script processor ended, SXXX, Uxxxx'
DORU011   EQU  DORU010+8+35,4
DORU012   EQU  DORU010+8+42,4
         ENDIF
         NI    PROCFLAG,255-RUNSCACT  , allow BSPRUNSC processing now
         BAL   R14,SHUTCANC           , remove shutdown msg if any
         LM    R14,R1,DORUSAVE        , restore registers
         BR    R14                    , retrun to caller
DORUSAVE DS    4F
         TITLE 'Subroutines - SHUTCANC: Cancel Shutdown Sequence'
***********************************************************************
* The shutdown TCB will be detached unconditionally                   *
***********************************************************************
SHUTCANC DS    0H                     , Cancel shutdown
         STM   R14,R1,SHUTSAVE        , save registers
         IF    (TM,PROCFLAG,SHUTDOWN+RUNSCACT,O)
           WTO 'BSPPILOT - Shutdown sequence terminated'
           NI  PROCFLAG,255-SHUTDOWN-RUNSCACT
           DETACH TCBADDR             , stop BSPRUNSC
           L   R1,SHTWTOID            , Get ID of shutdown WTO
           IF  (LTR,R1,R1,NZ)         , if any
            XC SHTWTOID,SHTWTOID      , clear field
            DOM MSG=(1)               , unhighlight shutdown msg
           ENDIF                      , is there a MSGID
         ENDIF                        , is shutdown in progress
         LM    R14,R1,SHUTSAVE        , restore registers
         BR    R14                    , return to caller
SHUTSAVE DS    4F                     , SHUTCANC Save Area: R14 - R1
         TITLE 'Constants'
WTORWAIT DC    C'00003000'            , 30 seconds wait time
INITWAIT DC    C'00001000'            , 10 seconds wait time
WAITTIME DC    C'00100000'            , 10 minutes wait time
         DS    0F
EXECPARM DC    AL2(4),C'EXEC'
         DS    0F
NOREPLYU DC    AL2(8),C'NOREPLYU'
         TITLE 'Variables'
TIMRWAIT DS    D
DOUBLE   DS    D                      , general purpose doubleword
FULLW    DS    F                      , general Purpose fullword
CSCLADDR DS    F                      , Reply area for EXTRACT
TIMERECB DS    F                      , number of ECBs in list
WTORECB  DS    F                      , number of ECBs in list
RUNSCECB DS    F                      , RUNSC ECB
ECBLIST  DS    0F                     , list of ECBs we are waiting for
WTOECBA  DS    F
TIMECBA  DS    F                      , posted on STIMER WAIT
CIBECBA  DS    F                      , posted on MODIFY/STOP
RSECBA   DS    F                      , RUNSC subtask ECB
MSGID    DS    F                      , ID of initial message
WTOID    DS    F                      , ID of WTOR to be cancelled
SHTWTOID DS    F                      , ID of shutdown WTO
TCBADDR  DS    F                      , address of BSPRUNSC TCB
RUNSCPRM DS    0F                     , parameters for BSPRUNSC
RUNSCPR1 DS    AL2                    , length of parm
RUNSCPR2 DS    CL8                    , name of script
ECBPOST  EQU   B'01000000'            , X'40': ECB was posted
PROCFLAG DS    X                      , processing flag
INITDONE EQU   B'10000000'            , initialization is done
MSGDOMED EQU   B'01000000'            , Initial message deleted
WTORACTV EQU   B'00100000'            , WTOR on screen
RESETCIB EQU   B'00010000'            , Reset of CIB required
GETOUT   EQU   B'00001000'            , End of program requested
RUNSCACT EQU   B'00000100'            , BSPRUNSC is active
SHUTDOWN EQU   B'00000010'            , SHUTDOWN in progress
PARMFLAG DS    X                      , JCL PARM flag
PARMNWTO EQU   B'10000000'            , X'80': PARM=NOWTO specified
PARMCWTO EQU   B'01000000'            , X'04': PARM=CATWTO specified
PARMINV  EQU   B'00000001'            , X'01': invalid PARM field
PARMAREA DS    CL100                  , for JCL parameters
WTOREPLY DS    0C                     , reply area for WTOR
ONEBYTE  DS    C                      , Work area for RC
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
//SYSPUNCH DD  DISP=(NEW,PASS),UNIT=VIO,SPACE=(CYL,(1,1))
//LINK    EXEC PGM=IEWL,
//             COND=(0,NE),
//             PARM='XREF,LIST,MAP'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(1,1))
//SYSLIN   DD  DISP=(OLD,DELETE),DSN=*.ASM.SYSPUNCH
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.LINKLIB(BSPPILOT)