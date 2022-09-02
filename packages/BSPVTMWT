//BSPVTMWT JOB (JOB),
//             'INSTALL BSPVTMWT',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//ASM     EXEC PGM=IFOX00,PARM='DECK,NOOBJECT,TERM,NOXREF'
//SYSIN    DD  DATA,DLM=@@
BSPVTMWT TITLE 'Wait for VTAM ACB'
***********************************************************************
*                                                                     *
* This routine waits for a VTAM ACB to become available.  The ACB     *
* name to be waited for is specified in the PARM keyword of the       *
* JCL EXEC Card                                                       *
*                                                                     *
* The program will retry to open the ACB a predetermined number of    *
* times, which can be set in symbol MAXTRIES (see below).  The        *
* wait time in between retries is controlled by the RETRYSEC          *
* symbol (see below)                                                  *
*                                                                     *
***********************************************************************
         SPACE 1
BSPVTMWT BSPENTER RENT=NO                   , non-reentrant SA chains
MAXTRIES EQU   30                           , maximum retries
RETRYSEC EQU   10                           , seconds between retries
         L     R4,0(,R1)                    , get address of parms
         USING PARMLIST,R4                  , tell assembler
         WTO   MF=(E,VTW0100I)              , tell the world we're here
         LH    R2,PARMLEN                   , get length of parms
         IF    (LTR,R2,R2,Z)                , no parm given?
          MVC  RETCODE,=F'8'                , Set RC field
          WTO  MF=(E,VTW0102E)              , issue message
          B    RETURN                       , and go back to caller
         ELSEIF (CH,R2,GT,=H'8')            , length too big?
          WTO  MF=(E,VTW0101W)              , else issue message
          LA   R2,8                         , set maximum length
         ENDIF                              , of length checking
         STC   R2,APPLIDL                   , set length field
         BCTR  R2,0                         , decrement for EXecute
         BLANK APPLIDT                      , init applid text
         MVC   APPLIDT(*-*),PARMTEXT        , Insert text
         EX    R2,*-6                       , via EXecute
         MVC   VTW0201I+3+32(8),APPLIDT     , insert applid into
         MVC   VTW0202I+3+20(8),APPLIDT     , the various messages
         MVC   VTW0203I+3+32(8),APPLIDT     , that could be
         MVC   VTW0204E+3+33(8),APPLIDT     , displayed
         MVC   VTW0205E+3+32(8),APPLIDT     , by
         MVC   VTW0206E+3+32(8),APPLIDT     , this
         MVC   VTW0207I+3+34(8),APPLIDT     , program
         WTO   MF=(E,VTW0201I)              , issue WAIT message
         LA    R6,APPLACB                   , point to ACB
         USING IFGACB,R6                    , tell assembler
         DO    FROM=(R5)                    , set number of retries
OPENACB  DS    0H                           , try to
         OPEN  APPLACB                      , open the ACB
         IF    (LTR,R15,R15,Z)              , ACB available?
          CLOSE APPLACB                     , just close it
          WTO  MF=(E,VTW0202I)              , tell the world
          MVC  RETCODE,=F'0'                , set RC = 0
          B    RETURN                       , and return to caller
         ENDIF                              , OPEN successful
         X2CHRTRN WORK,ACBERFLG,LEN=1       , make printable
         MVC   VTW0207I+3+23(L'WORK),WORK   , make printable
         WTO   MF=(E,VTW0207I)              , tell the world
         IF    (CLI,ACBERFLG,EQ,X'58')      , ACB already open?
          WTO  MF=(E,VTW0205E)              , tell everyone
          MVC  RETCODE,=F'4'                , Load RC
          B    RETURN                       , and finish
         ELSEIF (CLI,ACBERFLG,EQ,X'5A')     , unknown ACB?
          WTO  MF=(E,VTW0206E)              , tell everyone
          MVC  RETCODE,=F'16'               , Load RC
          B    RETURN                       , and finish
         ENDIF
         WTO   MF=(E,VTW0203I)              , issue RETRY message
         STIMER WAIT,BINTVL=INTERVAL        , wait for some time
         ENDDO                              , FROM=(R5)
         MVC   RETCODE,=F'12'               , indicate timeout
         WTO   MF=(E,VTW0204E)              , tell the world
*        B     RETURN                       , and go back to caller
RETURN   DS    0H                           , at end of module
         WTO   MF=(E,VTW0999I)              , issue a message
         L     R15,RETCODE                  , pick up RC
         BSPRET RC=(15)                     , return to caller
RETCODE  DS    F                            , RC field
INTERVAL DC    A(RETRYSEC*100)              , 10 seconds wait time
WORK     DS    CL2
VTW0100I WTO   'BSPVW00I - Application wait processor starting',MF=L
VTW0101W WTO   'BSPVW01W - EXEC Parm too long, truncated',MF=L
VTW0102E WTO   'BSPVW02E - EXEC Parm missing, terminating|',MF=L
VTW0201I WTO   'BSPVW01I - Waiting for ACB for XXXXXXXX  ',MF=L
VTW0202I WTO   'BSPVW02I - ACB for XXXXXXXX is available',MF=L
VTW0203I WTO   'BSPVW03I - Retrying opening of XXXXXXXX ACB',MF=L
VTW0204E WTO   'BSPVW04E - Time out waiting for XXXXXXXX',MF=L
VTW0205E WTO   'BSPVW05E - ACB for application XXXXXXXX in use',MF=L
VTW0206E WTO   'BSPVW06E - ACB for application XXXXXXXX not found',MF=L
VTW0207I WTO   'BSPVW07I - Error code XX opening XXXXXXXX',MF=L
VTW0999I WTO   'BSPVW99I - Application wait processor ended',MF=L
APPLID   DS    0CL9                         , application id field
APPLIDL  DS    C                            , length of applid
APPLIDT  DS    CL8                          , text of applid
APPLACB  ACB   AM=VTAM,APPLID=APPLID        , VTAM ACB to open
PARMLIST DSECT                              , JCL EXEC PARM layout
PARMLEN  DS    H                            , Length of EXEC parm
PARMTEXT DS    CL100                        , Text of EXEC parm
         IFGACB AM=VTAM                     , ACB mapping macro
         BSPEND                             , of module
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
//             PARM='LIST,LET,MAP,AC=1'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(TRK,(50,20))
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.LINKLIB
//SYSLIN   DD  DISP=(OLD,DELETE),DSN=*.ASM.SYSPUNCH
//          DD *
 NAME BSPVTMWT(R)
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=BSPVTMWT
//TSO     PROC MEM=00
//********************************************************************
//*
//* Name: BSPVTMWT
//*
//* Desc: Run the BSPVTMWT program
//*
//********************************************************************
//********************************************************************
//* THIS IS ONLY NEEDED IF YOU DO NOT USE THE AUTOPILOT FUNCTIONS    *
//********************************************************************
//WAITTSO EXEC PGM=BSPVTMWT,PARM='TXX'
//TSO     EXEC PGM=IKTCAS00,TIME=1440,COND=(4,LT,WAITTSO)
//PARMLIB  DD  DISP=SHR,DSN=SYS1.PARMLIB(TSOKEY&MEM),FREE=CLOSE
//PRINTOUT DD  SYSOUT=A,DCB=(LRECL=133,RECFM=FBB)
//
@@