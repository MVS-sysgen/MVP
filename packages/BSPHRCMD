//BSPHRCMD JOB (JOB),
//             'INSTALL BSPHRCMD',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//* THIS JCL WAS GENERATED AUTOMATICALLY BY make_release.sh
//*
//RACUPDT EXEC PGM=IEBGENER
//SYSUT1   DD  DATA,DLM=@@
BSPHRCMD TITLE 'Execute HERCULES commands from PARM and SYSIN'          00010000
*********************************************************************** 00020000
*  This program will read a command from the PARM statement or a      * 00030000
*  sequence of commands from SYSIN and execute them on the Hercules   * 00040000
*  console via the DIAG 8 interface                                   * 00050000
*                                                                     * 00060000
*  Required DD statement: none                                        * 00070000
*                                                                     * 00080000
*  Optional DD statements:                                            * 00090000
*           SYSIN    -  Input dataset for control statements          * 00100000
*                                                                     * 00110000
*           SYSPRINT -  Output dataset for messages etc               * 00120000
*                       Default: SYSOUT=*                             * 00130000
*                                                                     * 00140000
*           SYSUDUMP -  Default: SYSOUT=A                             * 00150000
**                                                              JW12109 00151000
** Updated 18APR12 - Authorization through RAC added.           JW12109 00152000
**   On systems with resource access control active BSPHRCMD    JW12109 00153000
**   executes only if the caller has read access to profile     JW12109 00154000
**   DIAG8CMD in the FACILITY class.                            JW12109 00155000
**                                                              JW12109 00156000
*********************************************************************** 00160000
         PRINT OFF,NOGEN                                                00170000
         COPY  BSPGLBLS                                                 00180000
         COPY  BSPSGLBL                                                 00190000
         DCBD  DSORG=(PS),DEVD=DA     , DCB layout                      00200000
         IEFZB4D0                     , dynalloc dsects                 00210000
         IEFZB4D2                     , and equates                     00220000
         CVT   DSECT=YES                                        JW12109 00222000
         IHAPSA ,                                               JW12109 00224000
CVTSAF   EQU   248 CVTSAF doesn't exist but is a reserved field in 3.8J 00226000
         ICHSAFV  DSECT=YES  map SAFV                           JW12109 00228000
         PRINT ON,NOGEN               , Macro expansion not needed      00230000
BSPHRCMD BSPENTER BASE=(R11),RENT=YES                                   00240000
         BAL   R14,ALCUDUMP           , allocate SYSUDUMP if needed     00250000
         BAL   R14,SETINIT            , initialize some variables       00260000
         BAL   R14,ALCPRINT           , allocate SYSPRINT if needed     00270000
         IF    (LTR,R15,R15,Z)        , if allocated                    00280000
          BAL  R14,OPNPRINT           , open SYSPRINT                   00290000
         ENDIF (LTR,R15,R15,Z)        ,                                 00300000
         BAL   R14,GETPARAM           , get parameter, if any           00310000
         IF    (LTR,R15,R15,Z)        , if there was a parameter        00320000
          BAL  R14,ECHOLINE           , report the command executed     00330000
          BAL  R14,PROCLINE           , execute the command             00340000
         ENDIF (LTR,R15,R15,Z)        , there was a parameter           00350000
         BAL   R14,OPNSYSIN           , OPEN the SYSIN file if present  00360000
         TITLE 'Main processing loop'                                   00370000
         IF    (LTR,R15,R15,Z)        , if open was okay                00380000
          DO FOREVER                  , Loop through all blocks         00390000
           BAL R14,GETLINE            , read one command                00400000
           BAL R14,ECHOLINE           , report the command executed     00410000
           BAL R14,PROCLINE           , process script line             00420000
          ENDDO                       , end of loop through member      00430000
         ENDIF                                                          00440000
EXIT     DS    0H                     , end of data                     00450000
         IF    (TM,PROCFLAG,HAVEPARM+HAVESYSN,Z)                        00460000
          MSGPUT MSG03W                                                 00470000
          BAL  R14,PUTMSG             , write last message              00480000
          SETMAXCC 4                  , set minimum RC                  00490000
         ENDIF                                                          00500000
         MSGPUT MSG99I                , insert message                  00510000
         L     R15,MAXCC              , get return code                 00520000
         CVD   R15,DBL                , make a number                   00530000
         UNPK  MSG99I1,DBL+5(3)       , make printable                  00540000
         OI    MSG99I1+L'MSG99I1-1,C'0'      last digit printable       00550000
         BAL   R14,PUTMSG             , write last message              00560000
         BAL   R14,CLOSYSIN           , close SYSIN if open             00570000
         BAL   R14,CLOPRINT           , close SYSPRINT if open          00580000
         L     R15,MAXCC              , get return code                 00590000
         BSPRET RC=(15)               , and return with rc in r15       00600000
         TITLE 'Subroutines: PUTMSG - Display an error message'         00610000
*********************************************************************** 00620000
* Routine to display an error message. The error message is assumed   * 00630000
* to be stored in the MSGTEXT area (and is at most 124 bytes long)    * 00640000
* The messages will be written to SYSPRINT DD if available and open.  * 00650000
* If not, the messages will be isssued via WTO                        * 00660000
*                                                                     * 00670000
* Registers on Entry: R14 = Return address                            * 00680000
*********************************************************************** 00690000
         SPACE 1                                                        00700000
PUTMSG   DS    0H                     , output message on sysprint/wto  00710000
         STM   R14,R3,PUTMSAVE        , save registers                  00720000
         IF    (TM,PROCFLAG,PRNTOPEN,O) SYSPRINT available?             00730000
          IF   (CP,LINENUM,GT,=PL2'55') end of page reached?            00740000
           MVC PRNTLINE,HEAD001       , Put in page header              00750000
           ZAP LINENUM,=P'12'         , reinit line number              00760000
           AP  PAGENUM,=P'1'          , increment page number           00770000
           MVC HEAD0011,=X'40202120'  , insert edit mask                00780000
           ED  HEAD0011,PAGENUM       , beautify page number            00790000
           PUT SYSPRINT,PRNTLINE      , write page header               00800000
           PUT SYSPRINT,HEAD0C1       , Insert LOGO1                    00810000
           PUT SYSPRINT,HEAD0C2       , Insert LOGO2                    00820000
           PUT SYSPRINT,HEAD0C3       , Insert LOGO3                    00830000
           PUT SYSPRINT,HEAD0C4       , Insert LOGO4                    00840000
           PUT SYSPRINT,HEAD0C5       , Insert LOGO5                    00850000
           PUT SYSPRINT,HEAD0C6       , Insert LOGO6                    00860000
          ENDIF                                                         00870000
          BLANK PRNTTEXT              , erase any garbage               00880000
          MVC   PRNTTEXT(L'MSGTEXT),MSGTEXT                             00890000
          PUT   SYSPRINT,PRNTLINE                                       00900000
         ELSE                         , no SYSPRINT, use WTO instead    00910000
          WTO  MF=(E,THEWTO)                                            00920000
         ENDIF                                                          00930000
         XR    R15,R15                , clear RC                        00940000
         ST    R15,PUTMSAVE+4         , set caller's RC                 00950000
         LM    R14,R3,PUTMSAVE        , restore return address          00960000
         BR    R14                    , and return                      00970000
         TITLE 'Subroutines: ALCUDUMP - Allocate SYSUDUMP if needed'    00980000
*********************************************************************** 00990000
* Of course this program never has problems, but we allocated a       * 01000000
* SYSUDUMP DD for SYSOUT=A anyways                                    * 01010000
*********************************************************************** 01020000
ALCUDUMP DS    0H                     , allocate SYSUDUMP               01030000
         STM   R14,R1,ALCUSAVE        , save registers                  01040000
         MVC   TEMPDDN,TUSDDDNM       , insert DDNAME into msg text     01050000
         MVC   SVC99WA(TUSDLEN),TUSDPTR move text units to WS           01060000
         LA    R1,SVC99WA+TUSDDDN-TUSDPTR  point to DDNAME              01070000
         ST    R1,SVC99P1             , put into TU list                01080000
         LA    R1,SVC99WA+TUSDCLS-TUSDPTR  , point to CLASS parm        01090000
         ICM   R1,B'1000',=XL1'80'    , indicate last parm              01100000
         ST    R1,SVC99P2             , put into TU list                01110000
         LA    R1,SVC99WA             , point to work area              01120000
         BAL   R14,DOSVC99            , go and do it                    01130000
         IF    (LTR,R15,R15,NZ)       , Error on DYNALLOC?              01140000
          BAL  R14,PUTMSG             , tell the user                   01150000
          SETMAXCC 4                  , indicate a problem              01160000
         ENDIF (LTR,R15,R15,NZ)       , Error on DYNALLOC               01170000
         LM    R14,R1,ALCUSAVE        , restore registers               01180000
         BR    R14                    , return to caller                01190000
*********************************************************************** 01200000
* SYSUDUMP DYNALLOC parameters                                        * 01210000
*********************************************************************** 01220000
         SPACE 1                                                        01230000
TUSDPTR  DS    0F                     , text unit pointers              01240000
         DC    A(TUSDDDN)             , address of DDNAME               01250000
         DC    X'80'                  , end of list indicator           01260000
         DC    AL3(TUSDCLS)           , address of SYSOUT CLASS info    01270000
TUSDDDN  DC    AL2(DALDDNAM)          , key for DDNAME                  01280000
         DC    AL2(1)                 , number of entries               01290000
         DC    AL2(8)                 , length of 1 entry               01300000
TUSDDDNM DC    CL8'SYSUDUMP'          , contens of entry                01310000
TUSDCLS  DC    AL2(DALSYSOU)          , key for SYSOUT                  01320000
         DC    AL2(0)                 , number of entries (0 = default  01330000
*        DC    AL2(1)                 , length of 1 entry(class         01340000
*        DC    C'*'                   , sysout class                    01350000
TUSDLEN  EQU   *-TUSDPTR                                                01360000
         TITLE 'Subroutines: ALCPRINT - Allocate SYSPRINT if needed'    01370000
*********************************************************************** 01380000
* Allocate SYSPRINT DD if not already present                         * 01390000
*********************************************************************** 01400000
         SPACE                                                          01410000
ALCPRINT DS    0H                     , allocate SYSPRINT DD            01420000
         XR    R15,R15                , init return code                01430000
         STM   R14,R1,ALCPSAVE        , save the registers              01440000
         MVC   TEMPDDN,TUSPDDNM       , insert DDNAME into msg text     01450000
         MVC   SVC99WA(TUSPLEN),TUSPPTR move text units to WS           01460000
         LA    R1,SVC99WA+TUSPDDN-TUSPPTR  point to DDNAME              01470000
         ST    R1,SVC99P1             , put into TU list                01480000
         LA    R1,SVC99WA+TUSPCLS-TUSPPTR  , point to CLASS parm        01490000
         ICM   R1,B'1000',=XL1'80'    , indicate last parm              01500000
         ST    R1,SVC99P2             , put into TU list                01510000
         LA    R1,SVC99WA             , point to work area              01520000
         BAL   R14,DOSVC99            , go and do it                    01530000
         IF    (LTR,R15,R15,NZ)       , Error on DYNALLOC?              01540000
          BAL  R14,PUTMSG             , tell the user                   01550000
          SETMAXCC 4                  , and end with RC=12              01560000
          ST   R15,ALCPSAVE+4         , set caller's RC                 01570000
         ENDIF (LTR,R15,R15,NZ)       , Error on DYNALLOC?              01580000
         LM    R14,R1,ALCPSAVE        , restore the registers           01590000
         BR    R14                    , and return to caller            01600000
         SPACE 2                                                        01610000
*********************************************************************** 01620000
* SYSPRINT DYNALLOC parameters                                        * 01630000
*********************************************************************** 01640000
         SPACE                                                          01650000
TUSPPTR  DS    0F                     , text unit pointers              01660000
         DC    A(TUSPDDN)             , address of DDNAME               01670000
         DC    X'80'                  , end of list indicator           01680000
         DC    AL3(TUSPCLS)           , address of SYSOUT CLASS info    01690000
TUSPDDN  DC    AL2(DALDDNAM)          , key for DDNAME                  01700000
         DC    AL2(1)                 , number of entries               01710000
         DC    AL2(8)                 , length od 1 entry               01720000
TUSPDDNM DC    CL8'SYSPRINT'          , contens of entry                01730000
TUSPCLS  DC    AL2(DALSYSOU)          , key for SYSOUT                  01740000
         DC    AL2(0)                 , number of entries               01750000
*        DC    AL2(1)                 , length of 1 entry               01760000
*        DC    C'*'                   , sysout class                    01770000
TUSPLEN  EQU   *-TUSPPTR                                                01780000
         TITLE 'Subroutines: OPNPRINT - Open SYSPRINT'                  01790000
*********************************************************************** 01800000
* Open SYSPRINT DD for output processing (for a job)                  * 01810000
*********************************************************************** 01820000
         SPACE                                                          01830000
OPNPRINT DS    0H                     , open SYSPRINT                   01840000
         STM   R14,R1,OPNPSAVE        , save registers                  01850000
         MVC   SYSPRINT,SYSPRDCB      , move DCB to reentrant storage   01860000
         LA    R1,SYSPRINT            , point to SYSPRINT DCB           01870000
         BAL   R14,OPENFILE           , go open the file                01880000
         IF    (LTR,R15,R15,Z)        , if successful                   01890000
          OI   PROCFLAG,PRNTOPEN      , indicate that SYSPRINT is open  01900000
         ENDIF (LTR,R15,R15,Z)        , if successful                   01910000
         LM    R14,R1,OPNPSAVE        , restore registers               01920000
         BR    R14                    , return to caller                01930000
         PUSH  PRINT                                                    01940000
         PRINT NOGEN                                                    01950000
SYSPRDCB DCB   DDNAME=SYSPRINT,       , ddname for this file           -01960000
               DSORG=PS,              , file is sequential             -01970000
               LRECL=133,             , record length                  -01980000
               BLKSIZE=1330,          , block size                     -01990000
               MACRF=(PM),            , will be opened for output      -02000000
               RECFM=FBA              , fixed block, ansi cntlchars     02010000
SYSPRLEN EQU   *-SYSPRDCB             , length of DCB                   02020000
         TITLE 'Subroutines: OPNSYSIN - Open SYSIN'                     02030000
*********************************************************************** 02040000
* Open SYSIN DD for input processing (if present)                     * 02050000
*********************************************************************** 02060000
         SPACE                                                          02070000
OPNSYSIN DS    0H                     , open SYSPRINT                   02080000
         LA    R15,8                  , initialize return code          02090000
         STM   R14,R1,OPNSSAVE        , save registers                  02100000
         DEVTYPE =CL8'SYSIN',DEVTYPE  , Check if SYSIN allocated        02110000
         IF    (LTR,R15,R15,Z)        , if yes                          02120000
          MVC  SYSIN,SYSINDCB         , move DCB to reentrant storage   02130000
          LA   R1,SYSIN               , point to SYSIN DCB              02140000
          BAL  R14,OPENFILE           , go open the file                02150000
          IF   (LTR,R15,R15,Z)        , if successful                   02160000
           LA  R15,0                  , indicate success                02170000
           ST  R15,OPNSSAVE+4         , set caller's RC                 02180000
           OI  PROCFLAG,SYSNOPEN      , indicate that SYSPRINT is open  02190000
          ENDIF (LTR,R15,R15,Z)       , open was succesful              02200000
         ENDIF (LTR,R15,R15,Z)        , Test if SYSIN allocated         02210000
         LM    R14,R1,OPNSSAVE        , restore registers               02220000
         BR    R14                    , return to caller                02230000
         PUSH  PRINT                                                    02240000
         PRINT NOGEN                                                    02250000
SYSINDCB DCB   DDNAME=SYSIN,          , ddname for this file           -02260000
               DSORG=PS,              , file is sequential             -02270000
               LRECL=80,              , record length                  -02280000
               BLKSIZE=80,            , block size                     -02290000
               MACRF=(GM),            , will be opened for output      -02300000
               RECFM=F,               , fixed block                    -02310000
               EODAD=EXIT             , end of file address             02320000
SYSINLEN EQU   *-SYSINDCB             , length of DCB                   02330000
         TITLE 'Subroutines: DOSVC99 - Perform DYNALLOC functions'      02340000
*********************************************************************** 02350000
* Allocate a file to the current JOB/STC using SVC99.                 * 02360000
*                                                                     * 02370000
* Registers on entry:  R1 --->  Text Unit parameter list              * 02380000
*      field TEMPDDN:  DDNAME to be allocated                         * 02390000
*                                                                     * 02400000
* Registers on exit:   R15 = Retrun code                              * 02410000
*                        0 : Allocation successful                    * 02420000
*                  nonzero : RC from DYNALLOC call                    * 02430000
*      field MSGTEXT: error message text                              * 02440000
*********************************************************************** 02450000
DOSVC99  DS    0H                     , perform DYNALLOC functions      02460000
         STM   R14,R3,DOSVSAVE        , save registers                  02470000
         LR    R3,R1                  , point to Text Unit Pointerlist  02480000
         DEVTYPE TEMPDDN,DEVTYPE      , Test for DD card                02490000
         IF    (LTR,R15,R15,NZ)       , if no DD card                   02500000
          LA   R2,REQBLK              , point to request block          02510000
          USING S99RB,R2              , tell assembler                  02520000
          XC   REQBLK,REQBLK          , clear RB                        02530000
          MVI  S99RBLN,REQBLKLN       , set up length                   02540000
          MVI  S99VERB,S99VRBAL       , indicate ALLOC function         02550000
          ST   R3,S99TXTPP            , put into Request block          02560000
          LA   R3,RBPTR               , Point to RB pointer             02570000
          USING S99RBP,R3             , tell assembler                  02580000
          ST   R2,S99RBPTR            , st RB address into RB PTR       02590000
          OI   S99RBPTR,S99RBPND      , turn on high order bit          02600000
          LA   R1,RBPTR               , get addres of RB pointer        02610000
          DYNALLOC                    , issue SVC 99                    02620000
          ST   R15,DOSVSAVE+4         , set caller's R15                02630000
          XR   R15,R15                , clear R15                       02640000
          ICM  R15,B'0011',S99ERROR   , get error code                  02650000
          ST   R15,DOSVSAVE+8         , place into caller's R0          02660000
          MSGPUT MSG01E               , insert message text             02670000
          MVC  MSG01E1,TEMPDDN        , insert DDNAME into message      02680000
          X2CHRTRN MSG01E2,DOSVSAVE+06,LEN=2                            02690000
          X2CHRTRN MSG01E3,DOSVSAVE+10,LEN=2                            02700000
         ELSE                                                           02710000
          ST   R15,DOSVSAVE+4         , set caller's R15                02720000
          MSGPUT MSG20I               , set up message text             02730000
         ENDIF                                                          02740000
         LM    R14,R3,DOSVSAVE        , restore return address          02750000
         BR    R14                    , and retrun to caller            02760000
         DROP  R2,R3                  , not needed any more             02770000
         TITLE 'Subroutines: SETINIT  - Initialize some variables'      02780000
*********************************************************************** 02790000
* Initialze some variables, set processing options according to       * 02800000
* runtime environmaent (STC or BATCH)                                 * 02810000
*********************************************************************** 02820000
         SPACE 1                                                        02830000
SETINIT  DS    0H                     , Initialize some variables       02840000
         STM   R14,R1,SETISAVE        , save registers                  02850000
         XC    MEMCC,MEMCC            , return code field               02860000
         XC    MAXCC,MAXCC            , return code field               02870000
         XC    LASTCC,LASTCC          , return code field               02880000
         MVI   PROCFLAG,X'0'          , clear parm flag                 02890000
         ZAP   LINENUM,=P'100'        , force page break                02900000
         ZAP   PAGENUM,=P'0'          , init page number                02910000
         TESTENV                      , STC? BATCH? TSO?                02920000
         IF    (CH,R1,EQ,=H'0')       , is this a batch job             02930000
          OI   PROCFLAG,ISJOB         , remember in process option flag 02940000
         ELSEIF (CH,R1,EQ,=H'4')      , is this an STC                  02950000
          OI   PROCFLAG,ISSTC         , put indicator to flag           02960000
         ELSE                         , it is not supported             02970000
          BLANK MSGTEXT                                                 02980000
          MSGPUT MSG14E               ,  insert message text            02990000
          BAL  R14,PUTMSG             , issue message                   03000000
          SETMAXCC 12                 , get out with RC=12              03010000
          B    EXIT                                                     03020000
         ENDIF                                                          03030000
         L     R1,CVTPTR     get CVT address                    JW12109 03040000
         ICM   R1,B'1111',CVTSAF(R1) SAFV defined ?             JW12109 03040700
         BZ    GOAHEAD       no RAC, go ahead                   JW12109 03041400
         USING SAFV,R1       addressability of SAFV             JW12109 03042100
         CLC   SAFVIDEN(4),SAFVID SAFV initialized ?            JW12109 03042800
         BNE   GOAHEAD       no RAC, go ahead                   JW12109 03043500
         DROP  R1            SAFV no longer needed              JW12109 03044200
         RACHECK ENTITY=DIAG8CMD,CLASS='FACILITY',ATTR=READ     JW12109 03044900
         LTR   R15,R15       RAC authorization granted?         JW12109 03045600
         BZ    GOAHEAD       yes, go ahead                      JW12109 03046300
         SETMAXCC 12         get out with RC=12                 JW12109 03047000
         B     EXIT                                             JW12109 03047700
GOAHEAD  LM    R14,R1,SETISAVE        , restore all register    JW12109 03048400
         BR    R14                    , and return to caller            03050000
         TITLE 'Subroutines: GETLINE - Read a line from the SCRIPT'     03060000
*********************************************************************** 03070000
*  This routine will read a record from the SYSIN file and place it   * 03080000
*  into PARMAREA                                                      * 03090000
*********************************************************************** 03100000
         SPACE 1                                                        03110000
GETLINE  DS    0H                     , Build SCRPTLIN                  03120000
         STM   R14,R1,GETLSAVE        , save the registers              03130000
         BLANK PARMAREA               , clear receiving buffer          03140000
         GET   SYSIN,PARMAREA         , read next record                03150000
         OI    PROCFLAG,HAVESYSN      , indicate at least 1 rec read    03160000
         LM    R14,R1,GETLSAVE        , restore the registers           03170000
         BR    R14                    , back to caller                  03180000
         TITLE 'Subroutines: ECHOLINE - Echo the command line'          03190000
*********************************************************************** 03200000
*  This routine will echo the command line to the console unless the  * 03210000
*  NOECHO parameter is set or the line is in a non-executed           * 03220000
*  conditional block                                                  * 03230000
*                                                                     * 03240000
*  Registers on ENTRY :  R14 = return address                         * 03250000
*                                                                     * 03260000
*  Registers on exit  :  unchanged                                    * 03270000
*********************************************************************** 03280000
         SPACE 1                                                        03290000
ECHOLINE DS    0H                     , process the PARM statement      03300000
         STM   R14,R1,ECHOSAVE        , save registers                  03310000
         MSGPUT MSG91I                , load message body               03320000
         MVC   MSG91I1,PARMAREA       , insert command text             03330000
         BAL   R14,PUTMSG             , send message                    03340000
         LM    R14,R1,ECHOSAVE        , restore rgisters                03350000
         BR    R14                    , return to caller                03360000
         TITLE 'Subroutines: PROCLINE - Run the command via DIAG 8'     03370000
*********************************************************************** 03380000
*  This routine will run the command which is in PARMAREA             * 03390000
*********************************************************************** 03400000
         SPACE 1                                                        03410000
PROCLINE DS    0H                     , process the PARM statement      03420000
         STM   R14,R1,PROCSAVE        , save registers                  03430000
         MODESET MODE=SUP             , back to normal                  03440000
         LRA   R14,PARMAREA           , point to command                03450000
         LA    R15,L'PARMAREA         , get command length              03460000
         DIAG  R14,R15,8              , Issue the command               03470000
         MODESET MODE=PROB            , back to normal                  03480000
         LM    R14,R1,PROCSAVE        , restore rgisters                03490000
         BR    R14                    , return to caller                03500000
         TITLE 'Subroutines: GETPARAM - Get JCL Parameter'              03510000
*********************************************************************** 03520000
* Registers on entry:                                                 * 03530000
*       R1  = address of parameter list                               * 03540000
*       R14 = Return address                                          * 03550000
* Registers on exit:                                                  * 03560000
*       R15 = Return code                                             * 03570000
*         0 : Parameter moved to PARMAREA                             * 03580000
*         4 : No parameter passed via JCL                             * 03590000
*       All other registers are restored                              * 03600000
*       PARMAREA: Text of JCL Parm                                    * 03610000
*********************************************************************** 03620000
         SPACE 1                                                        03630000
GETPARAM DS    0H                     , Get JCL Parameter               03640000
         STM   R14,R1,GETPSAVE        , save all registers              03650000
         BLANK PARMAREA               , clear parm text area            03660000
         L     R1,0(0,R1)             , Address of passed parm          03670000
         LH    R15,0(0,R1)            , R15 = Length of parameter       03680000
         LA    R1,2(0,R1)             , R1  = Address of Parameter      03690000
         IF    (LTR,R15,R15,NZ)       , If a parm is given              03700000
          OI   PROCFLAG,HAVEPARM      , indicate we have a PARM         03710000
          BCTR R15,0                  , decrement for EX                03720000
          MVC  PARMAREA(*-*),0(R1)    , move in parameters              03730000
          EX   R15,*-6                , via EX                          03740000
          XR   R15,R15                , Indicate PARM read              03750000
         ELSE                         , no parm given                   03760000
          LA   R15,4                  , Indicate in RC                  03770000
         ENDIF                        , test if parm given              03780000
         ST    R15,GETPSAVE+4         , set caller's RC                 03790000
         LM    R14,R1,GETPSAVE        , restore all registers           03800000
         BR    R14                    , and return to caller            03810000
         TITLE 'Subroutine OPENFILE - Open files as needed'             03820000
*********************************************************************** 03830000
* Open a file, report any errors if open fails                        * 03840000
*                                                                     * 03850000
* Registers on entry:  R1  = address of DCB to be opened              * 03860000
*                      R14 = Return address                           * 03870000
*                                                                     * 03880000
* Registers on exit:   R15 = Returncode                               * 03890000
*                        0 ==> OPEN successful                        * 03900000
*                        8 ==> OPEN failed                            * 03910000
*********************************************************************** 03920000
         SPACE 1                                                        03930000
OPENFILE DS    0H                     , Open routine                    03940000
         STM   14,3,OPENSAVE          , save registers                  03950000
         LR    R3,R1                  , Point to DCB                    03960000
         USING IHADCB,R3              , tell assembler                  03970000
         MVC   TEMPDDN,DCBDDNAM       , insert DD name into msg         03980000
         MVC   OCLIST,THELIST         , Set up open/close list          03990000
         IF    (CLC,DCBDDNAM,EQ,TUSPDDNM)         SYSPRINT?             04000000
          OPEN ((R3),OUTPUT),MF=(E,OCLIST)                              04010000
         ELSE                                                           04020000
          OPEN ((R3),INPUT),MF=(E,OCLIST)                               04030000
         ENDIF                                                          04040000
         IF    (TM,DCBOFLGS,DCBOFOPN,O) if open was successfull         04050000
          LA   R15,0                  , clear return code               04060000
         ELSE                         , when open failed                04070000
          MSGPUT MSG02E               , insert message body             04080000
          MVC MSG02E1,TEMPDDN         , insert DDname into message      04090000
          BAL R14,PUTMSG              , issue the message               04100000
          LA  R15,8                   , load error RC                   04110000
         ENDIF (TM,DCBOFLGS,DCBOFOPN,O) if open was okay                04120000
         ST    R15,OPENSAVE+4         , set caller's R15                04130000
         LM    14,3,OPENSAVE          , restore registers               04140000
         BR    R14                    , and return to caller            04150000
         DROP  R3                 , not needed outside this module      04160000
         TITLE 'Subroutines: CLOPRINT - Close SYSPRINT if open'         04170000
*********************************************************************** 04180000
* Close SYSPRINT if open                                              * 04190000
*********************************************************************** 04200000
         SPACE 1                                                        04210000
CLOPRINT DS    0H                     , Close SYSPRINT if open          04220000
         IF    (TM,PROCFLAG,PRNTOPEN,O) close PARMDIR if open           04230000
          STM  R14,R1,CLOPSAVE        , save registers                  04240000
          LA   R3,SYSPRINT            , point to DCB                    04250000
          MVC   OCLIST,THELIST                                          04260000
          CLOSE ((R3)),MF=(E,OCLIST)  , and close it                    04270000
          NI   PROCFLAG,255-PRNTOPEN                                    04280000
          LM   R14,R1,CLOPSAVE        , restore registers               04290000
         ENDIF (TM,PROCFLAG,PRNTOPEN,O) close PARMDIR if open           04300000
         BR    R14                    , return to caller                04310000
         TITLE 'Subroutines: CLOSYSIN - Close SYSIN if open'            04320000
*********************************************************************** 04330000
* Close SYSIN if open                                                 * 04340000
*********************************************************************** 04350000
         SPACE 1                                                        04360000
CLOSYSIN DS    0H                     , Close SYSPRINT if open          04370000
         IF    (TM,PROCFLAG,SYSNOPEN,O) Test if SYSIN is open           04380000
          STM  R14,R1,CLOSSAVE        , save registers                  04390000
          LA   R3,SYSIN               , point to DCB                    04400000
          MVC   OCLIST,THELIST                                          04410000
          CLOSE ((R3)),MF=(E,OCLIST)  , and close it                    04420000
          NI   PROCFLAG,255-SYSNOPEN                                    04430000
          LM   R14,R1,CLOSSAVE        , restore registers               04440000
         ENDIF (TM,PROCFLAG,SYSNOPEN,O) close PARMDIR if open           04450000
         BR    R14                    , return to caller                04460000
         TITLE 'Constants'                                    '         04470000
THELIST  OPEN  (,),MF=L               , Open close RDJFCB list          04480000
         LTORG                                                          04490000
         TITLE 'Reentrant Storage'                                      04500000
WORKAREA DSECT                                                          04510000
*********************************************************************** 04520000
* The description of a few variables has been placed here because     * 04530000
* IFOX00 doesn't easily allow certain forward references in the EQU   * 04540000
* statement.                                                          * 04550000
*********************************************************************** 04560000
THEWTO   WTO   '----+----1----+----2----+----3----+----4----+----5----+-04570000
               ----6----+----7----+----8----+----9----+----0----+----1--04580000
               ---+----2----',MF=L                                      04590000
MSGTEXT  EQU   THEWTO+4,124           , area for message texts          04600000
PRNTLINE DS    0CL133                 , line to SYSPRINT                04610000
PRNTCC   DS    CL1                    , control character               04620000
PRNTTEXT DS    CL132                  , text to be printed              04630000
BSPHRCMD CSECT                                                          04640000
         TITLE 'Constants - Report Header Lines'                        04650000
**********************************************************************  04660000
*  BSPHRCMD Version 1.0                                                 04670000
*                                                                       04680000
*          ¦l      _,,,---,,_                                           04690000
*    ZZZzz /,:.-':''  . -.  ;-;;,                                       04700000
*         ¦,4-  ) )-,_. ,( (  :'-                                       04710000
*        '---''(_/--'  :-')_)                                           04720000
*                                                                       04730000
*  Placed into the Hercules Domain                                      04740000
*  by Volker Bandke, BSP GmbH'                                          04750000
*                                                                       04760000
**********************************************************************  04770000
         SPACE 1                                                        04780000
HEAD001  DS    0CL(133)                                                 04790000
         DC    C'1BSPHRCMD Version &BSPVER..&BSPMOD..2'                 04800000
         FILL  HEAD001                ,                                 04810000
         ORG   HEAD001+133-8                                            04820000
         DC    C'PAGE '                                                 04830000
HEAD0011 EQU   PRNTLINE+133-4,4                                         04840000
         ORG   ,                      ,                                 04850000
HEAD0C1  DS    0CL(133)                                                 04860000
         DC    CL60'0'                                                  04870000
         DC    C'         ¦l      _,,,---,,_'                           04880000
         FILL  HEAD0C1                                                  04890000
HEAD0C2  DS    0CL(133)                                                 04900000
         DC    CL60' '                                                  04910000
         DC    C'   ZZZzz /,:.-'':''    -.  ;-;;,'                      04920000
         FILL  HEAD0C2                                                  04930000
HEAD0C3  DS    0CL(133)                                                 04940000
         DC    CL60' '                                                  04950000
         DC    C'        ¦,4-  ) )-,_. ,( (  :''-'''                    04960000
         FILL  HEAD0C3                                                  04970000
HEAD0C4  DS    0CL(133)                                                 04980000
         DC    CL60' '                                                  04990000
         DC    C'       ''---''''(_/--''  :-'')_)'                      05000000
         FILL  HEAD0C4                                                  05010000
HEAD0C5  DS    0CL(133)                                                 05020000
         DC    CL60'0'                                                  05030000
         DC    C' Placed into the Hercules Domain'                      05040000
         FILL  HEAD0C5                                                  05050000
HEAD0C6  DS    0CL(133)                                                 05060000
         DC    CL60' '                                                  05070000
         DC    C' by Volker Bandke, BSP GmbH'                           05080000
         FILL  HEAD0C6                                                  05090000
         DS    0CL(133)                                                 05100000
         TITLE 'Constants - Error and status messages'                  05110000
**********************************************************************  05120000
*   BSPHC01E - xxxxxxxx DD statement not allocated                      05130000
*   BSPHC02E - Open failed for DD xxxxxxxx                              05140000
*   BSPHC03W - No commands in PARM or SYSIN                             05150000
*   BSPHC14E - Environment not BATCH or STC                             05160000
*   BSPHC91I - Processing command: ccccccccc                            05170000
*   BSPHC99I - Function terminated, highest RC=xxxx                     05180000
*********************************************************************** 05190000
MSG01E   DC    C'BSPHC01E - Allocation failed for XXXXXXXX, RC=XXXX, S9+05200000
               9ERROR=XXXX'                                             05210000
MSG01E1  EQU   MSGTEXT+33,8                                             05220000
MSG01E2  EQU   MSGTEXT+46,4                                             05230000
MSG01E3  EQU   MSGTEXT+61,4                                             05240000
MSG02E   DC    C'BSPHC02E - Open failed for DD statement XXXXXXXX'      05250000
MSG02E1  EQU   MSGTEXT+40,8                                             05260000
MSG03W   DC    C'BSPHC03W - No commands in PARM or SYSIN'               05270000
MSG14E   DC    C'BSPHC14E - Environment not BATCH or STC'               05280000
MSG20I   DC    C'BSPHC20I - DD already allocated, no allocation done'   05290000
MSG91I   DC    C'BSPHC91I - Processing command: ----+----1----+----2----05300000
               -+----3----+----4----+----5----+----6----+----7----+----805310000
               8----+----9----+----0'                                   05320000
MSG91I1  EQU   MSGTEXT+31,100                                           05330000
MSG99I   DC    C'BSPHC99I - End of processing, MAXRC=xxxx'              05340000
MSG99I1  EQU   MSGTEXT+36,4                                             05350000
DIAG8CMD DC    CL39'DIAG8CMD' facility name to authorize        JW12109 05353000
SAFVID   DC    CL4'SAFV'     SAFV eye catcher                   JW12109 05356000
         TITLE 'Reentrant Storage'                                      05360000
WORKAREA DSECT                        , reentrant storage               05370000
DBL      DS    D                      , Double word for CVB             05380000
ALCPSAVE DS    4F                     , ALCPDUMP Save area R14 - R1     05390000
ALCUSAVE DS    4F                     , ALCUDUMP Save area R14 - R1     05400000
CLOPSAVE DS    4F                     , CLOPRINT Save area R14 - R1     05410000
CLOSSAVE DS    4F                     , CLOSYSIN Save area R14 - R1     05420000
DOSVSAVE DS    6F                     , DOSVC99  Save area R14 - R3     05430000
DOWASAVE DS    8F                     , DOWAIT   Save area R14 - R5     05440000
ECHOSAVE DS    4F                     , ECHOLINE Save area R14 - R1     05450000
GETPSAVE DS    4F                     , GETPARAM Save area R14 - R1     05460000
GETLSAVE DS    4F                     , GETLINE  Save area R14 - R1     05470000
OPENSAVE DS    6F                     , OPENFILE Save area R14 - R3     05480000
OPNPSAVE DS    4F                     , OPNPRINT Save area R14 - R1     05490000
OPNSSAVE DS    4F                     , OPNSYSIN Save area R14 - R1     05500000
PUTMSAVE DS    6F                     , PUTMSG   Save area R14 - R3     05510000
PROCSAVE DS    4F                     , PROCLINE Save area R14 - R1     05520000
SETISAVE DS    4F                     , SETINIT  Save area R14 - R1     05530000
TEMPDDN  DS    CL8                    , for DDNAME                      05540000
MAXCC    DS    F                      , returncode given to caller      05550000
MEMCC    DS    F                      , current RC                      05560000
LASTCC   DS    F                      , current RC                      05570000
DEVTYPE  DS    6F                     , for devtype macro               05580000
         DS    0D                                                       05590000
SYSPRINT DS    CL(SYSPRLEN)           , reentrant DCB for SYSPRINT      05600000
         DS    0D                                                       05610000
SYSIN    DS    CL(SYSINLEN)           , reentrant DCB for SNAPDUMP      05620000
         DS    0D                                                       05630000
PARMAREA DS    CL100                  , Area for commands               05640000
RBPTR    DS    F                      , request block pointer           05650000
REQBLK   DS    CL(S99RBEND-S99RB)     , Request block                   05660000
REQBLKLN EQU   L'REQBLK               , length of request block         05670000
SVC99WA  DS    CL100                  , parameter area for SVC99        05680000
SVC99P1  EQU   SVC99WA+0,4            , SVC 99 parameter 1              05690000
SVC99P2  EQU   SVC99WA+4,4            , SVC 99 parameter 2              05700000
SVC99P3  EQU   SVC99WA+8,4            , SVC 99 parameter 3              05710000
OCLIST   OPEN  (,),MF=L                                                 05720000
PROCFLAG DS    XL1                    , Processing control flag         05730000
ISTSO    EQU   B'10000000'            , running as a TSO user           05740000
ISJOB    EQU   B'01000000'            , running as a batch job          05750000
ISSTC    EQU   B'00100000'            , running as a started task       05760000
PRNTOPEN EQU   B'00010000'            , SYSPRINT is open                05770000
SYSNOPEN EQU   B'00001000'            , SYSIN is open                   05780000
HAVEPARM EQU   B'00000100'            , We have a JCL Parm              05790000
HAVESYSN EQU   B'00000010'            , We have SYSIN line(s)           05800000
LINENUM  DS    PL2                                                      05810000
PAGENUM  DS    PL2                                                      05820000
         BSPEND                       , of module                       05830000
@@
//SYSUT2   DD  DISP=(,PASS),UNIT=VIO,
//             SPACE=(CYL,(1,1)),DCB=(LRECL=80,RECFM=FB,BLKSIZE=3120)
//SYSIN    DD  DUMMY
//SYSPRINT DD  SYSOUT=* 
//ASM     EXEC PGM=IFOX00,PARM='DECK,NOOBJECT,TERM,NOXREF'
//********************************************************************
//* You might have to change the DSNAMES in the next 2 DD statements *
//********************************************************************
//SYSIN    DD  DISP=(OLD,DELETE),DSN=*.RACUPDT.SYSUT2
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
//             PARM='LIST,LET,MAP,RENT,REUS,REFR,AC=1'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(TRK,(50,20))
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.CMDLIB
//SYSLIN   DD  DISP=(OLD,DELETE),DSN=*.ASM.SYSPUNCH
//          DD *
 ALIAS BSPOSCMD
 NAME BSPHRCMD(R)
//*
//* Add the RAKF permissions
//*
//EXEC     EXEC PGM=IKJEFT01,                  
//       REGION=8192K                                         
//TSOLIB   DD   DSN=BREXX.CURRENT.RXLIB,DISP=SHR                             
//RXLIB    DD   DSN=BREXX.CURRENT.RXLIB,DISP=SHR                             
//SYSEXEC  DD   DSN=SYS2.EXEC,DISP=SHR                         
//SYSPRINT DD   SYSOUT=*                                      
//SYSTSPRT DD   SYSOUT=*                                      
//SYSTSIN  DD   *
 RX RDEFINE 'FACILITY DIAG8CMD UACC(NONE)'
 RX PERMIT 'DIAG8CMD CLASS(FACILITY) ID(ADMIN) ACCESS(READ)'
//STDOUT   DD   SYSOUT=*,DCB=(RECFM=FB,LRECL=140,BLKSIZE=5600)
//STDERR   DD   SYSOUT=*,DCB=(RECFM=FB,LRECL=140,BLKSIZE=5600)
//STDIN    DD   DUMMY   
