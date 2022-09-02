//BSPAPFCK JOB (JOB),
//             'INSTALL BSPAPFCK',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//ASM     EXEC PGM=IFOX00,PARM='DECK,NOOBJECT,TERM,NOXREF'
//SYSIN    DD  DATA,DLM=@@
BSPAPFCK TITLE 'Check IEAAPFxx members for Plausibility'
***********************************************************************
*                                                                     *
* This program scans PARMLIB for members with the name IEAAPFxx.      *
* For each line in each member the following checks are made:         *
*                                                                     *
*  a1) Is the dataset cataloged?                                      *
*  a2) If yes, is the dataset cataloged on the named volume?          *
*  c)  Is the named volume available?                                 *
*  d)  Is the dataset physically located on the named volume?         *
*                                                                     *
*  Required DD names:                                                 *
*                                                                     *
*  PARMLIB    points to PDS with IEAAPFxx members to be checked       *
*  SYSPRINT   output queue for report                                 *
*                                                                     *
*  Assembly and Link JCL can be found in CBT.MVS38J.CNTL(APFCK#)      *
*  Execution JCL can be found in         CBT.MVS38J.CNTL(APFCK$)      *
*                                                                     *
*  This program has been tested on the Turnkey MVS system Verison 3   *
*  It is _not_ guaranteed to be bug-free (which program is?)          *
*  Use at your own risk                                               *
*                                                                     *
***********************************************************************
         EJECT                                                        *
* +-------------------------------------------------------------------+
* |                                                                   |
* |                   |l      _,,,---,,_                              |
* |            ZZZZZ /,:.-':'    -.  ;-;;,                            |
* |                 |,4-  ) )-,_. ,( (  :'-'                          |
* |                '---''(_/--'  :-')_)                               |
* |                                                                   |
* | 1. Anyone can make a decision given enough facts                  |
* | 2. A good manager can make a decision without enough facts        |
* | 3. A perfect manager can operate in perfect ignorance             |
* |                                                                   |
* |          (Another wisdom from the Fortune Cookie Jar)             |
* |                                                                   |
* +-------------------------------------------------------------------+
         PRINT OFF,NOGEN
         COPY  BSPGLBLS
         COPY  BSPSGLBL
         GBLC  &LRECL
         GBLC  &BLKSIZE
&LRECL   SETC  '133'                  , record length for sysprint
&BLKSIZE SETC  '&LRECL.0'             , blocksize for sysprint
         PRINT ON,NOGEN
BSPAPFCK BSPENTER RENT=NO,BASE=(R11,R12)
         TITLE 'Symbols: Register Equates'
PARMREG  EQU   R1                     , parameter register
DCBREG   EQU   R3                     , pointer to dcb
DIRPTR   EQU   R4                     , pointer to directory entry
LINKREG  EQU   R5                     , reg used for bal
BLOCKPTR EQU   R6                     , pointer to io block area
RECOFFST EQU   R7                     , record pointer within block
RECPTR   EQU   R8                     , address of current record
RETCODE  EQU   R15                    , return code register
OPEN01   DS    0H                     , try to open SYSPRINT
         LA    DCBREG,SYSPRINT        , point to sysprint dcb
         BAL   LINKREG,OPENFILE       , test allocation and open file
         LTR   RETCODE,RETCODE        , was open successful?
         BZ    OPEN02                 , if so, open next
         MVC   LASTCC,=F'12'          , indicate error
         MVC   MAXCC,=F'12'           , indicate error
         B     GOHOME                 , and terminate
OPEN02   DS    0H                     , try to open PARMLIB
         LA    DCBREG,PARMLIB         , point to sysprint dcb
         BAL   LINKREG,OPENFILE       , test allocation and open file
         LTR   RETCODE,RETCODE        , was open successful?
         BNZ   CLOSE010               , bif not
OPEN03   DS    0H                     , try to open PARMLIB directory
         LA    DCBREG,PARMDIR         , point to sysprint dcb
         BAL   LINKREG,OPENFILE       , test allocation and open file
         LTR   RETCODE,RETCODE        , was open successful?
         BZ    OPEN04                 , if so, open next
         CLOSE PARMLIB                , close open files
         B     CLOSE010               , and terminate with error code
OPEN04   DS    0H                     , get JFCB for PARMLIB
         LA    DCBREG,PARMLIB         , point to parmlib dd
         RDJFCB ((DCBREG))            , get jfcb info
         LTR   RETCODE,RETCODE        , was jfcb okay
         BZ    INIT0100               , bif yes
         CLOSE PARMDIR                , close open file
         CLOSE PARMLIB                . close open file
         B     CLOSE010               , and terminate with error code
INIT0100 DS    0H                     ,
         MVI   FLAG,X'0'              , init flag byte
         MVC   MAXCC,=F'0'            , init mac'ximum rc
         MVC   LASTCC,=F'0'           , init lastcc
         MVC   HEAD002A,JFCBDSNM      , insert dsname
         MVC   HEAD002B,JFCBVOLS      , and volume
         LH    R0,JFCBLKSI            , block length
         GETMAIN R,LV=(0)             , get area
         LR    BLOCKPTR,R1            , save address of block area
DIR0100  DS    0H                     , read directory block
         READ  DIRDECB,SF,PARMDIR,DIRAREA,'S'
         CHECK DIRDECB                , wait for i/o completion
         LA    DIRPTR,DIRAREA+10      , point to area
         USING PDSDIRDS,DIRPTR        , tell assembler
         CLC   =CL6'IEAAPF',PDSKEY    , block reached already
         BH    DIR0100                , then try next block
DIR0200  DS    0H                     , read directory record
         CLC   PDSMEMBR,=XL8'FFFFFFFFFFFFFFFF' EOD?
         BE    TERM9999               , term at last member
         CLC   =CL6'IEAAPG',PDSMEMBR  , terminate, if we are
         BNH   TERM9999               , past IEAAPFxx
         CLC   =CL6'IEAAPF',PDSMEMBR  , IEAAPFxx member ?
         BE    PROCAPF                , Go process it
         B     EOMEM1                 ,
EOMEM    DS    0H                     , end of member reached
         CLC   LASTCC,=F'0'           , any errors?
         BNE   EOMEM1                 , bif yes
         LA    PARMREG,BSPAC14I       , point to allok msg
         BAL   LINKREG,PUTMSG         , and show it
EOMEM1   DS    0H                     ,
         MVC   LASTCC,=F'0'           , Set OK code
         ZAP   LINENUM,=P'100'        , reinit to force ff
         CLC   PDSMEMBR,PDSKEY        , last member in block
         BE    DIR0100                , then get next directory block
         XR    R1,R1                  , clear workreg
         NI    PDSC,X'1F'             , mask out high order 3 bits
         ICM   R1,B'0001',PDSC        , number of data hwords
         SLL   R1,1                   , number of bytes
         LA    DIRPTR,12(DIRPTR)      , point to userdata
         AR    DIRPTR,R1              , point to next entry
         B     DIR0200                , and process entry
         BOX   'Process the IEAAPFxx member'
PROCAPF  DS    0H                     , process apf member
         MVC   HEAD003A,PDSMEMBR      , insert membername to header
         FIND  PARMLIB,PDSMEMBR,D     , look for member in parmlib
READBLCK DS    0H                     , read one block from member
         READ  READDECB,SF,PARMLIB,(BLOCKPTR)
         CHECK READDECB               , wait for io completion
         LH    R0,JFCBLKSI            , get ds blocksize
         L     R1,READDECB+16         , point to iob
         SH    R0,14(R1)              , subtract residual block length
         STH   R0,BLOCKLEN            , save actual block length read
         XR    RECOFFST,RECOFFST      , clear offset register
READREC  DS    0H                     , read a record
         LA    RECPTR,0(RECOFFST,BLOCKPTR) get record at spec. offset
         MVC   BSPAC081,0(RECPTR)     , insert card image
         LR    R1,RECPTR              , point to beginning of record
         XR    R15,R15                , clear counter register
         BOX   'Search for first nonblank character in line'
READ0050 DS    0H                     , Scan for first
         CLI   0(R1),C' '             , nonblank character
         BNE   READ0100               , first nonblank found
         LA    R1,1(R1)               , next byte in card
         LA    R15,1(R15)             , increment counter
         B     READ0050               , go araund again
READ0100 DS    0H                     ,
         CLC   =CL2'/*',0(R1)         , is this a comment?
         BE    NEXTCARD               , then get next record
         BOX   'Search for first blank following DSNAME'
         XC    TRTTAB,TRTTAB          , clear trttab
         MVI   TRTTAB+C' ',X'FF'      , search argument
         LA    R14,72                 , maximum line length
         SR    R14,R15                , length of leading blanks
         BCTR  R14,0                  , minus one for execute
         LR    R0,R1                  , save start of dsname
         EX    R14,TRT                , r1 ===> blank after dsname
         LR    R14,R1                 , save end of dsname
         SR    R14,R0                 , length of dsname
         BCTR  R14,0                  , minus 1 for ex
         LR    R15,R0                 , start of dsname
         BLANK DSNAME                 , clear DSNAME field
         EX    R14,MOVEDSN            , insert DSNAME from IEABSPAC
         BOX   'Search for next nonblank character (=VOLSER start)'
         LR    R15,R1                 , start of blanks
         SR    R15,RECPTR             , offset of blanks
         LA    R14,72                 , line length
         SR    R14,R15                , length of rest of line
         BCTR  R14,0                  , decrement for execute
         BLANK TRTTAB,CHAR=X'FF'      , set everything to X'FF'
         MVI   TRTTAB+C' ',X'00'      , insert zero
         EX    R14,TRT                , r1 ===> volume name
         BOX   'Search for next '' '' or '','' (=VOLSER end)'
         LR    R0,R1                  , save address
         SR    R0,RECPTR              , r0=offset within record
         LA    R14,72                 , length of record
         SR    R14,R0                 , length of rest
         BCTR  R14,0                  , minus one for decrement
         XC    TRTTAB,TRTTAB          , clear table
         MVI   TRTTAB+C' ',X'FF'      , insert search arg
         MVI   TRTTAB+C',',X'FF'      , into trt table
         LR    R0,R1                  , r0 points to volume
         EX    R14,TRT                , r1 ===> past volume
         SR    R1,R0                  , length of string
         BCTR  R1,0                   , minus 1 for execute
         LR    R14,R0                 , start of volume string
         BLANK VOLUME                 , clear volume area
         EX    R1,MOVEVOL             , insert volume into slot
         BOX   'Check if DSNAME is cataloged'
         LOCATE CATLIST               , reuse trt table
         LTR   RETCODE,RETCODE        , did we find entry
         BNZ   CAT0900                , indicate error
         BOX   'Check if catalog points to our Volume'
         CLC   VOLUME,CAMAREA+6       , look at volume
         BE    CHCKVTOC               , if okay, check vtoc             oc
         OI    FLAG,CATDIFF           , indicate differnce
         MVC   BSPAC091,CAMAREA+6     , insert volume into message
         MVC   LASTCC,=F'4'           , indicate error rc
         CLC   MAXCC,LASTCC           , are we higher than max
         BH    CHCKVTOC               , bif not
         MVC   MAXCC,LASTCC           , else update maxcc
         B     CHCKVTOC               , process next card
CAT0900  DS    0H                     , error in camlst locate
         OI    FLAG,NOTCAT            , indicate error
         LA    R14,CATRCTB            , point to table
         L     R1,0(R15,R14)          , load message address
         MVC   BSPAC101,0(R1)         , insert message text
         MVC   LASTCC,=F'4'           , insert return code
         CLC   MAXCC,LASTCC           , are we higher than max
         BH    CHCKVTOC               , bif not
         MVC   MAXCC,LASTCC           , else update
         BOX   'Test if named dataset is on our volume'
CHCKVTOC DS    0H                     ,
         OBTAIN VTOCLIST              , test if dsn on volume
         LTR   RETCODE,RETCODE        , Okay?
         BZ    NEXTCARD               , Then go to next record
         BOX   'Indicate dataset is not on volume'
VTOC0900 DS    0H                     ,
         OI    FLAG,NOTVTOC           , indicate not found
         LA    R14,VTOCRCTB           , point to table
         L     R1,0(R15,R14)          , load message address
         MVC   BSPAC121,0(R1)         , insert message text
         MVC   LASTCC,=F'12'          , set lastCC
         CLC   MAXCC,LASTCC           , and
         BH    NEXTCARD               , maxcc
         MVC   MAXCC,LASTCC           , before processing next record
NEXTCARD DS    0H                     ,
         CLI   FLAG,X'0'              , any flags set
         BE    NEXTC100               , bif no error
         LA    R1,BSPAC08I            , else display card image
         BAL   LINKREG,PUTMSG         , onto sysprint
         LA    R1,BSPAC13E            , display error indicator msg
         BAL   LINKREG,PUTMSG         , on sysprint
         TM    FLAG,NOTCAT            , any error within catalog
         BZ    NEXTC010               , bif not
         LA    R1,BSPAC10W            , else point to catalog msg
         BAL   LINKREG,PUTMSG         , and display it
NEXTC010 DS    0H                     ,
         TM    FLAG,CATDIFF           , difference between cat & member r
         BZ    NEXTC020               ,
         LA    PARMREG,BSPAC09W       , point to error message
         BAL   LINKREG,PUTMSG         , display message
NEXTC020 DS    0H                     ,
         TM    FLAG,NOTVTOC           , dsn not on disk
         BZ    NEXTC030               ,
         LA    PARMREG,BSPAC12E       ,point to msg text
         BAL   LINKREG,PUTMSG         , show message
NEXTC030 DS    0H                     ,
         MVI   FLAG,X'0'              , re-initialize flagbyte
NEXTC100 DS    0H                     ,
         LA    RECOFFST,80(RECOFFST)  , next card
         CH    RECOFFST,BLOCKLEN      , end of block?
         BE    READBLCK               , Then read next block
         B     READREC                , else get next record
         BOX   'Exit on Error'
CLOSE010 DS    0H                     , Error exit routine
         CLOSE SYSPRINT               , close the SYSPRINT file
         MVC   LASTCC,=F'12'          , set rc
         CLC   MAXCC,LASTCC           , set
         BH    GOHOME                 , return code
         MVC   MAXCC,LASTCC           , and
         B     GOHOME                 , go home
PUTMSG   DS    0H                     , output message on sysprint
         ST    PARMREG,PUTMSG00       , registers
         CP    LINENUM,=PL2'55'       , last line printed
         BL    PUTLINE                , bif not
         PUT   SYSPRINT,HEAD0C1       ,
         PUT   SYSPRINT,HEAD0C2       ,
         PUT   SYSPRINT,HEAD0C3       ,
         PUT   SYSPRINT,HEAD0C4       ,
         PUT   SYSPRINT,HEAD0C5       ,
         PUT   SYSPRINT,HEAD0C6       ,
         ZAP   LINENUM,=P'12'         , reinit line number
         AP    PAGENUM,=P'1'          , increment page number
         MVC   HEAD001A-1(4),=X'40202120'
         ED    HEAD001A-1(4),PAGENUM  , beautify page number
         PUT   SYSPRINT,HEAD001       , write header lines
         PUT   SYSPRINT,HEAD002       , to
         PUT   SYSPRINT,HEAD003       , report file
PUTLINE  DS    0H                     ,
         L     R0,PUTMSG00                       -
         LA    R1,SYSPRINT            ,
         PUT   (R1),(R0)              ,
         BR    LINKREG                , and return
         DC    F'0'                   ,
OPENFILE DS    0H                     ,
         ST    LINKREG,OPENFILE-4     , save return address
         USING IHADCB,DCBREG          , tell assembler
         MVC   BSPAC011,DCBDDNAM      , insert ddname
         MVC   BSPAC021,DCBDDNAM      , into message texts
         CLC   DCBDDNAM,=CL8'SYSPRINT'  wto's are necessary for
         BNE   OPENF010               , open errors on sysprint, only
         MVC   WTO00011,DCBDDNAM      , insert ddname
         MVC   WTO00021,DCBDDNAM      , into message wtos
OPENF010 DS    0H                     ,
         DEVTYPE DCBDDNAM,DEVTYPE     , test if ddstatement allocated
         LTR   RETCODE,RETCODE        , test allocation
         BNZ   OPENF900               ,
OPENF090 DS    0H                     ,
         CLC   DCBDDNAM,=CL8'SYSPRINT'
         BE    OPENF100               ,
         OPEN  ((DCBREG),INPUT)       , open dcb
         B     OPENF110               ,
OPENF100 DS    0H                     ,
         OPEN  ((DCBREG),OUTPUT)      , open dcb
OPENF110 DS    0H                     ,
         TM    DCBOFLGS,DCBOFOPN      , open successful
         BNZ   OPENF995               , then return with rc=0
         CLC   DCBDDNAM,=CL8'SYSPRINT'  issue wto for sysprint
         BE    OPENF800               , open error SYSPRINT req. WTO
         LA    R1,BSPAC02E            , else print error message
         BAL   LINKREG,PUTMSG         , to sysprint
         B     OPENF990               , and return
OPENF800 DS    0H                     , WTO for open error
         CNOP  0,4                    , Align for insert
WTO0002  WTO   'BSPAC02E - Open failed for DD-statement xxxxxxxx, funct-
               ion terminated'        , Issue WTO
WTO00021 EQU   WTO0002+8+40,8         , Insertion point
         B     OPENF990               , return with error set
OPENF900 DS    0H                     ,
         CLC   DCBDDNAM,=CL8'SYSPRINT'
         BE    OPENF910               ,
         LA    R1,BSPAC01E            , point to message text
         BAL   LINKREG,PUTMSG         , display message
         B     OPENF990               , and return
OPENF910 DS    0H                     ,
         CNOP  0,4                    , align to fullword boundary
         PRINT GEN                    ,
WTO0001  WTO   'BSPAC01E - XXXXXXXX DD statement missing, function term-
               inated'                ,
WTO00011 EQU   WTO0001+8+11,8         ,
OPENF990 DS    0H                     , return with errorcode set
         LA    RETCODE,12             , indicate error
         B     OPENF999               , and return
OPENF995 DS    0H                     ,
         XR    RETCODE,RETCODE        ,
OPENF999 DS    0H                     ,
         L     LINKREG,OPENFILE-4     , restore return address
         BR    LINKREG                , and return to caller
TERM9999 DS    0H                     ,
         PUT   SYSPRINT,BSPAC99I      ,
         CLOSE PARMDIR                ,
         CLOSE PARMLIB                ,
         CLOSE SYSPRINT               ,
GOHOME   DS    0H                     ,
         L     RETCODE,MAXCC          , get return code
         BSPRET RC=(15)               , and return with rc in r15
         BOX   'MESSAGES',CTL1=EJECT,CTr=yes
BLKMSG   DS    0CL(&LRECL)            ,
         DC    C'0Block address = X'''
BLKMSG1  DC    CL8' '                 ,
         DC    C''', Length=X'''      ,
BLKMSG2  DC    CL8' '                 ,
         DC    C''''
         FILL  BLKMSG                 ,
DCBMSG   DS    0CL(&LRECL)            ,
         DC    C'0Parmlib DCB is at address x'''
DCBMSG1  DC    CL8' '                 ,
         DC    C''''
         FILL  DCBMSG                 ,
HEAD001  DS    0CL(&LRECL)
         DC    C'1BSPAPFCK Version &BSPVER..&BSPMOD'
         DC    CL20' '                ,
         DC    C'Report'              ,
         FILL  HEAD001                ,
*        ORG   HEAD001+&LRECL-L'HEAD001A-L'HEAD001B-1
         ORG   HEAD001+&LRECL-5-4-1
HEAD001B DC    C'PAGE '               ,
HEAD001A DC    C'   '                 ,
         ORG   ,                      ,
HEAD0C1  DS    0CL(&LRECL)
         DC    CL60'1'
         DC    C'         |l      _,,,---,,_'
         FILL  HEAD0C1
HEAD0C2  DS    0CL(&LRECL)
         DC    CL60' '
         DC    C'   ZZZzz /,:.-'':''    -.  ;-;;,'
         FILL  HEAD0C2
HEAD0C3  DS    0CL(&LRECL)
         DC    CL60' '
         DC    C'        |,4-  ) )-,_. ,( (  :''-'''
         FILL  HEAD0C3
HEAD0C4  DS    0CL(&LRECL)
         DC    CL60' '
         DC    C'       ''---''''(_/--''  :-'')_)'
         FILL  HEAD0C4
HEAD0C5  DS    0CL(&LRECL)
         DC    CL60'0'
         DC    C' Placed into the Hercules Domain'
         FILL  HEAD0C5
HEAD0C6  DS    0CL(&LRECL)
         DC    CL60' '
         DC    C' by Volker Bandke, BSP GmbH'
         FILL  HEAD0C6
HEAD002  DS    0CL(&LRECL)
         DC    C'0Volume: '
HEAD002B DC    CL6'******'            ,
         DC    C'   Dataset: '
HEAD002A DC    CL44'******** NOT KNOWN ********'
         FILL  HEAD002                ,
HEAD003  DS    0CL(&LRECL)
         DC    C'0Member being processed: '
HEAD003A DC    CL8'????????'         -
         FILL  HEAD003                ,
HEAD004  DC    CL(&LRECL)' '          ,
BSPAC01E DS    0CL(&LRECL)             -
         DC    C'0BSPAC01E - '        ,
BSPAC011 DC    CL8' '                 ,
         DC    C' DD statement not allocated, function terminated'
         FILL  BSPAC01E               ,
BSPAC02E DS    0CL(&LRECL)             -
         DC    C'0BSPAC02E - Open failed for DD statement '
BSPAC021 DC    CL8' '                 ,
         DC    C', function terminated'
         FILL  BSPAC02E               ,
BSPAC06E DS    0CL(&LRECL)
         DC    C'0BSPAC06E - RDJFCB failed for parmlib, function termin-
               ated'                  ,
         FILL  BSPAC06E               ,
BSPAC07E DS    0CL(&LRECL)
         DC    C'0BSPAC07E - Find failed with rc=x'''
BSPAC071 DC    C'  '                  ,
         DC    C''''                  ,
         FILL  BSPAC07E               ,
BSPAC08I DS    0CL(&LRECL)
         DC    C'0BSPAC08I - '
BSPAC081 DC    CL72' '                ,
         FILL  BSPAC08I               ,
BSPAC09W DS    0CL(&LRECL)
         DC    C'0BSPAC09E - File is cataloged on '
BSPAC091 DC    CL6' '                 ,
         DC    C', possible abend S047 situation'
         FILL  BSPAC09W               ,
BSPAC10W DS    0CL(&LRECL)
         DC    C'0BSPAC10W - '
BSPAC101 DC    CL60' '                ,
         FILL  BSPAC10W               ,
BSPAC11I DC    CL(&LRECL)' BSPAC11I - No errors found for this statemen+
               t'                     ,
BSPAC12E DS    0CL(&LRECL)
         DC    C'0BSPAC12E - '
BSPAC121 DC    CL60' '                ,
         FILL  BSPAC12E               ,
BSPAC13E DC    CL(&LRECL)'0BSPAC13E - Error encountered processing abov+
               e statement'           ,
BSPAC14I DC    CL(&LRECL)'0BSPAC14I - No errors found processing this m+
               ember'
BSPAC99I DC    CL(&LRECL)' BSPAC99I - End of processing, function termi-
               nated'                 ,
         PRINT NOGEN                  ,
         BOX   'DCB''S',CTR=YES,CTL1=EJECT
SYSPRINT DCB   DDNAME=SYSPRINT,       , ddname for this file           -
               DSORG=PS,              , file is sequential             -
               LRECL=&LRECL,          , record length                  -
               BLKSIZE=&BLKSIZE,      , and blocksize                  -
               MACRF=(PM),            , will be opened for output      -
               RECFM=FBA              , fixed block, ansi cntlchars
PARMDIR  DCB   DDNAME=PARMLIB,        , ddname for this file           -
               DSORG=PS,              , file is sequential             -
               LRECL=256,             , record length                  -
               BLKSIZE=256,           , and blocksize                  -
               KEYLEN=8,              , in order to get record key     -
               MACRF=R,               , will be opened for input       -
               RECFM=F                , fixed block
         PRINT GEN                    ,
PARMLIB  DCB   DDNAME=PARMLIB,        , ddname for this file           -
               DSORG=PO,              , file is sequential             -
               MACRF=R,               , will be opened for input       -
               EXLST=EXLST,           , exit list                      -
               EODAD=EOMEM            , return on eof
         PRINT NOGEN                  ,
         BOX   'VARIABLE DATA AREAS',CTL1=EJECT,CTR=YES
EXLST    DC    XL1'87',AL3(JFCBAREA)  ,
DOUBLE   DC    D'0'                   ,
CAMAREA  DS    0CL265                 ,
TRTTAB   DC    XL256'0'               ,
         FILL  CAMAREA                ,
PUTMSG00 DC    F'0'                   ,
BLOCKADR DC    F'0'                   ,
PAGENUM  DC    PL2'0'                 ,
LINENUM  DC    PL2'100'               ,
LASTCC   DC    F'0'                   , returncode given to caller
MAXCC    DC    F'0'                   , returncode given to caller
DEVTYPE  DC    6F'0'                  , for devtype macro
JFCBAREA DS    0CL176                 ,
         IEFJFCBN ,                   ,
DIRAREA  DS    0CL264' '              ,
PDSKEY   DC    CL8' '                 ,
PDSCOUNT DC    XL2'00'                ,
         FILL  DIRAREA                ,
BLOCKLEN DC    H'0'                   ,
BLDLAREA DC    H'1'                   ,
BLDENTL  DC    Y(BLDELEN)             ,
BLDMEMBR DC    CL8' '                 ,
BLDTTR   DC    XL3'0'                 ,
BLDCONC  DC    XL1'0'                 ,
BLDLOC   DC    XL1'0'                 ,
BLDTYPE  DC    XL1'0'                 ,
BLDUSER  DC    XL62'0'                ,
BLDELEN  EQU   *-BLDMEMBR             ,
DSNAME   DC    CL44' '                ,
         DC    C'VOLUME'                -
VOLUME   DC    CL6' '                 ,
FLAG     DC    XL1'0'                 ,
NOTCAT   EQU   B'10000000'            ,
CATDIFF  EQU   B'01000000'            ,
NOTVTOC  EQU   B'00100000'            ,
         BOX   'CONSTANT DATA AREA',CTL1=EJECT,CTR=YES
         DS    0F                     ,
TRT      TRT   0(*-*,R1),TRTTAB       ,
MOVEDSN  MVC   DSNAME(*-*),0(R15)     , insert dsname into field
MOVEVOL  MVC   VOLUME(*-*),0(R14)     ,
CATLIST  CAMLST NAME,DSNAME,,CAMAREA   -
VTOCLIST CAMLST SEARCH,DSNAME,VOLUME,CAMAREA  -
CATRCTB  DC    A(0)                   ,
         DC    A(CATRC04)             ,
         DC    A(CATRC08)             ,
         DC    A(CATRC0C)             ,
         DC    A(CATRC10)             ,
         DC    A(CATRC14)             ,
         DC    A(CATRC18)             ,
         DC    A(CATRC1C)             ,
         DC    A(CATRC20)             ,
CATRC04  DC    CL60'RC=04 (X''04'') reading catalog'
CATRC08  DC    CL60'Dataset not cataloged'
CATRC0C  DC    CL60'RC=12 (X''0C'') reading catalog'
CATRC10  DC    CL60'RC=16 (X''10'') reading catalog'
CATRC14  DC    CL60'Invalid dataset name'
CATRC18  DC    CL60'Permanent I/O error reading catalog'
CATRC1C  DC    CL60'RC=28 (X''1C'') reading catalog'
CATRC20  DC    CL60'RC=32 (X''20'') reading catalog'
VTOCRCTB DC    A(0)                   ,
         DC    A(VTOCRC04)            ,
         DC    A(VTOCRC08)            ,
         DC    A(VTOCRC0C)            ,
         DC    A(VTOCRC10)            ,
VTOCRC04 DC    CL60'Required volume is not mounted'
VTOCRC08 DC    CL60'Format-1 DSCB not found on volume'
VTOCRC0C DC    CL60'Permanent I/O error reading VTOC'
VTOCRC10 DC    CL60'Invalid workarea pointer'
         DCBD  DEVD=DA,DSORG=(PS,PO)  ,
PDSDIRDS DSECT                        ,
PDSMEMBR DS    CL8                    ,
PDSTTR   DS    CL3                    ,
PDSC     DS    CL1                    ,
PDSUSER  EQU   *                      ,
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
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.LINKLIB
//SYSLIN   DD  DISP=(OLD,DELETE),DSN=*.ASM.SYSPUNCH
//          DD *
 NAME BSPAPFCK(R)
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=BSPAPFCK
//BSPAPFCK  JOB  (SETUP),
//             'Run BSPAPFCK',
//             CLASS=A,
//             MSGCLASS=H,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//********************************************************************
//*
//* Name: BSPAPFCK
//*
//* Desc: Run the BSPAPFCK program
//*
//********************************************************************
//LISTAPF EXEC PGM=BSPAPFCK
//
@@