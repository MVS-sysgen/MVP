//DDN  JOB (TSO),
//             'Install DDN',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//* ********************************************************
//* *  INSTALL THE 'DD' TSO COMMAND AS 'DDN'               *
//* ********************************************************
//ASMFC  EXEC ASMFC,
//             PARM.ASM='OBJ,NODECK,TERM'
//ASM.SYSIN DD *
         TITLE '   D D    '                                             00280540
*********************************************************************** 00280550
*                                                                     * 00280560
*        'DD' TSO COMMAND                                             * 00280570
*                                                                     * 00280580
*********************************************************************** 00280590
         SPACE                                                          00280600
*  WRITTEN BY. BILL GODFREY, PRC (PLANNING RESEARCH CORPORATION).       00280610
*  INSTALLATION. PRC, MCLEAN VA.                                        00280620
*  DATE WRITTEN. MAY 14 1980.                                           00280630
*  DATE UPDATED. MAY 26 1982.                                           00280640
*  ATTRIBUTES. RE-ENTRANT.                                              00280650
*  EXTERNAL REFERENCES. 'S99DAIR' SUBPROGRAM.                           00280660
*   SOURCE FOR S99DAIR SHOULD FOLLOW THE 'END' STATEMENT OF             00280670
*   THIS SOURCE, FOR BATCH ASSEMBLY.                                    00280680
*  DESCRIPTION.                                                         00280690
*   THIS IS A SHORTHAND VERSION OF THE ALLOC COMMAND.                   00280700
*                                                                       00280710
*   FOR EXAMPLE, INSTEAD OF TYPING                                      00280720
*      ALLOC FI(SYSPROC) DA(CLIST) SHR                                  00280730
*   THE USER CAN TYPE                                                   00280740
*      DD SYSPROC CLIST                                                 00280750
*                                                                       00280760
*   IN ADDITION, IF THE FILENAME IS ALREADY ALLOCATED                   00280770
*   THIS COMMAND WILL FREE IT AND RE-ALLOCATE IT.                       00280780
*                                                                       00280790
*   THIS COMMAND HAS MANY OF THE CAPABILITIES OF THE ALLOC              00280800
*   COMMAND, AND A FEW THAT THE VANILLA ALLOC COMMAND DOESNT,           00280810
*   SUCH AS COPIES, FORM, AND FCB FOR SYSOUT, AND                       00280820
*   RECFM, LRECL, BLKSIZE.                                              00280830
*                                                                       00280840
*   THERE ARE 3 RESERVED DSNAMES (SYSOUT, DUMMY, AND $)                 00280850
*   AND ONE RESERVED DSNAME LEVEL ($.) WHICH THIS COMMAND               00280860
*   RECOGNIZES FOR SPECIAL USES.  IF YOU WANT TO ALLOCATE               00280870
*   A DATA SET THAT HAS A NAME LIKE ANY OF THESE, YOU                   00280880
*   CANNOT DO IT WITH THIS COMMAND, UNLESS (1) THE DSNAME               00280890
*   IS OF THE FORM PREFIX.SYSOUT, PREFIX.DUMMY, OR PREFIX.$             00280900
*   AND YOU SPECIFY THE FULLY QUALIFIED NAME IN QUOTES,                 00280910
*   OR (2) YOU SPECIFY THE 'DATASET' KEYWORD (OR 'DA')                  00280920
*   WHICH FORCES THE COMMAND TO TREAT THE RESERVED DSNAMES              00280930
*   AS REGULAR DSNAMES.  EXAMPLE - DD SYSUT1 DUMMY DA                   00280940
*                                                                       00280950
*   IF THE DSNAME IS 'SYSOUT' THEN A SYSOUT FILE                        00280960
*   WILL BE ALLOCATED, NOT A DATA SET NAMED SYSOUT.                     00280970
*                                                                       00280980
*   IF THE DSNAME IS 'DUMMY' THEN A DUMMY FILE                          00280990
*   WILL BE ALLOCATED, NOT A DATA SET NAMED DUMMY.                      00281000
*                                                                       00281010
*   YOU MAY NOT ABBREVIATE THE WORD 'SYSOUT' OR 'DUMMY'                 00281020
*   IF YOU DO, YOU WILL ALLOCATE A DATA SET WITH THE                    00281030
*   SPECIFIED NAME INSTEAD OF A SYSOUT OR DUMMY FILE.                   00281040
*                                                                       00281050
*   IF THE DSNAME IS '$' THEN A TEMPORARY DATA SET                      00281060
*   WILL BE ALLOCATED, NOT A DATA SET NAMED $.                          00281070
*                                                                       00281080
*   SYNTAX -                                                            00281090
*      DD  FILENAME  DSNAME  SHR/OLD/MOD/NEW  USING(ATTR)               00281100
*                            UNIT(UNIT)  VOLUME(VOLUME)                 00281110
*                            SPACE(PRIM,SEC)  DIR(NUMBER)               00281120
*                            TRACKS/CYLINDERS/BLOCK(LENGTH)             00281130
*                            DATASET  RLSE  FORM(ID)  HOLD              00281140
*                            DEST(NAME)  COPIES(N)  FCB(ID)             00281150
*                            RECFM(X)  LRECL(N)  BLKSIZE(N)             00281160
*                            LABEL(TYPE)  POS(N)  EXPDT(N)              00281170
*                            CHARS(NAME) BURST/NOBURST DSORG(XX)        00281180
*                                                                       00281190
*   DEFAULTS -                                                          00281200
*      SHR (UNLESS SPACE OR DIR OR TRK/CYL/BLK)                         00281210
*      NEW (IF SPACE OR DIR OR TRK/CYL/BLK SPECIFIED)                   00281220
*      TRACKS (IF SPACE IS SPECIFIED)                                   00281230
*                                                                       00281240
*   TEMPORARY DATA SETS -                                               00281250
*      IF THE DSNAME IS '$' OR IF THE FIRST LEVEL OF THE                00281260
*      DSNAME IS '$.' THEN A TEMPORARY DATA SET                         00281270
*      WILL BE ALLOCATED.                                               00281280
*                                                                       00281290
*      IF THE DSNAME IS '$' (QUOTED OR NOT)                             00281300
*      THEN THE DSNAME WILL BE IGNORED AND THE SYSTEM                   00281310
*      WILL GENERATE A TEMPORARY DSNAME.                                00281320
*                                                                       00281330
*      IF THE FIRST LEVEL OF THE DSNAME IS '$.'                         00281340
*      (QUOTED OR NOT) THEN THE NEXT QUALIFIER IN THE DSNAME            00281350
*      WILL BE PREFIXED WITH AN AMPERSAND AND THE SYSTEM                00281360
*      WILL GENERATE A TEMPORARY DSNAME THAT INCLUDES THE               00281370
*      SPECIFIED QUALIFIER.  FOR INSTANCE, THE DSNAME                   00281380
*      $.SYSUT1 WILL BE CHANGED TO &SYSUT1.                             00281390
*                                                                       00281400
*      TEMPORARY DATA SETS ARE ALLOCATED                                00281410
*      AS DISP=(NEW,DELETE) INSTEAD OF (NEW,CATLG)                      00281420
*      OR DISP=(OLD,DELETE) INSTEAD OF (OLD,CATLG).                     00281430
*                                                                       00281440
*   S99DAIR                                                             00281450
*      S99DAIR IS A MODIFIED VERSION OF IKJDAIR WHICH ACCEPTS           00281460
*      SOME NEW FIELDS IN DA08, DA1C, DA30 PARAMETER BLOCKS,            00281470
*      AND INTERPRETS SOME EXISTING FIELDS DIFFERENTLY.                 00281480
*      THE DA30OCLS FIELD IS IGNORED BY IKJDAIR, BUT S99DAIR            00281490
*      WILL ALLOCATE THE SPECIFIED CLASS.  IF THE DA08PQTY              00281500
*      FIELD (PRIMARY SPACE) IS ZERO (WITH DISP NEW OR MOD)             00281510
*      IKJDAIR WILL ALLOCATE DEFAULT SPACE, BUT S99DAIR                 00281520
*      WILL DO THAT ONLY IF SPACE TYPE (TRK, CYL, BLK) IS               00281530
*      NOT SPECIFIED. OTHERWISE IT WILL ALLOCATE ZERO SPACE.            00281540
*                                                                       00281550
*   LOG OF CHANGES -                                                    00281560
*   04JUN80 - ADDED KEYWORDS OLD, MOD, UNIT, VOLUME, USING.             00281570
*   05FEB81 - ADDED KEYWORDS NEW SPACE TRACK/CYL/BLOCK DIR.             00281580
*             CONCATENATION SUPPORT ADDED.                              00281590
*             SUPPORT FOR TEMPORARY DATA SETS ADDED.                    00281600
*   02MAR81 - SUPPORT FOR TERMINAL, SYSOUT, AND DUMMY.                  00281610
*   06MAR81 - 'TEMP' KEYWORD NULLIFIED.  TEMPORARY                      00281620
*             DATA SETS ARE NOW INDICATED BY DSNAME ALONE.              00281630
*             'FORM' 'HOLD' 'DEST' 'COPIES' AND 'FCB' ADDED.            00281640
*             ALLOW DISP=MOD TO CREATE A NEW DATA SET.                  00281650
*             CALL 'S99DAIR' INSTEAD OF IKJDAIR. DROP SYSO99.           00281660
*   10MAR81 - ALLOW HYPHEN IN UNIT NAME. 'RECFM', 'LRECL'               00281670
*             'BLKSIZE' ADDED FOR DSN, SYSOUT, AND TERMINAL.            00281680
*             'POS', 'LABEL', AND 'EXPDT' ADDED.                        00281690
*             (EXPDT MAINLY FOR TAPE MANAGEMENT SYSTEM).                00281700
*   08APR81 - ADD A BIT TO INDICATE THAT EXISTING ALLOCATIONS           00281710
*             MUST NOT BE USED TO SATISFY THE REQUEST.                  00281720
*             SET THE BIT ON WHEN ALLOCATING FOR CONCATENATE.           00281730
*             THIS FIXES A BUG IN WHICH CONCATENATIONS                  00281740
*             USED EXISTING ALLOCATIONS, MAKING THE EXISTING            00281750
*             DDNAME DISAPPEAR FROM THE TIOT.                           00281760
*   16JUL81 - ALLOW SYSOUT CLASS TO BE 2 CHARACTERS IF THE              00281770
*             FIRST IS A POUND SIGN AND IGNORE THE POUND SIGN.          00281780
*             NUMERIC SYSOUT CLASSES CANNOT BE SPECIFIED IN THE         00281790
*             SYSOUT(X) OPERAND BECAUSE THAT OPERAND IS DEFINED TO      00281800
*             PARSE AS A DSNAME(MEMBER) AND A MEMBER CANNOT BEGIN       00281810
*             WITH A NUMERIC.                                           00281820
*             TO SPECIFY A NUMERIC SYSOUT CLASS, TYPE IN                00281830
*             SYSOUT(#0) THRU SYSOUT(#9).                               00281840
*   08DEC81 - ALLOW SIMPLE NUMERIC SYSOUT CLASS, WITHOUT POUND SIGN,    00281850
*             UNLESS USER IS PROMPTED BY PARSE FOR THE OPERAND.         00281860
*   08DEC81 - ADD 'CHARS' KEYWORD FOR 3800 PRINTER.THIS KEYWORD         00281870
*             WORKS ONLY IF 3800 ENHANCEMENTS ARE INSTALLED,            00281880
*             ALLOWING SETPRT TO SYSOUT FILES.                          00281890
*             IF 'CHARS' IS SPECIFIED, THIS COMMAND MUST                00281900
*             OPEN THE SYSOUT FILE TO ISSUE THE SETPRT MACRO.           00281910
*             THIS WILL RESULT IN AN 013-34 ABEND IF RECFM AND          00281920
*             LRECL ARE NOT SPECIFIED IN THE SAME COMMAND.              00281930
*   14DEC81 - ADD 'BURST' AND 'NOBURST' KEYWORDS FOR 3800.              00281940
*             THE SETPRT MACRO APPARENTLY ALWAYS SETS BURST OPTION      00281950
*             TO YES OR NO - IT WONT JUST LEAVE IT AS IS.               00281960
*             SO WHEN WE ISSUE SETPRT FOR 'CHARS' WE GET BURST=N        00281970
*             EVEN THOUGH WE DIDNT SPECIFY THE BURST=N OPERAND.         00281980
*             SO WE ADDED THESE KEYWORDS.                               00281990
*             INSTALLATIONS RUNNING WITH DEFAULT BURST=YES WILL         00282000
*             WANT TO CHANGE THE INSTRUCTION AT 'BURSTDEF'.             00282010
*   14MAY82 - ADD DSORG KEYWORD. NOTE - S99DAIR MODIFIED FOR DSORG.     00282020
*   26MAY82 - PROBLEM IN S99DAIR FIXED. TEMPORARY DSNAMES LIKE $.ABC    00282030
*             WERE BEING TREATED LIKE $ (NO DSNAME) INSTEAD OF &ABC.    00282040
*             MISC CHANGES IN CONFLICTING KEYWORDS FOR $.               00282050
         SPACE                                                          00282060
         GBLB  &MVS                                                     00282070
&MVS     SETB  1                   1 - MVS   0 - SVS,MVT                00282080
         SPACE                                                          00282090
DD       START                                                          00282100
         USING *,R10,R11                                                00282110
         B     @PROLOG-*(,15)                                           00282120
         DC    AL1(11),CL11'DD '                                        00282130
         DC    CL16' &SYSDATE &SYSTIME '                                00282140
@SIZE    DC    0F'0',AL1(1),AL3(@DATAL)                                 00282150
@PROLOG  STM   14,12,12(13)                                             00282160
         LR    R10,R15             BASE                                 00282170
         LA    R15,1                                                    00282180
         LA    R11,4095(R15,R10)   BASE                                 00282190
         LR    R2,R1                                                    00282200
         USING CPPL,R2                                                  00282210
         L     R0,@SIZE                                                 00282220
         GETMAIN R,LV=(0)                                               00282230
         LR    R9,R1                                                    00282240
         USING @DATA,R9                                                 00282250
         ST    13,4(,1)            CHAIN SAVEAREA                       00282260
         ST    1,8(,13)            CHAIN SAVEAREA                       00282270
         LR    13,1                                                     00282280
         SPACE 1                                                        00282290
         STM   R10,R11,BASES                                            00282300
         MVI   STATUS,0                                                 00282310
         XC    LINKAREA(8),LINKAREA                                     00282320
         SLR   R15,R15                                                  00282330
         STH   R15,RC              SET RC = 0                           00282340
         SPACE                                                          00282350
************************************************************            00282360
*                                                          *            00282370
*        SET UP IOPL FOR PUTLINE                           *            00282380
*                                                          *            00282390
************************************************************            00282400
         SPACE                                                          00282410
         LA    R15,MYIOPL                                               00282420
         USING IOPL,R15                                                 00282430
         MVC   IOPLUPT(4),CPPLUPT                                       00282440
         MVC   IOPLECT(4),CPPLECT                                       00282450
         LA    R0,MYECB                                                 00282460
         ST    R0,IOPLECB                                               00282470
         XC    MYECB,MYECB                                              00282480
         LA    R0,MYPTPB                                                00282490
         ST    R0,IOPLIOPB                                              00282500
         DROP  R15                 IOPL                                 00282510
         SPACE                                                          00282520
************************************************************            00282530
*                                                          *            00282540
*        SCAN THE COMMAND BUFFER FOR SYSOUT(1) THRU (9)    *            00282550
*        AND CHANGE IT TO SYSOUT(A) THRU (I) AND SET A     *            00282560
*        SWITCH SO WE CAN CHANGE IT BACK LATER.            *            00282570
*        PARSE THINKS THAT CLASS IS A MEMBER NAME          *            00282580
*        AND A MEMBER NAME CANNOT BE A NUMBER.             *            00282590
*                                                          *            00282600
************************************************************            00282610
         SPACE                                                          00282620
         MVI   SETCLASS,X'40'      INITIALIZE SYSOUT CLASS SWITCH       00282630
         L     R1,CPPLCBUF                                              00282640
         LH    R0,0(,R1)           GET LENGTH OF CBUF                   00282650
         SH    R0,=H'4'            MINUS LENGTH OF PREFIX HALFWORDS     00282660
         SH    R0,=H'9'            MINUS LENGTH OF ' SYSOUT(X'          00282670
         BNP   CBUFX               BUFFER NOT LONG ENOUGH               00282680
         LA    R1,4(,R1)                                                00282690
CBUFLOOP CLC   1(7,R1),=C'SYSOUT('                                      00282700
         BE    CBUFGOT                                                  00282710
         CLC   1(7,R1),=X'A2A8A296A4A34D' LOWER CASE 'SYSOUT('          00282720
         BE    CBUFGOT                                                  00282730
CBUFINCR LA    R1,1(,R1)                                                00282740
         BCT   R0,CBUFLOOP                                              00282750
         B     CBUFX                                                    00282760
CBUFGOT  CLI   0(R1),C' '          PRECEDED BY BLANK                    00282770
         BE    CBUFGOT2                                                 00282780
         CLI   0(R1),C','          PRECEDED BY COMMA                    00282790
         BE    CBUFGOT2                                                 00282800
         CLI   0(R1),X'05'         PRECEDED BY TAB                      00282810
         BE    CBUFGOT2                                                 00282820
         B     CBUFX                                                    00282830
CBUFGOT2 CLI   9(R1),C')'          CLOSE PAREN                          00282840
         BNE   CBUFX                                                    00282850
         CLI   8(R1),C'0'          NUMERIC CLASS                        00282860
         BL    CBUFX               NO, BRANCH                           00282870
         NI    8(R1),X'CF'         CHANGE 1-9 TO A-I                    00282880
         OI    SETCLASS,X'F0'      AND SET SWITCH FOR LATER             00282890
         CLI   8(R1),X'C0'         SYSOUT(0)                            00282900
         BNE   *+8                  BECOMES                             00282910
         MVI   8(R1),X'7C'           SYSOUT(@)                          00282920
CBUFX    EQU   *                                                        00282930
         SPACE                                                          00282940
************************************************************            00282950
*                                                          *            00282960
*        SET UP PPL FOR PARSE                              *            00282970
*                                                          *            00282980
************************************************************            00282990
         SPACE                                                          00283000
         LA    R15,MYPPL                                                00283010
         USING PPL,R15                                                  00283020
         MVC   PPLUPT(4),CPPLUPT                                        00283030
         MVC   PPLECT(4),CPPLECT                                        00283040
         LA    R0,MYECB                                                 00283050
         ST    R0,PPLECB                                                00283060
         XC    MYECB,MYECB                                              00283070
         L     R0,=A(DDPCL)                                             00283080
         ST    R0,PPLPCL                                                00283090
         LA    R0,MYANS                                                 00283100
         ST    R0,PPLANS                                                00283110
         XC    MYANS(4),MYANS                                           00283120
         MVC   PPLCBUF(4),CPPLCBUF                                      00283130
         ST    R9,PPLUWA           STORE R9 FOR VALIDITY CHECK RTN      00283140
         DROP  R15                 PPL                                  00283150
         SPACE 1                                                        00283160
************************************************************            00283170
*                                                          *            00283180
*        CALL THE PARSE SERVICE ROUTINE                    *            00283190
*                                                          *            00283200
************************************************************            00283210
         SPACE 1                                                        00283220
         LR    R1,R15              POINT TO PPL                         00283230
         AIF   (NOT &MVS).SKIP2                                         00283240
         L     R15,16              CVTPTR                               00283250
         TM    524(R15),X'80'      IF HI ORDER BIT NOT ON               00283260
         BNO   PARSELNK               THEN DO LINK, NOT CALL            00283270
         L     R15,524(,R15)       CVTPARS                              00283280
         BALR  R14,R15             CALL IKJPARS                         00283290
         B     PARSEEXT            SKIP AROUND LINK                     00283300
PARSELNK EQU   *                                                        00283310
.SKIP2   ANOP                                                           00283320
         LINK  EP=IKJPARS,SF=(E,LINKAREA)                               00283330
PARSEEXT EQU   *                                                        00283340
         SPACE 1                                                        00283350
         LTR   R15,R15                                                  00283360
         BZ    PARSEOK                                                  00283370
         LA    R1,MSG01                                                 00283380
         LA    R0,L'MSG01                                               00283390
         BAL   R14,PUTMSG                                               00283400
         LA    R15,12                                                   00283410
         B     EXIT                                                     00283420
PARSEOK  EQU   *                                                        00283430
         SPACE                                                          00283440
         L     R3,MYANS                                                 00283450
         USING IKJPARMD,R3                                              00283460
         SPACE                                                          00283470
************************************************************            00283480
*                                                          *            00283490
*        GET THE FILENAME (DDNAME)                         *            00283500
*                                                          *            00283510
************************************************************            00283520
         SPACE                                                          00283530
         MVC   DDSAVE,=CL8' '                                           00283540
         LA    R6,DDN                                                   00283550
         L     R15,0(,R6)                                               00283560
         LH    R1,4(,R6)                                                00283570
         BCTR  R1,0                                                     00283580
         B     *+10                                                     00283590
         MVC   DDSAVE(0),0(R15)                                         00283600
         EX    R1,*-6                                                   00283610
         SPACE                                                          00283620
************************************************************            00283630
*                                                          *            00283640
*         FIND CONFLICTING KEYWORDS                        *            00283650
*                                                          *            00283660
************************************************************            00283670
         SPACE                                                          00283680
         CLI   SETCLASS,X'F0'      WAS CBUF MODIFIED                    00283690
         BNE   SETOK               NO, BRANCH                           00283700
         CLI   DATAKW+1,1          WAS 'DATASET' SPECIFIED              00283710
         BNE   SETOK               NO, BRANCH                           00283720
         LA    R1,=C'INVALID MEMBER NAME'  SYSOUT(0) THRU (9)           00283730
         LA    R0,19                                                    00283740
         B     SYNTAX                                                   00283750
SETOK    EQU   *                                                        00283760
*                                                                       00283770
*              IF 'OLD/SHR/NEW/MOD' NOT SPECIFIED                       00283780
*                  AND NEITHER 'SPACE' NOR 'DIR'                        00283790
*                  NOR 'TRK/CYL/BLOCK' IS SPECIFIED                     00283800
*                  THEN ASSUME DISP 'SHR'.                              00283810
*                                                                       00283820
*              IF 'OLD/SHR/NEW/MOD' NOT SPECIFIED                       00283830
*                  AND EITHER 'SPACE' OR 'DIR'                          00283840
*                  OR 'TRK/CYL/BLOCK' IS SPECIFIED                      00283850
*                  THEN ASSUME DISP 'NEW'.                              00283860
*                                                                       00283870
         CLI   DISPKW+1,4          NEW SPECIFIED                        00283880
         BE    CNEWCCAT            YES, BRANCH                          00283890
CNOTNEW  CLI   DISPKW+1,0          IS DISP SPECIFIED                    00283900
         BNE   CSOM                YES, MUST BE SHR/OLD/MOD             00283910
         CLI   SPACEKW+1,0         IS SPACE SPECIFIED                   00283920
         BNE   CASSMNEW            YES, ASSUME NEW                      00283930
         CLI   DIRKW+1,0           IS DIR SPECIFIED                     00283940
         BNE   CASSMNEW            YES, ASSUME NEW                      00283950
         CLI   TYPKW+1,0           IS TRK/CYL/BLOCK SPECIFIED           00283960
         BNE   CASSMNEW            YES, ASSUME NEW                      00283970
CASSMSHR MVI   DISPKW+1,1          NO, ASSUME SHR                       00283980
         B     CDISPOK             END DISP CHECKING                    00283990
CASSMNEW MVI   DISPKW+1,4          YES, ASSUME NEW                      00284000
         B     CNEWCCAT            YES, ASSUME NEW                      00284010
CSOM     EQU   *                                                        00284020
         CLI   DISPKW+1,3          MOD SPECIFIED                        00284030
         BE    CNEWCCAT            YES, GO CHECK FOR CONCAT             00284040
         CLI   SPACEKW+1,0         SPACE WITH OLD/SHR                   00284050
         BE    COSHDIR             NO, BRANCH                           00284060
         LA    R1,=C'SPACE INVALID WITH SHR/OLD'                        00284070
         LA    R0,26                                                    00284080
         B     SYNTAX                                                   00284090
COSHDIR  CLI   DIRKW+1,0           DIR WITH OLD/SHR                     00284100
         BE    COSMTRK             NO, BRANCH                           00284110
         LA    R1,=C'DIR INVALID WITH SHR/OLD'                          00284120
         LA    R0,24                                                    00284130
         B     SYNTAX                                                   00284140
COSMTRK  CLI   TYPKW+1,0           TRK/CYL/BLK WITH OLD/SHR             00284150
         BE    CDISPOK                                                  00284160
         LA    R1,=C'TRK/CYL/BLK INVALID WITH SHR/OLD'                  00284170
         LA    R0,32                                                    00284180
         B     SYNTAX                                                   00284190
CNEWCCAT EQU   *                                                        00284200
         CLI   DSN+24,X'FF'        IS THERE ONLY ONE DSNAME             00284210
         BE    CNEWSPC             YES, BRANCH                          00284220
         LA    R1,=C'NEW/MOD INVALID WITH CONCATENATION REQUEST'        00284230
         LA    R0,42                                                    00284240
         B     SYNTAX                                                   00284250
CNEWSPC  CLI   TYPKW+1,0           IS SPACE TYPE SPECIFIED              00284260
         BE    CDISPOK             NO, BRANCH                           00284270
         CLI   SPACEKW+1,0         SPACE SPECIFIED                      00284280
         BNE   CDISPOK             YES, BRANCH                          00284290
         LA    R1,=C'MISSING SPACE KEYWORD'                             00284300
         LA    R0,21                                                    00284310
SYNTAX   BAL   R14,PUTMSG                                               00284320
         LA    R15,12                                                   00284330
         B     STOP                                                     00284340
CDISPOK  EQU   *                                                        00284350
         SPACE                                                          00284360
         LA    R6,DSN                                                   00284370
         CLI   24(R6),X'FF'        IS THIS TO BE A CONCATENATION        00284380
         BE    *+8                 NO                                   00284390
         OI    STATUS,STATCON      YES                                  00284400
         LR    R0,R6               SAVE FIRST OF LIST OF NAMES          00284410
         B     *+8                                                      00284420
SYNDSN01 L     R6,24(,R6)                                               00284430
         TM    6(R6),X'80'         IS DSN PRESENT                       00284440
         BO    SYNDSN03            YES, BRANCH                          00284450
SYNDSN02 LA    R1,MSG05            NO - JUST MEMBER NAME                00284460
         LA    R0,L'MSG05                                               00284470
         B     SYNTAX                                                   00284480
SYNDSN03 L     R1,0(R6)                                                 00284490
         CLI   0(R1),C'*'          IS THERE AN ASTERISK                 00284500
         BNE   SYNDSN05            NO, BRANCH                           00284510
         CLI   5(R6),1             IS LENGTH = 1                        00284520
         BNE   SYNDSN99            NO, BRANCH                           00284530
         CR    R6,R0               IS THIS IN A CONCATENATION           00284540
         BNE   SYNDSN04            YES, BRANCH                          00284550
         CLI   24(R6),X'FF'        IS THIS FIRST IN A CONCATENATION     00284560
         BE    SYNDSN99            NO, BRANCH                           00284570
SYNDSN04 LA    R1,=C'ASTERISK (*) INVALID WITH CONCATENATION'           00284580
         LA    R0,39                                                    00284590
         B     SYNTAX                                                   00284600
SYNDSN05 CLI   DATAKW+1,1          DATASET SPECIFIED                    00284610
         BE    SYNDSN99            YES, BYPASS SYSOUT/DUMMY/$           00284620
         CLI   5(R6),6             IS LENGTH = 6                        00284630
         BNE   SYNDSN08            NO, BRANCH                           00284640
         CLC   0(6,R1),=C'SYSOUT'                                       00284650
         BNE   SYNDSN08                                         26MAY82 00284660
         TM    14(R6),X'80'        IS A SYSOUT CLASS SPECIFIED          00284670
         BZ    SYNDSN06            NO, BRANCH                           00284680
         CLI   13(R6),1            IS IT ONLY ONE CHARACTER             00284690
         BE    SYNDSN06            YES, BRANCH                          00284700
         CLI   13(R6),2            IS IT 2 CHARACTERS                   00284710
         BNE   SYNDSNX5            NO, BRANCH                           00284720
         L     R1,8(,R6)           POINT TO CLASS                       00284730
         CLI   0(R1),C'#'          IS IT SYSOUT(#X)                     00284740
         BNE   SYNDSNX5            NO, BRANCH                           00284750
         LA    R1,1(,R1)           YES, POINT PAST # TO X               00284760
         ST    R1,8(,R6)           YES, CHANGE IT TO SYSOUT(X)          00284770
         MVI   13(R6),1            CHANGE LENGTH FROM 2 TO 1            00284780
         L     R1,0(,R6)           RESTORE R1                           00284790
         B     SYNDSN06            PROCEED                              00284800
SYNDSNX5 LA    R1,=C'INVALID SYSOUT CLASS - MORE THAN ONE CHARACTER'    00284810
         LA    R0,46                                                    00284820
         B     SYNTAX                                                   00284830
SYNDSN06 CR    R6,R0               IS THIS IN A CONCATENATION           00284840
         BNE   SYNDSN07            YES, BRANCH                          00284850
         CLI   24(R6),X'FF'        IS THIS FIRST IN A CONCATENATION     00284860
         BE    SYNDSN99            NO, BRANCH                           00284870
SYNDSN07 LA    R1,=C'SYSOUT INVALID WITH CONCATENATION'                 00284880
         LA    R0,33                                                    00284890
         B     SYNTAX                                                   00284900
SYNDSN08 CLI   5(R6),5             IS LENGTH = 5                        00284910
         BNE   SYNDSN10            NO, BRANCH                           00284920
         CLC   0(5,R1),=C'DUMMY'                                        00284930
         BNE   SYNDSN10                                         26MAY82 00284940
         OI    6(R6),X'40'         FORCE QUOTES                         00284950
         CR    R6,R0               IS THIS IN A CONCATENATION           00284960
         BNE   SYNDSN09            NO, BRANCH                           00284970
         CLI   24(R6),X'FF'        IS THIS FIRST IN A CONCATENATION     00284980
         BE    SYNDSN99            NO, BRANCH                           00284990
SYNDSN09 LA    R1,=C'DUMMY INVALID WITH CONCATENATION'                  00285000
         LA    R0,32                                                    00285010
         B     SYNTAX                                                   00285020
SYNDSN10 EQU   *                                                        00285030
*        CLI   TEMPKW+1,1          TEMP SPECIFIED                       00285040
*        BNE   NOTTEMP             NO, BRANCH                           00285050
         CLI   0(R1),C'$'          DOES NAME BEGIN WITH $               00285060
         BNE   SYNDSN99            NO, NOT TEMPORARY                    00285070
         CLI   5(R6),1             IS LENGTH = 1                        00285080
         BE    SYNDSN11            YES, BRANCH                  26MAY82 00285090
         CLI   1(R1),C'.'          IS IT $.XXX                  26MAY82 00285100
         BNE   SYNDSN99            NO, BRANCH                           00285110
SYNDSN11 CLI   DISPKW+1,2          SHR OR OLD WITH DSN $                00285120
         BH    SYNDSN99            NO, BRANCH                           00285130
         LA    R1,=C'DSNAME $ INVALID WITH SHR OR OLD'                  00285140
         LA    R0,32                                                    00285150
         B     SYNTAX                                                   00285160
SYNDSN99 CLI   24(R6),X'FF'        IS THERE ANOTHER DSNAME              00285170
         BNE   SYNDSN01            YES, BRANCH                          00285180
         SPACE                                                          00285190
************************************************************            00285200
*                                                          *            00285210
*         FILL IN DCB PARAMETERS                           *            00285220
*                                                          *            00285230
************************************************************            00285240
         SPACE                                                          00285250
         SLR   R0,R0                                                    00285260
         STH   R0,DSORGVAL                                              00285270
         STH   R0,RECSIZE                                               00285280
         STH   R0,BLOCKSI                                               00285290
         MVI   RECFORM,0                                                00285300
         CLI   DSOKW+1,0           DSORG SPECIFIED                      00285310
         BE    SYNDSOX             NO, BRANCH                           00285320
         LH    R1,DSOSFKW                                               00285330
         SLL   R1,1                MULTIPLY BY 2                        00285340
         LH    R1,DSORGTAB(R1)                                          00285350
         STH   R1,DSORGVAL                                              00285360
SYNDSOX  EQU   *                                                        00285370
         CLI   RECKW+1,0           RECFM SPECIFIED                      00285380
         BE    SYNRECX             NO, BRANCH                           00285390
         CLI   RECKWF+1,0          F, V, OR U SPECIFIED                 00285400
         BNE   SYNRECF             YES, BRANCH                          00285410
         LA    R1,=C'INVALID RECFM - MISSING F, V, OR U'                00285420
         LA    R0,34                                                    00285430
         B     SYNTAX                                                   00285440
SYNRECF  EQU   *                                                        00285450
         LA    R1,RECFORM                                               00285460
         CLI   RECKWF+1,1          F                                    00285470
         BNE   *+8                                                      00285480
         OI    0(R1),RECFMF                                             00285490
         CLI   RECKWF+1,2          V                                    00285500
         BNE   *+8                                                      00285510
         OI    0(R1),RECFMV                                             00285520
         CLI   RECKWF+1,3          U                                    00285530
         BNE   *+8                                                      00285540
         OI    0(R1),RECFMU                                             00285550
         CLI   RECKWB+1,1          B                                    00285560
         BNE   *+8                                                      00285570
         OI    0(R1),RECFMB                                             00285580
         CLI   RECKWS+1,1          S                                    00285590
         BNE   *+8                                                      00285600
         OI    0(R1),RECFMS                                             00285610
         CLI   RECKWA+1,1          A                                    00285620
         BNE   *+8                                                      00285630
         OI    0(R1),RECFMA                                             00285640
         CLI   RECKWA+1,2          M                                    00285650
         BNE   *+8                                                      00285660
         OI    0(R1),RECFMM                                             00285670
SYNRECX  EQU   *                                                        00285680
         SPACE                                                          00285690
         CLI   LREKW+1,1           LRECL                                00285700
         BNE   NOLRE                                                    00285710
         LA    R15,LRE                                                  00285720
         L     R14,0(,R15)                                              00285730
         LH    R1,4(,R15)                                               00285740
         BCTR  R1,0                                                     00285750
         B     *+10                                                     00285760
         PACK  DOUBLE,0(0,R14)                                          00285770
         EX    R1,*-6                                                   00285780
         CVB   R1,DOUBLE                                                00285790
         STH   R1,RECSIZE                                               00285800
NOLRE    EQU   *                                                        00285810
         SPACE                                                          00285820
         CLI   BLKKW+1,1           BLKSIZE                              00285830
         BNE   NOBLK                                                    00285840
         LA    R15,BLK                                                  00285850
         L     R14,0(,R15)                                              00285860
         LH    R1,4(,R15)                                               00285870
         BCTR  R1,0                                                     00285880
         B     *+10                                                     00285890
         PACK  DOUBLE,0(0,R14)                                          00285900
         EX    R1,*-6                                                   00285910
         CVB   R1,DOUBLE                                                00285920
         STH   R1,BLOCKSI                                               00285930
NOBLK    EQU   *                                                        00285940
         SPACE                                                          00285950
************************************************************            00285960
*                                                          *            00285970
*        PREPARE DAIR PARAMETERS                           *            00285980
*                                                          *            00285990
************************************************************            00286000
         SPACE                                                          00286010
         LA    R1,MYDAPL                                                00286020
         USING DAPL,R1                                                  00286030
         MVC   DAPLUPT(4),CPPLUPT                                       00286040
         MVC   DAPLECT(4),CPPLECT                                       00286050
         LA    R0,MYECB                                                 00286060
         ST    R0,DAPLECB                                               00286070
         MVC   DAPLPSCB(4),CPPLPSCB                                     00286080
         LA    R7,MYDAPB                                                00286090
         ST    R7,DAPLDAPB                                              00286100
         DROP  R1                  DAPL                                 00286110
         SPACE                                                          00286120
************************************************************            00286130
*                                                          *            00286140
*        FREE THE FILENAME (DDNAME)                        *            00286150
*                                                          *            00286160
************************************************************            00286170
         SPACE                                                          00286180
         CLI   DDSAVE,C' '         PART OF CONCATENATION                00286190
         BE    FREE2               YES, BYPASS FREE                     00286200
         USING DAPB18,R7                                                00286210
         XC    0(40,R7),0(R7)                                           00286220
         MVI   DA18CD+1,X'18'                                           00286230
         MVC   DA18DDN,DDSAVE                                           00286240
         MVC   DA18MNM(8),=CL8' '                                       00286250
         MVC   DA18SCLS(2),=CL8' '                                      00286260
         OI    DA18CTL,DA18PERM                                         00286270
         LA    R1,MYDAPL                                                00286280
         BAL   R14,CALLDAIR        UNALLOCATE                           00286290
         DROP  R7                  DAPB18                               00286300
FREE2    EQU   *                                                        00286310
         SPACE                                                          00286320
************************************************************            00286330
*                                                          *            00286340
*         GET AN ATTRIBUTE LIST NAME IF SPECIFIED          *            00286350
*                                                          *            00286360
************************************************************            00286370
         SPACE                                                          00286380
         MVC   ATTRIB,=CL8' '                                           00286390
         LA    R15,ALN                                                  00286400
         TM    6(R15),X'80'        ATTRIB LIST NAME SPECIFIED?          00286410
         BZ    ALNX                NO - BRANCH                          00286420
         LH    R1,4(,R15)          GET LENGTH OF ATTRIB NAME            00286430
         BCTR  R1,0                MINUS 1 FOR EX                       00286440
         L     R14,0(,R15)         GET ADDRESS OF ATTRIB NAME           00286450
         B     *+10                                                     00286460
         MVC   ATTRIB(0),0(R14)    MOVE ATTRIB NAME                     00286470
         EX    R1,*-6                                                   00286480
ALNX     EQU   *                                                        00286490
         SPACE                                                          00286500
************************************************************            00286510
*                                                          *            00286520
*         ALLOCATE A TERMINAL IF SECOND OPERAND '*'        *            00286530
*                                                          *            00286540
************************************************************            00286550
         SPACE                                                          00286560
         LA    R6,DSN                                                   00286570
         L     R1,0(R6)            POINT TO DSNAME                      00286580
         CLI   0(R1),C'*'          ASTERISK                             00286590
         BNE   NOTTERM             NO, BRANCH                           00286600
         CLI   5(R6),1             LENGTH = 1                           00286610
         BNE   NOTTERM             NO, BRANCH                           00286620
         LA    R7,MYDAPB                                                00286630
         USING DAPB1C,R7                                                00286640
         MVC   DA1CCD(MODEL1CL),MODEL1C                                 00286650
         MVC   DA1CDDN,DDSAVE                                           00286660
         OI    DA1CCTL,DA1CPERM                                         00286670
         MVC   DA1CALN(8),ATTRIB   MOVE ATTRIB NAME                     00286680
         CLI   ATTRIB,C' '                                              00286690
         BE    *+8                                                      00286700
         OI    DA1CCTL,DA1CATRL    SET ATTRIB NAME INDICATOR ON         00286710
         SPACE                                                          00286720
         CLI   MSGKW+1,1           MSG                                  00286730
         BNE   *+8                 NO                                   00286740
         OI    DE1CCTLX,DE1CMSG    YES                                  00286750
         MVC   DE1CRECF,RECFORM    DCB RECFM                            00286760
         MVC   DE1CLREC,RECSIZE    DCB LRECL                            00286770
         MVC   DE1CBLKS,BLOCKSI    DCB BLKSIZE                          00286780
         SPACE                                                          00286790
         LA    R1,MYDAPL                                                00286800
         BAL   R14,CALLDAIR                                             00286810
         LTR   R15,R15                                                  00286820
         BNZ   ALLOFAIL                                                 00286830
         DROP  R7                  DAPB1C                               00286840
         B     STOP                                                     00286850
         SPACE                                                          00286860
************************************************************            00286870
*                                                          *            00286880
*         ALLOCATE A SYSOUT FILE                           *            00286890
*                                                          *            00286900
************************************************************            00286910
         SPACE                                                          00286920
NOTTERM  CLI   DATAKW+1,1          IS 'DATASET' SPECIFIED               00286930
         BE    NOTSYSO             YES, BYPASS SYSOUT                   00286940
         CLI   5(R6),6             IS LENGTH = 6                        00286950
         BNE   NOTSYSO             NO, BRANCH                           00286960
         CLC   0(6,R1),=C'SYSOUT'  IS IT SYSOUT                         00286970
         BNE   NOTSYSO             NO, BRANCH                           00286980
         SPACE                                                          00286990
         MVC   FORMS,=CL4' '       START WITH FORM BLANK                00287000
         LA    R15,FORM            POINT TO FORM PDE                    00287010
         TM    6(R15),X'80'        IS FORM(XXXX) SPECIFIED              00287020
         BZ    NOFORM              NO, BRANCH                           00287030
         L     R14,0(,R15)         YES, POINT TO OPERAND                00287040
         LH    R1,4(,R15)          GET LENGTH                           00287050
         BCTR  R1,0                LENGTH MINUS 1 FOR EX                00287060
         B     *+10                                                     00287070
         MVC   FORMS(0),0(R14)     (EXECUTED)                           00287080
         EX    R1,*-6              MOVE FORM                            00287090
NOFORM   EQU   *                                                        00287100
         SPACE                                                          00287110
         LA    R7,MYDAPB                                                00287120
         USING DAPB30,R7                                                00287130
         MVC   DA30CD(MODEL30L),MODEL30                                 00287140
         MVC   DA30DDN,DDSAVE                                           00287150
         OI    DA30CTL,DA30PERM                                         00287160
         TM    14(R6),X'80'        SYSOUT(CLASS) SPECIFIED              00287170
         BZ    *+14                NO, SKIP NEXT 2                      00287180
         L     R14,8(,R6)                                               00287190
         MVC   DA30OCLS(1),0(R14)  CLASS (IGNORED UNDER MVS)            00287200
         OC    DA30OCLS(1),SETCLASS RESET CLASS IF NECESSARY            00287210
         CLI   DA30OCLS,X'FC'      WAS IT 7C, FROM SYSOUT(0)            00287220
         BNE   *+8                                                      00287230
         MVI   DA30OCLS,C'0'       YES, MAKE IT SYSOUT(0)               00287240
         MVC   DA30FORM,FORMS                                           00287250
         MVC   DA30ALN(8),ATTRIB   MOVE ATTRIB NAME                     00287260
         CLI   ATTRIB,C' '                                              00287270
         BE    *+8                                                      00287280
         OI    DA30CTL,DA30ATRL    SET ATTRIB NAME INDICATOR ON         00287290
*               THE FOLLOWING OPTIONS ARE NOT PROVIDED BY IKJDAIR       00287300
*               BUT ARE PROVIDED BY THE 'S99DAIR' SUBPROGRAM.           00287310
         CLI   HOLDKW+1,1          'HOLD' SPECIFIED                     00287320
         BNE   *+8                 NO                                   00287330
         OI    DE30CTLX,DE30HOLD   YES                                  00287340
         SPACE                                                          00287350
         LA    R14,DEST            POINT TO DEST PDE                    00287360
         TM    6(R14),X'80'        IS DEST(ROUTE) SPECIFIED             00287370
         BZ    NO30DEST            NO, BRANCH                           00287380
         MVC   DE30DEST,=CL8' '    YES, PAD WITH BLANKS                 00287390
         LH    R1,4(,R14)          GET LENGTH                           00287400
         L     R14,0(,R14)         YES, POINT TO OPERAND                00287410
         BCTR  R1,0                LENGTH MINUS 1 FOR EX                00287420
         B     *+10                                                     00287430
         MVC   DE30DEST(0),0(R14)  (EXECUTED)                           00287440
         EX    R1,*-6              MOVE DEST                            00287450
NO30DEST EQU   *                                                        00287460
         SPACE                                                          00287470
         LA    R14,FCB             POINT TO FCB PDE                     00287480
         TM    6(R14),X'80'        IS FCB(NAME) SPECIFIED               00287490
         BZ    NO30FCB             NO, BRANCH                           00287500
         MVC   DE30FCB,=CL4' '     YES, PAD WITH BLANKS                 00287510
         LH    R1,4(,R14)          GET LENGTH                           00287520
         L     R14,0(,R14)         YES, POINT TO OPERAND                00287530
         BCTR  R1,0                LENGTH MINUS 1 FOR EX                00287540
         B     *+10                                                     00287550
         MVC   DE30FCB(0),0(R14)   (EXECUTED)                           00287560
         EX    R1,*-6              MOVE FCB                             00287570
NO30FCB  EQU   *                                                        00287580
         SPACE                                                          00287590
         LA    R14,COPI            POINT TO COPIES PDE                  00287600
         TM    6(R14),X'80'        IS COPIES(NN) SPECIFIED              00287610
         BZ    NO30CPYS            NO, BRANCH                           00287620
         LH    R1,4(,R14)          GET LENGTH                           00287630
         L     R14,0(,R14)         YES, POINT TO OPERAND                00287640
         BCTR  R1,0                LENGTH MINUS 1 FOR EX                00287650
         B     *+10                                                     00287660
         PACK  DOUBLE,0(0,R14)     (EXECUTED)                           00287670
         EX    R1,*-6              PACK COPIES                          00287680
         CVB   R1,DOUBLE                                                00287690
         STC   R1,DE30CPYS                                              00287700
NO30CPYS EQU   *                                                        00287710
         CLI   MSGKW+1,1           MSG                                  00287720
         BNE   *+8                 NO                                   00287730
         OI    DE30CTLX,DE30MSG    YES                                  00287740
         MVC   DE30RECF,RECFORM    DCB RECFM                            00287750
         MVC   DE30LREC,RECSIZE    DCB LRECL                            00287760
         MVC   DE30BLKS,BLOCKSI    DCB BLKSIZE                          00287770
         LA    R1,MYDAPL                                                00287780
         BAL   R14,CALLDAIR                                             00287790
         LTR   R15,R15             WAS IT SUCCESSFUL                    00287800
         BNZ   ALLOFAIL            NO, BRANCH                           00287810
         SPACE                                                          00287820
************************************************************            00287830
*                                                          *            00287840
*        DO OPEN-SETPRT-CLOSE FOR 'CHARS' AND 'BURST'      *            00287850
*                                                          *            00287860
************************************************************            00287870
         SPACE                                                          00287880
         CLI   CHARKW+1,1          CHARS SPECIFIED                      00287890
         BE    SETPRTGO            YES, GO ISSUE SETPRT                 00287900
         CLI   BURSTKW+1,0         BURST OR NOBURST SPECIFIED           00287910
         BE    STOP                NEITHER, BYPASS SETPRT               00287920
SETPRTGO LA    R1,SOUDCBW                                               00287930
         MVC   SOUDCBW(SOUDCBL),SOUDCB                                  00287940
         MVC   DDNAM(8,R1),DA30DDN                                      00287950
         DROP  R7                  DAPB30                               00287960
         MVC   SETPRTL(48),SETPRTM                                      00287970
         CLI   BURSTKW+1,0         'BURST' OR 'NOBURST'                 00287980
         BNE   *+8                 BRANCH IF EITHER                     00287990
BURSTDEF MVI   BURSTKW+1,2         SET DEFAULT (BURST 1, NOBURST 2)     00288000
         CLI   BURSTKW+1,1         'BURST'                              00288010
         BNE   *+8                                                      00288020
         OI    SETPRTL+16,X'80'    BURST=Y                              00288030
         CLI   BURSTKW+1,2         'NOBURST'                            00288040
         BNE   *+8                                                      00288050
         NI    SETPRTL+16,X'7F'    BURST=N                              00288060
         CLI   CHARKW+1,1                                               00288070
         BNE   CHARSX                                                   00288080
         LA    R7,CHARS            ADDRESS OF PARSE PDE FOR CHARS       00288090
         LA    R15,SETPRTL+32      WHERE TO PUT CHARS NAMES             00288100
         LA    R0,4                MAXIMUM OF 4 NAMES                   00288110
         B     *+8                 SKIP NEXT INSTRUCTION                00288120
CHARLOOP L     R7,8(,R7)           POINT TO NEXT PDE                    00288130
         L     R14,0(,R7)          GET ADDRESS OF NAME                  00288140
         MVC   0(4,R15),=CL4' '    PAD WITH BLANKS                      00288150
         LH    R1,4(,R7)           GET LENGTH OF NAME                   00288160
         BCTR  R1,0                LENGTH MINUS 1 FOR EX                00288170
         B     *+10                                                     00288180
         MVC   0(0,R15),0(R14)     (EXECUTED)                           00288190
         EX    R1,*-6              MOVE CHARS NAME TO SETPRT MF=L       00288200
         LA    R15,4(,R15)         POINT TO NEXT NAME SLOT              00288210
         CLI   8(R7),X'FF'         END OF LIST                          00288220
         BE    CHARSX              YES, BRANCH                          00288230
         BCT   R0,CHARLOOP         NO, BRANCH TO DO OTHERS              00288240
*              FOR NOW, IGNORE OTHERS IF MORE THAN 4                    00288250
CHARSX   EQU   *                                                        00288260
         LA    R1,SOUDCBW                                               00288270
         MVC   RECFM(1,R1),RECFORM                                      00288280
         MVC   LRECL(2,R1),RECSIZE                                      00288290
         MVC   RECFM(2,R1),BLOCKSI                                      00288300
         LR    R7,R1                                                    00288310
         MVI   OPEN,X'80'                                               00288320
         OPEN  ((R7),OUTPUT),MF=(E,OPEN)                                00288330
         L     R1,CPPLUPT                                               00288340
         MVC   SAVEUPT,UPTSWS-UPT(R1)     SAVE PROFILE WTPMSG           00288350
         NI    UPTSWS-UPT(R1),255-UPTWTP  SUPPRESS IEF288I              00288360
         SETPRT (R7),MF=(E,SETPRTL)                                     00288370
         ST    R15,SETPRTRC                                             00288380
         L     R1,CPPLUPT                                               00288390
         MVC   UPTSWS-UPT(1,R1),SAVEUPT   RESTORE PROFILE WTPMSG        00288400
         MVI   CLOSE,X'80'                                              00288410
         CLOSE ((R7)),MF=(E,CLOSE)                                      00288420
         L     R15,SETPRTRC                                             00288430
         LTR   R15,R15                                                  00288440
         BZ    STOP                                                     00288450
         CLI   SETPRTRC+3,X'18'    DEVICE NOT A 3800                    00288460
         BNE   SETPRTER            BRANCH IF SOME OTHER ERROR           00288470
         LA    R1,=C'OPERAND INVALID WITHOUT 3800 ENHANCEMENTS'         00288480
         LA    R0,41                                                    00288490
         B     SYNTAX                                                   00288500
SETPRTER MVC   MSGWK(L'SETPRTE),SETPRTE                                 00288510
         UNPK  MSGWK+20(9),SETPRTRC(5)                                  00288520
         TR    MSGWK+20(8),HEXTAB-240                                   00288530
         MVI   MSGWK+28,C''''                                           00288540
         LA    R1,MSGWK                                                 00288550
         LA    R0,L'SETPRTE                                             00288560
         B     SYNTAX                                                   00288570
         SPACE                                                          00288580
NOTSYSO  EQU   *                                                        00288590
         SPACE                                                          00288600
************************************************************            00288610
*                                                          *            00288620
*        GET THE SPACE VALUES                              *            00288630
*                                                          *            00288640
************************************************************            00288650
         SPACE                                                          00288660
         SR    R0,R0                                                    00288670
         ST    R0,PRIMARY                                               00288680
         ST    R0,SECDARY                                               00288690
         ST    R0,DIRBLKS                                               00288700
         ST    R0,BLOCKSP                                               00288710
         SPACE                                                          00288720
         CLI   SPACEKW+1,1                                              00288730
         BNE   NOSPACE                                                  00288740
         LA    R6,SPACE                                                 00288750
         L     R15,0(,R6)          ADDRESS OF NUMBER                    00288760
         LH    R1,4(,R6)           LENGTH OF NUMBER                     00288770
         BCTR  R1,0                                                     00288780
         B     *+10                                                     00288790
         PACK  DOUBLE,0(0,R15)                                          00288800
         EX    R1,*-6                                                   00288810
         CVB   R1,DOUBLE                                                00288820
         ST    R1,PRIMARY                                               00288830
         SPACE                                                          00288840
         CLI   8(R6),X'FF'         IS THERE A SECOND VALUE              00288850
         BE    NOSEC               NO, BRANCH                           00288860
         L     R6,8(,R6)           YES, POINT TO ITS PDE                00288870
         L     R15,0(,R6)          ADDRESS OF NUMBER                    00288880
         LH    R1,4(,R6)           LENGTH OF NUMBER                     00288890
         BCTR  R1,0                                                     00288900
         B     *+10                                                     00288910
         PACK  DOUBLE,0(0,R15)                                          00288920
         EX    R1,*-6                                                   00288930
         CVB   R1,DOUBLE                                                00288940
         ST    R1,SECDARY                                               00288950
NOSEC    EQU   *                                                        00288960
NOSPACE  EQU   *                                                        00288970
         SPACE                                                          00288980
         CLI   TYPKW+1,1           BLOCK(NN)                            00288990
         BNE   NOBLOCK                                                  00289000
         LA    R6,BLOCK                                                 00289010
         L     R15,0(,R6)          ADDRESS OF NUMBER                    00289020
         LH    R1,4(,R6)           LENGTH OF NUMBER                     00289030
         BCTR  R1,0                                                     00289040
         B     *+10                                                     00289050
         PACK  DOUBLE,0(0,R15)                                          00289060
         EX    R1,*-6                                                   00289070
         CVB   R1,DOUBLE                                                00289080
         ST    R1,BLOCKSP                                               00289090
NOBLOCK  EQU   *                                                        00289100
         SPACE                                                          00289110
         CLI   DIRKW+1,1                                                00289120
         BNE   NODIR                                                    00289130
         LA    R6,DIR                                                   00289140
         L     R15,0(,R6)                                               00289150
         LH    R1,4(,R6)                                                00289160
         BCTR  R1,0                                                     00289170
         B     *+10                                                     00289180
         PACK  DOUBLE,0(0,R15)                                          00289190
         EX    R1,*-6                                                   00289200
         CVB   R1,DOUBLE                                                00289210
         ST    R1,DIRBLKS                                               00289220
NODIR    EQU   *                                                        00289230
         SPACE                                                          00289240
************************************************************            00289250
*                                                          *            00289260
*        GET THE DATA SET NAME                             *            00289270
*                                                          *            00289280
************************************************************            00289290
         SPACE                                                          00289300
         LA    R1,CONCDAPB+12      LOCATION TO STORE FIRST DDNAME       00289310
         ST    R1,CONCPTR          SAVE DDNAME POINTER                  00289320
         LA    R6,DSN                                                   00289330
         B     *+8                                                      00289340
DSNLOOP  L     R6,24(,R6)                                               00289350
         XC    DSNAME(2),DSNAME                                         00289360
         MVI   DSNAME+2,C' '                                            00289370
         MVC   DSNAME+3(43),DSNAME+2                                    00289380
*        TM    6(R6),X'80'         IS DATASET NAME SPECIFIED?           00289390
*        BZ    SYNDSN02            NO, JUST MEMBER NAME                 00289400
         L     R1,0(,R6)           GET ADDRESS OF NAME                  00289410
*        CLI   TEMPKW+1,1          TEMP SPECIFIED                       00289420
*        BNE   NOTTEMP             NO, BRANCH                           00289430
         CLI   DATAKW+1,1          IS 'DATASET' SPECIFIED               00289440
         BE    NOTTEMP             YES, BYPASS $                        00289450
         CLI   0(R1),C'$'          DOES NAME BEGIN WITH $               00289460
         BE    TEMPDSN             YES, CHANGE IT TO TEMPORARY          00289470
NOTTEMP  LH    R14,4(,R6)          GET LENGTH                           00289480
         STH   R14,DSNAME                                               00289490
         BCTR  R14,0                                                    00289500
         B     *+10                                                     00289510
         MVC   DSNAME+2(0),0(R1)                                        00289520
         EX    R14,*-6                                                  00289530
         CLI   DATAKW+1,1          IS 'DATASET' SPECIFIED               00289540
         BE    GOTDSN              YES, BYPASS DUMMY                    00289550
         CLC   DSNAME+2(6),=C'DUMMY '                                   00289560
         BNE   GOTDSN                                                   00289570
         MVC   DSNAME(10),NULLFILE                                      00289580
         OI    6(R6),X'40'        DUMMY SAME AS QUOTED 'NULLFILE'       00289590
         B     GOTDSN                                                   00289600
         SPACE                                                          00289610
TEMPDSN  LH    R0,4(,R6)          GET LENGTH                            00289620
         MVI   DSNAME+2,AMP                                             00289630
         BCTR  R0,0               REDUCE LENGTH BY 1                    00289640
         LTR   R0,R0                                                    00289650
         BZ    TEMPQUAL           LEAVE DSN NULL (AND AMP IN DSNAME+2)  00289660
         CLI   1(R1),C'.'                                               00289670
         BNE   NOTTEMP                                                  00289680
         LA    R1,2(,R1)          POINT PAST PERIOD                     00289690
         BCTR  R0,0               REDUCE LENGTH BY 1                    00289700
         LTR   R0,R0              IS LENGTH ZERO                        00289710
         BZ    TEMPQUAL           NO, BRANCH (PARSE WONT ALLOW JUST $.) 00289720
TEMPREST CH    R0,=H'8'           IS LENGTH GREATER THAN 8              00289730
         BNH   *+8                NO, SKIP NEXT                         00289740
         LH    R0,=H'8'           TRUNCATE TO MAX 8                     00289750
         LA    R14,1              START WITH LENGTH 1 FOR AMPERSAND     00289760
         LA    R15,DSNAME+3       ADDRESS TO MOVE TO                    00289770
TEMPLOOP MVC   0(1,R15),0(R1)     MOVE 1 CHARACTER                      00289780
         LA    R1,1(,R1)                                                00289790
         LA    R15,1(,R15)                                              00289800
         LA    R14,1(,R14)        INCREMENT LENGTH                      00289810
         CLI   0(R1),C'.'                                               00289820
         BE    *+8                                                      00289830
         BCT   R0,TEMPLOOP                                              00289840
         STH   R14,DSNAME         STORE LENGTH                          00289850
TEMPQUAL OI    6(R6),X'40'        TREAT UNQUOTED LIKE QUOTED            00289860
GOTDSN   EQU   *                                                        00289870
         SPACE                                                          00289880
************************************************************            00289890
*                                                          *            00289900
*        ALLOCATE THE DATA SET TO THE FILENAME             *            00289910
*                                                          *            00289920
************************************************************            00289930
         SPACE                                                          00289940
         LA    R7,MYDAPB                                                00289950
         USING DAPB08,R7                                                00289960
         MVC   0(MODEL08L,R7),MODEL08                                   00289970
         LA    R0,DSNAME                                                00289980
         CLI   DSNAME+1,0                                               00289990
         BE    *+8                                                      00290000
         ST    R0,DA08PDSN                                              00290010
         MVC   DA08DDN(8),DDSAVE                                        00290020
         MVC   DA08PQTY,PRIMARY                                         00290030
         MVC   DA08SQTY,SECDARY                                         00290040
         MVC   DA08DQTY,DIRBLKS                                         00290050
         CLI   DISPKW+1,4          NEW                                  00290060
         BNE   SETDAPMD            NO, BRANCH                           00290070
         MVI   DA08DSP1,DA08NEW    YES, MARK IT NEW                     00290080
         MVI   DA08DPS2,DA08CAT                 CATLG                   00290090
         MVI   DA08DPS3,DA08CATL                CATLG                   00290100
SETDAPTY CLI   SPACEKW+1,1         SPACE SPECIFIED                      00290110
         BNE   *+8                 NO, NO DEFAULT TYPE                  00290120
         MVI   DA08CTL,DA08TRKS    YES, DEFAULT TRACKS                  00290130
         CLI   TYPKW+1,2           CYL SPECIFIED                        00290140
         BNE   *+8                                                      00290150
         MVI   DA08CTL,DA08CYLS                                         00290160
         CLI   TYPKW+1,1           BLOCK(NN) SPECIFIED                  00290170
         BNE   *+14                                                     00290180
         MVI   DA08CTL,DA08ABKL    AVERAGE BLOCK LENGTH                 00290190
         MVC   DA08BLK,BLOCKSP                                          00290200
         B     SETDAPCM                                                 00290210
SETDAPMD CLI   DISPKW+1,3          MOD                                  00290220
         BNE   SETDAPOS                                                 00290230
         MVI   DA08DSP1,DA08MOD    MOD                                  00290240
         MVI   DA08DPS2,DA08CAT                 CATLG                   00290250
         MVI   DA08DPS3,DA08CATL                CATLG                   00290260
         CLI   TYPKW+1,0           WAS TRK/CYL/BLK SPECIFIED            00290270
         BNE   SETDAPTY            YES, GO SET IT                       00290280
         CLI   SPACEKW+1,0         WAS SPACE SPECIFIED                  00290290
         BNE   SETDAPTY            YES, GO SET TRK                      00290300
         CLI   DIRKW+1,0           WAS DIR SPECIFIED                    00290310
         BNE   SETDAPTY            YES, GO SET TRK                      00290320
*        MVI   DA08DPS2,DA08KEEP                KEEP                    00290330
*        MVI   DA08DPS3,DA08KEP                 KEEP                    00290340
         B     SETDAPCM            NO, DO NOT SPECIFY TYPE              00290350
SETDAPOS MVI   DA08DSP1,DA08SHR                                         00290360
         CLI   DISPKW+1,2          OLD                                  00290370
         BNE   *+8                 NOT OLD, SKIP NEXT INSTR             00290380
         MVI   DA08DSP1,DA08OLD    OLD                                  00290390
         MVI   DA08DPS2,DA08KEEP                                        00290400
         MVI   DA08DPS3,DA08KEP                                         00290410
         SPACE                                                          00290420
SETDAPCM CLI   DSNAME+2,AMP        IS DSN TEMPORARY                     00290430
         BNE   *+12                NO                                   00290440
         MVI   DA08DPS2,DA08DEL    YES                                  00290450
         MVI   DA08DPS3,DA08DELE   YES                                  00290460
         SPACE                                                          00290470
         OI    DA08CTL,DA08PERM                                         00290480
         TM    6(R6),X'40'         IS DSN IN QUOTES?                    00290490
         BO    *+8                 IF IN QUOTES, SKIP NEXT INS          00290500
         OI    DA08CTL,DA08UID     DAIR TO ADD PREFIX                   00290510
         SPACE                                                          00290520
         TM    14(R6),X'80'        MEMBER SPECIFIED?                    00290530
         BZ    MEMBX               NO - BRANCH                          00290540
         LH    R1,12(,R6)          GET LENGTH OF MEMBER                 00290550
         BCTR  R1,0                MINUS 1 FOR EX                       00290560
         L     R14,8(,R6)          GET ADDRESS OF MEMBER NAME           00290570
         B     *+10                                                     00290580
         MVC   DA08MNM(0),0(R14)   MOVE MEMBER NAME                     00290590
         EX    R1,*-6                                                   00290600
MEMBX    EQU   *                                                        00290610
         TM    22(R6),X'80'        PASSWORD SPECIFIED?                  00290620
         BZ    PASSX               NO - BRANCH                          00290630
         LH    R1,20(,R6)          GET LENGTH OF PSWD                   00290640
         BCTR  R1,0                MINUS 1 FOR EX                       00290650
         L     R14,16(,R6)         GET ADDRESS OF PSWD                  00290660
         B     *+10                                                     00290670
         MVC   DA08PSWD(0),0(R14)  MOVE PSWD                            00290680
         EX    R1,*-6                                                   00290690
PASSX    EQU   *                                                        00290700
         LA    R15,UNIT                                                 00290710
         TM    6(R15),X'80'        UNIT SPECIFIED?                      00290720
         BZ    UNITX               NO - BRANCH                          00290730
         LH    R1,4(,R15)          GET LENGTH OF UNIT                   00290740
         BCTR  R1,0                MINUS 1 FOR EX                       00290750
         L     R14,0(,R15)         GET ADDRESS OF UNIT NAME             00290760
         B     *+10                                                     00290770
         MVC   DA08UNIT(0),0(R14)  MOVE UNIT NAME                       00290780
         EX    R1,*-6                                                   00290790
UNITX    EQU   *                                                        00290800
         LA    R15,VOL                                                  00290810
         TM    6(R15),X'80'        VOLUME SPECIFIED?                    00290820
         BZ    VOLX                NO - BRANCH                          00290830
         LH    R1,4(,R15)          GET LENGTH OF VOLUME NAME            00290840
         BCTR  R1,0                MINUS 1 FOR EX                       00290850
         L     R14,0(,R15)         GET ADDRESS OF VOLUME NAME           00290860
         B     *+10                                                     00290870
         MVC   DA08SER(0),0(R14)   MOVE VOLUME NAME                     00290880
         EX    R1,*-6                                                   00290890
VOLX     EQU   *                                                        00290900
         SPACE                                                          00290910
         MVC   DA08ALN(8),ATTRIB   MOVE ATTRIB NAME                     00290920
         CLI   ATTRIB,C' '                                              00290930
         BE    *+8                                                      00290940
         OI    DA08CTL,DA08ATRL    SET ATTRIB NAME INDICATOR ON         00290950
         SPACE                                                          00290960
         CLC   DSNAME(10),NULLFILE IS DSN NULLFILE                      00290970
         BNE   DUMX                                                     00290980
         TM    6(R6),X'40'         IS DSN IN QUOTES?                    00290990
         BZ    DUMX                NO, ITS NOT NECESSARILY DUMMY        00291000
         OI    DA08CTL,DA08DMMY                                         00291010
DUMX     EQU   *                                                        00291020
         SPACE                                                          00291030
         CLI   RLSEKW+1,1          RLSE                                 00291040
         BNE   *+8                                                      00291050
         OI    DA08CTL,DA08RLSE                                         00291060
         SPACE                                                          00291070
         TM    STATUS,STATCON      IS THIS PART OF A CONCATENATION      00291080
         BZ    *+8                 NO                                   00291090
         OI    DE08CTLX,DE08NEX    YES, DO NOT USE EXISTING ALLOCATION  00291100
         SPACE                                                          00291110
         CLI   MSGKW+1,1           MSG                                  00291120
         BNE   *+8                 NO                                   00291130
         OI    DE08CTLX,DE08MSG    YES                                  00291140
         SPACE                                                          00291150
         MVC   DE08DSOR,DSORGVAL   DCB DSORG                            00291160
         MVC   DE08RECF,RECFORM    DCB RECFM                            00291170
         MVC   DE08LREC,RECSIZE    DCB LRECL                            00291180
         MVC   DE08BLKS,BLOCKSI    DCB BLKSIZE                          00291190
         SPACE                                                          00291200
         LA    R15,MSV                                                  00291210
         TM    6(R15),X'80'        MSVGP SPECIFIED?                     00291220
         BZ    MSVX                NO - BRANCH                          00291230
         LH    R1,4(,R15)          GET LENGTH OF MSVGP NAME             00291240
         BCTR  R1,0                MINUS 1 FOR EX                       00291250
         L     R14,0(,R15)         GET ADDRESS OF MSVGP NAME            00291260
         B     *+10                                                     00291270
         MVC   DA08SER(0),0(R14)   USE VOL SER FIELD FOR NAME           00291280
         EX    R1,*-6                                                   00291290
         OI    DE08CTLX,DE08MSVG   INDICATE MSVGP PRESENT               00291300
MSVX     EQU   *                                                        00291310
         SPACE                                                          00291320
         LA    R15,POS                                                  00291330
         TM    6(R15),X'80'        POS SPECIFIED?                       00291340
         BZ    POSX                NO - BRANCH                          00291350
         LH    R1,4(,R15)          GET LENGTH OF POS                    00291360
         BCTR  R1,0                MINUS 1 FOR EX                       00291370
         L     R14,0(,R15)         GET ADDRESS OF POS                   00291380
         B     *+10                                                     00291390
         PACK  DOUBLE(8),0(0,R14)  PACK POS                             00291400
         EX    R1,*-6                                                   00291410
         CVB   R1,DOUBLE                                                00291420
         STH   R1,DE08POS          TAPE DATA SET SEQUENCE NUMBER        00291430
POSX     EQU   *                                                        00291440
         SPACE                                                          00291450
         LH    R15,LAB             GET KEYWORD NUMBER (0 - 8)           00291460
         LA    R15,LABTYPE(R15)    POINT TO LABEL TYPE IN TABLE         00291470
         MVC   DE08LABL,0(R15)     TAPE LABEL TYPE                      00291480
         SPACE                                                          00291490
         CLI   DSNAME+2,AMP        IS DATA SET TEMPORARY                00291500
         BE    EXPX                YES, IGNORE EXPDT                    00291510
         LA    R15,EXP                                                  00291520
         TM    6(R15),X'80'        EXPDT SPECIFIED?                     00291530
         BZ    EXPX                NO - BRANCH                          00291540
         LH    R1,4(,R15)          GET LENGTH OF EXPDT                  00291550
         BCTR  R1,0                MINUS 1 FOR EX                       00291560
         L     R14,0(,R15)         GET ADDRESS OF EXPDT                 00291570
         B     *+10                                                     00291580
         PACK  DOUBLE(8),0(0,R14)  PACK EXPDT                           00291590
         EX    R1,*-6                                                   00291600
         CVB   R1,DOUBLE                                                00291610
         SR    R0,R0                                                    00291620
         D     R0,=F'1000'         GET YY IN R1, DDD IN R0              00291630
         STC   R1,DE08EXYR                                              00291640
         STH   R0,DE08EXJD                                              00291650
EXPX     EQU   *                                                        00291660
         SPACE                                                          00291670
         LA    R1,MYDAPL                                                00291680
         BAL   R14,CALLDAIR                                             00291690
         LTR   R15,R15                                                  00291700
         BNZ   ALLOFAIL                                                 00291710
         L     R1,CONCPTR          POINT TO LIST OF DDNAMES             00291720
         MVC   0(8,R1),DA08DDN     SAVE THE DDNAME                      00291730
         MVC   DDSAVE,=CL8' '      BLANK THE DDNAME FOR NEXT DAIR       00291740
         LA    R1,8(,R1)           POINT TO NEXT SLOT                   00291750
         ST    R1,CONCPTR          SAVE UPDATED POINTER                 00291760
         DROP  R7                  DAPB08                               00291770
         CLI   24(R6),X'FF'        IS THERE ANOTHER DSNAME              00291780
         BNE   DSNLOOP             YES, BRANCH                          00291790
         SPACE                                                          00291800
************************************************************            00291810
*                                                          *            00291820
*         CONCATENATE IF THERE WAS MORE THAN ONE           *            00291830
*                                                          *            00291840
************************************************************            00291850
         SPACE                                                          00291860
         LA    R7,CONCDAPB                                              00291870
         USING DAPB0C,R7                                                00291880
         XC    0(12,R7),0(R7)                                           00291890
         MVI   DA0CCD+1,X'0C'                                           00291900
         LA    R0,12(,R7)          BEGINNING OF DDNAME LIST             00291910
         L     R1,CONCPTR          END OF DDNAME LIST                   00291920
         SR    R1,R0               LENGTH OF DDNAME LIST                00291930
         SRL   R1,3                DIVIDE BY 8                          00291940
         CH    R1,=H'1'            IS THERE ONLY ONE DDNAME             00291950
         BE    NOCONCAT            YES, BYPASS CONCAT                   00291960
         STH   R1,DA0CNUMB                                              00291970
         LA    R1,MYDAPL                                                00291980
         USING DAPL,R1                                                  00291990
         ST    R7,DAPLDAPB                                              00292000
         DROP  R1                  DAPL                                 00292010
         DROP  R7                  DAPB0C                               00292020
         BAL   R14,CALLDAIR                                             00292030
         LTR   R15,R15                                                  00292040
         BZ    STOP                                                     00292050
ALLOFAIL BAL   R14,DAIRFAIL                                             00292060
         LA    R15,12                                                   00292070
         B     STOP                                                     00292080
NOCONCAT SLR   R15,R15                                                  00292090
STOP     STH   R15,RC              SET HIGHEST RC                       00292100
         SPACE                                                          00292110
************************************************************            00292120
*                                                          *            00292130
*         CLEAN UP                                         *            00292140
*                                                          *            00292150
************************************************************            00292160
         SPACE                                                          00292170
         IKJRLSA MYANS                                                  00292180
         SPACE                                                          00292190
         CLI   RC+1,0              IS RC ZERO?                          00292200
         BZ    STACKDX             YES, BRANCH                          00292210
         MVC   MYSTPB(STACKDL),STACKD                                   00292220
         SPACE                                                          00292230
         STACK DELETE=ALL,PARM=MYSTPB,MF=(E,MYIOPL)                     00292240
         SPACE                                                          00292250
         TCLEARQ                                                        00292260
STACKDX  EQU   *                                                        00292270
         SPACE                                                          00292280
         LH    R15,RC                                                   00292290
         B     EXIT                                                     00292300
         SPACE                                                          00292310
************************************************************            00292320
*                                                          *            00292330
*         CALL IKJDAIR SERVICE ROUTINE                     *            00292340
*                                                          *            00292350
************************************************************            00292360
          SPACE                                                         00292370
CALLDAIR ST    R14,DAIRREGS                                             00292380
         CLI   IKJKW+1,1                                                00292390
         BE    CALLIKJ                                                  00292400
*        LINK  EP=S99DAIR                                               00292410
         CALL  S99DAIR                                                  00292420
         L     R14,DAIRREGS                                             00292430
         BR    R14                                                      00292440
         SPACE                                                          00292450
CALLIKJ  EQU   *                                                        00292460
         AIF   (NOT &MVS).SKIP6                                         00292470
         L     R15,16                                                   00292480
         TM    X'2DC'(R15),X'80'  CVTDAIR                               00292490
         BNO   DAIRLINK                                                 00292500
         L     R15,X'2DC'(,R15)                                         00292510
         BALR  R14,R15                                                  00292520
         B     DAIRFINI                                                 00292530
DAIRLINK EQU   *                                                        00292540
.SKIP6   ANOP                                                           00292550
         LINK  EP=IKJDAIR,SF=(E,LINKAREA)                               00292560
DAIRFINI L     R14,DAIRREGS                                             00292570
         BR    R14                                                      00292580
         SPACE                                                          00292590
************************************************************            00292600
*                                                          *            00292610
*        DYNAMIC ALLOCATION FAILURE ROUTINE                *            00292620
*                                                          *            00292630
************************************************************            00292640
         SPACE                                                          00292650
DAIRFAIL ST    R14,MYDFREGS                                             00292660
         AIF   (NOT &MVS).SKIP7                                         00292670
         LA    R1,MYDFPARM                                              00292680
*        USING DFDSECTD,R1         MAPPED BY IKJEFFDF DFDSECT=YES       00292690
         ST    R15,MYDFRC                                               00292700
         LA    R15,MYDFRC                                               00292710
         ST    R15,4(,R1)          DFRCP                                00292720
         LA    R15,MYDAPL                                               00292730
         ST    R15,0(,R1)          DFDAPLP                              00292740
         SLR   R15,R15                                                  00292750
         ST    R15,MYJEFF02                                             00292760
         LA    R15,MYJEFF02                                             00292770
         ST    R15,8(,R1)          DFJEFF02                             00292780
         LA    R15,1               DFDAIR                               00292790
         STH   R15,MYDFID                                               00292800
         LA    R15,MYDFID                                               00292810
         ST    R15,12(,R1)         DFIDP                                00292820
         SLR   R15,R15                                                  00292830
         ST    R15,16(,R1)         DFCPPLP (MVS ONLY)                   00292840
         LINK  EP=IKJEFF18,SF=(E,LINKAREA)                              00292850
         L     R15,MYDFRC                                               00292860
*        DROP  R1                  DFDSECTD                             00292870
.SKIP7   ANOP                                                           00292880
         AIF   (&MVS).SKIP8                                             00292890
         LA    R1,MSGDAIR                                               00292900
         LA    R0,L'MSGDAIR                                             00292910
         BAL   R14,PUTMSG                                               00292920
.SKIP8   ANOP                                                           00292930
         L     R14,MYDFREGS                                             00292940
         BR    R14                                                      00292950
         SPACE                                                          00292960
************************************************************            00292970
*                                                          *            00292980
*        PUTMSG ROUTINE                                    *            00292990
*                                                          *            00293000
************************************************************            00293010
         SPACE                                                          00293020
PUTMSG   STM   R14,R1,PUTLINS                                           00293030
         XC    MYOLD(8),MYOLD                                           00293040
         XC    MYSEG1(4),MYSEG1                                         00293050
         MVC   MYPTPB(12),MODLPTPM                                      00293060
         LA    R14,1               NO. OF MESSAGE SEGMENTS              00293070
         ST    R14,MYOLD                                                00293080
         LA    R14,MYSEG1          POINT TO 1ST SEGMENT                 00293090
         ST    R14,MYOLD+4                                              00293100
         LR    R14,R0              LENGTH IN R0                         00293110
         LA    R14,4(,R14)         ADD 4                                00293120
         LA    R15,MYSEG1+4                                             00293130
         CLC   0(3,R1),=C'IKJ'     IS F POSOTECEEDED BY MESSAGE ID?     00293140
         BE    *+16                YES - BRANCH                         00293150
         LA    R14,1(,R14)         ADD 1 TO LENGTH                      00293160
         MVI   0(R15),C' '         INSERT LEADING BLANK                 00293170
         LA    R15,1(,R15)         BUMP POINTER                         00293180
         STH   R14,MYSEG1                                               00293190
         LR    R14,R0                                                   00293200
         BCTR  R14,0                                                    00293210
         B     *+10                                                     00293220
         MVC   0(0,R15),0(R1)      MOVE MESSAGE IN                      00293230
         EX    R14,*-6                                                  00293240
         L     R15,MYPUTLEP                                             00293250
         SPACE                                                          00293260
         PUTLINE PARM=MYPTPB,OUTPUT=(MYOLD),MF=(E,MYIOPL)               00293270
         SPACE                                                          00293280
         LM    R14,R1,PUTLINS                                           00293290
         BR    R14                                                      00293300
         SPACE                                                          00293310
         SPACE                                                          00293320
************************************************************            00293330
*                                                          *            00293340
*         RETURN                                           *            00293350
*                                                          *            00293360
************************************************************            00293370
         SPACE                                                          00293380
EXIT     LR    1,13                                                     00293390
         L     R0,@SIZE                                                 00293400
         L     13,4(,13)                                                00293410
         ST    15,16(,13)                                               00293420
         FREEMAIN R,A=(1),LV=(0)                                        00293430
         LM    14,12,12(13)                                             00293440
         BR    14                                                       00293450
         SPACE                                                          00293460
************************************************************            00293470
*                                                          *            00293480
*         CONSTANTS                                        *            00293490
*                                                          *            00293500
************************************************************            00293510
         SPACE                                                          00293520
         LTORG                                                          00293530
RECFM    EQU   36                                                       00293540
DDNAM    EQU   40                                                       00293550
BLKSI    EQU   62                                                       00293560
LRECL    EQU   82                                                       00293570
AMP      EQU   C'&&'                                                    00293580
NULLFILE DC    H'8',CL8'NULLFILE'                                       00293590
MODLPTPM PUTLINE OUTPUT=(1,TERM,SINGLE,INFOR),                         X00293600
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),MF=L                  00293610
         SPACE                                                          00293620
STACKD   STACK DELETE=ALL,MF=L                                          00293630
STACKDL  EQU   *-STACKD                                                 00293640
         SPACE                                                          00293650
MSG01    DC    C'ERROR IN PARSE SERVICE ROUTINE'                        00293660
MSG05    DC    C'IKJ58509I DATA SET NAME REQUIRED WITH MEMBER NAME'     00293670
MSGDAIR  DC    C'UNABLE TO ALLOCATE'                                    00293680
MODEL08  DC    AL2(8),XL10'0',CL24' ',XL16'0',CL16' ',XL8'0',CL8' '     00293690
         DC    XL16'0'        (LAST 16 BYTES FOR S99DAIR)               00293700
MODEL08L EQU   *-MODEL08                                                00293710
MODEL18  DC    X'0018',XL10'0',CL18' ',XL2'0',CL8' '                    00293720
MODEL18L EQU   *-MODEL18                                                00293730
MODEL1C  DC    X'001C',XL6'0',CL16' ',XL8'0' (LAST 8 FOR S99DAIR)       00293740
MODEL1CL EQU   *-MODEL1C                                                00293750
MODEL30  DC    X'0030',XL10'0',CL24' ',XL12'0',CL14' ',XL2'0',CL8' '    00293760
         DC    XL12'0',CL12' '       (LAST 24 BYTES FOR S99DAIR)        00293770
MODEL30L EQU   *-MODEL30                                                00293780
         SPACE                                                          00293790
RECFMU   EQU   X'C0'    UNDEFINED LENGTH                                00293800
RECFMF   EQU   X'80'    FIXED LENGTH                                    00293810
RECFMV   EQU   X'40'    VARIABLE LENGTH                                 00293820
RECFMT   EQU   X'20'    TRACK OVERFLOW                                  00293830
RECFMB   EQU   X'10'    BLOCKED                                         00293840
RECFMS   EQU   X'08'    VARIABLE SPANNED OR FIXED STANDARD              00293850
RECFMA   EQU   X'04'    ASA CC                                          00293860
RECFMM   EQU   X'02'    MACHINE CC                                      00293870
         SPACE                                                          00293880
DSORGTAB DC    H'0'     MUST BE IN SAME ORDER AS PARSE IKJNAME'S        00293890
         DC    X'4000'  PS                                              00293900
         DC    X'4100'  PSU                                             00293910
         DC    X'0200'  PO                                              00293920
         DC    X'0300'  POU                                             00293930
         DC    X'2000'  DA                                              00293940
         DC    X'2100'  DAU                                             00293950
         SPACE                                                          00293960
NL       EQU   X'01'    TAPE LABEL TYPE, NO LABELS                      00293970
SL       EQU   X'02'    STANDARD                                        00293980
NSL      EQU   X'04'    NONSTANDARD                                     00293990
SUL      EQU   X'0A'    STANDARD AND USER                               00294000
BLP      EQU   X'10'    BYPASS LABEL PROCESSING                         00294010
LTM      EQU   X'21'    DOS LEADING TAPE MARK                           00294020
AL       EQU   X'40'    ANSI                                            00294030
AUL      EQU   X'48'    ANSI AND USER                                   00294040
         SPACE                                                          00294050
LABTYPE  DC    AL1(0,NL,SL,NSL,SUL,BLP,LTM,AL,AUL)                      00294060
         DC    0D'0'                                                    00294070
         PRINT NOGEN                                                    00294080
SOUDCB   DCB   DDNAME=PRT3800,DSORG=PS,MACRF=PM                         00294090
SOUDCBL  EQU   *-SOUDCB                                                 00294100
         PRINT GEN                                                      00294110
SETPRTM  SETPRT 7,MF=L                                                  00294120
SETPRTE  DC    CL29'SETPRT ERROR CODE X''XXXXXXXX'''                    00294130
HEXTAB   DC    C'0123456789ABCDEF'                                      00294140
         DC    0D'0'                                                    00294150
         SPACE                                                          00294160
************************************************************            00294170
*                                                          *            00294180
*        PARSE PARAMETERS                                  *            00294190
*                                                          *            00294200
************************************************************            00294210
         SPACE                                                          00294220
         PRINT NOGEN                                                    00294230
DDPCL    IKJPARM                                                        00294240
DDN      IKJIDENT 'FILENAME (DDNAME)',                                 +00294250
               FIRST=ALPHA,OTHER=ALPHANUM,MAXLNTH=8,                   +00294260
               PROMPT='FILENAME (DDNAME)'                               00294270
DSN      IKJPOSIT DSTHING,LIST,PROMPT='DATA SET NAME'                   00294280
DATAKW   IKJKEYWD                                                       00294290
         IKJNAME 'DATASET'                                              00294300
UNITKW   IKJKEYWD                                                       00294310
         IKJNAME 'UNIT',SUBFLD=UNITSF                                   00294320
VOLKW    IKJKEYWD                                                       00294330
         IKJNAME 'VOLUME',SUBFLD=VOLSF                                  00294340
DISPKW   IKJKEYWD                                                       00294350
         IKJNAME 'SHR'                                                  00294360
         IKJNAME 'OLD'                                                  00294370
         IKJNAME 'MOD'                                                  00294380
         IKJNAME 'NEW'                                                  00294390
ALNKW    IKJKEYWD                                                       00294400
         IKJNAME 'USING',SUBFLD=ALNSF                                   00294410
SPACEKW  IKJKEYWD                                                       00294420
         IKJNAME 'SPACE',SUBFLD=SPACESF                                 00294430
TYPKW    IKJKEYWD                                                       00294440
         IKJNAME 'BLOCK',SUBFLD=BLOCKSF                                 00294450
         IKJNAME 'CYLINDERS'                                            00294460
         IKJNAME 'TR'                                                   00294470
         IKJNAME 'TRACKS'                                               00294480
         IKJNAME 'TRKS'                                                 00294490
DIRKW    IKJKEYWD                                                       00294500
         IKJNAME 'DIR',SUBFLD=DIRSF                                     00294510
RLSEKW   IKJKEYWD                                                       00294520
         IKJNAME 'RLSE'                                                 00294530
DSOKW    IKJKEYWD                                                       00294540
         IKJNAME 'DSORG',SUBFLD=DSOSF                                   00294550
RECKW    IKJKEYWD                                                       00294560
         IKJNAME 'RECFM',SUBFLD=RECSF                                   00294570
LREKW    IKJKEYWD                                                       00294580
         IKJNAME 'LRECL',SUBFLD=LRESF                                   00294590
BLKKW    IKJKEYWD                                                       00294600
         IKJNAME 'BLKSIZE',SUBFLD=BLKSF                                 00294610
MSVKW    IKJKEYWD                                                       00294620
         IKJNAME 'MSVGP',SUBFLD=MSVSF                                   00294630
TEMPKW   IKJKEYWD                                                       00294640
         IKJNAME 'TEMPORARY'                                            00294650
HOLDKW   IKJKEYWD                                                       00294660
         IKJNAME 'HOLD'                                                 00294670
DESTKW   IKJKEYWD                                                       00294680
         IKJNAME 'DEST',SUBFLD=DESTSF                                   00294690
FORMKW   IKJKEYWD                                                       00294700
         IKJNAME 'FORM',SUBFLD=FORMSF                                   00294710
FCBKW    IKJKEYWD                                                       00294720
         IKJNAME 'FCB',SUBFLD=FCBSF                                     00294730
COPIKW   IKJKEYWD                                                       00294740
         IKJNAME 'COPIES',SUBFLD=COPISF                                 00294750
LABKW    IKJKEYWD                                                       00294760
         IKJNAME 'LABEL',SUBFLD=LABSF                                   00294770
EXPKW    IKJKEYWD                                                       00294780
         IKJNAME 'EXPDT',SUBFLD=EXPSF                                   00294790
POSKW    IKJKEYWD                                                       00294800
         IKJNAME 'POSITION',SUBFLD=POSSF                                00294810
CHARKW   IKJKEYWD                                                       00294820
         IKJNAME 'CHARS',SUBFLD=CHARSF                                  00294830
BURSTKW  IKJKEYWD                                                       00294840
         IKJNAME 'BURST'                                                00294850
         IKJNAME 'NOBURST'                                              00294860
IKJKW    IKJKEYWD                                                       00294870
         IKJNAME '$IKJDAIR'                                             00294880
MSGKW    IKJKEYWD                                                       00294890
         IKJNAME '$MSG'                                                 00294900
UNITSF   IKJSUBF                                                        00294910
UNIT     IKJIDENT 'UNIT NAME',                                         +00294920
               FIRST=ALPHANUM,OTHER=ANY,MAXLNTH=8,                     +00294930
               PROMPT='UNIT NAME'                                       00294940
VOLSF    IKJSUBF                                                        00294950
VOL      IKJIDENT 'VOLUME SERIAL',                                     +00294960
               FIRST=ALPHANUM,OTHER=ALPHANUM,MAXLNTH=6,                +00294970
               PROMPT='VOLUME SERIAL'                                   00294980
ALNSF    IKJSUBF                                                        00294990
ALN      IKJIDENT 'ATTRIBUTE LIST NAME',                               +00295000
               FIRST=ALPHA,OTHER=ALPHANUM,MAXLNTH=8,                   +00295010
               PROMPT='ATTRIBUTE LIST NAME'                             00295020
SPACESF  IKJSUBF                                                        00295030
SPACE    IKJIDENT 'SPACE VALUE',LIST,                                  +00295040
               FIRST=NUMERIC,OTHER=NUMERIC,MAXLNTH=5,                  +00295050
               PROMPT='SPACE VALUE'                                     00295060
BLOCKSF  IKJSUBF                                                        00295070
BLOCK    IKJIDENT 'BLOCK SIZE',                                        +00295080
               FIRST=NUMERIC,OTHER=NUMERIC,MAXLNTH=5,                  +00295090
               PROMPT='BLOCK SIZE'                                      00295100
DIRSF    IKJSUBF                                                        00295110
DIR      IKJIDENT 'DIRECTORY VALUE',                                   +00295120
               FIRST=NUMERIC,OTHER=NUMERIC,MAXLNTH=5,                  +00295130
               PROMPT='NUMBER OF DIRECTORY BLOCKS'                      00295140
DSOSF    IKJSUBF                                                        00295150
DSOSFKW  IKJKEYWD                                                       00295160
         IKJNAME 'PS'                                                   00295170
         IKJNAME 'PSU'                                                  00295180
         IKJNAME 'PO'                                                   00295190
         IKJNAME 'POU'                                                  00295200
         IKJNAME 'DA'                                                   00295210
         IKJNAME 'DAU'                                                  00295220
RECSF    IKJSUBF                                                        00295230
RECKWF   IKJKEYWD                                                       00295240
         IKJNAME 'F'                                                    00295250
         IKJNAME 'V'                                                    00295260
         IKJNAME 'U'                                                    00295270
RECKWB   IKJKEYWD                                                       00295280
         IKJNAME 'B'                                                    00295290
RECKWS   IKJKEYWD                                                       00295300
         IKJNAME 'S'                                                    00295310
RECKWA   IKJKEYWD                                                       00295320
         IKJNAME 'A'                                                    00295330
         IKJNAME 'M'                                                    00295340
LRESF    IKJSUBF                                                        00295350
LRE      IKJIDENT 'LRECL',                                             +00295360
               FIRST=NUMERIC,OTHER=NUMERIC,MAXLNTH=5,                  +00295370
               PROMPT='DCB LRECL',VALIDCK=LREVC                         00295380
BLKSF    IKJSUBF                                                        00295390
BLK      IKJIDENT 'BLKSIZE',                                           +00295400
               FIRST=NUMERIC,OTHER=NUMERIC,MAXLNTH=5,                  +00295410
               PROMPT='DCB BLKSIZE',VALIDCK=LREVC                       00295420
MSVSF    IKJSUBF                                                        00295430
MSV      IKJIDENT 'MSVGP',                                             +00295440
               FIRST=ALPHANUM,OTHER=ALPHANUM,MAXLNTH=8,                +00295450
               PROMPT='MASS STORAGE GROUP'                              00295460
DESTSF   IKJSUBF                                                        00295470
DEST     IKJIDENT 'DESTINATION',                                       +00295480
               FIRST=ALPHA,OTHER=ALPHANUM,MAXLNTH=8,                   +00295490
               PROMPT='DESTINATION NAME'                                00295500
FORMSF   IKJSUBF                                                        00295510
FORM     IKJIDENT 'FORM NUMBER',                                       +00295520
               FIRST=ALPHANUM,OTHER=ALPHANUM,MAXLNTH=4,                +00295530
               PROMPT='FORM NUMBER'                                     00295540
FCBSF    IKJSUBF                                                        00295550
FCB      IKJIDENT 'FCB',                                               +00295560
               FIRST=ALPHANUM,OTHER=ALPHANUM,MAXLNTH=4,                +00295570
               PROMPT='FCB IMAGE'                                       00295580
COPISF   IKJSUBF                                                        00295590
COPI     IKJIDENT 'SYSOUT COPIES',                                     +00295600
               FIRST=NUMERIC,OTHER=NUMERIC,MAXLNTH=2,                  +00295610
               PROMPT='SYSOUT COPIES'                                   00295620
LABSF    IKJSUBF                                                        00295630
LAB      IKJKEYWD                                                       00295640
         IKJNAME 'NL'                                                   00295650
         IKJNAME 'SL'                                                   00295660
         IKJNAME 'NSL'                                                  00295670
         IKJNAME 'SUL'                                                  00295680
         IKJNAME 'BLP'                                                  00295690
         IKJNAME 'LTM'                                                  00295700
         IKJNAME 'AL'                                                   00295710
         IKJNAME 'AUL'                                                  00295720
EXPSF    IKJSUBF                                                        00295730
EXP      IKJIDENT 'EXPIRATION DATE',                                   +00295740
               FIRST=NUMERIC,OTHER=NUMERIC,MAXLNTH=5,                  +00295750
               PROMPT='EXPIRATION DATE'                                 00295760
POSSF    IKJSUBF                                                        00295770
POS      IKJIDENT 'POSITION',                                          +00295780
               FIRST=NUMERIC,OTHER=NUMERIC,MAXLNTH=4,                  +00295790
               PROMPT='DATA SET SEQUENCE NUMBER',                      +00295800
               HELP='RELATIVE POSITION OF DATA SET'                     00295810
CHARSF   IKJSUBF                                                        00295820
CHARS    IKJIDENT 'CHARACTER SET NAME',LIST,                           +00295830
               FIRST=ALPHA,OTHER=ALPHANUM,MAXLNTH=4,                   +00295840
               PROMPT='CHARACTER SET NAME FOR 3800 PRINTER'             00295850
         IKJENDP                                                        00295860
         PRINT GEN                                                      00295870
         SPACE                                                          00295880
************************************************************            00295890
*                                                          *            00295900
*         VALIDITY CHECK ROUTINES FOR PARSE                *            00295910
*                                                          *            00295920
************************************************************            00295930
         SPACE                                                          00295940
         DC    0D'0'                                                    00295950
LREVC    STM   14,12,12(13)                                             00295960
         L     R2,0(,R1)           POINT R2 TO PDE                      00295970
         L     R9,4(,R1)           POINT TO WORK AREA                   00295980
         LM    R10,R11,BASES       RESTORE BASE REGISTERS               00295990
         SR    15,15               SET RC INITIALLY TO ZERO             00296000
         L     R14,0(,R2)          POINT TO OPERAND                     00296010
         LH    R1,4(,R2)           LENGTH                               00296020
         BCTR  R1,0                LENGTH MINUS 1 FOR EX                00296030
         B     *+10                BRANCH AROUND PACK                   00296040
         PACK  DOUBLE,0(0,R14)     (EXECUTED)                           00296050
         EX    R1,*-6              PACK THE OPERAND                     00296060
         CVB   R1,DOUBLE           CONVERT TO BINARY                    00296070
         LTR   R1,R1               IS IT ZERO                           00296080
         BZ    LREVCX4             YES, ERROR                           00296090
         C     R1,=F'32760'        IS IT GREATER THAN 32760             00296100
         BNH   LREVCX              NO, BRANCH                           00296110
LREVCX4  LA    R15,4               RETURN CODE 4 FOR INVALID OPERAND    00296120
LREVCX   L     14,12(,13)                                               00296130
         LM    0,12,20(13)                                              00296140
         BR    14                                                       00296150
         SPACE                                                          00296160
         LTORG                                                          00296170
         SPACE                                                          00296180
PCLADDR  DC    0D'0'               END OF CSECT                         00296190
         SPACE                                                          00296200
************************************************************            00296210
*                                                          *            00296220
*         WORK SPACE (MAP OF GETMAINED AREA)               *            00296230
*                                                          *            00296240
************************************************************            00296250
         SPACE                                                          00296260
@DATA    DSECT                                                          00296270
         DS    18F                 REGISTER SAVEAREA                    00296280
BASES    DS    2F                                                       00296290
LINKAREA DS    2F                                                       00296300
MYPPL    DS    7F                                                       00296310
MYANS    DS    F                                                        00296320
MYECB    DS    F                  USED BY PUTLINE ROUTINE               00296330
MYIOPL   DS    4F                 USED BY PUTLINE ROUTINE               00296340
MYPTPB   DS    3F                 USED BY PUTLINE ROUTINE               00296350
MYOLD    DS    2F                 USED BY PUTLINE ROUTINE               00296360
MYSEG1   DS    2H,CL256           USED BY PUTLINE ROUTINE               00296370
PUTLINS  DS    4F                 USED BY PUTLINE ROUTINE               00296380
MYPUTLEP DS    F                  ADDRESS OF IKJPUTL                    00296390
MYSTPB   DS    0F                 5 WORDS USED BY STACK DELETE          00296400
MYDAPL   DS    5F                                                       00296410
MYDAPB   DS    27F                                                      00296420
DSNAME   DS    H,CL44                                                   00296430
VOLSER   DS    CL6                                                      00296440
MSGWK    DS    CL128                                                    00296450
STATUS   DS    X                                                        00296460
STATCON  EQU   X'80'                                                    00296470
SETCLASS DS    X                                                        00296480
RC       DS    H                                                        00296490
MYDFPARM DS    5F  USED BY DAIRFAIL                                     00296500
MYDFREGS DS    F   USED BY DAIRFAIL                                     00296510
MYDFRC   DS    F   USED BY DAIRFAIL                                     00296520
MYJEFF02 DS    F   USED BY DAIRFAIL                                     00296530
MYDFID   DS    H   USED BY DAIRFAIL                                     00296540
HALF     DS    H                                                        00296550
DOUBLE   DS    D                                                        00296560
DDSAVE   DS    CL8                                                      00296570
ATTRIB   DS    CL8                                                      00296580
FORMS    DS    CL4                                                      00296590
PRIMARY  DS    F                                                        00296600
SECDARY  DS    F                                                        00296610
DIRBLKS  DS    F                                                        00296620
BLOCKSP  DS    F                                                        00296630
SAVEUPT  DS    X                                                        00296640
RECFORM  DS    X                                                        00296650
DSORGVAL DS    H                                                        00296660
RECSIZE  DS    H                                                        00296670
BLOCKSI  DS    H                                                        00296680
DAIRREGS DS    F                                                        00296690
OPEN     DS    0F                                                       00296700
CLOSE    DS    F                                                        00296710
SOUDCBW  DS    0F,(SOUDCBL)X                                            00296720
SETPRTRC DS    F                                                        00296730
SETPRTL  DS    12F                                                      00296740
         DS    0D                                                       00296750
CONCPTR  DS    F                                                        00296760
CONCDAPB DS    3F                                                       00296770
         DS    50CL8                                                    00296780
@DATAL   EQU   *-@DATA                                                  00296790
         SPACE                                                          00296800
         IKJCPPL                                                        00296810
         SPACE 3                                                        00296820
         IKJPPL                                                         00296830
         SPACE                                                          00296840
         IKJUPT                                                         00296850
         SPACE 2                                                        00296860
         IKJIOPL                                                        00296870
         SPACE 2                                                        00296880
         IKJDAPL                                                        00296890
         SPACE 2                                                        00296900
         IKJDAP08                                                       00296910
DE08CTLX DS    X        FLAGS                                           00296920
DE08MSG  EQU   X'80'    AUTOMATIC DAIRFAIL MESSAGE                      00296930
DE08FC   EQU   X'40'    FREE=CLOSE (NOT IMPLEMENTED)                    00296940
DE08MSVG EQU   X'20'    MSVGP NAME IN VOL SER FIELD                     00296950
DE08RET  EQU   X'10'    RETENTION PERIOD, NOT EXPIRATION DATE           00296960
DE08NEX  EQU   X'01'    DO NOT USE AN EXISTING ALLOCATION               00296970
DE08RECF DS    X        DCB RECFM                                       00296980
DE08LREC DS    H        DCB LRECL                                       00296990
DE08BLKS DS    H        DCB BLKSIZE                                     00297000
DE08DSOR DS    XL2      DCB DSORG                                       00297010
*                       END OF 8 BYTE SECTION                           00297020
         DS    X        RESERVED                                        00297030
DE08EXPD DS    0XL3     EXP DATE, BINARY, 1 BYTE YEAR, 2 BYTES DAY      00297040
DE08EXYR DS    X        EXP DATE YEAR, BINARY                           00297050
DE08EXJD DS    0H       EXP DATE JULIAN DAY, BINARY                     00297060
DE08RETP DS    H        RETENTION PERIOD (0-9999)                       00297070
DE08POS  DS    H        TAPE DATA SET SEQUENCE NUMBER (0-9999)          00297080
DE08LABL DS    X        TAPE LABEL TYPE (HEX 01 02 10 FOR NL SL BLP)    00297090
DE08DEN  DS    X        TAPE DENSITY (HEX 83 C3 D3 FOR 800 1600 6250)   00297100
*                       END OF 8 BYTE SECTION                           00297110
*                       DAPB08 HAS BEEN EXPANDED BY 16 BYTES            00297120
         SPACE 2                                                        00297130
         IKJDAP0C                                                       00297140
         SPACE 2                                                        00297150
         IKJDAP18                                                       00297160
         SPACE 2                                                        00297170
         IKJDAP1C                                                       00297180
DE1CCTLX DS    X        FLAGS                                           00297190
DE1CMSG  EQU   X'80'    AUTOMATIC DAIRFAIL MESSAGE                      00297200
DE1CRECF DS    X        DCB RECFM                                       00297210
DE1CLREC DS    H        DCB LRECL                                       00297220
DE1CBLKS DS    H        DCB BLKSIZE                                     00297230
         DS    XL2      RESERVED                                        00297240
*                       DAPB1C HAS BEEN EXPANDED BY 08 BYTES            00297250
         SPACE 2                                                        00297260
         IKJDAP30                                                       00297270
DE30CTLX DS    X        FLAGS                                           00297280
DE30MSG  EQU   X'80'    AUTOMATIC DAIRFAIL MESSAGE                      00297290
DE30FC   EQU   X'40'    FREE=CLOSE                                      00297300
DE30HOLD EQU   X'20'    HOLD                                            00297310
DE30RECF DS    X        DCB RECFM                                       00297320
DE30LREC DS    H        DCB LRECL                                       00297330
DE30BLKS DS    H        DCB BLKSIZE                                     00297340
         DS    XL2      RESERVED                                        00297350
DE30CPYS DS    X        COPIES                                          00297360
         DS    XL3      RESERVED                                        00297370
DE30FCB  DS    CL4      FCB                                             00297380
DE30DEST DS    CL8      DEST                                            00297390
*                       DAPB1C HAS BEEN EXPANDED BY 24 BYTES            00297400
         SPACE 2                                                        00297410
R0       EQU   0                                                        00297420
R1       EQU   1                                                        00297430
R2       EQU   2                                                        00297440
R3       EQU   3                                                        00297450
R4       EQU   4                                                        00297460
R5       EQU   5                                                        00297470
R6       EQU   6                                                        00297480
R7       EQU   7                                                        00297490
R8       EQU   8                                                        00297500
R9       EQU   9                                                        00297510
R10      EQU   10                                                       00297520
R11      EQU   11                                                       00297530
R12      EQU   12                                                       00297540
R13      EQU   13                                                       00297550
R14      EQU   14                                                       00297560
R15      EQU   15                                                       00297570
         END                                                            00297580
//ASM.SYSTERM DD SYSOUT=*
//ASM.SYSGO DD DSN=&&OBJSET,UNIT=SYSSQ,SPACE=(80,(200,50)),
//             DISP=(MOD,PASS)
//ASMFCL EXEC ASMFCL,
//             PARM.ASM='OBJ,NODECK,TERM',
//             PARM.LKED='LIST,MAP,RENT,REUS,REFR'
//ASM.SYSIN DD *
         TITLE '   S 9 9 D A I R  '                                     00297590
*********************************************************************** 00297600
*                                                                     * 00297610
*        'S99DAIR' SUBPROGRAM                                         * 00297620
*                                                                     * 00297630
*********************************************************************** 00297640
*                                                                       00297650
*        CURRENT STATUS: VERSION 1.4                                    00297660
*          NOT THOROUGHLY TESTED BUT NO KNOWN PROBLEMS.                 00297670
*                                                                       00297680
*        WRITTEN BY. BILL GODFREY,  PLANNING RESEARCH CORPORATION.      00297690
*        INSTALLATION. PRC COMPUTER CENTER, INC.  MCLEAN VA             00297700
*        DATE WRITTEN. MARCH 6 1981.                                    00297710
*        DATE UPDATED. MAY 26 1982.                                     00297720
*        ATTRIBUTES. RE-ENTRANT.                                        00297730
*        MACROS. GETMAIN FREEMAIN LINK IEFZB4D0 IEFZB4D2 IKJEFFDF.      00297740
*         IKJCPPL, IKJUPT, IKJDAPL, IKJDAP08, IKJDAP1C, IKJDAP30.       00297750
*        LOG OF CHANGES.                                                00297760
*         30MAR81. LABEL IN08RDDX ADDED. UNALLOC IF MEM SPEC, NOT PDS.  00297770
*         07APR81. ADDED DE08NEX BIT, TO SET ON S99NOCNV BIT.           00297780
*         14MAY82. DE08DSOR (DSORG) NOW SUPPORTED.                      00297790
*         26MAY82. PROBLEM CORRECTED WITH AMPERSAND DSNAMES.            00297800
*        DESCRIPTION.                                                   00297810
*         THIS SUBPROGRAM IS AN ENHANCED VERSION OF IKJDAIR FOR TYPES   00297820
*         '0008' (DSNAME), '001C' (TERMINAL), AND '0030' (SYSOUT).      00297830
*                                                                       00297840
*         IT DOES NOT REPLACE IKJDAIR, BUT CAN BE CALLED INSTEAD        00297850
*         OF IKJDAIR, AND IF THE DAPB TYPE IS NOT HANDLED BY THIS       00297860
*         PROGRAM, THEN THIS PROGRAM WILL PASS IT ON TO IKJDAIR.        00297870
*                                                                       00297880
*         THIS SUBPROGRAM ACCEPTS A PARAMETER LIST SIMILAR              00297890
*         TO THAT PASSED TO IKJDAIR, AND EITHER PASSES IT ON            00297900
*         TO IKJDAIR OR PERFORMS THE REQUESTED FUNCTION ITSELF.         00297910
*                                                                       00297920
*         WHEN MODIFYING AN EXISTING TSO COMMAND TO GIVE                00297930
*         IT THE ABILITY TO DYNAMICALLY ALLOCATE A GREATER              00297940
*         VARIETY OF DATA SETS THAN 'IKJDAIR' ALLOWS, IT                00297950
*         IS INCONVENIENT TO CONVERT THE CODE FROM 'IKJDAIR'            00297960
*         ORIENTED PARAMETER LISTS TO 'SVC-99' ORIENTED                 00297970
*         PARAMETER LISTS.  THIS SUBPROGRAM WAS WRITTEN TO              00297980
*         SIMPLIFY THAT TASK BY ALLOWING THE 'IKJDAIR' LOGIC            00297990
*         TO BE LEFT UNCHANGED, WHILE INCORPORATING THE NEW             00298000
*         SVC-99 FUNCTIONS IN THE FORM OF NEW FIELDS IN THE             00298010
*         DAIR PARAMETER BLOCKS.                                        00298020
*                                                                       00298030
*         TO CONVERT A COMMAND, SIMPLY ADD YOUR NEW KEYWORDS            00298040
*         AND OPERANDS, ADD CODE TO INTERPRET THEM INTO THE NEW         00298050
*         FIELDS, AND CALL 'S99DAIR' INSTEAD OF 'IKJDAIR'. BE SURE      00298060
*         TO EXTEND ANY DAPB'S FOR TYPE 0008, 001C, AND 0030 BY         00298070
*         THE PROPER NUMBER OF BYTES, AND ZERO ALL UNUSED FIELDS.       00298080
*                                                                       00298090
*         THE FOLLOWING FEATURES ARE NOT IN IKJDAIR, BUT ARE            00298100
*         AVAILABLE THRU THIS PROGRAM -                                 00298110
*                                                                       00298120
*          A) ALLOCATE A DATA SET, SYSOUT, OR A TERMINAL, WITH          00298130
*             SPECIFIED DCB ATTRIBUTES, WITHOUT HAVING TO               00298140
*             ALLOCATE AN ATTRIBUTE LIST NAME.                          00298150
*          B) ALLOCATE A DATA SET WITH ZERO TRACKS PRIMARY SPACE.       00298160
*          C) ALLOCATE A SYSOUT FILE SPECIFYING CLASS, DEST, HOLD,      00298170
*             COPIES, AND FCB.  IKJDAIR IGNORES CLASS EVEN THOUGH       00298180
*             THERE ALREADY IS A FIELD FOR IT.                          00298190
*          D) ALLOCATE A DATA SET SPECIFYING MSVGP.                     00298200
*          E) ALLOCATE A TAPE SPECIFYING FILE POSITION, LABEL TYPE,     00298210
*             EXPIRATION DATE (OR RETENTION PERIOD), AND DENSITY.       00298220
*             (FOR USERS WITH TAPE MOUNT OR BACKGROUND COMMANDS).       00298230
*          F) OPTIONALLY REQUEST THAT EXPLANATORY MESSAGES BE           00298240
*             ISSUED IF ALLOCATION FAILS (EXCEPT IN CASES WHERE         00298250
*             ALLOCATION WAS NEVER ATTEMPTED DUE TO INVALID             00298260
*             PARAMETERS - DAIR RETURN CODE 4).                         00298270
*                                                                       00298280
*         THESE FUNCTIONS ARE REQUESTED BY APPENDING NEW FIELDS         00298290
*         TO THE DA08 AND DA30 PARAMETER AREAS, AND THEN                00298300
*         CALLING THIS SUBPROGRAM INSTEAD OF IKJDAIR.                   00298310
*                                                                       00298320
*         OTHER DIFFERENCES BETWEEN THIS PROGRAM AND IKJDAIR.           00298330
*                                                                       00298340
*          A) THIS PROGRAM DOES NOT HAVE THE SAME DEGREE OF             00298350
*             VALIDITY CHECKING THAT IKJDAIR DOES.  SOME INVALID        00298360
*             DATA IN THE OLD FIELDS WHICH IKJDAIR WOULD REJECT         00298370
*             MAY GO UNNOTICED HERE (SYSOUT SPACE FOR EXAMPLE).         00298380
*          B) DEFAULT SPACE FOR NEW DATA SETS IS NOT HANDLED            00298390
*             THE WAY IKJDAIR DOES.  IF SPACE TYPE IS SPECIFIED,        00298400
*             DEFAULT SPACE IS NOT ASSIGNED.                            00298410
*                                                                       00298420
*         THE OPERATING SYSTEM MUST BE OS/VS2 MVS.                      00298430
*                                                                       00298440
*         AT ENTRY REGISTER 1 POINTS TO A 'DAPL', MAPPED BY             00298450
*         THE STANDARD 'IKJDAPL' MACRO.                                 00298460
*                                                                       00298470
*         UPON RETURN TO CALLER, REGISTER 15 IS ZERO IF THE             00298480
*         ALLOCATION WAS SUCCESSFUL.  OTHERWISE REGISTER 15             00298490
*         HAS A VALUE FROM 4 TO 52.                                     00298500
*                                                                       00298510
*         THERE IS SOME CODE TO ALLOW FOR THE POSSIBILITY THAT THIS     00298520
*         IS INSTALLED AS A FRONT END TO IKJDAIR, BUT WE HAVE NOT       00298530
*         INSTALLED IT AS A FRONT END, NOR HAS IT BEEN TESTED AS A      00298540
*         FRONT END.  TO USE IT AS A FRONT END, THE 'A(0)' AT LABEL     00298550
*         'DAIRPTR' WOULD HAVE TO BE CHANGED TO V(IEFDB4D0) BEFORE      00298560
*         INSTALLATION OF THE FRONT END, AND THE 3 COMMENTED INSTRUC-   00298570
*         TIONS AT LABEL 'CALDAIR' WOULD HAVE TO BE UN-COMMENTED.       00298580
*                                                                       00298590
*$DOC$***************************************************************** 00298600
         SPACE                                                          00298610
S99DAIR  START                                                          00298620
*        WXTRN IEFDB4D0                                                 00298630
         USING *,R10,R11                                                00298640
         B     @PROLOG-*(,15)                                           00298650
         DC    AL1(11),CL11'S99DAIR 1.4'                                00298660
         DC    CL16' &SYSDATE &SYSTIME '                                00298670
@PROLOG  STM   14,12,12(R13)                                            00298680
         LR    R10,R15             BASE REGISTER                        00298690
         LA    R15,1                                                    00298700
         LA    R11,4095(R15,R10)   BASE REGISTER                        00298710
         LR    R2,R1               SAVE PARAMETER LIST ADDRESS          00298720
         USING DAPL,R2                                                  00298730
         L     R3,DAPLDAPB                                              00298740
         LA    R4,IN08                                                  00298750
         CLC   0(2,R3),=X'0008'    DSNAME                               00298760
         BE    INIT                YES, BRANCH                          00298770
         LA    R4,IN1C                                                  00298780
         CLC   0(2,R3),=X'001C'    TERMINAL                             00298790
         BE    INIT                YES, BRANCH                          00298800
         LA    R4,IN30                                                  00298810
         CLC   0(2,R3),=X'0030'    SYSOUT                               00298820
         BE    INIT                YES, BRANCH                          00298830
         EJECT                                                          00298840
CALDAIR  EQU   *                                                        00298850
*        L     R15,DAIRPTR         GET ADDRESS OF IKJDAIR               00298860
*        LTR   R15,R15             IS THIS A FRONT END                  00298870
*        BNZ   GOTDAIR             YES, BRANCH                          00298880
TSSDAIR  L     R15,16              CVTPTR                               00298890
         TM    732(R15),X'80'      IS IKJDAIR IN LPA                    00298900
         BNO   LNKDAIR             NO, BRANCH                           00298910
         L     R15,732(,R15)       YES, GET IKJDAIR ENTRY ADDRESS       00298920
GOTDAIR  LM    0,12,20(R13)        RESTORE ALL REGISTERS                00298930
         BR    R15                 DAIR WILL RETURN TO OUR CALLER       00298940
         SPACE                                                          00298950
LNKDAIR  LA    R0,80               LENGTH OF SAVEAREA PLUS LINK SF      00298960
         GETMAIN R,LV=(0)          GET A SAVEAREA                       00298970
         ST    R13,4(,R1)          PUT OLD ADDRESS IN NEW SAVEAREA      00298980
         ST    R1,8(,R13)          PUT NEW ADDRESS IN OLD SAVEAREA      00298990
         LR    R13,R1              TRANSFER SAVEAREA BASE               00299000
         LA    R15,72(,R13)        POINT TO LINK PARAMETER AREA         00299010
         XC    0(8,R15),0(R15)     CLEAR LINK PARAMETER AREA            00299020
         LR    R1,R2               RESTORE ORIGINAL R1                  00299030
         LINK  EP=IKJDAIR,SF=(E,(R15))                                  00299040
         LR    R1,R13              SET UP R1 FOR FREEMAIN               00299050
         LA    R0,80               SET UP R0 FOR FREEMAIN               00299060
         L     R13,4(,R13)         RESTORE ORIGINAL SAVEAREA BASE       00299070
         LR    R2,R15              SAVE R15 ACROSS FREEMAIN             00299080
         FREEMAIN R,LV=(0),A=(1)                                        00299090
         LR    R15,R2              RESTORE R15 (DAIR RETURN CODE)       00299100
         LM    0,12,20(R13)        RESTORE OTHER REGISTERS              00299110
         L     R14,12(,R13)        RESTORE RETURN ADDRESS               00299120
         BR    R14                 RETURN                               00299130
         SPACE                                                          00299140
         LTORG                                                          00299150
DAIRPTR  DC    A(0)  V(IEFDB4D0)   EXTERNAL REFERENCE TO DAIR           00299160
         EJECT                                                          00299170
*********************************************************************** 00299180
*                                                                       00299190
*         INITIALIZATION                                                00299200
*                                                                       00299210
*********************************************************************** 00299220
         SPACE                                                          00299230
INIT     LA    R0,@DATAL                                                00299240
         LA    R1,1                SUBPOOL                              00299250
         SLL   R1,24               TO LEFT BYTE                         00299260
         OR    R0,R1               OVER R0                              00299270
         GETMAIN R,LV=(0)                                               00299280
         ST    R13,4(,R1)          PUT OLD ADDRESS IN NEW SAVEAREA      00299290
         ST    R1,8(,R13)          PUT NEW ADDRESS IN OLD SAVEAREA      00299300
         LR    R13,R1              TRANSFER SAVEAREA BASE               00299310
         LR    R9,R1               WORKAREA BASE                        00299320
         USING @DATA,R9                                                 00299330
         SPACE                                                          00299340
         LA    R1,WK99RB                                                00299350
         ST    R1,WK99RBP                                               00299360
         OI    WK99RBP,X'80'                                            00299370
         XC    WK99RB(20),WK99RB                                        00299380
         LA    R14,WK99RB                                               00299390
         USING S99RB,R14                                                00299400
         MVI   S99RBLN,20                                               00299410
         MVI   S99VERB,S99VRBAL                                         00299420
         LA    R1,WK99TUPL                                              00299430
         ST    R1,S99TXTPP                                              00299440
         DROP  R14                 S99RB                                00299450
         SPACE                                                          00299460
         XC    WK99TUPL(TUPL$LEN),WK99TUPL                              00299470
         OI    WK99TUPL+TUPL$LEN-4,X'80'                                00299480
         BR    R4                  TO IN08 OR IN1C OR IN30              00299490
         USING S99TUNIT,R4                                              00299500
         EJECT                                                          00299510
*********************************************************************** 00299520
*                                                                       00299530
*         DAIR 08 - DSNAME ALLOCATION                                   00299540
*                                                                       00299550
*********************************************************************** 00299560
         SPACE                                                          00299570
IN08     DC    0H'0'                                                    00299580
         USING DAPB08,R3                                                00299590
         SPACE                                                          00299600
         XC    DA08DARC(4),DA08DARC ZERO DARC AND CTRC                  00299610
         SPACE                                                          00299620
         TM    DA08CTL,DA08ATRL    ATTRIBUTE NAME PRESENT               00299630
         BZ    IN08ATOK            NO, BRANCH                           00299640
         CLI   DA08ALN,C' '        YES, IS IT BLANK                     00299650
         BE    EXIT4               YES, ERROR                           00299660
IN08ATOK EQU   *                                                        00299670
         SPACE                                                          00299680
         CLI   DA08UNIT,0                                               00299690
         BE    EXIT4                                                    00299700
         CLI   DA08SER,0                                                00299710
         BE    EXIT4                                                    00299720
         CLI   DA08DSP1,DA08NEW                                         00299730
         BNE   IN08XNEW                                                 00299740
*        CLI   DA08DPS2,DA08KEEP   NEW KEEP NOT ALLOWED BY IKJDAIR      00299750
*        BE    EXIT4                        BUT ITS OK WITH US          00299760
         CLI   DA08DPS2,DA08UCAT   NEW UNCAT NOT ALLOWED BY IKJDAIR     00299770
         BE    EXIT4                        OR US EITHER                00299780
IN08XNEW EQU   *                                                        00299790
         EJECT                                                          00299800
*********************************************************************** 00299810
*                                                                       00299820
*         CHECK DSNAME                                                  00299830
*                                                                       00299840
*********************************************************************** 00299850
         SPACE                                                          00299860
         MVI   DSNAME,C' '                                              00299870
         MVC   DSNAME+1(43),DSNAME                                      00299880
         MVI   WKDSRET,0                                                00299890
         L     R14,DA08PDSN        POINTER TO DSNAME                    00299900
         LTR   R14,R14             IS DSNAME PRESENT                    00299910
         BZ    IN08DSNN            NO, BRANCH                           00299920
         MVI   WKDSRET,C'S'        INDICATE NAME IS SPECIFIED           00299930
         LH    R0,0(,R14)          YES, GET LENGTH                      00299940
         LTR   R0,R0               IS LENGTH ZERO                       00299950
         BZ    EXIT4               YES, ERROR                           00299960
         CLI   2(R14),C' '         IS DSNAME BLANK                      00299970
         BNE   IN08DSNL            NO, BRANCH                           00299980
         CH    R0,=H'44'           YES, IS ITS LENGTH 44                00299990
         BNE   EXIT4               NO, ERROR                            00300000
         CLI   2(R14),X'40'        IS IT BLANK                          00300010
         BNE   EXIT4               NO, ERROR                            00300020
         TM    DA08CTL,DA08UID     PREFIXING REQUESTED WITH BLANK NAME  00300030
         BO    EXIT4               YES, ERROR                           00300040
         MVI   WKDSRET,C'R'        INDICATE RETURN DSNAME               00300050
         B     IN08DSOK                                                 00300060
IN08DSNN TM    DA08CTL,DA08UID     PREFIXING REQUESTED WITH NO NAME     00300070
         BZ    IN08DSOK            NO, BRANCH                           00300080
         B     EXIT4               YES, ERROR                           00300090
IN08DSNL CH    R0,=H'44'           IS LENGTH GREATER THAN 44            00300100
         BH    EXIT4               YES, ERROR                           00300110
         CLI   2(R14),C'&&'        TEMPORARY NAME                       00300120
         BNE   IN08DSNM            NO, BRANCH                           00300130
         TM    DA08CTL,DA08UID     IS PREFIXING REQUESTED WITH TEMP     00300140
         BO    EXIT4               YES, ERROR                   26MAY82 00300150
IN08DSNM EQU   *                                                        00300160
         LA    R15,DSNAME          START WITH FIRST BYTE                00300170
         SLR   R1,R1               START WITH LENGTH ZERO               00300180
         STH   R1,DSNAMEL          START WITH LENGTH ZERO               00300190
         TM    DA08CTL,DA08UID     PREFIXING REQUESTED                  00300200
         BZ    IN08DSNQ            NO, BRANCH                           00300210
         L     R14,DAPLUPT         POINT TO USER PROFILE TABLE          00300220
         LTR   R14,R14             IS THERE A UPT                       00300230
*        BZ    IN08DSNQ            NO, SKIP PREFIXING                   00300240
         BZ    EXIT4               NO, ERROR                            00300250
         USING UPT,R14                                                  00300260
         IC    R1,UPTPREFL         GET LENGTH OF PREFIX                 00300270
         LTR   R1,R1               IS IT ZERO                           00300280
         BZ    IN08DSNQ            YES, SKIP PREFIXING                  00300290
         B     *+10                                                     00300300
         MVC   0(0,R15),UPTPREFX                                        00300310
         EX    R1,*-6              MOVE USERID TO DSNAME AREA           00300320
         DROP  R14                 UPT                                  00300330
         LA    R15,0(R1,R15)       POINT PAST USERID                    00300340
         MVI   0(R15),C'.'         APPEND PERIOD                        00300350
         LA    R15,1(,R15)         POINT PAST PERIOD                    00300360
         LA    R1,1(,R1)           ADD 1 TO LENGTH                      00300370
         STH   R1,DSNAMEL          STORE LENGTH OF USERID PLUS 1        00300380
IN08DSNQ EQU   *                                                        00300390
         LR    R1,R0               SAVE LENGTH OF SPECIFIED NAME        00300400
         AH    R0,DSNAMEL          ADD LENGTH OF PREFIX OR ZERO         00300410
         STH   R0,DSNAMEL          SET COMBINED LENGTH                  00300420
         CH    R0,=H'44'           IS LENGTH GREATER THAN 44            00300430
         BH    EXIT4               YES, ERROR                           00300440
         L     R14,DA08PDSN        POINT TO DSN VALUE                   00300450
         BCTR  R1,0                LENGTH MINUS 1 FOR EX                00300460
         B     *+10                BRANCH AROUND EXECUTED MVC           00300470
         MVC   0(0,R15),2(R14)     (EXECUTED)                           00300480
         EX    R1,*-6              MOVE DSN TO DSNAME (AFTER PREFIX)    00300490
IN08DSOK EQU   *                                                        00300500
         SPACE                                                          00300510
         MVI   DEFPTR,0            RESET DEFAULT SPACE SWITCH           00300520
         MVI   SV08CTL,0           RESET DEFAULT SPACE FLAGS            00300530
*                                                                       00300540
*              PROCESS DO-NOT-USE-EXISTING-ALLOCATION                   00300550
*                                                                       00300560
         TM    DE08CTLX,DE08NEX                                         00300570
         BZ    NEXX                                                     00300580
         LA    R14,WK99RB                                               00300590
         USING S99RB,R14                                                00300600
         OI    S99FLAG1,S99NOCNV                                        00300610
         DROP  R14                                                      00300620
NEXX     EQU   *                                                        00300630
*                                                                       00300640
*              PROCESS RETURN-DSORG                                     00300650
*                                                                       00300660
         LA    R4,WK99TXRO                                              00300670
         ST    R4,WK99TURO                                              00300680
         LA    R15,DALRTORG                                             00300690
         STH   R15,S99TUKEY                                             00300700
         LA    R15,1                                                    00300710
         STH   R15,S99TUNUM                                             00300720
         LA    R15,2                                                    00300730
         STH   R15,S99TULNG                                             00300740
         XC    S99TUPAR(2),S99TUPAR                                     00300750
         SPACE                                                          00300760
*                                                                       00300770
*              PROCESS DDNAME                                           00300780
*                                                                       00300790
         MVI   WKDDRET,0                                                00300800
         LA    R4,WK99TXDD                                              00300810
         ST    R4,WK99TUDD                                              00300820
         CLI   DA08DDN,X'40'       DD TO BE RETURNED?                   00300830
         BE    IN08DDR             YES - BRANCH                         00300840
*                                                                       00300850
*              DDNAME IS SPECIFIED                                      00300860
*                                                                       00300870
         LA    R15,DALDDNAM                                             00300880
         STH   R15,S99TUKEY                                             00300890
         LA    R15,1                                                    00300900
         STH   R15,S99TUNUM                                             00300910
         LA    R15,8                                                    00300920
         STH   R15,S99TULNG                                             00300930
         MVC   S99TUPAR(8),DA08DDN                                      00300940
         B     IN08DDX                                                  00300950
*                                                                       00300960
*              DDNAME NOT SPECIFIED                                     00300970
*                                                                       00300980
IN08DDR  LA    R15,DALRTDDN                                             00300990
         STH   R15,S99TUKEY                                             00301000
         LA    R15,1                                                    00301010
         STH   R15,S99TUNUM                                             00301020
         LA    R15,8                                                    00301030
         STH   R15,S99TULNG                                             00301040
         MVC   S99TUPAR(8),=CL8' '                                      00301050
         MVI   WKDDRET,C'R'                                             00301060
IN08DDX  EQU   *                                                        00301070
*                                                                       00301080
*              PROCESS DSNAME                                           00301090
*                                                                       00301100
         LA    R4,WK99TXDS                                              00301110
         ST    R4,WK99TUDS                                              00301120
         CLI   WKDSRET,0           IS DSNAME POINTER PRESENT            00301130
         BE    IN08DSR             NO, BRANCH                           00301140
         CLI   DSNAME,C' '         YES, IS IT 44 BLANKS                 00301150
         BE    IN08DSR             YES, BRANCH                          00301160
*                                                                       00301170
*              DSNAME IS SPECIFIED                                      00301180
*                                                                       00301190
         LA    R15,DALDSNAM                                             00301200
         STH   R15,S99TUKEY                                             00301210
         LA    R15,1                                                    00301220
         STH   R15,S99TUNUM                                             00301230
         LH    R15,DSNAMEL                                              00301240
         STH   R15,S99TULNG                                             00301250
         MVC   S99TUPAR(44),DSNAME                                      00301260
         B     IN08DSX                                                  00301270
*                                                                       00301280
*              PROCESS RETURN-DSNAME                                    00301290
*                                                                       00301300
IN08DSR  LA    R15,DALRTDSN                                             00301310
         STH   R15,S99TUKEY                                             00301320
         LA    R15,1                                                    00301330
         STH   R15,S99TUNUM                                             00301340
         LA    R15,44                                                   00301350
         STH   R15,S99TULNG                                             00301360
         MVI   S99TUPAR,C' '                                            00301370
         MVC   S99TUPAR+1(43),S99TUPAR                                  00301380
IN08DSX  EQU   *                                                        00301390
*                                                                       00301400
*              PROCESS MEMBER                                           00301410
*                                                                       00301420
         CLI   DA08MNM,X'40'                                            00301430
         BE    IN08MEX                                                  00301440
         LA    R4,WK99TXME                                              00301450
         ST    R4,WK99TUME                                              00301460
         LA    R15,DALMEMBR                                             00301470
         STH   R15,S99TUKEY                                             00301480
         LA    R15,1                                                    00301490
         STH   R15,S99TUNUM                                             00301500
         LA    R15,8                                                    00301510
         STH   R15,S99TULNG                                             00301520
         MVC   S99TUPAR(8),DA08MNM                                      00301530
IN08MEX  EQU   *                                                        00301540
*                                                                       00301550
*              PROCESS UNIT                                             00301560
*                                                                       00301570
         CLI   DA08UNIT,X'40'                                           00301580
         BE    IN08UNX                                                  00301590
         LA    R4,WK99TXUN                                              00301600
         ST    R4,WK99TUUN                                              00301610
         LA    R15,DALUNIT                                              00301620
         STH   R15,S99TUKEY                                             00301630
         LA    R15,1                                                    00301640
         STH   R15,S99TUNUM                                             00301650
         LA    R15,8                                                    00301660
         STH   R15,S99TULNG                                             00301670
         MVC   S99TUPAR(8),DA08UNIT                                     00301680
IN08UNX  EQU   *                                                        00301690
*                                                                       00301700
*              PROCESS VOLUME SERIAL                                    00301710
*                                                                       00301720
         CLI   DA08SER,X'40'                                            00301730
         BE    IN08VOX                                                  00301740
         TM    DE08CTLX,DE08MSVG                                        00301750
         BO    IN08VOX                                                  00301760
         LA    R4,WK99TXVL                                              00301770
         ST    R4,WK99TUVL                                              00301780
         LA    R15,DALVLSER                                             00301790
         STH   R15,S99TUKEY                                             00301800
         LA    R15,1                                                    00301810
         STH   R15,S99TUNUM                                             00301820
         LA    R15,6                                                    00301830
         STH   R15,S99TULNG                                             00301840
         MVC   S99TUPAR(6),DA08SER                                      00301850
IN08VOX  EQU   *                                                        00301860
*                                                                       00301870
*              PROCESS STATUS (OLD, SHR, NEW, MOD)                      00301880
*                                                                       00301890
         LA    R4,WK99TXD1                                              00301900
         ST    R4,WK99TUD1                                              00301910
         LA    R15,DALSTATS                                             00301920
         STH   R15,S99TUKEY                                             00301930
         LA    R15,1                                                    00301940
         STH   R15,S99TUNUM                                             00301950
         LA    R15,1                                                    00301960
         STH   R15,S99TULNG                                             00301970
         MVC   S99TUPAR(1),DA08DSP1                                     00301980
         CLI   S99TUPAR,0          IS IT ZEROES                         00301990
         BNE   *+8                 NO                                   00302000
         MVI   S99TUPAR,1          YES, MAKE IT OLD                     00302010
*                                                                       00302020
*              PROCESS NORMAL DISP (KEEP, DELETE, CATLG, UNCAT)         00302030
*                                                                       00302040
         CLI   DA08DPS2,0                                               00302050
         BE    IN08D2X                                                  00302060
         LA    R4,WK99TXD2                                              00302070
         ST    R4,WK99TUD2                                              00302080
         LA    R15,DALNDISP                                             00302090
         STH   R15,S99TUKEY                                             00302100
         LA    R15,1                                                    00302110
         STH   R15,S99TUNUM                                             00302120
         LA    R15,1                                                    00302130
         STH   R15,S99TULNG                                             00302140
         MVC   S99TUPAR(1),DA08DPS2                                     00302150
IN08D2X  EQU   *                                                        00302160
*                                                                       00302170
*              PROCESS CONDITIONAL DISP                                 00302180
*                                                                       00302190
         CLI   DA08DPS3,0                                               00302200
         BE    IN08D3X                                                  00302210
         LA    R4,WK99TXD3                                              00302220
         ST    R4,WK99TUD3                                              00302230
         LA    R15,DALCDISP                                             00302240
         STH   R15,S99TUKEY                                             00302250
         LA    R15,1                                                    00302260
         STH   R15,S99TUNUM                                             00302270
         LA    R15,1                                                    00302280
         STH   R15,S99TULNG                                             00302290
         MVC   S99TUPAR(1),DA08DPS3                                     00302300
IN08D3X  EQU   *                                                        00302310
*                                                                       00302320
*              PROCESS ATTRIBUTE LIST NAME                              00302330
*                                                                       00302340
         TM    DA08CTL,DA08ATRL    IS ATTRIBUTE NAME PRESENT            00302350
         BZ    IN08ATX             NO, BRANCH                           00302360
         LA    R4,WK99TXAT                                              00302370
         ST    R4,WK99TUAT                                              00302380
         LA    R15,DALDCBDD                                             00302390
         STH   R15,S99TUKEY                                             00302400
         LA    R15,1                                                    00302410
         STH   R15,S99TUNUM                                             00302420
         LA    R15,8               SET LENGTH INITIALLY TO 8            00302430
*        LA    R1,DA08ALN+7 POINT TO 8TH CHAR OF NAME                   00302440
*IN08ATL CLI   0(R1),X'40'         BLANK?                               00302450
*        BNE   IN08ATS             NO - BRANCH TO STORE LENGTH          00302460
*        BCTR  R15,0               YES - DECREMENT LENGTH BY 1          00302470
*        BCT   R1,IN08ATL          POINT BACK 1 CHAR AND BRANCH         00302480
IN08ATS  STH   R15,S99TULNG                                             00302490
         MVC   S99TUPAR(8),DA08ALN                                      00302500
IN08ATX  EQU   *                                                        00302510
*                                                                       00302520
*              PROCESS PASSWORD                                         00302530
*                                                                       00302540
         CLI   DA08PSWD,X'40'                                           00302550
         BE    IN08PWX                                                  00302560
         LA    R4,WK99TXPW                                              00302570
         ST    R4,WK99TUPW                                              00302580
         LA    R15,DALPASSW                                             00302590
         STH   R15,S99TUKEY                                             00302600
         LA    R15,1                                                    00302610
         STH   R15,S99TUNUM                                             00302620
         LA    R15,8                                                    00302630
         STH   R15,S99TULNG                                             00302640
         MVC   S99TUPAR(8),DA08PSWD                                     00302650
IN08PWX  EQU   *                                                        00302660
*                                                                       00302670
*              PROCESS DUMMY                                            00302680
*                                                                       00302690
         TM    DA08CTL,DA08DMMY                                         00302700
         BZ    IN08DMX                                                  00302710
         LA    R4,WK99TXDM                                              00302720
         ST    R4,WK99TUDM                                              00302730
         LA    R15,DALDUMMY                                             00302740
         STH   R15,S99TUKEY                                             00302750
         LA    R15,0                                                    00302760
         STH   R15,S99TUNUM                                             00302770
*                                                                       00302780
*              BYPASS SPACE PARAMETERS IF DUMMY                         00302790
*                                                                       00302800
         B     IN08SPCX                                                 00302810
IN08DMX  EQU   *                                                        00302820
*                                                                       00302830
*              BYPASS SPACE PARAMETERS IF NOT NEW OR MOD                00302840
*                                                                       00302850
         CLI   DA08DSP1,DA08NEW                                         00302860
         BE    IN08SPC                                                  00302870
         CLI   DA08DSP1,DA08MOD                                         00302880
         BNE   IN08SPCX                                                 00302890
IN08SPC  EQU   *                                                        00302900
         MVC   SV08BLK,DA08BLK     COPY                                 00302910
         MVC   SV08PQTY,DA08PQTY    SPACE INFO                          00302920
         MVC   SV08SQTY,DA08SQTY     INTO AREA                          00302930
         MVC   SV08DQTY,DA08DQTY      COMMON WITH                       00302940
         MVC   SV08CTL,DA08CTL         DEFAULT SPACE                    00302950
         NI    SV08CTL,DA08CYLS+DA08RLSE LEAVE ONLY THESE BITS          00302960
*                                                                       00302970
*              IF SPACE TYPE IS SPECIFIED, FORCE THE USE OF             00302980
*              DA08PQTY, EVEN IF IT IS ZERO.  THIS LOGIC                00302990
*              IS DIFFERENT FROM IKJDAIR, WHICH DOES NOT ALLOW          00303000
*              ZERO SPACE ALLOCATIONS.                                  00303010
*                                                                       00303020
         TM    DA08CTL,DA08TRKS+DA08ABKL                                00303030
         BNZ   IN08PQ                                                   00303040
*                                                                       00303050
*              IF PRIMARY SPACE IS NONZERO, GO PROCESS PQTY             00303060
*                                                                       00303070
         CLC   DA08PQTY+1(3),=XL3'00'                                   00303080
         BNE   IN08PQ                                                   00303090
*                                                                       00303100
*              IF PRIMARY AND DIR ARE ZERO, LET SVC 99 GET DEFAULTS.    00303110
*                                                                       00303120
         CLC   DA08DQTY+1(3),=XL3'00'                                   00303130
         BE    IN08SPCX            BYPASS SPACE, LET SVC 99 DO IT.      00303140
*                                                                       00303150
*              PRIMARY SPACE IS ZERO, BUT DIRECTORY IS NOT.             00303160
*              IKJDAIR LOADS 'IEFAB445' TO GET DEFAULT SPACE VALUES.    00303170
*              WE CHOSE TO CODE THAT INFORMATION IN THIS                00303180
*              MODULE INSTEAD, IN THE SAME FORMAT AS IEFAB445.          00303190
*                                                                       00303200
         OI    DEFPTR,X'80'        INDICATE DEFAULTS PRESENT            00303210
         MVC   SV08BLK+1(3),DEFDRLH                                     00303220
         MVC   SV08PQTY+1(3),DEFPQTY                                    00303230
         MVC   SV08SQTY+1(3),DEFSQTY                                    00303240
         MVC   SV08DQTY,DA08DQTY   USE SPECIFIED DQTY                   00303250
         MVI   SV08CTL,0                                                00303260
         TM    DEFTYPE,DEFTRK                                           00303270
         BNO   *+8                                                      00303280
         OI    SV08CTL,DA08TRKS                                         00303290
         TM    DEFTYPE,DEFCYL                                           00303300
         BNO   *+8                                                      00303310
         OI    SV08CTL,DA08CYLS                                         00303320
         TM    DEFTYPE,DEFBLK                                           00303330
         BNO   *+8                                                      00303340
         OI    SV08CTL,DA08ABKL                                         00303350
         TM    DEFTYPE,DEFRLSE                                          00303360
         BNO   *+8                                                      00303370
         OI    SV08CTL,DA08RLSE                                         00303380
         TM    DEFTYPE,DEFCNTIG                                         00303390
         BNO   *+8                                                      00303400
         OI    SV08CTL,X'08'                                            00303410
         TM    DEFTYPE,DEFMIXG                                          00303420
         BNO   *+8                                                      00303430
         OI    SV08CTL,X'04'                                            00303440
         TM    DEFTYPE,DEFALX                                           00303450
         BNO   *+8                                                      00303460
         OI    SV08CTL,X'02'                                            00303470
         TM    DEFTYPE,DEFRND                                           00303480
         BNO   *+8                                                      00303490
         OI    SV08CTL,X'01'                                            00303500
*                                                                       00303510
*              PRIMARY SPACE IS NONZERO OR SPACE TYPE IS SPECIFIED      00303520
*                                                                       00303530
IN08PQ   LA    R4,WK99TXSP                                              00303540
         ST    R4,WK99TUSP                                              00303550
         LA    R15,DALPRIME                                             00303560
         STH   R15,S99TUKEY                                             00303570
         LA    R15,1                                                    00303580
         STH   R15,S99TUNUM                                             00303590
         LA    R15,3                                                    00303600
         STH   R15,S99TULNG                                             00303610
         MVC   S99TUPAR(3),SV08PQTY+1                                   00303620
*                                                                       00303630
*              PROCESS SECONDARY SPACE                                  00303640
*                                                                       00303650
         CLC   SV08SQTY+1(3),=XL3'00'                                   00303660
         BE    IN08S2X                                                  00303670
         LA    R4,WK99TXS2                                              00303680
         ST    R4,WK99TUS2                                              00303690
         LA    R15,DALSECND                                             00303700
         STH   R15,S99TUKEY                                             00303710
         LA    R15,1                                                    00303720
         STH   R15,S99TUNUM                                             00303730
         LA    R15,3                                                    00303740
         STH   R15,S99TULNG                                             00303750
         MVC   S99TUPAR(3),SV08SQTY+1                                   00303760
IN08S2X  EQU   *                                                        00303770
*                                                                       00303780
*              PROCESS DIRECTORY BLOCKS                                 00303790
*                                                                       00303800
         CLC   SV08DQTY+1(3),=XL3'00'                                   00303810
         BE    IN08DIX                                                  00303820
         LA    R4,WK99TXDI                                              00303830
         ST    R4,WK99TUDI                                              00303840
         LA    R15,DALDIR                                               00303850
         STH   R15,S99TUKEY                                             00303860
         LA    R15,1                                                    00303870
         STH   R15,S99TUNUM                                             00303880
         LA    R15,3                                                    00303890
         STH   R15,S99TULNG                                             00303900
         MVC   S99TUPAR(3),SV08DQTY+1                                   00303910
IN08DIX  EQU   *                                                        00303920
*                                                                       00303930
*              DETERMINE SPACE TYPE                                     00303940
*                                                                       00303950
         TM    SV08CTL,DA08TRKS+DA08ABKL                                00303960
         BZ    IN08TYX             BRANCH IF BOTH OFF                   00303970
         LA    R15,DALCYL          IN CYLINDERS                         00303980
         BO    IN08TY0             BRANCH IF BOTH ON                    00303990
         LA    R15,DALTRK          IN TRACKS                            00304000
         TM    SV08CTL,DA08TRKS                                         00304010
         BO    IN08TY0                                                  00304020
*                                                                       00304030
*              PROCESS SPACE TYPE (AVERAGE BLOCK)                       00304040
*                                                                       00304050
         LA    R4,WK99TXTY                                              00304060
         ST    R4,WK99TUTY                                              00304070
         LA    R15,DALBLKLN        IN AVERAGE BLOCKS                    00304080
         STH   R15,S99TUKEY                                             00304090
         LA    R15,1                                                    00304100
         STH   R15,S99TUNUM                                             00304110
         LA    R15,3                                                    00304120
         STH   R15,S99TULNG                                             00304130
         MVC   S99TUPAR(3),SV08BLK+1                                    00304140
         B     IN08TYX                                                  00304150
*                                                                       00304160
*              PROCESS SPACE TYPE (CYL OR TRK)                          00304170
*                                                                       00304180
IN08TY0  LA    R4,WK99TXTY                                              00304190
         ST    R4,WK99TUTY                                              00304200
         STH   R15,S99TUKEY                                             00304210
         LA    R15,0                                                    00304220
         STH   R15,S99TUNUM                                             00304230
IN08TYX  EQU   *                                                        00304240
*                                                                       00304250
IN08SPCX EQU   *                                                        00304260
*                                                                       00304270
*              IF NEW-OR-MOD AND ATRL.EQ.0 AND BLK.NE.0                 00304280
*              THEN SET UP DCB BLKSIZE                                  00304290
*                                                                       00304300
         CLI   DA08DSP1,DA08NEW                                         00304310
         BE    IN08BK1                                                  00304320
         CLI   DA08DSP1,DA08MOD                                         00304330
         BNE   IN08BKX                                                  00304340
IN08BK1  TM    DA08CTL,DA08ATRL    IS THERE AN ATTRIBUTE LIST           00304350
         BO    IN08BKX             YES, BYPASS                          00304360
         CLC   DA08BLK+2(2),=XL2'00' IS BLOCKS(SIZE) SPECIFIED          00304370
         BE    IN08BKX             YES, BYPASS                          00304380
         CLC   DE08BLKS,=XL2'00'   IS DCB BLKSIZE SPECIFIED             00304390
         BE    IN08BKX             YES, BYPASS                          00304400
*                                                                       00304410
*              DCB BLKSIZE FOR AVERAGE BLOCK WITHOUT ATTRIBUTE          00304420
*                                                                       00304430
         LA    R4,WK99TXBK                                              00304440
         ST    R4,WK99TUBK                                              00304450
         LA    R15,DALBLKSZ        DCB BLKSIZE                          00304460
         STH   R15,S99TUKEY                                             00304470
         LA    R15,1                                                    00304480
         STH   R15,S99TUNUM                                             00304490
         LA    R15,2                                                    00304500
         STH   R15,S99TULNG                                             00304510
         MVC   S99TUPAR(2),DA08BLK+2                                    00304520
IN08BKX  EQU   *                                                        00304530
*                                                                       00304540
*              PROCESS PERMANENTLY ALLOC                                00304550
*                                                                       00304560
         TM    DA08CTL,DA08PERM                                         00304570
         BZ    IN08PEX                                                  00304580
         LA    R4,WK99TXPE                                              00304590
         ST    R4,WK99TUPE                                              00304600
         LA    R15,DALPERMA                                             00304610
         STH   R15,S99TUKEY                                             00304620
         LA    R15,0                                                    00304630
         STH   R15,S99TUNUM                                             00304640
IN08PEX  EQU   *                                                        00304650
*                                                                       00304660
*              PROCESS RLSE                                             00304670
*                                                                       00304680
         TM    DEFPTR,X'80'        WERE DEFAULTS USED                   00304690
         BZ    IN08RL1             NO, BRANCH                           00304700
         TM    SV08CTL,DA08RLSE    YES, WAS RLSE IN DEFAULTS            00304710
         BO    IN08RLS             YES, BRANCH                          00304720
         B     IN08RLX             NO, BYPASS                           00304730
IN08RL1  TM    DA08CTL,DA08RLSE    RLSE SPECIFIED BY CALLER             00304740
         BZ    IN08RLX             NO, BRANCH                           00304750
IN08RLS  LA    R4,WK99TXRL                                              00304760
         ST    R4,WK99TURL                                              00304770
         LA    R15,DALRLSE                                              00304780
         STH   R15,S99TUKEY                                             00304790
         LA    R15,0                                                    00304800
         STH   R15,S99TUNUM                                             00304810
IN08RLX  EQU   *                                                        00304820
*                                                                       00304830
*              PROCESS MSVGP                                            00304840
*                                                                       00304850
         TM    DE08CTLX,DE08MSVG                                        00304860
         BZ    IN08MSX                                                  00304870
         CLI   DA08SER,X'40'                                            00304880
         BE    IN08MSX                                                  00304890
         LA    R4,WK99TXMS                                              00304900
         ST    R4,WK99TUMS                                              00304910
         LA    R15,DALMSVGP                                             00304920
         STH   R15,S99TUKEY                                             00304930
         LA    R15,1                                                    00304940
         STH   R15,S99TUNUM                                             00304950
         LA    R15,8                                                    00304960
         STH   R15,S99TULNG                                             00304970
         MVC   S99TUPAR(8),DA08SER                                      00304980
IN08MSX  EQU   *                                                        00304990
*                                                                       00305000
*              DCB DSORG                                                00305010
*                                                                       00305020
         CLI   DE08DSOR,0                                               00305030
         BE    IN08DSOX                                                 00305040
         LA    R4,WK99TXDO                                              00305050
         ST    R4,WK99TUDO                                              00305060
         LA    R15,DALDSORG        DCB DSORG                            00305070
         STH   R15,S99TUKEY                                             00305080
         LA    R15,1                                                    00305090
         STH   R15,S99TUNUM                                             00305100
         LA    R15,2                                                    00305110
         STH   R15,S99TULNG                                             00305120
         MVC   S99TUPAR(2),DE08DSOR                                     00305130
IN08DSOX EQU   *                                                        00305140
*                                                                       00305150
*              DCB RECFM                                                00305160
*                                                                       00305170
         CLI   DE08RECF,0                                               00305180
         BE    IN08RECX                                                 00305190
         LA    R4,WK99TXRF                                              00305200
         ST    R4,WK99TURF                                              00305210
         LA    R15,DALRECFM        DCB RECFM                            00305220
         STH   R15,S99TUKEY                                             00305230
         LA    R15,1                                                    00305240
         STH   R15,S99TUNUM                                             00305250
         LA    R15,1                                                    00305260
         STH   R15,S99TULNG                                             00305270
         MVC   S99TUPAR(1),DE08RECF                                     00305280
IN08RECX EQU   *                                                        00305290
*                                                                       00305300
*              DCB LRECL                                                00305310
*                                                                       00305320
         CLC   DE08LREC,=XL2'00'                                        00305330
         BE    IN08LREX                                                 00305340
         LA    R4,WK99TXLR                                              00305350
         ST    R4,WK99TULR                                              00305360
         LA    R15,DALLRECL        DCB LRECL                            00305370
         STH   R15,S99TUKEY                                             00305380
         LA    R15,1                                                    00305390
         STH   R15,S99TUNUM                                             00305400
         LA    R15,2                                                    00305410
         STH   R15,S99TULNG                                             00305420
         MVC   S99TUPAR(2),DE08LREC                                     00305430
IN08LREX EQU   *                                                        00305440
*                                                                       00305450
*              DCB BLKSIZE                                              00305460
*                                                                       00305470
         CLC   DE08BLKS,=XL2'00'                                        00305480
         BE    IN08BLKX                                                 00305490
         LA    R4,WK99TXBK                                              00305500
         ST    R4,WK99TUBK                                              00305510
         LA    R15,DALBLKSZ        DCB BLKSIZE                          00305520
         STH   R15,S99TUKEY                                             00305530
         LA    R15,1                                                    00305540
         STH   R15,S99TUNUM                                             00305550
         LA    R15,2                                                    00305560
         STH   R15,S99TULNG                                             00305570
         MVC   S99TUPAR(2),DE08BLKS                                     00305580
IN08BLKX EQU   *                                                        00305590
*                                                                       00305600
*               TAPE DATA SET SEQUENCE NUMBER                           00305610
*                                                                       00305620
         CLC   DE08POS,=XL2'00'                                         00305630
         BE    IN08POSX                                                 00305640
         LA    R4,WK99TXPO                                              00305650
         ST    R4,WK99TUPO                                              00305660
         LA    R15,DALDSSEQ        DATA SET SEQUENCE NUMBER             00305670
         STH   R15,S99TUKEY                                             00305680
         LA    R15,1                                                    00305690
         STH   R15,S99TUNUM                                             00305700
         LA    R15,2                                                    00305710
         STH   R15,S99TULNG                                             00305720
         MVC   S99TUPAR(2),DE08POS  MAX VALUE H'9999'                   00305730
IN08POSX EQU   *                                                        00305740
*                                                                       00305750
*               TAPE LABEL TYPE                                         00305760
*                                                                       00305770
         CLI   DE08LABL,0                                               00305780
         BE    IN08LBLX                                                 00305790
         LA    R4,WK99TXLB                                              00305800
         ST    R4,WK99TULB                                              00305810
         LA    R15,DALLABEL        LABEL TYPE                           00305820
         STH   R15,S99TUKEY                                             00305830
         LA    R15,1                                                    00305840
         STH   R15,S99TUNUM                                             00305850
         LA    R15,1                                                    00305860
         STH   R15,S99TULNG                                             00305870
         MVC   S99TUPAR(1),DE08LABL                                     00305880
IN08LBLX EQU   *                                                        00305890
*                                                                       00305900
*               TAPE DENSITY                                            00305910
*                                                                       00305920
         CLI   DE08DEN,0                                                00305930
         BE    IN08DENX                                                 00305940
         LA    R4,WK99TXDN                                              00305950
         ST    R4,WK99TUDN                                              00305960
         LA    R15,DALDEN          TAPE DENSITY                         00305970
         STH   R15,S99TUKEY                                             00305980
         LA    R15,1                                                    00305990
         STH   R15,S99TUNUM                                             00306000
         LA    R15,1                                                    00306010
         STH   R15,S99TULNG                                             00306020
         MVC   S99TUPAR(1),DE08DEN                                      00306030
IN08DENX EQU   *                                                        00306040
*                                                                       00306050
*               EXPIRATION DATE                                         00306060
*                                                                       00306070
         CLC   DE08EXPD,=XL3'00'                                        00306080
         BE    IN08EXPX                                                 00306090
         LA    R4,WK99TXEX                                              00306100
         ST    R4,WK99TUEX                                              00306110
         LA    R15,DALEXPDT        EXPIRATION DATE                      00306120
         STH   R15,S99TUKEY                                             00306130
         LA    R15,1                                                    00306140
         STH   R15,S99TUNUM                                             00306150
         LA    R15,5                                                    00306160
         STH   R15,S99TULNG                                             00306170
         SLR   R0,R0                                                    00306180
         IC    R0,DE08EXYR         YY                                   00306190
         CVD   R0,DOUBLE                                                00306200
         OI    DOUBLE+7,X'0F'                                           00306210
         UNPK  S99TUPAR(2),DOUBLE+6(2)                                  00306220
         LH    R0,DE08EXJD         DDD                                  00306230
         CVD   R0,DOUBLE                                                00306240
         OI    DOUBLE+7,X'0F'                                           00306250
         UNPK  S99TUPAR+2(3),DOUBLE+6(2)                                00306260
IN08EXPX EQU   *                                                        00306270
         DROP  R3                  DAPB08                               00306280
         B     ALLOC                                                    00306290
         SPACE                                                          00306300
         LTORG                                                          00306310
         EJECT                                                          00306320
*********************************************************************** 00306330
*                                                                       00306340
*         DAIR 1C - TERMINAL ALLOCATION                                 00306350
*                                                                       00306360
*********************************************************************** 00306370
         SPACE                                                          00306380
IN1C     DC    0H'0'                                                    00306390
         USING DAPB1C,R3                                                00306400
         SPACE                                                          00306410
         TM    DA1CCTL,DA1CATRL    ATTRIBUTE NAME PRESENT               00306420
         BZ    IN1CATOK            NO, BRANCH                           00306430
         CLI   DA1CALN,C' '        YES, IS IT BLANK                     00306440
         BE    EXIT4               YES, ERROR                           00306450
IN1CATOK EQU   *                                                        00306460
*                                                                       00306470
*              PROCESS TERMINAL                                         00306480
*                                                                       00306490
         LA    R4,WK99TXTE                                              00306500
         ST    R4,WK99TUTE                                              00306510
         LA    R15,DALTERM                                              00306520
         STH   R15,S99TUKEY                                             00306530
         LA    R15,0                                                    00306540
         STH   R15,S99TUNUM                                             00306550
*                                                                       00306560
*              PROCESS DDNAME                                           00306570
*                                                                       00306580
         MVI   WKDDRET,0                                                00306590
         LA    R4,WK99TXDD                                              00306600
         ST    R4,WK99TUDD                                              00306610
         CLI   DA1CDDN,X'40'       DD TO BE RETURNED?                   00306620
         BE    IN1CDDR             YES - BRANCH                         00306630
*                                                                       00306640
*              DDNAME IS SPECIFIED                                      00306650
*                                                                       00306660
         LA    R15,DALDDNAM                                             00306670
         STH   R15,S99TUKEY                                             00306680
         LA    R15,1                                                    00306690
         STH   R15,S99TUNUM                                             00306700
         LA    R15,8                                                    00306710
         STH   R15,S99TULNG                                             00306720
         MVC   S99TUPAR(8),DA1CDDN                                      00306730
         B     IN1CDDX                                                  00306740
*                                                                       00306750
*              DDNAME NOT SPECIFIED                                     00306760
*                                                                       00306770
IN1CDDR  LA    R15,DALRTDDN                                             00306780
         STH   R15,S99TUKEY                                             00306790
         LA    R15,1                                                    00306800
         STH   R15,S99TUNUM                                             00306810
         LA    R15,8                                                    00306820
         STH   R15,S99TULNG                                             00306830
         MVC   S99TUPAR(8),=CL8' '                                      00306840
         MVI   WKDDRET,C'R'                                             00306850
IN1CDDX  EQU   *                                                        00306860
*                                                                       00306870
*              PROCESS ATTRIBUTE LIST NAME                              00306880
*                                                                       00306890
         TM    DA1CCTL,DA1CATRL    IS ATTRIBUTE NAME PRESENT            00306900
         BZ    IN1CNOAT            NO, BRANCH                           00306910
         LA    R4,WK99TXAT                                              00306920
         ST    R4,WK99TUAT                                              00306930
         LA    R15,DALDCBDD                                             00306940
         STH   R15,S99TUKEY                                             00306950
         LA    R15,1                                                    00306960
         STH   R15,S99TUNUM                                             00306970
         LA    R15,8                                                    00306980
         STH   R15,S99TULNG                                             00306990
         MVC   S99TUPAR(8),DA1CALN                                      00307000
IN1CNOAT EQU   *                                                        00307010
*                                                                       00307020
*              PROCESS PERMANENTLY ALLOC                                00307030
*                                                                       00307040
         TM    DA1CCTL,DA1CPERM                                         00307050
         BZ    IN1CNOPE                                                 00307060
         LA    R4,WK99TXPE                                              00307070
         ST    R4,WK99TUPE                                              00307080
         LA    R15,DALPERMA                                             00307090
         STH   R15,S99TUKEY                                             00307100
         LA    R15,0                                                    00307110
         STH   R15,S99TUNUM                                             00307120
IN1CNOPE EQU   *                                                        00307130
*                                                                       00307140
*              DCB RECFM                                                00307150
*                                                                       00307160
         CLI   DE1CRECF,0                                               00307170
         BE    IN1CRECX                                                 00307180
         LA    R4,WK99TXRF                                              00307190
         ST    R4,WK99TURF                                              00307200
         LA    R15,DALRECFM        DCB RECFM                            00307210
         STH   R15,S99TUKEY                                             00307220
         LA    R15,1                                                    00307230
         STH   R15,S99TUNUM                                             00307240
         LA    R15,1                                                    00307250
         STH   R15,S99TULNG                                             00307260
         MVC   S99TUPAR(2),DE1CRECF                                     00307270
IN1CRECX EQU   *                                                        00307280
*                                                                       00307290
*              DCB LRECL                                                00307300
*                                                                       00307310
         CLC   DE1CLREC,=XL2'00'                                        00307320
         BE    IN1CLREX                                                 00307330
         LA    R4,WK99TXLR                                              00307340
         ST    R4,WK99TULR                                              00307350
         LA    R15,DALLRECL        DCB LRECL                            00307360
         STH   R15,S99TUKEY                                             00307370
         LA    R15,1                                                    00307380
         STH   R15,S99TUNUM                                             00307390
         LA    R15,2                                                    00307400
         STH   R15,S99TULNG                                             00307410
         MVC   S99TUPAR(2),DE1CLREC                                     00307420
IN1CLREX EQU   *                                                        00307430
*                                                                       00307440
*              DCB BLKSIZE                                              00307450
*                                                                       00307460
         CLC   DE1CBLKS,=XL2'00'                                        00307470
         BE    IN1CBLKX                                                 00307480
         LA    R4,WK99TXBK                                              00307490
         ST    R4,WK99TUBK                                              00307500
         LA    R15,DALBLKSZ        DCB BLKSIZE                          00307510
         STH   R15,S99TUKEY                                             00307520
         LA    R15,1                                                    00307530
         STH   R15,S99TUNUM                                             00307540
         LA    R15,2                                                    00307550
         STH   R15,S99TULNG                                             00307560
         MVC   S99TUPAR(2),DE1CBLKS                                     00307570
IN1CBLKX EQU   *                                                        00307580
         DROP  R3                  DAPB1C                               00307590
         B     ALLOC                                                    00307600
         SPACE                                                          00307610
         LTORG                                                          00307620
         EJECT                                                          00307630
*********************************************************************** 00307640
*                                                                       00307650
*         DAIR 30 - SYSOUT ALLOCATION                                   00307660
*                                                                       00307670
*********************************************************************** 00307680
         SPACE                                                          00307690
IN30     DC    0H'0'                                                    00307700
         USING DAPB30,R3                                                00307710
         SPACE                                                          00307720
         TM    DA30CTL,DA30ATRL    ATTRIBUTE NAME PRESENT               00307730
         BZ    IN30ATOK            NO, BRANCH                           00307740
         CLI   DA30ALN,C' '        YES, IS IT BLANK                     00307750
         BE    EXIT4               YES, ERROR                           00307760
IN30ATOK EQU   *                                                        00307770
         SPACE                                                          00307780
         MVI   WKDSRET,0                                                00307790
         L     R14,DA30PDSN        POINTER TO DSNAME                    00307800
         LTR   R14,R14             IS DSNAME PRESENT                    00307810
         BZ    IN30DSOK            NO, BRANCH                           00307820
         CLC   0(2,R14),=H'44'     YES, IS ITS LENGTH 44                00307830
         BNE   EXIT4               NO, ERROR                            00307840
         CLI   2(R14),X'40'        IS IT BLANK                          00307850
         BNE   EXIT4               NO, ERROR                            00307860
         MVI   WKDSRET,C'R'        YES, INDICATE RETURN DSNAME          00307870
IN30DSOK EQU   *                                                        00307880
         SPACE                                                          00307890
         CLI   DA30UNIT,0                                               00307900
         BE    EXIT4                                                    00307910
         SPACE                                                          00307920
*                                                                       00307930
*              PROCESS RETURN-DSNAME                                    00307940
*                                                                       00307950
         CLI   WKDSRET,C'R'                                             00307960
         BNE   IN30NODS                                                 00307970
         LA    R4,WK99TXDS                                              00307980
         ST    R4,WK99TUDS                                              00307990
         LA    R15,DALRTDSN                                             00308000
         STH   R15,S99TUKEY                                             00308010
         LA    R15,1                                                    00308020
         STH   R15,S99TUNUM                                             00308030
         LA    R15,44                                                   00308040
         STH   R15,S99TULNG                                             00308050
         MVI   S99TUPAR,C' '                                            00308060
         MVC   S99TUPAR+1(43),S99TUPAR                                  00308070
IN30NODS EQU   *                                                        00308080
*                                                                       00308090
*              PROCESS DDNAME                                           00308100
*                                                                       00308110
         MVI   WKDDRET,0                                                00308120
         LA    R4,WK99TXDD                                              00308130
         ST    R4,WK99TUDD                                              00308140
         CLI   DA30DDN,X'40'       DD TO BE RETURNED?                   00308150
         BE    IN30DDR             YES - BRANCH                         00308160
*                                                                       00308170
*              DDNAME IS SPECIFIED                                      00308180
*                                                                       00308190
         LA    R15,DALDDNAM                                             00308200
         STH   R15,S99TUKEY                                             00308210
         LA    R15,1                                                    00308220
         STH   R15,S99TUNUM                                             00308230
         LA    R15,8                                                    00308240
         STH   R15,S99TULNG                                             00308250
         MVC   S99TUPAR(8),DA30DDN                                      00308260
         B     IN30SO                                                   00308270
*                                                                       00308280
*              DDNAME NOT SPECIFIED                                     00308290
*                                                                       00308300
IN30DDR  LA    R15,DALRTDDN                                             00308310
         STH   R15,S99TUKEY                                             00308320
         LA    R15,1                                                    00308330
         STH   R15,S99TUNUM                                             00308340
         LA    R15,8                                                    00308350
         STH   R15,S99TULNG                                             00308360
         MVC   S99TUPAR(8),=CL8' '                                      00308370
         MVI   WKDDRET,C'R'                                             00308380
*                                                                       00308390
*              PROCESS SYSOUT CLASS                                     00308400
*                                                                       00308410
IN30SO   LA    R4,WK99TXSO                                              00308420
         ST    R4,WK99TUSO                                              00308430
         LA    R15,DALSYSOU                                             00308440
         STH   R15,S99TUKEY                                             00308450
         LA    R15,1                                                    00308460
         STH   R15,S99TUNUM                                             00308470
         LA    R15,1                                                    00308480
         STH   R15,S99TULNG                                             00308490
         MVC   S99TUPAR(1),DA30OCLS                                     00308500
         CLI   DA30OCLS,X'40'      CLASS SPECIFIED?                     00308510
         BE    IN30NOSO            NO - BRANCH                          00308520
         CLI   DA30OCLS,X'00'      CLASS SPECIFIED?                     00308530
         BNE   IN30SOX             YES - BRANCH                         00308540
IN30NOSO SR    R15,R15             INDICATE NULL ENTRY                  00308550
         STH   R15,S99TUNUM         FOR THIS KEY                        00308560
IN30SOX  EQU   *                                                        00308570
*                                                                       00308580
*              PROCESS PROGRAM NAME OR INTRDR                           00308590
*                                                                       00308600
         CLI   DA30PGNM,X'40'                                           00308610
         BE    IN30NOPG                                                 00308620
         CLI   DA30PGNM,X'00'                                           00308630
         BE    IN30NOPG                                                 00308640
         LA    R4,WK99TXPG                                              00308650
         ST    R4,WK99TUPG                                              00308660
         LA    R15,DALSPGNM                                             00308670
         STH   R15,S99TUKEY                                             00308680
         LA    R15,1                                                    00308690
         STH   R15,S99TUNUM                                             00308700
         LA    R15,8               SET LENGTH INITIALLY TO 8            00308710
         LA    R1,DA30PGNM+7 POINT TO 8TH CHAR OF NAME                  00308720
IN30PGLP CLI   0(R1),X'40'         BLANK?                               00308730
         BNE   IN30PGLN            NO - BRANCH TO STORE LENGTH          00308740
         BCTR  R15,0               YES - DECREMENT LENGTH BY 1          00308750
         BCT   R1,IN30PGLP         POINT BACK 1 CHAR AND BRANCH         00308760
IN30PGLN STH   R15,S99TULNG                                             00308770
         MVC   S99TUPAR(8),DA30PGNM                                     00308780
IN30NOPG EQU   *                                                        00308790
*                                                                       00308800
*              PROCESS ATTRIBUTE LIST NAME                              00308810
*                                                                       00308820
         TM    DA30CTL,DA30ATRL    IS ATTRIBUTE NAME PRESENT            00308830
         BZ    IN30NOAT            NO, BRANCH                           00308840
         LA    R4,WK99TXAT                                              00308850
         ST    R4,WK99TUAT                                              00308860
         LA    R15,DALDCBDD                                             00308870
         STH   R15,S99TUKEY                                             00308880
         LA    R15,1                                                    00308890
         STH   R15,S99TUNUM                                             00308900
         LA    R15,8               SET LENGTH INITIALLY TO 8            00308910
         LA    R1,DA30ALN+7 POINT TO 8TH CHAR OF NAME                   00308920
IN30ATLP CLI   0(R1),X'40'         BLANK?                               00308930
         BNE   IN30ATLN            NO - BRANCH TO STORE LENGTH          00308940
         BCTR  R15,0               YES - DECREMENT LENGTH BY 1          00308950
         BCT   R1,IN30ATLP         POINT BACK 1 CHAR AND BRANCH         00308960
IN30ATLN STH   R15,S99TULNG                                             00308970
         MVC   S99TUPAR(8),DA30ALN                                      00308980
IN30NOAT EQU   *                                                        00308990
*                                                                       00309000
*              PROCESS FORM                                             00309010
*                                                                       00309020
         CLI   DA30FORM,X'40'      FORM?                                00309030
         BE    IN30NOFO            NO - BRANCH                          00309040
         CLI   DA30FORM,X'00'      FORM?                                00309050
         BE    IN30NOFO                                                 00309060
         LA    R4,WK99TXFO                                              00309070
         ST    R4,WK99TUFO                                              00309080
         LA    R15,DALSFMNO                                             00309090
         STH   R15,S99TUKEY                                             00309100
         LA    R15,1                                                    00309110
         STH   R15,S99TUNUM                                             00309120
         LA    R15,4                                                    00309130
         STH   R15,S99TULNG                                             00309140
         MVC   S99TUPAR(4),DA30FORM                                     00309150
IN30NOFO EQU   *                                                        00309160
*                                                                       00309170
*              PROCESS PERMANENTLY ALLOC                                00309180
*                                                                       00309190
         TM    DA30CTL,DA30PERM                                         00309200
         BZ    IN30NOPE                                                 00309210
         LA    R4,WK99TXPE                                              00309220
         ST    R4,WK99TUPE                                              00309230
         LA    R15,DALPERMA                                             00309240
         STH   R15,S99TUKEY                                             00309250
         LA    R15,0                                                    00309260
         STH   R15,S99TUNUM                                             00309270
IN30NOPE EQU   *                                                        00309280
*                                                                       00309290
*              PROCESS COPIES                                           00309300
*                                                                       00309310
         CLI   DE30CPYS,1          IS COPIES ZERO OR 1                  00309320
         BNH   IN30NOCO            YES, DO NOT SPECIFY COPIES           00309330
         LA    R4,WK99TXCO                                              00309340
         ST    R4,WK99TUCO                                              00309350
         LA    R15,DALCOPYS                                             00309360
         STH   R15,S99TUKEY                                             00309370
         LA    R15,1                                                    00309380
         STH   R15,S99TUNUM                                             00309390
         LA    R15,1               SVC99 ONLY ALLOWS 1 BYTE.            00309400
         STH   R15,S99TULNG                                             00309410
         MVC   S99TUPAR(1),DE30CPYS                                     00309420
IN30NOCO EQU   *                                                        00309430
*                                                                       00309440
*              PROCESS DEST                                             00309450
*                                                                       00309460
         CLI   DE30DEST,X'40'      DEST?                                00309470
         BE    IN30NODE            NO - BRANCH                          00309480
         CLI   DE30DEST,X'00'      DEST?                                00309490
         BE    IN30NODE            NO - BRANCH                          00309500
         LA    R4,WK99TXDE                                              00309510
         ST    R4,WK99TUDE                                              00309520
         LA    R15,DALSUSER                                             00309530
         STH   R15,S99TUKEY                                             00309540
         LA    R15,1                                                    00309550
         STH   R15,S99TUNUM                                             00309560
         LA    R15,8                                                    00309570
         STH   R15,S99TULNG                                             00309580
         MVC   S99TUPAR(8),DE30DEST                                     00309590
IN30NODE EQU   *                                                        00309600
*                                                                       00309610
*              PROCESS HOLD                                             00309620
*                                                                       00309630
         TM    DE30CTLX,DE30HOLD   HOLD?                                00309640
         BZ    IN30NOHO            NO - BRANCH                          00309650
         LA    R4,WK99TXHO                                              00309660
         ST    R4,WK99TUHO                                              00309670
         LA    R15,DALSHOLD                                             00309680
         STH   R15,S99TUKEY                                             00309690
         LA    R15,0                                                    00309700
         STH   R15,S99TUNUM                                             00309710
IN30NOHO EQU   *                                                        00309720
*                                                                       00309730
*              PROCESS FREE=CLOSE                                       00309740
*                                                                       00309750
         CLC   DA30PGNM,=CL8'INTRDR' IF INTERNAL READER                 00309760
         BE    IN30FR              THEN FORCE FREE=CLOSE                00309770
         TM    DE30CTLX,DE30FC     FREE=CLOSE REQUESTED?                00309780
         BZ    IN30NOFR            NO - BRANCH                          00309790
IN30FR   LA    R4,WK99TXFC                                              00309800
         ST    R4,WK99TUFC                                              00309810
         LA    R15,DALCLOSE                                             00309820
         STH   R15,S99TUKEY                                             00309830
         LA    R15,0                                                    00309840
         STH   R15,S99TUNUM                                             00309850
IN30NOFR EQU   *                                                        00309860
*                                                                       00309870
*              PROCESS FCB                                              00309880
*                                                                       00309890
         CLI   DE30FCB,X'40'       FCB?                                 00309900
         BE    IN30NOFC            NO - BRANCH                          00309910
         CLI   DE30FCB,X'00'       FCB?                                 00309920
         BE    IN30NOFC            NO - BRANCH                          00309930
         LA    R4,WK99TXFC                                              00309940
         ST    R4,WK99TUFC                                              00309950
         LA    R15,DALFCBIM                                             00309960
         STH   R15,S99TUKEY                                             00309970
         LA    R15,1                                                    00309980
         STH   R15,S99TUNUM                                             00309990
         LA    R15,4                                                    00310000
         STH   R15,S99TULNG                                             00310010
         MVC   S99TUPAR(4),DE30FCB                                      00310020
IN30NOFC EQU   *                                                        00310030
*                                                                       00310040
*              PROCESS OTHER THINGS                                     00310050
*                                                                       00310060
         SPACE                                                          00310070
*              UNLIKE IKJDAIR, THIS PROGRAM IGNORES THE                 00310080
*              DA30UNIT FIELD, ALL FIELDS RELATED TO SPACE              00310090
*              (PRIMARY, SECONDARY, TRKS, ABKL, RLSE)                   00310100
*              AND THE DUMMY FLAG.                                      00310110
         SPACE                                                          00310120
*                                                                       00310130
*              DCB RECFM                                                00310140
*                                                                       00310150
         CLI   DE30RECF,0                                               00310160
         BE    IN30RECX                                                 00310170
         LA    R4,WK99TXRF                                              00310180
         ST    R4,WK99TURF                                              00310190
         LA    R15,DALRECFM        DCB RECFM                            00310200
         STH   R15,S99TUKEY                                             00310210
         LA    R15,1                                                    00310220
         STH   R15,S99TUNUM                                             00310230
         LA    R15,1                                                    00310240
         STH   R15,S99TULNG                                             00310250
         MVC   S99TUPAR(2),DE30RECF                                     00310260
IN30RECX EQU   *                                                        00310270
*                                                                       00310280
*              DCB LRECL                                                00310290
*                                                                       00310300
         CLC   DE30LREC,=XL2'00'                                        00310310
         BE    IN30LREX                                                 00310320
         LA    R4,WK99TXLR                                              00310330
         ST    R4,WK99TULR                                              00310340
         LA    R15,DALLRECL        DCB LRECL                            00310350
         STH   R15,S99TUKEY                                             00310360
         LA    R15,1                                                    00310370
         STH   R15,S99TUNUM                                             00310380
         LA    R15,2                                                    00310390
         STH   R15,S99TULNG                                             00310400
         MVC   S99TUPAR(2),DE30LREC                                     00310410
IN30LREX EQU   *                                                        00310420
*                                                                       00310430
*              DCB BLKSIZE                                              00310440
*                                                                       00310450
         CLC   DE30BLKS,=XL2'00'                                        00310460
         BE    IN30BLKX                                                 00310470
         LA    R4,WK99TXBK                                              00310480
         ST    R4,WK99TUBK                                              00310490
         LA    R15,DALBLKSZ        DCB BLKSIZE                          00310500
         STH   R15,S99TUKEY                                             00310510
         LA    R15,1                                                    00310520
         STH   R15,S99TUNUM                                             00310530
         LA    R15,2                                                    00310540
         STH   R15,S99TULNG                                             00310550
         MVC   S99TUPAR(2),DE30BLKS                                     00310560
IN30BLKX EQU   *                                                        00310570
         DROP  R3                  DAPB30                               00310580
         EJECT                                                          00310590
*********************************************************************** 00310600
*                                                                     * 00310610
*              ALLOCATE                                               * 00310620
*                                                                     * 00310630
*********************************************************************** 00310640
         SPACE                                                          00310650
ALLOC    LA    R1,WK99RBP                                               00310660
         SVC   99                                                       00310670
*                                                                       00310680
*              TEST RETURN CODE                                         00310690
*                                                                       00310700
         LTR   R15,R15             WAS IT SUCCESSFUL                    00310710
         BNZ   FAILED              NO, BRANCH                           00310720
         SPACE                                                          00310730
         USING DAPB08,R3                                                00310740
         CLI   1(R3),X'08'         IS THIS DA08                         00310750
         BNE   RETORGX             NO, BRANCH                           00310760
         LA    R4,WK99TXRO                                              00310770
         MVC   DA08DSO,S99TUPAR    RETURN 1 BYTE OF DSORG               00310780
RETORGX  EQU   *                                                        00310790
         SPACE                                                          00310800
         CLI   WKDSRET,C'R'        DSNAME TO BE RETURNED?               00310810
         BNE   IN08RDSX            NO - BRANCH                          00310820
         LA    R4,WK99TXDS                                              00310830
         L     R15,DA08PDSN        OR DA30PDSN                          00310840
         LH    R14,S99TULNG                                             00310850
         STH   R14,0(,R15)         RETURN LENGTH                        00310860
         BCTR  R14,0               LENGTH MINUS 1 FOR EX                00310870
         B     *+10                                                     00310880
         MVC   2(0,R15),S99TUPAR   (EXECUTED)                           00310890
         EX    R14,*-6             MOVE DSNAME                          00310900
         SR    R15,R15                                                  00310910
IN08RDSX EQU   *                                                        00310920
         SPACE                                                          00310930
         CLI   WKDDRET,C'R'        DDNAME TO BE RETURNED?               00310940
         BNE   IN08RDDX                                                 00310950
         LA    R4,WK99TXDD                                              00310960
         LA    R1,DA08DDN          OR DA30DDN                           00310970
         CLI   1(R3),X'1C'         IS THIS DA1C                         00310980
         BNE   *+8                 NO, SKIP NEXT INSTRUCTION            00310990
         LA    R1,DA1CDDN-DAPB1C(,R3) YES, POINT TO DA1CDDN             00311000
         MVC   0(8,R1),=CL8' '     PAD DAXXDDN WITH BLANKS              00311010
         LH    R14,S99TULNG                                             00311020
         BCTR  R14,0               LENGTH MINUS 1 FOR EX                00311030
         B     *+10                                                     00311040
         MVC   0(0,R1),S99TUPAR    (EXECUTED)                           00311050
         EX    R14,*-6             MOVE DDNAME                          00311060
IN08RDDX EQU   *                                                        00311070
         SPACE                                                          00311080
         CLI   1(R3),X'08'         IS THIS DA08                         00311090
         BNE   EXIT0               NO, BRANCH                           00311100
         CLI   DA08MNM,C' '        WAS MEMBER SPECIFIED                 00311110
         BE    EXIT0               NO, BRANCH                           00311120
         TM    DA08DSO,DSORGPO     IS DSORG PO (OR POU)                 00311130
         BO    EXIT0               YES, BRANCH                          00311140
*                                                                       00311150
*               MEMBER SPECIFIED FOR NON-PDS.  UNALLOCATE IT.           00311160
*                                                                       00311170
         LA    R14,WK99RB                                               00311180
         USING S99RB,R14                                                00311190
         MVI   S99VERB,S99VRBUN                                         00311200
         LA    R1,WK99FREE                                              00311210
         ST    R1,S99TXTPP                                              00311220
         DROP  R14                 S99RB                                00311230
*                                                                       00311240
         LA    R4,WK99FXDD                                              00311250
         ST    R4,WK99FREE                                              00311260
         LA    R15,DUNDDNAM                                             00311270
         STH   R15,S99TUKEY                                             00311280
         LA    R15,1                                                    00311290
         STH   R15,S99TUNUM                                             00311300
         LA    R15,8                                                    00311310
         STH   R15,S99TULNG                                             00311320
         MVC   S99TUPAR(8),DA08DDN                                      00311330
*                                                                       00311340
         LA    R4,WK99FXDD                                              00311350
         ST    R4,WK99FREE+4                                            00311360
         LA    R15,DUNREMOV                                             00311370
         STH   R15,S99TUKEY                                             00311380
         LA    R15,0                                                    00311390
         STH   R15,S99TUNUM                                             00311400
*                                                                       00311410
         OI    WK99FREE+4,X'80'                                         00311420
         LA    R1,WK99RBP                                               00311430
         SVC   99                                                       00311440
         CLI   WKDDRET,C'R'        WAS DDNAME ORIGINALLY BLANK          00311450
         BNE   *+10                NO, BRANCH AROUND MVC                00311460
         MVC   DA08DDN,=CL8' '     YES, RESTORE THE BLANKS              00311470
         MVC   DA08DARC,=X'0330'   SET ERROR CODE IN DARC               00311480
         LA    R15,12                                                   00311490
         B     EXIT                                                     00311500
*                                                                       00311510
*         RETURN TO CALLER                                              00311520
*                                                                       00311530
EXIT4    LA    R15,4               RETURN CODE 4                        00311540
         B     EXIT                                                     00311550
EXIT0    SR    15,15               RETURN CODE ZERO                     00311560
EXIT     LA    R0,@DATAL                                                00311570
         LA    R1,1                SUBPOOL                              00311580
         SLL   R1,24               TO LEFT BYTE                         00311590
         OR    R0,R1               OVER R0                              00311600
         LR    R1,R13              AREA TO BE FREED                     00311610
         L     R13,4(,R13)         RESTORE OLD R13                      00311620
         LR    R2,R15              SAVE RETURN CODE                     00311630
         FREEMAIN R,A=(1),LV=(0)                                        00311640
         LR    R15,R2              RESTORE RETURN CODE                  00311650
         LM    0,12,20(R13)        RESTORE OLD REGISTERS                00311660
         L     R14,12(,R13)        RESTORE RETURN ADDRESS               00311670
         BR    R14                 RETURN                               00311680
         SPACE 3                                                        00311690
         LTORG                                                          00311700
         DC    0D'0'                                                    00311710
DEFAULTS DS    0CL21               FROM IEFAB445                        00311720
DEFSPACE DS    0CL13                                                    00311730
DEFPQTY  DC    AL3(10)                                                  00311740
DEFSQTY  DC    AL3(50)                                                  00311750
DEFDRLH  DC    AL3(1000)                                                00311760
DEFDQTY  DC    AL3(0)                                                   00311770
DEFTYPE  DC    X'30'                                                    00311780
DEFTRK   EQU   X'80'                                                    00311790
DEFCYL   EQU   X'40'                                                    00311800
DEFBLK   EQU   X'20'                                                    00311810
DEFRLSE  EQU   X'10'                                                    00311820
DEFCNTIG EQU   X'08'                                                    00311830
DEFMIXG  EQU   X'04'                                                    00311840
DEFALX   EQU   X'02'                                                    00311850
DEFRND   EQU   X'01'                                                    00311860
DEFUNIT  DC    CL8'SYSALLDA'                                            00311870
         DC    0D'0'                                                    00311880
         EJECT                                                          00311890
*********************************************************************** 00311900
*                                                                       00311910
*         DAIR RETURN CODES AND ERROR CODES                             00311920
*                                                                       00311930
*********************************************************************** 00311940
*                                                                       00311950
*         DAIR RETURN CODES                                             00311960
*                                                                       00311970
*           0 - SUCCESS                                                 00311980
*           4 - INVALID PARAMETER LIST                                  00311990
*           8 - ERROR IN CATALOG MANAGEMENT, CTRC                       00312000
*          12 - ERROR IN DYNAMIC ALLOCATION, DARC                       00312010
*          16 - NO TIOT ENTRIES AVAILABLE          FROM 043C, 0450      00312020
*          20 - DDNAME UNAVAILABLE                 FROM 0410            00312030
*          24 - DSN IS PART OF A CONCATENATION                          00312040
*          28 - DD REFERBACK NOT FOUND             FROM 0454            00312050
*          32 - DATA REQUESTED AS NEW EXISTS       FROM 0448            00312060
*          36 - ERROR IN IKJEHCIR                                       00312070
*          40 - RETURN AREA FOR QUALIFIERS SMALL                        00312080
*          44 - DATA SET FOUND BUT MARKED DELETE   FROM 044C            00312090
*          48 - RESERVED                                                00312100
*          52 - REQUEST DENIED BY EXIT IEFDB401    FROM RC=8            00312110
*                                                                       00312120
*********************************************************************** 00312130
*                                                                       00312140
*         DAIR ERROR CODES (IN DAXXDARC)                                00312150
*                                                                       00312160
*          0304 - DD NOT SPECIFIED                                      00312170
*          0308 - DD NOT FOUND                                          00312180
*          0314 - DECAT WOULD CAUSE DUPES      FROM 0424                00312190
*          0318 - INVALID DDNAME               FROM 035C FOR 0001       00312200
*          031C - INVALID MEMBER               FROM 035C FOR 0003       00312210
*          0320 - INVALID DSNAME               FROM 035C FOR 0002       00312220
*          0324 - INVALID SYSOUT PGM           FROM 035C FOR 0019       00312230
*          0328 - INVALID SYSOUT FORM          FROM 035C FOR 001A       00312240
*          032C - INVALID SYSOUT CLASS         FROM 035C FOR 0018       00312250
*          0330 - MEMBER SPECIFIED BUT NOT PDS                          00312260
*          0334 - DSNAME LENGTH GT 44          FROM 037C FOR 0002       00312270
*          0338 - INVALID DISP                 FROM 035C FOR 0004,5,6   00312280
*                                                                       00312290
*********************************************************************** 00312300
         EJECT                                                          00312310
*                                                                       00312320
*               CONVERT ERROR CODES TO DAIR CODES                       00312330
*                                                                       00312340
FAILED   CLI   1(R3),X'1C'         IS THIS A TYPE 1C                    00312350
         BNE   FAILED30                                                 00312360
         TM    DE1CCTLX-DAPB1C(R3),DE1CMSG                              00312370
         BO    FAILMSG             YES, BRANCH                          00312380
         B     FAILNMSG            NO, BYPASS MESSAGE                   00312390
FAILED30 CLI   1(R3),X'30'         IS THIS A TYPE 30                    00312400
         BNE   FAILED08                                                 00312410
         TM    DE30CTLX-DAPB30(R3),DE30MSG                              00312420
         BO    FAILMSG             YES, BRANCH                          00312430
         B     FAILNMSG            NO, BYPASS MESSAGE                   00312440
FAILED08 TM    DE08CTLX,DE08MSG    AUTOMATIC MESSAGE                    00312450
         BZ    FAILNMSG            NO, BRANCH                           00312460
FAILMSG  BAL   R8,PUTMSG           YES, CALL IKJEFF18                   00312470
FAILNMSG LA    R14,WK99RB                                               00312480
         USING S99RB,R14                                                00312490
FAIL04   CH    R15,=H'4'           UNABLE TO ALLOCATE                   00312500
         BNE   FAIL08                                                   00312510
FAIL04A  CLC   S99ERROR,=X'0410'   RCDDUNAV                             00312520
         BNE   FAIL04B                                                  00312530
         LA    R15,20              RETURN CODE 20                       00312540
         B     FAIL04X                                                  00312550
FAIL04B  CLC   S99ERROR,=X'0424'   RCDCONDD, DECAT WOULD MAKE DUPLICATE 00312560
         BNE   FAIL04C                                                  00312570
         MVC   DA08DARC,=X'0314'   DRDUPEDD                             00312580
         LA    R15,12                                                   00312590
         B     FAIL04X                                                  00312600
FAIL04C  CLC   S99ERROR,=X'043C'   RCNTUNLC, DYNAMNBR TOO SMALL         00312610
         BE    FAIL04C1                                                 00312620
         CLC   S99ERROR,=X'0450'   RCLIMITS, MORE THAN 1635             00312630
         BNE   FAIL04D                                                  00312640
FAIL04C1 LA    R15,16              RETURN CODE 16                       00312650
         B     FAIL04X                                                  00312660
FAIL04D  CLC   S99ERROR,=X'0448'   RCNEWFND,                            00312670
         BNE   FAIL04E                                                  00312680
         LA    R15,32              RETURN CODE 32                       00312690
         B     FAIL04X                                                  00312700
FAIL04E  CLC   S99ERROR,=X'044C'   RCDELETE,                            00312710
         BNE   FAIL04F                                                  00312720
         LA    R15,44              RETURN CODE 44                       00312730
         B     FAIL04X                                                  00312740
FAIL04F  CLC   S99ERROR,=X'0454'   RCDCBRNF, ATTRIB DD NOT FOUND        00312750
         BNE   FAIL04G                                                  00312760
         LA    R15,28              RETURN CODE 28                       00312770
         B     FAIL04X                                                  00312780
FAIL04G  CLI   S99ERROR,X'17'      LOCATE ERROR                         00312790
         BE    FAIL04G1                                                 00312800
         CLI   S99ERROR,X'57'      CATALOG ERROR                        00312810
         BNE   FAIL04H                                                  00312820
FAIL04G1 SLR   R1,R1                                                    00312830
         IC    R1,S99ERROR+1                                            00312840
         STH   R1,DA08CTRC         CTRC                                 00312850
         LA    R15,8               RETURN CODE 8                        00312860
         CLI   S99ERROR,X'17'      LOCATE                               00312870
         BNE   FAIL04X                                                  00312880
         MVC   DA08DARC,S99ERROR   DARC TOO IF LOCATE                   00312890
         B     FAIL04X                                                  00312900
FAIL04H  MVC   DA08DARC,S99ERROR   DARC SAME AS S99ERROR                00312910
         LA    R15,12              RETURN CODE 12                       00312920
FAIL04X  B     EXIT                                                     00312930
         SPACE                                                          00312940
FAIL08   CH    R15,=H'8'           REQUEST DENIED BY EXIT               00312950
         BNE   FAIL12                                                   00312960
         LA    R15,52              REQUEST DENIED                       00312970
         B     EXIT                                                     00312980
         SPACE                                                          00312990
FAIL12   CH    R15,=H'12'          INVALID PARAMETER                    00313000
         BNE   *+4                                                      00313010
         CLC   S99ERROR,=X'035C'   RCINPARM                             00313020
         BNE   FAIL12V             NO, BRANCH                           00313030
FAIL12A  CLC   S99INFO,=AL2(DALDDNAM)                                   00313040
         BNE   FAIL12B                                                  00313050
         MVC   DA08DARC,=X'0318'   DRDDNINV, INVALID DDNAME             00313060
         B     FAIL12X                                                  00313070
FAIL12B  CLC   S99INFO,=AL2(DALDSNAM)                                   00313080
         BNE   FAIL12C                                                  00313090
         MVC   DA08DARC,=X'0320'   DRDSNINV, INVALID DSNAME             00313100
         B     FAIL12X                                                  00313110
FAIL12C  CLC   S99INFO,=AL2(DALMEMBR)                                   00313120
         BNE   FAIL12D                                                  00313130
         MVC   DA08DARC,=X'031C'   DRMBRINV, INVALID MEMBER NAME        00313140
         B     FAIL12X                                                  00313150
FAIL12D  CLC   S99INFO,=AL2(DALSTATS)                                   00313160
         BE    FAIL12D1                                                 00313170
         CLC   S99INFO,=AL2(DALNDISP)                                   00313180
         BE    FAIL12D1                                                 00313190
         CLC   S99INFO,=AL2(DALCDISP)                                   00313200
         BNE   FAIL12E                                                  00313210
FAIL12D1 MVC   DA08DARC,=X'0338'   DRDSPINV, INVALID DISP               00313220
         B     FAIL12X                                                  00313230
FAIL12E  CLC   S99INFO,=AL2(DALSPGNM)                                   00313240
         BNE   FAIL12F                                                  00313250
         MVC   DA08DARC,=X'0324'   DRSYSPGM, INVALID SYSOUT PGM         00313260
         B     FAIL12X                                                  00313270
FAIL12F  CLC   S99INFO,=AL2(DALSFMNO)                                   00313280
         BNE   FAIL12G                                                  00313290
         MVC   DA08DARC,=X'0328'   DRSYSFRM, INVALID FORM               00313300
         B     FAIL12X                                                  00313310
FAIL12G  CLC   S99INFO,=AL2(DALSYSOU) (THIS TEST NOT IN IKJDAIR)        00313320
         BNE   FAIL12S                                                  00313330
         MVC   DA08DARC,=X'032C'   DR      , INVALID CLASS              00313340
         B     FAIL12X                                                  00313350
FAIL12V  CLC   S99ERROR,=X'037C'   RCINKEYL, INVALID LENGTH             00313360
         BNE   FAIL12S                                                  00313370
         CLC   S99INFO,=AL2(DALDSNAM)                                   00313380
         BNE   FAIL12S                                                  00313390
         MVC   DA08DARC,=X'0334'   DRDSNLEN                             00313400
         B     FAIL12X                                                  00313410
FAIL12S  LA    R15,4               INVALID PARAMETER                    00313420
*        MVC   DA08DARC,S99ERROR   (DARC NOT EXAMINED IF RC 4)          00313430
         B     EXIT                                                     00313440
FAIL12X  LA    R15,12                                                   00313450
         B     EXIT                                                     00313460
         DROP  R14                 S99RB                                00313470
*                                                                       00313480
*               ISSUE APPROPRIATE ERROR MESSAGE                         00313490
*                                                                       00313500
PUTMSG   LA    R1,OURCPPL                                               00313510
         USING CPPL,R1                                                  00313520
         MVC   CPPLUPT(4),DAPLUPT                                       00313530
         MVC   CPPLPSCB(4),DAPLPSCB                                     00313540
         MVC   CPPLECT(4),DAPLECT                                       00313550
         DROP  R1                  CPPL                                 00313560
         LA    R1,WKDFAREA                                              00313570
         USING DFDSECTD,R1                                              00313580
         ST    R15,WKDFRC          STORE SVC 99 RETURN CODE             00313590
         LA    R15,WKDFRC                                               00313600
         ST    R15,DFRCP           STORE ADDRESS OF RETURN CODE         00313610
         LA    R15,WK99RB                                               00313620
         ST    R15,DFS99RBP        STORE ADDRESS OF SVC 99 REQUEST BLK  00313630
         SR    R15,R15                                                  00313640
         ST    R15,WKJEFF02        STORE ZERO IN IKJEFF02 ADDRESS       00313650
         LA    R15,WKJEFF02                                             00313660
         ST    R15,DFJEFF02        STORE ADDRESS OF ZERO                00313670
         LA    R15,DFSVC99                                              00313680
         STH   R15,WKDFID          STORE TYPE OF REQUEST (SVC 99)       00313690
         LA    R15,WKDFID                                               00313700
         ST    R15,DFIDP           STORE ADDRESS OF TYPE                00313710
         LA    R15,OURCPPL                                              00313720
         ST    R15,DFCPPLP         STORE CPPL ADDRESS OR ZERO           00313730
*        CLC   DFCPPLP+1(3),=AL3(0) CPPL ADDRESS SUPPLIED               00313740
*        BNE   *+8                 YES - USE PUTLINE                    00313750
*        OI    WKDFID,DFWTP        NO - USE WRITE-TO-PROGRAMMER         00313760
         XC    LINKAREA(8),LINKAREA                                     00313770
         LINK  EP=IKJEFF18,SF=(E,LINKAREA)                              00313780
         L     R15,WKDFRC          RETURN SVC 99 RETURN CODE            00313790
         BR    R8                                                       00313800
         SPACE                                                          00313810
         LTORG                                                          00313820
         DC    0D'0'                                                    00313830
         SPACE 3                                                        00313840
*********************************************************************** 00313850
*                                                                     * 00313860
*         MAP OF GETMAINED WORK SPACE                                 * 00313870
*                                                                     * 00313880
*********************************************************************** 00313890
         SPACE                                                          00313900
@DATA    DSECT                                                          00313910
         DS    18F                                                      00313920
WK99RBP  DS    A                   SVC 99 RB POINTER                    00313930
WK99RB   DS    5A                  SVC 99 REQUEST BLOCK                 00313940
*                                                                       00313950
* TEXT UNIT POINTERS                                                    00313960
*                                                                       00313970
WK99TUPL DS    0A                  SVC 99 TEXT LIST POINTERS            00313980
WK99TURO DS    A       08          TEXT UNIT POINTER, RETURN DSORG      00313990
WK99TUDS DS    A       08 30       TEXT UNIT POINTER, DSNAME            00314000
WK99TUDD DS    A       08 30       TEXT UNIT POINTER, DDNAME            00314010
WK99TUTE DS    0A            1C    TEXT UNIT POINTER, TERMINAL          00314020
WK99TUSO DS    A          30       TEXT UNIT POINTER, SYSOUT CLASS      00314030
WK99TUCO DS    A          30       TEXT UNIT POINTER, COPIES            00314040
WK99TUDE DS    A          30       TEXT UNIT POINTER, DEST              00314050
WK99TUHO DS    A          30       TEXT UNIT POINTER, HOLD              00314060
WK99TUPG DS    A          30       TEXT UNIT POINTER, PGM NAME          00314070
WK99TUPE DS    A       08 30       TEXT UNIT POINTER, PERM              00314080
WK99TUFR DS    A          30       TEXT UNIT POINTER, FREE=CLOSE        00314090
WK99TUFC DS    A          30       TEXT UNIT POINTER, FCB               00314100
WK99TUFO DS    A       08 30       TEXT UNIT POINTER, FORM              00314110
WK99TUAT DS    A       08          TEXT UNIT POINTER, ATTR LIST NAME    00314120
WK99TUME DS    A       08          TEXT UNIT POINTER, MEMBER            00314130
WK99TUUN DS    A       08          TEXT UNIT POINTER, UNIT              00314140
WK99TUMS DS    0A      08          TEXT UNIT POINTER, MSVGP             00314150
WK99TUVL DS    A       08          TEXT UNIT POINTER, VOLUME SERIAL     00314160
WK99TUD1 DS    A       08          TEXT UNIT POINTER, STATUS            00314170
WK99TUD2 DS    A       08          TEXT UNIT POINTER, NORMAL DISP       00314180
WK99TUD3 DS    A       08          TEXT UNIT POINTER, CONDITIONAL DISP  00314190
WK99TUPW DS    A       08          TEXT UNIT POINTER, PASSWORD          00314200
WK99TUDM DS    A       08          TEXT UNIT POINTER, DUMMY             00314210
WK99TUSP DS    A       08          TEXT UNIT POINTER, PRIMARY SPACE     00314220
WK99TUS2 DS    A       08          TEXT UNIT POINTER, SECONDARY SPACE   00314230
WK99TUDI DS    A       08          TEXT UNIT POINTER, DIRECTORY BLOCKS  00314240
WK99TUTY DS    A       08          TEXT UNIT POINTER, SPACE TYPE        00314250
WK99TURL DS    A       08          TEXT UNIT POINTER, RLSE              00314260
WK99TUDO DS    A       08          TEXT UNIT POINTER, DCB DSORG         00314270
WK99TULR DS    A       08 30       TEXT UNIT POINTER, DCB LRECL         00314280
WK99TUBK DS    A       08 30       TEXT UNIT POINTER, DCB BLKSIZE       00314290
WK99TURF DS    A       08 30       TEXT UNIT POINTER, DCB RECFM         00314300
WK99TUPO DS    A       08          TEXT UNIT POINTER, TAPE POS          00314310
WK99TULB DS    A       08          TEXT UNIT POINTER, LABEL TYPE        00314320
WK99TUDN DS    A       08          TEXT UNIT POINTER, TAPE DENSITY      00314330
WK99TUEX DS    A       08          TEXT UNIT POINTER, EXPDT             00314340
         DS    A                   HI ORDER BIT ON                      00314350
TUPL$LEN EQU   *-WK99TUPL                                               00314360
*                                                                       00314370
WK99FREE DS    2A                  TEXT UNIT POINTERS, UNALLOCATE       00314380
WK99FXDD DS    3H,CL8              TEXT UNIT, DDNAME UNALLOCATE         00314390
WK99FXRM DS    3H                  TEXT UNIT, REMOVE IN-USE ATTRIBUTE   00314400
*                                                                       00314410
* TEXT UNITS                                                            00314420
*                                                                       00314430
WK99TXRO DS    3H,H                TEXT UNIT, RETURN DSORG              00314440
WK99TXDS DS    3H,CL44             TEXT UNIT, DSNAME                    00314450
WK99TXDD DS    3H,CL8              TEXT UNIT, DDNAME                    00314460
WK99TXSO DS    3H,CL1              TEXT UNIT, SYSOUT CLASS              00314470
WK99TXCO DS    3H,H                TEXT UNIT, COPIES                    00314480
WK99TXDE DS    3H,CL8              TEXT UNIT, DEST                      00314490
WK99TXHO DS    3H                  TEXT UNIT, HOLD=YES                  00314500
WK99TXPG DS    3H,CL8              TEXT UNIT, PROGRAM NAME              00314510
WK99TXPE DS    3H                  TEXT UNIT, PERM                      00314520
WK99TXFR DS    3H                  TEXT UNIT, FREE=CLOSE                00314530
WK99TXFC DS    3H,CL4              TEXT UNIT, FCB                       00314540
WK99TXFO DS    3H,CL4              TEXT UNIT, FORM NUMBER               00314550
WK99TXAT DS    3H,CL8              TEXT UNIT, ATTRIBUTE LIST NAME       00314560
WK99TXME DS    3H,CL8              TEXT UNIT, MEMBER                    00314570
WK99TXUN DS    3H,CL8              TEXT UNIT, UNIT                      00314580
WK99TXMS DS    3H,CL8              TEXT UNIT, MSVGP                     00314590
WK99TXVL DS    3H,CL6              TEXT UNIT, VOLUME SERIAL             00314600
WK99TXD1 DS    3H,CL1              TEXT UNIT, STATUS                    00314610
WK99TXD2 DS    3H,CL1              TEXT UNIT, NORMAL DISP               00314620
WK99TXD3 DS    3H,CL1              TEXT UNIT, CONDITIONAL DISP          00314630
WK99TXPW DS    3H,CL8              TEXT UNIT, PASSWORD                  00314640
WK99TXDM DS    3H                  TEXT UNIT, DUMMY                     00314650
WK99TXSP DS    3H,CL3              TEXT UNIT, PRIMARY SPACE             00314660
WK99TXS2 DS    3H,CL3              TEXT UNIT, SECONDARY SPACE           00314670
WK99TXDI DS    3H,CL3              TEXT UNIT, DIRECTORY BLOCKS          00314680
WK99TXTY DS    3H,CL3              TEXT UNIT, SPACE TYPE AND BLOCK LEN  00314690
WK99TXRL DS    3H                  TEXT UNIT, RLSE                      00314700
WK99TXDO DS    3H,H                TEXT UNIT, DCB DSORG                 00314710
WK99TXLR DS    3H,H                TEXT UNIT, DCB LRECL                 00314720
WK99TXBK DS    3H,H                TEXT UNIT, DCB BLKSIZE               00314730
WK99TXRF DS    3H,C                TEXT UNIT, DCB RECFM                 00314740
WK99TXPO DS    3H,H                TEXT UNIT, TAPE POS                  00314750
WK99TXLB DS    3H,C                TEXT UNIT, TAPE LABEL TYPE           00314760
WK99TXDN DS    3H,C                TEXT UNIT, TAPE DENSITY              00314770
WK99TXEX DS    3H,CL5              TEXT UNIT, EXPDT                     00314780
WK99TXTE DS    3H                  TEXT UNIT, TERMINAL                  00314790
*                                                                       00314800
*                                                                       00314810
*                                                                       00314820
WKDFAREA DS    5F                  IKJEFF18 PARAMETER BLOCK             00314830
WKDFRC   DS    F                   SVC 99 RETURN CODE                   00314840
WKJEFF02 DS    F                   MESSAGE ROUTINE (ZERO)               00314850
WKDFID   DS    H                   TYPE OF FAILURE (SVC 99)             00314860
WKDSRET  DS    C                   SWITCH                               00314870
WKDDRET  DS    C                   SWITCH                               00314880
LINKAREA DS    2F                                                       00314890
OURCPPL  DS    4F                                                       00314900
DSNAMEL  DS    H                                                        00314910
DSNAME   DS    CL44                                                     00314920
SV08BLK  DS    F                                                        00314930
SV08PQTY DS    F                                                        00314940
SV08SQTY DS    F                                                        00314950
SV08DQTY DS    F                                                        00314960
SV08CTL  DS    C                                                        00314970
DEFPTR   DS    C                                                        00314980
DOUBLE   DS    D                                                        00314990
         DS    0D                                                       00315000
@DATAL   EQU   *-@DATA                                                  00315010
         EJECT                                                          00315020
         IEFZB4D0                                                       00315030
         EJECT                                                          00315040
         IEFZB4D2                                                       00315050
         EJECT                                                          00315060
         IKJEFFDF DFDSECT=YES                                           00315070
         SPACE 3                                                        00315080
         IKJCPPL                                                        00315090
         SPACE 3                                                        00315100
         IKJUPT                                                         00315110
         SPACE 3                                                        00315120
         IKJDAPL                                                        00315130
         SPACE 3                                                        00315140
         IKJDAP08                                                       00315150
DE08CTLX DS    X        FLAGS                                           00315160
DE08MSG  EQU   X'80'    AUTOMATIC ERROR MESSAGE                         00315170
DE08FC   EQU   X'40'    FREE=CLOSE (NOT IMPLEMENTED YET)                00315180
DE08MSVG EQU   X'20'    MSVGP IS IN VOL SER FIELD                       00315190
DE08RET  EQU   X'10'    RETENTION PERIOD, NOT EXPIRATION DATE           00315200
DE08NEX  EQU   X'01'    DO NOT USE AN EXISTING ALLOCATION.              00315210
DE08RECF DS    X        DCB RECFM                                       00315220
DE08LREC DS    H        DCB LRECL                                       00315230
DE08BLKS DS    H        DCB BLKSIZE                                     00315240
DE08DSOR DS    H        DCB DSORG                                       00315250
*                       END OF 8 BYTE SECTION                           00315260
         DS    X        RESERVED                                        00315270
DE08EXPD DS    0XL3     EXP DATE, BINARY, 1 BYTE YEAR, 2 BYTES DAY      00315280
DE08EXYR DS    X        EXP DATE YEAR, BINARY                           00315290
DE08EXJD DS    0H       EXP DATE JULIAN DAY, BINARY                     00315300
DE08RETP DS    H        RETENTION PERIOD (0-9999)                       00315310
DE08POS  DS    H        TAPE DATA SET SEQUENCE NUMBER (0-9999)          00315320
DE08LABL DS    X        TAPE LABEL TYPE (HEX 01 02 10 FOR NL SL BLP)    00315330
DE08DEN  DS    X        TAPE DENSITY (HEX 83 C3 D3 FOR 800 1600 6250)   00315340
*                       END OF 8 BYTE SECTION                           00315350
*              DAPB08 HAS BEEN EXTENDED BY 16 BYTES                     00315360
         SPACE 3                                                        00315370
         IKJDAP1C                                                       00315380
DE1CCTLX DS    X        FLAGS                                           00315390
DE1CMSG  EQU   X'80'    AUTOMATIC ERROR MESSAGE                         00315400
DE1CRECF DS    X        DCB RECFM                                       00315410
DE1CLREC DS    H        DCB LRECL                                       00315420
DE1CBLKS DS    H        DCB BLKSIZE                                     00315430
         DS    XL2      RESERVED                                        00315440
*              DAPB1C HAS BEEN EXTENDED BY 08 BYTES                     00315450
         SPACE 3                                                        00315460
         IKJDAP30                                                       00315470
DE30CTLX DS    X        FLAGS                                           00315480
DE30MSG  EQU   X'80'    AUTOMATIC ERROR MESSAGE                         00315490
DE30FC   EQU   X'40'    FREE=CLOSE                                      00315500
DE30HOLD EQU   X'20'    HOLD                                            00315510
DE30RECF DS    X        DCB RECFM                                       00315520
DE30LREC DS    H        DCB LRECL                                       00315530
DE30BLKS DS    H        DCB BLKSIZE                                     00315540
         DS    XL2      RESERVED                                        00315550
*                       END OF 8 BYTE SECTION                           00315560
DE30CPYS DS    AL1      COPIES                                          00315570
         DS    XL3      RESERVED                                        00315580
DE30FCB  DS    CL4      FCB                                             00315590
DE30DEST DS    CL8      DEST                                            00315600
*              DAPB30 HAS BEEN EXTENDED BY 24 BYTES                     00315610
         SPACE 3                                                        00315620
NL       EQU   X'01'    TAPE LABEL TYPE, NO LABELS                      00315630
SL       EQU   X'02'    STANDARD                                        00315640
NSL      EQU   X'04'    NONSTANDARD                                     00315650
SUL      EQU   X'0A'    STANDARD AND USER                               00315660
BLP      EQU   X'10'    BYPASS LABEL PROCESSING                         00315670
LTM      EQU   X'21'    DOS LEADING TAPE MARK                           00315680
AL       EQU   X'40'    ANSI                                            00315690
AUL      EQU   X'48'    ANSI AND USER                                   00315700
         SPACE 3                                                        00315710
DEN0     EQU   X'03'    200 BPI                                         00315720
DEN1     EQU   X'43'    556 BPI                                         00315730
DEN2     EQU   X'83'    800 BPI                                         00315740
DEN3     EQU   X'C3'    1600 BPI                                        00315750
DEN4     EQU   X'D3'    6250 BPI                                        00315760
         SPACE 3                                                        00315770
T7C      EQU   X'13'    DATA CONVERSION                                 00315780
T7E      EQU   X'23'    EVEN PARITY                                     00315790
T7ET     EQU   X'2B'    EVEN PARITY, BCD-EBCDIC TRANSLATION             00315800
T7T      EQU   X'3B'    BCD-EBCDIC TRANSLATION                          00315810
         SPACE 3                                                        00315820
RECFMU   EQU   X'C0'    UNDEFINED LENGTH                                00315830
RECFMF   EQU   X'80'    FIXED LENGTH                                    00315840
RECFMV   EQU   X'40'    VARIABLE LENGTH                                 00315850
RECFMT   EQU   X'20'    TRKOV                                           00315860
RECFMB   EQU   X'10'    BLOCKED                                         00315870
RECFMS   EQU   X'08'    VARIABLE SPANNED OR FIXED STANDARD              00315880
RECFMA   EQU   X'04'    ASA CC                                          00315890
RECFMM   EQU   X'02'    MACHINE CC                                      00315900
         SPACE 3                                                        00315910
DSORGPO  EQU   X'02'    PARTITIONED                                     00315920
DSORGPOU EQU   X'03'    PARTITIONED UNMOVEABLE                          00315930
DSORGDA  EQU   X'20'    DIRECT ACCESS                                   00315940
DSORGDAU EQU   X'21'    DIRECT ACCESS UNMOVEABLE                        00315950
DSORGPS  EQU   X'40'    SEQUENTIAL                                      00315960
DSORGPSU EQU   X'41'    SEQUENTIAL UNMOVEABLE                           00315970
         SPACE 3                                                        00315980
R0       EQU   0                                                        00315990
R1       EQU   1                                                        00316000
R2       EQU   2                                                        00316010
R3       EQU   3                                                        00316020
R4       EQU   4                                                        00316030
R5       EQU   5                                                        00316040
R6       EQU   6                                                        00316050
R7       EQU   7                                                        00316060
R8       EQU   8                                                        00316070
R9       EQU   9                                                        00316080
R10      EQU   10                                                       00316090
R11      EQU   11                                                       00316100
R12      EQU   12                                                       00316110
R13      EQU   13                                                       00316120
R14      EQU   14                                                       00316130
R15      EQU   15                                                       00316140
         END                                                            00316150
//ASM.SYSTERM DD SYSOUT=*
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR
//LKED.SYSIN DD *
 NAME DDN(R)
/*
//HELP    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP
//SYSIN    DD  *
./ ADD NAME=DDN
)F FUNCTION -                                                           00316350
  THE DDN COMMAND IS LIKE A SHORTHAND 'ALLOC' COMMAND.                  00316360
                                                                        00316370
  IT IS SIMILAR TO THE ALLOC COMMAND EXCEPT THE FILENAME                00316380
  AND DATASET NAME ARE REQUIRED POSITIONAL OPERANDS INSTEAD             00316390
  OF KEYWORD OPERANDS.  FOR EXAMPLE, INSTEAD OF TYPING                  00316400
                                                                        00316370
     ALLOC FI(SYSPROC) DA(CLIST) SHR                                    00316410
                                                                        00316370
  YOU CAN TYPE                                                          00316420
                                                                        00316370
     DDN SYSPROC CLIST                                                  00316430
                                                                        00316440
  IF THE FILENAME IS CURRENTLY ALLOCATED, IT WILL BE FREED AND REUSED.  00316450
                                                                        00316460
  NOT ALL OPERANDS OF ALLOC ARE AVAILABLE IN THE DDN COMMAND.           00316470
  SOME OPERANDS ARE NEW FUNCTIONS NOT AVAILABLE IN THE ALLOC COMMAND.   00316480
)X SYNTAX  -                                                            00316490
         DDN  'FILENAME'  'DSNAME'  SHR/OLD/MOD/NEW  USING('ATTR')      00316500
                                    UNIT('UNITNAME') VOLUME('VOLUME')   00316510
                                    SPACE('PRIME','SEC')  DIR('NUMBER') 00316520
                                    TRACKS/CYLINDERS/BLOCKS('SIZE')     00316530
                                    DATASET  FORM('ID')  FCB('ID') HOLD 00316540
                                    COPIES('NUMBER')  DEST('ID')        00316550
                                    RECFM('X')  LRECL('N') BLKSIZE('N') 00316560
                                    DSORG(PS/PO/DA/PSU/POU/DAU)         00316570
  REQUIRED - 'FILENAME', 'DSNAME'                                       00316580
  DEFAULTS - SHR (UNLESS SPACE, DIR, OR TRACKS/CYL/BLOCKS SPECIFIED,    00316590
                  IN WHICH CASE THE DEFAULT IS NEW)                     00316600
             FOR NEW DATA SETS, IF 'SPACE' IS NOT SPECIFIED AND         00316610
             SPACE TYPE (TRACKS, CYL, BLOCK) IS NOT SPECIFIED, THEN     00316620
             THE SYSTEM DEFAULT SPACE VALUES WILL BE USED.              00316630
             THESE ARE CURRENTLY SPACE(10,50) BLOCKS(1000) RLSE.        00316640
  ALIAS    - NONE                                                       00316650
)O OPERANDS -                                                           00316660
  'FILENAME' - THE FILENAME TO BE ALLOCATED.                            00316670
  'DSNAME'  -  THE DATA SET(S) TO BE ALLOCATED TO THE FILENAME.         00316680
               IF MORE THAN ONE IS SPECIFIED, THEY WILL BE CONCATENATED.00316690
               IF '*' IS SPECIFIED, THE TERMINAL WILL BE ALLOCATED.     00316700
               IF 'SYSOUT' OR 'SYSOUT(X)', A SYSOUT FILE WILL BE        00316710
               ALLOCATED (X IS SYSOUT CLASS, AND CAN BE A THRU Z OR     00316720
               #0 THRU #9 WHICH ARE TREATED AS 0 THRU 9).               00316730
               IF 'DUMMY', A DUMMY FILE WILL BE ALLOCATED.              00316740
               IF '$', A TEMPORARY DATA SET WILL BE ALLOCATED.          00316750
               IF '$.NAME', A TEMPORARY DATA SET WILL BE ALLOCATED,     00316760
               WITH 'NAME' APPENDED TO THE GENERATED DSNAME.            00316770
))OLD       -  THE DATA SET IS TO BE ALLOCATED EXCLUSIVELY              00316780
               TO THIS SESSION.                                         00316790
))MOD       -  THE DATA SET IS TO BE ALLOCATED EXCLUSIVELY              00316800
               TO THIS SESSION, TO APPEND MORE DATA.                    00316810
))NEW       -  THE DATA SET IS TO BE CREATED.                           00316820
))UNIT('UNIT') - THE UNIT NAME OF THE DEVICE ON WHICH THE DATA SET      00316830
               RESIDES, IF NOT CATALOGED.                               00316840
))VOLUME('VOLUME') - THE VOLUME SERIAL ON WHICH THE DATA SET            00316850
               RESIDES, IF NOT CATALOGED.                               00316860
))USING('ATTR-LIST-NAME') - THE NAME OF AN ATTRIBUTE LIST               00316870
               OF DCB PARAMETERS TO BE MERGED WITH THE DATA SET.        00316880
))DATASET   -  THIS KEYWORD IS NEEDED ONLY IF YOU WISH TO               00316890
               ALLOCATE A DATA SET HAVING THE NAME 'SYSOUT'             00316900
               OR 'DUMMY' OR '$' OR '$.NAME'.  THIS KEYWORD             00316910
               FORCES THE COMMAND TO USE THE SECOND OPERAND             00316920
               AS A DSNAME IF IT IS ANY OF THESE NAMES.                 00316930
))SPACE('PRIME','SEC') - THE PRIMARY AND SECONDARY AMOUNTS OF SPACE     00316940
               TO BE ALLOCATED TO A NEW DATA SET ON DISK.               00316950
))TRACKS    -  THE VALUES IN THE 'SPACE' OPERANDS ARE IN TRACKS.        00316960
))CYLINDERS -  THE VALUES IN THE 'SPACE' OPERANDS ARE IN CYLINDERS.     00316970
))BLOCKS('SIZE')  -  THE VALUES IN THE 'SPACE' OPERANDS ARE IN BLOCKS   00316980
               AND THIS IS THE SIZE OF A BLOCK.                         00316990
))DSORG(PS/PO/DA/PSU/POU/DAU) - DCB DSORG.                              00317000
))RECFM('X') - DCB RECORD FORMAT.                                       00317010
))LRECL('N') - DCB LOGICAL RECORD LENGTH.                               00317020
))BLKSIZE('N') - DCB BLOCK SIZE.                                        00317030
))FORM('ID') - SYSOUT FORM NUMBER.                                      00317040
))FCB('ID')  - SYSOUT FORMS CONTROL BUFFER IMAGE ID.                    00317050
))HOLD       - SYSOUT TO BE HELD.                                       00317060
))DEST('ID') - SYSOUT DESTINATION.                                      00317070
))COPIES('NUMBER') - SYSOUT COPIES (1 TO 99).                           00317080
./ ENDUP
/*
