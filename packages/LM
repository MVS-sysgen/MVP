//LM  JOB (TSO),
//             'Install LM',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*-----------------------------------------------------------------*   00020000
//*       INSTALLATION OF THE  'LM'  TSOCP                              00030000
//*-----------------------------------------------------------------*   00040000
//ASM1     EXEC ASMFCL,PARM.ASM='TERM,NOLIST,OBJ'                       00050000
//SYSLIB   DD DSN=SYS1.MACLIB,DISP=SHR                                  00060000
//         DD DSN=SYS1.AMODGEN,DISP=SHR                                 00070000
//SYSPRINT DD SYSOUT=*                                                  00080000
//SYSTERM  DD SYSOUT=*                                                  00090000
//ASM.SYSIN    DD *                                                     00100000
         MACRO                                                          00000010000
         EQUREGS                                                        00000020000
R0       EQU   0                                                        00000030000
R1       EQU   1                                                        00000040000
R2       EQU   2                                                        00000050000
R3       EQU   3                                                        00000060000
R4       EQU   4                                                        00000070000
R5       EQU   5                                                        00000080000
R6       EQU   6                                                        00000090000
R7       EQU   7                                                        00000100000
R8       EQU   8                                                        00000110000
R9       EQU   9                                                        00000120000
R10      EQU   10                                                       00000130000
R11      EQU   11                                                       00000140000
R12      EQU   12                                                       00000150000
R13      EQU   13                                                       00000160000
R14      EQU   14                                                       00000170000
R15      EQU   15                                                       00000180000
         MEND                                                           00000190000
LM       TITLE '      SHOW DIRECTORY (LIST MEMBER NAMES OF A PDS)      X00010000
               MARCH 27, 1978'                                          00020000
*                                                                       00030000
*                                                                       00040000
*                                                                       00050000
*                                                                       00060000
*                                                                       00070000
SHOWDIR  CSECT                                                          00910000
* ******************************************************************    00920000
*   WRITTEN BY DICK SZEIDE (CSC AT OMB) .                               00930000
*   MODIFIED FOR GSFC BY RICK DUVALL (CSC) APRIL 1977.                  00940000
*                                                                   JFS 00950000
*   MOFIFIED BY J. SCHINDLER, CSC, JANUARY 26, 1978 TO:             JFS 00960000
*   1) PUT IN TITLE CARD.                                           JFS 00970000
*   2) CORRECTED PROGRAM SO THAT THE DS (OR DA) DATA SET NAME WOULD JFS 00980000
*      OUTPUT CORRECTLY WHEN THE DATA SET IS NOT SEQUENTIAL.        JFS 00990000
*   3) PUT IN SOME PRINT NOGEN'S (COULDN'T SEE THE FOREST FOR THE   JFS 01000000
*      TREES).                                                      JFS 01010000
*   4) CHANGED DATE ON SAVE MACRO.                                  JFS 01020000
*   5) CHANGED ERROR MESSAGE (OUTMESS).                             JFS 01030000
*   6) PUT IN EJECT BEFORE HELP DATA LOCATED AT THE END OF THIS     JFS 01040000
*      SOURCE.                                                      JFS 01050000
*   MODIFIED BY J. SCHINDLER, CSC, MARCH 27, 1978 TO:       JFS03/27/78 01060000
*   1) CORRECT AN ERROR WHEN THE PRINT DATA EXISTS,         JFS03/27/78 01070000
*      BUT IS NOT CURRENTLY ALLOCATED.                      JFS03/27/78 01080000
*                                                                   JFS 01090000
* *****************************************************************     01100000
         EQUREGS                                                        01110000
         SAVE  (14,12),,SHOWDIR-3/27/78                     JFS03/27/78 01120000
         BALR  R12,0                  BASE ADDR                         01130000
         USING *,R12                                                    01140000
*                                                                       01150000
         LR    R11,R13                SAVE BACKWARD POINTER             01160000
         LR    R10,R1      PRESERVE CPPL POINTER                        01170000
*                                                                       01180000
         LA    R0,WKLEN           LENGTH TO GETMAIN                SFI  01190000
         A     R0,TABLEN          ADD IN CORE FOR TABLE            SFI  01200000
         GETMAIN R,LV=(0)             GET WORK AREA.               SFI  01210000
         LR    R13,R1                      AND MAKIT ADDRESSABLE        01220000
         USING SAVEAREA,R13                                             01230000
         LA    R2,SAVEAREA            ADDR OF WKA IN R1                 01240000
         LA    R3,WKLEN               LENGTH OF WKA                SFI  01250000
         A     R3,TABLEN          ADD IN LENGTH OF TABLE           SFI  01260000
         LA    R4,256          MAX LENGTH OF MOVE ON 360           SFI  01270000
         MVI   0(R2),X'00'      ZERO 1ST BYTE                      SFI  01280000
         S     R3,=F'2'         SUB 1 FOR BYTE ALREADY MOVED AND   SFI  01290000
*                               1 TO GET LENGTH-1                  SFI  01300000
MVCLOOP  CR    R3,R4            IS LENGTH REMAINING < 256          SFI  01310000
         BL    EXMOVE            BR IF YES                         SFI  01320000
         MVC   1(256,R2),0(R2)   PROPAGATE ZEROES                  SFI  01330000
         SR    R3,R4             SUB NUMBER OF BYTES MOVED         SFI  01340000
         AR    R2,R4             INCR POINTER TO SECTION TO CLEAR  SFI  01350000
         B     MVCLOOP           GO TO TOP OF LOOP                 SFI  01360000
MOVEIT   MVC   1(0,R2),0(R2)                                       SFI  01370000
EXMOVE   EX    R3,MOVEIT         CLEAR REMAINING BYTES             SFI  01380000
*                                                                       01390000
*                                                                       01400000
         ST    R11,SAVEAREA+4        BSCK CHAIN                         01410000
         ST    R13,8(R11)          FOREWARD CHAIN                       01420000
*                                                                       01430000
        EJECT                                                           01440000
         USING CPPL,R10               CPPL ADDR WAS SAVED IN R10        01450000
         MVC   IOPLUPT(4),CPPLUPT      PREPARE IOPL                     01460000
         MVC   IOPLECT(4),CPPLECT                                       01470000
         LA    R2,ECB                                                   01480000
         ST    R2,IOPLECB                                               01490000
         DROP  R10                    WE NO LONGER NEED THE CPPL        01500000
*                                                                       01510000
*                                                                       01520000
         BAL   R3,PARSINPT                 PARSE INPUT BUFFER           01530000
         BAL   R3,DSNFMT                   FORMAT DSNAME FOR DAIR       01540000
         BAL   R3,PREALLOC                 IS DSNAME ALLOCATED?         01550000
         BAL   R3,ALLOC                    ALLOCATE INPUT               01560000
         BAL   R3,OPENER               OPEN DCB FOR INPUT               01570000
*                                                                       01580000
*                                                                       01590000
         BAL   R3,ALIAS                SPECIAL FORMAT FOR ALIASES?      01600000
         BAL   R3,PSTRING              SET UP MASKS FOR COMPARE         01610000
         BAL   R3,LINESIZE             LINE SIZE SPECIFIED?             01620000
         BAL   R3,PRINT                DO WE WANT DISK OP?              01630000
         BAL   R3,PRNTALOC             ALLOCATE IF YES.                 01640000
         BAL   R3,PUTBL                 BLANK OUTPUT AREA               01650000
*                                                                       01660000
         LA    R5,CHANTOP                                               01670000
         LR    R7,R5                                               SFI  01680000
         A     R7,TABLEN          GET END OF TABLE ADDR            SFI  01690000
         LA    R6,16                    LENGTH OF AN ENTRY              01700000
         SR    R7,R6         SUB TO MAKE R7 POINT TO LAST ITEM     SFI  01710000
*                                                                       01720000
         LA    R10,DIRDATA+256          FORCE GETTER TO GET A DIRECTORY 01730000
         LR    R9,R10                       BLOCK ON FIRST ACCESS       01740000
*                                                                       01750000
         OI    LINECNT+(L'LINECNT-1),X'0C'          FOR MSG & SEQ       01760000
*                                                                       01770000
MAINLOOP BAL   R3,GETTER                GET A MEMBER ENTRY              01780000
         BAL   R3,CHAIN                 CHECK FOR ALIAS                 01790000
         B     MAINLOOP                 THE EXITS FROM THIS LOOP ARE    01800000
*                                           END-OF-FILE (LOGICAL OR     01810000
*                                            PHYSICAL) ON INPUT.        01820000
EODAD    LR    R7,R5              SET UP THE HIGH-WATER MARK AS         01830000
         LA    R5,CHANTOP             LIMIT FOR THE RUN-OUT             01840000
         CR    R5,R7              CHECK FOR NO OUTPUT                   01850000
         BE    EODEDIT                                                  01860000
         LA    R6,16              SET INCREMENT                         01870000
         SR    R7,R6              BACK OFF FOR FLYING START             01880000
*                                                                       01890000
         LA    R9,PUTLINE-10      OUTPUTLINE LIMIT                      01900000
         AH    R9,LINELENG                                              01910000
         LA    R8,10              OUTLINE INCREMENT                     01920000
         BAL   R3,PUTBL                                                 01930000
*                                                                       01940000
PRNTLOOP BAL   R3,FORMAT          FORMAT OUTPUT FOR EACH MEMBER,        01950000
         BXLE  R5,R6,PRNTLOOP        AND PRINT ON A FULL LINE.          01960000
         BAL   R3,FLUSHER         PRINT LAST LINE                       01970000
*                                                                       01980000
EODEDIT  L        R3,HITCOUNT        MAKE HIT COUNT VISIBLE        SFI  01990000
         LTR   R3,R3              WERE ANY MEMBERS FOUND?          SFI  02000000
         BNZ   EODPUT                RC = 4 IF HITCOUNT=0               02010000
         OI    RCDE+3,X'04'                                             02020000
         B     EODPUT2            BYPASS CHECK FOR < 10 FOUND      SFI  02030000
EODPUT   C     R3,=F'10'            WERE 10 MEMBERS LISTED?        SFI  02040000
         NOP   QUIT                 IF NOT, DON'T PRINT COUNT      SFI  02050000
**** ZAP ABOVE TO A BL   QUIT TO ACTIVATE (X'4700' TO X'4740')          02060000
EODPUT2  CVD   R3,HITCOUNT          CONVERT TO DECIMAL             SFI  02070000
         ED    FINAL,HITCOUNT+5     EDIT FOR OUTPUT                SFI  02080000
*                                                                  SFI  02090000
         MVC   PUTLINE(LFINAL),FINAL    MOVE TO OUT AREA           SFI  02100000
         LA    R0,LFINAL            LENGTH FOR TPUT                     02110000
         L     R10,SAVPARMD       LOAD POINTER TO PARAM LIST       SFI  02120000
         USING IKJPARMD,R10       ADDRESSABILITY FOR PARMS         SFI  02130000
         TM    PARPFXD+6,X'80'    WAS MASK PARM SPECIFIED          SFI  02140000
         BNO   NOMASK             BR IF NOT                        SFI  02150000
*    IF IT WAS, WE WILL DISPLAY IT FOR THE USER                    SFI  02160000
         MVC   PUTLINE+22(L'OUTMASK),OUTMASK  MOVE IN 'USING MASK' SFI  02170000
         L     R6,PARPFXD         GET POINTER TO MASK              SFI  02180000
         LH    R5,PARPFXD+4       GET LENGTH OF MASK               SFI  02190000
         BCTR  R5,0               DECR TO GET LENGTH-1             SFI  02200000
         EX    R5,MOMOVE          MOVE MASK TO OUTPUT LINE         SFI  02210000
         LA    R6,PUTLINE+22+L'OUTMASK-12+1(R5)                    SFI  02220000
*      NOW R6 POINTS TO END OF MASK IN OUTPUT LINE +1              SFI  02230000
         MVI   0(R6),C''''        PUT ENDING QUOTE AFTER MASK      SFI  02240000
         MVC   3(LFINAL-22,R6),FINAL+22   RESTORE REST OF LINE     SFI  02250000
         LA    R0,LFINAL(R5)      SET UP LENGTH TO TPUT            SFI  02260000
NOMASK   EQU   *                                                   SFI  02270000
         BAL   R3,FORTPUTT              AND WRITE FINAL LINE            02280000
*                                                                       02290000
*                                                                       02300000
QUIT     CLOSE ,MF=(E,CLOSEL)    CLOSE DCB OR DCBS                      02310000
*                                                                       02320000
FREE     L     R15,DONTFREE              RETRIEVE SWITCH                02330000
         B     FREETAB(R15)                                             02340000
MOMOVE   MVC   PUTLINE+22+L'OUTMASK-12(0),0(R6)                    SFI  02350000
FREETAB  B     FREEMAIN                  FREE IT                        02360000
*                                                                       02370000
         LA    R1,XDAPL                ADDR OF 'FREE' PARAMETER BLOCK   02380000
         USING DAPL,R1                                                  02390000
         LA    R9,DA18AREA             ADDR OF DAIR WORKAREA            02400000
         ST    R9,DAPLDAPB             ADDRESSABLE FOR "FREE"           02410000
         DROP  R1                                                       02420000
         LINK  EP=IKJEFD00             FREE DATA SET                    02430000
*                                                                       02440000
         LTR   R15,R15                 WHAT DOES DAIR SAY?              02450000
         BZ    FREEMAIN                    IF NOTHING, WE PASS          02460000
         BAL   R3,DAIRERR                  OTHERWISE, WE TALK IT OVER   02470000
*                                                                       02480000
FREEMAIN EQU   *                                                   SFI  02490000
*REEMAIN L     R15,RCDE             MAKE RC VISIBLE                     02500000
*        BAL   R3,RESTOHEX                                              02510000
*        STH   R5,STCMFULL                                         SFI  02520000
*        CLC   RCZIP(2),STCMFULL                                   SFI  02530000
*        BE    RCZERO              BR IF IT IS. ( NO RC MESSAGE)   SFI  02540000
*        MVC   PUTLINE+3(2),STCMFULL                               SFI  02550000
*        MVC   PUTLINE(3),=C'RC='                                       02560000
*        LA    R5,5            LGTH FOR TPUT                            02570000
*        BAL   R3,FORTPUT                                               02580000
*                                                                       02590000
RCZERO   L     R2,SAVEAREA+4       RETRIVE BACKWARD CHAIN               02600000
         L     R3,RCDE             RETRIVE MAX RC                       02610000
         LA    R0,WKLEN           LENGTH TO FREE                   SFI  02620000
         A     R0,TABLEN          ADD IN CORE FOR TABLE            SFI  02630000
         LR    R1,R13             AREA TO FREE                     SFI  02640000
         FREEMAIN R,LV=(0),A=(1)   RETURN WHAT WE GOT              SFI  02650000
*                                                                       02660000
         LR    R13,R2                   OLD SAVEAREA ADDRESSABLE        02670000
         LR    R15,R3                   PRESERVE RC                     02680000
         RETURN (14,12),,RC=(15)                                        02690000
         DC    0H'0'                                                    02700000
*RCZIP   DC    X'F0F0'                                             SFI  02710000
         EJECT                                                          02720000
*                                                                       02730000
*            * ERROR ROUTINES *                                         02740000
*                                                                       02750000
PARSERR  TPUT  PARSMSG,L'PARSMSG                                        02760000
         OI    RCDE+3,X'10'                 NO OUTPUT                   02770000
         B     FREEMAIN                                                 02780000
*                                                                       02790000
*                                                                       02800000
NOTPART  LH    R6,DSNBUF          GET LENGTH OF DSN                SFI  02810000
         BCTR  R6,0               DECREMENT TO GET LENGTH-1        SFI  02820000
         EX    R6,DSNMVIN         MOVE DSN TO MESSAGE              SFI  02830000
         LA    R6,INMESS+19(R6)   POINT PAST DSN                   SFI  02840000
         MVC   0(L'PARTMSG,R6),PARTMSG     MOVE IN MESSAGE         SFI  02850000
BADIN    TPUT  INMESS,L'INMESS    PUT OUT BAD INPUT MESSAGE        SFI  02860000
         OI    RCDE+3,X'0C'                 INVALID INPUT          SFI  02870000
         B     FREE                                                     02880000
*                                                                       02890000
*                                                                       02900000
DSNMVIN  MVC   INMESS+16(0),DSNBUF+2       VARIABLE LENGTH MOVE    SFI  02910000
OPENERR  LH    R6,DSNBUF          GET LENGTH OF DSN                SFI  02920000
         BCTR  R6,0               DECREMENT TO GET LENGTH-1        SFI  02930000
         EX    R6,DSNMVIN         MOVE DSN TO MESSAGE              SFI  02940000
         LA    R6,INMESS+19(R6)   POINT PAST DSN                   SFI  02950000
         MVC   0(L'OPENMSG,R6),OPENMSG     MOVE IN MESSAGE         SFI  02960000
         B     BADIN                                               SFI  02970000
*                                                                       02980000
*                                                                       02990000
PRDAIRER ST    R3,PRSTR3                                                03000000
         BAL   R3,DAIRERR                                               03010000
         L     R3,PRSTR3                                                03020000
         NI    PRINTSW,X'00'            IN THIS CASE, TURN OFF PRINT,   03030000
         OI    RCDE+3,X'08'             NOT QUITE RIGHT OUTPUT     SFI  03040000
         BR    R3                            BUT GIVE WHAT WE CAN.      03050000
*                                                                       03060000
PROPNERR LH    R6,DSNBUF          GET LENGTH OF DSN                SFI  03070000
         BCTR  R6,0               DECREMENT TO GET LENGTH-1        SFI  03080000
         EX    R6,DSNMVOUT        MOVE DSN TO MESSAGE              SFI  03090000
         LA    R6,OUTMESS+20(R6)   POINT PAST DSN                  SFI  03100000
         MVC   0(L'OPENMSG,R6),OPENMSG     MOVE IN MESSAGE         SFI  03110000
BADOUT   TPUT  OUTMESS,L'OUTMESS    PUT OUT BAD INPUT MESSAGE      SFI  03120000
         NI    PRINTSW,X'00'                                            03130000
         OI    RCDE+3,X'08'             NOT QUITE RIGHT OUTPUT     SFI  03140000
         BR    R3                                                       03150000
*                                                                       03160000
DSNMVOUT MVC   OUTMESS+17(0),DSNBUF+2      VARIABLE LENGTH MOVE    SFI  03170000
         EJECT                                                          03180000
*                                                                       03190000
*              * SET UP MASKS FOR MEMBER NAME COMPARE  *                03200000
*                                                                       03210000
*                                                                       03220000
         USING IKJPARMD,R10                                             03230000
PSTRING  TM    PARPFXD+6,B'10000000'    IS THE STRING PRESENT?          03240000
         BCR   14,R3                       NO - TAKE DEFAULT            03250000
         MVC   PSAND,=X'FFFFFFFFFFFFFFFF'                               03260000
*                                                                       03270000
*                                                                       03280000
         L     R6,PARPFXD              POINTER TO STRING                03290000
         LH    R5,PARPFXD+4            LENGTH OF STRING                 03300000
         CH    R5,=H'8'                VALIDATE STRING LENGTH, AND      03310000
         BL    PSMASK                      TRUNCATE TO 8.               03320000
         LA    R5,8                                                     03330000
*                                                                       03340000
PSMASK   MVC   PSXC(8),=C'********'    DEFAULT TO ALL MASK CHARS        03350000
         BCTR  R5,0                    DEC REGISTER FOR EXECUTE         03360000
         EX    R5,PSORIN               MOVE INTO MASK, AND SHIFT TO     03370000
*                                          UPPER CASE.                  03380000
*                                                                       03390000
         SR    R4,R4                   CONSTANT ZERO                    03400000
         LA    R7,8                    LIMIT LOOP                       03410000
         LA    R8,C'*'                 COMPARANDS FOR MASK CHARS        03420000
         SR    R15,R15                 IC REGISTER                      03430000
*                                                                       03440000
         CLC   PSXC(3),PSALL0        SPECIAL CASE FOR COMPATIBILITY     03450000
         BNE   PSLOOP                      WITH OPSDIR                  03460000
         XC    PSXC(8),PSXC                                             03470000
         XC    PSAND,PSAND                                              03480000
         BR    R3                                                       03490000
*                                                                       03500000
PSLOOP   IC    R15,PSXC-1(R7)          GET CHARACTER                    03510000
         CR    R15,R8                  IS IT A MASK CHARACTER?          03520000
         BNE   PSINCR                      IF SO, ZERO OUT              03530000
*                                          THE CORRESPONDING POSITION   03540000
         STC   R4,PSAND-1(R7)              IN THE AND & OR MASKS        03550000
         STC   R4,PSXC-1(R7)                                            03560000
         B     PSINCR                                                   03570000
*                                                                       03580000
PSINCR   BCT   R7,PSLOOP               8 REPITITIONS                    03590000
         BR    R3                      AND GET OUT                      03600000
*                                                                       03610000
*                                                                       03620000
PSORIN   MVC   PSXC(0),0(R6)           MOVE INTO TEST POSITION          03630000
         EJECT                                                          03640000
*                                                                       03650000
*              *  DETERMINE OUTPUT LINE SIZE  *                         03660000
*                                                                       03670000
LINESIZE LA    R15,70                  DEFAULT IS 7 ENTRIES/LINE        03680000
         TM    PARLND+6,B'10000000'    IS LINE LENGTH SPECIFIED?        03690000
         BNO   LINEQUIT                    NO - TAKE DEFAULT            03700000
*                                                                       03710000
         L     R4,PARLND               ADDRESS OF STRING                03720000
         IC    R5,0(R4)               TAKE FIRST DIGIT OF STRING        03730000
         LA    R4,15                   SET UP AN AND MASK               03740000
         NR    R5,R4                   R5 SHOULD HAVE FOUR BITS LEFT    03750000
         IC    R15,LINETAB(R5)         GET ACTUAL LINE LENGTH           03760000
*                                                                       03770000
LINEQUIT STH   R15,LINELENG                                             03780000
         BR    R3                                                       03790000
         DROP  R10                                                      03800000
*                                                                       03810000
*                                                                       03820000
*               DECIDE UPON SPECIAL FORMAT FOR ALIASES                  03830000
*                                                                       03840000
*                                                                       03850000
         USING IKJPARMD,R10                                             03860000
ALIAS    ICM   R15,B'0011',PARALIAS   IF THE KEYWORD NUMBER IS ODD WE   03870000
         BCR   8,R3                    WILL  SUPPRESS                   03880000
         SLL   R15,7                  THE SEARCH FOR EQUAL TTR          03890000
         STC   R15,CHAND+1               AND THE PUTTING OF ALIAS       03900000
         BR    R3                     NAMES IN PARENTHESIS              03910000
         DROP  R10                                                      03920000
*                                                                       03930000
         EJECT                                                          03940000
         USING CPPL,R10                    AND MAKE IT ADDRESSABLE      03950000
         USING PPL,R8                                                   03960000
PARSINPT LA    R8,PPLAREA                MAKE PPL ADDRESSABLE           03970000
         MVC   PPLUPT,CPPLUPT          SET UP IKJPARSE PARMS            03980000
         MVC   PPLECT,CPPLECT                                           03990000
         MVC   PPLCBUF,CPPLCBUF                                         04000000
         LA    R9,ECB                                                   04010000
         ST    R9,PPLECB                                                04020000
         MVC   PPLPCL(4),=A(PARMDSN)                                    04030000
         LA    R9,ANS                                                   04040000
         ST    R9,PPLANS                                                04050000
*                                                                       04060000
*                                                                       04070000
*                                                                       04080000
         LA    R1,PPL                  MAKE PPL ADDRESSABLR             04090000
         LINK  EP=IKJPARS              PARSE  THE INPUT BUFFER          04100000
*                                                                       04110000
         LTR   R15,R15                 CHECK FOR RETURN OTHER THAN ZERO 04120000
         BNZ   PARSERR                                                  04130000
*                                                                       04140000
         DROP  R10                                                      04150000
         ICM   R10,B'1111',PPLANS      TOP OF PDL CHAIN                 04160000
         BNP   PARSERR                 PARSE OF BUFFER UNSUCESSFUL      04170000
         L     R10,0(R10)              PDL HEADER                       04180000
         ST    R10,SAVPARMD       SAVE POINTER TO PARAM LIST       SFI  04190000
         BR    R3                                                       04200000
         DROP  R8                                                       04210000
         EJECT                                                          04220000
         USING IKJPARMD,R10                                             04230000
*                                                                       04240000
PREALLOC LA    R9,DA00AREA             MAKE DAIR WORK AREAS             04250000
         USING DAPB00,R9                                                04260000
*                                                                       04270000
         LA    R1,XDAPL                                                 04280000
         USING DAPL,R1                                                  04290000
         ST    R9,DAPLDAPB                  POINT DAPL TO SRCH          04300000
         DROP  R1                                                       04310000
         LINK  EP=IKJEFD00                                              04320000
*                                                                       04330000
         B     PRECONT(R15)                 CHECK FOR ERROR             04340000
PRECONT  B     PRETM                                                    04350000
*                                                                       04360000
         BAL   R3,DAIRERR                   ERROR DETECTED              04370000
         B     FREEMAIN                     NO OUTPUT                   04380000
*                                                                       04390000
PRETM    LA    R15,4                         4 MEANS FREE IT            04400000
         TM    DA00FLG,B'00001010'          WAS IT ALLOCATED            04410000
         BZ    PRETEND                         YES - DON'T FREE IT      04420000
*                                                                       04430000
         SR    R15,R15                       0 MEANS DON'T FREE         04440000
         TM    DA00DSO,B'10'                 IS IT PARTITIONED?         04450000
         BNO   NOTPART                            ERROR MSG IF NOT.     04460000
*                                                                       04470000
PRETEND  ST    R15,DONTFREE                  SAVE DON'T FREE SWITCH     04480000
         BR    R3                                                       04490000
*                                                                       04500000
         DROP  R9                                                       04510000
         DROP  R10                                                      04520000
         EJECT                                                          04530000
         USING IKJPARMD,R10                                             04540000
*                                                                       04550000
ALLOC    LA    R8,DA08AREA             MAKE DAIR WORK AREAS             04560000
         LA    R9,DA18AREA                 ADDRESSABLE                  04570000
         USING DAPB08,R8                                                04580000
         USING DAPB18,R9                                                04590000
*                                                                       04600000
DAUNIT   MVI   DA08DDN,C' '                                             04610000
         MVC   DA08DDN+1(23),DA08DDN BLANK UNIT AND SERIAL NUMBER       04620000
         MVC   DA08MNM(16),DA08UNIT    BLANK MEMBER & PASSWORD          04630000
*                                                                       04640000
         OI    DA08DSP1,X'08'          DISP=(SHR,KEEP,KEEP)             04650000
         OI    DA08DPS2,X'08'                                           04660000
         OI    DA08DPS3,X'08'                                           04670000
         OI    DA18DPS2,X'08'                                           04680000
*                                                                       04690000
         MVC   DA18MNM(10),DA08UNIT    BLANK MEMBER & SYSOUT CLASS      04700000
*                                                                       04710000
         LA    R1,XDAPL                POINTER FOR LINK                 04720000
         USING DAPL,R1                                                  04730000
         ST    R8,DAPLDAPB             ADDRESS OF DAIR WORK AREA IN     04740000
*                                          DAIR PARAMETER BLOCK         04750000
         DROP  R1                                                       04760000
         LINK  EP=IKJEFD00             CALL ALLOCATE                    04770000
*                                                                       04780000
*                                                                       04790000
         CH    R15,=H'8'               WE HAVE A SPECIAL MESSAGE        04800000
         BNE   ALLOCONT                    FOR A DATA SET NOT IN THE    04810000
         CH    R15,DA08CTRC                CATALOG.                     04820000
         BNE   ALLOCONT                                                 04830000
*                                                                       04840000
         LH    R6,DSNBUF            GET LENGTH OF DSN                   04850000
         BCTR  R6,0                 AND DECREMENT FOR EXECUTE           04860000
         EX    R6,DSNMOVE3          PUT NAME IN MESSAGE                 04870000
         LA    R6,MSGNOTCT+13(R6)    POINT PAST DSNAME                  04880000
         MVC   0(17,R6),=C'IS NOT CATALOGED.'                           04890000
*                                                                       04900000
         TPUT  MSGNOTCT,L'MSGNOTCT                                      04910000
         OI    RCDE+3,X'0C'           NO OUTPUT RC                 SFI  04920000
         B     FREEMAIN                                                 04930000
*                                                                       04940000
ALLOCONT LTR   R15,R15              ARE WE ALLOCATED                    04950000
         BZ    ALLOCKPO                  YES - CHECK FOR PARTITIONED    04960000
         BAL   R3,DAIRERR                                               04970000
         B     FREEMAIN                                                 04980000
*                                                                       04990000
*                                                                       05000000
ALLOCKPO LA    R7,DCB                  MAKE INPUT DCB ADDRESSABLE       05010000
         USING IHADCB,R7                                                05020000
         MVC   DCBDDNAM(8),DA08DDN     PUT DDNAME INTO DCB              05030000
         MVC   DA18DDN(8),DA08DDN         AND FREE BLOCK                05040000
*                                                                       05050000
*            * CHECK FOR DSORG=PO, AND SET UP FOR OPEN                  05060000
         TM    DA08DSO,B'10'           IS IT PARTITIONED ?              05070000
         BNO   NOTPART                     NO - DON'T REPORTIT          05080000
*                                                                       05090000
         BR    R3                                                       05100000
*                                                                       05110000
DSNMOVE3 MVC   MSGNOTCT+10(0),DSNBUF+2    VARIABLE LENGTH MOVE          05120000
*                                                                       05130000
         DROP  R7                                                       05140000
         DROP  R8                                                       05150000
         DROP  R9                                                       05160000
         DROP  R10                                                      05170000
         EJECT                                                          05180000
OPENER   LA    R9,DCB                  MAKE INPUT DCB ADDRESSABLE       05190000
         USING IHADCB,R9                                                05200000
*                                                                       05210000
         OPEN  (DCB,(INPUT))                                            05220000
         TM    DCBOFLGS,B'10000'       ARE WE OPEN ?                    05230000
         BNO   OPENERR                     NOPE - TELL 'EM              05240000
         DROP  R9                                                       05250000
         BR    R3                                                       05260000
*                                                                       05270000
         EJECT                                                          05280000
GETTER   CR    R9,R10              ARE WE PAST LAST MEMBER ENTRY?       05290000
         BL    GETINCR                 IF SO, GET A NEW DIRECTORY BLOCK 05300000
*                                                                       05310000
GETREAD  LA    R4,DIRDATA             GET THE OUTAREA IN A REG          05320000
         READ  DECB1,SF,DCB,(R4),'S'  GET THE BLOCK                     05330000
         CHECK DECB1                                                    05340000
*                                                                       05350000
         LA    R9,DIRDATA+2        POINTER TO CURRENT MEMBER            05360000
         LA    R10,DIRDATA         SET UP LIMIT TO SEARCH FOR           05370000
         AH    R10,DIRDATA              MEMBER NAMES                    05380000
         B     GETCOMPR                                                 05390000
*                                                                       05400000
GETINCR  SR    R4,R4                GET NUMBER OF HALFWORDS OF          05410000
         IC    R4,11(R9)               USER DATA,                       05420000
         LA    R8,31                   BUT LIMITED TO 5 BITS            05430000
         NR    R4,R8                                                    05440000
         SLA   R4,1             CONVERT TO BYTES                        05450000
         LA    R9,12(R4,R9)       POINT TO NEXT MEMBER ENTRY            05460000
*                                                                       05470000
         CR    R9,R10             HAVE WE PASSED LAST MEMBER?           05480000
         BNL   GETREAD                 YES - GET A NEW DIRECTORY BLOCK  05490000
*                                                                       05500000
GETCOMPR CLC   0(8,R9),=X'FFFFFFFFFFFFFFFF'  LAST ENTRY IN DIRECTORY?   05510000
         BE    EODAD                   YES - QUIT                       05520000
*                                                                       05530000
         MVC   PSARGU(8),0(R9)    MOVE MEMBER NAME TO WORK AREA         05540000
         NC    PSARGU,PSAND            MASK OFF NON-COMPARE BYTES       05550000
         XC    PSARGU,PSXC             DOES IT MATCH THE MASK?          05560000
         BNZ   GETINCR                     NO - PASS IT BY              05570000
*                                                                       05580000
         L     R8,HITCOUNT                                         SFI  05590000
         LA    R8,1(R8)                     COUNT THE MEMBER       SFI  05600000
         ST    R8,HITCOUNT                                         SFI  05610000
         BR    R3                                                       05620000
*                                                                       05630000
         EJECT                                                          05640000
CHAIN    MVC   0(12,R5),0(R9)        PUT MEMBER NAME IN TABLE           05650000
         ST    R3,CHAINR3            SAVE FOR SUBROUTINE CALLS          05660000
         ST    R5,CHAINR5            SAVE FOR ALIAS CHASING             05670000
CHAND    NI    11(R5),X'80'          TURN OFF ALL INDICATORS BUT ALIAS  05680000
         TM    CHAND+1,X'80'          IF NOALIAS WAS SPECIFIED, WE      05690000
         BZ    CHANOGOT                SKIP THE SEARCH FOR EQUAL TTR    05700000
*                                                                       05710000
*                                                                       05720000
         LA    R2,CHANTOP            START SEARCH FOR EQUAL TTR         05730000
         MVC   STCMFULL(4),8(R9)     MOVE TO FULLWORD ALIGN       SFI   05740000
         L     R15,STCMFULL                                       SFI   05750000
         SRL   R15,8                 SHIFT THE N OFFA TTRN              05760000
         BAL   R3,CHANSEEK                                              05770000
         LTR   R2,R2                 WAS AN EQUAL TTR FOUND?            05780000
         BZ    CHANOGOT                  NO - WHO CARES ?               05790000
*                                                                       05800000
         TM    11(R9),X'80'          IS THE CURRENT GUY AN ALIAS ?      05810000
         BNO   CHASTKIT                  IF SO, WE'LL RUN THE CHAIN OF  05820000
*                                        ALIASES, AND PUT THE NEW KID   05830000
         BAL   R3,CHALIAS                AT THE TAIL.                   05840000
         OI    11(R5),X'01'              IF NOT, WE'LL PUT THE NEW ONE  05850000
         ST    R5,12(R2)                 AT THE HEAD OF THE CHAIN.      05860000
         B     CHANOGOT                                                 05870000
*                                                                       05880000
CHASTKIT ST    R2,12(R5)             POINT TO FORMER CHAIN HEAD         05890000
         OI    11(R2),X'01'          INDICATE THAT FORMER HEAD NOW      05900000
*                                    HAS A PREDECESSOR                  05910000
*                                                                       05920000
CHANOGOT BXH   R5,R6,CHNOCORE       INCREMENT & CHECK FOR OUT OF        05930000
         L     R3,CHAINR3               STORAGE<H                       05940000
         BR    R3                   RETURN TO CALLER                    05950000
*                                                                       05960000
*                                                                       05970000
CHANSEEK CR    R2,R5                R2=0 MEANS THIS IS A UNIQUE TTR     05980000
         BNL   CHANR0                                                   05990000
*                                                                       06000000
CHANSEER LR    R4,R6                PUT INCREMENT IN R4                 06010000
         L     R14,8(R2)            TTRN FROM TABLE                     06020000
         SRL   R14,8                DROP N                              06030000
         CR    R14,R15                                                  06040000
         BE    CHANEQ               FOUND EQUAL TTR                     06050000
*                                                                       06060000
         AR    R2,R4                R2=0 MEANS THIS IS A UNIQUE TTR     06070000
         B     CHANSEEK                                                 06080000
*                                                                       06090000
*                                                                       06100000
CHANR0   SR    R2,R2                                                    06110000
CHANEQ   BR    R3                                                       06120000
*                                                                       06130000
CHALIAS  ICM   R4,B'1111',12(R2)  LOAD AND TEST POINTER TO NEXT ALIAS   06140000
         BZ    CHGOTIT                                                  06150000
         LR    R2,R4              FOLLOW THE ALIAS CHAIN 'TILL          06160000
         B     CHALIAS                IT ENDS                           06170000
*                                                                       06180000
CHGOTIT  BR    R3                                                       06190000
*                                                                       06200000
*                                                                       06210000
CHNOCORE TPUT  MSGNOCOR,L'MSGNOCOR                                      06220000
         OI    RCDE+3,X'08'       BAD OUTPUT RC                         06230000
         B     EODAD                                                    06240000
         EJECT                                                          06250000
FORMAT   TM    11(R5),X'01'         DOES THIS ENTRY HAVE A              06260000
         BO    FORMRET                   PREDECESSOR?                   06270000
*                                        YES - SKIP IT                  06280000
         ST    R3,FORMR3            SAVE OUR RETURN ADDR                06290000
         ST    R5,FORMR5            SAVE OUR INPUT PTR                  06300000
*                                                                       06310000
         TM    11(R5),X'80'         ARE WE AN ALIAS, HMMMM?             06320000
         BNO   FORMCHAI                   IF NOT WE'RE AN ORPHAN        06330000
*                                                                       06340000
         MVC   1(8,R10),=C'*ORPHAN*'      SO MARK IT AS SUCH            06350000
         BAL   R3,FORMINCR                                              06360000
*                                                                       06370000
FORMCHAI BAL   R3,FORMSTIK             PUT THIS GUY IN OUTPUT LINE      06380000
         ICM   R5,B'1111',12(R5)      GET NEXT ALIAS                    06390000
         BNZ   FORMCHAI              AND CHAIN TO THE END               06400000
*                                                                       06410000
*                                                                       06420000
FORMFLUS L     R3,FORMR3              RETRIEVE RETURN ADDR              06430000
         L     R5,FORMR5              AND ORIGIONAL MEMBER              06440000
         B     FORMRET                                                  06450000
*                                                                       06460000
*                                                                       06470000
FORMSTIK MVC   1(8,R10),0(R5)         PUT MEMBER NAME IN OUTPUT LINE    06480000
         TM    11(R5),X'80'        IF WE ARE AN ALIAS, WE WILL          06490000
         BNO   FORMINCR             PRINT IN PARENTHISIS                06500000
         MVI   0(R10),C'('                                              06510000
         LA    R15,8(R10)            WE'LL SCAN RIGHT TO LEFT TO PLACE  06520000
FORMSCAN CLI   0(R15),C' '                  A RIGHT PARENTHESIS AFTER   06530000
         BNE   FORMPARN                     THE LAST CHAR               06540000
         BCT   R15,FORMSCAN                                             06550000
FORMPARN MVI   1(R15),C')'                                              06560000
FORMINCR BXLE  R10,R8,FORMSTR         INCREMENT OUTPUT POINTER AND      06570000
*                                         CHECK LINE LENGTH             06580000
FORMWRR  IC    R15,PRINTSW            SNAG CONTROL FOR DISK O/P         06590000
         EX    R15,PUTBR                                                06600000
FORTPUT  LH    R0,LINELENG            LENGTH FOR TPUT                   06610000
FORTPUTT TPUT  PUTLINE,(R0)                                             06620000
*                                                                       06630000
PUTBL    MVI   PUTLINE,C' '            BLANK OUT THE                    06640000
         MVC   PUTLINE+1(99),PUTLINE   OUTPUT LINE                      06650000
         LA    R10,PUTLINE                  AND RESET POINTER           06660000
         LA    R15,4                   R15=4 MEANS WE ARE WORKING       06670000
         BR    R3                         ON A NEW LINE.                06680000
*                                                                       06690000
FORMSTR  SR    R15,R15                 R15=0 MEANS CONTINUE PRESENT     06700000
         BR    R3                          LINE                         06710000
*                                                                       06720000
FORMRET   BR    R3                                                      06730000
*                                                                       06740000
FLUSHER  LA    R6,PUTLINE              WRITE THE LAST BUFFER,           06750000
         SR    R10,R6                       IF NECESSARY                06760000
         BCR   13,R3                                                    06770000
         B     FORMWRR                                                  06780000
*                                                                       06790000
*                                                                       06800000
*                                                                       06810000
*                                                                       06820000
PUTBR    BC    0,*+4                SNAG THE TPUT, AND MAKE IT A        06830000
         AP    LINECNT,=P'10'           PUT. THEN INCREMENT EDIT LINE   06840000
         UNPK  PUTLINE+73(8),LINECNT    COUNT AND PUT IN OUTPUT LINE.   06850000
         OI    PUTLINE+80,X'F0'     MAKE POSITIVE                       06860000
         PUT   PRDCB,PUTLINE+1                                          06870000
         B     PUTBL                                                    06880000
        EJECT                                                           06890000
*      N.B. - ALL REFERENCES IN THIS SECTION TO THE 'PRINT'        SFI  06900000
*             PARAMETER REALLY REFER TO THE 'DA' PARAMETER.        SFI  06910000
*             ALSO, ALL REFERENCES TO 'PRINTING' REALLY            SFI  06920000
*             MEAN 'CREATING A DISK DATA SET.                      SFI  06930000
PRINT    LA    R8,DA04AREA              MAKE DAIR WKA ADDRESSABLE       06940000
         USING DAPB04,R8                                                06950000
         USING IKJPARMD,R10                                             06960000
*                                                                       06970000
         TM    PARPND+6,X'80'           IS PRINT PARAMETER PRESENT?     06980000
         BO    PRSWITST                     YES -SET SWITCH             06990000
NOPRINT  OI    CLOSEL,X'80'                  NO  - TRUNCATE CLOSE LSFI  07000000
         BR    R3                            AND RETURN                 07010000
*                                                                       07020000
PRSWITST L     R6,PARPND                POINTER TO DSNAME          SFI  07030000
         CLI   0(R6),C'*'         IF DA PARAM IS '*' THEN          SFI  07040000
         BE    NOPRINT            DON'T CREATE DISK OUTPUT         SFI  07050000
         OI    PRINTSW,X'F0'            SET SWITCH FOR DISK OUTPUT SFI  07060000
         MVI   DA04CD+1,X'04'           SET JUST LOOKIN' CODE           07070000
         LH    R5,PARPND+4              LENGTH OF DSNAME                07080000
         STH   R5,PRPNDSN              LENGTH IF QUOTED                 07090000
         BCTR  R5,0                     DECREMENT FOR INCREMENT         07100000
         EX    R5,PRDSNMV               MOVE TO DAIR AREA               07110000
*                                                                       07120000
         TM    PARPND+6,B'01000000'     IS DSN IN QUOTES?               07130000
         BO    PRUNIT                       YES - SKIP REST.            07140000
*                                                                       07150000
         OI    DA04CTL,B'100000'         USER ID NEEDED                 07160000
         SR    R14,R14         SCAN TABLE OF VALID QUALIFIERS           07170000
         LA    R15,PRQALND       FOR DISK DSNAME                        07180000
         LA    R7,PRQALTB            TOP OF TABLE                       07190000
*                                                                       07200000
PRQALOOP IC    R14,0(R7)      LENGTH OF TABLE ENTRY                     07210000
         LA    R6,PRPNDSN+2(R5) POINT AT LAST 2 CHARS OF DSN            07220000
         SR    R6,R14           BACK UP BY LENGTH OF TABLE ENTRY        07230000
         EX    R14,PRQACMP      COMPARE TO TABLE ENTRY, AND QUIT        07240000
         BE    PRUNIT              IF A MATCH IS FOUND                  07250000
*                                                                       07260000
         LA    R14,2(R14)        INCREMENT TO ACTUAL LENGTH             07270000
         BXLE  R7,R14,PRQALOOP      SEARCH CONTINUES                    07280000
*                                                                       07290000
         LA     R6,PRPNDSN+1(R5)    POINT PAST ENTERED NAME             07300000
         MVC   2(5,R6),=C'.DATA'                                        07310000
         LA    R5,6(R5)                         AND INCREMENT ACTUAL    07320000
PRASMD   STH   R5,PRPNDSN                       LENGTH                  07330000
*                                                                       07340000
*                                                                       07350000
PRUNIT   LA    R5,PRPNDSN              ADDR OF DAIR DSN                 07360000
         ST    R5,DA04PDSN                                              07370000
*                                                                       07380000
         LA    R6,XDAPL                   WE EXPECT THE DAPL TO BE      07390000
         USING DAPL,R6                         ALREADY FORMATTED        07400000
         ST    R8,DAPLDAPB                                              07410000
         DROP  R6                                                       07420000
*                                                                       07430000
         LR    R1,R6                       DAPL ADDR INTO R1            07440000
         LINK  EP=IKJEFD00                 ALLOCATE OR ELSE             07450000
*                                                                       07460000
         LA    R9,DA08AREA           MAKE ALLOCATE WKA ADDRESSABLE      07470000
         USING DAPB08,R9                                                07480000
*                                                                       07490000
         MVC   DA08DSP1(3),=X'040204'  DEFAULT DISP=(NEW,CATLG,DELETE)  07500000
         OI    DA08CTL,X'10'           DEFAULT SPACE IS RLSE            07510000
         MVC    DA08CTL(1),DA04CTL       CARRY ACROSS USERID            07520000
         MVC   DA08PDSN(4),DA04PDSN       CARRY ACROSS DSNAME           07530000
*                                                                       07540000
         B     *+4(R15)        INDEXED BY RC FROM DAIR                  07550000
         B     PROLD       RC=0 MEANS IT EXISTS                         07560000
         B     PRDAIRER    RC=4 MEANS OUR ERROR                         07570000
         CLC   DA04CTRC(2),=X'0008'  RC=8 AND CATALOG8 MEANS DISP=NEW   07580000
         BE    PRRET3                                                   07590000
         B     PRDAIRER                                                 07600000
*                                                                       07610000
PROLD    MVC   DA08DSP1(3),=X'010808'      DISP=(OLD,KEEP,KEEP) AND     07620000
         NI    DA08CTL,B'00100000'           ONLY ALLOW USERID          07630000
         TM    DA04FLG,X'02' IS DSNAME IN DSE?              JFS03/27/78 07640000
         BZ    PRRET3        NO                             JFS03/27/78 07650000
         TM    DA04DSO,X'40'                 IS IT DSORG=PS             07660000
         BO    PRRET3                            YES - OK               07670000
*                                                                       07680000
         LA    R6,OUTMESS+16  OUTPUT ERROR MESSAGE                  JFS 07690000
         TM    PARPND+6,X'40'  DATA SET NAME IN QUOTES?             JFS 07700000
         BZ    PRNOQ1          NO                                   JFS 07710000
         MVI   0(R6),C''''     YES, MOVE IN FIRST QUOTE             JFS 07720000
         LA    R6,1(R6)  INCREMENT FOR QUOTE                        JFS 07730000
PRNOQ1   DC    0H'0'                                                JFS 07740000
         L     R15,PARPND  LOAD ADDR OF DSNAME                      JFS 07750000
         LH    R14,PARPND+4  LOAD LENGTH OF DSNAME                  JFS 07760000
         BCTR  R14,R0  DECREMENT FOR MVC INST.                      JFS 07770000
         EX    R14,PRMVC   MOVE DSNAME                              JFS 07780000
         LA    R6,1(R6,R14)   ADD LENGTH OF DSNAME                  JFS 07790000
         TM    PARPND+6,X'40'  DSNAME IN QUOTES?                    JFS 07800000
         BZ    PRNOQ2          NO                                   JFS 07810000
         MVI   0(R6),C''''     YES, MOVE IN LAST QUOTE              JFS 07820000
         LA    R6,1(R6)  FOR THE QUOTE                              JFS 07830000
PRNOQ2   DC    0H'0'                                                JFS 07840000
         LA    R6,1(R6)   FOR 1 SPACE AFTER DSNAME                  JFS 07850000
         MVC   0(L'MSGNOTPS,R6),MSGNOTPS     MOVE IN MESSAGE       SFI  07860000
         B     BADOUT             GO PUT MESS                      SFI  07870000
*                                                                       07880000
*                                                                       07890000
PRRET3   BR    R3                                                       07900000
*                                                                       07910000
PRQACMP  CLC   0(0,R6),1(R7)   VARIABLE LENGTH COMPARE FOR              07920000
*                                    QUALIFIER                          07930000
PRMVC    MVC   0(0,R6),0(R15)   MOVE INST.                          JFS 07940000
*                                                                   JFS 07950000
         DROP  R8,R9,R10                                                07960000
         EJECT                                                          07970000
PRNTALOC IC    R1,PRINTSW                  ARE WE NEEDED?               07980000
         EX    R1,PRNTSNAG                     IF NOT, GO BACK          07990000
         BR    R3                                                       08000000
*                                                                       08010000
PRNTSNAG BC    0,*+4            CONTINUE IF NEEDED                      08020000
         LA    R8,DA08AREA      MAKE WORK AREAS ADDRESSABLE             08030000
         USING DAPB08,R8                                                08040000
*                                                                       08050000
         MVI   DA08DDN,C' '               BLANK UNIT,SERIAL#, MEMBER    08060000
         MVC   DA08DDN+1(23),DA08DDN      NAME, PASSWORD, AND           08070000
         MVC   DA08MNM(16),DA08DDN        SYSOUT CLASS                  08080000
         MVI   DA08DSO,X'40'              DSORG=(PS)                    08090000
         MVC   DA08BLK(12),PRSPACE        SPACE=(1680,(20,10),RLSE)     08100000
         OI    DA08CTL,X'40'              ALLOCATE IN BLOCKS            08110000
*                                                                       08120000
         LA    R6,XDAPL                   WE EXPECT THE DAPL TO BE      08130000
         USING DAPL,R6                         ALREADY FORMATTED        08140000
         ST    R8,DAPLDAPB                                              08150000
         DROP  R6                                                       08160000
*                                                                       08170000
         LR    R1,R6        DAPL INTO R1                                08180000
         LINK  EP=IKJEFD00                                              08190000
         LTR   R15,R15                                                  08200000
         BNZ   PRDAIRER                                                 08210000
         TM    DA08DSO,X'40'  IS DATA SET SEQUENTIAL?      JFS03/27/78  08220000
         BO    PRNTSEQ        YES                          JFS03/27/78  08230000
         L     R10,SAVPARMD   LOAD ADDR OF PARSE PARM      JFS03/27/78  08240000
PRNTSEQ  DC    0H'0'                                       JFS03/27/78  08250000
*                                                                       08260000
         LA    R6,PRDCB              OUTPUT DCB                         08270000
         USING IHADCB,R6                                                08280000
         MVC   DCBDDNAM(8),DA08DDN   PUT DDNAME INTO OUTPUT DCB         08290000
         OPEN  (PRDCB,(OUTPUT))                                         08300000
         TM    DCBOFLGS,B'10000'                                        08310000
         BNO   PROPNERR                                                 08320000
*                                                                       08330000
         LH    R6,LINELENG          LIMIT LINE LENGTH TO 70             08340000
         LA    R7,70                                                    08350000
         CR    R6,R7                                                    08360000
         BNH   PRRET                                                    08370000
         STH   R7,LINELENG                                              08380000
*                                                                       08390000
PRRET    BR    R3                                                       08400000
*                                                                       08410000
PRDSNMV  MVC   PRPNDSN+2(0),0(R6)   GET PRINT DSN INTO DAIR FORMAT      08420000
         DROP  R6,R8                                                    08430000
         EJECT                                                          08440000
RESTOHEX LA    R6,4               PREPARE WORK REGISTERS                08450000
*                                    R6 GETS A BIT FOR EACH SIGNIFICANT 08460000
         LR    R9,R6                 DIGIT. R7 IS THE SOURCE OF BITS.   08470000
         SR    R7,R7                 IT IS EITHER ALL F'S OR ALL 0'S.   08480000
         LA    R10,1                 R10 IS A ONE-TIME SWITCH.          08490000
*                                    R15 HAS THE ARGUMENT. R14 IS FOR   08500000
*                                    WORK, R4, AND R5 GET RESULTS.      08510000
RESLOOP  SLDL  R4,8              MOVE PREVIOUS RESULT OVER              08520000
         SR    R14,R14                                                  08530000
         SLDL  R14,4             GET 4 BITS INTO R14                    08540000
         LTR   R14,R14           ARE WE SIGNIFICANT?                    08550000
         BZ    *+6                   NO  - LEAVE BITS ZERO              08560000
         BCTR  R7,0                  YES - SET SOURCE BITS ON           08570000
*                                                                       08580000
         SLDL  R6,1              MOVE BIT OR ZERO, DEPENDING ON         08590000
         IC    R5,ONETOF(R14)        SIGNIFICANCE, AND TRANSLATE        08600000
*                                    THE BITS IN R14                    08610000
         BCT   R9,RESLOOP        AFTER 4 NIBBLES WE RESET THE           08620000
         BCT   R10,RESOUT            SIGNIFICANCE SWITCH AND LOOP       08630000
         SR    R7,R7             AFTER THE SECOND 4 NIBBLES, WE QUIT.   08640000
         SLL   R6,4              R6 WILL CONTAIN BOTH A BIT MAP         08650000
         LA    R6,5(R6)               AND THE REGISTER NUMBERS IN       08660000
         LA    R9,4                   WHICH THE DIGITS ARE HELD.        08670000
         B     RESLOOP                                                  08680000
*                                                                       08690000
RESOUT   BR    R3                                                       08700000
*                                                                       08710000
*                                                                       08720000
*                                                                       08730000
RESTODEC CVD   R15,RESDWD               PACK IT                         08740000
         L     R15,RESDWD+4             BACK INTO REGISTER              08750000
         SRL   R15,4                    SHIFT OFF SIGN                  08760000
         B     RESTOHEX                 CONTINUE AS THOUGH IT WAS HEX   08770000
*                                                                       08780000
         EJECT                                                          08790000
         USING DAPL,R1                                                  08800000
DAIRERR  L     R8,DAPLDAPB          ASSUME R1 STILL POINTS TO DAPL      08810000
         DROP  R1                                                       08820000
         USING DAPB08,R8            WE'LL USE THE '08 PARAMETER BLOCK   08830000
*                                        DSECT, BUT ACTUALLY HANDLE     08840000
*                                        MANY BLOCKS.                   08850000
         OI    RCDE+3,X'0C'         INDICATE BAD OUTPUT            SFI  08860000
         MVC   DAMSGWK(L'DAMSGMSK),DAMSGMSK  INITIALIZE ERROR MESSAGE   08870000
*                                                   WORK AREA.          08880000
         ST    R3,DAIRR3            SAVE RETURN REGISTER                08890000
         ST    R15,DAIR15          SAVE INPUT ARGUMENT                  08900000
*                                                                       08910000
         XC    ECB,ECB              ZERO THE EVENT CONTROL BLOCK        08920000
         LA    R0,STKLEN            LENGTH OF SP78 AREA                 08930000
         ICM   R0,B'1000',=AL1(78)  INDICATE SUBPOOL                    08940000
         GETMAIN R,LV=(0)                                               08950000
*                                                                       08960000
         LR    R2,R1             WE SHARE SP78 WITH THE TERMINAL        08970000
         USING LSD,R2                MONITOR, SO IT WON'T GO AWAY       08980000
         XC    LSD(STKLEN),LSD       WHEN WE TERMINATE.                 08990000
*                                                                       09000000
         LA    R4,INSTGLST       ADDR OF COMMANDS TO BE STACKED         09010000
         LA    R5,32             LENGTH OF A COMMAND                    09020000
         SLL   R5,16                  IN LEFT HALFWORD                  09030000
         LR    R6,R4             ADDR OF FIRST COMMAND TO BE STACKED    09040000
         STM   R4,R6,LSD         INITIALIZE LIST SOURCE DESCRIPTOR      09050000
         MVC   INSTGLST(INSTGL),INSTGMSK    INITIALIZE IN-STORAGE LIST  09060000
         ST    R2,IOPLIOPB       COMPLETE PARAMETER CHAIN FROM R1       09070000
*                                                                       09080000
*                                                                       09090000
         L     R15,DAIR15              GET DAIR RC BACK                 09100000
         BAL   R3,RESTODEC              MAKE IT VISIBLE                 09110000
         LA    R9,DAIRR15             FOR EX                            09120000
         BAL   R3,STCMST             PUT IT INTO MESSAGE                09130000
         LA    R9,INSTGLST+L'INSTDAIR       AND INTO STACK COMMAND      09140000
         BAL   R3,STCMMVC(R7)                                      SFI  09150000
         MVC   LSDTOTLN(2),=H'96'       LENGTH OF THREE ENTRIES         09160000
*                                                                       09170000
         L     R15,DA08CD               ENTRY CODE AND FLAGS            09180000
         BAL   R3,RESTOHEX                                              09190000
         STH   R4,STCMFULL      THEY GO INTO MESSAGE WITHOUT       SFI  09200000
         MVC   DAIRNTRY(2),STCMFULL                                SFI  09210000
         STH   R5,STCMFULL       ZERO SUPPRESSION                  SFI  09220000
         MVC   DAIRFLG(2),STCMFULL                                 SFI  09230000
*                                                                       09240000
         LH    R15,DA08DARC             DAIR RETURN CODE                09250000
         BAL   R3,RESTOHEX                                              09260000
         LA    R9,DAIRDARC                                              09270000
         BAL   R3,STCMST                                           SFI  09280000
         LA    R9,INSTGLST+32+L'INSTDARC                                09290000
         BAL   R3,STCMMVC(R7)                                      SFI  09300000
*                                                                       09310000
         LH    R15,DA08CTRC             CATALOG RETURN CODE             09320000
         BAL   R3,RESTODEC              CONVERT TO DECIMAL              09330000
         LA    R9,DAIRCTRC              CTLG RETURN CODE                09340000
         BAL   R3,STCMST                                           SFI  09350000
         LA    R9,INSTGLST+64+L'INSTCTRC     AND INTO STACK COMMAND     09360000
         BAL   R3,STCMMVC(R7)                                      SFI  09370000
*                                                                       09380000
         SR    R15,R15              CHECK TO SEE IF RC'S ARE THERE.     09390000
         CH    R15,DA08DARC            IS THERE A DARC RETURN CODE?     09400000
         BNE   DAIRCKCL                   YES - LET IT ALONE            09410000
         MVC   INSTGLST+32(32),INSTGLST+64  NO  - SQUEEZE EMPTY ENTRY   09420000
         MVC   LSDTOTLN(2),=H'64'              AND CUT TO LENGTH        09430000
*                                                                       09440000
DAIRCKCL CH    R15,DA08CTRC            IS THERE A CATALOG RC            09450000
         BNE   DAIRSTK                    YES - LET IT ALONE            09460000
         LH    R15,LSDTOTLN               NO  - CUT IT OFF              09470000
         SH    R15,=H'32'                                               09480000
         STH   R15,LSDTOTLN                                             09490000
*                                                                       09500000
DAIRSTK  STACK PARM=DAIRSTKL,ECB=ECB,STORAGE=(LSD,SOURCE),        MMMMMM09510000
               MF=(E,IOPL)                                              09520000
*                                                                       09530000
*                                                                       09540000
         LA    R0,DAMSGLEN         LGTH OF MSG                          09550000
         LA    R1,DAMSGWK          ADDR OF MSG                          09560000
         TPUT  (R1),(R0),R                                              09570000
*                                                                       09580000
*                                                                       09590000
         L     R3,DAIRR3               RETURN ADDRESS                   09600000
         BR    R3                                                       09610000
*                                                                       09620000
STCMST   SRDL  R6,4          SHIFT LENGHT MASK OUT OF R6 TO R7     SFI  09630000
         SLL   R6,4          SHIFT BACK TO SET UP REG FOR STORE    SFI  09640000
         EX    R6,STCMSTOR      STORE DATA                         SFI  09650000
*        ST    0,STCMFULL    STORE THE INDICATED REGISTER          SFI  09660000
         SRL   R7,28         SHIFT LENGTH CODE TO LOW ORDER BYTE   SFI  09670000
         STC   R7,STCMMASK    STORE MASK BYTE                      SFI  09680000
         TR    STCMMASK,STCMTRAN   TRANSLATE TO GET BRANCH OFFSET  SFI  09690000
         IC    R7,STCMMASK   RE-LOAD FOR USE IN INDEX REGISTER     SFI  09700000
         B     STCMMVC(R7)    BRANCH TO CORRECT MOMVC INSTRUCTION  SFI  09710000
* THIS CODE IS USED TO MOVE CORRECT BYTES FOR STCM REPLACEMENT     SFI  09720000
STCMMVC  MVC   0(4,R9),STCMFULL                                    SFI  09730000
         BR    R3                RETURN                            SFI  09740000
         MVC   0(3,R9),STCMFULL+1                                  SFI  09750000
         BR    R3                                                  SFI  09760000
         MVC   0(2,R9),STCMFULL+2                                  SFI  09770000
         BR    R3                                                  SFI  09780000
         MVC   0(1,R9),STCMFULL+3                                  SFI  09790000
         BR    R3                                                  SFI  09800000
STCMTRAN DC    X'06181010080808080000000000000000'                 SFI  09810000
STCMSTOR ST    0,STCMFULL    STORE COMMAND TO BE EXECUTED          SFI  09820000
         EJECT                                                          09830000
         USING IKJPARMD,R10                                             09840000
*                                                                       09850000
DSNFMT   LA    R8,DA08AREA             MAKE DAIR WORK AREAS             09860000
         LA    R9,DA18AREA                 ADDRESSABLE                  09870000
         LA    R4,DA00AREA                 ADDRESSABLE                  09880000
         USING DAPB00,R4                                                09890000
         USING DAPB08,R8                                                09900000
         USING DAPB18,R9                                                09910000
*                                                                       09920000
         MVI   DA08CD+1,X'08'          FILLING OUT DAIR PARAMETER AREAS 09930000
         MVI   DA18CD+1,X'18'          ENTRY CODES                      09940000
*                                                                       09950000
         L     R7,PARDSN               POINTER TO DSN                   09960000
         LH    R6,PARDSN+4             LENGTH OF DSN                    09970000
         STH   R6,DSNBUF               PASS LENGTH TO DAIR              09980000
         BCTR  R6,0                    DECREMENT FOR EXECUTE            09990000
         EX    R6,DSNMOVE              MOVE DSN TO BUFFER               10000000
         EX    R6,DSNMOVE2             PUT DSN IN FINAL MSG             10010000
         B     DSNMOVE+6               JUMP EX 'ED MOVE                 10020000
DSNMOVE  MVC   DSNBUF+2(0),0(R7)     PUT DSN INTO DAIR FORMAT           10030000
DSNMOVE2 MVC   FINDSN(0),0(R7)         DSN INTO FINAL MSG               10040000
*                                                                       10050000
         LA    R6,DSNBUF               ADDR OF DSN BUFFER FOR DAIR      10060000
         ST    R6,DA00PDSN                 PASS TO FIND                 10070000
         ST    R6,DA08PDSN                 PASS TO ALLOCATE             10080000
*                                                                       10090000
         TM    PARDSN+6,B'01000000'    IS THE DSN IN QUOTES             10100000
         BO    DSNDAPL                     YES - DON'T SET USERID FLAG  10110000
         OI    DA00CTL,B'00100000'         NO  - DAIR MUST DO USERID    10120000
         OI    DA08CTL,B'00100000'                                      10130000
*                                                                       10150000
*                                                                       10160000
*              FORMAT DAIR PARMMETER BLOCK                              10170000
*                                                                       10180000
DSNDAPL  LA    R6,XDAPL                                                 10190000
         USING DAPL,R6                                                  10200000
         MVC   DAPLUPT(12),PPLAREA    UPT, ECT, & ECB ARE FROM PARSE    10210000
*                                          PARAMETER LIST               10220000
         LA    R5,DAPLPSCB            TARGET FIELD                      10230000
         EXTRACT (R5),'S',FIELDS=(PSB)   PROTECTED STEP CONTROI BLOCK   10240000
*                                                                       10250000
         BR    R3                                                       10260000
         DROP  R4                                                       10270000
         DROP  R6                                                       10280000
         DROP  R8                                                       10290000
         DROP  R9                                                       10300000
         EJECT                                                          10310000
*                                                                       10320000
*                            * * *  WORKING STORAGE * * *               10330000
*                                                                       10340000
WKPOINTR  DC   F'0'                                                     10350000
*                                                                       10360000
PSALL0   DC    C'ALL',XL5'0'                                            10370000
*                                                                       10380000
LINETAB  DC    X'500A141E28323C46505A'                                  10390000
*                                                                       10400000
TABLEN   DC    F'32000'           TABLE LENGTH IN BYTES            SFI  10410003
*                                                                       10420000
PARSMSG  DC    C'INVALID PARAMETERS SPECIFIED. PLEASE TRY AGAIN.'  SFI  10430000
*                                                                       10440000
DAIRMSG  DC    C'DAIR ERROR.'                                      SFI  10450000
*                                                                       10460000
PARTMSG  DC    C'IS NOT A PDS'                                     SFI  10470000
*                                                                       10480000
OPENMSG  DC    C'-  UNSUCCESSFUL OPEN.'                            SFI  10490000
*                                                                       10500000
MSGNOCOR DC    C'INSUFFIECENT STORAGE TO SORT ALIASES. OUTPUT TRUNCATEDX10510000
               .'                                                       10520000
MSGNOTCT DC    CL80'DATA SET'                                           10530000
*                                                                       10540000
MSGNOTPS DC    C'IS NOT SEQUENTIAL, NOTHING OUTPUT TO DATA SET'     JFS 10550000
*                                                                       10560000
INMESS   DC    CL80'INPUT DATA SET'                                SFI  10570000
*                                                                       10580000
OUTMESS   DC    CL80'OUTPUT DATA SET'                              SFI  10590000
OUTMASK  DC    C'USING MASK  ''            '                       SFI  10600000
FINAL    DC    X'402020202121'                                          10610000
          DC    C'  MEMBERS FOUND IN DATA SET   '                       10620000
FINDSN   DC    CL44' '                                                  10630000
LFINAL   EQU   *-FINAL                                                  10640000
*                                                                       10650000
PRSPACE  DC    A(1680)                                                  10660000
         DC    A(20)                                                    10670000
         DC    A(10)                                                    10680000
*                                                                       10690000
*                                                                       10700000
*                                                                       10710000
CLOSEL   CLOSE (DCB,,PRDCB),MF=L                                        10720000
*                                                                       10730000
ONETOF   DC    C'0123456789ABCDEF'                                      10740000
*                                                                       10750000
DAMSGMSK DC    C'DAIR R15=0    DAIR ENTRY=0    DAIR FLG=0    DAIRDARC=0X10760000
                   DAIRCTRC=0            '                              10770000
*                                                                       10780000
INSTGMSK DS    0C                                                       10790000
INSTDAIR DC    C'HELP ERROR OP(DAIR'                                    10800000
         DC    CL(32-L'INSTDAIR)' '                                     10810000
INSTDARC DC    C'HELP ERROR OP('                                        10820000
         DC    CL(32-L'INSTDARC)' '                                     10830000
INSTCTRC DC    C'HELP ERROR OP(CATALOG'                                 10840000
         DC    CL(32-L'INSTCTRC)' '                                     10850000
INSTGL   EQU   *-INSTGMSK                                               10860000
PRQALTB  DC    AL1(3),C'.ASM'                                           10870000
         DC    AL1(3),C'.PLI'                                           10880000
         DC    AL1(4),C'.DATA'                                          10890000
         DC    AL1(4),C'.CNTL'                                          10900000
         DC    AL1(4),C'.FORT'                                          10910000
         DC    AL1(5),C'.CLIST'                                         10920000
         DC    AL1(4),C'.SPIT'                                     SFI  10930000
PRQALND  DC    AL1(5),C'.COBOL'                                         10940000
*                                                                       10950000
*                                                                       10960000
         LTORG                                                          10970000
*        PRINT NOGEN                                                JFS 10980000
         EJECT                                                          10990000
PARMDSN  IKJPARM                                                        11000000
PARDSN   IKJPOSIT DSNAME,PROMPT='DSNAME OF SUBJECT PDS'                 11010000
PARALIAS IKJKEYWD                                                       11020000
         IKJNAME   'ALIAS'                                              11030000
         IKJNAME   'NOALIAS'                                            11040000
PARPARM  IKJKEYWD                                                       11050000
         IKJNAME  'PREFIX',SUBFLD=PARPFX                                11060000
         IKJNAME  'MASK',SUBFLD=PARPFX                             SFI  11070000
PARLINE  IKJKEYWD                                                       11080000
         IKJNAME  'LINESIZE',SUBFLD=PARLN                          SFI  11090000
PARPNT   IKJKEYWD                                                       11100000
         IKJNAME  'DA',SUBFLD=PARDS                                SFI  11110000
         IKJNAME 'DS',SUBFLD=PARDS                                 SFI  11120000
PARPFX   IKJSUBF                                                        11130000
PARPFXD  IKJIDENT  'STRING',MAXLNTH=8,FIRST=ANY,OTHER=ANY,             X11140000
               PROMPT='MASK'                                            11150000
PARLN    IKJSUBF                                                        11160000
PARLND   IKJIDENT 'NUMBER',MAXLNTH=2,FIRST=NUMERIC,OTHER=NUMERIC,      X11170000
               PROMPT='LINESIZE'                                        11180000
PARDS    IKJSUBF                                                        11190000
PARPND   IKJPOSIT  DSTHING,PROMPT='DSNAME YOU WISH TO RECIEVE OUTPUT'   11200000
         IKJENDP                                                        11210000
*                                                                       11220000
DCB      DCB   DSORG=PS,RECFM=U,BLKSIZE=256,MACRF=(R),EODAD=EODAD       11230000
         EJECT                                                          11240000
PRDCB    DCB   DSORG=PS,RECFM=FB,BLKSIZE=1680,MACRF=(PM),              M11250000
               LRECL=80                                                 11260000
*                                                                       11270000
         EJECT                                                          11280000
*        ADDITIONAL WORK AREAS                                          11290000
*                                                                       11300000
WKA      DSECT                                                          11310000
SAVEAREA DS    18F                                                      11320000
*                                                                       11330000
RCDE     DS    F             RETURN CODE FOR PROGAM                     11340000
DONTFREE DS    F             SWITCH TO PREVENT FREEING A PREALLOCATED   11350000
*                                 DATASET                               11360000
CHAINR3  DS    F                                                        11370000
CHAINR5  DS    F                                                        11380000
FORMR3   DS    F                                                        11390000
FORMR5   DS    F                                                        11400000
*                                                                       11410000
*                                                                       11420000
*                                                                       11430000
XPPL     DS    7F            * RESERVE SEVEN WORDS FOR PARSE    *       11440000
*                            *    PARAMETER  LIST               *       11450000
CBUF     DS    CL80          * COMMAND BUFFER                   *       11460000
ECB      DS    F             * EVENT CONTROL BLOCK FOR PARSE    *       11470000
ANS      DS    F             * POINTER TO PARAMETER DESCRIPTOR  *       11480000
*                            *    LIST                          *       11490000
PPLAREA  DS    7F                                                       11500000
DA00AREA DS     5F                                                      11510000
DA04AREA DS     5F                                                      11520000
*                                                                       11530000
DA08AREA DS    CL84                                                     11540000
         DS    0F                                                       11550000
DA18AREA DS    CL40                                                     11560000
DSNBUF   DS    H                                                        11570000
         DS    CL44                                                     11580000
*                                                                       11590000
XDAPL    DS    5F                      RESERVE 5 WDS FOR DAIR PARMMETER 11600000
*                                          BLOCK                        11610000
*                                                                       11620000
PSAND    DS    CL8                     AND THE MEMBER NAME WITH PSAND,  11630000
PSXC     DS    CL8                         SO IT WILL LOOK LIKE PSXC    11640000
PSARGU   DS    CL8                         FOR AN EQUAL COMPARE         11650000
*                                                                       11660000
LINELENG DS    H                                                        11670000
*                                                                       11680000
DIRDATA  DS    CL256                                                    11690000
PUTLINE  DS    CL120                                                    11700000
*                                                                       11710000
HITCOUNT DS    D                                                   SFI  11720000
LINECNT  DS    PL5                                                      11730000
*                                                                       11740000
PRINTSW  DS    XL1                                                      11750000
*                                                                       11760000
         DS    0H                                                       11770000
PRPNDSN  DS    CL46                                                     11780000
PRSTR3   DS    F                                                        11790000
*                                                                       11800000
IOPL     DS    0F                                                       11810000
IOPLUPT  DS    F                                                        11820000
IOPLECT  DS    F                                                        11830000
IOPLECB  DS    F                                                        11840000
IOPLIOPB DS    F                                                        11850000
*                                                                       11860000
DAIRSTKL DS    D           SAME AS STACK MF=L                           11870000
*                                                                       11880000
*                                                                       11890000
DAIRR3   DS    F                                                        11900000
DAIR15   DS    F                                                        11910000
*                                                                       11920000
DAMSGWK  DS    0C                                                       11930000
         DS    9C                                                       11940000
DAIRR15  DS    5C                                                       11950000
         DS    11C                                                      11960000
DAIRNTRY DS    5C                                                       11970000
         DS    9C                                                       11980000
DAIRFLG  DS    5C                                                       11990000
         DS    9C                                                       12000000
DAIRDARC DS    5C                                                       12010000
         DS    9C                                                       12020000
DAIRCTRC DS    5C                                                       12030000
DAMSGLEN EQU   *-DAMSGWK                                                12040000
STCMMASK DS    C                                                        12050000
STCMFULL DS    F                                                        12060000
SAVPARMD DS    F                  AREA TO SAVE --> PARAMETER LIST  SFI  12070000
*                                                                       12080000
RESDWD   DS    D                                                        12090000
WKLEN    EQU   *-SAVEAREA                                          SFI  12100000
CHANTOP  DS    6000F              THE TABLE                        SFI  12110000
         EJECT                                                          12120000
*                            * * *  DSECTS  * * *                       12130000
*                                                                       12140000
*        PRINT GEN                                                      12150000
         IKJPPL                                                         12160000
       EJECT                                                            12170000
         IKJDAP00        SEARCH                                         12180000
       EJECT                                                            12190000
         IKJDAP04        SEARCH                                         12200000
         EJECT                                                          12210000
         IKJDAP08        ALLOCATE                                       12220000
         EJECT                                                          12230000
         IKJDAP18        DEALLOCATE                                     12240000
         EJECT                                                          12250000
         IKJDAPL         DAIR PARAMETER BLOCK                           12260000
         EJECT                                                          12270000
         IKJCPPL                                                        12280000
         EJECT                                                          12290000
*                                                                       12300000
*                                                                       12310000
*        THIS IS THE LIST SOURCE DESCRIPTOR, AND THE IN-STORAGE         12320000
*        LIST THAT ARE PASSED TO STACK. THEY GO INTO SP78, SO           12330000
*        THEY ARE NOT FREED WHEN SHOWDIR ENDS.                          12340000
*                                                                       12350000
LSD      DSECT                                                          12360000
LSDADATA DS    F                                                        12370000
LSDRCLEN DS    H                                                        12380000
LSDTOTLN DS    H                                                        12390000
LSDANEXT DS    F                                                        12400000
LSDRSVRD DS    F                                                        12410000
*                                                                       12420000
*                                                                       12430000
*                                                                       12440000
INSTGLST DS    4CL32                                                    12450000
*                                                                       12460000
STKLEN   EQU   *-LSD                                                    12470000
         EJECT                                                          12480000
*        PRINT NOGEN                                                    12490000
         DCBD                                                           12500000
         EJECT                                                          12510000
*./ ADD   NAME=SHOWDIR                                                  12520000
*./ NUMBER NEW1=10,INCR=10                                              12530000
*./ ALIAS   NAME=LM                                                     12540002
*)F FUNCTION -                                                          12550000
*  THE SHOWDIR COMMAND IS USED TO PRINT OUT PART OR ALL OF              12560000
*  THE DIRECTORY OF A PARTITIONED DATA SET.                             12570000
*  OPTIONALLY, IT WILL PRODUCE A DISK DATA SET SUITABLE                 12580000
*  FOR MANIPULATION WITH EDIT.                                          12590000
*  SHOWDIR GENERATES A RETURN CODE OF ZERO IF SUCCESSFUL.               12600000
*  SHOULD NO MEMBERS MEET THE REQUIREMENTS FOR LISTING (SEE MASK        12610000
*  PARAMETER) A RETURN CODE OF 4 IS RETURNED. THE FOLLOWING             12620000
*  RETURN CODES INDICATE UNSUCCESSFUL COMPLETION OF SHOWDIR.            12630000
*      08 - OUTPUT INCOMPLETE OR OUTPUT DATA SET NOT CREATED.           12640000
*      12 - INPUT DATA SET UNREADABLE OR NOT PARTITIONED.               12650000
*      16 - PARSE ERROR PROCESSING USER'S PARAMETERS.                   12660000
*  WHENEVER A NON-ZERO RETURN CODE IS GENERATED, AN APPROPRIATE         12670000
*  MESSAGE WILL BE DISPLAYED AT THE USER'S TERMINAL.                    12680000
*)X SYNTAX  -                                                           12690000
*         SHOWDIR  'DSNAME'   LINESIZE('SIZE')    MASK('MASK')          12700000
*                  DA('DSN')  ALIAS/NOALIAS                             12710000
*  REQUIRED - DSNAME                                                    12720000
*  DEFAULTS - LINESIZE=7    MASK=ALL        ALIAS                       12730000
*  ALIAS    - LM                                                        12740002
*)O OPERANDS -                                                          12750000
*   'DSNAME'         - THE PARTITIONED DATA SET FOR WHICH THE DIRECTORY 12760000
*                      LISTING IS REQUIRED.                             12770000
*))LINESIZE('SIZE') - THE NUMBER OF MEMBER NAMES TO BE DISPLAYED        12780000
*                      ON A SINGLE LINE. MAXIMUM IS 9.                  12790000
*                      IF THE "DA" OPTION IS TAKEN, THE FIRST           12800000
*                      MEMBER NAME ON EACH LINE WILL BEGIN IN           12810000
*                      POSITION 1.                                      12820000
*))MASK('MASK')   - AN OPTIONAL MASK OR PREFIX  USED AS A SEARCH MASK.  12830000
*               UP TO EIGHT CHARACTERS ON WHICH TO COMPARE              12840000
*               FOR EQUAL BEFORE PRINTING. AN ASTERISK (*), IN ANY      12850000
*               POSITION MAKES IT A NON-COMPARE POSITION.               12860000
*               EXAMPLES:                                               12870000
*                                                                       12880000
*                    MASK(BPS)    WILL RETURN ANY MEMBER NAME           12890000
*                                   THAT BEGINS WITH "BPS".             12900000
*                                                                       12910000
*                    PRE(*****XYS)  WILL RETURN ANY MEMBER NAME THAT    12920000
*                                   ENDS IN "XYZ" IN COLUMNS 6-8.       12930000
*                                                                       12940000
*                    M(BPS****7)   WILL RETURN ANY MEMBER NAME THAT     12950000
*                                   BEGINS "BPS" AND ENDS IN A "7".     12960000
*                                                                       12970000
*))PREFIX('MASK')    - SAME AS MASK PARAMETER                           12980000
*                                                                       12990000
*))DA('DSN')      WILL RETURN THE LIST OF MEMBER NAMES IN               13000000
*               A SEQUENTIAL DATA SET NAMED, "USERID.(DSN).DATA".       13010000
*               A DSNAME IN QUOTES WILL BE RESPECTED, AS WILL A VALID   13020000
*               QUALIFIER (ASM,CLIST,CNTL,COBOL,DATA,FORT,PLI OR SPIT). 13030000
*               THE DATA SET WILL HAVE SEQUENCE NUMBERS IN EDIT FORMAT. 13040000
*               N.B. - DA(*) WILL BE TREATED AS IF THE DA               13050000
*               PARAMETER WAS NOT ENTERED AT ALL (OUTPUT WILL GO TO     13060000
*               THE USER'S TERMINAL').                                  13070000
*                                                                       13080000
*))DS('DSN')   SAME AS DA PARAMETER                                     13090000
*                                                                       13100000
*))ALIAS/NOALIAS    - "NOALIAS" WILL SUPPRESS THE SPECIAL FORMATTING    13110000
*               GIVEN TO ALIAS NAMES. "ALIAS" IS THE DEFAULT.           13120000
*               NOTE -- AN "*ORPHAN*" IS AN ALIAS NAME THAT             13130000
*               MATCHES NO MAIN MEMBER NAME.                            13140000
*               A SPURIOUS *ORPHAN* WILL BE GENERATED WHEN              13150000
*               AN ALIAS MEMBER NAME MATCHES THE SEARCH MASK,           13160000
*               BUT THE MAIN NAME DOES NOT. ALSO, AN ALIAS WILL NOT     13170000
*               BE SHOWN IF IT DOES NOT MATCH THE SEARCH MASK           13180000
*               EVEN THOUGH THE MAIN MEMBER NAME MATCHES AND            13190000
*               IS DISPLAYED.                                           13200000
         END                                                            13210000
//LKED.SYSLMOD  DD DISP=SHR,DSN=SYS2.CMDLIB                             00120000
//LKED.SYSPRINT DD SYSOUT=*                                             00130000
//LKED.SYSIN    DD *                                                    00140000
 ALIAS SHOWDIR                                                          00150000
 NAME LM(R)                                                             00170000
/*                                                                      00180000
//HELP    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)                        00190000
//SYSPRINT DD  SYSOUT=*                                                 00200000
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP                                   00210000
//SYSIN    DD  *                                                        00220000
./ ADD NAME=LM                                                          00230000
./ ALIAS NAME=SHOWDIR                                                   00250000
)F FUNCTION -
  THE SHOWDIR COMMAND IS USED TO PRINT OUT PART OR ALL OF
  THE DIRECTORY OF A PARTITIONED DATA SET.
  OPTIONALLY, IT WILL PRODUCE A DISK DATA SET SUITABLE
  FOR MANIPULATION WITH EDIT.
  SHOWDIR GENERATES A RETURN CODE OF ZERO IF SUCCESSFUL.
  SHOULD NO MEMBERS MEET THE REQUIREMENTS FOR LISTING (SEE MASK
  PARAMETER) A RETURN CODE OF 4 IS RETURNED. THE FOLLOWING
  RETURN CODES INDICATE UNSUCCESSFUL COMPLETION OF SHOWDIR.
      08 - OUTPUT INCOMPLETE OR OUTPUT DATA SET NOT CREATED.
      12 - INPUT DATA SET UNREADABLE OR NOT PARTITIONED.
      16 - PARSE ERROR PROCESSING USER'S PARAMETERS.
  WHENEVER A NON-ZERO RETURN CODE IS GENERATED, AN APPROPRIATE
  MESSAGE WILL BE DISPLAYED AT THE USER'S TERMINAL.
)X SYNTAX  -
         LM  'DSNAME'   LINESIZE('SIZE')    MASK('MASK')
              DA('DSN')  ALIAS/NOALIAS
  REQUIRED - DSNAME
  DEFAULTS - LINESIZE=7    MASK=ALL        ALIAS
  ALIAS    - SHOWDIR, LISTMEM
)O OPERANDS -
   'DSNAME'         - THE PARTITIONED DATA SET FOR WHICH THE DIRECTORY
                      LISTING IS REQUIRED.
))LINESIZE('SIZE') - THE NUMBER OF MEMBER NAMES TO BE DISPLAYED
                      ON A SINGLE LINE. MAXIMUM IS 9.
                      IF THE 'DA' OPTION IS TAKEN, THE FIRST
                      MEMBER NAME ON EACH LINE WILL BEGIN IN
                      POSITION 1.
))MASK('MASK')   - AN OPTIONAL MASK OR PREFIX  USED AS A SEARCH MASK.
               UP TO EIGHT CHARACTERS ON WHICH TO COMPARE
               FOR EQUAL BEFORE PRINTING. AN ASTERISK (*), IN ANY
               POSITION MAKES IT A NON-COMPARE POSITION.
               EXAMPLES:

                    MASK(BPS)    WILL RETURN ANY MEMBER NAME
                                   THAT BEGINS WITH 'BPS'.

                    PRE(*****XYS)  WILL RETURN ANY MEMBER NAME THAT
                                   ENDS IN 'XYZ' IN COLUMNS 6-8.

                    M(BPS****7)   WILL RETURN ANY MEMBER NAME THAT
                                   BEGINS 'BPS' AND ENDS IN A '7'.

))PREFIX('MASK')    - SAME AS MASK PARAMETER

))DA('DSN')      WILL RETURN THE LIST OF MEMBER NAMES IN
               A SEQUENTIAL DATA SET NAMED, 'USERID.(DSN).DATA'.
               A DSNAME IN QUOTES WILL BE RESPECTED, AS WILL A VALID
               QUALIFIER (ASM,CLIST,CNTL,COBOL,DATA,FORT,PLI OR SPIT).
               THE DATA SET WILL HAVE SEQUENCE NUMBERS IN EDIT FORMAT.
               NOTE - DA(*) WILL BE TREATED AS IF THE DA
               PARAMETER WAS NOT ENTERED AT ALL (OUTPUT WILL GO TO
               THE USER'S TERMINAL).

))DS('DSN')   SAME AS DA PARAMETER

))ALIAS/NOALIAS    - 'NOALIAS' WILL SUPPRESS THE SPECIAL FORMATTING
               GIVEN TO ALIAS NAMES. 'ALIAS' IS THE DEFAULT.
               NOTE -- AN '*ORPHAN*' IS AN ALIAS NAME THAT
               MATCHES NO MAIN MEMBER NAME.
               A SPURIOUS *ORPHAN* WILL BE GENERATED WHEN
               AN ALIAS MEMBER NAME MATCHES THE SEARCH MASK,
               BUT THE MAIN NAME DOES NOT. ALSO, AN ALIAS WILL NOT
               BE SHOWN IF IT DOES NOT MATCH THE SEARCH MASK
               EVEN THOUGH THE MAIN MEMBER NAME MATCHES AND
               IS DISPLAYED.

./ ENDUP                                                                00270000
//                                                                      00280000
