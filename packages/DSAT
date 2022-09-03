//DSAT  JOB (TSO),
//             'Install DSAT',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00000300
//* Source: CBT (V488) FILE #860 Member DSAT                            00000401
//* Target: SYS2.CMDLIB   SYS2.HELP                                     00000500
//*                                                                     00000600
//********************************************************************* 00000700
//* This JOB installs DSAT (dataset attributes TSO command).          * 00000801
//********************************************************************* 00000900
//*                                                                     00001000
//INSTALL PROC SOUT='*',               <=== SYSOUT CLASS                00001100
//             LIB='SYS2.CMDLIB',      <=== TARGET LOAD LIBRARY         00001200
//             HELP='SYS2.HELP',       <=== HELP LIBRARY                00001300
//             SYSTS=SYSDA,            <=== UNITNAME FOR WORK DATASETS  00001400
//             ASMBLR=IFOX00,          <=== NAME OF YOUR ASSEMBLER      00001500
//             ALIB='SYSC.LINKLIB',    <=== LOCATION OF YOUR ASSEMBLER  00001600
//             MACLIB='SYS1.MACLIB',   <=== MACLIB DATASET NAME         00001700
//             AMODGEN='SYS1.AMODGEN'  <=== AMODGEN DATASET NAME        00001801
//*                                                                     00001900
//IEBUPDTE EXEC PGM=IEBUPDTE,PARM=NEW                                   00002000
//SYSPRINT DD  SYSOUT=&SOUT                                             00002100
//SYSUT1   DD  DSN=&HELP,DISP=SHR                                       00002200
//SYSUT2   DD  DSN=&HELP,DISP=SHR                                       00002300
//*                                                                     00002400
//ASM     EXEC PGM=&ASMBLR,REGION=4096K,                                00002501
//             PARM='NOOBJECT,DECK,NOALIGN,LIST,NOXREF'                 00002601
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00002700
//SYSTERM  DD  SYSOUT=&SOUT                                             00002800
//SYSPRINT DD  SYSOUT=&SOUT                                             00002900
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00003000
//         DD  DSN=&AMODGEN,DISP=SHR                                    00003100
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00003200
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00003300
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00003400
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(,PASS,DELETE),            00003500
//             SPACE=(TRK,(10,1),RLSE),                                 00003600
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)                      00003700
//*                                                                     00003800
//LKED    EXEC PGM=IEWL,COND=(0,NE),PARM='LIST,MAP,XREF'                00003900
//SYSPRINT DD  SYSOUT=&SOUT                                             00004000
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,10)                               00004100
//SYSLMOD  DD  DSN=&LIB,DISP=SHR                                        00004200
//SYSLIN   DD  DSN=&&SYSLIN,DISP=(OLD,DELETE)                           00004300
//         DD  DDNAME=SYSIN                                             00004400
//        PEND                                                          00004500
//*                                                                     00004600
//        EXEC INSTALL                                                  00004700
//*                                                                     00004800
//IEBUPDTE.SYSIN DD *                                                   00004900
./ ADD NAME=DSAT                                                        00005001
./ NUMBER NEW1=10,INCR=10                                               00005100
)F FUNCTION -                                                           00005201
               THE DSAT COMMAND IS USED TO DISPLAY ALLOCATION           00005301
               INFORMATION FOR DATA SETS ON A DIRECT ACCESS             00005401
               DEVICE.                                                  00005501
                                                                        00005601
               DSAT WILL SEARCH THE OS CATALOG AND CVOLS FOR THE        00005701
               ENTRIES FOR THE DATA SETS SPECIFIED.  ALLOCATION         00005801
               INFORMATION WILL BE OBTAINED FROM THE VOLUME TABLE       00005901
               OF CONTENTS, FORMATTED AND DISPLAYED.  IF A NAME IS      00006001
               AN INDEX NAME, ALL DATA SETS BELOW THE INDEX WILL        00006101
               BE DISPLAYED.                                            00006201
                                                                        00006301
               THE USER MAY BYPASS THE CATALOG SEARCH BY SUPPLYING      00006401
               THE VOLUME SERIAL ON WHICH THE DATA SET RESIDES.         00006501
               THIS OPTION PERMITS DISPLAYING INFORMATION FOR           00006601
               UNCATALOGED DATA SETS.                                   00006701
                                                                        00006801
               THE ATTRIBUTES TO BE DISPLAYED MAY BE SELECTED BY        00006901
               THE USER WHEN HE ENTERS THE DSAT COMMAND BY              00007001
               SPECIFYING KEYWORD OPERANDS.                             00007101
                                                                        00007201
               THE DSAT COMMAND MAY BE USED IN COMMAND PROCEDURES       00007301
               TO FIND THE ALLOCATION OF A DATA SET OR A GROUP OF       00007401
               DATA SETS AND SET THE RETURN CODE TO THE SPECIFIED       00007501
               VALUE.  THE RETURN CODE MAY THEN BE TESTED WITH THE      00007601
               WHEN COMMAND.  OUTPUT MAY BE SUPPRESSED BY               00007701
               SPECIFYING NOPRINT.  THE HARDCOPY KEYWORD MAY BE USED    00007801
               TO DIRECT THE OUTPUT TO A PRE-ALLOCATED DATA SET.        00007901
                                                                        00008001
               THE USER MAY CHOOSE WHAT INFORMATION WILL BE             00008101
               DISPLAYED BY ENTERING KEYWORDS.                          00008201
                                                                        00008301
               THE INFORMATION THAT MAY BE DISPLAYED IS:                00008401
                1. VOLUME SERIAL ON WHICH THE DATA SET IS LOCATED.      00008501
                2. FILE SEQUENCE NUMBER.                                00008601
                3. DEVICE TYPE CODE FROM CATALOG ENTRY.                 00008701
                4. ALLOCATION  (ALLOCATED, USED, AND EXTENTS).          00008801
                5. SECONDARY ALLOCATION (AMOUNT AND UNITS).             00008901
                6. DATA SET ORGANIZATION.                               00009001
                7. DCB (RECFM, LRECL, AND BLKSIZE).                     00009101
                8. CREATION DATE.                                       00009201
                9. EXPIRATION DATE.                                     00009301
               10. LAST DATE REFERENCED (MVS SU 60).                    00009401
               11. FULLY QUALIFIED DATA SET NAME.                       00009501
               12. CCHHR OF THE FORMAT 1 DSCB.                          00009601
               13. GENERATION DATA GROUP DATA.                          00009701
               14. PDS DIRECTORY INFORMATION.                           00009801
)X SYNTAX -                                                             00009901
               DSAT (NAME-LIST)  SERIAL/NOSERIAL   ALLOC/NOALLOC        00010001
                                 DSORG/NODSORG     DCB/NODCB            00010101
                                 CRDATE/NOCRDATE   EXDATE/NOEXDATE      00010201
                                 LASTREF/NOLASTREF                      00010301
                                 TOTALS/NOTOTALS   PRINT/NOPRINT        00010401
                                 HEADER/NOHEADER   ALL/DAONLY           00010501
                                 GDGDATA/NOGDGDATA SEQNO/NOSEQNO        00010601
                                 DEVTYPE/NODEVTYPE DEVICE/NODEVICE      00010701
                                 CCHHR/NOCCHHR     PDS/NOPDS            00010801
                                 SECONDARY/NOSECONDARY                  00010901
                                 GENERIC           DSONLY               00011001
                                 YMD/MDY/DMY                            00011101
                                 HARDCOPY(DDNAME)  VOLUME(SERIAL)       00011201
                                 RC(TALLOC/TUSED/TDIFF/                 00011301
                                    LALLOC/LUSED/LDIFF/NUM/             00011401
                                    PREVIOUS/DSORG/DIRALLOC/            00011501
                                    DIRUSED/ENTRIES/ALIASES/            00011601
                                    MEMBERS)                            00011701
                                                                        00011801
               DEFAULTS -  NAME-LIST DEFAULTS TO PREFIX                 00011901
                           SERIAL   ALLOC   DSORG    DCB    CRDATE      00012001
                           NOEXDATE TOTALS  PRINT    HEADER ALL         00012101
                           NOPDS    NOSEQNO NODEVICE NODEVTYPE          00012201
                           NOGDGDATA    NOLASTREF    NOSECONDARY        00012301
)O OPERANDS -                                                           00012401
               NAME-LIST - ONE OR MORE DATA SET OR INDEX LEVEL          00012501
                           NAMES. TSO NAMING CONVENTIONS ARE USED.      00012601
                           IF A SPECIFIED NAME IS AN INDEX NAME,        00012701
                           DATA SETS UNDER THAT LEVEL WILL BE           00012801
                           DISPLAYED.  IF NO NAME IS SPECIFIED,         00012901
                           THE PREFIX WILL BE USED.  FOR MVT AND        00013001
                           SVS, THIS IS THE USERID.  FOR MVS,           00013101
                           THE PREFIX MAY BE SPECIFIED WITH THE         00013201
                           PROFILE COMMAND.  IF NO NAME IS              00013301
                           SPECIFIED AND THE PROFILE SPECIFIES          00013401
                           NOPREFIX, THE USERID WILL BE USED.           00013501
                                                                        00013601
                           NOTE - IF A KEYWORD IS ENTERED,              00013701
                                  NAME-LIST MUST BE SPECIFIED.  IF      00013801
                                  NAME-LIST IS OMITTED, IT WILL         00013901
                                  NOT DEFAULT TO THE PREFIX, BUT        00014001
                                  THE KEYWORD WILL BE INTERPRETED       00014101
                                  AS THE NAME-LIST, NOT A KEYWORD.      00014201
))SERIAL        LIST VOLUME SERIALS.                                    00014301
))NOSERIAL      DO NOT LIST VOLUME SERIALS.                             00014401
))SEQNO         LIST FILE SEQUENCE NUMBERS.                             00014501
))NOSEQNO       DO NOT LIST FILE SEQUENCE NUMBERS.                      00014601
))DEVTYPE       DISPLAY DEVICE TYPE CODE.                               00014701
))NODEVTYPE     DO NOT DISPLAY DEVICE TYPE CODE.                        00014801
))DEVICE        LIST DEVICE NAME (3330, 3350, ETC.).                    00014901
))NODEVICE      DO NOT LIST DEVICE NAME.                                00015001
))ALLOC         LIST ALLOCATION INFORMATION (TRACKS ALLOCATED,          00015101
                TRACKS USED, AND EXTENTS).                              00015201
))NOALLOC       DO NOT LIST ALLOCATION INFORMATION.                     00015301
))SECONDARY     DISPLAY SECONDARY ALLOCATION INFORMATION.               00015401
))NOSECONDARY   DO NOT DISPLAY SECONDARY ALLOCATION INFORMATION.        00015501
))DSORG         LIST DATA SET ORGANIZATION.                             00015601
))NODSORG       DO NOT LIST DATA SET ORGANIZATION.                      00015701
))PDS           LIST PDS DIRECTORY INFORMATION (BLOCKS ALLOC, USED,     00015801
                ENTRIES, AND ALIASES).                                  00015901
))NOPDS         DO NOT LIST PDS DIRECTORY INFORMATION.                  00016001
))DCB           LIST DCB INFORMATION (RECFM, BLKSIZE, AND LRECL).       00016101
))NODCB         DO NOT LIST DCB INFORMATION.                            00016201
))CREATE        LIST CREATION DATE.                                     00016301
))NOCRDATE      DO NOT LIST CREATION DATE.                              00016401
))EXDATE        LIST EXPIRATION DATE.                                   00016501
))NOEXDATE      DO NOT LIST EXPIRATION DATE.                            00016601
))LASTREF       DISPLAY LAST DATE REFERENCED (MVS SU 60).               00016701
))NOLASTREF     DO NOT DISPLAY LAST DATE REFERENCED.                    00016801
))YMD/MDY/DMY   SPECIFIES THE FORMAT IN WHICH THE                       00016901
                ABOVE DATES ARE DISPLAYED.                              00017001
))CCHHR         DISPLAY FORMAT 1 DSCB ADDRESS.                          00017101
))NOCCHHR       DO NOT DISPLAY CCHHR.                                   00017201
))TOTALS        DISPLAY TOTALS (TRACKS ALLOCATED, TRACKS USED,          00017301
                AND DATA SETS DISPLAYED).                               00017401
))NOTOTALS      DO NOT DISPLAY TOTALS.                                  00017501
))PRINT         OUTPUT IS TO BE DISPLAYED.                              00017601
))NOPRINT       OUTPUT IS NOT TO BE DISPLAYED, EXCEPT FOR ERROR         00017701
                MESSAGES.                                               00017801
))HEADER        DISPLAY OUTPUT HEADER LINE.                             00017901
))NOHEADER      DO NOT DISPLAY HEADER LINE.                             00018001
))ALL           DISPLAY ALL DATA SET NAMES UNDER AN INDEX LEVEL.        00018101
))DAONLY        DISPLAY DATA SETS ON MOUNTED DIRECT ACCESS              00018201
                DEVICES.                                                00018301
))GDGDATA       INFORMATION ON GENERATION DATA GROUP INDEXES IS TO      00018401
                BE DISPLAYED.                                           00018501
))NOGDGDATA     GENERATION DATA GROUP DATA WILL NOT BE DISPLAYED.       00018601
))GENERIC       NAME SPECIFIES A GENERIC KEY.         ** MVS ONLY **    00018701
))DSONLY        TREAT NAMES AS DATA SET NAMES, NOT INDEX NAMES.         00018801
))VOLUME        SPECIFIES THE VOLUME TO BE SEARCHED FOR THE DATA SET.   00018901
))HARDCOPY      SPECIFIES THE DDNAME A PRE-ALLOCATED DATA SET TO        00019001
                RECEIVE A COPY OF DSAT OUTPUT.                          00019101
))RC            SPECIFIES HOW THE RETURN CODE IS TO BE SET:             00019201
                RC NOT SPECIFIED:  4 - SUCCESSFUL COMPLETION            00019301
                                  16 - UNABLE TO INTERPRET COMMAND      00019401
                TALLOC               - TOTAL TRACKS USED                00019501
                TUSED                - TOTAL TRACKS USED                00019601
                TDIFF                - TOTAL OVERALLOCATION             00019701
                                       (TALLOC-TUSED)                   00019801
                TPCTUSE              - PERCENT OF TOTAL USED            00019901
                LALLOC               - TRACKS ALLOCATED BY LAST         00020001
                                       DATA SET                         00020101
                LUSED                - TRACKS USED BY LAST DATA SET     00020201
                LDIFF                - LAST DATA SET OVERALLOCATION     00020301
                                       (LALLOC-LUSED)                   00020401
                LPCTUSE              - PERCENT OF LAST DS TRACK USE     00020501
                NUM                  - NUMBER OF DATA SETS DISPLAYED    00020601
                                       (INCLUDES DATA SETS NOT          00020701
                                       FOUND, ETC.)                     00020801
                PREVIOUS             - ASSUMES THE RETURN CODE OF       00020901
                                       THE PREVIOUS COMMAND.            00021001
                DSORG                - SETS THE RETURN CODE BASED       00021101
                                       ON THE DATA SET ORGANIZATION     00021201
                                       OF THE DATA SETS DISPLAYED.      00021301
                                       1 - ALL DATA SETS WERE SEQ.      00021401
                                       2 - ALL DATA SETS WERE PDS       00021501
                                       3 - DATA SETS WERE MIXED PDS     00021601
                                           AND SEQUENTIAL               00021701
                                       4 - AN ERROR OCCURRED OR A       00021801
                                           DATA SET OTHER THAN PDS      00021901
                                           OR SEQUENTIAL WAS            00022001
                                           PROCESSED                    00022101
                DIRALLOC - SETS THE RETURN CODE TO THE                  00022201
                           NUMBER OF DIRECTORY BLOCKS                   00022301
                           ALLOCATED.                                   00022401
                DIRUSED  - SETS THE RETURN CODE TO THE                  00022501
                           NUMBER OF DIRECTORY BLOCKS                   00022601
                           USED.                                        00022701
                ENTRIES  - SETS THE RETURN CODE TO THE                  00022801
                           NUMBER OF ENTRIES IN THE                     00022901
                           DIRECTORY (MEMBERS AND                       00023001
                           ALIASES).                                    00023101
                ALIASES  - SETS THE RETURN CODE TO THE                  00023201
                           NUMBER OF ALIASES.                           00023301
                MEMBERS  - SETS THE RETURN CODE TO                      00023401
                           ENTRIES-ALIASES.                             00023501
./ ENDUP                                                                00023600
//*----------------------------------------------------------- IEBUPDTE 00023700
//*                                                                     00023800
//ASM.SYSIN DD *                                                        00023900
         MACRO                                                          00024000
         YREGS                                                          00024100
         GBLA  &REGS                                                    00024200
&REGS    SETA  1                                                        00024300
         SPACE 1                                                        00024400
R0       EQU   0                                                        00024500
R1       EQU   1                                                        00024600
R2       EQU   2                                                        00024700
R3       EQU   3                                                        00024800
R4       EQU   4                                                        00024900
R5       EQU   5                                                        00025000
R6       EQU   6                                                        00025100
R7       EQU   7                                                        00025200
R8       EQU   8                                                        00025300
R9       EQU   9                                                        00025400
R10      EQU   10                                                       00025500
R11      EQU   11                                                       00025600
R12      EQU   12                                                       00025700
R13      EQU   13                                                       00025800
R14      EQU   14                                                       00025900
R15      EQU   15                                                       00026000
         SPACE 1                                                        00026100
         MEND                                                           00026200
DSAT     TITLE 'D S A T  ***  DISPLAY DATA SETS AND THEIR ATTRIBUTES'   00026301
*********************************************************************** 00026401
*                                                                     * 00026501
*     THIS PROGRAM, DEVELOPED AT FPL OR SUPPLIED BY OTHER USERS       * 00026601
*     ON A NON-RESTRICTIVE BASIS, IS OF GENERAL INTEREST              * 00026701
*     SUBMITTED FOR UNRESTRICTED DISTRIBUTION.  THIS PROGRAM          * 00026801
*     HAS MET A BASIC SET OF PROGRAMMING AND DOCUMENTATION            * 00026901
*     STANDARDS, BUT MAY NOT HAVE BEEN PROGRAM TESTED IN ANY          * 00027001
*     FORMAL FASHION BY FPL.  THE USER IS EXPECTED TO MAKE THE        * 00027101
*     FINAL EVALUATION AS TO THE USEFULLNESS IN HIS OWN               * 00027201
*     ENVIRONMENT.                                                    * 00027301
*                                                                     * 00027401
*     FPL MAKES NO WARRANTY, EXPRESSED OR IMPLIED, INCLUDING, BUT     * 00027501
*     NOT LIMITED TO, THE IMPLIED WARRANTIES OR MERCHANTABILITY AND   * 00027601
*     FITNESS FOR A PARTICULAR PUTPOSE AS TO THE DOCUMENTATION,       * 00027701
*     FUNCTION, OR PERFORMANCE OF THESE PROGRAMS.                     * 00027801
*                                                                     * 00027901
*     ACCEPTANCE AND USE OF THIS PROGRAM CONSTITUTES A RELEASE        * 00028001
*     FROM LIABILITY OF FPL FOR ANY PROBLEMS USE OF THE PROGRAM       * 00028101
*     MAY CAUSE AT THE USER'S INSTALLATION.                           * 00028201
*                                                                     * 00028301
*     USERS ARE INVITED TO SUBMIT SUGGESTIONS OR ERROR DOCUMENTATION  * 00028401
*     TO FPL, HOWEVER, NO PROMISE CAN BE MADE THAT SUCH SUGGESTIONS   * 00028501
*     WILL BE IMPLEMENTED OR ERRORS CORRECTED.  SUBMIT COMMENTS TO:   * 00028601
*              COORDINATOR OF TECHNICAL SYSTEMS                       * 00028701
*              SYSTEMS & PROGRAMMING DEPARTMENT                       * 00028801
*              FLORIDA POWER & LIGHT COMPANY - GENERAL OFFICE         * 00028901
*              P. O. BOX  529100                                      * 00029001
*              MIAMI, FLORIDA  33152                                  * 00029101
*                                                                     * 00029201
*     THIS PROGRAM IS MADE AVAILABLE BY FPL WITHOUT CHARGE OR         * 00029301
*     CONSIDERATION.  RECIPIENTS ARE FREE TO MAKE THIS PROGRAM        * 00029401
*     AVAILABLE TO OTHERS IN LIKE MANNER.  IT MAY NOT BE SOLD.        * 00029501
*                                                                     * 00029601
* INSTALLATION INSTRUCTIONS                                           * 00029701
*                                                                     * 00029801
*     THIS MODULE MAY BE ASSEMBLED AND LINKED INTO A LIBRARY IN THE   * 00029901
*     LINK LIST OR SPECIFIED AS A STEPLIB IN THE TSO LOGON PROCEDURE. * 00030001
*     THE COMMAND MAY BE INVOKED FROM TSO BY ENTERING THE NAME OR AN  * 00030101
*     ALIAS OF THE MODULE.                                            * 00030201
*                                                                     * 00030301
*     BEFORE ASSEMBLING THE MODULE, ENSURE THAT ALL THE REQUIRED      * 00030401
*     MACROS ARE AVAILABLE.  SOME MACROS FROM SYS1.AMODGEN MAY BE     * 00030501
*     REQUIRED.  THE $ENTER MACRO USES SYSTEM VARIABLE SYMBOLS        * 00030601
*     &SYSDATE AND &SYSTIME.  THESE SYMBOLS ARE NOT SUPPORTED BY ALL  * 00030701
*     ASSEMBLERS.  IF THE ASSEMBLER TO BE USED DOES NOT SUPPORT THESE * 00030801
*     SYMBOLS, THE $ENTER MACRO MUST BE MODIFIED.                     * 00030901
*                                                                     * 00031001
*     THE MODULE MAY BE LINKED WITH ATTRIBUTES RENT AND REFR AND IS   * 00031101
*     ELIGIBLE FOR INCLUSION IN THE LINK PACK AREA.                   * 00031201
*                                                                     * 00031301
*********************************************************************** 00031401
*********************************************************************** 00031501
*                                                                     * 00031601
*  SS0802         ALIAS DSAT                                          * 00031701
*                                                                     * 00031801
*  FUNCTION       THE DSAT COMMAND IS USED TO DISPLAY ALLOCATION      * 00031901
*                 INFORMATION FOR DATA SETS ON A DIRECT ACCESS        * 00032001
*                 DEVICE.                                             * 00032101
*                                                                     * 00032201
*  DESCRIPTION    DSAT WILL SEARCH THE OS CATALOG AND CVOLS FOR THE   * 00032301
*                 ENTRIES FOR THE DATA SETS SPECIFIED.  ALLOCATION    * 00032401
*                 INFORMATION WILL BE OBTAINED FROM THE VOLUME TABLE  * 00032501
*                 OF CONTENTS, FORMATTED AND DISPLAYED.  IF A NAME IS * 00032601
*                 AN INDEX NAME, ALL DATA SETS BELOW THE INDEX WILL   * 00032701
*                 BE DISPLAYED.                                       * 00032801
*                                                                     * 00032901
*                 THE USER MAY BYPASS THE CATALOG SEARCH BY SUPPLYING * 00033001
*                 THE VOLUME SERIAL ON WHICH THE DATA SET RESIDES.    * 00033101
*                 THIS OPTION PERMITS DISPLAYING INFORMATION FOR      * 00033201
*                 UNCATALOGED DATA SETS.                              * 00033301
*                                                                     * 00033401
*                 THE ATTRIBUTES TO BE DISPLAYED MAY BE SELECTED BY   * 00033501
*                 THE USER WHEN ENTERING THE DSAT COMMAND AND         * 00033601
*                 SPECIFYING KEYWORD OPERANDS.                        * 00033701
*                                                                     * 00033801
*                 THE DSAT COMMAND MAY BE USED IN COMMAND PROCEDURES  * 00033901
*                 TO FIND THE ALLOCATION OF A DATA SET OR A GROUP OF  * 00034001
*                 DATA SETS AND SET THE RETURN CODE TO THE SPECIFIED  * 00034101
*                 VALUE.  THE RETURN CODE MAY THEN BE TESTED WITH THE * 00034201
*                 WHEN COMMAND.  OUTPUT MAY BE SUPPRESSED BY          * 00034301
*                 SPECIFYING NOPRINT.                                 * 00034401
*                                                                     * 00034501
*                 THE USER MAY CHOOSE WHAT INFORMATION WILL BE        * 00034601
*                 DISPLAYED BY ENTERING KEYWORDS.                     * 00034701
*                                                                     * 00034801
*                 THE INFORMATION THAT MAY BE DISPLAYED IS:           * 00034901
*                  1. VOLUME SERIAL ON WHICH THE DATA SET IS LOCATED. * 00035001
*                  2. FILE SEQUENCE NUMBER.                           * 00035101
*                  3. DEVICE TYPE CODE FROM CATALOG ENTRY.            * 00035201
*                  4. ALLOCATION  (ALLOCATED, USED, AND EXTENTS).     * 00035301
*                  5. SECONDARY ALLOCATION (AMOUNT AND UNITS).        * 00035401
*                  6. DATA SET ORGANIZATION.                          * 00035501
*                  7. DCB (RECFM, LRECL, AND BLKSIZE).          GP09143 00035601
*                  8. CREATION DATE.                                  * 00035701
*                  9. EXPIRATION DATE.                                * 00035801
*                 10. FULLY QUALIFIED DATA SET NAME.                  * 00035901
*                 11. CCHHR OF THE FORMAT 1 DSCB.                     * 00036001
*                 12. GENERATION DATA GROUP DATA.                     * 00036101
*                 13. PDS DIRECTORY INFORMATION.                      * 00036201
*                                                                     * 00036301
*  SYNTAX         DSAT (NAME-LIST)  SERIAL/NOSERIAL   ALLOC/NOALLOC   * 00036401
*                                   DSORG/NODSORG     DCB/NODCB       * 00036501
*                                   CRDATE/NOCRDATE   EXDATE/NOEXDATE * 00036601
*                                   LASTREF/NOLASTREF                   00036701
*                                   TOTALS/NOTOTALS   PRINT/NOPRINT   * 00036801
*                                   HEADER/NOHEADER   ALL/DAONLY      * 00036901
*                                   GDGDATA/NOGDGDATA SEQNO/NOSEQNO   * 00037001
*                                   DEVTYPE/NODEVTYPE DEVICE/NODEVICE * 00037101
*                                   CCHHR/NOCCHHR     PDS/NOPDS       * 00037201
*                                   SECONDARY/NOSECONDARY             * 00037301
*                                   GENERIC           DSONLY          * 00037401
*                                   YMD/MDY/DMY                       * 00037501
*                                   HARDCOPY(DDNAME)  VOLUME(SERIAL)  * 00037601
*                                   RC(TALLOC/TUSED/TDIFF/            * 00037701
*                                      LALLOC/LUSED/LDIFF/NUM/        * 00037801
*                                      PREVIOUS/DSORG/DIRALLOC/       * 00037901
*                                      DIRUSED/ENTRIES/ALIASES/       * 00038001
*                                      MEMBERS)                       * 00038101
*                                                                     * 00038201
*                 DEFAULTS -  NAME-LIST DEFAULTS TO PREFIX            * 00038301
*                             SERIAL   ALLOC   DSORG    DCB    CRDATE * 00038401
*                             NOEXDATE TOTALS  PRINT    HEADER ALL    * 00038501
*                             NOPDS    NOSEQNO NODEVICE NODEVTYPE     * 00038601
*                             NOGDGDATA    NOLASTREF    NOSECONDARY   * 00038701
*                                                                     * 00038801
*  OPERANDS       NAME-LIST - ONE OR MORE DATA SET OR INDEX LEVEL     * 00038901
*                             NAMES. TSO NAMING CONVENTIONS ARE USED. * 00039001
*                             IF A SPECIFIED NAME IS AN INDEX NAME,   * 00039101
*                             DATA SETS UNDER THAT LEVEL WILL BE      * 00039201
*                             DISPLAYED.  IF NO NAME IS SPECIFIED,    * 00039301
*                             THE PREFIX WILL BE USED.  FOR MVT AND   * 00039401
*                             SVS, THIS IS THE USERID.  FOR MVS,      * 00039501
*                             THE PREFIX MAY BE SPECIFIED WITH THE    * 00039601
*                             PROFILE COMMAND.  IF NO NAME IS         * 00039701
*                             SPECIFIED AND THE PROFILE SPECIFIES     * 00039801
*                             NOPREFIX, THE USERID WILL BE USED.      * 00039901
*                                                                     * 00040001
*                             NOTE - IF A KEYWORD IS ENTERED,         * 00040101
*                                    NAME-LIST MUST BE SPECIFIED.  IF * 00040201
*                                    NAME-LIST IS OMITTED, IT WILL    * 00040301
*                                    NOT DEFAULT TO THE PREFIX, BUT   * 00040401
*                                    THE KEYWORD WILL BE INTERPRETED  * 00040501
*                                    AS THE NAME-LIST, NOT A KEYWORD. * 00040601
*                                                                     * 00040701
*                 SERIAL/NOSERIAL                                     * 00040801
*                             SPECIFIES WHETHER OR NOT THE VOLUME     * 00040901
*                             SERIAL IS TO BE DISPLAYED FOR EACH DATA * 00041001
*                             SET.                                    * 00041101
*                                                                     * 00041201
*                 SEQNO/NOSEQNO                                       * 00041301
*                             SPECIFIES WHETHER OR NOT THE FILE       * 00041401
*                             SEQUENCE NUMBER IS TO BE DISPLAYED.     * 00041501
*                             IF NOSERIAL IS SPECIFIED, THE FILE      * 00041601
*                             SEQUENCE NUMBER WILL NOT BE DISPLAYED.  * 00041701
*                                                                     * 00041801
*                 DEVTYPE/NODEVTYPE                                   * 00041901
*                             SPECIFIES WHETHER OR NOT THE DEVICE     * 00042001
*                             CODE FROM THE CATALOG ENTRY IS TO BE    * 00042101
*                             DISPLAYED.  IF VOLUME IS SPECIFIED,     * 00042201
*                             THE DEVICE TYPE IS OBTAINED FROM THE    * 00042301
*                             UCB.                                    * 00042401
*                                                                     * 00042501
*                 DEVICE/NODEVICE                                     * 00042601
*                             SPECIFIES WHETHER OR NOT THE NAME OF    * 00042701
*                             THE DEVICE (3330, 2314, ETC.) IS TO     * 00042801
*                             BE DISPLAYED.  THIS VALUE IS ONLY       * 00042901
*                             RECOGNIZED FOR DIRECT ACCESS DEVICES.   * 00043001
*                                                                     * 00043101
*                 ALLOC/NOALLOC                                       * 00043201
*                             SPECIFIES WHETHER OR NOT ALLOCATION     * 00043301
*                             INFORMATION (TRACKS ALLOCATED, TRACKS   * 00043401
*                             USED, AND NUMBER OF EXTENTS) IS TO BE   * 00043501
*                             DISPLAYED.                              * 00043601
*                                                                     * 00043701
*                 SECONDARY/NOSECONDARY                               * 00043801
*                             SPECIFIES WHETHER OR NOT SECONDARY      * 00043901
*                             ALLOCATION INFORMATION (AMOUNT OF       * 00044001
*                             SECONDARY SPACE AND UNITS) IS TO BE     * 00044101
*                             DISPLAYED.  IF NOALLOC IS SPECIFIED,    * 00044201
*                             NO SECONDARY ALLOCATION INFORMATION     * 00044301
*                             WILL BE DISPLAYED.                      * 00044401
*                                                                     * 00044501
*                 DSORG/NODSORG                                       * 00044601
*                             SPECIFIES WHETHER OR NOT DATA SET       * 00044701
*                             ORGANIZATION IS TO BE DISPLAYED.        * 00044801
*                                                                     * 00044901
*                 PDS/NOPDS                                           * 00045001
*                             SPECIFIES WHETHER OR NOT PDS DIRECTORY  * 00045101
*                             INFORMATION IS DISPLAYED.  SELECTION    * 00045201
*                             OF THE PDS OPTION WILL RESULT IN        * 00045301
*                             DYNAMIC ALLOCATION/DEALLOCATION AND     * 00045401
*                             READING OF THE PDS.  THESE OPERATIONS   * 00045501
*                             WILL RESULT IN PERFORMANCE DEGRADATION. * 00045601
*                                                                     * 00045701
*                             THE MESSAGE "DATA SET IN USE" INDICATES * 00045801
*                             ANOTHER USER HAD EXCLUSIVE CONTROL OF   * 00045901
*                             THE DATA SET AND DSAT COULD NOT         * 00046001
*                             ALLOCATE IT.                            * 00046101
*                                                                     * 00046201
*                             IF AN ERROR OCCURS DURING PROCESSING OF * 00046301
*                             THE PDS DIRECTORY, AN ERROR MESSAGE     * 00046401
*                             WILL APPEAR IN THE PDS DATA FIELDS.     * 00046501
*                               RC =   RETURN CODE FROM DAIR          * 00046601
*                               DARC = DYNAMIC ALLOCATION RETURN CODE * 00046701
*                               CTRC = CATALOG RETURN CODE            * 00046801
*                                                                     * 00046901
*                 DCB/NODCB                                           * 00047001
*                             SPECIFIES WHETHER OR NOT DCB INFORMATION* 00047101
*                             (RECFM, BLKSIZE, AND LRECL) IS TO BE    * 00047201
*                             DISPLAYED.                              * 00047301
*                                                                     * 00047401
*                 CRDATE/NOCRDATE                                     * 00047501
*                             SPECIFIES WHETHER OR NOT THE CREATION   * 00047601
*                             DATE IS TO BE DISPLAYED.                * 00047701
*                                                                     * 00047801
*                 EXDATE/NOEXDATE                                     * 00047901
*                             SPECIFIES WHETHER OR NOT THE EXPIRATION * 00048001
*                             DATE IS TO BE DISPLAYED.                * 00048101
*                                                                     * 00048201
*                 LASTREF/NOLASTREF                                   * 00048301
*                             SPECIFIES WHETHER OR NOT THE LAST DATE  * 00048401
*                             REFERENCED IS TO BE DISPLAYED.  MVS     * 00048501
*                             SU 60 REQUIRED.                         * 00048601
*                                                                     * 00048701
*                 YMD/MDY/DMY                                         * 00048801
*                             SPECIFIES THE FORMAT IN WHICH THE       * 00048901
*                             ABOVE DATES ARE DISPLAYED.              * 00049001
*                                                                     * 00049101
*                 CCHHR/NOCCHHR                                       * 00049201
*                             SPECIFIES WHETHER OR NOT THE CCHHR OF   * 00049301
*                             THE FORMAT 1 DSCB IS TO BE DISPLAYED.   * 00049401
*                                                                     * 00049501
*                 TOTALS/NOTOTALS                                     * 00049601
*                             SPECIFIES WHETHER OR NOT ALLOCATION     * 00049701
*                             AND NUMBER OF DATA SET ENTRIES DISPLAYED* 00049801
*                             TOTALS ARE TO BE DISPLAYED.  IF NOALLOC * 00049901
*                             IS SPECIFIED, TOTALS ARE NOT DISPLAYED. * 00050001
*                                                                     * 00050101
*                 PRINT/NOPRINT                                       * 00050201
*                             SPECIFIES WHETHER OR NOT OUTPUT IS TO BE* 00050301
*                             SUPPRESSED.  THIS IS INTENDED FOR USE IN* 00050401
*                             COMMAND PROCEDURES WHERE DSAT IS BEING  * 00050501
*                             USED TO SET THE RETURN CODE.  ERROR     * 00050601
*                             MESSAGES ARE NOT SUPPRESSED.            * 00050701
*                                                                     * 00050801
*                 HEADER/NOHEADER                                     * 00050901
*                             SPECIFIES WHETHER OR NOT THE HEADER     * 00051001
*                             LINE IS TO BE SUPPRESSED.               * 00051101
*                                                                     * 00051201
*                 ALL/DAONLY                                          * 00051301
*                             SPECIFIES WHETHER OR NOT DATA SETS      * 00051401
*                             ON NON-DIRECT ACCESS DEVICES OR         * 00051501
*                             ON UNMOUNTED DEVICES ARE TO BE          * 00051601
*                             DISPLAYED.                              * 00051701
*                                                                     * 00051801
*                 GDGDATA/NOGDGDATA                                   * 00051901
*                             SPECIFIES WHETHER OR NOT GENERATION     * 00052001
*                             DATA GROUP DATA IS TO BE DISPLAYED.     * 00052101
*                             IF GDGDATA IS SPECIFIED, THE            * 00052201
*                             INFORMATION DISPLAYED IS -              * 00052301
*                               OPTIONS - OPTIONS SPECIFIED WHEN THE  * 00052401
*                                         INDEX WAS BUILT.            * 00052501
*                                         E INDICATES EMPTY,          * 00052601
*                                         D INDICATES DELETE.         * 00052701
*                               CURRENT - NUMBER OF GENERATION DATA   * 00052801
*                                         SETS CURRENTLY CATALOGED    * 00052901
*                                         UNDER THE INDEX.            * 00053001
*                               MAX     - MAXIMUM NUMBER OF ENTRIES   * 00053101
*                                         WHICH MAY EXIST AT ONE      * 00053201
*                                         TIME.                       * 00053301
*                                                                     * 00053401
*                             NOTE: THIS OPTION IS INOPERATIVE IN     * 00053501
*                                   MVS IF THE DATA MANAGMENT SU (8)  * 00053601
*                                   IS NOT INSTALLED.  THIS OPERAND   * 00053701
*                                   IS ALSO INOPERATIVE FOR           * 00053801
*                                   GENERATION INDICES IN VSAM        * 00053901
*                                   CATALOGS.  IF THE KEYWORD IS      * 00054001
*                                   ENTERED, IT WILL BE IGNORED.  NO  * 00054101
*                                   ERROR MESSAGE WILL BE GENERATED.  * 00054201
*                                                                     * 00054301
*                 GENERIC     SPECIFIES THAT THE NAME SPECIFIED IS    * 00054401
*                             TO BE USED AS A GENERIC KEY TO LOCATE   * 00054501
*                             DATA SET NAMES.  ALL DATA SETS WHICH    * 00054601
*                             MATCH THE KEY WILL BE DISPLAYED.  THE   * 00054701
*                             NAME NEED NOT SPECIFY AN INDEX LEVEL.   * 00054801
*                             THIS OPTION IS VALID FOR MVS ONLY       * 00054901
*                             AND ENTRIES IN A VSAM CATALOG.          * 00055001
*                                                                     * 00055101
*                             GENERIC IS IGNORED IF DSONLY IS         * 00055201
*                             SPECIFIED.                              * 00055301
*                                                                     * 00055401
*                 DSONLY      SPECIFIES THAT NAME-LIST ENTRIES        * 00055501
*                             REPRESENT DATA SET NAMES ONLY.  IF      * 00055601
*                             A NAME SPECIFIED IS FOUND TO BE AN      * 00055701
*                             INDEX, DSAT WILL TREAT IT AS NOT        * 00055801
*                             FOUND.                                  * 00055901
*                                                                     * 00056001
*                 VOLUME      SPECIFIES THE VOLUME SERIAL NUMBER      * 00056101
*                             OF THE VOLUME TO BE SEARCHED FOR        * 00056201
*                             THE SPECIFIED DATA SETS.  DSAT WILL     * 00056301
*                             ASSUME THAT THE NAME-LIST CONSISTS      * 00056401
*                             OF DATA SET NAMES, NOT INDEX NAMES.     * 00056501
*                                                                     * 00056601
*                             Unable to get the above working with    * 00056701
*                             VOLUME(*), so changed logic as follows: * 00056801
*                             1) Request without VOLUME() shows all   * 00056901
*                                information that matches.            * 00057001
*                             2) Request with VOLUME(*) shows all     * 00057101
*                                information associated with a serial * 00057201
*                             3) Request for specific VOLUME returns  * 00057301
*                                information for that volume only.    * 00057401
*                                               G. Postpischil  GP09144 00057501
*                                                                     * 00057601
*                 HARDCOPY(DDNAME)                                    * 00057701
*                             SPECIFIES THAT A HARD COPY LISTING      * 00057801
*                             OF THE DSAT OUTPUT IS DESIRED.          * 00057901
*                             DDNAME SPECIFIES THE DDNAME OF A        * 00058001
*                             PRE-ALLOCATED DATA SET WHERE THE        * 00058101
*                             OUTPUT WILL BE WRITTEN.  THE DATA SET   * 00058201
*                             ATTRIBUTES WILL BE                      * 00058301
*                                RECFM=FB,LRECL=120,BLKSIZE=3000      * 00058401
*                                                                     * 00058501
*                 RC                                                  * 00058601
*                             SPECIFIES THE RETURN CODE SETTING.  IF  * 00058701
*                             RC IS NOT SPECIFIED, THE RETURN CODE    * 00058801
*                             WILL BE SET TO:                         * 00058901
*                              4 - SUCCESSFUL COMPLETION.             * 00059001
*                             16 - UNABLE TO INTERPRET COMMAND.       * 00059101
*                                                                     * 00059201
*                             THE RC OPERAND PERMITS THE USER TO      * 00059301
*                             SPECIFY THAT THE RETURN CODE IS TO BE   * 00059401
*                             SET TO A VALUE DEPENDING ON THE         * 00059501
*                             ALLOCATION OF THE DATA SETS.            * 00059601
*                             TALLOC   - TOTAL TRACKS ALLOCATED       * 00059701
*                             TUSED    - TOTAL TRACKS USED            * 00059801
*                             TDIFF    - TOTAL OVERALLOCATION         * 00059901
*                                        (TALLOC-TUSED)               * 00060001
*                             TPCTUSE  - PERCENT OF TOTAL USED        * 00060101
*                             LALLOC   - TRACKS ALLOCATED BY LAST     * 00060201
*                                        DATA SET                     * 00060301
*                             LUSED    - TRACKS USED BY LAST DATA SET * 00060401
*                             LDIFF    - LAST DATA SET OVERALLOCATION * 00060501
*                                        (LALLOC-LUSED)               * 00060601
*                             LPCTUSE  - PERCENT OF LAST DS TRACK USE * 00060701
*                             NUM      - NUMBER OF DATA SETS DISPLAYED* 00060801
*                                        (INCLUDES DATA SETS NOT      * 00060901
*                                        FOUND, ETC.)                 * 00061001
*                             PREVIOUS - ASSUMES THE RETURN CODE OF   * 00061101
*                                        THE PREVIOUS COMMAND.        * 00061201
*                             DSORG    - SETS THE RETURN CODE BASED   * 00061301
*                                        ON THE DATA SET ORGANIZATION * 00061401
*                                        OF THE DATA SETS DISPLAYED.  * 00061501
*                                        1 - ALL DATA SETS WERE SEQ.  * 00061601
*                                        2 - ALL DATA SETS WERE PDS   * 00061701
*                                        3 - DATA SETS WERE MIXED PDS * 00061801
*                                            AND SEQUENTIAL           * 00061901
*                                        4 - AN ERROR OCCURRED OR A   * 00062001
*                                            DATA SET OTHER THAN PDS  * 00062101
*                                            OR SEQUENTIAL WAS        * 00062201
*                                            PROCESSED                * 00062301
*                             DIRALLOC - SETS THE RETURN CODE TO THE  * 00062401
*                                        NUMBER OF DIRECTORY BLOCKS   * 00062501
*                                        ALLOCATED.                   * 00062601
*                             DIRUSED  - SETS THE RETURN CODE TO THE  * 00062701
*                                        NUMBER OF DIRECTORY BLOCKS   * 00062801
*                                        USED.                        * 00062901
*                             ENTRIES  - SETS THE RETURN CODE TO THE  * 00063001
*                                        NUMBER OF ENTRIES IN THE     * 00063101
*                                        DIRECTORY (MEMBERS AND       * 00063201
*                                        ALIASES).                    * 00063301
*                             ALIASES  - SETS THE RETURN CODE TO THE  * 00063401
*                                        NUMBER OF ALIASES.           * 00063501
*                             MEMBERS  - SETS THE RETURN CODE TO      * 00063601
*                                        ENTRIES-ALIASES.             * 00063701
*                                                                     * 00063801
*                                                                     * 00063901
*                             NOTE - IF NOALLOC IS SPECIFIED, THE     * 00064001
*                                    RETURN CODE FOR TALLOC, TUSED,   * 00064101
*                                    TDIFF, LALLOC, LUSED, AND LDIFF  * 00064201
*                                    WILL BE ZERO.                    * 00064301
*                                                                     * 00064401
*                                    IF NODSORG IS SPECIFIED, THE     * 00064501
*                                    RETURN CODE FOR DSORG WILL BE    * 00064601
*                                    ZERO.                            * 00064701
*                                                                     * 00064801
*                                    IF NOPDS IS SPECIFIED OR THE     * 00064901
*                                    LAST OR ONLY DATA SET IS NOT     * 00065001
*                                    A PDS, THE RETURN CODE FOR       * 00065101
*                                    DIRALLOC, DIRUSED, ENTRIES,      * 00065201
*                                    ALIASES, AND MEMBERS WILL BE     * 00065301
*                                    ZERO.                            * 00065401
*                                                                     * 00065501
*  SUBCOMMANDS    DSAT HAS NO SUBCOMMANDS.                            * 00065601
*                                                                     * 00065701
*  ATTRIBUTES     RENT, REUS, REFR, ENABLED, NON-PRIVILEGED           * 00065801
*                                                                     * 00065901
*  EXAMPLES       DISPLAY ALL DATA SETS UNDER THE TERMINAL USER'S ID. * 00066001
*                                                                     * 00066101
*                 DSAT                                                * 00066201
*                                                                     * 00066301
*                                                                     * 00066401
*                 DISPLAY ALL "FPL" DATA SETS.  DISPLAY EXPIRATION    * 00066501
*                 DATES.                                              * 00066601
*                                                                     * 00066701
*                 DSAT 'FPL' EXDATE                                   * 00066801
*                                                                     * 00066901
*                                                                     * 00067001
*                 IN A PROCEDURE, DETERMINE IF DATA SET SS.MACLIB     * 00067101
*                 HAS 20 TRACKS LEFT.  IF SO, TERMINATE.  IF NOT,     * 00067201
*                 COMPRESS IT.                                        * 00067301
*                                                                     * 00067401
*                 DSAT 'SS.MACLIB' RC(LDIFF) NOPRINT                  * 00067501
*                 WHEN SYSRC(GT 20) END                               * 00067601
*                 COMPRESS 'SS.MACLIB'                                * 00067701
*                 END                                                 * 00067801
*                                                                     * 00067901
*                                                                     * 00068001
*  FPL MACROS USED - NONE (REMOVED FOR HERCULES MVS 3.8)              * 00068101
*                                                                     * 00068201
*                                                                     * 00068301
*  IBM MACROS USED                                                    * 00068401
*                                                                     * 00068501
*        GETMAIN           OBTAIN WORKING STORAGE                     * 00068601
*        FREEMAIN          RELEASE WORKING STORAGE                    * 00068701
*        LINK              INVOKE SERVICE ROUTINE                     * 00068801
*        LOCATE            READ CATALOG                               * 00068901
*        OBTAIN            READ VTOC                                  * 00069001
*        CAMLST            CONSTRUCT PARM LIST FOR LOCATE AND OBTAIN  * 00069101
*        OPEN              OPEN PDS DIRECTORY                         * 00069201
*        CLOSE             CLOSE PDS DIRECTORY                        * 00069301
*        READ              READ PDS DIRECTORY                         * 00069401
*        CHECK             WAIT FOR COMPLETION OF READ                * 00069501
*        PUT               WRITE A RECORD                             * 00069601
*        DCB               DATA CONTROL BLOCK FOR PDS DIRECTORY       * 00069701
*        PUTLINE           OUTPUT A LINE TO TERMINAL                  * 00069801
*        IKJPARM           BEGIN PARSE PARAMETER CONTROL LIST         * 00069901
*        IKJIDENT          DESCRIBE POSITIONAL PARAMETER              * 00070001
*        IKJPOSIT          DESCRIBE POSITIONAL PARAMETER              * 00070101
*        IKJKEYWD          DESCRIBE A KEYWORD PARAMETER               * 00070201
*        IKJNAME           DESCRIBE A KEYWORD VALUE                   * 00070301
*        IKJSUBF           BEGIN A SUBFIELD DESCRIPTION               * 00070401
*        IKJENDP           END PARSE PARAMETER CONTROL LIST           * 00070501
*        IKJRLSA           RELEASE STORAGE OBTAINED BY PARSE          * 00070601
*        IKJECT            MAP ECT                                    * 00070701
*        IKJUPT            MAP UPT                                    * 00070801
*        IKJDAP08          MAP DYNAMIC ALLOCATION CONTROL BLOCK (ALC) * 00070901
*        IKJDAP18          MAY DYNAMIC ALLOCATION CONTROL BLOCK (FREE)* 00071001
*                                                                     * 00071101
*  TSO SERVICE ROUTINES USED                                          * 00071201
*                                                                     * 00071301
*        IKJPARS           INTERPRET COMMAND OPERANDS                 * 00071401
*        PUTLINE           DISPLAY OUTPUT MESSAGES                    * 00071501
*        IKJDAIR           DYNAMIC ALLOCATION                         * 00071601
*                                                                     * 00071701
*  REGISTER ASSIGNMENTS                                               * 00071801
*                                                                     * 00071901
*        R0  - LINKAGE REGISTER/TEMPORARY WORK REGISTER               * 00072001
*        R1  - LINKAGE REGISTER/TEMPORARY WORK REGISTER               * 00072101
*        R2  - WORK REGISTER/OUTPUT AREA POINTER                      * 00072201
*        R3  - WORK REGISTER/DSNAME PDL POINTER                       * 00072301
*        R4  - WORK REGISTER                                          * 00072401
*        R5  - WORK REGISTER                                          * 00072501
*        R6  - WORK REGISTER                                          * 00072601
*        R7  - WORK REGISTER (SLOCATE ENTRY)                          * 00072701
*        R8  - WORK REGISTER                                          * 00072801
*        R9  - BASE REGISTER FOR PCL AND PDL                          * 00072901
*        R10 - BASE REGISTER FOR SUBROUTINE/STATIC DATA AREA          * 00073001
*        R11 - PROGRAM BASE REGISTER 1                                * 00073101
*        R12 - PROGRAM BASE REGISTER 2                                * 00073201
*        R13 - WORK AREA BASE REGISTER (SAVE AREA FIRST 18 WORDS)     * 00073301
*        R14 - LINKAGE REGISTER                                       * 00073401
*        R15 - LINKAGE REGISTER/RETURN CODE                           * 00073501
*                                                                     * 00073601
*  FOR FURTHER INFORMATION ON THE WORK REGISTERS, SEE INDIVDUAL       * 00073701
*  ROUTINES.                                                          * 00073801
*                                                                     * 00073901
*  THIS VERSION OF DSAT WILL WORK ON MVT, SVS, OR MVS IF ASSEMBLED    * 00074001
*  WITH THE PROPER SYSTEM NAME IN THE SETC STATEMENT AT THE           * 00074101
*  BEGINNING OF THIS MODULE.                                          * 00074201
*                                                                     * 00074301
*  THE ORIGINAL MVS MODIFICATIONS WERE MADE BY DAVID JOHN FLYNN OF    * 00074401
*  PRATT & WHITNEY AIRCRAFT.                                          * 00074501
*                                                                     * 00074601
*                                                GORDON P. WEST 09/76 * 00074701
*********************************************************************** 00074801
*********************************************************************** 00074901
*                                                                     * 00075001
*  UPDATE LOG                                                         * 00075101
*                                                                     * 00075201
*  09/24/76  MVS EXPANSION MODIFIED TO REPLACE IKJEHCIR WITH          * 00075301
*            DIRECT INVOCATION OF SVC 26 WITH A CTGPL.  PERFORMANCE   * 00075401
*            ENHANCEMENT.                                             * 00075501
*                                                                     * 00075601
*            GENERIC OPERAND ADDED FOR MVS EXPANSION.  REPLACEMENT    * 00075701
*            OF IKJEHCIR IS A PREREQUISITE FOR THIS MODIFICATION.     * 00075801
*                                                                     * 00075901
*  11/16/76  CCHHR OPTION ADDED.                                      * 00076001
*                                                                     * 00076101
*  01/14/77  SUPPORT FOR MVS WITH DATA MANAGMENT SU (8) ADDED.        * 00076201
*            IKJEHCIR REMOVED FROM MVT AND SVS EXPANSION.             * 00076301
*            DEVTYPE OPTION ADDED.                                    * 00076401
*                                                                     * 00076501
*  04/25/77  DSONLY OPTION ADDED.                                     * 00076601
*                                                                     * 00076701
*  05/13/77  ENTRY TYPE ADDED TO DSORG FOR VSAM CATALOG ENTRIES.      * 00076801
*                                                                     * 00076901
*  05/31/77  SUPPRESS DATA SET NOT FOUND MESSAGES FOR VOLUME(*).      * 00077001
*                                                                     * 00077101
*  07/25/77  ADD TEST FOR NOT READY TO UCB LOOKUP ROUTINES.           * 00077201
*                                                                     * 00077301
*  08/10/77  ADD PDS OPTION TO OBTAIN DIRECTORY INFORMATION.          * 00077401
*            ADD DEVICE OPTION TO DISPLAY DEVICE NAME (3330, ETC.)    * 00077501
*            OBTAIN DEVTYPE FROM UCB IF VOLUME SPECIFIED.             * 00077601
*                                                                     * 00077701
*  08/31/77  ADD PUNCH STATEMENT FOR ALIAS STATEMENT.                 * 00077801
*            CHECK FOR ZERO OR NEGATIVE VOLUME COUNT.                 * 00077901
*                                                                     * 00078001
*  09/28/77  ADD MORE INFORMATION ON ERRORS ENCOUNTERED DURING        * 00078101
*                 PDS PROCESSING.                                     * 00078201
*                                                                     * 00078301
*  03/10/78  ADD HARDCOPY OPTION.                                     * 00078401
*            PROVIDE DEVTYPE FROM CATALOG FOR NON-DASD DEVICES.       * 00078501
*            TEST FOR NO PREFIX, NO DSNAME, NO USERID                 * 00078601
*                                                                     * 00078701
*  03/20/78  LAST DATE REFERENCED ADDED - MVS SU 60 (OR EQUIVALENT    * 00078801
*                 REQUIRED).                                          * 00078901
*                                                                     * 00079001
*  09/25/08  ELIMINATED LOOP UNDER MVS 3.8; REWORKED.                 * 00079101
*            DISPLAY CATALOG NAME FOR VSAM SUPERLOCATE.               * 00079201
*            FIXED OTHER ERRORS.             G. POSTPISCHIL           * 00079301
*                                                                     * 00079401
*  05/nn/09  Fixed lots of errors not previously found (VOLUME, VBS   * 00079501
*            LRECL, space, non-non-VSAM catalog data, etc.)     GP09143 00079601
*            Distinct CSECTs to catch bad branches; addressability.   * 00079701
*            Rearranged code to facilitate GDS processing; fixed      * 00079801
*            incorrect track counts for multi-volume DS.        GP09143 00079901
*                                                                     * 00080001
*********************************************************************** 00080101
*********************************************************************** 00080201
*                                                                     * 00080301
*        THE FOLLOWING PUNCH STATEMENT(S) CAUSE LINKAGE EDITOR        * 00080401
*        CONTROL STATEMENTS TO BE INCLUDED IN THE OBJECT OUTPUT.      * 00080501
*                                                                     * 00080601
*********************************************************************** 00080701
*                                                                       00080801
         PUNCH '    ORDER DSAT(P)  &SYSDATE &SYSTIME'           GP08269 00080901
         PRINT NOGEN         SAVE A TREE                        GP08269 00081001
DSAT     START 0                                                        00081101
*********************************************************************** 00081201
*                                                                     * 00081301
*                      REGISTER EQUIVALENCES                          * 00081401
*                                                                     * 00081501
*********************************************************************** 00081601
         YREGS ,                                                GP08269 00081701
         B     BEGIN-DSAT(,R15)         BRANCH AROUND ID        GP08269 00081801
         DC    AL1(23),CL23'DSAT     &SYSDATE &SYSTIME'         GP08269 00081901
BEGIN    STM   R14,R12,12(R13)          SAVE REGISTERS          GP08269 00082001
         LR    R11,R15                  LOAD BASE ADDRESS               00082101
         LA    R12,4095(,R11)           ADD 4095 TO LAST BASE           00082201
         LA    R12,1(,R12)              ADD 1 MORE                      00082301
         USING DSAT,R11,R12             DEFINE BASE REGISTERS           00082401
         LA    R15,15                   LOAD PGM MASK SETTING           00082501
         SLA   R15,24                   SHIFT TO BITS 4-7               00082601
         SPM   R15                      SET PGM MASK AND COND           00082701
         GETMAIN RC,LV=DSECTLEN,SP=1,BNDRY=PAGE                 GP08269 00082801
         LR    R15,R13                  SAVE OLD SAVEAREA ADDR          00082901
         LR    R13,R1                   LOAD STORAGE ADDRESS            00083001
         USING DSATSAVE,R13             DEFINE BASE REGISTER            00083101
         L     R1,24(,R15)              RESTORE REG 1 CONTENTS          00083201
         ST    R15,4(,R13)              CHAIN SAVE AREAS                00083301
         ST    R13,8(,R15)                                              00083401
         L     R10,=A(DSATDATA)       LOAD COMMON DATA SECTION  GP09143 00083501
         USING DSATDATA,R10                                             00083601
*********************************************************************** 00083701
*                                                                     * 00083801
*        CONSTRUCT TSO CONTROL BLOCKS                                 * 00083901
*                                                                     * 00084001
*********************************************************************** 00084101
*, IKJCPPL                                                              00084201
*********************************************************************** 00084301
*    THE COMMAND PROCESSOR PARAMETER LIST (CPPL) IS A LIST OF         * 00084401
*    ADDRESSES PASSED FROM THE TMP TO THE CP VIA REGISTER 1           * 00084501
*********************************************************************** 00084601
CPPL     DSECT                                                          00084701
CPPLCBUF DS    A        PTR TO COMMAND BUFFER                           00084801
CPPLUPT  DS    A        PTR TO UPT                                      00084901
CPPLPSCB DS    A        PTR TO PSCB                                     00085001
CPPLECT  DS    A        PTR TO ECT                                      00085101
**/                                                                     00085201
* END IKJCPPL - #TSOBLKS                                                00085301
*, IKJIOPL                                                              00085401
IOPL     DSECT                                                          00085501
*********************************************************************** 00085601
*    THE I/O SERVICE ROUTINE PARAMETER LIST (IOPL) IS A LIST OF       * 00085701
*    FULLWORD ADDRESSES PASSED BY THE INVOKER OF ANY I/O SERVICE      * 00085801
*    ROUTINE TO THE APPROPRIATE SERVICE ROUTINE VIA REGISTER ONE.     * 00085901
*********************************************************************** 00086001
IOPLUPT  DS    A        PTR TO UPT                                      00086101
IOPLECT  DS    A        PTR TO ECT                                      00086201
IOPLECB  DS    A        PTR TO USER'S ECB                               00086301
IOPLIOPB DS    A        PTR TO THE I/O SERVICE RTN PARM BLOCK           00086401
**/                                                                     00086501
* END IKJIOPL - #TSOBLKS                                                00086601
IOPLLEN  EQU   *-IOPL                                                   00086701
*, IKJPPL                                                               00086801
PPL      DSECT                                                          00086901
*********************************************************************** 00087001
*    THE PARSE PARAMETER LIST (PPL) IS A LIST OF ADDRESSES PASSED     * 00087101
*    FROM THE INVOKER TO PARSE VIA REGISTER 1                         * 00087201
*********************************************************************** 00087301
PPLUPT   DS    A        PTR TO UPT                                      00087401
PPLECT   DS    A        PTR TO ECT                                      00087501
PPLECB   DS    A        PTR TO CP'S ECB                                 00087601
PPLPCL   DS    A        PTR TO PCL                                      00087701
PPLANS   DS    A        PTR TO ANS PLACE                                00087801
PPLCBUF  DS    A        PTR TO CMD BUFFER                               00087901
PPLUWA   DS    A        PTR TO USER WORK AREA (FOR VALIDITY CK RTNS)    00088001
**/                                                                     00088101
* END IKJPPL  - #TSOBLKS                                                00088201
PPLLEN   EQU   *-PPL                                                    00088301
*, IKJDAPL                                                              00088401
*********************************************************************** 00088501
*    THE DYNAMIC ALLOCATION INTERFACE ROUTINE (DAIR) PARAMETER LIST   * 00088601
*    (DAPL) IS A LIST OF ADDRESSES PASSED FROM THE INVOKER TO DAIR    * 00088701
*    VIA REGISTER 1                                                     00088801
*********************************************************************** 00088901
DAPL     DSECT                                                          00089001
DAPLUPT  DS    A        PTR TO UPT                                      00089101
DAPLECT  DS    A        PTR TO ECT                                      00089201
DAPLECB  DS    A        PTR TO CP'S ECB                                 00089301
DAPLPSCB DS    A        PTR TO PSCB                                     00089401
DAPLDAPB DS    A        PTR TO DAIR PARAMETER BLOCK                     00089501
**/                                                                     00089601
* END IKJDAPL - #TSOBLKS                                                00089701
DAPLLEN  EQU   *-DAPL                                                   00089801
DSAT     CSECT                          CONTINUE USER CSECT             00089901
*********************************************************************** 00090001
*                                                                     * 00090101
*                     SET UP TSO CONTROL BLOCKS                       * 00090201
*                                                                     * 00090301
*********************************************************************** 00090401
         XC    DSATECB(4),DSATECB       ZERO ECB                        00090501
         LR    R15,R1                   LOAD CPPL ADDRESS       GP09143 00090601
         USING CPPL,R15                 DEFINE CPPL BASE        GP09143 00090701
         ST    R15,MYCPPL               SAVE FOR QUICK RELOAD   GP09143 00090801
         LA    R8,DSATIOPL                                              00090901
         USING IOPL,R8                                                  00091001
         LA    R9,DSATPPL                                               00091101
         USING PPL,R9                                                   00091201
         LA    R7,DSATDAPL                                              00091301
         USING DAPL,R7                                                  00091401
*--------BUILD IOPL                                                     00091501
         MVC   IOPLUPT(4),CPPLUPT       COPY UPT ADDRESS                00091601
         MVC   IOPLECT(4),CPPLECT       COPY ECT ADDRESS                00091701
         LA    R1,DSATECB               GET ECB ADDRESS                 00091801
         ST    R1,IOPLECB                                               00091901
         LA    R1,DSATPTPB              GET IOPB ADDRESS                00092001
         ST    R1,IOPLIOPB                                              00092101
*--------BUILD PPL                                                      00092201
         MVC   PPLUPT(4),CPPLUPT        COPY UPT ADDRESS                00092301
         MVC   PPLECT(4),CPPLECT        COPY ECT ADDRESS                00092401
         LA    R1,DSATECB               GET ECB ADDRESS                 00092501
         ST    R1,PPLECB                                                00092601
         L     R1,=A(DSATPCL)           GET PCL ADDRESS         GP09144 00092701
         ST    R1,PPLPCL                                                00092801
         LA    R1,DSATANS               GET ANSWER AREA WORD ADDRESS    00092901
         ST    R1,PPLANS                                                00093001
         MVC   PPLCBUF(4),CPPLCBUF      COPY COMMAND BUFFER ADDRESS     00093101
*--------BUILD DAPL                                                     00093201
         MVC   DAPLUPT(4),CPPLUPT       COPY UPT ADDRESS                00093301
         MVC   DAPLECT(4),CPPLECT       COPY ECT ADDRESS                00093401
         LA    R1,DSATECB               GET ECB ADDRESS                 00093501
         ST    R1,DAPLECB                                               00093601
         MVC   DAPLPSCB(4),CPPLPSCB     COPY PSCB ADDRESS               00093701
         DROP  R7                                                       00093801
         MVC   DSATPTPB(ENDPTPB-MODLPTPB),MODLPTPB  INITIALIZE PTPB     00093901
         MVC   DSATHEDR(4),MODLINE                                      00094001
         MVI   FLAGS,X'00'             CLEAR FLAGS                      00094101
         MVI   FLAGS2,X'00'                                             00094201
         MVC   OUTLINE,BLANKS          BLANK OUTPUT LINE                00094301
         DROP  R8                      DROP IOPL BASE REGISTER          00094401
         DROP  R15                     DONE WITH CPPL           GP09143 00094501
*--------GET CATALOG WORKAREA                                           00094601
         L     R2,=F'32767'            LENGTH OF WORK AREA      GP08269 00094701
         GETMAIN RC,LV=(R2),SP=1,BNDRY=PAGE   OBTAIN WORK AREA  GP08269 00094801
         ST    R1,WORKAREA             SAVE ADDRESS OF WORK AREA        00094901
         ST    R2,WORKLEN              SAVE LENGTH OF WORK AREA         00095001
*********************************************************************** 00095101
*                                                                     * 00095201
*                  PARSE THE COMMAND                                  * 00095301
*                                                                     * 00095401
*********************************************************************** 00095501
         LA    R1,PPL                   LOAD PARSE PARAMETER LIST ADDR  00095601
         LINK  EP=IKJPARS               LINK TO PARSE ROUTINE           00095701
         LTR   R15,R15                  TEST RETURN CODE                00095801
         BNZ   ERRPARS                                                  00095901
         L     R9,DSATANS               LOAD ANSWER AREA ADDRESS        00096001
         USING DSATPDL,R9               DEFINE PDE BASE REGISTER        00096101
*********************************************************************** 00096201
*                                                                     * 00096301
*        SET UP CATALOG AND VTOC SEARCH CONTROL BLOCKS                * 00096401
*                                                                     * 00096501
*********************************************************************** 00096601
*--------SET UP CAMLISTS                                                00096701
         LM    R1,R4,SEARCH            OBTAIN, SEARCH                   00096801
         LA    R2,OBTNAME              OBTAIN DSN               GP09143 00096901
         LA    R3,DSATSER                                               00097001
         LA    R4,DSATFMT1                                              00097101
         STM   R1,R4,DSATDCB1                                           00097201
         LM    R1,R4,SEEK              OBTAIN, SEEK                     00097301
         LA    R2,DS1PTRDS                                              00097401
         LA    R3,DSATSER                                               00097501
         LA    R4,DSATFMT3                                              00097601
         STM   R1,R4,DSATDCB3                                           00097701
         LM    R1,R4,NAME              LOCATE, NAME                     00097801
         LA    R2,DSNAME                                                00097901
         LA    R4,CATBLOCK                                              00098001
         STM   R1,R4,LOCLIST                                            00098101
         LA    R2,OBTNAME              GDS NAME                 GP09143 00098201
         LA    R4,GDSSPACE             GDS VOLLIST              GP09143 00098301
         STM   R1,R4,GDSLIST                                    GP09143 00098401
         LM    R1,R4,NAMECVOL          LOCATE, NAME AND CVOL            00098501
         LA    R2,DSNAME                                                00098601
         LA    R3,CVOL                                                  00098701
         LA    R4,CATBLOCK                                              00098801
         STM   R1,R4,LOCLIST2                                           00098901
         LM    R1,R4,TTRLOC            LOCATE, TTR                      00099001
         LA    R2,TTR                                                   00099101
         LA    R3,CVOL                                                  00099201
         STM   R1,R4,LOCBYTTR                                           00099301
         XC    CTGPL(CTGPLLEN),CTGPL   ZERO OUT CTGPL                   00099401
         OI    CTGOPTN1,CTGNAME                                 GP08269 00099501
         OI    CTGOPTN2,CTGRCATN                                GP08269 00099601
         OI    CTGOPTN3,CTGSUPLT+CTGAM0                                 00099701
         L     R1,WORKAREA             SET UP WORK AREA ADDRESS         00099801
         ST    R1,CTGWKA                                                00099901
         LA    R1,DSNAME               SET UP SPECIFIC NAME     GP09143 00100001
         CLC   DSONLY,=H'1'            SEE IF DSONLY SPECIFIED  GP09127 00100101
         BE    ZEROTOT                 YES                      GP09127 00100201
         OI    CTGOPTN1,CTGGENLD       SET GENERIC LOCATE               00100301
         LA    R1,INDEX-1              SET UP SEARCH KEY ADDR   GP09143 00100401
ZEROTOT  ST    R1,CTGENT               SET MASK OR NAME ADDR    GP09144 00100501
         XC    TOTALRUN(5*4),TOTALRUN  ZERO TOTAL FIELDS        GP09143 00100601
         CLC   HARDCOPY,ZEROS          SEE IF HARDCOPY SPEC.    GP09127 00100701
         BE    SETUPHDR                                         GP09127 00100801
         OI    FLAGS2,$HARDCPY         TURN ON HARDCOPY INDICATOR       00100901
         MVC   HARDCPY(HARDLEN),HARDDCB     DCB AND OPEN CONTROL BLOCKS 00101001
         LA    R2,HARDCPY+40           LOAD DCBDDNAME ADDRESS           00101101
         L     R4,DDNAME               LOAD DDNAME OPERAND ADDRESS      00101201
         LH    R1,DDNAME+4             LOAD DDNAME OPERAND LENGTH       00101301
         BCTR  R1,0                    REDUCE LENGTH BY 1               00101401
         EX    R1,MOVENAME             MOVE DDNAME IN TO DCB            00101501
         LA    R1,HARDOPEN             LOAD OPEN LIST FORM ADDRESS      00101601
         LA    R2,HARDCPY              LOAD DCB ADDRESS                 00101701
         OPEN  ((R2),OUTPUT),MF=(E,(R1))    OPEN THE DATA SET           00101801
         TM    HARDCPY+48,X'10'        TEST FOR SUCCESSFUL OPEN         00101901
         BO    SETUPHDR                                                 00102001
         MVC   OUTLINE(48),DSATERR6    MOVE IN ERROR MESSAGE            00102101
         PUTLINE OUTPUT=(DSATHEDR,,,DATA),MF=(E,DSATIOPL)               00102201
         NI    FLAGS2,X'FF'-$HARDCPY   TURN OFF HARDCOPY INDICATOR      00102301
*********************************************************************** 00102401
*                                                                     * 00102501
*        SET UP HEADER LINE                                           * 00102601
*                                                                     * 00102701
*********************************************************************** 00102801
SETUPHDR LA    R2,OUTLINE              LOAD OUTPUT AREA ADDRESS         00102901
         MVC   OUTLINE,BLANKS          BLANK THE LINE                   00103001
         CLC   SERIAL,=H'2'            TEST FOR NOSERIAL                00103101
         BE    TRYDEV                                                   00103201
         ST    R2,SERFLD               SAVE OUTPUT FIELD ADDRESS        00103301
         MVC   0(6,R2),=C'SERIAL'      MOVE IN HEADER                   00103401
         LA    R2,7(,R2)               INCREMENT POINTER                00103501
         CLC   SEQNO,=H'2'             TEST FOR NOSEQNO                 00103601
         BE    TRYDEV                                                   00103701
         MVC   0(4,R2),=C' SEQ'        SET UP HEADER                    00103801
         LA    R2,5(,R2)               INCREMENT POINTER                00103901
TRYDEV   CLC   DEVTYPE,=H'2'           TEST FOR NODEVTYPE               00104001
         BE    TRYDEVIC                                                 00104101
         ST    R2,DEVFLD               SAVE OUTPUT FIELD ADDRESS        00104201
         MVC   0(8,R2),=C' DEVTYPE'    MOVE IN HEADER                   00104301
         LA    R2,9(,R2)               INCREMENT POINTER                00104401
TRYDEVIC CLC   DEVICE,=H'2'            TEST FOR NODEVICE                00104501
         BE    TRYALLOC                                                 00104601
         ST    R2,DEVFIELD             SAVE OUTPUT FIELD ADDRESS        00104701
         MVC   0(6,R2),=C'DEVICE'      MOVE IN HEADER                   00104801
         LA    R2,7(,R2)               INCREMENT POINTER                00104901
TRYALLOC ST    R2,ERRFLD               STORE ERROR FIELD ADDRESS        00105001
         CLC   ALLOC,=H'2'             TEST FOR NOALLOC                 00105101
         BE    TRYDSORG                                                 00105201
         ST    R2,ALLOCFLD             SAVE OUTPUT FIELD ADDRESS        00105301
         MVC   0(14,R2),=C'ALLOC  USED EX'  SET UP HEADER               00105401
         LA    R2,15(,R2)              INCREMENT POINTER                00105501
         CLC   SECOND,=H'2'            TEST FOR NOSECONDARY             00105601
         BE    TRYDSORG                                                 00105701
         MVC   0(10,R2),=C' SEC UNITS'      SET UP HEADER               00105801
         LA    R2,11(,R2)              INCREMENT POINTER                00105901
TRYDSORG CLC   DSORG,=H'2'             TEST FOR NODSORG                 00106001
         BE    TRYPDS                                                   00106101
         ST    R2,DSORGFLD             SAVE OUTPUT FIELD ADDRESS        00106201
         MVC   0(5,R2),=C'DSORG'       MOVE IN HEADER                   00106301
         LA    R2,6(,R2)               INCREMENT POINTER                00106401
TRYPDS   CLC   PDS,=H'2'               TEST FOR NOPDS                   00106501
         BE    TRYDCB                                                   00106601
         ST    R2,PDSFLD               SAVE OUTPUT FIELD ADDRESS        00106701
         MVC   0(19,R2),=C'DIRA DIRU ENTR  AL '                         00106801
         LA    R2,20(,R2)              INCREMENT POINTER                00106901
TRYDCB   CLC   DCB,=H'2'               TEST FOR NODCB                   00107001
         BE    TRYCRDAT                                                 00107101
         ST    R2,DCBFLD               SAVE OUTPUT FIELD ADDRESS        00107201
         MVC   0(16,R2),=C'-DCB ATTRIBUTES-'  MOVE IN HEADER            00107301
         LA    R2,17(,R2)              INCREMENT POINTER                00107401
TRYCRDAT CLC   CRDATE,=H'2'            TEST FOR NOCRDATE                00107501
         BE    TRYEXDAT                                                 00107601
         ST    R2,CRDATFLD             SAVE OUTPUT FIELD ADDRESS        00107701
         MVC   0(8,R2),=C'CR. DATE'    MOVE IN HEADER                   00107801
         LA    R2,9(,R2)               INCREMENT POINTER                00107901
TRYEXDAT CLC   EXDATE,=H'2'            TEST FOR NOEXDATE                00108001
         BE    TRYLREFD                                                 00108101
         ST    R2,EXDATFLD             SAVE OUTPUT FIELD ADDRESS        00108201
         MVC   0(8,R2),=C'EX. DATE'    MOVE IN HEADER                   00108301
         LA    R2,9(,R2)               INCREMENT POINTER                00108401
TRYLREFD CLC   LASTREF,=H'1'           TEST FOR LASTREF                 00108501
         BNE   TRYCCHHR                                                 00108601
         ST    R2,LREFFLD              SAVE OUTPUT FIELD ADDRESS        00108701
         MVC   0(8,R2),=C'LAST REF'    MOVE IN HEADER                   00108801
         LA    R2,10(,R2)              INCREMENT POINTER                00108901
TRYCCHHR CLC   CCHHR,=H'2'             TEST FOR NOCCHHR                 00109001
         BE    DODSNAME                                                 00109101
         ST    R2,CCHHRFLD             SAVE OUTPUT FIELD ADDRESS        00109201
         MVC   0(10,R2),=C' C C H H R' MOVE IN HEADER                   00109301
         LA    R2,11(,R2)              INCREMENT POINTER                00109401
DODSNAME L     R1,ERRFLD               LOAD ADDRESS OF ERROR FIELD      00109501
         LA    R1,33(,R1)              ALLOW ROOM FOR 32 BYTE MESSAGE   00109601
         CR    R2,R1                   COMPARE TO POINTER               00109701
         BH    DSNFLDOK                                                 00109801
         LR    R2,R1                   SET POINTER TO MIN FIELD ADDR    00109901
DSNFLDOK LA    R1,OUTLINE+120-44       LOAD LAST POSSIBLE ADDR FOR DSN  00110001
         CR    R2,R1                   SEE IF WE'RE PAST IT             00110101
*        BH    TOOLONG                 WHAT NONSENSE                    00110201
         ST    R2,DSNFLD               SAVE OUTPUT FIELD ADDRESS        00110301
         MVC   0(8,R2),=C'-DSNAME-'    MOVE IN HEADER                   00110401
         CLC   HEADER,=H'2'            TEST FOR HEADER                  00110501
         BE    BUILDDSN                                                 00110601
         BAL   R2,PUTL                 DISPLAY LINE                     00110701
*********************************************************************** 00110801
*                                                                     * 00110901
*        CONSTRUCT DSNAME                                             * 00111001
*                                                                     * 00111101
*********************************************************************** 00111201
BUILDDSN LA    R3,DSN                  LOAD DSN PDL ADDRESS             00111301
SETUPNXT ST    R3,CURRPDL              REMEMBER THE ENTRY       GP09144 00111401
         MVC   DSNAME,BLANKS           CLEAR DSNAME FIELD               00111501
         MVC   OUTLINE,BLANKS                                           00111601
         NI    FLAGS2,X'FF'-$FOUND     RESET DATA SET FOUND FLAG        00111701
         LA    R2,DSNAME               SET POINTER TO DSNAME            00111801
*--------SEE IF NAME ENCLOSED IN QUOTES                                 00111901
         TM    6(R3),X'40'                                              00112001
         BNZ   MOVEREST                                                 00112101
*--------PREPEND PREFIX TO NAME                                         00112201
         L     R5,MYCPPL               GET CPPL BACK            GP09143 00112301
         USING CPPL,R5                                          GP09143 00112401
         L     R1,CPPLUPT              LOAD UPT ADDRESS                 00112501
         USING UPT,R1                  DEFINE UPT ADDRESSABILITY        00112601
         LA    R4,UPTPREFX             LOAD ADDRESS OF PREFIX ENTRY     00112701
         CLI   UPTPREFL,0              TEST FOR NOPREFIX                00112801
         BNE   GETLEN                                                   00112901
         CLC   4(2,R3),ZEROS           SEE IF DSNAME ENTERED    GP09127 00113001
         BNE   MOVEREST                                         GP09127 00113101
         DROP  R1                                                       00113201
         L     R4,CPPLPSCB             LOAD USERID ADDRESS              00113301
         DROP  R5                        DONE WITH CPPL         GP09143 00113401
         CLI   7(R4),0                 SEE IF NO USERID (BATCH)         00113501
         BE    NOID                                                     00113601
GETLEN   SR    R1,R1                   CLEAR REGISTER 1                 00113701
         IC    R1,7(,R4)               LOAD PREFIX LENGTH               00113801
         BCTR  R1,R0                   REDUCE LENGTH BY 1               00113901
         EX    R1,MOVENAME             COPY PREFIX TO DSNAME FIELD      00114001
         LA    R2,1(R1,R2)             INCREMENT PAST USERID            00114101
         CLC   4(2,R3),ZEROS           SEE IF DSNAME ENTERED    GP09127 00114201
         BE    LOCATE                  NO                       GP09127 00114301
         MVI   0(R2),C'.'              ADD .                            00114401
         LA    R2,1(,R2)               INCREMENT PAST .                 00114501
*--------FULLY QUALIFIED NAME SPECIFIED                                 00114601
MOVEREST L     R4,0(,R3)               LOAD ADDRESS OF INPUT            00114701
         LH    R1,4(,R3)               LOAD LENGTH                      00114801
         BCTR  R1,R0                   REDUCE LENGTH BY 1               00114901
         EX    R1,MOVENAME             COPY NAME                        00115001
*********************************************************************** 00115101
*                                                                     * 00115201
*        LOCATE CATALOG BLOCK                                         * 00115301
*                                                                     * 00115401
*********************************************************************** 00115501
LOCATE   MVC   INDEX,DSNAME            COPY TO INDEX NAME ALSO          00115601
         SR    R7,R7                   NOT SUPERLOCATE          GP09143 00115701
         L     R2,DSNFLD               LOAD DSNAME FIELD ADDRESS        00115801
         MVC   0(44,R2),DSNAME         COPY DSNAME TO OUTPUT LINE       00115901
         XC    TOTALDS(8),TOTALDS      RESET DATA SET ALLOCATION TOTALS 00116001
         XC    PDSTOTLS(16),PDSTOTLS   RESET PDS TOTALS                 00116101
         MVI   CATBLOCK+2,X'FF'        GARBAGE INTO CATALOG BLOCK AREA  00116201
         TM    VOLSER+6,X'80'          SEE IF VOLUME SERIAL IS PRESENT  00116301
         BZ    NOVOLSER                                         GP09143 00116401
         OI    FLAGS2,$F2VMASK         INDICATE SEARCH BY SER.  GP09143 00116501
         MVC   CEREAL,=CL6' '          BLANK OUT VOLUME SERIAL  GP09143 00116601
         LH    R1,VOLSER+4             GET VOLUME SERIAL LENGTH GP09143 00116701
         BCTR  R1,0                    REDUCE BY 1              GP09143 00116801
         LA    R2,CEREAL               LOAD DESTINATION ADDRESS GP09143 00116901
         L     R4,VOLSER               LOAD SOURCE ADDRESS      GP09143 00117001
         EX    R1,MOVENAME             COPY VOLUME SERIAL       GP09143 00117101
         CLI   0(R4),C'*'              TEST FOR * ENTERED       GP09143 00117201
         BNE   NOVOLSER                NO; DONE                 GP09143 00117301
         XI    FLAGS2,$F2VMASK+$F2VANY  DO ANY VOLUME           GP09143 00117401
NOVOLSER CLC   GENERIC,ZEROS           TEST FOR GENERIC OPTION  GP09127 00117501
         BNE   PROCGRP                                          GP09127 00117601
         LOCATE LOCLIST                LOCATE BY NAME                   00117701
         LTR   R15,R15                 TEST RETURN CODE                 00117801
         BNZ   CHECK8                                                   00117901
         CLC   DSNAME,INDEX            SEE IF ALIAS NAME RETURNED       00118001
         BE    OBTAIN1                                                  00118101
         CLC   DSNAME(8),=C'SYSCTLG.'  SEE IF CVOL NAME RETURNED        00118201
         BNE   PROCGRP                                                  00118301
         MVC   CVOL,CATBLOCK+6         COPY CVOL SERIAL                 00118401
         MVC   DSNAME,INDEX            RESET DATA SET/INDEX NAME        00118501
         LOCATE LOCLIST2               LOCATE BY NAME AND CVOL          00118601
         CH    R15,=H'12'              RC = 12 - PROCESS BY TTR         00118701
         BE    PROCTTR                                                  00118801
         LTR   R15,R15                 IF CVOL NAME RETURNED AGAIN,     00118901
         BZ    PROCGRP                    SU 8 NOT INSTALLED            00119001
         B     BASLOCER                                                 00119101
CHECK8   CH    R15,=H'8'               SEE IF EMPTY INDEX STRUCTURE     00119201
         BNE   CHECK12                                                  00119301
         CLC   CATBLOCK+2(8),=X'0000000000000001'  TEST FOR ICE         00119401
         BNE   PROCGRP                                                  00119501
         CLC   CATBLOCK+20(8),=X'FFFFFFFFFFFFFFFF' TEST FOR ILE         00119601
         BNE   PROCGRP                                                  00119701
*--------EMPTY INDEX STRUCTURE                                          00119801
         L     R1,ERRFLD               LOAD ERROR FIELD ADDRESS         00119901
         MVC   0(32,R1),EMPTYNDX       COPY ERROR MESSAGE               00120001
         L     R2,DSNFLD               LOAD DSNAME FIELD ADDRESS        00120101
         MVC   0(44,R2),INDEX          COPY INDEX NAME                  00120201
         BAL   R2,PUTL                 DISPLAY LINE                     00120301
         B     ENDGRP                                                   00120401
.TESTNDX ANOP                                                           00120501
*--------TEST FOR INDEX (MVT, SVS, OR MVS WITH SU 8)                    00120601
CHECK12  CH    R15,=H'12'              TEST FOR INDEX NAME              00120701
         BNE   BASLOCER                                                 00120801
         TITLE 'D S A T  ***  PROCESS CVOL CATALOG ENTRIES'     GP09143 00120901
*********************************************************************** 00121001
*                                                                     * 00121101
*        PROCESS DATA SET GROUP VIA TTR                               * 00121201
*                                                                     * 00121301
*********************************************************************** 00121401
PROCTTR  CLC   DSONLY,ZEROS            SEE IF DSONLY SPECIFIED  GP09127 00121501
         BNE   BASLOCER                                         GP09127 00121601
         OI    FLAGS,DSATGRP           SET DSGROUP FLAG                 00121701
         MVC   CVOL,CATBLOCK+259       SAVE CVOL SERIAL                 00121801
         L     R1,WORKAREA             LOAD FIRST BLOCK ADDRESS         00121901
         ST    R1,FIRSTBLK             SAVE FIRST BLOCK ADDRESS         00122001
         ST    R1,CURRBLK              SAVE CURRENT BLOCK ADDRESS       00122101
         LA    R2,10(,R1)              LOAD ADDRESS OF FIRST DATA ENTRY 00122201
         ST    R2,0(,R1)               SAVE CURRENT ENTRY ADDRESS       00122301
         MVC   0(254,R2),CATBLOCK+2    COPY CATALOG BLOCK               00122401
         LA    R4,INDEX                LOAD INDEX NAME                  00122501
FINDEND1 LA    R4,1(,R4)               POINT TO NEXT CHARACTER          00122601
         CLI   0(R4),C' '              TEST FOR BLANK                   00122701
         BNE   FINDEND1                                                 00122801
         ST    R4,ENDNAME              SAVE END OF NAME ADDRESS         00122901
         B     PROCNTRY                LOAD INDEX NAME                  00123001
FINDEND  LA    R4,1(,R4)               POINT TO NEXT CHARACTER          00123101
         CLI   0(R4),C' '              TEST FOR BLANK                   00123201
         BNE   FINDEND                                                  00123301
         ST    R4,ENDNAME              SAVE END OF NAME ADDRESS         00123401
READCTLG ST    R1,CURRBLK              SAVE CURRENT BLOCK ADDRESS       00123501
         LA    R2,8(,R1)               LOAD WORK AREA ADDRESS           00123601
         ST    R2,LOCBYTTR+12          SAVE IN PARM LIST                00123701
         LA    R2,2(,R2)               FIRST 2 BYTES WILL BE LENGTH     00123801
         ST    R2,0(,R1)               INITIALIZE POINTER               00123901
         LOCATE LOCBYTTR               LOCATE BY TTR                    00124001
         LTR   R15,R15                 TEST RETURN CODE                 00124101
         BNZ   BASLOCER                                                 00124201
         L     R1,CURRBLK              LOAD CURRENT BLOCK ADDRESS       00124301
PROCNTRY CLC   0(8,R2),=X'FFFFFFFFFFFFFFFF'  TEST FOR LINK ENTRY        00124401
         BE    LNKENTRY                                                 00124501
         CLC   0(8,R2),=X'0000000000000001'  TEST FOR INDEX CONTROL     00124601
         BE    DSATNEXT                                                 00124701
         CLI   11(R2),X'01'            DATA SET WITH OVER 5 VOLUMES     00124801
         BE    DSNOVER5                                                 00124901
         CLI   11(R2),X'00'            TEST FOR INDEX ENTRY             00125001
         BE    NDXENTRY                                                 00125101
         CLI   11(R2),X'02'            TEST FOR GDG ENTRY               00125201
         BNE   DSNENTRY                                                 00125301
*--------GENERATION INDEX                                               00125401
         OI    FLAGS,GDG               INDICATE GDG                     00125501
         CLC   GDGDATA,=H'2'           SEE IF GDG DATA REQUESTED        00125601
         BE    NDXENTRY                                                 00125701
         MVC   OUTLINE(L'GDGMSG),GDGMSG     COPY MESSAGE                00125801
         L     R15,ENDNAME             LOAD END OF INDEX NAME           00125901
         MVI   0(R15),C'.'             ADD .                            00126001
         MVC   1(8,R15),0(R2)          ADD INDEX QUALIFIER              00126101
         L     R15,DSNFLD              LOAD DSNAME FIELD ADDRESS        00126201
         MVC   0(44,R15),INDEX         COPY INDEX TO MESSAGE            00126301
         SR    R15,R15                 LOAD MAXIMUM ENTRIES             00126401
         IC    R15,13(,R2)                                              00126501
         CVD   R15,DSATPDEC            CONVERT MAX                      00126601
         OI    DSATPDEC+7,X'0F'        SET SIGN CODE                    00126701
         UNPK  DSATDEC,DSATPDEC+4(4)   UNPACK                           00126801
         MVC   OUTLINE+36(3),DSATDEC+5 COPY TO MESSAGE                  00126901
         LH    R15,14(,R2)             LOAD CURRENT                     00127001
         CVD   R15,DSATPDEC            CONVERT TO DECIMAL               00127101
         OI    DSATPDEC+7,X'0F'        SET SIGN CODE                    00127201
         UNPK  DSATDEC,DSATPDEC+4(4)   UNPACK                           00127301
         MVC   OUTLINE+28(3),DSATDEC+5 COPY TO MESSAGE                  00127401
         TM    12(R2),X'03'            TEST FLAG BYTE                   00127501
         BZ    PUTGDG                                                   00127601
         BM    GDGEORD                                                  00127701
         MVC   OUTLINE+15(4),=C'E,D '  INDICATE EMPTY, DELETE           00127801
         B     PUTGDG                                                   00127901
GDGEORD  MVC   OUTLINE+15(4),=C'E   '                                   00128001
         TM    12(R2),X'01'                                             00128101
         BNZ   PUTGDG                                                   00128201
         MVI   OUTLINE+15,C'D'                                          00128301
PUTGDG   LR    R4,R2                   SAVE R2                          00128401
         BAL   R2,PUTL                 DISPLAY LINE                     00128501
         LR    R2,R4                   RESTORE R2                       00128601
         MVC   OUTLINE,BLANKS          CLEAR OUTPUT AREA                00128701
*--------INDEX ENTRY - ADD NAME TO INDEX AND GET NEXT BLOCK             00128801
NDXENTRY L     R4,ENDNAME              LOAD END OF NAME ADDRESS         00128901
         MVI   0(R4),C'.'                                               00129001
         MVC   1(8,R4),0(R2)           ADD INDEX NAME                   00129101
         NI    FLAGS,X'FF'-NOTEMPTY    CLEAR EMPTY INDICATOR            00129201
         MVC   TTR,8(R2)               COPY TTR                         00129301
         L     R1,CURRBLK              LOAD CURRENT BLOCK ADDRESS       00129401
         LA    R1,264(,R1)             INCREMENT TO NEXT BLOCK          00129501
         B     FINDEND                                                  00129601
*--------INDEX LINK ENTRY                                               00129701
LNKENTRY CLC   8(3,R2),ZEROS           TEST FOR ZERO TTR        GP09127 00129801
         BE    ENDLEVEL                                         GP09127 00129901
         MVC   TTR,8(R2)               COPY TTR                         00130001
         L     R1,CURRBLK              LOAD CURRENT BLOCK ADDRESS       00130101
         B     READCTLG                                                 00130201
*--------END OF INDEX LEVEL                                             00130301
ENDLEVEL TM    FLAGS,NOTEMPTY          TEST FOR EMPTY INDEX             00130401
         BNZ   TESTDONE                                                 00130501
         TM    FLAGS,GDG               TEST FOR GDG                     00130601
         BNZ   MTGDG                                                    00130701
         MVC   OUTLINE(32),EMPTYNDX    MOVE MESSAGE INTO LINE           00130801
         B     PUTEMPTY                                                 00130901
MTGDG    MVC   OUTLINE(32),EMPTYGDG    MOVE MESSAGE INTO LINE           00131001
PUTEMPTY L     R2,DSNFLD               LOAD DSNAME FIELD                00131101
         MVC   0(44,R2),INDEX          MOVE INDEX INTO MESSAGE          00131201
         BAL   R2,PUTL                 DISPLAY MESSAGE                  00131301
         MVC   OUTLINE,BLANKS          CLEAR LINE                       00131401
TESTDONE OI    FLAGS,NOTEMPTY          SET NOT EMPTY                    00131501
         NI    FLAGS,X'FF'-GDG         CLEAR GDG INDICATOR              00131601
         CLC   FIRSTBLK,CURRBLK        TEST FOR DONE                    00131701
         BE    ENDGRP                                                   00131801
         L     R4,ENDNAME              REMOVE LOWEST LEVEL NAME         00131901
FINDDOT  BCTR  R4,0                                                     00132001
         CLI   0(R4),C'.'                                               00132101
         MVI   0(R4),C' '                                               00132201
         BNE   FINDDOT                                                  00132301
         ST    R4,ENDNAME                                               00132401
         L     R1,CURRBLK              POINT TO PREVIOUS BLOCK          00132501
         S     R1,=F'264'                                               00132601
         ST    R1,CURRBLK                                               00132701
*--------NEXT ENTRY                                                     00132801
DSATNEXT L     R1,CURRBLK              LOAD CURRENT BLOCK ADDRESS       00132901
         L     R2,0(,R1)               LOAD POINTER                     00133001
         SR    R15,R15                 LOAD HALFWORD COUNT              00133101
         IC    R15,11(,R2)                                              00133201
         SLA   R15,1                   DOUBLE TO GET BYTES              00133301
         LA    R2,12(R15,R2)           12 BYTES FOR HEADER              00133401
         ST    R2,0(,R1)               SAVE POINTER                     00133501
         B     PROCNTRY                                                 00133601
*--------DATA SET ENTRY WITH OVER 5 VOLUMES                             00133701
DSNOVER5 OI    FLAGS,NOTEMPTY          INDICATE NOT EMPTY               00133801
         MVC   DSNAME,INDEX            COPY INDEX TO DSNAME             00133901
         L     R1,ENDNAME              GET INDEX NAME LENGTH            00134001
         LA    R15,INDEX                                                00134101
         SR    R1,R15                                                   00134201
         LA    R1,DSNAME(R1)           POINT TO END OF NAME             00134301
         MVI   0(R1),C'.'                                               00134401
         MVC   1(8,R1),0(R2)           ADD NAME                         00134501
         TM    2(R1),X'F0'             TEST FOR GDG NAME                00134601
         BNZ   LOCATE2                                                  00134701
         XC    2(4,R1),=X'FFFFFFFF'    COMPLEMENT GENERATION NUMBER     00134801
LOCATE2  L     R2,DSNFLD               LOAD DSNAME FIELD ADDRESS        00134901
         MVC   0(44,R2),DSNAME         COPY DSNAME TO OUTPUT LINE       00135001
         XC    TOTALDS(8),TOTALDS      RESET DATA SET ALLOCATION TOTALS 00135101
         XC    PDSTOTLS(16),PDSTOTLS   RESET PDS TOTALS                 00135201
         LOCATE LOCLIST                LOCATE BY NAME                   00135301
         LTR   R15,R15                 TEST RETURN CODE                 00135401
         BZ    OBTAIN1                                                  00135501
         B     BASLOCER                                                 00135601
*--------DATA SET ENTRY                                                 00135701
DSNENTRY OI    FLAGS,NOTEMPTY          INDICATE NOT EMPTY               00135801
         MVC   DSNAME,INDEX            COPY INDEX TO DSNAME             00135901
         L     R1,ENDNAME                                               00136001
         LA    R15,INDEX                                                00136101
         SR    R1,R15                  GET LENGTH OF INDEX              00136201
         LA    R1,DSNAME(R1)           POINT TO END OF NAME             00136301
         MVI   0(R1),C'.'                                               00136401
         MVC   1(8,R1),0(R2)           ADD NAME                         00136501
         TM    2(R1),X'F0'             TEST FOR GDG NAME                00136601
         BNZ   DSNMVC                                                   00136701
         XC    2(4,R1),=X'FFFFFFFF'    COMPLEMENT GENERATION NUMBER     00136801
DSNMVC   LA    R8,12(,R2)              POINT TO VOLUME LIST     GP12013 00136901
         L     R2,DSNFLD               LOAD DSNAME FIELD ADDRESS        00137001
         MVC   0(44,R2),DSNAME         COPY DSNAME                      00137101
         XC    TOTALDS(8),TOTALDS      RESET DATA SET ALLOCATION TOTALS 00137201
         XC    PDSTOTLS(16),PDSTOTLS   RESET PDS TOTALS                 00137301
         MVC   OBTNAME,DSNAME          SET OBTAIN NAME          GP09143 00137401
         L     R15,=A(FORMINFO)        FORMAT ONE DS            GP09143 00137501
         BALR  R14,R15                 FORMAT ONE DS            GP09143 00137601
         CH    R15,=H'8'     0-OK 4-SKP 8-ENDG 12-EXIT          GP09143 00137701
         BE    ENDGRP                  ELSE DONE WITH GROUP     GP09143 00137801
         BH    EXIT                                             GP09143 00137901
         TM    FLAGS,DSATGRP           GROUP OR SINGLE DS?      GP09143 00138001
         BNZ   DSATNEXT                NEXT DS, IF ANY          GP09143 00138101
         B     ENDGRP                  ELSE DONE WITH GROUP     GP09143 00138201
         TITLE 'D S A T  ***  PROCESS VSAM CATALOG ENTRIES'     GP09143 00138301
*********************************************************************** 00138401
*                                                                     * 00138501
*        PROCESS DATA SET GROUP VIA VSAM LOCATE                       * 00138601
*                                                                     * 00138701
*********************************************************************** 00138801
PROCGRP  OI    FLAGS2,$VSAMLOC         INDICATE VSAM LOCATE BEING USED  00138901
         LA    R1,INDEX+44             LOAD END OF SEARCH KEY ADDRESS   00139001
BACKUP   BCTR  R1,0                    DECREMENT POINTER                00139101
         CLI   0(R1),C' '              TEST FOR BLANK                   00139201
         BE    BACKUP                                                   00139301
         LA    R7,1(,R1)               SAVE ADDRESS OF BLANK    GP09143 00139401
         LA    R15,INDEX               LOAD INDEX ADDRESS               00139501
         SR    R7,R15                  GET LENGTH OF INDEX NAME         00139601
         CLC   DSONLY,=H'1'            TEST FOR DSONLY          GP09127 00139701
         BE    SAVELEN                 YES                      GP09127 00139801
         CLC   GENERIC,=H'1'           TEST FOR GENERIC         GP09127 00139901
         BE    SAVELEN                 YES                      GP09127 00140001
*  THE ABOVE DOES NOT MATCH DOCUMENTATION - FOR VSAM CATALOGS, GENERIC  00140101
*     IS ASSUMED (UNLESS DSONLY). APPEARS TO WORK BACKWARDS     GP09143 00140201
         LA    R7,1(,R7)               ADD ONE MORE FOR THE PERIOD      00140301
         MVI   1(R1),C'.'              ADD THE PERIOD                   00140401
SAVELEN  STC   R7,INDEXLEN                                              00140501
RETRYLOC L     R15,WORKAREA            LOAD WORK AREA                   00140601
         XC    0(4,R15),0(R15)         ZERO LENGTH FIELDS               00140701
         L     R1,WORKLEN              LOAD LENGTH OF WORK AREA         00140801
         CH    R1,=H'32767'            NOT TOO LARGE?           GP09143 00140901
         BNH   *+8                     OK                       GP09143 00141001
         LH    R1,=H'32767'            USE MAXIMUM SUPPORTED    GP09143 00141101
         STH   R1,0(,R15)              STORE IN WORK AREA               00141201
         LOCATE CTGPL                  LOCATE ALL DSNAMES               00141301
         LTR   R15,R15                 TEST RETURN CODE                 00141401
         BZ    PROCDATA                                                 00141501
*********************************************************************** 00141601
*                                                                     * 00141701
*        RETURN CODES 40 AND 44 INDICATE THAT THE WORK AREA IS TOO    * 00141801
*        SMALL TO CONTAIN ALL THE DATA SET NAMES, BUT AS MANY NAMES   * 00141901
*        AS COULD BE HELD HAVE BEEN RETURNED.                         * 00142001
*                                                                     * 00142101
*        CODE 44 INDICATES THAT SVC 26 COULD NOT DETERMINE THE SIZE   * 00142201
*        WORK AREA REQUIRED, SO CONTINUE PROCESSING THE NAMES THAT    * 00142301
*        WERE RETURNED.                                               * 00142401
*                                                                     * 00142501
*        CODE 40 INDICATES THAT SVC 26 RETURNED THE REQUIRED WORK     * 00142601
*        AREA SIZE IN THE SECOND HALFWORD OF THE WORK AREA.  FREE     * 00142701
*        THE ORIGINAL WORK AREA, OBTAIN A NEW WORK AREA OF REQUIRED   * 00142801
*        SIZE, AND RE-TRY THE LOCATE REQUEST.                         * 00142901
*                                                                     * 00143001
*        NOTE THAT SVC 26 CONSIDERS A SIZE OF 32,768 (X'8000') OR     * 00143101
*        MORE A NEGATIVE SIZE AND WILL NOT RETURN ANY INFORMATION.    * 00143201
*        IF MORE THAN 32,767 IS REQUIRED, JUST PROCESS WHAT WAS       * 00143301
*        RETURNED.                                                    * 00143401
*                                                                     * 00143501
*********************************************************************** 00143601
         CH    R15,=H'44'              SEE IF WORK AREA TOO SMALL       00143701
         BE    PARTIAL                                                  00143801
         CH    R15,=H'40'                                               00143901
         BNE   BASLOCER                                                 00144001
*--------GET NEW, BIGGER WORK AREA                                      00144101
         L     R1,WORKAREA             LOAD WORK AREA ADDRESS           00144201
         CLC   0(2,R1),=H'32767'       ALREADY AT MAXIMUM?      GP09143 00144301
         BNL   PARTIAL                 YES                      GP09143 00144401
         L     R2,WORKLEN              LOAD WORK AREA LENGTH            00144501
         XC    0(2,R1),0(R1)           ZERO 2 BYTES                     00144601
         L     R7,0(,R1)               LOAD LENGTH NEEDED       GP08269 00144701
         FREEMAIN R,LV=(R2),A=(R1),SP=1  FREE OLD WORK AREA             00144801
         GETMAIN  RC,LV=(R7),SP=1,BNDRY=PAGE  LARGER WORK AREA  GP08269 00144901
         ST    R7,WORKLEN              SAVE WORK AREA LENGTH    GP08269 00145001
         ST    R1,WORKAREA             SAVE WORK AREA ADDRESS           00145101
         ST    R1,CTGWKA               STORE ADDRESS IN CTGPL           00145201
         B     RETRYLOC                                                 00145301
*--------TOO MANY NAMES FOR WORK AREA                                   00145401
PARTIAL  OI    FLAGS2,$INCMPLT         INDICATE NAME LIST INCOMPLETE    00145501
         L     R15,WORKAREA            LOAD WORK AREA ADDRESS   GP12013 00145601
         MVC   0(4,R15),=A((32767/45)*45)    SET MAXIMUM LENGTH GP12013 00145701
*--------PROCESS DATA SETS                                              00145801
PROCDATA OI    FLAGS,DSATGRP           SET GROUP INDICATOR              00145901
         L     R15,WORKAREA            LOAD WORK AREA ADDRESS           00146001
         XC    0(2,R15),0(R15)         ZERO HIGH TWO BYTES              00146101
         L     R1,0(,R15)              LOAD LENGTH OF WORK AREA USED    00146201
         AR    R1,R15                  ADD TO WORK AREA ADDRESS GP12013 00146301
         S     R1,=F'44'               BACK OFF ONE ENTRY LESS ONE      00146401
         ST    R1,ENDWORK              SAVE END OF WORK AREA USED ADDR  00146501
         LA    R7,4(,R15)              LOAD ADDRESS OF FIRST ENTRY      00146601
         CLI   0(R7),C'0'    (USER) CATALOG ENTRY?              GP09127 00146701
         BNE   DSATPROC      NO                                 GP09127 00146801
         CLI   1(R7),C' '    PRINTABLE NAME ?                   GP09127 00146901
         BNH   DSATNXT3      NO; SKIP IT                        GP09127 00147001
         L     R2,ERRFLD     GET GENERAL USE FIELD              GP09127 00147101
         MVC   0(13,R2),=C'Using Catalog'                       GP09127 00147201
         L     R2,DSNFLD               LOAD ADDRESS OF DSNAME   GP09127 00147301
         MVC   0(44,R2),1(R7)          COPY DSNAME INTO MESSAGE GP09127 00147401
         MVC   DSNAME,1(R7)            SET LOCATE NAME          GL09127 00147501
         LOCATE LOCLIST                LOCATE BY NAME           GP09127 00147601
         LTR   R15,R15                 TEST RETURN CODE         GP09127 00147701
         BNZ   PUTCATNM                                         GP09127 00147801
         L     R2,ERRFLD     GET GENERAL USE FIELD              GP09127 00147901
         MVC   16(2,R2),=C'on'                                  GP09127 00148001
         LA    R8,CATBLOCK             LOAD VOLUME LIST ADDRESS GP09127 00148101
         MVC   19(6,R2),6(R8)          SHOW SERIAL              GP09127 00148201
PUTCATNM BAL   R2,PUTL                                          GP09127 00148301
         MVC   OUTLINE,BLANKS                                   GP09127 00148401
         ST    R7,CURRBLK              SET ENTRY ADDRESS        GP09127 00148501
         SPACE 1                                                        00148601
DSATNXT2 L     R7,CURRBLK              LOAD ADDR LAST ENTRY PROCESSED   00148701
DSATNXT3 LA    R7,45(,R7)              INCREMENT TO NEXT NAME           00148801
DSATPROC C     R7,ENDWORK              SEE IF NAME IS OUT OF WORK AREA  00148901
         BH    TESTCOMP                                                 00149001
         ST    R7,CURRBLK              SAVE ADDRESS OF THIS ENTRY       00149101
         MVC   DSNAME,1(R7)            COPY DSNAME                      00149201
         XC    TOTALDS(8),TOTALDS      RESET DATA SET ALLOCATION TOTALS 00149301
         XC    PDSTOTLS(16),PDSTOTLS   RESET PDS TOTALS                 00149401
         L     R2,DSNFLD               LOAD ADDRESS OF DSNAME FIELD     00149501
         MVC   0(44,R2),DSNAME         COPY DSNAME INTO MESSAGE         00149601
         LOCATE LOCLIST                LOCATE BY NAME                   00149701
         LTR   R15,R15                 TEST RETURN CODE                 00149801
         BZ    OBTAIN1                                          GP09143 00149901
         CLI   0(R7),C'A'              NON-VSAM ?               GP09143 00150001
         BE    BASLOCER                YES                      GP09143 00150101
         CLI   0(R7),C'B'              GDG DEFINITION?          GP09143 00150201
         BE    GETGDG                  YES                      GP09143 00150301
BASFAIL  LA    R15,8                   DISPLAY AS VSAM TYPE     GP09143 00150401
         MVC   VOLCNT,=H'1'            PREVENT ERROR MSG        GP09143 00150501
         B     BASDSNER                AND ISSUE BETTER MESSAGE GP09143 00150601
TESTCOMP TM    FLAGS2,$INCMPLT         SEE IF NAME LIST INCOMPLETE      00150701
         BZ    ENDGRP                                                   00150801
         MVC   OUTLINE,BLANKS          BLANK OUT LINE           GP12013 00150901
         L     R2,ERRFLD               LOAD ERROR FIELD ADDRESS         00151001
         MVC   0(32,R2),DSATERR3       MOVE IN ERROR MESSAGE            00151101
         BAL   R2,PUTL                 DISPLAY LINE             GP12013 00151201
         B     ENDGRP                    QUIT GROUP NOW         GP12013 00151301
BASPUTL  BAL   R2,PUTL                 DISPLAY LINE                     00151401
BASRSET  MVC   OUTLINE,BLANKS          BLANK OUT LINE                   00151501
         B     SKIPDS                                           GP09143 00151601
         SPACE 1                                                        00151701
*DEFER*  CLC   GDGINFO,=H'2'           WANT GDG INFO ?          GP09143 00151801
*DEFER*  BE    BASFAIL                 NO; JUST SHOW GDG ENTRY  GP09143 00151901
GETGDG   LA    R2,SHOWSPAC             RETURN AREA              GP09143 00152001
         MVC   0(4,R2),=AL2(256*8+12,0)   RESET WORK COUNTER    GP09143 00152101
         SHOWCAT NAME=DSNAME,AREA=(R2),MF=(B,SHOWCATL)          GP09143 00152201
         SHOWCAT NAME=DSNAME,AREA=(R2),MF=(E,SHOWCATL)          GP09143 00152301
         N     R15,=X'FFFFFFDF' ^X'20' UNEXPECTED RETURN CODE?  GP09143 00152401
         BNZ   BASFAIL                 YES; JUST SHOW BASE      GP09143 00152501
         LH    R2,SHOWSPAC+2           GET LENGTH USED          GP09143 00152601
         SH    R2,=H'12'               LESS FIXED OVERHEAD      GP09143 00152701
         BNP   BASFAIL                 NO DATA RETURNED         GP09143 00152801
         LA    R1,SHOWSPAC+12-8        PRESET ONE HIGHER        GP09143 00152901
         AH    R2,=H'8'                DITTO                    GP09143 00153001
         SRL   R2,3                    DIVIDE TO GET NUMBER     GP09143 00153101
         STM   R1,R2,GDSCURR           SET FOR NEXT INCREMENT   GP09143 00153201
         LA    R15,44-9                MAX BASE LENGTH          GP09143 00153301
         LA    R14,DSNAME+44-9         BASE NAME                GP09143 00153401
GDSGTLEN CLI   0(R14),C' '             TRAILING NON-BLANK YET?  GP09143 00153501
         BNE   GDSSTLEN                YES; STASH LENGTH        GP09143 00153601
         BCTR  R14,0                                            GP09143 00153701
         BCT   R15,GDSGTLEN            TRY AGAIN                GP09143 00153801
GDSSTLEN LA    R15,1(,R15)             TRUE BASE LENGTH         GP09143 00153901
         ST    R15,GDSBLEN             SAVE BASE LENGTH         GP09143 00154001
         L     R2,ERRFLD                                        GP09143 00154101
         MVC   0(16,R2),CATBASE+1      IDENTIFY                 GP09143 00154201
         BAL   R2,PUTL                 SHOW BASE                GP09143 00154301
         MVC   OUTLINE,BLANKS           AND CLEAR               GP09143 00154401
         SPACE 1                                                        00154501
LOOPGDS  LM    R1,R2,GDSCURR           GET PRIOR GENERATION     GP09143 00154601
         LA    R1,8(,R1)               SKIP TO NEXT             GP09143 00154701
         SH    R2,=H'1'                ACCOUNT FOR IT           GP09143 00154801
         BNP   SKIPDS                  DONE HERE                GP09143 00154901
         STM   R1,R2,GDSCURR           SET NEXT GENERATION      GP09143 00155001
         CLI   0(R1),C'A'              NON-VSAM ENTRY ?         GP09143 00155101
         BNE   LOOPGDS                 NO; SKIP                 GP09143 00155201
         MVC   OBTNAME,DSNAME          GET BASE NAME            GP09143 00155301
         L     R15,GDSBLEN             GET BASE LENGTH          GP09143 00155401
         LA    R15,OBTNAME(R15)        POINT TO INSERTION       GP09143 00155501
         MVC   0(9,R15),=C'.GnnnnV00'  MOVE PATTERN             GP09143 00155601
         MVC   2(4,R15),4(R1)          COMPLETE NAME            GP09143 00155701
         L     R2,DSNFLD               GET LINE POINTER         GP09143 00155801
         MVC   0(44,R2),OBTNAME        COMPLETE NAME            GP09143 00155901
         LA    R8,GDSSPACE             POINT TO RETURN          GP09143 00156001
         XC    0(4,R8),0(R8)           CLEAR IT                 GP09143 00156101
         LOCATE GDSLIST                LOOK FOR IT              GP09143 00156201
         CLC   0(2,R8),ZEROS           ANY VOLUME(S) FOUND ?    GP09143 00156301
         BE    LOOPGDS                   NO; SKIP THIS ONE      GP09143 00156401
         L     R15,=A(FORMINFO)                                 GP09143 00156501
         BALR  R14,R15                                          GP09143 00156601
         CH    R15,=H'8'               TEST                     GP09143 00156701
         BL    LOOPGDS                 DO NEXT GENERATION       GP09143 00156801
         BE    DSATNXT2                DO NEXT VSAM CAT ENTRY   GP09143 00156901
         B     EXIT                    MAJOR ERROR              GP09143 00157001
         SPACE 1             OBTAIN DSCB1 FAILED                GP09143 00157101
BASDSNER CH    R15,=H'8'               TEST RETURN CODE                 00157201
         BH    BASDSNR1                                                 00157301
         BE    BASDSNTV                                                 00157401
         CLC   DAONLY,=H'2'            SEE IF MESSAGE TO BE SUPRESSED   00157501
         BE    BASRSET                                                  00157601
         OI    FLAGS,COUNT             COUNT THIS DATA SET              00157701
         B     BASDSNR1                                                 00157801
BASDSNTV TM    FLAGS2,$VSAMLOC         SUPER-LOCATE BEING USED? GP09143 00157901
         BZ    BASDSNR1                NO; NO INFO              GP09143 00158001
         L     R7,CURRBLK              CURRENT ENTRY            GP09143 00158101
         CLI   0(R7),C'A'              NON-VSAM ENTRY ?         GP09127 00158201
         BE    BASDSNR1                YES; REALLY NOT FOUND    GP09127 00158301
         TM    FLAGS2,$F2VANY+$F2VMASK  USER SPECIFIED VOLUME?  GP09143 00158401
         BNZ   BASRSET                 YES - NO ERROR MSG       GP09143 00158501
         L     R2,ERRFLD               GET ERROR FIELD          GP09127 00158601
         LM    R15,R1,=A(CATCOMP,CATCOMP2-CATCOMP,CATCOMPN)     GP09127 00158701
BASDSNLP CLC   0(1,R7),0(R15)          MATCH ?                  GP09127 00158801
         BE    BASDSNMG                YES; DISPLAY MESSAGE     GP09127 00158901
         BXLE  R15,R0,BASDSNLP         TRY AGAIN                GP09127 00159001
         CLI   0(R7),C' '              PRINTABLE ENTRY TYPE?    GP12013 00159101
         BL    DSATNXT3                  NO; IGNORE IT          GP12013 00159201
         MVC   CATCOMP2-CATCOMP(1,R2),0(R7)  SHOW ENTRY TYPE    GP09127 00159301
BASDSNMG OI    FLAGS2,$F2ENTY          SHOW MATCH FOUND         GP12018 00159401
         MVC   0(CATCOMP2-CATCOMP-1,R2),1(R15)   IDENTIFY       GP09127 00159501
         MVC   VOLCNT,=H'1'            DON'T DO MORE SERIALS    GP09143 00159601
         B     BASPUTL                 AND PRINT                GP09127 00159701
         SPACE 1                                                GP09127 00159801
BASDSNR1 OI    FLAGS,$NOTPOPS          INDICATE ERROR                   00159901
         SLA   R15,3                   MULTIPLY RETURN CODE BY 8        00160001
         LA    R15,DSNERRS-32(R15)     LOAD PROPER MESSAGE ADDRESS      00160101
         L     R1,ERRFLD               LOAD ERROR FIELD ADDRESS         00160201
         MVC   0(32,R1),0(R15)         COPY MESSAGE TO OUTPUT LINE      00160301
         B     BASPUTL                 PRINT IT                 GP09143 00160401
         SPACE 1                                                        00160501
*********************************************************************** 00160601
*                                                                     * 00160701
*   Call DSCB OBTAIN and data formatting routine                      * 00160801
*                                                                     * 00160901
*********************************************************************** 00161001
         SPACE 1                                                        00161101
OBTAIN1  LA    R8,CATBLOCK             LOAD VOLUME LIST ADDRESS         00161201
         MVC   OBTNAME,DSNAME          SET OBTAIN NAME          GP09143 00161301
         L     R15,=A(FORMINFO)        FORMAT ONE DS            GP09143 00161401
         BALR  R14,R15                 FORMAT ONE DS            GP09143 00161501
         CH    R15,=H'8'     0-OK 4-SKP 8-ENDG 12-EXIT          GP09143 00161601
         BE    ENDGRP                                           GP09143 00161701
         BH    EXIT                                             GP09143 00161801
         SPACE 1                                                        00161901
*********************************************************************** 00162001
*                                                                     * 00162101
*        PROCESS NEXT DATA SET                                        * 00162201
*                                                                     * 00162301
*********************************************************************** 00162401
SKIPDS   TM    FLAGS,DSATGRP           TEST FOR DATA SET GROUP  GP09143 00162501
         BZ    ENDGRP                                                   00162601
         TM    FLAGS2,$VSAMLOC         TEST FOR VSAM LOCATE             00162701
         BZ    DSATNEXT                GET NEXT DSNAME FROM CVOL        00162801
         B     DSATNXT2                GET NEXT DSNAME FROM LOCATE      00162901
ENDGRP   TM    FLAGS2,$FOUND+$F2ENTY   ANYTHING FOUND ?         GP12018 00163001
         BNZ   ENDGRP2                   YES; NO OOPS MSG       GP12018 00163101
         MVC   OUTLINE,BLANKS          BLANK OUTPUT LINE        GP12018 00163201
         TM    FLAGS2,$F2VANY+$F2VMASK SEE IF VOLUME ENTERED    GP09143 00163301
         BZ    NOTANY                    NO                     GP12018 00163401
         TM    FLAGS2,$F2VMASK         SPECIFIC REQUEST ?       GP09143 00163501
         BZ    NOTANY                  NO; USE ANY VOLUME MSG   GP09143 00163601
         MVC   OUTLINE(L'MSGNULL+4),MSGNULL                     GP12018 00163701
         MVC   OUTLINE+L'MSGNULL+5(L'CEREAL),CEREAL             GP12018 00163801
         TM    FLAGS,DSATGRP           GROUP OR SINGLE DS ?     GP09143 00163901
         BNZ   NOTACOM                 GROUP; SKIP DSN          GP09143 00164001
         L     R1,DSNFLD               LOAD DSNAME FIELD ADDR   GP09143 00164101
         MVC   0(44,R1),DSNAME         COPY DSNAME              GP09143 00164201
         B     NOTACOM                                          GP09143 00164301
NOTANY   MVC   OUTLINE(L'MSGNULL),MSGNULL  MENTION CATALOG      GP12018 00164401
NOTACOM  BAL   R2,PUTL                 DISPLAY ERROR MESSAGE    GP09143 00164501
ENDGRP2  NI    FLAGS,RESET             RESET DATA SET GROUP FLG GP09143 00164601
         NI    FLAGS2,X'FF'-$VSAMLOC-$INCMPLT                   GP09143 00164701
         L     R3,CURRPDL              REGAIN PDL ADDRESS       GP09144 00164801
         L     R3,24(,R3)              LOAD ADDRESS OF NEXT PDE         00164901
         C     R3,=X'FF000000'         TEST FOR END OF CHAIN            00165001
         BNE   SETUPNXT                                                 00165101
*********************************************************************** 00165201
*                                                                     * 00165301
*        DISPLAY TOTALS IF REQUESTED                                  * 00165401
*                                                                     * 00165501
*********************************************************************** 00165601
         CLC   TOTALS,=H'2'            SEE IF TOTALS REQUESTED          00165701
         BE    RETURN                                                   00165801
         MVC   OUTLINE,BLANKS          CLEAR OUTPUT LINE                00165901
         LM    R4,R8,TOTALRUN          LOAD TOTALS                      00166001
         CVD   R8,DSATPDEC             CONVERT NUMBER OF DATA SETS      00166101
         MVC   DSATDEC,=X'4020202020202120'                             00166201
         ED    DSATDEC,DSATPDEC+4      EDIT                             00166301
         L     R2,DSNFLD               LOAD DATA SET NAME FIELD         00166401
         MVC   0(4,R2),DSATDEC+4       MOVE INTO OUTPUT LINE            00166501
         MVC   5(19,R2),=C'DATA SETS DISPLAYED'                         00166601
         CH    R8,=H'1'                TEST FOR ONE OR ZERO DATA SETS   00166701
         BH    CNVTALOC                                                 00166801
         TM    FLAGS,$MULTVOL          TEST FOR MULTIVOLUME             00166901
         BZ    RETURN                                                   00167001
         NI    FLAGS,X'FF'-$MULTVOL    CLEAR MULTIVOLUME INDICATOR      00167101
         MVC   5(19,R2),=C'DATA SET DISPLAYED '                         00167201
CNVTALOC CLC   ALLOC,=H'2'             SEE IF ALLOCATION REQUESTED      00167301
         BE    DISPLAYT                                                 00167401
         L     R2,ALLOCFLD             LOAD ALLOCATION FIELD ADDRESS    00167501
         CVD   R4,DSATPDEC             CONVERT ALLOCATED TOTAL TO DEC   00167601
         MVC   DSATDEC,=X'4020202020202120'                             00167701
         ED    DSATDEC,DSATPDEC+4      EDIT                             00167801
         MVC   0(5,R2),DSATDEC+3       MOVE INTO OUTPUT LINE            00167901
         CVD   R5,DSATPDEC             CONVERT USED TOTAL TO DECIMAL    00168001
         MVC   DSATDEC,=X'4020202020202120'                             00168101
         ED    DSATDEC,DSATPDEC+4      EDIT                             00168201
         MVC   6(5,R2),DSATDEC+3       MOVE INTO OUTPUT LINE            00168301
DISPLAYT BAL   R2,PUTL                 DISPLAY LINE                     00168401
*********************************************************************** 00168501
*                                                                     * 00168601
*        SET UP RETURN CODE                                           * 00168701
*                                                                     * 00168801
*********************************************************************** 00168901
RETURN   LM    R4,R8,TOTALRUN          LOAD TOTALS                      00169001
         LA    R14,EXIT                SET FOR QUICK BRANCH     GP09143 00169101
         LH    R15,RC                  LOAD RETURN CODE                 00169201
         C     R15,=F'10'              SEE IF A PDS CODE                00169301
         BL    BRANCH                                                   00169401
         LM    R4,R7,PDSTOTLS          LOAD PDS TOTALS                  00169501
BRANCH   SLA   R15,2                   MULTIPLY BY 4                    00169601
         BZR   R14                     PREVENT LOOP             GP09143 00169701
         B     BRTABLE(R15)            BRANCH TO PROPER ROUTINE         00169801
BRTABLE  B     RC4                                                      00169901
*   1    B     RCTALLOC                                                 00170001
           LR  R15,R4                  TOTAL TRACKS ALLOCATED   GP09143 00170101
           BR  R14                     GO TO EXIT               GP09143 00170201
*   2    B     RCTUSED                                                  00170301
           LR  R15,R5                  TOTAL TRACKS USED        GP09143 00170401
           BR  R14                     GO TO EXIT               GP09143 00170501
*   3    B     RCLALLOC                                                 00170601
           LR  R15,R6                  LAST  TRACKS ALLOCATED   GP09143 00170701
           BR  R14                     GO TO EXIT               GP09143 00170801
*   4    B     RCLUSED                                                  00170901
           LR  R15,R7                  LAST  TRACKS ALLOCATED   GP09143 00171001
           BR  R14                     GO TO EXIT               GP09143 00171101
         B     RCTDIFF       5                                          00171201
         B     RCLDIFF       6                                          00171301
*   7    B     RCNUM                                                    00171401
          LR   R15,R8                  LOAD DATA SET COUNT      GP09143 00171501
          BR   R14                                              GP09143 00171601
         B     RCPREV        8                                          00171701
         B     RCDSORG       9                                          00171801
*  10    B     RCDIRA                                                   00171901
          LR   R15,R4                  DIRECTORY BLOCKS ALLOC.  GP09143 00172001
          BR   R14                                              GP09143 00172101
*  11    B     RCDIRU                                                   00172201
          LR   R15,R5                  DIRECTORY BLOCKS USED    GP09143 00172301
          BR   R14                                              GP09143 00172401
*  12    B     RCENTR                                                   00172501
          LR   R15,R6                  NUMBER OF ENTRIES        GP09143 00172601
          BR   R14                                              GP09143 00172701
         B     RCMEMB       13                                          00172801
*  14    B     RCALIAS                                          GP09143 00172901
          LR   R15,R7                  NUMBER OF ALIASES        GP09143 00173001
          BR   R14                                              GP09143 00173101
         B     RCPCTUSE     15                                  GP09143 00173201
         B     RCPCLUSE     16                                  GP09143 00173301
RCMEMB   LR    R15,R6                  NUMBER OF ENTRIES                00173401
         SR    R15,R7                  MINUS ALIASES = MEMBERS          00173501
         BR    R14                                              GP09143 00173601
RCDSORG  NI    FLAGS,$NOTPOPS+$DSPO+$DSPS   SAVE DSORG INDICATORS       00173701
         LA    R15,4                   SET COMPLETION CODE              00173801
         TM    FLAGS,$NOTPOPS                                           00173901
         BNZR  R14                                              GP09143 00174001
         IC    R15,FLAGS               LOAD FLAGS AS RC                 00174101
         BR    R14                                              GP09143 00174201
         SPACE 1                                                        00174301
RCPREV   L     R15,MYCPPL              GET CPPL BACK            GP09143 00174401
         USING CPPL,R15                                         GP09143 00174501
         L     R15,CPPLECT             LOAD ECT ADDRESS                 00174601
         USING ECT,R15                                                  00174701
         LH    R15,ECTRTCD+1           LOAD PREVIOUS RETURN CODE        00174801
         DROP  R15                                                      00174901
         BR    R14                                              GP09143 00175001
RCTDIFF  LR    R15,R4                  LOAD TOTAL TRACKS ALLOCATED      00175101
         SR    R15,R5                  SUBTRACTS TOTAL TRACKS USED      00175201
         BR    R14                                              GP09143 00175301
RCLDIFF  LR    R15,R6                  LOAD TRACKS ALLOCATED BY LAST DS 00175401
         SR    R15,R7                  SUBTRACT TRACKS USED BY LAST DS  00175501
         BR    R14                                              GP09143 00175601
         SPACE 1                                                        00175701
RCPCTUSE LTR   R15,R4                  ANY ALLOCATED ?          GP09143 00175801
         BNP   EXIT                    NONE; RETURN 0           GP09143 00175901
         LTR   R15,R5                  TRACKS USED              GP09143 00176001
         BNP   EXIT                    NONE; RETURN 0           GP09143 00176101
         M     R14,=F'100'             SCALE TO PERCENT         GP09143 00176201
         DR    R14,R4                  USED*100/ALLOC           GP09143 00176301
         B     EXIT                    RETURN TRUNCATED PERCENT GP09143 00176401
         SPACE 1                                                        00176501
RCPCLUSE LTR   R15,R6                  ANY ALLOCATED ?          GP09143 00176601
         BNP   EXIT                    NONE; RETURN 0           GP09143 00176701
         LTR   R15,R7                  TRACKS USED              GP09143 00176801
         BNP   EXIT                    NONE; RETURN 0           GP09143 00176901
         M     R14,=F'100'             SCALE TO PERCENT         GP09143 00177001
         DR    R14,R6                  USED*100/ALLOC           GP09143 00177101
         B     EXIT                    RETURN TRUNCATED PERCENT GP09143 00177201
         SPACE 1                                                        00177301
RC4      LA    R15,4                   SET RETURN CODE TO 4             00177401
*********************************************************************** 00177501
*                                                                     * 00177601
*        RELEASE STORAGE AND RETURN                                   * 00177701
*                                                                     * 00177801
*********************************************************************** 00177901
EXIT     LR    R4,R15                  SAVE RETURN CODE         GP09143 00178001
         IKJRLSA DSATANS               RELEASE PARSE STORAGE            00178101
         TM    FLAGS2,$HARDCPY         TEST FOR HARDCOPY OPTION         00178201
         BZ    EXIT2                                                    00178301
         LA    R1,HARDOPEN             LOAD OPEN LIST FORM ADDRESS      00178401
         LA    R2,HARDCPY              LOAD DCB ADDRESS                 00178501
         CLOSE ((R2)),MF=(E,(R1))      CLOSE HARDCPY DCB                00178601
EXIT2    L     R1,WORKAREA             LOAD WORK AREA ADDRESS           00178701
         L     R2,WORKLEN              LOAD WORK AREA LENGTH            00178801
         FREEMAIN R,LV=(R2),A=(R1),SP=1  FREE WORK AREA                 00178901
         LR    R1,R13                   LOAD STORAGE ADDRESS            00179001
         L     R3,4(,R13)               SAVE OLD SAVEAREA ADDR          00179101
   FREEMAIN R,LV=DSECTLEN,A=(1),SP=1                                    00179201
         LR    R15,R4                   RESTORE RETURN CODE             00179301
         LR    R13,R3                   RESTORE OLD SAVEAREA            00179401
         L     R14,12(,R13)             RESTORE RETURN ADDRESS          00179501
         LM    R0,R12,20(R13)           RESTORE REGS 0 - 12             00179601
         MVI   12(R13),X'FF'            SET RETURNED FLAG               00179701
         SPM   R14                      RESTORE PROGRAM MASK            00179801
         BR    R14                      RETURN                          00179901
*********************************************************************** 00180001
*        ERRORS                                                       * 00180101
*********************************************************************** 00180201
*--------ERROR IN PARSE ROUTINE                                         00180301
ERRPARS  MVC   OUTLINE(28),DSATERR1                                     00180401
         B     SETERROR                                                 00180501
*--------NO PREFIX, NO DSNAME, AND NO USERID (BATCH)                    00180601
NOID     MVC   OUTLINE(32),DSATERR7                                     00180701
         B     RLSA                                                     00180801
*--------TOO MANY OPERANDS - OUTPUT WILL EXCEED LINE LENGTH'            00180901
TOOLONG  MVC   OUTLINE,BLANKS                                           00181001
         MVC   OUTLINE(44),DSATERR4                                     00181101
RLSA     IKJRLSA DSATANS               FREE PARSE WORK AREA             00181201
*--------SET ERROR INDICATOR AND DISPLAY ERROR MESSAGE                  00181301
SETERROR OI    FLAGS,$NOTPOPS          INDICATE ERROR                   00181401
         BAL   R2,PUTL2                PUT MESSAGE                      00181501
         LA    R4,16                   SET CONDITION CODE       GP09143 00181601
         B     EXIT2                                                    00181701
*--------ERROR IN LOCATE MACRO                                          00181801
BASLOCER L     R1,ERRFLD               LOAD ERROR FIELD ADDRESS         00181901
         CL    R15,=A((LOCMSGND-LOCMSGS)/32*4)  KNOWN ERROR?    GP09143 00182001
         BH    BASLOCBD                                         GP09143 00182101
         SLA   R15,3                   MULTIPLY CODE BY 8       GP09143 00182201
         LA    R15,LOCMSGS-32(R15)     LOAD MESSAGE ADDRESS     GP09143 00182301
         MVC   0(32,R1),0(R15)         COPY MESSAGE TO OUTPUT   GP09143 00182401
         B     BASLOCMG                GO TO COMMON MESSAGE     GP09143 00182501
BASLOCBD MVC   0(32,R1),LOCMSGND       ISSUE UNSPECIFIED MSG    GP09143 00182601
         ST    R15,WORK                SAVE RETURN              GP09143 00182701
         UNPK  DSATDEC(9),WORK(5)      UNPACK                   GP09143 00182801
         TR    DSATDEC,TRTABLE         MAKE PRINTABLE           GP09143 00182901
         MVC   18(8,R1),DSATDEC        APPEND TO MESSAGE        GP09143 00183001
BASLOCMG OI    FLAGS,$NOTPOPS          INDICATE ERROR           GP09143 00183101
         B     BASPUTL                 PRINT IT                 GP09143 00183201
         SPACE 2                                                        00183301
         LTORG ,                                                GP09143 00183401
         TITLE 'D S A T  ***  FORMAT INFORMATION FOR ONE DS'    GP09143 00183501
*********************************************************************** 00183601
*                                                                     * 00183701
*   DATA SET PROCESSING - FORMAT ONE OR MORE (MULTI-VOLUME) LINES     * 00183801
*     VOLUME LIST (IN CATALOG FORMAT:  COUNT/TYPE/SERIAL/FILE/TYPE..) * 00183901
*     PASSED IN R8                                                    * 00184001
*   RETURNS R15 =  0    DATA SET INFORMATION COMPLETE                 * 00184101
*                  4    SKIP THIS DATA SET                            * 00184201
*                  8    SKIP THIS DATA SET GROUP                      * 00184301
*                 12    FATAL ERROR - QUIT PROGRAM                    * 00184401
*                                                                     * 00184501
*********************************************************************** 00184601
         PUSH  USING                                            GP09143 00184701
         DROP  R11,R12                                          GP09143 00184801
DSATFORM CSECT ,                                                GP09143 00184901
FORMINFO LA    R15,4                   PRESET FOR DS SKIP       GP09143 00185001
         STM   R0,R15,SAVEFORM         SAVE REGISTERS           GP09143 00185101
         BALR  R12,0                                            GP09143 00185201
         USING *,R12                                            GP09143 00185301
         MVC   HALF,0(R8)              VOLUME COUNT TO ALIGNED AREA     00185401
         LH    R1,HALF                 LOAD VOLUME COUNT        GP09143 00185501
         LA    R8,2(,R8)               BUMP POINTER TO FIRST VOL FIELD  00185601
         LTR   R1,R1                   VALID COUNT ?            GP09143 00185701
         BNP   FORMINFM                NO; SKIP MATCH CHECK     GP09143 00185801
         CH    R1,=H'20'               TEST AGAINST MAX COUNT   GP09143 00185901
         BNH   *+8                     OK                       GP09143 00186001
         LA    R1,20                   TRUNCATE                 GP09143 00186101
         TM    FLAGS2,$F2VMASK         MATCH SERIAL ?           GP09143 00186201
         BZ    FORMINFM                NO; DO ALL IN LIST       GP09143 00186301
         LA    R15,4(,R8)              POINT TO FIRST SERIAL    GP09143 00186401
FORMINFL CLC   CEREAL,0(R15)           MATCH ?                  GP09143 00186501
         BE    FORMINFM                YES; PROCESS ANY MATCH   GP09143 00186601
         LA    R15,12(,R15)            NEXT ENTRY               GP09143 00186701
         BCT   R1,FORMINFL             TRY AGAIN                GP09143 00186801
*PRESET* MVI   SAVEFORM+4*R15+3,4      SHOW SKIPPED BY VOL-SER  GP09143 00186901
         B     FORMINFX                SKIP THIS ENTRY          GP09143 00187001
         SPACE 1                                                GP09143 00187101
*********************************************************************** 00187201
*   LOOP THROUGH VOLUME LIST                                          * 00187301
*********************************************************************** 00187401
FORMINFM LH    R1,HALF                 LOAD VOLUME COUNT        GP09143 00187501
         CH    R1,=H'20'               TEST AGAINST MAX COUNT   GP09143 00187601
         BNH   FORMVLUP                                         GP09143 00187701
         LA    R1,20                   SET MAX COUNT            GP09143 00187801
FORMVLUP CH    R1,=H'1'                TEST VOLUME COUNT        GP09143 00187901
         BL    FORMBVCT                BAD VOLUME COUNT         GP09143 00188001
         BE    FORMSVCT                                         GP09143 00188101
         OI    FLAGS,$MULTVOL          INDICATE MULTIVOLUME     GP09143 00188201
FORMSVCT STH   R1,VOLCNT               STORE REMAINING VOLUME COUNT     00188301
         XC    VOLDSTRK,VOLDSTRK       ZERO TRACKS FOR VOLUME   GP09143 00188401
         MVC   DSATSER,4(R8)           COPY SERIAL              GP09143 00188501
         CLI   DSATSER,C'*'            SYSRES CONVENTION?       GP09127 00188601
         BNE   FORMNRES                NO                       GP09127 00188701
         L     R14,CVTPTR              GET CVT                  GP09127 00188801
         L     R14,CVTSYSAD-CVTMAP(,R14)  GET IPL UCB           GP09127 00188901
         MVC   DSATSER,UCBVOLI-UCBOB(R14)    USE IPL SERIAL     GP09127 00189001
FORMNRES CLC   SERIAL,=H'2'            TEST FOR NOSERIAL        GP09143 00189101
         BE    FORMNDEV                YES; SKIP DISPLAY        GP09143 00189201
         L     R2,SERFLD               LOAD OUTPUT AREA ADDRESS GP09143 00189301
         MVC   0(6,R2),DSATSER         COPY SERIAL TO OUTPUT    GP09143 00189401
         CLC   SEQNO,=H'2'             TEST FOR NOSEQNO         GP09143 00189501
         BE    FORMNDEV                                         GP09143 00189601
         MVC   HALF,10(R8)             SEQ NO TO ALIGNED AREA   GP09143 00189701
         L     R1,FULL                 LOAD FILE SEQUENCE NO.   GP09127 00189801
         CVD   R1,DSATPDEC             CONVERT TO DECIMAL       GP09143 00189901
         MVC   DSATDEC,=X'4020202020202120'   MOVE IN MASK      GP09143 00190001
         ED    DSATDEC,DSATPDEC+4      EDIT SEQUENCE NUMBER     GP09143 00190101
         MVC   7(4,R2),DSATDEC+4       MOVE TO MESSAGE          GP09143 00190201
FORMNDEV CLC   DEVTYPE,=H'2'           TEST FOR DEVTYPE         GP09143 00190301
         BE    FORMISDA                                         GP09143 00190401
         UNPK  DSATDEC(9),0(5,R8)      UNPACK DEVTYPE           GP09143 00190501
         TR    DSATDEC(8),TRTABLE      TRANSLATE TO EBCDIC      GP09143 00190601
         L     R2,DEVFLD               LOAD OUTPUT ADDRESS      GP09143 00190701
         MVC   0(8,R2),DSATDEC         MOVE TO MESSAGE          GP09143 00190801
FORMISDA CLI   2(R8),UCB3DACC          TEST FOR DIRECT ACCESS   GP09143 00190901
         BNE   FORMNTDA                                         GP09143 00191001
         LA    R0,DVCTYPMK             GET TYPE MASK            GP09143 00191101
         SR    R2,R2                   CLEAR REG 2              GP09143 00191201
         IC    R2,3(,R8)               LOAD DEVTYPE FIELD 4     GP09143 00191301
         CR    R2,R0                   IS IT VALID ?            GP09143 00191401
         BH    FORMNTDA                                         GP09143 00191501
         LTR   R3,R2                   AND NONZERO?             GP09143 00191601
         BZ    FORMNTDA                                         GP09143 00191701
         L     R1,CVTPTR               GET CVT                  GP09144 00191801
         L     R1,CVTZDTAB-CVTMAP(,R1)   GET DEVICE CHAR. TABLE GP09144 00191901
         IC    R2,DVCTIOFF-DVCTI(R2,R1)  GET DEVICE OFFSET      GP09144 00192001
         LTR   R2,R2                   IS IT VALID ?            GP09144 00192101
         BNZ   FORMGTRK                PROCESS IT               GP09143 00192201
*--------DEVICE CODE NOT IN CATALOG, GET IT FROM UCB            GP09143 00192301
         PUSH  USING                                            GP09143 00192401
         SR    R1,R1                   SIGNAL START OF LOOP     GP09143 00192501
FORMLUCB BAL   R14,LOOKUCB             GET NEXT DASD UCB        GP09143 00192601
         CH    R15,=H'4'               CHECK RETURN             GP09143 00192701
         BH    FORMNRDY                VOLUME NOT AVAILABLE     GP09143 00192801
         USING UCBOB,R1                                         GP09143 00192901
         CLC   DSATSER,UCBVOLI         COMPARE SERIAL           GP09143 00193001
         BNE   FORMLUCB                NO MATCH; TRY AGAIN      GP09143 00193101
         LA    R0,DVCTYPMK             GET TYPE MASK            GP09143 00193201
         N     R2,UCBTYP               MASK UCBTBYT4            GP09143 00193301
         BZ    FORMNTDA                                         GP09143 00193401
         LR    R3,R0                   SAVE FOR DEVICE NAME     GP09144 00193501
         POP   USING                                            GP09127 00193601
         L     R1,CVTPTR               GET CVT                  GP09144 00193701
         L     R1,CVTZDTAB-CVTMAP(,R1)   GET DEVICE CHAR. TABLE GP09144 00193801
         IC    R2,DVCTIOFF-DVCTI(R2,R1)  GET DEVICE OFFSET      GP09144 00193901
         LTR   R2,R2                   IS IT VALID ?            GP09144 00194001
         BZ    FORMNTDA                NO; SKIP                 GP09144 00194101
FORMGTRK LH    R0,DVCTRK-DVCT(R2,R1)   GET TRACKS PER CYLINDER  GP09144 00194201
         STH   R0,TRKPRCYL             SAVE IT                  GP09143 00194301
         LTR   R0,R0                   TEST IT                  GP09143 00194401
         BNZ   FORMTDV2                                         GP09143 00194501
FORMNRDY LA    R15,4                   VOL NOT MOUNTED - OBTAIN ERROR 4 00194601
         B     DSNERR                                                   00194701
FORMTDV2 CLC   DEVICE,=H'2'            TEST FOR DEVICE                  00194801
         BE    GETFMT1                                                  00194901
         MH    R3,=H'6'                DEVICE NAME LENGTH       GP09144 00195001
         LA    R15,DEVNAME(R3)         LOAD FIELD ADDRESS       GP09144 00195101
         L     R2,DEVFIELD             LOAD OUTPUT AREA ADDRESS         00195201
         MVC   0(6,R2),0(R15)          DEVICE NAME TO OUTPUT    GP09144 00195301
GETFMT1  OBTAIN DSATDCB1               GET FORMAT 1 DSCB                00195401
         LTR   R15,R15                 TEST RETURN CODE                 00195501
         BNZ   DSNERRF1                TEST FOR VOLUME SCREEN   GP09143 00195601
         L     R6,VOLDSTRK             TRACKS ON VOLUME         GP09143 00195701
         LA    R1,DS1EXT1              POINT TO FIRST EXTENT    GP09143 00195801
         LA    R0,3                    AT MOST 3 IN THIS FMT1   GP09143 00195901
         BAL   R14,TRACKNUM            COUNT TRACKS             GP09143 00196001
         ST    R6,VOLDSTRK             TRACKS ON VOL UPDATED    GP09143 00196101
         LA    R1,DS1PTRDS   FORMAT 1 LINK POINTER              GP08269 00196201
FMT3RTRY ST    R1,DSATDCB3+4                                            00196301
         CLC   DS1PTRDS-DS1PTRDS(5,R1),ZEROS     LINK ADDRESS?  GP09127 00196401
         BE    PROCESS                 NO; DONE                 GP09127 00196501
         OBTAIN DSATDCB3               GET THE FORMAT 3 DSCB            00196601
         LTR   R15,R15                 TEST RETURN CODE                 00196701
         BNZ   DSNERR                                                   00196801
         CLI   DS3FMTID,C'3'           EXTENT DESCRIPTOR?       GP08269 00196901
         BNE   F3RECURS                NO; READ AGAIN           GP09143 00197001
         SPACE 1                                                        00197101
         MVC   DS3EXTNT+4*10(9*10),DS3ADEXT   MAKE CONTIGUOUS   GP09143 00197201
         L     R6,VOLDSTRK             TRACKS IN DS             GP09143 00197301
         LA    R1,DS3EXTNT             POINT TO FIRST EXTENT    GP09143 00197401
         LA    R0,4+9                  AT MOST 13 IN THIS FMT3  GP09143 00197501
         BAL   R14,TRACKNUM            COUNT TRACKS             GP09143 00197601
         ST    R6,VOLDSTRK             TRACKS IN DS UPDATED     GP09143 00197701
F3RECURS LA    R1,DS3PTRDS             REALLY DS2?              GP08269 00197801
         B     FMT3RTRY                AND GET FMT3             GP08269 00197901
         SPACE 1                                                        00198001
*********************************************************************** 00198101
*                                                                     * 00198201
*        PROCESS THE DSCB                                             * 00198301
*                                                                     * 00198401
*********************************************************************** 00198501
PROCESS  OI    FLAGS,COUNT             COUNT THIS DATA SET              00198601
         OI    FLAGS2,$FOUND           INDICATE DATA SET FOUND          00198701
         MVI   SAVEFORM+4*15+3,0       SET GOOD RETURN          GP09143 00198801
         CLC   ALLOC,=H'2'             TEST FOR NOALLOC                 00198901
         BE    PROCDSRG                                                 00199001
         BAL   R14,$ALLOC              BRANCH TO PROCESSING ROUTINE     00199101
PROCDSRG CLC   DSORG,=H'2'             TEST FOR NODSORG                 00199201
         BE    PROCPDS                                                  00199301
         BAL   R14,$DSORG              BRANCH TO PROCESSING ROUTINE     00199401
PROCPDS  CLC   PDS,=H'2'               TEST FOR NOPDS                   00199501
         BE    PROCDCB                                                  00199601
         TM    DS1DSORG,X'02'          MAKE SURE DSORG = PO             00199701
         BZ    PROCDCB                                                  00199801
         L     R15,=A(DSATPDS)         LOAD SUBROUTINE ADDRESS  GP09144 00199901
         BALR  R14,R15                 BRANCH TO PROCESSING ROUTINE     00200001
PROCDCB  CLC   DCB,=H'2'               TEST FOR NODCB                   00200101
         BE    PROCCRDT                                                 00200201
         BAL   R14,$DCB                BRANCH TO PROCESSING ROUTINE     00200301
PROCCRDT CLC   CRDATE,=H'2'            TEST FOR NOCRDATE                00200401
         BE    PROCEXDT                                                 00200501
         BAL   R14,$CRDATE             BRANCH TO PROCESSING ROUTINE     00200601
PROCEXDT CLC   EXDATE,=H'2'            TEST FOR NOEXDATE                00200701
         BE    PROCLREF                                                 00200801
         BAL   R14,$EXDATE             BRANCH TO PROCESSING ROUTINE     00200901
PROCLREF CLC   LASTREF,=H'1'           TEST FOR LASTREF                 00201001
         BNE   PROCCHHR                                                 00201101
         BAL   R14,$LASTREF            BRANCH TO PROCESSING ROUTINE     00201201
PROCCHHR CLC   CCHHR,=H'2'             TEST FOR NOCCHHR                 00201301
         BE    FORMLPUT                                         GP09143 00201401
         BAL   R14,$CCHHR              BRANCH TO PROCESSING ROUTINE     00201501
         SPACE 1                                                GP09143 00201601
FORMLPUT BAL   R2,PUTL                 DISPLAY INFORMATION      GP09143 00201701
FORMRSET MVC   OUTLINE,BLANKS          BLANK OUTPUT LINE        GP09143 00201801
         LA    R8,12(,R8)              INCREMENT TO NEXT VOLUME GP09143 00201901
         LH    R1,VOLCNT               LOAD VOLUME COUNT        GP09143 00202001
         BCT   R1,FORMVLUP             LOOP FOR MORE VOLUMES    GP09143 00202101
*********************************************************************** 00202201
*        ACCUMULATE RUN TOTALS FROM DS VALUES                         * 00202301
*********************************************************************** 00202401
         LM    R4,R8,TOTALRUN          LOAD TOTALS FIELDS               00202501
         AR    R4,R6                   ACCUMULATE TOTAL TRACKS ALLOC    00202601
         AR    R5,R7                   ACCUMULATE TOTAL TRACKS USED     00202701
         TM    FLAGS,COUNT             COUNT IT?                        00202801
         BZ    FORMSTOT                                                 00202901
         NI    FLAGS,X'FF'-COUNT       RESET COUNT BIT                  00203001
         LA    R8,1(,R8)               INCREMENT DATA SET COUNT         00203101
FORMSTOT STM   R4,R8,TOTALRUN          STORE TOTALS                     00203201
FORMINFX MVC   OUTLINE,BLANKS          BLANK OUTPUT LINE        GP09143 00203301
         LM    R0,R15,SAVEFORM         RELOAD REGISTERS AND RC  GP09143 00203401
         BR    R14                     RETURN TO CALLER         GP09143 00203501
         SPACE 1                                                GP09143 00203601
*--------VOLUME COUNT IS NEGATIVE OR ZERO                       GP09143 00203701
FORMBVCT L     R1,ERRFLD               LOAD ERROR FIELD ADDRESS GP09143 00203801
         MVC   0(32,R1),DSATERR5       MOVE IN ERROR MESSAGE    GP09143 00203901
         MVC   VOLCNT,=H'1'            SET COUNT TO 1           GP09143 00204001
         OI    FLAGS,$NOTPOPS          SET ERROR INDICATOR      GP09143 00204101
         B     FORMLPUT                DISPLAY MESSAGE; RC=8    GP09143 00204201
         SPACE 1                                                GP09143 00204301
*--------NOT A DIRECT ACCESS DEVICE                                     00204401
FORMNTDA CLC   DAONLY,=H'2'            SEE IF MESSAGE TO BE SUPPRESSED  00204501
         BE    FORMRSET                RESET; DO NEXT VOL       GP09143 00204601
         OI    FLAGS,COUNT+$NOTPOPS    COUNT THIS DATA SET, ERROR       00204701
         L     R1,ERRFLD               LOAD ERROR FIELD ADDRESS GP09143 00204801
         MVC   0(32,R1),DSATERR2       COPY MESSAGE TO OUTPUT LINE      00204901
         B     FORMLPUT                PRINT THE LINE           GP09143 00205001
         SPACE 2                                                        00205101
*********************************************************************** 00205201
*                                                                     * 00205301
*   UCB LOOKUP ROUTINE                                                * 00205401
*   SR R1,R1                 START SCAN THROUGH UCBS                  * 00205501
*   BAL R14,LOOKUCB          GET NEXT UCB                             * 00205601
*   ... R15=0       UCB ADDRESS IN R1                                 * 00205701
*           4       UCB ADDRESS IN R1, BUT DEVICE UNAVAILABLE (NRDY)  * 00205801
*           8       END OF UCB CHAIN                                  * 00205901
*                                                                     * 00206001
*********************************************************************** 00206101
         PUSH  USING                                            GP09143 00206201
LOOKUCB  LA    R15,8                   PRESET FOR END OF UCBS   GP09143 00206301
         STM   R0,R15,SAVE2            SAVE REGISTERS           GP09143 00206401
         LTR   R1,R1                   NEW SCAN ?               GP09143 00206501
         BNZ   LOOKUCB1                NO; CONTINUE             GP09143 00206601
         XC    SERPTR,SERPTR           ZERO SERIAL POINTER      GP09143 00206701
         XC    LASTUCB,LASTUCB         ZERO LAST UCB PROCESSED  GP09143 00206801
         L     R2,CVTPTR               LOAD CVT ADDRESS         GP09143 00206901
         L     R2,CVTILK2-CVTMAP(,R2)  LOAD UCB TABLE ADDRESS   GP09143 00207001
         B     LOOKUCB2                START LOOKUP             GP09143 00207101
LOOKUCB1 L     R2,SERPTR               LOAD PREVIOUS ENTRY      GP09143 00207201
LOOKUCBI LA    R2,2(,R2)               INCREMENT POINTER        GP09143 00207301
LOOKUCB2 LH    R1,0(,R2)               LOAD UCB ADDRESS         GP09143 00207401
         N     R1,=X'0000FFFF'         CLEAR SIGN EXTENSION     GP09143 00207501
         BZ    LOOKUCBI                EMPTY - TRY AGAIN        GP09143 00207601
         CL    R1,=X'0000FFFF'         TEST FOR END OF TABLE    GP09143 00207701
         BE    LOOKUCBX                GET OUT; R15=8           GP09143 00207801
         ST    R1,LASTUCB              STORE LAST UCB USED      GP09143 00207901
         USING UCBOB,R1                  MAP                    GP09143 00208001
         CLI   UCBTBYT3,UCB3DACC       TEST FOR DIRECT ACCESS   GP09143 00208101
         BNE   LOOKUCBI                  BUMP                   GP09143 00208201
         CLI   UCBVOLI,X'00'           TEST FOR ANY SERIAL      GP09143 00208301
         BE    LOOKUCBI                  BUMP IF NONE           GP09143 00208401
         C     R1,LASTUCB              SEE IF ALTERNATE PATH    GP09143 00208501
         BNH   LOOKUCBI                  YES; DONE ALREADY      GP09143 00208601
         ST    R2,SERPTR               STORE UCB TABLE POINTER  GP09143 00208701
         ST    R1,SAVE2+4                AND RETURN TO USER     GP09143 00208801
         MVI   SAVE2+15*4+3,4          CHANGE RETURN CODE TO 4  GP09143 00208901
         TM    UCBFLA,UCBNRY           TEST FOR NOT READY       GP09143 00209001
         BO    LOOKUCBX                  NOT READY              GP09143 00209101
         TM    UCBJBNR,UCBOLDSM+UCBBOX   AVAILABLE?             GP09143 00209201
         BNZ   LOOKUCBX                MAYBE, BUT NOT NOW       GP09143 00209301
         MVI   SAVE2+15*4+3,0          SET GOOD RETURN          GP09143 00209401
LOOKUCBX LM    R0,R15,SAVE2            RESTORE REGISTERS        GP09143 00209501
         BR    R14                     RETURN TO CALLER         GP09143 00209601
         POP   USING                                            GP09143 00209701
         SPACE 2                                                        00209801
*********************************************************************** 00209901
*   CALCULATE TRACKS IN ONE DSCB                                      * 00210001
*     R1 - FIRST IN GROUP OF EXTENTS   R0 - NUMBER TO PROCESS         * 00210101
*********************************************************************** 00210201
TRACKNUM STM   R7,R5,SAVE2             SAVE ALL REGISTERS EXC 6 GP09143 00210301
         LTR   R0,R0                   SANITY CHECK             GP09143 00210401
         BNP   TRACKNUX                TOO BAD - RETURN         GP09143 00210501
TRACKNXT CLI   0(R1),X'00'                                      GP09143 00210601
         BE    TRACKNUX                                         GP09143 00210701
         CLI   0(R1),X'80'             SPLIT CYLINDER ALLOC?    GP08269 00210801
         BE    TRACKSPT                YES; DIFFERNT CALC       GP08269 00210901
         MVC   HALF(2),6(R1)           LOAD ENDING CYL NUMBER   GP09143 00211001
         L     R3,FULL                                          GP09127 00211101
         MVC   HALF(2),2(R1)           LOAD STARTING CYL NUMBER GP09143 00211201
         S     R3,FULL                 SUBTRACT STARTING FROM ENDING    00211301
         MH    R3,TRKPRCYL             MULTIPLY BY TRACKS PER CYL       00211401
         MVC   HALF(2),8(R1)           LOAD ENDING TRACK        GP09143 00211501
         A     R3,FULL                 ADD TO TRACK COUNT       GP09127 00211601
         MVC   HALF(2),4(R1)           LOAD STARTING TRACK      GP09143 00211701
         S     R3,FULL                 SUBTRACT FROM TRACK CNT  GP09127 00211801
         AH    R3,=H'1'                TOTAL TRACKS IN  EXTENT  GP08269 00211901
         B     TRACKCOM                JOIN COMMON              GP08269 00212001
TRACKSPT MVC   HALF(2),6(R1)           LOAD ENDING CYL NUMBER   GP08269 00212101
         L     R15,FULL                                         GP08269 00212201
         MVC   HALF(2),2(R1)           LOAD STARTING CYL NUMBER GP08269 00212301
         S     R15,FULL                 SUBTRACT ENDING         GP08269 00212401
         AH    R15,=H'1'                RELATIVITY              GP08269 00212501
         MVC   HALF(2),8(R1)           LOAD ENDING TRACK        GP08269 00212601
         L     R3,FULL                 GET END TRACK            GP08269 00212701
         MVC   HALF(2),4(R1)           LOAD STARTING TRACK      GP08269 00212801
         S     R3,FULL                 SUBTRACT                 GP08269 00212901
         AH    R3,=H'1'                TOTAL TRACKS IN CYLINDER GP08269 00213001
         MR    R0,R15                  TRACKS IN SPLIT EXTENT   GP08269 00213101
TRACKCOM AR    R6,R3                   ACCUMULATE TOTAL         GP09143 00213201
         LA    R1,10(,R1)              NEXT EXTENT              GP09143 00213301
         BCT   R0,TRACKNXT             DECREMENT EXTENT COUNT   GP09143 00213401
TRACKNUX LM    R7,R5,SAVE2             UPDATED TRACK COUNT IN 6 GP09143 00213501
         BR    R14                     RETURN TO CALLER         GP09143 00213601
         SPACE 1                                                        00213701
*********************************************************************** 00213801
*                                                                     * 00213901
*        GET ALLOCATION  (ALLOCATED, USED, AND EXTENTS)               * 00214001
*                                                                     * 00214101
*********************************************************************** 00214201
*                                                                     * 00214301
*        R2        - OUTPUT AREA ADDRESS                              * 00214401
*        R3        - RESERVED (DSNAME PDE POINTER)                    * 00214501
*        R6        - TRACKS ALLOCATED                                 * 00214601
*        R7        - EXTENT DESCRIPTION POINTER/TRACKS USED           * 00214701
*        R8 - R13  - RESERVED (POINTERS AND BASE REGISTERS)           * 00214801
*                                                                     * 00214901
*********************************************************************** 00215001
*--------GET TRACK ALLOCATION                                           00215101
$ALLOC   L     R2,ALLOCFLD             LOAD OUTPUT AREA ADDRESS         00215201
         L     R5,VOLDSTRK             TRACKS ON VOLUME         GP09143 00215301
         CVD   R5,DSATPDEC             CONVERT TRACKS TO DECIMAL        00215401
         MVC   DSATDEC,=X'4020202020202120'      MOVE MASK              00215501
         ED    DSATDEC,DSATPDEC+4      EDIT                             00215601
         MVC   0(5,R2),DSATDEC+3       MOVE INTO MESSAGE                00215701
         L     R6,TOTALDS              GET TRACKS ALLOCATED     GP09143 00215801
         AR    R6,R5                   UPDATE DS TOTAL          GP09143 00215901
         ST    R6,TOTALDS              SET TRACKS ALLOCATED     GP09143 00216001
*--------GET TRACKS USED                                                00216101
         SR    R7,R7                                            GP09144 00216201
         CLC   DS1LSTAR,ZEROS          EVER USED ?              GP09143 00216301
         BE    DSATXFRM                NO; LEAVE 0              GP09143 00216401
         LR    R7,R5         PROVISIONALLY SET USED=ALLOCATED   GP09143 00216501
         TM    DS1SMSFG,DS1PDSE+DS1PDSEX+DS1STRP  FUNNY NEW ?   GP09143 00216601
         BNZ   DSATXFRM                YES; FULLY USED          GP09143 00216701
         TM    DS1DSORG+1,0            DSORG=VSAM OR ?          GP09186 00216801
         BNZ   DSATXFRM                YES; SKIP                GP09186 00216901
         TM    DS1DSORG,X'42'          DSORG=PS OR PO ?         GP09143 00217001
         BZ    DSATXFRM                NO; USED = ALLOC         GP09143 00217101
DSATXSOM LH    R7,DS1LSTAR             LOAD TRACKS USED                 00217201
         N     R7,=X'0000FFFF'         BIG DISKS?               GP09143 00217301
         LA    R7,1(,R7)               ADD 1 FOR RELATIVITY     GP09143 00217401
DSATXFRM CR    R5,R7                   ALLOC < USED ?           GP09143 00217501
         BNH   *+6                     NO                       GP09143 00217601
         LR    R5,R7                   USE LOWER (BAD FMT1?)    GP09143 00217701
         CVD   R5,DSATPDEC             CONVERT TO DECIMAL       GP09143 00217801
         A     R7,TOTALDS+4            LOAD PRIOR LSTAR         GP09143 00217901
         ST    R7,TOTALDS+4            SAVE FOR RUN TOTALS      GP09143 00218001
         MVC   DSATDEC,=X'4020202020202120'  MOVE MASK                  00218101
         ED    DSATDEC,DSATPDEC+4      EDIT                             00218201
         MVC   6(5,R2),DSATDEC+3       MOVE INTO MESSAGE                00218301
*--------GET NUMBER OF EXTENTS                                          00218401
DSATXNTS SR    R1,R1                                                    00218501
         IC    R1,DS1NOEPV             LOAD EXTENTS             GP08269 00218601
         CVD   R1,DSATPDEC             CONVERT TO DECIMAL               00218701
         MVC   DSATDEC,=X'4020202020202120'  MOVE MASK                  00218801
         ED    DSATDEC,DSATPDEC+4      EDIT                             00218901
         MVC   11(3,R2),DSATDEC+5      MOVE INTO MESSAGE        GP09143 00219001
ALLOCEND CLC   SECOND,=H'2'            TEST FOR NOSECONDARY             00219101
         BE    NOSECOND                                                 00219201
         LA    R2,15(,R2)              INCREMENT FIELD POINTER          00219301
         TM    DS1SCALO,X'C0'          TEST ALLOCATION TYPE             00219401
         BM    TRKALLOC                                                 00219501
         BO    CYLALLOC                                                 00219601
         MVC   5(5,R2),=C'ABSTR'       ABSOLUTE TRACK ALLOCATION        00219701
         B     SECAMT                                                   00219801
CYLALLOC MVC   5(5,R2),=C' CYL '       CYLINDER ALLOCATION              00219901
         B     SECAMT                                                   00220001
TRKALLOC TM    DS1SCALO,X'80'          TEST FOR TRACKS OR BLOCKS        00220101
         BZ    BLKALLOC                                                 00220201
         MVC   5(5,R2),=C' TRK '       TRACK ALLOCATION                 00220301
         B     SECAMT                                                   00220401
BLKALLOC MVC   5(5,R2),=C'BLOCK'       BLOCK ALLOCATION                 00220501
SECAMT   MVC   WORK,DS1SCALO           COPY TO ALIGNED (F) WORK AREA    00220601
         L     R1,WORK                 LOAD ALLOCATION WORD             00220701
         N     R1,=X'00FFFFFF'         ZERO OUT HIGH BYTE       GP09143 00220801
         CVD   R1,DSATPDEC             CONVERT TO DECIMAL               00220901
         MVC   DSATDEC,=X'4020202020202120'  MOVE IN MASK               00221001
         ED    DSATDEC,DSATPDEC+4      EDIT                             00221101
         MVC   0(4,R2),DSATDEC+4                                        00221201
NOSECOND BR    R14                     RETURN                           00221301
*********************************************************************** 00221401
*                                                                     * 00221501
*        GET DSORG                                                    * 00221601
*                                                                     * 00221701
*********************************************************************** 00221801
$DSORG   L     R2,DSORGFLD             LOAD OUTPUT AREA ADDRESS         00221901
         TM    FLAGS2,$VSAMLOC         SEE IF VSAM CATALOG ENTRY        00222001
         BZ    DSORG001                                                 00222101
         L     R1,CURRBLK              LOAD CURRENT ENTRY ADDRESSS      00222201
         MVC   0(1,R2),0(R1)           COPY TYPE CODE                   00222301
         MVI   1(R2),C'-'                                               00222401
         LA    R2,1(,R2)               INCREMENT OVER TYPE CODE         00222501
DSORG001 LA    R1,DSORGTBL             LOAD DSORG TABLE ADDRESS         00222601
         LA    R4,4                    LOAD LENGTH OF ENTRY             00222701
         LA    R5,DSORGEND             LOAD END OF TABLE ADDRESS        00222801
         TM    DS1DSORG,X'01'          TEST FOR UNMOVABLE               00222901
         BZ    DSORGCLC                                                 00223001
         MVI   3(R2),C'U'              INDICATE UMMOVABLE               00223101
         XI    DS1DSORG,X'01'          TURN OFF UNMOVABLE BIT           00223201
DSORGCLC CLC   DS1DSORG,0(R1)          TEST DSORG CODE                  00223301
         BE    DSORGMVC                                                 00223401
         BXLE  R1,R4,DSORGCLC                                           00223501
         OI    FLAGS,$NOTPOPS          FALL THROUGH TO **       GP09144 00223601
DSORGMVC MVC   1(2,R2),2(R1)           COPY DSORG CODE                  00223701
         CLC   1(2,R2),=C'PO'          TEST FOR PDS                     00223801
         BE    DSORGPO                                                  00223901
         CLC   1(2,R2),=C'PS'          TEST FOR SEQUENTIAL              00224001
         BE    DSORGPS                                                  00224101
         B     DSORGRTN                                                 00224201
DSORGPO  OI    FLAGS,$DSPO             PARTITIONED                      00224301
         B     DSORGRTN                                                 00224401
DSORGPS  OI    FLAGS,$DSPS             SEQUENTIAL                       00224501
DSORGRTN BR    R14                     RETURN                           00224601
*********************************************************************** 00224701
*                                                                     * 00224801
*        GET DCB ATTRIBUTES                                           * 00224901
*                                                                     * 00225001
*********************************************************************** 00225101
$DCB     LM    R15,R1,=A(TABREC,2,TABRECN)  POINT TO RECFMs     GP09144 00225201
         L     R2,DCBFLD           LOAD OUTPUT AREA ADDRESS     GP09144 00225301
         LR    R5,R2               FOR RECFM UPDATE             GP09144 00225401
         MVC   WORK(1),DS1RECFM    MAKE WORKING COPY            GP09144 00225501
         SR    R3,R3                 FOR IC                     GP09144 00225601
         MVI   0(R2),C'?'          IN CASE NOT SPECIFIED        GP09144 00225701
         TM    DS1RECFM,X'C0'      ANY OF F, U, V ?             GP09144 00225801
         BZ    RECFMLUP            YES; ELSE ALLOW D            GP09144 00225901
         AR    R15,R0              SKIP RECFM=D ENTRY           GP09144 00226001
RECFMLUP IC    R3,0(,R15)          GET TEST BIT                 GP09144 00226101
         EX    R3,EXRECFTM         SEE IF BIT(S) ON             GP09144 00226201
         BNO   RECFMLNX            NO; TRY NEXT                 GP09144 00226301
         MVC   0(1,R5),1(R15)      MOVE RECFM CHAR              GP09144 00226401
         LA    R5,1(,R5)           ADVANCE                      GP09144 00226501
         EX    R3,EXRECFXI         TURN BIT(S) OFF              GP09144 00226601
RECFMLNX BXLE  R15,R0,RECFMLUP     TRY NEXT                     GP09144 00226701
         SPACE 1                                                GP09144 00226801
*--------GET BLOCKSIZE                                                  00226901
DSATBLK  LH    R1,DS1BLKL              LOAD BLOCK SIZE          GP09127 00227001
         N     R1,=X'0000FFFF'     SUPPORT DOS/VSE SIZES        GP09127 00227101
         CVD   R1,DSATPDEC             CONVERT TO DECIMAL               00227201
         MVC   DSATDEC,=X'4020202020202120'  MOVE MASK                  00227301
         ED    DSATDEC,DSATPDEC+4      EDIT                             00227401
         MVC   11(5,R2),DSATDEC+3      MOVE INTO MESSAGE        GP09143 00227501
*--------GET LRECL                                                      00227601
         SR    R1,R1                   CLEAR FOR INSERT         GP09127 00227701
         ICM   R1,3,DS1LRECL           LOAD LRECL               GP09127 00227801
         CLM   R1,3,=X'8000'           LARGE SPANNED ?          GP09127 00227901
         BNE   DSATLR#                 NO                       GP09127 00228001
         TM    DS1RECFM,X'80'          F OR U RATHER THAN V?    GP09127 00228101
         BNZ   DSATLR#                 YES; NOT SPANNED         GP09127 00228201
         TM    DS1RECFM,X'48'          V(B)S ?                  GP09127 00228301
         BNO   DSATLR#                 NO; NOT SPANNED          GP09127 00228401
         MVI   6(R2),C'X'              INDICATE IT              GP09143 00228501
         BR    R14                     RETURN                   GP08269 00228601
DSATLR#  CVD   R1,DSATPDEC             CONVERT TO DECIMAL               00228701
         MVC   DSATDEC,=X'4020202020202120'  MOVE MASK                  00228801
         ED    DSATDEC,DSATPDEC+4      EDIT                             00228901
         MVC   5(5,R2),DSATDEC+3       MOVE INTO MESSAGE        GP09143 00229001
         BR    R14                     RETURN                           00229101
EXRECFTM TM    WORK,*-*            TEST BIT ON                  GP09144 00229201
EXRECFXI XI    WORK,*-*            TURN BIT OFF                 GP09144 00229301
*********************************************************************** 00229401
*                                                                     * 00229501
*        CREATION DATE                                                * 00229601
*                                                                     * 00229701
*********************************************************************** 00229801
$CRDATE  L     R1,DS1CREDT-1           LOAD CREATION DATE               00229901
         L     R2,CRDATFLD             LOAD OUTPUT AREA ADDRESS         00230001
         BAL   R5,DATECONV             CONVERT IT                       00230101
         BR    R14                     RETURN                           00230201
*********************************************************************** 00230301
*                                                                     * 00230401
*        EXPIRATION DATE                                              * 00230501
*                                                                     * 00230601
*********************************************************************** 00230701
$EXDATE  L     R1,DS1EXPDT             LOAD EXPIRATION DATE             00230801
         SRL   R1,8                    SHIFT RIGHT 1 BYTE       GP09143 00230901
         L     R2,EXDATFLD             LOAD OUTPUT AREA ADDRESS         00231001
         BAL   R5,DATECONV             CONVERT IT                       00231101
         BR    R14                     RETURN                           00231201
*********************************************************************** 00231301
*                                                                     * 00231401
*        LAST DATE REFERENCED - MVS SU 60 OR EQUIVALENT REQUIRED      * 00231501
*                                                                     * 00231601
*********************************************************************** 00231701
$LASTREF MVC   DSATDEC(4),DS1REFD-1    MOVE LAST DATE REF TO FULLWORD   00231801
         L     R1,DSATDEC              LOAD DATE LAST REFERENCED        00231901
         L     R2,LREFFLD              LOAD OUTPUT AREA ADDRESS         00232001
         BAL   R5,DATECONV             CONVERT IT                       00232101
         TM    DS1DSIND,X'02'          SEE IF UPDATE INDICATOR ON       00232201
         BZ    REFRTRN                                                  00232301
         MVI   8(R2),C'*'              INDICATE UPDATE BIT ON           00232401
REFRTRN  BR    R14                     RETURN                           00232501
*********************************************************************** 00232601
*                                                                     * 00232701
*        DATE CONVERSION ROUTINE                                      * 00232801
*                                                                     * 00232901
*             THIS SECTION WAS ADAPTED FROM AN INSTALLATION           * 00233001
*             DATE CONVERSION ROUTINE TO AVOID HAVING MANY            * 00233101
*             DIFFERENT DATE CONVERSION ROUTINES.  IT IS              * 00233201
*             NECESSARY TO CONVERT THE DATE TO PACKED DECIMAL         * 00233301
*             FORM (AS IS THE DATE RETURNED BY TIME) TO               * 00233401
*             CONFORM TO THIS ROUTINE.                                * 00233501
*                                                                     * 00233601
*********************************************************************** 00233701
DATECONV N     R1,=X'00FFFFFF'         ZERO HIGH BYTE           GP09144 00233801
         BZR   R5                      ZERO DATE - LEAVE BLANK  GP09144 00233901
         ST    R1,DSATPDEC             SAVE FOR HEX DISPLAY     GP09144 00234001
         LR    R15,R1                  COPY                     GP09144 00234101
         SRL   R15,16                  ISOLATE YEAR             GP09144 00234201
         N     R1,=X'0000FFFF'         ISOLATE JULIAN DAYS      GP09144 00234301
         BZ    DATEBAD                 INVALID DAY = 0          GP09144 00234401
         CH    R1,=H'366'              NOT TOO HIGH ?           GP09144 00234501
         BH    DATEBAD                 TOO BAD                  GP09144 00234601
         EX    R15,EXISLEAP            IS THIS A LEAP YEAR?     GP09144 00234701
         BZ    DATENADJ                YES; DON'T ADJUST        GP09144 00234801
         CH    R1,=H'59'               PAST POSSIBLE LEAP DAY?  GP09144 00234901
         BNH   DATENADJ                NO; NO NON-LEAP ADJUST   GP09144 00235001
         LA    R1,1(,R1)               FINAGLE A BIT            GP09144 00235101
DATENADJ LA    R6,DATETABL             POINT TO DECEMBER        GP09144 00235201
         LA    R3,12                   FOR DECEMBER             GP09144 00235301
DATENLUP CH    R1,0(,R6)               LARGER THAN ENTRY?       GP09144 00235401
         BH    DATEFND                 YES                      GP09144 00235501
         LA    R6,2(,R6)               NEXT TABLE ENTRY         GP09144 00235601
         BCT   R3,DATENLUP             TRY AGAIN                GP09144 00235701
         B     DATEBAD                                          GP09144 00235801
DATEFND  SH    R1,0(,R6)               DAYS IN MONTH            GP09144 00235901
         AH    R15,=H'1900'            MAKE CENTURY (4 LATER)   GP09144 00236001
         MH    R15,=H'100'             SCALE                    GP09144 00236101
         AR    R3,R15                  YYMM                     GP09144 00236201
         MH    R3,=H'100'              CHEAT                    GP09144 00236301
         AR    R1,R3                   YYMMDD                   GP09144 00236401
         CVD   R1,DSATDEC              MAKE PACKED              GP09144 00236501
         OI    DSATDEC+7,X'0F'         POSITIVE POSITIVE        GP09144 00236601
         UNPK  DATEWORK(8),DSATDEC+4(4)  MAKE PRINTABLE         GP09144 00236701
         MVC   DATEWORK+8(2),=C'-/'    MAKE DATE SEPARATORS     GP09144 00236801
         LA    R1,DATEISO              SET FOR ISO DATE         GP09144 00236901
         CLC   DATEFORM,=H'2'          IS IT ISO YY-MM-DD?      GP09144 00237001
         BL    DATEEDIT                YES                      GP09144 00237101
         LA    R1,DATEUSA              IS IT USA MM/DD/YY?      GP09144 00237201
         BE    DATEEDIT                YES                      GP09144 00237301
         LA    R1,DATEOTH              ELSE USE DD/MM/YY        GP09144 00237401
DATEEDIT MVC   0(8,R2),0(R1)           MOVE DATE PATTERN        GP09144 00237501
         TR    0(8,R2),DATEWORK        FORMAT                   GP09144 00237601
         BR    R5                      RETURN TO CALLER         GP09144 00237701
         SPACE 1                                                        00237801
EXISLEAP TM    =X'03',*-*              TEST (BINARY) FOR LEAP   GP09144 00237901
DATEISO  DC    AL1(2,3,8,4,5,8,6,7)    GAP FOR CENTURY ?        GP09144 00238001
DATEUSA  DC    AL1(4,5,9,6,7,9,2,3)    MM/DD/YY                 GP09144 00238101
DATEOTH  DC    AL1(6,7,9,4,5,9,2,3)    DD/MM/YY                 GP09144 00238201
DATETABL DC    H'335,305,274,244,213,182,152,121,91,60,31,0'    GP09144 00238301
         SPACE 1                                                        00238401
*--------DATE INVALID, DISPLAY IT IN HEX                                00238501
DATEBAD  UNPK  DSATDEC(7),DSATPDEC+1(4)  UNPACK INVALID DATE FIELD      00238601
         TR    DSATDEC(6),TRTABLE      TRANSLATE TO EBCDIC              00238701
         MVC   1(6,R2),DSATDEC         MOVE IN HEX DATE VALUE   GP09144 00238801
         BR    R5                      RETURN                           00238901
         SPACE 1                                                        00239001
*********************************************************************** 00239101
*                                                                     * 00239201
*        CCHHR PROCESSING                                             * 00239301
*                                                                     * 00239401
*********************************************************************** 00239501
$CCHHR   UNPK  DSATDEC(11),DSATFMT1+96(6)  UNPACK CCHHR PLUS ONE BYTE   00239601
         TR    DSATDEC(10),TRTABLE     TRANSLATE TO CHARACTER           00239701
         L     R2,CCHHRFLD             LOAD OUTPUT AREA ADDRESS         00239801
         MVC   0(10,R2),DSATDEC        COPY CCHHR TO OUTPUT LINE        00239901
         BR    R14                     RETURN                           00240001
         SPACE 1                                                        00240101
*********************************************************************** 00240201
*                                                                     * 00240301
*        ERRORS                                                       * 00240401
*                                                                     * 00240501
*********************************************************************** 00240601
*--------ERROR IN LOCATE MACRO                                          00240701
LOCERROR L     R1,ERRFLD               LOAD ERROR FIELD ADDRESS         00240801
         CL    R15,=A((LOCMSGND-LOCMSGS)/32*4)  KNOWN ERROR?    GP09143 00240901
         BH    LOCERRBD                                         GP09143 00241001
         SLA   R15,3                   MULTIPLY RETURN CODE BY 8        00241101
         LA    R15,LOCMSGS-32(R15)     LOAD MESSAGE ADDRESS             00241201
         MVC   0(32,R1),0(R15)         COPY MESSAGE TO OUTPUT LINE      00241301
         B     LOCERRMG                GO TO COMMON MESSAGE     GP09143 00241401
LOCERRBD MVC   0(32,R1),LOCMSGND       ISSUE UNSPECIFIED MSG    GP09143 00241501
         ST    R15,WORK                SAVE RETURN              GP09143 00241601
         UNPK  DSATDEC(9),WORK(5)      UNPACK                   GP09143 00241701
         TR    DSATDEC,TRTABLE         MAKE PRINTABLE           GP09143 00241801
         MVC   18(8,R1),DSATDEC        APPEND TO MESSAGE        GP09143 00241901
LOCERRMG OI    FLAGS,$NOTPOPS          INDICATE ERROR                   00242001
         B     FORMLPUT                PRINT IT AND TERM DS     GP09143 00242101
         SPACE 1                                                        00242201
*--------ERROR IN OBTAIN MACRO                                          00242301
DSNERRF1 DS    0H            OBTAIN DSCB1 FAILED                GP09143 00242401
DSNERR   CH    R15,=H'8'               TEST RETURN CODE                 00242501
         BH    DSNERR1                                                  00242601
         BE    TESTVALL                                                 00242701
         CLC   DAONLY,=H'2'            SEE IF MESSAGE TO BE SUPRESSED   00242801
         BE    FORMRSET                                                 00242901
         OI    FLAGS,COUNT             COUNT THIS DATA SET              00243001
         B     DSNERR1                                                  00243101
TESTVALL TM    FLAGS2,$VSAMLOC         SUPER-LOCATE BEING USED? GP09143 00243201
         BZ    DSNERR1                 NO; NO INFO              GP09143 00243301
         L     R7,CURRBLK              CURRENT ENTRY            GP09143 00243401
         CLI   0(R7),C'A'              NON-VSAM ENTRY ?         GP09127 00243501
         BE    DSNERR1                 YES; REALLY NOT FOUND    GP09127 00243601
         TM    FLAGS2,$F2VANY+$F2VMASK  USER SPECIFIED VOLUME?  GP09143 00243701
         BNZ   FORMRSET                YES - NO ERROR MSG       GP09143 00243801
         L     R2,ERRFLD               GET ERROR FIELD          GP09127 00243901
         LM    R15,R1,=A(CATCOMP,CATCOMP2-CATCOMP,CATCOMPN)     GP09127 00244001
DSNELOOP CLC   0(1,R7),0(R15)          MATCH ?                  GP09127 00244101
         BE    DSNECATF                YES; DISPLAY MESSAGE     GP09127 00244201
         BXLE  R15,R0,DSNELOOP         TRY AGAIN                GP09127 00244301
         MVC   CATCOMP2-CATCOMP(1,R2),0(R7)  SHOW ENTRY TYPE    GP09127 00244401
DSNECATF OI    FLAGS2,$F2ENTY          SHOW SOMETHING FOUND     GP12018 00244501
         MVC   0(CATCOMP2-CATCOMP-1,R2),1(R15)   IDENTIFY       GP09127 00244601
         MVC   VOLCNT,=H'1'            DON'T DO MORE SERIALS    GP09143 00244701
         B     FORMLPUT                AND PRINT                GP09127 00244801
         SPACE 1                                                GP09127 00244901
DSNERR1  OI    FLAGS,$NOTPOPS          INDICATE ERROR                   00245001
         SLA   R15,3                   MULTIPLY RETURN CODE BY 8        00245101
         LA    R15,DSNERRS-32(R15)     LOAD PROPER MESSAGE ADDRESS      00245201
         L     R1,ERRFLD               LOAD ERROR FIELD ADDRESS         00245301
         MVC   0(32,R1),0(R15)         COPY MESSAGE TO OUTPUT LINE      00245401
         B     FORMLPUT                PRINT IT                         00245501
         POP   USING                                            GP09143 00245601
*********************************************************************** 00245701
*                                                                     * 00245801
*        CONSTANT DATA                                                * 00245901
*                                                                     * 00246001
*********************************************************************** 00246101
         LTORG                                                          00246201
*********************************************************************** 00246301
*        DEVICE NAME TABLE                                            * 00246401
*********************************************************************** 00246501
DEVNAME  DC    CL6' '                  00 (00) - UNASSIGNED             00246601
         DC    CL6'2311'               01 (01) - 2311 DISK              00246701
         DC    CL6'2301'               02 (02) - 2301 DRUM              00246801
         DC    CL6'2303'               03 (03) - 2303 DRUM              00246901
*OLD*    DC    CL6'2302'               04 (04) - 2302 DISK FILE         00247001
         DC    CL6'9345'               04 (04) - 9345           GP08269 00247101
         DC    CL6'2312' UNSUPPORTED   05 (05) - 2321 DATA CELL         00247201
         DC    CL6'2305-1'             06 (06) - 2305-1                 00247301
         DC    CL6'2305-2'             07 (07) - 2305-2                 00247401
         DC    CL6'2314'               08 (08) - 2314 DISK              00247501
         DC    CL6'3330'               09 (09) - 3330 DISK              00247601
         DC    CL6'3340'               10 (0A) - 3340           GP08269 00247701
         DC    CL6'3350'               11 (0B) - 3350                   00247801
         DC    CL6'3375'               12 (0C) - 3375           GP08269 00247901
         DC    CL6'3330-1'             13 (0D) - 3330-11 DISK           00248001
         DC    CL6'3380'               14 (0E) - 3380           GP08269 00248101
         DC    CL6'3390'               15 (0F) - 3390           GP08269 00248201
         SPACE 1                                                        00248301
*********************************************************************** 00248401
*        DATA SET ORGANIZATION TABLE                                  * 00248501
*          RE-ORDERED BY (ESTIMATED) FREQUENCY                  GP09143 00248601
*********************************************************************** 00248701
DSORGTBL DC    X'0200',C'PO'           PARTITIONED ORGANIZATION         00248801
         DC    X'4000',C'PS'           PHYSICAL SEQUENTIAL              00248901
         DC    X'0008',C'AM'           VSAM                             00249001
         DC    X'2000',C'DA'           DIRECT ACCESS                    00249101
         DC    X'0080',C'GS'           GRAPHICS ?               GP09144 00249201
         DC    X'0040',C'TX'           TCAM LINE GROUP          GP09144 00249301
         DC    X'0020',C'TQ'           TCAM MSG QUEUE           GP09144 00249401
         DC    X'0004',C'TR'           TCAM ?                   GP09144 00249501
         DC    X'1000',C'CX'           BTAM/QTAM LINE GROUP     GP09144 00249601
DSORGEND DC    X'8000',C'IS'           INDEXED SEQUENTIAL               00249701
         DC    X'0000',C'**'           NO MATCH                 GP09144 00249801
         SPACE 1                                                        00249901
*********************************************************************** 00250001
*    RECORD FORMAT TABLE (NOTE DUAL DEFINITION OF X'20' BIT)          * 00250101
*********************************************************************** 00250201
TABREC   DC    X'20',C'D'          RECFM=D                              00250301
TABRECU  DC    X'C0',C'U'          RECFM=U                              00250401
         DC    X'80',C'F'          RECFM=F                              00250501
         DC    X'40',C'V'          RECFM=V                              00250601
         DC    X'20',C'T'          RECFM=.T                             00250701
         DC    X'10',C'B'          RECFM=.B                             00250801
         DC    X'08',C'S'          RECFM=.S                             00250901
         DC    X'04',C'A'          RECFM=..A                            00251001
TABRECN  DC    X'02',C'M'          RECFM=..M                            00251101
         TITLE 'D S A T  ***  GET PDS DIRECTORY STATISTICS'     GP09143 00251201
*********************************************************************** 00251301
*                                                                     * 00251401
*        PDS DIRECTORY PROCESSING SUBROUTINE                          * 00251501
*                                                                     * 00251601
*        CALLED FROM DSAT TO DYNAMICALLY ALLOCATE A PDS, READ THE     * 00251701
*        DIRECTORY AND COUNT DIRECTORY BLOCKS AND ENTRIES.            * 00251801
*                                                                     * 00251901
*        THIS SUBROUTINE IS ONLY CALLED WHEN THE PDS OPTION IS        * 00252001
*        REQUESTED AND A PDS IS ENCOUNTERED.                          * 00252101
*                                                                     * 00252201
*        LINKAGE CONVENTIONS - THIS SUBROUTINE DEPENDS ON THE         * 00252301
*            PREVIOUS SAVE AREA FOR ADDRESSABILITY TO THE WORK        * 00252401
*            AREA.  SINCE THE ENTIRE WORK AREA IS AVAILABLE, NO       * 00252501
*            PARAMETERS ARE PASSED.                                   * 00252601
*                                                                     * 00252701
*            THE VALUES IN PDSTOTLS ARE CLEARED BY THE DSAT           * 00252801
*            MAINLINE CODE AND ARE RELIED UPON TO CONTAIN ZEROS.      * 00252901
*                                                                     * 00253001
*        IF AN ERROR IS DETECTED, THE PDS DATA FIELD IS USED FOR      * 00253101
*        AN ERROR MESSAGE.                                            * 00253201
*            RC=     RETURN CODE FROM DAIR (REGISTER 15)              * 00253301
*            DARC=   DYNAMIC ALLOCATION RETURN CODE (DAXXDARC FIELD)  * 00253401
*            CTRC=   CATALOG RETURN CODE (DAXXCTRC FIELD)             * 00253501
*                                                                     * 00253601
*            SEE GC28-6764 GUIDE TO WRITING A TMP OR CP   (MVT, SVS)  * 00253701
*             OR GC28-0627 OS/VS2 SPL: JOB MANAGMENT      (MVS)       * 00253801
*             FOR DYNAMIC ALLOCATION RETURN CODE DESCRIPTIONS.        * 00253901
*                                                                     * 00254001
*********************************************************************** 00254101
         PUSH  USING                                            GP09143 00254201
         DROP  R11,R12                                          GP09143 00254301
DSATPDS  CSECT                                                          00254401
         B     BEGPDS-DSATPDS(,R15)     BRANCH AROUND ID        GP08269 00254501
         DC    AL1(9),CL9'DSATPDS'      IDENTIFIER              GP08269 00254601
BEGPDS   STM   R14,R12,12(R13)          SAVE REGISTERS          GP08269 00254701
         LR    R12,R15                  LOAD BASE ADDRESS               00254801
         USING DSATPDS,R12              DEFINE BASE REGISTERS           00254901
         LA    R15,15                   LOAD PGM MASK SETTING           00255001
         SLA   R15,24                   SHIFT TO BITS 4-7               00255101
         SPM   R15                      SET PGM MASK AND COND           00255201
         LR    R15,R13                  SAVE OLD SAVEAREA ADDR          00255301
         LA    R13,SAVE2                LOAD NEW SAVEAREA ADDR          00255401
         ST    R15,4(,R13)              CHAIN SAVE AREAS                00255501
         ST    R13,8(,R15)                                              00255601
*********************************************************************** 00255701
*                                                                     * 00255801
*              SET UP TSO CONTROL BLOCKS                              * 00255901
*                                                                     * 00256001
*********************************************************************** 00256101
         L     R11,4(,R13)             LOAD WORK AREA ADDRESS           00256201
         USING DSATSAVE,R11            DSECT ADDRESSABILITY             00256301
         DROP  R13                     I DON'T WANT IT TO USE 13        00256401
         LA    R10,DSATDAPL            LOAD DAPL ADDRESS                00256501
         USING DAPL,R10                DAPL ADDRESSABILITY              00256601
*--------SET UP DAPB'S                                                  00256701
         LA    R9,DA18DAPB             LOAD FREE DAPB ADDRESS           00256801
         USING DAPB18,R9               DEFINE BASE REGISTER             00256901
         XC    DAPB18(DA18LEN),DAPB18  ZERO OUT CONTROL BLOCK           00257001
         MVC   DA18CD,=X'0018'         MOVE IN BLOCK CODE               00257101
         XC    DA18PDSN,DA18PDSN       ZERO DSNAME ADDRESS POINTER      00257201
         MVC   DA18DDN,=CL8'DSATDDN'   MOVE IN DDNAME                   00257301
         MVC   DA18MNM,=CL8' '         BLANK OUT MEMBER NAME            00257401
         MVC   DA18SCLS,=CL2' '        BLANK OUT SYSOUT CLASS           00257501
         MVI   DA18DPS2,DA18KEEP       DISP=KEEP                        00257601
         MVI   DA18CTL,DA18PERM                                         00257701
         MVC   DA18JBNM,=CL8' '        BLANK OUT MEMBER NAME            00257801
         LA    R9,DA08DAPB             LOAD ALLOCATE DAPB ADDRESS       00257901
         USING DAPB08,R9               DEFINE BASE REGISTER             00258001
         XC    DAPB08(DA08LEN),DAPB08  ZERO OUT CONTROL BLOCK           00258101
         MVC   DA08CD,=X'0008'         MOVE IN BLOCK CODE               00258201
         LA    R1,DSNLEN               LOAD DSNAME FIELD ADDRESS        00258301
         ST    R1,DA08PDSN             SAVE IT                          00258401
         MVC   DA08DDN,=CL8'DSATDDN'   MOVE IN DDNAME                   00258501
         MVC   DA08UNIT,=CL8' '        BLANK OUT UNIT NAME              00258601
         MVC   DA08SER,=CL8' '         BLANK OUT SERIAL FIELD           00258701
         MVC   DA08SER(6),DSATSER      COPY IN VOLUME SERIAL            00258801
         MVC   DA08MNM,=CL8' '         BLANK OUT MEMBER NAME            00258901
         MVC   DA08PSWD,=CL8' '        BLANK OUT PASSWORD               00259001
         MVI   DA08DSP1,DA08SHR        DISP=(SHR,KEEP,KEEP)             00259101
         MVI   DA08DPS2,DA08KEEP                                        00259201
         MVI   DA08DPS3,DA08KEP                                         00259301
         MVC   DA08ALN,=CL8' '         BLANK OUT ATTRIBUTE LIST NAME    00259401
         DROP  R9                                                       00259501
*--------GET DSNAME LENGTH                                              00259601
         LA    R1,DSNAME-1             LOAD START ADDR - 1              00259701
         LA    R2,44                   LOAD MAXIMUM DSNANME LEN         00259801
PDSEND   LA    R15,0(R2,R1)            LOAD ADDR OF CHAR                00259901
         CLI   0(R15),C' '             TEST FOR BLANK                   00260001
         BNE   PDSSAVE                                                  00260101
         BCT   R2,PDSEND               DECREMENT COUNTER                00260201
PDSSAVE  STH   R2,DSNLEN               SAVE LENGTH                      00260301
*--------SET UP OPEN LIST FORM                                          00260401
         MVC   OPENLIST(OPENLEN),MODLOPEN                               00260501
*********************************************************************** 00260601
*                                                                     * 00260701
*              ALLOCATE THE DATA SET                                  * 00260801
*                                                                     * 00260901
*********************************************************************** 00261001
*--------SET UP ALLOCATE DAPB                                           00261101
SETUPDDN LA    R9,DA08DAPB             LOAD ALLOCATE DAPB ADDRESS       00261201
         USING DAPB08,R9               ADDRESSABILITY FOR DSORG TEST    00261301
         ST    R9,DAPLDAPB             STORE DAPB ADDRESS IN DAPL       00261401
         LA    R1,DAPL                 LOAD DAPL ADDRESS                00261501
         LINK  EP=IKJDAIR              LINK TO DAIR                     00261601
         LTR   R15,R15                 TEST RETURN CODE                 00261701
         BZ    TESTORG                                                  00261801
         C     R15,=F'20'              TEST FOR FILE IN USE             00261901
         BNE   ERRDAIR                                                  00262001
         LA    R1,DA18DAPB             LOAD FREE DAPB ADDRESS           00262101
         ST    R1,DAPLDAPB             STORE IN DAPL                    00262201
         LA    R1,DAPL                 LOAD DAPL ADDRESS                00262301
         LINK  EP=IKJDAIR              FREE THE FILE                    00262401
         LTR   R15,R15                 TEST RETURN CODE                 00262501
         BNZ   ERRDAIR                                                  00262601
         LA    R1,DA08DAPB             LOAD ALLOCATE DAPB ADDRESS       00262701
         ST    R1,DAPLDAPB             STORE IN DAPL                    00262801
         LA    R1,DAPL                 LOAD DAPL ADDRESS                00262901
         LINK  EP=IKJDAIR              FREE THE FILE                    00263001
         LTR   R15,R15                 TEST RETURN CODE                 00263101
         BNZ   ERRDAIR                                                  00263201
*********************************************************************** 00263301
*                                                                     * 00263401
*              BE SURE DSORG PO                                       * 00263501
*                                                                     * 00263601
*********************************************************************** 00263701
TESTORG  TM    DA08DSO,X'02'           TEST FOR PARTITIONED             00263801
         BZ    ERRDSORG                                                 00263901
*--------INITIALIZE DCB                                                 00264001
         MVC   DSATDCB(DCBLEN),PSDCB   COPY DCB                         00264101
*********************************************************************** 00264201
*                                                                     * 00264301
*              OPEN DATA SET                                          * 00264401
*                                                                     * 00264501
*********************************************************************** 00264601
*--------OPEN THE DATA SET                                              00264701
         LA    R1,OPENLIST             LOAD ADDRESS OF OPEN LIST FORM   00264801
         LA    R2,DSATDCB              LOAD DCB ADDRESS                 00264901
         OPEN  ((R2)),MF=(E,(R1))      OPEN THE DATA SET                00265001
         TM    DSATDCB+48,X'10'        SEE IF IT OPENED                 00265101
         BZ    ERROPEN                                                  00265201
*********************************************************************** 00265301
*                                                                     * 00265401
*        READ DIRECTORY AND COUNT BLOCKS AND ENTRIES                  * 00265501
*                                                                     * 00265601
*********************************************************************** 00265701
         LM    R2,R5,PDSTOTLS          ZERO TOTALS                      00265801
NEXTBLK  READ  DIRDECB,SF,DSATDCB,PDSREC,MF=E  READ DIR BLOCK           00265901
         CHECK DIRDECB                 WAIT FOR COMPLETION OF READ      00266001
         LA    R6,PDSREC               LOAD RECORD ADDRESS              00266101
         LA    R2,1(,R2)               COUNT BLOCK                      00266201
         LH    R0,8(,R6)               LOAD COUNT                       00266301
         SH    R0,=H'2'                LESS 2 FOR COUNT FIELD           00266401
         LA    R6,10(,R6)              BUMP PAST KEY AND COUNT          00266501
NEXTENT  LTR   R0,R0                   SEE IF ANY LEFT IN BLOCK         00266601
         BNP   NEXTBLK                                                  00266701
         CLC   ENDMEMB,0(R6)           TEST FOR END OF ENTRIES          00266801
         BNE   PROCENTR                                                 00266901
         LR    R3,R2                   SET USED = TOTAL                 00267001
         B     NEXTBLK                                                  00267101
PROCENTR LA    R4,1(,R4)               COUNT ENTRY                      00267201
         TM    11(R6),X'80'            TEST FOR ALIAS                   00267301
         BZ    DIRLEN                                                   00267401
         LA    R5,1(,R5)               COUNT ALIAS                      00267501
DIRLEN   SR    R1,R1                                                    00267601
         NI    11(R6),X'1F'            ZERO INDICATOR BITS              00267701
         IC    R1,11(,R6)              LOAD LENGTH                      00267801
         SLL   R1,1                    DOUBLE TO GET LENGTH IN BYTES    00267901
         LA    R1,12(,R1)              ADD FIXED FIELD LENGTH           00268001
         AR    R6,R1                   INCREMENT POINTER                00268101
         SR    R0,R1                   DECREMENT BYTE COUNT             00268201
         B     NEXTENT                                                  00268301
*--------END OF DATA                                                    00268401
DIREOD   STM   R2,R5,PDSTOTLS          SAVE TOTALS                      00268501
         L     R1,PDSFLD               LOAD OUTPUT AREA ADDRESS         00268601
         CVD   R2,DSATPDEC             CONVERT BLOCKS USED              00268701
         MVC   DSATDEC,=X'4020202020202120'  MOVE IN MASK               00268801
         ED    DSATDEC,DSATPDEC+4      EDIT                             00268901
         MVC   00(4,R1),DSATDEC+4      MOVE INTO MESSAGE                00269001
         CVD   R3,DSATPDEC             CONVERT BLOCKS ALLOCATED         00269101
         MVC   DSATDEC,=X'4020202020202120'  MOVE IN MASK               00269201
         ED    DSATDEC,DSATPDEC+4      EDIT                             00269301
         MVC   05(4,R1),DSATDEC+4      MOVE INTO MESSAGE                00269401
         CVD   R4,DSATPDEC             CONVERT DIRECTORY ENTRIES        00269501
         MVC   DSATDEC,=X'4020202020202120'  MOVE IN MASK               00269601
         ED    DSATDEC,DSATPDEC+4      EDIT                             00269701
         MVC   10(4,R1),DSATDEC+4      MOVE INTO MESSAGE                00269801
         CVD   R5,DSATPDEC             CONVERT ALIASES                  00269901
         MVC   DSATDEC,=X'4020202020202120'  MOVE IN MASK               00270001
         ED    DSATDEC,DSATPDEC+4      EDIT                             00270101
         MVC   15(4,R1),DSATDEC+4      MOVE INTO MESSAGE                00270201
*--------CLOSE THE DATA SET                                             00270301
CLOSE    LA    R1,OPENLIST             LOAD ADDRESS OF OPEN LIST FORM   00270401
         LA    R2,DSATDCB              LOAD DCB ADDRESS                 00270501
*        CLOSE ((R2)),MF=(E,(R1))      THIS RELEASES FREE SPACE         00270601
         IC    14,0(,R1)                         SAVE OPTION BYTE       00270701
         ST    R2,0(,R1)                         STORE DCB ADDR IN LIST 00270801
         STC   14,0(,R1)                         RESTORE OPTION BYTE    00270901
         SVC   20                                ISSUE CLOSE SVC        00271001
*--------FREE THE DATA SET                                              00271101
FREE     LA    R1,DA18DAPB             LOAD FREE DAPB ADDRESS           00271201
         ST    R1,DAPLDAPB             STORE DAPB ADDRESS IN DAPL       00271301
         LA    R1,DAPL                 LOAD DAPL ADDRESS                00271401
*        LINK  EP=IKJDAIR              LINK TO DAIR                     00271501
         CNOP  0,4                                                      00271601
         BAL   15,*+20            BRANCH AROUND CONSTANTS               00271701
         DC    A(*+8)             ADDR. OF PARM. LIST                   00271801
         DC    A(0)                DCB ADDRESS PARAMETER                00271901
         DC    CL8'IKJDAIR'        EP PARAMETER                         00272001
         SVC   6                   ISSUE LINK SVC                       00272101
         B     PDSRTRN                                                  00272201
*--------ERROR IN DYNAMIC ALLOCATION                                    00272301
ERRDAIR  L     R2,PDSFLD               ADDRESS OF ERROR MESSAGE FIELD   00272401
         L     R1,DAPLDAPB             LOAD DAPB ADDRESS                00272501
         USING DAPB08,R1               ADDRESSABILITY                   00272601
         C     R15,=F'12'              TEST FOR RETURN CODE 12          00272701
         BNE   CTLGERR                                                  00272801
         CLC   DA08DARC,=X'0210'       TEST FOR DATA SET IN USE         00272901
         BNE   DYNALCER                                                 00273001
         MVC   0(16,R2),=CL16' DATA SET IN USE'                         00273101
         B     PDSRTRN                                                  00273201
DYNALCER UNPK  DSATDEC(5),DA08DARC(3)  TRANSLATE TO EBCDIC              00273301
         TR    DSATDEC(4),TRTABLE2                                      00273401
         MVC   0(14,R2),=CL14'RC=000C, DARC='                           00273501
         MVC   14(4,R2),DSATDEC        MOVE IN RETURN CODE              00273601
         B     PDSRTRN                                                  00273701
CTLGERR  C     R15,=F'8'               TEST FOR CATALOG ERROR           00273801
         BNE   OTHERERR                                                 00273901
         UNPK  DSATDEC(5),DA08CTRC(3)  TRANSLATE TO EBCDIC              00274001
         TR    DSATDEC(4),TRTABLE2                                      00274101
         MVC   0(14,R2),=CL14'RC=0008, CTRC='                           00274201
         MVC   14(4,R2),DSATDEC        MOVE IN ERROR CODE               00274301
         B     PDSRTRN                                                  00274401
OTHERERR ST    R15,DSATPDEC            STORE RETURN CODE                00274501
         UNPK  DSATDEC(5),DSATPDEC+2(3)                                 00274601
         MVC   0(14,R2),=CL14'DAIR ERROR RC='                           00274701
         MVC   14(4,R2),DSATDEC                                         00274801
         B     PDSRTRN                                                  00274901
*--------OPEN ERROR                                                     00275001
ERROPEN  L     R2,PDSFLD               LOAD ERROR FIELD ADDRESS         00275101
         MVC   0(18,R2),=CL18'UNABLE TO OPEN DIR'                       00275201
         B     PDSRTRN                                                  00275301
*--------DATA SET NOT PARTITIONED - THIS IS A PROGRAM LOGIC ERROR       00275401
ERRDSORG L     R2,PDSFLD               LOAD ERROR FIELD ADDRESS         00275501
         MVC   0(18,R2),=CL18'DATA SET NOT A PDS'                       00275601
         B     PDSRTRN                                                  00275701
*********************************************************************** 00275801
*                                                                     * 00275901
*              END OF ROUTINE                                         * 00276001
*                                                                     * 00276101
*********************************************************************** 00276201
PDSRTRN  L     R13,4(,R13)              RESTORE OLD SAVEAREA            00276301
         LA    R15,0                    SET RETURN CODE                 00276401
         L     R14,12(,R13)             RESTORE RETURN ADDRESS          00276501
         LM    R0,R12,20(R13)           RESTORE REGS 0 - 12             00276601
         MVI   12(R13),X'FF'            SET RETURNED FLAG               00276701
         SPM   R14                      RESTORE PROGRAM MASK            00276801
         BR    R14                      RETURN                          00276901
*********************************************************************** 00277001
*                                                                     * 00277101
*              CONSTANTS                                              * 00277201
*                                                                     * 00277301
*********************************************************************** 00277401
ENDMEMB  DC    XL8'FFFFFFFFFFFFFFFF'    LAST DIRECTORY ENTRY            00277501
PSDCB    DCB   DDNAME=DSATDDN,                                         +00277601
               DSORG=PS,                                               +00277701
               KEYLEN=8,                                               +00277801
               RECFM=F,                                                +00277901
               BLKSIZE=256,                                            +00278001
               EODAD=DIREOD,                                           +00278101
               MACRF=R                                                  00278201
DCBLEN   EQU   *-PSDCB                                                  00278301
MODLOPEN OPEN  (,),MF=L                                                 00278401
OPENLEN  EQU   *-MODLOPEN                                               00278501
TRTABLE2 EQU   *-X'F0'                                                  00278601
         DC    C'0123456789ABCDEF'                                      00278701
*********************************************************************** 00278801
*                                                                     * 00278901
*              LITERAL POOL                                           * 00279001
*                                                                     * 00279101
*********************************************************************** 00279201
         LTORG                                                          00279301
         TITLE 'D S A T  ***  DATA DEFINITIONS'                 GP09143 00279401
*********************************************************************** 00279501
*                                                                     * 00279601
*   STATIC DATA, PARSE CONTROL BLOCK, AND COMMON SUBROUTINES          * 00279701
*                                                                     * 00279801
*********************************************************************** 00279901
         POP   USING                                            GP09143 00280001
DSATDATA CSECT ,                                                GP09143 00280101
         DROP  R11,R12                                          GP09143 00280201
         USING DSATDATA,R10            USE FIXED ADDRESS        GP09143 00280301
*********************************************************************** 00280401
*                                                                     * 00280501
*        DISPLAY LINE                                                 * 00280601
*                                                                     * 00280701
*********************************************************************** 00280801
PUTL     CLC   PRINT,=H'2'             TEST FOR NOPRINT                 00280901
         BE    PUTLHCPY                                                 00281001
PUTL2    PUTLINE OUTPUT=(DSATHEDR,,,DATA),MF=(E,DSATIOPL)               00281101
PUTLHCPY TM    FLAGS2,$HARDCPY         TEST FOR HARDCOPY                00281201
         BZ    PUTLBACK                                                 00281301
         PUT   HARDCPY,OUTLINE         PRINT LINE                       00281401
PUTLBACK BR    R2                      RETURN                           00281501
         LTORG ,                                                GP09143 00281601
         SPACE 1                                                GP09127 00281701
CATCOMP  DC    C'C',CL16'VSAM Cluster'                          GP09127 00281801
CATCOMP2 DC    C'D',CL16'VSAM Data   '                          GP09127 00281901
         DC    C'I',CL16'VSAM Index  '                          GP09127 00282001
         DC    C'G',CL16'VSAM Alt. Index'                       GP09127 00282101
CATBASE  DC    C'B',CL16'GDG Base    '                          GP09127 00282201
         DC    C'P',CL16'Page Space  '                          GP09127 00282301
         DC    C'R',CL16'Path Entry  '                          GP09127 00282401
         DC    C'U',CL16'User Catalog'                          GP09127 00282501
         DC    C'M',CL16'Master Catalog'                        GP09127 00282601
         DC    C'X',CL16'Data Set Alias'                        GP09127 00282701
CATCOMPN DC    C'Y',CL16'Upgrade Set '                          GP09127 00282801
         DC    C' ',CL16'Catlg Entry '    N+1                   GP09127 00282901
NAME     CAMLST NAME,0,,0                                               00283001
NAMECVOL CAMLST NAME,0,0,0                                              00283101
TTRLOC   CAMLST BLOCK,0,0,0                                             00283201
SEARCH   CAMLST SEARCH,0,0,0                                            00283301
SEEK     CAMLST SEEK,0,0,0                                              00283401
DIGITMSK EQU   *-X'F0'                                                  00283501
ZEROS    DC    X'00000000000000000000FFFFFFFFFFFF' MASK FOR DETECTING   00283601
*                                                  INVALID DEC DIGITS   00283701
TRTABLE  EQU   *-X'F0'                                                  00283801
         DC    C'0123456789ABCDEF'     HEX TO EBCDIC TRANSLATE TABLE    00283901
MOVENAME MVC   0(0,R2),0(R4)                                            00284001
MODLPTPB PUTLINE MF=L                                                   00284101
ENDPTPB  EQU   *                                                        00284201
HARDDCB  DCB   DDNAME=X,               WILL BE REPLACED AT EXEC TIME   +00284301
               MACRF=PM,                                               +00284401
               DSORG=PS,                                               +00284501
               RECFM=FB,                                               +00284601
               LRECL=120,                                              +00284701
               BLKSIZE=3000                                             00284801
HARDLEN  EQU   *-HARDDCB                                                00284901
         SPACE 1                                                        00285001
MODLINE  DC    H'124,0'                                                 00285101
BLANKS   DC    CL120' '                                                 00285201
GDGMSG   DC    CL40'GDG -- OPTIONS=NONE CURRENT=000 MAX=000'            00285301
MSGNULL  DC    C'NO ENTRIES IN CATALOG',C' FOR'                 GP12018 00285401
DSATERR1 DC    CL28'UNABLE TO INTERPRET COMMAND'                        00285501
DSATERR2 DC    CL32'UNSUPPORTED DEVICE TYPE         '                   00285601
DSATERR3 DC    CL32'TOO MANY NAMES - LIST INCOMPLETE'                   00285701
DSATERR4 DC    CL44'TOO MUCH OUTPUT REQUESTED - LINE EXCEEDED'          00285801
DSATERR5 DC    CL32'VOLUME COUNT IS ZERO OR NEGATIVE'                   00285901
DSATERR6 DC    CL48'UNABLE TO OPEN HARDCOPY DCB - HARDCOPY CANCELLED'   00286001
DSATERR7 DC    CL32'NO PREFIX, NO DSNAME, NO USERID '                   00286101
EMPTYNDX DC    CL32'EMPTY INDEX STRUCTURE           '                   00286201
EMPTYGDG DC    CL32'EMPTY GENERATION INDEX STRUCTURE'                   00286301
NOVOL    DC    CL32'NOT ON ANY MOUNTED VOLUME       '                   00286401
DSNERRS  DC    CL32'VOLUME NOT MOUNTED              '  ORC04            00286501
         DC    CL32'NOT ON VOLUME AS                '  ORC08    GP09143 00286601
         DC    CL32'VTOC I/O ERR OR BAD DSCB        '  ORC12    GP09127 00286701
         DC    CL32'INVALID WORK AREA POINTER       '  ORC16    GP09127 00286801
         DC    CL32'CCHHR NOT IN VTOC               '  ORC20    GP09127 00286901
LOCMSGS  DC    CL32'CATALOG VOLUME NOT MOUNTED      '  LRC04            00287001
         DC    CL32'NAME NOT FOUND                  '  LRC08            00287101
         DC    CL32'INDEX NAME SPECIFIED            '  LRC12            00287201
         DC    CL32'DATA SET AT LOWER LEVEL         '  LRC16            00287301
         DC    CL32'SYNTAX ERROR IN DSNAME          '  LRC20            00287401
         DC    CL32'PERMANENT CATALOG I/O ERROR     '  LRC24            00287501
         DC    CL32'TTR OUT OF SYSCTLG              '  LRC28            00287601
         DC    CL32'INVALID WORK AREA POINTER       '  LRC32            00287701
         DC    CL32'USER CATALOG NOT ALLOCATED      '  LRC36            00287801
         DC    CL32'WORK AREA TOO SMALL      CODE 40'  LRC40            00287901
         DC    CL32'WORK AREA TOO SMALL      CODE 44'  LRC44            00288001
         DC    CL32'INVALID OPERATION-RETURN CODE 48'  LRC48            00288101
         DC    CL32'I/O ERROR ON USER VOLUME-CODE 52'  LRC52            00288201
         DC    CL32'PASSWORD NOT CORRECT            '  LRC56            00288301
         DC    CL32'STEPCAT/JOBCAT NOT OPEN         '  LRC60            00288401
LOCMSGND DC    CL32'LOCATE error: R15=xxxxxxxx      '   other   GP09143 00288501
         SPACE 1                                                        00288601
DSATPCL  IKJPARM  DSECT=DSATPDL                                         00288701
DSN      IKJPOSIT DSNAME,LIST                                           00288801
SERIAL   IKJKEYWD DEFAULT='SERIAL'                                      00288901
         IKJNAME  'SERIAL'                                              00289001
         IKJNAME  'NOSERIAL'                                            00289101
ALLOC    IKJKEYWD DEFAULT='ALLOC'                                       00289201
         IKJNAME  'ALLOC'                                               00289301
         IKJNAME  'NOALLOC'                                             00289401
SECOND   IKJKEYWD DEFAULT='NOSECONDARY'                                 00289501
         IKJNAME  'SECONDARY'                                           00289601
         IKJNAME  'NOSECONDARY'                                         00289701
DSORG    IKJKEYWD DEFAULT='DSORG'                                       00289801
         IKJNAME  'DSORG'                                               00289901
         IKJNAME  'NODSORG'                                             00290001
DCB      IKJKEYWD DEFAULT='DCB'                                         00290101
         IKJNAME  'DCB'                                                 00290201
         IKJNAME  'NODCB'                                               00290301
CRDATE   IKJKEYWD DEFAULT='CRDATE'                                      00290401
         IKJNAME  'CRDATE'                                              00290501
         IKJNAME  'NOCRDATE'                                            00290601
EXDATE   IKJKEYWD DEFAULT='NOEXDATE'                                    00290701
         IKJNAME  'EXDATE'                                              00290801
         IKJNAME  'NOEXDATE'                                            00290901
LASTREF  IKJKEYWD DEFAULT='NOLASTREF'                                   00291001
         IKJNAME  'LASTREF'                                             00291101
         IKJNAME  'NOLASTREF'                                           00291201
TOTALS   IKJKEYWD DEFAULT='TOTALS'                                      00291301
         IKJNAME  'TOTALS'                                              00291401
         IKJNAME  'NOTOTALS'                                            00291501
PRINT    IKJKEYWD DEFAULT='PRINT'                                       00291601
         IKJNAME  'PRINT'                                               00291701
         IKJNAME  'NOPRINT'                                             00291801
HEADER   IKJKEYWD DEFAULT='HEADER'                                      00291901
         IKJNAME  'HEADER'                                              00292001
         IKJNAME  'NOHEADER'                                            00292101
DAONLY   IKJKEYWD DEFAULT='ALL'                                         00292201
         IKJNAME  'ALL'                                                 00292301
         IKJNAME  'DAONLY'                                              00292401
GDGDATA  IKJKEYWD DEFAULT='NOGDGDATA'                                   00292501
         IKJNAME  'GDGDATA'                                             00292601
         IKJNAME  'NOGDGDATA'                                           00292701
SEQNO    IKJKEYWD DEFAULT='NOSEQNO'                                     00292801
         IKJNAME  'SEQNO'                                               00292901
         IKJNAME  'NOSEQNO'                                             00293001
DEVTYPE  IKJKEYWD DEFAULT='NODEVTYPE'                                   00293101
         IKJNAME  'DEVTYPE'                                             00293201
         IKJNAME  'NODEVTYPE'                                           00293301
DEVICE   IKJKEYWD DEFAULT='NODEVICE'                                    00293401
         IKJNAME  'DEVICE'                                              00293501
         IKJNAME  'NODEVICE'                                            00293601
CCHHR    IKJKEYWD DEFAULT='NOCCHHR'                                     00293701
         IKJNAME  'CCHHR'                                               00293801
         IKJNAME  'NOCCHHR'                                             00293901
PDS      IKJKEYWD DEFAULT='NOPDS'                                       00294001
         IKJNAME  'PDS'                                                 00294101
         IKJNAME  'NOPDS'                                               00294201
GENERIC  IKJKEYWD                                                       00294301
         IKJNAME  'GENERIC'                                             00294401
DSONLY   IKJKEYWD                                                       00294501
         IKJNAME  'DSONLY'                                              00294601
DATEFORM IKJKEYWD DEFAULT='MDY'                                 GP09144 00294701
         IKJNAME  'YMD'                                         GP09144 00294801
         IKJNAME  'MDY'                                         GP09144 00294901
         IKJNAME  'DMY'                                         GP09144 00295001
VOLUME   IKJKEYWD                                                       00295101
         IKJNAME  'VOLUME',SUBFLD=VOLSUB                                00295201
HARDCOPY IKJKEYWD                                                       00295301
         IKJNAME  'HARDCOPY',SUBFLD=DDNSUB                              00295401
RCODE    IKJKEYWD                                                       00295501
         IKJNAME  'RC',SUBFLD=RCSUB                                     00295601
VOLSUB   IKJSUBF                                                        00295701
VOLSER   IKJIDENT 'VOLUME SERIAL',                                     +00295801
               ASTERISK,                                               +00295901
               MAXLNTH=6,                                              +00296001
               FIRST=ALPHANUM,                                         +00296101
               OTHER=ALPHANUM,                                         +00296201
               PROMPT='VOLUME SERIAL'                                   00296301
DDNSUB   IKJSUBF                                                        00296401
DDNAME   IKJIDENT 'DDNAME',                                            +00296501
               MAXLNTH=8,                                              +00296601
               FIRST=ALPHANUM,                                         +00296701
               OTHER=ALPHANUM,                                         +00296801
               PROMPT='DDNAME FOR HARDCOPY OUTPUT'                      00296901
RCSUB    IKJSUBF                                                        00297001
RC       IKJKEYWD                                                       00297101
         IKJNAME 'TALLOC'                                               00297201
         IKJNAME 'TUSED'                                                00297301
         IKJNAME 'LALLOC'                                               00297401
         IKJNAME 'LUSED'                                                00297501
         IKJNAME 'TDIFF'                                                00297601
         IKJNAME 'LDIFF'                                                00297701
         IKJNAME 'NUM'                                                  00297801
         IKJNAME 'PREVIOUS'                                             00297901
         IKJNAME 'DSORG'                                                00298001
         IKJNAME 'DIRALLOC'                                             00298101
         IKJNAME 'DIRUSED'                                              00298201
         IKJNAME 'ENTRIES'                                              00298301
         IKJNAME 'MEMBERS'                                              00298401
         IKJNAME 'ALIASES'                                              00298501
         IKJNAME 'TPCTUSE'             PERCENT USED - ALL DS    GP09143 00298601
         IKJNAME 'LPCTUSE'             PERCENT USED - LAST DS   GP09143 00298701
         IKJENDP                                                        00298801
         TITLE 'DYNAMIC ALLOCATION CONTROL BLOCK DSECTS'                00298901
*********************************************************************** 00299001
*                                                                     * 00299101
*              DYNAMIC ALLOCATION CONTROL BLOCK DSECTS                * 00299201
*                                                                     * 00299301
*********************************************************************** 00299401
         IKJDAP18                                                       00299501
DA18LEN  EQU   *-DAPB18                 DAPB LENGTH                     00299601
         IKJDAP08                                                       00299701
DA08LEN  EQU   *-DAPB08                 DAPB LENGTH                     00299801
*********************************************************************** 00299901
*                                                                     * 00300001
*                  WORK AREA DSECT                                    * 00300101
*                                                                     * 00300201
*********************************************************************** 00300301
DSATDS   DSECT                                                          00300401
DSATSAVE DS    18A                     SAVE AREA                        00300501
SAVE2    DS    18F                     SAVE AREA                        00300601
SAVEFORM DS    16F                     SAVE AREA FOR DS INFO    GP09143 00300701
MYCPPL   DS    A                       QUICK REFERENCE          GP09143 00300801
DSATDCB1 CAMLST SEARCH,0,0,0                                            00300901
DSATDCB3 CAMLST SEEK,0,0,0                                              00301001
LOCLIST  CAMLST NAME,0,,0              LOCATE BY NAME                   00301101
LOCLIST2 CAMLST NAME,0,0,0             LOCATE BY NAME AND CVOL          00301201
LOCBYTTR CAMLST BLOCK,0,0,0            LOCATE BY TTR                    00301301
GDSLIST  CAMLST NAME,OBTNAME,,WORKAREA                          GP09143 00301401
OBTNAME  DS    CL44                    NAME FOR FORMINFO OBTAIN GP09143 00301501
GDSBLEN  DS    F                       LENGTH OF BASE NAME      GP09143 00301601
GDSCURR  DS    A                       GENERATION ENTRY IN LIST GP09143 00301701
GDSCNTR  DS    F                       NO. OF SHOWCAT ENTRIES   GP09143 00301801
SHOWCATL SHOWCAT MF=L                                           GP09143 00301901
DSATPTPB PUTLINE MF=L                  LIST FORM OF PUTLINE             00302001
DSATIOPL DS    0A                      ALIGN TO FULLWORD                00302101
         ORG   *+IOPLLEN               LENGTH OF CONTROL BLOCK          00302201
DSATPPL  DS    0A                      ALIGN TO FULLWORD                00302301
         ORG   *+PPLLEN                LENGTH OF CONTROL BLOCK          00302401
DSATANS  DS    A                       ADDRESS OF PARSE ANSWER AREA     00302501
DSATECB  DS    F                       EVENT CONTROL BLOCK              00302601
DSATDAPL DS    0F                      FULLWORD ALIGNMENT               00302701
         ORG   *+DAPLLEN                                                00302801
DA18DAPB DS    0F                      FULLWORD ALIGNMENT               00302901
         ORG   *+DA18LEN                                                00303001
DA08DAPB DS    0F                      FULLWORD ALIGNMENT               00303101
         ORG   *+DA08LEN                                                00303201
CURRPDL  DS    A                       ADDRESS OF CURRENT PDS   GP09144 00303301
CURRBLK  DS    A                       ADDRESS OF CURRENT CAT BLOCK     00303401
ENDWORK  DS    A                       ADDRESS OF END OF VSAM WORK AREA 00303501
TOTALRUN DS    F                       TOTAL TRACKS ALLOCATED           00303601
         DS    F                       TOTAL TRACKS USED                00303701
TOTALDS  DS    F                       LAST DATA SET TRACKS ALLOCATED   00303801
         DS    F                       LAST DATA SET TRACKS USED        00303901
TOTAL#DS DS    F                       NUMBER OF DATA SETS DISPLAYED    00304001
PDSTOTLS DS    F                       NUMBER OF DIR BLOCKS ALLOCATED   00304101
         DS    F                       NUMBER OF DIR BLOCKS USED        00304201
         DS    F                       NUMBER OF DIRECTORY ENTRIES      00304301
         DS    F                       NUMBER OF ALIASES                00304401
VOLDSTRK DS    F                       TRACKS ALLOCATED FOR VOLUME      00304501
VOLCNT   DS    H                       VOLUME COUNT                     00304601
TRKPRCYL DS    H                       TRACKS PER CYLINDER              00304701
SERFLD   DS    A                       ADDRESS OF SERIAL FIELD          00304801
DEVFLD   DS    A                       ADDRESS OF DEVTYPE FIELD         00304901
DEVFIELD DS    A                       ADDRESS OF DEVICE NAME FIELD     00305001
ALLOCFLD DS    A                       ADDRESS OF ALLOCATION FIELD      00305101
DSORGFLD DS    A                       ADDRESS OF DSORG FIELD           00305201
PDSFLD   DS    A                       ADDRESS OF PDS INFO FIELD        00305301
DCBFLD   DS    A                       ADDRESS OF DCB FIELD             00305401
CRDATFLD DS    A                       ADDRESS OF CREATION DATE FIELD   00305501
EXDATFLD DS    A                       ADDRESS OF EXPIRATION DATE FIELD 00305601
LREFFLD  DS    A                       ADDRESS OF DATE LAST REF FIELD   00305701
DSNFLD   DS    A                       ADDRESS OF DSNAME FIELD          00305801
ERRFLD   DS    A                       ADDRESS OF ERROR FIELD           00305901
CCHHRFLD DS    A                       ADDRESS OF CCHHR FIELD           00306001
SERPTR   DS    A                       SERIAL TABLE POINTER             00306101
LASTUCB  DS    A                       LAST UCB PROCESSED ADDRESS       00306201
FLAGS    DS    X                       INDICATORS                       00306301
NOTEMPTY EQU   X'80'  1... ....          INDEX IS NOT EMPTY             00306401
GDG      EQU   X'40'  .1.. ....          GDG BEING PROCESSED (CVOL)     00306501
COUNT    EQU   X'20'  ..1. ....          COUNT THIS DATA SET            00306601
DSATGRP  EQU   X'10'  ...1 ....          GROUP PROCESSING IN PROGRESS   00306701
$MULTVOL EQU   X'08'  .... 1...          MULTI-VOLUME DATA SET          00306801
$NOTPOPS EQU   X'04'  .... .1..          DATA SET NOT PS OR PO          00306901
$DSPO    EQU   X'02'  .... ..1.          DATA SET IS PARTITIONED        00307001
$DSPS    EQU   X'01'  .... ...1          DATA SET IS SEQUENTIAL         00307101
RESET    EQU   $NOTPOPS+$DSPO+$DSPS+$MULTVOL   RESET MASK               00307201
FLAGS2   DS    X                       MORE INDICATORS                  00307301
$VSAMLOC EQU   X'80'  1... ....          VSAM LOCATE IN PROGRESS        00307401
$INCMPLT EQU   X'40'  .1.. ....          TOO MANY DSNAMES               00307501
$FOUND   EQU   X'20'  ..1. ....          DATA SET FOUND                 00307601
$HARDCPY EQU   X'10'  ...1 ....          HARDCOPY OPTION REQUESTED      00307701
$F2VMASK EQU   X'08'  .... 1...          USER VOLUME MASK       GP09143 00307801
$F2VANY  EQU   X'04'  .... .1..          USER HAD VOL(*)        GP09143 00307901
$F2ENTY  EQU   X'02'  .... ..1.          FOUND NON-DS ENTITY    GP12018 00308001
*                     .... ...X          NOT USED                       00308101
DSATSER  DS    CL6                     SERIAL                           00308201
CEREAL   DS    CL6                     USER SPECIFIED VOLUME    GP09143 00308301
INDEXLEN DS    X                       LENGTH OF INDEX NAME             00308401
INDEX    DS    CL44                    INDEX NAME                       00308501
         DS    0F                                                       00308601
DSATHEDR DS    H,H                     MESSAGE HEADER                   00308701
OUTLINE  DS    CL120                   OUTPUT LINE                      00308801
FULL     DC    0F'0',H'0'    HALF-TO-FULL CONVERSION AREA       GP09127 00308901
HALF     DS    H                       HALFWORD ALIGNED WORK AREA       00309001
DSATDEC  DS    D                       WORK AREA FOR DECIMAL CONVERSION 00309101
DSATPDEC DS    D                       WORK AREA FOR DECIMAL CONVERSION 00309201
WORK     DS    F                       WORK SPACE               GP09144 00309301
DATEWORK DS    CL10                    WORK AREA FOR DATE CONVERSION    00309401
         SPACE 1             NOTE OVERLAYS                      GP09127 00309501
DSNLEN   DS    H                       LENGTH OF DSNAME (PDS)           00309601
DSNAME   DS    CL44                    DSNAME                           00309701
DSATFMT1 DS    148C                    FORMAT 1 DSCB (DS DESCRIPTION)   00309801
         ORG   DSNAME                  EXPLAIN FIELDS           GP09127 00309901
         IECSDSL1 1                      FOR FORMAT 1 DSCB      GP09127 00310001
DS1FLAG1 EQU   DS1NOBDB+1,1,C'X'  MORE FLAGS                            00310101
DS1COMPR EQU   X'80'           COMPRESSABLE EXTENDED IF DS1STRP         00310201
DS1CPOIT EQU   X'40'           CHECKPOINTED D S                         00310301
DS1SMSFG EQU   DS1FLAG1+17,1,C'X'  SMS FLAG                             00310401
DS1SMSDS EQU   X'80'           SMS D S                                  00310501
DS1SMSUC EQU   X'40'           NO BCS ENTRY                             00310601
DS1REBLK EQU   X'20'           MAY BE REBLOCKED                         00310701
DS1CRSDB EQU   X'10'           BLKSZ BY DADSM                           00310801
DS1PDSE  EQU   X'08'           PDS/E                                    00310901
DS1STRP  EQU   X'04'           EXTENDED FORMAT D S                      00311001
DS1PDSEX EQU   X'02'           HFS D S                                  00311101
DS1DSAE  EQU   X'01'           EXTENDED ATTRIBUTES EXIST                00311201
DS1SCEXT EQU   DS1SMSFG+1,3,C'X'  SECONDARY SPACE EXTENSION             00311301
DS1SCXTF EQU   DS1SCEXT,1,C'X'  -"- FLAG                                00311401
DS1SCAVB EQU   X'80'           SCXTV IS AVG BLOCK LEN                   00311501
DS1SCMB  EQU   X'40'                 IS IN MEGBYTES                     00311601
DS1SCKB  EQU   X'20'                 IS IN KILOBYTES                    00311701
DS1SCUB  EQU   X'10'                 IS IN BYTES                        00311801
DS1SCCP1 EQU   X'08'           SCXTV COMPACTED BY 256                   00311901
DS1SCCP2 EQU   X'04'                 COMPACTED BY 65536                 00312001
DS1SCXTV EQU   DS1SCXTF+1,2,C'X'  SEC SPACE EXTNSION VALUE              00312101
DS1ORGAM EQU   DS1ACBM         CONSISTENT NAMING - VSAM D S             00312201
DS1RECFF EQU   X'80'           RECFM F                                  00312301
DS1RECFV EQU   X'40'           RECFM V                                  00312401
DS1RECFU EQU   X'C0'           RECFM U                                  00312501
DS1RECFT EQU   X'20'           RECFM T   001X XXXX IS D                 00312601
DS1RECFB EQU   X'10'           RECFM B                                  00312701
DS1RECFS EQU   X'08'           RECFM S                                  00312801
DS1RECFA EQU   X'04'           RECFM A                                  00312901
DS1RECMC EQU   X'02'           RECFM M                                  00313001
*   OPTCD DEFINITIONS   BDAM    W.EFA..R                                00313101
*                       ISAM    WUMIY.LR                                00313201
*             BPAM/BSAM/QSAM    WUCHBZTJ                                00313301
DS1OPTIC EQU   X'80'  FOR DS1ORGAM - CATLG IN ICF CAT                   00313401
DS1OPTBC EQU   X'40'           ICF CATALOG                              00313501
DS1RACDF EQU   DS1IND40                                                 00313601
DS1SECTY EQU   DS1IND10                                                 00313701
DS1WRSEC EQU   DS1IND04                                                 00313801
DS1SCAL1 EQU   DS1SCALO,1,C'X'    SEC. ALLOC FLAGS                      00313901
DS1DSPAC EQU   X'C0'         SPACE REQUEST MASK                         00314001
DS1CYL   EQU   X'C0'           CYLINDER BOUND                           00314101
DS1TRK   EQU   X'80'           TRACK                                    00314201
DS1AVRND EQU   X'41'           AVG BLOCK + ROUND                        00314301
DS1AVR   EQU   X'40'           AVG BLOCK LEN                            00314401
DS1MSGP  EQU   X'20'                                                    00314501
DS1EXT   EQU   X'10'           SEC. EXTENSION EXISTS                    00314601
DS1CONTG EQU   X'08'           REQ. CONTIGUOUS                          00314701
DS1MXIG  EQU   X'04'           MAX                                      00314801
DS1ALX   EQU   X'02'           ALX                                      00314901
DS1DSABS EQU   X'00'           ABSOLUTE TRACK                           00315001
DS1SCAL3 EQU   DS1SCAL1+1,3,C'X'  SEC ALLOC QUANTITY                    00315101
         SPACE 1             NOTE OVERLAYS                      GP09127 00315201
DSATFMT3 DS    0D,148C                 FORMAT 3 DSCB (ADD EXTENTS)      00315301
         ORG   DSATFMT3                                         GP09127 00315401
         IECSDSL1 3                    DEFINE FORMAT 3 DSCB     GP09127 00315501
         SPACE 1                                                        00315601
CATBLOCK DS    0D,265C                 CATALOG BLOCK                    00315701
CVOL     DS    CL6                     CONTROL VOLUME SERIAL            00315801
TTR      DS    XL3                     TTR OF CATALOG BLOCK             00315901
FIRSTBLK DS    A                       ADDRESS OF FIRST CATALOG BLOCK   00316001
ENDNAME  DS    A                       ADDRESS OF END OF INDEX NAME     00316101
         SPACE 1                                                        00316201
CTGPL    DS    0D                                                       00316301
*                                                                       00316401
CTGOPTN1 DS    X              FIRST OPTION BYTE:                        00316501
CTGBYPSS EQU   X'80' 1... ....  BYPASS CATALOG MANAGMENT SECURITY       00316601
CTGMAST  EQU   X'40' .1.. ....  CHECK THE MASTER PASSWORD               00316701
CTGCI    EQU   X'20' ..1. ....  CHECK CONTROL INTERVAL PASSWORD         00316801
CTGUPD   EQU   X'10' ...1 ....  CHECK UPDATE PASSWORD                   00316901
CTGREAD  EQU   X'08' .... 1...  CHECK READ PASSWORD                     00317001
CTGNAME  EQU   X'04' .... .1..  CTGENT CONTAINS DSNAME OR SERIAL ADDR   00317101
*                    .... .0..  CTGENT CONTAINS CONTROL INTERVAL NUMBER 00317201
CTGCNAME EQU   X'02' .... ..1.  CTGCAT CONTAINS CATALOG DSNAME ADDRESS  00317301
*                    .... ..0.  CTGCAT CONTAINS CATALOG ACB ADDRESS     00317401
CTGGENLD EQU   X'01' .... ...1  GENERIC LOCATE REQUEST                  00317501
*                                                                       00317601
CTGOPTN2 DS    X              SECOND OPTION BYTE:                       00317701
CTGEXT   EQU   X'80' 1... ....  EXTEND OPTION (WITH UPDATE)             00317801
CTGNSVS  EQU   X'80'            NO SCRATCH VSAM SPACE (WITH DELETE)     00317901
CTGERASE EQU   X'40' .1.. ....  ERASE OPTION (WITH DELETE)              00318001
CTGSMF   EQU   X'40'            WRITE SMF RECORD OPTION (WITH LSPACE)   00318101
CTGGALL  EQU   X'40'            SEARCH ALL CATALOGS (WITH LISTCAT)      00318201
CTGPURG  EQU   X'20' ..1. ....  PURGE OPTION (WITH DELETE)              00318301
CTGVMNT  EQU   X'20'            CALLER IS VSAM OPEN/CLOSE/EOV           00318401
CTGRCATN EQU   X'20'            RETURN CATALOG NAME (WITH LISTCAT)      00318501
CTGGTNXT EQU   X'10' ...1 ....  GET-NEXT OPTION (WITH LISTCAT)          00318601
CTGDISC  EQU   X'08' .... 1...  DISCONNECT OPTION (WITH EXPORT)         00318701
CTGOVRID EQU   X'04' .... .1..  ERASE OVERRIDE OPTION (WITH DELETE)     00318801
CTGSCR   EQU   X'02' .... ..1.  SCRATCH SPACE (WITH DELETE NON-VSAM)    00318901
*                    .... ...X  RESERVED                                00319001
*                                                                       00319101
CTGOPTN3 DS    0X             THIRD OPTION BYTE                         00319201
CTGFUNC  DS    X     XXX. ....  SPECIFIES THE CALLER-REQUESTED FUNCTION 00319301
CTGLOC   EQU   X'20' 001. ....    LOCATE                                00319401
CTGLSP   EQU   X'40' 010. ....    LSPACE                                00319501
CTGUPDAT EQU   X'60' 011. ....    UPDATE                                00319601
CTGCMS   EQU   X'80' 100. ....    CATALOG MGMT SERV. FUNCT. SEE CTGOPNS 00319701
CTGSUPLT EQU   X'10' ...1 ....  SUPERLOCATE FUNCTION                    00319801
CTGGDGL  EQU   X'08' .... 1...  GDG LOCATE - CALLER SUPPLIED BASE LEVEL 00319901
CTGSRH   EQU   X'04' .... .1..  SEARCH MASTER CATALOG ONLY              00320001
*                    .... .0..  SEARCH USER CATALOGS FIRST              00320101
*                    .... ..X.  RESERVED                                00320201
CTGAM0   EQU   X'01' .... ...1  OS/VS2 CATALOG MANAGMENT REQUEST        00320301
*                    .... ...0  OS CATALOG REQUEST USER SUPPLIED CAMLST 00320401
*                                                                       00320501
CTGOPTN4 DS    X              FOURTH OPTION BYTE                        00320601
CTGLBASE EQU   X'80' 1... ....  LOCATE BASE LEVEL (SUPERLOCATE GDG)     00320701
CTGDOCAT EQU   X'40' .0.. ....  DYNAMICALLY LOCATE AND OPEN CAT IF REQ. 00320801
*                    .1.. ....  DO NOT DYNAMICALLY OPEN NEEDED CATALOG  00320901
*                    ..XX XXXX  RESERVED                                00321001
*                                                                       00321101
CTGENT   DS    0A             ADDRESS OF CATALOG RECORD IDENTIFIER      00321201
CTGFVT   DS    A              ADDRESS OF CALLER'S CTGFV                 00321301
*                                                                       00321401
CTGCAT   DS    0A             ADDRESS OF CATALOG DSNAME OR ACB          00321501
CTGCVOL  DS    A              ADDRESS OF OS SYSCTLG DSNAME              00321601
*                                                                       00321701
CTGWKA   DS    A              ADDRESS OF CALLER'S WORK AREA             00321801
*                                                                       00321901
CTGDSORG DS    XL2            DATA SET ORGANIZATION IF SUPERLOCATE      00322001
*                                                                       00322101
CTGOPTNS DS    X              CATALOG MANAGMENT SERVICES REQUEST OPTION 00322201
CTGDEFIN EQU   X'08' 0000 1...  DEFINE                                  00322301
CTGALTER EQU   X'10' 0001 0...  ALTER                                   00322401
CTGDELET EQU   X'18' 0001 1...  DELETE                                  00322501
CTGLTCAT EQU   X'20' 0010 0...  LISTCAT                                 00322601
*                    .... .XXX    RESERVED                              00322701
*                                                                       00322801
         DS    X              RESERVED                                  00322901
*                                                                       00323001
CTGTYPE  DS    X              TYPE OF CATALOG RECORD                    00323101
CTGTALIN EQU   C'A'             NON-VSAM DATA SET                       00323201
CTGTGBS  EQU   C'B'             GDG BASE RECORD                         00323301
CTGTCL   EQU   C'C'             CLUSTER                                 00323401
CTGTDATA EQU   C'D'             DATA SET                                00323501
CTGTINDX EQU   C'I'             INDEX                                   00323601
CTGTMCAT EQU   C'M'             MASTER CATALOG                          00323701
CTGTPGS  EQU   C'P'             PAGE DATA SET                           00323801
CTGTUCAT EQU   C'U'             USER CATALOG                            00323901
CTGTVOL  EQU   C'V'             VOLUME                                  00324001
CTGTANM  EQU   C'X'             ALIAS NAME                              00324101
*                                                                       00324201
CTGNOFLD DS    X              NUMBER OF ENTRIES IN CTGFIELD             00324301
*                                                                       00324401
CTGDDNM  DS    0A             ADDRESS OF DD STATEMENT (OPTIONAL)        00324501
CTGNEWNM DS    0A             ADDRESS OF NEW DSNAME (ALTER)             00324601
CTGFDBK  DS    XL2            FEEDBACK AREA (SUPERLOCATE)               00324701
CTGFBFLG DS    X              FLAGS (SUPERLOCATE)                       00324801
CTGPAR   EQU   X'80' 1... ....  PARALLEL MOUNT                          00324901
CTGKEEP  EQU   X'40' .1.. ....  FORCED KEEP                             00325001
CTGGDGB  EQU   X'20' ..1. ....  GDG BASE LOCATED                        00325101
CTGNGDSN EQU   X'10' ...1 ....  GDG DSNAME GENERATED (DSNAME.GXXXXVYY)  00325201
*                    .... XXXX  RESERVED                                00325301
         DS    X              RESERVED                                  00325401
*                                                                       00325501
CTGJSCB  DS    0A             ADDRESS OF JSCB                           00325601
CTGPSWD  DS    A              ADDRESS OF CALLER-SUPPLIED PASSWORD       00325701
*                                                                       00325801
CTGFIELD EQU   *              VARIABLE LENGTH FIELD                     00325901
*                               4 BYTE ADDRESS OF EACH CTGFL TO SPECIFY 00326001
*                               EACH CATALOG FIELD TO BE PROCESSED.     00326101
*                               LENGTH OF CTGFIELD IS CTGNOFLD VALUE    00326201
*                               TIMES 4.                                00326301
*                                                                       00326401
CTGPLLEN EQU   *-CTGPL                 LENGTH OF CTGPL                  00326501
WORKAREA DS    A                       ADDRESS OF CATALOG WORK AREA     00326601
WORKLEN  DS    F                       LENGTH OF CATALOG WORK AREA      00326701
*                                                                       00326801
*        FOR LOCATE BY TTR, THIS AREA IS LARGE ENOUGH TO CONTAIN        00326901
*        22 (MAXIMUM NUMBER OF LEVELS IN A DATA SET NAME) CATALOG       00327001
*        BLOCKS.  EACH 264 BYTE BLOCK IS ALLOCATED AS FOLOWS:           00327101
*                                                                       00327201
*                 A                      POINTER TO ENTRY PROCESSED     00327301
*                 A                      NOT USED (NEEDED TO ALIGN)     00327401
*                 CL256                  CATALOG BLOCK                  00327501
*                                                                       00327601
*        FOR VSAM LOCATES, THIS AREA CONTAINS A 4 BYTE HEADER PLUS      00327701
*        45 BYTE DSNAME ENTRIES.                                        00327801
*                 EACH 45 BYTE ENTRY CONSISTS OF -                      00327901
*                 A  1 BYTE TYPE CODE                                   00328001
*                 A 44 BYTE DSNAME                                      00328101
*                                                                       00328201
         SPACE 1                                                        00328301
HARDCPY  DCB   DDNAME=X,               WILL BE REPLACED AT EXEC TIME   +00328401
               MACRF=PM,                                               +00328501
               DSORG=PS,                                               +00328601
               RECFM=FB,                                               +00328701
               LRECL=120,                                              +00328801
               BLKSIZE=3000                                             00328901
HARDOPEN OPEN  (,),MF=L                                                 00329001
OPENLIST OPEN  (,),MF=L                OPEN CONTROL BLOCK LIST FORM     00329101
         READ  DIRDECB,SF,,,MF=L       LIST FORM OF READ                00329201
DSATDCB  DCB   DDNAME=DSATDDN,                                         +00329301
               DSORG=PS,                                               +00329401
               KEYLEN=8,                                               +00329501
               RECFM=F,                                                +00329601
               BLKSIZE=256,                                            +00329701
               EODAD=DIREOD,                                           +00329801
               MACRF=R                                                  00329901
PDSREC   DS    264X                    DIRECTORY RECORD                 00330001
         SPACE 1                                                        00330101
GDSSPACE EQU   PDSREC                  RETURN FROM GDS LOCATE   GP09143 00330201
         DS    F                                                GP09143 00330301
SHOWSPAC DS    (12+256*8)X             SHOWCAT RETURN           GP09143 00330401
DSECTLEN EQU   *-DSATDS                LENGTH OF WORK AREA              00330501
         SPACE 1                                                        00330601
         IKJECT                                                         00330701
         IKJUPT                                                         00330801
         PRINT NOGEN                                            GP09127 00330901
         CVT   DSECT=YES                                        GP09127 00331001
MYUCB    DSECT ,                                                GP09127 00331101
         IEFUCBOB ,                                             GP09127 00331201
         IHADVCT ,                     DEVICE CHARACTERISTICS   GP09144 00331301
         END                                                            00331401
/*                                                                      00331500
//*----------------------------------------------------------- ASM      00331600
/*                                                                      00331700
//LKED.SYSIN DD *                                                       00331800
  NAME DSAT(R)                                                          00331901
//*----------------------------------------------------------- LKED     00332000
//                                                                      00332100

