//ZAPDSCB  JOB (TSO),
//             'Install ZAPDSCB',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00020000
//*  ZAPDSCB -                                                          00030000
//*    IS A BATCH UTILITY USED TO MODIFY A VARIETY OF DATASET           00040000
//*    ATTRIBUTES.  SEE THE COMMENTS IN THE CODE BELOW FOR              00050000
//*    DOCUMENTATION.                                                   00060000
//*                                                                     00070000
//*  INSTALLATION -                                                     00080000
//*    MODIFY THE JOB CARD ABOVE AND THE PROCEDURE DEFAULTS             00090000
//*    BELOW TO SUIT YOUR TASTE.  NOTE THAT THE TARGET LIBRARY          00100000
//*    MUST BE AUTHORIZED.                                              00110000
//*                                                                     00120000
//INSTALL PROC SOUT='*',             <=== SYSOUT CLASS                  00130000
//             LIB='SYS2.LINKLIB',   <=== TARGET LOAD LIBRARY           00140000
//             MBR=ZAPDSCB,          <=== LOAD MODULE NAME              00150000
//             ASMBLR=IEUASM,        <=== NAME OF YOUR ASSEMBLER        00160000
//             ALIB='SYS1.LINKLIB',  <=== LOCATION OF YOUR ASSEMBLER    00170000
//             SYSTS=SYSDA,          <=== UNITNAME FOR WORK DATASETS    00180000
//             SMPMTS='SYS1.SMPMTS', <=== SMPMTS MACRO LIBRARY          00190000
//             MACLIB='SYS1.MACLIB', <=== MACLIB MACRO LIBRARY          00200000
//             AMODGEN='SYS1.AMODGEN', <=== AMODGEN MACRO LIBRARY       00210000
//             MODGEN='SYS1.SMPMTS'  <=== MODGEN MACRO LIBRARY          00220000
//*                                  USE 'SYS1.MODGEN' FOR MVS-XA       00230000
//*                                  USE 'SYS1.SMPMTS' FOR MVS-370      00240000
//*                                                                     00250000
//ASM     EXEC PGM=&ASMBLR,REGION=2048K,PARM='NOOBJECT,DECK'            00260000
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00270000
//SYSTERM  DD  SYSOUT=&SOUT                                             00280000
//SYSPRINT DD  SYSOUT=&SOUT                                             00290000
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00300000
//         DD  DSN=&MODGEN,DISP=SHR                                     00310000
//         DD  DSN=&SMPMTS,DISP=SHR                                     00320000
//         DD  DSN=&AMODGEN,DISP=SHR                                    00330000
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00340000
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00350000
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00360000
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(,PASS,DELETE),            00370000
//             SPACE=(TRK,(5,1),RLSE)                                   00380000
//*                                                                     00390000
//LKED    EXEC PGM=HEWL,COND=(0,NE),PARM='LIST,MAP,XREF,AC=1'           00400000
//SYSPRINT DD  SYSOUT=&SOUT                                             00410000
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,5)                                00420000
//SYSLMOD  DD  DSN=&LIB(&MBR),DISP=SHR                                  00430000
//SYSLIN   DD  DSN=&&SYSLIN,DISP=(OLD,DELETE)                           00440000
//        PEND                                                          00450000
//*                                                                     00460000
//        EXEC INSTALL                                                  00470000
//ASM.SYSIN DD *                                                        00480000
         TITLE 'DOCUMENTATION'                                          00490000
         SPACE                                                          00500000
*  FUNCTION -                                                           00510000
*             THIS PROGRAM UPDATES DSCB'S WITH SPECIFIED VALUES ON      00520000
*             THE DD CARD FOR THE DATASET(S). THE DSCB IS UPDATED       00530000
*             WITHOUT REGARD TO THE ACTUAL DATASET FORMAT, THUS         00540000
*             CARE MUST BE TAKEN USING THIS PROGRAM - IT IS ABOUT       00550000
*             AS DANGEROUS AS SUPERZAP.                                 00560000
*                                                                       00570000
*  USE -                                                                00580000
*             THIS PROGRAM REQUIRES APF AUTHORIZATION.                  00590000
*             THE PROGRAM UPDATES THE DSCB'S FOR ANY DATASETS ALLOCATED 00600000
*             WITH A DDNAME BEGINNING WITH "ZAP". THE PARAMETERS THAT   00610000
*             CAN BE CHANGED ARE:                                       00620000
*                 EXPIRATION DATE (CAN BE SET TO ZERO VIA "RETPD=0"),   00630000
*                 ASM2 USE COUNT  (CAN BE SET TO ZERO VIA "FCB=ZUSE"),  00640000
*                 SEC SPACE AMT   (SET VIA "SPACE=(TRK,(0,##))"),       00650000
*                 DSORG,RECFM,BLKSIZE,LRECL,KEYLEN,RKP,OPTCD AND        00660000
*                 PASSWORD PROTECTION. (CAN BE REMOVED VIA "FCB=NOPW"   00670000
*                                       OR SET VIA LABEL=(,,PASSWORD)   00680000
*                                       AND LABEL=(,,NOPWREAD)      )   00690000
*             THIS PROGRAM USES NO FILES FOR CONTROL OR MESSAGES, ALL   00700000
*             DIAGNOSTICS PRODUCED ARE ROUTED TO THE SYSTEM CONSOLE.    00710000
*                                                                       00720000
*             SAMPLE JCL FOR USING PROGRAM -                            00730000
*                 //ZAPDSCB EXEC PGM=ZAPDSCB                            00740000
*                 //SNAP     DD  SYSOUT=A     SNAP  DUMP WHEN I/O ERROR 00750000
*                 //SYSUDUMP DD  SYSOUT=A     ABEND DUMP OTHER ERRORS   00760000
*                 //ZAP1     DD  DSN=SYS1.LINKLIB,LABEL=EXPDT=99350     00770000
*                 //ZAP2     DD  DSN=SYS1.CDS,LABEL=RETPD=0             00780000
*                    ... ETC                                            00790000
*                                                                       00800000
*  AUTHOR -                                                             00810000
*             DAVE PHILLIPS                                             00820000
*               A. DUDA AND SONS, INC.                                  00830000
*               P.O. BOX 257                                            00840000
*               OVIEDO, FL  32765                                       00850000
*                                                                       00860000
         TITLE 'EQUATES'                                                00870000
         PRINT OFF                                                      00880000
         MACRO                                                          00890000
&NAME   SYNADAF &ACSMETH=,&PARM1=,&PARM2=                               00900000
         LCLC  &CB1                                                     00910000
         AIF   (T'&ACSMETH EQ 'O').AMERR                                00920000
         AIF   ('&ACSMETH' EQ '(0)').AMERR                              00930000
&CB1     SETC  '&ACSMETH'(1,1)                                          00940000
         AIF   ('&CB1' EQ '(').AMERR                                    00950000
.*  &CB1     SETC  '(0)'                     FIX A STUPID MACRO         00960000
         AIF   ('&ACSMETH' EQ 'EXCP').AMEXCP                            00970000
&CB1     SETC  '(1)'                                                    00980000
         AIF   ('&ACSMETH' EQ 'BPAM').AMCOM                             00990000
&CB1     SETC  '(2)'                                                    01000000
         AIF   ('&ACSMETH' EQ 'BSAM').AMCOM                             01010000
&CB1     SETC  '(3)'                                                    01020000
         AIF   ('&ACSMETH' EQ 'QSAM').AMCOM                             01030000
&CB1     SETC  '(4)'                                                    01040000
         AIF   ('&ACSMETH' EQ 'BDAM').AMCOM                             01050000
&CB1     SETC  '(5)'                                                    01060000
         AIF   ('&ACSMETH' EQ 'BISAM').AMCOM                            01070000
&CB1     SETC  '(6)'                                                    01080000
         AIF   ('&ACSMETH' EQ 'QISAM').AMCOM                            01090000
&CB1     SETC  '(7)'                                                    01100000
         AIF   ('&ACSMETH' EQ 'BTAM').AMCOM                             01110000
&CB1     SETC  '(8)'                                                    01120000
         AIF   ('&ACSMETH' EQ 'QTAM').AMCOM                             01130000
&CB1     SETC  '(9)'                                                    01140000
         AIF   ('&ACSMETH' EQ 'GAM').AMCOM                              01150000
.AMERR   MNOTE 8,' ERROR ** ACCESS METHOD NOT SPEC. OR ILLEGAL '  15138 01160000
         MEXIT                                                    15138 01170000
.AMEXCP  ANOP                                                           01180000
&NAME    LA    15,0(0,15)     SET ACCESS METHOD IN REG. 15              01190000
         AGO     .PARMA                                                 01200000
.AMCOM   ANOP                                                     15138 01210000
&NAME    LA    15,0(0,15)              ZERO HIGH ORDER BYTE       15138 01220000
         CNOP  0,4                                                15138 01230000
         O     15,*+8         SET ACCESS METHOD IN REG. 15              01240000
         BC    15,*+8         BRANCH AROUND LIST                        01250000
         DC    AL1&CB1        ACCESS METHOD CODE                        01260000
         DC    AL3(0)                                                   01270000
.PARMA   AIF   (T'&PARM1 EQ 'O').PARMB                                  01280000
         AIF   ('&PARM1' EQ '(1)').PARMB                                01290000
&CB1     SETC  '&PARM1'(1,1)                                            01300000
         AIF   (NOT ('&CB1' EQ '(')).SYMA                               01310000
         LR    1,&PARM1(1)    GET ADDRESS IN REG. 1                     01320000
         AGO   .PARMB                                                   01330000
.SYMA    LA    1,&PARM1       GET ADDRESS IN REG. 1                     01340000
.PARMB   AIF   (T'&PARM2 EQ 'O').COM                                    01350000
         AIF   ('&PARM2' EQ '(0)').COM                                  01360000
&CB1     SETC  '&PARM2'(1,1)                                            01370000
         AIF   (NOT ('&CB1' EQ '(')).SYMB                               01380000
         LR    0,&PARM2(1)    SET PARAMETER IN REG. 0                   01390000
         AGO   .COM                                                     01400000
.SYMB    LA    0,&PARM2(1)    GET PARAMETER IN REG. 0                   01410000
.COM     SVC   68             ISSUE SVC                                 01420000
         MEND                                                           01430000
         MACRO                                                          01440000
&LABEL   SNAPSHOT &SWITCH,&DCB=SNAPDCB,&TCB=,&ID=,&SDATA=(CB),         C01450000
               &PDATA=(PSW,REGS,SA),&STORAGE=,&LIST=,                  C01460000
               &SNAPDCB=NO,&DDNAME=SNAPSHOT                             01470000
.*                                                                      01480000
         GBLB  &TRACE             GLOBAL TRACE CONTROL                  01490000
         LCLC  &TAG                                                     01500000
.*                                                                      01510000
&TAG     SETC  '&LABEL.        '(1,8)     SET UP FOR MNOTES             01520000
.*                                                                      01530000
         AIF   ('&SWITCH' EQ '').ENTRY                                  01540000
         AIF   ('&SWITCH' EQ 'ON').SETON                                01550000
         AIF   ('&SWITCH' NE 'OFF').CHKOC                               01560000
&TRACE   SETB  0                  TURN TRACE OFF                        01570000
         MEXIT                                                          01580000
.SETON   ANOP                                                           01590000
&TRACE   SETB  1                  TURN TRACE ON                         01600000
         MEXIT                                                          01610000
.*                                                                      01620000
.CHKOC   AIF   (NOT &TRACE).LABEL                                       01630000
         AIF   ('&SWITCH' EQ 'DCB').DODCB                               01640000
         AIF   ('&SWITCH' NE 'OPEN').CHKCLS                             01650000
.*                                                                      01660000
         MNOTE '&TAG OPEN  (&DCB,(OUTPUT))'                             01670000
&LABEL   OPEN  (&DCB,(OUTPUT))                                          01680000
         AIF   ('&SNAPDCB' EQ 'YES').DCBDONE                            01690000
         B     SNAP&SYSNDX                                              01700000
         SPACE                                                          01710000
.DODCB   ANOP                                                           01720000
         MNOTE '&DCB  DCB DSORG=PS,RECFM=VBA,MACRF=(W),BLKSIZE=882,'    01730000
         MNOTE '             LRECL=125,DDNAME=&DDNAME'                  01740000
         PUSH  PRINT                                                    01750000
         PRINT NOGEN                                                    01760000
&DCB     DCB   DSORG=PS,RECFM=VBA,MACRF=(W),BLKSIZE=882,LRECL=125,     C01770000
               DDNAME=&DDNAME                                           01780000
         POP   PRINT                                                    01790000
         AIF   ('&SWITCH' EQ 'DCB').DCBDONE                             01800000
SNAP&SYSNDX DS  0H                                                      01810000
.DCBDONE ANOP                                                           01820000
         SPACE                                                          01830000
         MEXIT                                                          01840000
.*                                                                      01850000
.CHKCLS  AIF ('&SWITCH' NE 'CLOSE').BADSW                               01860000
.*                                                                      01870000
         MNOTE '&TAG CLOSE (&DCB)'                                      01880000
&LABEL   CLOSE (&DCB)                                                   01890000
         SPACE                                                          01900000
         MEXIT                                                          01910000
.*                                                                      01920000
.*                                                                      01930000
.ENTRY   AIF   (&TRACE).TRACE                                           01940000
.LABEL   AIF   ('&LABEL' EQ '').STOP                                    01950000
.*  TRACE NOT ON SO JUST SET A TAG                                      01960000
&LABEL   EQU   *                                                        01970000
         MEXIT                                                          01980000
.*                                                                      01990000
.TRACE   ANOP                                                           02000000
.*                                                                      02010000
         MNOTE '&TAG SNAP  DCB=&DCB,TCB=&TCB,ID=&ID,STORAGE=&STORAGE,'  02020000
         MNOTE '               PDATA=&PDATA,SDATA=&SDATA,LIST=&LIST'    02030000
&LABEL   SNAP  DCB=&DCB,TCB=&TCB,ID=&ID,STORAGE=&STORAGE,              *02040000
               SDATA=&SDATA,PDATA=&PDATA,LIST=&LIST                     02050000
         SPACE                                                          02060000
         MEXIT                                                          02070000
.*                                                                      02080000
.BADSW   MNOTE 4,'INVALID OPTION, ''&SWITCH'''                          02090000
.STOP    MEND                                                           02100000
         SPACE 2                                                        02110000
         PRINT ON                                                       02120000
         SPACE 3                                                        02130000
*        REGISTER EQUATES                                               02140000
         SPACE                                                          02150000
R0       EQU   0                  WORK REGISTER                         02160000
R1       EQU   1                  WORK REGISTER                         02170000
R2       EQU   2                  WORK REGISTER                         02180000
R3       EQU   3                  WORK REGISTER                         02190000
R4       EQU   4                  WORK REGISTER                         02200000
R5       EQU   5                  WORK REGISTER                         02210000
R6       EQU   6                  WORK REGISTER                         02220000
R7       EQU   7                  WORK REGISTER                         02230000
R8       EQU   8                  WORK REGISTER                         02240000
R9       EQU   9                  WORK REGISTER                         02250000
TIOTLEN  EQU   10                 CONTAINS LENGTH OF TIOT ENTRY         02260000
@TIOT    EQU   11                 BASE REGISTER FOR TIOT ENTRIES        02270000
BASEREG  EQU   12                 BASE REGISTER FOR ZAPDSCB CSECT       02280000
R13      EQU   13                 SAVE AREA POINTER                     02290000
R14      EQU   14                 LINK REGISTER                         02300000
R15      EQU   15                 RETURN CODE REGISTER                  02310000
         TITLE 'HOUSEKEEPING AND POINTER SETUP'                         02320000
ZAPDSCB  CSECT                                                          02330000
         SAVE  (14,12),,&SYSDATE-&SYSTIME-ZAPDSCB                       02340000
         BALR  BASEREG,0                                                02350000
         USING *,BASEREG                                                02360000
         SPACE                                                          02370000
         LA    R1,SAVEAREA                                              02380000
         ST    R13,4(,R1)         NEW TO OLD                            02390000
         ST    R1,8(,R13)         OLD TO NEW                            02400000
         LR    R13,R1             -> NEW SAVE AREA                      02410000
         SPACE 2                                                        02420000
         L     R1,16              -> CVT                                02430000
         L     R1,0(,R1)          -> TCB WORDS                          02440000
         L     R1,4(,R1)          -> TCB                                02450000
         L     R1,12(,R1)         -> TIOT                               02460000
         SPACE                                                          02470000
         USING TIOT,R1                                                  02480000
         LA    @TIOT,TIOENTRY     -> FIRST DD ENTRY                     02490000
         DROP  R1                                                       02500000
         USING TIOENTRY,@TIOT                                           02510000
         SPACE                                                          02520000
         SR    TIOTLEN,TIOTLEN    CLEAR TIOT LENGTH REGISTER            02530000
         TITLE 'LOOP THRU TIOT LOOKING FOR "ZAP" DD CARDS'              02540000
DDLOOP   AR    @TIOT,TIOTLEN      -> NEXT DD ENTRY                      02550000
         ICM   TIOTLEN,1,TIOELNGH    LENGTH OF DD ENTRY                 02560000
         BZ    ENDTIOT            YES - END OF TIOT                     02570000
         SPACE                                                          02580000
         CLC   =C'ZAP',TIOEDDNM   DDNAME START WITH ZAP ???             02590000
         BNE   DDLOOP             NO                                    02600000
         TM    TIOELINK,X'FF'     SYSIN/SYSOUT/SUBSYSTEM DS ???         02610000
         BNZ   DDLOOP             YES, IGNORE                           02620000
         TM    TIOESTTA,TIOSJBLB  A JOBLIB ???                          02630000
         BO    DDLOOP             YES - REJECT                          02640000
         SPACE                                                          02650000
         L     R1,TIOESTTB        -> UCB                                02660000
         USING UCB,R1                                                   02670000
         SPACE                                                          02680000
         TM    UCBTBYT3,UCB3DACC  A DIRECT ACCESS DEVICE ???            02690000
         BNO   DDLOOP             NO - REJECT                           02700000
         DROP  R1                                                       02710000
         SPACE 2                                                        02720000
* TIOT ENTRY HAS BEEN SELECTED FOR A POSSIBLE "ZAP" DD                  02730000
         SPACE                                                          02740000
         MVC   DCB+40(8),TIOEDDNM GET DDNAME OF DATASET                 02750000
         SPACE                                                          02760000
         RDJFCB (DCB)             GET JOB FILE CONTROL BLOCK            02770000
         LTR   R15,R15            GET JFCB OK ?                         02780000
         BNZ   BADJFCB            NOPE                                  02790000
         SPACE                                                          02800000
         TM    JFCBTSDM,JFCSDS    A SYSIN/SYSOUT DATA SET ?             02810000
         BO    INVALIDD           YES - REJECT                          02820000
         SPACE                                                          02830000
         TM    JFCBLTYP,JFCSL     STANDARD LABEL ??                     02840000
         BNO   INVALIDD           NO - REJECT                           02850000
         SPACE                                                          02860000
         TM    JFCBIND1,JFCRLSE+JFCADDED+JFCGDG+JFCPDS    CHECK FOR    *02870000
                           RLSE SPECIFIED,OR A GDG,OR A MEMBER OF A PDS 02880000
         BNZ   INVALIDD           NOT ALLOWED                           02890000
         SPACE                                                          02900000
         TM    JFCBIND2,JFCNEW    DATA-SET NEW ??                       02910000
         BO    INVALIDD           YES - REJECT                          02920000
         TM    JFCBIND2,JFCMOD    DATA-SET ALLOCATED MOD ?              02930000
         BO    INVALIDD           YES - REJECT                          02940000
         SPACE                                                          02950000
         TM    JFCBIND2,JFCTEMP   TEMPORARY DATA SET ???                02960000
         BO    INVALIDD           YES - REJECT                          02970000
         SPACE                                                          02980000
         TM    JFCDSRG2,JFCORGAM  A VSAM DS ??                          02990000
         BO    INVALIDD           YES - REJECT                          03000000
         TITLE 'UPDATE THE DSCB'                                        03010000
         SPACE 2                                                        03020000
* "ZAP" DD CARD HAS PASSED TESTS, SO SAVE INFO USER WANTS CHANGED       03030000
         SPACE                                                          03040000
         MVC   DSNAME,JFCBDSNM    DSNAME                                03050000
         MVC   VOLSER,JFCBVOLS    VOLSER                                03060000
         MVC   CREDT,JFCBCRDT     CREATION DATE  (ALWAYS CURRENT DATE)  03070000
         MVC   EXPDT,JFCBXPDT     EXPIRATION DATE                       03080000
         MVC   KEYLEN,JFCKEYLE    KEY LENGTH                            03090000
         MVC   DSORG,JFCDSRG1     DSORG                                 03100000
         MVC   RECFM,JFCRECFM     RECFM                                 03110000
         MVC   BLKSIZE,JFCBLKSI   BLKSIZE                               03120000
         MVC   LRECL,JFCLRECL     LRECL                                 03130000
         MVC   RKP,JFCRKP         RKP                                   03140000
         MVC   OPTCD,JFCOPTCD     OPTCD                                 03150000
         MVC   SPECOPER,JFCFCBID  SPECIAL OPER WORD   (REALLY FCB=)     03160000
         MVC   PSWDBITS,JFCBIND2  FLAGS FOR PSWD PROT AND THINGS        03170000
         NI    PSWDBITS,JFCBRWPW  LEAVE ONLY PSWD BITS                  03180000
         MVC   SECQTY,JFCBSQTY    GET SECONDARY SPACE QUANITY           03190000
         MVC   SECBITS,JFCBCTRI   FLAGS FOR SPACE ALLOC                 03200000
         NI    SECBITS,JFCBSPAC   LEAVE ONLY SPACE BITS                 03210000
         SPACE                                                          03220000
         OC    DSCBINFO,DSCBINFO  ANY CHANGES SPECIFIED ???             03230000
         BZ    NOCHANGE           NO - GET NEXT DD CARD                 03240000
         SPACE                                                          03250000
* GET THE DSCB FROM THE VTOC                                            03260000
         SPACE                                                          03270000
         OBTAIN ADSCB             GET THE DSCB FROM VTOC                03280000
         LTR    R15,R15           OK ?                                  03290000
         BNZ    BADOBTAN                                                03300000
         SPACE                                                          03310000
* INITIALIZE A JFCB FOR READING THE VTOC                                03320000
         SPACE                                                          03330000
         XC    JFCB(JFCBLGTH),JFCB                 CLEAN SLATE          03340000
         MVI   JFCBDSNM,X'04'                      DSNAME               03350000
         MVC   JFCBDSNM+1(L'JFCBDSNM-1),JFCBDSNM   OF VTOC              03360000
         MVI   JFCBTSDM,JFCNWRIT                   DON'T WRT JFCB BACK  03370000
         MVI   JFCKEYLE,44                         KEYLEN = 44          03380000
         MVC   JFCBLKSI,=AL2(96)                   BLKSIZE = 96         03390000
         MVC   JFCLRECL,=AL2(96)                   LRECL = 96           03400000
         MVI   JFCBNVOL,1                          NVOL = 1             03410000
         MVC   JFCBVOLS(6),VOLSER                  VOLSER               03420000
         MVI   JFCBVLCT,1                          NVOL = 1             03430000
         SPACE                                                          03440000
* OPEN THE VTOC FOR UPDATE SO WE CAN WRITE THE DSCB BACK                03450000
         SPACE                                                          03460000
         OPEN  (DCB,(UPDAT)),TYPE=J                                     03470000
         LTR   R15,R15            OPEN SUCCESSFULL ?                    03480000
         BNZ   BADOPEN            NO                                    03490000
         EJECT                                                          03500000
* UPDATE THE DSCB FROM INFO SAVED FROM JFCB                             03510000
         SPACE                                                          03520000
         MVC   DS1DSNAM,DSNAME    FILL IN DSNAME                        03530000
         SPACE                                                          03540000
         OC    EXPDT,EXPDT        ANY EXPIRATION DATE ??                03550000
         BZ    NOEXPDT            NO                                    03560000
         MVC   DS1EXPDT,EXPDT     YES                                   03570000
         CLC   CREDT,EXPDT        DID USER SPECIFY RETPD=0 ???          03580000
         BNE   NOEXPDT            NO - LEAVE EXPDT ASIS                 03590000
         XC    DS1EXPDT,DS1EXPDT  YES - ZERO OUT EXPIRATION DATE        03600000
         SPACE                                                          03610000
NOEXPDT  OC    KEYLEN,KEYLEN      ANY KEYLEN ??                         03620000
         BZ    NOKEYLEN           NO                                    03630000
         MVC   DS1KEYL,KEYLEN     YES                                   03640000
         SPACE                                                          03650000
NOKEYLEN OC    DSORG,DSORG        ANY DSORG ??                          03660000
         BZ    NODSORG            NO                                    03670000
         MVC   DS1DSORG,DSORG     YES                                   03680000
         SPACE                                                          03690000
NODSORG  OC    RECFM,RECFM        ANY RECFM                             03700000
         BZ    NORECFM            NO                                    03710000
         MVC   DS1RECFM,RECFM     YES                                   03720000
         SPACE                                                          03730000
NORECFM  OC    BLKSIZE,BLKSIZE    ANY BLKSIZE ??                        03740000
         BZ    NOBLKSIZ           NO                                    03750000
         MVC   DS1BLKL,BLKSIZE    YES                                   03760000
         SPACE                                                          03770000
NOBLKSIZ OC    LRECL,LRECL        ANY LRECL ??                          03780000
         BZ    NOLRECL            NO                                    03790000
         MVC   DS1LRECL,LRECL     YES                                   03800000
         SPACE                                                          03810000
NOLRECL  OC    RKP,RKP            ANY RKP ??                            03820000
         BZ    NORKP              NO                                    03830000
         MVC   DS1RKP,RKP         YES                                   03840000
         SPACE                                                          03850000
NORKP    OC    OPTCD,OPTCD        ANY OPTCD ??                          03860000
         BZ    NOOPTCD            NO                                    03870000
         MVC   DS1OPTCD,OPTCD     YES                                   03880000
         SPACE                                                          03890000
NOOPTCD  CLI   PSWDBITS,JFCBRWPW  SET NOPWREAD AND PWWRITE ??           03900000
         BNE   NOWRITPW           NO                                    03910000
         OI    DS1DSIND,X'14'     YES                                   03920000
         SPACE                                                          03930000
NOWRITPW CLI   PSWDBITS,JFCSECUR  SET PWREAD AND PWWRITE ??             03940000
         BNE   NOREADPW           NO                                    03950000
         OI    DS1DSIND,X'10'     YES                                   03960000
         NI    DS1DSIND,X'FF'-X'04'                                     03970000
         SPACE                                                          03980000
NOREADPW CLC   SPECOPER,=C'ZUSE'  ZERO USE COUNT ??   (FCB=ZUSE)        03990000
         BNE   NOZUSE             NO                                    04000000
         XC    ASM2UCNT,ASM2UCNT  YES                                   04010000
         B     NONOPW                                                   04020000
         SPACE                                                          04030000
NOZUSE   CLC   SPECOPER,=C'NOPW'  REMOVE PASSWORD PROT ??  (FCB=NOPW)   04040000
         BNE   NONOPW             NO                                    04050000
         NI    DS1DSIND,X'FF'-X'14'  YES                                04060000
         SPACE                                                          04070000
NONOPW   TM    SECBITS,JFCBSPAC   SPACE SPECIFIED ?                     04080000
         BZ    NOSECQTY           NOPE                                  04090000
         MVC   DS1SCALO+1(3),SECQTY   YES - SET SECONDARY SPACE AMT     04100000
         SPACE                                                          04110000
NOSECQTY DS    0H                                                       04120000
         SPACE                                                          04130000
* WRITE THE DSCB BACK TO THE VTOC                                       04140000
         SPACE                                                          04150000
         ENQ   (SYSVTOC,VOLSER,E,,SYSTEMS)            DON'T BUMP HEADS *04160000
                                                      WITH DASDM        04170000
         SPACE                                                          04180000
         XC    IOBSEEK(3),IOBSEEK     ZERO MBB OF MBBCCHHR              04190000
         MVC   IOBSEEK+3(5),DSCBCCHH  FILL IN CCHHR OF MBBCCHHR         04200000
         XC    ECB,ECB                INITIALIZE ECB                    04210000
         SPACE                                                          04220000
         EXCP  IOB                WRITE THE DSCB BACK                   04230000
         SPACE                                                          04240000
         WAIT  ECB=ECB            WAIT FOR I/O TO COMPLETE              04250000
         SPACE                                                          04260000
         DEQ   (SYSVTOC,VOLSER,,SYSTEMS),RET=HAVE    LEGGO OF VTOC      04270000
         SPACE                                                          04280000
         TM    ECB,X'7F'          CHANNEL PROGRAM EXECUTE OKAY ???      04290000
         BNO   IOERROR            NOPE                                  04300000
         SPACE                                                          04310000
         CLOSE (DCB)              CLOSE VTOC                            04320000
         SPACE                                                          04330000
         MVC   WTOUPDAT+35(44),DSNAME      MOVE IN DSNAME               04340000
         MVC   WTOUPDAT+24(6),VOLSER       MOVE IN VOLSER               04350000
WTOUPDAT WTO   'DSCB UPDATED ON VOLSER FOR 1...5...10....5...20....5...*04360000
               30....5...40...4',ROUTCDE=11                             04370000
         SPACE 2                                                        04380000
         B     DDLOOP             DO IT ALL AGAIN                       04390000
         TITLE 'CLEANUP AND RETURN'                                     04400000
ENDTIOT  DS    0H                 END OF DD CARDS                       04410000
         SPACE                                                          04420000
         TM    DCB+48,X'10'       IS THE VTOC DCB OPEN ???              04430000
         BZ    RTN                NO - JUST RETURN                      04440000
         SPACE                                                          04450000
         CLOSE (DCB)              CLOSE VTOC DCB                        04460000
         SPACE                                                          04470000
         DEQ   (SYSVTOC,VOLSER,,SYSTEMS),RET=HAVE  MAKE SURE VTOC FREED 04480000
         SPACE                                                          04490000
RTN      L     R13,4(,R13)        -> OLD SAVE AREA                      04500000
         SR    R15,R15            RC = 0                                04510000
         RETURN (14,12),,RC=(15)                                        04520000
         TITLE 'ERROR EXITS'                                            04530000
BADJFCB  BAL   R14,GETRC          GET RC IN CHARACTERS                  04540000
         MVC   WTOJFCB+33(5),RC   MOVE TO MSG                           04550000
WTOJFCB  WTO   'RETURN CODE FROM RDJFCB =99999',ROUTCDE=11              04560000
         SPACE                                                          04570000
         ABEND 100,DUMP                                                 04580000
         SPACE 2                                                        04590000
INVALIDD MVC   WTODD+8(8),TIOEDDNM                                      04600000
WTODD    WTO   '-DDNAME- DD CARD INVALID, NO UPDATE PERFORMED',        *04610000
               ROUTCDE=11                                               04620000
         B     DDLOOP                                                   04630000
         SPACE 2                                                        04640000
NOCHANGE MVC   WTONOCHG+33(8),TIOEDDNM                                  04650000
WTONOCHG WTO   'NO CHANGES SPECIFIED FOR -DDNAME- DD CARD, IGNORED',   *04660000
               ROUTCDE=11                                               04670000
         B     DDLOOP                                                   04680000
         SPACE 2                                                        04690000
BADOBTAN BAL   R14,GETRC          GET RC IN CHARACTERS                  04700000
         MVC   WTOBTAIN+53(5),RC   MOVE TO MSG                          04710000
         MVC   WTOBTAIN+43(8),TIOEDDNM                                  04720000
WTOBTAIN WTO   'RETURN CODE FROM OBTAIN FOR DDNAME -DDNAME- =99999',   *04730000
               ROUTCDE=11                                               04740000
         B     DDLOOP                                                   04750000
         SPACE 2                                                        04760000
BADOPEN  BAL   R14,GETRC          GET RC IN CHARACTERS                  04770000
         MVC   WTOOPEN+46(5),RC   MOVE TO MSG                           04780000
         MVC   WTOOPEN+33(6),VOLSER                                     04790000
WTOOPEN  WTO   'RETURN CODE FROM OPEN ON VOLSER VTOC =99999',          *04800000
               ROUTCDE=11                                               04810000
         SPACE                                                          04820000
         ABEND 200,DUMP                                                 04830000
         SPACE 2                                                        04840000
GETRC    EQU   *                  SUBROUTINE TO MAKE RC PRINTABLE       04850000
         CVD   R15,DBLWRD         DECIMAL                               04860000
         OI    DBLWRD+7,X'0F'     INSURE RIGHT CHAR A EBDIC NUMERIC     04870000
         UNPK  RC(5),DBLWRD+5(3)  CHARACTER                             04880000
         BR    14                 RETURN                                04890000
         TITLE 'I/O ERROR HANDLING ROUTINE'                             04900000
         SPACE                                                          04910000
IOERROR  EQU   *                  I/O ERROR ANALYSIS ROUTINE            04920000
         SPACE                                                          04930000
         SYNADAF ACSMETH=EXCP,PARM1=IOB       GET SYNAD INFO            04940000
         SPACE                                                          04950000
         STM   14,12,12(R13)      SAVE REGISTERS IN NEW SAVE AREA       04960000
         SPACE                                                          04970000
         CLI   8(R1),C' '         ANY BINARY ADDRESSES ???              04980000
         BE    NOBIN              NOPE                                  04990000
         SPACE                                                          05000000
         UNPK  22(7,R1),8(4,R1)   UNPACK BUFFER ADDRESS                 05010000
         MVC   29(1,R1),11(R1)    GET RIGHT DIGIT                       05020000
         NC    22(8,R1),=8X'0F'   PUT WITHIN RANGE OF XLATE TABLE       05030000
         TR    22(8,R1),=C'0123456789ABCDEF'   CONVERT TO HEX           05040000
         SPACE                                                          05050000
         UNPK  39(3,R1),12(2,R1)  UNPACK NO BYTES READ                  05060000
         MVC   42(1,R1),13(R1)    GET RIGHT DIGIT                       05070000
         NC    39(4,R1),=8X'0F'   PUT WITHIN RANGE OF XLATE TABLE       05080000
         TR    39(4,R1),=C'0123456789ABCDEF'   CONVERT TO HEX           05090000
         SPACE                                                          05100000
NOBIN    MVC   8(14,R1),=C'I/O ERROR,BFR='     FILL                     05110000
         MVC   30(9,R1),=C',NOBYTES='          IN                       05120000
         MVC   43(4,R1),=C',CC='               LABELS                   05130000
         SPACE                                                          05140000
         UNPK  47(1,R1),ECB(1)    UNPACK COND CODE                      05150000
         MVC   48(1,R1),ECB       GET RIGHT DIGIT                       05160000
         NC    47(2,R1),=8X'0F'   PUT WITHIN RANGE OF XLATE TABLE       05170000
         TR    47(2,R1),=C'0123456789ABCDEF'   CONVERT TO HEX           05180000
         SPACE                                                          05190000
* CONSTRUCT A PARAMETER LIST FOR WTO SVC                                05200000
         SPACE                                                          05210000
         LR    R5,R1              SAVE REGISTER ONE                     05220000
         L     R3,4(,R1)          AND PREFIX                            05230000
         L     R4,128(,R1)        AND SUFFIX                            05240000
         SPACE                                                          05250000
         MVC   6(2,R5),=X'8000'   MCS FLAGS FOR WTO                     05260000
         MVC   128(4,R5),=X'00000020'  SET ROUTING AND DESCRIPTOR CODES 05270000
         SPACE                                                          05280000
         WTO   MF=(E,4(5))        WRITE TO PROGRAMMER  (ROUTCDE=11)     05290000
         SPACE                                                          05300000
         LR    R1,R5              RESTORE REGISTER ONE                  05310000
         ST    R3,4(,R1)          AND PREFIX                            05320000
         ST    R4,128(,R1)        AND SUFFIX                            05330000
         LM    14,12,12(R13)      AND REST OF REGISTERS                 05340000
         SPACE                                                          05350000
         SYNADRLS                                                       05360000
         SPACE                                                          05370000
         SNAPSHOT ON                                                    05380000
         SNAPSHOT OPEN,SNAPDCB=YES                                      05390000
         SNAPSHOT PDATA=(PSW,REGS,SA,JPA,SPLS)                          05400000
         SNAPSHOT CLOSE                                                 05410000
         SPACE                                                          05420000
         B     ENDTIOT            ENDTIOT WILL DO CLEANUP               05430000
         SPACE 2                                                        05440000
         SNAPSHOT DCB,DDNAME=SNAP                                       05450000
         SNAPSHOT OFF                                                   05460000
         TITLE 'CONSTANTS'                                              05470000
SYSVTOC  DC    CL8'SYSVTOC'       QNAME TO ENQ ON VTOC                  05480000
         SPACE                                                          05490000
ADSCB    CAMLST SEARCH,DSNAME,VOLSER,DSCB+44                            05500000
         SPACE 2                                                        05510000
DCB      DCB   DSORG=DA,DEVD=DA,MACRF=E,KEYLEN=44,RECFM=FS,            C05520000
               EXLST=JFCBLIST,DDNAME=ZAP                                05530000
         SPACE 2                                                        05540000
ECB      DS    F                  ECB FOR I/O TO WRITE DSCB BACK        05550000
         SPACE 3                                                        05560000
JFCBLIST DC    X'87',AL3(JFCB)                                          05570000
         SPACE 4                                                        05580000
IOB      DS    0F                 IOB START                             05590000
IOBFLAG1 DC    X'42'              CMD CHAINING,UNRELATED CHAN PGM       05600000
IOBFLAG2 DC    X'00'                                                    05610000
IOBSENS0 DC    X'00'              SENSE                                 05620000
IOBSENS1 DC    X'00'              BYTES                                 05630000
IOBECBCC DC    X'00'              COMPLETION CODE OF I/O EVENT          05640000
IOBECBPT DC    AL3(ECB)           -> ECB POSTED WHEN I/O EVENT FINISHES 05650000
IOBFLAG3 DC    X'00'                                                    05660000
IOBCSW   DC    XL7'00'            LOW 7 BYTES OF CHANNEL STATUS WORD    05670000
IOBSIOCC DC    X'00'              COND CODE FOR SIO INSTRUCTION         05680000
IOBSTART DC    AL3(CHANPGM)       -> CHANNEL PROGRAM TO EXECUTE         05690000
         DC    X'00'                                                    05700000
IOBDCBPT DC    AL3(DCB)           -> DCB FOR DATASET                    05710000
IOBRESTR DC    F'0'                                                     05720000
IOBINCAM DC    X'0000'            AMOUNT TO INCREMENT DCBBLKCT FIELD    05730000
IOBERRCT DC    X'0000'            NUMBER OF RETRIES DURING ERROR OPER   05740000
IOBSEEK  DC    XL8'00'            SEEK ADDRESS FOR CHANNEL PROGRAM      05750000
         SPACE 2                                                        05760000
CHANPGM  DS    0D                 CHANNEL PROGRAM START                 05770000
SEARCH   CCW   X'31',IOBSEEK+3,X'40',5       SEARCH ID EQ FOR CCHHR     05780000
TIC      CCW   X'08',SEARCH,0,0              TIC TO SEARCH IF NOT FOUND 05790000
WRITE    CCW   X'0D',DSCB,0,140              WRITE DSCB                 05800000
         TITLE 'FORMAT ONE DATA-SET CONTROL BLOCK'                      05810000
DSCB     DS    0F                                                       05820000
         IECSDSL1 (1)                                                   05830000
DSCBCCHH DS    CL5                 CCHHR OF DSCB RETURNED BY OBTAIN     05840000
         DS    CL47                REST OF OBTAIN'S 148 BYTE WKAREA     05850000
         SPACE                                                          05860000
ASM2UCNT EQU   IECSDSL1+45,3       ASM2 VER 2.4 USE COUNT               05870000
         SPACE 3                                                        05880000
         LTORG                                                          05890000
         SPACE 3                                                        05900000
         TITLE 'JOB FILE CONTROL BLOCK'                                 05910000
JFCB     DS    0F                                                       05920000
         IEFJFCBN LIST=NO                                               05930000
         TITLE 'TASK INPUT/OUTPUT TABLE DSECT'                          05940000
TIOT     DSECT                                                          05950000
         IEFTIOT1                                                       05960000
         TITLE 'UNIT CONTROL BLOCK DSECT'                               05970000
UCB      DSECT                                                          05980000
         IEFUCBOB LIST=NO                                               05990000
         TITLE 'WORKAREAS'                                              06000000
ZAPDSCB  CSECT                                                          06010000
         SPACE                                                          06020000
DBLWRD   DS    D                  WORKAREA FOR CVD INSTRUCTIONS         06030000
SAVEAREA DS    18F                                                      06040000
         SPACE                                                          06050000
DSNAME   DS    CL44               DSNAME                                06060000
VOLSER   DS    CL6                VOLSER                                06070000
CREDT    DS    CL3                CREATION DATE  (CURRENT DATE)         06080000
EXPDT    DS    CL3                EXPIRATION DATE                       06090000
KEYLEN   DS    CL1                KEY LENGTH                            06100000
DSORG    DS    CL2                DSORG                                 06110000
RECFM    DS    CL1                RECFM                                 06120000
BLKSIZE  DS    CL2                BLKSIZE                               06130000
LRECL    DS    CL2                LRECL                                 06140000
RKP      DS    CL2                RKP                                   06150000
OPTCD    DS    XL1                OPTCD                                 06160000
SPECOPER DS    CL4                SPECIAL OPERATIONS WORD               06170000
PSWDBITS DS    XL1                PASSWORD PROTECTION INDICATORS        06180000
SECQTY   DS    AL3                SECONDARY SPACE QUANITY               06190000
SECBITS  DS    XL1                SPACE ALLOC INDICATORS                06200000
DSCBINFO EQU   EXPDT,*-EXPDT                                            06210000
RC       DS    CL5                AREA FOR CONVERTING RC'S TO CHAR      06220000
         SPACE 2                                                        06230000
         END                                                            06240000
/*                                                                      06250000
//                                                                      06260000
