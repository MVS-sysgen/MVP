//PTFSEL  JOB (TSO),
//             'Install PTFSEL',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//A          EXEC     PGM=IFOX00,REGION=4096K,
//             PARM='NODECK,OBJECT'
//SYSLIB       DD       DSN=SYS1.MACLIB,DISP=SHR
PTFS     TITLE 'PTF SELECTION PROGRAM    -  BY LIONEL DYCK '            00010007
*********************************************************************** 00020004
*   PTFSEL                                                            * 00030004
*                                                                     * 00040004
*       THIS PROGRAM IS DESIGNED TO COPY AND LIST EITHER              * 00050004
*       ALL OR SELECTED PTFS FROM A PTF FILE .                        * 00060004
*                                                                     * 00070004
*       A LISTING OF ALL SELECTED PTF'S COVER LETTERS                 * 00080004
*       IS PRODUCED ALONG WITH A RECORD COUNT FOR EACH                * 00090004
*       PTF AND A CUMULATIVE PTF AND RECORD COUNT.                    * 00100004
*                                                                     * 00110004
*       A SUMMARY LISTING IS PRODUCED OF ALL SELECTED PTF'S           * 00120004
*       IN A SORTED ORDER WITH A COUNT OF HOW MANY TIMES              * 00130004
*       THAT PTF WAS FOUND AND A RECORD COUNT OF THE                  * 00140004
*       FIRST OCCURANCE OF THAT PTF, WITH THE PAGE NUMBER             * 00150004
*       WHERE THE PTF STARTS.                                         * 00160004
*                                                                     * 00170004
*       IT WILL SELECT ALL PTFS IF PARM=ALL IS SPECIFIED              * 00180004
*       OTHERWISE IT WILL EXPECT TO FIND CONTROL CARDS                * 00190004
*       IN THE SYSIN DD FILE WHICH WILL DEFINE THOSE PTFS             * 00200004
*       TO BE WORKED UPON.                                            * 00210004
*                                                                     * 00220004
*       A PARM OF SUMM WILL CAUSE ONLY THE SUMMARY REPORT             * 00230004
*       TO BE PRODUCED.                                               * 00240004
*                                                                     * 00250004
*       THE FORMAT OF THE SYSIN DD CONTROL CARD IS:                   * 00260004
*                                                                     * 00270004
*       BEGINNING IN COLUMN 1 CODE THE PTF NUMBER FOLLOWED            * 00280004
*       BY A COMMA AND THEN THE NEXT THRU THE END OF THE CARD         * 00290004
*       OR YOU MAY CODE ONE PTF NUMBER PER CARD OR A VARIABLE         * 00300004
*       NUMBER OF PTFS PER CARD SO LONG AS THERE IS A COMMA           * 00310004
*       SEPEREATING THE PTF NUMBER AND THE LAST ENTRY ON THE          * 00320004
*       CARD HAS NO COMMA AFTER IT.                                   * 00330004
*                                                                     * 00340004
*       AN * IN COLUMN 1 OF ANY INPUT CARD DENOTES A                  * 00350004
*       COMMENT CARD                                                  * 00360004
*                                                                     * 00370004
*       THE INPUT CARD SCAN BE TERMINATED WITH END AND                * 00380004
*       THIS WILL TERMINATE THE READING OF THE                        * 00390004
*       SELECTION PTF #'S..                                           * 00400004
*       NOTE THAT THIS WILL HELP WITH TSO.....!!                      * 00410004
*                                                                     * 00420004
*       IF A PTF NUMBER IS FOUND TO BE BLANK THE                      * 00430004
*       SCAN OF THAT CARD WILL BE TERMINATED AND                      * 00440004
*       AN ERROR MESSAGE WILL BE PRINTED....                          * 00450004
*                                                                     * 00460004
*                                                                     * 00470004
*       IF PTFOUT DD IS MISSING THEN THE COPY WILL BE                 * 00480004
*       IGNORED.............!                                         * 00490004
*                                                                     * 00500004
*       THE JCL TO EXECUTE PTFSEL ARE:                                * 00510004
*                                                                     * 00520004
*          //PTFSEL   EXEC  PGM=PTFSEL                                * 00530004
*          //SYSPRINT  DD SYSOUT=A                                    * 00540004
*          //PTFIN     DD DSN=PTF-FILE                                * 00550004
*          //PTFOUT    DD DSN=PTF-OUTPUT FILE OR DUMMY                * 00560004
*                      (NOTE: A DCB IS REQUIRED FOR THIS FILE         * 00570004
*                       IF USED).                                     * 00580004
*          //SYSIN     DD *                                           * 00590004
*          UZ12345,UZ67890,ETC    CONTROL CARDS                       * 00600004
*          END                                                        * 00610004
*          /*                                                         * 00620004
*                                                                     * 00630004
*       THIS PROGRAM WAS WRITTEN BY:                                  * 00640004
*                                                                     * 00650004
*********************************************************************** 00660004
*                                                                     * 00670004
*       LIONEL B. DYCK                                                * 00680004
*       ROCKWELL INTERNATIONAL                                        * 00690004
*       INFORMATION SYSTEMS CENTER                                    * 00700004
*       2201 SEAL BEACH BLVD.                                         * 00710004
*       SEAL BEACH, CALIF. 90740                                      * 00720004
*       MAIL CODE 041-SH48                                            * 00730004
*       (213) 594-2520                                                * 00740004
*                                                                     * 00750004
*   NO WARRENTY EXPRESSED OR IMPLIED IS CONNNECTED WITH               * 00760004
*   THE USE OF THIS PROGRAM BY ANYONE INCLUDING THE AUTHOR......      * 00770004
*                                                                     * 00780004
*                                                                     * 00790004
*********************************************************************** 00800004
*        MODIFICATION LEVEL 1.3 - 05/11/79     LIONEL DYCK            * 00810007
*        MODIFICATION LEVEL 1.4 - 07/30/81     LIONEL DYCK            * 00820007
*********************************************************************** 00830007
         EJECT                                                          00840004
*********************************************************************** 00850004
         MACRO                                                          00000010
&NAME    PRIME &REG=12,&SAVE=,&ID=,&LV=0                                00000020
.* PRIME WILL SAVE REGISTERS, CREATE A NEW SAVE AREA, CHAIN THE NEW     00000030
.* SAVE AREA TO THE CALLERS SAVE AREA, ESTABLISH ADDRESSABILITY,        00000040
.* OPTIONALLY OBTAIN ADDITIONAL STORAGE AND IN GENERAL PERFORM THE      00000050
.* NECESSARY ENTRY LINKAGE.                                             00000060
.* IF THE USER DOES NOT SPECIFY WHAT REGISTER HE REQUIRES FOR A BASE,   00000070
.* TWELVE IS USED.  NOTE..IF REG= IS SPECIFIED TO BE 0, 1, 13, 14, OR   00000080
.* 15  A WARNING MESSAGE IS ISSUED.                                     00000090
.* IF THE USER SPECIFIES A SAVE AREA VIA THE SAVE= OPERAND THEN PRIME   00000100
.* WILL CHAIN THE USERS SAVE AREA TO THE CALLERS SAVE AREA. IF THE      00000110
.* USER DOES NOT SPECIFY SAVE,THEN PRIME WILL OBTAIN A NEW SAVE AREA    00000120
.* VIA THE GETMAIN SVC.  AN ADDITIONAL AMOUNT OF STORAGE MAY BE         00000130
.* OBTAINED AT THIS TIME BY SPECIFIEING THE LV=, OPERAND.  IF LV IS NOT 00000140
.* SPECIFIED IN REGISTER NOTATION, 4023 IS THE MAXIMUM VALUE.           00000150
.* IF THE USER SPECIFIES SAVE=NONE, PRIME WILL NOT OBTAIN A NEW AREA.   00000160
.* REG 13 WILL POINT TO THE NEW 18 WORD SAVE AREA FOLLOWED BY THE EXTRA 00000170
.* STORAGE AREA.                                                        00000180
         LCLA  &A,&B                                                    00000190
         LCLC  &E,&F,&G,&H                                              00000200
         AIF   ('&NAME' EQ '').NONAME                                   00000210
&NAME    DS    0H                                                       00000220
.NONAME  ANOP                                                           00000230
         AIF   ('&ID' EQ '').CONT4                                      00000240
         AIF   ('&ID' EQ '*').SPEC                                      00000250
&A       SETA  ((K'&ID+2)/2)*2+4                                        00000260
         USING *,&REG                                                   00000270
&A       SETA  K'&ID                                                    00000280
         DC    AL1(&A)                                                  00000290
.CONTB   AIF   (&A GT 32).SPLIT                                         00000300
.CONTAA  AIF   (&A GT 8).BRAK                                           00000310
&E       SETC  '&ID'(&B+1,&A)                                           00000320
         DC    CL&A'&E'                                                 00000330
         AGO   .CONT1                                                   00000340
.BRAK    ANOP                                                           00000350
&E       SETC  '&ID'(&B+1,8)                                            00000360
         DC    CL8'&E'                                                  00000370
&B       SETA  &B+8                                                     00000380
&A       SETA  &A-8                                                     00000390
         AGO   .CONTAA                                                  00000400
.SPLIT   ANOP                                                           00000410
&E       SETC  '&ID'(&B+1,8)                                            00000420
&F       SETC  '&ID'(&B+9,8)                                            00000430
&G       SETC  '&ID'(&B+17,8)                                           00000440
&H       SETC  '&ID'(&B+25,8)                                           00000450
         DC    CL32'&E.&F.&G.&H'                                        00000460
&B       SETA  &B+32                                                    00000470
&A       SETA  &A-32                                                    00000480
         AGO   .CONTB                                                   00000490
.SPEC    AIF   ('&NAME' EQ '').CSECTN                                   00000500
&E       SETC  '&NAME'                                                  00000510
&A       SETA  1                                                        00000520
.CONTQ   AIF   ('&E'(1,&A) EQ '&E').LVE                                 00000530
&A       SETA  &A+1                                                     00000540
         AGO   .CONTQ                                                   00000550
.LVE     ANOP                                                           00000560
&B       SETA  ((&A+2)/2)*2+4                                           00000570
         USING *,&REG                                                   00000580
         DC    AL1(&A)                                                  00000590
         DC    CL&A'&E'                                                 00000600
         AGO   .CONT1                                                   00000610
.CSECTN  AIF   ('&SYSECT' EQ '').E4                                     00000620
&E       SETC  '&SYSECT'                                                00000630
&A       SETA  1                                                        00000640
         AGO   .CONTQ                                                   00000650
.E4      IHBERMAC 78,360                                                00000660
.CONT4   ANOP                                                           00000670
         USING *,&REG                                                   00000680
.CONT1   ANOP                                                           00000690
         AIF   ('&REG' EQ '0' OR '&REG' EQ '1' OR '&REG' EQ '13').ERR1  00000700
         AIF   ('&REG' EQ '14' OR '&REG' EQ '15').ERR1                  00000710
.CONT3   ANOP                                                           00000720
         DS    0H                                                       00000730
         STM   14,12,12(13)             SAVE REGS IN CALLERS AREA       00000740
         LR    &REG,15                  SET TRUE BASE EQUAL EP          00000750
         AIF ('&SAVE' EQ 'NONE').A                                      00000760
         AIF   ('&SAVE' EQ '').C                                        00000770
         AIF   ('&SAVE'(1,1) EQ '(').F                                  00000780
         LA    1,&SAVE                  SET R1 EQ USERS SAVE ADDRESS    00000790
         AGO   .D                                                       00000800
.F       ANOP                                                           00000810
         AIF   ('&SAVE' EQ '(1)').D                                     00000820
         LR    1,&SAVE(1)               PICK UP USERS SAVE AREA         00000830
         AGO   .D                                                       00000840
.C       ANOP                                                           00000850
         AIF   ('&LV'(1,1) EQ '(').E                                    00000860
         AIF   ('&LV' GT '4023').ERR2                                   00000870
         LA    0,&LV.+72(0,0)           PARAMETER FOR GETMAIN           00000880
         AGO   .CONT2                                                   00000890
.E       ANOP                                                           00000900
         AIF   ('&LV' EQ '(0)').CONT2                                   00000910
         LA    0,72(&LV(1).,0)          PARAMETER FOR GETMAIN           00000920
.CONT2   ANOP                                                           00000930
         BAL   1,*+4                    INDICATE GETMAIN                00000940
         SVC   10                       ISSUE SVC 10                    00000950
         ST    0,0(0,1)                 SAVE LENGTH IN 1ST WORD         00000960
.D       ANOP                                                           00000970
         XC    4(68,1),4(1)             CLEAR AREA                      00000980
         ST    1,8(0,13)                CHAIN FORWARD                   00000990
         ST    13,4(0,1)                CHAIN BACKWARD                  00001000
         LM    0,1,20(13)               RESET R0 AND R1                 00001010
         L     13,8(0,13)               SET SAVE AREA REGISTER          00001020
.A       ANOP                                                           00001030
         MEXIT                                                          00001040
.ERR1    MNOTE 'ILLEGAL BASE REGISTER SPECIFIED'                        00001050
         AGO   .CONT3                                                   00001060
.ERR2    MNOTE 'LV GT 4023, LV REQUEST NOT HONORED'                     00001070
         LA    0,72(0,0)                PARAMETER FOR GETMAIN           00001080
         AGO   .CONT2                                                   00001090
         MEND                                                           00001100
         MACRO                                                          00000010
&NAME    TERME &OP=,&SAVE=,&LV=0                                        00000020
.* TERME WILL RESTORE REGISTERS, UNCHAIN SAVE AREAS, RELEASE DYNAMIC    00000030
.* STORAGE OBTAINED BY THE PRIME MACRO, RESET THE PGM MASK AND IN       00000040
.* GENERAL PERFORM THE NECESSARY EXIT LINKAGE.                          00000050
.* TERME WILL FREE THE USERS SAVE AREA PLUS THE NUMBER OF BYTES         00000060
.* SPECIFIED BY THE LV OPERAND UNLESS THE USER IMPLIES THAT THE SAVE    00000070
.* AREA WAS NOT OBTAINED VIA A GETMAIN.  THE USER IMPLIES THIS BY       00000080
.* SPECIFYING SAVE= SOME VALUE OR SYMBOL.                               00000090
.* IF THE USER SPECIFIED SAVE=NONE IN THE PRIME MACRO, HE SHOULD DO     00000100
.*  THE SAME FOR TERME.                                                 00000110
.* THE USER MAY CHOOSE TO FOLLOW THE TERME MACRO WITH THE XCTL MACRO    00000120
.* RATHER THAN BRANCH ON 14.  THE USER ACCOMPLISHES THIS BY CODING      00000130
.* OP=XCTL.                                                             00000140
         AIF   ('&NAME' EQ '').F                                        00000150
&NAME    DS    0H                                                       00000160
.F       ANOP                                                           00000170
         AIF ('&SAVE' EQ 'NONE').C                                      00000180
         L     13,4(0,13)              GET PTR TO USERS AREA            00000190
         STM   0,1,20(13)               TEMPORARY SAVE OF R0 AND R1     00000200
         AIF   ('&SAVE' NE '').C                                        00000210
         AIF   ('&LV'(1,1) EQ '(').E                                    00000220
         AIF   ('&LV' EQ '0').CONT                                      00000230
         AIF   ('&LV' GT '4023').ERR                                    00000240
         LA    0,&LV.+72(0,0)           PARAMETER FOR FREEMAIN          00000250
         AGO   .CONT                                                    00000260
.E       ANOP                                                           00000270
         AIF   ('&LV' EQ '(0)').CONT                                    00000280
         LA    0,72(&LV(1).,0)          PARAMETER FOR FREEMAIN          00000290
.CONT    ANOP                                                           00000300
         L     1,8(0,13)                GET SAVE AREA ADDRESS           00000310
         LA    1,0(0,1)                 INDICATE FREEMAIN               00000320
         AIF   ('&LV' NE '0').CONTC                                     00000330
         L     0,0(0,1)                 LENGTH IS IN 1ST WORD OF AREA   00000340
.CONTC   ANOP                                                           00000350
         SVC   10                       ISSUE SVC 10                    00000360
.C       ANOP                                                           00000370
         L     14,12(0,13)              RESTORE                         00000380
         LM    0,12,20(13)              REGISTERS                       00000390
         XC    8(4,13),8(13)            DEQUEUE SAVE AREA               00000400
         AIF   ('&OP' EQ 'XCTL').D                                      00000410
         BR    14                       RETURN TO CALLER                00000420
         MEXIT                                                          00000430
.D       ANOP                                                           00000440
         BALR  15,0                     ESTABLISH ADDRESSABILITY        00000450
         USING *,15                                                     00000460
         MEXIT                                                          00000470
.ERR     MNOTE 'LV GT 4023, LV REQUEST NOT HONORED'                     00000480
         LA    0,72(0,0)                PARAMETER FOR FREEMAIN          00000490
         AGO   .CONT                                                    00000500
         MEND                                                           00000510
         MACRO                                                          00860004
&LABEL   PARM                                                           00870004
.*       THIS MACRO IS DESIGNED TO MOVE THE PARM INTO A DC DEFINED      00880004
.*       WITHIN THE USER PROGRAM BY THE NAME OF PARM                    00890004
.*       IT WILL MOVE THE EXACT LENGTH OF THE PARM VIA AN EX INSTR.     00900004
&LABEL   L     1,0(1)                                                   00910004
         LH    2,0(1)                                                   00920004
         EX    2,#&SYSNDX                                               00930004
         B     @&SYSNDX                                                 00940004
#&SYSNDX MVC   PARM(0),2(1)                                             00950004
@&SYSNDX EQU   *                                                        00960004
         MEND                                                           00970004
*********************************************************************** 00980004
         EJECT                                                          00990004
PTFSEL   CSECT                                                          01000013
R0       EQU   0                                                        01010012
R1       EQU   1                                                        01020012
R2       EQU   2                                                        01030012
R3       EQU   3                                                        01040012
R4       EQU   4                                                        01050012
R5       EQU   5                                                        01060012
R6       EQU   6                                                        01070012
R7       EQU   7                                                        01080012
R8       EQU   8                                                        01090012
R9       EQU   9                                                        01100012
R10      EQU   10                                                       01110012
R11      EQU   11                                                       01120012
R12      EQU   12                                                       01130012
R13      EQU   13                                                       01140012
R14      EQU   14                                                       01150012
R15      EQU   15                                                       01160012
         EJECT                                                          01170012
PTFSEL   CSECT                                                          01180013
         PRIME 12,,,,X                                                  01190013
         EJECT                                                          01200004
         PARM                                                           01210004
*-------------------------------------------------------------*         01220004
         GETMAIN R,LV=TBLSIZ   GETMAIN SIZE FOR PTF TABLE               01230004
*-------------------------------------------------------------*         01240004
         ST    R1,PTFTBL       STORE START OF PTF TABLE                 01250004
         A     R1,=AL4(TBLSIZ)   ADD LENGTH                             01260004
         LM    R9,R11,PTFTBL   -> TO PTF SELECTION TABLE                01270004
         SR    R1,R10          SUB 1 ENTRY                              01280004
         SR    R1,R10          DO IT 1 MORE TIME                        01290004
         ST    R1,PTFLAST      AND STORE IT                             01300004
         LR    R11,R1          PLACE INTO R11                           01310004
         USING ENT,R9                                                   01320004
         EJECT                                                          01330004
*        INITIALIZE THE PTF TABLE                                       01340004
CLRLP    MVC   PTF#(4),=4X'FF'                                          01350004
         ZAP   PTFRECS,=P'00'                                           01360004
         NI    PTFSW,X'00'                                              01370004
         BXLE  R9,R10,CLRLP                                             01380004
         LM    R9,R11,PTFTBL   -> TO PTF SELECTION TABLE                01390004
         SR    R3,R3         ZERO R3                                    01400004
         EJECT                                                          01410004
         OPEN  (SYSPRINT,OUTPUT)                                        01420004
*------------------------------------------------------------------*    01430004
*    PROCESS THE PARM AND DETERMINE IF ALL OR INDIVIDUAL SELECTION *    01440004
*    IS TO BE PERFORMED.............                               *    01450004
*------------------------------------------------------------------*    01460004
         CLC   PARM(4),=C'SUMM' SUMMARY REPORT ON LIST ONLY             01470004
         BNE   CLCALL          NO                                       01480004
         OI    SW,SUMMLIST     SET SUMM LIST BIT                        01490004
         B     OPENS           AND DO ALL                               01500004
CLCALL   CLC   PARM(3),=C'ALL' SELECT ALL ?                             01510004
         BE    OPENS           YES SO DO NOT SET SWITCH / BYPASS SYSIN  01520004
         OI    SW,SELO         TURN ON SELECT OPTION BIT                01530004
         OPEN  SYSIN                                                    01540004
         PUT   SYSPRINT,TITLE                                           01550004
         SPACE                                                          01560004
*------------------------------------------------------------------*    01570004
GETA     GET   SYSIN                                                    01580004
         LR    R4,R1           -> INPUT RECORD                          01590004
         MVC   WORKA(80),0(R4) PUT INPUT RECORD INTO WORK               01600004
         PUT   SYSPRINT,WORK   PUT INPUT RECORD                         01610004
         CLI   0(R4),C'*'      COMMENT CARD ?                           01620004
         BE    GETA            YES - BYPASS IT                          01630004
MVC      CLC   0(3,R4),=CL3'END'  END FOR TSO AND EASE OF CODING        01640004
         BE    SYSINE           FINISH THIS PROCESSING                  01650004
         CLI   0(R4),C' '      BLANK ENTRY                              01660004
         BE    BLANKCRD        GO PRINT ERROR MESSAGE                   01670004
         MVC   PTF#,0(R4)      MOVE PTF# INTO SELECTION TABLE           01680004
         ST    R9,LASTENT     SAVE -> LAST ENTRY                        01690004
         BXLE  R9,R10,CLI      INCRIMENT TABLE -> AND CONT              01700004
*------------------------------------------------------------------*    01710004
         ABEND 1               PASSED END OF PTF TABLE             *    01720004
*------------------------------------------------------------------*    01730004
BLANKCRD PUT   SYSPRINT,WORK   PUT INPUT RECORD                         01740004
         PUT   SYSPRINT,BADCARD  PUT ERROR MESSAGE                      01750004
         B     GETA            CONTINUE READ                            01760004
*------------------------------------------------------------------*    01770004
CLI      CLI   7(R4),C','      MORE ON CARD ?                           01780004
         BNE   GETA            NO SO GET THE NEXT CARD                  01790004
         LA    R4,8(R4)        INCR R4                                  01800004
         B     MVC             GO GET NEXT PTF ON THIS CARD             01810004
         SPACE 2                                                        01820004
SYSINE   CLOSE (SYSIN)                                                  01830004
         EJECT                                                          01840004
OPENS    OPEN  (PTFIN,,PTFOUT,OUTPUT)                                   01850004
         LH    R6,=H'01'     INITIALIZE R6                              01860004
*------------------------------------------------------------------*    01870004
READIT   GET   PTFIN                     * READ THE PTF FILE       *    01880004
*------------------------------------------------------------------*    01890004
         LR    R4,R1         -> INPUT RECORD                            01900004
         CLC   0(2,R4),=C'++' SMP CONTROL CARD?                         01910004
         BE    SMPCRD         YES                                       01920004
         TM    SW,PTFO        ARE WE ACCEPTING PTF'S NOW                01930004
         BO    READIT         NO                                        01940004
         TM    SW1,JCLINSW    JCLIN PROCESSING ?                        01950004
         BO    PUTSYS         YES - PRINT IT                            01960004
         TM    SW,VERO        CONTROL CARD PROCESSING                   01970004
         BO    VERCD           GO PROCESS VER CARD                      01980004
         BZ    PUTIT          NO                                        01990004
         SPACE 2                                                        02000004
PUTSYS   EQU   *         CODE TO PRINT THE DATA                         02010004
         TM    SW,SUMMLIST     SUMM LIST ONLY                           02020004
         BO    PUTIT           YES BY-PASS THE SYSPRINT                 02030004
         MVC   WORKA(80),0(R4) MOVE TO PRINT WORK AREA                  02040004
         PUT   SYSPRINT,WORK  PRINT IT                                  02050004
         AP    LCTR,=P'01'    ADD 1 TO LINE COUNTER                     02060004
         CP    LCTR,LINECNT   LINES YET ON THIS PAGE                    02070004
         BL    PUTIT          NO OK                                     02080004
         AP    PGCTR,=P'01'   INCR PAGE CTR                             02090004
         MVC   PAGE#(6),=X'402020202021'                                02100004
         ED    PAGE#(6),PGCTR  EDIT PAGE NUMBER                         02110004
         PUT   SYSPRINT,PTFLINE                                         02120004
         PUT   SYSPRINT,BLANKLNE PUT BLANK LINE                         02130004
         ZAP   LCTR,=P'02'    REINITIALIZE LINE COUNTER                 02140004
*------------------------------------------------------------------*    02150004
PUTIT    EQU   *                                                        02160004
         AP    CTR,=P'01'     INCR COUNTER FOR TOTAL RECORDS            02170004
         TM    SW,PTFADD      USE REAL COUNTER OR NOT                   02180004
         BZ    ADDIT          YES                                       02190004
         AP    XCTR,=P'01'    INCR ALT CTR                              02200004
         B     TMOUT                                                    02210004
*------------------------------------------------------------------*    02220004
ADDIT    AP    PTFRECS,=P'01' INCR COUNTER FOR PTF RECORDS              02230004
*------------------------------------------------------------------*    02240004
TMOUT    TM    PTFOUT+X'30',X'10'  DCB OPEN ??                          02250004
         BZ    READIT         NO - IGNORE PUT                           02260004
         PUT   PTFOUT,(4)     PUT IT ON OUTPUT                          02270004
         B     READIT         CONTINUE                                  02280004
         EJECT                                                          02290004
*------------------------------------------------------------------*    02300004
*      PROCESS THE SMP CONTROL CARD AND DETERMINE IS IT IS         *    02310004
*      A  ++VER ++PTF ++IF ++FUNCTION ++APAR ++USERMOD ++JCLIN     *    02320004
*      THE SCAN WILL STOP WHEN A ( IS ENCOUNTERED ...              *    02330004
*------------------------------------------------------------------*    02340004
SMPCRD   LR    R5,R4          -> START OF CARD                          02350004
         LA    R7,50(R5)      -> LAST COLUMN TO SCAN        *HMD 07/81* 02360007
         NI    SW,X'FE'       TURN OFF VER CARD PROCESSING BIT          02370004
         NI    SW1,X'80'      TURN OFF ALL CARD PROCESSING BIT          02380004
*------------------------------------------------------------------*    02390004
LOOP1    CLC   2(3,R5),=C'PTF'   IS IT A PTF CARD                       02400004
         BE    PTF            YES  -  GO SEE IF WE WANT IT ???          02410004
         CLC   2(4,R5),=C'APAR'  IS IT A APAR CARD (SMP 4)              02420004
         BE    PTF            YES  -  GO SEE IF WE WANT IT ???          02430004
         CLC   2(8,R5),=C'FUNCTION' IS IT A FUNCTION CARD (SMP 4)       02440004
         BE    PTF            YES  -  GO SEE IF WE WANT IT ???          02450004
         CLC   2(7,R5),=C'USERMOD' IS IT A USERMOD CARD (SMP 4)         02460004
         BE    PTF            YES  -  GO SEE IF WE WANT IT ???          02470004
         CLC   2(3,R5),=C'VER' IS IT A VER CARD                         02480004
         BE    VER             YES - WE WANT TO SEE ALL OF IT           02490004
         CLC   2(2,R5),=C'IF'  IS IT AN 'IF' CARD??         *HMD 07/81* 02500007
         BE    VER             YES - TREAT AS IF VER        *HMD 07/81* 02510007
         CLC   2(5,R5),=C'JCLIN' IS IT A VER CARD                       02520004
         BE    JCLIN           YES - WE WANT TO SEE ALL OF IT           02530004
*------------------------------------------------------------------*    02540004
         BXLE  R5,R6,LOOP1                                              02550004
*------------------------------------------------------------------*    02560005
         TM    SW,PTFO        ARE WE ACCEPTING PTF'S NOW           *    02570005
         BO    READIT         NO                                   *    02580005
*------------------------------------------------------------------*    02590005
         B     SMPC               DEFAULT                               02600004
*------------------------------------------------------------------*    02610004
PSE      EQU   *                                                        02620004
         NI    SW1,X'FC'      TURN OFF SMP AND VER BITS                 02630004
         NI    SW,X'FE'       TURN OFF VER CARD PROCESSING BIT          02640004
         B     PUTSYS          GO PRINT AND PUT IT                      02650004
PS       EQU   *               PRINT IT AND PUT ROUTINE                 02660004
         TM    SW,PTFO         ARE WE ACCEPTING PTF'S NOW               02670004
         BO    READIT          NO                                       02680004
         B     PUTSYS          GO PRINT AND PUT IT                      02690004
         EJECT                                                          02700004
PTF      EQU   *                                                        02710004
         NI    SW1,X'7F'       TURN OFF FMIDHAVE BIT                    02720004
PTFL     CLI   2(R5),C'('      FIND START OF PTF                        02730004
         BE    PTFX            YES - GO SEE IF WE WANT IT ??            02740004
         BXLE  R5,R6,PTFL      CONTINUE TILL WE FIND IT                 02750004
         B     PS              GIVE UP AND PRINT AND PUT IT             02760004
*------------------------------------------------------------------*    02770004
PTFX     EQU   *                                                        02780004
         TM    SW,PTFO         SELECTION IN PROGRESS ?                  02790004
         BO    NI                                                       02800004
*------------------------------------------------------------------*    02810004
         TM    SW,FIRST        FIRST PASS ?                             02820004
         BZ    NI              YES BYPASS PTF COUNT                     02830004
*------------------------------------------------------------------*    02840004
         TM    SW,SUMMLIST                                              02850004
         BO    NI                                                       02860004
         MVC   XCOUNT(6),PTFCNT MOVE EDIT PATTERN                       02870004
         TM    SW,PTFADD       REGULAR OR ALTERNATE COUNTER             02880004
         BZ    EDRECS          REGULAR OK                               02890004
         ED    XCOUNT,XCTR     EDIT ALT PTF RECORD COUNTER              02900004
         B     PUTCNT                                                   02910004
*------------------------------------------------------------------*    02920004
EDRECS   ED    XCOUNT,PTFRECS  EDIT PTF RECORD COUNT                    02930004
*------------------------------------------------------------------*    02940004
PUTCNT   PUT   SYSPRINT,CNTLINE AND PUT IT ON SYSPRINT                  02950004
*------------------------------------------------------------------*    02960004
NI       NI    SW,X'FB'        TURN OFF THE PTF NO ACCEPT BIT           02970004
         NI    SW,X'DF'        TURN OFF ALT COUNT USE BIT               02980004
         OI    SW,FIRST        TURN ON FIRST PASS                       02990004
         LM    R9,R11,PTFTBL   -> TO PTF SELECTION TABLE                03000004
*------------------------------------------------------------------*    03010004
PTFSLP   CLC   3(7,R5),PTF#    PTF # IN TABLE                           03020004
         BE    OK              YES - WE DO WANT IT                      03030004
         CLC   ENTB(4),=4X'FF'  END OF TABLE                            03040004
         BE    NOTFND          NOT IN TABLE - IGNORE IT                 03050004
         BXLE  R9,R10,PTFSLP   CONTINUE TILL FOUND                      03060004
*------------------------------------------------------------------*    03070004
NOTFND   TM    SW,SELO         SELECTION ON ??                          03080004
         BZ    OK              NO DO ALL                                03090004
         OI    SW,PTFO         TURN ON NON-SELECTION BIT                03100004
         B     READIT          AND CONTINUE                             03110004
*------------------------------------------------------------------*    03120004
OK       EQU   *               WE ARE ACCEPTING THIS PTF                03130004
         CLC   PTF#,3(R5)      PTF #'S = ??                             03140004
         BE    ZAP             YES                                      03150004
         CLI   PTF#,X'FF'   NULL ENTRY ?                                03160004
         BE    PTFMV                                                    03170004
         BXLE  R9,R10,MVCPTF                                            03180004
*------------------------------------------------------------------*    03190004
MVCPTF   CLC   PTF#,3(R5)      PTF#'S = ?                               03200004
         BE    ZAP             YES                                      03210004
         CLI   PTF#,X'FF'   NULL ENTRY ?                                03220004
         BE    PTFMV                                                    03230004
         BXLE  R9,R10,MVCPTF   CONTINUE SCAN                            03240004
*------------------------------------------------------------------*    03250004
PTFMV    MVC   PTF#,3(R5)      MOVE PTF # INTO TABLE                    03260004
         ST    R9,LASTENT      SAVE -> LAST ENTRY                       03270004
*------------------------------------------------------------------*    03280004
ZAP      EQU   *                                                        03290004
*------------------------------------------------------------------*    03300004
ICPTFSW  IC    R3,PTFSW                                                 03310004
         AH    R3,=H'01'       INCR PTFSW BY 1                          03320004
         STC   R3,PTFSW        AND SAVE IT                              03330004
         CLI   PTFSW,X'01'     ONE COUNT ?                              03340004
         BNH   READ                                                     03350004
         OI    SW,PTFADD       SET TO USE ALT COUNTER                   03360004
         B     ZAPX                                                     03370004
*------------------------------------------------------------------*    03380004
ZAPX     ZAP   XCTR,=P'00'     INIT ALT COUNTER                         03390004
         B     UPPTF                                                    03400004
*------------------------------------------------------------------*    03410004
READ     ZAP   PTFRECS,=P'00'  INITITALIZE PTF RECORD COUNT             03420004
*------------------------------------------------------------------*    03430004
UPPTF    EQU   *               UPDATE PTF PAGE COUNTER                  03440004
         AP    PCTR,=P'01'     INCRIMENT PTF COUNTER                    03450004
         TM    SW,SUMMLIST     SUMM LIST ON SYSPRINT ONLY               03460004
         BO    PS              YES - BY-PASS THE HEADER                 03470004
         MVC   PTFLINE#,3(R5)  MOVE PTF# TO HEADER LINE                 03480004
         AP    PGCTR,=P'01'    INCR PAGE COUNTER                        03490004
         TM    SW,PTFADD       ALT OR REGULAR COUNT ?                   03500004
         BZ    PGMVC           REGULAR SO MOVE IT                       03510004
         B     EDPAGE          ALT SO - GO EDIT IT                      03520004
*------------------------------------------------------------------*    03530004
PGMVC    MVC   PTFPAGE,PGCTR   PLACE INTO ENTRY                         03540004
*------------------------------------------------------------------*    03550004
EDPAGE   MVC   PAGE#(6),=X'402020202021'                                03560004
         ED    PAGE#(6),PGCTR  EDIT PAGE NUMBER                         03570004
         PUT   SYSPRINT,PTFLINE  AND PRINT IT                           03580004
         PUT   SYSPRINT,BLANKLNE PUT BLANK LINE                         03590004
         ZAP   LCTR,=P'02'    REINITIALIZE LINE COUNTER                 03600004
         B     PS              NOW GO PRINT RECORD AND COPY IT          03610004
         SPACE 2                                                        03620004
*------------------------------------------------------------------*    03630004
VER      EQU   *               VERIFY CARD PROCESSING              *    03640004
*------------------------------------------------------------------*    03650005
         TM    SW,PTFO        ARE WE ACCEPTING PTF'S NOW           *    03660005
         BO    READIT         NO                                   *    03670005
*------------------------------------------------------------------*    03680005
         OI    SW,VERO         TURN ON VERIFY CARD PROCESSING      *    03690004
         B     VERFM                                               *    03700004
*------------------------------------------------------------------*    03710004
VERCD    EQU   *                                                        03720004
         LR    R5,R4           GO FIND FMID                             03730004
         TM    SW1,SLASH          * FIND END SLASH ?                    03740004
         BO    FINDBSLS           YES                                   03750004
         TM    SW1,FMIDHAVE       IF ?                                  03760004
         BO    SMPC               * TREAT AS OTHER                      03770004
         TM    SW1,VERSW          * TRUE VER CARD                       03780004
         BO    VERFM              YES                                   03790004
         TM    SW1,SMPSW          OTHER ?                               03800004
         BO    SMPCD              * GO PROCESS AS IF                    03810004
*------------------------------------------------------------------*    03820004
VERFM    LR    R5,R4           GO FIND FMID                             03830004
         LA    R7,60(R5)      -> LAST COLUMN TO SCAN                    03840004
*------------------------------------------------------------------*    03850004
VLP      EQU   *                                                        03860004
         CLC   0(4,R5),=C'FMID'                                         03870004
         BE    FMID                                                     03880004
         CLC   0(2,R5),=C'/*'                                           03890004
         BE    BSLASH             YES - GO FIND END SLASH               03900004
         CLI   0(R5),C'.'                                               03910004
         BE    PSE                                                      03920004
         BXLE  R5,R6,VLP                                                03930004
         B     PS              AND GO PRINT AND PUT IT                  03940004
*------------------------------------------------------------------*    03950004
JCLIN    EQU   *                                                        03960004
*------------------------------------------------------------------*    03970005
         TM    SW,PTFO        ARE WE ACCEPTING PTF'S NOW           *    03980005
         BO    READIT         NO                                   *    03990005
*------------------------------------------------------------------*    04000005
         OI    SW1,JCLINSW      TURN ON VERIFY CARD PROCESSING          04010004
         B     PUTSYS           AND GO PRINT AND PUT IT                 04020004
*------------------------------------------------------------------*    04030004
SMPC     EQU   *                  * IF PROCESSING                       04040004
         OI    SW,VERO         TURN ON VERIFY CARD PROCESSING           04050004
         OI    SW1,SMPSW        TURN ON VERIFY CARD PROCESSING          04060004
*------------------------------------------------------------------*    04070004
SMPCD    EQU   *               GO FIND FMID                             04080004
         TM    SW1,SLASH          * FIND END SLASH ?                    04090004
         BO    FINDBSLS           YES                                   04100004
         LR    R5,R4           GO FIND FMID                             04110004
         LA    R7,70(R5)      -> LAST COLUMN TO SCAN                    04120004
*------------------------------------------------------------------*    04130004
SMPCL    EQU   *                                                        04140004
         CLC   0(2,R5),=C'/*'                                           04150004
         BE    BSLASH             YES - GO FIND END SLASH               04160004
         CLI   0(R5),C'.'                                               04170004
         BE    PSE                                                      04180004
         BXLE  R5,R6,SMPCL                                              04190004
         B     PS    GIVE UP ***                                        04200004
*------------------------------------------------------------------*    04210004
BSLASH   OI    SW1,SLASH              * SET SWITCH FOR BACK SLASH       04220004
*------------------------------------------------------------------*    04230004
FINDBSLS EQU   *                                                        04240004
         CLC   0(2,R5),=C'*/'                                           04250004
         BE    FINDPERD           END OF CARD                           04260004
         BXLE  R5,R6,FINDBSLS                                           04270004
         B     PS    GIVE UP ***                                        04280004
*------------------------------------------------------------------*    04290004
FINDPERD EQU   *                                                        04300004
         CLI   0(R5),C'.'                                               04310004
         NI    SW1,X'FB'          TURN OFF SLASH BIT                    04320004
         BE    PSE                END OF CARD                           04330004
         BXLE  R5,R6,FINDPERD                                           04340004
         B     PS    GIVE UP ***                                        04350004
*------------------------------------------------------------------*    04360004
FMID     EQU   *                                                        04370004
*------------------------------------------------------------------*    04380004
FMIDL    CLI   4(R5),C'('      FIND FMID                                04390004
         BE    FMIDFD                                                   04400004
         CLC   0(2,R5),=C'/*'                                           04410004
         BE    BSLASH             YES - GO FIND END SLASH               04420004
         CLI   0(R5),C'.'                                               04430004
         BE    PSE                                                      04440004
         BXLE  R5,R6,FMIDL                                              04450004
         B     PS    GIVE UP ***                                        04460004
*------------------------------------------------------------------*    04470004
FMIDFD   EQU   *                                                        04480004
         MVC   PTFFMID,5(R5)   MOVE FMID TO TABLE                       04490004
         OI    SW1,FMIDHAVE       * SET FMID FOUND 1 PER PTF            04500004
         B     PS                  AND PRINT IT                         04510004
*------------------------------------------------------------------*    04520004
         EJECT                                                          04530004
PTFEND   EQU   *               END OF PTF PROCESSING                    04540004
         MVC   PTFLAST,LASTENT   -> LAST ENTRY                          04550004
         TM    SW,PTFO         SELECTION IN PROGRESS ?                  04560004
         BO    ECNT                                                     04570004
         TM    SW,SUMMLIST     SUMM LIST ON SYSPRINT ONLY               04580004
         BO    ECNT            YES - BY-PASS THE LAST PTF COUNT         04590004
         MVC   XCOUNT(6),PTFCNT MOVE EDIT PATTERN                       04600004
         ED    XCOUNT,PTFRECS  EDIT PTF RECORD COUNT                    04610004
         PUT   SYSPRINT,CNTLINE AND PUT IT ON SYSPRINT                  04620004
*------------------------------------------------------------------*    04630004
ECNT     ED    COUNT,CTR       RECORD COUNTER                           04640004
         ED    PTFCNT,PCTR     PTF COUNTER                              04650004
*------------------------------------------------------------------*    04660004
         PUT   SYSPRINT,ENDLINE                                         04670004
         EJECT                                                          04680004
*****************************************************************       04690004
*                                                               *       04700004
*        SORT THE PTF ENTRIES IN THE TABLE                      *       04710004
*        INTO ASCENDING ORDER                                   *       04720004
*                                                               *       04730004
*****************************************************************       04740004
         SPACE 2                                                        04750004
         XC    SW,SW              CLEAR SWITCH                          04760004
SORTPTFS LM    R9,R11,PTFTBL        -> PTF TABLE                        04770004
         NI    SW,X'EF'             TURN OFF THE SORT BIT               04780004
*------------------------------------------------------------------*    04790004
SORTLP   CLI   ENTB,X'FF'           NULL ENTRY                          04800004
         BE    SORTEND              YES  -  END THIS SORT PASS          04810004
         CLC   PTF#,ENTB            COMPARE THIS AND NEXT ENTRY         04820004
         BNH   SORTBXLE             LESS OR EQUAL TO - OK               04830004
*------------------------------------------------------------------*    04840004
         MVC   ENTSAVE,ENTA         SAVE THIS ENTRY                     04850004
         MVC   ENTA,ENTB            REPLACE IT WITH NEXT                04860004
         MVC   ENTB,ENTSAVE         REPLACE NEXT WITH THIS              04870004
         OI    SW,SORT              TURN ON SORT SWITCH                 04880004
*------------------------------------------------------------------*    04890004
SORTBXLE BXLE  R9,R10,SORTLP        AND CONTINUE THE SORT               04900004
*------------------------------------------------------------------*    04910004
         SPACE 2                                                        04920004
SORTEND  EQU   *                                                        04930004
         TM    SW,SORT              SORT BIT ON ?                       04940004
         BZ    ENDSORT              YES - LEAVE THIS MESS               04950004
         B     SORTPTFS             TRY ANOTHER PASS                    04960004
*------------------------------------------------------------------*    04970004
ENDSORT  B     LPRINT               EXIT THE SORT HERE                  04980004
*------------------------------------------------------------------*    04990004
         EJECT                                                          05000004
*****************************************************************       05010004
*                                                               *       05020004
*        SORT THE PTF ENTRIES IN THE TABLE                      *       05030004
*        INTO FMID ORDER                                        *       05040004
*                                                               *       05050004
*****************************************************************       05060004
         SPACE 2                                                        05070004
$ORTFMID LM    R9,R11,PTFTBL        -> PTF TABLE                        05080004
         OI    SW,$ORT              TURN ON FMID SORT BIT               05090004
         NI    SW,X'EF'             TURN OFF THE SORT BIT               05100004
*------------------------------------------------------------------*    05110004
$ORTLP   CLI   ENTB,X'FF'           NULL ENTRY                          05120004
         BE    $ORTEND              YES  -  END THIS SORT PASS          05130004
         CLC   PTFFMID,$TFFMID      COMPARE THIS AND NEXT ENTRY         05140004
         BNH   $ORTBXLE             LESS OR EQUAL TO - OK               05150004
*------------------------------------------------------------------*    05160004
         MVC   ENTSAVE,ENTA         SAVE THIS ENTRY                     05170004
         MVC   ENTA,ENTB            REPLACE IT WITH NEXT                05180004
         MVC   ENTB,ENTSAVE         REPLACE NEXT WITH THIS              05190004
         OI    SW,SORT              TURN ON SORT SWITCH                 05200004
*------------------------------------------------------------------*    05210004
$ORTBXLE BXLE  R9,R10,$ORTLP        AND CONTINUE THE SORT               05220004
*------------------------------------------------------------------*    05230004
         SPACE 2                                                        05240004
$ORTEND  EQU   *                                                        05250004
         TM    SW,SORT              SORT BIT ON ?                       05260004
         BZ    END$ORT              YES - LEAVE THIS MESS               05270004
         B     $ORTFMID             TRY ANOTHER PASS                    05280004
*------------------------------------------------------------------*    05290004
END$ORT  EQU   *                    EXIT THE SORT HERE                  05300004
*------------------------------------------------------------------*    05310004
         EJECT                                                          05320004
LPRINT   LM    R9,R11,PTFTBL   LOAD PTF TABLE ->'S                      05330004
         SR    R3,R3                                                    05340004
*------------------------------------------------------------------*    05350004
EHEAD    PUT   SYSPRINT,HEAD                                            05360004
         PUT   SYSPRINT,BLANKLNE                                        05370004
*------------------------------------------------------------------*    05380004
         ZAP   LCTR,=P'02'    REINITIALIZE LINE COUNTER                 05390004
ELOOP    EQU   *                                                        05400004
*------------------------------------------------------------------*    05410004
         CLI   PTF#,X'FF'                                               05420004
         BE    ENDIT                                                    05430004
*------------------------------------------------------------------*    05440004
         MVC   OPTF,PTF#                                                05450004
         MVC   OPFMID,PTFFMID                                           05460004
         MVC   OPTFR(6),=X'402020202021'                                05470004
         MVC   OPAGE(6),=X'402020202021'                                05480004
         ED    OPTFR(6),PTFRECS                                         05490004
         ED    OPAGE(6),PTFPAGE                                         05500004
*------------------------------------------------------------------*    05510004
         MVI   OPTFSW,C'*'                                              05520004
         MVC   COMM(9),=C'NOT FOUND'                                    05530004
         SLR   R3,R3                                        *HMD 07/81* 05540009
         IC    R3,PTFSW                                                 05550004
         LTR   R3,R3                                                    05560004
         BZ    PUTE                                                     05570004
*------------------------------------------------------------------*    05580004
         MVC   COMM(9),=CL9' '                                          05590004
         CVD   R3,DOUBLEWK               CONVERT PTF COUNT  *HMD 07/81* 05600010
         MVC   WORDWK(4),=X'40202120'    MOVE EDIT PATTERN  *HMD 07/81* 05610010
         ED    WORDWK(4),DOUBLEWK+6      UNPACK THE VALUE   *HMD 07/81* 05620010
         MVC   OPTFSW(3),WORDWK+1        MOVE IN THE COUNT  *HMD 07/81* 05630010
*------------------------------------------------------------------*    05640004
PUTE     PUT   SYSPRINT,ELINE                                           05650004
         MVC   OPFMID,=CL7' '                                           05660004
         MVC   COMM(9),=CL9' '                                          05670004
         AP    LCTR,=P'01'                                              05680004
         CP    LCTR,LINECNT                                             05690004
         BH    EHEAD                                                    05700004
*------------------------------------------------------------------*    05710004
BXLE     BXLE  R9,R10,ELOOP                                             05720004
*------------------------------------------------------------------*    05730004
ENDIT    TM    SW,$ORT                                                  05740004
         BZ    $ORTFMID                                                 05750004
         CLOSE (SYSPRINT,,PTFIN,,PTFOUT)                                05760004
*------------------------------------------------------------------*    05770004
         TERME    ,                                                     05780011
         EJECT                                                          05790004
         LTORG                                                          05800004
         EJECT                                                          05810004
         DS    0D                                                       05820009
DOUBLEWK DC    D'0'            DOUBLE WORD WORK AREA        *HMD 07/81* 05830009
PGCTR    DC    PL3'0'          PAGE COUNTER                             05840004
PARM     DC    CL5' '          PARM                                     05850004
WORK     DC    C' '            CC FOR PTF                               05860004
WORKA    DC    CL80' '         WORK AREA FOR PTF RECORD FOR PRINT       05870007
*                                                           *HMD 07/81* 05880007
         SPACE                                                          05890004
LASTENT  DS    F               -> LAST PTF ENTRY                        05900004
SW1      DC    X'00'           SWITCH FOR VER PROCESSING                05910004
VERSW    EQU   1                  VER CARD                              05920004
SMPSW    EQU   2                  IF CARD                               05930004
SLASH    EQU   4                  /* FOUND                              05940004
JCLINSW  EQU   8                  JCLIN SWITCH                          05950004
FMIDHAVE EQU   128                FMID FOUND FOR THIS PTF               05960004
*                                                                       05970004
SW       DC    X'00'           SWITCH FOR PROCESSING                    05980004
VERO     EQU   1               VERIFY CARD PROCESSING                   05990004
SELO     EQU   2               PTF SELECTION OPTION                     06000004
*                              0 = SELECT   1 = ALL                     06010004
PTFO     EQU   4               ACCEPT PTF OPTION                        06020004
*                              0 = YES THIS PTF IS ACCEPTED             06030004
*                              1 = SKIP TILL FIND NEXT PTF              06040004
FIRST    EQU   8               0 = FIRST  1 = NOT FIRST                 06050004
SORT     EQU   16              0 = CLEAN SORT PASS  1 = DID SORT        06060004
PTFADD   EQU   32              0 = USE NORMAL CTR   1 = USE ALTERNATE   06070004
SUMMLIST EQU   64              SUMM LISTING ONLY IF 1                   06080004
$ORT     EQU   128             0 = CLEAN SORT PASS  1 = DID SORT        06090004
         SPACE                                                          06100004
ENTSAVE  DS    CL21            SAVE FOR SORT                            06110004
CTR      DC    PL4'00'         COUNTER FOR RECORDS PRODUCED ON OP       06120004
PCTR     DC    PL3'00'         PTF COUNTER                              06130004
LCTR     DC    PL3'0'          LINE COUNTER                             06140004
WORDWK   DC    XL4'00'         WORK AREA FOR EDIT           *HMD 07/81* 06150010
LINECNT  DC    PL2'58'                   * LINES PER PAGE   *HMD 07/81* 06160007
XCTR     DC    PL3'0'          ALT COUNTER                              06170004
SFMID    DC    CL7' '          FMID SAVE AREA                           06180004
BLANKLNE DC    CL80' '          BLANK LINE                              06190007
PTFLINE  DC    C'1',CL20' '                                             06200004
         DC    C'PTF '                                                  06210004
PTFLINE# DC    CL7' '                                                   06220004
         DC    C' CONTROL INFORMATION',CL10' ',C'PAGE ='                06230007
PAGE#    DC    CL6' ',CL30' '                                           06240007
ENDLINE  DC    C'1',CL2' '                                              06250004
         DC    C'PTF SELECTION  RECORDS PROCESSED'                      06260004
         DC    C' = '                                                   06270004
COUNT    DC    X'4020202020202021',CL3' ',C'PTFS = '                    06280004
PTFCNT   DC    X'402020202021',CL40' '                                  06290007
CNTLINE  DC    C'0'                                                     06300004
XCOUNT   DC    CL6' ',C' RECORDS PROCESSED FOR THIS PTF',CL50' '        06310007
TITLE    DC    CL80'1    PTF SELECTION RECORDS '                        06320007
HEAD     DC    CL5'1'                                                   06330004
         DC    CL7'  PTF#',CL3' ',C'FOUND',CL2' ',CL7'RECORDS',CL4' '   06340004
         DC    C'   FMID ',CL4' '                                       06350004
         DC    C'PAGE #',CL28' '                                        06360009
ELINE    DC    CL5' '                                                   06370004
OPTF     DC    CL7' '                                                   06380004
         DC    CL5' '                                                   06390004
OPTFSW   DC    CL1' '                                                   06400004
         DC    CL5' '                                                   06410004
OPTFR    DC    CL6' '                                                   06420004
         DC    CL4' '                                                   06430004
OPFMID   DC    CL7' '                                                   06440004
         DC    CL4' '                                                   06450004
OPAGE    DC    CL6' '                                                   06460004
         DC    CL5' '                                                   06470004
COMM     DC    CL9' '                                                   06480004
         DC    CL50' '                                                  06490007
BADCARD  DC    CL80'***** THIS CARD CONTAINS A BLANK PTF NUMBER *****'  06500007
*                                                           *HMD 07/81* 06510007
         EJECT                                                          06520004
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,LRECL=80,BLKSIZE=6160,X06530008
               RECFM=FBA,BUFNO=5                                        06540007
         EJECT                                                          06550007
SYSIN    DCB   DSORG=PS,MACRF=GL,DDNAME=SYSIN,EODAD=SYSINE              06560004
         EJECT                                                          06570007
PTFIN    DCB   DSORG=PS,MACRF=GL,DDNAME=PTFIN,EODAD=PTFEND              06580004
         EJECT                                                          06590007
PTFOUT   DCB   DSORG=PS,MACRF=PM,DDNAME=PTFOUT,LRECL=80,RECFM=FB        06600004
         EJECT                                                          06610007
PTFTBL   DC    A(0)          FIRST ENTRY IN PTFTBL                      06620004
         DC    A(21)         LENGTH OF PTF TABLE ENTRY                  06630004
PTFLAST  DC    A(0)          END OF TABLE                               06640004
         SPACE 2                                                        06650007
TBLDSEC  DSECT                                              *HMD 07/81* 06660007
TBL      DS   9000CL21       9000 21 BYTE ENTRIES FOR PTFS  *HMD 07/81* 06670007
TBLSIZ   EQU  *-TBL          LENGTH OF TABLE                            06680004
         EJECT                                                          06690007
ENT      DSECT                                                          06700004
ENTA     DS    CL21          ENTRY FOR PTF                              06710004
         ORG   ENTA                                                     06720004
PTF#     DS    CL7           PTF NUMBER                                 06730004
PTFSW    DS    CL1           PTF FOUND COUNT                            06740004
PTFRECS  DS    CL3           PTF RECORD COUNT - FOR FIRST OCCURANCE     06750004
PTFPAGE  DS    CL3           PTF PAGE NUMBER  - FOR FIRST OCCURANCE     06760004
PTFFMID  DS    CL7           PTF FMID                                   06770004
ENTLNGH  EQU   *-PTF#        LENGTH OF 1 PTF ENTRY                      06780004
ENTB     DS    CL21          ENTRY FOR NEXT PTF                         06790004
         ORG   ENTB                                                     06800004
$TF#     DS    CL7           PTF NUMBER                                 06810004
$TFSW    DS    CL1           PTF FOUND COUNT                            06820004
$TFRECS  DS    CL3           PTF RECORD COUNT - FOR FIRST OCCURANCE     06830004
$TFPAGE  DS    CL3           PTF PAGE NUMBER  - FOR FIRST OCCURANCE     06840004
$TFFMID  DS    CL7           PTF FMID                                   06850004
ENTL2    EQU   *-PTF#        LENGTH OF 2 ENTRIES                        06860004
         END                                                            06870004
//SYSUT1       DD       UNIT=SYSDA,
//             SPACE=(CYL,(5,2),,CONTIG)
//SYSUT2       DD       UNIT=SYSDA,
//             SPACE=(CYL,(5,2),,CONTIG)
//SYSUT3       DD       UNIT=SYSDA,
//             SPACE=(CYL,(5,2),,CONTIG)
//SYSPRINT     DD       SYSOUT=*,
//             DCB=(RECFM=FM,LRECL=121,BLKSIZE=121)
//SYSGO        DD       DSN=&&LOADSET,DISP=(MOD,PASS),
//             UNIT=SYSDA,SPACE=(2960,(111,12)),DCB=BLKSIZE=2960
//***
//***
//L          EXEC     PGM=IEWL,
//             PARM='XREF,LIST,LET,DCBS',
//             REGION=1024K
//SYSUT1       DD       UNIT=SYSDA,
//             SPACE=(CYL,(5,2),,CONTIG)
//SYSPRINT     DD       SYSOUT=*
//SYSLMOD      DD       DSN=SYS2.LINKLIB,
//             DISP=OLD
//SYSLIN       DD       DSN=&&LOADSET,DISP=(OLD,DELETE,DELETE)
//             DD       *
  NAME     PTFSEL(R)
//
