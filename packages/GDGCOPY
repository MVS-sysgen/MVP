//GDGCOPY  JOB (TSO),
//             'Install GDGCOPY',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00020000
//MACROS   EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW                      00030000
//SYSUT2   DD  DSN=GDGCOPY.MACLIB,DISP=(NEW,PASS,DELETE),               00040000
//             UNIT=SYSDA,VOL=SER=PUB001,                               00050000
//             SPACE=(TRK,(5,,1)),DCB=(SYS1.MACLIB)                     00060000
//SYSPRINT DD  DUMMY                                                    00070000
//SYSIN    DD  *                                                        00080000
./ ADD NAME=$REGS                                                       00090000
         MACRO                                                          00100000
         $REGS &LIST=NO                                                 00110000
.********************************************************************** 00120000
.*                                                                    * 00130000
.*                                  CHANEY SYSTEMS SUPPORT, INC.      * 00140000
.*                                                                    * 00150000
.*       THIS MACRO PROVIDES STANDARD BASE REGISTER EQUATES FOR       * 00160000
.*       ALL "CSS" SUPPORT ROUTINES.                                  * 00170000
.*                                                                    * 00180000
.********************************************************************** 00190000
         GBLB  &REGS                                                    00200000
         AIF   (&REGS).MEXIT                                            00210000
&REGS    SETB  1                                                        00220000
         AIF   ('&LIST' NE 'YES').LIST1                                 00230000
         PUSH  PRINT               SAVE CURRENT PRINT SETTINGS          00240000
         PRINT OFF                 TEMPORARILY TURN OFF PRINT           00250000
.LIST1   ANOP                                                           00260000
R0       EQU   0                                                        00270000
R1       EQU   1                                                        00280000
R2       EQU   2                                                        00290000
R3       EQU   3                                                        00300000
R4       EQU   4                                                        00310000
R5       EQU   5                                                        00320000
R6       EQU   6                                                        00330000
R7       EQU   7                                                        00340000
R8       EQU   8                                                        00350000
R9       EQU   9                                                        00360000
R10      EQU   10                                                       00370000
R11      EQU   11                                                       00380000
R12      EQU   12                                                       00390000
R13      EQU   13                                                       00400000
R14      EQU   14                                                       00410000
R15      EQU   15                                                       00420000
         AIF   ('&LIST' NE 'YES').MEXIT                                 00430000
         POP   PRINT               RESTORE CURRENT PRINT SETTINGS       00440000
.MEXIT   ANOP                                                           00450000
         MEND                                                           00460000
./ ADD NAME=$PROLOG                                                     00470000
         MACRO                                                          00480000
&LABEL   $PROLOG &LV=,&RENT=NO,&ERRCODE=,&C=,&SP=,&GM=,&LIST=NO         00490000
.********************************************************************** 00500000
.*                                                                    * 00510000
.*                                  CHANEY SYSTEMS SUPPORT, INC.      * 00520000
.*                                                                    * 00530000
.*       THIS MACRO PROVIDES STANDARD LINKAGE AND BASE REGISTER       * 00540000
.*       SPECIFICATIONS FOR MOST MEMBERS OF THE CSS TOOL KIT.         * 00550000
.*                                                                    * 00560000
.*       THE FIRST OPERERAND IS A POSITIONAL LIST OF REGISTERS        * 00570000
.*       TO BE USED AS BASE(S) FOR THE CSECT BEING DEFINED AND        * 00580000
.*       THE LABEL BECOMES THE CSECT NAME.                            * 00590000
.*                                                                    * 00600000
.*       LV=NNN    SPECIFIES AN ADDITIONAL AMOUNT OF STORAGE TO BE    * 00610000
.*                 GOTTEN FOLLOWING THE DYNAMIC SAVE AREA.  THIS      * 00620000
.*                 ADDITIONAL STORAGE IS ADDRESSED VIA REG 13         * 00630000
.*                 (JUST FOLLOWING THE 18 FULLWORD SAVEAREA) AND      * 00640000
.*                 IS LIMITED TO 4023.                                * 00650000
.*                                                                    * 00660000
.*       RENT=YES  IF USED, SPECIFIES THAT THE LENGTH OF ADDITIONAL   * 00670000
.*                 STORAGE IS DETERMINED BASED UPON THE SIZE OF       * 00680000
.*                 THE DYNAMIC STORAGE DEFINED BETWEEN TWO            * 00690000
.*                 LABELS:  "SAVEAREA" AND "WORKEND".  THE WORK       * 00700000
.*                 AREA IS ADDRESSABLE VIA REG 13 THE SAME AS THE     * 00710000
.*                 LV= OPERAND.  THE USER DEFINED CONSTANTS THAT      * 00720000
.*                 EXIST IN THE SOURCE ARE COPIED TO THE NEWLY        * 00730000
.*                 AQUIRED STORAGE AND ARE DIRECTLY ADDRESSABLE.      * 00740000
.*                 CAUTION: THE DYNAMIC AREA MUST BE DEFINED          * 00750000
.*                          "IMMEDIATELY PRIOR TO ANY DSECTS" AND     * 00760000
.*                          YOU MUST SPECIFY "LTORG" PRIOR TO THE     * 00770000
.*                          DEFINITION OF "SAVEAREA".                 * 00780000
.*                                                                    * 00790000
.*                 NOTE: LV= AND RENT=YES ARE MUTUALLY EXCLUSIVE.     * 00800000
.*                                                                    * 00810000
.*       ERRCODE=  SPECIFIES THE RETURN CODE TO BE RETURNED TO        * 00820000
.*                 THE CALLER IN THE EVENT THAT THE CONDITIONAL       * 00830000
.*                 GETMAIN FAILS.  IF SPECIFIED, THE GETMAIN THAT     * 00840000
.*                 IS ISSUED WILL BE CONDITIONAL AND IF IT DOES       * 00850000
.*                 NOT COMPLETE NORMALLY, THIS ERROR CODE WILL        * 00860000
.*                 BE RETURNED TO THE CALLER.  IF ERRCODE IS NOT      * 00870000
.*                 SPECIFIED, THE GETMAIN THAT IS ISSUED WILL BE      * 00880000
.*                 UNCONDITIONAL WITH A RELATED 80A ABEND IN THE      * 00890000
.*                 EVENT OF FAILURE.                                  * 00900000
.*                                                                    * 00910000
.*       SP=       IS USED TO CAUSE AN MVS SPLEVEL MACRO TO BE        * 00920000
.*                 EXECUTED AS PART OF THE ASSEMBLY.  POSSIBLE        * 00930000
.*                 OPTIONS ARE "1" (370) OR "2" (XA).  IF NOT         * 00940000
.*                 SPECIFIED, THE SPLEVEL MACRO IS NOT USED.          * 00950000
.*                                                                    * 00960000
.*       C=        IS A MEANS OF PROVIDING ADDITIONAL DATA IN THE     * 00970000
.*                 EYECATCHER.  IF USED, THE DATA SPECIFIED MUST      * 00980000
.*                 BE ENCLOSED WITHIN QUOTES AND IS LIMITED TO        * 00990000
.*                 46 CHARACTERS.                                     * 01000000
.*                                                                    * 01010000
.*       GM=NO     IS NOT SUPPORTED BY THIS MACRO BUT IS ALLOWED      * 01020000
.*                 FOR COMPATIBILITY OF OLDER VERSIONS.               * 01030000
.*                                                                    * 01040000
.*       LIST=NO   SUPPRESSES GENERATION OF LISTINGS FOR $PROLOG,     * 01050000
.*                 $EPILOG AND $REGS WHEN EXPANSION IS ACTIVE         * 01060000
.*                 (PRINT GEN).                                       * 01070000
.*                                                                    * 01080000
.*       EXAMPLES:                                                    * 01090000
.*                                                                    * 01100000
.*       SECTNAME $PROLOG ,        R12 IS BASE BY DEFAULT             * 01110000
.*       SECTNAME $PROLOG R12,R11  R12 IS 1ST BASE AND R11 IS SECOND  * 01120000
.*       SECTNAME $PROLOG R2,LV=8  R2 IS BASE AND AN ADDITIONAL       * 01130000
.*                                 8 BYTES ARE ADDED TO THE STORAGE   * 01140000
.*                                 GOTTEN FOR THE SAVEAREA.           * 01150000
.*                                                                    * 01160000
.*       SECTNAME $PROLOG RENT=YES R12 IS BASE AND THE ADDITIONAL     * 01170000
.*                                 STORAGE TO BE GOTTEN IS DEFINED    * 01180000
.*                                 BETWEEN "SAVEAREA" AND "WORKEND".  * 01190000
.*       SAVEAREA DS    9D         (SAVE AREA FOR $PROLOG GENERATION) * 01200000
.*       MYFIELD1 DC    CL8'DATA1' (PROGRAM CONSTANTS)                * 01210000
.*       MYFIELD2 DC    CL8'DATA2' (PROGRAM CONSTANTS)                * 01220000
.*       WORKEND  EQU   *          (END OF DYNAMIC WORK AREA)         * 01230000
.*                                                                    * 01240000
.********************************************************************** 01250000
         GBLA  &EPILOG             DEFINE GLOBAL FOR $EPILOG            01260000
         GBLB  &REGS,&LSAVE        DEFINE GLOBALS FOR $REGS/$EPILOG     01270000
         LCLA  &AA,&AB,&BUMP,&X    DEFINE LOCAL VARIABLES               01280000
         LCLC  &GMT,&BASE,&LISTOPT DEFINE LOCAL VARIABLES               01290000
&X       SETA  &SYSNDX             SET LABEL CONSTANT                   01300000
&EPILOG  SETA  0                   RESET LV= GLOBAL                     01310000
&BUMP    SETA  4096                SET FOR BASE REG BUMP                01320000
&LSAVE   SETB  0                   RESET RENT GLOBAL FOR $EPILOG        01330000
&GMT     SETC  'RU'                SET UNCONDITIONAL GETMAIN            01340000
         AIF   ('&LIST' EQ 'YES').LIST1                                 01350000
         PUSH PRINT                                                     01360000
         PRINT OFF                                                      01370000
.LIST1   ANOP                                                           01380000
&LABEL   CSECT                                                          01390000
         AIF   (T'&SP EQ 'O').GO1  IF NO SPLEVEL REQUIRED               01400000
         SPLEVEL SET=&SP           ISSUE USER REQUESTED SPLEVEL MACRO   01410000
.GO1     ANOP                                                           01420000
         USING *,R15               TEMPORARY BASE                       01430000
         B     $&X.A               BRANCH AROUND CONSTANTS              01440000
         DC    CL8'&LABEL'         PROVIDE EYECATCHER                   01450000
         AIF   (T'&C EQ 'O').GO2   COMMENTS ADDITION?                   01460000
         DC    CL46&C                                                   01470000
.GO2     ANOP                                                           01480000
         DC    C'&SYSDATE @ &SYSTIME' DATE/TIME STAMP OBJECT            01490000
         AIF   (T'&LV   EQ 'O').GO3 IF LV= NOT SPECIFIED                01500000
         AIF   ('&RENT' NE 'YES').GO3 RENT NOT ALSO SPECIFIED           01510000
         MNOTE 12,'$PROLOG - RENT=YES AND LV=&LV MUTUALLY EXCLUSIVE'    01520000
         MEXIT                                                          01530000
.GO3     AIF   ('&RENT' EQ 'YES').GO4   RENT=YES SPECIFIED              01540000
         AIF   (T'&LV   NE 'O').GO4   LV= SPECIFIED                     01550000
&LSAVE   SETB  1                   SET NORENT GLOBAL FOR $EPILOG        01560000
$AVE&X   DC    18F'0'              DEFINED SAVE AREA                    01570000
.GO4     ANOP                                                           01580000
         AIF   (T'&LABEL NE 'O').GO5 INSURE CSECT NAME PROVIDED         01590000
         MNOTE 8,'$PROLOG - CSECT NAME NOT SUPPLIED'                    01600000
.GO5     ANOP                                                           01610000
$&X.A    STM   R14,R12,12(R13)     SAVE CALLERS REGISTERS               01620000
&BASE    SETC  'R12'               ASSUME A BASE REGISTER               01630000
         AIF   (N'&SYSLIST EQ 0).GO6 USE DEFAULT IF NOT SPECIFIED       01640000
&BASE    SETC  '&SYSLIST(1)'       SET THE SPECIFIED BASE REGISTER      01650000
.GO6     ANOP                                                           01660000
         LR    &BASE,R15           SET FIRST BASE REGISTER              01670000
         DROP  R15                 FREE THE TEMPORARY BASE              01680000
         USING &LABEL,&BASE        INFORM ASSEMBLER                     01690000
         AIF   (N'&SYSLIST EQ 0).GO7                                    01700000
&AA      SETA  2                   NUMBER TO DEFINE +1                  01710000
.LOOP    ANOP                                                           01720000
         AIF   (&AA GT N'&SYSLIST).GO7                                  01730000
&AB      SETA  &AA-1               NUMBER OF LAST BASE REG DEFINED      01740000
         LA    &SYSLIST(&AA),2048(&SYSLIST(&AB)) SET NEXT BASE REG      01750000
         LA    &SYSLIST(&AA),2048(&SYSLIST(&AA)) SET NEXT BASE REG      01760000
         USING &LABEL+&BUMP,&SYSLIST(&AA) INFORM THE ASSEMBLER          01770000
&BUMP    SETA  &BUMP+4096          BUMP INDEX                           01780000
&AA      SETA  &AA+1               BUMP CONTROL COUNT                   01790000
         AGO   .LOOP                                                    01800000
.GO7     AIF   (T'&ERRCODE EQ 'O').GO8 IF ERROR CODE NOT SPECIFIED      01810000
&GMT     SETC  'RC'                ERROR CODE WAS SPECIFIED             01820000
.GO8     AIF   (T'&LV   NE 'O').GO10 LV= SPECIFIED, DO GETMAIN          01830000
         AIF   ('&RENT' EQ 'YES').GO12 RENT SPECIFIED, DO GETMAIN       01840000
         AIF   (T'&ERRCODE EQ 'O').GO9 IF ERROR CODE NOT SPECIFIED      01850000
      MNOTE 8,'$PROLOG - ERRCODE=&ERRCODE INVALID WITHOUT RENT=YES/LV=' 01860000
.GO9     ANOP                                                           01870000
$&X.B    LA    R2,$AVE&X           ADDRESS OF SAVE AREA                 01880000
         AGO   .COMMON                                                  01890000
.GO10    ANOP                                                           01900000
&EPILOG  SETA  &LV+72              SET SIZE FOR $EPILOG FREEMAIN        01910000
         LA    R0,&LV+72           SET SIZE FOR GETMAIN                 01920000
         GETMAIN &GMT,LV=(0)       GET STORAGE                          01930000
         AIF   (T'&ERRCODE EQ 'O').GO11 IF UNCONDITIONAL                01940000
         LTR   R15,R15             STORAGE GOTTEN?                      01950000
         BZ    $&X.C               YES, CONTINUE                        01960000
         LA    R15,&ERRCODE        SET SPECIFIED ERROR CODE             01970000
         ST    R15,16(R13)         INTO SAVE AREA                       01980000
         LM    R14,R12,12(R13)     RESTORE CALLERS REGS                 01990000
         BR    R14                 AND RETURN                           02000000
$&X.C    DS    0H                                                       02010000
.GO11    ANOP                                                           02020000
         LR    R2,R1               SAVE THE GOTTEN STORAGE ADDRESS      02030000
         LR    R14,R2               MVCL - TARGET ADDR                  02040000
         LA    R15,&LV+72           MVCL - TARGET SIZE                  02050000
         SR    R0,R0                MVCL - SOURCE ADDR (NONE)           02060000
         SR    R1,R1                MVCL - SOURCE SIZE (NONE)           02070000
         MVCL  R14,R0              ZERO GOTTEN STORAGE                  02080000
         AGO   .COMMON                                                  02090000
.GO12    ANOP                                                           02100000
$&X.B    GETMAIN &GMT,LV=WORKEND-SAVEAREA GET THE SAVE AREA STORAGE     02110000
         AIF   (T'&ERRCODE EQ 'O').GO13 IF UNCONDITIONAL                02120000
         LTR   R15,R15             STORAGE GOTTEN?                      02130000
         BZ    $&X.C               YES, CONTINUE                        02140000
         LA    R15,&ERRCODE        SET SPECIFIED ERROR CODE             02150000
         ST    R15,16(R13)         INTO SAVE AREA                       02160000
         LM    R14,R12,12(R13)     RESTORE CALLERS REGS                 02170000
         BR    R14                 AND RETURN                           02180000
$&X.C    DS    0H                                                       02190000
.GO13    ANOP                                                           02200000
         LR    R2,R1               SAVE THE GOTTEN STORAGE ADDRESS      02210000
         LR    R14,R2               MVCL - TARGET ADDR                  02220000
         LA    R15,WORKEND-SAVEAREA MVCL - TARGET SIZE                  02230000
         LA    R0,SAVEAREA          MVCL - SOURCE ADDR                  02240000
         LR    R1,R15               MVCL - SOURCE SIZE                  02250000
         MVCL  R14,R0              COPY TO WORKING STORAGE              02260000
         USING SAVEAREA,R13        PROVIDE ADDRESSABILITY               02270000
.COMMON  ANOP                                                           02280000
         LR    R14,R13             COPY OLD SAVE AREA ADDRESS           02290000
         LR    R13,R2              SET NEW SAVEAREA ADDRESS             02300000
         ST    R14,4(R13)          CHAIN SAVEAREA - BACKWARD            02310000
         ST    R13,8(R14)          CHAIN SAVEAREA - FORWARD             02320000
         LM    R15,R2,16(R14)      RESTORE ENTRY REGISTERS              02330000
         SR    R14,R14             RESET RETURN ADDRESS                 02340000
         AIF   (&REGS).SKIPREG                                          02350000
&LISTOPT SETC  'LIST=&LIST'                                             02360000
         $REGS &LISTOPT            DEFINE STANDARD REG EQUATES          02370000
.SKIPREG AIF   ('&LIST' EQ 'YES').MEXIT                                 02380000
         POP  PRINT                                                     02390000
.MEXIT   ANOP                                                           02400000
         MEND                                                           02410000
./ ADD NAME=$EPILOG                                                     02420000
         MACRO                                                          02430000
&LABEL   $EPILOG &RETCODE,&LIST=NO                                      02440000
.********************************************************************** 02450000
.*                                                                    * 02460000
.*                                  CHANEY SYSTEMS SUPPORT, INC.      * 02470000
.*                                                                    * 02480000
.*       THIS MACRO PROVIDES STANDARD DE-LINKAGE CONVENTIONS FOR      * 02490000
.*       MOST CSS TOOL KIT MEMBERS.                                   * 02500000
.*                                                                    * 02510000
.********************************************************************** 02520000
         GBLA  &EPILOG                                                  02530000
         GBLB  &LSAVE                                                   02540000
         AIF   ('&LIST' EQ 'YES').LIST1                                 02550000
         PUSH  PRINT                                                    02560000
         PRINT OFF                                                      02570000
.LIST1   ANOP                                                           02580000
&LABEL   DS    0H                                                       02590000
         AIF   (&LSAVE).GO3                                             02600000
         AIF   (&EPILOG EQ 0).GO1                                       02610000
         LA    R0,&EPILOG          GET SAVEAREA LENGTH                  02620000
         AGO   .GO2                                                     02630000
.GO1     LA    R0,WORKEND-SAVEAREA GET SAVEAREA LENGTH                  02640000
.GO2     LR    R1,R13              GET SAVEAREA ADDRESS FOR FREEMAIN    02650000
.GO3     ANOP                                                           02660000
         L     R13,4(R13)          GET BACK CHAIN POINTER               02670000
         ST    R15,16(R13)         SAVE REGISTER 15 (RETCODE)           02680000
         AIF   (&LSAVE).GO4                                             02690000
         FREEMAIN RU,LV=(0),A=(1)  FREE SAVEAREA                        02700000
.GO4     ANOP                                                           02710000
         LM    R14,R12,12(R13)     RESTORE CALLERS REGS                 02720000
         AIF   (T'&RETCODE EQ 'O').GO5                                  02730000
         LA    R15,&RETCODE        SET RETURN CODE                      02740000
.GO5     ANOP                                                           02750000
         BR    R14                 RETURN TO CALLER                     02760000
         AIF   ('&LIST' EQ 'YES').MEXIT                                 02770000
         POP   PRINT                                                    02780000
.MEXIT   ANOP                                                           02790000
         MEND                                                           02800000
./ ENDUP                                                                02810000
/*                                                                      02820000
//*                                                                     02830000
//GDGCOPY EXEC ASMFCL,PARM='LIST,NODECK,LOAD,NOTERM'                    02840000
//ASM.SYSLIB DD                                                         02850000
//           DD DSN=GDGCOPY.MACLIB,DISP=(OLD,DELETE)                    02860000
//ASM.SYSIN DD *                                                        02870000
*********************************************************************** 02880000
*                                                                     * 02890000
*                         CHANEY SYSTEMS SUPPORT, INC.                * 02900000
*                                                                     * 02910000
*                                                                     * 02920000
*        THIS PROGRAM COPIES INPUT GENERATION DATASETS                * 02930000
*        IN REVERSE ORDER (OLDEST GDG FIRST) TO AN OUTPUT             * 02940000
*        DATASET(SYSUT2) ALLOWING A USER TO MAINTAIN DATA             * 02950000
*        IN CHRONOLOGICAL SEQUENCE.  THIS IS REQUIRED IF USING        * 02960000
*        RMF OR OTHER POST PROCESSOR SYSTEMS THAT REQUIRE SMF         * 02970000
*        DATA TO BE IN SEQUENCE (IT AVOIDS LARGE SORTS).              * 02980000
*                                                                     * 02990000
*        THE INPUT DATASET NAME (HIGH LEVEL OF THE GDG) IS PASSED     * 03000000
*        AS PARM INFORMATION AND IS USED TO "DYNAMICALLY" ALLOCATE    * 03010000
*        EACH GENERATION LEVEL OF THE GROUP.  ONCE THIS ROUTINE       * 03020000
*        IS COMPLETE, THE USER MAY SCRATCH AND UNCATALOG THE          * 03030000
*        GENERATION DATA SETS IF HE WISHES (THIS ROUTINE "WILL NOT"   * 03040000
*        SCRATCH OR UNCATALOG ANY DATASET).                           * 03050000
*                                                                     * 03060000
*        SINCE THIS ROUTINE IS OF GENERAL PURPOSE, THE USER MUST      * 03070000
*        PROVIDE ALL DCB INFORMATION INFORMATION FOR BOTH SYSUT1      * 03080000
*        (IF NON-LABELED OR NSL) AND SYSUT2 BY CODING DCB ON THE      * 03090000
*        DD CARD.  IF THE INPUT GDG (PARM INFO) IS STANDARD LABEL,    * 03100000
*        IT NEED NOT BE DEFINED BY SYSUT1.  IN EITHER CASE, SYSUT1    * 03110000
*        WILL BE DYNAMICALLY DEALLOCATED AND UNCHANGED (SINCE PARM    * 03120000
*        INFO IS USED TO IDENTIFY THE INPUT FILE(GDG)).               * 03130000
*                                                                     * 03140000
*        NOTE THAT FOR VS/VBS FILES, "BFTEK=A" WILL AUTOMATICALLY     * 03150000
*        BE ADDED TO THE SYSUT1 DCB VIA AN OPEN EXIT.  THIS IS        * 03160000
*        DONE TO INSURE THAT A LOGICAL RECORD IS PASSED BY QSAM       * 03170000
*        AND ALLOWS THE OUTPUT TO BE REBLOCKED (IF SPECIFIED).        * 03180000
*                                                                     * 03190000
*                                                                     * 03200000
*  REQUIRED JCL:                                                      * 03210000
*                                                                     * 03220000
*        <----------- STANDARD LABELED INPUT FILE ----------------->  * 03230000
*         //GDGCOPY  EXEC PGM=GDGCOPY,PARM='STD.LABELED.GDG.PREFIX'   * 03240000
*         //SYSUT2   DD DSN=OUTPUT.DSN(+1)                            * 03250000
*                                                                     * 03260000
*        <-----------    NON-LABELED INPUT FILE   ----------------->  * 03270000
*         //GDGCOPY  EXEC PGM=GDGCOPY,PARM='NON.LABELED.GDG.PREFIX'   * 03280000
*         //SYSUT1   DD DCB=????                                      * 03290000
*         //SYSUT2   DD DSN=OUTPUT.DSN(+1)                            * 03300000
*                                                                     * 03310000
*  LINKAGE EDITOR ATTRIBUTES AND CONSIDERATIONS                       * 03320000
*  ENTRY POINT: GDGCOPY (NON-RENT)                                    * 03330000
*                                                                     * 03340000
*********************************************************************** 03350000
         PRINT NOGEN                                                    03360000
         EJECT                                                          03370000
*---------------------------------------------------------------------- 03380000
*                                                                       03390000
*        INITIALIZATION AND DCB/JFCB/DSCB MERGE                         03400000
*                                                                       03410000
*---------------------------------------------------------------------- 03420000
GDGCOPY  $PROLOG R12              LINKAGE CONVENTIONS                   03430000
ENTRY010 DS    0H                                                       03440000
         L     R1,0(R1)           GET PARM POINTER                      03450000
         LH    R2,0(R1)           GET PARM SIZE                         03460000
         CH    R2,=H'0'           ANY PARM?                             03470000
         BE    RETURN             NO, RETURN TO CALLER                  03480000
         CH    R2,=H'35'          VALID SIZE?                           03490000
         BH    RETURN             NO, RETURN TO CALLER                  03500000
         LA    R3,CAMDSN(R2)      GET OFFSET TO GDG SUFFIX              03510000
         MVC   0(3,R3),=C'(0)'    START WITH CURRENT GENERATION         03520000
         ST    R3,GDGADDR         SAVE ADDRESS OF GENERATION LEVEL      03530000
         BCTR  R2,0               DECREMENT FOR EXECUTE                 03540000
         EX    R2,PARMMOVE        MOVE DSNAME TO CAMLIST                03550000
         L     R1,16              GET CVT POINTER                       03560000
         L     R1,0(R1)           OLD/NEW TCB POINTER                   03570000
         L     R1,4(R1)           OUR TCB POINTER                       03580000
         L     R1,12(R1)          OUR TIOT POINTER                      03590000
         LA    R1,24(R1)          ADDRESS OF 1ST DD ENTRY               03600000
         SR    R15,R15            CLEAR FOR ICM                         03610000
AAA010   ICM   R15,1,0(R1)        GET SIZE OF ENTRY                     03620000
         BZ    ENTRY020           NOT FOUND, CONTINUE                   03630000
         CLC   4(8,R1),=CL8'SYSUT1' IS THIS SYSUT1 DD NAME?             03640000
         BE    BBB010             FOUND, OPEN DUMMY FILE                03650000
         AR    R1,R15             BUMP TO NEXT DD ENTRY                 03660000
         B     AAA010             CHECK NEXT ENTRY                      03670000
*        OPEN TO ALLOW NORMAL MERGE (RDJFCB ONLY GETS JCL INFO)         03680000
*        NOTE: SYSUT1 IS ONLY NEEDED IF INPUT IS NL                     03690000
BBB010   OI    FLAG,UT1           INDICATE SYSUT1 WAS FOUND             03700000
         OPEN  (SYSUT1,(INPUT))   OPEN DUMMY INPUT FILE TO FILL DCB     03710000
         LA    R6,SYSUT1          ADDRESSABILITY TO DCB                 03720000
         USING IHADCB,R6          INFORM ASSEMBLER                      03730000
         TM    DCBOFLGS,DCBOFOPN  WAS THE SYSUT1 CARD PROVIDED          03740000
         BZ    ENTRY020           NO, SKIP THE MERGE                    03750000
         LH    R3,DCBBLKSI        GET BLKSIZE                           03760000
         LH    R4,DCBLRECL        GET LRECL                             03770000
         IC    R5,DCBRECFM        GET RECFM                             03780000
         CLOSE (SYSUT1)           CLOSE THE DUMMY INPUT FILE            03790000
         STH   R3,DCBBLKSI        SET BLKSIZE                           03800000
         STH   R4,DCBLRECL        SET LRECL                             03810000
         STC   R5,DCBRECFM        SET RECFM                             03820000
         B     ENTRY020           BRANCH AROUND                         03830000
PARMMOVE MVC   CAMDSN(0),2(R1)    MOVE DSNAME TO LIST                   03840000
         EJECT                                                          03850000
*---------------------------------------------------------------------- 03860000
*                                                                       03870000
*        FIND THE OLDEST GENERATION DATASET NAME VIA THE CATALOG        03880000
*                                                                       03890000
*---------------------------------------------------------------------- 03900000
ENTRY020 DS    0H                                                       03910000
         LOCATE CAMLIST           IS THIS THERE A '0' GENERATION        03920000
         LTR   R15,R15            ZERO RETURN CODE ?                    03930000
         BNZ   RETURN             NO - END OF THE LINE                  03940000
AAA020   AP    GDG#,=P'1'         ADD 1 TO RELATIVE GENERATION NUMBER   03950000
         BAL   R14,ENTRY900       CONVERT GDG#                          03960000
         LOCATE CAMLIST           LOOK FOR OLDEST GENERATION            03970000
         LTR   R15,R15            ZERO RETURN CODE ?                    03980000
         BZ    AAA020             YES, CHECK FOR NEXT                   03990000
         SP    GDG#,=P'1'         BUMP BACK TO PREVIOUS GDG#            04000000
         BAL   R14,ENTRY900       CONVERT GDG#                          04010000
         LOCATE CAMLIST           CONVERT GDG NUMBER                    04020000
         LTR   R15,R15            ZERO RETURN CODE ?                    04030000
         BZ    BBB020             YES, CONTINUE                         04040000
         ABEND 0,DUMP             SHOULD NOT OCCUR                      04050000
BBB020   TM    FLAG,UT1           SHOULD WE DE-ALLOCATE SYSUT1          04060000
         BZ    CCC020             NO, CONTINUE                          04070000
         BAL   R14,ENTRY920       DEALLOCATE THE DUMMY FILE             04080000
CCC020   OPEN  (SYSUT2,(OUTPUT))  OPEN OUTPUT FILE                      04090000
EXIT020  EQU   *                                                        04100000
         EJECT                                                          04110000
*---------------------------------------------------------------------- 04120000
*                                                                       04130000
*        COPY THE GDG TO THE OUTPUT FILE                                04140000
*                                                                       04150000
*---------------------------------------------------------------------- 04160000
ENTRY030 DS    0H                                                       04170000
         BAL   R14,ENTRY910       ALLOC THE INPUT GDG                   04180000
         MVC   STATUS+26(44),CAMDSN MODIFY WRITE TO PROGRAMMER          04190000
STATUS   WTO   'GDGCOPY  - ACTIVE=                                     X04200000
                      ',ROUTCDE=11 INFORM USER GENERATIONS COPIED       04210000
         OPEN  SYSUT1             OPEN THE INPUT GDG                    04220000
AAA030   EQU   *                                                        04230000
         GET   SYSUT1             GET A RECORD                          04240000
         ST    R1,SAVE14          SAVE FOR DEBUGGING 002 ABENDS         04250000
         LR    R0,R1              POINT TO RECORD FOR PUT               04260000
         PUT   SYSUT2,(0)         WRITE RECORD                          04270000
         B     AAA030             GET NEXT RECORD                       04280000
BBB030   EQU   *                                                        04290000
         CLOSE SYSUT1             CLOSE THE INPUT DATA SET              04300000
         BAL   R14,ENTRY920       DE-ALLOCATE THE GDG                   04310000
         B     ENTRY040           SKIP OVER OPEN EXIT CODE              04320000
         SPACE 3                                                        04330000
CCC030   LA    R6,SYSUT1          GET ADDRESS OF INPUT DCB              04340000
         TM    DCBRECFM,DCBRECF   CHECK FOR FIXED LENGTH                04350000
         BOR   R14                YES, IGNORE IT                        04360000
         TM    DCBRECFM,DCBRECV   CHECK FOR VARIABLE LENGTH             04370000
         BZR   R14                NO, IGNORE IT                         04380000
         TM    DCBRECFM,DCBRECSB  CHECK FOR SPANNED RECORDS             04390000
         BZR   R14                NO, IGNORE IT                         04400000
         OI    DCBBFALN,DCBBFTA   OVER-RIDE TO BFTEK=A                  04410000
         BR    R14                RETURN TO OPEN                        04420000
*---------------------------------------------------------------------- 04430000
*                                                                       04440000
*        UPDATE TO THE NEXT GENERATION IF ANY                           04450000
*                                                                       04460000
*---------------------------------------------------------------------- 04470000
ENTRY040 DS    0H                                                       04480000
         CP    GDG#,=P'0'         DID WE JUST PROCESS GDG(0)?           04490000
         BE    EOJ                YES, ALL DONE                         04500000
         SP    GDG#,=P'1'         NO, DECREMENT TO NEXT GENERATION      04510000
         BAL   R14,ENTRY900       CONVERT GDG#                          04520000
         LOCATE CAMLIST           CONVERT NAME FOR DYNALLOC             04530000
         LTR   R15,R15            ZERO RETURN CODE ?                    04540000
         BZ    ENTRY030           YES, CONTINUE                         04550000
         ABEND 0,DUMP             SHOULD NOT OCCUR                      04560000
         SPACE 3                                                        04570000
*---------------------------------------------------------------------- 04580000
*                                                                       04590000
*        CONVERT THE GDG NUMBER FOR ALLOCATE/LOCATE                     04600000
*                                                                       04610000
*---------------------------------------------------------------------- 04620000
ENTRY900 DS    0H                                                       04630000
         L     R15,GDGADDR        GET MOVE TO ADDRESS                   04640000
         MVC   0(9,R15),=CL9' '   CLEAR POSSIBLE REDUNDANT GDG#         04650000
         UNPK  1(3,R15),GDG#      UNPACK TO CAMLIST                     04660000
         OI    3(R15),X'F0'       CHANGE SIGN                           04670000
         MVI   1(R15),C'-'        NEGATIVE GDG NUMBER                   04680000
         MVI   0(R15),C'('        MOVE LEFT PAREN                       04690000
         MVC   4(2,R15),=CL25') ' MOVE RIGHT PAREN AND BLANK            04700000
         BR    R14                RETURN                                04710000
         EJECT                                                          04720000
*---------------------------------------------------------------------- 04730000
*                                                                       04740000
*        ALLOCATE THE GENERATION DATA SET                               04750000
*                                                                       04760000
*---------------------------------------------------------------------- 04770000
ENTRY910 DS    0H                                                       04780000
         ST    R14,SAVE14         SAVE CALLED FROM ADDRESS              04790000
         LA    R11,DAWORK         ADDRESS OF REQUEST BLOCK              04800000
         USING S99RBP,R11         REQ BLK POINTER DSECT                 04810000
         LA    R10,S99RBP+4       ADDRESSABILITY OF RB DSECT            04820000
         USING S99RB,R10          RB DSECT                              04830000
         ST    R10,S99RBPTR       MAKE RBPTR POINT TO RB                04840000
         OI    S99RBPTR,S99RBPND  TURN ON HOB IN RBPTR                  04850000
         XC    S99RB(RBLEN),S99RB  CLEAR RB                             04860000
         MVI   S99RBLN,RBLEN      PUT LEN IN ITS LENGTH FIELD           04870000
         MVI   S99VERB,S99VRBAL   SET VERB CODE TO ALLOCATE             04880000
* SET BITS FOR "WAIT FOR DEVICES" & "WAIT FOR VOLUMES"                  04890000
         LA    R9,S99RB+RBLEN     POINT BEYOND RB (1ST TEXT PTR)        04900000
         USING S99TUPL,R9         ADDRESSABILITY OF TEXT UNIT POINTERS  04910000
         ST    R9,S99TXTPP        INIT TEXT PTR IN RB                   04920000
         LA    R8,S99TUPL+16      POINT BEYOND LAST TXT PTR             04930000
*                                 (ROOM FOR 4 TEXT UNITS)               04940000
         USING S99TUNIT,R8        ADDRESSABILITY OF TEXT UNITS          04950000
* DDNAME                                                                04960000
         ST    R8,S99TUPTR        1ST PTR TO 1ST UNIT                   04970000
         LA    R1,DALDDNAM        KEY FOR DDNAME                        04980000
         STH   R1,S99TUKEY        PUT IN TEXT UNIT KEY FIELD            04990000
         MVC   S99TUNUM,=H'1'     MOVE 1 TO TXT UNIT NUMBR FLD          05000000
         MVC   S99TULNG,=H'6'     MOVE 6 TO TXT UNIT LNGTH FLD          05010000
         MVC   S99TUPAR(6),=C'SYSUT1'  DDNAME                           05020000
         LA    R8,12(R8)          BUMP TO NEXT TXT UNIT                 05030000
         LA    R9,4(R9)           BUMP TO NEXT TXT UNIT PTR             05040000
* DISP                                                                  05050000
         ST    R8,S99TUPTR        2ND PTR TO 2ND UNIT                   05060000
         LA    R1,DALSTATS        KEY FOR DSN STATUS                    05070000
         STH   R1,S99TUKEY        PUT IN TEXT UNIT KEY FIELD            05080000
         MVC   S99TUNUM,=H'1'     MOVE 1 TO TXT UNIT NUMBR FLD          05090000
         MVC   S99TULNG,=H'1'     MOVE 1 TO TXT UNIT LNGTH FLD          05100000
         MVI   S99TUPAR,X'08'     INDICATE SHR DATA SET                 05110000
         LA    R8,9(R8)           BUMP TO NEXT TXT UNIT                 05120000
         LA    R9,4(R9)           BUMP TO NEXT TXT UNIT PTR             05130000
* DATA SET NAME                                                         05140000
         ST    R8,S99TUPTR        3RD PTR TO 3RD UNIT                   05150000
         OI    S99TUPTR,S99TUPLN  TURN ON HOB TO INIDICATE LAST PTR     05160000
         LA    R1,DALDSNAM        KEY FOR DSNAME                        05170000
         STH   R1,S99TUKEY        PUT IN TEXT UNIT KEY FIELD            05180000
         MVC   S99TUNUM,=H'1'     MOVE 1 TO TXT UNIT NUMBR FLD          05190000
         MVC   S99TULNG,=H'44'    MOVE 44 TO TXT UNIT LNGTH FLD         05200000
         MVC   S99TUPAR(44),CAMDSN  DSNAME                              05210000
RETRY    DS    0H                                                       05220000
         LR    R1,R11             ADDR OF REQUEST BLOCK                 05230000
         DYNALLOC                                                       05240000
         ST    R15,RC             SAVE RETURN CODE                      05250000
         LTR   R15,R15            RETURN CODE ZERO ?                    05260000
         BNZ   ERROR1             NO - DECODE ERROR                     05270000
         L     R14,SAVE14         GET CALLED FROM ADDRESS               05280000
         BR    R14                                                      05290000
         EJECT                                                          05300000
*---------------------------------------------------------------------- 05310000
*                                                                       05320000
*     DE-ALLOCATE THE GENERATION DATA SET                               05330000
*                                                                       05340000
*---------------------------------------------------------------------- 05350000
ENTRY920 DS    0H                                                       05360000
         ST    R14,SAVE14         SAVE CALLED FROM ADDRESS              05370000
         LA    R11,DAWORK         ADDRESS OF REQUEST BLOCK              05380000
         USING S99RBP,R11         REQ BLK POINTER DSECT                 05390000
         LA    R10,S99RBP+4       ADDRESSABILITY OF RB DSECT            05400000
         USING S99RB,R10          RB DSECT                              05410000
         ST    R10,S99RBPTR       MAKE RBPTR POINT TO RB                05420000
         OI    S99RBPTR,S99RBPND  TURN ON HOB IN RBPTR                  05430000
         XC    S99RB(RBLEN),S99RB  CLEAR RB                             05440000
         MVI   S99RBLN,RBLEN      PUT LEN IN ITS LENGTH FIELD           05450000
         MVI   S99VERB,S99VRBUN   SET VERB CODE TO UN-ALLOCATE          05460000
         LA    R9,S99RB+RBLEN     POINT BEYOND RB (1ST TEXT PTR)        05470000
         USING S99TUPL,R9         ADDRESSABILITY OF TEXT UNIT POINTERS  05480000
         ST    R9,S99TXTPP        INIT TEXT PTR IN RB                   05490000
         LA    R8,S99TUPL+16      POINT BEYOND LAST TXT PTR             05500000
*                                 (ROOM FOR 4 TEXT UNITS)               05510000
         USING S99TUNIT,R8        ADDRESSABILITY OF TEXT UNITS          05520000
* DDNAME                                                                05530000
         ST    R8,S99TUPTR        1ST PTR TO 1ST UNIT                   05540000
         LA    R1,DALDDNAM        KEY FOR DDNAME                        05550000
         STH   R1,S99TUKEY        PUT IN TEXT UNIT KEY FIELD            05560000
         MVC   S99TUNUM,=H'1'     MOVE 1 TO TXT UNIT NUMBR FLD          05570000
         MVC   S99TULNG,=H'6'     MOVE 6 TO TXT UNIT LNGTH FLD          05580000
         MVC   S99TUPAR(6),=C'SYSUT1'  DDNAME                           05590000
         LA    R8,12(R8)          BUMP TO NEXT TXT UNIT                 05600000
         LA    R9,4(R9)           BUMP TO NEXT TXT UNIT PTR             05610000
* IN-USE ATTRIBUTE                                                      05620000
         ST    R8,S99TUPTR        2ND PTR TO 2ND UNIT                   05630000
         OI    S99TUPTR,S99TUPLN  TURN ON HOB TO INIDICATE LAST PTR     05640000
         LA    R1,DUNUNALC        KEY FOR UNALLOC IF IN-USE             05650000
         STH   R1,S99TUKEY        PUT IN TEXT UNIT KEY FIELD            05660000
         MVC   S99TUNUM,=H'0'     MOVE 0 TO TXT UNIT NUMBR FLD          05670000
         LR    R1,R11             ADDR OF REQUEST BLOCK                 05680000
         DYNALLOC                                                       05690000
         ST    R15,RC             SAVE RETURN CODE                      05700000
         LTR   R15,R15            RETURN CODE ZERO ?                    05710000
         BNZ   ERROR1             NO - DECODE ERROR                     05720000
         L     R14,SAVE14         GET CALLED FROM ADDRESS               05730000
         BR    R14                                                      05740000
         EJECT                                                          05750000
*---------------------------------------------------------------------- 05760000
*                                                                       05770000
*        END OF JOB                                                     05780000
*                                                                       05790000
*---------------------------------------------------------------------- 05800000
EOJ      XC    RC,RC              CLEAR RETCODE                         05810000
         CLOSE (SYSUT2)           CLOSE OUTPUT FILE                     05820000
         WTO   'GDGCOPY  - NORMAL COMPLETION',ROUTCDE=11                05830000
RETURN   L     R15,RC             PICK UP RETURN CODE                   05840000
         $EPILOG                                                        05850000
         SPACE 3                                                        05860000
*---------------------------------------------------------------------- 05870000
*                                                                       05880000
*        ERROR ROUTINES                                                 05890000
*                                                                       05900000
*---------------------------------------------------------------------- 05910000
ERROR1   DS    0H                                                       05920000
         L     R1,RC              PICK UP RETURN CODE                   05930000
         CVD   R1,DBWRD                                                 05940000
         UNPK  MSG1+9(3),DBWRD+6(2)        RETURN CODE                  05950000
         OI    MSG1+11,X'F0'                                            05960000
         UNPK  MSG1+15(5),S99ERROR(3)      DYNAMIC ERROR CODE           05970000
         MVI   MSG1+19,X'40'                                            05980000
         UNPK  MSG1+29(5),S99INFO(3)      DYNAMIC INFO CODE             05990000
         MVI   MSG1+33,X'40'                                            06000000
MSG1     WTO   ' XXX - XXXXX ERROR - XXXXX INFO (DYNAMIC ALLOC ERROR)',X06010000
               ROUTCDE=11                                               06020000
         CLC   MSG1+9(10),=CL10'004 - 0214' NO DEVICES AVAILABLE?       06030000
         BE    SLEEP                    YES, WAIT A MINUTE              06040000
         CLC   MSG1+9(10),=CL10'004 - 0484' NO DEVICES AVAILABLE?       06050000
         BNE   ABEND1                   NO, ABEND                       06060000
SLEEP    DS    0H                                                       06070000
         STIMER WAIT,DINTVL=WAITTIME    WAIT A WHILE                    06080000
         XC    RC,RC                    RESET D/A RETURN CODE           06090000
         XC    S99ERROR,S99ERROR        RESET D/A ERROR CODE            06100000
         XC    S99INFO,S99INFO          RESET D/A INFO CODE             06110000
         B     RETRY                    AND RETRY THE REQUEST           06120000
ABEND1   DS    0H                                                       06130000
         ABEND 1,DUMP                                                   06140000
DBWRD    DC    D'0'                                                     06150000
         EJECT                                                          06160000
*---------------------------------------------------------------------- 06170000
*                                                                       06180000
*        CAMLIST FOR LOCATING EACH GDG                                  06190000
*                                                                       06200000
*---------------------------------------------------------------------- 06210000
CAMLIST  CAMLST NAME,CAMDSN,,CAMWORK                                    06220000
CAMDSN   DC    CL44' '                                                  06230000
CAMWORK  DS    0D                                                       06240000
         DC    265C' '                                                  06250000
*---------------------------------------------------------------------- 06260000
*                                                                       06270000
*        STORAGE AREAS                                                  06280000
*                                                                       06290000
*---------------------------------------------------------------------- 06300000
GDGADDR  DC    F'0'               ADDRESS OF '(NNN)' FIELD              06310000
RC       DC    F'16'              RETURN CODE (PRIMED FOR ERROR)        06320000
SAVE14   DC    F'0'               REG 14 SAVE AREA                      06330000
EXLST    DC    X'85',AL3(CCC030)  OPEN LIST FOR SYSUT1                  06340000
GDG#     DC    PL2'0'             CURRENT GDG NUMBER                    06350000
FLAG     DC    X'00'              OPTIONS SWITCHES                      06360000
UT1      EQU   X'01'              SYSUT1 WAS FOUND IN TIOT              06370000
WAITTIME DC    C'00010000'        WAIT TIME FOR DYNALLOC                06380000
         LTORG                                                          06390000
SYSUT1   DCB   DDNAME=SYSUT1,DSORG=PS,MACRF=GL,EODAD=BBB030,           X06400000
               EXLST=EXLST                                              06410000
SYSUT2   DCB   DDNAME=SYSUT2,DSORG=PS,MACRF=PM                          06420000
DAWORK   DC    XL256'00'                                                06430000
         DCBD  DSORG=QS                                                 06440000
         IEFZB4D0                                                       06450000
         IEFZB4D2                                                       06460000
RBLEN    EQU   (S99RBEND-S99RB)                                         06470000
         END                                                            06480000
/*                                                                      06490000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR                             06500000
//LKED.SYSIN DD *                                                       06510000
  NAME GDGCOPY(R)                                                       06520000
/*                                                                      06530000
//                                                                      06540000
