//TAPEMAP  JOB (TSO),
//             'Install TAPEMAP',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00020000
// EXEC ASMFCL,MAC1='SYS1.AMODGEN',                                     00030000
//             PARM.ASM='LIST,XREF,OBJECT,NODECK',                      00040000
//             PARM.LKED='NCAL,MAP,LIST,LET'                            00050000
//ASM.SYSIN DD *                                                        00060000
XYZZY    OPSYN PRINT               CUSTOMIZED PRINT CONTROL   06/84 DBC 00010400
PRINT    OPSYN ANOP                SUPPRESS STD PRINT CONTROL 06/84 DBC 00020000
         MACRO                                                          02000000
&NFS     FL2   &DSECT=YES                                               02010000
         LCLC  &P                                                       02020000
&P       SETC  'FL2'                                                    02030000
         AIF   ('&NFS' EQ '').NO$NFS                                    02040000
&P       SETC  '&NFS'                                                   02050000
.NO$NFS  AIF   ('&DSECT' NE 'YES').NODSECT                              02060000
&P.D     DSECT                                                          02070000
.NODSECT ANOP                                                           02080000
.***************                                                        02090000
&P.LABI  DC    C'HDR'                   OR 'EOF' OR 'EOV'               02100000
&P.NO    DC    C'2'                     CONSTANT                        02110000
&P.RECFM DC    C' '                     F/V/U/D                         02120000
&P.BLKL  DC    CL5' '                   BLKSIZE                         02130000
&P.LRECL DC    CL5' '                   RECORD LEN (X=99999)            02140000
&P.DEN   DC    C' '                     DENSITY (C'0' TO C'4')      THO 02150000
&P.FILP  DC    C' '                     1=VOL SW OCCURRED, 0=NOT        02160000
&P.JOBD  DC    CL8' '                   JOBNAME OF CREATOR              02170000
&P.JSSP  DC    C'/'                     SLASH                           02180000
&P.STEPD DC    CL8' '                   STEPNAME OF CREATOR             02190000
&P.TRTCH DC    CL2' '                                                   02200000
&P.CNTRL DC    C' '                     A/M/<BLANK>                     02210000
         DC    C' '                                                     02220000
&P.BLKA  DC    C' '                     B/S/R/<BLANK>                   02230000
         DC    CL41' '                                                  02240000
         SPACE 2                                                        02250000
         MEND                                                           02260000
         EJECT ,                                              09/84 DBC 02270000
         MACRO                                                          02280000
&NFS     FL1   &DSECT=YES                                               02290000
         LCLC  &P                                                       02300000
&P       SETC  'FL1'                                                    02310000
         AIF   ('&NFS' EQ '').NO$NFS                                    02320000
&P       SETC  '&NFS'                                                   02330000
.NO$NFS  AIF   ('&DSECT' NE 'YES').NODSECT                              02340000
&P.D     DSECT                                                          02350000
.NODSECT ANOP                                                           02360000
&P.LABI  DC    C'HDR'                   OR 'EOF' OR 'EOV'               02370000
&P.NO    DC    C'1'                     CONSTANT                        02380000
&P.ID    DC    CL17' '                  DATASET NAME                    02390000
&P.FILSR DC    CL6' '                   FIRST VOL NAME                  02400000
&P.VOLSQ DC    CL4' '                   VOL # OF DS                     02410000
&P.FILSQ DC    CL4' '                   DS SEQ #                        02420000
&P.GNO   DC    CL4' '                   GENERATION NUMBER               02430000
&P.VNG   DC    CL2' '                   VERSION NUMBER                  02440000
&P.CREDT DC    CL6' YYDDD'              CREATION DATE                   02450000
&P.EXPDT DC    CL6' YYDDD'              EXPIRATION DATE                 02460000
&P.FSEC  DC    C' '                     0=NO PW, 1=PW, 3=NOPWREAD       02470000
&P.BLKCT DC    CL6' '                   # OF BLOCKS                     02480000
&P.SYSCD DC    CL13' '                                                  02490000
         DC    CL7' '                                                   02500000
         SPACE 2                                                        02510000
         MEND                                                           02520000
         EJECT ,                                              09/84 DBC 02530000
         MACRO                                                          02540000
&NFS     VOL   &DSECT=YES                                               02550000
         LCLC  &P                                                       02560000
&P       SETC  'VOL'                                                    02570000
         AIF   ('&NFS' EQ '').NO$NFS                                    02580000
&P       SETC  '&NFS'                                                   02590000
.NO$NFS  AIF   ('&DSECT' NE 'YES').NODSECT                              02600000
&P.D     DSECT                                                          02610000
.NODSECT ANOP                                                           02620000
&P.LABI  DC    C'VOL'                   CONSTANT                        02630000
&P.NO    DC    C'1'                     CONSTANT                        02640000
&P.SERNO DC    CL6'VOLSER'              VOLUME NAME                     02650000
         DC    C'0'                                                     02660000
         DC    CL30' '                                                  02670000
&P.OWNER DC    CL10' '                  OWNER ID                        02680000
         DC    CL29' '                                                  02690000
         SPACE 2                                                        02700000
         MEND                                                           02710000
         EJECT ,                                              09/84 DBC 02720000
         MACRO                                                          02730000
&NFS     HEXTAB &DUAL=YES                                               02740000
.*  HEXTAB  LDW  UPDATED 02-21-79                                       02750000
.*  UPDATED 03-17-79                                                    02760000
         GBLC  &HEXTAB#                                                 02770000
         LCLC  &N                                                       02780000
&N       SETC  '&NFS'                   ASSUME GIVEN                    02790000
         AIF   ('&N' NE '').OK          YEP - USE IT                    02800000
&N       SETC  '&HEXTAB#'               ASSUME GLOBAL PRESENT           02810000
         AIF   ('&N' NE '').OK          YEP - USE IT                    02820000
&HEXTAB# SETC  'HEXTAB'                 ELSE, SET IT                    02830000
&N       SETC  'HEXTAB'                 HERE TOO                        02840000
.OK      AIF   ('&DUAL' EQ 'YES').DUAL                                  02850000
&N       EQU   *-C'0'                                                   02860000
         DC    C'0123456789ABCDEF'                                      02870000
         MEXIT                                                          02880000
.DUAL    ANOP                                                           02890000
&N       EQU   *-C'A'                                                   02900000
         DC    X'0A0B0C0D0E0F'                                          02910000
         ORG   &N+C'0'                                                  02920000
         DC    C'0123456789ABCDEF'                                      02930000
         ORG                                                            02940000
         MEND                                                           02950000
         EJECT ,                                              09/84 DBC 02960000
         MACRO                                                          02970000
&NFS     HEX   &TO,&FROM,&L,&LEN=,&HEXTAB=,&BYTE=C' ',&DIGITS=          02980000
.*  HEX -- 12/18/76 -- LDW      FIXED 5-2-77    12-25-77                02990000
.*      REWRITTEN FROM SCRATCH -- 01/29/79  FIXED  02-16-79  03-03-79   03000000
.*      UPDATED  04-21-79                                               03010000
         GBLC  &HEXTAB#                                                 03020000
         LCLA  &LT                      UNPK "TO" LENGTH                03030000
         LCLA  &LF                      UNPK "FROM" LENGTH              03040000
         LCLA  &LTR                     TR "TO" LENGTH                  03050000
         LCLA  &LL                      &LEN ONE WAY OR ANOTHER         03060000
         LCLC  &F1,&F2,&T1,&T2,&LX                                      03070000
&F1      SETC  '&FROM(1)'               FOR SHORTER STRING LATER        03080000
&F2      SETC  '&FROM(2)'               FOR SHORTER STRING LATER        03090000
&T1      SETC  '&TO(1)'                 FOR SHORTER STRING LATER        03100000
&T2      SETC  '&TO(2)'                 FOR SHORTER STRING LATER        03110000
&LX      SETC  '&L&LEN'                 GET LENGTH USING EITHER METHOD  03120000
         AIF   ('&LX' EQ '').DEFLEN     LENGTH WILL DEFAULT TO 4        03130000
&LL      SETA  &LX                      GET IT                          03140000
         AGO   .OKLEN                                                   03150000
.DEFLEN  ANOP                                                           03160000
&LL      SETA  4                        SET THE DEFAULT LENGTH          03170000
.OKLEN   AIF   ('&HEXTAB' EQ '').OKHEX1                                 03180000
&HEXTAB# SETC  '&HEXTAB'                                                03190000
.OKHEX1  AIF   ('&HEXTAB#' NE '').OKHEX2                                03200000
&HEXTAB# SETC  'HEXTAB'                                                 03210000
.OKHEX2  ANOP                                                           03220000
&LT      SETA  &LL*2                                                    03230000
         AIF   ('&DIGITS' EQ '').OKDIGIT                                03240000
&LT      SETA  &DIGITS                                                  03250000
.OKDIGIT AIF   (N'&TO NE 2).TO1                                         03260000
         AIF   (N'&FROM NE 2).T2F1                                      03270000
.*  N'&TO = 2     N'&FROM = 2                                           03280000
&NFS     UNPK  &T1.(&LT+1,&T2),&F1.(&LL+1,&F2)                          03290000
         TR    &T1.(&LT,&T2),&HEXTAB#                                   03300000
         MVI   &T1+&LT.(&T2),&BYTE                                      03310000
         AGO   .DONE                                                    03320000
.T2F1    AIF   (N'&FROM NE 1).ERRF                                      03330000
.*  N'&TO = 2     N'&FROM = 1                                           03340000
&NFS     UNPK  &T1.(&LT+1,&T2),&FROM.(&LL+1)                            03350000
         TR    &T1.(&LT,&T2),&HEXTAB#                                   03360000
         MVI   &T1+&LT.(&T2),&BYTE                                      03370000
         AGO   .DONE                                                    03380000
.TO1     AIF   (N'&TO NE 1).ERRT                                        03390000
         AIF   (N'&FROM NE 2).T1F1                                      03400000
.*  N'&TO = 1     N'&FROM = 2                                           03410000
&NFS     UNPK  &TO.(&LT+1),&F1.(&LL+1,&F2)                              03420000
         TR    &TO.(&LT),&HEXTAB#                                       03430000
         MVI   &TO+&LT,&BYTE                                            03440000
         AGO   .DONE                                                    03450000
.T1F1    AIF   (N'&FROM NE 1).ERRF                                      03460000
.*  N'&TO = 1     N'&FROM = 1                                           03470000
&NFS     UNPK  &TO.(&LT+1),&FROM.(&LL+1)                                03480000
         TR    &TO.(&LT),&HEXTAB#                                       03490000
         MVI   &TO+&LT,&BYTE                                            03500000
         AGO   .DONE                                                    03510000
.ERRF    MNOTE 8,'ERROR IN "FROM" PARAMETER, MACRO TERMINATED'          03520000
         MEXIT                                                          03530000
.ERRT    MNOTE 8,'ERROR IN "TO" PARAMETER, MACRO TERMINATED'            03540000
         MEXIT                                                          03550000
.DONE    SPACE 1                                                        03560000
         MEND                                                           03570000
         EJECT ,                                              09/84 DBC 03580000
         MACRO                                                          03590000
&NFS     MONTHS  &DUMMY                 04-19-77 LDW                    03600000
&NFS     DC    H'31',C'JAN '            THIRTY DAYS                     03610000
         DC    H'28',C'FEB '            HATH SEPTEMBER,                 03620000
         DC    H'31',C'MAR '            APRIL, JUNE,                    03630000
         DC    H'30',C'APR '            AND NOVEMBER.                   03640000
         DC    H'31',C'MAY '            ALL THE REST                    03650000
         DC    H'30',C'JUN '            HAVE THIRTY ONE,                03660000
         DC    H'31',C'JUL '            EXCEPT FEBRUARY,                03670000
         DC    H'31',C'AUG '            WHICH HAS ONLY 28.              03680000
         DC    H'30',C'SEP '                                            03690000
         DC    H'31',C'OCT '                                            03700000
         DC    H'30',C'NOV '                                            03710000
         DC    H'31',C'DEC '                                            03720000
         SPACE 2                                                        03730000
         MEND                                                           03740000
         EJECT ,                                              09/84 DBC 03750000
         MACRO                                                          03760000
&NFS     DAYS  &TYPE,&LEN=9                                04-19-77 LDW 03770000
         AIF   ('&TYPE' EQ 'RIGHT').RIGHT                               03780000
&NFS     DC    CL&LEN'SUNDAY   '                                        03790000
         DC    CL&LEN'MONDAY   '                                        03800000
         DC    CL&LEN'TUESDAY  '                                        03810000
         DC    CL&LEN'WEDNESDAY'                                        03820000
         DC    CL&LEN'THURSDAY '                                        03830000
         DC    CL&LEN'FRIDAY   '                                        03840000
         DC    CL&LEN'SATURDAY '                                        03850000
         AGO   .EXIT                                                    03860000
.RIGHT   ANOP                                                           03870000
&NFS     DC    CL&LEN'   SUNDAY'                                        03880000
         DC    CL&LEN'   MONDAY'                                        03890000
         DC    CL&LEN'  TUESDAY'                                        03900000
         DC    CL&LEN'WEDNESDAY'                                        03910000
         DC    CL&LEN' THURSDAY'                                        03920000
         DC    CL&LEN'   FRIDAY'                                        03930000
         DC    CL&LEN' SATURDAY'                                        03940000
.EXIT    SPACE 2                                                        03950000
         MEND                                                           03960000
         EJECT ,                                              09/84 DBC 05090000
         MACRO                                                          05100000
&NFS     DCBEXIT  &BLKSIZE=,&BUFNO=                                     05110000
.*  DCBEXIT   04-20-78  LDW                                             05120000
         LCLC  &NAME                                                    05130000
&NAME    SETC  '&NFS'                                                   05140000
         PUSH  USING                                                    05150000
         DROP                                                           05160000
         USING *,R15                                                    05170000
         AIF   ('&BLKSIZE' EQ '').B1                                    05180000
&NAME    CLC   62(2,R1),=F'0'           BLKSIZE GIVEN?                  05190000
&NAME    SETC  ''                                                       05200000
         AIF   ('&BUFNO' EQ '').A1                                      05210000
         BNE   *+10                     YES - USE IT                    05220000
         AGO   .A2                                                      05230000
.A1      BNER  R14                      YES - USE IT                    05240000
.A2      MVC   62(2,R1),=Y(&BLKSIZE)    SET DEFAULT                     05250000
.B1      AIF   ('&BUFNO' EQ '').C1                                      05260000
&NAME    CLI   20(R1),0                 BUFNO GIVEN?                    05270000
         BNER  R14                      YES - USE IT                    05280000
         MVI   20(R1),&BUFNO            SET DEFAULT                     05290000
.C1      BR    R14                      RETURN TO OPEN                  05300000
         SPACE 2                                                        05310000
         POP   USING                                                    05320000
         SPACE 3                                                        05330000
         MEND                                                           05340000
         EJECT ,                                              09/84 DBC 05350000
         MACRO                                                          18120000
&NFS     PRTLN &A,&I                                                    18130000
         GBLC  &XA                                            02/93 RT  00021005
         LCLC  &LQ                                                      18140000
         LCLA  &L                                                       18150000
&LQ      SETC  'L'''                    TO FOOL THE ASSEMBLER           18160000
         AIF   ('&A'(1,1) NE '''').NOTQ NOT QUOTED STRING               18170000
&L       SETA  K'&A-2                   SET LENGTH OF IT                18180000
&NFS     MVC   OUTBUFF(&L),=C&A                                         18190000
         AGO   .BAL                                                     18200000
.NOTQ    AIF   ('&A' EQ 'OUTBUFF').OUTBUFF                              18210000
         AIF   ('&I' EQ 'I').I                                          18220000
&NFS     MVC   OUTBUFF(&LQ&A),&A                                        18230000
.BAL     BA&XA R14,PRTLN                                                18240000
         AGO   .MEND                                                    18250000
.I       ANOP                                                           18260000
&NFS     L     R15,=A(&A)                                               18270000
         MVC   OUTBUFF(&LQ&A),0(R15)                                    18280000
         AGO   .BAL                                                     18290000
.OUTBUFF ANOP                                                           18300000
&NFS     BA&XA R14,PRTLN                                                18310000
.MEND    SPACE 1                                                        18320000
         MEND                                                           18330000
         EJECT ,                                              09/84 DBC 18340000
         MACRO                                                          18350000
&NFS     PRTLN2  &DUMMY                                                 18360000
         GBLC  &XA                                            02/93 RT  00021005
&NFS     BA&XA R14,PRTLN2                                               18370000
         MEND                                                           18380000
         EJECT ,                                              09/84 DBC 18390000
         MACRO                                                          18400000
&NFS     TAPIO &CCW,&DUMMY,&TM=UNEXTPMK                                 18410000
         GBLC  &XA                                            02/93 RT  00021005
         AIF   ('&DUMMY' EQ '').OK                                      18420000
         MNOTE 4,'** TOO MANY POSITIONAL OPERANDS -- IGNORED **'        18430000
.OK      ANOP                                                           18440000
&NFS     LA    R0,&CCW                                                  18450000
         BA&XA R10,TAPIO                                                18460000
         B     &TM                      TAPEMARK FOUND                  18470000
         SPACE 1                                                        18480000
         MEND                                                           18490000
         EJECT ,                                              09/84 DBC 18500000
         MACRO                                                          18510000
&NFS     NEWPAGE  &C,&F                                                 18520000
         GBLC  &XA                                            02/93 RT  00021005
         AIF   ('&C' EQ '').JUSTBAL                                     18530000
         AIF   ('&C' EQ '(R0)').R0                                      18540000
&NFS     LA    R0,&C                                                    18550000
.R0      BA&XA R10,NEWPAGE&F                                            18560000
         MEXIT                                                          18570000
.JUSTBAL ANOP                                                           18580000
&NFS     BA&XA R10,NEWPAGE&F+4                                          18590000
         MEND                                                           18600000
         EJECT ,                                              09/84 DBC 18610000
         MACRO                                                          18620000
&NFS     IFP2  &L,&B                                                    18630000
&NFS     TM    TFLAG2,T2@PRT2                                           18640000
         AIF   ('&L'(1,1) EQ 'N').NO                                    18650000
         BO    &B                                                       18660000
         MEXIT                                                          18670000
.NO      BNO   &B                                                       18680000
         MEND                                                           18690000
         EJECT ,                                              09/84 DBC 18700000
         MACRO                                                          18710000
&NFS     ICALL &RTN,&REG,&R1=,&R15=                                     18720000
         GBLC  &XA                                            02/93 RT  00021005
         LCLC  &NAME                                                    18730000
&NAME    SETC  '&NFS'                                                   18740000
         AIF   ('&R1' EQ '').A                                          18750000
&NAME    LA    R1,&R1                                                   18760000
&NAME    SETC  ''                                                       18770000
.A       AIF   ('&R15' EQ '').B                                         18780000
&NAME    LA    R15,&R15                                                 18790000
&NAME    SETC  ''                                                       18800000
.B       ANOP                                                           18810000
&NAME    BA&XA &REG,&RTN                                                18820000
         SPACE 1                                                        18830000
         MEND                                                           18840000
*---------------------------------------------------------------*   DRK
* CHANGES BY JOHN KALINICH 2007/04/26  VERSION 2.5              *   DRK
*                                                               *   DRK
* ADDED NEW FDR DEVICE TYPES AND CODES TO TABLE.                *   DRK
*    CHANGES BY STEVE MYERS HAVE BEEN SKIPPED.                  *   SBG
*---------------------------------------------------------------*       00001604
* CHANGES BY SAM GOLOB   2005/10/31    VERSION 2.3              *
*                                                               *
* IEBCOPY RECOGNITION CODE SHOULD NOT DEPEND ON FIRST BYTE.     *
* IBM IS USING FLAGS THERE FOR IEBCOPY UNLOADING PDSE'S.        *
*---------------------------------------------------------------*       00001604
* CHANGES BY SAM GOLOB   2005/10/11    VERSION 2.3              *
*                                                               *
* TESTED UCB TYPE BYTE 4 FOR THE X'80' BIT INSTEAD OF X'80' AND *
* X'81' EXACTLY, IN DETERMINING IF A TAPE IS A CARTRIDGE OR     *
* NOT.  THIS ALLOWS DETECTION OF 3590'S AS CARTRIDGES.          *
*---------------------------------------------------------------*       00001604
* CHANGES BY SAM GOLOB   2005/02/02    VERSION 2.2              *
*                                                               *
* ALLOWED FOR SMP/E CHANGES FOR RECENT SYSTEM LEVELS. Z/OS 1.5  *
* A ++ASSIGN CARD ON THE TOP OF AN SMPPTFIN FILE USED TO THROW  *
* OFF THE SMPPTFIN FORMAT DETECTION.  NOW A FEW DIFFERENT SMP/E *
* CONTROL CARDS WILL INDICATE THE PRESENCE OF AN SMPPTFIN FILE, *
* BUT THEY WILL NOT CAUSE THE PRINT OF SYSMOD NUMBERS IN THE    *
* SYSPRNT2 DD NAME.  THESE DELIBERATELY DO NOT INCLUDE SUCH     *
* SMP/E CONTROL CARDS THAT WOULD NORMALLY OCCUR IN THE MIDDLE   *
* OF A PTF OR OTHER SYSMOD, SUCH AS:  ++MOD, ++MAC, ++SRC,      *
* ++MACUPD, ++SRCUPD, OR THE ++DATA TYPE OF CARDS.  ++JCLIN     *
* IS INCLUDED IN THE FOLLOWING LIST, EVEN THOUGH IT NORMALLY    *
* WOULD OCCUR WITHIN A PTF.  THE REASON FOR THAT IS THAT IT IS  *
* POSSIBLE FOR A PURE ++JCLIN STREAM TO BE INPUTTED TO SMP/E,   *
* SO SUCH A FILE SHOULD BE MARKED AS THOUGH IT IS IN SMPPTFIN   *
* FORMAT, PROVIDED THAT A ++JCLIN CARD IS INSERTED AT THE TOP   *
* OF THE FILE.                                                  *
*                                                               *
*   LIST OF SUCH SUPPORTED SMP/E CONTROL CARDS CURRENTLY....    *
*  ++ ASSIGN                                                    *
*  ++ HOLD                                                      *
*  ++ RELEASE                                                   *
*  ++ FEATURE                                                   *
*  ++ PRODUCT                                                   *
*  ++ NULL                                                      *
*  ++ JCLIN                                                     *
*                                                               *
*---------------------------------------------------------------*       00001604
* CHANGES BY PETER MCFARLAND  02/12/04                         2/04PMCF 00000204
* WHEN AN I/O OCCURS (ECB=X'41') AN ATTEMPT TO GET THE UCB     2/04PMCF 00000204
* EXTENSION IS MADE FOR THE I/O SENSE BYTES.  IF THE UCBS ARE  2/04PMCF 00000204
* GENED TO BE ABOVE THE 16M LINE THEN THE ADDR. OF THE UCB-EX  2/04PMCF 00000204
* IN THE UCB IS NOT VALID. S0C4 ABENDS OCCUR IN RECOVERY.      2/04PMCF 00000204
* CODE WAS ADDED TO BRANCH AROUND THIS SITUATION SKIPPING THE  2/04PMCF 00000204
* SENSE BYTES DISPLAY.  SEE MSG IOS000I FOR SENSE BYTES. PMCF  2/04PMCF 00000204
*------------------------------------------------------------  2/04PMCF 00001604
* CHANGES BY JOEL C. EWING 2003/06/25:                         6/03 JCE
* CHANGED DAY-OF-WEEK CALCULATION IN TODAY CSECT TO USE        6/03 JCE
* SIMPLER AND HOPEFULLY CORRECT ALGORITHM BASED ON LILIAN      6/03 JCE
* DATES.                                                       6/03 JCE
*------------------------------------------------------------  6/03 JCE
* ELIMINATED THE X'00' IN THE REPORT HEADERS, BY BLANKING OUT SBG 06/01
* THE DATE, DAY AND TIME WORK FIELDS IN THE TODAY CSECT.      SBG 06/01
*------------------------------------------------------------  2/00 JCH 00001604
* CLUDGED DAY OF WEEK 2 DAYS LATER FOR Y2K, IN TODAY CSECT.   SBG 03/00
*------------------------------------------------------------  2/00 JCH 00001604
* CHANGES BY JOHN C HALLGREN  02/10/00                         2/00 JCH 00000204
*                                                              2/00 JCH 00000204
* SET HDR2 DENSITY TO ACTUAL IF NO HDR2 FOUND (DOS SL, RS6000) 2/00 JCH 00000204
*------------------------------------------------------------  3/94 SBG 00001604
* CHANGES BY SAM GOLOB  3/94:                                  3/94 SBG 00000204
*                                                              3/94 SBG 00000204
*   CONVUNIT FIXED FOR 3390'S.                                 3/94 SBG 00000204
*   IHDASDR1 FIXED FOR 3390'S BUT PROBABLY NOT NECESSARY.      3/94 SBG 00000204
*------------------------------------------------------------  7/93 SBG 00001604
*                                                              6/93 RT  00000204
* CHANGES BY RON TANSKY 6/93:                                  6/93 RT  00000304
*                                                              6/93 RT  00000404
* ADDED CHECK FOR IEBCOPY PDS/E HEADER                         6/93 RT  00000404
*                                                              6/93 RT  00000404
*                                                              1/93 RT  00000204
* CHANGES BY RON TANSKY 1/93:                                  1/93 RT  00000304
*                                                              1/93 RT  00000404
*  GOT RID OF THE ENT (ENTRY) MACRO.  IT'S MASSIVE OVERKILL IN 1/93 RT  00000512
*  THIS PGM.  REPLACED IT BY SIMPLE INLINE CODE.  THIS SAVES   1/93 RT  00000604
*  14 PAGES WORTH OF LISTING (AT 80 LINES PER PAGE).           1/93 RT  00000704
*                                                              1/93 RT  00000812
*  GOT RID OF OTHER UNUSED MACROS.                             1/93 RT  00000912
*                                                              1/93 RT  00001004
*  ADDED CAPABILITY OF MAPPING DOS SL TAPES.                   1/93 RT  00001112
*  CHANGED WAY PGM RECOGNIZES 'IEHINITT' TAPES, AS IT WAS      1/93 RT  00001212
*  CALLING DOS SL TAPES 'IEHINITT'.                            1/93 RT  00001312
*                                                              1/93 RT  00001412
*  CHANGED DEFAULT TO 80 LINES PER PAGE (&LINEPPG)             1/93 RT  00001509
*                                                              1/93 RT  00001709
*  CHANGED LINE-COUNTER-2 TO STORAGE INSTEAD OF A REGISTER.    1/93 RT  00001812
*                                                              1/93 RT  00001912
*  CHANGED SOME REGISTER EQUATES TO STANDARD (RNN).            1/93 RT  00002012
*                                                              1/93 RT  00002109
*  ADDED A VERSION NUMBER TO THE TITLE PAGE.                   1/93 RT  00002210
*                                                              1/93 RT  00002310
*  MOVED THE CURRENT-DATE-AND-TIME ROUTINE TO ITS OWN CSECT.   1/93 RT  00002406
*  CHANGED THE ROUTINE TO INLINE CODE RATHER THAN MACRO CALL.  1/93 RT  00002512
*  MODIFIED THE ROUTINE TO HANDLE THE CENTURY.                 1/93 RT  00002607
*                                                              1/93 RT  00002707
*  ADDED A BASE REGISTER (R9).                                 1/93 RT  00002806
*  BETWEEN MOVING THE DATE ROUTINE AND THE EXTRA BASE REG,     1/93 RT  00002907
*  THIS SHOULD PROVIDE LOTS OF ROOM FOR EXPANSION.             1/93 RT  00003007
*                                                              1/93 RT  00003107
*  ADDED CONDITIONAL ASSEMBLY FOR MVS/XA SUPPORT.  SYMBOLIC    1/93 RT  00003107
*  VARIABLE '&XA' IS SET TO 'L' OR 'S' TO GENERATE BAL         1/93 RT  00003107
*  OR BAS INSTRUCTION, RESPECTIVELY                            1/93 RT  00003107
*                                                              1/93 RT  00003107
*------------------------------------------------------------ 12/88 SBG 00001604
*  SOMEBODY SHOULD ADD SUPPORT FOR DFDSS TAPES.               12/88 SBG 00001701
*------------------------------------------------------------ 11/88 SBG 00001801
*  THIS PROGRAM, AS IT IS, IS SORELY IN NEED OF A NEW BASE    11/88 SBG 00002001
*   REGISTER AND/OR RE-ARCHITECTING, SUCH AS BEING SPLIT INTO 11/88 SBG 00003001
*   TWO SEPARATE MODULES.  FUTURE MODIFIERS, PLEASE BEAR      11/88 SBG 00004001
*   THIS IN MIND.  THREE BASE REGISTERS ALMOST EXCEEDED.      11/88 SBG 00005001
*                                                             12/88 SBG 00005101
*  FOOTNOTE:  I WAS ABLE TO ADDRESS SEVERAL HUNDRED MORE      12/88 SBG 00005201
*   BYTES BY MOVING SOME OF THE FIXED LITERALS TO THE WORK    12/88 SBG 00005301
*   AREA DSECT.  THESE HAVE TO BE INITIALIZED IN THE GETMAINED12/88 SBG 00005401
*   SPACE WITH EVERY EXECUTION OF THE PROGRAM.  AN EXACT COPY 12/88 SBG 00005501
*   OF THESE LITERALS (WHICH ARE CLEARLY MARKED) IS PUT AT    12/88 SBG 00005601
*   THE VERY END OF THE PROGRAM.  SEE LABEL "LITMOVES".       12/88 SBG 00005701
*   LITMOVES IS ADDRESSABLE IN THE PROGRAM VIA AN ADCON.      12/88 SBG 00005801
*                                                             12/88 SBG 00005901
*  FUTURE MODIFIERS, PLEASE BEWARE OF THIS BEFORE             12/88 SBG 00006001
*   ADDING NEW LITERALS.                                      12/88 SBG 00006101
*------------------------------------------------------------ 11/88 SBG 00007001
*  DO NOT GLOBALLY CHANGE MEANINGS OF ANY REGISTER EQUATES.   11/88 SBG 00010001
*   UNLESS YOU ARE VERY VERY THOROUGH.                        11/88 SBG 00010204
*------------------------------------------------------------ 11/88 SBG 00010301
*------------------------------------------------------------ 07/92 SBG 00020105
*                                                             07/92 SBG 00020305
*  *************                                              SBG 03/00 00020405
*  * ATTENTION * YOU CAN USE IECTDEBX INSTEAD OF IEZDEB TO    SBG 03/00 00020505
*  ************* SPECIFY DEB FIELDS.  I DON'T RECOMMEND THE   SBG 03/00 00020605
*                IDEA.  IT IS BETTER TO CODE DEBX=NO.         SBG 03/00 00020705
*                                                             SBG 03/00 00020805
*   IEZDEB IS ALIVE AND WELL.  AT THE SP4 LEVEL, I THOUGHT    SBG 03/00
*    IT WASN'T, SO I CODED THIS FLEXIBILITY.  BETTER TO       SBG 03/00
*    LEAVE THINGS AS THEY ALWAYS WERE.                        SBG 03/00
*                                                             SBG 03/00
         GBLC  &DEBX                                          07/92 SBG 00021005
*&DEBX   SETC  'YES'    'YES' TO USE IECTDEBX MACRO FOR DEB.  SBG 03/00 00022007
&DEBX    SETC  'NO'     'NO'  TO USE IEZDEB   MACRO FOR DEB.  SBG 03/00 00023007
*------------------------------------------------------------ 02/93 RT  00020105
         GBLC  &XA                                            02/93 RT  00021005
&XA      SETC  'L'      'L' TO USE NON-XA BAL INSTRUCTION     02/93 RT  00022007
*&XA     SETC  'S'      'S' TO USE     XA BAS INSTRUCTION     02/93 RT  00022007
         LCLB  &HERC    SET TO TRUE IF COMPILING IN HERCULES  07/02 SRS
&HERC    SETB  1        YES, THIS IS A HERCULES VERSION       07/02 SRS
*&HERC   SETB  0        NO                                    07/02 SRS
*                                                             07/92 SBG 00024005
*------------------------------------------------------------ 07/92 SBG 00029005
TAPEMAP  TITLE 'TAPE ANALYSIS PROGRAM  (T A P E M A P)'                 00030000
*********************************************************************** 00040000
*                                                                     * 00050000
*                                                                     * 00060000
*  THIS PROGRAM MAY NOT BE DISTRIBUTED WITHOUT PERMISSION OF THE      * 00070000
*  AUTHOR.  ALTHOUGH THIS PROGRAM HAS BEEN EXTENSIVELY TESTED, AND    * 00080000
*  IS IN USE IN A PRODUCTION ENVIRONMENT (MVT 21.8 ON A 360/91) AT    * 00090000
*  UCLA'S OFFICE OF ACADEMIC COMPUTING (FORMERLY CAMPUS COMPUTING     * 00100000
*  NETWORK), NO GUARANTEE IS MADE OF (OR RESPONSIBILITY ASSUMED FOR)  * 00110000
*  CORRECT OR RELIABLE OPERATION.  WE MAY TRY TO HELP WITH PROBLEMS.  * 00120000
*                                                                     * 00130000
*  CONTACT:  CHRIS THOMAS  (213) 825-7424                             * 00140000
*                                                                     * 00150000
*********************************************************************** 00160000
         SPACE 2                                                        00170000
*********************************************************************** 00180000
*                                                                     * 00190000
*  THIS PROGRAM IS BASED ON THE PROGRAM TAPEINDX WRITTEN BY           * 00200000
*  MICHAEL S. MAITEN OF THE UCLA COMPUTER CLUB.                       * 00210000
*  IT HAS BEEN ALMOST COMPLETELY REWRITTEN BY LEONARD D. WOREN OF     * 00220000
*  THE UCLA COMPUTER CLUB.                                            * 00230000
*                                                                     * 00240000
*  MODIFIED  1979 BY WALT FARRELL, RAINIER NATIONAL BANK              * 00250000
*                                                           * 09/84 DBC 00260000
*  HI GUYS. GEE, CAN I PLAY TOO?                            * 09/84 DBC 00270000
*                                                                     * 00280000
*********************************************************************** 00290000
         EJECT ,                                              06/87 THO 00300000
************************************************************* 07/92 SBG 00301004
*                                                             07/92 SBG 00302004
* LAST CHANGE DATE - JUL. 17, 1992, BY SAM GOLOB.             07/92 SBG 00302104
*                                                             07/92 SBG 00302204
*  07/17/92  -  FIXED 2 PROBLEMS.  NEW IEBCOPY FOR NL TAPE    07/92 SBG 00303004
*               FILES WASN'T WORKING BECAUSE THE HEADER RECD  07/92 SBG 00304004
*               EXPANDED BY 4 BYTES, AND ITS SIZE WAS BEING   07/92 SBG 00305004
*               CHECKED AT LABEL "NL$NMOVE".                  07/92 SBG 00306004
*                                                             07/92 SBG 00307004
*               CONDITIONAL ASSEMBLY FOR MVS SP4 SYSTEMS      07/92 SBG 00308004
*               WHICH HAVE DEB EXTENSION SUPPORT AND IEZDEB   07/92 SBG 00309004
*               MACRO ISN'T THERE ANY MORE.  I USED IECTDEBX. 07/92 SBG 00309104
*               PLEASE SET GLOBAL VARIABLE &DEBX TO 'YES' OR  07/92 SBG 00309204
*               'NO' ABOVE.                                   07/92 SBG 00309304
*                                                             07/92 SBG 00309404
************************************************************* 07/92 SBG 00309504
         EJECT ,                                              07/92 SBG 00309604
************************************************************* 11/88 SBG 00310000
*                                                             11/88 SBG 00320000
* LAST CHANGE DATE - NOV. 28, 1988, BY SAM GOLOB.             11/88 SBG 00330001
*                                                             11/88 SBG 00340000
*  INSERTED SUPPORT FOR ><ADD NAME= WITHIN IEBUPDTE UNLOADS.  11/88 SBG 00350000
*  INSERTED GLOBAL VARIABLE FOR LINES-PER-PAGE ON REPORTS.    11/88 SBG 00350101
*     SEE VARIABLE &LINEPPG.                                  11/88 SBG 00350201
*  INSERTED SUPPORT FOR CBT973-COMPRESSED FILES.              11/88 SBG 00351000
*                                                             11/88 SBG 00360000
*  CBT973 SUPPORT MAKES IT POSSIBLE TO MEANINGFULLY MAP       11/88 SBG 00360101
*  AND FIND ALMOST EVERYTHING ON THE CBT MVS MODS TAPE.       11/88 SBG 00360201
*                                                             11/88 SBG 00360300
*  IT ALSO HELPS IN FINDING IBM SOURCE MODULES IN CBT973-     11/88 SBG 00360600
*  COMPRESSED IEBUPDTE FORMAT.                                11/88 SBG 00360700
*                                                             11/88 SBG 00360800
*  SEE CBT TAPE FILES 188 AND 189 FOR A DESCRIPTION OF THIS   11/88 SBG 00360900
*  METHOD OF SQUEEZING MANY IBM SOURCE TAPE REELS TOGETHER    11/88 SBG 00361000
*  ON A FEW LARGE REELS.  TAPEMAP WILL NOW SHOW MEMBER NAMES  11/88 SBG 00361100
*  ON THE CBT973-COMPRESSED LARGE REELS.                      11/88 SBG 00361200
*                                                             11/88 SBG 00361300
*  CBT973 SUPPORT IS DONE BY USING THE CBT973 EXPANSION       11/88 SBG 00361401
*  ALGORITHM TO RESTORE ONE 80-BYTE RECORD AT A TIME.  NORMAL 11/88 SBG 00361501
*  IEBUPDTE PROCESSING IS THEN DONE ON THIS RECORD.           11/88 SBG 00361601
*                                                             11/88 SBG 00361701
************************************************************* 11/88 SBG 00361800
         EJECT ,                                              06/87 THO 00361900
************************************************************* 11/87 SBG 00362000
*                                                                   SBG 00362100
* LAST CHANGE DATE - JUNE 11, 1986, BUT ADDED 11/87 BY TENNIE OLSON THO 00362200
*                                                                   SBG 00362300
*  FIXED BUG  -  SAM GOLOB  -  NEWSWEEK INC., MOUNTAIN LAKES, N.J.  SBG 00362400
*                                                                   SBG 00363000
*  ORIGINAL CODE WOULD NOT DETECT AN APAR TAPE AS SUCH.             SBG 00370000
*                                                                   SBG 00380000
*  TAPEMAP HAS TO CHECK FOR SMPPTFIN FORMAT BEFORE IT CHECKS        SBG 00390000
*   FOR IEBUPDTE FORMAT.  IF A PTF OR APAR HAS SOURCE UPDATES       SBG 00400000
*   WHICH ARE DONE WITH IEBUPDTE, AND THIS PTF OR APAR IS AT THE    SBG 00410000
*   BEGINNING OF THE TAPE TO BE READ, THE PROGRAM WOULD NOT PRINT   SBG 00420000
*   THE PTF OR APAR NAMES, BUT THE NAMES OF THE PROGRAMS BEING      SBG 00430000
*   UPDATED, WHICH IS NOT WHAT WE WANT WHEN WE MAP AN APAR TAPE.    SBG 00440000
*   WE ALWAYS WANT THE NAMES OF THE PTFS AND APARS UNDER THESE      SBG 00450000
*   CIRCUMSTANCES.                                                  SBG 00460000
*                                                                   SBG 00470000
*  THEREFORE I AM MOVING THE CODE WHICH FEELS FOR SMPPTFIN FORMAT   SBG 00480000
*  BEFORE THE CODE WHICH FEELS FOR IEBUPDTE FORMAT, TO DETECT       SBG 00490000
*  AN APAR TAPE WHEN WE HAVE ONE.                                   SBG 00500000
*                                                             12/88 SBG 00501001
************************************************************* 07/85 SBG 00510000
         EJECT ,                                              06/87 THO 00520000
************************************************************* 06/87 THO 00530000
*                                                           * 06/87 THO 00540000
* LAST CHANGE DATE - JUNE 09, 1987                          * 06/87 THO 00550000
*                                                           * 06/87 THO 00560000
*                  - I ADDED SUPPORT FOR 3480 CARTRIDGES.   * 06/87 THO 00570000
*                    ADDED CODE TO MAKE CORRECT CALCULA-    * 06/87 THO 00580000
*                    TIONS IN TAPEFEET. 37871 BYTES PER     * 06/87 THO 00590000
*                    IN OR 302971 BITS PER INCH WITH A      * 06/87 THO 00600000
*                    GAP OF .08 INCHES. INDICATED DEN WAS   * 06/87 THO 00610000
*                    NOT APPLICABLE FOR 3480.               * 06/87 THO 00620000
*                                                           * 06/87 THO 00630000
*     TENNIE OLSON                                            12/88 SBG 00631001
*     VF CORPORATION - WRANGLER DIVISION                      12/88 SBG 00632001
*     201 NORTH EUGENE STREET                                 12/88 SBG 00633001
*     GREENSBORO, N.C. 27401                                  12/88 SBG 00634001
*     (919) 373-3952                                          12/88 SBG 00635001
*                                                             12/88 SBG 00636001
************************************************************* 06/87 THO 00640000
         EJECT ,                                              07/85 DBC 00650000
************************************************************* 07/85 DBC 00660000
*                                                           * 07/85 DBC 00670000
* LAST CHANGE DATE - JULY 26, 1985                          * 07/85 DBC 00680000
*                                                           * 07/85 DBC 00690000
*                  - I ADDED SUPPORT FOR LISTING THE PTFS   * 07/85 DBC 00700000
*                    CONTAINED ON AN SMPPTFIN TAPE.         * 07/85 DBC 00710000
*                                                           * 07/85 DBC 00720000
*                  - WHEN AN SL TAPE IS BEING SCANNED WITH  * 07/85 DBC 00730000
*                    NL ANALYSIS, THE LOGICAL FILE NUMBER   * 07/85 DBC 00740000
*                    IS DISPLAYED ON THE "LABELS" LINE      * 07/85 DBC 00750000
*                    (OLD), AND THE PHYSICAL FILE NUMBER IS * 07/85 DBC 00760000
*                    DISPLAYED ON THE "SCAN" LINE.          * 07/85 DBC 00770000
*                                                           * 07/85 DBC 00780000
************************************************************* 07/85 DBC 00790000
         EJECT ,                                              09/84 DBC 00800000
************************************************************* 09/84 DBC 00810000
*                                                           * 09/84 DBC 00820000
* LAST CHANGE DATE - OCTOBER 1, 1984                        * 09/84 DBC 00830000
*                                                           * 09/84 DBC 00840000
*                  - I RECEIVED FROM ARNIE CASINGHINO (OF   * 09/84 DBC 00850000
*                    THE CONNECTICUT BANK AND TRUST) A TAPE * 09/84 DBC 00860000
*                    CONTAINING A VERSION OF TAPEMAP THAT   * 09/84 DBC 00870000
*                    WAS BASED ON AN EARLIER UCLA VERSION   * 09/84 DBC 00880000
*                    AND WAS MODIFIED BY WALT FARRELL OF    * 09/84 DBC 00890000
*                    THE RAINIER NATIONAL BANK. THE MODS    * 09/84 DBC 00900000
*                    WERE MAINLY IN SUPPORT OF DETAILED     * 09/84 DBC 00910000
*                    LISTINGS OF THE CONTENTS OF FDR AND    * 09/84 DBC 00920000
*                    FDRDSF TAPES. I HAVE NOW TRANSPORTED   * 09/84 DBC 00930000
*                    THOSE MODS INTO THIS VERSION OF        * 09/84 DBC 00940000
*                    TAPEMAP.                               * 09/84 DBC 00950000
*                                                           * 09/84 DBC 00960000
*                  - I ADDED SUPPORT FOR BLKSIZES GREATER   * 09/84 DBC 00970000
*                    THAN 32K UP TO 64K-1 BYTES. (THE       * 09/84 DBC 00980000
*                    RECENT RELEASES OF FDR WRITE BLOCKS    * 09/84 DBC 00990000
*                    THAT ARE 50K LARGE OR LARGER).         * 09/84 DBC 01000000
*                                                           * 09/84 DBC 01010000
*                  - I HAVE ADDED A REWIND COMMAND JUST     * 09/84 DBC 01020000
*                    AFTER THE TAPE IS OPENED TO INSURE     * 09/84 DBC 01030000
*                    THAT THE TAPE IS PROPERLY POSITIONED.  * 09/84 DBC 01040000
*                    THIS MAKES MAPPING STANDARD LABELED    * 09/84 DBC 01050000
*                    TAPES IN A UCC/1 ENVIRONMENT A LITTLE  * 09/84 DBC 01060000
*                    EASIER.                                * 09/84 DBC 01070000
*                                                           * 09/84 DBC 01080000
************************************************************* 09/84 DBC 01090000
         EJECT ,                                              06/84 DBC 01100000
************************************************************* 06/84 DBC 01110000
*                                                           * 06/84 DBC 01120000
* LAST CHANGE DATE - JUNE 4, 1984                           * 06/84 DBC 01130000
*                                                           * 06/84 DBC 01140000
*                  - ADDED OPSYN STATEMENTS TO SUPPRESS THE * 06/84 DBC 01150000
*                    AUTHOR'S USE OF "PRINT NOGEN".         * 06/84 DBC 01160000
*                                                           * 06/84 DBC 01170000
*                  - ADDED CODE TO RECOGNIZE FILES IN       * 06/84 DBC 01180000
*                    IEBUPDAT RELOAD FORMAT.                * 06/84 DBC 01190000
*                                                           * 06/84 DBC 01200000
*                  - DELETED THE SILLY JFCBDSNM CHANGE THAT * 06/84 DBC 01210000
*                    MADE TAPE MANAGEMENT SYSTEMS SO        * 06/84 DBC 01220000
*                    UNHAPPY.                               * 06/84 DBC 01230000
*                                                           * 06/84 DBC 01240000
*                  - UPDATED THE LIST OF KNOWN DEVICE TYPES * 06/84 DBC 01250000
*                    TO INCLUDE 3375S AND 3380S.            * 06/84 DBC 01260000
*                                                           * 06/84 DBC 01270000
*                  - CHANGED THE PARM DEFAULT FROM NOSCAN   * 06/84 DBC 01280000
*                    TO SCAN.                               * 06/84 DBC 01290000
*                                                           * 06/84 DBC 01300000
*                  - ADDED THE "INFO SOURCE" COLUMN TO THE  * 06/84 DBC 01310000
*                    SYSPRINT OUTPUT.                       * 06/84 DBC 01320000
*                                                           * 06/84 DBC 01330000
*                  - FIXED THE CCC ON THE LINE OF DASHES    * 06/84 DBC 01340000
*                    (WAS RANDOM, NOW IS C' ').             * 06/84 DBC 01350000
*                                                           * 06/84 DBC 01360000
************************************************************* 06/84 DBC 01370000
         EJECT                                                          01380000
*********************************************************************** 01390000
*                                                                     * 01400000
*   UPDATES:                                                          * 01410000
*                                                                     * 01420000
*  ADD CHECK FOR FILE WITH HEADER OF                     05/01/79 WBF * 01430000
*    $$TAPEMAP.PRINT.FILE$$                                           * 01440000
*  AND PRINT THE CONTENTS OF THE FILE                                 * 01450000
*  IF ONE IS FOUND                                                    * 01460000
*                                                                     * 01470000
*  LIST DETAILS OF SLICK BACKUP FILES                    04/23/79 WBF * 01480000
*                                                                     * 01490000
*  FIX MINOR BUGS IN NL CODE,                            04/08/79 WBF * 01500000
*  LIST DETAILS OF FDR OR FDRDSF FILES,                      "        * 01510000
*  FORCE BLP AND EXPDT=98000 (FOR UCC-1),                    "        * 01520000
*  COSMETIC CHANGES FOR RAINIER NATIONAL BANK                "        * 01530000
*                                                                     * 01540000
*                                                                     * 01550000
*  FIX BUG IN I/O ERROR RECOVERY                     U16 06/16/79 LDW * 01560000
*                                                                     * 01570000
*  FIX VOLSER FOR NL TAPES;  PRINT SENSE BIT MEANINGS;  CHANGE FORMAT * 01580000
*  OF "REQUESTED VOL" MSG;  FIX ./DDNAME= BUG        U15 02/10/79 LDW * 01590000
*                                                                     * 01600000
*  RE-ARRANGE CODE, ADD 'NULL=' PARM.                U14 06/28/78 LDW * 01610000
*                                                                     * 01620000
*  PRINT SENSE INFO WHEN I/O ERROR OCCURS            U13 05/05/78 LDW * 01630000
*                                                                     * 01640000
*  FIX A COUPLE OF TURKEY BUGS                03/24/78 - 04/06/78 LDW * 01650000
*                                                                     * 01660000
*  LIST MEMBERS IN AN IEBUPDTE INPUT STREAM              02/22/78 LDW * 01670000
*                                                                     * 01680000
*  LIST MEMBERS UNLOADED BY IEHMOVE                      01/05/78 LDW * 01690000
*                                                                     * 01700000
*  NL TAPE ANALYSIS CODE ADDED                           12/26/77 LDW * 01710000
*                                                                     * 01720000
*  DO A SENSE TO DETERMINE TRUE TAPE DENSITY             11/14/77 LDW * 01730000
*                                                                     * 01740000
*  FIX SOME MISCELLANEOUS MINOR BUGS                     11/08/77 LDW * 01750000
*                                                                     * 01760000
*  ADD CODE TO LIST MEMBERS UNLOADED BY IEBCOPY, AND ATTRS FOR        * 01770000
*     IEBISAM AND IEHDASDR UNLOADED DATASETS.            03/16/77 LDW * 01780000
*                                                                     * 01790000
*  GENERAL RE-WRITING OF TAPE READ CODE TO PUT IN CODE TO PRINT OUT   * 01800000
*     THE ORIGINAL ATTRS FOR UNLOADED DATASETS CREATED BY IEHMOVE     * 01810000
*     (SYSMOVE) AND IEBCOPY (VS2COPY);  CHANGE TAPE I/O ERROR AND     * 01820000
*     TAPE MARK DETECTION ALGORITHM.                     02/13/77 LDW * 01830000
*                                                                     * 01840000
*  PUT IN DSECTS FOR LABELS;  FIX A 01/06/77 BUG;  RE-WRITE PRINTLIN  * 01850000
*     MACRO (CHANGE TO PRTLN)                            02/07/77 LDW * 01860000
*                                                                     * 01870000
*  MORE FIXES:  COUNT LINES AND DO PAGE EJECT;  AND MUCH OTHER        * 01880000
*     MISCELLANEOUS STUFF                                01/06/77 LDW * 01890000
*                                                                     * 01900000
*  UNCLUDGED A LITTLE:  FIX LENGTH CALCULATION;  MAKE SOME MACROS     * 01910000
*     INTO SUBROUTINES (IT SEEMS THAT MSM NEVER HEARD OF SUBROUTINES; * 01920000
*     PUT IN SYMBOLIC OFFSETS FOR PRINT LINE INFO.       11/08/76 LDW * 01930000
*                                                                     * 01940000
*  TAPEINDX -- CLUDGED UP IN A HURRY FROM A SPASM PROGRAM TO MAKE A   * 01950000
*          QUICK LOAD MODULE                             10/30/74 MSM * 01960000
*                                                                     * 01970000
*********************************************************************** 01980000
         EJECT                                                          18850000
         GBLA  &LINEPPG                                       11/88 SBG 18851001
&LINEPPG SETA  60                                             11/88 SBG 18852001
TAPEMAP  CSECT                                                          18811002
         USING *,R15                                                    18812002
         B     ARNDCONS                                                 18813002
         DC    AL1(25)                                                  18814002
         DC    CL8'TAPEMAP'                                             18815002
         DC    CL8'&SYSDATE'                                            18816002
         DC    CL8'&SYSTIME'                                            18817002
         DS    0F                                                       18817102
GETM     DC    AL1(0),AL3(WORKLEN)                                      18817202
ARNDCONS DS    0H                                                       18818002
         SAVE  (14,12)                                                  18818102
         DROP  R15                                                      18819002
         USING TAPEMAP,R12,R11,R7,R9                                    18819102
         LR    R12,R15                                                  18819302
         LA    R11,4095(,R12)                                           18819402
         LA    R11,1(,R11)                                              18819502
         LA    R7,4095(,R11)                                            18819602
         LA    R7,1(,R7)                                                18819702
         LA    R9,4095(,R7)                                             18819802
         LA    R9,1(,R9)                                                18819902
         L     R2,0(,R1)            PICK UP PARM POINTER
         L     R0,GETM                                                  18820002
         GETMAIN R,LV=(0)                                               18820102
         ST    R1,8(,R13)           FORWARD SAVE-AREA CHAIN PTR         18820202
         ST    R13,4(,R1)           BACKWARD SAVE-AREA CHAIN PTR        18820302
         LR    R13,R1               NEW SAVE-AREA ADDR                  18820402
         USING WORKD,R13                                                18820502
         L     R15,=A(LITMOVES)     POINT TO LITERALS TO LOAD 12/88 SBG 18871001
         MVC   EMPTYLIT(256),0(R15)     MOVE THEM TO WORKAREA 12/88 SBG 18872001
         MVC   EMPTYLIT+256(256),256(R15)                     10/90 SBG 18872101
         MVC   EMPTYLIT+512(LITMVLEN-512),512(R15)            10/90 SBG 18873001
         MVI   BLANKS,C' '                                              18880000
         MVC   BLANKS+1(L'BLANKS+L'OUTBUFF),BLANKS  GET OUTBUFF ALSO    18890000
         MVC   OUTCLR2(L'OUTBUFF2+2),OUTCLEAR  GET CC FOR "DASHES" ALSO 18900000
         MVC   DASHES(2),=C' -'    PRIME                      06/84 DBC 18910000
         MVC   DASHES+2(L'DASHES-2),DASHES+1                            18920000
         MVC   PARM#MSG+1(L'PARM#MSG-1),PARM#MSG                        18930000
         XC    JFCB(176),JFCB                                           18940000
         LA    R0,JFCB                                                  18950000
         ST    R0,EXLST                                                 18960000
         MVI   EXLST,X'87'                                              18970000
         LA    R0,TAPEBUFF              GET ADDR OF DATA BUFFER         18980000
         ST    R0,TCCW#DAT              SET INTO CCW                    18990000
         MVI   TCCW#DAT,RD              RESTORE THE OPCODE              19000000
         LA    R1,FL1LABI               GET LABEL BUFFER ADDR           19010000
         ST    R1,TCCW#LBL              SET IN CCW                      19020000
         MVI   TCCW#LBL,RD              RESTORE OPCODE                  19030000
         ST    R1,TCCW#EOV              SET IN CCW            02/93 RT  19020000
         MVI   TCCW#EOV,RD              RESTORE OPCODE        02/93 RT  19030000
         LA    R0,FL2LABI               GET LABEL BUFFER ADDR           19040000
         ST    R0,TCCW#LBL+8            SET IN CCW                      19050000
         MVI   TCCW#LBL+8,RD            RESTORE OPCODE                  19060000
         LA    R1,MV#BUFF2              GET 2ND BUFFER ADDR             19070000
         ST    R1,MV#ABUF2              SAVE                            19080000
         LA    R0,OUTBUFF2+110          GET ADDR OF END OF OUTBUFF2     19090000
         ST    R0,OB2END                SAVE FOR FUTURE REFERENCE       19100000
         LA    R0,OUTBUFF2+110+10       FOR UPDTE STUFF                 19110000
         ST    R0,OB2END2                                               19120000
         SPACE 2                                                        19130000
         OPEN  MF=(E,OPENMFL)           OPEN PRINT FILE                 19140000
         L     R1,OPENMFL               GET SYSPRINT DCB ADDR           19150000
         TM    48(R1),X'10'             OPEN?                           19160000
         BO    PRINTOK                  YES                             19170000
         LA    R1,100                   GET ABEND CODE                  19180000
         SVC   13                       AND LEAVE                       19190000
         SPACE 2                                                        19200000
PRINTOK  LH    R15,0(,R2)               GET PARM LEN                    19210000
         LTR   R15,R15                  ANY?                            19220000
         BNP   DONEPARM                 NO                              19230000
         LA    R14,2(,R2)               SET ADDR OF FIRST ITEM          19240000
         LR    R0,R14                   SAVE IT                         19250000
         MVC   PARM#MSG(8),=C' PARM='' '                                19260000
         MVC   PARM#MSG+8(L'PARM#MSG-8),PARM#MSG+7   BLANK THE REST     19270000
         CH    R15,=H'21'               PARM TOO LONG FOR "TITLE2"?     19280000
         BNH   *+8                      NO - SKIP                       19290000
         OI    TFLAG2,T2@LPARM          YES - SET FLAG                  19300000
         EX    R15,MVCPARM              SAVE PARM IN OUTBUFF2           19310000
         LA    R1,PARM#MSG+7(R15)       GET ADDR OF END+1               19320000
         MVI   0(R1),C''''              PUT IN ENDING QUOTE             19330000
         OI    TFLAG1,T1@PARM           SET "HAVE PARM" FLAG            19340000
         SPACE 2                                                        19350000
NEXTPARM LM    R1,R3,=A(PARMTAB,12,PARMLAST)                            19360000
TESTPARM LH    R4,0(,R1)                GET LENGTH OF ITEM              19370000
         EX    R4,CLCPARM               THIS IT?                        19380000
         BNE   INCRPARM                 NO - TRY NEXT                   19390000
         OC    PARMFLAG,11(R1)          TURN ON BITS                    19400000
         LA    R14,2(R14,R4)            INCR SCAN PTR                   19410000
         SR    R15,R4                   DECR LENGTH LEFT                19420000
         SH    R15,=H'2'                                                19430000
         BP    NEXTPARM                 LOOP IF ANY MORE                19440000
         B     DONEPARM                 DONE...                         19450000
         SPACE 2                                                        19460000
NULLPARM SH    R15,=H'5'                DECR LENGTH LEFT            U14 19470000
         BNP   DONEPARM                 NOTHING LEFT                U14 19480000
         LA    R14,5(,R14)              POINT TO OPERAND            U14 19490000
         SR    R1,R1                    CLEAR ACCUMULATOR           U14 19500000
         SPACE 1                                                        19510000
NULLLOOP CLI   0(R14),C'0'              DIGIT?                      U14 19520000
         BL    HAVENULL                 NO - HAVE COMPLETE NUMBER   U14 19530000
         IC    R0,0(,R14)               GET A DIGIT                 U14 19540000
         N     R0,F15                   STRIP IT                    U14 19550000
         MH    R1,=H'10'                SHIFT PREVIOUS              U14 19560000
         AR    R1,R0                    ADD NEW                     U14 19570000
         LA    R14,1(,R14)              BUMP SCAN PTR               U14 19580000
         BCT   R15,NULLLOOP             CONTINUE FOR LENGTH OF PARM U14 19590000
         SPACE 2                                                        19600000
HAVENULL LTR   R1,R1                    ANYTHING GIVEN?             U14 19610000
         BNP   *+8                      YES - USE IT                U14 19620000
         STH   R1,NULLNUMB              SET NUMBER                  U14 19630000
         LA    R14,1(,R14)              BUMP SCAN PTR               U14 19640000
         SH    R15,H1                   DECR LENGTH LEFT            U14 19650000
         BP    NEXTPARM                 CONTINUE IF MORE            U14 19660000
         B     DONEPARM                 NONE LEFT                   U14 19670000
         SPACE 2                                                        19680000
INCRPARM BXLE  R1,R2,TESTPARM           KEEP LOOKING                    19690000
*  DIDN'T FIND IT IN TABLE                                              19700000
         CLC   =C'NULL=',0(R14)         SPECIAL KIND?               U14 19710000
         BE    NULLPARM                 YES - PROCESS IT            U14 19720000
*  INVALID PARM ITEM                                                    19730000
         SR    R14,R0                   COMPUTE OFFSET INTO PARM        19740000
         LA    R14,1(,R14)              FIX IT                          19750000
         CVD   R14,DWD                  CONVERT TO PACKED               19760000
         UNPK  PARMERR+28(3),DWD+6(2)   PUT INTO MSG                    19770000
         OI    PARMERR+30,C'0'          FIX SIGN                        19780000
         OI    TFLAG1,T1@PERR           SET "PARM ERROR" FLAG           19790000
         SPACE 3                                                        19800000
DONEPARM LA    LCTR,&LINEPPG            INIT LINECOUNTER      11/88 SBG 19810001
         CALL  TODAY,(TTL1DATE,TTL1DAY,TTL1TIME)              02/93 RT  19850000
         L     R1,16                    -> CVT                          19890000
         L     R1,0(,R1)                -> TCBWORDS                     19900000
         L     R1,4(,R1)                -> TCB                          19910000
         L     R1,12(,R1)               -> TIOT                         19920000
         LA    R1,24(,R1)               -> DD SECTION                   19930000
         ST    R1,DD#PTR                SAVE PTR                        19940000
         SPACE 1                                                        19950000
         TM    PARMFLAG,PF@INLIN+PF@NATTR PARM=INLINE OR NOATTR?        19960000
         BNZ   OK$PRT2                  YES - SKIP SECOND PRINT FILE    19970000
         L     R2,OPENMFL2              GET SYSPRNT2 DCB ADDR           19980000
         ICALL TIOTSCAN,R10,R15=40(,R2) SEE IF WE HAVE IT               19990000
         B     OK$PRT2                  NOPE                            20000000
         SPACE 1                                                        20010000
         OPEN  MF=(E,OPENMFL2)          OPEN SECOND PRINT FILE          20020000
         OI    TFLAG2,T2@PRT2           ASSUME IT WORKS.                20030000
         TM    48(R2),X'10'             DID IT OPEN?                    20040000
         BO    OK$PRT2                  YES                             20050000
         SPACE 2                                                        20060000
ABEND99  LA    R1,99                    GET ABEND CODE                  20070000
         SVC   13                       BYE                             20080000
         SPACE 2                                                        20090000
OK$PRT2  L     R2,OPENMFLI              GET SYSIN DCB ADDR              20100000
         ICALL TIOTSCAN,R10,R15=40(,R2) SEE IF IT'S THERE               20110000
         B     RDJFCB                   NOPE                            20120000
         MVC   RET#ADDR,=A(READCARD)    SET RETURN ADDR                 20130000
         SPACE 2                                                        20140000
         OPEN  MF=(E,OPENMFLI)          OPEN INPUT FILE                 20150000
         TM    48(R2),X'10'             OPEN?                           20160000
         BZ    ABEND99                  NO.  (THIS SHOULD NEVER HAPPEN) 20170000
         MVI   OPENMFL,0                SET TO CLOSE BOTH FILES         20180000
         SPACE 2                                                        20190000
READCARD L     R1,OPENMFLI              GET SYSIN DCB ADDR              20200000
         GET   (1),INBUFF                                               20210000
         OI    TFLAG1,T1@DATA           SET FLAG                        20220000
         CLC   =C'./DDNAME',INBUFF      IS IT THIS?                     20230000
         BE    INDDNAME                 YES - PROCESS                   20240000
         CLC   =C'./VOLUME',INBUFF      IS IT THIS?                     20250000
         BNE   *+10                     NO - MUST BE VOL NAME           20260000
         MVC   INBUFF(6),INBUFF+9       MOVE OVER                       20270000
         MVI   JFCB+117,1               # OF VOL SER #'S                20280000
         MVC   JFCB+118(6),INBUFF       VOL SER #                       20290000
         MVI   JFCB+52,X'40'            VOLUME SERIAL LIST CHANGED      20300000
         B     TAPEINDX                 SKIP THE RDJFCB, GO TO SUBROUTN 20310000
         SPACE 2                                                        20320000
INDDNAME L     R1,TAPEMFL               GET ADDR OF TAPEDCB         U14 20330000
         MVC   40(8,R1),INBUFF+9        MOVE DDNAME INTO DCB        U15 20340000
         SPACE 2                                                        20350000
RDJFCB   RDJFCB  MF=(E,TAPEMFL)                                         20360000
         B     TAPEINDX                 GO TO SUBROUTINE                20370000
         SPACE 3                                                        20380000
EOD2     CLOSE MF=(E,OPENMFL)                                           20390000
         IFP2  N,LEAVE                  SKIP IF NOT OPEN                20400000
         CLOSE MF=(E,OPENMFL2)                                          20410000
         B     LEAVE                    RETURN                          20420000
         SPACE 2                                                        20430000
EOD      TM    TFLAG1,T1@DATA           DATA READ IN?                   20440000
         BO    EOD2                     YES.                            20450000
         MVC   RET#ADDR,=A(EOD2)        SET NEW RETURN ADDR             20460000
         B     RDJFCB                                                   20470000
         SPACE 2                                                        20480000
MVCPARM  MVC   PARM#MSG+7(0),0(R14)     << EXECUTED >>                  20490000
CLCPARM  CLC   2(0,R1),0(R14)           << EXECUTED >>                  20500000
         SPACE 2                                                        20510000
******** PRINT GEN                                                      20520000
         EJECT                                                          20530000
*********************************************************************** 20540000
*                                                                     * 20550000
* T A P E I N D X                                                     * 20560000
*                                                                     * 20570000
*    A ROUTINE WHICH READS IN THE VOLUME NAME & ALL THE DATA SET NAMES* 20580000
*        (FROM THE TRAILER LABELS) OF A TAPE (WITH STANDARD LABELS)   * 20590000
*        AND FORMATS AND PRINTS THE INFORMATION FROM THE LABELS.      * 20600000
*                                                                     * 20610000
*********************************************************************** 20620000
         SPACE 1                                                        20630000
TAPEINDX MVC   LCTR2,=F'0'              INIT 2ND LINE COUNTER 02/93 RT  20640000
         MVC   NUMBNULL,=F'0'           INIT COUNT OF NULL FILES    RT  20650000
         OI    TFLAG1,T1@PAGE1+T1@ANLZ  1ST PAGE/THIS VOL + ANLZ LABEL  20660000
         SPACE 1                                                        20670000
* OPEN TAPE VOLUME FOR LABEL=BLP                                        20680000
         MVC   JFCB+44(8),BLANKS        BLANK OUT ELEM NAME             20690000
         MVC   JFCB+68(2),=H'1'         FILE SEQ #                 WBF  20700000
         MVC   JFCB+70(2),=H'1'         VOL SEQ #                       20710000
         MVC   JFCB+83(3),=X'620000'    SET EXPDT=98000 FOR UCC-1  WBF  20720000
         MVI   JFCB+98,X'40'            DSORG=PS                        20730000
         MVI   JFCB+100,X'90'           RECFM=FB                        20740000
         MVC   TITLE1+5(6),JFCB+118     INIT IN CASE NOT SL             20750000
         SPACE 1                                                        20760000
         OPEN  MF=(E,TAPEMFL),TYPE=J                                    20770000
         TM    TAPEDCB+DCBOFLGS-IHADCB,DCBOFOPN   FILE OPEN ?       THO 20780000
         BO    TPOPENOK                 YES.                        THO 20790000
         NEWPAGE ,                      PRINT THE HEADER                20800000
         PRTLN '-*** TAPE VOLUME COULD NOT BE OPENED ***'               20810000
         B     RETURN                                                   20820000
         SPACE 3                                                        20830000
TPOPENOK NI    TAPEDCB+DCBMACF1-IHADCB,X'F7'  BLK COUNT INACCURATE  THO 20840000
         XC    LEN#TAPE(4*2),LEN#TAPE   CLEAR SL & NL LENGTHS           20850000
         NI    TFLAG1,255-T1@BADEN-T1@SL RESET FLAGS                    20860000
         MVI   WHERE,0                  SET "WHERE ARE WE" FLAG         20870000
         TAPIO TCCW#RWD,TM=*+4     INSURE PROPER POSITION     09/84 DBC 20880000
         XC    FILE#SEQ,FILE#SEQ   CLEAR PHYSICAL FILE#       07/85 DBC 20890000
         TAPIO TCCW#DAT,TM=LEADTPMK     READ IN VOLUME LABEL            20900000
         MVC   VOLLABI(80),TAPEBUFF     SAVE VOLUME LABEL               20910000
         SPACE 1                                                        20920000
DO$SENSE L     R2,LASTSIZE         PRESERVE LENGTH OVER SENSE 09/84 DBC 20930000
         TAPIO TCCW#SNS                 READ SENSE DATA                 20940000
         L     R1,TAPEDCB+DCBDEBAD-IHADCB   LOAD ADDRESS OF DEB     THO 20950000
         AIF   ('&DEBX' EQ 'YES').NEWDEB                            SBG 20951005
         L     R1,DEBSUCBA-DEBBASIC(,R1)    LOAD ADDRESS OF UCB     THO 20960005
         AGO   .OLDDEB                                              SBG 20960105
.NEWDEB  ANOP                                                       SBG 20960205
         L     R1,DEBUCBAD-DEBNMSUB(,R1)    LOAD ADDRESS OF UCB     SBG 20960303
.OLDDEB  ANOP                                                       SBG 20960405
         CLC   UTY3420C(4),UCBTYP-UCB(R1)        3480 COMPAT MODE? ^SBG 20961001
         BE    IS3480                   YEP. TREAT AS 3480.        ^SBG 20962001
         TM    UCBTBYT4-UCB(R1),X'80'       THIS A 3490 TAPE DRIVE ?SBG 20970000
         BO    IS3480
* --------->>> DON'T USE MAPPING MACRO BECAUSE OF OLDER SYSTEMS.    SBG
* ------ CLI   UCBTBYT4-UCB(R1),UCB3480     THIS A 3480 TAPE DRIVE ?THO 20970000
         CLI   UCBTBYT4-UCB(R1),X'80'       THIS A 3480 TAPE DRIVE ?THO 20970000
         BNE   IS3420                   NO, THEN A 3420 DRIVE       THO 20980000
IS3480   LA    R1,C'5'                  THEN INDICATE SO     ^SBG - THO 20990001
         MVC   TITLE1+34(4),=C'CART'    THEN INDICATE CARTRIDGE     THO 21000000
         NI    PARMFLAG,255-PF@DEN1     TURN OFF                    THO 21010000
         B     TRUE$GOT                                             THO 21020000
IS3420   LA    R1,C'3'                  ASSUME 1600 BPI             THO 21030000
         TM    SENSDATA+3,B'00000100'   PE MODE?                        21040000
         BO    TRUE$GOT                 YES                             21050000
         LA    R1,C'4'                  ASSUME 6250 BPI                 21060000
         TM    SENSDATA+9,B'00001000'   CAPABLE OF 6250 BPI?            21070000
         BO    TRUE$GOT                 YES                             21080000
         AIF   (&HERC).TRUE$            FORCE 6250 IF HERCULES 07/02SRS
         LA    R1,C'2'                  ELSE, 800 BPI                   21090000
         SPACE 2                                                        21100000
.TRUE$   ANOP
TRUE$GOT LR    R0,R1                    COPY DENSITY CHAR               21110000
         N     R1,F15                   STRIP IT                        21120000
         SLL   R1,2                     *4                              21130000
         LA    R1,DEN#LIST(R1)          POINT TO 4 CHAR FORM            21140000
         ST    R1,TRUE#DEN              SAVE ADDR                       21150000
         STC   R0,TRUE#DEN              SAVE 1 CHAR FORM                21160000
         SPACE 2                                                        21170000
         CLI   NUMBNULL+1,0             HOW MANY NULL SKIPPED?      U14 21180000
         BNE   NULL1ST                  MORE THAN ZERO.             U14 21190000
         CLC   VOLLABI(4),=C'VOL1VOL1'  VOLUME LABEL?                   21200000
         BE    TPVOL1OK                 YES.                            21210000
         SPACE 3                                                        21220000
         MVC   VOLSERNO(6),JFCB+118     SET HERE FOR OVERPRINTING   U15 21230000
         NEWPAGE ,                      PRINT THE HEADER LINES          21240000
         PRTLN '0++++ NO VOL1 LABEL FOUND.  NL ANALYSIS FOLLOWS ++++'   21250000
         BCTR  LCTR,0                   ACCOUNT FOR EXTRA BLANK LINE    21260000
         PRTLN OUTBUFF                  BLANK LINE                      21270000
         SPACE 1                                                        21280000
NULL1ST  MVC   FL1ID,BLANKS             SET THE DSN                 U14 21290000
         SPACE 3                                                        21300000
NL$NEXT  ST    R2,LASTSIZE         RESTORE SIZE OF FIRST BLOC 09/84 DBC 21310000
         ST    R2,BYTECNT          INIT TOTAL BYTES READ      09/84 DBC 21320000
         ST    R2,MAXBLKSI         INIT BIGGEST BLOCK READ    09/84 DBC 21330000
         MVC   BLOCKCNT,F1              INIT # OF BLOCKS READ           21340000
         MVI   NLFLAGS,NL@F             INIT NL TYPE FLAGS              21350000
         CH    R2,TAPEBUFF              IS FIRST BLOCK RECFM=V?         21360000
         BNE   *+8                      NO - SKIP                       21370000
         OI    NLFLAGS,NL@V             YES - SET FLAG                  21380000
         MVI   WHERE,3                  SET "WHERE ARE WE" FLAG         21390000
         MVC   UNLOADER,UNLOADER-1      CLEAR INDICATOR                 21400000
         LH    R1,FILE#SEQ              GET FILE NUMBER                 21410000
         LA    R1,1(,R1)           ADJ TO IDENTIFY THE NUMBER 07/85 DBC 21420000
*                                  OF THE FILE CURRENTLY      07/85 DBC 21430000
*                                  BEING SCANNED              07/85 DBC 21440000
         CVD   R1,DWD                   -> PACKED                       21450000
         UNPK  DWD(5),DWD+5(3)          -> EBCDIC                       21460000
         OI    DWD+4,C'0'               FIX SIGN                        21470000
         MVC   FL1FILSQ,DWD+1           PUT INTO "LABEL"                21480000
         SPACE 1                                                        21490000
*  NOW SEE IF THIS FILE IS ANYTHING SPECIAL THAT WE KNOW ABOUT.         21500000
***      CLC   =H'800',LASTSIZE         RIGHT SIZE?                     21510000
***      BNE   NL$NMOVE                 NOT IEHMOVE                     21520000
         BA&XA R10,TEST$MV              IS IT IEHMOVE?                  21530000
         B     NL$NMOVE                 NO                              21540000
         B     IEHMOVE2                 YES - 2 PRINT FILE FORMAT       21550000
         B     IEHMOVE1                 YES - 1 PRINT FILE FORMAT       21560000
         SPACE 2                                                        21570000
NL$NMOVE CLC   TAPEBUFF(8),=H'60,0,56,0' RECFM=V,BLK=60,REC=56?         21580000
         BE    NLTSTCPY             YES-IT'S THE OLD IEBCOPY FORMAT+SBG 21590001
         CLC   TAPEBUFF(8),=H'64,0,60,0' IS IT IEBCOPY NEW FORMAT? +SBG 21591001
         BNE   NL$NCOPY                 NO - NOT IEBCOPY                21592001
NLTSTCPY BA&XA R10,TEST$CPY             IS IT VS2COPY?             +SBG 21600001
         B     NL$NCOPY                 NO                              21610000
         B     IEBCOPY2                 YES - 2 PRINT FILE FORMAT       21620000
         B     IEBCOPY1                 YES - 1 PRINT FILE FORMAT       21630000
         SPACE 2                                                        21640000
NL$NCOPY BA&XA R10,TEST$DMP             IS IT IEHDASDR?                 21650000
         B     NL$NDSDR                 NO                              21660000
         NOP   0                        YES - BUT DON'T USE 2ND     WBF 21670000
*                                             PRINT FILE            WBF 21680000
         B     IHDASDR1                                                 21690000
         SPACE 2                                                        21700000
******************************************************************* SBG 21710000
*  ORIGINAL CODE WOULD NOT DETECT AN APAR TAPE AS SUCH.             SBG 21720000
*                                                                   SBG 21730000
*  WHEN READING PTF TAPES WHICH HAVE APARS FIRST, AND               SBG 21740000
*   THE APARS HAVE IEBUPDTE CONTROL CARDS IN THEM FOR SOURCE UPDATE,SBG 21750000
*   WE WANT TAPEMAP TO THINK THAT THE TAPE HAS PTFS OR APARS,       SBG 21760000
*   NOT IEBUPDTE CONTROL STATEMENTS, WHICH ARE SECONDARY            SBG 21770000
*   TO THE FACT THAT WE HAVE AN APAR TAPE.                          SBG 21780000
*                                                                   SBG 21790000
*  THEREFORE I AM MOVING THE CODE WHICH FEELS FOR SMPPTFIN FORMAT   SBG 21800000
*  BEFORE THE CODE WHICH FEELS FOR IEBUPDTE FORMAT, TO DETECT       SBG 21810000
*  AN APAR TAPE WHEN WE HAVE ONE.                                   SBG 21820000
******************************************************************* SBG 21830000
*  NOTE THAT WE DON'T WANT TO TEST FOR SMPPTFIN ON A CBT TAPE.12/88 SBG 21830101
******************************************************************* SBG 21830201
NL$NDSDR DS    0H                                             11/88 SBG 21831001
         BA&XA R10,TEST$CBT             IS IT CBT973 COMPRESS 11/88 SBG 21832001
         B     NL$NCBT                  NO                    11/88 SBG 21833001
         B     CBT2                     YES - 2 PRINT FILE FMT11/88 SBG 21834001
         B     CBT1                     YES - 1 PRINT FILE FMT11/88 SBG 21835001
         SPACE 3                                              11/88 SBG 21836001
NL$NCBT  DS    0H                       LABEL FOR NOT DASDR         SBG 21840001
         SPACE 3                                              07/85 DBC 21850000
         BA&XA R10,TEST$PTF        DOES FILE CONTAIN PTFS?    07/85 DBC 21860000
         B     NL$NPTFS            +0 NO, SKIP                06/86 SBG 21870000
         B     PTFS2               +4 YES, GO DUMP IT         07/85 DBC 21880000
         B     PTFS1               +8 YES, JUST SAY SO        07/85 DBC 21890000
*       NEXT 5 LINES ARE SWITCHED WITH PREV 6 LINES & NAMES CHANGED SBG 21900000
NL$NPTFS DS    0H                                             06/86 SBG 21910001
         BA&XA R10,TEST$UPS             MAYBE IEBUPDTE INPUT STREAM?    21920000
         B     NL$NUPDT                 BOY WILL THIS BE UNRELIABLE SBG 21930000
         B     IEBUPDT2                 YES - 2 PRINT FILE FORMAT       21940000
         B     IEBUPDT1            +8 YES & 1-PRT: HANDLE     07/85 DBC 21950000
NL$NUPDT DS    0H                                             06/86 SBG 21960000
         BA&XA R10,TEST$FDR             IS IT FDR?            07/85 DBC 21980000
         B     NL$NFDR                  NO                              21990000
         B     FDR2                     YES - 2 PRINT FILE FORMAT   WBF 22000000
         B     FDR1                     YES,- 1 PRINT FILE FORMAT   WBF 22010000
         SPACE 2                                                        22020000
NL$NFDR  BA&XA R10,TEST$IS              IS IT IEBISAM?        11/88 SBG 22060301
         B     NL$NCDOC                 DIDN'T THINK SO...              22060401
         B     IEBISAM2                 YES - 2 PRINT FILE FORMAT       22060500
         B     IEBISAM1                 YES - 1 PRINT FILE FORMAT       22060600
         SPACE 3                                                        22070000
NL$NCDOC BA&XA R10,TEST$CH              IS IT CBT DOC FILE?   12/88 SBG 22071001
         B     NL$NIS                   NUH UH.               12/88 SBG 22072001
         B     NL$LOOP                  YES - DISPLAY EXTRCT'D12/88 SBG 22073001
         B     CBTDOC1                   DATA ON REPORT #1    12/88 SBG 22074001
         SPACE 3                                              12/88 SBG 22075001
NL$NIS   BA&XA R10,TEST$SLK             IS IT A SLICK BACKUP?           22080000
         B     NL$NSLK                  DIDN'T THINK SO...              22090000
         B     SLK2                     YES - 2 PRINT FILE FORMAT       22100000
         B     SLK1                     YES - 1 PRINT FILE FORMAT       22110000
         SPACE 3                                                        22120000
NL$NSLK  BA&XA R10,TEST$MAP             IS IT A TAPEMAP PRINT FILE?     22130000
         B     NL$LOOP                  NO                              22140000
         B     MAP2                     YES - 2 PRINT FILE FORMAT       22150000
****     B     NL$LOOP                  YES - 1 PRINT FILE FORMAT       22160000
         SPACE 3                                                        22170000
NL$LOOP  TAPIO TCCW#DAT,TM=NL$EOF       READ A BLOCK                    22180000
         B     NL$LOOP                  DO THAT TILL EOF                22190000
         SPACE 2                                                        22200000
NL$EOF   CLI   UNLOADER,C' '            A SECOND LINE?                  22210000
         BE    *+12                     NO - SKIP                       22220000
         NEWPAGE  2                     MAKE SURE WE HAVE ENOUGH        22230000
         SPACE 1                                                        22240000
NL$SL    DS    0H                                             07/85 DBC 22250000
         LH    R1,FILE#SEQ              GET THE FILE SEQ #              22260000
         CVD   R1,DWD                   PACK IT                         22270000
         MVC   #FILE-2(6),=X'402020202120'                    09/84 DBC 22280000
         ED    #FILE-2(6),DWD+5                                         22290000
         SPACE 1                                              09/84 DBC 22300000
         MVC   #UNLOAD,UNLOADER                                         22310000
************************************************************* 12/88 SBG 22311001
*  IF THIS IS A CBT HEADER, PUT THE DISPLAY INFO INTO       * 12/88 SBG 22312001
*    THE DSNAME SLOT.  VER XXX  MM-DD-YY.                   * 12/88 SBG 22313001
************************************************************* 12/88 SBG 22314001
         SPACE 1                                              12/88 SBG 22315001
         CLC   UNLOADER(8),CBHEADER     WAS IT CBTHEADER?     12/88 SBG 22316001
         BNE   NL$CBHD                  NO. NORMAL PROCESSING 12/88 SBG 22317001
         MVC   #DSN(17),CBHLIT     DISPLAY CBT DOC INFO       12/88 SBG 22318001
         MVC   #DSN+4(4),CBTVERS   PUT IN VERSION NUMBER      12/88 SBG 22319001
         MVC   #DSN+9(8),CBTDATE   PUT IN RELEASE DATE        12/88 SBG 22319101
NL$CBHD  DS    0H                                             12/88 SBG 22319201
         SPACE 3                                                        22320000
         CLC   =C'(NULL)',UNLOADER+2 SL NULL FILE?            07/85 DBC 22330000
         BE    NLSLDONE                 YES - CAN'T DO ANYTHING         22340000
         SPACE 1                                                        22350000
         MVC   #INFOSRC,=C' SCAN ' IDENTIFY INFO SOURCE       06/84 DBC 22360000
         MVI   #RECFM,C'V'              ASSUME RECFM=V                  22370000
         TM    NLFLAGS,NL@V             IS IT?                          22380000
         BO    NL$1                     YES - CONTINUE                  22390000
         MVI   #RECFM,C'F'              ASSUME RECFM=F                  22400000
         TM    NLFLAGS,NL@F             IS IT?                          22410000
         BO    NL$1                     YES - CONTINUE                  22420000
         MVI   #RECFM,C'U'              ANYTHING ELSE IS RECFM=U        22430000
         SPACE 2                                                        22440000
NL$1     L     R0,MAXBLKSI         GET SIZE OF LARGEST BLOCK  09/84 DBC 22450000
         CVD   R0,DWD                                                   22460000
         MVC   #BLKSIZE(5),=X'2020202120'                               22470000
         ED    #BLKSIZE-1(6),DWD+5                                      22480000
         SPACE 1                                                        22490000
*******  MVI   #LRECL+4,C'?'                                            22500000
         SPACE 1                                                        22510000
         L     R0,BLOCKCNT              GET BLOCK COUNT                 22520000
         CVD   R0,DWD                                                   22530000
         MVC   #BLKCNT-1(7),=X'20202020202120'                          22540000
         ED    #BLKCNT-2(8),DWD+4                                       22550000
         SPACE 1                                                        22560000
         MVC   #DEN+1(1),TRUE#DEN                                       22570000
         L     R14,TRUE#DEN                                             22580000
         TM    PARMFLAG,PF@DEN1                                         22590000
         BO    *+10                                                     22600000
         MVC   #DEN,24(R14)                                         THO 22610000
         SPACE 1                                                        22620000
         L     R15,BLOCKCNT             COUNT # OF GAPS                 22630000
         L     R1,BYTECNT                                               22640000
         M     R1-1,=F'100'             SCALE                           22650000
         D     R1-1,0(,R14)             DIVIDE BY BYTES/INCH            22660000
         CLI   TRUE#DEN,C'5'            IS IT A 3480 TAPE DRIVE ?   THO 22670000
         BNE   DEN3420A                 NO, THEN 3420               THO 22680000
         AH    R1,=Y(8)                 ADD 1 TAPEMARK              THO 22690000
         TM    TFLAG1,T1@SL             REALLY SL?                      22700000
         BNO   *+12                     NO - SKIP                       22710000
         AH    R1,=Y(2*8)               YES - 2 MORE TAPEMARKS      THO 22720000
         LA    R15,4(,R15)              INCR # OF GAPS                  22730000
         M     R15-1,=F'08'             EACH GAP IS .08 INCH        THO 22740000
         B     LENCALC1                                             THO 22750000
DEN3420A AH    R1,=Y(375)               ADD 1 TAPEMARK              THO 22760000
         TM    TFLAG1,T1@SL             REALLY SL?                  THO 22770000
         BNO   *+12                     NO - SKIP                   THO 22780000
         AH    R1,=Y(2*375)             YES - 2 MORE TAPEMARKS      THO 22790000
         LA    R15,4(,R15)              INCR # OF GAPS              THO 22800000
         M     R15-1,=F'60'             EACH GAP IS .60 INCH            22810000
         CLI   TRUE#DEN,C'4'            UNLESS IT'S A 6250 TAPE         22820000
         BNE   *+8                      IT ISN'T                        22830000
         SRL   R15,1                    THEN THEY'RE .3 INCH EACH       22840000
LENCALC1 AR    R1,R15                   ADD IN LENGTH OF GAPS       THO 22850000
         LR    R2,R1                    SAVE                            22860000
         ICALL TAPEFEET,R14,R15=#LENGTH-2                     06/84 DBC 22870000
         A     R2,LEN#TAPE+4                                            22880000
         ST    R2,LEN#TAPE+4                                            22890000
         LR    R1,R2                    COPY FOR SUBROUTINE             22900000
         ICALL TAPEFEET,R14,R15=#CUMLEN-2                     06/84 DBC 22910000
         SPACE 2                                                        22920000
         PRTLN OUTBUFF                                                  22930000
         CLI   UNLOADER,C' '            ANY SECOND PRINT LINE?          22940000
         BE    NL$DONE                  NO - SKIP                       22950000
         TM    PARMFLAG,PF@INLIN        PARM=INLINE?                    22960000
         BNO   NL$DONE                  NO - NO SECOND LINE             22970000
         PRTLN OUTBUFF2                                                 22980000
         MVC   OUTBUFF2,OUTCLR2                                         22990000
         SPACE 2                                                        23000000
NL$DONE  TM    TFLAG1,T1@SL             THIS TAPE SL?                   23010000
         BO    NLSLDONE                 YES - DIFFERENT HANDLING        23020000
         SPACE 1                                                        23030000
NL$DONEX TAPIO TCCW#DAT,TM=NULL$NL      READ NEXT FILE'S FIRST BLK  U14 23040000
         L     R2,LASTSIZE         GET SIZE OF LAST BLOCK     09/84 DBC 23050000
         B     NL$NEXT                  PROCESS NEXT FILE               23060000
         SPACE 3                                              07/85 DBC 23070000
NULL$NL  LH    R1,NUMBNULL              GET NUMBER OF NULL FILES    U14 23080000
         LA    R1,1(,R1)                INCR                        U14 23090000
         STH   R1,NUMBNULL              SAVE NEW COUNT              U14 23100000
         CH    R1,NULLNUMB              DONE ENOUGH?                U14 23110000
         BH    HAVE$EOV                 YES - STOP                  U14 23120000
         LH    R1,FILE#SEQ         NO, GET FILE #             07/85 DBC 23130000
         MVC   #FILE-1(5),=X'2020202120'  MOVE IN EDIT MASK         U14 23140000
         CVD   R1,DWD                                               U14 23150000
         ED    #FILE-2(6),DWD+5                                     U14 23160000
         MVC   #UNLOAD+2(6),=C'(NULL)'  PUT IN INDICATOR      06/84 DBC 23170000
         PRTLN OUTBUFF                                              U14 23180000
         L     R0,LEN#TAPE+4            GET ACCUMULATED TAPE LENGTH U14 23190000
         CLI   TRUE#DEN,C'5'            IS IT A 3480 TAPE DRIVE ?   THO 23200000
         BNE   DEN3420B                 NO, THEN 3420               THO 23210000
         AH    R0,=Y(8)                 COUNT 1 TAPEMARK            THO 23220000
         ST    R0,LEN#TAPE+4            SAVE UPDATED                U14 23230000
         B     NL$DONEX                                             U14 23240000
DEN3420B AH    R0,=Y(375)               COUNT 1 TAPEMARK            THO 23250000
         ST    R0,LEN#TAPE+4            SAVE UPDATED                THO 23260000
         B     NL$DONEX                                             THO 23270000
         SPACE 3                                                        23280000
************************************************************* 12/88 SBG 23281001
*   THIS IS THE START OF NORMAL STANDARD LABEL PROCESSING.  * 12/88 SBG 23282001
************************************************************* 12/88 SBG 23283001
TPVOL1OK OI    TFLAG1,T1@SL             INDICATE SL PROCESSING12/88 SBG 23290001
         MVC   TITLE1+5(6),VOLSERNO                                     23300000
         MVC   TITLE1+19(10),VOLOWNER                                   23310000
         MVC   TITLE1+13(6),TITLE1+12   BLANK OUT KEYWORD               23320000
         CLC   VOLOWNER,BLANKS          OWNER ALL BLANK?                23330000
         BE    *+10                                                     23340000
         MVC   TITLE1+13(6),=C'OWNER='                                  23350000
         NI    TFLAG2,255-T2@RQVOL      TURN OFF FLAG                   23360000
         CLC   VOLSERNO(6),JFCB+118     GET WHAT WE ASKED FOR?          23370000
         BE    MATCHVOL                 YES - CONTINUE                  23380000
         OI    TFLAG2,T2@RQVOL          SET FLAG                        23390000
         SPACE 2                                                        23400000
MATCHVOL NEWPAGE ,                      PRINT HEADER LINES              23410000
         NI    TFLAG1,255-T1@ANLZ       RESET FLAG                      23420000
         TM    PARMFLAG,PF@NL      NL PROCESSING FORCED?      06/84 DBC 23430000
         BNZ   ANLZ1               YES, SET ANALYZE SIGNAL    06/84 DBC 23440000
         TM    PARMFLAG,PF@NOCHK   NO, SCAN BYPASSED?         06/84 DBC 23450000
         BNZ   NOANLZ1             YES, SKIP                  06/84 DBC 23460000
ANLZ1    OI    TFLAG1,T1@ANLZ      NO, SET ANALYZE SIGNAL     06/84 DBC 23470000
NOANLZ1  DS    0H                                             06/84 DBC 23480000
         SPACE 1                                                        23490000
         TM    PARMFLAG,PF@NL           FORCE NL ANALYSIS?              23500000
         BNO   *+12                     NO - CONTINUE                   23510000
FORCENL  XI    TFLAG1,T1@SL             YES - TURN OFF FLAG             23520000
         B     NL$NEXT                  AND GO TO NL ROUTINE            23530000
         SPACE 1                                                        23540000
         MVI   WHERE,1                  SET "WHERE ARE WE" FLAG         23550000
         MVC   FL1LABI(4),BLANKS        CLEAR OUT 'HDR1'      02/93 RT
         TAPIO TCCW#LBL,TM=MAYBETT      READ THE HDR LABELS             23560000
         B     DATAREAD                 SKIP FIRST TIME                 23570000
MAYBETT  EQU   *                                               1/93 RT  23531005
         CLC   FL1LABI(4),=C'HDR1'      IS IT SL DOS TAPE?     1/93 RT  23531105
         BNE   FORCENL                                         1/93 RT  23532005
         MVI   FL2LABI,C'?'             CLEAR HDR2 TO '?'      1/93 RT  23533005
         MVC   FL2LABI+1(70),FL2LABI                           1/93 RT  23534007
         CLI   FL1LABI+4,C'0'      POSSIBLY IEHINITT?          1/93 RT  23534106
         BNE   DECRFILE             NO                         1/93 RT  23534206
         CLC   FL1LABI+5(70),FL1LABI+4      IEHINITT?          1/93 RT  23534306
         BE    IEHINITT             YES                        1/93 RT  23534406
DECRFILE LH    R0,FILE#SEQ         DECR TAPEMARK COUNTER       1/93 RT  23534507
         BCTR  R0,0                                            1/93 RT  23534605
         STH   R0,FILE#SEQ         STORE NEW VALUE             1/93 RT  23534705
         TAPIO TCCW#BSF            BACKSPACE A FILE            1/93 RT  23534805
         B     DATAREAD                 SKIP FIRST TIME                 23570000
         SPACE 3                                              07/85 DBC 23580000
NLSLDONE TM    TFLAG1,T1@SL        SL-TAPE?                   07/85 DBC 23590000
         BZ    SPACER              NO, SKIP THIS              07/85 DBC 23600000
         CLC   =C'0001',FL1VOLSQ        FIRST VOLUME OF FILE?           23610000
         BE    TEST$EOV                 YES.                            23620000
         CLC   =C'    ',FL1VOLSQ        MISSING VOL SEQ FLD?  02/93 RT  23610000
         BE    TEST$EOV                 YES.                  02/93 RT  23620000
         MVC   FVOL#MSG+22(4),FL1VOLSQ  VOL SEQ #                       23630000
         MVC   FVOL#MSG+80(6),FL1FILSR  FIRST VOLSER                    23640000
         PRTLN FVOL#MSG                                                 23650000
         SPACE 2                                                        23660000
TEST$EOV CLC   =C'EOV2',FL2LABI                                         23670000
         BNE   NOT$EOV                  EOV.                            23680000
         PRTLN ' *** THE ABOVE FILE IS CONTINUED ON ANOTHER VOLUME'     23690000
         B     HAVE$EOV            ALL DONE                   07/85 DBC 23700000
NOT$EOV  DS    0H                                             07/85 DBC 23710000
         SPACE 3                                              07/85 DBC 23720000
SPACER   PRTLN OUTBUFF                  BLANK LINE                      23730000
         TAPIO TCCW#FSF                 FWD SPACE                       23740000
         MVI   WHERE,1                  SET "WHERE ARE WE" FLAG         23750000
         TAPIO TCCW#LBL,TM=MAYBET2      READ HDR LABELS                 23760000
         B     DATAREAD                                       02/93 RT
         SPACE 1                                                        23770000
MAYBET2  EQU   *                                               1/93 RT  23760006
         CLC   FL1LABI(4),=C'HDR1'      IS IT SL DOS TAPE?     1/93 RT  23761006
         BNE   HAVE$EOV                                        1/93 RT  23762006
         MVI   FL2LABI,C'?'             CLEAR HDR2 TO '?'      1/93 RT  23763006
         MVC   FL2LABI+1(70),FL2LABI                           1/93 RT  23764007
         LH    R0,FILE#SEQ         DECR TAPEMARK COUNTER       1/93 RT  23765007
         BCTR  R0,0                                            1/93 RT  23766006
         STH   R0,FILE#SEQ         STORE NEW VALUE             1/93 RT  23767006
         TAPIO TCCW#BSF            BACKSPACE A FILE            1/93 RT  23768006
         B     DATAREAD                                                 23769006
DATAREAD TAPIO TCCW#FSF                 SKIP TO END OF HDR LABELS       23770000
         MVC   UNLOADER,UNLOADER-1      CLEAR UNLOADED FLAG             23771009
         MVI   NLFLAGS,NL@V+NL@F        ASSUME RECFM=V & RECFM=F        23800000
         MVI   WHERE,3                  SET "WHERE ARE WE" FLAG         23810000
         TAPIO TCCW#DAT,TM=NULLFILE     READ DATA                       23820000
         L     R1,LASTSIZE         GET SIZE OF (FIRST) BLOCK  09/84 DBC 23830000
         ST    R1,MAXBLKSI         INIT BIGGEST BLOCK READ    09/84 DBC 23840000
         AH    R1,=Y(4*80)              2 HDR + 2 EOF LABELS            23850000
         ST    R1,BYTECNT               INIT TOTAL BYTES READ           23860000
         MVC   BLOCKCNT,F1              INIT NUMBER OF BLOCKS READ      23870000
         NI    NLFLAGS,255-NL@CHANG     FIRST BLOCK NOT A CHANGE        23880000
         OI    NLFLAGS,NL@F             STILL COULD BE RECFM=F          23890000
         SPACE 2                                                        23901001
         BA&XA R10,TEST$CH              TEST FOR CBT HEADER   10/90 SBG 23902001
         B     TSTRECFM                 NOT CBTHEADER, GO ON  10/90 SBG 23903001
         NOP   0                        NO EFFECT ON REPORT 2 10/90 SBG 23904001
         B     TEST$LCT                 PROCESS HEADER FILE   10/90 SBG 23905001
         SPACE 2                                              10/90 SBG 23906001
TSTRECFM CLI   FL2RECFM,C'F'            RECFM=FB?             10/90 SBG 23910001
         BNE   NOT$RF$F                 NO - NOT IEHMOVE OR IEBISAM     23920000
         CLC   FL2LRECL,C00080          LRECL=80?                       23930000
         BNE   FIND$EOF                 NO - NOT IEHMOVE OR IEBISAM     23940000
***      CLC   FL2BLKL,C00800           BLKSIZE=800?                    23950000
***      BNE   TST$UPDT                 NO - NOT IEHMOVE'D              23960000
         CLI   FL2BLKA,C'B'             BLOCKED?                        23960100
*  SWITCH TESTS FOR SMPPTFIN AND IEBUPDTE                           SBG 23960200
         BNE   TST$UPDT                 NO - NOT IEHMOVE'D     2/93 RT  23960300
         BA&XA R10,TEST$MV              IS IT IEHMOVE?                  23960400
         B     TST$UPDT                 NO - TRY NEXT               SBG 23960500
         B     IEHMOVE2                 YES - 2 PRINT FILE FORMAT       23960600
         B     TEST$LCT                 YES - 1 PRINT FILE FORMAT       23960700
         SPACE 3                                                        23960800
************************************************************* 06/86 SBG 23960900
*  ORIGINAL CODE WOULD NOT DETECT AN APAR TAPE AS SUCH.             SBG 23961000
*                                                                   SBG 23961100
*  WHEN READING PTF TAPES WHICH HAVE APARS FIRST, AND               SBG 23961200
*   THE APARS HAVE IEBUPDTE CONTROL CARDS IN THEM FOR SOURCE UPDATE,SBG 23961300
*   WE WANT TAPEMAP TO THINK THAT THE TAPE HAS PTFS OR APARS,       SBG 23961400
*   NOT IEBUPDTE CONTROL STATEMENTS, WHICH ARE SECONDARY            SBG 23961500
*   TO THE FACT THAT WE HAVE AN APAR TAPE.                          SBG 23961600
*                                                                   SBG 23961700
*  THEREFORE I AM MOVING THE CODE WHICH FEELS FOR SMPPTFIN FORMAT   SBG 23961800
*  BEFORE THE CODE WHICH FEELS FOR IEBUPDTE FORMAT, TO DETECT       SBG 23961900
*  AN APAR TAPE WHEN WE HAVE ONE.                                   SBG 23962000
************************************************************* 06/86 SBG 23962100
         SPACE 3                                              07/85 DBC 23962200
TST$UPDT DS    0H                LABEL FOR TEST IEBUPDTE            SBG 23962300
*                          BUT TEST FOR PTFIN COMES FIRST, SO       SBG 23962400
*  I SWITCHED DAVE'S CODE AROUND BETWEEN THESE 3 LABELS.            SBG 23962500
         BA&XA R10,TEST$PTF        DOES FILE CONTAIN PTFS?    07/85 DBC 23962600
         B     TST$NPTF            +0 NO, SKIP                07/85 DBC 23962700
         B     PTFS2               +4 YES & 2-PRT FMT; HANDLE 07/85 DBC 23962800
         B     TEST$LCT            +8 YES & 1-PRT FMT; HANDLE 07/85 DBC 23962900
TST$NPTF DS    0H                                             07/85 DBC 23963000
         BA&XA R10,TEST$UPS             SEE IF IEBUPDTE INPUT STREAM    23963100
         B     TST$NUPD            +0 PROBABLY NOT            07/85 DBC 23963200
         B     IEBUPDT2                 YES - 2 PRINT FILE FORMAT       23963300
         B     TEST$LCT            +8 YES, JUST SAY SO        07/85 DBC 23963400
TST$NUPD DS    0H                                             07/85 DBC 23963500
*  I SWITCHED DAVE'S CODE AROUND BETWEEN THESE 3 LABELS.            SBG 23963600
         SPACE 3                                                        23963700
TST$ISAM BA&XA R10,TEST$IS              IS IT IEBISAM?                  24160000
         B     TST$MAP                  NO                    11/88 SBG 24170000
         B     IEBISAM2                 YES - 2 PRINT FILE FORMAT       24180000
         B     TEST$LCT                 YES - 1 PRINT FILE FORMAT       24190000
         SPACE 3                                                        24200000
TST$MAP  BA&XA R10,TEST$MAP             IS IT A TAPEMAP PRINT FILE?     24210000
         B     FIND$EOF                 NO                              24220000
         B     MAP2                     YES - 2 PRINT FILE FORMAT       24230000
         B     TEST$LCT                 YES - 1 PRINT FILE FORMAT       24240000
         SPACE 3                                                        24250000
NOT$RF$F CLC   =X'CA6D0F',TAPEBUFF+9    TEST IEBCOPY FIRST    10/05 SBG 24251001
         BE    FORGTLBL         IEBCOPY-IGNORE WHAT'S IN LABEL08/90 SBG 24252001
*        CLC   =X'01CA6D0F',TAPEBUFF+8  TEST IEBCOPY FOR PDSE 06/93 RT  24251001
*        BE    FORGTLBL         IEBCOPY-IGNORE WHAT'S IN LABEL06/93 RT  24252001
         CLI   FL2RECFM,C'V'            RECFM=V?              08/90 SBG 24260001
         BNE   NOT$RF$V                 NO - NOT IEBCOPY'D              24270000
         CLI   FL2BLKA,C'S'             RECFM=VS?                       24271001
         BNE   TST$CBT                  NOT VS. MAYBE CBT973. 11/88 SBG 24272001
FORGTLBL BA&XA R10,TEST$CPY             IS IT VS2COPY?        08/90 SBG 24273001
         B     TST$CBT                  NO - MAYBE CBT                  24274001
         B     IEBCOPY2                 YES - 2 PRINT FILE FORMAT       24275001
         B     TEST$LCT                 YES - 1 PRINT FILE FORMAT       24276001
         SPACE 3                                                        24277001
TST$CBT  BA&XA R10,TEST$CBT             IS IT CBT973 COMPRESS 11/88 SBG 24280000
         B     FIND$EOF                 NO                    11/88 SBG 24290001
         B     CBT2                     YES - 2 PRINTFILE FMT 11/88 SBG 24300000
         B     TEST$LCT                 YES - 1 PRINTFILE FMT 11/88 SBG 24310001
         SPACE 3                                              11/88 SBG 24310100
NOT$RF$V CLI   FL2RECFM,C'U'            RECFM=U?                        24350000
         BNE   FIND$EOF                 NO - NOT IEHDASDR               24360000
         BA&XA R10,TEST$DMP             IS IT IEHDASDR?                 24370000
         B     TST$FDR                  NO                              24380000
         NOP   0                        DON'T USE 2ND PRINT FILE    WBF 24390000
         B     TEST$LCT                 CONTINUE                        24400000
         SPACE 3                                                        24410000
TST$FDR  BA&XA R10,TEST$FDR             IS IT FDR?                      24420000
         B     TST$SLK                  NO                              24430000
         B     FDR2                     YES - 2 PRINT FILE FORMAT   WBF 24440000
         B     TEST$LCT                 YES - 1 PRINT FILE FORMAT   WBF 24450000
         SPACE 3                                                        24460000
TST$SLK  BA&XA R10,TEST$SLK             IS IT IEBISAM?                  24470000
         B     FIND$EOF                 NO                              24480000
         B     SLK2                     YES - 2 PRINT FILE FORMAT       24490000
         B     TEST$LCT                 YES - 1 PRINT FILE FORMAT       24500000
         SPACE 2                                                        24510000
TEST$LCT TM    PARMFLAG,PF@INLIN        PARM=INLINE?                    24520000
         BNO   FIND$EOF                 NO - SKIP THIS                  24530000
         NEWPAGE  2                     MORE THAN 1 LINE LEFT?          24540000
         SPACE 1                                                        24550000
         CLC   IEBCOPY,UNLOADER         UNLOADED BY IEBCOPY?            24560000
         BE    IEBCOPY1                 YES - PRINT EXTRA INFO          24570000
         CLC   IEHMOVE,UNLOADER         UNLOADED BY IEHMOVE?            24580000
         BE    IEHMOVE1                 YES - PRINT EXTRA INFO          24590000
         CLC   IEBISAM,UNLOADER         UNLOADED BY IEBISAM?            24600000
         BE    IEBISAM1                 YES - PRINT EXTRA INFO          24610000
         CLC   IEHDASDR,UNLOADER        DUMPED BY IEHDASDR?             24620000
         BE    IHDASDR1                 YES - PRINT EXTRA INFO          24630000
         CLC   FDR,UNLOADER             DUMPED BY FDR?                  24640000
         BE    FDR1                     YES - PRINT EXTRA INFO          24650000
         CLC   SLICKMNG,UNLOADER        UNLOADED BY SLICK?              24660000
         BE    SLK1                     YES - PRINT EXTRA INFO          24670000
         CLC   CBT973,UNLOADER          CBT973 COMPRESS FORMAT11/88 SBG 24680000
         BE    CBT1                     YES - PRINT EXTRA INFO11/88 SBG 24680100
         CLC   CBTEMPTY,UNLOADER        CBT TAPE EMPTY FILE?  11/88 SBG 24680201
         BE    CBTEMP1                  YES - PRINT EXTRA INFO11/88 SBG 24680301
         CLC   CBHEADER,UNLOADER        CBT TAPE DOC FILE?    12/88 SBG 24680401
         BE    CBTDOC1                  YES - PRINT DESCRPTN  12/88 SBG 24680501
         CLC   SMPPTFIN,UNLOADER   SYSMODS?                   07/85 DBC 24680600
         BE    PTFS1               YES, GO SAY SO             07/85 DBC 24690000
         CLC   IEBUPXXX,UNLOADER   NO, UPDATE STREAM?         07/85 DBC 24700000
         BE    IEBUPDT1            YES, GO SAY SO             07/85 DBC 24710000
****     CLC   CTAPEMAP,UNLOADER        CREATED FOR TAPEMAP?            24720000
****     BE    MAP1                     DOESN'T MATTER - MORE THAN      24730000
****                                    ONE LINE OF INFO, SO CAN'T      24740000
****                                    PRINT IT INLINE                 24750000
         SPACE 3                                                        24760000
FIND$EOF TM    PARMFLAG,PF@NOCHK        CHECKING SL TAPE?     06/84 DBC 24770000
         BZ    SCAN$EOF                 YES - READ TILL EOF   06/84 DBC 24780000
         SPACE 1                                                        24790000
         TAPIO TCCW#FSF                 FSF PAST REST OF DATA           24800000
         SPACE 2                                                        24810000
READ$EOF MVI   WHERE,2                  SET "WHERE ARE WE" FLAG         24820000
         MVC   FL1LABI(4),BLANKS                             02/93 RT
         TAPIO TCCW#EOV,TM=MISS$EOF     READ TRAILER LABEL              24830000
TEST$EOF CLC   =C'EOF1',FL1LABI         TRAILER LABEL?                  24840000
         BE    HAVE$EOF                 YES - OK                        24850000
         CLC   =C'EOV1',FL1LABI         OTHER KIND?                     24860000
         BE    HAVE$EOF                 YES - OK                        24870000
         SPACE 1                                                        24880000
MISS$EOF PRTLN '0EOF OR EOV LABEL NOT FOUND WHERE EXPECTED'             24890000
         B     REWINDIT                 REWIND AND LEAVE    %%%%%%%%%   24900000
         SPACE 2                                                        24910000
SCAN$EOF TAPIO TCCW#DAT,TM=SL$SAVE      READ & TALLY A DATA BLOCK       24920000
         B     SCAN$EOF                 DO THAT TILL EOF                24930000
         SPACE 2                                                        24940000
SL$SAVE  MVC   NL#SAVE(16),MAXBLKSI SAVE ALL INFO            09/84 DBC  24950000
         MVC   NL#FLAGX,NLFLAGS         MORE...                         24960000
         B     READ$EOF                 GO READ THE EOF LABELS          24970000
         SPACE 2                                                        24980000
HAVE$EOF LA    R1,1                     ASSUME 1-LINE GROUPS            24990000
         TM    PARMFLAG,PF@NOCHK        CHECKING?             06/84 DBC 25000000
         BNZ   *+8                      NO - SKIP             06/84 DBC 25010000
         LA    R1,2(,R1)                YES - NEED 2 MORE EACH          25020001
         CLI   UNLOADER,C' '            IS THIS FILE UNLOADED?          25030000
         BE    *+8                      NO - SKIP                       25040000
         LA    R1,1(,R1)                YES - NEED 1 MORE LINE          25050000
         TM    PARMFLAG,PF@INLIN        INLINE INFO?          12/88 SBG 25051001
         BZ    *+8                      NO - SKIP             12/88 SBG 25052001
         LA    R1,1(,R1)                YES - NEED EXTRA LINE 12/88 SBG 25053001
         LR    R0,R1                    COPY FOR STUPID MACRO           25060000
         NEWPAGE  (R0)                  CHECK FOR ENOUGH LINES          25070000
         SPACE 2                                                        25080000
         TM    PARMFLAG,PF@NOCHK   SL TAPE WITH NL SCANNING?  07/85 DBC 25090000
         BZ    BYWHOZ              YES, DEFER NAMING THE      07/85 DBC 25100000
*                                  UNLOADER UNTIL I BUILD THE 07/85 DBC 25110000
*                                  "SCAN" LINE MESSAGE.       07/85 DBC 25120000
         MVC   #UNLOAD,UNLOADER         SAY WHO UNLOADED IT             25130000
BYWHOZ   DS    0H                                             07/85 DBC 25140000
         MVC   #FILE(4),FL1FILSQ        FILE #                          25150000
         ICALL DEZERO,R14,R1=#FILE      REMOVE LEADING ZEROES           25160000
         MVC   #DSN(17),FL1ID           DSN                             25170000
         CLI   FL1FSEC,C'0'             PASSWORD PROTECTED?             25180000
         BE    NOTPSWD                  NO - SKIP                       25190000
         CLI   FL1FSEC,C'1'             PASSWORD PROTECTED?             25200000
         BE    YESPSWD                  YES - MOVE IT IN                25210000
         CLI   FL1FSEC,C'3'             NOPWREAD?                       25220000
         BNE   WHATPSWD                 NO - WHAT THEN????              25230000
         MVC   #PSWD,=C' WRT'           YES                   06/84 DBC 25240000
         B     NOTPSWD                  CONTINUE                        25250000
WHATPSWD MVC   #PSWD,=C' ???'           SAY WHAT?             06/84 DBC 25260000
         B     NOTPSWD                                                  25270000
YESPSWD  MVC   #PSWD,=C' YES'           SAY SO.               06/84 DBC 25280000
         SPACE 3                                                        25290000
NOTPSWD  MVC   #CDATE(2),FL1CREDT+1     YEAR CREATED                    25300000
         MVI   #CDATE+2,C'.'                                            25310000
         MVC   #CDATE+3(3),FL1CREDT+3   DAY CREATED                     25320000
         MVC   #EDATE(2),FL1EXPDT+1     YEAR EXPIRES                    25330000
         MVI   #EDATE+2,C'.'                                            25340000
         MVC   #EDATE+3(3),FL1EXPDT+3   DAY EXPIRES                     25350000
         SPACE 3                                              06/84 DBC 25360000
         MVC   #INFOSRC,=C'LABELS' SHOW INFO SOURCE           06/84 DBC 25370000
         MVC   #BLKCNT(6),FL1BLKCT      BLOCK COUNT                     25380000
         ICALL DEZERO,R14,R1=#BLKCNT    REMOVE LEADING ZEROES           25390000
         SPACE 2                                                        25400000
         MVC   #RECFM(1),FL2RECFM       RECFM                           25410000
         MVC   #RECFM+1(1),FL2BLKA      BLOCK ATTRIBUTE                 25420000
         MVC   #RECFM+2(1),FL2CNTRL     CONTROL CHARACTER               25430000
         CLI   #RECFM+1,C'R'            WEIRDO RECFM?  (FR OR VR)       25440000
         BNE   NOT$RF$R                 NOT RECFM=FR OR VR              25450000
         MVC   #RECFM+3(1),#RECFM+2     MOVE CTL CHAR OVER              25460000
         MVC   #RECFM+1(2),=C'BS'       IT'S REALLY FBS OR VBS          25470000
NOT$RF$R MVC   #BLKSIZE(5),FL2BLKL      BLOCK LENGTH                    25480000
         ICALL DEZERO,R14,R1=#BLKSIZE   REMOVE LEADING ZEROES           25490000
         MVC   #LRECL(5),FL2LRECL       RECORD LENGTH                   25500000
         ICALL DEZERO,R14,R1=#LRECL     REMOVE LEADING ZEROES           25510000
         CLI   FL2DEN,C'?'              WAS A "DOS SL" TAPE FOUND?  JCH 25512000
         BNE   *+10                     NO -> HDR2 VALUE IS VALID   JCH 25514000
         MVC   FL2DEN(1),TRUE#DEN       YES-> USE ACTUAL AS VALUE   JCH 25516000
         CLI   TRUE#DEN,C'5'            3480 CARTRIDGE ?            THO 25520000
         BNE   *+8                      NO THEN BYPASS MOD          THO 25530000
         MVI   FL2DEN,C'5'              MODIFY WITH 3480 DEN        THO 25540000
         IC    R15,FL2DEN               GET TAPE DENSITY                25550000
         STC   R15,#DEN+1               SET IN PRINT LINE               25560000
         CLC   TRUE#DEN(1),FL2DEN       LABEL CORRECT?                  25570000
         BE    *+8                      YES - OK                        25580000
         OI    TFLAG1,T1@BADEN          SET BAD DENSITY FLAG            25590000
         N     R15,F15                  JUST DIGIT                      25600000
         SLL   R15,2                    MULTIPLY BY 4                   25610000
         LA    R3,DEN#LIST(R15)         POINT TO DENSITY VALUE          25620000
         TM    PARMFLAG,PF@DEN1         PARM=DEN1?                      25630000
         BO    DEN$1                    YES - LEAVE 1 CHAR DEN          25640000
         CLI   FL2DEN,C'5'              TOO HIGH?                   THO 25650000
         BH    DEN$1                    YES - SKIP                      25660000
         CLI   FL2DEN,C'0'              TOO LOW?                        25670000
         BL    DEN$1                    YES - SKIP                      25680000
         MVC   #DEN,24(R3)              MOVE CHAR FORM OF DENSITY   THO 25690000
         SPACE 1                                                        25700000
DEN$1    MVC   #TRTCH+1(2),FL2TRTCH     TAPE RECORDING TECHNI 06/84 DBC 25710000
         SPACE 1                                              06/84 DBC 25720000
         MVC   #CREATOR(8),FL2JOBD GET CREATING JOBNAME       06/84 DBC 25730000
         CLI   FL2STEPD,C' '       IS THERE A STEPNAME?       06/84 DBC 25740000
         BE    GOTCREAT            NO, SKIP                   06/84 DBC 25750000
         MVI   #CREATOR-1,C'X'     YES, SET SCAN STOPPER      06/84 DBC 25760000
         LA    R1,#CREATOR+8       LOAD BACK-SCANNER          06/84 DBC 25770000
         BA&XA.R R14,0             LOAD LOOP HEAD             06/84 DBC 25780000
         BCTR  R1,0                BACK SCAN                  06/84 DBC 25790000
         CLI   0(R1),C' '          EO-JOBNAME YET?            06/84 DBC 25800000
         BER   R14                 NO, CONTINUE BACKSCAN      06/84 DBC 25810000
         MVI   1(R1),C'/'          YES, INSERT SEPARATOR      06/84 DBC 25820000
         MVC   2(8,R1),FL2STEPD    GET STEPNAME               06/84 DBC 25830000
         MVI   #CREATOR-1,C' '     CLEAR STOPPER              06/84 DBC 25840000
GOTCREAT DS    0H                                             06/84 DBC 25850000
         SPACE 2                                                        25860000
*    *****  COMPUTE THE LENGTH (INCHES) OF THIS TAPE DATA SET.          25870000
         SPACE 1                                              06/84 DBC 25880000
         TM    PARMFLAG,PF@NOCHK   DON'T DO IT IF A SCAN IS - 06/84 DBC 25890000
         BZ    SKIPGUES             BEING DONE ANYWAY         06/84 DBC 25900000
         SPACE 1                                                        25910000
         PACK  DWD,FL1BLKCT(6)          BLOCK COUNT                     25920000
         CVB   R15,DWD                  SAVE BLOCK COUNT                25930000
         LA    R1,4(,R15)               ADD 4 GAPS TO BLOCK COUNT.      25940000
         L     R3,TRUE#DEN              GET PTR TO TRUE DENSITY         25950000
         CLI   TRUE#DEN,C'5'            IS IT A 3480 TAPE DRIVE ?   THO 25960000
         BNE   *+12                     NO, THEN 3420               THO 25970000
         M     R1-1,=F'08'              EACH GAP IS .08 INCH        THO 25980000
         B     *+20                                                 THO 25990000
         M     R1-1,=F'60'              EACH GAP IS 6/10 INCH           26000000
*  NOTE - LENGTH KEPT IN .01 INCHES                                     26010000
         CLI   TRUE#DEN,C'4'            6250 BPI?                       26020000
         BNE   *+8                      NO - SKIP                       26030000
         SRL   R1,1                     GAPS AT 6250 ARE .3 INCH        26040000
         LR    R2,R1                    SAVE LENGTH OF GAPS             26050000
         PACK  DWD,FL2BLKL(5)           BLKSIZE.                        26060000
         CVB   R1,DWD                                                   26070000
         MR    R1-1,R15                 BLKSIZE * BLOCK_COUNT           26080000
         AH    R1,=Y(4*80)              2 HDR & 2 EOF LABELS            26090000
         M     R1-1,=F'100'             CONVERT TO .01                  26100000
         D     R1-1,0(,R3)              DIVIDE BY DENSITY.              26110000
         CLI   TRUE#DEN,C'5'            IS IT A 3480 TAPE DRIVE ?   THO 26120000
         BNE   *+12                     NO, THEN 3420               THO 26130000
         AH    R1,=Y(8*3)               3 TAPEMARKS @ 0.08 INCH     THO 26140000
         B     *+8                                                  THO 26150000
         AH    R1,=Y(375*3)             3 TAPEMARKS @ 3.75 INCH         26160000
         AR    R2,R1                    ADD TO LENGTH OF DATA           26170000
         LR    R1,R2                    COPY FOR SUBROUTINE             26180000
         ICALL TAPEFEET,R14,R15=#LENGTH-2 FORMAT FILE LENGTH  06/84 DBC 26190000
         A     R2,LEN#TAPE              ADD TO TOTAL LENGTH             26200000
         ST    R2,LEN#TAPE              SAVE NEW TOTAL LENGTH           26210000
         LR    R1,R2                    COPY FOR SUBROUTINE             26220000
         ICALL TAPEFEET,R14,R15=#CUMLEN-2 FORMAT CUMULATIVE L 06/84 DBC 26230000
SKIPGUES DS    0H                                             06/84 DBC 26240000
         SPACE 1                                                        26250000
         PRTLN OUTBUFF                                                  26260000
         SPACE 2                                                        26270000
         TM    PARMFLAG,PF@NOCHK        SL WITH NL ANALYSIS?  06/84 DBC 26280000
         BNZ   NO$CHECK                 NO - SEE IF SECOND LI 06/84 DBC 26290000
         MVC   MAXBLKSI(16),NL#SAVE RESTORE INFO              09/84 DBC 26300000
         MVC   NLFLAGS,NL#FLAGX         MORE...                         26310000
         B     NL$SL                    YES - GO DO IT                  26320000
         SPACE 2                                                        26330000
NO$CHECK TM    PARMFLAG,PF@INLIN        INLINE MESSAGES?                26340000
         BNO   NLSLDONE                 NO - DO NEXT FILE               26350000
         CLI   UNLOADER,C' '            UNLOADED?                       26360000
         BE    NLSLDONE                 NO - DO NEXT FILE               26370000
         PRTLN OUTBUFF2                 PRINT TRUE ATTRS OF UNLOADED DS 26380000
         MVC   OUTBUFF2,OUTCLR2         CLEAR OTHER PRINT LINE          26390000
         B     NLSLDONE                 DO NEXT FILE                    26400000
         SPACE 3                                                        26410000
HAVE$EOV PRTLN '-     *** EOV ***'                                      26420000
         CLC   LCTR2,=F'0'              ANYTHING PRINTED ON OTHER PAGE? 26430000
         BZ    NO$EOV2                  NO - SKIP                       26440000
         MVC   OUTBUFF2(17),=C'-     *** EOV ***'                       26450000
         PRTLN2                         FLAG END OF OTHER PRINT FILE    26460000
         SPACE 1                                                        26470000
NO$EOV2  TM    TFLAG1,T1@BADEN          ANY INCORRECT DENSITIES?        26480000
         BNO   NO$BADEN                 NO - SKIP                       26490000
         LA    R0,9                     ASSUME NOT PARM=NONOTE          26500000
         TM    PARMFLAG,PF@NONOT        WANT THE NOTE?                  26510000
         BNO   *+8                      YES - SO USE 9                  26520000
         LA    R0,6                     PARM=NONOTE, SO USE 6           26530000
         NEWPAGE  (R0)                  GO TO TOP OF PAGE IF NEEDED     26540000
         LA    LCTR,1(,LCTR)            WILL CALL PRTLN 1 EXTRA TIME    26550000
         MVC   OUTBUFF(113),INCORLAB    REPL LITERAL BY FIELD 10/90 SBG 26560001
*                                       FOR ADDRESSABILITY    10/90 SBG 26570001
         L     R2,TRUE#DEN              GET PTR TO 4 CHAR TRUE DEN      26590000
         CLI   TRUE#DEN,C'2'            800 BPI?                        26600000
         BNE   *+6                      NO - OK                         26610000
         BCTR  R2,0                     RIGHT JUSTIFY THE "800"         26620000
         MVC   OUTBUFF+98(4),20(R2)     MOVE INTO PRINT LINE            26630000
         PRTLN OUTBUFF                                                  26640000
         MVI   OUTBUFF,C'+'             SET FOR OVERPRINT               26650000
         MVC   OUTBUFF+98(4),20(R2)     LIGHT UP THE DENSITY            26660000
         LA    R0,OUTBUFF               POINT TO OUTPUT LINE            26670000
         BA&XA R14,PUTPRTLN             PRINT IT ONCE (NO CLEAR OUTBUFF 26680000
         PRTLN OUTBUFF                  AND THE THIRD TIME              26690000
         SPACE 2                                                        26700000
NO$BADEN TM    PARMFLAG,PF@NONOT        WANT THE NOTE?                  26710000
         BO    REWINDIT                 NO - SO DON'T                   26720000
         NEWPAGE  4                     ENOUGH LINES LEFT ON PAGE?      26730000
         PRTLN '0NOTE:  LENGTH(S) ARE COMPUTED, (BASED ON BLKSIZE, BLKC$26740000
               OUNT, AND DENSITY), AND ARE THEREFORE ONLY APPROXIMATE.' 26750000
         TM    TFLAG1,T1@ANLZ           ANALYZING?                      26760000
         BO    REWINDIT                 YES - GOOD LENGTHS PRINTED      26770000
         PRTLN '        LENGTHS FOR RECFMS OTHER THAN F,FB,ETC MAY NOT $26780000
               BE VERY CLOSE TO THE TRUE LENGTH.'                       26790000
         SPACE 2                                                        26800000
REWINDIT TAPIO TCCW#RWD                 REWIND THE TAPE                 26810000
         SPACE 2                                                        26820000
CLOSE$TP CLOSE MF=(E,TAPEMFL)                                           26830000
         SPACE 2                                                        26840000
RETURN   L     R14,RET#ADDR             GET RETURN ADDR                 26850000
         BR    R14                                                      26860000
         SPACE 2                                                        26870000
NULLFILE MVC   UNLOADER+2(6),=C'(NULL)' PUT IN INDICATOR      06/84 DBC 26880000
         TAPIO TCCW#LBL   %%%%%         READ EOF'S                %%%%% 26890000
         B     TEST$EOF   %%%%%         TEST FOR 'EOF'            %%%%% 26900000
         SPACE 2                                                        26910000
UNEXTPMK PRTLN '0*** UNEXPECTED TAPE MARK ***'                          26920000
         B     REWINDIT                 CLOSE UP & LEAVE                26930000
         SPACE 2                                                        26940000
LEADTPMK NEWPAGE  ,                     PRINT THE HEADER LINES          26950000
         PRTLN '0++++ TAPE HAS LEADING TAPE MARK ++++'                  26960000
         BCTR  LCTR,0                   ACCOUNT FOR EXTRA BLANK LINE    26970000
         PRTLN OUTBUFF                  BLANK LINE                      26980000
         MVI   VOLLABI,C'X'             INSURE NL ROUTINE               26990000
         MVI   NUMBNULL+1,1             INIT NUMBER OF NULL FILES   U14 27000000
         TAPIO TCCW#DAT,TM=LEAD$EOV     READ FIRST BLOCK FROM FILE  U14 27010000
         B     DO$SENSE                 DO NL ANALYSIS                  27020000
         SPACE 2                                                        27030000
LEAD$EOV MVI   NUMBNULL+1,2             SET NUMBER OF NULL FILES    U14 27040000
         CLI   NULLNUMB+1,2             TOO MANY?                   U14 27050000
         BL    HAVE$EOV                 YES - STOP                  U14 27060000
         MVC   #UNLOAD+2(6),=C'(NULL)'  FLAG IT               06/84 DBC 27070000
         MVI   #FILE+3,C'1'             SET FOR PRINT               U14 27080000
         PRTLN OUTBUFF                                              U14 27090000
         B     DO$SENSE                                             U14 27100000
         SPACE 2                                                        27110000
IEHINITT PRTLN '-++++ TAPE HAS BEEN RE-LABELLED WITH IEHINITT ++++'     27120000
         B     REWINDIT                 ...                             27130000
         EJECT                                                          27140000
TEST$MV  CLC   =C'UNLOADED',TAPEBUFF+16 IEHMOVE'S TRADEMARK?            27150000
         BNER  R10                      NO                              27160000
         MVC   UNLOADER(7),IEHMOVE      SAY WHO                         27170000
         SPACE 1                                                        27180000
TEST$RET IFP2  Y,4(,R10)                RETURN POINT FOR 2 PRINT FILES  27190000
         B     8(,R10)                  RETURN POINT FOR 1 PRINT FILE   27200000
         SPACE 2                                                        27210000
************************************************************  11/88 SBG 27220000
* THE CBT973 COMPRESS PROGRAM MAY BE FOUND IN SOURCE FORM  *  11/88 SBG 27230000
* ON THE CBT MODS TAPE, FILE (2,NL), RECFM=FB,LRECL=80,    *  11/88 SBG 27240000
* BLKSIZE=32720.  IT TAKES 80-BYTE FB RECORDS AS INPUT,    *  11/88 SBG 27250000
* AND PRODUCES 94-BYTE LRECL, VB RECORDS AS OUTPUT.        *  11/88 SBG 27260000
*                                                          *  11/88 SBG 27270000
* DATA COMPRESSION TAKES THE FORM OF SQUEEZING OUT BLANKS. *  11/88 SBG 27280000
* A 10-BYTE BIT MAP HEADER TELLS THE PROGRAM WHICH OF THE  *  11/88 SBG 27290000
* 80 CHARACTERS WERE BLANKS, AND WHICH WERE NOT.  A RDW    *  11/88 SBG 27300000
* PRECEDES THE 10-BYTE BIT MAP, FOLLOWED BY THE NON-BLANK  *  11/88 SBG 27310000
* BYTES OF THE RECORD.  THE PROGRAM COUNTS ON A LOT OF     *  11/88 SBG 27320000
* BLANK CHARACTERS IN THE RECORD, TO ALLOW COMPRESSION     *  11/88 SBG 27330000
* TO BE ACHIEVED.                                          *  11/88 SBG 27340000
*                                                          *  11/88 SBG 27350000
* IT IS THE PRACTICE ON THE CBT TAPE TO IEBUPDTE-UNLOAD    *  11/88 SBG 27360000
* A SOURCE-FORMAT PDS TO A SEQUENTIAL FILE, AND THEN TO    *  11/88 SBG 27370000
* COMPRESS THE BLANKS OUT WITH THE CBT973 PROGRAM WHEN     *  11/88 SBG 27380000
* UNLOADING THE FILE ON TO THE CBT TAPE.  TAPEMAP USES     *  11/88 SBG 27390000
* THE CBT973 DECOMPRESS ALGORITHM TO DECOMPRESS ONE        *  11/88 SBG 27400000
* 80-BYTE RECORD AT A TIME, AND THEN USES ITS IEBUPDTE     *  11/88 SBG 27410000
* MEMBER NAME INTERPRETATION ON EACH DECOMPRESSED RECORD.  *  11/88 SBG 27420000
*                                                          *  11/88 SBG 27430000
* MEMBER NAMES IN UNLOADED LIBRARIES ARE THUS DIRECTLY     *  12/88 SBG 27440001
* REPORTED BY DOING A "TAPEMAP" RUN ON A CBT TAPE.         *  11/88 SBG 27450000
*                                                          *  12/88 SBG 27460001
* USUALLY ANY FB 80-BYTE LRECL NON-IEBUPDTE FILES ARE ALSO *  12/88 SBG 27461001
* COMPRESSED.  THESE FILES WILL APPEAR ON REPORT 2 BUT     *  12/88 SBG 27462001
* WILL NOT SHOW AS HAVING ANY MEMBERS.                     *  12/88 SBG 27463001
************************************************************  11/88 SBG 27470000
TEST$CBT CLC   TAPEBUFF+2(3),=X'000000' LOOK LIKE BDW+X'00'?  11/88 SBG 27480000
         BNER  R10                      NO CAN'T BE CBT973 FMT11/88 SBG 27490000
         CLC   TAPEBUFF+0(2),=X'0000'   LOOK LIKE BDW?        11/88 SBG 27500000
         BER   R10                      NO CAN'T BE CBT973 FMT11/88 SBG 27510000
         CLC   TAPEBUFF+6(2),=X'0000'   LOOK LIKE RDW?        11/88 SBG 27520000
         BNER  R10                      NO CAN'T BE CBT973 FMT11/88 SBG 27530000
         CLC   TAPEBUFF+4(2),=X'0000'   LOOK LIKE RDW?        11/88 SBG 27540000
         BER   R10                      NO CAN'T BE CBT973 FMT11/88 SBG 27550000
TEST$CBA LA    R1,TAPEBUFF+4    IF CBT973, THEN START OF RDW  11/88 SBG 27560000
         LH    R3,0(,R1)    DON'T BE MODULO 256..SAVE IT ALL. 07/90 SBG 27580101
         C     R3,=F'94'            LRECL=94 MAX FOR CBT973.  11/88 SBG 27581001
         BHR   R10                  BETWEEN 95 & 255, NOT CBT 11/88 SBG 27582001
         ST    R4,CBSAVE4           USE R4 FOR WKREG          11/88 SBG 27583001
         LH    R4,TAPEBUFF          GET BLKSIZE FROM BDW      11/88 SBG 27584001
         S     R4,=F'4'             4 LESS FOR COMPARE W/RECSZ11/88 SBG 27585001
         CR    R3,R4                ONE RECORD IN THIS BLOCK? 11/88 SBG 27586001
         BL    TEST$CBN             YES. SKIP TEST PAST EO-REC11/88 SBG 27587001
************************************************************* 12/88 SBG 27587101
*   "EMPTY FILES" ON THE CBT TAPE ARE NOT REALLY EMPTY.     * 12/88 SBG 27587201
*    THEY ARE ONE-RECORD CBT973 COMPRESSED FILES WITH A     * 12/88 SBG 27587301
*    CERTAIN FIXED LITERAL EXPRESSION ON THE ONE CARD.      * 12/88 SBG 27587401
*    A USER OF THE CBT TAPE WOULD LIKE TO KNOW WHICH TAPE   * 12/88 SBG 27587501
*    FILES ARE CONSIDERED "EMPTY" BY THE TAPE ADMINISTRATOR.* 12/88 SBG 27587601
************************************************************* 12/88 SBG 27587701
TEST$CBL CLC   14(L'EMPTYLIT,R1),EMPTYLIT   CBT "EMPTY" FILE? 11/88 SBG 27587801
         BE    TEST$CBE             FLAG AS CBT EMPTY FILE    12/88 SBG 27587901
         CLC   14(L'EMPTYLI2,R1),EMPTYLI2   OLD "EMPTY" FILE? 12/88 SBG 27588001
         BNE   TEST$CBN             NO-STILL TEST FOR CBT973. 12/88 SBG 27588101
************************************************************* 12/88 SBG 27588201
*   AN "EMPTY FILE" ON THE CBT TAPE HAS BEEN FOUND.         * 12/88 SBG 27588301
************************************************************* 12/88 SBG 27588401
TEST$CBE MVC   UNLOADER(8),CBTEMPTY  REPORT AS EMPTY.         12/88 SBG 27588501
         L     R4,CBSAVE4           RESTORE REG 4             11/88 SBG 27588601
         TM    TFLAG2,T2@PRT2       IS SECOND REPORT WANTED?  12/88 SBG 27588701
         BOR   R10            NO SECOND REPORT FOR EMPTY FILE 12/88 SBG 27588801
         B     8(,R10)        WE WANT TO FLAG IF ONE REPORT   12/88 SBG 27588901
************************************************************* 12/88 SBG 27589001
*   CBTEMPTY IS FLAGGED ON 'PARM=INLINE', BUT WE DON'T WANT * 12/88 SBG 27589101
*    WANT EMPTY CBT FILES REPORTED ON THE SECOND REPORT     * 12/88 SBG 27589201
*    TO CONFUSE SOMEONE WHO MAY THINK IT IS A NONTRIVIAL    * 12/88 SBG 27589301
*    NON-IEBUPDTE CBT973-COMPRESSED FILE.                   * 12/88 SBG 27589401
************************************************************* 12/88 SBG 27589501
TEST$CBN DS    0H                   NOT CBT EMPTY FILE        11/88 SBG 27589601
         AR    R3,R1                PAST RECORD END           11/88 SBG 27589701
         LH    R4,0(,R3)            GET SECOND RECORD SIZE    11/88 SBG 27589801
         C     R4,=F'14'            SEE IF LESS THAN 14       11/88 SBG 27589901
         BL    TEST$CJ1             LESS THAN 14 IS NO GOOD   08/90 SBG 27590001
         C     R4,=F'94'            SEE IF MORE THAN 94       08/90 SBG 27590101
         BNH   TEST$CJ2             IN BETWEEN, TEST FURTHER  08/90 SBG 27590301
TEST$CJ1 L     R4,CBSAVE4           RESTORE REG 4 BEFORE RET  08/90 SBG 27590401
         BR    R10                  GO BACK TO CALLER         08/90 SBG 27590501
TEST$CJ2 CLI   0(R3),X'00'          IF CBT973, MUST BE X'00'  08/90 SBG 27600001
         BNER  R10                  NOT SO, THEN NOT CBT973   11/88 SBG 27610000
         CLI   1(R3),X'00'          IF CBT973, CAN'T BE X'00' 11/88 SBG 27620001
         BER   R10                  IT IS, THEN NOT CBT973    11/88 SBG 27630000
         CLC   2(2,R3),=X'0000'     LOOK LIKE RDW?            11/88 SBG 27640001
         BNER  R10                  NO. CAN'T BE CBT973 FMT   11/88 SBG 27650001
TEST$CBM LA    R1,14(,R1)           GO TO "NONBLANK" DATA     11/88 SBG 27660001
TEST$CBO CLI   0(R1),X'40'          HAD BETTER NOT BE A BLANK 11/88 SBG 27670001
         BER   R10                  IT IS A BLANK. NO GOOD.   11/88 SBG 27680000
         LA    R1,1(,R1)            BUMP ONE BYTE             11/88 SBG 27690001
         CR    R1,R3                END OF NONBLANK DATA?     11/88 SBG 27700000
         BL    TEST$CBO             NO, KEEP TRYING           11/88 SBG 27710001
         MVC   UNLOADER(6),CBT973   ASSUME CBT973 FORMAT      11/88 SBG 27740000
         B     TEST$RET                 RETURN                11/88 SBG 27740100
         SPACE 2                                              11/88 SBG 27740200
TEST$CPY CLC   =X'CA6D0F',TAPEBUFF+9    VS2COPY'S TRADEMARK?  10/05 SBG 27740300
         BE    TEST$CYY                 YES                   06/93 RT  27740400
         CLC   =X'CA6D0F',TAPEBUFF+9    VS2COPY'S PDSE MK?    10/05 SBG 27740300
         BNER  R10                      NO                              27740400
TEST$CYY MVC   UNLOADER(7),IEBCOPY      SAY WHO                         27740500
         B     TEST$RET                 RETURN                          27740600
         SPACE 2                                                        27740700
TEST$UPS ST    R10,DWD                  SAVE MY RETURN ADDR             27740800
         LA    R2,80                    BXLE INCR                       27740900
         LA    R15,TAPEBUFF             BXLE START                      27741000
         LR    R3,R15                   COPY FOR END                    27741100
         A     R3,LASTSIZE         POINT PAST END            09/84 DBC  27741200
         SR    R3,R2                    POINT TO LAST CARD IN BLOCK     27741300
         SPACE 1                                                        27741400
TEST$UPL LR    R1,R15                   COPY CARD START ADDR            27741500
         BA&XA R10,TEST$UP              IS THIS IEBUPDTE CTL CARD?      27741600
         B     TEST$UPB                 NO - GO BXLE                    27741700
         MVC   UNLOADER(8),IEBUPXXX SAY WHO                   06/84 DBC 27741800
         L     R10,DWD                  GET BACK MY RETURN ADDR         27741900
         B     TEST$RET                 AND RETURN TO CALLER            27742000
         SPACE 1                                                        27742100
TEST$UPB BXLE  R15,R2,TEST$UPL          TRY ALL CARDS IN THIS BLOCK     27742200
         L     R10,DWD                  RESTORE RETURN ADDR             27742300
         BR    R10                      NOT IEBUPDTE INPUT STREAM       27742400
         SPACE 2                                                        27742500
TEST$UP  CLC   =C'./',0(R1)             START RIGHT?                    27742600
         MVI   UPDTXFLG,C' '            ASSUME ./ CONTROL CARD11/88 SBG 27742700
         BE    TEST$UPC                 CONTINUE              11/88 SBG 27742800
TEST$UPX CLC   =C'><',0(R1)             SUBORDINATE MEMBER?   11/88 SBG 27742900
         BNER  R10                      NO - THAT WAS QUICK             27743000
         MVI   UPDTXFLG,C'>'            REALLY >< CONTROL CARD11/88 SBG 27744000
TEST$UPC DS    0H                       CONTINUE PROCESSING   11/88 SBG 27744100
         LA    R1,2(,R1)                POINT TO SCAN START             27744200
         LA    R0,69                    MAX SCAN LENGTH                 27744300
         BA&XA R14,F$BLANK              FIND END OF NAME FIELD          27744400
         BA&XA R14,F$CHARS              FIND THE VERB                   27744500
         CLC   =C'ADD ',0(R1)                                           27744600
         BE    TU$SAVE                                                  27744700
         CLC   =C'REPL ',0(R1)                                          27744800
         BE    TU$SAVE                                                  27744900
         CLC   =C'CHANGE ',0(R1)                                        27745000
         BE    TU$SAVE                                                  27745100
         CLC   =C'CHNGE ',0(R1)    IEBUPDAT TYPE CHANGE?      06/84 DBC 27745200
         BNER  R10                 NO, RETURN EMPTY HANDED    06/84 DBC 27745300
         SPACE 2                                                        27745400
TU$SAVE  MVC   UPDT#TYP(1),0(R1)        A,R,C                           27745500
         MVC   IEBUPXXX,=C'IEBUPDTE' ASSUMPTION               06/84 DBC 27745600
         STM   R0,R1,TU$SAVE2      SAVE SCANNER               06/84 DBC 27745700
         SPACE 1                                                        27745800
TU$LOOP  LA    R1,1(,R1)                                                27745900
         CLC   =C'MEMBER=',0(R1)                                        27746000
         BE    TU$MEM                                                   27746100
         CLC   =C'NAME=',0(R1)                                          27746200
         BE    TU$NAM                                                   27746300
         BCT   R0,TU$LOOP               SCAN THE REST OF THE CARD       27746400
         SPACE 3                                              06/84 DBC 27746500
************************************************************* 06/84 DBC 27746600
*        THIS IS NOT AN INTERESTING IEBUPDTE TYPE CARD, BUT * 06/84 DBC 27746700
*        IT STILL MIGHT BE AN INTERESTING IEBUPDAT TYPE     * 06/84 DBC 27746800
*        CARD. SCAN TO SEE IF THE OPERAND FIELDS START WITH * 06/84 DBC 27746900
*        A FIRST POSITIONAL PARAMETER THAT IS A MEMBER      * 06/84 DBC 27750000
*        NAME.                                              * 06/84 DBC 27760000
************************************************************* 06/84 DBC 27770000
         SPACE 1                                              06/84 DBC 27780000
         LM    R0,R1,TU$SAVE2      RESTORE VERB PTR           06/84 DBC 27790000
         BA&XA R14,F$BLANK         SCAN PAST THE VERB         06/84 DBC 27800000
         BA&XA R14,F$CHARS         SCAN TO THE OPERANDS       06/84 DBC 27810000
         STM   R0,R1,TU$SAVE2      SAVE OPERANDS PTR          06/84 DBC 27820000
         SPACE 1                                              06/84 DBC 27830000
         LA    R0,9                MAX L'NAME+1               06/84 DBC 27840000
         BA&XA.R  R14,0            LOOP HEAD                  06/84 DBC 27850000
         CLI   0(R1),C'='          IEBUPDTE TYPE CARD?        06/84 DBC 27860000
         BER   R10                 YES, GIVE UP               06/84 DBC 27870000
         CLI   0(R1),C','          NO, EO-1ST OPERAND?        06/84 DBC 27880000
         BE    TU$UPDAT            YES, GO SAVE               06/84 DBC 27890000
         CLI   0(R1),C' '          MAYBE, CHECK AGAIN         06/84 DBC 27900000
         BE    TU$UPDAT            YES, GO SAVE               06/84 DBC 27910000
         LA    R1,1(,R1)           NO, ADVANCE SCANNER        06/84 DBC 27920000
         BCTR  R0,R14              LOOP BACK                  06/84 DBC 27930000
         BR    R10                 1ST OPERAND TOO LONG; GVUP 06/84 DBC 27940000
         SPACE 1                                              06/84 DBC 27950000
TU$UPDAT MVC   IEBUPXXX,=C'IEBUPDAT' RESET "UNLOADER" NAME    06/84 DBC 27960000
         LM    R0,R1,TU$SAVE2      RESTORE NAME POINTER       06/84 DBC 27970000
         B     4(,R10)             RETURN WITH NAME           06/84 DBC 27980000
         SPACE 2                                                        27990000
TU$MEM   LA    R1,7(,R1)                POINT TO MEMBER NAME            28000000
         B     *+8                      SKIP OTHER LA                   28010000
         SPACE 1                                                        28020000
TU$NAM   LA    R1,5(,R1)                POINT TO MEMBER NAME            28030000
         SPACE 1                                                        28040000
         MVI   8(R1),C','               BE SURE TO STOP                 28050000
         B     4(,R10)                  FOUND A NAME                    28060000
         EJECT ,                                              07/85 DBC 28070000
************************************************************* 07/85 DBC 28080000
*        TEST TO SEE IF THE CURRENT FILE IS A SMPPTFIN      * 07/85 DBC 28090000
*        FILE.                                              * 07/85 DBC 28100000
************************************************************* 07/85 DBC 28110000
         SPACE 1                                              07/85 DBC 28120000
TEST$PTF ST    R10,DWD             SAVE THE RETURN PTR        07/85 DBC 28130000
         LA    R2,80               GET BXLE SCANNER           07/85 DBC 28140000
         LA    R15,TAPEBUFF        --> TAPE BUFFER            07/85 DBC 28150000
         LR    R3,R15              GET -                      07/85 DBC 28160000
         AL    R3,LASTSIZE          BXLE -                    07/85 DBC 28170000
         SLR   R3,R2                 LIMIT                    07/85 DBC 28180000
         SPACE 1                                              07/85 DBC 28190000
TEST$PTL LR    R1,R15              --> NEXT CARD              07/85 DBC 28200000
         BA&XA R10,TEST$PT         SEE IF IT'S ++PTF, ++APAR, 07/85 DBC 28210000
*                                  ++FUNCTION, OR ++USERMOD   07/85 DBC 28220000
         B     TEST$PTB            +0 NOPE, SKIP TO NEXT CARD 07/85 DBC 28230000
         MVC   UNLOADER(8),SMPPTFIN +4 YEP, NAME THE          07/85 DBC 28240000
*                                  "UNLOADER"                 07/85 DBC 28250000
         L     R10,DWD             RESTORE RETURN ADDRESS     07/85 DBC 28260000
         B     TEST$RET            GO RETURN TO CALLER        07/85 DBC 28270000
         SPACE 1                                              07/85 DBC 28280000
TEST$PTB BXLE  R15,R2,TEST$PTL     ADVANCE TO NEXT CARD AND   07/85 DBC 28290000
*                                  LOOP TO TEST IT            07/85 DBC 28300000
         SPACE 1                                              07/85 DBC 28310000
         L     R10,DWD             NO ++PTF ATC. CARD WAS     07/85 DBC 28320000
*                                  FOUND. RESTORE LINK        07/85 DBC 28330000
         BR    R10                 RETURN EMPTY HANDED        07/85 DBC 28340000
         SPACE 3                                              07/85 DBC 28350000
************************************************************* 07/85 DBC 28360000
*        SEE IF THE CURRENT RECORD IS A SMP SYSMOD HEADER   * 07/85 DBC 28370000
*        CARD (++PTF, ++APAR, ++FUNCTION, OR ++USERMOD).    * 07/85 DBC 28380000
*        NOTE, THIS SCAN EXAMINES ONLY ONE CARD AT A TIME.  * 07/85 DBC 28390000
*        ACCORDINGLY, IT WILL WORK ONLY IF THE SYSMOD'S     * 07/85 DBC 28400000
*        NAME IS NOT CONTINUED ONTO ANOTHER CARD.           * 07/85 DBC 28410000
************************************************************* 07/85 DBC 28420000
         SPACE 1                                              07/85 DBC 28430000
TEST$PT  CLC   =C'++',0(R1)        PLUSPLUS CARD?             07/85 DBC 28440000
         BNER  R10                 NO, RETURN +0              07/85 DBC 28450000
         LA    R1,2(,R1)           YES, ADVANCE SCANNER       07/85 DBC 28460000
         LA    R0,69               LOAD SCAN LIMIT            07/85 DBC 28470000
         BA&XA R14,F$CHARS         SKIP BLANKS                07/85 DBC 28480000
         SPACE 1                                              07/85 DBC 28490000
         LA    R14,7               L'SYSMOD TYPE NAME         07/85 DBC 28500000
         CLC   =C'USERMOD',0(R1)   ++USERMOD?                 07/85 DBC 28510000
         BE    TP$SAVE             YES, PROCEED               07/85 DBC 28520000
         LA    R14,3               NO, RESET L'NAME           07/85 DBC 28530000
         CLC   =C'PTF',0(R1)       ++PTF?                     07/85 DBC 28540000
         BE    TP$SAVE             YES, PROCEED               07/85 DBC 28550000
         LA    R14,8               NO, RESET L'NAME           07/85 DBC 28560000
         CLC   =C'FUNCTION',0(R1)  ++FUNCTION?                07/85 DBC 28570000
         BE    TP$SAVE             YES, PROCEED               07/85 DBC 28580000
         LA    R14,4               YES, SET L'NAME            07/85 DBC 28610000
         CLC   =C'APAR',0(R1)      NO, ++APAR?                07/85 DBC 28590000
         BE    TP$SAVE             YES. GO ON.                          28600000
* --- >>      SMPPTFIN STATEMENTS NOT TO BE REPORTED - BELOW
*             THESE STATEMENTS SHOW THAT THE FILE IS
*             SMPPTFIN, BUT THEY DO NOT INDICATE SYSMODS.
         LA    R14,6               NO, RESET L'NAME
         CLC   =C'ASSIGN',0(R1)    ++ASSIGN
         BE    TP$ASSGN            YES, LET US KNOW SMPPTFIN
         LA    R14,4               NO, RESET L'NAME
         CLC   =C'HOLD',0(R1)      ++HOLD
         BE    TP$ASSGN            YES, LET US KNOW SMPPTFIN
         LA    R14,7               NO, RESET L'NAME
         CLC   =C'RELEASE',0(R1)   ++RELEASE
         BE    TP$ASSGN            YES, LET US KNOW SMPPTFIN
         LA    R14,7               NO, RESET L'NAME
         CLC   =C'FEATURE',0(R1)   ++FEATURE
         BE    TP$ASSGN            YES, LET US KNOW SMPPTFIN
         LA    R14,7               NO, RESET L'NAME
         CLC   =C'PRODUCT',0(R1)   ++PRODUCT
         BE    TP$ASSGN            YES, LET US KNOW SMPPTFIN
         LA    R14,4               NO, RESET L'NAME
         CLC   =C'NULL',0(R1)      ++NULL
         BE    TP$ASSGN            YES, LET US KNOW SMPPTFIN
         LA    R14,5               NO, RESET L'NAME
         CLC   =C'JCLIN',0(R1)     ++JCLIN
         BE    TP$ASSGN            YES, LET US KNOW SMPPTFIN
         BNER  R10                 NO, RETURN +0
* --- >>      SMPPTFIN STATEMENTS NOT TO BE REPORTED - ABOVE
         SPACE 1                                              07/85 DBC 28620000
TP$SAVE  MVC   PTF#TYPE,0(R1)      SAVE SYSMOD TYPE           07/85 DBC 28630000
         SR    R0,R14              DECR LOOP CNTL; TOO FAR?   07/85 DBC 28640000
         BNPR  R10                 YES, RETURN +0             07/85 DBC 28650000
         ALR   R1,R14              NO, ADVANCE SCANNER        07/85 DBC 28660000
         BA&XA R14,F$CHARS         SKIP MORE BLANKS, IF ANY   07/85 DBC 28670000
         CLI   0(R1),C'('          SUB-VALUE?                 07/85 DBC 28680000
         BNER  R10                 NO, RETURN +0              07/85 DBC 28690000
         BCT   R0,TP$PTF           YES, DECR SCAN LIMIT       07/85 DBC 28700000
         BR    R10                 RETURN +0 IF TOO FAR       07/85 DBC 28710000
TP$PTF   LA    R1,1(,R1)           ADVANCE ONE                07/85 DBC 28720000
         BA&XA R14,F$CHARS         SKIP MORE BLANKS, IF ANY   07/85 DBC 28730000
         B     4(,R10)             RETURN +4 WITH A POINTER   07/85 DBC 28740000
*                                  TO THE SYSMOD NAME.        07/85 DBC 28750000
TP$ASSGN DS    0H                  ASSIGN ETC., SKIP PRINT,
         MVI   PTF#TYPE,C'X'       BUT SHOW SMPPTFIN WITH SPECIAL CHAR
         B     4(,R10)             RETURN +4 WITH A POINTER             28740000
         EJECT ,                                              07/85 DBC 28760000
************************************************************* 12/88 SBG 28760101
*  WE WANT TO CHECK IF WE HAVE FILE 1 FROM A REAL CBT TAPE. * 12/88 SBG 28760201
*   IF THAT'S WHAT WE HAVE, WE WANT TO REPORT THE VERSION   * 12/88 SBG 28760301
*   NUMBER AND CREATION DATE OF THAT CBT TAPE.              * 12/88 SBG 28760401
************************************************************* 12/88 SBG 28760501
TEST$CH  ST    R15,CBSAVE15        SAVE REG 15                12/88 SBG 28760601
         LA    R15,TAPEBUFF        CBT HEADER. LOOK FOR       12/88 SBG 28760801
         CLC   CHJOBLIT,0(R15)     ARNIE CASINGHINO'S JOBNAME 12/88 SBG 28760901
         BE    TEST$CHG            CBT HEADER                 12/88 SBG 28761001
         L     R15,CBSAVE15        RESTORE REG 15             12/88 SBG 28761101
         BR    R10                 RETURN NOT FOUND           12/88 SBG 28761301
TEST$CHG MVC   UNLOADER(8),CBHEADER     FLAG AS FILE 1 OF CBT 12/88 SBG 28761401
         MVI   CSTATCLR,X'40'      CLEAR VERS & DATE          12/88 SBG 28761501
         MVC   CBTVERS(12),CSTATCLR   NOW DO IT .             12/88 SBG 28761601
         ST    R4,CBSAVE4          SAVE REG 4                 12/88 SBG 28761701
         LR    R4,R15              ADDR OF TAPE BUFFER        12/88 SBG 28761801
         A     R4,CDOCSIZE         END OF BLOCK ADDRESS       12/88 SBG 28761901
TEST$CHL LA    R15,1(,R15)         SEARCH FOR DATE AND VERSION12/88 SBG 28762001
         CR    R15,R4              END OF BLOCK YET?          12/88 SBG 28762101
         BNL   TEST$CHR            NO DATE OR VERSION FOUND   12/88 SBG 28762201
         CLC   VERSLIT1,0(R15)     DATE HDR FOUND?            12/88 SBG 28762301
         BE    TEST$CHD            YEP. GET DATE.             12/88 SBG 28762401
         CLC   VERSLIT2,0(R15)     VERSION # HDR FOUND?       12/88 SBG 28762501
         BNE   TEST$CHL            NOPE. KEEP TRYING.         12/88 SBG 28762601
TEST$CHV MVC   CBTVERS(4),10(R15)  SAVE VERSION #             12/88 SBG 28762701
         L     R4,CBSAVE4          RESTORE REG 4              12/88 SBG 28762801
         L     R15,CBSAVE15        RESTORE REG 15             12/88 SBG 28762901
         B     TEST$RET            DONE WITH EXTRACTION.      12/88 SBG 28763001
TEST$CHD MVC   CBTDATE(8),10(R15)  SAVE TAPE RELEASE DATE     12/88 SBG 28763101
         B     TEST$CHL            KEEP TRYING FOR VERSION #. 12/88 SBG 28763201
TEST$CHR L     R4,CBSAVE4          RESTORE REG 4              12/88 SBG 28763301
         L     R15,CBSAVE15        RESTORE REG 15             12/88 SBG 28763401
         BR    R10                 RETURN UNSUCCESSFUL        12/88 SBG 28763501
         EJECT                                                12/88 SBG 28764001
F$BLANK  BCTR  R0,0                     -1 FROM LENGTH                  28770000
         CLI   0(R1),C' '               GOT A BLANK YET?                28780000
         BER   R14                      YES - RETURN                    28790000
         LA    R1,1(,R1)                MOVE SCAN PTR                   28800000
         BCT   R0,F$BLANK+2             KEEP SCANNING                   28810000
         BR    R10                      NONE--RETURN TO CALLER'S CALLER 28820000
         SPACE 2                                                        28830000
F$CHARS  BCTR  R0,0                     -1 FROM LENGTH                  28840000
         CLI   0(R1),C' '               FIND A CHAR?                    28850000
         BNER  R14                      YES - RETURN                    28860000
         LA    R1,1(,R1)                INCR SCAN PTR                   28870000
         BCT   R0,F$CHARS+2             KEEP SCANNING                   28880000
         BR    R10                      NONE--RETURN TO CALLER'S CALLER 28890000
         SPACE 2                                                        28900000
TEST$DMP CLC   =X'F47006016663B24D',TAPEBUFF+12  IEHDASDR'S TRADEMARK?  28910000
         BNER  R10                      NO                              28920000
         MVC   UNLOADER(8),IEHDASDR     SAY WHO                         28930000
         MVC   DASDRSAV(24),TAPEBUFF    SAVE HEADER RECORD              28940000
         LR    R2,R10                   SAVE RETURN ADDR                28950000
         TAPIO TCCW#DAT                 IGNORE CCWS                     28960000
         SPACE 1                                                        28970000
COM$DUMP TAPIO TCCW#DAT                 READ FIRST TRK'S DATA           28980000
         MVC   FDRSAVE(8),TAPEBUFF+12   SAVE IN CASE FDR                28990000
         LR    R10,R2                   RETURN                          29000000
         B     TEST$RET                                                 29010000
         SPACE 2                                                        29020000
TEST$FDR CLC   =C'THATS ALL FOLK',TAPEBUFF+5   HIS TRADEMARK?           29030000
         BNER  R10                      NO                              29040000
         LR    R2,R10                   SAVE RETURN ADDR                29050000
         MVC   DASDRSAV(24),TAPEBUFF    SAVE HEADER RECORD              29060000
         MVC   UNLOADER(3),FDR          SAY FDR FOR NOW                 29070000
         CLI   TAPEBUFF+19,C'S'         WAS IT FDR?                     29080000
         BE    COM$DUMP                 YES                             29090000
         MVC   UNLOADER(6),FDRDSF       NO  - MAYBE FDRDSF              29100000
         CLI   TAPEBUFF+19,C'F'         FDRDSF?                         29110000
         BE    COM$DUMP                 YES                             29120000
         MVC   UNLOADER(6),FDRXXX       NO  - BUT SOME KIND OF FDR?     29130000
         B     COM$DUMP                 CONTINUE                        29140000
         SPACE 2                                                        29150000
TEST$SLK CLC   =F'20',LASTSIZE     RIGHT LENGTH FOR SLICK HDR 09/84 DBC 29160000
         BNER  R10                                                      29170000
         CLC   TAPEBUFF+8(12),FFS         FF'S AT END OF RECORD?        29180000
         BNER  R10                                                      29190000
         CLI   TAPEBUFF+1,C'B'          BACKUP FLAG PRESENT?            29200000
         BNER  R10                                                      29210000
         MVC   DASDRSAV(20),TAPEBUFF    SAVE THE FIRST RECORD           29220000
         LR    R2,R10                   SAVE RETURN ADDR                29230000
         TAPIO TCCW#DAT                 GET 2ND RECORD                  29240000
         LR    R10,R2                   RESTORE RETURN ADDR             29250000
         CLC   TAPEBUFF(2),=H'0'        1ST HALFWORD = 0?               29260000
         BNER  R10                                                      29270000
         CLC   TAPEBUFF+2(2),=H'0'      2ND HALFWORD = 0 OR FF'S?       29280000
         BE    TEST$SK1                                                 29290000
         CLC   TAPEBUFF+2(2),FFS                                        29300000
         BNER  R10                                                      29310000
TEST$SK1 LH    R2,TAPEBUFF+4            VERIFY LENGTH OF RECORD         29320000
         LTR   R2,R2                                                    29330000
         BNPR  R10                                                      29340000
         LA    R2,8(,R2)                                                29350000
         C     R2,LASTSIZE         LENGTH OK?                09/84 DBC  29360000
         BNER  R10                                                      29370000
         CLC   TAPEBUFF+6(2),=H'0'      LAST HALF OF RDW = 0?           29380000
         BNER  R10                                                      29390000
         MVC   UNLOADER(8),SLICKMNG     SHOW SLICKMNG AS UNLOADER       29400000
         B     TEST$RET                 RETURN TO MAINLINE              29410000
         SPACE 3                                                        29420000
* FOR IEBISAM, THE BEGINNING OF THE DCB IS UNLOADED TO THE FIRST        29430000
* TWO CARDS.  SO CHECK (GUESS?) IF IT LOOKS LIKE AN ISAM DCB            29440000
TEST$IS  CLC   TAPEBUFF(4),=Y(0,154)    SEQ # AND LENGTH CORRECT?       29450000
         BNER  R10                      NO                              29460000
         CLC   TAPEBUFF+80(2),H1        SECOND SEQ # CORRECT            29470000
         BNER  R10                      NO                              29480000
         TM    TAPEBUFF+4+48,X'10'      DCBOFLGS,DCBOFOPN               29490000
         BNOR  R10                      NOT "OPEN"                      29500000
         CLI   TAPEBUFF+4+26,X'80'      DSORG=IS?                       29510000
         BE    *+10                     YES - CONTINUE                  29520000
         CLI   TAPEBUFF+4+26,X'81'      DSORG=ISU?                      29530000
         BNER  R10                      NO                              29540000
         MVC   UNLOADER(7),IEBISAM      SAY WHO                         29550000
         B     TEST$RET                 RETURN                          29560000
         SPACE 3                                                        29570000
TEST$MAP CLC   =C'$$TAPEMAP.PRINT.FILE$$',TAPEBUFF  CHECK FOR HEADER    29580000
         BNER  R10                      NO                              29590000
         L     R1,LASTSIZE         VERIFY BLOCK MULT OF 80 BY 09/84 DBC 29600000
         SR    R0,R0                                                    29610000
         LA    R2,80                                                    29620000
         DR    R0,R2                                                    29630000
         LTR   R0,R0                                                    29640000
         BNZR  R10                      NO                              29650000
         MVC   UNLOADER(7),CTAPEMAP     YES - THIS IS FOR US            29660000
         B     TEST$RET                                                 29670000
         EJECT                                                          29680000
IEBUPDT1 MVC   OUTBUFF2+8(27),=C'IS AN IEBUPXXX INPUT STREAM' 07/85 DBC 29690000
         MVC   OUTBUFF2+14(8),IEBUPXXX CORRECT RELOADER NAME  07/85 DBC 29700000
         B     PRT$ONE             GO RELOOP                  07/85 DBC 29710000
         SPACE 3                                              07/85 DBC 29720000
PTFS1    MVC   OUTBUFF2+8(35),=C'IS AN SMPPTFIN SYSMODS INPUT STREAM' C 29730000
         B     PRT$ONE             GO RELOOP                  07/85 DBC 29740000
         SPACE 3                                              07/85 DBC 29750000
CBT1     CLC   CBTEMPTY,UNLOADER        CBT EMPTY FILE        11/88 SBG 29750101
         BE    CBTEMP1                  PRINT SPECIAL INFO    11/88 SBG 29750201
         MVC   OUTBUFF2+8(28),=C'IS IN CBT973 COMPRESS FORMAT'  /88 SBG 29750301
         B     PRT$ONE             GO RELOOP                  11/88 SBG 29750400
         SPACE 3                                              07/85 DBC 29750500
CBTEMP1  MVC   OUTBUFF2+8(25),=C'IS AN EMPTY CBT TAPE FILE'   11/88 SBG 29750601
         B     PRT$ONE             GO RELOOP                  11/88 SBG 29750701
         SPACE 3                                              07/85 DBC 29750801
CBTDOC1  MVC   OUTBUFF2+8(27),=C'CBT TAPE DOCUMENTATION FILE' 12/88 SBG 29750901
         B     PRT$ONE             GO RELOOP                  12/88 SBG 29751001
         SPACE 3                                              12/88 SBG 29752001
IEHMOVE1 MVC   #BLKCNT+OUTBUFF2-OUTBUFF(4),=C'DSN='                     29760000
         MVC   #BLKCNT+OUTBUFF2-OUTBUFF+4(44),TAPEBUFF+85 MOVE DISK DSN 29770000
         SPACE 1                                                        29780000
         BA&XA R10,SET$ONE                                              29790000
         DC    Y(169)                   DSORG OFFSET                    29800000
         DC    Y(171)                   RECFM OFFSET                    29810000
         DC    Y(173)                   BLKSIZE OFFSET                  29820000
         DC    Y(175)                   LRECL OFFSET                    29830000
         SPACE 3                                                        29840000
IEBCOPY1 BA&XA R10,SET$ONE                                              29850000
         DC    Y(12)                    DSORG OFFSET                    29860000
         DC    Y(18)                    RECFM OFFSET                    29870000
         DC    Y(14)                    BLKSIZE OFFSET                  29880000
         DC    Y(16)                    LRECL OFFSET                    29890000
         SPACE 3                                                        29900000
IEBISAM1 BA&XA R10,SET$ONE                                              29910000
         DC    Y(4+26)                  DSORG OFFSET                    29920000
         DC    Y(4+36)                  RECFM OFFSET                    29930000
         DC    Y(4+62)                  BLKSIZE OFFSET                  29940000
         DC    Y(4+2+82)                LRECL OFFSET                    29950000
         SPACE 3                                                        29960000
SET$ONE  MVC   OUTBUFF2+8(24),=C'TRUE ATTRIBUTES:  DSORG='              29970000
         ICALL CNVDSORG,R14,R15=OUTBUFF2+32 FORMAT THE DSORG            29980000
         ICALL CNVRECFM,R14,R15=#RECFM+OUTBUFF2-OUTBUFF                 29990000
         SPACE 1                                                        30000000
         LA    R1,TAPEBUFF                                              30010000
         AH    R1,4(,R10)               POINT TO BLKSIZE                30020000
         ICALL CONVHALF,R14,R15=#BLKSIZE-1+OUTBUFF2-OUTBUFF             30030000
         SPACE 1                                                        30040000
         LA    R1,TAPEBUFF                                              30050000
         AH    R1,6(,R10)               POINT TO LRECL                  30060000
         ICALL CONVHALF,R14,R15=#LRECL-1+OUTBUFF2-OUTBUFF               30070000
         SPACE 2                                                        30080000
*  FOR PARM=INLINE, THE REPORT MUST PRINT BOTH THE RESULTS    12/88 SBG 30080101
*  OF THE LABEL PROCESSING (FOR SL) AND THE RESULTS OF THE    12/88 SBG 30080201
*  SCAN PROCESSING.                                           12/88 SBG 30080301
PRT$ONE  TM    TFLAG1,T1@SL             SL TAPE?              12/88 SBG 30080401
         BNO   PRT$ONEX                 NO. DO NORMAL STUFF   12/88 SBG 30080501
         TM    PARMFLAG,PF@INLIN        INLINE ?              12/88 SBG 30081001
         BO    FIND$EOF                 REGULAR INLINE STUFF  12/88 SBG 30083001
PRT$ONEX TM    TFLAG1,T1@ANLZ           ANALYZING?            12/88 SBG 30090001
         BNO   FIND$EOF                 NO                              30100000
         B     NL$LOOP                  YES - KEEP SCANNING NL TAPE     30110000
         SPACE 3                                                        30120000
*  FIRST RECORD SAVED IN "DASDRSAV"                                     30130000
IHDASDR1 MVC   OUTBUFF2+8(15),=C'DUMP FROM CCHH='                       30140000
         UNPK  OUTBUFF2+23(9),DASDRSAV+0(5)                             30150000
         TR    OUTBUFF2+23(8),HEXTAB    MAKE EBCDIC                     30160000
         MVC   OUTBUFF2+31(5),=C' OF A'                                 30170000
         IC    R1,DASDRSAV+21           GET DEVTYPE                     30180000
         N     R1,=F'255'               CLEAN                           30190000
         MH    R1,=H'6'                                                 30200000
         LA    R14,=C'2321  2311  2314  2302  2301  2305-12305-23330  3*30210000
               340  3350  3375  3330-13380  3390  '           03/94 SBG 30220000
         LA    R14,0(R14,R1)            GET ADDR OF THIS DEVTYPE        30230000
         MVC   OUTBUFF2+37(6),0(R14)    MOVE TO PRINT LINE              30240000
         CLC   =F'0',DASDRSAV+8         THIS TAPE START AT 0?           30250000
         BNE   PRT$ONE                  NO - CAN'T FIND VOLSER          30260000
*  SECOND RECORD IS IN "TAPEBUFF"                                       30270000
         CLC   =C'VOL1VOL1',TAPEBUFF+216  LABEL?                        30280000
         BNE   PRT$ONE                  NO - SKIP                       30290000
         MVC   OUTBUFF2+46(7),=C'VOLUME='                               30300000
         MVC   OUTBUFF2+53(6),TAPEBUFF+224                              30310000
         MVC   OUTBUFF2+61(6),=C'OWNER='                                30320000
         MVC   OUTBUFF2+67(10),TAPEBUFF+261                             30330000
         B     PRT$ONE                                                  30340000
         SPACE 3                                                        30350000
* FIRST RECORD SAVED IN "DASDRSAV"                                      30360000
FDR1     MVC   OUTBUFF2+8(20),=C'DUMPED FROM A UUUUUU'                  30370000
         MVC   FDRDVEND(1),DASDRSAV+4   SET UP FENCE FOR DEV TYPE       30380000
         LA    R1,FDRDVTAB              POINT TO FDR DEV TYPE TABLE     30390000
FDR1LOOP CLC   DASDRSAV+4(1),0(R1)      MATCH?                          30400000
         BE    FDR1FND                  YES                             30410000
         LA    R1,7(,R1)                NO  - BUMP TO NEXT ENTRY        30420000
         B     FDR1LOOP                                                 30430000
FDR1FND  MVC   OUTBUFF2+22(6),1(R1)     PUT DEV TYPE IN MESSAGE         30440000
         CLC   =C'DUMMYDSF',FDRSAVE     IS IT A NORMAL DUMP?            30450000
         BE    PRT$ONE                  YES - ALL FOR NOW               30460000
*                                       NO  - MUST BE ABS TRK DUMP      30470000
         MVC   OUTBUFF2+28(22),=C' (ABSOLUTE TRACK DUMP)'               30480000
         B     PRT$ONE                                                  30490000
         SPACE 3                                                        30500000
SLK1     MVC   OUTBUFF2+8(30),=C'SLICK LIBRARY BACKUP, CYCLE = '        30510000
         MVC   OUTBUFF2+38(1),DASDRSAV  PUT IN THE CYCLE NUMBER         30520000
         MVC   OUTBUFF2+39(12),=C', MAXSIZE = '                         30530000
         ICALL CONVHALF,R14,R1=DASDRSAV+2,R15=OUTBUFF2+41               30540000
         B     PRT$ONE                  PRINT THE INFO LINE             30550000
         EJECT                                                          30560000
IEHMOVE2 LA    R0,7                     ASSUME PARM=NOMEM               30570000
         TM    PARMFLAG,PF@NOMEM        IS IT?                          30580000
         BO    *+8                      YES                             30590000
         LA    R0,9                     NO                              30600000
         TM    TAPEBUFF+169,X'02'       DSORG=PO?                       30610000
         BO    *+8                      YES                             30620000
         LA    R0,7                     NO                              30630000
         NEWPAGE  (R0),2                ENOUGH LINES LEFT ON PAGE?      30640000
         SPACE 1                                                        30650000
         BA&XA R10,SET$FULL             SET UP HEADER                   30660000
         DC    Y(169)                   DSORG OFFSET                    30670000
         DC    Y(171)                   RECFM OFFSET                    30680000
         DC    Y(173)                   BLKSIZE OFFSET                  30690000
         DC    Y(175)                   LRECL OFFSET                    30700000
         SPACE 1                                                        30710000
         MVC   OUTBUFF2+47(7),IEHMOVE   WHO IT WAS UNLOADED BY          30720000
         SPACE 1                                                        30730000
         ICALL CONVUNIT,R10,R1=TAPEBUFF+282 CONVERT UNIT TYPE           30740000
         SPACE 1                                                        30750000
         PRTLN2                         PRINT FIRST INFO LINE           30760000
         SPACE 1                                                        30770000
         MVI   OUTBUFF2,C'0'            DOUBLE SPACE FOR MSM            30780000
         MVC   OUTBUFF2+25(7),=C'C-DATE='                               30790000
         ICALL CONVDATE,R10,R1=TAPEBUFF+138,R15=OUTBUFF2+32 C-DATE      30800000
         SPACE 1                                                        30810000
         MVC   OUTBUFF2+42(7),=C'E-DATE='                               30820000
         ICALL CONVDATE,R10,R1=TAPEBUFF+141,R15=OUTBUFF2+49 E-DATE      30830000
         SPACE 1                                                        30840000
         MVC   OUTBUFF2+59(4),=C'DSN='                                  30850000
         MVC   OUTBUFF2+63(44),TAPEBUFF+85       MOVE IN OLD DSN        30860000
         SPACE 1                                                        30870000
****     CLC   =C'VS2',TAPEBUFF+152     CHECK PROGRAMMING SYSTEM        30880000
****     BE    MV$NOVOL                 SKIP IF OS/VS2                  30890000
         TM    TAPEBUFF+132,X'0F'       SEE IF LOOKS LIKE PACKED DATE   30900000
         BO    MV$NOVOL                 SKIP IF NEW FORMAT IEHMOVE DS   30910000
         CLI   TAPEBUFF+135,0           INVALID FOR VOLSER ANYWAY?      30920000
         BE    MV$NOVOL                 SKIP IF NEW FORMAT IEHMOVE DS   30930000
         MVC   OUTBUFF2+120(4),=C'VOL='                                 30940000
         MVC   OUTBUFF2+124(6),TAPEBUFF+130                             30950000
         SPACE 1                                                        30960000
MV$NOVOL PRTLN2                         PRINT SECOND INFO LINE          30970000
         SPACE 2                                                        30980000
         TM    TAPEBUFF+169,X'02'       DSORG=PO?                       30990000
         BNO   STAR$TWO                 NO - SKIP                       31000000
         BA&XA R10,FMU$                 PRINT HEADER                    31010000
         SPACE 2                                                        31020000
         L     BLEN,LASTSIZE       GET SIZE OF BLOCK          09/84 DBC 31030000
         SH    BLEN,=H'80'              MINUS FIRST CARD USED           31040000
         LA    BPTR,TAPEBUFF+80         -> NEXT CARD TO USE             31050000
         BA&XA R10,SEG$NEXT             NOW HAVE CARDS 1 & 2            31060000
         BA&XA R10,SEG$NEXT             NOW 2 & 3                       31070000
         BA&XA R10,SEG$NEXT             NOW 3 & 4                       31080000
         BA&XA R10,SEG$NEXT             NOW 4 & 5                       31090000
         LA    DPTR,MV#BUFF1+46         -> FIRST MEMBER NAME -4         31100000
         CLI   0(DPTR),X'C8'            START OF MEMBER?                31110000
         BE    MV$GOT1                  YES                             31120000
         LA    DPTR,3(,DPTR)            -> OTHER PLACE                  31130000
         CLI   0(DPTR),X'C8'            START OF MEMBER?                31140000
         BE    MV$GOT1                  YES                             31150000
         BA&XA R14,BOOM                 PRINT LOGIC ERROR MSG           31160000
         SPACE 2                                                        31170000
MV$GOT1  LA    DPTR,4(,DPTR)            -> FIRST MEMBER NAME            31180000
         SPACE 2                                                        31190000
MV$LOOP  BA&XA R10,MOVE$MEM             MEMBER NAME TO PRINT LINE       31200000
         BA&XA R10,SEG$TEST             SEE IF WITHIN RANGE             31210000
         SPACE 2                                                        31220000
*  RECORDS IN AN IEHMOVE UNLOADED DATASET HAVE THE FOLLOWING FORMAT:    31230000
*  FIRST 2 BYTES OF EACH CARD IS A BINARY SEQUENCE NUMBER.              31240000
*  AFTER THAT, THERE IS SOME HEADER INFO (THE FMT1 DSCB + OTHER JUNK),  31250000
*  THEN THE RECORDS IN THIS FORMAT:  LENGTH OF THIS "RECORD", 1 BYTE    31260000
*  INDICATOR WITH THE FOLLOWING BIT MEANINGS:                           31270000
*    X'80' -> 3 BYTE TTR FOLLOWS INDICATOR BYTE                         31280000
*    X'40' -> UNLOADED DS IS PDS                                        31290000
*    X'20' -> RECORD IS PART OF A MEMBER                                31300000
*    X'10' -> RECORD IS A NOTE LIST                                     31310000
*    X'08' -> RECORD IS A DIRECTORY ENTRY                               31320000
*    X'04' -> RECORD IS A DUMMY RECORD                                  31330000
*  THIS INFORMATION IS IN THE SOURCE TO MODULE IEHMVSRA                 31340000
         SPACE 1                                                        31350000
MV$NXTBL CLI   2(DPTR),X'E0'            DATA RECORD?                    31360000
         BE    MV$REC                   YES                             31370000
         CLI   2(DPTR),X'D0'            NOTE LIST RECORD?               31380000
         BE    MV$REC                   YES                             31390000
         CLI   2(DPTR),X'C8'            DIRECTORY RECORD?               31400000
         BE    MV$MEND                  YES                             31410000
         CLI   2(DPTR),X'C4'            DUMMY RECORD?                   31420000
         BE    MV$DUMMY                 YES                             31430000
         CLI   2(DPTR),X'0A'            END OF DS?                      31440000
         BNH   DIR$END                  YES                             31450000
*** LOGIC ERROR ***                                                     31460000
         PRTLN2                         PRINT (POSSIBLY) UNFINISHED LIN 31470000
         BA&XA R14,BOOM                 PRINT LOGIC ERROR MSG           31480000
         SPACE 2                                                        31490000
MV$DUMMY MVC   DWD(2),0(DPTR)           COPY LENGTH TO ALIGNED PLACE    31500000
         LH    DLEN,DWD                 LOAD LENGTH OF DUMMY RECORD     31510000
         LA    DPTR,12(DPTR,DLEN)       POINT TO NEXT                   31520000
         BA&XA R10,SEG$TEST             MAKE SURE STILL WITHIN RANGE    31530000
         B     MV$LOOP                  DO NEXT MEMBER NAME             31540000
         SPACE 2                                                        31550000
MV$REC   MVC   DWD(2),0(DPTR)           COPY LEN TO ALIGNED PLACE       31560000
         LH    DLEN,DWD                 GET BLOCK LENGTH                31570000
         LA    DPTR,6(DPTR,DLEN)        POINT PAST LEN,E0,TTR,DATA      31580000
         SPACE 2                                                        31590000
MV$SCAN  CL    DPTR,MV#ABUF2            WITHIN RANGE?                   31600000
         BL    MV$NXTBL                 YES - PROCESS NEXT BLOCK        31610000
         BA&XA R10,SEG$NEXT             SKIP 78 BYTES                   31620000
         B     MV$SCAN                  KEEP GOING                      31630000
         SPACE 2                                                        31640000
MV$MEND  LA    DPTR,6(,DPTR)            -> MEMBER NAME                  31650000
         BA&XA R10,SEG$TEST             WITHIN RANGE?                   31660000
         B     MV$LOOP                  GO PRINT NEXT NAME              31670000
         SPACE 2                                                        31680000
SEG$TEST CL    DPTR,MV#ABUF2            WITHIN RANGE?                   31690000
         BLR   R10                      YES - JUST RETURN               31700000
         SPACE 1                                                        31710000
SEG$NEXT MVC   MV#BUFF1,MV#BUFF2                                        31720000
         LTR   BLEN,BLEN                ANY LEFT?                       31730000
         BP    SEG$MVC                  YES - USE IT                    31740000
         BZ    *+8                      NO                              31750000
* RAN OFF END OF DATASET                                                31760000
***      EX    0,*                      *** LOGIC ERROR ***             31770000
         B     DIR$END                  FORGET THAT - IT'S THE END      31780000
         SPACE 1                                                        31790000
         ST    R10,R10SAVE              SAVE RETURN ADDR                31800000
         TAPIO TCCW#DAT,TM=SEG$EOF      READ ANOTHER BLOCK              31810000
         L     R10,R10SAVE              RESTORE RETURN ADDR             31820000
         LA    BPTR,TAPEBUFF            RESET PTR                       31830000
         L     BLEN,LASTSIZE       AND LENGTH LEFT            09/84 DBC 31840000
         SPACE 1                                                        31850000
SEG$MVC  SH    BLEN,=H'80'              DECR LENGTH LEFT / THIS BLOCK   31860000
         MVC   MV#BUFF2,2(BPTR)         MOVE A SEGMENT                  31870000
         LA    BPTR,80(,BPTR)           -> NEXT SEGMENT                 31880000
         SH    DPTR,=H'78'              BACK UP DATA PTR                31890000
         BR    R10                      RETURN TO CALLER                31900000
         SPACE 2                                                        31910000
SEG$EOF  BCTR  BLEN,0                   SET FLAG                        31920000
         L     R10,R10SAVE              RESTORE RETURN ADDR             31930000
         OI    TFLAG1,T1@MVEOF          REMEMBER HIT EOF                31940000
         BR    R10                      RETURN TO CALLER                31950000
         SPACE 3                                                        31960000
CONVDATE SR    R0,R0                    CLEAR FOR IC                    31970000
         IC    R0,0(,R1)                GET THE YEAR                    31980000
         CVD   R0,DWD                   CONVERT TO PACKED               31990000
         UNPK  0(3,R15),DWD+6(2)        AND THEN TO EBCDIC              32000000
         OI    2(R15),C'0'              FIX SIGN                        32010000
         MVC   0(2,R15),1(R15)          MOVE YEAR OVER                  32020000
         MVI   2(R15),C'.'              PUT IN THE DOT                  32030000
         MVC   DWD(2),1(R1)             MOVE DAY TO ALIGNED PLACE       32040000
         LH    R0,DWD                   GET THE YEAR                    32050000
         CVD   R0,DWD                   CONVERT TO PACKED               32060000
         UNPK  3(3,R15),DWD+6(2)        AND THEN TO EBCDIC              32070000
         OI    5(R15),C'0'              FIX THE SIGN                    32080000
         BR    R10                      RETURN TO CALLER                32090000
         EJECT                                                          32100000
IEBCOPY2 LA    R0,7                     ASSUME MEMBERS                  32110000
         TM    PARMFLAG,PF@NOMEM        PARM=NOMEMBERS?                 32120000
         BNO   *+8                      NOPE - OK                       32130000
         LA    R0,5                     NO MEMBERS                      32140000
         NEWPAGE  (R0),2                TEST LINE COUNTER               32150000
         SPACE 1                                                        32160000
         BA&XA R10,SET$FULL             SET UP HEADER                   32170000
         DC    Y(12)                    DSORG OFFSET                    32180000
         DC    Y(18)                    RECFM OFFSET                    32190000
         DC    Y(14)                    BLKSIZE OFFSET                  32200000
         DC    Y(16)                    LRECL OFFSET                    32210000
         SPACE 1                                                        32220000
         MVC   OUTBUFF2+47(7),IEBCOPY   WHO IT WAS UNLOADED BY          32230000
         SPACE 1                                                        32240000
         ICALL CONVUNIT,R10,R1=TAPEBUFF+24  FORMAT THE UNIT TYPE        32250000
         SPACE 2                                                        32260000
         BA&XA R10,FMU                  'FOLLOWING MEMBERS UNLOADED:'   32270000
         SPACE 2                                                        32280000
         TAPIO TCCW#DAT                 IGNORE THE NEXT BLOCK           32290000
         SPACE 2                                                        32300000
IBCPY$LP TAPIO TCCW#DAT                 READ A DIRECTORY BLOCK          32310000
         LH    BLEN,TAPEBUFF+4          GET RDW                         32320000
         SH    BLEN,=H'8'               -8 FOR BDR & RDW                32330000
         LA    BPTR,TAPEBUFF+8          INIT BLOCK PTR                  32340000
         SPACE 2                                                        32350000
DIR$NEXT LA    DPTR,22(,BPTR)           INIT DIR BLK PTR                32360000
         LH    DLEN,20(,BPTR)           GET LENGTH USED                 32370000
         SH    DLEN,=H'2'               LENGTH OF DATA                  32380000
         SPACE 2                                                        32390000
DIR$LP   CLC   0(8,DPTR),=8X'FF'        END OF DIRECTORY?               32400000
         BE    DIR$END                  YES - DONE                      32410000
         BA&XA R10,MOVE$MEM             MOVE NAME TO PRINT LINE         32420000
         LTR   DLEN,DLEN                ANYTHING LEFT?                  32430000
         BP    DIR$LP                   CONTINUE IF MORE LEFT           32440000
         LA    BPTR,256+12+8(,BPTR)     NEXT DIRECTORY BLOCK            32450000
         SH    BLEN,=Y(256+12+8)        DECR LENGTH LEFT                32460000
         BP    DIR$NEXT                 MORE IN THIS TAPE BLOCK         32470000
         B     IBCPY$LP                 GET NEXT BLOCK FROM TAPE        32480000
         SPACE 2                                                        32490000
DIR$END  LA    R0,OUTBUFF2+16           GET START PTR FOR COMPARE       32500000
         CR    R0,PPTR                  ANY ON THIS LINE?               32510000
         BE    STAR$TWO                 NO - DONE                       32520000
         PRTLN2                         PRINT PARTIAL LINE              32530000
         B     STAR$TWO                 CONTINUE PROCESSING             32540000
         SPACE 3                                                        32550000
MOVE$MEM MVC   0(8,PPTR),0(DPTR)        MOVE NAME TO PRINT LINE         32560000
         IC    R1,11(,DPTR)             GET USER DATA LENGTH            32570000
         N     R1,=F'31'                STRIP OFF JUNK                  32580000
         LA    R1,12(R1,R1)             GET LENGTH OF ENTRY             32590000
         AR    DPTR,R1                  ADVANCE DIR PTR                 32600000
         SR    DLEN,R1                  DECR LENGTH LEFT                32610000
         SPACE 1                                                        32620000
         LA    PPTR,10(,PPTR)           NEXT PRINT LINE POSITION        32630000
         CL    PPTR,OB2END              END OF PRINT LINE?              32640000
         BNHR  R10                      NO - RETURN                     32650000
         LR    PPTR,R10                 SAVE RETURN ADDR                32660000
         PRTLN2                         PRINT IT                        32670000
         LR    R10,PPTR                 RESTORE RETURN ADDR             32680000
         LA    PPTR,OUTBUFF2+16         RESET PTR                       32690000
         BR    R10                      RETURN TO CALLER                32700000
         EJECT ,                                              09/84 DBC 32710000
IEBUPDT2 LA    R0,7                                                     32720000
         TM    PARMFLAG,PF@NOMEM        PARM=NOMEM?                     32730000
         BNO   *+8                      NO                              32740000
         LA    R0,5                     YES                             32750000
         NEWPAGE  (R0),2                                                32760000
         BA&XA R10,SET$FULD             SET UP HEADER INFO              32770000
         DC    4Y(0)                    FILLER                          32780000
         MVC   OUTBUFF2+31(27),=C'IS AN IEBUPXXX INPUT STREAM' 6/84 DBC 32790000
         MVC   OUTBUFF2+37(8),IEBUPXXX CORRECT RELOADER NAME  06/84 DBC 32800000
         MVC   OUTBUFF2+58(55),=C':   A-ADD   C-CHANGE   R-REPLACE   > X32810000
               = SUBORDINATE NAME'                            11/88 SBG 32811000
         BA&XA R10,FMU                  'FOLLOWING MEMBERS UNLOADED:'   32820000
         SPACE 2                                                        32830000
UPDTE$L1 LA    BLEN,80                  BXLE INCR                       32840000
         LA    DPTR,TAPEBUFF            POINT TO START                  32850000
         LR    BPTR,DPTR                COPY START ADDR                 32860000
         A     BPTR,LASTSIZE       POINT PAST END OF BLOCK    09/84 DBC 32870000
         SR    BPTR,BLEN                BACK UP FOR BXLE                32880000
         SPACE 1                                                        32890000
UPDTE$L2 LR    R1,DPTR                  COPY CARD ADDR                  32900000
         BA&XA R10,TEST$UP              IS THIS ONE?                    32910000
         B     UPDTE$BX                 NO - SKIP                       32920000
         SPACE 1                                                        32930000
         MVC   0(1,PPTR),UPDT#TYP       MOVE A,R,C                      32940000
         MVI   1(PPTR),C'-'             SEPARATOR                       32950000
         CLI   UPDTXFLG,C'>'            SUBORDINATE MEMBER?   11/88 SBG 32960000
         BNE   UPDTE$LB                 NO. USE - AS SEPARATOR11/88 SBG 32970000
         MVI   1(PPTR),C'>'             SUBORD.  USE >.       11/88 SBG 32970100
UPDTE$LB DS    0H                       GO PUT IN MEMBER NAME 11/88 SBG 32970200
         MVC   2(8,PPTR),0(R1)          MEMBER NAME                     32970300
         LA    R1,1(,PPTR)                                              32970400
UPDTE$L3 CLI   0(R1),C','               END OF MEMBER NAME?             32980000
         BE    UPDTE$EM                 YES                             32990000
         CLI   0(R1),C' '               END OF MEMBER NAME?             33000000
         BE    UPDTE$EM                 YES                             33010000
         LA    R1,1(,R1)                INCR SCAN PTR                   33020000
         B     UPDTE$L3                 CONTINUE                        33030000
         SPACE 1                                                        33040000
UPDTE$EM MVC   0(7,R1),OUTBUFF+1        BLANK OUT GARBAGE               33050000
         LA    PPTR,11(,PPTR)           BUMP PRINT LINE PTR             33060000
         CL    PPTR,OB2END2             PAST END?                       33070000
         BNH   UPDTE$BX                 NO - CONTINUE                   33080000
         PRTLN2                         PRINT THE MEMBERS               33090000
         LA    PPTR,OUTBUFF2+16         RESET PTR                       33100000
         SPACE 1                                                        33110000
UPDTE$BX BXLE  DPTR,BLEN,UPDTE$L2       KEEP SCANNING                   33120000
         TAPIO TCCW#DAT,TM=UPDT$EOF     GET NEXT BLOCK                  33130000
         B     UPDTE$L1                 GO CHECK IT                     33140000
         SPACE 2                                                        33150000
UPDT$EOF OI    TFLAG1,T1@MVEOF          SET "HIT EOF"                   33160000
         B     DIR$END                  CLEAN UP                        33170000
         EJECT ,                                              07/85 DBC 33180000
CBT2     DS    0H   INSERT CODE TO DISPLAY MEMBERS FOR CBT973.11/88 SBG 33180100
         LA    R0,7                                           11/88 SBG 33180200
         TM    PARMFLAG,PF@NOMEM        PARM=NOMEM?           11/88 SBG 33180300
         BNO   *+8                      NO                    11/88 SBG 33180400
         LA    R0,5                     YES                   11/88 SBG 33180500
         NEWPAGE  (R0),2                                      11/88 SBG 33180600
         BA&XA R10,SET$FULD             SET UP HEADER INFO    11/88 SBG 33180700
         DC    4Y(0)                    FILLER                11/88 SBG 33180800
         MVC   OUTBUFF2+31(27),=C'IS A CBT973 COMPRESSED FILE' 1/88 SBG 33180900
         MVC   OUTBUFF2+58(55),=C':   A-ADD   C-CHANGE   R-REPLACE   > X33181000
               = SUBORDINATE NAME'                            11/88 SBG 33181100
         BA&XA R10,FMU         'FOLLOWING MEMBERS UNLOADED:'  11/88 SBG 33181200
         SPACE 2                                              11/88 SBG 33181300
*   SAVE REGISTERS USED FOR CBT973 DECOMPRESS ROUTINE.        11/88 SBG 33181400
         SPACE 2                                              11/88 SBG 33181500
*   **************          CBT973 DECOMPRESS ROUTINE.        11/88 SBG 33181600
         SPACE 1                                              11/88 SBG 33181700
CBT$D00  STM   R1,R5,CBSTOR01           REGS 1 THRU 5         11/88 SBG 33181800
         STM   R8,R10,CBSTOR08          REGS 8 THRU 10        11/88 SBG 33181900
         LA    R5,TAPEBUFF     BEGINNING OF BLOCK PROCESSING. 11/88 SBG 33182000
         ST    R1,CBSTOR1          USE R1 FOR WORK REG NOW    11/88 SBG 33183000
         LH    R1,0(,R5)           GET BLOCK SIZE FROM BDW    11/88 SBG 33184000
         ST    R1,CBTBLK1          STORE BLOCK SIZE FOR LATER 11/88 SBG 33185000
         AR    R1,R5               POINT PAST E-O-BLOCK       11/88 SBG 33186000
         ST    R1,CBTAPEND         REMEMBER E-O-BLOCK ADDRESS 11/88 SBG 33187000
         L     R1,CBSTOR1          RESTORE R1.                11/88 SBG 33188000
         LA    R5,4(,R5)                POINT TO RDW          11/88 SBG 33189000
*                              BEGINNING OF RECORD PROCESSING.11/88 SBG 33190000
CBT$D0B  LA    R8,14(,R5)          POINT TO BEGIN OF DATA     11/88 SBG 33200000
         ST    R1,CBSTOR1          USE R1 FOR WORK REG NOW    11/88 SBG 33210000
         ST    R6,CBSTOR06         USE R6 FOR WORK REG NOW    11/88 SBG 33220000
         LH    R1,0(,R5)           STORE RECORD SIZE FROM RDW 07/90 SBG 33241001
*   THIS IS NEVER BIGGER THAN 94 -- LET ALONE 255 ||          11/88 SBG 33250000
         C     R1,=F'94'           LRECL=94 MAX FOR CBT973    07/90 SBG 33251001
         BH    CBT$EOF             BREAK OUT OF LOOP IF BIGGER07/90 SBG 33252001
         S     R1,=F'12'           -12 JUST TO GO PAST DATA   11/88 SBG 33260000
*                                   AND LAST 2 HEADER BITS    11/88 SBG 33270000
         LA    R5,2(,R5)           HALFWAY TO HEADER BITS     11/88 SBG 33280000
         MVI   CBTCLEAR,X'40'      MOVE IN BLANK TO ...       11/88 SBG 33290000
         MVC   CBTCARD,CBTCLEAR       CLEAR 80-BYTE WORKAREA. 11/88 SBG 33300000
         LA    R2,CBTCARD          ADDRESS OF 80-BYTE CARD    11/88 SBG 33310000
         LA    R6,5                5 LOOPS TO EXPAND CARD     11/88 SBG 33320000
CBT$D0C  EQU   *                   EXPAND 16 BYTES AT A TIME  11/88 SBG 33330000
         LA    R4,16               LOOP COUNTER               11/88 SBG 33340000
         LA    R5,2(,R5)           BUMP HEADER BIT POINTER    11/88 SBG 33350000
         ICM   R3,12,0(R5)         2 BYTES MASK BITS TO SLIDE 11/88 SBG 33360000
CBT$D0D  EQU   *                   INNER LOOP TO DO 16 BYTES  11/88 SBG 33370000
         ST    R2,CBSTOR2          TEMP SAVE R2               02/93 RT
         SLR   R2,R2               CLEAR TARGET REGISTER      11/88 SBG 33380000
         SLDL  R2,1                SLIDE OVER ONE MASK BIT    11/88 SBG 33390000
         LTR   R2,R2               IS BIT 0 OR 1 ?            11/88 SBG 33400000
         L     R2,CBSTOR2          RESTORE R2                 02/93 RT
         BZ    CBT$D0F             IF 0, NEEDS BLANK TO FILL  11/88 SBG 33410000
         MVC   0(1,R2),0(R8)       MOVE THE NON-BLANK         11/88 SBG 33420000
         LA    R8,1(,R8)           BUMP DATA PTR TO NEXT BYTE 11/88 SBG 33430000
CBT$D0E  EQU   *                   RESTORE THE CARD-IMAGE     11/88 SBG 33440000
         LA    R2,1(,R2)           POINT TO NEXT CARD BYTE    11/88 SBG 33450000
         BCT   R4,CBT$D0D          RESTORE 16-BYTE GROUP      11/88 SBG 33460000
         BCT   R6,CBT$D0C          DO THAT 5 TIMES            11/88 SBG 33470000
         AR    R5,R1               BUMP POINTER TO NEXT RDW.  11/88 SBG 33480000
         LM    R1,R4,CBSTOR01      REGS 1 THRU 4. 5 HAS PTR   11/88 SBG 33490000
         LM    R8,R10,CBSTOR08     REGS 8 THRU 10             11/88 SBG 33500000
*        L     R6,CBSTOR06         RESTORE R6. USED FOR PPTR. 11/89 SBG 33510001
         B     CBT$L2              ONE CARD IS RESTORED, FOR  11/88 SBG 33520000
*                                   PURPOSES OF EXAMINATION.  11/88 SBG 33530000
CBT$D0F  EQU   *                   RTN TO INSERT A BLANK      11/88 SBG 33540000
         MVI   0(R2),X'40'         MOVE BLANK IN TO DECOMPRESS11/88 SBG 33550000
         B     CBT$D0E             CONTINUE RESTORING CARD.   11/88 SBG 33550100
         SPACE 1                                              11/88 SBG 33550200
*   **************   END OF CBT973 DECOMPRESS ROUTINE.        11/88 SBG 33550300
         SPACE 2                                              11/88 SBG 33550400
*  AT THIS POINT, A CARD-IMAGE OF DATA IS RESTORED INTO       11/88 SBG 33550500
*    THE FIELD, CBTCARD.  WE THEN USE REGULAR IEBUPDTE        11/88 SBG 33550600
*    PROCESSING TO LOOK FOR MEMBER NAMES.                     11/88 SBG 33550700
         SPACE 2                                              11/88 SBG 33550800
CBT$L2   L     PPTR,CBSTOR06       RESTORE REGISTER PPTR      11/88 SBG 33550900
         LA    R1,CBTCARD          LOAD CARD ADDRESS          11/88 SBG 33551000
         ST    R5,CBSTOR05         SAVE RECD PTR              11/88 SBG 33551100
         BA&XA R10,TEST$UP         IS THIS A MEMBER INDICATOR 11/88 SBG 33551200
         B     CBT$BX              NO - SKIP TO THE NEXT CARD 11/88 SBG 33551300
         L     R5,CBSTOR05         RESTORE RECD PTR           11/88 SBG 33551400
         SPACE 1                                              11/88 SBG 33551500
         MVC   0(1,PPTR),UPDT#TYP    MOVE A,R,C               11/88 SBG 33551600
         MVI   1(PPTR),C'-'             SEPARATOR             11/88 SBG 33551700
         CLI   UPDTXFLG,C'>'            SUBORDINATE MEMBER?   11/88 SBG 33551800
         BNE   CBT$LB                   NO. USE - AS SEPARATOR11/88 SBG 33551900
         MVI   1(PPTR),C'>'             SUBORD.  USE >.       11/88 SBG 33552000
CBT$LB   DS    0H                       GO PUT IN MEMBER NAME 11/88 SBG 33552100
         MVC   2(8,PPTR),0(R1)          MEMBER NAME           11/88 SBG 33552200
         LA    R1,1(,PPTR)                                    11/88 SBG 33552300
CBT$L3   CLI   0(R1),C','               END OF MEMBER NAME?   11/88 SBG 33552400
         BE    CBT$EM                   YES                   11/88 SBG 33552500
         CLI   0(R1),C' '               END OF MEMBER NAME?   11/88 SBG 33552600
         BE    CBT$EM                   YES                   11/88 SBG 33552700
         LA    R1,1(,R1)                INCR SCAN PTR         11/88 SBG 33552800
         B     CBT$L3                   CONTINUE              11/88 SBG 33552900
         SPACE 1                                              11/88 SBG 33553000
CBT$EM   MVC   0(7,R1),OUTBUFF+1        BLANK OUT GARBAGE     11/88 SBG 33553100
         LA    PPTR,11(,PPTR)           BUMP PRINT LINE PTR   11/88 SBG 33553200
         CL    PPTR,OB2END2             PAST END?             11/88 SBG 33553300
         BNH   CBT$BX                   NO - CONTINUE         11/88 SBG 33553400
         ST    R5,CBSTOR05              SAVE RECD PTR         11/88 SBG 33553500
         PRTLN2                         PRINT THE MEMBERS     11/88 SBG 33553700
         LA    PPTR,OUTBUFF2+16         RESET PTR             11/88 SBG 33553900
         SPACE 1                                              11/88 SBG 33554000
CBT$BX   L     R5,CBSTOR05              RESTORE RECD PTR      11/88 SBG 33554100
         C     R5,CBTAPEND              AT END OF BLOCK YET?  11/88 SBG 33554200
         BL    CBT$D0B                  NO. GET NEXT RECORD   11/88 SBG 33555000
*    TRY TO RESTORE REGISTERS THAT MIGHT BE USED ELSEWHERE.   11/88 SBG 33555100
         TAPIO TCCW#DAT,TM=CBT$EOF      GET NEXT BLOCK        11/88 SBG 33555200
         B     CBT$D00                  GO CHECK IT           11/88 SBG 33555300
         SPACE 2                                              11/88 SBG 33555400
CBT$EOF  OI    TFLAG1,T1@MVEOF          SET "HIT EOF"         11/88 SBG 33555500
         B     DIR$END                  CLEAN UP              11/88 SBG 33555600
         EJECT ,                                              11/88 SBG 33555700
************************************************************* 07/85 DBC 33555800
*                                                           * 07/85 DBC 33555900
*        THE CURRENT TAPE FILE CONTAINS SMPPTFIN SYSMODS.   * 07/85 DBC 33556000
*        SCAN THE FILE TO GET THE NAMES OF THE SYSMODS      * 07/85 DBC 33556100
*        CONTAINED THEREIN, AND DISPLAY THEM.               * 07/85 DBC 33556200
*                                                           * 07/85 DBC 33556300
************************************************************* 07/85 DBC 33556400
         SPACE 1                                              07/85 DBC 33556500
PTFS2    LA    R0,7                REQUIRED HEADER SPACE      07/85 DBC 33556600
         TM    PARMFLAG,PF@NOMEM   SCAN FOR "MEMBERS"?        07/85 DBC 33556700
         BNO   *+8                 YES, PROCEED               07/85 DBC 33556800
         LA    R0,5                NO, DON'T NEED SO MUCH     07/85 DBC 33556900
         NEWPAGE (R0),2            WIDOW PREVENTION           07/85 DBC 33557000
         SPACE 1                                              07/85 DBC 33557100
         BA&XA R10,SET$FULD        MESSAGE SETUP              07/85 DBC 33557200
         DC    4Y(0)                    FILLER                07/85 DBC 33557300
         MVC   OUTBUFF2+31(35),=C'IS AN SMPPTFIN SYSMODS INPUT STREAM'  33557400
         MVC   OUTBUFF2+66(43),=C':   A-APAR   F-FUNCTION   P-PTF   U-U*33557500
               SERMOD'             DEFINITIONS                07/85 DBC 33557600
         BA&XA R10,FMU             DISPLAY THE MESSAGE        07/85 DBC 33557700
         SPACE 3                                              07/85 DBC 33557800
PTF$L1   LA    BLEN,80             BXLE INCRIMENT             07/85 DBC 33557900
         LA    DPTR,TAPEBUFF       BXLE SCANNER               07/85 DBC 33558000
         LR    BPTR,DPTR           SET -                      07/85 DBC 33558100
         AL    BPTR,LASTSIZE        BXLE-                     07/85 DBC 33558200
         SLR   BPTR,BLEN             LIMIT                    07/85 DBC 33558300
         SPACE 1                                              07/85 DBC 33558400
PTF$L2   LR    R1,DPTR             MESSAGE SINK SCANNER       07/85 DBC 33558500
         BA&XA R10,TEST$PT         DOES THE CURRENT CARD NAME 07/85 DBC 33558600
*                                  THE START OF A NEW SYSMOD? 07/85 DBC 33558700
         B     PTF$BX              +0 NO, SKIP                07/85 DBC 33558800
PTF$L2Z  DS    0H                  NEW +4
         CLI   PTF#TYPE,C'X'       SPECIAL ++ TYPE DESIGNATION?
         BE    PTF$BX              PRINTING IS TO BE SKIPPED.
         MVC   0(1,PPTR),PTF#TYPE  +4 YES, GET SYSMOD TYPE    07/85 DBC 33558900
         MVI   1(PPTR),C'-'        FRAME                      07/85 DBC 33559000
         MVC   2(7,PPTR),0(R1)     SYSMOD NAME (ALWAYS SEVEN  07/85 DBC 33559100
*                                  CHARACTERS LONG)           07/85 DBC 33559200
         LA    PPTR,10(,PPTR)      ADVANCE SINK SCANNERR      07/85 DBC 33559300
         CL    PPTR,OB2END2        IS MSG LINE FULL?          07/85 DBC 33559400
         BNH   PTF$BX              NOT YET; GO  LOOP          07/85 DBC 33560000
         PRTLN2 ,                  YES, PRINT THE LINE        07/85 DBC 33570000
         LA    PPTR,OUTBUFF2+16    RESET SINK SCANNER         07/85 DBC 33580000
PTF$BX   DS    0H
         BXLE  DPTR,BLEN,PTF$L2    CONTINUE SCANNING THE BLK  07/85 DBC 33590000
         SPACE 1                                              07/85 DBC 33600000
         TAPIO TCCW#DAT,TM=PTF$EOF READ NEXT BLOCK            07/85 DBC 33610000
         B     PTF$L1              LOOP TO SCAN IT            07/85 DBC 33620000
         SPACE 1                                              07/85 DBC 33630000
PTF$EOF  OI    TFLAG1,T1@MVEOF     EOF; REMEMBER              07/85 DBC 33640000
         B     DIR$END             GO DISPLAY FILE            07/85 DBC 33650000
*                                  DESCRIPTION MESSAGES       07/85 DBC 33660000
         EJECT ,                                              07/85 DBC 33670000
IEBISAM2 BA&XA R10,SET$FULL             SET UP HEADER                   33680000
         DC    Y(4+26)                  DSORG OFFSET                    33690000
         DC    Y(4+36)                  RECFM OFFSET                    33700000
         DC    Y(4+62)                  BLKSIZE OFFSET                  33710000
         DC    Y(4+2+82)                LRECL OFFSET                    33720000
         SPACE 1                                                        33730000
         MVC   OUTBUFF2+47(7),IEBISAM   WHO IT WAS UNLOADED BY          33740000
         SPACE 1                                                        33750000
         ICALL CONVUNIT,R10,R1=TAPEBUFF+4+17-3  CONVERT UNIT TYPE       33760000
         SPACE 1                                                        33770000
         PRTLN2                         PRINT SECOND INFO LINE          33780000
         B     STAR$TWO                                                 33790000
         EJECT ,                                              09/84 DBC 33800000
FDR2     NEWPAGE 9,2                    CHECK FOR ROOM ON PAGE          33810000
         BA&XA R10,SET$FULD             SET UP HEADER INFO              33820000
         DC    4Y(0)                    FILLER                          33830000
         MVC   OUTBUFF2+31(14),=C'WAS DUMPED BY ' SET UP UNLOADER       33840000
         MVC   OUTBUFF2+45(6),UNLOADER            MESSAGE               33850000
         CLC   FDRSAVE,=C'DUMMYDSF'     NORMAL DUMP?                    33860000
         BE    *+10                     YES                             33870000
*                                       NO  - WARN USER                 33880000
         MVC   OUTBUFF2+54(22),=C' (ABSOLUTE TRACK DUMP)'               33890000
         PRTLN2                         PRINT THE HEADER                33900000
         TM    PARMFLAG,PF@NOMEM        DETAILS WANTED?                 33910000
         BO    FDR2NODT                 NO                              33920000
*                                       YES - CHECK DUMP TYPE           33930000
         CLC   FDRSAVE,=C'DUMMYDSF'     WAS IT A DSF'ABLE DUMP?         33940000
         BNE   FDR2ABS                  NO                              33950000
*                                       YES - ISSUE MESSAGE             33960000
         MVI   OUTBUFF2,C'0'                                            33970000
         MVC   OUTBUFF2+16(27),=C'FOLLOWING DATA SETS DUMPED:'          33980000
         PRTLN2                                                         33990000
         ST    R15,CBSAVE15             SAVE REG 15           11/88 SBG 33990101
         L     R15,=A(DSFHDING)         ADDRESSABILITY TO VAR 11/88 SBG 33991001
         MVC   OUTBUFF2(L'DSFHDING),0(R15)     COLUMN HEADING 11/88 SBG 34000001
         L     R15,CBSAVE15             RESTORE REG 15        11/88 SBG 34001001
         PRTLN2                                                         34010000
FDR2LOOP CLC   =C'DUMMYDSF',TAPEBUFF+12 IS THIS A DSCB BLOCK?           34020000
         BNE   FDR2VOL                  NO  - TRY FOR VOLSER            34030000
*                                       YES                             34040000
         LA    R5,TAPEBUFF+40           POINT TO FIRST DSCB             34050000
         LH    R6,TAPEBUFF+30           GET DSCB COUNT THIS BLOCK       34060000
FDR2DSCB LTR   R6,R6                    MORE DSCB'S TO GO?              34070000
         BNP   FDR2NBLK                 NO  - GET NEXT BLOCK            34080000
*                                       YES                             34090000
         CLI   44(R5),C'1'              FORMAT 1 DSCB?                  34100000
         BNE   FDR2NDSC                 NO                              34110000
*                                       YES                             34120000
         CLC   LCTR2,=F'1'              ROOM ON PAGE FOR NEXT LINE?     34130000
         BH    FDR2DSCC                 NO                              34140000
         NEWPAGE ,2                     FORCE EJECT                     34150000
         ST    R15,CBSAVE15             SAVE REG 15           11/88 SBG 34150101
         L     R15,=A(DSFHDING)         ADDRESSABILITY TO VAR 11/88 SBG 34151001
         MVC   OUTBUFF2(L'DSFHDING),0(R15)     COLUMN HEADING 11/88 SBG 34152001
         L     R15,CBSAVE15             RESTORE REG 15        11/88 SBG 34153001
         PRTLN2                         PRINT THEM                      34170000
FDR2DSCC MVC   OUTBUFF2+DSFDSN(44),0(R5)   MOVE IN DSNAME               34180000
         LH    R14,98(,R5)                 GET LAST TRACK USED          34190000
         LA    R14,1(,R14)                 MAKE 1 RELATIVE              34200000
         STH   R14,98(,R5)                 SAVE IT BACK                 34210000
         ICALL CONVHALF,R14,R1=98(,R5),R15=OUTBUFF2+DSFTRK              34220000
         ICALL CNVRECFX,R14,R1=84(,R5),R15=OUTBUFF2+DSFRECFM            34230000
         ICALL CONVHALF,R14,R1=88(,R5),R15=OUTBUFF2+DSFLRECL-1          34240000
         ICALL CONVHALF,R14,R1=86(,R5),R15=OUTBUFF2+DSFBLKSZ+1          34250000
         ICALL CNVDSORX,R14,R1=82(,R5),R15=OUTBUFF2+DSFDSORG            34260000
         ICALL CONVDATE,R10,R1=53(,R5),R15=OUTBUFF2+DSFCDATE            34270000
         ICALL CONVDATE,R10,R1=56(,R5),R15=OUTBUFF2+DSFEDATE            34280000
         TM    93(R5),X'40'            RACF DEFINED?                    34290000
         BZ    *+10                    NO                               34300000
         MVC   OUTBUFF2+DSFSECUR+5(4),=C'RACF'   YES                    34310000
         TM    93(R5),X'10'            PASSWORD?                        34320000
         BZ    FDR2PRT                 NO                               34330000
         MVC   OUTBUFF2+DSFSECUR(4),=C'WPWD'  YES - ASSUME WPWD         34340000
         TM    93(R5),X'04'            READ PSWD REQUIRED?              34350000
         BO    *+8                     NO                               34360000
         MVI   OUTBUFF2+DSFSECUR,C'R'  YES                              34370000
         TM    93(R5),X'50'            BOTH RACF/PSWD?                  34380000
         BNO   FDR2PRT                 NO                               34390000
         MVI   OUTBUFF2+DSFSECUR+4,C'/'  YES                            34400000
FDR2PRT  PRTLN2                        PRINT THE LINE                   34410000
FDR2NDSC LA    R5,148(,R5)             BUMP PAST DSCB/COUNT FIELD       34420000
         S     R6,=F'1'                                                 34430000
         B     FDR2DSCB                                                 34440000
FDR2NBLK TAPIO TCCW#DAT,TM=FDR2EOF     GET NEXT BLOCK                   34450000
         B     FDR2LOOP                                                 34460000
FDR2EOF  MVI   OUTBUFF2,C'0'                                            34470000
         OI    TFLAG1,T1@MVEOF         REMEMBER WE HIT EOF              34480000
       MVC  OUTBUFF2+12(63),=C'0***LISTING TERMINATED. FDR FILE NOT COM$34490000
               PLETE ON THIS VOLUME***'                                 34500000
         PRTLN2                                                         34510000
FDR2FAKE MVC   TAPEBUFF+4(4),=X'FFFFFFFF'   FAKE OUT FDR2VOL            34520000
FDR2VOL  MVI   OUTBUFF2,C'0'                                            34530000
         MVC   OUTBUFF2+8(20),=C'DUMPED FROM A UUUUUU'                  34540000
         MVC   FDRDVEND(1),DASDRSAV+4   SET UP FENCE FOR DEV TYPE       34550000
         LA    R1,FDRDVTAB              POINT TO FDR DEV TYPE TABLE     34560000
FDR2VOLA CLC   DASDRSAV+4(1),0(R1)      MATCH?                          34570000
         BE    FDR2VOLB                 YES                             34580000
         LA    R1,7(,R1)                NO  - BUMP TO NEXT ENTRY        34590000
         B     FDR2VOLA                                                 34600000
FDR2VOLB MVC   OUTBUFF2+22(6),1(R1)     PUT DEV TYPE IN MESSAGE         34610000
         MVC   OUTBUFF2+28(16),=C'         VOLSER='                     34620000
         CLC   UNLOADER(6),FDRDSF       WAS THIS DSF?                   34630000
         BE    FDR2VOLM                 YES - NO VOLSER AVAILABLE       34640000
         CLC   =XL4'00',TAPEBUFF+4      DOES DUMP INCLUDE CYL/TRK 0/0?  34650000
         BNE   FDR2VOLM                 NO  - NO VOLSER AVAILABLE       34660000
*                                       YES                             34670000
         LA    R1,TAPEBUFF+20           POINT TO FIRST COUNT FIELD      34680000
         LA    R0,TAPEBUFF              POINT TO 2ND TRACK IN BLOCK     34690000
         AH    R0,TAPEBUFF+2                                            34700000
FDR2VOLC CLC   0(5,R1),=X'0000000003'   POINTING TO REC 3 ON TRK 0?     34710000
         BE    FDR2VOLG                 YES                             34720000
         BH    FDR2VOLM                 NO - AND NO VOLSER AVAIL        34730000
         SR    R15,R15                                                  34740000
         IC    R15,5(,R1)               GET KEY LENGTH                  34750000
         AH    R15,6(,R1)               ADD IN DATA LENGTH              34760000
         LA    R1,8(R15,R1)             POINT TO NEXT RECORD            34770000
         CR    R1,R0                    PAST TRACK 0?                   34780000
         BNL   FDR2VOLM                 YES                             34790000
         B     FDR2VOLC                 NO  - KEEP GOING                34800000
FDR2VOLG CLC   8(8,R1),=C'VOL1VOL1'     GOT VOL LABEL?                  34810000
         BNE   FDR2VOLM                 NO  - STRANGE                   34820000
         MVC   OUTBUFF2+44(6),16(R1)    YES - MOVE IT INTO MSG          34830000
         B     FDR2VOLP                                                 34840000
FDR2VOLM MVC   OUTBUFF2+43(14),=C' NOT AVAILABLE'                       34850000
FDR2VOLP PRTLN2                                                         34860000
         B     STAR$TWO                                                 34870000
FDR2ABS  MVI   OUTBUFF2,C'0'                                            34880000
         MVC   OUTBUFF2+16(63),=C'ABSOLUTE TRACK DETAILS NOT AVAILABLE $34890000
               IN THIS VERSION OF TAPEMAP'                              34900000
         PRTLN2                                                         34910000
         B     FDR2FAKE                                                 34920000
FDR2NODT CLC   =C'DUMMYDSF',TAPEBUFF+12  DSCB BLOCK?                    34930000
         BNE   FDR2VOL                   NO  - CHECK FOR VOLSER         34940000
         TAPIO TCCW#DAT,TM=FDR2EOF       YES - CHECK NEXT BLOCK         34950000
         B     FDR2NODT                                                 34960000
         SPACE 3                                                        34970000
SLK2     NEWPAGE 9,2                    CHECK FOR ROOM ON PAGE          34980000
         BA&XA R10,SET$FULD             SET UP HEADER INFO              34990000
         DC    4Y(0)                    FILLER                          35000000
         MVC   OUTBUFF2+31(16),=C'WAS UNLOADED BY ' SET UP UNLOADER     35010000
         MVC   OUTBUFF2+47(8),UNLOADER            MESSAGE               35020000
         PRTLN2                         PRINT THE HEADER                35030000
         MVC   OUTBUFF2+8(30),=C'SLICK LIBRARY BACKUP, CYCLE = '        35040000
         MVC   OUTBUFF2+38(1),DASDRSAV  PUT IN THE CYCLE NUMBER         35050000
         MVC   OUTBUFF2+39(12),=C', MAXSIZE = '                         35060000
         ICALL CONVHALF,R14,R1=DASDRSAV+2,R15=OUTBUFF2+41               35070000
         PRTLN2                         PRINT FIRST INFO LINE           35080000
         TM    PARMFLAG,PF@NOMEM        DETAILS WANTED?                 35090000
         BO    STAR$TWO                 NO                              35100000
*                                       YES                             35110000
         MVI   OUTBUFF2,C'0'                                            35120000
         MVC   OUTBUFF2+16(27),=C'FOLLOWING MEMBERS UNLOADED:'          35130000
         PRTLN2                                                         35140000
         L     R15,=A(SLKHDING)         ADDRESSABILITY TO VAR 11/88 SBG 35141000
         MVC   OUTBUFF2(L'SLKHDING),0(R15)     COLUMN HEADING 11/88 SBG 35150000
         PRTLN2                                                         35160000
SLK2LOOP OC    TAPEBUFF(2),TAPEBUFF     IS THIS A HEADER BLOCK?         35170000
         BNZ   SLK2NBLK                 NO  - TRY NEXT BLOCK            35180000
*                                       YES                             35190000
         CLC   LCTR2,=F'1'              ROOM ON PAGE FOR NEXT LINE?     35200000
         BH    SLK2MGO                  NO                              35210000
         NEWPAGE ,2                     FORCE EJECT                     35220000
         ST    R15,CBSAVE15             SAVE REG 15           11/88 SBG 35220101
         L     R15,=A(SLKHDING)         ADDRESSABILITY TO VAR 11/88 SBG 35220200
         MVC   OUTBUFF2(L'SLKHDING),0(R15)     COLUMN HEADING 11/88 SBG 35220300
         L     R15,CBSAVE15             RESTORE REG 15        11/88 SBG 35220401
         PRTLN2                         PRINT THEM                      35240000
SLK2MGO  MVC   OUTBUFF2+SLKMEM(10),TAPEBUFF+8   MEMBER NAME             35250000
         MVI   OUTBUFF2+SLKSTAT-1,C'.'          .                       35260000
         MVC   OUTBUFF2+SLKSTAT(4),TAPEBUFF+18  STATUS                  35270000
         ICALL CONVP2,R14,R1=TAPEBUFF+32,       REVISION               X35280000
               R15=OUTBUFF2+SLKREV                                      35290000
         MVC   OUTBUFF2+SLKINFO(8),TAPEBUFF+34  INFO                    35300000
         MVC   OUTBUFF2+SLKTYPE(8),TAPEBUFF+42  TYPE                    35310000
         ICALL CONVP4,R14,R1=TAPEBUFF+58,       ACTST                  X35320000
               R15=OUTBUFF2+SLKACTST                                    35330000
         ICALL CONVP4,R14,R1=TAPEBUFF+62,       DELST                  X35340000
               R15=OUTBUFF2+SLKDELST                                    35350000
         ICALL CONVP3,R14,R1=TAPEBUFF+66,       BLOCKS                 X35360000
               R15=OUTBUFF2+SLKBLKS                                     35370000
         ICALL CONVMDY,R14,R1=TAPEBUFF+74,      C-DATE                 X35380000
               R15=OUTBUFF2+SLKCREAT                                    35390000
         ICALL CONVMDY,R14,R1=TAPEBUFF+122,     U-DATE                 X35400000
               R15=OUTBUFF2+SLKUPD                                      35410000
         PRTLN2                                                         35420000
SLK2NBLK TAPIO TCCW#DAT,TM=SLK2EOF         GET NEXT BLOCK               35430000
         B     SLK2LOOP                                                 35440000
SLK2EOF  OI    TFLAG1,T1@MVEOF             SET "HIT EOF"                35450000
         B     STAR$TWO                                                 35460000
         EJECT ,                                              09/84 DBC 35470000
MAP2     NEWPAGE 9,2                    CHECK FOR ROOM ON PAGE          35480000
         BA&XA R10,SET$FULD             SET UP HEADER INFO              35490000
         DC    4Y(0)                    FILLER                          35500000
         MVC   OUTBUFF2+31(49),=C'CONTAINS THE FOLLOWING USER-SUPPLIED $35510000
               INFORMATION:'                                            35520000
         PRTLN2                         PRINT THE HEADER INFO           35530000
** USE R5 TO POINT TO INFO TO BE PRINTED,                               35540000
** AND R6 TO CONTROL DEBLOCKING LOOP                                    35550000
         LA    R5,TAPEBUFF+80           SKIP FILE HEADER INFO           35560000
         L     R6,LASTSIZE                                    09/84 DBC 35570000
         SH    R6,=H'80'                                                35580000
         MVI   OUTBUFF2,C'0'            DOUBLE SPACE FIRST LINE         35590000
         B     MAPLOOPX                 BRANCH TO MIDDLE OF LOOP        35600000
MAPLOOP  LA    R5,80(,R5)               GET TO NEXT INFO LOGICAL RECORD 35610000
         SH    R6,=H'80'                CHECK FOR END OF BLOCK          35620000
MAPLOOPX BNP   MAPNEXT                  GET NEXT BLOCK IF NECESSARY     35630000
MAPLOOPY MVC   OUTBUFF2+16(72),0(R5)    PRINT 72 CHARS OF INFO          35640000
         PRTLN2                                                         35650000
         B     MAPLOOP                  GET NEXT LOGICAL RECORD         35660000
MAPNEXT  TAPIO TCCW#DAT,TM=MAPEOF       READ NEXT BLOCK                 35670000
         L     R1,LASTSIZE         VERIFY BLOCK MULT OF 80 BY 09/84 DBC 35680000
         SR    R0,R0                                                    35690000
         LA    R2,80                                                    35700000
         DR    R0,R2                                                    35710000
         LTR   R0,R0                                                    35720000
         BNZ   MAPERR                   NO  - ERROR                     35730000
         LA    R5,TAPEBUFF              RESET LOOP REGS                 35740000
         L     R6,LASTSIZE                                    09/84 DBC 35750000
         B     MAPLOOPY                                                 35760000
MAPERR   MVC   OUTBUFF2+12(53),=C'*** LISTING TERMINATED, INVALID BLOCK$35770000
                ENCOUNTERED ***'                                        35780000
         PRTLN2                                                         35790000
         B     STAR$TWO                                                 35800000
MAPEOF   OI    TFLAG1,T1@MVEOF          REMEMBER EOF FOUND              35810000
****     B     STAR$TWO                                                 35820000
         EJECT ,                                              09/84 DBC 35830000
STAR$TWO MVC   OUTBUFF2(2),=C'0*'                                       35840000
         MVC   OUTBUFF2+2(L'OUTBUFF2-2),OUTBUFF2+1                      35850000
         PRTLN2                                                         35860000
         TM    TFLAG1,T1@MVEOF          ALREADY HIT EOF?                35870000
         BO    CHKMVEOF                 YES - SEE WHERE TO GO           35880000
         TM    TFLAG1,T1@SL             SL TAPE?                        35890000
         BO    FIND$EOF                 YES - CONTINUE PROCESSING       35900000
         B     NL$LOOP                  SCAN THRU REST OF FILE          35910000
         SPACE 1                                                        35920000
CHKMVEOF NI    TFLAG1,255-T1@MVEOF      RESET FLAG                      35930000
         TM    TFLAG1,T1@SL             SL TAPE?                        35940000
         BO    SL$SAVE                  YES - GO READ THE EOF LABELS    35950000
         B     NL$EOF                   NO                              35960000
         SPACE 3                                                        35970000
FMU      PRTLN2                         PRINT INFO LINE                 35980000
FMU$     TM    PARMFLAG,PF@NOMEM        WANT MEMBER LISTING?            35990000
         BO    STAR$TWO                 NO - DONE                       36000000
         MVI   OUTBUFF2,C'0'            SET CC                          36010000
         MVC   OUTBUFF2+16(27),=C'FOLLOWING MEMBERS UNLOADED:'          36020000
         PRTLN2                         PRINT HEADER                    36030000
         MVI   OUTBUFF2,C'0'            DOUBLE SPACE FIRST MEMBER LINE  36040000
         LA    PPTR,OUTBUFF2+16         INIT PRINT LINE PTR             36050000
         BR    R10                      RETURN TO CALLER                36060000
         SPACE 3                                                        36070000
SET$FULL MVC   OUTBUFF2+54(17),=C'...  WAS:  DSORG='                    36080000
         ICALL CNVDSORG,R14,R15=OUTBUFF2+71  CONVERT DSORG              36090000
         SPACE 1                                                        36100000
         MVC   OUTBUFF2+76(6),=C'LRECL='                                36110000
         LA    R1,TAPEBUFF                                              36120000
         AH    R1,6(,R10)               GET LRECL ADDR                  36130000
         ICALL CONVHALF,R14,R15=OUTBUFF2+82  CONVERT LRECL              36140000
         BA&XA R14,SLIDE                MOVE IT LEFT                    36150000
         SPACE 1                                                        36160000
         MVC   OUTBUFF2+90(6),=C'RECFM='                                36170000
         ICALL CNVRECFM,R14,R15=OUTBUFF2+96  CONVERT RECFM              36180000
         SPACE 1                                                        36190000
         MVC   OUTBUFF2+104(8),=C'BLKSIZE='                             36200000
         LA    R1,TAPEBUFF                                              36210000
         AH    R1,4(,R10)               GET BLKSIZE ADDR                36220000
         ICALL CONVHALF,R14,R15=OUTBUFF2+112 CONVERT BLKSIZE            36230000
         BA&XA R14,SLIDE                MOVE IT LEFT                    36240000
         SPACE 2                                                        36250000
SET$FULD MVI   OUTBUFF2,C'-'            SET CC                          36260000
         MVC   OUTBUFF2+1(17),FL1ID     MOVE IN (TAPE) DSN              36270000
         MVC   OUTBUFF2+19(5),=C'(FILE'                                 36280000
         MVC   OUTBUFF2+25(4),FL1FILSQ  MOVE IN FILE SEQ#               36290000
         MVC   OUTBUFF2+29(17),=C') WAS UNLOADED BY'                    36300000
         B     8(,R10)                  RETURN TO CALLER                36310000
         SPACE 3                                                        36320000
BOOM     STM   R0,R15,BOOMREGS          SAVE ALL REGS                   36330000
         MVC   OUTBUFF2(50),=C'0*** TAPEMAP LOGIC ERROR - CONTACT TEC S$36340000
               UPPORT ***'                                          WBF 36350000
         PRTLN2                         PRINT ERROR MSG                 36360000
         TM    PARMFLAG,PF@TEST         ABEND?                          36370000
         BNO   STAR$TWO                 NO - IGNORE REST OF FILE        36380000
         SPACE 1                                                        36390000
         CLOSE MF=(E,OPENMFL)           CLOSE SYSPRINT                  36400000
         IFP2  N,BOOM2                  SKIP IF NOT OPEN                36410000
         CLOSE MF=(E,OPENMFL2)          CLOSE SYSPRNT2                  36420000
         SPACE 1                                                        36430000
BOOM2    LA    R14,BOOMREGS             POINT TO REG SAVE AREA          36440000
         EX    0,*                      AND AWAY WE GO                  36450000
         EJECT                                                          36380000
*********************************************************************** 36390000
*                                                                     * 36400000
*  EXIT THE PROGRAM                                                   * 36410002
*                                                                     * 36420000
*********************************************************************** 36430000
LEAVE    DS    0H                                              1/93 RT  36430202
         LR    R1,R13                                          1/93 RT  36430302
         L     R13,4(,R13)                                     1/93 RT  36430402
         L     R0,GETM                                         1/93 RT  36430502
         FREEMAIN R,A=(1),LV=(0)                               1/93 RT  36430602
         MVI   12(R13),X'FF'        INVALIDATE FWD-CHAIN       1/93 RT  36430702
         RETURN (14,12),RC=0                                   1/93 RT  36430802
         EJECT                                                 1/93 RT  36431002
*********************************************************************** 36432002
*                                                                     * 36433002
*  SUBROUTINE TO DO I/O TO TAPE                                       * 36434002
*                                                                     * 36435002
*********************************************************************** 36436002
TAPIO    ST    R0,TAPEIOB+16            SET CCW ADDR                    36520000
         EXCP  TAPEIOB                                                  36530000
         WAIT  ECB=TAPEECB                                              36540000
         CLI   TAPEECB,X'7F'            DID IT WORK?                    36550000
         BE    TAPIO$OK                 YES - CONTINUE                  36560000
         CLI   TAPEECB,X'41'            CHANNEL PROGRAM ERROR?          36570000
         BNE   TIONTMK             NO, NOT TAPEMARK           07/85 DBC 36580000
         CLI   IOBCSW+4,X'0D'      YES, CHEND, DEVEND, UNITEXCPT?       36590000
         BNE   TIONTMK             NO, NOT FILE MARK          07/85 DBC 36600000
         LA    R0,1                YES, GET FILE INCRIMENT    07/85 DBC 36610000
         AH    R0,FILE#SEQ         INCR TAPEMARK COUNTER      07/85 DBC 36620000
         STH   R0,FILE#SEQ         STORE NEW VALUE            07/85 DBC 36630000
         BR    R10                 RETURN TO CALLER           07/85 DBC 36640000
TIONTMK  DS    0H                                             07/85 DBC 36650000
         SPACE 1                                              07/85 DBC 36660000
         ST    R10,LKRTAPIO             SAVE R10 FOR RECURSION      U16 36670000
         CH    LCTR,=H'&LINEPPG'        ANYTHING ON PAGE? U13 11/88 SBG 36680001
         BL    *+8                      YES - CONTINUE              U13 36690000
         NEWPAGE  ,                     PRINT THE TITLE             U13 36700000
         MVC   OUTBUFF(44),=C'-*** TAPE I/O ERROR *** ECB CODE=XX *** C$36710000
               SW='                                                 U13 36720000
         HEX   OUTBUFF+33,TAPEECB,LEN=1                             U13 36730000
         HEX   OUTBUFF+44,IOBCSW+1,LEN=7                            U13 36740000
         MVC   OUTBUFF+59(24),=C'*** IOBSENSE0,1=XXXX ***'          U13 36750000
         HEX   OUTBUFF+75,TAPEIOB+2,LEN=2                           U13 36760000
         PRTLN OUTBUFF                                                  36770000
         SPACE 1                                                        36780000
**       L     R1,DD#PTR                -> DD SECTION OF TIOT       U13 36790000
**       SH    R1,=H'24'                -> TIOT                     U13 36800000
**       AH    R1,TAPEDCB+40            -> DD ENTRY FOR TAPE        U13 36810000
**       L     R1,16(,R1)               -> UCB                      U13 36820000
         L     R1,TAPEMFL               -> DCB                      U13 36830000
         L     R1,44(,R1)               -> DEB                      U13 36840000
         L     R1,32(,R1)               -> UCB                      U13 36850000
         L     R2,20(,R1)               -> EXTENDED SENSE INFO      U13 36860000
         CLI   IOBCSW+4,X'0E'  CH-END, DEV-END, UNIT-CHECK ?       PMCF 36590000
         BE    $SKPSENS      SKIP SENSE DATA, UCBEX ABOVE THE LINE PMCF
         MVC   OUTBUFF(16),=C' *** SENSE DATA='                     U13 36870000
         HEX   OUTBUFF+16,(00,R2),LEN=4                             U13 36880000
         HEX   OUTBUFF+25,(04,R2),LEN=4                             U13 36890000
         HEX   OUTBUFF+35,(08,R2),LEN=4                             U13 36900000
         HEX   OUTBUFF+44,(12,R2),LEN=4                             U13 36910000
         HEX   OUTBUFF+54,(16,R2),LEN=4                             U13 36920000
         HEX   OUTBUFF+63,(20,R2),LEN=4                             U13 36930000
         PRTLN OUTBUFF                  PRINT THE SENSE DATA        U13 36940000
         SPACE 3                                                        36950000
         NEWPAGE  10                    NEXT PAGE IF NEED ROOM      U15 36960000
         MVC  OUTBUFF(35),=C'0    BITS SET ARE FLAGGED ''*'' BELOW' U15 36970000
         PRTLN OUTBUFF                  PRINT HEADER                U15 36980000
         MVI   OUTBUFF,C'0'             DOUBLE SPACE FIRST ONE      U15 36990000
         SPACE 2                                                        37000000
*  R2 -> SENSE DATA                                                 U15 37010000
         LA    R3,10                    NUMBER OF BYTES             U15 37020000
         L     R4,=A(ERR#LIST)          -> ERROR MESSAGES           U15 37030000
         SPACE 1                                                        37040000
NEXTBYTE LA    R6,OUTBUFF+5             -> LINE POS                 U15 37050000
         LA    R1,X'80'                 MASK BIT                    U15 37060000
         SPACE 1                                                        37070000
TEST$BIT MVC   2(10,R6),0(R4)           MOVE TEXT                   U15 37080000
         EX    R1,BIT$TM                IS THIS BIT SET?            U15 37090000
         BZ    *+8                      NO - SKIP                   U15 37100000
         MVI   0(R6),C'*'               FLAG THIS ONE               U15 37110000
         SPACE 1                                                        37120000
         LA    R4,10(,R4)               -> NEXT MSG                 U15 37130000
         LA    R6,16(,R6)               -> NEXT PRINT LINE POS      U15 37140000
         SRA   R1,1                     MOVE BIT RIGHT              U15 37150000
         BNZ   TEST$BIT                 CONTINUE WITH THIS BYTE     U15 37160000
         PRTLN OUTBUFF                  PRINT DESC OF THIS BYTE     U15 37170000
         LA    R2,1(,R2)                -> NEXT BYTE OF SENSE DATA  U15 37180000
         BCT   R3,NEXTBYTE              CONTINUE IF MORE            U15 37190000
$SKPSENS DS    0H                       SKIPPING SENSE DATA IN UCB PMCF
         SPACE 3                                                        37200000
         CLI   WHERE,3                  READING DATA?                   37210000
         BNE   CLOSE$TP                 NO - CLOSE UP AND LEAVE         37220000
         L     R1,TAPEIOB+16            GET CCW ADDR                    37230000
         CLI   0(R1),RD                 WAS IT A READ?                  37240000
         BNE   CLOSE$TP                 NO - CLOSE UP AND LEAVE         37250000
         LA    R0,TCCW#FSF              SKIP REST OF DATA...            37260000
         L     R10,LKRTAPIO             RESTORE R10                 U16 37270000
         B     TAPIO                    START AT TOP OF THIS SUBRTN     37280000
         SPACE 2                                                        37290000
BIT$TM   TM    0(R2),*-*                << EXECUTED >>              U15 37300000
         SPACE 3                                              07/85 DBC 37310000
TAPIO$OK LA    R0,TCCW#FSF         WAS THIS A -               07/85 DBC 37320000
         CLM   R0,7,TAPEIOB+17      FORWARD SKIP FILE CMD?    07/85 DBC 37330000
         BNE   TIOSAMFL            NO, STILL IN SAME FILE     07/85 DBC 37340000
         LA    R0,1                YES, GET FILE INCRIMENT    07/85 DBC 37350000
         AH    R0,FILE#SEQ         GET NEW FILE# -1           07/85 DBC 37360000
         STH   R0,FILE#SEQ         STORE BACK                 07/85 DBC 37370000
TIOSAMFL DS    0H                                             07/85 DBC 37380000
         SPACE 1                                              07/85 DBC 37390000
         L     R0,=A(BUFFSIZE)          GET LENGTH REQUESTED  07/85 DBC 37400000
         MVC   DWD+2(2),IOBCSW+6        COPY LENGTH LEFT                37410000
         XC    DWD(2),DWD               CLEAR HIGH 2 BYTES              37420000
         S     R0,DWD                   MINUS LENGTH UNUSED             37430000
         ST    R0,LASTSIZE         EQUALS SIZE READ           09/84 DBC 37440000
         TM    TFLAG1,T1@ANLZ           ANALYZING?                      37450000
         BNO   4(,R10)                  NO - SKIP THIS JUNK             37460000
         A     R0,BYTECNT               ADD PREVIOUS BYTE COUNT         37470000
         ST    R0,BYTECNT               SAVE NEW TOTAL                  37480000
         L     R1,BLOCKCNT              GET BLOCK COUNT                 37490000
         LA    R1,1(,R1)                INCR                            37500000
         ST    R1,BLOCKCNT              SAVE NEW COUNT                  37510000
         CLC   LASTSIZE+2(2),TAPEBUFF START WITH BLKSIZE?     09/84 DBC 37520000
         BE    *+8                      YES - COULD BE RECFM=V          37530000
         NI    NLFLAGS,255-NL@V         NO - NOT RECFM=V                37540000
         CLC   LASTSIZE,MAXBLKSI        SAME SIZE?                      37550000
         BE    4(,R10)                  YES - DONE HERE                 37560000
         BH    TAPIO$H                  LAST BLOCK IS BIGGEST           37570000
         TM    NLFLAGS,NL@CHANG         ALREADY CHANGED?                37580000
         BO    TAPIO$NL                 YES - TURN OFF RECFM=F          37590000
         OI    NLFLAGS,NL@CHANG         SET "CHANGED"                   37600000
         B     4(,R10)                  RETURN                          37610000
         SPACE 1                                                        37620000
TAPIO$H  OI    NLFLAGS,NL@CHANG         SET "CHANGED"                   37630000
         MVC   MAXBLKSI,LASTSIZE        SAVE NEW BIGGEST BLKSIZE        37640000
TAPIO$NL NI    NLFLAGS,255-NL@F         NOT RECFM=F                     37650000
         B     4(,R10)                  RETURN TO CALLER                37660000
         EJECT                                                          37670000
*********************************************************************** 37680000
*                                                                     * 37690000
*  SUBROUTINE TO HANDLE PRINT OUTPUT                                  * 37700000
*                                                                     * 37710000
*********************************************************************** 37720000
NEWPAGE  CR    LCTR,R0                  ENOUGH LINES LEFT?              37730000
         BNLR  R10                      YES - JUST RETURN               37740000
         LA    LCTR,LINESPP-2           RESET LINE COUNTER              37750000
         PRTLN TITLE1                                                   37760000
         TM    TFLAG1,T1@PAGE1          IS THIS FIRST PAGE OF VOL?      37770000
         BNO   NEWPAG$2                 NO - SKIP THIS STUFF.           37780000
         NI    TFLAG1,255-T1@PAGE1      TURN OFF FLAG                   37790000
         MVC   TITLE2+1(28),TITLE2      BLANK IT OUT                    37800000
         SPACE 1                                                        37810000
NEWPAG$3 TM    TFLAG1,T1@PARM           HAVE PARM FIELD?                37820000
         BNO   NEWPAG$4                 NO - SKIP                       37830000
         TM    TFLAG2,T2@LPARM          IS IT TOO LONG?                 37840000
         BO    NEWPAG$4                 YES - SKIP                      37850000
         MVC   TITLE2+1(28),PARM#MSG+1  MOVE IN THE PARM FIELD          37860000
         SPACE 1                                                        37870000
NEWPAG$4 PRTLN TITLE2                   PRINT AUTHOR LINE               37880000
         SPACE 1                                                        37890000
NEWPAG$1 TM    TFLAG1,T1@PARM           HAVE PARM FIELD?                37900000
         BNO   NEWPAG$2                 NO - DON'T PRINT IT             37910000
         TM    TFLAG2,T2@LPARM          LONG PARM FIELD?                37920000
         BNO   NEWPAG$5                 NO - ALREADY PRINTED        U15 37930000
         PRTLN PARM#MSG                 PRINT IT                        37940000
NEWPAG$5 TM    TFLAG1,T1@PERR           ERROR IN PARM FIELD?            37950000
         BNO   NEWPAG$2                 NO - DON'T PRINT IT             37960000
         PRTLN PARMERR                  PRINT IT                        37970000
         SPACE 1                                                        37980000
NEWPAG$2 LA    R0,DASHES                                                37990000
         BA&XA R14,PUTPRTLN             SEPARATOR LINE                  38000000
         PRTLN COLHEAD1,I                                               38010000
         PRTLN COLHEAD2,I                                               38020000
         LA    R0,DASHES                                                38030000
         BA&XA R14,PUTPRTLN             SEPARATOR LINE                  38040000
         MVI   OUTBUFF,C'0'             DOUBLE SPACE THE FIRST FILE U15 38050000
         BCTR  LCTR,0                   ADJUST THE LINE COUNT       U15 38060000
         TM    TFLAG2,T2@RQVOL          WRONG VOLUME MOUNTED?       U15 38070000
         BNOR  R10                      NO - ALL DONE               U15 38080000
         NI    TFLAG2,255-T2@RQVOL      TURN OFF FLAG NOW           U15 38090000
         MVI   OUTBUFF,C'-'             TRIPLE SPACE                U15 38100000
         MVI   OUTBUFF+41,C'*'          START OF BOX                U15 38110000
         MVC   OUTBUFF+42(49),OUTBUFF+41  REST OF IT                U15 38120000
         PRTLN OUTBUFF                  PRINT TOP OF BOX            U15 38130000
         MVC   OUTBUFF+41(50),=C'* ID OF MOUNTED VOLUME DIFFERS FROM ID$38140000
                REQUESTED *'            MOVE IN MSG                 U15 38150000
         PRTLN OUTBUFF                  PRINT MSG                   U15 38160000
         SH    LCTR,=H'2'               ACCOUNT FOR TRIPLE SPACE    U15 38170000
         MVI   OUTBUFF+41,C'*'          START OF BOX                U15 38180000
         MVC   OUTBUFF+42(49),OUTBUFF+41  REST OF IT                U15 38190000
         PRTLN OUTBUFF                  PRINT BOTTOM OF BOX         U15 38200000
         MVI   OUTBUFF,C'0'             DOUBLE SPACE FIRST FILE     U15 38210000
         BR    R10                      RETURN                          38220000
         SPACE 3                                                        38230000
PRTLN    ST    R14,PRT#SAVE             SAVE RETURN ADDR                38240000
         LA    R0,OUTBUFF               POINT TO OUTPUT LINE            38250000
         BA&XA R14,PUTPRTLN             WRITE OUTPUT LINE               38260000
         L     R14,PRT#SAVE             RESTORE RETURN ADDR             38270000
         MVC   OUTBUFF,OUTCLEAR         CLEAR OUTPUT LINE               38280000
         BCTR  LCTR,R14                 RETURN IF MORE LINES LEFT       38290000
         ST    R14,R14SAVE              SAVE MY R14                     38300000
         ST    R10,R10SAVE              SAVE SOMEONE'S R10              38310000
         NEWPAGE ,                      DO A NEW PAGE                   38320000
         L     R14,R14SAVE              RESTORE MY R14                  38330000
         L     R10,R10SAVE              RESTORE SOMEONE'S R10           38340000
         BR    R14                      RETURN TO CALLER                38350000
         SPACE 2                                                        38360000
PUTPRTLN L     R1,OPENMFL               GET DCB ADDR                    38370000
         PUT   (1),(0)                  WRITE OUTPUT LINE               38380000
         ORG   *-2                      BACK OVER BALR                  38390000
         BR    R15                      MAKE HIM RETURN TO MY CALLER    38400000
         SPACE 3                                                        38410000
************************************************************* 12/88 SBG 38410101
* A BAL TO NEWPAGE2+4 IS DONE IN SEVERAL PLACES.            * 12/88 SBG 38411001
* THE CONDITION CODE AT THOSE POINTS IS NOT SET AT LABEL    * 12/88 SBG 38412001
* NEWPAGE2, BUT RATHER, JUST ABOVE THOSE BAL INSTRUCTIONS.  * 12/88 SBG 38413001
* IT IS THEREFORE IMPORTANT THAT THE INSTRUCTION JUST BELOW * 12/88 SBG 38414001
* LABEL NEWPAGE2 HAVE THE CORRECT CONDITION SETTINGS.       * 12/88 SBG 38415001
************************************************************* 12/88 SBG 38416001
NEWPAGE2 C     R0,LCTR2                 ENOUGH LINES LEFT?    12/88 SBG 38420001
         BLR   R10                      YES - JUST RETURN     12/88 SBG 38430001
         SPACE 1                                                        38440000
         LA    R0,TITLE1                GET RECORD ADDR                 38450000
         BA&XA R14,PUTPRNT2+4           WRITE IT OUT                    38460000
         LA    R0,DASHES                                                38470000
         BA&XA R14,PUTPRNT2+4           WRITE IT OUT                    38480000
         LA    R14,LINESPP-3            RESET LINE COUNTER    02/93 RT
         ST    R14,LCTR2                                      02/93 RT  38490000
         BR    R10                      RETURN TO CALLER                38500000
         SPACE 3                                                        38510000
PRTLN2   ST    R14,PRT#SAVE             SAVE RETURN ADDR                38520000
         LA    R1,3                     ASSUME TRIPLE SPACE             38530000
         CLI   OUTBUFF2,C'-'            IS IT?                          38540000
         BE    P2$OK                    YES                             38550000
         BCTR  R1,0                     ASSUME DOUBLE SPACE             38560000
         CLI   OUTBUFF2,C'0'            IS IT?                          38570000
         BE    P2$OK                    YES                             38580000
         BCTR  R1,0                     ASSUME SINGLE SPACE             38590000
         CLI   OUTBUFF2,C' '            IS IT?                          38600000
         BE    P2$OK                    YES                             38610000
         MVI   OUTBUFF2,C' '            MAKE A BLANK                    38620000
         SPACE 1                                                        38630000
P2$OK    L     R14,LCTR2                DECR LINE COUNT       02/93 RT  38640000
         SR    R14,R1                                         02/93 RT  38640000
         ST    R14,LCTR2                                      02/93 RT  38640000
         BP    P2$GO                    CONTINUE IF OK                  38650000
         NEWPAGE  ,2                    SET NEW PAGE                    38660000
         MVI   OUTBUFF2,C'-'            TRIPLE SPACE FIRST LINE     U13 38670000
         L     R14,LCTR2                ACCOUNT FOR IT        02/93 RT  38680000
         SH    R14,=H'3'                                      02/93 RT  38680000
         ST    R14,LCTR2                                      02/93 RT  38680000
         SPACE 1                                                        38690000
P2$GO    BA&XA R14,PUTPRNT2             WRITE OUTPUT LINE               38700000
         MVC   OUTBUFF2,OUTCLR2         CLEAR OUTPUT LINE               38710000
         L     R14,PRT#SAVE             GET RETURN ADDR                 38720000
         BR    R14                      RETURN TO CALLER                38730000
         SPACE 3                                                        38740000
PUTPRNT2 LA    R0,OUTBUFF2              GET RECORD ADDR                 38750000
         L     R1,OPENMFL2              GET SYSPRNT2 DCB ADDR           38760000
         PUT   (1),(0)                  WRITE IT OUT                    38770000
         ORG   *-2                      BACK OVER BALR                  38780000
         BR    R15                      MAKE HIM RETURN TO MY CALLER    38790000
         SPACE 3                                                        38800000
         PUSH  PRINT                                                U13 38810000
         PRINT GEN                                                  U13 38820000
         SPACE 1                                                        38830000
PRT$EXIT DCBEXIT  BLKSIZE=3192,BUFNO=1                              U15 38840000
         POP   PRINT                                                U13 38850000
         EJECT                                                          38860000
*********************************************************************** 38870000
*                                                                     * 38880000
*    ICALLABLE SUBROUTINES                                            * 38890000
*                                                                     * 38900000
*********************************************************************** 38910000
         SPACE 3                                                        38920000
*  SUBROUTINE TO CONVERT A HALFWORD TO PRINTABLE FORM                   38930000
CONVHALF MVC   DWD(2),0(R1)             MOVE FIELD TO AN ALIGNED PLACE  38940000
         LH    R0,DWD                   PICK IT UP                      38950000
         CVD   R0,DWD                   CONVERT TO PACKED               38960000
         MVC   1(5,R15),=X'2020202120'  MOVE IN EDIT MASK               38970000
         ED    0(6,R15),DWD+5           DO THE EDIT                     38980000
         BR    R14                      RETURN TO CALLER                38990000
         SPACE 3                                                        39000000
*  SUBROUTINE TO CONVERT A 2-BYTE PACKED FIELD TO PRINTABLE FORM        39010000
CONVP2   MVC   1(3,R15),=X'202120'      MOVE EDIT PATTERN TO OUT AREA   39020000
         ED    0(4,R15),0(R1)           CONVERT PACKED TO DECIMAL       39030000
         BR    R14                                                      39040000
         SPACE 3                                                        39050000
*  SUBROUTINE TO CONVERT A 3-BYTE PACKED FIELD TO PRINTABLE FORM        39060000
CONVP3   MVC   1(5,R15),=X'2020202120'  MOVE EDIT PATTERN TO OUT AREA   39070000
         ED    0(6,R15),0(R1)           CONVERT PACKED TO DECIMAL       39080000
         BR    R14                                                      39090000
         SPACE 3                                                        39100000
*  SUBROUTINE TO CONVERT A 4-BYTE PACKED FIELD TO PRINTABLE FORM        39110000
CONVP4   MVC   1(7,R15),=X'20202020202120'  MOVE EDIT PATTERN           39120000
         ED    0(8,R15),0(R1)           CONVERT PACKED TO DECIMAL       39130000
         BR    R14                                                      39140000
         SPACE 3                                                        39150000
*  SUBROUTINE TO CONVERT A PACKED DATE FIELD TO PRINTABLE FORM          39160000
CONVMDY  MVC   MDYP(4),=PL4'0'          ZERO MDYP (4-BYTES PACKED)      39170000
         MVC   MDYP(3),0(R1)            GET MMDDYY (PACKED W/O SIGN)    39180000
         UNPK  MDYU(7),MDYP(4)          UNPACK IT                       39190000
         MVC   0(2,R15),MDYU            MOVE MM                         39200000
         MVI   2(R15),C'/'                   /                          39210000
         MVC   3(2,R15),MDYU+2               DD                         39220000
         MVI   5(R15),C'/'                   /                          39230000
         MVC   6(2,R15),MDYU+4               YY                         39240000
         BR    R14                      RETURN                          39250000
         SPACE 3                                                        39260000
*  SUBROUTINE TO REMOVE LEADING BLANKS                                  39270000
SLIDE    CLI   0(R15),C' '              NON-BLANK YET?                  39280000
         BNER  R14                      YES - RETURN TO CALLER          39290000
         MVC   0(6,R15),1(R15)          MOVE IT LEFT 1 POS              39300000
         B     SLIDE                    AND CHECK IT AGAIN              39310000
         SPACE 3                                                        39320000
*  SUBROUTINE TO REMOVE LEADING ZEROES                                  39330000
DEZERO   CLI   0(R1),C'0'               NON-ZERO YET?                   39340000
         BNER  R14                      YES - RETURN                    39350000
         MVI   0(R1),C' '               MAKE IT A BLANK                 39360000
         A     R1,=F'1'                 ADVANCE                         39370000
         B     DEZERO                   LOOP                            39380000
         SPACE 3                                                        39390000
*  SUBROUTINE TO CONVERT A UNIT TYPE                                    39400000
CONVUNIT MVC   OUTBUFF2+120(9),=C'UNIT=????'                            39410000
         CLI   3(R1),X'0F'              TOO BIG?              03/94 SBG 39420000
         BHR   R10                      YES - RETURN                    39430000
         IC    R14,3(,R1)               ELSE GET BYTE                   39440000
         N     R14,=F'255'              ISOLATE IT                      39450000
         BZR   R10                      INVALID IF ZERO                 39460000
         BCTR  R14,0                    -1 BECAUSE NO ZERO              39470000
         MH    R14,=H'6'                LENGTH OF A TABLE ENTRY         39480000
         LA    R15,=C'2311  2301  2303  2302  2321  2305-12305-22314  3$39490000
               330  3340  3350  3375  3330-13380  3390  '     03/94 SBG 39500000
         LA    R14,0(R14,R15)           POINT TO CORRECT ENTRY          39510000
         MVC   OUTBUFF2+125(6),0(R14)   MOVE DEVTYPE TO LINE            39520000
         BR    R10                      RETURN TO CALLER                39530000
         SPACE 3                                                        39540000
*  SUBROUTINE TO CONVERT A DSORG                                        39550000
CNVDSORG LA    R1,TAPEBUFF                                              39560000
         AH    R1,0(,R10)               GET DSORG ADDR                  39570000
         SPACE 1                                                        39580000
CNVDSORX TM    0(R1),X'01'              DSORG=**U                       39590000
         BNO   *+8                      NO - SKIP                       39600000
         MVI   2(R15),C'U'                                              39610000
         SPACE 1                                                        39620000
         MVC   0(2,R15),=C'PO'          ASSUME DSORG=PO                 39630000
         TM    0(R1),X'02'              IS IT?                          39640000
         BOR   R14                      YES - RETURN TO CALLER          39650000
         SPACE 1                                                        39660000
         MVC   0(2,R15),=C'DA'          ASSUME DSORG=DA                 39670000
         TM    0(R1),X'20'              IS IT?                          39680000
         BOR   R14                      YES - RETURN TO CALLER          39690000
         SPACE 1                                                        39700000
         MVC   0(2,R15),=C'PS'          ASSUME DSORG=PS                 39710000
         TM    0(R1),X'40'              IS IT?                          39720000
         BOR   R14                      YES - RETURN TO CALLER          39730000
         SPACE 1                                                        39740000
         MVC   0(2,R15),=C'IS'          ASSUME DSORG=IS                 39750000
         TM    0(R1),X'80'              IS IT?                          39760000
         BOR   R14                      YES - RETURN TO CALLER          39770000
         SPACE 1                                                        39780000
         MVC   0(4,R15),=C'VSAM'        ASSUME VSAM                     39790000
         CLC   0(2,R1),=X'0008'         IS IT?                          39800000
         BER   R14                      YES - RETURN TO CALLER          39810000
         SPACE 1                                                        39820000
         MVC   0(4,R15),=C'**  '        NONE OF THE ABOVE???            39830000
         BR    R14                      RETURN TO CALLER                39840000
         SPACE 3                                                        39850000
*  SUBROUTINE TO CONVERT A RECFM                                        39860000
CNVRECFM LA    R1,TAPEBUFF                                              39870000
         AH    R1,2(,R10)               GET RECFM ADDR                  39880000
         SPACE 1                                                        39890000
CNVRECFX TM    0(R1),X'C0'              RECFM=U?                        39900000
         BNO   *+12                     NO - TRY NEXT                   39910000
         MVI   0(R15),C'U'                                              39920000
         B     RECFM$2                  CONTINUE                        39930000
         SPACE 1                                                        39940000
         TM    0(R1),X'80'              RECFM=F?                        39950000
         BNO   *+12                     NO - TRY NEXT                   39960000
         MVI   0(R15),C'F'                                              39970000
         B     RECFM$2                  CONTINUE                        39980000
         SPACE 1                                                        39990000
         TM    0(R1),X'40'              RECFM=V?                        40000000
         BNO   *+8                      NO - TRY NEXT                   40010000
         MVI   0(R15),C'V'                                              40020000
         SPACE 1                                                        40030000
RECFM$2  LA    R15,1(,R15)                                              40040000
         TM    0(R1),X'10'              BLOCKED?                        40050000
         BNO   *+12                     NO - TRY NEXT                   40060000
         MVI   0(R15),C'B'                                              40070000
         LA    R15,1(,R15)                                              40080000
         SPACE 1                                                        40090000
         TM    0(R1),X'08'              SPANNED?                        40100000
         BNO   *+12                     NO - TRY NEXT                   40110000
         MVI   0(R15),C'S'                                              40120000
         LA    R15,1(,R15)                                              40130000
         SPACE 1                                                        40140000
         TM    0(R1),X'20'              TRK OVFL?                       40150000
         BNO   *+12                     NO - TRY NEXT                   40160000
         MVI   0(R15),C'T'                                              40170000
         LA    R15,1(,R15)                                              40180000
         SPACE 1                                                        40190000
         TM    0(R1),X'02'              RECFM=A?                        40200000
         BNO   *+12                     NO - TRY NEXT                   40210000
         MVI   0(R15),C'A'                                              40220000
         LA    R15,1(,R15)                                              40230000
         SPACE 1                                                        40240000
         TM    0(R1),X'01'              RECFM=M?                        40250000
         BNOR  R14                      NO - RETURN TO CALLER           40260000
         MVI   0(R15),C'M'                                              40270000
         BR    R14                      RETURN TO CALLER                40280000
         SPACE 3                                                        40290000
*  SUBROUTINE TO COMPUTE TAPEFEET                                       40300000
TAPEFEET XR    R0,R0                                                    40310000
         D     R0,=F'1200'              GET LENGTH IN FEET              40320000
         CH    R0,=H'600'               ROUND UP?                       40330000
         BL    *+8                      NO                              40340000
         A     R1,=F'1'                 ROUND UP.                       40350000
         CVD   R1,DWD                                                   40360000
         MVC   1(5,R15),=X'2020202120'                                  40370000
         ED    0(6,R15),DWD+5                                           40380000
         BR    R14                      RETURN TO CALLER                40390000
         SPACE 3                                                        40400000
*  SUBROUTINE TO FIND A TIOT ENTRY                                      40410000
TIOTSCAN L     R1,DD#PTR                -> DD SECTION                   40420000
         XR    R0,R0                    CLEAR FOR IC                    40430000
         BA&XA R14,*+6                  SET LOOP ADDR & SKIP            40440000
         SPACE 1                                                        40450000
         AR    R1,R0                    POINT TO NEXT ENTRY             40460000
         IC    R0,0(,R1)                GET LENGTH OF THIS ENTRY        40470000
         LTR   R0,R0                    END OF TIOT?                    40480000
         BZR   R10                      YES - "NOT FOUND" RETURN        40490000
         CLC   4(8,R1),0(R15)           THIS IT?                        40500000
         BNER  R14                      NO - LOOP                       40510000
         CLC   =F'0',16(R1)             DD DUMMY? (LIKE MSM?)           40520000
         BNE   4(,R10)                  NO - RETURN (FOUND)             40530000
         BR    R10                      SAY NOT FOUND                   40540000
         EJECT                                                          40550000
         LTORG ,                                                        40560000
         SPACE 6                                                        40570000
*  REGISTER EQUATES                                            1/93 RT  40500002
R0       EQU   0                                               1/93 RT  40500102
R1       EQU   1                                               1/93 RT  40500202
R2       EQU   2                                               1/93 RT  40500302
R3       EQU   3                                               1/93 RT  40500402
R4       EQU   4                                               1/93 RT  40500502
R5       EQU   5                                               1/93 RT  40500602
R6       EQU   6                                               1/93 RT  40500702
R7       EQU   7                                               1/93 RT  40500802
R8       EQU   8                                               1/93 RT  40500902
R9       EQU   9                                               1/93 RT  40501002
R10      EQU   10                                              1/93 RT  40501102
R11      EQU   11                                              1/93 RT  40501202
R12      EQU   12                                              1/93 RT  40501302
R13      EQU   13                                              1/93 RT  40501402
R14      EQU   14                                              1/93 RT  40501502
R15      EQU   15                                              1/93 RT  40501602
*  REGISTERS USED IN ROUTINES TO LIST IEHMOVE & IEBCOPY UNLOADED MEMBRS 40580000
BLEN     EQU   R2      *PAIR*           BLOCK LENGTH LEFT               40590000
BPTR     EQU   R3      *PAIR*           -> CURRENT POS IN BLOCK         40600000
DLEN     EQU   R4                       DIRBLK OR DATA LENGTH LEFT      40610000
DPTR     EQU   R5                       -> CURRENT POS IN BLOCK         40620000
PPTR     EQU   R6                       -> POS IN PRINT LINE            40630000
         SPACE 1                                                        40640000
*  OTHER MISC REGISTERS                                                 40650000
LCTR     EQU   R8                       # OF LINES LEFT THIS PAGE       40660000
LCTR2    DS    F                        STORAGE INSTEAD OF REG 1/93 RT  40670000
         SPACE 1                                                        40690000
CC       EQU   X'40'                    COMMAND CHAINING                40700000
SLI      EQU   X'20'                    SUPPRESS LENGTH INDIC.          40710000
         SPACE 1                                                        40720000
RD       EQU   X'02'                    READ                            40730000
BSF      EQU   X'2F'                    BACKSPACE FILE        02/93 RT  40740000
FSF      EQU   X'3F'                    FORWARD SPACE FILE              40740000
RWD      EQU   X'07'                    REWIND                          40750000
         SPACE 1                                                        40760000
BUFFSIZE EQU   X'FFFF'             SIZE OF TAPE BUFFER        09/84 DBC 40770000
LINESPP  EQU   &LINEPPG                 LINES PRINTED PER PAGE11/88 SBG 40780001
         SPACE 2                                                        40790000
OPENMFL2 OPEN  (SYSPRNT2,OUTPUT),MF=L                                   40800000
OPENMFL  OPEN  (SYSPRINT,OUTPUT),MF=L                                   40810000
OPENMFLI OPEN  SYSIN,MF=L                                               40820000
TAPEMFL  OPEN  TAPEDCB,MF=L                                             40830000
         SPACE 1                                                        40840000
EXLST    DC    A(0)                                                     40850000
PRTEXLST DC    X'85',AL3(PRT$EXIT)                                      40860000
         SPACE 2                                                        40870000
TAPEECB  DC    A(0)                                                     40880000
TAPEIOB  DC    X'42000000'              IOBFLAG1 (SET FOR CMD CHAINING) 40890000
         DC    A(TAPEECB)               ECB ADDR                        40900000
IOBCSW   DC    2F'0'                    CSW                             40910000
         DC    A(0)                     CHANNEL PGM ADDR                40920000
         DC    A(TAPEDCB),A(0)          DCB ADDR                        40930000
         DC    H'1',H'0'                TAPE BLOCK COUNT INCREMENT      40940000
         SPACE 1                                                        40950000
TCCW#BSF CCW   BSF,0,0,1                BACKSPACE FILE                  40960000
         SPACE 1                                                        40950000
TCCW#FSF CCW   FSF,0,0,1                FWD SPACE FILE                  40960000
         SPACE 1                                                        40970000
TCCW#LBL CCW   RD,*-*,CC,80             READ 80 BYTE BLOCK INTO FL1LABI 40980000
         CCW   RD,*-*,00,80             READ IN NEXT BLOCK INTO FL2LABI 40990000
         SPACE 1                                                        41000000
TCCW#EOV CCW   RD,*-*,00,80             READ 80 BYTE BLOCK INTO FL1LABI 40980000
         SPACE 1                                                        41000000
TCCW#DAT CCW   RD,*-*,SLI,BUFFSIZE      READ DATA BLOCK                 41010000
         SPACE 1                                                        41020000
TCCW#RWD CCW   RWD,0,0,1                REWIND THE TAPE                 41030000
         SPACE 1                                                        41040000
TCCW#SNS CCW   4,SENSDATA,SLI,24        READ SENSE DATA                 41050000
         SPACE 2                                                        41060000
SENSDATA DC    XL24'00'                 SENSE DATA BUFFER               41070000
         SPACE 2                                                        41080000
TFLAG1   DC    X'00'                    FLAGS                           41090000
T1@PARM  EQU   X'80'                    HAVE PARM FIELD                 41100000
T1@PERR  EQU   X'40'                    ERROR IN PARM FIELD             41110000
T1@PAGE1 EQU   X'20'                    THIS IS FIRST TITLE GROUP       41120000
T1@BADEN EQU   X'10'                    DENSITY ERROR IN TAPE LABEL(S)  41130000
T1@SL    EQU   X'08'                    THIS TAPE IS SL                 41140000
T1@ANLZ  EQU   X'04'                    PARM=CHECK OR NL TAPE           41150000
T1@MVEOF EQU   X'02'                    IEHMOVE2 HAS READ TILL EOF      41160000
T1@DATA  EQU   X'01'                    HAVE INPUT DATA                 41170000
         SPACE 1                                                        41180000
TFLAG2   DC    X'00'                    MORE                            41190000
T2@RQVOL EQU   X'80'                    REQUESTED VOL MISMATCH          41200000
T2@LPARM EQU   X'40'                    PARM FIELD LONGER THAN 21 CHARS 41210000
T2@PRT2  EQU   X'20'                    SECOND PRINT FILE IN USE        41220000
         SPACE 1                                                        41230000
NLFLAGS  DC    X'00'                    NON-LABELLED TAPE FLAGS         41240000
NL@V     EQU   X'80'                    RECFM=V                         41250000
NL@F     EQU   X'40'                    RECFM=F                         41260000
NL@CHANG EQU   X'20'                    BLOCKSIZE HAS CHANGED           41270000
         SPACE 1                                                        41280000
PARMFLAG DC    X'00'                                                    41290000
PF@TEST  EQU   X'80'                    TEST                            41300000
PF@INLIN EQU   X'40'                    INLINE                          41310000
PF@NOMEM EQU   X'20'                    NOMEMBERS (NOMEM)               41320000
PF@DEN1  EQU   X'10'                    DEN1                            41330000
PF@NATTR EQU   X'08'                    NOATTR                          41340000
PF@NONOT EQU   X'04'                    NONOTE                          41350000
PF@NOCHK EQU   X'02'                    NOCHECK (NOSCAN)      06/84 DBC 41360000
PF@NL    EQU   X'01'                    NL                              41370000
         SPACE 1                                                        41380000
PARMTAB  DC    H'3',C'TEST     ',AL1(PF@TEST)                           41390000
         DC    H'5',C'INLINE   ',AL1(PF@INLIN)                          41400000
         DC    H'8',C'NOMEMBERS',AL1(PF@NOMEM)                          41410000
         DC    H'4',C'NOMEM    ',AL1(PF@NOMEM)                          41420000
         DC    H'3',C'DEN1     ',AL1(PF@DEN1)                           41430000
         DC    H'5',C'NOATTR   ',AL1(PF@NATTR)                          41440000
         DC    H'5',C'NONOTE   ',AL1(PF@NONOT)                          41450000
         DC    H'6',C'NOCHECK  ',AL1(PF@NOCHK)                06/84 DBC 41460000
         DC    H'5',C'NOSCAN   ',AL1(PF@NOCHK)                06/84 DBC 41470000
PARMLAST DC    H'1',C'NL       ',AL1(PF@NL)                             41480000
         SPACE 2                                                        41490000
         DC    C' '                     FOR CLEARING "UNLOADER"         41620000
UNLOADER DC    CL8' '             /   /IEBCOPY/IEHMOVE/IEBISA 06/84 DBC 41630000
C00080   DC    C'00080'                                                 41640000
         ORG   C00080+1                                                 41650000
C00800   DC    C'00800'                                                 41660000
RET#ADDR DC    A(EOD2)                  RETURN ADDR                     41670000
DEN#LIST DC    F'200,556,800,1600,6250,37871'  DENSITY VALUES       THO 41680000
         DC    C'200 556 800 16006250N/A '     RIGHT AFTER ABO LINE THO 41690000
TRUE#DEN DC    C'3',AL3(DEN#LIST+8)     REAL DEN;  -> 4 CHAR REAL DEN   41700000
F1       DC    F'1'                                                 U14 41710000
H1       EQU   F1+2                                                 U14 41720000
F15      DC    F'15'                                                U14 41730000
NULLNUMB DC    H'0'                     # OF NULL FILES TO SKIP     U14 41740000
NUMBNULL DC    H'0'                     # OF NULL FILES / THIS TAPE U14 41750000
WHERE    DC    X'00'                    WHERE-WE-ARE FLAG               41760000
         SPACE 1                                                        41770000
         PUSH  PRINT                                                    41780000
         PRINT GEN                                                      41790000
         SPACE 1                                                        41800000
HEXTAB   HEXTAB  DUAL=NO                                                41810000
         SPACE 3                                                    THO 41860000
         POP   PRINT                                                THO 41870000
         SPACE 1                                                    THO 41880000
*                                                             DRK APR07
* TABLE OF FDR DEVICE TYPES AND CODES                         DRK APR07
* COURTESY OF BRUCE BLACK, INNOVATION DP                      DRK APR07
*                                                             DRK APR07
FDRDVTAB DS    0C                                             09/84 DBC 41890000
         DC    C'A2305-1'                                     DRK APR07
         DC    C'BF6421 '   (FUJITSU)                         DRK APR07
         DC    C'C2305-2'                                     DRK APR07
         DC    C'D3380-1'   (3390-1 COMPATIBILITY, 1113 CYLS) DRK APR07
         DC    C'E3380-2'   (3390-2 COMPATIBILITY, 2226 CYLS) DRK APR07
         DC    C'F3390-1'   NATIVE (1113 CYLS)                DRK APR07
         DC    C'G3390-2'   NATIVE (2226 CYLS)                DRK APR07
         DC    C'H3380E4'   (EMC E- "MINUS", 1408 CYLS)       DRK APR07
         DC    C'I3390-F'   (HITACHI, 2655 CYLS)              DRK APR07
         DC    C'J3380-3'   (3390-3 COMPATIBILITY, 3339 CYLS) DRK APR07
         DC    C'K3380-K'   (2655 CYLS)                       DRK APR07
         DC    C'L3380-E'   (1770 CYLS)                       DRK APR07
         DC    C'M3380  '   (SINGLE DENSITY, 885 CYLS)        DRK APR07
         DC    C'N3375  '                                     DRK APR07
         DC    C'O3390-3'   NATIVE (3339 CYLS)                DRK APR07
         DC    C'P9345-1'   (1440 CYLS)                       DRK APR07
         DC    C'Q9345-2'   (2156 CYLS)                       DRK APR07
         DC    C'R3380K4'   (EMC K- "MINUS", 1830 CYLS)       DRK APR07
         DC    C'S3390E2'   (EMC, 1540 CYLS)                  DRK APR07
         DC    C'T2314  '                                     DRK APR07
         DC    C'U334035'                                     DRK APR07
         DC    C'V334070'                                     DRK APR07
         DC    C'W3390-9'   (10017 CYLS)                      DRK APR07
         DC    C'X3330  '   (MODEL 1)                         DRK APR07
         DC    C'Y333011'                                     DRK APR07
         DC    C'Z3350  '                                     DRK APR07
         DC    C'03380K5'   (EMC, K+ "PLUS," 3993 CYLS)       DRK APR07
         DC    C'1399553'   (3995-153, 4365 CYLS)             DRK APR07
         DC    C'2339027'   (32760 CYLS)                      DRK APR07
         DC    C'3339054'   (65520 CYLS)                      DRK APR07
FDRDVEND DC    C' ??????'          FENCE FOR UNKNOWN FDR DEVICES        42000000
         EJECT                                                          42100000
WORKD    DSECT                          RESUME                          42110000
         DS    9D                  SAVE AREA                  01/93 RT
DD#PTR   DS    A                        -> DD SECTION OF TIOT           42120000
LEN#TAPE DS    F                        TAPE LENGTH                     42130000
         DS    F                        NL LENGTH                       42140000
FILE#SEQ DS    H                        NL FILE SEQ #                   42150000
         DS    H                        (UNUSED)                        42160000
PRT#SAVE DS    A                        SAVE FOR RETURN ADDR            42170000
R10SAVE  DS    A                        MORE...                         42180000
LKRTAPIO DS    A                        ...OF THE...                    42190000
R14SAVE  DS    A                        ...SAME                         42200000
TU$SAVE2 DS    2A                  USED BY TEST$UP            06/84 DBC 42210000
BOOMREGS DS    16F                      REG SAVE AREA FOR LOGIC ERROR   42220000
MAXBLKSI DS    F                   BIGGEST BLOCK READ SO FAR  09/84 DBC 42230000
LASTSIZE DS    F                   SIZE OF THE LAST BLK READ  09/84 DBC 42240000
BLOCKCNT DS    F                        # OF BLOCKS READ                42250000
BYTECNT  DS    F                        TOTAL # OF BYTES READ           42260000
CBTBLK1  DS    F                   STORE BLOCKSIZ FOR CBT973. 11/88 SBG 42260100
CBTAPEND DS    F                   STORE EO-BLOCK FOR CBT973. 11/88 SBG 42260200
CBSTOR1  DS    F                   STORE REGISTER FOR CBT973. 11/88 SBG 42260300
CBSTOR2  DS    F                   STORE REGISTER FOR CBT973. 01/93 RT  42260300
CBSTOR01 DS    F                   STORE REGISTER FOR CBT973. 11/88 SBG 42260400
CBSTOR02 DS    F                   STORE REGISTER FOR CBT973. 11/88 SBG 42260500
CBSTOR03 DS    F                   STORE REGISTER FOR CBT973. 11/88 SBG 42260600
CBSTOR04 DS    F                   STORE REGISTER FOR CBT973. 11/88 SBG 42260700
CBSTOR05 DS    F                   STORE REGISTER FOR CBT973. 11/88 SBG 42260800
CBSTOR06 DS    F                   STORE REGISTER FOR CBT973. 11/88 SBG 42260900
CBSTOR08 DS    F                   STORE REGISTER FOR CBT973. 11/88 SBG 42261000
CBSTOR09 DS    F                   STORE REGISTER FOR CBT973. 11/88 SBG 42261100
CBSTOR10 DS    F                   STORE REGISTER FOR CBT973. 11/88 SBG 42261200
CBSAVE4  DS    F                   STORE REGISTER FOR CBT973. 11/88 SBG 42261301
CBSAVE15 DS    F                   STORE REGISTER 15          11/88 SBG 42261401
NL#SAVE  DS    4F                  SAVE FOR NL STUFF IF SL    09/84 DBC 42270000
NL#FLAGX DS    X                        SAME                            42280000
UPDT#TYP DS    C                        A,R,C FOR UPDTE                 42290000
PTF#TYPE DS    C                   A,F,P,U FOR SYSMODS        07/85 DBC 42300000
UPDTXFLG DS    C                   FLAG IF ./ OR >< IN UPDTE  11/88 SBG 42301000
BLANKS   DS    0CL17,CL16' '                                            42310000
OUTCLEAR DC    C' '                     FOR CLEARING OUTBUFF            42320000
OUTBUFF  DS    CL133                                                    42330000
OUTCLR2  DC    C' '                     FOR CLEARING OUTBUFF2           42340000
OUTBUFF2 DS    CL133,7C                                             U14 42350000
DASHES   DS    CL133                                                    42360000
INBUFF   DS    CL80                                                     42370000
LABELS   DS    3CL80                    FOR VOL,FL1,FL2                 42380000
CBTCLEAR DC    C' '                     FOR CLEARING CBT CARD 11/88 SBG 42390000
CBTCARD  DS    CL80              CBT973 CARD-IMAGE EXPANSION  11/88 SBG 42390100
CSTATCLR DC    C' '                     FOR CLEARING CBT DOC  12/88 SBG 42390201
CBTVERS  DS    CL4                      CBT TAPE VERSION #    12/88 SBG 42390301
CBTDATE  DS    CL8                      CBT TAPE RELEASE DATE 12/88 SBG 42390401
MV#ABUF2 DS    A(MV#BUFF2)              ADDR OF SECOND DE-SPANNING BUFF 42390500
MV#BUFF1 DS    CL78                     FIRST DE-SPANNING BUFFER        42400000
MV#BUFF2 DS    CL78                     SECOND DE-SPANNING BUFFER       42410000
PARM#MSG DS    CL110                    FOR PARM                        42420000
OB2END   DS    A(OUTBUFF2+110)          ADDR OF END OF OUTBUFF2         42430000
OB2END2  DS    A(OUTBUFF2+110+10)       SAME FOR UPDTE                  42440000
DASDRSAV DS    XL24                     FOR HEADER RECORD               42450000
FDRSAVE  DS    CL8                      FOR 'DUMMYDSF' IF PRESENT       42460000
MDYP     DS    PL4'0'                   FOR CONVMDY                     42470000
MDYU     DS    CL7                      FOR CONVMDY                     42480000
         SPACE 2                                              12/88 SBG 42480101
***** LOADED  LITERALS  -  SEE END OF THE PROGRAM  ********** 12/88 SBG 42480201
         DS    0D                                             12/88 SBG 42480301
EMPTYLIT DC    C'THISFILEHASBEENDELETEDORISRESERVED'  NEWVERS 12/88 SBG 42481001
EMPTYLI2 DC    C'DELETEDBYPREVIOUSVERSIONOFTHISTAPE'  OLDVERS 12/88 SBG 42482001
REQ#VOL  DC    C'+** REQUESTED VOL="123456" **'                         42483001
         SPACE 1                                                        42484001
FVOL#MSG DC    C' *** THIS IS VOLUME # XXXX OF THE ABOVE MULTIPLE VOLUM$42485001
               E FILE.  FIRST VOLUME WAS XXXXXX.'                       42486001
         SPACE 1                                                        42487001
PARMERR  DC    C' UNKNOWN PARM OPTION AT COL=XXX.  ANY REMAINING PARM I$42488001
               GNORED.'                                                 42489001
FFS      DC    XL12'FFFFFFFFFFFFFFFFFFFFFFFF'                           42489101
CHJOBLIT DC    C'//MVSMODS'       ARNIE CASINGHINO'S JOBNAME  12/88 SBG 42489201
VERSLIT1 DC    C'* VERSION'       DATE OF CBT TAPE - PREFIX   12/88 SBG 42489301
VERSLIT2 DC    C'VERSION #'       VERSION OF CBT TAPE PREFIX  12/88 SBG 42489401
CBHLIT   DC    C'VER        -  -  '   DISPLAY FOR CBTHEADER   12/88 SBG 42489501
         SPACE 1                                              12/88 SBG 42489701
************************************************************* 12/88 SBG 42489801
*  TABLE OF UNLOADER LITERALS - NOTE, IF YOU ADD TO THIS      12/88 SBG 42489901
*   TABLE, YOU MUST ADD TO THE LIST OF LITERALS TO BE MOVED   12/88 SBG 42490001
*   INTO THIS WORKAREA THAT IS AT THE END OF THIS PROGRAM.    12/88 SBG 42490101
*  PLEASE SEE THE LABEL CALLED "LITMOVES" -  THIS WAS BECAUSE 12/88 SBG 42490201
*   THE 3RD BASE REGISTER WAS EXCEEDED, AND I MOVED SOME OF   12/88 SBG 42490301
*   THE LITERALS INTO THE WORKAREA DSECT.  THEY THEREFORE     12/88 SBG 42490401
*   HAVE TO BE INITIALIZED IN THE GETMAINED WORKAREA STORAGE. 12/88 SBG 42490501
************************************************************* 12/88 SBG 42490601
IEBCOPY  DC    C'IEBCOPY'                                               42490701
IEHMOVE  DC    C'IEHMOVE'                                               42490801
IEBISAM  DC    C'IEBISAM'                                               42490901
IEHDASDR DC    C'IEHDASDR'                                              42491001
IEBUPXXX DC    C'IEBUPXXX'         IEBUPDTE OR IEBUPDAT       06/84 DBC 42491101
FDR      DC    C'FDR   '                                                42491201
FDRDSF   DC    C'FDRDSF'                                                42491301
FDRXXX   DC    C'FDR???'                                                42491401
SLICKMNG DC    C'SLICKMNG'                                    09/84 DBC 42491501
CTAPEMAP DC    C'TAPEMAP'                                               42491601
SMPPTFIN DC    C'SMPPTFIN'                                    07/85 DBC 42491701
CBT973   DC    C'CBT973'                                      11/88 SBG 42491801
CBTEMPTY DC    C'CBTEMPTY'         EMPTY FILE ON THE CBT TAPE 11/88 SBG 42491901
CBHEADER DC    C'CBT DOC '         DOC FILE FOR THE CBT TAPE  12/88 SBG 42492001
UTY3420C DC    X'33008003'        UNIT TYPE FOR 3420C CARTS   09/89^SBG 42492101
         DS    0F                 ALIGNMENT                   12/88 SBG 42492201
CDOCSIZE DC    F'32720'           BLOCK SIZE OF CBT DOC FILE  12/88 SBG 42492301
INCORLAB DC    C'-*****  ONE OR MORE FILES HAVE DENSITY INDICATED INCOR$42492401
               RECTLY IN LABELS.  ALL FILES ARE WRITTEN AT XXXX BPI  **$42492501
               ***'                                                     42492601
***** LOADED  LITERALS  -  SEE END OF THE PROGRAM  ********** 12/88 SBG 42492701
         SPACE 2                                                        42492801
DWD      DS    D                                                        42493000
JFCB     DS    XL176                    DWD ALIGNED                     42500000
************************************************************* 12/88 SBG 42500101
*  WE CONTINUE TO ADDRESS THE PROBLEM OF EXCEEDING 3 BASE   * 12/88 SBG 42501001
*  REGISTERS.  PREVIOUSLY, THE TAPE BUFFER WAS ALIGNED ON   * 12/88 SBG 42502001
*  HALF A PAGE.  WE WANT TO HAVE MOST OF THIS HALF A PAGE   * 12/88 SBG 42503001
*  FOR COPYING LITERALS SO THEY CAN BE ADDRESSABLE FROM THE * 12/88 SBG 42504001
*  WORKAREA'S BASE REGISTER.  WE HOPE THE ALIGNMENT OF      * 12/88 SBG 42505001
*  WORKD+2048 WASN'T REALLY NECESSARY.                      * 12/88 SBG 42506001
*-----------------------------------------------------------* 12/88 SBG 42506101
*  HOWEVER, IF TAPEBUFF+NNN HAS BEEN SPECIFIED IN ANY       * 12/88 SBG 42506201
*  INSTRUCTION, THE SUM OF THE DISPLACEMENT OF TAPEBUFF     * 12/88 SBG 42506301
*  AND NNN MUST NOT EXCEED 4095.  THE LARGEST SUCH          * 12/88 SBG 42506401
*  DISPLACEMENT WE HAVE FOUND AT THIS WRITING IS 282.       * 12/88 SBG 42506501
*  THAT IS TO SAY, TAPEBUFF+282 IS THE FARTHEST OUT         * 12/88 SBG 42506601
*  ADDRESS THAT THE CURRENT INCARNATION OF THIS PROGRAM     * 12/88 SBG 42506701
*  HAS TO GET TO, FROM REGISTER 13.  THEREFORE, I HAVE      * 12/88 SBG 42506801
*  SUBTRACTED 300 FROM 4096 AND SET TAPEBUFF AT WORKD+3796. * 12/88 SBG 42506901
*  PLEASE REMEMBER THAT AT LEAST DOUBLEWORD ALIGNMENT       * 12/88 SBG 42507001
*  SHOULD BE OBSERVED.                                      * 12/88 SBG 42507101
************************************************************* 12/88 SBG 42508001
         ORG   WORKD+3796   ALIGN THE BUFFER NOT SO REAL WELL 12/88 SBG 42510001
TAPEBUFF DS    CL(BUFFSIZE)             TAPE INPUT DATA BUFFER          42520000
WORKLEN  EQU   *-WORKD                                                  42530000
         SPACE 2                                                        42540000
         ORG   OUTBUFF+1                                                42550000
#UNLOAD  DS    CL8,1C                                         06/84 DBC 42560000
#FILE    DS    CL4,2C                                         06/84 DBC 42570000
#DSN     DS    CL17,2C                                                  42580000
#PSWD    DS    CL4,2C                                         06/84 DBC 42590000
#CDATE   DS    CL6,2C                                                   42600000
#EDATE   DS    CL6,2C                                         06/84 DBC 42610000
#INFOSRC DS    CL6,2C                                         06/84 DBC 42620000
#RECFM   DS    CL5,2C                                         06/84 DBC 42630000
#LRECL   DS    CL5,2C                                                   42640000
#BLKSIZE DS    CL5,2C                                                   42650000
#BLKCNT  DS    CL6,2C                                                   42660000
#DEN     DS    CL4,1C                                         06/84 DBC 42670000
#TRTCH   DS    CL3,2C                                         06/84 DBC 42680000
#LENGTH  DS    CL4,2C                                         06/84 DBC 42690000
#CUMLEN  DS    CL4,2C                                         06/84 DBC 42700000
#CREATOR DS    CL17                                           06/84 DBC 42710000
         SPACE 2                                                        42720000
         PUSH  PRINT                                                    42730000
         PRINT GEN                                                      42740000
         SPACE 2                                                        42750000
         ORG   LABELS                                                   42760000
         SPACE 2                                                        42770000
         VOL   DSECT=NO                                                 42780000
         FL1   DSECT=NO                                                 42790000
         FL2   DSECT=NO                                                 42800000
         POP   PRINT                                                    42810000
         EJECT                                                          42820000
TAPEMAP  CSECT                          RESUME                          42830000
         SPACE 2                                                        42840000
         XYZZY NOGEN               SUPPRESS EXPANSION         06/84 DBC 42850000
         SPACE 1                                              06/84 DBC 42860000
TAPEDCB  DCB   DSORG=PS,MACRF=E,DDNAME=SYSUT1,DEVD=TA,EXLST=EXLST,     $42870000
               IOBAD=TAPEIOB                                            42880000
         XYZZY GEN                                            09/84 DBC 42890000
         SPACE 2                                                        42900000
TITLE1   DC    C'1VOL=                             TAPE ANALYSIS PROGRA$42910000
               M   (T A P E M A P)  V2.5          DAYDAYDAY  MON DD, 19$42920000
               YY  (YY.DDD)  HH:MM:SS'                                  42930000
TTL1TIME EQU   *-9                                                      42940000
TTL1DATE EQU   TTL1TIME-23                                              42950000
TTL1DAY  EQU   TTL1DATE-11                                              42960000
         SPACE 2                                                        42970000
TITLE2   DC    C'                                                      $42980000
                                                                       $42990000
                                    '                                   43000000
         SPACE 2                                                        43010000
DSFHDING DS    CL133                                                    43020000
         ORG   DSFHDING                                                 43030000
         DC    CL17'0'                                                  43040000
DSFDSN   EQU   *-DSFHDING                                               43050000
         DC    CL44'-------------------DSNAME-------------------',C'-'  43060000
DSFTRK   EQU   *-DSFHDING                                               43070000
         DC    CL9'TRKS USED',C'-'                     TRACKS USED      43080000
DSFRECFM EQU   *-DSFHDING                                               43090000
         DC    CL5'RECFM',C'-'                         RECFM            43100000
DSFLRECL EQU   *-DSFHDING                                               43110000
         DC    CL5'LRECL',C'-'                         LRECL            43120000
DSFBLKSZ EQU   *-DSFHDING                                               43130000
         DC    CL7'BLKSIZE',C'-'                       BLKSIZE          43140000
DSFDSORG EQU   *-DSFHDING                                               43150000
         DC    CL5'DSORG',C'-'                         DSORG            43160000
DSFCDATE EQU   *-DSFHDING                                               43170000
         DC    CL7'CREATED',C'-'                       C-DATE           43180000
DSFEDATE EQU   *-DSFHDING                                               43190000
         DC    CL7'EXPIRES',C'-'                       E-DATE           43200000
DSFSECUR EQU   *-DSFHDING                                               43210000
         DC    CL8'SECURITY',C'-'                      SECURITY         43220000
         ORG   ,                                                        43230000
SLKHDING DS    CL133                                                    43240000
         ORG   SLKHDING                                                 43250000
         DC    CL17'0'                                                  43260000
SLKMEM   EQU   *-SLKHDING                                               43270000
         DC    CL10'NAME------.'          MEMBER                        43280000
SLKSTAT  EQU   *-SLKHDING                                               43290000
         DC    CL4'STAT',C'-'             STATUS                        43300000
SLKREV   EQU   *-SLKHDING-1                                             43310000
         DC    CL3'REV',C'-'              REVISION                      43320000
SLKINFO  EQU   *-SLKHDING                                               43330000
         DC    CL10'---INFO---',C'-'      INFO                          43340000
SLKTYPE  EQU   *-SLKHDING                                               43350000
         DC    CL8'--TYPE--',C'-'         TYPE                          43360000
SLKACTST EQU   *-SLKHDING-1                                             43370000
         DC    CL7'--ACTST',C'-'          ACTST                         43380000
SLKDELST EQU   *-SLKHDING-1                                             43390000
         DC    CL7'--DELST',C'-'          DELST                         43400000
SLKBLKS  EQU   *-SLKHDING-1                                             43410000
         DC    CL5'-BLKS',C'-'            BLOCKS                        43420000
SLKCREAT EQU   *-SLKHDING                                               43430000
         DC    CL8'-CREATED',C'-'         CREATE DATE                   43440000
SLKUPD   EQU   *-SLKHDING                                               43450000
         DC    CL8'-UPDATED'              LAST UPDATE DATE              43460000
         ORG   ,                                                        43470000
         SPACE 2                                                        43480000
         XYZZY NOGEN                                          09/84 DBC 43490000
         SPACE 1                                              09/84 DBC 43500000
SYSIN    DCB  DSORG=PS,MACRF=GM,DDNAME=SYSIN,LRECL=80,EODAD=EOD,BUFNO=1 43510000
         SPACE 2                                                        43520000
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,LRECL=133,            $43530000
               RECFM=FBA,EXLST=PRTEXLST                                 43540000
         SPACE 2                                                        43550000
SYSPRNT2 DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRNT2,LRECL=133,            $43560000
               RECFM=FBA,EXLST=PRTEXLST                                 43570000
         SPACE 1                                              06/84 DBC 43580000
         XYZZY GEN                 RESTORE EXPANSIONS         06/84 DBC 43590000
COLHEAD1 DC    CL133' '            1ST TITLE LINE             06/84 DBC 43600000
         ORG   COLHEAD1+1          RELOCATE BACK              06/84 DBC 43610000
         DC    CL8'RELOAD',C' '    #UNLOAD                    06/84 DBC 43620000
         DC    CL4'FILE',C'  '     #FILE                      06/84 DBC 43630000
         DC    CL17' ',C'  '       #DSN                       06/84 DBC 43640000
         DC    CL4'PSWD',C'  '     #PSWD                      06/84 DBC 43650000
         DC    CL6' ',C'  '        #CDATE                     06/84 DBC 43660000
         DC    CL6' ',C'  '        #EDATE                     06/84 DBC 43670000
         DC    CL6' INFO',C'  '    #INFOSRC                   06/84 DBC 43680000
         DC    CL5' ',C'  '        #RECFM                     06/84 DBC 43690000
         DC    CL5' ',C'  '        #LRECL                     06/84 DBC 43700000
         DC    CL5'BLOCK',C'  '    #BLKSIZE                   06/84 DBC 43710000
         DC    CL6' BLOCK',C'  '   #BLKCNT                    06/84 DBC 43720000
         DC    CL3' ',C'  '        #DEN                       06/84 DBC 43730000
         DC    CL3' ',C'  '        #TRTCH                     06/84 DBC 43740000
         DC    CL4'LNTH',C'  '     #LENGTH                    06/84 DBC 43750000
         DC    CL4'TOTL',C'  '     #CUMLEN                    06/84 DBC 43760000
         DC    C'CREATOR'          #CREATOR                   06/84 DBC 43770000
         ORG   ,                   RESTORE                    06/84 DBC 43780000
         SPACE 3                                              06/84 DBC 43790000
COLHEAD2 DC    CL133' '            2ND TITLE LINE             06/84 DBC 43800000
         ORG   COLHEAD2+1          RELOCATE BACK              06/84 DBC 43810000
         DC    CL8'FORMAT',C' '    #UNLOAD                    06/84 DBC 43820000
         DC    CL4'SEQ#',C'  '     #FILE                      06/84 DBC 43830000
         DC    CL17'DATASET NAME',C'  ' #DSN                  06/84 DBC 43840000
         DC    CL4'REQD',C'  '     #PSWD                      06/84 DBC 43850000
         DC    CL6'C-DATE',C'  '   #CDATE                     06/84 DBC 43860000
         DC    CL6'E-DATE',C'  '   #EDATE                     06/84 DBC 43870000
         DC    CL6'SOURCE',C'  '   #INFOSRC                   06/84 DBC 43880000
         DC    CL5'RECFM',C'  '    #RECFM                     06/84 DBC 43890000
         DC    CL5'LRECL',C'  '    #LRECL                     06/84 DBC 43900000
         DC    CL5' SIZE',C'  '    #BLKSIZE                   06/84 DBC 43910000
         DC    CL6' COUNT',C'  '   #BLKCNT                    06/84 DBC 43920000
         DC    CL3'DEN',C'  '      #DEN                       06/84 DBC 43930000
         DC    CL3'TRT',C'  '      #TRTCH                     06/84 DBC 43940000
         DC    CL4'(FT)',C'  '     #LENGTH                    06/84 DBC 43950000
         DC    CL4'LNTH',C'  '     #CUMLEN                    06/84 DBC 43960000
         DC    C'JOBNAME/STEPNAME' #CREATOR                   06/84 DBC 43970000
         ORG   ,                   RESTORE                    06/84 DBC 43980000
         SPACE 3                                              06/84 DBC 43990000
         EJECT                                                          44000000
ERR#LIST DC    0F'0'                                                U15 44010000
*                                                                   U15 44020000
*  ERROR MSGS FOR EACH SENSE BIT FOR 3420 TAPE DRIVES               U15 44030000
*                                                                   U15 44040000
         SPACE 1                                                        44050000
*** BYTE 0                       BIT                                U15 44060000
         DC    C'CMD REJ   '      0     80                          U15 44070000
         DC    C'INT REQ   '      1     40                          U15 44080000
         DC    C'BUS OUT CK'      2     20                          U15 44090000
         DC    C'EQC CHK   '      3     10                          U15 44100000
         DC    C'DATA CHK  '      4     08                          U15 44110000
         DC    C'OVERRUN   '      5     04                          U15 44120000
         DC    C'WORD CNT 0'      6     02                          U15 44130000
         DC    C'DATA CNVTR'      7     01                          U15 44140000
         SPACE 1                                                        44150000
*** BYTE 1                       BIT                                U15 44160000
         DC    C'NOISE     '      0     80                          U15 44170000
         DC    C'SEL+RDY+NB'      1     40                          U15 44180000
         DC    C'NOT READY '      2     20                          U15 44190000
         DC    C'7 TRK FEAT'      3     10                          U15 44200000
         DC    C'@ LOAD PT '      4     08                          U15 44210000
         DC    C'WRITE STAT'      5     04                          U15 44220000
         DC    C'FILE PROT '      6     02                          U15 44230000
         DC    C'NOT CAPABL'      7     01                          U15 44240000
         SPACE 1                                                        44250000
*** BYTE 2                       BIT                                U15 44260000
         DC    C'TIE BIT 0 '      0     80                          U15 44270000
         DC    C'TIE BIT 1 '      1     40                          U15 44280000
         DC    C'TIE BIT 2 '      2     20                          U15 44290000
         DC    C'TIE BIT 3 '      3     10                          U15 44300000
         DC    C'TIE BIT 4 '      4     08                          U15 44310000
         DC    C'TIE BIT 5 '      5     04                          U15 44320000
         DC    C'TIE BIT 6 '      6     02                          U15 44330000
         DC    C'TIE BIT 7 '      7     01                          U15 44340000
         SPACE 1                                                        44350000
*** BYTE 3                       BIT                                U15 44360000
         DC    C'R/W VRC   '      0     80                          U15 44370000
         DC    C'MT/LRC    '      1     40                          U15 44380000
         DC    C'SKEW      '      2     20                          U15 44390000
         DC    C'END DC/CRC'      3     10                          U15 44400000
         DC    C'ENV/ECC   '      4     08                          U15 44410000
         DC    C'1600 BPI  '      5     04                          U15 44420000
         DC    C'BACKWARD  '      6     02                          U15 44430000
         DC    C'C/P COMPAR'      7     01                          U15 44440000
         SPACE 1                                                        44450000
*** BYTE 4                       BIT                                U15 44460000
         DC    C'MP H E    '      0     80                          U15 44470000
         DC    C'DROP READY'      1     40                          U15 44480000
         DC    C'TAPE INDIC'      2     20                          U15 44490000
         DC    C'W T VRC   '      3     10                          U15 44500000
         DC    C'MICROPGM  '      4     08                          U15 44510000
         DC    C'LWR       '      5     04                          U15 44520000
         DC    C'UNIT CHECK'      6     02                          U15 44530000
         DC    C'RSRVD RPQ '      7     01                          U15 44540000
         SPACE 1                                                        44550000
*** BYTE 5                       BIT                                U15 44560000
         DC    C'NEW SUBSYS'      0     80                          U15 44570000
         DC    C'NEW SUBSYS'      1     40                          U15 44580000
         DC    C'WTM CHECK '      2     20                          U15 44590000
         DC    C'ID BURST  '      3     10                          U15 44600000
         DC    C'START READ'      4     08                          U15 44610000
         DC    C'PART''L REC'     5     04                          U15 44620000
         DC    C'POSTAMBLE '      6     02                          U15 44630000
         DC    C'RSRVD RPQ '      7     01                          U15 44640000
         SPACE 1                                                        44650000
*** BYTE 6                       BIT                                U15 44660000
         DC    C'7 TRK UNIT'      0     80                          U15 44670000
         DC    C'WRT CURR  '      1     40                          U15 44680000
         DC    C'DUAL DEN  '      2     20                          U15 44690000
         DC    C'NOT 1600  '      3     10                          U15 44700000
         DC    C'MOD 4,6,8 '      4     08                          U15 44710000
         DC    C'MD 5,6,7,8'      5     04                          U15 44720000
         DC    C'MOD 3,4   '      6     02                          U15 44730000
         DC    C'MD 3,4,7,8'      7     01                          U15 44740000
         SPACE 1                                                        44750000
*** BYTE 7                       BIT                                U15 44760000
         DC    C'LAMP FAIL '      0     80                          U15 44770000
         DC    C'BOTM LEFT '      1     40                          U15 44780000
         DC    C'BOTM RIGHT'      2     20                          U15 44790000
         DC    C'RESET KEY '      3     10                          U15 44800000
         DC    C'DSE       '      4     08                          U15 44810000
         DC    C'ERASE HEAD'      5     04                          U15 44820000
         DC    C'AIR BEARNG'      6     02                          U15 44830000
         DC    C'LOAD FAIL '      7     01                          U15 44840000
         SPACE 1                                                        44850000
*** BYTE 8                       BIT                                U15 44860000
         DC    C'IBG       '      0     80                          U15 44870000
         DC    C'SPARE ?   '      1     40                          U15 44880000
         DC    C'SPARE ?   '      2     20                          U15 44890000
         DC    C'EARLY READ'      3     10                          U15 44900000
         DC    C'CTL BURST '      4     08                          U15 44910000
         DC    C'SLOW RD B '      5     04                          U15 44920000
         DC    C'SLOW RD E '      6     02                          U15 44930000
         DC    C'VELOCITY  '      7     01                          U15 44940000
         SPACE 1                                                        44950000
*** BYTE 9                       BIT                                U15 44960000
         DC    C'6250 CORR '      0     80                          U15 44970000
         DC    C'WRT VEL CH'      1     40                          U15 44980000
         DC    C'CHAN BUFF '      2     20                          U15 44990000
         DC    C'CRC III   '      3     10                          U15 45000000
         DC    C'6250 FEAT '      4     08                          U15 45010000
         DC    C'SPARE ?   '      5     04                          U15 45020000
         DC    C'SPARE ?   '      6     02                          U15 45030000
         DC    C'TCU RSRVD '      7     01                          U15 45040000
         EJECT                                                12/88 SBG 45050001
************************************************************* 12/88 SBG 45050101
*    THE FOLLOWING LITERALS ARE MOVED INTO THE WORKAREA     * 12/88 SBG 45050201
*    DSECT.  THE REASON IS THAT THE 3RD BASE REGISTER HAS   * 12/88 SBG 45050301
*    BEEN EXCEEDED IN THE COURSE OF PROGRAM MODIFICATIONS,  * 12/88 SBG 45050401
*    AND SOME LITERALS WHICH WERE ADDRESSABLE BY THE BASE   * 12/88 SBG 45050501
*    REGISTERS, HAVE NOW BEEN MOVED TO THE WORK AREA DSECT. * 12/88 SBG 45050601
*    THEREFORE, ANY CHANGES TO THAT PART OF THE WORKAREA    * 12/88 SBG 45050701
*    DSECT MUST BE REFLECTED BY CORRESPONDING CHANGES       * 12/88 SBG 45050801
*    HERE.                                                  * 12/88 SBG 45050901
************************************************************* 12/88 SBG 45051001
         DS    0D                  ALIGNMENT                  12/88 SBG 45051101
LITMOVES DC    C'THISFILEHASBEENDELETEDORISRESERVED'  NEWVERS 12/88 SBG 45051201
         DC    C'DELETEDBYPREVIOUSVERSIONOFTHISTAPE'  OLDVERS 12/88 SBG 45052001
         DC    C'+** REQUESTED VOL="123456" **'                         45052101
         SPACE 1                                                        45052201
         DC    C' *** THIS IS VOLUME # XXXX OF THE ABOVE MULTIPLE VOLUM$45052301
               E FILE.  FIRST VOLUME WAS XXXXXX.'                       45052401
         SPACE 1                                                        45052501
         DC    C' UNKNOWN PARM OPTION AT COL=XXX.  ANY REMAINING PARM I$45052601
               GNORED.'                                                 45052701
         DC    XL12'FFFFFFFFFFFFFFFFFFFFFFFF'                           45052801
         DC    C'//MVSMODS'       ARNIE CASINGHINO'S JOBNAME  12/88 SBG 45052901
         DC    C'* VERSION'       DATE OF CBT TAPE - PREFIX   12/88 SBG 45053001
         DC    C'VERSION #'       VERSION OF CBT TAPE PREFIX  12/88 SBG 45053101
         DC    C'VER        -  -  '   DISPLAY FOR CBTHEADER   12/88 SBG 45053201
************************************************************* 12/88 SBG 45053401
         DC    C'IEBCOPY'          UNLOADER NAMES             12/88 SBG 45053501
         DC    C'IEHMOVE'                                     12/88 SBG 45053601
         DC    C'IEBISAM'                                     12/88 SBG 45053701
         DC    C'IEHDASDR'                                    12/88 SBG 45053801
         DC    C'IEBUPXXX'         IEBUPDTE OR IEBUPDAT       12/88 SBG 45053901
         DC    C'FDR   '                                      12/88 SBG 45054001
         DC    C'FDRDSF'                                      12/88 SBG 45054101
         DC    C'FDR???'                                      12/88 SBG 45054201
         DC    C'SLICKMNG'                                    12/88 SBG 45054301
         DC    C'TAPEMAP'                                     12/88 SBG 45054401
         DC    C'SMPPTFIN'                                    12/88 SBG 45054501
         DC    C'CBT973'                                      12/88 SBG 45054601
         DC    C'CBTEMPTY'         EMPTY FILE ON THE CBT TAPE 12/88 SBG 45054701
         DC    C'CBT DOC '         DOC FILE FOR THE CBT TAPE  12/88 SBG 45054801
         DC    X'33008003'        UNIT TYPE FOR 3420C CARTS   09/89^SBG 45054901
         DS    0F                 ALIGNMENT                   12/88 SBG 45055001
         DC    F'32720'           BLOCK SIZE OF CBT DOC FILE  12/88 SBG 45055101
         DC    C'-*****  ONE OR MORE FILES HAVE DENSITY INDICATED INCOR$45055201
               RECTLY IN LABELS.  ALL FILES ARE WRITTEN AT XXXX BPI  **$45055301
               ***'                                                     45055401
LITMVLEN EQU   *-LITMOVES                                     12/88 SBG 45055501
************************************************************* 12/88 SBG 45056001
*    END OF LITERAL MOVES.                                  * 12/88 SBG 45056101
************************************************************* 12/88 SBG 45056201
         EJECT                                                12/88 SBG 45057001
*************************************************************  1/93 RT  45054804
*    TODAY CSECT                                            *  1/93 RT  45054904
*                                                           *  1/93 RT  45055004
*   PARAMETERS:                                             *  1/93 RT  45055104
*     DATE FIELD (LEN 22)                                   *  1/93 RT  45055207
*     DAY FIELD (LEN 9)                                     *  1/93 RT  45055304
*     TIME FIELD (LEN 9)                                    *  1/93 RT  45055404
*                                                           *  1/93 RT  45055504
*                                                           *  1/93 RT  45055604
*************************************************************  1/93 RT  45055704
TODAY    CSECT                                                 1/93 RT  45055907
         DROP  R12,R11,R7,R9,R13                               1/93 RT  45056007
         USING *,R15                                           1/93 RT  45056107
         B     TODARND                                         1/93 RT  45056207
         DC    AL1(25)                                         1/93 RT  45056307
         DC    CL8'TODAY'                                      1/93 RT  45056407
         DC    CL8'&SYSDATE'                                   1/93 RT  45056507
         DC    CL8'&SYSTIME'                                   1/93 RT  45056607
TODARND  DS    0H                                              1/93 RT  45056707
         SAVE  (14,12)                                         1/93 RT  45056807
         DROP  R15                                             1/93 RT  45056907
         USING TODAY,R4                                        1/93 RT  45057007
         LM    R5,R7,0(R1)          LOAD THE PARAMETER PTRS    1/93 RT  45057107
         LR    R4,R15                                          1/93 RT  45057207
         LA    R1,TODSAVE                                      1/93 RT  45057307
         ST    R1,8(,R13)           FWD SAVE-AREA CHAIN PTR    1/93 RT  45057407
         ST    R13,4(,R1)           BKWD SAVE-AREA CHAIN PTR   1/93 RT  45057507
         LR    R13,R1               NEW SAVE-AREA ADDR         1/93 RT  45057607
         TIME  DEC                      GET TIME                        45057807
         ST    R0,TODDWT                PUT TIME INTO WORK AREA         45057907
         ST    R1,TODPDAT               PUT DATE INTO WORK AREA         45058007
         MVC   TOD1DDT(40),TOD1DDT-1    BLANK THE DATE, DAY, TIME
         AP    TODPDAT,=P'1900000'      CONVERT CCYY TO YYYY   6/03 JCE
         UNPK  TOD1DATE+14(7),TODPDAT(4) UNPK THE DATE         6/03 JCE 45058107
         OI    TOD1DATE+20,C'0'         FIX THE SIGN                    45058207
         MVC   TOD1DATE+8(4),TOD1DATE+14 SAVE YEAR YYYY        6/03 JCE 45058507
         MVC   TOD1DATE+15(2),TOD1DATE+16   MOVE THE YEAR OVER 6/03 JCE 45058907
         MVI   TOD1DATE+17,C'.'         PUT IN THE DOT                  45059007
         MVI   TOD1DATE+6,C','          PUT IN THE COMMA                45059107
         MVI   TOD1DATE+21,C')'         PUT IN THE PARENTHESES          45059207
         MVI   TOD1DATE+14,C'('                                         45059307
*        LEAP YEAR DETERMINATION BELOW BREAKS IN 2100          6/03 JCE
         TM    TODPDAT+1,X'01'          POSSIBLE LEAP YEAR?             45059407
         BO    TODNOLP                   NO (ODD YEAR) - NOT LEAP       45059507
         TM    TODPDAT+1,X'12'          DIVISIBLE BY 4?                 45059607
         BM    TODNOLP                   NO - NOT LEAP YEAR             45059707
         MVI   MONTHS+7,29              FEB HAS 29 DAYS THIS YEAR       45059807
TODNOLP  MVC   TODDWD+6(2),TODPDAT+2    COPY DDDF                       45059907
         XC    TODDWD(6),TODDWD         CLEAR BEGINNING                 45060007
         CVB   R0,TODDWD                GET DAY OF YEAR IN BINARY       45060107
         LA    R15,MONTHS               POINT TO TABLE                  45060207
TODFINMO SH    R0,0(,R15)               SUB NUMBER OF DAYS THIS MONTH   45060307
         BNP   TODGOTMO                 HAVE MONTH                      45060407
         LA    R15,6(,R15)              NEXT TABLE ENTRY                45060507
         B     TODFINMO                 KEEP LOOKING                    45060607
TODGOTMO MVC   TOD1DATE(3),2(R15)       MOVE NAME OF MONTH              45060707
         AH    R0,0(,R15)               GET BACK CORRECT RESIDUAL       45060807
         CVD   R0,TODDWD                                                45060907
         UNPK  TOD1DATE+4(2),TODDWD+6(2)  DAY OF MONTH                  45061007
         OI    TOD1DATE+5,C'0'          FIX SIGN                        45061107
         MVC   TOD1DATE+10(2),TOD1DATE+15 MOVE IN YEAR                  45061207
*        REWORK OF DAY-OF-WEEK CALC USING A SIMPLER, MORE      6/03 JCE
*    OBVIOUS ALGORITHM BASED ON LILIAN DATES.                  6/03 JCE
*    DAY 1 LILIAN = FRIDAY, THUS MOD(LILIANDAY+4,7) = 0...6    6/03 JCE
*    CORRESPONDING TO SUN ... SAT                              6/03 JCE
*     FOR DATE YYYY.DDD, IF X = YYYY-1201 THEN                 6/03 JCE
*    LILIANDAY + 4 = FLOOR(X*365.25) - 139444                  6/03 JCE
*                           - FLOOR(X/100)                     6/03 JCE
*                           + FLOOR(X/400)   + DDD  + 4        6/03 JCE
*    THE ABOVE FORMULA AND CODE IS BORROWED FROM A DATE        6/03 JCE
*    CONVERSION ROUTINE D1MADRES WRITTEN BY J C EWING AT       6/03 JCE
*    DATA-TRONICS CORP IN AUGUST 1991, AND IS A MATHEMATICAL   6/03 JCE
*    SIMPLIFICATION OF THE ALGORITHM PUBLISHED BY B.G. OHMS IN 6/03 JCE
*    'COMPUTER PROCESSING OF DATES OUTSIDE THE TWENTIETH       6/03 JCE
*    CENTURY', IBM SYSTEMS JOURNAL, VOL 25 NO 2, 1986, P. 247. 6/03 JCE
         ZAP   TODDWD,=P'0'                                    6/03 JCE
         MVO   TODDWD,TODPDAT(2)  00 00 00 00 00 0Y YY YC      6/03 JCE
         CVB   R1,TODDWD          YEAR IN BINARY               6/03 JCE
         SH    R1,=H'1201'        YEAR - 1201                  6/03 JCE
         LR    R15,R1                                          6/03 JCE
         MH    R15,=AL2((36525*4)/100)  (Y-1201)*365.25*4      6/03 JCE
         SRA   R15,2               R15= FLOOR((Y-1201)*365.25) 6/03 JCE
         S     R15,=F'139440'      UNADJ DAYS + 4              6/03 JCE
         SR    R0,R0               COMPUTE CENTURY ADJUSTMENT  6/03 JCE
         D     R0,=F'100'          R1=CENTURY LEAP DAYS        6/03 JCE
         SR    R15,R1              SUBTRACT CENTURY LEAP DAYS  6/03 JCE
         SRA   R1,2                400-YR LEAP DAYS            6/03 JCE
         AR    R15,R1              ADD BACK 400 YR LEAP DAYS   6/03 JCE
         ZAP   TODDWD,TODPDAT+2(2) EXTRACT DDD                 6/03 JCE
         CVB   R1,TODDWD           R1=DDD                      6/03 JCE
         AR    R15,R1              DAYS FROM OCT 15, 1582 + 4  6/03 JCE
* Z=MOD(D,7)+1                                                          45064707
         XR    R14,R14                  CLEAR FOR DIVIDE                45064807
         D     R14,=F'7'                GET MOD(D,7) IN R4=Z            45064907
         MH    R14,=Y(L'DAYS)           OFFSET INTO TABLE               45065007
         LA    R14,DAYS(R14)            POINT TO DAY IN TABLE           45065107
         MVC   TOD1DAY(L'DAYS),0(R14)   DAY TO OUTPUT LOCATION          45065207
         OI    TODDWT+3,X'0F'           FIX THE SIGN                    45065307
         MVC   TOD1TIME(9),=X'4021207A20207A2020'    ' DD:DD:DD'        45065407
         ED    TOD1TIME(9),TODDWT                                       45065507
         MVC   0(22,R5),TOD1DATE                               1/93 RT  45065607
         MVC   0(9,R6),TOD1DAY                                 1/93 RT  45065707
         MVC   0(9,R7),TOD1TIME                                1/93 RT  45065807
         L     R13,4(,R13)                                     1/93 RT  45065907
         MVI   12(R13),X'FF'        INVALIDATE FWD-CHAIN       1/93 RT  45066007
         RETURN (14,12),RC=0                                   1/93 RT  45066107
         SPACE 1                                                        45066205
DAYS     DAYS  LEFT                                                 THO 45066305
         SPACE 1                                                    THO 45066405
MONTHS   MONTHS                                                     THO 45066505
         LTORG                                                          45066605
         DROP  R4                                                       45066704
TODSAVE  DS    9D                                                       45066804
TODDWT   DS    D                    PACKED TIME                         45066907
TODDWD   DS    D                    PACKED DATE WORK FIELD              45067007
TODPDAT  DS    F                    PACKED DATE                         45067107
         DC    CL8' '
TOD1DDT  DS    0CL40          DATE, DAY, TIME TAKEN TOGETHER
TOD1DATE DC    CL22' '                                                  45067207
TOD1DAY  DC    CL9' '                                                   45067304
TOD1TIME DC    CL9' '                                                   45067404
         DC    CL8' '
         EJECT                                                          45067504
         XYZZY NOGEN                                                    45067602
         DCBD  DSORG=PS,DEVD=DA                                     THO 45060000
UCB      DSECT                                                      THO 45070000
         IEFUCBOB                                                   THO 45080000
         AIF   ('&DEBX' EQ 'YES').NEWDEB1                           SBG 45081005
         IEZDEB                                                     THO 45081105
         AGO   .OLDDEB1                                             SBG 45082005
.NEWDEB1 ANOP                                                       SBG 45090105
         IECTDEBX                                                   SBG 45091003
.OLDDEB1 ANOP                                                       SBG 45097005
         END   TAPEMAP                                                  45100000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR     <-- TARGET LOAD LIBRARY 00080000
//LKED.SYSIN DD *                                                       00090000
 SETCODE AC(0)                                                          00100000
 NAME    TAPEMAP(R)                                                     00110000
//                                                                      00120000
