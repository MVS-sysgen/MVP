//VOLS  JOB (TSO),
//             'Install VOLS',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00020000
//* ***************************************************************** * 00030000
//* INSTALL VOLS COMMAND IN SYS2.CMDLIB (HELP IN SYS2.HELP)           * 00040000
//* ***************************************************************** * 00050000
//*                                                                     00060000
//VOLS    EXEC ASMFCL,MAC1='SYS1.AMODGEN',                              00070000
//             PARM.ASM='LIST,NODECK,LOAD,RENT',                        00071000
//             PARM.LKED='LIST,MAP,NOCAL,RENT'                          00080000
//ASM.SYSIN DD *                                                        00090000
         MACRO                                                          00100000
&LABEL   BEGIN &SAVAREA,       * NAME OF SAVE AREA IN PROGRAM          *00110000
               &NAME,          * NAME ON STM INSTRUCTION IF &STMLBL='' *00120000
               &CSECT=YES,       YES - GENERATE CSECT IF NONE FOUND    *00130000
               &TYPE=CSECT,      TYPE OF ENTRY: CSECT OR ENTRY         *00140000
               &BASE=,           UP TO 3 BASE REGS R,(R,R),OR(R,R,R)   *00150000
               &SAVE=,           ADDRESS OR SPECIFICATION OF SAVEAREA  *00160000
               &TITSTMT=YES,     YES - GENERATE A TITLE STATEMENT      *00170000
               &TITLBL=*DFLT,    LABEL FOR TITLE STATEMENT             *00180000
               &TITLE=,          TITLE INFORMATION                     *00190000
               &STMLBL=,         LABEL FOR THE STM STATEMENT           *00200000
               &NOP=NO,          YES - GENERATE NOPR'S FOR ZAPPING     *00210000
               &REQUS=YES,       YES - GENERATE REGISTER EQUATES       *00220000
               &R=R,             CHARACTERS TO PREFIX REGISTER NUMBERS *00230000
               &GEN=NOGEN,       LIST CODE FOR GETMAINING SAVE AREA    *00240000
               &USING13=YES,     YES - INCLUDE USING R13 FOR &SAVAREA  *00250000
               &VER=,            VERSION OF CODE              (4 CHAR) *00260000
               &ID=,           * HAS NO FUNCTION - FOR COMPATIBILITY   *00270000
               &ICATCH=YES,      YES - INCLUDE WSRCC BEGIN EYECATCH    *00280000
               &BUREAU=WSRCC,    BUREAU RESPONSIBLE FOR CODE  (7 CHAR) *00290000
               &SECTION=TECHSUP, SECTION RESPONSIBLE FOR CODE (7 CHAR) *00300000
               &UNIT=SYSSOFT,    UNIT RESPONSIBLE FOR CODE    (7 CHAR) *00310000
               &AUTHOR=***       AUTHOR'S INITIALS            (3 CHAR)  00320000
.*                                                                      00330000
.* ABOVE COMMENTS PRECEEDED BY A ASTERISK ('*') INDICATE PARAMETER      00340000
.* IS INCLUDED FOR COMPATIBILITY ONLY AND SHOULD NOT BE USED            00350000
.*                                                                      00360000
.*                                                                      00370000
         GBLB  &RFLAG                                                   00380000
         GBLC  &RLEN,&RSP                                               00390000
         LCLC  &CLABEL,&SCODE,&SREG1,&SREG2,&SLBL,&SAVNAME,&BREG1       00400000
         LCLC  &TITLBLC,&TITL,&TITLVER,&VERTEMP                         00410000
.*                                                                      00420000
         LCLC  &C,&SPOT,&HDR,&CVAL                                      00430000
.*                                                                      00440000
.*                                                                      00450000
.*                                                                      00460000
         MNOTE *,'***WSRCC REENTRANT BEGIN MACRO V01.01***'             00470000
.*                  WITH THANKS TO HEWLETT-PACKARD BAEDP                00480000
.*                                                                      00490000
.*                                                                      00500000
&CLABEL  SETC  '&SYSECT'                                                00510000
         AIF   ('&CSECT' NE 'YES').CLBL99                               00520000
&CLABEL SETC   '&LABEL'                                                 00530000
         AIF   ('&CLABEL' NE '').CLBL99                                 00540000
&CLABEL  SETC  'PRIVATE'                                                00550000
         MNOTE *,'*** NO LABEL ON BEGIN, &TYPE CALLED ''PRIVATE'' ***'  00560000
.CLBL99  ANOP                                                           00570000
.*                                                                      00580000
.*                                                                      00590000
         AIF    ('&TYPE' NE 'ENTRY').ENT99                              00600000
&CLABEL  DS    0H                                                       00610000
         ENTRY &CLABEL                                                  00620000
         AGO   .CST99                                                   00630000
.ENT99   ANOP                                                           00640000
.*                                                                      00650000
         AIF   ('&CSECT' NE 'YES').CST99                                00660000
&CLABEL  CSECT                                                          00670000
.CST99   ANOP                                                           00680000
.*                                                                      00690000
.*                                                                      00700000
.*                                                                      00710000
&SLBL    SETC  '&STMLBL'                                                00720000
         AIF   ('&SLBL' NE '').S1                                       00730000
&SLBL    SETC  '&NAME'                                                  00740000
.S1      ANOP                                                           00750000
.*                                                                      00760000
.*                                                                      00770000
         USING &CLABEL,&R.15                                            00780000
&C       SETC  'C'                                                      00790000
&SPOT    SETC  '&SYSECT'                                                00800000
         AIF   ('&CLABEL' EQ '').LBL99                                  00810000
&SPOT    SETC  '&CLABEL'                                                00820000
         AIF   ('&TYPE' NE 'ENTRY').LBL99                               00830000
&C       SETC  'E'                                                      00840000
.LBL99   ANOP                                                           00850000
.*                                                                      00860000
.*                                                                      00870000
.*   PAD THE VERSION WITH _'S                                           00880000
&VERTEMP SETC  '____'                                                   00890000
         AIF   ('&VER' EQ '').VERNULL                                   00900000
&VERTEMP SETC  '&VER'                                                   00910000
         AIF   ('&VER'(1,1) NE '''').NOQUOTE                            00920000
&VERTEMP SETC  '&VER'(2,K'&VER-2)                                       00930000
.NOQUOTE ANOP                                                           00940000
         LCLC  &PADSTG                                                  00950000
&PADSTG  SETC  (4)'_'                                                   00960000
&PADSTG  SETC  '&VERTEMP'.'&PADSTG'                                     00970000
&VERTEMP SETC  '&PADSTG'(1,4)                                           00980000
.VERNULL ANOP                                                           00990000
.*                                                                      01000000
.*                                                                      01010000
&HDR     SETC  'CL4''&VERTEMP.'''                                       01020000
.HDR99   ANOP                                                           01030000
.*                                                                      01040000
         AIF   ('&ICATCH' NE 'YES').NOICTCH                             01050000
         B     *+68                                                     01060000
         DC    AL1(64),CL3'ID='                                         01070000
         DC    CL8'&SPOT'                                               01080000
         DC    C'&C',AL3(&SPOT)                                         01090000
         DC    &HDR                                                     01100000
         DC    C'_',C'&SYSDATE.@&SYSTIME._'                             01110000
         DC    CL7'&BUREAU',C'-'                                        01120000
         DC    CL7'&SECTION',C':'                                       01130000
         DC    CL7'&UNIT',C','                                          01140000
         DC    CL3'&AUTHOR',C'.'                                        01150000
.NOICTCH ANOP                                                           01160000
&SLBL    STM   &R.14,&R.12,12(&R.13)                                    01170000
.*                                                                      01180000
         AIF ('&NOP' EQ 'NO').NONOP                                     01190000
         CNOP  0,4                                                      01200000
         B     *+20                                                     01210000
         DC    CL16'ZAP TO 17FF07FE:'                                   01220000
         NOPR  R15         CAN ZAP TO 17FF ( XR  R15,R15 )              01230000
         NOPR  R14         CAN ZAP TO 07FE ( BR  R14     )              01240000
.NONOP   ANOP                                                           01250000
         DROP  &R.15                                                    01260000
.*                                                                      01270000
.*                                                                      01280000
&BREG1   SETC  '&BASE(1)'                                               01290000
         AIF   ('&BREG1' NE '').B10                                     01300000
&BREG1   SETC '&R.12'                                                   01310000
         MNOTE *,'** NO BASE REGISTER SPECIFIED ASSUME REGISTER 12 **'  01320000
.B10     ANOP                                                           01330000
         LR    &BREG1,&R.15                                             01340000
         USING &CLABEL,&BREG1                                           01350000
         AIF   ('&BASE(2)' EQ '').B999                                  01360000
         LA    &BASE(2),4095(&BREG1)                                    01370000
         LA    &BASE(2),1(&BASE(2).)                                    01380000
         USING &CLABEL+4096,&BASE(2)                                    01390000
         AIF   ('&BASE(3)' EQ '').B999                                  01400000
         LA    &BASE(3),4095(&BASE(2).)                                 01410000
         LA    &BASE(3),1(&BASE(3).)                                    01420000
         USING &CLABEL+8192,&BASE(3)                                    01430000
.B999    ANOP                                                           01440000
.*                                                                      01450000
         AIF   ('&SAVE' EQ 'NONE').SV99                                 01460000
&SAVNAME SETC  '&SAVE(1)'                                               01470000
&RLEN    SETC  '&SAVE(2)'                                               01480000
&RSP     SETC  '&SAVE(3)'                                               01490000
         AIF   ('&SAVNAME' NE '').SV10                                  01500000
&SAVNAME SETC  '&SAVAREA'                                               01510000
.SV10    ANOP                                                           01520000
         AIF   ('&SAVE(1)' NE '' OR '&SAVE(2)'  NE '' OR '&SAVE(3)' NE *01530000
               '').SV20                                                 01540000
         AIF   ('&SAVAREA' NE '').SVNRENT                               01550000
         AGO   .SVRENT                                                  01560000
.SV20    ANOP                                                           01570000
         AIF   ('&SAVE(2)' EQ '' AND '&SAVE(3)' EQ '').SVNRENT          01580000
         AGO   .SVRENT                                                  01590000
.*                                                                      01600000
.*                                                                      01610000
.SVNRENT ANOP                                                           01620000
&RFLAG   SETB  0                                                        01630000
         LR    &R.15,&R.13                                              01640000
         LA    &R.13,&SAVNAME                                           01650000
         ST    &R.15,4(&R.13)                                           01660000
         ST    &R.13,8(&R.15)                                           01670000
         AGO   .SV99                                                    01680000
.*                                                                      01690000
.*                                                                      01700000
.SVRENT  ANOP                                                           01710000
&RFLAG   SETB  1                                                        01720000
         AIF   ('&RLEN' NE '').SVRSET                                   01730000
&RLEN    SETC  '72'                                                     01740000
         MNOTE *,'*** REENTRANT SAVE AREA FORMAT USED ***'              01750000
.SVRSET  ANOP                                                           01760000
.*                                                                      01770000
.********* FOLLOWING FIX APPLIED 15 JAN 79 - DP *********************   01780000
.* IF &RLEN IS AN 'L'' TYPE CONSTANT, DOUBLE-UP THE QUOTES FOR MNOTE    01790000
         GBLC  &RLENA,&RLENB                                            01800000
         LCLA  &LRLEN                                                   01810000
&RLENA   SETC  ''                                                       01820000
&RLENB   SETC  '&RLEN'                                                  01830000
         AIF   ('&RLEN'(1,2) NE 'L''').NORLQ                            01840000
&RLENA   SETC  'L'''''                                                  01850000
&LRLEN   SETA  K'&RLEN-2                                                01860000
&RLENB   SETC  '&RLEN'(3,&LRLEN)                                        01870000
.NORLQ   ANOP                                                           01880000
         MNOTE *,'*      GETMAIN R,LV=&RLENA.&RLENB,SP=&RSP'            01890000
         PUSH  PRINT                                                    01900000
         PRINT &GEN                                                     01910000
         GETMAIN R,LV=&RLEN,SP=&RSP                                     01920000
         POP   PRINT                                                    01930000
         LR    &R.15,&R.13                                              01940000
         LR    &R.13,&R.1                                               01950000
         ST    &R.15,4(&R.13)                                           01960000
         ST    &R.13,8(&R.15)                                           01970000
         LM    &R.15,&R.1,16(&R.15)    RESET 0, 1, AND 15               01980000
         AGO   .SV99                                                    01990000
.*                                                                      02000000
.*                                                                      02010000
.SV99    ANOP                                                           02020000
.*                                                                      02030000
.*                                                                      02040000
         AIF   ('&SAVNAME' EQ '').UTH99                                 02050000
         AIF   ('&USING13' NE 'YES').UTH99                              02060000
         USING &SAVNAME,&R.13                                           02070000
.UTH99   ANOP                                                           02080000
.*                                                                      02090000
         AIF   ('&REQUS' NE 'YES').REQNO                                02100000
         MNOTE *,'REGISTER NAMES &R.0 THROUGH &R.15 ARE ASSIGNED'       02110000
&R.0     EQU   0                                                        02120000
&R.1     EQU   1                                                        02130000
&R.2     EQU   2                                                        02140000
&R.3     EQU   3                                                        02150000
&R.4     EQU   4                                                        02160000
&R.5     EQU   5                                                        02170000
&R.6     EQU   6                                                        02180000
&R.7     EQU   7                                                        02190000
&R.8     EQU   8                                                        02200000
&R.9     EQU   9                                                        02210000
&R.10    EQU   10                                                       02220000
&R.11    EQU   11                                                       02230000
&R.12    EQU   12                                                       02240000
&R.13    EQU   13                                                       02250000
&R.14    EQU   14                                                       02260000
&R.15    EQU   15                                                       02270000
.REQNO   ANOP                                                           02280000
.*                                                                      02290000
         AIF   ('&TITSTMT' NE 'YES').TIT99                              02300000
&TITLBLC SETC  '&CLABEL'                                                02310000
         AIF   ('&TITLBL' EQ '*DFLT').TIT50                             02320000
&TITLBLC SETC  '&TITLBL'                                                02330000
.TIT50   ANOP                                                           02340000
&TITL    SETC  '&TITLE'                                                 02350000
         PUSH  PRINT                                                    02360000
         PRINT OFF       TITLE STATEMENT FOLLOWS                        02370000
         AIF   ('&TITLE' NE '').TIT79                                   02380000
&TITL    SETC  ''' '''                                                  02390000
         AIF   ('&VER' EQ '').TIT79                                     02400000
&TITLVER SETC  '&VER'                                                   02410000
&TITL    SETC  '''VER '.'&TITLVER'.''''                                 02420000
.TIT79   ANOP                                                           02430000
         AIF   ('&TITL'(1,1) EQ '''').TIT89                             02440000
&TITL    SETC  ''''.'&TITL'.''''                                        02450000
.TIT89   ANOP                                                           02460000
&TITLBLC TITLE &TITL                                                    02470000
         POP   PRINT                                                    02480000
.TIT99   ANOP                                                           02490000
         MEND                                                           02500000
         MACRO                                                          02510000
&LABEL   FINISH  &RC=0,                ABSOLUTE OR (R) FOR RETURN CODE *02520000
               &R=,                    PREFIX FOR REGISTERS            *02530000
               &GEN=NOGEN,             GEN - LIST CODE FOR FREEMAIN    *02540000
               &RC1=,                  ABS OR (R) VALUE RETURNED IN R1 *02550000
               &RTRNOFF=,              RETURN OFFSET VALUE             *02560000
               &RETURN=YES             YES - GENERATE CODE FOR RETURN   02570000
.*                                                                      02580000
.*                                                                      02590000
.*                                                                      02600000
         GBLB  &RFLAG                                                   02610000
         GBLC  &RLEN,&RLENA,&RLENB,&RSP,&CT                             02620000
         LCLC  &OPCD,&OPC1,&RETCODE,&RETCD1                             02630000
.*                                                                      02640000
.*                                                                      02650000
          AIF   ('&SYSLIST(1)' EQ '').ERR99                             02660000
          MNOTE 8,'*** PARTIAL REGISTER LIST NO LONGER SUPPORTED IN BEG*02670000
                IN OR FINISH ***'                                       02680000
.ERR99   ANOP                                                           02690000
.*                                                                      02700000
.*                                                                      02710000
&RETCODE SETC  '&RC(1)'                                                 02720000
         AIF   ('&RETCODE' NE '').RC99                                  02730000
&RETCODE SETC  '0'                                                      02740000
         MNOTE *,'*** NO RETURN CODE SPECIFIED ** ZERO ASSUMED ***'     02750000
.RC99    ANOP                                                           02760000
.*                                                                      02770000
.*                                                                      02780000
&OPCD    SETC  'LR'                                                     02790000
         AIF   ('&RC'(1,1) EQ '(').OP99                                 02800000
&OPCD    SETC  'LA'                                                     02810000
.OP99    ANOP                                                           02820000
.*                                                                      02830000
.*                                                                      02840000
.*                                                                      02850000
&RETCD1  SETC  '&RC1(1)'                                                02860000
         AIF   ('&RETCD1' EQ '').OP199                                  02870000
.*                                                                      02880000
.*                                                                      02890000
&OPC1    SETC  'LR'                                                     02900000
         AIF   ('&RC1'(1,1) EQ '(').OP199                               02910000
&OPC1    SETC  'LA'                                                     02920000
.OP199   ANOP                                                           02930000
         CNOP  0,4                                                      02940000
&LABEL   OI    4(&R.13),X'FF'                                           02950000
         AIF   (NOT &RFLAG).GOHOME                                      02960000
.*                                                                      02970000
.*                                                                      02980000
.RENT   ANOP                                                            02990000
         AIF   ('&OPCD' NE 'LR').RENT10                                 03000000
         LR    &R.2,&RETCODE                                            03010000
&RETCODE SETC  '&R.2'                                                   03020000
.RENT10  ANOP                                                           03030000
         AIF   ('&OPC1' NE 'LR').RENT20                                 03040000
         LR    &R.3,&RETCD1                                             03050000
&RETCD1  SETC  '&R.3'                                                   03060000
.RENT20  ANOP                                                           03070000
         LR    &R.1,&R.13                                               03080000
         L     &R.13,4(&R.13)                                           03090000
         MNOTE *,'*      FREEMAIN R,LV=&RLENA.&RLENB,A=(1),SP=&RSP'     03100000
         PUSH  PRINT                                                    03110000
         PRINT &GEN                                                     03120000
         FREEMAIN R,LV=&RLEN,A=(1),SP=&RSP                              03130000
         POP   PRINT                                                    03140000
         AGO   .COMEND                                                  03150000
.*                                                                      03160000
.*                                                                      03170000
.*                                                                      03180000
.GOHOME ANOP                                                            03190000
         L     &R.13,4(&R.13)                                           03200000
.COMEND  ANOP                                                           03210000
         &OPCD &R.15,&RETCODE                                           03220000
         L     &R.14,12(&R.13)                                          03230000
         AIF   ('&OPC1' EQ '').GH90                                     03240000
         L     &R.0,20(&R.13)                                           03250000
         &OPC1 &R.1,&RETCD1                                             03260000
         LM    &R.2,&R.12,28(&R.13)                                     03270000
         AGO   .GH99                                                    03280000
.GH90    ANOP                                                           03290000
         LM    &R.0,&R.12,20(&R.13)                                     03300000
.GH99    ANOP                                                           03310000
.*                                                                      03320000
         AIF   ('&RETURN' NE 'YES').RET99                               03330000
         MVI   12(&R.13),X'FF'                                          03340000
.*                                                                      03350000
         AIF   ('&RTRNOFF' EQ '').BR14                                  03360000
         B     &RTRNOFF.(&R.14)      TAKE SPECIAL RETURN WITH OFFSET    03370000
         AGO   .RET99                                                   03380000
.BR14    ANOP                                                           03390000
         BR    &R.14                                                    03400000
.RET99   ANOP                                                           03410000
         MEND                                                           03420000
         TITLE 'WSRCC - VOLS TSO C.P.  --  VERSION 6A'              #6A 03430000
         PRINT ON,NOGEN                                                 03440000
*********************************************************************** 03450000
*                                                                     * 03460000
*  THIS PROGRAM (COMMAND) WILL LIST THE AVAILABLE FREE SPACE ON ALL   * 03470000
*  OR SELECTED ONLINE DISK PACKS.  AS IT IS HERE, IT WILL NOT WORK    * 03480000
*  UNDER MVS/XA.  THIS CP WAS ORIGIONALLY FROM CAMBRIDAGE SYSTEMS     * 03490000
*  GROUP AND WAS DISTRIBUTED IN AN "AS IS" FORM WITH A VERY EARLY     * 03500000
*  VERSION OF THE ASM2 PACKAGE.  THIS CP CONTAINED NO COPYRITE NOTICE * 03510000
*  IN ANY FORM .  I HAVE RECEIVED PERMISSION FROM CSG TO PUT THIS ON  * 03520000
*  THE CBT TAPE.  - J.MARTIN  02/15/85                                * 03530000
*                                                                     * 03540000
*********************************************************************** 03550000
         SPACE 1                                                        03560000
*--------------MODIFICATION.LOG---------------------------------------* 03570000
*                                                                     * 03580000
*  PROGRAMMER     DATE     SUMMARY OF CHANGES MADE TO THIS PROGRAM    * 03590000
*--------------+--------+---------------------------------------------* 03600000
* TOM JARVIS    01/21/80  1. CHANGED CONTROL UNIT NAME CHECK SO THAT  * 03610000
*                            ALL DISK VOLUMES ARE LISTED.             * 03620000
* JOE MARTIN    03/29/82  2. ADDED THE UNIT ATTR DISPLAY FOR EACH     * 03630000
*                            VOLUME. DISPLAY IS STOR, PRIV OR PUBL.   * 03640000
*                            ADDED THE BEGIN AND FINISH MACROS.       * 03650000
*                            MOVED MOST OF WORKAREA TO DSECT.         * 03660000
*                                                                     * 03670000
* JOE MARTIN    05/20/82  3. ADDED SOME SELECTION TO THE VOLS COMMAND.* 03680000
*                            FORMAT IS "TYPE(....) (DEVT AND ATTR)    * 03690000
*                            WHERE THE "...." IS DEVICE TYPE OR       * 03700000
*                            OR DEVICE ATTRIBUTE. EXAMPLE:            * 03710000
*                            VOLS TYPE(3350) -- SELECT ONLY 3350'S    * 03720000
*                            VOLS TYPE(PRIV) -- SELECT PRIVATE VOLS.  * 03730000
*                                                                     * 03740000
* JOE MARTIN    11/02/82  4. ADDED NUMBER OF OPEN DCB'S TO THE        * 03750000
*                            OUTPUT DISPLAY FOR THE VOLS COMMAND.     * 03760000
*                                                                     * 03770000
* JOE MARTIN    03/21/83  5. ADDED SELECTION FOR VOLSER OR UCB ADDR   * 03780000
*                            ADDED PUTLINE SUPPORT REPLACING TPUT.    * 03790000
*                            FIXED PROBLEM WITH UCB TABLE LOOKUP RTN. * 03800000
*                                                                     * 03810000
* JOE MARTIN    03/25/83  6. ADDED CLEAR KEYWORD TO CLEAR THE SCREEN  * 03820000
*                            WHEN RUNNING UNDER VTAM.                 * 03830000
*                                                                     * 03840000
* JAY MOSELEY   10/26/04  6A. ADDED SUPPORT FOR 3390 (LEFT 2314 IN)   * 03850000
*                             USING SAM GOLUB'S CHANGES FOR MODEL.    * 03860000
*                             INTENDED ONLY FOR USE UNDER MVS 3.8J.   * 03870000
*                                                                     * 03880000
* JAY MOSELEY   01/18/15  6B. ADDED SUPPORT FOR 3340 AND 3375         * 03890000
*                             USING SAM GOLUB'S CHANGES FOR MODEL.    * 03900000
*                             INTENDED ONLY FOR USE UNDER MVS 3.8J.   * 03910000
*                                                                     * 03920000
*---------------------------------------------------------------------* 03930000
         SPACE 2                                                        03940000
VOLS     BEGIN SAVE=(SAVEAREA,@SL,17),AUTHOR=CSG,VER=006A           #6A 03950000
         SPACE 1                                                        03960000
         LR    R11,R1                  POINT AT CPPL.               #3  03970000
         USING CPPL,R11                                                 03980000
         ST    R11,VOLCPPL             SAVE CPPL ADDRESS            #3  03990000
         LA    R7,VIOPL                POINT AT IOPL                #5  04000000
         USING IOPL,R7                 SET UP ADDRESSIBILITY - IOPL #5  04010000
         LA    R9,VOLPPL               POINT TO PPL                 #3  04020000
         USING PPL,R9                  SET UP ADDRESSABILITY TO PPL #3  04030000
         L     R10,CPPLPSCB            POINT AT PSCB.               #3  04040000
         ST    R10,VOLPSCB             STORE BUFFER ADDRESS IN PPL  #3  04050000
         L     R10,CPPLCBUF            COMMAND BUFFER               #3  04060000
         ST    R10,PPLCBUF             STORE BUFFER ADDRESS IN PPL  #3  04070000
         L     R10,CPPLUPT             USER PROFILE TABLE           #3  04080000
         ST    R10,PPLUPT              STORE UPT IN PPL             #3  04090000
         ST    R10,IOPLUPT             STORE UPT IN IOPL            #5  04100000
         L     R10,CPPLECT             ENVIRONMENT CONTROL TABLE    #3  04110000
         ST    R10,PPLECT              STORE ECT IN PPL             #3  04120000
         ST    R10,IOPLECT             STORE ECT IN IOPL            #5  04130000
         L     R10,VOLPCLP             PARSE CONTROL LIST           #3  04140000
         ST    R10,PPLPCL              STORE PCL IN PPL             #3  04150000
         LA    R10,VOLECB              EVENT CONTROL BLOCK          #3  04160000
         ST    R10,PPLECB              STORE ECB IN PPL             #3  04170000
         ST    R10,IOPLECB             STORE ECB IN IOPL            #5  04180000
         LA    R10,VOLANS              ANSWER AREA                  #3  04190000
         ST    R10,PPLANS              STORE ANSWER ADDRESS IN PPL  #3  04200000
         LA    R10,VOLUWA              USER WORK AREA               #3  04210000
         ST    R10,PPLUWA              STORE WORK ADDRESS IN PPL    #3  04220000
         DROP  R7,R9,R11                                            #6A 04230000
         MVI   LINEFLG,C'N'            INITIALIZE DEVICE SELECT FLG #5  04240000
         LR    R1,R9                   POINTER TO PPL IN R1         #3  04250000
         CALLTSSR EP=IKJPARS           CALL PARSER                  #3  04260000
         LTR   R15,R15                 CHECK FOR ZERO PARSE RETURN  #3  04270000
         BNZ   FINISH                  GO AWAY UPSET                #3  04280000
         L     R8,VOLANS               LOAD ANSWER AREA FOR USING   #3  04290000
         USING PCLDSECT,R8             SET UP ADDRESSABILITY TO ANS #3  04300000
         MVI   SCRNFLG,C'N'            SET NOT-SCREEN FLAG          #6  04310000
         SR    R7,R7                   CLEAR R7 FOR POINTER         #6  04320000
         LH    R7,VOLCLEAR             GET POINTER VALUE            #6  04330000
         LTR   R7,R7                   IF PARM NOT PRESENT          #6  04340000
         BZ    CHKVOLTP                   THEN CONTINUE             #6  04350000
         GTSIZE ,                      GET SCREEN DISPLAY SIZE      #6  04360000
         LTR   R0,R0                   IF DISPLAY TERMINAL          #6  04370000
         BZ    CHKVOLTP                   THEN CONTINUE             #6  04380000
         MVI   SCRNFLG,C'Y'            SET SCREEN FLAG              #6  04390000
         BCTR  R0,0                    REDUCE FOR THE "***"         #6  04400000
         ST    R0,MAXLINES             SAVE SCREEN DISPLAY SIZE     #6  04410000
         STFSMODE ON,INITIAL=YES       CLEAR SCREEN                 #6  04420000
         STFSMODE OFF                  CLEAR SCREEN                 #6  04430000
CHKVOLTP EQU   *                                                        04440000
         L     R7,VOLTYPE              POINT AT PARAMETER           #3  04450000
         LH    R6,VOLTYPE+4            GET LENGTH OF PARAMETER      #3  04460000
         BCTR  R6,0                    REDUCE R6 FOR EX OF CLC      #3  04470000
         EX    R6,CLC#ALL              CHECK FOR "ALL"              #3  04480000
         BE    SET#ALL                 YES = GO SETUP FOR "ALL"     #3  04490000
         EX    R6,CLC#PRIV             CHECK FOR "PRIVATE"          #3  04500000
         BE    SET#PRIV                YES = GO SETUP FOR "PRIVATE" #3  04510000
         EX    R6,CLC#PUBL             CHECK FOR "PUBLIC"           #3  04520000
         BE    SET#PUBL                YES = GO SETUP FOR "PUBLIC"  #3  04530000
         EX    R6,CLC#STOR             CHECK FOR "STORAGE"          #3  04540000
         BE    SET#STOR                YES = GO SETUP FOR "STORAGE" #3  04550000
         EX    R6,CLC#3380             CHECK FOR "3380"             #3  04560000
         BE    SET#3380                YES = GO SETUP FOR "3380"    #3  04570000
         EX    R6,CLC#3350             CHECK FOR "3350"             #3  04580000
         BE    SET#3350                YES = GO SETUP FOR "3350"    #3  04590000
         EX    R6,CLC#3330             CHECK FOR "3330"             #3  04600000
         BE    SET#3330                YES = GO SETUP FOR "3330"    #3  04610000
         EX    R6,CLC#3311             CHECK FOR "3311"             #3  04620000
         BE    SET#3311                YES = GO SETUP FOR "3311"    #3  04630000
         EX    R6,CLC#2314             CHECK FOR "2314"             #3  04640000
         BE    SET#2314                YES = GO SETUP FOR "2314"    #3  04650000
         EX    R6,CLC#3390             CHECK FOR "3390"             #6A 04660000
         BE    SET#3390                YES = GO SETUP FOR "3390"    #6A 04670000
         EX    R6,CLC#3375             CHECK FOR "3375"             #6B 04680000
         BE    SET#3375                YES = GO SETUP FOR "3375"    #6B 04690000
         EX    R6,CLC#3340             CHECK FOR "3340"             #6B 04700000
         BE    SET#3340                YES = GO SETUP FOR "3340"    #6B 04710000
         PUTLINE PARM=LINEOUT,OUTPUT=(PBP1,DATA),MF=(E,VIOPL)       #5  04720000
         PUTLINE PARM=LINEOUT,OUTPUT=(PBP2,DATA),MF=(E,VIOPL)       #5  04730000
*        TPUT  BADPARM1,LBPRM1         TELL USER OF BAD PARM VALUE  #3  04740000
*        TPUT  BADPARM2,LBPRM2         TELL USER OF BAD PARM VALUE  #3  04750000
         LA    R15,8                   SET RETRUN CODE              #3  04760000
         B     FINISH                  GO AWAY WITH NO VOLS OUTPUT  #3  04770000
         SPACE                                                          04780000
CLC#ALL  CLC   ADALL(0),0(R7)          EXECUTED COMPARE OF PARM.    #3  04790000
CLC#PRIV CLC   APRIV(0),0(R7)          EXECUTED COMPARE OF PARM.    #3  04800000
CLC#PUBL CLC   APUBL(0),0(R7)          EXECUTED COMPARE OF PARM.    #3  04810000
CLC#STOR CLC   ASTOR(0),0(R7)          EXECUTED COMPARE OF PARM.    #3  04820000
CLC#3380 CLC   D3380(0),0(R7)          EXECUTED COMPARE OF PARM.    #3  04830000
CLC#3350 CLC   D3350(0),0(R7)          EXECUTED COMPARE OF PARM.    #3  04840000
CLC#3330 CLC   D3330(0),0(R7)          EXECUTED COMPARE OF PARM.    #3  04850000
CLC#3311 CLC   D3311(0),0(R7)          EXECUTED COMPARE OF PARM.    #3  04860000
CLC#2314 CLC   D2314(0),0(R7)          EXECUTED COMPARE OF PARM.    #3  04870000
CLC#3390 CLC   D3390(0),0(R7)          EXECUTED COMPARE OF PARM.    #6A 04880000
CLC#3375 CLC   D3375(0),0(R7)          EXECUTED COMPARE OF PARM.    #6B 04890000
CLC#3340 CLC   D3340(0),0(R7)          EXECUTED COMPARE OF PARM.    #6B 04900000
         SPACE                                                          04910000
SET#ALL  EQU   *                                                    #3  04920000
         MVI   SELDEVT,ALLDEVT         SET SELECTION BITS           #6A 04930000
         MVI   SELATTR,ALLATTR         SET SELECTION BITS           #6A 04940000
         B     TPUTH1                  CONTINUE PROCESSING          #3  04950000
SET#PRIV EQU   *                                                    #3  04960000
         MVI   SELATTR,ATTRPRIV        SET SELECTION BIT            #6A 04970000
         MVI   SELDEVT,ALLDEVT         SET BITS FOR DEVICE TYPES    #6A 04980000
         B     TPUTH1                  CONTINUE PROCESSING          #3  04990000
SET#PUBL EQU   *                                                    #3  05000000
         MVI   SELATTR,ATTRPUBL        SET SELECTION BIT            #6A 05010000
         MVI   SELDEVT,ALLDEVT         SET BITS FOR DEVICE TYPES    #6A 05020000
         B     TPUTH1                  CONTINUE PROCESSING          #3  05030000
SET#STOR EQU   *                                                    #3  05040000
         MVI   SELATTR,ATTRSTOR        SET SELECTION BIT            #6A 05050000
         MVI   SELDEVT,ALLDEVT         SET BITS FOR DEVICE TYPES    #6A 05060000
         B     TPUTH1                  CONTINUE PROCESSING          #3  05070000
SET#3380 EQU   *                                                    #3  05080000
         MVI   SELDEVT,DEVT3380        SET SELECTION BIT            #6A 05090000
         MVI   SELATTR,ALLATTR         SET BITS FOR ALL ATTRIBS     #6A 05100000
         B     TPUTH1                  CONTINUE PROCESSING          #3  05110000
SET#3350 EQU   *                                                    #3  05120000
         MVI   SELDEVT,DEVT3350        SET SELECTION BIT            #6A 05130000
         MVI   SELATTR,ALLATTR         SET BITS FOR ALL ATTRIBS     #6A 05140000
         B     TPUTH1                  CONTINUE PROCESSING          #3  05150000
SET#3330 EQU   *                                                    #3  05160000
         MVI   SELDEVT,DEVT3330        SET SELECTION BIT            #6A 05170000
         MVI   SELATTR,ALLATTR         SET BITS FOR ALL ATTRIBS     #6A 05180000
         B     TPUTH1                  CONTINUE PROCESSING          #3  05190000
SET#3311 EQU   *                                                    #3  05200000
         MVI   SELDEVT,DEVT3311        SET SELECTION BIT            #6A 05210000
         MVI   SELATTR,ALLATTR         SET BITS FOR ALL ATTRIBS     #6A 05220000
         B     TPUTH1                  CONTINUE PROCESSING          #3  05230000
SET#2314 EQU   *                                                    #3  05240000
         MVI   SELDEVT,DEVT2314        SET SELECTION BIT            #6A 05250000
         MVI   SELATTR,ALLATTR         SET BITS FOR ALL ATTRIBS     #6A 05260000
         B     TPUTH1                  CONTINUE PROCESSING          #3  05270000
SET#3390 EQU   *                                                    #6A 05280000
         MVI   SELDEVT,DEVT3390        SET SELECTION BIT            #6A 05290000
         MVI   SELATTR,ALLATTR         SET BITS FOR ALL ATTRIBS     #6A 05300000
         B     TPUTH1                  CONTINUE PROCESSING          #6A 05310000
SET#3375 EQU   *                                                    #6B 05320000
         MVI   SELDEVT,DEVT3375        SET SELECTION BIT            #6B 05330000
         MVI   SELATTR,ALLATTR         SET BITS FOR ALL ATTRIBS     #6B 05340000
         B     TPUTH1                  CONTINUE PROCESSING          #6B 05350000
SET#3340 EQU   *                                                    #6B 05360000
         MVI   SELDEVT,DEVT3340        SET SELECTION BIT            #6B 05370000
         MVI   SELATTR,ALLATTR         SET BITS FOR ALL ATTRIBS     #6B 05380000
         B     TPUTH1                  CONTINUE PROCESSING          #6B 05390000
         EJECT                                                          05400000
TPUTH1   EQU   *                                                        05410000
         MVI   SERFLG,C'N'         SET VOLSER FLAG                  #5  05420000
         L     R6,VOLNAME              POINT AT PARM                #5  05430000
         LTR   R6,R6                   IF PARM ADDRESS IS ZERO      #5  05440000
         BZ    TPUTH2                     THEN SKIP MOVE OF VOLSER  #5  05450000
         LH    R7,VOLNAME+4            GET LENGTH OF PARM           #5  05460000
         LTR   R7,R7                   IF LENGTH IS ZERO            #5  05470000
         BZ    TPTH2                      THEN SKIP BCTR            #5  05480000
         BCTR  R7,0                    REDUCE FOR EX OF CLC         #5  05490000
TPTH2    EQU   *                                                        05500000
         STH   R7,SERLEN               SAVE SERIAL NUMBER LENGTH    #5  05510000
         MVI   SERFLG,C'Y'         SET VOLSER FLAG                  #5  05520000
         EX    R7,SERMOVE              MOVE VOLSER TO HOLD AREA     #5  05530000
TPUTH2   EQU   *                                                        05540000
         MVI   UCBFLG,C'N'             SET UCBADR FLAG              #5  05550000
         L     R6,VOLADDR              POINT AT PARM                #5  05560000
         LTR   R6,R6                   IF PARM ADDRESS IS ZERO      #5  05570000
         BZ    TPUTH3                     THEN SKIP MOVE OF UCBADR  #5  05580000
         LH    R7,VOLADDR+4            GET LENGTH OF PARM           #5  05590000
         LTR   R7,R7                   IF LENGTH IS ZERO            #5  05600000
         BZ    TPTH3                      THEN SKIP BCTR            #5  05610000
         BCTR  R7,0                    REDUCE FOR EX OF CLC         #5  05620000
TPTH3    EQU   *                                                        05630000
         STH   R7,UCBLEN               SAVE UCB ADDRESS LENGTH      #5  05640000
         MVI   UCBFLG,C'Y'             SET UCBADR FLAG              #5  05650000
         EX    R7,UCBMOVE              MOVE UCBADR TO HOLD AREA     #5  05660000
         B     TPUTH3                  SKIP THE EXECUTED STUFF      #5  05670000
         SPACE                                                          05680000
SERMOVE  MVC   SERVAL(0),0(R6)         MOVE IN VOL SERIAL           #5  05690000
UCBMOVE  MVC   UCBVAL(0),0(R6)         MOVE IN UCB ADDRESS          #5  05700000
         SPACE                                                          05710000
TPUTH3   EQU   *                                                        05720000
         SR    R9,R9                   CLEAR OUT LINE COUNTER       #6  05730000
         PUTLINE PARM=LINEOUT,OUTPUT=(PLH1,DATA),MF=(E,VIOPL)       #5  05740000
         PUTLINE PARM=LINEOUT,OUTPUT=(PLH2,DATA),MF=(E,VIOPL)       #5  05750000
         LA    R9,2(0,R9)              INCRIMENT THE LINE COUNTER   #6  05760000
         L     R4,16             @ CVT                                  05770000
         L     R2,40(R4)         @ UCB LOOKUP TABLE                     05780000
         SPACE                                                          05790000
UCBLOOP  EQU   *                                                        05800000
         LH    R3,0(R2)          GET UCB ADDR.                          05810000
         SLL   R3,16               MAKE SURE THE UCB ADDR IS            05820000
         SRL   R3,16               CLEAN IN THE HIGH ORDER BITS         05830000
         LA    R2,2(R2)          BUMP TO NEXT...                        05840000
         LTR   R3,R3               CHECK FOR NULL                       05850000
         BZ    UCBLOOP           GET NEXT IF NULL                   #5  05860000
         CL    R3,=X'0000FFFF'   END OF TABLE?                          05870000
         BE    DONE              YES...                                 05880000
         TM    18(R3),X'20'      DISK?                                  05890000
         BZ    UCBLOOP            NO                                #5  05900000
         TM    3(R3),X'80'       ONLINE?                                05910000
         BZ    UCBLOOP            NO                                #5  05920000
         TM    6(R3),X'40'       READY? (CHECK NOT READY FLAG)      #5  05930000
         BNZ   UCBLOOP            NO    WAS BCR 7,R4                #5  05940000
         MVC   LINE(LNGT),BLANKS   MOVE IN BLANKS TO OUTPUT LINE    #2  05950000
         MVC   ADDRT(3),13(R3)     MAKE COPY OF CHARACTER UNIT NAME #1  05960000
         TR    ADDRT(3),TRTAB      TR ALPHA CHARACTERS A-F TO FA-FF #1  05970000
         CLC   ADDRTL,ADDRT        COMPARE UNIT NAMES               #1  05980000
         BNL   UCBLOOP             GET NEXT UCB                     #5  05990000
CHKVOL   EQU   *                                                        06000000
         CLI   SERFLG,C'N'         IF VOL COMPARE NOT NEEDED        #5  06010000
         BE    CHKUCB                 THEN CHECK ADDRESS WANTED     #5  06020000
         LH    R6,SERLEN           LOAD LENGTH OF SERIAL#-1         #5  06030000
         EX    R6,SERCLC           COMPARE SERIAL NUMBER TO UCB     #5  06040000
         BNE   UCBLOOP             TRY NEXT VOLSER                  #5  06050000
CHKUCB   EQU   *                                                        06060000
         CLI   UCBFLG,C'N'         IF UCB COMPARE NOT NEEDED        #5  06070000
         BE    SAVE01                 THEN CONTINUE LIKE BEFORE     #5  06080000
         LH    R6,UCBLEN           LOAD LENGTH OF UCB ADDRESS       #5  06090000
         EX    R6,UCBCLC           COMPARE UCB ACCRESS TO UCB       #5  06100000
         BNE   UCBLOOP             TRY NEXT VOLSER                  #5  06110000
         B     SAVE01                  CONTINUE AS USUAL            #5  06120000
         SPACE                                                          06130000
SERCLC   CLC   SERVAL(0),28(R3)        COMPARE REQUESTED VOL - UCB  #5  06140000
UCBCLC   CLC   UCBVAL(0),13(R3)        COMPARE REQUESTED ADR - UCB  #5  06150000
         SPACE                                                          06160000
SAVE01   EQU   *                                                        06170000
         MVC   VOLSER(6),28(R3)    MOVE THE VOLSER TO OUTPUT AREA       06180000
         MVC   ADDR(3),13(R3)      MOVE THE UNIT ADDRESS TO AREA        06190000
         MVC   ADDRTL,ADDRT        KEEP TRANSLATED UNIT NAME        #1  06200000
         EJECT                                                          06210000
T3380    EQU   *                                                        06220000
         CLI   19(R3),X'0E'        IS THIS A 3380                       06230000
         BNE   T3350               NO GO TRY 3350                       06240000
         TM    SELDEVT,DEVT3380    CHECK FOR 3380 SELECTION         #6A 06250000
         BZ    UCBLOOP             DO NOT CONTINUE IF NO SELECTION  #3  06260000
         MVC   DEVT(4),=C'3380'    MOVE THE IN DEVICE TYPE              06270000
         B     GOTDEV              GO SETUP FOR OUTPUT                  06280000
T3350    EQU   *                                                        06290000
         CLI   19(R3),X'0B'        IS THIS A 3350                       06300000
         BNE   T3311               NO GO TRY 3330-1                     06310000
         TM    SELDEVT,DEVT3350    CHECK FOR 3350 SELECTION         #6A 06320000
         BZ    UCBLOOP             DO NOT CONTINUE IF NO SELECTION  #3  06330000
         MVC   DEVT(4),=C'3350'    MOVE THE IN DEVICE TYPE              06340000
         B     GOTDEV              GO SETUP FOR OUTPUT                  06350000
T3311    EQU   *                                                        06360000
         CLI   19(R3),X'0D'        IS THIS A 3330-1                     06370000
         BNE   T3330               NO GO TRY 3330                       06380000
         TM    SELDEVT,DEVT3311    CHECK FOR 3311 SELECTION         #6A 06390000
         BZ    UCBLOOP             DO NOT CONTINUE IF NO SELECTION  #3  06400000
         MVC   DEVT(4),=C'3311'    MOVE THE IN DEVICE TYPE              06410000
         B     GOTDEV              GO SETUP FOR OUTPUT                  06420000
T3330    EQU   *                                                        06430000
         CLI   19(R3),X'09'        IS THIS A 3330                       06440000
         BNE   T2314               NO GO TRY 2314                       06450000
         TM    SELDEVT,DEVT3330    CHECK FOR 3330 SELECTION         #6A 06460000
         BZ    UCBLOOP             DO NOT CONTINUE IF NO SELECTION  #3  06470000
         MVC   DEVT(4),=C'3330'    MOVE THE IN DEVICE TYPE              06480000
         B     GOTDEV              GO SETUP FOR OUTPUT                  06490000
T2314    EQU   *                                                        06500000
         CLI   19(R3),X'08'        IS THIS A 2314                       06510000
         BNE   T3390               NO GO TRY 3390                   #6A 06520000
         TM    SELDEVT,DEVT2314    CHECK FOR 2314 SELECTION         #6A 06530000
         BZ    UCBLOOP             DO NOT CONTINUE IF NO SELECTION  #3  06540000
         MVC   DEVT(4),=C'2314'    MOVE THE IN DEVICE TYPE              06550000
         B     GOTDEV              GO SETUP FOR OUTPUT                  06560000
T3390    EQU   *                                                    #6A 06570000
         CLI   19(R3),X'0F'        IS THIS A 3390                   #6A 06580000
         BNE   T3375               NO GO TRY 3375                   #6B 06590000
         TM    SELDEVT,DEVT3390    CHECK FOR 3390 SELECTION         #6A 06600000
         BZ    UCBLOOP             DO NOT CONTINUE IF NO SELECTION  #6A 06610000
         MVC   DEVT(4),=C'3390'    MOVE THE IN DEVICE TYPE          #6A 06620000
         B     GOTDEV              GO SETUP FOR OUTPUT              #6A 06630000
T3375    EQU   *                                                    #6B 06640000
         CLI   19(R3),X'0C'        IS THIS A 3375                   #6B 06650000
         BNE   T3340               NO GO TRY 3340                   #6B 06660000
         TM    SELDEVT,DEVT3375    CHECK FOR 3375 SELECTION         #6B 06670000
         BZ    UCBLOOP             DO NOT CONTINUE IF NO SELECTION  #6B 06680000
         MVC   DEVT(4),=C'3375'    MOVE THE IN DEVICE TYPE          #6B 06690000
         B     GOTDEV              GO SETUP FOR OUTPUT              #6B 06700000
T3340    EQU   *                                                    #6B 06710000
         CLI   19(R3),X'0A'        IS THIS A 3340                   #6B 06720000
         BNE   UNKN                NO GO MOVE IN "UNKN"             #6B 06730000
         TM    SELDEVT,DEVT3375    CHECK FOR 3340 SELECTION         #6B 06740000
         BZ    UCBLOOP             DO NOT CONTINUE IF NO SELECTION  #6B 06750000
         MVC   DEVT(4),=C'3340'    MOVE THE IN DEVICE TYPE          #6B 06760000
         B     GOTDEV              GO SETUP FOR OUTPUT              #6B 06770000
UNKN     EQU   *                                                        06780000
         MVC   DEVT(4),=C'UNKN'    FOUND SOMETHING THAT WE DONT KNOW    06790000
GOTDEV   EQU   *                                                        06800000
         LR    R0,R3               GOT A GOOD ONE.                      06810000
         LA    R1,OUTPUT           SETUP FOR LSPCE SVC                  06820000
         SVC   78                  ISSUE LSPCE                          06830000
         MVC   ALLC(3),=C'YES'     SET ALLOCATED MSG                    06840000
         TM    3(R3),X'08'         TEST IF ALLOCATED                    06850000
         BO    CHKPRIV             BRANCH IF ALLOCATED              #2  06860000
         MVC   ALLC(3),=C'NO '     NOT ALLOCATED                        06870000
CHKPRIV  TM    34(R3),X'10'        IS THIS VOL PRIVATE              #2  06880000
         BZ    CHKPUBL             NO -- TRY PUBLIC                 #2  06890000
         TM    SELATTR,ATTRPRIV    CHECK FOR PRIV SELECTION         #6A 06900000
         BZ    UCBLOOP             DO NOT CONTINUE IF NO SELECTION  #3  06910000
         MVC   ATTR(4),=C'PRIV'    MOVE IN PUBLIC LITERAL           #2  06920000
         B     MOVESPCE            GO DO SPACE MOVES                #2  06930000
CHKPUBL  TM    34(R3),X'08'        IS THIS VOL PUBLIC               #2  06940000
         BZ    CHKSTOR             NO -- TRY STORAGE                #2  06950000
         TM    SELATTR,ATTRPUBL    CHECK FOR PUBL SELECTION         #6A 06960000
         BZ    UCBLOOP             DO NOT CONTINUE IF NO SELECTION  #3  06970000
         MVC   ATTR(4),=C'PUBL'    MOVE IN PRIVATE LITERAL          #2  06980000
         B     MOVESPCE            GO DO SPACE MOVES                #2  06990000
CHKSTOR  TM    34(R3),X'04'        IS THIS VOL PUBLIC               #2  07000000
         BZ    CHKFAIL             NO -- INDICATE UNKN              #2  07010000
         TM    SELATTR,ATTRSTOR    CHECK FOR STOR SELECTION         #6A 07020000
         BZ    UCBLOOP             DO NOT CONTINUE IF NO SELECTION  #3  07030000
         MVC   ATTR(4),=C'STOR'    MOVE IN PRIVATE LITERAL          #2  07040000
         B     MOVESPCE            GO DO SPACE MOVES                #2  07050000
CHKFAIL  MVC   ATTR(4),=C'UNKN'    MOVE IN UNKNOWN LITERAL          #2  07060000
MOVESPCE MVC   CYL(4),FCYL         MOVE IN FREE CYLS                    07070000
         MVC   TRK(4),FTRK         MOVE IN FREE TRKS                    07080000
         MVC   XTNS(4),NXTN        MOVE IN NUM EXTENTS                  07090000
         MVC   XCYL(4),OCYL        MOVE IN CONTIG CYLS                  07100000
         MVC   XTRK(4),OTRK        MOVE IN CONTIG TRKS                  07110000
         MVI   PLUS,C'+'           MOVE IN THE "+"                  #2  07120000
         SR    R5,R5                   CLEAR R5 FOR ICM             #4  07130000
         MVC   DCBS,=CL6'     0'       MOVE IN INITIAL DCB COUNT    #4  07140000
         ICM   R5,B'0001',35(R3)       INSERT THE DCB COUNT         #4  07150000
         BZ    DONEDCB                 GO AWAY                      #4  07160000
         SLL   R5,25                   SHIFT LEFT TO REMOVE "MOUNT" #4  07170000
         SRL   R5,25                   SHIFT RIGHT TO RESTORE COUNT #4  07180000
         CVD   R5,DCBCNT               MAKE DCBCOUNT DECIMAL        #4  07190000
         MVC   DCBS(6),DCBEDT          MOVE IN EDIT FIELD           #4  07200000
         ED    DCBS(6),DCBCNT+5        PUT IN THE EDITED DCB COUNT  #4  07210000
         SPACE                                                          07220000
DONEDCB  EQU   *                                                    #4  07230000
         CLI   SCRNFLG,C'N'            IF THIS IS NOT-SCREEN MODE   #6  07240000
         BE    SHOWLINE                   THEN CONTINUE THIS CODE   #6  07250000
         C     R9,MAXLINES             IF LINES EXCEEDED            #6  07260000
         BL    SHOWLINE                   THEN DISPLAY HEADING      #6  07270000
         SR    R9,R9                   CLEAR OUT LINE COUNTER       #6  07280000
         PUTLINE PARM=LINEOUT,OUTPUT=(PLH1,DATA),MF=(E,VIOPL)       #5  07290000
         PUTLINE PARM=LINEOUT,OUTPUT=(PLH2,DATA),MF=(E,VIOPL)       #5  07300000
         LA    R9,2(0,R9)              INCRIMENT THE LINE COUNTER   #6  07310000
SHOWLINE MVC   PLNE(4),PLNELN          MOVE LINE LENGTH FOR PUTLINE #5  07320000
         PUTLINE PARM=LINEOUT,OUTPUT=(PLNE,DATA),MF=(E,VIOPL)       #5  07330000
         LA    R9,1(0,R9)              INCRIMENT THE LINE COUNTER   #6  07340000
         MVI   LINEFLG,C'Y'        SET THE DISPLAYED FLAG           #5  07350000
         B     UCBLOOP             GO DO NEXT UCB ENTRY                 07360000
         SPACE                                                          07370000
DONE     EQU   *                                                        07380000
         CLI   LINEFLG,C'Y'            IF SOME DEVICES DISPLAYED    #5  07390000
         BE    DONE1                      THEN SKIP NODEVICE MSG    #5  07400000
         PUTLINE PARM=LINEOUT,OUTPUT=(PLBS,DATA),MF=(E,VIOPL)       #5  07410000
         PUTLINE PARM=LINEOUT,OUTPUT=(PLE2,DATA),MF=(E,VIOPL)       #5  07420000
         SPACE                                                          07430000
DONE1    EQU   *                                                        07440000
         PUTLINE PARM=LINEOUT,OUTPUT=(PLBS,DATA),MF=(E,VIOPL)       #5  07450000
         PUTLINE PARM=LINEOUT,OUTPUT=(PLE1,DATA),MF=(E,VIOPL)       #5  07460000
*        TPUT  BLANKS,1            PUT OUT BLANK LINE               #2  07470000
*        TPUT  END1,L'END1         PUT OUT END MESSAGE              #2  07480000
         LA    R15,0               SETUP RETURN CODE                #3  07490000
         SPACE                                                          07500000
FINISH   FINISH RC=(R15)                                            #3  07510000
         EJECT                                                          07520000
PLH1     DC    AL2(LHDR1+4),AL2(0)     LENGTH FOR PUTLINE           #5  07530000
HDR1     DC    C'            UNIT TOT FR TOT FR  TOT   LRG. FREE   IN ' 07540000
         DC    C'  UNIT  OPEN'                                      #4  07550000
LHDR1    EQU   *-HDR1                                               #2  07560000
PLH2     DC    AL2(LHDR2+4),AL2(0)     LENGTH FOR PUTLINE           #5  07570000
HDR2     DC    C'VOLSER DEVT ADDR   CYL    TRK   XTNS  CYLS+TRKS   USE' 07580000
         DC    C'  ATTR  DCBS'                                      #4  07590000
LHDR2    EQU   *-HDR2                                               #2  07600000
PLE1     DC    AL2(LEND1+4),AL2(0)     LENGTH FOR PUTLINE           #5  07610000
END1     DC    C'**PROCESSING COMPLETED**  ---  VOLS REL.01 MOD.6B' #6B 07620000
LEND1    EQU   *-END1                                               #5  07630000
PLE2     DC    AL2(LEND2+4),AL2(0)     LENGTH FOR PUTLINE           #5  07640000
END2     DC    C'  0 DEVICES SELECTED '                             #4  07650000
LEND2    EQU   *-END2                                               #5  07660000
PLBS     DC    AL2(5),AL2(0)           LENGTH FOR PUTLINE           #5  07670000
BLANKS   DC    CL80' '             BLANKS FOR ASSORTED THINGS       #2  07680000
PLNELN   DC    AL2(LNGT+4),AL2(0)      LENGTH FOR PUTLINE OF LINE   #5  07690000
DCBEDT   DC    XL06'402020202120'  EDIT PATTERN FOR OPEN DCB COUNT  #4  07700000
TRTAB    DC    256X'00'            TRANSLATE TABLE                 1.   07710000
         ORG   TRTAB+C'A'                                          1.   07720000
         DC    X'FAFBFCFDFEFF'                                     1.   07730000
         ORG   TRTAB+C'0'                                          1.   07740000
         DC    X'F0F1F2F3F4F5F6F7F8F9'                             1.   07750000
         ORG                                                            07760000
PBP1     DC    AL2(LBPRM1+4),AL2(0)    LENGTH FOR PUTLINE           #5  07770000
BADPARM1 DC    C'INVALID SELECTION KEYWORD - '                      #3  07780000
LBPRM1   EQU   *-BADPARM1          SETUP LENGTH FOR TPUT            #3  07790000
PBP2     DC    AL2(LBPRM2+4),AL2(0)    LENGTH FOR PUTLINE           #5  07800000
BADPARM2 DC    C'  VALID ARE: ALL PRIVATE PUBLIC STORAGE '          #3  07810000
         DC    C'2314 3330 3311 3340 3350 3375 3380 AND 3390'       #6B 07820000
LBPRM2   EQU   *-BADPARM2          SETUP LENGTH FOR TPUT            #3  07830000
ADALL    DC    CL8'ALL     '           SELECT ALL VOLUMES           #3  07840000
APRIV    DC    CL8'PRIVATE '           ATTRIB IS PRIVATE            #3  07850000
APUBL    DC    CL8'PUBLIC  '           ATTRIB IS PUBLIC             #3  07860000
ASTOR    DC    CL8'STORAGE '           ATTRIB IS STORAGE            #3  07870000
D3380    DC    CL8'3380    '           DEVICE IS 3380               #3  07880000
D3350    DC    CL8'3350    '           DEVICE IS 3350               #3  07890000
D3330    DC    CL8'3330    '           DEVICE IS 3330               #3  07900000
D3311    DC    CL8'3311    '           DEVICE IS 3311               #3  07910000
D2314    DC    CL8'2314    '           DEVICE IS 2314               #3  07920000
D3390    DC    CL8'3390    '           DEVICE IS 3390               #6A 07930000
D3375    DC    CL8'3375    '           DEVICE IS 3375               #6B 07940000
D3340    DC    CL8'3340    '           DEVICE IS 3340               #6B 07950000
ALLATTR  EQU   X'0F'                   SETUP FOR ALL ATTRIBS        #3  07960000
ATTRPRIV EQU   X'04'               SETUP FOR PRIVATE DEVICES        #3  07970000
ATTRPUBL EQU   X'02'               SETUP FOR PUBLIC  DEVICES        #3  07980000
ATTRSTOR EQU   X'01'               SETUP FOR STORAGE DEIVCES        #3  07990000
ALLDEVT  EQU   X'FF'                   SETUP FOR DEVICE TYPES       #3  08000000
DEVT2314 EQU   X'01'               SETUP DEVICE TYPE 2314           #3  08010000
DEVT3330 EQU   X'02'               SETUP DEVICE TYPE 3330           #3  08020000
DEVT3311 EQU   X'04'               SETUP DEVICE TYPE 3311           #3  08030000
DEVT3350 EQU   X'08'               SETUP DEVICE TYPE 3350           #3  08040000
DEVT3380 EQU   X'10'               SETUP DEVICE TYPE 3380           #3  08050000
DEVT3390 EQU   X'20'               SETUP DEVICE TYPE 3390           #6A 08060000
DEVT3375 EQU   X'40'               SETUP DEVICE TYPE 3375           #6B 08070000
DEVT3340 EQU   X'80'               SETUP DEVICE TYPE 3340           #6B 08080000
VOLPCLP  DC    V(PCLCSECT)         POINTER TO PCLCSECT FOR PARSE    #3  08090000
         SPACE                                                          08100000
         LTORG                                                          08110000
         EJECT                                                          08120000
WORKAREA DSECT                                                      #2  08130000
SAVEAREA DS    18F                     SAVEAREA FOR REGISTERS       #2  08140000
SELFLAGS DS    F                       SELECTION FLAGS              #3  08150000
SELATTR  EQU   SELFLAGS                                             #6A 08160000
SELDEVT  EQU   SELFLAGS+1                                           #6A 08170000
VOLCPPL  DS    F                       ADDRESS OF CPPL              #3  08180000
VOLPSCB  DS    F                       ADDRESS OF PSCB              #5  08190000
VOLECB   DS    F                       ECB FOR PARSE TO USE         #3  08200000
VOLANS   DS    F                       ANSWER AREA FOR PARSE        #3  08210000
VOLUWA   DS    4F                      USER WORK AREA FOR PARSE     #3  08220000
VOLPPL   DS    7F                      PPL FOR USE WITH PARSE       #3  08230000
VIOPL    DS    4F                      IOPL FOR PUTLINE SUPPORT     #5  08240000
MAXLINES DS    F                       MAX SCREEN SIZE              #6  08250000
SCRNFLG  DS    CL1                     DISPLAY SCREEN FLAG          #6  08260000
SERLEN   DS    H                       LENGTH OF VOLSER PARM        #5  08270000
UCBLEN   DS    H                       LENGTH OF UCBADR PARM        #5  08280000
SERFLG   DS    CL1                     FLAG USED TO SELECT VOLSER   #5  08290000
UCBFLG   DS    CL1                     FLAG USED TO SELECT UCBADR   #5  08300000
SERVAL   DS    CL6                     VALUE OF THE VOLSER PARM     #5  08310000
UCBVAL   DS    CL3                     VALUE OF THE UCBADR PARM     #5  08320000
LINEFLG  DS    CL1                     DEVICE SELECTED FLAG         #5  08330000
TIMEDATE DS    D                       AREA FOR THE TIME AND DATE   #3  08340000
DCBCNT   DS    D                       NUMBER OF OPEN DCBS          #4  08350000
OUTPUT   DS    0CL30                   LSPACE OUTPUT AREA           #2  08360000
         DS    CL6                                                  #2  08370000
FCYL     DS    CL4                                                  #2  08380000
         DS    CL1                                                  #2  08390000
FTRK     DS    CL4                                                  #2  08400000
         DS    CL1                                                  #2  08410000
NXTN     DS    CL4                                                  #2  08420000
         DS    CL1                                                  #2  08430000
OCYL     DS    CL4                                                  #2  08440000
         DS    CL1                                                  #2  08450000
OTRK     DS    CL4                                                  #2  08460000
         SPACE 1                                                        08470000
         DS    0D                  ALIGNMENT                        #2  08480000
PLNE     DS    AL2(LNGT+4),AL2(0)      LENGTH FOR PUTLINE           #5  08490000
LINE     EQU   *                                                    #2  08500000
VOLSER   DS    CL7' '              VOLUME SERIAL NUMBER             #2  08510000
DEVT     DS    CL6' '              DEVICE TYPE                      #2  08520000
ADDR     DS    CL5' '              DEVICE ADDRESS                   #2  08530000
CYL      DS    CL7' '              FREE CYLS                        #2  08540000
TRK      DS    CL7' '              FREE TRKS                        #2  08550000
XTNS     DS    CL6' '              EXTENTS                          #2  08560000
XCYL     DS    CL4' '              CONTIG CYLS                      #2  08570000
PLUS     DS    CL1'+'                                               #2  08580000
XTRK     DS    CL7' '              CONTIG TRKS                      #2  08590000
ALLC     DS    CL5' '              ALLOCATED FLAG                   #2  08600000
ATTR     DS    CL4' '              VOLUME ATTRIBUTES PUBL/PRIV/STOR #2  08610000
DCBS     DS    CL6' '              NUMBER OF OPEN DCBS              #4  08620000
LNGT     EQU   *-LINE                                               #2  08630000
         SPACE 1                                                        08640000
ADDRT    DS    CL3                 FIELD TO TRANSLATE UNIT NAME IN 1#2  08650000
ADDRTL   DS    CL3                 FIELD TO HOLD TRANS UNIT NAME   1#2  08660000
LINEOUT  PUTLINE MF=L              LIST FORMAT OF PUTLINE           #5  08670000
@SL      EQU   ((*-SAVEAREA+8/16)*16) LENGTH OF WORKAREA            #2  08680000
         EJECT                                                          08690000
VOLS     CSECT                                                          08700000
         PRINT ON,NOGEN                                             #3  08710000
PCLCSECT IKJPARM DSECT=PCLDSECT                                     #3  08720000
         SPACE                                                          08730000
VOLCLEAR IKJKEYWD ,                                                 #6  08740000
         IKJNAME 'CLEAR'                                            #6  08750000
         SPACE                                                          08760000
VOLPTYPE IKJKEYWD DEFAULT='TYPE'                                    #3  08770000
         IKJNAME  'TYPE',SUBFLD=$VOLTYPE                            #3  08780000
         IKJNAME  'ATTRIBUTE',SUBFLD=$VOLTYPE                       #3  08790000
         IKJNAME  'DEVICETYPE',ALIAS='DEVT',SUBFLD=$VOLTYPE         #3  08800000
         SPACE                                                          08810000
VOLPNAME IKJKEYWD ,                                                 #5  08820000
         IKJNAME  'SERIAL',SUBFLD=$VOLNAME                          #5  08830000
         IKJNAME  'VOLSER',SUBFLD=$VOLNAME                          #5  08840000
         SPACE                                                          08850000
VOLPADDR IKJKEYWD ,                                                 #5  08860000
         IKJNAME  'ADDRESS',SUBFLD=$VOLADDR                         #5  08870000
         IKJNAME  'UCBNAME',SUBFLD=$VOLADDR                         #5  08880000
         SPACE                                                          08890000
$VOLTYPE IKJSUBF                                                        08900000
VOLTYPE  IKJIDENT 'SELECTION VALUE',UPPERCASE,MAXLNTH=8,            #3 X08910000
               FIRST=ALPHANUM,OTHER=ALPHANUM,                       #3 X08920000
               DEFAULT='ALL',HELP='DASD DEVICE SELECTION VALUE'     #3  08930000
         SPACE                                                          08940000
$VOLNAME IKJSUBF                                                        08950000
VOLNAME  IKJIDENT 'SELECTION VALUE',UPPERCASE,MAXLNTH=6,            #5 X08960000
               FIRST=ALPHANUM,OTHER=ALPHANUM,                       #5 X08970000
               PROMPT='VOLUME SERIAL NUMBER - 6 DIGIT MAX',         #5 X08980000
               HELP='DASD VOLSER SELECTION VALUE'                   #5  08990000
         SPACE                                                          09000000
$VOLADDR IKJSUBF                                                        09010000
VOLADDR  IKJIDENT 'SELECTION VALUE',UPPERCASE,MAXLNTH=3,            #5 X09020000
               FIRST=ALPHANUM,OTHER=ALPHANUM,                       #5 X09030000
               PROMPT='UCB ADDRESS - 3 DIGIT MAX',                  #5 X09040000
               HELP='DASD ADDRESS SELECTION VALUE'                  #5  09050000
         SPACE                                                          09060000
         IKJENDP                                                        09070000
         SPACE                                                          09080000
         IKJPPL                                                         09090000
         SPACE                                                          09100000
         IKJCPPL                                                        09110000
         SPACE                                                          09120000
         IKJIOPL                                                        09130000
         SPACE                                                          09140000
         CVT   DSECT=YES                                                09150000
         END                                                            09160000
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR      <== TARGET LOAD LIBRARY 09170000
//LKED.SYSIN DD *                                                       09180000
  ALIAS DF                                                              09190000
  NAME VOLS(R)                                                          09191000
//HELP    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)                        09200000
//SYSPRINT DD  SYSOUT=*                                                 09210000
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP           <== TARGET HELP LIBRARY 09220000
//SYSIN    DD  *                                                        09230000
./ ADD NAME=VOLS                                                        09240000
                                                                        09250000
)F Function -                                                           09260000
                                                                        09270000
       VOLS Will inform the user which DASD volumes are                 09280000
       online and how much freespace there is on each                   09290000
       volume.  VOLS also indicates whether or not the                  09300000
       volumes listed are currently in use, what the                    09310000
       volume attribute is and how many DCBs are                        09320000
       currently open for files on that volume.                         09330000
                                                                        09340000
       Sample output:                                                   09350000
                                                                        09360000
            UNIT TOT FR TOT FR  TOT   LRG. FREE   IN   UNIT  OPEN       09370000
VOLSER DEVT ADDR   CYL    TRK   XTNS  CYLS+TRKS   USE  ATTR  DCBS       09380000
SAMP01 3330  2C0  0003   0021   0004  0003+0000   NO   PRIV     0       09390000
SAMP02 3311  2D4  0130   0678   0077  0006+0013   NO   PRIV     0       09400000
SAMP03 3350  4D0  0179   0112   0013  0059+0000   YES  STOR     5       09410000
SAMP04 3380  4F1  0064   0017   0003  0042+0000   YES  PRIV    21       09420000
                                                                        09430000
**PROCESSING COMPLETED**  ---  VOLS REL.01 MOD.6B                       09440000
)X SYNTAX -                                                             09450000
                                                                        09460000
   VOLS Type('voltype')                                                 09470000
             DEVIcetype   DEVT('devicetype')                            09480000
             Attribute('volattr')                                       09490000
        Volume('volser')   Serial('volser')                             09500000
        Ucbaddr('device-cuu')   Address('device-cuu')                   09510000
        Clear                                                           09520000
                                                                        09530000
        Required - none                                                 09540000
        Default  - TYPE(ALL)                                            09550000
                                                                        09560000
  NOTE: Devicetype, Devt, and Attribute are aliases for Type.           09570000
        Serial is an alias for Volume.                                  09580000
        Address is an alias for Ucbaddr.                                09590000
                                                                        09600000
)O Operands -                                                           09610000
))TYPE       - Specifies selection criteria.                            09620000
               Valid entries for TYPE are:                              09630000
                     All     - select ALL devices -- DEFAULT            09640000
                     PRivate - select PRIVATE devices                   09650000
                     PUblic  - select PUBLIC devices                    09660000
                     Storage - select STORAGE devices                   09670000
                     3390    - select 3390 devices                      09680000
                     3380    - select 3380 devices                      09690000
                     3375    - select 3375 devices                      09700000
                     3350    - select 3350 devices                      09710000
                     3340    - select 3340 devices                      09720000
                     3311    - select 3311 devices                      09730000
                     3330    - select 3330 devices                      09740000
                     2314    - select 2314 devices                      09750000
))DEVICETYPE - Specifies selection criteria - alias for TYPE.           09760000
))DEVT       - Specifies selection criteria - alias for TYPE.           09770000
))ATTRIBUTE  - Specifies selection criteria - alias for TYPE.           09780000
                                                                        09790000
))VOLUME     - Specifies selection by volume serial number.             09800000
               The VOLUME parameter may be a specific volser or         09810000
               some level of volser up to 6 characters in length.       09820000
               If a level is specified all devices that have serial     09830000
               numbers beginning with that level will be displayed.     09840000
))SERIAL     - Specifies selection criteria - alias for VOLUME.         09850000
                                                                        09860000
))UCBADDR    - Specifies selection by the device address.               09870000
               The UCBADDR parameter may be a spedific address or       09880000
               some level of address up to 3 characters in length.      09890000
               if a level is specified all devices that have addresses  09900000
               beginning with that level will be displayed.             09910000
))ADDRESS    - Spedifies selection criteria - alias for UCBADDR.        09920000
                                                                        09930000
))CLEAR      - Clears the screen if executing on a display video and    09940000
               redisplays the headings on the top of all subsequent     09950000
               screens.                                                 09960000
                                                                        09970000
./ ENDUP                                                                09980000
//                                                                      09990000
