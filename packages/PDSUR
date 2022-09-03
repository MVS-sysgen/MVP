//PDSUR  JOB (TSO),
//             'Install PDSUR',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00030000
//*     ASM LINKEDIT FROM SOURCE ASM                                    00040000
//*                                                                     00050000
//ASMX EXEC ASMXCL,REGION.ASM=1024K,                                    00060000
//          PARM.ASM='OBJECT,NODECK,NOESD,NORLD,FLAG(5)',               00070000
//          PARM.LKED='NCAL,MAP,LIST,LET'                               00080000
//ASM.SYSIN DD DATA,DLM=@@
UR TITLE '     PDSUR -- PDS UNLOAD/RELOAD UTILITY PROGRAM'              00001000
**  TITLE - PDS UNLOAD/RELOAD UTILITY PROGRAM                       **  00002000
**                                                                  **  00003000
**  NAME - PDSUR                                                    **  00004000
**                                                                  **  00005000
**  STATUS - VERSION 1.2,  UPDATED 03JUL73                          **  00006000
**         WITH FIXES MADE IN 2016 FOR LARGE TRACKS, 2321, DIR EOF  **  00006105
**                                                                  **  00007000
**  CSECTS -                                                        **  00008000
**       PDSUR    - MAIN CSECT                                      **  00009000
**       PDSUR1   - UNLOAD OPERATION PROGRAM CSECT                  **  00010000
**       PDSUR2   - RELOAD OPERATION PROGRAM CSECT                  **  00011000
**                                                                  **  00012000
**  ENTRY POINTS -                                                  **  00013000
**       PDSUR   (ONLY ENTRY)                                       **  00014000
**                                                                  **  00015000
**  DESIGNED AND WRITTEN BY -                                       **  00016000
**       GENE CZARCINSKI,  CODE 531                                 **  00017000
**       NASA/GODDARD SPACE FLIGHT CENTER                           **  00018000
**       GREENBELT, MARYLAND  20771                                 **  00019000
**                                                                  **  00020000
**  FUNCTION/OPERATION -                                            **  00021000
**       THIS ROUTINE IS A PARTITIONED DATA SET UTILITY PROGRAM     **  00022000
**       DESIGNED TO PROCESS BACKUP COPIES OF A PDS.  IT CAN BE     **  00023000
**       USED TO UNLOAD OR (RE)LOAD A PDS.  THE UNLOADED FORMAT     **  00024000
**       IS "IEHMOVE" COMPATABLE ALTHOUGH IT CAN HANDLE UNLOADED    **  00025000
**       BLKSIZES LARGER THAN 800 WHEREAS IEHMOVE CANNOT.  PDSUR    **  00026000
**       USES JCL TO SPECIFY (AND ALLOCATE) DIRECT ACCESS SPACE     **  00027000
**       FOR THE PDS (RATHER THAN THE DYNAMIC ALLOCATION THAT       **  00028000
**       IEHMOVE USES).  SINCE PDSUR OPERATES AS A DATA SET UTILITY,**  00029000
**       A SEPARATE DD STATEMENT IS REQUIRED FOR EACH PDS AND FOR   **  00030000
**       EACH (UNLOADED) PDS (SEQUENTIAL FILE).                     **  00031000
**                                                                  **  00032000
**  ATTRIBUTES -                                                    **  00033000
**       SERIALLY REUSABLE, BLOCK LOADED, PROB. PGM. STATE          **  00034000
**                                                                  **  00035000
**  LANGUAGE -                                                      **  00036000
**       OS/360 ASSEMBLER LANGUAGE AS DEFINED BY ASSEMBLER(G),      **  00037000
**       VERSION 2, LEVEL 5.                                        **  00038000
**                                                                  **  00039000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  00040000
**                                                                  **  00041000
**  NOTES -                                                         **  00042000
**       1. THIS ROUTINE PROCESSES IBM'S IEHMOVE COMPATABLE         **  00043000
**          UNLOADED FORMAT FOR A PDS (BUT AS A DATA SET UTILITY    **  00044000
**          RATHER THAN A SYSTEM UTILITY).                          **  00045000
**       2. THE DEFAULT BLKSIZE FOR UNLOADED (TAPE) OUTPUT IS 800.  **  00046000
**       3. A PDSUR ID RECORD IS INSERTED AS THE LAST RECORD IN     **  00047000
**          AN UNLOADED PDS TO IDENTIFY CREATOR.                    **  00048000
**       4. REBLOCKING (FOR LOADING) IS VALID ONLY FOR RECFM=F/FB.  **  00049000
**       5. THIS ROUTINE IS DESIGNED TO RUN AS A JOB-STEP TASK      **  00050000
**          SINCE THE SYSPRINT & SYSIN DDNAMES CANNOT BE CHANGED    **  00051000
**          EXCEPT BY RE-ASSEMBLY.                                  **  00052000
**       6. THIS ROUTINE WAS DEVELOPED AND TESTED UNDER OS/360      **  00053000
**          MVT RELEASE 20.6.  HOWEVER, IT SHOULD RUN (WITH NO      **  00054000
**          MODIFICATIONS) UNDER EARLY OR LATER RELEASES OF MVT,    **  00055000
**          UNDER MFT-II, OR UNDER OTHER COMPATABLE VERSIONS OF     **  00056000
**          OS/360 OR OS/370.                                       **  00057000
**       7. ALL OPERATIONS MUST BE SPECIFIED THRU THE SYSIN FILE.   **  00058000
**          VALID OPERATIONS ARE: 'RELOAD' AND 'UNLOAD'.            **  00059000
**       8. ALTHOUGH NOT STRICTLY IEHMOVE COMPATABLE, THIS          **  00060000
**          ROUTINE WILL HANDLE BLKSIZES LARGER THAN 800 FOR THE    **  00061000
**          UNLOADED DATA SET.                                      **  00062000
**       9. UNLOADED DATA SETS MUST BE SEQUENTIALLY ORGANIZED.      **  00063000
**          UNLOADED DATA SETS WILL NORMALLY RESIDE ON TAPE, BUT    **  00064000
**          THERE IS NOTHING TO PREVENT IT FROM BEING OTHER THAN    **  00065000
**          TAPE.                                                   **  00066000
**      10. ALL CONTROL STATEMENTS MUST BE CONTAINED ON A SINGLE    **  00067000
**          CARD IMAGE (NO CONTINUE CARDS).                         **  00068000
**                                                                  **  00069000
**                                                                  **  00070000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  00071000
 EJECT                                                                  00072000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  00073000
**  LOCAL  MACRO  DEFINITIONS                                       **  00074000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  00075000
         MACRO                                                          00076000
&N       TAPEIN    &A,&B                                                00077000
&N       BAL       R12,TAPEIN          READ IN A RECORD                 00078000
         B         &A                  EOF/ERROR RETURN                 00079000
         MEND                                                           00080000
**                                                                      00081000
         MACRO                                                          00082000
&N       SETRC     &RC                                                  00083000
         LA        R15,&RC             RETURN CODE                      00084000
         BAL       R14,SETRET                                           00085000
         MEND                                                           00086000
**                                                                      00087000
         MACRO                                                          00088000
&N       CMPR      &A,&B,&TYPE=E                                        00089000
&N       CLC       =C&A,0(R2)                                           00090000
         B&TYPE    &B                                                   00091000
         MEND                                                           00092000
**                                                                      00093000
         MACRO                                                          00094000
         NEXT      &A,&B                                                00095000
         LA        R4,&A                                                00096000
L&SYSNDX CLI       0(R2),C' '          LOCATE NEXT NON-BLANK            00097000
         BNE       *+16                                                 00098000
         LA        R2,1(R2)                                             00099000
         BCT       R4,L&SYSNDX                                          00100000
         B         &B                                                   00101000
         MEND                                                           00102000
**                                                                      00103000
         MACRO                                                          00104000
&N       INFOJFCB  &A,&B,&TYPE=1                                        00105000
&N       LA        R15,2                                                00106000
         BAL       R12,PRINTA                                           00107000
         MVI       0(R1),C'0'                                           00108000
         MVC       11(L'MSG22A,R1),MSG22A                               00109000
         MVC       19(L'MSG22C,R1),MSG22C                               00110000
         MVC       L'MSG22C+19(6,R1),&A+JFCBVOLS                        00111000
         MVC       40(L'MSG22D,R1),MSG22D                               00112000
         MVC       L'MSG22D+40(44,R1),&A+JFCBDSNM                       00113000
         CHKTAPE   &A                                                   00114000
         AIF       ('&TYPE' EQ '1').ONE                                 00115000
         CLI       TYPE,8              LIST?                            00116000
         BE        X&SYSNDX            YES, SKIP.                       00117000
.ONE     ANOP                                                           00118000
         BAL       R12,PRINT1                                           00119000
         MVC       11(L'MSG22B,R1),MSG22B                               00120000
         MVC       19(L'MSG22C,R1),MSG22C                               00121000
         MVC       L'MSG22C+19(6,R1),&B+JFCBVOLS                        00122000
         MVC       40(L'MSG22D,R1),MSG22D                               00123000
         MVC       L'MSG22D+40(44,R1),&B+JFCBDSNM                       00124000
         CHKTAPE   &B                                                   00125000
X&SYSNDX BAL       R12,PRINT1                                           00126000
         MEND                                                           00127000
**                                                                      00128000
         MACRO                                                          00129000
         CHKTAPE   &A                                                   00130000
         AIF       ('&A' EQ 'JFCB2').OK                                 00131000
         MEXIT                                                          00132000
.OK      TM        SWB,SWB4            TAPE?                            00133000
         BZ        X&SYSNDX            NO.                              00134000
         LH        R0,JFCB2+JFCBFLSQ                                    00135000
         CVD       R0,WORK1                                             00136000
         OI        WORK1+7,X'0F'                                        00137000
         MVI       L'MSG22C+25(R1),C','                                 00138000
         UNPK      L'MSG22C+26(4,R1),WORK1                              00139000
X&SYSNDX EQU       *                                                    00140000
         MEND                                                           00141000
**                                                                      00142000
         MACRO                                                          00143000
&N       SEARCH    &A,&B,&C,&D,&E                                       00144000
&N       LA        R14,&A                                               00145000
A&SYSNDX L         R14,0(R14)                                           00146000
         LTR       R14,R14                                              00147000
         BZ        &E                                                   00148000
         LA        R1,4(R14)                                            00149000
         LA        R15,32                                               00150000
B&SYSNDX CLI       0(R1),&B                                             00151000
         BE        &E                                                   00152000
         CLC       0(8,R1),0(&C)                                        00153000
         BE        &D                                                   00154000
         LA        R1,8(R1)                                             00155000
         BCT       R15,B&SYSNDX                                         00156000
         B         A&SYSNDX                                             00157000
         MEND                                                           00158000
**                                                                      00159000
         MACRO                                                          00160000
&N       INFOPDS1  &A,&B,&C,&D,&E                                       00161000
         LCLC      &X                                                   00162000
&X       SETC      'L'''                                                00163000
&N       BAL       R12,PRINT1                                           00164000
         MVC       11(&X&D,R1),&D                                       00165000
         BAL       R12,PRINT1                                           00166000
         LA        R1,5(R1)                                             00167000
         MVC       11(&X&E,R1),&E                                       00168000
         MVI       17(R1),C'?'                                          00169000
         LA        R15,18(R1)                                           00170000
         TM        &A,RECU                                              00171000
         BZ        B&SYSNDX                                             00172000
         MVI       17(R1),C'U'                                          00173000
         BO        A&SYSNDX                                             00174000
         MVI       17(R1),C'F'                                          00175000
         TM        &A,RECF                                              00176000
         BO        *+8                                                  00177000
         MVI       17(R1),C'V'                                          00178000
         TM        &A,RECFB-RECF                                        00179000
         BZ        *+12                                                 00180000
         MVI       0(R15),C'B'                                          00181000
         LA        R15,1(R15)                                           00182000
         TM        &A,RECS                                              00183000
         BZ        *+12                                                 00184000
         MVI       0(R15),C'S'                                          00185000
         LA        R15,1(R15)                                           00186000
A&SYSNDX TM        &A,RECT                                              00187000
         BZ        *+12                                                 00188000
         MVI       0(R15),C'T'                                          00189000
         LA        R15,1(R15)                                           00190000
         TM        &A,RECA                                              00191000
         BZ        *+12                                                 00192000
         MVI       0(R15),C'A'                                          00193000
         B         B&SYSNDX                                             00194000
         TM        &A,RECM                                              00195000
         BZ        *+8                                                  00196000
         MVI       0(R15),C'M'                                          00197000
B&SYSNDX LH        R0,&B               LRECL                            00198000
         CVD       R0,WORK1                                             00199000
         OI        WORK1+7,X'0F'                                        00200000
         UNPK      31(5,R1),WORK1                                       00201000
         LH        R0,&C               BLKSIZE                          00202000
         CVD       R0,WORK1                                             00203000
         OI        WORK1+7,X'0F'                                        00204000
         UNPK      46(5,R1),WORK1                                       00205000
         MEND                                                           00206000
**                                                                      00207000
         MACRO                                                          00208000
&N       INFOPDS2                                                       00209000
&N       MVC       WORK1(2),UNLDDIR    DIR BLKS                         00210000
         LH        R0,WORK1                                             00211000
         CVD       R0,WORK1                                             00212000
         OI        WORK1+7,X'0F'                                        00213000
         UNPK      68(3,R1),WORK1                                       00214000
         CH        R0,=H'999'                                           00215000
         BNH       *+10                                                 00216000
         UNPK      68(5,R1),WORK1                                       00217000
         MEND                                                           00218000
**                                                                      00219000
 EJECT                                                                  00220000
PDSUR    START     0                                                    00221000
**                                                                      00222000
R0       EQU       0         OS PARM REG; WORK REG                      00223000
R1       EQU       1         OS PARM REG; WORK REG                      00224000
R2       EQU       2         LOCAL WORK REG                             00225000
R3       EQU       3         LOCAL WORK REG                             00226000
R4       EQU       4         LOCAL WORK REG                             00227000
R5       EQU       5         POINTER TO PDS BUFFER                      00228000
R6       EQU       6         DIRECTORY ENTRY POINTER                    00229000
R7       EQU       7         PNTR TO DIR NL TTR'S                       00230000
R8       EQU       8                                                    00231000
R9       EQU       9         DATA BASE REGISTER                         00232000
R10      EQU       10        MAIN BASE REGISTER                         00233000
R11      EQU       11        SECONDARY CSECT BASE REGISTER              00234000
R12      EQU       12        LOCAL LINK REGISTER                        00235000
R13      EQU       13        SAVE AREA PNTR                             00236000
R14      EQU       14        OS LINK REG; WORK REG                      00237000
R15      EQU       15        OS EP ADRS REG; WORK REG                   00238000
**                                                                      00239000
OFLG     EQU       X'10'     DCB OPEN FLAGS                             00240000
RECMASK  EQU       X'F0'                                                00241000
RECU     EQU       X'C0'     RECFM=U                                    00242000
RECF     EQU       X'80'     RECFM=F                                    00243000
RECFB    EQU       X'90'     RECFM=FB                                   00244000
RECV     EQU       X'40'     RECFM=V                                    00245000
RECVB    EQU       X'50'     RECFM=VB                                   00246000
RECT     EQU       X'20'     TRACK OVERFLOW                             00247000
RECS     EQU       X'08'     FBS OR VBS                                 00248000
RECA     EQU       X'04'     ASA CONTROL CHARACTER                      00249000
RECM     EQU       X'02'     MACHINE CONTROL CHARACTER                  00250000
ALIAS    EQU       X'80'     ALIAS BIT IN PDS DIRECTORY BLOCK ENTRY     00251000
JFCBDSNM EQU       0         OFFSET TO DSNAME                           00252000
JFCBVOLS EQU       118       OFFSET TO VOLSER                           00253000
JFCBIND2 EQU       87        OFFSET TO 'DISP=...'                       00254000
JFCBNEW  EQU       X'C0'     BITS ON IF DISP=NEW                        00255000
JFCRECFM EQU       100       OFFSET TO RECFM                            00256000
JFCBLKSI EQU       102       OFFSET TO BLKSIZE                          00257000
JFCLRECL EQU       104       OFFSET TO LRECL                            00258000
JFCBFLSQ EQU       68        OFFSET TO MAG TAPE FILE SEQ NO.            00259000
**  IBM'S UNLOADED RECORD TYPE INDICATORS                               00260000
@TTR     EQU       X'80'     TTR                                        00261000
@PDS     EQU       X'40'     UNLOADED DATA SET IS A PDS                 00262000
@MEM     EQU       X'20'     RECORD IS PART OF MEMBER                   00263000
@NL      EQU       X'10'     RECORD IS A NOTE LIST                      00264000
@DIR     EQU       X'08'     RECORD IS A DIRECTORY RECORD               00265000
@DUM     EQU       X'04'     RECORD IS A DUMMY RECORD                   00266000
@X       EQU       X'02'     NOT USED                                   00267000
@END     EQU       X'01'     "END OF FILE"                              00268000
**                                                                      00269000
RC1      EQU       04        RETURN CODE - WARNING                      00270000
RC2      EQU       08        RETURN CODE - ERROR                        00271000
RC3      EQU       12        RETURN CODE - SERIOUS ERROR                00272000
RC4      EQU       16        RETURN CODE - TERMINAL ERROR               00273000
RC5      EQU       20        RETURN CODE - SYSPRINT OPEN ERROR          00274000
**                                                                      00275000
SWA1     EQU       X'80'     OPERATION TERMINATED                       00276000
SWA2     EQU       X'40'     EOF DETECTED                               00277000
SWA3     EQU       X'20'     FLAG FOR PROCESSING ALIAS                  00278000
SWA4     EQU       X'10'     FLAG FOR A NOTELIST PROCESSED              00279000
SWA5     EQU       X'08'     STOW REPLACE TO BE USED                    00280000
SWA6     EQU       X'04'     FLAG TO LEAVE TAPE MOUNTED                 00281000
SWA7     EQU       X'02'     NOTELIST FLAG                              00282000
SWA8     EQU       X'01'     DIRECTORY ENTRY PROCESSED                  00283000
SWB1     EQU       X'80'     REBLOCKING FLAG #1                         00284000
SWB2     EQU       X'40'     REBLOCKING FLAG #2                         00285000
SWB3     EQU       X'20'     DIR UPDATE FLAG FOR REBLOCK                00286000
SWB4     EQU       X'10'     "1" -> SEQUENTIAL DEVICE IS A TAPE         00287000
SWB5     EQU       X'08'     FLUSH FLAG #1                              00288000
SWB6     EQU       X'04'     FLUSH FLAG #2                              00289000
SWB7     EQU       X'02'     SELECT OR EXCLUDE SPECIFIED                00290000
SWB8     EQU       X'01'     "0"=SELECT;  "1"=EXCLUDE                   00291000
SWX1     EQU       X'80'     EOF DETECTED ON SYSIN READING LAST CARD    00292000
SWX2     EQU       X'40'     RECFM=V OPEN FLAG                          00293000
SWX3     EQU       X'20'                                                00294000
SWX4     EQU       X'10'                                                00295000
SWX5     EQU       X'08'                                                00296000
SWX6     EQU       X'04'                                                00297000
SWX7     EQU       X'02'                                                00298000
SWX8     EQU       X'01'                                                00299000
**                                                                      00300000
 EJECT                                                                  00301000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  00302000
**  PROGRAM INITIALIZATION                                          **  00303000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  00304000
         SAVE      (14,12),,PDSUR__VERSION-1.2_GENE-CZARCINSKI          00305000
         BALR      R10,0               GET BASE ADRS                    00306000
         USING     *,R10               ESTABLISH ADDRESSABILITY         00307000
         LA        R15,SAVEAREA        SET UP SAVE AREA                 00308000
         ST        R13,4(R15)                                           00309000
         ST        R15,8(R13)                                           00310000
         LR        R13,R15                                              00311000
         LR        R9,R15                                               00312000
         USING     SAVEAREA,R9         DATA BASE ADDRESSABILITY         00313000
         SR        R15,R15                                              00314000
         STH       R15,RETCOD                                           00315000
         STH       R15,PNUM                                             00316000
         STC       R15,SWX                                              00317000
         MVC       PCNT,PLIM                                            00318000
         OPEN      (SYSPRINT,(OUTPUT))                                  00319000
         TM        SYSPRINT+(DCBOFLGS-IHADCB),OFLG  CHECK OPEN          00320000
         BO        STAEINIT            OPEN OK.                         00321000
         WTO       'PDSUR:  OPEN ERROR FOR DDNAME=SYSPRINT',           +00322000
               ROUTCDE=11,DESC=7                                        00323000
         L         R13,4(R13)          ERROR ... ABORT                  00324000
         RETURN    (14,12),T,RC=RC5                                     00325000
STAEINIT STAE      STAEXIT,CT                                           00326000
         OPEN      (SYSIN,(INPUT))                                      00327000
         TM        SYSIN+(DCBOFLGS-IHADCB),OFLG  CHECK OPEN             00328000
         BO        CARDINIT            OPEN OK.                         00329000
         BAL       R12,PRINT2          ERROR -- PRNT MSG                00330000
         MVC       5(L'MSG08,R1),MSG08                                  00331000
         MVC       (L'MSG08+5)(5,R1),=C'SYSIN'                          00332000
         SETRC     RC4                                                  00333000
         B         ENDTASK2                                             00334000
CARDINIT BAL       R12,GETCARD                                          00335000
         B         ENDTASK                                              00336000
         ST        R1,LASTCARD                                          00337000
 TITLE '           PERFORM GENERAL INITIALIZATION'                      00338000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  00339000
**  PERFORM  GENERAL  INITIALIZATION  (FOR REQUESTED OPERATION)     **  00340000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  00341000
BEGIN    TM        SWX,SWX1            EOF ON SYSIN?                    00342000
         BO        ENDTASK             YES, DONE.                       00343000
         MVC       FROMDD,=CL8' '                                       00344000
         MVC       TODD,=CL8' '                                         00345000
         GETMAIN   R,LV=0              COLLECT CORE                     00346000
         XC        SWITCHES,SWITCHES   CLEAR ALL SWITCHES               00347000
**                                                                      00348000
**  GET AND ANALYZE A CONTROL CARD                                      00349000
**                                                                      00350000
         BAL       R12,PRINT2          PRINT HEADER                     00351000
         MVC       0(MSG25L,R1),MSG25                                   00352000
         LR        R2,R1               SAVE POINTER                     00353000
         TIME      DEC                 GET DATE/TIME                    00354000
         ST        R0,WORK1            NOW FORTMAT IT FOR HEADING       00355000
         XC        WORK1+8(8),WORK1+8                                   00356000
         ST        R1,WORK1+12                                          00357000
         MVC       WORK1+5(1),WORK1+13 SAVE YEAR                        00358000
         CVB       R15,WORK1+8         CONVERT DATE TO BINARY           00359000
         XR        R14,R14                                              00360000
         D         R14,=F'1000'        TO SEPARATE YEAR & DAY           00361000
         ST        R15,WORK1+8                                          00362000
         LA        R15,MONTHS1         FOR STD YEAR                     00363000
         TM        WORK1+11,X'03'      LEAP YEAR?                       00364000
         BNZ       *+8                 NO.                              00365000
         LA        R15,MONTHS2         YES, USE TABLE 2                 00366000
         XR        R1,R1                                                00367000
BEGIN10  SH        R14,0(R15)                                           00368000
         BNP       BEGIN11                                              00369000
         LA        R1,3(R1)                                             00370000
         LA        R15,2(R15)                                           00371000
         B         BEGIN10                                              00372000
BEGIN11  AH        R14,0(R15)                                           00373000
         MH        R14,=H'10'                                           00374000
         CVD       R14,WORK1+8         FOR DAY OF MONTH                 00375000
         LA        R1,MONTHS3(R1)      FOR MONTH IN CHARS               00376000
         MVC       L'MSG25+3(3,R2),0(R1)                                00377000
         MVC       WORK1+4(1),WORK1+14 DAY OF MONTH                     00378000
         ED        L'MSG25(8,R2),WORK1+4                                00379000
         CLI       L'MSG25+1(R2),C' '                                   00380000
         BNE       *+8                                                  00381000
         MVI       L'MSG25+1(R2),C'0'                                   00382000
         ED        L'MSG25+8(9,R2),WORK1                                00383000
         CLI       L'MSG25+9(R2),C' '                                   00384000
         BNE       *+8                                                  00385000
         MVI       L'MSG25+9(R2),C'0'                                   00386000
*                                                                       00387000
         L         R2,LASTCARD                                          00388000
         LA        R15,2               TO PRINT THE CARD IMAGE          00389000
         BAL       R12,PRINT1                                           00390000
         MVI       0(R1),C'0'                                           00391000
         MVC       5(80,R1),0(R2)                                       00392000
         LA        R3,71(R2)           COMPUTE END OF CARD              00393000
         NEXT      40,CTLCRDER                                          00394000
         MVI       TYPE,0                                               00395000
         CMPR      'UNLOAD ',CTLCRD3                                    00396000
         CMPR      'U ',CTLCRD4                                         00397000
         MVI       TYPE,4                                               00398000
         CMPR      'RELOAD ',CTLCRD3                                    00399000
         CMPR      'R ',CTLCRD4                                         00400000
         MVI       TYPE,8                                               00401000
         CMPR      'LIST ',CTLCRD3A                                     00402000
         CMPR      'L ',CTLCRD4                                         00403000
         B         CTLCRDER            ERROR                            00404000
CTLCRD3  LA        R2,2(R2)            PROCESS CONTROL CARD OPERAND     00405000
CTLCRD3A LA        R2,3(R2)                                             00406000
CTLCRD4  LA        R2,2(R2)                                             00407000
         NEXT      16,CTLCRDER                                          00408000
CTLCRD4B CR        R2,R3               END OF CARD?                     00409000
         BNL       CTLCRDER            ERROR                            00410000
         CMPR      'FROMDD=',CTLCRD6A                                   00411000
         CMPR      'F=',CTLCRD6A+4                                      00412000
         CMPR      'TODD=',CTLCRD6B                                     00413000
         CMPR      'T=',CTLCRD6B+4                                      00414000
         CMPR      'REPLACE',CTLCRD5A                                   00415000
         CLI       0(R2),C'R'                                           00416000
         BE        CTLCRD5                                              00417000
         CMPR      'SELECT',CTLCRD7A                                    00418000
         CMPR      'EXCLUDE',CTLCRD7B                                   00419000
         CLI       0(R2),C'S'                                           00420000
         BE        CTLCRD7C                                             00421000
         CLI       0(R2),C'E'                                           00422000
         BE        CTLCRD7D                                             00423000
         CMPR      'LEAVE',CTLCRD8A                                     00424000
         CLI       0(R2),C'L'                                           00425000
         BE        CTLCRD8B                                             00426000
         B         CTLCRDER            ERROR                            00427000
CTLCRD5A LA        R2,6(R2)                                             00428000
CTLCRD5  OI        SWA,SWA5            SET "REPLACE" SWITCH             00429000
         LA        R2,1(R2)                                             00430000
         B         CTLCRD7G                                             00431000
CTLCRD6A LA        R2,5(R2)            UPDATE POINTER . . . FROMDD      00432000
         LA        R2,2(R2)                                             00433000
         LA        R14,FROMDD                                           00434000
         B         CTLCRD6C                                             00435000
CTLCRD6B LA        R2,3(R2)            UPDATE POINTER . . . TODD        00436000
         LA        R2,2(R2)                                             00437000
         LA        R14,TODD                                             00438000
CTLCRD6C LA        R4,8                SET LENGTH                       00439000
         CR        R2,R3               END OF CARD?                     00440000
         BNL       CTLCRDER            ERROR                            00441000
CTLCRD6D CLI       0(R2),C','          COMMA                            00442000
         BE        CTLCRD6E            YES                              00443000
         CLI       0(R2),C' '          END OF OPERAND                   00444000
         BE        CTLCRDX                                              00445000
         IC        R0,0(R2)            GET CHAR                         00446000
         STC       R0,0(R14)           AND SAVE IT                      00447000
         LA        R2,1(R2)            UPDATE POINTERS                  00448000
         LA        R14,1(R14)                                           00449000
         CR        R2,R3               END OF CARD                      00450000
         BNL       CTLCRDX                                              00451000
         BCT       R4,CTLCRD6D                                          00452000
         CLI       0(R2),C' '                                           00453000
         BE        CTLCRDX                                              00454000
CTLCRD6E CLI       0(R2),C','          COMMA?                           00455000
         BNE       CTLCRDER            ERROR                            00456000
         LA        R2,1(R2)                                             00457000
         B         CTLCRD4B                                             00458000
CTLCRD7A LA        R2,6(R2)                                             00459000
         B         CTLCRD7F                                             00460000
CTLCRD7B LA        R2,7(R2)                                             00461000
         B         CTLCRD7E                                             00462000
CTLCRD7C LA        R2,1(R2)                                             00463000
         B         CTLCRD7F                                             00464000
CTLCRD7D LA        R2,1(R2)                                             00465000
CTLCRD7E OI        SWB,SWB7+SWB8                                        00466000
         B         CTLCRD7G                                             00467000
CTLCRD7F OI        SWB,SWB7                                             00468000
         NI        SWB,255-SWB8                                         00469000
CTLCRD7G CR        R2,R3                                                00470000
         BNL       CTLCRDX                                              00471000
         CLI       0(R2),C' '                                           00472000
         BE        CTLCRDX                                              00473000
         CLI       0(R2),C','                                           00474000
         BNE       CTLCRDER                                             00475000
         LA        R2,1(R2)                                             00476000
         B         CTLCRD4B                                             00477000
CTLCRD8A LA        R2,4(R2)                                             00478000
CTLCRD8B LA        R2,1(R2)                                             00479000
         OI        SWA,SWA6            FLAG LEAVE OPTION                00480000
         B         CTLCRD7G                                             00481000
**  GET NEXT CONTROL CARD                                               00482000
CTLCRDX  LA        R6,PNTR9                                             00483000
         XR        R8,R8                                                00484000
CTLCRDX1 BAL       R12,GETCARD                                          00485000
         B         CTLCRDA                                              00486000
         ST        R1,LASTCARD                                          00487000
         LR        R2,R1                                                00488000
         LA        R3,71(R1)                                            00489000
         NEXT      40,CTLCRDA                                           00490000
CTLCRDX3 LA        R1,2(R2)                                             00491000
         CMPR      'M ',CTLCRDX4                                        00492000
         LA        R1,7(R2)                                             00493000
         CMPR      'MEMBER ',CTLCRDA,TYPE=NE                            00494000
CTLCRDX4 LR        R2,R1                                                00495000
         BAL       R12,PRINT1                                           00496000
         L         R15,LASTCARD                                         00497000
         MVC       5(80,R1),0(R15)                                      00498000
         NEXT      20,CTLCRDX1                                          00499000
CTLCRDX6 LTR       R8,R8                                                00500000
         BNZ       CTLCRDX7                                             00501000
         GETMAIN   R,LV=260                                             00502000
         ST        R1,0(R6)                                             00503000
         LA        R7,4(R1)                                             00504000
         LR        R6,R1                                                00505000
         XC        0(4,R1),0(R1)                                        00506000
         LA        R8,32                                                00507000
CTLCRDX7 MVC       0(8,R7),=CL8' '                                      00508000
         LR        R1,R7                                                00509000
         LA        R15,8                                                00510000
CTLCRDX8 CLI       0(R2),C' '                                           00511000
         BE        CTLCRDX9                                             00512000
         CLI       0(R2),C','                                           00513000
         BE        CTLCRDXA                                             00514000
         LTR       R15,R15                                              00515000
         BNP       *+18                                                 00516000
         IC        R0,0(R2)                                             00517000
         STC       R0,0(R1)                                             00518000
         BCTR      R15,0                                                00519000
         LA        R1,1(R1)                                             00520000
         LA        R2,1(R2)                                             00521000
         CR        R2,R3                                                00522000
         BL        CTLCRDX8                                             00523000
CTLCRDX9 CH        R15,=H'8'                                            00524000
         BE        *+10                                                 00525000
         LA        R7,8(R7)                                             00526000
         BCTR      R8,0                                                 00527000
         B         CTLCRDX1                                             00528000
CTLCRDXA CH        R15,=H'8'                                            00529000
         BE        *+10                                                 00530000
         LA        R7,8(R7)                                             00531000
         BCTR      R8,0                                                 00532000
CTLCRDXB LA        R2,1(R2)                                             00533000
         CR        R2,R3                                                00534000
         BL        CTLCRDX7                                             00535000
         B         CTLCRDX1                                             00536000
**                                                                      00537000
**  GENERAL INITIALIZATION COMPLETE ... GO PROCESS THE REQUEST          00538000
**                                                                      00539000
CTLCRDA  CLI       FROMDD,C' '         CHECK IF PARMS SPECIFIED         00540000
         BE        CTLCRDAA                                             00541000
         CLI       TYPE,8              SKIP FOR LIST                    00542000
         BE        *+12                                                 00543000
         CLI       TODD,C' '                                            00544000
         BE        CTLCRDAA                                             00545000
         SR        R11,R11             CLEAR REG                        00546000
         IC        R11,TYPE            PICK UP TYPE                     00547000
         L         R11,TYPETBL(R11)                                     00548000
         BR        R11                 GOTO PROCESSOR                   00549000
**                                                                      00550000
CTLCRDAA BAL       R12,PRINT1                                           00551000
         MVC       5(L'MSG04,R1),MSG04                                  00552000
         BAL       R12,PRINT1                                           00553000
         MVC       5(L'MSG01,R1),MSG01                                  00554000
         SETRC     RC2                                                  00555000
         B         BEGIN                                                00556000
**  CONTROL CARD FORMAT ERROR                                           00557000
CTLCRDER BAL       R12,PRINT1                                           00558000
         MVC       5(L'MSG01,R1),MSG01                                  00559000
         SETRC     RC2                                                  00560000
         BAL       R12,GETCARD                                          00561000
         B         BEGIN                                                00562000
         ST        R1,LASTCARD                                          00563000
         B         BEGIN                                                00564000
**                                                                      00565000
TYPETBL  DC        A(UNLOAD)           FOR DUMP                         00566000
         DC        A(RELOAD)           FOR LOAD                         00567000
         DC        A(RELOAD)           FOR LIST                         00568000
 TITLE '           UNLOAD  OPERATION'                                   00569000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  00570000
**  UNLOAD OPERATION INITIALIZATION                                 **  00571000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  00572000
PDSUR1   CSECT                                                          00573000
         USING     UNLOAD,R11                                           00574000
UNLOAD   MVC       TAPE+(DCBDDNAM-IHADCB)(L'DCBDDNAM),TODD              00575000
         MVC       PDS+(DCBDDNAM-IHADCB)(L'DCBDDNAM),FROMDD             00576000
         XC        PDSBUF1,PDSBUF1                                      00577000
         XC        ALLOCM,ALLOCM                                        00578000
         XC        ALLOCS,ALLOCS                                        00579000
         XC        PNTR5,PNTR5                                          00580000
         MVI       JFCB1,0                                              00581000
         MVI       JFCB2,0                                              00582000
         DEVTYPE   TODD,WORK1+8                                         00583000
         CLI       WORK1+10,X'80'                                       00584000
         BNE       *+8                 NOT TAPE                         00585000
         OI        SWB,SWB4                                             00586000
         RDJFCB    (PDS,,TAPE)                                          00587000
         CLI       JFCB1,0             WAS PDS JFCB READ IN?            00588000
         BE        INIT1               NO                               00589000
         CLI       JFCB2,0             TAPE JFCB OK?                    00590000
         BE        INIT1CA             NO.                              00591000
         OBTAIN    PDSDSCB                                              00592000
         MVC       DSNAME,JFCB1+JFCBDSNM                                00593000
         INFOJFCB  JFCB1,JFCB2                                          00594000
**                                                                      00595000
**  INIT THE PDS FILE (FOR READING THE DIRECTORY)                       00596000
**                                                                      00597000
         NI        SWX,255-SWX2        TURN OFF RECFM=V SWITCH          00598000
         OPEN      (PDS,(INPUT)),TYPE=J                                 00599000
         TM        PDS+(DCBOFLGS-IHADCB),OFLG  CHECK OPEN               00600000
         BO        INIT1A              OPEN OK                          00601000
INIT1    BAL       R12,PRINT1          PRINT ERROR MSG FOR PDS FILE     00602000
         BAL       R12,PRINT1                                           00603000
         MVC       5(L'MSG08,R1),MSG08                                  00604000
         MVC       (L'MSG08+5)(L'FROMDD,R1),FROMDD                      00605000
         SETRC     RC3                                                  00606000
         B         BEGIN                                                00607000
INIT1A   OI        SWX,SWX2            SET FOR SECOND PASS (RECFM=V)    00608000
         MVC       PDS+(DCBLRECL-IHADCB)(2),=H'256'                     00609000
         MVC       PDS+(DCBBLKSI-IHADCB)(2),=H'256'                     00610000
**                                                                      00611000
**  ALLOCATE AND BUILD THE DIRECTORY TABLE                              00612000
**                                                                      00613000
         LA        R2,PNTR1            INIT                             00614000
         XR        R6,R6               FOR DIR BLOCK COUNTER            00615000
INIT1AA  GETMAIN   R,LV=260            GET CORE FOR ONE BLOCK           00616000
         XC        0(4,R1),0(R1)       INIT & LINK BLOCK                00617000
         ST        R1,0(R2)                                             00618000
         LA        R5,4(R1)                                             00619000
         LR        R2,R1                                                00620000
         MVI       6(R1),X'FF'                                          00621000
         MVC       4(2,R1),=H'4'                                        00622000
         BAL       R12,PDSIN           READ IN THE BLOCK                00623000
         B         INIT1AD             EOF RETURN                       00624000
         LA        R6,1(R6)            UPDATE BLOCK COUNT               00625000
         STH       R6,USEDBLKS                                          00626000
         LH        R4,0(R5)            BLOCK "USED BYTES" COUNT         00627000
         LA        R4,0(R5,R4)         COMPUTE END OF BLOCK ADRS        00628000
         LA        R5,2(R5)            POINT TO BEGINNING OF BLOCK      00629000
         SR        R15,R15             CLEAR WORK REG                   00630000
INIT1AB  CLI       0(R5),X'FF'         END OF DIRECTORY?                00631000
         BE        INIT1AC             YES.                             00632000
         IC        R15,11(R5)          COMPUTE ADRS OF NEXT ENTRY       00633000
         N         R15,=F'31'                                           00634000
         SLL       R15,1                                                00635000
         LA        R5,12(R5,R15)                                        00636000
         CR        R5,R4               END OF BLOCK?                    00637000
         BL        INIT1AB             NO.                              00638000
         B         INIT1AA             YES, GO GET NEXT BLOCK           00639000
INIT1AC  GETMAIN   R,LV=260                                             00640000
         XC        0(4,R1),0(R1)                                        00641000
         ST        R1,0(R2)                                             00642000
         MVI       6(R1),X'FF'                                          00643000
         MVC       4(2,R1),=H'4'                                        00644000
         LA        R5,4(R1)            WAS LR R5,R1 - SOC4 TERM1B .FIX. 00645001
INIT1AC1 BAL       R12,PDSIN                                            00646000
         B         INIT1AD                                              00647000
         LA        R6,1(R6)                                             00648000
         B         INIT1AC1                                             00649000
INIT1AD  STH       R6,WORK1                                             00650000
         MVC       UNLDDIR(2),WORK1                                     00651000
**  SCAN DIRECTORY AND ELEMINATE "ALL ALIAS" ENTRIES                    00652000
INIT1B   LA        R1,PNTR1            INIT FOR SCAN                    00653000
         ST        R1,PNTR2                                             00654000
         XC        PNTR3A,PNTR3A                                        00655000
INIT1BA  BAL       R12,GETNTRY         GET A DIRECTORY ENTRY            00656000
         B         INIT1C              END OF SCAN                      00657000
         TM        11(R1),ALIAS        IS ENTRY AN ALIAS?               00658000
         BZ        INIT1BA             NO.                              00659000
         LR        R2,R1               SAVE PNTR TO ALIAS ENTRY         00660000
         MVC       SAVEPNTR(16),PNTR2  YES, SAVE POSITION               00661000
         LA        R1,PNTR1            AND INIT                         00662000
         ST        R1,PNTR2                                             00663000
         XC        PNTR3A,PNTR3A                                        00664000
         SR        R3,R3                                                00665000
INIT1BB  BAL       R12,GETNTRY         GET A BLOCK                      00666000
         B         INIT1BC             END OF DIRECTORY                 00667000
         CLC       8(3,R2),8(R1)       TTR=TTR?                         00668000
         BNE       INIT1BB             NO.                              00669000
         TM        11(R1),ALIAS        IS THIS AN ALIAS?                00670000
         BZ        INIT1BD             NO, MUST BE MAIN NAME            00671000
         LTR       R3,R3               HAS AN ALIAS BEEN FOUND          00672000
         BNZ       INIT1BB             YES.                             00673000
         LR        R3,R1               NO, USE THIS ONE                 00674000
         B         INIT1BB                                              00675000
INIT1BC  NI        0(R3),255-ALIAS     NO MAIN FOUND, DELETE ALIAS      00676000
INIT1BD  MVC       PNTR2(16),SAVEPNTR  RESTORE TO PICK UP SCAN          00677000
         B         INIT1BA                                              00678000
**                                                                      00679000
**  INITIALIZE THE TAPE FILE & RE-INIT THE PDS DCB                      00680000
**                                                                      00681000
INIT1C   MVC       PDS+(DCBRECFM-IHADCB)(1),SAVRECFM                    00682000
         MVC       PDS+(DCBLRECL-IHADCB)(2),SAVLRECL                    00683000
         MVC       PDS+(DCBBLKSI-IHADCB)(2),SAVBLKSI                    00684000
         TM        SAVRECFM,RECF       RECFM=V?                         00685000
         BO        INIT1C0X            NO, RECFM=U/F                    00686000
         CLOSE     (PDS,LEAVE)         YES, RESET FOR RECFM=V           00687000
         MVI       PDS+(DCBRECFM-IHADCB),0                              00688000
         XC        PDS+(DCBLRECL-IHADCB)(2),PDS+(DCBLRECL-IHADCB)       00689000
         XC        PDS+(DCBBLKSI-IHADCB)(2),PDS+(DCBBLKSI-IHADCB)       00690000
         OPEN      (PDS,(INPUT)),TYPE=J                                 00691000
INIT1C0X OPEN      (TAPE,(OUTPUT)),TYPE=J                               00692000
         TM        TAPE+(DCBOFLGS-IHADCB),OFLG  CHECK OPEN              00693000
         BO        INIT1CB             OPEN OK                          00694000
INIT1CA  BAL       R12,PRINT1          OPEN ERROR -- PRINT MSG          00695000
         BAL       R12,PRINT1                                           00696000
         MVC       5(L'MSG08,R1),MSG08                                  00697000
         MVC       (L'MSG08+5)(L'TODD,R1),TODD                          00698000
         SETRC     RC3                                                  00699000
         B         TERM1                                                00700000
**                                                                      00701000
**  OUTPUT THE HEADER RECORDS                                           00702000
**                                                                      00703000
INIT1CB  XC        PNTR8A,PNTR8A                                        00704000
         XC        PNTR8B,PNTR8B                                        00705000
         XC        TAPECNTR,TAPECNTR                                    00706000
         LA        R2,UNLREC1L         OUTPUT IBM'S ID RECORD           00707000
         LA        R3,UNLREC1                                           00708000
         BAL       R12,TAPEOUT                                          00709000
**  FORMAT & OUTPUT THE DATA SET DESCRIPTOR RECORD                      00710000
**       DIR BLK COUNT ALREADY FILLED BY DIR READ ROUTINE               00711000
         XC        UNLDSPAR,UNLDSPAR                                    00712000
         XC        UNLDSIZE,UNLDSIZE                                    00713000
         XC        UNLDSSIZ,UNLDSSIZ                                    00714000
         DEVTYPE   FROMDD,WORK1+8,DEVTAB                                00715000
         MVI       UNLDFLAG,X'40'                                       00716000
         MVC       UNLDDEVT,WORK1+8                                     00717000
**  COMPUTE SECONDARY ALLOCATION                                        00718000
         TM        DS1SCALO,X'80'      BLK OR ABS ALLOC?                00719000
         BZ        INIT1CC             YES, SKIP                        00720000
         MVC       WORK1(4),DS1SCALO   COMPUTE SEC. ALLOC.              00721000
         L         R1,WORK1                                             00722000
         LA        R1,0(R1)            CLEAR TOP BYTE                   00723000
         CLI       WORK1+11,X'05'      IF UCBTYP+3 NOT 2321       .FIX. 00723103
         BE        *+8                    THEN TURN OFF 2321 BIT  .FIX. 00723203
         NI        WORK1+25,255-X'02'     (BIT ALSO TESTED LATER) .FIX. 00723303
         TM        WORK1+25,X'02'      2321?                            00724000
         BO        DEV2321X            YES                              00725003
         TM        DS1SCALO,X'40'      CYL ALLOC?                       00726000
         BZ        *+8                 NO.                              00727000
         MH        R1,WORK1+18         YES, CONVERT CYL TO TRK          00728000
         ST        R1,ALLOCS                                            00729000
*        MH        R1,WORK1+20         CONVERT TRK TO BYTES      *.FIX. 00730001
         XC        WORK1(2),WORK1      IN CASE WORK1+20 > 7FFF    .FIX. 00730101
         MVC       WORK1+2(2),WORK1+20 IN CASE WORK1+20 > 7FFF    .FIX. 00730201
         M         R0,WORK1            CONVERT TRK TO BYTES       .FIX. 00730301
         ST        R1,WORK1                                             00731000
         MVC       UNLDSSIZ,WORK1                                       00732000
         B         INIT1CC                                              00733000
DEV2321X XR        R2,R2               COMPUTE SEC. ALLOC. FOR 2321     00734000
         IC        R2,WORK1+18                                          00735000
         STH       R2,WORK1                                             00736000
         TM        DS1SCALO,X'40'      CYL ALLOC?                       00737000
         BZ        *+8                 NO                               00738000
         MH        R1,WORK1            YES, CONVERT CYL TO TRK          00739000
         ST        R1,ALLOCS                                            00740000
         MH        R1,WORK1+20         CONVERT TRK TO BYTES             00741000
         ST        R1,WORK1                                             00742000
         MVC       UNLDSSIZ,WORK1                                       00743000
** COMPUTE PRIMARY ALLOCATION                                           00744000
INIT1CC  TM        WORK1+25,X'02'      2321?                            00745000
         BNO       INIT1CCA            NO                               00746003
         XR        R1,R1               COMPUTE PRIMARY FOR 2321         00747000
         XR        R2,R2                                                00748000
         MVI       WORK1,0                                              00749000
         LA        R14,2                                                00750000
DEV2321A LA        R15,3                                                00751000
         LA        R3,DSCB+61                                           00752000
DEV2321B CLI       0(R3),0             LAST VALID EXT?                  00753000
         BE        DEV2321C            YES                              00754000
         IC        R2,4(R14,R3)                                         00755000
         AR        R1,R2                                                00756000
         IC        R2,0(R14,R3)                                         00757000
         SR        R1,R2                                                00758000
         LA        R3,10(R3)           POINT AT NEXT EXT.               00759000
         BCT       R15,DEV2321B        AND LOOP THRU 3 EXT.             00760000
DEV2321C CH        R14,=H'5'           DONE?                            00761000
         BNL       INIT1CE             YES                              00762000
         IC        R2,WORK1+15(R14)    GET MULT FACTOR                  00763000
         STC       R2,WORK1+1                                           00764000
         MH        R1,WORK1            AND USE IT TO CONVERT            00765000
         LA        R14,1(R14)                                           00766000
         B         DEV2321A                                             00767000
INIT1CCA MVC       WORK1(8),DSCB+63    PROCESS CYL                      00768000
         LH        R1,WORK1+4                                           00769000
         SH        R1,WORK1                                             00770000
         CLI       DSCB+71,0                                            00771000
         BE        INIT1CD                                              00772000
         MVC       WORK1(8),DSCB+73                                     00773000
         AH        R1,WORK1+4                                           00774000
         SH        R1,WORK1                                             00775000
         CLI       DSCB+81,0                                            00776000
         BE        INIT1CD                                              00777000
         MVC       WORK1(8),DSCB+83                                     00778000
         AH        R1,WORK1+4                                           00779000
         SH        R1,WORK1                                             00780000
INIT1CD  MH        R1,WORK1+18         CONVERT CYL TO TRK               00781000
         MVC       WORK1(8),DSCB+65    PROCESS TRK                      00782000
         AH        R1,WORK1+4                                           00783000
         SH        R1,WORK1                                             00784000
         CLI       DSCB+71,0                                            00785000
         BE        INIT1CE                                              00786000
         MVC       WORK1(8),DSCB+75                                     00787000
         AH        R1,WORK1+4                                           00788000
         SH        R1,WORK1                                             00789000
         LA        R1,1(R1)            PLUS ONE                         00790000
         CLI       DSCB+81,0                                            00791000
         BE        INIT1CE                                              00792000
         MVC       WORK1(8),DSCB+85                                     00793000
         AH        R1,WORK1+4                                           00794000
         SH        R1,WORK1                                             00795000
         LA        R1,1(R1)            PLUS ONE                         00796000
INIT1CE  LA        R1,1(R1)            PLUS ONE                         00797000
         ST        R1,ALLOCM                                            00798000
*        MH        R1,WORK1+20         CONVERT TRK TO BYTES      *.FIX. 00799001
         XC        WORK1(2),WORK1      IN CASE WORK1+20 > 7FFF    .FIX. 00799101
         MVC       WORK1+2(2),WORK1+20 IN CASE WORK1+20 > 7FFF    .FIX. 00799201
         M         R0,WORK1            CONVERT TRK TO BYTES       .FIX. 00799301
         ST        R1,WORK1                                             00800000
         MVC       UNLDSIZE,WORK1                                       00801000
         LA        R2,200              OUTPUT SECOND REC (REL 21)       00802000
         LA        R3,UNLREC2                                           00803000
         BAL       R12,TAPEOUT                                          00804000
**                                                                      00805000
**  LIST PDS CHARACTERISTICS                                            00806000
**                                                                      00807000
         INFOPDS1  DS1RECFM,DS1LRECL,DS1BLKL,MSG24A,MSG24B              00808000
         INFOPDS2                                                       00809000
         MVC       75(14,R1),=C'DIR-BLKS-USED='                         00810000
         LH        R0,USEDBLKS                                          00811000
         CVD       R0,WORK1                                             00812000
         OI        WORK1+7,X'0F'                                        00813000
         UNPK      89(3,R1),WORK1                                       00814000
         CH        R0,=H'999'                                           00815000
         BNH       *+10                                                 00816000
         UNPK      89(5,R1),WORK1                                       00817000
         BAL       R12,PRINT1                                           00818000
         LA        R1,15(R1)                                            00819000
         MVC       1(L'MSG24C,R1),MSG24C                                00820000
         L         R0,ALLOCM           MAIN ALLOC                       00821000
         CVD       R0,WORK1                                             00822000
         OI        WORK1+7,X'0F'                                        00823000
         UNPK      12(5,R1),WORK1                                       00824000
         L         R0,ALLOCS           SEC. ALLOC                       00825000
         CVD       R0,WORK1                                             00826000
         OI        WORK1+7,X'0F'                                        00827000
         UNPK      31(5,R1),WORK1                                       00828000
         BAL       R12,PRINT1                                           00829000
**                                                                      00830000
**  INIT OPERATION VARIABLES & ALLOCATE THE PDS BUFFER                  00831000
**                                                                      00832000
         LA        R1,PNTR1            INIT POINTERS                    00833000
         ST        R1,PNTR2                                             00834000
         XC        PNTR3A,PNTR3A                                        00835000
         LH        R0,PDS+(DCBBLKSI-IHADCB)  LENGTH OF BUFFER           00836000
         STH       R0,PDSBUFL                                           00837000
         GETMAIN   R,LV=(0)            ALLOCATE THE CORE                00838000
         ST        R1,PDSBUF1                                           00839000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  00840000
**  UNLOAD THE PDS -- MEMBER BY MEMBER                              **  00841000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  00842000
**                                                                      00843000
**  PROCESS DIRECTORY ENTRY                                             00844000
**                                                                      00845000
DUMP00   BAL       R12,GETNTRY         GET ONE DIRECTORY ENTRY          00846000
         B         TERM1               END OF DIRECTORY                 00847000
         TM        11(R1),ALIAS        IS THIS AN ALIAS?                00848000
         BO        DUMP00              YES, SKIP IT                     00849000
         LR        R6,R1               SAVE POINTER                     00850000
         TM        SWB,SWB7            SELECT OR EXCLUDE?               00851000
         BZ        DUMP05                                               00852000
         SEARCH    PNTR9,C' ',R6,DUMP03,DUMP04                          00853000
DUMP03   TM        SWB,SWB8                                             00854000
         BO        DUMP00              IGNORE, EXCLUDE                  00855000
         B         DUMP05              PROCESS, SELECT                  00856000
DUMP04   TM        SWB,SWB8                                             00857000
         BZ        DUMP00              IGNORE, SELECT                   00858000
         B         DUMP05              PROCESS, EXCLUDE                 00859000
DUMP05   XC        LLITTR,LLITTR       OUTPUT THE DIR ENTRY             00860000
         L         R1,PNTR3B                                            00861000
         STH       R1,LLI                                               00862000
         MVI       LLI+2,@PDS+@TTR+@DIR                                 00863000
         LA        R2,6                                                 00864000
         LA        R3,LLI                                               00865000
         BAL       R12,TAPEOUT                                          00866000
         L         R2,PNTR3B                                            00867000
         LR        R3,R6                                                00868000
         BAL       R12,TAPEOUT                                          00869000
         MVC       TTR3,8(R6)          SAVE TTR TO INIT FOR DATA        00870000
         MVI       TTR3+3,0                                             00871000
**  INIT FOR NOTELISTS                                                  00872000
         NI        SWA,255-(SWA4+SWA7) TURN OFF DONE SWITCH             00873000
         XR        R2,R2                                                00874000
         IC        R2,11(R6)                                            00875000
         SRL       R2,5                                                 00876000
         N         R2,=F'3'                                             00877000
         LA        R7,12(R6)                                            00878000
         STH       R2,NLCNT                                             00879000
         LTR       R2,R2                                                00880000
         BNZ       *+8                                                  00881000
         OI        SWA,SWA7            FLAG NO NOTELISTS                00882000
**                                                                      00883000
**  UNLOAD A MEMBER (PROCESSING ANY NOTELISTS)                          00884000
**                                                                      00885000
DUMP10   TM        SWA,SWA7            ANY MORE NOTELISTS?              00886000
         BO        DUMP10B             NO                               00887000
         CLI       3(R7),0             TTR FOR NOTELIST                 00888000
         BNE       DUMP11              YES                              00889000
DUMP10A  LA        R7,4(R7)            UPDATE TTR/NOTELIST PNTR         00890000
         LH        R2,NLCNT                                             00891000
         BCT       R2,*+8                                               00892000
         OI        SWA,SWA7            NO MORE NOTELISTS                00893000
         STH       R2,NLCNT                                             00894000
         B         DUMP10                                               00895000
DUMP10B  XC        LLI(6),LLI          OUTPUT DUMMY RECORD              00896000
         TM        SWA,SWA4            WAS NOTELIST PROCESSED?          00897000
         BZ        DUMP15              NO                               00898000
         MVC       LLI(2),=H'20'                                        00899000
         MVI       LLI+2,@PDS+@TTR+@DUM                                 00900000
         LA        R2,6                                                 00901000
         LA        R3,LLI                                               00902000
         BAL       R12,TAPEOUT                                          00903000
         XC        WORK1(20),WORK1                                      00904000
         LA        R2,20                                                00905000
         LA        R3,WORK1                                             00906000
         BAL       R12,TAPEOUT                                          00907000
         B         DUMP15A                                              00908000
DUMP11   MVC       TTR1,0(R7)                                           00909000
         MVI       TTR1+3,0                                             00910000
         POINT     PDS,TTR1            POSITION TO NOTELIST REC         00911000
         L         R5,PDSBUF1                                           00912000
         BAL       R12,PDSIN           AND READ IN THE NOTELIST         00913000
         B         DUMP40                                               00914000
         LH        R1,BLKLEN                                            00915000
         STH       R1,LLI                                               00916000
         MVI       LLI+2,@PDS+@TTR+@NL                                  00917000
         MVC       LLITTR(3),0(R7)                                      00918000
         LA        R2,6                OUTPUT THE NOTELIST REC          00919000
         LA        R3,LLI                                               00920000
         BAL       R12,TAPEOUT                                          00921000
         LH        R2,BLKLEN                                            00922000
         L         R3,PDSBUF1                                           00923000
         BAL       R12,TAPEOUT                                          00924000
         TM        SWA,SWA4                                             00925000
         BO        *+12                                                 00926000
         OI        SWA,SWA4                                             00927000
         B         DUMP15                                               00928000
         OI        SWA,SWA4                                             00929000
         L         R5,PDSBUF1                                           00930000
         POINT     PDS,TTR3                                             00931000
         BAL       R12,PDSIN                                            00932000
         B         DUMP40                                               00933000
         B         DUMP15A                                              00934000
DUMP15   POINT     PDS,TTR3            POSITION TO MEMBER'S DATA        00935000
DUMP15A  L         R5,PDSBUF1                                           00936000
DUMP16   XC        LLI(6),LLI          OUTPUT THE DATA                  00937000
         L         R5,PDSBUF1                                           00938000
         BAL       R12,PDSIN                                            00939000
         B         DUMP20                                               00940000
         NOTE      PDS                                                  00941000
         ST        R1,TTR3                                              00942000
         TM        SWA,SWA7            NOTELIST PROCESSING?             00943000
         BO        *+14                NO                               00944000
         CLC       TTR3(3),TTR1                                         00945000
         BE        DUMP10A                                              00946000
         MVC       LLITTR(3),TTR3                                       00947000
         LH        R1,BLKLEN                                            00948000
         STH       R1,LLI                                               00949000
         MVI       LLI+2,@PDS+@TTR+@MEM                                 00950000
         LA        R2,6                OUTPUT LLI & TTR                 00951000
         LA        R3,LLI                                               00952000
         BAL       R12,TAPEOUT                                          00953000
         LH        R2,BLKLEN           OUTPUT DATA                      00954000
         LR        R3,R5                                                00955000
         BAL       R12,TAPEOUT                                          00956000
         B         DUMP16                                               00957000
DUMP20   BAL       R12,PRINT1          OUTPUT EOM MSG                   00958000
         MVC       11(L'MSG07,R1),MSG07                                 00959000
         MVC       L'MSG07+11(8,R1),0(R6)                               00960000
         MVC       L'MSG07+19(L'MSG06,R1),MSG06                         00961000
**                                                                      00962000
**  OUTPUT ANY ALIAS DIRECTORY ENTRIES FOR CURRENT MEMBER               00963000
**                                                                      00964000
DUMP30   MVC       SAVEPNTR(16),PNTR2  SAVE POSITION IN DIR TBL         00965000
         LA        R1,PNTR1            INIT FOR TBL SCAN                00966000
         XC        PNTR3A,PNTR3A                                        00967000
         ST        R1,PNTR2                                             00968000
DUMP31   BAL       R12,GETNTRY         GET ONE ENTRY                    00969000
         B         DUMP35              END OF SCAN; GO PROC NEXT MEMBR  00970000
         TM        11(R1),ALIAS        IS THIS AN ALIAS?                00971000
         BZ        DUMP31              NO.                              00972000
         CLC       8(3,R6),8(R1)       TTR=TTR?                         00973000
         BNE       DUMP31              NO.                              00974000
         LR        R4,R1               THIS IS AN ALIAS; PROCESS IT     00975000
         TM        SWB,SWB7+SWB8       EXCLUDE IN EFFECT?               00976000
         BNO       DUMP33              NO.                              00977000
         SEARCH    PNTR9,C' ',R4,DUMP31,DUMP33                          00978000
DUMP33   L         R2,PNTR3B                                            00979000
         STH       R2,LLI                                               00980000
         XC        LLITTR,LLITTR                                        00981000
         MVI       LLI+2,@PDS+@TTR+@DIR                                 00982000
         LA        R2,6                                                 00983000
         LA        R3,LLI                                               00984000
         BAL       R12,TAPEOUT                                          00985000
         L         R2,PNTR3B                                            00986000
         LR        R3,R4                                                00987000
         BAL       R12,TAPEOUT                                          00988000
         BAL       R12,PRINT1          OUTPUT 'ALIAS UNLOADED' MSG      00989000
         MVC       11(L'MSG11,R1),MSG11                                 00990000
         MVC       L'MSG11+11(8,R1),0(R4)                               00991000
         MVC       L'MSG11+19(L'MSG06,R1),MSG06                         00992000
         B         DUMP31                                               00993000
DUMP35   MVC       PNTR2(16),SAVEPNTR  RESTORE TABLE POSITION           00994000
         B         DUMP00              AND CONTINUE                     00995000
**                                                                      00996000
DUMP40   BAL       R12,PRINT1          ERROR READING NOTELIST           00997000
         MVC       5(L'MSG21,R1),MSG21                                  00998000
         MVC       L'MSG21+5(L'MSG09,R1),MSG09                          00999000
         SETRC     RC2                                                  01000000
         B         TERM1                                                01001000
**                                                                      01002000
**  END OF DUMP OPERATION:  TERMINATE                                   01003000
**                                                                      01004000
TERM1    TM        TAPE+(DCBOFLGS-IHADCB),OFLG                          01005000
         BZ        TERM1X                                               01006000
         LA        R2,LASTRECL         OUTPUT LAST (MY ID) RECORD       01007000
         LA        R3,LASTREC                                           01008000
         BAL       R12,TAPEOUT                                          01009000
         TM        SWA,SWA6            LEAVE?                           01010000
         BZ        TERM1V              NO.                              01011000
         CLOSE     (TAPE,LEAVE)                                         01012000
         B         TERM1W                                               01013000
TERM1V   CLOSE     TAPE                                                 01014000
TERM1W   LA        R1,TAPE                                              01015000
         BAL       R12,FREEPOOL                                         01016000
TERM1X   CLOSE     PDS                                                  01017000
         L         R1,PDSBUF1          FREE PDS BUFFER                  01018000
         LTR       R1,R1                                                01019000
         BZ        TERM1A              BUFF NOT ALLOCATED               01020000
         LH        R0,PDSBUFL                                           01021000
         FREEMAIN  R,LV=(0),A=(1)                                       01022000
TERM1A   L         R2,PNTR1            FREE DIRECTORY TABLE             01023000
TERM1B   LTR       R1,R2                                                01024000
         BZ        TERM1C                                               01025000
         L         R2,0(R2)                                             01026000
         FREEMAIN  R,LV=260,A=(1)                                       01027000
         B         TERM1B                                               01028000
TERM1C   L         R2,PNTR9                                             01029000
         XC        PNTR9,PNTR9                                          01030000
TERM1C1  LTR       R1,R2                                                01031000
         BZ        TERM1D                                               01032000
         L         R2,0(R2)                                             01033000
         FREEMAIN R,LV=260,A=(1)                                        01034000
         B         TERM1C1                                              01035000
TERM1D   LA        R15,2               PRINT END MSG                    01036000
         BAL       R12,PRINTA                                           01037000
         MVI       0(R1),C'0'                                           01038000
         TM        SWA,SWA1            OPERATION ABORTED?               01039000
         BO        TERM1Z              YES                              01040000
         MVC       5(L'MSG03,R1),MSG03                                  01041000
         B         BEGIN                                                01042000
TERM1Z   MVC       5(L'MSG12,R1),MSG12                                  01043000
         B         BEGIN                                                01044000
 SPACE 4                                                                01045000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01046000
**  LOCATE THE NEXT DIRECTORY ENTRY & RETURN ADRS IN R1                 01047000
**       RETURN = 4(R12) FOR NORMAL RETURN                              01048000
**       RETURN = 0(R12) FOR END OF DIRECTORY                           01049000
GETNTRY  L         R1,PNTR3A           CHECK BYTE COUNT                 01050000
         LTR       R1,R1                                                01051000
         BP        GETNTRY1            STILL SOME LEFT                  01052000
         L         R15,PNTR2           GET NEXT BLOCK                   01053000
         LTR       R15,R15             END OF TABLE?                    01054000
         BZR       R12                 YES.                             01055000
         L         R15,0(R15)                                           01056000
         ST        R15,PNTR2                                            01057000
         LH        R0,4(R15)           BLOCK BYTE COUNT                 01058000
         SH        R0,=H'2'                                             01059000
         LA        R1,6(R15)           ADRS OF DATA                     01060000
         ST        R0,PNTR3A                                            01061000
         ST        R1,PNTR3                                             01062000
         B         GETNTRY2                                             01063000
GETNTRY1 L         R1,PNTR3            COMPUTE ADRS OF NEXT ENTRY       01064000
         AL        R1,PNTR3B                                            01065000
         ST        R1,PNTR3                                             01066000
GETNTRY2 SR        R15,R15             COMPUTE LEN & UPDATE BYTE CNT    01067000
         IC        R15,11(R1)                                           01068000
         N         R15,=F'31'                                           01069000
         SLL       R15,1                                                01070000
         LA        R15,12(R15)                                          01071000
         ST        R15,PNTR3B                                           01072000
         LCR       R15,R15                                              01073000
         A         R15,PNTR3A                                           01074000
         ST        R15,PNTR3A                                           01075000
         CLI       0(R1),X'FF'         CHECK FOR END OF DIRECTORY       01076000
         BNE       4(R12)                                               01077000
         BR        R12                 END OF DIRECTORY                 01078000
 SPACE 4                                                                01079000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01080000
**  READ IN A BLOCK FROM THE PDS                                        01081000
**       NORMAL RETURN - OFFSET=4                                       01082000
**       EOF RETURN    - OFFSET=0                                       01083000
PDSIN    XC        DECBIN,DECBIN       CLEAR THE ECB                    01084000
         READ      DECBIN,SF,PDS,(R5),'S'                               01085000
         CHECK     DECBIN                                               01086000
         L         R15,DECBIN+16       COMPUTE & SAVE LENGTH            01087000
         LH        R14,PDS+(DCBBLKSI-IHADCB)                            01088000
         SH        R14,14(R15)                                          01089000
         STH       R14,BLKLEN                                           01090000
         B         4(R12)              RETURN TO CALLER                 01091000
PDSEOF   BR        R12                 EOF RETURN                       01092000
 SPACE 4                                                                01093000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01094000
**  OUTPUT UNLOADED RECORDS TO TAPE                                     01095000
TAPEOUT  LTR       R2,R2               ANY BYTES LEFT?                  01096000
         BNPR      R12                 NO, RETURN                       01097000
         LM        R14,R15,PNTR8A      LOAD BUFFER PARMS                01098000
         LTR       R1,R15              ANY SPACE AVAILABLE?             01099000
         BNP       TAPEOUT2            NO, GO GET NEW REC               01100000
         CR        R2,R15              COMPARE REQUEST TO AVAILABLE     01101000
         BH        TAPEOUT1            REQUEST GREATER                  01102000
         LA        R0,0(R2,R14)        PROCESS SMALL REQUEST            01103000
         SR        R1,R2                                                01104000
         STM       R0,R1,PNTR8A                                         01105000
         BCTR      R2,0                                                 01106000
         EX        R2,TAPEOUTM                                          01107000
         BR        R12                                                  01108000
TAPEOUT1 BCTR      R15,0                                                01109000
         EX        R15,TAPEOUTM                                         01110000
         SR        R2,R1                                                01111000
         AR        R3,R1                                                01112000
TAPEOUT2 PUT       TAPE                                                 01113000
         LA        R14,2(R1)                                            01114000
         LA        R15,78                                               01115000
         STM       R14,R15,PNTR8A                                       01116000
         LH        R15,TAPECNTR                                         01117000
         LA        R15,1(R15)                                           01118000
         STH       R15,TAPECNTR                                         01119000
         STH       R15,0(R1)                                            01120000
         B         TAPEOUT             TO CONTINUE                      01121000
TAPEOUTM MVC       0(0,R14),0(R3)                                       01122000
 TITLE '           RELOAD OPERATION'                                    01123000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01124000
**  RELOAD  OPERATION                                               **  01125000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01126000
PDSUR2   CSECT                                                          01127000
         USING     RELOAD,R11                                           01128000
RELOAD   MVC       TAPE+(DCBDDNAM-IHADCB)(L'DCBDDNAM),FROMDD            01129000
         MVC       PDS+(DCBDDNAM-IHADCB)(L'DCBDDNAM),TODD               01130000
         XC        PNTR1,PNTR1                                          01131000
         XC        PNTR2,PNTR2                                          01132000
         XC        PDSBUF1,PDSBUF1                                      01133000
         XC        PNTR4,PNTR4                                          01134000
         XC        PNTR5,PNTR5                                          01135000
         MVI       JFCB1,0                                              01136000
         MVI       JFCB2,0                                              01137000
         DEVTYPE   FROMDD,WORK1+8                                       01138000
         CLI       WORK1+10,X'80'                                       01139000
         BNE       *+8                 NOT TAPE                         01140000
         OI        SWB,SWB4                                             01141000
         CLI       TYPE,8              LIST?                            01142000
         BNE       INIT2L              NO.                              01143000
         RDJFCB    TAPE                                                 01144000
         B         INIT2LA                                              01145000
INIT2L   RDJFCB    (PDS,,TAPE)                                          01146000
         CLI       JFCB1,0                                              01147000
         BE        INIT2X                                               01148000
INIT2LA  CLI       JFCB2,0                                              01149000
         BE        INIT2Y                                               01150000
         INFOJFCB  JFCB2,JFCB1,TYPE=2                                   01151000
**                                                                      01152000
**  INIT THE TAPE FILE                                                  01153000
**                                                                      01154000
         OPEN      (TAPE,(INPUT)),TYPE=J                                01155000
         TM        TAPE+(DCBOFLGS-IHADCB),OFLG  CHECK OPEN              01156000
         BO        INIT2A                                               01157000
INIT2Y   BAL       R12,PRINT1          PRINT ERROR MSG                  01158000
         BAL       R12,PRINT1                                           01159000
         MVC       5(L'MSG08,R1),MSG08                                  01160000
         MVC       (L'MSG08+5)(L'FROMDD,R1),FROMDD                      01161000
         SETRC     RC3                                                  01162000
         B         BEGIN                                                01163000
INIT2A   XC        PNTR8A,PNTR8A       INIT & GET HEADER REC            01164000
         XC        PNTR8B,PNTR8B                                        01165000
         XC        TAPECNTR,TAPECNTR                                    01166000
         LA        R2,UNLREC1L                                          01167000
         LA        R3,WORKAREA                                          01168000
         TAPEIN    LOAD11,LOAD11                                        01169000
         CLC       UNLREC1(UNLREC1L),WORKAREA                           01170000
         BE        INIT2A2             ID IS OK.                        01171000
INIT2A1  BAL       R12,PRINT1          NOT HEADER--PRINT MSG            01172000
         MVC       5(L'MSG13,R1),MSG13                                  01173000
         SETRC     RC3                                                  01174000
         OI        SWA,SWA1                                             01175000
         B         TERM2                                                01176000
INIT2A2  LA        R2,3                GET LLI OF SECOND REC            01177000
         LA        R3,WORK1                                             01178000
         TAPEIN    LOAD11,LOAD11                                        01179000
         CLC       UNLREC2,WORK1       LLI OK (REL 21)?                 01180000
         BE        *+10                YES.                             01181000
         CLC       =X'00C800',WORK1    NO, TRY LLI FOR PRIOR REL 21     01182000
         BNE       INIT2A1             NO                               01183000
         LH        R2,WORK1            'LL' TO READ IN OLD DSCB         01184000
         LA        R3,WORKAREA                                          01185000
         TAPEIN    LOAD11,LOAD11                                        01186000
**                                                                      01187000
**  INIT THE PDS FILE                                                   01188000
**                                                                      01189000
         CLI       TYPE,8              LIST?                            01190000
         BE        INIT2D              YES, SKIP.                       01191000
         TM        SWA,SWA5            STOW ADD OR REPLACE?             01192000
         BO        INIT2C              REPLACE                          01193000
         MVC       PDSDIR+(DCBDDNAM-IHADCB)(L'DCBDDNAM),TODD            01194000
         OPEN      (PDSDIR,(INPUT)),TYPE=J                              01195000
         TM        PDSDIR+(DCBOFLGS-IHADCB),OFLG                        01196000
         BZ        INIT2X                                               01197000
         GETMAIN   R,LV=260            CORE FOR BUFFER BLOCK            01198000
         ST        R1,PNTR1                                             01199000
         GETMAIN   R,LV=260                                             01200000
         XC        0(4,R1),0(R1)                                        01201000
         ST        R1,PNTR2                                             01202000
         LA        R3,4(R1)                                             01203000
         LR        R2,R1                                                01204000
         MVI       4(R1),X'FF'                                          01205000
         LA        R6,32                                                01206000
INIT2BA  XC        DIRDECB,DIRDECB                                      01207000
         L         R5,PNTR1                                             01208000
         READ      DIRDECB,SF,PDSDIR,(R5),'S'                           01209000
         CHECK     DIRDECB                                              01210000
         LH        R4,0(R5)                                             01211000
         LA        R4,0(R5,R4)                                          01212000
         LA        R5,2(R5)                                             01213000
INIT2BB  BCT       R6,INIT2BC                                           01214000
         GETMAIN   R,LV=260                                             01215000
         XC        0(4,R1),0(R1)                                        01216000
         ST        R1,0(R2)                                             01217000
         LA        R3,4(R1)                                             01218000
         LR        R2,R1                                                01219000
         MVI       4(R1),X'FF'                                          01220000
         LA        R6,31                                                01221000
INIT2BC  MVC       0(8,R3),0(R5)                                        01222000
         LA        R3,8(R3)                                             01223000
         CLI       0(R5),X'FF'                                          01224000
         BE        INIT2BZ                                              01225000
         IC        R15,11(R5)                                           01226000
         N         R15,=F'31'                                           01227000
         SLL       R15,1                                                01228000
         LA        R5,12(R5,R15)                                        01229000
         CR        R5,R4                                                01230000
         BL        INIT2BB                                              01231000
         B         INIT2BA                                              01232000
DIREOF   LTR       R6,R6                                                01233000
         BNP       *+8                                                  01234000
         MVI       0(R3),X'FF'                                          01235000
INIT2BZ  L         R1,PNTR1                                             01236000
         FREEMAIN  R,LV=260,A=(1)                                       01237000
         XC        PNTR1,PNTR1                                          01238000
         CLOSE     (PDSDIR,LEAVE)                                       01239000
INIT2C   MVC       SAVRECFM,DS1RECFM                                    01240000
         MVC       SAVLRECL,DS1LRECL                                    01241000
         MVC       SAVBLKSI,DS1BLKL                                     01242000
         OPEN      (PDS,(OUTPUT)),TYPE=J   AND OPEN THE FILE            01243000
         TM        PDS+(DCBOFLGS-IHADCB),OFLG  CHECK OPEN               01244000
         BO        INIT2D              OPEN OK                          01245000
INIT2X   BAL       R12,PRINT1          OPEN ERROR--PRINT MSG            01246000
         BAL       R12,PRINT1                                           01247000
         MVC       5(L'MSG08,R1),MSG08                                  01248000
         MVC       (L'MSG08+5)(L'TODD,R1),TODD                          01249000
         SETRC     RC3                                                  01250000
         OI        SWA,SWA1                                             01251000
         B         TERM2                                                01252000
**                                                                      01253000
**  LIST PDS CHARACTERISTICS                                            01254000
**                                                                      01255000
INIT2D   INFOPDS1  DS1RECFM,DS1LRECL,DS1BLKL,MSG24D,MSG24B              01256000
         INFOPDS2                                                       01257000
         CLI       TYPE,8              LIST?                            01258000
         BE        INIT2FA             YES, SKIP.                       01259000
         INFOPDS1  PDS+(DCBRECFM-IHADCB),PDS+(DCBLRECL-IHADCB),        +01260000
               PDS+(DCBBLKSI-IHADCB),MSG24E,MSG24F                      01261000
         BAL       R12,PRINT1                                           01262000
**                                                                      01263000
**  PROCESS/CHECK THE PDS'S DCB PARAMETERS                              01264000
**                                                                      01265000
         MVC       WORK1(1),SAVRECFM   CHECK RECFM                      01266000
         MVC       WORK1+1(1),PDS+(DCBRECFM-IHADCB)                     01267000
         NC        WORK1(2),=X'C0C0'                                    01268000
         CLC       WORK1(1),WORK1+1                                     01269000
         BNE       INIT2E              NO--ERROR                        01270000
         TM        PDS+(DCBRECFM-IHADCB),RECV                           01271000
         BO        INIT2F              NOT RECFM=F/FB                   01272000
         TM        PDS+(DCBRECFM-IHADCB),RECF                           01273000
         BNO       INIT2F              NOT RECFM=F/FB                   01274000
         TM        SAVRECFM,RECFB                                       01275000
         BO        INIT2D4             OLD RECFM=FB                     01276000
         TM        PDS+(DCBRECFM-IHADCB),RECFB                          01277000
         BO        INIT2D2             NEW RECFM=FB                     01278000
INIT2D1  CLC       SAVBLKSI,PDS+(DCBBLKSI-IHADCB)  BOTH RECFM=F         01279000
         BNE       INIT2E              BLKSIZE ERROR                    01280000
         B         INIT2F                                               01281000
INIT2D2  CLC       SAVBLKSI,PDS+(DCBBLKSI-IHADCB)  OLD=F, NEW=FB        01282000
         BE        *+8                                                  01283000
         OI        SWB,SWB1            FLAG REBLOCKING.                 01284000
         CLC       SAVBLKSI,PDS+(DCBLRECL-IHADCB)                       01285000
         BNE       INIT2E              RECLEN ERROR                     01286000
         B         INIT2F                                               01287000
INIT2D4  TM        PDS+(DCBRECFM-IHADCB),RECFB   OLD RECFM=FB           01288000
         BO        INIT2D5             NEW RECFM=FB (ALSO)              01289000
         OI        SWB,SWB1            FLAG REBLOCKING, NEW RECFM=F     01290000
         CLC       SAVLRECL,PDS+(DCBBLKSI-IHADCB)                       01291000
         BNE       INIT2E              RECLEN ERROR                     01292000
         B         INIT2F                                               01293000
INIT2D5  CLC       SAVLRECL,PDS+(DCBLRECL-IHADCB)    BOTH RECFM=FB      01294000
         BNE       INIT2E              RECLEN ERROR                     01295000
         CLC       SAVBLKSI,PDS+(DCBBLKSI-IHADCB)                       01296000
         BE        INIT2F                                               01297000
         OI        SWB,SWB1            BLKSIZE UNEQUAL, REBLOCK         01298000
         B         INIT2F                                               01299000
INIT2E   BAL       R12,PRINT1          DCB PARM ERR--PRINT MSG          01300000
         MVC       5(L'MSG16,R1),MSG16                                  01301000
         SETRC     RC3                                                  01302000
         OI        SWA,SWA1                                             01303000
         B         TERM2                                                01304000
**                                                                      01305000
**  INIT OPERATION PARAMETERS & ALLOCATE THE PDS BUFFER                 01306000
**                                                                      01307000
INIT2F   TM        SWB,SWB1            REBLOCKING?                      01308000
         BZ        INIT2FA             NO.                              01309000
         BAL       R12,PRINT1          YES, ISSUE MSG.                  01310000
         MVC       5(L'MSG23,R1),MSG23                                  01311000
         BAL       R12,PRINT1                                           01312000
INIT2FA  GETMAIN   R,LV=260                                             01313000
         ST        R1,PNTR4                                             01314000
         CLI       TYPE,8              LIST?                            01315000
         BE        LOAD00              YES, SKIP                        01316000
         LH        R0,PDS+(DCBBLKSI-IHADCB)  GET LENGTH                 01317000
         TM        SWB,SWB1            REBLOCK?                         01318000
         BZ        *+8                 NO.                              01319000
         AH        R0,SAVBLKSI         YES, INCREASE BUFFER             01320000
         STH       R0,PDSBUFL                                           01321000
         GETMAIN   R,LV=(0)                                             01322000
         ST        R1,PDSBUF1                                           01323000
         LR        R5,R1                                                01324000
         TM        SWB,SWB1            REBLOCK?                         01325000
         BZ        INIT2FB             NO.                              01326000
         MVC       PDSBUF2B+2(2),PDS+(DCBBLKSI-IHADCB)                  01327000
         AH        R1,SAVBLKSI                                          01328000
         ST        R1,PDSBUF2A                                          01329000
         MVC       PDSBUF2C,PDSBUF2A                                    01330000
         XC        PDSBUF2D,PDSBUF2D                                    01331000
         OI        SWB,SWB2                                             01332000
**  COMPUTE MAX NOTELIST LENGTH                                         01333000
INIT2FB  LA        R1,1024             FOR RECFM = U OR V               01334000
         TM        PDS+(DCBRECFM-IHADCB),RECF  RECFM=F                  01335000
         BNO       *+18                NO                               01336000
         LH        R1,PDS+(DCBLRECL-IHADCB)                             01337000
         LTR       R1,R1                                                01338000
         BP        *+8                                                  01339000
         LH        R1,PDS+(DCBBLKSI-IHADCB)                             01340000
         STH       R1,PNTR5L                                            01341000
**                                                                      01342000
**  LOAD THE PDS                                                        01343000
**                                                                      01344000
LOAD00   LA        R2,3                GET THE LLI                      01345000
         LA        R3,LLI                                               01346000
         TAPEIN    LOAD11,LOAD11                                        01347000
         TM        LLI+2,@TTR          TTR PRESENT?                     01348000
         BZ        LOAD01              NO                               01349000
         LA        R2,3                YES, GET IT.                     01350000
         LA        R3,LLITTR                                            01351000
         TAPEIN    LOAD11,LOAD11                                        01352000
LOAD01   TM        LLI+2,@END          END OF INPUT?                    01353000
         BO        LOAD20X             YES.                             01354000
         TM        LLI+2,@DIR          DIRECTORY RECORD?                01355000
         BO        LOAD20              YES                              01356000
         CLI       TYPE,8              LIST?                            01357000
         BE        LOAD60              YES, SKIP PROCESSING (FLUSH).    01358000
         TM        SWB,SWB5            FLUSH?                           01359000
         BO        LOAD60              YES.                             01360000
         TM        LLI+2,@MEM          MEMBER'S DATA?                   01361000
         BO        LOAD30              YES                              01362000
         TM        LLI+2,@NL           NOTE LIST?                       01363000
         BO        LOAD40              YES                              01364000
         TM        LLI+2,@DUM                                           01365000
         BO        LOAD50              FOR DUMMY RECORD                 01366000
         B         LOAD11              ERROR                            01367000
**  ERROR HANDLERS                                                      01368000
LOAD10   LA        R15,2                                                01369000
         BAL       R12,PRINTA                                           01370000
         MVI       0(R1),C'0'                                           01371000
         MVC       5(L'MSG09,R1),MSG09                                  01372000
         OI        SWA,SWA1                                             01373000
         B         TERM2                                                01374000
LOAD11   BAL       R12,PRINT1                                           01375000
         MVC       5(L'MSG05,R1),MSG05                                  01376000
         SETRC     RC2                                                  01377000
         B         LOAD10                                               01378000
**                                                                      01379000
**  PROCESS DIRECTORY ENTRY                                             01380000
**                                                                      01381000
LOAD20   LH        R2,LLI              READ IN THE RECORD               01382000
         L         R3,PNTR4                                             01383000
         LA        R3,148(R3)                                           01384000
         LR        R7,R3                                                01385000
         TAPEIN    LOAD11,LOAD11                                        01386000
LOAD20X  CLI       TYPE,8              LIST?                            01387000
         BE        LOAD28              YES.                             01388000
         TM        SWA,SWA8            HAS ENTRY BEEN STOWED?           01389000
         BZ        LOAD21              YES                              01390000
         TM        SWB,SWB2            REBLOCK?                         01391000
         BZ        LOAD20Y             NO.                              01392000
         L         R1,PDSBUF2D                                          01393000
         LTR       R1,R1               ANY DATA TO OUTPUT?              01394000
         BNP       LOAD20Y             NO.                              01395000
         L         R5,PDSBUF2A         YES, OUTPUT IT.                  01396000
         BAL       R12,PDSOUT                                           01397000
         XC        PDSBUF2D,PDSBUF2D                                    01398000
         MVC       PDSBUF2C,PDSBUF2A                                    01399000
LOAD20Y  NI        SWB,255-SWB3                                         01400000
         NI        SWA,255-SWA8                                         01401000
         L         R2,PNTR4                                             01402000
         LA        R2,74(R2)                                            01403000
         STOW      PDS,(R2),R                                           01404000
         LR        R4,R15              SAVE STOW'S RC                   01405000
         BAL       R12,PRINT1                                           01406000
         MVC       11(L'MSG07,R1),MSG07                                 01407000
         MVC       L'MSG07+11(8,R1),0(R2)                               01408000
         BAL       R12,STOWMSG                                          01409000
LOAD21   TM        LLI+2,@END          END?                             01410000
         BO        TERM2               YES.                             01411000
         TM        11(R7),ALIAS        IS THIS AN ALIAS?                01412000
         BZ        LOAD25              NO                               01413000
         TM        SWB,SWB5            FLUSH?                           01414000
         BO        LOAD00              YES, SKIP ALIAS PROCESSING       01415000
         TM        SWB,SWB7            SELECT OR EXCLUDE?               01416000
         BZ        LOAD21V             NO.                              01417000
         SEARCH    PNTR9,C' ',R7,LOAD21S,LOAD21T                        01418000
LOAD21S  TM        SWB,SWB8            FOUND                            01419000
         BO        LOAD00              EXCLUDE...IQNORE                 01420000
         B         LOAD21V             SELECT...PROCESS                 01421000
LOAD21T  TM        SWB,SWB8            NOT FOUND                        01422000
         BZ        LOAD00              SELECT...SKIP PROCESSING         01423000
LOAD21V  TM        SWA,SWA5            ADD OR REPLACE?                  01424000
         BO        LOAD21X             REPLACE.                         01425000
         SEARCH    PNTR2,X'FF',R7,LOAD21W,LOAD21X                       01426000
LOAD21W  BAL       R12,PRINT1                                           01427000
         MVC       11(L'MSG11,R1),MSG11                                 01428000
         MVC       L'MSG11+11(8,R1),0(R7)                               01429000
         MVC       L'MSG11+19(L'MSG15B,R1),MSG15B                       01430000
         B         LOAD00                                               01431000
LOAD21X  L         R2,PNTR4            YES, PROCESS IT                  01432000
         MVC       8(3,R7),82(R2)      SET MAIN TTR FOR ALIAS           01433000
         OI        SWA,SWA3            FLAG ALIAS PROCESSING            01434000
         LH        R1,NLCNT                                             01435000
         LTR       R1,R1                                                01436000
         BZ        LOAD21B             NO USER TTR'S                    01437000
         LA        R2,12(R2)                                            01438000
         LA        R3,12(R7)                                            01439000
LOAD21A  MVC       0(3,R3),74(R2)      UPDATE USER TTR'S                01440000
         LA        R2,4(R2)                                             01441000
         LA        R3,4(R3)                                             01442000
         BCT       R1,LOAD21A                                           01443000
LOAD21B  STOW      PDS,(R7),R                                           01444000
         LR        R4,R15              SAVE STOW'S RC                   01445000
         BAL       R12,PRINT1                                           01446000
         MVC       11(L'MSG11,R1),MSG11                                 01447000
         MVC       L'MSG11+11(8,R1),0(R7)                               01448000
         BAL       R12,STOWMSG                                          01449000
         NI        SWA,255-SWA3        RESET ALIAS FLAG                 01450000
         B         LOAD00                                               01451000
**                                                                      01452000
LOAD25   L         R2,PNTR4            GET NEW DIR                      01453000
         MVC       0(74,R2),148(R2)                                     01454000
         MVC       74(74,R2),148(R2)                                    01455000
         OI        SWA,SWA8            TURN ON DIR SWITCH               01456000
         NI        SWB,255-(SWB5+SWB6) TURN OFF FLUSH FLAGS             01457000
         XR        R1,R1                                                01458000
         IC        R1,11(R2)                                            01459000
         SRL       R1,5                                                 01460000
         N         R1,=F'3'                                             01461000
         STH       R1,NLCNT                                             01462000
         NI        SWB,255-SWB2                                         01463000
         TM        SWB,SWB1            REBLOCKING?                      01464000
         BZ        *+14                                                 01465000
         LTR       R1,R1               ANY TTRN?                        01466000
         BNZ       *+8                 YES, NO REBLOCK.                 01467000
         OI        SWB,SWB2            SET FOR REBLOCK                  01468000
         TM        SWB,SWB7            SELECT OR EXCLUDE?               01469000
         BZ        LOAD27              NO.                              01470000
         SEARCH    PNTR9,C' ',R2,LOAD26A,LOAD26B                        01471000
LOAD26A  TM        SWB,SWB8            FOUND                            01472000
         BO        LOAD26C             EXCLUDE...FLUSH                  01473000
         B         LOAD27                                               01474000
LOAD26B  TM        SWB,SWB8            NOT FOUND                        01475000
         BO        LOAD27              EXCLUDE..PROCESS                 01476000
LOAD26C  NI        SWA,255-SWA8        FLUSH                            01477000
         OI        SWB,SWB5                                             01478000
         B         LOAD00                                               01479000
LOAD27   TM        SWA,SWA5            ADD OR REPLACE?                  01480000
         BO        LOAD00              REPLACE...CONTINUE               01481000
         SEARCH    PNTR2,X'FF',R2,LOAD27A,LOAD00                        01482000
LOAD27A  NI        SWA,255-SWA8        FLUSH                            01483000
         OI        SWB,SWB5                                             01484000
         BAL       R12,PRINT1                                           01485000
         MVC       11(L'MSG07,R1),MSG07                                 01486000
         MVC       L'MSG07+11(8,R1),0(R2)                               01487000
         MVC       L'MSG07+19(L'MSG15,R1),MSG15                         01488000
         B         LOAD00                                               01489000
**  'LIST' PROCESSOR                                                    01490000
LOAD28   TM        LLI+2,@END          END OF TAPE?                     01491000
         BO        TERM2               YES, DONE.                       01492000
         BAL       R12,PRINT1          PRINT LIST MESSAGE               01493000
         MVC       11(L'MSG07,R1),MSG07                                 01494000
         MVC       L'MSG07+11(8,R1),0(R7)                               01495000
         TM        11(R7),ALIAS                                         01496000
         BZ        *+10                                                 01497000
         MVC       11(L'MSG11,R1),MSG11                                 01498000
         B         LOAD00                                               01499000
**                                                                      01500000
**  PROCESS DATA RECORD                                                 01501000
**                                                                      01502000
LOAD30M  MVC       0(0,R5),0(R2)                                        01503000
**                                                                      01504000
LOAD30   LH        R2,LLI              GET THE RECORD FROM TAPE         01505000
         L         R3,PDSBUF1                                           01506000
         TAPEIN    LOAD11,LOAD11                                        01507000
         TM        SWB,SWB2            REBLOCK?                         01508000
         BO        LOAD35              YES                              01509000
         LH        R1,LLI              STD. OUTPUT TO THE PDS           01510000
         L         R5,PDSBUF1                                           01511000
         BAL       R12,PDSOUT                                           01512000
         MVC       TTR1,LLITTR                                          01513000
         BAL       R12,UPDIR                                            01514000
         LH        R15,PNTR5NL                                          01515000
         LTR       R15,R15                                              01516000
         BZ        LOAD00                                               01517000
         L         R14,PNTR5A                                           01518000
         CLC       LLITTR,0(R14)                                        01519000
         BNE       LOAD00                                               01520000
         MVC       0(3,R14),TTR2                                        01521000
         LA        R14,4(R14)                                           01522000
         BCTR      R15,0                                                01523000
         ST        R14,PNTR5A                                           01524000
         STH       R15,PNTR5NL                                          01525000
         B         LOAD00                                               01526000
LOAD35   L         R2,PDSBUF1                                           01527000
         LH        R3,LLI                                               01528000
         L         R4,PDSBUF2D                                          01529000
         L         R5,PDSBUF2C                                          01530000
LOAD36   LH        R15,PDS+(DCBLRECL-IHADCB)                            01531000
         SR        R3,R15                                               01532000
         AR        R4,R15                                               01533000
LOAD36A  CH        R15,=H'256'                                          01534000
         BNH       LOAD36B                                              01535000
         MVC       0(256,R5),0(R2)                                      01536000
         LA        R2,256(R2)                                           01537000
         LA        R5,256(R5)                                           01538000
         SH        R15,=H'256'                                          01539000
         B         LOAD36A                                              01540000
LOAD36B  BCTR      R15,0                                                01541000
         EX        R15,LOAD30M                                          01542000
         LA        R2,1(R2,R15)                                         01543000
         LA        R5,1(R5,R15)                                         01544000
         C         R4,PDSBUF2B                                          01545000
         BL        LOAD37                                               01546000
         LR        R1,R4                                                01547000
         L         R5,PDSBUF2A                                          01548000
         ST        R5,PDSBUF2C                                          01549000
         XR        R4,R4                                                01550000
         BAL       R12,PDSOUT                                           01551000
         TM        SWB,SWB3            FIRST TIME THRU?                 01552000
         BO        LOAD37              NO, SKIP DIR UPDATE              01553000
         OI        SWB,SWB3                                             01554000
         MVC       TTR1,LLITTR                                          01555000
         BAL       R12,UPDIR                                            01556000
LOAD37   LTR       R3,R3                                                01557000
         BP        LOAD36                                               01558000
         ST        R4,PDSBUF2D                                          01559000
         ST        R5,PDSBUF2C                                          01560000
         B         LOAD00                                               01561000
**                                                                      01562000
**  PROCESS NOTELIST RECORD                                             01563000
**                                                                      01564000
LOAD40   L         R4,PNTR5                                             01565000
         LTR       R4,R4                                                01566000
         BNZ       LOAD41                                               01567000
         LH        R0,PNTR5L                                            01568000
         GETMAIN   R,LV=(0)                                             01569000
         ST        R1,PNTR5                                             01570000
         LR        R4,R1                                                01571000
LOAD41   TM        SWA,SWA7            NOTELIST TO OUTPUT?              01572000
         BZ        LOAD42              NO                               01573000
         NI        SWA,255-SWA7        YES                              01574000
         L         R5,PNTR5                                             01575000
         LH        R1,PNTR5CNT                                          01576000
         BAL       R12,PDSOUT                                           01577000
         MVC       TTR1(3),TTR3                                         01578000
         BAL       R12,UPDIR           UPDATE DIRECTORY ENTRY           01579000
LOAD42   TM        LLI+2,@DUM          PROCESSING DUMMY INPUT?          01580000
         BO        LOAD00              YES, DONE.                       01581000
         LH        R2,LLI              NO, GET NEXT NOTELIST            01582000
         L         R3,PNTR5                                             01583000
         TAPEIN    LOAD11,LOAD11                                        01584000
         OI        SWA,SWA7                                             01585000
         MVC       PNTR5CNT,LLI                                         01586000
         MVC       PNTR5A,PNTR5                                         01587000
         L         R14,PNTR4                                            01588000
         LH        R15,NLCNT                                            01589000
         LA        R14,12(R14)                                          01590000
         XR        R1,R1                                                01591000
LOAD43   CLC       LLITTR,0(R14)                                        01592000
         BNE       *+16                                                 01593000
         IC        R1,3(R14)                                            01594000
         STH       R1,PNTR5NL                                           01595000
         B         LOAD44                                               01596000
         LA        R14,4(R14)                                           01597000
         BCT       R15,LOAD43                                           01598000
LOAD44   MVC       TTR3(3),LLITTR                                       01599000
         B         LOAD00                                               01600000
**                                                                      01601000
**  PROCESS DUMMY INPUT RECORD                                          01602000
**                                                                      01603000
LOAD50   LH        R2,LLI                                               01604000
         LA        R3,WORK1                                             01605000
         LTR       R2,R2                                                01606000
         BNP       LOAD51                                               01607000
         TAPEIN    LOAD11,LOAD11                                        01608000
LOAD51   B         LOAD41              TO PROCESS ANY NOTELISTS         01609000
**                                                                      01610000
**  FLUSH INPUT FOR THIS ENTRY                                          01611000
**                                                                      01612000
LOAD60   OI        SWB,SWB6            TURN ON TAPEIN'S FLAG            01613000
         LH        R2,LLI              LENGTH OF DATA TO SKIP           01614000
         TAPEIN    LOAD11,LOAD11                                        01615000
         NI        SWB,255-SWB6        TURN FLAG OFF                    01616000
         B         LOAD00                                               01617000
**                                                                      01618000
**  END OF OPERATION:  TERMINATE                                        01619000
**                                                                      01620000
TERM2    TM        TAPE+(DCBOFLGS-IHADCB),OFLG                          01621000
         BZ        TERM2X                                               01622000
         TM        SWA,SWA6            LEAVE TAPE?                      01623000
         BZ        TERM2V              NO.                              01624000
         CLOSE     (TAPE,LEAVE)                                         01625000
         B         TERM2W                                               01626000
TERM2V   CLOSE     TAPE                                                 01627000
TERM2W   LA        R1,TAPE                                              01628000
         BAL       R12,FREEPOOL                                         01629000
TERM2X   CLOSE     PDS                                                  01630000
         L         R1,PDSBUF1          FREE THE PDS BUFFER              01631000
         LTR       R1,R1                                                01632000
         BZ        TERM2A              NO BUFFER ALLOCATED              01633000
         LH        R0,PDSBUFL                                           01634000
         FREEMAIN  R,LV=(0),A=(1)                                       01635000
**  FREE CORE FOR ANY ALLOCATED AREAS                                   01636000
TERM2A   L         R1,PNTR4                                             01637000
         LTR       R1,R1                                                01638000
         BZ        TERM2B                                               01639000
         FREEMAIN  R,LV=260,A=(1)                                       01640000
TERM2B   L         R1,PNTR5                                             01641000
         LTR       R1,R1                                                01642000
         BZ        TERM2C                                               01643000
         LH        R0,PNTR5L                                            01644000
         FREEMAIN  R,LV=(0),A=(1)                                       01645000
TERM2C   L         R1,PNTR1                                             01646000
         LTR       R1,R1                                                01647000
         BNP       TERM2D                                               01648000
         FREEMAIN  R,LV=260,A=(1)                                       01649000
TERM2D   L         R2,PNTR2                                             01650000
         XC        PNTR2,PNTR2                                          01651000
TERM2D1  LTR       R1,R2                                                01652000
         BZ        TERM2E                                               01653000
         L         R2,0(R2)                                             01654000
         FREEMAIN  R,LV=260,A=(1)                                       01655000
         B         TERM2D1                                              01656000
TERM2E   L         R2,PNTR9                                             01657000
         XC        PNTR9,PNTR9                                          01658000
TERM2E1  LTR       R1,R2                                                01659000
         BZ        TERM2Z                                               01660000
         L         R2,0(R2)                                             01661000
         FREEMAIN  R,LV=260,A=(1)                                       01662000
         B         TERM2E1                                              01663000
TERM2Z   TM        SWA,SWA1            ABORT?                           01664000
         BO        BEGIN                                                01665000
         LA        R15,2               PRINT TERM MSG                   01666000
         BAL       R12,PRINTA                                           01667000
         MVI       0(R1),C'0'                                           01668000
         MVC       5(L'MSG03,R1),MSG03                                  01669000
         B         BEGIN                                                01670000
 SPACE 4                                                                01671000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01672000
**  PDS OUTPUT PROCESSOR                                                01673000
PDSOUT   STH       R1,PDS+(DCBBLKSI-IHADCB)                             01674000
         WRITE     DECBOUT,SF,PDS,(R5),'S'                              01675000
         CHECK     DECBOUT                                              01676000
         NOTE      PDS                                                  01677000
         ST        R1,TTR2                                              01678000
         BR        R12                                                  01679000
 SPACE 4                                                                01680000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01681000
**  STOW MESSAGE HANDLERS                                               01682000
STOWMSG  B         *+4(R4)             PROCESS STOW RETURN CODE         01683000
         B         STOWMSGA            RC=00                            01684000
         B         STOWMSGE            RC=04                            01685000
         B         STOWMSGC            RC=08                            01686000
         B         STOWMSGD            RC=12                            01687000
         B         STOWMSGE            RC=16                            01688000
STOWMSGA MVC       26(L'MSG14,R1),MSG14                                 01689000
         BR        R12                                                  01690000
STOWMSGC MVC       26(L'MSG10,R1),MSG10                                 01691000
         BR        R12                                                  01692000
STOWMSGD MVC       26(L'MSG17,R1),MSG17                                 01693000
         SETRC     RC3                                                  01694000
         B         LOAD10                                               01695000
STOWMSGE MVC       26(L'MSG18,R1),MSG18                                 01696000
         SETRC     RC3                                                  01697000
         B         LOAD10                                               01698000
 SPACE 4                                                                01699000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01700000
**  UPDATE DIRECTORY TTR'S                                              01701000
UPDIR    L         R14,PNTR4                                            01702000
         CLC       TTR1(3),8(R14)                                       01703000
         BNE       UPDIRA                                               01704000
         MVC       82(3,R14),TTR1                                       01705000
         BR        R12                                                  01706000
UPDIRA   LH        R15,NLCNT                                            01707000
         LTR       R15,R15                                              01708000
         BZR       R12                                                  01709000
         LA        R14,12(R14)                                          01710000
UPDIRB   CLC       TTR1(3),0(R14)                                       01711000
         BNE       UPDIRC                                               01712000
         MVC       74(3,R14),TTR2                                       01713000
UPDIRC   LA        R14,4(R14)                                           01714000
         BCT       R15,UPDIRB                                           01715000
         BR        R12                                                  01716000
 TITLE '           END OF TASK PROCESSING'                              01717000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01718000
**  END OF TASK PROCESSING                                          **  01719000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01720000
PDSUR    CSECT                                                          01721000
ENDTASK  CLOSE     SYSIN                                                01722000
         LA        R1,SYSIN                                             01723000
         BAL       R12,FREEPOOL                                         01724000
ENDTASK2 BAL       R12,PRINT1                                           01725000
         BAL       R12,PRINT1                                           01726000
         MVC       5(L'MSG02,R1),MSG02                                  01727000
         CLOSE     SYSPRINT                                             01728000
         LA        R1,SYSPRINT         FOR FREEPOOL                     01729000
         BAL       R12,FREEPOOL                                         01730000
         STAE      0                   CANCEL THE STAE                  01731000
         LH        R15,RETCOD                                           01732000
         L         R13,4(R13)          RESTORE OLD SAVE AREA            01733000
         RETURN    (14,12),T,RC=(15)                                    01734000
 TITLE '           SYNCHRONOUS ERROR HANDLING'                          01735000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01736000
**  SYNCHRONOUS  ERROR  HANDLING                                    **  01737000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01738000
SYNERR1  SYNADAF   ACSMETH=BPAM                                         01739000
         STM       R14,R12,SYNSAV                                       01740000
         LR        R2,R1               SAVE PNTR TO MSG                 01741000
         BAL       R12,PRINT1          PRINT THE MSG                    01742000
         BAL       R12,PRINT1                                           01743000
         MVC       5(L'MSG19,R1),MSG19                                  01744000
         BAL       R12,PRINT1                                           01745000
         MVC       15(78,R1),50(R2)                                     01746000
         SETRC     RC2                                                  01747000
         LM        R14,R12,SYNSAV      RESTORE THE REGS                 01748000
         SYNADRLS                                                       01749000
         OI        SWA,SWA1            SET TERM FLAG                    01750000
         XR        R15,R15                                              01751000
         IC        R15,TYPE                                             01752000
         L         R15,ATERM(R15)      ADRS OF APPROPRIATE TERMINATOR   01753000
         BR        R15                                                  01754000
ATERM    DC        A(TERM1,TERM2)                                       01755000
 TITLE '           SUBROUTINES'                                         01756000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01757000
**  PROCESS PAGING AND SET UP FOR PRINTING                              01758000
PRINT1   LA        R15,1               SET TO ONE LINE                  01759000
         B         PRINTA                                               01760000
PRINT2   LA        R15,256             FOR PAGE EJECT                   01761000
**                                                                      01762000
PRINTA   AH        R15,PCNT                                             01763000
         STH       R15,PCNT                                             01764000
         CH        R15,PLIM                                             01765000
         BNH       PRINTB                                               01766000
         MVC       PCNT,=H'4'                                           01767000
         PUT       SYSPRINT                                             01768000
         BAL       R14,CLEARBUF                                         01769000
         MVI       0(R1),C'1'                                           01770000
         MVC       2(L'HEADING,R1),HEADING                              01771000
         MVC       110(4,R1),=C'PAGE'                                   01772000
         LH        R15,PNUM            UPDATE AND FORMAT PAGE NO.       01773000
         LA        R15,1(R15)                                           01774000
         STH       R15,PNUM                                             01775000
         CVD       R15,WORK1                                            01776000
         OI        WORK1+7,X'0F'                                        01777000
         UNPK      116(4,R1),WORK1(8)                                   01778000
         PUT       SYSPRINT                                             01779000
         BAL       R14,CLEARBUF                                         01780000
         MVI       0(R1),C'0'          DOUBLE SPACE                     01781000
PRINTB   PUT       SYSPRINT                                             01782000
         BAL       R14,CLEARBUF                                         01783000
         BR        R12                 RETURN TO CALLER                 01784000
**  CLEAR PRINT BUFFER                                                  01785000
CLEARBUF MVI       0(R1),C' '                                           01786000
         MVC       1(120,R1),0(R1)                                      01787000
         BR        R14                                                  01788000
 SPACE 4                                                                01789000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01790000
**  TAPE INPUT PROCESSOR                                                01791000
**       NORMAL RETURN   - OFFSET=4                                     01792000
**       ERR/EOF RETURN  - OFFSET=0                                     01793000
TAPEIN   LTR       R2,R2               ANY BYTES LEFT?                  01794000
         BNP       4(R12)              NO, RETURN.                      01795000
         LM        R14,R15,PNTR8A      LOAD BUFFER PARMS                01796000
         LTR       R1,R15              ANY SPACE AVAILABLE?             01797000
         BNP       TAPEIN2             NO, GET NEW REC                  01798000
         CR        R2,R15              COMPARE REQUEST TO AVAILABLE     01799000
         BH        TAPEIN1             REQUEST GREATER                  01800000
         LA        R0,0(R2,R14)        PROCESS SMALL REQUEST            01801000
         SR        R1,R2                                                01802000
         STM       R0,R1,PNTR8A                                         01803000
         TM        SWB,SWB6            FLUSH?                           01804000
         BO        4(R12)              YES, SKIP MOVING DATA.           01805000
         BCTR      R2,0                                                 01806000
         EX        R2,TAPEINM                                           01807000
         B         4(R12)                                               01808000
TAPEIN1  TM        SWB,SWB6            FLUSH?                           01809000
         BO        *+12                YES, SKIP MOVING DATA            01810000
         BCTR      R15,0                                                01811000
         EX        R15,TAPEINM                                          01812000
         AR        R3,R1                                                01813000
         SR        R2,R1                                                01814000
TAPEIN2  GET       TAPE                                                 01815000
         LA        R14,2(R1)                                            01816000
         LA        R15,78                                               01817000
         STM       R14,R15,PNTR8A                                       01818000
         LH        R1,0(R1)                                             01819000
         LH        R14,TAPECNTR                                         01820000
         LA        R14,1(R14)                                           01821000
         STH       R14,TAPECNTR                                         01822000
         LH        R14,TAPECNTR                                         01823000
         CR        R14,R1                                               01824000
         BE        TAPEIN                                               01825000
         BR        R12                 SEQUENCE ERROR.                  01826000
TAPEINM  MVC       0(0,R3),0(R14)                                       01827000
**                                                                      01828000
TAPEEOF  BR        R12                 EOF RETURN                       01829000
 SPACE 4                                                                01830000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01831000
**  FREE BUFFER POOL                                                    01832000
FREEPOOL FREEPOOL  (1)                                                  01833000
         BR        R12                                                  01834000
 SPACE 4                                                                01835000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01836000
**  SET RETURN CODE                                                     01837000
SETRET   CH        R15,RETCOD          LESS THAN OR EQUAL?              01838000
         BNHR      R14                 YES, RETURN                      01839000
         STH       R15,RETCOD          NO, REPLACE                      01840000
         BR        R14                                                  01841000
 SPACE 4                                                                01842000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01843000
**  READ IN A CONTROL CARD                                              01844000
GETCARD  GET       SYSIN                                                01845000
         B         4(R12)                                               01846000
EOFSYSIN OI        SWX,SWX1            FLAG EOF ON SYSIN                01847000
         BR        R12                                                  01848000
 TITLE '           DCB  EXIT  ROUTINES'                                 01849000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01850000
**  DCB EXIT ROUTINES                                               **  01851000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01852000
         USING     IHADCB,R1                                            01853000
EXIT1    LH        R4,DCBLRECL         CHECK BLKSIZE                    01854000
         CH        R4,DCBBLKSI                                          01855000
         BNH       EXIT1B              BLKSIZE HIGHER THAN LRECL        01856000
         CH        R4,=H'121'          IS THIS SYSPRINT?                01857000
         BNE       EXIT1A              NO.                              01858000
         LA        R4,3509             YES, DEFAULT TO HALF TRK.        01859000
         STH       R4,DCBBLKSI                                          01860000
         BR        R14                                                  01861000
EXIT1A   STH       R4,DCBBLKSI         DEFAULT TO BLKSIZE=LRECL         01862000
         NI        DCBRECFM,X'EF'      AND TURN OFF BLOCKED BIT         01863000
EXIT1B   XR        R2,R2               CHECK FOR BLKSIZE MULT OF        01864000
         LH        R3,DCBBLKSI         LRECL                            01865000
         DR        R2,R4                                                01866000
         MH        R3,DCBLRECL         AND FORCE IT TO BE               01867000
         STH       R3,DCBBLKSI                                          01868000
         BR        R14                                                  01869000
**                                                                      01870000
EXIT2    MVI       DCBBUFNO,0          PDS DCB EXIT                     01871000
         CLI       TYPE,4              RELOAD?                          01872000
         BE        EXIT2A              YES                              01873000
         MVC       SAVRECFM,DCBRECFM                                    01874000
         MVC       SAVLRECL,DCBLRECL                                    01875000
         MVC       SAVBLKSI,DCBBLKSI                                    01876000
         TM        SWX,SWX2            PASS #2?                         01877000
         BOR       R14                 NO.                              01878000
         TM        DCBRECFM,RECF       RECFM=V?                         01879000
         BOR       R14                 NO, RETURN                       01880000
         MVI       DCBRECFM,RECF       YES, SET TO RECFM=F FOR PASS 1   01881000
         BR        R14                                                  01882000
EXIT2A   CLI       DCBRECFM,0          NEW OUTPUT PDS                   01883000
         BNE       *+10                                                 01884000
         MVC       DCBRECFM,SAVRECFM                                    01885000
         MVC       WORK1(1),SAVRECFM                                    01886000
         MVC       WORK1+1(1),DCBRECFM                                  01887000
         NC        WORK1(2),=X'C0C0'                                    01888000
         CLC       WORK1(1),WORK1+1                                     01889000
         BNER      R14                 DIFFERENT RECFMS...ABORT         01890000
EXIT2B   TM        DCBRECFM,RECU                                        01891000
         BNO       EXIT2C              NOT RECFM=U                      01892000
         NI        DCBRECFM,255-(RECFB-RECF)                            01893000
EXIT2BA  CLC       DCBLRECL,SAVLRECL                                    01894000
         BH        *+10                                                 01895000
         MVC       DCBLRECL,SAVLRECL                                    01896000
         CLC       DCBBLKSI,SAVBLKSI                                    01897000
         BH        *+10                                                 01898000
         MVC       DCBBLKSI,SAVBLKSI                                    01899000
         BR        R14                                                  01900000
EXIT2C   TM        DCBRECFM,RECF                                        01901000
         BO        EXIT2D              FOR RECFM=F/FB                   01902000
         CLC       DCBLRECL,SAVLRECL   RECFM=V/VB                       01903000
         BH        *+10                                                 01904000
         MVC       DCBLRECL,SAVLRECL                                    01905000
         CLC       DCBBLKSI,SAVBLKSI                                    01906000
         BH        *+10                                                 01907000
         MVC       DCBBLKSI,SAVBLKSI                                    01908000
         BR        R14                                                  01909000
EXIT2D   CLC       DCBLRECL,=H'0'      RECFM=F/FB                       01910000
         BNE       *+10                                                 01911000
         MVC       DCBLRECL,SAVLRECL                                    01912000
         TM        DCBRECFM,RECFB      BLOCKED?                         01913000
         BO        EXIT2E              YES                              01914000
         CLC       DCBBLKSI,=H'0'                                       01915000
         BNE       *+10                                                 01916000
         MVC       DCBBLKSI,SAVLRECL                                    01917000
         BR        R14                                                  01918000
EXIT2E   CLC       DCBBLKSI,=H'0'                                       01919000
         BNE       *+10                                                 01920000
         MVC       DCBBLKSI,SAVBLKSI                                    01921000
         XR        R2,R2               ROUND BLKSIZE TO LRECL           01922000
         LH        R3,DCBBLKSI                                          01923000
         LH        R4,DCBLRECL                                          01924000
         DR        R2,R4                                                01925000
         MH        R3,DCBLRECL                                          01926000
         STH       R3,DCBBLKSI                                          01927000
         BR        R14                                                  01928000
**                                                                      01929000
EXIT3    LH        R4,DCBLRECL         TAPE DCB EXIT                    01930000
         CH        R4,DCBBLKSI                                          01931000
         BNE       *+10                                                 01932000
         NI        DCBRECFM,X'EF'      RECFM=F                          01933000
         BR        R14                                                  01934000
         BL        *+14                                                 01935000
         LA        R4,800              DEFAULT BLKSIZE=800              01936000
         STH       R4,DCBBLKSI                                          01937000
         BR        R14                                                  01938000
         XR        R2,R2                                                01939000
         LH        R3,DCBBLKSI                                          01940000
         DR        R2,R4                                                01941000
         MH        R3,DCBLRECL                                          01942000
         STH       R3,DCBBLKSI                                          01943000
         BR        R14                                                  01944000
**                                                                      01945000
EXIT4    MVI       DCBBUFNO,0                                           01946000
         BR        R14                                                  01947000
         DROP      R1                                                   01948000
 TITLE '           STAE  EXIT  ROUTINE'                                 01949000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01950000
**  STAE  EXIT  ROUTINE                                             **  01951000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01952000
         USING     STAEXIT,R15                                          01953000
STAEXIT  CLOSE     SYSPRINT                                             01954000
         BR        R14                                                  01955000
         DROP      R15                                                  01956000
 TITLE '           DATA CONSTANTS, STORAGE AND LITERALS'                01957000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01958000
**  DATA CONSTANTS, STORAGE AND LITERALS                            **  01959000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  01960000
SAVEAREA DC        9D'0'               OS REG SAVE AREA                 01961000
WORK1    DC        4D'0'                                                01962000
SYNSAV   DC        8D'0'                                                01963000
FROMDD   DC        CL8' '                                               01964000
TODD     DC        CL8' '                                               01965000
ALLOCM   DC        F'0'                                                 01966000
ALLOCS   DC        F'0'                                                 01967000
LASTCARD DC        A(0)                                                 01968000
PDSBUF1  DC        A(0)                PNTR TO PDS BUFFER               01969000
PDSBUF2A DC        A(0)                REBLOCKING BUFFER                01970000
PDSBUF2B DC        A(0)                                                 01971000
PDSBUF2C DC        A(0)                                                 01972000
PDSBUF2D DC        A(0)                                                 01973000
TTR1     DC        F'0'                                                 01974000
TTR2     DC        F'0'                                                 01975000
TTR3     DC        F'0'                                                 01976000
SAVEPNTR DC        4F'0'               FOR SAVING PNTR VALUES           01977000
PNTR1    DC        A(0)                PNTR TO HEAD OF DIRECTORY TABLE  01978000
PNTR2    DC        A(0)                PNTR TO CURRENT DIR TBL PSTN     01979000
PNTR3    DC        A(0)                PNTR TO CURRENT ENTRY IN BLOCK   01980000
PNTR3A   DC        A(0)                "BYTE COUNT" FOR CURRENT BLK     01981000
PNTR3B   DC        A(0)                "BYTE COUNT" FOR CURRENT ENTRY   01982000
PNTR4    DC        A(0)                PNTR TO DIR ENTRIES FOR LOAD     01983000
PNTR4A   DC        A(0)                                                 01984000
PNTR5    DC        A(0)                PNTR TO LOAD-NOTELIST WORKAREA   01985000
PNTR5A   DC        A(0)                                                 01986000
PNTR8A   DC        A(0)                TAPE BUFFER POINTER              01987000
PNTR8B   DC        A(0)                TAPE BFR AVAIL SPACE             01988000
PNTR9    DC        A(0)                PNTR TO SELECT/EXCLUDE MEMBER    01989000
*                                      NAME TABLE                       01990000
         DS        0F                  DCB EXIT LISTS                   01991000
EXLST1   DC        X'85',AL3(EXIT1)                                     01992000
EXLST2   DC        X'07',AL3(JFCB1),X'85',AL3(EXIT2)                    01993000
EXLST3   DC        X'07',AL3(JFCB2),X'85',AL3(EXIT3)                    01994000
EXLST4   DC        X'07',AL3(JFCB1),X'85',AL3(EXIT4)                    01995000
RETCOD   DC        H'0'                                                 01996000
PNUM     DC        H'0'                                                 01997000
PCNT     DC        H'0'                                                 01998000
PDSBUFL  DC        H'0'                                                 01999000
BLKLEN   DC        H'0'                LENGTH OF CURRENT BLK            02000000
PLIM     DC        H'60'               MAX LINES PER PAGE               02001000
PNTR5L   DC        H'0'                LENGTH OF NOTELIST WORKAREA      02002000
PNTR5NL  DC        H'0'                LENGTH OF NOTELIST               02003000
PNTR5CNT DC        H'0'                LENGTH OF NOTELIST RECORD        02004000
NLCNT    DC        H'0'                COUNT OF NL TTRS IN DIR          02005000
TAPECNTR DC        H'0'                                                 02006000
USEDBLKS DC        H'0'                                                 02007000
MONTHS1  DC        H'31,28,31,30,31,30,31,31,30,31,30,31'               02008000
MONTHS2  DC        H'31,29,31,30,31,30,31,31,30,31,30,31'               02009000
MONTHS3  DC        C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'              02010000
LLI      DC        X'000000'           FOR UNLOADED RECORDS             02011000
LLITTR   DC        X'000000'                                            02012000
SAVLRECL DC        H'0'                                                 02013000
SAVBLKSI DC        H'0'                                                 02014000
SAVRECFM DC        X'00'                                                02015000
UNLREC1  DC        FL2'75',X'0E'       IBM'S ID RECORD                  02016000
         DC        C'THIS IS AN UNLOADED DATA SET PRODUCED BY'          02017000
         DC        X'80',C'THE IBM UTILITY, SYSMOVE.OMMBRLDWB'          02018000
UNLREC1L EQU       *-UNLREC1                                            02019000
LASTREC  DC        AL1(0,0,@END),C'THIS UNLOADED PDS WAS CREATED BY THE+02020000
                GENE CZARCINSKI/GSFC UTILITY PROGRAM PDSUR.'            02021000
LASTRECL EQU       *-LASTREC                                            02022000
TYPE     DC        X'00'                                                02023000
SWITCHES DS        0XL2                                                 02024000
SWA      DC        X'00'                                                02025000
SWB      DC        X'00'                                                02026000
SWX      DC        X'00'                                                02027000
         LTORG                                                          02028000
 TITLE '            MESSAGES'                                           02029000
HEADING  DC        C'PDSUR -- 1.2/03JUL73                      PARTITIO+02030000
               NED DATA SET UNLOAD/RELOAD UTILITY PROGRAM'              02031000
MSG01    DC        C'CONTROL CARD ERROR. CARD IGNORED.'                 02032000
MSG02    DC        C'END OF TASK.'                                      02033000
MSG03    DC        C'END OF OPERATION.'                                 02034000
MSG04    DC        C'REQUIRED CONTROL CARD OPERAND MISSING.'            02035000
MSG05    DC        C'INVALID DUMP DATA SET FORMAT. OPERATION ABORTED.'  02036000
MSG06    DC        C' HAS BEEN UNLOADED.'                               02037000
MSG07    DC        C'MEMBER '                                           02038000
MSG08    DC        C'OPEN ERROR FOR DDNAME='                            02039000
MSG09    DC        C'OPERATION TERMINATED.'                             02040000
MSG10    DC        C' HAS BEEN RELOADED.'                               02041000
MSG11    DC        C'ALIAS  '                                           02042000
MSG12    DC        C'OPERATION ABORTED.'                                02043000
MSG13    DC        C'INVALID DUMP DATA SET IDENTIFICATION.  OPERATION A+02044000
               BORTED.'                                                 02045000
MSG14    DC        C' HAS BEEN RELOADED AND REPLACED IN LIBRARY.'       02046000
MSG15    DC        C' ALREADY EXISTS.  MEMBER NOT ADDED.'               02047000
MSG15A   DC        C'FOR ABOVE MEMBER NOT ADDED.'                       02048000
MSG15B   DC        C'ALREADY EXITS. NOT ADDED.'                         02049000
MSG16    DC        C'INVALID DCB/DATA SET PARMS FOR PDS FILE.'          02050000
MSG17    DC        C' NOT ADDED.  DIRECTORY ERROR. NO SPACE LEFT.'      02051000
MSG18    DC        C' NOT ADDED.  PERMANENT I/O ERROR DETECTED ATTEMPTI+02052000
               NG TO UPDATE DIRECTORY.'                                 02053000
MSG19    DC        C'SYNCHRONOUS  I/O  ERROR  DETECTED  --'             02054000
MSG20    DC        C'EXECUTION  ABORTED  *****************'             02055000
MSG21    DC        C'ERROR DETECTED READING NOTELIST.  '                02056000
MSG22A   DC        C'FROM -'                                            02057000
MSG22B   DC        C'TO   -'                                            02058000
MSG22C   DC        C'VOL='                                              02059000
MSG22D   DC        C'DSN='                                              02060000
MSG23    DC        C'NOTICE--REBLOCKING INVOKED (VALID FOR RECFM=F/FB O+02061000
               NLY).'                                                   02062000
MSG24A   DC        C'PDS CHARACTERISTICS -'                             02063000
MSG24B   DC        C'RECFM=        LRECL=       BLKSIZE=       DIR-BLKS+02064000
               -ALLOC=       '                                          02065000
MSG24C   DC        C'MAIN ALLOC=     ,  SEC. ALLOC=       (DASD TRACKS)+02066000
               '                                                        02067000
MSG24D   DC        C'OLD PDS CHARACTERISTICS -'                         02068000
MSG24E   DC        C'NEW PDS CHARACTERISTICS -'                         02069000
MSG24F   DC        C'RECFM=        LRECL=       BLKSIZE=      '         02070000
MSG25    DC        C' DATE/TIME =',X'4021204B4B4B20204021204B20204B2020+02071000
               '                                                        02072000
MSG25L   EQU       *-MSG25                                              02073000
 TITLE '            DCB''S'                                             02074000
SYSIN    DCB       DSORG=PS,MACRF=(GL),DDNAME=SYSIN,EODAD=EOFSYSIN,    +02075000
               RECFM=FB,LRECL=80,EXLST=EXLST1                           02076000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  02077000
SYSPRINT DCB       DSORG=PS,MACRF=(PL),DDNAME=SYSPRINT,                +02078000
               RECFM=FBA,LRECL=121,EXLST=EXLST1                         02079000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  02080000
TAPE     DCB       DSORG=PS,MACRF=(GL,PL),DDNAME=TAPE,EODAD=TAPEEOF,   +02081000
               RECFM=FB,LRECL=80,EXLST=EXLST3                           02082000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  02083000
PDS      DCB       DSORG=PO,MACRF=(R,W),DDNAME=PDS,EODAD=PDSEOF,       +02084000
               BUFNO=0,NCP=1,EXLST=EXLST2,SYNAD=SYNERR1                 02085000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  02086000
PDSDIR   DCB       DSORG=PO,MACRF=(R),DDNAME=PDS,EXLST=EXLST4,         +02087000
               BUFNO=0,NCP=1,RECFM=F,LRECL=256,BLKSIZE=256,            +02088000
               SYNAD=SYNERR1,EODAD=DIREOF                               02089000
 TITLE '           JFCB''S, THE PDS DSCB, AND A WORKAREA'               02090000
PDSDSCB  CAMLST    SEARCH,JFCB1+JFCBDSNM,JFCB1+JFCBVOLS,DSCB            02091000
**                                                                      02092000
         DS        0D                                                   02093000
**                                                                      02094000
JFCB1    DC        XL176'00'           FOR PDS                          02095000
**                                                                      02096000
JFCB2    DC        XL176'00'           FOR TAPE                         02097000
**                                                                      02098000
         DS        0D                                                   02099000
         DC        X'00'               FOR ALIGNMENT                    02100000
UNLREC2  DC        X'00C500'           LLI FOR REC 2 (OS REL 21)        02101000
WORKAREA EQU       *                                                    02102000
DSNAME   DC        CL44' '                                              02103000
DSCB     DC        XL200'00',XL150'00'                                  02104000
         ORG       WORKAREA+140                                         02105000
UNLDDIR  DS        XL2                 DIRECTORY QUANTITY               02106000
UNLDFLAG DS        XL1                 UNLOAD CONTROL FLAG              02107000
UNLDSIZE DS        XL4                 PRIMARY ALLOC (IN BYTES)         02108000
UNLDSSIZ DS        XL4                 SECONDARY ALLOC (IN BYTES)       02109000
UNLDSPAR DS        XL42                UNUSED                           02110000
UNLDDEVT DS        XL4                 DEVICE TYPE INFO                 02111000
         ORG                                                            02112000
DS1RECFM EQU       DSCB+40                                              02113000
DS1BLKL  EQU       DSCB+42                                              02114000
DS1LRECL EQU       DSCB+44                                              02115000
DS1SCALO EQU       DSCB+50                                              02116000
 TITLE '           DSECTS'                                              02117000
         DCBD      DSORG=(QS,PO),DEVD=(DA,TA)                           02118000
**                                                                  **  02119000
**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**==**  02120000
**                                                                  **  02121000
         END       PDSUR                                                02122000
@@
//LKED.SYSLMOD DD DISP=SHR,DSN=SYSC.LINKLIB    <-- Target Load Library  00100000
//LKED.SYSIN DD *                                                       00110000
 SETSSI  CB492949                                                       00120000
 SETCODE AC(0)                                                          00130000
 NAME    PDSUR(R)                                                       00140000
/*                                                                      00150000
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=PDSURL01
//SBGOLOBL  JOB (ACCT#),S-GOLOB,
// NOTIFY=&SYSUID,
// CLASS=B,MSGCLASS=X
//* ------------------------------------------------------- *//
//*    RELOAD AN IEHMOVE FORMAT FILE TO A PDS USING PDSUR   *//
//* ------------------------------------------------------- *//
//STEP1    EXEC PGM=PDSUR
//STEPLIB  DD DSN=SYS1.W$$.LINKLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//IN       DD DSN=SBGOLOB.IEHMOVE.B.CNTL,DISP=SHR     (SEQ)
//OUT      DD DSN=SBGOLOB.TEST.B.CNTL,DISP=SHR        (PDS)
//SYSIN    DD *
 RELOAD FROMDD=IN,TODD=OUT
/*
./ ADD NAME=PDSURN01
//SBGOLOBN  JOB (ACCT#),S-GOLOB,
// NOTIFY=&SYSUID,
// CLASS=B,MSGCLASS=X
//* UNLOAD A PDS TO AN IEHMOVE FILE
//* ------------------------------------------------------- *//
//*    UNLOAD A PDS TO AN IEHMOVE FORMAT FILE USING PDSUR   *//
//* ------------------------------------------------------- *//
//STEP1    EXEC PGM=PDSUR
//STEPLIB  DD DSN=SYS1.W$$.LINKLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//IN       DD DSN=SBGOLOB.B.CNTL,DISP=SHR             (PDS)
//OUT      DD DSN=SBGOLOB.IEHMOVE.B.CNTL,DISP=SHR     (SEQ)
//SYSIN    DD *
 UNLOAD FROMDD=IN,TODD=OUT
/*