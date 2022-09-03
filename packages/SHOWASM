//SHOWASM  JOB (TSO),
//             'Install SHOWASM',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00020000
//* ***************************************************************** * 00030000
//* INSTALL SHOWASM COMMAND IN SYS2.CMDLIB (HELP IN SYS2.HELP)        * 00040000
//* ***************************************************************** * 00050000
//*                                                                     00060000
//SHOWASM EXEC ASMFCL,PARM.ASM='LIST,NODECK,LOAD',                      00070000
//             MAC1='SYS1.AMODGEN',PARM.LKED='LIST,MAP,NOCAL'           00080000
//ASM.SYSIN DD *                                                        00090000
         MACRO                                                          00000100
&NAME    PGM   &BASE=R12,&SAVE=$$SAVE,&START=$$START,&EOJ=$$EOJ,&RC=16  00000200
&NAME    CSECT                                                          00000300
         B     14(0,R15)           BRANCH AROUND PROGRAM ID.            00000400
         DC    AL1(8)              CSECT NAME LENGTH.                   00000500
         DC    CL8'&NAME'          CSECT IDENTIFIER.                    00000600
         DC    X'FF'               FILLER.                              00000700
         STM   R14,R12,12(R13)     STORE THE REGISTERS.                 00000800
         BALR  &BASE,0             ESTABLISH ADDRESSABILITY.            00000900
         USING *,&BASE             TELL ASSEMBLER ABOUT BASE REGISTER.  00001000
R0       EQU   0                   REGISTER 0.                          00001100
R1       EQU   1                   REGISTER 1.                          00001200
R2       EQU   2                   REGISTER 2.                          00001300
R3       EQU   3                   REGISTER 3.                          00001400
R4       EQU   4                   REGISTER 4.                          00001500
R5       EQU   5                   REGISTER 5.                          00001600
R6       EQU   6                   REGISTER 6.                          00001700
R7       EQU   7                   REGISTER 7.                          00001800
R8       EQU   8                   REGISTER 8.                          00001900
R9       EQU   9                   REGISTER 9.                          00002000
R10      EQU   10                  REGISTER 10.                         00002100
R11      EQU   11                  REGISTER 11.                         00002200
R12      EQU   12                  REGISTER 12.                         00002300
R13      EQU   13                  REGISTER 13.                         00002400
R14      EQU   14                  REGISTER 14.                         00002500
R15      EQU   15                  REGISTER 15.                         00002600
         LA    R15,&SAVE           ADDRESS OF OUR SAVE AREA.            00002700
         ST    R13,4(R15)          BACKWARD SAVE AREA CHAIN.            00002800
         ST    R15,8(R13)          FORWARD SAVE AREA CHAIN.             00002900
         LR    R13,R15             POINT R13 TO CURRENT SAVE AREA.      00003000
         B     &START              BRANCH TO ENTRY CODE.                00003100
&EOJ     CH    R15,$$RC            IS RETURN CODE HIGHER THAN &RC?      00003200
         BNH   *+6                 YES - LETS ZERO R15.                 00003300
         SR    R15,R15             ZERO OUT REGISTER 15.                00003400
         L     R13,&SAVE+4         POINT R13 TO PREVIOUS SAVE AREA.     00003500
         L     R14,12(R13)         RESTORE REGISTER 14.                 00003600
         LM    R0,R12,20(R13)      RESTORE THE REGISTERS.               00003700
         BR    R14                 RETURN TO OS.                        00003800
&SAVE    DS    18F                 OUR SAVE AREA.                       00003900
$$RC     DC    H'&RC'              RETURN CODE.                         00004000
$$START  DS    0H                  DEFAULT ENTRY POINT.                 00004100
         MEND                                                           00004200
*          DATA SET TSO036     AT LEVEL 003 AS OF 01/24/78
*          DATA SET TSO036     AT LEVEL 002 AS OF 01/16/78              00001
TSO036   TITLE 'SHOWASM - DISPLAY ASM PAGE/SWAP DSN STATUS     '        00002
*.....................................................................* 00003
*                                                                     * 00004
*   THIS IS THE SHOWASM COMMAND FOR TSO/STC USAGE                     * 00005
*                                                                     * 00006
*   THIS COMMAND WILL DISPLAY THE CURRENT AUX STORAGE MGR SLOT STATUS * 00007
*     AND STATUS FOR ALL CURRENT PAGE AND SWAP DATASETS ON THE SYSTEM * 00008
*                                                                     * 00009
*                                                                     * 00010
*  REQUIRES MACROS FROM SYS1.AMODGEN (CVT)                            * 00011
*  RUNS UNDER TSO (FOR OPER PRIVLEDGE USER) OR AS STC.                * 00012
*.....................................................................* 00013
         SPACE 3                                                        00014
*********************************************************************** 00015
*        ACCUMULATE HALF                                                00016
*********************************************************************** 00017
         MACRO                                                          00018
&L       ACH   &F                                                       00019
&L       LA    R1,1                INSERT 1                             00020
         AH    R1,&F               ADD ACCUMULATOR                      00021
         STH   R1,&F               SAVE ACCUMULATOR                     00022
         MEND                                                           00023
         SPACE 3                                                        00024
*********************************************************************** 00025
*        EDITING A 3 BYTES FIELD (VIA CEH)                              00026
*********************************************************************** 00027
         MACRO                                                          00028
&L       CEH3  &TO,&FROM                                                00029
&L       CEH   TRAN,&FROM          USE CONVERT.                  V1M0G  00030
         TM    TRAN,X'0F'          OVERFLOW ?                           00031
         BZ    *+14                NO                                   00032
         MVC   &TO,XAST9           YES, MOVE OFLOW ID                   00033
         B     *+10                BRANCH OVER                          00034
         MVC   &TO,TRAN+1          INSERT TO TAREGET                    00035
         MEND                                                           00036
         SPACE 3                                                        00037
*********************************************************************** 00038
*        CONVERT AND EDIT HALF                                          00039
*********************************************************************** 00040
         MACRO                                                          00041
&L       CEH    &TO,&FROM                                               00042
         LH    R0,&FROM            PICK UP VALUE                        00043
         LA    R1,&TO              PICK UP TARGET ADDRESS               00044
         BAL   LINKREG,CVTERZZ     GO CONVERT VALUE                     00045
         MEND                                                           00046
         SPACE 2                                                 V1M0C  00047
         GBLC  &HEADING                                                 00048
&HEADING SETC  'SHOWASM COMMAND - DISPLAY AUX STORAGE MANAGER - V1M2 '  00049
         TITLE '&HEADING - PROGRAM INITIALIZATION'                      00050
         SPACE                                                          00051
TSO036   PGM    ,   SHOWASM                                             00052
         EJECT                                                          00053
LINKREG  EQU   6                   LINK REGISTER                        00054
         EXTRACT MF=(E,EXTRACT)                                         00055
         L     R1,ANSWER            (R1)=ADDR OF TSO FLAG FROM EXTRACT  00056
         MVC   TSOFLAG,0(R1)        MOVE IN THE ANSWER TO OUR FLAG      00057
         TM    TSOFLAG,X'80'        ARE WE ONLINE                       00058
         BNO   INITIAL                NO..GO GO INITIALIZE              00059
         EXTRACT MF=(E,EXTRPSCB)      YES..CHECK FOR OPER PRIVLEDGE     00060
         L     R1,ANSWER                                                00061
         USING PSCB,R1                                                  00062
CHKOPER  TM    PSCBATR1,PSCBCTRL      DOES USER HAVE OPER PRIVLEDGE     00063
         BO    INITIAL                YES..GO ALLOW OPERATION           00064
         DROP  R1                                                       00065
         LA    R1,EMSG1                                                 00066
         BAL   LINKREG,PUTMSG                                           00067
         B     $$EOJ                                                    00068
         EJECT                                                          00069
*.....................................................................* 00070
*    GENERATE SLOT STATUS DISPLAY                                     * 00071
*.....................................................................* 00072
         SPACE 2                                                        00073
INITIAL  DS    0H                                                       00074
         LOAD  EP=DEVNAMET                                              00075
         ST    R0,DEVADDR                                               00076
         SPACE                                                          00077
         MVI   MESSAGE,C' '                    CLEAR OUT                00078
         MVC   MESSAGE+1(L'MESSAGE-1),MESSAGE     MESSAGE AREA          00079
         L     R9,CVTPTR          (R9)=ADDR CVT                         00080
         USING CVT,R9                                                   00081
         L     R9,CVTASMVT        (R9)= ADDR ASMVT                      00082
         DROP  R9                                                       00083
         USING ASMVT,R9                                                 00084
DOSLOTS  L     R4,ASMSLOTS        LOAD TOTAL SLOTS AVAIL                00085
         LA    R5,M1TOTSL         LOAD PLACE TO PUT IT                  00086
         BAL   LINKREG,EDITNUMB    GO EDIT THE NUMBER                   00087
*                                                                       00088
         L     R4,ASMBKSLT        LOAD UNRESERVED SLOTS AVAIL           00089
         LA    R5,M1UNRSV         LOAD PLACE TO PUT IT                  00090
         BAL   LINKREG,EDITNUMB    GO EDIT THE NUMBER                   00091
*                                                                       00092
         L     R4,ASMVSC          LOAD SLOTS ALLOC TO VIO USE           00093
         LA    R5,M1VIO           LOAD PLACE TO PUT IT                  00094
         BAL   LINKREG,EDITNUMB    GO EDIT THE NUMBER                   00095
*                                                                       00096
         L     R4,ASMNVSC         LOAD SLOTS ALLOC TO NON-VIO USE       00097
         LA    R5,M1NONVIO        LOAD PLACE TO PUT IT                  00098
         BAL   LINKREG,EDITNUMB    GO EDIT THE NUMBER                   00099
*                                                                       00100
         L     R4,ASMERRS         LOAD CNT OF BAD SLOTS ON LOCAL DS OP  00101
         LA    R5,M1BADSL         LOAD PLACE TO PUT IT                  00102
         BAL   LINKREG,EDITNUMB    GO EDIT THE NUMBER                   00103
*                                                                       00104
         LA    R1,HEADER1A       LOAD FIRST HEADER ADDR                 00105
         BAL   LINKREG,PUTMSG      GO PRINT IT                          00106
         LA    R1,HEADER1B       LOAD SECOND HEADER ADDR                00107
         BAL   LINKREG,PUTMSG      GO PRINT IT                          00108
         LA    R1,WTOMSG         LOAD MESSAGE ADDR                      00109
         BAL   LINKREG,PUTMSG      GO PRINT IT                          00110
         EJECT                                                          00111
*.....................................................................* 00112
*    GENERATE CORE USAGE STATUS DISPLAY                               * 00113
*.....................................................................* 00114
         SPACE 2                                                        00115
         L     R11,16             CVT                                   00116
         MVC   PVTADR,356(R11)    PVT                                   00117
         L     R1,16              LOAD CVT ADDRESS.              V2M0   00118
         L     R8,X'230'(,R1)     LOAD ADDRESS OF GLOBAL DATA.   V2M0   00119
         L     R8,8(,R8)          LOAD ADDRESS OF CSA PQE.       V2M0   00120
         L     R6,20(,R8)         LOAD SIZE OF CSA.              V2M0   00121
         A     R6,24(,R8)         ADD START OF CSA.              V2M0   00122
         ST    R6,PLPASTRT        SAVE ADDRESS OF PLPA START.    V2M0   00123
         SPACE                                                          00124
         MVI   MESSAGE,C' '      CLEAR OUT MESAGE                       00125
         MVC   MESSAGE+1(L'MESSAGE-1),MESSAGE                           00126
         SPACE                                                          00127
DOCORE   L     R1,PVTADR          RESTORE PVT PTR                       00128
         L     R5,12(,R1)         APPARENT ORIGIN OF PFT.               00129
         SR    R7,R7              PREPARE FOR ICM.               V1M0G  00130
         ICM   R7,B'0011',18(R1)  LAST PFTE OFFSET.              V1M0G  00131
         ST    R7,LASTPFTE        SAVE                           V1M0G  00132
         LH    R6,16(,R1)         OFFSET TO REAL ORIGIN OF PFT.         00133
LOOP     LR    R2,R5              ORG OF PFT                            00134
         AR    R2,R6              + DISP TO FIRST PFTE                  00135
         SPACE 2                                                        00136
*                                                                       00137
         TM    12(R2),X'04'       BAD FRAME ?                           00138
         BO    ADDBAD             YES                                   00139
         TM    13(R2),X'40'       OFFLINE FRAME                         00140
         BZ    NOSTL              NO                                    00141
ADDBAD   ACH   BADOFF                                                   00142
         B     BUMP               OUT                                   00143
*                                                                       00144
NOSTL    TM    12(R2),X'02'       ALLOCATED FOR V=R?                    00145
         BZ    NOVR               NO                                    00146
         ACH   NOVRR                                                    00147
NOVR     DS    0H                                                       00148
         TM    13(R2),X'10'       IRREGULAR BIT ?                       00149
         BZ    NOVIOP             NO                                    00150
         ACH   VIO                YES, SUM UP                           00151
NOVIOP   TM    14(R2),X'80'       SYSTEM OR LOCAL                       00152
         BZ    SYST               SYSTEM                                00153
         CLI   14(R2),X'80'       YES .. MEANS LOCAL                    00154
         BNE   LSQA               LOCAL SQA                             00155
         ACH   TIUS               TOTAL IN USE                          00156
*                                                                       00157
         TM    12(R2),X'10'       IS PAGE LONG FIXED?                   00158
         BZ    SHORT              NO                                    00159
         ACH   TLFX                                                     00160
         B     BUMP               OVER SHORT                            00161
*                                                                       00162
SHORT    TM    9(R2),X'FF'        IS THERE A FIX COUNT?                 00163
         BZ    UICPPP             NO                                    00164
         ACH   TSFX                                                     00165
LOWER    B     BUMP               OUT                                   00166
*                                                                       00167
UICPPP   DS    0H                                                       00168
         TM    12(R2),X'02'       V=R X                                 00169
         BO    BUMP               YES                                   00170
         XR    R11,R11            CLEAR                                 00171
         IC    R11,15(,R2)        GET UIC                               00172
         CLI   15(R2),X'FF'       SPECIAL?                              00173
         BE    LUIC               YES,DO  NOT COUNT                     00174
         CH    R11,HIUICP         NEW MAX ?                             00175
         BL    LUIC               NO                                    00176
         STH   R11,HIUICP         YES .. SAVE                           00177
LUIC     B     BUMP               OUT                                   00178
*                                                                       00179
LSQA     CLI   14(R2),X'84'       EQUAL MEANS LSQA                      00180
         BNE   BUMP               OUT                                   00181
         ACH   TLSQ                                                     00182
         B     BUMP               OUT                                   00183
*                                                                       00184
SYST     CLI   14(R2),X'0C'       EQUAL SQA?                            00185
         BNE   COMM               NO                                    00186
         ACH   SQA                                                      00187
         B     BUMP               OUT                                   00188
*                                                                       00189
COMM     CLI   14(R2),X'08'       EQUAL COMMON?                         00190
         BNE   AVAIL              NO                                    00191
*                                                                       00192
         ICM   R11,B'0011',2(R2)  LOAD VBN FROM PFTE.            V2M0   00193
         SLL   R11,8              SHIFT TO PROPER ADDRESS.       V2M0   00194
         CL    R11,PLPASTRT       COMPARE TO PLPA START ADDRESS. V2M0   00195
         BL    CSAPAGE            BRANCH IF CSA PAGE.            V2M0   00196
         ACH   PLPA                                                     00197
         B     COMMLBLA           BYPASS IF NOT CSA FRAME.       V2M0   00198
CSAPAGE  ACH   CSASIZE            COUNT AS CSA FRAME.            V2M0   00199
COMMLBLA TM    12(R2),X'10'       FIX ?                                 00200
         BO    YESFIX             YES                                   00201
         TM    9(R2),X'FF'        ANY FIX COUNT ?                       00202
         BZ    UICCCC             NO                                    00203
YESFIX   ACH   CFIX                                                     00204
         B     BUMP               OUT                                   00205
UICCCC   DS    0H                                                       00206
         XR    R11,R11            CLEAR                                 00207
         IC    R11,15(,R2)        GET UIC                               00208
         CLI   15(R2),X'FF'       SPECIAL?                              00209
         BE    LUICC              YES,DO NOT COUNT                      00210
         CH    R11,HIUICC         NEW MAX ?                             00211
         BL    LUICC              NO                                    00212
         STH   R11,HIUICC         YES .. SAVE                           00213
LUICC    B     BUMP                                                     00214
*                                                                       00215
AVAIL    ACH   AVQ                                                      00216
         SPACE 3                                                        00217
BUMP     LA    R6,16(,R6)         NEXT PFTE                             00218
         C     R6,LASTPFTE         REACHED END ?                 V1M0G  00219
         BNH   LOOP               NO .. LOOP                            00220
*                                                                       00221
*********************************************************************** 00222
*        PRINT ROUTINE FOR REPORT CORE USEAGE                           00223
*********************************************************************** 00224
*                                                                       00225
         PRINT NOGEN                                             V2M0   00226
CEF2     CEH   PSQA,SQA                                                 00227
         CEH   PLSQ,TLSQ                                                00228
         CEH   PPLPA,PLPA                                               00229
         CEH   PUSE,TIUS                                                00230
         CEH   PTLFX,TLFX                                               00231
         CEH   PTSFX,TSFX                                               00232
         CEH   PAVQ,AVQ                                                 00233
         CEH3  PVIO,VIO                                                 00234
         CEH3  BOFF,BADOFF                                              00235
         CEH   PCFIX,CFIX                                               00236
         CEH   PCSASZ,CSASIZE                                           00237
         CEH3  PVR,NOVRR                                                00238
         CEH3  HUICP,HIUICP                                             00239
         CEH3  HUICC,HIUICC                                             00240
*                                                                       00241
         PRINT GEN                                               V2M0   00242
*                                                                       00243
PRNTCORE LA    R1,HEADER2A       LOAD FIRST HEADER ADDR                 00244
         BAL   LINKREG,PUTMSG      GO PRINT IT                          00245
         LA    R1,HEADER2B       LOAD SECOND HEADER ADDR                00246
         BAL   LINKREG,PUTMSG      GO PRINT IT                          00247
         LA    R1,WTOMSG         LOAD MESSAGE ADDR                      00248
         BAL   LINKREG,PUTMSG      GO PRINT IT                          00249
         EJECT                                                          00250
*.....................................................................* 00251
*    GENERATE PAGE DATASET STATUS DISPLAY                             * 00252
*.....................................................................* 00253
         SPACE 2                                                        00254
DOPARTS  LA    R1,HEADER3A       LOAD FIRST HEADER ADDR                 00255
         BAL   LINKREG,PUTMSG      GO PRINT IT                          00256
         LA    R1,HEADER3B       LOAD SECOND HEADER ADDR                00257
         BAL   LINKREG,PUTMSG      GO PRINT IT                          00258
*                                                                       00259
         L     R8,ASMPART        LOAD ADDRESS OF PAGE ACTVTY REF TABLE  00260
         USING PART,R8            ESTABLISH ADDRESSABILITY              00261
*                                                                       00262
         L     R10,PARTEUSE      LOAD NUMBER OF PARTS TO PROCESS        00263
         LTR   R10,R10                                                  00264
         BNP   PROCSART          NOTHING TO DO..GO PROCESS SART         00265
         LA    R11,PARTENTS(R8)  LOAD ADDRESS OF FIRST PART ENTRY       00266
         USING PARTENT,R11       GET ADDRESSABILITY                     00267
*                                                                       00268
PARTLOOP MVI   MESSAGE,C' '      CLEAR OUT MESAGE                       00269
         MVC   MESSAGE+1(L'MESSAGE-1),MESSAGE                           00270
*                                                                       00271
         TM    PAREFLG1,PAREDSBD  IS DATASET BAD?                       00272
         BZ    PCKINUSE           NO-> CHECK FOR NOT IN USE             00273
         MVC   M2USAGE,=CL6'* BAD*'                                     00274
         B     PDOUCB                                                   00275
PCKINUSE TM    PAREFLG1,PARENUSE                                        00276
         BO    BUMPPART          NOBODY HOME HERE                       00277
*                                                                       00278
PSTATUS  TM    PARETYPE,PAREPLPA    IS THIS PLPA?                       00279
         BZ    PCHK2                                                    00280
         MVC   M2USAGE,=CL6'PLPA'                                       00281
         B     PDOUCB                                                   00282
PCHK2    TM    PARETYPE,PARECOMM    IS IT COMMON ?                      00283
         BZ    PCHK3                                                    00284
         MVC   M2USAGE,=CL6'COMMON'                                     00285
         B     PDOUCB                                                   00286
PCHK3    TM    PARETYPE,PAREDPLX    IS THIS A DUPLEX DATASET            00287
         BZ    PCHK4                                                    00288
         MVC   M2USAGE,=CL6'DUPLEX'                                     00289
         B     PDOUCB                                                   00290
PCHK4    TM    PARETYPE,PARELOCL    IS THIS A LOCAL DATASET             00291
         BZ    PDOUCB                                                   00292
         MVC   M2USAGE,=CL6'LOCAL'                                      00293
         SPACE 2                                                        00294
PDOUCB   L     R7,PAREUCBP            GET UCB ADDRESS FOR THIS DSN      00295
         USING UCBSECT,R7                                               00296
*                                                                       00297
         MVC   M2VOLUME,UCBVOLI     MOVE IN VOLSER                      00298
         UNPK  M2UNIT(5),UCBCHAN(3)    CONVERT CURRENT CUU              00299
         NC    M2UNIT,HEXMASK            TO NICE PRINTABLE HEX          00300
         TR    M2UNIT,HEXTAB             FORMAT FOR                     00301
         MVI   M2UNIT,C' '               FOR A GREAT                    00302
         MVI   M2UNIT+4,C' '             DISPLAY                        00303
*                                                                       00304
         L     R1,DEVADDR           LOAD DEVICE NAME TABLE ADDRESS      00305
         L     R2,0(R1)             LOAD NUMBER OF ENTRIES              00306
         LA    R1,4(R1)               ADDRESS OF FIRST ENTRY            00307
PDEVLOP  CLC   UCBTYP,8(R1)         COMPARE                             00308
         BE    PDEVFND               GOTIT...                           00309
         LA    R1,12(R1)             NOGOT..BUMP                        00310
         BCT   R2,PDEVLOP              AND GRIND                        00311
         MVC   M2TYPE,=CL8'????????'  NO GOT                            00312
         B     PDODSN                                                   00313
         DROP  R7                                                       00314
PDEVFND  MVC   M2TYPE,0(R1)         THIS IS IT                          00315
PDODSN   DS    0H                                                       00316
         LH    R5,PARENN            LOAD PARENT NUMBER                  00317
         MH    R5,=H'44'            MULTIPLY BY 44                      00318
         L     R4,PARTDSNL          LOAD START ADDRESS OF DSN LIST      00319
         AR    R4,R5                ADD THE TWO                         00320
         MVC   M2DSNAME,0(R4)       GET THE DSNAME                      00321
*                                                                       00322
PDOSLOTS L     R4,PARESZSL        LOAD TOTAL SLOTS IN DATASET           00323
         LA    R5,M2TOTAL         LOAD PLACE TO PUT IT                  00324
         BAL   LINKREG,EDITNUMB    GO EDIT THE NUMBER                   00325
*                                                                       00326
         L     R4,PARESLTA        LOAD SLOTS AVAIL FOR ALLOC            00327
         LA    R5,M2AVAIL         LOAD PLACE TO PUT IT                  00328
         BAL   LINKREG,EDITNUMB    GO EDIT THE NUMBER                   00329
*                                                                       00330
         L     R4,PARERRCT        LOAD NUMBER OF BAD SLOTS              00331
         LA    R5,M2BAD           LOAD PLACE TO PUT IT                  00332
         BAL   LINKREG,EDITNUMB    GO EDIT THE NUMBER                   00333
         SPACE                                                          00334
         LA    R1,WTOMSG          LOAD MESSAGE ADDRESS                  00335
         BAL   LINKREG,PUTMSG       GO PUT MESSAGE                      00336
*                                                                       00337
BUMPPART LA    R11,64(R11)        BUMP PARTENT POINTER                  00338
         BCT   R10,PARTLOOP       AND RETURN FOR MORE IF NEED BE        00339
*                                                                       00340
*                                                                       00341
         DROP  R8                                                       00342
         DROP  R11                                                      00343
         EJECT                                                          00344
*.....................................................................* 00345
*    GENERATE SWAP DATASET STATUS DISPLAY                             * 00346
*.....................................................................* 00347
         SPACE 2                                                        00348
PROCSART DS    0H                                                       00349
*                                                                       00350
         L     R8,ASMSART        LOAD ADDRESS OF SWAP ACTVTY REF TABLE  00351
         USING SART,R8            ESTABLISH ADDRESSABILITY              00352
*                                                                       00353
         L     R10,SARUSE        LOAD NUMBER OF SARTS TO PROCESS        00354
         LTR   R10,R10                                                  00355
         BNP   DOFINMSG          NOTHING TO DO..GO FINISH               00356
         LA    R11,SARENTS(R8)   LOAD ADDRESS OF FIRST SART ENTRY       00357
         USING SARENT,R11        GET ADDRESSABILITY                     00358
*                                                                       00359
SARTLOOP MVI   MESSAGE,C' '      CLEAR OUT MESAGE                       00360
         MVC   MESSAGE+1(L'MESSAGE-1),MESSAGE                           00361
*                                                                       00362
         TM    SREFLG,SREDSBD    IS DATASET BAD?                        00363
         BZ    SCKINUSE           NO-> CHECK FOR NOT IN USE             00364
         MVC   M2USAGE,=CL6'SW-BAD'                                     00365
         B     SDOUCB                                                   00366
SCKINUSE TM    SREFLG,SRENUSE                                           00367
         BO    BUMPSART          NOBODY HOME HERE                       00368
*                                                                       00369
         MVC   M2USAGE,=CL6'SWAP'                                       00370
         SPACE 2                                                        00371
SDOUCB   L     R7,SREUCB              GET UCB ADDRESS FOR THIS DSN      00372
         USING UCBSECT,R7                                               00373
*                                                                       00374
         MVC   M2VOLUME,UCBVOLI     MOVE IN VOLSER                      00375
         UNPK  M2UNIT(5),UCBCHAN(3)    CONVERT CURRENT CUU              00376
         NC    M2UNIT,HEXMASK            TO NICE PRINTABLE HEX          00377
         TR    M2UNIT,HEXTAB             FORMAT FOR                     00378
         MVI   M2UNIT,C' '               FOR A GREAT                    00379
         MVI   M2UNIT+4,C' '             DISPLAY                        00380
*                                                                       00381
         L     R1,DEVADDR           LOAD DEVICE NAME TABLE ADDRESS      00382
         L     R2,0(R1)             LOAD NUMBER OF ENTRIES              00383
         LA    R1,4(R1)               ADDRESS OF FIRST ENTRY            00384
SDEVLOP  CLC   UCBTYP,8(R1)         COMPARE                             00385
         BE    SDEVFND               GOTIT...                           00386
         LA    R1,12(R1)             NOGOT..BUMP                        00387
         BCT   R2,SDEVLOP              AND GRIND                        00388
         MVC   M2TYPE,=CL8'????????'  NO GOT                            00389
         B     SDODSN                                                   00390
         DROP  R7                                                       00391
SDEVFND  MVC   M2TYPE,0(R1)         THIS IS IT                          00392
SDODSN   DS    0H                                                       00393
         LH    R5,SRENN             LOAD SARENT NUMBER                  00394
         MH    R5,=H'44'            MULTIPLY BY 44                      00395
         L     R4,SARDSNL           LOAD START ADDRESS OF DSN LIST      00396
         AR    R4,R5                ADD THE TWO                         00397
         MVC   M2DSNAME,0(R4)       GET THE DSNAME                      00398
*                                                                       00399
SDOSLOTS L     R4,SRETOTSL        LOAD TOTAL SLOTS IN DATASET           00400
         LA    R5,M2TOTAL         LOAD PLACE TO PUT IT                  00401
         BAL   LINKREG,EDITNUMB    GO EDIT THE NUMBER                   00402
*                                                                       00403
         L     R4,SREAVLSL        LOAD SLOTS AVAIL FOR ALLOC            00404
         LA    R5,M2AVAIL         LOAD PLACE TO PUT IT                  00405
         BAL   LINKREG,EDITNUMB    GO EDIT THE NUMBER                   00406
*                                                                       00407
         L     R4,SRERRCNT        LOAD NUMBER OF BAD SLOTS              00408
         LA    R5,M2BAD           LOAD PLACE TO PUT IT                  00409
         BAL   LINKREG,EDITNUMB    GO EDIT THE NUMBER                   00410
         SPACE                                                          00411
         LA    R1,WTOMSG          LOAD MESSAGE ADDRESS                  00412
         BAL   LINKREG,PUTMSG       GO PUT MESSAGE                      00413
*                                                                       00414
BUMPSART LA    R11,48(R11)        BUMP SARTENT POINTER                  00415
         BCT   R10,SARTLOOP       AND RETURN FOR MORE IF NEED BE        00416
*                                                                       00417
         DROP  R8                                                       00418
         DROP  R11                                                      00419
DOFINMSG LA    R1,FINMSG                                                00420
         BAL   LINKREG,PUTMSG                                           00421
         B     $$EOJ                                                    00422
         EJECT                                                          00423
*.....................................................................* 00424
*        LOCAL SUBROUTINE TO CONVERT NUMBER TO PRINTABLE              * 00425
*                                                                     * 00426
*  ENTER WITH (R4) NUMBER TO CONVERT                                  * 00427
*             (R5) = ADDR OF TARGET 6 CHAR FIELD TO RECEIVE NUMBER    * 00428
*  BAL   LINKREG,EDITNUM                                              * 00429
*                                                                     * 00430
*                                                                     * 00431
*.....................................................................* 00432
         SPACE                                                          00433
EDITNUMB CVD   R4,DBLW           CONVERT NUMBER TO PACKED               00434
         MVC   0(6,R5),EDITMASK  MOVE IN THE MASK                       00435
         ED    0(6,R5),DBLW+5    EDIT THE VALUE                         00436
         BR    LINKREG           RETURN                                 00437
         SPACE                                                          00438
*.....................................................................* 00439
*        CONVERT TO PRINTABLE..USED BY CEH CEH3 MACROS                * 00440
*.....................................................................* 00441
         SPACE                                                          00442
CVTERZZ  DS    0H                                                       00443
         C     R0,=F'9999'          IS IT TOO BIG                       00444
         BH    CVTERZZ1                                                 00445
         CVD   R0,DBLW                                                  00446
         OI    DBLW+7,X'0F'                                             00447
         MVC   TRAN1,=X'402020202120'                                   00448
         ED    TRAN1,DBLW+5                                             00449
         MVC   0(4,R1),TRAN1+2                                          00450
         BR    LINKREG                                                  00451
CVTERZZ1 MVC   0(4,R1),XAST9                                            00452
         BR    LINKREG                                                  00453
         EJECT                                                          00454
*.....................................................................* 00455
*        LOCAL SUBROUTINE FOR IO TO OPER/USER                         * 00456
*.....................................................................* 00457
         SPACE                                                          00458
*                                                                       00459
* ENTER WITH WTO,WTOR LIST FORM POINTED TO BY R1                        00460
*                                                                       00461
*    BAL  LINKREG,PUTMSG    OR                                          00462
*    BAL  LINKREG,PUTGET                                                00463
*                                                                       00464
*  USES RXX FOR WORK                                                    00465
*                                                                       00466
RXX      EQU   14                                                       00467
*                                                                       00468
PUTMSG   DS    0H                                                       00469
         TM    TSOFLAG,X'80'       IS IT TSO?                           00470
         BO    TPUTIT              YES-> GO DO TPUT                     00471
         SVC   35                  NO-> USE WTO                         00472
         BR    LINKREG                                                  00473
TPUTIT   DS    0H                                                       00474
         LH    R0,0(,R1)           GET LENGTH OF WTO MESSAGE            00475
         S     R0,=F'4'            SUBTRACT OFF HEADER                  00476
         LA    R1,4(,R1)           BUMP MSG ADDRESS PAST HDR            00477
         TPUT  (1),(0),R           DO THE TPUT                          00478
         BR    LINKREG                                                  00479
         SPACE 2                                                        00480
PUTGET   DS    0H                                                       00481
         TM    TSOFLAG,X'80'       IS IT TSO?                           00482
         BO    TSOIT               YES-> GO USE TGET                    00483
         LR    RXX,R1              NO-> USE WTOR .. SAVE MSG ADDRESS    00484
         SVC   35                  DO THE WTOR...                       00485
         L     R1,4(,RXX)          GET ADDR OF ECB                      00486
         XC    0(4,R1),0(R1)       CLEAN OUT THE ECB....                00487
         LA    R0,1                                                     00488
         SVC   1                   WAIT FOR WTOR TO COMPLETE            00489
         B     PUTRET              RETURN                               00490
TSOIT    DS    0H                                                       00491
         LR    RXX,R1              SAVE                                 00492
         LH    R0,8(,R1)           PUT LENGTH                           00493
         S     R0,=F'4'                                                 00494
         LA    R1,12(,R1)          PAST HDR                             00495
         TPUT  (1),(0),R                                                00496
         SR    R0,R0                                                    00497
         IC    R0,0(,RXX)          GET REPLY LENGTH                     00498
         L     R1,0(,RXX)          GET REPLY ADRS                       00499
         LA    R1,0(,R1)                                                00500
         ICM   R1,B'1000',=X'80'   INDICATE TGET...                     00501
         TGET  (1),(0),R                                                00502
PUTRET   DS    0H                                                       00503
         L     R1,0(,RXX)          GET REPLY ADRS                       00504
         XR    R0,R0                                                    00505
         IC    R0,0(,RXX)          GET REPLY LENGTH                     00506
UPPER    DS    0H                                                       00507
         OI    0(R1),C' '          UPPER CASE                           00508
         LA    R1,1(,R1)           NXT                                  00509
         BCT   R0,UPPER                                                 00510
         BR    LINKREG                                                  00511
         EJECT                                                          00512
*.....................................................................* 00513
*        MESSAGES - OPER QUERIES AND RESPONSES FOR GOOD WORK          * 00514
*.....................................................................* 00515
         SPACE                                                          00516
HEADER1A WTO   'SHOWASM - CURRENT SLOT STATUS',                        X00517
               ROUTCDE=(11),MF=L                                        00518
         SPACE 1                                                        00519
HEADER1B WTO   ' TOTAL UNRSV .VIO. N-VIO .BAD.',                       X00520
               ROUTCDE=(11),MF=L                                        00521
         SPACE 1                                                        00522
HEADER2A WTO   'SHOWASM - STORAGE USAGE STATUS',                       X00523
               ROUTCDE=(11),MF=L                                        00524
         SPACE 1                                                        00525
HEADER2B WTO   '  SQA LSQA PLPA  CSA CFIX INUS LLFX LSFX AVAL VIO BOF VX00526
               =R  UICC UICP ',ROUTCDE=(11),MF=L                        00527
         EJECT                                                          00528
HEADER3A WTO   'SHOWASM - PAGE/SWAP DATASET STATUS',                   X00529
               ROUTCDE=(11),MF=L                                        00530
         SPACE 1                                                        00531
HEADER3B WTO   ' USAGE  VOLUME UNIT DEV-TYPE TOTAL AVAIL .BAD.  DSNAME X00532
                ',ROUTCDE=(11),MF=L                                     00533
         SPACE 1                                                        00534
FINMSG   WTO   'SHOWASM - DISPLAY ENDED',                              X00535
               ROUTCDE=(11),MF=L                                        00536
         EJECT                                                          00537
         SPACE 1                                                        00538
WTOMSG   WTO   '                                                       X00539
                                        ',                             X00540
               ROUTCDE=(11),MF=L                                        00541
         SPACE 2                                                        00542
         ORG   WTOMSG+4                                                 00543
MESSAGE  DS    CL80                                                     00544
         ORG   WTOMSG+4                                                 00545
M1TOTSL  DS    CL6                                                      00546
M1UNRSV  DS    CL6                                                      00547
M1VIO    DS    CL6                                                      00548
M1NONVIO DS    CL6                                                      00549
M1BADSL  DS    CL6                                                      00550
         ORG   WTOMSG+5                                                 00551
M2USAGE  DS    CL6                                                      00552
         DS    CL1                                                      00553
M2VOLUME DS    CL6                                                      00554
         DS    CL1                                                      00555
M2UNIT   DS    CL4                                                      00556
         DS    CL1                                                      00557
M2TYPE   DS    CL8                                                      00558
M2TOTAL  DS    CL6                                                      00559
M2AVAIL  DS    CL6                                                      00560
M2BAD    DS    CL6                                                      00561
         DS    CL2                                                      00562
M2DSNAME DS    CL32                                                     00563
         ORG   WTOMSG+5                                                 00564
PSQA     DS    CL4                                                      00565
         DS    CL1                                                      00566
PLSQ     DS    CL4                                                      00567
         DS    CL1                                                      00568
PPLPA    DS    CL4                                                      00569
         DS    CL1                                                      00570
PCSASZ   DS    CL4                                                      00571
         DS    CL1                                                      00572
PCFIX    DS    CL4                                                      00573
         DS    CL1                                                      00574
PUSE     DS    CL4                                                      00575
         DS    CL1                                                      00576
PTLFX    DS    CL4                                                      00577
         DS    CL1                                                      00578
PTSFX    DS    CL4                                                      00579
         DS    CL1                                                      00580
PAVQ     DS    CL4                                                      00581
         DS    CL1                                                      00582
PVIO     DS    CL3                                                      00583
         DS    CL1                                                      00584
BOFF     DS    CL3                                                      00585
         DS    CL1                                                      00586
PVR      DS    CL3                                                      00587
         DS    CL2                                                      00588
HUICC    DS    CL3                                                      00589
         DS    CL2                                                      00590
HUICP    DS    CL3                                                      00591
*                                                                       00592
         ORG                                                            00593
         EJECT                                                          00594
*.....................................................................* 00595
*        MESSAGES - BOZO RESPONSES                                    * 00596
*.....................................................................* 00597
         SPACE                                                          00598
EMSG1    WTO   'SORRY...SHOWASM IS ONLY AVAILABLE TO OPER PRIV USERS', X00599
               ROUTCDE=(11),MF=L                                        00600
         SPACE                                                          00601
         EJECT                                                          00602
*.....................................................................* 00603
*        VARIABLES                                                    * 00604
*.....................................................................* 00605
         SPACE                                                          00606
TSOFLAG  DC    X'00'               ='80' IF TSO. '00' IF NOT TSO        00607
ANSWER   DC    F'0'                ADDRESS POINTER FOR EXTRACT          00608
DEVADDR  DC    AL4(0)              ADDRESS OF DEVNAMET                  00609
DBLW     DC    D'0'                                                     00610
TRAN     DC    F'0'                                                     00611
TRAN1    DC    CL6' '                                                   00612
*                                                                       00613
*                                                                       00614
LASTPFTE DS    F                                                        00615
PVTADR   DS    F                                                        00616
PLPASTRT DS    F                                                V2M0    00617
*                                                                       00618
*                                                                       00619
CTS      DS    0F               ACCUMULATORS FOR PAGE COUNTS            00620
SQA      DC    H'0'                                                     00621
TLSQ     DC    H'0'                                                     00622
PLPA     DC    H'0'                                                     00623
AVQ      DC    H'0'                                                     00624
TLFX     DC    H'0'                                                     00625
TSFX     DC    H'0'                                                     00626
TIUS     DC    H'0'                                                     00627
BADOFF   DC    H'0'                                                     00628
VIO      DC    H'0'                                                     00629
NOVRR    DC    H'0'                                                     00630
CSASIZE  DC    H'0'                                                     00631
HIUICP   DC    H'0'                                                     00632
HIUICC   DC    H'0'                                                     00633
CFIX     DC    H'0'                                                     00634
         EJECT                                                          00635
*.....................................................................* 00636
*        CONSTANTS                                                    * 00637
*.....................................................................* 00638
         SPACE 2                                                        00639
EDITMASK DC    X'402020202120'                                          00640
HEXMASK  DC    4X'0F'                                                   00641
HEXTAB   DC    C'0123456789ABCDEF'                                      00642
XAST9    DC    C' *****'                                                00643
         EJECT                                                          00644
*.....................................................................* 00645
*        LIST FORMS OF MACROS                                         * 00646
*.....................................................................* 00647
         SPACE 2                                                        00648
EXTRACT  EXTRACT ANSWER,'S',FIELDS=(TSO),MF=L                           00649
         SPACE 2                                                        00650
EXTRPSCB EXTRACT ANSWER,'S',FIELDS=(PSB),MF=L                           00651
         EJECT                                                          00652
         IKJPSCB                                                        00653
         EJECT                                                          00654
UCBSECT  DSECT                                                          00655
         IEFUCBOB                                                       00656
         EJECT                                                          00657
CVT      DSECT                                                          00658
         CVT   SYS=VMS,TSO=YES                                          00659
*                                                                       00660
         EJECT                                                          00661
ASMVT    DSECT                                                          00662
         ORG   ASMVT+4                                                  00663
ASMSART  DS    A                ADDR OF SWAP ACTIVITY REF TABLE         00664
ASMPART  DS    A                ADDR OF PAGE ACTIVITY REF TABLE         00665
         ORG   ASMVT+X'6C'                                              00666
ASMBKSLT DS    F                COUNT OF UNRESVD LOCAL SLOTS            00667
ASMSLOTS DS    F                COUNT OF TOTAL LOCAL SLOTS IN ALL       00668
*                                 OPEN LOCAL PAGE DATASETS              00669
ASMVSC   DS    F                COUNT OF TOTAL SLOTS ALLOC TO VIO USE   00670
ASMNVSC  DS    F                COUNT OF TOTAL SLOTS ALLOC TO NON-VIO   00671
ASMERRS  DS    F                COUNT OF BAD SLOTS ON LOCAL DSNS        00672
         SPACE                                                          00673
PART     DSECT              PAGE ACTIVITY REFERENCE TABLE               00674
PARTHDR  DS    XL80                                                     00675
         ORG   PART+4                                                   00676
PARTSIZE DS    F            NUMBER OF PARTENTRIES IN THIS PART          00677
PARTEUSE DS    F            NUMBER OF PARTE IN USE                      00678
         ORG   PART+24                                                  00679
PARTDSNL DS    F            ADDRESS OF DSNLIST FOR THE PARTE            00680
         ORG   PART+80                                                  00681
PARTENTS EQU   *-PART       START OF PART ENTRIES                       00682
         SPACE 2                                                        00683
PARTENT  DSECT              PART ENTRY                                  00684
         ORG   PARTENT+8                                                00685
PARETYPE DS    XL1         PAGE DATASET TYPE                            00686
PAREPLPA EQU   B'10000000'  PLPA                                        00687
PARECOMM EQU   B'01000000'  COMMON                                      00688
PAREDPLX EQU   B'00100000'  DUPLEX                                      00689
PARELOCL EQU   B'00010000'  LOCAL                                       00690
*                                                                       00691
PAREFLG1 DS    XL1          PARTE FLAGS                                 00692
PARENUSE EQU   B'10000000'   PARE NOT IN USE 1=NOT IN USE               00693
PAREDSBD EQU   B'01000000'   DATASET BAD 1=YES 0=NO                     00694
         ORG   PARTENT+10                                               00695
PARENN   DS    H            PART NUMBER FOR THIS PARTE                  00696
         ORG   PARTENT+16                                               00697
PARESZSL DS    F            SIZE OF PAGE DATASET IN SLOTS               00698
PARESLTA DS    F            NUMBER OF SLOTS AVAILABLE                   00699
PARERRCT DS    F            COUNT OF NUMBER OF BAD SLOTS                00700
         ORG   PARTENT+44                                               00701
PAREUCBP DS    F            POINTER TO UCB FOR PAGE DATASET             00702
         EJECT                                                          00703
SART     DSECT              SWAP ACTIVITY REFERENCE TABLE               00704
SARTHDR  DS    XL80                                                     00705
         ORG   SART+8                                                   00706
SARUSE   DS    F            NUMBER OF SARTE IN USE                      00707
         ORG   SART+24                                                  00708
SARDSNL  DS    F            ADDRESS OF DSNLIST FOR THE SARTE            00709
         ORG   SART+80                                                  00710
SARENTS  EQU   *-SART       START OF SART ENTRIES                       00711
         SPACE 2                                                        00712
SARENT   DSECT              SART ENTRY                                  00713
*                                                                       00714
         ORG   SARENT+9                                                 00715
SREFLG   DS    XL1          SARTE FLAGS                                 00716
SRENUSE  EQU   B'10000000'   SARTE NOT IN USE 1=NOT IN USE              00717
SREDSBD  EQU   B'01000000'   DATASET BAD 1=YES 0=NO                     00718
         ORG   SARENT+10                                                00719
SRENN    DS    H            SARE NUMBER FOR THIS PARTE                  00720
         ORG   SARENT+16                                                00721
SRETOTSL DS    F            TOTAL NUMBER OF SWAP SETS ON THIS DSN       00722
SREAVLSL DS    F            COUNT OF SWAP SETS AVAILABLE                00723
SRERRCNT DS    F            COUNT OF NUMBER OF ERROR SWAP SETS          00724
         ORG   SARENT+44                                                00725
SREUCB   DS    F            POINTER TO UCB FOR SWAP DATASET             00726
         END   TSO036                                                   00727
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR    <== TARGET LOAD LIBRARY   00110000
//LKED.SYSIN DD *                                                       00120000
  NAME SHOWASM(R)                                                       00130000
//HELP    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)                        00140000
//SYSPRINT DD  SYSOUT=*                                                 00150000
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP         <== TARGET HELP LIBRARY   00160000
//SYSIN    DD  *                                                        00170000
./ ADD NAME=SHOWASM                                                     00180000
)F FUNCTION -                                                           00190000
   THE SHOWASM COMMAND DISPLAYS THE CURRENT AUXILIARY STORAGE           00200000
   MANAGER SLOT STATUS AND THE CURRENT PAGE AND SWAP DATASET STATUS.    00210000
)X SYNTAX -                                                             00220000
         SHOWASM                                                        00230000
)O OPERANDS -                                                           00240000
  THERE ARE NO OPERANDS ON THE SHOWASM COMMAND                          00250000
./ ENDUP                                                                00260000
//PROC    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)                        00270000
//SYSPRINT DD  SYSOUT=*                                                 00280000
//SYSUT2   DD  DISP=SHR,DSN=SYS2.PROCLIB      <== TARGET PROC LIBRARY   00290000
//SYSIN    DD  DATA                                                     00300000
./ ADD NAME=SHOWASM                                                     00310000
//*-------------------------------------------------------------------* 00320000
//*   DISPLAY AUX STORAGE MANAGER SLOTS + PAGE/SWAP DATASET STATUS    * 00330000
//*-------------------------------------------------------------------* 00340000
//SHOWASM  PROC                                                         00350000
//SHOWASM  EXEC PGM=SHOWASM                                             00360000
//STEPLIB   DD  DSN=SYS2.CMDLIB,DISP=SHR                                00370000
./ ENDUP                                                                00380000
//                                                                      00390000
//                                                                      00400000
