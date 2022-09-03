//MNEMAC  JOB (TSO),
//             'Install MNEMAC',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//IEBUPDTE EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW                      00030000
//SYSPRINT DD  SYSOUT=*                                                 00040000
//SYSUT2   DD  DSN=SYS1.MNEMAC,DISP=(,CATLG,DELETE),                    00050000
//             UNIT=SYSDA,VOL=SER=PUB001,           <== TARGET          00060000
//             SPACE=(TRK,(60,30,89),RLSE),                             00070000
//             DCB=(SYS1.MACLIB)                                        00080000
//SYSIN    DD  *                                                        00090000
./ ADD NAME=ADB      0100-02254-02254-0900-00006-00006-00000-JJAEGER    00100000
         MACRO                                                          00110000
&LABEL   ADB   &R1,&S2                                                  00120000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    00130000
         DS    0H                                                       00140000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'001A'              00150000
         MEND                                                           00160000
./ ADD NAME=ADBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    00170000
         MACRO                                                          00180000
&LABEL   ADBR  &R1,&R2                                                  00190000
         DS    0H                                                       00200000
&LABEL.  DC    0XL4'00',X'B31A00',AL.4(&R1.,&R2.)                       00210000
         MEND                                                           00220000
./ ADD NAME=AEB      0100-02254-02254-0900-00006-00006-00000-JJAEGER    00230000
         MACRO                                                          00240000
&LABEL   AEB   &R1,&S2                                                  00250000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    00260000
         DS    0H                                                       00270000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'000A'              00280000
         MEND                                                           00290000
./ ADD NAME=AEBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    00300000
         MACRO                                                          00310000
&LABEL   AEBR  &R1,&R2                                                  00320000
         DS    0H                                                       00330000
&LABEL.  DC    0XL4'00',X'B30A00',AL.4(&R1.,&R2.)                       00340000
         MEND                                                           00350000
./ ADD NAME=AG       0100-02254-02254-0900-00006-00006-00000-JJAEGER    00360000
         MACRO                                                          00370000
&LABEL   AG    &R1,&S2                                                  00380000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    00390000
         DS    0H                                                       00400000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0008'              00410000
         MEND                                                           00420000
./ ADD NAME=AGF      0100-02254-02254-0900-00006-00006-00000-JJAEGER    00430000
         MACRO                                                          00440000
&LABEL   AGF   &R1,&S2                                                  00450000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    00460000
         DS    0H                                                       00470000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0018'              00480000
         MEND                                                           00490000
./ ADD NAME=AGFR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    00500000
         MACRO                                                          00510000
&LABEL   AGFR  &R1,&R2                                                  00520000
         DS    0H                                                       00530000
&LABEL.  DC    0XL4'00',X'B91800',AL.4(&R1.,&R2.)                       00540000
         MEND                                                           00550000
./ ADD NAME=AGHI     0100-02254-02254-0900-00005-00005-00000-JJAEGER    00560000
         MACRO                                                          00570000
&LABEL   AGHI  &R1,&I2                                                  00580000
         DS    0H                                                       00590000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'0B'),Y(&I2.)                  00600000
         MEND                                                           00610000
./ ADD NAME=AGR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    00620000
         MACRO                                                          00630000
&LABEL   AGR   &R1,&R2                                                  00640000
         DS    0H                                                       00650000
&LABEL.  DC    0XL4'00',X'B90800',AL.4(&R1.,&R2.)                       00660000
         MEND                                                           00670000
./ ADD NAME=AHI      0100-02254-02254-0900-00005-00005-00000-JJAEGER    00680000
         MACRO                                                          00690000
&LABEL   AHI   &R1,&I2                                                  00700000
         DS    0H                                                       00710000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'0A'),Y(&I2.)                  00720000
         MEND                                                           00730000
./ ADD NAME=ALC      0100-02254-02254-0900-00006-00006-00000-JJAEGER    00740000
         MACRO                                                          00750000
&LABEL   ALC   &R1,&S2                                                  00760000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    00770000
         DS    0H                                                       00780000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0098'              00790000
         MEND                                                           00800000
./ ADD NAME=ALCG     0100-02254-02254-0900-00006-00006-00000-JJAEGER    00810000
         MACRO                                                          00820000
&LABEL   ALCG  &R1,&S2                                                  00830000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    00840000
         DS    0H                                                       00850000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0088'              00860000
         MEND                                                           00870000
./ ADD NAME=ALCGR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    00880000
         MACRO                                                          00890000
&LABEL   ALCGR &R1,&R2                                                  00900000
         DS    0H                                                       00910000
&LABEL.  DC    0XL4'00',X'B98800',AL.4(&R1.,&R2.)                       00920000
         MEND                                                           00930000
./ ADD NAME=ALCR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    00940000
         MACRO                                                          00950000
&LABEL   ALCR  &R1,&R2                                                  00960000
         DS    0H                                                       00970000
&LABEL.  DC    0XL4'00',X'B99800',AL.4(&R1.,&R2.)                       00980000
         MEND                                                           00990000
./ ADD NAME=ALG      0100-02254-02254-0900-00006-00006-00000-JJAEGER    01000000
         MACRO                                                          01010000
&LABEL   ALG   &R1,&S2                                                  01020000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    01030000
         DS    0H                                                       01040000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'000A'              01050000
         MEND                                                           01060000
./ ADD NAME=ALGF     0100-02254-02254-0900-00006-00006-00000-JJAEGER    01070000
         MACRO                                                          01080000
&LABEL   ALGF  &R1,&S2                                                  01090000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    01100000
         DS    0H                                                       01110000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'001A'              01120000
         MEND                                                           01130000
./ ADD NAME=ALGFR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    01140000
         MACRO                                                          01150000
&LABEL   ALGFR &R1,&R2                                                  01160000
         DS    0H                                                       01170000
&LABEL.  DC    0XL4'00',X'B91A00',AL.4(&R1.,&R2.)                       01180000
         MEND                                                           01190000
./ ADD NAME=ALGR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    01200000
         MACRO                                                          01210000
&LABEL   ALGR  &R1,&R2                                                  01220000
         DS    0H                                                       01230000
&LABEL.  DC    0XL4'00',X'B90A00',AL.4(&R1.,&R2.)                       01240000
         MEND                                                           01250000
./ ADD NAME=AXBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    01260000
         MACRO                                                          01270000
&LABEL   AXBR  &R1,&R2                                                  01280000
         DS    0H                                                       01290000
&LABEL.  DC    0XL4'00',X'B34A00',AL.4(&R1.,&R2.)                       01300000
         MEND                                                           01310000
./ ADD NAME=BAKR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    01320000
         MACRO                                                          01330000
&LABEL   BAKR  &R1,&R2                                                  01340000
         DS    0H                                                       01350000
&LABEL.  DC    0XL4'00',X'B24000',AL.4(&R1.,&R2.)                       01360000
         MEND                                                           01370000
./ ADD NAME=BASSM    0100-02254-02254-0900-00005-00005-00000-JJAEGER    01380000
         MACRO                                                          01390000
&LABEL   BASSM &R1,&R2                                                  01400000
         DS    0H                                                       01410000
&LABEL.  DC    0XL2'00',X'0C',AL.4(&R1.,&R2.)                           01420000
         MEND                                                           01430000
./ ADD NAME=BCTG     0100-02254-02254-0900-00006-00006-00000-JJAEGER    01440000
         MACRO                                                          01450000
&LABEL   BCTG  &R1,&S2                                                  01460000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    01470000
         DS    0H                                                       01480000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0046'              01490000
         MEND                                                           01500000
./ ADD NAME=BCTGR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    01510000
         MACRO                                                          01520000
&LABEL   BCTGR &R1,&R2                                                  01530000
         DS    0H                                                       01540000
&LABEL.  DC    0XL4'00',X'B94600',AL.4(&R1.,&R2.)                       01550000
         MEND                                                           01560000
./ ADD NAME=BRAS     0100-02254-02254-0900-00005-00005-00000-JJAEGER    01570000
         MACRO                                                          01580000
&LABEL   BRAS  &R1,&I2                                                  01590000
         DS    0H                                                       01600000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'05'),Y((&I2.-(*-2))/2)        01610000
         MEND                                                           01620000
./ ADD NAME=BRASL    0100-02254-02254-0900-00005-00005-00000-JJAEGER    01630000
         MACRO                                                          01640000
&LABEL   BRASL &R1,&I2                                                  01650000
         DS    0H                                                       01660000
&LABEL.  DC    0XL6'00',X'C0',AL.4(&R1.,X'05'),AL4((&I2.-(*-2))/2)      01670000
         MEND                                                           01680000
./ ADD NAME=BRC      0100-02254-02254-0900-00005-00005-00000-JJAEGER    01690000
         MACRO                                                          01700000
&LABEL   BRC   &R1,&I2                                                  01710000
         DS    0H                                                       01720000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'04'),Y((&I2.-(*-2))/2)        01730000
         MEND                                                           01740000
./ ADD NAME=BRCL     0100-02254-02254-0900-00005-00005-00000-JJAEGER    01750000
         MACRO                                                          01760000
&LABEL   BRCL  &R1,&I2                                                  01770000
         DS    0H                                                       01780000
&LABEL.  DC    0XL6'00',X'C0',AL.4(&R1.,X'04'),AL4((&I2.-(*-2))/2)      01790000
         MEND                                                           01800000
./ ADD NAME=BRCT     0100-02254-02254-0900-00005-00005-00000-JJAEGER    01810000
         MACRO                                                          01820000
&LABEL   BRCT  &R1,&I2                                                  01830000
         DS    0H                                                       01840000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'06'),Y((&I2.-(*-2))/2)        01850000
         MEND                                                           01860000
./ ADD NAME=BRCTG    0100-02254-02254-0900-00005-00005-00000-JJAEGER    01870000
         MACRO                                                          01880000
&LABEL   BRCTG &R1,&I2                                                  01890000
         DS    0H                                                       01900000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'07'),Y((&I2.-(*-2))/2)        01910000
         MEND                                                           01920000
./ ADD NAME=BRE      0100-02254-02254-0900-00004-00004-00000-JJAEGER    01930000
         MACRO                                                          01940000
&LABEL   BRE &I2                                                        01950000
&LABEL.  BRC 8,&I2.                                                     01960000
         MEND                                                           01970000
./ ADD NAME=BRH      0100-02254-02254-0900-00004-00004-00000-JJAEGER    01980000
         MACRO                                                          01990000
&LABEL   BRH &I2                                                        02000000
&LABEL.  BRC 2,&I2.                                                     02010000
         MEND                                                           02020000
./ ADD NAME=BRL      0100-02254-02254-0900-00004-00004-00000-JJAEGER    02030000
         MACRO                                                          02040000
&LABEL   BRL &I2                                                        02050000
&LABEL.  BRC 4,&I2.                                                     02060000
         MEND                                                           02070000
./ ADD NAME=BRM      0100-02254-02254-0900-00004-00004-00000-JJAEGER    02080000
         MACRO                                                          02090000
&LABEL   BRM &I2                                                        02100000
&LABEL.  BRC 4,&I2.                                                     02110000
         MEND                                                           02120000
./ ADD NAME=BRNE     0100-02254-02254-0900-00004-00004-00000-JJAEGER    02130000
         MACRO                                                          02140000
&LABEL   BRNE &I2                                                       02150000
&LABEL.  BRC 7,&I2.                                                     02160000
         MEND                                                           02170000
./ ADD NAME=BRNH     0100-02254-02254-0900-00004-00004-00000-JJAEGER    02180000
         MACRO                                                          02190000
&LABEL   BRNH &I2                                                       02200000
&LABEL.  BRC 13,&I2.                                                    02210000
         MEND                                                           02220000
./ ADD NAME=BRNL     0100-02254-02254-0900-00004-00004-00000-JJAEGER    02230000
         MACRO                                                          02240000
&LABEL   BRNL &I2                                                       02250000
&LABEL.  BRC 11,&I2.                                                    02260000
         MEND                                                           02270000
./ ADD NAME=BRNM     0100-02254-02254-0900-00004-00004-00000-JJAEGER    02280000
         MACRO                                                          02290000
&LABEL   BRNM &I2                                                       02300000
&LABEL.  BRC 11,&I2.                                                    02310000
         MEND                                                           02320000
./ ADD NAME=BRNO     0100-02254-02254-0900-00004-00004-00000-JJAEGER    02330000
         MACRO                                                          02340000
&LABEL   BRNO &I2                                                       02350000
&LABEL.  BRC 14,&I2.                                                    02360000
         MEND                                                           02370000
./ ADD NAME=BRNP     0100-02254-02254-0900-00004-00004-00000-JJAEGER    02380000
         MACRO                                                          02390000
&LABEL   BRNP &I2                                                       02400000
&LABEL.  BRC 13,&I2.                                                    02410000
         MEND                                                           02420000
./ ADD NAME=BRNZ     0100-02254-02254-0900-00004-00004-00000-JJAEGER    02430000
         MACRO                                                          02440000
&LABEL   BRNZ &I2                                                       02450000
&LABEL.  BRC 7,&I2.                                                     02460000
         MEND                                                           02470000
./ ADD NAME=BRO      0100-02254-02254-0900-00004-00004-00000-JJAEGER    02480000
         MACRO                                                          02490000
&LABEL   BRO &I2                                                        02500000
&LABEL.  BRC 1,&I2.                                                     02510000
         MEND                                                           02520000
./ ADD NAME=BRP      0100-02254-02254-0900-00004-00004-00000-JJAEGER    02530000
         MACRO                                                          02540000
&LABEL   BRP &I2                                                        02550000
&LABEL.  BRC 2,&I2.                                                     02560000
         MEND                                                           02570000
./ ADD NAME=BRU      0100-02254-02254-0900-00004-00004-00000-JJAEGER    02580000
         MACRO                                                          02590000
&LABEL   BRU &I2                                                        02600000
&LABEL.  BRC 15,&I2.                                                    02610000
         MEND                                                           02620000
./ ADD NAME=BRXH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    02630000
         MACRO                                                          02640000
&LABEL   BRXH  &R1,&R3,&I2                                              02650000
         DS    0H                                                       02660000
&LABEL.  DC    0XL4'00',X'84',AL.4(&R1.,&R3.),Y((&I2.-(*-2))/2)         02670000
         MEND                                                           02680000
./ ADD NAME=BRXHG    0100-02254-02254-0900-00005-00005-00000-JJAEGER    02690000
         MACRO                                                          02700000
&LABEL   BRXHG &R1,&R3,&I2                                              02710000
         DS    0H                                                       02720000
&LABEL.  DC    0XL6'00',X'EC',AL.4(&R1.,&R3.),Y((&I2.-(*-2))/2),X'0044' 02730000
         MEND                                                           02740000
./ ADD NAME=BRXLE    0100-02254-02254-0900-00005-00005-00000-JJAEGER    02750000
         MACRO                                                          02760000
&LABEL   BRXLE &R1,&R3,&I2                                              02770000
         DS    0H                                                       02780000
&LABEL.  DC    0XL4'00',X'85',AL.4(&R1.,&R3.),Y((&I2.-(*-2))/2)         02790000
         MEND                                                           02800000
./ ADD NAME=BRXLG    0100-02254-02254-0900-00005-00005-00000-JJAEGER    02810000
         MACRO                                                          02820000
&LABEL   BRXLG &R1,&R3,&I2                                              02830000
         DS    0H                                                       02840000
&LABEL.  DC    0XL6'00',X'EC',AL.4(&R1.,&R3.),Y((&I2.-(*-2))/2),X'0045' 02850000
         MEND                                                           02860000
./ ADD NAME=BRZ      0100-02254-02254-0900-00004-00004-00000-JJAEGER    02870000
         MACRO                                                          02880000
&LABEL   BRZ &I2                                                        02890000
&LABEL.  BRC 8,&I2.                                                     02900000
         MEND                                                           02910000
./ ADD NAME=BSA      0100-02254-02254-0900-00005-00005-00000-JJAEGER    02920000
         MACRO                                                          02930000
&LABEL   BSA   &R1,&R2                                                  02940000
         DS    0H                                                       02950000
&LABEL.  DC    0XL4'00',X'B25A00',AL.4(&R1.,&R2.)                       02960000
         MEND                                                           02970000
./ ADD NAME=BSG      0100-02254-02254-0900-00005-00005-00000-JJAEGER    02980000
         MACRO                                                          02990000
&LABEL   BSG   &R1,&R2                                                  03000000
         DS    0H                                                       03010000
&LABEL.  DC    0XL4'00',X'B25800',AL.4(&R1.,&R2.)                       03020000
         MEND                                                           03030000
./ ADD NAME=BSM      0100-02254-02254-0900-00005-00005-00000-JJAEGER    03040000
         MACRO                                                          03050000
&LABEL   BSM   &R1,&R2                                                  03060000
         DS    0H                                                       03070000
&LABEL.  DC    0XL2'00',X'0B',AL.4(&R1.,&R2.)                           03080000
         MEND                                                           03090000
./ ADD NAME=BXHG     0100-02254-02254-0900-00005-00005-00000-JJAEGER    03100000
         MACRO                                                          03110000
&LABEL   BXHG  &R1,&R3,&S2                                              03120000
         DS    0H                                                       03130000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'0044'           03140000
         MEND                                                           03150000
./ ADD NAME=BXLEG    0100-02254-02254-0900-00005-00005-00000-JJAEGER    03160000
         MACRO                                                          03170000
&LABEL   BXLEG &R1,&R3,&S2                                              03180000
         DS    0H                                                       03190000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'0045'           03200000
         MEND                                                           03210000
./ ADD NAME=CDB      0100-02254-02254-0900-00006-00006-00000-JJAEGER    03220000
         MACRO                                                          03230000
&LABEL   CDB   &R1,&S2                                                  03240000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    03250000
         DS    0H                                                       03260000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0019'              03270000
         MEND                                                           03280000
./ ADD NAME=CDBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    03290000
         MACRO                                                          03300000
&LABEL   CDBR  &R1,&R2                                                  03310000
         DS    0H                                                       03320000
&LABEL.  DC    0XL4'00',X'B31900',AL.4(&R1.,&R2.)                       03330000
         MEND                                                           03340000
./ ADD NAME=CDFBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    03350000
         MACRO                                                          03360000
&LABEL   CDFBR &R1,&R2                                                  03370000
         DS    0H                                                       03380000
&LABEL.  DC    0XL4'00',X'B39500',AL.4(&R1.,&R2.)                       03390000
         MEND                                                           03400000
./ ADD NAME=CDFR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    03410000
         MACRO                                                          03420000
&LABEL   CDFR  &R1,&R2                                                  03430000
         DS    0H                                                       03440000
&LABEL.  DC    0XL4'00',X'B3B500',AL.4(&R1.,&R2.)                       03450000
         MEND                                                           03460000
./ ADD NAME=CDGBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    03470000
         MACRO                                                          03480000
&LABEL   CDGBR &R1,&R2                                                  03490000
         DS    0H                                                       03500000
&LABEL.  DC    0XL4'00',X'B3A500',AL.4(&R1.,&R2.)                       03510000
         MEND                                                           03520000
./ ADD NAME=CDSG     0100-02254-02254-0900-00005-00005-00000-JJAEGER    03530000
         MACRO                                                          03540000
&LABEL   CDSG  &R1,&R3,&S2                                              03550000
         DS    0H                                                       03560000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'003E'           03570000
         MEND                                                           03580000
./ ADD NAME=CEB      0100-02254-02254-0900-00006-00006-00000-JJAEGER    03590000
         MACRO                                                          03600000
&LABEL   CEB   &R1,&S2                                                  03610000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    03620000
         DS    0H                                                       03630000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0009'              03640000
         MEND                                                           03650000
./ ADD NAME=CEBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    03660000
         MACRO                                                          03670000
&LABEL   CEBR  &R1,&R2                                                  03680000
         DS    0H                                                       03690000
&LABEL.  DC    0XL4'00',X'B30900',AL.4(&R1.,&R2.)                       03700000
         MEND                                                           03710000
./ ADD NAME=CEFBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    03720000
         MACRO                                                          03730000
&LABEL   CEFBR &R1,&R2                                                  03740000
         DS    0H                                                       03750000
&LABEL.  DC    0XL4'00',X'B39400',AL.4(&R1.,&R2.)                       03760000
         MEND                                                           03770000
./ ADD NAME=CEFR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    03780000
         MACRO                                                          03790000
&LABEL   CEFR  &R1,&R2                                                  03800000
         DS    0H                                                       03810000
&LABEL.  DC    0XL4'00',X'B3B400',AL.4(&R1.,&R2.)                       03820000
         MEND                                                           03830000
./ ADD NAME=CEGBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    03840000
         MACRO                                                          03850000
&LABEL   CEGBR &R1,&R2                                                  03860000
         DS    0H                                                       03870000
&LABEL.  DC    0XL4'00',X'B3A400',AL.4(&R1.,&R2.)                       03880000
         MEND                                                           03890000
./ ADD NAME=CFC      0100-02254-02254-0900-00005-00005-00000-JJAEGER    03900000
         MACRO                                                          03910000
&LABEL   CFC   &S2                                                      03920000
         DS    0H                                                       03930000
&LABEL.  DC    0XL4'00',X'B21A',S(&S2.)                                 03940000
         MEND                                                           03950000
./ ADD NAME=CFDBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    03960000
         MACRO                                                          03970000
&LABEL   CFDBR &R1,&M3,&R2                                              03980000
         DS    0H                                                       03990000
&LABEL.  DC    0XL4'00',X'B399',AL.4(&M3.,0,&R1.,&R2.)                  04000000
         MEND                                                           04010000
./ ADD NAME=CFDR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    04020000
         MACRO                                                          04030000
&LABEL   CFDR  &R1,&M3,&R2                                              04040000
         DS    0H                                                       04050000
&LABEL.  DC    0XL4'00',X'B3B3',AL.4(&M3.,0,&R1.,&R2.)                  04060000
         MEND                                                           04070000
./ ADD NAME=CFEBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    04080000
         MACRO                                                          04090000
&LABEL   CFEBR &R1,&M3,&R2                                              04100000
         DS    0H                                                       04110000
&LABEL.  DC    0XL4'00',X'B398',AL.4(&M3.,0,&R1.,&R2.)                  04120000
         MEND                                                           04130000
./ ADD NAME=CFER     0100-02254-02254-0900-00005-00005-00000-JJAEGER    04140000
         MACRO                                                          04150000
&LABEL   CFER  &R1,&M3,&R2                                              04160000
         DS    0H                                                       04170000
&LABEL.  DC    0XL4'00',X'B3B8',AL.4(&M3.,0,&R1.,&R2.)                  04180000
         MEND                                                           04190000
./ ADD NAME=CFXR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    04200000
         MACRO                                                          04210000
&LABEL   CFXR  &R1,&M3,&R2                                              04220000
         DS    0H                                                       04230000
&LABEL.  DC    0XL4'00',X'B3BA',AL.4(&M3.,0,&R1.,&R2.)                  04240000
         MEND                                                           04250000
./ ADD NAME=CG       0100-02254-02254-0900-00006-00006-00000-JJAEGER    04260000
         MACRO                                                          04270000
&LABEL   CG    &R1,&S2                                                  04280000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    04290000
         DS    0H                                                       04300000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0020'              04310000
         MEND                                                           04320000
./ ADD NAME=CGDBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    04330000
         MACRO                                                          04340000
&LABEL   CGDBR &R1,&M3,&R2                                              04350000
         DS    0H                                                       04360000
&LABEL.  DC    0XL4'00',X'B3A9',AL.4(&M3.,0,&R1.,&R2.)                  04370000
         MEND                                                           04380000
./ ADD NAME=CGEBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    04390000
         MACRO                                                          04400000
&LABEL   CGEBR &R1,&M3,&R2                                              04410000
         DS    0H                                                       04420000
&LABEL.  DC    0XL4'00',X'B3A8',AL.4(&M3.,0,&R1.,&R2.)                  04430000
         MEND                                                           04440000
./ ADD NAME=CGF      0100-02254-02254-0900-00006-00006-00000-JJAEGER    04450000
         MACRO                                                          04460000
&LABEL   CGF   &R1,&S2                                                  04470000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    04480000
         DS    0H                                                       04490000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0030'              04500000
         MEND                                                           04510000
./ ADD NAME=CGFR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    04520000
         MACRO                                                          04530000
&LABEL   CGFR  &R1,&R2                                                  04540000
         DS    0H                                                       04550000
&LABEL.  DC    0XL4'00',X'B93000',AL.4(&R1.,&R2.)                       04560000
         MEND                                                           04570000
./ ADD NAME=CGHI     0100-02254-02254-0900-00005-00005-00000-JJAEGER    04580000
         MACRO                                                          04590000
&LABEL   CGHI  &R1,&I2                                                  04600000
         DS    0H                                                       04610000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'0F'),Y(&I2.)                  04620000
         MEND                                                           04630000
./ ADD NAME=CGR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    04640000
         MACRO                                                          04650000
&LABEL   CGR   &R1,&R2                                                  04660000
         DS    0H                                                       04670000
&LABEL.  DC    0XL4'00',X'B92000',AL.4(&R1.,&R2.)                       04680000
         MEND                                                           04690000
./ ADD NAME=CHI      0100-02254-02254-0900-00005-00005-00000-JJAEGER    04700000
         MACRO                                                          04710000
&LABEL   CHI   &R1,&I2                                                  04720000
         DS    0H                                                       04730000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'0E'),Y(&I2.)                  04740000
         MEND                                                           04750000
./ ADD NAME=CHSC     0100-02254-02254-0900-00005-00005-00000-JJAEGER    04760000
         MACRO                                                          04770000
&LABEL   CHSC  &R1,&R2                                                  04780000
         DS    0H                                                       04790000
&LABEL.  DC    0XL4'00',X'B25F00',AL.4(&R1.,&R2.)                       04800000
         MEND                                                           04810000
./ ADD NAME=CKSM     0100-02254-02254-0900-00005-00005-00000-JJAEGER    04820000
         MACRO                                                          04830000
&LABEL   CKSM  &R1,&R2                                                  04840000
         DS    0H                                                       04850000
&LABEL.  DC    0XL4'00',X'B24100',AL.4(&R1.,&R2.)                       04860000
         MEND                                                           04870000
./ ADD NAME=CLCLE    0100-02254-02254-0900-00005-00005-00000-JJAEGER    04880000
         MACRO                                                          04890000
&LABEL   CLCLE &R1,&R3,&S2                                              04900000
         DS    0H                                                       04910000
&LABEL.  DC    0XL4'00',X'A9',AL.4(&R1.,&R3.),S(&S2.)                   04920000
         MEND                                                           04930000
./ ADD NAME=CLCLU    0100-02254-02254-0900-00005-00005-00000-JJAEGER    04940000
         MACRO                                                          04950000
&LABEL   CLCLU &R1,&R3,&S2                                              04960000
         DS    0H                                                       04970000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'008F'           04980000
         MEND                                                           04990000
./ ADD NAME=CLG      0100-02254-02254-0900-00006-00006-00000-JJAEGER    05000000
         MACRO                                                          05010000
&LABEL   CLG   &R1,&S2                                                  05020000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    05030000
         DS    0H                                                       05040000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0021'              05050000
         MEND                                                           05060000
./ ADD NAME=CLGF     0100-02254-02254-0900-00006-00006-00000-JJAEGER    05070000
         MACRO                                                          05080000
&LABEL   CLGF  &R1,&S2                                                  05090000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    05100000
         DS    0H                                                       05110000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0031'              05120000
         MEND                                                           05130000
./ ADD NAME=CLGFR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    05140000
         MACRO                                                          05150000
&LABEL   CLGFR &R1,&R2                                                  05160000
         DS    0H                                                       05170000
&LABEL.  DC    0XL4'00',X'B93100',AL.4(&R1.,&R2.)                       05180000
         MEND                                                           05190000
./ ADD NAME=CLGR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    05200000
         MACRO                                                          05210000
&LABEL   CLGR  &R1,&R2                                                  05220000
         DS    0H                                                       05230000
&LABEL.  DC    0XL4'00',X'B92100',AL.4(&R1.,&R2.)                       05240000
         MEND                                                           05250000
./ ADD NAME=CLMH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    05260000
         MACRO                                                          05270000
&LABEL   CLMH  &R1,&R3,&S2                                              05280000
         DS    0H                                                       05290000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'0020'           05300000
         MEND                                                           05310000
./ ADD NAME=CLST     0100-02254-02254-0900-00005-00005-00000-JJAEGER    05320000
         MACRO                                                          05330000
&LABEL   CLST  &R1,&R2                                                  05340000
         DS    0H                                                       05350000
&LABEL.  DC    0XL4'00',X'B25D00',AL.4(&R1.,&R2.)                       05360000
         MEND                                                           05370000
./ ADD NAME=CMPSC    0100-02254-02254-0900-00005-00005-00000-JJAEGER    05380000
         MACRO                                                          05390000
&LABEL   CMPSC &R1,&R2                                                  05400000
         DS    0H                                                       05410000
&LABEL.  DC    0XL4'00',X'B26300',AL.4(&R1.,&R2.)                       05420000
         MEND                                                           05430000
./ ADD NAME=CPYA     0100-02254-02254-0900-00005-00005-00000-JJAEGER    05440000
         MACRO                                                          05450000
&LABEL   CPYA  &R1,&R2                                                  05460000
         DS    0H                                                       05470000
&LABEL.  DC    0XL4'00',X'B24D00',AL.4(&R1.,&R2.)                       05480000
         MEND                                                           05490000
./ ADD NAME=CSCH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    05500000
         MACRO                                                          05510000
&LABEL   CSCH  &S2                                                      05520000
         DS    0H                                                       05530000
&LABEL.  DC    0XL4'00',X'B230',S(&S2.)                                 05540000
         MEND                                                           05550000
./ ADD NAME=CSG      0100-02254-02254-0900-00005-00005-00000-JJAEGER    05560000
         MACRO                                                          05570000
&LABEL   CSG   &R1,&R3,&S2                                              05580000
         DS    0H                                                       05590000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'0030'           05600000
         MEND                                                           05610000
./ ADD NAME=CSP      0100-02254-02254-0900-00005-00005-00000-JJAEGER    05620000
         MACRO                                                          05630000
&LABEL   CSP   &R1,&R2                                                  05640000
         DS    0H                                                       05650000
&LABEL.  DC    0XL4'00',X'B25000',AL.4(&R1.,&R2.)                       05660000
         MEND                                                           05670000
./ ADD NAME=CUSE     0100-02254-02254-0900-00005-00005-00000-JJAEGER    05680000
         MACRO                                                          05690000
&LABEL   CUSE  &R1,&R2                                                  05700000
         DS    0H                                                       05710000
&LABEL.  DC    0XL4'00',X'B25700',AL.4(&R1.,&R2.)                       05720000
         MEND                                                           05730000
./ ADD NAME=CUTFU    0100-02254-02254-0900-00005-00005-00000-JJAEGER    05740000
         MACRO                                                          05750000
&LABEL   CUTFU &R1,&R2                                                  05760000
         DS    0H                                                       05770000
&LABEL.  DC    0XL4'00',X'B2A700',AL.4(&R1.,&R2.)                       05780000
         MEND                                                           05790000
./ ADD NAME=CUUTF    0100-02254-02254-0900-00005-00005-00000-JJAEGER    05800000
         MACRO                                                          05810000
&LABEL   CUUTF &R1,&R2                                                  05820000
         DS    0H                                                       05830000
&LABEL.  DC    0XL4'00',X'B2A600',AL.4(&R1.,&R2.)                       05840000
         MEND                                                           05850000
./ ADD NAME=CVBG     0100-02254-02254-0900-00006-00006-00000-JJAEGER    05860000
         MACRO                                                          05870000
&LABEL   CVBG  &R1,&S2                                                  05880000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    05890000
         DS    0H                                                       05900000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'000E'              05910000
         MEND                                                           05920000
./ ADD NAME=CVDG     0100-02254-02254-0900-00006-00006-00000-JJAEGER    05930000
         MACRO                                                          05940000
&LABEL   CVDG  &R1,&S2                                                  05950000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    05960000
         DS    0H                                                       05970000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'002E'              05980000
         MEND                                                           05990000
./ ADD NAME=CXBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    06000000
         MACRO                                                          06010000
&LABEL   CXBR  &R1,&R2                                                  06020000
         DS    0H                                                       06030000
&LABEL.  DC    0XL4'00',X'B34900',AL.4(&R1.,&R2.)                       06040000
         MEND                                                           06050000
./ ADD NAME=CXFR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    06060000
         MACRO                                                          06070000
&LABEL   CXFR  &R1,&R2                                                  06080000
         DS    0H                                                       06090000
&LABEL.  DC    0XL4'00',X'B3B600',AL.4(&R1.,&R2.)                       06100000
         MEND                                                           06110000
./ ADD NAME=CXR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    06120000
         MACRO                                                          06130000
&LABEL   CXR   &R1,&R2                                                  06140000
         DS    0H                                                       06150000
&LABEL.  DC    0XL4'00',X'B36900',AL.4(&R1.,&R2.)                       06160000
         MEND                                                           06170000
./ ADD NAME=DDB      0100-02254-02254-0900-00006-00006-00000-JJAEGER    06180000
         MACRO                                                          06190000
&LABEL   DDB   &R1,&S2                                                  06200000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    06210000
         DS    0H                                                       06220000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'001D'              06230000
         MEND                                                           06240000
./ ADD NAME=DDBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    06250000
         MACRO                                                          06260000
&LABEL   DDBR  &R1,&R2                                                  06270000
         DS    0H                                                       06280000
&LABEL.  DC    0XL4'00',X'B31D00',AL.4(&R1.,&R2.)                       06290000
         MEND                                                           06300000
./ ADD NAME=DEB      0100-02254-02254-0900-00006-00006-00000-JJAEGER    06310000
         MACRO                                                          06320000
&LABEL   DEB   &R1,&S2                                                  06330000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    06340000
         DS    0H                                                       06350000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'000D'              06360000
         MEND                                                           06370000
./ ADD NAME=DEBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    06380000
         MACRO                                                          06390000
&LABEL   DEBR  &R1,&R2                                                  06400000
         DS    0H                                                       06410000
&LABEL.  DC    0XL4'00',X'B30D00',AL.4(&R1.,&R2.)                       06420000
         MEND                                                           06430000
./ ADD NAME=DL       0100-02254-02254-0900-00006-00006-00000-JJAEGER    06440000
         MACRO                                                          06450000
&LABEL   DL    &R1,&S2                                                  06460000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    06470000
         DS    0H                                                       06480000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0097'              06490000
         MEND                                                           06500000
./ ADD NAME=DLG      0100-02254-02254-0900-00006-00006-00000-JJAEGER    06510000
         MACRO                                                          06520000
&LABEL   DLG   &R1,&S2                                                  06530000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    06540000
         DS    0H                                                       06550000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0087'              06560000
         MEND                                                           06570000
./ ADD NAME=DLGR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    06580000
         MACRO                                                          06590000
&LABEL   DLGR  &R1,&R2                                                  06600000
         DS    0H                                                       06610000
&LABEL.  DC    0XL4'00',X'B98700',AL.4(&R1.,&R2.)                       06620000
         MEND                                                           06630000
./ ADD NAME=DLR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    06640000
         MACRO                                                          06650000
&LABEL   DLR   &R1,&R2                                                  06660000
         DS    0H                                                       06670000
&LABEL.  DC    0XL4'00',X'B99700',AL.4(&R1.,&R2.)                       06680000
         MEND                                                           06690000
./ ADD NAME=DSG      0100-02254-02254-0900-00006-00006-00000-JJAEGER    06700000
         MACRO                                                          06710000
&LABEL   DSG   &R1,&S2                                                  06720000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    06730000
         DS    0H                                                       06740000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'000D'              06750000
         MEND                                                           06760000
./ ADD NAME=DSGF     0100-02254-02254-0900-00006-00006-00000-JJAEGER    06770000
         MACRO                                                          06780000
&LABEL   DSGF  &R1,&S2                                                  06790000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    06800000
         DS    0H                                                       06810000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'001D'              06820000
         MEND                                                           06830000
./ ADD NAME=DSGFR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    06840000
         MACRO                                                          06850000
&LABEL   DSGFR &R1,&R2                                                  06860000
         DS    0H                                                       06870000
&LABEL.  DC    0XL4'00',X'B91D00',AL.4(&R1.,&R2.)                       06880000
         MEND                                                           06890000
./ ADD NAME=DSGR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    06900000
         MACRO                                                          06910000
&LABEL   DSGR  &R1,&R2                                                  06920000
         DS    0H                                                       06930000
&LABEL.  DC    0XL4'00',X'B90D00',AL.4(&R1.,&R2.)                       06940000
         MEND                                                           06950000
./ ADD NAME=DXBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    06960000
         MACRO                                                          06970000
&LABEL   DXBR  &R1,&R2                                                  06980000
         DS    0H                                                       06990000
&LABEL.  DC    0XL4'00',X'B34D00',AL.4(&R1.,&R2.)                       07000000
         MEND                                                           07010000
./ ADD NAME=EAR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    07020000
         MACRO                                                          07030000
&LABEL   EAR   &R1,&R2                                                  07040000
         DS    0H                                                       07050000
&LABEL.  DC    0XL4'00',X'B24F00',AL.4(&R1.,&R2.)                       07060000
         MEND                                                           07070000
./ ADD NAME=EFPC     0100-02254-02254-0900-00005-00005-00000-JJAEGER    07080000
         MACRO                                                          07090000
&LABEL   EFPC  &R1,&R2                                                  07100000
         DS    0H                                                       07110000
&LABEL.  DC    0XL4'00',X'B38C00',AL.4(&R1.,&R2.)                       07120000
         MEND                                                           07130000
./ ADD NAME=EPSW     0100-02254-02254-0900-00005-00005-00000-JJAEGER    07140000
         MACRO                                                          07150000
&LABEL   EPSW  &R1,&R2                                                  07160000
         DS    0H                                                       07170000
&LABEL.  DC    0XL4'00',X'B98D00',AL.4(&R1.,&R2.)                       07180000
         MEND                                                           07190000
./ ADD NAME=EREG     0100-02254-02254-0900-00005-00005-00000-JJAEGER    07200000
         MACRO                                                          07210000
&LABEL   EREG  &R1,&R2                                                  07220000
         DS    0H                                                       07230000
&LABEL.  DC    0XL4'00',X'B24900',AL.4(&R1.,&R2.)                       07240000
         MEND                                                           07250000
./ ADD NAME=EREGG    0100-02254-02254-0900-00005-00005-00000-JJAEGER    07260000
         MACRO                                                          07270000
&LABEL   EREGG &R1,&R2                                                  07280000
         DS    0H                                                       07290000
&LABEL.  DC    0XL4'00',X'B90E00',AL.4(&R1.,&R2.)                       07300000
         MEND                                                           07310000
./ ADD NAME=ESEA     0100-02254-02254-0900-00005-00005-00000-JJAEGER    07320000
         MACRO                                                          07330000
&LABEL   ESEA  &R1                                                      07340000
         DS    0H                                                       07350000
&LABEL.  DC    0XL4'00',X'B99D00',AL.4(&R1.,0)                          07360000
         MEND                                                           07370000
./ ADD NAME=ESTA     0100-02254-02254-0900-00005-00005-00000-JJAEGER    07380000
         MACRO                                                          07390000
&LABEL   ESTA  &R1,&R2                                                  07400000
         DS    0H                                                       07410000
&LABEL.  DC    0XL4'00',X'B24A00',AL.4(&R1.,&R2.)                       07420000
         MEND                                                           07430000
./ ADD NAME=FIDR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    07440000
         MACRO                                                          07450000
&LABEL   FIDR  &R1,&R2                                                  07460000
         DS    0H                                                       07470000
&LABEL.  DC    0XL4'00',X'B37F00',AL.4(&R1.,&R2.)                       07480000
         MEND                                                           07490000
./ ADD NAME=FIER     0100-02254-02254-0900-00005-00005-00000-JJAEGER    07500000
         MACRO                                                          07510000
&LABEL   FIER  &R1,&R2                                                  07520000
         DS    0H                                                       07530000
&LABEL.  DC    0XL4'00',X'B37700',AL.4(&R1.,&R2.)                       07540000
         MEND                                                           07550000
./ ADD NAME=FIXR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    07560000
         MACRO                                                          07570000
&LABEL   FIXR  &R1,&R2                                                  07580000
         DS    0H                                                       07590000
&LABEL.  DC    0XL4'00',X'B36700',AL.4(&R1.,&R2.)                       07600000
         MEND                                                           07610000
./ ADD NAME=HSCH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    07620000
         MACRO                                                          07630000
&LABEL   HSCH  &S2                                                      07640000
         DS    0H                                                       07650000
&LABEL.  DC    0XL4'00',X'B231',S(&S2.)                                 07660000
         MEND                                                           07670000
./ ADD NAME=ICMH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    07680000
         MACRO                                                          07690000
&LABEL   ICMH  &R1,&R3,&S2                                              07700000
         DS    0H                                                       07710000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'0080'           07720000
         MEND                                                           07730000
./ ADD NAME=IESBE    0100-02254-02254-0900-00005-00005-00000-JJAEGER    07740000
         MACRO                                                          07750000
&LABEL   IESBE &R1,&R2                                                  07760000
         DS    0H                                                       07770000
&LABEL.  DC    0XL4'00',X'B25900',AL.4(&R1.,&R2.)                       07780000
         MEND                                                           07790000
./ ADD NAME=IIHH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    07800000
         MACRO                                                          07810000
&LABEL   IIHH  &R1,&I2                                                  07820000
         DS    0H                                                       07830000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'00'),Y(&I2.)                  07840000
         MEND                                                           07850000
./ ADD NAME=IIHL     0100-02254-02254-0900-00005-00005-00000-JJAEGER    07860000
         MACRO                                                          07870000
&LABEL   IIHL  &R1,&I2                                                  07880000
         DS    0H                                                       07890000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'01'),Y(&I2.)                  07900000
         MEND                                                           07910000
./ ADD NAME=IILH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    07920000
         MACRO                                                          07930000
&LABEL   IILH  &R1,&I2                                                  07940000
         DS    0H                                                       07950000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'02'),Y(&I2.)                  07960000
         MEND                                                           07970000
./ ADD NAME=IILL     0100-02254-02254-0900-00005-00005-00000-JJAEGER    07980000
         MACRO                                                          07990000
&LABEL   IILL  &R1,&I2                                                  08000000
         DS    0H                                                       08010000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'03'),Y(&I2.)                  08020000
         MEND                                                           08030000
./ ADD NAME=J        0100-02254-02254-0900-00004-00004-00000-JJAEGER    08040000
         MACRO                                                          08050000
&LABEL   J &I2                                                          08060000
&LABEL.  BRC 15,&I2.                                                    08070000
         MEND                                                           08080000
./ ADD NAME=JE       0100-02254-02254-0900-00004-00004-00000-JJAEGER    08090000
         MACRO                                                          08100000
&LABEL   JE &I2                                                         08110000
&LABEL.  BRC 8,&I2.                                                     08120000
         MEND                                                           08130000
./ ADD NAME=JH       0100-02254-02254-0900-00004-00004-00000-JJAEGER    08140000
         MACRO                                                          08150000
&LABEL   JH &I2                                                         08160000
&LABEL.  BRC 2,&I2.                                                     08170000
         MEND                                                           08180000
./ ADD NAME=JL       0100-02254-02254-0900-00004-00004-00000-JJAEGER    08190000
         MACRO                                                          08200000
&LABEL   JL &I2                                                         08210000
&LABEL.  BRC 4,&I2.                                                     08220000
         MEND                                                           08230000
./ ADD NAME=JM       0100-02254-02254-0900-00004-00004-00000-JJAEGER    08240000
         MACRO                                                          08250000
&LABEL   JM &I2                                                         08260000
&LABEL.  BRC 4,&I2.                                                     08270000
         MEND                                                           08280000
./ ADD NAME=JNE      0100-02254-02254-0900-00004-00004-00000-JJAEGER    08290000
         MACRO                                                          08300000
&LABEL   JNE &I2                                                        08310000
&LABEL.  BRC 7,&I2.                                                     08320000
         MEND                                                           08330000
./ ADD NAME=JNH      0100-02254-02254-0900-00004-00004-00000-JJAEGER    08340000
         MACRO                                                          08350000
&LABEL   JNH &I2                                                        08360000
&LABEL.  BRC 13,&I2.                                                    08370000
         MEND                                                           08380000
./ ADD NAME=JNL      0100-02254-02254-0900-00004-00004-00000-JJAEGER    08390000
         MACRO                                                          08400000
&LABEL   JNL &I2                                                        08410000
&LABEL.  BRC 11,&I2.                                                    08420000
         MEND                                                           08430000
./ ADD NAME=JNM      0100-02254-02254-0900-00004-00004-00000-JJAEGER    08440000
         MACRO                                                          08450000
&LABEL   JNM &I2                                                        08460000
&LABEL.  BRC 11,&I2.                                                    08470000
         MEND                                                           08480000
./ ADD NAME=JNO      0100-02254-02254-0900-00004-00004-00000-JJAEGER    08490000
         MACRO                                                          08500000
&LABEL   JNO &I2                                                        08510000
&LABEL.  BRC 14,&I2.                                                    08520000
         MEND                                                           08530000
./ ADD NAME=JNOP     0100-02254-02254-0900-00004-00004-00000-JJAEGER    08540000
         MACRO                                                          08550000
&LABEL   JNOP &I2                                                       08560000
&LABEL.  BRC 0,&I2.                                                     08570000
         MEND                                                           08580000
./ ADD NAME=JNZ      0100-02254-02254-0900-00004-00004-00000-JJAEGER    08590000
         MACRO                                                          08600000
&LABEL   JNZ &I2                                                        08610000
&LABEL.  BRC 7,&I2.                                                     08620000
         MEND                                                           08630000
./ ADD NAME=JO       0100-02254-02254-0900-00004-00004-00000-JJAEGER    08640000
         MACRO                                                          08650000
&LABEL   JO &I2                                                         08660000
&LABEL.  BRC 1,&I2.                                                     08670000
         MEND                                                           08680000
./ ADD NAME=JP       0100-02254-02254-0900-00004-00004-00000-JJAEGER    08690000
         MACRO                                                          08700000
&LABEL   JP &I2                                                         08710000
&LABEL.  BRC 13,&I2.                                                    08720000
         MEND                                                           08730000
./ ADD NAME=JZ       0100-02254-02254-0900-00004-00004-00000-JJAEGER    08740000
         MACRO                                                          08750000
&LABEL   JZ &I2                                                         08760000
&LABEL.  BRC 8,&I2.                                                     08770000
         MEND                                                           08780000
./ ADD NAME=KDB      0100-02254-02254-0900-00006-00006-00000-JJAEGER    08790000
         MACRO                                                          08800000
&LABEL   KDB   &R1,&S2                                                  08810000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    08820000
         DS    0H                                                       08830000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0018'              08840000
         MEND                                                           08850000
./ ADD NAME=KDBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    08860000
         MACRO                                                          08870000
&LABEL   KDBR  &R1,&R2                                                  08880000
         DS    0H                                                       08890000
&LABEL.  DC    0XL4'00',X'B31800',AL.4(&R1.,&R2.)                       08900000
         MEND                                                           08910000
./ ADD NAME=KEB      0100-02254-02254-0900-00006-00006-00000-JJAEGER    08920000
         MACRO                                                          08930000
&LABEL   KEB   &R1,&S2                                                  08940000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    08950000
         DS    0H                                                       08960000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0008'              08970000
         MEND                                                           08980000
./ ADD NAME=KEBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    08990000
         MACRO                                                          09000000
&LABEL   KEBR  &R1,&R2                                                  09010000
         DS    0H                                                       09020000
&LABEL.  DC    0XL4'00',X'B30800',AL.4(&R1.,&R2.)                       09030000
         MEND                                                           09040000
./ ADD NAME=KXBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    09050000
         MACRO                                                          09060000
&LABEL   KXBR  &R1,&R2                                                  09070000
         DS    0H                                                       09080000
&LABEL.  DC    0XL4'00',X'B34800',AL.4(&R1.,&R2.)                       09090000
         MEND                                                           09100000
./ ADD NAME=LAE      0100-02254-02254-0900-00006-00006-00000-JJAEGER    09110000
         MACRO                                                          09120000
&LABEL   LAE   &R1,&S2                                                  09130000
         MNOTE 4,'RX FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'     09140000
         DS    0H                                                       09150000
&LABEL.  DC    0XL4'00',X'51',AL.4(&R1.,0),S(&S2.)                      09160000
         MEND                                                           09170000
./ ADD NAME=LAM      0100-02254-02254-0900-00005-00005-00000-JJAEGER    09180000
         MACRO                                                          09190000
&LABEL   LAM   &R1,&R3,&S2                                              09200000
         DS    0H                                                       09210000
&LABEL.  DC    0XL4'00',X'9A',AL.4(&R1.,&R3.),S(&S2.)                   09220000
         MEND                                                           09230000
./ ADD NAME=LARL     0100-02254-02254-0900-00005-00005-00000-JJAEGER    09240000
         MACRO                                                          09250000
&LABEL   LARL  &R1,&I2                                                  09260000
         DS    0H                                                       09270000
&LABEL.  DC    0XL6'00',X'C0',AL.4(&R1.,X'00'),AL4((&I2.-(*-2))/2)      09280000
         MEND                                                           09290000
./ ADD NAME=LCDBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    09300000
         MACRO                                                          09310000
&LABEL   LCDBR &R1,&R2                                                  09320000
         DS    0H                                                       09330000
&LABEL.  DC    0XL4'00',X'B31300',AL.4(&R1.,&R2.)                       09340000
         MEND                                                           09350000
./ ADD NAME=LCEBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    09360000
         MACRO                                                          09370000
&LABEL   LCEBR &R1,&R2                                                  09380000
         DS    0H                                                       09390000
&LABEL.  DC    0XL4'00',X'B30300',AL.4(&R1.,&R2.)                       09400000
         MEND                                                           09410000
./ ADD NAME=LCGFR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    09420000
         MACRO                                                          09430000
&LABEL   LCGFR &R1,&R2                                                  09440000
         DS    0H                                                       09450000
&LABEL.  DC    0XL4'00',X'B91300',AL.4(&R1.,&R2.)                       09460000
         MEND                                                           09470000
./ ADD NAME=LCGR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    09480000
         MACRO                                                          09490000
&LABEL   LCGR  &R1,&R2                                                  09500000
         DS    0H                                                       09510000
&LABEL.  DC    0XL4'00',X'B90300',AL.4(&R1.,&R2.)                       09520000
         MEND                                                           09530000
./ ADD NAME=LCTLG    0100-02254-02254-0900-00005-00005-00000-JJAEGER    09540000
         MACRO                                                          09550000
&LABEL   LCTLG &R1,&R3,&S2                                              09560000
         DS    0H                                                       09570000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'002F'           09580000
         MEND                                                           09590000
./ ADD NAME=LCXBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    09600000
         MACRO                                                          09610000
&LABEL   LCXBR &R1,&R2                                                  09620000
         DS    0H                                                       09630000
&LABEL.  DC    0XL4'00',X'B34300',AL.4(&R1.,&R2.)                       09640000
         MEND                                                           09650000
./ ADD NAME=LCXR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    09660000
         MACRO                                                          09670000
&LABEL   LCXR  &R1,&R2                                                  09680000
         DS    0H                                                       09690000
&LABEL.  DC    0XL4'00',X'B36300',AL.4(&R1.,&R2.)                       09700000
         MEND                                                           09710000
./ ADD NAME=LDE      0100-02254-02254-0900-00006-00006-00000-JJAEGER    09720000
         MACRO                                                          09730000
&LABEL   LDE   &R1,&S2                                                  09740000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    09750000
         DS    0H                                                       09760000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0024'              09770000
         MEND                                                           09780000
./ ADD NAME=LDEB     0100-02254-02254-0900-00006-00006-00000-JJAEGER    09790000
         MACRO                                                          09800000
&LABEL   LDEB  &R1,&S2                                                  09810000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    09820000
         DS    0H                                                       09830000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0004'              09840000
         MEND                                                           09850000
./ ADD NAME=LDEBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    09860000
         MACRO                                                          09870000
&LABEL   LDEBR &R1,&R2                                                  09880000
         DS    0H                                                       09890000
&LABEL.  DC    0XL4'00',X'B30400',AL.4(&R1.,&R2.)                       09900000
         MEND                                                           09910000
./ ADD NAME=LDER     0100-02254-02254-0900-00005-00005-00000-JJAEGER    09920000
         MACRO                                                          09930000
&LABEL   LDER  &R1,&R2                                                  09940000
         DS    0H                                                       09950000
&LABEL.  DC    0XL4'00',X'B32400',AL.4(&R1.,&R2.)                       09960000
         MEND                                                           09970000
./ ADD NAME=LEDBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    09980000
         MACRO                                                          09990000
&LABEL   LEDBR &R1,&R2                                                  10000000
         DS    0H                                                       10010000
&LABEL.  DC    0XL4'00',X'B34400',AL.4(&R1.,&R2.)                       10020000
         MEND                                                           10030000
./ ADD NAME=LEXR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    10040000
         MACRO                                                          10050000
&LABEL   LEXR  &R1,&R2                                                  10060000
         DS    0H                                                       10070000
&LABEL.  DC    0XL4'00',X'B36600',AL.4(&R1.,&R2.)                       10080000
         MEND                                                           10090000
./ ADD NAME=LFPC     0100-02254-02254-0900-00005-00005-00000-JJAEGER    10100000
         MACRO                                                          10110000
&LABEL   LFPC  &S2                                                      10120000
         DS    0H                                                       10130000
&LABEL.  DC    0XL4'00',X'B29D',S(&S2.)                                 10140000
         MEND                                                           10150000
./ ADD NAME=LG       0100-02254-02254-0900-00006-00006-00000-JJAEGER    10160000
         MACRO                                                          10170000
&LABEL   LG    &R1,&S2                                                  10180000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    10190000
         DS    0H                                                       10200000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0004'              10210000
         MEND                                                           10220000
./ ADD NAME=LGF      0100-02254-02254-0900-00006-00006-00000-JJAEGER    10230000
         MACRO                                                          10240000
&LABEL   LGF   &R1,&S2                                                  10250000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    10260000
         DS    0H                                                       10270000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0014'              10280000
         MEND                                                           10290000
./ ADD NAME=LGFR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    10300000
         MACRO                                                          10310000
&LABEL   LGFR  &R1,&R2                                                  10320000
         DS    0H                                                       10330000
&LABEL.  DC    0XL4'00',X'B91400',AL.4(&R1.,&R2.)                       10340000
         MEND                                                           10350000
./ ADD NAME=LGH      0100-02254-02254-0900-00006-00006-00000-JJAEGER    10360000
         MACRO                                                          10370000
&LABEL   LGH   &R1,&S2                                                  10380000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    10390000
         DS    0H                                                       10400000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0015'              10410000
         MEND                                                           10420000
./ ADD NAME=LGHI     0100-02254-02254-0900-00005-00005-00000-JJAEGER    10430000
         MACRO                                                          10440000
&LABEL   LGHI  &R1,&I2                                                  10450000
         DS    0H                                                       10460000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'09'),Y(&I2.)                  10470000
         MEND                                                           10480000
./ ADD NAME=LGR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    10490000
         MACRO                                                          10500000
&LABEL   LGR   &R1,&R2                                                  10510000
         DS    0H                                                       10520000
&LABEL.  DC    0XL4'00',X'B90400',AL.4(&R1.,&R2.)                       10530000
         MEND                                                           10540000
./ ADD NAME=LHI      0100-02254-02254-0900-00005-00005-00000-JJAEGER    10550000
         MACRO                                                          10560000
&LABEL   LHI   &R1,&I2                                                  10570000
         DS    0H                                                       10580000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'08'),Y(&I2.)                  10590000
         MEND                                                           10600000
./ ADD NAME=LKPG     0100-02254-02254-0900-00005-00005-00000-JJAEGER    10610000
         MACRO                                                          10620000
&LABEL   LKPG  &R1,&R2                                                  10630000
         DS    0H                                                       10640000
&LABEL.  DC    0XL4'00',X'B26200',AL.4(&R1.,&R2.)                       10650000
         MEND                                                           10660000
./ ADD NAME=LLGC     0100-02254-02254-0900-00006-00006-00000-JJAEGER    10670000
         MACRO                                                          10680000
&LABEL   LLGC  &R1,&S2                                                  10690000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    10700000
         DS    0H                                                       10710000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0090'              10720000
         MEND                                                           10730000
./ ADD NAME=LLGF     0100-02254-02254-0900-00006-00006-00000-JJAEGER    10740000
         MACRO                                                          10750000
&LABEL   LLGF  &R1,&S2                                                  10760000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    10770000
         DS    0H                                                       10780000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0016'              10790000
         MEND                                                           10800000
./ ADD NAME=LLGFR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    10810000
         MACRO                                                          10820000
&LABEL   LLGFR &R1,&R2                                                  10830000
         DS    0H                                                       10840000
&LABEL.  DC    0XL4'00',X'B91600',AL.4(&R1.,&R2.)                       10850000
         MEND                                                           10860000
./ ADD NAME=LLGH     0100-02254-02254-0900-00006-00006-00000-JJAEGER    10870000
         MACRO                                                          10880000
&LABEL   LLGH  &R1,&S2                                                  10890000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    10900000
         DS    0H                                                       10910000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0091'              10920000
         MEND                                                           10930000
./ ADD NAME=LLGT     0100-02254-02254-0900-00006-00006-00000-JJAEGER    10940000
         MACRO                                                          10950000
&LABEL   LLGT  &R1,&S2                                                  10960000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    10970000
         DS    0H                                                       10980000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0017'              10990000
         MEND                                                           11000000
./ ADD NAME=LLGTR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    11010000
         MACRO                                                          11020000
&LABEL   LLGTR &R1,&R2                                                  11030000
         DS    0H                                                       11040000
&LABEL.  DC    0XL4'00',X'B91700',AL.4(&R1.,&R2.)                       11050000
         MEND                                                           11060000
./ ADD NAME=LLIHH    0100-02254-02254-0900-00005-00005-00000-JJAEGER    11070000
         MACRO                                                          11080000
&LABEL   LLIHH &R1,&I2                                                  11090000
         DS    0H                                                       11100000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'0C'),Y(&I2.)                  11110000
         MEND                                                           11120000
./ ADD NAME=LLIHL    0100-02254-02254-0900-00005-00005-00000-JJAEGER    11130000
         MACRO                                                          11140000
&LABEL   LLIHL &R1,&I2                                                  11150000
         DS    0H                                                       11160000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'0D'),Y(&I2.)                  11170000
         MEND                                                           11180000
./ ADD NAME=LLILH    0100-02254-02254-0900-00005-00005-00000-JJAEGER    11190000
         MACRO                                                          11200000
&LABEL   LLILH &R1,&I2                                                  11210000
         DS    0H                                                       11220000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'0E'),Y(&I2.)                  11230000
         MEND                                                           11240000
./ ADD NAME=LLILL    0100-02254-02254-0900-00005-00005-00000-JJAEGER    11250000
         MACRO                                                          11260000
&LABEL   LLILL &R1,&I2                                                  11270000
         DS    0H                                                       11280000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'0F'),Y(&I2.)                  11290000
         MEND                                                           11300000
./ ADD NAME=LMD      0100-02254-02254-0900-00005-00005-00000-JJAEGER    11310000
         MACRO                                                          11320000
&LABEL   LMD   &R1,&R3,&S2,&S4                                          11330000
         DS    0H                                                       11340000
&LABEL.  DC    0XL6'00',X'EF',AL.4(&R1.,&R3.),S(&S2.),S(&S4.)           11350000
         MEND                                                           11360000
./ ADD NAME=LMG      0100-02254-02254-0900-00005-00005-00000-JJAEGER    11370000
         MACRO                                                          11380000
&LABEL   LMG   &R1,&R3,&S2                                              11390000
         DS    0H                                                       11400000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'0004'           11410000
         MEND                                                           11420000
./ ADD NAME=LMH      0100-02254-02254-0900-00005-00005-00000-JJAEGER    11430000
         MACRO                                                          11440000
&LABEL   LMH   &R1,&R3,&S2                                              11450000
         DS    0H                                                       11460000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'0096'           11470000
         MEND                                                           11480000
./ ADD NAME=LNDBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    11490000
         MACRO                                                          11500000
&LABEL   LNDBR &R1,&R2                                                  11510000
         DS    0H                                                       11520000
&LABEL.  DC    0XL4'00',X'B31100',AL.4(&R1.,&R2.)                       11530000
         MEND                                                           11540000
./ ADD NAME=LNEBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    11550000
         MACRO                                                          11560000
&LABEL   LNEBR &R1,&R2                                                  11570000
         DS    0H                                                       11580000
&LABEL.  DC    0XL4'00',X'B30100',AL.4(&R1.,&R2.)                       11590000
         MEND                                                           11600000
./ ADD NAME=LNGFR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    11610000
         MACRO                                                          11620000
&LABEL   LNGFR &R1,&R2                                                  11630000
         DS    0H                                                       11640000
&LABEL.  DC    0XL4'00',X'B91100',AL.4(&R1.,&R2.)                       11650000
         MEND                                                           11660000
./ ADD NAME=LNGR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    11670000
         MACRO                                                          11680000
&LABEL   LNGR  &R1,&R2                                                  11690000
         DS    0H                                                       11700000
&LABEL.  DC    0XL4'00',X'B90100',AL.4(&R1.,&R2.)                       11710000
         MEND                                                           11720000
./ ADD NAME=LNXBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    11730000
         MACRO                                                          11740000
&LABEL   LNXBR &R1,&R2                                                  11750000
         DS    0H                                                       11760000
&LABEL.  DC    0XL4'00',X'B34100',AL.4(&R1.,&R2.)                       11770000
         MEND                                                           11780000
./ ADD NAME=LNXR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    11790000
         MACRO                                                          11800000
&LABEL   LNXR  &R1,&R2                                                  11810000
         DS    0H                                                       11820000
&LABEL.  DC    0XL4'00',X'B36100',AL.4(&R1.,&R2.)                       11830000
         MEND                                                           11840000
./ ADD NAME=LPDBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    11850000
         MACRO                                                          11860000
&LABEL   LPDBR &R1,&R2                                                  11870000
         DS    0H                                                       11880000
&LABEL.  DC    0XL4'00',X'B31000',AL.4(&R1.,&R2.)                       11890000
         MEND                                                           11900000
./ ADD NAME=LPEBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    11910000
         MACRO                                                          11920000
&LABEL   LPEBR &R1,&R2                                                  11930000
         DS    0H                                                       11940000
&LABEL.  DC    0XL4'00',X'B30000',AL.4(&R1.,&R2.)                       11950000
         MEND                                                           11960000
./ ADD NAME=LPGFR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    11970000
         MACRO                                                          11980000
&LABEL   LPGFR &R1,&R2                                                  11990000
         DS    0H                                                       12000000
&LABEL.  DC    0XL4'00',X'B91000',AL.4(&R1.,&R2.)                       12010000
         MEND                                                           12020000
./ ADD NAME=LPGR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    12030000
         MACRO                                                          12040000
&LABEL   LPGR  &R1,&R2                                                  12050000
         DS    0H                                                       12060000
&LABEL.  DC    0XL4'00',X'B90000',AL.4(&R1.,&R2.)                       12070000
         MEND                                                           12080000
./ ADD NAME=LPQ      0100-02254-02254-0900-00006-00006-00000-JJAEGER    12090000
         MACRO                                                          12100000
&LABEL   LPQ   &R1,&S2                                                  12110000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    12120000
         DS    0H                                                       12130000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'008F'              12140000
         MEND                                                           12150000
./ ADD NAME=LPSWE    0100-02254-02254-0900-00005-00005-00000-JJAEGER    12160000
         MACRO                                                          12170000
&LABEL   LPSWE &S2                                                      12180000
         DS    0H                                                       12190000
&LABEL.  DC    0XL4'00',X'B2B2',S(&S2.)                                 12200000
         MEND                                                           12210000
./ ADD NAME=LPXBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    12220000
         MACRO                                                          12230000
&LABEL   LPXBR &R1,&R2                                                  12240000
         DS    0H                                                       12250000
&LABEL.  DC    0XL4'00',X'B34000',AL.4(&R1.,&R2.)                       12260000
         MEND                                                           12270000
./ ADD NAME=LPXR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    12280000
         MACRO                                                          12290000
&LABEL   LPXR  &R1,&R2                                                  12300000
         DS    0H                                                       12310000
&LABEL.  DC    0XL4'00',X'B36000',AL.4(&R1.,&R2.)                       12320000
         MEND                                                           12330000
./ ADD NAME=LRAG     0100-02254-02254-0900-00006-00006-00000-JJAEGER    12340000
         MACRO                                                          12350000
&LABEL   LRAG  &R1,&S2                                                  12360000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    12370000
         DS    0H                                                       12380000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0003'              12390000
         MEND                                                           12400000
./ ADD NAME=LRV      0100-02254-02254-0900-00006-00006-00000-JJAEGER    12410000
         MACRO                                                          12420000
&LABEL   LRV   &R1,&S2                                                  12430000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    12440000
         DS    0H                                                       12450000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'001E'              12460000
         MEND                                                           12470000
./ ADD NAME=LRVG     0100-02254-02254-0900-00006-00006-00000-JJAEGER    12480000
         MACRO                                                          12490000
&LABEL   LRVG  &R1,&S2                                                  12500000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    12510000
         DS    0H                                                       12520000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'000F'              12530000
         MEND                                                           12540000
./ ADD NAME=LRVGR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    12550000
         MACRO                                                          12560000
&LABEL   LRVGR &R1,&R2                                                  12570000
         DS    0H                                                       12580000
&LABEL.  DC    0XL4'00',X'B90F00',AL.4(&R1.,&R2.)                       12590000
         MEND                                                           12600000
./ ADD NAME=LRVH     0100-02254-02254-0900-00006-00006-00000-JJAEGER    12610000
         MACRO                                                          12620000
&LABEL   LRVH  &R1,&S2                                                  12630000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    12640000
         DS    0H                                                       12650000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'001F'              12660000
         MEND                                                           12670000
./ ADD NAME=LRVR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    12680000
         MACRO                                                          12690000
&LABEL   LRVR  &R1,&R2                                                  12700000
         DS    0H                                                       12710000
&LABEL.  DC    0XL4'00',X'B91F00',AL.4(&R1.,&R2.)                       12720000
         MEND                                                           12730000
./ ADD NAME=LTDBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    12740000
         MACRO                                                          12750000
&LABEL   LTDBR &R1,&R2                                                  12760000
         DS    0H                                                       12770000
&LABEL.  DC    0XL4'00',X'B31200',AL.4(&R1.,&R2.)                       12780000
         MEND                                                           12790000
./ ADD NAME=LTEBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    12800000
         MACRO                                                          12810000
&LABEL   LTEBR &R1,&R2                                                  12820000
         DS    0H                                                       12830000
&LABEL.  DC    0XL4'00',X'B30200',AL.4(&R1.,&R2.)                       12840000
         MEND                                                           12850000
./ ADD NAME=LTGFR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    12860000
         MACRO                                                          12870000
&LABEL   LTGFR &R1,&R2                                                  12880000
         DS    0H                                                       12890000
&LABEL.  DC    0XL4'00',X'B91200',AL.4(&R1.,&R2.)                       12900000
         MEND                                                           12910000
./ ADD NAME=LTGR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    12920000
         MACRO                                                          12930000
&LABEL   LTGR  &R1,&R2                                                  12940000
         DS    0H                                                       12950000
&LABEL.  DC    0XL4'00',X'B90200',AL.4(&R1.,&R2.)                       12960000
         MEND                                                           12970000
./ ADD NAME=LTXBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    12980000
         MACRO                                                          12990000
&LABEL   LTXBR &R1,&R2                                                  13000000
         DS    0H                                                       13010000
&LABEL.  DC    0XL4'00',X'B34200',AL.4(&R1.,&R2.)                       13020000
         MEND                                                           13030000
./ ADD NAME=LTXR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    13040000
         MACRO                                                          13050000
&LABEL   LTXR  &R1,&R2                                                  13060000
         DS    0H                                                       13070000
&LABEL.  DC    0XL4'00',X'B36200',AL.4(&R1.,&R2.)                       13080000
         MEND                                                           13090000
./ ADD NAME=LURA     0100-02254-02254-0900-00005-00005-00000-JJAEGER    13100000
         MACRO                                                          13110000
&LABEL   LURA  &R1,&R2                                                  13120000
         DS    0H                                                       13130000
&LABEL.  DC    0XL4'00',X'B24B00',AL.4(&R1.,&R2.)                       13140000
         MEND                                                           13150000
./ ADD NAME=LURAG    0100-02254-02254-0900-00005-00005-00000-JJAEGER    13160000
         MACRO                                                          13170000
&LABEL   LURAG &R1,&R2                                                  13180000
         DS    0H                                                       13190000
&LABEL.  DC    0XL4'00',X'B90500',AL.4(&R1.,&R2.)                       13200000
         MEND                                                           13210000
./ ADD NAME=LXD      0100-02254-02254-0900-00006-00006-00000-JJAEGER    13220000
         MACRO                                                          13230000
&LABEL   LXD   &R1,&S2                                                  13240000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    13250000
         DS    0H                                                       13260000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0025'              13270000
         MEND                                                           13280000
./ ADD NAME=LXDR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    13290000
         MACRO                                                          13300000
&LABEL   LXDR  &R1,&R2                                                  13310000
         DS    0H                                                       13320000
&LABEL.  DC    0XL4'00',X'B32500',AL.4(&R1.,&R2.)                       13330000
         MEND                                                           13340000
./ ADD NAME=LXE      0100-02254-02254-0900-00006-00006-00000-JJAEGER    13350000
         MACRO                                                          13360000
&LABEL   LXE   &R1,&S2                                                  13370000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    13380000
         DS    0H                                                       13390000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0026'              13400000
         MEND                                                           13410000
./ ADD NAME=LXER     0100-02254-02254-0900-00005-00005-00000-JJAEGER    13420000
         MACRO                                                          13430000
&LABEL   LXER  &R1,&R2                                                  13440000
         DS    0H                                                       13450000
&LABEL.  DC    0XL4'00',X'B32600',AL.4(&R1.,&R2.)                       13460000
         MEND                                                           13470000
./ ADD NAME=LXR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    13480000
         MACRO                                                          13490000
&LABEL   LXR   &R1,&R2                                                  13500000
         DS    0H                                                       13510000
&LABEL.  DC    0XL4'00',X'B36500',AL.4(&R1.,&R2.)                       13520000
         MEND                                                           13530000
./ ADD NAME=LZDR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    13540000
         MACRO                                                          13550000
&LABEL   LZDR  &R1,&R2                                                  13560000
         DS    0H                                                       13570000
&LABEL.  DC    0XL4'00',X'B37500',AL.4(&R1.,&R2.)                       13580000
         MEND                                                           13590000
./ ADD NAME=LZER     0100-02254-02254-0900-00005-00005-00000-JJAEGER    13600000
         MACRO                                                          13610000
&LABEL   LZER  &R1,&R2                                                  13620000
         DS    0H                                                       13630000
&LABEL.  DC    0XL4'00',X'B37400',AL.4(&R1.,&R2.)                       13640000
         MEND                                                           13650000
./ ADD NAME=LZXR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    13660000
         MACRO                                                          13670000
&LABEL   LZXR  &R1,&R2                                                  13680000
         DS    0H                                                       13690000
&LABEL.  DC    0XL4'00',X'B37600',AL.4(&R1.,&R2.)                       13700000
         MEND                                                           13710000
./ ADD NAME=MDB      0100-02254-02254-0900-00006-00006-00000-JJAEGER    13720000
         MACRO                                                          13730000
&LABEL   MDB   &R1,&S2                                                  13740000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    13750000
         DS    0H                                                       13760000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'001C'              13770000
         MEND                                                           13780000
./ ADD NAME=MDBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    13790000
         MACRO                                                          13800000
&LABEL   MDBR  &R1,&R2                                                  13810000
         DS    0H                                                       13820000
&LABEL.  DC    0XL4'00',X'B31C00',AL.4(&R1.,&R2.)                       13830000
         MEND                                                           13840000
./ ADD NAME=MEE      0100-02254-02254-0900-00006-00006-00000-JJAEGER    13850000
         MACRO                                                          13860000
&LABEL   MEE   &R1,&S2                                                  13870000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    13880000
         DS    0H                                                       13890000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0037'              13900000
         MEND                                                           13910000
./ ADD NAME=MEEB     0100-02254-02254-0900-00006-00006-00000-JJAEGER    13920000
         MACRO                                                          13930000
&LABEL   MEEB  &R1,&S2                                                  13940000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    13950000
         DS    0H                                                       13960000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0017'              13970000
         MEND                                                           13980000
./ ADD NAME=MEEBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    13990000
         MACRO                                                          14000000
&LABEL   MEEBR &R1,&R2                                                  14010000
         DS    0H                                                       14020000
&LABEL.  DC    0XL4'00',X'B31700',AL.4(&R1.,&R2.)                       14030000
         MEND                                                           14040000
./ ADD NAME=MEER     0100-02254-02254-0900-00005-00005-00000-JJAEGER    14050000
         MACRO                                                          14060000
&LABEL   MEER  &R1,&R2                                                  14070000
         DS    0H                                                       14080000
&LABEL.  DC    0XL4'00',X'B33700',AL.4(&R1.,&R2.)                       14090000
         MEND                                                           14100000
./ ADD NAME=MGHI     0100-02254-02254-0900-00005-00005-00000-JJAEGER    14110000
         MACRO                                                          14120000
&LABEL   MGHI  &R1,&I2                                                  14130000
         DS    0H                                                       14140000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'0D'),Y(&I2.)                  14150000
         MEND                                                           14160000
./ ADD NAME=MHI      0100-02254-02254-0900-00005-00005-00000-JJAEGER    14170000
         MACRO                                                          14180000
&LABEL   MHI   &R1,&I2                                                  14190000
         DS    0H                                                       14200000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'0C'),Y(&I2.)                  14210000
         MEND                                                           14220000
./ ADD NAME=ML       0100-02254-02254-0900-00006-00006-00000-JJAEGER    14230000
         MACRO                                                          14240000
&LABEL   ML    &R1,&S2                                                  14250000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    14260000
         DS    0H                                                       14270000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0096'              14280000
         MEND                                                           14290000
./ ADD NAME=MLG      0100-02254-02254-0900-00006-00006-00000-JJAEGER    14300000
         MACRO                                                          14310000
&LABEL   MLG   &R1,&S2                                                  14320000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    14330000
         DS    0H                                                       14340000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0086'              14350000
         MEND                                                           14360000
./ ADD NAME=MLGR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    14370000
         MACRO                                                          14380000
&LABEL   MLGR  &R1,&R2                                                  14390000
         DS    0H                                                       14400000
&LABEL.  DC    0XL4'00',X'B98600',AL.4(&R1.,&R2.)                       14410000
         MEND                                                           14420000
./ ADD NAME=MLR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    14430000
         MACRO                                                          14440000
&LABEL   MLR   &R1,&R2                                                  14450000
         DS    0H                                                       14460000
&LABEL.  DC    0XL4'00',X'B99600',AL.4(&R1.,&R2.)                       14470000
         MEND                                                           14480000
./ ADD NAME=MS       0100-02254-02254-0900-00006-00006-00000-JJAEGER    14490000
         MACRO                                                          14500000
&LABEL   MS    &R1,&S2                                                  14510000
         MNOTE 4,'RX FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'     14520000
         DS    0H                                                       14530000
&LABEL.  DC    0XL4'00',X'71',AL.4(&R1.,0),S(&S2.)                      14540000
         MEND                                                           14550000
./ ADD NAME=MSCH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    14560000
         MACRO                                                          14570000
&LABEL   MSCH  &S2                                                      14580000
         DS    0H                                                       14590000
&LABEL.  DC    0XL4'00',X'B232',S(&S2.)                                 14600000
         MEND                                                           14610000
./ ADD NAME=MSG      0100-02254-02254-0900-00006-00006-00000-JJAEGER    14620000
         MACRO                                                          14630000
&LABEL   MSG   &R1,&S2                                                  14640000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    14650000
         DS    0H                                                       14660000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'000C'              14670000
         MEND                                                           14680000
./ ADD NAME=MSGF     0100-02254-02254-0900-00006-00006-00000-JJAEGER    14690000
         MACRO                                                          14700000
&LABEL   MSGF  &R1,&S2                                                  14710000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    14720000
         DS    0H                                                       14730000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'001C'              14740000
         MEND                                                           14750000
./ ADD NAME=MSGFR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    14760000
         MACRO                                                          14770000
&LABEL   MSGFR &R1,&R2                                                  14780000
         DS    0H                                                       14790000
&LABEL.  DC    0XL4'00',X'B91C00',AL.4(&R1.,&R2.)                       14800000
         MEND                                                           14810000
./ ADD NAME=MSGR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    14820000
         MACRO                                                          14830000
&LABEL   MSGR  &R1,&R2                                                  14840000
         DS    0H                                                       14850000
&LABEL.  DC    0XL4'00',X'B90C00',AL.4(&R1.,&R2.)                       14860000
         MEND                                                           14870000
./ ADD NAME=MSR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    14880000
         MACRO                                                          14890000
&LABEL   MSR   &R1,&R2                                                  14900000
         DS    0H                                                       14910000
&LABEL.  DC    0XL4'00',X'B25200',AL.4(&R1.,&R2.)                       14920000
         MEND                                                           14930000
./ ADD NAME=MSTA     0100-02254-02254-0900-00005-00005-00000-JJAEGER    14940000
         MACRO                                                          14950000
&LABEL   MSTA  &R1,&R2                                                  14960000
         DS    0H                                                       14970000
&LABEL.  DC    0XL4'00',X'B24700',AL.4(&R1.,&R2.)                       14980000
         MEND                                                           14990000
./ ADD NAME=MVCDK    0100-02254-02254-0900-00005-00005-00000-JJAEGER    15000000
         MACRO                                                          15010000
&LABEL   MVCDK &S1,&S2                                                  15020000
         DS    0H                                                       15030000
&LABEL.  DC    0XL6'00',X'E50F',S(&S1.),S(&S2.)                         15040000
         MEND                                                           15050000
./ ADD NAME=MVCLE    0100-02254-02254-0900-00005-00005-00000-JJAEGER    15060000
         MACRO                                                          15070000
&LABEL   MVCLE &R1,&R3,&S2                                              15080000
         DS    0H                                                       15090000
&LABEL.  DC    0XL4'00',X'A8',AL.4(&R1.,&R3.),S(&S2.)                   15100000
         MEND                                                           15110000
./ ADD NAME=MVCLU    0100-02254-02254-0900-00005-00005-00000-JJAEGER    15120000
         MACRO                                                          15130000
&LABEL   MVCLU &R1,&R3,&S2                                              15140000
         DS    0H                                                       15150000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'008E'           15160000
         MEND                                                           15170000
./ ADD NAME=MVCSK    0100-02254-02254-0900-00005-00005-00000-JJAEGER    15180000
         MACRO                                                          15190000
&LABEL   MVCSK &S1,&S2                                                  15200000
         DS    0H                                                       15210000
&LABEL.  DC    0XL6'00',X'E50E',S(&S1.),S(&S2.)                         15220000
         MEND                                                           15230000
./ ADD NAME=MVPG     0100-02254-02254-0900-00005-00005-00000-JJAEGER    15240000
         MACRO                                                          15250000
&LABEL   MVPG  &R1,&R2                                                  15260000
         DS    0H                                                       15270000
&LABEL.  DC    0XL4'00',X'B25400',AL.4(&R1.,&R2.)                       15280000
         MEND                                                           15290000
./ ADD NAME=MVST     0100-02254-02254-0900-00005-00005-00000-JJAEGER    15300000
         MACRO                                                          15310000
&LABEL   MVST  &R1,&R2                                                  15320000
         DS    0H                                                       15330000
&LABEL.  DC    0XL4'00',X'B25500',AL.4(&R1.,&R2.)                       15340000
         MEND                                                           15350000
./ ADD NAME=MXBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    15360000
         MACRO                                                          15370000
&LABEL   MXBR  &R1,&R2                                                  15380000
         DS    0H                                                       15390000
&LABEL.  DC    0XL4'00',X'B34C00',AL.4(&R1.,&R2.)                       15400000
         MEND                                                           15410000
./ ADD NAME=NG       0100-02254-02254-0900-00006-00006-00000-JJAEGER    15420000
         MACRO                                                          15430000
&LABEL   NG    &R1,&S2                                                  15440000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    15450000
         DS    0H                                                       15460000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0080'              15470000
         MEND                                                           15480000
./ ADD NAME=NGR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    15490000
         MACRO                                                          15500000
&LABEL   NGR   &R1,&R2                                                  15510000
         DS    0H                                                       15520000
&LABEL.  DC    0XL4'00',X'B98000',AL.4(&R1.,&R2.)                       15530000
         MEND                                                           15540000
./ ADD NAME=NIHH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    15550000
         MACRO                                                          15560000
&LABEL   NIHH  &R1,&I2                                                  15570000
         DS    0H                                                       15580000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'04'),Y(&I2.)                  15590000
         MEND                                                           15600000
./ ADD NAME=NIHL     0100-02254-02254-0900-00005-00005-00000-JJAEGER    15610000
         MACRO                                                          15620000
&LABEL   NIHL  &R1,&I2                                                  15630000
         DS    0H                                                       15640000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'05'),Y(&I2.)                  15650000
         MEND                                                           15660000
./ ADD NAME=NILH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    15670000
         MACRO                                                          15680000
&LABEL   NILH  &R1,&I2                                                  15690000
         DS    0H                                                       15700000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'06'),Y(&I2.)                  15710000
         MEND                                                           15720000
./ ADD NAME=NILL     0100-02254-02254-0900-00005-00005-00000-JJAEGER    15730000
         MACRO                                                          15740000
&LABEL   NILL  &R1,&I2                                                  15750000
         DS    0H                                                       15760000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'07'),Y(&I2.)                  15770000
         MEND                                                           15780000
./ ADD NAME=OG       0100-02254-02254-0900-00006-00006-00000-JJAEGER    15790000
         MACRO                                                          15800000
&LABEL   OG    &R1,&S2                                                  15810000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    15820000
         DS    0H                                                       15830000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0081'              15840000
         MEND                                                           15850000
./ ADD NAME=OGR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    15860000
         MACRO                                                          15870000
&LABEL   OGR   &R1,&R2                                                  15880000
         DS    0H                                                       15890000
&LABEL.  DC    0XL4'00',X'B98100',AL.4(&R1.,&R2.)                       15900000
         MEND                                                           15910000
./ ADD NAME=OIHH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    15920000
         MACRO                                                          15930000
&LABEL   OIHH  &R1,&I2                                                  15940000
         DS    0H                                                       15950000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'08'),Y(&I2.)                  15960000
         MEND                                                           15970000
./ ADD NAME=OIHL     0100-02254-02254-0900-00005-00005-00000-JJAEGER    15980000
         MACRO                                                          15990000
&LABEL   OIHL  &R1,&I2                                                  16000000
         DS    0H                                                       16010000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'09'),Y(&I2.)                  16020000
         MEND                                                           16030000
./ ADD NAME=OILH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    16040000
         MACRO                                                          16050000
&LABEL   OILH  &R1,&I2                                                  16060000
         DS    0H                                                       16070000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'0A'),Y(&I2.)                  16080000
         MEND                                                           16090000
./ ADD NAME=OILL     0100-02254-02254-0900-00005-00005-00000-JJAEGER    16100000
         MACRO                                                          16110000
&LABEL   OILL  &R1,&I2                                                  16120000
         DS    0H                                                       16130000
&LABEL.  DC    0XL4'00',X'A5',AL.4(&R1.,X'0B'),Y(&I2.)                  16140000
         MEND                                                           16150000
./ ADD NAME=PALB     0100-02254-02254-0900-00005-00005-00000-JJAEGER    16160000
         MACRO                                                          16170000
&LABEL   PALB  &R1,&R2                                                  16180000
         DS    0H                                                       16190000
&LABEL.  DC    0XL4'00',X'B24800',AL.4(&R1.,&R2.)                       16200000
         MEND                                                           16210000
./ ADD NAME=PGIN     0100-02254-02254-0900-00005-00005-00000-JJAEGER    16220000
         MACRO                                                          16230000
&LABEL   PGIN  &R1,&R2                                                  16240000
         DS    0H                                                       16250000
&LABEL.  DC    0XL4'00',X'B22E00',AL.4(&R1.,&R2.)                       16260000
         MEND                                                           16270000
./ ADD NAME=PGOUT    0100-02254-02254-0900-00005-00005-00000-JJAEGER    16280000
         MACRO                                                          16290000
&LABEL   PGOUT &R1,&R2                                                  16300000
         DS    0H                                                       16310000
&LABEL.  DC    0XL4'00',X'B22F00',AL.4(&R1.,&R2.)                       16320000
         MEND                                                           16330000
./ ADD NAME=PKA      0100-02254-02254-0900-00006-00006-00000-JJAEGER    16340000
         MACRO                                                          16350000
&LABEL   PKA                                                            16360000
         MNOTE 8,'SS_L FORMAT UNSUPPORTED'                              16370000
         DS    0H                                                       16380000
&LABEL.  DC    0XL2'00',X'0000'   E9                                    16390000
         MEND                                                           16400000
./ ADD NAME=PKU      0100-02254-02254-0900-00006-00006-00000-JJAEGER    16410000
         MACRO                                                          16420000
&LABEL   PKU                                                            16430000
         MNOTE 8,'SS_L FORMAT UNSUPPORTED'                              16440000
         DS    0H                                                       16450000
&LABEL.  DC    0XL2'00',X'0000'   E1                                    16460000
         MEND                                                           16470000
./ ADD NAME=PLO      0100-02254-02254-0900-00005-00005-00000-JJAEGER    16480000
         MACRO                                                          16490000
&LABEL   PLO   &R1,&S2,&R3,&S4                                          16500000
         DS    0H                                                       16510000
&LABEL.  DC    0XL6'00',X'EE',AL.4(&R1.,&R3.),S(&S2.),S(&S4.)           16520000
         MEND                                                           16530000
./ ADD NAME=PR       0100-02254-02254-0900-00005-00005-00000-JJAEGER    16540000
         MACRO                                                          16550000
&LABEL   PR                                                             16560000
         DS    0H                                                       16570000
&LABEL.  DC    0XL2'00',X'0101'                                         16580000
         MEND                                                           16590000
./ ADD NAME=RCHP     0100-02254-02254-0900-00005-00005-00000-JJAEGER    16600000
         MACRO                                                          16610000
&LABEL   RCHP  &S2                                                      16620000
         DS    0H                                                       16630000
&LABEL.  DC    0XL4'00',X'B23B',S(&S2.)                                 16640000
         MEND                                                           16650000
./ ADD NAME=RLL      0100-02254-02254-0900-00005-00005-00000-JJAEGER    16660000
         MACRO                                                          16670000
&LABEL   RLL   &R1,&R3,&S2                                              16680000
         DS    0H                                                       16690000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'001D'           16700000
         MEND                                                           16710000
./ ADD NAME=RLLG     0100-02254-02254-0900-00005-00005-00000-JJAEGER    16720000
         MACRO                                                          16730000
&LABEL   RLLG  &R1,&R3,&S2                                              16740000
         DS    0H                                                       16750000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'001C'           16760000
         MEND                                                           16770000
./ ADD NAME=RP       0100-02254-02254-0900-00005-00005-00000-JJAEGER    16780000
         MACRO                                                          16790000
&LABEL   RP    &S2                                                      16800000
         DS    0H                                                       16810000
&LABEL.  DC    0XL4'00',X'B277',S(&S2.)                                 16820000
         MEND                                                           16830000
./ ADD NAME=RSCH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    16840000
         MACRO                                                          16850000
&LABEL   RSCH  &S2                                                      16860000
         DS    0H                                                       16870000
&LABEL.  DC    0XL4'00',X'B238',S(&S2.)                                 16880000
         MEND                                                           16890000
./ ADD NAME=SACF     0100-02254-02254-0900-00005-00005-00000-JJAEGER    16900000
         MACRO                                                          16910000
&LABEL   SACF  &S2                                                      16920000
         DS    0H                                                       16930000
&LABEL.  DC    0XL4'00',X'B279',S(&S2.)                                 16940000
         MEND                                                           16950000
./ ADD NAME=SAL      0100-02254-02254-0900-00005-00005-00000-JJAEGER    16960000
         MACRO                                                          16970000
&LABEL   SAL   &S2                                                      16980000
         DS    0H                                                       16990000
&LABEL.  DC    0XL4'00',X'B237',S(&S2.)                                 17000000
         MEND                                                           17010000
./ ADD NAME=SAM24    0100-02254-02254-0900-00005-00005-00000-JJAEGER    17020000
         MACRO                                                          17030000
&LABEL   SAM24                                                          17040000
         DS    0H                                                       17050000
&LABEL.  DC    0XL2'00',X'010C'                                         17060000
         MEND                                                           17070000
./ ADD NAME=SAM31    0100-02254-02254-0900-00005-00005-00000-JJAEGER    17080000
         MACRO                                                          17090000
&LABEL   SAM31                                                          17100000
         DS    0H                                                       17110000
&LABEL.  DC    0XL2'00',X'010D'                                         17120000
         MEND                                                           17130000
./ ADD NAME=SAM64    0100-02254-02254-0900-00005-00005-00000-JJAEGER    17140000
         MACRO                                                          17150000
&LABEL   SAM64                                                          17160000
         DS    0H                                                       17170000
&LABEL.  DC    0XL2'00',X'010E'                                         17180000
         MEND                                                           17190000
./ ADD NAME=SAR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    17200000
         MACRO                                                          17210000
&LABEL   SAR   &R1,&R2                                                  17220000
         DS    0H                                                       17230000
&LABEL.  DC    0XL4'00',X'B24E00',AL.4(&R1.,&R2.)                       17240000
         MEND                                                           17250000
./ ADD NAME=SCHM     0100-02254-02254-0900-00005-00005-00000-JJAEGER    17260000
         MACRO                                                          17270000
&LABEL   SCHM  &S2                                                      17280000
         DS    0H                                                       17290000
&LABEL.  DC    0XL4'00',X'B23C',S(&S2.)                                 17300000
         MEND                                                           17310000
./ ADD NAME=SCKPF    0100-02254-02254-0900-00005-00005-00000-JJAEGER    17320000
         MACRO                                                          17330000
&LABEL   SCKPF                                                          17340000
         DS    0H                                                       17350000
&LABEL.  DC    0XL2'00',X'0107'                                         17360000
         MEND                                                           17370000
./ ADD NAME=SDB      0100-02254-02254-0900-00006-00006-00000-JJAEGER    17380000
         MACRO                                                          17390000
&LABEL   SDB   &R1,&S2                                                  17400000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    17410000
         DS    0H                                                       17420000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'001B'              17430000
         MEND                                                           17440000
./ ADD NAME=SDBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    17450000
         MACRO                                                          17460000
&LABEL   SDBR  &R1,&R2                                                  17470000
         DS    0H                                                       17480000
&LABEL.  DC    0XL4'00',X'B31B00',AL.4(&R1.,&R2.)                       17490000
         MEND                                                           17500000
./ ADD NAME=SEB      0100-02254-02254-0900-00006-00006-00000-JJAEGER    17510000
         MACRO                                                          17520000
&LABEL   SEB   &R1,&S2                                                  17530000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    17540000
         DS    0H                                                       17550000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'000B'              17560000
         MEND                                                           17570000
./ ADD NAME=SEBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    17580000
         MACRO                                                          17590000
&LABEL   SEBR  &R1,&R2                                                  17600000
         DS    0H                                                       17610000
&LABEL.  DC    0XL4'00',X'B30B00',AL.4(&R1.,&R2.)                       17620000
         MEND                                                           17630000
./ ADD NAME=SERVC    0100-02254-02254-0900-00005-00005-00000-JJAEGER    17640000
         MACRO                                                          17650000
&LABEL   SERVC &R1,&R2                                                  17660000
         DS    0H                                                       17670000
&LABEL.  DC    0XL4'00',X'B22000',AL.4(&R1.,&R2.)                       17680000
         MEND                                                           17690000
./ ADD NAME=SFPC     0100-02254-02254-0900-00005-00005-00000-JJAEGER    17700000
         MACRO                                                          17710000
&LABEL   SFPC  &R1,&R2                                                  17720000
         DS    0H                                                       17730000
&LABEL.  DC    0XL4'00',X'B38400',AL.4(&R1.,&R2.)                       17740000
         MEND                                                           17750000
./ ADD NAME=SG       0100-02254-02254-0900-00006-00006-00000-JJAEGER    17760000
         MACRO                                                          17770000
&LABEL   SG    &R1,&S2                                                  17780000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    17790000
         DS    0H                                                       17800000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0009'              17810000
         MEND                                                           17820000
./ ADD NAME=SGF      0100-02254-02254-0900-00006-00006-00000-JJAEGER    17830000
         MACRO                                                          17840000
&LABEL   SGF   &R1,&S2                                                  17850000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    17860000
         DS    0H                                                       17870000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0019'              17880000
         MEND                                                           17890000
./ ADD NAME=SGFR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    17900000
         MACRO                                                          17910000
&LABEL   SGFR  &R1,&R2                                                  17920000
         DS    0H                                                       17930000
&LABEL.  DC    0XL4'00',X'B91900',AL.4(&R1.,&R2.)                       17940000
         MEND                                                           17950000
./ ADD NAME=SGR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    17960000
         MACRO                                                          17970000
&LABEL   SGR   &R1,&R2                                                  17980000
         DS    0H                                                       17990000
&LABEL.  DC    0XL4'00',X'B90900',AL.4(&R1.,&R2.)                       18000000
         MEND                                                           18010000
./ ADD NAME=SIE      0100-02254-02254-0900-00005-00005-00000-JJAEGER    18020000
         MACRO                                                          18030000
&LABEL   SIE   &S2                                                      18040000
         DS    0H                                                       18050000
&LABEL.  DC    0XL4'00',X'B214',S(&S2.)                                 18060000
         MEND                                                           18070000
./ ADD NAME=SLAG     0100-02254-02254-0900-00005-00005-00000-JJAEGER    18080000
         MACRO                                                          18090000
&LABEL   SLAG  &R1,&R3,&S2                                              18100000
         DS    0H                                                       18110000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'000B'           18120000
         MEND                                                           18130000
./ ADD NAME=SLB      0100-02254-02254-0900-00006-00006-00000-JJAEGER    18140000
         MACRO                                                          18150000
&LABEL   SLB   &R1,&S2                                                  18160000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    18170000
         DS    0H                                                       18180000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0099'              18190000
         MEND                                                           18200000
./ ADD NAME=SLBG     0100-02254-02254-0900-00006-00006-00000-JJAEGER    18210000
         MACRO                                                          18220000
&LABEL   SLBG  &R1,&S2                                                  18230000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    18240000
         DS    0H                                                       18250000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0089'              18260000
         MEND                                                           18270000
./ ADD NAME=SLBGR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    18280000
         MACRO                                                          18290000
&LABEL   SLBGR &R1,&R2                                                  18300000
         DS    0H                                                       18310000
&LABEL.  DC    0XL4'00',X'B98900',AL.4(&R1.,&R2.)                       18320000
         MEND                                                           18330000
./ ADD NAME=SLBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    18340000
         MACRO                                                          18350000
&LABEL   SLBR  &R1,&R2                                                  18360000
         DS    0H                                                       18370000
&LABEL.  DC    0XL4'00',X'B99900',AL.4(&R1.,&R2.)                       18380000
         MEND                                                           18390000
./ ADD NAME=SLG      0100-02254-02254-0900-00006-00006-00000-JJAEGER    18400000
         MACRO                                                          18410000
&LABEL   SLG   &R1,&S2                                                  18420000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    18430000
         DS    0H                                                       18440000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'000B'              18450000
         MEND                                                           18460000
./ ADD NAME=SLGF     0100-02254-02254-0900-00006-00006-00000-JJAEGER    18470000
         MACRO                                                          18480000
&LABEL   SLGF  &R1,&S2                                                  18490000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    18500000
         DS    0H                                                       18510000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'001B'              18520000
         MEND                                                           18530000
./ ADD NAME=SLGFR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    18540000
         MACRO                                                          18550000
&LABEL   SLGFR &R1,&R2                                                  18560000
         DS    0H                                                       18570000
&LABEL.  DC    0XL4'00',X'B91B00',AL.4(&R1.,&R2.)                       18580000
         MEND                                                           18590000
./ ADD NAME=SLGR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    18600000
         MACRO                                                          18610000
&LABEL   SLGR  &R1,&R2                                                  18620000
         DS    0H                                                       18630000
&LABEL.  DC    0XL4'00',X'B90B00',AL.4(&R1.,&R2.)                       18640000
         MEND                                                           18650000
./ ADD NAME=SLLG     0100-02254-02254-0900-00005-00005-00000-JJAEGER    18660000
         MACRO                                                          18670000
&LABEL   SLLG  &R1,&R3,&S2                                              18680000
         DS    0H                                                       18690000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'000D'           18700000
         MEND                                                           18710000
./ ADD NAME=SQD      0100-02254-02254-0900-00006-00006-00000-JJAEGER    18720000
         MACRO                                                          18730000
&LABEL   SQD   &R1,&S2                                                  18740000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    18750000
         DS    0H                                                       18760000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0035'              18770000
         MEND                                                           18780000
./ ADD NAME=SQDB     0100-02254-02254-0900-00006-00006-00000-JJAEGER    18790000
         MACRO                                                          18800000
&LABEL   SQDB  &R1,&S2                                                  18810000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    18820000
         DS    0H                                                       18830000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0015'              18840000
         MEND                                                           18850000
./ ADD NAME=SQDBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    18860000
         MACRO                                                          18870000
&LABEL   SQDBR &R1,&R2                                                  18880000
         DS    0H                                                       18890000
&LABEL.  DC    0XL4'00',X'B31500',AL.4(&R1.,&R2.)                       18900000
         MEND                                                           18910000
./ ADD NAME=SQDR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    18920000
         MACRO                                                          18930000
&LABEL   SQDR  &R1,&R2                                                  18940000
         DS    0H                                                       18950000
&LABEL.  DC    0XL4'00',X'B24400',AL.4(&R1.,&R2.)                       18960000
         MEND                                                           18970000
./ ADD NAME=SQE      0100-02254-02254-0900-00006-00006-00000-JJAEGER    18980000
         MACRO                                                          18990000
&LABEL   SQE   &R1,&S2                                                  19000000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    19010000
         DS    0H                                                       19020000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0034'              19030000
         MEND                                                           19040000
./ ADD NAME=SQEB     0100-02254-02254-0900-00006-00006-00000-JJAEGER    19050000
         MACRO                                                          19060000
&LABEL   SQEB  &R1,&S2                                                  19070000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    19080000
         DS    0H                                                       19090000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0014'              19100000
         MEND                                                           19110000
./ ADD NAME=SQEBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    19120000
         MACRO                                                          19130000
&LABEL   SQEBR &R1,&R2                                                  19140000
         DS    0H                                                       19150000
&LABEL.  DC    0XL4'00',X'B31400',AL.4(&R1.,&R2.)                       19160000
         MEND                                                           19170000
./ ADD NAME=SQER     0100-02254-02254-0900-00005-00005-00000-JJAEGER    19180000
         MACRO                                                          19190000
&LABEL   SQER  &R1,&R2                                                  19200000
         DS    0H                                                       19210000
&LABEL.  DC    0XL4'00',X'B24500',AL.4(&R1.,&R2.)                       19220000
         MEND                                                           19230000
./ ADD NAME=SQXBR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    19240000
         MACRO                                                          19250000
&LABEL   SQXBR &R1,&R2                                                  19260000
         DS    0H                                                       19270000
&LABEL.  DC    0XL4'00',X'B31600',AL.4(&R1.,&R2.)                       19280000
         MEND                                                           19290000
./ ADD NAME=SQXR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    19300000
         MACRO                                                          19310000
&LABEL   SQXR  &R1,&R2                                                  19320000
         DS    0H                                                       19330000
&LABEL.  DC    0XL4'00',X'B33600',AL.4(&R1.,&R2.)                       19340000
         MEND                                                           19350000
./ ADD NAME=SRAG     0100-02254-02254-0900-00005-00005-00000-JJAEGER    19360000
         MACRO                                                          19370000
&LABEL   SRAG  &R1,&R3,&S2                                              19380000
         DS    0H                                                       19390000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'000A'           19400000
         MEND                                                           19410000
./ ADD NAME=SRLG     0100-02254-02254-0900-00005-00005-00000-JJAEGER    19420000
         MACRO                                                          19430000
&LABEL   SRLG  &R1,&R3,&S2                                              19440000
         DS    0H                                                       19450000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'000C'           19460000
         MEND                                                           19470000
./ ADD NAME=SRNM     0100-02254-02254-0900-00005-00005-00000-JJAEGER    19480000
         MACRO                                                          19490000
&LABEL   SRNM  &S2                                                      19500000
         DS    0H                                                       19510000
&LABEL.  DC    0XL4'00',X'B299',S(&S2.)                                 19520000
         MEND                                                           19530000
./ ADD NAME=SRST     0100-02254-02254-0900-00005-00005-00000-JJAEGER    19540000
         MACRO                                                          19550000
&LABEL   SRST  &R1,&R2                                                  19560000
         DS    0H                                                       19570000
&LABEL.  DC    0XL4'00',X'B25E00',AL.4(&R1.,&R2.)                       19580000
         MEND                                                           19590000
./ ADD NAME=SSCH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    19600000
         MACRO                                                          19610000
&LABEL   SSCH  &S2                                                      19620000
         DS    0H                                                       19630000
&LABEL.  DC    0XL4'00',X'B233',S(&S2.)                                 19640000
         MEND                                                           19650000
./ ADD NAME=STAM     0100-02254-02254-0900-00005-00005-00000-JJAEGER    19660000
         MACRO                                                          19670000
&LABEL   STAM  &R1,&R3,&S2                                              19680000
         DS    0H                                                       19690000
&LABEL.  DC    0XL4'00',X'9B',AL.4(&R1.,&R3.),S(&S2.)                   19700000
         MEND                                                           19710000
./ ADD NAME=STCKE    0100-02254-02254-0900-00005-00005-00000-JJAEGER    19720000
         MACRO                                                          19730000
&LABEL   STCKE &S2                                                      19740000
         DS    0H                                                       19750000
&LABEL.  DC    0XL4'00',X'B278',S(&S2.)                                 19760000
         MEND                                                           19770000
./ ADD NAME=STCMH    0100-02254-02254-0900-00005-00005-00000-JJAEGER    19780000
         MACRO                                                          19790000
&LABEL   STCMH &R1,&R3,&S2                                              19800000
         DS    0H                                                       19810000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'002C'           19820000
         MEND                                                           19830000
./ ADD NAME=STCPS    0100-02254-02254-0900-00005-00005-00000-JJAEGER    19840000
         MACRO                                                          19850000
&LABEL   STCPS &S2                                                      19860000
         DS    0H                                                       19870000
&LABEL.  DC    0XL4'00',X'B23A',S(&S2.)                                 19880000
         MEND                                                           19890000
./ ADD NAME=STCRW    0100-02254-02254-0900-00005-00005-00000-JJAEGER    19900000
         MACRO                                                          19910000
&LABEL   STCRW &S2                                                      19920000
         DS    0H                                                       19930000
&LABEL.  DC    0XL4'00',X'B239',S(&S2.)                                 19940000
         MEND                                                           19950000
./ ADD NAME=STCTG    0100-02254-02254-0900-00005-00005-00000-JJAEGER    19960000
         MACRO                                                          19970000
&LABEL   STCTG &R1,&R3,&S2                                              19980000
         DS    0H                                                       19990000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'0025'           20000000
         MEND                                                           20010000
./ ADD NAME=STFL     0100-02254-02254-0900-00005-00005-00000-JJAEGER    20020000
         MACRO                                                          20030000
&LABEL   STFL                                                           20040000
         DS    0H                                                       20050000
&LABEL.  DC    0XL4'00',X'B2B10000'                                     20060000
         MEND                                                           20070000
./ ADD NAME=STFPC    0100-02254-02254-0900-00005-00005-00000-JJAEGER    20080000
         MACRO                                                          20090000
&LABEL   STFPC &S2                                                      20100000
         DS    0H                                                       20110000
&LABEL.  DC    0XL4'00',X'B29C',S(&S2.)                                 20120000
         MEND                                                           20130000
./ ADD NAME=STG      0100-02254-02254-0900-00006-00006-00000-JJAEGER    20140000
         MACRO                                                          20150000
&LABEL   STG   &R1,&S2                                                  20160000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    20170000
         DS    0H                                                       20180000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0024'              20190000
         MEND                                                           20200000
./ ADD NAME=STMG     0100-02254-02254-0900-00005-00005-00000-JJAEGER    20210000
         MACRO                                                          20220000
&LABEL   STMG  &R1,&R3,&S2                                              20230000
         DS    0H                                                       20240000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'0024'           20250000
         MEND                                                           20260000
./ ADD NAME=STMH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    20270000
         MACRO                                                          20280000
&LABEL   STMH  &R1,&R3,&S2                                              20290000
         DS    0H                                                       20300000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'0026'           20310000
         MEND                                                           20320000
./ ADD NAME=STPQ     0100-02254-02254-0900-00006-00006-00000-JJAEGER    20330000
         MACRO                                                          20340000
&LABEL   STPQ  &R1,&S2                                                  20350000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    20360000
         DS    0H                                                       20370000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'008E'              20380000
         MEND                                                           20390000
./ ADD NAME=STRAG    0100-02254-02254-0900-00005-00005-00000-JJAEGER    20400000
         MACRO                                                          20410000
&LABEL   STRAG &S1,&S2                                                  20420000
         DS    0H                                                       20430000
&LABEL.  DC    0XL6'00',X'E502',S(&S1.),S(&S2.)                         20440000
         MEND                                                           20450000
./ ADD NAME=STRV     0100-02254-02254-0900-00006-00006-00000-JJAEGER    20460000
         MACRO                                                          20470000
&LABEL   STRV  &R1,&S2                                                  20480000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    20490000
         DS    0H                                                       20500000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'003E'              20510000
         MEND                                                           20520000
./ ADD NAME=STRVG    0100-02254-02254-0900-00006-00006-00000-JJAEGER    20530000
         MACRO                                                          20540000
&LABEL   STRVG &R1,&S2                                                  20550000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    20560000
         DS    0H                                                       20570000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'002F'              20580000
         MEND                                                           20590000
./ ADD NAME=STRVH    0100-02254-02254-0900-00006-00006-00000-JJAEGER    20600000
         MACRO                                                          20610000
&LABEL   STRVH &R1,&S2                                                  20620000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    20630000
         DS    0H                                                       20640000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'003F'              20650000
         MEND                                                           20660000
./ ADD NAME=STSCH    0100-02254-02254-0900-00005-00005-00000-JJAEGER    20670000
         MACRO                                                          20680000
&LABEL   STSCH &S2                                                      20690000
         DS    0H                                                       20700000
&LABEL.  DC    0XL4'00',X'B234',S(&S2.)                                 20710000
         MEND                                                           20720000
./ ADD NAME=STSI     0100-02254-02254-0900-00005-00005-00000-JJAEGER    20730000
         MACRO                                                          20740000
&LABEL   STSI  &S2                                                      20750000
         DS    0H                                                       20760000
&LABEL.  DC    0XL4'00',X'B27D',S(&S2.)                                 20770000
         MEND                                                           20780000
./ ADD NAME=STURA    0100-02254-02254-0900-00005-00005-00000-JJAEGER    20790000
         MACRO                                                          20800000
&LABEL   STURA &R1,&R2                                                  20810000
         DS    0H                                                       20820000
&LABEL.  DC    0XL4'00',X'B24600',AL.4(&R1.,&R2.)                       20830000
         MEND                                                           20840000
./ ADD NAME=STURG    0100-02254-02254-0900-00005-00005-00000-JJAEGER    20850000
         MACRO                                                          20860000
&LABEL   STURG &R1,&R2                                                  20870000
         DS    0H                                                       20880000
&LABEL.  DC    0XL4'00',X'B92500',AL.4(&R1.,&R2.)                       20890000
         MEND                                                           20900000
./ ADD NAME=SXBR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    20910000
         MACRO                                                          20920000
&LABEL   SXBR  &R1,&R2                                                  20930000
         DS    0H                                                       20940000
&LABEL.  DC    0XL4'00',X'B34B00',AL.4(&R1.,&R2.)                       20950000
         MEND                                                           20960000
./ ADD NAME=TAM      0100-02254-02254-0900-00005-00005-00000-JJAEGER    20970000
         MACRO                                                          20980000
&LABEL   TAM                                                            20990000
         DS    0H                                                       21000000
&LABEL.  DC    0XL2'00',X'010B'                                         21010000
         MEND                                                           21020000
./ ADD NAME=TAR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    21030000
         MACRO                                                          21040000
&LABEL   TAR   &R1,&R2                                                  21050000
         DS    0H                                                       21060000
&LABEL.  DC    0XL4'00',X'B24C00',AL.4(&R1.,&R2.)                       21070000
         MEND                                                           21080000
./ ADD NAME=TBDR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    21090000
         MACRO                                                          21100000
&LABEL   TBDR  &R1,&M3,&R2                                              21110000
         DS    0H                                                       21120000
&LABEL.  DC    0XL4'00',X'B351',AL.4(&M3.,0,&R1.,&R2.)                  21130000
         MEND                                                           21140000
./ ADD NAME=TBEDR    0100-02254-02254-0900-00005-00005-00000-JJAEGER    21150000
         MACRO                                                          21160000
&LABEL   TBEDR &R1,&M3,&R2                                              21170000
         DS    0H                                                       21180000
&LABEL.  DC    0XL4'00',X'B350',AL.4(&M3.,0,&R1.,&R2.)                  21190000
         MEND                                                           21200000
./ ADD NAME=TCDB     0100-02254-02254-0900-00006-00006-00000-JJAEGER    21210000
         MACRO                                                          21220000
&LABEL   TCDB  &R1,&S2                                                  21230000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    21240000
         DS    0H                                                       21250000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0011'              21260000
         MEND                                                           21270000
./ ADD NAME=TCEB     0100-02254-02254-0900-00006-00006-00000-JJAEGER    21280000
         MACRO                                                          21290000
&LABEL   TCEB  &R1,&S2                                                  21300000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    21310000
         DS    0H                                                       21320000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0010'              21330000
         MEND                                                           21340000
./ ADD NAME=TCXB     0100-02254-02254-0900-00006-00006-00000-JJAEGER    21350000
         MACRO                                                          21360000
&LABEL   TCXB  &R1,&S2                                                  21370000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    21380000
         DS    0H                                                       21390000
&LABEL.  DC    0XL6'00',X'ED',AL.4(&R1.,0),S(&S2.),X'0012'              21400000
         MEND                                                           21410000
./ ADD NAME=THDER    0100-02254-02254-0900-00005-00005-00000-JJAEGER    21420000
         MACRO                                                          21430000
&LABEL   THDER &R1,&R2                                                  21440000
         DS    0H                                                       21450000
&LABEL.  DC    0XL4'00',X'B35800',AL.4(&R1.,&R2.)                       21460000
         MEND                                                           21470000
./ ADD NAME=THDR     0100-02254-02254-0900-00005-00005-00000-JJAEGER    21480000
         MACRO                                                          21490000
&LABEL   THDR  &R1,&R2                                                  21500000
         DS    0H                                                       21510000
&LABEL.  DC    0XL4'00',X'B35900',AL.4(&R1.,&R2.)                       21520000
         MEND                                                           21530000
./ ADD NAME=TMH      0100-02254-02254-0900-00005-00005-00000-JJAEGER    21540000
         MACRO                                                          21550000
&LABEL   TMH   &R1,&I2                                                  21560000
         DS    0H                                                       21570000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'00'),Y(&I2.)                  21580000
         MEND                                                           21590000
./ ADD NAME=TMHH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    21600000
         MACRO                                                          21610000
&LABEL   TMHH  &R1,&I2                                                  21620000
         DS    0H                                                       21630000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'02'),Y(&I2.)                  21640000
         MEND                                                           21650000
./ ADD NAME=TMHL     0100-02254-02254-0900-00005-00005-00000-JJAEGER    21660000
         MACRO                                                          21670000
&LABEL   TMHL  &R1,&I2                                                  21680000
         DS    0H                                                       21690000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'03'),Y(&I2.)                  21700000
         MEND                                                           21710000
./ ADD NAME=TML      0100-02254-02254-0900-00005-00005-00000-JJAEGER    21720000
         MACRO                                                          21730000
&LABEL   TML   &R1,&I2                                                  21740000
         DS    0H                                                       21750000
&LABEL.  DC    0XL4'00',X'A7',AL.4(&R1.,X'01'),Y(&I2.)                  21760000
         MEND                                                           21770000
./ ADD NAME=TP       0100-02254-02254-0900-00006-00006-00000-JJAEGER    21780000
         MACRO                                                          21790000
&LABEL   TP                                                             21800000
         MNOTE 8,'RSL FORMAT UNSUPPORTED'                               21810000
         DS    0H                                                       21820000
&LABEL.  DC    0XL2'00',X'0000'   EBC0                                  21830000
         MEND                                                           21840000
./ ADD NAME=TPI      0100-02254-02254-0900-00005-00005-00000-JJAEGER    21850000
         MACRO                                                          21860000
&LABEL   TPI   &S2                                                      21870000
         DS    0H                                                       21880000
&LABEL.  DC    0XL4'00',X'B236',S(&S2.)                                 21890000
         MEND                                                           21900000
./ ADD NAME=TRACE    0100-02254-02254-0900-00005-00005-00000-JJAEGER    21910000
         MACRO                                                          21920000
&LABEL   TRACE &R1,&R3,&S2                                              21930000
         DS    0H                                                       21940000
&LABEL.  DC    0XL4'00',X'99',AL.4(&R1.,&R3.),S(&S2.)                   21950000
         MEND                                                           21960000
./ ADD NAME=TRACG    0100-02254-02254-0900-00005-00005-00000-JJAEGER    21970000
         MACRO                                                          21980000
&LABEL   TRACG &R1,&R3,&S2                                              21990000
         DS    0H                                                       22000000
&LABEL.  DC    0XL6'00',X'EB',AL.4(&R1.,&R3.),S(&S2.),X'000F'           22010000
         MEND                                                           22020000
./ ADD NAME=TRAP2    0100-02254-02254-0900-00005-00005-00000-JJAEGER    22030000
         MACRO                                                          22040000
&LABEL   TRAP2                                                          22050000
         DS    0H                                                       22060000
&LABEL.  DC    0XL2'00',X'01FF'                                         22070000
         MEND                                                           22080000
./ ADD NAME=TRAP4    0100-02254-02254-0900-00005-00005-00000-JJAEGER    22090000
         MACRO                                                          22100000
&LABEL   TRAP4 &S2                                                      22110000
         DS    0H                                                       22120000
&LABEL.  DC    0XL4'00',X'B2FF',S(&S2.)                                 22130000
         MEND                                                           22140000
./ ADD NAME=TRE      0100-02254-02254-0900-00005-00005-00000-JJAEGER    22150000
         MACRO                                                          22160000
&LABEL   TRE   &R1,&R2                                                  22170000
         DS    0H                                                       22180000
&LABEL.  DC    0XL4'00',X'B2A500',AL.4(&R1.,&R2.)                       22190000
         MEND                                                           22200000
./ ADD NAME=TROO     0100-02254-02254-0900-00005-00005-00000-JJAEGER    22210000
         MACRO                                                          22220000
&LABEL   TROO  &R1,&R2                                                  22230000
         DS    0H                                                       22240000
&LABEL.  DC    0XL4'00',X'B99300',AL.4(&R1.,&R2.)                       22250000
         MEND                                                           22260000
./ ADD NAME=TROT     0100-02254-02254-0900-00005-00005-00000-JJAEGER    22270000
         MACRO                                                          22280000
&LABEL   TROT  &R1,&R2                                                  22290000
         DS    0H                                                       22300000
&LABEL.  DC    0XL4'00',X'B99200',AL.4(&R1.,&R2.)                       22310000
         MEND                                                           22320000
./ ADD NAME=TRTO     0100-02254-02254-0900-00005-00005-00000-JJAEGER    22330000
         MACRO                                                          22340000
&LABEL   TRTO  &R1,&R2                                                  22350000
         DS    0H                                                       22360000
&LABEL.  DC    0XL4'00',X'B99100',AL.4(&R1.,&R2.)                       22370000
         MEND                                                           22380000
./ ADD NAME=TRTT     0100-02254-02254-0900-00005-00005-00000-JJAEGER    22390000
         MACRO                                                          22400000
&LABEL   TRTT  &R1,&R2                                                  22410000
         DS    0H                                                       22420000
&LABEL.  DC    0XL4'00',X'B99000',AL.4(&R1.,&R2.)                       22430000
         MEND                                                           22440000
./ ADD NAME=TSCH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    22450000
         MACRO                                                          22460000
&LABEL   TSCH  &S2                                                      22470000
         DS    0H                                                       22480000
&LABEL.  DC    0XL4'00',X'B235',S(&S2.)                                 22490000
         MEND                                                           22500000
./ ADD NAME=UNPKA    0100-02254-02254-0900-00006-00006-00000-JJAEGER    22510000
         MACRO                                                          22520000
&LABEL   UNPKA                                                          22530000
         MNOTE 8,'SS_L FORMAT UNSUPPORTED'                              22540000
         DS    0H                                                       22550000
&LABEL.  DC    0XL2'00',X'0000'   EA                                    22560000
         MEND                                                           22570000
./ ADD NAME=UNPKU    0100-02254-02254-0900-00006-00006-00000-JJAEGER    22580000
         MACRO                                                          22590000
&LABEL   UNPKU                                                          22600000
         MNOTE 8,'SS_L FORMAT UNSUPPORTED'                              22610000
         DS    0H                                                       22620000
&LABEL.  DC    0XL2'00',X'0000'   E2                                    22630000
         MEND                                                           22640000
./ ADD NAME=UPT      0100-02254-02254-0900-00005-00005-00000-JJAEGER    22650000
         MACRO                                                          22660000
&LABEL   UPT                                                            22670000
         DS    0H                                                       22680000
&LABEL.  DC    0XL2'00',X'0102'                                         22690000
         MEND                                                           22700000
./ ADD NAME=XG       0100-02254-02254-0900-00006-00006-00000-JJAEGER    22710000
         MACRO                                                          22720000
&LABEL   XG    &R1,&S2                                                  22730000
         MNOTE 4,'RXE FORMAT INCOMPLETE, NO INDEX REGISTER ASSIGNED'    22740000
         DS    0H                                                       22750000
&LABEL.  DC    0XL6'00',X'E3',AL.4(&R1.,0),S(&S2.),X'0082'              22760000
         MEND                                                           22770000
./ ADD NAME=XGR      0100-02254-02254-0900-00005-00005-00000-JJAEGER    22780000
         MACRO                                                          22790000
&LABEL   XGR   &R1,&R2                                                  22800000
         DS    0H                                                       22810000
&LABEL.  DC    0XL4'00',X'B98200',AL.4(&R1.,&R2.)                       22820000
         MEND                                                           22830000
./ ADD NAME=XSCH     0100-02254-02254-0900-00005-00005-00000-JJAEGER    22840000
         MACRO                                                          22850000
&LABEL   XSCH  &S2                                                      22860000
         DS    0H                                                       22870000
&LABEL.  DC    0XL4'00',X'B276',S(&S2.)                                 22880000
         MEND                                                           22890000
./ ENDUP                                                                22900000
/*                                                                      22910000
//                                                                      22920000
