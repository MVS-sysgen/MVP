//RLSE  JOB (TSO),
//             'Install RLSE',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//* ********************************************************
//* *  INSTALL THE 'RELEASE' TSO COMMAND    (RLSE)         *
//* ********************************************************
//RELEASE EXEC ASMFCL,
//        PARM.ASM='OBJ,NODECK,TERM',
//        PARM.LKED='LIST,MAP,RENT,REUS,REFR'
//ASM.SYSPRINT DD SYSOUT=D
//ASM.SYSTERM  DD SYSOUT=*
//ASM.SYSIN    DD *
         TITLE '   R E L E A S E '                                      01244460
*********************************************************************** 01244470
*                                                                     * 01244480
*        'RELEASE' TSO COMMAND                                        * 01244490
*                                                                     * 01244500
*********************************************************************** 01244510
         SPACE                                                          01244520
*        WRITTEN BY. BILL GODFREY, PRC (PLANNING RESEARCH CORPORATION). 01244530
*        INSTALLATION. PRC, MCLEAN VA.                                  01244540
*        DATE WRITTEN. NOVEMBER 22 1978.                                01244550
*        DATE UPDATED. AUGUST 3 1981.                                   01244560
*        ATTRIBUTES. RE-ENTRANT.                                        01244570
*        DESCRIPTION.                                                   01244580
*            THIS TSO COMMAND RELEASES UNUSED SPACE FROM SEQUENTIAL     01244590
*            OR PARTITIONED DATA SETS.                                  01244600
*                                                                       01244610
*            THE 'LEAVE(NN)' KEYWORD ALLOWS THE USER TO SPECIFY         01244620
*            HOW MANY UNUSED TRACKS NOT TO RELEASE.                     01244630
*                                                                       01244640
*            THE 'EXTENT' KEYWORD ALLOWS THE USER TO RELEASE ONLY       01244650
*            UNUSED EXTENTS, LEAVING THE PRIMARY EXTENT INTACT.         01244660
*                                                                       01244670
*            24MAY79 - OBTAIN ADDED FOR CHECKING CYLINDER ALLOC.        01244680
*            24MAY79 - STACK DELETE ADDED.                              01244690
*            09JUN80 - FIX LINK TO STACK.                               01244700
*            31JUL81 - FOLLOWING PROBLEM REPORTED.                      01244710
*                      FIRST BYTE OF EXTENT DESCRIPTION IN F1-DSCB      01244720
*                      FOR CYLINDER ALLOCATED DATA SET IS BEING CHANGED 01244730
*                      FROM X'81' TO X'01'.                             01244740
*            03AUG81 - ABOVE PROBLEM FIXED. CAUSE WAS DETERMINED TO BE  01244750
*                      CYLINDER BITS IN JFCB NOT BEING RECOGNIZED BY    01244760
*                      MODULE IGG020P1, SINCE JFCB REWRITE WAS BEING    01244770
*                      INHIBITED.  REMOVED JFCB REWRITE INHIBIT,        01244780
*                      REMOVED JFCB DISP=MOD ALTERATION, CHANGED ALL    01244790
*                      OPEN-OUTPUT TO OPEN-EXTEND.  THUS, THE ONLY      01244800
*                      JFCB ALTERATIONS ARE FOR CYLINDER ALLOCATION     01244810
*                      AND RLSE, (AND RLSE IS SET OFF AGAIN IF THERE    01244820
*                      WAS ANY UNUSED SPACE LEFT UNRELEASED).           01244830
*            03AUG81 - CHANGE LOGIC SO THAT ONLY ONE DCB IS EVER OPEN   01244840
*                      AT ANY TIME.                                     01244850
         SPACE                                                          01244860
*              INTERCEPT 'LINK' MACROS IMBEDDED IN PUTLINE & STACK      01244870
*              TO MAKE SF=(E,LINKAREA) THE DEFAULT                      01244880
         SPACE                                                          01244890
         MACRO                                                          01244900
&NAME    LINK  &EP=,&SF=(E,LINKAREA)                                    01244910
&NAME    LA    15,&SF(2)                                                01244920
         LA    0,*+8                                                    01244930
         B     *+12                                                     01244940
         DC    CL8'&EP'                                                 01244950
         ST    0,0(0,15)                                                01244960
         SVC   6             ISSUE LINK SVC                             01244970
         MEND                                                           01244980
         SPACE                                                          01244990
RELEASE  START                                                          01245000
         USING *,R12                                                    01245010
         B     @PROLOG-*(,15)                                           01245020
         DC    AL1(11),CL11'RELEASE'                                    01245030
         DC    CL16' &SYSDATE &SYSTIME '                                01245040
@SIZE    DC    AL1(1),AL3(@DATAL)                                       01245050
@PROLOG  STM   14,12,12(13)                                             01245060
         LR    R12,15                                                   01245070
         LR    R2,R1                                                    01245080
         USING CPPL,R2                                                  01245090
         L     R0,@SIZE                                                 01245100
         GETMAIN R,LV=(0)                                               01245110
         LR    R9,R1                                                    01245120
         USING @DATA,R9                                                 01245130
         SPACE                                                          01245140
         LR    R0,R1               AREA TO BE CLEARED                   01245150
         L     R1,@SIZE            LENGTH TO BE CLEARED                 01245160
         SLR   R14,R14             ZERO SENDING ADDRESS                 01245170
         SLR   R15,R15             ZERO PAD AND LENGTH                  01245180
         MVCL  R0,R14              CLEAR IT ALL                         01245190
         SPACE                                                          01245200
         ST    R13,4(,R9)          PUT OLD ADDRESS IN NEW AREA          01245210
         ST    R9,8(,R13)          PUT NEW ADDRESS IN OLD AREA          01245220
         LR    R13,R9              SET SAVEAREA POINTER                 01245230
         SPACE                                                          01245240
         ST    R2,SAVE2                                                 01245250
         EJECT                                                          01245260
************************************************************            01245270
*                                                          *            01245280
*        SET UP IOPL FOR PUTLINE                           *            01245290
*                                                          *            01245300
************************************************************            01245310
         SPACE                                                          01245320
         LA    R15,MYIOPL                                               01245330
         USING IOPL,R15                                                 01245340
         MVC   IOPLUPT(4),CPPLUPT                                       01245350
         MVC   IOPLECT(4),CPPLECT                                       01245360
         LA    R0,MYECB                                                 01245370
         ST    R0,IOPLECB                                               01245380
         XC    MYECB,MYECB                                              01245390
         LA    R0,MYPTPB                                                01245400
         ST    R0,IOPLIOPB                                              01245410
         DROP  R15                                                      01245420
         SPACE                                                          01245430
         L     R15,16              LOAD CVT POINTER                     01245440
         TM    444(R15),X'80'      IS PUTLINE LOADED? (VS2)             01245450
         BNO   PUTLOAD             NO - BRANCH TO LOAD                  01245460
         L     R15,444(,R15)       YES - USE CVTPUTL                    01245470
         B     PUTLODED            BRANCH AROUND LOAD                   01245480
PUTLOAD  LA    R0,=CL8'IKJPUTL '                                        01245490
         LOAD  EPLOC=(0)                                                01245500
         LR    R15,R0              GET ENTRY ADDRESS                    01245510
         LA    R15,0(,R15)         CLEAR HI BYTE FOR DELETE ROUTINE     01245520
PUTLODED ST    R15,MYPUTLEP        SAVE PUTLINE ENTRY ADDRESS           01245530
         SPACE                                                          01245540
************************************************************            01245550
*                                                          *            01245560
*        SET UP PPL FOR PARSE                              *            01245570
*                                                          *            01245580
************************************************************            01245590
         SPACE                                                          01245600
         LA    R15,MYPPL                                                01245610
         USING PPL,R15                                                  01245620
         MVC   PPLUPT(4),CPPLUPT                                        01245630
         MVC   PPLECT(4),CPPLECT                                        01245640
         LA    R0,MYECB                                                 01245650
         ST    R0,PPLECB                                                01245660
         XC    MYECB,MYECB                                              01245670
*        L     R0,=A(RELEAPCL)                                          01245680
         LA    R0,PCLADDR                                               01245690
         ST    R0,PPLPCL                                                01245700
         LA    R0,MYANS                                                 01245710
         ST    R0,PPLANS                                                01245720
         XC    MYANS(4),MYANS                                           01245730
         MVC   PPLCBUF(4),CPPLCBUF                                      01245740
         ST    R9,PPLUWA                                                01245750
         DROP  R15                                                      01245760
         SPACE 1                                                        01245770
************************************************************            01245780
*                                                          *            01245790
*        CALL THE PARSE SERVICE ROUTINE                    *            01245800
*                                                          *            01245810
************************************************************            01245820
         SPACE 1                                                        01245830
         LR    R1,R15              POINT TO PPL                         01245840
         L     R15,16              CVTPTR                               01245850
         TM    524(R15),X'80'      IF HI ORDER BIT NOT ON               01245860
         BNO   PARSELNK               THEN DO LINK, NOT CALL            01245870
         L     R15,524(,R15)       CVTPARS                              01245880
         BALR  R14,R15             CALL IKJPARS                         01245890
         B     PARSEEXT            SKIP AROUND LINK                     01245900
PARSELNK EQU   *                                                        01245910
         LINK  EP=IKJPARS,SF=(E,LINKAREA)                               01245920
PARSEEXT EQU   *                                                        01245930
         SPACE 1                                                        01245940
         LTR   R15,R15                                                  01245950
         BZ    PARSEOK                                                  01245960
         LA    R1,MSG01                                                 01245970
         LA    R0,L'MSG01                                               01245980
         BAL   R14,PUTMSG                                               01245990
         OI    STATUS,STATERR      ERROR                                01246000
         B     EXIT                                                     01246010
PARSEOK  EQU   *                                                        01246020
         SPACE                                                          01246030
         L     R3,MYANS                                                 01246040
         USING IKJPARMD,R3                                              01246050
         SPACE                                                          01246060
************************************************************            01246070
*                                                          *            01246080
*        PROCESS 'LEAVE' KEYWORD                           *            01246090
*                                                          *            01246100
************************************************************            01246110
         SPACE                                                          01246120
         LA    R4,LEAVE                                                 01246130
         TM    6(R4),X'80'                                              01246140
         BZ    NOLEAVE                                                  01246150
         LH    R1,4(,R4)                                                01246160
         L     R14,0(,R4)                                               01246170
         BCTR  R1,0                                                     01246180
         B     *+10                                                     01246190
         PACK  DOUBLE(8),0(0,R14)                                       01246200
         EX    R1,*-6                                                   01246210
         CVB   R1,DOUBLE                                                01246220
         ST    R1,LEAVV                                                 01246230
NOLEAVE  EQU   *                                                        01246240
         SPACE                                                          01246250
************************************************************            01246260
*                                                          *            01246270
*        QUALIFY THE DSNAME IF NECESSARY                   *            01246280
*                                                          *            01246290
************************************************************            01246300
         SPACE                                                          01246310
         LA    R4,DSN                                                   01246320
STARTDSN NI    STATUS,255-STATCYL  SET OFF CYLINDER ALLOC SW            01246330
         SLR   R7,R7                                                    01246340
         BCTR  R7,0                                                     01246350
         TM    6(R4),X'80'         IS DATASET NAME SPECIFIED?           01246360
         BO    OKDSN               YES - BRANCH                         01246370
         LA    R1,MSG05            NO - JUST MEMBER NAME                01246380
         LA    R0,L'MSG05                                               01246390
         BAL   R14,PUTMSG                                               01246400
         OI    STATUS,STATERR      ERROR                                01246410
         B     NEXTDSN                                                  01246420
OKDSN    EQU   *                                                        01246430
         MVI   DSNAME,C' '                                              01246440
         MVC   DSNAME+1(45),DSNAME                                      01246450
         LH    R1,4(,R4)           GET LENGTH                           01246460
         STH   R1,DSNAME                                                01246470
         L     R14,0(,R4)          GET ADDRESS OF NAME                  01246480
         BCTR  R1,0                                                     01246490
         B     *+10                                                     01246500
         MVC   DSNAME+2(0),0(R14)                                       01246510
         EX    R1,*-6                                                   01246520
         MVC   DSNSAVE(46),DSNAME  SAVE DSN AS ENTERED                  01246530
         TM    6(R4),X'40'         IS IT QUOTED?                        01246540
         BO    DEFX                YES - SKIP DEFAULT SERVICE           01246550
         SPACE                                                          01246560
         LA    R15,MYIOPL                                               01246570
         USING IOPL,R15                                                 01246580
         LA    R14,MYDFPB                                               01246590
         ST    R14,IOPLIOPB                                             01246600
         USING DFPB,R14                                                 01246610
         XC    0(20,R14),0(R14)                                         01246620
         LA    R0,DSNAME                                                01246630
         ST    R0,DFPBDSN                                               01246640
         OI    DFPBCODE,X'04'      SEARCH CAT AND PROMPT IF MULTI       01246650
         MVC   DFPBPSCB,CPPLPSCB                                        01246660
         MVI   DFPBCNTL,X'20'      PREFIX THE DSNAME                    01246670
         DROP  R14                                                      01246680
         SPACE                                                          01246690
         LA    R1,MYIOPL                                                01246700
         SPACE                                                          01246710
         LR    R1,R15              POINT TO IOPL                        01246720
         L     R15,16              CVTPTR                               01246730
         TM    736(R15),X'80'      IF HI ORDER BIT NOT ON               01246740
         BNO   EHDEFLNK               THEN DO LINK, NOT CALL            01246750
         L     R15,736(,R15)       CVTEHDEF                             01246760
         BALR  R14,R15             CALL IKJEHDEF                        01246770
         B     EHDEFEXT            SKIP AROUND LINK                     01246780
EHDEFLNK EQU   *                                                        01246790
         LINK  EP=IKJEHDEF,SF=(E,LINKAREA)                              01246800
EHDEFEXT EQU   *                                                        01246810
         SPACE                                                          01246820
         B     DEFCODE(R15)                                             01246830
DEFCODE  B     DEF00               SUCCESS                              01246840
         B     NEXTDSN              MSG ALREADY ISSUED                  01246850
         B     DEF08               INVALID NAME GT 44                   01246860
         B     NEXTDSN              MSG ALREADY ISUED                   01246870
         B     DEF16               NOT IN CATALOG                       01246880
         B     DEF20               NOT IN CATALOG                       01246890
         B     DEF24               IMPOSSIBLE                           01246900
         B     DEF28               COMMAND SYSTEM ERROR                 01246910
         B     DEF32               IMPOSSIBLE                           01246920
         B     DEF36               ?                                    01246930
DEF08    EQU   *                                                        01246940
DEF16    EQU   *                                                        01246950
         B     DEF24                                                    01246960
DEF20    EQU   *                                                        01246970
LOCERR   EQU   *                                                        01246980
         MVC   MSGWK(L'MSG02),MSG02                                     01246990
         LA    R15,MSGWK+L'MSG02                                        01247000
         LA    R14,DSNAME                                               01247010
         LH    R1,0(,R14)                                               01247020
         BCTR  R1,0                                                     01247030
         B     *+10                                                     01247040
         MVC   MSGWK+L'MSG02(0),2(R14)                                  01247050
         EX    R1,*-6                                                   01247060
         LA    R15,1(R1,R15)                                            01247070
         MVC   0(L'MSG02A,R15),MSG02A                                   01247080
         LA    R0,L'MSG02+L'MSG02A+1(,R1)                               01247090
         LA    R1,MSGWK                                                 01247100
         BAL   R14,PUTMSG                                               01247110
         B     NEXTDSN                                                  01247120
DEF24    EQU   *                                                        01247130
DEF28    EQU   *                                                        01247140
DEF32    EQU   *                                                        01247150
DEF36    EQU   *                                                        01247160
         LA    R1,MSG03                                                 01247170
         LA    R0,L'MSG03                                               01247180
         BAL   R14,PUTMSG                                               01247190
         B     NEXTDSN                                                  01247200
         SPACE                                                          01247210
DEF00    EQU   *                                                        01247220
DEFX     EQU   *                                                        01247230
         SPACE                                                          01247240
************************************************************            01247250
*                                                          *            01247260
*        ALLOCATE THE DATASET                              *            01247270
*                                                          *            01247280
************************************************************            01247290
         SPACE                                                          01247300
         LA    R1,MYDAPL                                                01247310
         USING DAPL,R1                                                  01247320
         MVC   DAPLUPT(4),CPPLUPT                                       01247330
         MVC   DAPLECT(4),CPPLECT                                       01247340
         LA    R0,MYECB                                                 01247350
         ST    R0,DAPLECB                                               01247360
         MVC   DAPLPSCB(4),CPPLPSCB                                     01247370
         LA    R15,MYDAPB                                               01247380
         ST    R15,DAPLDAPB                                             01247390
         DROP  R1                                                       01247400
         USING DAPB08,R15                                               01247410
         XC    0(84,R15),0(R15)                                         01247420
         MVI   DA08CD+1,X'08'                                           01247430
         LA    R0,DSNAME                                                01247440
         ST    R0,DA08PDSN                                              01247450
         MVC   DA08DDN(8),=CL8' '                                       01247460
         MVC   DA08UNIT,=CL8' '                                         01247470
         MVC   DA08SER,=CL8' '                                          01247480
         MVC   DA08MNM,=CL8' '                                          01247490
         MVC   DA08PSWD,=CL8' '                                         01247500
         MVI   DA08DSP1,DA08OLD                                         01247510
*        MVI   DA08DSP1,DA08MOD                               .02AUG81. 01247520
         MVI   DA08DPS2,DA08KEEP                                        01247530
         MVI   DA08DPS3,DA08KEP                                         01247540
         TM    14(R4),X'80'        MEMBER SPECIFIED?                    01247550
         BZ    MEMBX               NO - BRANCH                          01247560
         LA    R1,MSG05                                                 01247570
         LA    R0,L'MSG05                                               01247580
         BAL   R14,PUTMSG                                               01247590
         B     NEXTDSN                                                  01247600
MEMBX    EQU   *                                                        01247610
         TM    22(R4),X'80'        PASSWORD SPECIFIED?                  01247620
         BZ    PASSX               NO - BRANCH                          01247630
         LH    R1,20(,R4)          GET LENGTH OF PSWD                   01247640
         BCTR  R1,0                MINUS 1 FOR EX                       01247650
         L     R14,16(,R4)         GET ADDRESS OF PSWD                  01247660
         B     *+10                                                     01247670
         MVC   DA08PSWD(0),0(R14)  MOVE PSWD                            01247680
         EX    R1,*-6                                                   01247690
PASSX    EQU   *                                                        01247700
         LA    R1,MYDAPL                                                01247710
         SPACE                                                          01247720
         BAL   R14,CALLDAIR                                             01247730
         LTR   R15,R15                                                  01247740
         BZ    OKDAIR                                                   01247750
         BAL   R14,DAIRFAIL                                             01247760
         OI    STATUS,STATERR      ERROR                                01247770
         B     NEXTDSN                                                  01247780
OKDAIR   EQU   *                                                        01247790
         OI    CLEANUP,CLEANDAR    TELL CLEANUP TO FREE IT              01247800
         LA    R15,MYDAPB                                               01247810
         MVC   DDSAVE,DA08DDN                                           01247820
         TM    DA08DSO,X'42'       IS DSORG PS OR PO                    01247830
         BNZ   OKDSORG             YES - BRANCH                         01247840
ERRDSORG LA    R1,MSG06            MUST BE PS OR PO                     01247850
         LA    R0,L'MSG06                                               01247860
         BAL   R14,PUTMSG                                               01247870
         OI    STATUS,STATERR      ERROR                                01247880
         B     NEXTDSN                                                  01247890
OKDSORG  EQU   *                                                        01247900
         DROP  R15                                                      01247910
         SPACE                                                          01247920
         DEVTYPE DDSAVE,DEVAREA,DEVTAB                                  01247930
         SPACE                                                          01247940
         CLI   DEVAREA+2,X'20'     DIRECT ACCESS DEVICE                 01247950
         BE    OKDA                YES, BRANCH                          01247960
         LA    R1,MSG09                                                 01247970
         LA    R0,L'MSG09                                               01247980
         BAL   R14,PUTMSG                                               01247990
         OI    STATUS,STATERR      ERROR                                01248000
         B     NEXTDSN                                                  01248010
OKDA     EQU   *                                                        01248020
         SPACE                                                          01248030
************************************************************            01248040
*                                                          *            01248050
*        CHECK FORMAT-1 DSCB FOR CYLINDER ALLOCATION       *            01248060
*                                                          *            01248070
************************************************************            01248080
         SPACE                                                          01248090
         L     R1,16               CVTPTR                               01248100
         L     R1,0(,R1)           TCB WORDS                            01248110
         L     R1,4(,R1)           CURRENT TCB                          01248120
         L     R1,12(,R1)          TIOT                                 01248130
         LA    R1,24(,R1)          TOIENTRY                             01248140
DDLOOP   CLI   0(R1),0             END OF TIOT                          01248150
         BE    ERROBT              YES, BRANCH (NEVER HAPPENS)          01248160
         CLC   4(8,R1),DDSAVE      DOES DDNAME MATCH                    01248170
         BE    DDFOUND                                                  01248180
         SLR   R15,R15                                                  01248190
         IC    R15,0(,R1)                                               01248200
         LA    R1,0(R15,R1)                                             01248210
         B     DDLOOP                                                   01248220
DDFOUND  L     R15,16(,R1)         TIOEFSRT-1                           01248230
         TM    18(R15),X'20'       DIRECT ACCESS DEVICE?                01248240
         BZ    ERROBT              NO, BYPASS OBTAIN                    01248250
         MVC   VOLSER,28(R15)      UCBVOLI                              01248260
OBTDSCB  LA    R1,OBTAINW                                               01248270
         MVC   0(OBTAINL,R1),OBTAIN                                     01248280
         LA    R0,DSNAME+2         DSN FOR OBTAIN                       01248290
         ST    R0,4(,R1)                                                01248300
         LA    R0,VOLSER           VOLUME FOR OBTAIN                    01248310
         ST    R0,8(,R1)                                                01248320
         LA    R0,MYDSCB           ANSWER AREA FOR OBTAIN               01248330
         ST    R0,12(,R1)                                               01248340
         OBTAIN (1)                                                     01248350
         LTR   R15,R15             WAS OBTAIN SUCCESSFUL                01248360
         BZ    OKDSCB              YES, BRANCH                          01248370
         SPACE                                                          01248380
*               OBTAIN HAS FAILED. HOW CAN THAT HAPPEN WHEN             01248390
*               DYNAMIC ALLOCATION WAS SUCCESSFUL? ONE WAY IT           01248400
*               CAN HAPPEN IS IF THE DSNAME IS AN ALIAS ENTRY           01248410
*               IN A VSAM CATALOG.  IF IT IS, A 'LOCATE' WILL           01248420
*               PUT THE TRUE NAME IN THE DSNAME FIELD, SO NOW           01248430
*               WE ISSUE A LOCATE, AND TRY THE OBTAIN AGAIN.            01248440
         SPACE                                                          01248450
         TM    STATUS,STATLOC      HAS LOCATE BEEN TRIED ALREADY?       01248460
         BZ    ALIAS               NO, GO TRY IT                        01248470
ERROBT   LA    R1,MSG10            UNABLE TO OBTAIN DSCB                01248480
         LA    R0,L'MSG10                                               01248490
         BAL   R14,PUTMSG                                               01248500
         OI    STATUS,STATERR      ERROR                                01248510
         B     NEXTDSN                                                  01248520
ALIAS    OI    STATUS,STATLOC      TRIP THE SWITCH                      01248530
         LA    R1,LOCATEW                                               01248540
         MVC   0(LOCATEL,R1),LOCATE                                     01248550
         LA    R0,DSNAME+2         DSNAME FOR LOCATE                    01248560
         ST    R0,4(,R1)                                                01248570
         LA    R0,LOCBUF           ANSWER AREA FOR LOCATE               01248580
         ST    R0,12(,R1)                                               01248590
         LOCATE (1)                                                     01248600
         LTR   15,15               WAS LOCATE SUCCESSFUL?               01248610
         BZ    OBTDSCB             YES, GO OBTAIN AGAIN                 01248620
         B     ERROBT              NO, ISSUE MESSAGE                    01248630
         SPACE                                                          01248640
OKDSCB   NI    STATUS,255-STATLOC  TURN OFF LOCATE SWITCH               01248650
         LA    R1,MYDSCB-44                                             01248660
         TM    X'5E'(R1),X'C0'     CYLINDER ALLOCATION?                 01248670
         BO    CYLSTAT             YES, BRANCH                          01248680
         TM    X'5E'(R1),X'41'     AVG AND ROUND ALLOCATION             01248690
         BO    CYLSTAT             YES, BRANCH                          01248700
         B     *+8                                                      01248710
CYLSTAT  OI    STATUS,STATCYL      YES, SET CYLINDER SWITCH             01248720
         TM    X'52'(R1),X'42'     DSORG=PS OR PO                       01248730
         BZ    ERRDSORG            NEITHER, BRANCH                      01248740
         CLI   X'54'(R1),0         IS THERE A RECFM                     01248750
         BNE   OKRECFM             YES, BRANCH                          01248760
         LA    R1,MSG11                                                 01248770
         LA    R0,L'MSG11                                               01248780
         BAL   R14,PUTMSG                                               01248790
         OI    STATUS,STATERR      ERROR                                01248800
         B     NEXTDSN                                                  01248810
OKRECFM  EQU   *                                                        01248820
         SPACE                                                          01248830
************************************************************            01248840
*                                                          *            01248850
*        SET UP THE DCB                                    *            01248860
*                                                          *            01248870
************************************************************            01248880
         SPACE                                                          01248890
         LA    R5,MODDCB                                                01248900
         MVC   0(CONDCBL,R5),CONDCB                                     01248910
         USING IHADCB,R5                                                01248920
         MVC   DCBDDNAM(8),DDSAVE                                       01248930
         SPACE                                                          01248940
         LA    R15,DYNEXLST                                             01248950
         IC    R0,DCBEXLSA-1                                            01248960
         ST    R15,DCBEXLSA-1                                           01248970
         STC   R0,DCBEXLSA-1                                            01248980
         LA    R1,JFCB                                                  01248990
         ST    R1,0(,R15)                                               01249000
         MVI   0(R15),X'87'                                             01249010
         LA    R6,RLSDCB                                                01249020
         MVC   0(CONDCBL,R6),0(R5)                                      01249030
         SPACE                                                          01249040
         MVI   OPEN,X'80'                                               01249050
         SPACE                                                          01249060
         RDJFCB ((R5)),MF=(E,OPEN)                                      01249070
         SPACE                                                          01249080
*        OI    JFCB+X'34',X'08'    DO NOT REWRITE JFCB          DELETED 01249090
*        NI    JFCB+X'57',X'3F'    ZERO 1ST 2 BITS (OLD=X'40')  DELETED 01249100
*        OI    JFCB+X'57',X'80'    DISP=MOD            =X'80'   DELETED 01249110
         TM    STATUS,STATCYL      CYLINDER ALLOCATION?                 01249120
         BZ    *+8                 NO, SKIP NEXT INSTRUCTION            01249130
         OI    JFCB+X'9B',X'C0'    YES, MARK JFCBCTRI CYL               01249140
         SPACE                                                          01249150
         MVI   OPEN,X'80'                                               01249160
         SPACE                                                          01249170
         OPEN  ((R5),EXTEND),TYPE=J,MF=(E,OPEN)                         01249180
         SPACE                                                          01249190
         TM    DCBOFLGS,X'10'                                           01249200
         BO    OKOPEN                                                   01249210
         LA    R1,MSG04                                                 01249220
         LA    R0,L'MSG04                                               01249230
         BAL   R14,PUTMSG                                               01249240
         OI    STATUS,STATERR      ERROR                                01249250
         B     NEXTDSN                                                  01249260
         SPACE                                                          01249270
OKOPEN   EQU   *                                                        01249280
         MVC   HOLDEOF,DCBFDAD     SAVE MBBCCHHR OF EOF                 01249290
         OI    CLEANUP,CLEANOP5    TELL CLEANUP TO CLOSE IT             01249300
         SPACE                                                          01249310
         L     R1,DCBDEBAD         POINT TO DEB                         01249320
         SLR   R14,R14                                                  01249330
         SLR   R15,R15                                                  01249340
         SLR   R0,R0                                                    01249350
         IC    R0,16(,R1)          GET NUMBER OF EXTENTS                01249360
         LTR   R0,R0               ZERO EXTENTS?                        01249370
         BZ    RLSNONE             YES, BRANCH                          01249380
         IC    R15,04(,R1)         GET LENGTH OF EXTENT FIELD           01249390
LOOPDEB  AH    R14,46(,R1)         ADD TRACKS TO COUNTER                01249400
         AR    R1,R15              POINT TO NEXT EXTENT                 01249410
         BCT   R0,LOOPDEB                                               01249420
         BCTR  R14,0               TRKS-1 = HIGH TRACK NUMBER           01249430
         LR    R8,R14              SAVE HIGH TRACK ALLOCATED            01249440
         SPACE                                                          01249450
         L     R1,DCBDEBAD                                              01249460
         LA    R2,DCBFDAD                                               01249470
         ST    R3,12(,R13)                                              01249480
         STM   R9,R13,16(R13)                                           01249490
         LR    R3,R13                                                   01249500
         L     R15,16              CVTPTR                               01249510
         L     R15,32(,R15)        CVTPRLTV                             01249520
         BALR  R14,R15                                                  01249530
         LR    R13,R3              RESTORE R13                          01249540
         LM    R9,R13,16(R13)                                           01249550
         L     R3,12(,R13)                                              01249560
         SPACE                                                          01249570
         SRL   R0,16               TTRZ TO TT (HIGH TRACK USED)         01249580
         LR    R1,R8               GET HIGH TRACK ALLOCATED             01249590
         SR    R1,R0               MINUS HIGH TRACK USED                01249600
         BNP   RLSNONE             BRANCH IF NOTHING TO RELEASE         01249610
         SPACE                                                          01249620
         LR    R7,R1               SAVE UNUSED TRACKS                   01249630
         SPACE                                                          01249640
*              IF DATA SET WAS ORIGINALLY ALLOCATED IN CYLINDERS,       01249650
*              THE FOLLOWING ROUTINE INSURES THAT THE NUMBER OF         01249660
*              TRACKS REMAINING WILL BE AN EVEN MULTIPLE OF THE         01249670
*              NUMBER OF TRACKS PER CYLINDER FOR THE DEVICE.            01249680
*              R7 WILL BE CHANGED TO INCLUDE ONLY THOSE UNUSED          01249690
*              TRACKS THAT ARE WITHIN UNUSED CYLINDERS.                 01249700
         SPACE                                                          01249710
         TM    STATUS,STATCYL      ALLOCATED BY CYLINDER?               01249720
         BZ    NOTCYL              NO, BRANCH                           01249730
         CLI   LEAVEKW+1,3         'EXTENTS' SPECIFIED                  01249740
         BE    NOTCYL              YES, EXISTING EXTENTS ASSUMED OK     01249750
*        LR    R1,R8               GET HIGH TRACK ALLOCATED             01249760
*        SLR   R0,R0                                                    01249770
         SRDL  R0,32                                                    01249780
         LH    R15,TRKCYL          GET TRACKS PER CYLINDER              01249790
         DR    R0,R15              GET TRACK WITHIN LAST CYL            01249800
         BCTR  R15,0               GET NO OF LAST TRK                   01249810
         CLR   R15,R0              IS LAST TRK = HIGH TRACK             01249820
         BNE   ADJUST              NO, ADJUSTMENT NECESSARY             01249830
         L     R0,LEAVV                                                 01249840
         LTR   R0,R0               IS LEAVE SPECIFIED                   01249850
         BZ    RLSALL              NO, TAKE FAST PATH                   01249860
         B     ADJLEAV             YES, ADJUST IT                       01249870
ADJUST   MH    R1,TRKCYL           CHANGE CYL TO TRK                    01249880
         AR    R15,R1              SET TO LAST TRACK                    01249890
         LR    R1,R8               GET HIGH TRACK ALLOCATED             01249900
         SR    R1,R15              MINUS LAST TRACK OF LAST CYL USED    01249910
         BNP   RLSNONE             BRANCH IF NOTHING TO RELEASE         01249920
         LR    R0,R7               SAVE OLD UNUSED TRACKS               01249930
         LR    R7,R1               NO OF TRACKS TO RELEASE              01249940
         SR    R0,R1               GET DIFF BETWEEN OLD & NEW           01249950
         LR    R1,R0               DIFF IN R1                           01249960
*                                                                       01249970
*              NOW WE HAVE AN IMPLICIT LEAVE(N) WHERE N IS IN R1        01249980
*              AND IS THE NUMBER OF TRACKS FROM THE LAST TRACK USED     01249990
*              TO THE END OF THE CYLINDER.  IF LEAVE WAS EXPLICITLY     01250000
*              SPECIFIED, WE CAN REDUCE THE SPECIFIED NUMBER BY THE     01250010
*              IMPLICIT NUMBER BECAUSE IF THE USER SPECIFIED LEAVE(8)   01250020
*              AND THE IMPLICIT NUMBER WAS 7, THAT MEANS THAT NORMAL    01250030
*              CYLINDER RELEASE WILL LEAVE THE USER 7 UNUSED TRACKS     01250040
*              AND OUR INTERNAL PROCESSING ONLY HAS TO WORK ON GETTING  01250050
*              THE ONE OTHER TRACK THE USER WANTS. THE USER WILL END    01250060
*              UP WITH MORE THAN 8 BECAUSE THE ONE OTHER TRACK MEANS    01250070
*              THE WHOLE CYLINDER CONTAINING THAT TRACK WILL BE LEFT.   01250080
*              AFTER THE SPECIFIED NUMBER IS REDUCED BY THE IMPLICIT    01250090
*              NUMBER, IF THE RESULT IS POSITIVE WE ROUND IT UP TO THE  01250100
*              NEXT HIGHER MULTIPLE OF TRACKS PER CYLINDER.             01250110
*              IF THE USER SPECIFIED LEAVE(7) AND THE IMPLICIT NUMBER   01250120
*              WAS 7 (OR MORE), OUR JOB IS SIMPLE BECAUSE A NORMAL      01250130
*              CYLINDER RELEASE WILL LEAVE ENOUGH UNUSED TRACKS.        01250140
*                                                                       01250150
         L     R0,LEAVV                                                 01250160
         SR    R0,R1               REDUCE SPECIFIED NUMBER BY R1        01250170
         ST    R0,LEAVV                                                 01250180
         BNP   RLSPART             IF RESULT LT 1, RELEASE R7 TRACKS.   01250190
ADJLEAV  SRDL  R0,32                                                    01250200
         BCTR  R1,0                                                     01250210
         LH    R15,TRKCYL                                               01250220
         DR    R0,R15                                                   01250230
         MH    R1,TRKCYL                                                01250240
         AR    R1,R15              PLUS 1 CYL                           01250250
         SR    R7,R1               REDUCE NUMBER OF RELEASABLE TRACKS   01250260
         BP    RLSPART             GO RELEASE THEM                      01250270
         B     RLSNONE             UNLESS THAT LEAVES NOTHING           01250280
         SPACE                                                          01250290
*                                                                       01250300
*              PROCESSING FOR NON-CYLINDER ALLOCATIONS,                 01250310
*              (OR CYLINDER ALLOCATIONS IF 'EXTENTS' SPECIFIED).        01250320
*                                                                       01250330
*              R7 CONTAINS # OF TRACKS RELEASABLE.                      01250340
*              IT CAN BE REDUCED BY THE NUMBER OF TRACKS SPECIFIED      01250350
*              IN THE  LEAVE(NN)  KEYWORD.                              01250360
*                                                                       01250370
NOTCYL   EQU   *                                                        01250380
         L     R15,LEAVV                                                01250390
         LTR   R15,R15             TO LEAVE SOME UNUSED TRACKS?         01250400
         BZ    LEAVEX              NO, BRANCH                           01250410
         SR    R7,R15              REDUCE RELEASABLE SPACE              01250420
         BP    RLSPART             BRANCH IF RELEASABLE SPACE           01250430
         B     RLSNONE             NO RELEASABLE SPACE                  01250440
         SPACE                                                          01250450
LEAVEX   EQU   *                                                        01250460
         CLI   LEAVEKW+1,3         'EXTENTS' SPECIFIED?                 01250470
         BNE   RLSALL              NO, GO RELEASE ALL UNUSED SPACE      01250480
         LR    R2,R0               SAVE HIGH TRACK USED                 01250490
         SPACE                                                          01250500
         L     R1,DCBDEBAD         POINT TO DEB                         01250510
         SLR   R14,R14                                                  01250520
         SLR   R15,R15                                                  01250530
         SLR   R0,R0                                                    01250540
         IC    R0,16(,R1)          GET NUMBER OF EXTENTS                01250550
         IC    R15,04(,R1)         GET LENGTH OF EXTENT FIELD           01250560
EXTLOOP  AH    R14,46(,R1)         ADD TRACKS TO COUNTER                01250570
         CR    R14,R2              TRACKS .GT. HIGH TRACK USED?         01250580
         BH    EXTSTOP             YES, KEEP THRU THIS EXTENT           01250590
         AR    R1,R15              POINT TO NEXT EXTENT                 01250600
         BCT   R0,EXTLOOP          BRANCH FOR EACH EXTENT               01250610
         B     RLSNONE             KEEP ALL EXTENTS                     01250620
EXTSTOP  BCTR  R0,0                SUBTRACT 1 FROM EXTENT               01250630
         LTR   R0,R0               WAS IT LAST EXTENT?                  01250640
         BNP   RLSNONE             YES, KEEP ALL EXTENTS                01250650
         BCTR  R14,0               HIGH TRACK IN THIS EXTENT            01250660
         LR    R7,R8               HIGH TRACK ALLOC                     01250670
         SR    R7,R14              GET NO TRACKS IN REMAIN EXTENTS      01250680
         BNP   RLSNONE                                                  01250690
         B     RLSPART                                                  01250700
         SPACE                                                          01250710
*                                                                       01250720
*              RELEASE THE NUMBER OF TRACKS SPECIFIED IN R7             01250730
*                                                                       01250740
         SPACE                                                          01250750
OPEN6    MVI   CLOSE,X'80'                                              01250760
         CLOSE ((R5)),MF=(E,CLOSE)                                      01250770
         NI    CLEANUP,255-CLEANOP5                                     01250780
         SPACE                                                          01250790
         OI    JFCB+X'56',X'C0'    RLSE                                 01250800
         MVI   OPEN,X'80'                                               01250810
         OPEN  ((R6),EXTEND),TYPE=J,MF=(E,OPEN)                         01250820
         BR    R2                                                       01250830
         SPACE                                                          01250840
RLSPART  BAL   R2,OPEN6                                                 01250850
         LR    R0,R8               GET HIGH TRACK ALLOCATED             01250860
         SR    R0,R7               MINUS RELEASED TRACKS                01250870
         SLL   R0,16               TT00 OF LAST TRACK TO BE KEPT        01250880
         LA    R1,X'100'           RECORD 1                             01250890
         OR    R0,R1               CHANGE TT00 TO TTR0  (R=1)           01250900
         SPACE                                                          01250910
*              PUT FAKE CCHHR ADDRESS IN R6 DCB.                        01250920
*              CONVERT TTR IN R0 TO CCHHR IN DCBFDAD                    01250930
         L     R1,DCBDEBAD-IHADCB(,R6)                                  01250940
         LA    R2,DCBFDAD-IHADCB(,R6)                                   01250950
         ST    R3,12(,R13)                                              01250960
         STM   R9,R13,16(R13)                                           01250970
         LR    R3,R13                                                   01250980
         L     R15,16                                                   01250990
         L     R15,28(,R15)        CVTPCNVT  TTR TO CCHHR               01251000
         BALR  R14,R15                                                  01251010
         LR    R13,R3                                                   01251020
         LM    R9,R13,16(R13)                                           01251030
         L     R3,12(,R13)                                              01251040
         SPACE                                                          01251050
         MVI   CLOSE,X'80'                                              01251060
         CLOSE ((R6)),MF=(E,CLOSE)                                      01251070
*                                                                       01251080
*              NOW RESTORE THE REAL END OF FILE                         01251090
*                                                                       01251100
         SPACE                                                          01251110
RESTREOF EQU   *                                                        01251120
         NI    JFCB+X'56',255-X'C0'   NO RLSE                           01251130
         SPACE                                                          01251140
         MVI   OPEN,X'80'                                               01251150
         OPEN  ((R6),EXTEND),TYPE=J,MF=(E,OPEN)                         01251160
         SPACE                                                          01251170
         MVC   DCBFDAD-IHADCB(8,R6),HOLDEOF                             01251180
         SPACE                                                          01251190
         MVI   CLOSE,X'80'                                              01251200
         CLOSE ((R6)),MF=(E,CLOSE)                                      01251210
         B     NEXTDSN                                                  01251220
         SPACE                                                          01251230
*                                                                       01251240
*              RELEASE ALL UNUSED SPACE                                 01251250
*                                                                       01251260
         SPACE                                                          01251270
RLSALL   BAL   R2,OPEN6                                                 01251280
         MVI   CLOSE,X'80'                                              01251290
         CLOSE ((R6)),MF=(E,CLOSE)                                      01251300
         B     NEXTDSN                                                  01251310
         SPACE                                                          01251320
*                                                                       01251330
*              RELEASE NOTHING                                          01251340
*                                                                       01251350
         SPACE                                                          01251360
RLSNONE  EQU   *                                                        01251370
         SLR   R7,R7                                                    01251380
         SPACE                                                          01251390
*                                                                       01251400
*              CLEANUP AND TERMINATE                                    01251410
*                                                                       01251420
         SPACE                                                          01251430
NEXTDSN  EQU   *                                                        01251440
         L     R2,SAVE2                                                 01251450
         TM    CLEANUP,CLEANOP5                                         01251460
         BZ    NOCLOSE                                                  01251470
         MVI   CLOSE,X'80'                                              01251480
         CLOSE ((R5)),MF=(E,CLOSE)                                      01251490
         NI    CLEANUP,255-CLEANOP5                                     01251500
NOCLOSE  TM    CLEANUP,CLEANDAR    IS DATA SET ALLOCATED                01251510
         BZ    NOFREE              NO, BYPASS UNALLOCATE                01251520
         LA    R1,MYDAPL                                                01251530
         LA    R15,MYDAPB                                               01251540
         USING DAPB18,R15                                               01251550
         XC    0(40,R15),0(R15)                                         01251560
         MVI   DA18CD+1,X'18'                                           01251570
         MVC   DA18DDN,DDSAVE                                           01251580
         MVC   DA18MNM(8),=CL8' '                                       01251590
         MVC   DA18SCLS(2),=CL8' '                                      01251600
         BAL   R14,CALLDAIR        UNALLOCATE                           01251610
         NI    CLEANUP,255-CLEANDAR FREED                               01251620
         SPACE                                                          01251630
NOFREE   EQU   *                                                        01251640
         SPACE                                                          01251650
         LTR   R7,R7               WAS RELEASE LOGIC ENTERED            01251660
         BM    NODIS               NO, BYPASS DISPLAY                   01251670
         LA    R1,MSGWK                                                 01251680
         MVI   0(R1),C' '                                               01251690
         MVC   1(L'MSGWK-1,R1),0(R1)                                    01251700
         MVC   0(6,R1),=X'402020202120'                                 01251710
         CVD   R7,DOUBLE                                                01251720
         ED    0(6,R1),DOUBLE+5                                         01251730
         MVC   7(6,R1),=C'TRACKS'                                       01251740
         LA    R15,13(R1)                                               01251750
         CL    R7,=F'1'            ONE TRACK                            01251760
         BNE   *+6                 NO, SKIP NEXT INSTR                  01251770
         BCTR  R15,0               CHANGE TRACKS TO TRACK               01251780
         MVC   0(24,R15),=C' RELEASED FROM DATA SET '                   01251790
         MVC   24(44,R15),DSNSAVE+2                                     01251800
         LA    R0,24+44(,R15)                                           01251810
         SR    R0,R1                                                    01251820
         BAL   R14,PUTMSG                                               01251830
NODIS    EQU   *                                                        01251840
         SPACE                                                          01251850
         L     R4,24(,R4)          POINT TO NEXT DSN PDE                01251860
         LA    R4,0(,R4)           CLEAR HIGH ORDER BYTE                01251870
         LTR   R4,R4               IS THERE ANOTHER                     01251880
         BNZ   STARTDSN            YES, GO PROCESS NEW DSNAME           01251890
         IKJRLSA MYANS                                                  01251900
         LH    R15,RC                                                   01251910
         B     EXIT                                                     01251920
         SPACE                                                          01251930
************************************************************            01251940
*                                                          *            01251950
*         CALL IKJDAIR SERVICE ROUTINE                     *            01251960
*                                                          *            01251970
************************************************************            01251980
         SPACE                                                          01251990
CALLDAIR ST    R14,DAIRREGS                                             01252000
         L     R15,16                                                   01252010
         TM    732(R15),X'80'     CVTDAIR                               01252020
         BNO   DAIRLINK                                                 01252030
         L     R15,732(,R15)                                            01252040
         BALR  R14,R15                                                  01252050
         B     DAIRFINI                                                 01252060
DAIRLINK EQU   *                                                        01252070
         LINK  EP=IKJDAIR,SF=(E,LINKAREA)                               01252080
DAIRFINI L     R14,DAIRREGS                                             01252090
         BR    R14                                                      01252100
         SPACE                                                          01252110
************************************************************            01252120
*                                                          *            01252130
*        DYNAMIC ALLOCATION FAILURE ROUTINE                *            01252140
*                                                          *            01252150
************************************************************            01252160
         SPACE                                                          01252170
DAIRFAIL ST    R14,MYDFREGS                                             01252180
         LA    R1,MYDFPARM                                              01252190
         USING DFDSECTD,R1                                              01252200
         ST    R15,MYDFRC                                               01252210
         LA    R15,MYDFRC                                               01252220
         ST    R15,DFRCP                                                01252230
         LA    R15,MYDAPL                                               01252240
         ST    R15,DFDAPLP                                              01252250
         SLR   R15,R15                                                  01252260
         ST    R15,MYJEFF02                                             01252270
         LA    R15,MYJEFF02                                             01252280
         ST    R15,DFJEFF02                                             01252290
         LA    R15,DFDAIR                                               01252300
         STH   R15,MYDFID                                               01252310
         LA    R15,MYDFID                                               01252320
         ST    R15,DFIDP                                                01252330
         SLR   R15,R15                                                  01252340
         ST    R15,DFCPPLP                                              01252350
         LINK  EP=IKJEFF18,SF=(E,LINKAREA)                              01252360
         L     R15,MYDFRC                                               01252370
         DROP  R1                                                       01252380
         L     R14,MYDFREGS                                             01252390
         BR    R14                                                      01252400
         SPACE                                                          01252410
************************************************************            01252420
*                                                          *            01252430
*        PUTMSG ROUTINE                                    *            01252440
*                                                          *            01252450
************************************************************            01252460
         SPACE                                                          01252470
PUTMSG   STM   R14,R1,PUTLINS                                           01252480
         XC    MYOLD(8),MYOLD                                           01252490
         XC    MYSEG1(4),MYSEG1                                         01252500
         MVC   MYPTPB(12),MODLPTPM                                      01252510
         LA    R14,1               NO. OF MESSAGE SEGMENTS              01252520
         ST    R14,MYOLD                                                01252530
         LA    R14,MYSEG1          POINT TO 1ST SEGMENT                 01252540
         ST    R14,MYOLD+4                                              01252550
         LR    R14,R0              LENGTH IN R0                         01252560
         LA    R14,4(,R14)         ADD 4                                01252570
         LA    R15,MYSEG1+4                                             01252580
         CLC   0(3,R1),=C'IKJ'     IS DATA PRECEEDED BY MESSAGE ID?     01252590
         BE    *+16                YES - BRANCH                         01252600
         LA    R14,1(,R14)         ADD 1 TO LENGTH                      01252610
         MVI   0(R15),C' '         INSERT LEADING BLANK                 01252620
         LA    R15,1(,R15)         BUMP POINTER                         01252630
         STH   R14,MYSEG1                                               01252640
         LR    R14,R0                                                   01252650
         BCTR  R14,0                                                    01252660
         B     *+10                                                     01252670
         MVC   0(0,R15),0(R1)      MOVE MESSAGE IN                      01252680
         EX    R14,*-6                                                  01252690
         LA    R1,MYIOPL                                                01252700
         L     R15,MYPUTLEP                                             01252710
         SPACE                                                          01252720
         PUTLINE PARM=MYPTPB,OUTPUT=(MYOLD),ENTRY=(15),MF=(E,(1))       01252730
         SPACE                                                          01252740
         LM    R14,R1,PUTLINS                                           01252750
         BR    R14                                                      01252760
         SPACE                                                          01252770
************************************************************            01252780
*                                                          *            01252790
*        PUTLINE ROUTINE                                   *            01252800
*                                                          *            01252810
************************************************************            01252820
         SPACE                                                          01252830
PUTLINE  STM   R14,R1,PUTLINS                                           01252840
         XC    MYSEG1(4),MYSEG1                                         01252850
         MVC   MYPTPB(12),MODLPTPB                                      01252860
         LR    R14,R0              LENGTH IN R0                         01252870
         LA    R14,4(,R14)         ADD 4                                01252880
         STH   R14,MYSEG1                                               01252890
         LR    R14,R0                                                   01252900
         BCTR  R14,0                                                    01252910
         B     *+10                                                     01252920
         MVC   MYSEG1+4(0),0(R1)   MOVE TEXT IN                         01252930
         EX    R14,*-6                                                  01252940
         LA    R1,MYIOPL                                                01252950
         L     R15,MYPUTLEP                                             01252960
         SPACE                                                          01252970
         PUTLINE PARM=MYPTPB,OUTPUT=(MYSEG1,DATA),ENTRY=(15),MF=(E,(1)) 01252980
         SPACE                                                          01252990
         LM    R14,R1,PUTLINS                                           01253000
         BR    R14                                                      01253010
         SPACE                                                          01253020
EXIT     SLR   R15,R15                                                  01253030
         TM    STATUS,STATERR                                           01253040
         BZ    EXITX                                                    01253050
         MVC   MYSTPB(STACKDL),STACKD                                   01253060
         STACK DELETE=ALL,PARM=MYSTPB,MF=(E,MYIOPL)                     01253070
         TCLEARQ                                                        01253080
         LA    R15,12                                                   01253090
EXITX    LR    R1,R13                                                   01253100
         L     0,@SIZE                                                  01253110
         L     13,4(,13)                                                01253120
         ST    15,16(,13)                                               01253130
         FREEMAIN R,A=(1),LV=(0)                                        01253140
         LM    14,12,12(13)                                             01253150
         BR    14                                                       01253160
         SPACE                                                          01253170
************************************************************            01253180
*                                                          *            01253190
*        CONSTANTS                                         *            01253200
*                                                          *            01253210
************************************************************            01253220
         SPACE                                                          01253230
MODLPTPM PUTLINE OUTPUT=(1,TERM,SINGLE,INFOR),                         X01253240
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),MF=L                  01253250
         SPACE                                                          01253260
MODLPTPB PUTLINE OUTPUT=(1,TERM,SINGLE,DATA),                          X01253270
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),MF=L                  01253280
         SPACE                                                          01253290
         PRINT NOGEN                                                    01253300
         SPACE                                                          01253310
CONDCB   DCB   DDNAME=DYNAM,DSORG=PS,MACRF=(W)                          01253320
CONDCBL  EQU   *-CONDCB                                                 01253330
         SPACE                                                          01253340
         PRINT GEN                                                      01253350
         SPACE                                                          01253360
OBTAIN   CAMLST SEARCH,2,3,4                                            01253370
OBTAINL  EQU   *-OBTAIN                                                 01253380
         SPACE                                                          01253390
LOCATE   CAMLST NAME,2,,4                                               01253400
LOCATEL  EQU   *-LOCATE                                                 01253410
         SPACE                                                          01253420
STACKD   STACK DELETE=ALL,MF=L                                          01253430
STACKDL  EQU   *-STACKD                                                 01253440
         SPACE                                                          01253450
MSG01    DC    C'ERROR IN PARSE SERVICE ROUTINE'                        01253460
MSG02    DC    C'IKJ58503I DATA SET '                                   01253470
MSG02A   DC    C' NOT IN CATALOG'                                       01253480
MSG03    DC    C'ERROR IN DEFAULT SERVICE ROUTINE'                      01253490
MSG04    DC    C'UNABLE TO OPEN DATASET'                                01253500
MSG05    DC    C'IKJ58509I MEMBER NAME MUST NOT BE SPECIFIED'           01253510
MSG06    DC    C'ORGANIZATION OF DATA SET MUST BE PARTITIONED OR SEQUEN+01253520
               TIAL'                                                    01253530
MSG09    DC    C'DATA SET IS NOT ON A DIRECT ACCESS DEVICE'             01253540
MSG10    DC    C'UNABLE TO OBTAIN DSCB FOR DATA SET'                    01253550
MSG11    DC    C'DATA SET HAS NEVER BEEN OPENED'                        01253560
         LTORG                                                          01253570
PCLADDR  DC    0D'0'  END OF CSECT, START OF PARSE PCL CSECT            01253580
         SPACE                                                          01253590
************************************************************            01253600
*                                                          *            01253610
*        PARSE PARAMETERS                                  *            01253620
*                                                          *            01253630
************************************************************            01253640
         SPACE                                                          01253650
         PRINT NOGEN                                                    01253660
RELEAPCL IKJPARM                                                        01253670
DSN      IKJPOSIT DSNAME,LIST,PROMPT='DATA SET NAME'                    01253680
LEAVEKW  IKJKEYWD                                                       01253690
         IKJNAME 'LEAVE',SUBFLD=LEAVESF                                 01253700
         IKJNAME 'SPACE',SUBFLD=LEAVESF                                 01253710
         IKJNAME 'EXTENTS'                                              01253720
LEAVESF  IKJSUBF                                                        01253730
LEAVE    IKJIDENT 'NUMBER OF TRACKS',                                  +01253740
               FIRST=NUMERIC,OTHER=NUMERIC,MAXLNTH=5,                  +01253750
               PROMPT='NUMBER OF UNUSED TRACKS TO LEAVE UN-RELEASED'    01253760
         IKJENDP                                                        01253770
         PRINT GEN                                                      01253780
         SPACE                                                          01253790
************************************************************            01253800
*                                                          *            01253810
*        DSECTS                                            *            01253820
*                                                          *            01253830
************************************************************            01253840
         SPACE                                                          01253850
@DATA    DSECT                                                          01253860
         DS    18F                 REGISTER SAVEAREA                    01253870
DOUBLE   DS    D                                                        01253880
LINKAREA DS    2F                                                       01253890
MYPPL    DS    7F                                                       01253900
MYANS    DS    F                                                        01253910
MYECB    DS    F                  USED BY PUTLINE ROUTINE               01253920
MYIOPL   DS    4F                 USED BY PUTLINE ROUTINE               01253930
MYPUTLEP DS    F                                                        01253940
MYPTPB   DS    3F                 USED BY PUTLINE ROUTINE               01253950
MYOLD    DS    2F                 USED BY PUTLINE ROUTINE               01253960
MYSEG1   DS    2H,CL100           USED BY PUTLINE ROUTINE               01253970
PUTLINS  DS    4F                 USED BY PUTLINE ROUTINE               01253980
MYSTPB   DS    0F                                                       01253990
MYDAPL   DS    5F                                                       01254000
MYDAPB   DS    21F                                                      01254010
MYDFPB   DS    5F                                                       01254020
DSNAME   DS    H,CL44                                                   01254030
DSNSAVE  DS    H,CL44                                                   01254040
CLEANUP  DS    X                                                        01254050
CLEANDAR EQU   X'80'                                                    01254060
CLEANOP5 EQU   X'40'                                                    01254070
STATUS   DS    H                                                        01254080
STATLOC  EQU   X'08'                                                    01254090
STATCYL  EQU   X'04'                                                    01254100
STATERR  EQU   X'01'                                                    01254110
RC       DS    H                                                        01254120
VOLSER   DS    CL6                                                      01254130
LOCATEW  DS    0F                                                       01254140
OBTAINW  DS    4F                                                       01254150
LOCBUF   DS    0D,0CL265                                                01254160
MYDSCB   DS    CL140                                                    01254170
MSGWK    DS    CL128                                                    01254180
DEVAREA  DS    0F                  5 FULLWORDS FOR DEVTYPE              01254190
TRKCYL   EQU   DEVAREA+10,2                                             01254200
MYDFPARM DS    5F  USED BY DAIRFAIL                                     01254210
MYDFREGS DS    F   USED BY DAIRFAIL                                     01254220
MYDFRC   DS    F   USED BY DAIRFAIL                                     01254230
MYJEFF02 DS    F   USED BY DAIRFAIL                                     01254240
MYDFID   DS    H   USED BY DAIRFAIL                                     01254250
LEAVV    DS    F                                                        01254260
HOLDEOF  DS    CL8                                                      01254270
DDSAVE   DS    CL8                                                      01254280
DAIRREGS DS    F                                                        01254290
OPEN     DS    0F                                                       01254300
CLOSE    DS    F                                                        01254310
DYNEXLST DS    F                                                        01254320
SAVE2    DS    F                                                        01254330
MODDCB   DS    0D,XL(CONDCBL)                                           01254340
RLSDCB   DS    0D,XL(CONDCBL)                                           01254350
         DS    0D                                                       01254360
JFCB     DS    XL176                                                    01254370
@DATAL   EQU   *-@DATA                                                  01254380
         SPACE                                                          01254390
IHADCB   DSECT                                                          01254400
         DS    XL5                                                      01254410
DCBFDAD  DS    CL8                 FULL DISK ADDRESS, MBBCCHHR          01254420
         DS    19XL1                                                    01254430
DCBBFTEK DS    XL1                                                      01254440
DCBEODAD DS    AL3                                                      01254450
DCBRECFM DS    X                                                        01254460
DCBEXLSA DS    AL3                                                      01254470
DCBDDNAM DS    CL8                                                      01254480
         ORG   *-4                                                      01254490
DCBDEBAD DS    A                                                        01254500
DCBOFLGS DS    X                                                        01254510
         DS    7XL1                                                     01254520
         DS    X                                                        01254530
DCBSYNAD DS    AL3                                                      01254540
         SPACE                                                          01254550
         IKJCPPL                                                        01254560
         SPACE 3                                                        01254570
         IKJPPL                                                         01254580
         SPACE                                                          01254590
         IKJDFPB                                                        01254600
         SPACE 2                                                        01254610
         IKJUPT                                                         01254620
         SPACE 2                                                        01254630
         IKJIOPL                                                        01254640
         SPACE 2                                                        01254650
         IKJDAPL                                                        01254660
         SPACE 2                                                        01254670
         IKJDAP08                                                       01254680
         SPACE 2                                                        01254690
         IKJDAP18                                                       01254700
         SPACE 2                                                        01254710
         IKJEFFDF DFDSECT=YES                                           01254720
         SPACE 2                                                        01254730
R0       EQU   0                                                        01254740
R1       EQU   1                                                        01254750
R2       EQU   2                                                        01254760
R3       EQU   3                                                        01254770
R4       EQU   4                                                        01254780
R5       EQU   5                                                        01254790
R6       EQU   6                                                        01254800
R7       EQU   7                                                        01254810
R8       EQU   8                                                        01254820
R9       EQU   9                                                        01254830
R10      EQU   10                                                       01254840
R11      EQU   11                                                       01254850
R12      EQU   12                                                       01254860
R13      EQU   13                                                       01254870
R14      EQU   14                                                       01254880
R15      EQU   15                                                       01254890
         END                                                            01254900
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR
//LKED.SYSPRINT DD SYSOUT=*
//LKED.SYSIN DD *
 NAME RLSE(R)
/*
//HELP    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP
//SYSIN    DD  *
./ ADD NAME=RLSE
)F FUNCTION -                                                           01255070
  THE RLSE COMMAND IS USED TO FREE UP UNUSED SPACE FROM AN              01255080
  EXISTING DATASET.                                                     01255090
)X SYNTAX -                                                             01255100
         RLSE  'DATASET-NAME'  LEAVE('TRACKS')  EXTENTS                 01255110
  REQUIRED - 'DATASET-NAME'                                             01255120
  DEFAULTS - ALL UNUSED TRACKS ARE RELEASED IF NEITHER                  01255130
             'LEAVE' NOR 'EXTENTS' IS SPECIFIED.                        01255140
  ALIAS    - NONE                                                       01255150
)O OPERAND -                                                            01255160
  'DATASET-NAME'                                                        01255170
         - SPECIFIES THE NAME OF THE DATASET WHICH IS TO HAVE           01255180
           ITS UNUSED SPACE RELEASED.                                   01255190
))LEAVE('TRACKS') - 'TRACKS' IS THE NUMBER OF UNUSED TRACKS             01255200
           NOT TO BE FREED.                                             01255210
))EXTENTS - THIS KEYWORD INDICATES THAT ONLY UNUSED SECONDARY           01255220
           EXTENTS ARE TO BE FREED.  THE PRIMARY EXTENT AND             01255230
           ANY PARTIALLY USED SECONDARY EXTENTS ARE NOT FREED,          01255240
           EVEN IF THEY CONTAIN UNUSED SPACE.                           01255250
           THIS KEYWORD AND 'LEAVE' ARE MUTUALLY EXCLUSIVE.             01255260
./ ENDUP
/*
