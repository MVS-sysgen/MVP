//EDIT JOB (JOB),
//             'INSTALL EDIT',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//* Create temp dataset for objects
//IEFBR14 EXEC PGM=IEFBR14
//OBJ      DD  DSN=&&OBJ,UNIT=SYSDA,
//             SPACE=(CYL,(1,1,5)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200),
//             DISP=(,PASS,DELETE)
//MACLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=&&MACLIBS,UNIT=SYSDA,
//             DISP=(,PASS,DELETE),
//             SPACE=(TRK,(04,02,02)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=32000,DSORG=PS)
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=SYCONVHX
         MACRO
&NAME    SYCONVHX &IN=,&OUT=,&L=
.*       USER MACRO
.*       CONVERT HEXADECIMAL TO EBCDIC NOTATION
.*       AT LEAST 1 BYTE MAX. 7 BYTES
.*       WRITTEN 750902
         GBLC  &SYPRTX1,&SYPRTX2
         LCLC  &SYMBOL
         AIF   (T'&IN EQ 'O' OR T'&OUT EQ 'O').ERROR5
         AIF   ('&SYPRTX1' NE '').CONT01
&SYPRTX1 SETC  'IHB&SYSNDX.A'       WORKAREA
&SYPRTX2 SETC  'IHB&SYSNDX.B'       CONSTANT
.CONT01  AIF   (T'&L NE 'N').ERROR1
         AIF   ('&L'(1,1) EQ '(' OR '&L'(K'&L,1) EQ ')').ERROR2
         AIF   (&L LT 1 OR &L GT 7).ERROR2
         AIF   ('&IN'(1,1) EQ '(').INREG1
         AIF   (T'&IN EQ 'N').ERROR6
&NAME    UNPK  &SYPRTX1.((&L+1)*2-1),&IN.(&L+1) EXPAND EACH BYTE
         AGO   .CONT02
.INREG1  AIF   (N'&IN EQ 1).INREG2
&SYMBOL  SETC  'IN'
         AGO   .ERROR3
.INREG2  ANOP
&NAME    UNPK  &SYPRTX1.((&L+1)*2-1),0(&L+1,&IN(1))
.CONT02  NC    &SYPRTX1.(&L*2),&SYPRTX2     RESET HIGH ORDER DIGITS
         TR    &SYPRTX1.(&L*2),=C'0123456789ABCDEF'
         AIF   ('&OUT'(1,1) EQ '(').OUTREG1
         AIF   (T'&OUT EQ 'N').ERROR7
         MVC   &OUT.(&L*2),&SYPRTX1        MOVE TO USER FIELD
         AGO   .CONT03
.OUTREG1 AIF   (N'&OUT EQ 1).OUTREG2
&SYMBOL  SETC  'OUT'
         AGO   .ERROR3
.OUTREG2 AIF   ('&OUT' EQ '(0)').ERROR4
         AIF   (T'&OUT EQ 'N').OUTREG3
         MNOTE 'IF LENGTH ERROR, REG 0 WAS SYMBOLICLY ASSIGNED'
         MVC   0(-1+&OUT(1),0),0(0)  *** DUMMY MVC TO TEST IF R 0
         ORG   *-6                        WAS SPECIFIED ***
         AGO   .OUTREG4
.OUTREG3 AIF   (&OUT(1) EQ 0).ERROR4
.OUTREG4 MVC   0(&L*2,&OUT(1)),&SYPRTX1
.CONT03  AIF   ('&SYPRTX1'(4,4) NE '&SYSNDX').DONE
         B     *+34
&SYPRTX1 DS    CL15                       WORKAREA
&SYPRTX2 DC    15X'0F'               CONSTANT FO NC INSTR.
         AGO   .DONE
.*       ERROR MESSAGE BLOCK
.ERROR1  MNOTE 12,'LENGTH OPERAND MUST BE A SELF DEFINING TERM'
         MEXIT
.ERROR2  MNOTE 12,'INVALID LENGTH SPECIFIED'
         MEXIT
.ERROR3  MNOTE 12,'SUBLIST NOTATION FOUND ON SYMBOL ''&SYMBOL''- NOT ALX
               LOWED'
         MEXIT
.ERROR4  MNOTE 12,'REGISTER 0 IS AN INVALID ASSIGNMENT'
         MEXIT
.ERROR5  MNOTE 12,'REQUIRED PARAMETER ''IN'' OR ''OUT'' MISSING'
         MEXIT
.ERROR6  MNOTE 12,'INVALID SPECIFICATION FOR PARAMETER ''IN'''
         MEXIT
.ERROR7  MNOTE 12,'INVALID SPECIFICATION FOR PARAMETER ''OUT'''
.DONE    MEXIT
.*
         MEND
@@
//ASM     PROC MEMBER=DUMMY,
//             ASMPARM=''
//ASM     EXEC PGM=IFOX00,PARM='OBJ,NODECK,LINECOUNT(51)&ASMPARM'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSDA,SPACE=(3200,(100,200))
//SYSUT2   DD  UNIT=SYSDA,SPACE=(3200,(100,200))
//SYSUT3   DD  UNIT=SYSDA,SPACE=(3200,(100,200))
//SYSGO    DD  DSN=&&OBJ(&MEMBER),DISP=OLD
//SYSLIB   DD  DSN=&&MACLIBS,DISP=OLD
//         DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSIN    DD  DUMMY
//        PEND
//TSTVS  EXEC ASM,MEMBER=TSTVS
//ASM.SYSIN DD DATA,DLM=@@
TSTVS    TITLE 'D Y N A M I C CONSOLE MONITOR'
*---------------------------------------------------------------------*
*                                                                     *
* PROGRAM:                  TSTVS                                     *
* FUNCTION:                 Line-editor from the operator console     *
* PURPOSE:                  Edit crucial PDS members if the           *
*                           SUBSYSTEMS aren't available               *
*                           and create and submit jobs if necessary.  *
*                           Update of members will be done if the     *
*                           MSG   'EDT200A enter command...' has been *
*                           replied with 'U member' or 'UN member'    *
*                           If replied with 'A member' or 'AN member' *
*                           a new member will be created.             *
*                           If 'UN' or 'AN' specified, TSTVS will     *
*                           prompt you for a linenumber (COLS 73-80)  *
*                           if a new record has been made.            *
*                                                                     *
* PROCESSOR.                ASSEMBER-H VERSION 2/HL-ASM               *
*                                                                     *
* REGISTER ASSIGNMENT.      R0-R1          RESERVED                   *
*                           R2             COUNT REG. NUMBER OF CRDS. *
*                           R3             SAVE REGISTER FOR PARM.    *
*                           R4             ADDRESS RECORD INPUT       *
*                           R5-R8          WORK-REGISTERS             *
*                           R9             THIRD BASE                 *
*                           R10            BASE-REGISTER              *
*                           R11            BASE-REGISTER              *
*                           R12            ADDRESS GETM. AREA         *
*                           R13            ADDRESS SAVE-AREA          *
*                           R14-R15        RESERVED                   *
* AUTHOR.                   Rob Prins - Jan 11, 1977                  *
*        PROGRAM STRUCTURE:                                           *
*                                                                     *
*        -------------        -------------        -------------      *
*        |  MAIN     |        | CHANGE    |        |  INSERT   |      *
*        |  ROUTINE  | <----> | ROUTINE   | <----> |  DEL ROUT.|      *
*        |           |        |           |        |           |      *
*        -------------        -------------        -------------      *
*             |                     |                                 *
*             V                     |                                 *
*        -------------              |                                 *
*        | DEL       |              |                                 *
*        | MEMBERS   |              |                                 *
*        | AND RDDIR |              |                                 *
*        -------------              |                                 *
*                                   |                                 *
*        _______________________________________________________      *
*             |                     |                    |            *
*        -------------        -------------        -------------      *
*        | PRINT     |        |  UPDIPL   |        |  SUBMIT   |      *
*        | ROUTINE   |        |  ROUTINE  |        |  ROUTINE  |      *
*        |           |        |           |        |           |      *
*        -------------        -------------        -------------      *
*        -------------        -------------        -------------      *
*        |  LIST     |        |  LINENUM  |        |   SAVE    |      *
*        |  ROUTINE  |        |   ROUTINE |        |  ROUTINE  |      *
*        |           |        |           |        |           |      *
*        -------------        -------------        -------------      *
*        -------------        -------------        -------------      *
*        |  FIND     |        |           |        |           |      *
*        |  ROUTINE  |        |           |        |           |      *
*        |           |        |           |        |           |      *
*        -------------        -------------        -------------      *
*                                                                     *
*        SUBROUTINES:                                                 *
*                    TSTVSALL:  ALLOCATE THE LIBRARY                  *
*                    TSTVSCOM:  PROCESS THE COMMANDS                  *
*                    TSTVSSEC:  check security                        *
*                                                                     *
*        Note: Assemble TSTVS with SYSPARM(NOSEC) to bypass           *
*              the security check of RACF or ACF2                     *
*                                                                     *
*                                                                     *
*------------------------------------------ (C)-2019 SKYBIRD SYSTEMS -*
         EJECT
*---------------------------------------------------------------------*
*                                                                     *
*        COPYRIGHT 77/01/11, Rob Prins                                *
*        LAST UPDATE 79/11/02  BY ROB PRINS                           *
*        ADD R SUBCOMMAND (REPEAT).                                   *
*        LAST UPDATE AT 79/12/04 BY ROB PRINS:                        *
*        CHANGE THE DESIGN OF TSTVS AND ADD ALLOC COMMAND             *
*                                                                     *
*        LAST UPDATE AT 80/04/22 BY ROB PRINS                         *
*        CHANGE START COMMAND FOR RDRTST FROM:                        *
*        "S RDRTST,M=MEMBERNAME" INTO                                 *
*        "S RDRTST,D=DATASETNAME,M=MEMBERNAME" FOR VS1 AND SVS SUBMIT.*
*        ERROR SOLVED IN UPDIPL ROUTINE        /* 80.08.26 */         *
*                                                                     *
*        LAST UPDATE AT 85/05/13 BY ROB PRINS                         *
*        ADD 'XLST' SUBCOMMAND. THIS COMMAND IS AN EXTENTION OF THE   *
*        LIST SUBCOMMAND. WITH 'XLST' THE WHOLE 80 COLUMS OF THE      *
*        RECORDS WILL BE DISPLAYED.                                   *
*                                                                     *
*        LAST UPDATE AT 86/02/12 BY ROB PRINS                         *
*        ADD 'L' SUBCOMMAND. THIS IS AN ALIAS OF THE 'LIST' SUBCOMMAND*
*        ADD 'R' COMMAND, RENAME OF MEMBERS SYNTAX: 'R MEMBER,NEWNAME'*
*        ADD 'F' SUBCOMMAND, SYNTAX: 'F $STRING$' INITIAL FIND, OR    *
*                'F' REPEAT FIND.                                     *
*        AFTER 'SAVE' SUBCOMMAND, LEAVE SUBCOMMAND MODE.              *
*                                                                     *
*        LAST UPDATE AT 87/04/23 BY ROB PRINS                         *
*        DISPLAY WHOLE LINE AFTER MATCH FOUND IN 'F' SUBCOMMAND       *
*        INSTEAD OF THE FIRST 55 POSITIONS                            *
*        REJECT 'SUBMIT' AND 'PRINT' SUBCOMMAND IF TSTVS HAS STARTED  *
*        VIA OPER.                                                    *
*                                                                     *
*        UPDATE BY ROB PRINS AT 91/12/18                              *
*        RETRY MAXIMUM 6 ABENDS TO PREVENT RECURSIVE ABENDS           *
*        ABEND90A SOLVED AFTER FIRST ABEND913 DURING OPEN             *
*        REJECT 'SUBMIT' AND 'PRINT' SUBCOMMAND IF JOBNAME=EDIT       *
*        SUBCOMMAND 'C ALL,...' ADDED                                 *
*                                                                     *
*        Update by Rob Prins at dec 24,1991 Christmas Supprise        *
*        ACF2 support added: TSTVS will ask for an authorized LOGONID *
*        and password. If the verification is successful, the LOGONID *
*        will be checked for the 'TSTVS' privilege (LIDI2F5)          *
*        If ACF2 is not active, the check will be bypassed and the    *
*        program password will be asked.                              *
*        MAX workspace size increased from 2999 to 3999               *
*                                                                     *
*        Update by Rob Prins at 1992/12/18, Christmas Supprise        *
*        All the ACF2 macro's are deleted and replaced by RACROUTE    *
*        The TSTVS privilege has been replaced by a resource called   *
*        '$TSTVS' in the FACILITY class. So TSTVS can be used in both *
*        an ACF2 or a RACF environment                                *
*        The complete security check is now done in module TSTVSSEC   *
*                                                                     *
*        Update by Rob Prins at Jul 26,1993                           *
*        The "Key" in   "PDSLIST,key" has now a variable length       *
*                                                                     *
*        Update by Rob Prins at Jul 29,1993                           *
*        Add the RENAME command to rename datasets                    *
*        Remove the VS1 support. TSTVS assumes, that the operating    *
*        system is always MVS (ESA)                                   *
*        MSGID's changed from 'JCL' into 'EDT'.                       *
*                                                                     *
*        Update by Rob Prins at Sept 04,2019                          *
*        Remove the LINK to program SYP120, This program is not       *
*        a part of TSTVS, so this LINK is changed into a SVC 34       *
*                                                                     *
*------------------------------------------ (C)-2019 SKYBIRD SYSTEMS -*
         SPACE 2
TSTVS    CSECT
         SAVE  (14,12),,*
         USING TSTVS,R15               GET LOCAL ADDRESSABILITY @911224
         LA    R11,SAVEA                                        @911224
         ST    R13,SAVEA+4                                      @911224
         ST    R11,8(0,R13)                                     @911224
         LR    R13,R11                                          @911224
         B     START                                            @911224
         DS    0F                                               @911224
SAVEA    DC    18F'-1'
         DC    CL8'&SYSDATE',C' ',CL8'&SYSTIME'
START    DS    0H
         DROP  R15                     KILL LOCAL ADDRESSABILITY@911224
         LA    R11,2048(,R13)          Setup second             @911224
         LA    R11,2048(,R11)                      base         @911224
         LA    R10,2048(,R11)          Setup third              @911224
         LA    R10,2048(,R10)                      base         @911224
         LA    R9,2048(,R10)           Setup fourth             @911224
         LA    R9,2048(,R9)                        base         @911224
         USING SAVEA,R13,R11,R10,R9    GET ADDRESSABILITY MODULE@911224
         L     R3,0(,R1)               SAVE PARM
         WTO   'EDT001I TSTVS Version 3.3 - &SYSDATE started',         *
               DESC=2,ROUTCDE=(2,3,7,8,11)                      @930729
         ST    R1,MSGID                save it's msgid          @930729
         CLC   0(2,R3),=H'0'           no parm in exec stmnt?
         BE    TSTVSRAC                BRANCH TO security mod   @921218
         LH    R6,0(,R3)               LOAD LENGTH
         LA    R3,2(,R3)               SKIP LENGTH
         LR    R7,R6
         CH    R6,=H'2'                LENGTH LONGER THAN 2
         BH    MESS                    YES: ERROR MSG AND TAKE DEF
LOOP     CLI   0(R3),C'0'              NUMERIC TEST
         BL    MESS                    TAKE DEFAULTS IF NOT NUMERIC
         LA    R3,1(,R3)
         BCT   R6,LOOP                 BRANCH FOR SECOND TEST
         SR    R3,R7                   RESTORE ADDRESS PARM
         BCTR  R7,0
         EX    R7,COMPPRM
         BNH   MESS                    TAKE DEFAULT ROUTCODES
         MVI   ZZZ-2,X'00'
         EX    R7,PACK1
         CVB   R6,DWB                  CONVERT PARM
         BCTR  R6,0                    NO SHIFT IS ROUTING CODE-1
         XR    R7,R7
         IC    R7,=X'80'               SET HIGH ORDER BIT BITS 24-31
         SLL   R7,8                    SHIFT 8 BITS TO THE LEFT
         SRL   R7,0(R6)                SHIFT TO CORRECT ROUTCDE
         STH   R7,ZZZ-2                STORE
         STH   R7,BBB-4                  ROUTING
         STH   R7,CCC-4                     CODES
         STH   R7,DDD-4                       IN
         STH   R7,FFF-4                         WTOR
         STH   R7,GGG-4                           EXPANSIONS
         STH   R7,HHH-4
         STH   R7,III-4
         STCM  R7,3,PARMROUT
         B     TSTVSRAC                BRANCH TO PASSWORD ROUT. @921218
MESS     DS    0H
         LA    R1,MSG99
         MVC   MSG99+4(60),MSG03       MSG EDT007E (DEFAULT ROUTCDE)
         WTO   MF=(E,(1))
         B     TSTVSRAC                                         @921218
COMPPRM  CLC   0(0,R3),=C'00'          EXECUTE ONLY
*---------------------------------------------------------------------*
* first call 'TSTVSSEC' to check the security
*---------------------------------------------------------------------*
TSTVSRAC DS    0H                                               @921218
         EXTRACT ADDRTIOT,FIELDS=TIOT  OBTAIN TIOT
         L     R3,ADDRTIOT             TIOT ADDRESS IN REG 3
         CLC   0(8,R3),=CL8'EDIT'      JOBNAME IS 'EDIT'        @911218
         BNE   TSTVSC                  NO: BRANCH               @911224
         OI    SW02,$OPER              MARK IT
TSTVSC   DS    0H                                               @921218
*---------------------------------------------------------------------*
*
*        If you want to bypass the security check with RACF or ACF2
*        please assemble TSTVS with SYSPARM(NOSEC)
*
*---------------------------------------------------------------------*
         AIF   ('&SYSPARM' EQ 'NOSEC').NOSEC
*        B     TSTVSOK                 #### BYPASS CHECK
         CALL  TSTVSSEC,((3))          Do first the sec. check  @921218
         LTR   R15,R15                 OK ??                    @921218
         BZ    TSTVSOK                 yes: use the facility    @921218
         L     R13,4(,R13)             pickup callers savearea  @921218
         RETURN (14,12),RC=8           terminate ....           @921218
.NOSEC   ANOP
TSTVSOK  DS    0H                                               @921218
         GETMAIN RU,LV=320000
         LR    R12,R1
         ST    R12,SAVEGETM            SAVE BEGINADDRESS
         STM   R9,R13,RECOVER          SAVE REGISTERS FOR ESTAE
         ESTAE STAEEXIT                ESTABLISH ESTAE ROUTINE
TSTVSST  DS    0H
         L     R4,SAVEGETM             ADDRESS OF WORKAREA
         L     R5,=A(320000)           LENGTH
         LR    R6,R4                   SAME ADDRESS IN R6
         L     R7,=X'FF000000'         PADDING CHAR X'FF'
         MVCL  R4,R6                   FILLUP AREA WITH X'FF'
         L     R12,SAVEGETM            RESTORE BEGINADDRESS
         CALL  TSTVSCOM,(PARM)         CALL COMMAND PROCESSOR
         B     BRLIST(R15)
BRLIST   B     TSTVSUPD                Update member
         B     TSTVSNEW                New member
         B     TSTVSDEL                Delete the member
         B     TSTVSUPD                Update the member
         B     TSTVSAL                 allocate library
         B     TSTVSEOJ                end of TSTVS
         B     TSTVSREN                Rename a dataset         @930729
TSTVSUPD DS    0H
         BAL   R14,RDDIR               TEST IF MEMBER PRESENT
         CLC   PARM33R(7),=C'PDSLIST'  LISTPDS ?
         BE    TSTVSST                 ASK FOR MEMBER-NAME.
         TM    SW01,SW2                MEMBER PRESENT IN DIRECTORY
         BNO   UPDERR                  GIVE ERROR MSG
         RDJFCB OUTDCB
         MVC   JFCB+44(8),PARM33M
         OI    JFCB+86,X'01'           MARK 'MEMBER IN PDS'
         OPEN  (OUTDCB),TYPE=J
         XR    R2,R2
         OI    SW02,$OPEN              MARK OPENED *RETRY ROUT* @911218
LOAD     DS    0H
         CH    R2,=H'3999'             MAXIMUM LINES REACHED ?
         BE    LOAD9                   YES: BRANCH
         GET   OUTDCB
         LR    R4,R1
         MVC   0(80,R12),0(R4)         STORE RECORD INTO WORKSPACE
         LA    R12,80(,R12)            Next entry in workspace
         LA    R2,1(,R2)               Count record
         B     LOAD
LOAD9    DS    0H
         CLOSE (OUTDCB)
         FREEPOOL OUTDCB               FREE THE BUFFERS
         NI    SW02,255-$OPEN          OUTDCB closed now        @911218
         ST    R2,REG2                 Save GR2
         B     CHANGE
UPDERR   MVC   MSG99+4(60),MSG05
         LA    R1,MSG99
         WTO   MF=(E,(1))              WRITE ERROR MSG EDT010E
         B     TSTVSST                 ASK FOR COMMAND
TSTVSNEW DS    0H
         L     R12,SAVEGETM
         XR    R2,R2                   ZERO IN COUNT REG
TSTVSINP DS    0H
         XC    ECB1,ECB1
         MVC   CARD(80),BLANKS
         WTOR  'EDT003A Enter data, ''SAVE'' OR ''SUBMIT''',           X
               REPLY,72,ECB1,ROUTCDE=(2,3)
BBB      WAIT  ECB=ECB1
         OC    REPLY(72),BLANKS        CHANGE IT TO CAPITALS
         CLC   REPLY(4),=C'SAVE'       SAVE WORKSPACE IN MEMBER ?
         BE    TSTVSCLS                YES: BRANCH
         CLC   REPLY(6),=C'SUBMIT'     SUBMIT ?
         BE    TSTVSCLS                YES: SAVE AND SUBMIT
         OI    SW01,$CHANGED           CHANGE SWITH ON
         TM    PARMSW,NBRS
         BNO   NOSEQ
TSTVSSEQ XC    ECB1,ECB1
         WTOR  'EDT004A Enter sequence number ',                       X
               REPLY2,8,ECB1,ROUTCDE=(2,3)
CCC      WAIT  ECB=ECB1
         LA    R7,8                    MAXIMUM REPLY LENGTH
         XR    R6,R6                   REG 6 ZERO
         LA    R8,REPLY2               LOAD REPLY ADDRESS
SEQ002   DS    0H
         CLI   0(R8),X'40'             Blank found ?
         BE    SEQ007
         CLI   0(R8),X'F0'             NUMERIC TEST
         BL    SEQ004
         CLI   0(R8),X'F9'             NUMERIC TEST
         BH    SEQ004
         LA    R8,1(,R8)               Load next character
         LA    R6,1(,R6)               Length = length+1
         BCT   R7,SEQ002
         B     SEQ007
SEQ004   DS    0H                      Error found in seq. nbr
         LA    R1,MSG99
         MVC   MSG99+4(60),MSG04
         WTO   MF=(E,(1))              WRITE ERROR MSG EDT008E
SEQ005   DS    0H
         MVC   REPLY2(8),BLANKS        CLEAR REPLY AREA
         B     TSTVSSEQ                ASK FOR NEW CARDNUMBER
SEQ007   DS    0H
         LTR   R6,R6                   TEST IF LENGTH ZERO
         BZ    SEQ005                  BRANCH TO WTOR
         BCTR  R6,0                    FOR EX INSTRUCTION
         EX    R6,PACK2                PACK CARDNUMBER
         UNPK  REPLY2(8),DWB
         OI    REPLY2+7,X'F0'          CLEAR SIGN
NOSEQ    TM    PARMSW,UPDT
         BNO   TSTVSI9
         MVC   0(80,R12),CARD
         B     CHANGE
TSTVSI9  DS    0H
         LA    R2,1(,R2)               Number of lines in workspace
         MVC   0(80,R12),CARD
         LA    R12,80(,R12)
         B     TSTVSINP
TSTVSCLS DS    0H
         BAL   R14,SAVE
         CLC   =C'SAVE ',REPLY         Save member requested ?  @930729
         BE    TSTVSST                 YES: end of TSTVS
         BAL   R14,SUB                 Else: submit first
         B     TSTVSST
TSTVSAL  DS    0H
         CALL  TSTVSALL,(PARM)         Allocate the dataset
         B     TSTVSST
TSTVSREN DS    0H                      Rename a dataset         @930729
         OI    PARMSW,$RENDS           Tell TSTVSALL, that a    @930729
*                                      rename should occur      @930729
         CALL  TSTVSALL,(PARM)         Rename the dataset       @930729
         NI    PARMSW,255-$RENDS       Reset flag               @930729
         B     TSTVSST                                          @930729
TSTVSDEL DS    0H                      DELETE MEMBER OR RENAME MEMBER
         B     DEL
TSTVSEOJ DS    0H                      Pickup workspace
         L     R12,SAVEGETM            LOAD ADDRESS WORKSPACE
         ESTAE 0                       KILL ESTAE ROUTINE
         OI    PARMSW,$UNALL           MARK UNALLOC ONLY BY TSTVSALL
         CALL  TSTVSALL,(PARM)
         FREEMAIN RU,LV=320000,A=(R12)
TSTVSEO2 DS    0H
         LA    R2,MSGID
         DOM   MSG=(2)                 delete the start msg     @930729
         L     R13,4(,R13)             Pickup callers save area
         RETURN (14,12),RC=0           BACK TO OS
PACK1    PACK  DWB,0(1,R3)             PACK ROUTING CODE ** EX ONLY **
PACK2    PACK  DWB,REPLY2(1)           PACK CARD NUMBER ** EX ONLY **
         TITLE 'U P D A T E   R O U T I N E'
CHANGE   DS    0H
         OI    SW02,$SUBMODE           MARK SUB COMMAND MODE
         NI    SW01,X'FF'-$LINEDEL     SW DEL. REQUESTED OFF
         XC    ECB1,ECB1
         MVC   REPLY4(60),BLANKS
         WTOR  'EDT005A Enter subcommand or ''HELP''',REPLY4,60,       X
               ECB1,ROUTCDE=(2,3)
DDD      WAIT  ECB=ECB1
         OC    REPLY4(60),BLANKS       TRANSLATE TO CAPITALS
         CLI   REPLY4,C' '
         BE    CHANGE                  NOTHING REPLIED.
         CLC   REPLY4(4),=C'END '      END OF SUBCOMMAND MODE
         BE    CHEND
         CLC   REPLY4(5),=C'SAVE '     SAVE CONTENTS ?
         BNE   CHEOJ                   NO TEST OTHER SUBCOMMANDS
         BAL   R14,SAVE                SAVE MEMBER
         B     CHEND3                  TERMINATE SUBCOMMAND MODE
CHEOJ    DS    0H
         CLC   REPLY4(3),=C'EOJ'       END OF JOB    ?
         BNE   CHSUB                   NO: TEST OTHER COMMANDS
         OI    SW01,$EOJ               SET FLAG
         B     CHEND                   BRANCH
CHSUB    DS    0H
         CLC   REPLY4(6),=C'SUBMIT'    SUBMIT THE WORKAREA ?
         BNE   CHADD                   NO TEST OTHER FONCTIONS
         OI    SW01,$WKSUBM            INDICATE WKAREA SUBMIT
         BAL   R14,SUB                 SUBMIT THE WORKAREA
         B     CHANGE                  NEW SUBCOMMAND
CHADD    DS    0H
         CLC   REPLY4(4),=C'ADD '      ADD JCL AFTER EOF WORKSPACE
         BNE   CHVFY                   NO: TEST OTHER FUNCTIONS
         LR    R12,R2                  # OF LINES IN WORKSPACE
         MH    R12,=H'80'              X RECORD LENGTH
         A     R12,SAVEGETM            + BEGIN ADRESS WORKSPACE
         NI    PARMSW,X'FF'-UPDT       SUMULATE NEW MEMBER
         B     TSTVSINP                ASK FOR INPUT STATEMENTS
CHVFY    CLI   REPLY4,C'V'             LIST LINE AFTER 'C' COMMAND
         BE    VFY
         CLC   REPLY4(4),=C'HELP'      HELP MSG EDT014A
         BNE   CHNG
         LA    R1,MSG99
         MVC   MSG99+4(60),MSG09
         WTO   MF=(E,(1))
         LA    R1,MSG99
         MVC   MSG99+4(60),MSG10       Write HELP lines
         WTO   MF=(E,(1))
         LA    R1,MSG99
         MVC   MSG99+4(60),MSG11
         WTO   MF=(E,(1))
         MVC   MSG99+4(60),MSG11A
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),MSG11B
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),MSG11C
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),MSG11D
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),MSG11E
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),MSG11F
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),MSG11G
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),MSG11H
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),MSG11I
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),MSG11J
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),MSG11K
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),MSG11L
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),MSG11M
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),MSG11N
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),MSG11O      LAST HELP LINE
         WTO   MF=(E,MSG99)
         B     CHANGE
CHNG     CLC   REPLY4(4),=C'LIST'      LIST WORKSPACE ? (FIRST 55 POS)
         BNE   CHNGL                   NO: TEST OTHER FUNCTIONS
         NI    SW01,255-$XLST          MARK NORMAL LIST
         BAL   R14,LIST                LIST ROUTINE
         B     CHANGE                  ASK FOR NEW SUBCOMMAND
CHNGL    CLC   REPLY4(2),=C'L '        LIST WORKSPACE ? (ALIAS OF LIST)
         BNE   CHNGXLST                NO: TEST OTHER FUNCTIONS
         NI    SW01,255-$XLST          MARK NORMAL LIST
         BAL   R14,LIST                LIST ROUTINE
         B     CHANGE                  ASK FOR NEW SUBCOMMAND
CHNGXLST CLC   REPLY4(4),=C'XLST'      LIST WORKSPACE ? (80 POS)
         BNE   CHNGNUM                 NO: TEST OTHER FUNCTIONS
         OI    SW01,$XLST              MARK EXTENDED LIST
         BAL   R14,LIST                LIST ROUTINE
         B     CHANGE                  ASK FOR NEW SUBCOMMAND
CHNGNUM  CLC   REPLY4(5),=C'RENUM'     CARDNUMBERS IN COLS 73-80 ?
         BE    RENUM                   YES: BRANCH
         CLC   REPLY4(5),=C'UNNUM'     REMOVE CARDNUMBERS ?
         BE    UNNUM                   YES: BRANCH
         CLI   REPLY4,C'C'             UPDATE INPLACE CHAR STRINGS ?
         BNE   CHINS                   NO: TEST OTHER FUNCTIONS
         CLC   =C'ALL',REPLY4+2        'CHANGE ALL' SUBCOMMAND
         BNE   CHSINGLE                NO: CHANGE SINGLE LINE
         LA    R4,1                    START WITH LINE=1
         L     R12,SAVEGETM            LOAD ADDR WORKSPACE
         OI    SW02,$CHALL             MAKE CHANGE ALL COMMAND  @911218
         OI    SW02,$NOMATCH           DEFAULT NO MATCH FOUND   @911218
*--------------------------------------------------------------------*
* FLAG '$NOMATCH' WILL BE RESET IF THE FIRST CHARACTER STRING IS
* REPLACED
*---------------------------------------------------------------------*
CHALL1   DS    0H                                               @911218
         CLC   0(2,R12),=X'FFFF'       E.O.F.?                  @911218
         BE    CHALL9                  YES: TEST RC             @911218
         CVD   R4,DWB                  CONVERT LINE TO DECIMAL  @911218
         UNPK  REPLY4+2(3),DWB         MAKE ZONED               @911218
         OI    REPLY4+4,X'F0'          REMOVE SIGN              @911218
         BAL   R14,UPDIPL              UPDATE INPLACE ROUTINE   @911218
         LA    R4,1(,R4)               Next line number         @911218
         LA    R12,80(,R12)            Next record in workspace @911218
         B     CHALL1                  LOOP UNTIL ALL LINES DONE@911218
         MVC   MSG99+4(60),MSG12                                @911218
CHALL9   DS    0H                                               @911218
         TM    SW02,$NOMATCH           NO LINE'S CHANGED ?      @911218
         BNO   CHANGE                  NO: BRANCH, YES: GIVE MSG@911218
         MVC   MSG99+4(60),MSG12       MOVE MSG 'TEXT NOT FOUND'@911218
         LA    R1,MSG99                                         @911218
         WTO   MF=(E,(1))              GIVE MSG 'TEXT NOT FOUND'@911218
         B     CHANGE                  ASK FOR NEXT SUBCOMMAND  @911218
CHSINGLE DS    0H
         NI    SW02,255-$CHALL         NO 'C ALL' SUBCOMMAND    @911218
         BAL   R14,UPDIPL              UPDATE INPLACE ROUTINE
         B     CHANGE
CHINS    DS    0H
         CLI   REPLY4,C'I'             LINE INSERTION ?
         BNE   CHREPEAT                NO: TEST OTHER FUNCTIONS
         B     INSERT                  INSERT/DELETE/REPEAT ROUTINE
CHREPEAT DS    0H
         CLC   REPLY4(2),=C'R '        REPEAT THE LINE BEHIND ITSSELF?
         BNE   CHDEL                   TEST OTHER SUBCOMMANDS
         B     INSERT                  INSERT THE REPEATED LINE
CHDEL    DS    0H
         CLI   REPLY4,C'D'             DELETE LINE OR RANGE ?
         BNE   CHPRT
         OI    SW01,$LINEDEL           SW DEL. REQUESTED
         B     INSERT
CHPRT    DS    0H
         CLC   REPLY4(5),=C'PRINT'     Print DATASET ?
         BNE   CHFIND
         BAL   R14,PRT                 BRANCH TO PRINT ROUTINE.
         B     CHANGE
CHFIND   DS    0H
         CLC   REPLY4(2),=C'F '        'F' (FIND) SUBCOMMAND ?
         BNE   NUM                     NO: THEN LINE NUMBER
         BAL   R14,FIND                EXEC FIND ROUTINE
         B     CHANGE
NUM      DS    0H
         LA    R1,REPLY4               OFFSET FOR LINENUM
         BAL   R14,LINENUM             GET LINENUMBER
         LTR   R15,R15                 ERROR IN LINENUM ?
         BNZ   CHANGE                  NEXT SUBCOMMAND
         L     R5,LINENR1              OBTAINED LINENBR
         BCTR  R5,0
         MH    R5,=H'80'
         L     R12,SAVEGETM
         AR    R12,R5
         MVC   MSG99+4(60),0(R12)
         LA    R1,MSG99
         WTO   MF=(E,(1))
         B     TSTVSINP
RENUM    DS    0H
         L     R12,SAVEGETM            LOAD GETM. AREA
         ZAP   DWB,=PL2'10'            FIRST LINE NUMBER
RENUM2   CLC   0(2,12),=X'FFFF'        END OF DATA
         BE    WTONUM                  GIVE MSG EDT013I
         UNPK  72(8,R12),DWB
         OI    79(R12),X'F0'
         AP    DWB,=PL2'10'            INCREMENT VALUE
         LA    R12,80(,R12)            Next record
         B     RENUM2
UNNUM    DS    0H
         L     R12,SAVEGETM            LOAD ADDR WORKSPACE
UNNUM2   DS    0H
         CLC   0(2,R12),=X'FFFF'       E.O.F.?
         BE    CHANGE
         MVC   72(8,R12),BLANKS        REMOVE LINE NUMBERS
         LA    R12,80(,R12)            Next record
         B     UNNUM2
WTONUM   DS    0H
         LA    R1,MSG99
         MVC   MSG99+4(60),MSG08
         WTO   MF=(E,(1))
         B     CHANGE                  ASK FOR NEW SUBCOMMAND
VFY      DS    0H                      LIST OR DONT LIST LINE AFTER
*                                      'C' SUBCOMMAND
         CLC   REPLY4+2(3),=C'ON ' LIST ?
         BE    VFY2
         MVC   REPLY4+2(3),=C'OFF'     MOVE DEFAULT
         NI    SW01,X'FF'-$VFY         SET LISTD ID OFF
         B     VFY3
VFY2     OI    SW01,$VFY               SET LIST ID ON
VFY3     DS    0H                      ISSUE MSG EDT022I
         MVC   ONOFF(3),REPLY4+2       MOVE VERIFY OPERAND
         MVC   MSG99+4(60),MSG18       MOVE IN MSG
         WTO   MF=(E,MSG99)            INFORM OPERATOR
         B     CHANGE
CHEND    DS    0H                      PROCESS 'END'
         TM    SW01,$CHANGED           NOT ALL DATA SAVED ?
         BNO   CHEND3                  BRANCH IF ALL SAVED
CHEND2   XC    ECB1,ECB1               CLEAR ECB
         MVC   REPLY8,BLANKS           BLANK REPLY AREA
         WTOR  'EDT025A *WARNING* nothing saved, Enter ''SAVE'' or ''ENX
               D''',REPLY8,4,ECB1,ROUTCDE=(2,3)
III      WAIT  ECB=ECB1                WAIT FOR EVENT
         CLC   REPLY8(4),=C'END '      NOTHING SAVE FORCED ?
         BE    CHEND3                  SAVE NOTHING IF YES
         CLC   REPLY8(4),=C'SAVE'      SAVE ?
         BNE   CHEND2                  WRONG REPLY
         BAL   R14,SAVE                SAVE THE MEMBER
CHEND3   DS    0H
         NI    SW01,255-$CHANGED       CHANGE SWITCH OFF
         NI    SW02,255-$SUBMODE       RESET SUBCOMMAND MODE FLAG
         TM    SW01,$EOJ               EOJ SUBCOMMAND ENTERED ?
         BO    TSTVSEOJ                YES: TERMINATE TSTVS
         B     TSTVSST                 ASK FOR NEW COMMAND
         TITLE 'L I S T        ROUTINE'
LIST     DS    0H
         STM   R14,R12,SAVER           SAVE REGISTERS
         L     R12,SAVEGETM            STORE ADDR WORKSPACE
         LA    R1,REPLY4+5             PARAMETER FOR LINENUM
         CLC   REPLY4(2),=C'L '        ALIAS OF 'LIST' ??
         BNE   LIST1                   NO 'LIST' OR 'XLST'
         CLI   REPLY4+2,X'40'          NO LINE NUMBER ?
         BE    LIST2                   LIST ENTIRE DATASET
         LA    R1,REPLY4+2             'L' COMMAND (ALIAS OF 'LIST')
         B     LIST1A                  BRANCH
LIST1    DS    0H
         CLI   REPLY4+5,C' '           NO LINE NUMBERS ??
         BE    LIST2                   LIST ENTIRE DATASET
LIST1A   DS    0H
         BAL   R14,LINENUM             OBTAIN LINENBRS OPERANDS
         LTR   R15,R15                 OK
         BNZ   LISTEND
         B     LIST4
LIST2    DS    0H
         ZAP   PACKFLD,=PL3'0'         *** LIST ENTIRE WORKSPACE ***
         ZAP   WTOCNT,=PL2'0'          COUNTER FOR EDT017A CONT MSG
         LH    R3,=H'3999'             MAX # OF LINES IN WORKSPACE
         MH    R3,=H'80'
         AR    R3,R12                  LAST LINE TO DISPLAY
LIST3    DS    0H
         CLC   0(2,R12),=X'FFFF'       EOF WORKSPACE
         BE    LISTEND
         CR    R12,R3                  OUT OF LINE RANGE ?
         BH    LISTEND                 END OF LIST ROUT
         AP    PACKFLD,=PL1'1'
         CP    WTOCNT,=P'16'           16 LINES DISPLAYED ??
         BL    LIST3#                  NO: DISPLAY NEXT LINE
         ZAP   WTOCNT,=PL2'0'          ZERO IN COUNTER
LISTCONT DS    0H
         MVC   REPLY6,BLANKS           CLEAR REPLY AREA
         XC    ECB1,ECB1               CLEAR ECB LIST
         WTOR  'EDT017A Should TSTVS continue this function? Reply Y OrX
                N ',REPLY6,3,ECB1,ROUTCDE=(2,3)
FFF      WAIT  ECB=ECB1
         OC    REPLY6,BLANKS           XLATE TO UPPERCASE
         CLC   REPLY6(2),=C'Y '        CONTINUE ?
         BE    LIST3#                  YES: LIST ANOTHER 16 LINES
         CLC   REPLY6(2),=C'N '        TERMINATE THE DISPLAY
         BNE   LISTCONT                NO: BRANCH TO WTOR
         B     LISTWTO2                YES: GIVE MSG EDT018I
LIST3#   DS    0H
         TM    SW01,$XLST              EXTENDED LIST ?
         BNZ   LIST3B                  YES: DO THAT
LIST3A   DS    0H
         UNPK  MSG99+4(4),PACKFLD      LINE NUMBER BEFORE LINE
         OI    MSG99+7,X'F0'           CLEAR SIGN
         MVI   MSG99+8,X'40'
         MVC   MSG99+9(55),0(R12)      1ST 55 POSITIONS ON CONSOLE
         LA    R1,MSG99
         WTO   MF=(E,(1))
         AP    WTOCNT,=PL1'1'
         LA    R12,80(,R12)            Next record
         B     LIST3
LIST3B   DS    0H
         MVC   MSG99+4(60),BLANKS      BLANK MSG FIELD
         UNPK  MSG99+4(4),PACKFLD      LINE NUMBER BEFORE LINE
         OI    MSG99+7,X'F0'           CLEAR SIGN
         MVC   MSG99+9(7),=C'01...40'  FIRST 40 COLUMNS
         MVC   MSG99+17(40),0(R12)     1ST 40 POSITIONS ON CONSOLE
         LA    R1,MSG99
         WTO   MF=(E,(1))
         AP    WTOCNT,=PL1'1'
         MVC   MSG99+4(4),=C'....'     START 2ND PART OF LINE
         MVC   MSG99+9(7),=C'41...80'  SECOND 40 COLUMNS
         MVC   MSG99+17(40),40(R12)    2ND 40 POSITIONS ON CONSOLE
         LA    R1,MSG99
         WTO   MF=(E,(1))
         AP    WTOCNT,=PL1'1'
         LA    R12,80(,R12)            Next record
         B     LIST3
LIST4    DS    0H                      *** LIST LINE RANGE *****
         L     R2,LINENR1              START LINE
         CVD   R2,DWB                  MAKE DECIMAL
         ZAP   PACKFLD(3),DWB
         SP    PACKFLD,=P'1'           MINUS 1
         BCTR  R2,0                    DECREASE WITH ONE
         MH    R2,=H'80'               X RECORD LENGTH
         AR    R12,R2
         L     R3,LINENR2              SECOND LINE NUMBER
         BCTR  R3,0                    DECREASE WITH ONE
         MH    R3,=H'80'               X RECORD LENGTH
         A     R3,SAVEGETM             LAST LINE TO DISPLAY
         ZAP   WTOCNT,=PL2'0'
         B     LIST3
LISTWTO2 DS    0H                      MSG EDT018I *****
         MVC   MSG99+4(60),MSG14
         LA    R1,MSG99
         WTO   MF=(E,(1))
LISTEND  DS    0H
         LM    R14,R12,SAVER           RESTORE THE REGISTERS
         BR    R14                     RETURN TO CALLER
         TITLE 'P R I N T        ROUTINE'
*---------------------------------------------------------------------*
*        ROUTINE TO PRINT THE WORKSPACE TO CHECK THE                  *
*        CHANGES MADE BY THIS PROGRAM                                 *
*        THE PRINT DATASET IS A SPIN-OFF SYSOUT DATASET               *
*        THIS ROUTINE IS ONLY SUPPORTED BY M.V.S.                     *
*------------------------------------------ (C)-1993 SKYBIRD SYSTEMS -*
         SPACE 2
PRT      DS    0H                      PRINT ROUTINE
         STM   R14,R12,SAVER           SAVE THE REGISTERS
         XC    SYSINT(4),SYSINT        CLEAR INTRDR SPECIFICATION
         TM    SW02,$OPER              ISSUED BY OPER ? (PRIM SUBSYS)
         BNZ   PRTWTO3                 YES: BRANCH
         TRT   REPLY4+6(1),CLSTAB      TEST IF SYSOUT IS VALID
         BNZ   PRT001                  TAKE DEFAULT IF INVALID
         MVC   SYSCLS(1),REPLY4+6      MOVE SYSOUT CLASS OF REPLY
PRT001   DS    0H
         LA    R1,BLKPTR               PREPARE DYNAMIC ALLOCATION
         SVC   99                      ALLOCATE
         MVC   MSG19CLS,SYSCLS         MOVE SYSOUTCLASS IN MESSAGE
         MVI   SYSCLS,C'C'             MOVE DEFAULT SYSOUT CLASS
         LTR   R15,R15                 TEST IF ALLOC O.K. ?
         BNZ   PRTWTO2                 INFORM OPERATOR IF NOT
         MVC   PRTDCB+DCBDDNAM-IHADCB(8),ALCDDNAM
*                                      MOVE GENERATED DDNAME
         OPEN  (PRTDCB,(OUTPUT))
         MVC   HEADMEM,JFCB+44         MOVE MEMBER INTO HEADING
         L     R12,SAVEGETM            LOAD GETM AREA ADDRESS
         ZAP   PACKFLD,=PL1'0'         INITIALIZE LINE COUNTER
         ZAP   LINECNT,=P'99'          FORCE HEADING
         ZAP   PAGECNT,=P'0'           PAGE COUNTER
PRT01    DS    0H                      **** PRINT DATASET *****
         CLC   0(2,R12),=X'FFFF'       END OF AREA ?
         BE    PRT02                   PRINT OK
         CP    LINECNT,=P'54'          BOTTOM OF PAGE REACHED ?
         BL    PRT01A                  PRINT LINE IF NOT
         AP    PAGECNT,=P'1'           PAGECNT = PAGECNT +1
         UNPK  HEADCNT,PAGECNT         UNPACK THE RESULT
         OI    HEADCNT+2,X'F0'         CLEAR SIGN
         PUT   PRTDCB,HEADLINE         WRITE LINE
         MVC   LINE,LINE-1             CLEAR LINE
         PUT   PRTDCB,LINE             WRITE BLANK LINE
         ZAP   LINECNT,=P'2'           INITIALIZE LINECOUNT
PRT01A   DS    0H
         AP    PACKFLD,=PL1'1'         LINE COUNTER
         MVC   LINE,LINE-1             BLANK LINE
         UNPK  LINE+1(4),PACKFLD       TSTVS SEQ NUMBER
         OI    LINE+4,X'F0'            CLEAR SIGN
         MVC   LINE+10(80),0(R12)      CARD IN PRINTLINE
         PUT   PRTDCB,LINE             WRITE LINE
         AP    LINECNT,=P'1'           COUNT LINES
         LA    R12,80(,R12)            Next record
         B     PRT01                   PRINT NEXT LINE
PRT02    DS    0H                      CLOSE DATASET
         CLOSE PRTDCB                  CLOSE AND UNALLOCATE
         FREEPOOL PRTDCB               AND FREE THE BUFFERS
         UNPK  MSG19#L,PACKFLD         # OF LINES
         OI    MSG19#L+3,X'F0'
         MVC   MSG99+4(60),MSG19       MOVE IN MSG
         WTO   MF=(E,MSG99)            INFORM OPERATOR/PROGRAMMER
         B     PRTEND            E N D   O F   R O U T I N E
PRTWTO3  DS    0H                      ISSUE MSG EDT028E
         LA    R1,MSG99
         MVC   MSG99+4(60),MSG24       IS NOT POSSIBLE IF SUBSYSTEM
         WTO   MF=(E,(1))              INFORM OPERATOR
         B     PRTEND
PRTWTO2  DS    0H                      MSG EDT019E
         SYCONVHX IN=ERROR,OUT=ERRORBC,L=2
         SYCONVHX IN=INFO,OUT=INFOBC,L=2
         LA    R1,MSG99                DYN ALLOC FAILED
         MVC   MSG99+4(60),MSG15
         WTO   MF=(E,(1))              INFORM
PRTEND   DS    0H
         LA    R2,INTKEY               REPAIRE INTRDR SPECIFICATION
         LA    R2,0(,R2)               CLEAR HI ORDER BYTE
         ST    R2,SYSINT
         LM    R14,R12,SAVER           RESTORE THE REGISTERS
         BR    R14 RETURN TO CALLER
         TITLE 'DELETE OR RENAME MEMBERS IN DIRECTORY'
DEL      DS    0H                      IF $RENAME ON THEN RENAME
         MVC   NOT,=C'   '             ) ELSE DELETE
         MVC   RENDEL,=CL8' DELETED'   DEFAULT DELETE
         MVC   RTRNTEXT,BLANKS
         OPEN  (PDSDCB,(UPDAT))
         TM    PARMSW,$RENAME          RENAME MEMBER REQUESTED ?
         BZ    DEL2                    NO: DELETE MEMBER
         STOW  PDSDCB,PARM33M,C        RENAME MEMBER
         MVC   RENDEL,=CL8' RENAMED'   CHANGE TEXT
         NI    PARMSW,255-$RENAME      FLAG OFF
         B     DEL4                    BRANCH
DEL2     DS    0H
         STOW  PDSDCB,PARM33M,D        DELETE MEMBER
DEL4     DS    0H
         LTR   R15,R15                 TEST COMPLETION
         BZ    DEL6                    YES: DELETE OK
         MVC   NOT,=C'NOT'             ELSE PRODUCE ERROR MSG
         MVC   RTRNTEXT(12),=C' RETURNCODE '
         CVD   R15,DWB
         UNPK  RTRNTEXT+12(4),DWB
         OI    RTRNTEXT+15,X'F0'
DEL6     DS    0H
         CLOSE (PDSDCB)
         MVC   WTOMEM,PARM33M
         LA    R1,MSG99
         MVC   MSG99+4(60),MSG06
         WTO   MF=(E,(1))              Write 'MEMBER DELETED' message
         B     TSTVSST                 ASK FOR NEW COMMAND
         TITLE 'READ DIRECTORIES FROM PARTITIONED DATASET'
RDDIR    DS    0H
         STM   R14,R12,SAVER           SAVE REGISTERS
         NI    SW01,X'FF'-SW2
         CLC   PARM33R(7),=C'PDSLIST'  LIST DIRECTORIES OF PDS ?
         BE    PDSL03                  CHECK PRESENCE OF MEMBER
         OPEN  (PDSDCB)
         MVC   BLDLM,PARM33M           Move membername
         BLDL  PDSDCB,BLDLLIST         BLDL  ENTRY SUPPLIED NAME
         LTR   R15,R15                 TEST COMPLETION OF BLDL
         BNZ   PDSL01                  MEMBER NOT PRESENT
         OI    SW01,SW2                MARK MEMBER PRESENT
PDSL01   DS    0H
         CLOSE PDSDCB
         B     PDSL99                  END OF BLDL ROUTINE
PDSL03   DS    0H
         MVC   MSG13(60),BLANKS
         RDJFCB OUTDCB                 PROVIDE JFCB INFORMATION
         OPEN  PDSDCB
         MVC   MSG13(8),=C'EDT016I '   MOVE MSG ID
         MVC   MSG13+8(44),JFCB        MOVE DATASET NAME
         LA    R1,MSG99
         MVC   MSG99+4(60),MSG13
         WTO   MF=(E,(1))              Write header
         LA    R7,16
PDSL05   DS    0H
         READ  DECB,SF,PDSDCB,MEMBAREA,256
         CHECK DECB
         XR    R3,R3
         LA    R4,MEMBAREA
         LH    R3,0(,R4)               # of bytes
         SH    R3,=H'2'                SUBTR. COUNT
         LA    R4,2(,R4)               Point to first member
PDSL07   DS    0H
         LTR   R3,R3                    TEST IF COUNT ZERO
         BZ    PDSL05                  LAST MEMBER IN DIR BLOCK
         CLI   0(R4),X'FF'             LAST MEMBER ?
         BE    PDSL15                  YES: CLOSE PDS
         XR    R1,R1                   clear register           @930726
         IC    R1,PARM33KL             Keylength                @930726
         LTR   R1,R1                   No length ?              @930726
         BZ    PDSL08                  Yes: Branch              @930726
         EX    R1,PDSLCMP              Test with key            @930726
         BL    PDSL13                  NEXT MEMBER IF NOT REACHED
PDSL08   DS    0H                                               @930726
         BCT   R7,PDSL11
         LA    R7,16
PDSL09   DS    0H                                               @930726
         MVC   REPLY6,BLANKS           CLEAR REPLY AREA
         XC    ECB1,ECB1               CLEAR ECB
         WTOR  'EDT017A Should TSTVS continue this function? Reply Y OrX
                N ',REPLY6,3,ECB1,ROUTCDE=(2,3)
GGG      WAIT  ECB=ECB1
         OC    REPLY6,BLANKS           XLATE TO UPPERCASE
         CLC   REPLY6(2),=C'Y '        CONTINUE ?
         BE    PDSL11                  YES: LIST NEXT 16 LINES
         CLC   REPLY6(2),=C'N '        TERMINATE PDSLIST ?
         BNE   PDSL09                  NO: REISSUE WTOR
         MVC   MSG99+4(60),MSG14       GIVE MSG EDT018I
         LA    R1,MSG99                GIVE MSG EDT018I
         WTO   MF=(E,(1))              GIVE MSG EDT018I
         B     PDSL15                  TERMINATE PDSLIST
PDSL11   DS    0H
         MVC   MSG13(60),BLANKS
         MVC   MSG13+8(8),0(R4)        MOVE MEMBERNAME IN MSG
         LA    R1,MSG99
         MVC   MSG99+4(60),MSG13
         WTO   MF=(E,(1))              GIVE MESSAGE TO OPER
PDSL13   DS    0H
         SR    R5,R5                   REG-5 ZERO
         SR    R6,R6                   REG-6 ZERO
         IC    R5,11(R4)               NUMBER OF ADDITIONAL HALFWORDS
         IC    R6,=X'1F'               MAXIMUM OF ADDITIONAL HALFWORDS
         NR    R5,R6                   TEST OF ADD HW NOT GREATER 31
         AR    R5,R5                    MULTIPLY BY 2 BECAUSE HALFWORDS
         LA    R4,12(R5,R4)             LOAD NEXT MEMBER ENTRY
         SR    R3,R5  SUBTRACT LENGTH OF ADD HALFWORDS FROM COUNT
         SH    R3,=H'12'                SUBTRACT OFFSET FROM COUNT
         B     PDSL07                   RETURN
PDSL15   DS    0H
         CLOSE PDSDCB
PDSL99   DS    0H
         LM    R14,R12,SAVER     RESTORE THE REGISTERS
         BR    R14
PDSLCMP  CLC   0(0,R4),PARM33KY        TEST WITH KEY ** exec only **
         TITLE 'I N S E R T  /  D E L E T E  / R E P E A T   LINES'
INSERT   DS    0H
         CLC   REPLY4+2(3),=C'   '       NO LINE NUMBER
         BNE   IN002                   IF NOT THEN NUMERIC TEST
         XC    LINENR1,LINENR1         INSERT BEFORE FIRST LINE
         B     INOK
IN002    DS    0H                      NUMERIC TEST
         LA    R1,REPLY4+2             LOAD PARM LINENUM
         BAL   R14,LINENUM             OBTAIN LINENUMBER
         LTR   R15,R15                 ERROR ?
         BNZ   CHANGE                  END OF ROUTINE
INOK     L     R5,LINENR1              CONVERT LINE-NO TO BIN
         TM    SW01,$LINEDEL           DELETE LINE ?
         BNO   IN001                   BRANCH IF NOT
         LTR   R5,R5                   LINE 0 COULD NOT DEL.
         BZ    INERR                   THEN MSG EDT006E
         CLC   LINENR1,LINENR2         NO SECOND LINE NUMBER ?
         BE    IN000A                  BRANCH IF YES
         ST    R3,DELR3                SAVE R3 FOR OTHER USE
         L     R3,LINENR2              OBTAIN 2ND LINENR
         SR    R3,R5                   2ND LINENR - 1ST LINENR
         LA    R3,1(,R3)               +/+ 1 FOR BCT LOOP
         B     DEL001                  GO TO DELETE ROUT
IN000A   DS    0H                      ONLY FIRST LINENR
         ST    R3,DELR3                SAVE R3
         LA    R3,1                    1 FOR BCT
         B     DEL001
IN001    DS    0H
         LR    R8,R2                   SAVE COUNT REG
         LA    R8,1(,R8)
         CH    R8,=H'3999'
         BH    INERR2                  LINE EXEEDS MAX LINES
         CLC   REPLY4(2),=C'R '        REPEAT FUNCTION ?
         BNE   IN001A                  ELSE INSERT
         OI    SW01,$CHANGED           CHANGE SWITCH ON
         LTR   R5,R5                   LINE NUMBER 0 SPEC. OR OMITTED
         BZ    IN001A                  THEN REPEAT LINE 1.
         BCTR  R5,0                    ELSE REPEAT SPECIFIED LINE
IN001A   DS    0H
         MH    R5,=H'80'               MULTIPLY WITH RECORD LENGTH
         LR    R8,R5
         L     R12,SAVEGETM            STORE ADDR WORKSPACE
         AR    R12,R5                  SHIFT LINES BEHIND
         MVC   HELP1(80),0(R12)        THE LINE TO BE INSERTED
IN003    DS    0H
         CLC   HELP1(2),=X'FFFF'       LAST LINE
         BE    INEND
         LA    R12,80(,R12)            Next record in workspace
         MVC   HELP2(80),0(R12)        SAVE LINE
         MVC   0(80,R12),HELP1
         MVC   HELP1,HELP2
         B     IN003
INERR    DS    0H                      LINE-NUMBER INVALID
         LA    R1,MSG99
         MVC   MSG99+4(60),MSG02       MSG EDT006E
         WTO   MF=(E,(1))
         B     CHANGE                  TRY AGAIN
INERR2   DS    0H
         LA    R1,MSG99                LINES EXCEEDS MAX.
         MVC   MSG99+4(60),MSG07       MSG EDT012E
         WTO   MF=(E,(1))
         B     CHANGE                  ADVICE SPLIT MEMBER
*                                      INTO TWO MEMBERS
INEND    DS    0H
         LA    R2,1(,R2)               COUNT = COUNT +1
         CLC   REPLY4(2),=C'R '        REPEAT FUNCTION ?
         BE    CHANGE                  ASK FOR NEW SUBCOMMAND
         L     R12,SAVEGETM            STORE GETM.AREA
         AR    R12,R8                  RIGHT POSITION
         B     TSTVSINP                ASK FOR JCL-STMNT
DEL001   DS    0H                      DEL LINE FROM GETM.-AREA
         BCTR  R5,0
         MH    R5,=H'80'               R5 FROM INSERT
         L     R12,SAVEGETM
         AR    R12,R5
         ST    R12,DELR12              SAVE FOR 2ND USE
DEL001A  DS    0H
         MVI   DEL003+1,X'00'          CHANGE BC 15 INTO BC 0
         L     R12,DELR12
DEL002   DS    0H
         CLC   0(2,R12),=X'FFFF'       END OF FILE
         BE    DEL005                  DEL LINE   COMPLETED
         OI    SW01,$CHANGED           CHANGE SWITH ON
DEL003   DS    0H
         BC    0,DEL004
         OI    DEL003+1,X'F0'          CHANGE BC 0 INTO BC 15
         BCTR  R2,0                    DECREASE COUNT WITH 1
DEL004   DS    0H
         MVC   0(80,R12),80(R12)
         LA    R12,80(,R12)            Next record
         B     DEL002
DEL005   DS    0H                      NEXT LINE
         BCT   R3,DEL001A              NEXT LINE TO DELETE
         L     R3,DELR3                RESTORE R3
         B     CHANGE
DELR3    DS    F                       FOR SAVE R3
DELR12   DS    F
         TITLE 'CHANGE OLD CHAR. STRING IN NEW CHAR. STRING'
UPDIPL   DS    0H
*
*        Syntax: C xxx,string1$string2$
*
*        Register usage:
*
*        R2        REGISTER FOR WORKSPACE
*        R3        WORKREGISTER
*        R4        BCT  REGISTER
*        R5        LENGTH CHAR. STRING
*        R6        ADRESS CHAR STRING
*        R7        WORKREGISTER.
*                  ALL REGISTERS ARE SAVED
*
         STM   R14,R12,SAVER           Save registers
         CLC   REPLY4+2(3),=X'F0F0F0'  Test if line number is numeric
         BNH   UPDWTO1                 GIVE ERROR MSG EDT006E
         OI    SW01,$C#CMD             Mark 'C' Subcommand
         LA    R1,REPLY4+2             Parameter
         BAL   R14,LINENUM             Obtain line number
         NI    SW01,X'FF'-$C#CMD
         LTR   R15,R15                 Error?
         BNZ   UPDBK                   Yes: end of routine
         L     R2,SAVEGETM             Restore workspace address
         L     R3,LINENR1              Obtain line number
         BCTR  R3,0                    Decrease with 1
         MH    R3,=H'80'               * record length
         AR    R2,R3                   Right line found
         ST    R2,SAVELST              Save it's address
         LA    R4,25                   Search max. 25 times
         XR    R5,R5                   Count string length
         L     R6,OFFSET            OFFSET IN REPLY 4 FROM LINENUM RT
         LR    R8,R6
UPD01A   DS    0H
         CLI   0(R6),C'$'              End of character string?
         BE    UPD01B     (or UPD01C)  Yes: branch
         LA    R5,1(,R5)               # bytes length
         LA    R6,1(,R6)               Test next position
         BCT   R4,UPD01A               Try again
         B     UPDWTO2                 Msg EDT015E String(s) not found
UPD01B   DS    0H
         MVC   UPD01A+6(2),=S(UPD01C)  Move different branch addr.
         LR    R7,R5                   Save length old string
         LA    R4,25                   Search max 25 times.
         LA    R6,1(,R6)               New character string
         ST    R6,SAVER6
         XR    R5,R5                   Clear register 5
         B     UPD01A                  Test length of new string
UPD01C   DS    0H
         ST    R7,LENGTH1              SAVE LENGTH OLD CHAR STRING
         ST    R5,LENGTH2              SAVE LENGTH NEW CHAR STRING
         LTR   R7,R7                   LENGTH OLD CHAR STRING ZERO
         BZ    UPD02A                  BRANCH IF YES.
         LA    R4,79                   SEARCH MAX 79 TIMES
         BCTR  R7,0                    DECREASE WITH 1 FOR EX
         SPACE
*** SEARCH OLD CHAR STRING AND REPLACE INTO NEW CHAR STRING ***/
         SPACE
UPD2     DS    0H
         EX    R7,COMPARE              Test if string is present
         BE    UPD02A                  Replace into new string
         LA    R2,1(,R2)               Next character
         BCT   R4,UPD2                 Try again
         B     UPDWTO2                 Msg: EDT015E Strings not found
COMPARE  CLC   0(0,R2),0(R8)           << executed >>
UPD02A   DS    0H
         ST    R2,SAVER2               Save position in line
         L     R7,LENGTH1
         L     R5,LENGTH2
         SR    R7,R5                   LENGTH1-LENGTH2
         C     R7,=F'0'                Equal?
         BE    UPD3                    Yes: replace immediately
         BH    UPD02B                  Old string is longer
         L     R3,SAVER2               Restore position in line
         A     R3,LENGTH1              + length of old string
         ST    R3,SAVER3               SAVE END OF OLD STRING IN LINE
         LPR   R5,R7                   MAKE RESULT POSITIVE
         L     R4,SAVELST              LOAD FIRST POS CHANG. LINE
         MVC   CARD(80),0(R4)          SAVE LINE.
         LA    R4,72(,R4)              LAST POSITION IN LINE.
         SR    R4,R3                   LENGTH OF REMAINING DATA.
         SR    R4,R5
         AR    R3,R5
         L     R5,SAVELST              RESTORE FIRST POS OF LINE
         SR    R3,R5                   Offset
         LR    R5,R3                   Save offset
         LA    R3,CARD
         AR    R3,R5
         L     R5,SAVER3
         BCTR  R4,0
         C     R4,=F'0'                NEGATIVE ?
         BL    UPD3                    IF SO NO SHIFT REM DATA
         EX    R4,MOVE2                Shift the remaining data
         L     R4,SAVELST              Restore address line
         MVC   0(80,R4),CARD           Move in changed line
         B     UPD3                    Replace
UPD02B   DS    0H
         L     R3,SAVER2               Restore position.
         A     R3,LENGTH1              + length of old string
         L     R4,SAVELST              Load first position changed line
         LA    R4,72(,R4)              Last position in line (col 72)
         ST    R3,SAVER3               1st char after 'Old string'
         SR    R4,R3                   Length of remaining data
         BCTR  R4,0                    Discount for EX instruction
         MVC   CARD,BLANKS             Clear work-area
         C     R4,=F'0'                Last char of 1st str. in c 72?
         BL    UPD02B1                 Yes: do not move remaining data
         EX    R4,MOVE3                Move remaining data
UPD02B1  DS    0H
         SR    R3,R7
         AR    R4,R7                   Substitute blanks in rem. data
         EX    R4,MOVE4                Shift remaining data to the left
UPD3     DS    0H
****     REPLACE OLD STRING INTO NEW STRING     ***/
         NI    SW02,255-$NOMATCH    AT LEAST 1 ONE LINE CHANGED @911218
         L     R6,SAVER6            RESTORE R6.
         L     R5,LENGTH2
         LTR   R5,R5
         BZ    UPDEND                  New string has no length
         BCTR  R5,0                    Discount for EX instruction
         EX    R5,MOVE1                Move new character string
         OI    SW01,$CHANGED           Mark data changed
         B     UPDEND                  Return
MOVE1    MVC   0(0,R2),0(R6)           << executed >>
MOVE2    MVC   0(0,R3),0(R5)           << executed >>
MOVE3    MVC   CARD(0),0(R3)           << executed >>
MOVE4    MVC   0(0,R3),CARD            << executed >>
UPDWTO1  DS    0H
****     MSG EDT006E, INVALID LINE NUMBER       ***/
         MVC   MSG99+4(60),MSG02       Invalid line number
         LA    R1,MSG99
         WTO   MF=(E,(1))              Msg: EDT006E
         B     UPDBK
UPDWTO2  DS    0H
         TM    SW02,$CHALL             'CHANGE ALL COMMAND'?    @911218
         BO    UPDBK                   NO: DO NOT ISSUE MSG     @911218
****     MSG EDT015E    TEXT NOT FOUND          ***/
         MVC   MSG99+4(60),MSG12
         LA    R1,MSG99
         WTO   MF=(E,(1))              Give message
         B     UPDBK
UPDEND   DS    0H
         TM    SW01,$VFY               List the line after Change?
         BNO   UPDBK                   No: return
         L     R2,SAVELST              Address of changed line
         MVC   MSG99+4(60),0(R2)       Move into WTO
         LA    R1,MSG99
         WTO   MF=(E,(1))              Inform operator
UPDBK    MVC   UPD01A+6(2),=S(UPD01B)  Restore branch address
         LM    R14,R12,SAVER           Restore the registers
         BR    R14                     Return
LENGTH1  DS    F                       LENGTH OF OLD CHAR STRING
LENGTH2  DS    F                       LENGTH OF NEW CHAR STRING
SAVER2   DS    F                       SAVE REG 2.
SAVER3   DS    F                       SAVE REG 3.
SAVER6   DS    F                       SAVE REG 6.
         TITLE 'F I N D      ROUTINE'
*---------------------------------------------------------------------*
*        THIS ROUTINE EXECUTES THE 'F' SUBCOMMAND                     *
*        THE SYNTAX IS 1. 'F  $STRING$' OR 2. 'F'                     *
*        IF ONLY AN 'F' IS ENTERED, A REPEAT FIND OF THE PREVIOUS     *
*        CHARACTER STRING IS EXECUTED                                 *
*-------------------------------------------(C)-1993 SKYBIRD SYSTEMS--*
         SPACE 2
FIND     DS    0H
         STM   R14,R12,SAVER           SAVE THE REGISTERS
         CLI   REPLY4+2,C'$'           DELIMITER FOUND ?
         BNE   RFIND                   NO: ISSUE REPEAT FIND
         LA    R3,REPLY4+3             POINT TO FIRST BYTE OF STRING
         LA    R4,56                   MAX LENGTH OF STRING
         XR    R6,R6                   COUNT LENGTH
         MVC   RSTRING,BLANKS          CLEAR PREVIOUS STRING
         LA    R5,RSTRING
FIND001  DS    0H
         CLI   0(R3),C'$'              END DELIMITER FOUND ?
         BE    FIND007                 YES: BRANCH
         MVC   0(1,R5),0(R3)           MOVE 1 BYTE STRING
         LA    R3,1(,R3)               Next byte in string
         LA    R5,1(,R5)               Incr output address
         LA    R6,1(,R6)               Incr count
         BCT   R4,FIND001              REPEAT UNTIL END DLM FOUND
         MVC   MSG99+4(60),MSG21       MOVE MSG ID
         WTO   MF=(E,MSG99)            ISSUE MSG
         MVC   RSTRING,BLANKS          CLEAR PREVIOUS STRING
         B     FIND999
FIND007  DS    0H
         ST    R6,RLEN                 SAVE LENGTH
         L     R12,SAVEGETM            START ADDRESS
         ST    R12,RFINDAD             SAVE ADDRESS
         B     RFIND003                BRANCH
RFIND    DS    0H
         CLC   RSTRING,BLANKS          NO PREVIOUS STRING SPECIFIED?
         BNE   RFIND001                NO: CONTINUE
         MVC   MSG99+4(60),MSG22       MOVE MSG ID
         WTO   MF=(E,MSG99)            ISSUE MSG
         B     FIND999
RFIND001 L     R12,RFINDAD             START WITH LAST FOUND LINE
RFIND003 CLC   0(2,R12),=X'FFFF'       END OF WORKSPACE ?
         BE    RFINDERR                ERROR.
         LR    R3,R12                  COPY R12
         L     R6,RLEN                 LENGTH OF STRING
         BCTR  R6,0                    -/- FOR EX.
         LA    R4,80                   MAX BCT VALUE
         SR    R4,R6                   MINUS LENGTH OF STRING
         LA    R4,1(,R4)               + 1
RFIND005 DS    0H
         EX    R6,RFINDCMP             COMPARE WITH STRING
         BE    RFIND019                YES: FOUND, DISPLAY LINE
         LA    R3,1(,R3)               NEXT BYTE IN LINE
         BCT   R4,RFIND005             LOOP UNTIL ALL DONE OR FOUND
         LA    R12,80(,R12)            Next record
         B     RFIND003                BRANCH
RFIND019 DS    0H
         MVC   MSG99+9(55),0(R12)      MOVE CONTENTS OF FOUND LINE
         MVI   MSG99+8,C' '
         LR    R5,R12                  CURRENT ADDRESS
         XR    R4,R4                   CLEAR EVEN REG.
         S     R5,SAVEGETM             MINUS START ADDRESS
         D     R4,=F'80'               DIVIDE BY RECORD LENGTH
         LA    R5,1(,R5)               PLUS ONE
         CVD   R5,DWB                  MAKE IT PACKED
         UNPK  MSG99+4(4),DWB          MAKE IT ZONED
         OI    MSG99+7,X'F0'           AND CLEAR SIGN
         WTO   MF=(E,MSG99)            ISSUE MSG WITH FOUND LINE
         MVC   MSG99+9(25),55(R12)     2ND PART OF LINE
         MVC   MSG99+34(30),BLANKS
         WTO   MF=(E,MSG99)            ISSUE MSG WITH 2ND PART OF LINE
         LA    R12,80(,R12)            POINT AFTER FOUND LINE
         ST    R12,RFINDAD             SAVE ITS ADDRESS
         B     FIND999                 END OF ROUTINE
RFINDERR MVC   MSG99+4(60),MSG12       TEXT NOT FOUND
         WTO   MF=(E,MSG99)            TELL OPERATOR
FIND999  DS    0H
         LM    R14,R12,SAVER           RESTORE REGS
         BR    R14                     RETURN TO CALLER
RFINDAD  DC    F'0'
RLEN     DC    F'0'
RFINDCMP CLC   0(0,R3),RSTRING         *** EXECUTE ONLY ***
RSTRING  DC    CL56' '
         TITLE 'S U B M I T  ROUTINE'
*---------------------------------------------------------------------*
*                                                                     *
*        Routine to submit the job in the specified member in the     *
*        JOB execution queue.                                         *
*        An internal reader will be allocated to submit the job.      *
*        If the allocation fails, a S RDRTST command will be issued   *
*        to start a reader procedure to submit the job.            -  *
*                                                                     *
*------------------------------------------ (C)-2019 SKYBIRD SYSTEMS -*
         SPACE 2
SUB      DS    0H                      TO GET BOUNDARY.
         STM   R14,R12,SAVER           SAVE THE REGISTERS
         TM    SW02,$OPER              SUBMIT via SUB=MSTR?
         BNO   SUB000                  No: branch
         LA    R1,MSG99
         MVC   MSG99+4(60),MSG24       Is not possible is subsystem
         WTO   MF=(E,(1))              INFORM OPERATOR
         B     SUBEND2
SUB000   DS    0H
         LA    R1,BLKPTR               LOAD ALLOCATION CONTROL BLK
         SVC   99                      ****** ALLOCATE  *********
         LTR   R15,R15                 TEST IF ALLOC OK ?
         BNZ   SUB009                  ISSUE MSG AND LINK TO SYP120
         MVC   INTRDR+DCBDDNAM-IHADCB(8),ALCDDNAM
*                                      MOVE GENERATED DD-NAME
         OPEN  (INTRDR,(OUTPUT))       ALLOCATION O.K.
         L     R12,SAVEGETM            LOAD WORKSPACE
*        ** FILL ALLOCATED INTRDR DATASET ****
SUB001   DS    0H
         CLC   0(2,R12),=X'FFFF'       END OF DATA IN WORKSPACE ?
         BE    SUB003                  YES: CLOSE THE INTRDR
         PUT   INTRDR,(12)             WRITE
         LA    R12,80(,R12)            Next record in workspace
         B     SUB001                  Return
*        EOD WORKSPACE ****  JOB SUBMITTED M.V.S. *****
SUB003   DS    0H
         CLOSE INTRDR                  FREE =CLOSE
         FREEPOOL INTRDR               Free the buffers
         B     SUBEND                  GO TO END OF THIS ROUTINE
SUB009   DS    0H                      ALLOCATION FAILED.
         SYCONVHX IN=ERROR,OUT=ERRORBC,L=2
*        CONVERT DYN ALLOC ERRORCODE TO EBCDIC
         SYCONVHX IN=INFO,OUT=INFOBC,L=2
*        CONVERT DYN ALLOC INFOCODE TO EBCDIC   *****
         LA    R1,MSG99                PREPARE WTO
         MVC   MSG99+4(60),MSG15       PREPARE WTO
         WTO   MF=(E,(1))              GIVE MSG EDT019E
         SPACE
         MVI   PARMRST,C' '            CLEAR
         MVC   PARMRST+1(L'PARMRST-1),PARMRST  FIELD
         MVC   CMDL(2),=AL2(PARMRST-PARM10) initial command length
SUB010   DS    0H                      Start the rdr if alloc fails
         LA    R2,PARMRST              LOAD REST OF PARM
         LH    R3,CMDL                 LOAD LENGTH
         LA    R4,JFCB                 LOAD ADDRESS DSNAME
         LA    R5,44                   MAX. LENGTH OF DSNAME
SUB010A  DS    0H                      MOVE DSNAME IN PARM
         CLI   0(R4),C' '              DSNAME PASSED ?
         BE    SUB010B                 YES: END OF LOOP
         MVC   0(1,R2),0(R4)           MOVE POSITION
         LA    R2,1(,R2)               NEXT POSITION OUTPUT
         LA    R3,1(,R3)               Count in length
         LA    R4,1(,R4)               NEXT POSITION OUTPUT
         BCT   5,SUB010A
SUB010B  DS    0H                      DSNAME= IN PARM
         MVC   0(4,R2),=C''',M='       MOVE KEYWORD FOR MEMBER
         MVC   4(8,R2),JFCB+44         MOVE MEMBERNAME IN START CMD
         LA    R3,16(,R3)              COUNT LENGTH
         STH   R3,CMDL                 NEW LENGTH
*
*        Length is CMDL + actual length of DSNAME
*        + string ',M= + 8 bytes member name + 4 bytes prefix
*
         MODESET KEY=ZERO              Set key to zero for SVC 34
*
*        Issue SVC 34 to invoke the 'S RDRTST,D=...,M=...' command
*
         LA    R1,CMDL                 Parameter list
         XR    R0,R0
         SVC   34                      ISSUE THE COMMAND *
         MODESET KEY=NZERO             RESTORE PROGRAM STATUS
SUBEND   DS    0H                      END OF SUBMIT ROUTINE
         L     R12,SAVEGETM            BEGIN OF GETM AREA
         LA    R12,2(,R12)
         LA    R2,8                    MAX LENGTH OF JOBNAME
         MVC   JOBNAME(8),BLANKS       CLEAR JOBNAME
         LA    R3,JOBNAME
SUB012   DS    0H                      BUILD 'JOB SUBMITTED' MSG
         CLI   0(R12),C' '             END OF JOBNAME ?
         BE    SUB014
         MVC   0(1,R3),0(R12)          MOVE CHAR BY CHAR
         LA    R3,1(,R3)               Next position
         LA    R12,1(,R12)
         BCT   R2,SUB012               BRANCH
*             J O B          S U B M I T T E D ****
SUB014   DS    0H
         LA    R1,MSG99
         MVC   MSG99+4(60),MSG17       EDT021I JOB SUBMITTED
         WTO   MF=(E,(1))              INFORM OPERATOR
SUBEND2  DS    0H
         NI    SW01,X'FF'-$WKSUBM      SWITCH OFF
         LM    R14,R12,SAVER           RESTORE THE REGISTERS
         BR    R14                     RETURN
         TITLE 'S A V E    R O U T I N E'
SAVE     DS    0H
         STM   R14,R12,SAVER           SAVE REGISTERS
         L     R12,SAVEGETM
         NI    SW01,X'FF'-$CHANGED     CHANGE SWITH OFF
         RDJFCB OUTDCB
SAV      DS    0H
         MVC   REPLY7(8),BLANKS
         XC    ECB1,ECB1               CLEAR ECB AREA
         MVC   HHH-14(8),PARM33M       MEMBER NAME IN WTOR
         WTOR  'EDT026A Enter new name to save your workarea or reply 'X
               'U'' to save in member xxxxxxxx',                       X
               REPLY7,8,ECB1,ROUTCDE=(2,3)
HHH      WAIT  ECB=ECB1
         CLC   REPLY7(2),=C'U '
         BE    SAVE3                   SAVE UNDER NAME OF EDT009A
         CLI   REPLY7,C'$'             NATIONAL CHAR ALSO ALLOWED
         BE    SAV001                  BRANCH IF YES
         CLI   REPLY7,C'@'             ALSO NATIONAL CHARACTER
         BE    SAV001                  BRANCH
         CLI   REPLY7,C'#'             ALSO NATIONAL CHARACTER
         BE    SAV001                  BRANCH
         TM    REPLY7,X'C0'
         BNO   SAV
         TM    REPLY7,X'F0'   FIRST CHAR MUST BE ALPHABETIC
         BO    SAV
SAV001   DS    0H
         MVC   JFCB+44(8),REPLY7       NEWNAME
         B     SAVE5
SAVE3    DS    0H
         MVC   JFCB+44(8),PARM33M
SAVE5    DS    0H
         OI    JFCB+86,X'01'
         OPEN  (OUTDCB,(OUTPUT)),TYPE=J
SAVE6    DS    0H
         CLC   0(2,R12),=X'FFFF'
         BE    SAVE7
         MVC   CARD(80),0(R12)
         PUT   OUTDCB,CARD
         LA    R12,80(,R12)
         B     SAVE6
SAVE7    DS    0H
         L     R12,SAVEGETM
         CLOSE (OUTDCB)
         FREEPOOL OUTDCB
         MVC   MSG20M,JFCB+44          MOVE MEMBER NAME IN MSG
         MVC   MSG99+4(60),MSG20       MOVE IN MSG
         WTO   MF=(E,MSG99)
         LM    R14,R12,SAVER           RESTORE REGISTERS
         BR    R14                     RETURN
         TITLE 'REGULATE LINENUMBERS *** FREE FORMAT ***'
*---------------------------------------------------------------------*
*                                                                     *
*        SCAN THE LINENUMBERS, WHICH ARE OPERANDS IN THE SUBCOMMANDS  *
*        THE LINENUMBERS ARE MAX FOUR POSITIONS LONG.                 *
*        THE OFFSET IN REPLY4 (REPLY FOR SUBCOMMAND) IS PLACED        *
*        IN GENERAL REGISTER 1.                                       *
*        WHEN ONE OF THE LINE NUMBERS IS IN ERROR RC 4 IS GIVEN       *
*        IN REGISTER 15 AND THE MSG EDT006E IS GIVEN.                 *
*        WHEN THE OPERAND IS ONLY 1 LINENUMBER, LINENUMBER 2 IS THE   *
*        SAME AS LINENUMBER 1 (THESE ARE TWO BINARY FULLWORDS TO BE   *
*        GIVEN BACK).                                                 *
*                                                                     *
*------------------------------------------ (C)-2019-Skybird Systems -*
         SPACE 2
LINENUM  DS    0H                      Scan line numbers
         STM   R14,R12,SAVELIN         SAVE REGISTERS
         XC    LINERET,LINERET         Clear return code
         LR    R2,R1                   SAVE REGISTER 1
         ST    R2,OFFSRPL4             SAVE THE OFFSET
         XC    LINENR1,LINENR1         CLEAR FIELDS
         XC    LINENR2,LINENR2
         CLI   0(R2),C' '              NO LINENRS PROVIDED ?
         BE    LINEEND                 END OF ROUTINE IF YES
         LA    R4,4                    MAXIMUM LENGTH OF LINENUM
         XR    R5,R5                   CLEAR GR 5 FOR COUNT POS.
LINE001  DS    0H                      BCT LOOP
         CLI   0(R2),C' '              END OF LINENBR
         BE    LINE005                 ONLY 1 LINENBR PROVIDED
         CLI   0(R2),C'-'              LINENBR RANGE ?
         BE    LINE007                 2ND LINENBR TOO
         CLI   0(R2),C','              COMMA INSTEAD OF HYPHEN ?
         BE    LINE007                 2ND LINENBR TOO.
         CLI   0(R2),C'0'              NUMERIC TEST
         BL    LINEERR
         CLI   0(R2),C'9'              NUMERIC TEST
         BH    LINEERR                 ERROR ROUTINE
         LA    R5,1(,R5)               COUNT POSITIONS
         LA    R2,1(,R2)               NEXT POSITION
         BCT   R4,LINE001
         CLI   0(R2),C' '              ONLY 1 LINENBR
         BNE   LINE007                 BRANCH IF NOT
LINE005  DS    0H
         BAL   R6,LINEBIN              MAKE LINENBR BINARY
         ST    R7,LINENR1              AND STORE THE RESULT
         ST    R7,LINENR2              LINENR2 IS THE SAME AS LINENR1
         LA    R2,1(,R2)
         ST    R2,OFFSET               FOR 'C' SUBCOMMAND
         B     LINEEND
LINE007  DS    0H                      TWO LINENBRS
         TM    SW01,$C#CMD             'C' SUBCOMMAND ?
         BO    LINE005                 THIS SUBCOMMAND HAS NEVER A
*                                      )SECOND LINENUMBER
         LA    R2,1(,R2)
         ST    R2,OFFSET               FOR 'C' SUBCOMMAND
         BAL   R6,LINEBIN              MAKE BINARY
         ST    R7,LINENR1              AND STORE THE RESULT
         ST    R2,OFFSRPL4             SAVE THE OFFSET
         LA    R4,4                    FOR BCT INSTRUCTION
         XR    R5,R5                   CLEAR COUNT REGISTER
LINE009  DS    0H
         CLI   0(R2),C' '              END OF LINENBR 2
         BE    LINE011                 BRANCH IF YES
         CLI   0(R2),C'0'              NUMERIC TEST
         BL    LINEERR                 ERROR
         CLI   0(R2),C'9'              NUMERIC TEST
         BH    LINEERR                 ERROR TOO
         LA    R5,1(,R5)               COUNT POSITION
         LA    R2,1(,R2)               NEXT POSITION
         BCT   R4,LINE009
LINE011  DS    0H
         BAL   R6,LINEBIN              MAKE BINARY
         ST    R7,LINENR2              STORE THE RESULT
         CLC   LINENR2,LINENR1         CHECK VALIDITY OF RANGE
         BNL   LINEEND                 IF LOW INVALID RANGE
LINEERR  DS    0H                      LINENBR ERROR
         MVC   MSG99+4(60),MSG02       MSG EDT006E
         WTO   MF=(E,MSG99)            INFORM OPERATOR
         LA    R2,4
         ST    R2,LINERET              RETURNCODE
LINEEND  DS    0H
         LM    R14,R12,SAVELIN         RESTORE REGISTERS
         L     R15,LINERET             RETURNCODE
         BR    R14                     BACK TO CALLER
*
LINEBIN  DS    0H                      MAKE LINENBRS BINARY
         L     R3,OFFSRPL4             LOAD STARTADDRESS OF LINENBR
         LTR   R5,R5                   LENGTH ZERO ?
         BZ    LINEERR                 NO LINENBR FOUND
         BCTR  R5,0                    FOR EX INSTRUCION
         EX    R5,LINEPK               PACK THE LINE
         CVB   R7,DWB                  MAKE BINARY
         LTR   R7,R7                   CHECK FOR ZERO
         BZ    LINEERR                 ERROR IF ZERO
         C     R7,=F'3999'             MAX NUMBER OF LINES ?
         BH    LINEERR                 ERROR
         BR    R6                      RETURN
LINEPK   PACK  DWB(8),0(0,R3)          **** EXECUTE ONLY ****
         TITLE 'ESTAE EXIT ROUTINE'
STAEEXIT DS    0H
         DROP
         USING *,R15                   GET TEMPORARY ADDRESSABILITY
         USING SDWA,R1                 GET ADDRESSABILITY OVER SDWA
         LM    R9,R13,RECOVER
         DROP  R15
         USING SAVEA,R13,R11,R10,R9    RESTORE ADDRESSABILITY   @911224
         XR    R2,R2                   CLEAR REGISTER 2
         ICM   R2,7,SDWACMPC           INSERT COMPLETION CODE
         SRL   R2,12                   SHIFT OUT USER COMPLETION CODE
         STH   R2,SYSTEMCC             SAVE SYSTEM COMPLETION CODE
         CLI   RETRYCNT,5              MORE THAN 5 RETRIES?     @911218
         BNH   SETRP4                  NO: RETRY                @911218
         SETRP RC=0,DUMP=YES           ELSE PERCOLATE           @911218
         BR    R14                     BRANCH TO RTM            @911218
         DROP  R1                      KILL ADDRESSABILITY
SETRP4   DS    0H                                               @911218
         SETRP RC=4,RETADDR=RET,RETREGS=NO,FRESDWA=YES,DUMP=NO
         BR    R14
SYSTEMCC DS    H
         TITLE 'ESTAE RETRY ROUTINE'
RET      DS    0H
         DROP
         USING *,R15
         LM    R9,R13,RECOVER          RESTORE REGISTERS
         L     R2,REG2                 SAVE COUNT REG
         DROP  R15
         USING SAVEA,R13,R11,R10,R9    RESTORE ADDRESSABILITY   @911224
         XR    R1,R1                   CLEAR REGISTER           @911218
         IC    R1,RETRYCNT             # ABEND RETRIES          @911218
         LA    R1,1(,R1)               Increase                 @911218
         STC   R1,RETRYCNT             AND SAVE                 @911218
         SYCONVHX IN=SYSTEMCC,OUT=SAVECC,L=2
         MVC   MSG23+13(3),SAVECC+1    MOVE COMPLETION CODE
         MVC   MSG99+4(60),MSG23       MSG EDT027D (ABENDXXX DETECTED)
         WTO   MF=(E,MSG99)
         CLOSE OUTDCB                  CLOSE
         TM    SW02,$OPEN              DCB OPENED ?             @911218
         BNO   RET2                    NO: BYPASS FREEPOOL      @911218
         FREEPOOL OUTDCB
         NI    SW02,255-$OPEN          OUTDCB CLOSED NOW        @911218
RET2     DS    0H                                               @911218
         MVC   OUTDCB(DCBLEN),COPYDCB  REFRESH  DCB
         CLOSE PDSDCB                        ALL
         CLOSE PRTDCB                            POSSIBLE DCBS
         NI    PARMSW,255-$RENDS       Reset rename flag        @930729
         TM    SW02,$SUBMODE           SUBCOMMAND MODE ?
         BO    CHANGE                  YES: BRANCH TO CHANGE ROUTINE
         B     TSTVSST                 ELSE ASK FOR COMMAND
REG2     DC    F'0'
RECOVER  DS    5F
SAVECC   DS    CL4
         TITLE 'A L L O C A T I O N     CONTROL BLOCKS'
         SPACE 2
         DS    0F
BLKPTR   DC    AL1(128),AL3(REQBLK)
*
REQBLK   DC    AL1(20)              LENGTH OF REQBLK
VERB     DC    X'01'                DSNAME ALLOC
FLAGS1   DC    X'2000'              FLGS
ERROR    DC    H'0'                 ERRORCODE
INFO     DC    H'0'                 INFOCODE
         DC    A(SYSOUT)            SYSOUT ALLOC
RESERVED DC    F'0'
FLAGS2   DC    F'0'                 FLGS
*
SYSOUT   DS    0F                   BOUNDARY
         DC    A(SYSKEY)            SYSOUT-SPEC
         DC    A(UNALLKEY)          FREE = CLOSE
SYSINT   DC    A(INTKEY)            INTERNAL READER
         DC    AL1(128),AL3(DDNAMKEY)
*
         DS    0H
SYSKEY   DC    X'0018'              SYSOUT SPECIF.
SYSNBR   DC    AL2(1)
SYSLEN   DC    AL2(1)               LENGTH
SYSCLS   DC    C'C'                 SYSOUT-CLASS
*
CLSTAB   DC    256X'FF'                TRANSLATE TABLE FOR SYSOUTCLS
         ORG   CLSTAB+C'A'
         DC    9X'00'
         ORG   CLSTAB+C'J'
         DC    9X'00'
         ORG   CLSTAB+C'S'
         DC    8X'00'
         ORG   CLSTAB+C'0'
         DC    10X'00'
         ORG
*
         DS    0H
UNALLKEY DC    X'001C'              UNALLOCATE AT CLOSE
UNALLNBR DC    AL2(0)
UNALLLEN DS    0H
UNALLPRM DS    0H
*
INTKEY   DC    X'0019'
INTNBR   DC    AL2(1)
INTLEN   DC    AL2(6)
INTPARM  DC    C'INTRDR'
*
         DS    0H
DDNAMKEY DC    X'0055'             DDNAME TU
         DC    AL2(1)
DDNAMLEN DC    AL2(8)              LENGTH
ALCDDNAM DC    CL8' '              DDNAME FROM SVC 99
*
LINECNT  DC    PL2'0'                  FOR PRINT ROUTINE
PAGECNT  DC    PL2'0'                  FOR PRINT ROUTINE
         DC    C' '
LINE     DS    CL121
HEADLINE DC    CL53'1(C) TSTVS SKYBIRD SYSTEMS: PRINT OF MODIFIED MEMBEX
               R '
HEADMEM  DS    CL8
         DC    CL40' '
         DC    C'PAGE '
HEADCNT  DS    CL3
         DC    CL12' '
         TITLE 'DATA CONTROL BLOCKS'
         PRINT  NOGEN
OUTDCB   DCB   LRECL=80,MACRF=(GL,PM),DDNAME=OUTP01,EXLST=EXLST,       X
               DSORG=PS,RECFM=FB,EODAD=LOAD9
DCBLEN   EQU   *-OUTDCB
COPYDCB  DCB   LRECL=80,MACRF=(GL,PM),DDNAME=OUTP01,EXLST=EXLST,       X
               DSORG=PS,RECFM=FB,EODAD=LOAD9
PDSDCB   DCB   DDNAME=INP01,MACRF=R,DSORG=PO,RECFM=F,BLKSIZE=256
INTRDR   DCB   DDNAME=INTRD,MACRF=PM,LRECL=80,RECFM=F,                 X
               BLKSIZE=80,DSORG=PS
PRTDCB   DCB   DDNAME=INTRD,MACRF=PM,LRECL=121,RECFM=FBA,              X
               BLKSIZE=1210,DSORG=PS
         PRINT GEN
         TITLE 'GENERAL EQUATES'
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         TITLE 'VARIABLES, CONSTANTS AND CONTROL BLOCKS'
ECB1     DC    F'0'
MSGID    DC    F'0'                    MSGID of start msg       @930729
CARD     DS    0CL80                   OUTPUT-LINE
REPLY    DC    CL72' '                 REPLY FOR JCLSTMNT
REPLY2   DC    CL8'0'                  REPLY FOR CARDNUMBER
BLANKS   DC    80CL1' '
SAVEGETM DC    F'0'                    START ADDRESS WORKSPACE
SAVELST  DC    F'0'                    SAVE FIELD FOR BLOCK UPDIPL
DWB      DC    D'0'
REPLY4   DC    CL60' '                 REPLY FOR LINE-NUMBER
REPLY6   DC    CL3'   '                REPLY FOR CONTINUATION
REPLY7   DS    CL8
REPLY8   DC    CL4' '                  REPLY FOR EDT025A
*
ADDRTIOT DS    A                       FOR PASSWORD CONTROL
*
SAVELIN  DC    18F'0'                  SAVEAREA FOR LINENUM
OFFSRPL4 DS    F
OFFSET   DS    F                       OFFSET IN REPLY4 FOR 'C' SUBC.
LINENR1  DS    F
LINENR2  DS    F
LINERET  DS    F                       RETURNCODE FOR LINENUM ROUT
*
BLDLLIST DS    0H                      LIST ENTRY FOR BLDL MACRO
N1       DC    AL2(1)                  NUMBER OF ENTRIES
L1       DC    AL2(14)                 LENGTH OF DIR. ENTRY
BLDLM    DS    CL8
         DS    CL6
*
PARM     EQU   *
PARMSW   DC    X'00'
ALLC     EQU   X'80'                   SET BY TSTVSALL
NBRS     EQU   X'20'                   SET BY TSTVSCOM
UPDT     EQU   X'10'                   SET BY TSTVSCOM
$UNALL   EQU   X'08'                   MARK UNALLOC ONLY BY TSTVSALL
$RENAME  EQU   X'04'                   IF ON: RENAME MEMBER     @TSTV02
$RENDS   EQU   X'02'                   IF ON: RENAME A dataset  @930729
PARMROUT DC    X'6000'                 DEFAULT ROUTCDE=(2,3)
*
PARM33   DS    0CL25                   Commarea passed by 'com' @930726
PARM33M  DS    CL8                     MEMBERNAME OF TSTVSCOM
PARM33R  DS    CL8                     Newname from tstvscom    @930726
PARM33KY DS    CL8                     Key 'PDSLIST,key'        @930726
PARM33KL DS    C                       Actual key length        @930726
*
RETRYCNT DC    XL1'00'                 NUMBER OF ABEND RETRIES
SW01     DC    XL1'00'
$EOJ     EQU   128          1 = EOJ SUBCOMMAND
$C#CMD   EQU   64       INDICATE 'C' SUBCOMMAND FOR LINENUM ROUTINE
$WKSUBM  EQU   32                      INDICATE WKAREA SUBMIT
$CHANGED EQU   16           1 IS MODIFCATIONS MADE AND NOT YET SAVED
$VFY     EQU   8            1 = V ON,   0 = V OFF
$LINEDEL EQU   4            IF $LINEDEL=1 LINE DELETION REQUESTED
SW2      EQU   2            IF SW2=1 MEMBERNAME PRESENT
$XLST    EQU   1            IF ON DO AN EXTENDED LIST ELSE A NORMAL
SW02     DC    XL1'00'
$SUBMODE EQU   128          IF ON: SUBCOMMAND MODE ENTERED
$OPER    EQU   64           IF ON: DO NOT ALLOW SUBMIT AND PRINT CMD
*                                  BECAUSE TSTVS IS ATTACHED BY A
*                                  PRIMARY SUBSYSTEM
$OPEN    EQU   32           IF ON: OUTDCB OPENED               @911218
$CHALL   EQU   16           IF ON: BUSY WITH 'C ALL' SUBCMD    @911218
$NOMATCH EQU   8            IF ON: NOT ONLY 1 MATCH IN 'C ALL' @911218
PACKFLD  DC    PL3'0'
WTOCNT   DC    PL2'0'
SAVER    DC    15F'0'
MEMBAREA DC    256X'00'
         DS    0F
EXLST    DC    X'87',AL3(JFCB)
JFCB     DS    CL176
HELP1    DS    CL80
HELP2    DS    CL80
CMDL     DS    H                       Length of command including pfx
         DC    H'0'                    Reserved
PARM10   DC    C'START RDRTST,D='''
PARMRST  DC    CL60' '
         DS    0H
         TITLE 'M E S S A G E    B L O C K'
MSG02    DC    CL60'EDT006E Invalid line-number, reply again'
MSG03    DC    C'EDT007E Parameter error, defaults taken (ROUTCDE=2,3) X
                     '
MSG04    DC    CL60'EDT008E seq. number not numeric, reply again'
MSG05    DC    C'EDT010E Member-name to be updated not present. ReenterX
                     '
MSG06    DC    CL15'EDT011I Member '
WTOMEM   DC    CL8' '
         DC    CL1' '
NOT      DC    CL3'   '
RENDEL   DC    CL8'        '
RTRNTEXT DC    CL25' '
MSG07    DC    C'EDT012E Maximum lines in member, insertion impossible.X
                     '
MSG08    DC    CL60'EDT013I Dataset has been re-numbered'
MSG09    DC    C'EDT014I SUBCOMMANDS                                   X
                     '
MSG10    DC    C'        XXX;       replace the line, EDT003A follows. X
                     '
MSG11    DC    C'        I XXX;     insert a line , EDT003A follows.   X
                     '
MSG11A   DC    C'        D XXX OR D XXX,YYY delete a line or a range ofX
                Lines'
MSG11B   DC    C'        R XXX      repeat the specified line after curX
               rent  '
MSG11C   DC    C'        L(IST),L(IST) x or L(IST) x,y List entire or lX
               inenrs'
MSG11D   DC    C'        XLST       same as list, but 80 positions dispX
               layed '
MSG11E   DC    C'        RENUM;     renumber cols 73-80 of the member  X
                     '
MSG11F   DC    C'        UNNUM;     remove numbers of cols 73-80       X
                     '
MSG11G   DC    C'        V ON/OFF;  list or not list line after "C" comX
               mand  '
MSG11H   DC    C'        ADD;       add lines at the end of the member X
                     '
MSG11I   DC    C'        SUBMIT;    submit the contents of the workareaX
                     '
MSG11J   DC    C'        C XXX,string1$string2$ modify character stringX
               s     '
MSG11K   DC    C'        F $string$ find string or "F" repeat find of sX
               tring '
MSG11L   DC    C'        PRINT x;   print spinoff sysout of workarea   X
                     '
MSG11M   DC    C'        SAVE;      save workarea in the member        X
                     '
MSG11N   DC    C'        END;       end of subcommands and ask for nextX
                cmd. '
MSG11O   DC    C'        EOJ;       terminate TSTVS from subcommand modX
               e.    '
MSG12    DC    CL60'EDT015E string(s) not found'
MSG13    DC    CL8'EDT016I '
MSGNAME  DS    CL52
         DS    0F
MSG14    DC    C'EDT018I function terminated by operator               X
                     '
MSG15    DC    CL30'EDT019E Dyn. ALLOC. errorcode '
ERRORBC  DS    CL4
         DC    CL10' Infocode '
INFOBC   DS    CL4
         DC    12C' '
MSG17    DC    CL12'EDT021I job '
JOBNAME  DS    CL8
         DC    CL10' submitted'
         DC    30C' '
MSG18    DC    C'EDT022I  verification turned '   29
ONOFF    DS    CL3                                 3
         DC    CL28' '
MSG19    DC    C'EDT023I '             MSG AFTER PRINT COMMAND
MSG19#L  DS    CL4                     # OF LINES PRINTED
         DC    C' lines printed in SYSOUT='
MSG19CLS DS    C
         DC    CL22' '
MSG20    DC    CL15'EDT024I member '   MEMBER SUCCESSFULLY SAVED
MSG20M   DC    CL8' '                  SAVED MEMBER
         DC    CL19' successfully saved'
         DC    CL18' '
MSG21    DC    CL60'EDT025E invalid character string'
MSG22    DC    CL60'EDT026E invalid repeat find, no string found'
MSG23    DC    CL60'EDT027D ABENDXXX detected, retry successful' 13
MSG24    DC    CL60'EDT028E allocation subsystem dataset not allowed'
MSG99    WTO   '                                                       X
                    ',ROUTCDE=(2,3),MF=L
ZZZ      EQU   *
*
         TITLE 'L I T E R A L        P O O L'
         LTORG
         TITLE 'DSECTS'
         PRINT NOGEN
         DCBD  DSORG=PS,DEVD=DA
         IHASDWA
         END
@@
//TSTVSALL  EXEC ASM,MEMBER=TSTVSALL,ASMPARM=',SYSPARM(MVS)'
//ASM.SYSIN DD DATA,DLM=@@
TSTVSALL TITLE 'DYNAMIC ALLOCATION ROUTINE FOR TSTVS'
*---------------------------------------------------------------------*
*                                                                     *
*        Module:        TSTVSALL.                                     *
*        Author:        Rob Prins, ING-Bank Amsterdam                 *
*                                                                     *
*        Function:      Allocate the library used in TSTVS            *
*                       under ddnames INP01 and OUTP01.               *
*                       if flag "$RENDS' is on: ask for an old DSNAME *
*                       and a new DSNAME and rename the specified     *
*                       dataset                                       *
*                                                                     *
*                                                                     *
*        PARAMETER:     1 BYTE ALLOCATION SWITCH:                     *
*                       AND 2 BYTES ROUTING CODE PASSED BY            *
*                       THE MAIN ROUTINE TSTVS.                       *
*                                                                     *
*                                                                     *
*        Returncodes:   0000 - OK                                     *
*                       0004 - Function canceled                      *
*                       0008 - RENAME errors occurred                 *
*                                                                     *
*        Modifications: July 29,1993 By Rpr: Support added for the    *
*                       RENAME command. Both catalogued and not       *
*                       catalogued datasets can be renamed.           *
*                                                                     *
*                       April 06,1995 By Rpr: DYNAMIC=YES added       *
*                       to UCBLOOK macro                        @950406
*                       Oct 27,1997   By Rpr: RANGE=ALL   added       *
*                       to UCBLOOK macro                        @971027
*                       August 16,2002 By Rpr: Include the MVS38J     *
*                       version of UCBLOOK (running throught the UCB  *
*                       lookup table). Use SYSPARM(MVS) to include    *
*                       This MVS function.                      @020814
*                                                                     *
*-------------------------------------- (C)-2002 SKYBIRD SYSTEMS -----*
         SPACE 2
TSTVSALL CSECT
         SAVE  (14,12),,*              SAVE REGISTERS.
         LR    R10,R15                 BASE REGISTER
         USING TSTVSALL,R10            GET ADDRESSABILITY
         LR    R2,R1
         LA    R1,SAVEAREA
         ST    R1,8(,R13)              FORWARD POINTER
         ST    R13,4(,R1)              BACKWARD POINTER
         LR    R13,R1
         L     R11,0(,R2)              OBTAIN PARM FROM MAIN ROUT
         USING PARMAREA,R11            GET ADDRESSABILITY OF THE PARM
         MVI   SW,0                    Reset switches           @930729
         XR    R7,R7
         ICM   R7,3,ROUTCDE            OBTAIN ROUTING CODE MESSAGES
         STH   R7,DSNWAIT-4            STORE IN WTORS
         STH   R7,VOLWAIT-4
         STH   R7,NEWNWAIT-4                                    @930729
         STH   R7,MSG99E-2
         TM    ALLOCSW,$UNALL          UNALLOCATE ONLY ?
         BNZ   ALLOC00                 YES: DO NOT ASK NEW DSNAME
ASKDSN   EQU   *
         NI    SW,255-$NCTLGD          Reset flag               @930729
         XC    ECB,ECB                 CLEAR ECB AREA
         MVC   DALDSN(44),BLANKS       CLEAR REPLY AREA
         WTOR  'EDT100A Enter dataset name or ''CANCEL''',             *
               DALDSN,44,ECB,ROUTCDE=(2,3)
DSNWAIT  WAIT  ECB=ECB                 WAIT FOR EVENT
         CLI   DALDSN,C' '             NOTHING REPLIED ?
         BE    ASKDSN                  ASK AGAIN
         CLC   =C'CANCEL',DALDSN       Function canceled ?      @930729
         BNE   ASKVOL                  No: continue             @930729
         MVC   MSG99+4(60),MSG05       move msg EDT198I         @930729
         WTO   MF=(E,MSG99)            Function canceled        @930729
         LA    R15,4                   Load return code         @930729
         B     RETURN                  and return               @930729
ASKVOL   EQU   *                       ASK VOLUME
         XC    ECB,ECB                 CLEAR EVENT CONTROL BLOCK
         MVC   DAVOLSER,BLANKS         CLEAR REPLY AREA
         WTOR  'EDT101A Enter volume serial or ''U''',DAVOLSER,6,ECB,  X
               ROUTCDE=(2,3)
VOLWAIT  WAIT  ECB=ECB                 WAIT FOR EVENT
         CLI   DAVOLSER,C' '           NOTHING REPLIED ?
         BE    ASKVOL                  ASK AGAIN.
         CLC   DAVOLSER(2),=C'U '      U MEANS LOOK IN CATALOG
         BE    DSNLOC                  BRANCH IF yes            @930729
         OI    SW,$NCTLGD              Mark dataset not catlgd  @930729
         B     FUNCTION                and branch               @930729
DSNLOC   DS    0H                                               @930729
         LOCATE LOC                    LOOK IN CATALOG
         LTR   R15,R15                 CHECK COMPLETION.
         BZ    VOL1                    Branch if alright
         MVC   MSG99+4(60),MSG04       DATASET NOT IN CATALOG
         WTO   MF=(E,MSG99)            INFORM OPER/PGMR
         B     ASKDSN                  ASK FOR NEW ALLOCATION
VOL1     DS    0H                      LOCATE IS OK
         MVC   DAVOLSER,LOCVOL         MOVE VOLUME SERIAL
FUNCTION DS    0H                      Check if func = ALLOCATE @930729
*                                      or RENAME                @930729
         MVC   CURDSN,DALDSN           Save dataset name        @930729
         TM    ALLOCSW,$RENDS          Rename of a dataset ?    @930729
         BNO   ALLOC00                 NO; Allocate.            @930729
         OBTAIN OBTNLIST               Check if DS is on volume @930729
         LTR   R15,R15                 TEST COMPLETION          @930729
         BZ    ASKNEWN                 OK: branch               @930729
         MVC   MSG99+4(60),MSG03       move msg EDT104E         @930729
         WTO   MF=(E,MSG99)            DS COULD NOT BE OBTAINED @930729
         B     ASKDSN                  Retry ...                @930729
ASKNEWN  DS    0H                      Ask for NEWNAME          @930729
         XC    ECB,ECB                 CLEAR ECB AREA           @930729
         MVC   NEWNAME(44),BLANKS      Clear reply area         @930729
         WTOR  'EDT106A Enter NEW dataset name or ''CANCEL''',         X
               NEWNAME,44,ECB,ROUTCDE=(2,3)                     @930729
NEWNWAIT WAIT  ECB=ECB                 WAIT FOR EVENT           @930729
         CLI   NEWNAME,C' '            NOTHING REPLIED ?        @930729
         BE    ASKNEWN                 ASK AGAIN                @930729
         CLC   =C'CANCEL',NEWNAME      Function canceled ?      @930729
         BNE   NEWNC001                No: continue             @930729
         MVC   MSG99+4(60),MSG05       move msg EDT198I         @930729
         WTO   MF=(E,MSG99)            Function canceled        @930729
         LA    R15,4                   Load return code         @930729
         B     RETURN                  and return               @930729
NEWNC001 DS    0H                                               @930729
         TM    SW,$NCTLGD              Dataset catalogued ?     @930729
         BO    NEWNC005                No: bypass LOCATE        @930729
         LA    R3,VCB                  Volume list for RENAME   @930729
         LA    R4,LOC#DEVT             Volume list from LOCATE  @930729
         LH    R5,LOC#VOLS             # volumes                @930729
         MVC   VOLCNT,LOC#VOLS         Copy # volumes           @930729
NEWNC003 DS    0H                                               @930729
         MVC   0(10,R3),0(R4)          Copy DEVT + volume       @930729
         XC    10(2,R3),10(R3)         Clear rename status code @930729
         LA    R3,12(,R3)              next entry output        @930729
         LA    R4,12(,R4)              next entry input         @930729
         BCT   R5,NEWNC003             loop until all done      @930729
         MVC   DALDSN,NEWNAME          move to CAMLST-NAME      @930729
         LOCATE LOC                    LOOK IN CATALOG          @930729
         LTR   R15,R15                 DS found in catalog ?    @930729
         BNZ   NEWNC005                No: Ok                   @930729
         MVC   MSG99+4(60),MSG06       move msg EDT107E         @930729
         WTO   MF=(E,MSG99)            Newname already catlgd   @930729
         B     ASKNEWN                 and retry ...            @930729
NEWNC005 DS    0H                                               @930729
         MVC   DALDSN,NEWNAME          move to CAMLST-SEARCH    @930729
         OBTAIN OBTNLIST               Check if DS is on volume @930729
         LTR   R15,R15                 TEST COMPLETION          @930729
         BNZ   NEWNC007                OK: already on volume    @930729
         MVC   MSG99+4(60),MSG07       move msg EDT108E         @930729
         WTO   MF=(E,MSG99)            Newname already on volume@930729
         B     ASKNEWN                 and retry ...            @941019
NEWNC007 DS    0H                                               @930729
         BAL   R14,RENAME              Rename the dataset       @930729
         B     RETURN
ALLOC00  EQU   *                       ALLOCATE
         TM    ALLOCSW,ALLOC           ALREADY ALLOCATED PRIOR ?
         BZ    ALLOC01                 GO ON IF NOT
         BAL   R14,UNALLOC             UNALLOCATE FIRST
         LTR   R15,R15                 UNALLOC OK ?
         BNZ   ASKDSN                  TRY AGAIN IF NOT
ALLOC01  EQU   *                       ALLOCATE THE DATASET
         TM    ALLOCSW,$UNALL          UNALLOCATE ONLY ?
         BNZ   ALLOC99                 YES: BYPASS IT
         BAL   R14,ALLOCPRC            ALLOCATE
         LTR   R15,R15                 TEST COMPLETION.
         BNZ   ASKDSN                  TRY AGAIN IF NOT
         MVC   MSG99+4(60),MSG01       GIVE SUCCESSFUL ALLOC MSG
         WTO   MF=(E,MSG99)            INFORM OPER/PROGRAMMER
ALLOC99  DS    0H                                               @930729
         XR    R15,R15                 RC = 0000                @930729
RETURN   L     R13,4(,R13)
         RETURN (14,12),RC=(15)        GO TO MAIN MODULE
         TITLE 'DYNAMIC ALLOCATION ROUTINE'
*--------------------------------------------------------------------*
*                                                                    *
*        THIS ROUTINE DYNAMICALLY ALLOCATES DATASETS THROUGH         *
*        THE DYNALLOC-MACRO <SVC 99>. TO DO THIS IT NEEDS THE DSNAME *
*        AND THE VOLUME-ID. THE DATASET WILL BE ALLOCATED WITH THE   *
*        DDNAME AS SPECIFIED IN 'DALDDN'. A RETURNCODE WILL BE       *
*        AVAILABLE IN REG. 15 AND IN AN ERROR-SITUATION A SUITABLE   *
*        MESSAGE WILL BE PRODUCED (REFER TO MVS JOB-MANAGEMENT).     *
*                                                                    *
*------------------------------------- (C)-1995 Skybird Systems -----*
         SPACE 2
ALLOCPRC DS    0H
         LA    R2,0
         ST    R14,ALLOCSVE            SAVE RETURNREGISTER
ALLOC001 EQU   *
         MVC   DALDDN(8),=C'INP01   '
         LA    R1,DALPTR               REQUEST BLOCK
         SVC   99                      ALLOCATE <SVC99>
         LTR   R15,R15                 TEST RETURNCODE
         BZ    ALLOC999                OK IF ZERO
ALLOC003 EQU   *
         CVD   R15,DWB
         UNPK  R15BC(4),DWB+4(4)       RETURNCODE
         OI    R15BC+3,X'F0'
         SYCONVHX IN=DAERROR,OUT=ERRORBC,L=2
         SYCONVHX IN=DAINFO,OUT=INFOBC,L=2
         MVC   ALLVERB,=C'ALLOCATION'  PREPARE MSG
         MVC   MSG99+4(60),MSG02       ALLOCATION ERR MSG
         WTO   MF=(E,MSG99)            INFORM OPER/PROGRAMMER
         LA    R2,12                   RETURNCODE
         B     ALLOCERR
ALLOC999 EQU   *
         MVC   DALDDN(8),=C'OUTP01  '  FILL IN DDNAME
         LA    R1,DALPTR               REQUEST BLOCK
         SVC   99                      ALLOCATE
         OBTAIN OBTNLIST
         LTR   R15,R15                 TEST COMPLETION
         BZ    ALLOCOK
         MVC   MSG99+4(60),MSG03
         WTO   MF=(E,MSG99)            DS COULD NOT BE OBTAINED
         LA    R2,12                   TRY AGAIN
ALLOCOK  EQU   *
         OI    ALLOCSW,ALLOC           MASK ALLOCATED
ALLOCERR L     R14,ALLOCSVE            LOAD RETURN ADDRESS
         LR    R15,R2                  RETURNCODE
         BR    R14                     RETURN
         TITLE 'DYNAMIC UNALLOCATION ROUTINE'
*--------------------------------------------------------------------*
*                                                                    *
*        THIS ROUTINE DYNAMICALLY UNALLOCATES A DATASET WITH THE     *
*        DDNAME AS SPECIFIED IN 'DALDDNAM'. THIS FUNCTION WILL ALSO  *
*        BE PERFORMED THROUGH THE DYNALLOC-MACRO AND A RETURNCODE    *
*        WILL BE AVAILABLE IN REG. 15.                               *
*                                                                    *
*------------------------------------- (C)-1995 Skybird Systems -----*
         SPACE 2
UNALLOC  EQU   *
         ST    R14,UNALLSVE            SAVE RETURNADDRESS
         LA    R2,0                    INIT RETCD
UNALL001 EQU   *
         MVC   DALDDN(8),=C'INP01   '
         LA    R1,DUNPTR               REQUEST BLOCK
         SVC   99                      UNALLOCATE
         LTR   R15,R15                 OK ?
         BZ    UNALL999                BRANCH IF SO
UNALL003 EQU   *
         CVD   R15,DWB
         UNPK  R15BC(4),DWB+4(4)       RETURNCODE
         OI    R15BC+3,X'F0'
         SYCONVHX IN=DUNERROR,OUT=ERRORBC,L=2
         SYCONVHX IN=DUNINFO,OUT=INFOBC,L=2
         MVC   ALLVERB(10),=C'UNALLOC.  '
         MVC   MSG99+4(60),MSG02
         WTO   MF=(E,MSG02)            GIVE ERROR MSG
         LA    R2,4                   RETURNCODE
         B     UNALLERR
UNALL999 EQU   *
         MVC   DALDDN(8),=C'OUTP01  '  FILL IN DDNAME
         LA    R1,DUNPTR               LOAD REQUEST BLOCK
         SVC   99                      ISSUE DYNALLOC SVC
         NI    ALLOCSW,X'FF'-ALLOC     MASK UNALLOCATED
UNALLERR L     R14,UNALLSVE            RESTORE R14
         LR    R15,R2                  RETURNCODE
         BR    R14                     RETURN
         TITLE 'Rename dataset Routine'
*---------------------------------------------------------------------*
*                                                                     *
*        Routine:    RENAME                                           *
*        Function:   Rename datasets:                                 *
*                    If the dataset is catalogued, the dataset will   *
*                    be renamed, uncatalogued and the newname will    *
*                    be catalogued (msg EDT101A replied with 'U')     *
*                    If the dataset is not catalogued (msg EDT101A    *
*                    has been replied with a volume serial number),   *
*                    only the dataset in the VTOC will be renamed.    *
*                                                                     *
*------------------------------------- (C)-1995 Skybird Systems ------*
         SPACE
RENAME   DS    0H
         USING TSTVSUCB,R4             GET ADDRESSABILITY OF UCB
         ST    R14,SAVE14R             Save reg.14              @930729
         TM    SW,$NCTLGD              Dataset catalogued ?     @930729
         BNO   REN002                  Yes: rename directly     @930729
         AIF   ('&SYSPARM' EQ 'MVS').SKIP1                      @020814
*---------------------------------------------------------------------*
*        Do the lookup of the UCB in the OS/390 way
*---------------------------------------------------------------------*
         MODESET KEY=ZERO              Goto system key          @930729
         UCBLOOK VOLSER=DAVOLSER,DEVCLASS=DASD,UCBPTR=ADDRUCB,         X
               NOPIN,DYNAMIC=YES,RANGE=ALL                      @971027
         LTR   R15,R15                 UCBLOOK ok?              @930729
         BZ    REN001                  YES: branch              @930729
         MODESET KEY=NZERO             Goback to user key       @930729
.SKIP1   ANOP
         AIF   ('&SYSPARM' NE 'MVS').SKIP2                      @020814
*---------------------------------------------------------------------*
*        Do the lookup of the UCB in the MVS way
*---------------------------------------------------------------------*
         L     R3,CVTPTR               GET CVTADRESS            @020814
         USING CVT,R3                  GET ADDRESSABILITY OF CVT@020814
         L     R2,CVTILK2              POINT TO START of lookup @020814
LOOKUP   EQU   *                       FIND UCB                 @020814
         CLC   0(2,R2),=X'FFFF'        END OF LOOKUP TABLE ?    @020814
         BE    LOOKERR                 YES: END OF UCBS REACHED @020814
         CLC   0(2,R2),=X'0000'        NULL ENTRY?              @020814
         BE    LOOKNXT                 GO TO NEXT ONE           @020814
         XR    R4,R4                   CLEAR REGISTER           @020814
         ICM   R4,3,0(R2)              # HALFWORDS IN R4        @020814
*        SLL   R4,1                 *2 BECAUSE LOOKUP=HALFWORDS @020814
         CLC   SRTEVOLI,DAVOLSER     IS THIS THE VOLSER WE NEED?@020814
         BE    LOOKFND                 BRANCH IF YES.           @020814
LOOKNXT  DS    0H                                               @020814
         LA    R2,2(,R2)              Next entry in lookup table@020814
         B     LOOKUP                  Search for next UCB      @020814
LOOKFND  DS    0H                                               @020814
         ST    R4,ADDRUCB              Save UCB address         @020814
         B     REN001                                           @031006
LOOKERR  DS    0H                                               @020814
         LA    R15,4                   RC=0004                  @020814
         DROP  R3                      kill addr of CVT         @020814
.SKIP2   ANOP
         CVD   R15,DWB                 Set                      @930729
         UNPK  MSG11+26(4),DWB            returncode            @930729
         OI    MSG11+29,X'F0'                in msg             @930729
         MVC   MSG11+35(6),DAVOLSER    Set VOLSER in msg        @930729
         MVC   MSG99+4(60),MSG11       move msg EDT111E         @930729
         WTO   MF=(E,MSG99)            UCBLOOK failed           @930729
         B     REN970                  and Branch               @930729
REN001   DS    0H                                               @930729
         AIF   ('&SYSPARM' EQ 'MVS').SKIP3                      @020814
         MODESET KEY=NZERO             Goback to user key       @930729
.SKIP3   ANOP
         MVC   VOLCNT,=H'1'            Volume count is 1        @930729
         MVC   VOLSER,DAVOLSER         Move volume name         @930729
         L     R4,ADDRUCB              Pickup UCB pointer       @930729
         MVC   VOLDEVT,UCBTYP          Move device type         @930729
         DROP  R4                      Kill local addr.         @930729
REN002   DS    0H                                               @930729
         SR    R0,R0                   Clear reg.0              @930729
         RENAME RENAMELS               Rename the dataset       @930729
         LTR   R15,R15                 Rename OK ?              @930729
         BZ    REN003                  Yes: continue            @930729
         CVD   R15,DWB                 Set                      @930729
         UNPK  MSG08+26(4),DWB            Returncode            @930729
         OI    MSG08+29,X'F0'                in message         @930729
         SYCONVHX IN=VOLRNCDE+1,OUT=MSG08+41,L=1 Status code    @930729
         MVC   MSG99+4(60),MSG08       move msg EDT109E         @930729
         WTO   MF=(E,MSG99)            Rename failed            @930729
         B     REN970                  and Branch               @930729
REN003   DS    0H                                               @930729
         TM    SW,$NCTLGD              Data set not catalogued  @930729
         BO    REN900                  YES: finished give msg   @930729
         CATALOG UNCATLG               Uncatalog the dataset    @930729
         LTR   R15,R15                 OK ???                   @930729
         BZ    REN005                  Yeah: branch             @930729
         MVC   MSG09+8(2),=C'UN'       UNcatalog failed         @930729
         CVD   R15,DWB                 Set                      @930729
         UNPK  MSG09+29(4),DWB            Returncode            @930729
         OI    MSG09+32,X'F0'                in message         @930729
         MVC   MSG99+4(60),MSG09       move msg EDT110E         @930729
         WTO   MF=(E,MSG99)            UNCATLG failed           @930729
REN005   DS    0H                                               @930729
         LA    R4,VCB                  Point to volume list     @930729
         LH    R5,VOLCNT               # volumes                @930729
REN007   DS    0H                                               @930729
         XC    10(2,R4),10(R4)         Clear status/dsseqnr     @930729
         LA    R4,12(,R4)              Next entry               @930729
         BCT   R5,REN007               Loop until all done      @930729
         CATALOG CATLG                 Catalog the new dataset  @930729
         LTR   R15,R15                 OK ???                   @930729
         BZ    REN900                  Yeah: branch             @930729
         MVC   MSG09+8(2),=C'  '       CAtalog failed           @930729
         CVD   R15,DWB                 Set                      @930729
         UNPK  MSG09+29(4),DWB            Returncode            @930729
         OI    MSG09+32,X'F0'                in message         @930729
         MVC   MSG99+4(60),MSG09       move msg EDT110E         @930729
         WTO   MF=(E,MSG99)            UNCATLG failed           @930729
         B     REN970                  Give RC=0012             @930729
REN900   DS    0H                                               @930729
         MVC   MSG99+4(60),MSG10       move msg EDT199I         @930729
         WTO   MF=(E,MSG99)            RENAME successful        @930729
         XR    R15,R15                 RC=0000                  @930729
         B     REN999                  return                   @930729
REN970   DS    0H                      errors occurred          @930729
         LA    R15,8                   Load RC,                 @930729
REN999   DS    0H                                               @930729
         L     R14,SAVE14R             Restore reg.14           @930729
         BR    R14                     Return to caller         @930729
         TITLE 'Variables and constants'
DWB      DS    D                       For CVD/CVB instructions @930729
ADDRUCB  DS    F                       A(UCB) if ds not catlgd  @930729
ECB      DS    F
SAVEAREA DC    18F'0'
MSG01    DC    CL60'EDT103I Allocation of dataset successful'
MSG02    DS    0CL60
         DC    CL8'EDT102E '           MSGID 8
ALLVERB  DS    CL10
         DC    CL11' Errorcode='       11
ERRORBC  DS    CL4
         DC    CL10',INFOCODE='        10
INFOBC   DS    CL4
         DC    CL5',R15='              5
R15BC    DS    CL4
         DC    CL4' '                  4
MSG03    DC    CL60'EDT104E Dataset not on volume'
MSG04    DC    CL60'EDT105E Dataset not in catalog'
MSG05    DC    CL60'EDT198I Function canceled by operator'      @930729
MSG06    DC    CL60'EDT107E Newname already catalogued'         @930729
MSG07    DC    CL60'EDT108E Newname already on volume'          @930729
MSG08    DC    CL60'EDT109E RENAME failed, RC=xxxx, Statuscd=xx' @93729
MSG09    DC    CL60'EDT110E xxCATALOG failed, RC=xxxx'          @930729
MSG10    DC    CL60'EDT199I RENAME of dataset successful'       @930729
MSG11    DC    CL60'EDT111E UCBLOOK failed RC=xxxx,VOL=vvvvvv'  @930729
BLANKS   DC    CL60' '
MSG99    WTO   '1234512345123451234512345123451234512345123451234512345X
               12345',ROUTCDE=(2,3),MF=L
MSG99E   EQU   *
*
LOC      CAMLST NAME,DALDSN,,LOCAREA   FOR LOCATE
OBTNLIST CAMLST SEARCH,DALDSN,DAVOLSER,WORKAREA
RENAMELS CAMLST RENAME,CURDSN,NEWNAME,VOLLIST                   @930729
CATLG    CAMLST CATBX,NEWNAME,,VOLLIST                          @930729
UNCATLG  CAMLST UCATDX,CURDSN                                   @930729
*
LOCAREA  DS    0D
LOC#VOLS DS    CL2                     # volume on which ds resides
LOC#DEVT DS    CL4                     devtype                  @930729
LOCVOL   DS    CL6                     FOUND VOLUME
LOCDSEQ  DS    CL2                     Dataset seq. number      @930729
         DS    CL251                   REST OF LOCAREA
*
         DS    0D
WORKAREA DS    CL140
CURDSN   DS    CL44                    Old dataset if RENAME    @930729
NEWNAME  DS    CL44                    New dataset if RENAME    @930729
*
         DS    0H                      volume list on hw-bound. @930729
VOLLIST  DS    0CL256                  Volume list for RENAME   @930729
VOLCNT   DS    CL2                     # volumes                @930729
VCB      DS    0CL12                   1 volume entry           @930729
VOLDEVT  DS    CL4                     Device-type              @930729
VOLSER   DS    CL6                     Volume serial number     @930729
VOLRNCDE DS    CL2                     Rename code              @930729
         DS    CL242                   Rest of volume list      @930729
*                                                               @930729
SW       DC    X'00'                                            @930729
$NCTLGD  EQU   128                                              @930729
         LTORG ,
         TITLE 'ALLOCATION CONTROL-BOCKS'
*--------------------------------------------------------------------*
*                                                                    *
*        THESE ARE THE CONTROL-BLOCKS USED BY THE DYNAMIC ALLOCATION *
*        ROUTINE and the DASDSM Rename routine.                      *
*                                                                    *
*------------------------------------- (C)-1995 Skybird Systems -----*
         SPACE 2
         DS    0F
DALPTR   DC    X'80',AL3(DALBLK)
*
DALBLK   DC    AL1(20)                 LENGTH OF REQ BLK
DALVERB  DC    AL1(1)                  DSNAME ALLOCATION
DALFLG1  DC    X'2000'                 FLAGS1
DAERROR  DC    AL2(0)                  ERROR CODE
DAINFO   DC    AL2(0)                  INFOCODE
DALTU    DC    A(DALTXT)               TEXT UNIT LIST
         DC    A(0)                    RESERVED
DALFLG2  DC    A(0)                    FLAGS2
*
DALTXT   DS    0F
         DC    A(DALDDNAM)             DDNAME TU
         DC    A(DALDSNAM)             DSNAME TU
         DC    A(DALSTATS)             DISPOSITION TU
         DC    A(DALVLSER)             VOLUME SERIAL TU
         DC    X'80',AL3(DALUNIT)      UNIT NAME CONTAINS SYSDA
*
DALDSNAM DS    0H                      DSNAME TU
         DC    X'0002'
         DC    AL2(1)
DALDSNL  DC    AL2(44)                 LENGTH
DALDSN   DS    CL44
*
DALSTATS DS    0H                      DATASET DISP.
         DC    X'0004'
         DC    AL2(1)
         DC    AL2(1)                  LENGTH
         DC    X'08'                   DISP=SHR
*
DALVLSER DS    0H                      VOLUME SERIAL TU
         DC    X'0010'
         DC    AL2(1)
         DC    AL2(6)                  VOLUME LENGTH
DAVOLSER DS    CL6
*
DALUNIT  DS    0H                      UNIT TU
         DC    X'0015'
         DC    AL2(1)
DALUNITL DC    AL2(8)                  LENGTH
DALUNITD DC    C'SYSALLDA'
*
DALDDNAM DS    0H                      DDNAME TU
         DC    X'0001'
         DC    AL2(1)
DALDDNL  DC    AL2(8)
DALDDN   DS    0CL8                    DDNAME
         DC    CL8'INP01'
         TITLE 'UNALLOCATION CONTROL-BLOCKS'
*--------------------------------------------------------------------*
*                                                                    *
*        THESE ARE THE CONTROL-BLOCKS USED BY THE DYNAMIC UNALLO-    *
*        CATION ROUTINE.                                             *
*                                                                    *
*------------------------------------- (C)-1995 Skybird Systems -----*
         SPACE 2
         DS    0F
DUNPTR   DC    X'80',AL3(DUNBLK)       REQUEST BLOCK POINTER
*
DUNBLK   DC    AL1(20)                 LENGTH OF REQ.BLK
DUNVERB  DC    AL1(2)                  UNALLOCATION VERB
DUNFLG1  DC    X'2000'                 FLAGS1
DUNERROR DC    AL2(0)                  ERRORCODE
DUNINFO  DC    AL2(0)                  INFOCODE
         DC    A(DUNTXT)               TEXT UNIT LIST
         DC    A(0)                    RESERVED
DUNFLG2  DC    A(0)                    FLAGS 2
*
DUNTXT   DS    0F
         DC    X'80',AL3(DALDDNAM)
*
SAVE14R  DS    F                       SAVE R14 In Rename rout. @930729
ALLOCSVE DS    F                       SAVE R14
UNALLSVE DS    F
*
*        GENERAL EQUATES.
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10                      First base register
R11      EQU   11                      Address of Parameter from TSTVS
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
         TITLE 'Dummy sections'
         PRINT NOGEN
PARMAREA DSECT                         FROM MAIN MODULE
ALLOCSW  DS   C                        ALLOCSW
ALLOC    EQU  X'80'
$UNALL   EQU  X'08'                    DON ONLY AN UNALLOCATE
$RENDS   EQU  X'02'                    Perform a rename         @930729
ROUTCDE  DS   CL2                      ROUTING CODE MESSAGES
*
TSTVSUCB DSECT ,                                                @930729
         IEFUCBOB LIST=NO
         AIF   ('&SYSPARM' NE 'MVS').SKIP4                      @020814
         CVT   DSECT=YES,LIST=NO
.SKIP4   ANOP
         END
@@
//TSTVSCOM  EXEC ASM,MEMBER=TSTVSCOM
//ASM.SYSIN DD DATA,DLM=@@
TSTVSCOM TITLE 'COMMAND PROCESSOR FOR TSTVS'
*---------------------------------------------------------------------*
*                                                                     *
*        MODULE:        TSTVSCOM.                                     *
*        AUTHOR:        Rob Prins, ING Bank Amsterdam                 *
*                                                                     *
*        FUNCTION:      PROCESS THE FOLLOWING COMMANDS OF TSTVS:      *
*                       'A MEMBER' OR 'AN MEMBER'                     *
*                       CREATE MEMBER IN THE ALLOCATED PDS.           *
*                       'U MEMBER' OR 'UN MEMBER'                     *
*                       UPDATE MEMBER IN THE ALLOCATED PDS.           *
*                       'D MEMBER' DELETE THE MEMBER.                 *
*                       'PDSLIST(,key)' LISTPDS of the PDS            *
*                       starting with 'key'                           *
*                       'EOJ' STOP PROCESSING OF TSTVS.               *
*                       'ALLOC' ALLOC THE LIBRARY.                    *
*                       'RENAME' Rename an entire dataset             *
*                       IN THIS MODULE ONLY THE COMMANDS OR SCREENED  *
*                       AND GIVEN BACK TO THE MAIN MODULE.            *
*                       LINKAGE: 1 BYTE SWITHES 2 BYTES ROUTCDE AND   *
*                       8 BYTES MEMBERNAME                            *
*                                                                     *
*        Modifications: Jul 26,1993 by Rpr: The "Key" in the          *
*                       "PDSLIST,key" has now a variable length       *
*                       Jul 29,1993 by Rpr: Add the RENAME command    *
*                       to rename a dataset                           *
*                       VS1 support deleted.                          *
*                                                                     *
*------------------------------------------ (C)-2002 SKYBIRD SYSTEMS -*
         SPACE 2
TSTVSCOM CSECT
         SAVE  (14,12),,*              SAVE REGISTERS.
         LR    R10,R15                 BASE REGISTER
         USING TSTVSCOM,R10            GET ADDRESSABILITY
         LR    R2,R1
         LA    R1,SAVEAREA
         ST    R1,8(,R13)              FORWARD POINTER
         ST    R13,4(,R1)              BACKWARD POINTER
         LR    R13,R1
         L     R11,0(,R2)              OBTAIN PARM FROM MAIN ROUT
         USING PARMAREA,R11
         XR    R7,R7
         ICM   R7,3,ROUTCDE            OBTAIN ROUTING CODE MESSAGES
         STH   R7,CMDWAIT-4            STORE IN WTORS
         STH   R7,MSG99E-2
ASKCMD   DS    0H
         LA    R4,0
         NI    SWITCH,X'FF'-UPDT-NBRS
         XC    ECB,ECB                 CLEAR ECB AREA
         MVC   REPLY(20),BLANKS        CLEAR REPLY AREA
         MVC   PARM33,BLANKS           CLEAR REPLY AREA
         WTOR  'EDT200A Enter command or ''HELP'' ',REPLY,20,ECB,      X
               ROUTCDE=(2,3)           ASK FOR COMMAND
CMDWAIT  WAIT  ECB=ECB                 WAIT FOR EVENT
         CLI   REPLY,C' '              NOTHING REPLIED ?
         BE    ASKCMD                  ASK AGAIN
         CLC   REPLY(5),=C'HELP '      TEST HELP
         BNE   CMD002                  GO ON IF NOT
         MVC   MSG99+4(60),HELP1       GIVE HELP INFO
         WTO   MF=(E,MSG99)            INFORM OPER/PROGRAMMER
         MVC   MSG99+4(60),HELP2
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),HELP3
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),HELP4
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),HELP5
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),HELP6
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),HELP7
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),HELP8
         WTO   MF=(E,MSG99)
         MVC   MSG99+4(60),HELP9       9th help msg             @930729
         WTO   MF=(E,MSG99)                                     @930729
         B     ASKCMD                  TRY AGAIN
CMD002   EQU   *                       PROCESS THE OTHER COMMANDS
         CLC   REPLY(6),=C'ALLOC '     ALLOCATE ?
         BNE   CMD004                  GO ON WITH THE OTHER CMDS
         LA    R4,16                   RC=16 IS ALLOC COMMAND
         B     CMDEND                  GO TO MAIN ROUTINE
CMD004   DS    0H                      TEST NEXT COMMANDS
         CLC   REPLY(4),=C'EOJ '       END OF JOB
         BNE   CMD006                  GO ON WITH OTHER COMMANDS
         LA    R4,20                   RC=20 IS EOJ COMMANDS
         B     CMDEND
CMD006   DS    0H                                               @930729
         CLC   =C'RENAME',REPLY        Rename entire dataset    @930729
         BNE   CMD008                  N: Continue              @930729
         LA    R4,24                   RC=24 is RENAME command  @930729
         B     CMDEND                  Return                   @930729
CMD008   DS    0H
         TM    SWITCH,ALLOC            ALLOCATED ?
         BO    CMD010                  ALLRIGHT
         MVC   MSG99+4(60),MSG02       ERROR MSG EDT202E
         WTO   MF=(E,MSG99)            INFORM OPERATOR/PROGRAMMER
         B     ASKCMD
CMD010   DS    0H
         CLC   REPLY(7),=C'PDSLIST'    LISTPDS ?
         BNE   CMD012                  GO ON WITH OTHER COMMANDS
         MVC   PARM33R(16),REPLY       MOVE REPLY               @930726
         LA    R3,PARM33KY             start of key             @930726
         LA    R1,PARM33KY+8           end of key               @930726
         TRT   PARM33KY,TRTABTST       find first blank         @930726
         SR    R1,R3                   compute length           @930726
         STC   R1,PARM33KL             store keylength          @930726
         LA    R4,12                   RC=12 IS PDSLIST COMMAND
         B     CMDEND                  GO TO MAIN
CMD012   DS    0H
         CLC   REPLY(2),=C'D '         DELETE MEMBER ?
         BNE   CMD014                  GO ON WITH OTHER CMDS
         LA    R4,8                    Rc=8, Command, delete member
         MVC   PARM33M(8),REPLY+2      MOVE MEMBER
         B     MEMBCHK                 CHECK MEMBERNAME
CMD014   DS    0H
         CLC   REPLY(2),=C'R '         RENAME MEMBER ?
         BNE   CMD016                  GO ON WITH OTHER CMDS
         MVC   PARM33M(8),BLANKS       CLEAR MEMBER
         LA    R3,REPLY+2              POINT TO MEMBER
         LA    R4,PARM33M              POINT TO OUTPUT
         LA    R5,8                    SCAN MAX 8 TIMES
CMD014A  DS    0H
         CLI   0(R3),C','              COMMA FOUND ?
         BE    CMD014Z                 YES BRANCH
         MVC   0(1,R4),0(R3)           MOVE 1 BYTE MEMBER
         LA    R3,1(,R3)               INCR R3
         LA    R4,1(,R4)               INCR R4
         BCT   R5,CMD014A              LOOP UNTIL ALL DONE
CMD014Z  DS    0H
         MVC   PARM33R(8),1(R3)
         LA    R4,8                    RC=8 RENAME MEMBER COMMAND
         OI    SWITCH,$RENAME          MARK RENAME COMMAND
         B     MEMBCHK                 CHECK MEMBERNAME
CMD016   DS    0H
         CLC   REPLY(2),=C'A '         ADD MEMBER
         BNE   CMD020
         LA    R4,4                    RC=4 IS ADD MEMBER
         MVC   PARM33M(8),REPLY+2      MOVE MEMBER
         B     MEMBCHK
CMD020   DS    0H
         CLC   REPLY(3),=C'AN '        ADD MEMBER WITH CARDNUMBERS
         BNE   CMD022
         LA    R4,4                    RC=4 IS ADD MEMBER
         MVC   PARM33M(8),REPLY+3      MOVE MEMBER
         OI    SWITCH,NBRS             MASK CARDNUMBERS ASK
         B     MEMBCHK
CMD022   DS    0H
         CLC   REPLY(2),=C'U '         UPDATE MEMBER
         BNE   CMD024
         XR    R4,R4                   RC=0 IS UPDATE MEMBER
         MVC   PARM33M(8),REPLY+2      MOVE MEMBER
         OI    SWITCH,UPDT
         B     MEMBCHK
CMD024   DS    0H
         CLC   REPLY(3),=C'UN '        UPDATE MEMBER WITH CARDNUMBERS
         BNE   CMD030                  ERROR
         XR    R4,R4                   RC=0 IS UPDATE MEMBER
         MVC   PARM33M(8),REPLY+3      MOVE IN MEMBER
         OI    SWITCH,NBRS             MASK ASK CARDNUMBERS
         OI    SWITCH,UPDT
         B     MEMBCHK
CMD030   DS    0H
         MVC   MSG99+4(60),MSG03       ERROR MSG EDT202E
         WTO   MF=(E,MSG99)            INFORM OPERATOR/PROGRAMMER
         B     ASKCMD
MEMBCHK  DS    0H
         CLI   PARM33M,C'#'            CHECK FIRST CHARACTER
         BE    MEMBOK                  OK
         CLI   PARM33M,C'$'
         BE    MEMBOK
         CLI   PARM33M,C'@'
         BE    MEMBOK
         CLI   PARM33M,C'A'            ALPHABETIC ?
         BL    MEMBERR                 INVALID MEMBERNAME
         CLI   PARM33M,C'Z'
         BH    MEMBERR
MEMBOK   DS    0H
         B     CMDEND                  GO TO MAIN ROUTINE
MEMBERR  DS    0H                      GIVE ERROR MSG EDT203E
         NI    SWITCH,255-$RENAME      FLAG OFF
         MVC   MSG99+4(60),MSG04
         WTO   MF=(E,MSG99)            INFORM OPER/PROGRAMMER
         B     ASKCMD                  TRY AGAIN
CMDEND   DS    0H
         LR    R15,R4                  RETURNCODE
         L     R13,4(,R13)
         RETURN (14,12),RC=(15)
SAVEAREA DC    18F'0'
REPLY    DS    CL20
MSG02    DC    CL60'EDT202E No dataset allocated - reply ''ALLOC'''
MSG03    DC    CL60'EDT203E Command invalid, Reenter'
MSG04    DC    CL60'EDT204E Membername invalid, Reenter'
HELP1    DC    CL60'EDT205I Commands:'
HELP2    DC    CL60'        ALLOC:            Allocate the library'
HELP3    DC    CL60'        EOJ:              Stop TSTVS'
HELP4    DC    CL60'        PDSLIST,(key):    List directory of libraryx
               '
HELP5    DC    CL60'        A(N) member:      Add a new member'
HELP6    DC    CL60'        U(N) member:      Update member (subcommand*
               s)'
HELP7    DC    CL60'        D member:         Delete the member'
HELP8    DC    CL60'        R member,newname: Rename member'
HELP9    DC    CL60'        RENAME:           Rename an entire dataset'
ECB      DS    F
BLANKS   DC    CL22' '
MSG99    WTO   '1234512345123451234512345123451234512345123451234512345X
               12345',ROUTCDE=(2,3),MF=L
MSG99E   EQU   *
TRTABTST DC    256X'00'                                         @930726
         ORG   TRTABTST                                         @930726
         DC    X'FF'                   binary zero              @930726
         ORG   TRTABTST+X'40'                                   @930726
         DC    X'FF'                   blank                    @930726
         ORG   ,                       reset location counter   @930726
*        GENERAL EQUATES.
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
         EJECT
PARMAREA DSECT                         FROM MAIN MODULE
SWITCH   DS   C                        ALLOCSW
ALLOC    EQU   X'80'                   SET IN TSTVSALL ROUT
NBRS     EQU   X'20'                   INDICATE LINENBRS
UPDT     EQU   X'10'                   REPLY='U' OR 'UN' MEMBERNAME
$UNALL   EQU   X'08'
$RENAME  EQU   X'04'                   IF ON: RENAME COMMAND
$RENDS   EQU   X'02'                   IF ON: RENAME dataset    @930729
ROUTCDE  DS    CL2                     ROUTING CODE MESSAGES
*
PARM33   DS    0CL25                   COMMAREA                 @930726
PARM33M  DS    CL8                     MEMBERNAME
PARM33R  DS    CL8                     NEWNAME
PARM33KY DS    CL8                     KEY FOR PDSLIST          @930726
PARM33KL DS    CL1                     Actual keylength         @930726
         END
@@
//TSTVSSEC  EXEC ASM,MEMBER=TSTVSSEC,ASMPARM=',SYSPARM(MVS)'
//ASM.SYSIN DD DATA,DLM=@@
TSTVSSEC TITLE 'Do all the security checking for TSTVS'
*---------------------------------------------------------------------*
*                                                                     *
*        MODULE:       TSTVSSEC                                       *
*        AUTHOR:       Rob Prins, Amsterdam.                          *
*                                                                     *
*        FUNCTION:     Ask for a valid userid+password from the       *
*                      operator. Validate the given user and          *
*                      check usage of the program TSTVS, by means     *
*                      of checking the resource '$TSTVS' in the       *
*                      FACILITY class.                                *
*                      All the checking is done via the SAF interface *
*                      so TSTVSSEC is suitable for RACF of ACF2       *
*                      systems.                                       *
*                      If ACF2 has been used, please map the          *
*                      FACILITY class in the GSO SAFMAPS              *
*                      If neither RACF or ACF2 aren't active, the     *
*                      TSTVS program password will be asked.          *
*                                                                     *
*        PARAMETER     Jobname                                        *
*                                                                     *
*        CALLED BY:    TSTVS                                          *
*                                                                     *
*        Note:         Assemble TSTVSSEC with SYSPARM(MVS) for the    *
*                      MVS38J version. Without SYSPARM(MVS), the      *
*                      OS/390 or z/OS version assembly will be done.  *
*                                                                     *
*                                                                     *
*        Return code:  0000 - Access to TSTVS is allowed              *
*                      /= 0000 - Access to TSTVS has been rejected    *
*                                                                     *
*      Change log:                                                    *
*      1995/12/19: RPr: incorrect length of resource $TSTVS corrected *
*      2022/01/19: RPr: MVS38J versions RACINIT and RACHECK added     *
*                       and escape possible with reply EOJ.           *
*                                                                     *
*------------------------------------------ (C)-2022 Skybird Systems -*
         SPACE 2
TSTVSSEC CSECT
         SAVE  (14,12),,*
         USING TSTVSSEC,R15            GET LOCAL ADDRESSABILITY @911224
         LA    R11,SAVEA                                        @911224
         ST    R13,SAVEA+4                                      @911224
         ST    R11,8(0,R13)                                     @911224
         LR    R13,R11                                          @911224
         B     START                                            @911224
         DS    0F                                               @911224
SAVEA    DC    18F'-1'
         DC    CL8'&SYSDATE',C' ',CL8'&SYSTIME'
START    DS    0H
         DROP  R15                     KILL LOCAL ADDRESSABILITY@911224
         USING SAVEA,R13               Get program addres.      @921218
         L     R3,0(R1)                Parm from TSTVS (jobname)@921218
         MVI   SW,0                    clear flags
         MVC   PASSWRD,PW1                                      @911224
         OC    PASSWRD,PW2                                      @911224
         MVC   PASSWRD(2),0(R3)        1ST 2 POS OF JOBNAME.
         GETMAIN R,LV=512              Obtain room for workarea @921218
         LR    R4,R1                   PICKUP GOTTEN ADDRESS    @911224
         LA    R6,2                    TRY MAX. 2 TIMES         @911224
         MVC   RC,=F'8'                Set default a bad code   @921218
ASKUID   DS    0H                                               @921218
         XC    ECB1,ECB1               CLEAR ECB                @911224
         MVC   USERID(8),BLANKS        Blank userid             @921218
         WTOR  'EDT301A Enter name of an authorized user or EOJ',      *
               USERID,8,ECB1,ROUTCDE=(9)                        @921218
         WAIT  ECB=ECB1                WAIT FOR REPLY.          @911224
         CLI   USERID,C' '             Nothing replied ?        @921218
         BE    ASKUID                  YES: ASK AGAIN           @921218
         CLC   =C'EOJ',USERID          End of Job replied?
         BNE   CONTUID                 No: continue
         MVC   RC,=F'8'                Set errornous return code
         B     TSTVSRET                Return
CONTUID  DS    0H                                               @911224
         LA    R1,USERID+8             Point after userid       @921218
         TRT   USERID,TRTAB            compute the length       @921218
         LA    R2,USERID               start of field           @921218
         SLR   R1,R2                   Diffence is the length   @921218
         STC   R1,USER                 and save length          @921218
ASKPWD   DS    0H                                               @911224
         XC    ECB1,ECB1               CLEAR ECB                @911224
         MVC   PASSWORD(8),BLANKS      Blank password field     @921218
         WTOR  'EDT302A Enter password of this authorized user',       *
               PASSWORD,8,ECB1,ROUTCDE=(9)                      @921218
         WAIT  ECB=ECB1                WAIT FOR REPLY.          @911224
         CLI   PASSWORD,C' '           NOTHING REPLIED ?        @921218
         BE    ASKPWD                  YES: ASK AGAIN           @911224
         LA    R1,PASSWORD+8           Point after password     @921218
         TRT   PASSWORD,TRTAB          compute the length       @921218
         LA    R2,PASSWORD             start of field           @921218
         SLR   R1,R2                   Diffence is the length   @921218
         STC   R1,PWD                  and save length          @921218
         LA    R8,USER                 A(userid)                @921218
         LA    R7,PWD                  password address         @921218
         AIF   ('&SYSPARM' EQ 'MVS').RAC1I
         RACROUTE REQUEST=VERIFY,                                      *
               ENVIR=CREATE,           Create an ACEE                  *
               ACEE=ACEEADR,           and place address in fullword   *
               USERID=(8),                                             *
               RELEASE=1.9,                                            *
               PASSWRD=(7),                                            *
               TERMID=LUTSTVS,         user's terminal or appl. name   *
               WORKA=(4),                                              *
               MF=(E,VFYLIST)
         AGO   .RAC1E
.RAC1I   ANOP
         RACINIT                                                       *
               ENVIR=CREATE,           Create an ACEE                  *
               ACEE=ACEEADR,           and place address in fullword   *
               USERID=(8),                                             *
               PASSWRD=(7),                                            *
               TERMID=LUTSTVS,         user's terminal or appl. name   *
               MF=(E,VFYLIST)
.RAC1E   ANOP
         XC    PASSWORD,PASSWORD       clear password           @921218
         LTR   R15,R15                 verify ok ?              @921218
         BZ    CHKRSRC                 yes,  LOGON SO FAR VALID.@921218
         MVC   MSG98+4(32),=C'EDT305E RACINIT failed for user=' @921218
         MVC   MSG98+36(8),USERID      move userid in error     @921218
         MVC   MSG98+44(4),=C' rc='                             @921218
         ST    R15,SAFRC               save SAF return code     @921218
         CVD   R15,DWB                 convert rc               @921218
         UNPK  MSG98+48(4),DWB         make it zone and ...     @921218
         OI    MSG98+51,X'F0'          remove sign              @921218
         MVC   MSG98+52(10),=C', RACF rc='                      @921218
         LA    R2,VFYLIST              point to RACROUTE list   @921218
         L     R15,0(,R2)              RACF return code         @921218
         ST    R15,RACFRC              save 'RACF?' rc          @921218
         CVD   R15,DWB                 to decimal               @921218
         UNPK  MSG98+62(4),DWB         make it zone and ...     @921218
         OI    MSG98+65,X'F0'          remove sign              @921218
         CLC   SAFRC,=F'8'             RC > 8                   @921218
         BH    RACRC                   YES: It's a real error   @921218
         BE    CHKPWE                  RC=8 possible passw err. @921218
         CLC   RACFRC,=F'20'           RACF  not active ??      @921218
         BE    BYPRAC                  yes: bypass RACF check   @930429
         CLC   RACFRC,=F'0'            ACF2  not active ??      @930429
         BNE   RACRC                   no: other error          @921218
BYPRAC   DS    0H                                               @930429
         MVC   MSG98+4(66),MSG25       move bypass msg          @921218
         LA    R1,MSG98                                         @921218
         WTO   MF=(E,(1))              INFORM OPERATOR          @911224
         FREEMAIN R,LV=512,A=(4)       freemain workarea        @921218
         B     TSTVSPW0                Ask for TSTVS password   @921218
CHKPWE   DS    0H                                               @921218
         CLC   RACFRC,=F'8'            password not auth.       @921218
         BE    PWNMATCH                yes: give suitable msg   @921218
         CLC   RACFRC,=F'12'           password expired         @921218
         BNE   RACRC                   no: other error          @921218
         MVC   MSG29+15(8),USERID      move userid in error     @921218
         MVC   MSG98+4(66),MSG29       move correct message     @921218
         B     RACRC                   branch                   @921218
PWNMATCH DS    0H                                               @921218
         MVC   MSG28+15(8),USERID      move userid in error     @921218
         MVC   MSG98+4(66),MSG28       move correct message     @921218
RACRC    DS    0H                                               @921218
         LA    R1,MSG98                                         @921218
         WTO   MF=(E,(1))              INFORM OPERATOR          @911224
         B     RACREOJ                 TERMINATE or try again   @921218
CHKRSRC  DS    0H                      Userid verified          @921218
         OI    SW,$CREATE              mark ACEE created        @921218
         L     R5,ACEEADR              Pickup ACEE address      @921218
         LA    R8,SAFENT               Entity field             @921218
         LA    R7,SAFCLASS             Class  field             @921218
         AIF   ('&SYSPARM' EQ 'MVS').RAC2I
         RACROUTE                                                      X
               REQUEST=AUTH,           check if resource authorized    X
               ACEE=(5),               current acee address            X
               WORKA=(4),                                              X
               ENTITYX=((8)),          resource name=$TSTVS            X
               ATTR=READ,                                              X
               RELEASE=1.9,                                            X
               CLASS=(7),              class name = FACILITY           X
               MF=(E,SAFLIST)
         AGO   .RAC2E
.RAC2I   ANOP
         LA    R8,ENTITY               Entity field             @921218
         RACHECK                                                       X
               ACEE=(5),               current acee address            X
               ENTITY=((8)),           resource name=$TSTVS            X
               ATTR=READ,                                              X
               CLASS=(7),              class name = FACILITY           X
               MF=(E,SAFLIST)
.RAC2E   ANOP
         LTR   R15,R15                 Auth SAF call ok ?       @940406
         BNZ   NOPRIV                  No: no access to $TSTVS  @940406
         LA    R1,MSG98                                         @921218
         MVC   MSG98+4(66),MSG27       MSG EDT309I              @921218
         WTO   MF=(E,(1))              userid accepted          @921218
         XC    RC,RC                   RC = 0000                @921218
         B     RACREOJ                 RELEASE storage and cont.@921218
NOPRIV   DS    0H                      No access to resource    @921218
         LA    R1,MSG98                                         @911224
         MVC   MSG98+4(66),MSG26       MSG EDT304E              @911224
         WTO   MF=(E,(1))              INFORM OPERATOR          @911224
         B     RACREOJ                 TRY AGAIN OR TERMINATE   @921218
RACREOJ  DS    0H                                               @921218
         TM    SW,$CREATE              ACEE has been created ?  @921218
         BNO   RACNDEL                 no: do not delete        @921218
         AIF   ('&SYSPARM' EQ 'MVS').RAC3I
         RACROUTE REQUEST=VERIFY,      delete ACEE                     *
               ENVIR=DELETE,                                           *
               ACEE=ACEEADR,           Delete this ACEE                *
               WORKA=(4)
         AGO   .RAC3E
.RAC3I   ANOP
         RACINIT                       delete ACEE                     *
               ENVIR=DELETE,                                           *
               ACEE=ACEEADR            Delete this ACEE
.RAC3E   ANOP
         NI    SW,255-$CREATE          flag off                 @921218
RACNDEL  DS    0H                                               @921218
         ICM   R15,15,RC               return code              @921218
         BZ    TSTVSFR2                All zero: verified       @921218
         BCT   R6,ASKUID               error: ask again         @921218
         FREEMAIN R,LV=512,A=(4)       RELEASE workarea         @921218
         B     TSTVSRET                TERMINATE PROCESSING     @911224
TSTVSFR2 DS    0H                                               @921218
         FREEMAIN R,LV=512,A=(4)       RELEASE workarea         @921218
         B     TSTVSRET                user has been verified   @921218
         TITLE 'No security package active, ask program password'
TSTVSPW0 DS    0H
         XC    ECB1,ECB1               CLEAR ECB FOR WTOR.
         WTOR  'EDT310A Enter TSTVS password',REPLYPW,8,ECB1,          *
               ROUTCDE=9
         WAIT  ECB=ECB1                WAIT FOR REPLY.
         MVC   SYPSWRD,PW3                                      @911224
         OC    SYPSWRD,PW4                                      @911224
         CLC   PASSWRD(2),=C'SY'       SYITSTVS OR NOT.
         BE    TSTVSPW2                IF SY THEN BRANCH
         CLC   PASSWRD(8),REPLYPW      ELSE COMPARE
         BNE   TSTVSRET                WRONG PASSWORD, EXIT
         XC    RC,RC                   Clear return code        @921218
         B     TSTVSRET                Return with RC=0000      @921218
TSTVSPW2 DS    0H                                               @921218
         CLC   REPLYPW(8),SYPSWRD      COMPARE PASSWORD
         BNE   TSTVSRET                WRONG PASSWORD, EXIT
         XC    RC,RC                   Clear return code        @921218
         TITLE 'Return to TSTVS mainline'
TSTVSRET DS    0H                                               @921218
         XC    PASSWRD,PASSWRD         CLEAR PASSWORD FIELD     @911224
         XC    SYPSWRD,SYPSWRD         CLEAR PASSWORD FIELD     @911224
         XC    REPLYPW,REPLYPW         CLEAR REPLY    FIELD     @911224
         L     R15,RC                  Return code              @921218
         L     R13,4(,R13)             Pickup callers savearea  @921218
         RETURN (14,12),RC=(15)        Return to caller         @921218
         TITLE 'Variables, constants and literals'
DWB      DS    D                       For CVD instructions
ACEEADR  DS    F                       A(ACEE) - RACROUTE VFY   @921218
ECB1     DS    F                       ECB for replies          @921218
RC       DS    F                       Returncode RACROUTE's    @921218
SAFRC    DS    F                       SAF return code          @921218
RACFRC   DS    F                       RACF? return code        @921218
*
SW       DS    X                       Flags
$CREATE  EQU   128                     1... .... ACEE created   @921218
*                                      .xxx xxxx reserved
PW1      DC    XL8'0000E002E005E040'
PW2      DC    XL8'000003E003E00200'
PASSWRD  DC    CL8' '
PW3      DC    XL8'E009D007E009C001'
PW4      DC    XL8'02C004D006D004F0'
SYPSWRD  DC    CL8' '
REPLYPW  DC    CL8' '
BLANKS   DC    CL8' '
*
TRTAB    DC    256X'00'                xlate table to check delimiters
         ORG   TRTAB
         DC    X'01'                   00 is a delimiter
         ORG   TRTAB+64
         DC    X'01'                   40 (a blank) is a delimiter
         ORG   ,
*
R0       EQU    0
R1       EQU    1
R2       EQU    2
R3       EQU    3
R4       EQU    4
R5       EQU    5
R6       EQU    6
R7       EQU    7
R8       EQU    8
R9       EQU    9
R10      EQU    10
R11      EQU    11
R12      EQU    12
R13      EQU    13
R14      EQU    14
R15      EQU    15
*
         LTORG ,
         TITLE 'SAF control blocks'
         AIF   ('&SYSPARM' EQ 'MVS').RAC9I
SAFLIST  RACROUTE                                                      X
               REQUEST=AUTH,           check resource                  X
               WORKA=,                                                 X
               ENTITYX=,               resource name                   X
               RELEASE=1.9,                                            X
               CLASS=,                 class name                      X
               ATTR=READ,                                              X
               MF=L
VFYLIST  RACROUTE REQUEST=VERIFY,                                      *
               ENVIR=,                                                 *
               USERID=,                                                *
               RELEASE=1.9,                                            *
               PASSWRD=,                                               *
               WORKA=,                                                 *
               MF=L
         AGO   .RAC9E
.RAC9I   ANOP
SAFLIST  RACHECK                                                       X
               ENTITY=,                resource name                   X
               CLASS=,                 class name                      X
               ATTR=READ,                                              X
               MF=L
VFYLIST  RACINIT                                                       *
               ENVIR=,                                                 *
               USERID=,                                                *
               PASSWRD=,                                               *
               MF=L
.RAC9E   ANOP
SAFLISTL EQU   *-SAFLIST               LENGTH OF RACROUTE LIST FORM
SAFCLASS DC    AL1(8),CL8'FACILITY'    CLASSNAME RACROUTE (FAST)AUTH
*
SAFENT   DS    0CL10                   RACROUTE ENTITY NAME     @921218
ENTL1    DC    AL2(0)                  BUFFER LENGTH            @921218
ENTL2    DC    AL2(6)                  ENTITY LENGTH            @951219
ENTITY   DC    CL6'$TSTVS'             entity name              @921218
         DC    CL34' '                 34 blanks for ACF2       @930506
*
PWD      DC    AL1(8)                  password length          @921218
PASSWORD DS    CL8                     real password            @921218
*
USER     DC    AL1(8)                  userid length            @921218
USERID   DS    CL8                     real userid              @921218
*
LUTSTVS  DC    CL8'LUTSTVS'            input source/terminal    @921218
         TITLE 'Message block'
MSG25    DC    CL66'EDT303E No security system active, bypass userid ch*
               eck...'
MSG26    DC    CL66'EDT304E Userid has no access to the ''TSTVS'' progr*
               am'
MSG27    DC    CL66'EDT309I Userid has been verified and accepted'
MSG28    DC    CL66'EDT306W Userid=xxxxxxxx, unauthorized password'
MSG29    DC    CL66'EDT307W Userid=xxxxxxxx, password has been expired'
*
MSG98    WTO   '                                                       X
                          ',ROUTCDE=(9),MF=L
         END
@@
//*
//LKED    EXEC PGM=IEWL,PARM='XREF,LIST,NCAL'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSDA,SPACE=(3200,(100,300))
//SYSLMOD  DD  DSN=SYS2.LINKLIB,DISP=SHR
//OBJ      DD  DSN=&&OBJ,DISP=OLD
//SYSLIN   DD  DDNAME=SYSIN
//SYSIN DD *
 INCLUDE OBJ(TSTVS,TSTVSCOM,TSTVSALL)
 INCLUDE OBJ(TSTVSSEC)
 SETCODE AC(1)
 ENTRY TSTVS
 NAME TSTVS(R)
//PROCLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.PROCLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=EDIT
//EDIT    PROC ROUT=2
//TSTVS   EXEC PGM=TSTVS,PARM='&ROUT',TIME=1440
//STEPLIB  DD  DSN=SYS2.LINKLIB,DISP=SHR
@@
//*
//* Add the RAKF permissions
//*
//RAKFUPDT EXEC PGM=IKJEFT01,                  
//       REGION=8192K                                         
//TSOLIB   DD   DSN=BREXX.CURRENT.RXLIB,DISP=SHR                             
//RXLIB    DD   DSN=BREXX.CURRENT.RXLIB,DISP=SHR                             
//SYSEXEC  DD   DSN=SYS2.EXEC,DISP=SHR                         
//SYSPRINT DD   SYSOUT=*                                      
//SYSTSPRT DD   SYSOUT=*                                      
//SYSTSIN  DD   *
 RX RDEFINE 'FACILITY $TSTVS UACC(NONE)'
 RX PERMIT '$TSTVS CLASS(FACILITY) ID(ADMIN,STCGROUP) ACCESS(READ)'
//STDOUT   DD   SYSOUT=*,DCB=(RECFM=FB,LRECL=140,BLKSIZE=5600)
//STDERR   DD   SYSOUT=*,DCB=(RECFM=FB,LRECL=140,BLKSIZE=5600)
//STDIN    DD   DUMMY  
