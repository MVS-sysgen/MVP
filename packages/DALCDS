//DALCDS JOB (JOB),
//             'INSTALL DALCDS',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//MACLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=&&MACLIBS,UNIT=SYSDA,
//             DISP=(,PASS,DELETE),
//             SPACE=(TRK,(04,02,02)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=DVCTBL
         MACRO
&NAME    DVCTBL
         PUSH  PRINT
         PRINT GEN
**********************************************************************
*   Macro:  DVCTBL    V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
**********************************************************************
*              8 bytes   /  DVCLST Table-------------------------    *
*              per entry+   01 bytes, device type code               *
*                        \  07 bytes, Device Type                    *
**********************************************************************
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
DVCLST$  DC    X'00',CL7'??***??'
DVCLST$L EQU   *-DVCLST$           Table Entry length
         DC    X'01',CL7'2311   '
         DC    X'02',CL7'2301   '
         DC    X'03',CL7'2303   '
         DC    X'04',CL7'9345   '
         DC    X'05',CL7'2321   '
         DC    X'06',CL7'2305-1 '
         DC    X'07',CL7'2305-2 '
         DC    X'08',CL7'2314   '
         DC    X'09',CL7'3330   '
         DC    X'0A',CL7'3340   '
         DC    X'0B',CL7'3350   '
         DC    X'0C',CL7'3375   '
         DC    X'0D',CL7'3330-11'
         DC    X'0E',CL7'3380   '
         DC    X'0F',CL7'3390   '
DVCLST$E EQU   (*-DVCLST$)/DVCLST$L  Table Entries
*
*              END OF DVCTBL
         POP   PRINT
         MEND
./ ADD NAME=ISPFPL
         MACRO
         ISPFPL
         PUSH  PRINT
         PRINT GEN
**********************************************************************
*   Macro:  ISPFPL    V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
**********************************************************************
*        ISPF LINK Parameter Address List                            *
**********************************************************************
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
ISPLINK  DS    F                   Entry point for ISPLINK
ISPFP1   DS    A                   ISPLINK Parm 1
ISPFP2   DS    A                   ISPLINK Parm 2
ISPFP3   DS    A                   ISPLINK Parm 3
ISPFP4   DS    A                   ISPLINK Parm 4
ISPFP5   DS    A                   ISPLINK Parm 5
ISPFP6   DS    A                   ISPLINK Parm 6
ISPFP7   DS    A                   ISPLINK Parm 7
ISPFP8   DS    A                   ISPLINK Parm 8
ISPFP9   DS    A                   ISPLINK Parm 9
ISPFP10  DS    A                   ISPLINK Parm 10
*
*              END OF ISPFPL
         POP   PRINT
         MEND
./ ADD NAME=ISPFSRV
         MACRO
         ISPFSRV
         PUSH  PRINT
         PRINT GEN
**********************************************************************
*   Macro:  ISPFSRV   V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
**********************************************************************
*        ISPF Services and keywords (kw)                             *
**********************************************************************
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
CONTROL  DC    CL8'CONTROL '               Service Control
VDEFINE  DC    CL8'VDEFINE '               Service VDEFINE
VDELETE  DC    CL8'VDELETE '               Service VDELETE
DISPLAY  DC    CL8'DISPLAY '               Service Display
SELECT   DC    CL8'SELECT  '               Service Select
BROWSE   DC    CL8'BROWSE  '               Service BROWSE
EDIT     DC    CL8'EDIT    '               Service EDIT
TBDISPL  DC    CL8'TBDISPL '               Service TBDISPL
TBCREATE DC    CL8'TBCREATE'               Service TBCREATE
TBERASE  DC    CL8'TBERASE '               Service TBERASE
TBADD    DC    CL8'TBADD   '               Service TBADD
TBMOD    DC    CL8'TBMOD   '               Service TBMOD
TBPUT    DC    CL8'TBPUT   '               Service TBPUT
TBGET    DC    CL8'TBGET   '               Service TBGET
TBSKIP   DC    CL8'TBSKIP  '               Service TBSKIP
TBDELETE DC    CL8'TBDELETE'               Service TBDELETE
TBTOP    DC    CL8'TBTOP   '               Service TBTOP
TBBOTTOM DC    CL8'TBBOTTOM'               Service TBBOTTOM
TBSORT   DC    CL8'TBSORT  '               Service TBSORT
TBEND    DC    CL8'TBEND   '               Service TBEND
TBCLOSE  DC    CL8'TBCLOSE '               Service TBCLOSE
TBEXIST  DC    CL8'TBEXIST '               Service TBEXIST
TBQUERY  DC    CL8'TBQUERY '               Service TBQUERY
TBSTATS  DC    CL8'TBSTATS '               Service TBSTATS
TBSCAN   DC    CL8'TBSCAN  '               Service TBSCAN
TBSARG   DC    CL8'TBSARG  '               Service TBSARG
VGET     DC    CL8'VGET    '               Service VGET
VPUT     DC    CL8'VPUT    '               Service VPUT
GETMSG   DC    CL8'GETMSG  '               Service GETMSG
SETMSG   DC    CL8'SETMSG  '               Service SETMSG
ERRORS   DC    CL8'ERRORS  '               kw ERRORS
RETURN   DC    CL8'RETURN  '               kw RETURN
LOCK     DC    CL8'LOCK    '               kw LOCK
SAVE     DC    CL8'SAVE    '               kw SAVE
RESTORE  DC    CL8'RESTORE '               kw RESTORE
LINE     DC    CL8'LINE    '               kw LINE
PROFILE  DC    CL8'PROFILE '               kw PROFILE
SHARED   DC    CL8'SHARED  '               kw SHARED
CHAR     DC    CL8'CHAR    '               kw CHAR
ALL      DC    CL8'*       '               kw ALL (*)
NOWRITE  DC    CL8'NOWRITE '               kw NOWRITE
REPLACE  DC    CL8'REPLACE '               kw REPLACE
YES      DC    CL8'YES     '               kw YES
NO       DC    CL8'NO      '               kw NO
B        DC    CL8'        '               kw B as blanks
*
*              END OF ISPFSRV
         POP   PRINT
         MEND
./ ADD NAME=LBISPL
         MACRO
&NAME    LBISPL &ENTRY,&PARMS,&VL
**********************************************************************
*   Macro:  LBISPL    V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
.*********************************************************************
.*       LBISPL ISPLINK,(PARM1,PARM2,PARM3,PARM4,PARM5),VL           *
.*********************************************************************
.*       This macro loads a parm address list using LA instruction   *
.*       and BALR to R15                                             *
.*       - Uses R1,R14,R15                                           *
.*       - Uses variables ISPFP1-ISPFP5                              *
.*********************************************************************
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
         LCLA  &A,&NPARMS
         LCLC  &B
&NPARMS  SETA  N'&PARMS
         AIF   (&A GT 5).TOOMANY
&A       SETA  1
.LP      ANOP
         AIF   (&A GT &NPARMS).BP1
         LA    1,&PARMS(&A)        Parm Addr List  &A
         ST    1,ISPFP&A
&A       SETA  &A+1
         AGO   .LP
.BP1     ANOP
         AIF   ('&VL' NE 'VL').BP2
         OI    ISPFP&NPARMS,X'80'  VL Mark
.BP2     ANOP
         LA    1,ISPFP1            R1 - Addr of PARMS
         L     15,&ENTRY           R15- Entry Point
         BALR  14,15               Branch to Entry Point
         AGO   .ENDME
.TOOMANY ANOP
         MNOTE 'More than 5 parms, max is 5!'
         AGO    .ENDME
.ENDME   ANOP
         MEND
./ ADD NAME=MISCDC
         MACRO
&NAME    MISCDC
         PUSH  PRINT
         PRINT GEN
**********************************************************************
*   Macro:  MISCDC    V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
**********************************************************************
*        This macro declares common misc constants                   *
**********************************************************************
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
QUOTES   DC    C'"'                Double quotes
APSTROFE DC    C''''               Apostrophe
*
*              END OF MISCDC
         POP   PRINT
         MEND
./ ADD NAME=MOVEC
         MACRO
&NAME    MOVEC &P
**********************************************************************
*   Macro:  MOVEC     V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
.*********************************************************************
.*       Macro Description:                                          *
.*       ------------------                                          *
.*       o  Move data string starting at R6 and augmenting R6 and R8 *
.*          by length of string moved.                               *
.*          by 1 for each byte moved.                                *
.*       o  &P must start and end with apostrophes.                  *
.*       o  &P length must be >0 and <= 42 bytes with single quotes. *
.*       o  MOVEI must be executed to set R6 and R8 prior to using   *
.*          this macro                                               *
.*          - R6=Current Address of &P as pointer                    *
.*          - R8=Current Length  of &P as bytes moved into &P        *
.*                                                                   *
.*       Examples:                                                   *
.*       ---------                                                   *
.*       MOVEC 'string data in quotes'                               *
.*                                                                   *
.*       - Uses R6,R8                                                *
.*********************************************************************
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
         LCLA  &A,&B,&X
         LCLC  &C,&D,&PD
&A       SETA  K'&P          Length of VAR
&B       SETA  &A-2          Minus quotes
&C       SETC  '&P'(1,1)     1st  position value
&D       SETC  '&P'(&A,1)    Last position value
&PD      SETC  '&P'(2,&A-2)    &P Content
&X       SETA  2
.CHK2QT  ANOP
         AIF   (&X GT &A-2).CONT0
         AIF   ('&P'(&X,2) EQ '''''').FND2QT
&X       SETA  &X+1
         AGO    .CHK2QT
.FND2QT  ANOP
&B       SETA  &B-1
&X       SETA  &X+2
         AGO    .CHK2QT
.CONT0   ANOP
         AIF   (&A LT 3).TOOSHRT
         AIF   (&A GT 42).TOOLONG
         AIF   ('&C' NE '''').NOQINB
         AIF   ('&D' NE '''').NOQINE
&NAME    MVC   0(&B,R6),=C&P
         LA    6,&B.(6)               adj addr
         LA    8,&B.(8)               adj len
         MNOTE 'Moving &B bytes from string to R6 pointer'
         AGO    .ENDME
.TOOSHRT ANOP
         MNOTE 12,'String content must be > 0 bytes'
         AGO    .ENDME
.TOOLONG ANOP
         MNOTE 12,'String content > 40 bytes'
         AGO    .ENDME
.NOQINB  ANOP
         MNOTE 12,'String must start with apostrophe'
         AGO    .ENDME
.NOQINE  ANOP
         MNOTE 12,'String must end with apostrophe'
         AGO    .ENDME
.ENDME   ANOP
         MEND
./ ADD NAME=MOVEI
         MACRO
&NAME    MOVEI &P,&M
**********************************************************************
*   Macro:  MOVEI     V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
.*********************************************************************
.*       Macro Description:                                          *
.*       ------------------                                          *
.*       o  &P length must be >0 and <= 256 bytes.                   *
.*       o  If &M is blank or 'INIT' Initialize R6 and R8            *
.*          - Initialize R6 and R8 for use in MOVEn macros           *
.*          - R6=Current Address of &P as pointer                    *
.*          - R8=Current Length  of &P as bytes moved into &P        *
.*       o  If &M is 'INIT' or 'ONLY'                                *
.*          - Initialize &P to spaces (x'40')                        *
.*       o  Data moves starting at PTR (R6) and R8 augments by 1 for *
.*          each byte.                                               *
.*                                                                   *
.*       Examples:                                                   *
.*       ---------                                                   *
.*       MOVEI VAR           Init R6=addr of VAR, R8=0  VAR=?        *
.*       MOVEI VAR,INIT      Init R6=addr of VAR, R8=0  VAR=spaces   *
.*       MOVEI VAR,ONLY      Init VAR=spaces                         *
.*                                                                   *
.*       - Uses R6,R8                                                *
.*********************************************************************
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
         LCLA  &A
         LCLC  &C
&A       SETA  L'&P          Length of VAR
&C       SETC  '&NAME'       Label
         AIF   (&A LE 0).TOOSHRT
         AIF   (&A GT 256).TOOLONG
         AIF   ('&M' EQ '').RINIT
         AIF   ('&M' EQ 'INIT').RINIT
         AIF   ('&M' EQ 'ONLY').VARINIT
         MNOTE 12,'Parm 2 must be ONLY'
         AGO    .ENDME
.RINIT   ANOP
&NAME    LA    6,&P                  Addr of String
         LA    8,0                   Length of String
         MNOTE 'Set R6 and R8 for &P'
&C       SETC   ''
         AIF   ('&M' EQ 'INIT').ENDME
.VARINIT ANOP
&C       MVI   &P,C' '               Blank TEXT
         AIF   (&A EQ 1).BPMVC
         MVC   &P+1(L'&P-1),&P
.BPMVC   ANOP
         MNOTE 'Blank &A bytes for &P'
         AGO    .ENDME
.TOOSHRT ANOP
         MNOTE 12,'&P must be > 0 bytes'
         AGO    .ENDME
.TOOLONG ANOP
         MNOTE 12,'&P length > 256 bytes'
         AGO    .ENDME
.ENDME   ANOP
         MEND
./ ADD NAME=MOVER
         MACRO
&NAME    MOVER &P,&LL
**********************************************************************
*   Macro:  MOVER     V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
.*********************************************************************
.*       Macro Description:                                          *
.*       ------------------                                          *
.*       o  Move data from &P starting at R6 to last non-blank byte  *
.*          augmenting R6 and R8 by 1 for each byte moved.           *
.*       o  &P length defaults to L'&P or value of &LL               *
.*       o  &LL must be numeric, if specified.                       *
.*       o  &P length must be >0 and <= 256 bytes.                   *
.*       o  MOVEI must be executed to set R6 and R8 prior to using   *
.*          this macro                                               *
.*          - R6=Current Address of &P as pointer                    *
.*          - R8=Current Length  of &P as bytes moved into &P        *
.*                                                                   *
.*       Examples:                                                   *
.*       ---------                                                   *
.*       MOVER PARM           Move PARM for length of PARM           *
.*       MOVER PARM,20        Move PARM for length of 20             *
.*                                                                   *
.*       - Uses R6,R8                                                *
.*       - Uses R3,R4,R7 for RTRIM subroutine                        *
.*********************************************************************
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
         LCLA  &A
         LCLC  &C
&C       SETC  T'&P          Type of VAR
         AIF   ('&C' EQ 'U').UNDEF
         AGO    .DEF
.UNDEF   ANOP
         MNOTE 'MOVEI, Undefined reference &P'
         MNOTE 'MOVEI, Using length of &P'
         AIF   ('&LL' GT '').SETLLL
         AGO    .STRT00
.SETLLL  ANOP
.*       AIF   (&LL GT &A).TO2LONG
&A       SETA  &LL         Length of OVERRIDE LENGTH
         AIF   (&A LE 0).TOOSHRT
         AIF   (&A GT 256).TOOLONG
         AGO    .STRT00
.*
.*
.DEF     ANOP
&A       SETA  L'&P          Length of VAR
         AIF   ('&LL' GT '').SETLLL2
         AGO    .SETLLL3
.SETLLL2 ANOP
.*       AIF   (&LL GT &A).TO2LONG
&A       SETA  &LL         Length of OVERRIDE LENGTH
.SETLLL3 ANOP
         AIF   (&A LE 0).TOOSHRT
         AIF   (&A GT 256).TOOLONG
.*
.*
.STRT00  ANOP
&NAME    LA    3,&P                  Addr of String
         AIF   ('&C' EQ 'U').SETUND
.SETDEF  ANOP
         LA    4,&A                  Override Length of String
         AGO    .SETCONT
.SETUND  ANOP
         LA    4,L'&P                Length of String
.SETCONT ANOP
         BAL   7,RTRIM               RTRIM PCNDSN
         MNOTE 'RTRIM, move 1-&A bytes to ptr R6, R8=cur len'
           MNOTE '       from &P type(&C)'
         AGO    .ENDME
.TOOSHRT ANOP
         MNOTE 12,'&P content must be > 0 bytes'
         AGO    .ENDME
.TOOLONG ANOP
         MNOTE 12,'&P content > 256 bytes'
         AGO    .ENDME
.TO2LONG ANOP
         MNOTE 12,'OVR LEN of &LL > &A bytes'
         AGO    .ENDME
.NOTCHAR ANOP
.*         MNOTE 12,'&P not character type (&C)'
         AGO    .ENDME
.ENDME   ANOP
         MEND
./ ADD NAME=MOVEV
         MACRO
&NAME    MOVEV &P,&OL
**********************************************************************
*   Macro:  MOVEV     V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
.*********************************************************************
.*       Macro Description:                                          *
.*       ------------------                                          *
.*       o  Move data from &P starting at R6 and augmenting R6 and R8*
.*          by length of &P.                                         *
.*          by 1 for each byte moved.                                *
.*       o  &P length defaults to L'&P or value of &OL               *
.*       o  &OL must be numeric, if specified.                       *
.*       o  &P length must be >0 and <= 256 bytes.                   *
.*       o  MOVEI must be executed to set R6 and R8 prior to using   *
.*          this macro                                               *
.*          - R6=Current Address of &P as pointer                    *
.*          - R8=Current Length  of &P as bytes moved into &P        *
.*                                                                   *
.*       Examples:                                                   *
.*       ---------                                                   *
.*       MOVEV PARM           Move PARM for length of PARM           *
.*       MOVEV PARM,20        Move PARM for length of 20             *
.*                                                                   *
.*       - Uses R6,R8                                                *
.*********************************************************************
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
         LCLA  &A,&B
&A       SETA  L'&P          Length of VAR
&B       SETA  K'&OL         Count  of VAR
         AIF   (&A GT 256).TOOLONG
         AIF   (&B GT 0).GOTOL
&B       SETA  &A
         AGO    .CONT01
.GOTOL   ANOP
         AIF   (&B GT 3).OL2BIG
         AIF   (&OL GT 256).OL2BIG2
&B       SETA  &OL
.CONT01  ANOP
&NAME    MVC   0(&B,6),&P
         LA    6,&B.(6)               adj addr
         LA    8,&B.(8)               adj len
         MNOTE 'Moving &B bytes from &P to R6 pointer'
         AGO    .ENDME
.OL2BIG  ANOP
         MNOTE 12,'Override Length > 3 digits'
         AGO    .ENDME
.OL2BIG2 ANOP
         MNOTE 12,'Override Length > 256'
         AGO    .ENDME
.TOOLONG ANOP
         MNOTE 12,'&P length > 256 bytes'
         AGO    .ENDME
.ENDME   ANOP
         MEND
./ ADD NAME=RDTECOMA
         MACRO
&NAME    RDTECOMA
         PUSH  PRINT
         PRINT GEN
**********************************************************************
*   Macro:  RDETCOMA  V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
**********************************************************************
*     Date-Time-Environment Communications Area                      *
**********************************************************************
*                                                                    *
*     Representation of date-time-environment result data:           *
*                                                                    *
*     0        1         2         3         4                       *
*     123456789012345678901234567890123456789012345                  *
*     +---+----+----+----+----+----+----+----+----+                  *
*     12/31/2001.365 10:11:12:45December 1Monday                     *
*     MM/DD/CCYY.JJJ HH:MM:SS:THLLLLLLLLLNWWWWWWWWW                  *
*                                                                    *
*     4   5         6         7         8                            *
*     67890123456789012345678901234567890                            *
*     +---+----+----+----+----+----+----+                            *
*     LARRY01 ILKJACCTTSOLOGONTSOFGISPARM                            *
*     Jobname StepnameProcstepEnvirParmty                            *
*                                                                    *
*     NOTE:  This macro should be in sync with                       *
*     -----  COBOL copybook, RDTECOMC.                               *
*                                                                    *
**********************************************************************
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
$DTECOMS EQU   *
RDATE    DS    0CL14           'MM/DD/CCYY.JJJ'
RDATEMM  DS    CL2             MM
RDATES1  DS    CL1             Separator
RDATEDD  DS    CL2             DD
RDATES2  DS    CL1             Separator
RDATECC  DS    CL2             CC
RDATEYY  DS    CL2             YY
RDATES3  DS    CL1             Separator
RDATEJJJ DS    CL3             JJJ
RDATES4  DS    CL1             Blank
RTIME    DS    0CL11           'HH:MM:SS:TT'
RTIMEHH  DS    CL2             HH
RTIMES1  DS    CL1             Separator
RTIMEMM  DS    CL2             MM
RTIMES2  DS    CL1             Separator
RTIMESS  DS    CL2             SS
RTIMES3  DS    CL1             Separator
RTIMETT  DS    CL2             TT
RMONTHN  DS    CL9             Month Name     i.e. January
RDAY#    DS    CL1             Weekday Number 0-Sunday to 6-Saturday
RDAYN    DS    CL9             Weekday        i.e  Tuesday
RJOBNAM  DS    CL8             JOB Name / UserID
RSTEPNAM DS    CL8             Step Name / Logon PROC
RPROCSTP DS    CL8             Proc Step Name / TSO Logon PROC
RENV     DS    CL5             Environment Type
*                              TSOFG - TSO Foreground
*                              TSOBG - TSO Background (run in batch)
*                              BATCH - Not TSO, must be Batch
RPARMTY  DS    CL6             PARM Type
*                              ISPARM - Parm
*                              ISCPPL - Parm
*                              ISPARM  Parm
$DTECOME EQU   *
$DTECOML EQU   $DTECOME-$DTECOMS     Length of Result Area
*
*              END OF RDTECOMA
         POP   PRINT
         MEND
./ ADD NAME=RDTECOMC
       01  RDTECOM.
      * /************************************************************/
      * /*Copybook: RDTECOMC  (COBOL)     V1R0M00    07/30/2020     */
      * /*  Author: Larry Belmontes, Jr.                            */
      * /*  https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j    */
      * /*  Copyright (C) 2020-2021  Larry Belmontes, Jr.           */
      * /************************************************************/
      * /************************************************************/
      * /* Date-Time-Environment Communications Area                */
      * /************************************************************/
      * /* Representation of date-time-environment result data:     */
      * /*                                                          */
      * /* 0        1         2         3         4                 */
      * /* 123456789012345678901234567890123456789012345            */
      * /* +---+----+----+----+----+----+----+----+----+            */
      * /* 12/31/2001.365 10:11:12:45December 1Monday               */
      * /* MM/DD/CCYY.JJJ HH:MM:SS:THLLLLLLLLLNWWWWWWWWW            */
      * /*                                                          */
      * /* 4   5         6         7         8                      */
      * /* 67890123456789012345678901234567890                      */
      * /* +---+----+----+----+----+----+----+                      */
      * /* LARRY01 ILKJACCTTSOLOGONTSOFGISPARM                      */
      * /* Jobname StepnameProcstepEnvirParmty                      */
      * /*                                                          */
      * /*                                                          */
      * /* NOTE:  This copybook should be in sync with              */
      * /* -----  assembler version, RDTECOMA.                      */
      * /*                                                          */
      * /************************************************************/
      * /* Disclaimer:                                              */
      * /* ======================================================== */
      * /*                                                          */
      * /*    No guarantee; No warranty; Install / Use at your own  */
      * /* risk.                                                    */
      * /*                                                          */
      * /*    This software is provided "AS IS" and without any     */
      * /* expressed  or implied warranties, including, without     */
      * /* limitation, the implied warranties of merchantability    */
      * /* and fitness for a particular purpose.                    */
      * /*                                                          */
      * /*    The author requests keeping authors name intact in    */
      * /* any modified versions.                                   */
      * /*                                                          */
      * /*    In addition, the author requests readers to submit    */
      * /* any code modifications / enhancements and associated     */
      * /* comments for consideration into a subsequent release     */
      * /* (giving credit to contributor(s)) thus, improving        */
      * /* overall functionality and further benefiting the         */
      * /* MVS 3.8J hobbyist public domain community.               */
      * /*                                                          */
      * /*                                                          */
           05  RDATE.
               10  RDATEMM       PIC X(02).
               10  RDATES1       PIC X(01).
               10  RDATEDD       PIC X(02).
               10  RDATES2       PIC X(01).
               10  RDATECC       PIC X(02).
               10  RDATEYY       PIC X(02).
               10  RDATES3       PIC X(01).
               10  RDATEJJJ      PIC X(03).
           05  RDATES4           PIC X(01).
           05  RTIME.
               10  RTIMEHH       PIC X(02).
               10  RTIMES1       PIC X(01).
               10  RTIMEMM       PIC X(02).
               10  RTIMES2       PIC X(01).
               10  RTIMESS       PIC X(02).
               10  RTIMES3       PIC X(01).
               10  RTIMETT       PIC X(02).
           05  RMONTHN           PIC X(09).
           05  RDAYNUM           PIC X(01).
           05  RDAYN             PIC X(09).
           05  RJOBNAM           PIC X(08).
           05  RSTEPNAM          PIC X(08).
           05  RPROCSTP          PIC X(08).
           05  RENV              PIC X(05).
      *                        TSOFG - TSO Foreground
      *                        TSOBG - TSO Background (run in batch)
      *                        BATCH - Not TSO, must be Batch
           05  RPARMTY           PIC X(06).
      *                        ISPARM - Parm
      *                        ISCPPL - Parm
      *                        ISPARM  Parm
./ ADD NAME=RTRIM
         MACRO
         RTRIM
         PUSH  PRINT
         PRINT GEN
**********************************************************************
*   Macro:  RTRIM     V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
*     * /************************************************************/
*     * /* Subroutine - RTRIM Remove trailing spaces         (R7)   */
*     * /* - R3 Address of string (on entry)                        */
*     * /* - R4 Original length of string (on entry)                */
*     * /* - R3 Address of string (restored on exit)                */
*     * /* - R4 Revised  length of string (on exit)                 */
*     * /* - R6 Receiving string address (on entry)                 */
*     * /* - R8 Receiving string length  (on entry)                 */
*     * /************************************************************/
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
RTRIM    EQU   *
         ST    R3,SAVER3S          Save R3
         AR    R3,R4               Point past end Addr of String
         BCTR  R3,0                Adjust to end addr of String
RTRIML   EQU   *
         CLI   0(R3),C' '          Blank delimiter?
         BNE   RTRIMM              No, found revised length
*                                  Yes, keep looking backwards
         BCTR  R3,0                Look back 1 position
         BCT   R4,RTRIML           Try again...
*                                  String all BLANKS!  Zero Length
         B     RTRIMX
RTRIMM   EQU   *
         L     R3,SAVER3S          Restore R3
         BCTR  R4,0                  Adjust for EX
         EX    R4,MVCTEXT            EX Move
         LA    R4,1(R4)              Restore from EX
         AR    R6,R4                 R6=Adj addr TEXT
         AR    R8,R4                 R8=Adj len  TEXT
RTRIMX   EQU   *
         L     R3,SAVER3S          Restore R3
         BR    R7                  Return to caller, R4=Revised Len
*
MVCTEXT  MVC   0(0,R6),0(R3)
*
*              END OF RTRIM
         POP   PRINT
         MEND
./ ADD NAME=SVC78A
         MACRO
         SVC78A
         PUSH  PRINT
         PRINT GEN
**********************************************************************
*   Macro:  SVC78A    V1R0M00    07/30/2020
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/SHRABIT-MACLIB-in-mvs-3-8j
*           Copyright (C) 2020-2021  Larry Belmontes, Jr.
**********************************************************************
**********************************************************************
*        SVC 78 Message area                                         *
**********************************************************************
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
FSPMSG   EQU   *                   30 byte message text for SVC 78
         DS    CL6
FSCYLS   DS    CL4                 Total free cylinders
         DS    CL1
FSTRKS   DS    CL4                 Total additional free tracks
         DS    CL1
FSEXTS   DS    CL4                 Total free extents
         DS    CL1
FSLCYLS  DS    CL4                 Cylinders in largest free extent
         DS    CL1
FSLTRKS  DS    CL4                 Additional trks in largest free ext
*
*              END OF SVC78A
         POP   PRINT
         MEND
@@
//* -------------------------------------------------------*
//* *  DALCDS for MVS3.8J TSO / Hercules                   *
//* *                                                      *
//* *  JOB: $INST04                                        *
//* *       Install DALCDS Program                         *
//* *                                                      *
//* *  - Programs install          to xxxxxxxx.ISPLLIB     *
//* *  - Install libraries marked...                       *
//* *    - Search for '<--TARGET'                          *
//* *    - Update install libraries per your               *
//* *      installation standard                           *
//* *                                                      *
//* -------------------------------------------------------*
//ASML     PROC MBR=WHOWHAT
//ASM      EXEC PGM=IFOX00,
//             PARM='NODECK,LOAD,RENT,TERM,XREF'
//SYSGO    DD  DSN=&&LOADSET,DISP=(MOD,PASS),SPACE=(CYL,(1,1)),
//             UNIT=VIO,DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//         DD  DSN=SYS2.MACLIB,DISP=SHR          ** YREG  **
//         DD  DSN=&&MACLIBS,DISP=OLD       ** ShareABitofIT **
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSPUNCH DD  DSN=NULLFILE
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(6,1))
//SYSUT2   DD  UNIT=VIO,SPACE=(CYL,(6,1))
//SYSUT3   DD  UNIT=VIO,SPACE=(CYL,(6,1))
//SYSIN    DD  DUMMY
//*
//LKED     EXEC PGM=IEWL,PARM='MAP,LIST,LET,RENT,XREF',
//             COND=(0,NE,ASM)
//SYSLIN   DD  DSN=&&LOADSET,DISP=(OLD,DELETE)
//         DD  DDNAME=SYSIN
//SYSLMOD  DD  DSN=SYSGEN.ISPF.LLIB(&MBR),DISP=SHR 
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(5,2))
//SYSIN    DD  DUMMY
//         PEND
//*
//* -------------------------------------------------------*
//* *                                                      *
//* *  Assemble Link-edit DALCDS                           *
//* *                                                      *
//* -------------------------------------------------------*
//DALCDS   EXEC  ASML,MBR=DALCDS
//ASM.SYSIN DD DATA,DLM=@@
         TITLE 'DALCDS - ISPF Display Allocated Dataset Information   '
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  ==================================================================
*
*  DDDDD     AAA   LL       CCCCC  DDDDD    SSSSS
*  DD   D  AA   AA LL      CC   CC DD   D  SS    S
*  DD   DD AA   AA LL      CC      DD   DD SS
*  DD   DD AAAAAAA LL      CC      DD   DD  SSSSS
*  DD   DD AA   AA LL      CC      DD   DD      SS
*  DD   D  AA   AA LL      CC   CC DD   D  S    SS
*  DDDDD   AA   AA LLLLLLL  CCCCC  DDDDD    SSSSS
*
*  ==================================================================
*
*
*  Program: DALCDS
*
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/DALCDS-in-MVS38J
*           Copyright (C) 2020  Larry Belmontes, Jr.
*
*  Disclaimer:
*  ===================================================================
*
*     No guarantee; No warranty; Install / Use at your own risk.
*
*     This software is provided "AS IS" and without any expressed
*  or implied warranties, including, without limitation, the
*  implied warranties of merchantability and fitness for a
*  particular purpose.
*
*     The author requests keeping authors name intact in any
*  modified versions.
*
*     In addition, the author requests readers to submit any
*  code modifications / enhancements and associated comments
*  for consideration into a subsequent release (giving credit
*  to contributor(s)) thus, improving overall functionality
*  and further benefiting the MVS 3.8J hobbyist public domain
*  community.
*
*
         EJECT
*  Overview:
*  ================================================================
*
*     DALCDS displays allocated dataset information for the current
*  TSO session under the MVS 3.8J environment.
*
*     DALCDS displays date and time on line 1 and User ID, Step name,
*  and Logon Proc name on line 4.  TSO allocations display on line 6.
*
*     DALCDS display snippet follows:
*
*   ---------------------------------------------------------
*   | dd/mm/yy.jjj hh:mm      -----  TSO Allocations   --...|   Line 1
*   | COMMAND ===>                                       ...|        2
*   |                                                    ...|        3
*   | UserID- LARRY01  Step- IKJACCNT Proc- ISPLGN00     ...|        4
*   |                                                    ...|        5
*   | DD# DDNAME   ORG DISP VOLSER DSNAME                ...|        6
*   | 001 STEPLIB  PO  Shr  PUB005 ISP.V2R1M0.LLIB       ...|        7
*   |                  Shr  PUB005 LARRY01.REVIEW.MYLOAD ...|        8
*   | 002 SYS00004 VS  Shr  PUB000 SYS1.UCAT.TSO         ...|        9
*   | 003 SYS00002 VS  Shr  MVSCAT SYS1.UCAT.MVS         ...|        .
*   | 004 ISPPROF  PO  Shr  PUB000 LARRY01.ISP.PROF      ...|        .
*   | 005 SYSOUT   PS  New         Terminal              ...|        .
*   ---------------------------------------------------------
*
*     The method used to acquire TSO allocations is via the TIOT
*  and JFCB.  In essence, for each DD, the JFCB is interrogated for
*  DSN and other file information.
*
*     Date and time values are sourced from ISPF Z variables or
*  subroutine GETDTE, if found in a link library.  If GETDTE is not
*  installed on your system or elect not to do so, no problem!
*  See Prerequisite section below for more information.
*
*     DALCDS uses ISPF generic message ISRZ002 for posting errors.
*  Message set ISRZ00 must be installed to display message content.
*  Otherwise, a 'not found' message is posted.  See Prerequisite
*  section below for more information.
*
*     Some common macros are used in this program and other
*  ShareABitofIT software before assembly-link-edit which requires
*  installation of SHRABIT.MACLIB.  See Prerequisite section below
*  for more information.
*
         EJECT
*  Overview:  (continued)
*  ================================================================
*
*     DALCDS, can be invoked directly from an ISPF panel or menu
*  using 'CMD(DALCDS) NEWAPPL(DALC)' as follows:
*
*     For example -
*
*       Assume an existing ISPF panel has the following )PROC section,
*       the 'NEW ENTRY' line can be added to invoke DALCDS when
*       option UA is entered on the panel (menu) command line.
*
*         )PROC
*           &ZSEL = TRANS( TRUNC (&ZCMD,'.')
*                         1,'CMD(xxxxx) NEWAPPL(ISR)'
*                         6,'PGM(xxxxxx)'
*                        UA,'CMD(DALCDS) NEWAPPL(DALC)'  <-- NEW ENTRY
*                       ' ',' '
*                         *,'?' )
*         )END
*
*     Alternatively, a command entry can be defined in the ISPF
*  command table (ISPCMDS) as follows:
*
*     For example -
*
*               VERB      T  ACTION
*                               DESCRIPTION
*
*          ____ DALCDS    6  SELECT CMD(DALCDS) NEWAPPL(DALC)
*                               DISPLAY TSO ALLOCATIONS
*
*     Then, you can access DALDCS from any screen by typing DALDCS
*  on the command line.
*
*     DALCDS is based on Wally Mclaughlin's ISPF product Ver 2.1 and
*  written in IFOX00 under MVS38J TK3 / Hercules.
*
*     Other ISPF resource required include:
*            - PDALCDS0  TSO Allocation Display Panel
*            - HDALCDS0  TSO Allocation Help    Panel
*            - ISRZ00    Messages, use the generic ISRZ002 message
*              This ISPF message set can be downloaded from
*              https://www.shareabitofit.net/ISRZ00-in-mvs38j/
*
*
         EJECT
*  Prerequisite: User Modifications and Others
*  ===================================================================
*
*     SHRABIT.MACLIB is NECESSARY to assemble this software.  This PDS
*  contains macros and copybooks.  Download from this website:
*       https://ShareABitofIT.net/SHRABIT-MACLIB-in-MVS38J
*
*     **** NOTE: SHRABIT.MACLIB is part of the installation process.
*                You can visit the SHRABIT.MACLIB page and download
*                at a later time!
*
*     ISPF generic message ISRZ002, part of message member ISRZ00, is
*  used for error message processing and posting by this software.
*  If ISRZ00 is NOT INSTALLED on your system, error posting results in
*  a message not found condition with no error context posted.
*  ISRZ00 can be downloaded from this website:
*       https://ShareABitofIT.net/ISRZ00-in-MVS38J
*
*     **** NOTE: ISRZ00 is part of the installation process.  You can
*                visit the ISRZ00 page and download at a later time!
*
*     This software displays date and time on line 1 using ISPF
*  Z-variables ZDATE, ZJDATE and ZTIME or subroutine GETDTE (if
*  installed on your system).
*
*     GETDTE, is OPTIONAL, and can be downloaded from website:
*       https://ShareABitofIT.net/GETDTE-in-MVS38J
*
*     If GETDTE is installed, two user-mods, ZP60014 and ZP60038 are
*  REQUIRED to process CLIST symbolic variables via the IKJCT441 API.
*  Otherwise, GETDTE will function normally, but will NOT create
*  CLIST variable, RDTEANSR.
*
*     More information on the above user-mods can be obtained from
*  the following website:
*       http://www.prycroft6.com.au/vs2mods/
*
*     Check your system to determine if one or both user-mods are
*  required.  ZP60038 requires ZP60014.
*
*
*
         EJECT
*  DALCDS Programs / Services:
*  ==================================================================
*
*    1) GETMAIN/FREEMAIN          Working storage
*    2) ISPLINK                   ISPLINK module for ISPF Services
*    3) SHRABIT.MACLIB            Macros for ShareABitofIT Programs
*    4) GETDTE                    Get Date-Time-Environment Utility
*
*
         EJECT
*  Table 1 - Program Register Usage
*
*  +------+----------------------------------------------------------+
*  | REG  |                                                          |
*  |USAGE |  DESCRIPTION                                             |
*  +------+----------------------------------------------------------+
*  |  R13 |  Calling program registers (R14-R12) upon entry          |
*  |  R14 |  Calling program return address upon entry               |
*  |  R15 |  Called  program entry address  upon entry               |
*  |  R1  |  Called  program parms starting address                  |
*  +------+----------------------------------------------------------+
*  |  R0  |  Working Register                                        |
*  |  R1  |  Working Register                                        |
*  |  R2  |  Working Register                                        |
*  |  R3  |  Working Register                                        |
*  |  R4  |  Working Register                                        |
*  |  R5  |  Working Register                                        |
*  |  R6  |  Working Register                                        |
*  |  R7  |  Working Register                                        |
*  |  R8  |  Working Register                                        |
*  |  R9  |  Address of WORKAREA via GETMAIN/FREEMAIN                |
*  |  R10 |  Base Register 1                                         |
*  |  R11 |  Base Register 2                                         |
*  |  R12 |  Base Register 3                                         |
*  |  R13 |  Address of SAVEAREA                                     |
*  +------+----------------------------------------------------------+
*  |  R15 |  Return Code upon exit                                   |
*  +------+----------------------------------------------------------+
*
         EJECT
*  Table 2 - Return Codes
*
*  +------+----------------------------------------------------------+
*  |RETURN|                                                          |
*  | CODE |  DESCRIPTION                                             |
*  +------+----------------------------------------------------------+
*  |  00  |  Successful                                              |
*  +------+----------------------------------------------------------+
*  | 4096 |  ISPLINK not found, link error                           |
*  +------+----------------------------------------------------------+
*  | 4099 |  Program requires TSO                                    |
*  +------+----------------------------------------------------------+
*
         EJECT
*  Macros and SYSLIB Location:
*  ==================================================================
*
*  Macro     Description                         Library
*  --------  ----------------------------------  ------------------
*  YREGS     Register Equates                    SYS2.MACLIB
*  IHAPSA    Prefixed Save Area                  SYS1.AMODGEN
*  CVT       Communication Vector Table          SYS1.AMODGEN
*  IKJTCB    Task Control Block                  SYS1.AMODGEN
*  IEFTIOT1  Task IO Table                       SYS1.AMODGEN
*  IEFJFCBN  Job File Control Block              SYS1.AMODGEN
*  IHAASCB   Address Space Control Block         SYS1.AMODGEN
*
*  ISPFPL    ISPF Parm List for LBISPL           SHRABIT.MACLIB
*  ISPFSRV   ISPF Services and Keywords          SHRABIT.MACLIB
*  LBISPL    CALL equivalent for ISPLINK         SHRABIT.MACLIB
*  MOVEC     Move quoted string data             SHRABIT.MACLIB
*  MOVEI     Initialize variable                 SHRABIT.MACLIB
*  MOVER     Move string right-trim              SHRABIT.MACLIB
*  MOVEV     Move variable                       SHRABIT.MACLIB
*  RTRIM     Right-trim subroutine               SHRABIT.MACLIB
*  RDTECOMA  GETDTE Commarea                     SHRABIT.MACLIB
*
*  References:
*  ==================================================================
*
*  - GC28-6628-9 OS Rel 21.7 System Control Blocks
*
*
         EJECT
*  Change History:
*  ==================================================================
*
*  MM/DD/CCYY Version  Name / Description
*  ---------- -------  --------------------------------------------
*
*  08/10/2020 1.0.00   Larry Belmontes Jr.
*                      - Initial version released to MVS 3.8J
*                        hobbyist public domain
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
DALCDS   CSECT
*****************************************************************/
*                          *    Software Option Flags           */
         LCLB  &USEDTE     * Use GETDTE flag                    */
&USEDTE  SETB  0           *   0=No,   1=Yes                    */
*                          *                                    */
*****************************************************************/
         USING DALCDS,R10,R11,R12  my BASE REGISTER(S)
         PRINT NOGEN
*
*     * /********************************************************/
*     * /* Save regs and declare base registers R10,R11,R12     */
*     * /* including saving R1 (PARMS) in R2                    */
*     * /* - R1  myPARMS address on entry                       */
*     * /* - R15 myENTRY address on entry                       */
*     * /********************************************************/
         STM   R14,R12,12(R13)     Save calling program registers
         LR    R10,R15             Load base1 w R15
         LA    R11,4095(R10)       Load base2 (R11) w/ offset
         LA    R11,1(R11)          Adjust base2
         LA    R12,4095(R11)       Load base3 (R12) w/ offset
         LA    R12,1(R12)          Adjust base3
         LR    R15,R13             Save callers registers
         LR    R2,R1               Save PARM addr
*
*     * /********************************************************/
*     * /* Program Eyecatcher                                   */
*     * /********************************************************/
         B     OVRSTAMP            Branch past eye-catcher
MYSTAMP  EQU   *
PGMID    DC    CL8'DALCDS  '       My Program STAMP
         DC    CL8'MVS3.8J '       OS
PGMVRM   DC    CL8'V1.0.00 '       .Version
         DC    CL8'08102020'       ..date  MMDDCCYY
         DC    CL1' '
PGMDS    DC    CL8'&SYSDATE'       Assemble Date
         DC    CL3' - '            .and
PGMTS    DC    CL8'&SYSTIME'       ..Time
         DC    C'Copyright (C) 2020'
         DC    C'Larry Belmontes, Jr. '
         DC    C'https://ShareABitofIT.net/DALCDS-in-MVS38J'
MYSTAMPL EQU   *-MYSTAMP
OVRSTAMP DS    0H
*
         EJECT
*     * /********************************************************/
*     * /* GETMAIN, working storage, using R9                   */
*     * /* - R0  Working register                               */
*     * /* - R1  Working register (GETMAIN address)             */
*     * /* - R9  Working Storage DSECT address                  */
*     * /********************************************************/
         LA    R0,WSAREAL          R0 = size of Workarea DSECT
         GETMAIN   R,LV=(R0)       Get Storage, R1 = addr of storage
         LTR   R15,R15             Did we get the storage?
         BNZ   NOMAINS             NO, return with rc in R15
         LR    R9,R1               YES. R9 = Workarea DSECT Register
         USING WSDSECT,R9          Tell Assembler
*
*     * /********************************************************/
*     * /* Update Save Areas for caller and calling programs    */
*     * /* - R1  myPARMS address on entry  (restored)           */
*     * /********************************************************/
         LR    R15,R13             Save callers savearea addr
         LA    R13,SAVEAREA        Load Addr of my savearea
         ST    R15,4(,R13)         Chain callers to my savearea
         ST    R13,8(,R15)         Chain my savearea to callers
         LR    R1,R2               Restore R1 as PARM addr
*
         ST    R2,MYCPPLP          Store CPPL Address
*
*     * /********************************************************/
*     * /* ISPLINK Entry Point                                  */
*     * /* - R0 Working Register                                */
*     * /********************************************************/
         XC    ISPLINK,ISPLINK     Clear ISPLINK Address
         LOAD  EP=ISPLINK,ERRET=ERR4096
         ST    R0,ISPLINK          Save ISPLINK Entry Point
*
*     * /********************************************************/
*     * /* GETDTE  Entry Point                                  */
*     * /* - R0 Working Register                                */
*     * /********************************************************/
         XC    GETDTE,GETDTE       Clear GETDTE  Address
         AIF  (NOT &USEDTE).NODTE
         LOAD  EP=GETDTE,ERRET=GETDTENO
         ST    R0,GETDTE           Save GETDTE  Entry Point
.NODTE   ANOP
GETDTENO EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Check if we are running under TSO FG                 */
*     * /* - R3 Working Register                                */
*     * /********************************************************/
         MVI   AMITSO,C'Y'         Assume TSO FG
         L     R3,PSAAOLD-PSA(0)   R3=ASCB Address
         L     R3,ASCBTSB-ASCB(R3) R3=ASCBTSB Address
         LTR   R3,R3               If 0, program under TSO BG
         BZ    ERR4099             ..error, TSO BG!!
*
*     * /********************************************************/
*     * /* Initialize                                           */
*     * /********************************************************/
         MVC   RCEXIT,=F'0'        Zero Return Code R15
*
         BAL   R14,ISPCNTL         CONTROL ERRORS RETURN
         BAL   R14,ISPVDEF         VDEFINE ISPF Variables
*
         MOVEI DJMSG,ONLY          Blank DJMSG
         MOVEI PMYMSG,ONLY         Blank PMYMSG
         MVC   PMYRC,=C'00000'     Zero  PMYRC
         MOVEI PMYCMD,ONLY         Blank PMYCMD
         ZAP   ROWN,=P'0'          Zero Row Number counter
*
         EJECT
*     * /********************************************************/
*     * /* Get PARAMETER information passed to program          */
*     * /* - R1 myPARMS address on entry                        */
*     * /* - R4 Starting parms address                          */
*     * /* - R6 Parm Length                                     */
*     * /* - R3, R4, R5 Working Register                        */
*     * /********************************************************/
         MOVEI DDSRCH,ONLY         Blank DDSRCH
         L     R1,MYCPPLP          R1=Parm Address at start up
PARMS    EQU   *
         LTR   R1,R1               Do we have a PARM?
         BZ    TBLSTRT             NO, start process
         LR    R3,R1               YES, R3=PARM/CPPL addr
         TM    0(R3),X'80'         Is it PARM or CPPL addr?
         BZ    CPPLIN              YES, CPPL addr
PARMIN   EQU   *                   NO,  must be PARM addr
         L     R4,0(,R3)           R4=Addr of PARM
         LH    R6,0(,R4)           R6=Length of PARM
         LTR   R6,R6               PARM > 0 length?
         BZ    TBLSTRT             NO, start process
         LA    R4,2(,R4)           YES, R4=point to start of PARM data
         B     PARMSXT              and continue...
CPPLIN   EQU   *
         ST    R3,MYCPPLP          Store CPPL Address
         L     R4,0(,R3)           R4=Command Buffer addr
         LH    R5,2(,R4)           R5=Offset to Command Variables
         LTR   R5,R5               Any Variables?
         BZ    TBLSTRT             NO, start process
         LH    R6,0(,R4)           YES, R6=Length of Command Buffer
         SR    R6,R5               Subtract variable offset
         SH    R6,=H'4'            Subtract prefix
         BE    TBLSTRT             NO, start process
         BM    TBLSTRT             NO, start process
         LA    R4,4(R4,R5)         R4=Point to variables start addr
PARMSXT  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Uppercase translation of PARMIN                      */
*     * /* - R4 Starting parms address                          */
*     * /* - R6 Parm Length                                     */
*     * /* - R8, R4, R5 Working Register                        */
*     * /********************************************************/
CAP#EM   EQU   *
         LR    R8,R6               R8=Parm Length
         BCTR  R8,0                Adjust length for OC
         EX    R8,UPPERC           Execute Uppercase translation
         B     UPPERCX
UPPERC   OC    0(0,R4),=256X'40'   EBCDIC Lower-Upper Case Translate
UPPERCX  EQU   *
*
*     * /********************************************************/
*     * /* GET DD from parm                                     */
*     * /********************************************************/
PARMSCAN EQU   *
         LR    R8,R6               R8=Parm Length
         C     R8,=F'8'            ParmLength > 8
         BNH   MOVERDO             No, move data
         LA    R8,8                Yes, Force 8 bytes
MOVERDO  EQU   *
         BCTR  R8,0                Adjust length for OC
         EX    R8,MOVERC           Execute Uppercase translation
         B     MOVERCX
MOVERC   MVC   DDSRCH(0),0(R4)     Move parm data to DDSRCH byte1-8
MOVERCX  EQU   *
*
*     * /********************************************************/
*     * /* Create TABLE                                         */
*     * /* - ISPEXEC TBCREATE TBLNAME                           */
*     * /*           NAMES                                      */
*     * /*           NOWRITE REPLACE                            */
*     * /********************************************************/
TBLSTRT  EQU   *
*        ISPLINK TBCREATE,tblname,keylist,namelist,nowrite,replace
        LBISPL ISPLINK,(TBCREATE,TBLNAME,B,TBLCOLS1,NOWRITE,REPLACE),VL
         ST    R15,RCEXIT          Store R15
         BAL   R14,ISPERR          Err Display, return here or MYEXIT
*
         EJECT
*     * /********************************************************/
*     * /* Scan TIOT for allocated DDNAMEs                      */
*     * /* - For each DDNAME, get DSN from JFCB                 */
*     * /* - R1, R2, R3, R4, R5, R6, R7, R8 Working Register    */
*     * /********************************************************/
         L     R2,CVTPTR           Address of CVT
         USING CVT,R2              Tell Assembler, CVT
         L     R2,CVTTCBP          Addrs (2 FW) pointing to TCB
         L     R2,4(R2)            Take 2nd FW as my TCB Addr
         USING TCB,R2              Tell Assembler, TCB
         L     R2,TCBTIO           Load TIOT Address
         USING TIOT,R2             Tell Assembler, TIOT
         MVC   PJOBNM,0(R2)        UserID
         MVC   PSTEPNM,8(R2)       Job Step
         MVC   PPROCNM,16(R2)      Proc Name
         LA    R1,TIOENTRY-TIOT1   Position past TIOT header
LOOPDDN  EQU   *
         AR    R2,R1               Point to TIOENTRY #1
         USING TIOENTRY,R2         Tell Assembler, TIOENTRY
         CLI   TIOELNGH,0          End of TIOT?
         BE    TIOT#EOD            Yes, done...
         TM    TIOESTTA,TIOSLTYP   Entry in use?
         BO    TIONEXT             Yes, get another
*
         CLI   DDSRCH,C' '         Specific DD request?
         BE    DDSTART             No, display DD info
         CLC   TIOEDDNM,DDSRCH     Yes. Is this the DD?
         BE    DDSTART               Yes, display DD info
         CP    ROWN,=P'0'            No. Already displayed DD info?
         BE    TIONEXT                 No, get next TIOT entry
         CLI   TIOEDDNM,C' '           Yes. Concatenation for DD?
         BNE   TIOT#EOD                  No, done...
*
*     * /********************************************************/
*     * /* Dataset Name              (from TIOT)                */
*     * /********************************************************/
DDSTART  EQU   *
         MVC   PDDNAME,TIOEDDNM    DDNAME
*
*     * /********************************************************/
*     * /* Row Number represents DD item count                  */
*     * /********************************************************/
         MOVEI PROWNUM,ONLY        Blank ROWNUM
         CLI   PDDNAME,C' '        DD Blank?
         BE    BYPSCNT             Yes, bypass count DD
CNTIT$   EQU   *                   No, count DD
         AP    ROWN,=P'1'          Add 1 to row counter
         UNPK  PROWNUM,ROWN        Unpack row counter
         OI    PROWNUM+2,X'F0'     Force F-zone
BYPSCNT  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Point to JFCB for DD     (TIOT -> JFCB)              */
*     * /********************************************************/
         SR    R5,R5               Clear R8
         ICM   R5,B'0111',TIOEJFCB Load JFCB Addr
         LA    R5,16(,R5)          Bump past header prefix
         USING JFCB,R5             Tell Assembler, JFCB
*
*     * /********************************************************/
*     * /* Volume Serial            (from JFCB)                 */
*     * /********************************************************/
         MVC   PVOLSER,JFCBVOLS    VOLSER
*
*     * /********************************************************/
*     * /* DSN, Member name         (from JFCB)                 */
*     * /********************************************************/
         TM    TIOELINK,TIOTTERM   TERMINAL allocation?
         BO    TERM#               Yes, branch to TERM
DSN#     EQU   *                   No, move DSN(mem)
         MOVEI PDSNAME             Move DSN
         MOVER JFCBDSNM,44         Set length due to DSECT label
         CLI   JFCBELNM,C' '       Member Name?
         BE    DSN#X               No, done...
         MOVEC '('                 Yes, move member name
         MOVER JFCBELNM,8          set length due to DSECT label
         MOVEC '('
         B     DSN#X
TERM#    EQU   *
         MOVEI PDSNAME             Move 'Terminal'
         MOVEC 'Terminal'
         B     DSN#X
DSN#X    EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Dataset Organization Type (from JFCB)                */
*     * /********************************************************/
         MVC   PDSORG,=C'   '      Blank DSO
         CLI   PDDNAME,C' '        DDNAME Blank?
         BE    DORG1X              Yes, display DSORG as blank
*                                  No, determine DSORG
         TM    JFCDSRG2,JFCORGAM         08  .... 1...
         BO    DORG15
         TM    JFCDSRG1,JFCORGIS         80  1... ....
         BO    DORG12
         TM    JFCDSRG1,JFCORGPS         40  .1.. ....
         BO    DORG14
         TM    JFCDSRG1,JFCORGDA         20  ..1. ....
         BO    DORG13
         TM    JFCDSRG1,JFCORGPO         02  .... ..1.
         BO    DORG11
         MVC   PDSORG,=C'???'            Unknown DSO
         B     DORG1X
*
DORG11   EQU   *
         MVC   PDSORG,=C'PO '
         B     DORG1X
DORG12   EQU   *
         MVC   PDSORG,=C'IS '
         B     DORG1X
DORG13   EQU   *
         MVC   PDSORG,=C'DA '
         B     DORG1X
DORG14   EQU   *
         MVC   PDSORG,=C'PS '
         B     DORG1X
DORG15   EQU   *
         MVC   PDSORG,=C'VS '
         B     DORG1X
DORG1X   EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Disposition Type (from JFCB)                         */
*     * /********************************************************/
         TM    JFCBIND2,JFCNEW           c0   11.. ....
         BO    DISP11
         TM    JFCBIND2,JFCMOD           80   1... ....
         BO    DISP12
         TM    JFCBIND2,JFCOLD+JFCSHARE  48   .1.. 1...
         BO    DISP14
         TM    JFCBIND2,JFCOLD           40   .1.. ....
         BO    DISP13
         TM    JFCBIND2,JFCSHARE         08   .... 1...
         BO    DISP14
         TM    JFCBIND2,JFCTEMP          01   .... ...1
         BO    DISP15
DISP1#   EQU   *
         MVC   PDISP1,=C'???'
         B     DISP1X
DISP11   EQU   *
         MVC   PDISP1,=C'New'
         B     DISP1X
DISP12   EQU   *
         MVC   PDISP1,=C'Mod'
         B     DISP1X
DISP13   EQU   *
         MVC   PDISP1,=C'Old'
         B     DISP1X
DISP14   EQU   *
         MVC   PDISP1,=C'Shr'
         B     DISP1X
DISP15   EQU   *
         MVC   PDISP1,=C'Tmp'
         B     DISP1X
DISP1X   EQU   *
*
         EJECT
ADDEM    EQU   *
*     * /********************************************************/
*     * /* Add TABLE row                                        */
*     * /* - ISPEXEC TBADD    TBLNAME                           */
*     * /* - R1, R14, R15 Working Register                      */
*     * /********************************************************/
         LBISPL ISPLINK,(TBADD,TBLNAME),VL
         ST    R15,RCEXIT          Store R15
         BAL   R14,ISPERR          Err Display, return here or MYEXIT
*
*     * /********************************************************/
*     * /* Point to next TIO Entry                              */
*     * /********************************************************/
TIONEXT  EQU   *
         XR    R1,R1               Clear R1
         IC    R1,TIOELNGH         Load TIO Entry Length
         B     LOOPDDN             Try next
*
         EJECT
*     * /********************************************************/
*     * /* End of TIOT                                          */
*     * /********************************************************/
TIOT#EOD EQU   *                   TIOT End of Data
         MVC   PANTBL,PANALCDS     Panel name
         MVC   PANCSR,ZCMD         Panel Cursor
         MOVEI PANMSG,ONLY         Blank message number
*
         CLI   DDSRCH,C' '         Specific DD request?
         BE    SET@TOP             No, continue
         CP    ROWN,=P'0'          Yes. Found specific DD?
         BNE   SET@TOP               Yes, continue
*                                    No, display no DD found
         ZAP   ROWN,=P'0'          Zero Row Number counter
         MOVEI PROWNUM,ONLY        Blank ROWNUM
         MOVEI PDDNAME             Move DDSRCH value
         MOVEV DDSRCH
         MOVEI PDSORG,ONLY         Blank DSORG
         MOVEI PVOLSER,ONLY        Blank VOLSER
         MOVEI PDSNAME             Move DD not found message
         MOVEC '** DD not found'
         MOVEI PDISP1,ONLY         Blank DISP1
         MOVEI DDSRCH,ONLY         Blank DDSRCH
         LBISPL ISPLINK,(TBADD,TBLNAME),VL
         ST    R15,RCEXIT          Store R15
         BAL   R14,ISPERR          Err Display, return here or MYEXIT
*
*     * /********************************************************/
*     * /* Position to TOP of TABLE                             */
*     * /* - ISPEXEC TBTOP    TBLNAME                           */
*     * /********************************************************/
SET@TOP  EQU   *
         LBISPL ISPLINK,(TBTOP,TBLNAME),VL
         ST    R15,RCEXIT          Store R15
         BAL   R14,ISPERR          Err Display, return here or MYEXIT
*
         EJECT
*     * /********************************************************/
*     * /* Display TABLE                                        */
*     * /* - ISPEXEC TBDISPL  TBLNAME  PANEL(PANTBL)            */
*     * /* -         AUTOSEL(NO) CURSOR(ZCMD) CSRROW(CSSR)      */
*     * /********************************************************/
*     * /* - ISPLINK TBDISPL,tblname,panel,msgid,csrfldnm,tblrow#,
*     * /* -         csrpos,YES/NO,crpname,rowidname,msgfldname)
*     * /********************************************************/
TBDSP    EQU   *
*     * /********************************************************/
*     * /* Contruct date and time for SCRNCLK variable          */
*     * /* ---------------------------------------------------- */
*     * /* If GETDTE subroutine is available,                   */
*     * /*     source date and time from GETDTE                 */
*     * /* Else                                                 */
*     * /*     source date and time from ISPF Z variables       */
*     * /********************************************************/
         L     R15,GETDTE          R15=GETDTE Entry
         LTR   R15,R15             Do we have an address?
         BZ    DT@ZVARS            No, use Z-vars
         LA    R1,$DTECOMS         R1 - Addr of Results
         ST    R1,DTPARM1               ...as PARM1
         OI    DTPARM1,X'80'       VL Mark
         LA    R1,DTPARML          R1 - Addr of DT PARM LIST
         BALR  R14,R15             Get date/time frm GETDTE

         LR    R3,R15              Load R3 w R15
         CVD   R3,DW               Convert to decimal
         UNPK  DJMSG+08(5),DW+5(3) Unpack
         OI    DJMSG+12,X'F0'      Force an F Zone

         STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT   LB1010e
         TPUT  DJMSG,15                                                 LB1010e
         LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT   LB1010e

*        LTR   R15,R15             GETDTE RC=0 ?
*        BNZ   DT@ZVARS            No, use Z vars
         MVC   PSCRNCLK,RDATE      dd/mm/ccyy.jjj hh:mm:ss
         B     TBDSP1
DT@ZVARS EQU   *
         MOVEI PSCRNCLK
         MOVER PZDATE              dd/mm/yy
         MOVEC '.'
         MOVEV PZJDATE+3,3         20.jjj
         MOVEC ' '
         MOVER PZTIME              hh:mm
TBDSP1   EQU   *
        LBISPL ISPLINK,(TBDISPL,TBLNAME,PANTBL,PANMSG,PANCSR),VL
**      LBISPL ISPLINK,(TBDISPL,TBLNAME,PANTBL,PANMSG,PANCSR,B,B,NO),VL
***     LBISPL ISPLINK,(TBDISPL,TBLNAME,PANTBL,B,B,B,B,NO),VL
         ST    R15,RCEXIT          Store R15
         C     R15,=F'8'           RC=8 (PF3/PF4, END/RETURN)
         BE    TBDSPX              Yes, Exit
         BAL   R14,ISPERR          Err Display, return here or MYEXIT
*
         EJECT
*     * /********************************************************/
*     * /* Process COMMAND from TBDISPL                         */
*     * /********************************************************/
*     * /* ZCMD          Action                                 */
*     * /* ------------  ---------------------------------      */
*     * /* Blanks        Do nothing                             */
*     * /* /xxxxxxxx     Search for DD xxxxxxxx                 */
*     * /* Other         Invalid Command                        */
*     * /********************************************************/
DO#CMD   EQU   *
         CLI   PZCMD,C' '          Blank ZCMD?
         BE    TBDSP               Yes, redisplay table
         CLI   PZCMD,C'/'          DD search?
         BE    DDCMD               Yes, search for DD
         MVC   PANMSG,=C'ISRZ002 ' Use generic ISPF msg ISRZ002
         MOVEI PZERRSM,ONLY        Blank ZERRSM
         MOVEI PZERRLM             Blank ZERRLM
         MOVEC 'Invalid Command '
         MOVER PZCMD
         MVC   PANCSR,=C'ZCMD    ' Panel Cursor
         B     TBDSP               Redisplay
*
*     * /********************************************************/
*     * /* Close (TBEND) current TABLE                          */
*     * /* - ISPEXEC TBEND    TBLNAME                           */
*     * /********************************************************/
DDCMD    EQU   *
         ZAP   ROWN,=P'0'          Zero Row Number counter
         MVC DDSRCH,PZCMD+1        Move 8 bytes after / as DD name
         LBISPL ISPLINK,(TBEND,TBLNAME),VL
         ST    R15,RCEXIT          Store R15
         BAL   R14,ISPERR          Err Display, return here or MYEXIT
         B     TBLSTRT             Look for requested DD
*
TBDSPX   EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* End TABLE Processing                                 */
*     * /* - ISPEXEC TBEND    TBLNAME                           */
*     * /********************************************************/
*     * /* - ISPLINK TBEND,tblname
*     * /* -         ?,crpname,rowidname,msgfldname)
*     * /********************************************************/
         LBISPL ISPLINK,(TBEND,TBLNAME),VL
         ST    R15,RCEXIT          Store R15
         BAL   R14,ISPERR          Err Display, return here or MYEXIT
*
*     * /********************************************************/
*     * /* Delete Functional Variables                          */
*     * /* - ISPEXEC VDELETE *                                  */
*     * /********************************************************/
*     * /* - ISPLINK VDELETE,namelist
*     * /********************************************************/
MYEXIT   EQU   *
         LBISPL ISPLINK,(VDELETE,ALL),VL
         DELETE EP=ISPLINK
         DELETE EP=GETDTE
*
         EJECT
*     * /********************************************************/
*     * /* - Restore savearea, FREEMAIN, and                    */
*     * /*   return to caller                                   */
*     * /********************************************************/
*
*     * /--------------------------------------------------------/
*     * /* R1 = my Working Storage Address from GETMAIN         */
*     * /********************************************************/
         LR    R1,R13              Working Storage Addr
*
*     * /--------------------------------------------------------/
*     * /* Get callers savearea into R13 and overlay my R15     */
*     * /* to be returned to caller                             */
*     * /--------------------------------------------------------/
         L     R13,SAVEAREA+4      Restore R13
         L     R5,RCEXIT           Set R15 to my exit value
*
*     * /--------------------------------------------------------/
*     * /* Working Storage FREEMAIN                             */
*     * /--------------------------------------------------------/
         LA    R0,WSAREAL          Size of WSAREA DSECT
         FREEMAIN  R,LV=(R0),A=(R1)    Free Storage
*
*     * /--------------------------------------------------------/
*     * /* Restore caller registers and return to caller        */
*     * /--------------------------------------------------------/
         LR    R15,R5              R15 = RC for exit
NOMAINS  EQU   *
         L     R14,12(R13)         Restore R14
         LM    R0,R12,20(R13)      Restore R0 thru R12
         BR    R14                 Return to caller
*
         TITLE 'DALCDS - Error Entry Points                           '
*     * /********************************************************/
*     * /* Error Setting                                        */
*     * /* - R7  Working Register                               */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
ERR4096  EQU   *
         MVC   RCEXIT,=F'4096'     ISPLINK  not found, link error
         MVC   DJMSG+15(MSG4096L),MSG4096
         B     ERR#GO
ERR4099  EQU   *
         MVC   RCEXIT,=F'4099'     Program requires TSO
         MVC   DJMSG+15(MSG4099L),MSG4099
         B     ERR#GO
ERR#GO   EQU   *
         BAL   R7,PREFXMSG         Prefix Message
         TPUT  DJMSG,L'DJMSG
         B     MYEXIT
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine -                                  (R14)  */
*     * /*  ISPEXEC CONTROL ERRORS RETURN                       */
*     * /* - R15, R1    Working Register                        */
*     * /********************************************************/
ISPCNTL  EQU   *
         ST    R14,SAVER14S
*        CALL  ISPLINK,(CONTROL,ERRORS,RETURN),VL
         LBISPL ISPLINK,(CONTROL,ERRORS,RETURN),VL
*
         L     R14,SAVER14S
         BR    R14
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine -                                  (R14)  */
*     * /*  ISPEXEC VDEFINE function variables                  */
*     * /* - R15, R1    Working Register                        */
*     * /********************************************************/
ISPVDEF  EQU   *
         ST    R14,SAVER14S
*        CALL  ISPLINK,(VDEFINE,KEYPRESS,PKYPRS,CHAR,L4),VL
         LBISPL ISPLINK,(VDEFINE,DPGMVRM,PGMVRM,CHAR,LPGMVRM),VL
         LBISPL ISPLINK,(VDEFINE,MYMSG,PMYMSG,CHAR,LMYMSG),VL
         LBISPL ISPLINK,(VDEFINE,MYRC,PMYRC,CHAR,LMYRC),VL
         LBISPL ISPLINK,(VDEFINE,MYCMD,PMYCMD,CHAR,LMYCMD),VL
*
         LBISPL ISPLINK,(VDEFINE,KEYPRESS,PKYPRS,CHAR,LKYPRS),VL
         LBISPL ISPLINK,(VDEFINE,RESP0,PRESP0,CHAR,LRESP0),VL
         LBISPL ISPLINK,(VDEFINE,ZCMD,PZCMD,CHAR,LZCMD),VL
*
         LBISPL ISPLINK,(VDEFINE,SCRNCLK,PSCRNCLK,CHAR,LSCRNCLK),VL
         LBISPL ISPLINK,(VDEFINE,ZDATE,PZDATE,CHAR,LZDATE),VL
         LBISPL ISPLINK,(VDEFINE,ZJDATE,PZJDATE,CHAR,LZJDATE),VL
         LBISPL ISPLINK,(VDEFINE,ZTIME,PZTIME,CHAR,LZTIME),VL
*
         LBISPL ISPLINK,(VDEFINE,SEL,TSEL,CHAR,LSEL),VL
*
         LBISPL ISPLINK,(VDEFINE,ZTDSELS,PZTDSELS,CHAR,LZTDSELS),VL
         LBISPL ISPLINK,(VDEFINE,ZTDTOP,PZTDTOP,CHAR,LZTDTOP),VL
         LBISPL ISPLINK,(VDEFINE,ZTDROWS,PZTDROWS,CHAR,LZTDROWS),VL
*
         LBISPL ISPLINK,(VDEFINE,ZERRSM,PZERRSM,CHAR,LZERRSM),VL
         LBISPL ISPLINK,(VDEFINE,ZERRLM,PZERRLM,CHAR,LZERRLM),VL
*
         LBISPL ISPLINK,(VDEFINE,JOBNM,PJOBNM,CHAR,LJOBNM),VL
         LBISPL ISPLINK,(VDEFINE,STEPNM,PSTEPNM,CHAR,LSTEPNM),VL
         LBISPL ISPLINK,(VDEFINE,PROCNM,PPROCNM,CHAR,LPROCNM),VL
*
         LBISPL ISPLINK,(VDEFINE,ROWNUM,PROWNUM,CHAR,LPROWNUM),VL
         LBISPL ISPLINK,(VDEFINE,DDNAME,PDDNAME,CHAR,LDDNAME),VL
         LBISPL ISPLINK,(VDEFINE,DSORG,PDSORG,CHAR,LDSORG),VL
         LBISPL ISPLINK,(VDEFINE,VOLSER,PVOLSER,CHAR,LVOLSER),VL
         LBISPL ISPLINK,(VDEFINE,DSNAME,PDSNAME,CHAR,LPDSNAME),VL
         LBISPL ISPLINK,(VDEFINE,DISP1,PDISP1,CHAR,LPDISP1),VL
*
         L     R14,SAVER14S
         BR    R14
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine -                                  (R14)  */
*     * /*  ISPF Error Display and return                       */
*     * /*   to caller or MYEXIT                                */
*     * /* - RCEXIT     on entry                                */
*     * /* - R15, R1    Working Register                        */
*     * /********************************************************/
ISPERR   EQU   *
         ST    R14,SAVER14S
         L     R15,RCEXIT
         LTR   R15,R15             RC=0 (Process)
         BZ    ISPERRX1            Yes, return to caller
*                                  No, display error and exit
         MVC   DJMSG(L'DSPLERR0),DSPLERR0
         L     R1,ISPFP1           ISPF Service Requested
         MVC   DJMSG+03(08),0(R1)
         MVC   DJMSG+11(06),=C'      '
         CVD   R15,DW              Convert to decimal
         UNPK  DJMSG+20(05),DW+5(03) Unpack and
         OI    DJMSG+24,X'F0'      ... force and F zone
         MVI   DJMSG+20,C'='       Restore equal sign
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
         TPUT  DJMSG,L'DSPLERR0
         LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
*
         LA    R14,MYEXIT          R14=Addr MYEXIT
         B     ISPERRX2            Return to MYEXIT
*
ISPERRX1 EQU   *
         L     R14,SAVER14S
ISPERRX2 EQU   *
         BR    R14
*
         EJECT
         RTRIM                     RTRIM Subroutine
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Prefix message                   (R7)   */
*     * /* w/ 15-byte header 'pgmid    nnnn -'                  */
*     * /* - R3, R7 Working Register                            */
*     * /********************************************************/
PREFXMSG EQU   *
         ST    R7,SAVER7S          Save R7 Subroutine
         MVC   DJMSG+00(8),PGMID
         L     R3,RCEXIT           Load RCEXIT code value
         CVD   R3,DW               Convert to decimal
         UNPK  DJMSG+08(5),DW+5(3) Unpack
         OI    DJMSG+12,X'F0'      Force an F Zone
         MVI   DJMSG+08,C' '       Blank out digit
         MVI   DJMSG+14,C'-'       Blank out digit
*
         L     R7,SAVER7S          Restore R7 Subroutine
         BR    R7                  Return to caller
*
         TITLE 'DALCDS - Literal Pool                                 '
         LTORG
*
         TITLE 'DALCDS - Constants                                    '
         ISPFSRV                   ISPF Service Constants
         EJECT
*     * /********************************************************/
*     * /* ISPF Constants for services                          */
*     * /********************************************************/
PANALCDS DC    CL8'PDALCDS0'              Panel ID  TSO Allocations
*
         EJECT
*     * /********************************************************/
*     * /* ISPF Variable Names                                  */
*     * /********************************************************/
DPGMVRM  DC    CL8'DPGMVRM '       VarName for PGMVRM
MYMSG    DC    CL8'MYMSG '         VarName for PMYMSG
MYRC     DC    CL8'MYRC    '       VarName for PMYRC
MYCMD    DC    CL8'MYCMD '         VarName for PMYCMD
*
KEYPRESS DC    CL8'KEYPRESS'       Varname for PKYRS
RESP0    DC    CL8'RESP0   '       Varname for PRESP0
ZCMD     DC    CL8'ZCMD    '       VarName for PZCMD
SCRNCLK  DC    CL8'SCRNCLK '       VarName for PSCRNCLK
ZDATE    DC    CL8'ZDATE   '       VarName for PZDATE
ZJDATE   DC    CL8'ZJDATE  '       VarName for PZJDATE
ZTIME    DC    CL8'ZTIME   '       VarName for PZTIME
*
SEL      DC    CL8'SEL     '       VarName for TSEL
*
ZTDSELS  DC    CL8'ZTDSELS '       VarName for PZTDSELS
ZTDTOP   DC    CL8'ZTDTOP  '       VarName for PZTDTOP
ZTDROWS  DC    CL8'ZTDROWS '       VarName for PTDROWS
*
ZERRSM   DC    CL8'ZERRSM  '       VarName for ZERRSM
ZERRLM   DC    CL8'ZERRLM  '       VarName for ZERRLM
*
JOBNM    DC    CL8'JOBNM   '       VarName for PJOBNM
STEPNM   DC    CL8'STEPNM  '       VarName for PSTEPNM
PROCNM   DC    CL8'PROCNM  '       VarName for PROCNM
*
ROWNUM   DC    CL8'ROWNUM  '       VarName for PROWNUM
DDNAME   DC    CL8'DDNAME  '       VarName for PDDNAME
DSORG    DC    CL8'DSORG   '       VarName for PDSORG
VOLSER   DC    CL8'VOLSER  '       VarName for PVOLSER
DSNAME   DC    CL8'DSNAME  '       VarName for PDSNAME
DISP1    DC    CL8'DISP1   '       VarName for PDISP1
*
         EJECT
*     * /********************************************************/
*     * /* ISPF Variable Names Lengths                          */
*     * /********************************************************/
         DS    0F
LPGMVRM  DC    A(L'PGMVRM)         PGMVRM
LMYMSG   DC    A(L'PMYMSG)         PMYMSG
LMYRC    DC    A(L'PMYRC)          PMYRC
LMYCMD   DC    A(L'PMYCMD)         PMYCMD
*
LKYPRS   DC    A(L'PKYPRS)         PKYRS
LRESP0   DC    A(L'PRESP0)         PRESP0
LZCMD    DC    A(L'PZCMD)          PZCMD
LSCRNCLK DC    A(L'PSCRNCLK)       PSCRNCLK
LZDATE   DC    A(L'PZDATE)         PZDATE
LZJDATE  DC    A(L'PZJDATE)        PZJDATE
LZTIME   DC    A(L'PZTIME)         PZTIME
*
LSEL     DC    A(L'TSEL)           SEL    Selection
*
LZTDSELS DC    A(L'PZTDSELS)       PZTDSELS
LZTDTOP  DC    A(L'PZTDTOP)        PZTDTOP
LZTDROWS DC    A(L'PZTDROWS)       PZTDROWS
*
LZERRSM  DC    A(L'PZERRSM)        PZERRSM
LZERRLM  DC    A(L'PZERRLM)        PZERRLM
*
LJOBNM   DC    A(L'PJOBNM)         PJOBNM
LSTEPNM  DC    A(L'PSTEPNM)        PSTEPNM
LPROCNM  DC    A(L'PPROCNM)        PROCNM
*
LPROWNUM DC    A(L'PROWNUM)        PROWNUM
LDDNAME  DC    A(L'PDDNAME)        PDDNAME
LDSORG   DC    A(L'PDSORG)         PDSORG
LVOLSER  DC    A(L'PVOLSER)        VOLSER VOLUME
LPDSNAME DC    A(L'PDSNAME)        PDSNAME
LPDISP1  DC    A(L'PDISP1)         PDISP1
*
         EJECT
*     * /********************************************************/
*     * /* ISPF Tables Constants                                */
*     * /********************************************************/
TBLNAME  DC    CL8'ALCDS0'         Table name
*                                  Table column names
TBLCOLS1 DC    C'(ROWNUM DDNAME DSORG VOLSER DSNAME'
         DC    C' DISP1)'
*
         EJECT
*     * /********************************************************/
*     * /* Local Messages                                       */
*     * /********************************************************/
MSG4096  DC    C'ISPLINK  not found, link error'
MSG4096L EQU   *-MSG4096
MSG4099  DC    C'Program requires TSO'
MSG4099L EQU   *-MSG4099
*
DSPLERR0 DC    C'** xxxxxxxxxxxxxx RC=xxxx, Process Ended!'
*
         TITLE 'DALCDS - Equates                                      '
*     * /********************************************************/
*     * /* E Q U A T E S                                        */
*     * /********************************************************/
*     *
*     *
*     * /********************************************************/
*     * /* Register Equates                                     */
*     * /********************************************************/
         YREGS                     Register EQU as Rn or Rnn
*
*R0       EQU   0
*R1       EQU   0
*R2       EQU   0
*R3       EQU   0
*R4       EQU   0
*R5       EQU   0
*R6       EQU   0
*R7       EQU   0
*R8       EQU   0
*R9       EQU   0
*R10      EQU   0
*R11      EQU   0
*R12      EQU   0
*R13      EQU   0
*R14      EQU   0
*R15      EQU   0
*
         TITLE 'DALCDS - System DSECTs                                '
*     *
*     * /********************************************************/
*     * /* System DSECTs                                        */
*     * /********************************************************/
         IHAPSA                    Prefixed Save Area
PSALEN   EQU   *-PSA               Length of PSA
         EJECT
         CVT   DSECT=YES,LIST=YES  Communication Vector Table
         EJECT
         IKJTCB DSECT=YES,LIST=YES Task Control Block
         EJECT
TIOT     DSECT
         IEFTIOT1                  Task IO Table
TIOTLEN  EQU   *-TIOT              Length of TIOT
         EJECT
JFCB     DSECT
         IEFJFCBN  LIST=YES        Job File Control BLock
JFCBLEN  EQU   *-JFCB              Length of JFCB
         EJECT
         IHAASCB                   Address Space Control Block
ASCBLEN  EQU   *-ASCB              Length of ASCB
*
         TITLE 'DALCDS - Working Storage Variables                    '
*     * /********************************************************/
*     * /* Working Storage  DSECT                               */
*     * /********************************************************/
WSDSECT  DSECT
WSAREA   EQU   *
*     *
*     * /********************************************************/
*     * /* mySAVEARA                                            */
*     * /********************************************************/
SAVEAREA DS    18F                 my Savearea
*                      +00  A(savearea of current CSECT)
*                      +04  A(savearea of calling CSECT)
*                      +08  A(savearea of called  CSECT)
*                      +12  R14
*                      +16  R15
*                      +20  R0
*                      +24  R1
*                      +28  R2
*                      +32  R3
*                      +36  R4
*                      +40  R5
*                      +44  R6
*                      +48  R7
*                      +52  R8
*                      +56  R9
*                      +60  R10
*                      +64  R11
*                      +68  R12
*
         EJECT
         ISPFPL                    ISPF Parm List for LBISPL macro
         EJECT
*     * /********************************************************/
*     * /* ISPF Variables                                       */
*     * /********************************************************/
PMYMSG   DS    CL50                MYMSG
PMYRC    DS    CL5                 MYRC
PMYCMD   DS    CL100               MYCMD
*
PKYPRS   DS    CL4                 KEYPRESS
PRESP0   DS    CL8                 RESP0
PZCMD    DS    CL50                ZCMD
PSCRNCLK DS    CL23                PSCRNCLK
PZDATE   DS    CL8                 ZDATE   DD/MM/YY
PZJDATE  DS    CL6                 ZJDATE  YY.JJJ
PZTIME   DS    CL5                 ZTIME   HH:MM
*
TSEL     DS    CL3                 SEL    Selection
*
PZTDSELS DS    CL4                 ZTDSELS
PZTDTOP  DS    CL6                 ZTDTOP
PZTDROWS DS    CL6                 ZTDROWS
*
PZERRSM  DS    CL24                ZERRSM
PZERRLM  DS    CL78                ZERRLM
*
PJOBNM   DS    CL8                 JOBNM
PSTEPNM  DS    CL8                 STEPNM
PPROCNM  DS    CL8                 ROCNM
*
PROWNUM  DS    CL3                 ROWNUM
PDDNAME  DS    CL8                 DDNAME
PDSORG   DS    CL3                 DSORG
PVOLSER  DS    CL6                 VOLSER VOLUME
PDSNAME  DS    CL54                DSNAME
PDISP1   DS    CL3                 DISP1
*
         EJECT
*     * /********************************************************/
*     * /* Program Variables                                    */
*     * /********************************************************/
SAVER3S  DS    F                   R3  Save in Subroutines
SAVER7S  DS    F                   R7  Save in Subroutines
SAVER14S DS    F                   R14 Save in Subroutines
DW       DS    D                   Double Word area
FW       DS    F                   Full Word   area
RCEXIT   DS    F                   Exit RC
AMITSO   DS    C                   TSO Flag
MYCPPLP  DS    F                   CPPL Address
DJMSG    DS    CL100               MY MESSAGE VARIABLE
PANTBL   DS    CL8                 Table Panel Name
PANMSG   DS    CL8                 Panel Message Number
PANCSR   DS    CL8                 Panel Cursor Field
GETDTE   DS    F                   Entry point for GETDTE
DTPARML  DS    0F                  GETDTE Parameter List
DTPARM1  DS    A                   Address of Results Area
ROWN     DS    PL2                 DD Row Counter
DDSRCH   DS    CL8                 Search DD
*
         EJECT
         RDTECOMA                  GETDTE Commarea
         EJECT
*
         DS    0F,(40)X
WSAREAE  EQU   *                   Working Storage END
WSAREAL  EQU   WSAREAE-WSAREA      Working Storage Area Length
         EJECT
         END   DALCDS
@@
//LKED.SYSLMOD DD  DISP=SHR,DSN=SYSGEN.ISPF.LLIB
//*
//LKED.SYSIN DD *
 ALIAS ISRDDN
 NAME DALCDS(R)
/*
//PLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.PLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=PDALCDS0
/********************************************************************/
/*                                                                  */
/*    PANEL: PDALCDS0                                               */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/DALCDS-in-MVS38J               */
/*         Copyright (C) 2020  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x PDALCDS0 panel for DALCDS Dialogue Manager              */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ---------------------------------------------------------------  */
/* MM/DD/CCYY VERSION  NAME / DESCRIPTION                           */
/* 08/10/2020 1.0.00   Larry Belmontes Jr.                          */
/*                     Initial version released to MVS 3.8J         */
/*                     Hobbyist Public Domain                       */
/*                                                                  */
/********************************************************************/
)ATTR DEFAULT(%@_)
%   TYPE(TEXT)   INTENS(HIGH)
@   TYPE(TEXT)   INTENS(LOW)
?   TYPE(TEXT)   INTENS(LOW)       HILITE(REVERSE) COLOR(BLUE)
_   TYPE(INPUT)  INTENS(HIGH) CAPS(ON)  JUST(LEFT)
!   TYPE(OUTPUT) INTENS(LOW)  CAPS(OFF) JUST(ASIS)
$   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(RED)
~   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(BLUE)
[   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(TURQ)
[   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(YELLOW)
{   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(GREEN)
}   TYPE(TEXT)   INTENS(HIGH)                      COLOR(YELLOW)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
{Z                      %-----? TSO Allocations  %-/-/--------------------------
%COMMAND ===>_Z                                                %SCROLL ===>_AMT
%                                                                      ~Z
%UserID-{JOBNM   %Step-[STEPNM  %Proc-[PROCNM  %                       ~Z
%
%DD# DDNAME   ORG DISP VOLSER DSNAME
)MODEL
[Z  [Z       $Z  [Z   {Z     [Z                                               %
)INIT
  .HELP = HDALCDS0
  .ZVARS = '(SCRNCLK ZCMD ZUSER ZPANELID
              ROWNUM DDNAME DSORG DISP1 VOLSER DSNAME)'
  &AMT = 'CSR'
  &ZTDMARK = '******************************* BOTTOM OF DATA *+
*******************+
*************'
)PROC
)END
./ ADD NAME=HDALCDS0
/********************************************************************/
/*                                                                  */
/*    PANEL: HDALCDS0                                               */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/DALCDS-in-MVS38J               */
/*         Copyright (C) 2020  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x HDALCDS0 panel for DALCDS Dialogue Manager              */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ---------------------------------------------------------------  */
/* MM/DD/CCYY VERSION  NAME / DESCRIPTION                           */
/* 08/10/2020 1.0.00   Larry Belmontes Jr.                          */
/*                     Initial version released to MVS 3.8J         */
/*                     Hobbyist Public Domain                       */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ~ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 [ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(TURQ)
 ? TYPE(TEXT)   INTENS(LOW)  HILITE(REVERSE) COLOR(BLUE)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
)BODY EXPAND(//)
%--HELP-----------------------? TSO Allocations  %-/-/-~v1.0.00%-
%Command ===>_ZCMD                                    / /   +&ZPANELID
%
+DALCDS displays current TSO DD allocation information in a table format as
+follows:
+
+?| Column   |  Description                                                 |+
+?|%DD#     ?|[ DD Sequence Number for display purposes                    ?|+
+?|%DDNAME  ?|[ DD Name, may include multiple dataset names                ?|+
+?|%ORG     ?|[ File Organization (shown once per concatenation)           ?|+
+?|%DISP    ?|[ Dataset disposition                                        ?|+
+?|%VOLSER  ?|[ Dataset Volser Number                                      ?|+
+?|%DSNAME  ?|[ Dataset Name, can include member name for PDS              ?|+
+?---------------------------------------------------------------------------+
+
+The display can scroll backward or forward using PF7 and PF8, respectively.
+
+Use the command, %/ddname+to search and display a specific DD allocation.
+Use the command, %/      +to display all DD allocations.
+
~Note:+ORG and/or DISP may not exhibit a valid value if the dataset has not
+       been used at time of display.
+
+
)INIT
 .CURSOR = ZCMD

)PROC

)END
@@
