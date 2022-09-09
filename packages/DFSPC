//DFSPC JOB (JOB),
//             'INSTALL DFSPC',
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
//* *  DFSPC for MVS3.8J TSO / Hercules                    *
//* *                                                      *
//* *  JOB: $INST04                                        *
//* *       Install DFSPC Program                          *
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
//* -------------------------------------------------------*
//* *                                                      *
//* *  Assemble Link-edit DFSPC                            *
//* *                                                      *
//* -------------------------------------------------------*
//DFSPC    EXEC  ASML,MBR=DFSPC
//ASM.SYSIN DD DATA,DLM=@@
         TITLE 'DFSPC - ISPF Display Freespace Information            '
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  ==================================================================
*
* DDDDD    FFFFFFF   SSSSS   PPPPPP    CCCC
* DD   D   FF       SS    S  PP   PP  CC  CC
* DD   DD  FF       SS       PP   PP  CC
* DD   DD  FFFFF     SSSSS   PPPPPP   CC
* DD   DD  FF            SS  PP       CC
* DD   D   FF       S    SS  PP       CC  CC
* DDDDD    FF        SSSS    PP        CCCC
*
*  ==================================================================
*
*
*  Program: DFSPC
*
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/DFSPC-in-MVS38J
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
*     DFSPC displays volume freespace and attributes for each
*  online DASD device via ISPF using table services.  DFSPC uses
*  SVC 78 as offered in MVS 3.8J and volume attributes are gathered
*  from DSCB-F4. The code base does not support indexed VTOCs as
*  not offered in MVS 3.8J.
*
*     DFSPC display snippet follows:
*
*   ------------------------------------------------------------------
*   | dd/mm/yy.jjj hh:mm      ------  DASD Freespace  ------------...|  Line 1
*   | COMMAND ===>                                                ...|       2
*   |                                                             ...|       3
*   |                                                             ...|       4
*   |                       -----FREE-----  -LARGEST-    VTOC  Ava...|       5
*   | CUU  VOLSER  DEVTYPE  Cyls Trks Exts  Cyls Trks    Trks  DSC...|       6
*   | 343  CBTCAT  3350     0193 0005 0002  0193 0000   00029  012...|       7
*   | 340  CBT000  3350     0000 0007 0001  0000 0007   00029  012...|       8
*   | 341  CBT001  3350     0013 0006 0002  0013 0000   00029  010...|       .
*   | 342  CBT002  3350     0036 0011 0001  0036 0011   00029  010...|       .
*   ------------------------------------------------------------------
*
*     Data can be sorted by typing 'SORT ColNn' into the COMMAND line.
*  ColNm must be a minimum of 3 characters.  For descending order,
*  preceed the ColNm with a minus sign. i.e. SORT -CUU
*
*     The method used to acquire DASD freespace is via a UCB scan for
*  non-VIO Online DASD devices.
*
*     Date and time values are sourced from ISPF Z variables or
*  subroutine GETDTE, if found in a link library.  If GETDTE is not
*  installed on your system or elect not to do so, no problem!
*  See Prerequisite section below for more information.
*
*     DFSPC uses ISPF generic message ISRZ002 for posting errors.
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
*     DFSPC can be invoked directly from an ISPF panel or menu
*  using 'CMD(DFSPC) NEWAPPL(DFSP)' as follows:
*
*     For example -
*
*       Assume an existing ISPF panel has the following )PROC section,
*       the 'NEW ENTRY' line can be added to invoke DSFPC when
*       option F is entered on the panel (menu) command line.
*
*         )PROC
*           &ZSEL = TRANS( TRUNC (&ZCMD,'.')
*                         1,'CMD(xxxxx) NEWAPPL(ISR)'
*                         6,'PGM(xxxxxx)'
*                         F,'CMD(DFSPC) NEWAPPL(DFSP)'   <-- NEW ENTRY
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
*          ____ DFSPC     5  SELECT CMD(DFSPC) NEWAPPL(DFSP)
*                               DISPLAY DASD FREESPACE
*
*     Then, you can access DFSPC from any screen by typing DFSPC
*  on the command line.
*
*     DFSPC is based on Wally Mclaughlin's ISPF product Ver 2.1 and
*  written in IFOX00 under MVS38J TK3 / Hercules.
*
*     Other ISPF resource required include:
*            - PDFSPC0   DASD Freespace Display Panel
*            - HDFSPC0   DASD Freespace Help    Panel
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
*  DFSPC Programs / Services:
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
*  IHAASCB   Address Space Control Block         SYS1.AMODGEN
*  IEFUCBOB  Unit Control Block                  SYS1.AMODGEN
*
*  DVCTBL    DASD Device Table                   SHRABIT.MACLIB
*  ISPFPL    ISPF Parm List for LBISPL           SHRABIT.MACLIB
*  ISPFSRV   ISPF Services and Keywords          SHRABIT.MACLIB
*  LBISPL    CALL equivalent for ISPLINK         SHRABIT.MACLIB
*  MOVEC     Move quoted string data             SHRABIT.MACLIB
*  MOVEI     Initialize variable                 SHRABIT.MACLIB
*  MOVER     Move string right-trim              SHRABIT.MACLIB
*  MOVEV     Move variable                       SHRABIT.MACLIB
*  RTRIM     Right-trim subroutine               SHRABIT.MACLIB
*  SVC78A    Freespace (SVC 78) Commarea         SHRABIT.MACLIB
*  RDTECOMA  GETDTE Commarea                     SHRABIT.MACLIB
*
*  References:
*  ==================================================================
*
*  - GC28-0708-1 OS/VS2 Debugging Handbook Vol.1 Rel 3.7 Nov78
*  - SY26-3919-1 MVS/370 DADSM Diagnosis Reference Oct1983
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
         TITLE 'DFSPC - ISPF Display Freespace Information            '
DFSPC    CSECT
*****************************************************************/
*                          *    Software Option Flags           */
         LCLB  &USEDTE     * Use GETDTE flag                    */
&USEDTE  SETB  0           *   0=No,   1=Yes                    */
*                          *                                    */
*****************************************************************/
         USING DFSPC,R10,R11,R12   my BASE REGISTER(S)
         PRINT NOGEN
*     *
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
PGMID    DC    CL8'DFSPC   '       My Program STAMP
         DC    CL8'MVS3.8J '       OS
PGMVRM   DC    CL8'V1.0.00 '       .Version
         DC    CL8'08102020'       ..date  MMDDCCYY
         DC    CL1' '
PGMDS    DC    CL8'&SYSDATE'       Assemble Date
         DC    CL3' - '            .and
PGMTS    DC    CL8'&SYSTIME'       ..Time
         DC    C'Copyright (C) 2020'
         DC    C'Larry Belmontes, Jr. '
         DC    C'https://ShareABitofIT.net/DFSPC-in-MVS38J'
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
         EJECT
*     * /********************************************************/
*     * /* Initialize                                           */
*     * /********************************************************/
         MVC   RCEXIT,=F'0'        Zero Return Code R15
*
         BAL   R14,ISPCNTL         CONTROL ERRORS RETURN
         BAL   R14,ISPVDEF         VDEFINE ISPF Variables
        LBISPL ISPLINK,(VGET,VGETV1,SHARED),VL   Get ZENVIR var
*
         MOVEI DJMSG,ONLY          Blank DJMSG
         MOVEI PMYMSG,ONLY         Blank PMYMSG
         MVC   PMYRC,=C'00000'     Zero  PMYRC
         MOVEI PMYCMD,ONLY         Blank PMYCMD
*
*     * /********************************************************/
*     * /* Create TABLE                                         */
*     * /* - ISPEXEC TBCREATE TBLNAME                           */
*     * /*           NAMES                                      */
*     * /*           NOWRITE REPLACE                            */
*     * /********************************************************/
TBLSTRT  EQU   *
*        ISPLINK TBCREATE,tblname,keylist,namelist,nowrite,replace
        LBISPL ISPLINK,(TBCREATE,TBLNAME,B,TBLNMS01,NOWRITE,REPLACE),VL
         ST    R15,RCEXIT          Store R15
         BAL   R14,ISPERR          Err Display, return here or MYEXIT
*
         EJECT
*     * /********************************************************/
*     * /* Find all non-VIO ONLINE DASD devices via UCB         */
*     * /* - R2, R3, R4         Working Registers               */
*     * /********************************************************/
         L     R4,CVTPTR           Address of CVT
         USING CVT,R4
         L     R2,CVTILK2          Address of UCB Lookup table
         USING UCB,R3
         DROP  R4                  Drop CVT addressability...
UCBSCAN  EQU   *
         SR    R3,R3               Clear R3
         LH    R3,0(R2)            Point to UCB
         LA    R2,2(R2)            Bump to next UCB entry
         ST    R2,FW               Use UCB addr as
         MVC   PUCB,FW               original sort sequence field
         CH    R3,=X'FFFF'         End of UCB Table?
         BE    UCBEOD              YES. Done.
         LTR   R3,R3               NULL?
         BZ    UCBSCAN             YES, get another
         TM    UCBJBNR,UCBVRDEV    VIO Device?
         BO    UCBSCAN             NO, re-scan
         TM    UCBSTAT,UCBONLI     Device ONLINE?
         BNO   UCBSCAN             NO, re-scan
         TM    UCBTBYT3,UCB3DACC   Direct Access?
         BZ    UCBSCAN             NO
*
*     * /********************************************************/
*     * /* Find device type (e.g. 3350) given unit type         */
*     * /* using below table (DVCLST)                           */
*     * /* - R4, R5             Working Registers               */
*     * /********************************************************/
GETDEVTY EQU   *                   YES, determine DASD type
         LA    R5,DVCLST$E         Load # of DVCLST entries
         LA    R4,DVCLST$          Address of DVCLST entries
LOOK4DT  EQU   *
         CLC   UCBUNTYP,0(R4)      UCB device type in table?
         BE    DEVFND              YES, found device in table
         LA    R4,DVCLST$L(R4)     NO, point to next device entry
         BCT   R5,LOOK4DT          Look at next entry
NODEVFND EQU   *
         MVC   PDUNIT,DVCLST$+1    MOVE DEVTTYPE, not found
         B     DEVTPXT
DEVFND   EQU   *
         MVC   PDUNIT,1(R4)        MOVE DEVTTYPE
         B     DEVTPXT
*
DEVTPXT  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Get Freespace for specific volume using SVC 78       */
*     * /* - R0, R1, R3         Working Registers               */
*     * /********************************************************/
         MVC   TVOLSER,UCBVOLI     Volume
         MVC   PDCUU,UCBNAME       CUU
         LR    R0,R3               R0=UCB Address
         LA    R1,FSPMSG           R1=SVC 78 Mesage return area
         SVC   78                  Invoke SVC 78
*
*     * /********************************************************/
*     * /* Get Format-4 DSCB for VOLUME device characterics     */
*     * /* - R4 Working Register                                */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
DSCBF4   EQU   *
         MVI   CAMDSN,X'04'        Set to F4 DSCB
         MVC   CAMDSN+1(L'CAMDSN-1),CAMDSN
*
         LA    R4,0                ZERO R3
         ST    R4,CAMLIST           ... FW1 of CAMLIST
         MVI   CAMLIST,X'C1'       INDICATE NAME   OPTION
         LA    R4,CAMDSN           Addr of DSN
         ST    R4,CAMLIST+4         ... FW2 of CAMLIST
         LA    R4,TVOLSER          Addr of VOL SER
         ST    R4,CAMLIST+8         ... FW3 of CAMLIST
         LA    R4,IECSDSF4         Addr of FORMAT 4 DSCB
         ST    R4,CAMLIST+12        ... FW4 of CAMPLIST
*
         OBTAIN  CAMLIST           Get F4 DSCB (Volume Info)
         LTR   R15,R15             Successful?
         BZ    DSCBF4X             YES.
*                                  NO.
         MOVEI PCYL4VOL,ONLY
         MOVEI PTRK4VOL,ONLY
         MOVEI PTRKSCYL,ONLY
         MOVEI PNADSCB,ONLY
         MOVEI PVTOCSZ,ONLY
*
         B     ADDEM
DSCBF4X  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Cylinders for Volume              from format-4 DSCB */
*     * /* - R4 Working Register                                */
*     * /********************************************************/
         LH    R4,DS4DEVSZ         Cylinders for volume
         ST    R4,HCYL4VOL         Store DS4DEVSZ
         CVD   R4,DW               Convert to decimal
         UNPK  PCYL4VOL(5),DW+5(3) Unpack and
         OI    PCYL4VOL+4,X'F0'     ... force and F zone
*
*     * /********************************************************/
*     * /* Tracks per Cylinder for Volume    from format-4 DSCB */
*     * /* - R4 Working Register                                */
*     * /********************************************************/
         LH    R4,DS4DEVSZ+2       Tracks/Cylinder
         ST    R4,HTRKSCYL         Store DS4DEVSZ+2
         CVD   R4,DW               Convert to decimal
         UNPK  PTRKSCYL(5),DW+5(3) Unpack and
         OI    PTRKSCYL+4,X'F0'     ... force and F zone
*
*     * /********************************************************/
*     * /* Tracks for volume =                                  */
*     * /*   Cylinders for Volume * Tracks per Cylinder         */
*     * /* - R4, R5 Working Registers                           */
*     * /********************************************************/
         L     R5,HCYL4VOL         R3=CYLS for volume
         M     R4,HTRKSCYL         (R2,R3)=CYL4VOL*TRKSCYL
*              R5                  tracks for volume
         CVD   R5,DW               Convert to decimal
         UNPK  PTRK4VOL(5),DW+5(3) Unpack and
         OI    PTRK4VOL+4,X'F0'     ... force and F zone
*
*     * /********************************************************/
*     * /* # of available DSCBs              from format-4 DSCB */
*     * /* - R4 Working Register                                */
*     * /********************************************************/
         LH    R4,DS4DSREC         # avail DSCBs
         CVD   R4,DW               Convert to decimal
         UNPK  PNADSCB(5),DW+5(3)  Unpack and
         OI    PNADSCB+4,X'F0'      ... force and F zone
*
         EJECT
*     * /********************************************************/
*     * /* VTOC Descriptor (tracks)          from format-4 DSCB */
*     * /*    xxCCRRCCRR  10 bytes                              */
*     * /* - R1, R4, R5 Working Register                        */
*     * /********************************************************/
         LA    R1,DS4VTOCE
         SLR   R5,R5               Clear R3
         ICM   R5,B'0011',2(R1)    R3=BeginCC
         M     R4,HTRKSCYL         R2/R3=Begin Relative Track at CC
         AH    R5,4(,R1)           Add BeginHH
         ST    R5,FW               Begin Relative Track
*
         SLR   R5,R5               Clear R3
         ICM   R5,B'0011',6(R1)    R3=EndCC
         M     R4,HTRKSCYL         R2/R3=End Relative Track at CC
         AH    R5,8(,R1)           Add EndHH
         LA    R5,1(R5)            Adj +1
*
         S     R5,FW               Number of VTOC tracks
         ST    R5,FW               FW=Number of VTOC tracks
*
         CVD   R5,DW               Convert to decimal
         UNPK  PVTOCSZ(5),DW+5(3)  Unpack and
         OI    PVTOCSZ+4,X'F0'      ... force and F zone
*
ADDEM    EQU   *
*     * /********************************************************/
*     * /* Add TABLE row                                        */
*     * /* - ISPEXEC TBADD    TBLNAME                           */
*     * /* - R1, R14, R15 Working Register                      */
*     * /********************************************************/
         LBISPL ISPLINK,(TBADD,TBLNAME),VL
         ST    R15,RCEXIT          Store R15
         BAL   R14,ISPERR          Err Display, return here or MYEXIT
         B     UCBSCAN             Get next UCB!
*
UCBEOD   EQU   *                   UCB EOD
         DROP  R3                  Drop UCB Addressability...
*
         EJECT
*     * /********************************************************/
*     * /* Sort TABLE by VOLSER                                 */
*     * /* - ISPEXEC TBSORT   TBLNAME                           */
*     * /* -         FIELDS(COL01,C,A)                          */
*     * /********************************************************/
         MVC   PANTBL,PANDFSPC     Panel name
         MVC   PANCSR,ZCMD         Panel Cursor
         MOVEI PANMSG              Blank message number
         MOVEI TBLSRT01            Default sort, by volume
         MOVEC '(VOLSER,C,A)'
SORTEM   EQU   *
         LBISPL ISPLINK,(TBSORT,TBLNAME,TBLSRT01),VL
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
*     * /* Get current date and time for SCRNCLK variable       */
*     * /* and save RDTEANSR var to SHARED pool                 */
*     * /********************************************************/
         L     R15,GETDTE          R15=GETDTE Entry
         LTR   R15,R15             Do we have an address?
         BZ    DT@ZVARS            No, use Z-vars
         LA    R1,$DTECOMS         R1 - Addr of Results
         ST    R1,DTPARM1               ...as PARM1
         OI    DTPARM1,X'80'       VL Mark
         LA    R1,DTPARML          R1 - Addr of DT PARM LIST
         BALR  R14,R15             Get date-time-env from GETDTE
         MVC   PSCRNCLK,RDATE      dd/mm/ccyy.jjj hh:mm:ss
        LBISPL ISPLINK,(VPUT,VPUTV1,SHARED),VL    Save RDTEANSR var
         B     TBDSP1
DT@ZVARS EQU   *
         MVI   $DTECOMS,C' '       Blank RDTEANSR data area
         MVC   $DTECOMS+1($DTECOML-1),$DTECOMS
         MOVEI PSCRNCLK
         MOVER PZDATE              dd/mm/yy
         MOVEC '.'
         MOVEV PZJDATE+3,3         20.jjj
         MOVEC ' '
         MOVER PZTIME              hh:mm
         MVC   $DTECOMS(L'PSCRNCLK),PSCRNCLK
        LBISPL ISPLINK,(VPUT,VPUTV1,SHARED),VL    Save RDTEANSR var
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
*     * /* SORT -colnm   Sort display by column name            */
*     * /* Other         Invalid Command                        */
*     * /********************************************************/
DO#CMD   EQU   *
         CLI   PZCMD,C' '          Blank ZCMD?
         BE    TBDSP               Yes, redisplay table
         CLC   =C'SORT ',PZCMD     Sort Command?
         BE    SORTIT              Yes, sort display
         MVC   PANMSG,=C'ISRZ002 ' Use generic ISPF msg ISRZ002
         MOVEI PZERRSM,ONLY        Blank ZERRSM
         MOVEI PZERRLM             Blank ZERRLM
         MOVEC 'Invalid Command: '
         MOVER PZCMD
         MVC   PANCSR,=C'ZCMD    ' Panel Cursor
         B     TBDSP               Redisplay

*     * /********************************************************/
*     * /* Check for valid SORT column request                  */
*     * /*   Sort column can be prefixed with - for DSC order   */
*     * /*   if supported by current ISPF version               */
*     * /*   (from Wally Mclaughlin)                            */
*     * /********************************************************/
SORTIT   EQU   *
         LA    R14,PZCMD+5         R14=ZCMD Compare Start Addr
*                                  Adjust for 'SORT -'
         CLI   0(R14),C'-'         DSC sort request?
         BNE   CHKZCMD             NO,  check ZCMD
         LA    R14,1(R14)          YES, adj ZCMD compare start addr
CHKZCMD  EQU   *
         CLC   =C'CUU',0(R14)      Sort by CUU
*        BE    SCUU                -- yields incorrect order due to HEX
         BE    SUCB                -- use UCB Binary column
         CLC   =C'VOL',0(R14)      Sort by VOLser
         BE    SVOL
         CLC   =C'DEV',0(R14)      Sort by DEVtype
         BE    SDEV
         CLC   =C'FCY',0(R14)      Sort by FCYls
         BE    SFCYL
         CLC   =C'FTR',0(R14)      Sort by FTRks
         BE    SFTRK
         CLC   =C'FEX',0(R14)      Sort by FEXts
         BE    SFEXT
         CLC   =C'LCY',0(R14)      Sort by LCYls
         BE    SLCYL
         CLC   =C'LTR',0(R14)      Sort by LTRks
         BE    SLTRK
         CLC   =C'VTO',0(R14)      Sort by VTOctrks
         BE    SVTOCSZ
         CLC   =C'DSC',0(R14)      Sort by DSCbs
         BE    SDSCBS
         CLC   =C'VCY',0(R14)      Sort by VCYls
         BE    SVCYL
         CLC   =C'VTR',0(R14)      Sort by VTRks
         BE    SVTRK
         MVC   PANMSG,=C'ISRZ002 ' Use generic ISPF msg ISRZ002
         MOVEI PZERRSM,ONLY        Blank ZERRSM
         MOVEI PZERRLM             Blank ZERRLM
         MOVEC 'Invalid Sort Column: '
         MOVER PZCMD
         MVC   PANCSR,=C'ZCMD    ' Panel Cursor
         B     TBDSP               Redisplay
*
         EJECT
*     * /********************************************************/
*     * /* Init workarea to ISPF variable name for SORT         */
*     * /********************************************************/
SUCB     EQU   *
         MVC   WORKAREA,UCB##
         B     CR8SCMD
SCUU     EQU   *
         MVC   WORKAREA,DCUU
         B     CR8SCMD
SVOL     EQU   *
         MVC   WORKAREA,VOLSER
         B     CR8SCMD
SDEV     EQU   *
         MVC   WORKAREA,DUNIT
         B     CR8SCMD
SFCYL    EQU   *
         MVC   WORKAREA,DFSCYLS
         B     CR8SCMD
SFTRK    EQU   *
         MVC   WORKAREA,DFSTRKS
         B     CR8SCMD
SFEXT    EQU   *
         MVC   WORKAREA,DFSEXTS
         B     CR8SCMD
SLCYL    EQU   *
         MVC   WORKAREA,DFSLCYLS
         B     CR8SCMD
SLTRK    EQU   *
         MVC   WORKAREA,DFSLTRKS
         B     CR8SCMD
SVTOCSZ  EQU   *
         MVC   WORKAREA,VTOCSZ
         B     CR8SCMD
SDSCBS   EQU   *
         MVC   WORKAREA,NADSCB
         B     CR8SCMD
SVCYL    EQU   *
         MVC   WORKAREA,CYL4VOL
         B     CR8SCMD
SVTRK    EQU   *
         MVC   WORKAREA,TRK4VOL
         B     CR8SCMD
*
         EJECT
*     * /********************************************************/
*     * /* Init ISPF SORT command                               */
*     * /*   e.g.  (DFSLTRKS,C,A)                               */
*     * /********************************************************/
CR8SCMD  EQU   *
         MOVEI TBLSRT01            Build TBSORT parm
         MOVEC '('
         MOVER WORKAREA
         MOVEC ',C,'
         CLC   =C'SORT -',PZCMD      DSC request?
         BE    CR8SDSC               YES.
CR8SASC  EQU   *                     NO.
         MOVEC 'A)'
         B     CR8SXIT
CR8SDSC  EQU   *
         CLC   PZENVIR+5(3),=C'2.1'  Above release 'ISPF 2.1'?
         BNH   CR8SASC               No, Force ASC sort order
         MOVEC 'D)'
CR8SXIT  EQU   *
*
         CLC   PZENVIR+5(3),=C'2.1'  Above release 'ISPF 2.1'?
         BNH   MSG1X                 No, set error 1X
         B     MSG1S                 Yes, set error 1S
         EJECT
MSG1X    EQU   *
         CLC   =C'SORT -',PZCMD      DSC request?
         BNE   MSG1S
         MVC   PANMSG,=C'ISRZ002 ' Use generic ISPF msg ISRZ002
         MOVEI PZERRSM,ONLY        Blank ZERRSM
         MOVEI PZERRLM             Blank ZERRLM
         MOVEC 'Sorted table ASC for '
         MOVER PZCMD+6,20
         MOVEC ' (ISPF '
         MOVEV PZENVIR+5,3
         MOVEC ' does not support DSC)'
         B     MSG1SET
MSG1S    EQU   *
         MVC   PANMSG,=C'ISRZ002 ' Use generic ISPF msg ISRZ002
         MOVEI PZERRSM,ONLY        Blank ZERRSM
         MOVEI PZERRLM             Blank ZERRLM
         MOVEC 'Sorted table via '
         MOVER PZCMD
MSG1SET  EQU   *
         MVC   PANCSR,=C'ZCMD    ' Panel Cursor
*
         B     SORTEM              Branch to SORT
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
         TITLE 'DFSPC - Error Entry Points                            '
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
         LBISPL ISPLINK,(VDEFINE,RDTEANSR,$DTECOMS,CHAR,LDTEANSR),VL
         LBISPL ISPLINK,(VDEFINE,ZDATE,PZDATE,CHAR,LZDATE),VL
         LBISPL ISPLINK,(VDEFINE,ZJDATE,PZJDATE,CHAR,LZJDATE),VL
         LBISPL ISPLINK,(VDEFINE,ZTIME,PZTIME,CHAR,LZTIME),VL
         LBISPL ISPLINK,(VDEFINE,ZENVIR,PZENVIR,CHAR,LZENVIR),VL
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
         LBISPL ISPLINK,(VDEFINE,UCB##,PUCB,CHAR,LPUCB),VL
         LBISPL ISPLINK,(VDEFINE,DCUU,PDCUU,CHAR,LDCUU),VL
         LBISPL ISPLINK,(VDEFINE,VOLSER,TVOLSER,CHAR,LVOLSER),VL
         LBISPL ISPLINK,(VDEFINE,DUNIT,PDUNIT,CHAR,LDUNIT),VL
         LBISPL ISPLINK,(VDEFINE,DFSCYLS,FSCYLS,CHAR,LFSCYLS),VL
         LBISPL ISPLINK,(VDEFINE,DFSTRKS,FSTRKS,CHAR,LFSTRKS),VL
         LBISPL ISPLINK,(VDEFINE,DFSEXTS,FSEXTS,CHAR,LFSEXTS),VL
         LBISPL ISPLINK,(VDEFINE,DFSLCYLS,FSLCYLS,CHAR,LFSLCYLS),VL
         LBISPL ISPLINK,(VDEFINE,DFSLTRKS,FSLTRKS,CHAR,LFSLTRKS),VL
         LBISPL ISPLINK,(VDEFINE,VTOCSZ,PVTOCSZ,CHAR,LVTOCSZ),VL
         LBISPL ISPLINK,(VDEFINE,NADSCB,PNADSCB,CHAR,LNADSCB),VL
         LBISPL ISPLINK,(VDEFINE,CYL4VOL,PCYL4VOL,CHAR,LCYL4VOL),VL
         LBISPL ISPLINK,(VDEFINE,TRK4VOL,PTRK4VOL,CHAR,LTRK4VOL),VL
         LBISPL ISPLINK,(VDEFINE,TRKSCYL,PTRKSCYL,CHAR,LTRKSCYL),VL
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
         TITLE 'DFSPC - Literal Pool                                  '
         LTORG
*
         TITLE 'DFSPC - Constants                                     '
         ISPFSRV                   ISPF Service Constants
         EJECT
*     * /********************************************************/
*     * /* ISPF Constants for services                          */
*     * /********************************************************/
PANDFSPC DC    CL8'PDFSPC0 '               Panel ID  Freespace
VGETV1   DC    C'(ZENVIR)'                 VGET List 1
VPUTV1   DC    C'(RDTEANSR)'               VPUT List 1
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
RDTEANSR DC    CL8'RDTEANSR'       VarName for $DTECOMS
ZDATE    DC    CL8'ZDATE   '       VarName for PZDATE
ZJDATE   DC    CL8'ZJDATE  '       VarName for PZJDATE
ZTIME    DC    CL8'ZTIME   '       VarName for PZTIME
ZENVIR   DC    CL8'ZENVIR  '       VarName for PZENVIR
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
UCB##    DC    CL8'UCB     '       VarName for PUCB
DCUU     DC    CL8'DCUU    '       VarName for PDCUU
VOLSER   DC    CL8'VOLSER  '       VarName for TVOLSER
DUNIT    DC    CL8'DUNIT   '       VarName for PDUNIT
DFSCYLS  DC    CL8'DFSCYLS '       VarName for FSCYLS
DFSTRKS  DC    CL8'DFSTRKS '       VarName for FSTRKS
DFSEXTS  DC    CL8'DFSEXTS '       VarName for FSEXTS
DFSLCYLS DC    CL8'DFSLCYLS'       VarName for FSLCYLS
DFSLTRKS DC    CL8'DFSLTRKS'       VarName for FSLTRKS
VTOCSZ   DC    CL8'VTOCSZ  '       VarName for PVTOCSZ
NADSCB   DC    CL8'NADSCB  '       VarName for PNADSCB
CYL4VOL  DC    CL8'CYL4VOL '       VarName for PCYL4VOL
TRK4VOL  DC    CL8'TRK4VOL '       VarName for PTRK4VOL
TRKSCYL  DC    CL8'TRKSCYL '       VarName for PTRKSCYL
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
LDTEANSR DC    A($DTECOML)         $DTECOMS
LZDATE   DC    A(L'PZDATE)         PZDATE
LZJDATE  DC    A(L'PZJDATE)        PZJDATE
LZTIME   DC    A(L'PZTIME)         PZTIME
LZENVIR  DC    A(L'PZENVIR)        PZENVIR
*
LACTN    DC    A(L'PACTN)          ACTN   Delete Selection
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
LPUCB    DC    A(L'PUCB)           PUCB
LDCUU    DC    A(L'PDCUU)          PDCUU
LVOLSER  DC    A(L'TVOLSER)        VOLSER VOLUME
LDUNIT   DC    A(L'PDUNIT)         PDUNIT
LFSCYLS  DC    A(L'FSCYLS)         FSCYLS
LFSTRKS  DC    A(L'FSTRKS)         FSTRKS
LFSEXTS  DC    A(L'FSEXTS)         FSEXTS
LFSLCYLS DC    A(L'FSLCYLS)        FSLCYLS
LFSLTRKS DC    A(L'FSLTRKS)        FSLTRKS
LVTOCSZ  DC    A(L'PVTOCSZ)        PVTOCSZ
LNADSCB  DC    A(L'PNADSCB)        PNADSCB
LCYL4VOL DC    A(L'PCYL4VOL)       PCYL4VOL
LTRK4VOL DC    A(L'PTRK4VOL)       PTRK4VOL
LTRKSCYL DC    A(L'PTRKSCYL)       PTRKSCYL
*
         EJECT
*     * /********************************************************/
*     * /* ISPF Tables Constants                                */
*     * /********************************************************/
TBLNAME  DC    CL8'TFSPC0'         Table name
*                                  Table column names
TBLNMS01 DC    C'(UCB DCUU VOLSER DUNIT DFSCYLS DFSTRKS DFSEXTS'
         DC    C' DFSLCYLS DFSLTRKS VTOCSZ NADSCB CYL4VOL'
         DC    C' TRK4VOL)'
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
*     * /********************************************************/
*     * /* Misc Constants                                       */
*     * /********************************************************/
         DVCTBL                    DVCLST Table
*
         TITLE 'DFSPC - Equates                                       '
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
         TITLE 'DFSPC - System DSECTs                                 '
*     *
*     * /********************************************************/
*     * /* System DSECTs                                        */
*     * /********************************************************/
         IHAPSA                    Prefixed Save Area
PSALEN   EQU   *-PSA               Length of PSA
         EJECT
         CVT   DSECT=YES,LIST=YES  Communication Vector Table
         EJECT
         IHAASCB                   Address Space Control Block
ASCBLEN  EQU   *-ASCB              Length of ASCB
         EJECT
         PRINT GEN
UCB      DSECT
         IEFUCBOB  LIST=YES,PREFIX=NO   Unit Control Block
         PRINT NOGEN
*
         TITLE 'DFSPC - Working Storage Variables                     '
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
PZENVIR  DS    CL20                ZENVIR  ISPF 2.1.MVS 3.8TSO
*
PACTN    DS    CL1                 Action Delete Selection
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
PUCB     DS    CL4                 UCB   **do not display BINARY**
PDCUU    DS    CL3                 DCUU
TVOLSER  DS    CL6                 VOLSER VOLUME
PDUNIT   DS    CL7                 DUNIT
PVTOCSZ  DS    CL5                 VTOCSZ
PNADSCB  DS    CL5                 NADSCB
PCYL4VOL DS    CL5                 CYL4VOL
PTRK4VOL DS    CL5                 TRK4VOL
PTRKSCYL DS    CL5                 TRKSCYL
*
         EJECT
*     * /********************************************************/
*     * /* Command buffer area                                  */
*     * /********************************************************/
COMMAND  DS    CL8
*
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
WORKAREA DS    CL8                 Workarea
UNPKAREA DS    CL16                Workarea for unpacking data
HCYL4VOL DS    F                   Cylinders for Volume
HTRKSCYL DS    F                   Tracks/Cylinder
ERRTXT   DS    CL17                Error Text msg
ERRX     DS    CL1                 Error Text type 1-     2-
TBLSRT01 DS    CL20                Sort Text C'(COL01,C,A)'
GETDTE   DS    F                   Entry point for GETDTE
DTPARML  DS    0F                  GETDTE Parameter List
DTPARM1  DS    A                   Address of Results Area
*
         EJECT
*     * /********************************************************/
*     * /* CAM and DSCB area                                    */
*     * /********************************************************/
CAMDSN   DS    CL44                CAM DSN
CAMLIST  DS    4F                  CAM Parm Address List
CAMWORK  DS    140C
         ORG   CAMWORK
         IECSDSL1 4                DSCB F4
*
         EJECT
         SVC78A                    Freespace (SVC 78) Commarea
         EJECT
         RDTECOMA                  GETDTE Commarea
         EJECT
*
         DS    0F,(40)X
WSAREAE  EQU   *                   Working Storage END
WSAREAL  EQU   WSAREAE-WSAREA      Working Storage Area Length
         EJECT
         END   DFSPC
@@
//PLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.PLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=PDFSPC0
/********************************************************************/
/*                                                                  */
/*    PANEL: PDFSPC0                                                */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/DFSPC-in-MVS38J                */
/*         Copyright (C) 2020  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x PDFSPC0 panel for DFSPC Dialogue Manager                */
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
/* 08/10/2019 1.0.00   Larry Belmontes Jr.                          */
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
{Z                       %-----? DASD Freespace %-/-/--------------------
%COMMAND ===>_Z                                                %SCROLL ===>_AMT
%                                                                      ~Z
%                                                                      ~Z
%                     %-----FREE----- %-LARGEST-   %VTOC %Avail %--Volume---
%CUU %VOLSER %DEVTYPE %Cyls%Trks%Exts %Cyls%Trks   %Trks %DSCBs % Cyls  Trks
)MODEL
[Z   $Z      {Z       [Z   [Z   [Z    [Z   [Z     [Z     [Z     [Z    [Z       %
)INIT
  .HELP = HDFSPC0
  .ZVARS = '(SCRNCLK  ZCMD ZUSER ZPANELID
              DCUU VOLSER DUNIT DFSCYLS DFSTRKS
              DFSEXTS DFSLCYLS DFSLTRKS VTOCSZ NADSCB CYL4VOL TRK4VOL)'
  &AMT = 'CSR'
  &ZTDMARK = '******************************* BOTTOM OF DATA *+
*******************+
*************'
)PROC
)END
./ ADD NAME=HDFSPC0
/********************************************************************/
/*                                                                  */
/*    PANEL: HDFSPC0                                                */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/DFSPC-in-MVS38J                */
/*         Copyright (C) 2020  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Help panel for PDFSPC0 panel                            */
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
/* 08/10/2019 1.0.00   Larry Belmontes Jr.                          */
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
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
)BODY EXPAND(//)
%--HELP----------------------? DASD Freespace %-----------------------~v1.0.00%-
%Command ===>_ZCMD                                                    +&ZUSER
%                                                                     +&ZPANELID
+DFSPC displays volume freespace and attributes for each online DASD device.
+The initial display is in%Volume Serial+order.  Data can be sorted by typing
%SORT ColName+into the%Command+line.  For descending order, preceed column name
+with%minus+sign.+ i.e.~SORT -FTR+
+
?| ColName - Sort by...                 | ColName - Sort by...                 |
?|%CUU     -[Unit Address              ?|%VOLser  -[Volume Serial Number      ?|
?|%DEVtype -[Unit Type                 ?|%FCYls   -[Tot nbr of Free Cylinders ?|
?|%FTRks   -[Tot nbr of                ?|%FEXts   -[Tot nbr of                ?|
?|%         [  Additional Free Tracks  ?|%         [  Free Extents            ?|
?|%LCYls   -[Nbr of Cylinders          ?|%LTRks   -[Nbr of Additional Tracks  ?|
?|%         [  in largest free extent  ?|%         [  in largest free extent  ?|
?|%VTOctrks-[VTOC size in Tracks       ?|%DSCbs   -[Available DSCBs on Volume ?|
?|%VCYls   -[Cylinders on Volume       ?|%VTRks   -[Tracks on Volume          ?|
?-------------------------------------------------------------------------------
+
+The display can scroll backward or forward using PF7 and PF8, respectively.
)INIT
 .CURSOR = ZCMD

)PROC

)END
@@
