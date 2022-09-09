//CCOMPR JOB (JOB),
//             'INSTALL CCOMPR',
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
//* *  CCOMPR for MVS3.8J TSO / Hercules                   *
//* *                                                      *
//* *  JOB: $INST04                                        *
//* *       Install CCOMPR Program                         *
//* *                                                      *
//* *  - Install libraries marked...                       *
//* *    - Search for '<--TARGET'                          *
//* *    - Update install libraries per your               *
//* *      installation standard                           *
//* *                                                      *
//* -------------------------------------------------------*
//*
//* -------------------------------------------------------*
//* *                                                      *
//* *  PROC: ASMLKED                                       *
//* *       Assembler Link-Edit                            *
//* *                                                      *
//* -------------------------------------------------------*
//ASML     PROC
//*
//ASM      EXEC PGM=IFOX00,
//             PARM='NODECK,LOAD,RENT,TERM,XREF'
//SYSGO    DD  DSN=&&LOADSET,DISP=(MOD,PASS),SPACE=(CYL,(1,1)),
//             UNIT=VIO,DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//         DD  DSN=SYS2.MACLIB,DISP=SHR          ** YREG  **
//         DD  DSN=&&MACLIBS,DISP=OLD   * myMACLIB **
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
//SYSLMOD  DD  DUMMY
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(5,2))
//SYSIN    DD  DUMMY
//*
//         PEND
//*

//* -------------------------------------------------------*
//* *  Assemble Link-Edit CCOMPR to ISPLLIB                *
//* -------------------------------------------------------*
//CCOMPR   EXEC  ASML
//ASM.SYSIN DD DATA,DLM=@@
         TITLE 'CCOMPR - ISPF Compare Front-end Interface             '
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*  ==================================================================
*
*   CCC      CCC     OOOOO   MM   MM  PPPPP   RRRRRR
*  CC  CC   CC  CC  OO   OO  MMM MMM  PP  PP  RR   RR
* CC       CC       OO   OO  MMMMMMM  PP  PP  RR   RR
* CC       CC       OO   OO  MM M MM  PPPPP   RRRRRR
* CC       CC       OO   OO  MM   MM  PP      RR RR
*  CC  CC   CC  CC  OO   OO  MM   MM  PP      RR  RR
*   CCC      CCC     OOOOO   MM   MM  PP      RN   RR
*
*  ==================================================================
*
*
*  Program: CCOMPR
*
*  Author:  Larry Belmontes Jr.
*           https://ShareABitofIT.net/CCompare-in-MVS38J
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
*     CCOMPR acts as a ISPF front-end to manage a COMPARE dialogue and
*  releases control when END/RETURN is detected.
*
*     Functions associated with CCOMPR include the following:
*  - Dynamically allocate / Unallocate a results sequential file
*      using DD OUT
*  - Format command lines for TSO requests (i.e. COMPARE CP)
*  - Verify compare datasets existence
*  - Cross-edits on compare panel options
*  - Invoke COMPARE CP
*  - Browse compare results via ISPF Browse service (if selected)
*  - Print compare results via PRINTOFF CP (if selected)
*  - Delete results dataset (if selected)
*
*     To distinguish between a COMPARE or TSO COMMAND request, an
*  asterick ('*') is used in the CCOMPR panel COMMAND line.
*
*     CCOMPR panel data is saved to the users ISPF PROFILE under the
*   CMPR application for retrieval between sessions.
*
*     CHKDSN, which is part of this CCOMPR distribution, is used to
*  validate dataset presence in real-time.
*
*     PRINTOFF is used to print browse results.
*
*     The COMPARE CP is the back-bone of this process to compare two
*  sequential, two PDS datasets or two PDS(member) datasets.
*
*     CCOMPR is written in IFOX00 Assembler (24-bit addressing) and
*  tested using Volker Bandke's MVS CD - MVS38J TK3 with TSO /
*  ISPF v2.1 from Wally Mclaughlin for ISPF services hosted on
*  Hercules 3.13 under Windows 10 Pro.
*
*  NOTE: CCOMPR requires COMPARE / COMPAREB from CBT File#296, which
*  ----- differs from the original COMPARE version supplied in  the
*        MVS 3.8J Tur(n)key 3 and TK4- distributions.
*
*  NOTE: CCOMPR tested with PRINTOFF version provided in the
*  ----- MVS 3.8J Tur(n)key 3 and TK4- distributions.
*
*     CCOMPR must be started as a command line request as it needs a
*  CPPL.
*
*     It is suggested that CCOMPR be invoked from an ISPF menu,
*  preferably, the Utilities menu, as option 3.C, using the following
*  menu entry:
*
*    %   C +COMPARE     - Compare Two Online Datasets using COMPARE
*
*     Assume an ISPF menu panel has the following sections, the
*  'NEW ENTRY' line can be added to invoke CCOMPR when menu option 'C'
*  is typed followed by the ENTER key
*
*    )PROC
*      &ZSEL = TRANS( TRUNC (&ZCMD,'.')
*                    1,'CMD(xxxxx) NEWAPPL(ISR)'
*                    6,'PGM(xxxxxx)'
*                    .
*                    .
*                    C,'CMD(CCOMPR 0) NEWAPPL(CMPR)'     <-- NEW ENTRY
*                    .
*                    .
*                  ' ',' '
*                    *,'?' )
*    )END
*
*
*     CCOMPR uses the following ISPF components:
*            - PCOMPR    Compare request panel
*            - HCOMPR    Compare Help Panel
*            - CMPR00    Messages
*            - CMPR01    Messages
*
*     CCOMPR uses the following software:
*            - COMPARE   Compare CP from CBT File#296
*            - PRINTOFF  Print CP from MVS38J TK3 or TK4- system
*            - CHKDSN    Check for DSN CP (part od CCOMPR distribution)
*
*
*     Thanks to the various COMPARE CP authors, Brent Tolman, Bill
*  Godfrey, Jim Marshall,  Bruce Leland, and Guy Albertelli per COMPARE
*  program documentation.
*
*     Thanks for the IPO-supplied PRINTOFF CP to facililate printing
*  of datasets from TSO command line.
*
*
*  Enjoy the added value of comparing datasets function via ISPF v2.x!
*
*  Larry Belmontes Jr.
*
*
*
         EJECT
*  Prerequisite: User Modifications <PREREQS>
*  ===================================================================
*
*     No specific user modifications are necessary.
*
*     COMPARE and PRINTOFF CPs are available on MVS38J TK3 and TK4-
*  systems.  Also, various versions (of COMPARE and PRINTOFF) are
*  available from CBTTAPE.ORG (www.cbttape.org).
*
*     If the above CPs are not installed, an error message is
*  displayed accordingly.
*
*     ISPF v2.1 (Wally Mclaughlin's product) must be installed on
*  MVS38J.
*
*
         EJECT
*  CCOMPR Programs / Services:
*  ==================================================================
*
*    1) GETMAIN/FREEMAIN          Working storage
*    2) Dynamic Allocation        File/DD Allocation/Unallocation
*    3) CHKDSN                    Public Domain CHKDSN  program
*                                  (part of this distribution)
*    4) COMPARE                   Public Domain COMPARE program
*    5) PRINTOFF                  Public Domain PRINTOFF program
*    6) SHRABIT.MACLIB            Macros for ShareABitofIT Programs
*                                  (part of this distribution)
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
*  |  00  |  Successful Completion                                   |
*  +------+----------------------------------------------------------+
*  | 4090 |  Must use command line type start                        |
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
*  IHAPSCB   Protected Step Control Block        SYS1.AMODGEN
*  IHAASCB   Address Space Control Block         SYS1.AMODGEN
*  IKJCPPL   CP Parm List                        SYS1.MACLIB
*  IKJUPT    User Profile Table                  SYS1.MACLIB
*  IKJPPL    Parse Parm List                     SYS1.MACLIB
*  IKJIOPL   I/O Service Routine Parm List       SYS1.MACLIB
*  IEFZB4D0  S99 Parm List                       SYS1.MACLIB
*  IEFZB4D2  S99 Text Unit Keys                  SYS1.MACLIB
*
*  ISPFPL    ISPF Parm List for LBISPL           SHRABIT.MACLIB
*  ISPFSRV   ISPF Services and Keywords          SHRABIT.MACLIB
*  LBISPL    CALL equivalent for ISPLINK         SHRABIT.MACLIB
*  MOVEC     Move quoted string data             SHRABIT.MACLIB
*  MOVEI     Initialize variable                 SHRABIT.MACLIB
*  MOVER     Move string right-trim              SHRABIT.MACLIB
*  MOVEV     Move variable                       SHRABIT.MACLIB
*  RTRIM     Right-trim subroutine               SHRABIT.MACLIB
*
*
*  References:
*  ==================================================================
*
*  - MVS38J TK3     Includes versions of COMPARE and PRINTOFF
*                   Various versions can be found at CBTTAPE.ORG
*                   Information on COMPARE  via TSO HELP COMPARE
*                   Information on PRINTOFF via TSO HELP PRINTOFF
*  - MVS38J TK4-    Includes versions of COMPARE and PRINTOFF
*                   Various versions can be found at CBTTAPE.ORG
*                   Information on COMPARE  via TSO HELP COMPARE
*                   Information on PRINTOFF via TSO HELP PRINTOFF
*  - GC28-0627-2    OS/VS2 MVS System Programming Library Job Mgmt
*
*
         EJECT
*  Messages:
*  ==================================================================
*
*  +--------+--------------------------------------------------------+
*  |Msg ID  |Description                                             |
*  +--------+--------------------------------------------------------+
*  |CMPR000 |Welcome...                                              |
*  +--------+--------------------------------------------------------+
*  |CMPR001 |OLD DSN - &MYMSG                                        |
*  |        |Note: Includes additional information for OLD DSN.      |
*  +--------+--------------------------------------------------------+
*  |CMPR002 |NEW DSN - &MYMSG                                        |
*  |        |Note: Includes additional information for NEW DSN.      |
*  +--------+--------------------------------------------------------+
*  |CMPR003 |Compare Type must be FULL, ASM or NOASM.                |
*  +--------+--------------------------------------------------------+
*  |CMPR004 |Compare Program Name must be COMPARB, IEBCMPR or ZEBCMPR|
*  |        |.                                                       |
*  +--------+--------------------------------------------------------+
*  |CMPR005 |Print Results must be Y or N.                           |
*  +--------+--------------------------------------------------------+
*  |CMPR006 |Delete Results must be Y or N.                          |
*  +--------+--------------------------------------------------------+
*  |CMPR007 |Member name required for OLD and NEW DSN.  Or, NO member|
*  |        |name.                                                   |
*  +--------+--------------------------------------------------------+
*  |CMPR008 |Use 'x' to mark option.                                 |
*  +--------+--------------------------------------------------------+
*  |CMPR009 |Invalid Selection &KEYPRESS  &LCMD                      |
*  +--------+--------------------------------------------------------+
*  |CMPR010 |Dataset Name is invalid, cannot contain blanks          |
*  +--------+--------------------------------------------------------+
*  |CMPR011 |Unit must be blank                                      |
*  +--------+--------------------------------------------------------+
*  |CMPR012 |Browse Results must be Y or N.                          |
*  +--------+--------------------------------------------------------+
*  |CMPR013 |Press ENTER to process request.                         |
*  +--------+--------------------------------------------------------+
*  |CMPR018 |RC=&MYRC &MYMSG                                         |
*  |        |Note: Includes return code and additional information.  |
*  +--------+--------------------------------------------------------+
*  |CMPR019 |Compare Complete. RC=&MYRC                              |
*  |        |Note: Includes return code.                             |
*  +--------+--------------------------------------------------------+
*
*
*
         EJECT
*  Change History:
*  ==================================================================
*
*  MM/DD/CCYY Version  Name / Description
*  ---------- -------  --------------------------------------------
*
*  08/10/2020 0.9.00   Larry Belmontes Jr.
*                      - Initial version released to MVS 3.8J
*                        hobbyist public domain
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         TITLE 'CCOMPR - ISPF Compare Option using COMPARE            '
CCOMPR   CSECT
         USING CCOMPR,R10,R11,R12  my BASE REGISTER(S)
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
PGMID    DC    CL8'CCOMPR '        My Program STAMP
         DC    CL8'MVS3.8J '       OS
PGMVRM   DC    CL8'V0.9.00 '       .Version
         DC    CL8'08102020'       ..date  MMDDCCYY
         DC    CL1' '
PGMDS    DC    CL8'&SYSDATE'       Assemble Date
         DC    CL3' - '            .and
PGMTS    DC    CL8'&SYSTIME'       ..Time
         DC    C'Copyright (C) 2020'
         DC    C'Larry Belmontes, Jr. '
         DC    C'https://ShareABitofIT.net/CCompare-in-MVS38J'
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
         ST    R2,MYCPPLP          Save Original PARM addr
         MVI   VDEF,C'N'           Vdefine switch
*
         EJECT
*     * /********************************************************/
*     * /* GET Parm Data                                        */
*     * /* - R1  Parm    Register                               */
*     * /* - R15 Working Register                               */
*     * /********************************************************/
         MVI   PRO,C'0'            Assume process PCOMPR panel
*                                  0-Display and process PCOMPR panel
*                                  1-Process vars only
***      LTR   R1,R1               Do I have a PARM?
***      BZ    NOPARM              NO, continue...
         TM    0(R1),X'80'         Yes. Is it PARM or CMDL addr?
         BZ    CMDLIN              YES, CMDL addr
PARMIN   EQU   *                   NO,  must be PARM addr
         B     ERR4090             ..error, must be command line start
***                                ..cannot be a PARM!
         L     R1,0(,R2)           Addrs of PARM
         LH    R15,0(,R1)          Length of PARM
         LTR   R15,R15             Have a PARM ?
         BZ    NOPARM              No, continue...
         CH    R15,=H'1'           Yes. Long enough for '0' or '1' ?
         BL    NOPARM              No, continue...
         MVC   PRO,2(R1)           Yes, get it! (point past parm len)
         B     CHKPARM
CMDLIN   EQU   *
         L     R1,0(,R2)           Command Buffer addr
         LH    R15,2(,R1)          Offset to Command Variables
         LTR   R15,R15             Any Variables?
         BZ    NOPARM              No, continue...
         LA    R1,4(R1,R15)        Point to variables start addr
         MVC   PRO,0(R1)           Yes, get it! (point past parm len)
CHKPARM   EQU   *
**       STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
**       TPUT  PRO,1               Display parm (0 or 1)
**       LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
         CLI   PRO,C'0'            Valid, 0?
         BE    PARMCNT             Yes.
         CLI   PRO,C'1'            Valid, 1?
         BE    PARMCNT             Yes.
         MVI   PRO,C'0'            No, assume value 0
         B     INVPARM
NOPARM   EQU   *
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
         TPUT  NOPRO,L'NOPRO       Display no parm msg
         LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
         B     PARMCNT
INVPARM  EQU   *
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
         TPUT  INVPRO,L'INVPRO     Display inval parm msg
         LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
         B     PARMCNT
NOPRO    DC    C'No parm or parm > 1 byte, assume 0'
INVPRO   DC    C'Invalid parm, assume 0'
PARMCNT  DS    0H
*
         EJECT
*     * /********************************************************/
*     * /* ISPLINK Entry Point                                  */
*     * /* - R0 Working Register                                */
*     * /********************************************************/
         XC    ISPLINK,ISPLINK     Clear ISPLINK Address
         LOAD  EP=ISPLINK,ERRET=ERR4096
         ST    R0,ISPLINK          Save ISPLINK Entry Point
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
*     * /* Initialize RC                                        */
*     * /********************************************************/
         MVC   RCEXIT,=F'0'        Zero Return Code R15
*
         BAL   R14,ISPCNTL         CONTROL ERRORS RETURN
         BAL   R14,ISPVDEF         VDEFINE ISPF Variables
         BAL   R14,ISPVGET         VGET Profile Variables
*     * /********************************************************/
*     * /* Initialize - OTHER                                   */
*     * /********************************************************/
         MOVEI DJMSG,ONLY          Blank DJMSG
*
*     * /********************************************************/
*     * /*  Initialize ISPF Function Variables                  */
*     * /********************************************************/
         MOVEI PMYMSG,ONLY         Blank PMYMSG
         MVC   PMYRC,=C'00000'     Zero  PMYRC
         MOVEI PMYCMD,ONLY         Blank PMYCMD
         MVC   PMYPGM,PGMID        Program name
         MVC   PMYVRM,PGMVRM       Version Rel Mod
*
         EJECT
*     * /********************************************************/
*     * /*  ISPEXEC DISPLAY PANEL(PCOMPR) CURSOR(&SCRNCSR) &PMSG*/
*     * /* - R15, R1    Working Register                        */
*     * /********************************************************/
**       MVC   PANMSG,=C'CMPR000 ' Welcome...
         MOVEI PANMSG,ONLY
         MVC   PANCSR,=C'ODSN    ' Panel Cursor
GETDATA  EQU   *
         CLI   PRO,C'0'            Process PCOMPR panel ?
         BNE   PROCVARS            No, process vars only..
*                                  Yes, go display PCOMPR
         DELETE EP=ISPLINK         Keep my pgms
         DELETE EP=CHKDSN             of the
         DELETE EP=COMPARE               load
         DELETE EP=PRINTOFF                 space...
*        CALL  ISPLINK,(DISPLAY,PANID,PANMSG,PANCSR),VL
         LBISPL ISPLINK,(DISPLAY,PANID,PANMSG,PANCSR),VL
*        LBISPL ISPLINK,(DISPLAY,PANID,B,PANCSR),VL
         ST    R15,RCEXIT          Store R15
         C     R15,=F'8'           RC=8? (END/RETURN JUMP)
         BE    MYEXITF             Yes, exit
         BAL   R14,ISPERR          Err Display, return here or MYEXIT
*                                  No, assume display error and exit
*     * /********************************************************/
*     * /* Process when ZCMD = '*'                              */
*     * /* to allow for TSO commands                            */
*     * /********************************************************/
PROCVARS EQU   *
**       STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
**       TPUT  PLCMD,1             Display 1st byte of ZCMD
**       LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
         CLI   PLCMD,C'*'          Process request from panel?
         BE    PROC00              Yes, process.
         CLI   PLCMD,C' '          Blank command line?
         BNE   INVSEL              No, Invalid selection
         MVC   PANMSG,=C'CMPR013 ' Yes, Press ENTER msg...
         B     GETNOTHR            Return to caller
INVSEL   EQU   *
         MVC   PANMSG,=C'CMPR009 ' No, Invalid selection msg...
         B     GETNOTHR            Return to caller
PROC00   EQU   *
*     * /********************************************************/
*     * /* Compare Done message, Cursor, RC (Default)           */
*     * /********************************************************/
         MVC   PANMSG,=C'CMPR019 ' Compare complete msg...
         MVC   PANCSR,=C'ODSN    ' Panel Cursor
         MVC   PMYRC,=C'00000'     Zero  PMYRC
*
         EJECT
*     * /********************************************************/
*     * /* Edit- Member name in both DSN or no member name      */
*     * /********************************************************/
         CLI   PCOMBR,C' '         OMBR specified?
         BE    CKMBR01             No, check NMBR for no NMBR
         CLI   PCNMBR,C' '         Yes, NMBR specified?
         BE    CKMBR02             No, OMBR w/ no NMBR - error
         B     CKMBR99             Yes, good edit
CKMBR01  EQU   *
         CLI   PCNMBR,C' '         NMBR specified?
         BE    CKMBR99             No, good edit - no members
*                                  Yes, no OMBR w/ NMBR - error
         MVC   PANCSR,=C'OMBR    ' Panel Cursor Field
         B     CKMBR90             Set error msg
CKMBR02  EQU   *
         MVC   PANCSR,=C'NMBR    ' Panel Cursor Field
CKMBR90  EQU   *
         MVC   PANMSG,=C'CMPR007 ' Need both mbr or no mbr names
         B     GETNOTHR            Display error and get user input
CKMBR99  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Edit- Unit Name (must be blank)                      */
*     * /********************************************************/
         CLC   PCOUNT,B            OUNT specified?
         BNE   CKUNT01             Yes, error
         CLC   PCNUNT,B            No, NUNT specified?
         BNE   CKUNT02             Yes, error
         B     CKUNT99             No, good edit
CKUNT01  EQU   *
         MVC   PANCSR,=C'OUNT    ' Panel Cursor Field
         B     CKUNT90             Set error msg
CKUNT02  EQU   *
         MVC   PANCSR,=C'NUNT    ' Panel Cursor Field
CKUNT90  EQU   *
         MVC   PANMSG,=C'CMPR011 ' UNT must be blank
         B     GETNOTHR            Display error and get user input
CKUNT99  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Edit- Compare Type Option                            */
*     * /* - R5  Working Register                               */
*     * /********************************************************/
         LA    R5,0                Clear R5
CKCT001  EQU   *
         CLI   PCFULL,C' '         Entry?
         BE    CKCT002             No.
         LA    R5,1(R5)            Yes, count it.
         MVC   PCTYPE,=C'FULL '
CKCT002  EQU   *
         CLI   PCASM,C' '          Entry?
         BE    CKCT003             No.
         LA    R5,1(R5)            Yes, count it.
         MVC   PCTYPE,=C'ASM  '
CKCT003  EQU   *
         CLI   PCNOASM,C' '        Entry?
         BE    CKCT999             No.
         LA    R5,1(R5)            Yes, count it.
         MVC   PCTYPE,=C'NOASM'
CKCT999  EQU   *
         C     R5,=F'1'            More than one entry?
         BE    CKCTXIT             No.
         MVC   PANCSR,=C'CFULL   ' Yes, Panel Cursor Field
         MVC   PANMSG,=C'CMPR003 ' Need only ONE Compare Type
         B     GETNOTHR            Display error and get user input
CKCTXIT  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Edit- Compare Program Option                         */
*     * /* - R5  Working Register                               */
*     * /********************************************************/
         LA    R5,0                Clear R5
         MOVEI PCPGM,ONLY          Blank PCPGM
CKCP001  EQU   *
         CLI   PCCMPARB,C' '       Entry?
         BE    CKCP002             No.
         LA    R5,1(R5)            Yes, count it.
CKCP002  EQU   *
         CLI   PCIBCMPR,C' '       Entry?
         BE    CKCP003             No.
         LA    R5,1(R5)            Yes, count it.
         MVC   PCPGM,=C'IEBCOMPR'
CKCP003  EQU   *
         CLI   PCZBCMPR,C' '       Entry?
         BE    CKCP999             No.
         LA    R5,1(R5)            Yes, count it.
         MVC   PCPGM,=C'ZEBCOMPR'
CKCP999  EQU   *
         C     R5,=F'1'            More than one entry?
         BE    CKCPXIT             No.
         MVC   PANCSR,=C'CCOMPARB' Yes, Panel Cursor Field
         MVC   PANMSG,=C'CMPR004 ' Need only ONE Compare Pgm
         B     GETNOTHR            Display error and get user input
CKCPXIT  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Build CHKDSN command line for OLD DSN                */
*     * /* - CHKDSN ODSN(OMBR) QUIET VOL(OVOL)                  */
*     * /* - R3, R4, R6, R8  Working Register                   */
*     * /********************************************************/
CKODS00  EQU   *
         MOVEI TEXT
         MOVEC 'CHKDSN '             Move CHKDSN as command
         STH   R8,OFFSET             Store OFFSET
         CLI   PCODSN,C''''          ODSN start w/ apostrophe?
         BNE   CKODS01               No, move ODSN from panel
         MOVEC ''''                  Yes, move ' before ODSN
CKODS01  EQU   *
         MOVER PCODSN
         CLI   PCOMBR,C' '           Member present?
         BE    CKODS02               No, bypass, continue...
         MOVEC '('                   Yes, move '('
         MOVER PCOMBR                           OMBR
         MOVEC ')'                                  ')'
CKODS02  EQU   *
         CLI   PCODSN,C''''          ODSN start w/ apostrophe?
         BNE   CKODS03               No, done w/ ODSN
         MOVEC ''''                  Yes, terminate ODSN w/ '
CKODS03  EQU   *
         MOVEC ' QUIET '
         CLI   PCOVOL,C' '           VOL provided?
         BE    CKODS04               No, bypass, continue...
         MOVEC 'VOL('                Move  'VOL('
         MOVER PCNVOL                           NVOL
         MOVEC ')'                                  ')'
CKODS04  EQU   *
CKODSXT  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Execute CHKDSN for OLD DSN                           */
*     * /* - Link to CHKDSN                                     */
*     * /* - R1, R2, R8, R15 Working Register                   */
*     * /********************************************************/
CKODSAA  EQU   *
         LA    R8,4(R8)            TEXT length +4 (prefix)
         STH   R8,CBUF
*        MVC   OFFSET(2),=H'8'     Offset moved in construct buffer
         L     R2,MYCPPLP          R2=CPPL Address
         LA    R1,CBUF
         ST    R1,0(R2)
         LR    R1,R2
         MVC   COMMAND,=C'CHKDSN  '
         LINK  EP=CHKDSN,ERRET=PGMNFND
         LTR   R15,R15             RC=0?
         BZ    CKODSXIT            Yes, continue...
*                                  No, ODSN not found error
         DELETE EP=CHKDSN
         MOVEI PMYMSG              Initialize PMYMSG
         CLI   PCOVOL,C' '         VOL specified?
         BE    CKODSA2             No, continue...
         MOVEC ' On '              Yes, include OVOL
         MOVER PCOVOL
CKODSA2  EQU   *
         C     R15,=F'16'          Member not found?
         BE    CKODSA3             Yes, mbr not fnd
         MOVEC ' Not Found'
         MVC   PANCSR,=C'ODSN    ' No, DSN not fnd Panel Cursor
         B     CKODSA4
CKODSA3  EQU   *
         MOVEC ' Member Not Found'
         MVC   PANCSR,=C'OMBR    ' Panel Cursor
CKODSA4  EQU   *
         MVC   PANMSG,=C'CMPR001 ' Old DSN error
         B     GETNOTHR            Display error and get user input
CKODSXIT EQU   *
         DELETE EP=CHKDSN
*
         EJECT
*     * /********************************************************/
*     * /* Build CHKDSN command line for NEW DSN                */
*     * /* - CHKDSN NDSN(NMBR) QUIET VOL(NVOL)                  */
*     * /* - R3, R4, R6, R8  Working Register                   */
*     * /********************************************************/
CKNDS00  EQU   *
         MOVEI TEXT
         MOVEC 'CHKDSN '             Move CHKDSN as command
         STH   R8,OFFSET             Store OFFSET
         CLI   PCNDSN,C''''          NDSN start w/ apostrophe?
         BNE   CKNDS01               No, move NDSN only
         MOVEC ''''                  Yes, terminate NDSN w/ '
CKNDS01  EQU   *
         MOVER PCNDSN                Move NDSN
         CLI   PCNMBR,C' '           Member present?
         BE    CKNDS02               No, bypass, continue...
         MOVEC '('                   Yes, move '('
         MOVER PCNMBR                             NMBR
         MOVEC ')'                                    ')'
CKNDS02  EQU   *
         CLI   PCNDSN,C''''          DSN start w/ apostrophe?
         BNE   CKNDS03               No, done w/ NDSN
         MOVEC ''''                  Yes, terminate NDSN w/ '
CKNDS03  EQU   *
         MOVEC ' QUIET '             Move ' QUITE '
         CLI   PCNVOL,C' '           VOL present?
         BE    CKNDS04               No, continue...
         MOVEC 'VOL('                Yes, move 'VOL('
         MOVER PCNVOL                               NVOL
         MOVEC ')'                                      ')'
CKNDS04  EQU   *
CKNDSXT  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Execute CHKDSN for NEW DSN                           */
*     * /* - Link to CHKDSN                                     */
*     * /* - R1, R2, R8, R15 Working Register                   */
*     * /********************************************************/
CKNDSAA  EQU   *
         LA    R8,4(R8)            TEXT length +4 (prefix)
         STH   R8,CBUF
*        MVC   OFFSET(2),=H'8'     Offset moved in construct buffer
         L     R2,MYCPPLP          R2=CPPL Address
         LA    R1,CBUF
         ST    R1,0(R2)
         LR    R1,R2
         MVC   COMMAND,=C'CHKDSN  '
         LINK  EP=CHKDSN,ERRET=PGMNFND
         LTR   R15,R15             RC=0?
         BZ    CKNDSXIT            Yes, continue...
*                                  No, NDSN not found error
         DELETE EP=CHKDSN
         MOVEI PMYMSG
         CLI   PCNVOL,C' '         VOL specified?
         BE    CKNDSA2             No, continue...
         MOVEC ' On '              Yes, move ' on '
         MOVER PCNVOL                             NVOL
CKNDSA2  EQU   *
         C     R15,=F'16'          Member not found?
         BE    CKNDSA3             Yes, mbr not fnd
         MOVEC ' Not Found'
         MVC   PANCSR,=C'NDSN    ' No, DSN not fnd Panel Cursor
         B     CKNDSA4
CKNDSA3  EQU   *
         MOVEC ' Member Not Found'
         MVC   PANCSR,=C'NMBR    ' Panel Cursor
CKNDSA4  EQU   *
         MVC   PANMSG,=C'CMPR002 ' New DSN error
         B     GETNOTHR            Display error and get user input
CKNDSXIT EQU   *
         DELETE EP=CHKDSN
*
         EJECT
*     * /********************************************************/
*     * /* Display any messages from COMPARE at line 21         */
*     * /* ISPEXEC CONTROL DISPLAY LINE START(21)               */
*     * /********************************************************/
         LBISPL ISPLINK,(CONTROL,DISPLAY,LINE,LINENO),VL
         MVC   TEXT(10),=C'Working...'
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
         TPUT  TEXT,10
         LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
*
         EJECT
*     * /********************************************************/
*     * /* Free Result Dataset - OUT                            */
*     * /* - via DYNALLOC                                       */
*     * /* - R5, R6, R7, R15 Working Register                   */
*     * /********************************************************/
FREE01A  EQU   *
         MVI   R99VERB,S99VRBUN    Unallocation VERB
         LA    R5,TUPTR003         Addr of Text-Unit-Pointer List
         BAL   R7,DYNALC           Issue SVC99
         ST    R15,RCEXIT
         C     R15,=F'4'           RC <= 4 ?
         BNH   FREE01AX            Yes, continue...
         MVC   ERRTXT,=C'Unalloc1 DD-OUT  '  No, error...
         BAL   R7,DOERR2           Format Dynalloc Error
         B     GETDATA             Display error, get input
FREE01AX EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Allocate OUT as DSN                                  */
*     * /* - R5, R6, R7, R15 Working Register                   */
*     * /********************************************************/
ALLC01A  EQU   *
         MVI   R99VERB,S99VRBAL    Allocation VERB
*                                  Insert DSN into text unit in WS
         MVC   RUPTR001(TUPTR0L1),TUPTR001  Move Alloc TU Parm List
         MVC   RUDSNA1(TUDSNA1L),TUDSNA1  Move DSN Alloc TU
         BAL   R14,TEMPDSN         Initialize Temp DSN
         MVC   RUDSN,DDSN          Move temp DSN to Alloc TU
*        MVC   TEXT(13),=C'Allocating...'
*        MVC   TEXT+13(44),RUDSN
*        STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
*        TPUT  TEXT,57
*        LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
         LA    R1,RUDSNA1          Addr of RUDSNA1
         ST    R1,RUPTR001+4       Store new addr of RUDSNA1
*
         LA    R5,RUPTR001         Addr of Text-Unit-Pointer List
         BAL   R7,DYNALC           Issue SVC99
         ST    R15,RCEXIT
         LTR   R15,R15             RC=0?
         BZ    ALLC01AX            Yes, continue...
         MVC   ERRTXT,=C'Alloc1 DD-OUT    '  No, error...
         BAL   R7,DOERR2           Format Dynalloc Error
         B     GETDATA             Display error, get input
ALLC01AX EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Build COMPARE command line                           */
*     * /* - COMPARE ODSN NDSN CTYPE CPGM NOPRINT OUTFILE(OUT)  */
*     * /* - R3, R4, R6, R7, R15  Working Register              */
*     * /********************************************************/
COMPR00  EQU   *
         MOVEI TEXT                  Blank TEXT
         MOVEV COMPARE               Move COMPARE command
         STH   R8,OFFSET             Store OFFSET
         MOVER PCODSN                Move ODSN from panel
         BCTR  R6,0                  Backup 1 byte from ODSN last char
         CLI   0(R6),C''''           Apostrophe ?
         BNE   COMPR01               No, bump up 1 byte
         BCTR  R8,0                  Yes, back down 1 for len, also
         B     COMPR02               continue...
COMPR01  EQU   *
         LA    R6,1(R6)
COMPR02  EQU   *
         CLI   PCOMBR,C' '           Member present?
         BE    COMPR03               No, continue...
         MOVEC '('                   Append (OMBR) to ODSN
         MOVER PCOMBR
         MOVEC ')'
COMPR03  EQU   *
         CLI   PCODSN,C''''          ODSN start w/ apostrophe?
         BNE   COMPR04               No, done w/ ODSN
         MOVEC ''''                  Yes, terminate ODSN w/ '
COMPR04  EQU   *
         MOVEC ' '                   Move BLANK
         MOVER PCNDSN                Move NDSN from panel
         BCTR  R6,0                  Backup 1 byte from NDSN last char
         CLI   0(R6),C''''           Apostrophe ?
         BNE   COMPR05               No, bump up 1 byte
         BCTR  R8,0                  Yes, back down 1 for len, also
         B     COMPR06               continue...
COMPR05  EQU   *
         LA    R6,1(R6)
COMPR06  EQU   *
         CLI   PCNMBR,C' '           Member present?
         BE    COMPR07               No, continue...
         MOVEC '('                   Yes, move '('
         MOVER PCNMBR                            NMBR
         MOVEC ')'                                  ')'
COMPR07  EQU   *
         CLI   PCNDSN,C''''          NDSN start w/ apostrophe?
         BNE   COMPR08               No, done w/ NDSN
         MOVEC ''''                  Yes, terminate NDSN w/ '
COMPR08  EQU   *
         MOVEC ' '
         MOVER PCTYPE                Move PCTYPE
         LTR   R4,R4                 PCTYPE present?
         BZ    COMPR09               No, continue...
         MOVEC ' '                   Yes, add a BLANK
COMPR09  EQU   *
         MOVER PCPGM                 Move PCPGM
         LTR   R4,R4                 PCPGM present?
         BZ    COMPR10               No, continue...
         MOVEC ' '                   Yes, add a BLANK
COMPR10  EQU   *
         MOVEC 'NOPRINT OUTFILE(OUT)'  Move NOPRINT and OUTFILE DD
         LA    R8,4(R8)               Add PREFIX to length
         STH   R8,CBUF                Store Buffer Length
         S     R8,=F'4'               Reset R8 to text length
*
         MVC   PMYCMD,TEXT         Compare Command
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
         TPUT  TEXT,90
         LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
*
         EJECT
*     * /********************************************************/
*     * /* Execute COMPARE                                      */
*     * /* - R1, R2,R7, R15  Working Register                   */
*     * /********************************************************/
COMPRSAA EQU   *
         L     R2,MYCPPLP          R2=CPPL Address
         LA    R1,CBUF
         ST    R1,0(R2)
         LR    R1,R2
         MVC   COMMAND,COMPARE
         LINK  EP=COMPARE,ERRET=PGMNFND
         ST    R15,CMPRRC2         Store COMPARE RC
         CVD   R15,DW              Convert to decimal
         UNPK  PMYRC(5),DW+5(3)    Unpack
         OI    PMYRC+04,X'F0'        Force an F Zone
         MVC   DW(4),PMYRC+1
         MVC   PMYRC(04),DW          Use 4 bytes only
         MVI   PMYRC+04,C' '         Blank last byte
***      B     COMPRSXT
COMPRSXT EQU   *
         DELETE EP=COMPARE
*
         EJECT
*     * /********************************************************/
*     * /* Append COMPARE info to temp dataset                  */
*     * /* - R7   Working Register                              */
*     * /********************************************************/
         MVC   WOUTFIL,OUTFIL      Move DCB constant to workarea
         MVC   WOPENO,OPENO        Move Open constant to workarea
*
         LA    R7,WOUTFIL
         OPEN  ((R7),(OUTPUT)),MF=(E,WOPENO)
         MOVEI PREC,ONLY           Initialize PREC
         MVC   PREC(11),=C'- Command: '
         MVC   PREC+11(L'PREC-11-1),TEXT Compare Command
         LA    R7,WOUTFIL
         PUT   (R7),PREC           WRITE TO OUTFIL
*
         MOVEI PREC,ONLY           Initialize PREC
         MVC   PREC(11),=C'- Results: '
         MVC   PREC+11(L'DDSN),DDSN Temp DSN
         LA    R7,WOUTFIL
         PUT   (R7),PREC           WRITE TO OUTFIL
*
         LA    R7,WOUTFIL          Yes, close file
         MVC   WCLOSEO,CLOSEO
         CLOSE ((R7),),MF=(E,WCLOSEO)
*
         EJECT
*     * /********************************************************/
*     * /* Browse compare results when RC < 16                  */
*     * /* - R15  Working Register                              */
*     * /********************************************************/
BRWS00   EQU   *
         CLI   PCBRW,C'Y'          Browse results requested?
         BNE   BRWS00XT            No, bypass
         CLC   CMPRRC2,=F'16'      RC < 16?
         BNL   BRWS00XT            No, bypass RC >= 16 !
         MOVEI BDSN                No, Initialize BDSN
         MOVEC ''''                Move '
         MOVER DDSN                      DDSN
         MOVEC ''''                          '
*        CALL  ISPLINK,(CONTROL,DISPLAY,SAVE),VL
         LBISPL ISPLINK,(CONTROL,DISPLAY,SAVE),VL
*        CALL  ISPLINK,(BROWSE,BDSN),VL
         LBISPL ISPLINK,(BROWSE,BDSN),VL
         ST    R15,RCEXIT          Store R15
         LTR   R15,R15             RC=0?
         BZ    BRWS00OK            Yes, continue...
         BAL   R7,DOERR1           Format General  Error
         MOVEI PMYMSG              Override PMYMSG
         MOVEC 'Cannot Browse Results File'
BRWS00OK EQU   *
*        CALL  ISPLINK,(CONTROL,DISPLAY,RESTORE),VL
         LBISPL ISPLINK,(CONTROL,DISPLAY,RESTORE),VL
BRWS00XT EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Print compare results via PRINTOFF  when RC < 16     */
*     * /* - R1, R2,R7, R15  Working Register                   */
*     * /********************************************************/
PRTF1AA  EQU   *
         CLI   PCPRT,C'Y'          Result printed requested?
         BNE   PRTF1XT             No, bypass
*                                  Yes, call PRINTOFF
         CLC   CMPRRC2,=F'16'      RC < 16?
         BNL   PRTF1XT             No, bypass RC >= 16 !
         MOVEI TEXT                Build command
         MOVEV PRINTOFF            Move PRINTOFF as command
         MOVEC ' '
         STH   R8,OFFSET           Store OFFSET
         MOVEC ''''                Move '
         MOVER DDSN                      DDSN
         MOVEC ''''                          '
         LA    R8,4(R8)            Add PREFIX to Length
         STH   R8,CBUF             Store Buffer Length
*
         L     R2,MYCPPLP          R2=CPPL Address
         LA    R1,CBUF             R8=Addr of command buffer
         ST    R1,0(R2)            Store in first FW of CPPL
         LR    R1,R2               R1=parm for command
         MVC   COMMAND,PRINTOFF
         LINK  EP=PRINTOFF,ERRET=PGMNFND
         ST    R15,RCEXIT          Store R15
         LTR   R15,R15             RC=0?
         BZ    PRTF1X              Yes, continue...
         BAL   R7,DOERR1           Format General  Error
***      B     PRTF1XT
PRTF1X   EQU   *
         DELETE EP=PRINTOFF
PRTF1XT  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* FREE OUT DD optionally with DELETE                   */
*     * /********************************************************/
FREE02A  EQU   *
         CLI   PCBRWDEL,C'Y'       Delete BROWSE DSN?
         BNE   FREE02C             No, FREE w/o DELETE option
         LA    R5,TUPTR03D         Addr of Text-Unit-Pointer List
         B     FREE02L
FREE02C  EQU   *
         LA    R5,TUPTR003         Addr of Text-Unit-Pointer List
FREE02L  EQU   *
         MVI   R99VERB,S99VRBUN    Unallocation VERB
         BAL   R7,DYNALC           Issue SVC99
FREE02X  EQU   *
*
         EJECT
*     * /********************************************************/
*     * /* Get another request...                               */
*     * /********************************************************/
GETNOTHR EQU   *
         MVC   PMYCSR,PANCSR       Set PMYCSR
         MVC   PMYERRNO,PANMSG     Set PMYERRNO
         CLI   PANMSG,C' '         Message ?
         BE    CHK#PRO             Yes, continue...
         CLI   PRO,C'0'            Process PCOMPR panel ?
         BE    CHK#PRO             Yes, continue...
*                                  No, set message for next display
*        CALL  ISPLINK,(SETMSG,PANMSG),VL
         LBISPL ISPLINK,(SETMSG,PANMSG),VL
*        CALL  ISPLINK,(VPUT,VARLST3A,SHARED),VL
         LBISPL ISPLINK,(VPUT,VARLST3A,SHARED),VL
CHK#PRO  EQU   *
         CLI   PRO,C'0'            Process PCOMPR panel ?
         BNE   MYEXITF             No, return to caller
         B     GETDATA             Yes, go display PCOMPR
*
         EJECT
MYEXITF  EQU   *
*
*     * /********************************************************/
*     * /* FREE OUT DD optionally with DELETE                   */
*     * /********************************************************/
FREE03A  EQU   *
         CLI   PCBRWDEL,C'Y'       Delete BROWSE DSN?
         BNE   FREE03C             No, FREE w/o DELETE option
         LA    R5,TUPTR03D         Addr of Text-Unit-Pointer List
         B     FREE03L             Yes, FREE w/ DELETE option
FREE03C  EQU   *
         LA    R5,TUPTR003         Addr of Text-Unit-Pointer List
FREE03L  EQU   *
         MVI   R99VERB,S99VRBUN    Unallocation VERB
         BAL   R7,DYNALC           Issue SVC99
FREE03X  EQU   *
*
         EJECT
MYEXIT   EQU   *
*     * /********************************************************/
*     * /* Delete Functional Variables                          */
*     * /* - ISPEXEC VDELETE *                                  */
*     * /********************************************************/
*     * /* - ISPLINK VDELETE,namelist
*     * /********************************************************/
         CLI   VDEF,C'N'           Issued VDEFINEs?
         BE    BYPSVDEL            No, Bypass!
*                                  Yes, VDELETE-m!
         LBISPL ISPLINK,(VDELETE,ALL),VL
BYPSVDEL EQU   *
         DELETE EP=ISPLINK         Keep my pgms
         DELETE EP=CHKDSN             of the
         DELETE EP=COMPARE               load
         DELETE EP=PRINTOFF                 space...
*
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
         TITLE 'CCOMPR - Error Entry Points                           '
*     * /********************************************************/
*     * /* Error Setting                                        */
*     * /* - R7  Working Register                               */
*     * /* - R15 Working Register and RC exit code register     */
*     * /********************************************************/
ERR4090  EQU   *
         MVC   RCEXIT,=F'4090'     Command line start
         MVC   DJMSG+15(MSG4090L),MSG4090
         B     ERR#GO
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
PGMNFND  EQU   *
         ST    R15,RCEXIT          Store R15
         BAL   R7,DOERR1           Format General  Error
         MOVEI PMYMSG              Override PMYMSG
         MOVEC 'CC=xxxx Program not found - '
         MOVER COMMAND
         CVD   R1,DW              Convert Completion Code to decimal
         UNPK  PMYMSG+03(5),DW+5(3)    Unpack
         OI    PMYMSG+07,X'F0'        Force an F Zone
         MVC   DW(4),PMYMSG+4
         MVC   PMYMSG+03(04),DW          Use 4 bytes only
         MVI   PMYMSG+07,C' '         Blank last byte
         B     GETDATA             Display error, get input
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
         MVI   VDEF,C'Y'           Vdefine switch
*        CALL  ISPLINK,(VDEFINE,KEYPRESS,PKYPRS,CHAR,L4),VL
         LBISPL ISPLINK,(VDEFINE,MYPGM,PMYPGM,CHAR,LMYPGM),VL
         LBISPL ISPLINK,(VDEFINE,MYVRM,PMYVRM,CHAR,LMYVRM),VL
         LBISPL ISPLINK,(VDEFINE,MYMSG,PMYMSG,CHAR,LMYMSG),VL
         LBISPL ISPLINK,(VDEFINE,MYRC,PMYRC,CHAR,LMYRC),VL
         LBISPL ISPLINK,(VDEFINE,MYCMD,PMYCMD,CHAR,LMYCMD),VL
         LBISPL ISPLINK,(VDEFINE,MYCSR,PMYCSR,CHAR,LMYCSR),VL
         LBISPL ISPLINK,(VDEFINE,MYERRNO,PMYERRNO,CHAR,LMYERRNO),VL
*
         LBISPL ISPLINK,(VDEFINE,KEYPRESS,PKYPRS,CHAR,LKYPRS),VL
         LBISPL ISPLINK,(VDEFINE,RESP0,PRESP0,CHAR,LRESP0),VL
         LBISPL ISPLINK,(VDEFINE,LCMD,PLCMD,CHAR,LLCMD),VL
         LBISPL ISPLINK,(VDEFINE,ZCMD,PZCMD,CHAR,LZCMD),VL
         LBISPL ISPLINK,(VDEFINE,ODSN,PCODSN,CHAR,LCODSN),VL
         LBISPL ISPLINK,(VDEFINE,OMBR,PCOMBR,CHAR,LCOMBR),VL
         LBISPL ISPLINK,(VDEFINE,OVOL,PCOVOL,CHAR,LCOVOL),VL
         LBISPL ISPLINK,(VDEFINE,OUNT,PCOUNT,CHAR,LCOUNT),VL
         LBISPL ISPLINK,(VDEFINE,NDSN,PCNDSN,CHAR,LCNDSN),VL
         LBISPL ISPLINK,(VDEFINE,NMBR,PCNMBR,CHAR,LCNMBR),VL
         LBISPL ISPLINK,(VDEFINE,NVOL,PCNVOL,CHAR,LCNVOL),VL
         LBISPL ISPLINK,(VDEFINE,NUNT,PCNUNT,CHAR,LCNUNT),VL
         LBISPL ISPLINK,(VDEFINE,CFULL,PCFULL,CHAR,LCFULL),VL
         LBISPL ISPLINK,(VDEFINE,CASM,PCASM,CHAR,LCASM),VL
         LBISPL ISPLINK,(VDEFINE,CNOASM,PCNOASM,CHAR,LCNOASM),VL
         LBISPL ISPLINK,(VDEFINE,CIEBCMPR,PCIBCMPR,CHAR,LCIBCMPR),VL
         LBISPL ISPLINK,(VDEFINE,CZEBCMPR,PCZBCMPR,CHAR,LCZBCMPR),VL
         LBISPL ISPLINK,(VDEFINE,CCOMPARB,PCCMPARB,CHAR,LCCMPARB),VL
         LBISPL ISPLINK,(VDEFINE,CBRW,PCBRW,CHAR,LCBRW),VL
         LBISPL ISPLINK,(VDEFINE,CPRT,PCPRT,CHAR,LCPRT),VL
         LBISPL ISPLINK,(VDEFINE,CBRWDEL,PCBRWDEL,CHAR,LCBRWDEL),VL
         L     R14,SAVER14S
         BR    R14
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine -                                  (R14)  */
*     * /*  ISPEXEC VGET (nnnnn nnnnnnn .......) PROFILE        */
*     * /*  ISPEXEC VGET (nnnnn nnnnnnn .......) SHARED         */
*     * /* - R15, R1    Working Register                        */
*     * /********************************************************/
ISPVGET  EQU   *
         ST    R14,SAVER14S
         LBISPL ISPLINK,(VGET,VGETV1,PROFILE),VL
         LBISPL ISPLINK,(VGET,VGETV2,PROFILE),VL
         LBISPL ISPLINK,(VGET,VGETV3,PROFILE),VL
         LBISPL ISPLINK,(VGET,VGETV4,PROFILE),VL
         CLI   PRO,C'0'            Process PCOMPR panel ?
         BE    ISPVGETX            Yes, continue...
*                                  No, Get shared vars
         LBISPL ISPLINK,(VGET,VARLST1A,SHARED),VL
ISPVGETX EQU   *
         L     R14,SAVER14S
         BR    R14
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine -                                  (R14)  */
*     * /*  ISPF Error Display and return                       */
*     * /*      caller or MYEXIT                                */
*     * /* - RCEXIT     on entry                                */
*     * /* - R15, R1    Working Register                        */
*     * /********************************************************/
ISPERR   EQU   *
         ST    R14,SAVER14S
         L     R15,RCEXIT
         LTR   R15,R15             RC=0 (Process)
         BZ    ISPERRX             Yes, return to caller
*                                  No, display error and exit
         MVC   DJMSG(L'DSPLERR0),DSPLERR0
         L     R1,ISPFP1           ISPF Service Requested
         MVC   DJMSG+03(08),0(R1)
         MVC   DJMSG+11(06),B      6 Blanks
         CVD   R15,DW              Convert to decimal
         UNPK  DJMSG+20(05),DW+5(03) Unpack and
         OI    DJMSG+24,X'F0'      ... force and F zone
         MVI   DJMSG+20,C'='       Restore equal sign
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
         TPUT  DJMSG,L'DSPLERR0
         LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
         LA    R14,MYEXIT          R14=Addr MYEXIT
         B     ISPERRXT            Return to MYEXIT
*
ISPERRX  EQU   *
         L     R14,SAVER14S
ISPERRXT EQU   *
         BR    R14
*
         EJECT
         RTRIM
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Assemble Temp DSN                (R14)  */
*     * /* - R2, R3, R6, R8, R5  Working Registers              */
*     * /* - R0, R1  (TPUT)      Working Registers              */
*     * /********************************************************/
TEMPDSN  EQU   *
         ST    R14,SAVER14S        Save R14
*     * /*------------------------------------------------------*/
*     * /* Establish USERID from TSO PREFIX or TSO USERID       */
*     * /*------------------------------------------------------*/
         MVI   DPFXL,X'00'         Initialize Dataset Name Prefix
         MVI   DPFX,C' '               length and
         MVC   DPFX+1(L'DPFX-1),DPFX     variable
GDSNPRF  EQU   *
         L     R2,MYCPPLP          Point to myCPPL
         USING CPPL,R2             Tell Assembler, CPPL
         L     R3,CPPLUPT          Point to UPT
         DROP  R2
         USING UPT,R3              Tell Assebler, UPT
         SLR   R8,R8               Clear R8
         IC    R8,UPTPREFL         PREFIX Length
         LTR   R8,R8               Length = 0?
         BZ    GDSNUSR             Yes, try USERID
         MVC   DPFXL,UPTPREFL      PREFIX Length
         MVC   DPFX(L'UPTPREFX),UPTPREFX  PREFIX
*        MVC   TEXT(13),=C'Using prefix='
*        MVC   TEXT+13(08),DPFX
*        STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
*        TPUT  TEXT,21
*        LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
         DROP  R3
         B     GDSNPRFX            Done, continue...
GDSNUSR  EQU   *
*                                  Get USERID from PSCB
         L     R2,MYCPPLP          Point to myCPPL
         USING CPPL,R2             Tell Assembler, CPPL
         L     R3,CPPLPSCB         Point to PSCB
         DROP  R2
         USING PSCB,R3             Tell Assebler, PSCB
         SLR   R8,R8               Clear R8
         IC    R8,PSCBUSRL         USERID Length
         LTR   R8,R8               Length = 0?
         BZ    NOUSRIDP            Yes, no DSN Prefix
         MVC   DPFXL,PSCBUSRL      USERID Length
         MVC   DPFX(L'PSCBUSER),PSCBUSER  USERID
*        MVC   TEXT(13),=C'Using userid='
*        MVC   TEXT+13(08),DPFX
*        STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
*        TPUT  TEXT,21
*        LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
         B     GDSNPRFX            Done, continue...
NOUSRIDP EQU   *
         MOVEI DJMSG
         MOVEC 'UserID cannot be determined. '
         MOVEC 'Process ended.'
         TPUT  DJMSG,(R8)
         B     MYEXIT              Exit
GDSNPRFX EQU   *
         DROP  R3
*
         EJECT
*     * /*------------------------------------------------------*/
*     * /* Initialize Results DSN using time stamp format       */
*     * /* - USERID.COMPARE.Dyyjjj.Thhmmss                      */
*     * /* - R6, R8, R1       Working Register                  */
*     * /* - R0, R1  (TPUT)   Working Register                  */
*     * /*------------------------------------------------------*/
DODSN    EQU   *
         MOVEI DDSN                Initialize DDSN
         MOVER DPFX                Move Prefix
         IC    R8,DPFXL            Store DSN Prefix Length
         MOVEC '.'                 Add dot after DSN Prefix
NOUSRPX  EQU   *
         MOVEV COMPARE,7           Move 'COMPARE'   to DDSN
         MOVEC '.D'                Move '.D'        to DDSN
*                                  Get current date and time
*                                  Time in R0  HH_MM_SS_TH
*                                  Date in R1  II_YY_DD_DC
         TIME  DEC
         ST    R1,WORKAREA         Date in R1  II_YY_DD_DC
         UNPK  UNPKAREA(7),WORKAREA+0(4)     Unpack IIYYDDD
         OI    UNPKAREA+6,X'F0'            F-zone   IIYYDDD
         MOVEV UNPKAREA+2,5   ***  Move YYDDD to DDSN
         MOVEC '.T'                Move '.T' to DDSN
         ST    R0,WORKAREA         Time in R0  HH_MM_SS_TH
         MVO   DW(5),WORKAREA      Pack via MVO into 5 bytes
         OI    DW+4,X'0F'          ... with an F zone
         UNPK  UNPKAREA(9),DW(5)   Unpack 9 bytes 0HHMMSSTH
         MOVEV UNPKAREA+1,6   ***  Move HHMMSS to DDSN
         STH   R8,DDSNL            Store DDSN length
SHOWDSN  EQU   *
         STM   R0,R1,DW            SAVE    R0,R1  to   DW before TPUT
         SR    R1,R1
         LH    R1,DDSNL
         TPUT  DDSN,(R1)
         LM    R0,R1,DW            RESTORE R0,R1  from DW after  TPUT
TEMPDSNX EQU   *
         L     R14,SAVER14S        Restore R14
         BR    R14                 Return
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Link to DYNALLOC                 (R7)   */
*     * /* - R5, R6 Working Registers                           */
*     * /* - R15 Return Code on exit                            */
*     * /* - R99VERB must contain S99VERB (on entry)            */
*     * /* - R5      must contain TUP List       (on entry)     */
*     * /********************************************************/
DYNALC   EQU   *
         ST    R7,SAVER7S          Save R7 Subroutine
         USING S99RBP,R5   10      Tell Assembler, Req Blk Ptr
         USING S99RB,R6    11      Tell Assembler, Req Blk
*                                  Setup SVC99 Req Blk
         LA    R6,REQBLK           Addressability REQBLK DSECT
         XC    REQBLK,REQBLK       Clear Req Blk
         MVI   S99RBLN,REQBLKLN    Req Blk Length
         MVC   S99VERB,R99VERB     S99 VERB
         ST    R5,S99TXTPP         Store into Req Blk
*                                  Setup SVC99 Req Blk Ptr
         LA    R5,RBPTR            Addressability RBPTR DSECT
         LA    R6,REQBLK           Load Addr of REQBLK
         ST    R6,S99RBPTR         Store into Req Blk Ptr
         OI    S99RBPTR,S99RBPND   Turn on high order bit
*                                  Issue DYNALLOC (SVC99)
         LA    R1,RBPTR            Load parm in R1
         DYNALLOC                  Issue SVC99
         MVC   R99RSC,S99RSC
         DROP  R5
         DROP  R6
         L     R7,SAVER7S          Restore R7 Subroutine
         BR    R7                  Return to caller
*
         EJECT
*     * /********************************************************/
*     * /* Subroutine - Error message CMPR018            (R7)   */
*     * /* - DW        Working Variable                         */
*     * /* - PMYRC     Working Variable (output)                */
*     * /* - PMYMSG    Working Variable (output)                */
*     * /* - PANMSG    Working Variable (output)                */
*     * /* - PANCSR    Working Variable (output)                */
*     * /* - R15 Return Code (on entrance)                      */
*     * /********************************************************/
DOERR1   EQU   *                   General ERR information
         MVI   ERRX,C'1'
         B     DOERRS
DOERR2   EQU   *                   Dynalloc ERR information
         MVI   ERRX,C'2'
         B     DOERRS
DOERRS   EQU   *
         ST    R7,SAVER7S          Save R7 Subroutine
         L     R15,RCEXIT
         CVD   R15,DW              Convert to decimal
         UNPK  PMYRC(5),DW+5(3)    Unpack
         OI    PMYRC+04,X'F0'        Force an F Zone
         MVC   DW(4),PMYRC+1
         MVC   PMYRC(04),DW          Use 4 bytes only
         MVI   PMYRC+04,C' '         Blank last byte
         MVI   PMYMSG,C' '         Clear PMYMSG
         MVC   PMYMSG+1(L'PMYMSG-1),PMYMSG
         CLI   ERRX,C'2'
         BE    DOR99
         MVC   PMYMSG(21),=C'from program-12345678'
         MVC   PMYMSG+13(8),COMMAND
         B     DOERRX              Return to caller
DOR99    EQU   *
         LA    R15,0               Clear R15
         LH    R15,R99ERROR        Load Error HW
         CVD   R15,DW              Convert to decimal
         MVC   R99MSG(3),=C'ERR'
         UNPK  R99MSG+03(5),DW+5(3)  Unpack
         OI    R99MSG+07,X'F0'       Force an F Zone
         MVI   R99MSG+03,C'='        Replace '=' sign
         LA    R15,0               Clear R15
         LH    R15,R99INFO         Load Info HW
         CVD   R15,DW              Convert to decimal
         MVC   R99MSG+08(5),=C' INFO'
         UNPK  R99MSG+13(5),DW+5(3)  Unpack
         OI    R99MSG+17,X'F0'       Force an F Zone
         MVI   R99MSG+13,C'='        Replace '=' sign
         MVC   PMYMSG(L'ERRTXT),ERRTXT
         MVC   PMYMSG+L'ERRTXT(L'R99MSG),R99MSG
DOERRX   EQU   *
         MVC   PANMSG,=C'CMPR018 ' Error
         MVC   PANCSR,=C'ODSN    ' Panel Cursor
         L     R7,SAVER7S          Restore R7 Subroutine
         BR    R7                  Return to caller
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
         L     R7,SAVER7S          Restore R7 Subroutine
         BR    R7                  Return to caller
*
         TITLE 'CCOMPR - Literal Pool                                 '
         LTORG
*
         TITLE 'CCOMPR - Constants                                    '
         ISPFSRV                           ISPF Service Constants
         EJECT
*     * /********************************************************/
*     * /* ISPF Constants for services                          */
*     * /********************************************************/
PANID    DC    CL8'PCOMPR  '               Panel ID
LINENO   DC    CL8'22      '               Line number
VGETV1   DC    C'(ODSN OMBR OVOL OUNT)'             VGET List 1
VGETV2   DC    C'(NDSN NMBR NVOL NUNT)'             VGET List 2
VGETV3   DC    C'(CFULL CASM CNOASM CIEBCMPR)'      VGET List 3
VGETV4   DC    C'(CZEBCMPR CCOMPARB CBRW CPRT CBRWDEL)'  VGET List 4
VARLST1A DC    C'(KEYPRESS RESP0 LCMD)'             VGET 1
VARLST3A DC    C'(MYPGM MYVRM MYMSG MYRC MYCMD MYCSR MYERRNO)' VPUT 1
*                                                           /
         EJECT
*     * /********************************************************/
*     * /* ISPF Variable Names                                  */
*     * /********************************************************/
MYPGM    DC    CL8'MYPGM   '       VarName for PMYPGM
MYVRM    DC    CL8'MYVRM   '       VarName for PMYVRM
MYMSG    DC    CL8'MYMSG   '       VarName for PMYMSG
MYRC     DC    CL8'MYRC    '       VarName for PMYRC
MYCMD    DC    CL8'MYCMD   '       VarName for PMYCMD
MYCSR    DC    CL8'MYCSR   '       VarName for PMYCMD
MYERRNO  DC    CL8'MYERRNO '       VarName for PMYCMD
*
KEYPRESS DC    CL8'KEYPRESS'       Varname for PKYPRS
RESP0    DC    CL8'RESP0   '       Varname for PRESP0
LCMD     DC    CL8'LCMD    '       VarName for PLCMD
ZCMD     DC    CL8'ZCMD    '       VarName for ZCMD
ODSN     DC    CL8'ODSN    '       VarName for PODSN
OMBR     DC    CL8'OMBR    '       VarName for POMBR
OVOL     DC    CL8'OVOL    '       VarName for POVOL
OUNT     DC    CL8'OUNT    '       VarName for POUNT
NDSN     DC    CL8'NDSN    '       VarName for PNDSN
NMBR     DC    CL8'NMBR    '       VarName for PNMBR
NVOL     DC    CL8'NVOL    '       VarName for PNVOL
NUNT     DC    CL8'NUNT    '       VarName for PNUNT
CFULL    DC    CL8'CFULL   '       VarName for PCFULL
CASM     DC    CL8'CASM    '       VarName for PCASM
CNOASM   DC    CL8'CNOASM  '       VarName for PCNOASM
CIEBCMPR DC    CL8'CIEBCMPR'       VarName for PIEBCMPR
CZEBCMPR DC    CL8'CZEBCMPR'       VarName for PZEBCMPR
CCOMPARB DC    CL8'CCOMPARB'       VarName for PCOMPARB
CBRW     DC    CL8'CBRW    '       VarName for PCBRW
CPRT     DC    CL8'CPRT    '       VarName for PCPRT
CBRWDEL  DC    CL8'CBRWDEL '       VarName for PCBRWDEL
*
         EJECT
*     * /********************************************************/
*     * /* ISPF Variable Names Lengths                          */
*     * /********************************************************/
         DS    0F
LMYPGM   DC    A(L'PMYPGM)         PMYPGM
LMYVRM   DC    A(L'PMYVRM)         PMYVRM
LMYMSG   DC    A(L'PMYMSG)         PMYMSG
LMYRC    DC    A(L'PMYRC)          PMYRC
LMYCMD   DC    A(L'PMYCMD)         PMYCMD
LMYCSR   DC    A(L'PMYCSR)         PMYCSR
LMYERRNO DC    A(L'PMYERRNO)       PMYERRNO
*
LKYPRS   DC    A(L'PKYPRS)         PKYRS
LRESP0   DC    A(L'PRESP0)         PRESP0
LLCMD    DC    A(L'PLCMD)          PLCMD (ZCMD)
LZCMD    DC    A(L'PZCMD)          PZCMD
LCODSN   DC    A(L'PCODSN)         PCODSN
LCOMBR   DC    A(L'PCOMBR)         PCOMBR
LCOVOL   DC    A(L'PCOVOL)         PCOVOL
LCOUNT   DC    A(L'PCOUNT)         PCOUNT
LCNDSN   DC    A(L'PCNDSN)         PCNDSN
LCNMBR   DC    A(L'PCNMBR)         PCNMBR
LCNVOL   DC    A(L'PCNVOL)         PCNVOL
LCNUNT   DC    A(L'PCNUNT)         PCNUNT
LCFULL   DC    A(L'PCFULL)         PCFULL
LCASM    DC    A(L'PCASM)          PCASM
LCNOASM  DC    A(L'PCNOASM)        PCNOASM
LCIBCMPR DC    A(L'PCIBCMPR)       PCIBCMPR
LCZBCMPR DC    A(L'PCZBCMPR)       PCZBCMPR
LCCMPARB DC    A(L'PCCMPARB)       PCCOMPARB
LCBRW    DC    A(L'PCBRW)          PCBRW
LCPRT    DC    A(L'PCPRT)          PCPRT
LCBRWDEL DC    A(L'PCBRWDEL)       PCBRWDEL
*
         EJECT
*     * /********************************************************/
*     * /* Local Messages                                       */
*     * /********************************************************/
MSG4090  DC    C'Command Line start required'
MSG4090L EQU   *-MSG4090
MSG4096  DC    C'ISPLINK  not found, link error'
MSG4096L EQU   *-MSG4096
MSG4099  DC    C'Program requires TSO'
MSG4099L EQU   *-MSG4099
*
DSPLERR0 DC    C'** xxxxxxxxxxxxxx RC=xxxx, Process Ended!'
*
*     * /********************************************************/
*     * /* Misc Contants                                        */
*     * /********************************************************/
COMPARE  DC    C'COMPARE '         Compare  pgm name
PRINTOFF DC    C'PRINTOFF'         Printoff pgm name
         EJECT
*     * /********************************************************/
*     * /* Text Units Pointer List                              */
*     * /********************************************************/
*        Allocation of OUT DD
*        Equivalent to ALLOC FI(DD) NEW CATALOG UNIT(SYSDA)
*                      BLKSZ(12100) LRECL(121) RECFM(F B A)
*                      TRACKS SP(10, 5) DA('dsn')
TUPTR001 DC    A(TUDDNA1)                              DD
         DC    A(TUDSNA1)                              DSN
         DC    A(TUDSSA1)                              NEW
         DC    A(TUDNDA1)                              CATALOG
         DC    A(TUBLKA1)                              BLKSZ
         DC    A(TULRLA1)                              LRECL
         DC    A(TURFMA1)                              FBA
         DC    A(TUTRKA1)                              TRACKS
         DC    A(TUPRMA1)                              10
         DC    A(TUSECA1)                              05
         DC    X'80'                                   *LAST TU ADDR
         DC    AL3(TUDSOA1)                            PS
TUPTR0L1 EQU   *-TUPTR001
*
*        Unallocation of OUT DD
*        Equivalent to FREE FI(DD)
TUPTR003 DS    0F
         DC    X'80'                                   *LAST TU ADDR
         DC    AL3(TUDDNU1)                            DD
TUPTR0L3 EQU   *-TUPTR003
*
*        Unallocation of OUT DD with DELETE option
*        Equivalent to FREE FI(DD) DELETE
TUPTR03D DS    0F
         DC    A(TUDDNU1)                              DD
         DC    X'80'                                   *LAST TU ADDR
         DC    AL3(TUDNDA3D)                           DELETE
TUPTRL3D EQU   *-TUPTR03D
*
         EJECT
*     * /********************************************************/
*     * /* Text Units for OUT DD Allocation                     */
*     * /********************************************************/
TUDDNA1  DC    AL2(DALDDNAM),AL2(1),AL2(8),CL8'OUT'    DD
TUDSNA1  DC    AL2(DALDSNAM),AL2(1),AL2(44)            DSN
         DC    CL44' '
TUDSNA1L EQU   *-TUDSNA1
*TUDSSA1  DC    AL2(DALSTATS),AL2(1),AL2(1),X'04'       NEW
TUDSSA1  DC    AL2(DALSTATS),AL2(1),AL2(1),X'02'       MOD
TUDNDA1  DC    AL2(DALNDISP),AL2(1),AL2(1),X'02'       CATALOG
TUUNTA1  DC    AL2(DALUNIT),AL2(1),AL2(5),C'SYSDA'     UNIT
TUBLKA1  DC    AL2(DALBLKSZ),AL2(1),AL2(2),AL2(12100)  BLKSZ
TULRLA1  DC    AL2(DALLRECL),AL2(1),AL2(2),AL2(121)    LRECL
TURFMA1  DC    AL2(DALRECFM),AL2(1),AL2(1),X'94'       FBA
TUTRKA1  DC    AL2(DALTRK),AL2(0)                    TRACKS
TUPRMA1  DC    AL2(DALPRIME),AL2(1),AL2(3),AL3(10)     10
TUSECA1  DC    AL2(DALSECND),AL2(1),AL2(3),AL3(05)     05
TUDSOA1  DC    AL2(DALDSORG),AL2(1),AL2(2),X'4000'     PS
*
*     * /********************************************************/
*     * /* Text Units for OUT DD Unallocation                   */
*     * /********************************************************/
TUDDNU1  DC    AL2(DUNDDNAM),AL2(1),AL2(3),C'OUT'      DD
TUDNDA3D DC    AL2(DUNOVDSP),AL2(1),AL2(1),X'04'       DELETE
*
         TITLE 'CCOMPR - DCB for OUTFIL                               '
*     * /********************************************************/
*     * /* OUTFIL DCB                                           */
*     * /********************************************************/
         DS    0F
OPENO    OPEN  (OUTFIL,(OUTPUT)),MF=L
OPENOL   EQU   *-OPENO
*
         DS    0F
CLOSEO   CLOSE (OUTFIL),MF=L
CLOSEOL  EQU   *-CLOSEO
*
         DS    0F
OUTFIL   DCB   DSORG=PS,MACRF=(PM),                                    X
               LRECL=121,                                              X
               DDNAME=OUT
*              LRECL=80,BLKSIZE=3200,RECFM=FB,
OUTFILL  EQU   *-OUTFIL
*
         TITLE 'CCOMPR - Equates                                      '
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
         TITLE 'CCOMPR - System DSECTs                                '
*     *
*     * /********************************************************/
*     * /* System DSECTs                                        */
*     * /********************************************************/
         IHAPSA                    Prefixed Save Area
PSALEN   EQU   *-PSA               Length of PSA
*        EJECT
         CVT   DSECT=YES,LIST=YES  Communication Vector Table
*        EJECT
         IKJTCB DSECT=YES,LIST=YES Task Control Block
*        EJECT
         IKJPSCB                   Protected Step Control Block
*        EJECT
         IHAASCB                   Address Space Control Block
ASCBLEN  EQU   *-ASCB              Length of ASCB
*        EJECT
         IKJCPPL                   Command Processor Parm List
CPPLLEN  EQU   *-CPPL              Length of CPPL
*        EJECT
         IKJUPT                    User Profile Table
UPTLEN   EQU   *-UPT               Length of UPT
*        EJECT
         IKJPPL                    Parse Parm List
PPLLEN   EQU   *-PPL               Length of PPL
*        EJECT
         IKJIOPL                   I/O Service Routine Parm List
IOPLLEN  EQU   *-IOPL              Length of IOPL
*        EJECT
         IEFZB4D0                  S99 Parm List
         EJECT
         IEFZB4D2                  S99 Text Unit Keys
*
         TITLE 'CCOMPR - Working Storage Variables                    '
*     * /********************************************************/
*     * /* Working Storage  DSECT                               */
*     * /********************************************************/
WSDSECT  DSECT
WSAREA   EQU   *
*
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
PMYPGM   DS    CL8                 MYPGM
PMYVRM   DS    CL8                 MYVRM
PMYMSG   DS    CL50                MYMSG
PMYRC    DS    CL5                 MYRC
PMYCMD   DS    CL100               MYCMD
PMYCSR   DS    CL8                 MYCSR
PMYERRNO DS    CL8                 MYERRNO
*
PKYPRS   DS    CL4                 KEYPRESS
PRESP0   DS    CL8                 RESP0
PLCMD    DS    CL50                LCMD (ZCMD)
PZCMD    DS    CL50                ZCMD
PCODSN   DS    CL46                ODSN w/ single quotes
PCOMBR   DS    CL8                 OMBR
PCOVOL   DS    CL6                 OVOL
PCOUNT   DS    CL4                 OUNT
PCNDSN   DS    CL46                NDSN w/ single quotes
PCNMBR   DS    CL8                 NMBR
PCNVOL   DS    CL6                 NVOL
PCNUNT   DS    CL4                 NUNT
PCFULL   DS    CL1                 CFULL
PCASM    DS    CL1                 CASM
PCNOASM  DS    CL1                 CNOASM
PCIBCMPR DS    CL1                 CIEBCMPR
PCZBCMPR DS    CL1                 CZEBCMPR
PCCMPARB DS    CL1                 CCOMPARB
PCBRW    DS    CL1                 CBRW
PCPRT    DS    CL1                 CPRT
PCBRWDEL DS    CL1                 CBRWDEL
*
         EJECT
*     * /********************************************************/
*     * /* Command buffer area                                  */
*     * /********************************************************/
COMMAND  DS    CL8
CBUF     DS    0D
CBUF#L   DS    H                   Buffer Length
OFFSET   DS    H                   Offset to operands
TEXT     DS    CL255               COMMAND
*
*     * /********************************************************/
*     * /* Program Variables                                    */
*     * /********************************************************/
CMPRRC2  DS    F                   Compare RC
PANMSG   DS    CL8                 Panel Message Number
PANCSR   DS    CL8                 Panel Cursor Field
PCTYPE   DS    CL5                 Compare Type for COMPARE
PCPGM    DS    CL8                 Compare Pgm  for COMPARE
WORKAREA DS    F                   Workarea
UNPKAREA DS    CL16                Workarea for unpacking data
ERRTXT   DS    CL17                Error Text msg
ERRX     DS    CL1                 Error Text type 1- General ERR
*                                                  2- DYNALL ERR
*
         EJECT
*     * /********************************************************/
*     * /* S99 Req-Blk-Ptr and Req Blk                          */
*     * /********************************************************/
RBPTR    DS    F
REQBLK   DS    CL(S99RBEND-S99RB)
REQBLKLN EQU   L'REQBLK
*
*     * /********************************************************/
*     * /* S99 Work Areas                                       */
*     * /********************************************************/
R99VERB  DS    CL1                 SVC99 VERB used by DYNALC sub
         DS    0F
R99RSC   DS    0CL4                REASON CODE FIELDS
R99ERROR DS    XL2                 ERROR REASON CODE
R99INFO  DS    XL2                 INFORMATION REASON CODE
R99MSG   DS    CL18
*
RUPTR001 DS    18F                 Workarea for text unit parm list
RUDSNA1  DS    AL2(DALDSNAM),AL2(1),AL2(44)      ** Must match
RUDSN    DS    CL44' '                           **   TUDSNA1
*
         EJECT
*     * /********************************************************/
*     * /* Dataset  Variable Data Area                          */
*     * /********************************************************/
DPFXL    DS    CL1                 DSN PREFIX   Length (must be here
*                                  place before Dataset Name DDSN)
DPFX     DS    CL08                DSN PREFIX
*
DDSNL    DS    0F,H                Dataset Name Length (must be here
*                                  place before Dataset Name DDSN)
DDSN     DS    CL44                Dataset Name
*
*     * /********************************************************/
*     * /* Misc Variables                                       */
*     * /********************************************************/
SAVER15  DS    F                   R15 Hold
SAVER15M DS    F                   R15 Hold for Message Processing
SAVER3S  DS    F                   R3  Save in Subroutines
SAVER7S  DS    F                   R7  Save in Subroutines
SAVER14S DS    F                   R14 Save in Subroutines
DW       DS    D                   Double Word area
FW       DS    F                   Full Word   area
RCEXIT   DS    F                   Exit RC
MYCPPLP  DS    F                   CPPL Address
DJMSG    DS    CL100               MY MESSAGE VARIABLE
BDSN     DS    CL56                Browse Dataset Name
AMITSO   DS    C                   TSO Flag         Y-TSO FG
*                                                   N-TSO BG
PRO      DS    C                   Process Switch   0-Panel / Vars
*                                                   1-Vars only
VDEF     DS    C                   Vdefine switch   Y-VDEFINEs issued
*                                                   N-No VDEFINEs
*
*     * /********************************************************/
*     * /* USERID Prefix Workarea                               */
*     * /********************************************************/
IDPREFIX DS    C                   Prefix UserID    Y-Yes     N-No
MYIDL    DS    H                   Prefix UserID Length
MYID     DS    CL8                 Prefix UserID
*
*     * /********************************************************/
*     * /* OUTFIL Workarea                                      */
*     * /********************************************************/
PREC     DS    CL121               OUT file record area
*
         DS    0F
WOUTFIL  DS    CL(OUTFILL)         DCB
*
         DS    0F
WOPENO   DS    CL(OPENOL)          OPEN
*
         DS    0F
WCLOSEO  DS    CL(CLOSEOL)         CLOSE
*
         DS    0F,(40)X
WSAREAE  EQU   *                   Working Storage END
WSAREAL  EQU   WSAREAE-WSAREA      Working Storage Area Length
         EJECT
         END   CCOMPR
@@
//LKED.SYSLMOD DD  DISP=SHR,DSN=SYSGEN.ISPF.LLIB(CCOMPR)
//PLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.PLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=HCOMPR
/********************************************************************/
/*                                                                  */
/*    PANEL: HCOMPR                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/CCompare-in-MVS38J             */
/*         Copyright (C) 2020  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Help panel for PCOMPR                                   */
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
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 02/12/2022 0.9.01   Larry Belmontes Jr.                          */
/*                     - Updated ZEBCOMPR CBT Tape number typo      */
/*                       and added README.TXT ZAP message.          */
/*                                                                  */
/* 08/10/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
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
%--HELP------------------ COMPARE TWO ONLINE DATASETS ---/-/-
%Command ===>_ZCMD                                       / /%
%                                                                     +&ZPANELID
+This function is a front-end program for the~COMPARE+CP.~COMPARE+will detect
+differences between two datasets (sequential, PDS with matching members, or
+two PDS members).
+
+The~COMPARE+CP executes one of three compare programs,~IEBCOMPR, ZEBCOMPR
+or~COMPAREB+(Yale Compare), to perform the actual compare task. IEBCOMPR and
+ZEBCOMPR must be installed on MVS 38J system. The ZEBCOMPR zap can be found
+on CBT File 316. See README.TXT for ZEBCOMPR ZAP for MVS 38J.  The COMPARE
+CP can be found on CBT File 296.
+
+In addition,~COMPARE+can be instructed to perform a~FULL, ASM+and~NOASM+type
+of compare (columns and data to compare).
+
+Compare results are displayed in a%BROWSE+session. After browsing, results are
+optionally~deleted+and/or~printed+to SYSOUT CLASS A, if selected from request
+panel.
+
+Use%HELP COMPARE+for full description of~COMPARE+program and parameters used
+in this ISPF front-end panel.
+
+ ~&MYPGM  ~&MYVRM
)INIT
 .CURSOR = ZCMD

)PROC

)END
./ ADD NAME=PCOMPR
/********************************************************************/
/*                                                                  */
/*    PANEL: PCOMPR                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/CCompare-in-MVS38J             */
/*         Copyright (C) 2020  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Compare panel for CP  CCOMPR 0                          */
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
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 08/10/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
 %   TYPE(TEXT) INTENS(HIGH)
 @   TYPE(TEXT) INTENS(LOW)
 _   TYPE(INPUT) INTENS(HIGH) CAPS(ON) JUST(LEFT)
 !   TYPE(OUTPUT) INTENS(LOW) CAPS(OFF) JUST(ASIS)
 $   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(RED)
 [   TYPE(TEXT)   INTENS(HIGH)                      COLOR(TURQ)
 [   TYPE(TEXT  ) INTENS(HIGH)                      COLOR(YELLOW)
 }   TYPE(TEXT)   INTENS(HIGH)                      COLOR(GREEN)
 {   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(GREEN)
 ?   TYPE(TEXT)   INTENS(LOW)  HILITE(REVERSE) COLOR(BLUE)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%------------------------?  COMPARE TWO ONLINE DATASETS  %----------------------
%Command ===>_ZCMD                                          +
%
%                                                                      !Z
+[OLD+DSN%===>_ODSN                                          +         !Z
+  Member%===>_OMBR    +
+  Volume%===>_OVOL  +  (If not catalogued)
+    Unit%===>_OUNT+    (Always BLANK)
+
+[NEW+DSN%===>_NDSN                                          +
+  Member%===>_NMBR    +
+  Volume%===>_NVOL  +  (If not catalogued)
+    Unit%===>_NUNT+    (Always BLANK)
+
+[Compare Options:
+[ Type   :+FULL_Z+ASM_Z+NOASM_Z+               (Use 'x' to select one Type)
+[ Program:+COMPAREB_Z+IEBCOMPR_Z+ZEBCOMPR_Z+   (Use 'x' to select one Program)
+[ Results:+Browse_Z+Print_Z+Delete_Z+          (Y/N for each results action)
+
)INIT
  .ZVARS = '(ZUSER ZPANELID
             CFULL CASM CNOASM
             CCOMPARB CIEBCMPR CZEBCMPR
             CBRW CPRT CBRWDEL)'
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  .HELP = HCOMPR
  &ZCMD = '*'

  VGET (ODSN OMBR OVOL OUNT) PROFILE
  VGET (NDSN NMBR NVOL NUNT) PROFILE
  VGET (CFULL CASM CNOASM CIEBCMPR) PROFILE
  VGET (CZEBCMPR CCOMPARB CBRW CPRT CBRWDEL) PROFILE

)REINIT
  &ZCMD = '*'
  REFRESH(*)          /* refresh all fields */
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY

)PROC
  &RESP0 = .RESP
  &KEYPRESS = .PFKEY
  &LCMD = &ZCMD
/*IF (&KEYPRESS NE PF03)
/*IF (&ZCMD NE 'END', 'RETURN')
  IF (&ZCMD EQ '*')
    VER (&ODSN,NONBLANK,DSNAME,MSG=CMPR010)
    VER (&NDSN,NONBLANK,DSNAME,MSG=CMPR010)
    VER (&CFULL,LIST, ,X,MSG=CMPR008)
    VER (&CASM,LIST, ,X,MSG=CMPR008)
    VER (&CNOASM,LIST, ,X,MSG=CMPR008)
    VER (&CIEBCMPR,LIST, ,X,MSG=CMPR008)
    VER (&CZEBCMPR,LIST, ,X,MSG=CMPR008)
    VER (&CCOMPARB,LIST, ,X,MSG=CMPR008)
    VER (&CBRW,NONBLANK,MSG=CMPR012)
    VER (&CBRW,LIST,Y,N,MSG=CMPR012)
    VER (&CPRT,NONBLANK,MSG=CMPR005)
    VER (&CPRT,LIST,Y,N,MSG=CMPR005)
    VER (&CBRWDEL,NONBLANK,MSG=CMPR006)
    VER (&CBRWDEL,LIST,Y,N,MSG=CMPR006)
    VPUT (ODSN OMBR OVOL OUNT) PROFILE
    VPUT (NDSN NMBR NVOL NUNT) PROFILE
    VPUT (CFULL CASM CNOASM CIEBCMPR) PROFILE
    VPUT (CZEBCMPR CCOMPARB CBRW CPRT CBRWDEL) PROFILE

)END
@@
//CLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.CLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=C$CCOMPR
PROC 0

/********************************************************************/
/*                                                                  */
/*    CLIST: C$CCOMPR                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/CCompare-in-MVS38J             */
/*         Copyright (C) 2020  Larry Belmontes, Jr.                 */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* IVP CLIST to validate   CCOMPR                                   */
/*                                                                  */
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
/* ================================================================ */
/* MM/DD/CCYY VERSION  NAME / DESCRIPTION                           */
/* ---------- -------  -------------------------------------------- */
/* 08/10/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/


/* ------------------------------------------------------ */
/* ISPEXEC SELECT 'CCOMPR 0' NEWAPPL(CMPR)                */
/* ------------------------------------------------------ */
ISPEXEC SELECT CMD(CCOMPR 0) NEWAPPL(CMPR)


END

@@
//MLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.MLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CMPR00
/********************************************************************/
/*                                                                  */
/* MESSAGES: CMPR00                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/CCompare-in-MVS38J             */
/*         Copyright (C) 2020  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Compare Messages used by Panel PCOMPR                   */
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
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 08/10/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
CMPR000  '                        ' .ALARM=NO
'CMPR000  Welcome...                                                           '
CMPR001  '                        ' .ALARM=NO
'CMPR001  OLD DSN - &MYMSG                                                     '
CMPR002  '                        ' .ALARM=NO
'CMPR002  NEW DSN - &MYMSG                                                     '
CMPR003  '                        ' .ALARM=NO
'CMPR003  Compare Type must be FULL, ASM or NOASM.                             '
CMPR004  '                        ' .ALARM=NO
'CMPR004  Compare Program Name must be COMPARB, IEBCMPR or ZEBCMPR.            '
CMPR005  '                        ' .ALARM=NO
'CMPR005  Print Results must be Y or N.                                        '
CMPR006  '                        ' .ALARM=NO
'CMPR006  Delete Results must be Y or N.                                       '
CMPR007  '                        ' .ALARM=NO
'CMPR007  Member name required for OLD and NEW DSN.  Or, NO member name.       '
CMPR008  '                        ' .ALARM=NO
'CMPR008  Use ''x'' to mark option.                                            '
CMPR009  '                        ' .ALARM=NO
'CMPR009  Invalid Selection &KEYPRESS  &LCMD                                   '
./ ADD NAME=CMPR01
/********************************************************************/
/*                                                                  */
/* MESSAGES: CMPR01                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/CCompare-in-MVS38J             */
/*         Copyright (C) 2020  Larry Belmontes, Jr.                 */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Compare Messages used by Panel PCOMPR                   */
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
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 08/10/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
CMPR010  '                        ' .ALARM=NO
'CMPR010  Dataset Name is invalid, cannot contain blanks                       '
CMPR011  '                        ' .ALARM=NO
'CMPR011  Unit must be blank                                                   '
CMPR012  '                        ' .ALARM=NO
'CMPR012  Browse Results must be Y or N.                                       '
CMPR013  '                        ' .ALARM=NO
'CMPR013  Press ENTER to process request.                                      '
CMPR018  '                        ' .ALARM=NO
'CMPR018  RC=&MYRC &MYMSG                                                      '
CMPR019  '                        ' .ALARM=NO
'CMPR019  Compare Complete. RC=&MYRC                                           '
@@
