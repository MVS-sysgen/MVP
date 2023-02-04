//GETJOBI JOB (TSO),
//             'Install GETJOBI',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,PASSWORD=SYS1
//* From:
//* https://ibmmainframes.com/viewtopic.php?p=167434&highlight=#167434
//*
//* Requires MACLIB and SXMACLIB
//*
//* STEP 1: Compile/Link LISTCDS to SYS2.LINKLIB(GETJOBI)
//*
//ASSEM        EXEC ASMFCL,MAC='SYS1.MACLIB',MAC1='SYS2.MACLIB',
//             MAC2='SYS2.SXMACLIB',PARM.LKED='(XREF,LET,LIST,CAL)'
//ASM.SYSIN    DD DATA,DLM='@@'
*PROCESS RENT                      PROGRAM IS RE-ENTRANT               
***********************************************************************
*---------------------------------------------------------------------*
*                          < G E T J O B I >                          *
*                        (GET JOB INFORMATION)                        *
*---------------------------------------------------------------------*
*                                                                     *
*        THIS SUB-PROGRAM IS CALLED TO OBTAIN THE JOB-NAME, PROCSTEP  *
*        NAME, STEP-NAME AND JOB-NUMBER (IN THIS ORDER). NOTE THE     *
*        REQUIRED 128-BYTE WORKAREA (FOR REENTRANCY PURPOSES).        *
*                                                                     *
*        EXAMPLE SYNTAX:                                              *
*                                                                     *
*        03  WS-GETJOBI-PARM-REC.                                     *
*            05  WS-GETJOBI-JOB-NAME                                  *
*                                  PIC  X(08).                        *
*            05  WS-GETJOBI-PROCSTEP-NAME                             *
*                                  PIC  X(08).                        *
*            05  WS-GETJOBI-STEP-NAME                                 *
*                                  PIC  X(08).                        *
*            05  WS-GETJOBI-JOB-NBR                                   *
*                                  PIC  X(08).                        *
*            05  WS-GETJOBI-WORKAREA                                  *
*                                  PIC  X(128).                       *
*                                                                     *
*        CALL WS-GETJOBI               USING WS-GETJOBI-PARM-REC      *
*        END-CALL                                                     *
*                                                                     *
*        THANK YOU FOR YOUR SUPPORT....                               *
*                                                                     *
*---------------------------------------------------------------------*
***********************************************************************
PRMDSECT DSECT                     PRMDSECT-DSECT                       
         USING *,R7                INFORM ASSEMBLER                     
PARMAREA EQU   *                   BEGIN PARMAREA                       
PARMJNME DS    CL8                 JOB-NAME                             
PARMPSTP DS    CL8                 PROCSTEP-NAME                       
PARMSTPN DS    CL8                 STEP-NAME                           
PARMJNBR DS    CL8                 JOB-NUMBER                           
PARMLGTH EQU   *-PARMAREA          CALCULATE PARM-LGTH                 
STGDSECT DSECT                     CALLER-STORAGE DSECT                 
         USING *,R9                INFORM ASSEMBLER                     
STGAREA  DS    0XL128              BEGIN STORAGE-AREA                   
FWORD    DS    F                   ALIGNED-FULLWORD WORKAREA           
REGSAVEA DS    XL72                72-BYTE REGISTER SAVEAREA           
EXTRPLST EXTRACT MF=L              EXTRACT PARAMETER-LIST (XL12)       
STGRMDR  EQU   STGAREA+L'STGAREA-* REMAINING-STORAGE                   
         DS    XL(STGRMDR)         STORAGE-REMAINDER                   
GETJOBI  CSECT                     PROGRAM IS RE-ENTRANT               
         USING *,R3                INFORM ASSEMBLER                     
         SAVE  (14,12)             SAVE REGISTERS                       
         LA    R3,0(,R15)          CSECT ADDRESSABILITY                 
         J     CHKRNENV            CHECK RUN-ENVIRONMENT               
EYECTCHR DC    CL47' <<< ASSEMBLY DATE/TIME: >>>'
         ORG   EYECTCHR+6          REDEFINITION                         
         DC    X'A2A285948293A8'   LOWER-CASE 'SSEMBLY'                 
         ORG   EYECTCHR+15         REDEFINITION                         
         DC    X'81A385'           LOWER-CASE 'ATE'                     
         ORG   EYECTCHR+20         REDEFINITION                         
         DC    X'899485'           LOWER-CASE 'IME'                     
         ORG   EYECTCHR+L'EYECTCHR RESET LOCATION-COUNTER               
CHKRNENV DS    0H                  ENSURE ALIGNMENT                     
         XR    R10,R10             ENSURE X'00'S                       
         L     R15,540             ADDRESS CURRENT TCB                 
         L     R15,208(,R15)       ADDRESS TCB EXTN                     
         L     R15,20(,R15)        ADDRESS AFCB                         
         LTR   R15,R15             BATCH ENVIRONMENT?                   
         JZ    ADDRPLST            YES, ADDRESS THE PARMLIST           
         CLC   =CL3'AFC',0(R15)    BATCH ENVIRONMENT?                   
         JNE   ADDRPLST            YES, ADDRESS THE PARMLIST           
         LA    R10,4095            INVALID FOR USE IN CICS             
ADDRPLST EQU   *                                                       
         L     R7,0(,R1)           ADDRESS THE PARMLIST                 
         LA    R7,0(,R7)           CLEAR TOP-BIT                       
         MVI   PARMAREA,C' '       ENSURE ALL SPACES                   
         MVC   PARMAREA+1(PARMLGTH-1),PARMAREA                         
         LA    R9,PARMLGTH(,R7)    ADDRESS CALLER'S WORKAREA           
         XC    STGAREA,STGAREA     ENSURE X'00'S                       
         LA    R15,REGSAVEA        POINT TO OUR SAVEAREA               
         ST    R13,4(,R15)         BACKWARD-CHAIN                       
         ST    R15,8(,R13)         FORWARD-CHAIN                       
         LR    R13,R15             POINT TO OUR SAVEAREA               
         LTR   R15,R10             BATCH ENVIRONMENT?                   
         JNZ   RTN2CLLR            NO, RETURN TO CALLER                 
         LA    R10,FWORD           POINT TO FWORD                       
         EXTRACT (R10),'S',FIELDS=TIOT,MF=(E,EXTRPLST)                 
         L     R10,FWORD           LOAD FROM FWORD                     
         MVC   PARMAREA(PARMLGTH-L'PARMJNBR),0(R10)                     
         L     R10,540             CURRENT TCB                         
         L     R10,180(,R10)       POINT TO JFCB                       
         L     R10,316(,R10)       POINT TO SSID                       
         MVC   PARMJNBR,12(R10)    POPULATE JOB-NUMBER                 
         XR    R15,R15             ENSURE X'00'S                       
RTN2CLLR EQU   *                                                       
         L     R13,4(,R13)         RESTORE CALLER'S R13                 
         XC    STGAREA,STGAREA     ENSURE X'00'S                       
         RETURN (14,12),RC=(15)    RESTORE AND RETURN                   
         YREGS ,                   MVS REGISTER-MACRO                   
         LTORG ,                                                  
         END   ,                   END 'GETJOBI'
@@
//LKED.SYSLMOD DD DISP=SHR,DSN=SYS2.LINKLIB(GETJOBI)
//LKED.SYSLIB   DD  DSN=SYS2.LINKLIB,DISP=SHR
//*
//* STEP 2: Install Help
//*
//STEP1   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.HELP,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=GETJOBI
)F FUNCTION -
    < G E T J O B I >  (GET JOB INFORMATION)

    THIS SUB-PROGRAM IS CALLED TO OBTAIN THE JOB-NAME, PROCSTEP
    NAME, STEP-NAME AND JOB-NUMBER (IN THIS ORDER). NOTE THE   
    REQUIRED 128-BYTE WORKAREA (FOR REENTRANCY PURPOSES).      
                                                        
    EXAMPLE SYNTAX:                                            
                                                        
        03  WS-GETJOBI-PARM-REC.                                   
            05  WS-GETJOBI-JOB-NAME                                
                                  PIC  X(08).                      
            05  WS-GETJOBI-PROCSTEP-NAME                           
                                  PIC  X(08).                      
            05  WS-GETJOBI-STEP-NAME                               
                                  PIC  X(08).                      
            05  WS-GETJOBI-JOB-NBR                                 
                                  PIC  X(08).                      
            05  WS-GETJOBI-WORKAREA                                
                                  PIC  X(128).                     

        CALL WS-GETJOBI               USING WS-GETJOBI-PARM-REC    
        END-CALL                                                   
                                                        
)X SYNTAX   - NONE
)O OPERANDS - NONE                             
@@
//*
//* TEST GETJOBI
//*
//*
//TEST     EXEC COBUCLG,                                                    
//         PARM.COB='FLAGW,LOAD,SUPMAP,SIZE=2048K,BUF=1024K,LIST',
//         PARM.GO='TEST123456789TEST'
//COB.SYSPUNCH DD DUMMY                                                     
//COB.SYSIN    DD *                                                         
      *PARAMETER PASSING EXAMPLE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARM.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER-LINE PIC X(133) VALUE ALL '='.
       LINKAGE SECTION.
       01   PARM-BUFFER.
            05 PARM-LENGTH   PIC S9(4) COMP.
            05 PARM-DATA.
               10 NUMB       PIC X(100).
       03  WS-GETJOBI-PARM-REC.
           05  WS-GETJOBI-JOB-NAME
                                 PIC  X(08).
           05  WS-GETJOBI-PROCSTEP-NAME
                                 PIC  X(08).
           05  WS-GETJOBI-STEP-NAME
                                 PIC  X(08).
           05  WS-GETJOBI-JOB-NBR
                                 PIC  X(08).
           05  WS-GETJOBI-WORKAREA
                                 PIC  X(128).
       PROCEDURE DIVISION USING PARM-BUFFER.
       MAINLINE SECTION.
           
           CALL 'GETJOBI' USING WS-GETJOBI-PARM-REC.
           DISPLAY '<< JOB INFORMATION >>'.
           DISPLAY 'JOB NUMBER     : ' WS-GETJOBI-JOB-NBR.
           DISPLAY 'JOB NAME       : ' WS-GETJOBI-JOB-NAME.
           DISPLAY 'JOB STEP NAME  : ' WS-GETJOBI-STEP-NAME.
           DISPLAY 'PROC STEP NAME : ' WS-GETJOBI-PROCSTEP-NAME.
           

           IF PARM-LENGTH > 0 THEN 
             DISPLAY 'PARAMETER LEN  : ' PARM-LENGTH
             DISPLAY 'PARAMETER      : ' PARM-DATA.
           
           
           DISPLAY '<< END INFORMATION >>'.

       MAINLINE-EXIT.
                GOBACK.
/*                                                                          
//COB.SYSLIB DD DSNAME=SYSC.COBLIB,DISP=SHR
//LKED.SYSLIB DD DSNAME=SYSC.COBLIB,DISP=SHR
//            DD DSNAME=SYS2.LINKLIB,DISP=SHR
//GO.SYSOUT  DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=161,BLKSIZE=16100)           
//
