   /* MVP: MVS/CE PACKAGE MANAGER */
PARSE ARG ACTION PACKAGE ARGUMENTS

   /*                 */
  /* PARSE ARGUMENTS */
 /*                 */

CALL SETG('DEBUG_ON',0)
CALL SETG('REPO', 'LOCAL')

IF (UPPER(ACTION) = '-H' | UPPER(PACKAGE) = '-H') THEN CALL USAGE
IF POS('-H',UPPER(ARGUMENTS)) > 0 THEN CALL USAGE

IF (UPPER(ACTION) = '-D' | UPPER(ACTION) = '--DEBUG') THEN DO
  CALL USAGE
  EXIT 1
END

IF LENGTH(ACTION) = 0 THEN DO
  CALL USAGE
  EXIT 1
END

IF (UPPER(PACKAGE) = '-D' | UPPER(PACKAGE) = '--DEBUG') THEN DO
  CALL SETG('DEBUG_ON',1)
  CALL DEBUG "DEBUGGING ENABLED"
  PACKAGE = ''
END

ARGS = ARGUMENTS

DO WHILE LENGTH(ARGUMENTS) > 0
  PARSE UPPER VAR ARGUMENTS P ARGUMENTS
  IF (P = "-D" | OR  P = '--DEBUG') THEN DO
    CALL SETG('DEBUG_ON',1)
    CALL DEBUG "DEBUGGING ENABLED"
  END
END

CALL DEBUG "MVP STARTED WITH THE FOLLOWING:"
CALL DEBUG "ACTION:" ACTION
CALL DEBUG "PACKAGE:" PACKAGE
CALL DEBUG "ARGUMENTS:" ARGS

   /*                                        */
  /* READ THE PARMLIB SYS2.PARMLIB(MVP0001) */
 /*                                        */
CALL DEBUG "READING CONFIG SYS2.PARMLIB(MVP0001)"
"ALLOC FI(PARMS) DA('SYS2.PARMLIB(MVP0001)') SHR "  
"EXECIO * DISKR PARMS (FINIS STEM PARMS."
IF RC > 0 THEN DO
    CALL ERROR "ERROR READING SYS2.PARMLIB(MVP0001):" RC
    "FREE F(PARMS)"
    EXIT 8
END
"FREE F(PARMS)"

CALL DEBUG "SYS2.PARMLIB(MVP0001) CONTAINS" PARMS.0 "LINES"

DO I=1 TO PARMS.0
    /* SKIP COMMENTS */
    CALL DEBUG "CONFIG LINE" I||":" PARMS.I
    IF POS("#",PARMS.I) = 1 THEN ITERATE
    /* GET THE TIMEOUT VARIABLE */
    IF POS("TIMEOUT",UPPER(PARMS.I)) THEN DO
        PARSE VAR PARMS.I . "=" T
        CALL SETG('TIMEOUT', T)
        CALL DEBUG "TIMEOUT SET TO:" GETG('TIMEOUT')
    END
    /* GET THE REPO LOCATION */
    IF POS("REPO",UPPER(PARMS.I)) THEN DO
        PARSE VAR PARMS.I . "=" R
        CALL SETG('REPO', R)
        CALL DEBUG "REPO SET TO:" GETG('REPO')
    END
END
CALL DEBUG "DONE READING CONFIG SYS2.PARMLIB(MVP0001)"

   /*                                         */
  /* READ PACKAGES AND INSTALLED PACKAGED DB */
 /*                                         */

CALL DEBUG "OPENING MVP.PACKAGES(CACHE)"
"ALLOC FI(CACHE) DA('MVP.PACKAGES(CACHE)') SHR "
"EXECIO * DISKR CACHE (FINIS STEM CACHE."
IF RC > 0 THEN DO
    CALL ERROR "ERROR READING MVP.PACKAGES(CACHE):" RC
    EXIT 8
END
"FREE F(CACHE)"
CALL DEBUG "READ" CACHE.0 "LINES"
CALL DEBUG "MVP.PACKAGES(CACHE) CLOSED"

CALL DEBUG "OPENING MVP.MVPDB"
"ALLOC FI(MVPDB) DA('MVP.MVPDB') SHR "
"EXECIO * DISKR MVPDB (FINIS STEM MVPDB."

IF RC > 0 THEN DO
    CALL ERROR "ERROR READING MVP.PACKAGES(MVPDB):" RC
    "FREE F(MVPDB)"
    EXIT 8
END
"FREE F(MVPDB)"
CALL DEBUG "READ" MVPDB.0 "LINES"
CALL DEBUG "MVP.MVPDB CLOSED"

   /*                         */
  /* WHICH STEP ARE WE DOING */
 /*                         */

SELECT
  WHEN UPPER(ACTION) == "INSTALL" THEN
    DO
      CALL INSTALL PACKAGE
    END
  WHEN UPPER(ACTION) == "LIST" THEN
    DO
      IF UPPER(PACKAGE) = '--INSTALLED' THEN DO
 
        DO I=1 TO MVPDB.0
          SORTIN.I = MVPDB.I
        END 
        
        SORTIN.0 = MVPDB.0

        CALL DEBUG "SORTING CACHE BEFORE PRINTING"
        CALL RXSORT
 
        SAY "LISTING INSTALLED PACKAGES:"
        SAY ""
        DO I=1 TO SORTIN.0
          SAY "" SORTIN.I
        END
      END
      ELSE IF LENGTH(PACKAGE) > 0 THEN DO
        SAY "PACKAGE" PACKAGE "PASSED WITH LIST DID YOU MEAN INFO?"
        RETURN
      END
      ELSE DO
        
        DO I=1 TO CACHE.0
          SORTIN.I = CACHE.I
        END

        SORTIN.0 = CACHE.0

        CALL DEBUG "SORTING CACHE BEFORE PRINTING"
        CALL RXSORT

        SAY "LISTING AVAILABLE PACKAGES:"
        SAY ""
        DO I=1 TO SORTIN.0
          PARSE VAR SORTIN.I PROG . VER
          SAY " " PROG VER
        END

      END
      SAY ""
      SAY "LISTING DONE"
    END
  WHEN (UPPER(ACTION) == "INFO" | UPPER(ACTION) == "SHOW") THEN
    DO
      CALL INFO PACKAGE
    END
  WHEN UPPER(ACTION) == "UPDATE" THEN
    DO
      SAY "UPDATING..."
      CALL RUN_INSTALL_JOB UPDATE
      IF RC > 0 THEN SAY "ERROR WITH UPDATE"
      ELSE SAY "UPDATE DONE"
    END
  WHEN UPPER(ACTION) == "SEARCH" THEN
    DO
      SAY "SEARCHING..."
      SAY ""
      CALL SEARCH PACKAGE
    END
END

RETURN

   /*                                        */
  /* THE END OF MVP BELOW ARE THE FUNCTIONS */
 /*                                        */

SEARCH:
  /* SEARCH THE PACKAGES DB AND THE PACKAGE
     DESCRIPTIONS FOR THE PASSED WORD */
  PARSE ARG SEARCHTERM
  IF LENGTH(SEARCHTERM) = 0 THEN RETURN
  CALL DEBUG "SEARCH: SEARCHING FOR:" SEARCHTERM

  DO I=1 TO CACHE.0
    SHORT_DESC = ""
    FOUND = 0
    PARSE VAR CACHE.I PROGNAME . .
    IF POS(UPPER(SEARCHTERM),UPPER(PROGNAME)) > 0 THEN DO
        FOUND = 1
    END
    CALL DEBUG "SEARCH: SEARCHING DESCRIPTION" PROGNAME
    /* GET THE DESCRIPTION */
    "ALLOC FI(DESCS) DA('MVP.PACKAGES("PROGNAME")') SHR "
    "EXECIO * DISKR DESCS (FINIS STEM DESCSEARCH."
    IF RC > 0 THEN DO
        CALL ERROR "ERROR READING MVP.PACKAGES("PROGNAME"):" RC
        "FREE F(DESCS)"
        EXIT 8
    END
    "FREE F(DESCS)"
    DO J=1 TO DESCSEARCH.0

      IF POS("DESCRIPTION:", UPPER(DESCSEARCH.J)) == 1 THEN DO
        PARSE VAR DESCSEARCH.J . SHORT_DESC
        CALL DEBUG 'SHORT DESC' SHORT_DESC
      END

      IF POS("PACKAGE:",UPPER(DESCSEARCH.J)) == 1 THEN ITERATE
      IF POS("VERSION:",UPPER(DESCSEARCH.J)) == 1 THEN ITERATE
      IF POS("DEPENDS:",UPPER(DESCSEARCH.J)) == 1 THEN ITERATE
      IF POS("HOMEPAGE:",UPPER(DESCSEARCH.J)) == 1 THEN ITERATE

      IF POS(UPPER(SEARCHTERM),UPPER(DESCSEARCH.J)) > 0 THEN DO
        FOUND = 1
      END

    END

    IF FOUND THEN DO
      PARSE VAR CACHE.I P . V
      SAY P V
      SAY " " SHORT_DESC
      SAY ""
    END /* DESCSEARCH. LOOP */
  END /* CACHE. LOOP */

  RETURN

INSTALL:
  /* INSTALL THE SPECIFIC PACKAGE */
  PARSE UPPER ARG PACKAGE

  IF LENGTH(PACKAGE) < 1 THEN DO
    CALL ERROR "INSTALL WITHOUT PACKAGE"
    CALL USAGE
    RETURN
  END

  DOESNT_EXIST = CHECK_EXISTS(PACKAGE)

  IF DOESNT_EXIST THEN DO
    SAY "UNABLE TO LOCATE PACKAGE" PACKAGE
    RETURN
  END

  ALREADY_INSTALLED = CHECK_INSTALLED(PACKAGE)

  IF ALREADY_INSTALLED THEN DO
    SAY PACKAGE "IS ALREADY INSTALLED"
    RETURN
  END

  CALL DEBUG "INSTALL: CHECKING FOR DEPENDENCIES"

  DEPENDENTS = CHECK_DEPENDS(PACKAGE)
  CALL DEBUG "INSTALL:" PACKAGE "REQUIRES" DEPENDENTS "(UNSORTED)"
  /* FIRST WE NEED TO REVERSE THE ORDER */
  REVDEP = PACKAGE
  DO WHILE DEPENDENTS \= ''
    PARSE VAR DEPENDENTS IDEP DEPENDENTS
    REVDEP = IDEP REVDEP
  END

  CALL DEBUG "INSTALL:" PACKAGE "REQUIRES" REVDEP "(SORTED)"

  TOTAL = 0
  NODUPES = ''
  DO WHILE REVDEP \= ''
    PARSE VAR REVDEP IDEP REVDEP
    IF WORDPOS(IDEP, NODUPES) > 0 THEN ITERATE
    IF CHECK_INSTALLED(IDEP) THEN ITERATE
    TOTAL = TOTAL + 1
    DEPSTEM.TOTAL = IDEP
    NODUPES = NODUPES IDEP
  END

  CALL DEBUG "INSTALL: TOTAL PACKAGES TO INSTALL:" TOTAL

  DEPSTEM.0 = TOTAL

  SAY "THE FOLLOWING NEW PACKAGES WILL BE INSTALLED: "
  DO I=1 TO DEPSTEM.0
    SAY "  " DEPSTEM.I
  END
  SAY TOTAL "PACKAGES TO INSTALL"

  DO I=1 TO DEPSTEM.0
    DO J=1 TO CACHE.0
      IF WORDPOS(DEPSTEM.I,CACHE.J) > 0 THEN DO
        PARSE VAR CACHE.J . TYPE VERS
        VERSION.I = VERS
        TYPE.I = TYPE
        LEAVE
      END
    END
  END

  DO I=1 TO DEPSTEM.0
    PNAME = DEPSTEM.I
    VERS = VERSION.I
    TYPE = TYPE.I
    SAY "  INSTALLING..." PNAME VERS
    CALL RUN_INSTALL_JOB "INSTALL" PNAME TYPE
    CALL MARK_INSTALLED PNAME VERS
    CALL WAIT(500)
  END

  SAY "DONE"

  RETURN

CHECK_EXISTS:
  /* CHECK IF A PACKAGE EXISTS */
  PARSE UPPER ARG EXISTING
  CALL DEBUG "CHECK_EXISTS: CHECKING FOR" PACKAGE

  DO I=1 TO CACHE.0
    IF WORDPOS(EXISTING, CACHE.I) > 0 THEN DO
      RETURN 0
    END
  END
  RETURN 1

CHECK_INSTALLED:
  /* CHECK IF THE PACKAGE IS ALREADY INSTALLED */
  PARSE UPPER ARG INSTALLED

  CALL DEBUG "CHECK_INSTALLED: CHECKING IF" INSTALLED "IS ALREADY INSTALLED"

  DO I=1 TO MVPDB.0
    /* CALL DEBUG "CHECK_INSTALLED:" WORDPOS(INSTALLED, MVPDB.I) */
    IF WORDPOS(INSTALLED, MVPDB.I) > 0 THEN RETURN 1
  END
  CALL DEBUG "CHECK_INSTALLED:" INSTALLED "IS NOT INSTALLED"

  RETURN 0

MARK_INSTALLED:
  /* MARK A PACKAGE AS INSTALLED IN THE DB */
  PARSE UPPER ARG M_INSTALLED M_VERSION


  CALL DEBUG "MARK_INSTALLED: MARKING " M_INSTALLED M_VERSION "AS INSTALLED"
  TO_APPEND = M_INSTALLED M_VERSION

  CALL DEBUG "MARK_INSTALLED: OPENING MVP.MVPDB"
  FSTREAM = OPEN("'MVP.MVPDB'",'A')
  IF FSTREAM < 0 THEN DO
    CALL ERROR "ERROR OPENING TO MVP.MVPDB"
    EXIT 8
  END
  CALL DEBUG "MARK_INSTALLED: APPENDING" TO_APPEND "TO MVP.MVPDB"
  CALL LINEOUT FSTREAM,TO_APPEND
  CALL CLOSE FSTREAM     

  RETURN


INFO:
  /* PRINTS THE PACKAGE DESCRIPTION FILE */
  PARSE UPPER ARG PACKAGE
  CALL DEBUG "INFO: GETTING INFORMATION FOR" PACKAGE

  IF LENGTH(PACKAGE) < 1 THEN DO
    CALL ERROR "INFO WITHOUT PACKAGE"
    CALL USAGE
    RETURN
  END

  RC = CHECK_EXISTS(PACKAGE)
  IF RC > 0 THEN DO
    CALL ERROR "PACKAGE " PACKAGE "DOES NOT EXIST"
  END

  CALL DESC PACKAGE

  DO I = 1 TO DESC.0
    SAY DESC.I
  END
  RETURN

DESC:
  /* READS THE MEMBER FROM MVP.PACKAGES AND STORES IT IN
     THE STEM DESC. */
  PARSE UPPER ARG MEMBER
  CALL DEBUG "DESC: GETTING DESCRIPTION FOR" PACKAGE

  "ALLOC FI(DESC) DA('MVP.PACKAGES("MEMBER")') SHR "
  "EXECIO * DISKR DESC (FINIS STEM DESC."
  IF RC > 0 THEN DO
      CALL ERROR "ERROR READING MVP.PACKAGES("MEMBER"):" RC
      "FREE F(DESC)"
      EXIT 8
  END
  "FREE F(DESC)"
  RETURN

RUN_INSTALL_JOB: PROCEDURE
  /* SUBMITS A JOB AND WAITS FOR THE RESULTS ON MTT */
  PARSE ARG TASK JOBNAME TYPE
  CALL DEBUG "RUN_INSTALL_JOB: STARTING" JOBNAME TYPE
  /* IF THIS IS AN UPDATE TAKS THE JOBNAME IS UPDATE */

  HIGHEST_JOBNUM = CALL GET_JOBNUM

  IF HIGHEST_JOBNUM == -1 THEN RETURN

  IF LENGTH(JOBNAME) = 0 THEN JOBNAME = TASK

  CALL DEBUG "RUN_INSTALL_JOB: JOBNAME:" JOBNAME "STEPCC:" STEPCC
  IF STEPCC = 'STEPCC' THEN STEPCC = ''

  

  IF GETG("REPO") == "LOCAL" THEN DO

    CALL DEBUG "RUN_INSTALL_JOB: RUNNING SH MVP/MVP " TASK JOBNAME
    ADDRESS COMMAND 'CP SH MVP/MVP ' TASK "'"||JOBNAME||"'"
    CALL DEBUG "RUN_INSTALL_JOB: RUNNING SH MVP/MVP COMPLETE " TASK JOBNAME

    IF RC <> 0 THEN DO
        CALL ERROR "ERROR RUNNING ADDRESS COMMANND CP SH ./MVP/MVP" TASK JOBNAME
        RETURN
    END
  END
  ELSE IF GETG("REPO") \= "LOCAL" THEN DO
    /* FOR FUTURE EXPANSION OF REMOTE REPOS */
    CALL ERROR "REMOTE MVP REPOS NOT SUPPORTED IN V1"
    EXIT 1
  END

  CALL CHECK_JOB JOBNAME HIGHEST_JOBNUM

  IF TYPE = "JCL" THEN DO
    CALL WTO "MVP001I" JOBNAME "INSTALL DONE"
    RETURN
  END

  IF TYPE = "XMI" THEN CALL PROCESS_XMI JOBNAME
  IF TYPE = "ZIP" THEN CALL PROCESS_ZIP JOBNAME

RETURN

GET_JOBNUM: PROCEDURE
  /* READS THE MTT AND RETURNS THE HIGHEST JOBNUM */
  CALL DEBUG "GET_JOBNUM: COLLECTING MTT"

  RC = MTT()
  IF RC > 0 THEN DO
    DO I=_LINE.0 TO 1 BY -1
      PARSE VAR _LINE.I . . JOBM HIGHEST_JOBNUM .
      IF JOBM = "JOB" THEN DO
        LEAVE
      END
    END
  END
  ELSE DO
    CALL ERROR "CANNOT CHECK JOB LOG"
    RETURN -1
  END

  CALL DEBUG "GET_JOBNUM: HIGHEST JOBNUM:" HIGHEST_JOBNUM

  RETURN HIGHEST_JOBNUM

CHECK_JOB: PROCEDURE
  /* READS THE MTT BACKWARDS FROM BOTTOM UP AND COMPARES
     THE FOUND JOB NUMBER WITH A GIVEN JOB NUMBER
     IF THE JOB NUMBER IS HIGHER THAT THE PROVIDED JOB NUMBER
     IT WILL THEN CHECK FOR IEF RECORDS AND THE PROVIDED JOBNAME

     ONCE IT KNOWS THE JOB HAS ENDED IT CHECKS THE CONDITION CODE FOR
     EACH STEP TO CONFIRM IT IS 0000
  */
  PARSE ARG JOBNAME PREV_JOBNUM

  ENDED = "IEF404I "||JOBNAME||" - ENDED"
  STARTED = "IEF403I "||JOBNAME||" - STARTED"
  FAILED = "IEF453I "||JOBNAME||" - JOB FAILED"

  NOTFOUND = 1
  ATTEMPTS = 0

  DO WHILE NOTFOUND
    CALL WAIT(1000)

    /* DONT LOOP FOREVER */
    IF ATTEMPTS >= GETG(TIMEOUT) THEN LEAVE
    ATTEMPTS = ATTEMPTS + 1

    RC = MTT()
    IF RC < 0 THEN ITERATE

    CALL DEBUG "CHECK_JOB: SEARCHING MTT FOR JOB OUTPUT FOR" TASK JOBNAME
    CALL DEBUG "CHECK_JOB: SEARCHING FOR SUCCESSFULL JOB COMPLETI (IEF404I)"ON
    DO I=_LINE.0 TO 1 BY -1
      PARSE VAR _LINE.I . . JOBM CUR_JOBNUM .
      IF JOBM = "JOB" THEN DO
      /*  CALL DEBUG "CHECK_JOB:" CUR_JOBNUM "<=" PREV_JOBNUM
        CALL DEBUG "LOOKING FOR " ENDED "in" _LINE.I */
        IF CUR_JOBNUM <= PREV_JOBNUM THEN LEAVE
        ELSE IF POS(ENDED,_LINE.I) > 0 THEN DO
          CALL DEBUG "CHECK_JOB:" JOBNAME "FOUND IN MTT"
          JOBEND = I
          NOTFOUND = 0
          LEAVE
        END
        ELSE IF POS(FAILED, _LINE.I) > 0 THEN DO
          CALL ERROR JOBNAME "FAILED TO INSTALL"
          JOBEND = I
          NOTFOUND = 0
          LEAVE
        END                                   
        /* ELSE CALL DEBUG "CHECK_JOB:" _LINE.I */
      END
    END
  END

  IF ATTEMPTS >= GETG(TIMEOUT) THEN DO
    CALL ERROR "UNABLE TO FIND" JOBNAME "IN MTT AFTER" GETG(TIMEOUT) SECONDS
    CALL ERROR "INSTALL ABORTING"
    EXIT -1
    RETURN
  END

  CALL DEBUG "CHECK_JOB: SEARCHING FOR IEF403I IN MTT"
  DO J=JOBEND TO 1 BY -1
    CALL DEBUG "RUN_INSTALL_JOB:" _LINE.J
    IF POS(STARTED,_LINE.J) > 0 THEN LEAVE
  END

  JOBSTART = J

  DO I=JOBSTART TO JOBEND
    PARSE VAR _LINE.I . . JOBM JOBNUM TYPE DET
    /* SKIP ENTRIES THAT AREN'T JOBS OR HAVE THE WRONG JOBNUMBER */
    IF JOBM \= 'JOB' | JOBNUM \= CUR_JOBNUM THEN ITERATE
    IF TYPE = "IEFACTRT" THEN DO
      PARSE VAR DET NAME '/' PROG '/' . '/' . '/' CC '/' JOBNAME
      IF CC \= 0000 THEN DO
        CALL ERROR "JOB JOB"||RIGHT(JOBNUM,5,'0') "FAILED"
        CALL ERROR "JOBNAME:" JOBNAME "STEP:" NAME "PROGRAM:" PROG
        CALL ERROR "MAXCC:" CC
        EXIT -1
      END
    END
  END

  RETURN

PROCESS_XMI:
  /* IF THE PACKAGE WAS AN XMI THIS FUNCTION PROCESSES
     THE XMI FILE BY SEARCHING FOR #NNNJCL/#NNNREX MEMBERS
     IN MVP.WORK AND RUNNING THEM IN ORDER
  */

  CALL WAIT(1000)
  PARSE ARG APPNAME

  CALL DEBUG "PROCESS_XMI: CHECKING IF MVP.WORK EXISTS"

  DSN_OK = SYSDSN("'MVP.WORK'")

  IF DSN_OK = 'OK' THEN I=1
  ELSE DO
    CALL ERROR "ATTEMPT TO PROCESS" APPNAME "MVP.WORK NOT FOUND"
    EXIT -1
  END

  CALL DEBUG "PROCESS_XMI: MVP.WORK EXISTS"

  NONE_FOUND = 1
  TOTAL_XMI_TASKS = 0


  /*

  THIS CURRENTLY BREAKS EXECIO INSTEAD WE BRUTE FORCE ENTRIES

  X = DIR("'MVP.WORK'")

  CALL DEBUG "PROCESS_XMI: LISTING MVP.WORK MEMBERS"
  DO I=1 TO DIRENTRY.0
    CALL DEBUG "PROCESS_XMI:" I DIRENTRY.I.NAME
  END

  CALL DEBUG "PROCESS_XMI: LOCATING INSTALL JCL/REXX TASKS"

  DO I=1 TO DIRENTRY.0
    IF POS("#",DIRENTRY.I.NAME) = 1 THEN DO
      XMI_NUM = STRIP(SUBSTR(DIRENTRY.I.NAME,2,3),,'0')
      XMI_TYPE = SUBSTR(DIRENTRY.I.NAME,5)
      WORK_TASKNAME = DIRENTRY.I.NAME
      DS = "PROCESS_XMI: FILE" XMI_NUM "FOUND TYPE" XMI_TYPE "("WORK_TASKNAME")"
      CALL DEBUG DS
      NONE_FOUND = 0
      TOTAL_XMI_TASKS = TOTAL_XMI_TASKS + 1
      SORTIN.TOTAL_XMI_TASKS = XMI_NUM XMI_TYPE WORK_TASKNAME
    END
  END

  */

  /* *********************************************** */
  /* DIRTY HACK WITHOUT DIR UNTIL V2R5M1 IS RELEASED */
  /* *********************************************** */
  DO I=1 TO 10

    DSN_OK = SYSDSN("'MVP.WORK(#"RIGHT(I,3,0)"JCL)'")
    IF DSN_OK = 'OK' THEN DO
      TOTAL_XMI_TASKS = TOTAL_XMI_TASKS + 1
      SORTIN.TOTAL_XMI_TASKS = I "JCL" "#"RIGHT(I,3,0)"JCL"
      NONE_FOUND = 0
    END

    DSN_OK = SYSDSN("'MVP.WORK(#"RIGHT(I,3,0)"REX)'")
    IF DSN_OK = 'OK' THEN DO
      TOTAL_XMI_TASKS = TOTAL_XMI_TASKS + 1
      SORTIN.TOTAL_XMI_TASKS = I "REX" "#"RIGHT(I,3,0)"REX"
      NONE_FOUND = 0
    END

  END
  /* *********************************************** */
  /* END DIRTY HACK UNTIL V2R5M1 IS RELEASED         */
  /* *********************************************** */

  CALL DEBUG "PROCESS_XMI: FOUND" TOTAL_XMI_TASKS "INSTALL TASKS"

  IF NONE_FOUND THEN DO
    CALL ERROR "MISSING PREINSTALL FILE #001JCL OR #001REX FOR" APPNAME
    EXIT 8
  END


  SORTIN.0 = TOTAL_XMI_TASKS
  CALL RXSORT

   DO I=1 TO SORTIN.0
      PARSE VAR SORTIN.I XMI_NUM XMI_TYPE WORK_TASKNAME
      CALL DEBUG "PROCESS_XMI: TASK" XMI_NUM "TYPE" XMI_TYPE
      IF XMI_TYPE == "JCL" THEN DO
        CALL DEBUG "PROCESS_XMI:" APPNAME "SUBMITTING" WORK_TASKNAME
        CALL SUBMIT_WORK_JCL "'MVP.WORK("|| WORK_TASKNAME ||")'" 
      END
  END

  RETURN

PROCESS_ZIP:
  /* IF THE PACKAGE WAS ZIP FILE THIS FUNCTION PROCESSES
     THE ZIP FILE BY SEARCHING FOR #NNNJCL/#NNNREX MEMBERS
     IN MVP.WORK AND RUNNING THEM IN ORDER
  */

 
  PARSE ARG APPNAME
  NOTFOUND = 1
  ATTEMPTS = 0

   DO WHILE NOTFOUND
    CALL WAIT(1000)

    /* DONT LOOP FOREVER */
    IF ATTEMPTS >= GETG(TIMEOUT) THEN DO
      CALL ERROR "PROCESS_ZIP: ATTEMPT TO PROCESS" APPNAME "MVP.WORK NOT FOUND"
      EXIT -1
    END
    ATTEMPTS = ATTEMPTS + 1
    CALL DEBUG "PROCESS_ZIP: CHECKING IF MVP.WORK EXISTS"
    DSN_OK = SYSDSN("'MVP.WORK'")
    IF DSN_OK = 'OK' THEN LEAVE

  END

  CALL DEBUG "PROCESS_ZIP: MVP.WORK EXISTS"

  NONE_FOUND = 1
  TOTAL_ZIP_TASKS = 0


  /*

  THIS CURRENTLY BREAKS EXECIO INSTEAD WE BRUTE FORCE ENTRIES

  X = DIR("'MVP.WORK'")

  CALL DEBUG "PROCESS_ZIP: LISTING MVP.WORK MEMBERS"
  DO I=1 TO DIRENTRY.0
    CALL DEBUG "PROCESS_ZIP:" I DIRENTRY.I.NAME
  END

  CALL DEBUG "PROCESS_ZIP: LOCATING INSTALL JCL/REXX TASKS"

  DO I=1 TO DIRENTRY.0
    IF POS("#",DIRENTRY.I.NAME) = 1 THEN DO
      ZIP_NUM = STRIP(SUBSTR(DIRENTRY.I.NAME,2,3),,'0')
      ZIP_TYPE = SUBSTR(DIRENTRY.I.NAME,5)
      ZIP_TASKNAME = DIRENTRY.I.NAME
      DS = "PROCESS_ZIP: FILE" ZIP_NUM "FOUND TYPE" ZIP_TYPE "("ZIP_TASKNAME")"
      CALL DEBUG DS
      NONE_FOUND = 0
      TOTAL_ZIP_TASKS = TOTAL_ZIP_TASKS + 1
      SORTIN.TOTAL_ZIP_TASKS = ZIP_NUM ZIP_TYPE ZIP_TASKNAME
    END
  END

  */

  /* *********************************************** */
  /* DIRTY HACK WITHOUT DIR UNTIL V2R5M1 IS RELEASED */
  /* *********************************************** */
  DO I=1 TO 10

    DSN_OK = SYSDSN("'MVP.WORK(#"RIGHT(I,3,0)"JCL)'")
    IF DSN_OK = 'OK' THEN DO
      TOTAL_ZIP_TASKS = TOTAL_ZIP_TASKS + 1
      SORTIN.TOTAL_ZIP_TASKS = I "JCL" "#"RIGHT(I,3,0)"JCL"
      NONE_FOUND = 0
    END

    DSN_OK = SYSDSN("'MVP.WORK(#"RIGHT(I,3,0)"REX)'")
    IF DSN_OK = 'OK' THEN DO
      TOTAL_ZIP_TASKS = TOTAL_ZIP_TASKS + 1
      SORTIN.TOTAL_ZIP_TASKS = I "REX" "#"RIGHT(I,3,0)"REX"
      NONE_FOUND = 0
    END

  END
  /* *********************************************** */
  /* END DIRTY HACK UNTIL V2R5M1 IS RELEASED         */
  /* *********************************************** */

  CALL DEBUG "PROCESS_ZIP: FOUND" TOTAL_ZIP_TASKS "INSTALL TASKS"

  IF NONE_FOUND THEN DO
    CALL ERROR "MISSING PREINSTALL FILE #001JCL OR #001REX FOR" APPNAME
    EXIT 8
  END


  SORTIN.0 = TOTAL_ZIP_TASKS
  CALL RXSORT

   DO I=1 TO SORTIN.0
      PARSE VAR SORTIN.I ZIP_NUM ZIP_TYPE WORK_TASKNAME
      CALL DEBUG "PROCESS_ZIP: TASK" ZIP_NUM "TYPE" ZIP_TYPE
      IF ZIP_TYPE == "JCL" THEN DO
        CALL DEBUG "PROCESS_ZIP:" APPNAME "SUBMITTING" WORK_TASKNAME
        CALL SUBMIT_WORK_JCL "'MVP.WORK("|| WORK_TASKNAME ||")'" 
      END
  END

  RETURN

SUBMIT_WORK_JCL: 
/* INSERTS THE MVP USERNAME AND PASSWORD TO THE WORK JCL */
/* RETURNS NOTHING */

  PARSE ARG JCL_LOCATION
  HJ = GET_JOBNUM()
  CALL DEBUG "SUBMIT_WORK_JCL: PROCESSING" JCL_LOCATION

  WORKJCL_STREAM = OPEN(JCL_LOCATION,'R')

  IF WORKJCL_STREAM < 0 THEN DO
    CALL ERROR "ERROR READING "||JCL_LOCATION||":" WORKJCL_STREAM
    EXIT 8
  END

  WORKJCL.0 = LINES(WORKJCL_STREAM)    
  DO K=1 TO LINES(WORKJCL_STREAM)
    WORKJCL.K = LINEIN(WORKJCL_STREAM) 
  END 
  
  CALL CLOSE WORKJCL_STREAM                      

  IF FIND(WORKJCL.1,'JOB') < 1 THEN DO
    CALL ERROR JCL_LOCATION ||"IS NOT A VALID JOB"
    EXIT 8
  END

  /* FIRST MAKE SURE THE QUEUE IS EMPTY */
  DO QUEUED()
    PULL ELEMENT
    CALL DEBUG 'SUBMIT_WORK_JCL: PREVIOUS QUEUE ITEM' ELEMENT
  END

  DO J=1 TO WORKJCL.0
    
    X=0
    DO WHILE (RIGHT(WORKJCL.J,X) == LEFT(" ",X))
      /* IF WE HAVE SPACES FIND THE FIRST NON SPACE CHARACTER */
      X = X + 1
      ITERATE
    END
    
    IF RIGHT(WORKJCL.J,1) == "," THEN DO
      /* IF THE FIRST LINE HAS A CONTINUATION, KEEP GOING */
      QUEUE WORKJCL.J
      ITERATE
    END


    JCL = LEFT(WORKJCL.J, LENGTH(WORKJCL.J) - (X - 1))||","
    CALL DEBUG "SUBMIT_WORK_JCL: CHANGING LINE" J "TO:" JCL
    QUEUE JCL

    CALL DEBUG "SUBMIT_WORK_JCL: ADDING TO MVP USER TO JOB"
    QUEUE "//    USER=MVP,PASSWORD="||GET_PW()

    LEAVE    
  END

  IF J = WORKJCL.0 THEN DO
    CALL ERROR "UNABLE TO PARSE WORK JCL FILE " JCL_LOCATION
    EXIT 8
  END

  DO J = (J+1) TO WORKJCL.0
    QUEUE WORKJCL.J
  END

  TOTAL = QUEUED()

  DO X=1 TO QUEUED()                                
    PULL ELEMENT                                    
    JCLSTEM.X = ELEMENT                             
  END                                               
  JCLSTEM.0 = TOTAL                                     
                                                    
  CALL DEBUG "SUBMIT_WORK_JCL: SUBMITTING" JCLSTEM.0 "LINES" 
  CALL SUBMIT('JCLSTEM.')                           
  CALL CHECK_JOB WORK_TASKNAME HJ

  RETURN

GET_PW: 
/* RETURNS A STRING WITH THE MVP USER PASSWORD FROM RAKF */
  CALL DEBUG "GET_PW: OPENING 'SYS1.SECURE.CNTL(USERS)'"


  PW_STREAM = OPEN("'SYS1.SECURE.CNTL(USERS)'",'R')

  IF PW_STREAM < 0 THEN DO
    CALL ERROR "ERROR READING 'SYS1.SECURE.CNTL(USERS)':" PW_STREAM
    EXIT 8
  END

  USERS.0 = LINES(PW_STREAM)    
  DO K=1 TO LINES(PW_STREAM)
    USERS.K = LINEIN(PW_STREAM) 
  END 
  
  CALL CLOSE PW_STREAM  

  PW = ""

  CALL DEBUG "GET_PW: SEARCHING FOR MVP USER"
  DO K=1 TO USERS.0
    IF LEFT(USERS.K, 4) == "MVP " THEN DO
      CALL DEBUG "GET_PW: MVP USER FOUND RETURNING"
      /* RETURNS THE PASSWORD FOR THE MVP USER */
      PW = SUBSTR(USERS.K,19,8)
      LEAVE
    END
  END

  IF PW="" THEN DO
    CALL DEBUG "GET_PW: MVP USER NOT FOUND!"
  END

  RETURN PW

DEBUG: PROCEDURE
  /* PRINTS A DEBUG MESSAGE TO TSO AND CONSOLE IF DEBUGGING IS ENABLED
     WITH THE ARGUMENT -D/--DEBUG
  */
  PARSE ARG STRING
  IF GETG(DEBUG_ON) THEN DO
    SAY "MVP000I - DEBUG -" STRING
    CALL WTO "MVP000I - DEBUG -" STRING
  END
  RETURN

ERROR:
  /* PRINTS MVP ERROR MESSAGE TO TSO AND MVS CONSOLE */
  PARSE ARG STRING
  SAY "MVP999E - ERROR -" STRING
  CALL WTO "MVP999E - ERROR -" STRING
  RETURN

USAGE:
  /* PRINTS MVP USAGE */
  SAY "MVS/CE PACKAGE MANAGER (MVP) V1.0"
  SAY "USAGE:"
  SAY "  RX MVP COMMAND [OPTIONS]"
  SAY "EXAMPLE:"
  SAY "  RX MVP LIST"
  SAY "  RX MVP INFO PACKAGE --DEBUG"
  SAY "  RX MVP UPDATE -D"
  SAY "  RX MVP INSTALL PACKAGE"
  SAY ""
  SAY "WHERE COMMAND IS ONE OF:"
  SAY "  LIST      - LIST PACKAGES BASED ON PACKAGE NAMES"
  SAY "  SEARCH    - SEARCH IN PACKAGE DESCRIPTIONS"
  SAY "  SHOW/INFO - SHOW PACKAGE DETAILS"
  SAY "  INSTALL   - INSTALL PACKAGES"
  SAY "  UPDATE    - UPDATE LIST OF AVAILABLE PACKAGES"
  SAY ""
  SAY "OPTIONS:"
  SAY "  -D/--DEBUG - SHOW DEBUGGING OUTPUT TO TSO AND MVS CONSOLE"
  SAY "  --INSTALLED - WHEN USING LIST SHOW ONLY INSTALLED PACKAGES"
  RETURN

CHECK_DEPENDS: PROCEDURE
  /* RECURSIVE FUNCTION TO GET PACKAGE DEPENDENCIES */
  PARSE ARG DEPENDS_CHECK
  DEPEND_TEXT = ''

  CALL DEBUG "CHECKING DEPENDENCIES FOR" DEPENDS_CHECK

  "ALLOC FI(DESC) DA('MVP.PACKAGES("DEPENDS_CHECK")') SHR "
  "EXECIO * DISKR DESC (FINIS STEM DESC."

  IF RC > 0 THEN DO
      CALL ERROR "ERROR READING MVP.PACKAGES("DEPENDS_CHECK"):" RC
      "FREE F(DESC)"
      EXIT 8
  END
  "FREE F(DESC)"

  DO I=1 TO DESC.0
    IF POS("DEPENDS:", UPPER(DESC.I)) > 0 THEN DO
      PARSE VAR DESC.I . DEPENDS
      CALL DEBUG "DEPENDENCIES FOR" DEPENDS_CHECK ":" DEPENDS
      LEAVE
    END
  END

  CALL DEBUG "CHECK_DEPENDS: ALL DEPENDENTS" DEPENDS

  DEPEND_TEXT = DEPENDS

  DO WHILE LENGTH(DEPENDS) > 0
    PARSE VAR DEPENDS DEPENDENT DEPENDS
    /* SAY "CURRENT DEPENDENCY" DEPENDENT */
    DEPEND_TEXT = DEPEND_TEXT CHECK_DEPENDS(DEPENDENT)
  END
  RETURN DEPEND_TEXT
