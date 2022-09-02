//SCHEDULE JOB (TSO),
//             'Install SCHEDULE',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(0,0),
//             REGION=0K,
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//* Origional source from Xephon MVS Feb 1999 from cbttape.org
//* That was not usable under MVS3.8J as provided.
//*
//* Converted the SCHEDULE program from xephon mag into
//* something that can run on mvs38j. Changes made are
//* documented in the "changes" member.
//*
//* It starts OK now and submits jobs and commands as
//* expected. Seems to be working OK but I haven't
//* fully tested it.
//*
//* READ THE CUSTOMISATION STEPS BELOW BEFORE RUNNING THIS JCL
//*
//* This JCL deck creates INSTALL and SAMPLE files
//*    prefix.CHKPOINT  - used by sample schedule job and proc
//*    prefix.CMDFILE   - sample commands for schedule testing
//*    prefix.JOBFILE   - sample jobs for schedule testing
//*    prefix.SRC       - contains
//*                       (a)the two assembler source files
//*                          and a 'assemble' job to submit
//*                          them both
//*                       (b)a test batch job to test the 
//*                          scheduler using the install test
//*                          datasets
//*                       (c)a test procedure you can copy to
//*                          a JES2 proclib to test the scheduler
//*                          using the install test datasets
//*                       (d)documentation, plus changes I have 
//*                          made, plus bugs I have found
//*    prefix.DEBUG     - contains debugging assistants
//*                       (a)a program that can display the 
//*                          timestamps recorded in the checkpoint
//*                          file, used to ensure they are updating
//*                       (b)a program to overwrite the checkpoint
//*                          file so I can test the 'harddown'
//*                          recovery handling.
//*
//* ==== INSTALLATION - STEP 1 ====
//* --- BEFORE RUNNING THIS JOB ---
//* CHANGE GLOBALLY 
//*     UNIT=SYSDA TO A DISK GROUP YOU CAN USE (IE:SYSDA)
//*     THE DSN SYSGEN.SCHEDULE PREFIX TO SOMETHING YOU CAN USE
//*         (used for install datasets and to customise samples)
//*     UNIT=3350,VOL=SER=PUB001 TO ONE OF YOUR DISKS
//*     SYS2.LINKLIB TO ONE OF YOUR APF DATASETS
//*         (updates the assemble job being put into the install
//*          dataset only at this point, not assembling yet)
//*
//* --- THEN RUN THIS JOB TO CREATE THE INSTALl DATASETS ---
//*     AFTER RUNNING THIS JOB...
//*
//* ==== INSTALLATION - STEP 2 ====
//*  IN prefix.SCHEDULE.SRC RUN THE ASSEMBLE JOB
//*  (this assembles the two assembler programs into the APF
//*   authourised library you selected in step 1)
//*
//* ==== TESTING AND CUSTOMISATION ====
//*  IN prefix.SCHEDULE.SRC YOU WILL FIND
//*     MEMBER SAMPJOB
//*        You can run this as a batch job to test that the
//*        scheduler starts correctly and runs as expected.
//*     MEMBER SAMPPROC
//*        When ready copy this to one of your JES2 procedure
//*        libraries and rename it to SCHEDULE. Then you can
//*        start it with "S SCHEDULE" from the console or
//*        startup customisation as needed.
//*        Update the filenames it used for you live dataset
//*        names of course.
//*     MEMBER SAMPPARM
//*        A sample schedule parameter file that is used by
//*        the two sample members above to run the sample
//*        jobs and commands from the sample files created
//*        during the install.
//*
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *
    DELETE SYSGEN.SCHEDULE.SRC
    DELETE SYSGEN.SCHEDULE.JOBFILE
    DELETE SYSGEN.SCHEDULE.CMDFILE
    DELETE SYSGEN.SCHEDULE.DEBUG
    DELETE SYSGEN.SCHEDULE.CHKPOINT
    SET LASTCC = 0
    SET MAXCC = 0
//*
//* ---------------------------------------------------------------
//* Create all the datasets required and initialise the checkpoint
//* file.
//* ---------------------------------------------------------------
//CREATE1  EXEC PGM=IEFBR14
//SYSPRINT DD   SYSOUT=*
//FILE1    DD   UNIT=3390,VOL=SER=PUB001,
//           DISP=(NEW,CATLG,DELETE),
//           DCB=(LRECL=80,RECFM=FB,BLKSIZE=9600,DSORG=PO),
//           SPACE=(CYL,(1,1,10)),
//           DSN=SYSGEN.SCHEDULE.SRC
//FILE2    DD   UNIT=3390,VOL=SER=PUB001,
//           DISP=(NEW,CATLG,DELETE),
//           DCB=(LRECL=80,RECFM=FB,BLKSIZE=9600,DSORG=PO),
//           SPACE=(CYL,(1,1,10)),
//           DSN=SYSGEN.SCHEDULE.JOBFILE
//FILE3    DD   UNIT=3390,VOL=SER=PUB001,
//           DISP=(NEW,CATLG,DELETE),
//           DCB=(LRECL=80,RECFM=FB,BLKSIZE=9600,DSORG=PO),
//           SPACE=(CYL,(1,1,10)),
//           DSN=SYSGEN.SCHEDULE.CMDFILE
//FILE4    DD   UNIT=3390,VOL=SER=PUB001,
//           DISP=(NEW,CATLG,DELETE),
//           DCB=(LRECL=80,RECFM=FB,BLKSIZE=9600,DSORG=PO),
//           SPACE=(TRK,(1,1,10)),
//           DSN=SYSGEN.SCHEDULE.DEBUG
//CREATE2  EXEC PGM=IEBGENER
//SYSPRINT DD   SYSOUT=*
//SYSUT1   DD   *
CHKPOINTCHKPOINT
/*
//SYSUT2   DD   UNIT=3390,VOL=SER=PUB001,
//           DISP=(NEW,CATLG,DELETE),
//           DCB=(LRECL=80,RECFM=FB,BLKSIZE=9600,DSORG=PS),
//           SPACE=(TRK,(1,1)),
//           DSN=SYSGEN.SCHEDULE.CHKPOINT
//SYSIN    DD   DUMMY
//*
//* ---------------------------------------------------------------
//* Populate the source (SRC) file with all the scheduler stuff.
//* ---------------------------------------------------------------
//LOADSTUF EXEC PGM=IEBUPDTE,COND=(0,NE)
//SYSPRINT DD   SYSOUT=*
//SYSUT1   DD   DISP=SHR,DSN=SYSGEN.SCHEDULE.SRC
//SYSUT2   DD   DISP=SHR,DSN=SYSGEN.SCHEDULE.SRC
//SYSIN    DD   DATA,DLM=QQ
./ ADD NAME=$DOC
ORIGIONAL SOURCE: XEPHON MVS MAGAZINE MVS9902.PDF FROM CBTTAPE.ORG

The origional documentation provided from that article is included
in this member (with corrections as the article documentation was
not totally correct) after the installation notes and my comments.

The changes I needed to make to get this working on MVS3.8J are
documented in the seperate CHANGES member in this dataset.

-------------------------------------------------------------------
-------------------------------------------------------------------
---                                                             ---
---                    to install                               ---
---                                                             ---
-------------------------------------------------------------------
-------------------------------------------------------------------
You should have made the global changes requested in the
install JCL, in which case all you have to do is
(1) run the job in the assemble member
(2) run the job in the sampjob member to make sure it is all working
(3) copy the sampproc to a member named SCHEDULE in one of your
    system procedure libraries and start it with the 'S SCHEDULE'
    command when you are ready to use it.
(*) Create your own jobs and command members of course in the 
    datasets assigned to DD's CMDFILE and JOBFILE
(*) And of course customise a parmlib member with the events you
    want triggered.

-------------------------------------------------------------------
-------------------------------------------------------------------
---                                                             ---
---               My additional usage notes                     ---
---                                                             ---
-------------------------------------------------------------------
-------------------------------------------------------------------
These notes are around the recovery option as the documentation
and code doesn't really explain that very well.

The scheduler will attempt to rerun events that were due to run at
a time the scheduler was shutdown for some reason if the RECOVER
option was set for those events AND if the the events were due to
occur less than 24hours before the scheduler was restarted.

To clarify what happens based upon observed effects and my reading
of the code as to when WTORs will be issued for recovery events
- if the scheduler was stopped after the last checkpoint time
  you will get the WTORs to confirm jobs should be restarted
- if the scheduler checkpoint time is not earlier than the
  time the scheduler was stopped events that are to be recovered
  will just run automatically without prompting

* my issue is I cannot figure out why the checkpoint time should
  ever be (for a clean shutdown) prior to the scheduler termination
  time as I would have though a checkpoint entry should have been
  written at shutdown
  (although I haven't checked the code to check on that).
  I have stopped/started the scheduler (cleanly) many times during
  testing and only once did I ever get the WTORs (resulting in a
  bugfix being made so that was lucky). From what I can see it will
  function correctly and issue WTORs if you IPL over the top of a
  running system. But you should never cancel the scheduler (see 
  the bugs member). But I don't know how I got the WTORs to appear
  on a clean shutdown in the first place. 

-------------------------------------------------------------------
-------------------------------------------------------------------
---                                                             ---
---                       origional doc                         ---
---                                                             ---
-------------------------------------------------------------------
-------------------------------------------------------------------

The scheduler is composed of two programs a driver program SCHEDM01,
and a subtask program SCHEDM02. Both programs have authorization
requirements so they should reside in an APF authorized library and
be linked AC(1).

The SCHEDM01 program is responsible for interpreting the parameter
library, for building the schedule, for establishing operator
communication, and for attaching the SCHEDM02 program.

The SCHEDM02 program is responsible for checking the schedule that
was built by SCHEDM01 and determining when the next scheduled event
is to occur. The basic approach is to:
 1 Determine what day of the week the program is operating in.
 2 Check to see if any events are waiting to occur for that day.
 3 If they are, do an STIMER wait for the time of day of the first
   event scheduled to occur that day. If there are not any events
   waiting to occur for that day, do an STIMER wait for 23:59:59.
 4 When the STIMER triggers, perform the required function and
   return to step 1.

The following sample JCL can be used when creating your scheduler:
//SCHEDULE EXEC PGM=SCHEDM01,TIME=1440
//SYSIN    DD   DSN=SCHEDULE.PARMLIB,DISP=SHR
//JOBFILE  DD   DSN=SCHEDULE.JOBFILE,DISP=SHR
//CMDFILE  DD   DSN=SCHEDULE.CMDFILE,DISP=SHR
//CHKPOINT DD   DSN=SCHEDULE.CHKPOINT,DISP=SHR
//INTRDR   DD   SYSOUT=(A,INTRDR)
//SYSABEND DD   SYSOUT=A

The datasets that are referenced above are used in the following ways:
    SYSIN     contains the parameter information used for scheduler
              start-up. This dataset has DCB characteristics of
              DSORG=PS, LRECL=80, RECFM=FB.
    JOBFILE   contains one member for each JOB referenced in SYSIN.
              Each JOB referenced in the JOB=jobname parameter must
              have a corresponding jobname member in the JOBFILE
              dataset. This dataset has DCB characteristics of
              DSORG=PO, LRECL=80, RECFM=FB.
    CMDFILE   contains one member for each CMD referenced in SYSIN.
              Each CMD referenced in the CMD=cmdname parameter must
              have a corresponding cmdname member in the CMDFILE
              dataset. This dataset has DCB chracteristics of
              DSORG=PO, LRECL=80, RECFM=FB.
    CHKPOINT  contains the scheduler checkpoint record. This dataset
              has DCB characteristics mDSORG=PS, LRECL=80, RECFM=F.
              For initial scheduler start-up, this file should contain
              a single record, in columns 1 through 16, of the
              following format:
                 CHKPOINTCHKPOINT
              This indicates to the scheduler that this is a first-time
              start-up and that RECOVER processing should not take
              place.

Here is some example parameter data that could be included in the
SYSIN dataset:

NOTES: 
  - Sample changed by me from what was in the supplied doco to
    what is actually expected by the source code as far as
    starting column numbers that are used
  - What the sample doesn't mention is that if an entry is set
    to repeat during the day every repeat also uses one of the
    JOBMAX time slots sp allow for that also... added a comment
    to the example as a reminder of that.
  - changed actual job and command examples to the samples I
    have provided in the install JCL job
  - This example is also in the SAMPPARM member ready for use.

* ALL COMMENT CARDS START WITH A ASTERISK
* NOTE: FOR JOBMAX AND TIME VALUES, ALL FOUR DIGITS MUST BE CODED
*
* JOBMAX SPECIFIES THE MAXIMUM NUMBER OF JOB= KEYWORDS THAT FOLLOW
* MID: ADDED INFO... MAXIMUM NUMBER OF JOBS/CMDS IN TOTAL, REPEAT
*      JOBS COUNT AS UNIQUE JOBS, IE: IF A JOB REPEATS FIVE TIMES
*      DURING THE DAY THAT IS FIVE JOB SLOTS USED !.
*
* CHECKPOINTINTERVAL SPECIFIES THE NUMBER OF MINUTES BETWEEN
* CHECKPOINT UPDATES (60 IS THE MAXIMUM)
JOBMAX=0030,CHECKPOINTINTERVAL=10
*
* COLUMN 1  MUST CONTAIN THE DAY OF THE WEEK OR 'EVERYDAY'
* COLUMN 11 MUST CONTAIN 'T=' FOLLOWED BY THE FOUR DIGIT TIME VALUE
* COLUMN 18 CAN CONTAIN 'JOB=' FOLLOWED BY THE JOBFILE MEMBER NAME OR
* COLUMN 18 CAN CONTAIN 'CMD=' FOLLOWED BY THE CMDFILE MEMBER NAME
* COLUMN 31 CAN CONTAIN THE 'CONTINUE=' KEYWORD FOLLOWED BY A FOUR
*           DIGIT TIME VALUE (HHMM)
*           THIS PARAMETER REPRESENTS THE TIME INTERVAL FROM THE 'T='
*           TIME AT WHICH THE JOB OR COMMAND IS REISSUED
* COLUMN 45 CAN CONTAIN THE 'RECOVER' KEYWORD
*           THIS PARAMETER INDICATES WHETHER THE JOB OR COMMAND IS
*           TO BE ISSUED IF THE TIME IT WAS SUPPOSED TO OCCUR IS
*           WITHIN THE LAST 24 HRS AND SCHEDULER WAS DOWN AT THAT TIME
*
* ISSUE THE COMMANDS IN THE SAMPCMD1 MEMBER OF CMDFILE AT 03:05 P.M.
* EVERYDAY.
EVERYDAY  T=1505 CMD=SAMPCMD1
*
* ISSUE THE COMMANDS IN THE SAMPCMD2 MEMBER OF CMDFILE AT 03:30 PM.
* EVERY WEEKDAY DAY AND PROCESS THESE COMMANDS IF SCHEDULER FAILED
* PRIOR TO 15:30 AND WAS STARTED AFTER THAT TIME (RECOVER).
MONDAY    T=1530 CMD=SAMPCMD2               RECOVER
TUESDAY   T=1530 CMD=SAMPCMD2               RECOVER
WEDNESDAY T=1530 CMD=SAMPCMD2               RECOVER
THURSDAY  T=1530 CMD=SAMPCMD2               RECOVER
FRIDAY    T=1530 CMD=SAMPCMD2               RECOVER
*
* SUBMIT THE JOB IN THE SAMPJOB1 MEMBER OF JOBFILE AT 00:00 EVERYDAY
* AND RE-SUBMIT THAT SAME JOB EVERY 90 MINUTES (CONTINUE=0130)
* THROUGHOUT THE DAY. (MID: 16 JOB SLOTS USED BY BELOW)
EVERYDAY  T=0000 JOB=SAMPJOB1 CONTINUE=0130
*
./ ADD NAME=CHANGES
CHANGES MADE TO GET THIS WORKING ON MVS3.8J

(1) Replaced use of STIMERM with STIMER as MVS3.8J does not have
    the STIMERM ability (todo one day, create one). STIMERM
    seemed to be mainly used for startup reply handling anyway.
(2) Added the Y2K/EPOC fix to the date returned from TIME DEC to
    stop SOC7 abends on date calculations
(3) Corrected the documentation, the XEPHON article documentation
    had incorrect column numbers in the comments for the parm member
    used (now based on whats actually in the source code)
(4) Updated the year tables used to allow it to run from 2013 
    until 2025 before it needs updating again.
(5) Explicity DOM the WTOR as under MVS3.8J if a timer pops and
    the code carried on the WTOR would still remain outstanding.

ADDITIONAL CHANGES MADE FOR MY USE...
... AND FURTHER CHANGES TO THE CODE I FOUND NEEDED TO GET IT
    WORKING PROPERLY ON MVS3.8J AS I TESTED IT FURTHER.

2013/05/23 - FOR MY USE
(A) Added four additional WTO messaages
   Commands issued appear in syslog when command logging
   is on (and in joblog) but on most consoles there is no
   way of knowing what the scheduler is doing which I think
   is bad, so I have added
 + SCHED031 (in schedm01) and SCHED032 (in schedm02)
   messages to WTO what commands are being issued, as
   provided they would appear to come out of nowhere and
   a command log is handy
 + SCHED033 (in schedm01) and SCHED034 (in schedm02)
   messages to WTO when a JOB or CMD member is being run
   by the scheduler for activity logging

2013/05/25 - MY ERROR, MISSED THAT In THE CUT/PASTE FROM
             THE PDF XEPHOM MAG THAT TRUNCATED SPACES
(A) Fixed trailing spaces count in SCHEDM02 for day names,
    all must be 9 bytes (determined by running the program
    as the fields are DC without length). Didn't change
    the fields to CL9 as I want the code to be as similar
    as possible to the Xephon code so you know what you
    are getting.

2013/05/25 - CODE CHANGE FOR MVS3.8J PROPER FUNCTION
(A) Added the TTIMER CANCEL and DOM of the WTOR in the HARDDOWN
    recovery section as if the timer popped the WTOR would 
    still remain active for the life of the program (or until
    replied to even though the program was no longer waiting
    for a reply). Likewise a WTOR reply would leave the timer
    active even though we had moved on.
    I am not testing to see what event trigged the ECB, just
    cancelling both at this point; may clean that up later.
(B) Adjusted where the member name and CMD/JOB were inserted
    into the HARDDOWN WTOR message, the MVS3.8J WTOR code/macro
    is two bytes shorter than whatever operating system version
    the schedule program was origionally written for so the
    offsets were wrong.

STILL TODO
(a) work out whats supposed to be in the startup message, currently
    shows a dayname OK but then what looks like hex adresses rather
    than meaninfull contents. Address of table start/end instead
    of activity start/end ???, may be what is expected
(b) add a modify command to display the next jobs start time (maybe)
./ ADD NAME=BUGS
KNOWN BUGS AS OF 2013/06/01
===========================
All three bugs are caused by the user (me) doing something stupid
so should not affect most users.

1. ---------------------------------------------------------------
1. Doesn't handle missing DD names properly, but still stops OK
1. IMPACT: none, it is supposed to stop if there is a missing DD
1. ---------------------------------------------------------------
1. Found when I had a JCL oopsie on the command file name
   if the CMDFILE DD is missing, it correctly reports an error in
   the subtask and stops... however it stops with a SA03 abend
   rather than stopping cleanly. So possibly an error handling
   issue in the supplied code somewhere.
   IMPACT: NO IMPACT... if the CMDFILE DD is provided and refers
           to a valid dataset name then this condition will never
           be triggered.

2. ---------------------------------------------------------------
2. Using the 'C SCHEDULE' command to stop SCHEDULE W I L L corrupt
2. the checkpoint file, making SCHEDULE unstartable.
2. However this buf was found as a result of BUG3 so may not
2. always occur.
2. IMPACT: scheduler unusable until the checkpoint file is 
2.         re-initialized to 'CHKPOINTCHKPOINT'
2. ---------------------------------------------------------------
2. Do NOT stop schedule with the cancel command, it is supposed
   to handle it but it definately does not.

   If SCHEDULE is cancelled with a 'C SCHEDULE' instead of stopped
   with a 'P SCHEDULE' the checkpoint file is corrupted and the
   scheduler cannot be restarted without re-initialising the
   checkpoint file.
   BUT: as it only happens if you cancel schedule, which you
        should not do I'm ignoring that for now.
   WARNING: THIS IS REPEATABLE !. Never 'C SCHEDULE'
   (The Abend when scheduler is restarted is S337-04 and the
    checkpoint file cannot be opened by any mathod except
    a direct oevrwrite that I can find).

3. ---------------------------------------------------------------
3. ECB event trigger issues when scheduler is doing something
3  stupid. Goes into total non-responsive mode if you do the below
3. IMPACT: Kills the scheduler, caused bug2, refer to bug2
3. ---------------------------------------------------------------
3. ECB handling issues when scheduler is busy, doing something
   most users will never do.

   If SCHEDULE is asked to run a command member that contains the
   command 'P SCHEDULE', it becomes totally non-responsive. 
   I assume that is because the schedule is waiting for the
   response from the SVC34 used to issue the command so doesn't
   notice the 'park' command and never accepts other ECB triggers
   while the park is still outstanding ?????. Anyway the symptoms
   are
     - schedule keeps right on running but...
     - no further scheduled events are actioned
     - all subsequent 'P SCHEDULE' commands from the console are
       ignored
     - the only way to stop schedule is to cancel it causing the
       checkpoint corruption issue in bug2.

   So don't issue the scheduler park command from within the
   scheduler.

4. ---------------------------------------------------------------
4. Have not found it yet
4. ---------------------------------------------------------------
4. Still bug hunting.
./ ADD NAME=ASSEMBLE
//SUBSCHED JOB (TSO),
//             'SUB ASSEMBLE SCHEDULE',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(0,0),
//             REGION=0K,
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//* SUBMIT THE TWO JOBS TO ASSEMBLE THE TWO PROGRAMS
//* INTO AN APF AUTHORISED LIBRARY.
//*
//SUBJOBS  EXEC PGM=IEBGENER
//SYSPRINT DD   SYSOUT=*
//SYSUT1   DD   DISP=SHR,
//    DSN=SYSGEN.SCHEDULE.SRC(SCHEDM02)
//         DD   DISP=SHR,
//    DSN=SYSGEN.SCHEDULE.SRC(SCHEDM01)
//SYSUT2   DD   SYSOUT=(A,INTRDR)
//SYSIN    DD   DUMMY
//
./ ADD NAME=SAMPJOB
//SAMPJOB  JOB (TSO),
//             'SAMP SCHEDULE WITH SAMPPARM',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(0,0),
//             REGION=0K,
//             USER=IBMUSER,
//             PASSWORD=SYS1
//SCHEDULE EXEC PGM=SCHEDM01
//SYSIN    DD   DISP=SHR,
//  DSN=SYSGEN.SCHEDULE.SRC(SAMPPARM)
//JOBFILE  DD   DISP=SHR,
//  DSN=SYSGEN.SCHEDULE.JOBFILE
//CMDFILE  DD   DISP=SHR,
//  DSN=SYSGEN.SCHEDULE.CMDFILE
//CHKPOINT DD   DISP=SHR,
//  DSN=SYSGEN.SCHEDULE.CHKPOINT
//INTRDR   DD SYSOUT=(A,INTRDR)
//SYSABEND DD SYSOUT=A
./ ADD NAME=SAMPPARM
* ALL COMMENT CARDS START WITH A ASTERISK
* NOTE: FOR JOBMAX AND TIME VALUES, ALL FOUR DIGITS MUST BE CODED
*
* JOBMAX SPECIFIES THE MAXIMUM NUMBER OF JOB= KEYWORDS THAT FOLLOW
* MID: ADDED INFO... MAXIMUM NUMBER OF JOBS/CMDS IN TOTAL, REPEAT
*      JOBS COUNT AS UNIQUE JOBS, IE: IF A JOB REPEATS FIVE TIMES
*      DURING THE DAY THAT IS FIVE JOB SLOTS USED !.
*
* CHECKPOINTINTERVAL SPECIFIES THE NUMBER OF MINUTES BETWEEN
* CHECKPOINT UPDATES (60 IS THE MAXIMUM)
JOBMAX=0030,CHECKPOINTINTERVAL=10
*
* COLUMN 1  MUST CONTAIN THE DAY OF THE WEEK OR 'EVERYDAY'
* COLUMN 11 MUST CONTAIN 'T=' FOLLOWED BY THE FOUR DIGIT TIME VALUE
* COLUMN 18 CAN CONTAIN 'JOB=' FOLLOWED BY THE JOBFILE MEMBER NAME OR
* COLUMN 18 CAN CONTAIN 'CMD=' FOLLOWED BY THE CMDFILE MEMBER NAME
* COLUMN 31 CAN CONTAIN THE 'CONTINUE=' KEYWORD FOLLOWED BY A FOUR
*           DIGIT TIME VALUE (HHMM)
*           THIS PARAMETER REPRESENTS THE TIME INTERVAL FROM THE 'T='
*           TIME AT WHICH THE JOB OR COMMAND IS REISSUED
* COLUMN 45 CAN CONTAIN THE 'RECOVER' KEYWORD
*           THIS PARAMETER INDICATES WHETHER THE JOB OR COMMAND IS
*           TO BE ISSUED IF THE TIME IT WAS SUPPOSED TO OCCUR IS
*           WITHIN THE LAST 24 HRS AND SCHEDULER WAS DOWN AT THAT TIME
*
* ISSUE THE COMMANDS IN THE SAMPCMD1 MEMBER OF CMDFILE AT 03:05 P.M.
* EVERYDAY.
EVERYDAY  T=1505 CMD=SAMPCMD1
*
* ISSUE THE COMMANDS IN THE SAMPCMD2 MEMBER OF CMDFILE AT 03:30 PM.
* EVERY WEEKDAY DAY AND PROCESS THESE COMMANDS IF SCHEDULER FAILED
* PRIOR TO 05:30 AND WAS STARTED AFTER THAT TIME (RECOVER).
MONDAY    T=1530 CMD=SAMPCMD2               RECOVER
TUESDAY   T=1530 CMD=SAMPCMD2               RECOVER
WEDNESDAY T=1530 CMD=SAMPCMD2               RECOVER
THURSDAY  T=1530 CMD=SAMPCMD2               RECOVER
FRIDAY    T=1530 CMD=SAMPCMD2               RECOVER
*
* SUBMIT THE JOB IN THE SAMPJOB1 MEMBER OF JOBFILE AT 00:00 EVERYDAY
* AND RE-SUBMIT THAT SAME JOB EVERY 90 MINUTES (CONTINUE=0130)
* THROUGHOUT THE DAY. (MID: 16 JOB SLOTS USED BY BELOW)
EVERYDAY  T=0000 JOB=SAMPJOB1 CONTINUE=0130
*
./ ADD NAME=SAMPPROC
//SCHEDULE PROC PREFIX='SYSGEN.SCHEDULE',     SCHEDULR DS PREFIX
// PARMS='SAMPPARM',                          PARMLIB MEMBER NAME
// PARMLIB='SYSGEN.SCHEDULE.SRC',            SYSTEM PARMLIB TO USE
// JOBQ='A'                                   INTRDR JOB CLASS FOR JOBS
//SCHEDULE EXEC PGM=SCHEDM01
//SYSIN    DD   DISP=SHR,
//  DSN=&PARMLIB(&PARMS)
//JOBFILE  DD   DISP=SHR,
//  DSN=&PREFIX..JOBFILE
//CMDFILE  DD   DISP=SHR,
//  DSN=&PREFIX..CMDFILE
//CHKPOINT DD   DISP=SHR,
//  DSN=&PREFIX..CHKPOINT
//INTRDR   DD SYSOUT=(&JOBQ,INTRDR)
//*SYSABEND DD SYSOUT=A
./ ADD NAME=SCHEDM01
//AS1SCHED JOB (TSO),
//             'ASSEMBLE SCHEDULE',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(2,1),
//             REGION=0K,
//             USER=IBMUSER,
//             PASSWORD=SYS1
//ASM1     EXEC PGM=IFOX00,
//   PARM='DECK,LOAD,TERM,TEST,SYSPARM((NOSP,NODEBUG)),XREF(SHORT)',
//   REGION=4096K,COND=(0,NE)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=SYS2.MACLIB
//SYSUT1   DD  SPACE=(CYL,(25,5)),UNIT=SYSDA
//SYSUT2   DD  SPACE=(CYL,(25,5)),UNIT=SYSDA
//SYSUT3   DD  SPACE=(CYL,(25,5)),UNIT=SYSDA
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DATA,DLM=ZZ
***********************************************************************
* SCHEDM01
*  MID REPLACED THE STIMERM CALLS WITH STIMER AS STIMERM IS NOT
*      AVAILABLE IN 38J. THIS MAY CAUSE ISSUES AS ONLY ONE STIMER
*      CAN BE ACTIVE AT A TIME. ALTHOUGH IT LOOKS LIKE ONLY ONE
*      TASK IS SCHEDULED AT A TIME.
***********************************************************************
         PRINT  ON,GEN
         SPACE
SCHEDM01 CSECT
         YREGS
         USING SCHEDM01,R12,R11
         STM   R14,R12,12(R13)
         LR    R12,R15                       SET INITIAL BASE
         LA    R11,4095(,R12)
         LA    R11,1(,R11)
         ST    R13,SAVE+4
         LA    R13,SAVE
***********************************************************************
SKIPID OPEN SYSIN                            OPEN SYSIN DATASET
         LA    R10,SYSIN                     ADDRESS DCB
         USING IHADCB,R10
         TM    DCBOFLGS,X'10'                OPEN OK?
         BZ    OPENERR1                      NO - INDICATE ERROR
         DROP  R10
*
SYSINLP1 GET   SYSIN                         READ A RECORD
         CLC   0(7,R1),=C'JOBMAX='           JOB MAX CARD?
         BE    JOBMAXFN                      YES - PROCESS
         B     SYSINLP1                      NO - TRY NEXT
*
EOD1     CLOSE (SYSIN)                       CLOSE SYSIN DATASET
         L     R3,=F'100'                    SET DEFAULT JOB MAX
         L     R8,=F'5'                      SET DEFAULT CHKPOINT INT.
         B     STRGCAL                       AND CONTINUE
*
JOBMAXFN PACK  DOUBLE+5(3),7(4,R1)     YES - THEN PACK MAX JOB NUMBER
         CVB   R3,DOUBLE                     CONVERT TO BINARY
         XC    DOUBLE(8),DOUBLE              CLEAR THE AREA
         L     R8,=F'5'                      SET DEFAULT CHKPOINT INT.
         CLC   12(19,R1),=C'CHECKPOINTINTERVAL=' LONG KEYWORD?
         BNE   STRGCAL                       NO - PROCESS DEFAULT
         TM    31(R1),X'F0'                  NUMERIC?
         BNO   CHKPTERR                      NO - ISSUE ERROR WTO
         TM    32(R1),X'F0'                  NUMERIC?
         BNO   CHKPTERR                      NO - ISSUE ERROR WTO
         PACK  DOUBLE+6(2),31(2,R1)          PACK CHECKPOINT INTERVAL
         CVB   R8,DOUBLE                     CONVERT TO BINARY
         C     R8,=F'60'                     MORE THAN AN HOUR?
         BNH   STRGCAL                       NO - EVERYTHING IS OK
         WTO   'SCHED010 - CHECKPOINT INTERVAL REQUESTED IS MORE THAN 6X
               0 MINUTES. MAXIMUM IS 60 MINUTES. RESET TO MAXIMUM.',   X
               ROUTCDE=(1),DESC=(1)
         L     R8,=F'60'                     SET MAX. CHKPOINT INT.
         B     STRGCAL                       GO GET STORAGE
CHKPTERR EQU   *
         WTO   'SCHED011 - CHECKPOINT INTERVAL REQUESTED IS NOT A NUMERX
               IC VALUE. A 5 MINUTE DEFAULT IS USED.',                 X
               ROUTCDE=(1),DESC=(1)
         L     R8,=F'5'                      SET DEFAULT CHKPOINT INT.
STRGCAL  LR    R2,R3                         SAVE AMOUNT
         MH    R3,=H'38'                 GET AMOUNT OF STORAGE REQUIRED
         LR    R9,R2                         SAVE JOBMAX
         ST    R8,CHKPTINT                   SAVE CHECKPOINT INTERVAL
         GETMAIN RC,LV=(R3)                  GET STORAGE
         LTR   R15,R15                       OK?
         BNZ   GETERR                        NO - BIG PROBLEM
         LR    R7,R1                         ADDRESS STORAGE
         AR    R7,R3                         ADDRESS LAST BYTE
         ST    R7,STORLIM                    SAVE LAST BYTE ADDRESS
         LR    R4,R1                         ADDRESS STORAGE
         LR    R5,R3                         GET LENGTH
         SR    R6,R6                         DUMMY ADDRESS
         SR    R7,R7                         ZERO SECOND LENGTH
         MVCL  R4,R6                         CLEAR AREA
         MH    R2,=H'4'                      GET POINTER STORAGE SIZE
         ST    R1,DAYPTRS+0                  STORE SUNDAY POINTER
         AR    R1,R2                         ADDRESS NEXT DAY
         ST    R1,DAYPTRS+4                  STORE MONDAY POINTER
         AR    R1,R2                         ADDRESS NEXT DAY
         ST    R1,DAYPTRS+8                  STORE TUESDAY POINTER
         AR    R1,R2                         ADDRESS NEXT DAY
         ST    R1,DAYPTRS+12                 STORE WEDNESDAY POINTER
         AR    R1,R2                         ADDRESS NEXT DAY
         ST    R1,DAYPTRS+16                 STORE THURSDAY POINTER
         AR    R1,R2                         ADDRESS NEXT DAY
         ST    R1,DAYPTRS+20                 STORE FRIDAY POINTER
         AR    R1,R2                         ADDRESS NEXT DAY
         ST    R1,DAYPTRS+24                 STORE SATURDAY POINTER
         MVC   NXTDPTRS(28),DAYPTRS          NEXT AVAIL IS SET TO FIRST
         AR    R1,R2                         ADDRESS FREE AREA
         ST    R1,JOBPTRS                    STORE JOB AREA ADDRESS
         ST    R1,NXTJPTRS                   NEXT AVAIL IS SET TO FIRST
         LR    R1,R9                         GET JOBMAX VALUE
         SLL   R1,2                          MULTIPLY BY FOUR
         ST    R1,DAYLEN                SAVE LENGTH OF DAY AREA STORAGE
*
         A     R9,=F'1'                      ADD ONE TO JOBMAX
         SLL   R9,4                          MULTIPLY BY SIXTEEN
         GETMAIN RC,LV=(R9)                  GET STORAGE
         LTR   R15,R15                       GOT IT OK?
         BNZ   GETERR                        NO - WE BETTER QUIT
         ST    R1,RECOVTBL                   SAVE TABLE ADDRESS
         LR    R0,R1                         MOVE ADDRESS
         LR    R1,R9                         SET LENGTH
         XR    R14,R14                       DUMMY ADDRESS
         XR    R15,R15                       SET PATTERN BYTE
         MVCL  R0,R14                        CLEAR STORAGE AREA
*
         OPEN  (CHKPOINT,INPUT)              OPEN CHECKPOINT DATASET
         TM    CHKPOINT+48,X'10'             OPEN OK?
         BZ    OPENERR3                      NO - ISSUE ERROR MESSAGE
         GET   CHKPOINT,CHKPTREC             READ CHECKPOINT RECORD
         CLC   CHKPTREC(8),=C'CHKPOINT'      CHKPOINT INIT RECORD?
         BE    CHKPTOFF                      YES - THE FIRST TIME
         CLC   CHKPTREC+8(8),=C'CHKPOINT'    CHKPOINT INIT RECORD?
         BE    CHKPTOFF                      YES - THE FIRST TIME
         OI    FLAG,ACTIVE                   SET ACTIVE FLAG
CHKPTOFF CLOSE CHKPOINT                      CLOSE CHECKPOINT DATASET
***********************************************************************
         MVC   SYSIN+33(3),=AL3(EOD2)        SET NEW EOD ADDRESS
         OPEN  SYSIN                         OPEN SYSIN DATASET
         LA    R10,SYSIN                     ADDRESS DCB
         USING IHADCB,R10
         TM    DCBOFLGS,X'10'                OPEN OK?
         BZ    OPENERR1                      NO - INDCIATE ERROR
         DROP  R10
SYSINLP2 GET   SYSIN                         READ A RECORD
         LR    R10,R1                        ADDRESS INPUT RECORD
         USING PARMIN,R10                    ADDRESSIBILITY
         CLI   COMMENT,C'*'                  COMMENT?
         BE    SYSINLP2                      YES - TRY NEXT
         NI    FLAG,255-DAILY                CLEAR FLAG
         CLC   TIMEID,=C'T='                 A VALID TIME PARM?
         BNE   BADPARM                       NO - INDICATE SO
         TM    RELTIME,X'F0'                 NUMERIC?
         BNO   BADPARM                       NO - BAD STUFF
         TM    RELTIME+1,X'F0'               NUMERIC?
         BNO   BADPARM                       NO - BAD STUFF
         TM    RELTIME+2,X'F0'               NUMERIC?
         BNO   BADPARM                       NO - BAD STUFF
         TM    RELTIME+3,X'F0'               NUMERIC?
         BNO   BADPARM                       NO - BAD STUFF
         PACK  RELVALUE,RELTIME              PACK RELEASE TIME
         CLC   JOBID,=C'JOB='                JOBFILE MEMBER?
         BE    CHKFIELD                      YES - GO CHECK FIELD INFO
         CLC   JOBID,=C'CMD='                CMDFILE MEMBER?
         BNE   BADPARM                       NO - ISSUE ERROR
CHKFIELD EQU   *
         CLI   JOB,C' '                      JOB PRESENT?
         BE    BADPARM                       NO - BAD STUFF
         NI    FLAG,255-CON                  TURN FLAG OFF
         CLC   CONTID,=C'CONTINUE='          CONTINUE REQUEST?
         BNE   NOCONT                        NO - JUST SKIP IT
         TM    CONTIME,X'F0'                 NUMERIC?
         BNO   BADPARM                       NO - BAD STUFF
         TM    CONTIME+1,X'F0'               NUMERIC?
         BNO   BADPARM                       NO - BAD STUFF
         TM    CONTIME+2,X'F0'               NUMERIC?
         BNO   BADPARM                       NO - BAD STUFF
         TM    CONTIME+3,X'F0'               NUMERIC?
         BNO   BADPARM                       NO - BAD STUFF
         PACK  CONVALH,CONTIME(2)            PACK CONTINUE TIME HOURS
         MP    CONVALH,=P'100'               PUSH HOURS OVER
         PACK  CONVALM,CONTIME+2(2)          PACK CONTINUE TIME MINUTES
         OI    FLAG,CON                      YES - SET FLAG
NOCONT   L     R1,NXTJPTRS                  ADDRESS NEXT AVAILABLE SPOT
         C     R1,STORLIM                    ALL STORAGE BEEN USED?
         BNL   STORUSE                       YES - THEN NO MORE
         CLC   DAY,=CL9'EVERYDAY '           DAILY JOB?
         BNE   CHK2                          NO - GO CHECK ON NEXT
         OI    FLAG,DAILY                    YES - SET FLAG
         B     SUNSET                        AND GO SET POINTERS
CHK2     CLC   DAY,=CL9'SUNDAY   '           SUNDAY JOB?
         BE    SUNSET                        YES - GO SET UP
         CLC   DAY,=CL9'MONDAY   '           MONDAY JOB?
         BE    MONSET                        YES - GO SET UP
         CLC   DAY,=CL9'TUESDAY  '           TUESDAY JOB?
         BE    TUESET                        YES - GO SET UP
         CLC   DAY,=CL9'WEDNESDAY'           WEDNESDAY JOB?
         BE    WEDSET                        YES - GO SET UP
         CLC   DAY,=CL9'THURSDAY '           THURSDAY JOB?
         BE    THUSET                        YES - GO SET UP
         CLC   DAY,=CL9'FRIDAY   '           FRIDAY JOB?
         BE    FRISET                        YES - GO SET UP
         CLC   DAY,=CL9'SATURDAY '           SATURDAY JOB?
         BE    SATSET                        YES - GO SET UP
         B     BADPARM 
SUNSET   L     R2,NXTDPTRS+0                 GET NEXT FREE POINTER AREA
         ST    R1,0(,R2)                     SET POINTER
*
         CLC   RECOVID(7),=C'RECOVER'        RECOVER ON?
         BNE   NORECOV1                      NO - DON'T SET RECOVER
         OI    0(R2),X'80'                   SET FLAG IN POINTER
NORECOV1 EQU   *
         CLC   JOBID,=C'CMD='                A COMMAND?
         BNE   NOCMD1                        NO - DON'T SET COMMAND
         OI    0(R2),X'40'                   SET FLAG IN POINTER
NOCMD1   EQU   *
*
         LA    R2,4(,R2)                 ADDRESS NEXT FREE
         ST    R2,NXTDPTRS+0             AND SAVE NEXT FREE ADDRESS
         TM    FLAG,DAILY                DAILY JOB?
         BNO   JOBIN                     NO - THEN GO BRING JOB NAME IN
*
MONSET   L     R2,NXTDPTRS+4             GET NEXT FREE POINTER AREA
         ST    R1,0(,R2)                 SET POINTER
*
         CLC   RECOVID(7),=C'RECOVER'    RECOVER ON?
         BNE   NORECOV2                  NO - DON'T SET RECOVER
         OI    0(R2),X'80'               SET FLAG IN POINTER
NORECOV2 EQU   *
         CLC   JOBID,=C'CMD='            A COMMAND?
         BNE   NOCMD2                    NO - DON'T SET COMMAND
         OI    0(R2),X'40'               SET FLAG IN POINTER
NOCMD2   EQU   *
*
         LA    R2,4(,R2)                 ADDRESS NEXT FREE
         ST    R2,NXTDPTRS+4             AND SAVE NEXT FREE ADDRESS
         TM    FLAG,DAILY                DAILY JOB?
         BNO   JOBIN                     NO - THEN GO BRING JOB NAME IN
TUESET   L     R2,NXTDPTRS+8             GET NEXT FREE POINTER AREA
         ST    R1,0(,R2)                 SET POINTER
*
         CLC   RECOVID(7),=C'RECOVER'    RECOVER ON?
         BNE   NORECOV3                  NO - DON'T SET RECOVER
         OI    0(R2),X'80'               SET FLAG IN POINTER
NORECOV3 EQU *
         CLC JOBID,=C'CMD='              A COMMAND?
         BNE NOCMD3                      NO - DON'T SET COMMAND
         OI 0(R2),X'40'                  SET FLAG IN POINTER
NOCMD3   EQU   *
*
         LA    R2,4(,R2)                 ADDRESS NEXT FREE
         ST    R2,NXTDPTRS+8             AND SAVE NEXT FREE ADDRESS
         TM    FLAG,DAILY                DAILY JOB?
         BNO   JOBIN                     NO - THEN GO BRING JOB NAME IN
WEDSET   L     R2,NXTDPTRS+12            GET NEXT FREE POINTER AREA
         ST    R1,0(,R2)                 SET POINTER
*
         CLC   RECOVID(7),=C'RECOVER'    RECOVER ON?
         BNE   NORECOV4                  NO - DON'T SET RECOVER
         OI    0(R2),X'80'               SET FLAG IN POINTER
NORECOV4 EQU   *
         CLC   JOBID,=C'CMD='            A COMMAND?
         BNE   NOCMD4                    NO - DON'T SET COMMAND
         OI    0(R2),X'40'               SET FLAG IN POINTER
NOCMD4   EQU   *
*
         LA    R2,4(,R2)                 ADDRESS NEXT FREE
         ST    R2,NXTDPTRS+12            AND SAVE NEXT FREE ADDRESS
         TM    FLAG,DAILY                DAILY JOB?
         BNO   JOBIN                     NO - THEN GO BRING JOB NAME IN
THUSET   L     R2,NXTDPTRS+16            GET NEXT FREE POINTER AREA
         ST    R1,0(,R2)                 SET POINTER
*
         CLC   RECOVID(7),=C'RECOVER'    RECOVER ON?
         BNE   NORECOV5                  NO - DON'T SET RECOVER
         OI    0(R2),X'80'               SET FLAG IN POINTER
NORECOV5 EQU   *
         CLC   JOBID,=C'CMD='            A COMMAND?
         BNE   NOCMD5                    NO - DON'T SET COMMAND
         OI    0(R2),X'40'               SET FLAG IN POINTER
NOCMD5   EQU   *
*
         LA    R2,4(,R2)                 ADDRESS NEXT FREE
         ST    R2,NXTDPTRS+16            AND SAVE NEXT FREE ADDRESS
         TM    FLAG,DAILY                DAILY JOB?
         BNO   JOBIN                     NO - THEN GO BRING JOB NAME IN
*
FRISET   L     R2,NXTDPTRS+20            GET NEXT FREE POINTER AREA
         ST    R1,0(,R2)                 SET POINTER
*
         CLC   RECOVID(7),=C'RECOVER'    RECOVER ON?
         BNE   NORECOV6                  NO - DON'T SET RECOVER
         OI    0(R2),X'80'               SET FLAG IN POINTER
NORECOV6 EQU   *
         CLC   JOBID,=C'CMD='            A COMMAND?
         BNE   NOCMD6                    NO - DON'T SET COMMAND
         OI    0(R2),X'40'               SET FLAG IN POINTER
NOCMD6   EQU   *
*
         LA    R2,4(,R2)                 ADDRESS NEXT FREE
         ST    R2,NXTDPTRS+20            AND SAVE NEXT FREE ADDRESS
         TM    FLAG,DAILY                DAILY JOB?
         BNO   JOBIN                     NO - THEN GO BRING JOB NAME IN
SATSET   L     R2,NXTDPTRS+24            GET NEXT FREE POINTER AREA
         ST    R1,0(,R2)                 SET POINTER
*
         CLC   RECOVID(7),=C'RECOVER'    RECOVER ON?
         BNE   NORECOV7                  NO - DON'T SET RECOVER
         OI    0(R2),X'80'               SET FLAG IN POINTER
NORECOV7 EQU   *
         CLC   JOBID,=C'CMD='            A COMMAND?
         BNE   NOCMD7                    NO - DON'T SET COMMAND
         OI    0(R2),X'40'               SET FLAG IN POINTER
NOCMD7   EQU   *
*
         LA    R2,4(,R2)                 ADDRESS NEXT FREE
         ST    R2,NXTDPTRS+24            AND SAVE NEXT FREE ADDRESS
JOBIN    SR    R9,R9                     CLEAR REG
         ICM   R9,B'0111',RELVALUE       GET RELEASE TIME
         SRL   R9,4                      DROP SIGN
         STH   R9,0(,R1)                 STORE RELEASE TIME
         MVC   2(8,R1),JOB               BRING IN JOB NAME
         LA    R1,10(,R1)                ADDRESS NEXT AVAILABLE
         ST    R1,NXTJPTRS               AND STORE IT
         TM    FLAG,CON                  CONTINUE?
         BNO   SYSINLP2                  NO - GET NEXT CARD
         SR    R9,R9                     CLEAR REGISTER
         ICM   R9,B'0011',RELVALUE+1     GET HMMF FROM 0HHMMF
         N     R9,=X'00000FFF'           GET 0MMF FROM HMMF
         XC    WORK,WORK                 CLEAR AREA
         STH   R9,WORK+3                 STORE 0MMF
         AP    WORK,CONVALM              ADD CONTINUE MINUTES
         DP    WORK,=PL2'60'             GET MINUTES AND HOURS
* WORK -> 0000HF,0MMF
         NC    RELVALUE+1(2),=X'F00F'    CLEAR PREVIOUS MINUTES
         OC    RELVALUE+1(2),WORK+3      SET NEW MINUTES
         MP    WORK(3),=P'100'           SHIFT HOURS OVER
         AP    RELVALUE,WORK(3)          ADD ON HOUR FROM MINUTES ADD
         AP    RELVALUE,CONVALH          ADD ON CONTINUE TIME HOURS
         CP    RELVALUE,=P'2359'         PAST END OF THE DAY?
         BNH   NOCONT                    NO - THEN CONTINUE
         B     SYSINLP2                  GET NEXT CARD
******************************************************************
BADPARM MVC    BADPMSG+34(30),DAY        MOVE PARM OUT
BADPMSG WTO    'SCHED012 - PARM IGNORED - X                            X
               ',ROUTCDE=(1),DESC=(1)
         B     SYSINLP2                 GET NEXT CARD
******************************************************************
STORUSE  MVC   STORWTO+41(8),JOB        MOVE JOB NAME OUT
         CLC   JOBID,=C'CMD='           IS IT A COMMAND?
         BNE   STORWTO                  NO - GO ISSUE THE MSG AS IS
         MVC   STORWTO+37(3),=C'CMD'    MOVE JOB NAME OUT
STORWTO  WTO   'SCHED013 - JOBMAX EXCEEDED - JOB XXXXXXXX AND FOLLOWINGX
               JOBS/CMDS IGNORED',ROUTCDE=(1),DESC=(1)
EOD2     CLOSE (SYSIN,FREE)             CLOSE SYSIN DATASET
         DROP  R10
******************************************************************
         L     R1,=F'0'                 SET FOR FIRST DAY
SORTSTRT L     R6,NXTDPTRS(R1)          ADDRESS END OF LIST
         S     R6,=F'4'                 LESS ONE ENTRY
         L     R5,DAYPTRS(R1)           ADDRESS START OF LIST
SORT     L     R3,0(,R5)                ADDRESS FIRST ENTRY
         L     R4,4(,R5)                ADDRESS NEXT ENTRY
         CLC   0(2,R3),0(R4)            NEXT ENTRY LESS THAN THIS
         BNH   NOSWAP                   NO - SKIP SWITCH
         ST    R3,4(,R5)                NEXT BECOMES FIRST
         ST    R4,0(,R5)                FIRST BECOMES NEXT
NOSWAP   A     R5,=F'4'                 ADDRESS NEXT SORT ENTRY
         CR    R5,R6                    AT END OF LIST?
         BL    SORT                     NO - CONTINUE
         S     R6,=F'4'                 LESS ONE ENTRY
         C     R6,DAYPTRS(R1)           FINISHED?
         BNH   FINISHED                YES - PUT ENTRIES BACK IN BLOCKS
         L     R5,DAYPTRS(R1)           ADDRESS START OF LIST
         B     SORT                     AND DO SOME MORE
FINISHED C     R1,=F'24'                COMPLETED ALL DAYS?
         BE    GETDAY                   YES - THEN FINISHED
         LA    R1,4(,R1)                ADDRESS NEXT DAY
         B     SORTSTRT                 AND CONTINUE
******************************************************************
* DAY OF WEEK CALC
GETDAY   TIME  DEC                      GET TIME AND DATE
         ST    R1,DATE                  STORE DATE
         ST    R0,TIME                  STORE TIME
         AP    DATE(4),=P'1900000'      Y2K:HERCULES MVS38J FIX
* INDEX YEAR TABLE
GETDAY2  LH    R2,DATE                  LOAD YEAR
         LA    R3,YEARTAB               ADDRESS YEAR TABLE
YEARSRCH CLM   R2,B'0001',0(R3)         YEAR FOUND?
         BE    YEARFND                  YES - GO PROCESS
         CLI   0(R3),X'FF'              NO - END OF TABLE?
         BE    EXPIRED                  YES - THE PROGRAM TABLE EXPIRED
         LA    R3,2(,R3)                NO - ADDRESS NEXT ENTRY
         B     YEARSRCH
YEARFND  SR    R2,R2                    CLEAR REGISTER
         IC    R2,1(,R3)                GET STARTING DAY OF YEAR
         XC    DOUBLE,DOUBLE            CLEAR DOUBLEWORD
         MVC   DOUBLE+6(2),DATE+2       MOVE IN DAY
         CVB   R1,DOUBLE                CONVERT DAY TO BINARY
         SR    R0,R0                    CLEAR EVEN REGISTER
         D     R0,=F'7'                 DIVIDE BY 7 (DAYS IN A WEEK)
         LR    R1,R0                    MOVE REMAINDER
         S     R1,=F'1'                 REMAINDER MINUS ONE
         AR    R1,R2                    PLUS STARTING DAY OF YEAR
         C     R1,=F'-1'                IS IT NEGATIVE?
         BNE   GETDAY3                  NO - THEN NO PROBLEMS
         L     R1,=F'6'                 SET TO SATURDAY
GETDAY3  IC    R1,DAYTABLE(R1)          GET CURRENT DAY OF THE WEEK
         STC   R1,CURRDAY               AND STORE IT
         MVC   JULDAY,DATE+2            SAVE JULIAN DAY
******************************************************************
* FIND FIRST JOB TO BE SUBMITTED
         SR    R3,R3                    CLEAR REGISTER
         IC    R3,CURRDAY               GET CURRENT DAY
         MH    R3,=H'4'                 GET DAY POINTER OFFSET
         L     R2,DAYPTRS(R3)           ADDRESS TODAYS WORK
*
         TM    FLAG,ACTIVE              CHKPOINT ACTIVE?
         BZ    SETTIMER                 NO - DON'T RECOVER
*
         MVC   TEMPDAT1(4),CHKPTREC+4   MOVE THE DATE
         MVC   TEMPDAT1+4(4),CHKPTREC   MOVE THE TIME
         MVC   TEMPDAT2(4),CHKPTREC+12  MOVE THE DATE
         MVC   TEMPDAT2+4(4),CHKPTREC+8 MOVE THE TIME
         CLC   TEMPDAT2(8),TEMPDAT1     TERM TIME>CHKPT TIME
         BH    TERMSOFT                 YES - SOFT SHUTDOWN
         OI    FLAG,HARDDOWN            SET HARD DOWN FLAG
TERMSOFT EQU   *
         MVC   CHKPTREC(8),CHKPTREC+8   MOVE IN TERMINATION TIME
*
         TIME  DEC                      GET CURRENT DATE & TIME
         STM   R0,R1,CURRTIME           SAVE IT
         CLC   CURRDATE(4),CHKPTREC+4   SAME DATE?
         BNE   DAYWRAP                  NO - DAY WRAP SINCE CHKPT
         L     R8,DAYPTRS(R3)           ADDRESS CURRENT DAYS WORK
         L     R14,DAYLEN               GET MAX DAY STORAGE
         LA    R14,0(R14,R8)            SET TO END OF DAY
         ST    R14,DAYEND               SAVE IT
JOBLOOP1 L     R10,0(,R8)               GET FIRST UNIT OF WORK
         LTR   R10,R10                  ANYTHING?
         BZ    RECOVEND                 NO - NO MORE TO DO
         C     R8,DAYEND                DONE THE DAY?
         BE    RECOVEND                 YES - NO MORE TO DO
         TM    0(R8),X'80'              RECOVER FLAG?
         BZ    NEXTJOB1                 NO - GET NEXT JOB
         CLC   0(2,R10),CHKPTREC        BEFORE LAST CHECKPOINT?
         BNH   NEXTJOB1                 YES - DON'T RECOVER
         CLC   0(2,R10),CURRTIME        AFTER CURRENT TIME?
         BNL   RECOVEND                 YES - THAT'S ALL WE NEED
         L     R9,RECOVTBL              GET RECOVERY TBL ADDR
TBLLOOP1 CLC   0(8,R9),=8X'00'          BLANK ENTRY?
         BE    ADDJOB1                  YES - GO ADD TO RECOV TBL
         CLC   0(8,R9),2(R10)           SAME JOB?
         BE    CHKFLAG1                 YES - GO SEE IF CMD OR JOB
         LA    R9,16(,R9)               POINT TO NEXT ENTRY
         B     TBLLOOP1                 GO CHECK IT OUT
ADDJOB1  MVC   0(8,R9),2(R10)           MOVE IN JOB
         TM    0(R8),X'40'              A COMMAND?
         BZ    SETJOB1                  NO - SET JOB FLAG
SETCMD1  OI    8(R9),X'40'              SET FLAG IN RECOVER AREA
         B     NEXTJOB1                 GO CHECK NEXT ENTRY
SETJOB1  OI    8(R9),X'80'              SET FLAG IN RECOVER AREA
NEXTJOB1 LA    R8,4(,R8)                POINT TO NEXT ENTRY
         B     JOBLOOP1                 CHECK IT OUT
CHKFLAG1 EQU   *
         TM    8(R9),X'C0'              BOTH FLAGS SET ALREADY?
         BO    NEXTJOB1                 YES - CAN'T DO ANY MORE
         TM    0(R8),X'40'              A COMMAND?
         BO    SETCMD1                  YES - GO SET COMMAND FLAG
         B     SETJOB1                  NO - GO SET JOB FLAG
DAYWRAP  EQU   *
         CLC   CHKPTREC(4),CURRTIME     MORE THAN ONE DAY?
         BH    WITHIN24                 NO - WITHIN 24 HOURS
         MVC   CHKPTREC(4),CURRTIME     RESET TO TODAYS TIME
WITHIN24 EQU   *
         LTR   R3,R3                    CURRENT DAY IS SUNDAY?
         BZ    SATPREV                  YES - SET PREVIOUS TO SAT
         LR    R10,R3                   SAVE VALUE
         S     R10,=F'4'                POINT TO PREVIOUS DAY
         L     R8,DAYPTRS(R10)          ADDRESS YESTERDAY'S WORK
         L     R14,DAYLEN               GET MAX DAY STORAGE
         LA    R14,0(R14,R8)            SET TO END OF DAY
         ST    R14,DAYEND               SAVE IT
         B     JOBLOOP2                 GO CHECK FOR BACKLOG
SATPREV  L     R10,=F'24'               SET TO SATURDAY
         L     R8,DAYPTRS(R10)          ADDRESS YESTERDAY'S WORK
         L     R14,DAYLEN               GET MAX DAY STORAGE
         LA    R14,0(R14,R8)            SET TO END OF DAY
         ST    R14,DAYEND               SAVE IT
JOBLOOP2 L     R10,0(,R8)               GET FIRST UNIT OF WORK
         LTR   R10,R10                  ANYTHING?
         BZ    DOTODAY                  NO - RECOVER TODAY
         C     R8,DAYEND                DONE THE DAY?
         BE    DOTODAY                  YES - RECOVER TODAY
         TM    0(R8),X'80'              RECOVER FLAG?
         BZ    NEXTJOB2                 NO - GET NEXT JOB
         CLC   0(2,R10),CHKPTREC        BEFORE LAST CHECKPOINT?
         BNH   NEXTJOB2                 YES - DON'T RECOVER
         L     R9,RECOVTBL              GET RECOVERY TBL ADDR
TBLLOOP2 CLC   0(8,R9),=8X'00'          BLANK ENTRY?
         BE    ADDJOB2                  YES - GO ADD TO RECOV TBL
         CLC   0(8,R9),2(R10)           SAME JOB?
         BE    CHKFLAG2                 YES - GO SEE IF CMD OR JOB
         LA    R9,16(,R9)               POINT TO NEXT ENTRY
         B     TBLLOOP2                 GO CHECK IT OUT
ADDJOB2  MVC   0(8,R9),2(R10)           MOVE IN JOB
         TM    0(R8),X'40'              A COMMAND?
         BZ    SETJOB2                  NO - SET JOB FLAG
SETCMD2  OI    8(R9),X'40'              SET FLAG IN RECOVER AREA
         B     NEXTJOB2                 GO CHECK NEXT ENTRY
SETJOB2  OI    8(R9),X'80'              SET FLAG IN RECOVER AREA
NEXTJOB2 LA    R8,4(,R8)                POINT TO NEXT ENTRY
         B     JOBLOOP2                 CHECK IT OUT
CHKFLAG2 EQU   *
         TM    8(R9),X'C0'              BOTH FLAGS SET ALREADY?
         BO    NEXTJOB2                 YES - CAN'T DO ANY MORE
         TM    0(R8),X'40'              A COMMAND?
         BO    SETCMD2                  YES - GO SET COMMAND FLAG
         B     SETJOB2                  NO - GO SET JOB FLAG
DOTODAY  EQU   *
         L     R8,DAYPTRS(R3)           ADDRESS CURRENT DAYS WORK
         L     R14,DAYLEN               GET MAX DAY STORAGE
         LA    R14,0(R14,R8)            SET TO END OF DAY
         ST    R14,DAYEND               SAVE IT
JOBLOOP3 L     R10,0(,R8)               GET FIRST UNIT OF WORK
         LTR   R10,R10                  ANYTHING?
         BZ    RECOVEND                 NO - NO MORE TO DO
         C     R8,DAYEND                DONE THE DAY?
         BE    RECOVEND                 YES - NO MORE TO DO
         TM    0(R8),X'80'              RECOVER FLAG?
         BZ    NEXTJOB3                 NO - GET NEXT JOB
         CLC   0(2,R10),CURRTIME        AFTER CURRENT TIME?
         BNL   RECOVEND                 YES - THAT'S ALL WE NEED
         L     R9,RECOVTBL              GET RECOVERY TBL ADDR
TBLLOOP3 CLC   0(8,R9),=8X'00'          BLANK ENTRY?
         BE    ADDJOB3                  YES - GO ADD TO RECOV TBL
         CLC   0(8,R9),2(R10)           SAME JOB?
         BE    CHKFLAG3                 YES - GO SEE IF CMD OR JOB
         LA    R9,16(,R9)               POINT TO NEXT ENTRY
         B     TBLLOOP3                 GO CHECK IT OUT
ADDJOB3  MVC   0(8,R9),2(R10)           MOVE IN JOB
         TM    0(R8),X'40'              A COMMAND?
         BZ    SETJOB3                  NO - SET JOB FLAG
SETCMD3  OI    8(R9),X'40'              SET FLAG IN RECOVER AREA
         B     NEXTJOB3                 GO CHECK NEXT ENTRY
SETJOB3  OI    8(R9),X'80'              SET FLAG IN RECOVER AREA
NEXTJOB3 LA    R8,4(,R8)                POINT TO NEXT ENTRY
         B     JOBLOOP3                 CHECK IT OUT
CHKFLAG3 EQU   *
         TM    8(R9),X'C0'              BOTH FLAGS SET ALREADY?
         BO    NEXTJOB3                 YES - CAN'T DO ANY MORE
         TM    0(R8),X'40'              A COMMAND?
         BO    SETCMD3                  YES - GO SET COMMAND FLAG
         B     SETJOB3                  NO - GO SET JOB FLAG
RECOVEND EQU   *
         OPEN  JOBFILE1                 OPEN JOB PDS
         TM    JOBFILE1+48,X'10'        OPEN OK?
         BZ    OPENERR2                 NO - ISSUE ERROR MESSAGE
         OPEN  CMDFILE1                 OPEN COMMAND PDS
         TM    CMDFILE1+48,X'10'        OPEN OK?
         BZ    OPENERR2                 NO - ISSUE ERROR MESSAGE
         L     R9,RECOVTBL              GET MEMBER TBL ADDRESS
SUBLOOP  CLC   0(8,R9),=8X'00'          END OF TABLE?
         BE    CLOSEIT                  YES - CLOSE DATASET
         MVC   NOMEM02+23(8),0(R9)      MOVE IN MEMBER NAME
         MVC   NOMEM02+19(3),=C'JOB'    MOVE IN JOB IDENTIFIER
         MVC   LOGIT1+39(8),0(R9)      MID: ADDED FOR SCHED033
         MVC   LOGIT1+28(3),=C'JOB'    MID: FOR SCHED033 MOVE IN 'CMD'
         TM    8(R9),X'40'              COMMAND FLAG SET?
         BZ    NOTCMD01                 NO - CHECK IN JOBFILE
         MVC   LOGIT1+28(3),=C'CMD'    MID: FOR SCHED033 MOVE IN 'CMD'
         FIND  CMDFILE1,(R9),D          FIND THE MEMBER
         LTR   R15,R15                  FOUND IT?
         BZ    GOSUB                    YES - GO SUBMIT IT
         MVC   NOMEM02+19(3),=C'CMD'    MOVE IN COMMAND IDENTIFIER
         B     NOMEM02                  WRITE MESSAGE
NOTCMD01 FIND  JOBFILE1,(R9),D          FIND THE MEMBER
         LTR   R15,R15                  FOUND IT?
         BZ    GOSUB                    YES - GO SUBMIT IT
NOMEM02  WTO   'SCHED014 - JOB XXXXXXXX NOT FOUND, NOT SUBMITTED',     X
               ROUTCDE=(1),DESC=(1)
         B     NEXTMEM                  GET NEXT MEMBER
GOSUB EQU *
         OPEN  (INTRDR,OUTPUT)          OPEN INTERNAL READER
*
         TM    FLAG,HARDDOWN           HARD TERMINATION?
* MID    BZ    READIT                  NO - DO RECOVERY
         BZ    READPRE                 MID: TO WTO MESSAGE FIRST
         MVC   DOUBLE(8),=C'00010000'  SET 1 MINUTE LIMIT FOR REPLY
         L     R6,=A(REPLYEXT)         GET EXIT ADDRESS
         STIMER REAL,(R6),DINTVL=DOUBLE MID: CHANGED
         MVC   SUBWTOR+104(8),0(R9)    MOVE IN THE JOBNAME
         MVC   SUBWTOR+100(3),=C'JOB'  MOVE IN 'JOB'
         TM    8(R9),X'40'             COMMAND FLAG SET?
         BZ    GETRIGHT                NO - DON'T MOVE IN CMD
         MVC   SUBWTOR+100(3),=C'CMD'  MOVE IN 'CMD'
GETRIGHT XC    ECBAREA(4),ECBAREA      CLEAR THE ECB
         MVI   WTOREPLY,C'Y'           SET DEFAULT REPLY
SUBWTOR  WTOR  'SCHED015 - SCHEDULER CHECKPOINT SHOWS HARD TERMINATION.X
               SHOULD RECOVERY CONTINUE FOR JOB XXXXXXXX (Y/N)?',      X
               WTOREPLY,1,ECBAREA,ROUTCDE=(1)
         ST    R1,SUBWTOID             MID: SAVE ID OF WTOR
         WAIT  ECB=ECBAREA             WAIT FOR THE REPLY
* MID: SHOULD
*    CHECK TO SEE IF THE TIMER POPPED BY TESTING THE ECBAREA
*    CHECK IF WE GOT A WTOR REPLY BY TESTING THE ECBAREA
*  HOWEVER AS THIS IS JUST MAKING IT WORK UNDER MVS3.8J
*    ALWAYS CANCEL TIMER AND FORCE THE WTOR INTO A REPLIED
*    STATE... OTHRWISE TIMER COULD POP AND WTOR REMAIN
*    OUTSTANDING... ACTUALLY SUPPLIED SCHEDULE CODE LEFT
*    THE WTOR OUTSTANDING WHEN THE TIMER POPPED SO FIXED :-).
         TTIMER CANCEL                 MID: ADDED NOW STIMER USED
         L     R1,SUBWTOID             MID: SHOULD...
         DOM   MSG=(R1),REPLY=YES      MID: FORCE THE WTOR REPLIED
         OI    WTOREPLY,X'40'          SET TO UPPER CASE
         CLI   WTOREPLY,C'Y'           RECOVER?
*        BE    READIT                  YES - DO RECOVERY
         BE    READPRE                 MID: TO LOGMSG    
         CLI   WTOREPLY,C'N'           DON'T RECOVER?
         BE    NEXTMEM                 YES - GET NEXT ONE
         B     GETRIGHT                GET PROPER REPLY
*
* MID: ADDED WTO MESSAGE TO SHOW SCHEDULE IS CAUSING THIS ACTIVITY
*      I ADDED THIS AHEAD OF THE READIT BLOCK AS READIT IS CALLED
*      MULTIPLE TIMES DEPENDING ON THE SIZE OF THE MEMBER BEING
*      PROCESSED So LEAVING IT IN THE  READIT LOOP JUST GENERATED
*      CONFUSING WTOS
READPRE  EQU   *
LOGIT1   WTO   'SCHED033 - INVOKING JOB MEMBER XXXXXXXX'
READIT   EQU   *
         LA    R0,INPUT                GET INPUT AREA ADDRESS
         L     R1,=F'32720'            GET THE LENGTH
         XR    R14,R14                 DUMMY ADDRESS
         XR    R15,R15                 MODEL BYTE
         MVCL  R0,R14                  CLEAR THE AREA
         LA    R7,INPUT                GET AREA ADDRESS
         TM    8(R9),X'40'             A COMMAND?
         BO    READCMD                 YES - GO READ THE COMMAND
         READ  DECBIN,SF,JOBFILE1,INPUT,'S'    READ A BLOCK
         CHECK DECBIN                  WAIT FOR IT
PUTLOOP1 PUT   INTRDR,(R7)             WRITE A RECORD
         LA    R7,80(,R7)              POINT TO NEXT RECORD
         CLC   0(4,R7),=F'0'           END OF BLOCK?
         BNE   PUTLOOP1                NO - WRITE NEXT RECORD
         B     READIT                  YES - GO FIND MORE
READCMD  READ  DECBIN01,SF,CMDFILE1,INPUT,'S'     READ A BLOCK
         CHECK DECBIN01                WAIT FOR IT
         MODESET KEY=ZERO,MODE=SUP
CMDLOOP1 MVC   CMD+4(72),0(R7)         MOVE IN COMMAND
         CLI   0(R7),C'*'              A COMMENT?
         BE    NEXTCMD1                YES - DON'T ISSUE A COMMAND
* MID: LOG COMMANDS BEING ISSUED IN MESSAGE SCHED031 (32 IN SCHED02)
         MVC   LOGCMD01+19(40),CMD+4
*                          ....+....1....+....2....+....3....+....4
LOGCMD01 WTO   'SCHED031 -                                         '
         XR    R0,R0                   CLEAR R0
         LA    R1,CMD                  GET COMMAND ADDRESS
         SVC   34                      ISSUE THE COMMAND
NEXTCMD1 LA    R7,80(,R7)              POINT TO NEXT RECORD
         CLC   0(4,R7),=F'0'           END OF BLOCK?
         BNE   CMDLOOP1                NO - ISSUE NEXT COMMAND
         MODESET KEY=NZERO,MODE=PROB
         B     READIT                  GO READ NEXT BLOCK
NEXTMEM  CLOSE INTRDR                  CLOSE INTERNAL READER
         TTIMER CANCEL                 CANCEL STIMER EXIT
         TM    8(R9),X'C0'             JOB AND CMD MEM NAME THE SAME?
         BO    CMDRESET                RESET THE COMMAND FLAG
         LA    R9,16(,R9)              POINT TO NEXT MEMBER NAME
         B     SUBLOOP                 SEE IF THERE IS MORE
CMDRESET NI    8(R9),255-X'40'         RESET THE FLAG
         B     SUBLOOP                 GO BACK UP AND DO THE JOB
CLOSEIT  CLOSE JOBFILE1                CLOSE PDS
         CLOSE CMDFILE1                CLOSE PDS
         NI    FLAG,255-ACTIVE         TURN RECOVER FLAG OFF
*
SETTIMER TM    FLAG,TIMERON            TIMER SET ALREADY?
         BO    ADDTASK                 YES - GO ADD TASK
         L     R15,CHKPTINT            GET CHKPOINT INT-MINUTES
         MH    R15,=H'6000'            CONVERT TO 1/100 SECONDS
         ST    R15,TIMEINT             SAVE TIME INTERVAL
*         STIMERM SET,ID=TIMERID,BINTVL=TIMEINT,EXIT=CHKPTEXT
**        STIMER REAL,CHKPTEXT,BINTVL=TIMEINT   MID: ADDRESSING ERR
         L     R6,=A(CHKPTEXT)
         STIMER REAL,(R6),BINTVL=TIMEINT
         OI    FLAG,TIMERON            SET THE FLAG
ADDTASK  EQU   *
         XC    TASKECB(4),TASKECB      CLEAR THE ECB
         ATTACH EP=SCHEDM02,ECB=TASKECB,SVAREA=YES,PARAM=(FLAG)
         LTR   R15,R15                 ATTACH TIMER TASK OK?
         BNZ   RETURN                  NO - GO HOME
         ST    R1,TCBADDR              SAVE TCB ADDRESS
* SET UP ESTAE
         ESTAE SCHEDERR,CT,XCTL=NO,PARAM=ERRPARM,PURGE=NONE,ASYNCH=YES,X
               TERM=YES
* ESTABLISH THE CONSOLE COMMUNICATION ENVIRONMENT.
         LA    R5,ANSRAREA             ADDR OF RESPONSE AREA
         EXTRACT (5),FIELDS=COMM       ADDR OF COMMUNICAT'N AREA
         L     R5,ANSRAREA             LOAD IT
         USING COMLIST,R5
         L     R3,COMCIBPT             GET ADDRESS OF CIB
         USING CIBNEXT,R3
         C     R3,=F'0'                CIB EXISTS?
         BE    SETCOUNT                NO - GO SET COUNT
         QEDIT ORIGIN=COMCIBPT,BLOCK=(3)    YES - FREE IT
         LTR   R15,R15 GO OK?
         BZ    SETCOUNT YES - GO SET COUNT
REPEAT   XC    ECBAREA(4),ECBAREA      CLEAR ECB
         LA    R8,ECBAREA              GET ECB ADDRESS
         MVI   WTOREPLY,C' '           PRIME REPLY AREA
         LA    R9,WTOREPLY             GET REPLY AREA ADDRESS
         WTOR  'SCHED020 - ERROR CONDITION RECOGNIZED IN CONSOLE INTERFX
               ACE. PROGRAM IS TERMINATING.  SHOULD SCHEDULING REMAIN  X
               ACTIVE (Y/N)?',                                         X
               (R9),1,(R8),ROUTCDE=(1)
         WAIT  ECB=ECBAREA             WAIT FOR REPLY
         OI    0(R9),X'40'             SET TO UPPER CASE
         CLI   0(R9),C'Y'              LEAVE SCHEDULING ON?
         BE    WAIT                    YES - JUST GO WAIT
         CLI   0(R9),C'N'              TURN SCHEDULING OFF?
         BE    CSTOP                   YES - GO STOP EVERYTHING
         B     REPEAT                  RE-ISSUE MESSAGE
SETCOUNT EQU *
* SET LIMIT ON MODIFY COMMANDS
         QEDIT ORIGIN=COMCIBPT,CIBCTR=1   ONE MODIFY AT A TIME
         WTO   'SCHED001 - SCHEDULE CONSOLE INTERFACE ENABLED. WAITING X
               FOR FURTHER REQUESTS.',ROUTCDE=(1),DESC=(6)
* WAIT FOR ANY OPERATOR REQUESTS.
WAIT L R8,COMECBPT ADDR OF COMMUNICATION ECB
         ST    R8,ECBLIST              SAVE ECB ADDR IN LIST
         LA    R1,TASKECB              GET SUBTASK ECB ADDRESS
         ST    R1,ECBLIST+4            SAVE ECB ADDR IN LIST
         OI    ECBLIST+4,X'80'         SET LAST ECB FLAG
         WAIT  1,ECBLIST=ECBLIST       WAIT FOR AN EVENT
         L     R8,ECBLIST              GET FIRST ECB
         TM    0(R8),X'40'             OPER CMD EVENT COMPLETE?
         BZ    TASKERR                 NO - SUBTASK FAILED
         BAL   R6,CMDPROC              PROCESS COMMAND
         B     WAIT                    GO WAIT
* OUR TASK HAS RECEIVED A REQUEST FROM THE OPERATOR. DETERMINE
* THE TYPE OF REQUEST AND ACT ACCORDINGLY.
CMDPROC  EQU   *
         L     R3,COMCIBPT             GET ADDRESS OF CIB
         LTR   R3,R3                   VALID POINTER?
         BZ    RETR6                   NO - RETURN
         CLI   CIBVERB,CIBMODFY        IS IT A MODIFY COMMAND?
         BE    CMODIFY                 YES - GO PROCESS
         CLI   CIBVERB,CIBSTOP         IS IT A STOP COMMAND?
         BE    CSTOP                   YES - GO PROCESS
RETR6 EQU *
         QEDIT ORIGIN=COMCIBPT,BLOCK=(3)    FREE CIB
         BR    R6                      RETURN
CMODIFY EQU *
* THIS IS A MODIFY COMMAND SO WE MUST CHECK FOR VALID SYNTAX.
         LH    R7,CIBDATLN             GET COMMAND LENGTH
         CLC   CIBDATA(4),=C'STOP'     A STOP COMMAND?
         BE    CSTOP                   YES - PROCESS LIKE STOP CMD
         B     RETR6                   GO BACK
CSTOP    EQU   *
         OI    FLAG,HALTPGM            SET HALT PROGRAM FLAG
         POST  WAITECB
TASKWAIT EQU   *
         WAIT  1,ECB=TASKECB           WAIT FOR SUBTASK TO FINISH
         DETACH TCBADDR                REMOVE THE SUBTASK
*         STIMERM CANCEL,ID=TIMERID     CANCEL THE TIMER EXIT
         TTIMER CANCEL
         ESTAE 0                       CANCEL ESTAE
         OPEN  (CHKPOINT,OUTPUT)       OPEN CHECKPOINT DATASET
         TIME  DEC                     GET THE DATE AND TIME
         STM   R0,R1,CHKPTREC+8        SAVE DATE AND TIME
         PUT   CHKPOINT,CHKPTREC       WRITE OUT LAST CKPT RECORD
         CLOSE CHKPOINT                CLOSE THE DATASET
*
RETURN   L     R13,SAVE+4
         LM    R14,R12,12(R13)
         XR    R15,R15 CLEAR RETURN CODE
         BR    R14
TASKERR  EQU *
         WTO   'SCHED030 - AN ERROR OCCURRED IN THE SCHEDULE SUBTASK.  X
               OPERATIONS ARE TERMINATING.',ROUTCDE=(1),DESC=(1)
         B     RETURN
EXPIRED  WTO   'SCHED016 - CURRENT YEAR IS NOT SUPPORTED - PROGRAM UPDAX
               TE REQUIRED',ROUTCDE=(1),DESC=(1)
         B     RETURN
GETERR   WTO   'SCHED017 - INSUFFICIENT REGION TO FACILITATE SPECIFIED X
               JOBMAX VALUE',ROUTCDE=(1),DESC=(1)
         B     RETURN
OPENERR1 WTO   'SCHED021 - SYSIN DATASET FAILED TO OPEN, CORRECT PROBLEX
               M AND RESTART',ROUTCDE=(1),DESC=(1)
         B     RETURN
OPENERR2 WTO   'SCHED022 - JOBFILE DATASET FAILED TO OPEN, CORRECT PROBX
               LEM AND RESTART',ROUTCDE=(1),DESC=(1)
         B     RETURN
OPENERR3 WTO   'SCHED023 - CHKPOINT DATASET FAILED TO OPEN, CORRECT PROX
               BLEM AND RESTART',ROUTCDE=(1),DESC=(1)
         B     RETURN
OPENERR4 WTO   'SCHED024 - CMDFILE DATASET FAILED TO OPEN, CORRECT PROBX
               LEM AND RESTART',ROUTCDE=(1),DESC=(1)
         B     RETURN
         DS    0D
FLAG     DC    X'00'
DAILY    EQU   X'80'
CON      EQU   X'40'
WORK     DS    CL5
CURRDAY  DS    X
JULDAY   DS    CL2
RELVALUE DS    CL3
CONVALH  DS    CL3
CONVALM  DS    CL2
DOUBLE   DS    D
DATE     DS    F
TIME     DS    F
**********************
SUN      EQU   0
MON      EQU   1
TUES     EQU   2
WED      EQU   3
THUR     EQU   4
FRI      EQU   5
SAT      EQU   6
**********************
YEARTAB  DC    X'13',AL1(TUES) 2013
         DC    X'14',AL1(WED)  2014
         DC    X'15',AL1(THUR) 2015
         DC    X'16',AL1(FRI)  2016
         DC    X'17',AL1(SUN)  2017
         DC    X'18',AL1(MON)  2018
         DC    X'19',AL1(TUES) 2019
         DC    X'20',AL1(WED)  2020
         DC    X'21',AL1(FRI)  2021
         DC    X'22',AL1(SAT)  2022
         DC    X'23',AL1(SUN)  2023
         DC    X'24',AL1(MON)  2024
         DC    X'25',AL1(WED)  2025
         DC    X'FF'
*
DAYTABLE DC    AL1(SUN),AL1(MON),AL1(TUES),AL1(WED),AL1(THUR),AL1(FRI)
         DC    AL1(SAT),AL1(SUN),AL1(MON),AL1(TUES),AL1(WED),AL1(THUR)
         DC    AL1(FRI),AL1(SAT)
*
NXTDPTRS DC    A(0),A(0),A(0),A(0),A(0),A(0),A(0)
DAYPTRS  DC    A(0),A(0),A(0),A(0),A(0),A(0),A(0)
*
NXTJPTRS DC    A(0)
JOBPTRS  DC    A(0)
DAYLEN   DS    F
WAITECB  DS    F
DATALEN  EQU   *-FLAG
SAVE     DS    18F
STORLIM  DS    F
SYSIN    DCB   DSORG=PS,MACRF=GL,DDNAME=SYSIN,EODAD=EOD1,LRECL=80
INTRDR   DCB   DSORG=PS,MACRF=PM,DDNAME=INTRDR,LRECL=80
TIMERID  DS    F
TIMEINT  DS    F
CHKPTINT DS    F
RECOVTBL DS    F
CURRTIME DS    F
CURRDATE DS    F
CHKPTREC DC    80C' '
CHKPOINT DCB   MACRF=(GM,PM),DSORG=PS,LRECL=80,DDNAME=CHKPOINT
JOBFILE1 DCB   DSORG=PO,DDNAME=JOBFILE,EODAD=NEXTMEM,MACRF=R
CMDFILE1 DCB   DSORG=PO,DDNAME=CMDFILE,EODAD=NEXTMEM,MACRF=R
ACTIVE   EQU   X'08'
TIMERON  EQU   X'04'
HALTPGM  EQU   X'02'
HARDDOWN EQU   X'01'
TASKECB  DS    F
TCBADDR  DS    F
ECBLIST  DS    2F
ECBAREA  DS    F
WTOREPLY DS    CL1
ANSRAREA DS    F
DAYEND   DS    F
ERRPARM  DS    18F
TEMPDAT1 DS    CL8
TEMPDAT2 DS    CL8
CMD      DC    X'004C0000',80C' '
SUBWTOID DS    F
*
TRTABLE  DC    256X'80'
         ORG   TRTABLE+0
         DC    C'0123456789ABCDEF'
         ORG   TRTABLE+193
         DC    X'0A0B0C0D0E0F'
         ORG   TRTABLE+240
         DC    X'00010203040506070809'
         ORG   ,
*
         LTORG
*
INPUT    DS    CL32720
         DC    F'0'
*
PARMIN   DSECT
COMMENT  DS    0C
DAY      DS    CL9
         DS    C
TIMEID   DS    CL2
RELTIME  DS    CL4
         DS    C
JOBID    DS    CL4
JOB      DS    CL8
         DS    C
CONTID   DS    CL9
CONTIME  DS    CL4
         DS    C
RECOVID  DS    CL7
         DCBD  DSORG=PS
         CVT   DSECT=YES
         IHAPSA
* IEFJESCT
         DSECT
         IEZCOM
         DSECT
         IEZCIB
CHKPTEXT CSECT
         STM   R14,R12,12(R13)          SAVE ENVIRONMENT
         LR    R2,R15                   SET BASE REGISTER
         USING CHKPTEXT,R2
         ST    R13,EXITSAVE+4           SAVE OLD SAVEAREA ADDRESS
         LA    R13,EXITSAVE             GET NEW SAVEAREA ADDRESS
         L     R3,=A(CHKPOINT)          GET DCB ADDRESS
         OPEN  ((R3),OUTPUT)            OPEN THE DATASET
         TIME  DEC                      GET THE TIME AND DATE
         L     R7,=A(CHKPTREC)          GET RECORD ADDRESS
         STM   R0,R1,0(R7)              SAVE IN OUTPUT AREA
         STM   R0,R1,8(R7)              SAVE IN OUTPUT AREA
         PUT   (R3),(R7)                WRITE THE RECORD
         CLOSE ((R3))                   CLOSE CHECKPOINT DATASET
         L     R4,=A(CHKPTINT)          GET ADDRESS OF INTERVAL
         L     R5,0(,R4)                LOAD INTERVAL VALUE
         MH    R5,=H'6000'              SET TO HUNDRETH SECONDS
         ST    R5,EXITINT               SAVE TIME INTERVAL
*         STIMERM SET,ID=EXITID,BINTVL=EXITINT,EXIT=CHKPTEXT
         STIMER REAL,CHKPTEXT,BINTVL=TIMEINT
         L     R13,EXITSAVE+4           GET OLD SAVEAREA ADDRESS
         LM    R14,R12,12(R13)          RESTORE THE ENVIRONMENT
         XR    R15,R15                  CLEAR R15
         BR    R14                      RETURN
EXITID   DS    F
EXITINT  DS    F
EXITSAVE DS    18F
         LTORG
REPLYEXT CSECT
         STM   R14,R12,12(R13)          SAVE ENVIRONMENT
         LR    R11,R15                  SET UP ...
         USING REPLYEXT,R11             EXIT ADDRESSABILITY
         ST    R13,EXITSV02+4
         LA    R13,EXITSV02
         L     R3,=A(ECBAREA)           GET ECB ADDRESS
         POST  (R3)
         L     R13,EXITSV02+4
         LM    R14,R12,12(R13)
         XR    R15,R15
         BR    R14
EXITSV02 DS    18F
         LTORG
SCHEDERR CSECT
* PRINT   NOGEN
         USING SCHEDERR,R15
         C     R0,=F'12'                SDWA PRESENT?
         BE    NOSDWA1                  NO - PROCESS AS SUCH
         STM   R14,R12,12(R13)          SAVE ENVIRONMENT
         B     SETUP CONTINUE
NOSDWA1  EQU   *
         STM   R14,R12,12(R2)           R2 POINTS TO SAVE AREA PARM
         LR    R13,R2                   POINT TO SAVE AREA
SETUP    EQU   *
         DROP  R15
         LR    R11,R15                  SET UP ...
         USING SCHEDERR,R11             NEW ADDRESSABILITY
         LR    R3,R0                    SAVE SDWA FLAG
         LR    R4,R1                    SAVE SDWA ADDRESS
         ST    R13,ERRSAVE+4            SAVE OLD SAVE AREA ADDRESS
         LA    R13,ERRSAVE              GET NEW SAVE AREA ADDRESS
         SR    R12,R12                  CLEAR R12
         C     R3,=F'12'                CHECK FOR SDWA
         BE    NOSDWA2                  NO - BYPASS SDWA PROCESSING
         LR    R12,R4                   SET UP ADDRESSABILITY ...
         USING SDWA,R12                 TO THE SDWA
         L     R2,SDWAPARM              GET PARAMETER AREA ADDRESS
NOSDWA2  EQU   *
         LTR   R12,R12                  SDWA?
         BZ    NOSDWA3                  NO - BYPASS SDWA
         UNPK  ERRDBL1(5),SDWAABCC+1(3) UNPACK ABEND CODE
         B     ERROR1
NOSDWA3  EQU   *
         ST    R4,ERRDBL2               SAVE ABEND CODE
         UNPK  ERRDBL1(5),ERRDBL2+1(3)  UNPACK ABEND CODE
ERROR1   EQU   *
         NC    ERRDBL1(6),=6X'0F'              MAKE ABEND ...
         TR    ERRDBL1(6),=C'0123456789ABCDEF' CODE READABLE
         CLC   ERRDBL1(3),=C'222'       OPERATOR CANCEL?
         BNE   RETDUMP                  NO - RETURN AND DUMP
         L     R6,=A(CHKPOINT)          GET DCB ADDRESS
         OPEN  ((R6),OUTPUT)            OPEN THE DATASET
         TIME  DEC                      GET THE TIME AND DATE
         L     R7,=A(CHKPTREC)          GET RECORD ADDRESS
         STM   R0,R1,8(R7)              SAVE IN OUTPUT AREA
         PUT   (R6),(R7)                WRITE THE RECORD
         CLOSE ((R6))                   CLOSE CHECKPOINT DATASET
         L     R5,16                    GET CVT ADDRESS
         L     R5,0(,R5)
         L     R6,12(,R5)               CURRENT ASCB ADDRESS
         L     R7,172(,R6)              GET JOBNAME AREA ADDRESS
         LTR   R7,R7                    VALID?
         BNZ   GETJOBN                  YES - EXTRACT JOBNAME
         L     R7,176(,R6)              GET SCTNAME AREA ADDRESS
         LTR   R7,R7                    VALID?
         BZ    NONAME                   NO - DON'T FILL IN
GETJOBN  MVC   ERRWTO+19(8),0(R7)       MOVE IN JOBNAME
         MVC   ERRWTO+70(8),0(R7)       MOVE IN JOBNAME
         MVC   ERRWTO+97(8),0(R7)       MOVE IN JOBNAME
         MODESET KEY=ZERO,MODE=SUP
ERRWTO   WTO   'SCHED009 - SCHEDULE ACCEPTS THE ''STOP'' COMMAND. PLEASX
               E USE ''P SCHEDULE'' WHEN TERMINATING SCHEDULE.',       X
               ROUTCDE=(1),DESC=(1)
         MODESET KEY=NZERO,MODE=PROB
NONAME   L     R13,ERRSAVE+4            GET OLD SAVE AREA ADDRESS
         LTR   R12,R12                  SDWA?
         BZ    END                      NO - END
         SETRP WKAREA=(R4),REGS=(14),DUMP=NO,RC=0
RETDUMP  EQU   *
         L     R13,ERRSAVE+4            GET OLD SAVE AREA ADDRESS
         LTR   R12,R12                  SDWA?
         BZ    END                      NO - END
         SETRP WKAREA=(R4),REGS=(14),DUMP=YES,RC=0
END      EQU   *
         LM    R0,R12,20(R13)           RESTORE ENVIRONMENT
         XR    R15,R15                  CLEAR R15
         LA    R15,4                    SET RETRY
         L     R14,12(,R13)             POINT TO RTM
         BR    R14                      RETURN
ERRSAVE  DS    18F
ERRDBL1  DS    D
         DS    F
ERRDBL2  DS    D
         DS    F
         LTORG
         PRINT NOGEN
         IHASDWA
         END
ZZ
//SYSPUNCH DD  SYSOUT=8
//SYSGO    DD  DISP=(MOD,PASS,DELETE),UNIT=SYSDA,
//    DSN=&&OBJLIB,SPACE=(CYL,(2,2))
//LKED1A  EXEC PGM=IEWL,
//             PARM='XREF,LIST,LET,TEST,AC=1',
//             REGION=1024K,COND=(0,NE)
//SYSLMOD  DD  DSN=SYS2.LINKLIB(SCHEDM01),DISP=SHR
//SYSLIN   DD  DSN=&&OBJLIB,DISP=(OLD,PASS,DELETE)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(8,1))
//SYSPRINT DD  SYSOUT=*
//LKED1B  EXEC PGM=IEWL,
//             PARM='XREF,LIST,LET,TEST,AC=1',
//             REGION=1024K,COND=(0,NE)
//SYSLMOD  DD  DSN=SYS2.LINKLIB,DISP=SHR
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(8,1))
//SYSPRINT DD  SYSOUT=*
//SYSLIN   DD  *
 INCLUDE SYSLMOD(SCHEDM01)
 ENTRY SCHEDM01
 NAME SCHEDM01(R)
/*
//
./ ADD NAME=SCHEDM02
//AS2SCHED JOB (TSO),
//             'ASSEMBLE SCHEDULE',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(0,0),
//             REGION=0K,
//             USER=IBMUSER,
//             PASSWORD=SYS1
//ASM2    EXEC PGM=IFOX00,
//   PARM='DECK,LOAD,TERM,TEST,SYSPARM((NOSP,NODEBUG)),XREF(SHORT)',
//   REGION=4096K,COND=(0,NE)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=SYS2.MACLIB
//SYSUT1   DD  SPACE=(CYL,(25,5)),UNIT=SYSDA
//SYSUT2   DD  SPACE=(CYL,(25,5)),UNIT=SYSDA
//SYSUT3   DD  SPACE=(CYL,(25,5)),UNIT=SYSDA
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DATA,DLM=ZZ
***********************************************************************
* SOURCE: XEPHON MVS UPDATE - MVS9902.PDF
***********************************************************************
* ### SUMMARY OF CHANGES
* CHANGE IDENTIFIER CHANGE DESCRIPTION
* C90197 ADDED FREE TO LAST CLOSE OF SYSIN
* C90201 ADDED SUPPORT FOR CHECKPOINTING DATASET
* AND RECOVER KEYWORD FOR CATCHUP PROCESSING
*
***********************************************************************
* *
* THE SCHEDM02 TASK OPERATES AS THE SCHEDULE JOB/CMD ISSUER. IT *
* INTERPRETS THE TABLE OF INFORMATION BUILT BY SCHEDM01 TO *
* DETERMINE WHEN THE NEXT JOB/CMD IS TO BE SUBMITTED AND IT MANAGES *
* THE STIMER EXIT USED TO SCHEDULE THE NEXT AVAILABLE JOB/CMD. *
* *
* SCHEDM02 NEEDS TO RESIDE IN AN APF AUTHORIZED LIBRARY AND BE *
* LINKED AC(1) *
* *
***********************************************************************
         PRINT ON,GEN
         SPACE
SCHEDM02 CSECT
         YREGS
         USING SCHEDM02,R12,R11
         STM   R14,R12,12(R13)
         LR    R12,R15                 SET INITIAL BASE
         LA    R11,4095(,R12)
         LA    R11,1(,R11)
         L     R9,0(,R1)               GET INCOMING PARMS
         USING DATAAREA,R9             SET ADDRESSABILITY
         ST    R13,SAVE+4
         LA    R13,SAVE
         LA    R1,WAITECB              GET ECB ADDRESS
         ST    R1,ECBADDR              SAVE IT
******************************************************************
* DAY OF WEEK CALC
GETDAY   TIME  DEC                     GET TIME AND DATE
         ST    R1,DATE                 STORE DATE
         ST    R0,TIME                 STORE TIME
* INDEX YEAR TABLE
GETDAY2  LH    R2,DATE                 LOAD YEAR
         LA    R3,YEARTAB              ADDRESS YEAR TABLE
YEARSRCH CLM   R2,B'0001',0(R3)        YEAR FOUND?
         BE    YEARFND                 YES - GO PROCESS
         CLI   0(R3),X'FF'             NO - END OF TABLE?
         BE    EXPIRED                 YES - THE PROGRAM TABLE EXPIRED
         LA    R3,2(,R3)               NO - ADDRESS NEXT ENTRY
         B     YEARSRCH
YEARFND  SR    R2,R2                   CLEAR REGISTER
         IC    R2,1(,R3)               GET STARTING DAY OF YEAR
         XC    DOUBLE,DOUBLE           CLEAR DOUBLEWORD
         MVC   DOUBLE+6(2),DATE+2      MOVE IN DAY
         CVB   R1,DOUBLE               CONVERT DAY TO BINARY
         SR    R0,R0                   CLEAR EVEN REGISTER
         D     R0,=F'7'                DIVIDE BY 7 (DAYS IN A WEEK)
         LR    R1,R0                   MOVE REMAINDER
         S     R1,=F'1'                REMAINDER MINUS ONE
         AR    R1,R2                   PLUS STARTING DAY OF YEAR
         C     R1,=F'-1'               IS IT NEGATIVE?
         BNE   GETDAY3                 NO - JUST GO ON
         L     R1,=F'6'                SET TO SATURDAY
GETDAY3  IC    R1,DAYTABLE(R1)         GET CURRENT DAY OF THE WEEK
         STC   R1,CURRDAY              AND STORE IT
         MVC   JULDAY,DATE+2           SAVE JULIAN DAY
******************************************************************
* FIND FIRST JOB TO BE SUBMITTED
         SR    R3,R3                   CLEAR REGISTER
         IC    R3,CURRDAY              GET CURRENT DAY
         MH    R3,=H'4'                GET DAY POINTER OFFSET
         L     R2,DAYPTRS(R3)          ADDRESS TODAYS WORK
         L     R14,DAYLEN              GET MAX DAY STORAGE
         LA    R14,0(R14,R2)           SET TO END OF DAY
         ST    R14,DAYEND              SAVE IT
         L     R15,DAYTBL(R3)
         MVC   STATWTO+19(9),0(R15)
         ST    R2,DBL2
         UNPK  DBL1(9),DBL2(5)
         NC    DBL1(8),=8X'0F'
         TR    DBL1(8),=C'0123456789ABCDEF'
         MVC   STATWTO+43(8),DBL1
         ST    R14,DBL2
         UNPK  DBL1(9),DBL2(5)
         NC    DBL1(8),=8X'0F'
         TR    DBL1(8),=C'0123456789ABCDEF'
         MVC   STATWTO+63(8),DBL1
STATWTO  WTO   'SCHED002 - XXXXXXXXX:  DAY START - XXXXXXXX  DAY END - X
               XXXXXXXX',ROUTCDE=(1),DESC=(6)
JOBSRCH  EQU   *
         L     R3,0(,R2)               ADDRESS JOB
         LTR   R3,R3                   ANY WORK FOR TODAY?
         BZ    DAYSLEEP                NO - SLEEP FOR THE DAY
         CLC   0(2,R3),TIME            JOB FOR THE FUTURE?
         BE    SUBMIT                  NO - SUBMIT IT RIGHT NOW
         BH    WAITTIME                YES - GO SET UP
         LA    R2,4(,R2)               NO - ADDRESS NEXT JOB
         B     JOBSRCH                 AND GO CHECK IT OUT
******************************************************************
* R2 POINTS TO JOB ADDRESS
* R3 POINTS TO JOB
WAITTIME UNPK  DOUBLE(5),0(3,R3)       UNPACK JOB SUBMIT TIME
         MVC   DOUBLE+4(4),=C'0000'    FILL IN REMAINDER
         L     R8,=A(TIMEEXIT)         GET TIMER EXIT ADDRESS
         STIMER REAL,(R8),TOD=DOUBLE   WAIT UNTIL SUBMIT TIME
         XC    WAITECB(4),WAITECB      CLEAR THE ECB
         LA    R8,WAITECB              GET ECB ADDRESS
         WAIT  1,ECB=(R8)              WAIT FOR THE TIME TO EXPIRE
         TM    FLAG,HALTPGM            PROGRAM HAS BEEN HALTED?
         BZ    NORET01                 NO - DON'T GO BACK
         TTIMER CANCEL                 CANCEL THE TIMER EXIT
         L     R15,=F'4'               SET RETURN CODE 4
         B     NONZERO                 RETURN NONZERO
NORET01  EQU   *
******************************************************************
SUBMIT   EQU *
         TM    0(R2),X'40'             A COMMAND?
         BO    CMD01                   YES - DO COMMAND STUFF
         OPEN  (JOBFILE)               OPEN THE PDS
         LA    R10,JOBFILE             ADDRESS DCB
         USING IHADCB,R10
         TM    DCBOFLGS,X'10'          OPEN OK?
         BZ    OPENERR2                NO - INDCIATE ERROR
         DROP  R10
         B     FIND01                  GO FIND MEMBER
*
CMD01    EQU *
         OPEN  (CMDFILE)               OPEN THE PDS
         TM    CMDFILE+48,X'10'        OPEN OK?
         BZ    OPENERR3                NO - ISSUE ERROR
         B     FIND02
FIND01   LA    R4,2(,R3)               ADDRESS THE MEMBER NAME
         MVC   LOGIT2+28(3),=C'JOB'    MID: FOR SCHED034 MOVE IN 'JOB'
         MVC   LOGIT2+39(8),0(R4)      MID: ADDED FOR SCHED034
         FIND  JOBFILE,(R4),D          LOCATE THE MEMBER
         LTR   R15,R15                 FIND GO OK?
         BNE   FINDERR                 NO, ERROR
         B     GETRDR                  YES - CONTINUE
FINDERR  MVC   NOMEM+23(8),2(R3)       NO - SET UP AND ISSUE MESSAGE
         MVC   NOMEM+19(3),=C'JOB'     MOVE IN IDENTIFIER
NOMEM    WTO   'SCHED014 - JOB XXXXXXXX NOT FOUND, NOT SUBMITTED',     X
               ROUTCDE=(1),DESC=(1)
         B     SUBEND
FIND02   LA    R4,2(,R3)               ADDRESS THE MEMBER NAME
         MVC   LOGIT2+28(3),=C'CMD'    MID: FOR SCHED034 MOVE IN 'CMD'
         MVC   LOGIT2+39(8),0(R4)      MID: ADDED FOR SCHED034
         FIND  CMDFILE,(R4),D          LOCATE THE MEMBER
         LTR   R15,R15                 FIND GO OK?
         BE    GETRDR                  YES - CONTINUE
         MVC   NOMEM+19(3),=C'CMD'     MOVE IN IDENTIFIER
         B     FINDERR                 ISSUE ERROR
******************************************************************
GETRDR   EQU *
* MID: ADDED THE BELOW WTO TO LOG WHAT SCHEDULE IS DOING
LOGIT2   WTO   'SCHED034 - INVOKING JOB MEMBER XXXXXXXX'
         TM    0(R2),X'40'             A COMMAND?
         BZ    OPENRDR                 NO - OPEN INTERNAL READER
READMEM2 LA    R4,INPUT                ADDRESS STORAGE
         L     R5,=F'32720'            GET LENGTH
         SR    R6,R6                   DUMMY ADDRESS
         SR    R7,R7                   ZERO SECOND LENGTH
         MVCL  R4,R6                   CLEAR AREA
         READ  DECBIN01,SF,CMDFILE,INPUT,'S' READ A RECORD
         CHECK DECBIN01                WAIT FOR THE RECORD
         LA    R7,INPUT                ADDRESS INPUT
         MODESET KEY=ZERO,MODE=SUP
CMDLOOP  MVC   CMD+4(72),0(R7)         MOVE IN COMMAND
         CLI   0(R7),C'*'              A COMMENT?
         BE    NEXTCMD                 YES - DON'T ISSUE COMMAND
* MID: LOG COMMANDS BEING ISSUED IN MESSAGE SCHED032 (31 IN SCHED01)
         MVC   LOGCMD02+19(40),CMD+4
*                          ....+....1....+....2....+....3....+....4
LOGCMD02 WTO   'SCHED032 -                                         '
         XR    R0,R0                   CLEAR R0
         LA    R1,CMD                  GET COMMAND ADDRESS
         SVC   34                      ISSUE THE COMMAND
NEXTCMD  LA    R7,80(,R7)              ADDRESS NEXT RECORD
         CLC   0(4,R7),=F'0'           RECORD EXIST?
         BNE   CMDLOOP                 YES - GO ISSUE COMMAND
         MODESET KEY=NZERO,MODE=PROB
         B     READMEM2                NO - GO GET SOME MORE
PDSEOD02 CLOSE CMDFILE
         B     SUBEND
OPENRDR  OPEN  (INTRDR,OUTPUT)         OPEN INTRDR
READMEM  LA    R4,INPUT                ADDRESS STORAGE
         L     R5,=F'32720'            GET LENGTH
         SR    R6,R6                   DUMMY ADDRESS
         SR    R7,R7                   ZERO SECOND LENGTH
         MVCL  R4,R6                   CLEAR AREA
         READ  DECB,SF,JOBFILE,INPUT,'S' READ A RECORD
         CHECK DECB                    WAIT FOR THE RECORD
         LA    R7,INPUT                ADDRESS INPUT
PUTLOOP  PUT   INTRDR,(R7)             WRITE RECORD TO INTRDR
         LA    R7,80(,R7)              ADDRESS NEXT RECORD
         CLC   0(4,R7),=F'0'           RECORD EXIST?
         BNE   PUTLOOP                 YES - GO WRITE IT OUT
         B     READMEM                 NO - GO GET SOME MORE
PDSEOD   CLOSE (INTRDR,,JOBFILE)
******************************************************************
SUBEND   TIME  DEC
         ST    R1,DATE                 STORE DATE
         ST    R0,TIME                 STORE TIME
         LA    R2,4(,R2)               ADDRESS NEXT JOB
         C     R2,DAYEND               END OF DAY?
         BE    DAYCHECK                YES - CHECK FOR SAME DAY
         L     R3,0(,R2)               ADDRESS JOB
         LTR   R3,R3                   ANY MORE WORK FOR TODAY?
         BZ    DAYCHECK                NO - CHECK FOR SAME DAY
         CLC   0(2,R3),TIME            JOB SHOULD HAVE BEEN SUBMITTED?
         BNH   SUBMIT                  YES - GO SUBMIT IT
         B     WAITTIME                NO - THEN GO SET UP
DAYCHECK CLC   DATE+2(2),JULDAY        STILL IN THE SAME DAY?
         BE    DAYSLEEP                YES - THEN SLEEP FOR THE REST
         B     NEWDAY                  NO - THEN START OVER...NEW DAY
******************************************************************
DAYSLEEP MVC   DOUBLE,=C'23595900'     WAKE UP AT MIDNIGHT
         L     R8,=A(TIMEEXIT)         GET TIMER EXIT ADDRESS
         STIMER REAL,(R8),TOD=DOUBLE   WAIT UNTIL SUBMIT TIME
         XC    WAITECB(4),WAITECB      CLEAR THE ECB
         LA    R8,WAITECB              GET ECB ADDRESS
         WAIT  1,ECB=(R8)              WAIT FOR THE TIME TO EXPIRE
         TM    FLAG,HALTPGM            PROGRAM HAS BEEN HALTED?
         BZ    NORET02                 NO - DON'T GO BACK
         TTIMER CANCEL
         L     R15,=F'8'               SET RETURN CODE 8
         B     NONZERO                 RETURN NONZERO
NORET02 EQU *
         MVC   DOUBLE,=C'00010000'     ONE MINTUE
         L     R8,=A(TIMEEXIT)         GET TIMER EXIT ADDRESS
         STIMER REAL,(R8),DINTVL=DOUBLE WAIT UNTIL SUBMIT TIME
         XC    WAITECB(4),WAITECB      CLEAR THE ECB
         LA    R8,WAITECB              GET ECB ADDRESS
         WAIT  1,ECB=(R8)              WAIT FOR THE TIME TO EXPIRE
         TM    FLAG,HALTPGM            PROGRAM HAS BEEN HALTED?
         BZ    NORET03                 NO - DON'T GO BACK
         TTIMER CANCEL
         L     R15,=F'12'              SET RETURN CODE 12
         B     NONZERO                 RETURN NONZERO
NORET03  EQU   *
NEWDAY   TIME  DEC
         ST    R1,DATE                 STORE DATE
         MVC   TIME,=X'00000100'       SET FOR FIRST SECOND OF NEW DAY
         B     GETDAY2                 GO CALCULATE THE DAY OF THE WEEK
******************************************************************
RETURN   L     R13,SAVE+4              RESTORE SAVEAREA
         LM    R14,R12,12(R13)         RESTORE ENVIRONMENT
         XR    R15,R15                 SET RETURN CODE
         BR    R14
NONZERO  L     R13,SAVE+4
         L     R14,12(,R13)
         LM    R0,R12,20(R13)
         BR    R14
EXPIRED  WTO   'SCHED016 - CURRENT YEAR IS NOT SUPPORTED - PROGRAM UPDAX
               TE REQUIRED',ROUTCDE=(1),DESC=(1)
         B     RETURN
OPENERR2 WTO   'SCHED022 - JOBFILE DATASET FAILED TO OPEN, CORRECT PROBX
               LEM AND RESTART',ROUTCDE=(1),DESC=(1)
         B     RETURN
OPENERR3 WTO   'SCHED024 - CMDFILE DATASET FAILED TO OPEN, CORRECT PROBX
               LEM AND RESTART',ROUTCDE=(1),DESC=(1)
         B     RETURN
DAYTBL   DC    A(SUNDAY)
         DC    A(MONDAY)
         DC    A(TUESDAY)
         DC    A(WEDNESDY)
         DC    A(THURSDAY)
         DC    A(FRIDAY)
         DC    A(SATURDAY)
SUNDAY   DC    C'SUNDAY   '
MONDAY   DC    C'MONDAY   '
TUESDAY  DC    C'TUESDAY  '
WEDNESDY DC    C'WEDNESDAY'
THURSDAY DC    C'THURSDAY '
FRIDAY   DC    C'FRIDAY   '
SATURDAY DC    C'SATURDAY '
DBL1     DS    2D
DBL2     DS    2D
SAVE     DS    18F
STORLIM  DS    F
DAYEND   DS    F
ECBADDR  DS    F
CMD      DC    X'004C0000',80C' '
INTRDR   DCB   DSORG=PS,MACRF=PM,DDNAME=INTRDR,LRECL=80
JOBFILE  DCB   DSORG=PO,DDNAME=JOBFILE,EODAD=PDSEOD,MACRF=R
CMDFILE  DCB   DSORG=PO,DDNAME=CMDFILE,EODAD=PDSEOD02,MACRF=R
         LTORG
INPUT    DS    CL32720
         DC    F'0'
DATAAREA DSECT
         DS    0D
FLAG     DS    XL1
DAILY    EQU   X'80'
CON      EQU   X'40'
HALTPGM  EQU   X'02'
WORK     DS    CL5
CURRDAY  DS    X
JULDAY   DS    CL2
RELVALUE DS    CL3
CONVALH  DS    CL3
CONVALM  DS    CL2
DOUBLE   DS    D
DATE     DS    F
TIME     DS    F
**********************
SUN      EQU   0
MON      EQU   1
TUES     EQU   2
WED      EQU   3
THUR     EQU   4
FRI      EQU   5
SAT      EQU   6
**********************
YEARTAB  DS    XL1,AL1
         DS    XL1,AL1
         DS    XL1,AL1
         DS    XL1,AL1
         DS    XL1,AL1
         DS    XL1,AL1
         DS    XL1,AL1
         DS    XL1,AL1
         DS    XL1,AL1
         DS    XL1,AL1
         DS    XL1,AL1
         DS    XL1,AL1
         DS    XL1,AL1
         DS    XL1
DAYTABLE DS    AL1,AL1,AL1,AL1,AL1,AL1
         DS    AL1,AL1,AL1,AL1,AL1,AL1
         DS    AL1,AL1
NXTDPTRS DS    A,A,A,A,A,A,A
DAYPTRS  DS    A,A,A,A,A,A,A
NXTJPTRS DS    A
JOBPTRS  DS    A
DAYLEN   DS    F
WAITECB  DS    F
DATALEN  EQU   *-DATAAREA
         DCBD  DSORG=PS
TIMEEXIT CSECT
         STM   R14,R12,12(R13)              SAVE ENVIRONMENT
         LR    R11,R15                      SET UP ...
         USING TIMEEXIT,R11                    EXIT ADDRESSABILITY
         ST    R13,EXITSV02+4
         LA    R13,EXITSV02
         L     R3,=A(ECBADDR)               GET ADDRESS OF ECB ADDRESS
         L     R4,0(,R3)                    GET ECB ADDRESS
         POST  (R4)
         L     R13,EXITSV02+4
         LM    R14,R12,12(R13)
         XR    R15,R15
         BR    R14
EXITSV02 DS    18F
         LTORG
         END
ZZ
//SYSPUNCH DD  SYSOUT=8
//SYSGO    DD  DISP=(MOD,PASS,DELETE),UNIT=SYSDA,
//    DSN=&&OBJLIB,SPACE=(CYL,(2,2))
//LKED2A  EXEC PGM=IEWL,
//             PARM='XREF,LIST,LET,TEST,AC=1',
//             REGION=1024K,COND=(0,NE)
//SYSLMOD  DD  DSN=SYS2.LINKLIB(SCHEDM02),DISP=SHR
//SYSLIN   DD  DSN=&&OBJLIB,DISP=(OLD,PASS,DELETE)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(8,1))
//SYSPRINT DD  SYSOUT=*
//LKED2B  EXEC PGM=IEWL,
//             PARM='XREF,LIST,LET,TEST,AC=1',
//             REGION=1024K,COND=(0,NE)
//SYSLMOD  DD  DSN=SYS2.LINKLIB,DISP=SHR
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(8,1))
//SYSPRINT DD  SYSOUT=*
//SYSLIN   DD  *
 INCLUDE SYSLMOD(SCHEDM02)
 ENTRY SCHEDM02
 NAME SCHEDM02(R)
/*
//
./ ENDUP
QQ
//*
//* ---------------------------------------------------------------
//* Create the dummy job and command members so the scheduler can
//* be started with the example sampparm member.
//* ---------------------------------------------------------------
//LOADDUMJ EXEC PGM=IEBUPDTE,COND=(0,NE)
//SYSPRINT DD   SYSOUT=*
//SYSUT1   DD   DISP=SHR,DSN=SYSGEN.SCHEDULE.JOBFILE
//SYSUT2   DD   DISP=SHR,DSN=SYSGEN.SCHEDULE.JOBFILE
//SYSIN    DD   DATA,DLM=QQ
./ ADD NAME=SAMPJOB1
//MARK001J JOB  (0),'DUMMY JOB',CLASS=A,MSGCLASS=A
//DUMMY    EXEC PGM=IEBGENER
//SYSPRINT DD   SYSOUT=*
//SYSUT1   DD   DATA,DLM=ZZ
This is just a dummy job to make sure the scheduler
has a job to execute. 
So nothing to see here.
ZZ
//SYSUT2   DD   SYSOUT=*
//SYSIN    DD   DUMMY
//
./ ENDUP
QQ
//LOADDUMC EXEC PGM=IEBUPDTE,COND=(0,NE)
//SYSPRINT DD   SYSOUT=*
//SYSUT1   DD   DISP=SHR,DSN=SYSGEN.SCHEDULE.CMDFILE
//SYSUT2   DD   DISP=SHR,DSN=SYSGEN.SCHEDULE.CMDFILE
//SYSIN    DD   DATA,DLM=QQ
./ ADD NAME=SAMPCMD1
D U,DASD,ONLINE
D U,TAPE,ONLINE
./ ADD NAME=SAMPCMD2
D U,DASD,OFFLINE
D U,TAPE,OFFLINE
./ ENDUP
QQ
//LOADDEBG EXEC PGM=IEBUPDTE,COND=(0,NE)
//SYSPRINT DD   SYSOUT=*
//SYSUT1   DD   DISP=SHR,DSN=SYSGEN.SCHEDULE.DEBUG  
//SYSUT2   DD   DISP=SHR,DSN=SYSGEN.SCHEDULE.DEBUG  
//SYSIN    DD   DATA,DLM=QQ
./ ADD NAME=$DOC
DEBUGGING UTILITIES
You should probably not use these. I created them simply to
try to check that my mucking about with converting STIMERM
calls to STIMER calls was not causing problems.

All assemplies are into my personal load library so you will
need to edit these jobs before running them to use your own
load libraries.

CKPTSHOW - displays the checkpoint last termination, last
           checkpoint, and curent system timestamps so
           checkpoint file activity can be confirmed as 
           actually happening
           While I could have actually put this into the scheduler
           to be triggered by a 'F stc,somecommand' request I
           decided as it is a debugging aid only it should stay
           seperate for now
CKPTHARD - sets last termonation timestamps to zeros to test
           the harddown recovery processing
./ ADD NAME=CKPTSHOW
//ASMCHECK JOB (TSO),
//             'ASSEMBLE CHECK',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(0,0),
//             REGION=0K,
//             USER=IBMUSER,
//             PASSWORD=SYS1
//* ===============================================================
//*
//* WTO THE TIMESTAMPS IN THE CHECKPOINT FILE
//*
//* This program just displays the last checkpoint entries for
//* termination timestamp, active checkpoint timestamp, and
//* the current date/time timestamp fpr comparison.
//*
//* (1) ASSEMBLES INTO MY PESONAL PROGRAM LIBRARY, CHANGE TO YOUR
//*     PERSONAL LIBRARY (GLOBALLY CHANGE IBMUSER.LOAD to whatever)
//* (2) RUNS IF ASSEMBLY WAS OK (change the CHKPOINT DD to wherever
//*     you installed the application to.
//*
//* ===============================================================
//ASM1     EXEC PGM=IFOX00,
//   PARM='DECK,LOAD,TERM,TEST,SYSPARM((NOSP,NODEBUG)),XREF(SHORT)',
//   REGION=4096K,COND=(0,NE)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=SYS2.MACLIB
//SYSUT1   DD  SPACE=(CYL,(25,5)),UNIT=SYSDA
//SYSUT2   DD  SPACE=(CYL,(25,5)),UNIT=SYSDA
//SYSUT3   DD  SPACE=(CYL,(25,5)),UNIT=SYSDA
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DATA,DLM=ZZ
***********************************************************************
*
* READCKPT: MID
*
* CREATED SIMPLY SO I CAN CONFIRM THE CHECKPOINT FILE IS BEING
* REGULARLY UPDATED AFTER I HAD CHANGED ALL THE STIMERM CALLS TO
* STIMER CALLS.
*
***********************************************************************
READCKPT CSECT
         STM   R14,R12,12(R13)
         BALR  R12,R0
         USING *,R12
         LA    R15,SAVEAREA
         ST    R15,8(R13)
         ST    R13,4(R15)
         LR    R13,R15
*----------------------------------------------------------------------
*         RECORD CONTAINS TIME DEC REGISTERS UNMODIFIED
*                R0,R1   (TIME,DATE)
* CKPTREC(8)     TERMINATION TIME
* CKPTREC+8(8)   CHECKPOINT TIME
*----------------------------------------------------------------------
*
         OPEN  (CHKPOINT,INPUT)        OPEN CHECKPOINT DATASET
         TM    CHKPOINT+48,X'10'       OPEN OK?
         BZ    RCKWTO04                NO - ISSUE ERROR MESSAGE
         GET   CHKPOINT,CHKPTREC       READ CHECKPOINT RECORD
         CLOSE CHKPOINT                CLOSE THE DATASET
*
         CLC   CHKPTREC(8),=C'CHKPOINT'   UNINITIALISED?
         BE    RCKWTO03
         CLC   CHKPTREC+8(8),=C'CHKPOINT' UNINITIALISED?
         BE    RCKWTO03
* ------ PLACE THE LAST OK TERMINATION CHECKPOINT DATE
* ------ FROM THE CHECKPOINT FILE INTO THE WTO
         L     R1,CHKPTREC             FIRST TIME
         ST    R1,SAARG                
         L     R1,CHKPTREC+4           FIRST DATE
         ST    R1,SAARG+4
         UNPK  SACHR,SAARG             PACKED TO EBCDIC
         OI    SACHR+15,X'F0'          REPAIR SIGN
         MVC   RCKWTO09+29(3),SACHR+12    MOVE IN DDD
         MVC   RCKWTO09+33(2),SACHR       MOVE IN HH  
         MVC   RCKWTO09+36(2),SACHR+2     MOVE IN MM  
* ------ PLACE THE CURRENT (LAST) CHECKPOINT DATE
* ------ FROM THE CHECKPOINT FILE INTO THE WTO
         L     R1,CHKPTREC+8           SECOND TIME
         ST    R1,SAARG                
         L     R1,CHKPTREC+12          SECOND DATE
         ST    R1,SAARG+4
         UNPK  SACHR,SAARG             PACKED TO EBCDIC
         OI    SACHR+15,X'F0'          REPAIR SIGN
         MVC   RCKWTO09+50(3),SACHR+12    MOVE IN DDD
         MVC   RCKWTO09+54(2),SACHR       MOVE IN HH  
         MVC   RCKWTO09+57(2),SACHR+2     MOVE IN MM  
*        B     RCKWTO09
* ------ PLACE THE CURRENT TIME INTO THE WTO FOR REFERENCE
         TIME  DEC
         ST    R0,SAARG                CURRENT TIME
         ST    R1,SAARG+4              CURRENT DATE
*        AP    SAARG(4),=P'1900000'    Y2K DATE FIX
         UNPK  SACHR,SAARG             PACKED TO EBCDIC
         OI    SACHR+15,X'F0'          REPAIR SIGN
         MVC   RCKWTO09+64(3),SACHR+12    MOVE IN DDD
         MVC   RCKWTO09+68(2),SACHR       MOVE IN HH  
         MVC   RCKWTO09+71(2),SACHR+2     MOVE IN MM  
* ------ THE WTO BEING USED
RCKWTO09 WTO   'SCHED026 - TERM CKPT=DDD HH:MM, CURR CKPT=DDD HH:MM NOWX
               =DDD HH:MM'
*
RCKEXIT  L     R13,SAVEAREA+4
         LM    R14,R12,12(R13)
         SLR   R15,R15
         BR    R14
*
RCKWTO03 WTO   'SCHED028 - CHECKPOINT NOT YET INITALISED'
         B     RCKEXIT
RCKWTO04 WTO   'SCHED029 - UNABLE TO OPEN DD CHKPOINT'
         B     RCKEXIT
         EJECT
SAVEAREA DS    18F             
*
* THE NEXT THREE VARIABLES ARE USED IN CONVERTING THE DATE
* REGISTER VALUES IN THE CHECKPOINT RECORD TO EBCDIC.
SAARG    DS    D                HHMMSSth,YYYYDDDF
*                              +0 1 2 3  4 5 6 7
SACHR    DS    CL15
RCKBYT16 DS    CL1              SACHR NEEDS LEN OF 15 FOR UNPK BUT
*                               ACTUAL AREA MUST BE 16 SO FILLER BYTE
*
* CHECKPOINT RECORD AND DCB
         DS    0F               ALIGN ON FULLWORD SO 'L Rx' CAN USE IT
CHKPTREC DC    CL80' '          
CHKPOINT DCB   MACRF=(GM),DSORG=PS,LRECL=80,DDNAME=CHKPOINT
         YREGS
         END
ZZ
//SYSPUNCH DD  SYSOUT=8
//SYSGO    DD  DISP=(MOD,PASS,DELETE),UNIT=SYSDA,
//    DSN=&&OBJLIB,SPACE=(CYL,(2,2))
//LKED1A  EXEC PGM=IEWL,
//             PARM='XREF,LIST,LET,TEST,AC=0',
//             REGION=1024K,COND=(0,NE)
//SYSLMOD  DD  DSN=IBMUSER.LOAD(READCKPT),DISP=SHR
//SYSLIN   DD  DSN=&&OBJLIB,DISP=(OLD,PASS,DELETE)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(8,1))
//SYSPRINT DD  SYSOUT=*
//LKED1B  EXEC PGM=IEWL,
//             PARM='XREF,LIST,LET,TEST,AC=0',
//             REGION=1024K,COND=(0,NE)
//SYSLMOD  DD  DSN=IBMUSER.LOAD,DISP=SHR
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(8,1))
//SYSPRINT DD  SYSOUT=*
//SYSLIN   DD  *
 INCLUDE SYSLMOD(READCKPT)
 ENTRY READCKPT
 NAME READCKPT(R)
/*
//RUNIT    EXEC PGM=READCKPT,COND=(0,NE)
//STEPLIB  DD   DISP=SHR,DSN=IBMUSER.LOAD
//CHKPOINT DD   DISP=SHR,DSN=SYSGEN.SCHEDULE.CHKPOINT
//
./ ADD NAME=CKPTHARD
//MARKSCHD JOB (0),'SCHEDULE',CLASS=A,MSGLEVEL=(1,1),MSGCLASS=T
//* ===============================================================
//*
//* OVERWRITE THE LAST TERMINATION TIME WITH ZEROS TO TRY
//* TO TEST THE HARDDOWN RECOVERY PROCESSING.
//*
//* This should be done when the scheduler is shutdown as HARD
//* recovery processing will only trigger when the scheduler
//* first starts.
//*
//* (1) ASSEMBLES INTO MY PESONAL PROGRAM LIBRARY, CHANGE TO YOUR
//*     PERSONAL LIBRARY (GLOBALLY CHANGE IBMUSER.LOAD to whatever)
//* (2) RUNS IF ASSEMBLY WAS OK (change the CHKPOINT DD to wherever
//*     you installed the application to.
//*
//* ===============================================================
//ASM1     EXEC PGM=IFOX00,
//   PARM='DECK,LOAD,TERM,TEST,SYSPARM((NOSP,NODEBUG)),XREF(SHORT)',
//   REGION=4096K,COND=(0,NE)
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//         DD  DISP=SHR,DSN=SYS2.MACLIB
//SYSUT1   DD  SPACE=(CYL,(25,5)),UNIT=SYSDA
//SYSUT2   DD  SPACE=(CYL,(25,5)),UNIT=SYSDA
//SYSUT3   DD  SPACE=(CYL,(25,5)),UNIT=SYSDA
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DATA,DLM=ZZ
***********************************************************************
*
* ZAPCKPT: MID
*
* DEBUGGING AID.
*
* OVERWITE THE CHECKPOINT FILE LAST CHECKPOINT TIME WITH ZEROS, 
* LEAVE THE TIME VALUES IN THE LAST TERMINATION DATE ALONE.
*
* THIS WILL CAUSE THE SCHEDULER TO RERUN ALL THE JOBS FOR THE DAY
* UP UNTIL THE CURRENT TIME...
* ...BUT MORE IMPORTANTLY IT WILL TEST THE HARDDOWN WTOR PROCESSING
* WHICH IS THE ENTIRE POINT OF THIS PROGRAM, I NEEDED A WAY TO TEST
* THOSE.
*
* S C H E D U L E    SHOULD BE SHUTDOWN
*
***********************************************************************
ZAPCKPT CSECT
         STM   R14,R12,12(R13)
         BALR  R12,R0
         USING *,R12
         LA    R15,SAVEAREA
         ST    R15,8(R13)
         ST    R13,4(R15)
         LR    R13,R15
*----------------------------------------------------------------------
*         RECORD CONTAINS TIME DEC REGISTERS UNMODIFIED
*                R0,R1   (TIME,DATE)
* CKPTREC(8)     TERMINATION TIME
* CKPTREC+8(8)   CHECKPOINT TIME
*         WE OVERWRITE TERMINATION TIME
*----------------------------------------------------------------------
         OPEN  (CHKPOINT,INPUT)        OPEN CHECKPOINT DATASET
         GET   CHKPOINT,CHKPTREC       READ CHECKPOINT RECORD
         CLOSE CHKPOINT                CLOSE THE DATASET
* ------ OVERWRITE THE LAST TERMINATION TIMESTAMP WITH ZEROS
*------- AND REWRITE THE RECORD
         SR    R2,R2
         ST    R2,CHKPTREC+8
         ST    R2,CHKPTREC+12
         OPEN  (CHKPOINT,OUTPUT)       OPEN CHECKPOINT DATASET
         PUT   CHKPOINT,CHKPTREC       WRITE OUT LAST CKPT RECORD
         CLOSE CHKPOINT 
RCKEXIT  L     R13,SAVEAREA+4
         LM    R14,R12,12(R13)
         SLR   R15,R15
         BR    R14
SAVEAREA DS    18F             
*
* CHECKPOINT RECORD AND DCB
         DS    0F               ALIGN ON FULLWORD SO 'L Rx' CAN USE IT
CHKPTREC DC    CL80' '          
CHKPOINT DCB   MACRF=(GM,PM),DSORG=PS,LRECL=80,DDNAME=CHKPOINT
         YREGS
         END
ZZ
//SYSPUNCH DD  SYSOUT=8
//SYSGO    DD  DISP=(MOD,PASS,DELETE),UNIT=SYSDA,
//    DSN=&&OBJLIB,SPACE=(CYL,(2,2))
//LKED1A  EXEC PGM=IEWL,
//             PARM='XREF,LIST,LET,TEST,AC=0',
//             REGION=1024K,COND=(0,NE)
//SYSLMOD  DD  DSN=IBMUSER.LOAD(ZAPCKPT),DISP=SHR
//SYSLIN   DD  DSN=&&OBJLIB,DISP=(OLD,PASS,DELETE)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(8,1))
//SYSPRINT DD  SYSOUT=*
//LKED1B  EXEC PGM=IEWL,
//             PARM='XREF,LIST,LET,TEST,AC=0',
//             REGION=1024K,COND=(0,NE)
//SYSLMOD  DD  DSN=IBMUSER.LOAD,DISP=SHR
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(8,1))
//SYSPRINT DD  SYSOUT=*
//SYSLIN   DD  *
 INCLUDE SYSLMOD(ZAPCKPT)
 ENTRY ZAPCKPT
 NAME ZAPCKPT(R)
/*
//RUNIT    EXEC PGM=ZAPCKPT,COND=(0,NE)
//STEPLIB  DD   DISP=SHR,DSN=IBMUSER.LOAD
//CHKPOINT DD   DISP=SHR,DSN=IBMUSER.SCHEDULE.CHKPOINT
//
./ ENDUP
QQ
//SUBJOBS  EXEC PGM=IEBGENER            
//SYSPRINT DD   SYSOUT=*                
//SYSUT1   DD   DISP=SHR,               
//    DSN=SYSGEN.SCHEDULE.SRC(SCHEDM02) 
//         DD   DISP=SHR,               
//    DSN=SYSGEN.SCHEDULE.SRC(SCHEDM01) 
//SYSUT2   DD   SYSOUT=(A,INTRDR)       
//SYSIN    DD   DUMMY                                                        
//PROCINST EXEC PGM=IEBGENER
//SYSUT1   DD DATA,DLM=@@
//SCHEDULE PROC PREFIX='SYSGEN.SCHEDULE',     SCHEDULR DS PREFIX        
// PARMS='SAMPPARM',                          PARMLIB MEMBER NAME       
// PARMLIB='SYSGEN.SCHEDULE.SRC',            SYSTEM PARMLIB TO USE      
// JOBQ='A'                                   INTRDR JOB CLASS FOR JOBS 
//SCHEDULE EXEC PGM=SCHEDM01                                            
//SYSIN    DD   DISP=SHR,                                               
//  DSN=&PARMLIB(&PARMS)                                                
//JOBFILE  DD   DISP=SHR,                                               
//  DSN=&PREFIX..JOBFILE                                                
//CMDFILE  DD   DISP=SHR,                                               
//  DSN=&PREFIX..CMDFILE                                                
//CHKPOINT DD   DISP=SHR,                                               
//  DSN=&PREFIX..CHKPOINT                                               
//INTRDR   DD SYSOUT=(&JOBQ,INTRDR)                                     
//*SYSABEND DD SYSOUT=A                                                            MACRO
@@
//SYSUT2   DD DISP=SHR,DSN=SYS2.PROCLIB(SCHEDULE)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//


                                     