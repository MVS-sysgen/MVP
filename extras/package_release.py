#!/usr/bin/env python3

# This script is for building releases for MVP (MVS/CE Package manager)
# It works by creating an XMI file which contains:
# - Task file(s), JCL files that will be submitted in numbered order and follow
#   a specific naming convention (#nnnJCL where nnn is 001 through 999) Note: the
#   jobname MUST match the filename.
# - XMI file(s) that makes up the release
# MVS/CE is required to use this script
# Author: Soldier of FORTRAN
# License: GPLv3

import zipfile, io, shutil, ebcdic, os, sys, time
import subprocess
import threading
import queue
import socket
from pathlib import Path
import argparse
from datetime import datetime

with open("build.log", 'w') as log:
    now = datetime.now()
    current_time = now.strftime("%x %X")
    log.write("This package was generated with package_release.py\n")
    log.write("Package generated on: {}\n".format(current_time))
    log.write("Command arguments: ")
    log.write(str(sys.argv))

my_parser = argparse.ArgumentParser()
my_parser.add_argument('--xmi-files','-x',  required=True, help="xmi file(s) to be included", nargs="+")
my_parser.add_argument('--task-files', '-t', required=True, help="JCL file(s) to be included", nargs="+")
my_parser.add_argument('--mvsce', '-m', default="MVSCE", help="MVSCE Folder location to generate MVP XMI file")
my_parser.add_argument('--name', '-a', default="RELEASE.xmit", help="Release XMI file output path/name")
my_parser.add_argument('--no-automation', '-n', help="Prevents running XMI build automation and just creates EBCDIC jcl file", action="store_true")
my_parser.add_argument('--dlm','-d', help="JCL Delimeter to use", default="??")
my_parser.add_argument('--blk','-b', help="Blocksize for XMI container", default="800")
args = my_parser.parse_args()

error_check = [
                'open error',
                'Creating crash dump',
                'DISASTROUS ERROR',
                'HHC01023W Waiting for port 3270 to become free for console connections',
                'disabled wait state 00020000 80000005'
              ]

quit_herc_event = threading.Event()
kill_hercules = threading.Event()
reset_herc_event = threading.Event()
STDERR_to_logs = threading.Event()
running_folder = os.getcwd()

class herc_automation:

    def __init__(self,
                config="conf/local.cnf",
                rc="conf/mvsce.rc"
                ):

        self.config = config
        self.rc = rc
        self.hercproc = False
        self.stderr_q = queue.Queue()
        self.stdout_q = queue.Queue()

    def kill(self):
        self.hercproc.kill()

    def start_threads(self):
        # start a pair of threads to read output from hercules
        self.stdout_thread = threading.Thread(target=self.queue_stdout, args=(self.hercproc.stdout,self.stdout_q))
        self.stderr_thread = threading.Thread(target=self.queue_stderr, args=(self.hercproc.stderr,self.stderr_q))
        self.check_hercules_thread = threading.Thread(target=self.check_hercules, args=[self.hercproc])
        # self.queue_printer_thread = threading.Thread(target=self.queue_printer, args=('prt00e.txt',printer_q))
        self.stdout_thread.daemon = True
        self.stderr_thread.daemon = True
        # self.queue_printer_thread.daemon = True
        self.check_hercules_thread.daemon = True
        self.stdout_thread.start()
        self.stderr_thread.start()
        self.check_hercules_thread.start()
        # self.queue_printer_thread.start()

    def queue_stdout(self, pipe, q):
        ''' queue the stdout in a non blocking way'''
        global reply_num
        while True:

            l = pipe.readline()
            if len(l.strip()) > 0:
                if len(l.strip()) > 3 and l[0:2] == '/*' and l[2:4].isnumeric():
                    reply_num = l[2:4]
                    print("Reply number set to {}".format(reply_num))
                if  "HHC90020W" not in l and "HHC00007I" not in l and "HHC00107I" not in l and "HHC00100I" not in l:
                    # ignore these messages, they're just noise
                    # HHC90020W 'hthread_setschedparam()' failed at loc=timer.c:193: rc=22: Invalid argument
                    # HHC00007I Previous message from function 'hthread_set_thread_prio' at hthreads.c(1170)
                    print("[HERCLOG] {}".format(l.strip()))
                    q.put(l)
                    for errors in error_check:
                        if errors in l:
                            print("Quiting! Irrecoverable Hercules error: {}".format(l.strip()))
                            kill_hercules.set()
            if reset_herc_event.is_set():
                break

    def queue_stderr(self, pipe, q):
        ''' queue the stderr in a non blocking way'''
        while True:
            l = pipe.readline()
            if len(l.strip()) > 0:
                if STDERR_to_logs.is_set():
                    print("[DIAG] {}".format(l.strip()))
                if 'MIPS' in l:
                    print("[DIAG] {}".format(l.strip()))
                q.put(l)

                for errors in error_check:
                    if errors in l:
                        print("Quiting! Irrecoverable Hercules error: {}".format(l.strip()))
                        kill_hercules.set()
            if reset_herc_event.is_set():
                break

    def check_hercules(self, hercproc):
        ''' check to make sure hercules is still running '''
        while hercproc.poll() is None:
            if quit_herc_event.is_set() or reset_herc_event.is_set():
                print("Quit Event enabled exiting hercproc monitoring")
                return
            if kill_hercules.is_set():
                hercproc.kill()
                break
            continue

        print("ERROR - Hercules Exited Unexpectedly")
        os._exit(1)

    def check_maxcc(self, jobname, steps_cc={}, printer_file='printers/prt00e.txt'):
      '''Checks job and steps results, raises error
          If the step is in steps_cc, check the step vs the cc in the dictionary
          otherwise checks if step is zero
      '''
      print("Checking {} job results".format(jobname))

      found_job = False
      failed_step = False

      logmsg = '[MAXCC] Jobname: {:<8} Procname: {:<8} Stepname: {:<8} Exit Code: {:<8}'

      with open(printer_file, 'r', errors='ignore') as f:
          for line in f.readlines():
              if 'IEF142I' in line and jobname in line:

                  found_job = True

                  x = line.strip().split()
                  y = x.index('IEF142I')
                  j = x[y:]

                  log = logmsg.format(j[1],'',j[2],j[10])
                  maxcc=j[10]
                  stepname = j[2]

                  if j[3] != "-":
                      log = logmsg.format(j[1],j[2],j[3],j[11])
                      stepname = j[3]
                      maxcc=j[11]

                  print(log)

                  if stepname in steps_cc:
                      expected_cc = steps_cc[stepname]
                  else:
                      expected_cc = '0000'

                  if maxcc != expected_cc:
                      error = "Step {} Condition Code does not match expected condition code: {} vs {} review prt00e.txt for errors".format(stepname,j[-1],expected_cc)
                      print(error)
                      failed_step = True

      if not found_job:
          raise ValueError("Job {} not found in printer output {}".format(jobname, printer_file))
      if failed_step:
          raise ValueError(error)


    def reset_hercules(self):
        print('Restarting hercules')
        self.quit_hercules(msg=False)

        # drain STDERR and STDOUT
        while True:
            try:
                line = self.stdout_q.get(False).strip()
            except queue.Empty:
                break

        while True:
            try:
                line = self.stderr_q.get(False).strip()
            except queue.Empty:
                break

        reset_herc_event.set()

        try:
            self.hercmd = subprocess.check_output(["which", "hercules"]).strip()
        except:
            raise Exception('hercules not found')

        print("Launching hercules")

        h = ["hercules", '--externalgui', '-f',self.config, '-r', self.rc]
        print(h)

        self.hercproc = subprocess.Popen(h,
                    stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    universal_newlines=True)
        reset_herc_event.clear()
        quit_herc_event.clear()
        self.start_threads()

        self.rc = self.hercproc.poll()
        if self.rc is not None:
            raise("Unable to start hercules")
        print("Hercules launched")
        #self.write_logs()
        print("Hercules Re-initialization Complete")


    def quit_hercules(self, msg=True):
        if msg:
            print("Shutting down hercules")
        if not self.hercproc or self.hercproc.poll() is not None:
            print("Hercules already shutdown")
            return
        quit_herc_event.set()
        self.send_herc('quit')
        self.wait_for_string('Hercules shutdown complete', stderr=True)
        if msg:
            print('Hercules has exited')

    def wait_for_string(self, string_to_waitfor, stderr=False, timeout=False):
        '''
           Reads stdout queue waiting for expected response, default is
           to check STDOUT queue, set stderr=True to check stderr queue instead
           default timeout is 30 minutes
        '''
        time_started = time.time()

        if not timeout:
            timeout = 1800

        if not timeout and self.timeout:
            timeout=self.timeout

        print("Waiting for string to appear in hercules log: {}".format(string_to_waitfor))

        while True:
            if time.time() > time_started + timeout:
                if self.substep:
                    exception = "Step: {} Substep: {} took too long".format(self.step, self.substep)
                    log = "Step: {} Substep: {} Timeout Exceeded {} seconds".format(self.step, self.substep, timeout)
                else:
                    exception = "Step: {} Timeout".format(self.step, self.substep)
                    log = "Step: {} Timeout Exceeded {} seconds".format(self.step, self.substep, timeout)
                print(log)
                raise Exception(exception)

            try:
                if stderr:
                    line = self.stderr_q.get(False).strip()
                else:
                    line = self.stdout_q.get(False).strip()

                while string_to_waitfor not in line:
                    if stderr:
                        line = self.stderr_q.get(False).strip()
                    else:
                        line = self.stdout_q.get(False).strip()
                    continue
                return

            except queue.Empty:
                continue

    def ipl(self, step_text='', clpa=False):
        print(step_text)
        self.reset_hercules()
        #self.wait_for_string("0:0151 CKD")
        self.wait_for_string("IKT005I TCAS IS INITIALIZED")

    def shutdown_mvs(self, cust=False):
        self.send_oper('$p jes2')
        if cust:
            self.wait_for_string('IEF404I JES2 - ENDED - ')
        else:
            self.wait_for_string('IEF196I IEF285I   VOL SER NOS= SPOOL0.')
        self.send_oper('z eod')
        self.wait_for_string('IEE334I HALT     EOD SUCCESSFUL')
        self.send_oper('quiesce')
        self.wait_for_string("disabled wait state")
        self.send_herc('stop')

    def send_herc(self, command=''):
        ''' Sends hercules commands '''
        print("Sending Hercules Command: {}".format(command))
        self.hercproc.stdin.write(command+"\n")
        self.hercproc.stdin.flush()

    def send_oper(self, command=''):
        ''' Sends operator/console commands (i.e. prepends /) '''
        self.send_herc("/{}".format(command))

    def send_reply(self, command=''):
        ''' Sends operator/console commands with automated number '''
        self.send_herc("/r {},{}".format(reply_num,command))

    def submit(self,jcl, host='127.0.0.1',port=3505, ebcdic=False):
        '''submits a job (in ASCII) to hercules listener'''

        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        try:
            # Connect to server and send data
            sock.connect((host, port))
            if ebcdic:
              sock.send(jcl)
            else:
              sock.send(jcl.encode())

        finally:
            sock.close()

header = '''//HEADER JOB (TSO),
//             'HEADER FOR MVP',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,PASSWORD=SYS1
//* Upload files
//* This file is used by make_release.py to create the package
//* for MVP
//* CLEAN UP
//CLEANUP EXEC PGM=IDCAMS
//SYSIN    DD *
  DELETE MVP.STAGING SCRATCH PURGE
  DELETE MVP.XMIT SCRATCH PURGE
  SET MAXCC=0
  SET LASTCC=0
//SYSPRINT DD SYSOUT=*
//SOURCE   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=MVP.STAGING,DISP=(,CATLG,DELETE),
//             UNIT=SYSDA,VOL=SER=PUB001,SPACE=(TRK,(50,50,10)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE={blk})

//SYSUT1   DD  DATA,DLM='{dlm}'
'''

xmi_upload = '''//COPY     EXEC PGM=IEBGENER
//SYSUT2   DD   DSN=MVP.STAGING({}),DISP=SHR
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   DUMMY
//SYSUT1   DD   DATA,DLM='{dlm}'
'''

footer = '''//XMITLLIB EXEC PGM=XMIT370
//STEPLIB  DD DSN=SYSC.LINKLIB,DISP=SHR
//XMITLOG  DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MVP.STAGING,DISP=SHR
//SYSUT2   DD DSN=&&SYSUT2,UNIT=SYSDA,
//         SPACE=(TRK,(255,255)),
//         DISP=(NEW,DELETE,DELETE)
//XMITOUT  DD SYSOUT=B
//* XMITOUT  DD DSN=&&XMIT,DISP=(,CATLG,DELETE),
//*           UNIT=SYSDA,VOL=SER=PUB001,SPACE=(TRK,(50,50))
'''


# open("punchcards/pch00d.txt", 'a').close()

temp = b''
line = "{:80}"
header = header.format(dlm=args.dlm,blk=args.blk)
for l in header.splitlines():
    t = line.format(l)
    temp += t.encode('cp1047')

xmitin = []
add_file = ''

for i in args.task_files:
    print("Reading {}".format(i))
    if (  i[0] == "#" and
          len(i) >= 5 and
          i[2:4].isnumeric() and
          (i[4:].split('.')[0] == "JCL" or i[4:].split('.')[0] == "REX")
        ):


        filename = i.split(".")[0]
        add_file += "./ ADD NAME={}\n".format(filename)

        with open(i,'r') as infile:
            add_file += infile.read().rstrip("\n") + "\n"
    else:
        print("File {} does not match #nnnJCL/#nnnREX naming convention, skipping")

add_file+= args.dlm

for l in add_file.splitlines():
    t = line.format(l)
    temp += t.encode('cp1047')

end = args.dlm
t = line.format(end)
e_end = t.encode('cp1047')

for i in args.xmi_files:
    print("Reading {}".format(i))
    xmi_ebcdic = b''
    x = xmi_upload.format(i.split(".")[0].upper(),dlm=args.dlm)

    for l in x.splitlines():
        t = line.format(l)
        xmi_ebcdic += t.encode('cp1047')

    with open(i, 'rb') as xmi:
        xmi_ebcdic += xmi.read()
    xmitin.append(xmi_ebcdic + e_end)

for xmi in xmitin:
    temp += xmi


for l in footer.splitlines():
    t = line.format(l)
    temp += t.encode('cp1047')

if args.no_automation:
    with open('output.ebcdic.jcl','wb') as outdd:
        outdd.write(temp)

    exit()

print("Changing to MVS/CE Folder {}".format(args.mvsce))
os.chdir(args.mvsce)
try:
    os.remove("punchcards/pch00d.txt")
except:
    print("punchcards/pch00d.txt Already deleted")
build = herc_automation()
try:
  build.ipl()
  build.send_oper("$s punch1")
  build.wait_for_string("$HASP000 OK")
  build.submit(temp,port=3506,ebcdic=True)
  build.wait_for_string("$HASP190 HEADER   SETUP -- PUNCH1")
  build.send_oper("$s punch1")
  build.wait_for_string("HASP250 HEADER   IS PURGED")
  build.check_maxcc("HEADER")
finally:
  build.quit_hercules()

with open("punchcards/pch00d.txt", 'rb') as punchfile:
  punchfile.seek(160)
  no_headers = punchfile.read()
  no_footers = no_headers[:-80]

os.chdir(running_folder)

with open(args.name, 'wb') as review_out:
  review_out.write(no_footers)


