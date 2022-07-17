   /* MVP: MVS/CE Package Manager */
parse arg action package arguments

   /*                 */
  /* Parse arguments */
 /*                 */

call setg('debug_on',0)
call setg('repo', 'local')

if (upper(action) = '-H' | upper(package) = '-H') then call usage
if pos('-H',upper(arguments)) > 0 then call usage

if (upper(action) = '-D' | upper(action) = '--DEBUG') then do
  call usage
  exit 1
end

if length(action) = 0 then do
  call usage
  exit 1
end

if (upper(package) = '-D' | upper(package) = '--DEBUG') then do
  call setg('debug_on',1)
  call debug "Debugging enabled"
  package = ''
end

args = arguments

do while length(arguments) > 0
  parse upper var arguments p arguments
  if (p = "-D" | or  p = '--DEBUG') then do
    call setg('debug_on',1)
    call debug "Debugging enabled"
  end
end

call debug "MVP Started with the following:"
call debug "Action:" action
call debug "Package:" package
call debug "Arguments:" args

   /*                                        */
  /* Read the parmlib SYS2.PARMLIB(MVP0001) */
 /*                                        */
call debug "Reading config SYS2.PARMLIB(MVP0001)"
"ALLOC FI(PARMS) DA('SYS2.PARMLIB(MVP0001)') SHR "  
"EXECIO * DISKR PARMS (FINIS STEM parms."
if rc > 0 then do
    call error "Error reading SYS2.PARMLIB(MVP0001):" rc
    "FREE F(PARMS)"
    exit 8
end
"FREE F(PARMS)"

call debug "SYS2.PARMLIB(MVP0001) contains" parms.0 "lines"

do i=1 to parms.0
    /* Skip comments */
    call debug "Config line" i||":" parms.i
    if pos("#",parms.i) = 1 then iterate
    /* Get the timeout variable */
    if pos("timeout",parms.i) then do
        parse var parms.i . "=" t
        call setg('timeout', t)
        call debug "Timeout set to:" getg('timeout')
    end
    /* Get the repo location */
    if pos("repo",parms.i) then do
        parse var parms.i . "=" r
        call setg('repo', r)
        call debug "Repo set to:" getg('repo')
    end
end
call debug "Done reading config SYS2.PARMLIB(MVP0001)"

   /*                                         */
  /* Read packages and installed packaged db */
 /*                                         */

call debug "Opening MVP.PACKAGES(CACHE)"
"ALLOC FI(CACHE) DA('MVP.PACKAGES(CACHE)') SHR "
"EXECIO * DISKR CACHE (FINIS STEM cache."
if rc > 0 then do
    call error "Error reading MVP.PACKAGES(CACHE):" rc
    exit 8
end
"FREE F(CACHE)"
call debug "Read" cache.0 "lines"
call debug "MVP.PACKAGES(CACHE) Closed"

call debug "Opening MVP.MVPDB"
"ALLOC FI(MVPDB) DA('MVP.MVPDB') SHR "
"EXECIO * DISKR MVPDB (FINIS STEM mvpdb."

if rc > 0 then do
    call error "Error reading MVP.PACKAGES(MVPDB):" rc
    "FREE F(MVPDB)"
    exit 8
end
"FREE F(MVPDB)"
call debug "Read" mvpdb.0 "lines"
call debug "MVP.MVPDB Closed"

   /*                         */
  /* Which step are we doing */
 /*                         */

SELECT
  WHEN upper(action) == "INSTALL" THEN
    DO
      call install package
    END
  WHEN upper(action) == "LIST" THEN
    DO
      if upper(package) = '--INSTALLED' then do
        say "Listing Installed Packages:"
        say ""
        do i=1 to mvpdb.0
          say "" mvpdb.i
        end
      end
      else if length(package) > 0 then do
        say "Package" package "passed with LIST did you mean INFO?"
        return
      end
      else do
        say "Listing Available Packages:"
        say ""
        do i=1 to cache.0
          parse var cache.i prog . ver
          say " " prog ver
        end
      end
      say ""
      say "Listing Done"
    END
  WHEN (upper(action) == "INFO" | upper(action) == "SHOW") THEN
    DO
      call info package
    END
  WHEN upper(action) == "UPDATE" THEN
    DO
      say "Updating..."
      call run_install_job UPDATE
      if rc > 0 then say "Error with update"
      else say "Update Done"
    END
  WHEN upper(action) == "SEARCH" THEN
    DO
      say "Searching..."
      say ""
      call search package
    END
end

return

   /*                                        */
  /* The end of MVP below are the functions */
 /*                                        */

search:
  /* Search the packages db and the package
     descriptions for the passed word */
  parse arg searchterm
  if length(searchterm) = 0 then return
  call debug "search: searching for:" searchterm

  do i=1 to cache.0
    short_desc = ""
    found = 0
    parse var cache.i progname . .
    if pos(upper(searchterm),progname) > 0 then do
        found = 1
    end
    call debug "search: searching description" progname
    /* Get the description */
    "ALLOC FI(DESCS) DA('MVP.PACKAGES("progname")') SHR "
    "EXECIO * DISKR DESCS (FINIS STEM descsearch."
    if rc > 0 then do
        call error "Error reading MVP.PACKAGES("progname"):" rc
        "FREE F(DESCS)"
        exit 8
    end
    "FREE F(DESCS)"
    do j=1 to descsearch.0

      if pos("Description:", descsearch.j) == 1 then do
        parse var descsearch.j . short_desc
        call debug 'short desc' short_desc
      end

      if pos("PACKAGE:",upper(descsearch.j)) == 1 then iterate
      if pos("VERSION:",upper(descsearch.j)) == 1 then iterate
      if pos("DEPENDS:",upper(descsearch.j)) == 1 then iterate
      if pos("HOMEPAGE:",upper(descsearch.j)) == 1 then iterate

      if pos(upper(searchterm),upper(descsearch.j)) > 0 then do
        found = 1
      end

    end

    if found then do
      parse var cache.i p . v
      say p v
      say " " short_desc
      say ""
    end /* descsearch. loop */
  end /* cache. loop */

  return

install:
  /* install the specific package */
  parse upper arg package

  if length(package) < 1 then do
    call error "INSTALL without package"
    call usage
    return
  end

  doesnt_exist = check_exists(package)

  if doesnt_exist then do
    say "Unable to locate package" package
    return
  end

  already_installed = check_installed(package)

  if already_installed then do
    say package "is already installed"
    return
  end

  call debug "install: Checking for dependencies"

  dependents = check_depends(package)
  call debug "install:" package "requires" dependents "(unsorted)"
  /* First we need to reverse the order */
  revdep = package
  do while dependents \= ''
    parse var dependents idep dependents
    revdep = idep revdep
  end

  call debug "install:" package "requires" revdep "(sorted)"

  total = 0
  nodupes = ''
  do while revdep \= ''
    parse var revdep idep revdep
    if wordpos(idep, nodupes) > 0 then iterate
    if check_installed(idep) then iterate
    total = total + 1
    depstem.total = idep
    nodupes = nodupes idep
  end

  call debug "install: Total packages to install:" total

  depstem.0 = total

  say "The following NEW packages will be installed: "
  do i=1 to depstem.0
    say "  " depstem.i
  end
  say total "packages to install"

  do i=1 to depstem.0
    do j=1 to cache.0
      if wordpos(depstem.i,cache.j) > 0 then do
        parse var cache.j . type vers
        version.i = vers
        type.i = type
        leave
      end
    end
  end

  do i=1 to depstem.0
    pname = depstem.i
    vers = version.i
    type = type.i
    say "  Installing..." pname vers
    call run_install_job "INSTALL" pname type
    call mark_installed pname vers
    call wait(500)
  end

  say "Done"

  return

check_exists:
  /* check if a package exists */
  parse upper arg existing
  call debug "check_exists: checking for" package

  do i=1 to cache.0
    if wordpos(existing, cache.i) > 0 then do
      return 0
    end
  end
  return 1

check_installed:
  /* check if the package is already installed */
  parse upper arg installed

  call debug "check_installed: Checking if" installed "is already installed"

  do i=1 to mvpdb.0
    /* call debug "check_installed:" wordpos(installed, mvpdb.i) */
    if wordpos(installed, mvpdb.i) > 0 then return 1
  end
  call debug "check_installed:" installed "is not installed"

  return 0

mark_installed:
  /* mark a package as installed in the db */
  parse upper arg M_installed M_version


  call debug "mark_installed: Marking " M_installed M_version "as installed"

  mvpdba.0 = 1
  mvpdba.1 = M_installed M_version

  call debug "mark_installed:  allocating MVP.MVPDB to MVPDBA"
  "ALLOC FI(MVPDBA) DA('MVP.MVPDB') SHR REUSE"

  call debug "mark_installed: Appending" mvpdba.1 "to MVP.MVPDB"
  "EXECIO * DISKA MVPDBA (FINIS STEM mvpdba."

  if rc > 0 then do
      call error "Error appending to MVP.MVPDB:" rc
      "FREE F(MVPDBA)"
      exit 8
  end
  "FREE F(MVPDBA)"

  return


info:
  /* prints the package description file */
  parse upper arg package
  call debug "info: getting information for" package

  if length(package) < 1 then do
    call error "INFO without package"
    call usage
    return
  end

  rc = check_exists(package)
  if rc > 0 then do
    call error "Package " package "does not exist"
  end

  call desc package

  do i = 1 to desc.0
    say desc.i
  end
  return

desc:
  /* reads the member from MVP.PACKAGES and stores it in
     the stem desc. */
  parse upper arg member
  call debug "desc: getting description for" package

  "ALLOC FI(DESC) DA('MVP.PACKAGES("member")') SHR "
  "EXECIO * DISKR DESC (FINIS STEM desc."
  if rc > 0 then do
      call error "Error reading MVP.PACKAGES("member"):" rc
      "FREE F(DESC)"
      exit 8
  end
  "FREE F(DESC)"
  return

run_install_job: procedure
  /* Submits a job and waits for the results on MTT */
  parse arg task jobname type
  call debug "run_install_job: starting" jobname type
  /* If this is an update taks the jobname is update */

  highest_jobnum = call get_jobnum

  if highest_jobnum == -1 then return

  if length(jobname) = 0 then jobname = task

  call debug "run_install_job: Jobname:" jobname "stepcc:" stepcc
  if stepcc = 'STEPCC' then stepcc = ''

  

  if getg("repo") == "local" then do

    call debug "run_install_job: Running SH MVP/MVP " task jobname
    ADDRESS COMMAND 'CP SH MVP/MVP ' task jobname
    call debug "run_install_job: Running SH MVP/MVP complete " task jobname

    if rc <> 0 then do
        call error "Error running ADDRESS COMMANND CP SH ./MVP/MVP" task jobname
        return
    end
  end
  else if getg("repo") \= "local" then do
    /* for future expansion of remote repos */
    call error "Remote MVP repos not supported in V1"
    exit 1
  end

  call check_job jobname highest_jobnum

  if type = "JCL" then do
    call wto "MVP001I" jobname "install done"
    return
  end

  if type = "XMI" then call process_XMI jobname

return

get_jobnum: procedure
  /* Reads the MTT and returns the highest jobnum */
  call debug "get_jobnum: Collecting MTT"

  rc = mtt()
  if rc > 0 then do
    do i=_line.0 to 1 by -1
      parse var _line.i . . jobm highest_jobnum .
      if jobm = "JOB" then do
        leave
      end
    end
  end
  else do
    call error "Cannot check job log"
    return -1
  end

  call debug "get_jobnum: Highest Jobnum:" highest_jobnum

  return highest_jobnum

check_job: procedure
  /* Reads the MTT backwards from bottom up and compares
     the found job number with a given job number
     if the job number is higher that the provided job number
     it will then check for IEF records and the provided jobname

     Once it knows the job has ended it checks the condition code for
     each step to confirm it is 0000
  */
  parse arg jobname prev_jobnum

  ended = "IEF404I "||jobname||" - ENDED"
  started = "IEF403I "||jobname||" - STARTED"
  failed = "IEF453I "||jobname||" - JOB FAILED"
  error = "IEF452I "||jobname||"  JOB NOT RUN - JCL ERROR"

  notfound = 1
  attempts = 0
  do while notfound
    call wait(1000)

    /* dont loop forever */
    if attempts >= getg(timeout) then leave
    attempts = attempts + 1

    rc = mtt()
    if rc < 0 then iterate

    call debug "check_job: Searching MTT for job output for" task jobname
    call debug "check_job: Searching for successfull job completion"
    do i=_line.0 to 1 by -1
      parse var _line.i . . jobm cur_jobnum .
      if jobm = "JOB" then do
        /* call debug "check_job:" cur_jobnum "<=" prev_jobnum */
        if cur_jobnum <= prev_jobnum then leave
        else if pos(ended,_line.i) > 0 then do
          call debug "check_job:" jobname "found in MTT"
          jobend = i
          notfound = 0
          leave
        end
        else if pos(failed, _line.i) > 0  then do
          call error jobname "failed to install"
          jobend = i
          notfound = 0
          leave
        end
        else if pos(error, _line.i) > 0  then do
          call error jobname "failed to install"
          jobend = i
          notfound = 0
          leave
        end                                      
        /* else call debug "check_job:" _line.i */
      end
    end
  end

  if attempts >= getg(timeout) then do
    call error "Unable to find" jobname "in MTT after" getg(timeout) seconds
    call error "Install aborting"
    exit -1
    return
  end

  call debug "check_job: searching for" started "in MTT"
  do j=jobend to 1 by -1
    call debug "run_install_job:" _line.j
    if pos(started,_line.j) > 0 then leave
  end

  jobstart = j

  do i=jobstart to jobend
    parse var _line.i . . jobm jobnum type det
    /* skip entries that aren't jobs or have the wrong jobnumber */
    if jobm \= 'JOB' | jobnum \= cur_jobnum then iterate
    if type = "IEFACTRT" then do
      parse var det name '/' prog '/' . '/' . '/' cc '/' jobname
      if cc \= 0000 then do
        call error "Job JOB"||right(jobnum,5,'0') "FAILED"
        call error "Jobname:" jobname "Step:" name "Program:" prog
        call error "MAXCC:" cc
        exit -1
      end
    end
  end

  return

process_XMI:
  /* if the package was an XMI this function processes
     the XMI file by searching for #nnnJCL/#nnnREX members
     in MVP.WORK and running them in order
  */

  call wait(1000)
  parse arg appname

  call debug "process_XMI: Checking if MVP.WORK exists"

  dsn_ok = SYSDSN("'MVP.WORK'")

  if dsn_ok = 'OK' then i=1
  else do
    call error "Attempt to process" appname "MVP.WORK not found"
    exit -1
  end

  call debug "process_XMI: MVP.WORK exists"

  none_found = 1
  total_xmi_tasks = 0


  /*

  This currently breaks EXECIO instead we brute force entries

  x = DIR("'MVP.WORK'")

  call debug "process_XMI: listing MVP.WORK members"
  do i=1 to DIRENTRY.0
    call debug "process_XMI:" i DIRENTRY.i.name
  end

  call debug "process_XMI: locating install JCL/REXX tasks"

  do i=1 to DIRENTRY.0
    if pos("#",DIRENTRY.i.name) = 1 then do
      xmi_num = strip(substr(DIRENTRY.i.name,2,3),,'0')
      xmi_type = substr(DIRENTRY.i.name,5)
      xmi_taskname = DIRENTRY.i.name
      ds = "process_XMI: File" xmi_num "found type" xmi_type "("xmi_taskname")"
      call debug ds
      none_found = 0
      total_xmi_tasks = total_xmi_tasks + 1
      sortin.total_xmi_tasks = xmi_num xmi_type xmi_taskname
    end
  end

  */

  /* *********************************************** */
  /* dirty hack without DIR until V2R5M1 is released */
  /* *********************************************** */
  do i=1 to 10

    dsn_ok = SYSDSN("'MVP.WORK(#"right(i,3,0)"JCL)'")
    if dsn_ok = 'OK' then do
      total_xmi_tasks = total_xmi_tasks + 1
      sortin.total_xmi_tasks = i "JCL" "#"right(i,3,0)"JCL"
      none_found = 0
    end

    dsn_ok = SYSDSN("'MVP.WORK(#"right(i,3,0)"REX)'")
    if dsn_ok = 'OK' then do
      total_xmi_tasks = total_xmi_tasks + 1
      sortin.total_xmi_tasks = i "REX" "#"right(i,3,0)"REX"
      none_found = 0
    end

  end
  /* *********************************************** */
  /* end dirty hack until V2R5M1 is released         */
  /* *********************************************** */

  call debug "process_XMI: found" total_xmi_tasks "install tasks"

  if none_found then do
    call error "Missing preinstall file #001JCL or #001REX for" appname
    exit 8
  end


  sortin.0 = total_xmi_tasks
  call RXSORT

   do i=1 to sortin.0
      parse var sortin.i xmi_num xmi_type xmi_taskname
      call debug "process_XMI: task" xmi_num "type" xmi_type
      if xmi_type == "JCL" then do
        call debug "process_XMI:" appname "Submitting" xmi_taskname
        call submit_XMI_jcl "'MVP.WORK("|| xmi_taskname ||")'" 
      end
  end

  return

submit_XMI_jcl: 
/* Inserts the MVP username and password to the XMI JCL */
/* returns nothing */

  parse arg jcl_location
  hj = get_jobnum()
  call debug "submit_XMI_jcl: Processing" jcl_location

  "ALLOC FI(XMIJCL) DA("||jcl_location||") SHR "
  "EXECIO * DISKR XMIJCL (FINIS STEM XMIJCL."
  if rc > 0 then do
      call error "Error reading "||jcl_location||":" rc
      "FREE F(XMIJCL)"
      exit 8
  end
  "FREE F(XMIJCL)"

  if find(XMIJCL.1,'JOB') < 1 then do
    call error jcl_location ||"is not a valid JOB"
    exit 8
  end

  /* first make sure the queue is empty */
  do queued()
    pull element
    call debug 'submit_XMI_jcl: previous queue item' element
  end

  do j=1 to XMIJCL.0
    
    x=0
    do while (right(XMIJCL.J,x) == left(" ",x))
      /* if we have spaces find the first non space character */
      x = x + 1
      iterate
    end
    
    if right(XMIJCL.J,1) == "," then do
      /* if the first line has a continuation, keep going */
      call debug "submit_XMI_jcl: adding to queue:" XMIJCL.J
      queue XMIJCL.J
      iterate
    end


    jcl = LEFT(XMIJCL.J, LENGTH(XMIJCL.J) - (x - 1))||","
    call debug "submit_XMI_jcl: adding to queue:" jcl
    queue jcl
    jcl = "//    USER=MVP,PASSWORD="||get_pw()
    call debug "submit_XMI_jcl: adding to queue:" jcl
    queue jcl

    leave    
  end

  if j = XMIJCL.0 then do
    call error "Unable to PARSE XMI JCL File " jcl_location
    exit 8
  end

  do j = (j+1) to XMIJCL.0
    call debug "submit_XMI_jcl: adding to queue: '"||XMIJCL.j||"'"
    queue XMIJCL.j
  end

  /*call SUBMIT("'MVP.WORK("|| xmi_taskname ||")'")*/
  call submit('*')
  call check_job xmi_taskname hj

  return

get_pw: 
/* Returns a string with the MVP user password from RAKF */
  call debug "get_pw: Opening 'SYS1.SECURE.CNTL(USERS)'"
  "ALLOC FI(USERS) DA('SYS1.SECURE.CNTL(USERS)') SHR "
  "EXECIO * DISKR USERS (FINIS STEM users."
  if rc > 0 then do
      call error "Error reading SYS1.SECURE.CNTL(USERS):" rc
      "FREE F(USERS)"
      exit 8
  end
  "FREE F(USERS)"

  PW = ""

  call debug "get_pw: Searching for MVP user"
  do k=1 to USERS.0
    if left(users.k, 4) == "MVP " then do
      call debug "get_pw: MVP User found returning"
      /* returns the password for the MVP user */
      PW = substr(USERS.k,19,8)
      leave
    end
  end

  if PW="" then do
    call debug "get_pw: MVP User not found!"
  end

  return PW

debug: procedure
  /* Prints a debug message to TSO and console if debugging is enabled
     with the argument -d/--debug
  */
  parse arg string
  if getg(debug_on) then do
    say "MVP000I - DEBUG -" string
    call wto "MVP000I - DEBUG -" string
  end
  return

error:
  /* prints MVP error message to TSO and mvs console */
  parse arg string
  say "MVP999E - ERROR -" string
  call wto "MVP999E - ERROR -" string
  return

usage:
  /* prints MVP usage */
  say "MVS/CE Package manager (MVP)"
  say "Usage:"
  say "  RX MVP command [options]"
  say "Example:"
  say "  RX MVP LIST"
  say "  RX MVP INFO PACKAGE --debug"
  say "  RX MVP UPDATE -d"
  say "  RX MVP INSTALL PACKAGE"
  say ""
  say "Where command is one of:"
  say "  list      - list packages based on package names"
  say "  search    - search in package descriptions"
  say "  show/info - show package details"
  say "  install   - install packages"
  say "  update    - update list of available packages"
  say ""
  say "Options:"
  say "  -d/--debug - Show debugging output to TSO and MVS console"
  say "  --installed - When using list show only installed packages"
  return

check_depends: procedure
  /* recursive function to get package dependencies */
  parse arg depends_check
  depend_text = ''

  /* say "checking dependencies for" depends_check */

  "ALLOC FI(DESC) DA('MVP.PACKAGES("depends_check")') SHR "
  "EXECIO * DISKR DESC (FINIS STEM desc."

  if rc > 0 then do
      call error "Error reading MVP.PACKAGES("depends_check"):" rc
      "FREE F(DESC)"
      exit 8
  end
  "FREE F(DESC)"

  do i=1 to desc.0
    if pos("Depends:", desc.i) > 0 then do
      parse var desc.i . Depends
      /* say "dependencies for" depends_check ":" Depends */
      leave
    end
  end

  call debug "check_depends: all dependents" Depends

  depend_text = Depends

  do while length(Depends) > 0
    parse var Depends dependent Depends
    /* say "Current dependency" dependent */
    depend_text = depend_text check_depends(dependent)
  end
  return depend_text
