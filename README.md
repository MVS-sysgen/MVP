# MVS/CE Package Manager - MVP

The package manager for MVS

## Quick Install

:warning: MVP might already be installed, try running `RX MVP` in TSO first :warning:

If you just want to get started and using it with a default install of MVS/CE
sysgen from https://github.com/MVS-sysgen/sysgen/releases, git clone this repo
to your MVSCE folder, then from the MVSCE folder run:

- `cd MVP`
- `python3 MVP INSTALL_MVP`

All steps must complete with a condition code of `00000` otherwise something
went wrong.

Once installed you can begin using it on MVS/CE with: `RX MVP` for help.

UFSD, FTPD, HTTPD and MVSMF are no longer MVP packages. On MVS/CE the
sysgen installs them with SMP from the mvslovers releases
(https://github.com/mvslovers), together with BREXX/370, RAKF and SDSF;
see the MVS/CE README for the installed levels and how to upgrade them.

:warning: if you're installing this on another system other than MVS/CE you must install Jay
Moseley's IEFACTRT-4 usermod JLM0001 availble here: http://www.jaymoseley.com/hercules/iefactrt/exit04.htm
and in the `extras` folder.

## What is this?

There is a lot of software available for MVS 3.8j but not everyone needs all
of it at the same time. I wanted to build a system where people could get a
barebones install of MVS 3.8j then install the software they wanted with ease.

To make software instalation easier MVP takes its inspiration from the debian
tool `apt`.

With MVP you can:
- List available packages
- List installed packages
- Install packages
- Search packages
- Show information about specific packages
- Update package listing

MVP will handle dependencies for you, so if one package relies on another MVP
will install them all in the correct order.

## Example

If you wanted to install Rob Prins' RPF editor you would first
search for any packages with RPF then install the package you want:

```
 READY
rx mvp search rpf
 Searching...

 RPF 1.9.0
   Rob Prins' Programming Facility

 READY
 rx mvp install RPF
```

## Command Syntax

MVP can take multiple arguments

- **UPDATE** this argument updates the software descriptions and cache file
- **INSTALL** this argument will install a package, e.g. `RX MVP INSTALL REVIEW`
- **SHOW**/**INFO** this argument will give you information about a specific package
- **SEARCH** this argument will search package name and descriptions for the given search term
- **LIST** this argument lists all available packages
  - **--installed** this sub-argument will list only installed packages

Two option arguments exist:
- **-h** will list the help and exit
- **-d**/**--debug** will enable verbose debugging

Here are some examples:
- `RX MVP LIST` List all available packages
- `RX MVP INFO RPF --debug` Show information about the RPF package and enable debugging information
- `RX MVP UPDATE -d` Update the package cache and descriptions and enable debugging
- `RX MVP INSTALL TSOAUTHC` Install the TSOAUTHC package

## How does it work?

MVP is made up of multiple components:
- `MVP` a python script that takes either `INSTALL` or `UPDATE` argument and submits the appropriate jobs to the ASCII or EBCDIC reader
  - `MVP.ini` the config file for `MVP` see below for details
- `cache` The database of: software, the type, and version (installed to `MVP.PACKAGES(CACHE)`)
- `desc/*` Descriptions of each package (installed to `MVP.PACKAGES(*)`)
- `MVP.rexx` The MVP rexx script which handles all the install and request processing (installed to `SYS2.EXEC(MVP)`)
  - `MVP.parmlib` A config file for for MVP.rexx (installed to `SYS2.PARMLIB(MVP0001)`)
  - `MVP.MVPDB` sequential dataset of installed packages

From a high level, when you type a command, for example `RX MVP INSTALL MACLIBS`, the MVP rexx script does the following:

1) Gathers dependencies
2) Reads the Master Trace Table (MTT) and gets the last run job
3) Issues `ADDRESS COMMAND SH ./MVP/MVP INSTALL MACLIB` which executes the MVP python script
4) The MVP python script submits a job with the package to install
5) The MVP rexx script reads the MTT waiting until the submitted job is completed
6) Checks return codes for each step
7) Updates the list of installed packages
8) Exits

So basically: `RX MVP INSTALL MACLIBS` runs the shell command `./MVP/MVP INSTALL MACLIBS` which submits the jobstream `packages/MACLIBS`.
It's a little more complicated for XMI files, see the **Packages** section below.

:warning: You should never use the `MVP` python script by itself

## Batch Job Usage

A JCL procedure is included with MVP located at `SYS2.PROCLIB(MVP)` and can be used to install, list, search, packages.

Example 1: Install RPF

```
//MVPRFE JOB (SYSGEN),'MVP INSTALL',CLASS=A,MSGCLASS=A,
//       MSGLEVEL=(1,1),NOTIFY=IBMUSER
//* This JCL installs RPF using MVP
//MVPINST EXEC MVP,INSTALL='RPF' 
```

Example 2: Install RPF with debugging enabled

```
//MVPRFE JOB (SYSGEN),'MVP INSTALL',CLASS=A,MSGCLASS=A,
//       MSGLEVEL=(1,1),NOTIFY=IBMUSER
//* This JCL installs RPF using MVP
//MVPINST EXEC MVP,INSTALL='RPF -D' 
```

Example 3: List available packages

```
//MVPTEST JOB (SYSGEN),'MVP INSTALL',CLASS=A,MSGCLASS=H,
//       MSGLEVEL=(1,1),NOTIFY=IBMUSER                  
//* This JCL LISTS available packages                   
//MVPINST EXEC MVP,ACTION='LIST'                         
```

## Security

To run MVP the user must have read access to the BRXMTTAUTH FACILITY class in
RAKF. If you're in the ADMIN group on MVS/CE you already have access.

## MVP.ini

The MVP config file is made up of one section (DEFAULT) and contains the following
- `hercules_ip` ip address of MVS/CE
- `ascii_reader` the port for the 3505 ASCII reader
- `ebcdic_reader` the port for the 3505 EBCDIC reader
- `user` the username used to submit jobs remotely
- `password` the password used to submit jobs remotely :warning: this setting will not exist until you install MVP, if you need to add it (for example your MVP folder becomes corrupt but not MVS/CE) get the password for the MVP user from `SYS1.SECURE.CNTL(USERS)`
  - When running MVP with `INSTALL_MVP` a random password will be generated and the MVP user will be added to RAKF
- `desc` the folder where package descriptions reside
- `packages` the folder where packages reside
- `cache` the filename for the cache file

Here's the default ini file:

```ini
[DEFAULT]
hercules_ip = 127.0.0.1
ascii_reader = 3505
ebcdic_reader = 3506
user = MVP
desc = desc
packages = packages
cache = cache
```

# Packages Details for Maintainers

## `cache` File

The cache file contains software packages available to be installed. Each line
is made up of a:
- Package name
- Type (JCL or XMI)
- Version

For example: `DUMMY JCL 1.0`

The package name, desription file, package in the `package` folder (and job name if type is JCL) **must** all be the same.

## Description File

An ASCII file named the same as the package in the `cache` file located in the `desc` folder which contains:
- Package: package name
- Version: version number
- Maintainer: Name [email optional] of maintainer
- Depends: Package dependencies
- Homepage: URL to software homepage
- Description: The first line is the short description use do display in search, the rest is the detailed description

For example `desc/RPF` contains:
```
Package: RPF
Version: 1.9.0
Maintainer: Soldier of FORTRAN <mainframed767@gmail.com>
Depends:
Homepage: http://www.prince-webdesign.nl/index.php/software/
Description: Rob Prins' Programming Facility

 TSO full screen Editor and multi-utility programmer facility called RPF. RPF
 runs under native TSO (or as a TSO command processor), even if ISPF is
 running too, and RPF has an ISPF-like editor, browser, and quite a few other
 utilities.
```

## JCL vs XMI

MVP Packages, stored in `packages/` come in two flavors: **JCL** and **XMI**.

### JCL Packages

The JCL files are easy, they're just a job stream that MVP submits to JES
through the ASCII socket reader.

An example of a job stream is the DUMMY package which is listed in the `cache`
file as `DUMMY JCL 1.0`. That JCL there following DUMMY tells both the MVP
python script and the MVP rexx script that DUMMY is just a JCL file. The
contents of the DUMMY package is:

```JCL
//DUMMY JOB (TSO),'DUMMY',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
//DUMMY1 EXEC PGM=IEFBR14
```

More complicated jobstreams like the `KLINGON` package contain many more steps
but its still just a job stream in ASCII. MVP supports as many steps as JES2
can handle. Once completed MVP will check the condition code for reach step to
check that it completes with a CC of `00000`.

Note: Notice the DUMMY job does not have a `USER=` or a `PASSWORD=` field in
the jobcard. The MVP python script will read the jobcard for any job and either
change the USER/PASSWORD fields to ther user and password declared in the
`MVP.ini` file or it will add them to the job.

### XMI Packages

XMI pacakges are a little more complicated. They consist of tasks (either
JCL or REXX scripts), some other types of binary files (most likely XMI files)
all packaged in to one XMI file. This XMI file is then wrapped in an EBCDIC
job stream which unloads the XMI file to `MVP.WORK`, a temporary dataset used
by MVP.

If we use the example of the `IMON370` package in the `packages` folder, it
looks like this:

```
+-------------+
|  MVP.WORK   | XMI File
+-------------+----------------+
|                              |
|     #001JCL                  |
|   +------------+             |
|   |  I370LOAD  | XMI File    |
|   +------------+---------+   |
|   |    GLSPARSE          |   |
|   |    IM                |   |
|   |    IMSWAP            |   |
|   |    IMVTOCRD          |   |
|   |    IM370GAT          |   |
|   |    IM370GLS          |   |
|   |    IM470SPY          |   |
|   +----------------------+   |
|                              |
+------------------------------+
```

The linux MVP python script, which is called with `MVP INSTALL IMON370` will
wrap this XMI file in an EBCDIC job stream with a job name the same as the
package name, in this case IMON370.


```
+-----------------------+
|  EBCDIC Job Stream    |
+-----------------------+------------+
|                                    |
|  +-------------+                   |
|  |  MVP.WORK   | XMI File          |
|  +-------------+----------------+  |
|  |                              |  |
|  |     #001JCL                  |  |
|  |   +------------+             |  |
|  |   |  I370LOAD  | XMI File    |  |
|  |   +------------+---------+   |  |
|  |   |    GLSPARSE          |   |  |
|  |   |    IM                |   |  |
|  |   |    IMSWAP            |   |  |
|  |   |    IMVTOCRD          |   |  |
|  |   |    IM370GAT          |   |  |
|  |   |    IM370GLS          |   |  |
|  |   |    IM470SPY          |   |  |
|  |   +----------------------+   |  |
|  |                              |  |
|  +------------------------------+  |
|                                    |
+------------------------------------+
```


This standard JCL uses `RECV370` to unload the XMI file to `MVP.WORK`, which
the MVP rexx scripts will look for.

Here is what that JCL looks like:

```jcl
//IMON370 JOB (TSO),
//             'MVP INSTALL',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),USER=MVP,PASSWORD=MVP
//* CLEAN UP
//CLEANUP EXEC PGM=IDCAMS
//SYSIN    DD *
  DELETE MVP.WORK SCRATCH PURGE
  SET MAXCC=0
  SET LASTCC=0
//SYSPRINT DD SYSOUT=*
//* UNLOAD XMIT TO MVP.WORK
//RECV370 EXEC PGM=RECV370
//STEPLIB  DD  DISP=SHR,DSN=SYSC.LINKLIB
//RECVLOG  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DUMMY
//* Work temp dataset
//SYSUT1   DD  DSN=&&SYSUT1,
//             UNIT=VIO,
//             SPACE=(CYL,(5,1)),
//             DISP=(NEW,DELETE,DELETE)
//* Output dataset
//SYSUT2   DD  DSN=MVP.WORK,
//             UNIT=3390,VOL=SER=PUB001,
//             SPACE=(CYL,(10,10,20),RLSE),
//             DISP=(NEW,CATLG,DELETE)
//XMITIN   DD DATA,DLM=$$
[inline XMI file]
$$
```

After its unloaded the dataset `MVP.WORK` will contain:

```
 +-------------+
 |  MVP.WORK   | PDS
 +-------------+-----+
 |  #001JCL          |
 |  I370LOAD         |
 +-------------------+

```

Once that job is completed the MVP rexx script searches the contents of
`MVP.WORK` for any files named `#nnnJCL` or `#nnnREX` where **nnn** is a zero
padded number between 001-999. The MVP rexx script will then begin submitting
or executing them in numbered order. If there are any conflicts in numbering
the JCL runs first before the REXX.

:warning: **THE JCL MEMBERS MUST HAVE THE SAME JOB NAME AS THEIR FILE NAME OR THE INSTALL WILL FAIL** :warning:


## Appendix A - Software Packaging

The `package_release.py` script in the `extras` folders was designed to make
packaging XMI file based packages easier.

Before we get started you will need python3 and the `xmi-reader` package
(`pip3 install xmi-reader`). No copy of MVS/CE or Hercules is required --
the script builds the package XMI entirely in Python.

`package_release.py` takes multiple arguments:

- `--xmi-files XMI_FILES [XMI_FILES ...]` one (or more) XMI file(s) to include
- `--task-files TASK_FILES [TASK_FILES ...]` one (or more) JCL/REXX file named `#nnnJCL` or `#nnnREX` where **nnn** is a zero padded number between 001-999. The MVP rexx script will then begin submitting or executing them in numbered order. If there are any conflicts in numbering the JCL runs first before the REXX.
- `--name` Name of the output file
- `--dsn` Dataset name to embed in the XMI metadata (defaults to the uppercased output file name)
- `--from-user` NETDATA origin userid embedded in the XMI (defaults to `MVP`)
- `--from-node` NETDATA origin node name embedded in the XMI (defaults to `MVP`)



If we take a look at the example IMON370, as released from https://www.prycroft6.com.au/vs2sw/index.html#imon370
it comes as an XMI named: `i370load.xmi`. To package up for MVP we create the JCL file below:

```jcl
//#001JCL JOB (TSO),
//             'Recieve XMI',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1)
//RECV370 EXEC PGM=RECV370
//STEPLIB  DD  DISP=SHR,DSN=SYSC.LINKLIB
//XMITIN   DD  DSN=MVP.WORK(I370LOAD),DISP=SHR
//RECVLOG  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DUMMY
//* Work temp dataset
//SYSUT1   DD  DSN=&&SYSUT1,
//             UNIT=VIO,
//             SPACE=(CYL,(5,1)),
//             DISP=(NEW,DELETE,DELETE)
//* Output dataset
//SYSUT2   DD  DSN=SYSGEN.IMON370.LOAD,
//             UNIT=SYSALLDA,VOL=SER=PUB001,
//             SPACE=(CYL,(15,2,20),RLSE),
//             DISP=(NEW,CATLG,DELETE)
//*
//*  THIS JOB COPIES THE IMON370 TSO PROGRAMS INTO SYS2.CMDLIB
//*
//IMONCOPY EXEC PGM=IEBCOPY
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DSN=SYSGEN.IMON370.LOAD,DISP=SHR
//SYSUT2   DD  DSN=SYS2.CMDLIB,DISP=SHR
//SYSIN    DD  *
  COPY INDD=((SYSUT1,R)),OUTDD=SYSUT2
  COPY INDD=((SYSUT2,R)),OUTDD=SYSUT2
```

Notice the job name is `#001JCL`. We save this JCL as the file `#001JCL.jcl` and place it in the same
folder as `i370load.xmi`. From that folder we run the package release tool:

```
python3 /path/to/MVP/extras/package_release.py\
 --xmi-files i370load.xmi --task-files \#001JCL.jcl\
 --name /path/to/MVP/packages/IMON370
```

This will generate a file with the following structure:

```
+-----------+
|  IMON370  |  XMI file
+-----------+------------------------+
|                                    |
|  +-------------+                   |
|  |  MVP.WORK   |                   |
|  +-------------+----------------+  |
|  |                              |  |
|  |     #001JCL                  |  |
|  |   +------------+             |  |
|  |   |  I370LOAD  | XMI File    |  |
|  |   +------------+---------+   |  |
|  |   |    GLSPARSE          |   |  |
|  |   |    IM                |   |  |
|  |   |    IMSWAP            |   |  |
|  |   |    IMVTOCRD          |   |  |
|  |   |    IM370GAT          |   |  |
|  |   |    IM370GLS          |   |  |
|  |   |    IM470SPY          |   |  |
|  |   +----------------------+   |  |
|  |                              |  |
|  +------------------------------+  |
|                                    |
+------------------------------------+
```

A log file called `build.log` contains the date/time the build was generated
as well as the arguments passed to the script.

When complete just update the `cache` file with `IMON370 XMI 1.0` and add
the description `desc/IMON370` file and you've created your first MVP release!

## Appendix B

Some program require changes to hercules config files. With that in mind MVP has
an action called `WRITE` which takes a file name (the file name and path **must**
be lowercase) and the string to write, this entire argument must be surrounded
by quotation marks. The file path begins one directory up from the MVP folder and
the file will either be created or appended to, if the folder does not exist
this will end in error. All strings placed in the file will be uppercase.

for example: `./MVP WRITE "conf/local/dummy.cnf This is an example"` will create
or append to the file `../conf/local/dummy.cnf` the following: `THIS IS AN EXAMPLE`.