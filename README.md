# zxmdr - mdr.pl
__ZX Microdrive cartridge .MDR file cruncher__

A few years ago I've been in contact with Marcos Cruz and he gave me the idea to write a .mdr manipulator since that format is well known.
Within a few days I managed to code a Perl script that simply examines and manipulates anything within a .mdr file.
The script also is able to read or write .tap files, so it can be used as a good mdr-to-tap or tap-to-mdr utility, so it can "dump" any file from cartridge to host file and vice versa.
Another nice feature is the "label" renaming of the cartridge: the cartridge label name is written in the header of each sector.

With this script, you are able to :

- get the catalog and all available info (starting address, length, start-line, used sectors, damaged or free sectors, etc. ).
- copy, rename, erase a file within a cartridge.
- remove "autorun" LINE from a program
- put/get any file from/to a host file to/from a cartridge 
- port files to/from  .tap  from/to  .mdr

Examples.

$> perl mdr.pl  m8.mdr

    Header: "M4        "
    
    
    Type  Bytes Filename     Addr. Actual   Vars   Line
    ____ ______ ___________ ______ ______ ______ ______
    
    Prnt  11776 Assembly.f  65 6E 20 23 20 31 30 31 20 0D 28 20 5A 38 30 20
    Prog    248 Forth1413    24451    257   +248     20
    Prog    275 Forth1413_   23813    284   +275
    Code   7863 forth1413d   25600   7872
    
    ____ ______ ___________ ___________________________________________
    
          20189 total used
              0 total bad
         108544 total free

$> perl mdr.pl m8  put=text.prnt  dump=hostfile.txt 

$> perl mdr.pl m8  label=NEW_LABEL

$> perl mdr.pl m8  noautorun=run

$> perl mdr.pl m8  erase=run

$> perl mdr.pl m8  rename=sys64 to=SYS64

$> perl mdr.pl m8  -p tape=vforth13.tap



Since it is a work-in-progress, any feedback is welcome.

The script accepts a "verbose" switch that can be used to completely dump the content of a cartridge. This feature is useful to verify the cartridge integrity of any sector. I used it to hack the checksum of bad-sectors to obtain a fully 254-sectors length cartridge: It is well known that in reality at least one sector is unusable due to the tape-juction and the peculiar way the ZX Microdrive FORMAT "M" command works. 


