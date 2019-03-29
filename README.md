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

  Header: "M8        "
  Type  Bytes Filename     Addr. Actual   Vars   Line
  ____ ______ ___________ ______ ______ ______ ______
  Prog   2295 Blk2Tap      23824   2304  +2295
  Prog   9254 Chomp.udg    23824   9263  +8721
  Prog   8688 Chomp2       23813   8697  +8688
  Prog    365 DoubleChr    23813    374   +365
  Prnt   1300 HelloWorld  48 65 6C 6C 6F 20 57 6F 72 6C 64 21 20 48 65 6C
  Prog   7333 Mannesman    23813   7342  +4706
  Prog    343 Primality    23813    352   +343
  Prog   2164 RandAcces1   24440   2173  +2164
  Prog   2295 RandAccess   24440   2304  +2295
  Prog    222 SQRdensity   23813    231   +222
  Prog    765 Stripes      23813    774   +765
  Prog    467 Tap2Blk      24440    476   +467
  Prog     64 TestCOM      23824     73    +64
  Bad     512             254
  ____ ______ ___________ ___________________________________________
 
       35663 total used
         512 total bad
       90624 total free

$> perl mdr.pl m8  put=text.prnt  dump=hostfile.txt 

$> perl mdr.pl m8  label=NEW_LABEL

$> perl mdr.pl m8  noautorun=run

$> perl mdr.pl m8  erase=run

$> perl mdr.pl m8  rename=sys64 to=SYS64

$> perl mdr.pl m8  -p tape=vforth13.tap



Since it is a work-in-progress, any feedback is welcome.

The script accepts a "verbose" switch that can be used to completely dump the content of a cartridge. This feature is useful to verify the cartridge integrity of any sector. I used it to hack the checksum of bad-sectors to obtain a fully 254-sectors length cartridge: It is well known that in reality at least one sector is unusable due to the tape-juction and the peculiar way the ZX Microdrive FORMAT "M" command works. 


