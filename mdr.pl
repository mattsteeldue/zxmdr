#!/usr/bin/perl

=pod

  mdr.pl

  rev.2022.12.01

  \ by Matteo Vitturi, 2016-2022

  \ Copying, modifying and distributing this software is allowed 
  \ provided this copyright notice is kept.  
  \ This work is available as-is with no whatsoever warranty.

  This is a script that allows you to manipulate .MDR cartrige files.
  This script needs a .MDR filename that is read entirely at start.
  Then some operations will be performed based on options given.
  
  Usage is:
  
    perl mdr.pl [switch] <cartridge> [key=value ...]
    
  where [switch] can be
  
     -h    for help
     -l    show catalog (optional)
     -f    used with -l switch: Show free sectors.
     -b    used with -l switch: Hide bad sectors.
     -s    with -l show sectors used instead of file details
     -r    removes autorun LINE from programs in cartridge.
     -v    verbose, show much more details at start
     -x    fix bad sectors
     -c    create empty if not exists

  <cartridge> is the filename of a .MDR file.
  Normally operation will be done "in-place" unless an out option is specified.
  [key=value] available are listed here below:
  
  * Cartridge only options:
    
    rename=name     rename "name". New name is given via "to" option
    copy=name       copy "name". New name given via "to" option
    to=newname      destination "name" in .MDR.
    erase=name      erase "name" from .MDR
    label=name      change label of cartridge.
    noautorun=name  remove autorun of a single file "name" inside .MDR
    autorun=name    set autorun of a single file "name" inside .MDR
    line=number     used with autorun option to specify run
    
  * Host file options:
    
    out=output.MDR  create a new cartridge file, original is unchanged.

  * From-to cartridge options:
  
    get=name        gets a file from .MDR and write it to "host" file
    dump=name       same as get, but output is just a dump file
    dump32=name     same as dump, but adds/takes a new-line every 32 characters.
    dumpblock=name  same as dump32, but adds a "Screen#" header for each 16 rows.
    text=name       same as dump, but converts LF to CR, or CR+LF to CR.
    put=name        reads "host" files and creates files to .MDR

  Examples:

    -l xyz.mdr noautorun=run                     remove autorun from "run"
    -l xyz.mdr erase=run                         erase "run" file
    -l xyz.mdr rename=abc to=ABC                 rename "abc" to "ABC"
    -l xyz.mdr -p tape=afile.tap                 put afile.tap content  
    -l xyz.mdr put=f1 host=file1.bin             copy host file1.bin to mdr "f1"


  From Z80 tecnical documentation
  
    .MDR FILES:
    -----------

    The emulator uses a cartridge file format identical to the 'Microdrive
    File' format of Carlo Delhez' Spectrum emulator Spectator for the QL,
    who devised the format.  This format is now also supported by XZX of
    Des Harriot.  The following information is adapted from Carlo's
    documentation.  It can also be found in the 'Spectrum Microdrive Book',
    by Ian Logan (co-writer of the excellent 'Complete Spectrum ROM
    Disassembly').

    A cartridge file contains 254 'sectors' of 543 bytes each, and a final
    byte flag which is non-zero is the cartridge is write protected, so the
    total length is 137923 bytes.  On the cartridge tape, after a GAP of
    some time the Interface I writes 10 zeros and 2 FF bytes (the
    preamble), and then a fifteen byte header-block-with-checksum.  After
    another GAP, it writes a preamble again, with a 15-byte record-
    descriptor-with-checksum (which has a structure very much like the
    header block), immediately followed by the data block of 512 bytes, and
    a final checksum of those 512 bytes.  The preamble is used by the
    Interface I hardware to synchronise, and is not explicitly used by the
    software.  The preamble is not saved to the microdrive file:

    offset length name    contents

      0      1   HDFLAG   Value 1, to indicate header block
      1      1   HDNUMB   sector number (values 254 down to 1)
      2      2            not used
      4     10   HDNAME   microdrive cartridge name (blank padded)
     14      1   HDCHK    header checksum (of first 14 bytes)

     15      1   RECFLG   - bit 0: always 0 to indicate record block
                          - bit 1: set for the EOF block
                          - bit 2: reset for a PRINT file
                          - bits 3-7: not used (value 0)
     16      1   RECNUM   data block sequence number (value starts at 0)
     17      2   RECLEN   data block length (<=512, LSB first)
     19     10   RECNAM   filename (blank padded)
     29      1   DESCHK   record descriptor checksum (of previous 14 bytes)
     30    512            data block
    542      1   DCHK     data block checksum (of all 512 bytes of data
                           block, even when not all bytes are used)
    ---------
    254 times


    (Actually, this information is 'transparent' to the emulator.  All it
    does is store 2 times 254 blocks in the .MDR file as it is OUTed,
    alternatingly of length 15 and 528 bytes.  The emulator does check
    checksums, see below; the other fields are dealt with by the emulated
    Interface I software.)

    A used record block is either an EOF block (bit 1 of RECFLG is 1) or
    contains 512 bytes of data (RECLEN=512, i.e.  bit 1 of MSB is 1).  An
    empty record block has a zero in bit 1 of RECFLG and also RECLEN=0.  An
    unusable block (as determined by the FORMAT command) is an EOF block
    with RECLEN=0.

    The three checksums are calculated by adding all the bytes together
    modulo 255; this will never produce a checksum of 255.  Possibly, this
    is the value that is read by the Interface I if there's no or bad data
    on the tape.

    In normal operation, all first-fifteen-byte blocks of each header or
    record block will have the right checksum.  If the checksum is not
    right, the block will be treated as a GAP.  For instance, if you type
    OUT 239,0 on a normal Spectrum with interface I, the microdrive motor
    starts running and the cartridge will be erased completely in 7
    seconds. CAT 1 will respond with 'microdrive not ready'.  Try it on the
    emulator...


    .TAP FILES:
    -----------

    The .TAP files contain blocks of tape-saved data.  All blocks start
    with two bytes specifying how many bytes will follow (not counting the
    two length bytes).  Then raw tape data follows, including the flag and
    checksum bytes.  The checksum is the bitwise XOR of all bytes including
    the flag byte.  For example, when you execute the line SAVE "ROM" CODE
    0,2 this will result:


             |------ Spectrum-generated data -------|       |---------|

       13 00 00 03 52 4f 4d 7x20 02 00 00 00 00 80 f1 04 00 ff f3 af a3

       ^^^^^...... first block is 19 bytes (17 bytes+flag+checksum)
             ^^... flag byte (A reg, 00 for headers, ff for data blocks)
                ^^ first byte of header, indicating a code block

       file name ..^^^^^^^^^^^^^
       header info ..............^^^^^^^^^^^^^^^^^
       checksum of header .........................^^
       length of second block ........................^^^^^
       flag byte ............................................^^
       first two bytes of rom .................................^^^^^
       checksum (checkbittoggle would be a better name!).............^^


    The emulator will always start reading bytes at the beginning of a
    block.  If less bytes are loaded than are available, the other bytes
    are skipped, and the last byte loaded is used as checksum.  If more
    bytes are asked for than exist in the block, the loading routine will
    terminate with the usual tape-loading-error flags set, leaving the
    error handling to the calling Z80 program.

    Note that it is possible to join .TAP files by simply stringing them
    together, for example COPY /B FILE1.TAP + FILE2.TAP ALL.TAP

    For completeness, I'll include the structure of a tape header.  A
    header always consists of 17 bytes:

        Byte    Length  Description
        0       1       Type (0,1,2 or 3)
        1       10      Filename (padded with blanks)
        11      2       Length of data block
        13      2       Parameter 1
        15      2       Parameter 2

    The type is 0,1,2 or 3 for a Program, Number array, Character array or
    Code file.  A screen$ file is regarded as a Code file with start
    address 16384 and length 6912 decimal.  If the file is a Program file,
    parameter 1 holds the autostart line number (or a number >=32768 if no
    LINE parameter was given) and parameter 2 holds the start of the
    variable area relative to the start of the program.  If it's a Code
    file, parameter 1 holds the start of the code block when saved, and
    parameter 2 holds 32768.  For data files finally, the byte at position
    14 decimal holds the variable name.

=cut

use strict ;
use warnings ;

umask 011 ;

# ____________________________________________________________________________

my @object_list = () ;
my %option = (
    shorthelp        => 0,
    help             => 0,
    showcat          => 0,
    showdeleted      => 0,
    showbad          => 1,
    sectors          => 0,
    noautorun        => '',
    autorun          => '',
    line             => 0,
    get              => '',
    put              => '',
    verbose          => 0,

    fix              => 0,
    create           => 0,
    cartridge        => '',
    tape             => '',
    dump             => '',
    dump32           => '',
    dumpblock        => '',
    text             => '',

    out              => '',
    rename           => '',
    to               => '',
    copy             => '',
    erase            => '',
    unerase          => '',
    label            => '',
    ) ;

for my $switch ( @ARGV ) {
    if ( $switch =~ /^-\w+$/ ) {
        my @switch = split(//, $switch) ;
        for my $ch ( @switch ) {
            $option{ help            } = 1    if $ch =~ /h/ ;
            $option{ showcat         } = 1    if $ch =~ /l/ ;
            $option{ showdeleted     } = 1    if $ch =~ /f/ ;
            $option{ showbad         } = 0    if $ch =~ /b/ ;
            $option{ sectors         } = 1    if $ch =~ /s/ ;
            $option{ noautorun       } = '*'  if $ch =~ /r/ ;
            $option{ get             } = '*'  if $ch =~ /g/ ;
            $option{ put             } = '*'  if $ch =~ /p/ ;
            $option{ verbose         } = 1    if $ch =~ /v/ ;
            $option{ fix             } = 1    if $ch =~ /x/ ;
            $option{ create          } = 1    if $ch =~ /c/ ;
            $option{ shorthelp       } = 1    unless $ch =~ /[-hlfbsrgpvxc]/ ; # catch-all
        }
    }
    # switch key=[list]
    elsif ( $switch =~ m/^(\w+)=\[(\S+)\]\s*$/ ) {
        my ( $key, $value ) = ( $1, $2 ) ; #'''
        my @ary = split /[,;]/, $value ;
        $option{ $key } = \@ary ;
    }
    # switch key="value"
    elsif ( $switch =~ m/^(\w+)=(\S+)\s*$/ ) {
        my ( $key, $value ) = ( $1, $2 ) ; #'''
        $option{ $key } = $value ;
    }
    # switch key="value"
    elsif ( $switch =~ m/^(\w+)=(.+)\s*$/ ) {
        my ( $key, $value ) = ( $1, $2 ) ; #'''
        $option{ $key } = $value ;
    }
    # the first of the remaining parameters is intended as cartridge filename
    else {
        if ( $option{ cartridge } ) {
            push @object_list, $switch ;
        }
        else {
            $option{ cartridge } = $switch ;
        }
    }
}

# ____________________________________________________________________________

$option{ dump32 } = $option{ dumpblock } if $option{ dumpblock } ;
$option{ dump } = $option{ dump32 } if $option{ dump32 } ;

# ____________________________________________________________________________

if ( $option{ text2tap } && $option{ tape } ) {
    text_to_tape( $option{ text2tap }, $option{ tape } ) ;
}

# ____________________________________________________________________________

if ( $option{ cartridge } && scalar(@ARGV) == 1 ) {
    $option{ showcat } = 1 ;
}

# ____________________________________________________________________________

if ( $option{ cartridge } && $option{ cartridge } !~ m/\.mdr$/i ) {
    $option{ cartridge } .= '.mdr' ;
}

# ____________________________________________________________________________

if ( $option{ shorthelp } or ( !$option{ cartridge } and not $option{ help } ) ) {
    print qq(
  Syntax: perl mdr.pl [switch] <cartridge> [key=value ...]
  Help:   perl mdr.pl -h   
    ) ;
   exit ;
}

# ____________________________________________________________________________


if ( $option{ help }) {
    print qq(

  mdr.pl

  This is a script that allows you to manipulate .MDR cartrige files.
  This script needs a .MDR filename that is read entirely at start.
  Then some operations will be performed based on options given.
  
  Usage is:
  
    perl mdr.pl [switch] <cartridge> [key=value ...]
    
  where [switch] can be
  
     -h    for help
     -l    show catalog (optional)
     -f    used with -l switch: Show free sectors.
     -b    used with -l switch: Hide bad sectors.
     -s    with -l show sectors used instead of file details
     -r    removes autorun LINE from programs in cartridge.
     -v    verbose, show much more details at start
     -x    fix bad sectors
     -c    create empty if not exists

  <cartridge> is the filename of a .MDR file.
  Normally operation will be done "in-place" unless an out option is specified.
  [key=value] available are listed here below
  
  * Cartridge only options:
    
    rename=name     rename "name". New name is given via "to" option
    copy=name       copy "name". New name given via "to" option
    to=newname      destination "name" in .MDR.
    erase=name      erase "name" from .MDR
    label=name      change label of cartridge.
    noautorun=name  remove autorun of a single file "name" inside .MDR
    autorun=name    set autorun of a single file "name" inside .MDR
    line=number     used with autorun option to specify run
    
  * Host file options:
    
    out=output.MDR  create a new cartridge file, original is unchanged.

  * From-to cartridge options:
  
    get=name        gets a file from .MDR and write it to "host" file
    dump=name       same as get, but output is just a dump file
    dump32=name     same as dump, but adds/takes a new-line every 32 characters.
    dumpblock=name  same as dump32, but adds a "Screen#" header for each 16 rows.
    text=name       same as dump, but converts LF to CR, or CR+LF to CR.
    put=name        reads "host" files and creates files to .MDR

  Examples:

    -l xyz.mdr noautorun=run                     remove autorun from "run"
    -l xyz.mdr erase=run                         erase "run" file
    -l xyz.mdr rename=abc to=ABC                 rename "abc" to "ABC"
    -l xyz.mdr -p tape=afile.tap                 put afile.tap content  
    -l xyz.mdr put=f1 host=file1.bin             copy host file1.bin to mdr "f1"

    \n) ;
    
    exit ;
}

# ____________________________________________________________________________

# mdr style checksum
sub checksum {
    my $string = shift ;
    my $length = shift ;
    my $sum = 0 ;
    for my $ch ( unpack("C$length", $string) ) {
        $sum += $ch ;
        $sum %= 255 ;
    }
    return $sum  ;
}

# ____________________________________________________________________________

# tape style checksum
sub checkbittoggle {
    my $string = shift ;
    my $length = shift ;
    my $sum = 0 ;
    for my $ch ( unpack("C$length", $string) ) {
        $sum ^= $ch ;
    }
    return $sum  ;
}

# ____________________________________________________________________________

my $whole = '' ;
my $record = { } ;
my $readonly = 0 ;
my $unformatted = 0;
my $free = { } ;
my %cart_label = () ;
my %cart_catalog = () ;
my %cart_cat_sec = () ;
my %cart_unusable = () ;
my $master_label = '' ;
my $master_count = 0 ;
my $SSIZE = 543 ; # constant
my $realsize = 0;
my $max_hdnumb = 0 ;

# ____________________________________________________________________________

# find first free sector.
my $current_head = 0 ;
sub first_free {
    my $pass = 0 ;
    my $i = $current_head ;
    while ( 0 == $pass || $i != $current_head ) {
        my $key = sprintf( "%03d", $i ) ;
        if ($record->{ $key }->{ unusable } ) {
            $i -= 1 ;
        } else {
            if ( $record->{ $key }->{ empty }  ) {
                $current_head = $i - 2; # motor latency
                $current_head += $max_hdnumb if $current_head < 1 ;
                return $key ;
            }
            $i -= 1 ;
            if ( $i < 1 ) {
                $pass++ ;
                $i = $max_hdnumb ;
            }
        }
    }
    die "Microdrive full" ;
    return $max_hdnumb + 1 ; # MDR is full
}

# ____________________________________________________________________________

sub slurp {
    open ( MDR, '<', $option{ cartridge } ) || warn "Cannot open mdr $option{ cartridge } " ;
    $realsize = sysread( MDR, $whole,  254 * $SSIZE + 1 ) ;
    close MDR ;
}

# ____________________________________________________________________________

sub burp {
    open ( MDR, '>', $option{ out } ) || die "Cannot write mdr $option{ out } " ;
    syswrite( MDR, $whole, $realsize ) ;
    close MDR ;
}

# ____________________________________________________________________________

# change label of mdr. Label is repeated in each sector.
sub new_label {
    my $newname = shift ;
    for ( my $i = 0 ; $i < 254 ; ++$i ) {
        my $offset = $i * $SSIZE ;
        my $header = substr( $whole, $offset, 15 ) ;  #sysread( MDR, $sector,  15 + 528 ) ;
        my ( $hdflag, $hdnumb, $ntused, $hdname, $hdchk  ) = unpack( 'CC S A10 C', $header ) ;
        $hdname = $newname . (' ' x 10);
        $header = pack( 'CC S A10', $hdflag, $hdnumb, $ntused, $hdname  ) ;
        $hdchk = checksum( $header, 14 ) ;
        $header = pack( 'CC S A10 C', $hdflag, $hdnumb, $ntused, $hdname, $hdchk  ) ;
        substr( $whole, $offset, 15 ) = $header ;
    }
    $option{ out } = $option{ cartridge } unless $option{ out };
}

# ____________________________________________________________________________

sub analyze {
    my $offset = 0 ;
    for ( my $i = 0 ; $i < 254 ; ++$i ) {
        $offset = $i * $SSIZE ;
        my $sector = substr( $whole, $offset, $SSIZE ) ;  #sysread( MDR, $sector,  15 + 528 ) ;
        if ( $offset + 1 >= $realsize || length( $sector ) < 15 + 528 ) {
            last ;
        }
        my ( $hdflag, $hdnumb, $ntused, $hdname, $hdchk  ) = unpack( 'CC S A10 C'    , $sector ) ;
        my ( $recflg, $recnum, $reclen, $recnam, $deschk ) = unpack( 'x15 CC S A10 C', $sector ) ;
        my $data = substr( $sector, 30, 512 ) ;
        my $dchk = unpack( "x15 x15 x512 C", $sector ) ;
        my $eofblk = ($recflg & 2) ? 1 : 0 ;
        my $empty  = (0 == ($recflg & 2) and 0 == ($reclen & 512)) ? 1 : 0 ;
        my $fhdchk  = checksum( substr( $sector,  0, 14), 14) ;
        my $fdeschk = checksum( substr( $sector, 15, 14), 14) ;
        my $fdchk   = checksum( substr( $sector, 30,512),512) ;
        my $noise    = checkbittoggle( substr( $sector, 30,512),512) ;
        my $unusable = ( 255 == $hdnumb || ($fdchk != $dchk && 0 == $noise ) ) ? 1 : 0 ; # or ($eofblk and ( 0 == $reclen || $reclen > 512 ) )  # || ($fdchk != $dchk && $dchk

        if ( $fhdchk != $hdchk || $fdeschk != $deschk || $fdchk  != $dchk ) {
            if ( $option{ fix } ) {
                if ( $fhdchk != $hdchk ) {
                    $hdchk  = $fhdchk  ;
                    substr( $sector,   14, 1 ) = pack( 'C', $hdchk  ) ;
                }
                if ( $fdeschk != $deschk ) {
                    $deschk = $fdeschk ;
                    substr( $sector,  15 + 14 , 1 ) = pack( 'C', $deschk ) ;
                }
                if ( $fdchk  != $dchk && $unusable ) {
                    $dchk   = $fdchk   ;
                    substr( $sector,  15 +15 + 512, 1 ) = pack( 'C', $dchk ) ;
                }
                substr( $whole, $offset, $SSIZE ) = $sector ;
                $option{ out } = $option{ cartridge } unless $option{ out };
            }
            unless ( $unusable && !$option{ verbose }) {
                warn "hdnumb $hdnumb | recnum $recnum | bad hdchk  checksum expected $fhdchk  found $hdchk \n" if $fhdchk  != $hdchk  ;
                warn "hdnumb $hdnumb | recnum $recnum | bad deschk checksum expected $fdeschk found $deschk\n" if $fdeschk != $deschk ;
                warn "hdnumb $hdnumb | recnum $recnum | bad dchk   checksum expected $fdchk   found $dchk  \n" if $fdchk   != $dchk && !($empty or $unusable) ;
                warn "Invalid checksum. Unformatted cartridge\n" if ( $dchk == 255 || $deschk == 255 || $dchk  == 255 ) ;
            }
        }

        # offset to sector
        printf ( "%5Xh : ", $SSIZE * $i ) if $option{ verbose } ;
    
        # any checksum will never be 255
        if ( $dchk == 255 || $deschk == 255 || $dchk  == 255 || $hdnumb == 255 ) {
            unless ( $unusable ) {
                print "Unformatted" if $option{ verbose } ;
                $unformatted++ ;
            }
        }

        # header
        if ( $option{ verbose } ){
            printf ( "%03d : ", $hdnumb ) ;
            printf ( "%02X %02X %04X %-10s : ", $hdflag, $hdnumb, $ntused, $hdname ) ;
            # record
            # $recnam = substr( $sector, 20, 10 ) unless $recnam ;
            printf ( "%03d : ", $recnum ) ;
            printf ( "%02X %02X %04X %-10s", $recflg, $recnum, $reclen, $recnam ) ;
            # flags meaning
            if ( $unusable ) {
                print  ": unusable "
                }
            else {
                print  ": empty    " if  !($recflg & 2) && ($reclen==0  ) ;
                print  ": not used " if !(($recflg & 2) || ($reclen==512));
                print  ": PRN"       if  !($recflg & 4) ;
                print  ": EOF"       if   ($recflg & 2) ;
                # show wrong checksum
                printf ( "[%02X!%02x]", $fhdchk,  $hdchk ) if $fhdchk  != $hdchk  ;
                printf ( "[%02X!%02x]", $fdeschk, $deschk) if $fdeschk != $deschk ;
                printf ( "[%02X!%02x]", $fdchk,   $dchk  ) if $fdchk   != $dchk   ;
            }
            # show first 32 bytes data dump
            1;
            printf ( ": " . ("%02X" x 8 . ' ') x 4, unpack("C32", substr( $data,0,32 ) ) ) if 2 == $option{ verbose } ;
            printf ( ": " . ("%02X" x 8 . ' ') x 64, unpack("C512", substr( $data,0,512 ) ) ) if 3 == $option{ verbose } ;
            ##print substr( $data,0,32 ) ;
        }
        
        # collect header name, catalog name etc.
        if ( $unusable ) {
            push @{ $cart_unusable{ $hdnumb } }, sprintf( "%03d", $hdnumb ) ;
        }
        elsif ( $empty ) {
            my $key = sprintf( '%-10s', $recnam ) . '-' ;
            $cart_catalog{ $key } += $reclen ;
            push @{ $cart_cat_sec{ $key } }, sprintf( "%03d", $hdnumb ) ;
            $cart_label{ $hdname } ++ ;
        }
        else {
            $cart_catalog{ $recnam } += $reclen ;
            push @{ $cart_cat_sec{ $recnam } }, sprintf( "%03d", $hdnumb ) ;
            $cart_label{ $hdname } ++ ;
        }

        print "\n" if $option{ verbose } ;

        my $temp = {
            hdflag  => $hdflag  , 
            hdnumb  => $hdnumb  , 
            ntused  => $ntused  , 
            hdname  => $hdname  , 
            hdchk   => $hdchk   ,
            
            recflg  => $recflg  , 
            recnum  => $recnum  , 
            reclen  => $reclen  , 
            recnam  => $recnam  , 
            deschk  => $deschk  , 
            
            data    => $data    , 
            dchk    => $dchk    ,
    
            fhdchk  => $fhdchk  ,
            fdeschk => $fdeschk ,
            fdchk   => $fdchk   ,
    
            eofblk  => $eofblk  ,
            empty   => $empty   ,
            unusable=> $unusable,
            sector  => $sector  ,
            offset  => $offset
            } ;
    
        $record->{ sprintf( "%03d", $hdnumb ) } = $temp ;
        $max_hdnumb = $hdnumb if $max_hdnumb < $hdnumb ;
        $current_head = $max_hdnumb ;
    
        1 ; # breakpoint
    }
    
    $readonly = substr( $whole, $offset, 1 ) ; # sysread( MDR, $readonly,  1 ) ;
    $readonly = unpack( 'C', $readonly ) ;
    
    for my $label (sort keys %cart_label) {
        if ( $master_count < $cart_label{$label} ) {
            $master_label = $label ;
            $master_count = $cart_label{$label} ;
        }
    }
    1;
}

# ____________________________________________________________________________

sub showcat {

    print "\n";

    printf ( "Header: \"%-10s\"\n", $master_label ) ;

    if ( scalar( keys %cart_label ) > 1 ) {
        for my $label (sort keys %cart_label) {
            printf ( "        \"%-10s\" (%d times)\n", $label, $cart_label{$label} ) ;
        }
    }
    my $fmt = "%-4s %7d %-11s " ;
    print "\n";
    print "\n";
    print "Type   Bytes Filename    ";
    print                          "Sector used\n" if $option{ sectors } ;
    print "____ _______ ___________ _______________________________________________\n" if $option{ sectors } ;
    print                          " Addr. Actual   Vars   Line\n" unless $option{ sectors } ;
    print "____ _______ ___________ ______ ______ ______ ______\n" unless $option{ sectors } ;

    print "\n";                
    my $used = 0 ;
    my $deleted = 0 ;
    my $bad  = 0 ;

    for my $name (sort keys %cart_catalog) {
        next unless $name ;
        my $size = $cart_catalog{$name} ;
        next unless $size ;
        $used += $size ;
        my $first_record = -1 ;
        my @temp = @{ $cart_cat_sec{ $name } } ;
        for my $nrec ( @temp ) {
            $first_record = $nrec if 0 == $record->{ $nrec }->{ recnum } ;
        }
        my @list = sort { $b cmp $a } @temp ;
        my $reclen = $record->{ $first_record }->{ reclen } || 0 ;
        my $recflg = $record->{ $first_record }->{ recflg } || -1 ;
        my $data = $record->{ $first_record }->{ data } || '00000000' ;
        my @detail = unpack("CSSSS", substr( $data,0,9 ) ) ;
        my $type = ( $reclen && !($recflg & 4)) ? 'Prnt' : 'Norm' ;

        if ( $first_record == -1 ) {
            $type = 'Bad!' ;
            $detail[1] = 512 * scalar( @temp ) ;
        }
        $type = "Prog" if $type eq 'Norm' && 0 == $detail[0] ;
        $type = "Numb" if $type eq 'Norm' && 1 == $detail[0] ;
        $type = "Char" if $type eq 'Norm' && 2 == $detail[0] ;
        $type = "Code" if $type eq 'Norm' && 3 == $detail[0] ;
        $type = "Scrn" if $type eq 'Code' && $detail[1] == 6912 && $detail[2] == 16384 ;

        $type = 'Prnt' if $type eq 'Norm' ; # default ?

        printf ( $fmt, $type, $detail[1], $name ) if $type ne 'Prnt';
        printf ( $fmt, $type, $size     , $name ) if $type eq 'Prnt';

        if ( $option{ sectors } or $first_record == -1 ) {
            for ( my $i = 0 ; $i <= $#list ; $i++ ) {
                print ','       if $i >  0 ;
                print $list[$i] if $i <= 7 ;
                print ' ... '   if $i == 7 && $#list > 7 ;
                last if  $i == 7  && $#list > 7 ;
            }
        }
        elsif ( $list[0] ) {
            my $data = $record->{ $list[0] }->{ data } ;
            printf ( '%6d %6d %+6d %6s', $detail[2], $size, $detail[3], ($detail[4]<32768?$detail[4]:'') ) if $type eq "Prog" ;
            printf ( '%6d %6d %6s', $detail[2], $size, chr(127&$detail[3]+64).'()' )                       if $type eq "Numb" ;
            printf ( '%6d %6d %6s', $detail[2], $size, chr(127&$detail[3])  .'$()' )                       if $type eq "Char" ;
            printf ( '%6d %6d', $detail[2], $size )                                                        if $type eq "Code" ;
            printf ( '%6d %6d', $detail[2], $size )                                                        if $type eq "Scrn" ;
            # printf ( ' ' x 14 . '%02X ' x 16, unpack("C16", substr( $data,0,16 ) ) )                       if $type eq "Prnt" ;
        }

        print "\n" ;

    }

    print "\n" if $option{ showdeleted } ;
    for my $name (sort keys %cart_catalog) {
        my $size = $cart_catalog{$name} ;
        next if $name && $size ;
        next unless $option{ showdeleted } ;
        my @list = sort { $b <=> $a } @{ $cart_cat_sec{ $name } } ;
        $size = 512 * scalar(@list) ;
        $deleted += $size ;
        my $type = 'Free';
        printf ( $fmt, $type, $size, $name ) ;
        print " sect: " ;
        for ( my $i = 0 ; $i <= $#list ; $i++ ) {
            print ','       if $i >  0 ;
            print $list[$i] if $i <= 7 ;
            print ' ... '   if $i == 7 && $#list > 7 ;
            last if  $i == 7  && $#list > 7 ;
        }
        print "\n" ;
    }

    if ( $option{ showbad } ) {
        print "\n" ;
        for my $hdnumb (sort keys %cart_unusable) {
            my $size = 512 ;
            $bad += $size ;
            my $recflg = 0 ;
            my $type = 'Bad' ;
            printf ( $fmt, $type, $size, '' ) ;
            print "$hdnumb" ;
            print " tape-junction" if $hdnumb == $max_hdnumb ;
            print "\n" ;
        }
    }
    
    my $free = 0 ;
    for ( my $i = 254 ; $i >= 0 ; $i --) {
        my $key = sprintf( "%03d", $i ) ;
        my $empty = $record->{ $key }->{ empty } ;
        my $unusable = $record->{ $key }->{ unusable } ;
        $free += 512 if $empty && !$unusable ;
    }

    print "____ _______ ___________ ___________________________________________\n";
    print "\n" ;
    printf ( "$fmt %3d K\n", '', $used, 'total used   ' , int(0.5+$used/1024) ) ;
    printf ( "$fmt %3d K\n", '', $free, 'total erased ' , int(0.5+$free/1024) ) if $option{ showdeleted } ;
    printf ( "$fmt %3d K\n", '', $bad , 'total bad    ' , int(0.5+$bad /1024) ) if $option{ showbad  } ;
    printf ( "$fmt %3d K\n", '', $free, 'total free   ' , int(0.5+$free/1024) ) ;
    print "\n" ;

}

# ____________________________________________________________________________

# erase a single filename from .MDR

sub erasefile {
    my $name = shift ;

    unless ( defined $cart_catalog{ $name } && $cart_catalog{$name} > 0 ) {
        warn "File  '$name'  does not exist in cartridge " . $option{ cartridge } . "\n";
        return ;
    }
    my @list = sort { $b <=> $a } @{ $cart_cat_sec{ $name } } ;
    for ( my $i = 0 ; $i <= $#list ; $i++ ) {
        my $key = $list[$i] ;
        my $sector = $record->{ $key }->{ sector } ;
        my ( $recflg, $recnum, $reclen, $recnam, $deschk ) = unpack( 'x15 CC S A10 C', $sector ) ;
        $recflg = 0 ;
        #$recnum = 1 ;
        $reclen = 0 ;
        $deschk = checksum( pack( 'CC S A10', $recflg, $recnum, $reclen, $recnam ) , 14 ) ;
        substr( $sector, 15, 15 ) = pack( 'CC S A10 C', $recflg, $recnum, $reclen, $recnam, $deschk ) ;
        substr( $sector, 30, 513) = chr(0) x 30 . chr(1) . chr(0) x 482 ;
        $record->{ $key }->{ sector } = $sector ;
        $record->{ $key }->{ empty } = 1 ;
        substr( $whole, $record->{ $key }->{ offset }, $SSIZE ) = $sector ;
    }
    $option{ out } = $option{ cartridge } unless $option{ out };
}

# ____________________________________________________________________________

# copy a single filename inside .MDR to a new name

sub copyfile {
    my $name = shift ;
    my $to = shift ;
    unless ( defined $cart_catalog{ $name } && $cart_catalog{$name} > 0 ) {
        warn "File  '$name'  does not exist in cartridge " . $option{ cartridge } ."\n" ;
        return ;
    }
    die "'copy' requires 'to' option" if $option{ copy } and !$option{ to } ;
    my $newname = substr( sprintf( '%-10s', $to ), 0, 10) ;
    if ( defined $cart_catalog{ $newname } && $cart_catalog{$newname} > 0 ) {
        warn "File  '$newname'  already exists in cartridge " . $option{ cartridge } ."\n" ;
        return ;
    }

    my @list = sort { # sort by recnum of each sector of file.
        $record->{ $a }->{ recnum }
        <=>
        $record->{ $b }->{ recnum }
        } @{ $cart_cat_sec{ $name } } ;

    for my $h (@list) {
        my $key = first_free() ;
        die "Microdrive full " if $key > $max_hdnumb ;
        my $sector= $record->{$key}->{ sector   } ;
        my ( $hdflag, $hdnumb, $ntused, $hdname, $hdchk  ) = unpack( 'CC S A10 C'    , $record->{$key}->{ sector   } ) ;
        my ( $recflg, $recnum, $reclen, $recnam, $deschk ) = unpack( 'x15 CC S A10 C', $record->{ $h }->{ sector   } ) ;
        my $data = substr( $record->{ $h }->{ sector   }, 30, 512+1 ) ;
        $recnam = $newname ;
        $deschk = checksum( pack( 'CC S A10', $recflg, $recnum, $reclen, $recnam ) , 14 ) ;
        substr( $sector, 15, 15 ) = pack( 'CC S A10 C', $recflg, $recnum, $reclen, $recnam, $deschk ) ;
        substr( $sector, 30, 512+1 ) = $data ;
        $record->{ $key }->{ sector } = $sector ;
        substr( $whole, $record->{ $key }->{ offset }, $SSIZE ) = $sector ;
        $record->{ $key }->{ empty } = 0 ;
    }

    $option{ out } = $option{ cartridge } unless $option{ out };
}

# ____________________________________________________________________________

# rename a single filename inside .MDR to a new name

sub renamefile {
    my $name = shift ;
    my $to = shift ;
    unless( defined $cart_catalog{ $name } && $cart_catalog{$name} > 0 ) {
        warn "File  '$name'  does not exist in cartridge " . $option{ cartridge } . "\n";
        return ;
    }
    die "'rename' requires 'to' option" if $option{ rename } and !$option{ to } ;
    my $newname = substr( sprintf( '%-10s', $to ), 0, 10) ;
    if ( defined $cart_catalog{ $newname } && $cart_catalog{$newname} > 0 ) {
        warn "File  '$newname'  already exists in cartridge " . $option{ cartridge } . "\n";
        return ;
    }

    my @list = sort { $b <=> $a } @{ $cart_cat_sec{ $name } } ;
    for ( my $i = 0 ; $i <= $#list ; $i++ ) {
        my $key = $list[$i] ;
        my $sector = $record->{ $key }->{ sector } ;
        my ( $recflg, $recnum, $reclen, $recnam, $deschk ) = unpack( 'x15 CC S A10 C', $sector ) ;
        $recnam = $newname ;
        $deschk = checksum( pack( 'CC S A10', $recflg, $recnum, $reclen, $recnam ) , 14 ) ;
        substr( $sector, 15, 15 ) = pack( 'CC S A10 C', $recflg, $recnum, $reclen, $recnam, $deschk ) ;
        $record->{ $key }->{ sector } = $sector ;
        substr( $whole, $record->{ $key }->{ offset }, $SSIZE ) = $sector ;
    }
    $option{ out } = $option{ cartridge } unless $option{ out };
}

# ____________________________________________________________________________

# remove autorun LINE from a single filename inside .MDR

sub noautorun {
    my $name = shift ;
    unless ( defined $cart_catalog{ $name } && $cart_catalog{$name} > 0 ) {
        warn "File  '$name'  does not exist in cartridge " . $option{ cartridge } . "\n";
        return ;
    }
    my @list = sort { $b <=> $a } @{ $cart_cat_sec{ $name } } ;
    for ( my $i = 0 ; $i <= $#list ; $i++ ) {
        my $key = "$list[$i]" ;
        next unless 0 == $record->{ $key }->{ recnum } ;
        my $sector = $record->{ $key }->{ sector } ;
        my $dchk = unpack( "x15 x15 x512 C", $sector ) ;
        my $data = substr( $sector, 30, 512 ) ;
        my @detail = unpack("CSSSS", substr( $data,0,9 ) ) ;
        next if 0 != $detail[0] && $option{ noautorun } eq '*' ;
        unless ( 0 == $detail[0] ) {
            warn "File  '$name'  is not a program in cartridge " . $option{ cartridge } . "\n";
            return;
        }
        if ( $detail[4]<32768 ) {
            substr( $data, 7, 2 ) = pack( 'S', 65535 ) ;
            $dchk = checksum( $data, 512 ) ;
            substr( $record->{ $key }->{ sector }, 30+7, 2 ) = pack( 'S', 65535 ) ;
            substr( $record->{ $key }->{ sector }, 30+512, 1 ) = pack( 'C', $dchk ) ;
            substr( $whole, $record->{ $key }->{ offset }, $SSIZE ) = $record->{ $key }->{ sector } ;
            $option{ out } = $option{ cartridge } unless $option{ out } ;
            print "File  '$name'  Autorun removed in cartridge " . $option{ cartridge } . "\n";
            return ;
        }
    }
}

# ____________________________________________________________________________

# remove autorun LINE from a single filename inside .MDR

sub autorun {
    my $name = shift ;
    my $line = shift ;
    unless ( defined $cart_catalog{ $name } && $cart_catalog{$name} > 0 ) {
        warn "File  '$name'  does not exist in cartridge " . $option{ cartridge } . "\n" ;
        return ;
    }
    die "Usage mdr.pl <cartridge.mdr> autorun=filename line=n" unless $line ;
    my @list = sort { $b <=> $a } @{ $cart_cat_sec{ $name } } ;
    for ( my $i = 0 ; $i <= $#list ; $i++ ) {
        my $key = "$list[$i]" ;
        next unless 0 == $record->{ $key }->{ recnum } ; # find first recnum
        my $sector = $record->{ $key }->{ sector } ;
        my $dchk = unpack( "x15 x15 x512 C", $sector ) ;
        my $data = substr( $sector, 30, 512 ) ;
        my @detail = unpack("CSSSS", substr( $data,0,9 ) ) ;
        unless ( 0 == $detail[0] ) {
            warn "File  '$name'  is not a program " . $option{ cartridge } . "\n";
            return ;
        }
        substr( $data, 7, 2 ) = pack( 'S', ( $line & 65535 ) ) ; # set 4th S
        $dchk = checksum( $data, 512 ) ;
        substr( $record->{ $key }->{ sector }, 30+7, 2 ) = pack( 'S', $line ) ;
        substr( $record->{ $key }->{ sector }, 30+512, 1 ) = pack( 'C', $dchk ) ;
        substr( $whole, $record->{ $key }->{ offset }, $SSIZE ) = $record->{ $key }->{ sector } ;
        $option{ out } = $option{ cartridge } unless $option{ out } ;
        print "Autorun set for $name to line $line\n" ;
        return ;
    }
}

# ____________________________________________________________________________

# remove autorun LINE to all possible filename inside .MDR

sub multi_noautorun {
    
    for my $name (sort keys %cart_catalog) {
        next unless $name ;
        my $size = $cart_catalog{$name} ;
        next unless $size ;
        noautorun( $name ) ;
    }
    $option{ noautorun } = '' ;
}



# ____________________________________________________________________________

# reads a file from MDR and returns "header" and "content"

sub getfile {

    my $name     = shift ;
    my $data = '' ;
    warn "File  '$name'  does not exist in cartridge " . $option{ cartridge } . "\n" unless defined $cart_catalog{ $name } && $cart_catalog{$name} > 0 ;
    my @list = sort { # sort by recnum of each sector of file.
        $record->{ $a }->{ recnum }
        <=>
        $record->{ $b }->{ recnum }
        } @{ $cart_cat_sec{ $name } } ;
    map { $data .= $record->{ $_ }->{ data } } @list ;
    
    my $reclen = $record->{ $list[0] }->{ reclen } ;
    my $recflg = $record->{ $list[0] }->{ recflg } ;
    my @detail = unpack("CSSSS", substr( $data,0,9 ) ) ;
    my $type = ( $reclen && !($recflg & 4)) ? 'Prnt' : 'Norm' ;
    my $bytes = $cart_catalog{ $name } ;
    my $header = '' ;
    my $content = '' ;

    if ( $type eq 'Prnt' ) {
        $content = substr( $data, 0, $bytes ) ;
    }
    elsif ( $option{ dump } || $option{ text } ) {
        $content = substr( $data, 9, $detail[1] ) ;
    }
    else {
        $header = pack ( 'CC A10 SSS' , 0, $detail[0], $name, $detail[1], $detail[4], $detail[3] ) if $detail[0] == 0;
        $header = pack ( 'CC A10 SSS' , 0, $detail[0], $name, $detail[1], $detail[3], $detail[4] ) if $detail[0] == 1;
        $header = pack ( 'CC A10 SSS' , 0, $detail[0], $name, $detail[1], $detail[3], $detail[4] ) if $detail[0] == 2;
        $header = pack ( 'CC A10 SSS' , 0, $detail[0], $name, $detail[1], $detail[2], 32768      ) if $detail[0] == 3;
        $header .= checkbittoggle( $header, length( $header ) ) ;
        # $header = pack ( 'S', length($header) ) . $header ;
        
        $content = substr( $data, $detail[1] ) ;
        $content .= checkbittoggle( $content, length( $content ) ) ;
        # $content = pack ( 'S', length($content) ) . $content ;
    }
    
    print length( $content ) ;

    if ( $option{ dump32 } ) {
        my @tmp = unpack( "(A32)*", $content );
        if ( $option{ dumpblock } ) {
            for( my ($k,$i) = (1,0); $i < $#tmp ; $k++, $i+=16 ) {
                $tmp[$i] = "\n\n\\ Screen# $k\n$tmp[$i]" ;
            }
        }
        local $" = "\n" ;
        $content = "@tmp\n" ;
    }

    return ($header, $content ) ;
}

# ____________________________________________________________________________

# Experimental...
sub text_to_tape {
    my $text = shift || die "Usage mdr.pl text2tap=<file.txt> tape=<hostfile.tap>" ;
    my $tape = shift || die "Usage mdr.pl text2tap=<file.txt> tape=<hostfile.tap>" ;
    open ( F, "<$text" ) || die "Cannot read $text" ;
    my @text = <F> ;
    close F ;
    open ( H, ">>$tape" ) || die "Cannot write $tape" ;
    for my $row (@text) {
        #my ($header, $content) = getfile( $name ) ;
        #print H pack ( 'S', length($header) ) . $header ;
        #print H pack ( 'S', length($content) ) . $content ;
    }
    $option{ noautorun } = '' ;
    close H ;
}

# ____________________________________________________________________________

sub mdr_to_tape {
    my $tape = shift || die "Usage mdr.pl -g <cartridge.mdr> tape=hostfile.tap" ;

    open ( H, ">>$tape" ) || die "Cannot write $tape" ;

    for my $name (sort keys %cart_catalog) {
        next unless $name ;
        my $size = $cart_catalog{$name} ;
        next unless $size ;
        
        my ($header, $content) = getfile( $name ) ;
        print H pack ( 'S', length($header) ) . $header ;
        print H pack ( 'S', length($content) ) . $content ;
    }
    $option{ noautorun } = '' ;

    close H ;
}    
    
# ____________________________________________________________________________

sub putfile {
    my $name = shift ;
    my $content = shift ;
    my $prnfile = shift || 0 ;
    
    erasefile( $name ) if ( defined $cart_catalog{ $name } && $cart_catalog{$name} > 0 ) ;
    
    my ( $recflg, $recnum, $reclen, $recnam, $deschk ) ; # unpack( 'x15 CC S A10 C', $sector )
    $recflg =  0 ;
    $recflg =  4 unless $prnfile ;
    $recnum = -1 ;
    $recnam = sprintf( "%-10s", substr( $name, 0, 10  ) ) ;
    
    while ( length( $content ) > 0 ) {
        ##printf ( ": " . ("%02X ") x 32, unpack("C32", substr( $content,0, 32 ) ) ) ;
        ##print "\n" ;
        $recnum ++ ;
        my $key = first_free() ;
        my $data = pack( 'C', 0 ) x 512 ;
        $reclen = length( $content ) ; 
        if ( $reclen > 512 ) { $reclen = 512 } else { $recflg |= 2 }
        substr( $data, 0, $reclen ) = substr( $content, 0, $reclen ) ;
        $deschk = checksum( pack( 'CC S A10', $recflg, $recnum, $reclen, $recnam ) , 14 ) ;
        my $dchk = checksum( $data, 512 ) ;
        my $eofblk = ($recflg & 2) ? 1 : 0 ;
        my $empty = (0 == ($recflg & 2) and 0 == ($reclen & 512)) ? 1 : 0 ;

        my $sector =
              substr( $record->{ $key }->{ sector  }, 0, 15 )
            . pack( 'CC S A10 C', $recflg, $recnum, $reclen, $recnam, $deschk )
            . pack( 'A512', $data )
            . pack( 'C', $dchk ) ;

        $record->{ $key }->{ sector   } = $sector   ;
        $record->{ $key }->{ empty    } = 0   ;
        substr( $whole, $record->{ $key }->{ offset }, $SSIZE ) = $sector ;
        
        $content = substr( $content, $reclen ) ;
    }
    
    $option{ out } = $option{ cartridge } unless $option{ out };
}

# ____________________________________________________________________________

sub file_to_mdr {
    my @list = () ;
    my @to   = () ;
    if ( ref( $_[0] ) ) {
        @list = @{$_[0]} ;
    }
    else {
        push @list, $_[0] ;
    }
    if ( ref( $_[1] ) ) {
        @to = @{$_[1]} ;
    }
    else {
        push @to, $_[1] if $_[1] ;
    }

    for ( my $i=0 ; $i <= $#list; $i++ ) {
        my $name = $list[$i] ;
        my $to   = $to[$i] || $name ;
        if ( open F, $name ) {
            my $content ;
            sysread( F, $content, -s F ) ;
            if ( defined $cart_catalog{ $to } && $cart_catalog{$to} > 0 ) {
                warn "File  '$to'  already exists in cartridge " . $option{ cartridge } . "\n" ;
            } else {
                putfile( $to, $content, 1 )
            }
        }
        else {
            warn "File  '$name'  cannot read in cartridge " . $option{ cartridge } ."\n" ;
        }
    }
    close F ;
}

# ____________________________________________________________________________

sub tape_to_mdr {

    my $tape = shift ;
    my $content ;

    open H, "<", "$tape" || die "Cannot read $tape" ;
    sysread( H, $content, -s H ) ;
    close H ;

    my ( $len, $flag, $type, $name, $datalength, $param1, $param2, $hchk ) ;
    my ( $header, $data, $dchk ) ;
   
    while ( length( $content ) > 2 ) {
        ( $len, $flag ) = unpack( 'SC', $content );
        ##printf ( ": " . ("%02X ") x 32, unpack("C32", substr( $content,0, 32 ) ) ) ;
        ##print "\n" ;
        if ( 0 == $flag ) {
            ( $len, $flag, $type, $name, $datalength, $param1, $param2, $hchk )  = unpack( 'S CC A10 SSS C', $content ) ;
            my $verify = checkbittoggle( substr($content, 2, 18), 18 ) ;
            warn( "File  '$name'  header: wrong checksum in cartridge " . $option{ cartridge } ."\n" ) if $hchk != $verify ;
            $header = pack ( 'CSSSS' , $type, $datalength,   23813, $param2  , $param1 ) if $type == 0;
            $header = pack ( 'CSSSS' , $type, $datalength,      -1, $param1  , $param2 ) if $type == 1;
            $header = pack ( 'CSSSS' , $type, $datalength,      -1, $param1  , $param2 ) if $type == 2;
            $header = pack ( 'CSSSS' , $type, $datalength, $param1,        -1,      -1 ) if $type == 3;
            my @detail = unpack("CSSSS", substr( $header,0,9 ) ) ;
            1;
        }
        else {
            warn "wrong length in tape $tape\n" if $len != 2+$datalength ;
            ##printf ( ": " . ("%02X ") x 32, unpack("C32", substr( $content,$datalength-32+4, 32 ) ) ) ;
            ##print "\n" ;
            my $pattern = "SCA${datalength}C" ;
            ( $len, $flag, $data, $dchk )  = unpack( $pattern, $content ) ;
            $data = substr( $content, 3, $len-2 ) ;
            my $verify = checkbittoggle( substr( $content, 2, $len-1 ), $len-1 ) ;
            warn( "File  '$name'  header: wrong checksum in cartridge " . $option{ cartridge } ."\n" ) if $dchk != $verify ;
            my @detail = unpack("CSSSS", substr( $header,0,9 ) ) ;
            my @follow = unpack("A${datalength}", $data ) ;
            print length($header) . ' ' . length($data) . "\n" ;
            if ( defined $cart_catalog{ $name } && $cart_catalog{$name} > 0 ) {
                warn "File  '$name'  already exists in cartridge " . $option{ cartridge } ."\n"  ;
            }
            else {
                putfile( $name, $header . $data  ) ;
            }
        }
        $content = substr( $content, 2+$len ) ;
    }

    $option{ out } = $option{ cartridge } unless $option{ out };
}

# ____________________________________________________________________________

sub sector_dump {

    print "\n";
    print "\n";
    for my $name (sort keys %cart_catalog) {
        next unless $name ;
        printf ( "File : [ %-10s ]\n", $name ) ;
        for my $rec ( sort { $record->{ $a }->{ recnum } <=> $record->{ $b }->{ recnum } } @{ $cart_cat_sec{ $name } } ) {
            print sprintf( "File : [ %-10s ] - Record : [ %03d ]\n", $name, $record->{ $rec }->{ recnum } ) ;
            my $reclen = $record->{ $rec }->{ reclen } ;
            my $recflg = $record->{ $rec }->{ recflg } ;
            if ( $reclen && !($recflg & 4) ) {
                for ( my $i = 0 ; $i < 16 ; ++$i ) {
                    print sprintf("%02d : %s\n", 1+$i, substr( $record->{ $rec}->{ data }, 32*$i,32 ) ) ;
                }
            }
            else {
                my $fmt = (("%02X" x 8 . ' ') x 4) ;
                for ( my $i = 0 ; $i < 16 ; ++$i ) {
                    my $data = substr( $record->{ $rec}->{ data }, 32*$i,32 ) ;
                    print sprintf( "%02d : ", 1+$i ) ;
                    my @ary = unpack("C32", $data ) ;
                    print sprintf( "$fmt\n", @ary ) ;
                }
            }
            print "\n\n" ;
        }
    }
}

# ____________________________________________________________________________

if ( $option{ create } ) {
    unless ( -f $option{ cartridge } ) {
        $realsize = 254 * $SSIZE + 1 ;
        $whole = pack ( "H2" x $realsize, "00" x $realsize ) ;
        $option{ out } = $option{ cartridge } ;

        for ( my $i = 0 ; $i < 254 ; ++$i ) {
            my $offset = $i * $SSIZE ;
            my ( $hdflag, $hdnumb, $ntused, $hdname, $hdchk  ) = # unpack( 'CC S A10 C', $header ) ;
               ( "01" , 254-$i , "00" , " "x10 , 0 ) ;
            $hdname = ' ' x 10;
            my $header = pack( 'H2 C S A10', $hdflag, $hdnumb, $ntused, $hdname  ) ;
            $hdchk = checksum( $header, 14 ) ;
            $header = pack( 'H2 C S A10 C', $hdflag, $hdnumb, $ntused, $hdname, $hdchk  ) ;
            substr( $whole, $offset, 15 ) = $header ;
        }
        burp ;
    }
}

# read mdr
slurp ;

# analyze and show content
analyze ;
showcat    if $option{ showcat } ;

# erase / rename / copy
erasefile   ( $option{ erase }  )                if $option{ erase } ;
renamefile  ( $option{ rename }, $option{ to } ) if $option{ rename } ;
copyfile    ( $option{ copy }  , $option{ to } ) if $option{ copy } ;

# remove all autorun
multi_noautorun if $option{ noautorun } eq '*' ;
# remove autorun from a single file
noautorun( $option{ noautorun } ) if $option{ noautorun } && $option{ noautorun } ne '*';
# add autoruno to a single file
autorun( $option{ autorun }, $option{ line } ) if $option{ autorun } ;
# change label to cartridge
new_label( $option{ label } ) if $option{ label } ;


# extract anything from cartridge to host-file
if ( $option{ get } && ( $option{ tape } or $option{ dump } or $option{ text } ) ) {
    my ($header,$content) = getfile( $option{ get } ) ;
    my $hostname = $option{ tape } || $option{ dump } || $option{ text };
    open (H, ">","$hostname") || die "Cannot write $hostname" ;
    syswrite( H, $header, length($header) ) if length( $header ) ;
    syswrite( H, $content, length($content) ) if length( $content ) ;
    close H ;
}

if ( $option{ put } && ( $option{ host } or $option{ text } ) ) {
    my $hostname = $option{ tape } || $option{ host } || $option{ text };
    my $content = '';
    open (H, "<","$hostname") || die "Cannot read $hostname" ;
    sysread( H, $content, -s $hostname ) ;
    close H ;

    if( $option{ dump32 } ) {
        my @tmp = split( /\n/, $content ) ;

        if ( $option{ dumpblock } ) {
            my @buf = () ;
            my $line = 16 ;
            my $scrn = 1 ;
            for my $row (@tmp) {
                if ( $row =~ /^\\ Screen# (\d+)/ ) {
                    my $nextscr = $1 ;
                    while ( $line < 16 ) {
                        push( @buf, ' ' x 32) ;
                        $line++ ;
                    }
                    while ( $scrn < $nextscr ) {
                        push( @buf, ' ' x 512);
                        $scrn++;
                    }
                    $line = 0 ;
                    $scrn++ ;
                    next ;
                }
                next if $line >= 16 ;
                push ( @buf,$row );
                $line++ ;
            }
            @tmp = @buf ;
        }

        map{ $_ = sprintf( '%-32s', $_ ) } @tmp ;
        local $" = '' ;
        $content = "@tmp" ;
    }

    if ( $option{ text } ) {  $content =~ s/\n/\r/mg }
    putfile( $option{ put }, $content, ( $option{ host }||$option{ text } ? 1 : 0 ) ) ;
    print "Host file $hostname copied to $option{ text } in cartridge $option{ cartridge } \n" ;
}

# put to mdr from tape or text file
if ( $option{ put } && !$option{ get } && $option{ tape } ) {
    tape_to_mdr( $option{ tape } )                if      $option{ tape } && $option{ put } eq '*';
    # file_to_mdr( $option{ put }, $option{ to } )  if ref( $option{ put } ) || $option{ put } ne '*';
}


# flush to mdr if necessary
burp       if $option{ out } ;
print "File $option{ cartridge } was NOT modified\n" unless $option{ out } ;

1;

__END__
-l d:\zx\Z80\20160827.MDR rename=run to=Run04
-l d:\zx\Z80\20160827.MDR -p tape=\zx\forth\vforth13.tap
-l d:\zx\Z80\20160829.MDR rename=sys64 to=SYS64
-l d:\zx\Z80\20160829.MDR -p tape=d:\zx\forth\vforth13.tap
-l d:\zx\Z80\20160829.MDR erase=run
-l d:\zx\Z80\20160829.MDR -p tape=d:\zx\forth\vforth13.tap
-l d:\zx\Z80\20160829.MDR put=[file1.txt,file2.txt]
-l d:\zx\Z80\20160829.MDR put=[file1.txt,file2.txt] to=file1
-l wl1.mdr rename="Sprite S()" to="Sprite s()"
-l -b -s d:\zx\Forth\2016\FORTH1.MDR
-l "d:/zx/Forth/2018/RUN.MDR" out=OUT.MDR label=FORTH 
-l "d:/zx/Forth/2018/RUN.MDR" rename=Forth
-l "d:/zx/Forth/2018/RUN.MDR" autorun=run line=65535
-l /zx/forth/F1413/M2.MDR get=forth1413d dump=forth1413d.dump.txt
/zx/forth/F1413/M7.MDR put=F1413.f dump=/zx/Forth/F1413/F1413.f
/zx/forth/F1413/M5.MDR   put=F1413.f dump=/zx/forth/F1413/F1413.f
/zx/forth/F1413/M5.MDR   put=.bat dump=/zx/forth/F1413/copy_source_to_mdr7.bat
/zx/forth/F1413/M5.MDR   -v -lb

/zx/forth/F1413/M5.MDR   put=F1413.f text=/zx/forth/F1413/F1413.f
/zx/forth/F1413/M5.MDR   get=F1413.f dump=/zx/forth/F1413/dump.txt
/zx/forth/F1413/fuse-test2 rename=rem to=rem1
/zx/forth/F1413/M2.MDR   get=!Blocks   dump=/zx/forth/F1413/!Blocks-mdr.txt
/zx/forth/F1413/M2.MDR   put=!Blocks   dumpblock=/zx/forth/F1413/!Blocks-tmp.txt

/zx/forth/F1413/M1.MDR   get=forth1413 dump=/zx/forth/F1413/forth1413.asm

c:\zx\forth\f1413\M2 -l get=!Blocks dumpblock=/zx/forth/F1413/!Blocks-mdr2.txt
c:\zx\forth\f1413\M3 -l get=!Blocks dumpblock=/zx/forth/F1413/!Blocks-mdr3.txt
c:\zx\forth\f1413\M4 -l get=!Blocks dumpblock=/zx/forth/F1413/!Blocks-mdr4.txt
c:\zx\forth\f1413\M5 -l get=!Blocks dumpblock=/zx/forth/F1413/!Blocks-mdr5.txt

/zx/forth/F1413/M2.MDR   get=!Blocks dump=/zx/forth/F1413/!Blocks.txt
/zx/forth/F15/M2.MDR   put=!Blocks host=/zx/forth/F15/!Blocks.bin.1.dat

/zx/forth/F15/M1.MDR -p  tape=/zx/forth/F15/20160819.TAP

/zx/forth/F15/m8.MDR  put=F15m.f  host=/zx/forth/F15/src/F15m.f
/zx/forth/F15/M2.mdr  get=!Blocks  text=/zx/forth/F15/src/dump.txt
/zx/forth/F15/M2.mdr  get=!Blocks  text=/zx/forth/F15/dump.txt

-c /zx/mdr/empty_new.mdr


