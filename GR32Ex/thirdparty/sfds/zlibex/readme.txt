-- notes ---------------------------------------------------------------------

  the zlibex.pas unit included in this archive will work with delphi 5, 6,
  and 7 and it should work with delphi 8, 2005, and 2006.  if you previously
  downloaded my delphi 5 unit, you will notice that the unit has been
  renamed.  this was done because borland included in its delphi 6 and up
  lib directories a zlib.dcu file; and i felt it was more correct to rename
  my unit and have developers update their code than to make developers
  worry about the possible file contention.

  please contact me if you find any errors, make any changes, add new
  functionality, or have any general suggestions so that i may incorporate
  them into my version.  i can be reached via my website at
  http://www.base2ti.com.

  thanks.

-- installation --------------------------------------------------------------

  first, copy all of the files into a folder (for example, c:\delphi\zlib).
  next, include the folder in the library path in the environment options.
  finally, "use" the zlibex unit as needed.

-- history -------------------------------------------------------------------

  2006.06.02  added DateTimeToUnix for delphi 5-

  2006.03.28  moved Z_DEFLATED to interface section
              added custom compression levels zcLevel1 thru zcLevel9

  2006.03.27  added ZCompressStreamWeb
              added ZCompressStreamG (simple gzip format)

  2006.03.24  added ZCompressStrG (simple gzip format)
              added ZAdler32 and ZCrc32

  2005.11.29  changed FStreamPos to Int64 for delphi 6+

  2005.07.25  updated to zlib version 1.2.3

  2005.03.04  modified ZInternalCompressStream loops
              modified ZInternalDecompressStream loops

  2005.02.07  fixed ZInternalCompressStream loop conditions
              fixed ZInternalDecompressStream loop conditions

  2005.01.11  updated to zlib version 1.2.2
              added ZCompressStrWeb

  2004.01.06  updated to zlib version 1.2.1

  2003.04.14  added ZCompress2 and ZDecompress2
              added ZCompressStr2 and ZDecompressStr2
              added ZCompressStream2 and ZDecompressStream2
              added overloaded T*Stream constructors to support InflateInit2
                and DeflateInit2
              fixed ZDecompressStream to use ZDecompressCheck instead of
                ZCompressCheck

  2002.03.15  updated to zlib version 1.1.4

  2001.11.27  enhanced TZDecompressionStream.Read to adjust source stream
                position upon end of compression data
              fixed endless loop in TZDecompressionStream.Read when
                destination count was greater than uncompressed data

  2001.10.26  renamed unit to integrate "nicely" with delphi 6

  2000.11.24  added soFromEnd condition to TZDecompressionStream.Seek
              added ZCompressStream and ZDecompressStream

  2000.06.13  optimized, fixed, rewrote, and enhanced the zlib.pas unit
                included on the delphi cd (zlib version 1.1.3)

-- acknowledgements ----------------------------------------------------------

  erik turner - thanks for the enhancements and recommendations.
    specifically, the ZCompressionStream and ZDecompressionStream routines.
    my apologies for the delay in getting these in here.

  david bennion - thanks for finding that nasty little endless loop quirk
    with the TZDecompressionStream.Read method.

  burak kalayci - thanks for emailing to inform me about the zlib 1.1.4
    update; and again for emailing about 1.2.1.

  vicente s nchez-alarcos - thanks for emailing to inform me about the zlib
    1.2.2 update.

  luigi sandon - thanks for pointing out the missing loop condition
    (Z_STREAM_END) in ZInternalCompressStream and ZInternalDecompressStream.

  ferry van genderen - thanks for assiting me fine tune and beta test the
    ZInternalCompressStream and ZInternalDecompressStream routines.

  mathijs van veluw - thanks for emailing to inform me about the zlib 1.2.3
    update.

  j. rathlev - pointing out the FStreamPos and TStream.Position type
    inconsitency.

  ralf wenske - prototyping and assisting with ZCompressStrG and
    ZCompressStreamG.

  roman krupicka - pointing out the DateUtils unit and the DateTimeToUnix
    function wasn't available prior to Delphi 6.

-- contents ------------------------------------------------------------------

  delphi files

    zlibex.pas
    zlibex.inc

  objects files used by zlibex.pas

    adler32.obj
    compress.obj
    crc32.obj
    deflate.obj
    infback.obj
    inffast.obj
    inflate.obj
    inftrees.obj
    trees.obj

  c++ Builder 6 files

    DelphiZLib.bpr
    DelphiZlib.cpp

  zlib 1.2.3 source files (http://www.zlib.net)

    adler32.c
    compress.c
    crc32.c
    deflate.c
    infback.c
    inffast.c
    inflate.c
    inftrees.c
    trees.c
    zutil.c
    crc32.h
    deflate.h
    inffast.h
    inffixed.h
    inflate.h
    inftrees.h
    trees.h
    zconf.h
    zlib.h
    zutil.h

