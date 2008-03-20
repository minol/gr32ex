BZip2 unit by Edison Mera
Version 1.03
Edition 30-01-2003
personal e-mail:                    efmera@yahoo.com
for questions about bzip2:      bzip2@edisonware.com
web page:                  http://www.edisonware.com

Observations are welcome.
This software is "FREEWARE", that is, you don't must to pay for it.

Edison Mera Menéndez.
Vía Carpetana 196, 6A
Madrid - Spain.

1. Overview and comments

For view this file better, open it with the WordPad.
If you modify the source or use this component set in a commercial product, please let me know.

This unit is similar to the zlib unit shipped with Delphi, but uses the better compression method available in the program bzip2.  Bzip2 is a compression program written by Julian Seward, for more information see http://sources.redhat.com/bzip2/.

2. Installation

- The file makefile.bcb (usage: make -f makefile.bcb) can be used to recompile bzip2 using Borland C++ Compiler.  After compilation, the object files in the subdirectory nostdio are for use with Delphi.

- Simply add to your Delphi project the file bzip2.pas.

3. Licence

Disclaimer of warranty:
This software is provided on an "as is" basis without warranty of any kind, expressed or implied, including but not limited to the implied warranties of merchantability and fitness for a particular purpose. The person using the software bears all risk as to the quality and performance of the software.  The author will not be liable for any special, incidental, consequential, indirect or similar damages due to loss of data or any other reason, even if the author or an agent of the author has been advised of the possibility of such damages. In no event shall the author's liability for any damages ever exceed the price paid for the license to use the software, regardless of the form of the claim.

4. History

05-12-1999 Version 1.00
First release, based on bzip2 0.9.5d

04-01-2000 Version 1.01
Corrections in documentation, the new home page for the bzip2 program is http://sourceware.cygnus.com/bzip2/index.html.

21-11-2002 version 1.02
Updated to work with BZIP2 1.0.2.
Updated contact information of author.
Added a demo that shows how to work with this unit.
Now the home page for the bzip2 program is http://sources.redhat.com/bzip2/

30-01-2003 version 1.03
Added a new demo that show how to compress / decompress BZ2 files.
Updated web page address of author.

5. Detected problems:

Noting at this moment.
