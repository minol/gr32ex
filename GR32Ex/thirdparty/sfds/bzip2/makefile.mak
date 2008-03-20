# change this if necessary
!ifndef BROOT
BROOT = $(MAKEDIR)\..\..\DELPHI6
!endif

# ZIP OPTIONS
ZIPROOT=C:\PKZIP
ZIP=$(ZIPROOT)\PKZIP.EXE
ZIPOPTIONS=-ex -P
UNZIP=$(ZIPROOT)\PKUNZIP.EXE
ZIPFILE=BZIP2.ZIP
FILELIST=FILELIST.TXT

# COMPILER OPTIONS
DCC=$(BROOT)\BIN\DCC32.EXE

all: install zipfile

install:
        $(DCC) BZIP2.PAS
zipfile:
        $(ZIP) $(ZIPOPTIONS) $(ZIPFILE) @$(FILELIST)
clean:
        del *.dcu
        del *.zip
