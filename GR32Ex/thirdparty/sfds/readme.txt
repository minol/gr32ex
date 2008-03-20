Single File Data Storage
Version 1.4.1
e-mail: muralexandru@yahoo.com

1.Installation:
Unzip the package to a directory of your choice.
Select Tools | Environment Options... on the menu bar.
Go to Library tab and add the full path of your Single File Data Storage directory with its subdirectories to the Library Path if you have not already done so.
The Library Path field should then look similar to this: 

$(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;c:\SFDS;c:\SFDS\bzip2;c:\SFDS\ZLibEx

Click OK. 

2.Description:
The Single File Data Storage library provides an efficient solution for an application to store many different types of data inside one file and to be able to access this data very fast and easily, WITHOUT bothering about CREATING TEMPORARY FILES OR STREAMS; when requesting to read, the compressed data is decompressed on the fly directly from the source stream.
Look at the samples and in the help file to see how easy it is to use SFDS.

3.Features:
- Single-file Virtual File System(read-only): SFDS files are ZIP like archive files (not really ZIP files) with enhanced functionality (see below). One or more SFDS files can be "mounted" in the application. Searching or requesting to open a stream for read will query all "mounted" files or you can just specify a single one.
- Transparent streaming compression/decompression with full TStream compatibility.
- Thread-safe(When reading from files): Read from multiple streams (located in the same SFDS file archive) at the same time (Just create a new clone of the stream in each thread - see demo).
- High performance: SFDS is perfect for Games(and other applications such as backup, etc) which need to store many (ussualy small) files in just a small number of (big) archives. Storing data in a small number of SFDS files results in faster access time to data (When opening a SFDS file the list of streams inside is cached in memory), and also makes it harder to modify files inside.
- Large file support (64-bit size) lets you store all the data you need in SFDS files of virtually unlimited size.
- Supported compression types: none (stored), zlib, bzip2. New formats can easily be added.
- Compression support is modular. Each application can chose to add only the compression it needs (if you need zlib compression/decompression simply add sfds_compressorzlib to the uses clause somewhere inside your project; add sfds_compressorbzip2 for BZip2).
- Per stream compression option; store one stream uncompressed, another compressed with zlib, another with bzip2, etc.
- No DLLs required.
- No file name restrictions (including unicode file names allowed - strings are stored UTF-8 encoded, just like in .dfm files). If the file name is an empty string, then you can still access the data via file index.
- Reading from compressed streams is just like reading from any other stream (even full seeking in any direction is allowed).
- You can create links to streams inside SFDS files (the new entries will point to the same data).
- Includes a list of opened reader objects, which are automatically destroyed if you forget about them (you only need to free the streams you open).
- It has lots of routines for adding/extracting, testing (MD5 error checking) files to/from the SFDS file format.
- It also has search routines, to easily find files inside SFDS archives (SFDS_FindFirst, SFDS_FindNext, SFDS_FindClose).
- Supports metadata information: you can set any fields: string, number, date, boolean, binary (Metadata Editor Form included).
- You can write/read SFDS files directly to/from any data source. Already implemented: Disk File[R/W], Memory Stream[R/W], Resource Stream[R]. Make a descendent of TSFDSCustomReader to read from any other custom source and a descendent of TSFDSCustomWriter to write to any other custom source. Once written, a SFDS file cannot have more data appended to it.
- There are no components to install, so this library also works with the free Turbo Delphi.
- Best of all: IT'S COMPLETLY FREE (Even for commercial use).

4.How to use:
 - In your project add SFDS to the uses clause.
 - If you also want ZLib compression/decompression support, then also add SFDS_CompressorZLib to the uses clause.
 - If you also want BZip2 compression/decompression support, then also add SFDS_CompressorBZip2 to the uses clause.

5.Examples:
{Load a text to a memo}
  if not SFDS_OpenStreamIndirect('Data.sfds::Data\Text\Readme.txt', OutStream, OutStreamSize) then Exit;
  Memo1.Lines.LoadFromStream(OutStream);  //Load Text in Memo1
{or}
  Reader := TSFDSFileReader.Create('Data.sfds' ); //Open SFDS File 
  OutStream := Reader.OpenStream('Data\Text\Readme.txt', OutStreamSize); //Get the stream 
  Memo1.Lines.LoadFromStream(OutStream); //Load Text in Memo1

{Write data to a SFDS file}
 Writer := TSFDSFileWriter.Create('data.sfds');
 Writer.WriteFile('readme.txt', ' Data\Text\Readme.txt', ProgressProc, 0, cfZlib, clFastest); //write a file from disk to the archive
 Writer.OpenStream('data\image1.bmp', 0, cfBZip2, clMax); //Open new stream
 Image1.Picture.Bitmap.SaveToStream(Writer); //Save a bitmap to the stream
 Writer.OpenStream('data\options.dat', 0, cfNone); //Open new stream
 Writer.Write(PChar('Some Option')^, 11); //Write some buffer to the stream
 Writer.Close; //Close the archive
 Writer.Free;  //Free the writer

6.Notes:
- The metadata information can contain strings, datetime values, boolean values, numbers in the range of int64 and any other binary data you want (pictures, etc).
- By construction, the metadata information is the only data that does not support compression (to allow applications compiled without support for compression to read it always).
- Programs compiled with Delphi5 (Obsolete in our days if using files larger than 2GB) whill fail to read sfds files larger than 2GB[In Delphi5 TStream max size is High(Longint)] (No problems from Delphi6 and UP)
- It may also compile on Delphi4 ? (Not tested, but it would have the same limitations as in Delphi5)
- SFDS is perfect for Games(and other applications such as backup, etc) which need to store many (ussualy small) files in just a small number of (big) archives.
- Works with FPC. Limitation: bzip2 (de)compression is not supported (Because FPC can't use the .obj files).
- Small memory footprint(depends on usage): Increases executable size to a maximum of 220KB with support for zlib&bzip2 or with maximum 130KB without any compression support. Additional dynamic memory is allocated for caching opened sfds file information about streams and metadata. When using the ExtractFile functions, default buffer used is 32KB. When decompressing, buffer varies from some KB (zlib) to some MB (Bzip2), depending on compression level applied (this applies to each opened stream; having many compressed streams opened at the same time increases memory usage).