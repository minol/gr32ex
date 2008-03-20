unit sfds;
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
{$I sfds.inc}

{*****************************************************************************}
{                                                                             }
{  SFDS (Single File Data Storage) Unit                                       }
{                                                                             }
{  For conditions of distribution and use, see LICENSE.TXT                    }
{                                                                             }
{*****************************************************************************}

interface
uses SysUtils, Classes, MD5, {$IFDEF VERSION_5_OR_BELLOW}Consts, FileCtrl{$ELSE}RtlConsts{$ENDIF};

type

  {$IFNDEF FPC}
  PtrUInt = Cardinal; //can hold a pointer
  {$ENDIF}

  {Raised when things go wrong.}
  ESFDSException = class(Exception);
  {Raised when an appropriate de-compressor can't be found.<BR>
  Used to provide compatibility with future versions of SFDS, which may add more compression and decompression methods.}
  ESFDSDecompressionError = class(ESFDSException);
  {Raised when a nil stream is provided to a TSFDSxxx class constructor.}
  ESFDSNilSourceStream = class(ESFDSException);
  {Raised when attempting to use invalid methods on some streams.}
  ESFDSInvalidStreamOperation = class(ESFDSException);
  {Raised when the user aborts an operation.}
  ESFDSUserAbortedOperation = class(ESFDSException);

  {The compression level used.<BR>
  Has no effect if the cfNone compression format is used.<BR>
  On modern computers, when using ZLib, there is almost no difference in compressing/decompressing speed between clFastest and clMax, so clMax should be used.}
  TSFDSCompressionLevel = (clFastest, clDefault, clMax);

  {Pointer to a SFDS_FileEntryRecord.}
  PSFDS_FileEntryRecord = ^SFDS_FileEntryRecord;
  {A SFDS_FileEntryRecord structure contains information about a file entry inside the archive.<BR>
  FileName is the name of the file. (as determined by options)<BR>
  OriginalFileName is the name of the file. (as stored in the SFDS file)<BR>
  FileSize is the unpacked size.<BR>
  FileAttributes are the attributes.<BR>
  EndPosition - StartPosition = File Size Compressed.<BR>
  The MD5Digest is automatically calculated and written when using TSFDSxxxWriter to create files. When reading(extracting) data from SFDS files, the error check is performed only by the TSFDSxxxReader.ExtractFile[Ex](extract or test the file) function.<BR>
  ExtraDataStr, ExtraDataInt stores additional data about files.}
  SFDS_FileEntryRecord = record
    FileName,
    OriginalFileName : AnsiString;
    FileSize : Int64;
    FileAttributes : Longint;
    StartPosition,
    EndPosition : Int64;
    CompressionFormat : Byte;
    CompressionLevel : Byte;
    MD5Digest : TMD5Digest;
    ExtraDataStr : AnsiString;
    ExtraDataInt : Longint;
  end;

  {Contains a list of SFDS_FileEntryRecord.}
  TSFDS_FileEntrys = class(TList)
  private
    function GetS(Index: Integer): PSFDS_FileEntryRecord;
  public
    destructor Destroy; override;
    {Adds a new entry to the list.}
    function AddEntry(Entry : SFDS_FileEntryRecord) : Integer;
    {Finds an entry based on its Filename.}
    function FindEntry(FileName : AnsiString) : PSFDS_FileEntryRecord;
    {Finds an entry based on its OriginalFilename.}
    function FindEntryOriginal(OriginalFileName : AnsiString) : PSFDS_FileEntryRecord;
    {Returns true if an entry with the filename exists or false if not.}
    function FileExists(FileName : AnsiString) : Boolean;
    {Returns true if an entry with the OriginalFilename exists or false if not.}
    function FileOriginalExists(OriginalFileName : AnsiString) : Boolean;
    {Returns the size of the filename if it exists or 0 otherwise.}
    function FileSize(FileName : AnsiString) : Int64;
    {Returns the attributes of the filename if it exists or 0 otherwise.}
    function FileAttributes(FileName : AnsiString) : Integer;
    {Returns the entry wih the Index, or nil if not in range.}
    property Entrys[Index: Integer]: PSFDS_FileEntryRecord read GetS; default;
  end;

  {TSelectiveStream is a stream that can be used to operate in only a specified region of the source stream.}
  TSelectiveStream = class(TStream)
  private
    FStartPos: Int64;
    FEndPos: Int64;
    FPosition : Int64;
    FNewSize : Int64;
    FSourceStream : TStream;
    FFreeSourceOnDestroy : Boolean;
  protected
    procedure SetSize(NewSize: Longint); override;
    {$IFNDEF VERSION_5_OR_BELLOW}
    procedure SetSize(const NewSize: Int64); override;
    {$ENDIF}
    function NewGetSize : Int64;
  public
    {SourceStream passed to the Create constructor. Read-Only.}
    property SourceStream : TStream read FSourceStream;
    {If True, the SourceStream passed to the Create constructor will be freed on Destroy.}
    property FreeSourceOnDestroy : Boolean read FFreeSourceOnDestroy write FFreeSourceOnDestroy;
    {Creates a new instance of the TSelectiveStream.<BR>
    StartPosition - EndPosition = The range in which this streams operates.<BR>
    Example: Create(SourceFileStream, 120, 200, True), creates a stream that reads-writes-seeks only betwen the 120 and 200 bytes of the original stream.}
    constructor Create(SourceStream : TStream; StartPosition,
      EndPosition : Int64; FreeSourceOnDestroy : Boolean = True); reintroduce;
    {Seeks in the source stream by Offset and Origin.<BR>
    It auto adjusts the Offset to fit the range if needed.}
    {$IFNDEF VERSION_5_OR_BELLOW}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    {$ENDIF}
    {Reads data from the source stream.<BR>
    It auto adjusts the Count to fit the range if needed.<BR>
    Returns the number of bytes actually read.}
    function Read(var Buffer; Count: Longint): Longint; override;
    {Writes data to the source stream.<BR>
    It auto adjusts the Count to fit the range if needed.<BR>
    Returns the number of bytes actually written.}
    function Write(const Buffer; Count: Longint): Longint; override;
    destructor Destroy; override;
  end;

  {Compression Strategy. The strategy parameter is used to tune the ZLib compression algorithm.}
  TSFDSCompressionStrategy = (csDefault, csFiltered, csHuffman, csRLE, csFixed);

  {Generic compressor class. Descendant classes must also override the Read, Write, Seek and SetSize methods.}
  TSFDSCompressor = class(TStream)
  public
    constructor Create(SourceStream : TStream; CompressionLevel : TSFDSCompressionLevel = clDefault; CompressionStrategy : TSFDSCompressionStrategy = csDefault); virtual; abstract;
  end;

  {Generic decompressor class. Descendant classes must also override the Read, Write, Seek and SetSize methods.}
  TSFDSDecompressor = class(TStream)
  public
    constructor Create(SourceStream : TStream; SourceStreamSize : Int64); virtual; abstract;
  end;

  TSFDSCompressorClass = class of TSFDSCompressor;
  TSFDSDecompressorClass = class of TSFDSDecompressor;

  PSFDS_CompressionFormatItem = ^SFDS_CompressionFormatItem;
  SFDS_CompressionFormatItem = record
    FormatName : AnsiString;
    FormatID : Byte;
    Compressor : TSFDSCompressorClass;
    Decompressor : TSFDSDecompressorClass;
  end;

  {Returns the number of registered compression formats.}
  function SFDS_CompressionFormatCount : Integer;
  {Returns the Index compression format item.}
  function SFDS_CompressionFormat(Index : Integer) : PSFDS_CompressionFormatItem;
  {Returns the compression format with the FormatID or nil if no such format is registered.}
  function SFDS_CompressionFormatByFormatID(FormatID : Byte) : PSFDS_CompressionFormatItem;
  {Returns the ID of the compression format with the FormatName or 0 if no such format is registered.}
  function SFDS_CompressionFormatIDByFormatName(FormatName : AnsiString) : Byte;
  {Registers a new compression format and returns its Index in the list.}
  function SFDS_RegisterCompressionFormat(FormatName : AnsiString; FormatID : Byte; CompressorClass : TSFDSCompressorClass; DecompressorClass : TSFDSDecompressorClass) : Integer;
  procedure SFDS_CheckCompressionFormatList;

  type
  TSFDSCustomReader = class;
  TSFDSCustomWriter = class;

  {The base class for all SFDS Readers/Writers Classes.}
  TSFDSCustomSource = class(TStream)
  private
    FFSFileName : AnsiString;
    FGuid: TGUID;
    FRelOffset : Int64;
    FSourceStream : TStream;
    FInternalCompressionFormat : Byte;
    FInternalFileListDataMD5 : TMD5Digest;
  protected
    function GetFInternalCompressionFormat : Byte;
    procedure SetFInternalCompressionFormat(const CompressionFormat : Byte);
  public
    {The unique identifier received when creating an reader/writer object:<BR>
    <UL>
    <LI>- The name of the file for the TSFDSFileWriter/TSFDSFileReader;</LI>
    <LI>- A string formated like "MEMORY: $%s" for TSFDSMemoryStreamWriter/TSFDSMemoryStreamReader, where the argument is the hex representation of the MemoryStream.Memory;</LI>
    <LI>- A string formated like ''EXE/RES: "Instance=%s" "ResName="%s" "ResType=%s"'' or ''"EXE/RES: "Instance=%s" "ResID=%s" "ResType=%s"'' for TSFDSResourceStreamReader, where the arguments are the parameters passed to the constructor;</LI>
    </UL>
    The string can be passed to the <a href="sfds.htm#SFDS_OpenStreamIndirect">SFDS_OpenStreamIndirect</a> function to open a stream, like "FileName::Dir\File.Txt" or "MEMORY: $%s::Dir\File.Txt', etc.}
    property FSFileName : AnsiString read FFSFileName write FFSFileName;
    {The GUID required for a specific SFDS file. Read-only.<BR>
    Can be used to delimitate SFDS files created by an application from another.}
    property RequiredGuid : TGUID read FGuid;
    {The relative offset from where the SFDS file data starts. Read-only.<BR>
    Used with SFX archives.}
    property RelativeOffset : Int64 read FRelOffset;
    {The source stream used to create the reader/writer object. Read-only.}
    property SourceStream : TStream read FSourceStream;
    constructor Create(SourceStream : TStream; RelativeOffset : Int64);
  end;

  {The mode used to clone a stream when requested.<BR>
  Used with TSFDSCustomReader.Clone.<BR>
  <UL>
  <LI>csmFull - Return an original copy of the stream used to create the reader.</LI>
  <LI>csmOnlyRequestedData - return a stream which has only the requested data.</LI>
  </UL>}
  TCloneStreamMode = (csmFull, csmOnlyRequestedData);
  {The mode used to make a SFX archive.<BR>
  <UL>
  <LI>aSfxNone - No SFX is used.</LI>
  <LI>aSfxmExeHdr - put sfx information on the header of the win32 exe (SFX method 1). The file won't be recognized as a SFX if after the creation is finished you compress it with an executable packer like UPX.</LI>
  <LI>aSfxEOF - put sfx information on the end of the file (SFX method 2). The file won't be recognized as a SFX if another program appends more data on the end of the file.</LI>
  </UL>
  For cross-platform compatibility you should use aSfxEOF (SFX method 2)}
  TAppendSFXMode = (aSfxNone, aSfxmExeHdr, aSfxEOF);

  TSFDSFileReader = class;
  TSFDSFileWriter = class;

  {If passed a compatible TSFDSProgressEvent to a TSFDSCustomWriter.WriteFile or TSFDSCustomReader.ExtractFile function, it will be called to visually update progress information. This will be a good time to call Application.ProcessMessages.<BR>
  Sender is the TSFDSCustomSource object which performs reading/writing.<BR>
  Progress is the total progress so far (in bytes).<BR>
  MaxProgress is the maximum total progress (in bytes).<BR>
  ThisTime holds the number of bytes processed from last call.<BR>
  Set Cancel to true to abort the operation.<BR>
  ArcName is the FSFileName of the Sender.<BR>
  StreamName is the name of the stream from/to which to object is currently reading/writing.}
  TSFDSProgressEvent = procedure(Sender : TSFDSCustomSource; Progress, MaxProgress, ThisTime : Int64; var Cancel : Boolean; ArcName, StreamName : AnsiString) of Object;
  {When opening a file, if the GUID passed in the constructor does not match the GUID of the file, this event is called to let you say if you can process the data inside or not.<BR>
  Can be used to delimitate SFDS files created by an application from another.<BR>
  For example you can make that all the files your application creates have a certain GUID, so you don't open files made by other applications.<BR>
  This is useful if your application needs a custom file format.}
  TSFDSSupportedEvent = procedure(Sender : TSFDSCustomReader; GUID : TGUID; var Supported : Boolean);
  {This event is called to let you know details about all the files inside a SFDS archive, immediately as they are read.<BR>
  FileNO is the number of the file information that was just read, out of a maximum of FileCount.}
  TSFDSOnReadFileEntry = procedure(Sender : TSFDSCustomReader; out FileData : SFDS_FileEntryRecord; FileNO, FileCount : Integer) of Object;

  {Specifies the type of metadata stored by an TSFDSFileExtraInfoField.<BR>
  String, Number, Date, Boolean or Binary Value.}
  TSFDSFileExtraInfo = ({ = 0}EIStr, { = 1}EINumber, { = 2}EIDate, { = 3}EIBoolean, { = 4}EIBinary);

  {Pointer to a TSFDSFileExtraInfoField.}
  PSFDSFileExtraInfoField = ^TSFDSFileExtraInfoField;
  {Stores metadata information.<BR>
  Name is the name of the property.<BR>
  Use FieldType to know what the value stores.}
  TSFDSFileExtraInfoField = record
    Name : AnsiString;
    case FieldType : TSFDSFileExtraInfo of
      EIStr :
        (ValueString : Pointer;
         ValueStringSize : Longint);
      EINumber :
        (ValueNumber : Int64);
      EIDate :
        (ValueDate : TDateTime);
      EIBoolean :
        (ValueBoolean : Boolean);
      EIBinary :
        (ValueBinary : Pointer;
         ValueBinarySize : Longint);
  end;

  {This is a list containing metadata properties of the SFDS file.}
  TSFDSFileExtraInfoList = class(TList)
  private
    function GetExtraInfoField(Index: Integer): PSFDSFileExtraInfoField;
  public
    {Assigns all metadata properties to another metadata list.}
    procedure Assign(Source: TSFDSFileExtraInfoList);
    destructor Destroy; override;
    {Adds a new, or updates an existing metadata property in the list.
    Returns the index of the property.}
    function AddOrUpdateField(Item: TSFDSFileExtraInfoField): Integer;
    {Deletes the Index metadata property from the list.}
    procedure Delete(Index: Integer);
    {Deletes all the metadata properties from the list.}
    procedure Clear; override;
    {Returns the index of the name metadata property if it exists, or -1 otherwise.}
    function IndexOfField(Name : AnsiString): Integer;
    {Saves all the metadata properties to the stream.}
    procedure SaveToStream(Stream : TStream);
    {Attempts to load metadata properties from the stream.<BR>
    The data must have been written by the SaveToStream method.}
    procedure LoadFromStream(Stream : TStream);
    {Returns the index metadata property from the list.}
    property ExtraInfoField[Index : Integer] : PSFDSFileExtraInfoField read GetExtraInfoField;
  end;

  {Determines the mode of autorenaming streams when loading a SFDS file.<BR>
  This affects the FileName of the SFDS_FileEntryRecord.<BR>
  Original name, as stored in the SFDS file will be kept in the OriginalFileName of the SFDS_FileEntryRecord<BR>
  <UL>
  <LI>sarmNone - Keep original name. (FileName = OriginalFileName)</LI>
  <LI>sarmSystem - Determined by the type of OS the application was compiled for. (Use "\" for Windows and "/" for UNIX like systems)</LI>
  <LI>sarmWindows - Automatically convert "/" to "\"</LI>
  <LI>sarmUNIX - Automatically convert "\" to "/"</LI>
  </UL>}
  TStreamAutorenameMode = (sarmNone, sarmSystem, sarmWindows, sarmUNIX);

  {Use TSFDSCustomStreamParser to get informations about the SFDS file.}
  TSFDSCustomStreamParser = class
  private
    FFreeSourceOnDestroy : Boolean;
    FSourceStream : TStream;
    FOnFileEntry: TSFDSOnReadFileEntry;
    FVersion: Longint;
    FAuthor: AnsiString;
    FAllFilesSize: Int64;
    FGuid: TGUID;
    FSubject: AnsiString;
    FTitle: AnsiString;
    FInternalCompressionFormat: Byte;
    FTimeCreated: TDateTime;
    FMaker: AnsiString;
    FMakerVersion: Longint;
    FRelOffset: Int64;
    FKeyWords: AnsiString;
    FComment: AnsiString;
    FReader : TSFDSCustomSource;
    FFileListPositionStart: Int64;
    FFileInfoExPosition: Int64;
    FFileNumber: Longint;
    FFileListPositionEnd: Int64;
    FInternalFileListDataMD5: TMD5Digest;
  public
    {SourceStream is the stream to be parsed.<BR>
    If FreeSourceOnDestroy then the SourceStream will also be freed when the object is destroyed.}
    constructor Create(SourceStream : TStream; RelativeOffset : Int64; FreeSourceOnDestroy : Boolean = False; Reader : TSFDSCustomSource = nil);
    destructor Destroy; override;
    {Call Parse to parse the SourceStream and fill in the Metadata and FileList.<BR>
    To conserve memory, you can just pass nil for the MetaData and FileList. In this case you should assign OnFileEntry.}
    procedure Parse(var MetaData : TSFDSFileExtraInfoList; var FileList : TSFDS_FileEntrys; AssignUniqueNames : Boolean = False; StreamAutorenameMode : TStreamAutorenameMode = sarmNone);
    property OnFileEntry : TSFDSOnReadFileEntry read FOnFileEntry write FOnFileEntry;
    {Metadata: Title property.}
    property Title : AnsiString read FTitle;
    {Metadata: Subject property.}
    property Subject : AnsiString read FSubject;
    {Metadata: Author property.}
    property Author : AnsiString read FAuthor;
    {Metadata: Version property.}
    property Version : Longint read FVersion;
    {Metadata: Maker property.}
    property Maker : AnsiString read FMaker;
    {Metadata: MakerVersion property.}
    property MakerVersion : Longint read FMakerVersion;
    {Metadata: Keywords property.}
    property KeyWords : AnsiString read FKeyWords;
    {Metadata: Comments property.}
    property Comments : AnsiString read FComment;
    {The date and time when this file was created. Read-only.<BR>
    This is NOT the OS timestamp information.}
    property TimeCreate : TDateTime read FTimeCreated;
    {The combined size in bytes of all files inside the SFDS archive.}
    property AllFilesSize : Int64 read FAllFilesSize;
    {The internal compression format applied to the file list. Read-Only.}
    property InternalCompressionFormat : Byte read FInternalCompressionFormat;
    {The GUID required for a specific SFDS file. Read-only.<BR>
    Can be used to delimitate SFDS files created by an application from another.}
    property RequiredGuid : TGUID read FGuid;
    {The relative offset from where the SFDS file data starts. Read-only.<BR>
    Used with SFX archives.}
    property RelativeOffset : Int64 read FRelOffset;
    property FileListPositionStart : Int64 read FFileListPositionStart;
    property FileListPositionEnd : Int64 read FFileListPositionEnd;
    property FileInfoExPosition : Int64 read FFileInfoExPosition;
    property FileNumber : Longint read FFileNumber;
    property InternalFileListDataMD5 : TMD5Digest read FInternalFileListDataMD5;
  end;

  {This is the base class for all customized SFDS streams readers.<BR>
  You will usually need to reintroduce the Create constructor, override the Destroy destructor and you must override the Clone function(this is critical).}
  TSFDSCustomReader = class(TSFDSCustomSource)
  private
    FFileEntrys : TSFDS_FileEntrys;
    FMakerVersion: Longint;
    FVersion: Longint;
    FMaker: AnsiString;
    FAuthor: AnsiString;
    FComment: AnsiString;
    FTitle: AnsiString;
    FSubject: AnsiString;
    FKeyWords: AnsiString;
    FFileNumber : Longint;
    FAllFilesSize : Int64;
    FFileListPositionStart,
    FFileListPositionEnd,
    FFileInfoExPosition : Int64;
    FSupportedProc : TSFDSSupportedEvent;
    FOnReadFileEntry : TSFDSOnReadFileEntry;
    FTimeCreated : TDateTime;
    FExtraInfoFields: TSFDSFileExtraInfoList;
    FAssignUniqueNames : Boolean;
    FStreamAutorenameMode: TStreamAutorenameMode;
    procedure ReadHeader;
    function GetFileCount: Integer;
    function GetExtraInfoCount: Integer;
    function GetExtraInfoFieldNo(Index: Integer): TSFDSFileExtraInfoField;
  public
    {Creates a new instance of the reader.<BR>
    The StartOffset is 0 for normal SFDS files and another value for SFX files. Usually you get this value by a call to the SFDS_IsStorageFile/Stream[Ex] functions.<BR>
    If AssignUniqueNames then duplicate streams are renamed with unique names (some applications might need this).}
    constructor Create(SourceStream : TStream; StartOffset : Int64; FormatGUID : PGUID = nil; SupportedProc : TSFDSSupportedEvent = nil; OnReadFileEntry : TSFDSOnReadFileEntry = nil; AssignUniqueNames : Boolean = False; StreamAutorenameMode : TStreamAutorenameMode = sarmNone); reintroduce;
    destructor Destroy; override;
    {Raises an exception. This stream cannot read or write directly.}
    function Write(const Buffer; Count: Longint): Longint; override;
    {Raises an exception. This stream cannot read or write directly.}
    function Read(var Buffer; Count: Longint): Longint; override;
    {This is called internally.<BR>
    Descendents must override the Clone function for this object to function.<BR>
    The reader object requests a stream containing the data from the StartPos to the EndPos from the original stream used to create the reader.<BR>
    The overriden function should set the Mode to one of the values bellow, indicating what data the returning stream has:<BR>
    <UL>
    <LI>- csmFull: Return an original copy of the stream used to create the reader.</LI>
    <LI>- csmOnlyRequestedData: return a stream which has only the requested data.</LI>
    </UL>}
    function Clone(StartPos, EndPos : Int64; out Mode : TCloneStreamMode) : TStream; virtual; abstract;
    {This functions opens a stream containing the data associated with the FileName entry.<BR>
    If the data is compressed, the decompression will be done on the fly when reading data from the stream.<BR>
    The StreamSize will also be returned, and is preferable that you don't call result.size, since the size property calls seek, which means data needs to be decompressed (if it is compressed).<BR>
    If the stream is not found, the compression method used is unknown or the compressed data is corrupted then an exception is raised.<BR>
    You are responsable for freeing the returned object.}
    function OpenStream(FileName : AnsiString; out StreamSize : Int64) : TStream; overload;
    {This functions opens a stream containing the data associated with the FileIndex entry.<BR>
    If the data is compressed, the decompression will be done on the fly when reading data from the stream.<BR>
    The StreamSize will also be returned, and is preferable that you don't callresult.size, since the size property calls seek, which means data needs to be decompressed (if it is compressed).<BR>
    If the stream is not found, the compression method used is unknown or the compressed data is corrupted then an exception is raised.<BR>
    You are responsable for freeing the returned object.}
    function OpenStream(FileIndex : Integer; out StreamSize : Int64) : TStream; overload;
    {OpenStreamEx is called by OpenStream internally.<BR>
    It opens a stream based on a PSFDS_FileEntryRecord, and not a FileName or FileIndex.<BR>
    If the stream is not found, the compression method used is unknown or the compressed data is corrupted then an exception is raised.<BR>
    You are responsable for freeing the returned object.}
    function OpenStreamEx(Entry : PSFDS_FileEntryRecord; out StreamSize : Int64) : TStream;
    {Provides access to raw (compressed) stream data.<BR>
    If the stream is not found an exception is raised.<BR>
    You are responsable for freeing the returned object.}
    function OpenRAWStream(FileName : AnsiString) : TStream; overload;
    {Provides access to raw (compressed) stream data.<BR>
    If the stream is not found an exception is raised.<BR>
    You are responsable for freeing the returned object.}
    function OpenRAWStream(FileIndex : Integer) : TStream; overload;
    {Provides access to raw (compressed) stream data.<BR>
    If the stream is not found an exception is raised.<BR>    
    You are responsable for freeing the returned object.}
    function OpenRAWStream(Entry : PSFDS_FileEntryRecord) : TStream; overload;
    {Call it to extract or test a stream inside the SFDS file.<BR>
    FileName is the name of the stream.<BR>
    DiskFileName is the destination file on the disk (if not test).<BR>
    Test or extract the data.<BR>
    ProgressEvent is called to update the visuals.<BR>
    Set ThrowExceptions to True to throw exceptions on failures or False to check for errors in the result of the function.<BR>
    If ThrowExceptions is false, the result ca be: E_OK, E_ECREATE, E_EOPEN, E_NO_MEMORY, E_EREAD, E_EWRITE, E_BAD_ARCHIVE.}
    function ExtractFile(FileName : AnsiString; DiskFileName : AnsiString; Test : boolean = False; ProgressEvent : TSFDSProgressEvent = nil; ThrowExceptions : Boolean = True) : Integer; overload;
    {Call it to extract or test a stream inside the SFDS file.<BR>
    FileIndex is the index of the stream.<BR>
    DiskFileName is the destination file on the disk (if not test).<BR>
    Test or extract the data.<BR>
    ProgressEvent is called to update the visuals.<BR>
    Set ThrowExceptions to True to throw exceptions on failures or False to check for errors in the result of the function.<BR>
    If ThrowExceptions is false, the result ca be: E_OK, E_ECREATE, E_EOPEN, E_NO_MEMORY, E_EREAD, E_EWRITE, E_BAD_ARCHIVE.}
    function ExtractFile(FileIndex : Integer; DiskFileName : AnsiString; Test : boolean = False; ProgressEvent : TSFDSProgressEvent = nil; ThrowExceptions : Boolean = True) : Integer; overload;
    {Call it to extract or test a stream inside the SFDS file.<BR>
    FileName is the name of the stream.<BR>
    DiskFileName is the destination file on the disk (if not test).<BR>
    Stream is the destination stream (if not test).<BR>
    If ToDisk the data is written to DiskFileName else to Stream.<BR>
    Test or extract the data.<BR>
    ProgressEvent is called to update the visuals.<BR>
    Set ThrowExceptions to True to throw exceptions on failures or False to check for errors in the result of the function.<BR>
    If ThrowExceptions is false, the result ca be: E_OK, E_ECREATE, E_EOPEN, E_NO_MEMORY, E_EREAD, E_EWRITE, E_BAD_ARCHIVE.}
    function ExtractFileEx(FileName : AnsiString; DiskFileName : AnsiString; Stream : TStream; ToDisk : boolean; Test : boolean = False; ProgressEvent : TSFDSProgressEvent = nil; ThrowExceptions : Boolean = True) : Integer; overload;
    {Call it to extract or test a stream inside the SFDS file.<BR>
    FileIndex is the index of the stream.<BR>
    DiskFileName is the destination file on the disk (if not test).<BR>
    Stream is the destination stream (if not test).<BR>
    If ToDisk the data is written to DiskFileName else to Stream.<BR>
    Test or extract the data.<BR>
    ProgressEvent is called to update the visuals.<BR>
    Set ThrowExceptions to True to throw exceptions on failures or False to check for errors in the result of the function.<BR>
    If ThrowExceptions is false, the result ca be: E_OK, E_ECREATE, E_EOPEN, E_NO_MEMORY, E_EREAD, E_EWRITE, E_BAD_ARCHIVE.}
    function ExtractFileEx(FileIndex : Integer; DiskFileName : AnsiString; Stream : TStream; ToDisk : boolean; Test : boolean = False; ProgressEvent : TSFDSProgressEvent = nil; ThrowExceptions : Boolean = True) : Integer; overload;
    {Extract all files to DestinationDir or just Test all files.<BR>
    ProgressEvent is called to update the visuals.<BR>
    Set ThrowExceptions to True to throw exceptions on failures or False to check for errors in the result of the function.<BR>
    If ThrowExceptions is false, the result ca be: E_OK, E_ECREATE, E_EOPEN, E_NO_MEMORY, E_EREAD, E_EWRITE, E_BAD_ARCHIVE.}
    function ExtractFilesTo(DestinationDir : AnsiString; Test : boolean = False; ProgressEvent : TSFDSProgressEvent = nil; ThrowExceptions : Boolean = True) : Integer; overload;
    {Extract all files matching the mask to DestinationDir or just Test all files matching the mask.<BR>
    ProgressEvent is called to update the visuals.<BR>
    Set ThrowExceptions to True to throw exceptions on failures or False to check for errors in the result of the function.<BR>
    If ThrowExceptions is false, the result ca be: E_OK, E_ECREATE, E_EOPEN, E_NO_MEMORY, E_EREAD, E_EWRITE, E_BAD_ARCHIVE.}
    function ExtractFilesTo(DestinationDir, Mask : AnsiString; Test : boolean = False; ProgressEvent : TSFDSProgressEvent = nil; ThrowExceptions : Boolean = True) : Integer; overload;
    {Utility function to copy data from Source to Dest.<BR>
    DiskFileName and FileName are only required to fill in the ProgressEvent parameters.<BR>
    Use it to quickly copy RAWStream data to another stream.}
    function DirectStreamToStreamCopy(Source, Dest : TStream; const DiskFileName, FileName : AnsiString; ProgressEvent : TSFDSProgressEvent = nil; ThrowExceptions : Boolean = True) : Integer; overload;
    {Utility function to copy RAW Data to Dest.<BR>
    DiskFileName is only required to fill in the ProgressEvent parameters.<BR>
    Use it to quickly copy RAWStream data to another stream.}
    function DirectStreamToStreamCopy(const FileName : AnsiString; Dest : TStream; const DiskFileName : AnsiString; ProgressEvent : TSFDSProgressEvent = nil; ThrowExceptions : Boolean = True) : Integer; overload;
    {Utility function to copy RAW Data to Dest.<BR>
    DiskFileName is only required to fill in the ProgressEvent parameters.<BR>
    Use it to quickly copy RAWStream data to another stream.}
    function DirectStreamToStreamCopy(const FileIndex : Integer; Dest : TStream; const DiskFileName : AnsiString; ProgressEvent : TSFDSProgressEvent = nil; ThrowExceptions : Boolean = True) : Integer; overload;
    {Metadata: Title property.}
    property Title : AnsiString read FTitle;
    {Metadata: Subject property.}
    property Subject : AnsiString read FSubject;
    {Metadata: Author property.}
    property Author : AnsiString read FAuthor;
    {Metadata: Version property.}
    property Version : Longint read FVersion;
    {Metadata: Maker property.}
    property Maker : AnsiString read FMaker;
    {Metadata: MakerVersion property.}
    property MakerVersion : Longint read FMakerVersion;
    {Metadata: Keywords property.}
    property KeyWords : AnsiString read FKeyWords;
    {Metadata: Comments property.}
    property Comments : AnsiString read FComment;
    {Metadata: Metadata List.}
    property ExtraInfoFields : TSFDSFileExtraInfoList read FExtraInfoFields;
    {Metadata: Get the Index metadata property from the metadata list.}
    property ExtraInfoField[Index : Integer] : TSFDSFileExtraInfoField read GetExtraInfoFieldNo;
    {Metadata: The number of metadata properties.}
    property ExtraInfoCount : Integer read GetExtraInfoCount;
    {The file list. Read-Only.}
    property FileEntrys : TSFDS_FileEntrys read FFileEntrys;
    {The number of files inside. Read-Only.}
    property FileCount : Integer read GetFileCount;
    {The date and time when this file was created. Read-only.<BR>
    This is NOT the OS timestamp information.}
    property TimeCreate : TDateTime read FTimeCreated;
    {The combined size in bytes of all files inside the SFDS archive.}
    property AllFilesSize : Int64 read FAllFilesSize;
    {The internal compression format applied to the file list. Read-Only.}
    property InternalCompressionFormat : Byte read GetFInternalCompressionFormat;
    {Stream auto-rename mode used by the object.}
    property StreamAutorenameMode : TStreamAutorenameMode read FStreamAutorenameMode;
  end;

  {This is the base class for all customized SFDS streams writers.<BR>
  You will usually need to reintroduce the Create constructor and override the Destroy destructor.}
  TSFDSCustomWriter = class(TSFDSCustomSource)
  private
    FFileEntrys : TSFDS_FileEntrys;
    FClosed : Boolean;
    FThisEntry : PSFDS_FileEntryRecord;
    FDigest : TMD5Context;
    FFileSize : Int64;
    Compressor : TSFDSCompressor;
    FInternalWrittingMechanismActive : Boolean;
    FAppendSFXMode : TAppendSFXMode;
    procedure UpdateLastEntry;
  public
   {Creates a new instance of the writer.<BR>
   SourceStream is the stream where data is to be written.<BR>
   If the stream has SFX executable data then set Append to true and AppendSFXMode to method 1 or 2.<BR>
   FormatGUID is the desired guid of the file.<BR>
   Title, Subject, Author, Version, Maker, MakerVersion, KeyWords, Comments and ExtraInfoFields contains metadata information.}
    constructor Create(SourceStream : TStream; Append : Boolean = False; AppendSFXMode : TAppendSFXMode = aSfxNone; FormatGUID : PGUID = nil;
      Title : AnsiString = ''; Subject : AnsiString = ''; Author : AnsiString = '';
      Version : Longint = 0; Maker : AnsiString = ''; MakerVersion : Longint = 0;
      KeyWords : AnsiString = ''; Comments : AnsiString = '';
      ExtraInfoFields : TSFDSFileExtraInfoList = nil); reintroduce;
    destructor Destroy; override;
    {Call Close to flush all data buffers and finish the file.<BR>
    After this point the only valid call is to Free/Destroy.}
    procedure Close;
    {Calling Seek will raise an exception, because this stream is write-only and not seekable.}
    {$IFNDEF VERSION_5_OR_BELLOW}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    {$ENDIF}
    {Call OpenStream to open a new stream for writing.<BR>
    FileName is the name of the stream.<BR>
    FileAttributes are the stream attributes.<BR>
    Set CompressionFormat and CompressionLevel for data compression selection.<BR>
    ExtraDataStr, ExtraDataInt contains additional data about the stream.<BR>
    NOW you can call Write as many times as you want, to write data associated with the stream.}
    procedure OpenStream(FileName : AnsiString; FileAttributes : Integer = 0;
      CompressionFormat : Byte = 0; CompressionLevel : TSFDSCompressionLevel = clFastest; CompressionStrategy : TSFDSCompressionStrategy = csDefault;
      ExtraDataStr : AnsiString = ''; ExtraDataInt : Integer = 0);
    {Creates a reference to an existent stream in this sfds file (points to the same data).<BR>
    At this point any opened stream is closed and no more data can be written to it.<BR>
    LinkTo is the name of the existent stream and LinkName is the name of the link.<BR>
    For an application reading the file there is no way to know if the entry is a link to another stream.<BR>
    Returns True if a link is created or false if the LinkTo entry can't be located.}
    function CreateLink(LinkTo : AnsiString; LinkName : AnsiString; NewAttr : Integer = 0; NewExtraDataStr : AnsiString = ''; NewExtraDataInt : Integer = 0) : boolean;
    {Attempts to write Count bytes to the opened stream, and returns the number of bytes actually written.}
    function Write(const Buffer; Count: Longint): Longint; override;
    {Calling Read will raise an exception, because this stream is write-only and not seekable.}
    function Read(var Buffer; Count: Longint): Longint; override;
    {Call WriteFile to write the data from DiskFileName to the FileName stream.<BR>
    ProgressEvent is called to update the visuals.<BR>
    Set ThrowExceptions to True to throw exceptions on failures or False to check for errors in the result of the function.<BR>
    If ThrowExceptions is false, the result ca be: E_OK, E_ECREATE, E_EOPEN, E_NO_MEMORY, E_EREAD, E_EWRITE.}
    function WriteFile(DiskFileName, FileName : AnsiString; ProgressEvent : TSFDSProgressEvent = nil; FileAttributes : Integer = 0;
      CompressionFormat : Byte = 0; CompressionLevel : TSFDSCompressionLevel = clFastest; CompressionStrategy : TSFDSCompressionStrategy = csDefault;
      ExtraDataStr : AnsiString = ''; ExtraDataInt : Integer = 0; ThrowExceptions : Boolean = True) : Integer;
    {Call WriteFileEx to write the data from DiskFileName (FromDisk = True) or Stream (FromDisk = False) to the FileName stream.<BR>
    ProgressEvent is called to update the visuals.<BR>
    Set ThrowExceptions to True to throw exceptions on failures or False to check for errors in the result of the function.<BR>
    If ThrowExceptions is false, the result ca be: E_OK, E_ECREATE, E_EOPEN, E_NO_MEMORY, E_EREAD, E_EWRITE.}
    function WriteFileEx(DiskFileName, FileName : AnsiString; Stream : TStream; FromDisk : boolean; ProgressEvent : TSFDSProgressEvent = nil; FileAttributes : Integer = 0;
      CompressionFormat : Byte = 0; CompressionLevel : TSFDSCompressionLevel = clFastest; CompressionStrategy : TSFDSCompressionStrategy = csDefault;
      ExtraDataStr : AnsiString = ''; ExtraDataInt : Integer = 0; ThrowExceptions : Boolean = True) : Integer;
    {The internal compression format applied to the file list.}
    property InternalCompressionFormat : Byte read GetFInternalCompressionFormat write SetFInternalCompressionFormat;
    {The number of bytes written to the opened stream.<BR>
    It is updated after each call to Write.}
    property OpenedStreamSize : Int64 read FFileSize;
  end;

  {This class allows writing a SFDS file data directly to a memory stream.}
  TSFDSMemoryStreamWriter = class(TSFDSCustomWriter)
  private
    FMemoryStream : TMemoryStream;
  public
   {Creates a new instance of the writer.<BR>
   MemoryStream is the stream where data is to be written.<BR>
   If the stream has SFX executable data then set Append to true and AppendSFXMode to method 1 or 2.<BR>
   FormatGUID is the desired guid of the file.<BR>
   Title, Subject, Author, Version, Maker, MakerVersion, KeyWords, Comments and ExtraInfoFields contains metadata information.}
    constructor Create(MemoryStream : TMemoryStream; Append : Boolean = False; AppendSFXMode : TAppendSFXMode = aSfxNone; FormatGUID : PGUID = nil;
      Title : AnsiString = ''; Subject : AnsiString = ''; Author : AnsiString = '';
      Version : Longint = 0; Maker : AnsiString = ''; MakerVersion : Longint = 0;
      KeyWords : AnsiString = ''; Comments : AnsiString = '';
      ExtraInfoFields : TSFDSFileExtraInfoList = nil); reintroduce;
    destructor Destroy; override;
  end;

  {This class allows reading a SFDS file data directly from a memory stream.}
  TSFDSMemoryStreamReader = class(TSFDSCustomReader)
  private
    FMemoryStream : TMemoryStream;
    FAutoFreeMemoryStream : Boolean;
  public
    {MemoryStream is the stream passed in the Create constructor.}
    property MemoryStream : TMemoryStream read FMemoryStream;
    {Creates a new instance of the reader.<BR>
    The StartOffset is 0 for normal SFDS files and another value for SFX files. Usually you get this value by a call to the SFDS_IsStorageStream[Ex] function.<BR>
    If AutoFreeMemoryStream then the MemoryStream is freed by the destructor.}
    constructor Create(MemoryStream : TMemoryStream; StartOffset : Int64; AutoFreeMemoryStream : Boolean = False; FormatGUID : PGUID = nil; SupportedProc : TSFDSSupportedEvent = nil; OnReadFileEntry : TSFDSOnReadFileEntry = nil; AssignUniqueNames : Boolean = False; StreamAutorenameMode : TStreamAutorenameMode = sarmNone); reintroduce;
    destructor Destroy; override;
    {Clone is called internally.}
    function Clone(StartPos, EndPos : Int64; out Mode : TCloneStreamMode) : TStream; override;
  end;

  {This class allows reading a SFDS file data directly from a resource stream.}
  TSFDSResourceStreamReader = class(TSFDSCustomReader)
  private
    FResourceStream : TResourceStream;
    FInstance : THandle;
    FResName : AnsiString;
    FResType : PChar;
    FResID : Integer;
    FUseResName : Boolean;
  public
    {ResourceStream is the stream created using the information passed in the Create or CreateFromID constructor.}
    property ResourceStream : TResourceStream read FResourceStream;
    {Creates a new instance of the reader.<BR>
    The StartOffset is 0 for normal SFDS files and another value for SFX files. Usually you get this value by a call to the SFDS_IsStorageStream[Ex] function.<BR>
    The Instance parameter is the instance handle associated with the executable or shared library that contains the resource.<BR>
    The ResName parameter is the name of the resource.<BR>
    The ResType parameter is a string identifying the type of the resource.}
    constructor Create(Instance: THandle; const ResName: AnsiString; ResType: PChar; StartOffset : Int64; FormatGUID : PGUID = nil; SupportedProc : TSFDSSupportedEvent = nil; OnReadFileEntry : TSFDSOnReadFileEntry = nil; AssignUniqueNames : Boolean = False; StreamAutorenameMode : TStreamAutorenameMode = sarmNone); reintroduce;
    {Creates a new instance of the reader.<BR>
    The StartOffset is 0 for normal SFDS files and another value for SFX files. Usually you get this value by a call to the SFDS_IsStorageStream[Ex] function.<BR>
    The Instance parameter is the instance handle associated with the executable or shared library that contains the resource.<BR>
    The ResID parameter is the ID of the resource.<BR>
    The ResType parameter is a string identifying the type of the resource.}
    constructor CreateFromID(Instance: THandle; ResID: Integer; ResType: PChar; StartOffset : Int64; FormatGUID : PGUID = nil; SupportedProc : TSFDSSupportedEvent = nil; OnReadFileEntry : TSFDSOnReadFileEntry = nil; AssignUniqueNames : Boolean = False; StreamAutorenameMode : TStreamAutorenameMode = sarmNone);
    destructor Destroy; override;
    {Clone is called internally.}
    function Clone(StartPos, EndPos : Int64; out Mode : TCloneStreamMode) : TStream; override;
  end;

  {This class allows reading a SFDS file data from a file on disk.}
  TSFDSFileReader = class(TSFDSCustomReader)
  private
    FFileStream : TFileStream;
  public
    {FileStream is the stream created using the information passed in the Create constructor.}
    property FileStream : TFileStream read FFileStream;
    {Creates a new instance of the reader.<BR>
    The StartOffset is 0 for normal SFDS files and another value for SFX files. Usually you get this value by a call to the SFDS_IsStorageFile[Ex] function.<BR>
    FileName is the name of the file on disk.}
    constructor Create(const FileName : AnsiString; StartOffset : Int64; FormatGUID : PGUID = nil; SupportedProc : TSFDSSupportedEvent = nil; OnReadFileEntry : TSFDSOnReadFileEntry = nil; AssignUniqueNames : Boolean = False; StreamAutorenameMode : TStreamAutorenameMode = sarmNone); reintroduce;
    destructor Destroy; override;
    {Clone is called internally.}
    function Clone(StartPos, EndPos : Int64; out Mode : TCloneStreamMode) : TStream; override;
  end;

  {This class allows writing a SFDS file data to a file on disk.}
  TSFDSFileWriter = class(TSFDSCustomWriter)
  private
    FFileStream : TFileStream;
  public
   {Creates a new instance of the writer.<BR>
   FileName is the name of the file on disk where data is to be written.<BR>
   If the file has SFX executable data then set Append to true and AppendSFXMode to method 1 or 2.<BR>
   FormatGUID is the desired guid of the file.<BR>
   Title, Subject, Author, Version, Maker, MakerVersion, KeyWords, Comments and ExtraInfoFields contains metadata information.}
    constructor Create(const FileName : AnsiString; Append : Boolean = False; AppendSFXMode : TAppendSFXMode = aSfxNone; FormatGUID : PGUID = nil;
      Title : AnsiString = ''; Subject : AnsiString = ''; Author : AnsiString = '';
      Version : Longint = 0; Maker : AnsiString = ''; MakerVersion : Longint = 0;
      KeyWords : AnsiString = ''; Comments : AnsiString = '';
      ExtraInfoFields : TSFDSFileExtraInfoList = nil); reintroduce;
    destructor Destroy; override;
  end;

  {This function checks if a file is a SFDS archive, and if it is it returns True and the ATPos where the SFDS data begins (this is non-zero if the file is a SFX archive).}
  function SFDS_IsStorageFile(SFDS_FileName : AnsiString; out ATPos : Int64) : boolean;
  {This function checks if a file is a SFDS archive, and if it is it returns True and the ATPos where the SFDS data begins (this is non-zero if the file is a SFX archive). It also returns the GUID of the file.}
  function SFDS_IsStorageFileEx(SFDS_FileName : AnsiString; out ATPos : Int64; out GUID : TGUID) : boolean;
  {This function checks if a stream has a SFDS archive data, and if it has it returns True and the ATPos where the SFDS data begins (this is non-zero if the stream is a SFX archive).}
  function SFDS_IsStorageStream(SourceStream : TStream; out ATPos : Int64) : boolean;
  {This function checks if a stream has a SFDS archive data, and if it has it returns True and the ATPos where the SFDS data begins (this is non-zero if the stream is a SFX archive). It also returns the GUID of the file.}
  function SFDS_IsStorageStreamEx(SourceStream : TStream; out ATPos : Int64; out GUID : TGUID) : boolean;
  {Returns the number of TSFDSxxxReader objects currently active.}
  function SFDS_ActiveReadersCount : Integer;
  {Returns the index TSFDSxxxReader object which is currently active.}
  function SFDS_Readers(Index : Integer) : TSFDSCustomReader;
  {Returns the TSFDSxxxReader object which is currently active, searching it by its name.
  If no such reader is active, it returns nil.}
  function SFDS_Find_ActiveReader_By_SFDSFileName(SFDSFileName : AnsiString) : TSFDSCustomReader;
  {Creates a new file reader object from a file located on disk.<BR>
  If the file is not found, or it is not a SFDS valid file, it returns nil.<BR>
  If AlwaysOpenNew then it creates a new object even if one is already active, otherwise it returns the one that is active.}
  function SFDS_CreateReader(SFDSFileName : AnsiString; AlwaysOpenNew : boolean = False; StreamAutorenameMode : TStreamAutorenameMode = sarmNone) : TSFDSCustomReader;
  {Try opening the StreamName which can be located on any opened reader or only in the Optional_SFDSFileName.<BR>
  It is used internally by the SFDS_OpenStreamIndirect function which is recommended to be used.}
  function SFDS_OpenFirstStream(StreamName : AnsiString; out Stream : TStream; out StreamSize : Int64; Optional_SFDSFileName : AnsiString = ''; SearchOnlyIn_Optional_SFDSFileName : boolean = False; StreamAutorenameMode : TStreamAutorenameMode = sarmNone) : boolean;
  {SFDSFileNameAndStreamName must be in format: SFDSFileName::StreamName.<BR>
   Ex: c:\Data.SFDS::Texts\Readme.txt - Specified SFDS file and stream name.<BR>
   Ex: Texts\Readme.txt - Searches all opened SFDS files for the first stream with that name.}
  function SFDS_OpenStreamIndirect(SFDSFileNameAndStreamName : AnsiString; out Stream : TStream; out StreamSize : Int64; StreamAutorenameMode : TStreamAutorenameMode = sarmNone) : boolean;

  type
  {Used in search functions.<BR>
  FileData is the data of the stream found in the SFDSFileName.}
  TSFDSFileData = record
    FileData : SFDS_FileEntryRecord;
    SFDSFileName : AnsiString;
  end;

  {Searches for the first instance of a stream name in a specified SFDS file, or in all opened SFDS files.<BR>
   Returns 0 on error or non zero on succes (non zero = the result is a search handle to use with SFDS_FindNext and SFDS_FindClose).<BR>
   Ex:  SearchHandle := SFDS_FindFirst('*.txt', 'TEXTS.sfds' , FDAT);  //Search for text files only in Texts.sfds.<BR>
   Ex:  SearchHandle := SFDS_FindFirst('*.*', '' , FDAT);  //Find all files from all opened SFDS files.<BR>
   Ex:  SearchHandle := SFDS_FindFirst('*.txt;*.bmp;*.jpg', '' , FDAT);  //Find txt, bmp, jpg files from all opened SFDS files.}
  function SFDS_FindFirst(const StreamName, SFDSFileName : AnsiString; out FileData : TSFDSFileData): PtrUInt;
  {Returns 0 if a file was successfully located, otherwise, it returns an error code.<BR>
   FileData returns the next entry matching the stream name specified in a previous call to SFDS_FindFirst.}
  function SFDS_FindNext(Handle : PtrUInt; out FileData : TSFDSFileData): Integer;
  {Releases memory allocated by SFDS_FindFirst.}
  procedure SFDS_FindClose(Handle : PtrUInt);

  {Returns ChunkSize used for allocating reading/writing buffer}
  function SFDS_ChunkSize : Longint;
  {Sets ChunkSize used for allocating reading/writing buffer<BR>Minimum is 8KB<BR>Maximum is 10MB<BR>Default is 32KB}
  procedure SFDS_Set_ChunkSize(NewChunkSize : Longint);


  const
  cfNone  = 0;
  cfZlib  = 1;
  cfBzip2 = 2;

  {Converts from Source Pointer of SourceSize to a string.<BR>
  Use it to translate TSFDSFileExtraInfoField.ValueString or TSFDSFileExtraInfoField.ValueBinary}
  function Pointer2String(Source : Pointer; SourceSize : Integer) : AnsiString;
  {Copies data from Source to a new location in memory and returns it in Dest.<BR>
  Dest must be NIL.<BR>
  Use it to set TSFDSFileExtraInfoField.ValueString or TSFDSFileExtraInfoField.ValueBinary, and don't forget to also set TSFDSFileExtraInfoField.ValueStringSize or TSFDSFileExtraInfoField.ValueBinarySize}
  procedure String2Pointer(Source : AnsiString; out Dest : Pointer);

implementation
uses sfds_streamingutils, sfds_searchutils, sfds_consts, sfds_platformutils;

  //Chunk size for reading/writing buffer
  var SFDS_VARChunkSize : Longint = DEFAULT_SFDS_ChunkSize;

  function SFDS_ChunkSize : Longint;
  begin
    result := SFDS_VARChunkSize;
  end;

  procedure SFDS_Set_ChunkSize(NewChunkSize : Longint);
  begin
    if NewChunkSize <= MIN_SFDS_ChunkSize
      then
        SFDS_VARChunkSize := MIN_SFDS_ChunkSize
      else
        if NewChunkSize >= MAX_SFDS_ChunkSize
          then
            SFDS_VARChunkSize := MAX_SFDS_ChunkSize
          else
            SFDS_VARChunkSize := NewChunkSize;
  end;

  function Pointer2String(Source : Pointer; SourceSize : Integer) : AnsiString;
  begin
    SetLength(result, SourceSize);
    System.Move(Source^, result[1], SourceSize);
  end;

  procedure String2Pointer(Source : AnsiString; out Dest : Pointer);
  var SourceSize : Integer;
  begin
    SourceSize := Length(Source);
    GetMem(Dest, SourceSize);
    System.Move(Source[1], Dest^, SourceSize);
  end;

{$IFDEF VERSION_5_OR_BELLOW}
function IsEqualGUID(const guid1, guid2: TGUID): Boolean; stdcall;
external 'ole32.dll' name 'IsEqualGUID';
{$EXTERNALSYM IsEqualGUID}
{$ENDIF}

  var SFDS_DefaultSupportedProc : TSFDSSupportedEvent = nil;

  type
  PSFDSFileReaderEntry = ^TSFDSCustomReaderEntry;
  TSFDSCustomReaderEntry = record
    Reader : TSFDSCustomReader;
  end;

  TSFDS_ActiveReaders = class(TList)
  private
    function GetS(Index: Integer): TSFDSCustomReader;
    procedure FreeAllReaders;
  public
    destructor Destroy; override;
    function AddReader(Reader : TSFDSCustomReader) : Integer;
    function FindFile(FileName : AnsiString) : TSFDSCustomReader;
    procedure Remove(Reader : TSFDSCustomReader);
    property Readers[Index: Integer]: TSFDSCustomReader read GetS;
  end;

  var ActiveReaders : TSFDS_ActiveReaders;

  type
  PSFDS_FindInfo = ^TSFDS_FindInfo;
  TSFDS_FindInfo = record
    StreamName : AnsiString;
    SFDSFileName : AnsiString;
    FileIndex : Int64;
    ReaderIndex : Integer;
  end;

function SFDS_NextMatch(SearchInfo : PSFDS_FindInfo;
  out FileData : TSFDSFileData) : Integer;
var SF : TSFDSCustomReader;
    Found : Boolean;
label 1;
begin
try
  result := 0;
  if (SearchInfo^.SFDSFileName <> '') then
     begin
       SF := SFDS_CreateReader(SearchInfo^.SFDSFileName);
       if SF = nil then
          begin
            result := SFDS_ERROR_NO_MORE_FILES;
            Exit;
          end;
       if SF.FileCount - 1 < SearchInfo^.FileIndex then
          begin
            result := SFDS_ERROR_NO_MORE_FILES;
            exit;
          end;
       Found := False;
       repeat
       if WildcardMatchFile(SF.FileEntrys.Entrys[SearchInfo^.FileIndex]^.FileName,
          SearchInfo^.StreamName) then
          begin
            FileData.FileData := SF.FileEntrys.Entrys[SearchInfo^.FileIndex]^;
            FileData.SFDSFileName := SearchInfo^.SFDSFileName;
            Found := True;
          end;
       Inc(SearchInfo^.FileIndex);
       until (SearchInfo^.FileIndex > SF.FileCount - 1) or Found;
       if (not Found) then
          begin
            result := SFDS_ERROR_NO_MORE_FILES;
            Exit;
          end;
     end
     else
     begin
      1:SF := SFDS_Readers(SearchInfo^.ReaderIndex);
      if SF = nil then
         begin
          result := SFDS_ERROR_NO_MORE_FILES;
          Exit;
         end;
      if ((SearchInfo^.FileIndex > (SF.FileCount - 1)) and
         (SearchInfo^.ReaderIndex > SFDS_ActiveReadersCount - 1)) then
          begin
            result := SFDS_ERROR_NO_MORE_FILES;
            Exit;
          end;
      if (SearchInfo^.FileIndex > (SF.FileCount - 1)) then
         begin
         SearchInfo^.FileIndex := 0;
         Inc(SearchInfo^.ReaderIndex);
         goto 1;
         end;
       Found := False;
       repeat
       if WildcardMatchFile(SF.FileEntrys.Entrys[SearchInfo^.FileIndex]^.FileName,
            SearchInfo^.StreamName) then
          begin
            FileData.FileData := SF.FileEntrys.Entrys[SearchInfo^.FileIndex]^;
            FileData.SFDSFileName := SF.FSFileName;
            Found := True;
          end;
       Inc(SearchInfo^.FileIndex);
       until (SearchInfo^.FileIndex > SF.FileCount - 1) or Found;
       if (not Found) then
          begin
            goto 1;
          end;
     end;
except
  result := SFDS_INVALID_HANDLE_VALUE;
end;
end;

function CStarFix(S : AnsiString) : AnsiString;
begin
  result := S;
  if Length(S) <= 3 then Exit;
  if Copy(S, Length(S) - 2, 3) = '*.*' then
     begin
       Delete(Result, Length(S) - 2, 3);
       Result := Result + '*';
     end;
end;

function SFDS_FindFirst(const StreamName, SFDSFileName : AnsiString;
  out FileData : TSFDSFileData): PtrUInt;
var SearchInfo : PSFDS_FindInfo;
begin
 result := 0;
 try
  New(SearchInfo);
  SearchInfo^.StreamName := CStarFix(StreamName);
  SearchInfo^.SFDSFileName := SFDSFileName;
  SearchInfo^.FileIndex := 0;
  SearchInfo^.ReaderIndex := 0;
  result := PtrUInt(Pointer(SearchInfo));
  if SFDS_NextMatch(SearchInfo, FileData) <> 0 then
     begin
     SFDS_FindClose(result);
     result := 0;
     end;
 except
 end;
end;

function SFDS_FindNext(Handle : PtrUInt; out FileData : TSFDSFileData): Integer;
begin
try
  if PSFDS_FindInfo(Handle) = nil then
     begin
       result := SFDS_INVALID_HANDLE_VALUE;
       Exit;
     end;
  result := SFDS_NextMatch(PSFDS_FindInfo(Handle), FileData);
except
  result := SFDS_INVALID_HANDLE_VALUE;
end;
end;

procedure SFDS_FindClose(Handle : PtrUInt);
var SearchInfo : PSFDS_FindInfo;
begin
try
  SearchInfo := PSFDS_FindInfo(Handle);
  if SearchInfo <> nil then Dispose(SearchInfo);
except
end;
end;

procedure SFDS_SupportedAlways(Sender : TSFDSCustomReader; GUID : TGUID; var Supported : Boolean);
begin
  Supported := True;
end;

function SFDS_OpenStreamIndirect(SFDSFileNameAndStreamName : AnsiString;
  out Stream : TStream; out StreamSize : Int64;
  StreamAutorenameMode : TStreamAutorenameMode = sarmNone) : boolean;
{
SFDSFileNameAndStreamName must be in format:
SFDSFileName::StreamName
Ex: c:\Data.SFDS::Texts\Readme.txt
or
Ex: Texts\Readme.txt || Searches all opened SFDS files for the first stream with that name...
}
var SFDSFileName, StreamName : AnsiString;
    P : Integer;
    S : AnsiString;
begin
  try
  S := SFDSFileNameAndStreamName;
  P := AnsiPos('::', S);
  if P = 0 then
     begin
      result := SFDS_OpenFirstStream(S, Stream, StreamSize, '', False, StreamAutorenameMode);
     end
     else
     begin
      SFDSFileName := Copy(S, 1, P - 1);
      Delete(S, 1, P + 1);
      StreamName := S;
      result := SFDS_OpenFirstStream(StreamName, Stream, StreamSize, SFDSFileName, True, StreamAutorenameMode);
     end;
  except
    result := False;
    Stream := nil;
    StreamSize := 0;
  end;
end;

function SFDS_OpenFirstStream(StreamName : AnsiString; out Stream : TStream;
  out StreamSize : Int64; Optional_SFDSFileName : AnsiString = '';
  SearchOnlyIn_Optional_SFDSFileName : boolean = False;
  StreamAutorenameMode : TStreamAutorenameMode = sarmNone) : boolean;
var SF : TSFDSCustomReader;
    I : Integer;
begin
  result := False;
  Stream := nil;
  StreamSize := 0;
 try
  if (Optional_SFDSFileName <> '') then
     begin
       SFDS_CreateReader(Optional_SFDSFileName, False, StreamAutorenameMode);
     end;
  if SearchOnlyIn_Optional_SFDSFileName and (Optional_SFDSFileName <> '') then
     begin
       SF := SFDS_Find_ActiveReader_By_SFDSFileName(Optional_SFDSFileName);
       if SF = nil then Exit;
       if not SF.FileEntrys.FileExists(StreamName) then exit;
       Stream := SF.OpenStream(StreamName, StreamSize);
       result := True;
     end
     else
     begin
         SF := SFDS_Find_ActiveReader_By_SFDSFileName(Optional_SFDSFileName);
         if Assigned(SF) then
           if SF.FileEntrys.FileExists(StreamName) then
              begin
              Stream := SF.OpenStream(StreamName, StreamSize);
              result := True;
              Exit;
              end;
         for I := 0 to SFDS_ActiveReadersCount - 1 do
             begin
               SF := SFDS_Readers(I);
                 if Assigned(SF) then
                   if SF.FileEntrys.FileExists(StreamName) then
                      begin
                      Stream := SF.OpenStream(StreamName, StreamSize);
                      result := True;
                      Exit;
                      end;
           end;
     end;
 except
  result := False;
  Stream := nil;
  StreamSize := 0;
 end;
end;

function SFDS_CreateReader(SFDSFileName : AnsiString; AlwaysOpenNew : boolean = False; StreamAutorenameMode : TStreamAutorenameMode = sarmNone) : TSFDSCustomReader;

  function InternalOpenNew(ASFDSFileName : AnsiString) : TSFDSCustomReader;
  var OFS : Int64;
  begin
    result := nil;
    if not SFDS_IsStorageFile(ASFDSFileName, OFS) then exit;
    result := TSFDSFileReader.Create(ASFDSFileName, OFS, nil, nil, nil, False, StreamAutorenameMode);
  end;

begin
  result := nil;
  try
  if AlwaysOpenNew then
     begin
       result := InternalOpenNew(SFDSFileName);
     end
     else
     begin
       result := SFDS_Find_ActiveReader_By_SFDSFileName(SFDSFileName);
       if result = nil then
          begin
            result := InternalOpenNew(SFDSFileName);
          end;
     end;
  except
  end;
end;

function SFDS_ActiveReadersCount : Integer;
begin
  result := ActiveReaders.Count;
end;

function SFDS_Readers(Index : Integer) : TSFDSCustomReader;
begin
  result := ActiveReaders.Readers[Index];
end;

function SFDS_Find_ActiveReader_By_SFDSFileName(SFDSFileName : AnsiString) : TSFDSCustomReader;
begin
  result := ActiveReaders.FindFile(SFDSFileName);
end;

procedure SFDS_Error(const ErrorMessage : AnsiString; const Args : array of const);
begin
  raise ESFDSException.CreateFmt(ErrorMessage, Args);
end;

function MD5Stream(Str : TStream) : TMD5Digest;
var
  Context: TMD5Context;
  DataBuffer : Pointer;
  Len : LongInt;
begin
  MD5Init(Context);
  GetMem(DataBuffer, DEFAULT_SFDS_ChunkSize);
  try
   repeat
   Len := Str.Read(DataBuffer^, DEFAULT_SFDS_ChunkSize);
   if Len > 0 then MD5Update(Context, DataBuffer^, Len);
   until Len < DEFAULT_SFDS_ChunkSize;
  finally
   FreeMem(DataBuffer, DEFAULT_SFDS_ChunkSize);
  end;
  Result := MD5Final(Context);
end;

type
  TSFDSGenericCompressionStream = class(TStream)
  private
    FContext : TMD5Context;
    FSrcStr : TStream;
  public
    constructor Create(Source : TStream); reintroduce;
    destructor Destroy; override;
    procedure Init;
    function Write(const buffer; count: Longint): Longint; override;
    function Read(var buffer; count: Longint): Longint; override;
    {$IFNDEF VERSION_5_OR_BELLOW}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    {$ENDIF}
    property MD5Context : TMD5Context read FContext;
  end;

  TSFDSCopyStream = class(TStream)
  private
    FContext : TMD5Context;
    FSrcStr : TStream;
  public
    constructor Create(Source : TStream); reintroduce;
    destructor Destroy; override;
    procedure Init;
    function Write(const buffer; count: Longint): Longint; override;
    function Read(var buffer; count: Longint): Longint; override;
    {$IFNDEF VERSION_5_OR_BELLOW}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    {$ENDIF}
    property MD5Context : TMD5Context read FContext;
  end;

constructor TSFDSGenericCompressionStream.Create(Source : TStream);
begin
  if Source = nil then raise ESFDSNilSourceStream.Create(SFDSNilStream);
  inherited Create;
  FSrcStr := Source;
end;

destructor TSFDSGenericCompressionStream.Destroy;
begin
  FSrcStr.Free;
  FSrcStr := nil;
  inherited Destroy;
end;

procedure TSFDSGenericCompressionStream.Init;
begin
  MD5Init(FContext);
end;

function TSFDSGenericCompressionStream.Write(const buffer; count: Longint): Longint;
begin
  result := FSrcStr.Write(buffer, count);
  MD5Update(FContext, buffer, count);
end;

function TSFDSGenericCompressionStream.Read(var buffer; count: Longint): Longint;
begin
  raise ESFDSInvalidStreamOperation.Create(SFDSWriteOnly);
end;

{$IFNDEF VERSION_5_OR_BELLOW}
function TSFDSGenericCompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TSFDSGenericCompressionStream.Seek(Offset: LongInt; Origin: Word): LongInt;
{$ENDIF}
begin
  result := FSrcStr.Seek(Offset, Origin);
end;

constructor TSFDSCopyStream.Create(Source : TStream);
begin
  if Source = nil then raise ESFDSNilSourceStream.Create(SFDSNilStream);
  inherited Create;
  FSrcStr := Source;
end;

destructor TSFDSCopyStream.Destroy;
begin
  FSrcStr := nil;
  inherited Destroy;
end;

procedure TSFDSCopyStream.Init;
begin
  MD5Init(FContext);
end;

function TSFDSCopyStream.Write(const buffer; count: Longint): Longint;
begin
  result := FSrcStr.Write(buffer, count);
  MD5Update(FContext, buffer, count);
end;

function TSFDSCopyStream.Read(var buffer; count: Longint): Longint;
begin
  raise ESFDSInvalidStreamOperation.Create(SFDSWriteOnly);
end;

{$IFNDEF VERSION_5_OR_BELLOW}
function TSFDSCopyStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TSFDSCopyStream.Seek(Offset: LongInt; Origin: Word): LongInt;
{$ENDIF}
begin
  result := FSrcStr.Seek(Offset, Origin);
end;

{}

function SFDS_IsStorageFile(SFDS_FileName : AnsiString; out ATPos : Int64) : boolean;
var G : TGUID;
begin
  result := SFDS_IsStorageFileEx(SFDS_FileName, ATPos, G);
end;

function SFDS_IsStorageStream(SourceStream : TStream; out ATPos : Int64) : boolean;
var G : TGUID;
begin
  result := SFDS_IsStorageStreamEx(SourceStream, ATPos, G);
end;

function SFDS_IsStorageFileEx(SFDS_FileName : AnsiString; out ATPos : Int64; out GUID : TGUID) : boolean;
var F : TFileStream;
begin
  AtPos := -1;
  result := False;
  F := nil;  
  if not FileExists(SFDS_FileName) then exit;
  try
   try
     F := TFileStream.Create(SFDS_FileName, fmOpenRead or fmShareDenyWrite);
     result := SFDS_IsStorageStreamEx(F, ATPos, GUID);
   finally
     F.Free;
   end;
  except
  end;
end;

function SFDS_IsStorageStreamEx(SourceStream : TStream; out ATPos : Int64; out GUID : TGUID) : boolean;
var SNP : Int64;
    Ps : Int64;
label 1;
const
  EMPTYGUID : TGUID = '{00000000-0000-0000-0000-000000000000}';

  function Internal_IS_SFDSFormat(StartPos : Int64) : boolean;
  var TempStr : AnsiString;
      TempBool : Boolean;
      L, TempInt : Longint;
  begin
  result := True;
  SourceStream.Seek(StartPos, {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF});
  L := Length(SFDS_SIGNATURE_STR);
  SetString(TempStr, PChar(nil), L);
  SourceStream.Read(PChar(TempStr)^, L);
  if not SameText(SFDS_SIGNATURE_STR, TempStr) then
     begin
     result := False;
     Exit;
     end;

  SourceStream.Read(TempInt, SizeOf(LongInt));
  if TempInt <> SFDS_SIGNATURE_INT then
     begin
     result := False;
     Exit;
     end;

  SourceStream.Read(Guid, SizeOf(TGuid));
  SourceStream.Read(TempBool, SizeOf(Boolean));
  if not TempBool then
     begin
     result := False;
     Exit;
     end;
  end;

  function IS_SEA(StartPos : Int64) : Int64;
  var SI : LongInt;
  begin
  result := -1;
  SourceStream.Seek(StartPos + SFDS_SELFEXTRHDRPOS, {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF});
  SourceStream.Read(SI, SizeOf(LongInt));
  if SI = SFDS_SIGNATURE_INT then
     begin
       SourceStream.Read(result, SizeOf(Int64));
     end;

  if result = -1 then
     begin
        SourceStream.Seek(SourceStream.Size - SizeOf(LongInt) - SizeOf(Int64), {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF});
        SourceStream.Read(SI, SizeOf(LongInt));
        if SI = SFDS_SIGNATURE_INT then
           begin
             SourceStream.Read(result, SizeOf(Int64));
           end;
     end;
  end;

  procedure NoMatch;
  begin
   result := False;
   ATPos := -1;
   GUID := EMPTYGUID;
  end;

begin
  ATPos := -1;
  GUID := EMPTYGUID;
  result := False;
  SNP := 0;
  if SourceStream = nil then raise ESFDSNilSourceStream.Create(SFDSNilStream);

  PS := SourceStream.Position;
  try
  ATPos := PS;
  result := Internal_IS_SFDSFormat(ATPos);
  except
    NoMatch;
  end;
  if result then exit;

  try
  ATPos := 0;
  result := Internal_IS_SFDSFormat(ATPos);
  except
    NoMatch;
  end;
  if result then exit;

  GUID := EMPTYGUID;
  try
  SNP := IS_SEA(PS);
  except
    NoMatch;
  end;
  if SNP > 0 then goto 1;


  GUID := EMPTYGUID;
  try
  SNP := IS_SEA(0);
  except
    NoMatch;
  end;

1: if SNP > 0 then
     begin
       ATPos := SNP;
        try
         result := Internal_IS_SFDSFormat(ATPos);
        except
         result := False;
        end;
       if not result then
                     begin
                       NoMatch;
                     end;
     end
     else NoMatch;
end;

{ TSelectiveFileStream }

constructor TSelectiveStream.Create(SourceStream : TStream; StartPosition,
  EndPosition : Int64; FreeSourceOnDestroy : Boolean = True);
begin
  if SourceStream = nil then raise ESFDSNilSourceStream.Create(SFDSNilStream);
  inherited Create;  
  FSourceStream := SourceStream;
  FFreeSourceOnDestroy := FreeSourceOnDestroy;
  FStartPos := StartPosition;
  FEndPos := EndPosition;
  FPosition := 0;
  FNewSize := FEndPos - FStartPos;
  Seek(0, {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF});
end;

destructor TSelectiveStream.Destroy;
begin
  if FreeSourceOnDestroy then if Assigned(SourceStream) then SourceStream.Free;
  FSourceStream := nil;
  inherited Destroy;
end;

function TSelectiveStream.NewGetSize: Int64;
begin
  result := FNewSize;
end;

function TSelectiveStream.Read(var Buffer; Count: Longint): Longint;
var NewCount : Integer;
begin
  NewCount := Count;
  if Count > NewGetSize - FPosition then NewCount := NewGetSize - FPosition;
  result := SourceStream.Read(Buffer, NewCount);
  FPosition := FPosition + Result;
end;

{$IFNDEF VERSION_5_OR_BELLOW}
function TSelectiveStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TSelectiveStream.Seek(Offset: LongInt; Origin: Word): LongInt;
{$ENDIF}
var NewOffset, TmpOffSet : Int64;
begin
   NewOffset := 0;
   TmpOffSet := Offset;
   case Origin of
   {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF} :
       begin
       if TmpOffSet > NewGetSize then TmpOffSet := NewGetSize;
       NewOffset := FStartPos + TmpOffSet;
       end;
   {$IFNDEF VERSION_5_OR_BELLOW}soCurrent{$ELSE}soFromCurrent{$ENDIF} :
       begin
       if TmpOffSet > NewGetSize - FPosition then TmpOffSet := NewGetSize - FPosition;
       NewOffset := TmpOffSet;
       end;
   {$IFNDEF VERSION_5_OR_BELLOW}soEnd{$ELSE}soFromEnd{$ENDIF} :
       begin
       if TmpOffSet > NewGetSize then TmpOffSet := NewGetSize;
       Origin := {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF};
       NewOffset := FEndPos + TmpOffSet;
       end;
   end;
   result := SourceStream.Seek(NewOffset, Origin);
   result := result - FStartPos;
   FPosition := result;
end;

procedure TSelectiveStream.SetSize(NewSize: Integer);
begin
  raise ESFDSInvalidStreamOperation.Create(SFDSSetSizeErr);
end;

{$IFNDEF VERSION_5_OR_BELLOW}
procedure TSelectiveStream.SetSize(const NewSize: Int64);
begin
  raise ESFDSInvalidStreamOperation.Create(SFDSSetSizeErr);
end;
{$ENDIF}

function TSelectiveStream.Write(const Buffer; Count: Longint): Longint;
var NewCount : Integer;
begin
  NewCount := Count;
  if Count > NewGetSize - FPosition then NewCount := NewGetSize - FPosition;
  result := SourceStream.Write(Buffer, NewCount);
  FPosition := FPosition + Result;
end;

{ TSFDS_FileEntrys }

function TSFDS_FileEntrys.AddEntry(Entry: SFDS_FileEntryRecord) : Integer;
var
  NewRec: PSFDS_FileEntryRecord;
begin
  New(NewRec);
  with NewRec^ do
       begin
       FileName := Entry.FileName;
       OriginalFileName := Entry.OriginalFileName;       
       FileSize := Entry.FileSize;
       FileAttributes := Entry.FileAttributes;
       StartPosition := Entry.StartPosition;
       EndPosition := Entry.EndPosition;
       CompressionLevel := Entry.CompressionLevel;
       CompressionFormat := Entry.CompressionFormat;
       MD5Digest := Entry.MD5Digest;
       ExtraDataStr := Entry.ExtraDataStr;
       ExtraDataInt := Entry.ExtraDataInt;
       end;
  result := Add(NewRec);
end;

destructor TSFDS_FileEntrys.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Dispose(PSFDS_FileEntryRecord(Items[I]));
  inherited Destroy;
end;

function TSFDS_FileEntrys.FileAttributes(FileName: AnsiString): Integer;
var
  Entry : PSFDS_FileEntryRecord;
begin
  result := 0;
  Entry := FindEntry(FileName);
  if Entry <> nil then result := Entry^.FileAttributes;
end;

function TSFDS_FileEntrys.FileExists(FileName: AnsiString): Boolean;
begin
  result := FindEntry(FileName) <> nil;
end;

function TSFDS_FileEntrys.FileOriginalExists(OriginalFileName: AnsiString): Boolean;
begin
  result := FindEntryOriginal(OriginalFileName) <> nil;
end;

function TSFDS_FileEntrys.FileSize(FileName: AnsiString): Int64;
var
  Entry : PSFDS_FileEntryRecord;
begin
  result := 0;
  Entry := FindEntry(FileName);
  if Entry <> nil then result := Entry^.FileSize;
end;

function TSFDS_FileEntrys.FindEntry(FileName: AnsiString): PSFDS_FileEntryRecord;
var
  I : Integer;
begin
  result := nil;
  for I := 0 to Count-1 do
    begin
      if SameText(PSFDS_FileEntryRecord(Items[I])^.FileName, FileName) then
         begin
           result := PSFDS_FileEntryRecord(Items[I]);
           exit;
         end;
    end;
end;

function TSFDS_FileEntrys.FindEntryOriginal(OriginalFileName: AnsiString): PSFDS_FileEntryRecord;
var
  I : Integer;
begin
  result := nil;
  for I := 0 to Count-1 do
    begin
      if SameText(PSFDS_FileEntryRecord(Items[I])^.OriginalFileName, OriginalFileName) then
         begin
           result := PSFDS_FileEntryRecord(Items[I]);
           exit;
         end;
    end;
end;

function TSFDS_FileEntrys.GetS(Index: Integer): PSFDS_FileEntryRecord;
begin
  result := PSFDS_FileEntryRecord(Items[Index]);
end;

{ TSFDSCustomSource }

constructor TSFDSCustomSource.Create(SourceStream : TStream; RelativeOffset : Int64);
begin
  if SourceStream = nil then raise ESFDSNilSourceStream.Create(SFDSNilStream);
  inherited Create;
  FSourceStream := SourceStream;
  FRelOffset := RelativeOffset;
end;

function TSFDSCustomSource.GetFInternalCompressionFormat : Byte;
begin
  result := FInternalCompressionFormat;
end;

procedure TSFDSCustomSource.SetFInternalCompressionFormat(const CompressionFormat : Byte);
begin
  FInternalCompressionFormat := CompressionFormat;
end;

{ TSFDSCustomReader }

function TSFDSCustomReader.Write(const Buffer; Count: Longint): Longint;
begin
  raise ESFDSInvalidStreamOperation.Create(SFDSReadOnly);
end;

function TSFDSCustomReader.Read(var Buffer; Count: Longint): Longint;
begin
  raise ESFDSInvalidStreamOperation.Create(SFDSReaderNoRead);
end;

constructor TSFDSCustomReader.Create(SourceStream : TStream; StartOffset : Int64; FormatGUID : PGUID;
  SupportedProc : TSFDSSupportedEvent; OnReadFileEntry : TSFDSOnReadFileEntry; AssignUniqueNames : Boolean; StreamAutorenameMode : TStreamAutorenameMode);
begin
  inherited Create(SourceStream, StartOffset);
  ActiveReaders.AddReader(Self);
  FRelOffset := StartOffset;
  SourceStream.Seek(FRelOffset, {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF});
  FFileEntrys := TSFDS_FileEntrys.Create;
  FExtraInfoFields := TSFDSFileExtraInfoList.Create;
  FSupportedProc := SupportedProc;
  FOnReadFileEntry := OnReadFileEntry;
  FAssignUniqueNames := AssignUniqueNames;
  FAllFilesSize := 0;
  if not Assigned(FSupportedProc) then FSupportedProc := SFDS_DefaultSupportedProc;
  if FormatGUID <> nil then FGuid := FormatGuid^
                       else FGuid := DEFAULTSFDSGUID;
  FStreamAutorenameMode := StreamAutorenameMode;
  ReadHeader;
end;

destructor TSFDSCustomReader.Destroy;
begin
  if Assigned(FExtraInfoFields) then FExtraInfoFields.Free;
  if Assigned(FFileEntrys) then FFileEntrys.Free;
  ActiveReaders.Remove(Self);
  inherited Destroy;
end;

function TSFDSCustomReader.DirectStreamToStreamCopy(const FileName: AnsiString;
  Dest: TStream; const DiskFileName : AnsiString; ProgressEvent: TSFDSProgressEvent;
  ThrowExceptions: Boolean): Integer;
var Source : TStream;
begin
  result := E_OK; Source := nil;
  try
    try
    Source := OpenRAWStream(FileName);
    result := DirectStreamToStreamCopy(Source, Dest, DiskFileName, FileName, ProgressEvent, ThrowExceptions);
    finally
      Source.Free;
    end;
  except
    if ThrowExceptions then raise;
  end;
end;

function TSFDSCustomReader.DirectStreamToStreamCopy(const FileIndex: Integer;
  Dest: TStream; const DiskFileName : AnsiString; ProgressEvent: TSFDSProgressEvent;
  ThrowExceptions: Boolean): Integer;
var Source : TStream;
begin
  result := E_OK; Source := nil;
  try
    try
    Source := OpenRAWStream(FileIndex);
    result := DirectStreamToStreamCopy(Source, Dest, DiskFileName, FileEntrys.Entrys[FileIndex].FileName, ProgressEvent, ThrowExceptions);
    finally
      Source.Free;
    end;
  except
    if ThrowExceptions then raise;
  end;
end;

function TSFDSCustomReader.DirectStreamToStreamCopy(Source, Dest: TStream;
  const DiskFileName, FileName : AnsiString; ProgressEvent: TSFDSProgressEvent;
  ThrowExceptions: Boolean): Integer;
var CopyBuffer : Pointer;
    BytesCopied : LongInt;
    K, S : Int64;
    Cancel : Boolean;
    ChunkSize : Longint;
begin
  ChunkSize := SFDS_ChunkSize;
  Cancel := False;
  result := E_OK;
  BytesCopied := 0; K := 0; S := Source.Size;
  if (Source = nil) or (Dest = nil) then
     begin
     result := E_EOPEN;
     if ThrowExceptions then raise ESFDSNilSourceStream.Create(SFDSNilStream);
     exit;
     end;
  try
   GetMem(CopyBuffer, ChunkSize);
  except
   result := E_NO_MEMORY;
   if ThrowExceptions then raise;
   exit;
  end;
  try
    repeat
     try
      BytesCopied := Source.Read(CopyBuffer^, ChunkSize);
     except
      result := E_EREAD;
      if ThrowExceptions then raise;
      exit;
     end;
    K := K + BytesCopied;
    if (BytesCopied > 0) then
       begin
        try
         Dest.WriteBuffer(CopyBuffer^, BytesCopied);
        except
         result := E_EWRITE;
         if ThrowExceptions then raise;
         exit;
        end;
       end;

    if Assigned(ProgressEvent) then ProgressEvent(Self, K, S, BytesCopied, Cancel, DiskFileName, FileName);
    until (BytesCopied < ChunkSize) or Cancel;
  finally
    FreeMem(CopyBuffer, ChunkSize);
  end;
  if Cancel then
    begin
    Result := E_EABORTED;
    if ThrowExceptions then raise ESFDSUserAbortedOperation.Create(SFDSAbortedByUser);
    Exit;
    end;
end;

function TSFDSCustomReader.ExtractFile(FileName, DiskFileName: AnsiString;
  Test: boolean; ProgressEvent: TSFDSProgressEvent; ThrowExceptions : boolean): Integer;
begin
  result := ExtractFileEx(FileName, DiskFileName, nil, True, Test, ProgressEvent, ThrowExceptions);
end;

function TSFDSCustomReader.ExtractFile(FileIndex : Integer; DiskFileName: AnsiString;
  Test: boolean; ProgressEvent: TSFDSProgressEvent; ThrowExceptions : boolean): Integer;
begin
  result := ExtractFileEx(FileIndex, DiskFileName, nil, True, Test, ProgressEvent, ThrowExceptions);
end;

function TSFDSCustomReader.ExtractFileEx(FileIndex : Integer; DiskFileName : AnsiString;
  Stream : TStream; ToDisk, Test : boolean; ProgressEvent : TSFDSProgressEvent;
  ThrowExceptions : Boolean) : Integer;
var F : TFileStream;
    CopyBuffer : Pointer;
    BytesCopied : LongInt;
    K, S : Int64;
    SF : TStream;
    MD5C : TMD5Context;
    DIG : TMD5Digest;
    Cancel : Boolean;
    StrSz : Int64;
    FileName : AnsiString;
    ChunkSize : Longint;
begin
  ChunkSize := SFDS_ChunkSize;
  F := nil;
  SF := nil;
  Cancel := False;
  result := E_OK;
  BytesCopied := 0;
  if (FileIndex < 0) or (FileIndex > FileEntrys.Count - 1) then
     begin
       result := E_EOPEN;
       if ThrowExceptions then raise ESFDSException.Create(SFDSFileEntryNotFound);
       exit;
     end;
  if ((ToDisk and (DiskFileName = '')) or ((not ToDisk) and (Stream = nil))) and (not Test) then
     begin
       result := E_ECREATE;
       if ThrowExceptions then raise EFOpenError.CreateFmt(SFOpenError, [DiskFileName]);
       Exit;
     end;
  MD5Init(MD5C);
  try
      // Open source
      try
       SF := OpenStream(FileIndex, StrSz);
       FileName := FileEntrys.Entrys[FileIndex]^.FileName;
      except
       result := E_EOPEN; F := nil; SF := nil;
       if ThrowExceptions then raise;
       exit;
      end;
      S := StrSz;
      K := 0;
      //  Create Dest
      if (not Test) and ToDisk then
         try
          if not DirectoryExists(ExtractFileDir(DiskFileName))
               then ForceDirectories(ExtractFileDir(DiskFileName));
          F := TFileStream.Create(DiskFileName, fmCreate or fmShareDenyWrite);
         except
          result := E_ECREATE;
          SF.Free;
          F := nil; SF := nil;
          if ThrowExceptions then raise;
          exit;
         end;
      try
       GetMem(CopyBuffer, ChunkSize);
      except
       result := E_NO_MEMORY;
       if not Test then F.Free;
       SF.Free;
       F := nil; SF := nil;
       if ThrowExceptions then raise;
       exit;
      end;
      try
        repeat
         try
          BytesCopied := SF.Read(CopyBuffer^, ChunkSize);
         except
          result := E_EREAD;
          if not Test then F.Free;
          SF.Free;
          F := nil; SF := nil;
          if ThrowExceptions then raise;
          exit;
         end;
        K := K + BytesCopied;
        if (BytesCopied > 0) then
           begin
            try
             if (not Test) and ToDisk then F.WriteBuffer(CopyBuffer^, BytesCopied)
                else if (not Test) and (not ToDisk) then Stream.WriteBuffer(CopyBuffer^, BytesCopied);
            except
             result := E_EWRITE;
             if not Test then F.Free;
             SF.Free;
             F := nil; SF := nil;
             if ThrowExceptions then raise;
             exit;
            end;

           MD5Update(MD5C, CopyBuffer^, BytesCopied);
           end;
        if Assigned(ProgressEvent) then ProgressEvent(Self, K, S, BytesCopied, Cancel, DiskFileName, FileName);
        until (BytesCopied < ChunkSize) or Cancel;
      finally
        FreeMem(CopyBuffer, ChunkSize);
      end;
      if Cancel then
      begin
        Result := E_EABORTED;
        if ThrowExceptions then raise ESFDSUserAbortedOperation.Create(SFDSAbortedByUser);
        Exit;
      end;
      DIG := MD5Final(MD5C);
      if not MD5DigestsEqual(DIG, FileEntrys.FindEntry(FileName)^.MD5Digest) then
         begin
           result := E_BAD_ARCHIVE;
           if ThrowExceptions then SFDS_Error(SFDSMD5Error, [DiskFileName, FileName]);
         end;
  finally
      if not Test then if Assigned(F) then F.Free;
      if Assigned(SF) then SF.Free;
  end;
end;

function TSFDSCustomReader.ExtractFilesTo(DestinationDir: AnsiString; Test: boolean;
  ProgressEvent: TSFDSProgressEvent; ThrowExceptions: Boolean): Integer;
begin
  result := ExtractFilesTo(DestinationDir, '*', Test, ProgressEvent, ThrowExceptions);
end;

function TSFDSCustomReader.ExtractFilesTo(DestinationDir, Mask: AnsiString;
  Test: boolean; ProgressEvent: TSFDSProgressEvent;
  ThrowExceptions: Boolean): Integer;
var I : Integer;
    TargetDir,
    FileName : AnsiString;
begin
result := E_OK;
TargetDir := IncludeTrailingPathDelimiter(DestinationDir);
  for I := 0 to GetFileCount - 1 do
    begin
      FileName := FileEntrys.Entrys[I]^.FileName;
      if (Mask = '*') or
         (Mask = '*.*') or
         WildcardMatchFile(FileName, Mask) then
           begin
             result := ExtractFile(I, TargetDir + FileName, Test, ProgressEvent, ThrowExceptions);
             if result <> E_OK then Exit;             
           end;
    end;
end;

function TSFDSCustomReader.ExtractFileEx(FileName, DiskFileName: AnsiString;
  Stream: TStream; ToDisk, Test: boolean; ProgressEvent: TSFDSProgressEvent;
  ThrowExceptions: Boolean): Integer;
var I, FileIndex : Integer;
begin
  FileIndex := -1;
  for I := 0 to FileEntrys.Count - 1 do
      begin
        if SameText(FileName, FileEntrys.Entrys[I]^.FileName) then
           begin
             FileIndex := I;
             Break;
           end;
      end;
  result := ExtractFileEx(FileIndex, DiskFileName, Stream, ToDisk, Test, ProgressEvent, ThrowExceptions);
end;

function TSFDSCustomReader.GetExtraInfoCount: Integer;
begin
  result := ExtraInfoFields.Count;
end;

function TSFDSCustomReader.GetExtraInfoFieldNo(
  Index: Integer): TSFDSFileExtraInfoField;
var P : TSFDSFileExtraInfoField;
begin
  if (Index <= ExtraInfoFields.Count - 1) and (Index > 0) then
     begin
       P := ExtraInfoFields.ExtraInfoField[Index]^;
       result.Name := P.Name;
       Result.FieldType := P.FieldType;
       case Result.FieldType of
         EIStr:
           begin
           GetMem(Result.ValueString, P.ValueStringSize);
           System.Move(P.ValueString^, Result.ValueString^, P.ValueStringSize);
           Result.ValueStringSize := P.ValueStringSize;
           end;
         EINumber: Result.ValueNumber := P.ValueNumber;
         EIDate: Result.ValueDate := P.ValueDate;
         EIBoolean: Result.ValueBoolean := P.ValueBoolean;
         EIBinary:
           begin
           GetMem(Result.ValueBinary, P.ValueBinarySize);
           System.Move(P.ValueBinary^, Result.ValueBinary^, P.ValueBinarySize);
           Result.ValueBinarySize := P.ValueBinarySize;
           end;
       end;
     end;
end;

function TSFDSCustomReader.GetFileCount: Integer;
begin
  result := FFileNumber;
  if result <> FFileEntrys.Count then result := FFileEntrys.Count;
end;

function TSFDSCustomReader.OpenStream(FileName: AnsiString; out StreamSize : Int64): TStream;
var Entry : PSFDS_FileEntryRecord;
begin
  Entry := FileEntrys.FindEntry(FileName);
  if Entry = nil then
     raise ESFDSException.Create(SFDSFileEntryNotFound);
      
  result := OpenStreamEx(Entry, StreamSize);
end;

function TSFDSCustomReader.OpenRAWStream(FileName: AnsiString): TStream;
var Entry : PSFDS_FileEntryRecord;
begin
  Entry := FileEntrys.FindEntry(FileName);
  if Entry = nil then
     raise ESFDSException.Create(SFDSFileEntryNotFound);

  result := OpenRAWStream(Entry);
end;

function TSFDSCustomReader.OpenRAWStream(FileIndex: Integer): TStream;
var Entry : PSFDS_FileEntryRecord;
begin
  if (FileIndex < 0) or (FileIndex > FileEntrys.Count - 1) then
     raise ESFDSException.Create(SFDSFileEntryNotFound);

  Entry := FileEntrys.Entrys[FileIndex];
  result := OpenRAWStream(Entry);
end;

function TSFDSCustomReader.OpenRAWStream(Entry: PSFDS_FileEntryRecord): TStream;
var CloneMode : TCloneStreamMode;
begin
  if (Entry <> nil) then
    begin
    result := Clone(FRelOffset + Entry^.StartPosition, FRelOffset + Entry^.EndPosition, CloneMode);
       case CloneMode of
         csmFull : result := TSelectiveStream.Create(result, FRelOffset + Entry^.StartPosition, FRelOffset + Entry^.EndPosition);
         csmOnlyRequestedData : begin  end;
       end;
    end
    else
    begin
      raise ESFDSException.Create(SFDSFileEntryNotFound);
    end;
end;

function TSFDSCustomReader.OpenStream(FileIndex : Integer; out StreamSize : Int64) : TStream;
var Entry : PSFDS_FileEntryRecord;
begin
  if (FileIndex < 0) or (FileIndex > FileEntrys.Count - 1) then
     raise ESFDSException.Create(SFDSFileEntryNotFound);

  Entry := FileEntrys.Entrys[FileIndex];
  result := OpenStreamEx(Entry, StreamSize);
end;

function TSFDSCustomReader.OpenStreamEx(Entry: PSFDS_FileEntryRecord;
  out StreamSize: Int64): TStream;
var CloneMode : TCloneStreamMode;
begin
  if (Entry <> nil) then
     begin
       StreamSize := Entry^.FileSize;
       result := Clone(FRelOffset + Entry^.StartPosition, FRelOffset + Entry^.EndPosition, CloneMode);
       case CloneMode of
         csmFull : result := TSelectiveStream.Create(result, FRelOffset + Entry^.StartPosition, FRelOffset + Entry^.EndPosition);
         csmOnlyRequestedData : begin  end;
       end;
       if Entry^.CompressionFormat <> 0 then
          if SFDS_CompressionFormatByFormatID(Entry^.CompressionFormat) <> nil then
             begin
             result := SFDS_CompressionFormatByFormatID(Entry^.CompressionFormat)^.Decompressor.Create(result, Entry^.FileSize);
             end
             else
             begin
               if Assigned(Result) then Result.Free; //Free Result And Show Error...
               raise ESFDSDecompressionError.CreateFmt(SFDSUnknownCompressionFormat, [IntToHex(Entry^.CompressionFormat, 3)]);
             end;
     end
     else
     begin
       raise ESFDSException.Create(SFDSFileEntryNotFound); 
     end;
end;

procedure TSFDSCustomReader.ReadHeader;
var Parser : TSFDSCustomStreamParser;
begin
  Parser := TSFDSCustomStreamParser.Create(SourceStream, FRelOffset, False, Self);
  try
    Parser.Parse(FExtraInfoFields, FFileEntrys, FAssignUniqueNames, FStreamAutorenameMode);
    FTimeCreated := Parser.TimeCreate;
    FMakerVersion := Parser.MakerVersion;
    FVersion := Parser.Version;
    FMaker := Parser.Maker;
    FAuthor := Parser.Author;
    FComment := Parser.Comments;
    FTitle := Parser.Title;
    FSubject := Parser.Subject;
    FKeyWords := Parser.KeyWords;
    FFileNumber := Parser.FileNumber;
    FAllFilesSize := Parser.AllFilesSize;
    FFileListPositionStart := Parser.FileListPositionStart;
    FFileListPositionEnd := Parser.FileListPositionEnd;
    FFileInfoExPosition := Parser.FileInfoExPosition;
  finally
    Parser.Free;
  end;
end;

{ TSFDSCustomWriter }

function TSFDSCustomWriter.Read(var Buffer; Count: Longint): Longint;
begin
  raise ESFDSInvalidStreamOperation.Create(SFDSWriteOnly);
end;

procedure TSFDSCustomWriter.Close;
var I64, OFFSet, FileListPosS, FileListPosE : Int64;
    B : Boolean;
    I : Integer;
    IST : LongInt;
    WrStr : TStream;
    cStr : TSFDSCopyStream;
    ComStr : TSFDSGenericCompressionStream;
    cntx : TMD5Context;
    TI : LongInt;

    procedure WriteFileEntry(Entry : SFDS_FileEntryRecord);
    begin
      WrStr.WriteBuffer(PChar(SFDS_SIGNATURE_FE)^, Length(SFDS_SIGNATURE_FE));
      WriteString(WrStr, Entry.FileName);
      WriteInteger(WrStr, Entry.FileSize);
      WriteInteger(WrStr, Entry.FileAttributes);
      WriteInteger(WrStr, Entry.StartPosition);
      WriteInteger(WrStr, Entry.EndPosition);
      WriteInteger(WrStr, Entry.CompressionFormat);
      WriteInteger(WrStr, Entry.CompressionLevel);
      WrStr.WriteBuffer(Entry.MD5Digest, SizeOf(TMD5Digest));
      WriteString(WrStr, Entry.ExtraDataStr);
      WriteInteger(WrStr, Entry.ExtraDataInt);
    end;

begin
  WrStr := nil;
  ComStr := nil;
  cStr := nil;
  if FClosed then Exit;
  UpdateLastEntry;
  FInternalWrittingMechanismActive := False;
  FClosed := True;
  FileListPosS := SourceStream.Position - FRelOffset;
  SourceStream.Position := SourceStream.Size;

  try
  WrStr := SourceStream;

  if (SFDS_CompressionFormatByFormatID(FInternalCompressionFormat) = nil) then
     FInternalCompressionFormat := 0;

  if (FInternalCompressionFormat = 0) then
       begin
         cStr := TSFDSCopyStream.Create(SourceStream);
         cStr.Init;
         WrStr := cStr;
       end
       else
       begin
         ComStr := TSFDSGenericCompressionStream.Create(SFDS_CompressionFormatByFormatID(FInternalCompressionFormat)^.Compressor.Create(SourceStream, clMax, csDefault));
         ComStr.Init;
         WrStr := ComStr;
       end;

  for I := 0 to FFileEntrys.Count - 1 do
      begin
        WriteFileEntry(PSFDS_FileEntryRecord(FFileEntrys.Items[I])^);
      end;

  if Assigned(ComStr) then cntx := ComStr.MD5Context;
  if Assigned(cStr) then cntx := cStr.MD5Context;
  FInternalFileListDataMD5 := MD5Final(Cntx);
  finally
    WrStr := nil;
    if Assigned(ComStr) then ComStr.Free;
    if Assigned(cStr) then cStr.Free;
  end;

  FileListPosE := SourceStream.Position - FRelOffset;
  OFFSet := FRelOffset + Length(SFDS_SIGNATURE_STR) + SizeOf(LongInt) + SizeOf(TGuid) + SizeOf(Boolean);
  SourceStream.Seek(OFFSet, {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF});
  TI := FFileEntrys.Count;         //Number of files
  SourceStream.WriteBuffer(TI, SizeOf(LongInt));
  TI := 0;
  SourceStream.WriteBuffer(TI, SizeOf(LongInt));
  
  I64 := FileListPosS;               //File list start position
  SourceStream.WriteBuffer(I64, SizeOf(Int64));
  I64 := FileListPosE;               //File list end position
  SourceStream.WriteBuffer(I64, SizeOf(Int64));

  SourceStream.WriteBuffer(FInternalCompressionFormat, SizeOf(Byte));
  SourceStream.WriteBuffer(FInternalFileListDataMD5, SizeOf(TMD5Digest));

  OFFSet := FRelOffset + Length(SFDS_SIGNATURE_STR) + SizeOf(LongInt) + SizeOf(TGuid);
  SourceStream.Seek(OFFSet, {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF});
  B := True;
  SourceStream.WriteBuffer(B, SizeOf(Boolean));  //Completed
  case FAppendSFXMode of
   aSfxNone : begin end;
   aSfxmExeHdr :
     begin
       SourceStream.Seek(SFDS_SELFEXTRHDRPOS, {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF});
       IST := SFDS_SIGNATURE_INT;
       SourceStream.Write(IST, SizeOf(LongInt));
       I64 := FRelOffset;
       SourceStream.Write(I64, SizeOf(Int64));
     end;
   aSfxEOF :
     begin
       SourceStream.Seek(0, {$IFNDEF VERSION_5_OR_BELLOW}soEnd{$ELSE}soFromEnd{$ENDIF});
       IST := SFDS_SIGNATURE_INT;
       SourceStream.Write(IST, SizeOf(LongInt));
       I64 := FRelOffset;
       SourceStream.Write(I64, SizeOf(Int64));
     end;
  end;
end;

constructor TSFDSCustomWriter.Create(SourceStream : TStream;
  Append : Boolean; AppendSFXMode : TAppendSFXMode;
  FormatGUID: PGUID; Title, Subject, Author: AnsiString; Version: Integer;
  Maker: AnsiString; MakerVersion: Integer; KeyWords, Comments: AnsiString;
  ExtraInfoFields : TSFDSFileExtraInfoList);

    procedure WriteHeader;
    var L : LongInt;
        B : Boolean;
        I64 : Int64;
        BTT : TDateTime;
    begin
      SourceStream.WriteBuffer(PChar(SFDS_SIGNATURE_STR)^, Length(SFDS_SIGNATURE_STR));
      L := SFDS_SIGNATURE_INT;
      SourceStream.WriteBuffer(L, SizeOf(LongInt));
      SourceStream.WriteBuffer(RequiredGuid, SizeOf(TGuid));
      B := False;
      SourceStream.WriteBuffer(B, SizeOf(Boolean));  //To be updated on close...
      I64 := 0;                         //no files yet!
      SourceStream.WriteBuffer(I64, SizeOf(Int64));  //To be updated on close...
      I64 := -1;                        //No file list start yet!
      SourceStream.WriteBuffer(I64, SizeOf(Int64));  //To be updated on close...
      I64 := -1;                        //No file list end yet!
      SourceStream.WriteBuffer(I64, SizeOf(Int64));  //To be updated on close...

      {dummy write, to be updated on close}
      SourceStream.Write(FInternalCompressionFormat, SizeOf(Byte));
      SourceStream.Write(FDigest, SizeOf(TMD5Digest));
      {dummy write, to be updated on close}

      BTT := Now;
      SourceStream.WriteBuffer(BTT, SizeOf(TDateTime));
      I64 := SourceStream.Position + SizeOf(Int64) - FRelOffset;
      SourceStream.WriteBuffer(I64, SizeOf(Int64));  //FileInfoExPosition
      WriteString(SourceStream, Title);
      WriteString(SourceStream, Subject);
      WriteString(SourceStream, Author);
      WriteInteger(SourceStream, Version);
      WriteString(SourceStream, Maker);
      WriteInteger(SourceStream, MakerVersion);
      WriteString(SourceStream, KeyWords);
      WriteString(SourceStream, Comments);

      if Assigned(ExtraInfoFields) then
         ExtraInfoFields.SaveToStream(SourceStream)
           else
             WriteInteger(SourceStream, 0);
    end;

begin
inherited Create(SourceStream, RelativeOffset);
Compressor := nil;
FInternalCompressionFormat := 0;
if SourceStream = nil then raise ESFDSNilSourceStream.Create(SFDSNilStream);
  if (not Append) then
     begin
       FRelOffset := 0;
     end
     else
     begin
       FRelOffset := SourceStream.Seek(0, {$IFNDEF VERSION_5_OR_BELLOW}soEnd{$ELSE}soFromEnd{$ENDIF});
     end;

  if Append and (FRelOffset > 0) then
     begin
       FAppendSFXMode := AppendSFXMode
     end
     else
       FAppendSFXMode := aSfxNone;

  if FormatGUID <> nil then FGuid := FormatGuid^
                       else FGuid := DEFAULTSFDSGUID;
  FFileEntrys := TSFDS_FileEntrys.Create;
  FClosed := False;
  WriteHeader;
  FInternalWrittingMechanismActive := True;
end;

destructor TSFDSCustomWriter.Destroy;
begin
  Close;
  if Assigned(FFileEntrys) then FFileEntrys.Free;
  inherited Destroy;
end;

{$IFNDEF VERSION_5_OR_BELLOW}
function TSFDSCustomWriter.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TSFDSCustomWriter.Seek(Offset: LongInt; Origin: Word): LongInt;
{$ENDIF}
begin
  raise ESFDSInvalidStreamOperation.Create(SFDSNoSeek);
end;

procedure TSFDSCustomWriter.OpenStream(FileName: AnsiString;
  FileAttributes: Integer; CompressionFormat : Byte;
  CompressionLevel: TSFDSCompressionLevel; CompressionStrategy : TSFDSCompressionStrategy;
  ExtraDataStr: AnsiString;
  ExtraDataInt: Integer);
var
  Dig : TMD5Context;
begin
  UpdateLastEntry;
  New(FThisEntry);
  FThisEntry^.FileName := FileName;
  FThisEntry^.OriginalFileName := FileName;  
  FThisEntry^.FileAttributes := FileAttributes;
  FThisEntry^.CompressionLevel := Byte(CompressionLevel);
  FThisEntry^.CompressionFormat := CompressionFormat;
  FThisEntry^.ExtraDataStr := ExtraDataStr;
  FThisEntry^.ExtraDataInt := ExtraDataInt;
  FThisEntry^.StartPosition := SourceStream.Position - FRelOffset;
  FDigest := Dig;
  MD5Init(FDigest);

  if SFDS_CompressionFormatByFormatID(CompressionFormat) = nil then
     FThisEntry^.CompressionFormat := 0;

     if (CompressionFormat <> 0) and (SFDS_CompressionFormatByFormatID(CompressionFormat) <> nil) then
        begin
          Compressor := SFDS_CompressionFormatByFormatID(CompressionFormat)^.Compressor.Create(SourceStream, CompressionLevel, CompressionStrategy);
        end;
  FFileSize := 0;
end;

function TSFDSCustomWriter.CreateLink(LinkTo : AnsiString; LinkName : AnsiString; NewAttr : Integer;
  NewExtraDataStr : AnsiString; NewExtraDataInt : Integer) : boolean;
var ExistentEntry : PSFDS_FileEntryRecord;
begin
  UpdateLastEntry;
  result := FFileEntrys.FileExists(LinkTo);
  if not result then exit;
  ExistentEntry := FFileEntrys.FindEntry(LinkTo);
  New(FThisEntry);
  FThisEntry^.FileName := LinkName;
  FThisEntry^.OriginalFileName := LinkName;  
  FThisEntry^.FileSize := ExistentEntry^.FileSize;
  FThisEntry^.FileAttributes := NewAttr;
  FThisEntry^.StartPosition := ExistentEntry^.StartPosition;
  FThisEntry^.EndPosition := ExistentEntry^.EndPosition;
  FThisEntry^.CompressionFormat := ExistentEntry^.CompressionFormat;
  FThisEntry^.CompressionLevel := ExistentEntry^.CompressionLevel;
  FThisEntry^.MD5Digest := ExistentEntry^.MD5Digest;  
  FThisEntry^.ExtraDataStr := NewExtraDataStr;
  FThisEntry^.ExtraDataInt := NewExtraDataInt;
  FFileEntrys.AddEntry(FThisEntry^);
  Dispose(FThisEntry); FThisEntry := nil;
end;

procedure TSFDSCustomWriter.UpdateLastEntry;
begin
  if FThisEntry = nil then Exit;
  SourceStream.Position := SourceStream.Size;
  if FThisEntry^.CompressionFormat <> 0 then
  begin
    if Assigned(Compressor) then begin Compressor.Free; Compressor := nil; end;
  end;
  FThisEntry^.EndPosition := SourceStream.Position - FRelOffset;
  FThisEntry^.MD5Digest := MD5Final(FDigest);
  FThisEntry^.FileSize := FFileSize;
  FFileEntrys.AddEntry(FThisEntry^);
  Dispose(FThisEntry); FThisEntry := nil;
end;

function TSFDSCustomWriter.Write(const Buffer; Count: Integer): Longint;
begin
if FInternalWrittingMechanismActive then
   begin
if FThisEntry = nil then
   SFDS_Error(SFDSWriteErrorNoStream, []);
if FClosed then
   SFDS_Error(SFDSWriteErrorClosed, []);
  if FThisEntry^.CompressionFormat <> 0 then
     begin
       result := Compressor.Write(Buffer, Count);
       FFileSize := FFileSize + result;
       MD5Update(FDigest, Buffer, Count);
     end
     else
     begin
       result := SourceStream.Write(Buffer, Count);
       FFileSize := FFileSize + result;
       MD5Update(FDigest, Buffer, Count);
     end;
   end
   else
   begin
     result := SourceStream.Write(Buffer, Count);
   end;
end;

function TSFDSCustomWriter.WriteFile(DiskFileName, FileName: AnsiString;
  ProgressEvent : TSFDSProgressEvent; FileAttributes: Integer;
  CompressionFormat : Byte; CompressionLevel: TSFDSCompressionLevel; CompressionStrategy : TSFDSCompressionStrategy;
  ExtraDataStr: AnsiString;
  ExtraDataInt: Integer; ThrowExceptions : Boolean) : Integer;
begin
  result := WriteFileEx(DiskFileName, FileName, nil, True, ProgressEvent, FileAttributes, CompressionFormat, CompressionLevel, CompressionStrategy,ExtraDataStr, ExtraDataInt, ThrowExceptions);
end;

function TSFDSCustomWriter.WriteFileEx(DiskFileName, FileName: AnsiString;
  Stream: TStream; FromDisk: boolean; ProgressEvent: TSFDSProgressEvent;
  FileAttributes: Integer; CompressionFormat : Byte;
  CompressionLevel: TSFDSCompressionLevel; CompressionStrategy : TSFDSCompressionStrategy;
  ExtraDataStr: AnsiString;
  ExtraDataInt: Integer; ThrowExceptions: Boolean): Integer;
var F : TFileStream;
    CopyBuffer : Pointer;
    BytesCopied : LongInt;
    K, S : Int64;
    Cancel : Boolean;
    ChunkSize : Longint;
begin
  result := E_OK;
  ChunkSize := SFDS_ChunkSize;
  F := nil;
  Cancel := False;
  BytesCopied := 0;
  if (FromDisk and (not FileExists(DiskFileName))) or ((not FromDisk) and (Stream = nil)) then
     begin
       result := E_EOPEN;
       if ThrowExceptions then raise EFOpenError.CreateFmt(SFOpenError, [DiskFileName]);
       Exit;
     end;
  try
    try
     if FromDisk then
       F := TFileStream.Create(DiskFileName, fmOpenRead or fmShareDenyWrite);
    except
     result := E_ECREATE; F := nil;
     if ThrowExceptions then raise;
     exit;
    end;

    try
    if FromDisk then S := F.Size
                else S := Stream.Size;
    except
     result := E_EREAD;
     if FromDisk then begin F.Free; F := nil; end;
     if ThrowExceptions then raise;
     Exit;
    end;
    K := 0;

    try
     OpenStream(FileName, FileAttributes, CompressionFormat,
      CompressionLevel, CompressionStrategy, ExtraDataStr, ExtraDataInt);
    except
     result := E_EOPEN;
     F.Free; F := nil;
     if ThrowExceptions then raise;
     exit;
    end;
    try
     GetMem(CopyBuffer, ChunkSize);
    except
     result := E_NO_MEMORY;
     F.Free; F := nil;
     if ThrowExceptions then raise;
     exit;
    end;
      try
        repeat
        try
         if FromDisk then BytesCopied := F.Read(CopyBuffer^, ChunkSize)
                     else BytesCopied := Stream.Read(CopyBuffer^, ChunkSize);
        except
         result := E_EREAD;
         F.Free; F := nil;
         if ThrowExceptions then raise;
         exit;
        end;
        K := K + BytesCopied;
        try
        if BytesCopied > 0 then Write(CopyBuffer^, BytesCopied);
        except
         result := E_EWRITE;
         F.Free; F := nil;
         if ThrowExceptions then raise;
         exit;
        end;
        if Assigned(ProgressEvent) then ProgressEvent(Self, K, S, BytesCopied, Cancel, DiskFileName, FileName);
        until (BytesCopied < ChunkSize) or Cancel;
      finally
        FreeMem(CopyBuffer, ChunkSize);
        if Cancel then begin
          Result := E_EABORTED;
          if ThrowExceptions then raise ESFDSUserAbortedOperation.Create(SFDSAbortedByUser);
        end;
      end;
  finally
    if Assigned(F) then F.Free;
  end;
end;

{ TSFDSFileReader }

constructor TSFDSFileReader.Create(const FileName : AnsiString; StartOffset : Int64;
  FormatGUID : PGUID; SupportedProc : TSFDSSupportedEvent;
  OnReadFileEntry : TSFDSOnReadFileEntry; AssignUniqueNames : Boolean; StreamAutorenameMode : TStreamAutorenameMode);
begin
  FFileStream := nil;
  FFileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  FFSFileName := FileName;
  inherited Create(FileStream, StartOffset, FormatGUID, SupportedProc, OnReadFileEntry, AssignUniqueNames, StreamAutorenameMode);
end;

destructor TSFDSFileReader.Destroy;
begin
  if Assigned(FFileStream) then FFileStream.Free;
  FFileStream := nil;
  inherited Destroy;
end;

function TSFDSFileReader.Clone(StartPos, EndPos : Int64; out Mode : TCloneStreamMode) : TStream;
begin
  Mode := csmFull; //Return the full stream. (from 0 to end)
  result := TFileStream.Create(FSFileName, fmOpenRead or fmShareDenyWrite);
end;

{ TSFDSFileWriter }

constructor TSFDSFileWriter.Create(const FileName : AnsiString; Append : Boolean; AppendSFXMode : TAppendSFXMode; FormatGUID : PGUID;
  Title : AnsiString; Subject : AnsiString; Author : AnsiString;
  Version : Longint; Maker : AnsiString; MakerVersion : Longint;
  KeyWords : AnsiString; Comments : AnsiString; ExtraInfoFields : TSFDSFileExtraInfoList);
begin
  FFileStream := nil;
  if (not Append) or (not FileExists(FileName)) then
     begin
       FFileStream := TFileStream.Create(FileName, fmCreate);
     end
     else
     begin
       FFileStream := TFileStream.Create(FileName, fmOpenWrite or fmShareDenyWrite);
     end;
  FFSFileName := FileName;
  inherited Create(FFileStream, Append, AppendSFXMode, FormatGUID, Title, Subject, Author, Version, Maker, MakerVersion, KeyWords, Comments, ExtraInfoFields);
end;

destructor TSFDSFileWriter.Destroy;
begin
  Close; //Flush (To be able to free the filestream safely)
  if Assigned(FFileStream) then FFileStream.Free;
  FFileStream := nil;
  inherited Destroy;
end;

{ TSFDSMemoryStreamWriter }

constructor TSFDSMemoryStreamWriter.Create(MemoryStream : TMemoryStream; Append : Boolean; AppendSFXMode : TAppendSFXMode; FormatGUID : PGUID;
   Title : AnsiString; Subject : AnsiString; Author : AnsiString;
   Version : Longint; Maker : AnsiString; MakerVersion : Longint;
   KeyWords : AnsiString; Comments : AnsiString; ExtraInfoFields : TSFDSFileExtraInfoList);
begin
  inherited Create(MemoryStream, Append, AppendSFXMode, FormatGUID, Title, Subject, Author, Version, Maker, MakerVersion, KeyWords, Comments, ExtraInfoFields);
  FMemoryStream := MemoryStream;
end;

destructor TSFDSMemoryStreamWriter.Destroy;
begin
  FMemoryStream := nil;
  inherited Destroy;
end;

{ TSFDSMemoryStreamReader }

const
SMEMFS = 'MEMORY: $%s';

constructor TSFDSMemoryStreamReader.Create(MemoryStream : TMemoryStream;
  StartOffset : Int64; AutoFreeMemoryStream : Boolean; FormatGUID : PGUID;
  SupportedProc : TSFDSSupportedEvent; OnReadFileEntry : TSFDSOnReadFileEntry;
  AssignUniqueNames : Boolean; StreamAutorenameMode : TStreamAutorenameMode);
begin
  inherited Create(MemoryStream, StartOffset, FormatGUID, SupportedProc, OnReadFileEntry, AssignUniqueNames, StreamAutorenameMode);
  FMemoryStream := MemoryStream;
  FAutoFreeMemoryStream := AutoFreeMemoryStream;
  FSFileName := Format(SMEMFS, [IntToHex(Integer(MemoryStream.Memory), 8)]);
end;

destructor TSFDSMemoryStreamReader.Destroy;
begin
  if FAutoFreeMemoryStream then if Assigned(FMemoryStream) then FMemoryStream.Free;
  FMemoryStream := nil;
  inherited Destroy;
end;

function TSFDSMemoryStreamReader.Clone(StartPos, EndPos : Int64; out Mode : TCloneStreamMode) : TStream;
//var Str : TSelectiveStream;
begin
  Mode := csmOnlyRequestedData;  //Return only the requested data in a new memory stream.
  result := TMemoryStream.Create;
  //The new code is thread safe...
  TMemoryStream(result).SetSize(EndPos - StartPos);
  System.Move(Pointer(Integer(FMemoryStream.Memory) + StartPos)^, TMemoryStream(result).Memory^, EndPos - StartPos);
//  Str := TSelectiveStream.Create(FMemoryStream, StartPos, EndPos, False);
//  TMemoryStream(result).LoadFromStream(Str);
// Str.Free;
end;

{ TSFDSResourceStreamReader }

const
SRESFS1 = 'EXE/RES: "Instance=%s" "ResName="%s" "ResType=%s"';
SRESFS2 = 'EXE/RES: "Instance=%s" "ResID=%s" "ResType=%s"';

constructor TSFDSResourceStreamReader.Create(Instance: THandle; const ResName: AnsiString;
  ResType: PChar; StartOffset : Int64; FormatGUID : PGUID;
  SupportedProc : TSFDSSupportedEvent; OnReadFileEntry : TSFDSOnReadFileEntry;
  AssignUniqueNames : Boolean; StreamAutorenameMode : TStreamAutorenameMode);
begin
  FResourceStream := nil;
  FResourceStream := TResourceStream.Create(Instance, ResName, ResType);
  FSFileName := Format(SRESFS1, [IntToStr(Instance), ResName, ResType]);
  FInstance := Instance;
  FResName := ResName;
  FResType := ResType;
  FResID := 0;
  FUseResName := True;
  inherited Create(FResourceStream, StartOffset, FormatGUID, SupportedProc, OnReadFileEntry, AssignUniqueNames, StreamAutorenameMode);
end;

constructor TSFDSResourceStreamReader.CreateFromID(Instance: THandle; ResID: Integer;
  ResType: PChar; StartOffset : Int64; FormatGUID : PGUID;
  SupportedProc : TSFDSSupportedEvent; OnReadFileEntry : TSFDSOnReadFileEntry;
  AssignUniqueNames : Boolean; StreamAutorenameMode : TStreamAutorenameMode);
begin
  FResourceStream := nil;
  FResourceStream := TResourceStream.CreateFromID(Instance, ResID, ResType);
  FSFileName := Format(SRESFS2, [IntToStr(Instance), IntToStr(ResID), ResType]);
  FInstance := Instance;
  FResName := '';
  FResType := ResType;
  FResID := ResID;
  FUseResName := False;
  inherited Create(FResourceStream, StartOffset, FormatGUID, SupportedProc, OnReadFileEntry, AssignUniqueNames, StreamAutorenameMode);
end;

destructor TSFDSResourceStreamReader.Destroy;
begin
  if Assigned(FResourceStream) then FResourceStream.Free;
  FResourceStream := nil;
  inherited Destroy;
end;

function TSFDSResourceStreamReader.Clone(StartPos, EndPos : Int64; out Mode : TCloneStreamMode) : TStream;
var NewResourceStream : TResourceStream;
begin
  Mode := csmFull;
  if FUseResName then
       NewResourceStream := TResourceStream.Create(FInstance, FResName, FResType)
     else
       NewResourceStream := TResourceStream.CreateFromID(FInstance, FResID, FResType);
  result := NewResourceStream;
end;

{ TSFDS_ActiveReaders }

function TSFDS_ActiveReaders.AddReader(Reader: TSFDSCustomReader): Integer;
var P : PSFDSFileReaderEntry;
begin
  result := -1;
  if Reader = nil then exit;
  New(P);
  P^.Reader := Reader;
  result := Add(P);
end;

destructor TSFDS_ActiveReaders.Destroy;
begin
  FreeAllReaders;
  inherited Destroy;
end;

function TSFDS_ActiveReaders.FindFile(FileName: AnsiString): TSFDSCustomReader;
var I : Integer;
begin
  result := nil;
  for I := 0 to Count - 1 do
    begin
      if SameText(Readers[I].FSFileName, FileName) then
         begin
           result := Readers[I];
           Exit;
         end;
    end;
end;

procedure TSFDS_ActiveReaders.FreeAllReaders;
var I : Integer;
begin
  for I := Count -1 downto 0 do
      begin
        Readers[I].Free;
      end;
end;

function TSFDS_ActiveReaders.GetS(Index: Integer): TSFDSCustomReader;
begin
  result := nil;
  if (Index < 0) or (Index > Count - 1) then exit;
  result := PSFDSFileReaderEntry(Items[Index])^.Reader;
end;

procedure TSFDS_ActiveReaders.Remove(Reader: TSFDSCustomReader);
var I : Integer;
    P : PSFDSFileReaderEntry;
begin
  for I := 0 to Count - 1 do
    begin
      if Readers[I] = Reader then
         begin
           P := PSFDSFileReaderEntry(Items[I]);
           Items[I] := nil;
           P^.Reader := nil;
           Dispose(P);
           Delete(I);
         end;
    end;
end;

{ TSFDSCompressionFormatsList }

type
  TSFDSCompressionFormatsList = class(TList)
  private
    function GetS(Index: Integer): PSFDS_CompressionFormatItem;
  public
    destructor Destroy; override;
    {Adds a new entry to the list.}
    function RegisterFormat(Entry : SFDS_CompressionFormatItem) : Integer;
    {Finds an entry based on its ID.}
    function FindFormat(ID : Byte) : PSFDS_CompressionFormatItem;
    {Returns the entry wih the Index, or nil if not in range.}
    function FormatIndex(FormatName : AnsiString) : Integer;
    property Format[Index: Integer]: PSFDS_CompressionFormatItem read GetS;
  end;

  var CompressionFormatsList : TSFDSCompressionFormatsList = nil;

function TSFDSCompressionFormatsList.GetS(Index: Integer): PSFDS_CompressionFormatItem;
begin
  result := PSFDS_CompressionFormatItem(Items[Index]);
end;

destructor TSFDSCompressionFormatsList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Dispose(PSFDS_CompressionFormatItem(Items[I]));
  inherited Destroy;
end;

function TSFDSCompressionFormatsList.RegisterFormat(Entry : SFDS_CompressionFormatItem) : Integer;
var
  NewRec: PSFDS_CompressionFormatItem;
begin
  New(NewRec);
  with NewRec^ do
       begin
       FormatName := Entry.FormatName;
       FormatID := Entry.FormatID;
       Compressor := Entry.Compressor;
       Decompressor := Entry.Decompressor;
       end;
  result := Add(NewRec);
end;

function TSFDSCompressionFormatsList.FindFormat(ID : Byte) : PSFDS_CompressionFormatItem;
var
  I : Integer;
begin
  result := nil;
  for I := 0 to Count-1 do
    begin
      if (PSFDS_CompressionFormatItem(Items[I])^.FormatID = ID) then
         begin
           result := PSFDS_CompressionFormatItem(Items[I]);
           exit;
         end;
    end;
end;

function TSFDSCompressionFormatsList.FormatIndex(FormatName : AnsiString) : Integer;
var
  I : Integer;
begin
  result := -1;
  for I := 0 to Count-1 do
    begin
      if SameText(PSFDS_CompressionFormatItem(Items[I])^.FormatName, FormatName) then
         begin
           result := I;
           exit;
         end;
    end;
end;

function SFDS_CompressionFormatCount : Integer;
begin
  result := 0;
  if not Assigned(CompressionFormatsList) then Exit;
  result := CompressionFormatsList.Count;
end;

function SFDS_CompressionFormat(Index : Integer) : PSFDS_CompressionFormatItem;
begin
  result := nil;
  if not Assigned(CompressionFormatsList) then Exit;
  result := CompressionFormatsList.Format[Index];
end;

function SFDS_CompressionFormatByFormatID(FormatID : Byte) : PSFDS_CompressionFormatItem;
begin
  result := nil;
  if not Assigned(CompressionFormatsList) then Exit;
  result := CompressionFormatsList.FindFormat(FormatID);
end;

function SFDS_CompressionFormatIDByFormatName(FormatName : AnsiString) : Byte;
var Indx : Integer;
begin
  result := 0;
  if not Assigned(CompressionFormatsList) then Exit;
  Indx := CompressionFormatsList.FormatIndex(FormatName);
  if Indx = -1 then exit;
  result := SFDS_CompressionFormat(Indx)^.FormatID;
end;

function SFDS_RegisterCompressionFormat(FormatName : AnsiString; FormatID : Byte; CompressorClass : TSFDSCompressorClass; DecompressorClass : TSFDSDecompressorClass) : Integer;
var Format : SFDS_CompressionFormatItem;
begin
  result := -1;
  SFDS_CheckCompressionFormatList;
  if not Assigned(CompressionFormatsList) then Exit;
  Format.FormatName := FormatName;
  Format.FormatID := FormatID;
  Format.Compressor := CompressorClass;
  Format.Decompressor := DecompressorClass;
  result := CompressionFormatsList.RegisterFormat(Format);
end;

procedure SFDS_CheckCompressionFormatList;
begin
  if Assigned(CompressionFormatsList) then Exit;
  CompressionFormatsList := TSFDSCompressionFormatsList.Create;
end;

{ TSFDSFileExtraInfoList }

function TSFDSFileExtraInfoList.AddOrUpdateField(
  Item: TSFDSFileExtraInfoField): Integer;
var Indx : Integer;
    PFld : PSFDSFileExtraInfoField;
    P : TSFDSFileExtraInfoField;
begin
Indx := IndexOfField(Item.Name);
  if Indx >= 0 then
     begin
       result := Indx;
       P := ExtraInfoField[Indx]^;
       P.Name := Item.Name;
       P.FieldType := Item.FieldType;
       case P.FieldType of
         EIStr:
           begin
           FreeMem(P.ValueString, P.ValueStringSize);
           GetMem(P.ValueString, Item.ValueStringSize);
           System.Move(Item.ValueString^, P.ValueString^, Item.ValueStringSize);
           P.ValueStringSize := Item.ValueStringSize;
           end;
         EINumber: P.ValueNumber := Item.ValueNumber;
         EIDate: P.ValueDate := Item.ValueDate;
         EIBoolean: P.ValueBoolean := Item.ValueBoolean;
         EIBinary:
           begin
           FreeMem(P.ValueBinary, P.ValueBinarySize);
           GetMem(P.ValueBinary, Item.ValueBinarySize);
           System.Move(Item.ValueBinary^, P.ValueBinary^, Item.ValueBinarySize);
           P.ValueBinarySize := Item.ValueBinarySize;
           end;
       end;
     end
     else
     begin
       New(PFld);
       PFld^.Name := Item.Name;
       PFld^.FieldType := Item.FieldType;
       case Item.FieldType of
         EIStr:
           begin
           GetMem(PFld^.ValueString, Item.ValueStringSize);
           System.Move(Item.ValueString^, PFld^.ValueString^, Item.ValueStringSize);
           PFld^.ValueStringSize := Item.ValueStringSize;
           end;
         EINumber: PFld^.ValueNumber := Item.ValueNumber;
         EIDate: PFld^.ValueDate := Item.ValueDate;
         EIBoolean: PFld^.ValueBoolean := Item.ValueBoolean;
         EIBinary:
           begin
           GetMem(PFld^.ValueBinary, Item.ValueBinarySize);
           System.Move(Item.ValueBinary^, PFld^.ValueBinary^, Item.ValueBinarySize);
           PFld^.ValueBinarySize := Item.ValueBinarySize;
           end;
       end;
       result := Add(PFld);
     end;
end;

procedure TSFDSFileExtraInfoList.Assign(Source: TSFDSFileExtraInfoList);
var I : Integer;
begin
  if Source = nil then Exit;
  Clear;
  for I := 0 to Source.Count - 1 do
     begin
       AddOrUpdateField(Source.ExtraInfoField[I]^);
     end;
end;

procedure TSFDSFileExtraInfoList.Clear;
var I : Integer;
begin
  for I := 0 to Count - 1 do
    begin
    if (PSFDSFileExtraInfoField(Items[I])^.FieldType = EIStr) then
      FreeMem(PSFDSFileExtraInfoField(Items[I])^.ValueString, PSFDSFileExtraInfoField(Items[I])^.ValueStringSize);
    if (PSFDSFileExtraInfoField(Items[I])^.FieldType = EIBinary) then
      FreeMem(PSFDSFileExtraInfoField(Items[I])^.ValueBinary, PSFDSFileExtraInfoField(Items[I])^.ValueBinarySize);
    Dispose(PSFDSFileExtraInfoField(Items[I]));
    end;
  inherited Clear;
end;

procedure TSFDSFileExtraInfoList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then Exit;
    if (PSFDSFileExtraInfoField(Items[Index])^.FieldType = EIStr) then
      FreeMem(PSFDSFileExtraInfoField(Items[Index])^.ValueString, PSFDSFileExtraInfoField(Items[Index])^.ValueStringSize);
    if (PSFDSFileExtraInfoField(Items[Index])^.FieldType = EIBinary) then
      FreeMem(PSFDSFileExtraInfoField(Items[Index])^.ValueBinary, PSFDSFileExtraInfoField(Items[Index])^.ValueBinarySize);
    Dispose(PSFDSFileExtraInfoField(Items[Index]));
  inherited Delete(Index);
end;

destructor TSFDSFileExtraInfoList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TSFDSFileExtraInfoList.GetExtraInfoField(
  Index: Integer): PSFDSFileExtraInfoField;
begin
  result := PSFDSFileExtraInfoField(Items[Index]);
end;

function TSFDSFileExtraInfoList.IndexOfField(Name: AnsiString): Integer;
var I : Integer;
begin
result := -1;
  for I := 0 to Count - 1 do
      begin
        if SameText(Name, ExtrainfoField[I]^.Name) then
           begin
             result := I;
             Exit;
           end;
      end;
end;

procedure TSFDSFileExtraInfoList.SaveToStream(Stream : TStream);
var I, K : Integer;
begin
      K := Count;
      WriteInteger(Stream, K);
      for I := 0 to K - 1 do
            begin
              WriteString(Stream, ExtraInfoField[I]^.Name);
              WriteInteger(Stream, Integer(ExtraInfoField[I]^.FieldType));
              case ExtraInfoField[I]^.FieldType of
                EIStr : WriteString(Stream, Pointer2String(ExtraInfoField[I]^.ValueString, ExtraInfoField[I]^.ValueStringSize));
                EINumber : WriteInteger(Stream, ExtraInfoField[I]^.ValueNumber);
                EIDate : WriteDateTime(Stream, ExtraInfoField[I]^.ValueDate);
                EIBoolean : WriteInteger(Stream, Integer(Boolean(ExtraInfoField[I]^.ValueBoolean)));
                EIBinary: WriteBinary(Stream, ExtraInfoField[I]^.ValueBinary^, ExtraInfoField[I]^.ValueBinarySize);
              end;
            end;
end;

procedure TSFDSFileExtraInfoList.LoadFromStream(Stream : TStream);
var I, L  : Integer;
    BSize : Longint;
    Field : TSFDSFileExtraInfoField;
    TempStr : AnsiString;
    Tmp : Int64;
begin
  Clear;
  L := ReadInt64(Stream);
  for I := 0 to L - 1 do
      begin
        Field.Name := ReadString(Stream);
        Tmp := ReadInt64(Stream);
        if (Tmp < 0) or (Tmp > 4) then
           Tmp := 4; //EIBinary
        Field.FieldType := TSFDSFileExtraInfo(Tmp);
        case Field.FieldType of
          EIStr :
            begin
            TempStr := ReadString(Stream);
            String2Pointer(TempStr, Field.ValueString);
            Field.ValueStringSize := Length(TempStr);
            end;
          EINumber : Field.ValueNumber := ReadInt64(Stream);
          EIDate : Field.ValueDate := ReadDateTime(Stream);
          EIBoolean : Field.ValueBoolean := Boolean(ReadInteger(Stream));
          EIBinary :
            begin
            BSize := ReadBinarySize(Stream);
            GetMem(Field.ValueBinary, BSize);
            Stream.Read(Field.ValueBinary^, BSize);
            Field.ValueBinarySize := BSize;
            end
        end;
        AddOrUpdateField(Field);
        if Field.FieldType = EIStr then FreeMem(Field.ValueString, Field.ValueStringSize);
        if Field.FieldType = EIBinary then FreeMem(Field.ValueBinary, Field.ValueBinarySize);
      end;
end;

{ TSFDSCustomStreamParser }

constructor TSFDSCustomStreamParser.Create(SourceStream: TStream;
  RelativeOffset : Int64; FreeSourceOnDestroy: Boolean;
  Reader : TSFDSCustomSource);
begin
  inherited Create;
  FFreeSourceOnDestroy := FreeSourceOnDestroy;
  FSourceStream := SourceStream;
  FReader := Reader;
  FRelOffset := RelativeOffset;
end;

destructor TSFDSCustomStreamParser.Destroy;
begin
  FReader := nil;
  if FFreeSourceOnDestroy then FSourceStream.Free;
  inherited Destroy;
end;

procedure TSFDSCustomStreamParser.Parse(var MetaData: TSFDSFileExtraInfoList;
  var FileList: TSFDS_FileEntrys; AssignUniqueNames: Boolean;
  StreamAutorenameMode: TStreamAutorenameMode);
var L, TempInt : LongInt;
    TempStr: AnsiString;
    Guid : TGuid;
    TempBool, Sup : Boolean;
    TempEntry  : SFDS_FileEntryRecord;
    SelStr : TSelectiveStream;
    Decompressor : TSFDSDecompressor;
    FNtryStr : TStream;
    TI : Longint;
    SourceFileName : AnsiString;

    procedure Get_Unique_Name(var FileName : AnsiString);
    var Counter : Integer;
        Original : AnsiString;
    begin
      Counter := 0;
      if (FileName = '') or
         ((Length(FileName) >= 1) and IsDelimiter(' ', FileName, 1))
        then FileName := 'Unnamed File';
      Original := FileName;
      while FileList.FileExists(FileName) do
        begin
          Inc(Counter);
          FileName := Original + '.' + IntToStr(Counter);
        end;
    end;

    procedure Auto_Rename_Stream(var StreamName : AnsiString);
    begin
      case StreamAutorenameMode of
        sarmNone : ;
        sarmSystem :  MakeValidPlatformName(StreamName);
        sarmWindows : MakeValidWindowsName(StreamName);
        sarmUNIX :    MakeValidUNIXName(StreamName);
      else
        //Do nothing
      end;
    end;

    function ReadFileEntry : SFDS_FileEntryRecord;
    var FName : AnsiString;
        L : LongInt;       //FPC Fix
    begin
      L := Length(SFDS_SIGNATURE_FE);
      SetString(TempStr, PChar(nil), L);
      FNtryStr.ReadBuffer(PChar(TempStr)^, L);
      if not SameText(TempStr, SFDS_SIGNATURE_FE) then
         SFDS_Error(SFDSEntryInvalid, [SourceFileName]);

      FName := ReadString(FNtryStr);
      Result.OriginalFileName := FName;
      Auto_Rename_Stream(FName);
      if AssignUniqueNames and Assigned(FileList) then Get_Unique_Name(FName);
      Result.FileName := FName;
      Result.FileSize := ReadInt64(FNtryStr);
      Result.FileAttributes := ReadInteger(FNtryStr);
      Result.StartPosition := ReadInt64(FNtryStr);
      Result.EndPosition := ReadInt64(FNtryStr);
      Result.CompressionFormat := ReadInteger(FNtryStr);
      Result.CompressionLevel := ReadInteger(FNtryStr);
      FNtryStr.ReadBuffer(Result.MD5Digest, SizeOf(TMD5Digest));
      Result.ExtraDataStr := ReadString(FNtryStr);
      Result.ExtraDataInt := ReadInteger(FNtryStr);
    end;

begin
  if Assigned(FReader) then SourceFileName := FReader.FSFileName
                       else SourceFileName := SFDSStreamParser;

  L := Length(SFDS_SIGNATURE_STR);
  SetString(TempStr, PChar(nil), L);
  FSourceStream.ReadBuffer(PChar(TempStr)^, L);
  if not SameText(SFDS_SIGNATURE_STR, TempStr) then
     SFDS_Error(SFDSNotValid, [SourceFileName]);

  FSourceStream.ReadBuffer(TempInt, SizeOf(LongInt));
  if TempInt <> SFDS_SIGNATURE_INT then
     SFDS_Error(SFDSNotValid, [SourceFileName]);

  FSourceStream.ReadBuffer(Guid, SizeOf(TGuid));
  FGuid := Guid;
      if not IsEqualGUID(Guid, RequiredGuid) then
         begin
         Sup := True;
         if Assigned(FReader) and (FReader.InheritsFrom(TSFDSCustomReader)) then
           if Assigned(TSFDSCustomReader(FReader).FSupportedProc) then TSFDSCustomReader(FReader).FSupportedProc(TSFDSCustomReader(FReader), GUID, Sup);
         if not Sup then SFDS_Error(SFDSFormatNotSupported, [SourceFileName]);
         end;

  FSourceStream.ReadBuffer(TempBool, SizeOf(Boolean));
  if not TempBool then
         SFDS_Error(SFDSIncomplete, [SourceFileName]);

  FSourceStream.ReadBuffer(FFileNumber, SizeOf(Longint));
  FSourceStream.ReadBuffer(TI, SizeOf(Longint));

  FSourceStream.ReadBuffer(FFileListPositionStart, SizeOf(Int64));
  FSourceStream.ReadBuffer(FFileListPositionEnd, SizeOf(Int64));

  FSourceStream.ReadBuffer(FInternalCompressionFormat, SizeOf(Byte));
  FSourceStream.ReadBuffer(FInternalFileListDataMD5, SizeOf(TMD5Digest));

  FSourceStream.ReadBuffer(FTimeCreated, SizeOf(TDateTime));
  FSourceStream.ReadBuffer(FFileInfoExPosition, SizeOf(Int64));
  FSourceStream.Seek(FRelOffset + FFileInfoExPosition, {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF});
  FTitle := ReadString(FSourceStream);
  FSubject := ReadString(FSourceStream);
  FAuthor := ReadString(FSourceStream);
  FVersion := ReadInteger(FSourceStream);
  FMaker := ReadString(FSourceStream);
  FMakerVersion := ReadInteger(FSourceStream);
  FKeyWords := ReadString(FSourceStream);
  FComment := ReadString(FSourceStream);

  if Assigned(MetaData) then MetaData.LoadFromStream(FSourceStream);

  if FSourceStream.Seek(FRelOffset + FFileListPositionStart, {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF}) <> FRelOffset + FFileListPositionStart then
       SFDS_Error(SFDSIncomplete, [SourceFileName]);

 FNtryStr := nil;
 try
  SelStr := TSelectiveStream.Create(FSourceStream, FRelOffset + FFileListPositionStart, FRelOffset + FFileListPositionEnd, False);
  FNtryStr := SelStr;
  if FInternalCompressionFormat <> 0 then
     begin
          if SFDS_CompressionFormatByFormatID(FInternalCompressionFormat) <> nil then
             begin
             Decompressor := SFDS_CompressionFormatByFormatID(FInternalCompressionFormat)^.Decompressor.Create(SelStr, FRelOffset + FFileListPositionEnd - FFileListPositionStart);
             FNtryStr := Decompressor;
             end
             else
             begin
               if Assigned(FNtryStr) then FNtryStr.Free;
               raise ESFDSDecompressionError.CreateFmt(SFDSUnknownCompressionFormat, [IntToHex(FInternalCompressionFormat, 3)]);
             end;
     end;

  if not MD5DigestsEqual(MD5Stream(FNtryStr), FInternalFileListDataMD5) then
           SFDS_Error(SFDSEntryCorrupted, [SourceFileName]);
  FNtryStr.Position := 0;

  if (Assigned(FReader) and
     FReader.InheritsFrom(TSFDSCustomReader) and
     Assigned(TSFDSCustomReader(FReader).FOnReadFileEntry)) or
     Assigned(FOnFileEntry) or
     Assigned(FileList)
     then
     begin
      for L := 0 to FFileNumber - 1 do
          begin
            TempEntry := ReadFileEntry;
            FAllFilesSize := FAllFilesSize + TempEntry.FileSize;

            if Assigned(FileList) then
              FileList.AddEntry(TempEntry);

            if Assigned(FReader) and (FReader.InheritsFrom(TSFDSCustomReader)) then
            if Assigned(TSFDSCustomReader(FReader).FOnReadFileEntry) then
               TSFDSCustomReader(FReader).FOnReadFileEntry(TSFDSCustomReader(FReader), TempEntry, L + 1, FFileNumber);

            if Assigned(FOnFileEntry) then FOnFileEntry(nil, TempEntry, L + 1, FFileNumber);
          end;
      end;
 finally
  if Assigned(FNtryStr) then FNtryStr.Free;
 end;
end;

initialization
SFDS_DefaultSupportedProc := SFDS_SupportedAlways;
SFDS_CheckCompressionFormatList;
ActiveReaders := TSFDS_ActiveReaders.Create;

finalization
ActiveReaders.Free;
CompressionFormatsList.Free;

end.
