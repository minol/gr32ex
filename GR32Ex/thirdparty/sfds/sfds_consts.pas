unit sfds_consts;

{*****************************************************************************}
{                                                                             }
{  SFDS (Single File Data Storage) Consts Unit                                }
{                                                                             }
{  For conditions of distribution and use, see LICENSE.TXT                    }
{                                                                             }
{*****************************************************************************}

interface

const
  //Default SFDS Guid (Empty Guid)
  DEFAULTSFDSGUID : TGUID = '{00000000-0000-0000-0000-000000000000}';
  {don't change}
  SFDS_SIGNATURE_INT  = $6B10903;
  {don't localize}
  SFDS_SIGNATURE_STR  = '.SFDS';
  {don't localize}
  SFDS_SIGNATURE_FE   = 'SFDS';
  {don't change}
  SFDS_SELFEXTRHDRPOS = 46;

  const
  {Default is 32KB Chunk}
  DEFAULT_SFDS_ChunkSize = 32768;
  {Min is 8KB Chunk}
  MIN_SFDS_ChunkSize = 8192;
  {Max is 10MB Chunk}
  MAX_SFDS_ChunkSize = 10485760;
  {Success}
  E_OK = 0;
  {No more files in archive}
  E_END_ARCHIVE = 10;
  {Not enough memory}
  E_NO_MEMORY	= 11;
  {Data is bad}
  E_BAD_DATA    = 12;
  {MD5 error in archive data}
  E_BAD_ARCHIVE = 13;
  {Archive format unknown}
  E_UNKNOWN_FORMAT = 14;
  {Cannot open existing file}
  E_EOPEN = 15;
  {Cannot create file}
  E_ECREATE = 16;
  {Error closing file}
  E_ECLOSE = 17;
  {Error reading from file}
  E_EREAD = 18;
  {Error writing to file}
  E_EWRITE = 19;
  {Buffer too small}
  E_SMALL_BUF = 20;
  {Function aborted by user}
  E_EABORTED = 21;
  {No files found}
  E_NO_FILES = 22;
  {Too many files to pack}
  E_TOO_MANY_FILES = 23;
  {Function not supported}
  E_NOT_SUPPORTED = 24;

  SFDS_INVALID_HANDLE_VALUE = 1;
  SFDS_ERROR_NO_MORE_FILES  = 2;

resourcestring
  SFDSReadOnly = 'Invalid stream operation. Cannot write to a read-only stream.';
  SFDSWriteOnly = 'Invalid stream operation. Cannot read from a write-only stream.';
  SFDSNoSeek = 'Invalid stream operation. Cannot seek in a write-only stream.';
  SFDSReaderNoRead = 'TSFDSCustomReader.Read error: Can''t read directly from this stream. Instead use the OpenStream method to get the data.';
  SFDSSetSizeErr = 'Stream.SetSize Error: This stream is fixed in size.';
  SFDSNilStream = 'Error: SourceStream is NIL.';
  SFDSUnknownCompressionFormat = 'Read error: Unknown compression format (ID : %s)';
  SFDSFileEntryNotFound = 'Requested stream can''t be opened because it does not exist.';
  SFDSNotValid = 'This file is not a SFDS file:' + #13#10 + '"%s"';
  SFDSEntryInvalid = 'The entry table of this SFDS file is invalid:' + #13#10 + '"%s"';
  SFDSEntryCorrupted = 'The entry table of this SFDS file is corrupted (MD5 Error):' + #13#10 + '"%s"';
  SFDSFormatNotSupported = 'The format of this SFDS file is not supported:' + #13#10 + '"%s"';
  SFDSIncomplete = 'This SFDS file is incomplete:' + #13#10 + '"%s"';
  SFDSWriteErrorNoStream = 'Can''t write data to SFDS file, because no stream is open.';
  SFDSWriteErrorClosed = 'Can''t write data to SFDS file, because the stream was closed.';
  SFDSMD5Error = 'MD5 Error in SFDS file:'+#13#10+'"%s",'+#13#10'in stream '+#13#10+'"%s".';
  SFDSAbortedByUser = 'Operation aborted by user.';
  SFDSStreamParser = '<STREAM PARSER - UNNAMED SOURCE>';

implementation

end.
