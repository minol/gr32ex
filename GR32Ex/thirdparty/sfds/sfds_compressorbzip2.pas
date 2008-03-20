unit sfds_compressorbzip2;
{$I sfds.inc}

{*****************************************************************************}
{                                                                             }
{  SFDS (Single File Data Storage) BZip2 Compressor Unit                      }
{                                                                             }
{  For conditions of distribution and use, see LICENSE.TXT                    }
{                                                                             }
{*****************************************************************************}

{$IFDEF FPC}
{$ERROR This unit can only be used when compiling with Delphi}
{$ENDIF}

interface
uses Classes, sfds, sfds_bzip2ex;

  type
  {An enhanced version of the TBZDecompressionStream.<BR>
  Used internally.<BR>
  It allows full seeking in the compressed stream.<BR>
  Seeking backwards from the current position, requires seeking to 0, and then forward to the required position (decompressing data on the fly).}
  TBZSFDSDecompressionStream = class(TBZDecompressionStream)
  private
    FUnpackedFileSize : Int64;
    FPosition : Int64;
    FStream   : TStream;
  public
    constructor Create(SourceStream: TStream; unpsize : int64); reintroduce; overload;
    {$IFNDEF VERSION_5_OR_BELLOW}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    {$ENDIF}
    function Read(var Buffer; Count: LongInt): LongInt; override;
    destructor Destroy; override;
  end;

  TSFDSBZip2Compressor = class(TSFDSCompressor)
  private
    FBZip2CompressionStream : TBZCompressionStream;
  protected
    procedure SetSize(NewSize: Longint); override;
    {$IFNDEF VERSION_5_OR_BELLOW}
    procedure SetSize(const NewSize: Int64); override;
    {$ENDIF}
  public
    constructor Create(SourceStream : TStream; CompressionLevel : TSFDSCompressionLevel = clDefault; CompressionStrategy : TSFDSCompressionStrategy = csDefault); override;
    destructor Destroy; override;
    {$IFNDEF VERSION_5_OR_BELLOW}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    {$ENDIF}
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TSFDSBZip2Decompressor = class(TSFDSDecompressor)
  private
    FBZip2DecompressionStream : TBZSFDSDecompressionStream;
  protected
    procedure SetSize(NewSize: Longint); override;
    {$IFNDEF VERSION_5_OR_BELLOW}
    procedure SetSize(const NewSize: Int64); override;
    {$ENDIF}
  public
    constructor Create(SourceStream : TStream; SourceStreamSize : Int64); override;
    destructor Destroy; override;
    {$IFNDEF VERSION_5_OR_BELLOW}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    {$ENDIF}
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

implementation
uses SFDS_Consts;

{ TBZSFDSDecompressionStream }

constructor TBZSFDSDecompressionStream.Create(SourceStream: TStream; unpsize : int64);
begin
  if SourceStream = nil then raise ESFDSNilSourceStream.Create(SFDSNilStream);
  inherited Create(SourceStream);
  FUnpackedFileSize := unpsize;
  FPosition := 0;
  FStream := SourceStream;
end;

destructor TBZSFDSDecompressionStream.Destroy;
begin
  if Assigned(FStream) then FStream.Free;
  FStream := nil;
  inherited destroy;
end;

function TBZSFDSDecompressionStream.Read(var Buffer;
  Count: LongInt): LongInt;
begin
  result := inherited Read(Buffer, Count);
  FPosition := FPosition + Result;
end;

{$IFNDEF VERSION_5_OR_BELLOW}
function TBZSFDSDecompressionStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
{$ELSE}
function TBZSFDSDecompressionStream.Seek(Offset: LongInt;
  Origin: Word): LongInt;
{$ENDIF}
begin
  if (Offset < FPosition) and (Origin = {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF}) then
     begin
       inherited Seek(0, {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF});
       result := inherited Seek(Offset, {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF});
       FPosition := Result;
       Exit;
     end;
  if (offset = 0) and (origin = {$IFNDEF VERSION_5_OR_BELLOW}soCurrent{$ELSE}soFromCurrent{$ENDIF}) then
  begin
    result := FPosition;
  end
  else
  if (offset = FPosition) and (origin = {$IFNDEF VERSION_5_OR_BELLOW}soBeginning{$ELSE}soFromBeginning{$ENDIF}) then
  begin
    result := FPosition;
  end
  else
  result := inherited Seek(Offset, Origin);
  FPosition := Result;
end;

{ TSFDSBZip2Compressor }

procedure TSFDSBZip2Compressor.SetSize(NewSize: Longint);
begin
  FBZip2CompressionStream.Size := NewSize;
end;

{$IFNDEF VERSION_5_OR_BELLOW}
procedure TSFDSBZip2Compressor.SetSize(const NewSize: Int64);
begin
  FBZip2CompressionStream.Size := NewSize;
end;
{$ENDIF}

constructor TSFDSBZip2Compressor.Create(SourceStream : TStream; CompressionLevel : TSFDSCompressionLevel; CompressionStrategy : TSFDSCompressionStrategy);
const
  BZ2CL : array[clFastest..clMax] of TBlockSize100k = (bs1, bs5, bs9);
begin
  if SourceStream = nil then raise ESFDSNilSourceStream.Create(SFDSNilStream);
  FBZip2CompressionStream := TBZCompressionStream.Create(BZ2CL[CompressionLevel], SourceStream);
end;

destructor TSFDSBZip2Compressor.Destroy;
begin
  if Assigned(FBZip2CompressionStream) then FBZip2CompressionStream.Free;
  inherited Destroy;
end;

{$IFNDEF VERSION_5_OR_BELLOW}
function TSFDSBZip2Compressor.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TSFDSBZip2Compressor.Seek(Offset: LongInt; Origin: Word): LongInt;
{$ENDIF}
begin
  result := FBZip2CompressionStream.Seek(Offset, Origin);
end;

function TSFDSBZip2Compressor.Read(var Buffer; Count: Longint): Longint;
begin
  result := FBZip2CompressionStream.Read(Buffer, Count);
end;

function TSFDSBZip2Compressor.Write(const Buffer; Count: Longint): Longint;
begin
  result := FBZip2CompressionStream.Write(Buffer, Count);
end;

{ TSFDSBZip2Decompressor }

procedure TSFDSBZip2Decompressor.SetSize(NewSize: Longint);
begin
  FBZip2DecompressionStream.Size := NewSize;
end;

{$IFNDEF VERSION_5_OR_BELLOW}
procedure TSFDSBZip2Decompressor.SetSize(const NewSize: Int64);
begin
  FBZip2DecompressionStream.Size := NewSize;
end;
{$ENDIF}

constructor TSFDSBZip2Decompressor.Create(SourceStream : TStream; SourceStreamSize : Int64);
begin
  if SourceStream = nil then raise ESFDSNilSourceStream.Create(SFDSNilStream);
  FBZip2DecompressionStream := TBZSFDSDecompressionStream.Create(SourceStream, SourceStreamSize);
end;

destructor TSFDSBZip2Decompressor.Destroy;
begin
  if Assigned(FBZip2DecompressionStream) then FBZip2DecompressionStream.Free;
  inherited Destroy;
end;

{$IFNDEF VERSION_5_OR_BELLOW}
function TSFDSBZip2Decompressor.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TSFDSBZip2Decompressor.Seek(Offset: LongInt; Origin: Word): LongInt;
{$ENDIF}
begin
  result := FBZip2DecompressionStream.Seek(Offset, Origin);
end;

function TSFDSBZip2Decompressor.Read(var Buffer; Count: Longint): Longint;
begin
  result := FBZip2DecompressionStream.Read(Buffer, Count);
end;

function TSFDSBZip2Decompressor.Write(const Buffer; Count: Longint): Longint;
begin
  result := FBZip2DecompressionStream.Write(Buffer, Count);
end;

initialization
  SFDS_RegisterCompressionFormat('BZip2', 2, TSFDSBZip2Compressor, TSFDSBZip2Decompressor);

end.
