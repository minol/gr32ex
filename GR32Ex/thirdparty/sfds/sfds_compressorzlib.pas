unit sfds_compressorzlib;
{$I sfds.inc}

{*****************************************************************************}
{                                                                             }
{  SFDS (Single File Data Storage) ZLib Compressor Unit                       }
{                                                                             }
{  For conditions of distribution and use, see LICENSE.TXT                    }
{                                                                             }
{*****************************************************************************}

interface
uses Classes, sfds,
     {$IFDEF FPC}
     sfds_paszlibex;     
     {$ELSE}
     sfds_zlibex;
     {$ENDIF}

type
  {An enhanced version of the TZDecompressionStream.<BR>
  Used internally.<BR>
  It allows full seeking in the compressed stream.<BR>
  Seeking backwards from the current position, requires seeking to 0, and then forward to the required position (decompressing data on the fly).}
  TZSFDSDecompressionStream = class(TZDecompressionStream)
  private
    FUnpackedFileSize : Int64;
    FPosition : Int64;
    FStream   : TStream;
  public
    constructor Create(source: TStream; unpsize : int64); reintroduce; overload;
    constructor Create(source: TStream; windowBits: Integer; unpsize : int64); reintroduce; overload;
    {$IFNDEF VERSION_5_OR_BELLOW}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    {$ENDIF}
    function Read(var Buffer; Count: LongInt): LongInt; override;
    destructor Destroy; override;
  end;

  TSFDSZLibCompressor = class(TSFDSCompressor)
  private
    FZLibCompressionStream : TZCompressionStream;
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

  TSFDSZLibDecompressor = class(TSFDSDecompressor)
  private
    FZLibDecompressionStream : TZSFDSDecompressionStream;
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

{ TZSFDSDecompressionStream }

constructor TZSFDSDecompressionStream.Create(source: TStream;
  unpsize: int64);
begin
  if Source = nil then raise ESFDSNilSourceStream.Create(SFDSNilStream);
  inherited Create(source);
  FUnpackedFileSize := unpsize;
  FPosition := 0;
  FStream := source;
end;

constructor TZSFDSDecompressionStream.Create(source: TStream;
  windowBits: Integer; unpsize: int64);
begin
  if Source = nil then raise ESFDSNilSourceStream.Create(SFDSNilStream);
  inherited Create(source, windowBits);
  FUnpackedFileSize := unpsize;
  FPosition := 0;
  FStream := source;
end;

destructor TZSFDSDecompressionStream.Destroy;
begin
  if Assigned(FStream) then FStream.Free;
  FStream := nil;
  inherited;
end;

function TZSFDSDecompressionStream.Read(var Buffer;
  Count: LongInt): LongInt;
begin
  result := inherited Read(Buffer, Count);
  FPosition := FPosition + Result;
end;

{$IFNDEF VERSION_5_OR_BELLOW}
function TZSFDSDecompressionStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
{$ELSE}
function TZSFDSDecompressionStream.Seek(Offset: LongInt;
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

{ TSFDSZLibCompressor }

procedure TSFDSZLibCompressor.SetSize(NewSize: Longint);
begin
  FZLibCompressionStream.Size := NewSize;
end;

{$IFNDEF VERSION_5_OR_BELLOW}
procedure TSFDSZLibCompressor.SetSize(const NewSize: Int64);
begin
  FZLibCompressionStream.Size := NewSize;
end;
{$ENDIF}

constructor TSFDSZLibCompressor.Create(SourceStream : TStream; CompressionLevel : TSFDSCompressionLevel; CompressionStrategy : TSFDSCompressionStrategy);
const
  ZCL : array[clFastest..clMax] of TZCompressionLevel = (zcFastest, zcDefault, zcMax);
  ZCS : array[csDefault..csFixed] of TZStrategy = (zsDefault, zsFiltered, zsHuffman, zsRLE, zsFixed);
begin
  if SourceStream = nil then raise ESFDSNilSourceStream.Create(SFDSNilStream);
  FZLibCompressionStream := TZCompressionStream.Create(SourceStream, ZCL[CompressionLevel], 15, 8, ZCS[CompressionStrategy]);
end;

destructor TSFDSZLibCompressor.Destroy;
begin
  if Assigned(FZLibCompressionStream) then FZLibCompressionStream.Free;
  inherited Destroy;
end;

{$IFNDEF VERSION_5_OR_BELLOW}
function TSFDSZLibCompressor.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TSFDSZLibCompressor.Seek(Offset: LongInt; Origin: Word): LongInt;
{$ENDIF}
begin
  result := FZLibCompressionStream.Seek(Offset, Origin);
end;

function TSFDSZLibCompressor.Read(var Buffer; Count: Longint): Longint;
begin
  result := FZLibCompressionStream.Read(Buffer, Count);
end;

function TSFDSZLibCompressor.Write(const Buffer; Count: Longint): Longint;
begin
  result := FZLibCompressionStream.Write(Buffer, Count);
end;

{ TSFDSZLibDecompressor }

procedure TSFDSZLibDecompressor.SetSize(NewSize: Longint);
begin
  FZLibDecompressionStream.Size := NewSize;
end;

{$IFNDEF VERSION_5_OR_BELLOW}
procedure TSFDSZLibDecompressor.SetSize(const NewSize: Int64);
begin
  FZLibDecompressionStream.Size := NewSize;
end;
{$ENDIF}

constructor TSFDSZLibDecompressor.Create(SourceStream : TStream; SourceStreamSize : Int64);
begin
  if SourceStream = nil then raise ESFDSNilSourceStream.Create(SFDSNilStream);
  FZLibDecompressionStream := TZSFDSDecompressionStream.Create(SourceStream, SourceStreamSize);
end;

destructor TSFDSZLibDecompressor.Destroy;
begin
  if Assigned(FZLibDecompressionStream) then FZLibDecompressionStream.Free;
  inherited Destroy;
end;

{$IFNDEF VERSION_5_OR_BELLOW}
function TSFDSZLibDecompressor.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TSFDSZLibDecompressor.Seek(Offset: LongInt; Origin: Word): LongInt;
{$ENDIF}
begin
  result := FZLibDecompressionStream.Seek(Offset, Origin);
end;

function TSFDSZLibDecompressor.Read(var Buffer; Count: Longint): Longint;
begin
  result := FZLibDecompressionStream.Read(Buffer, Count);
end;

function TSFDSZLibDecompressor.Write(const Buffer; Count: Longint): Longint;
begin
  result := FZLibDecompressionStream.Write(Buffer, Count);
end;

initialization
  SFDS_RegisterCompressionFormat('ZLib (Deflate)', 1, TSFDSZLibCompressor, TSFDSZLibDecompressor);

end.
