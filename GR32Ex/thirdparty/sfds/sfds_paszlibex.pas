unit sfds_paszlibex;

{*****************************************************************************}
{                                                                             }
{  SFDS (Single File Data Storage) Unit                                       }
{                                                                             }
{  For conditions of distribution and use, see LICENSE.TXT                    }
{                                                                             }
{      This contains only code from sfds_zlibex.pas(which in turn is adapted  }
{  from zlibex.pas) adapted to work with paszlib instead of .obj files.       }
{  Designed to be used only with FPC					      }
{                                                                             }
{                                                                             }
{*****************************************************************************}

{$IFNDEF FPC}
{$ERROR This unit can only be used when compiling with FPC}
{$ENDIF}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, paszlib;

type
  TZStreamRec = TZStream;

  TZCompressionLevel = (
    zcNone,
    zcFastest,
    zcDefault,
    zcMax,
    zcLevel1,
    zcLevel2,
    zcLevel3,
    zcLevel4,
    zcLevel5,
    zcLevel6,
    zcLevel7,
    zcLevel8,
    zcLevel9
  );

  TZStrategy = (
    zsDefault,
    zsFiltered,
    zsHuffman,
    zsRLE,
    zsFixed
  );
  
  {** TCustomZStream ********************************************************}

  TCustomZStream = class(TStream)
  private
    FStream    : TStream;
    FStreamPos : Int64;
    FOnProgress: TNotifyEvent;
    FZStream   : TZStreamRec;
    FBuffer    : Array [Word] of Char;
  protected
    constructor Create(stream: TStream);
    procedure DoProgress; dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  {** TZCompressionStream ***************************************************}

  TZCompressionStream = class(TCustomZStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(dest: TStream;
      compressionLevel: TZCompressionLevel = zcDefault); overload;
    constructor Create(dest: TStream; compressionLevel: TZCompressionLevel;
      windowBits, memLevel: Integer; strategy: TZStrategy); overload;
    destructor  Destroy; override;
    function  Read(var buffer; count: Longint): Longint; override;
    function  Write(const buffer; count: Longint): Longint; override;
    function  Seek(offset: Longint; origin: Word): Longint; override;
    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

  {** TZDecompressionStream *************************************************}

  TZDecompressionStream = class(TCustomZStream)
  public
    constructor Create(source: TStream); overload;
    constructor Create(source: TStream; windowBits: Integer); overload;
    destructor  Destroy; override;
    function  Read(var buffer; count: Longint): Longint; override;
    function  Write(const buffer; count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property OnProgress;
  end;
  
  { EZLibError }

  EZLibError = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(code: Integer); overload;

    property ErrorCode: Integer read FErrorCode write FErrorCode;
  end;
  
  EZCompressionError = class(EZLibError);
  EZDecompressionError = class(EZLibError);

  const
  _z_errmsg: array[0..9] of PChar = (
    'need dictionary',      // Z_NEED_DICT      (2)
    'stream end',           // Z_STREAM_END     (1)
    'ok',                   // Z_OK             (0)
    'file error',           // Z_ERRNO          (-1)
    'stream error',         // Z_STREAM_ERROR   (-2)
    'data error',           // Z_DATA_ERROR     (-3)
    'insufficient memory',  // Z_MEM_ERROR      (-4)
    'buffer error',         // Z_BUF_ERROR      (-5)
    'incompatible version', // Z_VERSION_ERROR  (-6)
    ''
  );

  ZLevels: Array [TZCompressionLevel] of Shortint = (
    Z_NO_COMPRESSION,       // zcNone
    Z_BEST_SPEED,           // zcFastest
    Z_DEFAULT_COMPRESSION,  // zcDefault
    Z_BEST_COMPRESSION,     // zcMax
    1,                      // zcLevel1
    2,                      // zcLevel2
    3,                      // zcLevel3
    4,                      // zcLevel4
    5,                      // zcLevel5
    6,                      // zcLevel6
    7,                      // zcLevel7
    8,                      // zcLevel8
    9                       // zcLevel9
  );
  
  Z_RLE                 = 3;
  Z_FIXED               = 4;

  ZStrategies: Array [TZStrategy] of Shortint = (
    Z_DEFAULT_STRATEGY,     // zsDefault
    Z_FILTERED,             // zsFiltered
    Z_HUFFMAN_ONLY,         // zsHuffman
    Z_RLE,                  // zsRLE
    Z_FIXED                 // zsFixed
  );

  SZInvalid = 'Invalid ZStream operation!';
  
  function ZCompressCheck(code: Integer): Integer;
  function ZDecompressCheck(code: Integer): Integer;

implementation

function ZCompressCheck(code: Integer): Integer;
begin
  result := code;

  if code < 0 then
  begin
    raise EZCompressionError.Create(code);
  end;
end;

function ZDecompressCheck(code: Integer): Integer;
begin
  Result := code;

  if code < 0 then
  begin
    raise EZDecompressionError.Create(code);
  end;
end;

{** TCustomZStream **********************************************************}

constructor TCustomZStream.Create(stream: TStream);
begin
  inherited Create;

  FStream := stream;
  FStreamPos := stream.Position;
end;

procedure TCustomZStream.DoProgress;
begin
  if Assigned(FOnProgress) then FOnProgress(Self);
end;

{** TZCompressionStream *****************************************************}

constructor TZCompressionStream.Create(dest: TStream;
  compressionLevel: TZCompressionLevel);
begin
  inherited Create(dest);

  FZStream.next_out := @FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(DeflateInit(FZStream,ZLevels[compressionLevel]));
end;

constructor TZCompressionStream.Create(dest: TStream;
  compressionLevel: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy);
begin
  inherited Create(dest);

  FZStream.next_out := @FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(DeflateInit2(FZStream,ZLevels[compressionLevel],Z_DEFLATED,
    windowBits,memLevel,ZStrategies[strategy]));
end;

destructor TZCompressionStream.Destroy;
begin
  FZStream.next_in := Nil;
  FZStream.avail_in := 0;

  try
    if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

    while ZCompressCheck(deflate(FZStream,Z_FINISH)) <> Z_STREAM_END do
    begin
      FStream.WriteBuffer(FBuffer,SizeOf(FBuffer) - FZStream.avail_out);

      FZStream.next_out := @FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);
    end;

    if FZStream.avail_out < SizeOf(FBuffer) then
    begin
      FStream.WriteBuffer(FBuffer,SizeOf(FBuffer) - FZStream.avail_out);
    end;
  finally
    deflateEnd(FZStream);
  end;

  inherited Destroy;
end;

function TZCompressionStream.Read(var buffer; count: Longint): Longint;
begin
  raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.Write(const buffer; count: Longint): Longint;
begin
  FZStream.next_in := @buffer;
  FZStream.avail_in := count;

  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  while FZStream.avail_in > 0 do
  begin
    ZCompressCheck(deflate(FZStream,Z_NO_FLUSH));

    if FZStream.avail_out = 0 then
    begin
      FStream.WriteBuffer(FBuffer,SizeOf(FBuffer));

      FZStream.next_out := @FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);

      FStreamPos := FStream.Position;

      DoProgress;
    end;
  end;

  result := Count;
end;

function TZCompressionStream.Seek(offset: Longint; origin: Word): Longint;
begin
  if (offset = 0) and (origin = soFromCurrent) then
  begin
    result := FZStream.total_in;
  end
  else raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.GetCompressionRate: Single;
begin
  if FZStream.total_in = 0 then result := 0
  else result := (1.0 - (FZStream.total_out / FZStream.total_in)) * 100.0;
end;

{** TZDecompressionStream ***************************************************}

constructor TZDecompressionStream.Create(source: TStream);
begin
  inherited Create(source);

  FZStream.next_in := @FBuffer;
  FZStream.avail_in := 0;

  ZDecompressCheck(InflateInit(FZStream));
end;

constructor TZDecompressionStream.Create(source: TStream;
  windowBits: Integer);
begin
  inherited Create(source);

  FZStream.next_in := @FBuffer;
  FZStream.avail_in := 0;

  ZDecompressCheck(InflateInit2(FZStream,windowBits));
end;

destructor TZDecompressionStream.Destroy;
begin
  inflateEnd(FZStream);

  inherited Destroy;
end;

function TZDecompressionStream.Read(var buffer; count: Longint): Longint;
var
  zresult: Integer;
begin
  FZStream.next_out := @buffer;
  FZStream.avail_out := count;

  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  zresult := Z_OK;

  while (FZStream.avail_out > 0) and (zresult <> Z_STREAM_END) do
  begin
    if FZStream.avail_in = 0 then
    begin
      FZStream.avail_in := FStream.Read(FBuffer,SizeOf(FBuffer));

      if FZStream.avail_in = 0 then
      begin
        result := count - FZStream.avail_out;

        Exit;
      end;

      FZStream.next_in := @FBuffer;
      FStreamPos := FStream.Position;

      DoProgress;
    end;

    zresult := ZDecompressCheck(inflate(FZStream,Z_NO_FLUSH));
  end;

  if (zresult = Z_STREAM_END) and (FZStream.avail_in > 0) then
  begin
    FStream.Position := FStream.Position - FZStream.avail_in;
    FStreamPos := FStream.Position;

    FZStream.avail_in := 0;
  end;

  result := count - FZStream.avail_out;
end;

function TZDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EZDecompressionError.Create(SZInvalid);
end;

function TZDecompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  buf: Array [0..8191] of Char;
  i  : Integer;
  TmpOffset : Int64;
begin
  if (offset = 0) and (origin = soBeginning) then
  begin
    ZDecompressCheck(inflateReset(FZStream));

    FZStream.next_in := @FBuffer;
    FZStream.avail_in := 0;

    FStream.Position := 0;
    FStreamPos := 0;
  end
  else if ((offset >= 0) and (origin = soCurrent)) or
          (((offset - FZStream.total_out) > 0) and (origin = soBeginning)) then
  begin
    TmpOffset := Offset;
    if origin = soBeginning then Dec(TmpOffset,FZStream.total_out);

    if TmpOffset > 0 then
    begin
      for i := 1 to TmpOffset div SizeOf(buf) do ReadBuffer(buf,SizeOf(buf));
      ReadBuffer(buf,TmpOffset mod SizeOf(buf));
    end;
  end
  else if (offset = 0) and (origin = soEnd) then
  begin
    while Read(buf,SizeOf(buf)) > 0 do ;
  end
  else raise EZDecompressionError.Create(SZInvalid);

  result := FZStream.total_out;
end;

{ EZLibError }

constructor EZLibError.Create(code: Integer);
begin
  inherited Create(_z_errmsg[2 - code]);

  FErrorCode := code;
end;

end.

