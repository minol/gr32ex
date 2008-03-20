unit sfds_streamingutils;
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
{$I sfds.inc}

{*****************************************************************************}
{                                                                             }
{  SFDS (Single File Data Storage) Streaming Utils Unit                       }
{                                                                             }
{  For conditions of distribution and use, see LICENSE.TXT                    }
{                                                                             }
{*****************************************************************************}

interface
uses Classes, {$IFDEF VERSION_5_OR_BELLOW}Consts, sfds_utf8_d5_utils{$ELSE}RtlConsts{$ENDIF};

  const
  su_vaInt8 = 2;
  su_vaInt16 = 3;
  su_vaInt32 = 4;
  su_vaString = 6;
  su_vaBinary = 10;
  su_vaLString = 12;
  su_vaDate = 17;
  su_vaWString = 18;
  su_vaInt64 = 19;
  su_vaUTF8String = 20;

procedure WriteString(Stream : TStream; Value : WideString); overload;
procedure WriteString(Stream : TStream; Value : AnsiString); overload;
function ReadString(Stream : TStream) : AnsiString;
function ReadWideString(Stream : TStream) : WideString;
procedure SkipValue(Stream : TStream);
procedure WriteInteger(Stream : TStream; Int : LongInt); overload;
procedure WriteInteger(Stream : TStream; Int : Int64); overload;
function ReadInteger(Stream : TStream) : Longint;
function ReadInt64(Stream : TStream) : Int64;
procedure WriteDateTime(Stream : TStream; const Value: TDateTime);
function ReadDateTime(Stream : TStream) : TDateTime;

procedure WriteBinary(Stream : TStream; const Buffer; Count: Longint);
function ReadBinarySize(Stream : TStream) : Longint;

implementation

procedure WriteValue(Stream : TStream; const Value: Byte);
begin
  Stream.WriteBuffer(Value, SizeOf(Byte));
end;

function ReadValue(Stream : TStream) : Byte;
begin
  Stream.ReadBuffer(Result, SizeOf(Byte));
end;

procedure WriteMinStr(Stream : TStream; const LocaleStr: AnsiString; const UTF8Str: UTF8String);
var
  L : Longint;
begin
  if LocaleStr <> UTF8Str then
  begin
    L := Length(UTF8Str);
    WriteValue(Stream, su_vaUtf8String);
    Stream.WriteBuffer(L, SizeOf(Longint));
    Stream.WriteBuffer(Pointer(UTF8Str)^, L);
  end
  else
  begin
    L := Length(LocaleStr);
    if L <= 255 then
    begin
      WriteValue(Stream, su_vaString);
      Stream.WriteBuffer(L, SizeOf(Byte));
    end else
    begin
      WriteValue(Stream, su_vaLString);
      Stream.WriteBuffer(L, SizeOf(Longint));
    end;
    Stream.WriteBuffer(Pointer(LocaleStr)^, L);
  end;
end;

procedure WriteInteger(Stream : TStream; Int : LongInt);
begin
  if (Int >= Low(Shortint)) and (Int <= High(Shortint)) then
  begin
    Writevalue(Stream, su_vaInt8);
    Stream.WriteBuffer(Int, SizeOf(Shortint));
  end
  else
  if (Int >= Low(Smallint)) and (Int <= High(Smallint)) then
  begin
    WriteValue(Stream, su_vaInt16);
    Stream.WriteBuffer(Int, SizeOf(Smallint));
  end
  else
  begin
    WriteValue(Stream, su_vaInt32);
    Stream.WriteBuffer(Int, SizeOf(Longint));
  end
end;

procedure WriteInteger(Stream : TStream; Int : Int64);
begin
  if (Int >= Low(LongInt)) and (Int <= High(LongInt)) then
    WriteInteger(Stream, Longint(Int))
  else
  begin
    WriteValue(Stream, su_vaInt64);
    Stream.WriteBuffer(Int, SizeOf(Int64));
  end;
end;

procedure ReadError(Ident: PResStringRec);
begin
  raise EReadError.CreateRes(Ident);
end;

procedure PropValueError;
begin
  ReadError(@SInvalidPropertyValue);
end;

function ReadInteger(Stream : TStream) : Longint;
var
  V : Byte;
begin
  result := 0;
  V := ReadValue(Stream);
  case V of
    su_vaInt8:  Stream.ReadBuffer(result, SizeOf(Shortint));
    su_vaInt16: Stream.ReadBuffer(result, SizeOf(Smallint));
    su_vaInt32: Stream.ReadBuffer(result, SizeOf(Longint));
  else
    PropValueError;
  end;
end;

function ReadInt64(Stream : TStream) : Int64;
var
  V : Byte;
begin
  Result := 0;
  V := ReadValue(Stream);
  case V of
    su_vaInt8:  Stream.ReadBuffer(result, SizeOf(Shortint));
    su_vaInt16: Stream.ReadBuffer(result, SizeOf(Smallint));
    su_vaInt32: Stream.ReadBuffer(result, SizeOf(Longint));
    su_vaint64: Stream.ReadBuffer(result, SizeOf(Int64));
  else
    PropValueError;
  end;
end;

procedure WriteString(Stream : TStream; Value : AnsiString);
begin
  WriteMinStr(Stream, Value, AnsiToUtf8(Value));
end;

procedure WriteString(Stream : TStream; Value : WideString);
var Len : Longint;
begin
  Len := Length(Value);
  WriteValue(Stream, su_vaWString);
  Stream.WriteBuffer(Len, SizeOf(Longint));
  Stream.WriteBuffer(Pointer(Value)^, Len * 2);
end;

function ReadString(Stream : TStream) : AnsiString;
var V : Byte;
    L : Longint;
    Temp: UTF8String;
begin;
  L := 0; Result := ''; Temp := '';
  V := ReadValue(Stream);
  case V of
    su_vaUTF8String :
       begin
         Stream.ReadBuffer(L, SizeOf(Longint));
         SetLength(Temp, L);
         Stream.ReadBuffer(Pointer(Temp)^, L);
         Result := Utf8Decode(Temp);
       end;
    su_vaWString :
       begin
         Stream.ReadBuffer(L, SizeOf(Longint));
         SetLength(Result, L * 2);
         Stream.ReadBuffer(Pointer(Result)^, L * 2);
       end;
    su_vaString :
       begin
         Stream.ReadBuffer(L, SizeOf(Byte));
         SetLength(Result, L);
         Stream.ReadBuffer(Pointer(Result)^, L);
       end;
    su_vaLString :
       begin
         Stream.ReadBuffer(L, SizeOf(Longint));
         SetLength(Result, L);
         Stream.ReadBuffer(Pointer(Result)^, L);
       end;
  else
    PropValueError;
  end;     
end;

function ReadWideString(Stream : TStream) : WideString;
var V : Byte;
    L : Longint;
    Temp: UTF8String;
    TmpStr : AnsiString;
begin
  L := 0; Result := ''; Temp := ''; TmpStr := '';
  V := ReadValue(Stream);
  case V of
    su_vaUTF8String :
       begin
         Stream.ReadBuffer(L, SizeOf(Longint));
         SetLength(Temp, L);
         Stream.ReadBuffer(Pointer(Temp)^, L);
         Result := Utf8Decode(Temp);
       end;
    su_vaWString :
       begin
         Stream.ReadBuffer(L, SizeOf(Longint));
         SetLength(Result, L);
         Stream.ReadBuffer(Pointer(Result)^, L * 2);
       end;
    su_vaString :
       begin
         Stream.ReadBuffer(L, SizeOf(Byte));
         SetLength(TmpStr, L);
         Stream.ReadBuffer(Pointer(TmpStr)^, L);
         result := TmpStr;
       end;
    su_vaLString :
       begin
         Stream.ReadBuffer(L, SizeOf(Longint));
         SetLength(TmpStr, L);
         Stream.ReadBuffer(Pointer(TmpStr)^, L);
         result := TmpStr;         
       end;
  else
    PropValueError;
  end;
end;

procedure WriteBinary(Stream : TStream; const Buffer; Count: Longint);
begin
   WriteValue(Stream, su_vaBinary);
   WriteInteger(Stream, Count);
   Stream.WriteBuffer(Buffer, Count);
end;

function ReadBinarySize(Stream : TStream) : Integer;
var V : Byte;
begin
   V := ReadValue(Stream);
   if V <> su_vaBinary then PropValueError;
   Result := ReadInteger(Stream);
end;

procedure WriteDateTime(Stream : TStream; const Value: TDateTime);
begin
  WriteValue(Stream, su_vaDate);
  Stream.WriteBuffer(Value, SizeOf(TDateTime));
end;

function ReadDateTime(Stream : TStream) : TDateTime;
begin
  if ReadValue(Stream) <> su_vaDate then PropValueError;
  Stream.ReadBuffer(Result, SizeOf(TDateTime));
end;

procedure SkipValue(Stream : TStream);
var L : Longint;
    I64 : Int64;
    DT : TDateTime;
begin
  L := 0; I64 := 0;
  case ReadValue(Stream) of
    su_vaInt8 :  Stream.ReadBuffer(I64, SizeOf(ShortInt));
    su_vaInt16 : Stream.ReadBuffer(I64, SizeOf(SmallInt));
    su_vaInt32 : Stream.ReadBuffer(I64, SizeOf(Longint));
    su_vaInt64 : Stream.ReadBuffer(I64, SizeOf(Int64));
    su_vaDate :  Stream.ReadBuffer(DT, SizeOf(TDateTime));
    su_vaString :
       begin
         Stream.ReadBuffer(L, SizeOf(Byte));
         Stream.Position := Stream.Position + L;
       end;
    su_vaWString :
       begin
         Stream.ReadBuffer(L, SizeOf(Longint));
         Stream.Position := Stream.Position + (L * 2);
       end;
    su_vaUTF8String, su_vaLString:
       begin
         Stream.ReadBuffer(L, SizeOf(Longint));
         Stream.Position := Stream.Position + L;
       end;
    su_vaBinary :
       begin
         L := ReadInteger(Stream);
         Stream.Position := Stream.Position + L;
       end;
  else
    PropValueError;
  end
end;

end.
