unit E_StgStr;
{-------------} Interface {--------------------}
Uses
    ActiveX, SysUtils, Windows;
(*******************************************************************
                           E_StgStor
    Ole Structured Storage Helper Routines.
    This unit has been modified relative to changes to Delphi 3
    and will not compile properly under Delphi 2. In particular,
    Delphi 2 used a vtable interface to IStorage and IStream and
    D3 uses StdCall. IUnknown has become a system interface and
    (although I have no first hand knowledge of this) it is no
    longer necessary to call Release direcly, rather the Interfaces
    are released when they go out of context. If anyone knows to
    the contrary, I will be more than humble if you correct me.
*******************************************************************)
Type
      TStructuredStorage = Class
       Private
         FFileName : String;
       Public
         hr          : HResult;
         StorageFile : IStorage;
         Constructor Create ; Virtual;
         Destructor Destroy; Override;

         Function IsStorageFile(Const FileName : String) : Boolean;

         Function CreateStorageFile(Const FileName : String;
                                          grfMode  : LongInt) : Boolean;

         Function OpenStorageFile(Const FileName : String;
                                        grfMode  : LongInt) : Boolean;

         Function CreateSubStorage(Const FileName    : String;
                                         grfMode     : LongInt;
                                   Var   Storage     : IStorage;
                                   Var   SubStorage  : IStorage) : Boolean;

         Function OpenSubStorage(Const FileName   : String;
                                       grfMode    : LongInt;
                                 Var   Storage    : IStorage;
                                 Var   SubStorage : IStorage) : Boolean;

         Procedure DoneStorage(Var Storage : IStorage);

         Function CreateStream(Var   Storage     : IStorage;
                               Const StreamName  : String;
                                     grfMode     : LongInt;
                               Var   Stream      : IStream) : Boolean;

         Function OpenStream(Var   Storage     : IStorage;
                             Const StreamName  : String;
                                   grfMode     : LongInt;
                             Var   Stream      : IStream) : Boolean;

         Procedure DoneStream(Var Stream : IStream);

         Function WriteString(Var   Stream : IStream;
                              Const S      : String) : Boolean;
         Function ReadString(Var Stream    : IStream;
                             Var S         : String) : Boolean;

         Function WriteInt(Var Stream : IStream;
                               TInt   : Integer) : Boolean;

         Function ReadInt(Var Stream    : IStream;
                          Var TInt      : Integer) : Boolean;
      end;
{-------------} Implementation {---------------}
(*
  STGM_DIRECT           = $00000000;
  STGM_TRANSACTED       = $00010000;
  STGM_SIMPLE           = $08000000;

  STGM_READ             = $00000000;
  STGM_WRITE            = $00000001;
  STGM_READWRITE        = $00000002;

  STGM_SHARE_DENY_NONE  = $00000040;
  STGM_SHARE_DENY_READ  = $00000030;
  STGM_SHARE_DENY_WRITE = $00000020;
  STGM_SHARE_EXCLUSIVE  = $00000010;

  STGM_PRIORITY         = $00040000;
  STGM_DELETEONRELEASE  = $04000000;

  STGM_CREATE           = $00001000;
  STGM_CONVERT          = $00020000;
  STGM_FAILIFTHERE      = $00000000;
*)
(*******************************************************************
 Structured Storage Object to help relieve some of pain when dealing
 with some of it's peculiarities.
*******************************************************************)
Constructor TStructuredStorage.Create;
begin
  Inherited Create;
  StorageFile:=Nil;
end;
Destructor TStructuredStorage.Destroy;
begin
 { If Assigned(StorageFile) then
    StorageFile.Release;  DONT NEED THIS IN D3, SIMPLY LET IT GO OUT OF CONTEXT}
  Inherited Destroy;
end;
(*******************************************************************
                           CreateStorageFile
 Create an Ole Structured Storage File
*******************************************************************)
Function TStructuredStorage.CreateStorageFile(Const FileName : String;
                                                    grfMode  : LongInt) : Boolean;
begin
  Hr:=StgCreateDocFile(PWideChar(WideString(FileName)),grfMode,0,StorageFile);
  Result:=Hr=S_OK;
end;
Function DidFail(hr : Hresult) : Boolean;
begin
  Result:=ActiveX.Failed(hr);
end;
(*******************************************************************
                           CreateSubStorage
 Create Sub Storage within a Storage.
*******************************************************************)
Function TStructuredStorage.CreateSubStorage(Const FileName    : String;
                                                   grfMode     : LongInt;
                                             Var   Storage     : IStorage;
                                             Var   SubStorage  : IStorage) : Boolean;
begin
  hr:=Storage.CreateStorage(PWideChar(WideString(FileName)),grfMode,0,0,SubStorage);
  Result := NOT DidFail(hr);
end;
(*******************************************************************
                           IsStorageFile
 See if FileName is indeed a structured storage file.
*******************************************************************)
Function TStructuredStorage.IsStorageFile(Const FileName : String) : Boolean;
begin
  hr:=StgIsStorageFile(PWideChar(WideString(FileName)));
  Result:=NOT DidFail(hr);
end;
(*******************************************************************
                           OpenStorageFile
 Open an Ole Structured Storage File
*******************************************************************)
Function TStructuredStorage.OpenStorageFile(Const FileName : String;
                                                  grfMode  : LongInt) : Boolean;
begin
  hr:=StgIsStorageFile(PWideChar(WideString(FileName)));
  If DidFail(hr) then
    Result:= false
    {Raise}
  else
  begin
    Hr:=StgOpenStorage(PWideChar(WideString(FileName)),Nil,grfMode,Nil,LongInt(Nil),StorageFile);
    Result:=Hr=S_OK;
  end;
end;
(*******************************************************************
                           OpenSubStorage
 Open an Ole Structured sub-Storage within a storage
*******************************************************************)
Function TStructuredStorage.OpenSubStorage(Const FileName   : String;
                                                 grfMode    : LongInt;
                                           Var   Storage    : IStorage;
                                           Var   SubStorage : IStorage) : Boolean;
begin
  hr:=Storage.OpenStorage(PWideChar(WideString(FileName)),Nil,grfMode,Nil,LongInt(Nil),SubStorage);
  Result := NOT DidFail(hr);
end;
(*******************************************************************
 Free up an OLE storage.
*******************************************************************)
Procedure TStructuredStorage.DoneStorage(Var Storage : IStorage);
begin
(*  If Assigned(Storage) then
      Storage.Release;
     DONT NEED THIS IN D3, SIMPLY LET IT GO OUT OF CONTEXT*)
end;
(*******************************************************************
 Create an Ole Stream within the current storage
*******************************************************************)
Function TStructuredStorage.CreateStream(Var   Storage     : IStorage;
                                         Const StreamName  : String;
                                               grfMode     : LongInt;
                                         Var   Stream      : IStream) : Boolean;
begin
  Hr:=Storage.CreateStream(PWideChar(WideString(StreamName)),grfMode,0,0,Stream);
  Result:=NOT DidFail(hr);
end;
(*******************************************************************
 Create an Ole Stream within the current storage
*******************************************************************)
Function TStructuredStorage.OpenStream(Var   Storage     : IStorage;
                                       Const StreamName  : String;
                                             grfMode     : LongInt;
                                       Var   Stream      : IStream) : Boolean;
begin
  Hr:=Storage.OpenStream(PWideChar(WideString(StreamName)),Nil,grfMode,0,Stream);
  Result:=NOT DidFail(hr);
end;
(*******************************************************************
 Free up an OLE stream.
*******************************************************************)
Procedure TStructuredStorage.DoneStream(Var Stream : IStream);
begin
{  If Assigned(Stream) then
    Stream.Release;
  Stream:=Nil; DONT NEED THIS IN D3, SIMPLY LET IT GO OUT OF CONTEXT}
end;
(*******************************************************************
                           WriteString
 Write a string to an opened storage stream.
*******************************************************************)
Function TStructuredStorage.WriteString(Var   Stream : IStream;
                                        Const S      : String) : Boolean;
Var
    Size : LongInt;
begin
  Result:=False;
  If WriteInt(Stream,Length(S)) then
  begin
    Size:=0;
    hr:=Stream.Write(PChar(S),Length(S),@Size);
    If DidFail(hr) then
      {Raise}
    else
      Result:=Size=Length(S);
  end;
end;
(*******************************************************************
                           ReadString
 Read a string from an opened storage stream.
*******************************************************************)
Function TStructuredStorage.ReadString(Var Stream    : IStream;
                                       Var S         : String) : Boolean;
Var
    StrSize : Integer;
    Size    : LongInt;
    Ps      : PChar;
begin
  Result:=False;
  S:='';
  If ReadInt(Stream,StrSize) then
  begin
    Size:=0;
    try
       GetMem(Ps,StrSize+1);
       hr:=Stream.Read(Ps,StrSize,@Size);
       If DidFail(hr) then
        {Raise}
       else If Size>0 then
       begin
         S:=String(Ps);
         SetLength(S,Size);
         Result:=Size=StrSize;
       end;
    finally
      FreeMem(Ps,StrSize+1);
    end;
  end;
end;
(*******************************************************************
                           WriteInt
 Write an Integer to the stream.
*******************************************************************)
Function TStructuredStorage.WriteInt(Var Stream : IStream;
                                         TInt   : Integer) : Boolean;
Var
    Size : LongInt;
begin
  Result:=False;
  Size:=0;
  hr:=Stream.Write(@TInt,SizeOf(Integer),@Size);
  If DidFail(hr) then
    {Raise}
  else
    Result:=Size=SizeOf(Integer);
end;
(*******************************************************************
                           ReadInt
 Read a string from an opened storage stream.
*******************************************************************)
Function TStructuredStorage.ReadInt(Var Stream    : IStream;
                                    Var TInt      : Integer) : Boolean;
Var
    Size : LongInt;
begin
  Result:=False;
  Size:=0;
  hr:=Stream.Read(@Tint,SizeOf(Integer),@Size);
  If DidFail(hr) then
    {Raise}
  else
    Result:=Size=SizeOf(Integer);
end;
{--------------------------END OF FILE--------------------------------}
end.
