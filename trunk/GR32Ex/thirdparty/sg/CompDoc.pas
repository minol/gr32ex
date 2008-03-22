(*
                     Compound Documents v1.01
                     ~~~~~~~~~~~~~~~~~~~~~~~~
                       Robert R. Marsh, SJ
                        rrm@sprynet.com
                http://home.sprynet.com/sprynet/rrm/

   Compound Documents, or OLE Structured Storage, provide an
   ingenious and easy way to effectively create a full file-system
   within a file. A compound document functions as a 'directory'
   (or root storage in the lingo) which can contain 'sub-directories'
   (aka storages) and/or 'files' (aka streams).
   Compound documents also have the ability to automatically buffer
   any changes until they are committed or rolled back.

   Unfortunately, the association with OLE/ActiveX keeps many Delphi
   users away. But while the details can be messy there is no deep
   difficulty. Some Delphi encapsulations of compound files are
   already available but either cost big bucks or mirror the
   underlying API too closely with all its arcane flags many of which
   are mutually exclusive. The components presented here encapsulate
   the OLE structured storage API in a what is, I hope, a Delphi-
   friendly manner, free from all the OLE clutter. What's more they
   work in all three versions of Delphi (see below for a caveat).

   A TRootStorage object corresponds to a physical file on disk.
   Other TStorage objects correspond to sub-storages of a root
   storage. Apart from their mode of construction both objects have
   similar behavior. They have methods to manage the sub-storages
   and streams they contain (CopyElement, MoveElement, RenameElement,
   DeleteElement, CopyTo, ListStorages, ListStreams) and methods to
   handle transaction processing (Commit, Revert).

   TStorageStream objects always belong to a parent storage object.
   They are fully compatible with Delphi's other stream objects.
   Despite the impression given in many descriptions transaction
   processing does not work at the stream level but only for storages.

   Transaction processing operates by publishing any changes visible
   at one level in the storage hierarchy to the parent level. A
   storage has no knowledge of changes made at a deeper level until
   they percolate upwards through a series of Commit operations.
   When a root storage commits its changes they are written to the
   physical file.

   Both storages and streams can be created as temporary objects by
   providing no Name parameter. A unique name is generated by Windows
   and is available through the Name property. Such objects are self-
   deleting.

   The OLE documentation warns that compound files are optimized
   for common operations (like the reading and writing of streams)
   and that other operations (especially those involving the
   enumeration of storage elements) can be slow. Although I have
   provided some enumeration methods (ListStorages, ListStreams) you
   will get better performance if you create a separate stream to
   store such information for yourself. In general, I have found
   read/write operations to be about 2 to 3 times slower than
   equivalent operations on 'ordinary' file streams. Not bad
   considering the extra functionality.

   You can find out more about Compound Documents / OLE Structured
   Storage from the excellent book "Inside OLE" (2nd ed.) by Kraig
   Brockschmidt (Microsoft Press) or from the Microsoft Developers
   Network library (via http://microsoft.com/msdn/). Good luck!

   One of the benefits of these components is that someone has read
   the small print for you and made many illegal operations
   impossible. I realize, however, that I have probably misread in
   some cases. So if you find problems with this code please let me
   know at the address above so that I can learn from my mistakes.

   I referred above to a caveat regarding the use of these components
   with Delphi 1. There are two issues. First, as I understand it, OLE2
   came on the scene after Windows 3.1 so that plain vanilla
   installations don't include the necessary OLE dlls. Nevertheless,
   it would be rare to find a machine that hasn't had the OLE2 files
   added by one application or another. The second issue has more to
   do with Borland. The OLE2 DCU and PAS files they supplied with D1
   seem to be contain errors (even on the D2 and D3 CDs). I have taken
   the liberty of correcting the problems which pertain to Compound
   Documents and also changed some of the flag declaration to bring them
   more into line with D2 and D3. The result is a file called OLE2_16
   which must be used with CompDoc.DCU under Delphi 1. Other versions
   of Delphi can ignore this file.

   If you like these components and find yourself using them please
   consider making a donation to your favorite charity. I would also
   be pleased if you would make acknowledgement in any projects that
   make use of them.

   These components are supplied as is. The author disclaims all
   warranties, expressed or implied, including, without limitation,
   the warranties of merchantability and of fitness for any purpose.
   The author assumes no liability for damages, direct or
   consequential, which may result from their use.

         Copyright (c) 1998-2001 Robert R. Marsh, S.J. &
          the British Province of the Society of Jesus

*)

unit CompDoc;

interface

{$IFNDEF VER80}{$IFNDEF VER90}
{$DEFINE VER3PLUS}
{$IFNDEF VER100}
{$DEFINE VER4PLUS}
{$ENDIF}
{$ENDIF}{$ENDIF}

uses
  {$IFDEF WIN32}Windows{$ELSE}WinTypes,
  WinProcs{$ENDIF},
  {$IFDEF VER90}OLE2{$ENDIF}
  {$IFDEF VER80}OLE2_16{$ENDIF}
  {$IFDEF VER3PLUS}ActiveX{$ENDIF},
  SysUtils,
  Classes;

{ These Mode flags govern the creation and opening of storages }
{ and streams. Note that some constructors use only some of    }
{ them.                                                        }
type
  { Corresponds to fmOpenRead etc. but applies to root storages, }
  { storages, and streams. An inner element should not have be   }
  { given a more permissive access mode than its parent storage. }
  { However, in transacted mode no conflict will arise until     }
  { Commit is called.                                            }
  TAccessMode = (amRead, amWrite, amReadWrite);
  { Corresponds to fmShareExclusive etc. Only applies to root    }
  { storages. Ordinary storages have to be opened for exclusive  }
  { use. (The small print!)                                      }
  TShareMode = (smExclusive, smDenyWrite, smDenyRead, smDenyNone);
  { Root storages and storages can be opened in transacted mode  }
  { such that their changes remain temporary until Commit is     }
  { called. Note that streams cannot be opened in transaction    }
  { mode. Any changes to a stream are commited directly to the   }
  { parent storage. This storage though can be transacted.       }
  // NB tmDirect can only be combined with either (amRead and smDenyWrite)
  // or (amReadWrite and smExclusive) ... more small print.
  TTransactMode = (tmDirect, tmTransacted);

type
  ECompDocError = class(Exception);
  ECDStorageError = class(ECompDocError);
  ECDStreamError = class(ECompDocError);

  {$IFNDEF WIN32}
type
  PWideChar = PChar;
  TCLSID = CLSID;
  {$ENDIF}

type
  TStorageTimes = record
    Creation: TFileTime;
    LastAccess: TFileTime;
    LastModify: TFileTime;
  end;

type
  { encapsulates the compound document storage object }
  TStorage = class(TObject)
  private
    FName: string;
    FParent: TStorage;
    FThis: IStorage;
    hr: HResult;
  protected
    { checks hr and raises exception with msg (msg ignored in D1) }
    procedure CheckError(Msg: string);
    procedure CopyMoveElement(const srcname, dstname: string; Dst: TStorage; flag: Longint);
    function GetCLSID: TCLSID;
    function GetName: string;
    function GetTimes: TStorageTimes;
    procedure SetCLSID(Value: TCLSID);
  public
    { Creates (CreateNew = true) or opens (CreateNew = false) }
    { a storage within another storage. Fails if              }
    { ParentStorage is nil.                                   }
    { If creating a new storage, Name is null (''), a self-   }
    { deleting temporary storage is created.                  }
    { If a storage is in transacted mode any methods that     }
    { make changes to the storage only take effect when       }
    { Commit is called.                                       }
    { Note that all storages other than root storages can     }
    { only be opened for exclusive access.                    }
    constructor Create(Name: string; ParentStorage: TStorage; AccessMode: TAccessMode;
      TransactMode: TTransactMode; CreateNew: Boolean);
    { Closes the storage. If the storage is temporary it is }
    { also deleted. If in transacted mode any uncommitted   }
    { changes are lost.                                     }
    destructor Destroy; override;
    { If the storage was opened in transacted mode Commit  }
    { publishes changes at its own level to the next       }
    { higher level. If the storage is a root storage the   }
    { changes are committed to the underlying file system .}
    procedure Commit;
    { Copies an element of the storage (i.e., a substorage }
    { or stream) to another storage, optionally changing   }
    { the element name.                                    }
    procedure CopyElement(const srcname, dstname: string; Dst: TStorage);
    { Copies all the contents of the storage to another }
    { storage. If the destination storage is not empty  }
    { the new elements will be added to it, possibly    }
    { overwriting elements of the same name.            }
    procedure CopyTo(Dst: TStorage);
    { Removes a substorage or stream from the storage. }
    procedure DeleteElement(const Name: string);
    { Fills StreamList with the names of all the storage's ]
    { streams.                                             }
    procedure ListStreams(StreamList: TStrings);
    { Fills StorageList with the names of all the storage's ]
    { substorages.                                          }
    procedure ListStorages(StorageList: TStrings);
    { Like CopyElement followed by delete. }
    procedure MoveElement(const srcname, dstname: string; Dst: TStorage);
    { Renames one of the storage's substorages or streams. }
    procedure RenameElement(const OldName, NewName: string);
    { In transacted mode undoes any changes made since the }
    { Commit.                                              }
    procedure Revert;
    { The CLSID associated with this storage. }
    property ClassID: TCLSID read GetCLSID write SetCLSID;
    { The last error code. Read-only.}
    property LastError: HResult read hr;
    { The Name of the storage. If the storage was created as  }
    { temporary the actual name will be retrieved. Read-only. }
    property Name: string read GetName;
    { The storage which contains this storage. Read-only. }
    property ParentStorage: TStorage read FParent;
    { The date/times of the storage's creation, last access, }
    { and last modification. Read-only.                      }
    property Times: TStorageTimes read GetTimes;
  end;

  { A root storage corresponds to a compound file. It has all }
  { the behaviors of any other storage but can also be opened }
  { in the full range of share modes.                         }
  TRootStorage = class(TStorage)
  public
    constructor Create(Name: string; AccessMode: TAccessMode; ShareMode: TShareMode;
      TransactMode: TTransactMode; CreateNew: Boolean);
    { Creates a new storage from ordinary file Name. The }
    { file's contents are placed in a stream named       }
    { 'CONTENTS'.                                        }
    constructor Convert(Name: string; AccessMode: TAccessMode; ShareMode: TShareMode;
      TransactMode: TTransactMode);
  end;

  { A descendant of TStream with all its behaviors (CopyFrom, }
  { ReadBuffer, etc.). Note that storage streams cannot be    }
  { opened in transacted mode.                                }
  TStorageStream = class(TStream)
  private
    FName: string;
    FParent: TStorage;
    FThis: IStream;
    hr: HResult;
  protected
    procedure CheckError(Msg: string);
    function GetName: string;
    {$IFDEF VER3PLUS}
    procedure SetSize(NewSize: Longint); override;
    {$ELSE}
    procedure SetSize(NewSize: Longint);
    {$ENDIF}
  public
    { Creates (CreateNew = true) or opens (CreateNew = false) }
    { a stream within a storage. Fails if ParentStorage is    }
    { nil. If creating a new stream, Name is null (''), a     }
    { self-deleting temporary stream is created.              }
    { Note that streams can only be opened for exclusivey.    }
    constructor Create(Name: string; ParentStorage: TStorage; AccessMode: TAccessMode;
      CreateNew: Boolean); virtual;
    { Constructs a stream another stream such that both have }
    { live access to the same data but at different offsets. }
    { The initial offset matches that of the other stream.   }
    { Changes written to one stream are immediately visible  }
    { to the other.                                          }
    constructor CloneFrom(CDStream: TStorageStream);
    { Closes the stream writing any changes to the parent }
    { storage.                                            }
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property LastError: HResult read hr;
    { The Name of the stream. If the stream was created as    }
    { temporary the actual name will be retrieved. Read-only. }
    property Name: string read GetName;
    { The storage which contains this stream. Read-only. }
    property ParentStorage: TStorage read FParent;
  end;

  { helper procedures }

  { True if a file exists and is a compound document. }
function FileIsCompoundDoc(const FileName: string): Boolean;

{ Converts an existing file into a compound document with the }
{ file contents as a stream names 'CONTENTS'.                 }
{ Fails if FileName is already compound or in use.            }
procedure ConvertFileToCompoundDoc(const FileName: string);

{ Defragments a compound document and thus shrinks it. }
{ Fails if FileName is in use.                    }
procedure PackCompoundDoc(const FileName: string);

{ Sets the file date/times of a compound doc. If any of the }
{ time values are zero that filetime will not be set.       }
{ Fails if FileName is in use.                              }
procedure SetTimesOfCompoundDoc(const FileName: string; Times: TStorageTimes);

implementation

const
  S_OK = HResult(0);
  E_Fail = HResult($80004005);

  {$IFNDEF WIN32}

function Succeeded(hr: HResult): Boolean;
begin
  Result := SucceededHR(hr);
end;

function Failed(hr: HResult): Boolean;
begin
  Result := FailedHR(hr);
end;

function StringToPWideChar(S: string): PWideChar;
var
  Size: Integer;
begin
  Size := Length(S) + 1;
  Result := StrAlloc(Size);
  Result := StrPCopy(Result, S);
end;

function PWideCharToString(pw: PWideChar): string;
begin
  Result := StrPas(pw);
end;

procedure FreePWideChar(pw: PWideChar);
begin
  if Assigned(pw) then StrDispose(pw);
end;

{$ELSE}

function StringToPWideChar(S: string): PWideChar;
var
  OldSize: Integer;
  NewSize: Integer;
begin
  OldSize := Length(S) + 1;
  NewSize := OldSize * 2;
  Result := AllocMem(NewSize);
  MultiByteToWideChar(CP_ACP, 0, PChar(S), OldSize, Result, NewSize);
end;

function PWideCharToString(pw: PWideChar): string;
var
  P: PChar;
  iLen: Integer;
begin
  iLen := lstrlenw(pw) + 1;
  GetMem(P, iLen);
  WideCharToMultiByte(CP_ACP, 0, pw, iLen, P, iLen * 2, nil, nil);
  Result := P;
  FreeMem(P, iLen);
end;

procedure FreePWideChar(pw: PWideChar);
begin
  if Assigned(pw) then FreeMem(pw);
end;
{$ENDIF}

var
  ThisMalloc: IMalloc;

procedure CoFreeMem(P: Pointer);
begin
  ThisMalloc.Free(P);
end;

procedure GetElements(Storage: IStorage; List: TStrings; GetStorages: Boolean);
var
  Enum: IEnumSTATSTG;
  StatStg: TStatStg;
  NumFetched: Longint;
  hr: HResult;
begin
  hr := Storage.EnumElements(0, nil, 0, Enum);
  if hr <> S_OK then
    raise ECompDocError.Create('failed enumeration');
  repeat
    {$IFDEF WIN32}
    hr := Enum.Next(1, StatStg, @NumFetched);
    {$ELSE}
    hr := Enum.Next(1, StatStg, NumFetched);
    {$ENDIF}
    if (hr = S_OK) then
    begin
      if GetStorages then
      begin
        if StatStg.dwType = STGTY_STORAGE then
          List.Add(PWideCharToString(StatStg.pwcsName));
      end
      else
      begin
        if StatStg.dwType = STGTY_STREAM then
          List.Add(PWideCharToString(StatStg.pwcsName));
      end;
      CoFreeMem(StatStg.pwcsName);
    end;
  until (hr <> S_OK);
  {$IFNDEF VER3PLUS}
  Enum.Release;
  {$ENDIF}
end;

function GetMode(Accessmode: TAccessMode; ShareMode: TShareMode;
  TransactMode: TTransactMode; CreateNew: Boolean): Longint;
begin
  Result := ord(AccessMode) or (succ(Ord(ShareMode)) shl 4) or (Ord(TransactMode) shl 16);
  if CreateNew then
    Result := Result or STGM_CREATE;
end;

constructor TStorage.Create(Name: string; ParentStorage: TStorage; AccessMode: TAccessMode;
  TransactMode: TTransactMode; CreateNew: Boolean);
var
  Mode: Longint;
  PName: PWideChar;
begin
  Mode := GetMode(AccessMode, smExclusive, TransactMode, CreateNew);
  if ParentStorage = nil then
  begin
    hr := E_Fail;
    CheckError('no parent storage speciified');
  end;
  if CreateNew then
  begin
    if Name = '' then
    begin
      PName := nil;
      Mode := Mode or STGM_DELETEONRELEASE;
    end
    else
      PName := StringToPWideChar(Name);
    try
      hr := ParentStorage.FThis.CreateStorage(PName, Mode, 0, 0, FThis);
      CheckError('storage create failed');
    finally
      FreePWideChar(PName);
    end;
    FName := Name;
    FParent := ParentStorage;
  end
  else
  begin
    if Name = '' then
    begin
      PName := nil;
      hr := E_FAIL;
    end
    else
    begin
      PName := StringToPWideChar(Name);
      hr := S_OK;
    end;
    CheckError('no storage name given');
    try
      hr := ParentStorage.FThis.OpenStorage(PName, nil, Mode, nil, 0, FThis);
      CheckError('storage open failed');
    finally
      FreePWideChar(PName);
    end;
    FName := Name;
    FParent := ParentStorage;
  end;
end;

destructor TStorage.Destroy;
begin
  FName := '';
  FParent := nil;
  {$IFNDEF VER3PLUS}
  if Assigned(FThis) then FThis.Release;
  {$ENDIF}
  FThis := nil;
  inherited Destroy;
end;

procedure TStorage.Commit;
const
  STG_E_MEDIUMFULL = HResult($80004070);
begin
  hr := FThis.Commit(STGC_DEFAULT);
  if hr = STG_E_MEDIUMFULL then
    hr := FThis.Commit(STGC_OVERWRITE);
  CheckError('storage failed to commit');
end;

procedure TStorage.CopyMoveElement(const srcname, dstname: string; Dst: TStorage; flag: Longint);
var
  SrcPName: PWideChar;
  DstPName: PWideChar;
begin
  SrcPName := StringToPWideChar(srcname);
  try
    DStPName := StringToPWideChar(dstname);
    try
      hr := FThis.MoveElementTo(SrcPName, Dst.FThis, DstPName, flag);
      CheckError('storage failed to copy/move');
    finally
      FreePWideChar(DstPName);
    end;
  finally
    FreePWideChar(SrcPName)
  end;
end;

procedure TStorage.CopyElement(const srcname, dstname: string; Dst: TStorage);
begin
  CopyMoveElement(srcname, dstname, Dst, STGMOVE_COPY);
end;

procedure TStorage.CopyTo(Dst: TStorage);
begin
  {$IFDEF WIN32}
  hr := FThis.CopyTo(0, nil, nil, Dst.FThis);
  {$ELSE}
  {  hr := FThis.CopyTo(0, GUID_NULL, nil, Dst.FThis);}
  {$ENDIF}
  CheckError('failed copyto operation');
end;

procedure TStorage.CheckError(Msg: string);
begin
  {$IFDEF WIN32}
  if (hr <> S_OK) then
  begin
    Msg := Msg + ': ' + SysErrorMessage(hr);
    raise ECDStorageError.Create(Msg);
  end;
  {$ELSE}
  if (hr <> S_OK) then
  begin
    Msg := Msg + ': Error Code $' + IntToHex(GetSCode(hr) xor $80030000, 1);
    raise ECDStorageError.Create(Msg);
  end;
  {$ENDIF}
end;

function TStorage.GetCLSID: TCLSID;
var
  StatStg: TStatStg;
begin
  FThis.Stat(StatStg, STATFLAG_NONAME);
  CheckError('fail to get CLSID');
  Result := StatStg.CLSID;
end;

{ fixed a bug 3/27/99 thanks to Petio Tonev }

function TStorage.GetName: string;
var
  StatStg: TStatStg;
begin
  if FName <> '' then
    Result := FName
  else
  begin
    hr := FThis.Stat(StatStg, STATFLAG_DEFAULT);
    CheckError('storage stat failed');
    try
      Result := PWideCharToString(StatStg.pwcsName);
    finally
      CoFreeMem(StatStg.pwcsName);
    end;
  end;
end;

function TStorage.GetTimes: TStorageTimes;
var
  StatStg: TStatStg;
begin
  FThis.Stat(StatStg, STATFLAG_NONAME);
  CheckError('fail to get CLSID');
  with Result do
  begin
    Creation := StatStg.ctime;
    LastAccess := StatStg.atime;
    LastModify := StatStg.mtime;
  end;
end;

procedure TStorage.DeleteElement(const Name: string);
var
  PName: PWideChar;
begin
  PName := StringToPWideChar(Name);
  try
    hr := FThis.DestroyElement(PName);
    CheckError('failed to delete element');
  finally
    FreePWideChar(PName);
  end;
end;

procedure TStorage.ListStreams(StreamList: TStrings);
begin
  GetElements(FThis, StreamList, False);
end;

procedure TStorage.ListStorages(StorageList: TStrings);
begin
  GetElements(FThis, StorageList, True);
end;

procedure TStorage.MoveElement(const srcname, dstname: string; Dst: TStorage);
begin
  CopyMoveElement(srcname, dstname, Dst, STGMOVE_MOVE);
end;

procedure TStorage.RenameElement(const OldName, NewName: string);
var
  OldPName: PWideChar;
  NewPName: PWideChar;
begin
  OldPName := StringToPWideChar(OldName);
  try
    NewPName := StringToPWideChar(NewName);
    try
      hr := FThis.RenameElement(OldPName, NewPName);
      CheckError('failed to rename element');
    finally
      FreePWideChar(NewPName);
    end;
  finally
    FreePWideChar(OldPName);
  end;
end;

procedure TStorage.Revert;
begin
  hr := FThis.Revert;
  CheckError('storage failed to revert');
end;

procedure TStorage.SetCLSID(Value: TCLSID);
begin
  hr := FThis.SetClass(Value);
  CheckError('failed to set CLSID');
end;

constructor TRootStorage.Create(Name: string; AccessMode: TAccessMode; ShareMode: TShareMode;
  TransactMode: TTransactMode; CreateNew: Boolean);
var
  Mode: Longint;
  PName: PWideChar;
begin
  Mode := GetMode(AccessMode, ShareMode, TransactMode, CreateNew);
  if CreateNew then
  begin
    if Name = '' then
    begin
      PName := nil;
      Mode := Mode or STGM_DELETEONRELEASE;
    end
    else
    begin
      PName := StringToPWideChar(Name);
    end;
    try
      hr := StgCreateDocFile(PName, Mode, 0, FThis);
      CheckError('root storage create failed');
    finally
      FreePWideChar(PName);
    end;
    FName := Name;
    FParent := nil;
  end
  else
  begin
    if Name = '' then
    begin
      PName := nil;
      hr := E_FAIL;
    end
    else
    begin
      PName := StringToPWideChar(Name);
      hr := S_OK;
    end;
    CheckError('no storage name given');
    try
      hr := StgIsStorageFile(PName);
      CheckError('not a storage file');
      hr := StgOpenStorage(PName, nil, Mode, nil, 0, FThis);
      CheckError('root storage open failed');
    finally
      FreePWideChar(PName);
    end;
    FName := Name;
    FParent := nil;
  end;
end;

constructor TRootStorage.Convert(Name: string; AccessMode: TAccessMode; ShareMode: TShareMode;
  TransactMode: TTransactMode);
var
  Mode: Longint;
  PName: PWideChar;
begin
  Mode := GetMode(AccessMode, ShareMode, TransactMode, False);
  if Name = '' then
  begin
    PName := nil;
    hr := E_FAIL;
  end
  else
  begin
    PName := StringToPWideChar(Name);
    hr := S_OK;
  end;
  CheckError('no storage name given');
  try
    hr := StgIsStorageFile(PName);
    if hr = S_OK then CheckError('already a storage file');
    hr := StgCreateDocFile(PName, (Mode or STGM_CONVERT), 0, FThis);
    if Failed(hr) then CheckError('root storage convert failed');
  finally
    FreePWideChar(PName);
  end;
  FName := Name;
  FParent := nil;
end;

constructor TStorageStream.Create(Name: string; ParentStorage: TStorage; AccessMode: TAccessMode;
  CreateNew: Boolean);
var
  Mode: Longint;
  PName: PWideChar;
begin
  Mode := GetMode(AccessMode, smExclusive, tmDirect, CreateNew);
  if CreateNew then
  begin
    if Name = '' then
    begin
      PName := nil;
      Mode := Mode or STGM_DELETEONRELEASE;
    end
    else
      PName := StringToPWideChar(Name);
    try
      hr := ParentStorage.FThis.CreateStream(PName, Mode, 0, 0, FThis);
      CheckError('stream create failed');
    finally
      FreePWideChar(PName);
    end;
    FName := Name;
    FParent := ParentStorage;
  end
  else
  begin
    if Name = '' then
    begin
      PName := nil;
      hr := E_FAIL;
    end
    else
    begin
      PName := StringToPWideChar(Name);
      hr := S_OK;
    end;
    CheckError('no stream name given');
    try
      hr := ParentStorage.FThis.OpenStream(PName, nil, Mode, 0, FThis);
      CheckError('stream open failed');
    finally
      FreePWideChar(PName);
    end;
    FName := Name;
    FParent := ParentStorage;
  end;
end;

constructor TStorageStream.CloneFrom(CDStream: TStorageStream);
begin
  hr := CDStream.FThis.Clone(FThis);
  CheckError('stream clone failed');
  FName := CDStream.FName;
  FParent := CDSTream.FParent;
end;

destructor TStorageStream.Destroy;
begin
  FName := '';
  FParent := nil;
  {$IFNDEF VER3PLUS}
  FThis.Release;
  {$ENDIF}
  FThis := nil;
  inherited Destroy;
end;

procedure TStorageStream.CheckError(Msg: string);
begin
  if (hr <> S_OK) then
  begin
    {$IFDEF WIN32}
    Msg := Msg + ': ' + SysErrorMessage(hr);
    {$ENDIF}
    raise ECDStreamError.Create(Msg);
  end;
end;

{ fixed a bug 3/27/99 thanks to Petio Tonev }

function TStorageStream.GetName: string;
var
  StatStg: TStatStg;
begin
  if FName <> '' then
    Result := FName
  else
  begin
    hr := FThis.Stat(StatStg, STATFLAG_DEFAULT);
    CheckError('stream stat failed');
    try
      Result := PWideCharToString(StatStg.pwcsName);
    finally
      CoFreeMem(StatStg.pwcsName);
    end;
  end;
end;

function TStorageStream.Read(var Buffer; Count: Longint): Longint;
var
  cn: Longint;
begin
  cn := 0;
  {$IFDEF WIN32}
  hr := FThis.Read(@Buffer, Count, @cn);
  {$ELSE}
  hr := FThis.Read(@Buffer, Count, cn);
  {$ENDIF}
  if not Failed(hr) then
    Result := cn
  else
    Result := 0;
end;

{$IFDEF WIN32}

function TStorageStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  ps: LargeInt;
begin
  hr := FThis.Seek(Offset, Origin, ps);
  if not Failed(hr) then
    {$IFDEF VER4PLUS}
    Result := ps
    {$ELSE}
    Result := trunc(ps)
    {$ENDIF}
  else
    Result := -1;
end;
{$ELSE}

function TStorageStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  ps: Longint;
  ps2: Longint;
begin
  hr := FThis.Seek(Offset, 0, Origin, ps, ps2);
  if not Failed(hr) then
    Result := ps
  else
    Result := -1;
end;
{$ENDIF}

procedure TStorageStream.SetSize(NewSize: Longint);
begin
  {$IFDEF WIN32}
  hr := FThis.SetSize(NewSize);
  {$ELSE}
  hr := FThis.SetSize(NewSize, 0);
  {$ENDIF}
end;

function TStorageStream.Write(const Buffer; Count: Longint): Longint;
var
  cn: Longint;
begin
  cn := 0;
  {$IFDEF WIN32}
  hr := FThis.Write(@Buffer, Count, @cn);
  {$ELSE}
  hr := FThis.Write(@Buffer, Count, cn);
  {$ENDIF}
  if not Failed(hr) then
    Result := cn
  else
    Result := 0;
end;

function FileIsCompoundDoc(const FileName: string): Boolean;
var
  hr: HResult;
  PName: PWideChar;
begin
  PName := StringToPWideChar(FileName);
  try
    hr := StgIsStorageFile(PName);
    Result := (hr = S_OK);
  finally
    FreePWideChar(PName);
  end;
end;

procedure ConvertFileToCompoundDoc(const FileName: string);
var
  old: TRootStorage;
begin
  if FileIsCompoundDoc(FileName) then
    raise ECompDocError.Create('already compound')
  else
  begin
    old := TRootStorage.Convert(FileName, amReadWrite, smExclusive, tmDirect);
    old.Free;
  end;
end;

procedure PackCompoundDoc(const FileName: string);
var
  ThisCLSID: TCLSID;
  Storage, StorageTmp: TRootStorage;
begin
  Storage := TRootStorage.Create(FileName, amReadWrite, smExclusive, tmDirect, False);
  ThisCLSID := Storage.ClassID;
  StorageTmp := TRootStorage.Create('', amReadWrite, smExclusive, tmDirect, True);
  Storage.CopyTo(StorageTmp);
  Storage.Free;
  Storage := TRootStorage.Create(FileName, amReadWrite, smExclusive, tmDirect, True);
  Storage.ClassID := ThisCLSID;
  StorageTmp.CopyTo(Storage);
  Storage.Free;
  StorageTmp.Free;
end;

procedure SetTimesOfCompoundDoc(const FileName: string; Times: TStorageTimes);
var
  PName: PWideChar;
  hr: HResult;
begin
  PName := StringToPWideChar(FileName);
  try
    hr := StgSetTimes(PName, Times.Creation, Times.LastAccess, Times.LastModify);
    if hr <> S_OK then
      raise ECompDocError.Create('set times failed');
  finally
    FreePWideChar(PName);
  end;
end;

var
  OldExitProc: Pointer;

procedure Finalize; far;
begin
  {$IFNDEF VER3PLUS}
  if Assigned(ThisMalloc) then ThisMalloc.Release;
  {$ENDIF}
  ExitProc := OldExitProc;
end;

initialization

  CoGetMalloc(1, ThisMalloc);
  OldExitProc := ExitProc;
  ExitProc := @Finalize;

end.

