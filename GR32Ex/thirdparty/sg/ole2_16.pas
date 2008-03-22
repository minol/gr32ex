
{*******************************************************}
{                                                       }
{       Delphi Runtime Library                          }
{       Windows 3.1 API Interface Unit                  }
{                                                       }
{       Copyright (c) 1992,93 Borland International     }
{                                                       }
{*******************************************************}

Unit Ole2_16;

interface

uses WinTypes;

const

  { verbs }
  OLEIVERB_PRIMARY = 0;
  OLEIVERB_SHOW = -1;
  OLEIVERB_OPEN = -2;
  OLEIVERB_HIDE = -3;
  OLEIVERB_UIACTIVATE = -4;
  OLEIVERB_INPLACEACTIVATE = -5;
  OLEIVERB_DISCARDUNDOSTATE = -6;

  NULL = 0;
  NOERROR = 0;

  { Severity values}
  SEVERITY_SUCCESS = 0;
  SEVERITY_ERROR = 1;

{ -------------------------- Facility definitions -------------------------}

  { generally useful errors ([SE]_*)}
  FACILITY_NULL = 0;

  { remote procedure call errors (RPC_E_*)}
  FACILITY_RPC = 1;

  { late binding dispatch errors}
  FACILITY_DISPATCH = 2;

  { storage errors (STG_E_*)}
  FACILITY_STORAGE = 3;

  { interface-specific errors}
  FACILITY_ITF = 4;

  S_OK = 0;
  S_FALSE =  (SEVERITY_SUCCESS shl 31) or (FACILITY_NULL shl 16) or (1);

{ --------------------- FACILITY_NULL errors ------------------------------}

  { relatively catastrophic failure}
  E_UNEXPECTED =         (SEVERITY_ERROR shl 31) or (FACILITY_NULL shl 16) or ($ffff);

  { not implemented}
  E_NOTIMPL =            (SEVERITY_ERROR shl 31) or (FACILITY_NULL shl 16) or (1);

  { ran out of memory}
  E_OUTOFMEMORY =        (SEVERITY_ERROR shl 31) or (FACILITY_NULL shl 16) or (2);

  { one or more arguments are invalid}
  E_INVALIDARG =         (SEVERITY_ERROR shl 31) or (FACILITY_NULL shl 16) or (3);

  { no such interface supported}
  E_NOINTERFACE =        (SEVERITY_ERROR shl 31) or (FACILITY_NULL shl 16) or (4);

  { invalid pointer}
  E_POINTER =            (SEVERITY_ERROR shl 31) or (FACILITY_NULL shl 16) or (5);

  { invalid handle}
  E_HANDLE =             (SEVERITY_ERROR shl 31) or (FACILITY_NULL shl 16) or (6);

  { operation aborted}
  E_ABORT =              (SEVERITY_ERROR shl 31) or (FACILITY_NULL shl 16) or (7);

  { unspecified error}
  E_FAIL =               (SEVERITY_ERROR shl 31) or (FACILITY_NULL shl 16) or (8);

  { general access denied error}
  E_ACCESSDENIED =       (SEVERITY_ERROR shl 31) or (FACILITY_NULL shl 16) or (9);


{ ----------------- FACILITY_ITF errors used by OLE ---------------------}

{ By convention, OLE interfaces divide the FACILITY_ITF range of errors
  into nonoverlapping subranges.  If an OLE interface returns a FACILITY_ITF
  scode, it must be from the range associated with that interface or from
  the shared range: OLE_E_FIRST...OLE_E_LAST.

  The ranges, their associated interfaces, and the header file that defines
  the actual scodes are given below. }

  { Generic OLE errors that may be returned by many interfaces}
  OLE_E_FIRST =  (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or ($0000);
  OLE_E_LAST =   (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or ($00FF);
  OLE_S_FIRST =  (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($0000);
  OLE_S_LAST =   (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($00FF);

  { interfaces: all}
  DRAGDROP_E_FIRST =     (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or ($0100);
  DRAGDROP_E_LAST =      (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or ($010F);
  DRAGDROP_S_FIRST =     (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($0100);
  DRAGDROP_S_LAST =      (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($010F);

  { interfaces: IDropSource, IDropTarget}
  CLASSFACTORY_E_FIRST =  (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or ($0110);
  CLASSFACTORY_E_LAST =   (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or ($011F);
  CLASSFACTORY_S_FIRST =  (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($0110);
  CLASSFACTORY_S_LAST =   (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($011F);

  { interfaces: IClassFactory}
  MARSHAL_E_FIRST =  (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or ($0120);
  MARSHAL_E_LAST =   (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or ($012F);
  MARSHAL_S_FIRST =  (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($0120);
  MARSHAL_S_LAST =   (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($012F);

  { interfaces: IMarshal, IStdMarshalInfo, marshal APIs}
  DATA_E_FIRST =     (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or ($0130);
  DATA_E_LAST =      (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or ($013F);
  DATA_S_FIRST =     (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($0130);
  DATA_S_LAST =      (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($013F);

  { interfaces: IDataObject}
  VIEW_E_FIRST =     (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($0140);
  VIEW_E_LAST =      (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($014F);
  VIEW_S_FIRST =     (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($0140);
  VIEW_S_LAST =      (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($014F);

  { interfaces: IViewObject}
  REGDB_E_FIRST =    (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($0150);
  REGDB_E_LAST =     (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($015F);
  REGDB_S_FIRST =    (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($0150);
  REGDB_S_LAST =     (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($015F);

  { API: reg.dat manipulation}
  { range 160 - 16F reserved}
  CACHE_E_FIRST =    (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($0170); 
  CACHE_E_LAST =     (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($017F);
  CACHE_S_FIRST =    (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($0170);
  CACHE_S_LAST =     (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($017F);

  { interfaces: IOleCache}
  OLEOBJ_E_FIRST =   (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($0180);
  OLEOBJ_E_LAST =    (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($018F);
  OLEOBJ_S_FIRST =   (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($0180);
  OLEOBJ_S_LAST =    (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($018F);

  { interfaces: IOleObject}
  CLIENTSITE_E_FIRST =   (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($0190); 
  CLIENTSITE_E_LAST =   (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($019F);
  CLIENTSITE_S_FIRST =  (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($0190);
  CLIENTSITE_S_LAST =    (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($019F);

  { interfaces: IOleClientSite}
  INPLACE_E_FIRST =  (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($01A0);
  INPLACE_E_LAST =   (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($01AF);
  INPLACE_S_FIRST =  (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($01A0);
  INPLACE_S_LAST =   (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($01AF);

  { interfaces: IOleWindow, IOleInPlaceObject, IOleInPlaceActiveObject,}
  {             IOleInPlaceUIWindow, IOleInPlaceFrame, IOleInPlaceSite}
  ENUM_E_FIRST =         (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($01B0);
  ENUM_E_LAST =      (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($01BF);
  ENUM_S_FIRST =     (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($01B0);
  ENUM_S_LAST =      (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($01BF);

  { interfaces: IEnum*}
  CONVERT10_E_FIRST =   (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($01C0);
  CONVERT10_E_LAST =    (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($01CF);
  CONVERT10_S_FIRST =   (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($01C0);
  CONVERT10_S_LAST =    (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($01CF);

  { API: OleConvertOLESTREAMToIStorage, OleConvertIStorageToOLESTREAM}
  CLIPBRD_E_FIRST =      (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($01D0);
  CLIPBRD_E_LAST =       (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($01DF);
  CLIPBRD_S_FIRST =      (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($01D0);
  CLIPBRD_S_LAST =       (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($01DF);

  { interfaces: OleSetClipboard, OleGetClipboard, OleFlushClipboard}
  MK_E_FIRST =       (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($01E0);
  MK_E_LAST =        (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($01EF);
  MK_S_FIRST =       (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($01E0);
  MK_S_LAST =        (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($01EF);

  { interfaces: IMoniker, IBindCtx, IRunningObjectTable, IParseDisplayName,}
  {             IOleContainer, IOleItemContainer, IOleLink}
  CO_E_FIRST =       (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($01F0);
  CO_E_LAST =        (SEVERITY_ERROR shl 31) or (  FACILITY_ITF shl 16) or ($01FF);
  CO_S_FIRST =       (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($01F0);
  CO_S_LAST =        (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or ($01FF);


{ *********************** Compobj errors **********************************}

  { CoInitialize has not been called and must be}
  CO_E_NOTINITIALIZED =          (CO_E_FIRST + $0);

  { CoInitialize has already been called and cannot be called again (temporary)}
  CO_E_ALREADYINITIALIZED =      (CO_E_FIRST + $1);

  { can't determine clsid (e.g., extension not in reg.dat)}
  CO_E_CANTDETERMINECLASS =      (CO_E_FIRST + $2);

  { the string form of the clsid is invalid (including ole1 classes)}
  CO_E_CLASSSTRING =             (CO_E_FIRST + $3);

  { the string form of the iid is invalid}
  CO_E_IIDSTRING =               (CO_E_FIRST + $4);

  { application not found}
  CO_E_APPNOTFOUND =             (CO_E_FIRST + $5);

  { application cannot be run more than once}
  CO_E_APPSINGLEUSE =            (CO_E_FIRST + $6);

  { some error in the app program file}
  CO_E_ERRORINAPP =              (CO_E_FIRST + $7);

  { dll not found}
  CO_E_DLLNOTFOUND =             (CO_E_FIRST + $8);

  { some error in the dll file}
  CO_E_ERRORINDLL =              (CO_E_FIRST + $9);

  { app written for other version of OS or other OS altogether}
  CO_E_WRONGOSFORAPP =           (CO_E_FIRST + $a);

  { object is not registered}
  CO_E_OBJNOTREG =               (CO_E_FIRST + $b);

  { object is already registered}
  CO_E_OBJISREG =                (CO_E_FIRST + $c);

  { handler is not connected to server}
  CO_E_OBJNOTCONNECTED =         (CO_E_FIRST + $d);

  { app was launched, but didn't registered a class factory}
  CO_E_APPDIDNTREG =             (CO_E_FIRST + $e);


{ ********************* ClassObject errors ********************************}

  { class does not support aggregation (or class object is remote)}
  CLASS_E_NOAGGREGATION =        (CLASSFACTORY_E_FIRST + $0);

  { dll doesn't support that class (returned from DllGetClassObject)}
  CLASS_E_CLASSNOTAVAILABLE =    (CLASSFACTORY_E_FIRST + $1);


{ *********************** Reg.dat errors **********************************}

  { some error reading the registration database }
  REGDB_E_READREGDB =            (REGDB_E_FIRST + $0);

  { some error reading the registration database }
  REGDB_E_WRITEREGDB =           (REGDB_E_FIRST + $1);

  { some error reading the registration database }
  REGDB_E_KEYMISSING =           (REGDB_E_FIRST + $2);

  { some error reading the registration database }
  REGDB_E_INVALIDVALUE =         (REGDB_E_FIRST + $3);

  { some error reading the registration database }
  REGDB_E_CLASSNOTREG =          (REGDB_E_FIRST + $4);

  { some error reading the registration database }
  REGDB_E_IIDNOTREG =            (REGDB_E_FIRST + $5);


{ Other value types }

  { these are mostly for internal use...}
  MEMCTX_SAME = $FFFFFFFE;       { same context (as some other pointer)}
  MEMCTX_UNKNOWN = $FFFFFFFF;    { unknown context (when asked about it)}
  MEMCTX_SHARED = 2;             { shared memory (between processes)}
  MEMCTX_TASK = 1;               { task (private) memory}

  { class context: used to determine what scope and kind of class object to use}
  CLSCTX_LOCAL_SERVER = 4;       { server exe (runs on same machine; diff proc)}
  CLSCTX_INPROC_HANDLER = 2;     { handler dll (runs in same process as caller)}
  CLSCTX_INPROC_SERVER = 1;      { server dll (runs in same process as caller)}

  { minimum number of bytes for interface marshl}
  MARSHALINTERFACE_MIN = 40;

  { verbs }
  DVASPECT_DOCPRINT = 8;
  DVASPECT_ICON = 4;
  DVASPECT_THUMBNAIL = 2;
  DVASPECT_CONTENT = 1;

  { TYpes of storage MEDiums; determines how data is stored or passed around}
  TYMED_NULL = 0;
  TYMED_MFPICT = $20;
  TYMED_GDI = $10;
  TYMED_ISTORAGE = 8;
  TYMED_ISTREAM = 4;
  TYMED_FILE = 2;
  TYMED_HGLOBAL = 1;

  { Advise Flags}
  ADVFCACHE_ONSAVE = $20;
  ADVFCACHE_FORCEBUILTIN = $10;
  ADVFCACHE_NOHANDLER = 8;
  ADVF_DATAONSTOP = $40;
  ADVF_ONLYONCE = 4;
  ADVF_PRIMEFIRST = 2;
  ADVF_NODATA = 1;

  { for OleCreateEmbeddingHelper flags; roles in low word; options in high word}

  { role is handler; implementation is }
  EMBDHLP_INPROC_HANDLER = 0;
  { default handler; pCF can be NULL}

  { role is server; pCF can't be NULL}
  EMBDHLP_INPROC_SERVER = 1;

  { create using pCF immediately; if pCF}
  EMBDHLP_CREATENOW = 0;
  { is NULL, uses std remoting handler}

  { delayed create; must supply pCF}
  EMBDHLP_DELAYCREATE = $10000;

  { Cache update Flags}
  UPDFCACHE_NODATACACHE = 1;
  UPDFCACHE_ONSAVECACHE = 2;
  UPDFCACHE_ONSTOPCACHE = 4;
  UPDFCACHE_NORMALCACHE = 8;
  UPDFCACHE_IFBLANK = $10;
  UPDFCACHE_ONLYIFBLANK = $80000000;

  { Storage types }
  CWCSTORAGENAME = $20;

  { Storage instantiation modes }
  STGM_DIRECT = 0;
  STGM_TRANSACTED = $10000;
  STGM_READ = 0;
  STGM_WRITE = 1;
  STGM_READWRITE = 2;
  STGM_SHARE_DENY_NONE = $40;
  STGM_SHARE_DENY_READ = 48;
  STGM_SHARE_DENY_WRITE = $20;
  STGM_SHARE_EXCLUSIVE = $10;
  STGM_PRIORITY = $40000;
  STGM_DELETEONRELEASE = $4000000;
  STGM_CREATE = $1000;
  STGM_CONVERT = $20000;
  STGM_FAILIFTHERE = 0;

  STGC_DEFAULT	                          = 0;
  STGC_OVERWRITE	                  = 1;
  STGC_ONLYIFCURRENT	                  = 2;
  STGC_DANGEROUSLYCOMMITMERELYTODISKCACHE = 4;

  STGMOVE_MOVE = 0;
  STGMOVE_COPY = 1;

  STATFLAG_DEFAULT = 0;
  STATFLAG_NONAME  = 1;

  STGTY_STORAGE	  = 1;
  STGTY_STREAM	  = 2;
  STGTY_LOCKBYTES = 3;
  STGTY_PROPERTY  = 4;


  { Storage Error Codes }
  STG_E_INVALIDFUNCTION = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or (1);
  STG_E_FILENOTFOUND = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or (2);
  STG_E_PATHNOTFOUND = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or (3);
  STG_E_TOOMANYOPENFILES = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or (4);
  STG_E_ACCESSDENIED = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or (5);
  STG_E_INVALIDHANDLE = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or (6);
  STG_E_INSUFFICIENTMEMORY = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or (8);
  STG_E_INVALIDPOINTER = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or (9);
  STG_E_NOMOREFILES = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($12);
  STG_E_DISKISWRITEPROTECTED = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($13);
  STG_E_SEEKERROR = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($19);
  STG_E_WRITEFAULT = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($1d);
  STG_E_READFAULT = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($1e);
  STG_E_SHAREVIOLATION = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($20);
  STG_E_LOCKVIOLATION = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($21);
  STG_E_FILEALREADYEXISTS = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($50);
  STG_E_INVALIDPARAMETER = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($57);
  STG_E_MEDIUMFULL = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($70);
  STG_E_ABNORMALAPIEXIT = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($fa);
  STG_E_INVALIDHEADER = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($fb);
  STG_E_INVALIDNAME = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($fc);
  STG_E_UNKNOWN = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($fd);
  STG_E_UNIMPLEMENTEDFUNCTION = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($fe);
  STG_E_INVALIDFLAG = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($ff);

  { Standard storage error codes }
  STG_E_INUSE = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($100);
  STG_E_NOTCURRENT = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($101);
  STG_E_REVERTED = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($102);
  STG_E_CANTSAVE = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($103);
  STG_E_OLDFORMAT = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($104);
  STG_E_OLDDLL = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($105);
  STG_E_SHAREREQUIRED = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($106);
  STG_E_NOTFILEBASEDSTORAGE = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($107);
  STG_E_EXTANTMARSHALLINGS = (SEVERITY_ERROR shl 31) or (FACILITY_STORAGE shl 16) or ($108);
  STG_S_CONVERTED = (SEVERITY_SUCCESS shl 31) or (FACILITY_STORAGE shl 16) or ($200);

  { IDrop??? Interfaces }
  MK_ALT = $20;

  DROPEFFECT_NONE = 0;
  DROPEFFECT_COPY = 1;
  DROPEFFECT_MOVE = 2;
  DROPEFFECT_LINK = 4;
  DROPEFFECT_SCROLL = $80000000;

  { default inset-width of the hot zone, in pixels}
  {   typical use: GetProfileInt("windows","DragScrollInset",DD_DEFSCROLLINSET)}
  DD_DEFSCROLLINSET = 11;

  { default delay before scrolling, in milliseconds}
  {   typical use: GetProfileInt("windows","DragScrollDelay",DD_DEFSCROLLDELAY)}
  DD_DEFSCROLLDELAY = 50;

  { default scroll interval, in milliseconds}
  {   typical use: GetProfileInt("windows","DragScrollInterval", DD_DEFSCROLLINTERVAL) }
  DD_DEFSCROLLINTERVAL = 50;

  { default delay before dragging should start, in milliseconds}
  {   typical use: GetProfileInt("windows", "DragDelay", DD_DEFDRAGDELAY)}
  DD_DEFDRAGDELAY = 200;

  { default minimum distance (radius) before dragging should start, in pixels}
  {   typical use: GetProfileInt("windows", "DragMinDist", DD_DEFDRAGMINDIST)}
  DD_DEFDRAGMINDIST = 2;

  { Dragdrop specific error codes }

  { trying to revoke a drop target that has not been registered }
  DRAGDROP_E_NOTREGISTERED = DRAGDROP_E_FIRST;

  { this window has already been registered as a drop target}
  DRAGDROP_E_ALREADYREGISTERED = DRAGDROP_E_FIRST + 1;

  { invalid HWND}
  DRAGDROP_E_INVALIDHWND = DRAGDROP_E_FIRST + 2;

  { successful drop took place }
  DRAGDROP_S_DROP = DRAGDROP_S_FIRST + 0;

  { drag-drop operation canceled}
  DRAGDROP_S_CANCEL = DRAGDROP_S_FIRST + 1;

  { use the default cursor}
  DRAGDROP_S_USEDEFAULTCURSORS = DRAGDROP_S_FIRST + 2;

  { bind flags; controls binding; stored in bind options above}
  BIND_JUSTTESTEXISTENCE = 2;
  BIND_MAYBOTHERUSER = 1;

  { system moniker types; returned from IsSystemMoniker.}
  MKSYS_POINTERMONIKER = 5;
  MKSYS_ITEMMONIKER = 4;
  MKSYS_ANTIMONIKER = 3;
  MKSYS_FILEMONIKER = 2;
  MKSYS_GENERICCOMPOSITE = 1;
  MKSYS_NONE = 0;

  { bit wise enum to control how much reduction takes place.}
  MKRREDUCE_ALL = 0;
  MKRREDUCE_THROUGHUSER = $10000;
  MKRREDUCE_TOUSER = $20000;
  MKRREDUCE_ONE = $30000;

  { IRunningObjectTable::Register flags }
  ROTFLAGS_REGISTRATIONKEEPSALIVE = 1;

  OLECONTF_ONLYIFRUNNING = $10;
  OLECONTF_ONLYUSER = 8;
  OLECONTF_OTHERS = 4;
  OLECONTF_LINKS = 2;
  OLECONTF_EMBEDDINGS = 1;

  {misc definitions}
  INPLACE_DEFBORDERWIDTH = 4;

  { bits returned from IOleObject::GetMistStatus}
  { bitwise}
  OLEMISC_RENDERINGISDEVICEINDEPEN = $200;
  OLEMISC_ACTIVATEWHENVISIBLE = $100;
  OLEMISC_INSIDEOUT = $80;
  OLEMISC_ISLINKOBJECT = $40;
  OLEMISC_CANLINKBYOLE1 = $20;
  OLEMISC_CANTLINKINSIDE = $10;
  OLEMISC_STATIC = 8;
  OLEMISC_INSERTNOTREPLACE = 4;
  OLEMISC_ONLYICONIC = 2;
  OLEMISC_RECOMPOSEONRESIZE = 1;

{*************** FACILITY_ITF scodes common to all interfaces ************}

{ By convention, OLE interfaces divide the FACILITY_ITF range of errors
  into nonoverlapping subranges.  If an interface returns a FACILITY_ITF
  scode, it must be from the range associated with that interface or from
  the shared range: OLE_E_FIRST...OLE_E_LAST. }

{ error codes }

  { invalid OLEVERB structure  }
  OLE_E_OLEVERB = OLE_E_FIRST;

  { invalid advise flags}
  OLE_E_ADVF = OLE_E_FIRST + 1;

  { you can't enuemrate any more, because the associated data is missing}
  OLE_E_ENUM_NOMORE = OLE_E_FIRST + 2;

  { this implementation doesn't take advises}
  OLE_E_ADVISENOTSUPPORTED = OLE_E_FIRST + 3;

  { there is no connection for this connection id}
  OLE_E_NOCONNECTION = OLE_E_FIRST + 4;

  { need run the object to perform this operation}
  OLE_E_NOTRUNNING = OLE_E_FIRST + 5;

  { there is no cache to operate on}
  OLE_E_NOCACHE = OLE_E_FIRST + 6;

  { Uninitialized object}
  OLE_E_BLANK = OLE_E_FIRST + 7;

  { linked object's source class has changed }
  OLE_E_CLASSDIFF = OLE_E_FIRST + 8;

  { not able to get the moniker of the object}
  OLE_E_CANT_GETMONIKER = OLE_E_FIRST + 9;

  { not able to bind to the source}
  OLE_E_CANT_BINDTOSOURCE = OLE_E_FIRST + 10;

  { object is static, operation not allowed}
  OLE_E_STATIC = OLE_E_FIRST + 11;

  { user cancelled out of save dialog}
  OLE_E_PROMPTSAVECANCELLED = OLE_E_FIRST + 12;

  { invalid rectangle }
  OLE_E_INVALIDRECT = OLE_E_FIRST + 13;

  { compobj.dll is too old for the ole2.dll initialized}
  OLE_E_WRONGCOMPOBJ = OLE_E_FIRST + 14;

  { invalid window handle}
  OLE_E_INVALIDHWND = OLE_E_FIRST + 15;

  { object is not in any of the inplace active states  }
  OLE_E_NOT_INPLACEACTIVE = OLE_E_FIRST + 16;

  { not able to convert the object }
  OLE_E_CANTCONVERT = OLE_E_FIRST + 17;

  { not able to perform the operation because object is not given storage yet.}
  OLE_E_NOSTORAGE = OLE_E_FIRST + 18;

  DVGEN_E_FIRST = OLE_E_FIRST + 100;

  { invalid FORMATETC structure}
  DV_E_FORMATETC = DVGEN_E_FIRST;

  { invalid DVTARGETDEVICE structure}
  DV_E_DVTARGETDEVICE = DVGEN_E_FIRST + 1;

  { invalid STDGMEDIUM structure}
  DV_E_STGMEDIUM = DVGEN_E_FIRST + 2;

  { invalid STATDATA structure}
  DV_E_STATDATA = DVGEN_E_FIRST + 3;

  { invalid lindex}
  DV_E_LINDEX = DVGEN_E_FIRST + 4;

  { invalid tymed}
  DV_E_TYMED = DVGEN_E_FIRST + 5;

  { invalid clipboard format}
  DV_E_CLIPFORMAT = DVGEN_E_FIRST + 6;

  { invalid aspect(s) }
  DV_E_DVASPECT = DVGEN_E_FIRST + 7;

  { tdSize paramter of the DVTARGETDEVICE structure is invalid}
  DV_E_DVTARGETDEVICE_SIZE = DVGEN_E_FIRST + 8;

  { object doesn't support IViewObject interface}
  DV_E_NOIVIEWOBJECT = DVGEN_E_FIRST + 9;


{ Success codes}

  { use the reg database to provide the requested info}
  OLE_S_USEREG = OLE_S_FIRST;

  { success, but static }
  OLE_S_STATIC = OLE_S_FIRST + 1;

  { macintosh clipboard format}
  OLE_S_MAC_CLIPFORMAT = OLE_S_FIRST + 2;

{*************************** Interface or API specific scodes *************}

  { Errors for OleConvertOLESTREAMToIStorage and OleConvertIStorageToOLESTREAM}
  { OLESTREAM Get method failed}
  CONVERT10_E_OLESTREAM_GET = CONVERT10_E_FIRST + 0;

  { OLESTREAM Put method failed}
  CONVERT10_E_OLESTREAM_PUT = CONVERT10_E_FIRST + 1;

  { Contents of the OLESTREAM not in correct format}
  CONVERT10_E_OLESTREAM_FMT = CONVERT10_E_FIRST + 2;

  { There was in an error in a Windows GDI call while converting the bitmap
  to a DIB. }
  CONVERT10_E_OLESTREAM_BITMAP_TO_DIB = CONVERT10_E_FIRST + 3;

  { Contents of the IStorage not in correct format}
  CONVERT10_E_STG_FMT = CONVERT10_E_FIRST + 4;

  { Contents of IStorage is missing one of the standard streams ("\1CompObj",
  "\1Ole", "\2OlePres000").  This may be the storage for a DLL object, or a
  class that does not use the def handler. }
  CONVERT10_E_STG_NO_STD_STREAM = CONVERT10_E_FIRST + 5;

  { There was in an error in a Windows GDI call while converting the DIB
  to a bitmap. }
  CONVERT10_E_STG_DIB_TO_BITMAP = CONVERT10_E_FIRST + 6;

  { Returned by either API, this scode indicates that the original object
  had no presentation, therefore the converted object does not either. }
  CONVERT10_S_NO_PRESENTATION = CONVERT10_S_FIRST + 0;

  { OpenClipboard Failed}
  CLIPBRD_E_CANT_OPEN = CLIPBRD_E_FIRST + 0;

  { EmptyClipboard Failed}
  CLIPBRD_E_CANT_EMPTY = CLIPBRD_E_FIRST + 1;

  { SetClipboard Failed}
  CLIPBRD_E_CANT_SET = CLIPBRD_E_FIRST + 2;

  { Data on clipboard is invalid}
  CLIPBRD_E_BAD_DATA = CLIPBRD_E_FIRST + 3;

  { CloseClipboard Failed}
  CLIPBRD_E_CANT_CLOSE = CLIPBRD_E_FIRST + 4;


  { Advise Flags}
  DATA_E_FORMATETC = DV_E_FORMATETC;
  DATA_S_SAMEFORMATETC = DATA_S_FIRST + 0;
  VIEW_E_DRAW = VIEW_E_FIRST;
  E_DRAW = VIEW_E_DRAW;

  VIEW_S_ALREADY_FROZEN = VIEW_S_FIRST;
  CACHE_E_NOCACHE_UPDATED = CACHE_E_FIRST;
  CACHE_S_FORMATETC_NOTSUPPORTED = CACHE_S_FIRST;
  CACHE_S_SAMECACHE = CACHE_S_FIRST+1;
  CACHE_S_SOMECACHES_NOTUPDATED = CACHE_S_FIRST+2;

  { Cache update Flags}
  UPDFCACHE_IFBLANKORONSAVECACHE = (UPDFCACHE_IFBLANK) or (UPDFCACHE_ONSAVECACHE);
  UPDFCACHE_ALL = (not UPDFCACHE_ONLYIFBLANK);
  UPDFCACHE_ALLBUTNODATACACHE = (UPDFCACHE_ALL) and  (not UPDFCACHE_NODATACACHE);

  {Moniker consts}
  MK_E_CONNECTMANUALLY = MK_E_FIRST;
  MK_E_EXCEEDEDDEADLINE = MK_E_FIRST + 1;
  MK_E_NEEDGENERIC = MK_E_FIRST + 2;
  MK_E_UNAVAILABLE = MK_E_FIRST + 3;
  MK_E_SYNTAX = MK_E_FIRST + 4;
  MK_E_NOOBJECT = MK_E_FIRST + 5;
  MK_E_INVALIDEXTENSION = MK_E_FIRST + 6;
  MK_E_INTERMEDIATEINTERFACENOTSUPPORTED = MK_E_FIRST + 7;
  MK_E_NOTBINDABLE = MK_E_FIRST + 8;

  { called IBindCtx->RevokeObjectBound for an object which was not bound }
  MK_E_NOTBOUND = MK_E_FIRST + 9;

  MK_E_CANTOPENFILE = MK_E_FIRST + 10;
  MK_E_MUSTBOTHERUSER = MK_E_FIRST + 11;
  MK_E_NOINVERSE = MK_E_FIRST + 12;
  MK_E_NOSTORAGE = MK_E_FIRST + 13;
  MK_E_NOPREFIX = MK_E_FIRST + 14;
  { reserved                     MK_S_FIRST}
  { reserved                     (MK_S_FIRST + 1)}
  MK_S_REDUCED_TO_SELF = MK_S_FIRST + 2;
  MK_S_ME = MK_S_FIRST + 4;
  MK_S_HIM = MK_S_FIRST + 5;
  MK_S_US = MK_S_FIRST + 6;
  MK_S_MONIKERALREADYREGISTERED = MK_S_FIRST + 7;

  OLEOBJ_E_NOVERBS = OLEOBJ_E_FIRST + 0;
  OLEOBJ_E_INVALIDVERB = OLEOBJ_E_FIRST + 1;
  OLEOBJ_S_INVALIDVERB = OLEOBJ_S_FIRST + 0;

  { verb number is valid but verb cannot be done now, for instance
    hiding a link or hiding a visible OLE 1.0 server }
  OLEOBJ_S_CANNOT_DOVERB_NOW = OLEOBJ_S_FIRST + 1;

  OLEOBJ_S_INVALIDHWND = OLEOBJ_S_FIRST + 2; { invalid hwnd passed}

  { Inplace editing specific error codes }
  INPLACE_E_NOTUNDOABLE = INPLACE_E_FIRST; { undo is not avaiable}
  INPLACE_E_NOTOOLSPACE = INPLACE_E_FIRST+1; { Space for tools is not available}
  { Message is too long, some of it had to be truncated before displaying}
  INPLACE_S_TRUNCATED = INPLACE_S_FIRST;

{ *************************** RPC errors **********************************}

  RPC_E_FIRST = (SEVERITY_ERROR shl 31) or (FACILITY_RPC shl 16) or (0);

  { call was rejected by callee, either by MF::HandleIncomingCall or }
  RPC_E_CALL_REJECTED = RPC_E_FIRST + 1;

  { call was canceld by call - returned by MessagePending
  this code only occurs if MessagePending return cancel }
  RPC_E_CALL_CANCELED = RPC_E_FIRST + 2;

  { the caller is dispatching an intertask SendMessage call and
  can NOT call out via PostMessage }
  RPC_E_CANTPOST_INSENDCALL = RPC_E_FIRST + 3;

  { the caller is dispatching an asynchronus call can NOT
  make an outgoing call on behalf of this call }
  RPC_E_CANTCALLOUT_INASYNCCALL = RPC_E_FIRST + 4;

  { the caller is not in a state where an outgoing call can be made this
  is the case if the caller has an outstandig call and another incoming
  call was excepted by HIC; now the caller is not allowed to call out
  again }
  RPC_E_CANTCALLOUT_INEXTERNALCALL = RPC_E_FIRST + 5;

  { the connection terminated or is in a bogus state and can not be used
  any more. Other connections are still valid. }
  RPC_E_CONNECTION_TERMINATED = RPC_E_FIRST + 6;

  { the callee (server [not server application]) is not available
  and disappeared; all connections are invalid }
  RPC_E_SERVER_DIED = RPC_E_FIRST + 7;

  { the caller (client ) disappeared while the callee (server) was
  processing a call }
  RPC_E_CLIENT_DIED = RPC_E_FIRST + 8;

  { the date paket with the marshalled parameter data is incorrect }
  RPC_E_INVALID_DATAPACKET = RPC_E_FIRST + 9;

  { the call was not transmitted properly; the message queue
  was full and was not emptied after yielding }
  RPC_E_CANTTRANSMIT_CALL = RPC_E_FIRST + $0A;

  { the client (caller) can not marshall the parameter data
  or unmarshall the return data - low memory etc. }
  RPC_E_CLIENT_CANTMARSHAL_DATA = RPC_E_FIRST + $0B;
  RPC_E_CLIENT_CANTUNMARSHAL_DATA = RPC_E_FIRST + $0C;

  { the server (caller) can not unmarshall the parameter data
  or marshall the return data - low memory }
  RPC_E_SERVER_CANTMARSHAL_DATA = RPC_E_FIRST + $0D;
  RPC_E_SERVER_CANTUNMARSHAL_DATA = RPC_E_FIRST + $0E;

  { received data are invalid; can be server or client data }
  RPC_E_INVALID_DATA = RPC_E_FIRST + $0F;

  { a particular parameter is invalid and can not be un/marshalled }
  RPC_E_INVALID_PARAMETER = RPC_E_FIRST + $10;

  { DDE conversation - no second outgoing call on same channel }
  RPC_E_CANTCALLOUT_AGAIN = RPC_E_FIRST + $11;

  { a internal error occured }
  RPC_E_UNEXPECTED = RPC_E_FIRST + $FFFF;

  CLSCTX_ALL = (CLSCTX_INPROC_SERVER) or (CLSCTX_INPROC_HANDLER) or (CLSCTX_LOCAL_SERVER);
  CLSCTX_INPROC = (CLSCTX_INPROC_SERVER) or (CLSCTX_INPROC_HANDLER);
  CLSCTX_SERVER = (CLSCTX_INPROC_SERVER) or (CLSCTX_LOCAL_SERVER);

type
  HIcon = THandle;
  HGlobal = THandle;
  HMenu = THandle;
  HAccel = THandle;
  HTask = THandle;
  HResult = Pointer;
  SCode = Longint;

  PStr = PChar;
  LPCSTR = PChar;
  LPLOGPALETTE = Pointer;
  LPMSG = Pointer;
 
  TRectLong = record
    Left, Top, Right, Bottom: Longint;
  end;

  TPointLong = record
    x, y: Longint; 
  end;

  PSize = ^Size;
  Size = record
  	cx, cy: Integer; 
  end;

  PSizeLong = ^SizeLong;
  SizeLong = record
    cx: Longint; 
    cy: Longint; 
  end;

{#IFNDEF WIN32}
  ULARGE_INTEGER = record
    LowPart  :  LongInt;
    HighPart :  LongInt;
  end;

  LARGE_INTEGER = record
    LowPart  :  LongInt;
    HighPart :  LongInt;
  end;
{#ENDIF}

(* 
  Status values are 32 bit values layed out as follows:
  3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
  1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
 +-+---------------------+-------+-------------------------------+
 |S|       Context       | Facil |               Code            |
 +-+---------------------+-------+-------------------------------+
 where
     S - is the severity code
         0 - Success
         1 - Error

     Context - context info
     Facility - is the facility code
     Code - is the facility's status code
*)

function SUCCEEDED(Status: SCODE): Boolean;
function FAILED(Status: SCODE): Boolean;
function SUCCEEDEDHR(hr: HRESULT): Boolean;
function FAILEDHR(hr: HRESULT): Boolean;
             
function SCODE_CODE(sc: SCode): Integer;
function SCODE_FACILITY(sc: SCode): Integer;
function SCODE_SEVERITY(sc: SCode): Integer;

type
  { Globally Unique Ids }
  GUID = record
  	Data1: Longint; 
  	Data2: Word; 
  	Data3: Word; 
  	Data4: Array[1..8] of Byte; 
  end;

  LPGUID = ^GUID;

  { Each dll/exe must initialize the GUIDs once.  This is done in one of
  two ways.  If you are not using precompiled headers for the file(s) which
  initializes the GUIDs, define INITGUID before including compobj.h.  This
  is how OLE builds the initialized versions of the GUIDs which are included
  in compobj.dll. 

  The alternative (which some versions of the compiler don't handle properly;
  they wind up with the initialized GUIDs in a data, not a text segment),
  is to use a precompiled version of compobj.h and then include initguid.h 
  after compobj.h followed by one or more of the guid defintion files. }

{ #define DEFINE_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8) \    EXTERN_C const GUID CDECL FAR name }
{ #define DEFINE_OLEGUID(name, l, w1, w2) \    DEFINE_GUID(name, l, w1, w2, 0xC0,0,0,0,0,0,0,0x46) }


  { Interface ID are just a kind of GUID }
  IID = GUID;
  LPIID = ^IID;

{ #define IID_NULL   GUID_NULL }

  { Class ID are just a kind of GUID}
  CLSID = GUID;
  LPCLSID = ^CLSID;
  REFIID  = ^IID;
  REFGUID = REFIID;
  REFCLSID = REFIID;

{ #define CLSID_NULL   GUID_NULL }

  { this file is the master definition of all GUIDs for the component object
  model and is included in compobj.h.  Some GUIDs for moinkers and storage 
  appear here as well.  All of these GUIDs are OLE GUIDs only in the sense 
  that part of the GUID range owned by OLE was used to define them.  
  
  NOTE: The second byte of all of these GUIDs is 0. }

var
  GUID_NULL: GUID;

  IID_IUnknown: GUID;
  IID_IClassFactory: GUID;
  IID_IMalloc: GUID;
  IID_IMarshal: GUID;

  { RPC related interfaces }
  IID_IRpcChannel: GUID;
  IID_IRpcStub: GUID;
  IID_IStubManager: GUID;
  IID_IRpcProxy: GUID;
  IID_IProxyManager: GUID;
  IID_IPSFactory: GUID;

  { storage related interfaces }
  IID_ILockBytes: GUID;
  IID_IStorage: GUID;
  IID_IStream: GUID;
  IID_IEnumStatStg: GUID;

  { moniker related interfaces }
  IID_IBindCtx: GUID;
  IID_IMoniker: GUID;
  IID_IRunningObjectTable: GUID;
  IID_IInternalMoniker: GUID;

  { storage related interfaces }
  IID_IRootStorage: GUID;
  IID_IDfReserved1: GUID;
  IID_IDfReserved2: GUID;
  IID_IDfReserved3: GUID;

  { concurrency releated interfaces }
  IID_IMessageFilter: GUID;

  { CLSID of standard marshaler }
  CLSID_StdMarshal: GUID;

  { interface on server for getting info for std marshaler }
  IID_IStdMarshalInfo: GUID;

  { interface to inform object of number of external connections }
  IID_IExternalConnection: GUID;

  { class registration flags; passed to CoRegisterClassObject
  class object only generates one instance same class object genereates
  multiple inst.  and local automatically goes into inproc tbl.
  multiple use, but separate control over each context. }

type 
  REGCLS = (
	 REGCLS_SINGLEUSE,       { class object only generates one instance }
	 REGCLS_MULTIPLEUSE,     { same class object genereates multiple inst.
                              and local automatically goes into inproc tbl. }
    REGCLS_MULTI_SEPARATE); { multiple use, but separate control over each
                              context. }

  { interface marshaling definitions }

  { marshaling flags; passed to CoMarshalInterface }
  MSHLFLAGS = (
    MSHLFLAGS_NORMAL,      { normal marshaling via proxy/stub}
    MSHLFLAGS_TABLESTRONG, { keep object alive; must explicitly release}
    MSHLFLAGS_TABLEWEAK);  { doesn't hold object alive; still must release}

  { marshal context: determines the destination context of the marshal operation}
  MSHCTX = (
    MSHCTX_LOCAL,        { unmarshal context is local (eg.shared memory)}
    MSHCTX_NOSHAREDMEM); { unmarshal context has no shared memory access}

  { call type used by IMessageFilter::HandleIncommingMessage}
  CALLTYPE = (
    CALLTYPE_ZERO,
    CALLTYPE_TOPLEVEL,             { toplevel call - no outgoing call }
    CALLTYPE_NESTED,               { callback on behalf of previous outgoing call - should always handle}
    CALLTYPE_ASYNC,                { aysnchronous call - can NOT be rejected}
    CALLTYPE_TOPLEVEL_CALLPENDING, { new toplevel call with new LID}
    CALLTYPE_ASYNC_CALLPENDING);    { async call - can NOT be rejected}

  PIUnknown = ^IUnknown;
  INTERFACEINFO = record
    pUnk: PIUnknown; 
    iid: IID; 
    wMethod: Word; 
  end;
  LPINTERFACEINFO = ^INTERFACEINFO;

  { status of server call - returned by IMessageFilter::HandleIncommingCall
  and passed to  IMessageFilter::RetryRejectedCall }
  SERVERCALL = (SERVERCALL_ISHANDLED, SERVERCALL_REJECTED,
    SERVERCALL_RETRYLATER);

  { Pending type indicates the level of nesting}
  PENDINGTYPE = (PENDINGTYPE_ZERO,
    PENDINGTYPE_TOPLEVEL,  { toplevel call}
    PENDINGTYPE_NESTED);    { nested call}

  { return values of MessagePending}
  PENDINGMSG = (
    PENDINGMSG_CANCELCALL,      { cancel the outgoing call}
    PENDINGMSG_WAITNOPROCESS,   { wait for the return and don't dispatch the message}
    PENDINGMSG_WAITDEFPROCESS); { wait and dispatch the message }

  { bit flags for IExternalConnection}
  EXTCONN = (EXTCONN_ZERO, EXTCONN_STRONG { strong connection} );

  TFileTime = record
    dwLowDateTime, dwHighDateTime: Longint;
  end;

{ IUnknown Interface }

  IUnknown = class
  public
    function QueryInterface(iid: REFIID; var Obj: Pointer): HResult; virtual; cdecl; export; abstract;
    function AddRef: Longint; virtual; cdecl; export; abstract;
    function Release: Longint; virtual; cdecl; export; abstract;
  end;

{ Class Factory Interface }

  IClassFactory = class(IUnknown)
  public
    function CreateInstance(var pUnkOuter: IUnknown; const riid: IID; 
      var ppvObject: LPVOID): HResult; virtual; cdecl; export; abstract;
    function LockServer(fLock: BOOL): HResult; virtual; cdecl; export; abstract;
  end;

{ Memory Allocation Interface }

  IMalloc = class(IUnknown)
  public
    function Alloc(cb: Longint): Pointer; virtual; cdecl; export; abstract;
    function Realloc(pv: Pointer; cb: Longint): Pointer; virtual; cdecl; export; abstract;
    procedure Free(pv: Pointer); virtual; cdecl; export; abstract;
    function GetSize(pv: Pointer): Longint; virtual; cdecl; export; abstract;
    function DidAlloc(pv: Pointer): Integer; virtual; cdecl; export; abstract;
    procedure HeapMinimize; virtual; cdecl; export; abstract;
  end;


{ IMarshal Interface }

  IStream = class;

  IMarshal = class(IUnknown)
  public
    function GetUnmarshalClass(const riid: IID; pv: LPVOID; dwDestContext: 
      Longint; pvDestContext: LPVOID; mshlflags: Longint; var pCid: CLSID): HResult; 
      virtual; cdecl; export; abstract;
    function GetMarshalSizeMax(const riid: IID; pv: LPVOID; dwDestContext: 
      Longint; pvDestContext: LPVOID; mshlflags: Longint; var pSize: Longint): HResult; 
      virtual; cdecl; export; abstract;
    function MarshalInterface(var pStm: IStream; const riid: IID; pv: LPVOID;
      dwDestContext: Longint; pvDestContext: LPVOID; mshlflags: Longint): HResult; 
      virtual; cdecl; export; abstract;
    function UnmarshalInterface(var pStm: IStream; const riid: IID; 
      var ppv: LPVOID): HResult; virtual; cdecl; export; abstract;
    function ReleaseMarshalData(var pStm: IStream): HResult; 
      virtual; cdecl; export; abstract;
    function DisconnectObject(dwReserved: Longint): HResult; 
      virtual; cdecl; export; abstract;
  end;


{ *** IStdMarshalInfo methods ***}

  IStdMarshalInfo = class(IUnknown)
    function GetClassForHandler(dwDestContext: Longint; pvDestContext: LPVOID; 
      var pClsid: CLSID): HResult; virtual; cdecl; export; abstract;
  end;


{ Message Filter Interface }

  IMessageFilter = class(IUnknown)
    function HandleInComingCall(dwCallType: Longint; htaskCaller: HTask; 
      dwTickCount: Longint; dwReserved: Longint): Longint; virtual; cdecl; export; abstract;
    function RetryRejectedCall(htaskCallee: HTask; dwTickCount: Longint; 
      dwRejectType: Longint): Longint; virtual; cdecl; export; abstract;
    function MessagePending(htaskCallee: HTask; dwTickCount: Longint; 
      dwPendingType: Longint): Longint; virtual; cdecl; export; abstract;
  end;


{ External Connection Information }

  IExternalConnection = class(IUnknown)
    function AddConnection(extconn: Longint; reserved: Longint): Longint; 
      virtual; cdecl; export; abstract;
    function ReleaseConnection(extconn: Longint; reserved: Longint; 
      fLastReleaseCloses: BOOL): Longint; virtual; cdecl; export; abstract;
  end;

{ Enumerator Interfaces }

  { Since we don't use parametrized types, we put in explicit declarations
  of the enumerators we need. }

  IEnumString = class(IUnknown)
    function Next(celt: Longint; var rgelt: PStr; 
      var pceltFetched: Longint): HResult; virtual; cdecl; export; abstract;
    function Skip(celt: Longint): HResult; virtual; cdecl; export; abstract;
    function Reset: HResult; virtual; cdecl; export; abstract;
    function Clone(var ppenm: IEnumString): HResult; virtual; cdecl; export; abstract;
  end;

  IEnumUnknown = class(IUnknown)
    function Next(celt: Longint; var rgelt: PIUnknown; 
      var pceltFetched: Longint): HResult; virtual; cdecl; export; abstract;
    function Skip(celt: Longint): HResult; virtual; cdecl; export; abstract;
    function Reset: HResult; virtual; cdecl; export; abstract;
    function Clone(var ppenm: IEnumUnknown): HResult; virtual; cdecl; export; abstract;
  end;

  FuncHResult = function (const p1: IID; const p2: IID; var p3: LPVOID): HResult;

{ Debugging Helpers }

  { rendering options }
  OLERENDER = (OLERENDER_NONE, OLERENDER_DRAW, OLERENDER_FORMAT, 
    OLERENDER_ASIS);
  LPOLERENDER = ^OLERENDER;

  { OLE verb; returned by IEnumOLEVERB}
  OLEVERB = record
    lVerb: Longint; 
    lpszVerbName: PStr; 
    fuFlags: Longint; 
    grfAttribs: Longint; 
  end;
  LPOLEVERB = ^OLEVERB;

  { Bitwise verb attributes used in OLEVERB.grfAttribs}
  OLEVERBATTRIB = (OLEVERBATTRIB_ZERO, OLEVERBATTRIB_NEVERDIRTIES,
    OLEVERBATTRIB_ONCONTAINERMENU);

  { IOleObject::GetUserType optons; determines which form of the string to use}
  USERCLASSTYPE = (USERCLASSTYPE_ZERO, USERCLASSTYPE_FULL,
    USERCLASSTYPE_SHORT, USERCLASSTYPE_APPNAME);

  OLECLOSE = (OLECLOSE_SAVEIFDIRTY, OLECLOSE_NOSAVE, OLECLOSE_PROMPTSAVE);

  { IOleObject::GetMoniker and IOleClientSite::GetMoniker options; determines
  if and how monikers should be assigned. }
  OLEGETMONIKER = (OLEGETMONIKER_ZERO, OLEGETMONIKER_ONLYIFTHERE,
    OLEGETMONIKER_FORCEASSIGN, OLEGETMONIKER_UNASSIGN, 
    OLEGETMONIKER_TEMPFORUSER);

  OLEWHICHMK = (OLEWHICHMK_ZERO, OLEWHICHMK_CONTAINER, OLEWHICHMK_OBJREL,
    OLEWHICHMK_OBJFULL);

{ OLE 1.0 OLESTREAM declarations }

  LPOLESTREAM = ^OLESTREAM;

  OLESTREAMVTBL = record
    Get: function (p1: LPOLESTREAM; p2: HResult; p3: Longint): Longint;
    Put: function (p1: LPOLESTREAM; p2: HResult; p3: Longint): Longint;
  end;
  LPOLESTREAMVTBL = ^OLESTREAMVTBL;

  OLESTREAM = record
    lpstbl: LPOLESTREAMVTBL; 
  end;

{ Clipboard Data structures }

{ variable sized string data may appear here }

  OBJECTDESCRIPTOR = record
    cbSize: Longint; 
    clsid: CLSID; 
    dwDrawAspect: Longint; 
    sizel: SizeLong; 
    pointl: TPointLong; 
    dwStatus: Longint; 
    dwFullUserTypeName: Longint; 
    dwSrcOfCopy: Longint; 
  end;

  IAdviseSink = class;

  PIStream = ^IStream;
  PIStorage = ^IStorage;
  PIOleClientSite = ^IOleClientSite;
  PIOleContainer = ^IOleContainer;
  PIAdviseSink = ^IAdviseSink;

  CLIPFORMAT = Word;

  { Data/View aspect; specifies the desired aspect of the object when 
  drawing or getting data. Data/View target device; determines the 
  device for drawing or gettting data }

  DVTargetDevice = record
    tdSize: Longint; 
    tdDriverNameOffset: Word; 
    tdDeviceNameOffset: Word; 
    tdPortNameOffset: Word; 
    tdExtDevmodeOffset: Word; 
    tdData: Array[1..1] of Byte; 
  end;

  { Format, etc.; completely specifices the kind of data desired, including tymed}
  PDVTargetDevice = ^DVTargetDevice;
  FORMATETC = record
    cfFormat: CLIPFORMAT; 
    ptd: PDVTargetDevice;
    dwAspect: Longint; 
    lindex: Longint; 
    tymed: Longint; 
  end;

  { SToraGe MEDIUM; a block of data on a particular medium }
  rec1 = record
    case Integer of
    1: (hGlobal: THandle); 
    2: (lpszFileName: PStr); 
    3: (pstm: PIStream); 
    4: (pstg: PIStorage); 
  end;

  STGMEDIUM = record
    tymed: Longint; 
    u: rec1; 
    pUnkForRelease: PIUnknown; 
  end;

  IMoniker = class;

  { TYpes of storage MEDiums; determines how data is stored or passed around
  DATA format DIRection }
  DATADIR = (DATADIR_ZERO, DATADIR_GET, DATADIR_SET);

{ Advise Flags}

  { Stats for data; used by several enumerations and by at least one
  implementation of IDataAdviseHolder; if a field is not used, it
  will be NULL. }

  { field used by: EnumAdvise, EnumData (cache), EnumFormats EnumAdvise }
  STATDATA = record
    formatetc: FORMATETC; 
    advf: Longint; 
    pAdvSink: PIAdviseSink; 
    dwConnection: Longint; 
  end;

{ DV Interfaces }

  IEnumFORMATETC = class(IUnknown)
    function Next(celt: Longint; var rgelt: FORMATETC; var pceltFetched: Longint): 
      HResult; virtual; cdecl; export; abstract;
    function Skip(celt: Longint): HResult; virtual; cdecl; export; abstract;
    function Reset: HResult; virtual; cdecl; export; abstract;
    function Clone(var ppenum: IEnumFORMATETC): HResult; 
      virtual; cdecl; export; abstract;
  end;

  IEnumStatData = class(IUnknown)
    function Next(celt: Longint; var rgelt: STATDATA; var pceltFetched: Longint): 
      HResult; virtual; cdecl; export; abstract;
    function Skip(celt: Longint): HResult; virtual; cdecl; export; abstract;
    function Reset: HResult; virtual; cdecl; export; abstract;
    function Clone(var ppenum: IEnumStatData): HResult; 
      virtual; cdecl; export; abstract;
  end;

  IDataObject = class(IUnknown)
    function GetData(var pformatetcIn: FORMATETC; var pmedium: STGMEDIUM):
      HResult; virtual; cdecl; export; abstract;
    function GetDataHere(var pformatetc: FORMATETC; var pmedium: STGMEDIUM): 
      HResult; virtual; cdecl; export; abstract;
    function QueryGetData(var pformatetc: FORMATETC): HResult; 
      virtual; cdecl; export; abstract;
    function GetCanonicalFormatEtc(var pformatetc: FORMATETC; 
      var pformatetcOut: FORMATETC): HResult; virtual; cdecl; export; abstract;
    function SetData(var pformatetc: FORMATETC; var pmedium: STGMEDIUM; 
      fRelease: BOOL): HResult; virtual; cdecl; export; abstract;
    function EnumFormatEtc(dwDirection: Longint; var ppenumFormatEtc: 
      IEnumFORMATETC): HResult; virtual; cdecl; export; abstract;
    function DAdvise(var pFormatetc: FORMATETC; advf: Longint; var pAdvSink: 
      IAdviseSink; var pdwConnection: Longint): HResult; virtual; cdecl; export; abstract;
    function DUnadvise(dwConnection: Longint): HResult; virtual; cdecl; export; abstract;
    function EnumDAdvise(var ppenumAdvise: IEnumStatData): HResult; 
      virtual; cdecl; export; abstract;
  end;

  FuncBOOL = function (p1: Longint): BOOL;
  PFuncBOOL = ^FuncBOOL;

  IViewObject = class(IUnknown)
    function Draw(dwDrawAspect: Longint; lindex: Longint; pvAspect: Pointer; 
	   var ptd: DVTargetDevice; hicTargetDev: HDC; hdcDraw: HDC; const lprcBounds: TRectLong; 
		const lprcWBounds: TRectLong; pfnContinue: PFuncBOOL; dwContinue: Longint): HResult; 
		virtual; cdecl; export; abstract;
    function GetColorSet(dwDrawAspect: Longint; lindex: Longint; pvAspect: Pointer; 
	   var ptd: DVTargetDevice; hicTarget: HDC; var ppColorSet: LPLOGPALETTE): HResult; 
		virtual; cdecl; export; abstract;
    function Freeze(dwDrawAspect: Longint; lindex: Longint; pvAspect: Pointer; 
	   var pdwFreeze: Longint): HResult; virtual; cdecl; export; abstract;
    function Unfreeze(dwFreeze: Longint): HResult; virtual; cdecl; export; abstract;
    function SetAdvise(aspects: Longint; advf: Longint; var pAdvSink: IAdviseSink): HResult; 
	   virtual; cdecl; export; abstract;
    function GetAdvise(var pAspects: Longint; var pAdvf: Longint; 
	   var ppAdvSink: PIAdviseSink): HResult; virtual; cdecl; export; abstract;
  end;

  IViewObject2 = class(IViewObject)
    function Draw(dwDrawAspect: Longint; lindex: Longint; pvAspect: Pointer; 
      var ptd: DVTargetDevice; hicTargetDev: HDC; hdcDraw: HDC; 
      const lprcBounds: TRectLong; const lprcWBounds: TRectLong; pfnContinue: PFuncBOOL; 
      dwContinue: Longint): HResult; virtual; cdecl; export; abstract;
    function GetColorSet(dwDrawAspect: Longint; lindex: Longint; 
      pvAspect: Pointer; var ptd: DVTargetDevice; hicTargetDev: HDC; 
      var ppColorSet: LPLOGPALETTE): HResult; virtual; cdecl; export; abstract;
    function Freeze(dwDrawAspect: Longint; lindex: Longint; pvAspect: Pointer; 
      var pdwFreeze: Longint): HResult; virtual; cdecl; export; abstract;
    function Unfreeze(dwFreeze: Longint): HResult; virtual; cdecl; export; abstract;
    function SetAdvise(aspects: Longint; advf: Longint; 
      var pAdvSink: IAdviseSink): HResult; virtual; cdecl; export; abstract;
    function GetAdvise(var pAspects: Longint; var pAdvf: Longint; 
      var ppAdvSink: PIAdviseSink): HResult; virtual; cdecl; export; abstract;
    function GetExtent(dwDrawAspect: Longint; lindex: Longint; 
      var ptd: DVTargetDevice; var lpsizel: SizeLong): HResult; virtual; cdecl; export; abstract;
  end;

  IAdviseSink = class(IUnknown)
    procedure OnDataChange(var pFormatetc: FORMATETC; var pStgmed: STGMEDIUM);
      virtual; cdecl; export; abstract;
    procedure OnViewChange(dwAspect: Longint; lindex: Longint);
      virtual; cdecl; export; abstract;
    procedure OnRename(var pmk: IMoniker); virtual; cdecl; export; abstract;
    procedure OnSave; virtual; cdecl; export; abstract;
    procedure OnClose; virtual; cdecl; export; abstract;
  end;

  IAdviseSink2 = class(IAdviseSink)
    procedure OnDataChange(var pFormatetc: FORMATETC; var pStgmed: STGMEDIUM);
      virtual; cdecl; export; abstract;
    procedure OnViewChange(dwAspect: Longint; lindex: Longint);
      virtual; cdecl; export; abstract;
    procedure OnRename(var pmk: IMoniker); virtual; cdecl; export; abstract;
    procedure OnSave; virtual; cdecl; export; abstract;
    procedure OnClose; virtual; cdecl; export; abstract;
    procedure OnLinkSrcChange(var pmk: IMoniker); virtual; cdecl; export; abstract;
  end;

  IDataAdviseHolder = class(IUnknown)
    function Advise(pDataObject: IDataObject; var pFetc: FORMATETC; advf: Longint;
      pAdvise: IAdviseSink; var pdwConnection: Longint): HResult; virtual; cdecl; export; abstract;
    function Unadvise(dwConnection: Longint): HResult; virtual; cdecl; export; abstract;
    function EnumAdvise(var ppenumAdvise: IEnumStatData): HResult; virtual; cdecl; export; abstract;
    function SendOnDataChange(pDataObject: IDataObject; dwReserved: Longint; 
      advf: Longint): HResult; virtual; cdecl; export; abstract;
  end;

  IOleCache = class(IUnknown)
    function Cache(var lpFormatetc: FORMATETC; advf: Longint; 
      var lpdwConnection: Longint): HResult; virtual; cdecl; export; abstract;
    function Uncache(dwConnection: Longint): HResult; virtual; cdecl; export; abstract;
    function EnumCache(var ppenumStatData: IEnumStatData): HResult;
      virtual; cdecl; export; abstract;
    function InitCache(pDataObject: IDataObject): HResult; virtual; cdecl; export; abstract;
    function SetData(var pformatetc: FORMATETC; var pmedium: STGMEDIUM; 
      fRelease: BOOL): HResult; virtual; cdecl; export; abstract;
  end;

  DISCARDCACHE = (DISCARDCACHE_SAVEIFDIRTY, DISCARDCACHE_NOSAVE);

  IOleCache2 = class(IOleCache)
    function Cache(var lpFormatetc: FORMATETC; advf: Longint; 
      var lpdwConnection: Longint): HResult; virtual; cdecl; export; abstract;
    function Uncache(dwConnection: Longint): HResult; virtual; cdecl; export; abstract;
    function EnumCache(var ppenumStatData: IEnumStatData): HResult;
      virtual; cdecl; export; abstract;
    function InitCache(pDataObject: IDataObject): HResult; virtual; cdecl; export; abstract;
    function SetData(var pformatetc: FORMATETC; var pmedium: STGMEDIUM; 
      fRelease: BOOL): HResult; virtual; cdecl; export; abstract;
    function UpdateCache(pDataObject: IDataObject; grfUpdf: Longint; 
      pReserved: LPVOID): HResult; virtual; cdecl; export; abstract;
    function DiscardCache(dwDiscardOptions: Longint): HResult; virtual; cdecl; export; abstract;
  end;

  IOleCacheControl = class(IUnknown)
    function QueryInterface(const riid: IID; var ppvObj: LPVOID): HResult; 
      virtual; cdecl; export; abstract;
    function AddRef: Longint; virtual; cdecl; export; abstract;
    function Release: Longint; virtual; cdecl; export; abstract;
    function OnRun(pDataObject: IDataObject): HResult; virtual; cdecl; export; abstract;
    function OnStop: HResult; virtual; cdecl; export; abstract;
  end;

  { Storage commit types }
{  STGC = (STGC_DEFAULT, STGC_OVERWRITE, STGC_ONLYIFCURRENT, STGC_THREE,
    STGC_DANGEROUSLYCOMMITMERELYTODI);}

{ Storage stat buffer }

  TStatStg = record
    pwcsName: PChar;
    dwType: Longint;
    cbSize: ULARGE_INTEGER;
    MTime, CTime, ATime: TFileTime;
    grfMode: Longint;
    grfLocksSupported: Longint;
    ClsId: ClsID;
    grfStateBits: Longint;
    Reserved: Longint;
  end;

  { Storage element types }
{  STGTY = (STGTY_ZERO, STGTY_STORAGE, STGTY_STREAM, STGTY_LOCKBYTES,
    STGTY_PROPERTY);}
  STREAM_SEEK = (STREAM_SEEK_SET, STREAM_SEEK_CUR, STREAM_SEEK_END);
  LOCKTYPE = (LOCK_ZERO, LOCK_WRITE, LOCK_EXCLUSIVE, LOCK_THREE,
    LOCK_ONLYONCE);
{  STGMOVE = (STGMOVE_MOVE, STGMOVE_COPY);
  STATFLAG = (STATFLAG_DEFAULT, STATFLAG_NONAME);}

{ Storage Enumerators }

  IEnumStatStg = class(IUnknown)
    function Next(celt: Longint; var rgelt: TStatStg; var pceltFetched: Longint): HResult;
      virtual; cdecl; export; abstract;
    function Skip(celt: Longint): HResult; virtual; cdecl; export; abstract;
    function Reset: HResult; virtual; cdecl; export; abstract;
    function Clone(var ppenm: IEnumStatStg): HResult; virtual; cdecl; export; abstract;
  end;

{ ILockBytes Interface }

  ILockBytes = class(IUnknown)
    function ReadAt(ulOffsetLow, ulOffsetHigh: Longint; pv: Pointer; cb: Longint; 
      var pcbRead: Longint): HResult; virtual; cdecl; export; abstract;
    function WriteAt(ulOffsetLow, ulOffsetHigh: Longint; const pv: Pointer; cb: Longint; 
      var pcbWritten: Longint): HResult; virtual; cdecl; export; abstract;
    function Flush: HResult; virtual; cdecl; export; abstract;
    function SetSize(cbLow, cbHigh: Longint): HResult; virtual; cdecl; export; abstract;
    function LockRegion(libOffsetLow, libOffsetHigh: Longint; 
      cbLow, cbHigh: Longint; dwLockType: Longint): HResult; virtual; cdecl; export; abstract;
    function UnlockRegion(libOffsetLow, libOffsetHigh: Longint; 
      cbLow, cbHigh: Longint; dwLockType: Longint): HResult; virtual; cdecl; export; abstract;
    function Stat(var pstatstg: TStatStg; grfStatFlag: Longint): HResult; 
      virtual; cdecl; export; abstract;
  end;

{ IStream Interface }

  IStream = class(IUnknown)
  public
    function Read(pv: Pointer; cb: Longint; var pcbRead: Longint): HResult; 
      virtual; cdecl; export; abstract;
    function Write(const pv: Pointer; cb: Longint; var pcbWritten: Longint): HResult; 
      virtual; cdecl; export; abstract;
    function Seek(dlibMoveLow, dblibMoveHigh: Longint; dwOrigin: Longint; 
      var plibNewPositionLow, plibNewPositionHigh: Longint): HResult; virtual; cdecl; export; abstract;
    function SetSize(libNewSizeLow, libNewSizeHigh: Longint): HResult; virtual; cdecl; export; abstract;
    function CopyTo(pstm: IStream; cbLow, cbHigh: Longint; 
      var pcbReadLow, pcbReadHigh: Longint; 
      var pcbWrittenLow, pcbWrittenHigh: Longint): HResult; virtual; cdecl; export; abstract;
    function Commit(grfCommitFlags: Longint): HResult; virtual; cdecl; export; abstract;
    function Revert: HResult; virtual; cdecl; export; abstract;
    function LockRegion(libOffsetLow, libOffsetHigh: Longint; 
      cbLow, cbHigh: Longint; dwLockType: Longint): HResult; virtual; cdecl; export; abstract;
    function UnlockRegion(libOffsetLow, libOffSetHigh: Longint; 
      cbLow, cbHigh: Longint; dwLockType: Longint): HResult; virtual; cdecl; export; abstract;
    function Stat(var pstatstg: TStatStg; grfStatFlag: Longint): HResult; 
      virtual; cdecl; export; abstract;
    function Clone(var ppstm: IStream): HResult; virtual; cdecl; export; abstract;
  end;

{ IStorage Interface }

  IStorage = class(IUnknown)
    function CreateStream(const pwcsName: PChar; grfMode: Longint; reserved1: Longint;
      reserved2: Longint; var ppstm: IStream): HResult; virtual; cdecl; export; abstract;
    function OpenStream(const pwcsName: PChar; reserved1: Pointer; grfMode: Longint;
      reserved2: Longint; var ppstm: IStream): HResult; virtual; cdecl; export; abstract;
    function CreateStorage(const pwcsName: PChar; grfMode: Longint; 
      reserved1: Longint; reserved2: Longint; var ppstg: IStorage): HResult;
      virtual; cdecl; export; abstract;
    function OpenStorage(const pwcsName: PChar; pstgPriority: IStorage; 
      grfMode: Longint; {var} snbExclude: PStr; reserved: Longint;
      var ppstg: IStorage): HResult; virtual; cdecl; export; abstract;
    function CopyTo(ciidExclude: Longint; const rgiidExclude: IID; 
      {var} snbExclude: PStr; pstgDest: IStorage): HResult; virtual; cdecl; export; abstract;
    function MoveElementTo(const lpszName: PChar; pstgDest: IStorage; 
      const lpszNewName: PChar; grfFlags: Longint): HResult; virtual; cdecl; export; abstract;
    function Commit(grfCommitFlags: Longint): HResult; virtual; cdecl; export; abstract;
    function Revert: HResult; virtual; cdecl; export; abstract;
    function EnumElements(reserved1: Longint; reserved2: Pointer; reserved3: Longint;
      var ppenm: IEnumStatStg): HResult; virtual; cdecl; export; abstract;
    function DestroyElement(const pwcsName: PChar): HResult; virtual; cdecl; export; abstract;
    function RenameElement(const pwcsOldName: PChar; 
      const pwcsNewName: PChar): HResult; virtual; cdecl; export; abstract;
    function SetElementTimes(const lpszName: PChar; const pctime: TFileTime; 
      const patime: TFileTime; const pmtime: TFileTime): HResult; 
      virtual; cdecl; export; abstract;
    function SetClass(const clsid: IID): HResult; virtual; cdecl; export; abstract;
    function SetStateBits(grfStateBits: Longint; grfMask: Longint): HResult; 
      virtual; cdecl; export; abstract;
    function Stat(var pstatstg: TStatStg; grfStatFlag: Longint): HResult; 
      virtual; cdecl; export; abstract;
  end;

{ IRootStorage Interface }

  IRootStorage = class(IUnknown)
  public
    function SwitchToFile(lpstrFile: PStr): HResult; virtual; cdecl; export; abstract;
  end;

  IDropTarget = class(IUnknown)
    function DragEnter(pDataObj: IDataObject; grfKeyState: Longint; 
      pt: TPointLong; var pdwEffect: Longint): HResult; virtual; cdecl; export; abstract;
    function DragOver(grfKeyState: Longint; pt: TPointLong; 
      var pdwEffect: Longint): HResult; virtual; cdecl; export; abstract;
    function DragLeave: HResult; virtual; cdecl; export; abstract;
    function Drop(pDataObj: IDataObject; grfKeyState: Longint; pt: TPointLong; 
      var pdwEffect: Longint): HResult; virtual; cdecl; export; abstract;
  end;

  IDropSource = class(IUnknown)
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult;
      virtual; cdecl; export; abstract;
    function GiveFeedback(dwEffect: Longint): HResult; virtual; cdecl; export;  abstract;
  end;

  IPersist = class(IUnknown)
    function GetClassID(var lpClassID: CLSID): HResult; virtual; cdecl; export; abstract;
  end;

  IPersistStorage = class(IPersist)
    function GetClassID(var lpClassID: CLSID): HResult; virtual; cdecl; export; abstract;
    function IsDirty: HResult; virtual; cdecl; export; abstract;
    function InitNew(pStg: IStorage): HResult; virtual; cdecl; export; abstract;
    function Load(pStg: IStorage): HResult; virtual; cdecl; export; abstract;
    function Save(pStgSave: IStorage; fSameAsLoad: BOOL): HResult;
      virtual; cdecl; export; abstract;
    function SaveCompleted(pStgNew: IStorage): HResult; virtual; cdecl; export; abstract;
    function HandsOffStorage: HResult; virtual; cdecl; export; abstract;
  end;


  IPersistStream = class(IPersist)
    function GetClassID(var lpClassID: CLSID): HResult; virtual; cdecl; export; abstract;
    function IsDirty: HResult; virtual; cdecl; export; abstract;
    function Load(pStm: IStream): HResult; virtual; cdecl; export; abstract;
    function Save(pStm: IStream; fClearDirty: BOOL): HResult; virtual; cdecl; export; abstract;
    function GetSizeMax(var pcbSizeLow, pcbSizeHigh: Longint): HResult; virtual; cdecl; export; abstract;
  end;


  IPersistFile = class(IPersist)
    function GetClassID(var lpClassID: CLSID): HResult; virtual; cdecl; export; abstract;
    function IsDirty: HResult; virtual; cdecl; export; abstract;
    function Load(const lpszFileName: LPCSTR; grfMode: Longint): HResult; 
      virtual; cdecl; export; abstract;
    function Save(const lpszFileName: LPCSTR; fRemember: BOOL): HResult; 
      virtual; cdecl; export; abstract;
    function SaveCompleted(const lpszFileName: LPCSTR): HResult;
      virtual; cdecl; export; abstract;
    function GetCurFile(var lplpszFileName: PStr): HResult; virtual; cdecl; export; abstract;
  end;

{ Moniker Object Interfaces }

  BIND_OPTS = record
    cbStruct: Longint; 
    grfFlags: Longint; 
    grfMode: Longint; 
    dwTickCountDeadline: Longint; 
  end;

  PIEnumMoniker = ^IEnumMoniker;
  PIRunningObjectTable = ^IRunningObjectTable;

  IBindCtx = class(IUnknown)
    function RegisterObjectBound(var punk: IUnknown): HResult; virtual; cdecl; export; abstract;
    function RevokeObjectBound(var punk: IUnknown): HResult; virtual; cdecl; export; abstract;
    function ReleaseBoundObjects: HResult; virtual; cdecl; export; abstract;
    function SetBindOptions(var pbindopts: BIND_OPTS): HResult; virtual; cdecl; export; abstract;
    function GetBindOptions(var pbindopts: BIND_OPTS): HResult; virtual; cdecl; export; abstract;
    function GetRunningObjectTable(var pprot: PIRunningObjectTable): HResult; 
      virtual; cdecl; export; abstract;
    function RegisterObjectParam(lpszKey: PStr; var punk: IUnknown): HResult;
      virtual; cdecl; export; abstract;
    function GetObjectParam(lpszKey: PStr; var ppunk: PIUnknown): HResult; 
      virtual; cdecl; export; abstract;
    function EnumObjectParam(var ppenum: IEnumString): HResult; virtual; cdecl; export; abstract;
    function RevokeObjectParam(lpszKey: PStr): HResult; virtual; cdecl; export; abstract;
  end;

  IMoniker = class(IPersistStream)
    function GetClassID(var lpClassID: CLSID): HResult; virtual; cdecl; export; abstract;
    function IsDirty: HResult; virtual; cdecl; export; abstract;
    function Load(pStm: IStream): HResult; virtual; cdecl; export; abstract;
    function Save(pStm: IStream; fClearDirty: BOOL): HResult; virtual; cdecl; export; abstract;
    function GetSizeMax(var pcbSizeLow, pcbSizeHigh: Longint): HResult; virtual; cdecl; export; abstract;
    function BindToObject(pbc: IBindCtx; pmkToLeft: IMoniker; 
      const riidResult: IID; var ppvResult: LPVOID): HResult; virtual; cdecl; export; abstract;
    function BindToStorage(pbc: IBindCtx; pmkToLeft: IMoniker; 
      const riid: IID; var ppvObj: LPVOID): HResult; virtual; cdecl; export; abstract;
    function Reduce(pbc: IBindCtx; dwReduceHowFar: Longint; var ppmkToLeft: IMoniker; 
      var ppmkReduced: IMoniker): HResult; virtual; cdecl; export; abstract;
    function ComposeWith(pmkRight: IMoniker; fOnlyIfNotGeneric: BOOL; 
      var ppmkComposite: IMoniker): HResult; virtual; cdecl; export; abstract;
    function Enum(fForward: BOOL; var ppenumMoniker: PIEnumMoniker): HResult;
      virtual; cdecl; export; abstract;
    function IsEqual(pmkOtherMoniker: IMoniker): HResult; virtual; cdecl; export; abstract;
    function Hash(var pdwHash: Longint): HResult; virtual; cdecl; export; abstract;
    function IsRunning(pbc: IBindCtx; pmkToLeft: IMoniker; 
      pmkNewlyRunning: IMoniker): HResult; virtual; cdecl; export; abstract;
    function GetTimeOfLastChange(pbc: IBindCtx; pmkToLeft: IMoniker; 
      var pfiletime: TFileTime): HResult; virtual; cdecl; export; abstract;
    function Inverse(var ppmk: IMoniker): HResult; virtual; cdecl; export; abstract;
    function CommonPrefixWith(pmkOther: IMoniker; 
      var ppmkPrefix: IMoniker): HResult; virtual; cdecl; export; abstract;
    function RelativePathTo(pmkOther: IMoniker; 
      var ppmkRelPath: IMoniker): HResult; virtual; cdecl; export; abstract;
    function GetDisplayName(pbc: IBindCtx; pmkToLeft: IMoniker; 
      var lplpszDisplayName: PStr): HResult; virtual; cdecl; export; abstract;
    function ParseDisplayName(pbc: IBindCtx; pmkToLeft: IMoniker; 
      lpszDisplayName: PStr; var pchEaten: Longint; 
      var ppmkOut: IMoniker): HResult; virtual; cdecl; export; abstract;
    function IsSystemMoniker(var pdwMksys: Longint): HResult; virtual; cdecl; export; abstract;
  end;

  IRunningObjectTable = class(IUnknown)
    function Register(grfFlags: Longint; var punkObject: IUnknown; 
      pmkObjectName: IMoniker; var pdwRegister: Longint): HResult; virtual; cdecl; export; abstract;
    function Revoke(dwRegister: Longint): HResult; virtual; cdecl; export; abstract;
    function IsRunning(pmkObjectName: IMoniker): HResult; virtual; cdecl; export; abstract;
    function GetObject(pmkObjectName: IMoniker; 
      var ppunkObject: PIUnknown): HResult; virtual; cdecl; export; abstract;
    function NoteChangeTime(dwRegister: Longint; 
      var pfiletime: TFileTime): HResult; virtual; cdecl; export; abstract;
    function GetTimeOfLastChange(pmkObjectName: IMoniker; 
      var pfiletime: TFileTime): HResult; virtual; cdecl; export; abstract;
    function EnumRunning(var ppenumMoniker: PIEnumMoniker): HResult; virtual; cdecl; export; abstract;
  end;


  IEnumMoniker = class(IUnknown)
    function Next(celt: Longint; var rgelt: IMoniker; 
      var pceltFetched: Longint): HResult; virtual; cdecl; export; abstract;
    function Skip(celt: Longint): HResult; virtual; cdecl; export; abstract;
    function Reset: HResult; virtual; cdecl; export; abstract;
    function Clone(var ppenm: IEnumMoniker): HResult; virtual; cdecl; export; abstract;
  end;

{ OLE Object Interfaces }


  IEnumOLEVERB = class(IUnknown)
    function Next(celt: Longint; var rgelt: OLEVERB; 
      var pceltFetched: Longint): HResult; virtual; cdecl; export; abstract;
    function Skip(celt: Longint): HResult; virtual; cdecl; export; abstract;
    function Reset: HResult; virtual; cdecl; export; abstract;
    function Clone(var ppenm: IEnumOLEVERB): HResult; virtual; cdecl; export; abstract;
  end;

  IOleClientSite = class;

  IOleObject = class(IUnknown)
    function SetClientSite(var pClientSite: IOleClientSite): HResult;
      virtual; cdecl; export; abstract;
    function GetClientSite(var ppClientSite: PIOleClientSite): HResult; 
      virtual; cdecl; export; abstract;
    function SetHostNames(const szContainerApp: LPCSTR; 
      const szContainerObj: LPCSTR): HResult; virtual; cdecl; export; abstract;
    function Close(dwSaveOption: Longint): HResult; virtual; cdecl; export; abstract;
    function SetMoniker(dwWhichMoniker: Longint; pmk: IMoniker): HResult; 
      virtual; cdecl; export; abstract;
    function GetMoniker(dwAssign: Longint; dwWhichMoniker: Longint; 
      var ppmk: IMoniker): HResult; virtual; cdecl; export; abstract;
    function InitFromData(pDataObject: IDataObject; fCreation: BOOL; 
      dwReserved: Longint): HResult; virtual; cdecl; export; abstract;
    function GetClipboardData(dwReserved: Longint; 
      var ppDataObject: IDataObject): HResult; virtual; cdecl; export; abstract;
    function DoVerb(iVerb: Longint; lpmsg: LPMSG; var pActiveSite: IOleClientSite; 
      lindex: Longint; hwndParent: HWND; lprcPosRect: TRect): HResult; virtual; cdecl; export; abstract;
    function EnumVerbs(var ppenumOleVerb: IEnumOLEVERB): HResult; virtual; cdecl; export; abstract;
    function Update: HResult; virtual; cdecl; export; abstract;
    function IsUpToDate: HResult; virtual; cdecl; export; abstract;
    function GetUserClassID(var pClsid: CLSID): HResult; virtual; cdecl; export; abstract;
    function GetUserType(dwFormOfType: Longint; var pszUserType: PStr): HResult; 
      virtual; cdecl; export; abstract;
    function SetExtent(dwDrawAspect: Longint; var lpsizel: SizeLong): HResult; 
      virtual; cdecl; export; abstract;
    function GetExtent(dwDrawAspect: Longint; var lpsizel: SizeLong): HResult; 
      virtual; cdecl; export; abstract;
    function Advise(pAdvSink: IAdviseSink; var pdwConnection: Longint): HResult;
      virtual; cdecl; export; abstract;
    function Unadvise(dwConnection: Longint): HResult; virtual; cdecl; export; abstract;
    function EnumAdvise(var ppenumAdvise: IEnumStatData): HResult; virtual; cdecl; export; abstract;
    function GetMiscStatus(dwAspect: Longint; var pdwStatus: Longint): HResult; 
      virtual; cdecl; export; abstract;
    function SetColorScheme(lpLogpal: LPLOGPALETTE): HResult; virtual; cdecl; export; abstract;
  end;

  IOleClientSite = class(IUnknown)
    function SaveObject: HResult; virtual; cdecl; export; abstract;
    function GetMoniker(dwAssign: Longint; dwWhichMoniker: Longint; 
      var ppmk: IMoniker): HResult; virtual; cdecl; export; abstract;
    function GetContainer(var ppContainer: PIOleContainer): HResult; virtual; cdecl; export; abstract;
    function ShowObject: HResult; virtual; cdecl; export; abstract;
    function OnShowWindow(fShow: BOOL): HResult; virtual; cdecl; export; abstract;
    function RequestNewObjectLayout: HResult; virtual; cdecl; export; abstract;
  end;

{ OLE Runnable Object Interface }

  IRunnableObject = class(IUnknown)
    function GetRunningClass(var lpClsid: CLSID): HResult; virtual; cdecl; export; abstract;
    function Run(pbc: IBindCtx): HResult; virtual; cdecl; export; abstract;
    function IsRunning: Integer; virtual; cdecl; export; abstract;
    function LockRunning(fLock: BOOL; fLastUnlockCloses: BOOL): HResult; 
      virtual; cdecl; export; abstract;
    function SetContainedObject(fContained: BOOL): HResult; virtual; cdecl; export; abstract;
  end;

{ OLE Container Interfaces }

  IParseDisplayName = class(IUnknown)
    function ParseDisplayName(pbc: IBindCtx; lpszDisplayName: PStr; 
      var pchEaten: Longint; var ppmkOut: IMoniker): HResult; virtual; cdecl; export; abstract;
  end;


  IOleContainer = class(IParseDisplayName)
    function ParseDisplayName(pbc: IBindCtx; lpszDisplayName: PStr; 
      var pchEaten: Longint; var ppmkOut: IMoniker): HResult; virtual; cdecl; export; abstract;
    function EnumObjects(grfFlags: Longint; 
      var ppenumUnknown: IEnumUnknown): HResult; virtual; cdecl; export; abstract;
    function LockContainer(fLock: BOOL): HResult; virtual; cdecl; export; abstract;
  end;

  BINDSPEED = (BINDSPEED_ZERO, BINDSPEED_INDEFINITE, BINDSPEED_MODERATE,
    BINDSPEED_IMMEDIATE);

  IOleItemContainer = class(IOleContainer)
    function ParseDisplayName(pbc: IBindCtx; lpszDisplayName: PStr; 
      var pchEaten: Longint; var ppmkOut: IMoniker): HResult; virtual; cdecl; export; abstract;
    function EnumObjects(grfFlags: Longint; 
      var ppenumUnknown: IEnumUnknown): HResult; virtual; cdecl; export; abstract;
    function LockContainer(fLock: BOOL): HResult; virtual; cdecl; export; abstract;
    function GetObject(lpszItem: PStr; dwSpeedNeeded: Longint; 
      pbc: IBindCtx; const riid: IID; var ppvObject: LPVOID): HResult; virtual; cdecl; export; abstract;
    function GetObjectStorage(lpszItem: PStr; pbc: IBindCtx; const riid: IID;
      var ppvStorage: LPVOID): HResult; virtual; cdecl; export; abstract;
    function IsRunning(lpszItem: PStr): HResult; virtual; cdecl; export; abstract;
  end;

{ OLE Advise Holder Interface }

  IOleAdviseHolder = class(IUnknown)
    function Advise(pAdvise: IAdviseSink; var pdwConnection: Longint): HResult;
      virtual; cdecl; export; abstract;
    function Unadvise(dwConnection: Longint): HResult; virtual; cdecl; export; abstract;
    function EnumAdvise(var ppenumAdvise: IEnumStatData): HResult; virtual; cdecl; export; abstract;
    function SendOnRename(pmk: IMoniker): HResult; virtual; cdecl; export; abstract;
    function SendOnSave: HResult; virtual; cdecl; export; abstract;
    function SendOnClose: HResult; virtual; cdecl; export; abstract;
  end;

{ OLE Link Interface }

  { Link update options }

  OLEUPDATE = (OLEUPDATE_ZERO, OLEUPDATE_ALWAYS, OLEUPDATE_TWO,
    OLEUPDATE_ONCALL);

  OLELINKBIND = (OLELINKBIND_ZERO, OLELINKBIND_EVENIFCLASSDIFF);

  IOleLink = class(IUnknown)
    function SetUpdateOptions(dwUpdateOpt: Longint): HResult; 
      virtual; cdecl; export; abstract;
    function GetUpdateOptions(var pdwUpdateOpt: Longint): HResult; virtual; cdecl; export; abstract;
    function SetSourceMoniker(pmk: IMoniker; const rclsid: IID): HResult; 
      virtual; cdecl; export; abstract;
    function GetSourceMoniker(var ppmk: IMoniker): HResult; virtual; cdecl; export; abstract;
    function SetSourceDisplayName(const lpszDisplayName: LPCSTR): HResult; 
      virtual; cdecl; export; abstract;
    function GetSourceDisplayName(var lplpszDisplayName: PStr): HResult; 
      virtual; cdecl; export; abstract;
    function BindToSource(bindflags: Longint; pbc: IBindCtx): HResult; 
      virtual; cdecl; export; abstract;
    function BindIfRunning: HResult; virtual; cdecl; export; abstract;
    function GetBoundSource(var ppUnk: PIUnknown): HResult; virtual; cdecl; export; abstract;
    function UnbindSource: HResult; virtual; cdecl; export; abstract;
    function Update(pbc: IBindCtx): HResult; virtual; cdecl; export; abstract;
  end;

{ OLE InPlace Editing Interfaces }

{ OleInPlaceFrameInfo}
  OIFI = record
    cb: Word; 
    fMDIApp: BOOL; 
    hwndFrame: HWND; 
    haccel: HAccel; 
    cAccelEntries: Integer; 
  end;

  OleMenuGroupWidths = record
    width: Array[1..6] of Longint; 
  end;

  BORDERWIDTHS = TRect;
  LPBORDERWIDTHS = ^TRect;
  LPCBORDERWIDTHS = ^TRect;

  IOleWindow = class(IUnknown)
    function GetWindow(var lphwnd: HWND): HResult; virtual; cdecl; export; abstract;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; virtual; cdecl; export; abstract;
  end;

  IOleInPlaceObject = class(IOleWindow)
    function GetWindow(var lphwnd: HWND): HResult; virtual; cdecl; export; abstract;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; virtual; cdecl; export; abstract;
    function InPlaceDeactivate: HResult; virtual; cdecl; export; abstract;
    function UIDeactivate: HResult; virtual; cdecl; export; abstract;
    function SetObjectRects(const lprcPosRect: TRect; 
      const lprcClipRect: TRect): HResult; virtual; cdecl; export; abstract;
    function ReactivateAndUndo: HResult; virtual; cdecl; export; abstract;
  end;


  IOleInPlaceUIWindow = class;
  IOleInPlaceActiveObject = class(IOleWindow)
    function GetWindow(var lphwnd: HWND): HResult; virtual; cdecl; export; abstract;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; virtual; cdecl; export; abstract;
    function TranslateAccelerator(lpmsg: LPMSG): HResult; virtual; cdecl; export; abstract;
    function OnFrameWindowActivate(fActivate: BOOL): HResult; virtual; cdecl; export; abstract;
    function OnDocWindowActivate(fActivate: BOOL): HResult; virtual; cdecl; export; abstract;
    function ResizeBorder(const lprectBorder: TRect; var lpUIWindow: IOleInPlaceUIWindow; 
      fFrameWindow: BOOL): HResult; virtual; cdecl; export; abstract;
    function EnableModeless(fEnable: BOOL): HResult; virtual; cdecl; export; abstract;
  end;

  IOleInPlaceUIWindow = class(IOleWindow)
    function GetWindow(var lphwnd: HWND): HResult; virtual; cdecl; export; abstract;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; virtual; cdecl; export; abstract;
    function GetBorder(var lprectBorder: TRect): HResult; virtual; cdecl; export; abstract;
    function RequestBorderSpace(const lpborderwidths: TRect): HResult; virtual; cdecl; export; abstract;
    function SetBorderSpace(const lpborderwidths: TRect): HResult; virtual; cdecl; export; abstract;
    function SetActiveObject(lpActiveObject: IOleInPlaceActiveObject; 
      const lpszObjName: LPCSTR): HResult; virtual; cdecl; export; abstract;
  end;

  IOleInPlaceFrame = class(IOleInPlaceUIWindow)
    function GetWindow(var lphwnd: HWND): HResult; virtual; cdecl; export; abstract;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; virtual; cdecl; export; abstract;
    function GetBorder(var lprectBorder: TRect): HResult; virtual; cdecl; export; abstract;
    function RequestBorderSpace(const lpborderwidths: TRect): HResult; virtual; cdecl; export; abstract;
    function SetBorderSpace(const lpborderwidths: TRect): HResult; virtual; cdecl; export; abstract;
    function SetActiveObject(lpActiveObject: IOleInPlaceActiveObject; 
      const lpszObjName: LPCSTR): HResult; virtual; cdecl; export; abstract;
    function InsertMenus(hmenuShared: HMenu; 
      var lpMenuWidths: OleMenuGroupWidths): HResult; virtual; cdecl; export; abstract;
    function SetMenu(hmenuShared: HMenu; holemenu: Word; 
      hwndActiveObject: HWND): HResult; virtual; cdecl; export; abstract;
    function RemoveMenus(hmenuShared: HMenu): HResult; virtual; cdecl; export; abstract;
    function SetStatusText(const lpszStatusText: LPCSTR): HResult; virtual; cdecl; export; abstract;
    function EnableModeless(fEnable: BOOL): HResult; virtual; cdecl; export; abstract;
    function TranslateAccelerator(lpmsg: LPMSG; wID: Word): HResult; virtual; cdecl; export; abstract;
  end;


  IOleInPlaceSite = class(IOleWindow)
    function GetWindow(var lphwnd: HWND): HResult; virtual; cdecl; export; abstract;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; virtual; cdecl; export; abstract;
    function CanInPlaceActivate: HResult; virtual; cdecl; export; abstract;
    function OnInPlaceActivate: HResult; virtual; cdecl; export; abstract;
    function OnUIActivate: HResult; virtual; cdecl; export; abstract;
    function GetWindowContext(var lplpFrame: IOleInPlaceFrame; 
      var lplpDoc: IOleInPlaceUIWindow; var lprcPosRect: TRect; 
      var lprcClipRect: TRect; var lpFrameInfo: OIFI): HResult; virtual; cdecl; export; abstract;
    function Scroll(scrollExtent: Size): HResult; virtual; cdecl; export; abstract;
    function OnUIDeactivate(fUndoable: BOOL): HResult; virtual; cdecl; export; abstract;
    function OnInPlaceDeactivate: HResult; virtual; cdecl; export; abstract;
    function DiscardUndoState: HResult; virtual; cdecl; export; abstract;
    function DeactivateAndUndo: HResult; virtual; cdecl; export; abstract;
    function OnPosRectChange(const lprcPosRect: TRect): HResult; virtual; cdecl; export; abstract;
  end;

{ OLE API Prototypes }

{*********** Function Declarations *************}

{ STD Object API Prototypes }

function GetScode(hr: HResult): SCode; 
function ResultFromScode(sc: SCode): HResult;
function CoBuildVersion: HResult;
function PropagateResult(hrPrev: HResult; scNew: SCode): HResult;

{ inituninit }

function CoInitialize(pMalloc: IMalloc): HResult;
procedure CoUninitialize;
function CoGetMalloc(dwMemContext: Longint; var ppMalloc: IMalloc): HResult; 
function CoGetCurrentProcess: Longint;
function CoCreateStandardMalloc(memctx: Longint; var ppMalloc: IMalloc): HResult;

{ register revoke get class objects }

function CoGetClassObject(const rclsid: IID; dwClsContext: Longint; 
  pvReserved: LPVOID; const riid: IID; var ppv: LPVOID): HResult; 
function CoRegisterClassObject(const rclsid: IID; var pUnk: IUnknown; 
  dwClsContext: Longint; flags: Longint; var lpdwRegister: Longint): HResult;
function CoRevokeClassObject(dwRegister: Longint): HResult;

{ marshaling interface pointers }

function CoMarshalInterface(var pStm: IStream; const riid: IID; var pUnk: IUnknown; 
  dwDestContext: Longint; pvDestContext: LPVOID; mshlflags: Longint): HResult;
function CoUnmarshalInterface(var pStm: IStream; const riid: IID; var ppv: LPVOID): HResult;
function CoMarshalHresult(var pstm: IStream; hresult: HResult): HResult; 
function CoUnmarshalHresult(var pstm: IStream; var phresult: HResult): HResult;
function CoReleaseMarshalData(var pStm: IStream): HResult;
function CoDisconnectObject(var pUnk: IUnknown; dwReserved: Longint): HResult; 
function CoLockObjectExternal(var pUnk: IUnknown; fLock: BOOL; fLastUnlockReleases: BOOL): HResult;
function CoGetStandardMarshal(const riid: IID; var pUnk: IUnknown; dwDestContext: Longint; 
  pvDestContext: LPVOID; mshlflags: Longint; var ppMarshal: IMarshal): HResult;
function CoIsHandlerConnected(var pUnk: IUnknown): Boolean;

{ dll loading helpers; keeps track of ref counts and unloads all on exit }

function CoLoadLibrary(lpszLibName: PStr; bAutoFree: BOOL): THandle;
procedure CoFreeLibrary(hInst: THandle);
procedure CoFreeAllLibraries;
procedure CoFreeUnusedLibraries;

{ helper for creating instances }

function CoCreateInstance(const rclsid: IID; var pUnkOuter: IUnknown; 
  dwClsContext: Longint; const riid: IID; var ppv: LPVOID): HResult;

{ other helpers }

function IsEqualGUID(rguid1 : REFGUID; rguid2 : REFGUID) : Boolean;
function StringFromCLSID(const rclsid: IID; var lplpsz: PStr): HResult; 
function CLSIDFromString(lpsz: PStr; var pclsid: CLSID): HResult;
function StringFromIID(const rclsid: IID; var lplpsz: PStr): HResult; 
function IIDFromString(lpsz: PStr; var lpiid: IID): HResult;
function CoIsOle1Class(const rclsid: IID): Boolean;
function ProgIDFromCLSID(const clsid: IID; var lplpszProgID: PStr): HResult;
function CLSIDFromProgID(const lpszProgID: LPCSTR; var lpclsid: CLSID): HResult;
function StringFromGUID2(const rguid: IID; lpsz: PStr; cbMax: Integer): Integer;
function CoCreateGuid(var pguid: GUID): HResult;
function CoFileTimeToDosDateTime(var lpFileTime: TFileTime; var lpDosDate: Word;
  var lpDosTime: Word): Boolean;
function CoDosDateTimeToFileTime(nDosDate: Word; nDosTime: Word; 
  var lpFileTime: TFileTime): Boolean;
function CoFileTimeNow(var lpFileTime: TFileTime): HResult;
function CoRegisterMessageFilter(lpMessageFilter: IMessageFilter; 
  var lplpMessageFilter: IMessageFilter): HResult;

{ TreatAs APIS }

function CoGetTreatAsClass(const clsidOld: IID; var pClsidNew: CLSID): HResult;
function CoTreatAsClass(const clsidOld: IID; const clsidNew: IID): HResult; 

{ the server dlls must define their DllGetClassObject and DllCanUnloadNow
  to match these; the typedefs are located here to ensure all are changed at 
  the same time. }

function DllGetClassObject(const rclsid: IID; const riid: IID; var ppv: LPVOID): HResult;
function DllCanUnloadNow : HResult;

{ DV APIs }

function CreateDataAdviseHolder(var ppDAHolder: IDataAdviseHolder): HResult;
function CreateDataCache(var pUnkOuter: IUnknown; const rclsid: IID;
  const iid: IID; var ppv: LPVOID): HResult;

{ Storage API Prototypes }

function StgCreateDocfile(const pwcsName: LPCSTR; grfMode: Longint;
  reserved: Longint; var ppstgOpen: IStorage): HResult;
function StgCreateDocfileOnILockBytes(plkbyt: ILockBytes; grfMode: Longint;
  reserved: Longint; var ppstgOpen: IStorage): HResult;
function StgOpenStorage(const pwcsName: LPCSTR; pstgPriority: IStorage; grfMode: Longint;
  {var} snbExclude: PStr; reserved: Longint; var ppstgOpen: IStorage): HResult;
function StgOpenStorageOnILockBytes(plkbyt: ILockBytes; pstgPriority: IStorage;
  grfMode: Longint; {var} snbExclude: PStr; reserved: Longint; var ppstgOpen: IStorage): HResult;
function StgIsStorageFile(const pwcsName: LPCSTR): HResult;
function StgIsStorageILockBytes(plkbyt: ILockBytes): HResult;
function StgSetTimes(const lpszName: LPCSTR; const pctime: TFileTime;
  const patime: TFileTime; const pmtime: TFileTime): HResult;

{ IMoniker methods }

function BindMoniker(pmk: IMoniker; grfOpt: Longint; const iidResult: IID; 
  var ppvResult: LPVOID): HResult;
function ParseDisplayName(pbc: IBindCtx; szUserName: PStr; 
  var pchEaten: Longint; var ppmk: IMoniker): HResult;
function MonikerRelativePathTo(pmkSrc: IMoniker; pmkDest: IMoniker; 
  var ppmkRelPath: IMoniker; fCalledFromMethod: BOOL): HResult;
function MonikerCommonPrefixWith(pmkThis: IMoniker; pmkOther: IMoniker; 
  var ppmkCommon: IMoniker): HResult;
function CreateBindCtx(reserved: Longint; var ppbc: IBindCtx): HResult;
function CreateGenericComposite(pmkFirst: IMoniker; pmkRest: IMoniker; 
  var ppmkComposite: IMoniker): HResult;
function GetClassFile(const szFilename: LPCSTR; var pclsid: CLSID): HResult; 
function CreateFileMoniker(lpszPathName: PStr; var ppmk: IMoniker): HResult;
function CreateItemMoniker(lpszDelim: PStr; lpszItem: PStr; var ppmk: IMoniker): HResult;
function CreateAntiMoniker(var ppmk: IMoniker): HResult;
function CreatePointerMoniker(var punk: IUnknown; var ppmk: IMoniker): HResult;
function GetRunningObjectTable(reserved: Longint; var pprot: IRunningObjectTable): HResult;

{ OLE API Prototypes }

function OleBuildVersion: HResult;

{ helper functions }

function ReadClassStg(pStg: IStorage; var pclsid: CLSID): HResult;
function WriteClassStg(pStg: IStorage; const rclsid: IID): HResult;
function ReadClassStm(pStm: IStream; var pclsid: CLSID): HResult;
function WriteClassStm(pStm: IStream; const rclsid: IID): HResult;
function WriteFmtUserTypeStg(pstg: IStorage; cf: CLIPFORMAT; lpszUserType: PStr): HResult;
function ReadFmtUserTypeStg(pstg: IStorage; var pcf: CLIPFORMAT; 
  var lplpszUserType: PStr): HResult;

{ initterm }

function OleInitialize(pMalloc: IMalloc): HResult;
procedure OleUninitialize;

{ APIs to query whether (EmbeddedLinked) object can be created from 
   the data object }

function OleQueryLinkFromData(pSrcDataObject: IDataObject): HResult;
function OleQueryCreateFromData(pSrcDataObject: IDataObject): HResult;

{ Object creation APIs }

function OleCreate(const rclsid: IID; const riid: IID; renderopt: Longint; 
  var pFormatEtc: FORMATETC; pClientSite: IOleClientSite; 
  pStg: IStorage; var ppvObj: LPVOID): HResult; 
function OleCreateFromData(pSrcDataObj: IDataObject; const riid: IID; 
  renderopt: Longint; var pFormatEtc: FORMATETC; pClientSite: IOleClientSite;
  pStg: IStorage; var ppvObj: LPVOID): HResult;
function OleCreateLinkFromData(pSrcDataObj: IDataObject; const riid: IID; 
  renderopt: Longint; var pFormatEtc: FORMATETC; pClientSite: IOleClientSite;
  pStg: IStorage; var ppvObj: LPVOID): HResult;
function OleCreateStaticFromData(pSrcDataObj: IDataObject; const iid: IID; 
  renderopt: Longint; var pFormatEtc: FORMATETC; pClientSite: IOleClientSite;
  pStg: IStorage; var ppvObj: LPVOID): HResult;
function OleCreateLink(pmkLinkSrc: IMoniker; const riid: IID; 
  renderopt: Longint; var lpFormatEtc: FORMATETC; pClientSite: IOleClientSite;
  pStg: IStorage; var ppvObj: LPVOID): HResult;
function OleCreateLinkToFile(const lpszFileName: LPCSTR; const riid: IID; 
  renderopt: Longint; var lpFormatEtc: FORMATETC; pClientSite: IOleClientSite;
  pStg: IStorage; var ppvObj: LPVOID): HResult;
function OleCreateFromFile(const rclsid: IID; const lpszFileName: LPCSTR; 
  const riid: IID; renderopt: Longint; var lpFormatEtc: FORMATETC; 
  pClientSite: IOleClientSite; pStg: IStorage; var ppvObj: LPVOID): HResult;
function OleLoad(pStg: IStorage; const riid: IID; pClientSite: IOleClientSite;
  var ppvObj: LPVOID): HResult;
function OleSave(pPS: IPersistStorage; pStg: IStorage; fSameAsLoad: BOOL): HResult;
function OleLoadFromStream(pStm: IStream; const iidInterface: IID; var ppvObj: LPVOID): HResult;
function OleSaveToStream(pPStm: IPersistStream; pStm: IStream): HResult; 
function OleSetContainedObject(var pUnknown: IUnknown; fContained: BOOL): HResult;
function OleNoteObjectVisible(var pUnknown: IUnknown; fVisible: BOOL): HResult;

{ DragDrop APIs }

function RegisterDragDrop(hwnd: HWND; pDropTarget: IDropTarget): HResult; 
function RevokeDragDrop(hwnd: HWND): HResult;
function DoDragDrop(pDataObj: IDataObject; pDropSource: IDropSource; 
  dwOKEffects: Longint; var pdwEffect: Longint): HResult; 
                        
{ Clipboard APIs }

function OleSetClipboard(pDataObj: IDataObject): HResult;
function OleGetClipboard(var ppDataObj: IDataObject): HResult;
function OleFlushClipboard: HResult;
function OleIsCurrentClipboard(pDataObj: IDataObject): HResult;

{ InPlace Editing APIs }

function OleCreateMenuDescriptor(hmenuCombined: HMenu; var lpMenuWidths: OleMenuGroupWidths): Word;
function OleSetMenuDescriptor(holemenu: Word; hwndFrame: HWND; 
  hwndActiveObject: HWND; lpFrame: IOleInPlaceFrame; lpActiveObj: IOleInPlaceActiveObject): HResult; 
function OleDestroyMenuDescriptor(holemenu: Word): HResult;
function OleTranslateAccelerator(lpFrame: IOleInPlaceFrame; 
  var lpFrameInfo: OIFI; lpmsg: LPMSG): HResult;

{ Helper APIs }

function OleDuplicateData(hSrc: THandle; cfFormat: CLIPFORMAT; uiFlags: Word): THandle;
function OleDraw(var pUnknown: IUnknown; dwAspect: Longint; hdcDraw: HDC; 
  const lprcBounds: TRect): HResult;
function OleRun(var pUnknown: IUnknown): HResult;
function OleIsRunning(pObject: IOleObject): Boolean;
function OleLockRunning(var pUnknown: IUnknown; fLock: BOOL; fLastUnlockCloses: BOOL): HResult;
procedure ReleaseStgMedium(var p1: STGMEDIUM);
function CreateOleAdviseHolder(var ppOAHolder: IOleAdviseHolder): HResult; 
function OleCreateDefaultHandler(const clsid: IID; var pUnkOuter: IUnknown; 
  const riid: IID; var lplpObj: LPVOID): HResult; 
function OleCreateEmbeddingHelper(const clsid: IID; var pUnkOuter: IUnknown; 
  flags: Longint; pCF: IClassFactory; const riid: IID; var lplpObj: LPVOID): HResult;
function IsAccelerator(hAccel: HAccel; cAccelEntries: Integer; lpMsg: LPMSG; 
  var lpwCmd: Word): Boolean;

{ Icon extraction Helper APIs }

function OleGetIconOfFile(lpszPath: PStr; fUseFileAsLabel: BOOL): HGlobal; 
function OleGetIconOfClass(const rclsid: IID; lpszLabel: PStr; fUseTypeAsLabel: BOOL): HGlobal;
function OleMetafilePictFromIconAndLabel(hIcon: HIcon; lpszLabel: PStr; 
  lpszSourceFile: PStr; iIconIndex: Word): HGlobal; 

{ Registration Database Helper APIs }

function OleRegGetUserType(const clsid: IID; dwFormOfType: Longint; var pszUserType: PStr): HResult;
function OleRegGetMiscStatus(const clsid: IID; dwAspect: Longint; var pdwStatus: Longint): HResult;
function OleRegEnumFormatEtc(const clsid: IID; dwDirection: Longint; var ppenum: IEnumFORMATETC): HResult;
function OleRegEnumVerbs(const clsid: IID; var ppenum: IEnumOLEVERB): HResult;

{ OLE 1.0 conversion APIS }

function OleConvertIStorageToOLESTREAM(pstg: IStorage; var polestm: OLESTREAM): HResult;
function OleConvertOLESTREAMToIStorage(var polestm: OLESTREAM; pstg: IStorage;
  const ptd: DVTargetDevice): HResult;
function OleConvertIStorageToOLESTREAMEx(pstg: IStorage; cfFormat: CLIPFORMAT;
  lWidth: Longint; lHeight: Longint; dwSize: Longint; var pmedium: STGMEDIUM; 
  var polestm: OLESTREAM): HResult;
function OleConvertOLESTREAMToIStorageEx(var polestm: OLESTREAM; pstg: IStorage; 
  var pcfFormat: CLIPFORMAT; var plwWid: Longint; var plHeight: Longint; 
  var pdwSize: Longint; var pmedium: STGMEDIUM): HResult;

{ Storage Utility APIs }

function GetHGlobalFromILockBytes(plkbyt: ILockBytes; var phglobal: HGlobal): HResult;
function CreateILockBytesOnHGlobal(hGlobal: HGlobal; fDeleteOnRelease: BOOL; 
  var pplkbyt: ILockBytes): HResult;
function GetHGlobalFromStream(pstm: IStream; var phglobal: HGlobal): HResult;
function CreateStreamOnHGlobal(hGlobal: HGlobal; fDeleteOnRelease: BOOL; 
  var ppstm: IStream): HResult;

{ ConvertTo APIS }

function OleDoAutoConvert(pStg: IStorage; var pClsidNew: CLSID): HResult; 
function OleGetAutoConvert(const clsidOld: IID; var pClsidNew: CLSID): HResult;
function OleSetAutoConvert(const clsidOld: IID; const clsidNew: IID): HResult;
function GetConvertStg(pStg: IStorage): HResult;
function SetConvertStg(pStg: IStorage; fConvert: BOOL): HResult;

implementation

{ replacements for c-macros }
function SUCCEEDED(Status: SCode): Boolean;
begin
  Result:= Status >= 0;
end;

function FAILED(Status: SCode): Boolean;
begin
  Result:= Status < 0;
end;

function SUCCEEDEDHR(hr: HResult): Boolean;
begin
  Result:= (SCode(hr) and $800FFFFF) >= 0;
end;

function FAILEDHR(hr: HResult): Boolean;
begin
  Result:= (SCode(hr) and $800FFFFF) < 0;
end;

function GetScode(hr: HResult): SCode;
begin
  Result:= SCode(hr) and $800FFFFF;
end;

function ResultFromScode(sc: SCode): HResult;
begin
  Result:= HResult(sc and $800FFFFF);
end;

function SCODE_CODE(sc: SCODE): Integer;      
begin
  Result:= sc and $FFFF;
end;

function SCODE_FACILITY(sc: SCode): Integer;  
begin
  Result:= (sc shr 16) and $1fff;
end;

function SCODE_SEVERITY(sc: SCode): Integer;  
begin
  Result:= (sc shr 31) and $01;
end;


{ STD Object API Prototypes }
function PropagateResult; external 'compobj';
function CoBuildVersion; external 'compobj';
function CoInitialize; external 'compobj';
procedure CoUninitialize; external 'compobj';
function CoGetMalloc; external 'compobj';
function CoGetCurrentProcess; external 'compobj';
function CoCreateStandardMalloc; external 'compobj';
function CoGetClassObject; external 'compobj';
function CoRegisterClassObject; external 'compobj';
function CoRevokeClassObject; external 'compobj';
function CoMarshalInterface; external 'compobj';
function CoUnmarshalInterface; external 'compobj';
function CoMarshalHresult; external 'compobj';
function CoUnmarshalHresult; external 'compobj';
function CoReleaseMarshalData; external 'compobj';
function CoDisconnectObject; external 'compobj';
function CoLockObjectExternal; external 'compobj';
function CoGetStandardMarshal; external 'compobj';
function CoIsHandlerConnected; external 'compobj';
function CoLoadLibrary; external 'compobj';
procedure CoFreeLibrary; external 'compobj';
procedure CoFreeAllLibraries; external 'compobj';
procedure CoFreeUnusedLibraries; external 'compobj';
function CoCreateInstance; external 'compobj';
function IsEqualGUID; external 'compobj';
function StringFromCLSID; external 'compobj';
function CLSIDFromString; external 'compobj';
function StringFromIID; external 'compobj';
function IIDFromString; external 'compobj';
function CoIsOle1Class; external 'compobj';
function ProgIDFromCLSID; external 'compobj';
function CLSIDFromProgID; external 'compobj';
function StringFromGUID2; external 'compobj';
function CoCreateGuid; external 'compobj';
function CoFileTimeToDosDateTime; external 'compobj';
function CoDosDateTimeToFileTime; external 'compobj';
function CoFileTimeNow; external 'compobj';
function CoRegisterMessageFilter; external 'compobj';
function CoGetTreatAsClass; external 'compobj';
function CoTreatAsClass; external 'compobj';
function DllGetClassObject; external 'compobj';
function DllCanUnloadNow; external 'compobj';

{ DV APIs }
function CreateDataAdviseHolder; external 'ole2';
function CreateDataCache; external 'ole2';

{ Storage API Prototypes }
function StgCreateDocfile; external 'storage';
function StgCreateDocfileOnILockBytes; external 'storage';
function StgOpenStorage; external 'storage';
function StgOpenStorageOnILockBytes; external 'storage';
function StgIsStorageFile; external 'storage';
function StgIsStorageILockBytes; external 'storage';
function StgSetTimes; external 'storage';

{ IMoniker methods }
function BindMoniker; external 'ole2';
function ParseDisplayName; external 'ole2';
function MonikerRelativePathTo; external 'ole2';
function MonikerCommonPrefixWith; external 'ole2';
function CreateBindCtx; external 'ole2';
function CreateGenericComposite; external 'ole2';
function GetClassFile; external 'ole2';
function CreateFileMoniker; external 'ole2';
function CreateItemMoniker; external 'ole2';
function CreateAntiMoniker; external 'ole2';
function CreatePointerMoniker; external 'ole2';
function GetRunningObjectTable; external 'ole2';

{ OLE API Prototypes }
function OleBuildVersion; external 'ole2';

{ helper functions }
function ReadClassStg; external 'ole2';
function WriteClassStg; external 'ole2';
function ReadClassStm; external 'ole2';
function WriteClassStm; external 'ole2';
function WriteFmtUserTypeStg; external 'ole2';
function ReadFmtUserTypeStg; external 'ole2';

{ initterm }
function OleInitialize; external 'ole2';
procedure OleUninitialize; external 'ole2';

{ APIs to query whether (EmbeddedLinked) object can be created from 
   the data object }
function OleQueryLinkFromData; external 'ole2';
function OleQueryCreateFromData; external 'ole2';

{ Object creation APIs }
function OleCreate; external 'ole2';
function OleCreateFromData; external 'ole2';
function OleCreateLinkFromData; external 'ole2';
function OleCreateStaticFromData; external 'ole2';
function OleCreateLink; external 'ole2';
function OleCreateLinkToFile; external 'ole2';
function OleCreateFromFile; external 'ole2';
function OleLoad; external 'ole2';
function OleSave; external 'ole2';
function OleLoadFromStream; external 'ole2';
function OleSaveToStream; external 'ole2';
function OleSetContainedObject; external 'ole2';
function OleNoteObjectVisible; external 'ole2';

{ DragDrop APIs }
function RegisterDragDrop; external 'ole2';
function RevokeDragDrop; external 'ole2';
function DoDragDrop; external 'ole2';

{ Clipboard APIs }
function OleSetClipboard; external 'ole2';
function OleGetClipboard; external 'ole2';
function OleFlushClipboard; external 'ole2';
function OleIsCurrentClipboard; external 'ole2';

{ InPlace Editing APIs }
function OleCreateMenuDescriptor; external 'ole2';
function OleSetMenuDescriptor; external 'ole2';
function OleDestroyMenuDescriptor; external 'ole2';
function OleTranslateAccelerator; external 'ole2';

{ Helper APIs }
function OleDuplicateData; external 'ole2';
function OleDraw; external 'ole2';
function OleRun; external 'ole2';
function OleIsRunning; external 'ole2';
function OleLockRunning; external 'ole2';
procedure ReleaseStgMedium; external 'ole2';
function CreateOleAdviseHolder; external 'ole2';
function OleCreateDefaultHandler; external 'ole2';
function OleCreateEmbeddingHelper; external 'ole2';
function IsAccelerator; external 'ole2';

{ Icon extraction Helper APIs }
function OleGetIconOfFile; external 'ole2';
function OleGetIconOfClass; external 'ole2';
function OleMetafilePictFromIconAndLabel; external 'ole2';

{ Registration Database Helper APIs }
function OleRegGetUserType; external 'ole2';
function OleRegGetMiscStatus; external 'ole2';
function OleRegEnumFormatEtc; external 'ole2';
function OleRegEnumVerbs; external 'ole2';

{ OLE 1.0 conversion APIS }
function OleConvertIStorageToOLESTREAM; external 'ole2';
function OleConvertOLESTREAMToIStorage; external 'ole2';
function OleConvertIStorageToOLESTREAMEx; external 'ole2';
function OleConvertOLESTREAMToIStorageEx; external 'ole2';

{ Storage Utility APIs }
function GetHGlobalFromILockBytes; external 'ole2';
function CreateILockBytesOnHGlobal; external 'ole2';
function GetHGlobalFromStream; external 'ole2';
function CreateStreamOnHGlobal; external 'ole2';

{ ConvertTo APIS }
function OleDoAutoConvert; external 'ole2';
function OleGetAutoConvert; external 'ole2';
function OleSetAutoConvert; external 'ole2';
function GetConvertStg; external 'ole2';
function SetConvertStg; external 'ole2';

end.
