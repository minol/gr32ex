unit SFX_DLGS;

interface
uses Windows, SysUtils, Messages, CommCtrl, SFDS, SFDS_Consts,
  ShlObj, ActiveX;

procedure CreateDialog(ParentDlgHandle: HWND;
  ResourceName: string; Modal: Boolean; PResDlgProc : Pointer);
function ResDlgProcMain(hwndDlg : HWND; uMsg : integer; wParam,
  lParam : integer) : integer; stdcall;
function ResDlgProcProgress(hwndDlg : HWND; uMsg : integer; wParam,
  lParam : integer) : integer; stdcall;
function ResDlgProcReplaceFile(hwndDlg : HWND; uMsg : integer; wParam,
  lParam : integer) : integer; stdcall;
function ResDlgProcInfo(hwndDlg : HWND; uMsg : integer; wParam,
  lParam : integer) : integer; stdcall;

type
  TProgressObject = class(TObject)
    public
      procedure ProgressEvent(Sender : TSFDSCustomSource; Progress, MaxProgress,
        ThisTime : Int64; var Cancel : Boolean; ArcName, StreamName : string);
  end;

var DlgResult : Integer = 0;
    DlgHandle : HWND = 0;
    DlgCancel : Boolean = False;
    ArcFullSize : Integer = 0;
    BytesProcessed : Integer = 0;
    ExtractPath : string = '';
    R : TSFDSFileReader = nil;
    YesToAll : Boolean = False;
    NoToAll  : Boolean = False;
    ReplaceFileDlgResult : Integer = 0;
    FileToBeReplaced : string = '';
    SFXTitle : string = '';
    CMT : string = '';
    P : TProgressObject;

const
  ID_YES = 6;
  ID_NO = 7;
  ID_CANCEL = 2;  
  ID_YESTOALL = 18;
  ID_NOTOALL = 19;
{$R SFDS_SFXDLG.resdlg}

resourcestring
  SNotFound = 'The archive is either in unknown format or damaged.' + #13#10 + 'Please download this file again.';
  SError = 'Error';
  SSizeK = 'Size: %s KB';
  SSizeM = 'Size: %s MB';
  SOpAborted = 'Operation aborted by user.';
  SSelDir = 'Select destination directory.';
  SConfirmAbort = 'Abort the extraction process?';
  SConfirm = 'Confirm';

procedure DoExtract;

implementation

function DlgItem_GetText(hwndDlg : HWND; ID : Integer) : string;
var SC : array[0..2047] of Char;
begin
  result := '';
  if GetDlgItemText(hwndDlg, ID, SC, 2048) = 0 then Exit;
  result := StrPas(SC);
end;

procedure DlgItem_SetText(hwndDlg : HWND; ID : Integer; Text : string);
begin
  SetDlgItemText(hwndDlg, ID, PChar(Text));
end;

procedure ProcessMessages;

  function ProcessMessage : Boolean;
  var Msg: TMsg;
  begin
  result := False;
    if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
    begin
      result := True;
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  end;

begin
  while ProcessMessage do {loop};
end;

function IntToKBStr(const I: Int64): String;
var
  X: Extended;
begin
  X := I / 1024;
  if Frac(X) > 0 then
    X := Int(X) + 1;  { always round up }
  Result := Format('%.0n', [X]);
end;

function IntToMBStr(const I: Int64): String;
var
  X: Extended;
begin
  X := (I / 1048576) * 10;
  if Frac(X) > 0 then
    X := Int(X) + 1;  { always round up }
  X := X / 10;
  Result := Format('%.1n', [X]);
end;

procedure CenterWnd(const Wnd: HWND);
var
  R: TRect;
  SW, SH, W, H: Integer;
begin
  SW := GetSystemMetrics(SM_CXSCREEN);
  SH := GetSystemMetrics(SM_CYSCREEN);
  GetWindowRect(Wnd, R);
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  R.Left := (SW - W) div 2;
  if R.Left < 0 then R.Left := 0;
  R.Top := (SH - H) div 2;
  if R.Top < 0 then R.Top := 0;
  MoveWindow(Wnd, R.Left, R.Top, W, H, True);
end;

procedure UpdateProgressDlg(Progress: Integer);
begin
  SendDlgItemMessage(DlgHandle, 21, PBM_SETRANGE, 0, MAKELPARAM(0, 100));
  SendDlgItemMessage(DlgHandle, 21, PBM_SETPOS, Progress, 0);
end;

procedure DeleteUnwantedMenuItems(hwndDlg: HWND; DisableClose : Boolean = False);
var H : HMenu;
begin
 H := GetSystemMenu(hwndDlg, False);
 DeleteMenu(H, SC_SIZE, MF_BYCOMMAND);
 DeleteMenu(H, SC_RESTORE, MF_BYCOMMAND);
 DeleteMenu(H, SC_MINIMIZE, MF_BYCOMMAND);
 DeleteMenu(H, SC_MAXIMIZE, MF_BYCOMMAND);
 if DisableClose then EnableMenuItem(H, SC_CLOSE, MF_BYCOMMAND or MF_GRAYED);
end;

{ This function solves for y in the equation "x is y% of z". }
function SolveForY(X, Z: Longint): Longint;
begin
  if Z = 0 then Result := 0
  else Result := Longint(Trunc( (X * 100.0) / Z ));
end;

procedure DlgCancelExtract;
begin
  MessageBox(DlgHandle, PChar(SOpAborted), PChar(SError), MB_ICONSTOP);
  Halt;
end;

function ShouldReplaceFile(FileName : string) : Boolean;
var C : HWND;
begin
  result := False;
  if YesToAll then begin result := True; Exit; end;
  if NoToAll then begin result := False; Exit; end;
  FileToBeReplaced := FileName;
  ReplaceFileDlgResult := 0;
  C := DlgHandle;
  CreateDialog(DlgHandle, 'SFXOVERWRITEFILEDLG', True, @ResDlgProcReplaceFile);
  DlgHandle := C;
  case ReplaceFileDlgResult of
   ID_YES : result := True;
   ID_NO : result := False;
   ID_CANCEL :
     begin
       DlgCancelExtract;
     end;
   ID_YESTOALL :
     begin
       YesToAll := True;
       result := True;
     end;
   ID_NOTOALL :
     begin
       NoToAll := True;
       result := False;
     end;
  else
    result := False;
  end;
end;

procedure TProgressObject.ProgressEvent(Sender : TSFDSCustomSource; Progress, MaxProgress,
  ThisTime : Int64; var Cancel : Boolean; ArcName, StreamName : string);
begin
  BytesProcessed := BytesProcessed + ThisTime;
  UpdateProgressDlg(SolveForY(BytesProcessed, ArcFullSize));
  ProcessMessages;
  Cancel := DlgCancel;
end;

procedure DoExtract;
var StreamName, DiskName, S : string;
    I : Integer;
begin
  for I := 0 to R.FileEntrys.Count - 1 do
      begin
        StreamName := R.FileEntrys.Entrys[I]^.FileName;
        {$WARNINGS OFF}
        DiskName := IncludeTrailingBackslash(ExtractPath) + StreamName;
        {$WARNINGS ON}
        if R.FileEntrys.Entrys[I]^.FileSize <= 1048576
          then S := Format(SSizeK, [IntToKbStr(R.FileEntrys.Entrys[I]^.FileSize)])
          else S := Format(SSizeM, [IntToMbStr(R.FileEntrys.Entrys[I]^.FileSize)]);
        DlgItem_SetText(DlgHandle, 22, DiskName + #13#10 + S);
        if FileExists(DiskName) then
           if not ShouldReplaceFile(DiskName) then
              begin
              BytesProcessed := BytesProcessed + R.FileEntrys.Entrys[I]^.FileSize;
              UpdateProgressDlg(SolveForY(BytesProcessed, ArcFullSize));
              ProcessMessages;
              Continue;
              end;
        try
        if R.ExtractFile(StreamName, DiskName, False, P.ProgressEvent) = E_EABORTED then DlgCancelExtract;
        except
          on E : Exception do
            begin
              if MessageBox(0, PChar(E.Message), PChar(SError), MB_ICONSTOP or MB_ABORTRETRYIGNORE) = ID_IGNORE then Continue;
              Halt;
            end;
        end;
      end;
  EndDialog(DlgHandle, 0);
end;

function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  if (uMsg = BFFM_INITIALIZED) and (lpData <> 0) then
    SendMessage(Wnd, BFFM_SETSELECTION, Integer(True), lpdata);
  result := 0;
end;

function SelectDirectory(const Caption: string; const Root: WideString;
  var Directory: string): Boolean;
var
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
begin
  Result := False;
  if not DirectoryExists(Directory) then
    Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(DlgHandle, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      end;
      with BrowseInfo do
      begin
        hwndOwner := DlgHandle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS;
        if Directory <> '' then
        begin
          lpfn := SelectDirCB;
          lParam := Integer(PChar(Directory));
        end;
      end;
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
      end;
      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

function ResDlgProcMain(hwndDlg: HWND; uMsg, wParam,
  lParam: integer): integer;
var S : string;
begin
  result := 0;
  case uMsg of
    WM_INITDIALOG:
      begin
        result := 1;
        DlgHandle := hwndDlg;
        CenterWnd(hwndDlg);
        DeleteUnwantedMenuItems(hwndDlg);
        try
        S := DlgItem_GetText(hwndDlg, 21);
        if S <> ''
           then
           DlgItem_SetText(hwndDlg, 21, Format(S, [IntToMbStr(ArcFullSize)]));
        except
        end;
        DlgItem_SetText(hwndDlg, 22, ExtractPath);
        SetWindowText(hwndDlg, PChar(SFXTitle));
      end;
    WM_COMMAND:
      begin
        if LongRec(wParam).Hi = BN_CLICKED then
            begin
            case LongRec(wParam).Lo of
            1 : begin
                 EndDialog(hwndDlg, 0);
                 DlgResult := ID_OK;
                 ExtractPath := DlgItem_GetText(hwndDlg, 22);
                 result := 1;
                end;
            2 : begin
                 DlgCancel := True;
                 EndDialog(hwndDlg, 0);
                 DlgResult := ID_CANCEL;
                 result := 1;
                end;
            23: begin
                 result := 1;
                 S := DlgItem_GetText(hwndDlg, 22);
                 if not SelectDirectory(SSelDir, '', S) then exit;
                 DlgItem_SetText(hwndDlg, 22, S);
                end;
            end;
            end;
      end;
    WM_QUERYENDSESSION:
      begin
        { Prevent the user from logging off or shutting down during unzipping }
        SetWindowLong(hwndDlg, DWL_MSGRESULT, 0);  { return 'False' }
      end;
    WM_DESTROY:
      begin
        DlgHandle := 0;
      end;
  end;
end;

function ResDlgProcProgress(hwndDlg: HWND; uMsg, wParam,
  lParam: integer): integer;
begin
  result := 0;
  case uMsg of
    WM_INITDIALOG:
      begin
        result := 1;
        DlgHandle := hwndDlg;
        CenterWnd(hwndDlg);
        DeleteUnwantedMenuItems(hwndDlg);
      end;
    WM_COMMAND:
      begin
        if LongRec(wParam).Hi = BN_CLICKED then
            begin
            case LongRec(wParam).Lo of
            1 : begin
                 EndDialog(hwndDlg, 0);
                 DlgResult := ID_OK;
                 result := 1;
                end;
            2 : begin
                 if MessageBox(DlgHandle, PChar(SConfirmAbort), PChar(SConfirm), MB_ICONWARNING or MB_YESNO) = ID_NO then Exit;
                 DlgCancel := True;
                 EndDialog(hwndDlg, 0);
                 DlgResult := ID_CANCEL;
                 result := 1;
                end;
            end;
            end;
      end;
    WM_QUERYENDSESSION:
      begin
        { Prevent the user from logging off or shutting down during unzipping }
        SetWindowLong(hwndDlg, DWL_MSGRESULT, 0);  { return 'False' }
      end;
    WM_DESTROY:
      begin
        DlgHandle := 0;
      end;
  end;
end;

function ResDlgProcReplaceFile(hwndDlg : HWND; uMsg : integer; wParam,
  lParam : integer) : integer; stdcall;
begin
  result := 0;
  case uMsg of
    WM_INITDIALOG:
      begin
        result := 1;
        CenterWnd(hwndDlg);
        DeleteUnwantedMenuItems(hwndDlg, True);
        DlgItem_SetText(hwndDlg, 20, FileToBeReplaced);
      end;
    WM_COMMAND:
      begin
        if LongRec(wParam).Hi = BN_CLICKED then
            begin
            case LongRec(wParam).Lo of
            6 : begin
                 EndDialog(hwndDlg, 0);
                 ReplaceFileDlgResult := ID_YES;
                 result := 1;
                end;
            7 : begin
                 EndDialog(hwndDlg, 0);
                 ReplaceFileDlgResult := ID_NO;
                 result := 1;
                end;
            2 : begin
                 EndDialog(hwndDlg, 0);
                 ReplaceFileDlgResult := ID_CANCEL;
                 result := 1;
                end;
            18 : begin
                 EndDialog(hwndDlg, 0);
                 ReplaceFileDlgResult := ID_YESTOALL;
                 result := 1;
                end;
            19 : begin
                 EndDialog(hwndDlg, 0);
                 ReplaceFileDlgResult := ID_NOTOALL;
                 result := 1;
                end;
            end;
            end;
      end;
    WM_QUERYENDSESSION:
      begin
        // Prevent the user from logging off or shutting down during unzipping
        SetWindowLong(hwndDlg, DWL_MSGRESULT, 0);  // return 'False'
      end;
    WM_DESTROY:
      begin
        //
      end;
  end;
end;

function ResDlgProcInfo(hwndDlg : HWND; uMsg : integer; wParam,
  lParam : integer) : integer; stdcall;
begin
  result := 0;
  case uMsg of
    WM_INITDIALOG:
      begin
        result := 1;
        CenterWnd(hwndDlg);
        DeleteUnwantedMenuItems(hwndDlg, True);
        DlgItem_SetText(hwndDlg, 50, CMT);
      end;
    WM_COMMAND:
      begin
        if LongRec(wParam).Hi = BN_CLICKED then
            begin
            case LongRec(wParam).Lo of
            40: begin
                 EndDialog(hwndDlg, 0);
                 result := 1;
                end;
            end;
            end;
      end;
    WM_QUERYENDSESSION:
      begin
        // Prevent the user from logging off or shutting down during unzipping
        SetWindowLong(hwndDlg, DWL_MSGRESULT, 0);  // return 'False'
      end;
    WM_DESTROY:
      begin
        //
      end;
  end;
end;

procedure CreateDialog(ParentDlgHandle: HWND;
  ResourceName: string; Modal: Boolean; PResDlgProc : Pointer);
begin
  DlgCancel := False;
  DlgHandle := 0;
  DlgResult := 0;
  if Modal then
       DialogBoxParam(HInstance, PChar(ResourceName), ParentDlgHandle, PResDlgProc, 0)
     else
       CreateDialogParam(HInstance, PChar(ResourceName), ParentDlgHandle, PResDlgProc, 0);
end;

initialization
  InitCommonControls;
  P := TProgressObject.Create;

finalization
  P.Free;

end.
