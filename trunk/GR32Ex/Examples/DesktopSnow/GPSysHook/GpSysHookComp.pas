(*:Component wrappers for the GpSysHook library
   @author Primoz Gabrijelcic
   @desc <pre>
   (c) 2001 Primoz Gabrijelcic
   Free for personal and commercial use. No rights reserved.

   Author            : Primoz Gabrijelcic
   Creation date     : 2001-10-02
   Last modification : 2001-11-08
   Version           : 1.02
</pre>*)(*
   History:
     1.02: 2001-11-08
       - Added filtering support to the TGpKeyboardHook component.
     1.01: 2001-10-10
       - Added support for the CBT hook with the TGpCBTHook component.
     1.0: 2001-10-06
       - Released.
     0.1: 2001-10-02
       - Created.
*)

unit GpSysHookComp;

{$I jedi.inc}

interface

uses
  SysUtils, 
  Windows,
  Messages,
  Classes,
  Controls,
  Forms,
  GpSysHookCommon;

type
  {:Unfiltered event signature. This event reports all received hook messages
    before they are processed. Application can prevent further processing of
    these messages by setting Handled to True. This only effects component
    that triggered the unfiltered event - hooked message will not be filtered
    and will be delivered to the target process (most probably it already was
    delivered when process receives this event).
  }
  TGpSysHookUnfilteredEvent = procedure(Sender: TObject; Code, wParam,
    lParam: longint; var Handled: boolean) of object;

  {:Base parent for all systemwide hook component wrappers.
  }
  TGpSysHook = class(TComponent)
  private
    FOnMessage: TMessageEvent;
    FHookedWnd     : HWND;
    FActive      : boolean;
    FHookDLLName : string;
    FIsFiltering : boolean;
    FListenerWnd : THandle;
    FOnUnfiltered: TGpSysHookUnfilteredEvent;
    procedure SetFiltering(const Value: boolean);
    property  AllowFiltering: boolean read FIsFiltering write SetFiltering;
  protected
    class function HookType: TGpHookType; virtual; abstract;
    function  MyName: string; virtual;
    procedure WndProc(var Message: TMessage);virtual;
    procedure ProcessMessage(var Message: TMessage); virtual; abstract;
    procedure SetHookDLLName(const Value: string); virtual;
    procedure DoUnfiltered(code, wParam, lParam: longint;
      var handled: boolean); virtual;
  public
    destructor  Destroy; override;
    function  Start: string;
    procedure Stop;
    property  Active: boolean read FActive;
    property  Handle: THandle read FListenerWnd;
  published
    property  HookDLL: string read FHookDLLName write SetHookDLLName;
    //added by riceball
    //0 means catch the all windows message. the default is 0
    property HookedWnd: HWND read FHookedWnd write FHookedWnd;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property  OnUnfiltered: TGpSysHookUnfilteredEvent
      read FOnUnfiltered write FOnUnfiltered;
  end; { TGpSysHook }

  {:Shell hook Accessibility event signature. 
  }
  TGpShellHookAccesibilityEvent = procedure(Sender: TObject;
    changedFeature: longint) of object;

  {:Shell hook notification event signature.
  }
  TGpShellNotifyEvent = procedure(Sender: TObject) of object;

  {:Shell hook notification event with window handle signature.
  }
  TGpShellNotifyWindowEvent = procedure(Sender: TObject;
    sourceWindow: THandle) of object;

  {:Shell hook Title redraw event signature.
  }
  TGpShellTitleRedrawEvent = procedure(Sender: TObject;
    sourceWindow: THandle; isFlashing: boolean) of object;

  {:WH_SHELL hook wrapper.
  }
  TGpShellHook = class(TGpSysHook)
  private
    FNotifyOwnEvents            : boolean;
    FOnAccessibilityStateChanged: TGpShellHookAccesibilityEvent;
    FOnActivateShellWindow      : TGpShellNotifyEvent;
    FOnLanguageChange           : TGpShellNotifyWindowEvent;
    FOnTaskManager              : TGpShellNotifyEvent;
    FOnWindowActivated          : TGpShellNotifyWindowEvent;
    FOnWindowCreated            : TGpShellNotifyWindowEvent;
    FOnWindowDestroyed          : TGpShellNotifyWindowEvent;
    FOnWindowTitleRedraw        : TGpShellTitleRedrawEvent;
  protected
    class function HookType: TGpHookType; override;
    procedure ProcessMessage(var Message: TMessage); override;
    procedure DoAccessibilityStateChanged(changedFeature: longint); virtual;
    procedure DoActivateShellWindow; virtual;
    procedure DoLanguageChange(sourceWindow: THandle); virtual;
    procedure DoWindowTitleRedraw(sourceWindow: THandle; isFlashing: boolean); virtual;
    procedure DoTaskManager; virtual;
    procedure DoWindowActivated(sourceWindow: THandle); virtual;
    procedure DoWindowCreated(sourceWindow: THandle); virtual;
    procedure DoWindowDestroyed(sourceWindow: THandle); virtual;
  published
    property  NotifyOwnEvents: boolean read FNotifyOwnEvents write FNotifyOwnEvents;
    property  OnAccessibilityStateChanged: TGpShellHookAccesibilityEvent
      read FOnAccessibilityStateChanged write FOnAccessibilityStateChanged;
    property  OnActivateShellWindow: TGpShellNotifyEvent
      read FOnActivateShellWindow write FOnActivateShellWindow;
    property  OnLanguageChange: TGpShellNotifyWindowEvent
      read FOnLanguageChange write FOnLanguageChange;
    property  OnTaskManager: TGpShellNotifyEvent
      read FOnTaskManager write FOnTaskManager;
    property  OnWindowActivated: TGpShellNotifyWindowEvent
      read FOnWindowActivated write FOnWindowActivated;
    property  OnWindowCreated: TGpShellNotifyWindowEvent
      read FOnWindowCreated write FOnWindowCreated;
    property  OnWindowDestroyed: TGpShellNotifyWindowEvent
      read FOnWindowDestroyed write FOnWindowDestroyed;
    property  OnWindowTitleRedraw: TGpShellTitleRedrawEvent
      read FOnWindowTitleRedraw write FOnWindowTitleRedraw;
  end; { TGpShellHook }

  {:Keyboard hook notification event signature.
  }
  TGpKeyboardNotifyEvent = procedure(Sender: TObject; VirtualKeyCode: longint;
    RepeatCount: word; ScanCode: byte; isExtendedKey, altIsDown,
    keyWasDownBefore, keyIsBeingPressed: boolean;
    var filterEvent: boolean) of object;

  {:WH_KEYBOARD hook wrapper.
  }
  TGpKeyboardHook = class(TGpSysHook)
  private
    FOnKeyAction: TGpKeyboardNotifyEvent;
  protected
    class function HookType: TGpHookType; override;
    procedure ProcessMessage(var Message: TMessage); override;
    procedure DoKeyAction(VirtualKeyCode: longint; RepeatCount: word;
      ScanCode: byte; isExtendedKey, altIsDown, keyWasDownBefore,
      keyIsBeingPressed: boolean; var filterEvent: boolean); virtual;
  published
    property  AllowFiltering;
    property  OnKeyAction: TGpKeyboardNotifyEvent
      read FOnKeyAction write FOnKeyAction;
  end; { TGpKeyboardHook }

  {:All possible type of mouse button movement.
  }
  TGpMouseButtonMovement = (mbmDown, mbmUp, mbmDouble);

  {:Mouse hook click notification signature.
  }
  TGpMouseClickNotifyEvent = procedure(Sender: TObject; sourceWindow: THandle;
    x, y: integer; button: TMouseButton;
    movement: TGpMouseButtonMovement) of object;

  {:Mouse hook movement notification signature.
    the mouse hook can not get the shift info.
  }
  TGpMouseMoveNotifyEvent = procedure(Sender: TObject; sourceWindow: THandle;
    x, y: integer) of object;

  {:WH_MOUSE hook wrapper.
  }
  TGpMouseHook = class(TGpSysHook)
  private
    FOnMouseClick: TGpMouseClickNotifyEvent;
    FOnMouseMove: TGpMouseMoveNotifyEvent;
  protected
    class function HookType: TGpHookType; override;
    procedure ProcessMessage(var Message: TMessage); override;
    procedure DoMouseClick(sourceWindow: THandle; x, y: integer;
      button: TMouseButton; movement: TGpMouseButtonMovement); virtual;
    procedure DoMouseMove(sourceWindow: THandle; x, y: integer); virtual;
  published
    property  OnMouseClick: TGpMouseClickNotifyEvent
      read FOnMouseClick write FOnMouseClick;
    property  OnMouseMove: TGpMouseMoveNotifyEvent
      read FOnMouseMove write FOnMouseMove;
  end; { TGpMouseHook }

  {:CBT hook Activate event signature. 
  }
  TGpCBTActivateEvent = procedure(Sender: TObject; newWindow,
    activeWindow: THandle) of object;

  {:CBT hook ClickSkipped event signature.
  }
  TGpCBTClickSkippedEvent = procedure(Sender: TObject; targetWindow: THandle;
    mouseMessage: longint) of object;

  {:CBT hook notification event with window handle signature.
  }
  TGpCBTNotifyWindowEvent  = procedure(Sender: TObject; windowHandle: THandle)
    of object;

  {:CBT hook KeySkipped event signature.
  }
  TGpCBTKeySkippedEvent = procedure(Sender: TObject; VirtualKeyCode: longint;
    RepeatCount: word; ScanCode: byte; isExtendedKey, altIsDown,
    keyWasDownBefore, keyIsBeingPressed: boolean) of object;

  {:CBT Hook MinMax event signature.
  }
  TGpCBTMinMaxEvent = procedure(Sender: TObject; targetWindow: THandle;
    showValue: word) of object;

  {:CBT hook notification event signature.
  }
  TGpCBTNotifyEvent = procedure(Sender: TObject) of object;

  {:CBT hook SetFocus event signature.
  }
  TGpCBTSetFocusEvent = procedure(Sender: TObject; newWindow,
    oldWindow: THandle) of object;

  {:CBT hook SysCommand event signature.
  }
  TGpCBTSysCommandEvent = procedure(Sender: TObject; sysCommand: longint)
    of object;

  {:WH_CBT hook wrapper.
  }
  TGpCBTHook = class(TGpSysHook)
  private
    FOnActivate    : TGpCBTActivateEvent;
    FOnClickSkipped: TGpCBTClickSkippedEvent;
    FOnCreateWnd   : TGpCBTNotifyWindowEvent;
    FOnDestroyWnd  : TGpCBTNotifyWindowEvent;
    FOnKeySkipped  : TGpCBTKeySkippedEvent;
    FOnMinMax      : TGpCBTMinMaxEvent;
    FOnMoveSize    : TGpCBTNotifyWindowEvent;
    FOnQueueSync   : TGpCBTNotifyEvent;
    FOnSetFocus    : TGpCBTSetFocusEvent;
    FOnSysCommand  : TGpCBTSysCommandEvent;
  protected
    class function HookType: TGpHookType; override;
    procedure ProcessMessage(var Message: TMessage); override;
    procedure DoActivate(newWindow, activeWindow: THandle); virtual;
    procedure DoClickSkipped(targetWindow: THandle; mouseMessage: longint); virtual;
    procedure DoCreateWnd(newWindow: THandle); virtual;
    procedure DoDestroyWnd(goneWindow: THandle); virtual;
    procedure DoKeySkipped(VirtualKeyCode: longint; RepeatCount: word;
      ScanCode: byte; isExtendedKey, altIsDown, keyWasDownBefore,
      keyIsBeingPressed: boolean); virtual;
    procedure DoMinMax(targetWindow: THandle; showValue: word); virtual;
    procedure DoMoveSize(targetWindow: THandle); virtual;
    procedure DoQueueSync; virtual;
    procedure DoSetFocus(newWindow, oldWindow: THandle); virtual;
    procedure DoSysCommand(sysCommand: longint); virtual;
  published
    property OnActivate: TGpCBTActivateEvent
      read FOnActivate write FOnActivate;
    property OnClickSkipped: TGpCBTClickSkippedEvent
      read FOnClickSkipped write FOnClickSkipped;
    property OnCreateWnd: TGpCBTNotifyWindowEvent
      read FOnCreateWnd write FOnCreateWnd;
    property OnDestroyWnd: TGpCBTNotifyWindowEvent
      read FOnDestroyWnd write FOnDestroyWnd;
    property OnKeySkipped: TGpCBTKeySkippedEvent
      read FOnKeySkipped write FOnKeySkipped;
    property OnMinMax: TGpCBTMinMaxEvent
      read FOnMinMax write FOnMinMax;
    property OnMoveSize: TGpCBTNotifyWindowEvent
      read FOnMoveSize write FOnMoveSize;
    property OnQueueSync: TGpCBTNotifyEvent
      read FOnQueueSync write FOnQueueSync;
    property OnSetFocus: TGpCBTSetFocusEvent
      read FOnSetFocus write FOnSetFocus;
    property OnSysCommand: TGpCBTSysCommandEvent
      read FOnSysCommand write FOnSysCommand;
  end; { TGpCBTHook }

  procedure Register;

implementation

uses
  GpSysHookLoader;

resourcestring
  sCannotActivateHookBecauseDLLnameIs  = 'Cannot activate hook because DLL name is not set.';
  sDLLnameCannotBeSetWhileHookIsActive = 'DLL name cannot be set while hook is active!';
  sFailedToCreateListeningWindow       = 'Failed to create listening window.';

procedure Register;
begin
  RegisterComponents('Gp',[TGpShellHook,TGpKeyboardHook,TGpMouseHook,TGpCBTHook]);
end; { Register }

{ TGpSysHook }

{:Systemwide hook wrapper destructor. Stops the listeners.
}
destructor TGpSysHook.Destroy;
begin
  Stop;
  inherited;
end; { TGpSysHook.Destroy }

{:Forwarder for the OnUnfiltered event.
}
procedure TGpSysHook.DoUnfiltered(code, wParam, lParam: longint;
  var handled: boolean);
begin
  if assigned(FOnUnfiltered) then
    FOnUnfiltered(Self,code,wParam,lParam,handled);
end; { TGpSysHook.DoUnfiltered }

{:Main look for the internal window receiving messages from the hook DLL.
  Forwards messages to the attached event handlers.
}
procedure TGpSysHook.WndProc(var Message: TMessage);
var
  Handled: Boolean;
  aMsg: TMsg;
begin
  Handled := False;
  if Assigned(FOnMessage) then
  begin
    if Message.Msg < WM_USER then
      aMsg.hwnd := FListenerWnd
    else
      aMsg.hwnd := FHookedWnd;
    aMsg.message := Message.Msg;
    aMsg.wParam := Message.wParam;
    aMsg.lParam := Message.lParam;
    aMsg.time := GetTickCount;
    FOnMessage(aMsg, Handled);
  end;
  if not Handled then
  begin
    if Message.Msg < WM_USER then
      with Message do
        Result := DefWindowProc(FListenerWnd, Msg, wParam, lParam)
    else
      ProcessMessage(Message);
  end;
end; { TGpSysHook.WndProc }

{:Returns name of the instance of this class to be used in the error reporting.
}
function TGpSysHook.MyName: string;
begin
  if Name <> '' then
    Result := Name
  else
    Result := ClassName;
end; { TGpSysHook.MyName }

{:Set the AllowFiltering property. Raises exception if hook is started.
  @since   2001-11-08
}
procedure TGpSysHook.SetFiltering(const Value: boolean);
begin
  if Active then
    raise Exception.Create('Cannot set AllowFiltering propert while hook is active!');
  FIsFiltering := Value;
end; { TGpSysHook.SetFiltering }

{:Set systemwide hook DLL name but only if component is inactive.
}
procedure TGpSysHook.SetHookDLLName(const Value: string);
begin
  if FActive then
    MessageBox(0, PChar(sDLLnameCannotBeSetWhileHookIsActive),
      PChar(MyName), MB_OK + MB_ICONERROR)
  else
    FHookDLLName := Value;
end; { TGpSysHook.SetHookDLLName }

{:Activate the hook.
}
function TGpSysHook.Start: string;
var
  hookRes: integer;
begin
  if not FActive then begin
    if FHookDLLName = '' then
      Result := sCannotActivateHookBecauseDLLnameIs
    else begin
      hookRes := LoadHookDLL(FHookDllName);
      if hookRes <> 0 then
        Result := HookErrorToStr(hookRes)
      else begin
        {$IFDEF COMPILER6_UP}
          {$IFDEF MSWINDOWS}   
            FListenerWnd := Classes.AllocateHWnd(WndProc);
          {$ENDIF}
          {$IFDEF LINUX}   
            FListenerWnd := WinUtils.AllocateHWnd(WndProc);
          {$ENDIF}
        {$ELSE}
        FListenerWnd := AllocateHwnd(WndProc);
        {$ENDIF}
        if FListenerWnd = 0 then
          Result := sFailedToCreateListeningWindow
        else begin
          hookRes := fnAttachReceiver(HookType,FListenerWnd,FIsFiltering, FHookedWnd);
          if hookRes <> 0 then begin
            DeallocateHWnd(FListenerWnd);
            FListenerWnd := 0;
            Result := HookErrorToStr(hookRes);
          end
          else begin
            Result := '';
            FActive := true;
          end;
        end;
      end;
    end;
  end;
end; { TGpSysHook.Start }

{:Deactivate the hook.
}
procedure TGpSysHook.Stop;
begin
  if FActive then begin
    if FListenerWnd <> 0 then begin
      {$IFDEF COMPILER6_UP}
        {$IFDEF MSWINDOWS}   
          Classes.DeallocateHWnd(FListenerWnd);
        {$ENDIF}
        {$IFDEF LINUX}
          WinUtils.DeallocateHWnd(FListenerWnd);
        {$ENDIF}   
      {$ELSE}
        DeallocateHWnd(FListenerWnd);
      {$ENDIF}
      FListenerWnd := 0;
    end;
    UnloadHookDLL;
    FActive := false;
  end;
end; { TGpSysHook.Stop }

{ TGpShellHook }

{:Forwarder for the OnAccessibilityStateChanged event.
}
procedure TGpShellHook.DoAccessibilityStateChanged(changedFeature: longint);
begin
  if assigned(FOnAccessibilityStateChanged) then
    FOnAccessibilityStateChanged(self, changedFeature);
end; { TGpShellHook.DoAccessibilityStateChanged }

{:Forwarder for the OnActivateShellWindow event.
}
procedure TGpShellHook.DoActivateShellWindow;
begin
  if assigned(FOnActivateShellWindow) then
    FOnActivateShellWindow(self);
end; { TGpShellHook.DoActivateShellWindow }

{:Forwarder for the OnLanguageChange event.
}
procedure TGpShellHook.DoLanguageChange(sourceWindow: THandle);
begin
  if assigned(FOnLanguageChange) then
    FOnLanguageChange(self,sourceWindow)
end; { TGpShellHook.DoLanguageChange }

{:Forwarder for the OnTaskManager event.
}
procedure TGpShellHook.DoTaskManager;
begin
  if assigned(FOnTaskManager) then
    FOnTaskManager(self);
end; { TGpShellHook.DoTaskManager }

{:Forwarder for the OnWindowActivated event.
}
procedure TGpShellHook.DoWindowActivated(sourceWindow: THandle);
begin
  if assigned(FOnWindowActivated) then
    FOnWindowActivated(self,sourceWindow);
end; { TGpShellHook.DoWindowActivated }

{:Forwarder for the OnWindowCreated event.
}
procedure TGpShellHook.DoWindowCreated(sourceWindow: THandle);
begin
  if assigned(FOnWindowCreated) then
    FOnWindowCreated(self,sourceWindow);
end; { TGpShellHook.DoWindowCreated }

{:Forwarder for the OnWindowDestroyed event.
}
procedure TGpShellHook.DoWindowDestroyed(sourceWindow: THandle);
begin
  if assigned(FOnWindowDestroyed) then
    FOnWindowDestroyed(self,sourceWindow);
end; { TGpShellHook.DoWindowDestroyed }

{:Forwarder for the OnWindowTitleRedraw event.
}
procedure TGpShellHook.DoWindowTitleRedraw(sourceWindow: THandle;
  isFlashing: boolean);
begin
  if assigned(FOnWindowTitleRedraw) then
    FOnWindowTitleRedraw(self,sourceWindow,isFlashing);
end; { TGpShellHook.DoWindowTitleRedraw }

{:Returns type of this hook.
}
class function TGpShellHook.HookType: TGpHookType;
begin
  Result := htShell;
end; { TGpShellHook.HookType }

{:Dispatch hook messages.
}
procedure TGpShellHook.ProcessMessage(var Message: TMessage);
var
  code   : DWORD;
  handled: boolean;
begin
  if Message.Msg >= WM_USER then begin
    Message.Result := 0;
    code := Message.msg-WM_USER;
    handled := false;
    DoUnfiltered(code,Message.wParam,Message.lParam,handled);
    if not handled then begin
      case code of
        HSHELL_ACCESSIBILITYSTATE :
          DoAccessibilityStateChanged(Message.wParam);
        HSHELL_ACTIVATESHELLWINDOW:
          DoActivateShellWindow;
        HSHELL_LANGUAGE:
          if (HWND(Message.wParam) <> Application.Handle) or NotifyOwnEvents then
            DoLanguageChange(Message.wParam);
        HSHELL_REDRAW:
          if (HWND(Message.wParam) <> Application.Handle) or NotifyOwnEvents then
            DoWindowTitleRedraw(Message.wParam,Message.lParam <> 0);
        HSHELL_TASKMAN:
          DoTaskManager;
        HSHELL_WINDOWACTIVATED:
          if (HWND(Message.wParam) <> Application.Handle) or NotifyOwnEvents then
            DoWindowActivated(Message.wParam);
        HSHELL_WINDOWCREATED:
          if (HWND(Message.wParam) <> Application.Handle) or NotifyOwnEvents then
            DoWindowCreated(Message.wParam);
        HSHELL_WINDOWDESTROYED:
          if (HWND(Message.wParam) <> Application.Handle) or NotifyOwnEvents then
            DoWindowDestroyed(Message.wParam);
      end; //case
    end;
  end;
end; { TGpShellHook.ProcessMessage }

{ TGpKeyboardHook }

{:Forwarder for the OnKeyAction event.
}
procedure TGpKeyboardHook.DoKeyAction(VirtualKeyCode: longint;
  RepeatCount: word; ScanCode: byte; isExtendedKey, altIsDown, keyWasDownBefore,
  keyIsBeingPressed: boolean; var filterEvent: boolean);
begin
  if assigned(FOnKeyAction) then
    FOnKeyAction(self,VirtualKeyCode,RepeatCount,ScanCode,isExtendedKey,
      altIsDown,keyWasDownBefore,keyIsBeingPressed,filterEvent);
end; { TGpKeyboardHook.DoKeyAction }

{:Returns type of this hook.
}
class function TGpKeyboardHook.HookType: TGpHookType;
begin
  Result := htKeyboard;
end; { TGpKeyboardHook.HookType }

{:Dispatch hook messages.
}
procedure TGpKeyboardHook.ProcessMessage(var Message: TMessage);
var
  code       : DWORD;
  filter     : boolean;
  flags      : byte;
  handled    : boolean;
  repeatCount: word;
  scanCode   : byte;
begin
  if Message.Msg >= WM_USER then begin
    Message.Result := 0;
    code := Message.msg-WM_USER;
    handled := false;
    DoUnfiltered(code,Message.wParam,Message.lParam,handled);
    if not handled then begin
      if code = HC_ACTION then begin
        repeatCount := Message.lParam AND $FFFF;
        scanCode    := (Message.lParam SHR 16) AND $FF;
        flags       := Message.lParam SHR 24;
        filter := false;
        DoKeyAction(Message.wParam, repeatCount, scanCode,
          (flags AND 1) <> 0, (flags AND 32) <> 0, (flags AND 64) <> 0,
          (flags AND 128) = 0, filter);
        if Filter then
          Message.Result := 1;
      end;
    end;
  end;
end; { TGpKeyboardHook.ProcessMessage }

{ TGpMouseHook }

{:Forwarder for the OnMouseClick event.
}
procedure TGpMouseHook.DoMouseClick(sourceWindow: THandle; x, y: integer;
  button: TMouseButton; movement: TGpMouseButtonMovement);
begin
  if assigned(FOnMouseClick) then
    FOnMouseClick(self, sourceWindow, x, y, button, movement);
end; { TGpMouseHook.DoMouseClick }

{:Forwarder for the OnMouseMove event.
}
procedure TGpMouseHook.DoMouseMove(sourceWindow: THandle; x, y: integer);
begin
  if assigned(FOnMouseMove) then
    FOnMouseMove(self, sourceWindow, x, y);
end; { TGpMouseHook.DoMouseMove }

{:Returns type of this hook.
}
class function TGpMouseHook.HookType: TGpHookType;
begin
  Result := htMouse;
end; { TGpMouseHook.HookType }

{:Dispatch hook messages.
}
procedure TGpMouseHook.ProcessMessage(var Message: TMessage);
var
  button  : TMouseButton;
  code    : DWORD;
  handled : boolean;
  movement: TGpMouseButtonMovement;
  x       : integer;
  y       : integer;
begin
  if Message.Msg >= WM_USER then begin
    Message.Result := 0;
    //the code is WM_MOUSEFIRST..WM_MOUSELAST: // 所有的滑鼠相关讯息
    code := Message.msg-WM_USER;
    handled := false;
    DoUnfiltered(code,Message.wParam,Message.lParam,handled);
    if not handled then begin
      x := Message.wParam AND $FFFF;
      y := (Message.wParam SHR 16) AND $FFFF;
      if (code AND $F) = 0 then
        DoMouseMove(Message.lParam,x,y)
      else begin
        case ((code AND $F)-1) div 3 of
          0: button := mbLeft;
          1: button := mbRight;
          2: button := mbMiddle;
          else button := mbLeft; //fallback
        end; //case
        case ((code AND $F)-1) mod 3 of
          0: movement := mbmDown;
          1: movement := mbmUp;
          2: movement := mbmDouble;
          else {cannot happen} movement := mbmDown; {to keep Delphi happy}
        end; //case
        DoMouseClick(Message.lParam,x,y,button,movement);
      end;
    end;
  end;
end; { TGpMouseHook.ProcessMessage }

{ TGpCBTHook }

procedure TGpCBTHook.DoActivate(newWindow, activeWindow: THandle);
begin
  if assigned(FOnActivate) then
    FOnActivate(self, newWindow, activeWindow);
end; { TGpCBTHook.DoActivate }

procedure TGpCBTHook.DoClickSkipped(targetWindow: THandle;
  mouseMessage: longint);
begin
  if assigned(FOnClickSkipped) then
    FOnClickSkipped(self, targetWindow, mouseMessage);
end; { TGpCBTHook.DoClickSkipped }

procedure TGpCBTHook.DoCreateWnd(newWindow: THandle);
begin
  if assigned(FOnCreateWnd) then
    FOnCreateWnd(self, newWindow);
end; { TGpCBTHook.DoCreateWnd }

procedure TGpCBTHook.DoDestroyWnd(goneWindow: THandle);
begin
  if assigned(FOnDestroyWnd) then
    FOnDestroyWnd(self, goneWindow);
end; { TGpCBTHook.DoDestroyWnd }

procedure TGpCBTHook.DoKeySkipped(VirtualKeyCode: longint; RepeatCount: word;
  ScanCode: byte; isExtendedKey, altIsDown, keyWasDownBefore,
  keyIsBeingPressed: boolean);
begin
  if assigned(FOnKeySkipped) then
    FOnKeySkipped(self, VirtualKeyCode, RepeatCount, ScanCode, isExtendedKey,
      altIsDown, keyWasDownBefore, keyIsBeingPressed);
end; { TGpCBTHook.DoKeySkipped }

procedure TGpCBTHook.DoMinMax(targetWindow: THandle; showValue: word);
begin
  if assigned(FOnMinMax) then
    FOnMinMax(self, targetWindow, showValue);
end; { TGpCBTHook.DoMinMax }

procedure TGpCBTHook.DoMoveSize(targetWindow: THandle);
begin
  if assigned(FOnMoveSize) then
    FOnMoveSize(self, targetWindow);
end; { TGpCBTHook.DoMoveSize }                                 

procedure TGpCBTHook.DoQueueSync;
begin
  if assigned(FOnQueueSync) then
    FOnQueueSync(self);
end; { TGpCBTHook.DoQueueSync }

procedure TGpCBTHook.DoSetFocus(newWindow, oldWindow: THandle);
begin
  if assigned(FOnSetFocus) then
    FOnSetFocus(self, newWindow, oldWindow);
end; { TGpCBTHook.DoSetFocus }

procedure TGpCBTHook.DoSysCommand(sysCommand: longint);
begin
  if assigned(FOnSysCommand) then
    FOnSysCommand(self, sysCommand);
end; { TGpCBTHook.DoSysCommand }

{:Returns type of this hook.
}
class function TGpCBTHook.HookType: TGpHookType;
begin
  Result := htCBT;
end; { TGpCBTHook.HookType }

{:Dispatch hook messages.
}
procedure TGpCBTHook.ProcessMessage(var Message: TMessage);
var
  code       : DWORD;
  flags      : byte;
  handled    : boolean;
  repeatCount: word;
  scanCode   : byte;
begin
  if Message.Msg >= WM_USER then begin
    Message.Result := 0;
    code := Message.msg-WM_USER;
    handled := false;
    DoUnfiltered(code,Message.wParam,Message.lParam,handled);
    if not handled then begin
      case code of
        HCBT_ACTIVATE:
          DoActivate(Message.wParam,Message.lParam);
        HCBT_CLICKSKIPPED:
          DoClickSkipped(Message.lParam,Message.wParam);
        HCBT_CREATEWND:
          DoCreateWnd(Message.wParam);
        HCBT_DESTROYWND:
          DoDestroyWnd(Message.wParam);
        HCBT_KEYSKIPPED:
          begin
            repeatCount := Message.lParam AND $FFFF;
            scanCode    := (Message.lParam SHR 16) AND $FF;
            flags       := Message.lParam SHR 24;
            DoKeySkipped(Message.wParam, repeatCount, scanCode,
              (flags AND 1) <> 0, (flags AND 32) <> 0, (flags AND 64) <> 0,
              (flags AND 128) = 0);
          end; // HCBT_KEYSKIPPED
        HCBT_MINMAX:
          DoMinMax(Message.wParam,Message.LParamLo);
        HCBT_MOVESIZE:
          DoMoveSize(Message.wParam);
        HCBT_QS:
          DoQueueSync;
        HCBT_SETFOCUS:
          DoSetFocus(Message.wParam,Message.lParam);
        HCBT_SYSCOMMAND:
          DoSysCommand(Message.wParam);
      end; //case
    end;
  end;
end; { TGpCBTHook.ProcessMessage }

end.
