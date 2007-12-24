(*:DLL implementing system-wide keyboard, mouse, and shell hooks. Supports
   multiple listeners, automatic unhooking on process detach, and only installs
   the hooks that are needed. Each listener can only listen to one hook because
   hook code is sent as a message ID. All internal IDs are generated from the
   module name so you only have to rename the DLL to make it peacefully coexist
   with another GpSysHookDLL DLL.

   @author Primoz Gabrijelcic
   @desc <pre>
   (c) 2001 Primoz Gabrijelcic
   Free for personal and commercial use. No rights reserved.

   Author            : Primoz Gabrijelcic
   Creation date     : 2001-09-25
   Last modification : 2001-10-06
   Version           : 1.01
</pre>*)(*
   History:
     1.01: 2001-10-06
       - Modified to work with more than on listener.
       - Internals simplified and modularized.
     1.0: 2001-09-25
       - Created and released.
*)

library GpSysHookDLL;

uses
  Windows,
  GpSysHook in 'GpSysHook.pas',
  GpSysHookCommon in 'GpSysHookCommon.pas';

procedure DLLEntryPoint(reason: integer);
begin  if reason = DLL_PROCESS_DETACH then    GpSysHook.ProcessDetached;end; { DLLEntryPoint }exports  AttachReceiver,
  DetachReceiver,
  LastError;

begin
  DisableThreadLibraryCalls(HInstance);
  GpSysHook.ProcessAttached;
  DLLProc := @DLLEntryPoint;
end.

