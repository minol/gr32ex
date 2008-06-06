unit GR_LayerRegisters;

{$I Setting.inc}

interface

uses
 {$IFDEF Debug}
 DbugIntf,
 {$ENDIF}
  SysUtils, Classes
  , GR_Layers
  , GR_StdLayers
  ;

implementation


initialization
  RegisterLayer(TGRTextLayer);
  RegisterLayer(TGRBitmapLayer);
end.
