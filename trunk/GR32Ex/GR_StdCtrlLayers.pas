unit GR_StdCtrlLayers;

interface

{$I Setting.inc}

uses
  {$ifdef Debug}
  DbugIntf,
  {$endif}
  Messages, {$IFDEF LINUX} WinUtils, {$ENDIF} Windows,
  SysUtils, Classes, Graphics, Controls
  , GR32
  , GR32_Layers
  , GR_ControlLayers
  , GR_Graphics
  , GR_GraphUtils
  , GR_Controls
  , GR_FilterEx
  ;

type
  TCustomLabelLayer = class(TBGCustomControlLayer)
  private
    FAlignment: TAlignment;
    FCaption: string;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetCaption(const Value: string);
  protected
    function InternalPaintBuffer(aBitmap32: TBitmap32): Boolean; override;
    property Alignment: TAlignment read FAlignment write SetAlignment default
      taCenter;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    property Caption: string read FCaption write SetCaption;
  end;
  
  TLabelLayer = class(TCustomLabelLayer)
  published
    property Background;
    property CaptionFont;
  end;
  

implementation

constructor TCustomLabelLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited Create(ALayerCollection);
  FAlignment := taCenter;
end;

function TCustomLabelLayer.InternalPaintBuffer(aBitmap32: TBitmap32): Boolean;
var
  vTextSize: TSize;
  Rect: TRect;
begin
  if Trim(Caption) <> '' then
  begin
    Rect := aBitmap32.ClipRect;
  {$ifdef Unicode_Supports}
    vTextSize := FCaptionFont.TextExtentW(aBitmap32, Caption);
  {$else}
    vTextSize := FCaptionFont.TextExtent(aBitmap32, Caption);
  {$endif}
    case Alignment of
      taLeftJustify:
      begin
        vTextSize.cx := Rect.Left;
        vTextSize.cy := Rect.Top;
      end;
      taCenter:
      begin
        //align the text to the panel center.
        vTextSize.cx := Rect.Left + (Rect.Right-Rect.Left) div 2 - vTextSize.cx div 2;
        if vTextSize.cx < Rect.Left then vTextSize.cx := Rect.Left;
        vTextSize.cy := Rect.Top + (Rect.Bottom-Rect.Top) div 2 - vTextSize.cy div 2;
        if vTextSize.cy < Rect.Top then vTextSize.cy := Rect.Top;
      end;
      taRightJustify:
      begin
        vTextSize.cx := Rect.Right  - vTextSize.cx;
        if vTextSize.cx < Rect.Left then vTextSize.cx := Rect.Left;
        vTextSize.cy := Rect.Top - vTextSize.cy;
        if vTextSize.cy < Rect.Top then vTextSize.cy := Rect.Top;
      end;
    end; //case
  {$ifdef Unicode_Supports}
    FCaptionFont.RenderTextW(aBitmap32, vTextSize.cx, vTextSize.cy, Caption);
  {$else}
    FCaptionFont.RenderText(aBitmap32, vTextSize.cx, vTextSize.cy, Caption);
  {$endif}
  end;
  Result := inherited InternalPaintBuffer(aBitmap32);
end;

procedure TCustomLabelLayer.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    //if not (csLoading in ComponentState) then
    begin
      //FSelfBuffer.Delete;
      InvalidateBuffer;
    end;
  end;
end;

procedure TCustomLabelLayer.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
  FCaption := Value;
  InvalidateBuffer;
  end;
end;


end.
