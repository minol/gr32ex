The GR32 扩展开发控件包. Ver 0.1

Writen by Riceball LEE(riceball@users.sourceforge.net)

功能:
 * 通用精灵动画引擎
 * 通用动画特效引擎
   * 水纹特效
   * 通用粒子特效动画引擎
     * 星空粒子特效
     * 下雪粒子特效
 * 支持半透明 GRControl 界面控件包:
   * 控件半透明化:
   * 界面控件定制图形化边框支持: 支持三态：Hot, Down, Normal
   * 半透明的复合背景支持
       * 墙纸 Wallpaper: 最先绘制（如果有的化）
       * 渐变 Gradient:  其次绘制（如果有的化）
       * 贴图 Texture:   最后绘制（如果有的化）
 * 高级字体支持
   * 贴图字体
   * 轮廓字体
   * 字体阴影
   * 抗锯齿
   * 半透明

在GR_AniGEffetcts单元中的 TGRAnimationEffects 类可以通过钩挂控件的方式，将动画绘制到 TCustomPaintBox32(TImage32), TCustomControl, TGraphicControl 和 TCustomForm，不过在标准GDI的控件上绘制的时候性能稍差。

在Image32上的GDI绘制粒子特效动画的例子在 Examples\particle\目录下，该例子同时渲染了水纹特效，下雪特效和星光特效。

The GR32 Extension 可视控件核心框架功能:
 * 所有的GR可视控件都是从TGRCustomControl或TGRGraphicControl)派生的。 TGRCustomControl是有Window 句柄的WinControl。
 * 控件半透明化:
 * 半透明的复合背景支持
 * 高级字体支持
   * 贴图字体
   * 轮廓字体
   * 字体阴影
   * 抗锯齿
   * 半透明
 * 许多有用的辅助类：
   * TCustomGraphicProperty(GR32_GraphUtils): you can add the perfect properties to your components too.
    * GR32_Graphics Properties:
     * TGradient 渐变类: (original writen by Kambiz R. Khojasteh(kambiz@delphiarea.com))
       It is an extremely fast gradient fill control with 
       a large set of styles. As built-in, TGradient can 
       draw gradient in 23 styles and provides an easily 
       method to define custom styles. In addition, this 
       control can shift and/or rotate the gradient colors, 
       which could be used for creating animated gradients.
       * AlphaBegin 起始的半透明度(new by riceball)
       * AlphaEnd   结束的半透明度(new by riceball)
       * AlphaChannel: Boolean. 是否将渐变作为Alpha通道使用. (new by riceball)
     * TWallpaper 墙纸类
       * Style: wlpsCenter, wlpsTile, wlpsStretch
       * Alpha: the alpha blending value.
       * FileName: the wallpaper picture filename
       * Picture: TPicture
     * TBackground 背景类: 混合复合背景。
        it include a wallpaper property, a texture and a Gradient property, they are alpha blending after you proper set.
       * Wallpaper: the first draw(if any)
       * Gradient: the second draw(if any)
       * Texture: the last draw(if any). Texture is also a wallpaper property.
       * Buffered: whether cache the result.
     * TFont32 高级字体类: it supports Outline font, textured font or 3D Font with shadow. of cause it supports the antialiasing and transparency.
       * Shadow: TShadowEffect: 阴影效果
       * Quality: TFontQuality: 抗锯齿质量.
       * Outline: Boolean: 只输出字体轮廓
       * Opacity: Byte: 透明度
       * LineSpacing: integer: 行间距
       * CharSpacing: integer: 字符间距. <Note: Not used yet>
       * Background: TBackground: 字体贴图
       * fucntion TextExtent and TextExtentW（Unicode版）
       * fucntion RenderText and RenderTextW（Unicode版）
       * function DrawText: 模拟WinAPI的DrawText函数。支持以下的格式化参数: 
           DT_CALCRECT, DT_TOP, DT_VCENTER, DT_BOTTOM, DT_LEFT, DT_CENTER, DT_RIGHT, DT_WORDBREAK, DT_NOPREFIX, DT_EXPANDTABS
   * TCustomEffectProperty
     * TShadowEffect
       * Opacity: Byte: the alpha blending value.
       * OffsetX: Integer: the shadow X offset
       * OffsetY: Integer: the shadow Y offset
       * Enabled: Bool
       * Color: TColor
       * Blur: Byte: the shadow blur.
   * TBitmap32Ex: derived from TBitmap32, use the Font32.
 * GR32_FilterEx: provide many 3X3 , 5X5 and 7x7 filters and add new standard filters easy and other useful proc.
   * ApplyTransparentColor: set the specified color as Transparent.
   * ApplyBlueChannelToAlpha: set the BlueChannel value as alpha channel value.
   * ApplyBWImage: covnert a color image to two-color(black, white) image.
   * ...

the TGRCustomControl and TGRGraphicControl Ver 0.1 Buffer Mechanism:

1. All things paint to FBuffer (include the parent background if need) 
   repaint the FBuffer when the FBufferDirty is true.
  related methods: PaintBuffer, PaintParentBackground
2. paint itself to the FSelfBuffer if FSelfBufferDirty is true:
  PaintSelfToBuffer:
  PaintSelfTo(aBuffer): if FSelfBuffer not dirty then paint the FSelfBuffer to aBuffer

How to set the transparent to the GRControls:
 1. GRControl.Transparent := true;
 2. GRControl.Color := clNone;


特别感谢:
  GR32 Team(http://sourceforge.net/projects/graphics32) for their great GR32 Pack. No Them No this!
  Roman Gudchenko(c)  mailto:roma@goodok.ru for the G32_Interface.pas
  sharman1@uswest.net for the G32_WConvolution.pas
  Kambiz R. Khojasteh(kambiz@delphiarea.com) for his gradient component.
  Jens Weiermann <wexmanAT@solidsoftwareDOT.de>
  Vladimir Vasilyev <Vladimir@tometric.ru>
  Patrick
  周劲羽 (zjy@cnpack.org)
  Troels Jakobsen - delphiuser@get2net.dk
  Others I missed.


Note: 
No Register function and Component Icon!
GR32单元的修改支持高级字体：你只需要将 Updater.exe 和 gr32v183Patch.upd 文件放在GR32目录下，然后运行 update.exe 即可应用该修改。

here are my changes in GR32.pas:

type
  TFontClass = Class of TFont; //added by riceball

  TBitmap32 = class(TCustomMap)
   ...
  protected
    ....
    procedure SetPixelFS(X, Y: Single; Value: TColor32);
    procedure SetPixelXS(X, Y: TFixed; Value: TColor32);

  public
    constructor Create; override;
    destructor Destroy; override;

    class function CreateFont: TFont; virtual; //added by riceball
    ...
  end;

constructor TBitmap32.Create;
begin
  ...
  //FFont := TFont.Create;
  FFont := CreateFont; //modified by riceball
  FFont.OnChange := FontChanged;
  ...
end;

//added by riceball
class function TBitmap32.CreateFont(): TFont;
begin
  Result := TFont.Create;
end;

* 发现为何不能拖动Layer：必须是click(MouseDown, MouseUp)选中，然后在按住MouseDown拖动！即使重载MouseDown也不行！

procedure TCustomImage32.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Layer: TCustomLayer;
begin
  inherited;

  if TabStop and CanFocus then SetFocus;
  
  if Layers.MouseEvents then
    Layer := TLayerCollectionAccess(Layers).MouseDown(Button, Shift, X, Y) //在用MouseDown(..., Layer)之前已经调用。
  else
    Layer := nil;

  // lock the capture only if mbLeft was pushed or any mouse listener was activated
  if (Button = mbLeft) or (TLayerCollectionAccess(Layers).MouseListener <> nil) then
    MouseCapture := True;

  MouseDown(Button, Shift, X, Y, Layer); //根本不会再调用 Layers.MouseDown，所以你要在自己的类中重新处理。
end;



procedure TImage32Ex.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if (Layer is TTransformationLayer) then
  begin
    Selection := TTransformationLayer(Layer);
    if Assigned(FRubberBand) then
    begin
      Layers.MouseListener := nil;
      Layer := TLayerCollectionAccess(Layers).MouseDown(Button, Shift, X, Y); // 重新处理 here
    end;
  end
  else 
    Selection := nil;
  inherited;
 {$IFDEF Debug}
  if Assigned(Layer) then sendDebug('Image32Ex.MouseDown Layer=' + Layer.ClassName);
 {$ENDIF}
end;
