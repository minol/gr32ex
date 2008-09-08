The GR32 Extension Controls Pack. Ver 0.1

Writen by Riceball LEE(riceball@users.sourceforge.net)

Feature:
 * General Sprites Engine
 * General Animation Effects Engine
   * Water Animation Effects
   * General Particle Engine
     * Star Particle Animation Effects
     * Snow Particle Animation Effects
 * General GRControl Pack:
   * Alpha Blending Controls:
   * General Frame supports: Hot, Down, Normal state for frame
   * Composed Background with Alpha Blending supports
       * Wallpaper: the first draw(if any)
       * Gradient: the second draw(if any)
       * Texture: the last draw(if any). Texture is also a wallpaper property.
 * Advanced Font
   * Outline font
   * Textured font 
   * Shadow font
   * Antialiasing and transparency.

The GR32 Extension Components Core Framework Feature:
 * the controls(derived from TGRCustomControl or TGRGraphicControl) can support the alpha blending. All GRControl should be derived from.
 * the controls(derived from TGRBGCustomControl or TGRBGGraphicControl) can support the very complex background.
 * the controls(derived from TGRCustomControl or TGRGraphicControl) can supports the Advanced Font which supports Outline font, 
    textured font or 3D Font with shadow. of cause it supports the antialiasing and transparency.
 * Many Useful Helper Classes
   * TCustomGraphicProperty(GR32_GraphUtils): you can add the perfect properties to your components too.
    * GR32_Graphics Properties:
     * TGradient Property: (original writen by Kambiz R. Khojasteh(kambiz@delphiarea.com))
       It is an extremely fast gradient fill control with 
       a large set of styles. As built-in, TGradient can 
       draw gradient in 23 styles and provides an easily 
       method to define custom styles. In addition, this 
       control can shift and/or rotate the gradient colors, 
       which could be used for creating animated gradients.
       * AlphaBegin (new by riceball)
       * AlphaEnd (new by riceball)
       * AlphaChannel: Boolean. whether treat the gradient as a alpha channel graph. (new by riceball)
     * TWallpaper Property
       * Style: wlpsCenter, wlpsTile, wlpsStretch
       * Alpha: the alpha blending value.
       * FileName: the wallpaper picture filename
       * Picture: TPicture
     * TBackground Property: you can build very complex composed background here. 
        it include a wallpaper property, a texture and a Gradient property, they are alpha blending after you proper set.
       * Wallpaper: the first draw(if any)
       * Gradient: the second draw(if any)
       * Texture: the last draw(if any). Texture is also a wallpaper property.
       * Buffered: whether cache the result.
     * TFont32 Property: it supports Outline font, textured font or 3D Font with shadow. of cause it supports the antialiasing and transparency.
       * Shadow: TShadowEffect: the font shadow
       * Quality: TFontQuality: antialiasing quality.
       * Outline: Boolean: whether the text is outline only.
       * Opacity: Byte: the alpha blending value.
       * LineSpacing: integer: Specifies the spacing between Lines.
       * CharSpacing: integer: Specifies the spacing between characters. <Note: Not used yet>
       * Background: TBackground: The Font background Texture if any.
       * fucntion TextExtent and TextExtentW
       * fucntion RenderText and RenderTextW
       * function DrawText: simulate the winapi. the following aFormat Options are supported: 
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
 * GR32_FilterEx: provide many 3X3 and 5X5 filters and add new standard filters easy and other useful proc.
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

Note:the TGRAnimationEffects(GR_AniGEffetcts) implement stdcontrol performance is very pool: invalidate the whole background every-time is not clever .
     and this make the speed slow down.
     the TGRAnimationEffects paint to the TImage32 is well.

The Animations:
   Control one layer or more layers to execute some animation. these layers are called as "target".
   each animation layer will be wrapped by an animationInstance when executing animation.

thanks to:
  GR32 Team(http://sourceforge.net/projects/graphics32) for their great GR32 Pack. No Them No this!
  Roman Gudchenko(c)  mailto:roma@goodok.ru for the G32_Interface.pas
  sharman1@uswest.net for the G32_WConvolution.pas
  Kambiz R. Khojasteh(kambiz@delphiarea.com) for his gradient component.
  Jens Weiermann <wexmanAT@solidsoftwareDOT.de>
  Vladimir Vasilyev <Vladimir@tometric.ru>
  Patrick
  JinYu Zhou (zjy@cnpack.org)
  Troels Jakobsen - delphiuser@get2net.dk
  Others I missed.


Note: 
No Register function and Component Icon!
I have to hack into GR32.pas to support my TFont32.
hack into GR32_layers to support the MouseInControl , focused layer and keyboard event.

you can apply the patch: gr32v183patch.txt(put Updater.exe and gr32v183Patch.upd to the graphics32 dir and run update.exe)

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

layers animation:
 * Path Animator: Line,spline (closed): key point Animation, related path or absoluted path animation.
 * distortion animator: keyframe animation.