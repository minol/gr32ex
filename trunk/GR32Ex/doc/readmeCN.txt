The GR32 ��չ�����ؼ���. Ver 0.1

Writen by Riceball LEE(riceball@users.sourceforge.net)

����:
 * ͨ�þ��鶯������
 * ͨ�ö�����Ч����
   * ˮ����Ч
   * ͨ��������Ч��������
     * �ǿ�������Ч
     * ��ѩ������Ч
 * ֧�ְ�͸�� GRControl ����ؼ���:
   * �ؼ���͸����:
   * ����ؼ�����ͼ�λ��߿�֧��: ֧����̬��Hot, Down, Normal
   * ��͸���ĸ��ϱ���֧��
       * ǽֽ Wallpaper: ���Ȼ��ƣ�����еĻ���
       * ���� Gradient:  ��λ��ƣ�����еĻ���
       * ��ͼ Texture:   �����ƣ�����еĻ���
 * �߼�����֧��
   * ��ͼ����
   * ��������
   * ������Ӱ
   * �����
   * ��͸��

��GR_AniGEffetcts��Ԫ�е� TGRAnimationEffects �����ͨ�����ҿؼ��ķ�ʽ�����������Ƶ� TCustomPaintBox32(TImage32), TCustomControl, TGraphicControl �� TCustomForm�������ڱ�׼GDI�Ŀؼ��ϻ��Ƶ�ʱ�������Բ

��Image32�ϵ�GDI����������Ч������������ Examples\particle\Ŀ¼�£�������ͬʱ��Ⱦ��ˮ����Ч����ѩ��Ч���ǹ���Ч��

The GR32 Extension ���ӿؼ����Ŀ�ܹ���:
 * ���е�GR���ӿؼ����Ǵ�TGRCustomControl��TGRGraphicControl)�����ġ� TGRCustomControl����Window �����WinControl��
 * �ؼ���͸����:
 * ��͸���ĸ��ϱ���֧��
 * �߼�����֧��
   * ��ͼ����
   * ��������
   * ������Ӱ
   * �����
   * ��͸��
 * ������õĸ����ࣺ
   * TCustomGraphicProperty(GR32_GraphUtils): you can add the perfect properties to your components too.
    * GR32_Graphics Properties:
     * TGradient ������: (original writen by Kambiz R. Khojasteh(kambiz@delphiarea.com))
       It is an extremely fast gradient fill control with 
       a large set of styles. As built-in, TGradient can 
       draw gradient in 23 styles and provides an easily 
       method to define custom styles. In addition, this 
       control can shift and/or rotate the gradient colors, 
       which could be used for creating animated gradients.
       * AlphaBegin ��ʼ�İ�͸����(new by riceball)
       * AlphaEnd   �����İ�͸����(new by riceball)
       * AlphaChannel: Boolean. �Ƿ񽫽�����ΪAlphaͨ��ʹ��. (new by riceball)
     * TWallpaper ǽֽ��
       * Style: wlpsCenter, wlpsTile, wlpsStretch
       * Alpha: the alpha blending value.
       * FileName: the wallpaper picture filename
       * Picture: TPicture
     * TBackground ������: ��ϸ��ϱ�����
        it include a wallpaper property, a texture and a Gradient property, they are alpha blending after you proper set.
       * Wallpaper: the first draw(if any)
       * Gradient: the second draw(if any)
       * Texture: the last draw(if any). Texture is also a wallpaper property.
       * Buffered: whether cache the result.
     * TFont32 �߼�������: it supports Outline font, textured font or 3D Font with shadow. of cause it supports the antialiasing and transparency.
       * Shadow: TShadowEffect: ��ӰЧ��
       * Quality: TFontQuality: ���������.
       * Outline: Boolean: ֻ�����������
       * Opacity: Byte: ͸����
       * LineSpacing: integer: �м��
       * CharSpacing: integer: �ַ����. <Note: Not used yet>
       * Background: TBackground: ������ͼ
       * fucntion TextExtent and TextExtentW��Unicode�棩
       * fucntion RenderText and RenderTextW��Unicode�棩
       * function DrawText: ģ��WinAPI��DrawText������֧�����µĸ�ʽ������: 
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


�ر��л:
  GR32 Team(http://sourceforge.net/projects/graphics32) for their great GR32 Pack. No Them No this!
  Roman Gudchenko(c)  mailto:roma@goodok.ru for the G32_Interface.pas
  sharman1@uswest.net for the G32_WConvolution.pas
  Kambiz R. Khojasteh(kambiz@delphiarea.com) for his gradient component.
  Jens Weiermann <wexmanAT@solidsoftwareDOT.de>
  Vladimir Vasilyev <Vladimir@tometric.ru>
  Patrick
  �ܾ��� (zjy@cnpack.org)
  Troels Jakobsen - delphiuser@get2net.dk
  Others I missed.


Note: 
No Register function and Component Icon!
GR32��Ԫ���޸�֧�ָ߼����壺��ֻ��Ҫ�� Updater.exe �� gr32v183Patch.upd �ļ�����GR32Ŀ¼�£�Ȼ������ update.exe ����Ӧ�ø��޸ġ�

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

* ����Ϊ�β����϶�Layer��������click(MouseDown, MouseUp)ѡ�У�Ȼ���ڰ�סMouseDown�϶�����ʹ����MouseDownҲ���У�

procedure TCustomImage32.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Layer: TCustomLayer;
begin
  inherited;

  if TabStop and CanFocus then SetFocus;
  
  if Layers.MouseEvents then
    Layer := TLayerCollectionAccess(Layers).MouseDown(Button, Shift, X, Y) //����MouseDown(..., Layer)֮ǰ�Ѿ����á�
  else
    Layer := nil;

  // lock the capture only if mbLeft was pushed or any mouse listener was activated
  if (Button = mbLeft) or (TLayerCollectionAccess(Layers).MouseListener <> nil) then
    MouseCapture := True;

  MouseDown(Button, Shift, X, Y, Layer); //���������ٵ��� Layers.MouseDown��������Ҫ���Լ����������´���
end;



procedure TImage32Ex.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if (Layer is TTransformationLayer) then
  begin
    Selection := TTransformationLayer(Layer);
    if Assigned(FRubberBand) then
    begin
      Layers.MouseListener := nil;
      Layer := TLayerCollectionAccess(Layers).MouseDown(Button, Shift, X, Y); // ���´��� here
    end;
  end
  else 
    Selection := nil;
  inherited;
 {$IFDEF Debug}
  if Assigned(Layer) then sendDebug('Image32Ex.MouseDown Layer=' + Layer.ClassName);
 {$ENDIF}
end;
