unit Unit1;

interface

uses
  DbugIntf,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls
  , Jpeg
  , GpSysHookCommon, GpSysHookComp
  , GR32, GR32_Resamplers, GR32_Image, GR32_System
  , GR32_Png
  , GR_Forms
  , GR_AniEffects
  , GR_Sprites
  , GR_AniGEffetcts
  , GR_ParticleAniEffects
  //, GR_AniEffectWater
  , GR_ParticleStar
  , GR_ParticleSnow
  , GR_DesktopControl
  , SimpleTimer
  ;

type
  TGREffectsLayeredForm = class(TGRCustomLayeredForm)
  private
    FEffEngine: TGRAnimationEffects;
    FOldUpdate: TNotifyEvent;
    procedure SetEffEngine(const Value: TGRAnimationEffects);
  protected
    procedure DoEffectUpdate(Sender: TObject);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure InternalPaintBuffer(aBuffer: TBitmap32); override;
  public
    property EffEngine: TGRAnimationEffects read FEffEngine write SetEffEngine;
  end;

  TGRPaintTHook = class(TGpSysHook)
  private
    FOnPaint: TNotifyEvent;
  protected
    class function HookType: TGpHookType; override;
    procedure ProcessMessage(var Message: TMessage); override;
  public
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;


  TForm1 = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    Image: TImage32;
    FDesktop: TGRDesktopControl;
    FEffEngine: TGRAnimationEffects;
    FStarSprites: TGRSprites;  
    FSnowSprites: TGRSprites;  
    FGpCBTHook: TGRPaintTHook;
    FLocked: Boolean;

    procedure DoPaintEvent(Sender: TObject);
  public
    { Déclarations publiques }
    constructor Create(aOwner: TComponent);override;
    destructor Destroy;override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TGREffectsLayeredForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  With Params do
  begin
    //wndParent := GetDesktopwindow;
    //ExStyle:=ExStyle or WS_EX_TOPMOST or WS_EX_TOOLWINDOW;
  end;
end;

procedure TGREffectsLayeredForm.DoEffectUpdate(Sender: TObject);
begin
end;

procedure TGREffectsLayeredForm.InternalPaintBuffer(aBuffer: TBitmap32); 
begin
end;

procedure TGREffectsLayeredForm.SetEffEngine(const Value: TGRAnimationEffects);
begin
	if Value <> FEffEngine then
	begin
	  if Assigned(FEffEngine) then
	  begin
	    FEffEngine.Enabled := False;
	    FEffEngine.OnUpdate := FOldUpdate;
	    FEffEngine.Control := nil;
	  end;
	  if Assigned(Value) then
	  begin
	  	Value.Enabled := False;
	    FOldUpdate := Value.OnUpdate;
  	  Value.OnUpdate := DoEffectUpdate;
  	  Value.Control := Self;
	  end;
	  FEffEngine := Value;
	end;
end;

class function TGRPaintTHook.HookType: TGpHookType;
begin
  Result := htCBT;
end; { TGRPaintTHook.HookType }

{:Dispatch hook messages.
}
procedure TGRPaintTHook.ProcessMessage(var Message: TMessage);
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
        HCBT_ACTIVATE
        //,HCBT_CLICKSKIPPED
        ,HCBT_CREATEWND
        ,HCBT_DESTROYWND
        ,HCBT_KEYSKIPPED
        ,HCBT_MINMAX
        ,HCBT_MOVESIZE
        ,HCBT_SETFOCUS
        //,HCBT_SYSCOMMAND
        :
          if Assigned(FOnPaint) then FOnPaint(Self);
      end; //case
    end;
  end;
end; { TGRPaintTHook.ProcessMessage }


procedure TForm1.DoPaintEvent(Sender: TObject);
begin
  //SendDebug('dd');
  FEffEngine.RequirePaint();
end;

{
procedure TForm1.DoPaintEvent(Sender: TObject; const aMsg: TMessage);
begin
  if not FLocked then
  try
    FLocked := True;
    case aMsg.Msg of
        WM_ACTIVATE
        //,WM_CLICKSKIPPED
        ,WM_CREATE
        ,WM_DESTROY
        //,WM_KEYSKIPPED
        //,WM_MINMAX
        ,WM_MOVE
        ,WM_SIZE
        , WM_WINDOWPOSCHANGED
        ,WM_SETFOCUS
        ,WM_SYSCOMMAND
        : begin
        FEffEngine.BGRePaint();
        end;
    end;
    //ShowMessage('Paint:'+ IntToStr(aMsg.Message));
    //SendDebug('Paint:'+ IntToStr(aMsg.Message));
  finally
    FLocked := False;
  end;
end;
}

constructor TForm1.Create(aOwner: TComponent);
var
  LPic: TPicture;
  s: string;
begin
  inherited;
  Image := TImage32.Create(Self);
  Image.Parent := Self;
  Image.Align := alClient;
  //Image32 := Image;
  //Image32.Color := clNone;
  //Image.Visible := False;
  //Image.Color := clGray;
  //Image.Bitmap.LoadFromFile('res\sky.jpg');
  Caption := 'Desktop Snowflake : Alt+F4 to Close';


  FEffEngine := TGRAnimationEffects.Create();
  FSnowSprites := TGRSprites.Create;  
  FStarSprites := TGRSprites.Create;  
  FGpCBTHook := TGRPaintTHook.Create(nil);
  FGpCBTHook.HookDLL := 'GpSysHookDLL';
  FGpCBTHook.OnPaint := DoPaintEvent;
  s := FGpCBTHook.Start;
  if s <> '' then
    ShowMessage(s);

  //the star particle animation effect
  with TGRParticlesEffect.Create(FEffEngine) do
  begin
    Sprites := FStarSprites;
    //UseCustomParticle(TGRSnowParticle);
    UseCustomParticle(TGRStarParticle);
    //Init the Particle property:
    with Particle do
    begin
    	LPic := TPicture.Create;
    	try
      //Looped := False;
      LPic.LoadFromFile('Res\pao.png');
      if LPic.Graphic is TBitmap then
      begin
        TBitmap(LPic.Graphic).Transparent := True;
        //TBitmap(LPic.Graphic).TransparentColor := clBlack;
      end;
      Picture.Assign(LPic);
      finally
        LPic.Free;
      end;
    end;

    Enabled := True;
    MaxParticles := 50;
    NumOfParticles := 2;
  end;

  //the snow particle animation effect
  with TGRParticlesEffect.Create(FEffEngine) do
  begin
    Sprites := FSnowSprites;
    UseCustomParticle(TGRSnowParticle);
    //Init the Particle property:
    with Particle do
    begin
    	LPic := TPicture.Create;
    	try
      //Looped := False;
      LPic.LoadFromFile('Res\snow.png');
      if LPic.Graphic is TBitmap then
      begin
        TBitmap(LPic.Graphic).Transparent := True;
        //TBitmap(LPic.Graphic).TransparentColor := clBlack;
      end;
      Picture.Assign(LPic);
      finally
        LPic.Free;
      end;
    end;
  
    Enabled := True;
    MaxParticles := 500;
    NumOfParticles := 200;
  end;

  FDesktop := TGRDesktopControl.Create(nil);
  FEffEngine.Control := FDesktop;
  //FEffEngine.Control := Image;
  FEffEngine.Enabled := True;

end;

destructor TForm1.Destroy;
begin
  //FGpCBTHook.Stop;
  FreeAndNil(FGpCBTHook);
  //FEffEngine.BGRePaint();
	FEffEngine.Enabled := False;
	FreeAndNil(FDesktop);
	FreeAndNil(FSnowSprites);
	FreeAndNil(FStarSprites);
	FEffEngine.Free;
  //FBitmap.Free;
  //FBitmap32.Free;
  InvalidateRect(0,Nil,False); 
  inherited;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
end;

end.
