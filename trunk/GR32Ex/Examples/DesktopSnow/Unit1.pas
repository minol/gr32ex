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
    Image: TImage32;
    OpenPlay: TButton;
    Snapshot: TButton;
    OpenDialog: TOpenDialog;
    CallBack: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FDesktop: TGRDesktopControl;
    FEffEngine: TGRAnimationEffects;
    FStarSprites: TGRSprites;  
    FSnowSprites: TGRSprites;  
    FGpCBTHook: TGRPaintTHook;

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
        //,HCBT_KEYSKIPPED
        ,HCBT_MINMAX
        ,HCBT_MOVESIZE
        ,HCBT_SETFOCUS
        ,HCBT_SYSCOMMAND
        :
          if Assigned(FOnPaint) then FOnPaint(Self);
      end; //case
    end;
  end;
end; { TGRPaintTHook.ProcessMessage }


procedure TForm1.DoPaintEvent(Sender: TObject);
begin
  //SendDebug('dd');
  FEffEngine.BGRePaint();
end;

constructor TForm1.Create(aOwner: TComponent);
var
  LPic: TPicture;
begin
  inherited;
  OpenPlay.Visible := False;
  Snapshot.Visible := False;
  CallBack.Visible := False;
  //Image.Color := clGray;
  //Image.Bitmap.LoadFromFile('res\sky.jpg');
  Caption := 'Desktop Snowflake : Alt+F4 to Close';


  FEffEngine := TGRAnimationEffects.Create();
  FSnowSprites := TGRSprites.Create;  
  FStarSprites := TGRSprites.Create;  
  FGpCBTHook := TGRPaintTHook.Create(nil);
  FGpCBTHook.HookDLL := 'DeskHook';
  FGpCBTHook.OnPaint := DoPaintEvent;
  ShowMessage(FGpCBTHook.Start);

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
  FEffEngine.Enabled := True;

end;

destructor TForm1.Destroy;
begin
  FGpCBTHook.Stop;
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
  CallBack.Checked := false;
end;

end.
