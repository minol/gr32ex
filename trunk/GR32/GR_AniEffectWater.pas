unit GR_AniEffectWater;

interface

uses
     Windows, Messages, Classes, Graphics, Controls
     , GR32
     , GR_AniEffects
     , uWaterEffect
     ;

type
  { Summary the water ripple animation effect. }
  TGRWaterAnimationEffect = class(TGRCustomAnimationEffect)
  private
    FBmpSrc: TBitmap;
    FClickBlob: Integer;
    FRandomBlob: Integer;
    FRandomDelay: Integer;
    FTrackBlob: Integer;
    FWater: TWaterEffect;
    function GetDamping: TWaterDamping;
    procedure SetDamping(const Value: TWaterDamping);
  protected
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoPaint(Sender: TControl; DC: HDC); override;
    procedure DoResize(Sender: TControl); override;
    procedure DoTimer(MoveCount: TFloat); override;
    procedure SizeChanged(Sender: TControl); override;
  public
    constructor Create(aOwner: TGRCustomAnimationEffects); override;
    destructor Destroy; override;
    { Summary �ڻ����ϲ���һ��ˮ��Ч���� }
    { Description
    x, y Ϊ���꣬���Ϊ -1 ��ʾ����㡣
    ARadius �� AHeight Ϊ��ʼ�뾶��Ч������
    }
    procedure Blob(x, y: Integer; ARadius, AHeight: Integer);
    { Summary ��ջ����ϵ�ˮ��Ч�� }
    procedure ClearWater;
    { Summary �������ʱ������ˮ��Ч�����ȣ�0 ��ʾ���� }
    property ClickBlob: Integer read FClickBlob write FClickBlob default
      csDefClickBlob;
    { Summary ˮ������ϵ�� }
    property Damping: TWaterDamping read GetDamping write SetDamping;
    { Summary ���������ˮ�������ȣ�0 ��ʾ���� }
    property RandomBlob: Integer read FRandomBlob write FRandomBlob default
      csDefRandomBlob;
    { Summary �������ˮ�ε���ʱ }
    property RandomDelay: Integer read FRandomDelay write FRandomDelay default
      csDefRandomDelay;
    { Summary ����ƶ��켣��ˮ�εķ��ȣ�0 ��ʾ���� }
    property TrackBlob: Integer read FTrackBlob write FTrackBlob default
      csDefTrackBlob;
  end;
  

implementation

uses
  Math;

constructor TGRWaterAnimationEffect.Create(aOwner: TGRCustomAnimationEffects);
begin
  inherited;
  FBmpSrc := TBitmap.Create;
  //FBmpDst := TBitmap.Create;
  FWater := TWaterEffect.Create;
  
  FRandomDelay := csDefRandomDelay;
  FRandomBlob := csDefRandomBlob;
  FTrackBlob := csDefTrackBlob;
  FClickBlob := csDefClickBlob;
  
  FWidth := cMaxWaterSize;
  FHeight := cMaxWaterSize;
end;

destructor TGRWaterAnimationEffect.Destroy;
begin
  FBmpSrc.Free;
  //FBmpDst.Free;
  FWater.Free;
  inherited;
end;

procedure TGRWaterAnimationEffect.Blob(x, y: Integer; ARadius, AHeight:
  Integer);
begin
  FWater.Blob(x, y, ARadius, AHeight);
end;

procedure TGRWaterAnimationEffect.ClearWater;
begin
  FWater.ClearWater;
end;

procedure TGRWaterAnimationEffect.DoMouseDown(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and PtInRect(Rect(Left, Top, Left+Width, Top+Height), Point(X, Y)) then
    Blob(X-Left, Y-Top, 1, FClickBlob);
end;

procedure TGRWaterAnimationEffect.DoMouseMove(Shift: TShiftState; X, Y:
  Integer);
begin
  if PtInRect(Rect(Left, Top, Left+Width, Top+Height), Point(X, Y)) then
  begin
    if ssLeft in Shift then
      Blob(X-Left, Y-Top, 1, FClickBlob)
    else
      Blob(X-Left, Y-Top, 1, FTrackBlob);
  end;
end;

procedure TGRWaterAnimationEffect.DoPaint(Sender: TControl; DC: HDC);
begin
  //x := Random(Sender.ClientWidth)- MaxWaterSize-1;
  
  BitBlt(FBmpSrc.Canvas.Handle, 0, 0, FBmpSrc.Width, FBmpSrc.Height, DC, FLeft, FTop, SRCCOPY);
  
  FWater.Render(FBmpSrc, FBmpSrc);
  BitBlt(DC, FLeft, FTop, FBmpSrc.Width, FBmpSrc.Height, FBmpSrc.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TGRWaterAnimationEffect.DoResize(Sender: TControl);
begin
  FWidth := Min(Sender.ClientWidth, cMaxWaterSize);
  FHeight := Min(Sender.ClientHeight, cMaxWaterSize);
  //w := Sender.ClientWidth;
  //h := Sender.ClientHeight;
  
  FLeft := (Sender.ClientWidth- FWidth) div 2;
  if FLeft < 0 then
  FLeft := 0;
  //y := Random(Sender.ClientHeight)- MaxWaterSize-1;
  FTop := (Sender.ClientHeight- FHeight) div 2;
  if FTop < 0 then
  FTop := 0;
  
  SizeChanged(Sender);
end;

procedure TGRWaterAnimationEffect.DoTimer(MoveCount: TFloat);
begin
  if (FRandomDelay > 0) and (FRandomBlob > 0) then
  begin
    if Random(Ceil(FRandomDelay / Integer(cMinIntervalValue)) + 1) = 0 then
      Blob(-1, -1, Random(2) + 1, Random(FRandomBlob) + 100);
  end;
end;

function TGRWaterAnimationEffect.GetDamping: TWaterDamping;
begin
  Result := FWater.Damping;
end;

procedure TGRWaterAnimationEffect.SetDamping(const Value: TWaterDamping);
begin
  FWater.Damping := Value;
end;

procedure TGRWaterAnimationEffect.SizeChanged(Sender: TControl);
begin
  FBmpSrc.SetSize(FWidth, FHeight);
  //FBmpDst.SetSize(FBmpSrc.Width,FBmpSrc.Height);
  FWater.SetSize(FBmpSrc.Width,FBmpSrc.Height);
end;


end.
