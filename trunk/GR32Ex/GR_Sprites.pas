{ Summary the General Sprites Engine }
{ Description
Note: I used the TFloat point type.
}
unit GR_Sprites;

interface

{.$Define DEBUG}

uses
  {$IFDEF DEBUG}
  //DbugIntf,
  CnDebug,
  {$ENDIF}
  SysUtils, Windows, Messages, Classes, Graphics
  //, Dialogs //ShowMessage
  , GR32 //TFixed
  , GR32_Math
  , GR32_Transforms
  ;

type
  //TPictureEx = TPicture;
  TPictureEx = TBitmap32;
  TGRSprite = class;
  TGRSprites = class;
  TGRSprite = class(TObject)
  private
    procedure SetHeight(const Value: Integer);
    procedure SetLeft(const Value: TFloat);
    procedure SetName(const Value: string);
    procedure SetOffsetX(const Value: TFloat);
    procedure SetOffsetY(const Value: TFloat);
    procedure SetRotation(const Value: TFloat);
    procedure SetScaleX(const Value: TFloat);
    procedure SetScaleY(const Value: TFloat);
    procedure SetTop(const Value: TFloat);
    procedure SetWidth(const Value: Integer);
    procedure SetZ(const Value: Integer);
  protected
    FAlpha: Byte;
    FCenterX: TFloat;
    FCenterY: TFloat;
    FHeight: Integer;
    FIsDead: Boolean;
    FLeft: TFloat;
    FName: string;
    FOffsetX: TFloat;
    FOffsetY: TFloat;
    FParent: TGRSprites;
    FRect: TRect;
    FRotation: TFloat;
    FScaleX: TFloat;
    FScaleY: TFloat;
    FTop: TFloat;
    FUpdateCount: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    FZ: Integer;
    IsTransformed: Boolean;
    { Summary the Position, Size, Rotation or Scale changed. }
    procedure InternalUpdate; virtual;
    function IsUpdating: Boolean;
    property Rect: TRect read FRect;
  public
    constructor Create(aOwner: TGRSprites); virtual;
    destructor Destroy; override;
    procedure BeginUpdate;
    { Summary let the sprite die. }
    procedure Die; virtual;
    { Summary the Collision occur with aSprite at X, Y }
    procedure DoOnCollision(aSprite: TGRSprite;  const X, Y:  Integer); virtual;
    procedure Draw(const aDC: HDC; const aRect: TRect); overload; virtual;
      abstract;
    procedure Draw(const aBuffer: TBitmap32); overload; virtual; abstract;
    procedure EndUpdate;
    procedure Move(const MoveCount: TFloat); virtual;
    procedure Update;
    property Parent: TGRSprites read FParent;
  published
    property Alpha: Byte read FAlpha write FAlpha default 255;
    property CenterX: TFloat read FCenterX;
    property CenterY: TFloat read FCenterY;
    property Height: Integer read FHeight write SetHeight;
    property IsDead: Boolean read FIsDead;
    property Left: TFloat read FLeft write SetLeft;
    property Name: string read FName write SetName;
    property OffsetX: TFloat read FOffsetX write SetOffsetX;
    property OffsetY: TFloat read FOffsetY write SetOffsetY;
    { Summary the degree. }
    property Rotation: TFloat read FRotation write SetRotation;
    property ScaleX: TFloat read FScaleX write SetScaleX;
    property ScaleY: TFloat read FScaleY write SetScaleY;
    property Top: TFloat read FTop write SetTop;
    property Visible: Boolean read FVisible write FVisible default true;
    property Width: Integer read FWidth write SetWidth;
    { Summary Z-Order }
    property Z: Integer read FZ write SetZ;
  end;
  
  TGRSprites = class(TObject)
  private
    function GetCount: Integer;
    function GetItems(Index: Integer): TGRSprite;
  protected
    { Summary the temp bitmap32 for Item use }
    FBitmap32: TBitmap32;
    { Summary the dead sprites(waiting removing). }
    FDeadList: TList;
    FLeft: TFloat;
    FSpriteList: TList;
    FTop: TFloat;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Sprite: TGRSprite);
    procedure CleanDeadSprites;
    procedure Draw(const aDC: HDC; const aRect: TRect); overload;
    procedure Draw(const aBuffer: TBitmap32); overload;
    procedure Move(MoveCount: TFloat);
    procedure Remove(const Sprite: TGRSprite);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TGRSprite read GetItems; default;
    property Left: TFloat read FLeft write FLeft;
    property Top: TFloat read FTop write FTop;
  end;
  
  TGRImageSprite = class(TGRSprite)
  private
    FPicture: TPictureEx;
    procedure SetPicture(const Value: TPictureEx);
  protected
    FOldPictureChangeProc: TNotifyEvent;
    FTransformation: TAffineTransformation;
    procedure ApplyTransformation;
    procedure DoPictureChange(Sender: TObject); virtual;
    procedure InternalUpdate; override;
  public
    constructor Create(aOwner: TGRSprites); override;
    destructor Destroy; override;
    procedure Draw(const aDC: HDC; const aRect: TRect); overload; override;
    procedure Draw(const aBuffer: TBitmap32); overload; override;
    property Picture: TPictureEx read FPicture write SetPicture;
  end;
  

function RoundNormal(Value: Extended): Integer;

implementation

uses
  Math;
  
function RoundNormal(Value: Extended): Integer;
var
     CurrencyValue: Currency;
begin
     if Value < 0 then
     begin
          CurrencyValue := Value - 0.5;
          Result := Trunc(CurrencyValue);
     end
     else
     begin
          CurrencyValue := Value + 0.5;
          Result := Trunc(CurrencyValue);
     end;
end;

constructor TGRSprite.Create(aOwner: TGRSprites);
begin
  inherited Create;
  FParent := aOwner;
  FAlpha := 255;
  FWidth := 10;
  FHeight := 10;
  FScaleX := 1;
  FScaleY := 1;
  FVisible := True;
  
  if Assigned(FParent) then FParent.Add(self);
end;

destructor TGRSprite.Destroy;
begin
  {$IFDEF DEBUG}
  CnDebugger.LogEnter('TGRSprite.Destroy');
  {$ENDIF}
  
  {$ifdef debug}
  SendDebug('FreeBefore.FList:'+ IntToStr(Parent.FSpriteList.Count));
  {$endif}
  Parent.Remove(Self);
  Parent.FDeadList.Remove(self);
  
  {$ifdef debug}
  SendDebug('FreeAfter.FList:'+ IntToStr(Parent.Count));
  {$endif}
  inherited Destroy;
  {$IFDEF DEBUG}
  CnDebugger.LogLeave('TGRSprite.Destroy');
  {$ENDIF}
end;

procedure TGRSprite.BeginUpdate;
begin
  Inc(FUpdateCount);
  //if FUpdateCount = 1 then SetUpdating(True);
end;

procedure TGRSprite.Die;
begin
  if not FIsDead then
  begin
    FIsDead := true;
    Parent.FDeadList.Add(self);
  end;
end;

procedure TGRSprite.DoOnCollision(aSprite: TGRSprite;  const X, Y:  Integer);
begin
end;

procedure TGRSprite.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount <= 0) then
  begin
      InternalUpdate;
  end;
  //if (FUpdateCount = 0) then
    //SetUpdating(False);
end;

procedure TGRSprite.InternalUpdate;
begin
  IsTransformed := (Rotation <> 0) or (ScaleX <> 1) or (ScaleY <> 1);
end;

function TGRSprite.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TGRSprite.Move(const MoveCount: TFloat);
begin
end;

procedure TGRSprite.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    FRect.Bottom := FRect.Top + FHeight;
    FCenterY := FHeight / 2;
    Update;
  end;
end;

procedure TGRSprite.SetLeft(const Value: TFloat);
begin
  if FLeft <> Value then
  begin
  FLeft := Value;
    {FRight := FLeft + FWidth;
    FRightMirror := FRight;
    FLeftMirror := FLeft;
    //}
    FRect.Left := RoundNormal(FLeft);
    FRect.Right := RoundNormal(FLeft) + FWidth;
    //FXCenter := FLeft + OffsetX + FWidth/2;
  
    Update;
  end;
end;

procedure TGRSprite.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
  end;
end;

procedure TGRSprite.SetOffsetX(const Value: TFloat);
begin
  FOffsetX := Value;
  //FXCenter := FLeft + FOffsetX + FWidth / 2;
end;

procedure TGRSprite.SetOffsetY(const Value: TFloat);
begin
  FOffsetY := Value;
  //FYCenter := FTop + FOffsetY + FHeight / 2;
end;

procedure TGRSprite.SetRotation(const Value: TFloat);
begin
  if FRotation <> Value then
  begin
    FRotation := Value;
    Update;
  end;
end;

procedure TGRSprite.SetScaleX(const Value: TFloat);
begin
  if FScaleX <> Value then
  begin
    FScaleX := Value;
    Update;
  end;
end;

procedure TGRSprite.SetScaleY(const Value: TFloat);
begin
  if FScaleY <> Value then
  begin
    FScaleY := Value;
    Update;
  end;
end;

procedure TGRSprite.SetTop(const Value: TFloat);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    FRect.Top := RoundNormal(FTop);
    FRect.Bottom := RoundNormal(FTop) + FHeight;
  
    //FYCenter:= FTop + FHeight/2;
    Update;
  end;
end;

procedure TGRSprite.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    FRect.Right := FRect.Left + FWidth;
    FCenterX := FWidth / 2;
    Update;
  end;
end;

procedure TGRSprite.SetZ(const Value: Integer);
begin
  if FZ <> Value then
  begin
    FZ := Value;
    Parent.FSpriteList.Remove(self);
    Parent.Add(self);
  end;
end;

procedure TGRSprite.Update;
begin
  BeginUpdate;
  try
  finally
    EndUpdate;
  end;
  //}
end;

constructor TGRSprites.Create;
begin
  inherited Create;
  FSpriteList := TList.Create;
  FDeadList := TList.Create;
  FBitmap32 := TBitmap32.Create;
  
  FBitmap32.DrawMode := dmBlend;
end;

destructor TGRSprites.Destroy;
begin
  while FSpriteList.Count > 0 do
    TGRSprite(FSpriteList.Items[FSpriteList.Count - 1]).Free;
  FreeAndNil(FSpriteList);
  FreeAndNil(FDeadList);
  FreeAndNil(FBitmap32);
  inherited Destroy;
end;

procedure TGRSprites.Add(const Sprite: TGRSprite);
var
  l, h, dif, m: Integer;
begin
  l := 0;
  h := FSpriteList.Count - 1;
  while l <= h do
  begin
    m := (l + h) div 2;
    dif := TGRSprite(FSpriteList.Items[m]).FZ - Sprite.FZ;
    if dif < 0 then l := m + 1
    else h := m - 1;
  end;
  FSpriteList.Insert(l, Sprite);
end;

procedure TGRSprites.CleanDeadSprites;
begin
  {$IFDEF DEBUG}
  CnDebugger.LogEnter('TGRSprites.CleanDeadSprites');
  {$ENDIF}
  While FDeadList.Count > 0 do
    TGRSprite(FDeadList[FDeadList.Count-1]).Free;
  
  //if FDeadList.Count > 0 then
  //if TGRSprite(FDeadList[0]) is TGRSprite then
    //TGRSprite(FDeadList[0]).Free;
  
  {$IFDEF DEBUG}
  CnDebugger.LogLeave('TGRSprites.CleanDeadSprites');
  {$ENDIF}
end;

procedure TGRSprites.Draw(const aDC: HDC; const aRect: TRect);
var
  I: Integer;
begin
  for i := 0 to FSpriteList.Count - 1 do
    TGRSprite(FSpriteList.Items[i]).Draw(aDC, aRect);
end;

procedure TGRSprites.Draw(const aBuffer: TBitmap32);
var
  I: Integer;
begin
  {$IFDEF DEBUG}
  //SendDebug('Draw Sprites:'+IntToStr(FSpriteList.Count));
  {$ENDIF}
  
  for i := 0 to FSpriteList.Count - 1 do
    with TGRSprite(FSpriteList.Items[i]) do
      if not IsDead then Draw(aBuffer);
end;

function TGRSprites.GetCount: Integer;
begin
  if FSpriteList <> nil then
    Result := FSpriteList.Count
  else
    Result := 0;
end;

function TGRSprites.GetItems(Index: Integer): TGRSprite;
begin
  if (FSpriteList <> nil) and (index >= 0) and (index < FSpriteList.Count) then
    Result := TGRSprite(FSpriteList[index])
  else Result := nil;
end;

procedure TGRSprites.Move(MoveCount: TFloat);
var
  I: Integer;
begin
  for i := 0 to FSpriteList.Count - 1 do
    TGRSprite(FSpriteList.Items[i]).Move(MoveCount);
end;

procedure TGRSprites.Remove(const Sprite: TGRSprite);
begin
  FSpriteList.Remove(Sprite);
end;

constructor TGRImageSprite.Create(aOwner: TGRSprites);
begin
  inherited Create(aOwner);
  FTransformation := TAffineTransformation.Create;
end;

destructor TGRImageSprite.Destroy;
begin
  FreeAndNil(FTransformation);
  inherited Destroy;
end;

procedure TGRImageSprite.ApplyTransformation;
begin
  {$IFDEF DEBUG}
    CnDebugger.LogEnter('TGRImageSprite.ApplyTransformation');
  try
  {$ENDIF}
  
  FTransformation.Clear;
  if (FWidth <> 0) and (FHeight <> 0) then
  begin
    FTransformation.SrcRect := FloatRect(0, 0, FWidth, FHeight);
    if Rotation <> 0 then
      FTransformation.Rotate(FCenterX, FCenterY, Rotation);
    {$IFDEF DEBUG}
    //SendDebug('Draw Sprite ScaleX:'+FloatToStr(ScaleX));
    //SendDebug('Draw Sprite ScaleY:'+FloatToStr(ScaleY));
    {$ENDIF}
    if (FScaleX <> 1) or (FScaleY <> 1) then
      FTransformation.Scale(FScaleX, FScaleY);
    FTransformation.Translate(FLeft + FOffsetX + FParent.Left, FTop + FOffsetY + FParent.Top);
  end;
  
  {$IFDEF DEBUG}
  finally
    CnDebugger.LogLeave('TGRImageSprite.ApplyTransformation');
  end;
  {$ENDIF}
end;

procedure TGRImageSprite.DoPictureChange(Sender: TObject);
begin
  if Assigned(FPicture) then
  begin
    FWidth := FPicture.Width;
    FHeight := FPicture.Height;
  
    FCenterX := FWidth / 2;
    FCenterY := FHeight / 2;
  
    Update;
  
    //Bug: this will raise the stack overflow here!!!! commet
    //if Assigned(FOldPictureChangeProc) and Assigned(Sender) then
      //FOldPictureChangeProc(Sender);
  end;
end;

procedure TGRImageSprite.Draw(const aDC: HDC; const aRect: TRect);
var
  RelX, RelY: Integer;
  W, H: Integer;
begin
  (*if not FVisible or not assigned(FPicture) or not Assigned(FParent) then Exit;
  
  RelX := RoundNormal(FLeft + FOffsetX + FParent.Left);
  RelY := RoundNormal(FTop + FOffsetY + FParent.Top);
  
  w := aRect.Right-aRect.Left;
  h := aRect.Bottom - aRect.Top;
  if //not aBuffer.Empty
     (RelX < w)
    and (RelY < h)
  then
  begin
    //FParent.FBitmap32.Assign(Picture);
    //FParent.FBitmap32.MasterAlpha := FAlpha;
    FPicture.MasterAlpha := FAlpha;
    if (ScaleX <> 1) or (ScaleY <> 1) then
    begin
      w := RoundNormal(FPicture.Width * ScaleX);
      h := RoundNormal(FPicture.Height * ScaleY);
      StretchBlt(aDC, RelX, RelY, w, h, FPicture.Handle, 0, 0,
        FPicture.Width, FPicture.Height, MERGECOPY);
    end
    else begin
      w := Min(FPicture.Width, w);
      h := Min(FPicture.Height, h);
      BitBlt(aDC, RelX, RelY, w, h, FPicture.Handle, 0, 0, MERGECOPY);
    end;
  
    if IsTransformed then
    begin
      //Transform(aBuffer, FParent.FBitmap32, FTransformation);
      //Transform(aBuffer, FPicture, FTransformation);
    end
    else//}
    begin
      //FParent.FBitmap32.DrawTo(aBuffer, RelX, RelY);
      //FPicture.DrawTo(aBuffer, RelX, RelY);
    end;
  end;
  //*)
end;

procedure TGRImageSprite.Draw(const aBuffer: TBitmap32);
var
  RelX, RelY: Integer;
begin
  {$IFDEF DEBUG}
    CnDebugger.LogEnter('TGRImageSprite.Draw');
  try
  {$ENDIF}
  if not FVisible or not assigned(FPicture) or not Assigned(FParent) then Exit;
  
  RelX := RoundNormal(FLeft + FOffsetX + FParent.Left);
  RelY := RoundNormal(FTop + FOffsetY + FParent.Top);
  {$IFDEF DEBUG}
    CnDebugger.LogInteger(RelX, 'RelX');
    CnDebugger.LogInteger(RelY, 'RelY');
  {$ENDIF}
  
  if not aBuffer.Empty
    and (RelX < aBuffer.Width)
    and (RelY < aBuffer.Height)
  then
  begin
    //FParent.FBitmap32.Assign(Picture);
    //FParent.FBitmap32.MasterAlpha := FAlpha;
    FPicture.MasterAlpha := FAlpha;
  {$IFDEF DEBUG}
    //CnDebugger.LogInteger(FAlpha, 'Alpha');
    //CnDebugger.LogObjectWithTag(aBuffer, 'Buffer');
    //CnDebugger.LogObjectWithTag(FPicture, 'Pic');
  {$ENDIF}
  
    if IsTransformed then
    begin
      //Transform(aBuffer, FParent.FBitmap32, FTransformation);
      Transform(aBuffer, FPicture, FTransformation);
      //FPicture.DrawTo(aBuffer, RelX, RelY);
    end
    else//}
    begin
      //FParent.FBitmap32.DrawTo(aBuffer, RelX, RelY);
      FPicture.DrawTo(aBuffer, RelX, RelY);
    end;
  {$IFDEF DEBUG}
    CnDebugger.LogMsg('AfterDraw');
  {$ENDIF}
  end;
  {$IFDEF DEBUG}
  finally
    CnDebugger.LogLeave('TGRImageSprite.Draw');
  end;
  {$ENDIF}
end;

procedure TGRImageSprite.InternalUpdate;
begin
  inherited InternalUpdate;
  if IsTransformed then
  begin
    ApplyTransformation;
  end;
end;

procedure TGRImageSprite.SetPicture(const Value: TPictureEx);
begin
  if FPicture <> Value then
  begin
  {if Assigned(FPicture) then
  begin
    FPicture.OnChange := FOldPictureChangeProc;
  end;//}
  FPicture := Value;
  if Assigned(FPicture) then
  begin
    //FOldPictureChangeProc := FPicture.OnChange;
    //FPicture.OnChange := DoPictureChange;
    DoPictureChange(nil);
  end;
  end;
end;


end.
