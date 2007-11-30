{ Summary the animation transition effects from One Image to another Image }
unit GR_AniTransition;

{$I GR32.inc}
{$T-,W-,X+,P+}
{$J+} {- Enable writeable const}

interface

uses
  {$IFDEF CLX}
  Qt, Types, QGraphics,
  {$ELSE}
  Windows, Graphics,
  {$ENDIF}
  Classes, SysUtils
  , Forms
  , GR32_Blend
  , GR32, GR_LowLevel
  ;

Type
  TBitmap32 = GR32.TBitmap32;
{!============================================================================!}

resourcestring

{ Error messages }

  rsTransitionAlreadyExists = 'TransitionName procedute %s already exists in list';
  rsTransitionTypeAlreadyExists = 'TransitionType  %s already exists in list';

const
  cDefaultPlayTime = 400; //ms

const
  { WARNINGS ! Do not translate this strings }
  SRandomSelection   = '[ RANDOM ] - Random selection';


type

  TGRTransitionProc = procedure;
  TGRCustomTransition = class;



  //TGRTransitionProcKind = (pkFade, pkSlide, pkManual);

  TGRTransitionParam = Class;
  TGRTransitionParamClass = Class of TGRTransitionParam;

{ TransitionProcList Declaration }

  TGRTransitionProcItem = class
  private
    //FKind: TGRTransitionProcKind;
    FKind: string;
    FProc: TGRTransitionProc;
    FName: string;
    FTitle: string;
    FParamClass: TGRTransitionParamClass;
  public
    property Kind: string read FKind write FKind;
    property Proc: TGRTransitionProc read FProc write FProc;
    //for index
    property Name: string read FName write FName;
    //for display
    property Title: string read FTitle write FTitle;
    property ParamClass: TGRTransitionParamClass read FParamClass write FParamClass;
  end;

  //the internal transition record for the developer of transition only.
  PGRTransitionRec = ^TGRTransitionRec;
  TGRTransitionRec = record
    Index: integer;
    //MatrixWidth, MatrixHeight: integer;
    //MatrixLen: integer;
    //MatrixFade: PGRMatrixFade;
    //CopyMatrixFade: PGRMatrixFade;
    //MatrixSlide: PGRMatrixSlide;
    Animating, Drawing, Reserved: boolean;
    CurTime: single;
    Transition: TGRCustomTransition;
    DestImage, SourceImage, ResultImage: TBitmap32;
    ProcItem: TGRTransitionProcItem;
    Canvas: TCanvas;
    X, Y: integer;
    //for use the transition param if any
    Param: TGRTransitionParam;
  end;

  //the abstract TGRTransitionParam
  TGRTransitionParam = class(TObject)
  private
  protected
    FOwner: PGRTransitionRec;

    procedure Init; virtual;
  public
    { Summary if aOwner is nil then means it create by editor not by animation
      exectuor. }
    { Description
    if it will start transition then 
    we will init it if any.
    }
    constructor Create(aOwner: PGRTransitionRec = nil); virtual;
    property Owner: PGRTransitionRec read FOwner;
  end;
  
{ Transition property }

  TGRTransitionRotation = (trNone, trRotate90, trRotate180, trRotate270);

  TGRCustomTransition = class(TComponent)
  private
    FEnabled: boolean;
    FPlayTime: integer;
    FResolution: integer;
    FTileCount: integer;
    FTransitionName: string;
    FRotation: TGRTransitionRotation;
    FBitmap: TBitmap;
    procedure SetResolution(const Value: integer);
    procedure SetTileCount(const Value: integer);
    procedure SetPlayTime(const Value: integer);
    procedure SetBitmap(const Value: TBitmap);
  protected
    FParam: TGRTransitionParam;
    FTransitionProc: TGRTransitionProcItem;

    procedure SetTransitionName(const Value: string);
    function GetTransitionProc: TGRTransitionProcItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property TransitionProc: TGRTransitionProcItem read GetTransitionProc;
    //the Transition parameters if any
    property Param: TGRTransitionParam read FParam;
  published
    //暂时使用bitmap作为Bitmap类型的转场
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property TransitionName: string read FTransitionName write FTransitionName;
    property Enabled: boolean read FEnabled write FEnabled default false;
    property Resolution: integer read FResolution write SetResolution default 1;
    property Rotation: TGRTransitionRotation read FRotation write FRotation default trNone;
    property TileCount: integer read FTileCount write SetTileCount default 1;
    property PlayTime: integer read FPlayTime write SetPlayTime default cDefaultPlayTime;
  end;


procedure ExecuteAnimation(ACanvas: TCanvas; AX, AY: integer; ASourceImage, ADestImage: TBitmap32; AAnimation: TGRCustomTransition);

function StartMultiAnimation(ACanvas: TCanvas; AX, AY: integer; ASourceImage, ADestImage: TBitmap32; AAnimation: TGRCustomTransition; AIndex: integer = -1): integer;
procedure StopMultiAnimation(AIndex: integer; DrawLastFrame: boolean = false);
function IsAnimating(AIndex: integer): boolean;

function ReserveIndex: integer;
procedure ReleaseReserved(AIndex: integer);

type

  TGRTransitionProcList = class(TList)
  private
    function GetProcs(Index: string): TGRTransitionProcItem;
    function GetItems(Index: Integer): TGRTransitionProcItem;
  public
    procedure Clear; override;
    property Procs[Index: string]: TGRTransitionProcItem read GetProcs; default;
    property Items[Index: Integer]: TGRTransitionProcItem read GetItems;
  end;

  ETransitionProcItemError = class(Exception);

  { Summary the Transition type procedute }
  TGRTransitionTypeProc = procedure(aTransitionRec: TGRTransitionRec;
    Percent: byte);

const
  TransitionProcList: TGRTransitionProcList = nil;
  TransitionList: TStrings = nil;  // All Transition list
  TransitionTypeList: TStrings = nil;


function GetTransitionList: TStrings;
function GetTransitionListComma: string;

procedure RegisterTransitionType(const aTypeName: string; const aProc: TGRTransitionTypeProc);
function FindTransitionType(aName: string): TGRTransitionTypeProc;

//internal used.
procedure RegisterTransition(
  const aName: string; 
  const aKind: string; 
  const aProc: TGRTransitionProc;
  const aTitle: string = ''; 
  const aParamClass: TGRTransitionParamClass = nil 
);


implementation {===============================================================}

uses
  GR_System
  , SimpleTimer
  ;

const
  cMaxTransitions = 200;
  cTimerInterval = 20;

var
  FTimer: TSimpleTimer;

type
  TGRTransitionExecutor = class
    {$IFDEF STATICEFFECTTIMER }
    T: TGRCounter;
    constructor Create;
    destructor Destroy; override;
    {$ENDIF}
    procedure Execute(Sender: TObject);
  end;

var
  FCurrentTransitions: array [0..cMaxTransitions] of TGRTransitionRec;

function ReserveIndex: integer;
var
  i: integer;
begin
  for i := Low(FCurrentTransitions) to High(FCurrentTransitions) do
    if (not FCurrentTransitions[i].Animating) and (not FCurrentTransitions[i].Reserved) then
    begin
      FCurrentTransitions[i].Reserved := true;
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure ReleaseReserved(AIndex: integer);
begin
  FCurrentTransitions[AIndex].Reserved := false;
end;

function FindIndex: integer;
var
  i: integer;
begin
  for i := Low(FCurrentTransitions) to High(FCurrentTransitions) do
    if (not FCurrentTransitions[i].Animating) and (not FCurrentTransitions[i].Reserved) then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function TimerEnabled: boolean;
var
  i: integer;
begin
  for i := Low(FCurrentTransitions) to High(FCurrentTransitions) do
    if FCurrentTransitions[i].Animating then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

function StartMultiAnimation(ACanvas: TCanvas; AX, AY: integer; ASourceImage, ADestImage: TBitmap32;
  AAnimation: TGRCustomTransition; AIndex: integer = -1): integer;
var
  MaskBitmap: TBitmap;
  Mask: TBitmap32;
  i, j: integer;
begin
  if AIndex = -1 then
    Result := FindIndex
  else
    Result := AIndex;
  if Result = -1 then Exit;

  with FCurrentTransitions[Result] do
  begin
    SourceImage := ASourceImage;
    DestImage := ADestImage;
    Transition := AAnimation;

    if SourceImage = nil then Exit;
    if SourceImage.Width * SourceImage.Height = 0 then Exit;

    { Check }
    if DestImage = nil then Exit;
    if DestImage.Width * DestImage.Height = 0 then Exit;

    { Select Transition }
    if Transition.TransitionName = SRandomSelection then
      ProcItem := TransitionProcList[TGRTransitionProcItem(TransitionProcList.Items[Random(TransitionProcList.Count)]).Name]
    else
      ProcItem := TransitionProcList[Transition.TransitionName];

    //if Transition.TransitionName <> SBitmap then
    if (ProcItem = nil) or (@ProcItem.Proc = nil) then Exit;

    ResultImage := TBitmap32.Create;
    ResultImage.SetSize(SourceImage.Width, SourceImage.Height);

    CurTime := 0.01;
    Animating := true;
    Drawing := false;
    Canvas := ACanvas;
    X := AX;
    Y := AY;

    if Assigned(ProcItem) and Assigned(ProcItem.ParamClass) then
    begin
      Param := ProcItem.ParamClass.Create(@FCurrentTransitions[Result]);
      Param.Init;
    end;

    FTimer.Enabled := TimerEnabled;
  end;
end;

procedure StopMultiAnimation(AIndex: integer; DrawLastFrame: boolean = false);
begin
  if AIndex = -1 then Exit;

  with FCurrentTransitions[AIndex] do
  begin
    if (DestImage = nil) or (SourceImage = nil) or (ResultImage = nil) then Exit;
    if not Animating then Exit;

    {$IFDEF COMPILER6}
    if Canvas.HandleAllocated and DrawLastFrame then
    {$ELSE}
    if DrawLastFrame then
    {$ENDIF}
    begin
      { Calc last frame }
      DestImage.DrawMode := dmBlend;
      DestImage.DrawTo(ResultImage, 0, 0);
      { Draw dest image }
      ResultImage.DrawTo(Canvas.Handle, X, Y);
    end;

    { Free image }
    ResultImage.Free;
    if Assigned(ProcItem) and Assigned(ProcItem.ParamClass) then
    begin
      FreeAndNil(Param);
    end;

    Animating := false;
    FTimer.Enabled := TimerEnabled;
  end;
end;

procedure DrawMultiAnimation(AIndex: integer);
var
  Percent: integer;
  LTransitionTypeProc: TGRTransitionTypeProc;
begin
  { Calc Transition frame }
  with FCurrentTransitions[AIndex] do
  begin
    if (DestImage = nil) or (SourceImage = nil) or (ResultImage = nil) then Exit;
    if not Animating then Exit;
    CurTime := CurTime + cTimerInterval;

    if Drawing then Exit;
    Drawing := true;
    try
      Percent := Round((CurTime / Transition.PlayTime) * 100);
      if Percent >= 100 then Percent := 100;

      { Copy source to result }
      SourceImage.DrawTo(ResultImage, 0, 0);

        LTransitionTypeProc := FindTransitionType(ProcItem.Kind);
        if Assigned(LTransitionTypeProc) then
          LTransitionTypeProc(FCurrentTransitions[AIndex], Percent);

      ResultImage.DrawTo(Canvas.Handle, X, Y);

      if Percent >= 100 then
      begin
        StopMultiAnimation(AIndex);
        Exit;
      end;
    finally
      Drawing := false;
    end;
  end;
end;

function IsAnimating(AIndex: integer): boolean;
begin
  Result := (AIndex >= 0) and FCurrentTransitions[AIndex].Animating;
end;

procedure ExecuteAnimation(ACanvas: TCanvas; AX, AY: integer; ASourceImage, ADestImage: TBitmap32; AAnimation: TGRCustomTransition);
var
  Index: integer;
begin
  Index := StartMultiAnimation(ACanvas, AX, AY, ASourceImage, ADestImage, AAnimation);
  while IsAnimating(Index) do
    Application.ProcessMessages;
end;

{$IFDEF STATICEFFECTTIMER }

constructor TGRTransitionExecutor.Create;
begin
  inherited;
  //T := TGRCounter.Create;
end;

destructor TGRTransitionExecutor.Destroy;
begin
  //T.Free;
  inherited;
end;

{$ENDIF}

procedure TGRTransitionExecutor.Execute(Sender: TObject);
var
  i: integer;
  {$IFNDEF STATICEFFECTTIMER }
  T: TGRCounter;
  {$ENDIF}
  LTime: single;
begin
  {$IFNDEF STATICEFFECTTIMER }
  //T := TGRCounter.Create;
  try
  {$ENDIF}
    T := StartCounter;

    for i := Low(FCurrentTransitions) to High(FCurrentTransitions) do
    begin
      DrawMultiAnimation(i);
    end;

    LTime := StopCounter(T);
  {$IFNDEF STATICEFFECTTIMER }
  finally
    //T.Free;
  end;
  {$ENDIF}

  { Add time to all effects }
  for i := Low(FCurrentTransitions) to High(FCurrentTransitions) do
    FCurrentTransitions[i].CurTime := FCurrentTransitions[i].CurTime + LTime;
end;

var
  FTransitionExecutor: TGRTransitionExecutor;

type
  PLongword = ^longword;

  PLongArray = ^TLongArray;
  TLongArray = array [0..0] of longword;

  PByteArray = ^TByteArray;
  TByteArray = array [0..0] of byte;
  


{$WARNINGS OFF}

procedure RegisterTransitionType(const aTypeName: string; const aProc: TGRTransitionTypeProc);
var
  i: integer;
  s: string;
begin
  s := UpperCase(aTypeName);
  for i := 0 to TransitionTypeList.Count - 1 do
  begin
    { Check by name }
    if TransitionTypeList[i] = s then
      raise ETransitionProcItemError.CreateFmt(rsTransitionTypeAlreadyExists, [aTypeName]);
    { Check by proc }
    if TransitionTypeList.Objects[i] = @aProc then
      raise ETransitionProcItemError.CreateFmt(rsTransitionTypeAlreadyExists, [aTypeName]);
  end;
  TransitionTypeList.AddObject(s, @aProc);
end;

function FindTransitionType(aName: string): TGRTransitionTypeProc;
var
  i: integer;
begin
  Result := nil;
  aName := UpperCase(aName);
  for i := 0 to TransitionTypeList.Count - 1 do
  begin
    if TransitionTypeList[i] = aName then
    begin
      Result := TGRTransitionTypeProc(TransitionTypeList.Objects[i]);
      break;
    end;
  end;
end;

procedure RegisterTransition(
  const aName: string; 
  const aKind: string; 
  const aProc: TGRTransitionProc;
  const aTitle: string = ''; 
  const aParamClass: TGRTransitionParamClass = nil 
);
var
  Item: TGRTransitionProcItem;
  i: integer;
begin
  if TransitionProcList = nil then
    TransitionProcList := TGRTransitionProcList.Create;

  for i := 0 to TransitionProcList.Count - 1 do
  begin
    { Check by name }
    if TGRTransitionProcItem(TransitionProcList.Items[i]).Name = LowerCase(AName) then
      raise ETransitionProcItemError.CreateFmt(rsTransitionAlreadyExists, [AName]);
    { Check by proc }
    if @TGRTransitionProcItem(TransitionProcList.Items[i]).Proc = @AProc then
      raise ETransitionProcItemError.CreateFmt(rsTransitionAlreadyExists, [AName]);
  end;

  Item := TGRTransitionProcItem.Create;
  {case AKind of
    pkFade: Item.Name := SFade + AName;
    pkSlide: Item.Name := SSlide + AName;
    pkManual: Item.Name := SManual + AName;
  end;}
  Item.Name := AName;
  Item.Kind := AKind;
  Item.Proc := AProc;
  Item.ParamClass := aParamClass;
  if aTitle <> '' then 
    Item.Title := aTitle
  else
    Item.Title := aKind + '-' + aName;

  TransitionProcList.Add(Item);

  TransitionList.Add(Item.Name);
end;

{ TGRTransitionProcList }

procedure TGRTransitionProcList.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    TGRTransitionProcItem(Items[i]).Free;

  inherited;
end;

function TGRTransitionProcList.GetItems(Index: Integer): TGRTransitionProcItem;
begin
  Result := TGRTransitionProcItem(inherited Items[index]);
end;

function TGRTransitionProcList.GetProcs(Index: string): TGRTransitionProcItem;
var
  i: integer;
  S: string;
begin
  { Equal }
  for i := 0 to Count - 1 do
  begin
    if LowerCase(TGRTransitionProcItem(Items[i]).Name) = LowerCase(Index) then
    begin
      Result := TGRTransitionProcItem(Items[i]);
      Exit;
    end;
  end;

  {
  for i := 0 to Count - 1 do
  begin
    S := LowerCase(TGRTransitionProcItem(Items[i]).Name);
    if Pos(SFade, S) > 0 then System.Delete(S, 1, Pos('-', S)+1);
    if Pos(SSlide, S) > 0 then System.Delete(S, 1, Pos('-', S)+1);
    if Pos(SManual, S) > 0 then System.Delete(S, 1, Pos('-', S)+1);

    if S = LowerCase(Index) then
    begin
      Result := TGRTransitionProcItem(Items[i]);
      Exit;
    end;
  end;
  }
  Result := nil;
end;



{ TransitionName list }

{procedure BuildTransitionList;
var
  i: integer;
begin
  TransitionList := TStringList.Create;

  TransitionList.Add(SRandomSelection);
  //TransitionList.Add(SBitmap);
  for i := 0 to TransitionProcList.Count - 1 do
    TransitionList.Add(TGRTransitionProcItem(TransitionProcList.Items[i]).Name);
end;
}
function GetTransitionListComma: string;
begin
  //if TransitionList = nil then BuildTransitionList;
  Result := TransitionList.CommaText;
end;

function GetTransitionList: TStrings; overload;
begin
  //if TransitionList = nil then BuildTransitionList;
  Result := TransitionList;
end;



{ TGRCustomTransition ================================================================}

constructor TGRCustomTransition.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmap := TBitmap.Create;
  FEnabled := false;
  FPlayTime := cDefaultPlayTime;
  FResolution := 1;
  FTileCount := 1;
  FTransitionName := SRandomSelection;
  FRotation := trNone;
end;

destructor TGRCustomTransition.Destroy;
begin
  FreeAndNil(FParam);
  FBitmap.Free;
  inherited;
end;

procedure TGRCustomTransition.Assign(Source: TPersistent);
begin
  if Source is TGRCustomTransition then
  begin
    FBitmap.Assign(TGRCustomTransition(Source).Bitmap);
    FEnabled := TGRCustomTransition(Source).Enabled;
    FPlayTime := TGRCustomTransition(Source).PlayTime;
    FResolution := TGRCustomTransition(Source).Resolution;
    FTileCount := TGRCustomTransition(Source).TileCount;
    FTransitionName := TGRCustomTransition(Source).TransitionName;
    FRotation := TGRCustomTransition(Source).Rotation;
  end
  else
    inherited ;
end;

function TGRCustomTransition.GetTransitionProc: TGRTransitionProcItem;
begin
  if FTransitionName = SRandomSelection then
  begin
    FTransitionProc := TransitionProcList.Items[Random(TransitionProcList.Count)];
    //if Assigned(FTransitionProc) and Assigned(FTransitionProc.ParamClass) then
      //FParam := FTransitionProc.ParamClass.Create;
  end
  else FTransitionProc := TransitionProcList[FTransitionName];
end;

procedure TGRCustomTransition.SetResolution(const Value: integer);
begin
  if FResolution <> Value then
  begin
    FResolution := Value;

    if FResolution < 1 then FResolution := 1;
    if FResolution > 10 then FResolution := 10;
  end;
end;

procedure TGRCustomTransition.SetTileCount(const Value: integer);
begin
  if FTileCount <> Value then
  begin
    FTileCount := Value;

    if FTileCount < 1 then FTileCount := 1;
    if FTileCount > 20 then FTileCount := 20;
  end;
end;

procedure TGRCustomTransition.SetTransitionName(const Value: string);
begin
  if FTransitionName <> Value then
  begin
    FTransitionProc := nil;
    FreeAndNil(FParam);
    FTransitionName := Value;
    if FTransitionName <> SRandomSelection then
      FTransitionProc := TransitionProcList[FTransitionName];
    if Assigned(FTransitionProc) and Assigned(FTransitionProc.ParamClass) then
      FParam := FTransitionProc.ParamClass.Create;
  end;
end;

procedure TGRCustomTransition.SetPlayTime(const Value: integer);
begin
  if FPlayTime <> Value then
  begin
    FPlayTime := Value;

    if FPlayTime <= 50 then FPlayTime := 50;
    if FPlayTime > 9000 then FPlayTime := 9000;
  end;
end;

procedure TGRCustomTransition.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

constructor TGRTransitionParam.Create(aOwner: PGRTransitionRec = nil);
begin
  inherited Create;
  FOwner := aOwner;
end;

procedure TGRTransitionParam.Init;
begin
end;

initialization
  //SigFx := SigFx;

  TransitionTypeList := TStringList.Create;
  RegisterClass(TGRCustomTransition);

  TransitionList := TStringList.Create;
  TransitionList.Add(SRandomSelection);

  Randomize;
  FillChar(FCurrentTransitions, SizeOf(FCurrentTransitions), 0);
  FTransitionExecutor := TGRTransitionExecutor.Create;
  FTimer := TSimpleTimer.CreateEx(cTimerInterval, FTransitionExecutor.Execute);
  //FTimer.OnTimer := FTransitionExecutor.Execute;
  //FTimer.Interval := cTimerInterval;
  //FTimer.Enabled := false;


finalization
  FTimer.Free;
  FTransitionExecutor.Free;

  if TransitionList <> nil then TransitionList.Free;
  TransitionList := nil;
  if TransitionProcList <> nil then TransitionProcList.Free;
  TransitionProcList := nil;
  
  FreeAndNil(TransitionTypeList);
end.
