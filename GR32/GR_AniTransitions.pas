//rename to GR_AniTransition
{ Summary the animation transition effects from One Image to another Image }
unit GR_AniTransitions;

interface

uses
  {$ifdef Debug}
  DbugIntf,
  {$endif} 
  Windows, Messages
  , Classes, SysUtils
  , Graphics
  //, Controls
  //, Forms
  //, Dialogs
  , GR32
  , GR_System
  //, GR_AniEffects
  , SimpleTimer
  ;  

resourcestring
  RSRandomSelection   = '[ RANDOM ] - Random selection';
  RSBitmap            = '[ BITMAP ] - Bitmap Animation';
  RSFade              = '[ FADE ] - ';
  RSSlide             = '[ SLIDE ] - ';
  RSManual            = '[ MANUAL ] - ';

const
  { WARNINGS ! Do not translate this strings }
  SRandomSelection   = 'RANDOM';
  SBitmap            = 'BITMAP';
  SFade              = 'FADE-';
  SSlide             = 'SLIDE-';
  SManual            = 'MANUAL-';

const
  cDefaultPlayTime = 400; //ms

type
  TGRCustomTransition = class;

  PGRMatrixFade = ^TGRMatrixFade;
  TGRMatrixFade = array[0..0] of Byte;

  TGRTransitionProcFade = procedure (var Matrix: TGRMatrixFade; Width, Height: integer; Percent: byte);

  TGRPointSlide = record
    Alpha: Byte;
    X, Y: SmallInt;
  end;

  PGRMatrixSlide = ^TGRMatrixSlide;
  TGRMatrixSlide = array[0..0] of TGRPointSlide;

  TGRTransitionProcSlide = procedure (var Matrix: TGRMatrixSlide; Width, Height: integer; Percent: byte);

  TGRTransitionProcManual = procedure (SourceImage, DestImage: TBitmap32; 
    Animation: TGRCustomTransition; Percent: byte);
  
  TGRTransitionProc = procedure;
  TGRTransitionRotation = (trNone, trRotate90, trRotate180, trRotate270);
  TGRTransitionProcKind = (pkFade, pkSlide, pkManual);
  TGRTransitionParam = Class;
  TGRTransitionParamClass = Class of TGRTransitionParam;

  { Sumary the Transition of Animation Item}
  //For Register the transition animation 
  TGRTransitionProcItem = object
  protected
    FKind: TGRTransitionProcKind;
    FProc: TGRTransitionProc;
    FName: string;
    FTitle: string;
    FParamClass: TGRTransitionParamClass;
  public
    property Kind: TGRTransitionProcKind read FKind write FKind;
    property Proc: TGRTransitionProc read FProc write FProc;
    //for index
    property Name: string read FName write FName;
    //for display
    property Title: string read FTitle write FTitle;
    property ParamClass: TGRTransitionParamClass read FParamClass write FParamClass;
  end;

  TGRTransitionRec = record
    Index: integer;
    Animating: boolean;
    Drawing: boolean;
    Reserved: boolean;
    CurTime: single;
    DestImage, SourceImage, ResultImage: TBitmap32;
    Canvas: TCanvas;
    X, Y: integer;
    ProcItem: TGRTransitionProcItem;
    //for use the Globle Params
    Transition: TGRCustomTransition;
    //for use the transition param if any
    Param: TGRTransitionParam;
  end;

  TGRTransitionParam = class(TObject)
  private
    FOwner: PGRTransitionRec;
  protected
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
  
  TGRCustomTransition = class(TObject)
  private
    FEnabled: Boolean;
    FParam: TGRTransitionParam;
    FPlayTime: Integer;
    FResolution: Integer;
    FRotation: TGRTransitionRotation;
    FTileCount: Integer;
    FTransitionName: string;
    function GetTransitionProc: TGRTransitionProcItem;
    procedure SetTransitionName(const Value: string);
  protected
    FTransitionProc: TGRTransitionProcItem;
  public
    constructor Create;
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Param: TGRTransitionParam read FParam;
    property PlayTime: Integer read FPlayTime write FPlayTime default
      cDefaultPlayTime;
    property Resolution: Integer read FResolution write FResolution default 1;
    property Rotation: TGRTransitionRotation read FRotation write FRotation;
    property TileCount: Integer read FTileCount write FTileCount default 1;
    { Description
    转场动画的全部设定在这里。
    }
    property TransitionName: string read FTransitionName write
      SetTransitionName;
    property TransitionProc: TGRTransitionProcItem read GetTransitionProc;
  end;
  
  { Summary cllect the Transition ProcItem }
  TGRTransitionProcList = class(TList)
  private
    function GetItems(Index: Integer): TGRTransitionProcItem;
    function GetProcs(Index: String): TGRTransitionProcItem;
  public
    property Items[Index: Integer]: TGRTransitionProcItem read GetItems;
    property Procs[Index: String]: TGRTransitionProcItem read GetProcs; default;
  end;
  

const
  ProcList: TGRTransitionProcList = nil;
  EffectList: TStrings = nil;  // All effects list

function GetEffectList: TStrings;
function GetEffectListComma: string;

procedure ExecuteAnimation(ACanvas: TCanvas; AX, AY: integer; ASourceImage, ADestImage: TBitmap32; AAnimation: TGRCustomTransition);

function StartMultiAnimation(ACanvas: TCanvas; AX, AY: integer; ASourceImage,
  ADestImage: TBitmap32; ATransition: TGRCustomTransition; AIndex: integer =
  -1): integer;
procedure StopMultiAnimation(AIndex: integer; DrawLastFrame: boolean = false);
function IsAnimating(AIndex: integer): boolean;

function ReserveIndex: integer;
procedure ReleaseReserved(AIndex: integer);

procedure RegisterTransition(const AName: string; const AKind: TGRTransitionProcKind; 
  const AProc: TGRTransitionProc; 
  const aTitle: string = ''; 
  const aParamClass: TGRTransitionParamClass = nil 
);

implementation

const
  cTimerInterval = 20;
  cMaxTransitions = 200;

type
  TGRTransitionExecutor = class(TObject)
  public
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

function StartMultiAnimation(ACanvas: TCanvas; AX, AY: integer; ASourceImage,
  ADestImage: TBitmap32; ATransition: TGRCustomTransition; AIndex: integer =
  -1): integer;
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
    SourceImage := aSourceImage;
    DestImage := aDestImage;
    Transition := aTransition;

    if SourceImage = nil then Exit;
    if SourceImage.Width * SourceImage.Height = 0 then Exit;

    if DestImage = nil then Exit;
    if DestImage.Width * DestImage.Height = 0 then Exit;

    { Select Transition }
    ProcItem := Transition.TransitionProc;

    if Transition.TransitionName <> SBitmap then
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

procedure BuildEffectList;
var
  i: integer;
begin
  EffectList := TStringList.Create;

  EffectList.Add(SRandomSelection);
  //EffectList.Add(SBitmap);
  for i := 0 to ProcList.Count - 1 do
    EffectList.Add(ProcList.Items[i].Name);
end;

function GetEffectListComma: string;
begin
  if EffectList = nil then BuildEffectList;
  Result := EffectList.CommaText;
end;

function GetEffectList: TStrings; overload;
begin
  if EffectList = nil then BuildEffectList;
  Result := EffectList;
end;


constructor TGRTransitionParam.Create(aOwner: PGRTransitionRec = nil);
begin
  inherited Create;
  FOwner := aOwner;
end;

procedure TGRTransitionParam.Init;
begin
end;

constructor TGRCustomTransition.Create;
begin
  inherited Create;
  FTileCount := 1;
  FPlayTime := cDefaultPlayTime;
  FResolution := 1;
end;

destructor TGRCustomTransition.Destroy;
begin
  FreeAndNil(FParam);
  inherited Destroy;
end;

function TGRCustomTransition.GetTransitionProc: TGRTransitionProcItem;
begin
  if FTransitionName = SRandomSelection then
  begin
    FTransitionProc := ProcList.Items[Random(ProcList.Count)];
    //if Assigned(FTransitionProc) and Assigned(FTransitionProc.ParamClass) then
      //FParam := FTransitionProc.ParamClass.Create;
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
      FTransitionProc := ProcList[FTransitionName];
    if Assigned(FTransitionProc) and Assigned(FTransitionProc.ParamClass) then
      FParam := FTransitionProc.ParamClass.Create;
  end;
end;

function TGRTransitionProcList.GetItems(Index: Integer): TGRTransitionProcItem;
begin
  Result := TGRTransitionProcItem(inherited Items[index]);
end;

function TGRTransitionProcList.GetProcs(Index: String): TGRTransitionProcItem;
var
  I: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if LowerCase(Items[i].Name) = LowerCase(Index) then
    begin
      Result := Items[i];
      Exit;
    end;
  end;
end;

procedure TGRTransitionExecutor.Execute(Sender: TObject);
var
  I: Integer;
  LT: TGRCounter;
  LTime: Single;
begin
  StartCounter(LT);
  
  for i := Low(MultiAniArray) to High(MultiAniArray) do
  begin
    DrawMultiAnimation(i);
  end;
  
  LTime := StopCounter(LT);
  
  { Add time to all effects }
  for i := Low(MultiAniArray) to High(MultiAniArray) do
    MultiAniArray[i].CurTime := MultiAniArray[i].CurTime + LTime;
end;


var
  FExecutor: TGRTransitionExecutor;
  FTimer: TSimpleTimer;
initialization
  Randomize;
  FillChar(FCurrentTransitions, SizeOf(FCurrentTransitions), 0);

  FExecutor := TGRTransitionExecutor.Create;
  FTimer := TSimpleTimer.CreateEx(cTimerInterval, FExecutor.Execute);

finalization
  FreeAndNil(FTimer);
  FreeAndNil(FExecutor);
end.
