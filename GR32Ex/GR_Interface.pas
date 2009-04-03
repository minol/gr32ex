(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is GR_Interface
 *
 * The Initial Developer of the Original Code is Roman Gudchenko
 *  Portions created by Roman Gudchenko(c)  (mailto:roma@goodok.ru) are Copyright (C) 2002
 *  Portions created by Riceball LEE are Copyright (C) 2004-2007
 *
 * All Rights Reserved.
 *
 * Contributor(s):
 *  Riceball LEE
 *
 * ***** END LICENSE BLOCK ***** *)
unit GR_Interface;
{
  DEFINE rus = my native russian
  DEFINE eng = my poor english
}

{$I Setting.inc} // Graphic32 (www.g32.org) options

//----------------------------------------------------------------------------------}
//
//  rus :  модуль содержит полезняшки и удобства для библиотеки GR32 (www.g32.org)
//
//  eng :  some usefull routines and additional buildups to GR32 library (www.g32.org)
//         that extends its drawning feature:
//
//         -- Curve and figures
//           - Bezier Curve d(qubic and quadric)
//           - arc- pie- segment- figures ( elliptic and rotated, transformed also)
//           - ellipse and rotated ellipse
//           - rounded polygones (rotated also)
//         -- True Type Font text drawning:
//            - true type font text (antialised, angled and transformed)
//            - text fitted in Bezier-curve
//         -- Splines:
//           - cardinal spline
//           - TCB spline
//           - normalized cardinal spline
//
//  compiler: Delphi 7.0; Delphi 6.0; Delphi 5.0;
//
//  author: Roman Gudchenko(c)  mailto:roma@goodok.ru
//
//  first published in 13.01.2002
//  last updated in  28.10.2002
//
//
//  you can find last version of this unit with test programs
//  on  http://www.goodok.ru/downloads/G32_Interface.zip  (zip, size about 50K)
//
//
{------------------------------------------------------------------------------------}

{!!! in plans:
 1. Rename tBitmap32Ex class to tCanvas32 that will be use tBitmap32.
 2. I m going use float point arithmetic.
 3. Complex regions combining and clipping.
}

{
 history
 -------
 version

 version 0.18
   + RenderTextExW by riceball for Unicode
   + gRenderTextExW by riceball for Unicode
   + DrawGlyphW by riceball for Unicode
   + RenderFittedTextW by riceball for Unicode
   * rename unit name to GR_Interface

 version 0.17
    1. new:  added gSpline_TangentNorm function:
         The xPoints array must be sorted by x-coordinate.
         In this case for every x value there will be only one point in curve (or nothing).
         It is not ideal qubic spline,  but appropriated
         ( for Mattias Andersson's LUT control )
    2. bugfix: RenderTextEx function redisigned, now width of text produced by RenderTextEx
       and TextOut functions are equal.


 version 0.16
    1. Added new function to TCB-spline plotting
    2. Added new functions to draw custom transformed figures (arc, pie, ellipse );
      So, for example,  if you want to draw many figures with constant rotation
      angle you can precalculate affine transformation matrix.
      This function names gEllipseT, gArcET, gPieET, gSegET for common transformation matrix
      and gArcET_СenterRelative for transformation relyative center of figure.

 version 0.15  (02.06.2002)

    1. bug fixed in proc.  CalculateRoundsArc
    2. Added new functions for cardinal splines plotting:
       gCardinalSpline
       gPolyCardianalSpline
    3. Added function for drawning point as symbol (gDrawSymol)
    4. Some speed optimization of BuildGlyphPolygon procedure was made;

 version 0.14  (29.05.2002)
    1. some speed optimizations of tBitmap.Polygone and tBitmap.RenderTextEx have been made
       by Alexander Muylaert [amuylaert_gelein@hotmail.com]; they are replace those part of code
       marked  $DEFINE G32i_ver013

    2. I have implemented gPolygonRounded and gPolyPolygoneRounded routines wich
       draw curves based on polygone with rounds.

 version 0.13  (27.05.2002)

   1. Added  functions to draw arc- pie- and segment figures;
      all of those figures can draw as elliptic and rotated (including rotated ellipse);
   2. I have tried to implement to draw rounded polygone by it work only for convex one;
      (i will reform youself soon - just correct proc. CalculateRoundsArc)
   3. Now you can to know curve length and position and tangent of any point at curve;
      you can apply drawning routines to tBitmap32 not only my tBitmap32Ex, exclude
      RenderFittedText routines

 version 0.12  (20.05.2002)

    i don't remember :)

}


interface

uses GR32, GR32_Polygons, Graphics, Windows, Classes, GR32_Transforms, Dialogs;


{ --- defines and types ---}

{ tPolygonDrawOptions }

  {eng: this type usefull to point draw method for control quality and speed }

  {eng: use pdoFloat to draw really antialised poligones, and
        pdoFilling to draw antialised filled poligones and curves  }
const
  pdoAntialising = 1; { eng: simple antialising }
  pdoFloat       = 2 or pdoAntialising;
  pdoFilling     = 4;
  pdoFastFilling = 8 or pdoFilling or pdoFloat; { not precisely and quality but fast }

  //UNICODE_SUPPORTS = True;

type
  tPolygonDrawOptions = longint;

{ rus: коэффициэнты, определяющие качество аппроксимации кривых }
{ eng: quality of curve approximation }
const
   { eng: for qubic bezier curve }
   {The minimal value of conditional length rogulki cubic curve Bezier, than is more exact than themes aproksimiruetsja krivulka less.}
   Bezier3SegmentMinLengthInPixel : word = 2;

   Bezier3SegmentMinLengthInPixelSQR : word = 4;
   { eng: for qubic bezier curve to site fitted text}
   Bezier3SegmentMinLengthInPixel_2 : word = 2; { точность аппроксимации кривой при расположении на ней текста }

   { eng: for quadric bezier curve   }
   Bezier2SegmentMinLengthInPixel : word = 2; { rus: минимальное значение условной длины рогульки квадратичной кривой Bezier,
                                                чем меньше тем точнее апроксимируется кривулька }
   FittedText_SpacingFactor : double = 1.0;

   eps_Fixed : GR32.tFixed = 100;             { "machine epsilon"}

  { -- pice of mathematics -- }

  {$IFNDEF COMPILER6}
  function Cosecant(const X: Extended): Extended;

  type
    TValueSign = -1..1;

  const
    NegativeValue = Low(TValueSign);
    ZeroValue = 0;
    PositiveValue = High(TValueSign);

  function Sign(const AValue: Integer): TValueSign; overload;
  function Sign(const AValue: Int64): TValueSign; overload;
  function Sign(const AValue: Double): TValueSign; overload;
  {$ENDIF}

  {-- some precalculated constants: -- }
  const

    div65536        = 0.0000152587890625;    { 1/65536}
    HalfPixel       = 32768;
    PixelInFixed    = 65536;

    Pi      : double      = 3.14159265358979323846;               { Pi }
    Pi2     : double      = 6.28318530717958647693;               { 2*Pi }
    PIDIV2  : double      = 1.57079632679489661923;               { Pi/2 }
    PIDIV4  : double      = 0.785398163397448309615;              { Pi/4 }
    div2    : double      = 0.5;


    Ratio3     : double         =  1/3;
    Ratio2div3 : double         =  2/3;
    Ratio4div3 : double         =  4/3;
    Ratio6     : double         =  1/6;


    EllipseToCurveCoeff_4 : double    = 0.2761423749153966992011258161395;

    EllipseToCurveCoeff_2 : double    = 0.5522847498307933984022516322796; { (4/3)(1-Cos(Pi/4))/(Sin(Pi/4)}

    EllipseToCurveCoeff_2inv : double = 0.447715250169206601597748367721; { 1 - EllipseToCurveCoeff_2}




const
  Identity_mat2 : tmat2 = ( eM11 : (fract : 0; value : 1);
                            eM12 : (fract : 0; value : 0);
                            eM21 : (fract : 0; value : 0);
                            eM22 : (fract : 0; value : 1); );

  VertFlip_mat2 : tmat2 = ( eM11 : (fract :  0; value : 1);
                            eM12 : (fract :  0; value : 0);
                            eM21 : (fract :  0; value : 0);
                            eM22 : (fract :  0; value : -1); );

  VertFlipMatrix: TFloatMatrix = (
    (1, 0, 0),
    (0, -1, 0),
    (0, 0, 1));

  { преобразование G32 матрицы в Win32 API матрицу преобразования }
  { convertation g32 float matrix to win32 API matrix }
  function FloatMatrixToMat2(const xMat : tFloatmatrix) : TMAT2;
  function Mat2ToFloatMatrix(xMat : tMat2) : tFloatMatrix;

  { other convertation functions }

  function WinFixToFixed(x : _FIXED) : GR32.TFixed;
  function FixedToWinFix(x : GR32.TFixed) : _Fixed;

  {$IFNDEF OPTIMIZE_CALLFUNCTIONS}
  function FloatToWinFix(x : single) : _FIXED;
  function WinFixToFloat(x : _Fixed) : double;
  {$ENDIF}

  { some useful functions:  }
  //function FixedRect(const xR : tRect) : tFixedRect; overload;
  //function FixedRect(const xLeft, xTop, xRight, xBottom : GR32.tFixed) : tFixedRect; overload;
  function IsNullRect(const xR : tFixedRect) : boolean;


  function MiddleLine(const p1, p2 : tFixedPoint) : tFixedPoint; overload;
  function PointsAreEqual(const p1, p2 : GR32.tFixedPoint) : boolean;
  function Distance(const p1, p2 :GR32.tFixedPoint) : GR32.tFixed; overload;
  function Distance(const p1x, p1y, p2x, p2y : double) : double; overload;
  function SqrDistance(const p1, p2 :GR32.tFixedPoint) : GR32.tFixed; { sqr of distance  }
  function Norm1(const x1, y1, x2, y2: integer) : integer; overload;
  function Norm1(const p1, p2 : tFixedPoint) : GR32.tFixed; overload;
  procedure RotateArrayOfFixedPoint(var xPoints : TArrayOfFixedPoint; const xCenter : tFixedPoint; const xAngle : double);
  procedure TransformArrayOfFixedPoint(var xPoints : TArrayOfFixedPoint; const xAT : TFloatMatrix);
  function TransformFloatPoint(const xP : tFloatPoint; const xAT : TFloatMatrix) : tFloatPoint;
  { --all valuations asume then segment a very small (about Bezier3SegmentMinLengthInPixel size )}
  { qubic bezier segment conditional length in norm2 ( more exacttly of all other, but slower) }
  function SegmentConditionalLengthQ3N2(const p1, p2, p3, p4 : tFixedPoint) : GR32.tFixed; { оценка длины сегмента кубической кривой}
  { qadric bezier segment conditional length in norm1 (supremum valuation) }
  function SegmentConditionalLengthQ2N1Sup(const x0, x1, x2 : tFixedPoint) : GR32.tFixed;
  { qadric bezier segment conditional length in norm2 (supremum valuation) }
  function SegmentConditionalLengthQ2N2Sup(const x0, x1, x2 : tFixedPoint) : GR32.tFixed;
  { qubic bezier segment conditional length in norm1 (supremum valuation) }
  function SegmentConditionalLengthQ3N1Sup(const x0, x1, x2, x3 : tFixedPoint) : GR32.tFixed;
  { qubic bezier segment conditional length in norm2 (supremum valuation) }
  function SegmentConditionalLengthQ3N2Sup(const x0, x1, x2, x3 : tFixedPoint) : GR32.tFixed;
  { qubic bezier segment conditional curvatre in norm1 }
  function SegmentConditionalCurvatureQ2(const x0, x1, x2 : tFixedPoint) :GR32.tFixed;
  { return position and tangent valuations of point, sited on xLength among segment - just interpolation of line}
  procedure gGetPointPositionValuationAtSegment(const p1, p2 : GR32.tFixedPoint;
                                               const xLength : GR32.tFixed;
                                               out xPathX, xPathY: GR32.tFixed;
                                               out xAngle : double);

  { return qubic bezier length with curve approximation }
  function gGetCurveLength(const xCurve: tArrayOfFixedPoint) : GR32.tFixed;


  { return position and tangent of point, sited on xLength among curve }
  procedure gGetPointAtCurve(const xCurve: tArrayOfFixedPoint;
                            const xLength : GR32.tFixed;
                            out xPathX, xPathY : GR32.tFixed; out xAngle : double
                            );

  { return position and tangent of point, sited on xLength among curve for massive calling to don't approximate curve every time }
  function gGetPointAtCurveEx(const xStartSegmentInd : integer;
                              const xStartLength : GR32.tFixed;
                              const xCurve: tArrayOfFixedPoint;
                              const xLength : GR32.tFixed;
                              out xPathX, xPathY : GR32.tFixed; out xAngle : double;
                              const xApproxNeed : boolean = true
                              ):integer;

  procedure gRenderTextEx(xBitmap : tBitmap32;
                                   const xHFont : HFont;
                                   const xLeft, yBottom: GR32.tFixed;
                                   const xText: string;
                                   const xColor : tColor32;
                                   const xOptions : tPolygonDrawOptions;
                                   const xTransformMatrix : TFloatMatrix);

  procedure gRenderTextExW(xBitmap : tBitmap32;
                                   const xHFont : HFont;
                                   const xLeft, yBottom: GR32.tFixed;
                                   const xText: WideString;
                                   const xColor : tColor32;
                                   const xOptions : tPolygonDrawOptions;
                                   const xTransformMatrix : TFloatMatrix);

{ -- following functions realized for using  directly on TBitmap32, not tBitmap32Ex  -- }

type
  tSymbolKind = (skCircle,
                 skSquare,
                 skTriangle,
                 skPlus,     { + }
                 skX,        { x }
                 skStar      { * }
                 );

  type

  tPen32Style = (gr_psSolid,
                 gr_psClear,      { reserved }
                 gr_psDash,       { reserved }
                 gr_psDot,        { reserved }
                 gr_psDashDot,    { reserved }
                 gr_psDashDotDot { reserved }
                 );

  tPen32Data = record
    Color: tColor32;
    Width: GR32.tFixed;
    Style: tPen32Style;
    EdgeSharpness : single;
  end;

  tPen32 = class(TPersistent)
  private
    function GetColor: tColor32;
    function GetStyle: tPen32Style;
    function GetWidth: GR32.tFixed;
    procedure SetColor(const Value: tColor32);
    procedure SetStyle(const Value: tPen32Style);
    procedure SetWidth(const Value: GR32.tFixed);
    function GetEdgeSharpness: single;
    procedure SetEdgeSharpness(const Value: single);
  protected
    fPenData : tPen32Data;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;

    property Color : tColor32 read GetColor write SetColor;
    property Width : GR32.tFixed read GetWidth write SetWidth;
    property Style : tPen32Style read GetStyle write SetStyle;
    property EdgeSharpness : single read GetEdgeSharpness write SetEdgeSharpness;
  end;


const

  gr32_Pen32Date_Default : tPen32Data = (Color: clBlue32;
                                     Width: PixelInFixed;
                                     Style: gr_psSolid;
                                     EdgeSharpness : 1
                                     );



type


  TBrush32Style = (gr_bsSolid,
                   gr_bsClear,         { reserved }
                   gr_bsHorizontal,    { reserved }
                   gr_bsVertical,      { reserved }
                   gr_bsFDiagonal,     { reserved }
                   gr_bsBDiagonal,     { reserved }
                   gr_bsCross,         { reserved }
                   gr_bsDiagCross      { reserved }
                  );

  tBrush32Data = record
    Color: tColor32;
    Bitmap: tBitmap32;                 { reserved }
    Style: tBrush32Style;
  end;

  tBrush32 = class(TPersistent)
  private
    function GetBitmap: tBitmap32;
    function GetColor: tColor32;
    function GetStyle: tBrush32Style;
    procedure SetBitmap(const Value: tBitmap32);
    procedure SetColor(const Value: tColor32);
    procedure SetStyle(const Value: tBrush32Style);
  protected
    fBrushData : tBrush32Data;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    property Color  : tColor32 read GetColor write SetColor;
    property Bitmap : tBitmap32 read GetBitmap write SetBitmap;
    property Style  : tBrush32Style read GetStyle write SetStyle;
  end;

const

  gr32_Brush32Date_Default : tBrush32Data =   ( Color: clWhite32;
                                                Bitmap: nil;
                                                Style: gr_bsSolid);



  procedure gDrawSymbol(xBitmap : tBitmap32;
                        const xP      : GR32.tFixedPoint;
                        const xSymbol : tSymbolKind;
                        const xSize   : GR32.tFixed;
                        const xColor  : tColor32;
                        const xOptions : tPolygonDrawOptions);


  procedure gDrawSymbols(xBitmap : tBitmap32;
                         const xPoints : tArrayOfFixedPoint;
                         const xSymbol : tSymbolKind;
                         const xSize   : GR32.tFixed;
                         const xColor  : tColor32;
                         const xOptions : tPolygonDrawOptions);
  { just call one of GR32.Poligon function according options }
  procedure gPolygon(Bitmap: TBitmap32;
                     const Points: TArrayOfFixedPoint;
                     const Color: TColor32;
                     const Options : tPolygonDrawOptions;
                     const Closed: Boolean;
                     const FillMode: TPolyFillMode = pfAlternate);

  procedure gPolygon_Styled(xBitmap: TBitmap32;
                       const xPoints: TArrayOfFixedPoint;
                       const xClosed: Boolean;
                       const xPenData : tPen32Data;
                       const xBrushData : tBrush32Data;
                       const xAntialised : boolean = true;
                       const xFillMode: TPolyFillMode = pfAlternate);


  { just call one of GR32.Poligon function according options }
  procedure gPolyPolygon(xBitmap: TBitmap32;
                     const xPoints: TArrayOfArrayOfFixedPoint;
                     const xColor: TColor32;
                     const xOptions : tPolygonDrawOptions;
                     const xClosed: Boolean;
                     const xFillMode: TPolyFillMode = pfAlternate);

  procedure gPolyPolygon_Styled(xBitmap: TBitmap32;
                       const xPoints: TArrayOfArrayOfFixedPoint;
                       const xClosed: Boolean;
                       const xPenData : tPen32Data;
                       const xBrushData : tBrush32Data;
                       const xAntialised : boolean = true;
                       const xFillMode: TPolyFillMode = pfAlternate);


  procedure gPolyBezier(Bitmap: TBitmap32;
                       const Points: TArrayOfFixedPoint;
                       const Color: TColor32;
                       const Options : tPolygonDrawOptions;
                       const Closed: Boolean;
                       const FillMode: TPolyFillMode = pfAlternate);

  procedure gPolyBezier_Styled(xBitmap: TBitmap32;
                       const xPoints: TArrayOfFixedPoint;
                       const xClosed: Boolean;
                       const xPenData : tPen32Data;
                       const xBrushData : tBrush32Data;
                       const xAntialised : boolean = true;
                       const xFillMode: TPolyFillMode = pfAlternate);



  procedure gPolyPolyBezier(Bitmap: TBitmap32;
                     const Points: TArrayOfArrayOfFixedPoint;
                     const Color: TColor32;
                     const Options : tPolygonDrawOptions;
                     const Closed: Boolean;
                     const FillMode: TPolyFillMode = pfAlternate);

  { build and draw curve wich implement rounded polygone based
    on xPoints polygone with round radius }
  procedure gPolygonRounded(xBitmap: TBitmap32;
                     const xPoints: TArrayOfFixedPoint;
                     const xRadius : GR32.tFixed;
                     const xColor: TColor32;
                     const xOptions : tPolygonDrawOptions;
                     const xClosed: Boolean;
                     const xFillMode: TPolyFillMode = pfAlternate);

  procedure gPolyPolygonRounded(xBitmap: TBitmap32;
                     const xPoints: TArrayOfArrayOfFixedPoint;
                     const xRadius : GR32.tFixed;
                     const xColor: TColor32;
                     const xOptions : tPolygonDrawOptions;
                     const xClosed: Boolean;
                     const xFillMode: TPolyFillMode = pfAlternate);

  { fill all bitmap with xColor exept xRect area }
  procedure gRectangleHole(xBitmap : tBitmap32;
                           const xRect : tFixedRect;
                           const xColor : tColor32;
                           const xOptions : tPolygonDrawOptions);

  { draw simple ellipse }
  procedure gEllipse(xBitmap : tBitmap32;
                     const xRect : TFixedRect;
                     const xColor: TColor32;
                     const xOptions : tPolygonDrawOptions = pdoFloat);

  procedure gEllipse_Styled(const xBitmap : tBitmap32;
                          const xRect : TFixedRect;
                          const xPenData : tPen32Data;
                          const xBrushData : tBrush32Data;
                          const xAntialised : boolean = true);


  { draw rotated ellipse }
  procedure gEllipseRotated(xBitmap : tBitmap32;
                            const xCenter : tFixedPoint;
                            const xA, xB : GR32.tFixed; { if xAngle = 0 then A <-> width and xB <-> Height }
                            const xAngle : double;      { value in radians }
                            const xColor : tColor32;
                            const xOptions : tPolygonDrawOptions = pdoFloat);

  { draw transformed ellipse with xAT affine tranformation matrix }
  procedure gEllipseT(xBitmap : tBitmap32;
                            const xCenter : tFixedPoint;
                            const xA, xB : GR32.tFixed; { if xAngle = 0 then A <-> width and xB <-> Height }
                            const xAT : TFloatMatrix;  { affine transformation matrix }
                            const xColor : tColor32;
                            const xOptions : tPolygonDrawOptions = pdoFloat);

  { TODO : procedure gEllipseTransformed }

  { draw arc; if xOptions = Fill then arc becames filled segment }
  procedure gArc(xBitmap : tBitmap32;
                 const xCenter : tFixedPoint;
                 const xR : GR32.tFixed;
                 const xStartAngle, xEndAngle : double;
                 const xColor : tColor32;
                 const xOptions : tPolygonDrawOptions = pdoFloat);

  { draw elliptic arc }
  procedure gArcElliptic(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);

  { draw elliptic and rotated arc }
  procedure gArcER(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xRotAngle : double;
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);

  { draw elliptic and transformed arc }
  procedure gArcET(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xAT : TFloatMatrix; { affine transformation matrix }
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);

  { draw elliptic and transformed arc }
  procedure gArcET_CenterRelative(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xAT : TFloatMatrix; { affine transformation matrix }
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);


  { TODO : procedure gArcEllipticeTransformed }

  { draw segment; segment is closed Arc }
  procedure gSegment(xBitmap : tBitmap32;
                 const xCenter : tFixedPoint;
                 const xR : GR32.tFixed;
                 const xStartAngle, xEndAngle : double;
                 const xColor : tColor32;
                 const xOptions : tPolygonDrawOptions = pdoFloat);
  { draw elliptic segment }
  procedure gSegmentElliptic(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);

  { draw elliptic and rotated segment for monsters }
  procedure gSegmentER(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xRotAngle : double;
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);

  { draw elliptic and transformed segment }
  procedure gSegmentET(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xAT : TFloatMatrix; { affine transformation matrix }
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);




  { draw pie - figure ; pie-figure is arc connected with center }
  procedure gPie(xBitmap : tBitmap32;
                 const xCenter : tFixedPoint;
                 const xR : GR32.tFixed;
                 const xStartAngle, xEndAngle : double;
                 const xColor : tColor32;
                 const xOptions : tPolygonDrawOptions = pdoFloat);

  procedure gPieElliptic(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double;
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);

  { draw elliptic rotated pie }
  procedure gPieER(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xRotAngle : double;
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);


  { draw elliptic and transformed pie }
  procedure gPieET(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xAT : TFloatMatrix; { affine transformation matrix }
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);

  procedure gPieET_CenterRelative(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xAT : TFloatMatrix; { affine transformation matrix }
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);

  { TODO : procedure gPieET  }


  { draw rounded rectangle }
  procedure gRectangleRounded(xBitmap : tBitmap32;
                                     const xRect : TFixedRect;
                                     const xR    : GR32.TFixed;
                                     const xColor : tColor32;
                                     const xOptions : tPolygonDrawOptions = pdoFloat);

  { draw rounded and rotated rectangle }
  procedure gRectangleRR(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xR     : GR32.tFixed;
                         const xAngle : double;
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);


  { build bezier curve to realize cardinal spline based on xPoints;
    if xTension = 1, simple poligone produced;
    if xTension = 0, common spline prodused;
  }
  procedure gCardinalSpline(xBitmap : tBitmap32;
                            const xPoints : tArrayOfFixedPoint;
                            const xTension : double;
                            const xColor : tColor32;
                            const xClosed : boolean;
                            const xOptions : tPolygonDrawOptions = pdoFloat);

  procedure gPolyCardinalSpline(xBitmap : tBitmap32;
                            const xPoints : tArrayOfArrayOfFixedPoint;
                            const xTension : double;
                            const xColor : tColor32;
                            const xClosed : boolean;
                            const xOptions : tPolygonDrawOptions = pdoFloat);
                            
  { build and draw qubic bezier curve to realize common TCB spline based ib xPoints;
    if xContinuity and xBias equals zero then TCB spline becomes cardinal spline }
  procedure gTCBSpline(xBitmap : tBitmap32;
                            const xPoints : tArrayOfFixedPoint;
                            const xTension : double;
                            const xContinuity : double;
                            const xBias : double;
                            const xColor : tColor32;
                            const xClosed : boolean;
                            const xOptions : tPolygonDrawOptions = pdoFloat);

  procedure gPolyTCBSpline(xBitmap : tBitmap32;
                            const xPoints : tArrayOfArrayOfFixedPoint;
                            const xTension : double;
                            const xContinuity : double;
                            const xBias : double;
                            const xColor : tColor32;
                            const xClosed : boolean;
                            const xOptions : tPolygonDrawOptions = pdoFloat);

  procedure gSpline_LengthNorm(xBitmap : tBitmap32;
                            const xPoints : tArrayOfFixedPoint;
                            const xTension : double;
                            const xColor : tColor32;
                            const xClosed : boolean;
                            const xOptions : tPolygonDrawOptions = pdoFloat);



  procedure gPolySpline_LengthNorm(xBitmap : tBitmap32;
                            const xPoints : tArrayOfArrayOfFixedPoint;
                            const xTension : double;
                            const xColor : tColor32;
                            const xClosed : boolean;
                            const xOptions : tPolygonDrawOptions = pdoFloat);

  procedure gSpline_TangentNorm(xBitmap : tBitmap32;
                            const xPoints : tArrayOfFixedPoint;
                            const xTension : double;
                            const xColor : tColor32;
                            const xOptions : tPolygonDrawOptions = pdoFloat);



  procedure gPolySpline_TangentNorm(xBitmap : tBitmap32;
                            const xPoints : tArrayOfArrayOfFixedPoint;
                            const xTension : double;
                            const xColor : tColor32;
                            const xOptions : tPolygonDrawOptions = pdoFloat);





  { TODO : LoadRectangleRounded  }
  { Load segments of bezier curve;
    in coommon case segments count can be greater then 2:
    dA = xEndAngle - xStartAngle;
    if dA = 0 then zero ;
    if 0 < dA <= pi then 1
    if pi < dA <=2pi then 2 and so on.
    }

  procedure LoadArcCurve(const xCenter : tFixedPoint;
                        const xA, xB : GR32.tFixed;
                        const xStartAngle, xEndAngle : double; { values in radians }
                        var yPP : TArrayOfFixedPoint);


  { Load segment of bezier curve witch round p1-p2-p3 andle with radius xR}
  { asumed that p1 point alway in yPP, we shuld add p21, p22 and two control point }
  procedure LoadRoundsCurve(const p1, p2, p3 : tFixedPoint;
                          const xR : GR32.tFixed;
                          var yPP : TArrayOfFixedPoint;
                          const xAddLast : boolean = true);

  { calculate arc parameters with radius xR in angle bases on p1-p2-p3 points}
  procedure CalculateRoundsArc(const p1, p2, p3 : tFixedPoint;
                             const xR : GR32.tFixed;
                             out p21, p22 : tFixedPoint; // end points of arc
                             out yC : tFixedPoint;       // center of arc
                             out yStartAngle, yEndAngle :double);

type

{ TBitmap32Ex }
{  }
{ end: wrapper around tBitmap32, to don't depend on future versions g32 lib. }


  TBitmap32Ex = class(tBitmap32)
  private
    fFont_e31 : Single;
    fFont_e32 : Single;
    fLastSumLength : GR32.tFixed; { Last found length of a segment.}
    function GetCanvas: tCanvas;
  protected
    fDrawOrign : tFixedPoint;
    fCanvas    : tCanvas;
    fFontMat2  : tMat2;

    fPen      : tPen32;
    fBrush    : tBrush32;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Polygon(const Points: TArrayOfFixedPoint;
                      const Color: TColor32;
                      const Options : tPolygonDrawOptions;
                      const Closed: Boolean;
                      const FillMode: TPolyFillMode = pfAlternate); overload;

    procedure Polygon(const Points: TArrayOfFixedPoint;
                     const Closed: boolean;
                     const Antialised : boolean = true;
                     const FillMode: TPolyFillMode = pfAlternate); overload;

    procedure PolyBezier(const Points: TArrayOfFixedPoint;
                         const Color: TColor32;
                         const Options : tPolygonDrawOptions;
                         const Closed: Boolean;
                         const FillMode: TPolyFillMode = pfAlternate); overload;


    procedure PolyBezier(const Points: TArrayOfFixedPoint;
                         const Closed: boolean;
                         const Antialised : boolean = true;
                         const FillMode: TPolyFillMode = pfAlternate); overload;


    procedure Ellipse(const xRect : tFixedRect;
                      const xColor: tColor32;
                      const xOptions : tPolygonDrawOptions = pdoFloat); overload;

    procedure Ellipse(const xRect : tFixedRect;
                      const Antialised : boolean = true); overload;


    procedure EllipseRotated(const xCenter : tFixedPoint;
                             const xA, xB : GR32.tFixed;
                             const xAngle : double;
                             const xColor : tColor32;
                             const xOptions : tPolygonDrawOptions = pdoFloat);

    procedure Arc(const xCenter : tFixedPoint;
                  const xR : GR32.tFixed;
                  const  xStartAngle, xEndAngle : double;
                  const  xColor : tColor32;
                  const  xOptions : tPolygonDrawOptions = pdoFloat);

    procedure ArcElliptic(const xCenter : tFixedPoint;
                          const xA, xB : GR32.tFixed;
                          const  xStartAngle, xEndAngle : double;
                          const  xColor : tColor32;
                          const  xOptions : tPolygonDrawOptions = pdoFloat);

    procedure Pie(const xCenter : tFixedPoint;
                  const xR : GR32.tFixed;
                  const  xStartAngle, xEndAngle : double;
                  const  xColor : tColor32;
                  const  xOptions : tPolygonDrawOptions = pdoFloat);

    procedure PieElliptic(const xCenter : tFixedPoint;
                          const   xA, xB : GR32.tFixed;
                          const  xStartAngle, xEndAngle : double;
                          const  xColor : tColor32;
                          const xOptions : tPolygonDrawOptions = pdoFloat);

    procedure Segment(const xCenter : tFixedPoint;
                      const xR : GR32.tFixed;
                      const  xStartAngle, xEndAngle : double;
                      const  xColor : tColor32;
                      const  xOptions : tPolygonDrawOptions = pdoFloat);
    procedure SegmentElliptic(const xCenter : tFixedPoint;
                              const xA, xB : GR32.tFixed;
                              const xStartAngle, xEndAngle : double;
                              const  xColor : tColor32;
                              const  xOptions : tPolygonDrawOptions = pdoFloat);



    { eng. fill all bitmap area with xColor except xRect (like window) }
    procedure RectangleHole(const xRect : tFixedRect;
                            const  xColor : TColor32;
                            const  xOptions : tPolygonDrawOptions); overload;
    procedure RectangleHole(const xRect : tRect;
                             const  xColor : TColor32;
                             const  xOptions : tPolygonDrawOptions); overload;

    procedure PolyPolygon(const Points : TArrayOfArrayOfFixedPoint;
                          const Color : tColor32;
                          const Options : tPolygonDrawOptions;
                          const Closed: Boolean;
                          const FillMode: TPolyFillMode = pfAlternate);
    procedure PolyPolyBezier(const Points : TArrayOfArrayOfFixedPoint;
                             const Color : tColor32;
                             const Options : tPolygonDrawOptions;
                             const Closed: Boolean;
                             const FillMode: TPolyFillMode = pfAlternate);

    procedure DrawSymbol(const xP      : GR32.tFixedPoint;
                        const xSymbol : tSymbolKind;
                        const xSize   : GR32.tFixed;
                        const xColor  : tColor32;
                        const xOptions : tPolygonDrawOptions);

    { eng : draw glyph of one symbol of current font in position (xLeft, yTop) }
    procedure  DrawGlyph(const xCharCode : longword;
                         const xLeft, yTop : GR32.tFixed;
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions);

    procedure  DrawGlyphW(const xCharCode : longword;
                         const xLeft, yTop : GR32.tFixed;
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions);


    { eng: render text with current font and symbols transform; xLeft, yTop is position of left and Bottom corner}
    procedure RenderTextEx(const xLeft, yBottom: GR32.tFixed;
                           const xText: string;
                           const xColor : tColor32;
                           const xOptions : tPolygonDrawOptions);

    procedure RenderTextExW(const xLeft, yBottom: GR32.tFixed;
                           const xText: WideString;
                           const xColor : tColor32;
                           const xOptions : tPolygonDrawOptions);


    function GetPointAtCurve(const xStartSegmentInd : integer;
                             const xCurve: tArrayOfFixedPoint;
                             const xLength : GR32.tFixed;
                             out xPathX, xPathY : GR32.tFixed; out xAngle : double;
                             const xApproxNeed : boolean = true  ): integer;
    { eng: text rendering along curve }
    procedure RenderFittedText(const xText : string;
                               const  xColor : tColor32;
                               const xOptions : tPolygonDrawOptions;
                               const xPath : tArrayOfFixedPoint);

    procedure RenderFittedTextW(const xText : WideString;
                               const  xColor : tColor32;
                               const xOptions : tPolygonDrawOptions;
                               const xPath : tArrayOfFixedPoint);

    { eng: setting symbols transformation matrix to cntrol it's rotations etc}
    function SelectFontMat2(const xValue : tMat2) : tMat2;
    function SelectFontTransform(const xValue : tFloatMatrix) : tFloatMatrix; {  }

    { TODO : DrawComplexCurve - for rendering pathes (complex curves) }
    { eng : true canvas }
    property Canvas : tCanvas read GetCanvas;
    { eng : orign of viewport for extended functions (Polygone, PolyBezie etc }
    //Displacement of system of coordinates (!) only at use of new functions
    property DrawOrign : tFixedPoint read fDrawOrign write fDrawOrign;  
    property Pen : tPen32 read fPen;
    property Brush : tBrush32 read fBrush;
  end;



implementation

  uses Sysutils, Math,  GR32_LowLevel;

  {$IFNDEF COMPILER6}
     {$I Delphi6Math.inc}
  {$ENDIF}


  {function FixedRect(const xR : tRect) : tFixedRect;
  begin
    result.Left := GR32.fixed(xR.left);
    result.Right := GR32.fixed(xR.right);
    result.Top := GR32.fixed(xR.top);
    result.Bottom := GR32.fixed(xR.bottom);
  end;

  function FixedRect(const xLeft, xTop, xRight, xBottom : GR32.tFixed) : tFixedRect;
  begin
    result.Left := xLeft;
    result.Right := xRight;
    result.Top := xTop;
    result.Bottom := xBottom;
  end; }

  function IsNullRect(const xR : tFixedRect) : boolean;
  begin
    result := (xR.Left = 0) and (xR.Top = 0) and (xR.Right = 0) and (xR.Bottom = 0);
  end;

  {$IFNDEF OPTIMIZE_CALLFUNCTIONS}
  function FloatToWinFix(x : single) : _FIXED;
  begin
    result := _Fixed(GR32.tFixed(trunc(x * 65536)));
  end;

  function WinFixToFloat(x : _Fixed) : double;
  begin
    result := div65536 * GR32.tFixed(x);
  end;
  {$ENDIF}

  function WinFixToFixed(x : _FIXED) : GR32.TFixed;
  begin
    result := GR32.TFixed(x);
  end;

  function FixedToWinFix(x : GR32.TFixed) : _Fixed;
  begin
    result := _Fixed(x);
  end;


  { Transformation G32 of a matrix in Win32 API a matrix of transformation }
  function  FloatMatrixToMat2(const xMat : tFloatMatrix) : TMAT2;
  begin
  {$IFDEF OPTIMIZE_CALLFUNCTIONS}
    result.eM11 := _Fixed(GR32.tFixed(trunc(xMat[0,0] * 65536)));
    result.eM21 := _Fixed(GR32.tFixed(trunc(xMat[1,0] * 65536)));
    result.eM12 := _Fixed(GR32.tFixed(trunc(xMat[0,1] * 65536)));
    result.eM22 := _Fixed(GR32.tFixed(trunc(xMat[1,1] * 65536)));
  {$ELSE}
    result.eM11 := FloatToWinFix(xMat[0,0]);
    result.eM21 := FloatToWinFix(xMat[1,0]);
    result.eM12 := FloatToWinFix(xMat[0,1]);
    result.eM22 := FloatToWinFix(xMat[1,1]);
  {$ENDIF}


  end;

  //Transformation Win32Api of a matrix of transformation to a g32-matrix
  function Mat2ToFloatMatrix(xMat : tMat2) : tFloatMatrix;
  begin
    {$IFDEF OPTIMIZE_CALLFUNCTIONS}
      result[0,0] := div65536 * GR32.tFixed(xMat.eM11);
      result[1,0] := div65536 * GR32.tFixed(xMat.eM21);
      result[0,1] := div65536 * GR32.tFixed(xMat.eM12);
      result[1,1] := div65536 * GR32.tFixed(xMat.eM22);
    {$ELSE}
      result[0,0] := WinFixToFloat(xMat.eM11);
      result[1,0] := WinFixToFloat(xMat.eM21);
      result[0,1] := WinFixToFloat(xMat.eM12);
      result[1,1] := WinFixToFloat(xMat.eM22);
    {$ENDIF}

  end;

  //{Reception of a matrix preobrazovanja turn.}
  function GetRotatedMat2(xAngle : double) : tMat2;
  var
    S, C : single;
  begin
    S := Sin(xAngle); C := Cos(xAngle);
    {$IFDEF OPTIMIZE_CALLFUNCTIONS}
      result.eM11 := _Fixed(GR32.tFixed(trunc(C * 65536)));
      result.eM21 := _Fixed(GR32.tFixed(trunc(S * 65536)));
      result.eM12 := _Fixed(GR32.tFixed(trunc(-S * 65536)));
      result.eM22 := _Fixed(GR32.tFixed(trunc(C * 65536)));
    {$ELSE}
      result.eM11 := FloatToWinFix(C);
      result.eM21 := FloatToWinFix(S);
      result.eM12 := FloatToWinFix(-S);
      result.eM22 := FloatToWinFix(C);
    {$ENDIF}
  end;

  function MultMat2(const M1, M2: tMat2) : tmat2;
  var
    m1_00, m1_01, m1_10, m1_11,
    m2_00, m2_01, m2_10, m2_11 : double;
  begin
    {$IFDEF OPTIMIZE_CALLFUNCTIONS}
      m1_00 := div65536 * GR32.tFixed(m1.eM11);
      m1_01 := div65536 * GR32.tFixed(m1.eM12);
      m1_10 := div65536 * GR32.tFixed(m1.eM21);
      m1_11 := div65536 * GR32.tFixed(m1.eM22);

      m2_00 := div65536 * GR32.tFixed(m2.eM11);
      m2_01 := div65536 * GR32.tFixed(m2.eM12);
      m2_10 := div65536 * GR32.tFixed(m2.eM21);
      m2_11 := div65536 * GR32.tFixed(m2.eM22);

      Result.eM11 := _Fixed(GR32.tFixed(trunc((m1_00 * m2_00 +  m1_10 * m2_01) * 65536)));
      Result.eM12 := _Fixed(GR32.tFixed(trunc((m1_01 * m2_00 +  m1_11 * m2_01) * 65536)));
      Result.eM21 := _Fixed(GR32.tFixed(trunc((m1_00 * m2_10 +  m1_10 * m2_11) * 65536)));
      Result.eM22 := _Fixed(GR32.tFixed(trunc((m1_01 * m2_10 +  m1_11 * m2_11) * 65536)));
    {$ELSE}
      m1_00 := WinFixToFloat(m1.eM11);
      m1_01 := WinFixToFloat(m1.eM12);
      m1_10 := WinFixToFloat(m1.eM21);
      m1_11 := WinFixToFloat(m1.eM22);

      m2_00 := WinFixToFloat(m2.eM11);
      m2_01 := WinFixToFloat(m2.eM12);
      m2_10 := WinFixToFloat(m2.eM21);
      m2_11 := WinFixToFloat(m2.eM22);

      Result.eM11 :=  FloatToWinFix(m1_00 * m2_00 +  m1_10 * m2_01);
      Result.eM12 :=  FloatToWinFix(m1_01 * m2_00 +  m1_11 * m2_01);
      Result.eM21 :=  FloatToWinFix(m1_00 * m2_10 +  m1_10 * m2_11);
      Result.eM22 :=  FloatToWinFix(m1_01 * m2_10 +  m1_11 * m2_11);
    {$ENDIF}
  end;

  function DetMat2(xMat : tMat2): Single;
  begin
    {$IFDEF OPTIMIZE_CALLFUNCTIONS}
    Result := div65536 * GR32.tFixed(xMat.eM11) * div65536 * GR32.tFixed(xMat.eM22)
            - div65536 * GR32.tFixed(xMat.eM12) * div65536 * GR32.tFixed(xMat.eM21);
    {$ELSE}
    Result := WinFixToFloat(xMat.eM11) * WinFixToFloat(xMat.eM22) - WinFixToFloat(xMat.eM12) * WinFixToFloat(xMat.eM21);
    {$ENDIF}
  end;


  function Norm1(const x1, y1, x2, y2: integer) : integer;
  begin
    result := abs(y2 - y1) + abs(x2 - x1);
  end;

  function Norm1(const p1, p2 : tFixedPoint) : GR32.tFixed;
  begin
    result := abs(p2.y - p1.y) + abs(p2.x - p1.x);
  end;

  function MiddleLine(const p1, p2 : tFixedPoint) : tFixedPoint;
  begin
    result.x := (p1.x + p2.x) div 2;
    result.y := (p1.y + p2.y) div 2;
  end;

  function PointsAreEqual(const p1, p2 : GR32.tFixedPoint) : boolean;
  begin
    result := (p1.x = p2.x) and (p1.y = p2.y);
  end;

  { Distance between points }
  function Distance(const p1, p2 :GR32.tFixedPoint) : GR32.tFixed; 
  begin
    result := round(hypot(p2.x - p1.x, p2.y - p1.y));
  end;

  { Distance between points }
  function Distance(const p1x, p1y, p2x, p2y :double) : double;
  begin
    result := hypot(p2x - p1x, p2y - p1y);
  end;

  //Square distance between points
  function SqrDistance(const p1, p2 :GR32.tFixedPoint) : GR32.tFixed;
  begin
    result := sqr(p2.x - p1.x) + sqr(p2.y - p1.y);
  end;

  procedure RotateArrayOfFixedPoint(var xPoints : TArrayOfFixedPoint; const xCenter : tFixedPoint; const xAngle : double);
  var
   vSin, vCos: extended;
   d : GR32.tFixedPoint;
   i : integer;
  begin
   SinCos(xAngle, vSin, vCos);
   for i := Low(xPoints) to High(xPoints) do
     begin
     d.x:=(xPoints[i].x - xCenter.x);
     d.y:=(xPoints[i].y - xCenter.y);
     xPoints[i].x := round(d.x*vCos + d.y*vSin + xCenter.x);
     xPoints[i].y := round(d.y*vCos - d.x*vSin + xCenter.y);
     end;
  end;

  { transformation on points array according affine transform matrix xAT }
  procedure TransformArrayOfFixedPoint(var xPoints : TArrayOfFixedPoint; const xAT : TFloatMatrix);
  var
    i : integer;
    x, y : single;
  begin
    for i := Low(xPoints) to High(xPoints) do
       begin
       x := xPoints[i].x*div65536;
       y := xPoints[i].y*div65536;
       xPoints[i].x := round((x*xAT[0,0] + y*xAT[1,0] + xAT[2,0])*65536);
       xPoints[i].y := round((x*xAT[0,1] + y*xAT[1,1] + xAT[2,1])*65536);
       end;
  end;

  function TransformFloatPoint(const xP : TFloatPoint; const xAT : TFloatMatrix) : tFloatPoint;
  begin
    result.x := xP.x*xAT[0,0] + xP.y*xAT[1,0] + xAT[2,0];
    result.y := xP.x*xAT[0,1] + xP.y*xAT[1,1] + xAT[2,1];
  end;



 { Estimation of length of a segment of a cubic curve }
 function SegmentConditionalLengthQ3N2(const p1, p2, p3, p4 : tFixedPoint) : GR32.tFixed;
 begin
   { rus: считаем, что истинная длина сегмента находится где то между LowValuation и TopValuation}
   { eng: consider, than real length is larger them  LowValuation and smoller TopValuation}
   result := (Distance(p1, p2) + Distance(p2, p3) + Distance(p3, p4) + Distance(p1, p4)) div 2;
 end;


  { qadric bezier segment conditional length in norm1 (supremum valuation) }
  function SegmentConditionalLengthQ2N1Sup(const x0, x1, x2 : tFixedPoint) : GR32.tFixed;
  begin
   { result := norm1(x0, x1)  + norm1(x1, x2); }
   result := abs(x0.X - x1.x) + abs(x0.Y - x1.Y) +
             abs(x1.X - x2.x) + abs(x1.Y - x2.Y);
  end;

  { qadric bezier segment conditional length in norm2 (supremum valuation) }
  function SegmentConditionalLengthQ2N2Sup(const x0, x1, x2 : tFixedPoint) : GR32.tFixed;
  begin
   { result := Distance(x0, x1)  + Distance(x1, x2); }
   result := round(hypot(x0.X - x1.x, x0.Y - x1.Y) +
             hypot(x1.X - x2.x, x1.Y - x2.Y));
  end;

  { qubic bezier segment conditional length in norm1 (supremum valuation) }
  function SegmentConditionalLengthQ3N1Sup(const x0, x1, x2, x3 : tFixedPoint) : GR32.tFixed;
  begin
  { result := norma(x0, x1)  + norma(x1, x2) + norma(x2, x3); }
  result := abs(x0.X - x1.x) + abs(x0.Y - x1.Y) +
            abs(x1.X - x2.x) + abs(x1.Y - x2.Y) +
            abs(x2.X - x3.x) + abs(x2.Y - x3.Y);
  end;

  { qubic bezier segment conditional length in norm2 (supremum valuation) }
  function SegmentConditionalLengthQ3N2Sup(const x0, x1, x2, x3 : tFixedPoint) : GR32.tFixed;
  begin
  { result := Distance(x0, x1)  + Distance(x1, x2) + Distance(x2, x3); }
  result := round(hypot(x0.X - x1.x, x0.Y - x1.Y) +
                  hypot(x1.X - x2.x, x1.Y - x2.Y) +
                  hypot(x2.X - x3.x, x2.Y - x3.Y));
  end;

  { qubic bezier segment conditional curvatre in norm1 }
  function SegmentConditionalCurvatureQ2(const x0, x1, x2 : tFixedPoint) :GR32.tFixed;
  begin
   result := SegmentConditionalLengthQ2N1Sup(x0, x1, x2);
  end;

{ two functions to incapsulate points adding to ArrayOfFixedPoint  one by one -
 for optimization in future if will need}
procedure AFP_AddPoint(var vPP : TArrayOfFixedPoint; const p : tFixedPoint);
var
  L : integer;
begin
  L := Length(vPP);
  SetLength(vPP,  L + 1);
  vPP[L] := p;
end;

procedure AFP_AddPoint2(var vPP : TArrayOfFixedPoint; const p1, p2 : tFixedPoint);
var
  L : integer;
begin
  L := Length(vPP);
  SetLength(vPP,  L + 2);
  vPP[L] := p1;
  vPP[L+1] := p2;
end;

procedure gRectangleHole(xBitmap : tBitmap32;
                           const xRect : tFixedRect;
                           const xColor : tColor32;
                           const xOptions : tPolygonDrawOptions);
var
  PP : TArrayOfArrayOfFixedPoint;

  function point(const x, y : GR32.tFixed) : tFixedPoint;
  begin
    result.x := x;
    result.y := y;
  end;
begin
  SetLength(PP, 2);
  Setlength(PP[0], 4);
  Setlength(PP[1], 4);

  PP[0, 0] := point(GR32.Fixed(-1), GR32.Fixed(-1));
  PP[0, 1] := point(GR32.Fixed(xBitmap.Width + 1), GR32.Fixed(-1));
  PP[0, 2] := point(GR32.Fixed(xBitmap.Width + 1), GR32.Fixed(xBitmap.Height + 1));
  PP[0, 3] := point(GR32.Fixed(-1), GR32.Fixed(xBitmap.Height + 1));

  PP[1, 0] := xRect.TopLeft;
  PP[1, 1] := point(xRect.Right, xRect.Top);
  PP[1, 2] := xRect.BottomRight;
  PP[1, 3] := point(xRect.left, xRect.Bottom);

  gPolyPolygon(xBitmap, PP, xColor, xOptions, true);

  PP := nil;
end;

{ draw simple ellipse }
procedure gEllipse(xBitmap : tBitmap32;
                   const xRect : TFixedRect;
                   const xColor: TColor32;
                   const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : TArrayOfFixedPoint;
  dy, dx : integer;
  C      : tFixedPoint; // center
begin
  { eng: approximate ellipse with four curve }

  c := MiddleLine(xRect.TopLeft, xRect.BottomRight);

  dx := trunc((xRect.Right - xRect.Left)*EllipseToCurveCoeff_4);
  dy := trunc((xRect.Bottom - xRect.Top)*EllipseToCurveCoeff_4);
  SetLength(PP, 3*4 + 1);

  PP[0].x := xRect.Left;    PP[0].y := c.Y;
  PP[1].x := xRect.Left;    PP[1].y := c.Y - dy;
  PP[2].x := c.x - dx;      PP[2].y := xRect.Top;
  PP[3].x := c.x;           PP[3].y := xRect.Top;
  PP[4].x := c.x + dx;      PP[4].y := xRect.Top;
  PP[5].x := xRect.Right;   PP[5].y := PP[1].y;
  PP[6].x := xRect.Right;   PP[6].y := c.Y;
  PP[7].x := xRect.Right;   PP[7].y := c.Y + dy;
  PP[8].x := PP[4].x;       PP[8].y :=  xRect.Bottom;
  PP[9].x := c.x;           PP[9].y := xRect.Bottom;
  PP[10].x := PP[2].x;      PP[10].y:= xRect.Bottom;
  PP[11].x := xRect.Left;   PP[11].y:= PP[7].y;
  PP[12].x := xRect.Left;   PP[12].y:= c.Y;

  gPolyBezier(xBitmap, PP, xColor, xOptions, true);
  PP := nil;
end;

procedure gEllipse_Styled(const xBitmap : tBitmap32;
                          const xRect : TFixedRect;
                          const xPenData : tPen32Data;
                          const xBrushData : tBrush32Data;
                          const xAntialised : boolean = true);
var
  PP : TArrayOfFixedPoint;
  dy, dx : integer;
  C      : tFixedPoint; // center
begin
  { eng: approximate ellipse with four curve }
  c := MiddleLine(xRect.TopLeft, xRect.BottomRight);

  dx := trunc((xRect.Right - xRect.Left)*EllipseToCurveCoeff_4);
  dy := trunc((xRect.Bottom - xRect.Top)*EllipseToCurveCoeff_4);
  SetLength(PP, 3*4 + 1);

  PP[0].x := xRect.Left;    PP[0].y := c.Y;
  PP[1].x := xRect.Left;    PP[1].y := c.Y - dy;
  PP[2].x := c.x - dx;      PP[2].y := xRect.Top;
  PP[3].x := c.x;           PP[3].y := xRect.Top;
  PP[4].x := c.x + dx;      PP[4].y := xRect.Top;
  PP[5].x := xRect.Right;   PP[5].y := PP[1].y;
  PP[6].x := xRect.Right;   PP[6].y := c.Y;
  PP[7].x := xRect.Right;   PP[7].y := c.Y + dy;
  PP[8].x := PP[4].x;       PP[8].y :=  xRect.Bottom;
  PP[9].x := c.x;           PP[9].y := xRect.Bottom;
  PP[10].x := PP[2].x;      PP[10].y:= xRect.Bottom;
  PP[11].x := xRect.Left;   PP[11].y:= PP[7].y;
  PP[12].x := xRect.Left;   PP[12].y:= c.Y;

  gPolyBezier_Styled(xBitmap, PP, true, xPenData, xBrushData, xAntialised);
  PP := nil;
end;


procedure LoadEllipseCurve(const xCenter : tFixedPoint;
                           const xA, xB : GR32.tFixed;  { if xAngle = 0 then A <-> width and xB <-> Height }
                           var yPP : TArrayOfFixedPoint);
var
  dy, dx : integer;
  A, B : GR32.tFixed;
begin
  dx := trunc(xA*EllipseToCurveCoeff_4);
  dy := trunc(xB*EllipseToCurveCoeff_4);
  A := xA div 2;
  B := xB div 2;
  SetLength(yPP, Length(yPP) + 3*4 + 1);
  yPP[0].x := xCenter.x - A;     yPP[0].y := xCenter.Y;
  yPP[1].x := yPP[0].x;          yPP[1].y := xCenter.Y - dy;
  yPP[2].x := xCenter.x - dx;    yPP[2].y := xCenter.Y - B;
  yPP[3].x := xCenter.x;         yPP[3].y := yPP[2].y;
  yPP[4].x := xCenter.x + dx;    yPP[4].y := yPP[2].y;
  yPP[5].x := xCenter.x + A;     yPP[5].y := xCenter.Y - dy;
  yPP[6].x := yPP[5].x;          yPP[6].y := xCenter.Y;
  yPP[7].x := yPP[5].x;          yPP[7].y := xCenter.Y + dy;
  yPP[8].x := xCenter.x + dx;    yPP[8].y := xCenter.Y + B;
  yPP[9].x := xCenter.x;         yPP[9].y := yPP[8].y;
  yPP[10].x := xCenter.x - dx;   yPP[10].y:= yPP[8].y;
  yPP[11].x := yPP[0].x;         yPP[11].y:= xCenter.Y + dy;
  yPP[12].x := yPP[0].x;         yPP[12].y:= xCenter.Y;

end;

{ draw rotated ellipse }
procedure gEllipseRotated(xBitmap : tBitmap32;
                          const xCenter : tFixedPoint;
                          const xA, xB : GR32.tFixed;  { if xAngle = 0 then A <-> width and xB <-> Height }
                          const xAngle : double;       { value in radians }
                          const xColor : tColor32;
                          const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : TArrayOfFixedPoint;
begin
  { now, rotate vector PP on  xAngle }
  LoadEllipseCurve(xCenter, xA, xB, PP);
  RotateArrayOfFixedPoint(PP, xCenter, xAngle);

  gPolyBezier(xBitmap, PP, xColor, xOptions, true);
  PP := nil;
end;

procedure gEllipseT(xBitmap : tBitmap32;
                            const xCenter : tFixedPoint;
                            const xA, xB : GR32.tFixed; { if xAngle = 0 then A <-> width and xB <-> Height }
                            const xAT : TFloatMatrix;
                            const xColor : tColor32;
                            const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : TArrayOfFixedPoint;
begin
  LoadEllipseCurve(xCenter, xA, xB, PP);
  { now, rotate vector PP on  xAngle }
  TransformArrayOfFixedPoint(PP, xAT);

  gPolyBezier(xBitmap, PP, xColor, xOptions, true);
  PP := nil;
end;


procedure LoadArcCurve(const xCenter : tFixedPoint;
                        const xA, xB : GR32.tFixed;
                        const xStartAngle, xEndAngle : double; { values in radians }
                        var yPP : TArrayOfFixedPoint);
var
  SinA, CosA, SinB, CosB, SinD, CosD : extended;
  dAngle : double;
  bcp : double;
  StartIndex : integer;
begin
  { calculation formulas based on http://www.stillhq.com/ctpfaq/2002/03/c1088.html#AEN1144 }

  dAngle := xEndAngle - xStartAngle;

  { if valuation of arc length is very small then exit;
   //  formula: length for circle (angle = 2pi) is 2*pi*R
       so length for ellipse is about angle*(R1+R2)/2 }
  if abs(dAngle)*(xA+xB) < eps_Fixed then
    begin
    exit;
    end;

  if abs(dAngle) >= pi then
    begin
    { DONE : split on two angles }
    LoadArcCurve(xCenter, xA, xB, xStartAngle, xStartAngle + 0.5*dAngle, yPP);
    LoadArcCurve(xCenter, xA, xB, xStartAngle + 0.5*dAngle, xEndAngle, yPP);
    exit;
    end;

  SinCos(xStartAngle, SinA, CosA);
  SinCos(xEndAngle, SinB, CosB);
  SinCos(dAngle*0.5, SinD, CosD);

  bcp := Ratio4div3* (1 - cosD)/SinD;

  StartIndex := Length(yPP);
  if StartIndex = 0 then
    begin
    SetLength(yPP, StartIndex + 4);

    yPP[StartIndex].x := xCenter.x + round(xA*CosA);
    yPP[StartIndex].y := xCenter.y - round(xB*SinA);
    end
  else
    begin
    SetLength(yPP, StartIndex + 3);
    dec(StartIndex);
    end;

  yPP[StartIndex + 1].x := xCenter.x + round(xA*(CosA - bcp*SinA));
  yPP[StartIndex + 1].y := xCenter.y - round(xB*(SinA + bcp*CosA));
  yPP[StartIndex + 2].x := xCenter.x + round(xA*(CosB + bcp*SinB));
  yPP[StartIndex + 2].y := xCenter.y - round(xB*(SinB - bcp*CosB));
  yPP[StartIndex + 3].x := xCenter.x + round(xA*CosB);
  yPP[StartIndex + 3].y := xCenter.y - round(xB*SinB);
end;

procedure gArc(xBitmap : tBitmap32;
               const xCenter : tFixedPoint;
               const xR : GR32.tFixed;
               const xStartAngle, xEndAngle : double;
               const xColor : tColor32;
               const xOptions : tPolygonDrawOptions = pdoFloat);
begin
  gArcElliptic(xBitmap, xCenter, xR, xR, xStartAngle, xEndAngle, xColor, xOptions);
end;

procedure gArcElliptic(xBitmap : tBitmap32;
                       const xCenter : tFixedPoint;
                       const xA, xB : GR32.tFixed;
                       const xStartAngle, xEndAngle : double; { values in radians +- pi }
                       const xColor : tColor32;
                       const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : TArrayOfFixedPoint;
begin
  SetLength(PP, 0);
  LoadArcCurve(xCenter, xA, xB, xStartAngle, xEndAngle, PP);
  gPolyBezier(xBitmap, PP, xColor, xOptions, false);
  PP := nil;
end;

{ draw elliptic and rotated arc }
procedure gArcER(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xRotAngle : double;
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : TArrayOfFixedPoint;
begin
  SetLength(PP, 0);
  LoadArcCurve(xCenter, xA, xB, xStartAngle, xEndAngle, PP);
  RotateArrayOfFixedPoint(PP, xCenter, xRotAngle);
  gPolyBezier(xBitmap, PP, xColor, xOptions, false);
  PP := nil;
end;

{ draw elliptic and transformed arc }
procedure gArcET(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xAT : TFloatMatrix; { affine transformation matrix }
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : TArrayOfFixedPoint;
begin
  SetLength(PP, 0);
  LoadArcCurve(xCenter, xA, xB, xStartAngle, xEndAngle, PP);
  TransformArrayOfFixedPoint(PP, xAT);
  gPolyBezier(xBitmap, PP, xColor, xOptions, false);
  PP := nil;
end;

{ draw elliptic and transformed arc }
procedure gArcET_CenterRelative(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xAT : TFloatMatrix; { affine transformation matrix }
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);

var
  PP : TArrayOfFixedPoint;
  M  : tFloatMatrix;
begin
  SetLength(PP, 0);
  LoadArcCurve(FixedPoint(0, 0), xA, xB, xStartAngle, xEndAngle, PP);
  M := xAT;
  M[2,0] := xCenter.x*div65536;
  M[2,1] := xCenter.y*div65536;
  TransformArrayOfFixedPoint(PP, M);

  gPolyBezier(xBitmap, PP, xColor, xOptions, false);
  PP := nil;
end;




procedure gSegment(xBitmap : tBitmap32;
                 const xCenter : tFixedPoint;
                 const xR : GR32.tFixed;
                 const xStartAngle, xEndAngle : double;
                 const xColor : tColor32;
                 const xOptions : tPolygonDrawOptions = pdoFloat);
begin
  gSegmentElliptic(xBitmap, xCenter, xR, xR, xStartAngle, xEndAngle, xColor, xOptions);
end;


procedure gSegmentElliptic(xBitmap : tBitmap32;
                       const xCenter : tFixedPoint;
                       const xA, xB : GR32.tFixed;
                       const xStartAngle, xEndAngle : double; { values in radians }
                       const xColor : tColor32;
                       const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : TArrayOfFixedPoint;
begin
  SetLength(PP, 0);
  LoadArcCurve(xCenter, xA, xB, xStartAngle, xEndAngle, PP);
  gPolyBezier(xBitmap, PP, xColor, xOptions, true);
  PP := nil;
end;

{ draw elliptic and rotated segment for monsters }
procedure gSegmentER(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xRotAngle : double;
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : TArrayOfFixedPoint;
begin
  SetLength(PP, 0);
  LoadArcCurve(xCenter, xA, xB, xStartAngle, xEndAngle, PP);
  RotateArrayOfFixedPoint(PP, xCenter, xRotAngle);
  gPolyBezier(xBitmap, PP, xColor, xOptions, true);
  PP := nil;
end;

{ draw elliptic and transformed segment }
 procedure gSegmentET(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xAT : TFloatMatrix; { affine transformation matrix }
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : TArrayOfFixedPoint;
begin
  SetLength(PP, 0);
  LoadArcCurve(xCenter, xA, xB, xStartAngle, xEndAngle, PP);
  TransformArrayOfFixedPoint(PP,xAT);
  gPolyBezier(xBitmap, PP, xColor, xOptions, true);
  PP := nil;
end;

procedure gPie(xBitmap : tBitmap32;
               const xCenter : tFixedPoint;
               const xR : GR32.tFixed;
               const xStartAngle, xEndAngle : double;
               const xColor : tColor32;
               const xOptions : tPolygonDrawOptions = pdoFloat);
begin
  gPieElliptic(xBitmap, xCenter, xR, xR, xStartAngle, xEndAngle, xColor, xOptions);
end;

procedure gPieElliptic(xBitmap : tBitmap32;
                       const xCenter : tFixedPoint;
                       const xA, xB : GR32.tFixed;
                       const xStartAngle, xEndAngle : double; { values in radians }
                       const xColor : tColor32;
                       const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : TArrayOfFixedPoint;
  L  : integer;
begin
  SetLength(PP, 0);
  LoadArcCurve(xCenter, xA, xB, xStartAngle, xEndAngle, PP);

  { connect with center }
  L := Length(PP);
  SetLength(PP, L + 3);
  PP[L] := PP[L-1];
  PP[L + 1]  := xCenter;
  PP[L + 2 ] := xCenter;

  gPolyBezier(xBitmap, PP, xColor, xOptions, true);

  PP := nil;
end;

{ draw elliptic rotated pie }
procedure gPieER(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xRotAngle : double;
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : TArrayOfFixedPoint;
  L  : integer;
begin
  { calculation formulas based on http://www.stillhq.com/ctpfaq/2002/03/c1088.html#AEN1144 }
  SetLength(PP, 0);
  LoadArcCurve(xCenter, xA, xB, xStartAngle, xEndAngle, PP);

  { connect with center }
  L := Length(PP);
  SetLength(PP, L + 3);
  PP[L] := PP[L-1];
  PP[L + 1]  := xCenter;
  PP[L + 2 ] := xCenter;

  RotateArrayOfFixedPoint(PP, xCenter, xRotAngle);
  gPolyBezier(xBitmap, PP, xColor, xOptions, true);

  PP := nil;
end;

{ draw elliptic and transformed pie }
procedure gPieET(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xAT : TFloatMatrix; { affine transformation matrix }
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : TArrayOfFixedPoint;
  L  : integer;
begin
  SetLength(PP, 0);
  LoadArcCurve(xCenter, xA, xB, xStartAngle, xEndAngle, PP);

  { connect with center }
  L := Length(PP);
  SetLength(PP, L + 3);
  PP[L] := PP[L-1];
  PP[L + 1]  := xCenter;
  PP[L + 2 ] := xCenter;

  TransformArrayOfFixedPoint(PP, xAT);
  gPolyBezier(xBitmap, PP, xColor, xOptions, true);

  PP := nil;
end;

{ draw elliptic pie transformed relatively on center  }
procedure gPieET_CenterRelative(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xStartAngle, xEndAngle : double; { values in radians }
                         const xAT : TFloatMatrix; { affine transformation matrix }
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : TArrayOfFixedPoint;
  L  : integer;
  M : tFloatMatrix;
begin
  SetLength(PP, 0);
  LoadArcCurve(FixedPoint(0, 0), xA, xB, xStartAngle, xEndAngle, PP);

  { connect with center }
  L := Length(PP);
  SetLength(PP, L + 3);
  PP[L] := PP[L-1];
  PP[L + 1]  := FixedPoint(0, 0);
  PP[L + 2 ] := FixedPoint(0, 0);
  M := xAT;
  M[2,0] := xCenter.x*div65536;
  M[2,1] := xCenter.y*div65536;
  TransformArrayOfFixedPoint(PP, M);
  gPolyBezier(xBitmap, PP, xColor, xOptions, true);

  PP := nil;
end;


{ draw rounded rectangle }
procedure gRectangleRounded(xBitmap : tBitmap32;
                            const xRect : TFixedRect;
                            const xR    : GR32.TFixed;
                            const xColor : tColor32;
                            const  xOptions : tPolygonDrawOptions = pdoFloat);

var
  PP : tArrayOfFixedPoint;
  dR : GR32.TFixed;
begin
  dR := round(EllipseToCurveCoeff_2inv*xR);

  SetLength(PP, 22);

  PP[0].x := xRect.Right - xR;    PP[0].y := xRect.Top;
  PP[1].x := xRect.Right - dR;    PP[1].y := xRect.Top;
  PP[2].x := xRect.Right;         PP[2].y := xRect.Top + dR;
  PP[3].x := xRect.Right;         PP[3].y := xRect.Top + xR;
  PP[4].x := xRect.Right;         PP[4].y := pp[3].y;

  PP[5].x := xRect.Right;         PP[5].y := xRect.Bottom  - xR;
  PP[6].x := xRect.Right;         PP[6].y := PP[5].y;
  PP[7].x := xRect.Right;         PP[7].y := xRect.Bottom  - dR;
  PP[8].x := PP[1].x;             PP[8].y := xRect.Bottom;
  PP[9].x := PP[0].x;             PP[9].y := xRect.Bottom;
  PP[10].x := PP[0].x;            PP[10].y := xRect.Bottom;


  PP[11].x := xRect.Left + xR;  PP[11].y := xRect.Bottom;
  PP[12].x := PP[11].x;         PP[12].y := xRect.Bottom;
  PP[13].x := xRect.Left + dR;  PP[13].y := xRect.Bottom;
  PP[14].x := xRect.Left;       PP[14].y := PP[7].y;
  PP[15].x := xRect.Left;       PP[15].y := PP[5].y;
  PP[16].x := xRect.Left;       PP[16].y := PP[5].y;

  PP[17].x := xRect.Left;       PP[17].y := PP[3].y;
  PP[18].x := xRect.Left;       PP[18].y := PP[3].y;
  PP[19].x := xRect.Left;       PP[19].y := PP[2].y;
  PP[20].x := PP[13].x;         PP[20].y := xRect.Top;
  PP[21].x := PP[11].x;         PP[21].y := xRect.Top;

  gPolyBezier(xBitmap, PP, xColor, xOptions, true);
  PP := nil;
end;

{ draw rounded and rotated rectangle }
procedure gRectangleRR(xBitmap : tBitmap32;
                         const xCenter : tFixedPoint;
                         const xA, xB : GR32.tFixed;
                         const xR     : GR32.tFixed;
                         const xAngle : double;
                         const xColor : tColor32;
                         const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : tArrayOfFixedPoint;
  dR : GR32.TFixed;
  vRect : tFixedRect;
begin
  dR := round(EllipseToCurveCoeff_2inv*xR);

  vRect := FixedRect(xCenter.x - xA, xCenter.y - xB, xCenter.x + xA, xCEnter.y + xB);

  SetLength(PP, 22);

  PP[0].x := vRect.Right - xR;    PP[0].y := vRect.Top;
  PP[1].x := vRect.Right - dR;    PP[1].y := vRect.Top;
  PP[2].x := vRect.Right;         PP[2].y := vRect.Top + dR;
  PP[3].x := vRect.Right;         PP[3].y := vRect.Top + xR;
  PP[4].x := vRect.Right;         PP[4].y := pp[3].y;

  PP[5].x := vRect.Right;         PP[5].y := vRect.Bottom  - xR;
  PP[6].x := vRect.Right;         PP[6].y := PP[5].y;
  PP[7].x := vRect.Right;         PP[7].y := vRect.Bottom  - dR;
  PP[8].x := PP[1].x;             PP[8].y := vRect.Bottom;
  PP[9].x := PP[0].x;             PP[9].y := vRect.Bottom;
  PP[10].x := PP[0].x;            PP[10].y := vRect.Bottom;

  PP[11].x := vRect.Left + xR;  PP[11].y := vRect.Bottom;
  PP[12].x := PP[11].x;         PP[12].y := vRect.Bottom;
  PP[13].x := vRect.Left + dR;  PP[13].y := vRect.Bottom;
  PP[14].x := vRect.Left;       PP[14].y := PP[7].y;
  PP[15].x := vRect.Left;       PP[15].y := PP[5].y;
  PP[16].x := vRect.Left;       PP[16].y := PP[5].y;

  PP[17].x := vRect.Left;       PP[17].y := PP[3].y;
  PP[18].x := vRect.Left;       PP[18].y := PP[3].y;
  PP[19].x := vRect.Left;       PP[19].y := PP[2].y;
  PP[20].x := PP[13].x;         PP[20].y := vRect.Top;
  PP[21].x := PP[11].x;         PP[21].y := vRect.Top;

  RotateArrayOfFixedPoint(PP, xCenter, xAngle);
  gPolyBezier(xBitmap, PP, xColor, xOptions, true);
  PP := nil;
end;


procedure BuildCardinalSplineCurve(xPoints : tArrayOfFixedPoint;
                                   const xTension : double;
                                   const xClosed : boolean;
                                   out yPP : tArrayOfFixedPoint);
var
  N : integer;
  i : integer;
  ind : integer;
  K : double;
begin
{ TODO : Optimize to run away from double calculation }

  N := Length(xPoints);
  if N < 3 then exit;

  { curve segment points  formula :
      b0 := P[n]
      b1 := b0 + (1-t)*(p[n+1] - p[n-1])/6;
      b2 := b3 - (1-t)*(p[n+2] - p[n])/6;
      b3 := p[n+1]
    }
  K := (1-xTension)*Ratio6;

  if xClosed then SetLength(yPP, N*3 + 1)
             else SetLength(yPP, (N - 1)*3 + 1);


    { -- load first segment :}
    yPP[0] := xPoints[0];
    if xClosed then
      begin
      { if i = 0 then i - 1 = N - 1}
      yPP[1].x := yPP[0].x + round(K*(xPoints[1].x - xPoints[N-1].x));
      yPP[1].y := yPP[0].y + round(K*(xPoints[1].y - xPoints[N-1].y));
      end
    else
      begin
      yPP[1] := xPoints[0];  // for free ends
      end;
    yPP[3] := xPoints[1];
    yPP[2].x := yPP[3].x - round(K*(xPoints[2].x - xPoints[0].x));
    yPP[2].y := yPP[3].y - round(K*(xPoints[2].y - xPoints[0].y));
    { -- load inner segments :}
    for i := 1 to N - 3 do
      begin
      ind := 3*i;
      { yPP[i] already assigned }

      yPP[ind + 1].x := yPP[ind].x + round(K*(xPoints[i+1].x - xPoints[i-1].x));
      yPP[ind + 1].y := yPP[ind].y + round(K*(xPoints[i+1].y - xPoints[i-1].y));
      yPP[ind + 2].x := xPoints[i+1].x - round(K*(xPoints[i+2].x - xPoints[i].x));
      yPP[ind + 2].y := xPoints[i+1].y - round(K*(xPoints[i+2].y - xPoints[i].y));
      yPP[ind + 3] := xPoints[i+1];
      end;
    { -- load last segment : }
    ind := 3*(N-2);
    { yPP[ind] := xPoints[N-2]; already assigned }
    yPP[ind + 1].x := yPP[ind].x + round(K*(xPoints[N-1].x - xPoints[N-3].x));
    yPP[ind + 1].y := yPP[ind].y + round(K*(xPoints[N-1].y - xPoints[N-3].y));
    if xClosed then
      begin
      {if i = n - 2 then i + 2 = 0}
      yPP[ind + 2].x := xPoints[N-1].x - round(K*(xPoints[0].x - xPoints[N-2].x));
      yPP[ind + 2].y := xPoints[N-1].y - round(K*(xPoints[0].y - xPoints[N-2].y));
      end
    else
      begin
      yPP[ind + 2] := xPoints[n-1];
      end;
    yPP[ind + 3] := xPoints[n-1];

    { now, if closed connect first and last points by curve segments }
    if xClosed   then
      begin
      ind := 3*(N-1);
      yPP[ind + 1].x := yPP[ind].x + round(K*(xPoints[0].x - xPoints[N-2].x));
      yPP[ind + 1].y := yPP[ind].y + round(K*(xPoints[0].y - xPoints[N-2].y));
      yPP[ind + 2].x := xPoints[0].x - round(K*(xPoints[1].x - xPoints[N-1].x));
      yPP[ind + 2].y := xPoints[0].y - round(K*(xPoints[1].y - xPoints[N-1].y));
      yPP[ind + 3] := xPoints[0];
      end;


end;

{ normalization of control point distance to segment  distance.}
procedure BuildLengthNormalizedSplineCurve(xPoints : tArrayOfFixedPoint;
                                   const xTension : double;
                                   const xClosed : boolean;
                                   out yPP : tArrayOfFixedPoint);
var
  N : integer;
  i : integer;
  ind : integer;
  K : double;
  L : double;
  L1 : double;
  L2 : double;
begin

 { TODO : Optimize to run away from double calculation }
  N := Length(xPoints);
  if N < 3 then exit;

  { curve segment points  formula :
      b0 := P[n]
      b1 := b0 + (1-t)*(p[n+1] - p[n-1])/6;
      b2 := b3 - (1-t)*(p[n+2] - p[n])/6;
      b3 := p[n+1]
    }

 K := (1-xTension)*Ratio3;

 if xClosed then SetLength(yPP, N*3 + 1)
            else SetLength(yPP, (N - 1)*3 + 1);



    { -- load first segment :}
    yPP[0] := xPoints[0];
    if xClosed then
      begin
      { if i = 0 then i - 1 = N - 1}
      yPP[1].x := yPP[0].x + round(K*(xPoints[1].x - xPoints[N-1].x));
      yPP[1].y := yPP[0].y + round(K*(xPoints[1].y - xPoints[N-1].y));
      end
    else
      begin
      yPP[1] := xPoints[0];  // for free ends
      end;
    yPP[3] := xPoints[1];
    yPP[2].x := yPP[3].x - round(K*(xPoints[2].x - xPoints[0].x));
    yPP[2].y := yPP[3].y - round(K*(xPoints[2].y - xPoints[0].y));

    { TODO }

    { -- load inner segments :}
    for i := 1 to N - 3 do
      begin
      ind := 3*i;
      { yPP[i] already assigned }
      L := Distance(xPoints[i], xPoints[i+1]);
      L1 := Distance(xPoints[i-1], xPoints[i+1]);
      L2 := Distance(xPoints[i], xPoints[i+2]);
      yPP[ind + 1].x := yPP[ind].x + round(K*(xPoints[i+1].x - xPoints[i-1].x)*L/L1);
      yPP[ind + 1].x := yPP[ind].x + round(K*(xPoints[i+1].x - xPoints[i-1].x)*L/(L1+L));
      yPP[ind + 1].x := yPP[ind].x + round(K*(xPoints[i+1].x - xPoints[i-1].x)*L/L1);
      yPP[ind + 1].y := yPP[ind].y + round(K*(xPoints[i+1].y - xPoints[i-1].y)*L/L1);
      yPP[ind + 2].x := xPoints[i+1].x - round(K*(xPoints[i+2].x - xPoints[i].x)*L/L2);

      yPP[ind + 2].y := xPoints[i+1].y - round(K*(xPoints[i+2].y - xPoints[i].y)*L/L2);
      yPP[ind + 3] := xPoints[i+1];
      end;
    { -- load last segment : }
    ind := 3*(N-2);
    { yPP[ind] := xPoints[N-2]; already assigned }

    yPP[ind + 1].x := yPP[ind].x + round(K*(xPoints[N-1].x - xPoints[N-3].x));
    yPP[ind + 1].y := yPP[ind].y + round(K*(xPoints[N-1].y - xPoints[N-3].y));

    if xClosed then
      begin
      {if i = n - 2 then i + 2 = 0}
      yPP[ind + 2].x := xPoints[N-1].x - round(K*(xPoints[0].x - xPoints[N-2].x));
      yPP[ind + 2].y := xPoints[N-1].y - round(K*(xPoints[0].y - xPoints[N-2].y));
      end
    else

      begin
      yPP[ind + 2] := xPoints[n-1];
      end;
    yPP[ind + 3] := xPoints[n-1];

    { now, if closed connect first and last points by curve segments }
    if xClosed   then
      begin
      ind := 3*(N-1);
      yPP[ind + 1].x := yPP[ind].x + round(K*(xPoints[0].x - xPoints[N-2].x));
      yPP[ind + 1].y := yPP[ind].y + round(K*(xPoints[0].y - xPoints[N-2].y));
      yPP[ind + 2].x := xPoints[0].x - round(K*(xPoints[1].x - xPoints[N-1].x));
      yPP[ind + 2].y := xPoints[0].y - round(K*(xPoints[1].y - xPoints[N-1].y));
      yPP[ind + 3] := xPoints[0];
      end;
end;


{ tangent normalization.
  The xPoints array must be sorted by x-coordinate.
  In this case for every x value there will be only one point in curve (or nothing).

  input: array of point sorted by x-coordinate
  output: qubic bezier curve that are interpolation of input points set
}
procedure BuildTangentNormalizedSplineCurve(xPoints : tArrayOfFixedPoint;
                                      const xTension : double;
                                      out yPP : tArrayOfFixedPoint);
// this is ivented myself one of way Y(X) interpolation:
type
  tPointF2 = record
             x, y : double;
             end;

var
  N : integer;
  i : integer;
  ind : integer;

  vDeltaLeft  : tPointF2;
  vDeltaRight : tPointF2;



  procedure GetDeltas(const xPLeft, xPCenter, xPRight : tFixedPoint;
                    out yDeltaLeft : tPointF2;
                    out yDeltaRight : tPointF2);
  var

    L1 : double; // distance between xPLeft, xPСenter point
    L2 : double; // distance between xPCenter, xPRight point
    K  : double;
    tan1 : double;
    tan2 : double;
    tan  : double;
    signX1 : double;
    signY1 : double;
    signX2 : double;
    signY2 : double;


  begin
    K := (1-xTension)*Ratio3;
    L1   := sqrt(sqr(xPCenter.x*div65536 - xPLeft.x*div65536) + sqr(xPCenter.y*div65536 - xPLeft.y*div65536));
    L2   := sqrt(sqr(xPCenter.x*div65536 - xPRight.x*div65536) + sqr(xPCenter.y*div65536 - xPRight.y*div65536));

    if (abs(xPCenter.x - xPLeft.x) = 0) or (abs(xPCenter.x - xPRight.x)=0) then
      begin
      // tan1 = infinitive;

      if abs(xPCenter.x - xPLeft.x) = 0 then
        begin
        yDeltaLeft.x := 0;
        yDeltaLeft.y := K*(xPCenter.y - xPLeft.y);
        yDeltaRight.x := 0;
        yDeltaRight.y :=65536*K*L2*Sign(xPCenter.y - xPLeft.y);
        end
      else
        begin
        Assert(abs(xPCenter.x - xPRight.x)=0);
        yDeltaLeft.x := 0;
        yDeltaLeft.y := -65536*K*L1*Sign(xPCenter.y - xPRight.y);
        yDeltaRight.x := 0;
        yDeltaRight.y := -K*(xPCenter.Y - xPRight.Y);
        end;
      end
    else
      begin
      L1 := min(L1, abs(xPCenter.x - xPLeft.x)*div65536);
      L2 := min(L2, abs(xPCenter.x - xPRight.x)*div65536);
      tan1 := (xPCenter.y - xPLeft.y)/(xPCenter.x - xPLeft.x);
      tan2 := (xPRight.y - xPCenter.y)/(xPRight.x - xPCenter.x);
      tan := tan1*div2 + tan2*div2; { as varian: tan := (L2*tan1*div2 + L1*tan2*div2)/(L1+L2); }
      { common case: }
      signX1 := sign(xPCenter.x - xPLeft.x);
      signY1 := sign(tan);

      yDeltaLeft.x := 65536*SignX1*K*L1/sqrt(1+sqr(tan));
      yDeltaLeft.y := SignY1*abs(yDeltaLeft.x*tan);

      signX2 := sign(xPRight.x - xPCenter.x);
      signY2 := sign(tan);

      yDeltaRight.x := 65536*SignX2*k*L2/sqrt(1+sqr(tan));
      yDeltaRight.y := SignY2*abs(yDeltaRight.x*tan);
      end;

  end;
begin
  { TODO : Optimize to run away from double calculation }
  N := Length(xPoints);
  if N < 3 then exit;

  { curve segment points  formula :
      b0 := P[n]
      b1 := b0 + (1-t)*(p[n+1] - p[n-1])/6;
      b2 := b3 - (1-t)*(p[n+2] - p[n])/6;
      b3 := p[n+1]
    }

  SetLength(yPP, (N - 1)*3 + 1);

   { -- load first segment :}
    yPP[0] := xPoints[0];
    yPP[1] := xPoints[0];  // for free ends

    yPP[3] := xPoints[1];

    //yPP[2].x := yPP[3].x - round(K*(xPoints[2].x - xPoints[0].x));
    GetDeltas(xPoints[0], xPoints[1], xPoints[2], vDeltaLeft, vDeltaRight);
    yPP[2].x := yPP[3].x - round(vDeltaLeft.x);
    yPP[2].y := yPP[3].y - round(vDeltaLeft.y);
    { -- load inner segments :}
    for i := 1 to N - 3 do
      begin
      ind := 3*i;
      { yPP[i] already assigned }
      //yPP[ind + 1].x := yPP[ind].x + round(K*(xPoints[i+1].x - xPoints[i-1].x));
      GetDeltas(xPoints[i-1], xPoints[i], xPoints[i+1], vDeltaLeft, vDeltaRight);
      yPP[ind + 1].x := xPoints[i].x + round(vDeltaRight.x);
      yPP[ind + 1].y := xPoints[i].y + round(vDeltaRight.y);

      //yPP[ind + 2].x := xPoints[i+1].x - round(K*(xPoints[i+2].x - xPoints[i].x));
      GetDeltas(xPoints[i], xPoints[i+1], xPoints[i+2], vDeltaLeft, vDeltaRight);
      yPP[ind + 2].x := xPoints[i+1].x - round(vDeltaLeft.x);
      yPP[ind + 2].y := xPoints[i+1].y - round(vDeltaLeft.y);

      yPP[ind + 3] := xPoints[i+1];
      end;
    { -- load last segment : }
    ind := 3*(N-2);
    { yPP[ind] := xPoints[N-2]; already assigned }
    //yPP[ind + 1].x := yPP[ind].x + round(K*(xPoints[N-1].x - xPoints[N-3].x));
    GetDeltas(xPoints[N-3], xPoints[N-2], xPoints[N-1], vDeltaLeft, vDeltaRight);
    yPP[ind + 1].x := yPP[ind].x + round(vDeltaRight.x);
    yPP[ind + 1].y := yPP[ind].y + round(vDeltaRight.y);
    yPP[ind + 2] := xPoints[n-1];
    yPP[ind + 3] := xPoints[n-1];
end;

procedure gCardinalSpline(xBitmap : tBitmap32;
                            const xPoints : tArrayOfFixedPoint;
                            const xTension : double;
                            const xColor : tColor32;
                            const xClosed : boolean;
                            const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : tArrayOfFixedPoint;
begin
  BuildCardinalSplineCurve(xPoints, xTension, xClosed, PP);
  gPolyBezier(xBitmap, PP, xColor, xOptions, xClosed);
  PP := nil;
end;

procedure gPolyCardinalSpline(xBitmap : tBitmap32;
                            const xPoints : tArrayOfArrayOfFixedPoint;
                            const xTension : double;
                            const xColor : tColor32;
                            const xClosed : boolean;
                            const xOptions : tPolygonDrawOptions = pdoFloat);
var
  i : integer;
  PP : tArrayOfArrayOfFixedPoint;
begin
  SetLength(PP, Length(xPoints));
  for i := 0 to Length(xPoints) - 1 do
    begin
    BuildCardinalSplineCurve(xPoints[i], xTension, xClosed, PP[i]);
    end;
  gPolyPolyBezier(xBitmap, PP, xColor, xOptions, xClosed);
  PP := nil;
end;

procedure gSpline_LengthNorm(xBitmap : tBitmap32;
                            const xPoints : tArrayOfFixedPoint;
                            const xTension : double;
                            const xColor : tColor32;
                            const xClosed : boolean;
                            const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : tArrayOfFixedPoint;
begin
  BuildLengthNormalizedSplineCurve(xPoints, xTension, xClosed, PP);
  gPolyBezier(xBitmap, PP, xColor, xOptions, xClosed);
  PP := nil;
end;

procedure gPolySpline_LengthNorm(xBitmap : tBitmap32;
                            const xPoints : tArrayOfArrayOfFixedPoint;
                            const xTension : double;
                            const xColor : tColor32;
                            const xClosed : boolean;
                            const xOptions : tPolygonDrawOptions = pdoFloat);
var
  i : integer;
  PP : tArrayOfArrayOfFixedPoint;
begin
  SetLength(PP, Length(xPoints));
  for i := 0 to Length(xPoints) - 1 do
    begin
    BuildLengthNormalizedSplineCurve(xPoints[i], xTension, xClosed, PP[i]);
    // for debug:
    //gDrawSymbols(xBitmap, PP[i],skTriangle , GR32.Fixed(3.0), clRed32, xOptions);
    end;
  gPolyPolyBezier(xBitmap, PP, xColor, xOptions, xClosed);
  PP := nil;
end;


{ tangent normalization.
  gSpline_TangentNorm
  The xPoints array must be sorted by x-coordinate.
  In this case for every x value there will be only one point in curve (or nothing).
  It is not ideal qubic spline,  but appropriated

  input: array of point sorted by x-coordinate
  output: qubic bezier curve that are interpolation of input points set
}
procedure gSpline_TangentNorm(xBitmap : tBitmap32;
                            const xPoints : tArrayOfFixedPoint;
                            const xTension : double;
                            const xColor : tColor32;
                              const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : tArrayOfFixedPoint;
begin
  BuildTangentNormalizedSplineCurve(xPoints, xTension, PP);
  gPolyBezier(xBitmap, PP, xColor, xOptions, false);
  PP := nil;
end;

procedure gPolySpline_TangentNorm(xBitmap : tBitmap32;
                            const xPoints : tArrayOfArrayOfFixedPoint;
                            const xTension : double;
                            const xColor : tColor32;
                            const xOptions : tPolygonDrawOptions = pdoFloat);
var
  i : integer;
  PP : tArrayOfArrayOfFixedPoint;
begin
  SetLength(PP, Length(xPoints));
  for i := 0 to Length(xPoints) - 1 do
    begin
    BuildTangentNormalizedSplineCurve(xPoints[i], xTension, PP[i]);
    end;
  gPolyPolyBezier(xBitmap, PP, xColor, xOptions, false);
  PP := nil;
end;

procedure BuildTCBSplineCurve(xPoints : tArrayOfFixedPoint;
                              const xTension : double;
                              const xContinuity : double;
                              const xBias : double;
                              const xClosed : boolean;
                              out yPP : tArrayOfFixedPoint);
var
  N : integer;
  i : integer;
  ind : integer;
  K   : double;
  Kmc : double;
  Kpc : double;
  Kmb : double;
  Kpb : double;
  k11, k12, k21, k22 : double;

begin
 { TODO : Optimize to run away from double calculation }

  N := Length(xPoints);
  if N < 3 then exit;

  { curve segment points  formula :
   --------------
      b0 := P[n]
      b1 := b0 + (1-t)*[ (1+b)(1-c)(P[n] - P[n-1]) + (1-b)(1+c)(P[n+1]-P[n]) ]/6;
      b2 := b3 - (1-t)*[ (1+b)(1+c)(P[n+1] - P[n]) + (1-b)(1-c)(P[n+2]-P[n+1]) ] /6;
      b3 := p[n+1]
  }
    K := (1-xTension)*Ratio6;
    Kmc := 1 - xContinuity; // minus c
    Kpc := 1 + xContinuity; // plus c
    Kmb := 1 - xBias; // minus b
    Kpb := 1 + xBias; // plus b

    k11 := k*Kpb*Kmc;
    K12 := k*Kmb*Kpc;
    k21 := k*Kpb*Kpc;
    K22 := k*Kmb*Kmc;
  {
    b1 := b0 + (k11 * (p[n] - p[n-1]) + k12 * (p[n+1] - p[n])
    b2 := b3 - (k21 * (p[n+1] - p[n]) + k22 * (p[n+2] - p[n+1])
  }

   if xClosed then SetLength(yPP, N*3 + 1)
              else SetLength(yPP, (N - 1)*3 + 1);


    { -- load first segment :}
    yPP[0] := xPoints[0];
    if xClosed then
      begin
      yPP[1].x := yPP[0].x + round(K11*(xPoints[0].x - xPoints[N-1].x) + K12*(xPoints[1].x - xPoints[0].x));
      yPP[1].y := yPP[0].y + round(K11*(xPoints[0].y - xPoints[N-1].y) + K12*(xPoints[1].y - xPoints[0].y));
      end
    else
      begin
      yPP[1] := xPoints[0];
      end;
    yPP[3] := xPoints[1];
    yPP[2].x := yPP[3].x - round(K21*(xPoints[1].x - xPoints[0].x) + k22*(xPoints[2].x - xPoints[1].x));
    yPP[2].y := yPP[3].y - round(K21*(xPoints[1].y - xPoints[0].y) + k22*(xPoints[2].y - xPoints[1].y));
    { -- laod inner segments :}
    for i := 1 to N - 3 do
      begin
      ind := 3*i;
      yPP[ind + 1].x := yPP[ind].x + round(K11*(xPoints[i].x - xPoints[i-1].x) + K12*(xPoints[i+1].x - xPoints[i].x));
      yPP[ind + 1].y := yPP[ind].y + round(K11*(xPoints[i].y - xPoints[i-1].y) + K12*(xPoints[i+1].y - xPoints[i].y));
      yPP[ind + 2].x := xPoints[i+1].x - round(K21*(xPoints[i+1].x - xPoints[i].x) + k22*(xPoints[i+2].x - xPoints[i+1].x));
      yPP[ind + 2].y := xPoints[i+1].y - round(K21*(xPoints[i+1].y - xPoints[i].y) + k22*(xPoints[i+2].y - xPoints[i+1].y));
      yPP[ind + 3] := xPoints[i+1];
      end;
    { -- load last segment : }
    ind := 3*(N-2);
    { yPP[ind] := xPoints[N-2]; already assigned ; i = N -2}
    yPP[ind + 1].x := yPP[ind].x + round(K11*(xPoints[N-2].x - xPoints[N-3].x) + K12*(xPoints[N-1].x - xPoints[N-2].x));
    yPP[ind + 1].y := yPP[ind].y + round(K11*(xPoints[N-2].y - xPoints[N-3].y) + K12*(xPoints[N-1].y - xPoints[N-2].y));
    if xClosed then
      begin
      {if i = n - 2 then i + 2 = 0}
      yPP[ind + 2].x := xPoints[N-1].x - round(K21*(xPoints[N-1].x - xPoints[N-2].x) + k22*(xPoints[0].x - xPoints[N-1].x));
      yPP[ind + 2].y := xPoints[N-1].y - round(K21*(xPoints[N-1].y - xPoints[N-2].y) + k22*(xPoints[0].y - xPoints[N-1].y));
      end
    else
      begin
      yPP[ind + 2] := xPoints[n-1];
      end;
    yPP[ind + 3] := xPoints[n-1];

    { now, if closed connect first and last points by curve segments }
    if xClosed   then
      begin
      ind := 3*(N-1);
      yPP[ind + 1].x := yPP[ind].x + round(K11*(xPoints[N-1].x - xPoints[N-2].x) + K12*(xPoints[0].x - xPoints[N-1].x));
      yPP[ind + 1].y := yPP[ind].y + round(K11*(xPoints[N-1].y - xPoints[N-2].y) + K12*(xPoints[0].y - xPoints[N-1].y));
      yPP[ind + 2].x := xPoints[0].x - round(K21*(xPoints[0].x - xPoints[N-1].x) + k22*(xPoints[1].x - xPoints[0].x));
      yPP[ind + 2].y := xPoints[0].y - round(K21*(xPoints[0].y - xPoints[N-1].y) + k22*(xPoints[1].y - xPoints[0].y));
      yPP[ind + 3] := xPoints[0];
      end;
end;



procedure gTCBSpline(xBitmap : tBitmap32;
                          const xPoints : tArrayOfFixedPoint;
                          const xTension : double;
                          const xContinuity : double;
                          const xBias : double;
                          const xColor : tColor32;
                          const xClosed : boolean;
                          const xOptions : tPolygonDrawOptions = pdoFloat);
var
  PP : tArrayOfFixedPoint;
begin
  BuildTCBSplineCurve(xPoints, xTension, xContinuity, xBias, xClosed, PP);
  gPolyBezier(xBitmap, PP, xColor, xOptions, xClosed);
  PP := nil;
end;

procedure gPolyTCBSpline(xBitmap : tBitmap32;
                          const xPoints : tArrayOfArrayOfFixedPoint;
                          const xTension : double;
                          const xContinuity : double;
                          const xBias : double;
                          const xColor : tColor32;
                          const xClosed : boolean;
                          const xOptions : tPolygonDrawOptions = pdoFloat);
var
  i : integer;
  PP : tArrayOfArrayOfFixedPoint;
begin
  SetLength(PP, Length(xPoints));
  for i := 0 to Length(xPoints) - 1 do
    begin
    BuildTCBSplineCurve(xPoints[i], xTension, xContinuity, xBias, xClosed, PP[i]);
    end;
  gPolyPolyBezier(xBitmap, PP, xColor, xOptions, xClosed);
  PP := nil;
end;

procedure CalculateRoundsArc(const p1, p2, p3 : tFixedPoint;
                             const xR : GR32.tFixed;
                             out p21, p22 : tFixedPoint; // end points of arc
                             out yC : tFixedPoint;       // center of arc
                             out yStartAngle, yEndAngle :double);
var
  d  : double; { distance from p2 to p21 = distance from p2 to p22 }
  f  : double; { distance from p2 to yC - center }
  A, B, C : double; { angles }
  SinB, SinC, CosB, CosC : extended;
  SinAB, CosAB : extended;

  CB : double;
  SignCB : TValueSign;
  absCB : double;
begin
  {we must remember, that y axes inverted}
  if PointsAreEqual(p1, p2) or PointsAreEqual(p2, p3) or PointsAreEqual(p1,p3) then
    begin
    yStartAngle := 0;
    yEndAngle := 0;
    p21 := p2; p22 := p2; yC := p2;
    exit;
    end;

  B := arctan2(p2.Y - p3.Y, p3.x - p2.X);
  C := arctan2(p2.Y - p1.Y, p1.x - p2.X);

  if (C < 0)  then C  := pi2 + C;
  if (B < 0)  then  B := pi2 + B;

  CB := C - B;
  SignCB := Sign(CB);
  AbsCB  := abs(CB);

  A := 0.5*(C + B); // биссектриса в сторну меньшего угла
  if absCB > pi then
    begin
    if A >= pi then A := A - pi
    else A := pi + A;
    end;

  d := abs(xR*Cotan(A-B));
  f := abs(xR*Cosecant(A-B));

  SinCos(B, SinB, CosB);
  SinCos(C, SinC, CosC);
  SinCos(A, SinAB, CosAB);

  p21.x := p2.x + round(d*CosC); p21.y := p2.y - round(d*SinC);
  p22.x := p2.x + round(d*CosB); p22.y := p2.y - round(d*SinB);
  yC.x := p2.x  + round(f*CosAB); yC.y := p2.y - round(f*SinAB);

  if absCB < pi then
    begin
    yStartAngle := C + SignCB*PIdiv2;
    yEndAngle   := yStartAngle + SignCB*(pi - absCB);
    end
  else
    begin
    yStartAngle := C - SignCB*PIdiv2;
    yEndAngle   := yStartAngle + SignCB*(pi - absCB);
    end;
end;


{ asumed that p1 point alway in yPP, we shuld add p21, p22 and two control point }
procedure LoadRoundsCurve(const p1, p2, p3 : tFixedPoint;
                          const xR : GR32.tFixed;
                          var yPP : TArrayOfFixedPoint;
                          const xAddLast : boolean = true);
var
  p21, p22, vC : tFixedPoint;
  vStartAngle, vEndAngle : double;

  SI : integer; // start index
begin
  SI := Length(yPP);

  if PointsAreEqual(p1, p2) then
    begin
    { DONE : Treat this situation }
    AFP_AddPoint(yPP, p1);
    exit;
    end;

  if  PointsAreEqual(p2, p3) then
    begin
    { DONE : Treat this situation }
    AFP_AddPoint2(yPP, p2, p2);
    exit;
    end;
  CalculateRoundsArc(p1, p2, p3, xR, p21, p22, vC, vStartAngle, vEndAngle);
  if SI = 0 then
    begin
    AFP_AddPoint(yPP, p21);
    end
  else
    begin
    AFP_AddPoint2(yPP, p21, p21);
    end;

  LoadArcCurve(vC, xR, xR, vStartAngle, vEndAngle, yPP);

  if xAddLast then
    begin
    AFP_AddPoint(yPP, p22);
    end;
end;

procedure gDrawSymbol(xBitmap : tBitmap32;
                      const xP      : GR32.tFixedPoint;
                      const xSymbol : tSymbolKind;
                      const xSize   : GR32.tFixed;
                      const xColor  : tColor32;
                      const xOptions : tPolygonDrawOptions );
var
  xRect : tFixedRect;
  PP  : TArrayOfFixedPoint;
begin
  xRect.Left := xP.x - xSize ; xRect.Top := xP.y - xSize;
  xRect.Right := xP.x + xSize; xRect.Bottom := xP.y + xSize;

  case xSymbol of
    skCircle:
      begin
      gEllipse(xBitmap, xRect, xColor, xOptions);
      end;
    skSquare:
      begin
      SetLength(PP, 4);
      PP[0] := xRect.TopLeft;
      PP[1].x := xRect.Right;  PP[1].y := xRect.Top;
      PP[2] := xRect.BottomRight;
      PP[3].x := xRect.Left;  PP[3].y := xRect.Bottom;
      gPolygon(xBitmap, PP, xColor, xOptions, true);
      end;
    skTriangle:
      begin
      SetLength(PP, 3);
      PP[0].x := xRect.Left; PP[0].y := xRect.Bottom;
      PP[1].x := xP.x; PP[1].y := xRect.Top;
      PP[2].x := xRect.Right; PP[2].y := xRect.Bottom;
      gPolygon(xBitmap, PP, xColor, xOptions, true);
      end;
    skPlus:
      begin
      xBitmap.LineXS(xRect.Left, xP.y - HalfPixel,  xRect.Right, xP.y - HalfPixel, xColor);
      xBitmap.LineXS(xP.x - HalfPixel, xRect.Top,   xP.x - HalfPixel,  xRect.Bottom, xColor)
      end;
    skX:
      begin
      xBitmap.LineXS(xRect.Left +  PixelInFixed, xRect.Top +  PixelInFixed,
                     xRect.Right, xRect.Bottom, xColor);
      xBitmap.LineXS(xRect.Left + PixelInFixed, xRect.Bottom - PixelInFixed,
                     xRect.Right, xRect.Top , xColor);
      end;
    skStar:
      begin
      xBitmap.LineXS(xRect.Left +  PixelInFixed, xRect.Top +  PixelInFixed,
                     xRect.Right, xRect.Bottom, xColor);
      xBitmap.LineXS(xRect.Left + PixelInFixed, xRect.Bottom - PixelInFixed,
                     xRect.Right, xRect.Top , xColor);
      xBitmap.LineXS(xP.x, xRect.Top + PixelInFixed,   xP.x,  xRect.Bottom + PixelInFixed, xColor);
      end;
  end;
end;

procedure gDrawSymbols(xBitmap : tBitmap32;
                        const  xPoints : tArrayOfFixedPoint;
                        const  xSymbol : tSymbolKind;
                        const  xSize   : GR32.tFixed;
                        const  xColor  : tColor32;
                        const  xOptions : tPolygonDrawOptions);
var
  i : integer;
begin
  for i := 0 to Length(xPoints) - 1 do
    begin
    gDrawSymbol(xBitmap, xPoints[i], xSymbol, xSize, xColor, xOptions);
    end;
end;

procedure gPolygon(Bitmap: TBitmap32;
                   const Points: TArrayOfFixedPoint;
                   const Color: TColor32;
                   const Options : tPolygonDrawOptions;
                   const Closed: Boolean;
                   const FillMode: TPolyFillMode = pfAlternate);
begin
        if (Options and  pdoFloat)  = pdoFloat then
        begin
        { автоматически - Antialiasing }
        if ByteBool(Options and pdoFilling) then
          begin
          if (Options and  pdoFastFilling) = pdoFastFilling then
            begin
            PolygonTS(Bitmap, Points,   Color, FillMode); // rus: заполнение без сглаживания
                                                          // eng: fill without antialising
            PolylineXS(Bitmap, Points,   Color, true); // rus: граница со сглаживанием
                                                       // eng: border with antialising
            end
          else
            begin
            PolygonXS(Bitmap, Points, Color, FillMode);
            end;
          end
        else
          begin
          PolylineXS(Bitmap, Points, Color, Closed);
          end
        end
      else
        begin
        { rus: используем целочисленную арифметику }
        { eng: use integer-based methods }
        if ByteBool(Options and pdoAntialising) then
          begin
          if ByteBool(Options and pdoFilling) then PolygonXS(Bitmap, Points, Color, FillMode) //и автоматически включаем дробную арифметику
                                      else PolylineAS(Bitmap, Points,   Color, Closed)
          end
        else
          begin
          if ByteBool(Options and pdoFilling) then PolygonTS(Bitmap, Points, Color, FillMode)
                                      else PolylineTS(Bitmap, Points,   Color, Closed);
          end
        end;
end;


var
  gv_StyledPolygon : TPolygon32;




procedure gPolygon_Styled(xBitmap: TBitmap32;
                       const xPoints: TArrayOfFixedPoint;
                       const xClosed: Boolean;
                       const xPenData : tPen32Data;
                       const xBrushData : tBrush32Data;
                       const xAntialised : boolean = true;
                       const xFillMode: TPolyFillMode = pfAlternate);
var
  OutlineP : TPolygon32;
  TmpPoly: TPolygon32;

  PP : TArrayOfArrayOfFixedPoint;
begin
  PP := gv_StyledPolygon.Points;

  SetLength(PP, 1);
  gv_StyledPolygon.Points[0] := xPoints;
  gv_StyledPolygon.Closed := xClosed;

  TmpPoly := gv_StyledPolygon.Outline;
  OutlineP := nil;
  try
    OutlineP := TmpPoly.Grow(xPenData.Width, xPenData.EdgeSharpness);

    OutlineP.FillMode := pfWinding;
  finally
    TmpPoly.Free;
  end;

  OutlineP.Antialiased := xAntialised;
  gv_StyledPolygon.Antialiased := xAntialised;

  if xBrushData.Style = gr_bsSolid then
    begin
    gv_StyledPolygon.Closed := xClosed;
    gv_StyledPolygon.FillMode := xFillMode;
    gv_StyledPolygon.DrawFill(xBitmap, xBrushData.Color);
    end;

  OutlineP.Closed := xClosed;
  OutlineP.DrawFill(xBitmap, xPenData.Color);

  if Assigned(OutlineP) then OutlineP.Free;
  PP := nil;
end;


procedure gPolyPolygon_Styled(xBitmap: TBitmap32;
                       const xPoints: TArrayOfArrayOfFixedPoint;
                       const xClosed: Boolean;
                       const xPenData : tPen32Data;
                       const xBrushData : tBrush32Data;
                       const xAntialised : boolean = true;
                       const xFillMode: TPolyFillMode = pfAlternate);
var
  OutlineP : TPolygon32;
  TmpPoly: TPolygon32;
begin

  gv_StyledPolygon.Points := xPoints;
  gv_StyledPolygon.Closed := xClosed;

  TmpPoly := gv_StyledPolygon.Outline;
  OutlineP := TmpPoly.Grow(xPenData.Width, xPenData.EdgeSharpness);

  OutlineP.FillMode := xFillMode;
  TmpPoly.Free;

  OutlineP.Antialiased := xAntialised;
  gv_StyledPolygon.Antialiased := xAntialised;

  if xBrushData.Style = gr_bsSolid then
    begin
    gv_StyledPolygon.Closed := xClosed;
    gv_StyledPolygon.FillMode := xFillMode;
    gv_StyledPolygon.DrawFill(xBitmap, xBrushData.Color);
    end;

  OutlineP.Closed := xClosed;
  OutlineP.DrawFill(xBitmap, xPenData.Color);

end;





procedure gPolyPolygon(xBitmap: TBitmap32;
                   const xPoints: TArrayOfArrayOfFixedPoint;
                   const xColor: TColor32;
                   const xOptions : tPolygonDrawOptions;
                   const xClosed: Boolean;
                   const xFillMode: TPolyFillMode = pfAlternate);
begin

        if (xOptions and  pdoFloat)  = pdoFloat then
        begin
        { Automatically - Antialiasing }
        if ByteBool(xOptions and pdoFilling) then
          begin
          if (xOptions and  pdoFastFilling) = pdoFastFilling then
            begin
            PolyPolygonTS(xBitmap, xPoints,   xColor, xFillMode); // Filling without smoothing
            PolyPolylineXS(xBitmap, xPoints,   xColor, true);    // Border with smoothing
            end
          else
            begin
            PolyPolygonXS(xBitmap, xPoints, xColor, xFillMode);
            end;
          end
        else
          begin
          PolyPolylineXS(xBitmap, xPoints, xColor, xClosed);
          end
        end
      else
        begin
          { We use integer arithmetics }
        if ByteBool(xOptions and pdoAntialising) then
          begin
          if ByteBool(xOptions and pdoFilling) then PolyPolygonXS(xBitmap, xPoints, xColor, xFillMode) //And automatically we include fractional arithmetics
                                      else PolyPolylineAS(xBitmap, xPoints,   xColor, xClosed)
          end
        else
          begin
          if ByteBool(xOptions and pdoFilling) then PolyPolygonTS(xBitmap, xPoints, xColor, xFillMode)
                                      else PolyPolylineTS(xBitmap,xPoints,   xColor, xClosed);
          end
        end;

end;


type
   tRelyativeRgn = (rrLeft, rrRight, rrTop, rrBottom, rrInside);
 { rus: относительное положение }
 { eng: relyative situation .. }

{.. on rectangle }
function GetRelRgn(const xP: tFixedPoint; const xR : TFixedRect) : tRelyativeRgn;
begin
  { rus: приоритет - по правой границе (наиболее вероятной) }
  { eng: right side is most probable side}
  if (xP.x > xR.Right) then
     begin
     result := rrRight; exit;
     end;
  if (xP.y > xR.Bottom ) then
     begin
     result := rrBottom;
     exit;
     end;
  if (xP.x < xR.Left) then
     begin
     result := rrLeft;
     exit;
     end;
  if (xP.y < xR.Top) then
     begin
     result := rrTop;
     exit;
     end;
  result := rrInside
end;



{ rus: возвращает true, если доподлино известно, что точки находятся за пределами прямоугольника
  условное правило (достаточное правило):
  ломанная лежит за пределами прямоугольника,  если все ее точки находятся по одну сторону прямоугольника
  например, все снизу.
 }
{eng:
 Returns true if dopodlino it is known, that points are outside a rectangular a conditional rule (a sufficient rule): 
 lomannaja all lays outside a rectangular if all its points are on one party of a rectangular for example, from below.
}
{ eng: return true, if surely knowns all points lays out of rectangle }
function ClipPolyOutside(const xPoints : TArrayOfFixedPoint; const xR : TFixedRect) : boolean;
var
  OldRgn : tRelyativeRgn;
  i : integer;
begin
  result := true;
  if Length(xPoints) = 0 then exit;

  OldRgn := GetRelRgn(xPoints[0], xR);

  if (OldRgn = rrInside) then
    begin
    result := false;
    exit;
    end;

  if Length(xPoints) = 1 then exit;  { OldRgn <> rrInside }

  for i := 1 to Length(xPoints) - 1 do
    begin
    if OldRgn <> GetRelRgn(xPoints[i], xR) then
      begin
      result := false;
      exit;
      end;
    end;
end;

//{ rus: возвращает true, если доподлино известно, что точки находятся за пределами прямоугольника}
{ eng: return true, if surely knowns all four points lays out of rectangle }
function ClipRectOutside(const p0, p1, p2, p3 : tFixedPoint; const xR : TFixedRect) : boolean; overload;
var
  OldRgn : tRelyativeRgn;
begin
  //result := true;

  OldRgn := GetRelRgn(p0, xR);

  if (OldRgn = rrInside) then
    begin
    result := false;
    exit;
    end;

  result := (OldRgn = GetRelRgn(p1, xR)) and (OldRgn = GetRelRgn(p2, xR)) and (OldRgn = GetRelRgn(p3, xR));
end;

{ возвращает true, если доподлино известно, что точки находятся за пределами прямоульника }
{ eng: return true, if surely knowns all three points lays out of rectangle }
function ClipRectOutside(const p0, p1, p2 : tFixedPoint; const xR : TFixedRect) : boolean; overload;
var
  OldRgn : tRelyativeRgn;
begin
  //result := true;

  OldRgn := GetRelRgn(p0, xR);

  if (OldRgn = rrInside) then
    begin
    result := false;
    exit;
    end;

  result := (OldRgn = GetRelRgn(p1, xR)) and (OldRgn = GetRelRgn(p2, xR));
end;


{
  CurvePoints -  список точек аппроксимирующий кубическую кривую безъё при вызове функции LoadApproxCurve;
                 чтобы выделять каждый раз необходимуя память сделан глобальным для модуля и реализованы
                 интерфейсные фунции СP_xxx
}

{  eng:   СurvePoints - is a storage of linear approximation of current qubic Bezier curve
}

const
  { rus: начальное количество точек (ёмкость), а также прирощение }
  MaxCurvePointsCount  = 512;
var
 { глобальный список точек, что бы не выделять память кажый раз }
 //Initial quantity of points (capacity), and also priroshchenie
  CurvePoints : TArrayOfFixedPoint; {array of TFixedPoint;}

  CurvePointsCount : integer;

  function g32i_GetCurvePointsCount : integer;
  begin
    result := CurvePointsCount;
  end;

  function g32i_GetCurvePointsMaxCount : integer;
  begin
    result := Length(CurvePoints);
  end;

  { eng: pretends only ! }
  { делаем вид, что очищаем точку }
  procedure CP_Clear;
  begin
    CurvePointsCount := 0;
  end;

  procedure CP_Dispose;
  begin
    CP_Clear;
    SetLength(CurvePoints, 0);
  end;

  { We add a point}
  procedure CP_Add(const p : tFixedPoint);
  begin
    {rus: без проверки переполнения! }
    {eng:  !!! without overflow cheking !!! }
    CurvePoints[CurvePointsCount] := p;
    inc(CurvePointsCount);
  end;


{ rus: подготовка списка точек кривой
  формирует список точек ломанной, аппроксимирующей кривую Безье,
  с учётом видимости (xClipRect) и минимальной длины кривой }

{ eng: Prepare list of points approximating  qubuce Bezier curve with clipping }

procedure BuildApproxCurve(const xPoints: TArrayOfFixedPoint;
                               const xMinSegmentLength : GR32.tFixed;
                               const xClipRect : tFixedRect
                               );
var
  i : integer;

   {
   We break a segment x0, x1, x2, x3 into necessary quantity of times, filling in the list Ѓ0€5urvePoints [CurvePointsCount]
     function recursive conditions: 
       x0 - already dobavlenyj unit 
       x1, x2 - managing points which will be or are added, or on the basis of which will be vychesliny others 
       x3 - unit which in any case will be added
   }
  { function is recoursive
    conditional:
    x0 - already added node
    x1, x2 are control points of segments, that can my added
    x3 - node, than should be added
    }
  procedure BreakSegment(const p0, p1, p2, p3 : tFixedPoint; xNeedClip : boolean);
  var
    p11, p21, p31, p22, p32, p33 : tFixedPoint;
  begin
       { если сегмент вырожденный или мы достигли предела памяти, то заканчиваем }
       { if segment is singular - no need to break  }
       if ((p0.x = p1.x) and (p0.y = p1.y) and (p2.x = p3.x) and (p2.y = p3.y)) then
         begin
          {$IFDEF OOPTIMIZE_CALLFUNCTIONS_CP_ADD}
            CurvePoints[CurvePointsCount] := p1;
            CurvePoints[CurvePointsCount+1] := p2;
            CurvePoints[CurvePointsCount+2] := p3;
            inc(CurvePointsCount, 3);
          {$ELSE}
            CP_Add(p1); CP_Add(p2); CP_Add(p3);
          {$ENDIF}
            exit;
         end;
      { if segment is out of clipping rect - no need break}
      if xNeedClip and ClipRectOutside(p0, p1, p2, p3, xClipRect) then
        begin
         { разбивать не нужно }
         {$IFDEF OPTIMIZE_CALLFUNCTIONS_CP_ADD}
            CurvePoints[CurvePointsCount] := p1;
            CurvePoints[CurvePointsCount+1] := p2;
            CurvePoints[CurvePointsCount+2] := p3;
            inc(CurvePointsCount, 3);
         {$ELSE}
           CP_Add(p1); CP_Add(p2); CP_Add(p3);
         {$ENDIF}
         exit;
        end;

      { если точки заканчиваются, то нужно подрастить множество точек }
      { if need  - grow points list }
      if (CurvePointsCount >=  Length(CurvePoints) div 2 - 4) then    // ! div 2 - посколку функция рекурсивная
        begin
          try
            SetLength(CurvePoints, MaxCurvePointsCount + length(CurvePoints));
          except
           {$IFDEF OPTIMIZE_CALLFUNCTIONS_CP_ADD}
            CurvePoints[CurvePointsCount] := p1;
            CurvePoints[CurvePointsCount+1] := p2;
            CurvePoints[CurvePointsCount+2] := p3;
            inc(CurvePointsCount, 3);
          {$ELSE}
            CP_Add(p1); CP_Add(p2); CP_Add(p3); { ? может всё таки следует продолжать }
           {$ENDIF}
            exit;
          end
        end;

        { rus: 1. вычисляем положение нового узла и еще четырёх управляющих точек }
        { end: 1. calculate situation of new one node and new four control poits of new two segment }
        {$IFDEF OPTIMIZE_CALLFUNCTIONS_MDL}
          p11.x := (p1.x + p0.x) div 2;          p11.y := (p1.y + p0.y) div 2;
          p21.x := (p2.x + p1.x) div 2;          p21.y := (p2.y + p1.y) div 2;

          p31.x := (p3.x + p2.x) div 2;          p31.y := (p3.y + p2.y) div 2;
          p22.x := (p11.x + p21.x) div 2;        p22.y := (p11.y + p21.y) div 2;
          p32.x := (p21.x + p31.x) div 2;        p32.y := (p21.y + p31.y) div 2;

          p33.x := (p22.x + p32.x) div 2;          p33.y := (p22.y + p32.y) div 2;
        {$ELSE}
          p11 := MiddleLine(p1, p0);      { 1я управляющая точка первого нового сегмента }
          p21 := MiddleLine(p2, p1 );

          p31 := MiddleLine(p3 ,p2 );     { 2я управляющая точка второго нового сегмента }
          p22 := MiddleLine(p11 , p21 );  { 2я управляющая точка первого нового сегмента }
          p32 := MiddleLine(p21 , p31 );  { 1я управляющая точка второго нового сегмента }
          { new node }
          p33 := MiddleLine(p22 , p32);   { это новый узел, который делит текущий сегмент на два новых }
        {$ENDIF}

        { будем разбивать сегмент до тех пор пока условная длина сегмента не снизится до требуемой }
        { will break until conditional length of segment is too large  }
        if SegmentConditionalLengthQ3N1Sup(p0, p11, p22, p33) > xMinSegmentLength then
           begin
           BreakSegment(p0, p11, p22, p33, xNeedClip)
           end
        else
          begin
          { новый сегмент }
          {$IFDEF OPTIMIZE_CALLFUNCTIONS_CP_ADD}
            CurvePoints[CurvePointsCount] := p11;
            CurvePoints[CurvePointsCount+1] := p22;
            CurvePoints[CurvePointsCount+2] := p33;
            inc(CurvePointsCount, 3);
          {$ELSE}
            CP_Add(p11);
            CP_Add(p22);
            CP_Add(p33);
          {$ENDIF}
          end;
        { p33 - новый узел ; утв: p33 уже добавлен }
        { p33 - new nodes - already added }
        if SegmentConditionalLengthQ3N1Sup(p33, p32, p31, p3) > xMinSegmentLength then
          begin
          BreakSegment(p33, p32, p31, p3, xNeedClip)
          end
        else
          begin
          {$IFDEF OPTIMIZE_CALLFUNCTIONS_CP_ADD}
            CurvePoints[CurvePointsCount] := p32;
            CurvePoints[CurvePointsCount+1] := p31;
            CurvePoints[CurvePointsCount+2] := p3;
            inc(CurvePointsCount, 3);
          {$ELSE}
            CP_Add(p32);
            CP_Add(p31);
            CP_Add(p3);
          {$ENDIF}
          end
  end;

begin
   CP_Clear;
   if Length(xPoints) = 0 then exit;

      i := 0;
      CP_Add(xPoints[0]);
      while i  <= Length(xPoints) - 4 do
        begin
        {для каждого из сегментов : }
        { (i) - узел1, (i+1) - 1я управляющая точка, (i+2) - 2я управляющая точка, (i+3) - cледующий узел   }
        BreakSegment(xPoints[i], xPoints[i+1], xPoints[i+2], xPoints[i+3], not IsNullRect(xClipRect));
        inc(i, 3);
        end;
end;

procedure gPolyBezier(Bitmap: TBitmap32;
                       const Points: TArrayOfFixedPoint;
                       const Color: TColor32;
                       const Options : tPolygonDrawOptions;
                       const Closed: Boolean;
                       const FillMode: TPolyFillMode = pfAlternate);

var
  PP : TArrayOfFixedPoint;
  i  : integer;
  PPCount : integer;
begin

  if (Length(Points) <= 1) or ((Length(Points) - 1) mod 3 <> 0) then exit;

  { TODO : на самом деле - неправильный алгоритм - поскольку кривая может попадать в область отображения, даже когда все её
    точки лежат за пределами }
  { eng: it's not truth algorithm for clipping, because even all points are out of clipping rect then is not means than
  any part of curve is invisible }
  if ClipPolyOutside(Points, FixedRect(rect(0, 0, Bitmap.Width, Bitmap.Height))) then exit;

  { rus: теперь строим точки полинома получившейся кривой }
  BuildApproxCurve(Points,  Bezier3SegmentMinLengthInPixel shl 16, FixedRect(rect(0, 0, Bitmap.Width, Bitmap.Height)));

  { rus: удаляем управляющие точки }
  { eng: delete control points }
  PPCount := (CurvePointsCount - 1) div 3 + 1;
  SetLength(PP, PPCount);
  for i := 0 to PPCount - 1 do
    begin
    PP[i] := CurvePoints[3*i];
    end;

  gPolygon(Bitmap,
           PP,
           Color,
           Options,
           Closed,
           FillMode);
end;

procedure gPolyBezier_Styled(xBitmap: TBitmap32;
                       const xPoints: TArrayOfFixedPoint;
                       const xClosed: Boolean;
                       const xPenData : tPen32Data;
                       const xBrushData : tBrush32Data;
                       const xAntialised : boolean = true;
                       const xFillMode: TPolyFillMode = pfAlternate);
var
  PP : TArrayOfFixedPoint;
  i  : integer;
  PPCount : integer;
begin

  if (Length(xPoints) <= 1) or ((Length(xPoints) - 1) mod 3 <> 0) then exit;

  { eng: it's not truth algorithm for clipping, because even all points are out of clipping rect then is not means than
  any part of curve is invisible }
  if ClipPolyOutside(xPoints, FixedRect(rect(0, 0, xBitmap.Width, xBitmap.Height))) then exit;

  BuildApproxCurve(xPoints,  Bezier3SegmentMinLengthInPixel shl 16, FixedRect(rect(0, 0, xBitmap.Width, xBitmap.Height)));

  { eng: delete control points }
  PPCount := (CurvePointsCount - 1) div 3 + 1;
  SetLength(PP, PPCount);
  for i := 0 to PPCount - 1 do
    begin
    PP[i] := CurvePoints[3*i];
    end;

  gPolygon_Styled(xBitmap, PP, xClosed, xPenData, xBrushData, xAntialised, xFillMode);
end;


procedure gPolyPolyBezier(Bitmap: TBitmap32;
                     const Points: TArrayOfArrayOfFixedPoint;
                     const Color: TColor32;
                     const Options : tPolygonDrawOptions;
                     const Closed: Boolean;
                     const FillMode: TPolyFillMode = pfAlternate);

var
  PP : TArrayOfArrayofFixedPoint;
  i, j  : integer;
  PPCount : integer;
begin
  SetLength(PP, Length(Points));

  for j := 0 to Length(Points) - 1 do
    begin
    if (Length(Points[j]) <= 1) or ((Length(Points[j]) - 1) mod 3 <> 0) then continue;

    { eng: clipping... }
    if ClipPolyOutside(Points[j], FixedRect(rect(0, 0, Bitmap.Width, Bitmap.Height))) then exit;

    { rus: теперь строим точки полинома получившейся кривой }
    { eng: aproximate... }
    BuildApproxCurve(Points[j],  Bezier3SegmentMinLengthInPixel shl 16, FixedRect(rect(0, 0, Bitmap.Width, Bitmap.Height)));

    { rus: удаляем управляющие точки }
    { eng: delete control points.. }
    PPCount := (CurvePointsCount - 1) div 3 + 1;
    SetLength(PP[j], PPCount);
    for i := 0 to PPCount - 1 do
      begin
      PP[j, i] := CurvePoints[3*i];
      end;
    end;

  {eng: display...}
  gPolyPolygon(Bitmap,
           PP,
           Color,
           Options,
           Closed,
           FillMode);
end;


procedure LoadRoundedPolygonAsCurve(const Points: TArrayOfFixedPoint;
                             const Radius : GR32.tFixed;
                             const Closed : boolean;
                             var yPP : TArrayOfFixedPoint);
var
  i : integer;
  N : integer;
begin
  N := Length(Points);

  SetLength(yPP, 0);

  if N <= 2 then
    begin
    if N <= 1 then exit;
    AFP_AddPoint2(yPP, Points[0], Points[0]);
    AFP_AddPoint2(yPP, Points[1], Points[1]);
    exit;
    end;

  if not Closed then  AFP_AddPoint2(yPP, Points[0], Points[0]);

  for i := 0 to N-3 do
      begin
      { вызывая в первый раз, процедура добавляет две точки p21, но нужно её вообще-то удалить }
      LoadRoundsCurve(Points[i], Points[i+1], Points[i+2], Radius, yPP);
      end;

 if not Closed then
    AFP_AddPoint2(yPP, Points[N-1], Points[N-1])
 else
    begin
      LoadRoundsCurve(Points[N-2], Points[N-1], Points[0], Radius, yPP);
      if not PointsAreEqual(Points[N-1], Points[0]) then
        LoadRoundsCurve(Points[N-1], Points[0], Points[1], Radius, yPP, false);
    end;
end;


procedure gPolygonRounded(xBitmap: TBitmap32;
                     const xPoints: TArrayOfFixedPoint;
                     const xRadius : GR32.tFixed;
                     const xColor: TColor32;
                     const xOptions : tPolygonDrawOptions;
                     const xClosed: Boolean;
                     const xFillMode: TPolyFillMode = pfAlternate);
var
  vPP : TArrayOfFixedPoint;
begin
  LoadRoundedPolygonAsCurve(xPoints, xRadius, xClosed, vPP);

  gPolyBezier(xBitmap, vPP, xColor, xOptions, false, xFillMode);
  vPP := nil;
end;

procedure gPolyPolygonRounded(xBitmap: TBitmap32;
                     const xPoints: TArrayOfArrayOfFixedPoint;
                     const xRadius : GR32.tFixed;
                     const xColor: TColor32;
                     const xOptions : tPolygonDrawOptions;
                     const xClosed: Boolean;
                     const xFillMode: TPolyFillMode = pfAlternate);
var
  vPP : TArrayOfArrayOfFixedPoint;
  i   : integer;
  L   : integer;
begin
  L := Length(xPoints);
  SetLength(vPP, L);
  for i := 0 to  L - 1 do
    begin
    LoadRoundedPolygonAsCurve(xPoints[i], xRadius, xClosed, vPP[i]);
    end;
  gPolyPolyBezier(xBitmap, vPP, xColor, xOptions, xClosed, xFillMode);
  vPP := nil;
end;

{
  rus: GlyphPolygon -  список точек аппроксимирующий  контур символа  при вызове функции BuildGlyphPolyPolygon;
                 чтобы выделять каждый раз необходимуя память сделан глобальным для модуля и реализованы
                 интерфейсные фунции PathPoints
}

type
    TFXPArray = array[0..0] of TPOINTFX;
    PFXPArray = ^TFXPArray;

{$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
var
    GlyphPolygonPP : TArrayOfArrayOfFixedPoint;
    GPPointCount  : TArrayOfInteger; // sizes
    GP_Count : integer; // current sizes of GPPointCounts

    function GetGlyphPolygon : TArrayOfArrayOfFixedPoint;
    var
      i,j : integer;
    begin
      SetLength(result, GP_Count);
      for i := 0 to GP_Count - 1 do
        begin
        SetLength(result[i], GPPointCount[i]);
        for j := 0  to GPPointCount[i] - 1 do result[i, j] := GlyphPolygonPP[i, j];
        end;
    end;

    procedure GlyphPolygone_Clear;
    var i : integer;
    begin
      for i := 0 to GP_Count - 1 do GPPointCount[i]:= 0;
      GP_Count := 1;
    end;

    procedure GlyphPolygone_NewLine;
    begin
      inc(GP_Count);
      if GP_Count > Length(GPPointCount) then
        begin
        SetLength(GPPointCount,   GP_Count);
        SetLength(GlyphPolygonPP, GP_Count);
        end;
      GPPointCount[GP_Count - 1] := 0;
    end;

    procedure GlyphPolygone_Add(const xP : tFixedPoint);
    begin
      inc(GPPointCount[GP_Count - 1]);
      if GPPointCount[GP_Count - 1] > Length(GlyphPolygonPP[GP_Count - 1]) then
        SetLength(GlyphPolygonPP[GP_Count - 1], 2*GPPointCount[GP_Count - 1]);
      GlyphPolygonPP[GP_Count - 1, GPPointCount[GP_Count - 1] - 1] := xP;
    end;

    procedure GlyphPolygone_Add2(const xP1, xP2 : tFixedPoint);
    begin
      inc(GPPointCount[GP_Count - 1], 2);
      if GPPointCount[GP_Count - 1] > Length(GlyphPolygonPP[GP_Count - 1]) then
        SetLength(GlyphPolygonPP[GP_Count - 1], 2*GPPointCount[GP_Count - 1]);
      GlyphPolygonPP[GP_Count - 1, GPPointCount[GP_Count - 1] - 2] := xP1;
      GlyphPolygonPP[GP_Count - 1, GPPointCount[GP_Count - 1] - 1] := xP2;
    end;
{$ELSE}
var
  GlyphPolygon : TPolygon32; { PointsContainer }

{$ENDIF}

{ rus : загрузка аппроксимированных контуров символа из буфера }
procedure BuildGlyphPolygon(xBufPtr :  PTTPolygonHeader;     {  rus : указатель на буфер, который содержит информацию о начертании символа }
                            const xBufSize : integer;              {  rus : размер буфера }
                            const xMinSegmentLength : GR32.tFixed; { rus :указываем минимальную условная кривизна (или длина) сегмента до которого нужно разбивать }
                            const xLeft, yTop : GR32.tFixed;       { rus :для указания положения символа, иначе мы не сможет определить область отсечения }
                            const xGM : TGLYPHMETRICS;             { rus :для указания размера и смещени символа относительно базовой линии и т.д }
                            const xCliptRect : GR32.tFixedRect);   { rus :указываем прямоугольник отсечения }


var
   pc : PTTPolyCurve;
   ps, p1, p2 : TFixedPoint;
   ofs, ofs2, pcSize : LongInt;
   done : boolean;
   i : LongInt;
   pfxA, pfxB, pfXC : TFixedPoint;
   lpAPFX : PFXPArray;
   polyN  : LongInt;
   pcType : LongInt;


  { rus: разбиваем сегмент p0, p1, p2  на необходимое количество раз, заполняя список СurvePoints[CurvePointsCount]
    функция рекурсивная
    условия: x0 - уже добавленый узел   (!)
             p1 - управляющие точки, которые будут добавлены
             p2 - узел, который в любом случае будет добавлен

   }

  procedure BreakSegment(const p0, p1, p2 : tFixedPoint);
  var
    p21, p3, p22 : tFixedPoint;
  begin

       { rus: если сегмент вырожденный или мы достигли предела памяти, то заканчиваем }
       if PointsAreEqual(p0, p1) or PointsAreEqual(p1, p2) then
         begin
         { rus: разбивать не нужно }
         {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
           GlyphPolygone_Add2(p1, p2);
           //GlyphPolygone_Add(p1); GlyphPolygone_Add(p2);
         {$ELSE}
           GlyphPolygon.Add(p1); GlyphPolygon.Add(p2);
         {$ENDIF}
         exit;
         end;

      //{ rus: если сегмент достоверно лежит за пределами прямоугольника отсечения, то разбивать не нужно}
      if ClipRectOutside(p0, p1, p2, xCliptRect) then
        begin
         { rus: разбивать не нужно }
         {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
           //GlyphPolygone_Add(p1); GlyphPolygone_Add(p2);
           GlyphPolygone_Add2(p1, p2);
         {$ELSE}
           GlyphPolygon.Add(p1); GlyphPolygon.Add(p2);
         {$ENDIF}
         exit;
        end;

        { rus: 1. вычисляем положение нового узла и 2x управляющих точек }
        {$IFDEF OPTIMIZE_CALLFUNCTIONS_MDL}
          p21.x := (p1.x + p0.x) div 2;          p21.y := (p1.y + p0.y) div 2;
          p22.x := (p2.x + p1.x) div 2;          p22.y := (p2.y + p1.y) div 2;
          p3.x := (p21.x + p22.x) div 2;         p3.y := (p21.y + p22.y) div 2;
        {$ELSE}
          p21 := MiddleLine(p1, p0); {rus:  новая управляющая точка перового нового сегмента p0-p21-p3}
          p22 := MiddleLine(p2, p1); { rus: новая управляющая точка второго нового сегмента p3 - p22 - p2}
          p3  := MiddleLine(p21, p22);
        {$ENDIF}

        { rus: будем разбивать сегмент до тех пор пока условная длина (или кривизна) сегмента не снизится до требуемой }
        if SegmentConditionalLengthQ2N1Sup(p0, p21, p3) > xMinSegmentLength then
           begin
           BreakSegment(p0, p21, p3)
           end
        else
          begin
          { rus: новый сегмент }
          {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
           //GlyphPolygone_Add(p21); GlyphPolygone_Add(p3);
           GlyphPolygone_Add2(p21, p3);
         {$ELSE}
           GlyphPolygon.Add(p21); GlyphPolygon.Add(p3);
         {$ENDIF}
          end;
        { rus: p33 - новый узел ; утв: p33 уже добавлен }
        if SegmentConditionalLengthQ2N1Sup(p3, p22,  p2) > xMinSegmentLength then
          begin
          BreakSegment(p3, p22, p2)
          end
        else
          begin
         {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
           //GlyphPolygone_Add(p22); GlyphPolygone_Add(p2);
           GlyphPolygone_Add2(p22, p2);
         {$ELSE}
           GlyphPolygon.Add(p22); GlyphPolygon.Add(p2);
         {$ENDIF}
          end
  end;

  function xWinFixToG32(const x : _FIXED) : GR32.TFixed;
  begin
     result := X.value*$10000 + x.fract + xLeft;
  end;

  function yWinFixToG32(const y : _FIXED) : GR32.TFixed;
  begin
   result := + y.value* $10000 + y.fract + yTop;
  end;

begin
  {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
     GlyphPolygone_Clear;
  {$ELSE}
     GlyphPolygon.Clear;
  {$ENDIF}
     done := false;
     ofs := 0;
     polyN := 0;

     while not Done do
      begin
           ps.X := xWinFixToG32( xBufPtr^.pfxStart.X );
           ps.Y := yWinFixToG32( xBufPtr^.pfxStart.Y );
           pcSize := xBufPtr^.cb - SizeOf(TTTPOLYGONHEADER);          // rus: размер, который занимает список кривых/полиномов
           pChar(pc) := pChar(xBufPtr) + SizeOf(TTTPOLYGONHEADER);    // rus: pc -  указтель на текущую структуру TTPOLYCURVE
           ofs2 := 0;                                                 // rus: смещение относительно начала списка кривых/полиномов

           p2 := ps;
           if polyN <> 0 then
             begin
              {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
                 GlyphPolygone_NewLine;
              {$ELSE}
                 GlyphPolygon.NewLine;
              {$ENDIF}

             end;
              {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
                 GlyphPolygone_Add(p2);
              {$ELSE}
                 GlyphPolygon.Add(p2);
              {$ENDIF}

           { rus: пока не прочитали весь буфер }
           while not Done and (ofs2 < pcSize) do
            begin
            {rus:  далее следует структура TTPOLYCURVE:
              wType word - тип (сплайн/ломанная)
              cpfx  word - количество точек
              apfx  - массив точек
            }
                 pcType := pc^.wType;
                 case pcType of
                   TT_PRIM_LINE:
                      begin
                           lpAPFX := @pc^.apfx[0];
                           for i := 0 to pc^.cpfx-1 do
                            begin
                                 p1 := p2;
                                 p2.X := xWinFixToG32(lpAPFX^[i].X);
                                 p2.Y := yWinFixToG32(lpAPFX^[i].Y);
                                 if not PointsAreEqual( p1, p2 )  then
                                 {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
                                   GlyphPolygone_Add(p2);
                                {$ELSE}
                                   GlyphPolygon.Add(p2);
                                {$ENDIF}
                            end;
                      end;
                    TT_PRIM_QSPLINE:
                      begin
                           lpAPFX := @pc^.apfx[0];
                           pfxA := p2;
                           for i := 0 to pc^.cpfx-2 do
                            begin
                                 pfxB.X := xWinFixToG32(lpAPFX^[i].X);
                                 pfxB.Y := yWinFixToG32(lpAPFX^[i].Y);
                                 if i < pc^.cpfx-2 then
                                  begin
                                       pfxC.X := xWinFixToG32(lpAPFX^[i+1].X);
                                       pfxC.Y := yWinFixToG32(lpAPFX^[i+1].Y);
                                       pfxC.X := (pfxC.X + pfxB.X) div 2;
                                       pfxC.Y := (pfxC.Y + pfxB.Y) div 2;
                                  end else
                                   begin
                                        pfxC.X := xWinFixToG32(lpAPFX^[i+1].X);
                                        pfxC.Y := yWinFixToG32(lpAPFX^[i+1].Y);
                                   end;
                                 BreakSegment(pfxA, pfxB, pfxC);
                                 pfxA := pfxC;
                            end;
                           p2 := pfxC;
                      end;
                  end;
                 ofs2 := ofs2 + SizeOf(TTTPOLYCURVE) + (pc^.cpfx-1)*SizeOf(TPOINTFX);
                 pChar(pc) := pChar(pc) + SizeOf(TTTPOLYCURVE) + (pc^.cpfx-1)*SizeOf(TPOINTFX);
            end;
           if not Done then
            begin
                 p1 := p2;
                 p2 := ps;
                 ofs := ofs + pcSize + SizeOf(TTTPOLYGONHEADER);
                 Done := (ofs >= (xBufSize - SizeOf(TTTPolygonHeader))) ;
                 if (not PointsAreEqual( p1, p2 )) then
                  {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
                     GlyphPolygone_Add(p2);
                  {$ELSE}
                     GlyphPolygon.Add(p2);
                  {$ENDIF}
                 pChar(xBufPtr) := pChar(pc);
                 inc( polyN );
        end;
      end;
   { TODO : удалить управляющие точки }
   { eng: TODO : remove control points }
end;

{ TBitmap32Ex }

constructor TBitmap32Ex.Create;
begin
  inherited Create;

  fCanvas := tCanvas.Create;
  fCanvas.Handle := Self.Handle;

  fPen      := tPen32.Create;
  fBrush    := tBrush32.Create;

  fDrawOrign.x := 0; fDrawOrign.y := 0;

  fFontMat2 := VertFlip_mat2;
  fLastSumLength := 0;

  Font.Size := 24;
  Font.Name := 'Tahoma';
  Font.Color := clBlack;
  Font.Style := [fsBold];
  UpdateFont;
end;

destructor TBitmap32Ex.Destroy;
begin
  fCanvas.Free;

  fPen.Free;
  fBrush.Free;

  inherited Destroy;
end;

{ rus: рисует один символ шрифта установленого в Font в указанной позиции  }
procedure TBitmap32Ex.DrawGlyph(const xCharCode : longword;
                                const xLeft, yTop: GR32.tFixed;
                                const xColor : tColor32;
                                const xOptions : tPolygonDrawOptions);
var
   Result  : LongWord; 
   bufSize : LongWord;
   bufPtr, buf  : PTTPolygonHeader;
   dc : hDC;
   gm : TGLYPHMETRICS; // rus: информация о расположении буквы

   //fUnicode : boolean; // rus: признвак уникодовской буквы
begin
     { rus: --- I. получить glyph-bufer  символа с помощью GetGlyphOutline для шрифта fFont }
     UpdateFont;  { устанавливаем в текущий контекст текущи шрифт }
     dc := Self.handle; { для удобства и исключения путанницы }

     bufSize := GetGlyphOutline( dc ,xCharCode,GGO_NATIVE,gm,0,nil,fFontMat2 );
     if (bufSize = GDI_ERROR) or (bufSize = 0) then exit;


     bufPtr := AllocMem( bufSize );
     try
       buf := bufPtr;
       Result := GetGlyphOutline( dc, xCharCode,GGO_NATIVE,gm,bufSize, pchar(buf),fFontMat2 );

       if (Result = GDI_ERROR) or (buf^.dwType <> TT_POLYGON_TYPE) then
       begin
           //FreeMem( bufPtr );
         Exit;
       end;

     BuildGlyphPolygon(bufPtr,
                          bufSize,
                          Bezier2SegmentMinLengthInPixel  shl 16,
                          xLeft, yTop,
                          gm,
                          FixedRect(fDrawOrign.x, fDrawOrign.y, GR32.Fixed(Self.Width) + fDrawOrign.x, GR32.Fixed(Self.Height) + fDrawOrign.y)); 

    finally
      FreeMem( bufPtr );
    end;

   {rus:  --- III. отобразить контур в указанной позиции  }
   {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
      gPolyPolygon(Self, GetGlyphPolygon, xColor, xOptions, true);
   {$ELSE}
      gPolyPolygon(Self, GlyphPolygon.Points, xColor, xOptions, true);
   {$ENDIF}


end;

procedure TBitmap32Ex.DrawGlyphW(const xCharCode : longword;
                                const xLeft, yTop: GR32.tFixed;
                                const xColor : tColor32;
                                const xOptions : tPolygonDrawOptions);
var
   Result, bufSize : LongWord;
   bufPtr, buf  : PTTPolygonHeader;
   dc : hDC;
   gm : TGLYPHMETRICS; // rus: информация о расположении буквы
begin
     { rus: --- I. получить glyph-bufer  символа с помощью GetGlyphOutline для шрифта fFont }
     UpdateFont;  
     dc := Self.handle;

     bufSize := GetGlyphOutlineW( dc,xCharCode,GGO_NATIVE,gm,0,nil,fFontMat2 ) ;
     if (bufSize = GDI_ERROR) or (bufSize = 0) then exit;

     bufPtr := AllocMem( bufSize );
     try
       buf := bufPtr;
       Result := GetGlyphOutlineW( dc, xCharCode,GGO_NATIVE,gm,bufSize, pchar(buf),fFontMat2 );

       if (Result = GDI_ERROR) or (buf^.dwType <> TT_POLYGON_TYPE) then
       begin
           //FreeMem( bufPtr );
           Exit;
       end;

       BuildGlyphPolygon(bufPtr, 
                          bufSize, 
                          Bezier2SegmentMinLengthInPixel  shl 16,   
                          xLeft, yTop,          
                          gm,
                          FixedRect(fDrawOrign.x, fDrawOrign.y, GR32.Fixed(Self.Width) + fDrawOrign.x, GR32.Fixed(Self.Height) + fDrawOrign.y));

    finally
      FreeMem( bufPtr );
    end;

   {rus:  --- III. отобразить контур в указанной позиции  }
   {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
      gPolyPolygon(Self, GetGlyphPolygon, xColor, xOptions, true);
   {$ELSE}
      gPolyPolygon(Self, GlyphPolygon.Points, xColor, xOptions, true);
   {$ENDIF}


end;

procedure TBitmap32Ex.RenderTextEx(const xLeft, yBottom: GR32.tFixed;
                                   const xText: string;
                                   const xColor : tColor32;
                                   const xOptions : tPolygonDrawOptions);
var
   vShift : tFloatPoint;

   Res, bufSize : LongWord;
   bufPtr, buf  : PTTPolygonHeader;
   gm1 : TGLYPHMETRICS; // rus: информация о расположении буквы
   //fUnicode : boolean; // rus: признак уникодовской буквы
   i : integer;
   vCharCode : longword;
   vChar     : char;
   vCharSize : Windows.TSize;
   vCharSizeFloat : tFloatPoint;
   vQuality : integer;
   vTextLength : integer;
   BufCap : LongWord;
   vCharOrign : tFloatPoint;
   vFontFloatMatrix : TFloatMatrix;
begin

  vFontFloatMatrix := Mat2ToFloatMatrix(fFontMat2);

  {  --- rus: I. получить glyph-bufer  символа с помощью GetGlyphOutline для шрифта fFont }

     SelectObject(Handle, Font.Handle);
     { TODO : rus: следует проверить является ли  xChar^ Unicode'ой буквой,
     с помощью GetFontUnicodeRanges  и установить  FUNICODE }

     vShift := FloatPoint(0, 0);
     vCharOrign := FloatPoint(0, 0);
     vTextLength := length(xText);

     if vTextLength > 0 then try
       BufCap := 1024;
       GetMem(BufPtr, BufCap);
       for i := 1 to  vTextLength do
         begin
         vChar := xText[i];
         vCharCode := ord(vChar);
         //FUNICODE := UNICODE_SUPPORTS;
         {Alexander Muylaert : Strange thing here, has it any use???}
         { answer (goodok)   : i don't know }
         //if not FUNICODE then  
           bufSize := GetGlyphOutline( Handle, vCharCode, GGO_NATIVE,gm1,0,nil,fFontMat2 {VertFlip_mat2})
         //else  bufSize := GetGlyphOutlineW(Handle, vCharCode, GGO_NATIVE,gm1,0,nil,fFontMat2 {VertFlip_mat2}) 
         ;
         if (bufSize = 0) then begin
          vShift.x := vShift.x + round(gm1.gmCellIncX + 0.5);
          vShift.y := vShift.y - round(gm1.gmCellIncY + 0.5);
          continue;
         end;

          if BufSize > BufCap then
            begin
            ReAllocMem(BufPtr, BufSize);
            BufCap := BufSize;
            end;
          buf := bufPtr;
          //if not FUNICODE then 
            Res := GetGlyphOutline( Handle, vCharCode,GGO_NATIVE,gm1,bufSize, pchar(buf),fFontMat2){VertFlip_mat2}
          //else Res := GetGlyphOutlineW( Handle, vCharCode,GGO_NATIVE,gm1,bufSize, pchar(buf),fFontMat2);//fFontMat2{VertFlip_mat2} )
          ;
          if (res = GDI_ERROR) or (buf^.dwType <> TT_POLYGON_TYPE) then continue;
        { now, build polygon :}
        vQuality := (Bezier2SegmentMinLengthInPixel  shl 16);

        BuildGlyphPolygon(bufPtr, 
                                bufSize,
                                vQuality,
                                xLeft + GR32.Fixed(vShift.x{ + vCharOrign.x}), yBottom +  GR32.Fixed(vShift.y),
                                gm1,
                                FixedRect(fDrawOrign.x,
                                          fDrawOrign.y,
                                          GR32.Fixed(Self.Width) + fDrawOrign.x,
                                          GR32.Fixed(Self.Height) + fDrawOrign.y)); 
        { next glyph shift calculation }
        GetTextExtentPoint32(Handle, @vChar, 1, vCharSize);
        vCharSizeFloat := FloatPoint(vCharSize.cx, 0);
        vCharSizeFloat := TransformFloatPoint(vCharSizeFloat,  vFontFloatMatrix);

        vShift.x := vShift.x + vCharSizeFloat.x;
        vShift.y := vShift.y + vCharSizeFloat.y;//gm1.gmCellIncY;

        { --- rus: III. отобразить контур в указанной позиции  }
       {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
          PolyPolygon(GetGlyphPolygon, xColor, xOptions, true);
       {$ELSE}
          PolyPolygon(GlyphPolygon.Points, xColor, xOptions, true);
       {$ENDIF}

       end;
    finally
      FreeMem(BufPtr);
    end;
end;

procedure TBitmap32Ex.RenderTextExW(const xLeft, yBottom: GR32.tFixed;
                                   const xText: WideString;
                                   const xColor : tColor32;
                                   const xOptions : tPolygonDrawOptions);
var
   vShift : tFloatPoint;

   Res, bufSize : LongWord;
   bufPtr, buf  : PTTPolygonHeader;
   gm1 : TGLYPHMETRICS; // rus: информация о расположении буквы
   i : integer;
   vCharCode : longword;
   vChar     : WideChar;
   vCharSize : Windows.TSize;
   vCharSizeFloat : tFloatPoint;
   vQuality : integer;
   vTextLength : integer;
   BufCap : LongWord;
   vCharOrign : tFloatPoint;
   vFontFloatMatrix : TFloatMatrix;
begin

  vFontFloatMatrix := Mat2ToFloatMatrix(fFontMat2);

  {  --- rus: I. получить glyph-bufer  символа с помощью GetGlyphOutline для шрифта fFont }

     SelectObject(Handle, Font.Handle);
     { TODO : rus: следует проверить является ли  xChar^ Unicode'ой буквой,
     с помощью GetFontUnicodeRanges  и установить  FUNICODE }

     vShift := FloatPoint(0, 0);
     vCharOrign := FloatPoint(0, 0);
     vTextLength := length(xText);

     if vTextLength > 0 then try
       BufCap := 1024;
       GetMem(BufPtr, BufCap);
       for i := 1 to  vTextLength do
         begin
         vChar := xText[i];
         vCharCode := ord(vChar);
         {Alexander Muylaert : Strange thing here, has it any use???}
         { answer (goodok)   : i don't know }
         bufSize := GetGlyphOutlineW(Handle, vCharCode, GGO_NATIVE,gm1,0,nil,fFontMat2 {VertFlip_mat2}) ;
         if (bufSize = 0) then begin
          vShift.x := vShift.x + round(gm1.gmCellIncX + 0.5);
          vShift.y := vShift.y - round(gm1.gmCellIncY + 0.5);
          continue;
         end;

          if BufSize > BufCap then
            begin
            ReAllocMem(BufPtr, BufSize);
            BufCap := BufSize;
            end;
          buf := bufPtr;
          Res := GetGlyphOutlineW( Handle, vCharCode,GGO_NATIVE,gm1,bufSize, pchar(buf),fFontMat2);//fFontMat2{VertFlip_mat2} );
          if (res = GDI_ERROR) or (buf^.dwType <> TT_POLYGON_TYPE) then continue;
        { now, build polygon :}
        vQuality := (Bezier2SegmentMinLengthInPixel  shl 16);

        BuildGlyphPolygon(bufPtr,
                                bufSize,
                                vQuality,
                                xLeft + GR32.Fixed(vShift.x), yBottom +  GR32.Fixed(vShift.y),
                                gm1, 
                                FixedRect(fDrawOrign.x,
                                          fDrawOrign.y,
                                          GR32.Fixed(Self.Width) + fDrawOrign.x,
                                          GR32.Fixed(Self.Height) + fDrawOrign.y));
        { next glyph shift calculation }
        GetTextExtentPoint32(Handle, @vChar, 1, vCharSize);
        vCharSizeFloat := FloatPoint(vCharSize.cx, 0);
        vCharSizeFloat := TransformFloatPoint(vCharSizeFloat,  vFontFloatMatrix);

        vShift.x := vShift.x + {vCharSizeFloat.x +} gm1.gmCellIncX;
        vShift.y := vShift.y + vCharSizeFloat.y;//gm1.gmCellIncY;

        { --- rus: III. отобразить контур в указанной позиции  }
       {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
          PolyPolygon(GetGlyphPolygon, xColor, xOptions, true);
       {$ELSE}
          PolyPolygon(GlyphPolygon.Points, xColor, xOptions, true);
       {$ENDIF}

       end;
    finally
      FreeMem(BufPtr);
    end;
end;

procedure gRenderTextEx(xBitmap : tBitmap32;
                                   const xHFont : HFont;
                                   const xLeft, yBottom: GR32.tFixed;
                                   const xText: string;
                                   const xColor : tColor32;
                                   const xOptions : tPolygonDrawOptions;
                                   const xTransformMatrix : TFloatMatrix);
var
   vShift : tFloatPoint;

   Res, bufSize : LongWord;
   bufPtr, buf  : PTTPolygonHeader;
   gm1 : TGLYPHMETRICS; // rus: информация о расположении буквы
   //fUnicode : boolean; // rus: признак уникодовской буквы
   i : integer;
   vCharCode : longword;
   vChar     : char;
   vCharSize : Windows.TSize;
   vCharSizeFloat : tFloatPoint;
   vQuality : integer;
   vTextLength : integer;
   BufCap : LongWord;
   vCharOrign : tFloatPoint;
   vFontMat2        : TMat2;
begin
   vFontMat2 := FloatMatrixToMat2(xTransformMatrix);
  {  --- rus: I. получить glyph-bufer  символа с помощью GetGlyphOutline для шрифта fFont }
  //I. To receive glyph-bufer a symbol with help GetGlyphOutline for a font fFont

     SelectObject(xBitmap.Handle, xHFont);
     { TODO : rus: следует проверить является ли  xChar^ Unicode'ой буквой,
     с помощью GetFontUnicodeRanges  и установить  FUNICODE }
     { TODO: eng: Whether it is necessary to check up is xChar ^ Unicode'Ѓ0…0Ѓ0…3 the letter, 
       with help GetFontUnicodeRanges and to establish
     }

     vShift := FloatPoint(0, 0);
     vCharOrign := FloatPoint(0, 0);
     vTextLength := length(xText);

     if vTextLength > 0 then try
       BufCap := 1024;
       GetMem(BufPtr, BufCap);
       for i := 1 to  vTextLength do
         begin
         vChar := xText[i];
         vCharCode := ord(vChar);
         //FUNICODE := UNICODE_SUPPORTS;
         {Alexander Muylaert : Strange thing here, has it any use???}
         { answer (goodok)   : i don't know }
         //if not FUNICODE then  
         bufSize := GetGlyphOutline( xBitmap.Handle, vCharCode, GGO_NATIVE,gm1,0,nil,vFontMat2)
         //                else  bufSize := GetGlyphOutlineW(xBitmap.Handle, vCharCode, GGO_NATIVE,gm1,0,nil,vFontMat2) 
         ;
         if (bufSize = 0) then begin
          vShift.x := vShift.x + round(gm1.gmCellIncX + 0.5);
          vShift.y := vShift.y - round(gm1.gmCellIncY + 0.5);
          continue;
         end;

          if BufSize > BufCap then
            begin
            ReAllocMem(BufPtr, BufSize);
            BufCap := BufSize;
            end;
          buf := bufPtr;
          //if not FUNICODE then 
          Res := GetGlyphOutline( xBitmap.Handle, vCharCode,GGO_NATIVE,gm1,bufSize, pchar(buf),vFontMat2)
          //                 else Res := GetGlyphOutlineW( xBitmap.Handle, vCharCode,GGO_NATIVE,gm1,bufSize, pchar(buf),vFontMat2)
          ;
          if (res = GDI_ERROR) or (buf^.dwType <> TT_POLYGON_TYPE) then continue;
        { now, build polygon :}
        vQuality := (Bezier2SegmentMinLengthInPixel  shl 16);

        BuildGlyphPolygon(bufPtr, { The index on the buffer which contains the information on a tracing of a symbol }
                                bufSize, { The size of the buffer }
                                vQuality,   { We specify minimal conditional curvature (or length) a segment up to which it is necessary to break }
                                xLeft + GR32.Fixed(vShift.x), yBottom +  GR32.Fixed(vShift.y), { For the instruction of position of a symbol, differently we cannot define area of cutting off }
                                gm1, { For the instruction of the size and smeshcheni a symbol concerning a base line and t.d}
                                FixedRect(0,
                                          0,
                                          GR32.Fixed(xBitmap.Width),
                                          GR32.Fixed(xBitmap.Height)));   { We specify a rectangular of cutting off }
        { next glyph shift calculation }
        GetTextExtentPoint32(xBitmap.Handle, @vChar, 1, vCharSize);
        vCharSizeFloat := FloatPoint(vCharSize.cx, 0);
        vCharSizeFloat := TransformFloatPoint(vCharSizeFloat,  xTransformMatrix);

        vShift.x := vShift.x + vCharSizeFloat.x;
        vShift.y := vShift.y + vCharSizeFloat.y;//gm1.gmCellIncY;

        { --- rus: III. отобразить контур в указанной позиции  }
        //III. To display a contour in the specified position
       {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
          gPolyPolygon(xBitmap, GetGlyphPolygon, xColor, xOptions, true);
       {$ELSE}
          gPolyPolygon(xBitmap, GlyphPolygon.Points, xColor, xOptions, true);
       {$ENDIF}
       end;
    finally
      FreeMem(BufPtr);
    end;
end;

procedure gRenderTextExW(xBitmap : tBitmap32;
                                   const xHFont : HFont;
                                   const xLeft, yBottom: GR32.tFixed;
                                   const xText: WideString;
                                   const xColor : tColor32;
                                   const xOptions : tPolygonDrawOptions;
                                   const xTransformMatrix : TFloatMatrix);
var
   vShift : tFloatPoint;

   Res, bufSize : LongWord;
   bufPtr, buf  : PTTPolygonHeader;
   gm1 : TGLYPHMETRICS; // The information on an arrangement of the letter
   i : integer;
   vCharCode : longword;
   vChar     : WideChar;
   vCharSize : Windows.TSize;
   vCharSizeFloat : tFloatPoint;
   vQuality : integer;
   vTextLength : integer;
   BufCap : LongWord;
   vCharOrign : tFloatPoint;
   vFontMat2        : TMat2;
begin
   vFontMat2 := FloatMatrixToMat2(xTransformMatrix);
  {  --- rus: I. получить glyph-bufer  символа с помощью GetGlyphOutline для шрифта fFont }

     SelectObject(xBitmap.Handle, xHFont);
     { TODO : rus: следует проверить является ли  xChar^ Unicode'ой буквой,
     с помощью GetFontUnicodeRanges  и установить  FUNICODE }

     vShift := FloatPoint(0, 0);
     vCharOrign := FloatPoint(0, 0);
     vTextLength := length(xText);

     if vTextLength > 0 then try
       BufCap := 1024;
       GetMem(BufPtr, BufCap);
       for i := 1 to  vTextLength do
         begin
         vChar := xText[i];
         vCharCode := ord(vChar);
         {Alexander Muylaert : Strange thing here, has it any use???}
         { answer (goodok)   : i don't know }
         bufSize := GetGlyphOutlineW(xBitmap.Handle, vCharCode, GGO_NATIVE,gm1,0,nil,vFontMat2) ;
         if (bufSize = 0) then begin
          vShift.x := vShift.x + round(gm1.gmCellIncX + 0.5);
          vShift.y := vShift.y - round(gm1.gmCellIncY + 0.5);
          continue;
         end;

          if BufSize > BufCap then
            begin
            ReAllocMem(BufPtr, BufSize);
            BufCap := BufSize;
            end;
          buf := bufPtr;
          Res := GetGlyphOutlineW( xBitmap.Handle, vCharCode,GGO_NATIVE,gm1,bufSize, pchar(buf),vFontMat2);
          if (res = GDI_ERROR) or (buf^.dwType <> TT_POLYGON_TYPE) then continue;
        { now, build polygon :}
        vQuality := (Bezier2SegmentMinLengthInPixel  shl 16);

        BuildGlyphPolygon(bufPtr, 
                                bufSize,
                                vQuality,
                                xLeft + GR32.Fixed(vShift.x), yBottom +  GR32.Fixed(vShift.y), 
                                gm1,
                                FixedRect(0,
                                          0,
                                          GR32.Fixed(xBitmap.Width),
                                          GR32.Fixed(xBitmap.Height))); 
        { next glyph shift calculation }
        GetTextExtentPoint32(xBitmap.Handle, @vChar, 1, vCharSize);
        vCharSizeFloat := FloatPoint(vCharSize.cx, 0);
        vCharSizeFloat := TransformFloatPoint(vCharSizeFloat,  xTransformMatrix);

        vShift.x := vShift.x + gm1.gmCellIncX;
        vShift.y := vShift.y + vCharSizeFloat.y;//gm1.gmCellIncY;

        { --- rus: III. отобразить контур в указанной позиции  }
       {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
          gPolyPolygon(xBitmap, GetGlyphPolygon, xColor, xOptions, true);
       {$ELSE}
          gPolyPolygon(xBitmap, GlyphPolygon.Points, xColor, xOptions, true);
       {$ENDIF}
       end;
    finally
      FreeMem(BufPtr);
    end;
end;


{возвращает длину кривой}
function gGetCurveLength(const xCurve: tArrayOfFixedPoint) : GR32.tFixed;
var
  vSegLength : GR32.tFixed;
  p1, p2, p3, p4 : tFixedPoint;
  iSeg : integer;
begin
  if (Length(xCurve) <= 1) or ((Length(xCurve) - 1) mod 3 <> 0)  then
      begin
      raise exception.CreateFmt('G32_Interface.GetCurveLength error : Invalidate qubic curve segment points count [%d]; must be four ', [Length(xCurve)]);
      exit;
      end;


  BuildApproxCurve(xCurve, Bezier3SegmentMinLengthInPixel shl 16,
                   FixedRect(0, 0, 0, 0));
  //{утв. Кривая находится в массиве  CurvePoints : array of TFixedPoint количество - CurvePointsCount, меньше или равно размера самого массива}

  result := 0;
  {2. оцениваем сегмент в нутри которого находится точка }
  for iSeg := 0 to (CurvePointsCount - 1) div 3 - 1 do
    begin
    { точки сегмента : }
    p1 := CurvePoints[3*iSeg];
    p2 := CurvePoints[3*iSeg + 1];
    p3 := CurvePoints[3*iSeg + 2];
    p4 := CurvePoints[3*iSeg + 3];
    vSegLength := SegmentConditionalLengthQ3N2(p1, p2, p3, p4);
    inc(result, vSegLength);
    end;

end;


procedure gGetPointPositionValuationAtSegment(const p1, p2 : GR32.tFixedPoint;
                                             const xLength : GR32.tFixed;
                                             out xPathX, xPathY: GR32.tFixed;
                                             out xAngle : double);
var
  dl : double;
begin
  if not PointsAreEqual(p1, p2) then
    begin
    dL := xLength/Distance(p1, p2);
    { interpolation : }
    xPathX := p1.x + round(dl*(p2.x - p1.x));
    xPathY := p1.y + round(dl*(p2.y - p1.y));

    if (p2.x = p1.x) then
      begin
      if (p2.y  < p1.y) then xAngle := PiDiv4 else xAngle := - PiDiv4;
      end
    else
      begin
      xAngle := ArcTan2(p2.y - p1.y, p2.x - p1.x);
      end;
    end
  else
    begin
    xPathX := p1.x;
    xPathY := p1.y;
    xAngle := 0;
    end;
  { TODO : угол касательной можно вычислить точно разбив отрезки соединяющие узлы и управляющие точки пополам }
end;


function gGetPointAtCurveEx(const xStartSegmentInd : integer;
                            const xStartLength : GR32.tFixed;
                            const xCurve: tArrayOfFixedPoint;
                            const xLength : GR32.tFixed;
                            out xPathX, xPathY : GR32.tFixed; out xAngle : double;
                            const xApproxNeed : boolean = true
                            ):integer;

var
 iSeg : integer;
 p1, p2, p3, p4 : tFixedPoint;
 vSumLength : GR32.tFixed; { cуммарная длина сегмента }
 vSegLength : GR32.tFixed; { длина cегмента }
begin

  if (Length(xCurve) <= 1) or ((Length(xCurve) - 1) mod 3 <> 0)  then
    begin
    raise exception.CreateFmt('G32_Interface.GetPointAtCurve error : Invalidate qubic curve segment points count [%d]; must be four ', [Length(xCurve)]);
    exit;
    end;

  if xApproxNeed then
  {1. аппроксимируем кривую }
  {eng: 1.approx curve }
  BuildApproxCurve(xCurve, Bezier3SegmentMinLengthInPixel_2 shl 16,
                   FixedRect(0, 0, 0, 0));

  {утв. Кривая находится в массиве  CurvePoints : array of TFixedPoint количество - CurvePointsCount, меньше или равно размера самого массива}
  {conditional: Curve is placed in CurvePoints array }

  if xStartSegmentInd <> 0 then 
    vSumLength := xStartLength
  else
  begin
    vSumLength := 0;
  end;
  result := xStartSegmentInd;
  iSeg := xStartSegmentInd;
  while iSeg < (CurvePointsCount - 1) div 3 do
  {2. оцениваем сегмент внутри которого находится точка }
  {2. find segment which point belong to}
    begin
    { точки сегмента : }
    p1 := CurvePoints[3*iSeg];
    p2 := CurvePoints[3*iSeg + 1];
    p3 := CurvePoints[3*iSeg + 2];
    p4 := CurvePoints[3*iSeg + 3];
    vSegLength := SegmentConditionalLengthQ3N2(p1, p2, p3, p4);
    if  (vSumLength <= xLength) and (vSumLength + vSegLength >= xLength ) then
      begin
      break;
      end;
    inc(vSumLength, vSegLength);
    inc(ISeg);
    end;

  {утв. iSeg - указывает на начало сегмента (p1, p2, p3, p4) }

  {3. find point situation inside segment }
  gGetPointPositionValuationAtSegment(p1, p4, xLength - vSumLength, xPathX, xPathY, xAngle);

  result := iSeg;
end;

procedure gGetPointAtCurve(const xCurve: tArrayOfFixedPoint;
                          const xLength : GR32.tFixed;
                          out xPathX, xPathY : GR32.tFixed; out xAngle : double
                          );
begin
  gGetPointAtCurveEx(0, 0, xCurve, xLength, xPathX, xPathY, xAngle, true);
end;


{возвразает положение точки на расстоянии xLength вдоль кривой, и угол наклона касательной }
{прим.  чтобы избежать аппроксимаци одной и той же кривой, можно её заранее аппроксимировать вызвав функцию BuildApproxCurve
  возвращает индекс сегмента, в котором лежит точка для уже аппроксимированной кривой
}
function TBitmap32Ex.GetPointAtCurve( const xStartSegmentInd : integer;
                                      const xCurve: tArrayOfFixedPoint;
                                      const  xLength : GR32.tFixed;
                                      out xPathX, xPathY : GR32.tFixed;
                                      out xAngle : double;
                                      const xApproxNeed : boolean = true
                                        ) : integer;

var
 iSeg : integer;
 p1, p2, p3, p4 : tFixedPoint;
 vSumLength : GR32.tFixed; { cуммарная длина сегмента }
 vSegLength : GR32.tFixed; { длина cегмента }
begin

  if (Length(xCurve) <= 1) or ((Length(xCurve) - 1) mod 3 <> 0)  then
    begin
    raise exception.CreateFmt('G32_Interface.GetPointAtCurve error : Invalidate qubic curve segment points count [%d]; must be four ', [Length(xCurve)]);
    exit;
    end;

  if xApproxNeed then
  {1. аппроксимируем кривую }
  {eng: 1.approx curve }
  BuildApproxCurve(xCurve, Bezier3SegmentMinLengthInPixel_2 shl 16,
                   FixedRect(fDrawOrign.x, fDrawOrign.y, GR32.Fixed(Self.Width) + fDrawOrign.x, GR32.Fixed(Self.Height) + fDrawOrign.y));

  {утв. Кривая находится в массиве  CurvePoints : array of TFixedPoint количество - CurvePointsCount, меньше или равно размера самого массива}
  {conditional: Curve is placed in CurvePoints array }

  if xStartSegmentInd <> 0 then vSumLength := fLastSumLength
                        else
                          begin
                          vSumLength := 0;
                          fLastSumLength := 0;
                          end;
  result := xStartSegmentInd;
  iSeg := xStartSegmentInd;
  while iSeg < (CurvePointsCount - 1) div 3 do
  {2. оцениваем сегмент внутри которого находится точка }
  {2. find segment which point belong to}
    begin
    { точки сегмента : }
    p1 := CurvePoints[3*iSeg];
    p2 := CurvePoints[3*iSeg + 1];
    p3 := CurvePoints[3*iSeg + 2];
    p4 := CurvePoints[3*iSeg + 3];
    vSegLength := SegmentConditionalLengthQ3N2(p1, p2, p3, p4);
    if  (vSumLength <= xLength) and (vSumLength + vSegLength >= xLength ) then
      begin
      break;
      end;
    inc(vSumLength, vSegLength);
    inc(ISeg);
    end;

  {утв. iSeg - указывает на начало сегмента (p1, p2, p3, p4) }
  {3. оцениваем положение точки внутри сегмента:}
  {3. find point situation inside segment }

  gGetPointPositionValuationAtSegment(p1, p4, xLength - vSumLength, xPathX, xPathY, xAngle);

  result := iSeg;
  fLastSumLength := vSumLength;
end;

{ прорисовка текста вдоль кривой xPath}
procedure TBitmap32Ex.RenderFittedText(const xText : string;
                                       const xColor : tColor32;
                                       const xOptions : tPolygonDrawOptions;
                                       const xPath : tArrayOfFixedPoint);
var
   Res, bufSize : LongWord;
   bufPtr, buf  : PTTPolygonHeader;
   gm1, gm : TGLYPHMETRICS; // rus: информация о расположении буквы

   //fUnicode : boolean; // rus: признак уникодовской буквы
   i : integer;
   vCharCode : longword;

   PathX, PathY : GR32.tFixed; { положение точки }
   vTangent : double; { угол касательной }

   vFontMat2 : tMat2;

   vQuality  : integer; { качество аппроксимации - минимальная условная длина сегмента }
   vCurveLength : GR32.tFixed;

   vLastSegment : integer;
   vLengthShift : GR32.tFixed;

   vApproxedPath : tArrayOfFixedPoint; { апроксимированная кривая }

   procedure GrowLengthShift(const dX , dY : GR32.tFixed);
   begin
     { dL^2  = dx^2 + dy^2}
     vLengthShift := vLengthShift + GR32.Fixed(sqrt(sqr(div65536*dX) + sqr(div65536*dy))*FittedText_SpacingFactor);
   end;
begin
  {подразумеваем, что xPath - один сегмент }
  if (Length(xPath) <= 1) or ((Length(xPath) - 1) mod 3 <> 0) then
  begin
    raise exception.CreateFmt('G32_Interface.Render fitted text error : Invalidate qubic curve segment points count [%d]; must be four ', [Length(xPath)]);
    exit;
  end;

  if ClipPolyOutside(xPath, FixedRect(fDrawOrign.x, fDrawOrign.y, GR32.Fixed(Self.Width) + fDrawOrign.x, GR32.Fixed(Self.Height)+ fDrawOrign.y)) then exit;

     vCurveLength := gGetCurveLength(xPath);

 {  --- rus: I. получить glyph-bufer  символа с помощью GetGlyphOutline для шрифта fFont }
     UpdateFont;  { устанавливаем в текущий контекст текущи шрифт }
     SelectObject(Handle, Font.Handle);

     { TODO : rus: следует проверить является ли  xChar^ Unicode'ой буквой, с помощью GetFontUnicodeRanges
              и установить  FUNICODE }
     { preprocess approximation: }
     BuildApproxCurve(xPath, Bezier3SegmentMinLengthInPixel_2 shl 16, FixedRect(0, 0, 0, 0));
     SetLength(vApproxedPath, CurvePointsCount);
     for i := 0 to High(vApproxedPath) do vApproxedPath[i] := CurvePoints[i];

     vLengthShift := 0;
     vLastSegment := 0;
     for i := 1 to length(xText) do
       begin
       vCharCode := ord(xText[i]);


       //FUNICODE := UNICODE_SUPPORTS;

       { rus: получаем в gm1  метрики текста вдоль прямой }
       { eng: set to gm1 glyph metrics along line using  fFontMat2}
       //if not FUNICODE then  
       bufSize := GetGlyphOutline( Handle ,vCharCode,GGO_NATIVE,gm1,0,nil, fFontMat2)
       //                else  bufSize := GetGlyphOutlineW( Handle,vCharCode,GGO_NATIVE,gm1,0,nil, fFontMat2)
       ;
       if (bufSize = 0) then
        begin
        GrowLengthShift(GR32.Fixed(gm1.gmCellIncX), GR32.Fixed(gm1.gmCellIncY));
        continue;
        end;

       if (vLengthShift < 0) or (vCurveLength < vLengthShift) then    break;

       { чтобы не аппроксимировать каждый раз, подставляем уже аппроксиммированную СurvePoints }
       vLastSegment := GetPointAtCurve(vLastSegment, vApproxedPath, vLengthShift, PathX, PathY, vTangent, false);

       { eng: now combine font transformation with rotation accroding the path tangent }

       vFontMat2 := GetRotatedMat2(vTangent);
       vFontMat2 := MultMat2(fFontMat2, vFontMat2);

       { for correct using GetGlyphOutline  call this function with new vFontMat2
         ( in other case GetGlyphOutline return -1 when font small)
       }
       //if not FUNICODE then  
       bufSize := GetGlyphOutline( Handle, vCharCode,GGO_NATIVE,gm,0,nil, vFontMat2)
       //                else  bufSize := GetGlyphOutlineW( Handle, vCharCode,GGO_NATIVE,gm,0,nil, vFontMat2)
       ;
       if (bufSize = 0) then
        begin
        GrowLengthShift(GR32.Fixed(gm1.gmCellIncX), GR32.Fixed(gm1.gmCellIncY));
        continue;
        end;


       if abs(DetMat2(vFontMat2)) > 0.01 then { на всякий случай }
         begin
           try
             bufPtr := AllocMem( bufSize );
             buf := bufPtr;
             //if not FUNICODE then 
             Res := GetGlyphOutline( Handle, vCharCode,GGO_NATIVE,gm,bufSize, pchar(buf),vFontMat2 )
             //                else Res := GetGlyphOutlineW( Handle, vCharCode,GGO_NATIVE,gm,bufSize, pchar(buf),vFontMat2 )
             ;

           { определяем траекторию }
           { eng: define thraectory ; 1.1 - coeff. control spacing }
           GrowLengthShift(GR32.Fixed(gm1.gmCellIncX), GR32.Fixed(gm1.gmCellIncY));

             if (res = GDI_ERROR) (*or (res <> bufSize) or (buf^.dwType <> TT_POLYGON_TYPE) *)then
              begin
                FreeMem(bufPtr);
                BufPtr := nil; {!!! }
                continue;
              end;

            vQuality := (Bezier2SegmentMinLengthInPixel  shl 16);
            { --- rus: II. cформировать  tArrayOfArrayOfFixed,  содержащий список ломанных, аппроксимирующий контур }

            BuildGlyphPolygon(bufPtr, { указатель на буфер, который содержит информацию о начертании символа }
                                  res, { размер буфера }
                                  vQuality,   { указываем минимальную условная кривизна (или длина) сегмента до которого нужно разбивать }
                                  PathX, PathY,  { для указания положения символа, иначе мы не сможем определить область отсечения }
                                  gm,            { для указания размера и смещени символа относительно базовой линии и т.д }
                                  FixedRect(fDrawOrign.x, fDrawOrign.y, GR32.Fixed(Self.Width) + fDrawOrign.x, GR32.Fixed(Self.Height) + fDrawOrign.y));   { указываем прямоугольник отсечения }
          { --- rus: III. отобразить контур в указанной позиции  }
          { eng: display glyph}
          {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
              PolyPolygon(GetGlyphPolygon, xColor, xOptions, true);
           {$ELSE}
              PolyPolygon(GlyphPolygon.Points, xColor, xOptions, true);
           {$ENDIF}
         finally
           FreeMem( bufPtr);
           bufPtr := nil;
         end;
       end;
     end;

  vApproxedPath := nil;
end;

procedure TBitmap32Ex.RenderFittedTextW(const xText : WideString;
                                       const xColor : tColor32;
                                       const xOptions : tPolygonDrawOptions;
                                       const xPath : tArrayOfFixedPoint);
var
   Res, bufSize : LongWord;
   bufPtr, buf  : PTTPolygonHeader;
   gm1, gm : TGLYPHMETRICS; // rus: информация о расположении буквы

   i : integer;
   vCharCode : longword;

   PathX, PathY : GR32.tFixed; { положение точки }
   vTangent : double; { угол касательной }

   vFontMat2 : tMat2;

   vQuality  : integer; { качество аппроксимации - минимальная условная длина сегмента }
   vCurveLength : GR32.tFixed;

   vLastSegment : integer;
   vLengthShift : GR32.tFixed;

   vApproxedPath : tArrayOfFixedPoint; { апроксимированная кривая }

   procedure GrowLengthShift(const dX , dY : GR32.tFixed);
   begin
     { dL^2  = dx^2 + dy^2}
     vLengthShift := vLengthShift + GR32.Fixed(sqrt(sqr(div65536*dX) + sqr(div65536*dy))*FittedText_SpacingFactor);
   end;
begin
  {подразумеваем, что xPath - один сегмент }
  if (Length(xPath) <= 1) or ((Length(xPath) - 1) mod 3 <> 0) then
  begin
    raise exception.CreateFmt('G32_Interface.Render fitted text error : Invalidate qubic curve segment points count [%d]; must be four ', [Length(xPath)]);
    exit;
  end;

  if ClipPolyOutside(xPath, FixedRect(fDrawOrign.x, fDrawOrign.y, GR32.Fixed(Self.Width) + fDrawOrign.x, GR32.Fixed(Self.Height)+ fDrawOrign.y)) then exit;

     vCurveLength := gGetCurveLength(xPath);

 {  --- rus: I. получить glyph-bufer  символа с помощью GetGlyphOutline для шрифта fFont }
     UpdateFont;  { устанавливаем в текущий контекст текущи шрифт }
     SelectObject(Handle, Font.Handle);

     { TODO : rus: следует проверить является ли  xChar^ Unicode'ой буквой, с помощью GetFontUnicodeRanges
              и установить  FUNICODE }
     { preprocess approximation: }
     BuildApproxCurve(xPath, Bezier3SegmentMinLengthInPixel_2 shl 16, FixedRect(0, 0, 0, 0));
     SetLength(vApproxedPath, CurvePointsCount);
     for i := 0 to High(vApproxedPath) do vApproxedPath[i] := CurvePoints[i];

     vLengthShift := 0;
     vLastSegment := 0;
     for i := 1 to length(xText) do
       begin
       vCharCode := ord(xText[i]);



       { rus: получаем в gm1  метрики текста вдоль прямой }
       { eng: set to gm1 glyph metrics along line using  fFontMat2}
       bufSize := GetGlyphOutlineW( Handle,vCharCode,GGO_NATIVE,gm1,0,nil, fFontMat2);
       if (bufSize = 0) then
        begin
        GrowLengthShift(GR32.Fixed(gm1.gmCellIncX), GR32.Fixed(gm1.gmCellIncY));
        continue;
        end;

       { если текст уже  выглядывает за линию, то не рисуем его }
       if (vLengthShift < 0) or (vCurveLength < vLengthShift) then    break;

       { чтобы не аппроксимировать каждый раз, подставляем уже аппроксиммированную СurvePoints }
       vLastSegment := GetPointAtCurve(vLastSegment, vApproxedPath, vLengthShift, PathX, PathY, vTangent, false);

       { rus:теперь следует довернуть матрицу fFontMat2, на угол касательной к кривой }
       { либо просто построить  новую матрицу vFontMat2 }

       { eng: now combine font transformation with rotation accroding the path tangent }

       vFontMat2 := GetRotatedMat2(vTangent);
       vFontMat2 := MultMat2(fFontMat2, vFontMat2);

       { for correct using GetGlyphOutline  call this function with new vFontMat2
         ( in other case GetGlyphOutline return -1 when font small)
       }
       bufSize := GetGlyphOutlineW( Handle, vCharCode,GGO_NATIVE,gm,0,nil, vFontMat2);
       if (bufSize = 0) then
        begin
        GrowLengthShift(GR32.Fixed(gm1.gmCellIncX), GR32.Fixed(gm1.gmCellIncY));
        continue;
        end;


       if abs(DetMat2(vFontMat2)) > 0.01 then { на всякий случай }
         begin
           try
             bufPtr := AllocMem( bufSize );
             buf := bufPtr;
             Res := GetGlyphOutlineW( Handle, vCharCode,GGO_NATIVE,gm,bufSize, pchar(buf),vFontMat2 );

           { определяем траекторию }
           { eng: define thraectory ; 1.1 - coeff. control spacing }
           GrowLengthShift(GR32.Fixed(gm1.gmCellIncX), GR32.Fixed(gm1.gmCellIncY));

             if (res = GDI_ERROR) (*or (res <> bufSize) or (buf^.dwType <> TT_POLYGON_TYPE) *)then
              begin
                FreeMem(bufPtr);
                BufPtr := nil; {!!! }
                continue;
              end;

            vQuality := (Bezier2SegmentMinLengthInPixel  shl 16);
            { --- rus: II. cформировать  tArrayOfArrayOfFixed,  содержащий список ломанных, аппроксимирующий контур }

            BuildGlyphPolygon(bufPtr, { указатель на буфер, который содержит информацию о начертании символа }
                                  res, { размер буфера }
                                  vQuality,   { указываем минимальную условная кривизна (или длина) сегмента до которого нужно разбивать }
                                  PathX, PathY,  { для указания положения символа, иначе мы не сможем определить область отсечения }
                                  gm,            { для указания размера и смещени символа относительно базовой линии и т.д }
                                  FixedRect(fDrawOrign.x, fDrawOrign.y, GR32.Fixed(Self.Width) + fDrawOrign.x, GR32.Fixed(Self.Height) + fDrawOrign.y));   { указываем прямоугольник отсечения }
          { --- rus: III. отобразить контур в указанной позиции  }
          { eng: display glyph}
          {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
              PolyPolygon(GetGlyphPolygon, xColor, xOptions, true);
           {$ELSE}
              PolyPolygon(GlyphPolygon.Points, xColor, xOptions, true);
           {$ENDIF}
         finally
           FreeMem( bufPtr);
           bufPtr := nil;
         end;
       end;
     end;

  vApproxedPath := nil;
end;

{ установка матрицы преобразования символов шритва (таким образом можно управлять наклоном и масштабированием шрифта)  }
function TBitmap32Ex.SelectFontMat2(const xValue : tMat2) : tMat2;
begin
  result := fFontMat2;
  fFontMat2 := xValue;
end;

function TBitmap32Ex.SelectFontTransform(const xValue : tFloatMatrix) : tFloatMatrix; { столбец свободных членов игнориуется }
begin
  result[2,0] := fFont_e31;
  result[2,1] := fFont_e32;

  SelectFontMat2(FloatMatrixToMat2(xValue));

  fFont_e31 := xValue[2,0];
  fFont_e31 := xValue[2,1];
end;

function TBitmap32Ex.GetCanvas: tCanvas;
begin
  fCanvas.Handle := Self.Handle;  {!! обязательно следует обновить, поскольку значение меняется }
  result := fCanvas;
end;

procedure TBitmap32Ex.PolyBezier(const Points: TArrayOfFixedPoint;
  const Color: TColor32; const Options: tPolygonDrawOptions; const Closed: Boolean;
  const FillMode: TPolyFillMode);
var
  i : integer;
begin
  for i := 0 to Length(Points) - 1 do
    begin
    Points[i].x := Points[i].x - fDrawOrign.x;
    Points[i].y := Points[i].y - fDrawOrign.y;
    end;

  gPolyBezier(Self, Points, Color, Options, Closed, FillMode);
end;

{ rus: рисование эллипса }
procedure TBitmap32Ex.Ellipse(const xRect : tFixedRect;
                              const xColor: tColor32;
                              const xOptions : tPolygonDrawOptions);
begin
  gEllipse(Self,
           FixedRect(xRect.Left - fDrawOrign.x,
                     xRect.Top  - fDrawOrign.y,
                     xRect.Right - fDrawOrign.x,
                     xRect.Bottom - fDrawOrign.y
                    ),
           xColor, xOptions);
end;

procedure TBitmap32Ex.Ellipse(const xRect : tFixedRect;   const Antialised : boolean = true);
begin
  gEllipse_Styled(Self,
                  FixedRect(xRect.Left - fDrawOrign.x,
                            xRect.Top  - fDrawOrign.y,
                            xRect.Right - fDrawOrign.x,
                           xRect.Bottom - fDrawOrign.y),
                  fPen.fPenData, fBrush.fBrushData, Antialised);
end;

procedure TBitmap32Ex.DrawSymbol(const xP : GR32.tFixedPoint;
                        const xSymbol : tSymbolKind;
                        const xSize   : GR32.tFixed;
                        const xColor  : tColor32;
                        const xOptions : tPolygonDrawOptions);
var
  vP : TFixedPoint;
begin
  vP.x := xP.x - fDrawOrign.x;
  vP.y := xP.y - fDrawOrign.y;
  gDrawSymbol(Self, vP, xSymbol, xSize, xColor, xOptions);
end;




procedure TBitmap32Ex.Arc(const xCenter : tFixedPoint;
                          const xR : GR32.tFixed;
                          const xStartAngle, xEndAngle : double;
                          const xColor : tColor32;
                          const xOptions : tPolygonDrawOptions = pdoFloat);
begin
  gArc(Self, xCenter, xR, xStartAngle, xEndAngle, xColor, xOptions);
end;

procedure TBitmap32Ex.ArcElliptic(const xCenter : tFixedPoint;
                                  const  xA, xB : GR32.tFixed;
                                  const xStartAngle, xEndAngle : double;
                                  const xColor : tColor32;
                                  const xOptions : tPolygonDrawOptions = pdoFloat);
begin
  gArcElliptic(Self, xCenter, xA, xB, xStartAngle, xEndAngle, xColor, xOptions);
end;

{ drawing rotated ellipse; angle value must be in radians }
procedure TBitmap32Ex.EllipseRotated(const xCenter : tFixedPoint;
                                     const  xA, xB : GR32.tFixed;
                                     const xAngle : double;
                                     const xColor : tColor32;
                                     const xOptions : tPolygonDrawOptions = pdoFloat);
begin
  gEllipseRotated(Self, xCenter, xA, xB, xAngle, xColor, xOptions);
end;

procedure TBitmap32Ex.Pie(const xCenter : tFixedPoint;
                          const xR : GR32.tFixed;
                          const xStartAngle, xEndAngle : double;
                          const xColor : tColor32;
                          const xOptions : tPolygonDrawOptions = pdoFloat);
begin
  gPie(Self, xCenter, xR, xStartAngle, xEndAngle, xColor, xOptions);
end;

procedure TBitmap32Ex.PieElliptic(const xCenter : tFixedPoint;
                                  const xA, xB : GR32.tFixed;
                                  const xStartAngle, xEndAngle : double;
                                  const xColor : tColor32;
                                  const xOptions : tPolygonDrawOptions = pdoFloat);
begin
  gPieElliptic(Self, xCenter, xA, xB, xStartAngle, xEndAngle, xColor, xOptions);
end;

procedure TBitmap32Ex.Segment(const xCenter : tFixedPoint;
                          const xR : GR32.tFixed;
                          const xStartAngle, xEndAngle : double;
                          const xColor : tColor32;
                          const xOptions : tPolygonDrawOptions = pdoFloat);
begin
  gSegment(Self, xCenter, xR, xStartAngle, xEndAngle, xColor, xOptions);
end;

procedure TBitmap32Ex.SegmentElliptic(const xCenter : tFixedPoint;
                                      const xA, xB : GR32.tFixed;
                                      const xStartAngle, xEndAngle : double;
                                      const xColor : tColor32;
                                      const xOptions : tPolygonDrawOptions = pdoFloat);
begin
  gSegmentElliptic(Self, xCenter, xA, xB, xStartAngle, xEndAngle, xColor, xOptions);
end;




procedure TBitmap32Ex.RectangleHole(const xRect : tFixedRect;
                                    const xColor : TColor32;
                                    const xOptions : tPolygonDrawOptions);
begin
  gRectangleHole(Self, xRect, xColor, xOptions);
end;

procedure TBitmap32Ex.RectangleHole(const xRect : tRect;
                                    const xColor : TColor32;
                                    const  xOptions : tPolygonDrawOptions);
begin
  RectangleHole(FixedRect(xRect), xColor, xOptions);
end;

procedure TBitmap32Ex.Polygon(const Points: TArrayOfFixedPoint;
                              const Color: TColor32;
                              const Options: tPolygonDrawOptions;
                              const  Closed: Boolean;
                              const FillMode: TPolyFillMode);
var
  i : integer;
begin

  for i := 0 to Length(Points) - 1 do
    begin
    Points[i].x := Points[i].x - fDrawOrign.x;
    Points[i].y := Points[i].y - fDrawOrign.y;
    end;
  gPolygon(Self, Points, Color, Options, Closed, FillMode);
  for i := 0 to Length(Points) - 1 do
    begin
    Points[i].x := Points[i].x + fDrawOrign.x;
    Points[i].y := Points[i].y + fDrawOrign.y;
    end;
end;

procedure TBitmap32Ex.PolyPolygon(
  const Points: TArrayOfArrayOfFixedPoint; const Color: tColor32;
  const Options: tPolygonDrawOptions; const Closed: Boolean; const FillMode: TPolyFillMode);
var
  i, j : integer;
  xPoints : tArrayOfFixedPoint;
begin
  if (fDrawOrign.X = 0) and (fDrawOrign.Y = 0) then begin
    gPolyPolygon(Self, Points, Color, Options, Closed, FillMode);
  end else if fDrawOrign.X = 0 then begin
    for i := 0 to Length(Points) - 1 do begin
      xPoints := Points[i];
      for j := 0 to Length(xPoints) - 1 do begin
        xPoints[j].y := xPoints[j].y - fDrawOrign.y;
      end;
    end;
    gPolyPolygon(Self, Points, Color, Options, Closed, FillMode);
    for i := 0 to Length(Points) - 1 do begin
      xPoints := Points[i];
      for j := 0 to Length(xPoints) - 1 do begin
        xPoints[j].y := xPoints[j].y + fDrawOrign.y;
      end;
    end;
  end else if fDrawOrign.Y = 0 then begin
    for i := 0 to Length(Points) - 1 do begin
      xPoints := Points[i];
      for j := 0 to Length(xPoints) - 1 do begin
        xPoints[j].X := xPoints[j].X - fDrawOrign.X;
      end;
    end;
    gPolyPolygon(Self, Points, Color, Options, Closed, FillMode);
    for i := 0 to Length(Points) - 1 do begin
      xPoints := Points[i];
      for j := 0 to Length(xPoints) - 1 do begin
        xPoints[j].X := xPoints[j].X + fDrawOrign.X;
      end;
    end;
  end else if (fDrawOrign.X <> 0) and (fDrawOrign.Y <> 0) then begin
    for i := 0 to Length(Points) - 1 do begin
      xPoints := Points[i];
      for j := 0 to Length(xPoints) - 1 do begin
        xPoints[j].x := xPoints[j].x - fDrawOrign.x;
        xPoints[j].y := xPoints[j].y - fDrawOrign.y;
      end;
    end;

    gPolyPolygon(Self, Points, Color, Options, Closed, FillMode);

    for i := 0 to Length(Points) - 1 do begin
      xPoints := Points[i];
      for j := 0 to Length(xPoints) - 1 do begin
        xPoints[j].x := xPoints[j].x + fDrawOrign.x;
        xPoints[j].y := xPoints[j].y + fDrawOrign.y;
      end;
    end;
  end;
end;


procedure TBitmap32Ex.PolyPolyBezier(const Points : TArrayOfArrayOfFixedPoint; const Color : tColor32;
                                     const Options : tPolygonDrawOptions;
                     const Closed: Boolean; const FillMode: TPolyFillMode = pfAlternate);
var
  i, j : integer;
  xPoints : tArrayOfFixedPoint;
begin
  for i := 0 to Length(Points) - 1 do
    begin
    xPoints := Points[i];
      for j := 0 to Length(xPoints) - 1 do
        begin
        xPoints[j].x := xPoints[j].x - fDrawOrign.x;
        xPoints[j].y := xPoints[j].y - fDrawOrign.y;
        end;
    end;

    gPolyPolyBezier(Self, Points, Color, Options, Closed, FillMode);

    for i := 0 to Length(Points) - 1 do
    begin
    xPoints := Points[i];
      for j := 0 to Length(xPoints) - 1 do
        begin
        xPoints[j].x := xPoints[j].x + fDrawOrign.x;
        xPoints[j].y := xPoints[j].y + fDrawOrign.y;
        end;
    end;
end;

procedure TBitmap32Ex.PolyBezier(const Points: TArrayOfFixedPoint;
  const Closed, Antialised: boolean; const FillMode: TPolyFillMode);
var
  i : integer;
begin
  for i := 0 to Length(Points) - 1 do
    begin
    Points[i].x := Points[i].x - fDrawOrign.x;
    Points[i].y := Points[i].y - fDrawOrign.y;
    end;
  gPolyBezier_Styled(Self, Points, closed,  fPen.fPenData, fBrush.fBrushData,  Antialised, FillMode);
  for i := 0 to Length(Points) - 1 do
    begin
    Points[i].x := Points[i].x + fDrawOrign.x;
    Points[i].y := Points[i].y + fDrawOrign.y;
    end;

end;

procedure TBitmap32Ex.Polygon(const Points: TArrayOfFixedPoint;
  const Closed, Antialised: boolean; const FillMode: TPolyFillMode);
var
  i : integer;
begin
  for i := 0 to Length(Points) - 1 do
    begin
    Points[i].x := Points[i].x - fDrawOrign.x;
    Points[i].y := Points[i].y - fDrawOrign.y;
    end;
  gPolygon_Styled(Self, Points, closed,  fPen.fPenData, fBrush.fBrushData,  Antialised, FillMode);
  for i := 0 to Length(Points) - 1 do
    begin
    Points[i].x := Points[i].x + fDrawOrign.x;
    Points[i].y := Points[i].y + fDrawOrign.y;
    end;

end;

{ tPen32 }

procedure tPen32.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor tPen32.Create;
begin
  inherited Create;
  fPenData := gr32_Pen32Date_Default;

end;

function tPen32.GetColor: tColor32;
begin
  result := fPenData.Color;
end;

function tPen32.GetEdgeSharpness: single;
begin
  result := fPenData.EdgeSharpness;
end;

function tPen32.GetStyle: tPen32Style;
begin
  result := fPenData.Style;
end;

function tPen32.GetWidth: GR32.tFixed;
begin
  result := fPenData.Width;
end;

procedure tPen32.SetColor(const Value: tColor32);
begin
  fPenData.Color := Value;
end;

procedure tPen32.SetEdgeSharpness(const Value: single);
begin
  fPenData.EdgeSharpness := Value;
end;

procedure tPen32.SetStyle(const Value: tPen32Style);
begin
  fPenData.Style := Value;
end;

procedure tPen32.SetWidth(const Value: GR32.tFixed);
begin
  fPenData.Width := Value;
end;

{ tBrush32 }

procedure tBrush32.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor tBrush32.Create;
begin
  fBrushData := gr32_Brush32Date_Default;
end;

function tBrush32.GetBitmap: tBitmap32;
begin
  result := fBrushData.Bitmap;
end;

function tBrush32.GetColor: tColor32;
begin
  result := fBrushData.Color;
end;

function tBrush32.GetStyle: tBrush32Style;
begin
  result := fBrushData.Style;
end;

procedure tBrush32.SetBitmap(const Value: tBitmap32);
begin
  fBrushData.Bitmap := Value;
end;

procedure tBrush32.SetColor(const Value: tColor32);
begin
  fBrushData.Color  := Value;
end;

procedure tBrush32.SetStyle(const Value: tBrush32Style);
begin
  fBrushData.Style := Value;
end;

initialization

  CP_Clear;
  SetLength(CurvePoints, MaxCurvePointsCount);

{$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
  GlyphPolygone_Clear;
  SetLength(GlyphPolygonPP, 1);
  SetLength(GPPointCount , 1);
  GP_Count := 1;

{$ELSE}
  GlyphPolygon := TPolygon32.Create;
{$ENDIF}

  gv_StyledPolygon := tPolygon32.Create;

finalization

  {$IFDEF OPTIMIZE_GLYPHPOLYGONE_STORAGE}
  {$ELSE}

   try
   GlyphPolygon.Free;
   except
   end;

   try
   except
   gv_StyledPolygon.Free;
   end;
  {$ENDIF}

end.

{ TODO list
  Pen Width and Brush
  +- 1. Оформить процедуру рисования полигона, передавая в качестве
     -цвет и ширина пера
     -цвет и стиль кисти
     -замкнутость
     -сглаживание

  2. подумать над закруглением углов - реально ли использовать параметр функцию закругления ломанной?
  3. реализовать перегруженные методв tBitmapEx посвящённые рисовани полигонов, кривых, сплайнов, фигур
  4. подумать над свойством UseBitmap(Bitmap32);

  5. Подумать, стоит выделить вспомогательные функции в отдельный модуль или нет (математика, апроксимация, геометрия)
}
