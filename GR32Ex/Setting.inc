{ General Compiler Directives and Compiler Versions Setting }
{$IFNDEF SETTING_INC}
{$DEFINE SETTING_INC}

{Include Delphi Versions File}
{$I GR32.inc}
{$I Jedi.inc}

{------------------------------------------------------------------------------}
{ Please change this to suit your needs                                        }
{------------------------------------------------------------------------------}
{.$define Debug}

{.$Define Unicode_Supports}

//if u use the GR32 1.7 below please do NOT enable this.
// NOTE: the 1.8 above is no the Bitmap32.StreachFilter property instead of Resampler property:
(*
    {$IFDEF GR32_1_8_Above}
    TLinearResampler.Create(bmp);
    {$ELSE}
    bmp.StretchFilter := sfLinear;
    {$ENDIF}
*)
{$Define GR32_1_8_Above}
{$Define Designtime_Supports}

// use the PNGImage
{$Define PNG_Supports}

//Enable this will slow down the performance.
//在擦除该元件背景后，在该控件上方的控件如果是透明或半透明，那么也要通知该控件需要重绘。
{$Define AlphaAboveControl_Supports}

//not fined.
{x$Define FastDraw_Supports}

{$IFDEF FastDraw_Supports}
  // paint the common GDI controls on the GRControls
  {$Define GDI_Supports}
{$ENDIF}

{the GR_Interface options}
{$DEFINE OPTIMIZE_CALLFUNCTIONS_MDL}       {5% in with no antialing... }
{$DEFINE OPTIMIZE_CALLFUNCTIONS_CP_ADD}    { ...and filling }
{$DEFINE OPTIMIZE_CALLFUNCTIONS}           {very very litle}
{$DEFINE OPTIMIZE_GLYPHPOLYGONE_STORAGE}   {2% in Filling mode- 20% without filling mode}

{-------------

{------------------------------------------------------------------------------}
{ Common compiler defines                                                      }
{------------------------------------------------------------------------------}
// support for multibyte character sets
{$IFDEF COMPILER3_UP}
  {$DEFINE MBCSSUPPORT}
{$ENDIF}

{***************** These compiler settings should not be changed ***************}
  {$A+ Force alignment on word/dword boundaries}
  {$S+ stack checking}

  {$B- Short evaluation of boolean values}
  {$H+ Long string support}
  {$V- No var string checking}
  {$X+ Extended syntax}
  {$P+ Open string parameters}
  {$J+ Writeable typed constants}
  {$R- No Range checking}

{$IFDEF BCB}
  {$OBJEXPORTALL ON}
{$ENDIF}

// additional tests for debugging
{$IFDEF DEBUG}
  {$HINTS ON; WARNINGS ON}
  {$D+,L+,W+,Y+,YD,Q+,T+}
{$ELSE}
  {$HINTS OFF; WARNINGS OFF}
  {$D-,L-,Y-,Q-,W-,T-}
  {$IMAGEBASE $00400000}
{$ENDIF}

{$IFNDEF COMPILER7}
  {$undef XPTHEMES}
{$ENDIF}

{the CompoundFile}
{$IFDEF COMPILER4_UP}
  {$DEFINE HasLongWord}       { D4 and above have a true LongWord type }
  {$DEFINE HasInt64 }         { D4 and above have a 64-bit Integer }
{$ENDIF}
{$IFDEF LINUX}
  {$DEFINE HasLongWord}       { Kylix has a true LongWord type }
  {$DEFINE HasInt64 }         { Kylix has a 64-bit Integer }
{$ENDIF}

{$ENDIF ~SETTING_INC}
