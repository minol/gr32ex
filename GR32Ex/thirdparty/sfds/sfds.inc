//Type int64 requires Delphi 4 or higher.
//Unit RTLConsts found on Delphi 6+
//
//Defines:
//VERSION_5_OR_BELLOW	- Delphi 4 or 5
//SUPPORTS_INLINE 	- from Delphi 2006 allows using the inline keyword
//

{$IFDEF VER120}  //DELPHI 4
  {$DEFINE VERSION_5_OR_BELLOW}
{$ENDIF}
{$IFDEF VER125}  //BCB 4
  {$DEFINE VERSION_5_OR_BELLOW}
{$ENDIF}
{$IFDEF VER130}  //DELPHI 5 or BCB 5
  {$DEFINE VERSION_5_OR_BELLOW}
{$ENDIF}

{$IFDEF VER180}  //Delphi 2006
  {$DEFINE SUPPORTS_INLINE}
{$ENDIF}
{$IFDEF VER185}  //Delphi 2007
  {$DEFINE SUPPORTS_INLINE}
{$ENDIF}
{$IFDEF FPC}
  {$DEFINE SUPPORTS_INLINE}
{$ENDIF}
